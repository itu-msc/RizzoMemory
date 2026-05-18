using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Net;
using System.Net.Sockets;
using System.Text;

public enum RzBoxKind
{
    Int,
    StringLiteral,
    Ptr,
}

public enum RzObjectType
{
    Object,
    String,
    Signal,
    PartialApp,
}

public readonly struct RzBox
{
    public readonly RzBoxKind Kind;
    public readonly long IntValue;
    public readonly RzObject ObjValue;
    public readonly string StringValue;

    private RzBox(RzBoxKind kind, long intValue, RzObject objValue, string stringValue)
    {
        Kind = kind;
        IntValue = intValue;
        ObjValue = objValue;
        StringValue = stringValue;
    }

    public static RzBox Int(long value) => new(RzBoxKind.Int, value, null, null);
    public static RzBox Ptr(RzObject value) => new(RzBoxKind.Ptr, 0, value, null);
    public static RzBox StringLiteral(string value) => new(RzBoxKind.StringLiteral, 0, null, value);
}

public delegate RzBox RzFun(RzBox[] args);

public class RzObject
{
    public int Tag;
    public RzObjectType ObjectType;
    public int Refcount;
    public RzBox[] Fields;

    public RzObject(int tag, RzObjectType objectType, int refcount, RzBox[] fields)
    {
        Tag = tag;
        ObjectType = objectType;
        Refcount = refcount;
        Fields = fields;
    }

    public virtual int NumFields => Fields.Length;

    public virtual RzBox GetField(int index)
    {
        if (index < 0 || index >= Fields.Length)
        {
            Rz.Fail($"Tried to access field '{index}' out of '{Fields.Length}'");
        }
        return Fields[index];
    }

    public virtual void SetFields(RzBox[] fields)
    {
        Fields = fields;
    }
}

public sealed class RzString : RzObject
{
    public string Value;

    public RzString(string value)
        : base(0, RzObjectType.String, 1, Array.Empty<RzBox>())
    {
        Value = value;
    }
}

public sealed class RzSignal : RzObject
{
    public RzBox Head;
    public RzBox Tail;
    public RzBox Updated;
    public RzBox Prev;
    public RzBox Next;
    public RzBox DebugIndex;

    public RzSignal(RzBox head, RzBox tail, int refcount = 1)
        : base(0, RzObjectType.Signal, refcount, Array.Empty<RzBox>())
    {
        Head = head;
        Tail = tail;
        Updated = Rz.Int(0);
        Prev = Rz.Ptr(null);
        Next = Rz.Ptr(null);
        DebugIndex = Rz.Int(0);
    }

    public override int NumFields => 5;

    public override RzBox GetField(int index) =>
        index switch
        {
            0 => Head,
            1 => Tail,
            2 => Updated,
            3 => Prev,
            4 => Next,
            _ => throw Rz.RuntimeFailure($"Tried to access signal field '{index}'"),
        };
}

public sealed class RzFunction : RzObject
{
    public RzFun Fun;
    public RzBox[] FreeVars;

    public RzFunction(RzFun fun, int arity, RzBox[] freeVars)
        : base(arity, RzObjectType.PartialApp, 1, Array.Empty<RzBox>())
    {
        Fun = fun;
        FreeVars = freeVars;
    }

    public int Arity => Tag;
    public override int NumFields => FreeVars.Length;
}

internal enum ConsolePollResult
{
    NoInput,
    Line,
    InputTooLong,
}

public static class Rz
{
    public const long ChannelConsoleIn = 0;
    public const long ChannelKeyboardIn = 1;
    private const long ChannelDynamicBase = 2;

    private const int TagListNil = 0;
    private const int TagListCons = 1;
    private const int TagSome = 1;
    private const int TagLaterNever = 0;
    private const int TagLaterWait = 1;
    private const int TagLaterTail = 2;
    private const int TagLaterSync = 4;
    private const int TagLaterWatch = 5;
    private const int TagLaterApp = 6;
    private const int TagSyncLeft = 0;
    private const int TagSyncRight = 1;
    private const int TagSyncBoth = 2;
    private const int TagDelay = 0;
    private const int TagOstar = 1;

    public static readonly RzBox Unit = RzBox.Int(0);
    public static readonly RzObject BoolTrue = new(0, RzObjectType.Object, -1, Array.Empty<RzBox>());
    public static readonly RzObject BoolFalse = new(1, RzObjectType.Object, -1, Array.Empty<RzBox>());
    public static readonly RzObject NeverObj = new(TagLaterNever, RzObjectType.Object, -1, Array.Empty<RzBox>());
    public static readonly RzBox Never = RzBox.Ptr(NeverObj);

    private static RzSignal heapHead = new(Unit, Never, -1);
    private static RzSignal heapTail = new(Unit, Never, -1);
    private static RzSignal heapCursor = heapTail;
    private static long heapSize;
    private static long nextDynamicChannel = ChannelDynamicBase;
    private static readonly Stopwatch Stopwatch = Stopwatch.StartNew();
    private static readonly List<RzSignal> OutputSignals = new();
    private static readonly List<RzPortOutput> PortOutputSignals = new();
    private static readonly List<RzTimer> Timers = new();
    private static readonly List<RzTcpInput> TcpInputs = new();
    private static readonly StringBuilder ConsoleLine = new();
    private static ulong randomState;
    private static bool shouldQuit;
    private static bool consoleLineTruncated;

    public static Exception RuntimeFailure(string message) => new InvalidOperationException(message);

    public static void Fail(string message)
    {
        Console.Error.WriteLine(message);
        Environment.Exit(1);
        throw RuntimeFailure(message);
    }

    public static RzBox Int(long value) => RzBox.Int(value);
    public static RzBox Ptr(RzObject value) => RzBox.Ptr(value);
    public static RzBox PtrSig(RzObject value) => RzBox.Ptr(value);
    public static RzBox StrLit(string value) => RzBox.StringLiteral(value);
    public static long UnboxInt(RzBox box) => box.IntValue;
    public static RzObject UnboxPtr(RzBox box) => box.ObjValue;
    public static int ObjectTag(RzObject obj) => obj.Tag;
    public static RzObject BoolCtor(bool value) => value ? BoolTrue : BoolFalse;

    public static void InitRizzo()
    {
        nextDynamicChannel = ChannelDynamicBase;
        heapHead = new RzSignal(Unit, Never, -1);
        heapTail = new RzSignal(Unit, Never, -1);
        heapHead.Next = Ptr(heapTail);
        heapTail.Prev = Ptr(heapHead);
        heapCursor = heapTail;
        heapSize = 0;
        OutputSignals.Clear();
        PortOutputSignals.Clear();
        foreach (RzTcpInput input in TcpInputs)
        {
            input.Dispose();
        }
        TcpInputs.Clear();
        Timers.Clear();
        ConsoleLine.Clear();
        consoleLineTruncated = false;
        randomState = 0;
        shouldQuit = false;
    }

    public static RzObject Ctor(int tag, RzBox[] fields) => new RzObject(tag, RzObjectType.Object, 1, fields);

    public static RzBox ObjectGetField(RzObject obj, int index) => obj.GetField(index);

    public static void RefcountInc(RzObject obj)
    {
        if (obj is not null && obj.Refcount >= 0)
        {
            obj.Refcount++;
        }
    }

    public static void RefcountIncBox(RzBox box)
    {
        if (!IsBoxed(box) && box.ObjValue is not null)
        {
            RefcountInc(box.ObjValue);
        }
    }

    public static void RefcountDecBox(RzBox box)
    {
        if (!IsBoxed(box) && box.ObjValue is not null)
        {
            RefcountDec(box.ObjValue);
        }
    }

    public static void RefcountDec(RzObject obj)
    {
        if (obj is null || obj.Refcount < 0)
        {
            return;
        }
        obj.Refcount--;
        if (obj.Refcount != 0)
        {
            return;
        }

        switch (obj.ObjectType)
        {
            case RzObjectType.Signal:
                SignalFree((RzSignal)obj);
                break;
            case RzObjectType.PartialApp:
                foreach (RzBox arg in ((RzFunction)obj).FreeVars)
                {
                    RefcountDecBox(arg);
                }
                break;
            case RzObjectType.Object:
                foreach (RzBox field in obj.Fields)
                {
                    RefcountDecBox(field);
                }
                break;
            case RzObjectType.String:
                break;
        }
    }

    public static RzObject ResetObject(RzObject obj)
    {
        if (obj is null)
        {
            return null;
        }
        if (obj.ObjectType == RzObjectType.Signal)
        {
            return ResetSignal((RzSignal)obj);
        }
        if (obj.Refcount != 1)
        {
            RefcountDec(obj);
            return null;
        }
        foreach (RzBox field in obj.Fields)
        {
            RefcountDecBox(field);
        }
        obj.SetFields(Array.Empty<RzBox>());
        return obj;
    }

    public static RzObject ReuseObject(RzObject obj, int tag, RzBox[] fields)
    {
        if (obj is null)
        {
            return Ctor(tag, fields);
        }
        obj.ObjectType = RzObjectType.Object;
        obj.Tag = tag;
        obj.SetFields(fields);
        return obj;
    }

    public static RzObject SignalCtor(RzBox head, RzBox tail)
    {
        heapSize++;
        RzSignal signal = new(head, tail);
        InsertSignalNode(signal);
        return signal;
    }

    public static RzObject ReuseSignal(RzObject obj, RzBox head, RzBox tail)
    {
        if (obj is null)
        {
            return SignalCtor(head, tail);
        }
        RzSignal signal = (RzSignal)obj;
        signal.Tag = 0;
        signal.ObjectType = RzObjectType.Signal;
        signal.Updated = Int(0);
        signal.Head = head;
        signal.Tail = tail;
        if (!ReferenceEquals(heapCursor, signal))
        {
            InsertSignalNode(signal);
        }
        return signal;
    }

    public static RzBox Call(RzFun fun, RzBox[] args) => fun(args);

    public static RzBox LiftFun(RzFun fun, int arity, RzBox[] freeVars) =>
        Ptr(new RzFunction(fun, arity, freeVars));

    public static RzBox LiftBuiltin(string name, int arity, RzBox[] freeVars) =>
        Ptr(new RzFunction(args => CallBuiltin(name, args), arity, freeVars));

    public static RzBox Apply1(RzObject funObj, RzBox arg)
    {
        RzFunction fun = (RzFunction)funObj;
        if (fun.Arity == fun.FreeVars.Length + 1)
        {
            RzBox[] args = new RzBox[fun.FreeVars.Length + 1];
            for (int i = 0; i < fun.FreeVars.Length; i++)
            {
                args[i] = fun.FreeVars[i];
                RefcountIncBox(args[i]);
            }
            args[^1] = arg;
            RefcountDec(funObj);
            return fun.Fun(args);
        }
        else
        {
            RzBox[] args = new RzBox[fun.FreeVars.Length + 1];
            for (int i = 0; i < fun.FreeVars.Length; i++)
            {
                args[i] = fun.FreeVars[i];
                RefcountIncBox(args[i]);
            }
            args[^1] = arg;
            RzFunction copy = new(fun.Fun, fun.Arity, args);
            RefcountDec(funObj);
            return Ptr(copy);
        }
    }

    public static RzBox Eq(RzBox a, RzBox b)
    {
        if (IsString(a) || IsString(b))
        {
            return Ptr(BoolCtor(IsString(a) && IsString(b) && StringData(a) == StringData(b)));
        }
        if (a.Kind != b.Kind)
        {
            return Ptr(BoolCtor(false));
        }
        if (a.Kind == RzBoxKind.Int)
        {
            return Ptr(BoolCtor(a.IntValue == b.IntValue));
        }

        RzObject left = a.ObjValue;
        RzObject right = b.ObjValue;
        if (left.ObjectType != right.ObjectType)
        {
            return Ptr(BoolCtor(false));
        }
        return left.ObjectType switch
        {
            RzObjectType.Signal => SignalEq((RzSignal)left, (RzSignal)right),
            RzObjectType.String => Ptr(BoolCtor(((RzString)left).Value == ((RzString)right).Value)),
            RzObjectType.PartialApp => Ptr(BoolCtor(false)),
            RzObjectType.Object => ObjectEq(left, right),
            _ => throw RuntimeFailure($"Runtime error: equality not defined for object type '{left.ObjectType}'"),
        };
    }

    public static RzBox CallBuiltin(string name, RzBox[] args) =>
        name switch
        {
            "start_event_loop" => BuiltinStartEventLoop(args),
            "clock" => BuiltinClock(args),
            "random_int" => BuiltinRandomInt(args),
            "port_input" => BuiltinPortInput(args),
            "parse_int" => BuiltinParseInt(args),
            "not" => BuiltinNot(args),
            "mod" => BuiltinMod(args),
            "eq" => BuiltinEq(args),
            "lt" => BuiltinLt(args),
            "leq" => BuiltinLeq(args),
            "gt" => BuiltinGt(args),
            "geq" => BuiltinGeq(args),
            "abs" => BuiltinAbs(args),
            "min" => BuiltinMin(args),
            "max" => BuiltinMax(args),
            "clamp" => BuiltinClamp(args),
            "add" => BuiltinAdd(args),
            "sub" => BuiltinSub(args),
            "mul" => BuiltinMul(args),
            "div" => BuiltinDiv(args),
            "string_contains" => BuiltinStringContains(args),
            "string_starts_with" => BuiltinStringStartsWith(args),
            "string_ends_with" => BuiltinStringEndsWith(args),
            "string_concat" => BuiltinStringConcat(args),
            "string_eq" => BuiltinStringEq(args),
            "string_is_empty" => BuiltinStringIsEmpty(args),
            "string_head" => BuiltinStringHead(args),
            "string_tail" => BuiltinStringTail(args),
            "string_split" => BuiltinStringSplit(args),
            "string_of_int" => BuiltinStringOfInt(args),
            "list_is_empty" => BuiltinListIsEmpty(args),
            "list_length" => BuiltinListLength(args),
            "console_out_signal" => BuiltinConsoleOutSignal(args),
            "console_out_signal_l" => BuiltinConsoleOutSignalL(args),
            "port_out_signal" => BuiltinPortOutSignal(args),
            "quit_at" => BuiltinQuitAt(args),
            "clear_screen" => BuiltinClearScreen(args),
            "hide_cursor" => BuiltinHideCursor(args),
            "show_cursor" => BuiltinShowCursor(args),
            "move_cursor" => BuiltinMoveCursor(args),
            "match_fail" => BuiltinMatchFail(args),
            _ => throw RuntimeFailure($"Unknown builtin '{name}'"),
        };

    public static RzBox RegisterOutputSignal(RzBox[] args)
    {
        ExpectArity("console_out_signal", 1, args);
        RzSignal signal = ExpectSignal("console_out_signal", args[0]);
        PrintRegisteredOutputHead(signal, true);
        OutputSignals.Add(signal);
        return Unit;
    }

    public static RzBox StartEventLoop()
    {
        while (!shouldQuit)
        {
            DrainTimersAndTcp();
            ConsolePollResult consoleResult = DrainConsoleInput(!HasTimers() && !HasTcpInputs(), out string line);
            DrainTimersAndTcp();
            if (consoleResult == ConsolePollResult.Line)
            {
                Step(ChannelConsoleIn, MakeString(line));
            }
            else if (consoleResult == ConsolePollResult.InputTooLong)
            {
                Console.WriteLine("Input too long, try again.");
            }
            else if (!HasTimers() && !HasTcpInputs())
            {
                if (Console.IsInputRedirected)
                {
                    break;
                }
            }
            else
            {
                int timeout = NextTimeoutMilliseconds();
                if (timeout > 0)
                {
                    System.Threading.Thread.Sleep(Math.Min(timeout, 10));
                }
            }
        }
        return Int(0);
    }

    private static bool IsBoxed(RzBox box) => box.Kind is RzBoxKind.Int or RzBoxKind.StringLiteral;

    private static bool IsString(RzBox box) =>
        box.Kind == RzBoxKind.StringLiteral || (box.Kind == RzBoxKind.Ptr && box.ObjValue is RzString);

    private static string StringData(RzBox box)
    {
        if (box.Kind == RzBoxKind.StringLiteral)
        {
            return box.StringValue;
        }
        if (box.Kind == RzBoxKind.Ptr && box.ObjValue is RzString str)
        {
            return str.Value;
        }
        Fail($"Runtime error: expected string box, got kind {box.Kind}");
        return "";
    }

    private static RzBox MakeString(string value) => Ptr(new RzString(value));

    private static void ExpectArity(string name, int expected, RzBox[] args)
    {
        if (args.Length != expected)
        {
            Fail($"Runtime error: builtin '{name}' expected {expected} argument(s), got {args.Length}");
        }
    }

    private static long ExpectInt(string name, int index, RzBox arg)
    {
        if (arg.Kind != RzBoxKind.Int)
        {
            Fail($"Runtime error: builtin '{name}' expected int for argument {index + 1}, got box kind {arg.Kind}");
        }
        return arg.IntValue;
    }

    private static void ExpectUnit(string name, int index, RzBox arg)
    {
        if (arg.Kind != RzBoxKind.Int || arg.IntValue != 0)
        {
            Fail($"Runtime error: builtin '{name}' expected unit for argument {index + 1}");
        }
    }

    private static RzBox ExpectString(string name, int index, RzBox arg)
    {
        if (!IsString(arg))
        {
            Fail($"Runtime error: builtin '{name}' expected string for argument {index + 1}, got box kind {arg.Kind}");
        }
        return arg;
    }

    private static bool ExpectBool(string name, int index, RzBox arg)
    {
        if (arg.Kind != RzBoxKind.Ptr || (arg.ObjValue != BoolTrue && arg.ObjValue != BoolFalse))
        {
            Fail($"Runtime error: builtin '{name}' expected bool for argument {index + 1}");
        }
        return arg.ObjValue == BoolTrue;
    }

    private static RzObject ExpectList(string name, int index, RzBox arg)
    {
        if (arg.Kind != RzBoxKind.Ptr || arg.ObjValue.ObjectType != RzObjectType.Object)
        {
            Fail($"Runtime error: builtin '{name}' expected list for argument {index + 1}");
        }
        int tag = arg.ObjValue.Tag;
        if (tag != TagListNil && tag != TagListCons)
        {
            Fail($"Runtime error: builtin '{name}' expected list constructor for argument {index + 1}, got tag {tag}");
        }
        return arg.ObjValue;
    }

    private static RzSignal ExpectSignal(string name, RzBox arg)
    {
        if (arg.Kind != RzBoxKind.Ptr || arg.ObjValue is not RzSignal signal)
        {
            Fail($"Runtime error: {name} expected a signal");
            return null;
        }
        return signal;
    }

    private static RzBox BuiltinStartEventLoop(RzBox[] args)
    {
        ExpectArity("start_event_loop", 1, args);
        return StartEventLoop();
    }

    private static RzBox BuiltinStringOfInt(RzBox[] args)
    {
        ExpectArity("string_of_int", 1, args);
        return MakeString(ExpectInt("string_of_int", 0, args[0]).ToString(CultureInfo.InvariantCulture));
    }

    private static RzBox BuiltinRandomInt(RzBox[] args)
    {
        ExpectArity("random_int", 1, args);
        long upper = ExpectInt("random_int", 0, args[0]);
        if (upper <= 0)
        {
            Fail($"Runtime error: builtin 'random_int' expected a positive upper bound, got {upper}");
        }
        return Int((long)(RandomNextU64() % (ulong)upper));
    }

    private static RzBox BuiltinMod(RzBox[] args)
    {
        ExpectArity("mod", 2, args);
        long lhs = ExpectInt("mod", 0, args[0]);
        long rhs = ExpectInt("mod", 1, args[1]);
        if (rhs == 0)
        {
            Fail("Runtime error: builtin 'mod' received division by zero");
        }
        return Int(lhs % rhs);
    }

    private static RzBox BuiltinAbs(RzBox[] args)
    {
        ExpectArity("abs", 1, args);
        long value = ExpectInt("abs", 0, args[0]);
        if (value == long.MinValue)
        {
            Fail("Runtime error: builtin 'abs' cannot negate INT64_MIN");
        }
        return Int(Math.Abs(value));
    }

    private static RzBox BuiltinMin(RzBox[] args)
    {
        ExpectArity("min", 2, args);
        return Int(Math.Min(ExpectInt("min", 0, args[0]), ExpectInt("min", 1, args[1])));
    }

    private static RzBox BuiltinMax(RzBox[] args)
    {
        ExpectArity("max", 2, args);
        return Int(Math.Max(ExpectInt("max", 0, args[0]), ExpectInt("max", 1, args[1])));
    }

    private static RzBox BuiltinClamp(RzBox[] args)
    {
        ExpectArity("clamp", 3, args);
        long value = ExpectInt("clamp", 0, args[0]);
        long lower = ExpectInt("clamp", 1, args[1]);
        long upper = ExpectInt("clamp", 2, args[2]);
        if (lower > upper)
        {
            Fail("Runtime error: builtin 'clamp' expected lower bound <= upper bound");
        }
        return Int(Math.Min(Math.Max(value, lower), upper));
    }

    private static RzBox BuiltinConsoleOutSignal(RzBox[] args) => RegisterOutputSignal(args);

    private static RzBox BuiltinPortOutSignal(RzBox[] args)
    {
        ExpectArity("port_out_signal", 2, args);
        long port = ExpectInt("port_out_signal", 0, args[0]);
        RzSignal signal = ExpectSignal("port_out_signal", args[1]);
        TcpClient client = ConnectLocalhost(port);
        RzPortOutput output = new(signal, client);
        SendRegisteredPortOutputHead(output, true);
        PortOutputSignals.Add(output);
        return Unit;
    }

    private static RzBox BuiltinConsoleOutSignalLStep(RzBox[] args)
    {
        ExpectArity("console_out_signal_l_step", 2, args);
        ExpectUnit("console_out_signal_l_step", 0, args[0]);
        RegisterOutputSignalDeferred(args[1]);
        return PtrSig(SignalCtor(Unit, Never));
    }

    private static RzBox BuiltinConsoleOutSignalL(RzBox[] args)
    {
        ExpectArity("console_out_signal_l", 1, args);
        RzBox liftedRegister = LiftFun(BuiltinConsoleOutSignalLStep, 2, Array.Empty<RzBox>());
        RzBox delayedRegister = Ptr(Ctor(TagDelay, new[] { liftedRegister }));
        SignalCtor(Int(0), Ptr(Ctor(TagLaterApp, new[] { delayedRegister, args[0] })));
        return Unit;
    }

    private static RzBox BuiltinClearScreen(RzBox[] args)
    {
        ExpectArity("clear_screen", 1, args);
        ExpectUnit("clear_screen", 0, args[0]);
        Console.Write("\u001b[2J\u001b[H");
        return Unit;
    }

    private static RzBox BuiltinHideCursor(RzBox[] args)
    {
        ExpectArity("hide_cursor", 1, args);
        ExpectUnit("hide_cursor", 0, args[0]);
        Console.Write("\u001b[?25l");
        return Unit;
    }

    private static RzBox BuiltinShowCursor(RzBox[] args)
    {
        ExpectArity("show_cursor", 1, args);
        ExpectUnit("show_cursor", 0, args[0]);
        Console.Write("\u001b[?25h");
        return Unit;
    }

    private static RzBox BuiltinMoveCursor(RzBox[] args)
    {
        ExpectArity("move_cursor", 2, args);
        long row = ExpectInt("move_cursor", 0, args[0]);
        long column = ExpectInt("move_cursor", 1, args[1]);
        if (row <= 0 || column <= 0)
        {
            Fail($"Runtime error: builtin 'move_cursor' expected positive row and column, got {row}, {column}");
        }
        Console.Write($"\u001b[{row};{column}H");
        return Unit;
    }

    private static RzBox BuiltinStringContains(RzBox[] args)
    {
        ExpectArity("string_contains", 2, args);
        return Ptr(BoolCtor(StringData(ExpectString("string_contains", 0, args[0])).Contains(StringData(ExpectString("string_contains", 1, args[1])), StringComparison.Ordinal)));
    }

    private static RzBox BuiltinStringStartsWith(RzBox[] args)
    {
        ExpectArity("string_starts_with", 2, args);
        return Ptr(BoolCtor(StringData(ExpectString("string_starts_with", 0, args[0])).StartsWith(StringData(ExpectString("string_starts_with", 1, args[1])), StringComparison.Ordinal)));
    }

    private static RzBox BuiltinStringEndsWith(RzBox[] args)
    {
        ExpectArity("string_ends_with", 2, args);
        return Ptr(BoolCtor(StringData(ExpectString("string_ends_with", 0, args[0])).EndsWith(StringData(ExpectString("string_ends_with", 1, args[1])), StringComparison.Ordinal)));
    }

    private static RzBox BuiltinListIsEmpty(RzBox[] args)
    {
        ExpectArity("list_is_empty", 1, args);
        return Ptr(BoolCtor(ExpectList("list_is_empty", 0, args[0]).Tag == TagListNil));
    }

    private static RzBox BuiltinListLength(RzBox[] args)
    {
        ExpectArity("list_length", 1, args);
        RzObject list = ExpectList("list_length", 0, args[0]);
        long length = 0;
        while (list.Tag == TagListCons)
        {
            length++;
            list = ExpectList("list_length", 0, list.GetField(1));
        }
        return Int(length);
    }

    private static RzBox BuiltinStringSplit(RzBox[] args)
    {
        ExpectArity("string_split", 2, args);
        string source = StringData(ExpectString("string_split", 0, args[0]));
        string delimiter = StringData(ExpectString("string_split", 1, args[1]));
        if (delimiter.Length == 0)
        {
            Fail("Runtime error: builtin 'string_split' expected a non-empty delimiter");
        }
        string[] parts = source.Split(delimiter, StringSplitOptions.None);
        RzBox result = Ptr(Ctor(TagListNil, Array.Empty<RzBox>()));
        for (int i = parts.Length - 1; i >= 0; i--)
        {
            result = Ptr(Ctor(TagListCons, new[] { MakeString(parts[i]), result }));
        }
        return result;
    }

    private static RzBox BuiltinClock(RzBox[] args)
    {
        ExpectArity("clock", 1, args);
        long intervalMs = ExpectInt("clock", 0, args[0]);
        if (intervalMs <= 0)
        {
            Fail($"Runtime error: builtin 'clock' expected a positive interval in milliseconds, got {intervalMs}");
        }
        long channel = TimerRegister(intervalMs);
        return MakeChannelSignal(channel, Int(0));
    }

    private static RzBox BuiltinPortInput(RzBox[] args)
    {
        ExpectArity("port_input", 1, args);
        long port = ExpectInt("port_input", 0, args[0]);
        long channel = TcpInputRegister(port);
        return MakeChannelSignal(channel, MakeString(""));
    }

    private static RzBox BuiltinQuitAt(RzBox[] args)
    {
        ExpectArity("quit_at", 1, args);
        RzBox liftedQuit = LiftFun(Quit, 2, Array.Empty<RzBox>());
        RzBox delayedQuit = Ptr(Ctor(TagDelay, new[] { liftedQuit }));
        SignalCtor(Int(0), Ptr(Ctor(TagLaterApp, new[] { delayedQuit, args[0] })));
        return Unit;
    }

    private static RzBox BuiltinParseInt(RzBox[] args)
    {
        ExpectArity("parse_int", 1, args);
        string source = StringData(ExpectString("parse_int", 0, args[0]));
        if (long.TryParse(source, NumberStyles.Integer, CultureInfo.InvariantCulture, out long value))
        {
            return Ptr(Ctor(1, new[] { Int(value) }));
        }
        return Ptr(Ctor(0, Array.Empty<RzBox>()));
    }

    private static RzBox BuiltinEq(RzBox[] args)
    {
        ExpectArity("eq", 2, args);
        return Eq(args[0], args[1]);
    }

    private static RzBox BuiltinNot(RzBox[] args)
    {
        ExpectArity("not", 1, args);
        return Ptr(BoolCtor(!ExpectBool("not", 0, args[0])));
    }

    private static RzBox BuiltinLt(RzBox[] args)
    {
        ExpectArity("lt", 2, args);
        return Ptr(BoolCtor(ExpectInt("lt", 0, args[0]) < ExpectInt("lt", 1, args[1])));
    }

    private static RzBox BuiltinLeq(RzBox[] args)
    {
        ExpectArity("leq", 2, args);
        return Ptr(BoolCtor(ExpectInt("leq", 0, args[0]) <= ExpectInt("leq", 1, args[1])));
    }

    private static RzBox BuiltinGt(RzBox[] args)
    {
        ExpectArity("gt", 2, args);
        return Ptr(BoolCtor(ExpectInt("gt", 0, args[0]) > ExpectInt("gt", 1, args[1])));
    }

    private static RzBox BuiltinGeq(RzBox[] args)
    {
        ExpectArity("geq", 2, args);
        return Ptr(BoolCtor(ExpectInt("geq", 0, args[0]) >= ExpectInt("geq", 1, args[1])));
    }

    private static RzBox BuiltinAdd(RzBox[] args)
    {
        ExpectArity("add", 2, args);
        return Int(ExpectInt("add", 0, args[0]) + ExpectInt("add", 1, args[1]));
    }

    private static RzBox BuiltinSub(RzBox[] args)
    {
        ExpectArity("sub", 2, args);
        return Int(ExpectInt("sub", 0, args[0]) - ExpectInt("sub", 1, args[1]));
    }

    private static RzBox BuiltinMul(RzBox[] args)
    {
        ExpectArity("mul", 2, args);
        return Int(ExpectInt("mul", 0, args[0]) * ExpectInt("mul", 1, args[1]));
    }

    private static RzBox BuiltinDiv(RzBox[] args)
    {
        ExpectArity("div", 2, args);
        long rhs = ExpectInt("div", 1, args[1]);
        if (rhs == 0)
        {
            Fail("Runtime error: builtin 'div' received division by zero");
        }
        return Int(ExpectInt("div", 0, args[0]) / rhs);
    }

    private static RzBox BuiltinStringConcat(RzBox[] args)
    {
        ExpectArity("string_concat", 2, args);
        return MakeString(StringData(ExpectString("string_concat", 0, args[0])) + StringData(ExpectString("string_concat", 1, args[1])));
    }

    private static RzBox BuiltinStringEq(RzBox[] args)
    {
        ExpectArity("string_eq", 2, args);
        return Ptr(BoolCtor(StringData(ExpectString("string_eq", 0, args[0])) == StringData(ExpectString("string_eq", 1, args[1]))));
    }

    private static RzBox BuiltinStringIsEmpty(RzBox[] args)
    {
        ExpectArity("string_is_empty", 1, args);
        return Ptr(BoolCtor(StringData(ExpectString("string_is_empty", 0, args[0])).Length == 0));
    }

    private static RzBox BuiltinStringHead(RzBox[] args)
    {
        ExpectArity("string_head", 1, args);
        string source = StringData(ExpectString("string_head", 0, args[0]));
        if (source.Length == 0)
        {
            Fail("Runtime error: string_head on empty string");
        }
        int width = char.IsHighSurrogate(source[0]) && source.Length > 1 ? 2 : 1;
        return MakeString(source[..width]);
    }

    private static RzBox BuiltinStringTail(RzBox[] args)
    {
        ExpectArity("string_tail", 1, args);
        string source = StringData(ExpectString("string_tail", 0, args[0]));
        if (source.Length == 0)
        {
            Fail("Runtime error: string_tail on empty string");
        }
        int width = char.IsHighSurrogate(source[0]) && source.Length > 1 ? 2 : 1;
        return MakeString(source[width..]);
    }

    private static RzBox BuiltinMatchFail(RzBox[] args)
    {
        ExpectArity("match_fail", 1, args);
        Fail($"Runtime error: {StringData(ExpectString("match_fail", 0, args[0]))}");
        return Unit;
    }

    private static RzBox Quit(RzBox[] args)
    {
        ExpectArity("quit", 2, args);
        ExpectUnit("quit", 0, args[0]);
        shouldQuit = true;
        return args[1];
    }

    private static RzBox MakeChannelSignal(long channel, RzBox head)
    {
        RzBox delayedStep = Ptr(Ctor(TagDelay, new[] { LiftFun(ChannelSignalStep, 3, new[] { Int(channel) }) }));
        RzBox waitLater = Ptr(Ctor(TagLaterWait, new[] { Int(channel) }));
        RzBox tail = Ptr(Ctor(TagLaterApp, new[] { delayedStep, waitLater }));
        return PtrSig(SignalCtor(head, tail));
    }

    private static RzBox ChannelSignalStep(RzBox[] args)
    {
        ExpectArity("channel_signal_step", 3, args);
        long channel = ExpectInt("channel_signal_step", 0, args[0]);
        ExpectUnit("channel_signal_step", 1, args[1]);
        return MakeChannelSignal(channel, args[2]);
    }

    private static void RegisterOutputSignalDeferred(RzBox arg)
    {
        RzSignal signal = ExpectSignal("console_out_signal_l", arg);
        if (signal.Updated.IntValue == 0)
        {
            PrintRegisteredOutputHead(signal, true);
        }
        OutputSignals.Add(signal);
    }

    private static void PrintRegisteredOutputHead(RzSignal signal, bool force)
    {
        if (signal.Updated.IntValue != 0 || force)
        {
            Console.WriteLine(DebugPrintBox(signal.Head));
            Console.Out.Flush();
        }
    }

    private static void PrintRegisteredOutputs()
    {
        foreach (RzSignal signal in OutputSignals)
        {
            PrintRegisteredOutputHead(signal, false);
        }
        foreach (RzPortOutput output in PortOutputSignals)
        {
            SendRegisteredPortOutputHead(output, false);
        }
    }

    private static void SendRegisteredPortOutputHead(RzPortOutput output, bool force)
    {
        if (output.Signal.Updated.IntValue != 0 || force)
        {
            byte[] bytes = Encoding.UTF8.GetBytes(StringData(output.Signal.Head) + "\n");
            output.Client.GetStream().Write(bytes, 0, bytes.Length);
            output.Client.GetStream().Flush();
        }
    }

    private static string DebugPrintBox(RzBox box)
    {
        return box.Kind switch
        {
            RzBoxKind.Int => box.IntValue.ToString(CultureInfo.InvariantCulture),
            RzBoxKind.StringLiteral => box.StringValue,
            RzBoxKind.Ptr when box.ObjValue is RzString str => str.Value,
            RzBoxKind.Ptr when box.ObjValue is RzSignal sig => $"signal(ref: {sig.Refcount}, head: {DebugPrintBox(sig.Head)}, tail: {DebugPrintBox(sig.Tail)}, updated: {sig.Updated.IntValue})",
            RzBoxKind.Ptr when box.ObjValue is RzFunction fun => $"pap(ref: {fun.Refcount}, arity: {fun.Arity}, applied_vars: {fun.FreeVars.Length})",
            RzBoxKind.Ptr => DebugPrintObject(box.ObjValue),
            _ => throw RuntimeFailure($"Unknown box tag: '{box.Kind}'"),
        };
    }

    private static string DebugPrintObject(RzObject obj)
    {
        string result = $"ctor({obj.Tag}, ref: {obj.Refcount}) ";
        if (obj.NumFields > 0)
        {
            string[] fields = new string[obj.NumFields];
            for (int i = 0; i < obj.NumFields; i++)
            {
                fields[i] = DebugPrintBox(obj.GetField(i));
            }
            result += "{ " + string.Join(", ", fields) + "}";
        }
        return result;
    }

    private static void Step(long channel, RzBox value)
    {
        HeapUpdate(channel, value);
        RefcountDecBox(value);
        PrintRegisteredOutputs();
    }

    private static void HeapUpdate(long channel, RzBox value)
    {
        if (heapHead.Next.ObjValue == heapTail)
        {
            return;
        }
        heapCursor = (RzSignal)heapHead.Next.ObjValue;
        while (heapCursor != heapTail)
        {
            RzSignal current = heapCursor;
            RzObject tail = UnboxPtr(current.Tail);
            if (!Ticked(tail, channel, value))
            {
                current.Updated = Int(0);
            }
            else
            {
                RefcountDecBox(current.Head);
                RzBox advanced = Advance(tail, channel, value);
                RzSignal signal = (RzSignal)UnboxPtr(advanced);
                current.Updated = Int(1);
                RefcountIncBox(signal.Head);
                RefcountIncBox(signal.Tail);
                current.Head = signal.Head;
                current.Tail = signal.Tail;
                RefcountDecBox(advanced);
            }
            heapCursor = (RzSignal)current.Next.ObjValue;
        }
    }

    private static bool Ticked(RzObject later, long channel, RzBox value)
    {
        return later.Tag switch
        {
            TagLaterNever => false,
            TagLaterApp => Ticked(UnboxPtr(later.GetField(1)), channel, value),
            TagLaterWait => later.GetField(0).IntValue == channel,
            TagLaterWatch => WatchTicked(later),
            TagLaterTail => TailTicked(later),
            TagLaterSync => Ticked(UnboxPtr(later.GetField(0)), channel, value) || Ticked(UnboxPtr(later.GetField(1)), channel, value),
            _ => throw RuntimeFailure($"rz_ticked - unknown later tag '{later.Tag}'"),
        };
    }

    private static bool WatchTicked(RzObject later)
    {
        RzSignal signal = (RzSignal)later.GetField(0).ObjValue;
        return signal.Head.ObjValue is not null && signal.Head.ObjValue.Tag == TagSome && signal.Updated.IntValue != 0;
    }

    private static bool TailTicked(RzObject later)
    {
        RzSignal signal = (RzSignal)later.GetField(0).ObjValue;
        return signal.Updated.IntValue != 0;
    }

    private static RzBox Advance(RzObject later, long channel, RzBox value)
    {
        switch (later.Tag)
        {
            case TagLaterWait:
                RefcountIncBox(value);
                RefcountDec(later);
                return value;
            case TagLaterApp:
            {
                RzBox delayedFun = later.GetField(0);
                RefcountIncBox(delayedFun);
                RzBox fun = AdvanceDelayed(delayedFun);
                RzBox argLater = later.GetField(1);
                RefcountIncBox(argLater);
                RefcountDec(later);
                RzBox arg = Advance(UnboxPtr(argLater), channel, value);
                return Apply1(UnboxPtr(fun), arg);
            }
            case TagLaterTail:
            {
                RzBox signal = later.GetField(0);
                RefcountIncBox(signal);
                RefcountDec(later);
                return signal;
            }
            case TagLaterWatch:
            {
                RzSignal signal = (RzSignal)later.GetField(0).ObjValue;
                RzObject some = signal.Head.ObjValue;
                if (some.Tag != TagSome)
                {
                    Fail("RUNTIME ERROR: tried to advance signal where head wasn't a SOME");
                }
                RzBox result = some.GetField(0);
                RefcountIncBox(result);
                RefcountDec(later);
                return result;
            }
            case TagLaterSync:
            {
                RzObject leftLater = later.GetField(0).ObjValue;
                RzObject rightLater = later.GetField(1).ObjValue;
                RefcountInc(leftLater);
                RefcountInc(rightLater);
                RzObject reusable = ResetObject(later);
                bool leftTicked = Ticked(leftLater, channel, value);
                bool rightTicked = Ticked(rightLater, channel, value);
                if (leftTicked && rightTicked)
                {
                    RzBox left = Advance(leftLater, channel, value);
                    RzBox right = Advance(rightLater, channel, value);
                    return Ptr(ReuseObject(reusable, TagSyncBoth, new[] { left, right }));
                }
                if (leftTicked)
                {
                    RefcountDec(rightLater);
                    RzBox left = Advance(leftLater, channel, value);
                    return Ptr(Ctor(TagSyncLeft, new[] { left }));
                }
                RefcountDec(leftLater);
                RzBox rightOnly = Advance(rightLater, channel, value);
                return Ptr(Ctor(TagSyncRight, new[] { rightOnly }));
            }
            default:
                throw RuntimeFailure($"rz_advance - unknown later tag '{later.Tag}'");
        }
    }

    private static RzBox AdvanceDelayed(RzBox delayed)
    {
        RzObject ptrDelayed = UnboxPtr(delayed);
        switch (ptrDelayed.Tag)
        {
            case TagDelay:
            {
                RzObject thunk = ptrDelayed.GetField(0).ObjValue;
                RefcountInc(thunk);
                RefcountDec(ptrDelayed);
                return Apply1(thunk, Unit);
            }
            case TagOstar:
            {
                RzBox leftDelay = ptrDelayed.GetField(0);
                RzBox rightDelay = ptrDelayed.GetField(1);
                RefcountIncBox(leftDelay);
                RefcountIncBox(rightDelay);
                RefcountDec(ptrDelayed);
                RzObject fun = AdvanceDelayed(leftDelay).ObjValue;
                RzBox right = AdvanceDelayed(rightDelay);
                return Apply1(fun, right);
            }
            default:
                throw RuntimeFailure($"Unknown delayed tag in 'rz_advance_delayed': {ptrDelayed.Tag}");
        }
    }

    private static RzBox SignalEq(RzSignal left, RzSignal right)
    {
        bool heads = Eq(left.Head, right.Head).ObjValue == BoolTrue;
        bool tails = Eq(left.Tail, right.Tail).ObjValue == BoolTrue;
        return Ptr(BoolCtor(heads && tails));
    }

    private static RzBox ObjectEq(RzObject left, RzObject right)
    {
        if (left.Tag != right.Tag || left.NumFields != right.NumFields)
        {
            return Ptr(BoolCtor(false));
        }
        for (int i = 0; i < left.NumFields; i++)
        {
            if (Eq(left.GetField(i), right.GetField(i)).ObjValue == BoolFalse)
            {
                return Ptr(BoolCtor(false));
            }
        }
        return Ptr(BoolCtor(true));
    }

    private static void SignalFree(RzSignal signal)
    {
        RefcountDecBox(signal.Head);
        RefcountDecBox(signal.Tail);
        RemoveSignalNode(signal);
        heapSize--;
    }

    private static RzObject ResetSignal(RzSignal signal)
    {
        if (signal.Refcount != 1)
        {
            RefcountDec(signal);
            return null;
        }
        RefcountDecBox(signal.Head);
        RefcountDecBox(signal.Tail);
        signal.Head = Unit;
        signal.Tail = Never;
        signal.Updated = Int(0);
        RemoveSignalNode(signal);
        signal.Prev = Ptr(null);
        signal.Next = Ptr(null);
        return signal;
    }

    private static void RemoveSignalNode(RzSignal signal)
    {
        if (signal.Prev.ObjValue is not RzSignal prev || signal.Next.ObjValue is not RzSignal next)
        {
            return;
        }
        prev.Next = Ptr(next);
        next.Prev = Ptr(prev);
        if (heapCursor == signal)
        {
            heapCursor = next;
        }
    }

    private static void InsertSignalNode(RzSignal signal)
    {
        RzSignal prev = (RzSignal)heapCursor.Prev.ObjValue;
        RzSignal next = heapCursor;
        signal.Prev = Ptr(prev);
        signal.Next = Ptr(next);
        next.Prev = Ptr(signal);
        prev.Next = Ptr(signal);
    }

    private static ulong RandomNextU64()
    {
        if (randomState == 0)
        {
            randomState = ((ulong)(Stopwatch.Elapsed.TotalSeconds * 1_000_000_000.0)) ^ 0x9E3779B97F4A7C15UL;
        }
        randomState ^= randomState << 13;
        randomState ^= randomState >> 7;
        randomState ^= randomState << 17;
        return randomState;
    }

    private static long ChannelAlloc() => nextDynamicChannel++;

    private static long TimerRegister(long intervalMs)
    {
        long channel = ChannelAlloc();
        double now = Stopwatch.Elapsed.TotalSeconds;
        Timers.Add(new RzTimer(channel, intervalMs / 1000.0, now));
        return channel;
    }

    private static bool HasTimers() => Timers.Count > 0;
    private static bool HasTcpInputs() => TcpInputs.Count > 0;

    private static int NextTimeoutMilliseconds()
    {
        if (Timers.Count == 0)
        {
            return HasTcpInputs() ? 10 : int.MaxValue;
        }
        double now = Stopwatch.Elapsed.TotalSeconds;
        double min = double.PositiveInfinity;
        foreach (RzTimer timer in Timers)
        {
            min = Math.Min(min, timer.NextFireSeconds - now);
        }
        int timerMs = min <= 0 ? 0 : Math.Min(int.MaxValue, (int)(min * 1000.0));
        return HasTcpInputs() ? Math.Min(timerMs, 10) : timerMs;
    }

    private static bool TimerTakeDue(out long channel, out RzBox value)
    {
        double now = Stopwatch.Elapsed.TotalSeconds;
        foreach (RzTimer timer in Timers)
        {
            if (timer.NextFireSeconds <= now)
            {
                channel = timer.Channel;
                long elapsedMs = (long)Math.Round((timer.NextFireSeconds - timer.StartSeconds) * 1000.0);
                value = Int(elapsedMs);
                timer.NextFireSeconds += timer.IntervalSeconds;
                return true;
            }
        }
        channel = 0;
        value = Unit;
        return false;
    }

    private static void DrainTimersAndTcp()
    {
        while (TimerTakeDue(out long timerChannel, out RzBox timerValue))
        {
            Step(timerChannel, timerValue);
        }
        while (TcpTakeInput(out long tcpChannel, out RzBox tcpValue))
        {
            Step(tcpChannel, tcpValue);
        }
    }

    private static ConsolePollResult DrainConsoleInput(bool block, out string line)
    {
        line = "";
        if (Console.IsInputRedirected)
        {
            return DrainRedirectedConsoleInput(out line);
        }
        return DrainInteractiveConsoleInput(block, out line);
    }

    private static ConsolePollResult DrainRedirectedConsoleInput(out string line)
    {
        line = "";
        try
        {
            if (Console.In.Peek() < 0)
            {
                return ConsolePollResult.NoInput;
            }
            string read = Console.ReadLine();
            if (read is null)
            {
                return ConsolePollResult.NoInput;
            }
            foreach (char ch in read)
            {
                Step(ChannelKeyboardIn, MakeString(ch.ToString()));
            }
            Step(ChannelKeyboardIn, MakeString("Enter"));
            line = read;
            return ConsolePollResult.Line;
        }
        catch (InvalidOperationException)
        {
            return ConsolePollResult.NoInput;
        }
    }

    private static ConsolePollResult DrainInteractiveConsoleInput(bool block, out string line)
    {
        line = "";
        if (block && !Console.KeyAvailable)
        {
            return HandleConsoleKey(Console.ReadKey(intercept: true), out line);
        }

        while (Console.KeyAvailable)
        {
            ConsolePollResult result = HandleConsoleKey(Console.ReadKey(intercept: true), out line);
            if (result is ConsolePollResult.Line or ConsolePollResult.InputTooLong)
            {
                return result;
            }
        }
        return ConsolePollResult.NoInput;
    }

    private static ConsolePollResult HandleConsoleKey(ConsoleKeyInfo key, out string line)
    {
        line = "";
        string keyName = KeyboardEventName(key);
        if (keyName.Length > 0)
        {
            Step(ChannelKeyboardIn, MakeString(keyName));
        }

        if (key.Key == ConsoleKey.Enter)
        {
            Console.WriteLine();
            line = ConsoleLine.ToString();
            ConsoleLine.Clear();
            bool truncated = consoleLineTruncated;
            consoleLineTruncated = false;
            return truncated ? ConsolePollResult.InputTooLong : ConsolePollResult.Line;
        }

        if (key.Key == ConsoleKey.Backspace)
        {
            if (ConsoleLine.Length > 0)
            {
                ConsoleLine.Length--;
                Console.Write("\b \b");
            }
            return ConsolePollResult.NoInput;
        }

        char ch = key.KeyChar;
        if (ch >= ' ' || ch == '\t')
        {
            if (ConsoleLine.Length + 1 < 4096)
            {
                ConsoleLine.Append(ch);
            }
            else
            {
                consoleLineTruncated = true;
            }
            Console.Write(ch);
        }

        return ConsolePollResult.NoInput;
    }

    private static string KeyboardEventName(ConsoleKeyInfo key)
    {
        return key.Key switch
        {
            ConsoleKey.Enter => "Enter",
            ConsoleKey.Backspace => "Backspace",
            ConsoleKey.Tab => "Tab",
            ConsoleKey.Escape => "Escape",
            ConsoleKey.UpArrow => "ArrowUp",
            ConsoleKey.DownArrow => "ArrowDown",
            ConsoleKey.LeftArrow => "ArrowLeft",
            ConsoleKey.RightArrow => "ArrowRight",
            _ => key.KeyChar >= ' ' ? key.KeyChar.ToString() : "",
        };
    }

    private static long TcpInputRegister(long port)
    {
        ValidatePort("port_input", port);
        foreach (RzTcpInput existing in TcpInputs)
        {
            if (existing.Port == port)
            {
                return existing.Channel;
            }
        }
        long channel = ChannelAlloc();
        RzTcpInput input = new(port, channel);
        TcpInputs.Add(input);
        return channel;
    }

    private static bool TcpTakeInput(out long channel, out RzBox value)
    {
        foreach (RzTcpInput input in TcpInputs)
        {
            input.AcceptPending();
            if (input.TryTakeLine(out string line))
            {
                channel = input.Channel;
                value = MakeString(line);
                return true;
            }
        }
        channel = 0;
        value = Unit;
        return false;
    }

    private static TcpClient ConnectLocalhost(long port)
    {
        ValidatePort("port_out_signal", port);
        TcpClient client = new();
        client.Connect(IPAddress.Loopback, (int)port);
        return client;
    }

    private static void ValidatePort(string name, long port)
    {
        if (port <= 0 || port > 65535)
        {
            Fail($"Runtime error: builtin '{name}' expected port in range 1..65535, got {port}");
        }
    }

    private sealed class RzTimer
    {
        public readonly long Channel;
        public readonly double IntervalSeconds;
        public readonly double StartSeconds;
        public double NextFireSeconds;

        public RzTimer(long channel, double intervalSeconds, double startSeconds)
        {
            Channel = channel;
            IntervalSeconds = intervalSeconds;
            StartSeconds = startSeconds;
            NextFireSeconds = startSeconds + intervalSeconds;
        }
    }

    private sealed class RzPortOutput
    {
        public readonly RzSignal Signal;
        public readonly TcpClient Client;

        public RzPortOutput(RzSignal signal, TcpClient client)
        {
            Signal = signal;
            Client = client;
        }
    }

    private sealed class RzTcpInput : IDisposable
    {
        public readonly long Port;
        public readonly long Channel;
        private readonly TcpListener listener;
        private readonly List<RzTcpClient> clients = new();

        public RzTcpInput(long port, long channel)
        {
            Port = port;
            Channel = channel;
            listener = new TcpListener(IPAddress.Loopback, (int)port);
            listener.Server.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true);
            listener.Start();
        }

        public void AcceptPending()
        {
            while (listener.Pending())
            {
                TcpClient client = listener.AcceptTcpClient();
                client.NoDelay = true;
                clients.Add(new RzTcpClient(client));
            }
        }

        public bool TryTakeLine(out string line)
        {
            for (int i = clients.Count - 1; i >= 0; i--)
            {
                RzTcpClient client = clients[i];
                if (client.TryTakeLine(out line, out bool closed))
                {
                    return true;
                }
                if (closed)
                {
                    client.Dispose();
                    clients.RemoveAt(i);
                }
            }
            line = "";
            return false;
        }

        public void Dispose()
        {
            foreach (RzTcpClient client in clients)
            {
                client.Dispose();
            }
            clients.Clear();
            listener.Stop();
        }
    }

    private sealed class RzTcpClient : IDisposable
    {
        private readonly TcpClient client;
        private readonly List<byte> buffer = new();

        public RzTcpClient(TcpClient client)
        {
            this.client = client;
        }

        public bool TryTakeLine(out string line, out bool closed)
        {
            closed = false;
            NetworkStream stream = client.GetStream();
            while (stream.DataAvailable)
            {
                int value = stream.ReadByte();
                if (value < 0)
                {
                    closed = true;
                    break;
                }
                if (value == '\n')
                {
                    line = Encoding.UTF8.GetString(buffer.ToArray());
                    buffer.Clear();
                    return true;
                }
                if (value != '\r')
                {
                    buffer.Add((byte)value);
                }
            }
            line = "";
            return false;
        }

        public void Dispose() => client.Dispose();
    }
}
