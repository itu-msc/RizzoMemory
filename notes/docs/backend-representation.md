# Backend representation and typed compilation

Created on the 18/03-2026

This note collects the current understanding of how the Rizzo compiler uses type information, what is erased before code generation, and how we could exploit compile-time types more directly in the backend.

The short version is:

- the compiler is statically typed, but the backend currently uses a mostly uniform runtime representation
- this is not the same as interpreting the program in C, but it does mean we are leaving some type-directed optimizations on the table
- we probably should not try to remove all boxing
- the best next step is a hybrid backend that keeps primitive values unboxed when possible, while preserving boxed runtime representations for higher-order values, constructors, and signals

## What the current compiler is doing

The current pipeline type checks expressions in `OCaml`, but most of that type information is erased before the later lowering passes and before C emission.

In practice, the compiler currently uses type information for things like:

- rejecting invalid programs during type checking
- deciding which overloaded operation a source operator means
- assigning built-in types and checking built-in use sites

An example is `+`, which is lowered differently depending on whether the type checker concluded that the operands are `Int` or `String`.

However, the generated C code still mostly uses a shared runtime value representation. The central ABI is `rz_box_t`, and most generated functions take and return boxed values. Constructor fields, closure captures, delayed values, and signal payloads are all represented in terms of that runtime model.

So the backend is best described as:

- ahead-of-time compiled
- statically type checked at the source level
- mostly type-erased by the time representation choices matter

That is not the same thing as a dynamic interpreter, but it does mean compile-time type information is not yet used to drive many low-level layout decisions.

## Are we wasting compile-time type information?

Partly yes.

The current design gets correctness and some type-directed lowering, but it does not yet fully exploit static types for code generation.

Examples of information we currently do **not** exploit much:

- whether a local binding can be stored as a raw `int32_t`
- whether a direct function call can use a typed C signature instead of the boxed closure ABI
- whether a primitive comparison can avoid generic boxed equality
- whether a binding participates in reference counting at all

That said, it is also important not to overstate the problem.

Not everything is equally boxed today. Integers are already immediate values inside `rz_box_t`; they are not heap allocated the way constructors, strings, closures, or signals are. So some low-level specialization is already present. The bigger issue is that the runtime interface is still mostly uniform, and most values are forced through that interface.

## Why not just compile everything directly to C values?

Because some language features genuinely benefit from a uniform runtime representation.

The main examples are:

- first-class functions and partial application
- algebraic data represented as runtime constructor objects
- polymorphic built-ins and generic equality
- reference counting and object reuse
- signals, delayed values, and the reactive heap

These features do not make a fully typed C backend impossible, but they do make it more complex.

For example, if every function had a fully typed C signature, then higher-order functions and partial applications would need either:

- monomorphization of all relevant functions, or
- multiple calling conventions, or
- a second boxed closure ABI anyway

Likewise, if every constructor field had a specialized layout, then tuples, options, sync values, and future user-defined constructors would need either many specialized C structs or a more complicated generic object system.

So the right conclusion is not "all boxing is wrong". The better conclusion is:

- some boxing is structural and useful
- some boxing is incidental and can likely be removed from hot monomorphic paths

## Signals need special treatment

Signals are the clearest example of a value that should probably stay as a runtime object.

A signal is not just a value of type `Signal a`. It has runtime identity and mutable reactive state. It lives in the global heap, links to other signals through `prev` and `next` pointers, participates in reference counting, and is updated over time as channels tick.

That means a signal is not just a compile-time shape. It is a runtime cell.

So even in a more type-directed backend, it still makes sense for `Signal a` to compile to a heap object with stable identity.

The real question is whether the signal payload should remain boxed.

There are two broad choices:

### 1. Keep generic signal payloads

This means the signal head and tail continue to use the boxed runtime representation.

Advantages:

- much simpler runtime
- works naturally with polymorphism
- works naturally with generic heap update logic
- keeps `watch`, `tail`, `sync`, and related operations uniform

Disadvantages:

- primitive payloads inside signals cannot be compiled all the way down to native C values

### 2. Specialize signal payloads by type

This means introducing several runtime signal layouts such as signal-of-int, signal-of-bool, and so on.

Advantages:

- payload access can become more direct
- less boxing in reactive code

Disadvantages:

- much more complicated runtime
- more complex heap update logic
- harder support for polymorphism and generic reactive combinators
- likely code duplication across signal and later representations

For this project, the safer and more pragmatic direction is to keep signals generic at first and only consider payload specialization later if profiling shows a real need.

## Recommended direction: a hybrid backend

The most realistic next design is a hybrid representation strategy.

The core idea is:

- use raw native C values for monomorphic primitive locals and direct primitive computations
- keep boxed runtime values at the boundaries where the language genuinely needs a uniform representation

Those boundaries include:

- constructor fields
- closure captures
- partial applications
- polymorphic calls
- signal payload storage
- delayed values and later values
- generic built-ins such as boxed equality

With this strategy, ordinary first-order integer code can become much more direct, without forcing the whole language runtime to become fully type-specialized.

## What would become unboxed first?

The best first candidates are:

- `Int`
- `Bool`
- channel identifiers

These values can often stay unboxed in local temporaries, arithmetic, comparisons, and direct calls.

For example, instead of compiling a monomorphic integer function through the boxed ABI everywhere, we could generate:

- a raw typed helper for direct known calls
- a boxed wrapper when the function is used as a value or captured in a closure

This is a strong compromise because it preserves higher-order behaviour while still giving the backend room to generate normal C arithmetic on hot paths.

## What should probably stay boxed for now?

The following should likely remain boxed in the first version of a typed-aware backend:

- constructors and tuples
- options and sync values
- closures and partial applications
- strings
- signals
- later and delay values

This keeps most of the current runtime intact and avoids turning the redesign into a complete rewrite.

## Annotation stage or a new IR?

One design question is whether backend representation information should live in:

- a new typed lower IR, or
- a new annotation on the existing typed AST

The incremental answer is that a new annotation stage is enough to get started.

We already have stage-indexed annotations in the AST. So one practical approach is to add another stage after type checking that stores:

- the source type
- the chosen backend representation

Conceptually, a representation annotation could classify expressions as something like:

- integer-like primitive
- Boolean primitive
- channel primitive
- boxed constructor/object value
- closure value
- signal value
- delayed or later value

This would let later passes decide:

- whether a local needs reference counting
- whether to emit a C local as `int32_t` or `rz_box_t`
- whether a call can use a direct raw helper or must go through the boxed ABI

### Why annotations are enough at first

If the representation decisions are still mostly local, annotations are a good fit.

Examples:

- this local binding is represented as an unboxed integer
- this expression must stay boxed because it flows into a constructor field
- this call target has a direct-call fast path

This approach has the advantage that it preserves most of the current compiler structure.

### When a new IR becomes useful

A separate IR becomes attractive once the backend needs to represent operations that do not really exist in the source language anymore.

Examples include:

- explicit box and unbox operations
- direct call versus closure call as different instruction kinds
- explicit runtime boundary crossings
- representation-aware reference counting instructions

At that point, a dedicated lower IR is cleaner because the structure of the program itself changes, not just its metadata.

So the practical recommendation is:

- start with a new annotation stage
- only introduce a new backend IR if the backend logic becomes too implicit or too branch-heavy

## What changes in reference counting?

Reference counting should become representation-aware.

Today, many parts of the pipeline assume a fairly uniform runtime value model. In a hybrid backend, that should change.

Primitive locals such as raw integers, booleans, and channel IDs do not need inc or dec operations. Heap-backed values still do.

So the ownership story becomes:

- primitive locals: no refcounting
- boxed constructor values: refcounted
- closures: refcounted
- signals: refcounted
- delayed and later values: refcounted

This is another reason a representation annotation is useful even before any new IR is introduced.

## A realistic migration plan

The recommended order of work is:

### Phase 1: preserve representation information after typechecking

Add a new stage after `typed` annotations that records the chosen backend representation for each expression and binding.

### Phase 2: exploit it in the C backend for primitive locals

Generate raw C locals for simple monomorphic primitive code paths.

### Phase 3: add direct-call fast paths

For known monomorphic functions, emit a raw typed C helper for direct calls and keep the boxed wrapper for closure use.

### Phase 4: make refcounting representation-aware

Teach the RC pipeline to ignore primitive locals and only manage heap-backed values.

### Phase 5: only then consider deeper specialization

Examples would include:

- selective monomorphization
- specialized constructor layouts
- specialized signal payloads

These are possible future directions, but they should come after the hybrid approach proves useful.

## Conclusion

The compiler is currently static in the front end but mostly type-erased in the backend.

That means there is real room to use compile-time type information better. But the goal should not be to remove every trace of boxing. Some runtime structures, especially signals and higher-order values, are naturally represented as boxed heap objects.

The most practical next step is therefore:

- keep types or representation choices alive after typechecking
- store them in a new annotation stage first
- use them to unbox primitive locals and direct known calls
- preserve the boxed runtime model for constructors, closures, and signals

This gives a concrete path toward better generated C without forcing a complete redesign of the runtime system.
