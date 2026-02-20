
# Rizzo v1 Syntax Specification (Draft)

This document defines a minimal, precise v1 surface syntax.

---

## 1) Scope of v1

Included in v1:

- Top-level value/function definitions
- Expressions: variables, literals, tuples, application, lambda, local let, match, `::`, `|>`
- Type annotations on expressions
- Core type syntax

Out of scope for v1:

- Modules/imports
- Type declarations (`type ...`)
- Type classes/traits
- String interpolation
- Multiline string literals
- Guards in match cases

---

## 2) Lexical structure

### 2.1 Whitespace and newlines

- Whitespace separates tokens.
- Newlines are not semantically significant (except inside comments/strings).

### 2.2 Comments

- Single-line: `// ...` until end of line.
- Multi-line: `/* ... */`.
- Nested multi-line comments are **not** supported in v1.

### 2.3 Identifiers

- Value identifiers (variables/functions):
  - Start: `[a-z_]`
  - Continue: `[a-zA-Z0-9_']*`
  - Examples: `x`, `map'`, `_tmp2`
- Type constructors:
  - Start: `[A-Z]`
  - Continue: `[a-zA-Z0-9_']*`
  - Examples: `Int`, `Bool`, `Signal`
- Type variables:
  - Form: `'` followed by value-identifier body
  - Examples: `'a`, `'elem`

### 2.4 Reserved keywords

`let`, `fun`, `in`, `match`, `with`, `true`, `false`

### 2.5 Reserved symbols

`(` `)` `,` `=` `:` `->` `|` `::` `|>`

---

## 3) Literals

### 3.1 Integer

- Decimal integers only in v1.
- Grammar: `[0-9]+`

### 3.2 Boolean

- `true` | `false`

### 3.3 Unit

- `()`

### 3.4 String

- Delimited by double quotes: `"..."`.
- Allowed escapes in v1:
  - `\\` (backslash)
  - `\"` (double quote)
  - `\n`, `\r`, `\t`
  - `\u{HEX}` where `HEX` is 1–6 hex digits
- Invalid escape sequences are lexical errors.
- Strings cannot span multiple lines in v1.

---

## 4) Grammar (EBNF)

```ebnf
program      ::= top_decl*

top_decl     ::= "let" ident "=" expr
                 | "fun" ident param+ "=" expr

param        ::= ident

expr         ::= let_expr

let_expr     ::= "let" ident "=" expr "in" expr
                 | match_expr

match_expr   ::= "match" expr "with" match_case ("|" match_case)*
                 | lambda_expr

match_case   ::= pattern "->" expr

lambda_expr  ::= "fun" param+ "->" expr
                 | pipe_expr

pipe_expr    ::= cons_expr ("|>" cons_expr)*

cons_expr    ::= app_expr ("::" cons_expr)?

app_expr     ::= atom atom*

atom         ::= ident
    | int_lit
    | bool_lit
    | string_lit
    | "()"
    | "(" expr ")"
    | "(" expr "," expr ")"
    | "(" expr ":" type_expr ")"

pattern      ::= "_"
    | ident
    | "(" pattern ")"
    | "(" pattern "," pattern ")"
    | pattern "::" pattern
    | "()"
    | "true"
    | "false"
    | int_lit
    | string_lit

type_expr    ::= fun_type

fun_type     ::= prod_type ("->" fun_type)?

prod_type    ::= app_type ("*" app_type)*

app_type     ::= type_atom type_atom*

type_atom    ::= type_ident
   | type_var
   | "(" type_expr ")"
```

Notes:

- `app_expr ::= atom atom*` means function application is left-associative.
- `cons_expr` makes `::` right-associative.
- `pipe_expr` makes `|>` left-associative.
- Match cases do not allow guards in v1.

---

## 5) Precedence and associativity

Highest to lowest:

1. Atom (`x`, literals, parenthesized expr)
2. Application (`f x y`) — left-associative
3. Cons (`::`) — right-associative
4. Pipe (`|>`) — left-associative
5. Lambda (`fun ... -> ...`) — binds weakly
6. Match (`match ... with ...`) — binds weakly
7. Let-in (`let x = ... in ...`) — weakest

---

## 6) Type syntax conventions

- Function type arrow is right-associative:
 	- `Int -> Int -> Int` means `Int -> (Int -> Int)`.
- Product `*` binds tighter than `->`:
 	- `A * B -> C` means `(A * B) -> C`.
- Type application is left-associative:
 	- `Signal Int` means `(Signal Int)`.
 	- `Either A B` means `((Either A) B)`.

---

## 7) Desugaring rules

- Top-level function declaration:
 	- `fun f x y = e`
 	- desugars to
 	- `let f = fun x y -> e`

- Pipe:
 	- `a |> f` desugars to `f a`
 	- `a |> f b` desugars to `(f b) a` only if parsed as `(a |> (f b))`
 	- To avoid ambiguity, prefer parentheses in user code.

---

## 8) Errors and constraints (v1)

- Duplicate parameter names in the same lambda/function are rejected.
- Empty match (`match e with`) is rejected.
- Invalid string escape is a lexical error.
- Using reserved keywords as identifiers is rejected.

---

## 9) Examples

```rizz
let id = fun x -> x

fun const x y = x

let pair = (1, "ok")

let pipeline =
 1
 |> id

let xs = 1 :: 2 :: 3 :: ()

let local =
 let x = "hello\n" in
 x

let out =
 match xs with
 | h :: t -> h
 | _ -> 0

let typed = (id : Int -> Int)
```

Global's/top level

```
let x = e
fun x params = e
```

expressions

```
e ::= 
| x --variables
| e es --application
| (e,e)
| e :: e   --Binary SigCons
| e |> e --binary pipe (>)
| match e with |? <case> -> expresion | ... -- patteren matching, | can be on new lines
| fun xs -> e
| let x = e in e
```

Comments using `//` for single line and `/* */` for multi-line

```
let <name> = expresion
fun <funName> <params ...> = { expresion } -- Implicit return of value on last line
fun <params ...> -> expresion -- lambda functions
<funName> <param> <param> ... -- applying a function
```

type (annotations). Capitalized, because ?

```
Int -> Int -> Int   -- functions with '->'
Signal A            -- polymorphic types go left-to-right
A * B               -- tuples?

(e: type)           -- annotations?
```

Strings:

```
"string" -- double quotes for strings
"\n"     -- escape sequences
```
