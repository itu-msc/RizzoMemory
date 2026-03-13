# Project Plan and progress for Rizzo

## Priority order of activities

1. Read the new paper
2. Fix the runtime?
3. Start on writing the paper
4. Make some example programs that we can analyse the memory management of, and use them in the paper.
5. Based on findings, consider next steps

## Counting immutable beans paper (Ullrich & de Moura)

- [x] Implement basic increment and decrement functions
- [x] Add Collect for Borrowed signature inference
- [x] Add `reset`/`reuse`
- [x] Consider using a *reverse call graph* for `collectO` for performance.
- [ ] Transform Rizzo AST to an intermediate language that fulfil the assumptions on page 4-5 of Ullrich & de Moura. This includes at least:
  - [ ] Eta expansion ONLY CONSTRUCTORS
  - [x] Lambda lifting
  - [x] Copy propagation (to eliminate trivial bindings of the shape `let x = y in ...`)
  - [x] Transform applications on (local) variables to a series of 1 apps, Or just change the `Refcount.VarApp` to take a list of primitives.
    - We currently have variable applications take one argument in the reference counted IR.
  - [x] Dead let-binding (code) elimination
  - [x] All parameter- and let-names must be unique in a scope. To allow shadowing or not ...
  - [x] A-Normalization (ANF, Flanagan et al.). May have to be more aggressive, since we need all arguments to be variables.
  - [x] Add consecutive lambda elimination (lowers nested lambdas to a single function with multiple parameters, e.g. `fun x -> fun y -> e` becomes `fun (x, y) -> e`)
  - [ ] Implement the suggestion in section "Preserving tail calls" on page 8 of Ullrich & de Moura.
- [x] Allow constant types and ensure they are not reference counted (e.g. `int`, `string`, `bool`)
- [ ] Read and consider the findings from the paper "Reference Counting with Frame Limited Reuse"

## Adapting immutable beans for Rizzo expressions(?)

- [x] How do we represent necessary Rizzo constructors (delayed, later, signal, sum + tuples(?)) => as ctors
- [x] Do we keep around the distinction between `fn_body` and `(r)expr`? => yes, limit how much we change the actual inc/dec code.
- [ ] How do the semantics of Rizzo constructions change the inc/dec rules?
- [ ] Push down tail instructions as far down the AST as possible, to allow more reuse opportunities.
- [ ] Ensure that `delay` calls are lazy evaluated

## Perceus paper

- [ ] Note down the performance goals of Perceus
- [ ] Implement a basic version of the Perceus optimizations

## Evaluate the memory management system

- [ ] Decide: How we would like to evaluate, which activities does it include? Measurements? Etc.
- [ ] Evaluate our adaptation of Ullrich & De Moura.
- [ ] Evaluate our use of Perceus(?)

## Rizzo Syntax design

- [ ] Design syntax for Rizzo
- [ ] Make code examples
- [ ] Add pattern matching in function parameters (if time permits)
  - Would help make functions able to take unit as an input, which is useful for writing functions that are only executed for their side effects (e.g. `print`).
- [ ] Consider Recursion based on Rizzo language rules (if time permits)

## Implement Rizzo Runtime (Transpiler)

- [x] Implement an initial transpiler from Rizzo to C
- [ ] Ensure the transpiler correctly handles the memory management system (reference counting, reuse, etc.)
- [ ] Add a pointer representation of signals (Important in the advance semantics)
  - [ ] `head` needs to be a function application
  - [ ] `tail` and `watch` needs to dereference the pointer to get the function of the real signals tail.
- [ ] Optimize type guard checks in the transpiler (e.g. for `signal` and `later`), to avoid unnecessary checks at runtime.

## Rizzo type checking

- [x] We could do bidirectional typing
- [x] Implement type checker
- [x] (OPT) Implement type inference
- [ ] Fix bugs as we find them :)

## Implement LSP server for Rizzo (if time permits)

- [x] Set up basic LSP server
- [x] Add syntax highlighting
- [x] Add error reporting
- [ ] Add code completion
- [x] Add go-to-definition
- [ ] Add other features as time permits

## Write up Rizzo paper

- [ ] Write introduction and motivation
- [ ] Describe the syntax and semantics of Rizzo
  - [ ] Describe the Ullrich & De Moura work (?)
- [ ] Explain the implementation details
  - [ ] Memory management

### paper structure

- Introduction
  - Motivation for the project
  - Goal of the project
- What is FRP and why is it useful?
  - Overview of Rizzo
- Counting Immutable Beans
  - Explanation of the paper by Ullrich & De Moura
  - Examples
  - Important AST transformations we need
- Rizzo lang
  - Syntax and semantics of Rizzo
    - very short reference to the grammar
  - Types
  - Memory model (heap)
- Inserting reference count instructions in Rizzo (RC IR)
  - Assumptions (refreshing the readers memory)
  - pure AST transformations (to satisfy the beans paper assumptions)
  - How we adapted the memory model for Rizzo (e.g. delayed, later, signal, sync ...)
- (Transpiler ?)
- Evaluation
  - Running a Rizzo program (maybe different section), we need to show we it means to execute. What does the heap look like etc.
  - Performance goals
  - Results
- Limitations
  - Combinators don't allow for full reuse -> show `map`. And show a case where it does reuse, `stop`.
- Literature review
- Conclusion and future work
