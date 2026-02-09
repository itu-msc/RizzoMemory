# Project Plan and progress for Rizzo

## Counting immutable beans paper (Ullrich & de Moura)

- [x] Impliment basic increment and decrement functions
- [x] Add Collect for Borrowed signature inference
- [ ] Add `reset`/`reuse`
- [x] Consider using a *reverse call graph* for `collectO` for performance.
- [ ] Transform Rizzo AST to an intermediate language that fulfil the assumptions on page 4-5 of Ullrich & de Moura. This includes at least:
  - [ ] Eta expansion
  - [ ] Lambda lifting
  - [ ] Copy propagation (to eliminate trivial bindings of the shape `let x = y in ...`)
  - [ ] Dead let-binding (code) elimination
  - [ ] All parameter- and let-names must be unique in a scope. To allow shadowing or not ...
  - [ ] A-Normalization (ANF, Flanagan et al.). May have to be more aggressive, since we need all arguments to be variables.

## Adapting immutable beans for Rizzo expressions(?)

- [x] How do we represent necessary Rizzo constructors (delayed, later, signal, sum + tuples(?)) => as ctors
- [x] Do we keep around the distinction between `fn_body` and `(r)expr`? => yes, limit how much we change the actual inc/dec code.
- [ ] How do the semantics of Rizzo constructions change the inc/dec rules?

## Perceus paper

- [ ] Note down the performance goals of Perceus
- [ ] Implement a basic version of the Perceus optimizations

## Evaluate the memory management system

- [ ] Decide: How we woud like to evaluate, which activities does it include? Measurements? Etc.
- [ ] Evaluate our adaptation of Ullrich & De Moura.
- [ ] Evaluate our use of Perceus(?)

## Rizzo Syntax design

- [ ] Design syntax for Rizzo
- [ ] Make code examples

## Rizzo type checking

- [ ] We could do bidirectional typing
- [ ] Implement typechecker
- [ ] (OPT) Implement type inference

## Implement LSP server for Rizzo (if time permits)

- [ ] Set up basic LSP server
- [ ] Add syntax highlighting
- [ ] Add error reporting
- [ ] Add code completion
- [ ] Add go-to-definition
- [ ] Add other features as time permits

## Write up Rizzo paper

- [ ] Write introduction and motivation
- [ ] Describe the syntax and semantics of Rizzo
  - [ ] Describe the Ullrich & De Moura work (?)
- [ ] Explain the implementation details
  - [ ] Memory management
