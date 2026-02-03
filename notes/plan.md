# Project Plan and progress for Rizzo

## Counting immutable beans paper
- [x] Impliment basic increment and decrement functions
- [ ] Add Collect for Borrowed signature inference
- [ ] Add `reset`/`reuse`
- [ ] Consider using a *reverse call graph* for `collectO` for performance.

## Adapting immutable beans for Rizzo expressions(?)
- [ ] How do we represent necessary Rizzo constructors (delayed, later, signal, sum + tuples(?))
- [ ] Do we keep around the distinction between `fn_body` and `(r)expr`?
- [ ] How do the semantics of Rizzo constructions change the inc/dec rules?

## Perceus paper
- [ ] Note down the performance goals of Perceus
- [ ] Implement a basic version of the Perceus optimizations

## Rizzo Syntax design
- [ ] Design syntax for Rizzo
- [ ] Make code examples

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

