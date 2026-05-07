# 2026-03-05 - supervisor

## The reuses

```rizz
fun stop f s : ('a -> Bool) -> Signal 'a -> Signal 'a = 
  match s with
  | x :: xs -> 
    if f x
    then x :: never
    else x :: (jump (fun a -> Some (a :: xs)) |> xs)
```

Doesn't quite produce what we want. The reset instruction is insereted BEFORE the if expression. This means the tail is still constructed and `s` isn't reused in the `true` branch.

```rizz
fun stop(f, s) =
  match s with
  | let x = proj_0 s in
    inc x;
    inc s;
    let xs = Ctor2(s) in
    let var36 = reset s in
    inc x;
    let var19 = f x in
    match var19 with
    | dec xs;
      dec var19;
      let var29 = reuse var36 in Signal(x, never) in
      ret var29
    | dec var19;
      let var20 = pap lifted_fun6(xs) in
      let var21 = pap jump(var20) in
      let var22 = Ctor0(var21) in
      let var23 = Ctor6(var22, xs) in
      let var28 = reuse var36 in Signal(x, var23) in
      ret var28
```

It is possible, however, to write the program in such a way to allow the algorithm to insert the reuse instruction where it would be necessary. This requires you to place explicit match expression in both branches. That is unfortunate.

```rizzo
fun stop f s : ('a -> Bool) -> Signal 'a -> Signal 'a =  
    if f (head s)
    then match s with | x :: xs -> x :: never
    else 
      match s with
      | x :: xs -> x :: (jump (fun a -> Some (a :: xs)) |> xs)
```

Producing this reference counted IR:

```rizzo
fun stop(f, s) =
  let var19 = proj_0 s in
  inc var19;
  let var20 = f var19 in
  match var20 with
  | dec var20;
    match s with
    | let x = proj_0 s in
      inc x;
      let var35 = reset s in /* s has not been incremeneted before this point, at least 1 instead of at least 2 :')  */
      let var30 = reuse var35 in Signal(x, never) in
      ret var30
  | dec var20;
    match s with
    | let x = proj_0 s in
      inc x;
      inc s;
      let xs = Ctor2(s) in
      let var37 = reset s in
      let var21 = pap lifted_fun6(xs) in
      let var22 = pap jump(var21) in
      let var23 = Ctor0(var22) in
      let var24 = Ctor6(var23, xs) in
      let var29 = reuse var37 in Signal(x, var24) in
      ret var29
```

### Reuse solutions

- We see: could we mark recursive functions as not applicable for reuse, ever? Maybe we can do more fine-grained and just mark functions that recurse on a signal?
- We could also try a transformation that pushes the match expression (on signals) as far down the tree as possible. Think, we don't have to produce a case expression BEFORE the tail is actually used.

- Re-use is probably limited¸ possibly so limited that the only useful case is `stop`.
  - Maybe it happens when you do a lot inlining?
  - It may only happen when a function 'switches' and creates a new signal
  - **Understanding the limitations of the algorithm we are implementing**
- Read the `Reference counting with frame limited reuse` by Lorezen & Leijen (2022) => they also have some "limitations"

### Other allocations

- We could add to a runtime represention in-direct signals, that are points to another signal in the heap. **This would still have to exist in the heap**.
  - It's in the data representiaton of signals.
  - `tail l'` -> follows the pointers all the way to the actual signal. (Same for watch)

## Other questions

- Evaluation => 
  - Evaluation could be running a rizzo program and monitoring the memory, signal heap/counts.  
  - Some examples showing the heap. How do we want to show it?


## Next steps

- Start the report - Form an idea of what "story" we want to tell.
  - Examples: Show a program (could be stop), show what it is compiled to, explain what it does, and show what executing that program does to the heap/ouptus. The example should be simple enough to explain the point ... but if that is a mathy expression or a memory dump, which-ever we think is easiest to read.
