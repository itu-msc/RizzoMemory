# Reference counting

We tried to implement the reference counting of [1].

```
[1] Lorenzen, Anton, and Daan Leijen. “Reference Counting with Frame Limited Reuse.” Implementation and Benchmarks for “Reference Counting with Frame Limited Reuse” 6, no. ICFP (2022): 103:357-103:380. https://doi.org/10.1145/3547634.
```

The goal was just to (double) check our suspicion that their technique solves a problem we faced during our [masters thesis](https://github.com/itu-msc/RizzoMemory), and it did!

Input program as seen in `bin/main.ml`:
```ml
\f -> \s -> 
  match s with 
  | SigCons(x, _) -> 
    let xs = Tail(s) in
    let hd = f x in 
    let map' = map f in 
    let delayed_map = delay map' in 
    let tl = laterapp delayed_map xs in 
    SigCons(hd,tl)
```

The transformed program, output by running `bin/main.ml`
```ml
\f -> \s -> 
  match s with 
  | SigCons(x, _) -> 
    dup x; 
    let xs = Tail(s) in 
    let hd = dup f; f x in 
    let map' = map f in 
    let delayed_map = delay map' in 
    let tl = laterapp delayed_map xs in 
    SigCons(hd,tl)
```

The algorithm we used for our masters thesis would instead produce something similar to:
```ml
\f -> \s ->
  match s with
  | SigCons(x, _) ->
    dup x; dup s;
    let xs = Tail(s) in
    r <- dropru s;
    dup f;
    let hd = f x in
    let map' = map f in
    let delayed_map = delay map'
    let tl = laterapp delayed_map xs in
    SigCons@r(hd,tl)
```
The output programs will always fail to reuse `s`, the `dup s` and `r <- dropru s` are wasted.
