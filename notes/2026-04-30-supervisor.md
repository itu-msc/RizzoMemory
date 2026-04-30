# 2026-04-16 - supervisor

There is now an install script!

## Questions

### Later values are re-computed unless you use tail/watch

Consider the program below:

```ml
fun entry x =
    let console_sig_later = mk_sig (wait console) in
    let signal1 = "" :: (map (fun x -> "signal1: " + x) |> console_sig_later) in
    let signal2 = "" :: (map (fun x -> "signal1: " + x) |> console_sig_later) in
    let _x = console_out_signal signal1 in
    let _x = console_out_signal signal2 in
    start_event_loop ()
``` 

Here the delayed computation `console_sig_later` is evaluated twice, once for `signal1` and once for `signal2`, resulting in two console signals. This is expected behaviour but it is a flaw (in the language Rizzo rather than our implementation) because you would reasonably expect the later to only be computed once. 

**Discuss this in the report!** $\Rightarrow$ and we should suggest a fix: <ins>treat delayed computations as lazy values</ins>, that is cache the results for the current time step. A later becomes a triple of $\langle l, val?, stp\rangle$ where $l$ is the delayed computation, $val?$ is the cached value (if any), and $stp$ holds the time step for which the value is valid. This requires maintaining a time-step-counter that we can compare $stp$ against.


### Signal combinators like $map$ create wasteful **inc** and **reset** instructions since the input signals are never valid for reuse at runtime. 

This is unfortunate, let us consider it a flaw since you to write your programs with a particular shape in order to avoid these (effectively) dead instructions.

**Discuss in report!** $\Rightarrow$ a fix is to STOP/HALT **reset**/**reuse** if we observe the $\mathsf{tail}$ constructor is used in a recursive call.

There is no need for the **inc**, if the **reset** wasn't inserted. See the program below which has $map$ with a match and $map$ with no match.
```ml
f: Owned, s: Owned
fun map_match(f, s) =
  match s with
  | #0 let sig_head152 = proj_0 s in
       inc sig_head152;
       inc s;
       let sig_tail151 = Ctor2(s) in
       let var446 = reset s in
       inc f;
       let var277 = f sig_head152 in
       let var278 = pap thunk_3_ALL_OWNED497(f) in
       let var279 = Ctor0(var278) in
       let var280 = Ctor6(var279, sig_tail151) in
       let var288 = reuse var446 in Signal(var277, var280) in
       ret var288
  | default dec s;
            dec f;
            let var289 = match_fail("Non-exhaustive pattern match") in
            ret var289

f: Owned, s: Owned
fun map_no_match(f, s) =
  let var281 = proj_0 s in
  inc var281;
  inc f;
  let var282 = f var281 in
  let var283 = pap thunk_1_ALL_OWNED499(f) in
  let var284 = Ctor0(var283) in
  let var285 = Ctor2(s) in
  let var286 = Ctor6(var284, var285) in
  let var287 = Signal(var282, var286) in
  ret var287
```

and here is the `map_no_match` when disabling **reset**/**reuse** insertion:

```ml
f: Owned, s: Owned
fun map_match(f, s) =
  match s with
  | #0 let sig_head152 = proj_0 s in
       inc sig_head152;
       let sig_tail151 = Ctor2(s) in
       inc f;
       let var277 = f sig_head152 in
       let var278 = pap thunk_3_ALL_OWNED455(f) in
       let var279 = Ctor0(var278) in
       let var280 = Ctor6(var279, sig_tail151) in
       let var288 = Signal(var277, var280) in
       ret var288
  | default dec s;
            dec f;
            let var289 = match_fail("Non-exhaustive pattern match") in
            ret var289
```
