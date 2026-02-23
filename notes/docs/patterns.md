# Patterns

> Note the info in this file is just a guiding introduction and can deviate slightly from the actual implementation.

This file tries to answer: *'How do we handle pattern matching?'*.

Consider the following code (in the reference counted IR):

```rizz
let mycons = Ctor3 (...) in
case mycons of
(do something ctor1)
(do something ctor2)
(do something ctor3) // <-- this is the one that matches
```

Casing (matching) on the `i`th constructor of a type, means jumping to/taking the `i`th branch of a case expression. This `i` (hereafter referred to as the `tag`) is stored in the header of a constructor at runtime and it is this `tag` that dictates the branch at runtime.

## Constants

In our version of the Rizzo language, we have chosen to allow *pattern matching* with constants as in '*take only this branch when the scrutinee has this constant value*'.

The code below matches on an int value *myint*. Depending on the value of *myint* it may call a different function. Say *myint* is 4, then the program should execute 'do\_something\_if\_4'. 

```rizz
match myint with
| 4 -> (do_something_if_4 ())
| 5 -> (do_something_if_5 ())
| _ -> (do_something_default ())
```

We have decided to translate such patterns to a sequence of if-then-else-expressions. For this to work, we will consider Boolean values as constructors, where `true`has 0 tag and `false` has 1 tag (this is preserve order of a traditional if-then-else, where true branch goes first).

We translate the *myint* program above into the reference-counted IR below:

```rizz
let myint = Ctor1 5 in
let boolRCtor = equality myint 4 in     // <- does the check and saves a boolean value
case boolRCtor of 
(do_something_if_4 ())                  //this branch is taken if 'tag_of(boolRCtor) == 0'
(let boolRCtor2 = equality myint 5 in   //this branch is taken if 'tag_of(boolRCtor) == 1'
  case boolRCtor2 of
  (do_something_if_5 ())                //Branch is taken if 'tag_of(boolRCtor2) == 0'
  (do_something_default ()))            //None of the constant patterns matched, so do default branch
```


## Tuples

This is just an example of how we can match on tuples. The first branch will never be taken, as the pattern does not match the shape of the scrutinee. The second branch is a catch-all, and it will be taken for any tuple (including the one we are matching on).

Source:

```rizz
let mytuple = ((3, 4), 2) in
match mytuple with
| ((3,4), c) -> (do something with a, b, c) // <-- this is the one that matches
| (a,b) -> (do something with a, b) // this would have caught everything else
```

Lowered shape (using our equality + bool case):

```rizz
let mytuple = ETuple (ETuple (RConst 3, RConst 4), RConst 2) in
let t0 = proj_0 mytuple in
let t1 = proj_1 mytuple in
let t00 = proj_0 t0 in
let t01 = proj_1 t0 in
let b0 = equality t00 3 in
case b0 of
| true  ->
  let b1 = equality t01 4 in
  case b1 of
  | true  -> let c = t1 in e1
  | false -> let a = t0 in let b = t1 in e2
| false -> let a = t0 in let b = t1 in e2
```
