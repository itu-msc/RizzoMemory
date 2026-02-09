# Milestones and progress tracking

## Translating 'Counting Immutable Beans' calculus to OCaml

We have so far implemented section 5's step 3 and 2, which are 'Inserting inc/dec instructions (Section 5.3)' and 'Inferring borrowed parameters (Section 5.2)' respectively.

We have added the insertion of inc/dec instructions based on Figure 5 of the paper, and we have added the inference of borrowed parameters based on Figure 4 of the paper.

An example of a mapping from the paper to our code is as follows:

 Paper                       | Code                             |
-----------------------------|----------------------------------|
 C (case x of 𝐹 , 𝛽𝑙)        | FnCase (x, fs) as case           |
 C (let z = pap c 𝑦 ; F, 𝛽𝑙) | FnLet (z, RPartialApp(c, ys), f) |

The code for the insertion of inc/dec instructions is in `src/lib/refcount.ml`. The main function for doing the insertion is [insert_rc](../src/lib/refcount.ml#L116), and the code for the inference of borrowed parameters is in [collect](../src/lib/refcount.ml#L157).

We have also implemented the examples of section 5.3, which have been added to [examples.ml](../src/bin/examples.ml).
It can be executed with `dune exec rizzoc` and the output is as expected.
