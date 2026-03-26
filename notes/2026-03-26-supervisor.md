## General
- Writing the reset/reuse section => Throwing examples at it is fine.
	- Fields/arguments => ... 
	- Closing part could be the stop with reference counting. In intermediate language
- Semantics: 
	- Examples? => an example that uses both signal combinators and reuse/reset.
		- zip combinator, should reuse the pair allocated on the heap
	- Not sure it matters to examples of semantic rules. Gets hairy, way too quick. Not necessary for our thesis.
- Potential improvements:
	- Topological sort (maybe)
	- Can we split the heap? => Represent the data flow at runtime. 
		- How do you get to that graph => We need to compute the clocks of later values. Can you build a graph over the clocks? Is it good enough that later Z depends on later Y and just follow the ?
		- Update semantics => checks if the later has ticked (did the attached clock tick?)
		- 2 disjoint clocks are fine to run in parallel.
		- Timing dependency != data dependency
			- Why not? => Could it be that `watch` depends on the data also? Waits for the later on the other signal but then ALSO that it produced the right thing?
		- Second linked list => if clocks are pre-calculated. The second linked list could tell where to jump to the next signal not is not disjoint. Where shortcuts take you to the next non-disjoint signal (or previous)
	- Costs at runtime but we have to amortise the improvements
		- How much do we gain from short cutting? 
		- Versus what does it cost to calculate the clock?
	- Consider: A nested delay, first wait for A, only then wait for B.
	  `(fun x' -> (fun y' -> y x ...) |> y) |> x`
	- **Could write a section/discussion on potential for optimisation based on this**
		- Discuss benefits and additional costs.
		- Why does it make sense? Is the shape of Rizzo programs such that it makes sense to do these additional computations.

## Future steps
- 