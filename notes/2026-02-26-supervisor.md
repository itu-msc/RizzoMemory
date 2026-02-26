# 2026-02-26 - supervisor

## Question
- Pattern matching -> how to?
	- How complicated should we go?
	- Case trees (decision trees)
	- Nice to have - certainly not needed!
- Tail constructor => map cannot reuse the input signal.
	- Answer: We cannot reuse signal in map because it will have to be re-used in the future.
	**100% we should have an example of why reuse is not possible in map (and the like)**
	Consider:
	```
	let t = const 0      --> creates heap location l
	let s = map (+1) t   --> creates heap location l2
	Heap:
	  l -> (0, never)
	  l2 -> (1, delay (map (+1)) (>) tail l)
	```
- Reuse in advance/reactive semantics =>
	- Where does the re-use actually happen? => 
		- it does happen in the advance step. When advancing the latearpp for a `map` computation, it would allocate a fresh signal and THAT one can be used.
	- (MAYBE): Write the reactive semantics in the `beans_pure` language. That way we get the reference-count instructions for free!
		- Probably need more language features => 
			- Lookup next signal, get that signal and move cursor.
			- MUTATIONS ARE BACK ON THE TABLE
	- We could store the instructions for `advance` "inline", instead of just the later value.
		- Then it would be possible to insert a `reuse heap_cursor` in front, before evaluating the tail. (We have a picture).
		```
		Heap:
		  l1 -> (0, never)
		  l2 -> (1, delay (map (+1)) (>) tail l1)
		
		Instead store:
		  l1 -> (0, never / nop)
		  l2 -> (1, let d = delay (map (+1)) (>) tail l1
		  			let s = reset l2 in
					case d of
					(...) -> ... (h' :: t'))
		```
		then insert a `reuse s in (h' :: t')` ...  	
		- **Alternatively, less cool**: Do a global pool of `reusable signals`, then we must remember to "remove" the signal from the linked list structure.

