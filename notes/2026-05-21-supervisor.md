## Questions
- Why is there no watch in the evaluation semantics (low priority)
- Feedback related questions -> 
	- Consider moving the reset/reuse for map and stop to a later section? Possibly the next? In section two we are sort of discussing reference counting for a functional language (not necessarily Rizzo).
- Future works:
	-  The section on the signal indirections ... 
		- Fine to mention. This is something can be improved!
		- We need to provide some confirmation/an observation of the problem. Is this something we observed while running a Rizzo program? 
	- Improving `update_heap` to reuse the location of the signal in the heap.
		- That is fine to mention in future work!
	- A built-in/native `mk_sig` which can reuse the `laterapp`, `delay` (and more?)
        - Answer: *"all the observations we made and discovered during/by implementing the language are valuable"*
		- For ourselves to think: Is it more general then `mk_sig`? Is `mk_sig` just an example? Is there a possibility for more general reuse of later values?
	- We discussed at some point the redesigning the heap structure, so we don't have to consider EVERY signal when only some of them depend on the channel.
		- At the core: reducing the number of checks we have to do to when updating the heap.
		- Solutions: 
			- Could pre-compute the clocks (but not sound since clocks may change dynamically). 
			- Annotate (in typing information) -> which other signals/clocks does this signal read from/depend.
## Note self
- Can we do cyclic signals with recursive functions?