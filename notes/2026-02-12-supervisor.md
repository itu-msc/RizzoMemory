# Supervisor meeting

## Going forward

- Complete the rizzo constructor mappings. Signal special case. 
  - SPECIAL CASE SIGNAL: We have decided that it is the concern of the runtime to get a reference to the previous/next siganls in the heap based on the cursor AND correctly reference counting these.

## Questions
- Why do we even need mutable references? What would go wrong if we added a `set` operation to the immutable beans calculus.
	- => probably not, since the language itself never mutates signals, runtime does.

## Notes

Special case for signal =>
- Why? => we want to both allocate a reference on the heap, but we also have to allocate the linked list structure for the signal heap ... 
- It can behave as a constructor of 4 elements, 2 for the signal itself and 2 more for the linked list forward/backwards.
	- Behaves differently from the other constructors semantically.
	- Projections are the same.
Later stuff =>
- Behave just like normal constructors.

Tranformations (Pre-processing) => Compiled Async Ratt. ETA expansion, lifting etc.
- Slightly different since they have explicit `PAB`. 
- Look into the issues they had along the way, much to learn for us

Look through paper for recursive definitions => 
- Inference of ownership - double-check infer_all(\_simple) for recursive defintions. Collect may have to note which function it is actually collecting ownership for.
- Can we partially apply variable applications
