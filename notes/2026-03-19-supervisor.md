# Meeting 2026-03-19 (supervisor)

## next steps
- Evaluation chapter?
	- Evaluate/measure the stuff we already have
	- Step it up in proper testing style, where we can call `dune runtest` and runs all the way to C (integration testing style).
		- May require more input - especially clock test
	- A series of small programs and some bigger programs.
	- `How does the heap size evolve over time?`
		- Same heap size after step 100 as there were in step 1
		    - Except if we use switch, then heap size could change.
		- Program that goes back and forth, between two modes.

## Notes

### Introduction format

There is a real danger of repeating ourselves, but think of it as we are just ZOOMING into the subject.

Fix:
- A sentence or 2 to bridge the gap between issues of FRP and Rizzo. Then another sentence or two after the Rizzo to make it flow better.
- Spelling error: First-class (hyphen)

### What is interesting when presenting "semantics"?
- We have this memory layout in C - because Rizzo says we must do this and that
    - Answer: We have to say that CTOR and our specialised signal are heap allocated
    - How far can we get away with this informally? Well, we have already started. We have mentioned inc/dec/reset/reuse in section 2.
    In Section 3, Rizzolang: we can then say we add htree additional (Signal, Later, Delay) constructors (or how we map these). Mention that we handle signal (cons) in special way: they are also heap allocated and reference counted but importantly they also go into the linked list
- This is probably best explained with a conceptual example
    - Show the heap structure, show CTOR values, and our signal value. Show how the linked list is built in the regular heap. Then show of our `heap_base` points in the heap ... Show how the heap jumps. 
```
heap:
l0 -> Ctor(l1, l2) C0
l1 -> ...
l2 -> ...
....
ln -> SigCons(ln1, ln2), prev, next=lm
```
- where C0 is the counter for location 0. Colour coded.
- Could even be used as a running example, present it in section 2, then add signal heap in section 3.
- Could even use it when we describe our modifications/adaptation of beans. When we annotate with reference counts, show how these are affected?
- For reuse and reset in section 2, show heap before and after reuse operation.


### Our combinator reuse limitation:

This is probably best shown with an example. Show a Rizzo program before and after compilation. Then annotate lines with reference count

### Guarded recursion:
- Restriction to guarded recursion is not that useful for a practical language.
- We could produce a warning whenever recursion is not guarded by a delay. Silence with an annotation
