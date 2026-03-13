# 2026-03-12 - supervisor

## Paper structure

- Explain what adaptation we need/did
- Details => 
	- The idea behind => The most valuable for us is to show how WE DO it. Rather than explaining essentially the same thing twice.
- Motivating Rizzo =>
	- if we do FRP we run into this issues.
	- Rizzo handles these by introducing these idea. Very particular semantics.
	- This introduces some issues that we then have to solve. the challenges that are introduced when you then have to implement Rizzo.
- The limitations of reference counting => we cannot do circular references in Rizzo
	- We do have general recursion, how could we create cycles?
	- The beans algorithm handles below:
		- you typically do not use it in FP because naive is bad, you end up inc/dec immediately

## next step
- Next sections
- Send Patrick a very early draft