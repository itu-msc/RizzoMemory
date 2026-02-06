# Supervisor meeting

## Actions - going forward

- Understand what [Perceus](https://dl.acm.org/doi/10.1145/3453483.3454032) does with immutable references. How do we deal with mutable references - probably requires some extensions.
- Try to include it in our small OCaml immutable beans.

## Agreements

- We share code earlier with Patrick - makes it easier to help us.
- Meetings: Thursdays at 12:30 going forward.

## General

- This semester, we could try to use Patrick more. Share code earlier, share what we have done/what think we have done.
- Translating Rizzo [beans calculus](https://dl.acm.org/doi/10.1145/3412932.3412935), seems fine.
	- **Caveat:** Signals of Rizzo are essentially mutable references. So we need to add the ability to read/update mutable references to "counting immutable beans" calculus.
	- We should look into the mutable references of Perceus.
	- Pros: Also seems fine for the goal of transpiling, compiling.