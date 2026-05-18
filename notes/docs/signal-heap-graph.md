# Signal dependency graph and scheduling notes

Created on the 26/03-2026. Revised after checking the current C runtime.

This note revisits whether the current signal heap could be changed from a
globally scanned linked list into a dependency graph. The simpler conclusion is
that the useful graph is not a graph of clocks alone. It is a graph of active
runtime dependencies:

- channel nodes are external input sources
- signal nodes are the stable mutable signal cells already used by the runtime
- edges point from the thing that can update to the signal that may update next

In other words, the intended shape is close to a DAG rooted at channels. When a
channel steps, only the subgraph reachable from that channel needs to be
considered for that time step.

## What the current runtime does

The current C runtime stores all live signals in one doubly-linked list.
The list does two jobs at once:

- it keeps live signals reachable for update and deallocation
- it gives a global update order for same-step dependencies

When `rz_step(chan, v)` runs, it calls `rz_heap_update(chan, v)`. That function
scans every live signal. For each signal it:

1. checks whether the signal tail has ticked
2. if not, clears the signal's `updated` flag
3. if so, advances the tail, obtains an intermediate signal, copies the
   intermediate signal's head and tail back into the original signal cell, and
   marks the original cell as updated

The important implementation detail is that `tail s` and `watch s` do not ask
the channel directly. They check whether another signal has already been marked
as updated in the current step. For `watch s`, the new head must also be a `Some`.

So the linked list is not merely storage. It is also the current scheduling
discipline.

## Corrected model

The best mental model is:

```text
channel C  -> signal A -> signal B -> signal D
channel K  -> signal X -> signal Y
```

If channel `C` fires, the runtime should only need to consider `A`, `B`, and
`D`, in dependency order. Signals `X` and `Y` can be left untouched because they
cannot observe anything that happened in this step.

The graph edges should represent the currently active dependencies inside each
signal's current tail:

| Later form | Active dependency |
| --- | --- |
| `never` | none |
| `wait c` | channel `c` |
| `tail s` | signal `s` |
| `watch s` | signal `s`, guarded by `s.updated && s.head is Some` |
| `sync l1 l2` | union of dependencies from `l1` and `l2` |
| `f |> l` / `laterapp f l` | same active dependencies as `l` |

This is enough to support the desired optimization: a step on a channel only
starts from signals subscribed to that channel, then propagates through signal
dependencies.

## What the older note got right

The older reasoning was right on the main constraints:

- a graph over clocks alone is too coarse
- dependency information is present in `Later` values
- signal identity must remain stable across updates
- dependencies can change after a signal updates
- new signals created during a step must not be processed in that same step

Those are real constraints in the runtime.

## What should be simplified

The graph does not need to start as a full general-purpose graph runtime with
components, parallel scheduling, and complex clock summaries.

The first useful version can be much smaller:

1. keep the linked list or another all-signals structure for lifetime/debugging
2. add subscriber lists beside the current heap:
   - `channel -> signals waiting on that channel`
   - `signal -> signals depending on tail/watch of that signal`
3. when a channel steps, seed a worklist from the channel subscriber list
4. process reachable signals in topological order
5. after a signal updates, re-extract dependencies from its new tail and update
   the subscriber lists

This keeps the proposed change close to the current runtime. It changes the
scheduling structure first, not the signal object model.

## Why a pure clock graph is not enough

A clock graph can say that a signal may eventually depend on a channel, but the
runtime needs to know the active same-step dependency.

For example:

- `wait c` directly depends on channel `c`
- `tail s` depends on whether `s` updated in this step
- `watch s` depends on whether `s` updated and whether the new head is `Some`
- `sync l1 l2` may update from either side, and `advance` must still know which
  side ticked

So clock summaries can be useful as an optimization or static approximation,
but they should not replace the active dependency graph.

## Stable signal cells matter

Signals in Rizzo behave like mutable runtime cells. Other values can hold
references to a signal, and those references must keep pointing to the same
signal after a time step.

That is why the current runtime copies the head and tail from an intermediate
signal back into the original signal. A graph scheduler must preserve that:

- graph nodes should be stable signal cells
- intermediate signals created by `advance` should not become permanent graph
  nodes unless they are genuinely retained by the program
- after copy-back, the original signal's subscriptions must be updated from its
  new tail

## Dynamic rewiring

The graph is not static. A signal's active dependencies are determined by its
current tail, and the tail can change whenever the signal updates.

After updating a signal, the scheduler must:

1. remove the signal from its old subscriber lists
2. inspect the new tail stored in the same stable signal cell
3. register the new active dependencies

This is the real cost of the optimization. The key empirical question is
whether maintaining these indices is cheaper than scanning the entire signal
heap on every step.

## Same-step activation

The linked list currently enforces a phase boundary with the heap cursor.
Signals created during an update are inserted into the already-processed part
of the list, so they are not visited again in the same traversal.

A graph scheduler needs an explicit equivalent. The simplest design is an
epoch field:

```text
signal.active_from_epoch
```

A signal created during epoch `n` can be registered in the dependency indices
immediately, but it must not be eligible for processing until epoch `n + 1`.

## Topological order

The current list works because a signal only depends on signals that are already
in the now heap. In graph terms, same-step dependencies should therefore be
acyclic and ordered from older prerequisites to newer dependents.

The graph scheduler can preserve this in one of two ways:

- maintain creation/list order as a topological order and process reachable
  signals according to that order
- compute a topological order for the reachable subgraph on each step

The first option is likely the better initial design because it is closest to
the current implementation. The dependency indices would decide which signals
are relevant; the existing order would still decide how to process them.

## Recommended first implementation

The least risky implementation plan is:

1. keep stable `rz_signal_t` cells and the current list for lifetime management,
   output registration, and debugging
2. add dependency extraction for `Later` values
3. store active subscriptions:
   - `wait c`: subscribe the owner signal to channel `c`
   - `tail s`: subscribe the owner signal as a dependent of `s`
   - `watch s`: subscribe the owner signal as a guarded dependent of `s`
   - `sync`: recurse into both sides
   - `laterapp`: recurse into the later argument
4. on `rz_step(chan, v)`, collect the reachable set from `chan`
5. process only that reachable set, in the existing heap order
6. after each updated signal, rewire its subscriptions from the new tail
7. keep a full-heap scan mode as a debug oracle until the graph scheduler is
   validated

This would make the desired semantics explicit: stepping a channel only affects
signals reachable from that channel for that time step.

## Expected benefits

- avoids full-heap scans when a program has independent signal subgraphs
- makes the dependency ordering visible instead of implicit in one list
- keeps the stable signal-cell semantics unchanged
- creates a cleaner path to later parallelization of independent components

## Expected costs

- extra memory for subscriber lists and per-signal subscription metadata
- extra work when a signal's tail changes and dependencies must be rewired
- more complicated deallocation, because a signal must be removed from
  subscriber lists as well as from the signal heap
- more invariants to test, especially around `watch`, `sync`, and signals
  created during an update

## Open questions

- How often do representative Rizzo programs contain independent channel-rooted
  subgraphs?
- How often do signal tails rewire to substantially different dependencies?
- Is processing the reachable set in existing heap order fast enough, or is a
  dedicated topological scheduler needed?
- Can dependency extraction be made cheap enough to beat repeated full-heap
  scans?

## Mermaid sketch

```mermaid
flowchart LR
    C1["channel console"] --> A["signal A"]
    A -->|tail| B["signal B"]
    B -->|watch guard| D["signal D"]

    C2["channel timer"] --> X["signal X"]
    X -->|tail| Y["signal Y"]
```

If `console` steps, only `A`, `B`, and possibly `D` are considered. The timer
subgraph is untouched for that step.
