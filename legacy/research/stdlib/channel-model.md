# Channel Model

**Status:** Open research direction
**Affects:** stdlib design, runtime design, structured concurrency, ownership transfer, capability model
**Date:** 2026-05-01

## Purpose

This note defines the first channel model for Concrete. Channels are the primary cross-task communication mechanism in the structured-concurrency direction; the design must compose with linear ownership transfer, capability scoping, and the eventual evidence-bearing audit story.

The near-term concurrency plan is OS threads with typed channels (see [concurrency.md](concurrency.md)). This note specifies what those channels should look like, and what they should *not* try to do yet.

## Core Position

Channels in Concrete should be:

1. capacity-typed
2. owning (send moves the value, receive moves it out)
3. lexically scoped where possible
4. capability-gated for cross-task use
5. closed by the sender side, with predictable behavior for pending receivers
6. small in surface area; broadcast and other variants are deferred

The single most important design decision is that **capacity is part of the type**. Unbounded channels are a separate, loud, opt-in form. Backpressure becomes a typed property rather than an emergent runtime behavior.

## Capacity-Typed Channels

```con
let ch: Channel<Msg, 16> = Channel::new();
```

The channel's capacity `16` is a const-generic parameter. Sending into a full channel either blocks (in the threaded backend) or yields (in any cooperative backend). Receiving from an empty channel similarly waits.

Why capacity in the type:

1. **Backpressure is the most common production failure mode.** A Go program with unbounded channels appears to work in development and dies of memory exhaustion at scale. A typed bound forces the question at design time.
2. **The type carries the bound.** A reviewer can see the bound in the signature without reading the implementation.
3. **Resource-bounded capabilities can require it.** A function declared `with(Heap(N))` needs to know its channel capacities to compute the bound.
4. **Evidence reports can use it.** "This subsystem buffers at most 16 messages per channel" is a typed claim that flows into the audit surface.

An unbounded channel is `Channel<T, Unbounded>`. The literal `Unbounded` is a marker, not a default. Any function using `Channel<T, Unbounded>` either holds an `Alloc` capability or fails to compile. The verbosity is the feature.

## Ownership Transfer

`send` consumes the value being sent. `recv` produces an owned value. The channel does not retain a reference to the sent value; once sent, the sender no longer owns it.

```con
fn send<T, N>(ch: &mut Channel<T, N>, value: T) -> Result<Unit, ChannelClosed>
fn recv<T, N>(ch: &mut Channel<T, N>) -> Result<T, ChannelClosed>
```

The send operation takes the channel by mutable reference (because the channel state is being modified) and the value by ownership (because it is being moved into the channel). The receive operation is symmetric: mutable reference to the channel, owned value out.

Linear values transfer cleanly: the sender's ownership is consumed at `send`; the receiver acquires ownership at `recv`. There is no shared ownership of the in-flight value.

Borrowed references cannot be sent. Their lifetime is tied to the source scope, and that scope cannot be guaranteed to outlive the receive. This is the Concrete equivalent of Rust's "no borrowed references across task boundaries" rule, expressed as ownership instead of as an auto-trait.

## Multi-Producer And Multi-Consumer

The simplest model is single-producer, single-consumer (SPSC). The first stdlib channel should be SPSC.

Multi-producer (MPSC) and multi-consumer (MPMC) variants require a sharing story for the channel handle itself. Two options:

1. **Sender/receiver split with cloneable senders.** Create the channel, get back a `Sender<T, N>` and `Receiver<T, N>`. The sender side can be cloned (one for each producer). This matches Rust's `mpsc` and is familiar.
2. **Channel handle through a `Shared<Channel<T, N>>`** that requires `with(Sync)`. Heavier but uniform with the rest of the shared-state story.

The first stdlib should ship SPSC and MPSC. MPMC can wait. Broadcast and pub-sub belong to a later note; they are not channels in the same sense.

## Close Behavior

A channel can be closed. After close:

1. Subsequent sends return `Err(ChannelClosed)`.
2. Pending recvs receive whatever is buffered, then receive `Err(ChannelClosed)`.
3. Closing twice is a no-op (idempotent close).

Who can close:

- The sender side can close. This is the common case ("no more messages from me").
- The receiver side closes implicitly when dropped, freeing the channel.
- For MPSC, the channel is closed when the last sender is dropped or any sender explicitly closes.

Close is a mode change, not a destructive operation. The channel must still exist to allow pending receivers to drain it. The receiver side is responsible for the eventual deallocation.

This composes with linear handles: the sender handle and receiver handle are linear values. The compiler enforces that they are consumed (via close, drop, or transfer) before scope exit.

## Send and Receive Inside Scopes

Channels are most useful inside structured scopes. The typical pattern:

```con
fn pipeline(input: Input) with(Async) -> Result<Output, Error> {
    scope s with(Async) {
        let (tx, rx): (Sender<Item, 16>, Receiver<Item, 16>) = channel();

        s.spawn(producer, tx, input);
        s.spawn(consumer, rx)
    }
}
```

The channel ends are handed to the spawned tasks. Each task owns its end linearly. When a task ends, its channel-end is dropped, which propagates close as appropriate.

The scope guarantees both tasks terminate before the function returns. The channel itself does not need to survive the scope.

## Select And Multiplexing

A `select` operation waits on multiple channels simultaneously. With linear receiver handles, the basic shape is:

```con
match select(rx_a, rx_b) {
    A(value, rx_a, rx_b) => { ... }
    B(value, rx_a, rx_b) => { ... }
}
```

Both receivers come back to the caller after the select completes. The first to be ready returns its value; the others are returned untouched. This preserves linearity.

A timeout variant can be expressed as selecting on a channel plus a timer:

```con
match select_with_timeout(rx, deadline 5s) {
    Got(value, rx) => { ... }
    Timeout(rx) => { ... }
}
```

For the first stdlib, support pairwise select. N-way select can be added later if it earns the elaboration complexity.

## Capability Requirements

Channels are concurrency primitives and must be capability-gated.

Initial rules:

1. Constructing an unbounded channel requires `with(Alloc)` (because capacity is dynamic).
2. Constructing a bounded channel requires no capability beyond what the value type already needs.
3. Sending and receiving across task boundaries requires the surrounding `with(Async)` or stronger.
4. Channels used purely within a single task (rare but possible) require no concurrency capability.

Sharing the same channel between tasks requires the channel handle to live in a scope that owns both tasks, or to be passed through `s.spawn` arguments. The capability check is local to the spawn site, not the channel itself.

## What This Replaces

Without typed bounded channels, the alternatives are:

- **Unbounded queues with hope.** Memory exhaustion under load.
- **Mutexes and condition variables.** Lower-level, easier to use wrong, harder to audit.
- **Shared mutable state.** Defeats the linear-ownership story.
- **External coordination libraries.** Fragments the ecosystem like Rust's `tokio::sync`, `crossbeam`, etc.

Typed channels with bounded capacity in the stdlib are the right primitive for cross-task communication in a linear-ownership setting. Everything fancier (broadcast, watch, pub-sub) can be built on top.

## What To Defer

Out of scope for the first channel model:

- Broadcast / multi-consumer-receive-all channels.
- Watch channels (latest-value semantics).
- Pub-sub.
- Channels with priority or fairness guarantees.
- Channels with custom backpressure policies (drop-oldest, drop-newest).
- Bounded channels with dynamic capacity.

Each of these has a real use case but adds API surface and design complexity. The first model should be small enough to mechanize and audit. Variants can be researched separately once the base model is shipped.

## Evidence Properties

Channels should contribute to the concurrency evidence story. Reportable facts:

| Property | Possible evidence level | Source |
|---|---|---|
| bounded buffer | compiler-enforced | typed capacity |
| no unbounded growth in subsystem | compiler-enforced or reported | absence of `Channel<_, Unbounded>` |
| close-on-sender-drop | compiler-enforced | linear sender handle |
| no message loss in transit | reported | implementation property of the channel runtime |
| fair scheduling | trusted assumption or backend assumption | runtime backend |

The first three are first-class compiler facts. The last two depend on the runtime and should be marked as such in audit output.

## What Not To Add

- No `unsafe` channel construction in the first model. Channels are runtime primitives with strong safety properties; the unsafe escape hatch can come later if a use case requires it.
- No global channel registry or named channels. Channels are local values. Cross-process or cross-machine communication is a different problem.
- No automatic conversion between bounded and unbounded channels. The capacity is part of the type for a reason.
- No "best-effort" send that silently drops on full. If drop-on-full is desired, it should be an explicit `try_send` or a wrapper combinator.
- No async-iterator interface as the primary surface. Channels are channels; iterators are iterators. The two can be bridged via combinators but should not be conflated in the type.

## Implementation Order

1. SPSC bounded channels with linear handles.
2. MPSC via cloneable sender, single receiver.
3. Pairwise select with linear handles.
4. Close semantics, sender-drop-closes-channel rule.
5. Unbounded channel as a separate type requiring `with(Alloc)`.
6. Evidence integration for channel facts in `--report` output.
7. Later: MPMC, broadcast, watch, etc. as separate research notes.

## One-Line Test

A channel model is good if a reviewer can read a function's signature and answer: how much memory can this subsystem buffer, who closes the channel, and what happens to pending receivers when it does — without reading the implementation.

## Relationship To Other Notes

- [concurrency.md](concurrency.md) — near-term threads-first plan; channels are part of the first surface
- [async-concurrency-evidence.md](async-concurrency-evidence.md) — long-term direction; channels must compose with structured scopes
- [long-term-concurrency.md](long-term-concurrency.md) — broader concurrency direction
- [allocation-budgets.md](allocation-budgets.md) — bounded-resource capabilities; channel capacity contributes to subsystem heap bounds
- [../predictable-execution/analyzable-concurrency.md](../predictable-execution/analyzable-concurrency.md) — predictable profile may require fixed-capacity channels only
- [../language/capability-polymorphism.md](../language/capability-polymorphism.md) — channel combinators need this to be usable
