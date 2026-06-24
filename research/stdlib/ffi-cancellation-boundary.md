# FFI And Cancellation Boundary

**Status:** Open research direction
**Affects:** structured concurrency, cancellation semantics, FFI, trusted boundaries, evidence reports
**Date:** 2026-05-01

## Purpose

This note specifies how cancellation interacts with foreign function calls and trusted regions. The structured-concurrency direction in [async-concurrency-evidence.md](async-concurrency-evidence.md) commits to cooperative cancellation. The predictable-execution work in [../predictable-execution/ffi-boundaries.md](../predictable-execution/ffi-boundaries.md) and [../language/trusted-boundary.md](../language/trusted-boundary.md) treats FFI as an analysis boundary. The two meet in the question: what happens when a task is mid-call into untrusted C while its scope is being canceled.

The honest answer affects evidence claims about scope cleanup, cancellation termination, and bounded shutdown time. It also affects the runtime backends' implementations.

## Core Position

Cancellation is cooperative. Cooperative cancellation has no effect inside trusted regions until the trusted region returns. The implication: long-running FFI calls block scope cancellation. Concrete must make this honest in the type system rather than hide it in the runtime.

Three principles:

1. **Cancellation cannot interrupt trusted code.** A scope's cancellation request waits at the FFI boundary until the foreign function returns.
2. **Trusted regions must declare bounded execution time, or be marked unbounded.** The compiler propagates the bound (or its absence) to scope-level cancellation latency claims.
3. **FFI calls that may run unbounded require explicit handling.** Either the surrounding code provides an out-of-band interrupt mechanism (e.g., a kill signal, a closeable file descriptor), or the scope inherits an unbounded cancellation latency claim.

These are language-level rules, not runtime behaviors. They affect what evidence reports can claim.

## What Cancellation Means In Concrete

Cancellation takes effect at checkpoints. A checkpoint is a point in the program where the runtime checks for a pending cancellation request. Every `await`, `recv`, `send` (when blocking), and explicit `check_cancel()` call is a checkpoint.

Pure CPU code is not a checkpoint. Long bounded loops in pure code do not respond to cancellation until they reach a natural checkpoint or end.

This is the cooperative model. The alternative (preemptive cancellation, e.g., signal-based or thread-abort) breaks linear cleanup. A task interrupted mid-acquisition of a linear resource cannot be unwound safely without a destructor system Concrete deliberately does not have.

## FFI As An Atomic Region

A call into foreign code is, from Concrete's perspective, atomic with respect to cancellation. The reasoning:

1. The foreign code does not check the cancellation flag.
2. Concrete cannot interrupt it without the foreign code's cooperation.
3. Resource state on the foreign side is opaque; even if Concrete could interrupt, recovery is not safe.

So the rule: cancellation requests issued during a foreign call are deferred until the foreign call returns. At that point, the calling Concrete code reaches its next checkpoint and observes the cancellation.

This is operationally identical to "wait for the foreign call to finish, then handle cancellation." The point is that the language model says so explicitly.

## FFI Declarations Must State Cancellation Behavior

To support evidence claims about scope cleanup time, FFI declarations must carry a cancellation-relevant attribute.

Three categories:

1. **Bounded.** The foreign function returns within a known time bound. The bound is part of the declaration. Example: a fixed-size memcpy.
2. **Bounded-on-input.** The foreign function returns within time bounded by some property of its input. The user asserts this bound; the compiler treats it as a trusted assumption.
3. **Unbounded.** The foreign function may run arbitrarily long. Cancellation cannot pre-empt it. Example: a blocking syscall without a timeout, a network call with no deadline.

A reasonable FFI surface:

```con
extern fn memcpy(dst: *mut u8, src: *const u8, n: Size)
    bounded
    -> Unit

extern fn read_blocking(fd: Fd, buf: *mut u8, len: Size)
    unbounded
    -> SsizeT

extern fn parse_input(buf: *const u8, len: Size)
    bounded_by(len)
    -> ParsedHandle
```

The bound is part of the FFI signature. Code that calls an unbounded FFI function inherits an unbounded cancellation latency unless the surrounding code provides an external interrupt mechanism.

## Out-Of-Band Interrupt Mechanisms

For some unbounded FFI calls, the surrounding code can provide an interrupt:

1. A blocking `read` on a file descriptor can be interrupted by closing the descriptor (or by `pthread_kill` with an EINTR-handling signal).
2. A blocking network operation can be interrupted by a timeout set on the socket.
3. A long-running computation in a foreign library can sometimes be interrupted by a library-specific cancellation API.

The pattern: the cancellation request triggers an out-of-band mechanism that causes the foreign call to return early (often with an error). The Concrete code then reaches a checkpoint and observes cancellation normally.

Concrete should not pretend this is automatic. Each unbounded FFI call that wants to be cancellable must wire an interrupt. The wiring is user code, not runtime magic.

A wrapper pattern:

```con
fn cancellable_read(io: Io, fd: Fd, buf: &mut [u8])
    with(File, Cancellable)
    -> Result<Size, IoError>
{
    register_cancel_handler(|| close_fd_safely(fd));
    let result = unsafe { read_blocking(fd, buf.as_mut_ptr(), buf.len()) };
    deregister_cancel_handler();
    interpret_result(result)
}
```

The `register_cancel_handler` mechanism is part of the runtime API. When cancellation is requested for a task in this state, the handler runs (closing the fd), the foreign call returns with an error, and the task observes cancellation at its next checkpoint.

This is honest: the user opted in to making the call cancellable. The compiler can verify the registration/deregistration pairing.

## Cancellation Latency

A scope's cancellation latency is the time between a cancellation request and the scope reaching `Closed` state.

For a scope:

```
latency = max over children of (per-child cancellation latency)
per-child latency = (FFI bound or unbounded) + (post-checkpoint cleanup time)
```

If any child is in an unbounded FFI call without an interrupt mechanism, the scope's latency is unbounded. The compiler can compute and report this.

Evidence levels:

| Property | Level | Source |
|---|---|---|
| scope cancellation terminates | enforced | absence of unbounded-without-interrupt FFI in any child |
| scope cancellation bounded by N | reported | bound = max of children's bounds; honest only if all bounds are honest |
| each child cancels at first checkpoint | enforced | structural property of cooperative cancellation |
| FFI calls bounded | trusted assumption | from FFI declarations |

A function that wants to claim "this scope cleans up within 100ms on cancellation" must either avoid unbounded FFI or wrap it with verified interrupts.

## Trusted Regions In Concrete Code

The same logic applies to user-marked trusted regions in pure Concrete:

```con
trusted {
    // Performs pointer arithmetic and a long loop.
    long_running_pure_computation()
}
```

A trusted region without checkpoints is, from cancellation's perspective, the same as an unbounded FFI call. The user asserts it terminates; the compiler accepts the assertion as a trusted boundary; cancellation has no effect inside it.

Trusted regions should declare bounds where possible:

```con
trusted bounded(n) {
    // Operates on n elements; bounded by n iterations.
}
```

This is a separate concern from memory safety (which is the usual reason for `trusted`); the bound attribute is for cancellation-latency reasoning.

## What This Replaces

Languages that ignore the FFI/cancellation boundary either:

1. Lie about cancellation guarantees and surprise users with hangs.
2. Use preemptive cancellation (signals, thread-abort) and produce unsafe state on the foreign side.
3. Disallow FFI from cancellable contexts entirely, restricting the language.

Concrete's position is to make the boundary visible and require explicit handling for cancellable FFI. The audit surface gains a property (cancellation latency); users gain control over which FFI calls are interruptible.

## What Not To Add

- No automatic conversion of unbounded FFI to bounded by adding a wrapper. The wrapper must be written by the user, not synthesized by the compiler.
- No preemptive cancellation primitive. The cooperative model is foundational to linear cleanup.
- No "best-effort" cancellation that may or may not interrupt FFI. Either it does (with explicit interrupt mechanism) or it does not.
- No global registry of cancellation handlers. Handlers are scoped to the task that registered them.
- No silent treatment of trusted regions as cancellation-transparent. Trusted code blocks cancellation just like FFI.

## Operational Semantics Sketch

The formal model in [../proof-evidence/concurrency-formal-model.md](../proof-evidence/concurrency-formal-model.md) treats the cancellation checkpoint as a primitive. This note refines what a checkpoint is.

A task's reduction sequence has implicit checkpoints at:

- Every `await` reduction.
- Every blocking channel send/recv.
- Every explicit `check_cancel()` call.
- Every loop iteration in a function declared `with(Cancellable)` (compiler-inserted).

A task's reduction sequence does not have checkpoints inside:

- A trusted region.
- An FFI call.
- A pure CPU sequence between two natural checkpoints.

The cancellation rule:

```
If a scope is in Cancelling state and a child task is at a checkpoint,
the child reduces to err(Cancelled) within bounded steps.

If the child is inside a trusted region or FFI call,
no reduction occurs until the region or FFI returns.
```

This is honest about the cooperative discipline.

## Implementation Order

1. Add cancellation attributes to the FFI declaration syntax.
2. Implement the cancellation handler register/deregister API.
3. Compute cancellation-latency reports from FFI bounds and trusted region bounds.
4. Add `with(Cancellable)` capability and its checkpoint requirements.
5. Wire the threaded backend to deliver cancellation at checkpoints.
6. Wire the evented backend's checkpoint discipline.
7. Wire the Sim backend to inject cancellation deterministically at checkpoints.

## One-Line Test

The FFI cancellation boundary is honest if a reviewer can ask "what is the maximum time between cancellation request and scope cleanup" and the compiler can answer with a bound, an unbounded marker, or a list of specific FFI calls that block the bound.

## Relationship To Other Notes

- [async-concurrency-evidence.md](async-concurrency-evidence.md) — umbrella; cancellation is part of the direction
- [../language/trusted-boundary.md](../language/trusted-boundary.md) — trusted regions in pure Concrete
- [../language/trusted-code-policy.md](../language/trusted-code-policy.md) — broader trust policy
- [../predictable-execution/ffi-boundaries.md](../predictable-execution/ffi-boundaries.md) — FFI in the predictable-execution profile
- [../proof-evidence/concurrency-formal-model.md](../proof-evidence/concurrency-formal-model.md) — formal model; this note refines checkpoint semantics
- [../proof-evidence/concurrency-evidence-example.md](../proof-evidence/concurrency-evidence-example.md) — worked example; cancellation latency could appear in reports
- [channel-model.md](channel-model.md) — channels are checkpoint sources
- [../predictable-execution/concurrent-stack-analysis.md](../predictable-execution/concurrent-stack-analysis.md) — sibling analysis across the FFI boundary
