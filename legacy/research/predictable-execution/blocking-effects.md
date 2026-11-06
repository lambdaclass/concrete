# Blocking Effects

**Status:** Open

This note defines how the predictable-execution direction should treat operations that may block.

## Why Blocking Matters

Allocation and recursion are not the only threats to predictable execution.

A function that waits on:

1. file I/O
2. network I/O
3. process state
4. sleep/timers
5. synchronization

can violate execution predictability even if it allocates nothing.

## First Profile Rule

In the first predictable-execution profile:

1. blocking operations are compile errors
2. hosted I/O is outside the profile by default
3. synchronization that can wait is outside the profile by default

The first profile should prefer a strong rule over a fuzzy one.

## Report Categories

The compiler should eventually report whether a function:

1. may block on file or console I/O
2. may block on network I/O
3. may block on process operations
4. may block on sleep/clock waiting
5. may block on synchronization

This should be tracked separately from authority. A function can have `Network` authority without necessarily blocking, and a predictable-execution profile cares about that distinction.

## Stdlib Consequences

This likely means the stdlib needs a clearer distinction between:

1. potentially blocking hosted APIs
2. non-blocking or bounded alternatives
3. freestanding-safe APIs

## Future Work

Later profiles may allow restricted blocking if:

1. the wait is bounded and explicit
2. the scheduler model is analyzable
3. the runtime policy is explicit enough to report

The first profile should not assume that sophistication exists yet.
