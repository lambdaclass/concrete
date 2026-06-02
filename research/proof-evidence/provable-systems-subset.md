# Provable Systems Subset

**Status:** Open

This note defines the useful middle ground between:

1. a tiny pure proof toy subset
2. the unrealistic goal of proving arbitrary full systems code

## Why This Matters

The important question is not only "what is provable?" It is:

1. what subset remains useful for real no-GC systems programming
2. what kinds of examples can live inside that subset
3. how that subset differs from both the general language and the predictable-execution profile

## Candidate Shape

The first provable systems subset may include:

1. pure and allocation-free parsing/validation cores
2. bounded computation over bytes, slices, fixed buffers, and simple structs
3. capability-free or tightly capability-restricted functions
4. explicit failure via `Result`
5. small report-linked claims over selected compiler analyses

It may exclude, at first:

1. unrestricted FFI
2. broad trusted regions
3. dynamic allocation
4. blocking operations
5. unconstrained concurrency

## What This Note Should Deliver

1. a concrete subset boundary
2. examples that live inside it
3. a clear relation to the broader language
4. a clear relation to predictable execution and high-integrity profiles
5. a clear list of property classes that are worth proving first
