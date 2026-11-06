# Bounded Loops

**Status:** Open

This note defines how the predictable-execution profile should treat loops.

## Why Loops Matter

After banning recursion, loops become the main source of unbounded execution.

If Concrete wants a restricted analyzable profile, it needs a clear answer to:

1. which loops count as statically bounded
2. which loops are only reportable, not enforceable
3. which loops are compile errors in the profile

## First Profile Rule

In the first predictable-execution profile:

1. loops with no structurally known upper bound are compile errors
2. loops that the compiler can prove are bounded are allowed
3. there is no "trust me" loop annotation in the first version

That keeps the profile honest and mechanically understandable.

## Likely Allowed Loop Shapes

The first version should probably accept only obvious cases:

1. loops over a statically known range
2. loops over a parameter or local value where the upper bound is explicit in the loop structure
3. loops over fixed-capacity buffers where the capacity is part of the type or immediate construction

Examples:

```con
let i: u64 = 0;
while i < 16 {
    i = i + 1;
}
```

```con
let i: u64 = 0;
while i < buf.len() {
    i = i + 1;
}
```

The second example is only acceptable if `buf.len()` is itself structurally bounded in the selected profile.

## Disallowed First-Profile Cases

These should be compile errors in the first profile:

1. open-ended `while true`
2. loops whose exit depends on I/O, FFI, or external state
3. loops whose bound depends on unclassified dynamic growth
4. loops that allocate unless a later bounded-allocation subprofile explicitly allows them

## What The Compiler Should Report

Even outside the restricted profile, the compiler should classify loops as:

1. statically bounded
2. bounded by parameter
3. bounded by fixed-capacity object
4. unknown bound
5. externally controlled bound

That report is useful even before enforcement is complete.

## Future Work

Later, Concrete may grow:

1. accepted loop-bound annotations
2. proof-backed loop bounds
3. richer structural inference

But the first profile should not depend on those.
