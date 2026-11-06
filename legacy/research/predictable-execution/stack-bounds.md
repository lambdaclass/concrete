# Stack Bounds

**Status:** Open

This note explores what Concrete can realistically say about stack use.

## Why Stack Bounds Matter

Predictable execution is not only about time. It also depends on whether stack growth is bounded and explainable.

Even without recursion, stack use can grow because of:

1. large locals
2. aggregate temporaries
3. calling depth
4. lowering decisions
5. backend code generation choices

## What Is Likely Feasible

Concrete should start with structural reporting, not exact byte promises.

Likely first reports:

1. recursion present / absent
2. static call depth where known
3. functions with large stack-allocated locals
4. stack-sensitive lowering patterns

This would already improve auditability.

## What Is Hard

Exact stack bounds become difficult when:

1. calling conventions vary by target
2. backend optimizations change frame layout
3. large values are copied or spilled differently
4. inline decisions affect frame composition

So Concrete should separate:

1. source-level structural stack facts
2. target/backend-specific stack measurements

## First Profile Rule

The first predictable-execution profile should likely rely on:

1. no recursion
2. restricted large-local patterns where structurally visible
3. reporting first, enforcement later

That is more credible than claiming exact stack bounds too early.
