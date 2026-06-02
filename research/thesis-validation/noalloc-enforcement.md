# NoAlloc Enforcement

**Status:** Open

This note defines the first thesis-validation target: making `NoAlloc` a compiler-enforced property rather than a convention.

## Why Start Here

`NoAlloc` is the clearest first operational restriction because:

1. allocation is already explicit in Concrete's capability model
2. allocation sites are comparatively easy to classify
3. rejecting allocation is easier than proving a full bounded-allocation budget
4. the result is immediately useful for predictable-execution and high-integrity profiles

## What `NoAlloc` Should Mean

A function in a `NoAlloc` context must not:

1. directly allocate
2. call any function that may allocate
3. use stdlib surfaces whose allocation behavior is not excluded

At first this should be enforced conservatively. False negatives are better than optimistic acceptance.

## Likely Enforcement Shape

The simplest path is:

1. compute allocation reachability over the call graph
2. classify functions as `no_alloc`, `allocates`, or later `bounded_alloc`
3. reject functions marked or profiled as `NoAlloc` if any transitive path reaches allocation

## Relationship To Bounded Allocation

`NoAlloc` is the first stage, not the whole story.

Later work may allow:

1. bounded allocation with structural reasoning
2. fixed-capacity or arena-based escape hatches
3. profile-aware stdlib subsets

But the first win should be binary and strict.

## What The Demo Should Show

Concrete should be able to demonstrate:

1. one function that remains valid under `NoAlloc`
2. one function rejected because allocation is reached transitively
3. one report that explains why the rejection happened
