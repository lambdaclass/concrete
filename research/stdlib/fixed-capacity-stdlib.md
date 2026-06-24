# Fixed-Capacity Stdlib

**Status:** Open

This note defines the likely stdlib support needed for no-allocation and predictable-execution profiles.

## Why This Matters

Saying "no allocation" is not enough. Real programs still need data structures and buffers.

If Concrete wants a usable predictable-execution profile, it likely needs fixed-capacity alternatives to some heap-backed stdlib types.

## Likely Needed Building Blocks

The first candidates are:

1. fixed-capacity byte buffer
2. fixed-capacity string/builder
3. ring buffer / queue
4. fixed-capacity vector
5. bounded channel if analyzable concurrency is ever allowed

## What This Replaces

In the restricted profile, ordinary heap-backed types such as:

1. `String`
2. `Vec`
3. `Bytes`
4. map/set types with dynamic growth

may need to be excluded unless they are used under a later bounded-allocation subprofile.

## Design Constraints

These types should:

1. make capacity explicit
2. make overflow behavior explicit
3. avoid hidden allocation
4. stay compatible with the language's ownership model
5. be usable in freestanding or runtime-minimal settings

## Open Question

Concrete should decide whether these are:

1. separate stdlib types for the restricted profile
2. profile-aware variants of existing types
3. later additions only after the restricted profile proves valuable

The first option is probably cleaner.
