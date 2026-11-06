# FFI Boundaries

**Status:** Open

This note defines how foreign calls should be treated in the predictable-execution direction.

## Why FFI Is Hard

Foreign code is an analysis boundary.

Even if the source language is explicit, foreign calls can hide:

1. allocation
2. blocking behavior
3. recursion
4. timing variability
5. hidden synchronization

That makes FFI one of the hardest parts of any analyzable profile.

## First Profile Rule

In the first predictable-execution profile:

1. unrestricted FFI is a compile error
2. `extern fn` calls are outside the profile
3. trusted wrappers do not make foreign timing behavior analyzable by themselves

This keeps the first profile honest.

## Later Possibilities

Later, Concrete may allow selected foreign boundaries if they are:

1. explicitly classified
2. wrapped by audited adapters
3. treated as unknown timing unless backed by stronger evidence

But that is a later extension, not part of the first profile.

## Reporting

The compiler should eventually be able to report:

1. whether a function crosses an FFI boundary
2. which foreign symbols are involved
3. whether those calls are allowed or disallowed in the selected profile
