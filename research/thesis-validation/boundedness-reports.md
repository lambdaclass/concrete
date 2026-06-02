# Boundedness Reports

**Status:** Open

This note defines the first report slice for thesis validation: surfacing execution-relevant uncertainty in a form that is mechanically reviewable.

## Why This Matters

Predictable execution starts with visibility before it reaches enforcement.

Concrete should be able to report, for each function or selected subset:

1. recursion / call-cycle presence
2. loop-bound status
3. blocking operations
4. FFI boundaries
5. trusted boundaries
6. allocation class
7. concurrency usage

## First Version

The first version should favor usefulness and honesty over perfection.

Good first classifications:

1. recursion present / absent
2. loop bound known / unknown
3. may block / does not block
4. crosses FFI / does not cross FFI
5. uses trusted / does not use trusted
6. no alloc / allocates / later bounded alloc

## Why This Is Valuable Even Before Full Profiles

This report slice would already let reviewers ask:

1. which functions are candidates for predictable-execution profiles?
2. which functions fall out of the profile and why?
3. where do trust and foreign boundaries enter the call graph?

That is a useful result even without full theorem proving or cycle-accurate timing analysis.

## What The Demo Should Show

Concrete should be able to produce a small report over a focused example set and show:

1. one function inside the restricted profile
2. one function outside it because of blocking
3. one function outside it because of FFI
4. one function outside it because of allocation
5. one function outside it because of unknown loop bounds or recursion
