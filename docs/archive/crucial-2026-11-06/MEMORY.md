**DECIDED (owner, 2026-11-06: option (a) on every question, [decision](../../decisions.md#decided-project-memory-2026-11-06)); archived. Reviewed by Codex, agy and Kimi.**

Non-normative historical record: the spec is [ConcreteSpec/](../../../ConcreteSpec/).

# Memory bounds (proposal v2, 2026-11-06)

## Recommendation

Before running a function, you should be able to ask how much heap it can use, per allocator, and
get a proved answer or an honest "not established". Branded allocator values, fallible
allocation and linear ownership make this tractable. Every allocation is either released at a
visible point, transferred, or returned in an error. There is no garbage collector or reference
counting to model.

1. **Quantities, kept apart.**
   - **Logical bytes** are what the program requests: `size_of(T)` from the layout authority,
     or a container's stated request.
   - **Footprint** is what an allocator consumes for those requests: alignment, headers, and
     bump space that a release does not reclaim. Each allocator's contract defines it.
   - Facts are reported per **allocator lineage**: an allocator value together with every fresh
     brand it receives on reset or per loop iteration (Q1.2). Brand-by-brand accounting would lose
     continuity across a reset.
2. **Facts per function and per lineage `A`**, each with a status (established / conditional /
   not established / not applicable):
   - `heap_requested(A)`: total logical bytes requested, and the allocation count;
   - `heap_net(A)`: the change in live logical bytes from entry to return. Allocations
     transferred out (results, `&mut` stores) count as positive. Storage consumed from
     parameters and released counts as negative;
   - `heap_peak(A)`: the maximum, during the call, of live logical bytes above the entry level;
   - `footprint_requested(A)` and `footprint_peak(A)`: the cumulative and the peak quantities
     measured in the allocator's footprint, when its contract gives a footprint function.

   Every established fact is a sound *bound* on the execution quantity, not its exact value:
   upper bounds for the peaks and totals, and a signed interval `[lo, hi]` for `heap_net`.

   Stack is a separate fact (decided: from target artifacts only). Stack bytes come only from
   the final artifact's frames plus the launcher's reservation. The BIR frame-accounting handoff
   says which frames and calls exist; BIR slot counts are never used as byte figures. Reports
   never fold these into one number.
3. **Composition rules.**
   - Requested bytes and allocation counts add.
   - Peak and net compose max-plus, as conservative bound rules. They are deliberately not
     exact, because branch maxima lose correlation:
     - sequencing: `peak(S₁;S₂) ≤ max(peak S₁, hi(net S₁) + peak S₂)`, and the net intervals
       add;
     - branches: the maximum of the peaks, and the hull of the net intervals;
     - a loop of at most `k` iterations: zero when `k = 0`, and otherwise
       `peak ≤ peak(body) + (k−1)·max(0, hi(net body))`;
     - calls: the callee's summary, with actual arguments substituted conservatively.
   - Failure paths and cleanup are paths like any other, and error branches keep their
     obligations (authority rule 10).
   - Upper bounds are polynomials with non-negative coefficients and `max`, over parameter counts
     and lengths that a type exposes. A net interval's lower end is the negation of such a polynomial.
   - Where a bound needs something the rules cannot see, the fact is "not established" with a
     reason, never a guess.
4. **Counts come from evidence, not termination alone.**
   - A counted loop's count comes from the octagon checker (OD47 snapshot).
   - A `while` loop needs a variant whose initial value is bounded by a polynomial in the
     parameters. A variant proves termination, but only its initial bound gives a count.
   - A loop under a termination permission gives "not established".
   - Recursion needs a bound on the number of calls for `heap_requested`.
   - A depth bound alone never establishes `heap_peak`. Branching calls that retain their
     results can make the peak exponential in the depth. The peak needs a solved recurrence over
     the call tree, with per-call `(peak, net)` summaries, or else it is "not established".
5. **Infallibility comes only from an allocator theorem.**
   - "Every allocation from `A` in this call succeeds" is derived only when `A`'s contract
     proves it. This uses the Q1.11 availability law and its success witness, stated over
     footprint, alignment and fragmentation.
   - Example: a bump arena with `capacity − used ≥ footprint_requested(A)`.
   - A general-purpose reclaiming allocator gets the fact only if its contract excludes
     fragmentation failure.
   - The system allocator never gets it, because its failures are environmental.
   - The `Result` types and input conservation stay unchanged; the fact only marks failure arms
     as unreachable.
6. **Policy, fail-closed.**
   - Policy entries in `Concrete.toml` (the owner's decision; the entry spellings are still open)
     can require at named boundaries:
     - `heap_peak(A) ≤ N`;
     - `footprint_peak(A) ≤ N`;
     - zero allocation;
     - infallibility.
   - "Not established" fails the policy. A dependency that raises a bound fails it too.
7. **Semantics and proof.**
   - The A2i semantics gains sized allocation, with lineage-level logical byte counters as
     *erased instrumentation*: removing the counters changes no run.
   - `heap_peak_sound`, `heap_net_sound` and `heap_requested_sound` are proved in Lean against
     that instrumentation (resource adequacy).
   - Allocator contracts give the footprint theorems.

## Other bounds and the predictable policy

The old roadmap deferred step, time and allocation-count bounds (R-0405, R-0415, R-0445).
"Bounded" promises no deadline. The predictable policy should require:

- **At first:** termination and the whole-program stack bound for the final artifact (both
  decided), plus `heap_peak = 0`.
- **From the allocation slice:** heap peak bounds and infallibility. The allocation count already
  comes with `heap_requested`.
- **Later, in a separate proposal:** step bounds over Core steps, in the same bound language
  (phase E). Wall-clock bounds need a target timing model and stay out of every claim until a
  target contract exists.

## What changes

- **Decisions:** one row, "heap facts and memory policies". It extends no-hidden-traps Q11/Q12
  ("may allocate") and the allocation decision of 2026-11-02.
- **Spec (A2i):** sized allocation and the instrumentation in the brand semantics. Today
  `Brand.lean` models identities only, and its reset requires no live heap.
- **Roadmap:** an R-task with acceptance examples:
  - a counted loop of pushes into a reserved `Vec`, with an exact bound;
  - an arena parser proved infallible;
  - `while` without a bounded variant giving "not established";
  - branching recursion with only a depth bound giving "not established" for requested bytes;
  - a reset loop accounted by lineage.

  The std container specs gain growth bounds.

## When

- **Slice 0:** `heap_* = 0` facts, *proved* by the derivation (no allocator is reachable), not
  schema defaults.
- **A2i:** the instrumentation and its interfaces.
- **Allocation slice:** the analysis, the theorems and the policy entries. This is not deferred
  to phase E: the facts query and the predictable policy need it as soon as allocation exists.

## Cost for people writing code

Usually none, because the facts are derived. Authors write a bounded variant on a `while` loop
that allocates, and a guard where a size comes from input. That is the no-hidden-traps pattern.

## Prior art

- **SPARK:** ownership; GNATstack covers stack only.
- **MISRA and Ravenscar:** no allocation after initialization, as a profile rather than a proof.
- **RAML/AARA:** amortized polynomial bounds; the upgrade path if containers need amortization.
- **CakeML:** space cost semantics and proofs of no out-of-memory.
- **Zig:** allocator values, without bounds.

## Owner questions

1. **Bound language.**
   - (a) **Recommended:** polynomials with `max` over counts, lengths and depths.
   - (b) Constants only.
   - (c) AARA potentials now.
2. **Container growth.**
   - (a) **Recommended:** std states a growth *upper bound* per operation, exact where cheap (for
     example `reserve(n)` requests exactly `n·size_of(T)`).
   - (b) Leave growth unspecified, which makes push bounds "not established".
3. **Which quantity policies guarantee.**
   - (a) **Recommended:** both. Logical bytes are always available; footprint is available where
     the allocator's contract defines it.
   - (b) Logical bytes only.
4. **Where budgets are written.**
   - (a) **Recommended:** only in `Concrete.toml` policies.
   - (b) Also a source attribute. That would be a second place, and its spelling would be a
     separate question.
5. **Infallibility.**
   - (a) **Recommended:** a derived fact, only from an allocator success theorem.
   - (b) Bounds only.
