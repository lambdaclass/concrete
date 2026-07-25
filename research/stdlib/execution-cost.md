# Execution Cost Analysis

**Status:** Roadmap-committed in layers. R-0445 owns the early source-resource
certificate; R-0244/R-0248 own obligation and target-budget strengthening;
R-0416–R-0419 own measurements; R-0405 retains hardware WCET research.

This note explores whether Concrete could provide static analysis of function execution cost — from simple structural reports up to bounded worst-case estimates.

## Why This Fits Concrete

Most languages make cost analysis very hard because of:

- hidden control flow (virtual dispatch, exceptions, implicit conversions)
- hidden allocation (GC, implicit copies)
- opaque call targets (closures, trait objects, dynamic linking)

Concrete avoids all of these by design:

- no hidden control flow — what you see is what executes
- no open-world dynamic dispatch — direct targets and conservative target sets
  for explicit indirect calls are enumerable after monomorphization
- no hidden allocation — `with(Alloc)` marks every allocation path
- no closures, no trait objects, no implicit conversions
- SSA IR already has a clean CFG and call graph

That makes Concrete unusually well-positioned for cost analysis compared to most systems languages.

## Levels Of Ambition

### Level 1: Structural cost reports

Compiler reports that describe the *shape* of a function's cost without computing a number.

Useful outputs:

- whether a function has unbounded loops
- whether a function is recursive (and to what depth)
- max static call depth
- whether all loop bounds are statically known
- which functions are "bounded" (no recursion, all loops bounded, no dynamic call targets)

This is purely structural — walk the call graph and CFG, classify functions. No hardware model needed.

Why this is valuable:

- it is just another `--report` output, fits the existing audit infrastructure
- it helps reviewers reason about latency and scheduling
- it identifies which functions *could* be analyzed more deeply
- it separates "provably bounded" from "might run forever"

Implementation cost is comparatively low because the compiler already has
loop, call-edge, target-set, allocation, layout, and stack facts. R-0445 must
compose those facts rather than add a second AST/CFG truth source.

### Level 2: Abstract source-step upper bounds

For functions where all loops are bounded and there is no recursion, compute an upper bound on total operations.

This requires:

- loop bound annotations or inference — something like `#[bound(N)]` on loops, or automatic inference for simple patterns like `for i in 0..n` where `n` is a parameter
- an explicit, versioned source cost model that counts operations without
  modeling hardware
- a conservative structural fold first; IPET (Implicit Path Enumeration
  Technique) is an optional later precision improvement only if workload
  evidence justifies solver complexity

The result is: "under these named input bounds and assumptions, this function
uses at most N source-model steps." If the required target set, loop bound, or
boundary contract is incomplete, the result is `unknown` with the exact reason.

That is useful for:

- relative comparison between implementations
- scheduling budgets in concurrent systems
- detecting regressions ("this function used to cost 200 ops, now it costs 2000")
- deciding whether a function is cheap enough to inline or call in a tight loop

Implementation cost: **moderate** — requires loop bound annotations, ILP encoding, and an ILP solver (GLPK or CBC are open-source). The SSA IR makes the CFG encoding straightforward.

### Level 3: Cycle-accurate WCET

Full Worst-Case Execution Time analysis with a hardware timing model.

This requires:

- a cycle-accurate model of the target CPU's pipeline
- cache analysis (abstract interpretation over cache states to classify each memory access as always-hit, always-miss, or unknown)
- branch prediction modeling
- bus and memory contention modeling (especially for multicore)
- TLB behavior

This is what tools like AbsInt aiT (commercial, used in Airbus avionics) and OTAWA (open-source academic) do. AbsInt has been building aiT for 20+ years with hardware models for hundreds of specific processors.

Implementation cost: **very high** — years of specialist work per target architecture.

Recommendation: **do not build this**. If Concrete ever needs cycle-accurate WCET, integrate with an existing tool (OTAWA is open-source, or emit annotations that AbsInt can consume) rather than reimplementing the hardware modeling.

## Why Safety-Critical Systems Use Simple Hardware

Avionics and space systems often run on deliberately simple processors (no out-of-order execution, no speculative prefetch, simple caches) specifically to make WCET analysis tractable. The LEON SPARC processor used in ESA missions is popular partly because it is easy to model.

This is relevant because it means Level 3 difficulty depends heavily on the target. A simple in-order core is orders of magnitude easier to model than a modern out-of-order x86.

## Relationship To Preemption And Concurrency

Execution cost analysis connects directly to scheduling and concurrency:

- if you can bound a function's cost, you can schedule it statically without needing preemption
- if you cannot bound it, you need either cooperative yields or preemptive interruption
- a capability like `with(Bounded)` could mark functions where the compiler verifies all loops have static bounds — useful for real-time task scheduling

This should be developed alongside the concurrency design, not independently.

See [concurrency.md](concurrency.md).

A Concrete-flavored approach to fair scheduling might combine:

- explicit yield points as a capability (`with(Yield)`) for cooperative scheduling
- structured concurrency where task boundaries are the natural preemption points
- cost reports showing "this function has no yield point for N call depth"
- budget-passing as an explicit value, not a hidden counter (unlike Erlang's implicit reduction counting)

That would keep the explicitness while addressing the real need for bounded execution.

## Relationship To Other Work

- [concurrency.md](concurrency.md) — scheduling, preemption, and structured concurrency
- [capability-sandboxing.md](capability-sandboxing.md) — `with(Bounded)` as a possible capability
- [ten-x-improvements.md](ten-x-improvements.md) — audit outputs as a 10x improvement

## Recommended Order

1. R-0444's parser combinators provide a real bounded-loop forcing workload.
2. R-0445 ships one source-scoped structural/symbolic resource certificate and
   explicit unknown reasons.
3. R-0244 upgrades useful bounds into obligations; R-0224/R-0248 bridge and
   enforce named target stack budgets.
4. R-0416–R-0419 correlate source facts with measurements without relabeling
   source steps as time.
5. Level 3 proceeds only for a specific deterministic target, preferably by
   exporting annotations to an existing WCET tool.

## Bottom Line

Concrete's design makes cost analysis much more tractable than in most languages. The right approach is:

- structural and symbolic source-resource facts are a natural extension of the
  audit output story
- useful bounds must name their source cost model, assumptions, and completeness
- Level 3 is a trap unless you integrate with existing tools

The biggest leverage is making cost *visible*, not making it *perfect*.
