# Bug 031: lazy address-of promotion inside a branch leaves the alloca uninitialized on sibling paths

**Status:** Fixed (2026-07-14) — all three sites; trust battery green
**Fixed in:** Lower.lean `prePromoteAddrTaken` (new helper) called by the
`.ifElse` statement, `.ifExpr` value-if, and `.match_` lowerings: any pre-branch
local whose address is taken inside a branch/arm is promoted BEFORE the
conditional terminator (alloca in entry, initializing store in the dominating
pre-branch block). Mirrors the C9 rule the while-loop lowering already applied
to its body. Match excludes names any arm REBINDS (enum payload binding /
var arm) — promotion is keyed by name and a shadowed `setVar` would store the
arm-local value into the outer variable's alloca.
**Regression tests:** `tests/programs/regress_031_if_branch_borrow.con` (101),
`regress_031_ifexpr_branch_borrow.con` (107), `regress_031_match_arm_borrow.con`
(8) — one per site.
**Battery:** ssa-verify-agreement 5/0, differential fuzz 450/0 (depths 3+4),
golden 54/0, fast 1648/0, examples 132/0, oracle 70/0.
**Discovered:** 2026-07-14
**Discovered in:** Phase 7 workload 1 — `base64_cli` decode segfaulted (rc=139)
on every input while encode worked.

## Symptom

A non-Copy local borrowed (most commonly via method-call autoborrow — `x.m()`
is `M_m(&x)` in Core) in MORE THAN ONE branch of an `if/else`, in a value-`if`
arm, or in a `match` arm not taken at runtime, reads garbage on the paths that
do not pass through the branch lowered FIRST. `Bytes.len()` returned garbage
silently; `Bytes.get_unchecked()` dereferenced a garbage pointer (SIGSEGV/
SIGTRAP). Scalars whose address is taken explicitly (`&n`) in two branches hit
the same hole.

## Minimal repro (differential: interp 101, compiled -1 before the fix)

```concrete pseudocode
struct Box3 { a: u64, b: u64, c: u64 }
impl Box3 { fn geta(&self) -> u64 { return self.a; } }
fn main() -> Int {
    let flag: bool = cond();          // false at runtime
    let x: Box3 = Box3 { a: 1, b: 2, c: 3 };
    let mut rc: Int = 0;
    if flag { rc = x.geta() as Int; }             // first &x lowered HERE
    else    { rc = (x.geta() as Int) + 100; }     // reads uninit alloca
    destroy(x);
    return rc;
}
```

## Root cause

`addrOfLocal` promotes a local to a stable alloca lazily, at the FIRST
address-take encountered during lowering: it emits the entry-block alloca, an
initializing store of the current value **into the block being lowered at that
moment**, and registers the promotion globally (`addPromotedAlloca`). When that
first address-take sits inside a branch, the store only executes on that
branch's path — but the global registration reroutes ALL subsequent
reads/writes (`lookupVar`/`setVar`) through the alloca, including the sibling
branch and post-merge code. Every path that does not execute the promoting
branch reads uninitialized stack memory.

The emitted SSA made it obvious (probe, pre-fix): the struct literal
initialized `%t0`, the promotion created `%addr.5`, and `Box3_geta(ptr
%addr.5)` was called with no store to `%addr.5` anywhere on the executed path.

The while-loop lowering already knew this rule — it pre-promotes aggregates
and address-taken scalars BEFORE the loop terminator (the C9 fix). The three
branch lowerings simply never got the same treatment.

## Why nothing caught it earlier

- The differential fuzzer's expression grammar does not generate struct
  methods/borrows inside both arms of a branch.
- Most real code takes the first borrow of a local in straight-line code
  (which dominates everything after), so the lazy store was usually fine.
- In `base64_cli` the very first borrow of `input` was `encode(&input)` /
  `decode(&input)` inside the command-dispatch branches — first-contact
  workload evidence doing exactly its job.

## Fix shape

`prePromoteAddrTaken preVars takes` — iterate the pre-branch var snapshot;
skip types `addrOfLocal` never promotes (arrays/refs/ptrs/heap are already
addressable) and already-promoted names; for each name where `takes name`
(composed from `cstmtsTakeAddrOf`/`cmatchArmTakesAddrOf`), emit entry alloca +
store in the still-dominating current block and register the promotion.

## Residual (pre-existing, narrower)

Promotion is keyed by NAME. A match arm that rebinds a promoted name (enum
payload binding or var arm) would `setVar`-store the arm-local value into the
outer variable's alloca. The new match pre-promotion EXCLUDES shadowed names;
the same name-collision hazard exists independently for the lazy promotion
path (promote `x`, then a later arm binds `x`) and predates this fix. Tracked
as a note here, not a hole: linear-use rules make the shape rare (the outer
`x` is usually consumed by then).
