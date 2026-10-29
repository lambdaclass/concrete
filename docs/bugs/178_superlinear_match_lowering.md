# Bug 178: `match` lowering was cubic in the number of arms

<!-- bug-ledger
summary: a 1000-arm `match` spent 8.45 s in the `lower` stage (~n^3): SSAVerify kept dominator sets as string lists intersected by `filter`/`contains` and rebuilt every strict dominator's definitions per block, and SSACleanup and Lower added quadratic list lookups and appends; deep `if` nesting was cubic in the obligation walk (`assignedScalarsS`). Now O(1) dominance (CHK + dominator-tree times), map-based cleanup, newest-first Lower accumulation; emitted IR unchanged. Pinned by `check_ssa_lowering_scaling.sh` (lower-stage heartbeats and `instructions:u` slope ≤ 1.3). Class 17, owner R-0422
coverage: superlinear match lowering (compile time, not output). Regression vehicle is check_ssa_lowering_scaling.sh (lower-stage heartbeat and instructions:u slope <= 1.3 over 250-2000 arms), not a tests/programs fixture (2026-10-22)
class: 17 — a wide `match` was cubic in Lower + SSAVerify + SSACleanup; deep `if` nesting cubic in the obligation walk
silent: no
ruling: Violates: unrecorded
-->

**Status:** FIXED (`wip/bug178`)
**Found:** 2026-10-22, by the compiler perf tracking (`scripts/perf/` on `wip/perf-tracking`),
measured on main 34f19e97 and confirmed on 185d998c.
**Severity:** compile time only. Output was correct, but a 1000-arm `match` took 8.45 s in the
`lower` stage and a 2000-arm one about 71 s. A 2000-deep nest of `if`s took about 160 s in
`lower` (the same cause) plus about 100 s in the obligation walk (a second cause, below).

## Observed

The `match` family of the perf suite (and of `check_compiler_complexity.sh`):

```
mod m { fn main() -> Int { let x: Int = 0;
  let r: Int = match x { 0 => 0, 1 => 1, ..., 999 => 999, _ => -1 }; return r; } }
```

Retired user-space instructions of `concrete <file> --emit-llvm` (`perf stat -e
instructions:u`, concrete-ci-1), and the `lower` stage of `--emit-trace-json`:

| arms | instructions | lower stage |
|---:|---:|---:|
| 250 | 1.12 G | 119 ms |
| 500 | 6.78 G | 987 ms |
| 1000 | 42.2 G | 9201 ms |
| 2000 | 334.1 G | 70889 ms |

Growth exponent 2.60, 2.64, 2.98 between consecutive sizes. `--emit-ssa-unverified`, which runs
`Lower` without the verifier or the cleanup pass, cost 0.81 G instructions at 2000 arms (exponent
1.11): almost all of it was in the two passes the trace reports as part of `lower`. `perf record`
put 61% of the time in `List.elem` on strings, 15% in `List.reverseAux` and 15% in `memcmp`.

The perf suite also reported `lower` growing superlinearly on deep `if` nesting
(`if acc < k { acc = acc + 1; <rest> }`, n deep). On main that shape cost 4.07 G, 25.8 G, 188.2 G
and 1555 G instructions at depth 250, 500, 1000 and 2000 (exponent 2.66, 2.87, 3.05), with
`lower` at 160 s for depth 2000. With the dominance fix alone, `lower` fell to 0.3 s but the
compile still cost 422.9 G instructions: 96% of it was `Report.assignedScalarsS`, called by the
obligation walk that runs on every compile (`enforceProvenRuntimeViolations`) and that the trace
does not time.

## Expected

Lowering, verifying and cleaning up a function costs roughly linear time in its blocks,
instructions and phi entries.

## Cause

`Pipeline.lower` runs `Lower`, then `SSAVerify`, `SSACleanup` and `SSAVerify` again. The cubic
term was in `Concrete/IR/SSAVerify.lean`:

- `computeDominators` kept every dominator set as a `List String`. Each intersection was
  `a.filter (s.contains ·)`, each predecessor and set lookup a `find?` over all blocks, and each
  pass rebuilt the result with `acc ++ [(lbl, val)]`. A wide `match` lowers to a chain of about
  2n check and arm blocks whose dominator sets have O(n) elements, so one pass was
  O(blocks × set²) = O(n³).
- `strictDomDefs` listed, for every block, the definitions of every strict dominator
  (`ctx.blocks.filter (strictDoms.contains ·)` and an `acc ++ blockDefs b` fold) before checking a
  single use: O(n²) work per block.
- `buildPredecessors` filtered all blocks for every label, `checkPhiNodes` looked up predecessors
  with `find?` and checked an arm-wide phi's labels against the predecessor list with list
  membership both ways, `checkBranchTargets` used a list of labels, `checkCallArity` a list of
  signatures, and `buildRegTypes` built a register-type list nothing read.

`Concrete/IR/SSACleanup.lean` and `Concrete/IR/Lower.lean` added quadratic terms of the same
shape: `findReachable` (a list as the visited set, `blocks.find?` per label, `wl ++ succs`),
`eliminateDeadBlocks` and `stripStalePhiEntries` (list membership, a per-block scan of all
blocks), `findEmptyBlocks` (a `find?` per empty block, then a scan of the target's phis, which is
O(arms²) when every arm is an empty block into one merge), `foldEmptyBlocks` (one pass over every
block per folded block), `applyReplacements` (`replacements.length` recomputed for every key, and
each key's replacement chain followed separately), the per-block `acc ++ blockRepls` folds, and in
`Lower` the `blocks ++ [block]` in `terminateBlock`, the `getLast?` in `currentBlockTerminated`,
`entryAllocas ++ [inst]`, `phiIncoming ++ [i]` / `armEndSnapshots ++ [snap]` per arm, and
`insertStoreBeforeTerm` mapping over every block once per arm of an aggregate merge.

The deep-nesting cost was `assignedScalarsS` in `Concrete/Report/ReportObligations.lean`: it built
its result with `t.flatMap … ++ …`, copying every level's list into its parent's, so asking about
a statement nested k deep cost O(k²); the scoped walk asks once per level, O(n³) in all. It also
ran when the scope was empty, where the answer cannot matter.

## Fix

- `SSAVerify`: a `DomInfo` built once per function. Labels map to node indices through a
  `HashMap`; immediate dominators come from the Cooper-Harvey-Kennedy iteration over `Array`s in
  reverse postorder; dominator-tree DFS entry/exit times answer "does `d` dominate `n`" in O(1).
  Uses are checked per use against a register → defining-blocks map (`definedInStrictDom`) instead
  of materialized per-block definition lists. Predecessors, branch targets, phi labels and call
  signatures are `HashMap`/`HashSet` lookups; the unused register-type list is gone.
- The relation is the one the old fixpoint computed, including its unusual cases. The old
  dataflow found the greatest fixpoint of `D(entry) = {entry}`, `D(r) = {r}` for a block `r` with
  no predecessors, and `D(n) = {n} ∪ ⋂ D(p)`. For a block reachable from a root (the entry or a
  predecessor-less block), that is its dominators in a graph where a virtual root precedes every
  root. For a block reachable from no root (only from a cycle nothing enters), it is every
  label, since such a set never shrinks. `DomInfo` answers both. Duplicate labels resolve to their
  first occurrence, as `find?` did. The first defining block of a register (used in error
  messages) is still the first in block order, and errors are produced in the same order.
- `SSACleanup`: a label → block map and a `HashSet` visited set for reachability; one
  predecessor map for stale-phi stripping; one label → phi-sources map for empty-block detection;
  the empty-block renames composed into one map, built back to front, and applied in one pass
  (the same labels as the old sequence of passes); replacement chains resolved once, with every
  key on an acyclic chain sharing its end (keys on or leading into a cycle keep the old
  fuel-bounded chase, whose result there depends on the fuel); per-block replacement lists
  flattened once.
- `Lower`: finished blocks and entry allocas accumulate newest first and are reversed once in
  `lowerFn`; the arm lists of a `match` are prepended and reversed once; the merge stores of an
  aggregate merge are inserted in one pass over the blocks (`insertStoresBeforeTerm`).
- `ReportObligations`: `assignedScalarsS` uses an accumulator (same names, same order), the
  scoped walk skips it for an else-less `if` when the scope is empty (the catch-all arm is left
  as it was: it is the `fact-invalidation` mutation's anchor, and `dropStaleHyps` answers `[]`
  for an empty scope), and `dropStaleHyps` checks membership in a `HashSet`.
- `--emit-trace-json` reports per-stage Lean heartbeats (`alloc_heartbeats`), a deterministic
  count of small allocations that does not depend on machine load.

Emitted output is unchanged. `--emit-llvm` and `--emit-ssa` were run on every `.con` file under
`tests/` and `examples/` (1684 files) and `concrete build --emit-llvm` in every `Concrete.toml`
project, with main (185d998c) and this branch (5a4ec78e), comparing stdout, stderr and exit
code: 0 files differ (1216 of the `--emit-llvm` runs succeed; the rest are rejection fixtures,
whose diagnostics are identical too). No ordering was changed intentionally.

After the fix (instructions, concrete-ci-1):

| shape, size | main | fixed | speedup |
|---|---:|---:|---:|
| match, 250 arms | 1.12 G | 0.221 G | 5.0x |
| match, 500 | 6.78 G | 0.272 G | 25x |
| match, 1000 | 42.2 G | 0.407 G | 104x |
| match, 2000 | 334.1 G | 0.815 G | 410x |
| nesting, depth 250 | 4.07 G | 0.280 G | 15x |
| nesting, 500 | 25.8 G | 0.421 G | 61x |
| nesting, 1000 | 188.2 G | 0.843 G | 223x |
| nesting, 2000 | 1555 G | 2.21 G | 703x |

Least-squares growth exponent over 250 – 2000 (whole `--emit-llvm` compile, instructions): match
2.73 → 0.62, nesting 2.86 → 0.99 (pairwise 0.30 / 0.58 / 1.00 and 0.59 / 1.00 / 1.39; the low
values reflect fixed start-up cost). The `lower` stage's heartbeats grow with slope 1.00 for both
gate shapes. The `lower` stage of a 2000-arm match went from 78.4 s to 64 ms. `functions` and
`statements` are unchanged (1.27 / 1.42 and 0.32 / 0.54 before and after, within noise).

The largest example projects (`concrete build --emit-llvm`, instructions, main → fixed) have no
wide matches or deep nests, so they gain little: lox 5.13 G → 4.58 G (11%), hmac_sha256 59.50 G →
59.26 G (0.4%), fixed_capacity 18.69 G → 18.47 G (1.2%), toml 4.47 G → 4.23 G, rocksdb_kv
4.39 G → 4.17 G, kvstore 4.24 G → 4.02 G, integrity 4.24 G → 4.02 G, packet 4.19 G → 3.96 G
(5% each).

**Regression gate.** `scripts/tests/check_ssa_lowering_scaling.sh` compiles a value `match` and an
assigning `match` with an unknown scrutinee at 250, 500, 1000 and 2000 arms, fits the log-log
slope of the `lower` stage's heartbeats, and fails above 1.3. Where `perf` can count
`instructions:u` it also fits the slope of the whole `--emit-llvm` compile. A size that does not
compile, or a trace without the heartbeat count, fails. It is registered as a CI step, as
`make test-ssa-lowering-scaling`, and in the `ssa` area of the pre-push hook.
`check_no_quadratic_append.sh` baselines go down: `Lower.lean` 64 → 32, `SSAVerify.lean` 2 → 1.

**Residuals (recorded, not fixed here).**

- CHK `intersect` at a merge with n predecessors walks each predecessor's dominator chain:
  O(n × depth) at an n-arm merge. It allocates nothing and is about 13% of a 2000-arm compile.
  Semi-NCA or Lengauer-Tarjan would remove it if a workload ever needs that.
  **Fixed 2026-10-26 (follow-up, branch `wip/bug178-dom-order`):** the R-0550 follow-up probes
  measured it as the larger half of a wide match's `lower` stage once bug 297 removed the
  merge's list appends (one `match` of n literal arms each assigning a local: 8000 arms 247 ms,
  10000 arms 352 ms, 16000 arms 895 – 940 ms on bug 297's compiler; Codex review 2026-10-26:
  CONFIRMED — the ordered CHK predecessor fold performs quadratic chain walking). `SSAVerify`
  now computes immediate dominators by Lengauer–Tarjan (`ltIdom`: the simple version, path
  compression without balancing, O(E log V); an explicit-stack DFS and an iterative compression,
  so no recursion is as deep as the graph) over the same graph, the virtual root preceding every
  root, so roots, unreachable regions and the dominator-tree DFS times are as before. Immediate
  dominators are unique, and the two algorithms agreed on every function of the 2067 programs of
  `tests/programs` and `examples`, 3000 generated programs and the scaling probe shapes (a
  temporary in-compiler comparison, removed before the commit, shown able to fire by a
  deliberately wrong result). `lower` at 8000 / 10000 / 16000 arms: 65 / 86 / 170 – 191 ms
  (the stage's instruction count grows 2.17 times from 4000 to 8000 arms). `--emit-ssa` and
  `--emit-llvm` are byte-identical with bug 297's compiler on the corpus and on 3000 generated
  programs. `run_tests.sh` times `lower` of a 16000-arm match against 400 ms, retried once at
  800 ms.
- Deep nesting (now n^1.39 at depth 1000 → 2000, 2.2 G instructions at 2000): the
  remaining quadratic terms are `Proof.stmtFlatUses` / `exprFlatUses` (the same `++`-per-level
  collector shape as `assignedScalarsS`, in proof identity bytes), `Lower.cstmtTakesAddrOf`
  re-walking each `if`'s subtree from `prePromoteAddrTaken` once per level, and
  `Resolve.lookupName`.
- `elaborate` on many functions (~n^1.95) and `parse` on long straight-line code (~n^1.84) are
  separate passes with separate causes, not investigated here.

## Structural cause

Compiler passes hold per-function state in association lists and recompute derived relations per
query: a `List (String × _)` searched with `find?`, a `List String` used as a set, a result built
by `acc ++ [x]` or `xs ++ ys` per element or per level, and relations (dominators, predecessors,
definitions, phi sources) rebuilt for each block or each use instead of once per function. Every
such choice is linear per step, so each pass is quadratic, and a pass that nests two of them
(the verifier's per-block dominator lists over per-block definition lists) is cubic. Nothing in
the pipeline states a complexity envelope per pass, and the existing guard
(`check_compiler_complexity.sh`) times wall-clock at sizes up to 200, where the cubic term was
still small, so each instance was found by measurement after the fact. Bug 026 (the parser's
`elems ++ [x]`), bug 027 (EmitSSA and the instruction-list quadratics) and the 2026-07 O(n²)
sweep (lexer, dominators, `collectAllUses`, `Mono.lookupFn`, reassignment chains) are the same
class.

## Structural owner

New class 17 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md), "Pass state in association lists
and per-query recomputation (superlinear compile time)", owned by **R-0422**, the compiler
throughput queue ("close measured compiler and emitter performance defects under one owned
queue ... require an explicit expected complexity envelope; and gate the class rather than one
file"). This fix adds the envelope gate for the `lower` stage. Lower's block and arm
accumulation belongs to ROADMAP #5, the structured Lower/SSA builder, whose merge primitives
should own them. SSAVerify's dominance belongs to R-0504 (S12 validators), whose SSA validator
should keep the O(1) dominance query.
