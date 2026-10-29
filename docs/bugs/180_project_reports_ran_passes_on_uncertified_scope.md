# Bug 180 — project reports ran Mono and lowering on modules CoreCheck never certified

<!-- bug-ledger
summary: in a project, `--report mono`, `verify` and `traceability` replaced a `ValidatedCore`'s modules by record update with the invoked file's modules (dependencies dropped, synthetic wrappers added) and ran Mono, lowering and the verify gates on that uncertified forest; wordfreq's report listed 0 specializations where the build emits 61, and verify called it clean. The reports now run their passes on the whole validated program and list the file's part, and R-0497 slice 1 seals the artifacts so the record update no longer compiles. Pinned by `regress_180_report_scope` (run_tests.sh) and `check_pipeline_artifacts.sh` (legs B, U, D)
fixtures: regress_180_report_scope/src/main.con
class: 18 — a project report replaced a `ValidatedCore`'s modules by record update and ran Mono and lowering on a forest CoreCheck never certified
silent: yes
exit: 0b
ruling: Outside the spec: tooling — project --report mono/verify/traceability ran passes on an uncertified module subset
-->

**Status:** FIXED (branch `verified-pipeline`)
**Found:** 2026-10-23, while sealing the pipeline artifacts (R-0497 slice 1): once
`ValidatedCore`'s constructor became private, the record update in `compileAndReport` that
replaced its modules no longer compiled.
**Severity:** silent wrong output. `--report mono` and `--report traceability` described a program
nothing compiles, and `--report verify` ran its four gates on that program and reported them clean.
Compilation itself was unaffected: `build`, `run` and `test` always used the whole program.

## Observed

Inside a project (`Concrete.toml` in scope), on `examples/wordfreq` at `185d998c`:

```
$ concrete examples/wordfreq/src/main.con --report mono
Functions before mono: 6
Functions after mono:  6
Generic functions:     0
Specializations:       0
$ (cd examples/wordfreq && concrete build --emit-llvm) | grep -c '^define.*_for_'
61
```

The build specializes 61 functions (`OrderedMap`, `Vec`, `Option`, the allocator helpers); the
report says there are none. `--report verify` printed `VERIFY-GATES: ok (all 4 gates clean)` on
the same six-function program, so its post-mono, post-lower and post-cleanup gates never saw any of
the 61 functions the binary contains. Every project example behaved the same way (all exit 0).

## Expected

A report that runs a compiling pass runs it on the program CoreCheck certified, and lists the
invoked file's part of the result. For wordfreq, `--report mono` lists the specializations its own
module instantiates (std's `OrderedMap`, `Vec`, `Option` instances among them), and
`--report verify` checks the whole program the build compiles.

## Cause

`compileAndReport` builds two views of a project's validated program: the user package (dependency
modules removed) for proof extraction, and the invoked file's modules (`scopeUserModulesToFile`,
which wraps matched subtrees in synthetic empty parents) for the report listing. Both were made by
record update, keeping the type:

```
let fullValidCore : ValidatedCore := { validCore with coreModules := userModules }
let scopedValidCore : ValidatedCore := { validCore with coreModules := scopedModules }
...
let validCore := scopedValidCore
```

`ValidatedCore` documents itself as "Core IR that has passed `coreCheckProgram` … Only
`Pipeline.coreCheck` constructs this". The scoped forest never passed CoreCheck: its wrappers are
new modules, and `std` and every other dependency were gone. The `verify`, `traceability` and `mono`
branches passed that value to `Pipeline.runVerifyGates` and `Pipeline.monomorphize`, which accepted
it because its type said it was certified. Mono found no generic definitions to instantiate (they
live in `std`), produced no specializations, and lowering and the verify gates ran on what was left.

## Fix

The three reports run Mono, lowering and the verify gates on `fullValidCore.source`, the
`ValidatedCore` CoreCheck produced for the whole loaded program; the listing still iterates the
invoked file's modules. "Whole loaded program" is not exactly the build's input: the report path
loads the project with dependency test functions kept, and `concrete build` strips them
(`loadProject … (stripTestFns := true)`). `--report mono` lists the invoked file's part of the
whole-program result (`Report.restrictMonoToScope`): each scope module's namesake in Mono's output,
with the specializations Mono placed there. Mono places an instance in the module that requests
it, so this is what the file instantiates, including instances of dependency generics, and it does
not move when an unrelated dependency changes. For wordfreq it lists 9 specializations, each one
the build emits (before: 0). Listing the whole program instead would have made five example
snapshots change with every `std` edit. Standalone files are unchanged: there the scope is the
whole program, and the restriction is the identity. `check_snapshots.sh` passes with no snapshot
changed.

The slice's type change is what makes the defect unrepresentable: the record update that caused
it no longer compiles anywhere outside `Pipeline.lean` (see below). The type change does not by
itself choose the repair. Re-validating the subset was the alternative, and it was rejected because
the subset without its dependencies is not a program CoreCheck can accept.

**Residuals, not changed here.**
- `--report traceability` matches mono and SSA names to a source function by bare name and the
  `_for_` prefix (`Report.collectTraceEntries`). With whole-program inputs, a project function named
  like a dependency function (`max`) can be shown that dependency's specializations and symbols.
  Standalone mode already passed the whole program, std included, so this is the existing
  standalone behaviour extended to projects. It is class 1 (declaration identity by spelled name),
  owned by R-0492.
- `--report core-semantics-diff` still builds its evaluation environment from the invoked file's
  modules, so a function that calls a sibling-file helper is excluded as not extractable. This is
  older than bug 180 and runs no compiling pass. The repair is to evaluate over the whole program
  and restrict only the reported roots.

## Structural cause

A pipeline artifact was a plain record. Its type stated which pass had accepted it, but nothing
bound that acceptance to the payload it carried, so any code could swap the payload and keep the
type, and every downstream consumer read the new payload as certified. Class 18 of
[STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md), its first entry.

## Structural owner

R-0497 (verified-pipeline S5, the artifact half). Slice 1, which lands with this fix, seals the
artifacts: `ValidatedCore`, `MonomorphizedProgram`, `SSAProgram` and `ElaboratedProgram` have
private constructors, and the first three carry erased `Prop` certificates about their exact
payload (`coreCheckProgram coreModules = .ok ()`; Mono's output equation and
`verifyPostMono coreModules = []`; the lowering chain and `ssaVerifyProgram ssaModules = .ok ()`).
Modules drawn from a validated program are a `CoreSubset` (with a `Sublist` proof against its
source), which proof extraction reads and no compiling pass accepts.

## Regression

- `tests/programs/regress_180_report_scope/` — a project whose `main` calls `std.math.max` at
  `u16`; `run_tests.sh` requires `--report mono` to list `max<u16> -> …` (before the fix it listed
  no specialization).
- `check_pipeline_artifacts.sh` leg B: wordfreq's project `--report mono` lists at least one
  specialization, and each one is a function `concrete build --emit-llvm` emits (before: none listed); legs U1 – U4 refuse the
  record update by elaboration, and D2 – D3 refuse a `CoreSubset` where Mono or the verify gates
  need a certificate.
