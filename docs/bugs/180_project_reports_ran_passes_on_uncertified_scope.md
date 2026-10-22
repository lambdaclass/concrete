# Bug 180 — project reports ran Mono and lowering on modules CoreCheck never certified

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

A report that runs a compiling pass describes the program the build compiles. For wordfreq,
`--report mono` lists the specializations the build emits; `--report verify` checks them.

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
`ValidatedCore` CoreCheck produced for the whole program; the listing still iterates the invoked
file's modules. `--report mono` therefore reports the whole program's counts (for wordfreq 908
functions before mono, 1239 after, 325 specializations, a superset of the 61 the build emits). Standalone files are unchanged: there the whole program and the scoped modules were already
the same value.

The fix is the slice's type change, not a patch at the three sites: the record update that caused
this no longer compiles anywhere outside `Pipeline.lean` (see below).

## Structural cause

A pipeline artifact was a plain record. Its type stated which pass had accepted it, but nothing
bound that acceptance to the payload it carried, so any code could swap the payload and keep the
type, and every downstream consumer read the new payload as certified. Class 16 of
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
- `check_pipeline_artifacts.sh` leg B: wordfreq's project `--report mono` lists every
  specialization `concrete build --emit-llvm` emits (before: 0 of 61); legs U1 – U4 refuse the
  record update by elaboration, and D2 – D3 refuse a `CoreSubset` where Mono or the verify gates
  need a certificate.
