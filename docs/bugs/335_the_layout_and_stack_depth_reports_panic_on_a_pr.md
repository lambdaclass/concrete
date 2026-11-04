# Bug 335 — The layout and stack-depth reports panic on a project file that uses std's Bytes and report zero sizes

**Status:** OPEN
**Found:** 2026-10-30, by the cleanup pass's identity sweeps (every report kind over the examples: `examples/cli_tool` panicked identically on both binaries); confirmed by Codex review
**Severity:** silent wrong result. The reports printed a Lean `PANIC` on stderr but exited 0 and stated sizes, alignments, offsets and frame sizes computed with 0 for the unknown type, so any consumer of the output read wrong numbers (ruling of 2026-10-30: stderr noise does not count as failing)

<!-- bug-ledger
compiler: legacy
summary: The layout and stack-depth reports panic on a project file that uses std's Bytes and report zero sizes
class: 11
coverage: archived reproducer(s) for bug 335; see Reproducer and Archive disposition below; no required passing regression against the frozen compiler
silent: yes
ruling: Outside the spec: tooling — the layout and stack-depth reports are report surfaces, not language behaviour; compilation is unaffected
-->

## Reproducer

`tests/regressions/bug335_the_layout_and_stack_depth_reports_panic_on_a_pr/repro.con`, the main
of a project (std is found as the bundled dependency; outside a project `std.cli` does not resolve):

```
$ mkdir -p p/src && cp repro.con p/src/main.con
$ printf '[package]\nname = "repro335"\nversion = "0.1.0"\n' > p/Concrete.toml
$ cd p/src && concrete main.con --report layout       # likewise --report stack-depth
PANIC at ... Concrete.Check.Layout:242:6: Layout.tyAlign: unknown named type 'Bytes'
PANIC at ... Concrete.Check.Layout:301:6: Layout.tySize: unknown named type 'Bytes'
...
$ echo $?
0
```

`examples/cli_tool/src/main.con` shows the same (`concrete main.con --report layout` in its
`src/`).

## Observed

On main `14121934` (OVH): `--report layout` and `--report stack-depth` print a run of Lean
`PANIC` lines for `Bytes` (`Layout.tyAlign`, `Layout.tySize`), then the report, and exit 0. The
report's numbers for any type that contains `Cli` are computed with size and alignment 0 for
`Bytes`. `concrete build` of the same project succeeds and the binary exits 0: compilation is not
affected.

## Expected

No language rule decides it (report surfaces). A report either computes the layout of every type
the program uses, std's included, or refuses with a diagnostic and a failure status; it never
prints a number derived from a default. The compiler's own layout for the build is correct
(`concrete build` lowers with the whole program's definitions).

## Cause

Codex review (2026-10-30), verdict REAL: the report's listing scope is used as its layout
universe. `Main.lean` (around 1724–1736) loads the enclosing project, then removes the dependency
modules and scopes the Core to the requested file; both reports receive only that scoped Core
(around 2896 and 3520). `Report.buildLayoutCtx` (`Concrete/Report/ReportInterface.lean`, around
634) collects type definitions from those modules alone, and the stack-depth report uses it too.
`std.cli`'s `Cli` has a field of type `Bytes`, defined in another std module the import closure
does not reach (`Concrete/Resolve/FileSummary.lean`, around 677 and 709). The unknown-type
branches of `Concrete/Check/Layout.lean` (242, 301) `panic!`, and Lean's `panic!` returns the
default `Nat`, 0, so the reports continue with wrong numbers and exit 0. `concrete build` keeps the
whole validated Core, and `Lower` supplements layout definitions from the entire program
(`Concrete/IR/Lower.lean`, around 2895–2929), so the build path is unaffected.

## Structural cause

Class 11 of [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md), absence read as a fact: a type the
layout table does not know is given size and alignment 0 (the `panic!` default) instead of failing,
like 052's `destroyable` default and 065's dropped callees. It also shows class 7's pattern (the
build and the reports compute layout over different universes).

## Structural owner

For the instance: one layout context built from the whole validated program and shared by
`Lower` and the reports (the reports then filter what they list, not what they know). For the
class: the layout functions return an error for an unknown type instead of `panic!` with a
default, so no caller can continue with 0; R-0494's single verdict authority is the reporting
side of the same rule.

## Historical branch containment (unlanded)

The archived branch attempted fail-closed containment (not landed): `Layout.unknownTypes`
(`Concrete/Check/Layout.lean`) returns exactly the named types for which `tySize`/`tyAlign` would
reach their `panic!` default, walking fields, variants and newtypes as they do; a nesting deeper
than its bound (256 type levels, 64 module levels) is refused too, never passed unchecked. Before printing,
`Main.lean` asks `Report.layoutReportUnknownTypes` (every non-generic struct and enum the layout
report lists) and `Report.stackDepthUnknownTypes` (every parameter and local of every function the
stack-depth report sizes); a non-empty answer prints `error: --report layout cannot lay out Bytes
(...; bug 335)` (likewise for stack-depth), prints no figure and exits 1. No other input changes:
across the identity sweep's 14389 report outputs, only `examples/cli_tool`'s layout and
stack-depth runs reached the panic. `check_stack_budget.sh` leg 6 pins the refusal on the project
reproducer. The repair (one layout context over the whole program, see the structural owner)
replaces the refusal with correct figures. The current [frozen-compiler rule](../archive/roadmap-2026-11/ROADMAP.md#defect-disposition-rule) records this legacy defect without scheduling repair.

## Archive disposition

Recorded against `legacy-compiler-final` (`066c9d9b8`); this defect remains open.
Source record: `refs/archive/branches/wip/bug-report-layout-panic`. Any repair or containment described above is an archived attempt, not a landed fix or current schedule.
Historical gate claims above describe the source branch; they do not register these fixtures in current gates. Expected results are the decided expected behaviour (or the explicitly stated spec gap), not required passes against the frozen compiler.

Preserved reproducer files:

- `tests/regressions/bug335_the_layout_and_stack_depth_reports_panic_on_a_pr/project/Concrete.toml`
- `tests/regressions/bug335_the_layout_and_stack_depth_reports_panic_on_a_pr/project/src/main.con`
- `tests/regressions/bug335_the_layout_and_stack_depth_reports_panic_on_a_pr/repro.con`
