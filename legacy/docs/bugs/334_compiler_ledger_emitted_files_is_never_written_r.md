# Bug 334 — Compiler ledger emitted_files is never written: --report compiler-ledger shows [] even after a build

**Status:** OPEN
**Found:** 2026-10-30, by the dead-code finder (`recordEmitted` has no caller), confirmed by Codex review
**Severity:** a report states an absence that is not true: `emitted_files` is always `[]`

<!-- bug-ledger
compiler: legacy
summary: Compiler ledger emitted_files is never written: --report compiler-ledger shows [] even after a build
class: 11
coverage: archived reproducer(s) for bug 334; see Reproducer and Archive disposition below; no required passing regression against the frozen compiler
silent: no
ruling: Outside the spec: tooling — the compiler ledger is a report surface, not a language behaviour
-->

## Reproducer

`tests/regressions/bug334_compiler_ledger_emitted_files_is_never_written_r/repro.con`, built as
the only source of a project (any program shows it; the defect is in the driver):

```
$ mkdir -p p/src && cp repro.con p/src/main.con
$ printf '[package]\nname = "ledgerprobe"\nversion = "0.1.0"\n' > p/Concrete.toml
$ cd p && concrete build -o probe        # Built probe
$ concrete --report compiler-ledger --json | jq .emitted_files
[]
```

## Observed

On main `14121934` (OVH): the build succeeds and writes `probe`; the ledger report exits 0 and
its `emitted_files` is `[]`. The same holds with any project and any build output path.

## Expected

No language rule decides it (a report surface). The ledger's own design says it records
"emitted files and replay commands" ([COMPILER_PIPELINE.md](../compiler/COMPILER_PIPELINE.md),
the compiler-ledger fact list), so either the list names the files the build wrote, or the field
is absent or marked unknown. An empty list claims that nothing was emitted.

## Cause

`Concrete/Report/CompilerLedger.lean`: `recordEmitted` is the only writer of `emittedFiles`, and
nothing calls it. The report also rebuilds the ledger from scratch at report time
(`Main.lean`, the `compiler-ledger` report), before and independently of any emission, so even a
caller in the build path would not reach the report.

## Structural cause

Class 11 of [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md), absence read as a fact: a field
that no run fills defaults to the empty list, and the empty list is rendered as the fact "no files
were emitted". Like 058 and 063, a default stands in for "not observed".

## Structural owner

The compiler-ledger work recorded in
[PHASE4_COMPILER_LEDGER_AUDIT.md](../verification/PHASE4_COMPILER_LEDGER_AUDIT.md): the ledger
must either be persisted by the build that emits (so the report reads the build's record) or say
"not recorded" for facts no run filled. For the class, R-0494's single verdict authority is the
pattern: a surface renders an observed absence and an unobserved field differently.

## Historical branch containment (unlanded)

No gate pins it yet; the record's open status is the rationale. The dead-code ratchet
(`wip/cleanup-dead-code`) keeps `recordEmitted` in its baseline with a `keep:` reason naming this
defect, so the writer is not removed before the fix. Meanwhile a user reads the build's own output
path, not `emitted_files`. The current [frozen-compiler rule](../archive/roadmap-2026-11/ROADMAP.md#defect-disposition-rule) records this legacy defect without scheduling repair.

## Archive disposition

Recorded against `legacy-compiler-final` (`066c9d9b8`); this defect remains open.
Source record: `refs/archive/branches/wip/bug-ledger-emitted-files`. Any repair or containment described above is an archived attempt, not a landed fix or current schedule.
Historical gate claims above describe the source branch; they do not register these fixtures in current gates. Expected results are the decided expected behaviour (or the explicitly stated spec gap), not required passes against the frozen compiler.

Preserved reproducer files:

- `tests/regressions/bug334_compiler_ledger_emitted_files_is_never_written_r/repro.con`
