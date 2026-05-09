# Bug bundle (wrong-code corpus)

Status: contract. Operative tools:
`scripts/tests/capture_wrong_code_bundle.sh`,
`scripts/tests/minimize_wrong_code.sh --bundle`.

A bug bundle is the **portable artifact** that captures a single
compiler failure: the source, the invocation, the predicate that
identified it as a bug, the compiler version, the rendered
diagnostics, IR dumps where the pipeline got far enough to produce
them, and any reports that ran cleanly.

The pipeline is now:

```
new bug → reduce → bundle → manifest entry → permanent regression
```

Bundles serve two audiences:

- **Future readers** triaging the same bug: a self-contained
  directory that they can read end-to-end without the surrounding
  repo state.
- **CI / automation**: a stable layout that the wrong-code corpus
  workflow produces and consumes consistently.

Bundles are not on the production build path. They are debugging /
hardening infrastructure.

## Required layout

Every bundle is a directory with this skeleton. Files marked
**always** are written even if their content is empty (so consumers
don't have to special-case missing files); files marked **if
available** are present only when the corresponding stage of the
pipeline succeeded enough to produce them.

```
<bundle-root>/
├── bundle.json            — corpus-specific metadata          [always]
├── manifest.json          — pipeline state map (debug-bundle)  [always]
├── source/
│   ├── program.con        — canonical source filename          [always]
│   └── <basename>.con     — original source under its name     [always]
├── command.txt            — invocation that surfaced the bug    [always]
├── predicate.txt          — predicate used to identify the bug  [always]
├── compiler-version.txt   — `concrete --version` banner         [always]
├── stdout.txt             — captured stdout of the invocation   [always]
├── stderr.txt             — captured stderr (incl. diagnostics) [always]
├── diagnostics.txt        — Lean-rendered diagnostic list       [if-failed]
├── core.txt               — Core IR dump                        [if-elab]
├── ssa.txt                — verified SSA IR dump                [if-lower-clean]
├── ssa-unverified.txt     — pre-verify SSA dump                 [if available]
├── llvm.ll                — emitted LLVM IR                     [if-emit]
├── consistency.txt        — ProofCore self-check                [if-corecheck]
├── verify.txt             — verifier diagnostic list            [if-verify-fail]
└── reports/
    ├── caps.txt           — `--report caps`                     [if-clean]
    ├── alloc.txt
    ├── layout.txt
    ├── authority.txt
    ├── effects.txt
    ├── unsafe.txt
    ├── recursion.txt
    ├── stack-depth.txt
    └── fingerprints.txt
```

`reports/` is only present if at least one report ran cleanly. An
empty bundle (no failure-stage diagnostics, no IR dumps, no reports)
is still legitimate — the source compiled cleanly, and the bundle
captures the *invocation context* (`command.txt`, `predicate.txt`,
`compiler-version.txt`) that classified it as a bug.

## What is always included

These files exist in every bundle, regardless of how far the
pipeline got:

- `bundle.json` — corpus-specific metadata: case id, predicate,
  source filename, line count, capture script identity. Sibling to
  `manifest.json` (the structural debug-bundle map). Consumers
  should prefer `bundle.json` for corpus questions and
  `manifest.json` for pipeline-state questions.
- `manifest.json` — produced by `concrete debug-bundle`. Records
  which pipeline stage failed and which IR artifacts are present.
- `source/program.con` — canonical filename so consumers don't have
  to look up the original basename. Identical content to the
  original source.
- `command.txt`, `predicate.txt`, `compiler-version.txt` — the
  debugging context.
- `stdout.txt`, `stderr.txt` — captured streams from re-running the
  failing invocation.

## What is included only if available

These files appear when the pipeline produced them:

- `diagnostics.txt` — populated when any pass returns errors.
- `core.txt` — Elab succeeded and produced a Core IR.
- `ssa.txt` — Lower succeeded *and* the SSA verifier accepted the
  output. If Lower succeeded but the verifier rejected, `ssa.txt`
  is absent and `ssa-unverified.txt` is captured instead.
- `ssa-unverified.txt` — the raw Lower output (no verifier, no
  cleanup), captured via `concrete <file> --emit-ssa-unverified`.
  Useful for verifier-rejection debugging (the WC-0004 path).
  Captured opportunistically: present whenever Lower runs at all,
  even on a clean compile.
- `llvm.ll` — Emit succeeded; the LLVM IR is the same that would go
  into clang.
- `consistency.txt` — ProofCore's self-check ran; the file lists any
  violations (or is empty if none).
- `verify.txt` — post-elab verifier produced warnings.
- `reports/<kind>.txt` — `--report <kind>` ran cleanly. Reports
  that error are skipped silently.

## Relation to `tests/wrong-code/manifest.toml`

The bundle is the **artifact**; the manifest entry is the
**registration**.

When a reduced repro is ready to land:

1. Capture a bundle:
   ```sh
   scripts/tests/capture_wrong_code_bundle.sh <source.con> \
       --case WC-NNNN --predicate <KIND>:<ARG> -o tests/wrong-code/cases/WC-NNNN
   ```
   or chain it from the reducer:
   ```sh
   scripts/tests/minimize_wrong_code.sh <original.con> \
       --predicate <KIND>:<ARG> \
       -o <reduced.con> \
       --bundle tests/wrong-code/cases/WC-NNNN
   ```
2. Add a manifest entry per the wrong-code corpus contract
   (`docs/WRONG_CODE_CORPUS.md`). The bundle's
   `tests/wrong-code/cases/WC-NNNN/source/program.con` becomes the
   `repro` path; the `predicate` field in `bundle.json` translates
   into the manifest's `kind` + `expected`.
3. Write the per-case notes file. Reference the bundle directory
   directly — the bundle is the artifact pointer the notes file
   describes.

For corpus cases that already have a stable repro outside
`tests/wrong-code/cases/<id>/` (most `bug_*.con` and adversarial
entries), bundle capture is **optional** and only worth running when
triaging the bug or producing release evidence — the existing repro
already serves the regression role.

For new bugs that have no other home, bundle capture is the path of
least resistance: it produces the case directory in the right shape
in one command.

## Smoke test

`make test-bundle-smoke` (alias for
`scripts/tests/test_bundle_smoke.sh`) captures a bundle on a known
failing source and asserts every always-included file is present
plus at least one stage-conditional file. Does not run a long
reduction; that is `make test-reducer-smoke`'s job.

## See also

- `docs/WRONG_CODE_CORPUS.md` — the manifest schema and lifecycle.
- `docs/REDUCER_WORKFLOW.md` — the reducer that feeds this pipeline.
- `Concrete/DebugBundle.lean` — the in-tree pipeline-state capture
  used as the structural skeleton.
