# Brief A: spec-vs-compiler differential with a seeded generator — branch gate-spec-diff

You are working in the full clone /home/unbalancedparen/projects/gates-spec-diff, branch
`gate-spec-diff`. It is based on branch `test-manifest` (brief B, the one test manifest
`tests/manifest.tsv`, its reader `scripts/checks/lib/test_manifest.py` and the one runner
`scripts/checks/compiler/tests.sh`), which is itself based on origin/wip/slice0 (slice 0 is not on
main yet). Read AGENTS.md (repository root), CLAUDE.md, Concrete/README.md (pipeline, gates 4/5/6) and
the brief-B changes (`git log -p origin/wip/slice0..HEAD`) first.

Do NOT push, do NOT touch git remotes, do NOT run `git rebase/reset/clean/stash`. Commit on the
branch in small commits. Every command you run is bounded with `timeout`. No heavy runs: at most
`timeout 1800 lake build Concrete` to typecheck; never the native gates (I run those on the CI
server). Scratch only under mktemp.

## Goal (owner-approved, verbatim intent)

"Spec-vs-compiler comparison from day one. Every test program runs both through the executable Lean
spec and as a native binary from the new compiler; observable results (exit code, stdout bytes,
trap kind) must match exactly; a mismatch fails the gate. Plus a random program generator
restricted to the currently accepted slice subset, seeded and reproducible, that grows with each
slice (each slice adds its constructs to the generator). Bound the run time per CI job; failing
seeds are saved as regression tests."

## The state you must work with

- The spec has an executable evaluator: `ConcreteSpec.run : Program → Nat → Outcome`
  (ConcreteSpec/Spec/Slice1Semantics.lean) and `ConcreteSpec.check`. Slice 0's Core is
  `ConcreteSpec.Slice0.Program` with `toCore` (ConcreteSpec/Spec/Slice0.lean).
- The compiler today is BIR + two backends (LLVM printer, LCB adapter) + the runtime entry. There is
  NO Core→BIR lowering yet (that is slice-1 work with no owner yet). So the native leg can only
  start from BIR today.
- Decision from the coordinator (do not reopen it): ship the gate now in two legs:
  1. **Spec leg**: a seeded generator of slice-0 Core programs (`Slice0.Program`), filtered by the
     spec's `check` (accepted programs) — oracle = `run (toCore q)`. A fixed hook
     `compileCore : ConcreteSpec.Program → Except String Concrete.BIR.Program` (in the gate's Lean
     driver, documented as THE place the lowering branch plugs in) that today returns
     `.error "no Core→BIR lowering yet (slice 1)"`. While the hook errors, the spec leg still
     generates, runs the oracle, checks the generator's own invariants (every generated program is
     in the fragment and accepted, outcomes are exits, the distribution covers each construct), and
     prints, loudly, `spec leg: 0 programs compiled (no Core→BIR lowering; becomes required in the
     lowering branch)`. It must never print or imply that a spec comparison passed when none ran.
     When the hook returns a BIR program, the driver writes it for the native leg exactly like a
     generated BIR program (below), with the expected observation computed FROM THE SPEC's outcome
     (exit c → status c, empty stderr; trap k → status 134 and the runtime trap line for the
     site) — so the lowering branch only has to fill the hook.
  2. **Interim BIR leg**: a seeded generator of BIR programs over the shapes the LCB adapter
     accepts today (see `Concrete/BIR.lean` Examples: checked `binOvf` with a `condbr` to a trap
     block, wrapped ops observed through `trunc`, widening, several sites, multi-block chains;
     8/16-bit types). Each generated program must be `wf` and accepted by `LCB.emit` (generate,
     then filter; count rejects and fail if the yield falls below a stated floor). Expected =
     BIR `run`; legs = LLVM-native, LCB-native, and LCB's in-process evaluator
     (`LCB.evalBody`), all compared exactly (status, stderr bytes, stdout empty) through the SAME
     emit/build/observe/judge machinery tests.sh uses (`slice0_bir.lean emit`,
     `slice0_common.sh` `observe`/`judge`, `slice0_native.sh`). Do not write a second judge.
- Randomness: a pure PRNG in Lean (e.g. splitmix64 on UInt64), no IO randomness. `seed` + index
  determines each program. The gate's seed comes from `SPEC_DIFF_SEED` or, by default, from the
  commit SHA (so each commit explores differently and any run is replayable); print the seed and a
  one-line replay command first.
- The generator lives in ONE Lean file per leg's generator, organised so a slice adds its
  constructs in one place (a list of weighted productions); write a short comment stating that each
  slice brief must extend it.

## Deliverables

1. Lean driver(s) under `scripts/checks/compiler/` (e.g. `spec_diff.lean`, generator module(s)
   imported by it; if Lean modules must live in a lake lib to be imported, put the generator in a
   file the driver can import without adding a new root entry — check the root inventory rules,
   scripts/checks/root_inventory.txt — and say what you chose). Commands: `gen-spec SEED N DIR`,
   `gen-bir SEED N DIR TARGET` (one subdirectory per program, same files as `emit` writes, plus the
   program's Lean term `program.lean.txt` via `repr`), `replay SEED INDEX DIR TARGET`.
2. Gate `scripts/checks/compiler/spec_diff.sh`: runs both legs under a total time budget
   (`SPEC_DIFF_SECS`, default 600) — it generates and judges programs until the budget or `N`
   (default e.g. 300) is reached, whichever first, and requires a minimum number judged (fail if
   fewer, e.g. the machine was too slow: INCOMPLETE is never a pass, follow bounded_run's
   conventions). Prints counts per outcome kind (exit/trap) and per construct, and fails if a
   generated set has zero traps or zero exits in the BIR leg (vacuity guard).
3. Failing seeds become regression tests: on a mismatch the gate prints the seed, the index, the
   program's Lean term, and the exact manifest row and Examples entry to add; document the
   procedure in the gate header. Add a regression section to the manifest format if needed
   (program kind `bir:` already covers it: the program is added to `Concrete.BIR.Examples` under a
   `regression_<seed>_<index>` name). Also make `SPEC_DIFF_REPLAY=seed:index` re-run one program.
4. Planted-failure controls inside the gate, each required to fail FOR THE PLANTED REASON (grep the
   expected FAIL message; a control that fails for another reason is itself a failure):
   - a mutated compiler output: the emitter's `--mutate drop-overflow-llvm` (the LLVM leg gets a
     program whose checked ops are replaced by wrapping ones, or the printer drops the overflow
     branch) must produce a status/stderr mismatch on a generated trapping program;
   - `--mutate lcb-off-by-one` (the adapted LCB function returns its word + 1 on exits) must be
     caught by both the native LCB leg and the in-process evaluator leg;
   - the spec leg: a planted wrong oracle (e.g. expected exit + 1) fed to the judge must be
     rejected — this proves the spec-derived expectation path is load-bearing even while
     `compileCore` is unwired (use a hand-written BIR program for `Slice0.answer` as the stand-in
     compiled output: the control asserts that the judge compares against the SPEC's outcome);
   - determinism of the generator: the same seed twice gives byte-identical program terms; a
     different seed gives a different set.
   Mutations are emitter flags used only by controls; they must be impossible to enable from the
   normal gate path by accident (name them clearly, refuse them without an explicit control flag).
5. CI: add jobs "Spec differential (x86-64)" and "Spec differential (aarch64-macos)" to
   .github/workflows/lean_action_ci.yml mirroring the Tests jobs (same setup steps, a job-level
   timeout that bounds the run), and to required-checks.txt.
6. Docs: Concrete/README.md gates 5 and 6 — describe the gate as built, the two legs, the
   `compileCore` hook, and that the spec leg becomes required (non-zero compiled programs) in the
   branch that adds Core→BIR lowering (slice-1 work, no owner yet). A changelog.d fragment.

## Constraints

- No language/spec decisions; do not edit ConcreteSpec/ or the compiler (Concrete/) except to add
  mutation hooks if they cannot live in the driver (prefer the driver: mutate the BIR program or the
  emitted LCB `Source.Function` value / LLVM text in the driver before writing).
- Reuse the B runner's machinery; no second judge, no second example list.
- Run `bash -n`, shellcheck if present, `timeout 1800 lake build Concrete` if you touched Lean in
  the lib, and `timeout 600 lake env lean --run <driver> gen-bir 1 5 <tmp> x86_64-linux` and
  `gen-spec` smoke runs. Do not run the native gate.
- Final message: commits, what you ran with results, open questions.
