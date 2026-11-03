### Preserve useful records before branch retirement (2026-11-03)

The branch retirement deletes 36 merged branches. It archives the 244 unmerged branches on
origin under `refs/archive/branches/*`, and in
`~agent/archive/concrete2-unmerged-branches-2026-11-03.bundle` on the dev server, then deletes
their branch heads. This rescue commits records and fixtures only; it does not perform remote
ref changes. The local `refs/remotes/origin/*` snapshot still exposes all 244 unmerged tips.

Rescued records, all `compiler: legacy`, status open, against `legacy-compiler-final`
(`066c9d9b8`), with their reproducer files and discovery attribution:

| Bug | Source branch (under `refs/archive/branches/`) |
|---|---|
| 264 | `wip/fix/work-queue-lifecycle-gate` (identical record on `wip/fuzz-farm-landing-candidate`) |
| 310 | `wip/bug-310` (identical record on `wip/stmt7-generic-main` and descendants) |
| 312 | `wip/bug-312` (fuller than `wip/syn3-destructure`; identical on `wip/bug-313`) |
| 313 | `wip/bug-313` (fuller than `wip/member-collisions`) |
| 314 | `wip/bug-314` |
| 317 | `wip/bug-317` |
| 334 | `wip/bug-ledger-emitted-files` |
| 335 | `wip/bug-report-layout-panic` |
| 336 | `wip/bug-destructure-mut` |
| 337 | `wip/bug-unknown-capability-name` |
| 338 | `wip/bug-borrow-mut-non-mut` |
| 339 | `wip/bug-bool-order-interp` |

The branch records were compared by blob across all remote-tracking tips. The fuller 312 and
313 variants retain structural-owner and scope explanations. Reproducer files are the union
of the source branches' bug-numbered fixtures. Their expected outcomes describe the intended
behaviour, not a required passing test of the frozen compiler. The records use main's open-bug
`coverage:` disposition, not `fixtures:` registration; no passing-test calls are imported.
Bug 264 is a historical fake-agent gate defect, with its command reproduction retained; no
work-queue gate change is imported. Bug 313 remains a MOD spec gap: the proposed MOD-5 rule and
its enforcing code never landed, and its extra duplicate-member fixtures do not decide owner
questions. No compiler, Main.lean or std implementation changes are rescued.

Preserved research records:

- `research/compiler/lean-backend-integration.md` and the two factorial measurement JSON files
  under `research/compiler/measurements/`, from `feat/lean-backend-integration`. Main's
  `lean-backend-architecture.md` is an architectural proposal, not the experiment or its raw
  measurements; these records remain useful historical evidence, not a current integration.
- `research/process/verification-docs-consolidation-2026-10.md`, from
  `wip/cleanup-docs-verification-plan`, whose reviewed inventory and corrections are fuller than
  `wip/snapshot/cleanup-docs-verification-plan-auto`. Main still has the verification document
  inventory it discusses; the later new-compiler roadmap supersedes its scheduling, not its
  historical measurements. The archive status header schedules nothing. JSON measurements carry
  that status in `archive_status` to preserve valid JSON.

Number collisions are reported, not overwritten or silently renumbered:

- 056: `docs/bugs/056_user_option_redefinition_shadowed.md` on
  `refs/archive/branches/wip/archive/worktree-h18-drop-glue` concerns builtin Option/Result
  shadowing. Main's 056 concerns fn-pointer reassignment and invalid SSA.
- 064: `docs/bugs/064_aliased_imported_type_unresolvable.md` concerns imported type aliases;
  main's 064 concerns contradictory `--query` diagnostics.
- 065: `docs/bugs/065_builtin_enum_list_built_twice.md` concerns duplicated builtin-enum
  construction; main's 065 concerns stack-depth unboundedness. Both 064 and 065 are preserved
  on `refs/archive/branches/codex/evidence-architecture-docs`,
  `refs/archive/branches/vcgen/calculus`, and the three
  `refs/archive/branches/wip/archive/docs/r0448-{roadmap-only,spike-disposition,spike-integrated}`
  refs. The scan of other record-title variants found no additional distinct-number collision.

Archive-only material includes the class-13 probes and audit results on
`refs/archive/branches/wip/class13-audit`; the Lean-backend integration implementation,
helpers, examples and benchmark harness on `refs/archive/branches/feat/lean-backend-integration`;
old-compiler fix/containment attempts on the bug source refs above (and
`refs/archive/branches/wip/stmt7-generic-main`, `refs/archive/branches/wip/syn3-destructure`,
`refs/archive/branches/wip/member-collisions`); and old changelog fragments of unlanded work.
The archive preserves those attempts without changing the frozen compiler or scheduling repairs.
