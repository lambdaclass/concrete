# C10 N2 audit, round 2

The read-only worktree is at `487085db1fcdf85a20c6310314170bfbcb44dc8f`, not the
round-7 revision named by the carry. Both it and the locally available round-7
commit `adc0656fc22c0cea0d74406778c59b9d1ff85c30` were audited. Round 7 was exported with `git archive` into a
throwaway directory; no fetch or worktree edit was needed.

Both runs use the union **ConcreteSpec.Spec.Frame + ConcreteSpec.Proofs.Frame**,
compiled once per dependency. Both finish **FAIL (exit 1), zero INCOMPLETE**.
The supplied build-slot wrapper held each focused source build. Checker commit
and exact-commit validation are recorded in [round-2 findings](N2-round2.md).

```sh
bash scripts/proofs/vacuity.sh --source /home/unbalancedparen/projects/gates-vacuity-c10 --scope ConcreteSpec.Spec.Frame --scope ConcreteSpec.Proofs.Frame
```

| Diagnostic class | Worktree 487085db1 | Round 7 adc0656fc |
| --- | ---: | ---: |
| `instance-application` | 4 | 1 |
| `instance-false-discharge` | 5 | 9 |
| `missing-instance` | 9 | 9 |
| `planted-proposition` | 0 | 1 |
| `unknown-name` | 3 | 9 |
| `unpaired-planted` | 12 | 16 |
| `unresolved-approval` | 54 | 53 |
| INCOMPLETE (all classes) | 0 | 0 |
| Total FAIL findings | 87 | 98 |
| Reported Obligations | 5 | 5 |

Complete reports, including every Helper and every declaration-level diagnostic:
[worktree report](audits/C10-487085db1.txt), [round-7 report](audits/C10-adc0656fc.txt).

The five Obligations are `ConcreteSpec.Core.Frame.ABI.Signature.WellFormed`,
`ConcreteSpec.Core.Frame.ABI.TransitionConforms`,
`ConcreteSpec.Core.Frame.ABI.ArtifactStackConforms`,
`ConcreteSpec.Core.Frame.ABI.RetentionConforms`, and
`ConcreteSpec.Core.Frame.ABI.LauncherConforms`. Earlier proof-only scope omitted all
five; that was a gate false negative. An Obligation-only scope now passes its
non-empty-population check and is covered by an explicit positive control.

## All 13 earlier round-7 unknown-name diagnostics

Reproduced with checker `6c76ac3c` against `adc0656fc`. **Four gate false positives,
nine real source diagnostics.** This supersedes the old audit's assertion that
unannotated Accepted examples needed declaration-name backticks.

| Declaration | Backtick contents | Classification |
| --- | --- | --- |
| `ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:500»` | `.ret` | Gate false positive: ordinary Accepted example has no N2 annotation. |
| `ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:500»` | `unitVal` | Gate false positive: ordinary Accepted example has no N2 annotation. |
| `ConcreteSpec.Core.Frame.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:482»` | `.ret` | Gate false positive: ordinary Accepted example has no N2 annotation. |
| `ConcreteSpec.Core.Frame.step_call_saves_caller` | `.ret` | Real: backticks contain a local, shorthand constructor, or expression rather than a declaration name. |
| `ConcreteSpec.Core.Frame.step_return_restores_caller` | `.ret` | Real: backticks contain a local, shorthand constructor, or expression rather than a declaration name. |
| `ConcreteSpec.Core.Frame.ABI.toyBodyAt_preserved` | `resultReg` | Real: backticks contain a local, shorthand constructor, or expression rather than a declaration name. |
| `ConcreteSpec.Core.Frame.caller_restoration` | `dst` | Real: backticks contain a local, shorthand constructor, or expression rather than a declaration name. |
| `ConcreteSpec.Core.Frame.c4Execution_wellFormed` | `Memory.WellFormed` | Gate false positive: resolves through `open ConcreteSpec.Core.Mem`. |
| `ConcreteSpec.Core.Frame.c1Call_restores_caller` | `.call` | Real: backticks contain a local, shorthand constructor, or expression rather than a declaration name. |
| `ConcreteSpec.Core.Frame.c1Call_restores_caller` | `.ret dst caller :: k` | Real: backticks contain a local, shorthand constructor, or expression rather than a declaration name. |
| `ConcreteSpec.Core.Frame.c1Call_restores_caller` | `dst` | Real: backticks contain a local, shorthand constructor, or expression rather than a declaration name. |
| `ConcreteSpec.Core.Frame.c1Call_restores_caller` | `caller` | Real: backticks contain a local, shorthand constructor, or expression rather than a declaration name. |
| `ConcreteSpec.Core.Frame.c1_steps_frame` | `entry` | Real: backticks contain a local, shorthand constructor, or expression rather than a declaration name. |

At the older worktree revision, the three diagnostics on unannotated examples
(`.ret`, `.ret`, `unitVal`) were also gate false positives and disappear. Its
unannotated Spec.Frame examples at lines 326 (`.ret`) and 780 (`kept`) are likewise
gate false positives and are no longer scanned; they were absent from the old
proof-only audit. Its remaining annotated-prose diagnostics (`.done`, `.ret`, `.ret`) are real under the
FINAL rule that backticked names on N2-tagged declarations resolve to declarations.

## Wrapper laws and retained source findings

**The wrapper-shape objection was a gate false positive.** `EffectResult` constrains
rejection by negating operation definedness and constrains success by definedness,
exact permissions and preservation. `ToyLaunchCorrect` requires insufficient
reservation on refusal and binds root, result and launcher conformance on entry.
`CheckedPairCorrect` requires absence of any well-formed handle on rejection and
binds endpoints, descriptors and result validity on success. These are semantic
contracts across outcomes; the FINAL convention does not require a literal
implication spelling. The checker now protects the subject operation before
exposing a conclusion's telescope, so computing a particular outcome cannot
mistake a branch proposition for a remaining case argument. Explicit and hidden
open case telescopes still fail. Positive and negative result-wrapper controls
check both argument preservation and a vacuous rejection branch's invalid plant.

**The toyBody membership discharge remains flagged, as requested:**

```text
FAIL instance-false-discharge ConcreteSpec.Core.Frame.ABI.«⟪example⟫ConcreteSpec/Proofs/Frame.lean:1270»
```

That Instance of `toyBody_stack_model` supplies the membership premise with
`by decide` at line 1272. Transitive proof-helper traversal reaches false
elimination in that hypothesis proof. Data/typeclass arguments are excluded;
the positive DecidableEq argument control passes. The eight other round-7
`instance-false-discharge` findings are separately listed in the complete report.
The gate does not impose a source-word ban on `decide`; it checks the elaborated
hypothesis proof.

The round-7 open Instance at line 1241 is `toyBody_stack_safe`: its `ToyTraceSafe`
conclusion still exposes the universally quantified sample and membership case.
Its linked Planteds are unpaired once that Instance is rejected. The other
missing-instance/unpaired-planted diagnostics likewise follow rejected evidence;
they do not assert that the Planteds will pass after the Instances are repaired.

The round-7 `planted-proposition` at line 546 is the caller-restoration plant.
The subject `Exec.bindDst` occurs transitively in the `C1Call` hypothesis and
`Exec.step` conclusion as well as the final locals equality. The planted statement
only changes the last occurrence: the generated proposition mutates every subject
dependency before substituting the Instance's exact arguments. The original
hypothesis and step cannot stand in for those mutated propositions.

**Not fixed here: source approvals and real source evidence findings.** The 54/53
PENDING approval references are unresolved source approvals. Real name, discharge,
open-case and planted-proposition findings belong to the spec writer. Neither
ConcreteSpec nor the C10 worktree was edited; no gate condition was weakened to
make either candidate pass. Migration remains owned by R-0219 in
[README.md](README.md), with one cumulative required CI job at promotion.
