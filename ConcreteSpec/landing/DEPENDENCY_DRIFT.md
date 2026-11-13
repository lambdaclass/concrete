# Round 16 dependency drift classification

The manifest described Lean 4.34.1, while this branch builds with Lean 4.35.0
(toolchain change 3a603156547d7d57de69d0b73eb3694cd0edd94c).
Fresh independent censuses used the same exporter and immutable source snapshot
`aa44140a33747fa37ece59c76090972e564c65b4`. Every recorded census field and the
whole declaration population reproduced exactly under 4.34.1. Under 4.35.0 there
are 35 changed dependency lists, 387 changed term digests, and one generated
helper replacement. No source text changed between these censuses.

These are toolchain changes, not source identifier aliases or evidence that a
held path rule landed. Kernel instance names are real edges. The historical
SourcePath/Span declarations remain held in the immutable source inventory;
current main deliberately excludes them (ISS-0068). No spec policy is changed here.
The existing authority conflicts in main remain tracked in ISS-0064/0065,
ISS-0068/0069/0070/0071; this comparison found no new main dependency on held paths.

The fix updates only classified census fields, the generated helper at its same
Lean-observed command, and the resulting contract membership digest. Assignments,
prerequisites, holds and regeneration commands are preserved. Generated relocation
and law/control views are regenerated through their producers. Each changed entry
names the toolchain reason. [toolchain-drift.json](toolchain-drift.json) records
**every** changed entry and its before/after fields, including all term-only changes.

| Entry | Cause | Fix (added / removed kernel names) |
|---|---|---|
| `ConcreteSpec.Borrow.Evidence.PermissiveBorrow.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Borrow.Evidence.PermissiveBorrow.rec` |
| `ConcreteSpec.Borrow.Evidence.PermissiveBorrow.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Borrow.Evidence.PermissiveBorrow.casesOn` / − `ConcreteSpec.Borrow.Evidence.PermissiveBorrow.rec` |
| `ConcreteSpec.Brand.Evidence.PermissiveOk.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Brand.Evidence.PermissiveOk.rec` |
| `ConcreteSpec.Brand.Evidence.PermissiveOk.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Brand.Evidence.PermissiveOk.casesOn` / − `ConcreteSpec.Brand.Evidence.PermissiveOk.rec` |
| `ConcreteSpec.Brand.Evidence.PermissiveRun.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Brand.Evidence.PermissiveRun.rec` |
| `ConcreteSpec.Brand.Evidence.PermissiveRun.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Brand.Evidence.PermissiveRun.casesOn` / − `ConcreteSpec.Brand.Evidence.PermissiveRun.rec` |
| `ConcreteSpec.Core.Exec.CleanupEvidence.NoDispatchStep.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Core.Exec.CleanupEvidence.NoDispatchStep.rec` |
| `ConcreteSpec.Core.Exec.CleanupEvidence.NoDispatchStep.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Core.Exec.CleanupEvidence.NoDispatchStep.casesOn` / − `ConcreteSpec.Core.Exec.CleanupEvidence.NoDispatchStep.rec` |
| `ConcreteSpec.Core.Exec.CoreEvidence.arbitraryStep.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Core.Exec.CoreEvidence.arbitraryStep.rec` |
| `ConcreteSpec.Core.Exec.CoreEvidence.arbitraryStep.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Core.Exec.CoreEvidence.arbitraryStep.casesOn` / − `ConcreteSpec.Core.Exec.CoreEvidence.arbitraryStep.rec` |
| `ConcreteSpec.Core.Frame.ABI.Launcher.Run.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Core.Frame.ABI.Launcher.Run.rec` |
| `ConcreteSpec.Core.Frame.ABI.Launcher.Run.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Core.Frame.ABI.Launcher.Run.casesOn` / − `ConcreteSpec.Core.Frame.ABI.Launcher.Run.rec` |
| `ConcreteSpec.Core.Frame.ABI.Signature.Call.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Core.Frame.ABI.Signature.Call.rec` |
| `ConcreteSpec.Core.Frame.ABI.Signature.Call.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Core.Frame.ABI.Signature.Call.casesOn` / − `ConcreteSpec.Core.Frame.ABI.Signature.Call.rec` |
| `ConcreteSpec.Core.Hybrid.State.WellFormed.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Core.Hybrid.State.WellFormed.rec` |
| `ConcreteSpec.Core.Hybrid.State.WellFormed.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Core.Hybrid.State.WellFormed.casesOn` / − `ConcreteSpec.Core.Hybrid.State.WellFormed.rec` |
| `ConcreteSpec.Core.Mem.Memory.WellFormed.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Core.Mem.Memory.WellFormed.rec` |
| `ConcreteSpec.Core.Mem.Memory.WellFormed.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Core.Mem.Memory.WellFormed.casesOn` / − `ConcreteSpec.Core.Mem.Memory.WellFormed.rec` |
| `ConcreteSpec.Core.Observe.Environment.Valid.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Core.Observe.Environment.Valid.rec` |
| `ConcreteSpec.Core.Observe.Environment.Valid.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Core.Observe.Environment.Valid.casesOn` / − `ConcreteSpec.Core.Observe.Environment.Valid.rec` |
| `ConcreteSpec.Core.Observe.ExecAdapter.WellFormed.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Core.Observe.ExecAdapter.WellFormed.rec` |
| `ConcreteSpec.Core.Observe.ExecAdapter.WellFormed.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Core.Observe.ExecAdapter.WellFormed.casesOn` / − `ConcreteSpec.Core.Observe.ExecAdapter.WellFormed.rec` |
| `ConcreteSpec.Core.instDecidableReferencesEarlier._f` | Lean 4.35 recursive-instance elaboration emits _proof_1 and its direct body dependencies instead of _aux_1 | + `ConcreteSpec.Core.CExpr.refs, ConcreteSpec.Core.ConstDecl.init, ConcreteSpec.Core.instDecidableEqConstId, ConcreteSpec.Core.instDecidableReferencesEarlier._proof_1` / − `ConcreteSpec.Core.instDecidableReferencesEarlier._aux_1` |
| `ConcreteSpec.Core.instDecidableReferencesEarlier._sunfold` | Lean 4.35 recursive-instance elaboration emits _proof_1 and its direct body dependencies instead of _aux_1 | + `ConcreteSpec.Core.CExpr.refs, ConcreteSpec.Core.ConstDecl.init, ConcreteSpec.Core.instDecidableEqConstId, ConcreteSpec.Core.instDecidableReferencesEarlier._proof_1` / − `ConcreteSpec.Core.instDecidableReferencesEarlier._aux_1` |
| `ConcreteSpec.Core.instDecidableReferencesEarlier._unsafe_rec` | Lean 4.35 recursive-instance elaboration emits _proof_1 and its direct body dependencies instead of _aux_1 | + `ConcreteSpec.Core.CExpr.refs, ConcreteSpec.Core.ConstDecl.init, ConcreteSpec.Core.instDecidableEqConstId, ConcreteSpec.Core.instDecidableReferencesEarlier._proof_1` / − `ConcreteSpec.Core.instDecidableReferencesEarlier._aux_1` |
| `ConcreteSpec.Evidence.AnyExec.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.Evidence.AnyExec.rec` |
| `ConcreteSpec.Evidence.AnyExec.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.Evidence.AnyExec.casesOn` / − `ConcreteSpec.Evidence.AnyExec.rec` |
| `ConcreteSpec.FieldsTy.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.FieldsTy.rec` |
| `ConcreteSpec.FieldsTy.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.FieldsTy.casesOn` / − `ConcreteSpec.FieldsTy.rec` |
| `ConcreteSpec.PatOk.casesOn` | Lean 4.35 mkCasesOnViaProjs builds this eliminator with projections instead of rec | + `(none)` / − `ConcreteSpec.PatOk.rec` |
| `ConcreteSpec.PatOk.recOn` | Lean 4.35 mkRecOnViaCasesOn delegates this eliminator to casesOn | + `ConcreteSpec.PatOk.casesOn` / − `ConcreteSpec.PatOk.rec` |
| `ConcreteSpec/Proofs/Observe2.lean:601:608` | Lean 4.35 decide elaboration retains the instance/proof dependency rather than the old reduced proof | + `ConcreteSpec.Core.instDecidableComponentOk` / − `ConcreteSpec.Core.SourcePath.Control, ConcreteSpec.Core.SourcePath.DrivePrefix` |
| `ConcreteSpec/Proofs/Observe2.lean:632:642` | Lean 4.35 decide elaboration retains the instance/proof dependency rather than the old reduced proof | + `ConcreteSpec.Core.instDecidableComponentOk` / − `ConcreteSpec.Core.SourcePath.Control, ConcreteSpec.Core.SourcePath.DrivePrefix` |
| `ConcreteSpec/Proofs/Observe2.lean:710:720` | Lean 4.35 decide elaboration retains the instance/proof dependency rather than the old reduced proof | + `ConcreteSpec.Core.instDecidableComponentOk` / − `ConcreteSpec.Core.SourcePath.Control, ConcreteSpec.Core.SourcePath.DrivePrefix` |
| `ConcreteSpec/Proofs/Prim.lean:675:687` | Lean 4.35 decide elaboration retains the instance/proof dependency rather than the old reduced proof | + `ConcreteSpec.Core.SafetyEvidence.context, ConcreteSpec.Core.instDecidableEqObligationId` / − `ConcreteSpec.Core.ObligationKind.safety, ConcreteSpec.Core.SiteId.mk, ConcreteSpec.Core.instDecidableEqObligationId.decEq._proof_1` |

The additional `_aux_1` → `_proof_1` generated-helper replacement stays at
`ConcreteSpec/Spec/Const.lean:571:578`, held in F06 for independent constant
authority. Its emission is verified from the new census; the old generated name
is removed from contract membership. This is not a source declaration rename.

Root checks now assert that the full inventory remains an unconditional step in
required Spec CI for every main push, PR and dispatched train. Controls refuse
step deletion, conditional execution, allowed failure, removal from required
checks and path filtering. The manifest records Lean's exact commit, so a future
upgrade must explicitly reconcile all census fields. Dependency diagnostics list
all mismatching entries together with added/removed names and cause-class hints;
those hints never authorize a manifest rewrite.
