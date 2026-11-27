import ConcreteSpec.Proofs.Checker
import ConcreteSpec.Proofs.Slice1
import ConcreteSpec.Admission.Statics

/-! Admission for the straight-line Core shared by slices 0 and 1. -/

namespace ConcreteSpec.Admission

/-- Keeps an evidence batch associated with the exact input of its traversal. -/
structure Batch (program : Program) where
  discharges : List Discharge
  issued : checkProgram program = .ok discharges

/-- Run admission and retain the equality that binds the evidence to this program. -/
def admitProgram (program : Program) : Except Refusal (Batch program) :=
  match h : checkProgram program with
  | .ok discharges => .ok ⟨discharges, h⟩
  | .error refusal => .error refusal

theorem admitProgram_iff (program : Program) : (∃ batch, admitProgram program = .ok batch) ↔ Checked program := by
  rw [← checkProgram_iff]
  unfold admitProgram
  split <;> simp_all

/-- Check an ordered subset of the issued evidence. Matching consumes the
issued occurrence and compares the site, type, operands and context together. -/
def Batch.validates {program : Program} (batch : Batch program) (proposed : List Discharge) : Bool :=
  proposed.isSublist batch.discharges

theorem Batch.validates_iff {program : Program} (batch : Batch program) (proposed : List Discharge) :
    batch.validates proposed = true ↔ proposed.Sublist batch.discharges :=
  List.isSublist_iff_sublist

private theorem bindPat_types (d : StructDecl) (facts : List (Name × Option Scalar))
    (pat : List (Name × Binder)) (ctx : Ctx) :
    (bindPat d facts pat ctx).tys = bindTypes d pat ctx.tys := by
  induction pat generalizing ctx with
  | nil => rfl
  | cons p rest ih =>
    obtain ⟨field, b⟩ := p
    cases b with
    | wild => exact ih ctx
    | name x =>
      cases h : Core.AList.lookup d.fields field with
      | some st =>
        let entry : Entry := ⟨.scalar st, true, false, fieldFact facts field, []⟩
        simpa only [bindPat, bindTypes, h, Ctx.tys, Core.AList.map, List.map_cons, entry] using
          ih ((x, entry) :: ctx)
      | none => simpa [bindPat, bindTypes, h] using ih ctx

theorem body_staticallyTyped {evidence : Ctx → Expr → Bool} {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {ending : Terminator}
      (ok : BodyOkWith evidence Δ Γ ss ending) : BodyTyped Δ Γ.tys ss ending := by
    induction ok with
    | ret ht => exact .ret ht
    | abort => exact .abort
    | cons hs _ ih =>
      cases hs with
      | let_ ht _ hu _ =>
        apply BodyTyped.letName ht
        have same := uses_tys hu
        unfold Ctx.tys Core.AList.map at same
        simpa only [Ctx.tys, Core.AList.map, List.map_cons, same] using ih
      | letWild ht _ _ hu => exact .letWild ht (uses_tys hu ▸ ih)
      | letStruct ht _ hd hp hu _ =>
        apply BodyTyped.letStruct ht hd hp
        simpa only [bindPat_types, uses_tys hu] using ih

theorem checked_staticallyTyped {program : Program} (h : Checked program) :
    StaticallyTyped program := ⟨h.1, body_staticallyTyped h.2⟩

theorem Batch.staticallyTyped {program : Program} (batch : Batch program) :
    StaticallyTyped program :=
  checked_staticallyTyped ((checkProgram_iff program).mp ⟨_, batch.issued⟩)

/-- Reach a statement by executing every preceding statement successfully. -/
inductive Reaches (Δ : Decls) : Env → List Stmt → Nat → Stmt → Env → Prop where
  | here {env s rest} : Reaches Δ env (s :: rest) 0 s env
  | next {env s env' rest n target before} : execStmt Δ env s = .ok env' → Reaches Δ env' rest n target before →
      Reaches Δ env (s :: rest) (n + 1) target before

private theorem stmtDischarge_shape {position : Nat} {Γ : Ctx} {s : Stmt} {d : Discharge}
    (h : d ∈ (stmtDischarge position Γ s).toList) :
    ∃ binder, s = .let_ binder (.scalar (.int d.type)) (.add d.left d.right) ∧
      d.position = position ∧ d.context = Γ := by
  simp only [Option.mem_toList] at h
  unfold stmtDischarge at h
  split at h
  · cases h
    exact ⟨_, rfl, rfl, rfl⟩
  · cases h

private theorem bodyDischarge_from {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {ending : Terminator}
    {start : Nat} {ds : List Discharge} (h : checkCoreBody Δ Γ start ss ending = .ok ds)
    {d : Discharge} (hd : d ∈ ds) : start ≤ d.position := by
  induction ss generalizing Γ start ds with
  | nil =>
    simp only [checkCoreBody, checkCoreBodyWith] at h
    split at h <;> simp_all
  | cons s rest ih =>
    simp only [checkCoreBody, checkCoreBodyWith, ArithmeticPolicy.discharge] at h
    split at h
    · cases h
    · rename_i next _
      split at h
      · cases h
      · rename_i tail ht
        cases h
        rcases List.mem_append.mp hd with hm | hm
        · obtain ⟨_, _, rfl, _⟩ := stmtDischarge_shape hm
          exact Nat.le_refl _
        · exact Nat.le_trans (Nat.le_succ _) (ih ht hm)

private theorem bodyDischarge_unique {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {ending : Terminator}
    {start : Nat} {ds : List Discharge} (h : checkCoreBody Δ Γ start ss ending = .ok ds) :
    (ds.map Discharge.position).Nodup := by
  induction ss generalizing Γ start ds with
  | nil =>
    simp only [checkCoreBody, checkCoreBodyWith] at h
    split at h <;> simp_all
  | cons s rest ih =>
    simp only [checkCoreBody, checkCoreBodyWith, ArithmeticPolicy.discharge] at h
    split at h
    · cases h
    · split at h
      · cases h
      · rename_i tail ht
        cases h
        cases hs : stmtDischarge start Γ s with
        | none => simpa [hs] using ih ht
        | some d =>
          obtain ⟨_, _, hp, _⟩ :=
            stmtDischarge_shape (position := start) (Γ := Γ) (s := s) (d := d) (by simp [hs])
          simp only [Option.toList, List.cons_append, List.nil_append, List.map_cons,
            List.nodup_cons]
          refine ⟨?_, ih ht⟩
          rintro hm
          obtain ⟨other, hother, eq⟩ := List.mem_map.mp hm
          have hlt := bodyDischarge_from ht hother
          omega

theorem Batch.uniquePositions {program : Program} (batch : Batch program) :
    (batch.discharges.map Discharge.position).Nodup := by
  have h := batch.issued
  unfold checkProgram checkProgramWith at h
  split at h
  · exact bodyDischarge_unique h
  · cases h

theorem Batch.validatedUnique {program : Program} {batch : Batch program} {proposed : List Discharge}
    (h : batch.validates proposed = true) : (proposed.map Discharge.position).Nodup :=
  ((batch.validates_iff proposed).mp h |>.map Discharge.position).nodup batch.uniquePositions

private theorem stmtDischarge_sound {Δ : Decls} {Γ Γ' : Ctx} {s : Stmt} {d : Discharge}
    {position : Nat} {env : Env} (hc : Corr Δ Γ env) (hs : StmtOk Δ Γ s Γ')
    (hm : d ∈ (stmtDischarge position Γ s).toList) :
    (∃ binder, s = .let_ binder (.scalar (.int d.type)) (.add d.left d.right)) ∧
    Corr Δ d.context env ∧
    AtomTy d.context.tys d.left (.int d.type) ∧ AtomTy d.context.tys d.right (.int d.type) ∧
    ∃ next, execStmt Δ env s = .ok next := by
  obtain ⟨binder, rfl, _, rfl⟩ := stmtDischarge_shape hm
  refine ⟨⟨binder, rfl⟩, hc, ?_⟩
  have ht : HasType Δ d.context.tys (.add d.left d.right) (.scalar (.int d.type)) := by
    cases hs <;> assumption
  have operands : AtomTy d.context.tys d.left (.int d.type) ∧
      AtomTy d.context.tys d.right (.int d.type) := by
    cases ht with
    | add hl hr => exact ⟨hl, hr⟩
  refine ⟨operands.1, operands.2, ?_⟩
  rcases execStmt_safe hc hs with ⟨k, hk⟩ | ⟨next, hn, _⟩
  · exact False.elim (execStmt_ne_trap hc hs k hk)
  · exact ⟨next, hn⟩

private theorem bodyDischarge_reachable {Δ : Decls} {ending : Terminator}
    {ss : List Stmt} {env before : Env} {n : Nat} {s : Stmt}
    (reach : Reaches Δ env ss n s before) {Γ : Ctx} (hc : Corr Δ Γ env)
    {start : Nat} {ds : List Discharge} (h : checkCoreBody Δ Γ start ss ending = .ok ds)
    {d : Discharge} (hm : d ∈ ds) (atSite : d.position = start + n) :
    (∃ binder, s = .let_ binder (.scalar (.int d.type)) (.add d.left d.right)) ∧
    Corr Δ d.context before ∧
    AtomTy d.context.tys d.left (.int d.type) ∧ AtomTy d.context.tys d.right (.int d.type) ∧
    ∃ next, execStmt Δ before s = .ok next := by
  induction reach generalizing Γ start ds with
  | here =>
    simp only [checkCoreBody, checkCoreBodyWith, ArithmeticPolicy.discharge] at h
    split at h
    · cases h
    · rename_i next hs
      split at h
      · cases h
      · rename_i tail ht
        cases h
        rcases List.mem_append.mp hm with head | rest
        · exact stmtDischarge_sound hc (checkStmt_iff.mp hs) head
        · have hlt := bodyDischarge_from ht rest
          omega
  | next step _ ih =>
    simp only [checkCoreBody, checkCoreBodyWith, ArithmeticPolicy.discharge] at h
    split at h
    · cases h
    · rename_i next hs
      split at h
      · cases h
      · rename_i tail ht
        cases h
        rcases List.mem_append.mp hm with head | rest
        · obtain ⟨_, _, hp, _⟩ := stmtDischarge_shape head
          omega
        · have hok := checkStmt_iff.mp hs
          rcases execStmt_safe hc hok with ⟨k, hk⟩ | ⟨env', he, hc'⟩
          · rw [step] at hk
            cases hk
          · rw [step] at he
            cases he
            exact ih hc' ht rest (by omega)

/-- At every reachable matching position, a discharge describes the exact
ordered operands and their type, its context holds, and the step succeeds. -/
theorem Batch.dischargeSafety {program : Program} (batch : Batch program) {d : Discharge}
    (member : d ∈ batch.discharges) {s : Stmt} {before : Env}
    (reached : Reaches program.decls [] program.body d.position s before) :
    (∃ binder, s = .let_ binder (.scalar (.int d.type)) (.add d.left d.right)) ∧
    Corr program.decls d.context before ∧
    AtomTy d.context.tys d.left (.int d.type) ∧ AtomTy d.context.tys d.right (.int d.type) ∧
    ∃ next, execStmt program.decls before s = .ok next := by
  have h := batch.issued
  unfold checkProgram checkProgramWith at h
  split at h
  · exact bodyDischarge_reachable reached .nil h member (by simp)
  · cases h

theorem Batch.validatedMember {program : Program} (batch : Batch program) {proposed : List Discharge}
    (h : batch.validates proposed = true) {d : Discharge} (member : d ∈ proposed) :
    d ∈ batch.discharges :=
  ((batch.validates_iff proposed).mp h).subset member

end ConcreteSpec.Admission
