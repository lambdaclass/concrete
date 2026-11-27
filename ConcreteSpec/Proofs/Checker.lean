import ConcreteSpec.Admission.Types
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Proofs.Basics
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Slice1Checker
import ConcreteSpec.Spec.Slice1Ownership
import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Spec.Statics

/-!
# The checker agrees with the judgments (`check_iff`), so every `check p = true` example in the specification is a statement about the judgment `Checked p`.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec
open Core (AList)

private theorem bind_ok_iff {ε α β : Type} {r : Except ε α} {f : α → Except ε β} {b : β} :
    (r >>= f) = .ok b ↔ ∃ a, r = .ok a ∧ f a = .ok b := by
  cases r <;> simp [Bind.bind, Except.bind]

private theorem bind_unit_ok_iff {ε α : Type} {r : Except ε Unit} {s : Except ε α} {a : α} :
    (r >>= fun _ => s) = .ok a ↔ r = .ok () ∧ s = .ok a := by
  cases r with
  | error e => simp [Bind.bind, Except.bind]
  | ok u => cases u; simp [Bind.bind, Except.bind]

private theorem firstRepeated_eq_none {α : Type} [DecidableEq α] (xs : List α) :
    firstRepeated? xs = none ↔ xs.Nodup := by
  suffices h : ∀ seen, firstRepeated? xs seen = none ↔ xs.Nodup ∧ ∀ x ∈ xs, x ∉ seen by
    simpa using h []
  induction xs with
  | nil => simp [firstRepeated?]
  | cons x rest ih =>
    intro seen
    by_cases h : x ∈ seen <;> simp [firstRepeated?, h, List.nodup_cons, ih] <;> grind

private theorem rejectFound_ok_iff {α ε : Type} (found : Option α) (reason : α → ε) :
    rejectFound found reason = .ok () ↔ found = none := by
  cases found <;> simp [rejectFound]

theorem inferAtomType_iff {Δ : Decls} {Γ : TyCtx} {a : Atom} {ty : Ty} :
    inferAtomType Γ a = .ok ty ↔ HasType Δ Γ (.atom a) ty := by
  constructor
  · intro h
    cases a with
    | int t n =>
      simp only [inferAtomType] at h
      split at h
      · cases h; exact .int (by assumption)
      · cases h
    | bool b => cases h; exact .bool
    | copy x =>
      simp only [inferAtomType] at h
      split at h
      · cases h; exact .copy (by assumption)
      · cases h
    | move x =>
      simp only [inferAtomType] at h
      split at h
      · cases h; exact .move (by assumption)
      · cases h
  · intro h
    cases h <;> simp_all [inferAtomType]

theorem atomTy_iff {Γ : TyCtx} {a : Atom} {st : STy} :
    inferAtomType Γ a = .ok (.scalar st) ↔ AtomTy Γ a st := by
  rw [inferAtomType_iff (Δ := [])]
  exact ⟨fun h => by cases h <;> constructor <;> assumption,
    fun h => by cases h <;> constructor <;> assumption⟩

private theorem inferAddOperand_iff {Γ : TyCtx} {a : Atom} {t : IntTy} :
    inferAddOperand Γ a = .ok t ↔ AtomTy Γ a (.int t) := by
  rw [← atomTy_iff]
  constructor
  · intro h
    unfold inferAddOperand at h
    split at h
    · cases h; assumption
    · cases h
    · cases h
  · intro h
    simp [inferAddOperand, h]

theorem covers_iff {α β : Type} {l : List (Name × α)} {m : List (Name × β)} :
    (∀ f a, AList.lookup l f = some a → ∃ b, AList.lookup m f = some b) ↔
      l.all (fun q => (AList.lookup m q.1).isSome) = true := by
  rw [List.all_eq_true]
  constructor
  · intro h q hq
    obtain ⟨a, ha⟩ := AList.lookup_isSome_of_mem hq
    obtain ⟨b, hb⟩ := h q.1 a ha
    simp [hb]
  · intro h f a ha
    have := h (f, a) (AList.lookup_mem ha)
    simp only [Option.isSome_iff_exists] at this
    exact this

private theorem checkFieldTypes_iff {Γ : TyCtx} {d : StructDecl} {fs : List (Name × Atom)} :
    checkFieldTypes Γ d fs = .ok () ↔
      ∀ p ∈ fs, ∃ st, AList.lookup d.fields p.1 = some st ∧ AtomTy Γ p.2 st := by
  induction fs with
  | nil => simp [checkFieldTypes]
  | cons p rest ih =>
    obtain ⟨field, atom⟩ := p
    cases hf : AList.lookup d.fields field with
    | none => simp [checkFieldTypes, hf]
    | some st =>
      cases ha : inferAtomType Γ atom with
      | error cause =>
        simp [checkFieldTypes, hf, ha, Except.mapError, Bind.bind, Except.bind,
          ← atomTy_iff]
      | ok ty =>
        by_cases ht : ty = .scalar st <;>
          simp [checkFieldTypes, hf, ha, ht, Except.mapError, Bind.bind, Except.bind,
            ← atomTy_iff, ih]

private theorem findMissing_eq_none {α β : Type} {l : List (Name × α)} {m : List (Name × β)} :
    l.find? (fun q => (AList.lookup m q.1).isNone) = none ↔
      (∀ f a, AList.lookup l f = some a → ∃ b, AList.lookup m f = some b) := by
  rw [covers_iff, List.all_eq_true, List.find?_eq_none]
  simp [Option.isSome_iff_ne_none]

private theorem checkFields_iff {Γ : TyCtx} {d : StructDecl} {fs : List (Name × Atom)} :
    checkFields Γ d fs = .ok () ↔ FieldsTy Γ d fs := by
  simp only [checkFields, bind_unit_ok_iff, rejectFound_ok_iff,
    firstRepeated_eq_none, checkFieldTypes_iff, findMissing_eq_none]
  exact ⟨fun ⟨hn, ht, hc⟩ => ⟨hn, ht, hc⟩, fun h => ⟨h.nodup, h.typed, h.covers⟩⟩

theorem inferExprType_iff {Δ : Decls} {Γ : TyCtx} {e : Expr} {ty : Ty} :
    inferExprType Δ Γ e = .ok ty ↔ HasType Δ Γ e ty := by
  constructor
  · intro h
    cases e with
    | atom a =>
      apply inferAtomType_iff.mp
      cases ha : inferAtomType Γ a with
      | ok actual => simpa [inferExprType, ha, Except.mapError] using h
      | error cause => simp [inferExprType, ha, Except.mapError] at h
    | add l r =>
      obtain ⟨lt, hl, hr⟩ := bind_ok_iff.mp h
      obtain ⟨rt, hr, heq⟩ := bind_ok_iff.mp hr
      split at heq
      · cases heq
        subst rt
        exact .add (inferAddOperand_iff.mp hl) (inferAddOperand_iff.mp hr)
      · cases heq
    | mk s fs =>
      cases hd : findDecl Δ s with
      | none => simp [inferExprType, hd] at h
      | some d =>
        simp only [inferExprType, hd] at h
        obtain ⟨⟨⟩, hf, heq⟩ := bind_ok_iff.mp h
        change Except.ok (.struct s) = .ok ty at heq
        cases heq
        exact .mk hd (checkFields_iff.mp hf)
  · intro h
    cases h with
    | int h => simp [inferExprType, inferAtomType, h, Except.mapError]
    | bool => rfl
    | copy h => simp [inferExprType, inferAtomType, h, Except.mapError]
    | move h => simp [inferExprType, inferAtomType, h, Except.mapError]
    | add hl hr =>
      simp [inferExprType, inferAddOperand_iff.mpr hl, inferAddOperand_iff.mpr hr, Bind.bind, Except.bind]
    | mk hd hf =>
      simp [inferExprType, hd, checkFields_iff.mpr hf, Bind.bind, Except.bind]

private theorem requireExprType_iff {Δ : Decls} {Γ : TyCtx} {e : Expr} {ty : Ty} :
    requireExprType Δ Γ ty e = .ok () ↔ HasType Δ Γ e ty := by
  rw [← inferExprType_iff]
  constructor
  · intro h
    obtain ⟨actual, ha, heq⟩ := bind_ok_iff.mp h
    by_cases ht : actual = ty
    · simpa [ht] using ha
    · simp [ht] at heq
  · intro h
    simp [requireExprType, h, Bind.bind, Except.bind]

theorem useAtom_iff {Δ : Decls} {Γ Γ' : Ctx} {a : Atom} :
    useAtom Δ Γ a = .ok Γ' ↔ UseAtom Δ Γ a Γ' := by
  constructor
  · intro h
    cases a with
    | int => cases h; exact .int
    | bool => cases h; exact .bool
    | copy x =>
      simp only [useAtom] at h
      split at h
      · rename_i e he
        split at h
        · rename_i live
          split at h
          · cases h; exact .copy he live (by assumption)
          · cases h
        · cases h
      · cases h
    | move x =>
      simp only [useAtom] at h
      split at h
      · rename_i e he
        split at h
        · rename_i live
          split at h
          · cases h
          · cases h
            exact .move he live (Bool.eq_false_iff.mpr (by assumption))
        · cases h
      · cases h
  · intro h
    cases h with
    | int => rfl
    | bool => rfl
    | copy hf hl hcp => simp [useAtom, hf, hl, hcp]
    | move hf hl hcp => simp [useAtom, hf, hl, hcp]

theorem readAtoms_iff {Δ : Decls} :
    ∀ {Γ Γ' : Ctx} {as : List Atom}, readAtoms Δ Γ as = .ok Γ' ↔ UseAtoms Δ Γ as Γ'
  | Γ, Γ', [] => by
    simp only [readAtoms, Except.ok.injEq]
    constructor
    · rintro rfl; exact .nil
    · intro h; cases h; rfl
  | Γ, Γ', a :: as => by
    constructor
    · intro h
      simp only [readAtoms] at h
      split at h
      · exact .cons (useAtom_iff.mp (by assumption)) (readAtoms_iff.mp h)
      · cases h
    · intro h
      cases h with
      | cons h₁ h₂ => simpa [readAtoms, useAtom_iff.mpr h₁] using readAtoms_iff.mpr h₂

theorem reads_iff {Δ : Decls} {Γ Γ' : Ctx} {e : Expr} :
    readAtoms Δ Γ e.atoms = .ok Γ' ↔ Uses Δ Γ e Γ' := by
  rw [readAtoms_iff]
  constructor
  · intro h
    cases e with
    | atom a => cases h with | cons ha hs => cases hs; exact .atom ha
    | add l r =>
      cases h with
      | cons hl hs => cases hs with | cons hr ht => cases ht; exact .add hl hr
    | mk s fs => exact .mk h
  · intro h
    cases h with
    | atom ha => exact .cons ha .nil
    | add hl hr => exact .cons hl (.cons hr .nil)
    | mk hf => exact hf

theorem checkPattern_iff {d : StructDecl} {pat : List (Name × Binder)} :
    checkPattern d pat = .ok () ↔ PatOk d pat := by
  have declared : pat.find? (fun p => (AList.lookup d.fields p.1).isNone) = none ↔
      (∀ p ∈ pat, ∃ st, AList.lookup d.fields p.1 = some st) := by
    rw [List.find?_eq_none]
    simp only [Option.isNone_iff_eq_none, Option.ne_none_iff_exists]
    constructor <;> intro h p hp <;> obtain ⟨v, hv⟩ := h p hp <;> exact ⟨v, hv.symm⟩
  simp only [checkPattern, bind_unit_ok_iff, rejectFound_ok_iff,
    firstRepeated_eq_none, declared, findMissing_eq_none]
  exact ⟨fun ⟨hn, hd, hc⟩ => ⟨hn, hd, hc⟩, fun h => ⟨h.nodup, h.declared, h.covers⟩⟩

theorem checkPatternBinders_iff {Γ : Ctx} {pat : List (Name × Binder)} :
    checkPatternBinders Γ pat = .ok () ↔ PatFresh Γ pat := by
  simp [checkPatternBinders, bind_unit_ok_iff, rejectFound_ok_iff, firstRepeated_eq_none,
    List.find?_eq_none, PatFresh, Fresh]

private theorem checkStmtExpr_iff {evidence : Ctx → Expr → Bool} {Δ : Decls} {Γ Γ' : Ctx} {t : Ty} {e : Expr} :
    checkStmtExprWith evidence Δ Γ t e = .ok Γ' ↔
      HasType Δ Γ.tys e t ∧ Uses Δ Γ e Γ' ∧ evidence Γ e = true := by
  constructor
  · intro h
    obtain ⟨⟨⟩, ht, h⟩ := bind_ok_iff.mp h
    obtain ⟨next, hu, h⟩ := bind_ok_iff.mp h
    split at h
    · cases h
      exact ⟨requireExprType_iff.mp ht, reads_iff.mp hu, by assumption⟩
    · cases h
  · rintro ⟨ht, hu, ha⟩
    simp [checkStmtExprWith, requireExprType_iff.mpr ht, reads_iff.mpr hu, ha, Bind.bind, Except.bind]

theorem checkStmtWith_iff {evidence : Ctx → Expr → Bool} {Δ : Decls} {Γ Γ' : Ctx} {s : Stmt} :
    checkStmtWith evidence Δ Γ s = .ok Γ' ↔ StmtOkWith evidence Δ Γ s Γ' := by
  constructor
  · intro h
    cases s with
    | let_ b t e =>
      cases b with
      | wild =>
        obtain ⟨next, he, h⟩ := bind_ok_iff.mp h
        obtain ⟨ht, hu, ha⟩ := checkStmtExpr_iff.mp he
        split at h
        · cases h
          exact .letWild ht ha (by assumption) hu
        · cases h
      | name x =>
        obtain ⟨next, he, h⟩ := bind_ok_iff.mp h
        obtain ⟨ht, hu, ha⟩ := checkStmtExpr_iff.mp he
        split at h
        · cases h
          exact .let_ ht ha hu (by assumption)
        · cases h
    | letStruct s pat e =>
      obtain ⟨next, he, h⟩ := bind_ok_iff.mp h
      obtain ⟨ht, hu, ha⟩ := checkStmtExpr_iff.mp he
      split at h
      · rename_i d hd
        obtain ⟨⟨⟩, hp, h⟩ := bind_ok_iff.mp h
        obtain ⟨⟨⟩, hf, h⟩ := bind_ok_iff.mp h
        cases h
        exact .letStruct ht ha hd (checkPattern_iff.mp hp) hu (checkPatternBinders_iff.mp hf)
      · cases h
  · intro h
    cases h with
    | let_ ht hk hu hf =>
      simp [checkStmtWith, checkStmtExpr_iff.mpr ⟨ht, hu, hk⟩, hf, Bind.bind, Except.bind]
    | letWild ht hk hcopy hu =>
      unfold WildcardPermitted at hcopy
      simp [checkStmtWith, checkStmtExpr_iff.mpr ⟨ht, hu, hk⟩, hcopy, Bind.bind, Except.bind]
    | letStruct ht hk hd hp hu hf =>
      simp [checkStmtWith, checkStmtExpr_iff.mpr ⟨ht, hu, hk⟩, hd, checkPattern_iff.mpr hp,
        checkPatternBinders_iff.mpr hf,
        Bind.bind, Except.bind]

theorem checkStmt_iff {Δ : Decls} {Γ Γ' : Ctx} {s : Stmt} :
    checkStmt Δ Γ s = .ok Γ' ↔ StmtOk Δ Γ s Γ' := checkStmtWith_iff

theorem finishChecks_iff (Δ : Decls) (atReturn : Bool) (Γ : Ctx) :
    finishChecks Δ atReturn Γ = .ok () ↔
      (if atReturn then Discharged Δ Γ else AbortReady Γ) := by
  induction Γ with
  | nil => cases atReturn <;> simp [finishChecks, Discharged, AbortReady]
  | cons entry rest ih =>
    obtain ⟨x, e⟩ := entry
    cases atReturn <;> cases hu : e.used <;> cases hl : e.live <;> cases h : e.ty.isCopy Δ <;>
      simp_all [finishChecks, Discharged, AbortReady]

theorem checkEndWith_iff {evidence : Ctx → Expr → Bool} {Δ : Decls} {Γ : Ctx} {r : Terminator} :
    checkEnd Δ Γ r = .ok () ↔ BodyOkWith evidence Δ Γ [] r := by
  cases r with
  | abort site =>
    rw [checkEnd, finishChecks_iff]
    exact ⟨BodyOkWith.abort, fun h => by cases h; assumption⟩
  | ret r =>
    constructor
    · intro h
      obtain ⟨⟨⟩, ht, h⟩ := bind_ok_iff.mp h
      obtain ⟨next, hu, h⟩ := bind_ok_iff.mp h
      exact .ret (requireExprType_iff.mp ht) (.atom (useAtom_iff.mp hu)) (by rwa [finishChecks_iff] at h)
    · intro h
      cases h with
      | ret ht hu hd =>
        cases hu with
        | atom hu =>
          simp [checkEnd, requireExprType_iff.mpr ht, useAtom_iff.mpr hu, finishChecks_iff, hd,
            Bind.bind, Except.bind]

theorem checkEnd_iff {Δ : Decls} {Γ : Ctx} {r : Terminator} :
    checkEnd Δ Γ r = .ok () ↔ BodyOk Δ Γ [] r := checkEndWith_iff

theorem checkBodyWith_iff {policy : ArithmeticPolicy} {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {r : Terminator} (position : Nat) :
    (∃ ds, checkCoreBodyWith policy Δ Γ position ss r = .ok ds) ↔ BodyOkWith policy.permits Δ Γ ss r := by
  induction ss generalizing Γ position with
  | nil =>
    constructor
    · rintro ⟨ds, h⟩
      simp only [checkCoreBodyWith] at h
      split at h
      · exact checkEndWith_iff.mp (by assumption)
      · cases h
    · intro h
      exact ⟨[], by simp [checkCoreBodyWith, checkEndWith_iff.mpr h]⟩
  | cons s ss ih =>
    constructor
    · rintro ⟨ds, h⟩
      simp only [checkCoreBodyWith] at h
      split at h
      · cases h
      · rename_i Γ₁ hs
        split at h
        · cases h
        · exact .cons (checkStmtWith_iff.mp hs) ((ih _).mp ⟨_, by assumption⟩)
    · intro h
      cases h with
      | cons hs hb =>
        obtain ⟨ds, hds⟩ := (ih (position + 1)).mpr hb
        exact ⟨(policy.discharge position Γ s).toList ++ ds, by simp [checkCoreBodyWith, checkStmtWith_iff.mpr hs, hds]⟩

theorem checkBody_iff {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {r : Terminator} (position : Nat) :
    (∃ ds, checkCoreBody Δ Γ position ss r = .ok ds) ↔ BodyOk Δ Γ ss r := checkBodyWith_iff (policy := .static) position

private theorem checkDeclsFrom_iff (Δ : Decls) (seen : List Name) (index : Nat) :
    checkDeclsFrom seen index Δ = .ok () ↔ DeclsOk Δ ∧ ∀ d ∈ Δ, d.name ∉ seen := by
  induction Δ generalizing seen index with
  | nil => simp [checkDeclsFrom, DeclsOk]
  | cons d rest ih =>
    by_cases dup : d.name ∈ seen <;> by_cases reserved : d.name = ⟨"main"⟩ <;>
      simp only [checkDeclsFrom, dup, reserved, ↓reduceIte] <;>
      simp_all [DeclsOk, bind_unit_ok_iff, rejectFound_ok_iff,
        firstRepeated_eq_none, List.mem_map] <;> grind

theorem checkDecls_iff (Δ : Decls) : checkDecls Δ = .ok () ↔ DeclsOk Δ := by
  simp [checkDecls, checkDeclsFrom_iff]

theorem checkProgramWith_iff (policy : ArithmeticPolicy) (p : Program) :
    (∃ ds, checkProgramWith policy p = .ok ds) ↔
    DeclsOk p.decls ∧ BodyOkWith policy.permits p.decls [] p.body p.ending := by
  unfold checkProgramWith
  split
  · rename_i hd
    simp [(checkDecls_iff p.decls).mp hd, checkBodyWith_iff]
  · rename_i failure hd
    have h : ¬ DeclsOk p.decls := by
      intro h
      rw [(checkDecls_iff p.decls).mpr h] at hd
      cases hd
    simp [h]

theorem checkProgram_iff (p : Program) :
    (∃ ds, checkProgram p = .ok ds) ↔ Checked p := checkProgramWith_iff .static p

theorem except_isOk_iff {ε α : Type} (r : Except ε α) :
    r.isOk = true ↔ ∃ a, r = .ok a := by
  cases r <;> simp [Except.isOk, Except.toBool]

theorem bodyOkB_iff {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {r : Terminator} :
    bodyOkB Δ Γ ss r = true ↔ BodyOk Δ Γ ss r := by
  rw [bodyOkB, except_isOk_iff, checkBody_iff]

/-- **Checker correspondence.** The checker accepts a program exactly when the judgments do. -/
theorem check_iff (p : Program) : check p = true ↔ Checked p := by
  rw [check, except_isOk_iff, checkProgram_iff]

end ConcreteSpec
