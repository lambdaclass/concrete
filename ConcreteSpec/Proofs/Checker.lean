import ConcreteSpec.Proofs.Slice1

/-!
# The checker agrees with the judgments (`check_iff`), so every `check p = true` example in the specification is a statement about the judgment `Checked p`.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec
open Core (AList)

/-- Law (subject `ConcreteSpec.atomTy`). The executable checker agrees with its independently specified judgment. -/
theorem atomTy_iff {Γ : TyCtx} {a : Atom} {st : STy} : atomTy Γ a = some st ↔ AtomTy Γ a st := by
  constructor
  · intro h
    cases a with
    | int t n =>
      simp only [atomTy] at h
      split at h
      · cases h; exact .int (by assumption)
      · cases h
    | bool b => simp only [atomTy, Option.some.injEq] at h; subst h; exact .bool
    | copy x =>
      simp only [atomTy] at h
      split at h
      · cases h; exact .copy (by assumption)
      · cases h
    | move x =>
      simp only [atomTy] at h
      split at h
      · cases h; exact .move (by assumption)
      · cases h
  · intro h
    cases h with
    | int h => simp [atomTy, h]
    | bool => rfl
    | copy h => simp [atomTy, h]
    | move h => simp [atomTy, h]

/-- Representation lemma. Association-list coverage expressed as a Boolean traversal. (approved: REV-base1c-agy-01025713b) -/
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

/-- Law (subject `ConcreteSpec.fieldsOkB`). The executable checker agrees with its independently specified judgment. -/
theorem fieldsOkB_iff {Γ : TyCtx} {d : StructDecl} {fs : List (Name × Atom)} :
    fieldsOkB Γ d fs = true ↔ FieldsTy Γ d fs := by
  simp only [fieldsOkB, Bool.and_eq_true, decide_eq_true_eq, ← covers_iff]
  constructor
  · rintro ⟨⟨hn, ht⟩, hc⟩
    refine ⟨hn, ?_, hc⟩
    intro p hp
    have := (List.all_eq_true.mp ht) p hp
    split at this
    · rename_i st hst
      exact ⟨st, hst, atomTy_iff.mp (by simpa using this)⟩
    · cases this
  · rintro ⟨hn, ht, hc⟩
    refine ⟨⟨hn, List.all_eq_true.mpr ?_⟩, hc⟩
    intro p hp
    obtain ⟨st, hst, ha⟩ := ht p hp
    simp [hst, atomTy_iff.mpr ha]

/-- Law (subject `ConcreteSpec.tyOf`). The executable checker agrees with its independently specified judgment. -/
theorem tyOf_iff {Δ : Decls} {Γ : TyCtx} {e : Expr} {ty : Ty} :
    tyOf Δ Γ e = some ty ↔ HasType Δ Γ e ty := by
  cases e with
  | atom a =>
    cases a with
    | int t n =>
      simp only [tyOf]
      constructor
      · intro h; split at h
        · cases h; exact .int (by assumption)
        · cases h
      · intro h; cases h with | int h => simp [h]
    | bool b =>
      simp only [tyOf, Option.some.injEq]
      constructor
      · intro h; subst h; exact .bool
      · intro h; cases h; rfl
    | copy x =>
      simp only [tyOf]
      constructor
      · exact .copy
      · intro h; cases h with | copy h => exact h
    | move x =>
      simp only [tyOf]
      constructor
      · exact .move
      · intro h; cases h with | move h => exact h
  | add l r =>
    constructor
    · intro h
      simp only [tyOf] at h
      split at h
      · rename_i t t' hl hr
        split at h
        · cases h; subst_vars
          exact .add (atomTy_iff.mp hl) (atomTy_iff.mp hr)
        · cases h
      · cases h
    · intro h
      cases h with
      | add hl hr => simp [tyOf, atomTy_iff.mpr hl, atomTy_iff.mpr hr]
  | mk s fs =>
    constructor
    · intro h
      simp only [tyOf] at h
      split at h
      · rename_i d hd
        split at h
        · cases h; exact .mk hd (fieldsOkB_iff.mp (by assumption))
        · cases h
      · cases h
    · intro h
      cases h with
      | mk hd hf => simp [tyOf, hd, fieldsOkB_iff.mpr hf]

/-- Law (subject `ConcreteSpec.useAtom`). The executable checker agrees with its independently specified judgment. -/
theorem useAtom_iff {Δ : Decls} {Γ Γ' : Ctx} {a : Atom} :
    useAtom Δ Γ a = some Γ' ↔ UseAtom Δ Γ a Γ' := by
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
        · rename_i hc; cases h; exact .copy he hc.1 hc.2
        · cases h
      · cases h
    | move x =>
      simp only [useAtom] at h
      split at h
      · rename_i e he
        split at h
        · rename_i hc; cases h; exact .move he hc.1 hc.2
        · cases h
      · cases h
  · intro h
    cases h with
    | int => rfl
    | bool => rfl
    | copy hf hl hcp => simp [useAtom, hf, hl, hcp]
    | move hf hl hcp => simp [useAtom, hf, hl, hcp]

/-- Law (subject `ConcreteSpec.useAtoms`). The executable checker agrees with its independently specified judgment. -/
theorem useAtoms_iff {Δ : Decls} :
    ∀ {Γ Γ' : Ctx} {as : List Atom}, useAtoms Δ Γ as = some Γ' ↔ UseAtoms Δ Γ as Γ'
  | Γ, Γ', [] => by
    simp only [useAtoms, Option.some.injEq]
    constructor
    · intro h; subst h; exact .nil
    · intro h; cases h; rfl
  | Γ, Γ', a :: as => by
    simp only [useAtoms]
    constructor
    · intro h
      split at h
      · rename_i Γ₁ h₁; exact .cons (useAtom_iff.mp h₁) (useAtoms_iff.mp h)
      · cases h
    · intro h
      cases h with
      | cons h₁ h₂ => rw [useAtom_iff.mpr h₁]; exact useAtoms_iff.mpr h₂

/-- Law (subject `ConcreteSpec.uses`). The executable checker agrees with its independently specified judgment. -/
theorem uses_iff {Δ : Decls} {Γ Γ' : Ctx} {e : Expr} : uses Δ Γ e = some Γ' ↔ Uses Δ Γ e Γ' := by
  cases e with
  | atom a =>
    simp only [uses]
    constructor
    · intro h; exact .atom (useAtom_iff.mp h)
    · intro h; cases h with | atom h => exact useAtom_iff.mpr h
  | add l r =>
    simp only [uses]
    constructor
    · intro h
      split at h
      · rename_i Γ₁ h₁; exact .add (useAtom_iff.mp h₁) (useAtom_iff.mp h)
      · cases h
    · intro h
      cases h with
      | add h₁ h₂ => rw [useAtom_iff.mpr h₁]; exact useAtom_iff.mpr h₂
  | mk s fs =>
    simp only [uses]
    constructor
    · intro h; exact .mk (useAtoms_iff.mp h)
    · intro h; cases h with | mk h => exact useAtoms_iff.mpr h

/-- Law (subject `ConcreteSpec.patOkB`). The executable checker agrees with its independently specified judgment. -/
theorem patOkB_iff {d : StructDecl} {pat : List (Name × Binder)} : patOkB d pat = true ↔ PatOk d pat := by
  simp only [patOkB, Bool.and_eq_true, decide_eq_true_eq]
  constructor
  · rintro ⟨⟨hn, hd⟩, hc⟩
    refine ⟨hn, ?_, covers_iff.mpr hc⟩
    intro p hp
    have := (List.all_eq_true.mp hd) p hp
    simpa [Option.isSome_iff_exists] using this
  · rintro ⟨hn, hd, hc⟩
    refine ⟨⟨hn, List.all_eq_true.mpr ?_⟩, covers_iff.mp hc⟩
    intro p hp
    obtain ⟨st, hst⟩ := hd p hp
    simp [hst]

/-- Law (subject `ConcreteSpec.stmtOut`). The executable checker agrees with its independently specified judgment. -/
theorem stmtOut_iff {Δ : Decls} {Γ Γ' : Ctx} {s : Stmt} :
    stmtOut Δ Γ s = some Γ' ↔ StmtOk Δ Γ s Γ' := by
  constructor
  · intro h
    cases s with
    | let_ b t e =>
      cases b with
      | wild =>
        simp only [stmtOut] at h
        split at h
        · rename_i hc
          exact .letWild (tyOf_iff.mp hc.1) hc.2.1 hc.2.2 (uses_iff.mp h)
        · cases h
      | name x =>
          simp only [stmtOut] at h
          split at h
          · rename_i ht
            split at h
            · rename_i Γ₁ hu
              split at h
              · cases h; exact .let_ (tyOf_iff.mp ht.1) ht.2 (uses_iff.mp hu) (by assumption)
              · cases h
            · cases h
          · cases h
    | letStruct s pat e =>
      simp only [stmtOut] at h
      split at h
      · rename_i ht
        split at h
        · rename_i d hd
          split at h
          · rename_i hp
            split at h
            · rename_i Γ₁ hu
              split at h
              · cases h
                exact .letStruct (tyOf_iff.mp ht.1) ht.2 hd (patOkB_iff.mp hp) (uses_iff.mp hu)
                  (by assumption)
              · cases h
            · cases h
          · cases h
        · cases h
      · cases h
  · intro h
    cases h with
    | let_ ht hk hu hf => simp [stmtOut, tyOf_iff.mpr ht, hk, uses_iff.mpr hu, hf]
    | letWild ht hk hcopy hu => simp [stmtOut, tyOf_iff.mpr ht, hk, hcopy, uses_iff.mpr hu]
    | letStruct ht hk hd hp hu hf =>
      simp [stmtOut, tyOf_iff.mpr ht, hk, hd, patOkB_iff.mpr hp, uses_iff.mpr hu, hf]

/-- Law (subject `ConcreteSpec.bodyOkB`). The executable checker agrees with its independently specified judgment. -/
theorem bodyOkB_iff {Δ : Decls} :
    ∀ {Γ : Ctx} {ss : List Stmt} {r : Atom}, bodyOkB Δ Γ ss r = true ↔ BodyOk Δ Γ ss r
  | Γ, [], r => by
    simp only [bodyOkB, Bool.and_eq_true, beq_iff_eq]
    constructor
    · rintro ⟨ht, hu⟩
      split at hu
      · rename_i Γ₁ hu'
        exact .ret (tyOf_iff.mp ht) (uses_iff.mp hu') (by simpa using hu)
      · cases hu
    · intro h
      cases h with
      | ret ht hu hd => simp [tyOf_iff.mpr ht, uses_iff.mpr hu, hd]
  | Γ, s :: ss, r => by
    simp only [bodyOkB]
    constructor
    · intro h
      split at h
      · rename_i Γ₁ hs; exact .cons (stmtOut_iff.mp hs) (bodyOkB_iff.mp h)
      · cases h
    · intro h
      cases h with
      | cons hs hb => rw [stmtOut_iff.mpr hs]; exact bodyOkB_iff.mpr hb

/-- **Checker correspondence.** The checker accepts a program exactly when the judgments do.
Law (subject `ConcreteSpec.check`). The executable checker agrees with its independently specified judgment. -/
theorem check_iff (p : Program) : check p = true ↔ Checked p := by
  simp only [check, Bool.and_eq_true, decide_eq_true_eq, bodyOkB_iff, Checked]

namespace CheckerEvidence

def rejectAtomTy : TyCtx → Atom → Option STy := fun _ _ => none

/-- Instance (law `ConcreteSpec.atomTy_iff`, case `atomTyAccepted`). -/
example : atomTy [] (.bool true) = some .bool ↔ AtomTy [] (.bool true) .bool := @atomTy_iff [] (.bool true) .bool

/-- Planted (law `ConcreteSpec.atomTy_iff`, case `atomTyAccepted`, subject `ConcreteSpec.atomTy`, mutant `ConcreteSpec.CheckerEvidence.rejectAtomTy`). -/
example : ¬ (rejectAtomTy [] (.bool true) = some .bool ↔ AtomTy [] (.bool true) .bool) := by
  intro h
  have accepted := (@atomTy_iff [] (.bool true) .bool).mp (by rfl)
  have bad := h.mpr accepted
  contradiction

def rejectFieldsOkB : TyCtx → StructDecl → List (Name × Atom) → Bool := fun _ _ _ => false

/-- Instance (law `ConcreteSpec.fieldsOkB_iff`, case `fieldsOkBAccepted`). -/
example : fieldsOkB [] ⟨"Token", false, []⟩ [] = true ↔ FieldsTy [] ⟨"Token", false, []⟩ [] := @fieldsOkB_iff [] ⟨"Token", false, []⟩ []

/-- Planted (law `ConcreteSpec.fieldsOkB_iff`, case `fieldsOkBAccepted`, subject `ConcreteSpec.fieldsOkB`, mutant `ConcreteSpec.CheckerEvidence.rejectFieldsOkB`). -/
example : ¬ (rejectFieldsOkB [] ⟨"Token", false, []⟩ [] = true ↔ FieldsTy [] ⟨"Token", false, []⟩ []) := by
  intro h
  have accepted := (@fieldsOkB_iff [] ⟨"Token", false, []⟩ []).mp (by rfl)
  have bad := h.mpr accepted
  contradiction

def rejectTyOf : Decls → TyCtx → Expr → Option Ty := fun _ _ _ => none

/-- Instance (law `ConcreteSpec.tyOf_iff`, case `tyOfAccepted`). -/
example : tyOf [] [] (.atom (.bool true)) = some (.scalar .bool) ↔ HasType [] [] (.atom (.bool true)) (.scalar .bool) := @tyOf_iff [] [] (.atom (.bool true)) (.scalar .bool)

/-- Planted (law `ConcreteSpec.tyOf_iff`, case `tyOfAccepted`, subject `ConcreteSpec.tyOf`, mutant `ConcreteSpec.CheckerEvidence.rejectTyOf`). -/
example : ¬ (rejectTyOf [] [] (.atom (.bool true)) = some (.scalar .bool) ↔ HasType [] [] (.atom (.bool true)) (.scalar .bool)) := by
  intro h
  have accepted := (@tyOf_iff [] [] (.atom (.bool true)) (.scalar .bool)).mp (by rfl)
  have bad := h.mpr accepted
  contradiction

def rejectUseAtom : Decls → Ctx → Atom → Option Ctx := fun _ _ _ => none

/-- Instance (law `ConcreteSpec.useAtom_iff`, case `useAtomAccepted`). -/
example : useAtom [] [] (.bool true) = some [] ↔ UseAtom [] [] (.bool true) [] := @useAtom_iff [] [] [] (.bool true)

/-- Planted (law `ConcreteSpec.useAtom_iff`, case `useAtomAccepted`, subject `ConcreteSpec.useAtom`, mutant `ConcreteSpec.CheckerEvidence.rejectUseAtom`). -/
example : ¬ (rejectUseAtom [] [] (.bool true) = some [] ↔ UseAtom [] [] (.bool true) []) := by
  intro h
  have accepted := (@useAtom_iff [] [] [] (.bool true)).mp (by rfl)
  have bad := h.mpr accepted
  contradiction

def rejectUseAtoms : Decls → Ctx → List Atom → Option Ctx := fun _ _ _ => none

/-- Instance (law `ConcreteSpec.useAtoms_iff`, case `useAtomsAccepted`). -/
example : useAtoms [] [] [.bool true] = some [] ↔ UseAtoms [] [] [.bool true] [] := @useAtoms_iff [] [] [] [.bool true]

/-- Planted (law `ConcreteSpec.useAtoms_iff`, case `useAtomsAccepted`, subject `ConcreteSpec.useAtoms`, mutant `ConcreteSpec.CheckerEvidence.rejectUseAtoms`). -/
example : ¬ (rejectUseAtoms [] [] [.bool true] = some [] ↔ UseAtoms [] [] [.bool true] []) := by
  intro h
  have accepted := (@useAtoms_iff [] [] [] [.bool true]).mp (by rfl)
  have bad := h.mpr accepted
  contradiction

def rejectUses : Decls → Ctx → Expr → Option Ctx := fun _ _ _ => none

/-- Instance (law `ConcreteSpec.uses_iff`, case `usesAccepted`). -/
example : uses [] [] (.atom (.bool true)) = some [] ↔ Uses [] [] (.atom (.bool true)) [] := @uses_iff [] [] [] (.atom (.bool true))

/-- Planted (law `ConcreteSpec.uses_iff`, case `usesAccepted`, subject `ConcreteSpec.uses`, mutant `ConcreteSpec.CheckerEvidence.rejectUses`). -/
example : ¬ (rejectUses [] [] (.atom (.bool true)) = some [] ↔ Uses [] [] (.atom (.bool true)) []) := by
  intro h
  have accepted := (@uses_iff [] [] [] (.atom (.bool true))).mp (by rfl)
  have bad := h.mpr accepted
  contradiction

def rejectPatOkB : StructDecl → List (Name × Binder) → Bool := fun _ _ => false

/-- Instance (law `ConcreteSpec.patOkB_iff`, case `patOkBAccepted`). -/
example : patOkB ⟨"Token", false, []⟩ [] = true ↔ PatOk ⟨"Token", false, []⟩ [] := @patOkB_iff ⟨"Token", false, []⟩ []

/-- Planted (law `ConcreteSpec.patOkB_iff`, case `patOkBAccepted`, subject `ConcreteSpec.patOkB`, mutant `ConcreteSpec.CheckerEvidence.rejectPatOkB`). -/
example : ¬ (rejectPatOkB ⟨"Token", false, []⟩ [] = true ↔ PatOk ⟨"Token", false, []⟩ []) := by
  intro h
  have accepted := (@patOkB_iff ⟨"Token", false, []⟩ []).mp (by rfl)
  have bad := h.mpr accepted
  contradiction

def rejectStmtOut : Decls → Ctx → Stmt → Option Ctx := fun _ _ _ => none

/-- Instance (law `ConcreteSpec.stmtOut_iff`, case `stmtOutAccepted`). -/
example : stmtOut [] [] (.let_ .wild (.scalar .bool) (.atom (.bool true))) = some [] ↔ StmtOk [] [] (.let_ .wild (.scalar .bool) (.atom (.bool true))) [] := @stmtOut_iff [] [] [] (.let_ .wild (.scalar .bool) (.atom (.bool true)))

/-- Planted (law `ConcreteSpec.stmtOut_iff`, case `stmtOutAccepted`, subject `ConcreteSpec.stmtOut`, mutant `ConcreteSpec.CheckerEvidence.rejectStmtOut`). -/
example : ¬ (rejectStmtOut [] [] (.let_ .wild (.scalar .bool) (.atom (.bool true))) = some [] ↔ StmtOk [] [] (.let_ .wild (.scalar .bool) (.atom (.bool true))) []) := by
  intro h
  have accepted := (@stmtOut_iff [] [] [] (.let_ .wild (.scalar .bool) (.atom (.bool true)))).mp (by rfl)
  have bad := h.mpr accepted
  contradiction

def rejectBodyOkB : Decls → Ctx → List Stmt → Atom → Bool := fun _ _ _ _ => false

/-- Instance (law `ConcreteSpec.bodyOkB_iff`, case `bodyOkBAccepted`). -/
example : bodyOkB [] [] [] (.int .u8 42) = true ↔ BodyOk [] [] [] (.int .u8 42) := @bodyOkB_iff [] [] [] (.int .u8 42)

/-- Planted (law `ConcreteSpec.bodyOkB_iff`, case `bodyOkBAccepted`, subject `ConcreteSpec.bodyOkB`, mutant `ConcreteSpec.CheckerEvidence.rejectBodyOkB`). -/
example : ¬ (rejectBodyOkB [] [] [] (.int .u8 42) = true ↔ BodyOk [] [] [] (.int .u8 42)) := by
  intro h
  have accepted := (@bodyOkB_iff [] [] [] (.int .u8 42)).mp (by rfl)
  have bad := h.mpr accepted
  contradiction

def rejectCheck : Program → Bool := fun _ => false

/-- Instance (law `ConcreteSpec.check_iff`, case `checkAccepted`). -/
example : check ⟨[], [], .int .u8 42⟩ = true ↔ Checked ⟨[], [], .int .u8 42⟩ := check_iff ⟨[], [], .int .u8 42⟩

/-- Planted (law `ConcreteSpec.check_iff`, case `checkAccepted`, subject `ConcreteSpec.check`, mutant `ConcreteSpec.CheckerEvidence.rejectCheck`). -/
example : ¬ (rejectCheck ⟨[], [], .int .u8 42⟩ = true ↔ Checked ⟨[], [], .int .u8 42⟩) := by
  intro h
  have accepted := (check_iff ⟨[], [], .int .u8 42⟩).mp (by rfl)
  have bad := h.mpr accepted
  contradiction

end CheckerEvidence

end ConcreteSpec
