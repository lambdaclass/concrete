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

theorem checkStmt_iff {Δ : Decls} {Γ Γ' : Ctx} {s : Stmt} :
    checkStmt Δ Γ s = .ok Γ' ↔ StmtOk Δ Γ s Γ' := by
  constructor
  · intro h
    cases s with
    | let_ b t e =>
      cases b with
      | wild =>
        simp only [checkStmt] at h
        split at h
        · rename_i ht
          split at h
          · rename_i ha
            split at h
            · exact .letWild (tyOf_iff.mp ht) ha (by assumption) (reads_iff.mp h)
            · cases h
          · cases h
        · cases h
      | name x =>
        simp only [checkStmt] at h
        split at h
        · rename_i ht
          split at h
          · rename_i ha
            split at h
            · rename_i Γ₁ hu
              split at h
              · cases h
                exact .let_ (tyOf_iff.mp ht) ha (reads_iff.mp hu) (by assumption)
              · cases h
            · cases h
          · cases h
        · cases h
    | letStruct s pat e =>
      simp only [checkStmt] at h
      split at h
      · rename_i ht
        split at h
        · rename_i ha
          split at h
          · rename_i d hd
            split at h
            · rename_i hp
              split at h
              · rename_i Γ₁ hu
                split at h
                · cases h
                  exact .letStruct (tyOf_iff.mp ht) ha hd (patOkB_iff.mp hp) (reads_iff.mp hu)
                    (by assumption)
                · cases h
              · cases h
            · cases h
          · cases h
        · cases h
      · cases h
  · intro h
    cases h with
    | let_ ht hk hu hf => simp [checkStmt, tyOf_iff.mpr ht, hk, reads_iff.mpr hu, hf]
    | letWild ht hk hcopy hu => simp [checkStmt, tyOf_iff.mpr ht, hk, hcopy, reads_iff.mpr hu]
    | letStruct ht hk hd hp hu hf =>
      simp [checkStmt, tyOf_iff.mpr ht, hk, hd, patOkB_iff.mpr hp, reads_iff.mpr hu, hf]

theorem finishChecks_iff (Δ : Decls) (atReturn : Bool) (Γ : Ctx) :
    finishChecks Δ atReturn Γ = .ok () ↔
      (if atReturn then Discharged Δ Γ else AbortReady Γ) := by
  induction Γ with
  | nil => cases atReturn <;> simp [finishChecks, Discharged, AbortReady]
  | cons entry rest ih =>
    obtain ⟨x, e⟩ := entry
    cases atReturn <;> cases hu : e.used <;> cases hl : e.live <;> cases h : e.ty.isCopy Δ <;>
      simp_all [finishChecks, Discharged, AbortReady]

theorem checkEnd_iff {Δ : Decls} {Γ : Ctx} {r : Terminator} :
    checkEnd Δ Γ r = .ok () ↔ BodyOk Δ Γ [] r := by
  cases r with
  | abort site =>
    rw [checkEnd, finishChecks_iff]
    exact ⟨BodyOk.abort, fun h => by cases h; assumption⟩
  | ret r =>
    constructor
    · intro h
      simp only [checkEnd] at h
      split at h
      · rename_i ht
        split at h
        · rename_i Γ₁ hu
          exact .ret (tyOf_iff.mp ht) (.atom (useAtom_iff.mp hu)) (by rwa [finishChecks_iff] at h)
        · cases h
      · cases h
    · intro h
      cases h with
      | ret ht hu hd =>
        cases hu with
        | atom hu => simp [checkEnd, tyOf_iff.mpr ht, useAtom_iff.mpr hu, finishChecks_iff, hd]

theorem checkBody_iff {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {r : Terminator} (position : Nat) :
    (∃ ds, checkCoreBody Δ Γ position ss r = .ok ds) ↔ BodyOk Δ Γ ss r := by
  induction ss generalizing Γ position with
  | nil =>
    constructor
    · rintro ⟨ds, h⟩
      simp only [checkCoreBody] at h
      split at h
      · exact checkEnd_iff.mp (by assumption)
      · cases h
    · intro h
      exact ⟨[], by simp [checkCoreBody, checkEnd_iff.mpr h]⟩
  | cons s ss ih =>
    constructor
    · rintro ⟨ds, h⟩
      simp only [checkCoreBody] at h
      split at h
      · cases h
      · rename_i Γ₁ hs
        split at h
        · cases h
        · exact .cons (checkStmt_iff.mp hs) ((ih _).mp ⟨_, by assumption⟩)
    · intro h
      cases h with
      | cons hs hb =>
        obtain ⟨ds, hds⟩ := (ih (position + 1)).mpr hb
        exact ⟨(stmtDischarge position Γ s).toList ++ ds, by simp [checkCoreBody, checkStmt_iff.mpr hs, hds]⟩

private theorem checkDeclFields_iff (Δ : Decls) :
    checkDeclFields Δ = .ok () ↔
      (∀ d ∈ Δ, d.name ≠ ⟨"main"⟩ ∧ (d.fields.map Prod.fst).Nodup) := by
  induction Δ with
  | nil => simp [checkDeclFields]
  | cons d rest ih =>
    by_cases hn : d.name = ⟨"main"⟩ <;> by_cases hf : (d.fields.map Prod.fst).Nodup <;>
      simp [checkDeclFields, hn, hf, ih]

theorem checkDecls_iff (Δ : Decls) : checkDecls Δ = .ok () ↔ DeclsOk Δ := by
  by_cases h : (Δ.map StructDecl.name).Nodup <;>
    simp [checkDecls, DeclsOk, h, checkDeclFields_iff]

theorem checkProgram_iff (p : Program) :
    (∃ ds, checkProgram p = .ok ds) ↔ Checked p := by
  unfold checkProgram
  split
  · rename_i hd
    simp [Checked, (checkDecls_iff p.decls).mp hd, checkBody_iff]
  · rename_i failure hd
    have h : ¬ DeclsOk p.decls := by
      intro h
      rw [(checkDecls_iff p.decls).mpr h] at hd
      cases hd
    simp [Checked, h]

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
