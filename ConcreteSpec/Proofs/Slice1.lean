import ConcreteSpec.Proofs.Basics
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Slice1Ownership
import ConcreteSpec.Spec.Slice1Semantics
import ConcreteSpec.Spec.Slice1Syntax
import ConcreteSpec.Spec.Statics

/-!
# Slice-1 proofs: fuel properties of the evaluator, and safety (`checked_never_stuck`, `checked_no_implicit_traps`, `checked_exits`).

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec
open Core (AList)

/-! ### Properties of the semantics -/

/-- Fuel monotonicity: a run that finishes with some fuel finishes the same way with more. -/
theorem runBody_mono {Δ : Decls} {r : Atom} :
    ∀ {n : Nat} {env : Env} {ss : List Stmt} {o : Outcome}, runBody Δ n env ss r = o →
      o ≠ .outOfFuel → ∀ m, n ≤ m → runBody Δ m env ss r = o := by
  intro n
  induction n with
  | zero => intro env ss o h hne; simp [runBody] at h; exact absurd h.symm hne
  | succ n ih =>
    intro env ss o h hne m hm
    obtain ⟨m, rfl⟩ : ∃ m', m = m' + 1 := ⟨m - 1, by omega⟩
    cases ss with
    | nil => simpa [runBody] using h
    | cons s ss =>
      simp only [runBody] at h ⊢
      split at h
      · exact ih h hne m (by omega)
      · exact h
      · exact h

/-- The `return` step never runs out of fuel. -/
theorem retOutcome_ne_outOfFuel (Δ : Decls) (env : Env) (r : Atom) :
    retOutcome Δ env r ≠ .outOfFuel := by
  unfold retOutcome; split <;> simp

/-- A body of `k` statements finishes with `k + 1` units of fuel. -/
theorem runBody_enough {Δ : Decls} {r : Atom} :
    ∀ (ss : List Stmt) (env : Env), runBody Δ (ss.length + 1) env ss r ≠ .outOfFuel
  | [], env => by simpa [runBody] using retOutcome_ne_outOfFuel Δ env r
  | s :: ss, env => by
    simp only [List.length_cons, runBody]
    split
    · exact runBody_enough ss _
    · simp
    · simp

/-- The evaluator is sound for `Exec`: an outcome reached with some fuel is an `Exec` outcome. -/
theorem runBody_sound {Δ : Decls} {r : Atom} :
    ∀ {n : Nat} {env : Env} {ss : List Stmt} {o : Outcome}, runBody Δ n env ss r = o →
      o ≠ .outOfFuel → Exec Δ env ss r o := by
  intro n
  induction n with
  | zero => intro env ss o h hne; simp [runBody] at h; exact absurd h.symm hne
  | succ n ih =>
    intro env ss o h hne
    cases ss with
    | nil => simp only [runBody] at h; subst h; exact .ret
    | cons s ss =>
      simp only [runBody] at h
      split at h
      · rename_i env' he; exact .step he (ih h hne)
      · rename_i k he; subst h; exact .trap he
      · rename_i w he; subst h; exact .stuck he

/-- The evaluator is complete for `Exec`: every `Exec` outcome is reached with enough fuel. -/
theorem exec_complete {Δ : Decls} {r : Atom} {env : Env} {ss : List Stmt} {o : Outcome}
    (h : Exec Δ env ss r o) : runBody Δ (ss.length + 1) env ss r = o := by
  induction h with
  | ret => simp [runBody]
  | step he _ ih => simp only [List.length_cons, runBody, he]; exact ih
  | trap he => simp [runBody, he]
  | stuck he => simp [runBody, he]

/-- `Exec` never ends in `outOfFuel`. -/
theorem exec_ne_outOfFuel {Δ : Decls} {r : Atom} {env : Env} {ss : List Stmt} {o : Outcome}
    (h : Exec Δ env ss r o) : o ≠ .outOfFuel := by
  induction h with
  | ret => exact retOutcome_ne_outOfFuel _ _ _
  | step _ _ ih => exact ih
  | trap => simp
  | stuck => simp

/-- Determinism: a body has at most one outcome. -/
theorem exec_deterministic {Δ : Decls} {r : Atom} {env : Env} {ss : List Stmt} {o₁ o₂ : Outcome}
    (h₁ : Exec Δ env ss r o₁) (h₂ : Exec Δ env ss r o₂) : o₁ = o₂ := by
  rw [← exec_complete h₁, ← exec_complete h₂]

/-- Totality: every body has exactly one outcome (slice 1 has no loops or calls). -/
theorem exec_total (Δ : Decls) (env : Env) (ss : List Stmt) (r : Atom) :
    ∃ o, Exec Δ env ss r o :=
  ⟨_, runBody_sound rfl (runBody_enough ss env)⟩

/-- The context describes the environment: the same identities in the same order, every live
binding holds a value of its type, a binding with a known value holds exactly that scalar and has
a Copy type, and a slot holding a struct value agrees with the binding's known field values. A
binding the context records as moved is otherwise unconstrained here; the evaluator marks such a
slot `moved` when it moves the value out. -/
inductive Corr (Δ : Decls) : Ctx → Env → Prop where
  | nil : Corr Δ [] []
  | cons {x e sl Γ env} : (e.live = true → ∃ v, sl = .val v ∧ VTy Δ v e.ty) →
      (∀ s, e.known = some s → sl = .val (.scalar s) ∧ e.ty.isCopy Δ = true) →
      (∀ sn vs, sl = .val (.struct sn vs) → FieldsHold e.kfields vs) →
      Corr Δ Γ env → Corr Δ ((x, e) :: Γ) ((x, sl) :: env)

theorem tys_lookup (x : Var) (Γ : Ctx) : AList.lookup Γ.tys x = (AList.lookup Γ x).map Entry.ty :=
  AList.lookup_map Entry.ty Γ x

theorem ctxMark_tys (x : Var) (mv : Bool) (Γ : Ctx) : (ctxMark Γ x mv).tys = Γ.tys :=
  AList.map_update Entry.ty (Entry.read mv) (fun _ => rfl) Γ x

theorem useAtom_tys {Δ : Decls} {Γ Γ' : Ctx} {a : Atom} (h : UseAtom Δ Γ a Γ') : Γ'.tys = Γ.tys := by
  cases h with
  | int => rfl
  | bool => rfl
  | copy => exact ctxMark_tys _ _ _
  | move => exact ctxMark_tys _ _ _

theorem useAtoms_tys {Δ : Decls} {Γ Γ' : Ctx} {as : List Atom} (h : UseAtoms Δ Γ as Γ') :
    Γ'.tys = Γ.tys := by
  induction h with
  | nil => rfl
  | cons h₁ _ ih => rw [ih, useAtom_tys h₁]

theorem uses_tys {Δ : Decls} {Γ Γ' : Ctx} {e : Expr} (h : Uses Δ Γ e Γ') : Γ'.tys = Γ.tys := by
  cases h with
  | atom h => exact useAtom_tys h
  | add h₁ h₂ => rw [useAtom_tys h₂, useAtom_tys h₁]
  | mk h => exact useAtoms_tys h

theorem vty_isCopy {Δ : Decls} {v : Val} {t : Ty} (h : VTy Δ v t) : v.isCopy Δ = t.isCopy Δ := by
  cases h <;> rfl

theorem corr_find {Δ : Decls} {x : Var} {Γ : Ctx} {env : Env} (hc : Corr Δ Γ env) :
    ∀ {e : Entry}, AList.lookup Γ x = some e →
      (e.live = true → ∃ v, AList.lookup env x = some (.val v) ∧ VTy Δ v e.ty) ∧
      (∀ s, e.known = some s → AList.lookup env x = some (.val (.scalar s)) ∧ e.ty.isCopy Δ = true) ∧
      (∀ sn vs, AList.lookup env x = some (.val (.struct sn vs)) → FieldsHold e.kfields vs) := by
  induction hc with
  | nil => intro e h; simp [AList.lookup] at h
  | cons hsl hk hkf _ ih =>
    rename_i x' e' sl Γ env
    intro e h
    simp only [AList.lookup, AList.lookup] at h ⊢
    split at h
    · cases h
      rename_i heq
      refine ⟨fun hl => ?_, fun s hs => ?_, fun sn vs hv => ?_⟩
      · obtain ⟨v, rfl, hv⟩ := hsl hl
        exact ⟨v, by simp [heq], hv⟩
      · obtain ⟨rfl, hcp⟩ := hk s hs
        exact ⟨by simp [heq], hcp⟩
      · simp only [heq, ↓reduceIte, Option.some.injEq] at hv
        exact hkf sn vs hv
    · rename_i hne
      simp only [hne, ↓reduceIte]
      exact ih h

theorem corr_mark_copy {Δ : Decls} {x : Var} {Γ : Ctx} {env : Env} (hc : Corr Δ Γ env) :
    Corr Δ (ctxMark Γ x false) env := by
  induction hc with
  | nil => exact .nil
  | cons hsl hk hkf hc ih =>
    rename_i x' e sl Γ env
    rw [ctxMark, AList.update_cons]
    by_cases h : x' = x
    · rw [ite_eq_left h]
      exact .cons (fun hl => hsl (by rw [← hl]; exact (Bool.and_true e.live).symm)) hk hkf hc
    · rw [ite_eq_right h]; exact .cons hsl hk hkf ih

/-- Moving `x` out keeps the description, provided `x`'s binding has no known value (a known
value is a Copy scalar, which is never moved). -/
theorem corr_mark_move {Δ : Decls} {x : Var} {Γ : Ctx} {env : Env} (hc : Corr Δ Γ env) :
    (∀ e, AList.lookup Γ x = some e → e.known = none) →
    Corr Δ (ctxMark Γ x true) (AList.update env x (fun _ => .moved)) := by
  induction hc with
  | nil => intro _; exact .nil
  | cons hsl hk hkf hc ih =>
    rename_i x' e sl Γ env
    intro hn
    simp only [ctxMark, AList.update]
    split
    · rename_i heq
      have hkn : e.known = none := hn e (by simp [AList.lookup, heq])
      exact .cons (by simp [Entry.read]) (by intro s hs; simp [Entry.read, hkn] at hs)
        (by intro sn vs h; cases h) hc
    · rename_i hne
      exact .cons hsl hk hkf (ih (fun e' h => hn e' (by simp [AList.lookup, hne, h])))

/-- Reading a live local in the mode its type requires succeeds with a value of its type, and the
context's record of the read matches what the evaluator did. -/
theorem readVar_safe {Δ : Decls} {Γ : Ctx} {env : Env} {x : Var} {e : Entry} {mv : Bool}
    (hc : Corr Δ Γ env) (hf : AList.lookup Γ x = some e) (hl : e.live = true)
    (hm : e.ty.isCopy Δ = !mv) :
    ∃ v env', readVar Δ env x mv = .ok (v, env') ∧ VTy Δ v e.ty ∧
      (∀ sn vs, v = .struct sn vs → FieldsHold e.kfields vs) ∧
      Corr Δ (ctxMark Γ x mv) env' := by
  obtain ⟨hlive, hknown, hkf⟩ := corr_find hc hf
  obtain ⟨v, hv, hty⟩ := hlive hl
  have hvc : v.isCopy Δ = !mv := by rw [vty_isCopy hty]; exact hm
  refine ⟨v, _, by simp [readVar, hv, hvc]; rfl, hty,
    fun sn vs h => hkf sn vs (by rw [hv, h]), ?_⟩
  cases mv
  · simpa using corr_mark_copy hc
  · simp only [↓reduceIte]
    refine corr_mark_move hc ?_
    intro e' he'
    rw [hf] at he'
    cases he'
    cases hk : e.known with
    | none => rfl
    | some s => have := (hknown s hk).2; rw [hm] at this; cases this

/-- A known atom evaluates to its known value without changing the environment (Q6.2). -/
theorem knownAtom_sound {Δ : Decls} {Γ : Ctx} {env : Env} {a : Atom} {s : Scalar}
    (hc : Corr Δ Γ env) (hk : knownAtom Γ a = some s) : evalAtom Δ env a = .ok (s, env) := by
  cases a with
  | int t n => simp [knownAtom] at hk; subst hk; rfl
  | bool b => simp [knownAtom] at hk; subst hk; rfl
  | move x => simp [knownAtom] at hk
  | copy x =>
    simp only [knownAtom, Option.bind_eq_some_iff] at hk
    obtain ⟨e, hf, hke⟩ := hk
    obtain ⟨hv, _⟩ := (corr_find hc hf).2.1 s hke
    simp [evalAtom, readVar, hv, Val.isCopy]

/-- Recording a read changes no binding's known value or field facts. -/
theorem knownAtom_mark (y : Var) (mv : Bool) (a : Atom) (Γ : Ctx) :
    knownAtom (ctxMark Γ y mv) a = knownAtom Γ a := by
  cases a with
  | copy x =>
    show (AList.lookup (ctxMark Γ y mv) x).bind Entry.known = (AList.lookup Γ x).bind Entry.known
    rw [ctxMark, AList.lookup_update]
    by_cases h : x = y
    · subst h; rw [ite_eq_left rfl]; cases AList.lookup Γ x <;> rfl
    · rw [ite_eq_right h]
  | _ => rfl

theorem useAtom_knownAtom {Δ : Decls} {Γ Γ' : Ctx} {a : Atom} (h : UseAtom Δ Γ a Γ') (b : Atom) :
    knownAtom Γ' b = knownAtom Γ b := by
  cases h with
  | int => rfl
  | bool => rfl
  | copy => exact knownAtom_mark _ _ _ _
  | move => exact knownAtom_mark _ _ _ _

theorem evalAtom_safe {Δ : Decls} {Γ Γ' : Ctx} {env : Env} {a : Atom} {st : STy}
    (hc : Corr Δ Γ env) (ht : AtomTy Γ.tys a st) (hu : UseAtom Δ Γ a Γ') :
    ∃ v env', evalAtom Δ env a = .ok (v, env') ∧ SVTy v st ∧ Corr Δ Γ' env' := by
  cases hu with
  | int => cases ht with | int h => exact ⟨_, _, rfl, .int h, hc⟩
  | bool => cases ht with | bool => exact ⟨_, _, rfl, .bool, hc⟩
  | copy hf hl hcp =>
    cases ht with
    | copy hx =>
      rw [tys_lookup, hf] at hx
      simp only [Option.map_some, Option.some.injEq] at hx
      obtain ⟨v, env', hr, hv, _, hc'⟩ := readVar_safe (mv := false) hc hf hl (by simpa using hcp)
      rw [hx] at hv
      cases hv with
      | scalar hs => exact ⟨_, env', by simp [evalAtom, hr], hs, hc'⟩
  | move hf hl hcp =>
    cases ht with
    | move hx =>
      rw [tys_lookup, hf] at hx
      simp only [Option.map_some, Option.some.injEq] at hx
      obtain ⟨v, env', hr, hv, _, hc'⟩ := readVar_safe (mv := true) hc hf hl (by simpa using hcp)
      rw [hx] at hv
      cases hv with
      | scalar hs => exact ⟨_, env', by simp [evalAtom, hr], hs, hc'⟩

theorem evalFields_safe {Δ : Decls} :
    ∀ {fs : List (Name × Atom)} {Γ Γ' : Ctx} {env : Env}, Corr Δ Γ env →
      (∀ p ∈ fs, ∃ st, AtomTy Γ.tys p.2 st) → UseAtoms Δ Γ (fs.map Prod.snd) Γ' →
      ∃ vs env', evalFields Δ env fs = .ok (vs, env') ∧ Corr Δ Γ' env' ∧
        (∀ f a, AList.lookup fs f = some a → ∀ st, AtomTy Γ.tys a st →
          ∃ v, AList.lookup vs f = some v ∧ SVTy v st) ∧
        (∀ f a, AList.lookup fs f = some a → ∀ s, knownAtom Γ a = some s → AList.lookup vs f = some s)
  | [], Γ, Γ', env, hc, _, hu => by
    cases hu
    exact ⟨[], env, rfl, hc, by intro f a h; simp [AList.lookup] at h,
      by intro f a h; simp [AList.lookup] at h⟩
  | (g, a) :: rest, Γ, Γ', env, hc, ht, hu => by
    cases hu with
    | cons hu₁ hu₂ =>
      rename_i Γ₁
      obtain ⟨st₀, ha⟩ := ht (g, a) (List.Mem.head _)
      obtain ⟨v, env₁, he, _, hc₁⟩ := evalAtom_safe hc ha hu₁
      have htys : Γ₁.tys = Γ.tys := useAtom_tys hu₁
      have ht' : ∀ p ∈ rest, ∃ st, AtomTy Γ₁.tys p.2 st := by
        intro p hp; rw [htys]; exact ht p (List.Mem.tail _ hp)
      obtain ⟨vs, env₂, hr, hc₂, hl, hk⟩ := evalFields_safe hc₁ ht' hu₂
      refine ⟨(g, v) :: vs, env₂, by simp [evalFields, he, hr], hc₂, ?_, ?_⟩
      · intro f a' hf st hst
        simp only [AList.lookup] at hf ⊢
        split at hf
        next hgf =>
          cases hf
          obtain ⟨v', env', he', hv', _⟩ := evalAtom_safe hc hst hu₁
          rw [he] at he'
          cases he'
          exact ⟨v, by simp [hgf], hv'⟩
        next hne =>
          simp only [hne, ↓reduceIte]
          exact hl f a' hf st (by rw [htys]; exact hst)
      · intro f a' hf s hks
        simp only [AList.lookup] at hf ⊢
        split at hf
        next hgf =>
          cases hf
          have h := knownAtom_sound hc hks
          rw [he] at h
          cases h
          simp [hgf]
        next hne =>
          simp only [hne, ↓reduceIte]
          exact hk f a' hf s (by rw [useAtom_knownAtom hu₁]; exact hks)

/-- Expression safety: a well-typed expression whose reads are allowed evaluates to a value of its
type, or traps. It never gets stuck, and in particular never reads a moved variable. A struct
value it produces agrees with the expression's known field values (Q6.2). -/
theorem evalExpr_safe {Δ : Decls} {e : Expr} {Γ Γ' : Ctx} {env : Env} {t : Ty}
    (hc : Corr Δ Γ env) (ht : HasType Δ Γ.tys e t) (hu : Uses Δ Γ e Γ') :
    (∃ k, evalExpr Δ env e = .trap k) ∨
    ∃ v env', evalExpr Δ env e = .ok (v, env') ∧ VTy Δ v t ∧
      (∀ sn vs, v = .struct sn vs → FieldsHold (knownFields Γ e) vs) ∧ Corr Δ Γ' env' := by
  cases ht with
  | int h => cases hu with
    | atom hu => cases hu; exact .inr ⟨_, _, rfl, .scalar (.int h), (fun _ _ h => by cases h), hc⟩
  | bool => cases hu with
    | atom hu => cases hu; exact .inr ⟨_, _, rfl, .scalar .bool, (fun _ _ h => by cases h), hc⟩
  | copy hx =>
    cases hu with
    | atom hu =>
      cases hu with
      | copy hf hl hcp =>
        rw [tys_lookup, hf] at hx
        simp only [Option.map_some, Option.some.injEq] at hx
        subst hx
        obtain ⟨v, env', hr, hv, hkf, hc'⟩ :=
          readVar_safe (mv := false) hc hf hl (by simpa using hcp)
        exact .inr ⟨v, env', by simpa [evalExpr] using hr, hv,
          by simpa [knownFields, knownFieldsAtom, hf] using hkf, hc'⟩
  | move hx =>
    cases hu with
    | atom hu =>
      cases hu with
      | move hf hl hcp =>
        rw [tys_lookup, hf] at hx
        simp only [Option.map_some, Option.some.injEq] at hx
        subst hx
        obtain ⟨v, env', hr, hv, hkf, hc'⟩ :=
          readVar_safe (mv := true) hc hf hl (by simpa using hcp)
        exact .inr ⟨v, env', by simpa [evalExpr] using hr, hv,
          by simpa [knownFields, knownFieldsAtom, hf] using hkf, hc'⟩
  | add hl hr =>
    rename_i l r t
    cases hu with
    | add hul hur =>
      rename_i Γ₁
      obtain ⟨v₁, env₁, he₁, hs₁, hc₁⟩ := evalAtom_safe hc hl hul
      rw [← useAtom_tys hul] at hr
      obtain ⟨v₂, env₂, he₂, hs₂, hc₂⟩ := evalAtom_safe hc₁ hr hur
      cases hs₁ with
      | int hin₁ =>
        rename_i a
        cases hs₂ with
        | int hin₂ =>
          rename_i b
          by_cases hsum : t.inRange (a + b) = true
          · exact .inr ⟨_, env₂, by simp [evalExpr, he₁, he₂, checkedAdd, hsum],
              .scalar (.int hsum), (fun _ _ h => by cases h), hc₂⟩
          · exact .inl ⟨.arithmeticOverflow, by simp [evalExpr, he₁, he₂, checkedAdd, hsum]⟩
  | mk hd hf =>
    rename_i s d fs
    cases hu with
    | mk hu =>
      have htyped : ∀ p ∈ fs, ∃ st, AtomTy Γ.tys p.2 st := by
        intro p hp
        obtain ⟨st, _, h⟩ := hf.typed p hp
        exact ⟨st, h⟩
      obtain ⟨vs, env', he, hc', hl, hk⟩ := evalFields_safe hc htyped hu
      refine .inr ⟨.struct s vs, env', by simp [evalExpr, he], .struct hd ?_, ?_, hc'⟩
      · intro f st hfs
        obtain ⟨a, ha⟩ := hf.covers f st hfs
        obtain ⟨st', hst', hat⟩ := hf.typed (f, a) (AList.lookup_mem ha)
        rw [hfs] at hst'
        cases hst'
        exact hl f a ha st hat
      · intro sn vs' hv f sv hff
        cases hv
        simp only [knownFields, fieldFact, AList.lookup_map] at hff
        cases hla : AList.lookup fs f with
        | none => simp [hla] at hff
        | some a =>
          simp only [hla, Option.map_some, Option.bind_some, id] at hff
          exact hk f a hla sv hff

/-- A known expression evaluates to its known value (Q6.2): the static table agrees with the
evaluator. -/
theorem knownExpr_sound {Δ : Decls} {Γ : Ctx} {env : Env} {e : Expr} {s : Scalar}
    (hc : Corr Δ Γ env) (hk : knownExpr Γ e = some s) : evalExpr Δ env e = .ok (.scalar s, env) := by
  cases e with
  | atom a =>
    have hk' : knownAtom Γ a = some s := hk
    have h := knownAtom_sound hc hk'
    cases a with
    | int t n => simp [knownAtom] at hk'; subst hk'; rfl
    | bool b => simp [knownAtom] at hk'; subst hk'; rfl
    | move x => simp [knownAtom] at hk'
    | copy x =>
      simp only [evalAtom] at h
      simp only [evalExpr]
      split at h <;> simp_all
  | add l r =>
    simp only [knownExpr] at hk
    split at hk
    · rename_i t a t' b hl hr
      split at hk
      · rename_i hcond
        cases hk
        obtain ⟨rfl, hin⟩ := hcond
        simp [evalExpr, knownAtom_sound hc hl, knownAtom_sound hc hr, checkedAdd, hin]
      · cases hk
    · cases hk
  | mk s fs => simp [knownExpr] at hk

/-! ### No hidden traps -/

/-- Reading a local never traps. -/
theorem readVar_ne_trap (Δ : Decls) (env : Env) (x : Var) (mv : Bool) (k : Trap) :
    readVar Δ env x mv ≠ .trap k := by
  unfold readVar; split <;> (try split) <;> simp

/-- Reading an atom never traps. -/
theorem evalAtom_ne_trap (Δ : Decls) (env : Env) (a : Atom) (k : Trap) : evalAtom Δ env a ≠ .trap k := by
  cases a with
  | int => simp [evalAtom]
  | bool => simp [evalAtom]
  | copy x =>
    simp only [evalAtom]
    have := readVar_ne_trap Δ env x false k
    split <;> simp_all
  | move x =>
    simp only [evalAtom]
    have := readVar_ne_trap Δ env x true k
    split <;> simp_all

/-- Struct-literal fields never trap: they are atoms. -/
theorem evalFields_ne_trap (Δ : Decls) (k : Trap) :
    ∀ (env : Env) (fs : List (Name × Atom)), evalFields Δ env fs ≠ .trap k
  | env, [] => by simp [evalFields]
  | env, (f, a) :: rest => by
    simp only [evalFields]
    have h₁ := evalAtom_ne_trap Δ env a k
    split
    · rename_i v env₁ _
      have h₂ := evalFields_ne_trap Δ k env₁ rest
      split <;> simp_all
    · simp_all
    · simp

/-- Recording a read changes no binding's bounds. -/
theorem atomBounds_mark (y : Var) (mv : Bool) (a : Atom) (Γ : Ctx) :
    atomBounds (ctxMark Γ y mv) a = atomBounds Γ a := by
  cases a with
  | copy x =>
    simp only [atomBounds]
    rw [ctxMark, AList.lookup_update]
    by_cases h : x = y
    · subst h; rw [ite_eq_left rfl]; cases AList.lookup Γ x <;> rfl
    · rw [ite_eq_right h]
  | _ => rfl

theorem useAtom_atomBounds {Δ : Decls} {Γ Γ' : Ctx} {a : Atom} (h : UseAtom Δ Γ a Γ') (b : Atom) :
    atomBounds Γ' b = atomBounds Γ b := by
  cases h with
  | int => rfl
  | bool => rfl
  | copy => exact atomBounds_mark _ _ _ _
  | move => exact atomBounds_mark _ _ _ _

/-- **The bounds are sound.** An atom the checker bounds evaluates to an integer of the bounded
type within the bounds: a known value exactly, any other local within its type (it holds a value of
its type, `Corr`). -/
theorem atomBounds_sound {Δ : Decls} {Γ Γ' : Ctx} {env : Env} {a : Atom} {t : IntTy} {lo hi : Int}
    (hc : Corr Δ Γ env) (hu : UseAtom Δ Γ a Γ') (hb : atomBounds Γ a = some (t, lo, hi)) :
    ∃ n env', evalAtom Δ env a = .ok (.int t n, env') ∧ lo ≤ n ∧ n ≤ hi ∧ Corr Δ Γ' env' := by
  cases hu with
  | int =>
    simp only [atomBounds, Option.some.injEq, Prod.mk.injEq] at hb
    obtain ⟨rfl, rfl, rfl⟩ := hb
    exact ⟨_, _, rfl, Int.le_refl _, Int.le_refl _, hc⟩
  | bool => simp [atomBounds] at hb
  | move => simp [atomBounds] at hb
  | copy hf hl hcp =>
    rename_i x e
    obtain ⟨v, env', hr, hv, _, hc'⟩ := readVar_safe (mv := false) hc hf hl (by simpa using hcp)
    simp only [atomBounds, hf] at hb
    split at hb
    · rename_i t₀ t₁ n hty hkn
      simp only [Option.some.injEq, Prod.mk.injEq] at hb
      obtain ⟨rfl, rfl, rfl⟩ := hb
      obtain ⟨hsl, _⟩ := (corr_find hc hf).2.1 _ hkn
      have hr' : readVar Δ env x false = .ok (.scalar (.int t₁ n), env) := by
        simp [readVar, hsl, Val.isCopy]
      rw [hr] at hr'
      cases hr'
      rw [hty] at hv
      cases hv with
      | scalar hs => cases hs with
        | int _ => exact ⟨n, env, by simp [evalAtom, hr], Int.le_refl _, Int.le_refl _, hc'⟩
    · rename_i t₀ kn hty _
      simp only [Option.some.injEq, Prod.mk.injEq] at hb
      obtain ⟨rfl, rfl, rfl⟩ := hb
      rw [hty] at hv
      cases hv with
      | scalar hs => cases hs with
        | int hin =>
          rename_i n _
          simp only [IntTy.inRange, decide_eq_true_eq] at hin
          exact ⟨n, env', by simp [evalAtom, hr], hin.1, hin.2, hc'⟩
    · simp at hb

/-- **Admitted operations do not trap.** In any environment the checker's context describes, an
expression whose operation is admitted evaluates without a trap: its evidence holds. -/
theorem evalExpr_ne_trap {Δ : Decls} {e : Expr} {Γ Γ' : Ctx} {env : Env}
    (hc : Corr Δ Γ env) (hu : Uses Δ Γ e Γ') (ha : admitted Γ e = true) (k : Trap) :
    evalExpr Δ env e ≠ .trap k := by
  cases e with
  | atom a =>
    cases a with
    | int => simp [evalExpr]
    | bool => simp [evalExpr]
    | copy x => exact readVar_ne_trap Δ env x false k
    | move x => exact readVar_ne_trap Δ env x true k
  | mk s fs =>
    simp only [evalExpr]
    have := evalFields_ne_trap Δ k env fs
    split <;> simp_all
  | add l r =>
    cases hu with
    | add hl hr =>
      simp only [admitted] at ha
      split at ha
      · rename_i t a b t' c d hbl hbr
        simp only [decide_eq_true_eq] at ha
        obtain ⟨rfl, hlo, hhi⟩ := ha
        obtain ⟨n₁, env₁, he₁, hl₁, hh₁, hc₁⟩ := atomBounds_sound hc hl hbl
        rw [← useAtom_atomBounds hl r] at hbr
        obtain ⟨n₂, env₂, he₂, hl₂, hh₂, _⟩ := atomBounds_sound hc₁ hr hbr
        have hin : t.inRange (n₁ + n₂) = true := by
          simp only [IntTy.inRange, decide_eq_true_eq]; constructor <;> omega
        simp [evalExpr, he₁, he₂, checkedAdd, hin]
      · cases ha

/-- Binding the fields of a struct never traps. -/
theorem destructure_ne_trap (k : Trap) :
    ∀ (pat : List (Name × Binder)) (vs : List (Name × Scalar)) (env : Env), destructure pat vs env ≠ .trap k
  | [], _, _ => by simp [destructure]
  | (_, .wild) :: rest, vs, env => by simpa [destructure] using destructure_ne_trap k rest vs env
  | (f, .name x) :: rest, vs, env => by
    simp only [destructure]
    split
    · exact destructure_ne_trap k rest vs _
    · simp

/-- A statement whose operation is admitted executes without a trap. -/
theorem execStmt_ne_trap {Δ : Decls} {Γ Γ' : Ctx} {env : Env} {s : Stmt}
    (hc : Corr Δ Γ env) (hs : StmtOk Δ Γ s Γ') (k : Trap) : execStmt Δ env s ≠ .trap k := by
  cases hs with
  | let_ _ ha hu _ =>
    have := evalExpr_ne_trap hc hu ha k
    simp only [execStmt]; split <;> simp_all
  | letWild _ ha _ hu =>
    have := evalExpr_ne_trap hc hu ha k
    simp only [execStmt]; split <;> simp_all
  | letStruct _ ha _ _ hu _ =>
    have := evalExpr_ne_trap hc hu ha k
    simp only [execStmt]; split <;> (try split) <;> simp_all [destructure_ne_trap]

theorem destructure_safe {Δ : Decls} {d : StructDecl} {vs : List (Name × Scalar)}
    {K : List (Name × Option Scalar)}
    (hvs : ∀ f st, AList.lookup d.fields f = some st → ∃ v, AList.lookup vs f = some v ∧ SVTy v st)
    (hK : FieldsHold K vs) :
    ∀ {pat : List (Name × Binder)} {Γ : Ctx} {env : Env}, Corr Δ Γ env →
      (∀ p ∈ pat, ∃ st, AList.lookup d.fields p.1 = some st) →
      ∃ env', destructure pat vs env = .ok env' ∧ Corr Δ (bindPat d K pat Γ) env'
  | [], Γ, env, hc, _ => ⟨env, rfl, hc⟩
  | (f, .wild) :: rest, Γ, env, hc, hp => by
    simp only [destructure, bindPat]
    exact destructure_safe hvs hK hc (fun p h => hp p (List.Mem.tail _ h))
  | (f, .name x) :: rest, Γ, env, hc, hp => by
    obtain ⟨st, hst⟩ := hp (f, .name x) (List.Mem.head _)
    obtain ⟨v, hv, hsv⟩ := hvs f st hst
    simp only [destructure, bindPat, hst, hv]
    exact destructure_safe hvs hK
      (Corr.cons
        (x := x) (e := { ty := .scalar st, live := true, used := false, known := fieldFact K f })
        (fun _ => ⟨_, rfl, .scalar hsv⟩)
        (by
          intro s hs
          have h := hK f s hs
          rw [hv] at h
          cases h
          exact ⟨rfl, rfl⟩)
        (by intro sn vs' h; cases h) hc)
      (fun p h => hp p (List.Mem.tail _ h))

/-- Statement safety: a checked statement executes or traps; it never gets stuck. -/
theorem execStmt_safe {Δ : Decls} {Γ Γ' : Ctx} {env : Env} {s : Stmt}
    (hc : Corr Δ Γ env) (hs : StmtOk Δ Γ s Γ') :
    (∃ k, execStmt Δ env s = .trap k) ∨ ∃ env', execStmt Δ env s = .ok env' ∧ Corr Δ Γ' env' := by
  cases hs with
  | let_ ht _ hu _ =>
    rename_i x t e
    rcases evalExpr_safe hc ht hu with ⟨k, hk⟩ | ⟨v, env₁, he, hv, hkf, hc₁⟩
    · exact .inl ⟨k, by simp [execStmt, hk]⟩
    · refine .inr ⟨_, by simp [execStmt, he], .cons (fun _ => ⟨v, rfl, hv⟩) ?_ ?_ hc₁⟩
      · intro s hs
        have hk := knownExpr_sound hc hs
        rw [he] at hk
        cases hk
        cases hv with | scalar _ => exact ⟨rfl, rfl⟩
      · intro sn vs h
        cases h
        exact hkf sn vs rfl
  | letWild ht _ _ hu =>
    rcases evalExpr_safe hc ht hu with ⟨k, hk⟩ | ⟨v, env₁, he, _, _, hc₁⟩
    · exact .inl ⟨k, by simp [execStmt, hk]⟩
    · exact .inr ⟨_, by simp [execStmt, he], hc₁⟩
  | letStruct ht _ hd hp hu _ =>
    rcases evalExpr_safe hc ht hu with ⟨k, hk⟩ | ⟨v, env₁, he, hv, hkf, hc₁⟩
    · exact .inl ⟨k, by simp [execStmt, hk]⟩
    · cases hv with
      | struct hd' hvs =>
        rw [hd] at hd'
        cases hd'
        obtain ⟨env', hde, hc'⟩ := destructure_safe hvs (hkf _ _ rfl) hc₁ hp.declared
        exact .inr ⟨env', by simp [execStmt, he, hde], hc'⟩

/-- An atom with a scalar type in `HasType` has that type in `AtomTy`. -/
theorem hasType_atom_scalar {Δ : Decls} {Γ : TyCtx} {a : Atom} {st : STy}
    (h : HasType Δ Γ (.atom a) (.scalar st)) : AtomTy Γ a st := by
  cases h with
  | int h => exact .int h
  | bool => exact .bool
  | copy h => exact .copy h
  | move h => exact .move h

/-- Body safety: from a described environment, a checked body never gets stuck, for any fuel. -/
theorem runBody_safe {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {r : Atom} (hb : BodyOk Δ Γ ss r) :
    ∀ {env : Env}, Corr Δ Γ env → ∀ n w, runBody Δ n env ss r ≠ .stuck w := by
  induction hb with
  | ret ht hu _ =>
    intro env hc n w
    cases n with
    | zero => simp [runBody]
    | succ n =>
      simp only [runBody, retOutcome]
      cases hu with
      | atom hu =>
        obtain ⟨v, env', he, hv, _⟩ := evalAtom_safe hc (hasType_atom_scalar ht) hu
        cases hv with | int _ => simp [he]
  | cons hs _ ih =>
    intro env hc n w
    cases n with
    | zero => simp [runBody]
    | succ n =>
      simp only [runBody]
      rcases execStmt_safe hc hs with ⟨k, hk⟩ | ⟨env', he, hc'⟩
      · simp [hk]
      · simp only [he]; exact ih hc' n w

/-- **Slice-1 safety.** An accepted program never gets stuck, with any amount of fuel: it never
reads a moved variable, never reads an unbound one, never reads in the wrong mode and never meets
a value of the wrong type (STMT-1: safe code never reaches a stuck state; OWN-1). -/
theorem checked_never_stuck {p : Program} (h : Checked p) : ∀ n w, run p n ≠ .stuck w :=
  runBody_safe h.2 .nil

/-- In particular an accepted program never reads a moved variable (OWN-1). -/
theorem checked_no_use_after_move {p : Program} (h : Checked p) (n : Nat) :
    run p n ≠ .stuck .movedRead :=
  checked_never_stuck h n _

/-- Body trap-freedom: from a described environment, a checked body never ends in a trap, for any
fuel. -/
theorem runBody_ne_trap {Δ : Decls} {Γ : Ctx} {ss : List Stmt} {r : Atom} (hb : BodyOk Δ Γ ss r) :
    ∀ {env : Env}, Corr Δ Γ env → ∀ n k, runBody Δ n env ss r ≠ .trap k := by
  induction hb with
  | ret =>
    rename_i r _ _ _
    intro env _ n k
    cases n with
    | zero => simp [runBody]
    | succ n =>
      have := evalAtom_ne_trap Δ env r k
      simp only [runBody, retOutcome]
      split <;> simp_all
  | cons hs _ ih =>
    intro env hc n k
    cases n with
    | zero => simp [runBody]
    | succ n =>
      simp only [runBody]
      have hnt := execStmt_ne_trap hc hs k
      rcases execStmt_safe hc hs with ⟨k', hk'⟩ | ⟨env', he, hc'⟩
      · exact absurd hk' (execStmt_ne_trap hc hs k')
      · simp only [he]; exact ih hc' n k

/-- **No hidden traps** (decision of 2026-11-06, Q1). An accepted program's run never ends in an
implicit trap, with any amount of fuel. Every trap of slice 1 is implicit: the only one is the
overflow of checked `+` (ARITH-3), and slice 1 has no explicit `abort()`. The Core semantics keeps
the trap outcome (`runBody` of an unchecked body can trap, `Examples.add100`); admission rules it
out. -/
theorem checked_no_implicit_traps {p : Program} (h : Checked p) : ∀ n k, run p n ≠ .trap k :=
  runBody_ne_trap h.2 .nil

/-- **Slice-1 behaviour.** An accepted program has exactly one outcome, and it is an exit
(STMT-1): it never gets stuck (`checked_never_stuck`) and never traps
(`checked_no_implicit_traps`). -/
theorem checked_exits {p : Program} (h : Checked p) :
    ∃ o, Exec p.decls [] p.body p.ret o ∧ ∃ c, o = .exit c := by
  obtain ⟨o, ho⟩ := exec_total p.decls [] p.body p.ret
  refine ⟨o, ho, ?_⟩
  have hfuel := exec_complete ho
  have hns := checked_never_stuck h (p.body.length + 1)
  have hnt := checked_no_implicit_traps h (p.body.length + 1)
  have hnf := exec_ne_outOfFuel ho
  unfold run at hns hnt
  rw [hfuel] at hns hnt
  cases o with
  | exit c => exact ⟨c, rfl⟩
  | trap k => exact absurd rfl (hnt k)
  | stuck w => exact absurd rfl (hns w)
  | outOfFuel => exact absurd rfl hnf

end ConcreteSpec
