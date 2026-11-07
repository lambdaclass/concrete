import ConcreteSpec.Proofs.Observe

/-!
# The slice-0 fragment: `slice0_exits`, `slice0_never_traps`, `toCore_injective`

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Slice0
open ConcreteSpec
open ConcreteSpec.Core (AList)

/-- The embedding is injective: a slice-1 program in slice 0 has exactly one slice-0 reading. -/
theorem Atom.toCore_injective {a b : Atom} (h : a.toCore = b.toCore) : a = b := by
  cases a <;> cases b <;> simp_all [Atom.toCore] <;> omega

theorem Expr.toCore_injective {a b : Expr} (h : a.toCore = b.toCore) : a = b := by
  cases a <;> cases b <;> simp_all [Expr.toCore] <;>
    first
    | exact Atom.toCore_injective h
    | exact ⟨Atom.toCore_injective h.1, Atom.toCore_injective h.2⟩

theorem Stmt.toCore_injective {a b : Stmt} (h : a.toCore = b.toCore) : a = b := by
  cases a; cases b
  simp only [Stmt.toCore, ConcreteSpec.Stmt.let_.injEq, Binder.name.injEq, true_and] at h
  obtain ⟨rfl, h⟩ := h
  rw [Expr.toCore_injective h]

theorem map_toCore_injective : ∀ {l l' : List Stmt}, l.map Stmt.toCore = l'.map Stmt.toCore → l = l'
  | [], [], _ => rfl
  | [], _ :: _, h => by simp at h
  | _ :: _, [], h => by simp at h
  | a :: l, b :: l', h => by
    simp only [List.map_cons, List.cons.injEq] at h
    rw [Stmt.toCore_injective h.1, map_toCore_injective h.2]

theorem toCore_injective {q q' : Program} (h : toCore q = toCore q') : q = q' := by
  cases q with
  | mk body ret =>
  cases q' with
  | mk body' ret' =>
  simp only [toCore, ConcreteSpec.Program.mk.injEq, true_and] at h
  obtain ⟨hb, hr⟩ := h
  have hb' : body = body' := map_toCore_injective hb
  rw [hb', Atom.toCore_injective hr]

/-- Every binding of the context is a `u8` with a known value. -/
def Known (Γ : Ctx) : Prop :=
  ∀ b ∈ Γ, b.2.ty = .scalar (.int .u8) ∧ ∃ n, b.2.known = some (.int .u8 n)

theorem known_mark {x : Var} {mv : Bool} : ∀ {Γ : Ctx}, Known Γ → Known (ctxMark Γ x mv)
  | [], h => h
  | (y, e) :: Γ, h => by
    simp only [ctxMark, AList.update]
    split
    · intro b hb
      cases hb with
      | head => exact h (y, e) (List.Mem.head _)
      | tail _ hb => exact h b (List.Mem.tail _ hb)
    · intro b hb
      cases hb with
      | head => exact h (y, e) (List.Mem.head _)
      | tail _ hb => exact known_mark (Γ := Γ) (fun b' h' => h b' (List.Mem.tail _ h')) b hb

theorem known_useAtom {Δ : Decls} {Γ Γ' : Ctx} {a : ConcreteSpec.Atom} (hk : Known Γ)
    (hu : UseAtom Δ Γ a Γ') : Known Γ' := by
  cases hu with
  | int => exact hk
  | bool => exact hk
  | copy => exact known_mark hk
  | move => exact known_mark hk

/-- A slice-0 atom that can be read has a known `u8` value. -/
theorem knownAtom_u8 {Δ : Decls} {Γ Γ' : Ctx} {a : Atom} (hk : Known Γ)
    (hu : UseAtom Δ Γ a.toCore Γ') :
    ∃ n, knownAtom Γ a.toCore = some (.int .u8 n) ∧ atomBounds Γ a.toCore = some (.u8, n, n) := by
  cases a with
  | lit n => exact ⟨n, rfl, rfl⟩
  | var x =>
    cases hu with
    | copy hf _ _ =>
      obtain ⟨ht, n, hn⟩ : _ ∧ ∃ n, _ := hk _ (AList.lookup_mem hf)
      simp only at ht hn
      exact ⟨n, by simp [Atom.toCore, knownAtom, hf, hn], by simp [Atom.toCore, atomBounds, hf, hn, ht]⟩

/-- A checked slice-0 expression has a known `u8` value: its operands are known exactly, so the
evidence that admits a sum (no hidden traps) says the sum fits. -/
theorem knownExpr_u8 {Δ : Decls} {Γ Γ' : Ctx} {e : Expr} (hk : Known Γ)
    (hu : Uses Δ Γ e.toCore Γ') (hv : admitted Γ e.toCore = true) :
    Known Γ' ∧ ∃ n, knownExpr Γ e.toCore = some (.int .u8 n) := by
  cases e with
  | atom a =>
    cases hu with
    | atom hu =>
      obtain ⟨n, hn, _⟩ := knownAtom_u8 hk hu
      exact ⟨known_useAtom hk hu, n, hn⟩
  | add l r =>
    cases hu with
    | add hl hr =>
      have hk₁ := known_useAtom hk hl
      obtain ⟨a, ha, ha'⟩ := knownAtom_u8 hk hl
      obtain ⟨b, hb, hb'⟩ := knownAtom_u8 hk₁ hr
      rw [useAtom_knownAtom hl] at hb
      rw [useAtom_atomBounds hl] at hb'
      refine ⟨known_useAtom hk₁ hr, a + b, ?_⟩
      simp only [Expr.toCore, admitted, ha', hb'] at hv
      simp only [Expr.toCore, knownExpr, ha, hb]
      simp_all [IntTy.inRange]

/-- A checked slice-0 body exits: from a described environment whose bindings all have known
`u8` values, it reaches `return` without a trap. -/
theorem body_exits {Δ : Decls} {r : Atom} :
    ∀ {ss : List Stmt} {Γ : Ctx} {env : Env}, BodyOk Δ Γ (ss.map Stmt.toCore) r.toCore →
      Corr Δ Γ env → Known Γ → ∃ c, runBody Δ (ss.length + 1) env (ss.map Stmt.toCore) r.toCore = .exit c
  | [], Γ, env, hb, hc, hk => by
    cases hb with
    | ret _ hu _ =>
      cases hu with
      | atom hu =>
        obtain ⟨n, hn, _⟩ := knownAtom_u8 hk hu
        exact ⟨n.toNat, by simp [runBody, retOutcome, knownAtom_sound hc hn]⟩
  | s :: ss, Γ, env, hb, hc, hk => by
    cases hb with
    | cons hs hb =>
      have hs' := hs
      simp only [Stmt.toCore] at hs'
      cases hs' with
      | let_ _ hv hu _ =>
        obtain ⟨hk₁, n, hn⟩ := knownExpr_u8 hk hu hv
        have he := knownExpr_sound hc hn
        rcases execStmt_safe hc hs with ⟨k, hx⟩ | ⟨env', hx, hc'⟩
        · simp [Stmt.toCore, execStmt, he] at hx
        · obtain ⟨c, h⟩ := body_exits hb hc' (by
            intro e' he'
            cases he' with
            | head => exact ⟨rfl, n, hn⟩
            | tail _ he' => exact hk₁ e' he')
          exact ⟨c, by simp only [List.length_cons, List.map_cons, runBody, hx]; exact h⟩

/-- **Slice-0 behaviour.** An accepted slice-0 program exits with a `u8` status. Every slice-0
value is statically known, so a `+` that could overflow is refused before it runs (Q6.1). -/
theorem slice0_exits {q : Program} (h : Checked (toCore q)) :
    ∃ c, c < 256 ∧ run (toCore q) (q.body.length + 1) = .exit c := by
  obtain ⟨c, hc⟩ := body_exits (r := q.ret) h.2 .nil (fun _ h => nomatch h)
  refine ⟨c, runBody_exit_u8 h.2 .nil _ c hc, ?_⟩
  simpa [run, toCore] using hc

/-- An accepted slice-0 program never traps, with any fuel: the first instance of
`checked_no_implicit_traps`. -/
theorem slice0_never_traps {q : Program} (h : Checked (toCore q)) (n : Nat) (k : Trap) :
    run (toCore q) n ≠ .trap k :=
  checked_no_implicit_traps h n k

end ConcreteSpec.Slice0
