import ConcreteSpec.Proofs.Basics

/-!
# Part 6: `sub_iff` and `authority_contained`

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Authority

theorem mem_all (c : Cap) : c ∈ Cap.all := by cases c <;> decide

/-- The inclusion check is sound: what `e` contains, `f` contains, for every value of the
variables. -/
theorem sub_sound {e f : CapExpr} (h : e.Sub f) (ρ : Nat → List Cap) (c : Cap) :
    e.Mem ρ c → f.Mem ρ c := by
  rintro (hc | ⟨v, hv, hcv⟩)
  · exact .inl (h.1 c hc)
  · rcases h.2 v hv with hf | hall
    · exact .inr ⟨v, hf, hcv⟩
    · exact .inl (hall c (mem_all c))

/-- **The inclusion check is exactly inclusion for every value of the variables** (rule 3, Q5.3). -/
theorem sub_iff (e f : CapExpr) : e.Sub f ↔ ∀ ρ c, e.Mem ρ c → f.Mem ρ c := by
  refine ⟨fun h ρ c => sub_sound h ρ c, fun h => ⟨?_, ?_⟩⟩
  · intro c hc
    rcases h (fun _ => []) c (.inl hc) with hf | ⟨_, _, hn⟩
    · exact hf
    · cases hn
  · intro v hv
    by_cases hvf : v ∈ f.vars
    · exact .inl hvf
    · refine .inr fun c _ => ?_
      rcases h (fun w => if w = v then [c] else []) c (.inr ⟨v, hv, by simp⟩) with hf | ⟨w, hw, hcw⟩
      · exact hf
      · by_cases hwv : w = v
        · subst hwv; exact absurd hw hvf
        · simp [hwv] at hcw

/-- The value of the callee's variables at a call: variable `v` stands for what the argument
`σ[v]` contains under the caller's values `ρ`. -/
def inst (σ : List CapExpr) (ρ : Nat → List Cap) (v : Nat) : List Cap :=
  Cap.all.filter (fun c => decide ((σ.getD v ⟨[], []⟩).Mem ρ c))

theorem mem_subst {σ : List CapExpr} {ρ : Nat → List Cap} {e : CapExpr} {c : Cap} :
    e.Mem (inst σ ρ) c → (e.subst σ).Mem ρ c := by
  rintro (hc | ⟨v, hv, hcv⟩)
  · exact .inl (List.mem_append_left _ hc)
  · simp only [inst, List.mem_filter, decide_eq_true_eq] at hcv
    rcases hcv.2 with hc | ⟨w, hw, hcw⟩
    · exact .inl (List.mem_append_right _ (List.mem_flatMap.mpr ⟨v, hv, hc⟩))
    · exact .inr ⟨w, List.mem_flatMap.mpr ⟨v, hv, hw⟩, hcw⟩

theorem find_mem {P : Prog} {h : String} {f : Fn} (hf : P.find h = some f) : f ∈ P :=
  List.mem_of_find?_eq_some hf

/-- A permitted function has no capability variables outside its parameters. -/
theorem mem_of_no_vars {e : CapExpr} (he : e.WellFormed 0) {ρ : Nat → List Cap} {c : Cap}
    (h : e.Mem ρ c) : c ∈ e.caps := by
  rcases h with hc | ⟨v, hv, _⟩
  · exact hc
  · exact absurd (he.2.2 v hv) (Nat.not_lt_zero v)

/-- The grant arguments of a call that runs. -/
theorem mapM_args {env : List Nat} : ∀ {gs : List Nat} {env' : List Nat},
    gs.mapM (fun i => env[i]?) = some env' →
    (∀ x ∈ env', x ∈ env) ∧ ∀ (gty : Nat → GrantTy) (cg : List GrantTy),
      gs.map (fun i => (env.map gty)[i]?) = cg.map some → env'.map gty = cg
  | [], env', h => by
    simp at h; subst h
    constructor
    · intro x hx; simp at hx
    · intro gty cg hg
      cases cg with
      | nil => rfl
      | cons _ _ => simp at hg
  | i :: is, env', h => by
    simp only [List.mapM_cons, Option.bind_eq_bind] at h
    cases hi : env[i]? with
    | none => simp [hi] at h
    | some x =>
      cases his : is.mapM (fun i => env[i]?) with
      | none => simp [hi, his] at h
      | some xs =>
        simp [hi, his] at h
        subst h
        obtain ⟨ihm, iht⟩ := mapM_args his
        refine ⟨fun y hy => ?_, fun gty cg hg => ?_⟩
        · rcases List.mem_cons.mp hy with rfl | hy
          · exact List.mem_of_getElem? hi
          · exact ihm y hy
        · cases cg with
          | nil => simp at hg
          | cons c cs =>
            simp only [List.map_cons, List.cons.injEq] at hg
            have hx : (env.map gty)[i]? = some (gty x) := by simp [hi]
            rw [hx] at hg
            simp only [List.map_cons, List.cons.injEq]
            exact ⟨Option.some.inj hg.1, iht gty cs hg.2⟩

/-- Every event of a run of permitted statements of `f` is an operation on one of the grants
bound to `f`'s parameters, of the operation's grant type, and each capability it needs is in
`f`'s permissions, for every value of `f`'s capability variables. -/
theorem exec_contained {P : Prog} (hP : ∀ f ∈ P, FnOk P f) :
    ∀ (n : Nat) (f : Fn) (ss : List Stmt), (∀ s ∈ ss, StmtOk P f s) →
    ∀ (ρ : Nat → List Cap) (env : List Nat) (gty : Nat → GrantTy),
      env.map gty = f.grants → ∀ tr, exec P n env ss = some tr →
      ∀ e ∈ tr, e.grant ∈ env ∧ gty e.grant = e.op.grant ∧ ∀ c ∈ e.op.caps, f.perms.Mem ρ c := by
  intro n
  induction n with
  | zero => intro f ss _ ρ env gty _ tr h; simp [exec] at h
  | succ n ih =>
    intro f ss hss ρ env gty henv tr h
    match ss, hss, h with
    | [], _, h =>
      simp [exec] at h; subst h; intro e he; cases he
    | .prim op g :: rest, hss, h =>
      simp only [exec, Option.bind_eq_bind] at h
      cases hid : env[g]? with
      | none => simp [hid] at h
      | some id =>
        cases hrest : exec P n env rest with
        | none => simp [hid, hrest] at h
        | some tr' =>
          simp [hid, hrest] at h
          subst h
          have hs := hss _ (List.mem_cons_self ..)
          simp only [StmtOk, PrimOk] at hs
          intro e he
          rcases List.mem_cons.mp he with rfl | he
          · refine ⟨List.mem_of_getElem? hid, ?_, ?_⟩
            · have : (env.map gty)[g]? = some (gty id) := by simp [hid]
              rw [henv, hs.1] at this
              exact (Option.some.inj this).symm
            · intro c hc
              exact sub_sound hs.2 ρ c (.inl hc)
          · exact ih f rest (fun s hs' => hss s (List.mem_cons_of_mem _ hs')) ρ env gty henv tr'
              hrest e he
    | .call h' caps gs :: rest, hss, h =>
      simp only [exec, Option.bind_eq_bind] at h
      have hs := hss _ (List.mem_cons_self ..)
      simp only [StmtOk, CallOk] at hs
      cases hfind : P.find h' with
      | none => simp [hfind] at h
      | some callee =>
        obtain ⟨callee', hf', _, _, hgs, hsub⟩ := hs
        rw [hfind] at hf'
        cases hf'
        cases henv' : gs.mapM (fun i => env[i]?) with
        | none => simp [hfind, henv'] at h
        | some env' =>
          cases h1 : exec P n env' callee.body with
          | none => simp [hfind, henv', h1] at h
          | some t₁ =>
            cases h2 : exec P n env rest with
            | none => simp [hfind, henv', h2] at h
            | some t₂ =>
              simp [hfind, henv', h1, h2] at h
              subst h
              have hcallee := hP callee (find_mem hfind)
              obtain ⟨hmem, htyp⟩ := mapM_args henv'
              have henv'ty : env'.map gty = callee.grants := by
                apply htyp gty callee.grants
                rw [henv]; exact hgs
              have hsub' := sub_sound hsub
              intro e he
              rcases List.mem_append.mp he with he | he
              · obtain ⟨hg, hty, hc⟩ := ih callee callee.body hcallee.2 (inst caps ρ) env' gty
                  henv'ty t₁ h1 e he
                exact ⟨hmem _ hg, hty, fun c hcin => hsub' ρ c (mem_subst (hc c hcin))⟩
              · exact ih f rest (fun s hs' => hss s (List.mem_cons_of_mem _ hs')) ρ env gty henv t₂
                  h2 e he

/-- **Authority containment.** In a permitted program, every event of a run of `main` is an
operation on one of the grants the launcher minted for `main`'s parameters, of the operation's
grant type, and every capability the operation needs is one that `main` declares. -/
theorem authority_contained {P : Prog} (hP : ProgOk P) {m : Fn} (hm : P.find "main" = some m)
    {gty : Nat → GrantTy} {launch : List Nat} (hl : launch.map gty = m.grants) {fuel : Nat}
    {tr : List Event} (hr : runMain P fuel launch = some tr) :
    ∀ e ∈ tr, e.grant ∈ launch ∧ gty e.grant = e.op.grant ∧ ∀ c ∈ e.op.caps, c ∈ m.perms.caps := by
  obtain ⟨_, hall, m', hm', h0⟩ := hP
  rw [hm] at hm'; cases hm'
  have hok := hall m (find_mem hm)
  simp only [runMain, hm, Option.bind_eq_bind, Option.bind_some] at hr
  intro e he
  obtain ⟨hg, hty, hc⟩ := exec_contained hall fuel m m.body hok.2 (fun _ => []) launch gty hl tr hr e he
  refine ⟨hg, hty, fun c hcin => mem_of_no_vars ?_ (hc c hcin)⟩
  rw [← h0]; exact hok.1

end ConcreteSpec.Authority
