import ConcreteSpec.Proofs.Basics

/-!
# Part 6: `sub_iff` and `authority_contained`

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Authority

/-- Helper (computation). Exhaustive enumeration of the finite capability carrier. (approved: REV-base1c-agy-01025713b). -/
theorem mem_all (c : Cap) : c ∈ Cap.all := by cases c <;> decide

/-- Law (subject `ConcreteSpec.Authority.CapExpr.Sub`). **The inclusion check is exactly inclusion for every value of the variables** (rule 3, Q5.3). -/
theorem sub_iff (e f : CapExpr) : e.Sub f ↔ ∀ ρ c, e.Mem ρ c → f.Mem ρ c := by
  have sound {e f : CapExpr} (h : e.Sub f) (ρ : Nat → List Cap) (c : Cap) :
      e.Mem ρ c → f.Mem ρ c := by
    rintro (hc | ⟨v, hv, hcv⟩)
    · exact .inl (h.1 c hc)
    · rcases h.2 v hv with hf | hall
      · exact .inr ⟨v, hf, hcv⟩
      · exact .inl (hall c (mem_all c))
  refine ⟨fun h ρ c => sound h ρ c, fun h => ⟨?_, ?_⟩⟩
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

/-- Helper (projection). Forward elimination of the complete inclusion equivalence; semantic evidence belongs to sub_iff. (approved: REV-base1c-agy-01025713b) -/
theorem sub_sound {e f : CapExpr} (h : e.Sub f) (ρ : Nat → List Cap) (c : Cap) :
    e.Mem ρ c → f.Mem ρ c := (sub_iff e f).mp h ρ c

/-- The value of the callee's variables at a call: variable v stands for what the argument
σ[v] contains under the caller's values ρ. -/
def inst (σ : List CapExpr) (ρ : Nat → List Cap) (v : Nat) : List Cap :=
  Cap.all.filter (fun c => decide ((σ.getD v ⟨[], []⟩).Mem ρ c))

/-- Law (subject `ConcreteSpec.Authority.CapExpr.subst`). -/
theorem mem_subst {σ : List CapExpr} {ρ : Nat → List Cap} {e : CapExpr} {c : Cap} :
    e.Mem (inst σ ρ) c → (e.subst σ).Mem ρ c := by
  rintro (hc | ⟨v, hv, hcv⟩)
  · exact .inl (List.mem_append_left _ hc)
  · simp only [inst, List.mem_filter, decide_eq_true_eq] at hcv
    rcases hcv.2 with hc | ⟨w, hw, hcw⟩
    · exact .inl (List.mem_append_right _ (List.mem_flatMap.mpr ⟨v, hv, hc⟩))
    · exact .inr ⟨w, List.mem_flatMap.mpr ⟨v, hv, hw⟩, hcw⟩

/-- Helper (inversion). A successful list search returns a member of that list. (approved: REV-base1c-agy-01025713b). -/
theorem find_mem {P : Prog} {h : String} {f : Fn} (hf : P.find h = some f) : f ∈ P :=
  List.mem_of_find?_eq_some hf

/-- Helper (inversion). Eliminate a variable-membership alternative forbidden by zero-parameter well-formedness. (approved: REV-base1c-agy-01025713b). A permitted function has no capability variables outside its parameters. -/
theorem mem_of_no_vars {e : CapExpr} (he : e.WellFormed 0) {ρ : Nat → List Cap} {c : Cap}
    (h : e.Mem ρ c) : c ∈ e.caps := by
  rcases h with hc | ⟨v, hv, _⟩
  · exact hc
  · exact absurd (he.2.2 v hv) (Nat.not_lt_zero v)

/-- Representation lemma. Successful positional lookup selects only input grants and commutes with mapping their types. (approved: REV-base1c-agy-01025713b). The grant arguments of a call that runs. -/
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

/-- Law (subject `ConcreteSpec.Authority.CapExpr.Equiv`). Set equality is exactly equality for every value of the variables (Q5.3). -/
theorem equiv_iff (e f : CapExpr) : e.Equiv f ↔ ∀ ρ c, e.Mem ρ c ↔ f.Mem ρ c := by
  constructor
  · rintro ⟨h1, h2⟩ ρ c; exact ⟨sub_sound h1 ρ c, sub_sound h2 ρ c⟩
  · intro h
    exact ⟨(sub_iff e f).mpr fun ρ c => (h ρ c).mp, (sub_iff f e).mpr fun ρ c => (h ρ c).mpr⟩

/-- Every produced event uses a bound grant of its declared kind and only permitted capabilities. -/
def TraceSafe (f : Fn) (ρ : Nat → List Cap) (env : List Nat) (gty : Nat → GrantTy) (tr : Trace) : Prop :=
  tr.events.all (fun e => decide (e.grant ∈ env ∧ gty e.grant = e.op.grant ∧
    ∀ c ∈ e.op.caps, f.perms.Mem ρ c)) = true

/-- Law (subject `ConcreteSpec.Authority.exec`). Every event of a run of permitted statements of f, finished or not, is an operation on one
of the grants bound to f's parameters, of the operation's grant type, and each capability it
needs is in f's permissions, for every value of f's capability variables. -/
theorem exec_contained {P : Prog} (hP : ∀ f ∈ P, FnOk P f) :
    ∀ (n : Nat) (f : Fn) (ss : List Stmt), (∀ s ∈ ss, StmtOk P f s) →
    ∀ (ρ : Nat → List Cap) (env : List Nat) (gty : Nat → GrantTy),
      env.map gty = f.grants → TraceSafe f ρ env gty (exec P n env ss) := by
  have contain : ∀ (n : Nat) (f : Fn) (ss : List Stmt), (∀ s ∈ ss, StmtOk P f s) →
    ∀ (ρ : Nat → List Cap) (env : List Nat) (gty : Nat → GrantTy),
      env.map gty = f.grants → ∀ e ∈ (exec P n env ss).events,
      e.grant ∈ env ∧ gty e.grant = e.op.grant ∧ ∀ c ∈ e.op.caps, f.perms.Mem ρ c := by
    intro n
    induction n with
    | zero => intro f ss _ ρ env gty _ e he; simp [exec] at he
    | succ n ih =>
      intro f ss hss ρ env gty henv e he
      match ss, hss, he with
      | [], _, he => simp [exec] at he
      | .prim op g :: rest, hss, he =>
        have hs := hss _ (List.mem_cons_self ..)
        simp only [StmtOk, PrimOk] at hs
        simp only [exec] at he
        cases hid : env[g]? with
        | none => simp [hid] at he
        | some id =>
          simp only [hid, List.mem_cons] at he
          rcases he with rfl | he
          · refine ⟨List.mem_of_getElem? hid, ?_, ?_⟩
            · have : (env.map gty)[g]? = some (gty id) := by simp [hid]
              rw [henv, hs.1] at this
              exact (Option.some.inj this).symm
            · intro c hc
              exact sub_sound hs.2 ρ c (.inl hc)
          · exact ih f rest (fun s hs' => hss s (List.mem_cons_of_mem _ hs')) ρ env gty henv e he
      | .call h' caps gs :: rest, hss, he =>
        have hs := hss _ (List.mem_cons_self ..)
        simp only [StmtOk, CallOk] at hs
        simp only [exec] at he
        cases hfind : P.find h' with
        | none => simp [hfind] at he
        | some callee =>
          obtain ⟨callee', hf', _, _, hgs, hsub⟩ := hs
          rw [hfind] at hf'
          cases hf'
          cases henv' : gs.mapM (fun i => env[i]?) with
          | none => simp [hfind, henv'] at he
          | some env' =>
            simp only [hfind, henv'] at he
            obtain ⟨hmem, htyp⟩ := mapM_args henv'
            have henv'ty : env'.map gty = callee.grants := by
              apply htyp gty callee.grants
              rw [henv]; exact hgs
            have hcallee := hP callee (find_mem hfind)
            have hcall : ∀ e ∈ (exec P n env' callee.body).events,
                e.grant ∈ env ∧ gty e.grant = e.op.grant ∧ ∀ c ∈ e.op.caps, f.perms.Mem ρ c := by
              intro e he
              obtain ⟨hg, hty, hc⟩ := ih callee callee.body hcallee.2 (inst caps ρ) env' gty
                henv'ty e he
              exact ⟨hmem _ hg, hty, fun c hcin => sub_sound hsub ρ c (mem_subst (hc c hcin))⟩
            split at he
            · rcases List.mem_append.mp he with he | he
              · exact hcall e he
              · exact ih f rest (fun s hs' => hss s (List.mem_cons_of_mem _ hs')) ρ env gty henv e he
            · exact hcall e he
  intro n f ss hss ρ env gty henv
  exact List.all_eq_true.mpr (fun e he => decide_eq_true (contain n f ss hss ρ env gty henv e he))

/-- Law (subject `ConcreteSpec.Authority.runMain`). **Authority containment.** In a permitted program, every event of a run of main, whether
or not the run finishes, is an operation on one of the grants the launcher minted for main's
parameters, of the operation's grant type, and every capability the operation needs is one that
main declares. -/
theorem authority_contained {P : Prog} (hP : ProgOk P) {m : Fn} (hm : P.find "main" = some m)
    {gty : Nat → GrantTy} {launch : List Nat} (hl : launch.map gty = m.grants) (fuel : Nat) :
    (runMain P fuel launch).events.all (fun e => decide (e.grant ∈ launch ∧
      gty e.grant = e.op.grant ∧ ∀ c ∈ e.op.caps, c ∈ m.perms.caps)) = true := by
  have contain : ∀ e ∈ (runMain P fuel launch).events,
      e.grant ∈ launch ∧ gty e.grant = e.op.grant ∧ ∀ c ∈ e.op.caps, c ∈ m.perms.caps := by
    obtain ⟨_, hall, m', hm', h0⟩ := hP
    rw [hm] at hm'; cases hm'
    have hok := hall m (find_mem hm)
    intro e he
    simp only [runMain, hm] at he
    have ht := exec_contained hall fuel m m.body hok.2 (fun _ => []) launch gty hl
    obtain ⟨hg, hty, hc⟩ := of_decide_eq_true ((List.all_eq_true.mp ht) e he)
    refine ⟨hg, hty, fun c hcin => mem_of_no_vars ?_ (hc c hcin)⟩
    rw [← h0]; exact hok.1
  exact List.all_eq_true.mpr (fun e he => decide_eq_true (contain e he))

namespace Evidence

def denyInclusion : CapExpr → CapExpr → Prop := fun _ _ => False
def denyEquivalence : CapExpr → CapExpr → Prop := fun _ _ => False
def loseSubstitution : List CapExpr → CapExpr → CapExpr := fun _ _ => ⟨[], []⟩
def forgedExec : Prog → Nat → List Nat → List Stmt → Trace := fun _ _ _ _ => ⟨[⟨.writeStdout, 99⟩], true⟩
def forgedMain : Prog → Nat → List Nat → Trace := fun _ _ _ => ⟨[⟨.writeStdout, 99⟩], true⟩

def mainFn : Fn := { logFn with name := "main" }

/-- Instance (law `ConcreteSpec.Authority.sub_iff`, case `variable`). -/
example := sub_iff (⟨[], [0]⟩ : CapExpr) ⟨[], [0]⟩

/-- Planted (law `ConcreteSpec.Authority.sub_iff`, case `variable`, subject `ConcreteSpec.Authority.CapExpr.Sub`, mutant `ConcreteSpec.Authority.Evidence.denyInclusion`). -/
example : ¬ (denyInclusion ⟨[], [0]⟩ ⟨[], [0]⟩ ↔
    ∀ ρ c, (⟨[], [0]⟩ : CapExpr).Mem ρ c → (⟨[], [0]⟩ : CapExpr).Mem ρ c) := by
  simp [denyInclusion]

/-- Instance (law `ConcreteSpec.Authority.equiv_iff`, case `variable`). -/
example := equiv_iff (⟨[], [0]⟩ : CapExpr) ⟨[], [0]⟩

/-- Planted (law `ConcreteSpec.Authority.equiv_iff`, case `variable`, subject `ConcreteSpec.Authority.CapExpr.Equiv`, mutant `ConcreteSpec.Authority.Evidence.denyEquivalence`). -/
example : ¬ (denyEquivalence ⟨[], [0]⟩ ⟨[], [0]⟩ ↔
    ∀ ρ c, (⟨[], [0]⟩ : CapExpr).Mem ρ c ↔ (⟨[], [0]⟩ : CapExpr).Mem ρ c) := by
  simp [denyEquivalence]

/-- Instance (law `ConcreteSpec.Authority.mem_subst`, case `concreteArgument`). -/
example := @mem_subst [console] (fun _ => []) ⟨[], [0]⟩ .console
  (by simp [CapExpr.Mem, inst, console, Cap.all])

/-- Planted (law `ConcreteSpec.Authority.mem_subst`, case `concreteArgument`, subject `ConcreteSpec.Authority.CapExpr.subst`, mutant `ConcreteSpec.Authority.Evidence.loseSubstitution`). -/
example : (⟨[], [0]⟩ : CapExpr).Mem (inst [console] (fun _ => [])) .console ∧
    ¬ (loseSubstitution [console] ⟨[], [0]⟩).Mem (fun _ => []) .console := by
  constructor <;> simp [CapExpr.Mem, inst, console, Cap.all, loseSubstitution]

/-- Instance (law `ConcreteSpec.Authority.exec_contained`, case `stdout`). -/
example := @exec_contained [logFn] (by decide) 2 logFn logFn.body (by decide)
  (fun _ => []) [7] (fun _ => .stdout) rfl

/-- Planted (law `ConcreteSpec.Authority.exec_contained`, case `stdout`, subject `ConcreteSpec.Authority.exec`, mutant `ConcreteSpec.Authority.Evidence.forgedExec`). -/
example : (∀ f ∈ [logFn], FnOk [logFn] f) ∧
    (∀ s ∈ logFn.body, StmtOk [logFn] logFn s) ∧
    [7].map (fun _ => GrantTy.stdout) = logFn.grants ∧
    ¬ TraceSafe logFn (fun _ => []) [7] (fun _ => .stdout) (forgedExec [logFn] 2 [7] logFn.body) := by
  refine ⟨by decide, by decide, rfl, ?_⟩
  unfold TraceSafe
  decide

/-- Instance (law `ConcreteSpec.Authority.authority_contained`, case `stdout`). -/
example := @authority_contained [mainFn] (by decide) mainFn rfl (fun _ => .stdout) [7] rfl 2

/-- Planted (law `ConcreteSpec.Authority.authority_contained`, case `stdout`, subject `ConcreteSpec.Authority.runMain`, mutant `ConcreteSpec.Authority.Evidence.forgedMain`). -/
example : ProgOk [mainFn] ∧ Prog.find [mainFn] "main" = some mainFn ∧
    [7].map (fun _ => GrantTy.stdout) = mainFn.grants ∧
    ¬ ((forgedMain [mainFn] 2 [7]).events.all (fun e => decide (e.grant ∈ [7] ∧
      (fun _ => GrantTy.stdout) e.grant = e.op.grant ∧ ∀ c ∈ e.op.caps, c ∈ mainFn.perms.caps)) = true) := by
  refine ⟨by decide, rfl, rfl, ?_⟩
  decide

end Evidence

end ConcreteSpec.Authority
