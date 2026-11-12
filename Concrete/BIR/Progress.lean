import Concrete.BIR

/-!
# BIR progress: a well-formed program never gets stuck

PROVISIONAL (slice 0; see `Concrete/README.md`, "The `adapter_correct` proof, in layers").

`Block.wf` checks each operand against the registers defined earlier in its own block (`scope`), so
the invariant is local to a block: every register `scope` types is bound in the environment to a
value of that type, an integer inside its type's range (`Typed`). Instructions extend it with their
results, in the order `Block.wf` extends `scope`; a block's entry establishes it for the parameters
alone (block arguments are evaluated in the predecessor's scope, and the registers are distinct,
so the newest binding of a parameter is its argument). Then no `runBlock` case is `stuck`: an exit
is a `u8` value (below 256), a trap is at a site of the table, every branch target exists with the
right arity, and every `condbr` reads a flag.

`run_progress`: for `WF p`, every `run p fuel` is either `outOfFuel` or an outcome the result word
encodes (`Outcome.Encodable`). With `behaviour_diverges_iff` (no WF needed), the behaviour is the
encodable outcome some fuel reaches, or divergence.
-/

open ConcreteSpec.Core (AList)

namespace Concrete.BIR
open ConcreteSpec (IntTy)

/-- `v` is a value of type `ty`: an integer of the type and inside its range, or a flag. -/
def Val.HasTy : Val → Ty → Prop
  | .int t x, ty => ty = .int t ∧ t.inRange x = true
  | .bool _, ty => ty = .bool

/-- Every register typed in `scope` is bound in `env` to a value of that type.
For example, a boolean register in the scope must have a boolean value in the environment. -/
def Typed (scope : List (Reg × Ty)) (env : Env) : Prop :=
  ∀ r ty, AList.lookup scope r = some ty → ∃ v, AList.lookup env r = some v ∧ v.HasTy ty

/-- An empty typing scope imposes no requirements on the environment. For example, it accepts an empty environment. -/
theorem typed_nil (env : Env) : Typed [] env := by
  intro r ty h
  simp [AList.lookup] at h

/-- Adding a correctly typed first binding preserves typing. For example, binding a boolean register in both lists preserves `Typed`. -/
theorem typed_cons {scope : List (Reg × Ty)} {env : Env} (h : Typed scope env) (r : Reg) {v : Val}
    {ty : Ty} (hv : v.HasTy ty) : Typed ((r, ty) :: scope) ((r, v) :: env) := by
  intro r' ty' h'
  by_cases hr : r = r'
  · subst hr
    simp [AList.lookup] at h'
    subst h'
    exact ⟨v, by simp [AList.lookup], hv⟩
  · simp only [AList.lookup, hr, ↓reduceIte] at h'
    obtain ⟨v', hv', ht'⟩ := h r' ty' h'
    exact ⟨v', by simp [AList.lookup, hr, hv'], ht'⟩

theorem int_of_hasTy {v : Val} {t : IntTy} (h : v.HasTy (.int t)) :
    ∃ x, v = .int t x ∧ t.inRange x = true := by
  cases v with
  | int t' x =>
    simp only [Val.HasTy, Ty.int.injEq] at h
    obtain ⟨rfl, hx⟩ := h
    exact ⟨x, rfl, hx⟩
  | bool b => simp [Val.HasTy] at h

theorem bool_of_hasTy {v : Val} (h : v.HasTy .bool) : ∃ b, v = .bool b := by
  cases v with
  | int t x => simp [Val.HasTy] at h
  | bool b => exact ⟨b, rfl⟩

/-- A well-typed operand evaluates, to a value of its type. -/
theorem evalOp_typed {scope : List (Reg × Ty)} {env : Env} (h : Typed scope env) {ty : Ty}
    {a : Operand} (ha : operandHas scope ty a = true) :
    ∃ v, evalOp env a = some v ∧ v.HasTy ty := by
  cases a with
  | reg r =>
    simp only [operandHas, beq_iff_eq] at ha
    exact h r ty ha
  | imm t x =>
    simp only [operandHas, Bool.and_eq_true, beq_iff_eq] at ha
    obtain ⟨rfl, hx⟩ := ha
    exact ⟨.int t x, by simp [evalOp, hx], rfl, hx⟩

theorem evalOp_int {scope : List (Reg × Ty)} {env : Env} (h : Typed scope env) {t : IntTy}
    {a : Operand} (ha : operandHas scope (.int t) a = true) :
    ∃ x, evalOp env a = some (.int t x) ∧ t.inRange x = true := by
  obtain ⟨v, hv, ht⟩ := evalOp_typed h ha
  obtain ⟨x, rfl, hx⟩ := int_of_hasTy ht
  exact ⟨x, hv, hx⟩

theorem intArgs_typed {scope : List (Reg × Ty)} {env : Env} (h : Typed scope env) {t : IntTy}
    {a b : Operand} (ha : operandHas scope (.int t) a = true)
    (hb : operandHas scope (.int t) b = true) : ∃ x y, intArgs env t a b = some (x, y) := by
  obtain ⟨x, hx, -⟩ := evalOp_int h ha
  obtain ⟨y, hy, -⟩ := evalOp_int h hb
  exact ⟨x, y, by simp [intArgs, hx, hy]⟩

/-- A well-formed instruction executes, and its results extend the invariant as `Block.wf`
extends the scope. -/
theorem execInst_typed {scope : List (Reg × Ty)} {env : Env} (h : Typed scope env) {i : Inst}
    (hi : Inst.wf scope i = true) :
    ∃ env', execInst env i = some env' ∧ Typed (i.defs.reverse ++ scope) env' := by
  cases i with
  | bin op t d a b =>
    simp only [Inst.wf, Bool.and_eq_true] at hi
    obtain ⟨x, y, hxy⟩ := intArgs_typed h hi.1 hi.2
    exact ⟨(d, .int t (wrap t (op.exact x y))) :: env, by simp [execInst, hxy],
      typed_cons h d (v := .int t (wrap t (op.exact x y))) ⟨rfl, wrap_inRange t _⟩⟩
  | binOvf op t d f a b =>
    simp only [Inst.wf, Bool.and_eq_true] at hi
    obtain ⟨x, y, hxy⟩ := intArgs_typed h hi.1 hi.2
    exact ⟨(f, .bool (!t.inRange (op.exact x y))) :: (d, .int t (wrap t (op.exact x y))) :: env,
      by simp [execInst, hxy],
      typed_cons (typed_cons h d (v := .int t (wrap t (op.exact x y))) ⟨rfl, wrap_inRange t _⟩) f
        (v := .bool (!t.inRange (op.exact x y))) rfl⟩
  | trunc fr to d a =>
    simp only [Inst.wf] at hi
    obtain ⟨x, hx, -⟩ := evalOp_int h hi
    exact ⟨(d, .int to (wrap to x)) :: env, by simp [execInst, hx],
      typed_cons h d (v := .int to (wrap to x)) ⟨rfl, wrap_inRange to _⟩⟩

/-- `Block.wf`'s fold step: the instructions so far are well formed, and the scope after them. -/
def wfStep (acc : Bool × List (Reg × Ty)) (i : Inst) : Bool × List (Reg × Ty) :=
  (acc.1 && Inst.wf acc.2 i, i.defs.reverse ++ acc.2)

theorem foldl_wfStep_false (l : List Inst) (s : List (Reg × Ty)) :
    (l.foldl wfStep (false, s)).1 = false := by
  induction l generalizing s with
  | nil => rfl
  | cons i l ih => exact ih _

theorem execInsts_typed :
    ∀ (l : List Inst) (scope : List (Reg × Ty)) (env : Env), Typed scope env →
      (l.foldl wfStep (true, scope)).1 = true →
      ∃ env', execInsts env l = some env' ∧ Typed (l.foldl wfStep (true, scope)).2 env'
  | [], _, env, h, _ => ⟨env, rfl, h⟩
  | i :: l, scope, env, h, hf => by
    simp only [List.foldl] at hf ⊢
    cases hi : Inst.wf scope i with
    | false =>
      have hs : wfStep (true, scope) i = (false, i.defs.reverse ++ scope) := by
        simp [wfStep, hi]
      rw [hs, foldl_wfStep_false] at hf
      exact absurd hf (by decide)
    | true =>
      have hs : wfStep (true, scope) i = (true, i.defs.reverse ++ scope) := by
        simp [wfStep, hi]
      rw [hs] at hf ⊢
      obtain ⟨env', hx, ht⟩ := execInst_typed h hi
      obtain ⟨env'', hx', ht'⟩ := execInsts_typed l _ env' ht hf
      exact ⟨env'', by simp [execInsts, hx, hx'], ht'⟩

/-- A well-formed block's instructions execute from its entry invariant, and its terminator is
well formed in the scope after them, which the environment then satisfies. -/
theorem block_typed {p : Program} {b : Block} (hb : Block.wf p b = true) {env : Env}
    (h : Typed b.params.reverse env) :
    ∃ env' scope, execInsts env b.insts = some env' ∧ Typed scope env' ∧
      b.term.wf p scope = true := by
  have hb' : ((b.insts.foldl wfStep (true, b.params.reverse)).1 &&
      b.term.wf p (b.insts.foldl wfStep (true, b.params.reverse)).2) = true := hb
  rw [Bool.and_eq_true] at hb'
  obtain ⟨env', hx, ht⟩ := execInsts_typed b.insts _ env h hb'.1
  exact ⟨env', _, hx, ht, hb'.2⟩

/-! ## Block entry -/

/-- Values fitting a parameter list, one for one. -/
inductive Fits : List (Reg × Ty) → List Val → Prop
  | nil : Fits [] []
  | cons {r : Reg} {ty : Ty} {v : Val} {ps : List (Reg × Ty)} {vs : List Val} :
      v.HasTy ty → Fits ps vs → Fits ((r, ty) :: ps) (v :: vs)

theorem args_fit {scope : List (Reg × Ty)} {env : Env} (h : Typed scope env) :
    ∀ (ps : List (Reg × Ty)) (args : List Operand), ps.length = args.length →
      (ps.zip args).all (fun (pr, a) => operandHas scope pr.2 a) = true →
      ∃ vs, args.mapM (evalOp env) = some vs ∧ Fits ps vs
  | [], [], _, _ => ⟨[], rfl, .nil⟩
  | [], _ :: _, hl, _ => by simp at hl
  | _ :: _, [], hl, _ => by simp at hl
  | (r, ty) :: ps, a :: args, hl, ha => by
    simp only [List.zip_cons_cons, List.all_cons, Bool.and_eq_true] at ha
    obtain ⟨v, hv, ht⟩ := evalOp_typed h ha.1
    obtain ⟨vs, hvs, hf⟩ := args_fit h ps args (by simpa using hl) ha.2
    exact ⟨v :: vs, by simp [List.mapM_cons, hv, hvs], .cons ht hf⟩

/-- A successful type lookup comes from a binding in the list. For example, finding register 7 as `bool` requires that pair to occur. -/
theorem lookupTy_mem {l : List (Reg × Ty)} {r : Reg} {ty : Ty} (h : AList.lookup l r = some ty) :
    r ∈ l.map Prod.fst := by
  induction l with
  | nil => simp [AList.lookup] at h
  | cons x l ih =>
    obtain ⟨r', t'⟩ := x
    by_cases hr : r' = r
    · simp [hr]
    · simp only [AList.lookup, hr, ↓reduceIte] at h
      exact List.mem_cons_of_mem _ (ih h)

/-- When the first list has no binding, look in the second. For example, an empty first list cannot hide a later register. -/
theorem lookupTy_append_none {l₁ l₂ : List (Reg × Ty)} {r : Reg} (h : AList.lookup l₁ r = none) :
    AList.lookup (l₁ ++ l₂) r = AList.lookup l₂ r := by
  induction l₁ with
  | nil => rfl
  | cons x l ih =>
    obtain ⟨r', t'⟩ := x
    by_cases hr : r' = r
    · simp [AList.lookup, hr] at h
    · rw [List.cons_append]
      simp only [AList.lookup, hr, ↓reduceIte] at h ⊢
      exact ih h

/-- A binding in the first list wins over the second list. For example, appending another type for register 7 cannot replace its first type. -/
theorem lookupTy_append_some {l₁ l₂ : List (Reg × Ty)} {r : Reg} {ty : Ty}
    (h : AList.lookup l₁ r = some ty) : AList.lookup (l₁ ++ l₂) r = some ty := by
  induction l₁ with
  | nil => simp [AList.lookup] at h
  | cons x l ih =>
    obtain ⟨r', t'⟩ := x
    by_cases hr : r' = r
    · simpa [AList.lookup, hr] using h
    · rw [List.cons_append]
      simp only [AList.lookup, hr, ↓reduceIte] at h ⊢
      exact ih h

/-- Entering a block binds its (distinct) parameters to fitting values: the parameters' scope is
satisfied, whatever the rest of the environment holds.
For example, a function with no parameters begins with an empty typed scope. -/
theorem entry_typed :
    ∀ {ps : List (Reg × Ty)} {vs : List Val}, Fits ps vs → (ps.map Prod.fst).Nodup →
      ∀ env : Env, Typed ps.reverse ((ps.map Prod.fst).zip vs ++ env)
  | [], [], .nil, _, env => by simpa using typed_nil env
  | (r, ty) :: ps, v :: vs, .cons hv hf, hn, env => by
    rw [List.map_cons, List.nodup_cons] at hn
    intro r' ty' h'
    rw [List.reverse_cons] at h'
    cases hl : AList.lookup ps.reverse r' with
    | some t =>
      rw [lookupTy_append_some hl, Option.some.injEq] at h'
      subst h'
      have hne : r ≠ r' := by
        intro he
        subst he
        exact hn.1 (by simpa using lookupTy_mem hl)
      obtain ⟨w, hw, ht⟩ := entry_typed hf hn.2 env r' t hl
      exact ⟨w, by simp [AList.lookup, hne, hw], ht⟩
    | none =>
      rw [lookupTy_append_none hl] at h'
      by_cases hr : r = r'
      · subst hr
        simp [AList.lookup] at h'
        subst h'
        exact ⟨v, by simp [AList.lookup], hv⟩
      · simp [AList.lookup, hr] at h'

theorem sublist_flatMap {α β : Type} (f : α → List β) {a : α} :
    ∀ {l : List α}, a ∈ l → (f a).Sublist (l.flatMap f)
  | x :: l, h => by
    rw [List.flatMap_cons]
    rcases List.mem_cons.mp h with rfl | h
    · exact List.sublist_append_left _ _
    · exact (sublist_flatMap f h).trans (List.sublist_append_right _ _)

theorem nodup_of_eraseDups : ∀ (n : Nat) (l : List Reg), l.length ≤ n →
    l.eraseDups.length ≤ l.length ∧ (l.eraseDups.length = l.length → l.Nodup)
  | _, [], _ => by simp [List.eraseDups_nil]
  | n + 1, a :: l, hl => by
    have hfl := List.length_filter_le (fun b => !b == a) l
    obtain ⟨hle, heq⟩ := nodup_of_eraseDups n (l.filter (fun b => !b == a)) (by simp at hl; omega)
    rw [List.eraseDups_cons, List.length_cons, List.length_cons]
    refine ⟨by omega, fun h => ?_⟩
    have hf : (l.filter (fun b => !b == a)).length = l.length := by omega
    have hall := List.length_filter_eq_length_iff.mp hf
    have hself : l.filter (fun b => !b == a) = l := List.filter_eq_self.mpr hall
    rw [List.nodup_cons]
    refine ⟨fun ha => by simpa using hall a ha, ?_⟩
    rw [← hself]
    exact heq (by omega)

/-! ## The WF facts progress uses -/

theorem wf_facts {p : Program} (hwf : WF p) :
    (∃ b, p.blocks[0]? = some b ∧ b.params = []) ∧ (p.defs.map Prod.fst).Nodup ∧
      p.sites.length ≤ maxSites ∧ ∀ b ∈ p.blocks, Block.wf p b = true := by
  simp only [WF, wf, Bool.and_eq_true, beq_iff_eq, decide_eq_true_eq, List.all_eq_true] at hwf
  obtain ⟨⟨⟨⟨⟨he, hn⟩, hs⟩, -⟩, -⟩, hb⟩ := hwf
  refine ⟨?_, (nodup_of_eraseDups _ _ (Nat.le_refl _)).2 hn, hs, hb⟩
  revert he
  cases h0 : p.blocks[0]? with
  | none => simp
  | some b => exact fun he => ⟨b, rfl, by simpa using he⟩

theorem params_nodup {p : Program} (hn : (p.defs.map Prod.fst).Nodup) {b : Block}
    (hb : b ∈ p.blocks) : (b.params.map Prod.fst).Nodup :=
  hn.sublist (((List.sublist_append_left _ _).trans
    (sublist_flatMap Block.defs hb : b.defs.Sublist p.defs)).map Prod.fst)

/-- A well-formed edge enters its target, with the target's entry invariant. -/
theorem edge_typed {p : Program} (hn : (p.defs.map Prod.fst).Nodup) {scope : List (Reg × Ty)}
    {env : Env} (h : Typed scope env) {t : Nat} {args : List Operand}
    (he : edgeWf p scope t args = true) :
    ∃ tb env', p.blocks[t]? = some tb ∧ enter p t args env = some env' ∧
      Typed tb.params.reverse env' := by
  simp only [edgeWf, Bool.and_eq_true] at he
  obtain ⟨-, he⟩ := he
  cases ht : p.blocks[t]? with
  | none => simp [ht] at he
  | some tb =>
    simp only [ht, Bool.and_eq_true, beq_iff_eq] at he
    obtain ⟨hl, ha⟩ := he
    obtain ⟨vs, hvs, hf⟩ := args_fit h tb.params args hl ha
    exact ⟨tb, (tb.params.map Prod.fst).zip vs ++ env, rfl, by simp [enter, ht, hl, hvs],
      entry_typed hf (params_nodup hn (List.mem_of_getElem? ht)) env⟩

theorem u8_lt {x : Int} (h : IntTy.u8.inRange x = true) : x.toNat < 256 := by
  simp [IntTy.inRange, IntTy.min, IntTy.max, IntTy.signed, IntTy.bits] at h
  omega

/-! ## Progress -/

/-- From a block's entry invariant, a well-formed program's run is never stuck: it runs out of fuel
or ends in an outcome the result word encodes. -/
theorem runBlock_progress {p : Program} (hwf : WF p) :
    ∀ (fuel j : Nat) (env : Env) (blk : Block), p.blocks[j]? = some blk →
      Typed blk.params.reverse env →
      (runBlock p fuel j env).Encodable p.sites ∨ runBlock p fuel j env = .outOfFuel
  | 0, _, _, _, _, _ => .inr rfl
  | fuel + 1, j, env, blk, hj, h => by
    obtain ⟨-, hn, hsl, hbl⟩ := wf_facts hwf
    obtain ⟨env', scope, hx, ht, htw⟩ := block_typed (hbl blk (List.mem_of_getElem? hj)) h
    simp only [runBlock, hj, hx]
    cases hterm : blk.term with
    | ret a =>
      rw [hterm] at htw
      simp only [Term.wf] at htw
      obtain ⟨x, hv, hr⟩ := evalOp_int ht htw
      exact .inl (by simpa [hv, Outcome.Encodable] using u8_lt hr)
    | trap s =>
      rw [hterm] at htw
      simp only [Term.wf, decide_eq_true_eq] at htw
      exact .inl (by simpa [htw, Outcome.Encodable] using hsl)
    | br t args =>
      rw [hterm] at htw
      simp only [Term.wf] at htw
      obtain ⟨tb, env'', htb, hen, ht''⟩ := edge_typed hn ht htw
      simp only [hen]
      exact runBlock_progress hwf fuel t env'' tb htb ht''
    | condbr c t ta e ea =>
      rw [hterm] at htw
      simp only [Term.wf, Bool.and_eq_true, beq_iff_eq] at htw
      obtain ⟨⟨⟨hc, -⟩, hyes⟩, hno⟩ := htw
      obtain ⟨v, hv, hb⟩ := ht c .bool hc
      obtain ⟨b, rfl⟩ := bool_of_hasTy hb
      cases b with
      | true =>
        obtain ⟨tb, env'', htb, hen, ht''⟩ := edge_typed hn ht hyes
        simp only [hv, hen]
        exact runBlock_progress hwf fuel t env'' tb htb ht''
      | false =>
        obtain ⟨tb, env'', htb, hen, ht''⟩ := edge_typed hn ht hno
        simp only [hv, hen]
        exact runBlock_progress hwf fuel e env'' tb htb ht''

/-- **BIR progress.** A well-formed program's run, at any fuel, has run out of fuel or ended in an
outcome the result word encodes: never `stuck`. -/
theorem run_progress {p : Program} (hwf : WF p) (fuel : Nat) :
    (run p fuel).Encodable p.sites ∨ run p fuel = .outOfFuel := by
  obtain ⟨⟨b, h0, hp⟩, -⟩ := wf_facts hwf
  simp only [run, h0]
  exact runBlock_progress hwf fuel 0 [] b h0 (by simpa [hp] using typed_nil [])

theorem run_ne_stuck {p : Program} (hwf : WF p) (fuel : Nat) : run p fuel ≠ .stuck := by
  rcases run_progress hwf fuel with h | h <;> intro hs <;> rw [hs] at h
  · exact h
  · cases h

/-- Divergence is exactly running out of fuel at every fuel (for any program: no final outcome
has the behaviour `diverges`). -/
theorem behaviour_diverges_iff (p : Program) :
    behaviour p = .diverges ↔ ∀ fuel, run p fuel = .outOfFuel := by
  constructor
  · intro hd fuel
    cases hr : run p fuel with
    | outOfFuel => rfl
    | exited c =>
      rw [behaviour_of_run (n := fuel) (by simp [hr, Outcome.final]), hr] at hd
      cases hd
    | trapped s =>
      rw [behaviour_of_run (n := fuel) (by simp [hr, Outcome.final]), hr] at hd
      simp only [Outcome.toBehaviour] at hd
      split at hd <;> cases hd
    | stuck =>
      rw [behaviour_of_run (n := fuel) (by simp [hr, Outcome.final]), hr] at hd
      cases hd
  · intro h
    unfold behaviour
    split
    · rename_i hex
      obtain ⟨n, hn⟩ := hex
      simp [h n, Outcome.final] at hn
    · rfl

end Concrete.BIR
