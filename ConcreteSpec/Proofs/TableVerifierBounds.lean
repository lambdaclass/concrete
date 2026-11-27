import ConcreteSpec.Spec.LL1
import ConcreteSpec.Spec.TableVerifier

/-! Static bounds for compiling the word verifier. The layout allocates disjoint
    nonterminal and occurrence grids. The instruction count charges each Horn
    rule once for closure and once in its head bucket, and each production once
    per column in its left-hand side bucket. -/

namespace ConcreteSpec.GrammarVerification
open Concrete.LL1

theorem grid_bound {i n c j : Nat} (hi : i < n) (hj : j < c) : i*c+j < n*c := by
  have h := Nat.mul_le_mul_right c hi
  rw [Nat.succ_mul] at h
  omega

theorem valid_fact_address {l : Layout} {f : Fact} (hf : f.valid l) :
    l.isFact (f.address l) := by
  cases f <;> simp only [Fact.valid, decide_eq_true_eq, Bool.and_eq_true] at hf
  all_goals try obtain ⟨hf,hc⟩ := hf
  all_goals try have upper := grid_bound hf hc
  all_goals simp only [Fact.address, Layout.isFact, Layout.scratch, Layout.suffixFirst,
    Layout.suffixProductive, Layout.suffixNull, Layout.prefixNull, Layout.follow,
    Layout.firstNt, Layout.productive, Layout.nullNt, Layout.ntBase, Layout.rank,
    Bool.and_eq_true]
  all_goals constructor <;> apply decide_eq_true <;> omega

private theorem fold_group_mem {α : Type} (key : α → Nat) (items : List α)
    (bins : Array (List α)) (i : Nat) (hi : i < bins.size) (x : α) :
    x ∈ (items.foldl (fun acc y => acc.modify (key y) (y :: ·)) bins)[i]! ↔
      x ∈ bins[i]! ∨ x ∈ items ∧ key x = i := by
  induction items generalizing bins with
  | nil => simp
  | cons y ys ih =>
    simp only [List.foldl_cons]
    rw [ih _ (by simpa)]
    by_cases h : key y = i
    · simp [h, hi, Array.getElem_modify]
      grind
    · simp [h, hi, Array.getElem_modify]
      grind

theorem group_mem {α : Type} (key : α → Nat) (size : Nat) (items : List α)
    (i : Nat) (hi : i < size) (x : α) :
    x ∈ (groupByAddress key size items)[i]! ↔ x ∈ items ∧ key x = i := by
  simpa [groupByAddress, hi] using fold_group_mem key items (Array.replicate size []) i (by simpa) x

theorem valid_of_representable {g : RawGrammar} (h : representable g) : g.valid := by
  simp only [representable, Bool.and_eq_true] at h
  exact h.1.1

theorem indexed_row_bounds {r : Row} {rs : List (Nat × List (Sym Nat Nat))} {idx base}
    (h : r ∈ indexRows idx base rs) :
    (r.lhs,r.rhs) ∈ rs ∧ r.base + r.rhs.length < base + (rs.map (fun p => p.2.length+1)).sum := by
  induction rs generalizing idx base with
  | nil => contradiction
  | cons item rs ih =>
    rcases List.mem_cons.mp h with rfl | tail
    · exact ⟨List.mem_cons_self, by simp; omega⟩
    · obtain ⟨member,lt⟩ := ih tail
      exact ⟨List.mem_cons_of_mem _ member, by simp; omega⟩

private def symbolFits (l : Layout) : Sym Nat Nat → Prop
  | .t t => t+1 < l.c
  | .n n => n < l.n

private theorem rowBounds {g : RawGrammar} (h : g.valid) {r : Row} (hr : r ∈ g.rows) :
    r.lhs < g.layout.n ∧ r.base + r.rhs.length < g.layout.positions ∧
      ∀ sym ∈ r.rhs, symbolFits g.layout sym := by
  obtain ⟨mem,pos⟩ := indexed_row_bounds hr
  simp only [RawGrammar.valid, Bool.and_eq_true, List.all_eq_true, decide_eq_true_eq] at h
  have row := h.2 _ mem
  refine ⟨row.1, by simpa [RawGrammar.layout,RawGrammar.positions] using pos, ?_⟩
  intro sym hs
  have fit := row.2 sym hs
  cases sym <;> simpa [symbolFits,RawGrammar.layout,RawGrammar.columns] using fit

private def RuleFits (l : Layout) (r : Rule Fact) : Prop :=
  r.head.valid l ∧ (∀ p ∈ r.premises, p.valid l = true) ∧ r.premises.length ≤ 2

private theorem symbolRules_fit {l : Layout} {lhs pos : Nat} {sym : Sym Nat Nat}
    (hn : lhs < l.n) (hp : pos+1 < l.positions) (hs : symbolFits l sym) :
    ∀ rule ∈ l.symbolRules lhs pos sym, RuleFits l rule := by
  have posLt : pos < l.positions := by omega
  cases sym with
  | t t =>
    change t+1 < l.c at hs
    simp [Layout.symbolRules, RuleFits, Fact.valid, posLt, hp, hs]
  | n n =>
    simp only [Layout.symbolRules, List.forall_mem_append, List.forall_mem_flatMap,
      List.mem_range, List.forall_mem_cons, List.not_mem_nil, false_implies, implies_true, and_true]
    change n < l.n at hs
    refine ⟨by simp [RuleFits,Fact.valid,hs,posLt,hp], ?_⟩
    intro col hc
    split <;> simp_all [RuleFits, Fact.valid]

private theorem rules_fit {g : RawGrammar} (h : g.valid) :
    ∀ rule ∈ grammarRules g, RuleFits g.layout rule := by
  have start : g.cfg.start < g.layout.n := by
    simp only [RawGrammar.valid,Bool.and_eq_true,decide_eq_true_eq] at h
    exact h.1.1
  have hc : 0 < g.layout.c := Nat.zero_lt_succ _
  simp only [grammarRules, List.forall_mem_cons, List.forall_mem_flatMap]
  refine ⟨by simp [RuleFits,Fact.valid,start,hc], ?_⟩
  intro r hr
  obtain ⟨lhs,stop,syms⟩ := rowBounds h hr
  have base : r.base < g.layout.positions := by omega
  simp only [Layout.rowRules, List.forall_mem_append, List.forall_mem_cons, List.not_mem_nil,
    false_implies, implies_true, and_true, List.forall_mem_map, List.mem_range,
    List.forall_mem_flatMap]
  refine ⟨⟨by simp [RuleFits,Fact.valid,lhs,stop,base], ?_⟩, ?_⟩
  · intro t ht
    simp [RuleFits, Fact.valid, lhs, base, show t+1 < g.layout.c by omega]
  · rintro ⟨sym,i⟩ member
    obtain ⟨lt,eqn⟩ := List.mem_zipIdx' member
    apply symbolRules_fit lhs (by omega)
    exact syms sym (by grind)

theorem grammarRules_valid {g : RawGrammar} (h : g.valid) :
    ∀ r ∈ grammarRules g, r.head.valid g.layout = true ∧ ∀ p ∈ r.premises, p.valid g.layout = true :=
  fun r hr => ⟨(rules_fit h r hr).1, (rules_fit h r hr).2.1⟩

theorem grammarClauses_valid {g : RawGrammar} (h : g.valid) :
    ∀ r ∈ (grammarClauses g).toList, g.layout.isFact r.head = true ∧
      ∀ p ∈ r.premises, g.layout.isFact p = true := by
  simp only [grammarClauses, List.toList_toArray, List.forall_mem_map, Rule.map]
  exact fun r hr => ⟨valid_fact_address (grammarRules_valid h r hr).1,
    fun p hp => valid_fact_address ((grammarRules_valid h r hr).2 p hp)⟩

theorem clausesInBounds_of_valid {g : RawGrammar} (h : g.valid) : clausesInBounds g := by
  simpa [clausesInBounds, ← Array.all_toList, List.all_eq_true] using grammarClauses_valid h

private theorem scratch_le {g : RawGrammar} (h : g.valid) :
    g.layout.scratch ≤ 10 * (g.positions * g.columns) := by
  have hn : g.nonterminals ≤ g.positions := by
    simp only [RawGrammar.valid, Bool.and_eq_true, decide_eq_true_eq] at h
    exact h.1.2
  have nc := Nat.mul_le_mul_right g.columns hn
  have pc : g.positions ≤ g.positions * g.columns := Nat.le_mul_of_pos_right _ (Nat.zero_lt_succ _)
  simp only [Layout.scratch, Layout.suffixFirst, Layout.suffixProductive, Layout.suffixNull,
    Layout.prefixNull, Layout.follow, Layout.firstNt, Layout.productive, Layout.nullNt,
    Layout.ntBase, Layout.rank, RawGrammar.layout, Nat.add_zero]
  omega

theorem scratch_budget {g : RawGrammar} (h : g.valid) : g.layout.scratch + 1 ≤ g.budget := by
  have := scratch_le h
  have hc : 0 < g.columns := Nat.zero_lt_succ _
  simp only [RawGrammar.budget, Nat.mul_assoc, Nat.add_mul, Nat.one_mul]
  omega

private theorem combined_bounds (ts : List Test) (limit : Nat)
    (h : ∀ t ∈ ts, t.inBounds limit = true) :
    (Test.all ts).inBounds limit = true ∧ (Test.any ts).inBounds limit = true := by
  have fold (op : Test → Test → Test)
      (join : ∀ p q, (op p q).inBounds limit = (p.inBounds limit && q.inBounds limit))
      (xs : List Test) (a : Test) (ha : a.inBounds limit) (hx : ∀ t ∈ xs, t.inBounds limit) :
      (xs.foldl op a).inBounds limit := by
    induction xs generalizing a with
    | nil => exact ha
    | cons x xs ih =>
      exact ih (op a x) (by rw [join, ha, hx x List.mem_cons_self]; rfl)
        (fun t ht => hx t (List.mem_cons_of_mem _ ht))
  cases ts with
  | nil => exact ⟨rfl,rfl⟩
  | cons x xs =>
    have tail := fun t ht => h t (List.mem_cons_of_mem x ht)
    exact ⟨fold _ (fun _ _ => rfl) xs x (h _ List.mem_cons_self) tail,
      fold _ (fun _ _ => rfl) xs x (h _ List.mem_cons_self) tail⟩

private theorem isFact_lt {l : Layout} {a : Nat} (h : l.isFact a) : a < l.scratch := by
  simp only [Layout.isFact, Bool.and_eq_true, decide_eq_true_eq] at h
  exact h.2

private theorem clause_bounds {l : Layout} {r : Clause}
    (h : l.isFact r.head ∧ ∀ a ∈ r.premises, l.isFact a = true) :
    r.closedTest.inBounds l.scratch ∧ r.justification.inBounds l.scratch := by
  have head := isFact_lt h.1
  have pos := (combined_bounds (r.premises.map Test.positive) l.scratch (by
    simpa [Test.positive,Test.inBounds] using fun p hp => isFact_lt (h.2 p hp))).1
  have less := (combined_bounds (r.premises.map (fun p => Test.less p r.head)) l.scratch (by
    simpa [Test.inBounds, head] using fun p hp => isFact_lt (h.2 p hp))).1
  simp [Clause.closedTest, Clause.justification, Test.implies, Test.inBounds, Test.positive,
    pos, less, head]

private theorem rank_lt {l : Layout} {n : Nat} (h : n < l.n) : l.rank n < l.scratch := by
  simp only [Layout.scratch,Layout.suffixFirst,Layout.suffixProductive,Layout.suffixNull,
    Layout.prefixNull,Layout.follow,Layout.firstNt,Layout.productive,Layout.nullNt,
    Layout.ntBase,Layout.rank]
  omega

private theorem prediction_bounds {g : RawGrammar} (h : g.valid) {r : Row}
    (hr : r ∈ g.rows) {col : Nat} (hc : col < g.layout.c) :
    (g.layout.prediction r col).inBounds g.layout.scratch := by
  obtain ⟨hn,stop,_⟩ := rowBounds h hr
  have hp : r.base < g.layout.positions := by omega
  have bound (f : Fact) (hf : f.valid g.layout) := isFact_lt (valid_fact_address hf)
  simp only [Layout.prediction,Test.inBounds,Test.positive,Bool.and_eq_true,decide_eq_true_eq]
  exact ⟨bound (.suffixFirst ..) (by simp [Fact.valid,hp,hc]),
    bound (.suffixNull ..) (by simpa [Fact.valid]), bound (.follow ..) (by simp [Fact.valid,hn,hc])⟩

theorem checks_inBounds {g : RawGrammar} (h : g.valid) :
    (checks g).all (Test.inBounds g.layout.scratch) = true := by
  rw [← Array.all_toList, List.all_eq_true]
  simp only [checks, factChecks, predictionChecks, Array.toList_append, Array.toList_map,
    List.forall_mem_append,List.forall_mem_map,List.forall_mem_flatMap,
    List.mem_range]
  constructor
  · refine ⟨fun r hr => (clause_bounds (grammarClauses_valid h r hr)).1, ?_⟩
    intro a ha
    have lt : a < g.layout.scratch := by
      obtain ⟨i,hi,rfl⟩ := List.mem_map.mp ha
      simp only [List.mem_range] at hi
      omega
    simp only [Test.implies, Test.inBounds, Test.positive, decide_eq_true lt, Bool.true_and]
    apply (combined_bounds _ _ ?_).2
    rw [List.forall_mem_map]
    intro r member
    have belongs := (group_mem Rule.head _ _ a lt r).1 member
    exact (clause_bounds (grammarClauses_valid h r belongs.1)).2
  · refine ⟨⟨?_, ?_⟩, ?_⟩
    · simp only [Layout.cellChecks,List.forall_mem_flatMap,List.mem_range,List.forall_mem_cons,
        List.forall_mem_map]
      intro n hn c hc
      have cell : g.layout.cell n c < g.layout.scratch :=
        Nat.lt_trans (grid_bound hn hc) (by
          simp only [Layout.scratch,Layout.suffixFirst,Layout.suffixProductive,Layout.suffixNull,
            Layout.prefixNull,Layout.follow,Layout.firstNt,Layout.productive,Layout.nullNt,
            Layout.ntBase,Layout.rank]
          omega)
      have each := fun r member =>
        prediction_bounds h ((group_mem Row.lhs _ g.rows n hn r).1 member).1 hc
      refine ⟨Bool.and_eq_true_iff.mpr ⟨decide_eq_true cell, (combined_bounds _ _
        (List.forall_mem_map.mpr each)).2⟩, ?_⟩
      exact fun r hr => Bool.and_eq_true_iff.mpr ⟨each r hr,decide_eq_true cell⟩
    · intro r hr t ht
      obtain ⟨lhs,stop,syms⟩ := rowBounds h hr
      obtain ⟨⟨sym,i⟩,member,eqn⟩ := List.mem_filterMap.mp ht
      obtain ⟨lt,atI⟩ := List.mem_zipIdx' member
      cases sym with
      | t _ => contradiction
      | n child =>
        cases eqn
        have childLt : child < g.layout.n := syms (.n child) (by grind)
        have pref := valid_fact_address (f := .prefixNull (r.base+i))
          (by simp only [Fact.valid,decide_eq_true_eq]; omega : (Fact.prefixNull (r.base+i)).valid g.layout)
        exact Bool.and_eq_true_iff.mpr ⟨decide_eq_true (isFact_lt pref),
          Bool.and_eq_true_iff.mpr ⟨decide_eq_true (rank_lt childLt),decide_eq_true (rank_lt lhs)⟩⟩
    · intro n hn
      exact decide_eq_true (isFact_lt (valid_fact_address (f := .productive n)
        (l := g.layout) (decide_eq_true hn)))

/-- Charge work to occurrences, preserving multiplicity even for identical rows. -/
private def cost {α : Type} (xs : List α) (f : α → Nat) := (xs.map f).sum

private theorem cost_mono {α : Type} (xs : List α) {f b : α → Nat}
    (h : ∀ x ∈ xs, f x ≤ b x) : cost xs f ≤ cost xs b := by
  induction xs with
  | nil => exact Nat.le_refl _
  | cons x xs ih =>
    exact Nat.add_le_add (h _ List.mem_cons_self) (ih (fun y hy => h y (List.mem_cons_of_mem _ hy)))

private theorem cost_linear {α : Type} (xs : List α) (f : α → Nat) (a b : Nat) :
    cost xs (fun x => a + b * f x) = xs.length * a + b * cost xs f := by
  induction xs with
  | nil => simp [cost]
  | cons x xs ih =>
    change (a + b * f x) + cost xs _ = (xs.length+1)*a + b*(f x + cost xs f)
    rw [ih, Nat.add_mul, Nat.mul_add]
    omega

private theorem cost_fixed {α : Type} (xs : List α) (f : α → Nat) (k : Nat)
    (h : ∀ x ∈ xs, f x ≤ k) : cost xs f ≤ xs.length * k := by
  have le := cost_mono xs (b := fun _ => k + 0 * 0) h
  rw [cost_linear xs (fun _ => 0) k 0] at le
  simpa using le

private theorem flat_cost {α β : Type} (xs : List α) (f : α → List β) (w : β → Nat) :
    cost (xs.flatMap f) w = cost xs (fun x => cost (f x) w) := by
  induction xs with
  | nil => rfl
  | cons x xs ih => simpa [cost, List.map_append, List.sum_append] using congrArg (cost (f x) w + ·) ih

private theorem flat_count {α β : Type} (xs : List α) (f : α → List β) :
    (xs.flatMap f).length = cost xs (fun x => (f x).length) := by
  exact List.length_flatMap

private theorem indexed_positions (rs : List (Nat × List (Sym Nat Nat))) (idx base : Nat) :
    cost (indexRows idx base rs) (fun r => r.rhs.length+1) =
      (rs.map (fun p => p.2.length+1)).sum := by
  induction rs generalizing idx base with
  | nil => rfl
  | cons x xs ih => exact congrArg (x.2.length+1 + ·) (ih _ _)

/-- A rule for an occurrence has at most two premises. Each symbol or end
position contributes at most seven rules per lookahead column. -/
private theorem rule_count (g : RawGrammar) :
    (grammarRules g).length ≤ 1 + 7 * (g.positions * g.columns) := by
  let l := g.layout
  have hc : 0 < l.c := Nat.zero_lt_succ _
  have symbol (lhs p : Nat) (sym : Sym Nat Nat) : (l.symbolRules lhs p sym).length ≤ 7*l.c := by
    cases sym with
    | t _ => simp [Layout.symbolRules]; omega
    | n _ =>
      simp only [Layout.symbolRules,List.length_append,List.length_cons,List.length_nil,flat_count]
      apply Nat.le_trans (Nat.add_le_add_left (cost_fixed _ _ 4 ?_) 3)
      · simp; omega
      · intro c _
        split <;> simp
  have row (r : Row) : (l.rowRules r).length ≤ (r.rhs.length+1) * (7*l.c) := by
    simp only [Layout.rowRules, List.length_append,List.length_map,List.length_range,
      List.length_cons,List.length_nil,flat_count]
    have per := cost_fixed r.rhs.zipIdx (fun (s,i) => (l.symbolRules r.lhs (r.base+i) s).length)
      (7*l.c) (fun (sym,i) _ => symbol r.lhs (r.base+i) sym)
    simp only [List.length_zipIdx] at per
    rw [Nat.add_mul]
    omega
  have le := cost_mono g.rows (b := fun r => 0 + (7*l.c)*(r.rhs.length+1))
    (fun r _ => by simpa [Nat.mul_comm] using row r)
  rw [cost_linear, RawGrammar.rows, indexed_positions] at le
  simpa [grammarRules, flat_count, Nat.mul_assoc, Nat.mul_comm, Nat.mul_left_comm, Nat.add_comm, cost,
    l, RawGrammar.layout, RawGrammar.positions,RawGrammar.rows] using Nat.add_le_add_left le 1

/-- Reading distinct buckets charges each inserted occurrence at most once. -/
private theorem group_count {α : Type} (key : α → Nat) (size : Nat) (xs : List α)
    (indices : List Nat) (unique : indices.Nodup) (inside : ∀ i ∈ indices, i < size) :
    cost indices (fun i => (groupByAddress key size xs)[i]!.length) ≤ xs.length := by
  have insert (bins : Array (List α)) (x : α) (is : List Nat) (nd : is.Nodup)
      (hi : ∀ i ∈ is, i < bins.size) :
      cost is (fun i => (bins.modify (key x) (x :: ·))[i]!.length) =
        cost is (fun i => bins[i]!.length) + (if key x ∈ is then 1 else 0) := by
    induction is with
    | nil => simp [cost]
    | cons i is ih =>
      have ⟨absent, tail⟩ := List.nodup_cons.mp nd
      have lt := hi i List.mem_cons_self
      have rest := ih tail (fun j hj => hi _ (List.mem_cons_of_mem _ hj))
      change _ + cost is _ = _ + cost is _ + _
      rw [rest]
      by_cases eqn : key x = i
      · simp [eqn,lt,Array.getElem_modify,absent]; omega
      · simp [eqn,lt,Array.getElem_modify,Nat.add_assoc]
  have fold (ys : List α) (bins : Array (List α)) (hs : bins.size = size) :
      cost indices (fun i => (ys.foldl (fun a y => a.modify (key y) (y :: ·)) bins)[i]!.length)
        ≤ cost indices (fun i => bins[i]!.length) + ys.length := by
    induction ys generalizing bins with
    | nil => simp
    | cons y ys ih =>
      have tail := ih (bins.modify (key y) (y :: ·)) (by simpa using hs)
      rw [insert bins y indices unique (hs ▸ inside)] at tail
      simp only [List.foldl_cons, List.length_cons]
      split at tail <;> omega
  have initial : cost indices (fun i => (Array.replicate size ([] : List α))[i]!.length) ≤
      indices.length * 0 := cost_fixed _ _ 0 (by intro i hi; simp [inside i hi])
  have le := fold xs (Array.replicate size []) (by simp)
  have empty : cost indices (fun i => (Array.replicate size ([] : List α))[i]!.length) = 0 := by omega
  simpa [groupByAddress, empty] using le

private theorem combined_width (ts : List Test) :
    (Test.all ts).width ≤ 1 + totalWidth ts ∧ (Test.any ts).width ≤ 1 + totalWidth ts := by
  have fold (op : Test → Test → Test) (join : ∀ a b, (op a b).width = a.width+b.width)
      (xs : List Test) (a : Test) : (xs.foldl op a).width = a.width + totalWidth xs := by
    induction xs generalizing a with
    | nil => simp [totalWidth]
    | cons x xs ih => simp [ih,join,totalWidth,Nat.add_assoc]
  cases ts with
  | nil => decide
  | cons x xs =>
    simp only [Test.all, Test.any, fold Test.both (fun _ _ => rfl),
      fold Test.either (fun _ _ => rfl), totalWidth,List.map_cons,List.sum_cons]
    omega

private theorem mapped_width {α : Type} (xs : List α) (f : α → Test) :
    totalWidth (xs.map f) = cost xs (fun x => (f x).width) := by
  simp [totalWidth,cost,List.map_map,Function.comp_def]

private theorem clause_width (r : Clause) (h : r.premises.length ≤ 2) :
    r.closedTest.width ≤ 7 ∧ r.justification.width ≤ 8 := by
  have pos := (combined_width (r.premises.map Test.positive)).1
  have less := (combined_width (r.premises.map (fun a => Test.less a r.head))).1
  rw [mapped_width] at pos less
  have wp := cost_fixed r.premises (fun a => (Test.positive a).width) 2 (fun _ _ => Nat.le_refl _)
  have wl := cost_fixed r.premises (fun a => (Test.less a r.head).width) 1 (fun _ _ => Nat.le_refl _)
  simp only [Clause.closedTest,Clause.justification,Test.implies,Test.width,Test.positive]
  omega

private theorem facts_width (g : RawGrammar) (h : g.valid) :
    totalWidth (factChecks g.layout (grammarClauses g)).toList ≤
      15 * (grammarRules g).length + 3 * g.layout.scratch := by
  let l := g.layout
  let rs := (grammarClauses g).toList
  let groups := groupByAddress Rule.head l.scratch rs
  have arity : ∀ r ∈ rs, r.premises.length ≤ 2 := by
    simp only [rs,grammarClauses,List.toList_toArray,List.forall_mem_map,Rule.map,List.length_map]
    exact fun r hr => (rules_fit h r hr).2.2
  have close := cost_fixed rs (fun r => r.closedTest.width) 7 (fun r hr => (clause_width r (arity r hr)).1)
  have inside (a : Nat) (ha : a ∈ l.factAddresses) : a < l.scratch := by
    obtain ⟨i,hi,rfl⟩ := List.mem_map.mp ha
    simp only [List.mem_range] at hi
    omega
  have unique : l.factAddresses.Nodup := by
    rw [Layout.factAddresses, ← List.range'_eq_map_range]
    exact List.nodup_range'
  have count := group_count Rule.head l.scratch rs l.factAddresses unique inside
  change cost l.factAddresses (fun a => groups[a]!.length) ≤ rs.length at count
  have localWidth (a : Nat) (ha : a ∈ l.factAddresses) :
      ((Test.positive a).implies (Test.any (groups[a]!.map Clause.justification))).width
        ≤ 3 + 8 * groups[a]!.length := by
    have anyWidth := (combined_width (groups[a]!.map Clause.justification)).2
    rw [mapped_width] at anyWidth
    have each := cost_fixed groups[a]! (fun r => (Clause.justification r).width) 8 (fun r hr =>
      (clause_width r (arity r ((group_mem Rule.head _ rs a (inside a ha) r).1 hr).1)).2)
    have bound := Nat.le_trans anyWidth (Nat.add_le_add_left each 1)
    change 2 + _ ≤ _
    omega
  have heads := cost_mono l.factAddresses localWidth
  rw [cost_linear] at heads
  have headCount : l.factAddresses.length ≤ l.scratch := by simp [Layout.factAddresses]
  have len : rs.length = (grammarRules g).length := by simp [rs,grammarClauses]
  change totalWidth (((grammarClauses g).map Clause.closedTest ++
    l.factAddresses.toArray.map (fun a => (Test.positive a).implies
      (Test.any (groups[a]!.map Clause.justification)))).toList) ≤ _
  simp only [Array.toList_append, Array.toList_map]
  change (cost (rs.map Clause.closedTest ++ _) Test.width) ≤ _
  have append {xs ys : List Test} : cost (xs ++ ys) Test.width =
      totalWidth xs + totalWidth ys := by simp [cost,totalWidth]
  rw [append,mapped_width,mapped_width]
  change _ ≤ 15 * (grammarRules g).length + 3 * l.scratch
  omega

private theorem predictions_width (g : RawGrammar) :
    totalWidth (predictionChecks g).toList ≤
      3 * (g.nonterminals * g.columns) + 14 * (g.positions * g.columns) +
        3 * g.positions + 2 * g.nonterminals := by
  let l := g.layout
  let rows := g.rows
  let groups := groupByAddress Row.lhs l.n rows
  have pos : cost rows (fun r => r.rhs.length+1) = g.positions := indexed_positions ..
  have lengths : rows.length ≤ g.positions := by
    have one := cost_mono rows (f := fun _ => 1) (b := fun r => r.rhs.length+1) (by intros; omega)
    have ones : (cost rows (fun _ => 1)) = rows.length := by
      simpa using cost_linear rows (fun _ => 0) 1 0
    omega
  have count := group_count Row.lhs l.n rows (List.range l.n) List.nodup_range
    (by simp)
  change cost (List.range l.n) (fun n => groups[n]!.length) ≤ rows.length at count
  have cells (n col : Nat) :
      totalWidth (((Test.positive (l.cell n col)).implies
        (Test.any (groups[n]!.map (l.prediction · col)))) ::
        groups[n]!.map (fun r => (l.prediction r col).implies (.word (l.cell n col) (r.index+1).toUInt64)))
        ≤ 3 + 14 * groups[n]!.length := by
    have anyWidth := (combined_width (groups[n]!.map (l.prediction · col))).2
    rw [mapped_width] at anyWidth
    have predict := cost_fixed groups[n]! (fun r => (l.prediction r col).width) 6 (fun _ _ => Nat.le_refl _)
    have check := cost_fixed groups[n]! (fun r =>
      ((l.prediction r col).implies (.word (l.cell n col) (r.index+1).toUInt64)).width) 8
      (fun _ _ => Nat.le_refl _)
    change (2 + _) + totalWidth (List.map _ _) ≤ _
    rw [mapped_width]
    omega
  have grid : totalWidth (l.cellChecks rows) ≤ 3 * (l.n*l.c) + 14 * (rows.length*l.c) := by
    change cost ((List.range l.n).flatMap _) Test.width ≤ _
    rw [flat_cost]
    apply Nat.le_trans (cost_mono _ (b := fun n => 3*l.c + (14*l.c)*groups[n]!.length) ?_) ?_
    · intro n _
      rw [flat_cost]
      apply Nat.le_trans (cost_fixed _ _ _ (fun col _ => cells n col)) ?_
      simp [Nat.mul_add,Nat.mul_assoc,Nat.mul_comm,Nat.mul_left_comm]
    · rw [cost_linear]
      simp only [List.length_range]
      have := Nat.mul_le_mul_left (14*l.c) count
      simpa only [Nat.mul_assoc,Nat.mul_comm,Nat.mul_left_comm] using Nat.add_le_add_left this (l.n*(3*l.c))
  have descent (r : Row) : totalWidth (l.descentChecks r) ≤ 3*(r.rhs.length+1) := by
    have widths := cost_fixed (l.descentChecks r) Test.width 3 (by
      intro t ht
      obtain ⟨⟨sym,i⟩,_,eqn⟩ := List.mem_filterMap.mp ht
      cases sym <;> cases eqn
      exact Nat.le_refl _)
    have len : (l.descentChecks r).length ≤ r.rhs.zipIdx.length := List.length_filterMap_le ..
    simp only [List.length_zipIdx] at len
    change cost _ _ ≤ _
    omega
  have edges := cost_mono rows (f := fun r => totalWidth (l.descentChecks r)) (b := fun r => 0 + 3*(r.rhs.length+1)) (fun r _ => by simpa using descent r)
  rw [cost_linear,pos] at edges
  have prod := cost_fixed (List.range l.n) (fun n => (Test.positive (l.productive n)).width) 2
    (fun _ _ => Nat.le_refl _)
  simp only [List.length_range] at prod
  have gridMax := Nat.mul_le_mul_right l.c lengths
  change cost ((l.cellChecks rows ++ rows.flatMap l.descentChecks) ++
    (List.range l.n).map (fun n => Test.positive (l.productive n))) Test.width ≤ _
  have append (xs ys : List Test) : cost (xs ++ ys) Test.width =
      cost xs Test.width + cost ys Test.width := by simp [cost]
  rw [append,append,flat_cost]
  change totalWidth (l.cellChecks rows) + cost rows (fun r => totalWidth (l.descentChecks r)) +
    totalWidth ((List.range l.n).map _) ≤ 3 * (l.n*l.c) + 14 * (g.positions*l.c) + 3*g.positions + 2*l.n
  rw [mapped_width]
  omega

theorem checks_width_budget {g : RawGrammar} (h : g.valid) :
    totalWidth (checks g).toList + 2 ≤ g.budget := by
  have facts := facts_width g h
  have predictions := predictions_width g
  have rules := rule_count g
  have scratch := scratch_le h
  have hn : g.nonterminals ≤ g.positions := by
    simp only [RawGrammar.valid,Bool.and_eq_true,decide_eq_true_eq] at h
    exact h.1.2
  have hc : 0 < g.columns := Nat.zero_lt_succ _
  have nc := Nat.mul_le_mul_right g.columns hn
  have pc := Nat.le_mul_of_pos_right g.positions hc
  have splitWidth : totalWidth (checks g).toList = totalWidth (factChecks g.layout (grammarClauses g)).toList +
      totalWidth (predictionChecks g).toList := by simp [checks,totalWidth]
  rw [splitWidth]
  simp only [RawGrammar.budget,Nat.mul_assoc,Nat.add_mul,Nat.one_mul]
  omega

end ConcreteSpec.GrammarVerification
