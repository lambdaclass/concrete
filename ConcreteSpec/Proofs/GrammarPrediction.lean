import ConcreteSpec.Proofs.TableVerifier
import ConcreteSpec.Spec.TableVerifier
import ConcreteSpec.Spec.LL1

namespace ConcreteSpec.GrammarVerification
open Concrete.LL1

private theorem row_base_ge {r : Row} {rs : List (Nat × List (Sym Nat Nat))} {idx base}
    (h : r ∈ indexRows idx base rs) : base ≤ r.base := by
  induction rs generalizing idx base with
  | nil => contradiction
  | cons item rs ih =>
    rcases List.mem_cons.mp h with rfl | tail
    · exact Nat.le_refl _
    · have := ih tail; omega

private theorem row_parts_aux {r : Row} {rs : List (Nat × List (Sym Nat Nat))} {idx base}
    (h : r ∈ indexRows idx base rs) (i : Nat) (hi : i ≤ r.rhs.length) :
    positionParts base (r.base+i) rs = (r.rhs.take i, r.rhs.drop i) := by
  induction rs generalizing idx base with
  | nil => contradiction
  | cons item rs ih =>
    rcases List.mem_cons.mp h with rfl | tail
    · change i ≤ item.2.length at hi
      simp [positionParts, show base+i < base+item.2.length+1 by omega]
    · have := row_base_ge tail
      simp only [positionParts, show ¬ r.base+i < base+item.2.length+1 by omega, ↓reduceIte]
      exact ih tail

private theorem row_parts {g : RawGrammar} {r : Row} (hr : r ∈ g.rows)
    (i : Nat) (hi : i ≤ r.rhs.length) :
    positionParts 0 (r.base+i) g.cfg.prods = (r.rhs.take i, r.rhs.drop i) :=
  row_parts_aux hr i hi

private theorem row_shape_aux {r : Row} {rs : List (Nat × List (Sym Nat Nat))} {idx base}
    (h : r ∈ indexRows idx base rs) : (r.lhs,r.rhs) ∈ rs := by
  induction rs generalizing idx base with
  | nil => contradiction
  | cons item rs ih =>
    rcases List.mem_cons.mp h with rfl | tail
    · exact List.mem_cons_self
    · exact List.mem_cons_of_mem _ (ih tail)

private theorem null_nil (g : Grammar Nat Nat) : NullableSeq g [] :=
  ⟨[], rfl, fun _ h => nomatch h⟩

private theorem null_cons {g : Grammar Nat Nat} {x : Nat} {rhs : List (Sym Nat Nat)} :
    NullableSeq g (.n x :: rhs) ↔ Nullable g x ∧ NullableSeq g rhs := by
  constructor
  · rintro ⟨xs,he,all⟩
    cases xs with
    | nil => contradiction
    | cons y ys =>
      simp only [List.map_cons, List.cons.injEq, Sym.n.injEq] at he
      obtain ⟨rfl,rfl⟩ := he
      exact ⟨all _ List.mem_cons_self, ys, rfl, fun n hn => all _ (List.mem_cons_of_mem _ hn)⟩
  · rintro ⟨hx,ys,rfl,hy⟩
    exact ⟨x::ys,rfl,fun n hn => (List.mem_cons.mp hn).elim (fun e => e ▸ hx) (hy n)⟩

private theorem null_snoc {g : Grammar Nat Nat} {xs : List (Sym Nat Nat)} {x}
    (h : NullableSeq g xs) (hx : Nullable g x) : NullableSeq g (xs ++ [.n x]) := by
  obtain ⟨ys,rfl,hy⟩ := h
  exact ⟨ys ++ [x],by simp,by simp; grind⟩

private theorem first_cons_t {g : Grammar Nat Nat} (t : Nat) (rhs : List (Sym Nat Nat)) :
    FirstSeq g (.t t :: rhs) t :=
  ⟨[],rhs,null_nil _,Or.inl rfl⟩

private theorem first_cons_n {g : Grammar Nat Nat} {n t : Nat} {rhs : List (Sym Nat Nat)} :
    FirstSeq g (.n n :: rhs) t ↔ First g n t ∨ Nullable g n ∧ FirstSeq g rhs t := by
  constructor
  · rintro ⟨pre,post,hp,alt⟩
    cases pre with
    | nil => simp at alt; obtain ⟨_,⟨rfl,_⟩,h⟩ := alt; exact Or.inl h
    | cons sym pre =>
      have eqn : sym = .n n := by
        rcases alt with he | ⟨_,he,_⟩ <;> exact (List.cons.inj he).1.symm
      subst sym
      rcases null_cons.mp hp with ⟨hn,hpre⟩
      exact Or.inr ⟨hn,pre,post,hpre,by simpa using alt⟩
  · rintro (h | ⟨hn,pre,post,hp,alt⟩)
    · exact ⟨[],rhs,null_nil _,Or.inr ⟨n,rfl,h⟩⟩
    · refine ⟨.n n :: pre,post,null_cons.mpr ⟨hn,hp⟩,?_⟩
      rcases alt with he | ⟨x,he,fx⟩
      · exact Or.inl (congrArg (_ :: ·) he)
      · exact Or.inr ⟨x,congrArg (_ :: ·) he,fx⟩

private theorem first_row {g : Grammar Nat Nat} {lhs rhs t} (hr : (lhs,rhs) ∈ g.prods)
    (h : FirstSeq g rhs t) : First g lhs t := by
  obtain ⟨pre,post,hp,alt⟩ := h
  rcases alt with rfl | ⟨y,rfl,hy⟩
  · exact .term hr hp
  · exact .nonterm hr hp hy

private theorem unfold_occurrence {rhs : List (Sym Nat Nat)} {sym i}
    (member : (sym,i) ∈ rhs.zipIdx) :
    i < rhs.length ∧ rhs.take (i+1) = rhs.take i ++ [sym] ∧
      rhs.drop i = sym :: rhs.drop (i+1) ∧ rhs = rhs.take i ++ sym :: rhs.drop (i+1) := by
  have eqn := List.mem_zipIdx_iff_getElem?.mp member
  obtain ⟨lt,hs⟩ := List.getElem?_eq_some_iff.mp eqn
  have cell : rhs[i] = sym := hs
  have drop := List.drop_eq_getElem_cons lt
  rw [cell] at drop
  exact ⟨lt,by rw [List.take_succ_eq_append_getElem lt, cell],drop,by rw [← drop, List.take_append_drop]⟩

private theorem symbolRules_sound {g : RawGrammar} {r : Row} (hr : r ∈ g.rows) {sym i}
    (hi : (sym,i) ∈ r.rhs.zipIdx) {rule : Rule Fact}
    (hm : rule ∈ g.layout.symbolRules r.lhs (r.base+i) sym) :
    (∀ p ∈ rule.premises, p.Meaning g) → rule.head.Meaning g := by
  obtain ⟨lt,take,drop,split⟩ := unfold_occurrence hi
  have here := row_parts hr i (by omega)
  have next := row_parts hr (i+1) lt
  rw [← Nat.add_assoc] at next
  have source := row_shape_aux hr
  cases sym with
  | t t =>
    simp only [Layout.symbolRules, List.mem_cons, List.not_mem_nil, or_false] at hm
    rcases hm with rfl | rfl
    · intro _
      exact ⟨t,rfl, by simp only [here, drop]; exact first_cons_t ..⟩
    · intro h
      have tail := h _ List.mem_cons_self
      simp only [Fact.Meaning, here, next, drop, ProductiveSeq, List.mem_cons] at tail ⊢
      exact fun child hc => tail child (by simpa using hc)
  | n child =>
    simp only [Layout.symbolRules, List.mem_append, List.mem_cons, List.not_mem_nil, or_false,
      List.mem_flatMap, List.mem_range] at hm
    rcases hm with (rfl | rfl | rfl) | ⟨col,_,common | other⟩
    · intro h
      have pre := h _ List.mem_cons_self
      have n := h _ (by exact List.mem_cons_of_mem _ List.mem_cons_self)
      simp only [Fact.Meaning, here, next, take] at pre n ⊢
      exact null_snoc pre n
    · intro h
      have n := h _ List.mem_cons_self
      have tail := h _ (List.mem_cons_of_mem _ List.mem_cons_self)
      simp only [Fact.Meaning, here, next, drop] at n tail ⊢
      exact null_cons.mpr ⟨n,tail⟩
    · intro h
      have n := h _ List.mem_cons_self
      have tail := h _ (List.mem_cons_of_mem _ List.mem_cons_self)
      simp only [Fact.Meaning, here, next, drop] at n tail ⊢
      intro x hx
      rcases List.mem_cons.mp hx with eqn | hx
      · exact (Sym.n.inj eqn) ▸ n
      · exact tail x hx
    · cases common
      intro h
      have tail := h _ List.mem_cons_self
      have parent := h _ (List.mem_cons_of_mem _ List.mem_cons_self)
      simp only [Fact.Meaning, next] at tail
      exact Follow.tail (split ▸ source) tail parent
    · split at other <;> simp only [List.not_mem_nil, List.mem_cons, or_false] at other
      rcases other with rfl | rfl | rfl
      · intro h
        obtain ⟨t,rfl,hn⟩ := h _ List.mem_cons_self
        exact ⟨t,rfl,by simp only [here,drop]; exact first_cons_n.mpr (Or.inl hn)⟩
      · intro h
        have hn := h _ List.mem_cons_self
        have tail := h _ (List.mem_cons_of_mem _ List.mem_cons_self)
        simp only [Fact.Meaning, next] at tail
        obtain ⟨t,rfl,ht⟩ := tail
        exact ⟨t,rfl,by simp only [here,drop]; exact first_cons_n.mpr (Or.inr ⟨hn,ht⟩)⟩
      · intro h
        have tail := h _ List.mem_cons_self
        simp only [Fact.Meaning, next] at tail
        obtain ⟨t,rfl,ht⟩ := tail
        exact Follow.first (split ▸ source) ht

theorem symbolic_sound {g : RawGrammar} {f : Fact} (h : Provable (grammarRules g) f) :
    f.Meaning g := by
  induction h with
  | step rule member _ ih =>
    rcases List.mem_cons.mp member with rfl | other
    · exact Follow.start
    obtain ⟨r,hr,hrow⟩ := List.mem_flatMap.mp other
    have start := row_parts hr 0 (Nat.zero_le _)
    have stop := row_parts hr r.rhs.length (Nat.le_refl _)
    simp only [Nat.add_zero, List.take_zero, List.drop_zero] at start
    simp only [List.take_length, List.drop_length] at stop
    have prod := row_shape_aux hr
    simp only [Layout.rowRules, List.mem_append, List.mem_cons, List.not_mem_nil, or_false,
      List.mem_map, List.mem_range, List.mem_flatMap] at hrow
    rcases hrow with ((rfl | rfl | rfl | rfl | rfl) | ⟨t,_,rfl⟩) | ⟨⟨sym,i⟩,hm,hs⟩
    · simpa only [Fact.Meaning, start] using null_nil g.cfg
    · simpa only [Fact.Meaning, stop] using null_nil g.cfg
    · simp [Fact.Meaning, stop, ProductiveSeq]
    · have hn := ih _ List.mem_cons_self
      simp only [Fact.Meaning, start] at hn
      obtain ⟨xs,he,all⟩ := hn
      exact Nullable.intro (he ▸ prod) all
    · have hp := ih _ List.mem_cons_self
      simp only [Fact.Meaning, start] at hp
      exact Productive.row prod hp
    · have hf := ih _ List.mem_cons_self
      simp only [Fact.Meaning, start] at hf
      obtain ⟨a,he,ht⟩ := hf
      exact ⟨a,he,first_row prod ht⟩
    · exact symbolRules_sound hr hm hs ih

private abbrev Known (g : RawGrammar) := Provable (grammarRules g)

private theorem members_rows_aux {idx base : Nat} {ps : List (Nat × List (Sym Nat Nat))} {p}
    (hp : p ∈ ps) : ∃ r ∈ indexRows idx base ps, (r.lhs,r.rhs) = p := by
  induction ps generalizing idx base with
  | nil => contradiction
  | cons x xs ih =>
    rcases List.mem_cons.mp hp with rfl | h
    · exact ⟨_,List.mem_cons_self,rfl⟩
    · obtain ⟨r,hr,e⟩ := ih (idx := idx+1) (base := base+x.2.length+1) h
      exact ⟨r,List.mem_cons_of_mem _ hr,e⟩

private theorem row_rule {g : RawGrammar} {r : Row} (hr : r ∈ g.rows) {conclusion premises}
    (member : ⟨conclusion,premises⟩ ∈ g.layout.rowRules r)
    (ready : ∀ p ∈ premises, Known g p) : Known g conclusion :=
  .step ⟨conclusion,premises⟩ (List.mem_cons_of_mem _ (List.mem_flatMap.mpr ⟨r,hr,member⟩)) ready

private theorem occurrence_rule {g : RawGrammar} {r : Row} (hr : r ∈ g.rows) {sym i}
    (hi : (sym,i) ∈ r.rhs.zipIdx) {conclusion premises}
    (member : ⟨conclusion,premises⟩ ∈ g.layout.symbolRules r.lhs (r.base+i) sym)
    (ready : ∀ p ∈ premises, Known g p) : Known g conclusion := by
  apply row_rule hr _ ready
  exact List.mem_append_right _ (List.mem_flatMap.mpr ⟨(sym,i),hi,member⟩)

private theorem at_split {rhs post : List (Sym Nat Nat)} {sym i}
    (eqn : rhs.drop i = sym :: post) (hlen : i ≤ rhs.length) :
    (sym,i) ∈ rhs.zipIdx ∧ rhs.drop (i+1) = post := by
  have dropLen := congrArg List.length eqn
  simp only [List.length_drop, List.length_cons] at dropLen
  have lt : i < rhs.length := by omega
  rw [List.drop_eq_getElem_cons lt] at eqn
  exact ⟨List.mem_zipIdx_iff_getElem?.mpr (by simp [lt, (List.cons.inj eqn).1]), (List.cons.inj eqn).2⟩

private theorem suffix_null {g : RawGrammar} {r : Row} (hr : r ∈ g.rows) (xs : List Nat)
    (known : ∀ n ∈ xs, Known g (.nullable n)) (i : Nat) (hi : i ≤ r.rhs.length)
    (eqn : r.rhs.drop i = xs.map Sym.n) : Known g (.suffixNull (r.base+i)) := by
  induction xs generalizing i with
  | nil =>
    have len := congrArg List.length eqn
    simp at len
    have eq : i = r.rhs.length := by omega
    subst i
    exact row_rule hr (premises := []) (by simp [Layout.rowRules]) (by simp)
  | cons n ns ih =>
    obtain ⟨atI,tail⟩ := at_split (by simpa using eqn) hi
    have lt := (List.mem_zipIdx' atI).1
    have next := ih (fun y hy => known y (List.mem_cons_of_mem _ hy)) (i+1) lt tail
    apply occurrence_rule hr atI (premises := [.nullable n,.suffixNull (r.base+i+1)])
    · simp [Layout.symbolRules]
    · simpa [Nat.add_assoc] using And.intro (known n List.mem_cons_self) next

private theorem nullable_complete {g : RawGrammar} {n} (h : Nullable g.cfg n) :
    Known g (.nullable n) := by
  induction h with
  | intro hp _ ih =>
    obtain ⟨r,member,he⟩ := members_rows_aux (idx := 0) (base := 0) hp
    have eqn := Prod.mk.inj he
    subst_vars
    rw [← eqn.1]
    have rhs := suffix_null (g := g) member _ ih 0 (Nat.zero_le _) (by simpa using eqn.2)
    exact row_rule member (premises := [.suffixNull r.base]) (by simp [Layout.rowRules])
      (by simpa using rhs)

private theorem first_suffix {g : RawGrammar} {r : Row} (hr : r ∈ g.rows) {t : Nat}
    (ht : t < g.terminals) (pre : List (Sym Nat Nat)) (i : Nat) (hi : i ≤ r.rhs.length)
    (hn : NullableSeq g.cfg pre) {sym post} (eqn : r.rhs.drop i = pre ++ sym :: post)
    (begins : sym = .t t ∨ ∃ child, sym = .n child ∧ Known g (.first child (t+1))) :
    Known g (.suffixFirst (r.base+i) (t+1)) := by
  induction pre generalizing i with
  | nil =>
    obtain ⟨hit,_⟩ := at_split eqn hi
    rcases begins with rfl | ⟨n,rfl,hf⟩
    · exact occurrence_rule hr hit (premises := []) (by simp [Layout.symbolRules]) (by simp)
    · apply occurrence_rule hr hit (premises := [.first n (t+1)])
      · apply List.mem_append_right
        apply List.mem_flatMap.mpr
        exact ⟨t+1,List.mem_range.mpr (Nat.succ_lt_succ ht),by simp⟩
      · simpa using hf
  | cons hd pre ih =>
    have ⟨xs,hy,_⟩ := hn
    cases xs with
    | nil => contradiction
    | cons n ns =>
      simp only [List.map_cons, List.cons.injEq] at hy
      rw [hy.1] at hn eqn
      have ⟨hn',hp⟩ := null_cons.mp hn
      obtain ⟨hit,tail⟩ := at_split eqn hi
      have next := ih (i+1) (List.mem_zipIdx' hit).1 hp tail
      apply occurrence_rule hr hit (premises := [.nullable n, .suffixFirst (r.base+i+1) (t+1)])
      · apply List.mem_append_right
        apply List.mem_flatMap.mpr
        exact ⟨t+1,List.mem_range.mpr (Nat.succ_lt_succ ht),by simp⟩
      · simpa [Nat.add_assoc] using And.intro (nullable_complete hn') next

private theorem first_complete {g : RawGrammar} {n t} (ht : t < g.terminals) (h : First g.cfg n t) :
    Known g (.first n (t+1)) := by
  induction h with
  | @term _ _ _ a hp hn | @nonterm _ _ _ _ a hp hn _ ih =>
    all_goals
      obtain ⟨r,hr,he⟩ := members_rows_aux (idx := 0) (base := 0) hp
      have eqn := Prod.mk.inj he
      rw [← eqn.1]
      apply row_rule (g := g) hr (premises := [.suffixFirst r.base (a+1)])
      · simp only [Layout.rowRules, List.mem_append, List.mem_map]
        exact Or.inl (Or.inr ⟨a, List.mem_range.mpr (by simpa [RawGrammar.layout, RawGrammar.columns] using ht), rfl⟩)
      · simp only [List.forall_mem_singleton]
        have atStart := first_suffix (g := g) hr ht _ 0 (Nat.zero_le _) hn
          (by simpa only [List.drop_zero] using eqn.2)
        first | exact atStart (Or.inl rfl) | exact atStart (Or.inr ⟨_,rfl,ih ht⟩)

private theorem first_seq_complete {g : RawGrammar} {r : Row} (hr : r ∈ g.rows)
    {i t : Nat} (hi : i ≤ r.rhs.length) (ht : t < g.terminals)
    (h : FirstSeq g.cfg (r.rhs.drop i) t) : Known g (.suffixFirst (r.base+i) (t+1)) := by
  obtain ⟨pre,post,hp,alt⟩ := h
  rcases alt with eqn | ⟨child,eqn,hf⟩
  · exact first_suffix hr ht pre i hi hp eqn (Or.inl rfl)
  · exact first_suffix hr ht pre i hi hp eqn (Or.inr ⟨child,rfl,first_complete ht hf⟩)

private theorem null_seq_complete {g : RawGrammar} {r : Row} (hr : r ∈ g.rows)
    {i : Nat} (hi : i ≤ r.rhs.length) (h : NullableSeq g.cfg (r.rhs.drop i)) :
    Known g (.suffixNull (r.base+i)) := by
  obtain ⟨xs,eqn,all⟩ := h
  exact suffix_null hr xs (fun n hn => nullable_complete (all n hn)) i hi eqn

private def column : Option Nat → Nat
  | none => 0
  | some t => t+1

private theorem column_lookahead (c : Nat) : column (lookahead c) = c := by cases c <;> rfl
private theorem lookahead_column (t : Option Nat) : lookahead (column t) = t := by cases t <;> rfl

private theorem follow_complete {g : RawGrammar} {n b} (h : Follow g.cfg n b)
    (hc : column b < g.columns) : Known g (.follow n (column b)) := by
  induction h with
  | start =>
    exact .step ⟨Fact.follow g.cfg.start 0,[]⟩ List.mem_cons_self (by simp)
  | @first lhs child pre post a hp hs =>
    obtain ⟨r,hr,e⟩ := members_rows_aux (idx := 0) (base := 0) hp
    have eqn := (Prod.mk.inj e).2
    have hit : (Sym.n child,pre.length) ∈ r.rhs.zipIdx :=
      List.mem_zipIdx_iff_getElem?.mpr (by simp [eqn])
    have after : r.rhs.drop (pre.length+1) = post := by simp [eqn]
    have suff := first_seq_complete (g := g) hr (List.mem_zipIdx' hit).1
      (show a < g.terminals from Nat.lt_of_succ_lt_succ hc) (after ▸ hs)
    apply occurrence_rule hr hit (premises := [.suffixFirst (r.base + pre.length + 1) (a+1)])
    · apply List.mem_append_right
      exact List.mem_flatMap.mpr ⟨a+1,List.mem_range.mpr hc,by simp [column]⟩
    · simpa [Nat.add_assoc] using suff
  | @tail lhs child pre post b hp hs hf ih =>
    obtain ⟨r,hr,e⟩ := members_rows_aux (idx := 0) (base := 0) hp
    have eqn := (Prod.mk.inj e).2
    have hit : (Sym.n child,pre.length) ∈ r.rhs.zipIdx :=
      List.mem_zipIdx_iff_getElem?.mpr (by simp [eqn])
    have after : r.rhs.drop (pre.length+1) = post := by simp [eqn]
    have suff := null_seq_complete (g := g) hr (List.mem_zipIdx' hit).1 (after ▸ hs)
    apply occurrence_rule hr hit (premises := [.suffixNull (r.base+pre.length+1),.follow r.lhs (column b)])
    · apply List.mem_append_right
      exact List.mem_flatMap.mpr ⟨column b,List.mem_range.mpr hc,by simp⟩
    · simpa [Nat.add_assoc, (Prod.mk.inj e).1] using And.intro suff (ih hc)

private theorem row_limits_aux {ps : List (Nat × List (Sym Nat Nat))} {idx base : Nat} {r : Row}
    (h : r ∈ indexRows idx base ps) :
    idx ≤ r.index ∧ r.index < idx+ps.length ∧
      r.base+r.rhs.length < base + (ps.map (fun p => p.2.length+1)).sum ∧
      (indexRows idx base ps)[r.index-idx]? = some r ∧
      ps[r.index-idx]? = some (r.lhs,r.rhs) := by
  induction ps generalizing idx base with
  | nil => contradiction
  | cons p ps ih =>
    rcases List.mem_cons.mp h with rfl | tail
    · simp [indexRows]
      omega
    · obtain ⟨lo,hi,high,cell,shape⟩ := ih tail
      have off : r.index-idx = (r.index-(idx+1))+1 := by omega
      simp only [List.length_cons, List.map_cons, List.sum_cons, indexRows, off, List.getElem?_cons_succ]
      exact ⟨by omega,by omega,by omega,cell,shape⟩

private theorem row_limits {g : RawGrammar} {r : Row} (h : r ∈ g.rows) :
    r.index < g.cfg.prods.length ∧ r.base + r.rhs.length < g.positions ∧
      g.rows[r.index]? = some r ∧ g.cfg.prods[r.index]? = some (r.lhs,r.rhs) := by
  simpa [RawGrammar.positions, RawGrammar.rows] using (row_limits_aux h).2

private theorem indexRows_shapes (ps : List (Nat × List (Sym Nat Nat))) (idx base : Nat) :
    (indexRows idx base ps).map (fun r => (r.lhs,r.rhs)) = ps := by
  induction ps generalizing idx base with
  | nil => rfl
  | cons p ps ih => simp [indexRows, ih]

private theorem row_at {g : RawGrammar} {i : Nat} {n rhs} (h : g.cfg.prods[i]? = some (n,rhs)) :
    ∃ r ∈ g.rows, r.index = i ∧ r.lhs = n ∧ r.rhs = rhs := by
  have shape := congrArg (fun ps => ps[i]?) (indexRows_shapes g.cfg.prods 0 0)
  simp only [List.getElem?_map, h] at shape
  obtain ⟨r,hr,he⟩ := Option.map_eq_some_iff.mp shape
  have member := List.mem_of_getElem? hr
  have index_aux (ps : List (Nat × List (Sym Nat Nat))) (base idx k : Nat) (x : Row)
      (hx : (indexRows idx base ps)[k]? = some x) : x.index = idx+k := by
    induction ps generalizing base idx k with
    | nil => contradiction
    | cons p ps ih =>
      cases k with
      | zero => cases hx; rfl
      | succ k => exact (ih _ _ _ hx).trans (by omega)
  exact ⟨r,member,by simpa using index_aux _ _ _ _ r hr,(Prod.mk.inj he).1,(Prod.mk.inj he).2⟩

private theorem predict_correspondence {g : RawGrammar} {mem : Array UInt64}
    (accepted : (compileTableVerifier g).accepts mem g.budget = true)
    {r : Row} (hr : r ∈ g.rows) {c : Nat} (hc : c < g.columns) (hn : r.lhs < g.nonterminals) :
    g.layout.PredictsAt (fun i => mem[i]!) r c ↔ Predicts g.cfg r.index (lookahead c) := by
  have limits := row_limits hr
  have pos : r.base < g.positions := by omega
  have first := accepted_symbolic_exact accepted (f := .suffixFirst r.base c)
    (by simp [Fact.valid,RawGrammar.layout,pos,hc])
  have nullable := accepted_symbolic_exact accepted (f := .suffixNull r.base)
    (by simp [Fact.valid,RawGrammar.layout,pos])
  have follow := accepted_symbolic_exact accepted (f := .follow r.lhs c)
    (by simp [Fact.valid,RawGrammar.layout,hn,hc])
  change (0 < mem[g.layout.suffixFirst r.base c]!.toNat ∨
    0 < mem[g.layout.suffixNull r.base]!.toNat ∧ 0 < mem[g.layout.follow r.lhs c]!.toNat) ↔ _
  simp only [Fact.address] at first nullable follow
  rw [first,nullable,follow]
  have start := row_parts hr 0 (Nat.zero_le _)
  simp only [Nat.add_zero,List.take_zero,List.drop_zero] at start
  constructor
  · rintro (h | ⟨n,f⟩)
    · have meaning := symbolic_sound h
      simp only [Fact.Meaning,start] at meaning
      obtain ⟨t,rfl,ht⟩ := meaning
      exact ⟨r.lhs,r.rhs,limits.2.2.2,Or.inl ⟨t,rfl,ht⟩⟩
    · exact ⟨r.lhs,r.rhs,limits.2.2.2,Or.inr
        ⟨by simpa only [Fact.Meaning,start] using symbolic_sound n,symbolic_sound f⟩⟩
  · rintro ⟨lhs,rhs,idx,why⟩
    rw [limits.2.2.2] at idx
    cases idx
    rcases why with ⟨t,he,ht⟩ | ⟨n,f⟩
    · have col := congrArg column he
      rw [column_lookahead] at col
      change c = t+1 at col
      subst c
      apply Or.inl
      simpa using (first_seq_complete (g := g) hr (i := 0) (Nat.zero_le _)
        (Nat.lt_of_succ_lt_succ hc) (by simpa using ht))
    · exact Or.inr ⟨by simpa using null_seq_complete (g := g) hr (i := 0) (Nat.zero_le _) (by simpa using n),
        by simpa only [column_lookahead] using follow_complete f (by rwa [column_lookahead])⟩

/-- Each cell in the decoded prefix retains the actual initial word, including
empty and EOF cells. All indexing after this point uses natural addresses. -/
theorem decodeCells_at {l : Layout} {mem : Array UInt64} {cells : Array (Option Nat)}
    (decoded : decodeCells l mem = some cells) {k : Nat} (hk : k < l.n*l.c) :
    cells[k]? = some (if mem[k]! == 0 then none else some (mem[k]!.toNat-1)) := by
  by_cases h : mem.size = l.scratch + 1
  · simp [decodeCells,h,guard] at decoded
    subst cells
    have hfit : k < mem.size := by
      have bounds : l.n*l.c ≤ l.scratch := by
        simp [Layout.scratch,Layout.suffixFirst,Layout.suffixProductive,Layout.suffixNull,
          Layout.prefixNull,Layout.follow,Layout.firstNt,Layout.productive,Layout.nullNt,
          Layout.ntBase,Layout.rank]
        omega
      omega
    simp [Array.getElem?_extract,hfit]
    omega
  · simp [decodeCells,h,guard] at decoded
    contradiction

/-- Exact predictive interpretation of the accepted initial memory, relative to
the original ordered grammar. Multiplicity, the start/EOF seed and every empty
cell participate in this biconditional. -/
theorem accepted_lookup_iff {g : RawGrammar} {mem : Array UInt64} {cells : Array (Option Nat)}
    (accepted : (compileTableVerifier g).accepts mem g.budget = true)
    (decoded : decodeCells g.layout mem = some cells)
    {n c : Nat} (hn : n < g.nonterminals) (hc : c < g.columns) (i : Nat) :
    cells[g.layout.cell n c]?.join = some i ↔
      Predicts g.cfg i (lookahead c) ∧ ∃ rhs, g.cfg.prods[i]? = some (n,rhs) := by
  have ⟨rep,_,valid⟩ := (compileTableVerifier_correct ..).1 accepted
  have fit := (by
    simp only [representable,Bool.and_eq_true,decide_eq_true_eq] at rep
    exact rep.1.1.1.2 : g.cfg.prods.length < UInt64.size)
  have cell := valid.2.2.1 n hn c hc
  let w := mem[g.layout.cell n c]!
  have lookup := decodeCells_at decoded (k := g.layout.cell n c) (grid_bound hn hc)
  rw [lookup]
  change (if w == 0 then none else some (w.toNat-1)) = some i ↔ _
  constructor
  · intro found
    split at found
    · contradiction
    · rename_i nonzero
      have pos : 0 < w.toNat := by
        apply Nat.pos_of_ne_zero
        intro hz
        have eq : w = 0 := UInt64.toNat_inj.mp hz
        simp [eq] at nonzero
      obtain ⟨r,hr,eq,hpred⟩ := cell.1 pos
      have word := cell.2 r hr eq hpred
      have limits := row_limits hr
      have exactNat := congrArg UInt64.toNat word
      change w.toNat = (UInt64.ofNat (r.index+1)).toNat at exactNat
      rw [UInt64.toNat_ofNat_of_lt' (by omega)] at exactNat
      have index : i = r.index := by have hi := Option.some.inj found; omega
      subst i
      exact ⟨(predict_correspondence accepted hr hc (eq ▸ hn)).1 hpred, r.rhs,eq ▸ limits.2.2.2⟩
  · rintro ⟨predict,rhs,atRow⟩
    obtain ⟨r,hr,rfl,eq,_⟩ := row_at atRow
    have word := cell.2 r hr eq ((predict_correspondence accepted hr hc (eq ▸ hn)).2 predict)
    change w = (r.index+1).toUInt64 at word
    have limits := row_limits hr
    have exactNat : w.toNat = r.index+1 := by rw [word]; exact UInt64.toNat_ofNat_of_lt' (by omega)
    have nonzero : w ≠ 0 := by intro h; simp [h] at exactNat
    simp [nonzero,exactNat]

theorem accepted_productive {g : RawGrammar} {mem : Array UInt64}
    (h : (compileTableVerifier g).accepts mem g.budget = true) {n : Nat}
    (hn : n < g.nonterminals) : Productive g.cfg n := by
  have known := (compileTableVerifier_sound h).2.2.2.2 n hn
  exact symbolic_sound ((accepted_symbolic_exact h (f := .productive n)
    (by simp [Fact.valid,RawGrammar.layout,hn])).1 known)

private theorem prefix_steps {g : RawGrammar} {r : Row} (hr : r ∈ g.rows) (xs : List Nat)
    (nullable : ∀ n ∈ xs, Nullable g.cfg n) {i : Nat} (hi : i ≤ r.rhs.length)
    (known : Known g (.prefixNull (r.base+i))) (tail : List (Sym Nat Nat))
    (eqn : r.rhs.drop i = xs.map Sym.n ++ tail) :
    Known g (.prefixNull (r.base+i+xs.length)) := by
  induction xs generalizing i with
  | nil => simpa using known
  | cons n ns ih =>
    obtain ⟨hit,rest⟩ := at_split (by simpa using eqn) hi
    have next : Known g (.prefixNull (r.base+i+1)) := by
      apply occurrence_rule hr hit (premises := [.prefixNull (r.base+i),.nullable n])
      · simp [Layout.symbolRules]
      · simpa using And.intro known (nullable_complete (nullable n List.mem_cons_self))
    have fin := ih (fun k hk => nullable k (List.mem_cons_of_mem _ hk)) (List.mem_zipIdx' hit).1
      (by simpa [Nat.add_assoc] using next) rest
    simpa [Nat.add_assoc, Nat.add_left_comm, Nat.add_comm] using fin

theorem accepted_rank_decreases {g : RawGrammar} {mem : Array UInt64}
    (h : (compileTableVerifier g).accepts mem g.budget = true) {r : Row} (hr : r ∈ g.rows)
    {pre child post} (eqn : r.rhs = pre ++ .n child :: post) (hn : NullableSeq g.cfg pre) :
    mem[g.layout.rank child]!.toNat < mem[g.layout.rank r.lhs]!.toNat := by
  have hit : (Sym.n child,pre.length) ∈ r.rhs.zipIdx :=
    List.mem_zipIdx_iff_getElem?.mpr (by simp [eqn])
  have atBase : Known g (.prefixNull r.base) :=
    row_rule hr (premises := []) (by simp [Layout.rowRules]) (by simp)
  obtain ⟨xs,rfl,hn⟩ := hn
  have prefixKnown := prefix_steps hr xs hn (i := 0) (Nat.zero_le _) (by simpa using atBase)
    (.n child :: post) (by simpa using eqn)
  simp only [Nat.add_zero] at prefixKnown
  have limits := row_limits hr
  have bound : r.base+xs.length < g.positions := by simp [eqn] at limits; omega
  have positive := (accepted_symbolic_exact h (f := .prefixNull (r.base+xs.length))
    (by simp [Fact.valid,RawGrammar.layout,bound])).2 prefixKnown
  exact (compileTableVerifier_sound h).2.2.2.1 r hr child xs.length (by simpa using hit) positive

end ConcreteSpec.GrammarVerification
