/-!
# LL(1): a decidable check and its soundness

PROVISIONAL (slice 0). The grammar is always LL(1) (owner rule). This file gives a decision
procedure `ll1 : Grammar → Bool` and proves it sound against DECLARATIVE definitions of nullable,
FIRST, FOLLOW and prediction (`ll1_sound`): when `ll1 g = true`, no lookahead predicts two
different productions of one nonterminal, so a table-driven parser never has a choice to make.

The check is certificate-style: `compute` iterates to a fixed point, and `check` verifies that the
sets are CLOSED under the rules and that the prediction sets are disjoint. Soundness needs only
closure (a closed set contains the least one, which is the declarative relation), so the iteration
itself needs no proof; if it stops short, `check` fails and the grammar is refused.

The check is generic (any terminal and nonterminal types with decidable equality). Where the
language's grammar lives, and so where this file belongs, follows the spec's unification plan
(A2i): the grammar is spec data, and this file moves beside it when it lands.
-/

namespace Concrete.LL1

inductive Sym (T N : Type) where
  | t (a : T)
  | n (x : N)
  deriving DecidableEq, Repr

structure Grammar (T N : Type) where
  start : N
  prods : List (N × List (Sym T N))
  deriving Repr

variable {T N : Type} [DecidableEq T] [DecidableEq N]

/-! ## The declarative relations -/

section Decl
variable (g : Grammar T N)

/-- `x` derives the empty string. -/
inductive Nullable : N → Prop
  | intro {x : N} {xs : List N} :
      (x, xs.map Sym.n) ∈ g.prods → (∀ y ∈ xs, Nullable y) → Nullable x

/-- A sequence derives the empty string: it is all nonterminals, each nullable. -/
def NullableSeq (α : List (Sym T N)) : Prop :=
  ∃ xs : List N, α = xs.map Sym.n ∧ ∀ y ∈ xs, Nullable g y

/-- `a` can begin a string `x` derives. -/
inductive First : N → T → Prop
  | term {x : N} {pre post : List (Sym T N)} {a : T} :
      (x, pre ++ Sym.t a :: post) ∈ g.prods → NullableSeq g pre → First x a
  | nonterm {x y : N} {pre post : List (Sym T N)} {a : T} :
      (x, pre ++ Sym.n y :: post) ∈ g.prods → NullableSeq g pre → First y a → First x a

/-- `a` can begin a string the sequence `α` derives. -/
def FirstSeq (α : List (Sym T N)) (a : T) : Prop :=
  ∃ pre post, NullableSeq g pre ∧
    (α = pre ++ Sym.t a :: post ∨ ∃ y, α = pre ++ Sym.n y :: post ∧ First g y a)

/-- `b` can follow `x` in a sentential form (`none` is the end of input). -/
inductive Follow : N → Option T → Prop
  | start : Follow g.start none
  | first {y x : N} {pre post : List (Sym T N)} {a : T} :
      (y, pre ++ Sym.n x :: post) ∈ g.prods → FirstSeq g post a → Follow x (some a)
  | tail {y x : N} {pre post : List (Sym T N)} {b : Option T} :
      (y, pre ++ Sym.n x :: post) ∈ g.prods → NullableSeq g post → Follow y b → Follow x b

/-- Production `i` is predicted on lookahead `b`. -/
def Predicts (i : Nat) (b : Option T) : Prop :=
  ∃ x rhs, g.prods[i]? = some (x, rhs) ∧
    ((∃ a, b = some a ∧ FirstSeq g rhs a) ∨ (NullableSeq g rhs ∧ Follow g x b))

end Decl

/-! ## The computed sets and the check -/

def symNullB (nu : List N) : Sym T N → Bool
  | .t _ => false
  | .n x => nu.contains x

def nullSeqB (nu : List N) (α : List (Sym T N)) : Bool := α.all (symNullB nu)

/-- The FIRST set of `x` in the table `fi`. -/
def firstOf (fi : List (N × T)) (x : N) : List T :=
  fi.filterMap fun (z, a) => if z = x then some a else none

def firstSeqB (nu : List N) (fi : List (N × T)) : List (Sym T N) → List T
  | [] => []
  | .t a :: _ => [a]
  | .n y :: rest => firstOf fi y ++ (if nu.contains y then firstSeqB nu fi rest else [])

/-- Every way to split a sequence around one symbol. -/
def splits : List (Sym T N) → List (List (Sym T N) × Sym T N × List (Sym T N))
  | [] => []
  | s :: rest => ([], s, rest) :: (splits rest).map fun (p, s', q) => (s :: p, s', q)

def followOf (fo : List (N × Option T)) (x : N) : List (Option T) :=
  fo.filterMap fun (z, b) => if z = x then some b else none

def nuClosed (g : Grammar T N) (nu : List N) : Bool :=
  g.prods.all fun (x, rhs) => !nullSeqB nu rhs || nu.contains x

def fiClosed (g : Grammar T N) (nu : List N) (fi : List (N × T)) : Bool :=
  g.prods.all fun (x, rhs) => (firstSeqB nu fi rhs).all fun a => fi.contains (x, a)

def foClosed (g : Grammar T N) (nu : List N) (fi : List (N × T)) (fo : List (N × Option T)) :
    Bool :=
  fo.contains (g.start, none) &&
  g.prods.all fun (y, rhs) => (splits rhs).all fun (_, s, post) =>
    match s with
    | .t _ => true
    | .n x => (firstSeqB nu fi post).all (fun a => fo.contains (x, some a)) &&
        (!nullSeqB nu post || (followOf fo y).all fun b => fo.contains (x, b))

/-- The lookaheads that predict a production `x → rhs`. -/
def predictB (nu : List N) (fi : List (N × T)) (fo : List (N × Option T)) (x : N)
    (rhs : List (Sym T N)) : List (Option T) :=
  (firstSeqB nu fi rhs).map some ++ (if nullSeqB nu rhs then followOf fo x else [])

def conflictFree (g : Grammar T N) (nu : List N) (fi : List (N × T)) (fo : List (N × Option T)) :
    Bool :=
  (List.range g.prods.length).all fun i => (List.range g.prods.length).all fun j =>
    i == j || match g.prods[i]?, g.prods[j]? with
      | some (x, r), some (y, s) =>
        x != y || (predictB nu fi fo x r).all fun b => !(predictB nu fi fo y s).contains b
      | _, _ => true

/-- The check over given sets: closed, and the predictions of each nonterminal disjoint. -/
def check (g : Grammar T N) (nu : List N) (fi : List (N × T)) (fo : List (N × Option T)) : Bool :=
  nuClosed g nu && fiClosed g nu fi && foClosed g nu fi fo && conflictFree g nu fi fo

/-- Add `step s` to `s` until nothing new appears (or the fuel runs out). -/
def saturate {α : Type} [DecidableEq α] (step : List α → List α) : Nat → List α → List α
  | 0, s => s
  | fuel + 1, s =>
    let new := (step s).filter (fun a => !s.contains a) |>.eraseDups
    if new.isEmpty then s else saturate step fuel (s ++ new)

def terminals (g : Grammar T N) : List T :=
  (g.prods.flatMap fun (_, rhs) => rhs.filterMap fun | .t a => some a | .n _ => none).eraseDups

/-- The sets, by fixed-point iteration. The fuel bounds the number of pairs that can be added. -/
def compute (g : Grammar T N) : List N × List (N × T) × List (N × Option T) :=
  let k := g.prods.length + 1
  let fuel := k * (terminals g).length + 2 * k + 2
  let nu := saturate (fun nu => g.prods.filterMap fun (x, rhs) =>
    if nullSeqB nu rhs then some x else none) fuel []
  let fi := saturate (fun fi => g.prods.flatMap fun (x, rhs) =>
    (firstSeqB nu fi rhs).map fun a => (x, a)) fuel []
  let fo := saturate (fun fo => g.prods.flatMap fun (y, rhs) => (splits rhs).flatMap
    fun (_, s, post) => match s with
      | .t _ => []
      | .n x => (firstSeqB nu fi post).map (fun a => (x, some a)) ++
          (if nullSeqB nu post then (followOf fo y).map fun b => (x, b) else [])) fuel
    [(g.start, none)]
  (nu, fi, fo)

/-- The grammar is LL(1). -/
def ll1 (g : Grammar T N) : Bool :=
  let (nu, fi, fo) := compute g
  check g nu fi fo

/-! ## Soundness -/

section Sound
variable {g : Grammar T N} {nu : List N} {fi : List (N × T)} {fo : List (N × Option T)}

omit [DecidableEq T] in
theorem nullSeqB_map (xs : List N) :
    nullSeqB nu (xs.map (Sym.n (T := T))) = xs.all (nu.contains ·) := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
    simp only [nullSeqB, List.map_cons, List.all_cons, symNullB] at ih ⊢
    rw [ih]

omit [DecidableEq T] in
theorem nullable_sound (h : nuClosed g nu = true) {x : N} (hx : Nullable g x) : x ∈ nu := by
  induction hx with
  | @intro x xs hp _ ih =>
    have hall : nullSeqB nu (xs.map (Sym.n (T := T))) = true := by
      rw [nullSeqB_map]; simpa using ih
    have := List.all_eq_true.mp h _ hp
    simp only [hall, Bool.not_true, Bool.false_or] at this
    simpa using this

omit [DecidableEq T] in
theorem nullSeq_sound (h : nuClosed g nu = true) {α : List (Sym T N)} (hα : NullableSeq g α) :
    nullSeqB nu α = true := by
  obtain ⟨xs, rfl, hxs⟩ := hα
  rw [nullSeqB_map]
  simpa using fun y hy => nullable_sound h (hxs y hy)

omit [DecidableEq T] in
theorem firstSeqB_append (pre rest : List (Sym T N)) (hpre : nullSeqB nu pre = true) {a : T}
    (ha : a ∈ firstSeqB nu fi rest) : a ∈ firstSeqB nu fi (pre ++ rest) := by
  induction pre with
  | nil => simpa using ha
  | cons s pre ih =>
    cases s with
    | t b => simp [nullSeqB, symNullB] at hpre
    | n y =>
      simp only [nullSeqB, List.all_cons, symNullB, Bool.and_eq_true] at hpre
      simp only [List.cons_append, firstSeqB, hpre.1, ↓reduceIte, List.mem_append]
      exact Or.inr (ih hpre.2)

omit [DecidableEq T] in
theorem mem_firstOf {x : N} {a : T} (h : (x, a) ∈ fi) : a ∈ firstOf fi x := by
  simp only [firstOf, List.mem_filterMap]
  exact ⟨(x, a), h, by simp⟩

theorem first_sound (hnu : nuClosed g nu = true) (hfi : fiClosed g nu fi = true) {x : N} {a : T}
    (hx : First g x a) : (x, a) ∈ fi := by
  induction hx with
  | @term x pre post a hp hpre =>
    have ha : a ∈ firstSeqB nu fi (pre ++ Sym.t a :: post) :=
      firstSeqB_append pre _ (nullSeq_sound hnu hpre) (by simp [firstSeqB])
    have := List.all_eq_true.mp hfi _ hp
    simpa using List.all_eq_true.mp this a ha
  | @nonterm x y pre post a hp hpre _ ih =>
    have ha : a ∈ firstSeqB nu fi (pre ++ Sym.n y :: post) :=
      firstSeqB_append pre _ (nullSeq_sound hnu hpre)
        (by simp only [firstSeqB, List.mem_append]; exact Or.inl (mem_firstOf ih))
    have := List.all_eq_true.mp hfi _ hp
    simpa using List.all_eq_true.mp this a ha

theorem firstSeq_sound (hnu : nuClosed g nu = true) (hfi : fiClosed g nu fi = true)
    {α : List (Sym T N)} {a : T} (h : FirstSeq g α a) : a ∈ firstSeqB nu fi α := by
  obtain ⟨pre, post, hpre, hα⟩ := h
  rcases hα with rfl | ⟨y, rfl, hy⟩
  · exact firstSeqB_append pre _ (nullSeq_sound hnu hpre) (by simp [firstSeqB])
  · exact firstSeqB_append pre _ (nullSeq_sound hnu hpre)
      (by simp only [firstSeqB, List.mem_append]; exact Or.inl (mem_firstOf (first_sound hnu hfi hy)))

omit [DecidableEq T] [DecidableEq N] in
theorem mem_splits (pre post : List (Sym T N)) (s : Sym T N) :
    (pre, s, post) ∈ splits (pre ++ s :: post) := by
  induction pre with
  | nil => simp [splits]
  | cons p pre ih =>
    simp only [List.cons_append, splits, List.mem_cons, List.mem_map]
    exact Or.inr ⟨(pre, s, post), ih, rfl⟩

omit [DecidableEq T] in
theorem mem_followOf {x : N} {b : Option T} (h : (x, b) ∈ fo) : b ∈ followOf fo x := by
  simp only [followOf, List.mem_filterMap]
  exact ⟨(x, b), h, by simp⟩

theorem follow_sound (hnu : nuClosed g nu = true) (hfi : fiClosed g nu fi = true)
    (hfo : foClosed g nu fi fo = true) {x : N} {b : Option T} (h : Follow g x b) : (x, b) ∈ fo := by
  simp only [foClosed, Bool.and_eq_true] at hfo
  obtain ⟨hstart, hrules⟩ := hfo
  induction h with
  | start => simpa using hstart
  | @first y x pre post a hp ha =>
    have hr := List.all_eq_true.mp (List.all_eq_true.mp hrules _ hp) _ (mem_splits pre post _)
    simp only [Bool.and_eq_true] at hr
    simpa using List.all_eq_true.mp hr.1 a (firstSeq_sound hnu hfi ha)
  | @tail y x pre post b hp hpost _ ih =>
    have hr := List.all_eq_true.mp (List.all_eq_true.mp hrules _ hp) _ (mem_splits pre post _)
    simp only [Bool.and_eq_true, nullSeq_sound hnu hpost, Bool.not_true, Bool.false_or] at hr
    simpa using List.all_eq_true.mp hr.2 b (mem_followOf ih)

theorem predicts_sound (hnu : nuClosed g nu = true) (hfi : fiClosed g nu fi = true)
    (hfo : foClosed g nu fi fo = true) {i : Nat} {b : Option T} {x : N} {rhs : List (Sym T N)}
    (hi : g.prods[i]? = some (x, rhs)) (h : Predicts g i b) : b ∈ predictB nu fi fo x rhs := by
  obtain ⟨x', rhs', hi', hp⟩ := h
  rw [hi] at hi'
  cases hi'
  simp only [predictB, List.mem_append, List.mem_map]
  rcases hp with ⟨a, rfl, ha⟩ | ⟨hn, hf⟩
  · exact Or.inl ⟨a, firstSeq_sound hnu hfi ha, rfl⟩
  · exact Or.inr (by simp only [nullSeq_sound hnu hn, ↓reduceIte]; exact mem_followOf (follow_sound hnu hfi hfo hf))

/-- **Soundness of the check.** If `check` accepts the sets, two productions of one nonterminal
predicted on the same lookahead are the same production. -/
theorem check_sound (h : check g nu fi fo = true) {i j : Nat} {x : N} {b : Option T}
    (hi : (g.prods[i]?.map Prod.fst) = some x) (hj : (g.prods[j]?.map Prod.fst) = some x)
    (pi : Predicts g i b) (pj : Predicts g j b) : i = j := by
  simp only [check, Bool.and_eq_true] at h
  obtain ⟨⟨⟨hnu, hfi⟩, hfo⟩, hcf⟩ := h
  cases hgi : g.prods[i]? with
  | none => simp [hgi] at hi
  | some pr =>
  cases hgj : g.prods[j]? with
  | none => simp [hgj] at hj
  | some ps =>
  obtain ⟨xi, ri⟩ := pr
  obtain ⟨xj, rj⟩ := ps
  simp only [hgi, hgj, Option.map_some, Option.some.injEq] at hi hj
  subst hi hj
  have bi := predicts_sound hnu hfi hfo hgi pi
  have bj := predicts_sound hnu hfi hfo hgj pj
  have hil : i < g.prods.length := by
    rcases Nat.lt_or_ge i g.prods.length with h | h
    · exact h
    · simp [List.getElem?_eq_none h] at hgi
  have hjl : j < g.prods.length := by
    rcases Nat.lt_or_ge j g.prods.length with h | h
    · exact h
    · simp [List.getElem?_eq_none h] at hgj
  have hc := List.all_eq_true.mp (List.all_eq_true.mp hcf i (List.mem_range.mpr hil)) j
    (List.mem_range.mpr hjl)
  simp only [hgi, hgj, bne_self_eq_false, Bool.false_or, Bool.or_eq_true, beq_iff_eq] at hc
  rcases hc with hc | hc
  · exact hc
  · have := List.all_eq_true.mp hc b bi
    simp [bj] at this

/-- **LL(1) soundness.** If `ll1 g = true`, no lookahead predicts two different productions of one
nonterminal. -/
theorem ll1_sound (h : ll1 g = true) {i j : Nat} {x : N} {b : Option T}
    (hi : (g.prods[i]?.map Prod.fst) = some x) (hj : (g.prods[j]?.map Prod.fst) = some x)
    (pi : Predicts g i b) (pj : Predicts g j b) : i = j :=
  check_sound h hi hj pi pj

end Sound

/-! ## Examples -/

namespace Examples

inductive Tm where
  | num | plus | lparen | rparen
  deriving DecidableEq, Repr

inductive Nt where
  | expr | rest | atom
  deriving DecidableEq, Repr

open Sym in
/-- `expr → atom rest`, `rest → + atom rest | ε`, `atom → num | ( expr )`: LL(1). -/
def sums : Grammar Tm Nt :=
  { start := .expr,
    prods := [(.expr, [n .atom, n .rest]),
              (.rest, [t .plus, n .atom, n .rest]), (.rest, []),
              (.atom, [t .num]), (.atom, [t .lparen, n .expr, t .rparen])] }

example : ll1 sums = true := by decide

open Sym in
/-- `expr → atom + expr | atom`: both alternatives start with `atom`, so not LL(1). -/
def unfactored : Grammar Tm Nt :=
  { start := .expr,
    prods := [(.expr, [n .atom, t .plus, n .expr]), (.expr, [n .atom]),
              (.atom, [t .num]), (.atom, [t .lparen, n .expr, t .rparen])] }

example : ll1 unfactored = false := by decide

open Sym in
/-- A FIRST/FOLLOW conflict: `rest → + atom | ε` where `+` can also follow `rest`. -/
def followConflict : Grammar Tm Nt :=
  { start := .expr,
    prods := [(.expr, [n .rest, t .plus, n .atom]),
              (.rest, [t .plus, n .atom]), (.rest, []),
              (.atom, [t .num])] }

example : ll1 followConflict = false := by decide

end Examples

end Concrete.LL1
