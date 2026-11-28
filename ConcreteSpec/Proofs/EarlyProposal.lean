import ConcreteSpec.Proofs.TableVerifier
import ConcreteSpec.Proofs.TableVerifierBounds
import ConcreteSpec.Spec.EarlyProposal
import ConcreteSpec.Spec.LL1
import ConcreteSpec.Spec.TableVerifier
import ConcreteSpec.Spec.VerifyMachine

/-! Structural completeness proof for `earlyProposal`. Every obligation in
`ValidCertificate` is reduced to sparse checks over the 1,106 nonzero entries
of `earlyProposalTree` and the 78 rows of `earlyRawGrammar`, then combined
with `compileTableVerifier_complete` without running `verifyRun` in the kernel. -/

namespace ConcreteSpec.GrammarVerification
open Concrete.LL1

/-! ## General sparse tree bridge -/

private def SparseTree.allNodes (p : Nat → UInt64 → Bool) : SparseTree → Bool
  | .leaf => true
  | .node addr val left right =>
    p addr val && left.allNodes p && right.allNodes p

private theorem u64_bne_zero_iff (w : UInt64) : (w != 0) = true ↔ 0 < w.toNat := by
  simp only [bne_iff_ne, ne_eq, Nat.pos_iff_ne_zero]
  exact not_congr (UInt64.toNat_inj (a := w) (b := 0)).symm

private theorem getD_setIfInBounds_ne (arr : Array UInt64) (a x : Nat) (v : UInt64)
    (hne : x ≠ a) : (arr.setIfInBounds a v).getD x 0 = arr.getD x 0 := by
  by_cases ha : a < arr.size
  · by_cases hx : x < arr.size
    · simp [Array.getD, hx, hne.symm]
    · simp [Array.getD, hx]
  · simp [Array.setIfInBounds, ha]

private theorem getD_setIfInBounds_eq (arr : Array UInt64) (a : Nat) (v : UInt64)
    (hlt : a < arr.size) : (arr.setIfInBounds a v).getD a 0 = v := by
  simp [Array.getD, hlt]

private theorem SparseTree.fill_size (t : SparseTree) (arr : Array UInt64) :
    (t.fill arr).size = arr.size := by
  induction t generalizing arr with
  | leaf => rfl
  | node a v l r ihl ihr =>
    rw [SparseTree.fill, ihr, Array.size_setIfInBounds, ihl]

private theorem SparseTree.fill_getD_outside {t : SparseTree} {lo hi : Nat}
    (hb : t.bounded lo hi = true) (arr : Array UInt64) {x : Nat} (hx : x < lo ∨ hi ≤ x) :
    (t.fill arr).getD x 0 = arr.getD x 0 := by
  induction t generalizing lo hi arr with
  | leaf => rfl
  | node a v l r ihl ihr =>
    simp only [SparseTree.bounded, Bool.and_eq_true, decide_eq_true_eq] at hb
    obtain ⟨⟨⟨⟨hlo, hhi⟩, _⟩, hl⟩, hr⟩ := hb
    have hne : x ≠ a := by omega
    rw [SparseTree.fill,
      ihr hr ((l.fill arr).setIfInBounds a v) (by omega),
      getD_setIfInBounds_ne _ a x v hne,
      ihl hl arr (by omega)]

private theorem SparseTree.fill_getD_general {t : SparseTree} {lo hi : Nat}
    (hb : t.bounded lo hi = true) (arr : Array UInt64) (harr : hi ≤ arr.size) (x : Nat) :
    (t.fill arr).getD x 0 = if t.lookup x != 0 then t.lookup x else arr.getD x 0 := by
  induction t generalizing lo hi arr with
  | leaf => simp [SparseTree.fill, SparseTree.lookup]
  | node a v l r ihl ihr =>
    simp only [SparseTree.bounded, Bool.and_eq_true, decide_eq_true_eq] at hb
    obtain ⟨⟨⟨⟨hlo, hhi⟩, hv⟩, hl⟩, hr⟩ := hb
    rw [SparseTree.fill, SparseTree.lookup]
    by_cases hlt : x < a
    · simp only [hlt, ↓reduceIte]
      rw [SparseTree.fill_getD_outside hr _ (Or.inl (by omega)),
        getD_setIfInBounds_ne _ a x v (by omega),
        ihl hl arr (by omega)]
    · by_cases hgt : a < x
      · simp only [hlt, hgt, ↓reduceIte]
        rw [ihr hr ((l.fill arr).setIfInBounds a v)
            (by rw [Array.size_setIfInBounds, SparseTree.fill_size]; exact harr),
          getD_setIfInBounds_ne _ a x v (by omega),
          SparseTree.fill_getD_outside hl arr (Or.inr (by omega))]
      · have heq : x = a := by omega
        subst heq
        simp only [hlt, ↓reduceIte, hv]
        rw [SparseTree.fill_getD_outside hr _ (Or.inl (by omega)),
          getD_setIfInBounds_eq _ x v (by rw [SparseTree.fill_size]; omega)]

private theorem SparseTree.fill_replicate_getD {t : SparseTree} {size : Nat}
    (hb : t.bounded 0 size = true) (x : Nat) :
    (t.fill (Array.replicate size 0)).getD x 0 = t.lookup x := by
  rw [SparseTree.fill_getD_general hb _ (by simp)]
  have hrep : (Array.replicate size (0 : UInt64)).getD x 0 = 0 := by
    by_cases h : x < size <;> simp [Array.getD, h]
  rw [hrep]
  cases hnz : (t.lookup x != 0)
  · simp only [bne_eq_false_iff_eq] at hnz
    simp [hnz]
  · simp

private theorem array_getElemBang_eq_getD (arr : Array UInt64) (x : Nat) :
    arr[x]! = arr.getD x 0 := by
  by_cases h : x < arr.size
  · simp [Array.getD, getElem!_pos, h]
  · simp only [Array.getD, h, ↓reduceDIte]
    rw [getElem!_neg arr x h]
    rfl

private theorem SparseTree.lookup_pos_of_allNodes {t : SparseTree} {lo hi : Nat}
    (hb : t.bounded lo hi = true) {p : Nat → UInt64 → Bool}
    (hall : t.allNodes p = true) {x : Nat} (hpos : 0 < (t.lookup x).toNat) :
    p x (t.lookup x) = true := by
  induction t generalizing lo hi with
  | leaf =>
    simp [SparseTree.lookup] at hpos
  | node a v l r ihl ihr =>
    simp only [SparseTree.bounded, Bool.and_eq_true, decide_eq_true_eq] at hb
    obtain ⟨⟨⟨⟨_, _⟩, _⟩, hl⟩, hr⟩ := hb
    simp only [SparseTree.allNodes, Bool.and_eq_true] at hall
    obtain ⟨⟨hpa, hpl⟩, hpr⟩ := hall
    simp only [SparseTree.lookup] at hpos ⊢
    split at hpos
    · rename_i hlt
      simpa [hlt] using ihl hl hpl hpos
    · split at hpos
      · rename_i hlt hgt
        simpa [hlt, hgt] using ihr hr hpr hpos
      · rename_i hlt hgt
        have heq : x = a := by omega
        subst heq
        simpa [hlt, hgt] using hpa

/-! ## Layout decoding and arithmetic helpers -/

private theorem grid_div_mod {i c j : Nat} (hj : j < c) :
    (i * c + j) / c = i ∧ (i * c + j) % c = j := by
  have hc : 0 < c := by omega
  constructor
  · rw [Nat.mul_comm, Nat.mul_add_div hc, Nat.div_eq_of_lt hj, Nat.add_zero]
  · rw [Nat.mul_comm, Nat.mul_add_mod, Nat.mod_eq_of_lt hj]

private def decodeFact (l : Layout) (a : Nat) : Option Fact :=
  if a < l.ntBase then none
  else if a < l.productive 0 then some (.nullable (a - l.nullNt 0))
  else if a < l.firstNt 0 0 then some (.productive (a - l.productive 0))
  else if a < l.follow 0 0 then
    let off := a - l.firstNt 0 0
    some (.first (off / l.c) (off % l.c))
  else if a < l.prefixNull 0 then
    let off := a - l.follow 0 0
    some (.follow (off / l.c) (off % l.c))
  else if a < l.suffixNull 0 then some (.prefixNull (a - l.prefixNull 0))
  else if a < l.suffixProductive 0 then some (.suffixNull (a - l.suffixNull 0))
  else if a < l.suffixFirst 0 0 then some (.suffixProductive (a - l.suffixProductive 0))
  else if a < l.scratch then
    let off := a - l.suffixFirst 0 0
    some (.suffixFirst (off / l.c) (off % l.c))
  else none

private theorem nat_beq_self (n : Nat) : (n == n) = true := decide_eq_true rfl

private theorem nat_div_lt_of_lt_mul {a b c : Nat} (h : a < b * c) : a / c < b := by
  match Nat.lt_or_ge (a / c) b with
  | .inl hlt => exact hlt
  | .inr hge =>
    have hmul : b * c ≤ a / c * c := Nat.mul_le_mul_right c hge
    have hdm : a / c * c + a % c = a := by rw [Nat.mul_comm]; exact Nat.div_add_mod a c
    omega

private theorem decodeFact_sound {l : Layout} (hc : 0 < l.c) {a : Nat}
    (hfact : l.isFact a = true) :
    ∃ f, decodeFact l a = some f ∧ f.valid l = true ∧ f.address l = a := by
  simp only [Layout.isFact, Bool.and_eq_true, decide_eq_true_eq] at hfact
  obtain ⟨hlo, hhi⟩ := hfact
  unfold decodeFact
  simp only [show ¬ a < l.ntBase by omega, ↓reduceIte]
  split
  · rename_i h
    refine ⟨_, rfl, ?_, ?_⟩
    · simp only [Fact.valid, decide_eq_true_eq]
      simp only [Layout.productive, Layout.nullNt] at h ⊢; omega
    · simp only [Fact.address, Layout.nullNt]; omega
  split
  · rename_i h1 h2
    refine ⟨_, rfl, ?_, ?_⟩
    · simp only [Fact.valid, decide_eq_true_eq]
      simp only [Layout.firstNt, Layout.productive, Layout.nullNt] at h1 h2 ⊢; omega
    · simp only [Fact.address, Layout.productive, Layout.nullNt] at h1 ⊢; omega
  split
  · rename_i h1 h2 h3
    have hoff : a - l.firstNt 0 0 < l.n * l.c := by
      simp only [Layout.follow, Layout.firstNt] at h2 h3 ⊢; omega
    have hdiv : (a - l.firstNt 0 0) / l.c < l.n :=
      nat_div_lt_of_lt_mul hoff
    have hmod : (a - l.firstNt 0 0) % l.c < l.c := Nat.mod_lt _ hc
    refine ⟨_, rfl, by simp [Fact.valid, hdiv, hmod], ?_⟩
    have hdm : (a - l.firstNt 0 0) / l.c * l.c + (a - l.firstNt 0 0) % l.c = a - l.firstNt 0 0 := by
      rw [Nat.mul_comm]; exact Nat.div_add_mod (a - l.firstNt 0 0) l.c
    have hge : l.firstNt 0 0 ≤ a := by omega
    have hbase : l.firstNt ((a - l.firstNt 0 0) / l.c) ((a - l.firstNt 0 0) % l.c) =
        l.firstNt 0 0 + ((a - l.firstNt 0 0) / l.c * l.c + (a - l.firstNt 0 0) % l.c) := by
      simp only [Layout.firstNt]; omega
    simp only [Fact.address]
    omega
  split
  · rename_i h1 h2 h3 h4
    have hoff : a - l.follow 0 0 < l.n * l.c := by
      simp only [Layout.prefixNull, Layout.follow, Layout.firstNt] at h3 h4 ⊢; omega
    have hdiv : (a - l.follow 0 0) / l.c < l.n :=
      nat_div_lt_of_lt_mul hoff
    have hmod : (a - l.follow 0 0) % l.c < l.c := Nat.mod_lt _ hc
    refine ⟨_, rfl, by simp [Fact.valid, hdiv, hmod], ?_⟩
    have hdm : (a - l.follow 0 0) / l.c * l.c + (a - l.follow 0 0) % l.c = a - l.follow 0 0 := by
      rw [Nat.mul_comm]; exact Nat.div_add_mod (a - l.follow 0 0) l.c
    have hge : l.follow 0 0 ≤ a := by omega
    have hbase : l.follow ((a - l.follow 0 0) / l.c) ((a - l.follow 0 0) % l.c) =
        l.follow 0 0 + ((a - l.follow 0 0) / l.c * l.c + (a - l.follow 0 0) % l.c) := by
      simp only [Layout.follow]; omega
    simp only [Fact.address]
    omega
  split
  · rename_i h1 h2 h3 h4 h5
    refine ⟨_, rfl, ?_, ?_⟩
    · simp only [Fact.valid, decide_eq_true_eq]
      simp only [Layout.suffixNull, Layout.prefixNull] at h4 h5 ⊢; omega
    · simp only [Fact.address, Layout.prefixNull] at h4 ⊢; omega
  split
  · rename_i h1 h2 h3 h4 h5 h6
    refine ⟨_, rfl, ?_, ?_⟩
    · simp only [Fact.valid, decide_eq_true_eq]
      simp only [Layout.suffixProductive, Layout.suffixNull] at h5 h6 ⊢; omega
    · simp only [Fact.address, Layout.suffixNull] at h5 ⊢; omega
  split
  · rename_i h1 h2 h3 h4 h5 h6 h7
    refine ⟨_, rfl, ?_, ?_⟩
    · simp only [Fact.valid, decide_eq_true_eq]
      simp only [Layout.suffixFirst, Layout.suffixProductive] at h6 h7 ⊢; omega
    · simp only [Fact.address, Layout.suffixProductive] at h6 ⊢; omega
  · rename_i h1 h2 h3 h4 h5 h6 h7
    have hoff : a - l.suffixFirst 0 0 < l.positions * l.c := by
      simp only [Layout.scratch, Layout.suffixFirst] at h7 hhi ⊢; omega
    have hdiv : (a - l.suffixFirst 0 0) / l.c < l.positions :=
      nat_div_lt_of_lt_mul hoff
    have hmod : (a - l.suffixFirst 0 0) % l.c < l.c := Nat.mod_lt _ hc
    refine ⟨_, rfl, by simp [Fact.valid, hdiv, hmod], ?_⟩
    have hdm : (a - l.suffixFirst 0 0) / l.c * l.c + (a - l.suffixFirst 0 0) % l.c =
        a - l.suffixFirst 0 0 := by
      rw [Nat.mul_comm]; exact Nat.div_add_mod (a - l.suffixFirst 0 0) l.c
    have hge : l.suffixFirst 0 0 ≤ a := by omega
    have hbase : l.suffixFirst ((a - l.suffixFirst 0 0) / l.c) ((a - l.suffixFirst 0 0) % l.c) =
        l.suffixFirst 0 0 +
          ((a - l.suffixFirst 0 0) / l.c * l.c + (a - l.suffixFirst 0 0) % l.c) := by
      simp only [Layout.suffixFirst]; omega
    simp only [Fact.address]
    omega

/-! ## General sparse certificate checks and structural soundness -/

private def checkProductive (l : Layout) (t : SparseTree) : Bool :=
  (List.range l.n).all fun n => t.lookup (l.productive n) != 0

private def checkDescent (l : Layout) (rows : List Row) (t : SparseTree) : Bool :=
  rows.all fun r => r.rhs.zipIdx.all fun (sym, i) =>
    match sym with
    | .t _ => true
    | .n child =>
      !(t.lookup (l.prefixNull (r.base + i)) != 0) ||
        decide (t.lookup (l.rank child) < t.lookup (l.rank r.lhs))

private def predictsB (l : Layout) (t : SparseTree) (r : Row) (col : Nat) : Bool :=
  (t.lookup (l.suffixFirst r.base col) != 0) ||
    ((t.lookup (l.suffixNull r.base) != 0) && (t.lookup (l.follow r.lhs col) != 0))

private def checkCellNode (l : Layout) (rows : List Row) (t : SparseTree) (a : Nat)
    (v : UInt64) : Bool :=
  if a < l.n * l.c then
    let n := a / l.c
    let col := a % l.c
    match rows[v.toNat - 1]? with
    | some r => decide (r.lhs = n) && predictsB l t r col
    | none => false
  else true

private def checkPredNode (l : Layout) (rows : List Row) (t : SparseTree) (a : Nat)
    (_ : UInt64) : Bool :=
  if l.follow 0 0 ≤ a && a < l.prefixNull 0 then
    let off := a - l.follow 0 0
    let lhs := off / l.c
    let col := off % l.c
    rows.all fun r =>
      if r.lhs == lhs && (t.lookup (l.suffixNull r.base) != 0) then
        t.lookup (l.cell lhs col) == (r.index + 1).toUInt64
      else true
  else if l.suffixFirst 0 0 ≤ a && a < l.scratch then
    let off := a - l.suffixFirst 0 0
    let pos := off / l.c
    let col := off % l.c
    rows.all fun r =>
      if r.base == pos then
        t.lookup (l.cell r.lhs col) == (r.index + 1).toUInt64
      else true
  else true

private def checkClosedNonCol (start : Nat) (l : Layout) (rows : List Row)
    (t : SparseTree) : Bool :=
  (t.lookup (l.follow start 0) != 0) &&
  rows.all fun r =>
    let stop := r.base + r.rhs.length
    (t.lookup (l.prefixNull r.base) != 0) &&
    (t.lookup (l.suffixNull stop) != 0) &&
    (t.lookup (l.suffixProductive stop) != 0) &&
    (!(t.lookup (l.suffixNull r.base) != 0) || (t.lookup (l.nullNt r.lhs) != 0)) &&
    (!(t.lookup (l.suffixProductive r.base) != 0) || (t.lookup (l.productive r.lhs) != 0)) &&
    r.rhs.zipIdx.all fun (sym, i) =>
      let pos := r.base + i
      match sym with
      | .t symT =>
        (t.lookup (l.suffixFirst pos (symT + 1)) != 0) &&
        (!(t.lookup (l.suffixProductive (pos + 1)) != 0) || (t.lookup (l.suffixProductive pos) != 0))
      | .n child =>
        (!((t.lookup (l.prefixNull pos) != 0) && (t.lookup (l.nullNt child) != 0)) ||
          (t.lookup (l.prefixNull (pos + 1)) != 0)) &&
        (!((t.lookup (l.nullNt child) != 0) && (t.lookup (l.suffixNull (pos + 1)) != 0)) ||
          (t.lookup (l.suffixNull pos) != 0)) &&
        (!((t.lookup (l.productive child) != 0) && (t.lookup (l.suffixProductive (pos + 1)) != 0)) ||
          (t.lookup (l.suffixProductive pos) != 0))

private def checkClosedColNode (l : Layout) (rows : List Row) (t : SparseTree) (a : Nat)
    (_ : UInt64) : Bool :=
  if l.firstNt 0 0 ≤ a && a < l.follow 0 0 then
    let off := a - l.firstNt 0 0
    let child := off / l.c
    let col := off % l.c
    if col == 0 then true
    else rows.all fun r => r.rhs.zipIdx.all fun (sym, i) =>
      match sym with
      | .n n => if n == child then t.lookup (l.suffixFirst (r.base + i) col) != 0 else true
      | .t _ => true
  else if l.follow 0 0 ≤ a && a < l.prefixNull 0 then
    let off := a - l.follow 0 0
    let lhs := off / l.c
    let col := off % l.c
    rows.all fun r =>
      if r.lhs == lhs then
        r.rhs.zipIdx.all fun (sym, i) =>
          match sym with
          | .n n =>
            if t.lookup (l.suffixNull (r.base + i + 1)) != 0 then
              t.lookup (l.follow n col) != 0
            else true
          | .t _ => true
      else true
  else if l.suffixFirst 0 0 ≤ a && a < l.scratch then
    let off := a - l.suffixFirst 0 0
    let pos := off / l.c
    let col := off % l.c
    if col == 0 then true
    else rows.all fun r =>
      (if r.base == pos then t.lookup (l.firstNt r.lhs col) != 0 else true) &&
      (if r.base < pos && pos ≤ r.base + r.rhs.length then
        let i := pos - r.base - 1
        match r.rhs[i]? with
        | some (.n n) =>
          (t.lookup (l.follow n col) != 0) &&
          (!(t.lookup (l.nullNt n) != 0) || (t.lookup (l.suffixFirst (r.base + i) col) != 0))
        | _ => true
       else true)
  else true

private def checkJustifiedNode (start : Nat) (l : Layout) (rows : List Row) (t : SparseTree)
    (a : Nat) (v : UInt64) : Bool :=
  let lt (p : Nat) : Bool :=
    let w := t.lookup p
    w != 0 && decide (w < v)
  let lt2 (p q : Nat) : Bool := lt p && lt q
  match decodeFact l a with
  | none => true
  | some (.nullable n) =>
    rows.any fun r => r.lhs == n && lt (l.suffixNull r.base)
  | some (.productive n) =>
    rows.any fun r => r.lhs == n && lt (l.suffixProductive r.base)
  | some (.first n col) =>
    col != 0 && decide (col < l.c) &&
      rows.any fun r => r.lhs == n && lt (l.suffixFirst r.base col)
  | some (.follow n col) =>
    (n == start && col == 0) ||
      rows.any fun r => r.rhs.zipIdx.any fun (sym, i) =>
        match sym with
        | .n child =>
          child == n &&
            (lt2 (l.suffixNull (r.base + i + 1)) (l.follow r.lhs col) ||
              (col != 0 && lt (l.suffixFirst (r.base + i + 1) col)))
        | .t _ => false
  | some (.prefixNull pos) =>
    rows.any fun r =>
      pos == r.base ||
        (decide (r.base < pos) && decide (pos ≤ r.base + r.rhs.length) &&
          let i := pos - r.base - 1
          match r.rhs[i]? with
          | some (.n n) => lt2 (l.prefixNull (r.base + i)) (l.nullNt n)
          | _ => false)
  | some (.suffixNull pos) =>
    rows.any fun r =>
      pos == r.base + r.rhs.length ||
        (decide (r.base ≤ pos) && decide (pos < r.base + r.rhs.length) &&
          let i := pos - r.base
          match r.rhs[i]? with
          | some (.n n) => lt2 (l.nullNt n) (l.suffixNull (pos + 1))
          | _ => false)
  | some (.suffixProductive pos) =>
    rows.any fun r =>
      pos == r.base + r.rhs.length ||
        (decide (r.base ≤ pos) && decide (pos < r.base + r.rhs.length) &&
          let i := pos - r.base
          match r.rhs[i]? with
          | some (.t _) => lt (l.suffixProductive (pos + 1))
          | some (.n n) => lt2 (l.productive n) (l.suffixProductive (pos + 1))
          | none => false)
  | some (.suffixFirst pos col) =>
    col != 0 && decide (col < l.c) &&
      rows.any fun r =>
        decide (r.base ≤ pos) && decide (pos < r.base + r.rhs.length) &&
        let i := pos - r.base
        match r.rhs[i]? with
        | some (.t symT) => col == symT + 1
        | some (.n n) =>
          lt (l.firstNt n col) || lt2 (l.nullNt n) (l.suffixFirst (pos + 1) col)
        | none => false

private theorem productive_of_check {l : Layout} {t : SparseTree}
    (h : checkProductive l t = true) :
    ∀ n < l.n, 0 < (t.lookup (l.productive n)).toNat := by
  intro n hn
  have hm := List.all_eq_true.mp h n (List.mem_range.mpr hn)
  exact (u64_bne_zero_iff _).1 hm

private theorem descent_of_check {l : Layout} {rows : List Row} {t : SparseTree}
    (h : checkDescent l rows t = true) :
    ∀ r ∈ rows, ∀ child i, (Sym.n child, i) ∈ r.rhs.zipIdx →
      0 < (t.lookup (l.prefixNull (r.base + i))).toNat →
      t.lookup (l.rank child) < t.lookup (l.rank r.lhs) := by
  intro r hr child i hmem hpos
  have hrow := List.all_eq_true.mp h r hr
  have hsym := List.all_eq_true.mp hrow (.n child, i) hmem
  have hnz := (u64_bne_zero_iff _).2 hpos
  simp only [hnz, Bool.not_true, Bool.false_or, decide_eq_true_eq] at hsym
  exact hsym

private theorem cells_of_check {g : RawGrammar} {t : SparseTree}
    (hb : t.bounded 0 (g.layout.scratch + 1) = true)
    (hcell : t.allNodes (checkCellNode g.layout g.rows t) = true)
    (hpred : t.allNodes (checkPredNode g.layout g.rows t) = true) :
    ∀ n < g.layout.n, ∀ col < g.layout.c,
      ((0 < (t.lookup (g.layout.cell n col)).toNat →
          ∃ r ∈ g.rows, r.lhs = n ∧ g.layout.PredictsAt t.lookup r col) ∧
       ∀ r ∈ g.rows, r.lhs = n → g.layout.PredictsAt t.lookup r col →
         t.lookup (g.layout.cell n col) = (r.index + 1).toUInt64) := by
  intro n hn col hc
  constructor
  · intro hpos
    have hnode := SparseTree.lookup_pos_of_allNodes hb hcell hpos
    have hlt : n * g.layout.c + col < g.layout.n * g.layout.c := grid_bound hn hc
    have hdm := grid_div_mod (i := n) (c := g.layout.c) (j := col) hc
    simp only [checkCellNode, Layout.cell, hlt, ↓reduceIte, hdm.1, hdm.2] at hnode
    cases hget : g.rows[(t.lookup (n * g.layout.c + col)).toNat - 1]? with
    | none => simp [hget] at hnode
    | some r =>
      simp only [hget, Bool.and_eq_true, decide_eq_true_eq] at hnode
      refine ⟨r, List.mem_of_getElem? hget, hnode.1, ?_⟩
      simp only [predictsB, Bool.or_eq_true, Bool.and_eq_true, u64_bne_zero_iff] at hnode
      exact hnode.2
  · intro r hr hlhs hpat
    have hposBound : r.base < g.layout.positions := by
      have h := (indexed_row_bounds hr).2
      simp only [RawGrammar.layout, RawGrammar.positions]
      omega
    rcases hpat with hsf | ⟨hsn, hfol⟩
    · have hnode := SparseTree.lookup_pos_of_allNodes hb hpred hsf
      have hlo : g.layout.suffixFirst 0 0 ≤ g.layout.suffixFirst r.base col := by
        simp only [Layout.suffixFirst]; omega
      have hhi : g.layout.suffixFirst r.base col < g.layout.scratch := by
        have gb := grid_bound hposBound hc
        simp only [Layout.scratch, Layout.suffixFirst]; omega
      have hnotfol : ¬ (g.layout.follow 0 0 ≤ g.layout.suffixFirst r.base col ∧
          g.layout.suffixFirst r.base col < g.layout.prefixNull 0) := by
        simp only [Layout.suffixFirst, Layout.suffixProductive, Layout.suffixNull,
          Layout.prefixNull, Layout.follow]
        omega
      have hoff : g.layout.suffixFirst r.base col - g.layout.suffixFirst 0 0 =
          r.base * g.layout.c + col := by
        simp only [Layout.suffixFirst]; omega
      have hdm := grid_div_mod (i := r.base) (c := g.layout.c) (j := col) hc
      simp only [checkPredNode, Bool.and_eq_true, decide_eq_true_eq, hnotfol, ↓reduceIte,
        hlo, hhi, and_self, hoff, hdm.1, hdm.2] at hnode
      have hrow := List.all_eq_true.mp hnode r hr
      simp only [nat_beq_self, ↓reduceIte, beq_iff_eq] at hrow
      exact hlhs ▸ hrow
    · have hnode := SparseTree.lookup_pos_of_allNodes hb hpred hfol
      have hlo : g.layout.follow 0 0 ≤ g.layout.follow r.lhs col := by
        simp only [Layout.follow]; omega
      have hhi : g.layout.follow r.lhs col < g.layout.prefixNull 0 := by
        have hln : r.lhs < g.layout.n := hlhs ▸ hn
        have gb := grid_bound hln hc
        simp only [Layout.prefixNull, Layout.follow, Layout.firstNt]; omega
      have hoff : g.layout.follow r.lhs col - g.layout.follow 0 0 =
          r.lhs * g.layout.c + col := by
        simp only [Layout.follow]; omega
      have hdm := grid_div_mod (i := r.lhs) (c := g.layout.c) (j := col) hc
      simp only [checkPredNode, Bool.and_eq_true, decide_eq_true_eq, hlo, hhi, and_self,
        ↓reduceIte, hoff, hdm.1, hdm.2] at hnode
      have hrow := List.all_eq_true.mp hnode r hr
      have hsnB : (t.lookup (g.layout.suffixNull r.base) != 0) = true := (u64_bne_zero_iff _).2 hsn
      simp only [nat_beq_self, hsnB, and_self, ↓reduceIte, beq_iff_eq] at hrow
      exact hlhs ▸ hrow

private theorem closed_of_check {g : RawGrammar} (hg : g.valid = true) {t : SparseTree}
    (hb : t.bounded 0 (g.layout.scratch + 1) = true)
    (hnoncol : checkClosedNonCol g.cfg.start g.layout g.rows t = true)
    (hcol : t.allNodes (checkClosedColNode g.layout g.rows t) = true) :
    Closed (grammarClauses g).toList (g.layout.height t.lookup) := by
  have hcPos : 0 < g.layout.c := Nat.zero_lt_succ _
  intro clause hcl hprems
  simp only [grammarClauses, List.toList_toArray, List.mem_map] at hcl
  obtain ⟨rule, hrule, rfl⟩ := hcl
  have hvalid := grammarRules_valid hg rule hrule
  have hheadFact : g.layout.isFact (rule.head.address g.layout) = true := valid_fact_address hvalid.1
  have hpremPos : ∀ p ∈ rule.premises, 0 < (t.lookup (p.address g.layout)).toNat := by
    intro p hp
    have hpf : g.layout.isFact (p.address g.layout) = true := valid_fact_address (hvalid.2 p hp)
    have hp' := hprems (p.address g.layout) (List.mem_map_of_mem hp)
    simpa [Layout.height, hpf] using hp'
  suffices hgoal : 0 < (t.lookup (rule.head.address g.layout)).toNat by
    simpa [Rule.map, Layout.height, hheadFact] using hgoal
  simp only [checkClosedNonCol, Bool.and_eq_true] at hnoncol
  obtain ⟨hstart, hallRows⟩ := hnoncol
  rcases List.mem_cons.mp hrule with rfl | hrowsMem
  · exact (u64_bne_zero_iff _).1 hstart
  · obtain ⟨r, hr, hrowRule⟩ := List.mem_flatMap.mp hrowsMem
    have hrow := List.all_eq_true.mp hallRows r hr
    simp only [Bool.and_eq_true] at hrow
    obtain ⟨⟨⟨⟨⟨hpn, hsn⟩, hsp⟩, hnull⟩, hprod⟩, hsyms⟩ := hrow
    have hrowValid := indexed_row_bounds hr
    have hposBound : r.base + r.rhs.length < g.layout.positions := by
      simpa [RawGrammar.layout, RawGrammar.positions] using hrowValid.2
    have hvalidRow : r.lhs < g.layout.n ∧ ∀ sym ∈ r.rhs,
        match sym with | .t symT => symT < g.terminals | .n child => child < g.layout.n := by
      simp only [RawGrammar.valid, Bool.and_eq_true, List.all_eq_true, decide_eq_true_eq] at hg
      have hm := hg.2 _ hrowValid.1
      refine ⟨hm.1, fun sym hs => ?_⟩
      cases sym <;> simpa [RawGrammar.layout] using hm.2 _ hs
    simp only [Layout.rowRules, List.mem_append, List.mem_cons, List.not_mem_nil, or_false,
      List.mem_map, List.mem_range, List.mem_flatMap] at hrowRule
    rcases hrowRule with ((rfl | rfl | rfl | rfl | rfl) | ⟨symT, hsymT, rfl⟩) | ⟨⟨sym, i⟩, hzi, hsymRule⟩
    · exact (u64_bne_zero_iff _).1 hpn
    · exact (u64_bne_zero_iff _).1 hsn
    · exact (u64_bne_zero_iff _).1 hsp
    · have hp := (u64_bne_zero_iff _).2 (hpremPos _ List.mem_cons_self)
      simp only [Fact.address] at hp ⊢
      simp only [hp, Bool.not_true, Bool.false_or] at hnull
      exact (u64_bne_zero_iff _).1 hnull
    · have hp := (u64_bne_zero_iff _).2 (hpremPos _ List.mem_cons_self)
      simp only [Fact.address] at hp ⊢
      simp only [hp, Bool.not_true, Bool.false_or] at hprod
      exact (u64_bne_zero_iff _).1 hprod
    · -- Schema 7: first r.lhs (symT + 1) ← suffixFirst r.base (symT + 1)
      have hsfPos : 0 < (t.lookup (g.layout.suffixFirst r.base (symT + 1))).toNat :=
        hpremPos _ List.mem_cons_self
      have hnode := SparseTree.lookup_pos_of_allNodes hb hcol hsfPos
      have hcolLt : symT + 1 < g.layout.c := by omega
      have hbaseLt : r.base < g.layout.positions := by omega
      have hsfLo : g.layout.suffixFirst 0 0 ≤ g.layout.suffixFirst r.base (symT + 1) := by
        simp only [Layout.suffixFirst]; omega
      have hsfHi : g.layout.suffixFirst r.base (symT + 1) < g.layout.scratch := by
        have gb := grid_bound hbaseLt hcolLt
        simp only [Layout.scratch, Layout.suffixFirst]; omega
      have hnotFi : ¬ (g.layout.firstNt 0 0 ≤ g.layout.suffixFirst r.base (symT + 1) ∧
          g.layout.suffixFirst r.base (symT + 1) < g.layout.follow 0 0) := by
        simp only [Layout.suffixFirst, Layout.suffixProductive, Layout.suffixNull,
          Layout.prefixNull, Layout.follow, Layout.firstNt]; omega
      have hnotFo : ¬ (g.layout.follow 0 0 ≤ g.layout.suffixFirst r.base (symT + 1) ∧
          g.layout.suffixFirst r.base (symT + 1) < g.layout.prefixNull 0) := by
        simp only [Layout.suffixFirst, Layout.suffixProductive, Layout.suffixNull,
          Layout.prefixNull, Layout.follow]; omega
      have hoff : g.layout.suffixFirst r.base (symT + 1) - g.layout.suffixFirst 0 0 =
          r.base * g.layout.c + (symT + 1) := by
        simp only [Layout.suffixFirst]; omega
      have hdm := grid_div_mod (i := r.base) (c := g.layout.c) (j := symT + 1) hcolLt
      have hcolNe : (symT + 1 == 0) = false := by simp
      simp only [checkClosedColNode, Bool.and_eq_true, decide_eq_true_eq, hnotFi, hnotFo,
        hsfLo, hsfHi, and_self, ↓reduceIte, hoff, hdm.1, hdm.2, hcolNe, Bool.false_eq_true] at hnode
      have hrowC := List.all_eq_true.mp hnode r hr
      simp only [Bool.and_eq_true, nat_beq_self, ↓reduceIte] at hrowC
      exact (u64_bne_zero_iff _).1 hrowC.1
    · have hsymB := List.all_eq_true.mp hsyms (sym, i) hzi
      have hget : r.rhs[i]? = some sym := List.mem_zipIdx_iff_getElem?.mp hzi
      have hilt : i < r.rhs.length := (List.getElem?_eq_some_iff.mp hget).1
      cases sym with
      | t symT =>
        simp only [Bool.and_eq_true] at hsymB
        simp only [Layout.symbolRules, List.mem_cons, List.not_mem_nil, or_false] at hsymRule
        rcases hsymRule with rfl | rfl
        · exact (u64_bne_zero_iff _).1 hsymB.1
        · have hp := (u64_bne_zero_iff _).2 (hpremPos _ List.mem_cons_self)
          simp only [Fact.address] at hp ⊢
          have h2 := hsymB.2
          simp only [hp, Bool.not_true, Bool.false_or] at h2
          exact (u64_bne_zero_iff _).1 h2
      | n child =>
        simp only [Bool.and_eq_true] at hsymB
        have hchildLt : child < g.layout.n := hvalidRow.2 (.n child) (List.mem_of_getElem? hget)
        simp only [Layout.symbolRules, List.mem_append, List.mem_cons, List.not_mem_nil,
          or_false, List.mem_flatMap, List.mem_range] at hsymRule
        rcases hsymRule with (rfl | rfl | rfl) | ⟨col, hcolLt, rfl | hrest⟩
        · have hp1 := (u64_bne_zero_iff _).2 (hpremPos _ List.mem_cons_self)
          have hp2 := (u64_bne_zero_iff _).2 (hpremPos _ (List.mem_cons_of_mem _ List.mem_cons_self))
          simp only [Fact.address] at hp1 hp2 ⊢
          have h1 := hsymB.1.1
          simp only [hp1, hp2, Bool.and_self, Bool.not_true, Bool.false_or] at h1
          exact (u64_bne_zero_iff _).1 h1
        · have hp1 := (u64_bne_zero_iff _).2 (hpremPos _ List.mem_cons_self)
          have hp2 := (u64_bne_zero_iff _).2 (hpremPos _ (List.mem_cons_of_mem _ List.mem_cons_self))
          simp only [Fact.address] at hp1 hp2 ⊢
          have h1 := hsymB.1.2
          simp only [hp1, hp2, Bool.and_self, Bool.not_true, Bool.false_or] at h1
          exact (u64_bne_zero_iff _).1 h1
        · have hp1 := (u64_bne_zero_iff _).2 (hpremPos _ List.mem_cons_self)
          have hp2 := (u64_bne_zero_iff _).2 (hpremPos _ (List.mem_cons_of_mem _ List.mem_cons_self))
          simp only [Fact.address] at hp1 hp2 ⊢
          have h2 := hsymB.2
          simp only [hp1, hp2, Bool.and_self, Bool.not_true, Bool.false_or] at h2
          exact (u64_bne_zero_iff _).1 h2
        · -- Schema 9d: follow child col ← [suffixNull (r.base + i + 1), follow r.lhs col]
          have hsnPos : 0 < (t.lookup (g.layout.suffixNull (r.base + i + 1))).toNat :=
            hpremPos _ List.mem_cons_self
          have hfoPos : 0 < (t.lookup (g.layout.follow r.lhs col)).toNat :=
            hpremPos _ (List.mem_cons_of_mem _ List.mem_cons_self)
          have hnode := SparseTree.lookup_pos_of_allNodes hb hcol hfoPos
          have hfoLo : g.layout.follow 0 0 ≤ g.layout.follow r.lhs col := by
            simp only [Layout.follow]; omega
          have hfoHi : g.layout.follow r.lhs col < g.layout.prefixNull 0 := by
            have gb := grid_bound hvalidRow.1 hcolLt
            simp only [Layout.prefixNull, Layout.follow, Layout.firstNt]; omega
          have hnotFi : ¬ (g.layout.firstNt 0 0 ≤ g.layout.follow r.lhs col ∧
              g.layout.follow r.lhs col < g.layout.follow 0 0) := by
            simp only [Layout.follow, Layout.firstNt]; omega
          have hoff : g.layout.follow r.lhs col - g.layout.follow 0 0 =
              r.lhs * g.layout.c + col := by
            simp only [Layout.follow]; omega
          have hdm := grid_div_mod (i := r.lhs) (c := g.layout.c) (j := col) hcolLt
          simp only [checkClosedColNode, Bool.and_eq_true, decide_eq_true_eq, hnotFi,
            hfoLo, hfoHi, and_self, ↓reduceIte, hoff, hdm.1, hdm.2] at hnode
          have hrowC := List.all_eq_true.mp hnode r hr
          simp only [nat_beq_self, ↓reduceIte] at hrowC
          have hsymC := List.all_eq_true.mp hrowC (.n child, i) hzi
          have hsnB := (u64_bne_zero_iff _).2 hsnPos
          simp only [hsnB, ↓reduceIte] at hsymC
          exact (u64_bne_zero_iff _).1 hsymC
        · split at hrest
          · cases hrest
          · rename_i hcolNe0
            have hcolEqFalse : (col == 0) = false := beq_false_of_ne hcolNe0
            simp only [List.mem_cons, List.not_mem_nil, or_false] at hrest
            rcases hrest with rfl | rfl | rfl
            · -- Schema 9e: suffixFirst (r.base + i) col ← [first child col]
              have hfiPos : 0 < (t.lookup (g.layout.firstNt child col)).toNat :=
                hpremPos _ List.mem_cons_self
              have hnode := SparseTree.lookup_pos_of_allNodes hb hcol hfiPos
              have hfiLo : g.layout.firstNt 0 0 ≤ g.layout.firstNt child col := by
                simp only [Layout.firstNt]; omega
              have hfiHi : g.layout.firstNt child col < g.layout.follow 0 0 := by
                have gb := grid_bound hchildLt hcolLt
                simp only [Layout.follow, Layout.firstNt, Layout.productive]; omega
              have hoff : g.layout.firstNt child col - g.layout.firstNt 0 0 =
                  child * g.layout.c + col := by
                simp only [Layout.firstNt]; omega
              have hdm := grid_div_mod (i := child) (c := g.layout.c) (j := col) hcolLt
              simp only [checkClosedColNode, Bool.and_eq_true, decide_eq_true_eq,
                hfiLo, hfiHi, and_self, ↓reduceIte, hoff, hdm.1, hdm.2, hcolEqFalse,
                Bool.false_eq_true] at hnode
              have hrowC := List.all_eq_true.mp hnode r hr
              have hsymC := List.all_eq_true.mp hrowC (.n child, i) hzi
              simp only [nat_beq_self, ↓reduceIte] at hsymC
              exact (u64_bne_zero_iff _).1 hsymC
            · -- Schema 9f: suffixFirst (r.base + i) col ← [nullable child, suffixFirst (r.base + i + 1) col]
              have hnuPos : 0 < (t.lookup (g.layout.nullNt child)).toNat :=
                hpremPos _ List.mem_cons_self
              have hsfPos : 0 < (t.lookup (g.layout.suffixFirst (r.base + i + 1) col)).toNat :=
                hpremPos _ (List.mem_cons_of_mem _ List.mem_cons_self)
              have hnode := SparseTree.lookup_pos_of_allNodes hb hcol hsfPos
              have hposLt : r.base + i + 1 < g.layout.positions := by omega
              have hsfLo : g.layout.suffixFirst 0 0 ≤ g.layout.suffixFirst (r.base + i + 1) col := by
                simp only [Layout.suffixFirst]; omega
              have hsfHi : g.layout.suffixFirst (r.base + i + 1) col < g.layout.scratch := by
                have gb := grid_bound hposLt hcolLt
                simp only [Layout.scratch, Layout.suffixFirst]; omega
              have hnotFi : ¬ (g.layout.firstNt 0 0 ≤ g.layout.suffixFirst (r.base + i + 1) col ∧
                  g.layout.suffixFirst (r.base + i + 1) col < g.layout.follow 0 0) := by
                simp only [Layout.suffixFirst, Layout.suffixProductive, Layout.suffixNull,
                  Layout.prefixNull, Layout.follow, Layout.firstNt]; omega
              have hnotFo : ¬ (g.layout.follow 0 0 ≤ g.layout.suffixFirst (r.base + i + 1) col ∧
                  g.layout.suffixFirst (r.base + i + 1) col < g.layout.prefixNull 0) := by
                simp only [Layout.suffixFirst, Layout.suffixProductive, Layout.suffixNull,
                  Layout.prefixNull, Layout.follow]; omega
              have hoff : g.layout.suffixFirst (r.base + i + 1) col - g.layout.suffixFirst 0 0 =
                  (r.base + i + 1) * g.layout.c + col := by
                simp only [Layout.suffixFirst]; omega
              have hdm := grid_div_mod (i := r.base + i + 1) (c := g.layout.c) (j := col) hcolLt
              simp only [checkClosedColNode, Bool.and_eq_true, decide_eq_true_eq,
                hnotFi, hnotFo, hsfLo, hsfHi, and_self, ↓reduceIte, hoff, hdm.1, hdm.2,
                hcolEqFalse, Bool.false_eq_true] at hnode
              have hrowC := List.all_eq_true.mp hnode r hr
              have hrange : r.base < r.base + i + 1 ∧ r.base + i + 1 ≤ r.base + r.rhs.length :=
                ⟨by omega, by omega⟩
              have hidx : r.base + i + 1 - r.base - 1 = i := by omega
              simp only [Bool.and_eq_true] at hrowC
              have h2 := hrowC.2
              simp only [hrange.1, hrange.2, and_self, ↓reduceIte, hidx, hget, Bool.and_eq_true] at h2
              have hnuB := (u64_bne_zero_iff _).2 hnuPos
              have h2r := h2.2
              simp only [hnuB, Bool.not_true, Bool.false_or] at h2r
              exact (u64_bne_zero_iff _).1 h2r
            · -- Schema 9g: follow child col ← [suffixFirst (r.base + i + 1) col]
              have hsfPos : 0 < (t.lookup (g.layout.suffixFirst (r.base + i + 1) col)).toNat :=
                hpremPos _ List.mem_cons_self
              have hnode := SparseTree.lookup_pos_of_allNodes hb hcol hsfPos
              have hposLt : r.base + i + 1 < g.layout.positions := by omega
              have hsfLo : g.layout.suffixFirst 0 0 ≤ g.layout.suffixFirst (r.base + i + 1) col := by
                simp only [Layout.suffixFirst]; omega
              have hsfHi : g.layout.suffixFirst (r.base + i + 1) col < g.layout.scratch := by
                have gb := grid_bound hposLt hcolLt
                simp only [Layout.scratch, Layout.suffixFirst]; omega
              have hnotFi : ¬ (g.layout.firstNt 0 0 ≤ g.layout.suffixFirst (r.base + i + 1) col ∧
                  g.layout.suffixFirst (r.base + i + 1) col < g.layout.follow 0 0) := by
                simp only [Layout.suffixFirst, Layout.suffixProductive, Layout.suffixNull,
                  Layout.prefixNull, Layout.follow, Layout.firstNt]; omega
              have hnotFo : ¬ (g.layout.follow 0 0 ≤ g.layout.suffixFirst (r.base + i + 1) col ∧
                  g.layout.suffixFirst (r.base + i + 1) col < g.layout.prefixNull 0) := by
                simp only [Layout.suffixFirst, Layout.suffixProductive, Layout.suffixNull,
                  Layout.prefixNull, Layout.follow]; omega
              have hoff : g.layout.suffixFirst (r.base + i + 1) col - g.layout.suffixFirst 0 0 =
                  (r.base + i + 1) * g.layout.c + col := by
                simp only [Layout.suffixFirst]; omega
              have hdm := grid_div_mod (i := r.base + i + 1) (c := g.layout.c) (j := col) hcolLt
              simp only [checkClosedColNode, Bool.and_eq_true, decide_eq_true_eq,
                hnotFi, hnotFo, hsfLo, hsfHi, and_self, ↓reduceIte, hoff, hdm.1, hdm.2,
                hcolEqFalse, Bool.false_eq_true] at hnode
              have hrowC := List.all_eq_true.mp hnode r hr
              have hrange : r.base < r.base + i + 1 ∧ r.base + i + 1 ≤ r.base + r.rhs.length :=
                ⟨by omega, by omega⟩
              have hidx : r.base + i + 1 - r.base - 1 = i := by omega
              simp only [Bool.and_eq_true] at hrowC
              have h2 := hrowC.2
              simp only [hrange.1, hrange.2, and_self, ↓reduceIte, hidx, hget, Bool.and_eq_true] at h2
              exact (u64_bne_zero_iff _).1 h2.1

private theorem clause_justified_of_rule {g : RawGrammar} (hg : g.valid = true)
    {t : SparseTree} {a : Nat} {rule : Rule Fact}
    (hmem : rule ∈ grammarRules g) (hhead : rule.head.address g.layout = a)
    (hprems : ∀ p ∈ rule.premises,
      0 < (t.lookup (p.address g.layout)).toNat ∧
        (t.lookup (p.address g.layout)).toNat < (t.lookup a).toNat) :
    ∃ c ∈ (grammarClauses g).toList, c.head = a ∧
      ∀ p ∈ c.premises,
        0 < g.layout.height t.lookup p ∧
          g.layout.height t.lookup p < g.layout.height t.lookup a := by
  have hv := grammarRules_valid hg rule hmem
  have hheadFact : g.layout.isFact a = true := hhead ▸ valid_fact_address hv.1
  refine ⟨rule.map (Fact.address g.layout), ?_, hhead, ?_⟩
  · simp only [grammarClauses, List.toList_toArray]
    exact List.mem_map_of_mem hmem
  · intro pa hpa
    obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hpa
    have hpf : g.layout.isFact (p.address g.layout) = true := valid_fact_address (hv.2 p hp)
    simpa [Layout.height, hpf, hheadFact] using hprems p hp

private theorem rowRule_mem_grammar {g : RawGrammar} {r : Row} (hr : r ∈ g.rows)
    {rule : Rule Fact} (hm : rule ∈ g.layout.rowRules r) :
    rule ∈ grammarRules g :=
  List.mem_cons_of_mem _ (List.mem_flatMap.mpr ⟨r, hr, hm⟩)

private theorem symRule_mem_grammar {g : RawGrammar} {r : Row} (hr : r ∈ g.rows)
    {sym : Sym Nat Nat} {i : Nat} (hzi : (sym, i) ∈ r.rhs.zipIdx)
    {rule : Rule Fact} (hm : rule ∈ g.layout.symbolRules r.lhs (r.base + i) sym) :
    rule ∈ grammarRules g :=
  rowRule_mem_grammar hr (List.mem_append_right _ (List.mem_flatMap.mpr ⟨(sym, i), hzi, hm⟩))

private theorem u64_lt_bounds {w v : UInt64}
    (h : (w != 0 && decide (w < v)) = true) :
    0 < w.toNat ∧ w.toNat < v.toNat := by
  simp only [Bool.and_eq_true, decide_eq_true_eq] at h
  exact ⟨(u64_bne_zero_iff w).1 h.1, UInt64.lt_iff_toNat_lt.1 h.2⟩

private theorem justified_of_check {g : RawGrammar} (hg : g.valid = true) {t : SparseTree}
    (hb : t.bounded 0 (g.layout.scratch + 1) = true)
    (hjust : t.allNodes (checkJustifiedNode g.cfg.start g.layout g.rows t) = true) :
    Justified (grammarClauses g).toList (g.layout.height t.lookup) := by
  have hc : 0 < g.layout.c := Nat.zero_lt_succ _
  intro a ha
  have hfact : g.layout.isFact a = true := by
    by_cases hf : g.layout.isFact a = true
    · exact hf
    · simp [Layout.height, hf] at ha
  have hpos : 0 < (t.lookup a).toNat := by simpa [Layout.height, hfact] using ha
  have hnode := SparseTree.lookup_pos_of_allNodes hb hjust hpos
  obtain ⟨f, hdec, hfval, haddr⟩ := decodeFact_sound hc hfact
  simp only [checkJustifiedNode, hdec] at hnode
  cases f with
  | nullable n =>
    obtain ⟨r, hr, hok⟩ := List.any_eq_true.mp hnode
    simp only [Bool.and_eq_true, beq_iff_eq] at hok
    obtain ⟨rfl, hlt⟩ := hok
    have hbnd := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hlt)
    exact clause_justified_of_rule hg (rule := ⟨Fact.nullable r.lhs, [Fact.suffixNull r.base]⟩)
      (rowRule_mem_grammar hr (by simp [Layout.rowRules])) haddr
      (by intro p hp; rcases List.mem_singleton.mp hp with rfl; exact hbnd)
  | productive n =>
    obtain ⟨r, hr, hok⟩ := List.any_eq_true.mp hnode
    simp only [Bool.and_eq_true, beq_iff_eq] at hok
    obtain ⟨rfl, hlt⟩ := hok
    have hbnd := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hlt)
    exact clause_justified_of_rule hg (rule := ⟨Fact.productive r.lhs, [Fact.suffixProductive r.base]⟩)
      (rowRule_mem_grammar hr (by simp [Layout.rowRules])) haddr
      (by intro p hp; rcases List.mem_singleton.mp hp with rfl; exact hbnd)
  | first n col =>
    simp only [Bool.and_eq_true, bne_iff_ne, ne_eq, decide_eq_true_eq] at hnode
    obtain ⟨⟨hcolNe, hcolLt⟩, hany⟩ := hnode
    obtain ⟨r, hr, hok⟩ := List.any_eq_true.mp hany
    simp only [Bool.and_eq_true, beq_iff_eq] at hok
    obtain ⟨rfl, hlt⟩ := hok
    have hbnd := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hlt)
    have hcolEq : col - 1 + 1 = col := by omega
    have hrmem : ⟨Fact.first r.lhs col, [Fact.suffixFirst r.base col]⟩ ∈ g.layout.rowRules r := by
      simp only [Layout.rowRules, List.mem_append, List.mem_map, List.mem_range]
      exact Or.inl (Or.inr ⟨col - 1, by omega, by rw [hcolEq]⟩)
    exact clause_justified_of_rule hg (rowRule_mem_grammar hr hrmem) haddr
      (by intro p hp; rcases List.mem_singleton.mp hp with rfl; exact hbnd)
  | follow n col =>
    have hcolLt : col < g.layout.c := by
      simp only [Fact.valid, Bool.and_eq_true, decide_eq_true_eq] at hfval
      exact hfval.2
    simp only [Bool.or_eq_true] at hnode
    rcases hnode with hstart | hany
    · simp only [Bool.and_eq_true, beq_iff_eq] at hstart
      obtain ⟨rfl, rfl⟩ := hstart
      exact clause_justified_of_rule hg List.mem_cons_self haddr (by simp)
    · obtain ⟨r, hr, hsymAny⟩ := List.any_eq_true.mp hany
      obtain ⟨⟨sym, i⟩, hzi, hok⟩ := List.any_eq_true.mp hsymAny
      cases sym with
      | t _ => cases hok
      | n child =>
        simp only [Bool.and_eq_true, beq_iff_eq, Bool.or_eq_true] at hok
        obtain ⟨rfl, hbranch⟩ := hok
        rcases hbranch with htail | hfirst
        · have hb1 := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact htail.1)
          have hb2 := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact htail.2)
          have hsmem : ⟨Fact.follow child col,
              [Fact.suffixNull (r.base + i + 1), Fact.follow r.lhs col]⟩ ∈
              g.layout.symbolRules r.lhs (r.base + i) (.n child) := by
            simp only [Layout.symbolRules, List.mem_append, List.mem_flatMap, List.mem_range]
            exact Or.inr ⟨col, hcolLt, by simp⟩
          exact clause_justified_of_rule hg (symRule_mem_grammar hr hzi hsmem) haddr (by
            intro p hp
            rcases List.mem_cons.mp hp with rfl | hp
            · exact hb1
            · rcases List.mem_singleton.mp hp with rfl; exact hb2)
        · have hcolNe : col ≠ 0 := bne_iff_ne.mp hfirst.1
          have hb1 := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hfirst.2)
          have hsmem : ⟨Fact.follow child col,
              [Fact.suffixFirst (r.base + i + 1) col]⟩ ∈
              g.layout.symbolRules r.lhs (r.base + i) (.n child) := by
            simp only [Layout.symbolRules, List.mem_append, List.mem_flatMap, List.mem_range]
            exact Or.inr ⟨col, hcolLt, by simp [hcolNe]⟩
          exact clause_justified_of_rule hg (symRule_mem_grammar hr hzi hsmem) haddr
            (by intro p hp; rcases List.mem_singleton.mp hp with rfl; exact hb1)
  | prefixNull pos =>
    obtain ⟨r, hr, hok⟩ := List.any_eq_true.mp hnode
    simp only [Bool.or_eq_true, beq_iff_eq] at hok
    rcases hok with rfl | hstep
    · exact clause_justified_of_rule hg (rule := ⟨Fact.prefixNull r.base, []⟩)
        (rowRule_mem_grammar hr (by simp [Layout.rowRules])) haddr (by simp)
    · simp only [Bool.and_eq_true, decide_eq_true_eq] at hstep
      obtain ⟨⟨hgt, hle⟩, hmatch⟩ := hstep
      let i := pos - r.base - 1
      have hposEq : r.base + i + 1 = pos := by omega
      cases hget : r.rhs[i]? with
      | none => simp [i, hget] at hmatch
      | some sym =>
        cases sym with
        | t _ => simp [i, hget] at hmatch
        | n n =>
          simp only [i, hget, Bool.and_eq_true] at hmatch
          have hb1 := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hmatch.1)
          have hb2 := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hmatch.2)
          have hzi : (Sym.n n, i) ∈ r.rhs.zipIdx := List.mem_zipIdx_iff_getElem?.mpr hget
          have hsmem : ⟨Fact.prefixNull pos, [Fact.prefixNull (r.base + i), Fact.nullable n]⟩ ∈
              g.layout.symbolRules r.lhs (r.base + i) (.n n) := by
            rw [← hposEq]
            simp [Layout.symbolRules]
          exact clause_justified_of_rule hg (symRule_mem_grammar hr hzi hsmem) haddr (by
            intro p hp
            rcases List.mem_cons.mp hp with rfl | hp
            · exact hb1
            · rcases List.mem_singleton.mp hp with rfl; exact hb2)
  | suffixNull pos =>
    obtain ⟨r, hr, hok⟩ := List.any_eq_true.mp hnode
    simp only [Bool.or_eq_true, beq_iff_eq] at hok
    rcases hok with rfl | hstep
    · exact clause_justified_of_rule hg (rule := ⟨Fact.suffixNull (r.base + r.rhs.length), []⟩)
        (rowRule_mem_grammar hr (by simp [Layout.rowRules])) haddr (by simp)
    · simp only [Bool.and_eq_true, decide_eq_true_eq] at hstep
      obtain ⟨⟨hle, hlt⟩, hmatch⟩ := hstep
      let i := pos - r.base
      have hposEq : r.base + i = pos := by omega
      cases hget : r.rhs[i]? with
      | none => simp [i, hget] at hmatch
      | some sym =>
        cases sym with
        | t _ => simp [i, hget] at hmatch
        | n n =>
          simp only [i, hget, Bool.and_eq_true] at hmatch
          have hb1 := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hmatch.1)
          have hb2 := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hmatch.2)
          have hzi : (Sym.n n, i) ∈ r.rhs.zipIdx := List.mem_zipIdx_iff_getElem?.mpr hget
          have hsmem : ⟨Fact.suffixNull pos, [Fact.nullable n, Fact.suffixNull (pos + 1)]⟩ ∈
              g.layout.symbolRules r.lhs (r.base + i) (.n n) := by
            rw [hposEq]
            simp [Layout.symbolRules]
          exact clause_justified_of_rule hg (symRule_mem_grammar hr hzi hsmem) haddr (by
            intro p hp
            rcases List.mem_cons.mp hp with rfl | hp
            · exact hb1
            · rcases List.mem_singleton.mp hp with rfl; exact hb2)
  | suffixProductive pos =>
    obtain ⟨r, hr, hok⟩ := List.any_eq_true.mp hnode
    simp only [Bool.or_eq_true, beq_iff_eq] at hok
    rcases hok with rfl | hstep
    · exact clause_justified_of_rule hg
        (rule := ⟨Fact.suffixProductive (r.base + r.rhs.length), []⟩)
        (rowRule_mem_grammar hr (by simp [Layout.rowRules])) haddr (by simp)
    · simp only [Bool.and_eq_true, decide_eq_true_eq] at hstep
      obtain ⟨⟨hle, hlt⟩, hmatch⟩ := hstep
      let i := pos - r.base
      have hposEq : r.base + i = pos := by omega
      cases hget : r.rhs[i]? with
      | none => simp [i, hget] at hmatch
      | some sym =>
        have hzi : (sym, i) ∈ r.rhs.zipIdx := List.mem_zipIdx_iff_getElem?.mpr hget
        cases sym with
        | t symT =>
          simp only [i, hget] at hmatch
          have hb1 := u64_lt_bounds hmatch
          have hsmem : ⟨Fact.suffixProductive pos, [Fact.suffixProductive (pos + 1)]⟩ ∈
              g.layout.symbolRules r.lhs (r.base + i) (.t symT) := by
            rw [hposEq]
            simp [Layout.symbolRules]
          exact clause_justified_of_rule hg (symRule_mem_grammar hr hzi hsmem) haddr
            (by intro p hp; rcases List.mem_singleton.mp hp with rfl; exact hb1)
        | n n =>
          simp only [i, hget, Bool.and_eq_true] at hmatch
          have hb1 := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hmatch.1)
          have hb2 := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hmatch.2)
          have hsmem : ⟨Fact.suffixProductive pos,
              [Fact.productive n, Fact.suffixProductive (pos + 1)]⟩ ∈
              g.layout.symbolRules r.lhs (r.base + i) (.n n) := by
            rw [hposEq]
            simp [Layout.symbolRules]
          exact clause_justified_of_rule hg (symRule_mem_grammar hr hzi hsmem) haddr (by
            intro p hp
            rcases List.mem_cons.mp hp with rfl | hp
            · exact hb1
            · rcases List.mem_singleton.mp hp with rfl; exact hb2)
  | suffixFirst pos col =>
    simp only [Bool.and_eq_true, bne_iff_ne, ne_eq, decide_eq_true_eq] at hnode
    obtain ⟨⟨hcolNe, hcolLt⟩, hany⟩ := hnode
    obtain ⟨r, hr, hok⟩ := List.any_eq_true.mp hany
    simp only [Bool.and_eq_true, decide_eq_true_eq] at hok
    obtain ⟨⟨hle, hlt⟩, hmatch⟩ := hok
    let i := pos - r.base
    have hposEq : r.base + i = pos := by omega
    cases hget : r.rhs[i]? with
    | none => simp [i, hget] at hmatch
    | some sym =>
      have hzi : (sym, i) ∈ r.rhs.zipIdx := List.mem_zipIdx_iff_getElem?.mpr hget
      cases sym with
      | t symT =>
        simp only [i, hget, beq_iff_eq] at hmatch
        subst hmatch
        have hsmem : ⟨Fact.suffixFirst pos (symT + 1), []⟩ ∈
            g.layout.symbolRules r.lhs (r.base + i) (.t symT) := by
          rw [hposEq]
          simp [Layout.symbolRules]
        exact clause_justified_of_rule hg (symRule_mem_grammar hr hzi hsmem) haddr (by simp)
      | n n =>
        simp only [i, hget, Bool.or_eq_true] at hmatch
        rcases hmatch with hfi | hnu
        · have hb1 := u64_lt_bounds hfi
          have hsmem : ⟨Fact.suffixFirst pos col, [Fact.first n col]⟩ ∈
              g.layout.symbolRules r.lhs (r.base + i) (.n n) := by
            rw [hposEq]
            simp only [Layout.symbolRules, List.mem_append, List.mem_flatMap, List.mem_range]
            exact Or.inr ⟨col, hcolLt, by simp [hcolNe]⟩
          exact clause_justified_of_rule hg (symRule_mem_grammar hr hzi hsmem) haddr
            (by intro p hp; rcases List.mem_singleton.mp hp with rfl; exact hb1)
        · simp only [Bool.and_eq_true] at hnu
          have hb1 := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hnu.1)
          have hb2 := u64_lt_bounds (by simp only [Bool.and_eq_true]; exact hnu.2)
          have hsmem : ⟨Fact.suffixFirst pos col,
              [Fact.nullable n, Fact.suffixFirst (pos + 1) col]⟩ ∈
              g.layout.symbolRules r.lhs (r.base + i) (.n n) := by
            rw [hposEq]
            simp only [Layout.symbolRules, List.mem_append, List.mem_flatMap, List.mem_range]
            exact Or.inr ⟨col, hcolLt, by simp [hcolNe]⟩
          exact clause_justified_of_rule hg (symRule_mem_grammar hr hzi hsmem) haddr (by
            intro p hp
            rcases List.mem_cons.mp hp with rfl | hp
            · exact hb1
            · rcases List.mem_singleton.mp hp with rfl; exact hb2)

/-! ## Instantiation to `earlyRawGrammar` and `earlyProposal` -/

private def earlyLayout : Layout := ⟨21, 81, 222⟩

private def earlyRows : List Row := [
  ⟨0, 0, 0, [.n 1, .t 0, .t 3, .t 7, .t 8, .t 11, .n 2, .t 9, .n 4, .t 10]⟩,
  ⟨1, 11, 1, [.t 26, .t 3, .t 9, .t 10, .n 1]⟩,
  ⟨2, 17, 1, []⟩,
  ⟨3, 18, 2, [.t 6]⟩,
  ⟨4, 20, 2, [.t 28]⟩,
  ⟨5, 22, 2, [.t 29]⟩,
  ⟨6, 24, 2, [.t 30]⟩,
  ⟨7, 26, 2, [.t 31]⟩,
  ⟨8, 28, 2, [.t 32]⟩,
  ⟨9, 30, 2, [.t 33]⟩,
  ⟨10, 32, 2, [.t 34]⟩,
  ⟨11, 34, 2, [.t 35]⟩,
  ⟨12, 36, 2, [.t 36]⟩,
  ⟨13, 38, 2, [.t 37]⟩,
  ⟨14, 40, 2, [.t 3]⟩,
  ⟨15, 42, 3, [.t 6]⟩,
  ⟨16, 44, 3, [.t 28]⟩,
  ⟨17, 46, 3, [.t 29]⟩,
  ⟨18, 48, 3, [.t 30]⟩,
  ⟨19, 50, 3, [.t 31]⟩,
  ⟨20, 52, 3, [.t 32]⟩,
  ⟨21, 54, 3, [.t 33]⟩,
  ⟨22, 56, 3, [.t 34]⟩,
  ⟨23, 58, 3, [.t 35]⟩,
  ⟨24, 60, 3, [.t 36]⟩,
  ⟨25, 62, 3, [.t 37]⟩,
  ⟨26, 64, 3, [.t 3]⟩,
  ⟨27, 66, 4, [.t 1, .n 5, .t 13, .n 10, .t 14, .n 4]⟩,
  ⟨28, 73, 4, [.t 2, .n 10, .t 14, .n 4]⟩,
  ⟨29, 78, 4, [.t 3, .t 7, .t 8, .t 14, .n 4]⟩,
  ⟨30, 84, 4, [.n 7, .n 4]⟩,
  ⟨31, 87, 4, []⟩,
  ⟨32, 88, 5, [.t 3, .n 6]⟩,
  ⟨33, 91, 5, [.t 79, .t 12, .n 3]⟩,
  ⟨34, 95, 5, [.t 23, .t 3, .t 12, .n 3]⟩,
  ⟨35, 100, 6, [.t 12, .n 3]⟩,
  ⟨36, 103, 6, [.t 9, .t 10]⟩,
  ⟨37, 106, 7, [.t 24, .n 15, .t 9, .n 4, .t 10, .n 8]⟩,
  ⟨38, 113, 8, [.t 25, .n 9]⟩,
  ⟨39, 116, 8, []⟩,
  ⟨40, 117, 9, [.t 9, .n 4, .t 10]⟩,
  ⟨41, 121, 9, [.n 7]⟩,
  ⟨42, 123, 10, [.n 12, .n 11]⟩,
  ⟨43, 126, 11, [.t 15, .n 12, .n 11]⟩,
  ⟨44, 130, 11, [.t 16, .n 12, .n 11]⟩,
  ⟨45, 134, 11, []⟩,
  ⟨46, 135, 12, [.n 14, .n 13]⟩,
  ⟨47, 138, 13, [.t 17, .n 14, .n 13]⟩,
  ⟨48, 142, 13, [.t 18, .n 14, .n 13]⟩,
  ⟨49, 146, 13, [.t 19, .n 14, .n 13]⟩,
  ⟨50, 150, 13, []⟩,
  ⟨51, 151, 14, [.t 4]⟩,
  ⟨52, 153, 14, [.t 3, .n 20]⟩,
  ⟨53, 156, 14, [.t 7, .n 10, .t 8]⟩,
  ⟨54, 160, 14, [.t 27, .t 3]⟩,
  ⟨55, 163, 14, [.t 21]⟩,
  ⟨56, 165, 14, [.t 58]⟩,
  ⟨57, 167, 14, [.t 59]⟩,
  ⟨58, 169, 14, [.t 20]⟩,
  ⟨59, 171, 15, [.n 17, .n 16]⟩,
  ⟨60, 174, 16, [.t 15, .n 17, .n 16]⟩,
  ⟨61, 178, 16, [.t 16, .n 17, .n 16]⟩,
  ⟨62, 182, 16, []⟩,
  ⟨63, 183, 17, [.n 19, .n 18]⟩,
  ⟨64, 186, 18, [.t 17, .n 19, .n 18]⟩,
  ⟨65, 190, 18, [.t 18, .n 19, .n 18]⟩,
  ⟨66, 194, 18, [.t 19, .n 19, .n 18]⟩,
  ⟨67, 198, 18, []⟩,
  ⟨68, 199, 19, [.t 4]⟩,
  ⟨69, 201, 19, [.t 3]⟩,
  ⟨70, 203, 19, [.t 7, .n 10, .t 8]⟩,
  ⟨71, 207, 19, [.t 27, .t 3]⟩,
  ⟨72, 210, 19, [.t 21]⟩,
  ⟨73, 212, 19, [.t 58]⟩,
  ⟨74, 214, 19, [.t 59]⟩,
  ⟨75, 216, 19, [.t 20]⟩,
  ⟨76, 218, 20, [.t 9, .t 10]⟩,
  ⟨77, 221, 20, []⟩
]

private theorem earlyLayout_eq : earlyRawGrammar.layout = earlyLayout := rfl
private theorem earlyStart_eq : earlyRawGrammar.cfg.start = 0 := by decide
private theorem earlyRows_eq : earlyRawGrammar.rows = earlyRows := by decide

private theorem earlyProposalTree_bounded :
    earlyProposalTree.bounded 0 (earlyLayout.scratch + 1) = true := by decide

private theorem earlyProposal_productive_check :
    checkProductive earlyLayout earlyProposalTree = true := by decide

private theorem earlyProposal_descent_check :
    checkDescent earlyLayout earlyRows earlyProposalTree = true := by decide

private theorem earlyProposal_cell_check :
    earlyProposalTree.allNodes (checkCellNode earlyLayout earlyRows earlyProposalTree) = true := by
  decide

private theorem earlyProposal_pred_check :
    earlyProposalTree.allNodes (checkPredNode earlyLayout earlyRows earlyProposalTree) = true := by
  decide

private theorem earlyProposal_closedNonCol_check :
    checkClosedNonCol 0 earlyLayout earlyRows earlyProposalTree = true := by decide

private theorem earlyProposal_closedCol_check :
    earlyProposalTree.allNodes (checkClosedColNode earlyLayout earlyRows earlyProposalTree) = true := by
  decide

private theorem earlyProposal_justified_check :
    earlyProposalTree.allNodes (checkJustifiedNode 0 earlyLayout earlyRows earlyProposalTree) = true := by
  decide

theorem earlyProposal_size :
    earlyProposal.size = earlyRawGrammar.layout.scratch + 1 := by
  rw [earlyProposal, SparseTree.fill_size, Array.size_replicate]

theorem earlyProposal_words :
    earlyProposal.size = (compileTableVerifier earlyRawGrammar).words := by
  simp [compileTableVerifier, earlyRawGrammar_representable, earlyProposal_size]

theorem earlyProposal_getD (a : Nat) :
    earlyProposal.getD a 0 = earlyProposalTree.lookup a := by
  have hb : earlyProposalTree.bounded 0 (earlyRawGrammar.layout.scratch + 1) = true := by
    rw [earlyLayout_eq]
    exact earlyProposalTree_bounded
  exact SparseTree.fill_replicate_getD hb a

theorem earlyProposal_getElemBang (a : Nat) :
    earlyProposal[a]! = earlyProposal.getD a 0 :=
  array_getElemBang_eq_getD earlyProposal a

theorem earlyProposal_valid :
    ValidCertificate earlyRawGrammar (fun a => earlyProposal.getD a 0) := by
  have hread : (fun a => earlyProposal.getD a 0) = earlyProposalTree.lookup :=
    funext earlyProposal_getD
  rw [hread]
  have hg : earlyRawGrammar.valid = true := valid_of_representable earlyRawGrammar_representable
  have hb : earlyProposalTree.bounded 0 (earlyRawGrammar.layout.scratch + 1) = true := by
    rw [earlyLayout_eq]; exact earlyProposalTree_bounded
  have hclosedNonCol : checkClosedNonCol earlyRawGrammar.cfg.start earlyRawGrammar.layout
      earlyRawGrammar.rows earlyProposalTree = true := by
    rw [earlyStart_eq, earlyLayout_eq, earlyRows_eq]; exact earlyProposal_closedNonCol_check
  have hclosedCol : earlyProposalTree.allNodes
      (checkClosedColNode earlyRawGrammar.layout earlyRawGrammar.rows earlyProposalTree) = true := by
    rw [earlyLayout_eq, earlyRows_eq]; exact earlyProposal_closedCol_check
  have hjust : earlyProposalTree.allNodes
      (checkJustifiedNode earlyRawGrammar.cfg.start earlyRawGrammar.layout
        earlyRawGrammar.rows earlyProposalTree) = true := by
    rw [earlyStart_eq, earlyLayout_eq, earlyRows_eq]; exact earlyProposal_justified_check
  have hcell : earlyProposalTree.allNodes
      (checkCellNode earlyRawGrammar.layout earlyRawGrammar.rows earlyProposalTree) = true := by
    rw [earlyLayout_eq, earlyRows_eq]; exact earlyProposal_cell_check
  have hpred : earlyProposalTree.allNodes
      (checkPredNode earlyRawGrammar.layout earlyRawGrammar.rows earlyProposalTree) = true := by
    rw [earlyLayout_eq, earlyRows_eq]; exact earlyProposal_pred_check
  have hdesc : checkDescent earlyRawGrammar.layout earlyRawGrammar.rows earlyProposalTree = true := by
    rw [earlyLayout_eq, earlyRows_eq]; exact earlyProposal_descent_check
  have hprod : checkProductive earlyRawGrammar.layout earlyProposalTree = true := by
    rw [earlyLayout_eq]; exact earlyProposal_productive_check
  exact ⟨closed_of_check hg hb hclosedNonCol hclosedCol,
    justified_of_check hg hb hjust,
    cells_of_check hb hcell hpred,
    descent_of_check hdesc,
    productive_of_check hprod⟩

theorem earlyProposal_accepted :
    (compileTableVerifier earlyRawGrammar).accepts earlyProposal earlyRawGrammar.budget = true := by
  have hvalid : ValidCertificate earlyRawGrammar (fun a => earlyProposal[a]!) := by
    have hfun : (fun a => earlyProposal[a]!) = (fun a => earlyProposal.getD a 0) :=
      funext earlyProposal_getElemBang
    rw [hfun]
    exact earlyProposal_valid
  exact compileTableVerifier_complete earlyRawGrammar_representable earlyProposal_size hvalid

end ConcreteSpec.GrammarVerification
