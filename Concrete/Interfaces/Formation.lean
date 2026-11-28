import Concrete.Interfaces.Carriers

/-!
# Register formation, backend feature policy scanning, and intra-IR occurrence accounting

Pinned interface module providing:
- `Account.mkChecked`: runtime checked constructor for `Account`;
- `RegisterFormation`: decidable well-formedness contract for `SSA` and `BIR` register CFGs
  (opcode/signature validity, literal decoding, place/layout validity, function/block inventory,
  call/return signatures, edge arguments, unique typed register definitions including distinct
  `dst`/`flag` on `addWithOverflow`, dominance of every use including terminators, and local
  structure in unreachable blocks). Its `{language : CoreLanguage}` implicit follows the
  `Contracts.lean`/`Pipeline.lean` convention (`SSA.WF`, `BIR.WF`) and is instantiated by F7 as
  `sliceLanguage s` when `lang = sliceLoweringLanguage s`;
- `firstUnsupported`: deterministic stored-order backend feature scanner over `BIR` tied
  directly to `ConcreteSpec.Core.BackendPolicy.supportsFeature` and projecting to
  `RegisteredRule` (likewise instantiated by F7 with `language := sliceLanguage s`);
- `AccountsFor`: intra-IR occurrence-accounting contract across all four `IRPoint`s with
  full observable trap identity preservation (kind, operation ID, stable key, span, and
  rendered diagnostic bytes), unused/pure/non-trapping `.deadValue` justification, identity
  law, unconditional intermediate-occurrence composition soundness, and direct composition
  under `ComposedEliminationsJustified`.
-/

namespace Concrete.Interfaces

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface

/-! ## Private list and association-list lemmas -/

private theorem alist_lookup_mem {κ α : Type} [DecidableEq κ] {l : AList κ α} {k : κ} {v : α}
    (h : AList.lookup l k = some v) : (k, v) ∈ l := by
  induction l with
  | nil => cases h
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [AList.lookup] at h
    split at h
    · rename_i heq; subst heq; cases Option.some.inj h; exact List.mem_cons_self
    · exact List.mem_cons_of_mem _ (ih h)

private theorem alist_lookup_of_mem {κ α : Type} [DecidableEq κ] {l : AList κ α}
    (hn : (List.map Prod.fst l).Nodup) {k : κ} {v : α} (h : (k, v) ∈ l) :
    AList.lookup l k = some v := by
  induction l with
  | nil => cases h
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    rw [List.map_cons, List.nodup_cons] at hn
    rcases List.mem_cons.mp h with heq | htl
    · cases heq; simp [AList.lookup]
    · have hne : k' ≠ k := fun eq => hn.1 (eq ▸ List.mem_map.mpr ⟨(k, v), htl, rfl⟩)
      simp [AList.lookup, hne]
      exact ih hn.2 htl

private theorem alist_lookup_append {κ α : Type} [DecidableEq κ] (l₁ l₂ : AList κ α) (k : κ) :
    AList.lookup (l₁ ++ l₂) k = match AList.lookup l₁ k with
      | some v => some v
      | none => AList.lookup l₂ k := by
  induction l₁ with
  | nil => rfl
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [List.cons_append, AList.lookup]
    split
    · rfl
    · exact ih

private theorem alist_lookup_filter_key {κ α : Type} [DecidableEq κ]
    (p : κ → Bool) (l : AList κ α) (k : κ) :
    AList.lookup (l.filter fun entry => p entry.1) k =
      if p k then AList.lookup l k else none := by
  induction l with
  | nil => cases p k <;> rfl
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [List.filter_cons]
    cases hpk' : p k'
    · simp only [Bool.false_eq_true, ↓reduceIte, ih, AList.lookup]
      cases hpk : p k
      · rfl
      · simp only [↓reduceIte]
        split
        · rename_i heq; subst heq; rw [hpk'] at hpk; cases hpk
        · rfl
    · simp only [↓reduceIte, AList.lookup]
      split
      · rename_i heq; subst heq; simp [hpk']
      · exact ih

private theorem list_find?_some {α : Type} {l : List α} {p : α → Bool} {x : α}
    (h : l.find? p = some x) :
    ∃ pre post, l = pre ++ x :: post ∧ (∀ y ∈ pre, p y = false) ∧ p x = true := by
  induction l with
  | nil => cases h
  | cons a rest ih =>
    simp only [List.find?] at h
    cases hpa : p a
    · simp only [hpa] at h
      obtain ⟨pre, post, rfl, hpre, hpx⟩ := ih h
      refine ⟨a :: pre, post, rfl, ?_, hpx⟩
      intro y hy
      rcases List.mem_cons.mp hy with rfl | hrest
      · exact hpa
      · exact hpre y hrest
    · simp only [hpa, Option.some.injEq] at h
      subst h
      exact ⟨[], rest, rfl, (fun _ hm => nomatch hm), hpa⟩

private theorem list_find?_none_iff {α : Type} {l : List α} {p : α → Bool} :
    l.find? p = none ↔ ∀ x ∈ l, p x = false := by
  induction l with
  | nil => simp [List.find?]
  | cons a rest ih =>
    simp only [List.find?]
    cases hpa : p a
    · simp only [ih]
      constructor
      · intro h x hx
        rcases List.mem_cons.mp hx with rfl | hrest
        · exact hpa
        · exact h x hrest
      · intro h x hx
        exact h x (List.mem_cons_of_mem a hx)
    · constructor
      · intro h; cases h
      · intro h
        have hfalse := h a List.mem_cons_self
        rw [hpa] at hfalse
        cases hfalse

private def decidable_of_bool_iff {P : Prop} (b : Bool) (h : b = true ↔ P) : Decidable P :=
  match b, h with
  | true, h => isTrue (h.mp rfl)
  | false, h => isFalse fun hp => Bool.noConfusion (h.mpr hp)

/-! ## Checked runtime constructor for `Account` -/

/-- Runtime-checked constructor for `Account`. Verifies all four structural invariants
without requiring compile-time `decide` proofs, returning `none` if any invariant fails. -/
def Account.mkChecked
    (mapped : AList Occurrence (List Occurrence))
    (eliminated : AList Occurrence EliminationReason) : Option Account :=
  if h1 : (List.map Prod.fst mapped).Nodup then
    if h2 : ∀ entry ∈ mapped, entry.2.Nodup then
      if h3 : (List.map Prod.fst eliminated).Nodup then
        if h4 : ∀ entry ∈ mapped, ∀ src ∈ entry.2, AList.lookup eliminated src = none then
          some ⟨mapped, eliminated, h1, h2, h3, h4⟩
        else none
      else none
    else none
  else none

theorem Account.mkChecked_eq_some_iff
    (mapped : AList Occurrence (List Occurrence))
    (eliminated : AList Occurrence EliminationReason) (account : Account) :
    Account.mkChecked mapped eliminated = some account ↔
      account.mapped = mapped ∧ account.eliminated = eliminated := by
  unfold Account.mkChecked
  constructor
  · intro h
    split at h
    · split at h
      · split at h
        · split at h
          · cases Option.some.inj h; exact ⟨rfl, rfl⟩
          · cases h
        · cases h
      · cases h
    · cases h
  · rintro ⟨hm, he⟩
    cases account with
    | mk m e h1 h2 h3 h4 =>
      dsimp only at hm he
      subst hm he
      rw [dite_eq_left h1, dite_eq_left h2, dite_eq_left h3, dite_eq_left h4]

theorem Account.mkChecked_isSome_iff
    (mapped : AList Occurrence (List Occurrence))
    (eliminated : AList Occurrence EliminationReason) :
    (Account.mkChecked mapped eliminated).isSome = true ↔
      (List.map Prod.fst mapped).Nodup ∧
      (∀ entry ∈ mapped, entry.2.Nodup) ∧
      (List.map Prod.fst eliminated).Nodup ∧
      (∀ entry ∈ mapped, ∀ src ∈ entry.2, AList.lookup eliminated src = none) := by
  unfold Account.mkChecked
  split
  · rename_i h1
    split
    · rename_i h2
      split
      · rename_i h3
        split
        · rename_i h4
          simp only [Option.isSome_some, true_iff]
          exact ⟨h1, h2, h3, h4⟩
        · rename_i h4
          simp only [Option.isSome_none, Bool.false_eq_true, false_iff]
          intro h; exact h4 h.2.2.2
      · rename_i h3
        simp only [Option.isSome_none, Bool.false_eq_true, false_iff]
        intro h; exact h3 h.2.2.1
    · rename_i h2
      simp only [Option.isSome_none, Bool.false_eq_true, false_iff]
      intro h; exact h2 h.2.1
  · rename_i h1
    simp only [Option.isSome_none, Bool.false_eq_true, false_iff]
    intro h; exact h1 h.1

theorem Account.mkChecked_of_account (account : Account) :
    Account.mkChecked account.mapped account.eliminated = some account :=
  (Account.mkChecked_eq_some_iff account.mapped account.eliminated account).mpr ⟨rfl, rfl⟩

/-! ## Finite-graph reachability and dominance -/

/-- Directed path in a finite graph `(nodes, edges)` from `start` to `target` whose
vertices all belong to `nodes` and avoid `avoid` (when `some d`). -/
inductive GraphPath {α : Type} (nodes : List α) (edges : List (α × α))
    (avoid : Option α) : α → α → Prop where
  | refl {b : α} :
      b ∈ nodes → avoid ≠ some b → GraphPath nodes edges avoid b b
  | snoc {a b c : α} :
      GraphPath nodes edges avoid a b → (b, c) ∈ edges →
      c ∈ nodes → avoid ≠ some c → GraphPath nodes edges avoid a c

/-- `target` is reachable from `entry` in `(nodes, edges)`. -/
def Reachable {α : Type} (nodes : List α) (edges : List (α × α)) (entry target : α) : Prop :=
  GraphPath nodes edges none entry target

/-- `dom` dominates `target` from `entry`: no directed path from `entry` to `target`
in `(nodes, edges)` avoids `dom`. -/
def Dominates {α : Type} (nodes : List α) (edges : List (α × α)) (entry dom target : α) : Prop :=
  ¬ GraphPath nodes edges (some dom) entry target

def notAvoided {α : Type} [DecidableEq α] (avoid : Option α) (v : α) : Bool :=
  match avoid with
  | none => true
  | some d => v != d

private theorem notAvoided_eq_true_iff {α : Type} [DecidableEq α] (avoid : Option α) (v : α) :
    notAvoided avoid v = true ↔ avoid ≠ some v := by
  cases avoid with
  | none => simp [notAvoided]
  | some d =>
    simp only [notAvoided, bne_iff_ne, ne_eq, Option.some.injEq]
    exact ⟨fun h heq => h heq.symm, fun h heq => h heq.symm⟩

def expandGraph {α : Type} [DecidableEq α] (nodes : List α) (edges : List (α × α))
    (avoid : Option α) (reached : List α) : List α :=
  nodes.filter fun v =>
    notAvoided avoid v &&
      (reached.contains v || edges.any fun e => reached.contains e.1 && e.2 == v)

def reachGraphLoop {α : Type} [DecidableEq α] (nodes : List α) (edges : List (α × α))
    (avoid : Option α) : Nat → List α → List α
  | 0, acc => acc
  | fuel + 1, acc =>
    let next := expandGraph nodes edges avoid acc
    if next == acc then acc else reachGraphLoop nodes edges avoid fuel next

/-- Executable fixed-point reachability on `(nodes, edges)` starting at `entry` and
avoiding `avoid`. Terminates early as soon as an iteration stabilizes. -/
def reachGraph {α : Type} [DecidableEq α] (nodes : List α) (edges : List (α × α))
    (avoid : Option α) (entry : α) : List α :=
  let init := nodes.filter fun v => v == entry && notAvoided avoid v
  reachGraphLoop nodes edges avoid nodes.length init

private def reachGraphIter {α : Type} [DecidableEq α] (nodes : List α) (edges : List (α × α))
    (avoid : Option α) : Nat → List α → List α
  | 0, acc => acc
  | n + 1, acc => reachGraphIter nodes edges avoid n (expandGraph nodes edges avoid acc)

private theorem reachGraphIter_fixed {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (avoid : Option α) {acc : List α}
    (hfix : expandGraph nodes edges avoid acc = acc) :
    ∀ n, reachGraphIter nodes edges avoid n acc = acc
  | 0 => rfl
  | n + 1 => by
    simp only [reachGraphIter, hfix]
    exact reachGraphIter_fixed nodes edges avoid hfix n

private theorem reachGraphLoop_eq_iter {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (avoid : Option α) :
    ∀ n acc, reachGraphLoop nodes edges avoid n acc = reachGraphIter nodes edges avoid n acc
  | 0, _ => rfl
  | n + 1, acc => by
    simp only [reachGraphLoop, reachGraphIter]
    cases h : (expandGraph nodes edges avoid acc == acc)
    · simp only [Bool.false_eq_true, ↓reduceIte]
      exact reachGraphLoop_eq_iter nodes edges avoid n _
    · simp only [↓reduceIte]
      have hfix : expandGraph nodes edges avoid acc = acc := eq_of_beq h
      rw [hfix, reachGraphIter_fixed nodes edges avoid hfix n]

private def reachGraphSteps {α : Type} [DecidableEq α] (nodes : List α) (edges : List (α × α))
    (avoid : Option α) (entry : α) : Nat → List α
  | 0 => nodes.filter fun v => v == entry && notAvoided avoid v
  | n + 1 => expandGraph nodes edges avoid (reachGraphSteps nodes edges avoid entry n)

private theorem reachGraphIter_step {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (avoid : Option α) :
    ∀ n acc, reachGraphIter nodes edges avoid (n + 1) acc =
      expandGraph nodes edges avoid (reachGraphIter nodes edges avoid n acc)
  | 0, _ => rfl
  | n + 1, acc => by
    simp only [reachGraphIter]
    exact reachGraphIter_step nodes edges avoid n (expandGraph nodes edges avoid acc)

private theorem reachGraphIter_eq_steps {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (avoid : Option α) (entry : α) :
    ∀ n, reachGraphIter nodes edges avoid n
        (nodes.filter fun v => v == entry && notAvoided avoid v) =
      reachGraphSteps nodes edges avoid entry n
  | 0 => rfl
  | n + 1 => by
    rw [reachGraphIter_step, reachGraphIter_eq_steps nodes edges avoid entry n]
    rfl

private theorem reachGraph_eq_steps {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (avoid : Option α) (entry : α) :
    reachGraph nodes edges avoid entry = reachGraphSteps nodes edges avoid entry nodes.length := by
  unfold reachGraph
  rw [reachGraphLoop_eq_iter, reachGraphIter_eq_steps]

private theorem reachGraphSteps_sound {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (avoid : Option α) (entry : α) :
    ∀ k u, u ∈ reachGraphSteps nodes edges avoid entry k → GraphPath nodes edges avoid entry u
  | 0, u, hu => by
    have hf := List.mem_filter.mp hu
    have hcond := Bool.and_eq_true_iff.mp hf.2
    have heq : u = entry := eq_of_beq hcond.1
    subst heq
    exact .refl hf.1 ((notAvoided_eq_true_iff avoid u).mp hcond.2)
  | k + 1, u, hu => by
    have hf := List.mem_filter.mp hu
    have hcond := Bool.and_eq_true_iff.mp hf.2
    have hna : avoid ≠ some u := (notAvoided_eq_true_iff avoid u).mp hcond.1
    rcases Bool.or_eq_true_iff.mp hcond.2 with hprev | hany
    · exact reachGraphSteps_sound nodes edges avoid entry k u (List.mem_of_elem_eq_true hprev)
    · obtain ⟨⟨a, b⟩, hedge, hmatch⟩ := List.any_eq_true.mp hany
      have hm := Bool.and_eq_true_iff.mp hmatch
      have hpa := reachGraphSteps_sound nodes edges avoid entry k a (List.mem_of_elem_eq_true hm.1)
      have hbu : b = u := eq_of_beq hm.2
      subst hbu
      exact .snoc hpa hedge hf.1 hna

private theorem reachGraphSteps_subset {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (avoid : Option α) (entry : α) :
    ∀ k u, u ∈ reachGraphSteps nodes edges avoid entry k →
      u ∈ nodes ∧ notAvoided avoid u = true
  | 0, u, hu => by
    have hf := List.mem_filter.mp hu
    exact ⟨hf.1, (Bool.and_eq_true_iff.mp hf.2).2⟩
  | k + 1, u, hu => by
    have hf := List.mem_filter.mp hu
    exact ⟨hf.1, (Bool.and_eq_true_iff.mp hf.2).1⟩

private theorem reachGraphSteps_mono_succ {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (avoid : Option α) (entry : α) (k : Nat) {u : α}
    (hu : u ∈ reachGraphSteps nodes edges avoid entry k) :
    u ∈ reachGraphSteps nodes edges avoid entry (k + 1) := by
  obtain ⟨hmem, hna⟩ := reachGraphSteps_subset nodes edges avoid entry k u hu
  refine List.mem_filter.mpr ⟨hmem, ?_⟩
  simp only [hna, Bool.true_and, List.elem_eq_true_of_mem hu, Bool.true_or]

private theorem reachGraphSteps_mono {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (avoid : Option α) (entry : α) {k₁ k₂ : Nat}
    (hle : k₁ ≤ k₂) {u : α} (hu : u ∈ reachGraphSteps nodes edges avoid entry k₁) :
    u ∈ reachGraphSteps nodes edges avoid entry k₂ := by
  induction hle with
  | refl => exact hu
  | step _ ih => exact reachGraphSteps_mono_succ nodes edges avoid entry _ ih

private inductive RevWalk {α : Type} (nodes : List α) (edges : List (α × α))
    (avoid : Option α) : α → α → List α → Prop where
  | single {a : α} :
      a ∈ nodes → avoid ≠ some a → RevWalk nodes edges avoid a a [a]
  | step {a b c : α} {rest : List α} :
      RevWalk nodes edges avoid a b rest → (b, c) ∈ edges →
      c ∈ nodes → avoid ≠ some c → RevWalk nodes edges avoid a c (c :: rest)

private theorem graphPath_to_revWalk {α : Type} {nodes : List α} {edges : List (α × α)}
    {avoid : Option α} {a b : α} (hp : GraphPath nodes edges avoid a b) :
    ∃ w, RevWalk nodes edges avoid a b w := by
  induction hp with
  | refl hb hna => exact ⟨[_], .single hb hna⟩
  | snoc _ hedge hc hna ih =>
    obtain ⟨w, hw⟩ := ih
    exact ⟨_ :: w, .step hw hedge hc hna⟩

private theorem revWalk_mem_nodes {α : Type} {nodes : List α} {edges : List (α × α)}
    {avoid : Option α} {a b : α} {w : List α} (hw : RevWalk nodes edges avoid a b w) :
    ∀ x ∈ w, x ∈ nodes := by
  induction hw with
  | single ha _ =>
    intro x hx
    cases List.mem_singleton.mp hx
    exact ha
  | step _ _ hc _ ih =>
    intro x hx
    rcases List.mem_cons.mp hx with rfl | hrest
    · exact hc
    · exact ih x hrest

private theorem revWalk_suffix_of_mem {α : Type} {nodes : List α} {edges : List (α × α)}
    {avoid : Option α} {a b c : α} {w : List α}
    (hw : RevWalk nodes edges avoid a b w) (hnd : w.Nodup) (hc : c ∈ w) :
    ∃ w', RevWalk nodes edges avoid a c w' ∧ w'.Nodup := by
  induction hw with
  | single ha hna =>
    cases List.mem_singleton.mp hc
    exact ⟨[_], .single ha hna, hnd⟩
  | step hrest _ _ _ ih =>
    rw [List.nodup_cons] at hnd
    rcases List.mem_cons.mp hc with rfl | hmem
    · exact ⟨_, .step hrest ‹_› ‹_› ‹_›, List.nodup_cons.mpr hnd⟩
    · exact ih hnd.2 hmem

private theorem revWalk_to_nodup {α : Type} [DecidableEq α] {nodes : List α} {edges : List (α × α)}
    {avoid : Option α} {a b : α} {w : List α} (hw : RevWalk nodes edges avoid a b w) :
    ∃ w', RevWalk nodes edges avoid a b w' ∧ w'.Nodup := by
  induction hw with
  | single ha hna =>
    exact ⟨[_], .single ha hna, List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩⟩
  | @step _ c _ _ hedge hc hna ih =>
    obtain ⟨w', hw', hnd⟩ := ih
    by_cases hmem : c ∈ w'
    · exact revWalk_suffix_of_mem hw' hnd hmem
    · exact ⟨c :: w', .step hw' hedge hc hna, List.nodup_cons.mpr ⟨hmem, hnd⟩⟩

private theorem length_erase_of_mem {α : Type} [DecidableEq α] {xs : List α} {a : α}
    (h : a ∈ xs) : (xs.erase a).length + 1 = xs.length := by
  induction xs with
  | nil => cases h
  | cons x rest ih =>
    simp only [List.erase_cons]
    by_cases hxa : x = a
    · subst hxa; simp
    · have hbeq : (x == a) = false := beq_false_of_ne hxa
      simp only [hbeq, Bool.false_eq_true, ↓reduceIte, List.length_cons]
      rcases List.mem_cons.mp h with rfl | hrest
      · exact absurd rfl hxa
      · have := ih hrest; omega

private theorem mem_erase_of_ne {α : Type} [DecidableEq α] {xs : List α} {a b : α}
    (hne : b ≠ a) (hb : b ∈ xs) : b ∈ xs.erase a := by
  induction xs with
  | nil => cases hb
  | cons x rest ih =>
    simp only [List.erase_cons]
    by_cases hxa : x = a
    · subst hxa
      simp only [beq_self_eq_true]
      rcases List.mem_cons.mp hb with rfl | hrest
      · exact absurd rfl hne
      · exact hrest
    · have hbeq : (x == a) = false := beq_false_of_ne hxa
      simp only [hbeq]
      rcases List.mem_cons.mp hb with rfl | hrest
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem x (ih hrest)

private theorem nodup_subset_length_le {α : Type} [DecidableEq α] :
    ∀ (w nodes : List α), w.Nodup → (∀ x ∈ w, x ∈ nodes) → w.length ≤ nodes.length
  | [], _, _, _ => Nat.zero_le _
  | a :: rest, nodes, hnd, hsub => by
    rw [List.nodup_cons] at hnd
    have ha : a ∈ nodes := hsub a List.mem_cons_self
    have hsub' : ∀ x ∈ rest, x ∈ nodes.erase a := fun x hx =>
      mem_erase_of_ne (fun eq => hnd.1 (eq ▸ hx)) (hsub x (List.mem_cons_of_mem a hx))
    have hle := nodup_subset_length_le rest (nodes.erase a) hnd.2 hsub'
    have her := length_erase_of_mem ha
    simp only [List.length_cons]
    omega

private theorem revWalk_mem_steps {α : Type} [DecidableEq α] {nodes : List α}
    {edges : List (α × α)} {avoid : Option α} {a b : α} {w : List α}
    (hw : RevWalk nodes edges avoid a b w) :
    b ∈ reachGraphSteps nodes edges avoid a (w.length - 1) := by
  induction hw with
  | single ha hna =>
    refine List.mem_filter.mpr ⟨ha, ?_⟩
    simp only [beq_self_eq_true, Bool.true_and, (notAvoided_eq_true_iff avoid _).mpr hna]
  | @step b c rest hrest hedge hc hna ih =>
    have hpos : 0 < rest.length := by
      cases hrest <;> simp
    have hlen : (c :: rest).length - 1 = (rest.length - 1) + 1 := by
      simp only [List.length_cons]; omega
    rw [hlen]
    refine List.mem_filter.mpr ⟨hc, ?_⟩
    simp only [(notAvoided_eq_true_iff avoid c).mpr hna, Bool.true_and]
    apply Bool.or_eq_true_iff.mpr
    right
    exact List.any_eq_true.mpr ⟨(b, c), hedge, by
      simp only [List.contains_eq_mem, ih, decide_true, beq_self_eq_true, Bool.and_self]⟩

theorem mem_reachGraph_iff {α : Type} [DecidableEq α] (nodes : List α) (edges : List (α × α))
    (avoid : Option α) (entry u : α) :
    u ∈ reachGraph nodes edges avoid entry ↔ GraphPath nodes edges avoid entry u := by
  rw [reachGraph_eq_steps]
  constructor
  · exact reachGraphSteps_sound nodes edges avoid entry nodes.length u
  · intro hp
    obtain ⟨w, hw⟩ := graphPath_to_revWalk hp
    obtain ⟨w', hw', hnd⟩ := revWalk_to_nodup hw
    have hsub := revWalk_mem_nodes hw'
    have hle : w'.length - 1 ≤ nodes.length :=
      Nat.le_trans (Nat.sub_le _ _) (nodup_subset_length_le w' nodes hnd hsub)
    exact reachGraphSteps_mono nodes edges avoid entry hle (revWalk_mem_steps hw')

def checkReachable {α : Type} [DecidableEq α] (nodes : List α) (edges : List (α × α))
    (entry target : α) : Bool :=
  (reachGraph nodes edges none entry).contains target

def checkDominates {α : Type} [DecidableEq α] (nodes : List α) (edges : List (α × α))
    (entry dom target : α) : Bool :=
  !(reachGraph nodes edges (some dom) entry).contains target

theorem checkReachable_eq_true_iff {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (entry target : α) :
    checkReachable nodes edges entry target = true ↔ Reachable nodes edges entry target := by
  unfold checkReachable Reachable
  rw [List.contains_eq_mem, decide_eq_true_eq, mem_reachGraph_iff]

theorem checkDominates_eq_true_iff {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (entry dom target : α) :
    checkDominates nodes edges entry dom target = true ↔ Dominates nodes edges entry dom target := by
  unfold checkDominates Dominates
  rw [Bool.not_eq_true', List.contains_eq_mem, decide_eq_false_iff_not, mem_reachGraph_iff]

instance instDecidableReachable {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (entry target : α) : Decidable (Reachable nodes edges entry target) :=
  decidable_of_bool_iff (checkReachable nodes edges entry target)
    (checkReachable_eq_true_iff nodes edges entry target)

instance instDecidableDominates {α : Type} [DecidableEq α] (nodes : List α)
    (edges : List (α × α)) (entry dom target : α) : Decidable (Dominates nodes edges entry dom target) :=
  decidable_of_bool_iff (checkDominates nodes edges entry dom target)
    (checkDominates_eq_true_iff nodes edges entry dom target)

/-! ## `RegisterFormation` for `SSA` and `BIR` -/

/-- Closed sum identifying the two register-based CFG carriers (`SSA` and `BIR`). -/
inductive RegisterIR {Ty : Type} (lang : LoweringLanguage Ty) (profile : Profile) where
  | ssa (ir : SSA lang profile)
  | bir (ir : BIR lang profile)

/-- Intra-block position where a register is defined. -/
inductive DefPoint where
  | param
  | inst (index : Nat)
  deriving DecidableEq, Repr

/-- Exact definition site of a register inside a function CFG. -/
structure DefSite where
  block : BlockId
  point : DefPoint
  deriving DecidableEq, Repr

/-- Intra-block position where a register is used. -/
inductive UsePoint where
  | inst (index : Nat)
  | term
  deriving DecidableEq, Repr

/-- Exact use site of a register inside a function CFG. -/
structure UseSite where
  block : BlockId
  point : UsePoint
  deriving DecidableEq, Repr

/-- Strict intra-block ordering between a definition and a use in the same block.
Block parameters precede all body instructions and terminators; an instruction at index `i`
precedes strictly later instructions `j > i` and the block terminator. -/
def DefPoint.strictlyBefore : DefPoint → UsePoint → Bool
  | .param, _ => true
  | .inst i, .inst j => decide (i < j)
  | .inst _, .term => true

/-- Dominance of a definition site over a use site inside a function CFG:
within the same block, strict program order applies (including in unreachable blocks);
across distinct blocks, every CFG path from `entry` to `useSite.block` must pass through
`defSite.block`. -/
def DefSite.DominatesUse (nodes : List BlockId) (edges : List (BlockId × BlockId))
    (entry : BlockId) (defSite : DefSite) (useSite : UseSite) : Prop :=
  if defSite.block = useSite.block then
    defSite.point.strictlyBefore useSite.point = true
  else
    Dominates nodes edges entry defSite.block useSite.block

def DefSite.checkDominatesUse (nodes : List BlockId) (edges : List (BlockId × BlockId))
    (entry : BlockId) (defSite : DefSite) (useSite : UseSite) : Bool :=
  if defSite.block == useSite.block then
    defSite.point.strictlyBefore useSite.point
  else
    checkDominates nodes edges entry defSite.block useSite.block

theorem DefSite.checkDominatesUse_eq_true_iff (nodes : List BlockId) (edges : List (BlockId × BlockId))
    (entry : BlockId) (defSite : DefSite) (useSite : UseSite) :
    defSite.checkDominatesUse nodes edges entry useSite = true ↔
      defSite.DominatesUse nodes edges entry useSite := by
  unfold DefSite.checkDominatesUse DefSite.DominatesUse
  by_cases h : defSite.block = useSite.block
  · simp [h]
  · simp [h, checkDominates_eq_true_iff]

/-- Flatten typed `Args` into a list of existentially typed reads in evaluation order. -/
def Args.toReads {Ty : Type} {Lit : Ty → Type} :
    {types : List Ty} → Args (SSARead Ty Lit) types → List (Sigma (SSARead Ty Lit))
  | _, .nil => []
  | _, .cons arg rest => ⟨_, arg⟩ :: rest.toReads

/-- All register definitions `(id, ty)` introduced by one `SSAInst`.
Two-result `addWithOverflow` introduces both `dst` and `flag`; duplicate-free checking
therefore forbids aliasing `dst` and `flag`. -/
def SSAInst.defs {Ty : Type} {lang : LoweringLanguage Ty} : SSAInst lang → List (Nat × Ty)
  | .op (ty := ty) dst .. => [(dst.id, ty)]
  | .addWithOverflow t dst flag _ _ => [(dst.id, lang.intTy t), (flag.id, lang.bool)]
  | .load (ty := ty) dst _ => [(dst.id, ty)]
  | .destructure _ bindings => bindings.map fun (_, dst) => (dst.2.id, dst.1)
  | .call _ (some ⟨ty, dst⟩) _ => [(dst.id, ty)]
  | .store .. | .beginLifetime _ | .endLifetime _ | .call _ none _ => []

/-- All `SSARead` operands evaluated by one `SSAInst` in left-to-right order. -/
def SSAInst.reads {Ty : Type} {lang : LoweringLanguage Ty} :
    SSAInst lang → List (Sigma (SSARead Ty lang.literal))
  | .op _ _ args _ => args.toReads
  | .addWithOverflow t _ _ lhs rhs => [⟨lang.intTy t, lhs⟩, ⟨lang.intTy t, rhs⟩]
  | .store (ty := ty) _ src => [⟨ty, src⟩]
  | .call _ _ args => args
  | .load .. | .destructure .. | .beginLifetime _ | .endLifetime _ => []

/-- All register uses `(id, ty)` occurring in one `SSAInst` (including register-backed
`destructure` sources). -/
def SSAInst.regUses {Ty : Type} {lang : LoweringLanguage Ty} (inst : SSAInst lang) :
    List (Nat × Ty) :=
  let fromReads := inst.reads.filterMap fun
    | ⟨ty, .value v⟩ => some (v.id, ty)
    | ⟨_, .literal _⟩ => none
  match inst with
  | .destructure (ty := ty) (.register v) _ => (v.id, ty) :: fromReads
  | _ => fromReads

def Terminator.regReads {Ty : Type} {lang : LoweringLanguage Ty} :
    Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool) →
      List (Sigma (SSARead Ty lang.literal))
  | .jump edge => edge.arguments
  | .branch test yes no => ⟨lang.bool, test⟩ :: (yes.arguments ++ no.arguments)
  | .ret (some arg) => [arg]
  | .ret none | .trap _ _ | .invalid => []

def BIRTerm.successors {Arg Cond : Type} : BIRTerm Arg Cond → List (CFGEdge Arg)
  | .jump edge => [edge]
  | .branch _ yes no => [yes, no]
  | .ret _ | .trap _ => []

def BIRTerm.regReads {Ty : Type} {lang : LoweringLanguage Ty} :
    BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool) →
      List (Sigma (SSARead Ty lang.literal))
  | .jump edge => edge.arguments
  | .branch test yes no => ⟨lang.bool, test⟩ :: (yes.arguments ++ no.arguments)
  | .ret (some arg) => [arg]
  | .ret none | .trap _ => []

/-- Unified terminator summary for `SSA` and `BIR` register CFGs. -/
inductive RegTerminator {Ty : Type} (lang : LoweringLanguage Ty) where
  | jump (next : CFGEdge (Sigma (SSARead Ty lang.literal)))
  | branch (test : SSARead Ty lang.literal lang.bool)
      (yes no : CFGEdge (Sigma (SSARead Ty lang.literal)))
  | ret (value : Option (Sigma (SSARead Ty lang.literal)))
  | trapOk
  | invalid

def RegTerminator.ofSSA {Ty : Type} {lang : LoweringLanguage Ty} :
    Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool) →
      RegTerminator lang
  | .jump edge => .jump edge
  | .branch test yes no => .branch test yes no
  | .ret value => .ret value
  | .trap _ _ => .trapOk
  | .invalid => .invalid

def RegTerminator.ofBIR {Ty : Type} {lang : LoweringLanguage Ty} (siteCount : Nat) :
    BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool) →
      RegTerminator lang
  | .jump edge => .jump edge
  | .branch test yes no => .branch test yes no
  | .ret value => .ret value
  | .trap slot => if slot.index < siteCount then .trapOk else .invalid

def RegTerminator.successors {Ty : Type} {lang : LoweringLanguage Ty} :
    RegTerminator lang → List (CFGEdge (Sigma (SSARead Ty lang.literal)))
  | .jump edge => [edge]
  | .branch _ yes no => [yes, no]
  | .ret _ | .trapOk | .invalid => []

def RegTerminator.reads {Ty : Type} {lang : LoweringLanguage Ty} :
    RegTerminator lang → List (Sigma (SSARead Ty lang.literal))
  | .jump edge => edge.arguments
  | .branch test yes no => ⟨lang.bool, test⟩ :: (yes.arguments ++ no.arguments)
  | .ret (some arg) => [arg]
  | .ret none | .trapOk | .invalid => []

def RegTerminator.regUses {Ty : Type} {lang : LoweringLanguage Ty} (term : RegTerminator lang) :
    List (Nat × Ty) :=
  term.reads.filterMap fun
    | ⟨ty, .value v⟩ => some (v.id, ty)
    | ⟨_, .literal _⟩ => none

/-- List-backed normalized CFG used by `RegisterFormation` (avoids `Array.map` so
kernel `decide` reduces directly on closed fixtures). -/
structure NormalizedCFG {Ty : Type} (lang : LoweringLanguage Ty) where
  entry : BlockId
  blocks : List (Block (Sigma (SSAValue Ty)) (LocatedOp (SSAInst lang)) (RegTerminator lang))

/-- Normalized register CFG view shared by `SSA` and `BIR`. -/
structure NormalizedRegProgram {Ty : Type} (lang : LoweringLanguage Ty) (profile : Profile) where
  entry : FnId
  signatures : AList FnId (FunctionSignature Ty)
  functions : AList FnId (LocalTypes Ty × NormalizedCFG lang)
  layouts : AList FnId (AList PlaceId Layout)
  mutableLocals : AList FnId (List PlaceId)
  declarations : Declarations

def RegisterIR.normalize {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} :
    RegisterIR lang profile → NormalizedRegProgram lang profile
  | .ssa s =>
    { entry := s.entry
      signatures := s.signatures
      functions := List.map (fun (fn, locals, graph) =>
        (fn, locals, ⟨graph.entry, graph.blocks.toList.map fun blk =>
          ⟨blk.id, blk.params, blk.body, RegTerminator.ofSSA blk.ending, blk.boundaryOrigins⟩⟩)) s.functions
      layouts := s.layouts
      mutableLocals := s.mutableLocals
      declarations := s.declarations }
  | .bir b =>
    { entry := b.entry
      signatures := b.signatures
      functions := List.map (fun (fn, locals, graph) =>
        (fn, locals, ⟨graph.entry, graph.blocks.toList.map fun blk =>
          ⟨blk.id, blk.params, blk.body, RegTerminator.ofBIR b.sites.size blk.ending, blk.boundaryOrigins⟩⟩)) b.functions
      layouts := b.layouts
      mutableLocals := b.mutableLocals
      declarations := b.declarations }

/-- Whether `ty` is a fixed-width integer scalar in `language`. -/
def isIntType (language : CoreLanguage) (ty : language.operands.Ty) : Bool :=
  match language.scalarType ty with
  | some (.int _) => true
  | _ => false

/-- A type is valid in `(language, declarations, target)` if it is a scalar type or
a declared aggregate/nominal type with a valid spec layout. -/
def typeValid (language : CoreLanguage) (declarations : Declarations) (target : Target)
    (ty : language.operands.Ty) : Bool :=
  (language.scalarType ty).isSome || (language.layout declarations target ty).isSome

/-- Decidable opcode and operand/result signature check for `SSAInst.op`.
Covers `.read`, all `.arithmetic` machine operators (including shift count typing),
and the early nominal constructors/destructors (`constructEmpty`, `destructureEmpty`). -/
def opcodeSignatureOk (language : CoreLanguage) (declarations : Declarations) (target : Target)
    (opcode : Operator) (types : List language.operands.Ty) (ty : language.operands.Ty)
    (addresses : List (Sigma (Place language.operands.Ty))) : Bool :=
  letI := language.typeDecidableEq
  typeValid language declarations target ty &&
  types.all (typeValid language declarations target) &&
  match opcode with
  | .read =>
    addresses.isEmpty &&
    match types with
    | [inputTy] => decide (inputTy = ty)
    | _ => false
  | .arithmetic op =>
    addresses.isEmpty &&
    match types with
    | [lhsTy, rhsTy] =>
      isIntType language ty &&
      decide (lhsTy = ty) &&
      if op.countOperand then isIntType language rhsTy else decide (rhsTy = ty)
    | _ => false
  | .named name =>
    if name == ⟨"constructEmpty"⟩ then
      addresses.isEmpty && types.isEmpty && (language.scalarType ty).isNone
    else if name == ⟨"destructureEmpty"⟩ then
      addresses.isEmpty &&
      (language.scalarType ty == some .unit) &&
      match types with
      | [argTy] => (language.scalarType argTy).isNone
      | _ => false
    else false

/-- Decidable place validity against a function's local table and `language.fieldType`. -/
def placeValid (language : CoreLanguage) (locals : LocalTypes language.operands.Ty)
    {ty : language.operands.Ty} (place : Place language.operands.Ty ty) : Bool :=
  letI := language.typeDecidableEq
  match AList.lookup locals place.root with
  | none => false
  | some rootTy =>
    match place.path.foldlM language.fieldType rootTy with
    | none => false
    | some resolvedTy => decide (resolvedTy = ty)

/-- Decidable literal decoding check for a single typed read. -/
def readLiteralOk (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (read : Sigma (SSARead language.operands.Ty lang.literal)) : Bool :=
  match read with
  | ⟨ty, .literal lit⟩ => (language.literal ty lit.encoding).isSome
  | ⟨_, .value _⟩ => true

/-- Collect every register definition `(regId, (ty, defSite))` in a normalized function CFG
in stored block/parameter/instruction order. -/
def functionDefTable {Ty : Type} {lang : LoweringLanguage Ty}
    (graph : NormalizedCFG lang) :
    AList Nat (Ty × DefSite) :=
  graph.blocks.flatMap fun blk =>
    (blk.params.map fun p => (p.2.id, (p.1, ⟨blk.id, .param⟩))) ++
    (blk.body.zipIdx.flatMap fun (op, idx) =>
      op.instruction.defs.map fun (id, ty) => (id, (ty, ⟨blk.id, .inst idx⟩)))

/-- Local structural formation check on a single `SSAInst` (opcode/signature, place validity,
destructure field typing, lifetime roots, call signature, and literal decoding). -/
def instLocalOk (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (declarations : Declarations) (target : Target)
    (signatures : AList FnId (FunctionSignature language.operands.Ty))
    (locals : LocalTypes language.operands.Ty) (inst : SSAInst lang) : Bool :=
  letI := language.typeDecidableEq
  inst.reads.all (readLiteralOk language) &&
  match inst with
  | @SSAInst.op _ _ opcode types ty _ _ _ addresses =>
    opcodeSignatureOk language declarations target opcode types ty addresses &&
    addresses.all fun p => placeValid language locals p.2
  | .addWithOverflow t _ _ _ _ =>
    (language.scalarType (lang.intTy t) == some (.int t)) &&
    (language.scalarType lang.bool == some .bool)
  | .load _ src => placeValid language locals src
  | .store dst _ => placeValid language locals dst
  | .beginLifetime place | .endLifetime place => (AList.lookup locals place).isSome
  | .destructure (ty := ty) src bindings =>
    let srcOk := match src with
      | .register _ => true
      | .materialized place => placeValid language locals place
    srcOk &&
    decide (bindings.map Prod.fst).Nodup &&
    bindings.all fun (field, ⟨fieldTy, _⟩) =>
      match language.fieldType ty field with
      | some actualTy => decide (actualTy = fieldTy)
      | none => false
  | .call callee dst args =>
    match AList.lookup signatures callee with
    | none => false
    | some calleeSig =>
      decide (args.map Sigma.fst = calleeSig.parameters) &&
      decide (dst.map Sigma.fst = calleeSig.result)

/-- Declarative well-formedness of one normalized register function CFG. -/
def FunctionRegisterFormed (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (prog : NormalizedRegProgram lang profile)
    (fn : FnId) (locals : LocalTypes language.operands.Ty)
    (graph : NormalizedCFG lang) :
    Prop :=
  let nodes := graph.blocks.map Block.id
  let edges := graph.blocks.flatMap fun blk =>
    blk.ending.successors.map fun edge => (blk.id, edge.target)
  let defs := functionDefTable graph
  (List.map Prod.fst locals).Nodup ∧
  (∀ entry ∈ locals, typeValid language prog.declarations profile.memoryTarget entry.2 = true) ∧
  nodes.Nodup ∧
  (∃ sig, AList.lookup prog.signatures fn = some sig ∧
    (∃ entryBlk ∈ graph.blocks, entryBlk.id = graph.entry ∧
      entryBlk.params.map Sigma.fst = sig.parameters) ∧
    (List.map Prod.fst defs).Nodup ∧
    (∀ d ∈ defs, typeValid language prog.declarations profile.memoryTarget d.2.1 = true) ∧
    ∀ blk ∈ graph.blocks,
      (∀ edge ∈ blk.ending.successors,
        ∃ tgt ∈ graph.blocks, tgt.id = edge.target ∧
          edge.arguments.map Sigma.fst = tgt.params.map Sigma.fst) ∧
      (match blk.ending with
        | .ret operand => operand.map Sigma.fst = sig.result
        | .invalid => False
        | _ => True) ∧
      (∀ r ∈ blk.ending.reads, readLiteralOk language r = true) ∧
      (∀ use ∈ blk.ending.regUses,
        ∃ defSite, AList.lookup defs use.1 = some (use.2, defSite) ∧
          defSite.DominatesUse nodes edges graph.entry ⟨blk.id, .term⟩) ∧
      ∀ pair ∈ blk.body.zipIdx,
        instLocalOk language prog.declarations profile.memoryTarget prog.signatures locals pair.1.instruction = true ∧
        ∀ use ∈ pair.1.instruction.regUses,
          ∃ defSite, AList.lookup defs use.1 = some (use.2, defSite) ∧
            defSite.DominatesUse nodes edges graph.entry ⟨blk.id, .inst pair.2⟩)

/-- Declarative well-formedness of a normalized register program (`SSA` or `BIR`). -/
def NormalizedRegProgram.Formed (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (prog : NormalizedRegProgram lang profile) : Prop :=
  (List.map Prod.fst prog.functions).Nodup ∧
  (List.map Prod.fst prog.signatures).Nodup ∧
  (List.map Prod.fst prog.layouts).Nodup ∧
  (List.map Prod.fst prog.mutableLocals).Nodup ∧
  (List.map Prod.fst prog.declarations).Nodup ∧
  (AList.lookup prog.functions prog.entry).isSome = true ∧
  (∀ s ∈ prog.signatures,
    (∀ p ∈ s.2.parameters, typeValid language prog.declarations profile.memoryTarget p = true) ∧
    (∀ r ∈ s.2.result.toList, typeValid language prog.declarations profile.memoryTarget r = true)) ∧
  (∀ fnEntry ∈ prog.functions,
    ∀ localEntry ∈ fnEntry.2.1,
      ∃ fnLayouts layout,
        AList.lookup prog.layouts fnEntry.1 = some fnLayouts ∧
        AList.lookup fnLayouts localEntry.1 = some layout ∧
        language.layout prog.declarations profile.memoryTarget localEntry.2 = some layout ∧
        layout.WellFormed profile.memoryTarget = true) ∧
  (∀ layEntry ∈ prog.layouts,
    (List.map Prod.fst layEntry.2).Nodup ∧
    ∃ locals graph,
      AList.lookup prog.functions layEntry.1 = some (locals, graph) ∧
      ∀ slot ∈ layEntry.2,
        ∃ ty, AList.lookup locals slot.1 = some ty ∧
          language.layout prog.declarations profile.memoryTarget ty = some slot.2 ∧
          slot.2.WellFormed profile.memoryTarget = true) ∧
  (∀ mutEntry ∈ prog.mutableLocals,
    ∃ locals graph,
      AList.lookup prog.functions mutEntry.1 = some (locals, graph) ∧
      ∀ root ∈ mutEntry.2, (AList.lookup locals root).isSome = true) ∧
  ∀ fnEntry ∈ prog.functions,
    FunctionRegisterFormed language prog fnEntry.1 fnEntry.2.1 fnEntry.2.2

/-- Executable formation checker for one normalized register function CFG. -/
def checkFunctionRegisterFormed (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (prog : NormalizedRegProgram lang profile)
    (fn : FnId) (locals : LocalTypes language.operands.Ty)
    (graph : NormalizedCFG lang) :
    Bool :=
  letI := language.typeDecidableEq
  let nodes := graph.blocks.map Block.id
  let edges := graph.blocks.flatMap fun blk =>
    blk.ending.successors.map fun edge => (blk.id, edge.target)
  let defs := functionDefTable graph
  decide (List.map Prod.fst locals).Nodup &&
  locals.all (fun entry => typeValid language prog.declarations profile.memoryTarget entry.2) &&
  decide nodes.Nodup &&
  match AList.lookup prog.signatures fn with
  | none => false
  | some sig =>
    (graph.blocks.any fun entryBlk =>
      entryBlk.id == graph.entry && decide (entryBlk.params.map Sigma.fst = sig.parameters)) &&
    decide (List.map Prod.fst defs).Nodup &&
    defs.all (fun d => typeValid language prog.declarations profile.memoryTarget d.2.1) &&
    graph.blocks.all fun blk =>
      (blk.ending.successors.all fun edge =>
        graph.blocks.any fun tgt =>
          tgt.id == edge.target && decide (edge.arguments.map Sigma.fst = tgt.params.map Sigma.fst)) &&
      (match blk.ending with
        | .ret operand => decide (operand.map Sigma.fst = sig.result)
        | .invalid => false
        | _ => true) &&
      blk.ending.reads.all (readLiteralOk language) &&
      (blk.ending.regUses.all fun use =>
        match AList.lookup defs use.1 with
        | none => false
        | some (defTy, defSite) =>
          decide (defTy = use.2) && defSite.checkDominatesUse nodes edges graph.entry ⟨blk.id, .term⟩) &&
      blk.body.zipIdx.all fun pair =>
        instLocalOk language prog.declarations profile.memoryTarget prog.signatures locals pair.1.instruction &&
        pair.1.instruction.regUses.all fun use =>
          match AList.lookup defs use.1 with
          | none => false
          | some (defTy, defSite) =>
            decide (defTy = use.2) && defSite.checkDominatesUse nodes edges graph.entry ⟨blk.id, .inst pair.2⟩

/-- Executable formation checker for a normalized register program. -/
def NormalizedRegProgram.checkFormed (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (prog : NormalizedRegProgram lang profile) : Bool :=
  decide (List.map Prod.fst prog.functions).Nodup &&
  decide (List.map Prod.fst prog.signatures).Nodup &&
  decide (List.map Prod.fst prog.layouts).Nodup &&
  decide (List.map Prod.fst prog.mutableLocals).Nodup &&
  decide (List.map Prod.fst prog.declarations).Nodup &&
  (AList.lookup prog.functions prog.entry).isSome &&
  prog.signatures.all (fun s =>
    s.2.parameters.all (typeValid language prog.declarations profile.memoryTarget) &&
    s.2.result.toList.all (typeValid language prog.declarations profile.memoryTarget)) &&
  prog.functions.all (fun fnEntry =>
    fnEntry.2.1.all fun localEntry =>
      match AList.lookup prog.layouts fnEntry.1 with
      | none => false
      | some fnLayouts =>
        match AList.lookup fnLayouts localEntry.1 with
        | none => false
        | some layout =>
          decide (language.layout prog.declarations profile.memoryTarget localEntry.2 = some layout) &&
          layout.WellFormed profile.memoryTarget) &&
  prog.layouts.all (fun layEntry =>
    decide (List.map Prod.fst layEntry.2).Nodup &&
    match AList.lookup prog.functions layEntry.1 with
    | none => false
    | some (locals, _) =>
      layEntry.2.all fun slot =>
        match AList.lookup locals slot.1 with
        | none => false
        | some ty =>
          decide (language.layout prog.declarations profile.memoryTarget ty = some slot.2) &&
          slot.2.WellFormed profile.memoryTarget) &&
  prog.mutableLocals.all (fun mutEntry =>
    match AList.lookup prog.functions mutEntry.1 with
    | none => false
    | some (locals, _) =>
      mutEntry.2.all fun root => (AList.lookup locals root).isSome) &&
  prog.functions.all fun fnEntry =>
    checkFunctionRegisterFormed language prog fnEntry.1 fnEntry.2.1 fnEntry.2.2

private theorem checkUseDominated_iff (language : CoreLanguage)
    (nodes : List BlockId) (edges : List (BlockId × BlockId)) (entry : BlockId)
    (defs : AList Nat (language.operands.Ty × DefSite))
    (use : Nat × language.operands.Ty) (useSite : UseSite) :
    (letI := language.typeDecidableEq
      match AList.lookup defs use.1 with
      | none => false
      | some (defTy, defSite) =>
        decide (defTy = use.2) && defSite.checkDominatesUse nodes edges entry useSite) = true ↔
      ∃ defSite, AList.lookup defs use.1 = some (use.2, defSite) ∧
        defSite.DominatesUse nodes edges entry useSite := by
  letI := language.typeDecidableEq
  cases hlook : AList.lookup defs use.1 with
  | none =>
    simp
  | some pair =>
    rcases pair with ⟨defTy, defSite⟩
    simp only [Bool.and_eq_true, decide_eq_true_eq, DefSite.checkDominatesUse_eq_true_iff]
    constructor
    · rintro ⟨rfl, hdom⟩
      exact ⟨defSite, rfl, hdom⟩
    · rintro ⟨ds, heq, hdom⟩
      rcases Prod.mk.inj (Option.some.inj heq) with ⟨rfl, rfl⟩
      exact ⟨rfl, hdom⟩

private theorem checkFunctionRegisterFormed_eq_true_iff (language : CoreLanguage)
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (prog : NormalizedRegProgram lang profile)
    (fn : FnId) (locals : LocalTypes language.operands.Ty)
    (graph : NormalizedCFG lang) :
    checkFunctionRegisterFormed language prog fn locals graph = true ↔
      FunctionRegisterFormed language prog fn locals graph := by
  letI := language.typeDecidableEq
  unfold checkFunctionRegisterFormed FunctionRegisterFormed
  dsimp only
  cases hsig : AList.lookup prog.signatures fn with
  | none =>
    simp
  | some sig =>
    simp only [Bool.and_eq_true, decide_eq_true_eq, List.all_eq_true, Option.some.injEq,
      exists_eq_left']
    constructor
    · rintro ⟨⟨⟨hlocNd, hlocTy⟩, hnodNd⟩, ⟨⟨⟨hentry, hdefNd⟩, hdefTy⟩, hblks⟩⟩
      refine ⟨hlocNd, hlocTy, hnodNd, ?_, hdefNd, hdefTy, ?_⟩
      · obtain ⟨eb, hebMem, hebCond⟩ := List.any_eq_true.mp hentry
        have hc := Bool.and_eq_true_iff.mp hebCond
        exact ⟨eb, hebMem, eq_of_beq hc.1, of_decide_eq_true hc.2⟩
      · intro blk hblk
        have hb := hblks blk hblk
        rcases hb with ⟨⟨⟨⟨hedges, hend⟩, htermLits⟩, htermUses⟩, hbody⟩
        refine ⟨?_, ?_, htermLits, ?_, ?_⟩
        · intro edge hedge
          obtain ⟨tgt, htgtMem, htgtCond⟩ := List.any_eq_true.mp (hedges edge hedge)
          have hc := Bool.and_eq_true_iff.mp htgtCond
          exact ⟨tgt, htgtMem, eq_of_beq hc.1, of_decide_eq_true hc.2⟩
        · cases he : blk.ending with
          | jump _ => trivial
          | branch _ _ _ => trivial
          | ret operand => rw [he] at hend; exact of_decide_eq_true hend
          | trapOk => trivial
          | invalid => rw [he] at hend; cases hend
        · intro use huse
          exact (checkUseDominated_iff language _ _ _ _ use ⟨blk.id, .term⟩).mp (htermUses use huse)
        · intro pair hpair
          have hp := hbody pair hpair
          refine ⟨hp.1, fun use huse => ?_⟩
          exact (checkUseDominated_iff language _ _ _ _ use ⟨blk.id, .inst pair.2⟩).mp (hp.2 use huse)
    · rintro ⟨hlocNd, hlocTy, hnodNd, ⟨eb, hebMem, hebId, hebParams⟩, hdefNd, hdefTy, hblks⟩
      refine ⟨⟨⟨hlocNd, hlocTy⟩, hnodNd⟩, ⟨⟨⟨?_, hdefNd⟩, hdefTy⟩, ?_⟩⟩
      · exact List.any_eq_true.mpr ⟨eb, hebMem, by simp [hebId, hebParams]⟩
      · intro blk hblk
        obtain ⟨hedges, hend, htermLits, htermUses, hbody⟩ := hblks blk hblk
        refine ⟨⟨⟨⟨?_, ?_⟩, htermLits⟩, ?_⟩, ?_⟩
        · intro edge hedge
          obtain ⟨tgt, htgtMem, htgtId, htgtArgs⟩ := hedges edge hedge
          exact List.any_eq_true.mpr ⟨tgt, htgtMem, by simp [htgtId, htgtArgs]⟩
        · cases he : blk.ending with
          | jump _ => rfl
          | branch _ _ _ => rfl
          | ret operand => rw [he] at hend; exact decide_eq_true hend
          | trapOk => rfl
          | invalid => rw [he] at hend; exact hend.elim
        · intro use huse
          exact (checkUseDominated_iff language _ _ _ _ use ⟨blk.id, .term⟩).mpr (htermUses use huse)
        · intro pair hpair
          obtain ⟨hloc, huses⟩ := hbody pair hpair
          exact ⟨hloc, fun use huse =>
            (checkUseDominated_iff language _ _ _ _ use ⟨blk.id, .inst pair.2⟩).mpr (huses use huse)⟩

private theorem NormalizedRegProgram.checkFormed_eq_true_iff (language : CoreLanguage)
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (prog : NormalizedRegProgram lang profile) :
    prog.checkFormed language = true ↔ prog.Formed language := by
  unfold NormalizedRegProgram.checkFormed NormalizedRegProgram.Formed
  simp only [Bool.and_eq_true, decide_eq_true_eq, List.all_eq_true]
  constructor
  · rintro ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨hfnNd, hsigNd⟩, hlayNd⟩, hmutNd⟩, hdeclNd⟩, hentry⟩, hsigTy⟩, hfnLay⟩, hlayInv⟩, hmutInv⟩, hfns⟩
    refine ⟨hfnNd, hsigNd, hlayNd, hmutNd, hdeclNd, hentry, hsigTy, ?_, ?_, ?_, ?_⟩
    · intro fnEntry hfn localEntry hloc
      have h := hfnLay fnEntry hfn localEntry hloc
      cases hl1 : AList.lookup prog.layouts fnEntry.1 with
      | none => simp [hl1] at h
      | some fnLayouts =>
        simp only [hl1] at h
        cases hl2 : AList.lookup fnLayouts localEntry.1 with
        | none => simp [hl2] at h
        | some layout =>
          simp only [hl2, Bool.and_eq_true, decide_eq_true_eq] at h
          exact ⟨fnLayouts, layout, rfl, hl2, h.1, h.2⟩
    · intro layEntry hlay
      obtain ⟨hnd, hrest⟩ := hlayInv layEntry hlay
      refine ⟨hnd, ?_⟩
      cases hfn : AList.lookup prog.functions layEntry.1 with
      | none => simp [hfn] at hrest
      | some pair =>
        rcases pair with ⟨locals, graph⟩
        simp only [hfn, List.all_eq_true] at hrest
        refine ⟨locals, graph, rfl, fun slot hslot => ?_⟩
        have hs := hrest slot hslot
        cases hlt : AList.lookup locals slot.1 with
        | none => simp [hlt] at hs
        | some ty =>
          simp only [hlt, Bool.and_eq_true, decide_eq_true_eq] at hs
          exact ⟨ty, rfl, hs.1, hs.2⟩
    · intro mutEntry hmut
      have h := hmutInv mutEntry hmut
      cases hfn : AList.lookup prog.functions mutEntry.1 with
      | none => simp [hfn] at h
      | some pair =>
        rcases pair with ⟨locals, graph⟩
        simp only [hfn, List.all_eq_true] at h
        exact ⟨locals, graph, rfl, h⟩
    · intro fnEntry hfn
      exact (checkFunctionRegisterFormed_eq_true_iff language prog _ _ _).mp (hfns fnEntry hfn)
  · rintro ⟨hfnNd, hsigNd, hlayNd, hmutNd, hdeclNd, hentry, hsigTy, hfnLay, hlayInv, hmutInv, hfns⟩
    refine ⟨⟨⟨⟨⟨⟨⟨⟨⟨⟨hfnNd, hsigNd⟩, hlayNd⟩, hmutNd⟩, hdeclNd⟩, hentry⟩, hsigTy⟩, ?_⟩, ?_⟩, ?_⟩, ?_⟩
    · intro fnEntry hfn localEntry hloc
      obtain ⟨fnLayouts, layout, hl1, hl2, hmatch, hwf⟩ := hfnLay fnEntry hfn localEntry hloc
      simp [hl1, hl2, hmatch, hwf]
    · intro layEntry hlay
      obtain ⟨hnd, locals, graph, hfn, hslots⟩ := hlayInv layEntry hlay
      refine ⟨hnd, ?_⟩
      simp only [hfn, List.all_eq_true]
      intro slot hslot
      obtain ⟨ty, hlt, hmatch, hwf⟩ := hslots slot hslot
      simp [hlt, hmatch, hwf]
    · intro mutEntry hmut
      obtain ⟨locals, graph, hfn, hroots⟩ := hmutInv mutEntry hmut
      simp only [hfn, List.all_eq_true]
      exact hroots
    · intro fnEntry hfn
      exact (checkFunctionRegisterFormed_eq_true_iff language prog _ _ _).mpr (hfns fnEntry hfn)

/-- Register formation contract for `SSA` and `BIR` CFGs. Enforces:
1. Function, signature, layout, mutable-local, and declaration inventory uniqueness and entry presence;
2. Recursive `Layout.WellFormed` and spec-layout agreement for every materialized local;
3. Valid block inventory, edge targets, and typed edge arguments (including in unreachable blocks);
4. Call and return signature matching and exclusion of invalid terminators / out-of-bounds trap slots;
5. Opcode/signature validity, place validity, and literal decoding;
6. Function-wide unique typed register definitions (including distinct `dst`/`flag` on `addWithOverflow`);
7. Dominance of every register use in body instructions and terminators (with strict intra-block
   order enforced even in unreachable blocks).

Note: `{language : CoreLanguage}` follows the repository's `{language : CoreLanguage}
{lang : LoweringLanguage language.operands.Ty}` parameterization (`SSA.WF`, `BIR.WF`, `BuildsSSA`,
`BuildsBIR` in `Contracts.lean`/`Pipeline.lean`, because `LoweringLanguage` in `Carriers.lean` is
indexed only by `Ty`). Freeze unit F7 instantiates `language := sliceLanguage s` whenever
`lang = sliceLoweringLanguage s` (on `At s profile .ssa` / `At s profile .bir`). -/
def RegisterFormation {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (ir : RegisterIR lang profile) : Prop :=
  ir.normalize.Formed language

/-- Executable register-formation decision procedure matching `RegisterFormation`. -/
def checkRegisterFormation {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (ir : RegisterIR lang profile) : Bool :=
  ir.normalize.checkFormed language

theorem checkRegisterFormation_eq_true_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (ir : RegisterIR lang profile) :
    checkRegisterFormation (language := language) ir = true ↔
      RegisterFormation (language := language) ir :=
  NormalizedRegProgram.checkFormed_eq_true_iff language ir.normalize

instance instDecidableRegisterFormation {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (ir : RegisterIR lang profile) : Decidable (RegisterFormation (language := language) ir) :=
  decidable_of_bool_iff (checkRegisterFormation (language := language) ir)
    (checkRegisterFormation_eq_true_iff ir)

abbrev SSA.RegisterFormation {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (ssa : SSA lang profile) : Prop :=
  Concrete.Interfaces.RegisterFormation (language := language) (.ssa ssa)

abbrev SSA.checkRegisterFormation {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile} (ssa : SSA lang profile) :
    Bool :=
  Concrete.Interfaces.checkRegisterFormation (language := language) (.ssa ssa)

theorem SSA.checkRegisterFormation_eq_true_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile} (ssa : SSA lang profile) :
    ssa.checkRegisterFormation (language := language) = true ↔
      ssa.RegisterFormation (language := language) :=
  Concrete.Interfaces.checkRegisterFormation_eq_true_iff (.ssa ssa)

abbrev BIR.RegisterFormation {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) : Prop :=
  Concrete.Interfaces.RegisterFormation (language := language) (.bir bir)

abbrev BIR.checkRegisterFormation {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile} (bir : BIR lang profile) :
    Bool :=
  Concrete.Interfaces.checkRegisterFormation (language := language) (.bir bir)

theorem BIR.checkRegisterFormation_eq_true_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile} (bir : BIR lang profile) :
    bir.checkRegisterFormation (language := language) = true ↔
      bir.RegisterFormation (language := language) :=
  Concrete.Interfaces.checkRegisterFormation_eq_true_iff (.bir bir)

/-! ### Projection lemmas from `RegisterFormation` -/

theorem SSA.RegisterFormation.functions_nodup {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile} {ssa : SSA lang profile}
    (h : ssa.RegisterFormation (language := language)) :
    (List.map Prod.fst ssa.functions).Nodup := by
  have hnd := h.1
  change (List.map Prod.fst (List.map (fun (fn, locals, graph) =>
    (fn, locals, (⟨graph.entry, graph.blocks.toList.map fun blk =>
      ⟨blk.id, blk.params, blk.body, RegTerminator.ofSSA blk.ending, blk.boundaryOrigins⟩⟩ :
      NormalizedCFG lang))) ssa.functions)).Nodup at hnd
  rw [List.map_map] at hnd
  exact hnd

theorem SSA.RegisterFormation.entry_mem {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile} {ssa : SSA lang profile}
    (h : ssa.RegisterFormation (language := language)) :
    ssa.entry ∈ List.map Prod.fst ssa.functions := by
  have hentry := h.2.2.2.2.2.1
  cases hlook : AList.lookup (RegisterIR.ssa ssa).normalize.functions ssa.entry with
  | none =>
    change (AList.lookup (RegisterIR.ssa ssa).normalize.functions ssa.entry).isSome = true at hentry
    rw [hlook] at hentry
    cases hentry
  | some val =>
    have hmem := alist_lookup_mem hlook
    unfold RegisterIR.normalize at hmem
    dsimp only at hmem
    obtain ⟨orig, horig, heq⟩ := List.mem_map.mp hmem
    exact List.mem_map.mpr ⟨orig, horig, (Prod.mk.inj heq).1⟩

theorem SSA.RegisterFormation.no_invalid_terminator {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile} {ssa : SSA lang profile}
    (h : ssa.RegisterFormation (language := language))
    {fn locals graph} (hfn : (fn, locals, graph) ∈ ssa.functions)
    {blk} (hblk : blk ∈ graph.blocks.toList) :
    blk.ending ≠ .invalid := by
  intro hinv
  have hnormMem : (fn, locals, (⟨graph.entry, graph.blocks.toList.map fun b =>
      ⟨b.id, b.params, b.body, RegTerminator.ofSSA b.ending, b.boundaryOrigins⟩⟩ :
      NormalizedCFG lang)) ∈
      (RegisterIR.ssa ssa).normalize.functions := by
    unfold RegisterIR.normalize
    dsimp only
    exact List.mem_map.mpr ⟨(fn, locals, graph), hfn, rfl⟩
  have hfnFormed := h.2.2.2.2.2.2.2.2.2.2 _ hnormMem
  obtain ⟨_, _, _, _, _, hblks⟩ := hfnFormed.2.2.2
  have hblkNorm : (⟨blk.id, blk.params, blk.body,
      RegTerminator.ofSSA blk.ending, blk.boundaryOrigins⟩ :
      Block _ _ (RegTerminator lang)) ∈
      (graph.blocks.toList.map fun b =>
        ⟨b.id, b.params, b.body, RegTerminator.ofSSA b.ending, b.boundaryOrigins⟩) :=
    List.mem_map.mpr ⟨blk, hblk, rfl⟩
  have hend := (hblks _ hblkNorm).2.1
  rw [hinv] at hend
  exact hend

theorem RegisterFormation.layouts_wf {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {ir : RegisterIR lang profile} (h : RegisterFormation (language := language) ir)
    {fn fnLayouts} (hfn : (fn, fnLayouts) ∈ ir.normalize.layouts)
    {place layout} (hslot : (place, layout) ∈ fnLayouts) :
    layout.WellFormed profile.memoryTarget = true := by
  obtain ⟨_, _, _, _, hslots⟩ := h.2.2.2.2.2.2.2.2.1 (fn, fnLayouts) hfn
  obtain ⟨_, _, _, hwf⟩ := hslots (place, layout) hslot
  exact hwf

theorem BIR.RegisterFormation.functions_nodup {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile} {bir : BIR lang profile}
    (h : bir.RegisterFormation (language := language)) :
    (List.map Prod.fst bir.functions).Nodup := by
  have hnd := h.1
  change (List.map Prod.fst (List.map (fun (fn, locals, graph) =>
    (fn, locals, (⟨graph.entry, graph.blocks.toList.map fun blk =>
      ⟨blk.id, blk.params, blk.body, RegTerminator.ofBIR bir.sites.size blk.ending, blk.boundaryOrigins⟩⟩ :
      NormalizedCFG lang))) bir.functions)).Nodup at hnd
  rw [List.map_map] at hnd
  exact hnd

theorem BIR.RegisterFormation.entry_mem {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile} {bir : BIR lang profile}
    (h : bir.RegisterFormation (language := language)) :
    bir.entry ∈ List.map Prod.fst bir.functions := by
  have hentry := h.2.2.2.2.2.1
  cases hlook : AList.lookup (RegisterIR.bir bir).normalize.functions bir.entry with
  | none =>
    change (AList.lookup (RegisterIR.bir bir).normalize.functions bir.entry).isSome = true at hentry
    rw [hlook] at hentry
    cases hentry
  | some val =>
    have hmem := alist_lookup_mem hlook
    unfold RegisterIR.normalize at hmem
    dsimp only at hmem
    obtain ⟨orig, horig, heq⟩ := List.mem_map.mp hmem
    exact List.mem_map.mpr ⟨orig, horig, (Prod.mk.inj heq).1⟩

theorem BIR.RegisterFormation.trapSlots_in_bounds {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile} {bir : BIR lang profile}
    (h : bir.RegisterFormation (language := language))
    {fn locals graph} (hfn : (fn, locals, graph) ∈ bir.functions)
    {blk} (hblk : blk ∈ graph.blocks.toList) {slot : TrapIndex}
    (htrap : blk.ending = .trap slot) :
    slot.index < bir.sites.size := by
  have hnormMem : (fn, locals, (⟨graph.entry, graph.blocks.toList.map fun b =>
      ⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending, b.boundaryOrigins⟩⟩ :
      NormalizedCFG lang)) ∈
      (RegisterIR.bir bir).normalize.functions := by
    unfold RegisterIR.normalize
    dsimp only
    exact List.mem_map.mpr ⟨(fn, locals, graph), hfn, rfl⟩
  have hfnFormed := h.2.2.2.2.2.2.2.2.2.2 _ hnormMem
  obtain ⟨sig, _, _, _, _, hblks⟩ := hfnFormed.2.2.2
  have hblkNorm : (⟨blk.id, blk.params, blk.body,
      RegTerminator.ofBIR bir.sites.size blk.ending, blk.boundaryOrigins⟩ :
      Block _ _ (RegTerminator lang)) ∈
      (graph.blocks.toList.map fun b =>
        ⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending, b.boundaryOrigins⟩) :=
    List.mem_map.mpr ⟨blk, hblk, rfl⟩
  have hend := (hblks _ hblkNorm).2.1
  rw [htrap] at hend
  unfold RegTerminator.ofBIR at hend
  by_cases hlt : slot.index < bir.sites.size
  · exact hlt
  · simp [hlt] at hend

private theorem nodup_of_append_left {α : Type} {xs ys : List α} (h : (xs ++ ys).Nodup) : xs.Nodup := by
  induction xs with
  | nil => exact List.nodup_nil
  | cons a rest ih =>
    rw [List.cons_append, List.nodup_cons] at h
    exact List.nodup_cons.mpr ⟨fun hm => h.1 (List.mem_append_left ys hm), ih h.2⟩

private theorem nodup_of_append_right {α : Type} {xs ys : List α} (h : (xs ++ ys).Nodup) : ys.Nodup := by
  induction xs with
  | nil => exact h
  | cons _ rest ih =>
    rw [List.cons_append, List.nodup_cons] at h
    exact ih h.2

private theorem nodup_flatMap_elem {α β : Type} {xs : List α} {f : α → List β}
    (h : (xs.flatMap f).Nodup) {x : α} (hx : x ∈ xs) : (f x).Nodup := by
  induction xs with
  | nil => cases hx
  | cons a rest ih =>
    rw [List.flatMap_cons] at h
    rcases List.mem_cons.mp hx with rfl | hrest
    · exact nodup_of_append_left h
    · exact ih (nodup_of_append_right h) hrest

/-- Well-formed register IR forbids aliasing the wrapped-result and overflow-flag destinations
of `SSAInst.addWithOverflow`. -/
theorem RegisterFormation.addWithOverflow_dst_ne_flag {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {ir : RegisterIR lang profile} (h : RegisterFormation (language := language) ir)
    {fn locals graph} (hfn : (fn, locals, graph) ∈ ir.normalize.functions)
    {blk} (hblk : blk ∈ graph.blocks)
    {op : LocatedOp (SSAInst lang)} {idx : Nat} (hop : (op, idx) ∈ blk.body.zipIdx)
    {t dst flag lhs rhs} (hinst : op.instruction = .addWithOverflow t dst flag lhs rhs) :
    dst.id ≠ flag.id := by
  have hfnFormed := h.2.2.2.2.2.2.2.2.2.2 _ hfn
  obtain ⟨_, _, _, hdefsNd, _, _⟩ := hfnFormed.2.2.2
  unfold functionDefTable at hdefsNd
  rw [List.map_flatMap] at hdefsNd
  have hblkNd := nodup_flatMap_elem hdefsNd hblk
  rw [List.map_append] at hblkNd
  have hbodyNd := nodup_of_append_right hblkNd
  rw [List.map_flatMap] at hbodyNd
  have hopNd := nodup_flatMap_elem hbodyNd hop
  dsimp only at hopNd
  rw [hinst] at hopNd
  simp only [SSAInst.defs, List.map_cons, List.map_nil, List.nodup_cons, List.mem_singleton] at hopNd
  exact hopNd.1

/-! ## `firstUnsupported`: deterministic backend feature policy scanner over `BIR` -/

private def typeWidthCause {profile : Profile} (language : CoreLanguage) (fn : FnId)
    (pos : FeaturePosition) (ty : language.operands.Ty) : List (FeatureCause profile) :=
  match language.scalarType ty with
  | some (.int intTy) => [.unsupportedWidth intTy fn pos]
  | _ => []

/-- Blocks of a `BIRGraph` reachable from `graph.entry`. -/
def BIR.reachableBlocks {Ty : Type} {lang : LoweringLanguage Ty} (graph : BIRGraph lang) :
    List BlockId :=
  let nodes := graph.blocks.toList.map Block.id
  let edges := graph.blocks.toList.flatMap fun blk =>
    blk.ending.successors.map fun edge => (blk.id, edge.target)
  reachGraph nodes edges none graph.entry

/-- Functions of a `BIR` program reachable from `bir.entry` via calls inside reachable blocks. -/
def BIR.reachableFunctions {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : List FnId :=
  let fnNames := List.map Prod.fst bir.functions
  let callEdges := bir.functions.flatMap fun (fn, _, graph) =>
    let rBlocks := BIR.reachableBlocks graph
    graph.blocks.toList.flatMap fun blk =>
      if rBlocks.contains blk.id then
        blk.body.filterMap fun op =>
          match op.instruction with
          | .call callee _ _ => some (fn, callee)
          | _ => none
      else []
  reachGraph fnNames callEdges none bir.entry

/-- Candidate backend-feature occurrences of one `SSAInst` in fixed per-occurrence precedence:
storage form, then call form, then operation, then integer widths (destination(s) then operands). -/
private def instFeatureCandidates {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (bir : BIR lang profile) (fn : FnId) (entryReachable : Bool) (pos : FeaturePosition)
    (inst : SSAInst lang) : List (FeatureCause profile) :=
  match inst with
  | @SSAInst.op _ _ opcode types ty _ _ _ addresses =>
    let storage :=
      if addresses.isEmpty then [.unsupportedStorage .registerOnly fn pos]
      else [.unsupportedStorage .addressOperand fn pos]
    let op := [.unsupportedOperation (Operator.policyOperation opcode) fn pos]
    let widths :=
      typeWidthCause language fn pos ty ++
      types.flatMap (typeWidthCause language fn pos) ++
      addresses.flatMap (fun p => typeWidthCause language fn pos p.1)
    storage ++ op ++ widths
  | .addWithOverflow t _ _ _ _ =>
    [.unsupportedOperation .addWithOverflow fn pos, .unsupportedWidth t fn pos]
  | .load (ty := ty) _ _ =>
    .unsupportedStorage .load fn pos :: typeWidthCause language fn pos ty
  | .store (ty := ty) _ _ =>
    .unsupportedStorage .store fn pos :: typeWidthCause language fn pos ty
  | .beginLifetime _ =>
    [.unsupportedStorage .beginLifetime fn pos]
  | .endLifetime _ =>
    [.unsupportedStorage .endLifetime fn pos]
  | .destructure (ty := ty) src bindings =>
    let form := match src with
      | .register _ => BackendPolicy.StorageForm.destructureRegister
      | .materialized _ => .destructureMaterialized
    .unsupportedStorage form fn pos ::
      (typeWidthCause language fn pos ty ++
        bindings.flatMap fun (_, ⟨fty, _⟩) => typeWidthCause language fn pos fty)
  | .call callee dst args =>
    let form :=
      if !entryReachable then BackendPolicy.CallForm.unreachableCall
      else if (AList.lookup bir.functions callee).isSome then .reachableInternalCall
      else .foreignCall
    .unsupportedCall form (some callee) fn pos ::
      ((dst.toList.flatMap fun d => typeWidthCause language fn pos d.1) ++
        args.flatMap fun a => typeWidthCause language fn pos a.1)

private def termFeatureCandidates {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (fn : FnId) (pos : FeaturePosition)
    (ending : BIRTerm (Sigma (SSARead language.operands.Ty lang.literal))
      (SSARead language.operands.Ty lang.literal lang.bool)) :
    List (FeatureCause profile) :=
  match ending with
  | .jump edge => edge.arguments.flatMap fun a => typeWidthCause language fn pos a.1
  | .branch _ yes no =>
    (yes.arguments ++ no.arguments).flatMap fun a => typeWidthCause language fn pos a.1
  | .ret (some arg) => typeWidthCause language fn pos arg.1
  | .ret none | .trap _ => []

/-- Every backend-feature candidate in `bir` in deterministic stored order:
for each function in `bir.functions`: signature parameters, signature result, local declarations,
and then for each block in `graph.blocks`: block parameters, body instructions (with fixed
per-occurrence precedence storage → call → operation → integer widths), and terminator. -/
def BIR.featureCandidates {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (bir : BIR lang profile) : List (FeatureCause profile) :=
  let rFns := bir.reachableFunctions
  bir.functions.flatMap fun (fn, locals, graph) =>
    let fnReach := rFns.contains fn
    let rBlocks := BIR.reachableBlocks graph
    let sigCandidates := match AList.lookup bir.signatures fn with
      | none => []
      | some sig =>
        (sig.parameters.zipIdx.flatMap fun (ty, idx) =>
          typeWidthCause language fn (.signatureParam idx) ty) ++
        (sig.result.toList.flatMap fun ty =>
          typeWidthCause language fn .signatureResult ty)
    let localCandidates := locals.flatMap fun (place, ty) =>
      typeWidthCause language fn (.localDecl place) ty
    let blockCandidates := graph.blocks.toList.flatMap fun blk =>
      let entryReachable := fnReach && rBlocks.contains blk.id
      let paramCands := blk.params.zipIdx.flatMap fun (p, idx) =>
        typeWidthCause language fn (.blockParam blk.id idx) p.1
      let bodyCands := blk.body.zipIdx.flatMap fun (op, idx) =>
        instFeatureCandidates (language := language) bir fn entryReachable
          (.instruction blk.id idx) op.instruction
      let termCands := termFeatureCandidates (language := language) fn (.terminator blk.id) blk.ending
      paramCands ++ bodyCands ++ termCands
    sigCandidates ++ localCandidates ++ blockCandidates

/-- Return the first unsupported backend feature in `bir` under `profile.policyProfile`,
scanning functions and blocks in stored order with fixed per-occurrence precedence.
Every returned cause is guaranteed to fail `BackendPolicy.supportsFeature profile.policyProfile`.

Note: `{language : CoreLanguage}` matches `RegisterFormation`, `SSA.WF`, and `BIR.WF`; freeze
unit F7 instantiates `language := sliceLanguage s` whenever `lang = sliceLoweringLanguage s`
(on `At s profile .bir`). -/
def firstUnsupported {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) : Option (FeatureCause profile) :=
  (bir.featureCandidates (language := language)).find? fun cause =>
    !BackendPolicy.supportsFeature profile.policyProfile cause.feature

/-- Project `firstUnsupported bir` to its single owning `RegisteredRule`. -/
def firstUnsupportedRule {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) : Option RegisteredRule :=
  (firstUnsupported (language := language) bir).map FeatureCause.rule

/-- Policy soundness (F4 carry-over): every cause returned by `firstUnsupported` violates
the profile's executable backend policy (`supportsFeature profile.policyProfile cause.feature = false`). -/
theorem firstUnsupported_policy_false {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {cause : FeatureCause profile}
    (h : firstUnsupported (language := language) bir = some cause) :
    BackendPolicy.supportsFeature profile.policyProfile cause.feature = false := by
  obtain ⟨_, _, _, _, hneg⟩ := list_find?_some h
  cases hs : BackendPolicy.supportsFeature profile.policyProfile cause.feature
  · rfl
  · rw [hs] at hneg; cases hneg

theorem firstUnsupported_not_supported {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {cause : FeatureCause profile}
    (h : firstUnsupported (language := language) bir = some cause) :
    ¬ BackendPolicy.FeatureSupported profile.policyProfile cause.feature := by
  intro hsup
  have htrue := (BackendPolicy.supportsFeature_eq_true_iff profile.policyProfile cause.feature).mpr hsup
  rw [firstUnsupported_policy_false h] at htrue
  cases htrue

theorem firstUnsupported_checkFeature_error {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {cause : FeatureCause profile}
    (h : firstUnsupported (language := language) bir = some cause) :
    BackendPolicy.checkFeature profile.policyProfile cause.feature = .error cause.rule.val := by
  rw [BackendPolicy.checkFeature_error_iff]
  exact ⟨firstUnsupported_not_supported h, cause.rule_val⟩

theorem firstUnsupported_rule_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {cause : FeatureCause profile}
    (h : firstUnsupported (language := language) bir = some cause) :
    firstUnsupportedRule (language := language) bir = some cause.rule ∧
      cause.rule.val = cause.feature.ruleId ∧
      cause.rule.val ∈ BackendPolicy.rules := by
  refine ⟨by simp [firstUnsupportedRule, h], cause.rule_val, ?_⟩
  rw [cause.rule_val]
  exact BackendPolicy.featureRule_mem cause.feature

theorem firstUnsupported_none_iff {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (bir : BIR lang profile) :
    firstUnsupported (language := language) bir = none ↔
      ∀ cause ∈ bir.featureCandidates (language := language),
        BackendPolicy.FeatureSupported profile.policyProfile cause.feature := by
  unfold firstUnsupported
  rw [list_find?_none_iff]
  constructor
  · intro hall cause hmem
    have hnot := hall cause hmem
    apply (BackendPolicy.supportsFeature_eq_true_iff profile.policyProfile cause.feature).mp
    cases hs : BackendPolicy.supportsFeature profile.policyProfile cause.feature
    · rw [hs] at hnot; cases hnot
    · rfl
  · intro hall cause hmem
    have htrue := (BackendPolicy.supportsFeature_eq_true_iff profile.policyProfile cause.feature).mpr
      (hall cause hmem)
    simp [htrue]

theorem firstUnsupported_is_earliest {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {cause : FeatureCause profile}
    (h : firstUnsupported (language := language) bir = some cause) :
    ∃ pre post,
      bir.featureCandidates (language := language) = pre ++ cause :: post ∧
      (∀ prev ∈ pre, BackendPolicy.FeatureSupported profile.policyProfile prev.feature) ∧
      ¬ BackendPolicy.FeatureSupported profile.policyProfile cause.feature := by
  obtain ⟨pre, post, hsplit, hpre, _⟩ := list_find?_some h
  refine ⟨pre, post, hsplit, ?_, firstUnsupported_not_supported h⟩
  intro prev hmem
  have hnot := hpre prev hmem
  apply (BackendPolicy.supportsFeature_eq_true_iff profile.policyProfile prev.feature).mp
  cases hs : BackendPolicy.supportsFeature profile.policyProfile prev.feature
  · rw [hs] at hnot; cases hnot
  · rfl

/-! ## `AccountsFor`: intra-IR occurrence accounting with identity and composition laws -/

private def cfgReachableBlocks {P I T : Type} (succs : T → List BlockId)
    (graph : CFG P (LocatedOp I) T) : List BlockId :=
  let nodes := graph.blocks.toList.map Block.id
  let edges := graph.blocks.toList.flatMap fun blk =>
    (succs blk.ending).map fun tgt => (blk.id, tgt)
  reachGraph nodes edges none graph.entry

private def progReachableFunctions {Ty P I T : Type}
    (succs : T → List BlockId) (calleeOf : I → Option FnId)
    (entry : FnId) (functions : AList FnId (LocalTypes Ty × CFG P (LocatedOp I) T)) :
    List FnId :=
  let fnNames := List.map Prod.fst functions
  let callEdges := functions.flatMap fun (fn, _, graph) =>
    let rBlocks := cfgReachableBlocks succs graph
    graph.blocks.toList.flatMap fun blk =>
      if rBlocks.contains blk.id then
        blk.body.filterMap fun op => (calleeOf op.instruction).map (fn, ·)
      else []
  reachGraph fnNames callEdges none entry

private def mirSuccs {Ty : Type} {lang : LoweringLanguage Ty}
    (t : Terminator (Sigma (Read Ty lang.literal)) (Read Ty lang.literal lang.bool)) : List BlockId :=
  (Terminator.successors t).map CFGEdge.target

private def memSuccs {Ty : Type} {lang : LoweringLanguage Ty}
    (t : MemoryTerm lang) : List BlockId :=
  (Terminator.successors t).map CFGEdge.target

private def ssaSuccs {Ty : Type} {lang : LoweringLanguage Ty}
    (t : Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)) :
    List BlockId :=
  (Terminator.successors t).map CFGEdge.target

private def birSuccs {Ty : Type} {lang : LoweringLanguage Ty}
    (t : BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)) :
    List BlockId :=
  (BIRTerm.successors t).map CFGEdge.target

private def mirCallee {Ty : Type} {lang : LoweringLanguage Ty} : MIRInst lang → Option FnId
  | .call callee .. => some callee
  | _ => none

private def memCallee {Ty : Type} {lang : LoweringLanguage Ty} : MemoryInst lang → Option FnId
  | .call callee .. => some callee
  | _ => none

private def ssaCallee {Ty : Type} {lang : LoweringLanguage Ty} : SSAInst lang → Option FnId
  | .call callee .. => some callee
  | _ => none

/-- Entry-reachable functions at any IR point (following only calls inside reachable blocks). -/
def AtLang.reachableFunctions {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} :
    {point : IRPoint} → AtLang lang profile point → List FnId
  | .mir, mir => progReachableFunctions mirSuccs mirCallee mir.entry mir.functions
  | .memory, mem => progReachableFunctions memSuccs memCallee mem.entry mem.functions
  | .ssa, ssa => progReachableFunctions ssaSuccs ssaCallee ssa.entry ssa.functions
  | .bir, bir => progReachableFunctions birSuccs ssaCallee bir.entry bir.functions

/-- Whether `block` is unreachable from `fn`'s entry block in `ir`. -/
def AtLang.blockUnreachable {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} :
    {point : IRPoint} → AtLang lang profile point → FnId → BlockId → Bool
  | .mir, mir, fn, block =>
    match AList.lookup mir.functions fn with
    | none => true
    | some (_, graph) => !(cfgReachableBlocks mirSuccs graph).contains block
  | .memory, mem, fn, block =>
    match AList.lookup mem.functions fn with
    | none => true
    | some (_, graph) => !(cfgReachableBlocks memSuccs graph).contains block
  | .ssa, ssa, fn, block =>
    match AList.lookup ssa.functions fn with
    | none => true
    | some (_, graph) => !(cfgReachableBlocks ssaSuccs graph).contains block
  | .bir, bir, fn, block =>
    match AList.lookup bir.functions fn with
    | none => true
    | some (_, graph) => !(cfgReachableBlocks birSuccs graph).contains block

/-- Whether an `Operator` is unconditionally pure and non-trapping. -/
def Operator.isPure : Operator → Bool
  | .read => true
  | .arithmetic (.wrapping _) => true
  | .arithmetic (.saturating _) => true
  | .arithmetic (.plain _) => false
  | .named _ => false

private def readIsCopyOrLit {Ty : Type} {Lit : Ty → Type} {ty : Ty} : Read Ty Lit ty → Bool
  | .literal _ | .copy _ => true
  | .move _ => false

private def argsCopyOrLit {Ty : Type} {Lit : Ty → Type} :
    {types : List Ty} → Args (Read Ty Lit) types → Bool
  | _, .nil => true
  | _, .cons arg rest => readIsCopyOrLit arg && argsCopyOrLit rest

private def memReadIsCopyOrLit {Ty : Type} {Lit : Ty → Type} {ty : Ty} :
    MemoryRead Ty Lit ty → Bool
  | .literal _ | .copy _ => true
  | .move _ => false

private def memArgsCopyOrLit {Ty : Type} {Lit : Ty → Type} :
    {types : List Ty} → Args (MemoryRead Ty Lit) types → Bool
  | _, .nil => true
  | _, .cons arg rest => memReadIsCopyOrLit arg && memArgsCopyOrLit rest

private def mirInstPure {Ty : Type} {lang : LoweringLanguage Ty} : MIRInst lang → Bool
  | .bind _ src => readIsCopyOrLit src
  | @MIRInst.op _ _ opcode _ _ _ _ args checks addresses =>
    addresses.isEmpty && argsCopyOrLit args &&
      checks.all (fun c => c.disposition != .residual) && Operator.isPure opcode
  | .assign .. | .endLifetime _ | .destructure .. | .call .. => false

private def memInstPure {Ty : Type} {lang : LoweringLanguage Ty} : MemoryInst lang → Bool
  | .bind dst src => dst.storage == .virtual && memReadIsCopyOrLit src
  | @MemoryInst.op _ _ opcode _ _ dst _ args checks addresses =>
    dst.storage == .virtual && addresses.isEmpty && memArgsCopyOrLit args &&
      checks.all (fun c => c.disposition != .residual) && Operator.isPure opcode
  | .addWithOverflow _ dst flag lhs rhs =>
    dst.storage == .virtual && flag.storage == .virtual &&
      memReadIsCopyOrLit lhs && memReadIsCopyOrLit rhs
  | .assign .. | .beginLifetime _ | .endLifetime _ | .destructure .. | .call .. => false

private def ssaInstPure {Ty : Type} {lang : LoweringLanguage Ty} : SSAInst lang → Bool
  | @SSAInst.op _ _ opcode _ _ _ _ _ addresses => addresses.isEmpty && Operator.isPure opcode
  | .addWithOverflow .. => true
  | .load .. | .store .. | .beginLifetime _ | .endLifetime _ | .destructure .. | .call .. => false

private def cfgOccurrencePure {Ty P I T : Type} (instPure : I → Bool)
    (functions : AList FnId (LocalTypes Ty × CFG P (LocatedOp I) T)) (occ : Occurrence) : Bool :=
  match AList.lookup functions occ.fn with
  | none => false
  | some (_, graph) =>
    match graph.blocks.find? (·.id == occ.block) with
    | none => false
    | some blk =>
      match occ.slot with
      | .instruction idx =>
        match blk.body[idx]? with
        | none => false
        | some op => instPure op.instruction
      | .boundary idx => decide (idx < blk.boundaryOrigins.length)
      | .trap => false

/-- Whether `occ` in `ir` is an unconditionally pure, non-trapping value/boundary occurrence
(never a `.trap`, `.call`, memory mutation, or lifetime op). -/
def AtLang.occurrencePure {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} :
    {point : IRPoint} → AtLang lang profile point → Occurrence → Bool
  | .mir, mir, occ => cfgOccurrencePure mirInstPure mir.functions occ
  | .memory, mem, occ => cfgOccurrencePure memInstPure mem.functions occ
  | .ssa, ssa, occ => cfgOccurrencePure ssaInstPure ssa.functions occ
  | .bir, bir, occ => cfgOccurrencePure ssaInstPure bir.functions occ

private def readUsedPlaces {Ty : Type} {Lit : Ty → Type} {ty : Ty} : Read Ty Lit ty → List PlaceId
  | .literal _ => []
  | .copy place | .move place => [place.root]

private def argsUsedPlaces {Ty : Type} {Lit : Ty → Type} :
    {types : List Ty} → Args (Read Ty Lit) types → List PlaceId
  | _, .nil => []
  | _, .cons arg rest => readUsedPlaces arg ++ argsUsedPlaces rest

private def mirInstDefPlaces {Ty : Type} {lang : LoweringLanguage Ty} : MIRInst lang → List PlaceId
  | .bind dst _ | .op dst .. => [dst.root]
  | .assign .. | .endLifetime _ | .destructure .. | .call .. => []

private def mirInstUsedPlaces {Ty : Type} {lang : LoweringLanguage Ty} : MIRInst lang → List PlaceId
  | .bind _ src => readUsedPlaces src
  | .assign dst src => dst.root :: readUsedPlaces src
  | .op _ _ args _ addresses => argsUsedPlaces args ++ addresses.map (fun p => p.2.root)
  | .endLifetime place => [place]
  | .destructure src _ => [src.root]
  | .call _ _ args _ => args.flatMap fun a => readUsedPlaces a.2

private def mirTermUsedPlaces {Ty : Type} {lang : LoweringLanguage Ty} :
    Terminator (Sigma (Read Ty lang.literal)) (Read Ty lang.literal lang.bool) → List PlaceId
  | .jump edge => edge.arguments.flatMap fun a => readUsedPlaces a.2
  | .branch test yes no =>
    readUsedPlaces test ++ (yes.arguments ++ no.arguments).flatMap fun a => readUsedPlaces a.2
  | .ret (some arg) => readUsedPlaces arg.2
  | .ret none | .trap _ _ | .invalid => []

private def memReadUsedPlaces {Ty : Type} {Lit : Ty → Type} {ty : Ty} :
    MemoryRead Ty Lit ty → List PlaceId
  | .literal _ => []
  | .copy place | .move place => [place.logical.root]

private def memArgsUsedPlaces {Ty : Type} {Lit : Ty → Type} :
    {types : List Ty} → Args (MemoryRead Ty Lit) types → List PlaceId
  | _, .nil => []
  | _, .cons arg rest => memReadUsedPlaces arg ++ memArgsUsedPlaces rest

private def memInstDefPlaces {Ty : Type} {lang : LoweringLanguage Ty} : MemoryInst lang → List PlaceId
  | .bind dst _ | .op dst .. => [dst.logical.root]
  | .addWithOverflow _ dst flag _ _ => [dst.logical.root, flag.logical.root]
  | .assign .. | .beginLifetime _ | .endLifetime _ | .destructure .. | .call .. => []

private def memInstUsedPlaces {Ty : Type} {lang : LoweringLanguage Ty} : MemoryInst lang → List PlaceId
  | .bind _ src => memReadUsedPlaces src
  | .assign dst src =>
    (if dst.logical.path.isEmpty then [] else [dst.logical.root]) ++ memReadUsedPlaces src
  | .op _ _ args _ addresses => memArgsUsedPlaces args ++ addresses.map (fun p => p.2.logical.root)
  | .addWithOverflow _ _ _ lhs rhs => memReadUsedPlaces lhs ++ memReadUsedPlaces rhs
  | .beginLifetime _ | .endLifetime _ => []
  | .destructure src _ => [src.logical.root]
  | .call _ _ args _ => args.flatMap fun a => memReadUsedPlaces a.2

private def memTermUsedPlaces {Ty : Type} {lang : LoweringLanguage Ty} :
    MemoryTerm lang → List PlaceId
  | .jump edge => edge.arguments.flatMap fun a => memReadUsedPlaces a.2
  | .branch test yes no =>
    memReadUsedPlaces test ++ (yes.arguments ++ no.arguments).flatMap fun a => memReadUsedPlaces a.2
  | .ret (some arg) => memReadUsedPlaces arg.2
  | .ret none | .trap _ _ | .invalid => []

private def ssaInstDefIds {Ty : Type} {lang : LoweringLanguage Ty} (inst : SSAInst lang) : List Nat :=
  inst.defs.map Prod.fst

private def ssaInstUseIds {Ty : Type} {lang : LoweringLanguage Ty} (inst : SSAInst lang) : List Nat :=
  inst.regUses.map Prod.fst

private def ssaTermUseIds {Ty : Type} {lang : LoweringLanguage Ty}
    (term : Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)) :
    List Nat :=
  (Terminator.regReads term).filterMap fun
    | ⟨_, .value v⟩ => some v.id
    | ⟨_, .literal _⟩ => none

private def birTermUseIds {Ty : Type} {lang : LoweringLanguage Ty}
    (term : BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)) :
    List Nat :=
  (BIRTerm.regReads term).filterMap fun
    | ⟨_, .value v⟩ => some v.id
    | ⟨_, .literal _⟩ => none

private def cfgOccurrenceUnused {Ty P I T Id : Type} [DecidableEq Id]
    (instDefs : I → List Id) (instUses : I → List Id) (termUses : T → List Id)
    (functions : AList FnId (LocalTypes Ty × CFG P (LocatedOp I) T)) (occ : Occurrence) : Bool :=
  match AList.lookup functions occ.fn with
  | none => false
  | some (_, graph) =>
    match graph.blocks.find? (·.id == occ.block) with
    | none => false
    | some blk =>
      match occ.slot with
      | .instruction idx =>
        match blk.body[idx]? with
        | none => false
        | some op =>
          let defs := instDefs op.instruction
          let uses := graph.blocks.toList.flatMap fun b =>
            (b.body.flatMap fun instr => instUses instr.instruction) ++ termUses b.ending
          defs.all fun id => !uses.contains id
      | .boundary idx => decide (idx < blk.boundaryOrigins.length)
      | .trap => false

/-- Whether `occ` in `ir` defines no value/place used anywhere in its enclosing function CFG
(across all blocks, preserving `RegisterFormation` on unreachable blocks as well).
Boundary origins define no values and are unused metadata; `.trap` occurrences are never unused. -/
def AtLang.occurrenceUnused {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} :
    {point : IRPoint} → AtLang lang profile point → Occurrence → Bool
  | .mir, mir, occ => cfgOccurrenceUnused mirInstDefPlaces mirInstUsedPlaces mirTermUsedPlaces mir.functions occ
  | .memory, mem, occ => cfgOccurrenceUnused memInstDefPlaces memInstUsedPlaces memTermUsedPlaces mem.functions occ
  | .ssa, ssa, occ => cfgOccurrenceUnused ssaInstDefIds ssaInstUseIds ssaTermUseIds ssa.functions occ
  | .bir, bir, occ => cfgOccurrenceUnused ssaInstDefIds ssaInstUseIds birTermUseIds bir.functions occ

/-- Look up the `Origin` attached to occurrence `occ` in `ir`. -/
def AtLang.originOf {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} {point : IRPoint}
    (ir : AtLang lang profile point) (occ : Occurrence) : Option Origin :=
  AList.lookup (AtLang.occurrences ir) occ

/-- Project carrying source files and paths for `ir` at any IR insertion point. -/
def AtLang.project {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} :
    {point : IRPoint} → AtLang lang profile point → Project
  | .mir, mir => MIR.project mir
  | .memory, mem => Memory.project mem
  | .ssa, ssa => SSA.project ssa
  | .bir, bir => BIR.project bir

/-- Extract the `TrapEntry` (`kind` and `origin`) of a `Terminator` when it is `.trap`. -/
def Terminator.trapEntry {A C : Type} : Terminator A C → Option TrapEntry
  | .trap kind origin => some ⟨kind, origin⟩
  | _ => none

private def cfgTrapEntries {Ty P I T : Type} (trapEntry : T → Option TrapEntry)
    (functions : AList FnId (LocalTypes Ty × CFG P (LocatedOp I) T)) :
    AList Occurrence TrapEntry :=
  functions.flatMap fun (fn, _, graph) =>
    graph.blocks.toList.flatMap fun blk =>
      match trapEntry blk.ending with
      | some entry => [(⟨fn, blk.id, .trap⟩, entry)]
      | none => []

/-- Enumerate every `.trap` terminator occurrence together with its `TrapEntry` (`kind` and `origin`)
in stored order, mirroring `AtLang.occurrences`. -/
def AtLang.trapEntries {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile} :
    {point : IRPoint} → AtLang lang profile point → AList Occurrence TrapEntry
  | .mir, mir => cfgTrapEntries Terminator.trapEntry mir.functions
  | .memory, mem => cfgTrapEntries Terminator.trapEntry mem.functions
  | .ssa, ssa => cfgTrapEntries Terminator.trapEntry ssa.functions
  | .bir, bir => cfgTrapEntries (fun
      | .trap slot => bir.sites[slot.index]?
      | _ => none) bir.functions

/-- Look up the `TrapEntry` (`kind` and `origin`) attached to occurrence `occ` in `ir`. -/
def AtLang.trapEntryOf {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} (ir : AtLang lang profile point) (occ : Occurrence) : Option TrapEntry :=
  AList.lookup (AtLang.trapEntries ir) occ

/-- Canonical UTF-8 diagnostic bytes rendered for `entry` in `project`, derived from
`TrapEntry.diagnosticLine` (matching `Contracts.trapDiagnostic` byte-for-byte). -/
def TrapEntry.renderedBytes (entry : TrapEntry) (project : Project) : Option (List UInt8) :=
  (entry.diagnosticLine project).map fun line => line.toUTF8.data.toList

/-- Full observable identity of a `.trap` occurrence:
- `kind`: trap kind (`TrapKind`);
- `origin`: operation ID (`origin.operation`), lexical span (`origin.lexical`), and
  persistent evidence key (`origin.evidenceKey`);
- `renderedBytes`: canonical UTF-8 diagnostic bytes rendered in the program's issuing `Project`. -/
structure TrapIdentity where
  kind : TrapKind
  origin : Origin
  renderedBytes : Option (List UInt8)
  deriving DecidableEq, Repr

def TrapIdentity.ofEntry (project : Project) (entry : TrapEntry) : TrapIdentity :=
  ⟨entry.kind, entry.origin, entry.renderedBytes project⟩

/-- Look up the full observable `TrapIdentity` (kind, operation ID, stable key, span, and
rendered diagnostic bytes) attached to `.trap` occurrence `occ` in `ir`. -/
def AtLang.trapIdentityOf {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} (ir : AtLang lang profile point) (occ : Occurrence) : Option TrapIdentity :=
  (AtLang.trapEntryOf ir occ).map (TrapIdentity.ofEntry (AtLang.project ir))

private theorem alist_lookup_flatMap {κ α β : Type} [DecidableEq κ] (xs : List β)
    (f : β → AList κ α) (k : κ) :
    AList.lookup (xs.flatMap f) k =
      xs.findSome? fun x => AList.lookup (f x) k := by
  induction xs with
  | nil => rfl
  | cons a rest ih =>
    rw [List.flatMap_cons, alist_lookup_append, List.findSome?_cons, ih]
    cases AList.lookup (f a) k <;> rfl

private theorem findSome?_map_option {α β γ : Type} (xs : List α) (f : α → Option β) (g : β → γ) :
    xs.findSome? (fun x => (f x).map g) = (xs.findSome? f).map g := by
  induction xs with
  | nil => rfl
  | cons a rest ih =>
    simp only [List.findSome?_cons]
    cases f a with
    | none => exact ih
    | some b => rfl

private theorem alist_lookup_isSome_of_mem_map_fst {κ α : Type} [DecidableEq κ] {l : AList κ α}
    {k : κ} (h : k ∈ List.map Prod.fst l) : ∃ v, AList.lookup l k = some v := by
  induction l with
  | nil => cases h
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [List.map_cons, List.mem_cons] at h
    simp only [AList.lookup]
    split
    · exact ⟨v', rfl⟩
    · rename_i hne
      rcases h with rfl | htl
      · exact absurd rfl hne
      · exact ih htl

private theorem alist_lookup_zipIdx_slot_ne {α : Type} (fn : FnId) (block : BlockId)
    (mkSlot : Nat → OccurrenceSlot) (hne : ∀ n, mkSlot n ≠ .trap)
    (f : α → Origin) (xs : List α) (k : Nat) {occ : Occurrence} (htrap : occ.slot = .trap) :
    AList.lookup ((xs.zipIdx k).map fun p => (Occurrence.mk fn block (mkSlot p.2), f p.1)) occ = none := by
  induction xs generalizing k with
  | nil => rfl
  | cons a rest ih =>
    simp only [List.zipIdx_cons, List.map_cons, AList.lookup]
    split
    · rename_i heq
      have hslot := congrArg Occurrence.slot heq
      dsimp only at hslot
      rw [htrap] at hslot
      exact absurd hslot (hne k)
    · exact ih (k + 1)

private theorem alist_lookup_block_occurrences_trap {P I T : Type} (fn : FnId)
    (blk : Block P (LocatedOp I) T) (trapEntry : T → Option TrapEntry)
    {occ : Occurrence} (htrap : occ.slot = .trap) :
    AList.lookup (blk.occurrences fn fun t => (trapEntry t).map TrapEntry.origin) occ =
      (AList.lookup (match trapEntry blk.ending with
        | some entry => [(⟨fn, blk.id, .trap⟩, entry)]
        | none => []) occ).map TrapEntry.origin := by
  unfold Block.occurrences
  rw [alist_lookup_append, alist_lookup_append]
  have h1 : AList.lookup (blk.body.zipIdx.map fun p => (Occurrence.mk fn blk.id (.instruction p.2), p.1.origin)) occ = none :=
    alist_lookup_zipIdx_slot_ne fn blk.id OccurrenceSlot.instruction (fun _ h => by cases h) LocatedOp.origin blk.body 0 htrap
  have h2 : AList.lookup (blk.boundaryOrigins.zipIdx.map fun p => (Occurrence.mk fn blk.id (.boundary p.2), p.1)) occ = none :=
    alist_lookup_zipIdx_slot_ne fn blk.id OccurrenceSlot.boundary (fun _ h => by cases h) id blk.boundaryOrigins 0 htrap
  rw [h1, h2]
  dsimp only
  cases trapEntry blk.ending with
  | none => rfl
  | some entry =>
    simp only [Option.map_some, AList.lookup]
    split <;> rfl

private theorem alist_lookup_cfg_occurrences_trap {Ty P I T : Type} (trapEntry : T → Option TrapEntry)
    (functions : AList FnId (LocalTypes Ty × CFG P (LocatedOp I) T))
    {occ : Occurrence} (htrap : occ.slot = .trap) :
    AList.lookup (functions.flatMap fun (fn, _, graph) =>
        graph.occurrences fn fun t => (trapEntry t).map TrapEntry.origin) occ =
      (AList.lookup (cfgTrapEntries trapEntry functions) occ).map TrapEntry.origin := by
  unfold cfgTrapEntries CFG.occurrences
  rw [alist_lookup_flatMap, alist_lookup_flatMap, ← findSome?_map_option]
  congr 1
  funext entry
  rcases entry with ⟨fn, locals, graph⟩
  rw [alist_lookup_flatMap, alist_lookup_flatMap, ← findSome?_map_option]
  congr 1
  funext blk
  exact alist_lookup_block_occurrences_trap fn blk trapEntry htrap

private theorem terminator_trapOrigin_eq {A C : Type} :
    (Terminator.trapOrigin : Terminator A C → Option Origin) =
      fun t => (Terminator.trapEntry t).map TrapEntry.origin := by
  funext t
  cases t <;> rfl

private theorem bir_trapEntryOf_map_origin {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (bir : BIR lang profile) {occ : Occurrence} (htrap : occ.slot = .trap) :
    (AList.lookup (cfgTrapEntries (fun
        | .trap slot => bir.sites[slot.index]?
        | _ => none) bir.functions) occ).map TrapEntry.origin =
      AList.lookup (BIR.occurrences bir) occ := by
  have hEq := (alist_lookup_cfg_occurrences_trap
    (fun (t : BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)) =>
      match t with
      | .trap slot => bir.sites[slot.index]?
      | _ => none)
    bir.functions htrap).symm
  refine hEq.trans ?_
  unfold BIR.occurrences
  congr 1
  refine congrArg (List.flatMap · bir.functions) ?_
  funext entry
  rcases entry with ⟨fn, locals, graph⟩
  dsimp only
  congr 1
  funext t
  cases t <;> rfl

/-- On every `.trap` occurrence, `AtLang.trapEntryOf` projects to the exact same `Origin` as
`AtLang.originOf` (unconditionally, without requiring duplicate-free function/block tables). -/
theorem AtLang.trapEntryOf_map_origin {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} :
    {point : IRPoint} → (ir : AtLang lang profile point) →
    {occ : Occurrence} → occ.slot = .trap →
    (AtLang.trapEntryOf ir occ).map TrapEntry.origin = AtLang.originOf ir occ
  | .mir, mir, occ, htrap => by
    change (AList.lookup (cfgTrapEntries Terminator.trapEntry mir.functions) occ).map TrapEntry.origin =
      AList.lookup (MIR.occurrences mir) occ
    unfold MIR.occurrences
    rw [terminator_trapOrigin_eq]
    exact (alist_lookup_cfg_occurrences_trap Terminator.trapEntry mir.functions htrap).symm
  | .memory, mem, occ, htrap => by
    change (AList.lookup (cfgTrapEntries Terminator.trapEntry mem.functions) occ).map TrapEntry.origin =
      AList.lookup (Memory.occurrences mem) occ
    unfold Memory.occurrences
    rw [terminator_trapOrigin_eq]
    exact (alist_lookup_cfg_occurrences_trap Terminator.trapEntry mem.functions htrap).symm
  | .ssa, ssa, occ, htrap => by
    change (AList.lookup (cfgTrapEntries Terminator.trapEntry ssa.functions) occ).map TrapEntry.origin =
      AList.lookup (SSA.occurrences ssa) occ
    unfold SSA.occurrences
    rw [terminator_trapOrigin_eq]
    exact (alist_lookup_cfg_occurrences_trap Terminator.trapEntry ssa.functions htrap).symm
  | .bir, bir, _, htrap => bir_trapEntryOf_map_origin bir htrap

/-- Every `.trap` occurrence listed in `AtLang.occurrences ir` has a defined `TrapEntry` and
`TrapIdentity`. -/
theorem AtLang.trapIdentityOf_some_of_mem {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {point : IRPoint} (ir : AtLang lang profile point)
    {occ : Occurrence} (hmem : occ ∈ (AtLang.occurrences ir).map Prod.fst)
    (htrap : occ.slot = .trap) :
    ∃ entry, AtLang.trapEntryOf ir occ = some entry ∧
      AtLang.trapIdentityOf ir occ = some (TrapIdentity.ofEntry (AtLang.project ir) entry) := by
  obtain ⟨orig, horig⟩ := alist_lookup_isSome_of_mem_map_fst hmem
  have hmap := AtLang.trapEntryOf_map_origin ir htrap
  change (AtLang.trapEntryOf ir occ).map TrapEntry.origin = AList.lookup (AtLang.occurrences ir) occ at hmap
  rw [horig] at hmap
  cases hentry : AtLang.trapEntryOf ir occ with
  | none => rw [hentry] at hmap; cases hmap
  | some entry =>
    exact ⟨entry, rfl, by simp [AtLang.trapIdentityOf, hentry]⟩

/-- Equality of `TrapIdentity` implies exact equality of all five preserved observable trap fields:
trap kind, operation ID, persistent evidence key, lexical span, and rendered UTF-8 diagnostic bytes. -/
theorem TrapIdentity.eq_fields {p₁ p₂ : Project} {e₁ e₂ : TrapEntry}
    (h : TrapIdentity.ofEntry p₁ e₁ = TrapIdentity.ofEntry p₂ e₂) :
    e₁.kind = e₂.kind ∧
    e₁.origin.operation = e₂.origin.operation ∧
    e₁.origin.evidenceKey = e₂.origin.evidenceKey ∧
    e₁.origin.lexical = e₂.origin.lexical ∧
    e₁.renderedBytes p₁ = e₂.renderedBytes p₂ := by
  unfold TrapIdentity.ofEntry at h
  obtain ⟨hk, ho, hb⟩ := TrapIdentity.mk.inj h
  exact ⟨hk, congrArg Origin.operation ho, congrArg Origin.evidenceKey ho,
    congrArg Origin.lexical ho, hb⟩

/-- Decidable check justifying an `EliminationReason` for occurrence `occ` in `ir`. -/
def checkEliminationJustified {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} (ir : AtLang lang profile point) (occ : Occurrence) :
    EliminationReason → Bool
  | .unreachableFunction => !(AtLang.reachableFunctions ir).contains occ.fn
  | .unreachableBlock =>
    !(AtLang.reachableFunctions ir).contains occ.fn ||
      AtLang.blockUnreachable ir occ.fn occ.block
  | .deadValue => AtLang.occurrencePure ir occ && AtLang.occurrenceUnused ir occ

/-- Declarative justification of `reason` for eliminating occurrence `occ` from `ir`.
- `.unreachableFunction`: enclosing function is unreachable from `ir.entry`;
- `.unreachableBlock`: enclosing function or block is unreachable;
- `.deadValue`: occurrence is pure, non-trapping, and unused anywhere in its function CFG. -/
def EliminationJustified {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} (ir : AtLang lang profile point) (occ : Occurrence) :
    EliminationReason → Prop
  | .unreachableFunction => occ.fn ∉ AtLang.reachableFunctions ir
  | .unreachableBlock =>
    occ.fn ∉ AtLang.reachableFunctions ir ∨ AtLang.blockUnreachable ir occ.fn occ.block = true
  | .deadValue => AtLang.occurrencePure ir occ = true ∧ AtLang.occurrenceUnused ir occ = true

theorem checkEliminationJustified_eq_true_iff {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {point : IRPoint} (ir : AtLang lang profile point)
    (occ : Occurrence) (reason : EliminationReason) :
    checkEliminationJustified ir occ reason = true ↔ EliminationJustified ir occ reason := by
  cases reason with
  | unreachableFunction =>
    simp [checkEliminationJustified, EliminationJustified, List.contains_eq_mem]
  | unreachableBlock =>
    simp [checkEliminationJustified, EliminationJustified, List.contains_eq_mem]
  | deadValue =>
    simp [checkEliminationJustified, EliminationJustified]

instance instDecidableEliminationJustified {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {point : IRPoint} (ir : AtLang lang profile point)
    (occ : Occurrence) (reason : EliminationReason) :
    Decidable (EliminationJustified ir occ reason) :=
  decidable_of_bool_iff (checkEliminationJustified ir occ reason)
    (checkEliminationJustified_eq_true_iff ir occ reason)

instance instDecidableEliminationJustifiedAt {slice : Slice} {profile : Profile}
    {point : IRPoint} (ir : At slice profile point)
    (occ : Occurrence) (reason : EliminationReason) :
    Decidable (EliminationJustified ir occ reason) :=
  decidable_of_bool_iff (checkEliminationJustified ir occ reason)
    (checkEliminationJustified_eq_true_iff ir occ reason)

instance instDecidableEliminationJustifiedMIR {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (ir : MIR lang profile) (occ : Occurrence) (reason : EliminationReason) :
    Decidable (EliminationJustified (point := .mir) ir occ reason) :=
  decidable_of_bool_iff (checkEliminationJustified (point := .mir) ir occ reason)
    (checkEliminationJustified_eq_true_iff (point := .mir) ir occ reason)

instance instDecidableEliminationJustifiedMemory {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (ir : Memory lang profile) (occ : Occurrence) (reason : EliminationReason) :
    Decidable (EliminationJustified (point := .memory) ir occ reason) :=
  decidable_of_bool_iff (checkEliminationJustified (point := .memory) ir occ reason)
    (checkEliminationJustified_eq_true_iff (point := .memory) ir occ reason)

instance instDecidableEliminationJustifiedSSA {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (ir : SSA lang profile) (occ : Occurrence) (reason : EliminationReason) :
    Decidable (EliminationJustified (point := .ssa) ir occ reason) :=
  decidable_of_bool_iff (checkEliminationJustified (point := .ssa) ir occ reason)
    (checkEliminationJustified_eq_true_iff (point := .ssa) ir occ reason)

instance instDecidableEliminationJustifiedBIR {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (ir : BIR lang profile) (occ : Occurrence) (reason : EliminationReason) :
    Decidable (EliminationJustified (point := .bir) ir occ reason) :=
  decidable_of_bool_iff (checkEliminationJustified (point := .bir) ir occ reason)
    (checkEliminationJustified_eq_true_iff (point := .bir) ir occ reason)

/-- Generic intra-IR occurrence-accounting contract `AccountsForLang a b account`:
1. `account` is `Valid` between the occurrence address sets of `a` and `b`;
2. Every output occurrence in `b` retains the `Origin` of at least one of its mapped contributors
   (supporting copying and fusion without forging or swapping origins);
3. Trap preservation: a mapped pair `(out, src)` has `out.slot = .trap ↔ src.slot = .trap`
   (traps neither appear from non-traps nor hide inside non-trap occurrences), and every mapped
   `.trap` occurrence preserves its full observable `TrapIdentity` (trap kind, operation ID,
   stable evidence key, lexical span, and rendered diagnostic bytes in its issuing `Project`);
4. Every eliminated source `(src, reason)` in `account.eliminated` satisfies
   `EliminationJustified a src reason`.

Ordinary value/control optimizations that make previously impure or reachable occurrences pure or
unreachable in `b` (such as constant folding, branch folding keeping the dead block, or inlining
keeping the callee for a later dead-code pass) are accepted without a monotonicity restriction. -/
def AccountsForLang {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} (a b : AtLang lang profile point) (account : Account) : Prop :=
  let occsA := (AtLang.occurrences a).map Prod.fst
  let occsB := (AtLang.occurrences b).map Prod.fst
  account.Valid occsA occsB ∧
  (∀ out ∈ occsB, ∃ src, account.MapsTo out src ∧ AtLang.originOf b out = AtLang.originOf a src) ∧
  (∀ out src, account.MapsTo out src →
    (out.slot = .trap ↔ src.slot = .trap) ∧
    (out.slot = .trap → AtLang.trapIdentityOf b out = AtLang.trapIdentityOf a src)) ∧
  (∀ src reason, AList.lookup account.eliminated src = some reason →
    EliminationJustified a src reason)

/-- Frozen occurrence-accounting contract at the repository-selected slice, profile, and IR point. -/
def AccountsFor {slice : Slice} {profile : Profile} {point : IRPoint}
    (a b : At slice profile point) (account : Account) : Prop :=
  AccountsForLang a b account

/-- Executable decision procedure for `AccountsForLang`. -/
def checkAccountsForLang {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} (a b : AtLang lang profile point) (account : Account) : Bool :=
  let occsA := (AtLang.occurrences a).map Prod.fst
  let occsB := (AtLang.occurrences b).map Prod.fst
  (account.mapped.all fun entry => occsB.contains entry.1) &&
  (occsB.all fun out => !(account.sourcesOf out).isEmpty) &&
  (account.mapped.all fun entry => entry.2.all fun src => occsA.contains src) &&
  (account.eliminated.all fun entry => occsA.contains entry.1) &&
  (occsA.all fun src =>
    (account.mapped.any fun entry => entry.2.contains src) ||
    (AList.lookup account.eliminated src).isSome) &&
  (occsB.all fun out =>
    (account.sourcesOf out).any fun src => AtLang.originOf b out == AtLang.originOf a src) &&
  (account.mapped.all fun entry =>
    entry.2.all fun src =>
      ((entry.1.slot == .trap) == (src.slot == .trap)) &&
      (if entry.1.slot == .trap then
        AtLang.trapIdentityOf b entry.1 == AtLang.trapIdentityOf a src
      else true)) &&
  (account.eliminated.all fun entry => checkEliminationJustified a entry.1 entry.2)

def checkAccountsFor {slice : Slice} {profile : Profile} {point : IRPoint}
    (a b : At slice profile point) (account : Account) : Bool :=
  checkAccountsForLang a b account

private theorem bool_beq_iff (x y : Bool) : (x == y) = true ↔ (x = true ↔ y = true) := by
  cases x <;> cases y <;> decide

theorem checkAccountsForLang_eq_true_iff {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {point : IRPoint} (a b : AtLang lang profile point) (account : Account) :
    checkAccountsForLang a b account = true ↔ AccountsForLang a b account := by
  unfold checkAccountsForLang AccountsForLang Account.Valid
  dsimp only
  simp only [Bool.and_eq_true, List.all_eq_true, List.any_eq_true, List.contains_eq_mem,
    decide_eq_true_eq]
  constructor
  · rintro ⟨⟨⟨⟨⟨⟨⟨h1, h2⟩, h3⟩, h4⟩, h5⟩, h6⟩, h7⟩, h8⟩
    refine ⟨⟨?_, ?_, ?_, ?_, ?_⟩, ?_, ?_, ?_⟩
    · intro out hout
      obtain ⟨entry, hentry, rfl⟩ := List.mem_map.mp hout
      exact h1 entry hentry
    · intro out hout
      have hne := h2 out hout
      cases hs : account.sourcesOf out with
      | nil => simp [hs] at hne
      | cons src rest =>
        exact ⟨src, by simp [Account.MapsTo, hs]⟩
    · intro out src hmaps
      obtain ⟨srcs, hmem, hsrc⟩ := (account.mapsTo_iff_mem_mapped out src).mp hmaps
      exact h3 (out, srcs) hmem src hsrc
    · intro src hsrc
      obtain ⟨entry, hentry, rfl⟩ := List.mem_map.mp hsrc
      exact h4 entry hentry
    · intro src hsrc
      rcases Bool.or_eq_true_iff.mp (h5 src hsrc) with hmap | helim
      · exact Or.inl ((account.mapped_iff_any src).mpr hmap)
      · exact Or.inr ((account.eliminated_iff_isSome src).mpr helim)
    · intro out hout
      obtain ⟨src, hsrc, heq⟩ := h6 out hout
      exact ⟨src, hsrc, eq_of_beq heq⟩
    · intro out src hmaps
      obtain ⟨srcs, hmem, hsrc⟩ := (account.mapsTo_iff_mem_mapped out src).mp hmaps
      obtain ⟨hslotEq, hid⟩ := h7 (out, srcs) hmem src hsrc
      refine ⟨?_, ?_⟩
      · have hiff := (bool_beq_iff (out.slot == .trap) (src.slot == .trap)).mp hslotEq
        simpa [beq_iff_eq] using hiff
      · intro htrap
        simp only [htrap, beq_self_eq_true, ↓reduceIte] at hid
        exact eq_of_beq hid
    · intro src reason hlook
      exact (checkEliminationJustified_eq_true_iff a src reason).mp (h8 (src, reason) (alist_lookup_mem hlook))
  · rintro ⟨⟨hv1, hv2, hv3, hv4, hv5⟩, horig, htrap, helim⟩
    refine ⟨⟨⟨⟨⟨⟨⟨?_, ?_⟩, ?_⟩, ?_⟩, ?_⟩, ?_⟩, ?_⟩, ?_⟩
    · intro entry hentry
      exact hv1 entry.1 (List.mem_map.mpr ⟨entry, hentry, rfl⟩)
    · intro out hout
      obtain ⟨src, hsrc⟩ := hv2 out hout
      cases hs : account.sourcesOf out with
      | nil => simp [Account.MapsTo, hs] at hsrc
      | cons _ _ => rfl
    · intro entry hentry src hsrc
      exact hv3 entry.1 src ((account.mapsTo_iff_mem_mapped entry.1 src).mpr ⟨entry.2, hentry, hsrc⟩)
    · intro entry hentry
      exact hv4 entry.1 (List.mem_map.mpr ⟨entry, hentry, rfl⟩)
    · intro src hsrc
      apply Bool.or_eq_true_iff.mpr
      rcases hv5 src hsrc with hmap | hel
      · exact Or.inl ((account.mapped_iff_any src).mp hmap)
      · exact Or.inr ((account.eliminated_iff_isSome src).mp hel)
    · intro out hout
      obtain ⟨src, hsrc, heq⟩ := horig out hout
      exact ⟨src, hsrc, beq_iff_eq.mpr heq⟩
    · intro entry hentry src hsrc
      have hmaps := (account.mapsTo_iff_mem_mapped entry.1 src).mpr ⟨entry.2, hentry, hsrc⟩
      obtain ⟨hslotIff, hid⟩ := htrap entry.1 src hmaps
      refine ⟨?_, ?_⟩
      · apply (bool_beq_iff (entry.1.slot == .trap) (src.slot == .trap)).mpr
        simpa [beq_iff_eq] using hslotIff
      · cases hs : (entry.1.slot == .trap)
        · simp
        · simp only [↓reduceIte]
          exact beq_iff_eq.mpr (hid (eq_of_beq hs))
    · intro entry hentry
      have hlook := alist_lookup_of_mem account.eliminatedKeysNodup hentry
      exact (checkEliminationJustified_eq_true_iff a entry.1 entry.2).mpr (helim entry.1 entry.2 hlook)

theorem checkAccountsFor_eq_true_iff {slice : Slice} {profile : Profile} {point : IRPoint}
    (a b : At slice profile point) (account : Account) :
    checkAccountsFor a b account = true ↔ AccountsFor a b account :=
  checkAccountsForLang_eq_true_iff a b account

instance instDecidableAccountsForLang {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {point : IRPoint} (a b : AtLang lang profile point) (account : Account) :
    Decidable (AccountsForLang a b account) :=
  decidable_of_bool_iff (checkAccountsForLang a b account)
    (checkAccountsForLang_eq_true_iff a b account)

instance instDecidableAccountsForLangMIR {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (a b : MIR lang profile) (account : Account) :
    Decidable (AccountsForLang (point := .mir) a b account) :=
  decidable_of_bool_iff (checkAccountsForLang (point := .mir) a b account)
    (checkAccountsForLang_eq_true_iff (point := .mir) a b account)

instance instDecidableAccountsForLangMemory {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (a b : Memory lang profile) (account : Account) :
    Decidable (AccountsForLang (point := .memory) a b account) :=
  decidable_of_bool_iff (checkAccountsForLang (point := .memory) a b account)
    (checkAccountsForLang_eq_true_iff (point := .memory) a b account)

instance instDecidableAccountsForLangSSA {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (a b : SSA lang profile) (account : Account) :
    Decidable (AccountsForLang (point := .ssa) a b account) :=
  decidable_of_bool_iff (checkAccountsForLang (point := .ssa) a b account)
    (checkAccountsForLang_eq_true_iff (point := .ssa) a b account)

instance instDecidableAccountsForLangBIR {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (a b : BIR lang profile) (account : Account) :
    Decidable (AccountsForLang (point := .bir) a b account) :=
  decidable_of_bool_iff (checkAccountsForLang (point := .bir) a b account)
    (checkAccountsForLang_eq_true_iff (point := .bir) a b account)

instance instDecidableAccountsFor {slice : Slice} {profile : Profile} {point : IRPoint}
    (a b : At slice profile point) (account : Account) :
    Decidable (AccountsFor a b account) :=
  decidable_of_bool_iff (checkAccountsFor a b account)
    (checkAccountsFor_eq_true_iff a b account)

instance instDecidableAccountsForMIR {slice : Slice} {profile : Profile}
    (a b : MIR (sliceLoweringLanguage slice) profile) (account : Account) :
    Decidable (AccountsFor (point := .mir) a b account) :=
  decidable_of_bool_iff (checkAccountsFor (point := .mir) a b account)
    (checkAccountsFor_eq_true_iff (point := .mir) a b account)

instance instDecidableAccountsForMemory {slice : Slice} {profile : Profile}
    (a b : Memory (sliceLoweringLanguage slice) profile) (account : Account) :
    Decidable (AccountsFor (point := .memory) a b account) :=
  decidable_of_bool_iff (checkAccountsFor (point := .memory) a b account)
    (checkAccountsFor_eq_true_iff (point := .memory) a b account)

instance instDecidableAccountsForSSA {slice : Slice} {profile : Profile}
    (a b : SSA (sliceLoweringLanguage slice) profile) (account : Account) :
    Decidable (AccountsFor (point := .ssa) a b account) :=
  decidable_of_bool_iff (checkAccountsFor (point := .ssa) a b account)
    (checkAccountsFor_eq_true_iff (point := .ssa) a b account)

instance instDecidableAccountsForBIR {slice : Slice} {profile : Profile}
    (a b : BIR (sliceLoweringLanguage slice) profile) (account : Account) :
    Decidable (AccountsFor (point := .bir) a b account) :=
  decidable_of_bool_iff (checkAccountsFor (point := .bir) a b account)
    (checkAccountsFor_eq_true_iff (point := .bir) a b account)

/-- Projection lemma: under `AccountsForLang a b account`, every output `.trap` occurrence `out`
in `b` maps solely from `.trap` occurrences `src` in `a` preserving all five observable trap
fields: trap kind, operation ID, persistent evidence key, lexical span, and rendered diagnostic
bytes. -/
theorem AccountsForLang.trap_preserves_fields {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {point : IRPoint} {a b : AtLang lang profile point} {account : Account}
    (h : AccountsForLang a b account) {out src : Occurrence}
    (hout : out ∈ (AtLang.occurrences b).map Prod.fst) (htrap : out.slot = .trap)
    (hmaps : account.MapsTo out src) :
    src.slot = .trap ∧
    ∃ entryA entryB,
      AtLang.trapEntryOf a src = some entryA ∧
      AtLang.trapEntryOf b out = some entryB ∧
      entryB.kind = entryA.kind ∧
      entryB.origin.operation = entryA.origin.operation ∧
      entryB.origin.evidenceKey = entryA.origin.evidenceKey ∧
      entryB.origin.lexical = entryA.origin.lexical ∧
      entryB.renderedBytes (AtLang.project b) = entryA.renderedBytes (AtLang.project a) := by
  obtain ⟨hval, _, htrapClause, _⟩ := h
  obtain ⟨hslotIff, hidEq⟩ := htrapClause out src hmaps
  have hsrcTrap : src.slot = .trap := hslotIff.mp htrap
  have hsrcMem : src ∈ (AtLang.occurrences a).map Prod.fst := hval.2.2.1 out src hmaps
  obtain ⟨entryA, heA, hidA⟩ := AtLang.trapIdentityOf_some_of_mem a hsrcMem hsrcTrap
  obtain ⟨entryB, heB, hidB⟩ := AtLang.trapIdentityOf_some_of_mem b hout htrap
  have heq := hidEq htrap
  rw [hidB, hidA] at heq
  have hfields := TrapIdentity.eq_fields (Option.some.inj heq)
  exact ⟨hsrcTrap, entryA, entryB, heA, heB, hfields⟩

/-! ### Identity, composition, and per-image justification laws for `AccountsFor` -/

/-- Identity law: `Account.identityFor a` unconditionally accounts for `a → a`. -/
theorem AccountsForLang.identity {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} (a : AtLang lang profile point) :
    AccountsForLang a a (Account.identityFor a) := by
  unfold AccountsForLang Account.identityFor
  refine ⟨Account.identity_valid _, ?_, ?_, ?_⟩
  · intro out hout
    exact ⟨out, (Account.mapsTo_identity_iff _ out out).mpr ⟨hout, rfl⟩, rfl⟩
  · intro out src hmaps
    obtain ⟨_, rfl⟩ := (Account.mapsTo_identity_iff _ out src).mp hmaps
    exact ⟨Iff.rfl, fun _ => rfl⟩
  · intro _ _ hlook
    cases hlook

theorem AccountsFor.identity {slice : Slice} {profile : Profile} {point : IRPoint}
    (a : At slice profile point) :
    AccountsFor a a (Account.identityFor a) :=
  AccountsForLang.identity a

private theorem compose_eliminated_lookup_cases (first second : Account)
    {src : Occurrence} {reason : EliminationReason}
    (hlook : AList.lookup (Account.compose first second).eliminated src = some reason) :
    AList.lookup first.eliminated src = some reason ∨
      (¬ (Account.compose first second).Mapped src ∧
        AList.lookup first.eliminated src = none ∧
        first.Mapped src ∧
        (∃ occB, first.MapsTo occB src ∧ AList.lookup second.eliminated occB = some reason) ∧
        (∀ occB, first.MapsTo occB src → second.Eliminated occB)) := by
  have hElimComp : (Account.compose first second).Eliminated src := ⟨reason, hlook⟩
  obtain ⟨hNotMapped, hFirstOrSecond⟩ := (Account.eliminated_compose_iff first second src).mp hElimComp
  have hmem := alist_lookup_mem hlook
  unfold Account.compose at hmem
  dsimp only at hmem
  rcases List.mem_append.mp hmem with hfirst | hsecond
  · left
    exact alist_lookup_of_mem first.eliminatedKeysNodup (List.mem_filter.mp hfirst).1
  · right
    obtain ⟨x, _, hmap⟩ := List.mem_filterMap.mp hsecond
    cases hcond : ((Account.compose first second).mapped.any fun e => e.2.contains x) ||
        (AList.lookup (first.eliminated.filter fun entry =>
          !((Account.compose first second).mapped.any fun e => e.2.contains entry.1)) x).isSome
    · change (if ((Account.compose first second).mapped.any fun e => e.2.contains x) ||
          (AList.lookup (first.eliminated.filter fun entry =>
            !((Account.compose first second).mapped.any fun e => e.2.contains entry.1)) x).isSome
        then none
        else match (first.imagesOf x).mapM (AList.lookup second.eliminated) with
          | some (reason :: _) => some (x, reason)
          | _ => none) = some (src, reason) at hmap
      rw [hcond] at hmap
      simp only [Bool.false_eq_true, ↓reduceIte] at hmap
      cases hm : (first.imagesOf x).mapM (AList.lookup second.eliminated) with
      | none => simp [hm] at hmap
      | some rs =>
        cases rs with
        | nil => simp [hm] at hmap
        | cons r0 rs' =>
          simp only [hm, Option.some.injEq, Prod.mk.injEq] at hmap
          obtain ⟨rfl, rfl⟩ := hmap
          have hcondBoth := Bool.or_eq_false_iff.mp hcond
          have hLookFilt : AList.lookup (first.eliminated.filter fun entry =>
              !((Account.compose first second).mapped.any fun e => e.2.contains entry.1)) x =
              if !((Account.compose first second).mapped.any fun e => e.2.contains x) then
                AList.lookup first.eliminated x
              else none :=
            alist_lookup_filter_key
              (fun k => !((Account.compose first second).mapped.any fun e => e.2.contains k))
              first.eliminated x
          have hFirstNone : AList.lookup first.eliminated x = none := by
            have hIsSomeFalse := hcondBoth.2
            rw [hLookFilt, hcondBoth.1] at hIsSomeFalse
            simp only [Bool.not_false, ↓reduceIte] at hIsSomeFalse
            cases hl : AList.lookup first.eliminated x with
            | none => rfl
            | some r => rw [hl] at hIsSomeFalse; cases hIsSomeFalse
          have hSecondAll : first.Mapped x ∧ ∀ occB, first.MapsTo occB x → second.Eliminated occB := by
            rcases hFirstOrSecond with ⟨r1, hr1⟩ | hS
            · rw [hFirstNone] at hr1; cases hr1
            · exact hS
          cases himgs : first.imagesOf x with
          | nil =>
            rw [himgs] at hm
            simp [List.mapM_nil, pure] at hm
          | cons occB rest =>
            rw [himgs, List.mapM_cons] at hm
            cases hl2 : AList.lookup second.eliminated occB with
            | none => simp [hl2, bind, Option.bind] at hm
            | some rB =>
              simp only [hl2, bind, Option.bind] at hm
              cases hrest : rest.mapM (AList.lookup second.eliminated) with
              | none => simp [hrest] at hm
              | some _ =>
                simp only [hrest, pure, Option.some.injEq, List.cons.injEq] at hm
                obtain ⟨rfl, _⟩ := hm
                have hmemImg : occB ∈ first.imagesOf x := by rw [himgs]; exact List.mem_cons_self
                exact ⟨hNotMapped, hFirstNone, hSecondAll.1,
                  ⟨occB, (first.mem_imagesOf_iff x occB).mp hmemImg, hl2⟩, hSecondAll.2⟩
    · change (if ((Account.compose first second).mapped.any fun e => e.2.contains x) ||
          (AList.lookup (first.eliminated.filter fun entry =>
            !((Account.compose first second).mapped.any fun e => e.2.contains entry.1)) x).isSome
        then none
        else match (first.imagesOf x).mapM (AList.lookup second.eliminated) with
          | some (reason :: _) => some (x, reason)
          | _ => none) = some (src, reason) at hmap
      rw [hcond] at hmap
      simp at hmap

/-- Explicit premise for collapsing two sequential occurrence accounts `first` (`a → b`) and
`second` (`b → c`) into a direct `AccountsForLang a c (Account.compose first second)`:
every elimination `(srcA, reason)` recorded in `(Account.compose first second).eliminated`
is justified in the source `a`.

Eliminations inherited from `first.eliminated` are automatically justified in `a` by
`AccountsForLang a b first`. This premise is therefore needed only for sources that survived
`first` into intermediate occurrences in `b` and were eliminated by `second` (when `first` did
not enable a new syntactic elimination in `b`). When `first` enables a new elimination in `b`
(such as branch folding making a trap block unreachable before dead-block elimination removes it),
sequential composition remains sound through the intermediate IR `b` via
`AccountsForLang.compose_through_intermediate`. -/
def ComposedEliminationsJustified {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} (a : AtLang lang profile point) (first second : Account) : Prop :=
  ∀ srcA reason,
    AList.lookup (Account.compose first second).eliminated srcA = some reason →
    EliminationJustified a srcA reason

/-- Executable decision procedure for `ComposedEliminationsJustified`. -/
def checkComposedEliminationsJustified {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {point : IRPoint} (a : AtLang lang profile point)
    (first second : Account) : Bool :=
  (Account.compose first second).eliminated.all fun entry =>
    checkEliminationJustified a entry.1 entry.2

theorem checkComposedEliminationsJustified_eq_true_iff {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {point : IRPoint} (a : AtLang lang profile point)
    (first second : Account) :
    checkComposedEliminationsJustified a first second = true ↔
      ComposedEliminationsJustified a first second := by
  unfold checkComposedEliminationsJustified ComposedEliminationsJustified
  rw [List.all_eq_true]
  constructor
  · intro hall srcA reason hlook
    exact (checkEliminationJustified_eq_true_iff a srcA reason).mp
      (hall (srcA, reason) (alist_lookup_mem hlook))
  · intro h entry hentry
    have hlook := alist_lookup_of_mem (Account.compose first second).eliminatedKeysNodup hentry
    exact (checkEliminationJustified_eq_true_iff a entry.1 entry.2).mpr (h entry.1 entry.2 hlook)

instance instDecidableComposedEliminationsJustified {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {point : IRPoint} (a : AtLang lang profile point)
    (first second : Account) : Decidable (ComposedEliminationsJustified a first second) :=
  decidable_of_bool_iff (checkComposedEliminationsJustified a first second)
    (checkComposedEliminationsJustified_eq_true_iff a first second)

instance instDecidableComposedEliminationsJustifiedAt {slice : Slice} {profile : Profile}
    {point : IRPoint} (a : At slice profile point)
    (first second : Account) : Decidable (ComposedEliminationsJustified a first second) :=
  decidable_of_bool_iff (checkComposedEliminationsJustified a first second)
    (checkComposedEliminationsJustified_eq_true_iff a first second)

instance instDecidableComposedEliminationsJustifiedMIR {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (a : MIR lang profile) (first second : Account) :
    Decidable (ComposedEliminationsJustified (point := .mir) a first second) :=
  decidable_of_bool_iff (checkComposedEliminationsJustified (point := .mir) a first second)
    (checkComposedEliminationsJustified_eq_true_iff (point := .mir) a first second)

instance instDecidableComposedEliminationsJustifiedMemory {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (a : Memory lang profile) (first second : Account) :
    Decidable (ComposedEliminationsJustified (point := .memory) a first second) :=
  decidable_of_bool_iff (checkComposedEliminationsJustified (point := .memory) a first second)
    (checkComposedEliminationsJustified_eq_true_iff (point := .memory) a first second)

instance instDecidableComposedEliminationsJustifiedSSA {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (a : SSA lang profile) (first second : Account) :
    Decidable (ComposedEliminationsJustified (point := .ssa) a first second) :=
  decidable_of_bool_iff (checkComposedEliminationsJustified (point := .ssa) a first second)
    (checkComposedEliminationsJustified_eq_true_iff (point := .ssa) a first second)

instance instDecidableComposedEliminationsJustifiedBIR {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} (a : BIR lang profile) (first second : Account) :
    Decidable (ComposedEliminationsJustified (point := .bir) a first second) :=
  decidable_of_bool_iff (checkComposedEliminationsJustified (point := .bir) a first second)
    (checkComposedEliminationsJustified_eq_true_iff (point := .bir) a first second)

/-- Unconditional intermediate-occurrence composition soundness: given `AccountsForLang a b first`
and `AccountsForLang b c second`, `Account.compose first second` is always valid between `a` and
`c`, every output occurrence in `c` traces through an intermediate occurrence `midB` in `b`
preserving `Origin`, every mapped pair preserves `.trap` status and full `TrapIdentity` through
`midB`, and every eliminated source `srcA` in the composed ledger is either justified directly in
`a` (eliminated by `first`) or mapped by `first` with EVERY intermediate image `occB` in `b`
eliminated by `second` and justified in `b`. -/
theorem AccountsForLang.compose_through_intermediate
    {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} {a b c : AtLang lang profile point} {first second : Account}
    (h1 : AccountsForLang a b first) (h2 : AccountsForLang b c second) :
    let occsA := (AtLang.occurrences a).map Prod.fst
    let occsB := (AtLang.occurrences b).map Prod.fst
    let occsC := (AtLang.occurrences c).map Prod.fst
    let comp := Account.compose first second
    comp.Valid occsA occsC ∧
    (∀ outC ∈ occsC, ∃ midB ∈ occsB, ∃ srcA ∈ occsA,
      second.MapsTo outC midB ∧ first.MapsTo midB srcA ∧ comp.MapsTo outC srcA ∧
      AtLang.originOf c outC = AtLang.originOf b midB ∧
      AtLang.originOf b midB = AtLang.originOf a srcA) ∧
    (∀ outC srcA, comp.MapsTo outC srcA →
      ∃ midB ∈ occsB, second.MapsTo outC midB ∧ first.MapsTo midB srcA ∧
        (outC.slot = .trap ↔ midB.slot = .trap) ∧
        (midB.slot = .trap ↔ srcA.slot = .trap) ∧
        (outC.slot = .trap →
          AtLang.trapIdentityOf c outC = AtLang.trapIdentityOf b midB ∧
          AtLang.trapIdentityOf b midB = AtLang.trapIdentityOf a srcA)) ∧
    (∀ srcA reason, AList.lookup comp.eliminated srcA = some reason →
      (AList.lookup first.eliminated srcA = some reason ∧ EliminationJustified a srcA reason) ∨
      (¬ comp.Mapped srcA ∧ first.Mapped srcA ∧
        (∃ occB ∈ occsB, first.MapsTo occB srcA ∧
          AList.lookup second.eliminated occB = some reason ∧
          EliminationJustified b occB reason) ∧
        (∀ occB, first.MapsTo occB srcA →
          ∃ reasonB, AList.lookup second.eliminated occB = some reasonB ∧
            EliminationJustified b occB reasonB))) := by
  obtain ⟨hv1, horig1, htrap1, helim1⟩ := h1
  obtain ⟨hv2, horig2, htrap2, helim2⟩ := h2
  refine ⟨Account.compose_valid hv1 hv2, ?_, ?_, ?_⟩
  · intro outC houtC
    obtain ⟨midB, hmaps2, heq2⟩ := horig2 outC houtC
    have hmidInB := hv2.2.2.1 outC midB hmaps2
    obtain ⟨srcA, hmaps1, heq1⟩ := horig1 midB hmidInB
    have hsrcInA := hv1.2.2.1 midB srcA hmaps1
    exact ⟨midB, hmidInB, srcA, hsrcInA, hmaps2, hmaps1,
      (Account.mapsTo_compose_iff first second outC srcA).mpr ⟨midB, hmaps2, hmaps1⟩,
      heq2, heq1⟩
  · intro outC srcA hmapsC
    obtain ⟨midB, hmaps2, hmaps1⟩ := (Account.mapsTo_compose_iff first second outC srcA).mp hmapsC
    have hmidInB := hv2.2.2.1 outC midB hmaps2
    obtain ⟨hslot2, hid2⟩ := htrap2 outC midB hmaps2
    obtain ⟨hslot1, hid1⟩ := htrap1 midB srcA hmaps1
    refine ⟨midB, hmidInB, hmaps2, hmaps1, hslot2, hslot1, fun htrapC => ?_⟩
    exact ⟨hid2 htrapC, hid1 (hslot2.mp htrapC)⟩
  · intro srcA reason hlook
    rcases compose_eliminated_lookup_cases first second hlook with
      hfirst | ⟨hNotMapped, _, hMapped1, ⟨occB, hmaps1, hlook2⟩, hAllElim⟩
    · exact Or.inl ⟨hfirst, helim1 srcA reason hfirst⟩
    · refine Or.inr ⟨hNotMapped, hMapped1, ?_, ?_⟩
      · obtain ⟨srcs, hmemM, _⟩ := (first.mapsTo_iff_mem_mapped occB srcA).mp hmaps1
        have hoccBInB := hv1.1 occB (List.mem_map.mpr ⟨(occB, srcs), hmemM, rfl⟩)
        exact ⟨occB, hoccBInB, hmaps1, hlook2, helim2 occB reason hlook2⟩
      · intro occB' hmaps1'
        obtain ⟨reasonB, hlook2'⟩ := hAllElim occB' hmaps1'
        exact ⟨reasonB, hlook2', helim2 occB' reasonB hlook2'⟩

/-- Exact characterization of direct composition: given `AccountsForLang a b first` and
`AccountsForLang b c second`, `AccountsForLang a c (Account.compose first second)` holds
if and only if every composed elimination is justified in the source `a`. -/
theorem AccountsForLang.compose_iff {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} {a b c : AtLang lang profile point} {first second : Account}
    (h1 : AccountsForLang a b first) (h2 : AccountsForLang b c second) :
    AccountsForLang a c (Account.compose first second) ↔
      ComposedEliminationsJustified a first second := by
  constructor
  · intro hcomp
    exact hcomp.2.2.2
  · intro hElim
    obtain ⟨hv, horig, htrap, _⟩ := AccountsForLang.compose_through_intermediate h1 h2
    refine ⟨hv, ?_, ?_, hElim⟩
    · intro outC houtC
      obtain ⟨_, _, srcA, _, _, _, hmapsC, heq2, heq1⟩ := horig outC houtC
      exact ⟨srcA, hmapsC, heq2.trans heq1⟩
    · intro outC srcA hmapsC
      obtain ⟨_, _, _, _, hslot2, hslot1, hid⟩ := htrap outC srcA hmapsC
      refine ⟨hslot2.trans hslot1, fun htrapC => ?_⟩
      obtain ⟨h2eq, h1eq⟩ := hid htrapC
      exact h2eq.trans h1eq

/-- Composition law: if `first` accounts for `a → b`, `second` accounts for `b → c`, and
each composed elimination is justified in `a` (`ComposedEliminationsJustified a first second`),
then `Account.compose first second` accounts for `a → c`. -/
theorem AccountsForLang.compose {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} {a b c : AtLang lang profile point} {first second : Account}
    (h1 : AccountsForLang a b first) (h2 : AccountsForLang b c second)
    (hElim : ComposedEliminationsJustified a first second) :
    AccountsForLang a c (Account.compose first second) :=
  (AccountsForLang.compose_iff h1 h2).mpr hElim

theorem AccountsFor.compose {slice : Slice} {profile : Profile} {point : IRPoint}
    {a b c : At slice profile point} {first second : Account}
    (h1 : AccountsFor a b first) (h2 : AccountsFor b c second)
    (hElim : ComposedEliminationsJustified a first second) :
    AccountsFor a c (Account.compose first second) :=
  AccountsForLang.compose h1 h2 hElim

/-- Intermediate-pullback composition corollary: if every source occurrence `srcA` eliminated only
at the second stage (not surviving into `c`, not eliminated by `first`, and mapped by `first` to
an intermediate occurrence `occB` eliminated by `second` with `reason`) pulls its elimination
justification back from `b` to `a`, then `Account.compose first second` accounts for `a → c`. -/
theorem AccountsForLang.compose_of_second_pullback
    {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} {a b c : AtLang lang profile point} {first second : Account}
    (h1 : AccountsForLang a b first) (h2 : AccountsForLang b c second)
    (hPullback : ∀ srcA occB reason,
      ¬ (Account.compose first second).Mapped srcA →
      AList.lookup first.eliminated srcA = none →
      first.MapsTo occB srcA →
      AList.lookup second.eliminated occB = some reason →
      EliminationJustified b occB reason →
      EliminationJustified a srcA reason) :
    AccountsForLang a c (Account.compose first second) := by
  refine AccountsForLang.compose h1 h2 fun srcA reason hlook => ?_
  rcases compose_eliminated_lookup_cases first second hlook with
    hfirst | ⟨hNotMapped, hFirstNone, _, ⟨occB, hmaps1, hlook2⟩, _⟩
  · exact h1.2.2.2 srcA reason hfirst
  · exact hPullback srcA occB reason hNotMapped hFirstNone hmaps1 hlook2
      (h2.2.2.2 occB reason hlook2)

theorem AccountsFor.compose_of_second_pullback {slice : Slice} {profile : Profile} {point : IRPoint}
    {a b c : At slice profile point} {first second : Account}
    (h1 : AccountsFor a b first) (h2 : AccountsFor b c second)
    (hPullback : ∀ srcA occB reason,
      ¬ (Account.compose first second).Mapped srcA →
      AList.lookup first.eliminated srcA = none →
      first.MapsTo occB srcA →
      AList.lookup second.eliminated occB = some reason →
      EliminationJustified b occB reason →
      EliminationJustified a srcA reason) :
    AccountsFor a c (Account.compose first second) :=
  AccountsForLang.compose_of_second_pullback h1 h2 hPullback

/-- Per-image elimination soundness (F4 carry-over): although `Account.compose` retains only
the first image's `EliminationReason` in its composed `eliminated` ledger, `AccountsFor`
verifies the elimination justification in the intermediate IR `b` for EVERY image `occB` of
`srcA`. -/
theorem AccountsForLang.compose_all_images_justified {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {point : IRPoint} {a b c : AtLang lang profile point}
    {first second : Account}
    (h1 : AccountsForLang a b first) (h2 : AccountsForLang b c second)
    {srcA occB : Occurrence} {reasonB : EliminationReason}
    (himg : first.MapsTo occB srcA)
    (helim : AList.lookup second.eliminated occB = some reasonB) :
    occB ∈ (AtLang.occurrences b).map Prod.fst ∧ EliminationJustified b occB reasonB := by
  obtain ⟨srcs, hmem, _⟩ := (first.mapsTo_iff_mem_mapped occB srcA).mp himg
  exact ⟨h1.1.1 occB (List.mem_map.mpr ⟨(occB, srcs), hmem, rfl⟩), h2.2.2.2 occB reasonB helim⟩

/-- Congruence under exact `MapsTo` and `eliminated`-lookup equality. -/
theorem AccountsForLang.congr {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {point : IRPoint} {a b : AtLang lang profile point} {w₁ w₂ : Account}
    (hmaps : ∀ out src, w₁.MapsTo out src ↔ w₂.MapsTo out src)
    (hkeysM : ∀ out, out ∈ List.map Prod.fst w₁.mapped ↔ out ∈ List.map Prod.fst w₂.mapped)
    (helim : ∀ src, AList.lookup w₁.eliminated src = AList.lookup w₂.eliminated src) :
    AccountsForLang a b w₁ ↔ AccountsForLang a b w₂ := by
  have hkeysE : ∀ src, src ∈ List.map Prod.fst w₁.eliminated ↔ src ∈ List.map Prod.fst w₂.eliminated := by
    intro src
    constructor
    · intro hmem
      obtain ⟨⟨k, r⟩, hkr, rfl⟩ := List.mem_map.mp hmem
      have hl1 := alist_lookup_of_mem w₁.eliminatedKeysNodup hkr
      rw [helim] at hl1
      exact List.mem_map.mpr ⟨(k, r), alist_lookup_mem hl1, rfl⟩
    · intro hmem
      obtain ⟨⟨k, r⟩, hkr, rfl⟩ := List.mem_map.mp hmem
      have hl2 := alist_lookup_of_mem w₂.eliminatedKeysNodup hkr
      rw [← helim] at hl2
      exact List.mem_map.mpr ⟨(k, r), alist_lookup_mem hl2, rfl⟩
  have hmapped : ∀ src, w₁.Mapped src ↔ w₂.Mapped src := fun src =>
    ⟨fun ⟨o, h⟩ => ⟨o, (hmaps o src).mp h⟩, fun ⟨o, h⟩ => ⟨o, (hmaps o src).mpr h⟩⟩
  have helimP : ∀ src, w₁.Eliminated src ↔ w₂.Eliminated src := fun src => by
    unfold Account.Eliminated; rw [helim]
  constructor
  · rintro ⟨⟨v1, v2, v3, v4, v5⟩, ho, ht, he⟩
    exact ⟨⟨fun o h => v1 o ((hkeysM o).mpr h),
      fun o h => let ⟨s, hs⟩ := v2 o h; ⟨s, (hmaps o s).mp hs⟩,
      fun o s h => v3 o s ((hmaps o s).mpr h),
      fun s h => v4 s ((hkeysE s).mpr h),
      fun s h => (v5 s h).imp (hmapped s).mp (helimP s).mp⟩,
      fun o h => let ⟨s, hs, eq⟩ := ho o h; ⟨s, (hmaps o s).mp hs, eq⟩,
      fun o s hs => ht o s ((hmaps o s).mpr hs),
      fun s r hl => he s r ((helim s).trans hl)⟩
  · rintro ⟨⟨v1, v2, v3, v4, v5⟩, ho, ht, he⟩
    exact ⟨⟨fun o h => v1 o ((hkeysM o).mp h),
      fun o h => let ⟨s, hs⟩ := v2 o h; ⟨s, (hmaps o s).mpr hs⟩,
      fun o s h => v3 o s ((hmaps o s).mp h),
      fun s h => v4 s ((hkeysE s).mp h),
      fun s h => (v5 s h).imp (hmapped s).mpr (helimP s).mpr⟩,
      fun o h => let ⟨s, hs, eq⟩ := ho o h; ⟨s, (hmaps o s).mpr hs, eq⟩,
      fun o s hs => ht o s ((hmaps o s).mp hs),
      fun s r hl => he s r ((helim s).symm.trans hl)⟩

/-! ## Controls for `Account.mkChecked`, `RegisterFormation`, `firstUnsupported`, and `AccountsFor` -/

section Controls

private def ctrlProj : Project := Project.singleton mainSourcePath (SourceText.ofBytes [52, 50])
private def ctrlSpan : FileSpan := ⟨⟨0⟩, 0, 1, 1, 1⟩
private def ctrlMain : FnId :=
  ⟨⟨⟨⟨"pkg"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"main"⟩⟩
private def ctrlHelper : FnId := { ctrlMain with name := ⟨"helper"⟩ }
private def ctrlExtern : FnId := { ctrlMain with name := ⟨"extern_fn"⟩ }
private def ctrlOrigin (site : Nat) : Origin :=
  ⟨⟨site⟩, ctrlSpan, ⟨.function ctrlMain, ⟨[]⟩⟩⟩

private def ctrlLCB : Profile := ⟨.lcb, .x86_64Linux, 64, 4096⟩
private def ctrlLLVM : Profile := ⟨.llvm, .x86_64Linux, 64, 4096⟩

private abbrev s0Lang := sliceLoweringLanguage slice0
private abbrev s0Core := sliceLanguage slice0
private abbrev u8Ty : EarlyTy := s0Lang.intTy .u8
private abbrev u32Ty : EarlyTy := s0Lang.intTy .u32
private abbrev boolTy : EarlyTy := s0Lang.bool

private def ctrlOcc (fn : FnId) (blk : Nat) (slot : OccurrenceSlot) : Occurrence :=
  ⟨fn, ⟨blk⟩, slot⟩

-- Control 1: `Account.mkChecked` accepts valid runtime inputs and rejects every invariant violation.
example :
    let o1 := ctrlOcc ctrlMain 0 (.instruction 0)
    let o2 := ctrlOcc ctrlMain 0 (.instruction 1)
    let o3 := ctrlOcc ctrlMain 0 (.instruction 2)
    (Account.mkChecked [(o2, [o1])] [(o3, .deadValue)]).isSome = true ∧
    Account.mkChecked [(o2, [o1]), (o2, [o3])] [] = none ∧
    Account.mkChecked [(o2, [o1, o1])] [] = none ∧
    Account.mkChecked [] [(o1, .deadValue), (o1, .unreachableBlock)] = none ∧
    Account.mkChecked [(o2, [o1])] [(o1, .deadValue)] = none := by
  decide

-- Well-formed checked-add BIR program parameterized by integer width (`u8` or `u32`).
private def checkedAddBIR (profile : Profile) (t : IntTy) (litBytes : List UInt8) :
    BIR s0Lang profile :=
  let ty := s0Lang.intTy t
  let sumV : SSAValue EarlyTy ty := ⟨0⟩
  let flgV : SSAValue EarlyTy boolTy := ⟨1⟩
  let lit : SSARead EarlyTy s0Lang.literal ty := .literal ⟨litBytes⟩
  let b0 : Block _ _ _ :=
    ⟨⟨0⟩, [],
      [⟨ctrlOrigin 0, .addWithOverflow t sumV flgV lit lit⟩],
      .branch (.value flgV) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
  let b1 : Block _ _ _ := ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩
  let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .ret (some ⟨ty, .value sumV⟩), []⟩
  ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some ty⟩)],
    [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [], #[⟨.overflow, ctrlOrigin 0⟩], [], []⟩

-- Control 2: `RegisterFormation` accepts valid `u8`/`u32` checked-add, zero-slot self-loop,
-- and parameterized loop, and rejects aliased `dst`/`flag`, sibling non-dominance, bad literal,
-- bad opcode signature, out-of-bounds trap index, and unreachable-block local defects.
example :
    let goodU8 := checkedAddBIR ctrlLCB .u8 [20]
    let goodU32 := checkedAddBIR ctrlLCB .u32 [20, 0, 0, 0]
    let zeroLoop : BIR s0Lang ctrlLCB :=
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[⟨⟨0⟩, [], [], .jump ⟨⟨0⟩, []⟩, []⟩]⟩)], [], #[], [], []⟩
    let paramLoop : BIR s0Lang ctrlLCB :=
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .jump ⟨⟨1⟩, [⟨u8Ty, .literal ⟨[1]⟩⟩]⟩, []⟩
      let b1 : Block _ _ _ :=
        ⟨⟨1⟩, [⟨u8Ty, ⟨0⟩⟩],
          [⟨ctrlOrigin 0, .addWithOverflow .u8 ⟨1⟩ ⟨2⟩ (.value ⟨0⟩) (.literal ⟨[1]⟩)⟩],
          .branch (.value ⟨2⟩) ⟨⟨1⟩, [⟨u8Ty, .value ⟨1⟩⟩]⟩ ⟨⟨2⟩, []⟩, []⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [], #[], [], []⟩
    let aliased : BIR s0Lang ctrlLCB :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨ctrlOrigin 0, .addWithOverflow .u8 ⟨0⟩ ⟨0⟩ (.literal ⟨[1]⟩) (.literal ⟨[1]⟩)⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let siblingNonDom : BIR s0Lang ctrlLCB :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨ctrlOrigin 0, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[1]⟩)⟩],
          .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
      let b1 : Block _ _ _ :=
        ⟨⟨1⟩, [],
          [⟨ctrlOrigin 1, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨2⟩ ()
            (.cons (.literal ⟨[7]⟩) .nil)⟩],
          .ret (some ⟨u8Ty, .value ⟨2⟩⟩), []⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨2⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [], #[], [], []⟩
    let badLit := checkedAddBIR ctrlLCB .u32 [20]
    let badTrapIdx : BIR s0Lang ctrlLCB :=
      { goodU8 with sites := #[] }
    let badUnreachable : BIR s0Lang ctrlLCB :=
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
      let dead : Block _ _ _ :=
        ⟨⟨1⟩, [],
          [⟨ctrlOrigin 1, .op (opcode := .named ⟨"bogus"⟩) (types := []) (ty := u8Ty) ⟨0⟩ () .nil⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, dead]⟩)], [], #[], [], []⟩
    goodU8.RegisterFormation (language := s0Core) ∧
    goodU32.RegisterFormation (language := s0Core) ∧
    zeroLoop.RegisterFormation (language := s0Core) ∧
    paramLoop.RegisterFormation (language := s0Core) ∧
    ¬ aliased.RegisterFormation (language := s0Core) ∧
    ¬ siblingNonDom.RegisterFormation (language := s0Core) ∧
    ¬ badLit.RegisterFormation (language := s0Core) ∧
    ¬ badTrapIdx.RegisterFormation (language := s0Core) ∧
    ¬ badUnreachable.RegisterFormation (language := s0Core) := by
  decide

-- Control 3: `firstUnsupported` accepts `u8` on both backends, accepts `u32` on LLVM, refuses `u32`
-- on LCB with `IntWidthSupported`, allows unreachable calls, refuses reachable internal/foreign
-- calls, and enforces operation-before-width precedence (`div` on `u32` cites `OperationSupported`).
example :
    let u8LCB := checkedAddBIR ctrlLCB .u8 [20]
    let u32LLVM := checkedAddBIR ctrlLLVM .u32 [20, 0, 0, 0]
    let u32LCB := checkedAddBIR ctrlLCB .u32 [20, 0, 0, 0]
    let unreachCall : BIR s0Lang ctrlLCB :=
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
      let dead : Block _ _ _ :=
        ⟨⟨1⟩, [], [⟨ctrlOrigin 1, .call ctrlHelper (some ⟨u8Ty, ⟨0⟩⟩) []⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      let h0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[1]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, dead]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)], [], #[], [], []⟩
    let reachCall : BIR s0Lang ctrlLCB :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [], [⟨ctrlOrigin 1, .call ctrlHelper (some ⟨u8Ty, ⟨0⟩⟩) []⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      let h0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[1]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)], [], #[], [], []⟩
    let reachForeign : BIR s0Lang ctrlLCB :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [], [⟨ctrlOrigin 1, .call ctrlExtern (some ⟨u8Ty, ⟨0⟩⟩) []⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlExtern, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let divU32 : BIR s0Lang ctrlLCB :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨ctrlOrigin 0, .op (opcode := .arithmetic (.plain .div)) (types := [u32Ty, u32Ty])
            (ty := u32Ty) ⟨0⟩ () (.cons (.literal ⟨[4, 0, 0, 0]⟩) (.cons (.literal ⟨[2, 0, 0, 0]⟩) .nil))⟩],
          .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    firstUnsupported (language := s0Core) u8LCB = none ∧
    firstUnsupported (language := s0Core) u32LLVM = none ∧
    firstUnsupported (language := s0Core) u32LCB =
      some (.unsupportedWidth .u32 ctrlMain .signatureResult) ∧
    (firstUnsupportedRule (language := s0Core) u32LCB).map Subtype.val =
      some BackendPolicy.intWidthRule ∧
    firstUnsupported (language := s0Core) unreachCall = none ∧
    firstUnsupported (language := s0Core) reachCall =
      some (.unsupportedCall .reachableInternalCall (some ctrlHelper) ctrlMain (.instruction ⟨0⟩ 0)) ∧
    firstUnsupported (language := s0Core) reachForeign =
      some (.unsupportedCall .foreignCall (some ctrlExtern) ctrlMain (.instruction ⟨0⟩ 0)) ∧
    firstUnsupported (language := s0Core) divU32 =
      some (.unsupportedOperation (.arithmetic (.plain .div)) ctrlMain (.instruction ⟨0⟩ 0)) := by
  decide

-- Control 4: `AccountsFor` accepts identity, valid block renaming, and valid composition;
-- rejects swapped trap origins, lost shared-origin occurrences, false elimination reasons,
-- and F4 carry-over second-image false elimination reasons hidden by `composeEliminated`.
example :
    let orig1 := ctrlOrigin 1
    let orig2 := ctrlOrigin 2
    let birA : At slice0 ctrlLCB .bir :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)⟩],
          .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
      let b1 : Block _ _ _ := ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .trap ⟨1⟩, []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [],
        #[⟨.overflow, orig1⟩, ⟨.overflow, orig2⟩], [], []⟩
    let birSwappedTraps : At slice0 ctrlLCB .bir :=
      { birA with sites := #[⟨.overflow, orig2⟩, ⟨.overflow, orig1⟩] }
    let birDupCopy : At slice0 ctrlLCB .bir :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)⟩,
           ⟨orig1, .addWithOverflow .u8 ⟨2⟩ ⟨3⟩ (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)⟩],
          .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let birSingle : At slice0 ctrlLCB .bir :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)⟩],
          .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let birEmpty : At slice0 ctrlLCB .bir :=
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let i0 := ctrlOcc ctrlMain 0 (.instruction 0)
    let i1 := ctrlOcc ctrlMain 0 (.instruction 1)
    let t1 := ctrlOcc ctrlMain 1 .trap
    let t2 := ctrlOcc ctrlMain 2 .trap
    let dropSecondWithoutElim : Account := { mapped := [(i0, [i0])], eliminated := [] }
    let badTrapElim : Account :=
      { mapped := [(i0, [i0]), (t2, [t2])], eliminated := [(t1, .deadValue)] }
    let copyAcc : Account := { mapped := [(i0, [i0]), (i1, [i0])], eliminated := [] }
    let secondBadImg : Account :=
      { mapped := [], eliminated := [(i0, .deadValue), (i1, .unreachableBlock)] }
    let secondGoodImg : Account :=
      { mapped := [], eliminated := [(i0, .deadValue), (i1, .deadValue)] }
    AccountsFor birA birA (Account.identityFor birA) ∧
    ¬ AccountsFor birA birSwappedTraps (Account.identityFor birA) ∧
    ¬ AccountsFor birDupCopy birSingle dropSecondWithoutElim ∧
    ¬ AccountsFor birA birA badTrapElim ∧
    AccountsFor birSingle birDupCopy copyAcc ∧
    ¬ AccountsFor birDupCopy birEmpty secondBadImg ∧
    AccountsFor birDupCopy birEmpty secondGoodImg ∧
    AccountsFor birSingle birEmpty (Account.compose copyAcc secondGoodImg) := by
  decide

-- Control 5: SSA formation (valid SSA, `.invalid` terminator in unreachable block,
-- same-block use-before-def in unreachable block, diamond merge non-dominance),
-- internal `u32` width check in a `u8`-returning function, storage refusal (`.load`),
-- and `AccountsFor` nonidentity block renaming + uncalled vs called helper elimination.
example :
    let orig1 := ctrlOrigin 1
    let goodSSA : SSA s0Lang ctrlLCB :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[20]⟩) (.literal ⟨[22]⟩)⟩],
          .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
      let b1 : Block _ _ _ := ⟨⟨1⟩, [], [], .trap .overflow orig1, []⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [], [], []⟩
    let ssaInvalidDead : SSA s0Lang ctrlLCB :=
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
      let dead : Block _ _ _ := ⟨⟨1⟩, [], [], .invalid, []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, dead]⟩)], [], [], []⟩
    let ssaDeadUseBeforeDef : SSA s0Lang ctrlLCB :=
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
      let dead : Block _ _ _ :=
        ⟨⟨1⟩, [],
          [⟨orig1, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨1⟩ () (.cons (.value ⟨0⟩) .nil)⟩,
           ⟨orig1, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨0⟩ () (.cons (.literal ⟨[1]⟩) .nil)⟩],
          .ret (some ⟨u8Ty, .value ⟨1⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, dead]⟩)], [], [], []⟩
    let diamondNonDom : BIR s0Lang ctrlLCB :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[1]⟩)⟩],
          .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
      let b1 : Block _ _ _ :=
        ⟨⟨1⟩, [],
          [⟨orig1, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨2⟩ () (.cons (.literal ⟨[7]⟩) .nil)⟩],
          .jump ⟨⟨3⟩, []⟩, []⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .jump ⟨⟨3⟩, []⟩, []⟩
      let b3 : Block _ _ _ := ⟨⟨3⟩, [], [], .ret (some ⟨u8Ty, .value ⟨2⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2, b3]⟩)], [], #[], [], []⟩
    let bodyU32InU8 : BIR s0Lang ctrlLCB :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig1, .addWithOverflow .u32 ⟨0⟩ ⟨1⟩ (.literal ⟨[1, 0, 0, 0]⟩) (.literal ⟨[2, 0, 0, 0]⟩)⟩],
          .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let loadBIR : BIR s0Lang ctrlLCB :=
      let p0 : PlaceId := ⟨0⟩
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [], [⟨orig1, .load (ty := u8Ty) ⟨0⟩ ⟨p0, []⟩⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [(p0, u8Ty)], ⟨⟨0⟩, #[b0]⟩)],
        [(ctrlMain, [(p0, Layout.scalar 1 1)])], #[], [], []⟩
    let birBlock0 : At slice0 ctrlLCB .bir :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let birBlock10 : At slice0 ctrlLCB .bir :=
      let b10 : Block _ _ _ :=
        ⟨⟨10⟩, [],
          [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨10⟩, #[b10]⟩)], [], #[], [], []⟩
    let renameAcc : Account :=
      { mapped := [(ctrlOcc ctrlMain 10 (.instruction 0), [ctrlOcc ctrlMain 0 (.instruction 0)])],
        eliminated := [] }
    let withUncalled : At slice0 ctrlLCB .bir :=
      let m0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
      let h0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[m0]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)], [], #[], [], []⟩
    let withCalled : At slice0 ctrlLCB .bir :=
      let m0 : Block _ _ _ :=
        ⟨⟨0⟩, [], [⟨orig1, .call ctrlHelper (some ⟨u8Ty, ⟨0⟩⟩) []⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      let h0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[m0]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)], [], #[], [], []⟩
    let mainOnly : At slice0 ctrlLCB .bir :=
      let m0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[42]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[m0]⟩)], [], #[], [], []⟩
    let dropUncalledAcc : Account :=
      { mapped := [], eliminated := [(ctrlOcc ctrlHelper 0 (.instruction 0), .unreachableFunction)] }
    let dropCalledAcc : Account :=
      { mapped := [],
        eliminated := [(ctrlOcc ctrlMain 0 (.instruction 0), .unreachableFunction),
          (ctrlOcc ctrlHelper 0 (.instruction 0), .unreachableFunction)] }
    goodSSA.RegisterFormation (language := s0Core) ∧
    ¬ ssaInvalidDead.RegisterFormation (language := s0Core) ∧
    ¬ ssaDeadUseBeforeDef.RegisterFormation (language := s0Core) ∧
    ¬ diamondNonDom.RegisterFormation (language := s0Core) ∧
    bodyU32InU8.RegisterFormation (language := s0Core) ∧
    firstUnsupported (language := s0Core) bodyU32InU8 =
      some (.unsupportedWidth .u32 ctrlMain (.instruction ⟨0⟩ 0)) ∧
    loadBIR.RegisterFormation (language := s0Core) ∧
    firstUnsupported (language := s0Core) loadBIR =
      some (.unsupportedStorage .load ctrlMain (.instruction ⟨0⟩ 0)) ∧
    AccountsFor birBlock0 birBlock10 renameAcc ∧
    AccountsFor withUncalled mainOnly dropUncalledAcc ∧
    ¬ AccountsFor withCalled mainOnly dropCalledAcc := by
  decide

-- Control 6 (F6b round 2):
-- - Trap preservation rejects kind changes (BIR and SSA), rendered-byte changes (altered project
--   file path with identical `TrapKind` and `Origin`), and disguising a `.trap` as a `.boundary`;
-- - Non-monotone optimizer passes are accepted: constant folding (`plain add 1 2 → read 3`),
--   branch folding keeping the now-dead trap block (`brA → brFolded`), dead-block elimination
--   (`brFolded → brDeleted`), and inlining keeping the callee for a later dead-code pass;
-- - Direct collapse of `brA → brFolded → brDeleted` fails `ComposedEliminationsJustified` while
--   sequential composition through `brFolded` is sound via `compose_through_intermediate`;
-- - `.deadValue` rejects eliminating a used pure instruction and accepts an unused pure one.
private def ctrlOtherPath : SourcePath :=
  ⟨SourceText.ofBytes [97, 46, 99, 111, 110], by decide⟩

private def ctrlOtherProj : Project :=
  Project.singleton ctrlOtherPath (SourceText.ofBytes [52, 50])

example :
    let orig1 := ctrlOrigin 1
    let orig2 := ctrlOrigin 2
    let mkBranch (proj : Project) (k : TrapKind)
        (term0 : BIRTerm (Sigma (SSARead EarlyTy s0Lang.literal)) (SSARead EarlyTy s0Lang.literal s0Lang.bool)) :
        At slice0 ctrlLCB .bir :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [], [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)⟩], term0, []⟩
      let b1 : Block _ _ _ := ⟨⟨1⟩, [], [], .trap ⟨0⟩, []⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨proj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)], [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [],
        #[⟨k, orig1⟩], [], []⟩
    let brA := mkBranch ctrlProj .overflow (.branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩)
    let brKind := mkBranch ctrlProj .divisionByZero (.branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩)
    let brOtherPath := mkBranch ctrlOtherProj .overflow (.branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩)
    let brFolded := mkBranch ctrlProj .overflow (.jump ⟨⟨2⟩, []⟩)
    let brDeleted : At slice0 ctrlLCB .bir :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [], [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)⟩],
          .jump ⟨⟨2⟩, []⟩, []⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)], [(ctrlMain, [], ⟨⟨0⟩, #[b0, b2]⟩)], [],
        #[⟨.overflow, orig1⟩], [], []⟩
    let brDisguisedTrap : At slice0 ctrlLCB .bir :=
      let b0 : Block _ _ _ :=
        ⟨⟨0⟩, [], [⟨orig1, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[1]⟩) (.literal ⟨[2]⟩)⟩],
          .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, []⟩, []⟩
      let b1 : Block _ _ _ := ⟨⟨1⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[0]⟩⟩), [orig1]⟩
      let b2 : Block _ _ _ := ⟨⟨2⟩, [], [], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)], [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)], [],
        #[], [], []⟩
    let mkSSATrap (k : TrapKind) : At slice0 ctrlLCB .ssa :=
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .trap k orig1, []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)], [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], [], []⟩
    let ssaOverflow := mkSSATrap .overflow
    let ssaDivZero := mkSSATrap .divisionByZero
    let mkOp (useAdd : Bool) : At slice0 ctrlLCB .bir :=
      let inst : SSAInst s0Lang :=
        if useAdd then
          .op (opcode := .arithmetic (.plain .add)) (types := [u8Ty, u8Ty]) (ty := u8Ty) ⟨0⟩ ()
            (.cons (.literal ⟨[1]⟩) (.cons (.literal ⟨[2]⟩) .nil))
        else
          .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨0⟩ () (.cons (.literal ⟨[3]⟩) .nil)
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [⟨orig1, inst⟩], .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)], [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let unusedRead : At slice0 ctrlLCB .bir :=
      let inst : SSAInst s0Lang :=
        .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨0⟩ () (.cons (.literal ⟨[3]⟩) .nil)
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [⟨orig1, inst⟩], .ret (some ⟨u8Ty, .literal ⟨[3]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)], [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let emptyRet3 : At slice0 ctrlLCB .bir :=
      let b0 : Block _ _ _ := ⟨⟨0⟩, [], [], .ret (some ⟨u8Ty, .literal ⟨[3]⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)], [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], [], #[], [], []⟩
    let calledProg : At slice0 ctrlLCB .bir :=
      let m0 : Block _ _ _ :=
        ⟨⟨0⟩, [], [⟨orig1, .call ctrlHelper (some ⟨u8Ty, ⟨0⟩⟩) []⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      let h0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig2, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨0⟩ () (.cons (.literal ⟨[7]⟩) .nil)⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[m0]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)], [], #[], [], []⟩
    let inlinedKeepCallee : At slice0 ctrlLCB .bir :=
      let m0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig2, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨0⟩ () (.cons (.literal ⟨[7]⟩) .nil)⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      let h0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig2, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨0⟩ () (.cons (.literal ⟨[7]⟩) .nil)⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩), (ctrlHelper, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[m0]⟩), (ctrlHelper, [], ⟨⟨0⟩, #[h0]⟩)], [], #[], [], []⟩
    let inlinedMainOnly : At slice0 ctrlLCB .bir :=
      let m0 : Block _ _ _ :=
        ⟨⟨0⟩, [],
          [⟨orig2, .op (opcode := .read) (types := [u8Ty]) (ty := u8Ty) ⟨0⟩ () (.cons (.literal ⟨[7]⟩) .nil)⟩],
          .ret (some ⟨u8Ty, .value ⟨0⟩⟩), []⟩
      ⟨ctrlProj, ctrlMain, [(ctrlMain, ⟨[], some u8Ty⟩)],
        [(ctrlMain, [], ⟨⟨0⟩, #[m0]⟩)], [], #[], [], []⟩
    let i0 := ctrlOcc ctrlMain 0 (.instruction 0)
    let t1 := ctrlOcc ctrlMain 1 .trap
    let b1Bound := ctrlOcc ctrlMain 1 (.boundary 0)
    let h0i0 := ctrlOcc ctrlHelper 0 (.instruction 0)
    let disguiseAcc : Account := { mapped := [(i0, [i0]), (b1Bound, [t1])], eliminated := [] }
    let dceTrapAcc : Account := { mapped := [(i0, [i0])], eliminated := [(t1, .unreachableBlock)] }
    let dropI0Acc : Account := { mapped := [], eliminated := [(i0, .deadValue)] }
    let inlineAcc : Account :=
      { mapped := [(i0, [i0, h0i0]), (h0i0, [h0i0])], eliminated := [] }
    let dceCalleeAcc : Account :=
      { mapped := [(i0, [i0])], eliminated := [(h0i0, .unreachableFunction)] }
    ¬ AccountsFor brA brKind (Account.identityFor brA) ∧
    ¬ AccountsFor brA brOtherPath (Account.identityFor brA) ∧
    ¬ AccountsFor ssaOverflow ssaDivZero (Account.identityFor ssaOverflow) ∧
    ¬ AccountsFor brA brDisguisedTrap disguiseAcc ∧
    AccountsFor (mkOp true) (mkOp false) (Account.identityFor (mkOp true)) ∧
    AccountsFor brA brFolded (Account.identityFor brA) ∧
    brFolded.RegisterFormation (language := s0Core) ∧
    AccountsFor brFolded brDeleted dceTrapAcc ∧
    ¬ ComposedEliminationsJustified brA (Account.identityFor brA) dceTrapAcc ∧
    ¬ AccountsFor brA brDeleted (Account.compose (Account.identityFor brA) dceTrapAcc) ∧
    AccountsFor calledProg inlinedKeepCallee inlineAcc ∧
    AccountsFor inlinedKeepCallee inlinedMainOnly dceCalleeAcc ∧
    ¬ AccountsFor (mkOp false) emptyRet3 dropI0Acc ∧
    AccountsFor unusedRead emptyRet3 dropI0Acc ∧
    ¬ EliminationJustified brFolded i0 .deadValue := by
  decide

end Controls

end Concrete.Interfaces
