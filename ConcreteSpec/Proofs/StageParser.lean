import ConcreteSpec.Spec.GrammarData
import ConcreteSpec.Spec.GrammarVocab
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.LL1
import ConcreteSpec.Spec.Rules
import ConcreteSpec.StageGrammarCore
import ConcreteSpec.StageInterface

/-! Generic proofs for the certified LL(1) parser machine (`ConcreteSpec/StageGrammarCore.lean`):
1. Linear work totality (`parserEngine_nonExhausted`) and exact step budget (`parserEngine_steps_le`)
   for any `SharedGrammar` + `CheckedTable` whose nonterminal ranks are bounded by `maxRank`.
2. Soundness and completeness of `ParserEngine.run` with respect to `Derives`, `Predictive`,
   `ParsesWith`, and `Refuses`, plus AST uniqueness (`derives_unique`).
3. Source-order first-violation theorems for both malformed refusals (earliest non-extendable
   syntax failure and viable prefix under productivity) and unsupported refusals (write-once
   first out-of-slice witness in pre-order alongside whole-stream least admitting slice). -/

namespace ConcreteSpec.StageGrammar

open ConcreteSpec.StageInterface
open ConcreteSpec.Core (RegisteredRule)
open ConcreteSpec.Core.SourcePathSpan (LocationRefusal)
open ConcreteSpec.GrammarData (ProductionId)
open Concrete.LL1

/-! ## Section 1: Basic slice, grammar cost, and coordinate helpers -/

private theorem slice_eq_of_val_eq {s₁ s₂ : Slice} (h : s₁.val = s₂.val) : s₁ = s₂ := by
  cases s₁ with
  | mk n₁ =>
    cases s₂ with
    | mk n₂ =>
      exact congrArg ConcreteSpec.Core.SliceId.mk (Fin.ext h)

private theorem slice_le_last (s : Slice) : s.val ≤ Slice.val ⟨Fin.last 21⟩ := by
  exact Nat.le_of_lt_succ s.n.isLt

private theorem action_cost_pos {In Out : Type} (a : Action In Out) : 1 ≤ a.cost := by
  induction a with
  | constant _ | input => exact Nat.le_refl 1
  | left _ ih | right _ ih | some _ ih | inl _ ih | inr _ ih | node _ _ ih | leaf _ ih =>
    simp only [Action.cost]
    omega
  | pair _ _ ihl ihr | cons _ _ ihl ihr =>
    simp only [Action.cost]
    omega

private theorem production_frameCost_ge {N : Type} {value : N → Type} (p : Production value) :
    p.rhs.length + 3 ≤ p.frameCost := by
  unfold Production.frameCost
  have hpos := action_cost_pos p.action
  omega

private theorem foldl_max_ge {α : Type 1} (f : α → Nat) (xs : List α) (acc : Nat) :
    acc ≤ xs.foldl (fun a x => max a (f x)) acc := by
  induction xs generalizing acc with
  | nil => exact Nat.le_refl acc
  | cons x xs ih =>
    simp only [List.foldl_cons]
    exact Nat.le_trans (Nat.le_max_left acc (f x)) (ih (max acc (f x)))

private theorem foldl_max_mem {α : Type 1} (f : α → Nat) (xs : List α) (acc : Nat)
    {x : α} (hx : x ∈ xs) :
    f x ≤ xs.foldl (fun a y => max a (f y)) acc := by
  induction xs generalizing acc with
  | nil => cases hx
  | cons y ys ih =>
    simp only [List.foldl_cons]
    rcases List.mem_cons.mp hx with rfl | hmem
    · exact Nat.le_trans (Nat.le_max_right acc (f x)) (foldl_max_ge f ys _)
    · exact ih (max acc (f y)) hmem

private theorem le_maxFrameCost {count : Nat} {g : SharedGrammar count}
    {p : Production g.value} (hp : p ∈ g.productions.toList) :
    p.frameCost ≤ g.maxFrameCost := by
  unfold SharedGrammar.maxFrameCost
  rw [← Array.foldl_toList]
  exact foldl_max_mem Production.frameCost g.productions.toList 1 hp

private theorem one_le_maxFrameCost {count : Nat} (g : SharedGrammar count) :
    1 ≤ g.maxFrameCost := by
  unfold SharedGrammar.maxFrameCost
  rw [← Array.foldl_toList]
  exact foldl_max_ge Production.frameCost g.productions.toList 1

private theorem le_maxNonterminals {count : Nat} {g : SharedGrammar count}
    {p : Production g.value} (hp : p ∈ g.productions.toList) :
    p.nonterminalCount ≤ g.maxNonterminals := by
  unfold SharedGrammar.maxNonterminals
  rw [← Array.foldl_toList]
  exact foldl_max_mem Production.nonterminalCount g.productions.toList 0 hp

private def rhsNtCount {count : Nat} (rhs : List (Sym Terminal (Fin count))) : Nat :=
  (rhs.filter fun | .n _ => true | .t _ => false).length

private theorem rhsNtCount_cons_t {count : Nat} (k : Terminal) (rest : List (Sym Terminal (Fin count))) :
    rhsNtCount (.t k :: rest) = rhsNtCount rest := rfl

private theorem rhsNtCount_cons_n {count : Nat} (n : Fin count) (rest : List (Sym Terminal (Fin count))) :
    rhsNtCount (.n n :: rest) = rhsNtCount rest + 1 := rfl

private theorem zeroTokenStepBound_le_succ (frameCost maxNt : Nat) :
    ∀ r : Nat, zeroTokenStepBound frameCost maxNt r ≤ zeroTokenStepBound frameCost maxNt (r + 1)
  | 0 => by
    simp only [zeroTokenStepBound]
    exact Nat.le_add_right _ _
  | r + 1 => by
    simp only [zeroTokenStepBound]
    exact Nat.add_le_add_left (Nat.mul_le_mul_left maxNt (zeroTokenStepBound_le_succ frameCost maxNt r)) frameCost

private theorem zeroTokenStepBound_mono (frameCost maxNt : Nat) {r₁ r₂ : Nat} (h : r₁ ≤ r₂) :
    zeroTokenStepBound frameCost maxNt r₁ ≤ zeroTokenStepBound frameCost maxNt r₂ := by
  induction h with
  | refl => exact Nat.le_refl _
  | step _ ih => exact Nat.le_trans ih (zeroTokenStepBound_le_succ frameCost maxNt _)

private theorem zeroTokenStepBound_le_unit (frameCost maxNt maxRank : Nat) :
    zeroTokenStepBound frameCost maxNt maxRank ≤
      frameCost + maxNt * zeroTokenStepBound frameCost maxNt maxRank := by
  cases maxRank with
  | zero =>
    simp only [zeroTokenStepBound]
    exact Nat.le_add_right _ _
  | succ r =>
    simp only [zeroTokenStepBound]
    exact Nat.add_le_add_left
      (Nat.mul_le_mul_left maxNt (zeroTokenStepBound_le_succ frameCost maxNt r)) frameCost

theorem parserView_entryFile_isSome {input : ParserView} (hwf : input.WF) :
    (input.source.project.fileAt input.source.entryFile).isSome = true := by
  unfold ParserView.WF ParserView.invalidLocation ParserView.validateCoordinates at hwf
  cases hfile : input.source.project.entries[input.source.entryFile.index]? with
  | some file =>
    simp only [ConcreteSpec.Core.SourcePathSpan.Project.fileAt, hfile, Option.isSome]
  | none =>
    simp only [ConcreteSpec.Core.SourcePathSpan.Project.files, List.getElem?_toArray,
      List.getElem?_map, Array.getElem?_toList, hfile, Option.map_none] at hwf
    cases hwf

/-! ## Section 2: Token slices and cursor monotonicity -/

private def tokenSlice (tokens : Array SurfaceToken) (lo hi : Nat) : List SurfaceToken :=
  (tokens.toList.drop lo).take (hi - lo)

private theorem tokenSlice_self (tokens : Array SurfaceToken) (lo : Nat) :
    tokenSlice tokens lo lo = [] := by
  simp [tokenSlice]

private theorem tokenSlice_full (tokens : Array SurfaceToken) :
    tokenSlice tokens 0 tokens.size = tokens.toList := by
  simp [tokenSlice, ← Array.length_toList]

private theorem tokenSlice_append_drop (tokens : Array SurfaceToken) {lo hi : Nat}
    (hle : lo ≤ hi) :
    tokenSlice tokens lo hi ++ tokens.toList.drop hi = tokens.toList.drop lo := by
  unfold tokenSlice
  have hdrop : tokens.toList.drop hi = (tokens.toList.drop lo).drop (hi - lo) := by
    rw [List.drop_drop]
    have hadd : lo + (hi - lo) = hi := by omega
    rw [hadd]
  rw [hdrop, List.take_append_drop]

private theorem list_take_add {α : Type} :
    ∀ (l : List α) (i j : Nat), l.take (i + j) = l.take i ++ (l.drop i).take j
  | [], _, _ => by simp
  | a :: as, 0, j => by simp
  | a :: as, i + 1, j => by
    rw [Nat.succ_add, List.take_succ_cons, List.take_succ_cons, List.drop_succ_cons, List.cons_append]
    exact congrArg (a :: ·) (list_take_add as i j)

private theorem tokenSlice_append (tokens : Array SurfaceToken) {lo mid hi : Nat}
    (h1 : lo ≤ mid) (h2 : mid ≤ hi) :
    tokenSlice tokens lo mid ++ tokenSlice tokens mid hi = tokenSlice tokens lo hi := by
  unfold tokenSlice
  have hsplit : hi - lo = (mid - lo) + (hi - mid) := by omega
  rw [hsplit, list_take_add, List.drop_drop]
  have hadd : lo + (mid - lo) = mid := by omega
  rw [hadd]

private theorem tokenSlice_cons (tokens : Array SurfaceToken) {lo hi : Nat} {tok : SurfaceToken}
    (hget : tokens[lo]? = some tok) (hlt : lo + 1 ≤ hi) :
    tokenSlice tokens lo hi = tok :: tokenSlice tokens (lo + 1) hi := by
  unfold tokenSlice
  have hltSize : lo < tokens.toList.length := by
    rw [Array.length_toList]
    exact (Array.getElem?_eq_some_iff.mp hget).1
  have htok : tokens.toList[lo] = tok := by
    have h1 := (Array.getElem?_eq_some_iff.mp hget).2
    simpa [Array.getElem_toList] using h1
  have hdrop : tokens.toList.drop lo = tok :: tokens.toList.drop (lo + 1) := by
    rw [List.drop_eq_getElem_cons hltSize, htok]
  have hsub : hi - lo = (hi - (lo + 1)) + 1 := by omega
  rw [hdrop, hsub, List.take_succ_cons]

private theorem list_getElem?_eq_drop_head? {α : Type} :
    ∀ (xs : List α) (i : Nat), xs[i]? = (xs.drop i).head?
  | [], 0 => rfl
  | [], _ + 1 => rfl
  | _ :: _, 0 => rfl
  | _ :: xs, i + 1 => list_getElem?_eq_drop_head? xs i

private theorem getElem?_eq_drop_head? (tokens : Array SurfaceToken) (i : Nat) :
    tokens[i]? = (tokens.toList.drop i).head? := by
  rw [← Array.getElem?_toList]
  exact list_getElem?_eq_drop_head? tokens.toList i

private theorem require_required_val {c : ParseCursor} {target slice : Slice}
    {cause : UnsupportedCause} {span : FileSpan} :
    (c.require target slice cause span).required.val = max c.required.val slice.val := by
  unfold ParseCursor.require
  dsimp only
  split <;> omega

private theorem require_required_ge_left {c : ParseCursor} {target slice : Slice}
    {cause : UnsupportedCause} {span : FileSpan} :
    c.required.val ≤ (c.require target slice cause span).required.val := by
  rw [require_required_val]
  exact Nat.le_max_left _ _

private theorem require_required_ge_right {c : ParseCursor} {target slice : Slice}
    {cause : UnsupportedCause} {span : FileSpan} :
    slice.val ≤ (c.require target slice cause span).required.val := by
  rw [require_required_val]
  exact Nat.le_max_right _ _

private theorem require_hit_some {c : ParseCursor} {target slice : Slice}
    {cause : UnsupportedCause} {span : FileSpan} {hit : FileSpan × UnsupportedCause}
    (h : c.firstUnsupportedHit = some hit) :
    (c.require target slice cause span).firstUnsupportedHit = some hit := by
  simp only [ParseCursor.require, h]

private theorem parseSymbol_parseArgs_mono {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) (tokens : Array SurfaceToken) (targetSlice : Slice)
    (eof : FileSpan) (fuel : Nat) :
    (∀ (n : Fin count) (c : ParseCursor),
      c.offset ≤ tokens.size →
      let c' := (parseSymbol table tokens targetSlice eof n c fuel).1
      c.offset ≤ c'.offset ∧ c'.offset ≤ tokens.size ∧
      c.steps ≤ c'.steps ∧ c.required.val ≤ c'.required.val ∧
      (∀ hit, c.firstUnsupportedHit = some hit → c'.firstUnsupportedHit = some hit)) ∧
    (∀ (prodId : ProductionId) (rhs : List (Sym Terminal (Fin count))) (c : ParseCursor),
      c.offset ≤ tokens.size →
      let c' := (parseArgs table tokens targetSlice eof prodId fuel rhs c).1
      c.offset ≤ c'.offset ∧ c'.offset ≤ tokens.size ∧
      c.steps ≤ c'.steps ∧ c.required.val ≤ c'.required.val ∧
      (∀ hit, c.firstUnsupportedHit = some hit → c'.firstUnsupportedHit = some hit)) := by
  induction fuel with
  | zero =>
    exact ⟨fun _ _ h => ⟨Nat.le_refl _, h, Nat.le_refl _, Nat.le_refl _, fun _ hh => hh⟩,
      fun _ _ _ h => ⟨Nat.le_refl _, h, Nat.le_refl _, Nat.le_refl _, fun _ hh => hh⟩⟩
  | succ fuel ih =>
    obtain ⟨ihSym, ihArgs⟩ := ih
    constructor
    · intro n c hoff
      unfold parseSymbol
      dsimp only
      cases table.lookup n (tokens[c.offset]?.map fun t => t.value.kind) with
      | none => exact ⟨Nat.le_refl _, hoff, Nat.le_succ _, Nat.le_refl _, fun _ hh => hh⟩
      | some index =>
        dsimp only
        cases g.productions[index]? with
        | none => exact ⟨Nat.le_refl _, hoff, Nat.le_succ _, Nat.le_refl _, fun _ hh => hh⟩
        | some production =>
          dsimp only
          by_cases hlhs : production.lhs = n
          · simp only [hlhs, ↓reduceDIte]
            let c₁ := ({c with steps := c.steps + 1}).require targetSlice production.firstSlice
              (.production production.id) ((tokens[c.offset]?.map Located.span).getD eof)
            have hargs := ihArgs production.id production.rhs c₁ hoff
            cases hpa : parseArgs table tokens targetSlice eof production.id fuel production.rhs c₁ with
            | mk c₂ res =>
              rw [hpa] at hargs
              have hreq : c.required.val ≤ c₁.required.val :=
                require_required_ge_left (c := {c with steps := c.steps + 1})
              cases res with
              | error stop =>
                exact ⟨hargs.1, hargs.2.1, Nat.le_trans (Nat.le_succ _) hargs.2.2.1,
                  Nat.le_trans hreq hargs.2.2.2.1,
                  fun hit hh => hargs.2.2.2.2 hit (require_hit_some (c := {c with steps := c.steps + 1}) hh)⟩
              | ok args =>
                exact ⟨hargs.1, hargs.2.1,
                  Nat.le_trans (Nat.le_succ _) (Nat.le_trans hargs.2.2.1 (Nat.le_add_right _ _)),
                  Nat.le_trans hreq hargs.2.2.2.1,
                  fun hit hh => hargs.2.2.2.2 hit (require_hit_some (c := {c with steps := c.steps + 1}) hh)⟩
          · simp only [hlhs, ↓reduceDIte]
            exact ⟨Nat.le_refl _, hoff, Nat.le_succ _, Nat.le_refl _, fun _ hh => hh⟩
    · intro prodId rhs c hoff
      cases rhs with
      | nil =>
        unfold parseArgs
        exact ⟨Nat.le_refl _, hoff, Nat.le_succ _, Nat.le_refl _, fun _ hh => hh⟩
      | cons sym rest =>
        cases sym with
        | t kind =>
          unfold parseArgs
          dsimp only
          cases htok : tokens[c.offset]? with
          | none => exact ⟨Nat.le_refl _, hoff, Nat.le_succ _, Nat.le_refl _, fun _ hh => hh⟩
          | some token =>
            dsimp only
            split
            · exact ⟨Nat.le_refl _, hoff, Nat.le_succ _, Nat.le_refl _, fun _ hh => hh⟩
            · have hlt : c.offset < tokens.size := (Array.getElem?_eq_some_iff.mp htok).1
              let c₁ := ({c with steps := c.steps + 1}).require targetSlice (g.required token.value)
                (tokenUnsupportedCause prodId token.value) token.span
              let c₂ := {c₁ with offset := c₁.offset + 1}
              have hargs := ihArgs prodId rest c₂ hlt
              have hreq : c.required.val ≤ c₁.required.val :=
                require_required_ge_left (c := {c with steps := c.steps + 1})
              exact ⟨Nat.le_trans (Nat.le_succ _) hargs.1, hargs.2.1,
                Nat.le_trans (Nat.le_succ _) hargs.2.2.1,
                Nat.le_trans hreq hargs.2.2.2.1,
                fun hit hh => hargs.2.2.2.2 hit (require_hit_some (c := {c with steps := c.steps + 1}) hh)⟩
        | n child =>
          unfold parseArgs
          dsimp only
          have hchild := ihSym child {c with steps := c.steps + 1} hoff
          cases hps : parseSymbol table tokens targetSlice eof child {c with steps := c.steps + 1} fuel with
          | mk c₁ res =>
            rw [hps] at hchild
            cases res with
            | error stop =>
              exact ⟨hchild.1, hchild.2.1, Nat.le_trans (Nat.le_succ _) hchild.2.2.1,
                hchild.2.2.2.1, hchild.2.2.2.2⟩
            | ok val =>
              have hrest := ihArgs prodId rest c₁ hchild.2.1
              exact ⟨Nat.le_trans hchild.1 hrest.1, hrest.2.1,
                Nat.le_trans (Nat.le_succ _) (Nat.le_trans hchild.2.2.1 hrest.2.2.1),
                Nat.le_trans hchild.2.2.2.1 hrest.2.2.2.1,
                fun hit hh => hrest.2.2.2.2 hit (hchild.2.2.2.2 hit hh)⟩

private theorem parseSymbol_parseArgs_prefix_congr {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) (t₁ t₂ : Array SurfaceToken) (targetSlice : Slice)
    (eof : FileSpan) (fuel : Nat) :
    (∀ (n : Fin count) (c : ParseCursor),
      c.offset ≤ t₁.size →
      (∀ i ≤ (parseSymbol table t₁ targetSlice eof n c fuel).1.offset, t₁[i]? = t₂[i]?) →
      parseSymbol table t₂ targetSlice eof n c fuel =
        parseSymbol table t₁ targetSlice eof n c fuel) ∧
    (∀ (prodId : ProductionId) (rhs : List (Sym Terminal (Fin count))) (c : ParseCursor),
      c.offset ≤ t₁.size →
      (∀ i ≤ (parseArgs table t₁ targetSlice eof prodId fuel rhs c).1.offset, t₁[i]? = t₂[i]?) →
      parseArgs table t₂ targetSlice eof prodId fuel rhs c =
        parseArgs table t₁ targetSlice eof prodId fuel rhs c) := by
  induction fuel with
  | zero => exact ⟨fun _ _ _ _ => rfl, fun _ _ _ _ _ => rfl⟩
  | succ fuel ih =>
    obtain ⟨ihSym, ihArgs⟩ := ih
    constructor
    · intro n c hoff hag
      have hmono := (parseSymbol_parseArgs_mono table t₁ targetSlice eof (fuel + 1)).1 n c hoff
      have hat : t₁[c.offset]? = t₂[c.offset]? := hag c.offset hmono.1
      unfold parseSymbol
      dsimp only
      rw [← hat]
      cases hlook : table.lookup n (t₁[c.offset]?.map fun t => t.value.kind) with
      | none => rfl
      | some index =>
        dsimp only
        cases hprod : g.productions[index]? with
        | none => rfl
        | some production =>
          dsimp only
          by_cases hlhs : production.lhs = n
          · simp only [hlhs, ↓reduceDIte]
            let c₁ := ({c with steps := c.steps + 1}).require targetSlice production.firstSlice
              (.production production.id) ((t₁[c.offset]?.map Located.span).getD eof)
            have hbound : (parseSymbol table t₁ targetSlice eof n c (fuel + 1)).1.offset =
                (parseArgs table t₁ targetSlice eof production.id fuel production.rhs c₁).1.offset := by
              unfold parseSymbol
              simp only [hlook, hprod, hlhs, ↓reduceDIte]
              cases parseArgs table t₁ targetSlice eof production.id fuel production.rhs c₁ with
              | mk c₂ res => cases res <;> rfl
            have hag' : ∀ i ≤ (parseArgs table t₁ targetSlice eof production.id fuel production.rhs c₁).1.offset,
                t₁[i]? = t₂[i]? := fun i hi => hag i (hbound.symm ▸ hi)
            rw [ihArgs production.id production.rhs c₁ hoff hag']
          · simp only [hlhs, ↓reduceDIte]
    · intro prodId rhs c hoff hag
      cases rhs with
      | nil => rfl
      | cons sym rest =>
        cases sym with
        | t kind =>
          have hmono := (parseSymbol_parseArgs_mono table t₁ targetSlice eof (fuel + 1)).2 prodId (.t kind :: rest) c hoff
          have hat : t₁[c.offset]? = t₂[c.offset]? := hag c.offset hmono.1
          unfold parseArgs
          dsimp only
          rw [← hat]
          cases htok : t₁[c.offset]? with
          | none => rfl
          | some token =>
            dsimp only
            split
            · rfl
            · rename_i hnotNe
              have hlt : c.offset < t₁.size := (Array.getElem?_eq_some_iff.mp htok).1
              let c₁ := ({c with steps := c.steps + 1}).require targetSlice (g.required token.value)
                (tokenUnsupportedCause prodId token.value) token.span
              let c₂ := {c₁ with offset := c₁.offset + 1}
              have hbound : (parseArgs table t₁ targetSlice eof prodId (fuel + 1) (.t kind :: rest) c).1.offset =
                  (parseArgs table t₁ targetSlice eof prodId fuel rest c₂).1.offset := by
                conv => lhs; unfold parseArgs
                simp only [htok, hnotNe, ↓reduceIte]
                rfl
              have hag' : ∀ i ≤ (parseArgs table t₁ targetSlice eof prodId fuel rest c₂).1.offset,
                  t₁[i]? = t₂[i]? := fun i hi => hag i (hbound.symm ▸ hi)
              rw [ihArgs prodId rest c₂ hlt hag']
        | n child =>
          unfold parseArgs
          dsimp only
          have hchildMono := (parseSymbol_parseArgs_mono table t₁ targetSlice eof fuel).1 child {c with steps := c.steps + 1} hoff
          cases hps : parseSymbol table t₁ targetSlice eof child {c with steps := c.steps + 1} fuel with
          | mk c₁ res =>
            rw [hps] at hchildMono
            have hleEnd : c₁.offset ≤ (parseArgs table t₁ targetSlice eof prodId (fuel + 1) (.n child :: rest) c).1.offset := by
              unfold parseArgs
              simp only [hps]
              cases res with
              | error _ => exact Nat.le_refl _
              | ok _ => exact ((parseSymbol_parseArgs_mono table t₁ targetSlice eof fuel).2 prodId rest c₁ hchildMono.2.1).1
            have hagChild : ∀ i ≤ (parseSymbol table t₁ targetSlice eof child {c with steps := c.steps + 1} fuel).1.offset,
                t₁[i]? = t₂[i]? := by
              intro i hi
              rw [hps] at hi
              exact hag i (Nat.le_trans hi hleEnd)
            rw [ihSym child {c with steps := c.steps + 1} hoff hagChild, hps]
            cases res with
            | error _ => rfl
            | ok val =>
              dsimp only
              have hbound : (parseArgs table t₁ targetSlice eof prodId (fuel + 1) (.n child :: rest) c).1.offset =
                  (parseArgs table t₁ targetSlice eof prodId fuel rest c₁).1.offset := by
                conv => lhs; unfold parseArgs
                simp only [hps]
              have hagRest : ∀ i ≤ (parseArgs table t₁ targetSlice eof prodId fuel rest c₁).1.offset,
                  t₁[i]? = t₂[i]? := fun i hi => hag i (hbound.symm ▸ hi)
              rw [ihArgs prodId rest c₁ hchildMono.2.1 hagRest]

/-! ## Section 3: Derivation and Predictive structural lemmas -/

private theorem derives_mono_aux {count : Nat} {g : SharedGrammar count} {s₁ s₂ : Slice}
    (hle : s₁.val ≤ s₂.val) :
    (∀ {n tokens value steps}, Derives g s₁ n tokens value steps →
      Derives g s₂ n tokens value steps) ∧
    (∀ {rhs tokens args steps}, DerivesArgs g s₁ rhs tokens args steps →
      DerivesArgs g s₂ rhs tokens args steps) := by
  constructor
  · intro n tokens value steps h
    exact Derives.rec
      (motive_1 := fun n tokens value steps _ => Derives g s₂ n tokens value steps)
      (motive_2 := fun rhs tokens args steps _ => DerivesArgs g s₂ rhs tokens args steps)
      (fun p _ _ _ hmem hslice _ ih => .apply p _ _ hmem (Nat.le_trans hslice hle) ih)
      .nil
      (fun hkind hreq _ ih => .token hkind (Nat.le_trans hreq hle) ih)
      (fun _ _ ihL ihR => .child ihL ihR)
      h
  · intro rhs tokens args steps h
    exact DerivesArgs.rec
      (motive_1 := fun n tokens value steps _ => Derives g s₂ n tokens value steps)
      (motive_2 := fun rhs tokens args steps _ => DerivesArgs g s₂ rhs tokens args steps)
      (fun p _ _ _ hmem hslice _ ih => .apply p _ _ hmem (Nat.le_trans hslice hle) ih)
      .nil
      (fun hkind hreq _ ih => .token hkind (Nat.le_trans hreq hle) ih)
      (fun _ _ ihL ihR => .child ihL ihR)
      h

theorem derives_mono {count : Nat} {g : SharedGrammar count} {s₁ s₂ : Slice}
    (hle : s₁.val ≤ s₂.val) {n : Fin count} {tokens : List SurfaceToken}
    {value : g.value n} {steps : Nat}
    (h : Derives g s₁ n tokens value steps) :
    Derives g s₂ n tokens value steps :=
  (derives_mono_aux hle).1 h

theorem predictive_mono {count : Nat} {g : SharedGrammar count} {table : CheckedTable g}
    {s₁ s₂ : Slice} (hle : s₁.val ≤ s₂.val) {n : Fin count}
    {tokens rest : List SurfaceToken} {value : g.value n} {steps : Nat}
    (h : Predictive table s₁ n tokens rest value steps) :
    Predictive table s₂ n tokens rest value steps :=
  Predictive.rec
    (motive_1 := fun n tokens rest value steps _ => Predictive table s₂ n tokens rest value steps)
    (motive_2 := fun rhs tokens rest args steps _ => PredictiveArgs table s₂ rhs tokens rest args steps)
    (fun p idx _ _ _ _ hget hlook hslice _ ih =>
      .apply p idx _ _ _ hget hlook (Nat.le_trans hslice hle) ih)
    (fun rest => .nil rest)
    (fun hkind hreq _ ih => .token hkind (Nat.le_trans hreq hle) ih)
    (fun _ _ ihL ihR => .child ihL ihR)
    h

theorem predictive_to_derives {count : Nat} {g : SharedGrammar count}
    {table : CheckedTable g} {slice : Slice} {n : Fin count}
    {tokens rest : List SurfaceToken} {value : g.value n} {steps : Nat}
    (h : Predictive table slice n tokens rest value steps) :
    Derives g slice n tokens value steps :=
  Predictive.rec
    (motive_1 := fun n tokens _ value steps _ => Derives g slice n tokens value steps)
    (motive_2 := fun rhs tokens _ args steps _ => DerivesArgs g slice rhs tokens args steps)
    (fun p _ _ _ _ _ hget _ hslice _ ih =>
      .apply p _ _ (Array.mem_toList_iff.mpr (Array.mem_of_getElem? hget)) hslice ih)
    (fun _ => .nil)
    (fun hkind hreq _ ih => .token hkind hreq ih)
    (fun _ _ ihL ihR => .child ihL ihR)
    h

private theorem nullableSeq_nil {T N : Type} [DecidableEq T] [DecidableEq N]
    (g : Grammar T N) : NullableSeq g [] :=
  ⟨[], rfl, fun _ h => nomatch h⟩

private theorem nullableSeq_cons {T N : Type} [DecidableEq T] [DecidableEq N]
    {g : Grammar T N} {n : N} {rhs : List (Sym T N)}
    (hn : Nullable g n) (hr : NullableSeq g rhs) : NullableSeq g (.n n :: rhs) := by
  obtain ⟨xs, rfl, hxs⟩ := hr
  refine ⟨n :: xs, rfl, fun y hy => ?_⟩
  rcases List.mem_cons.mp hy with rfl | hy'
  · exact hn
  · exact hxs y hy'

private theorem nullableSeq_append_single {T N : Type} [DecidableEq T] [DecidableEq N]
    {g : Grammar T N} {xs : List (Sym T N)} {n : N}
    (hxs : NullableSeq g xs) (hn : Nullable g n) : NullableSeq g (xs ++ [.n n]) := by
  obtain ⟨ns, rfl, hns⟩ := hxs
  refine ⟨ns ++ [n], by simp, fun y hy => ?_⟩
  rcases List.mem_append.mp hy with h1 | h2
  · exact hns y h1
  · rcases List.mem_singleton.mp h2 with rfl
    exact hn

private theorem derives_nullable_of_nil {count : Nat} {g : SharedGrammar count} {slice : Slice} :
    (∀ {n value steps}, Derives g slice n [] value steps → Nullable g.grammar n) ∧
    (∀ {rhs args steps}, DerivesArgs g slice rhs [] args steps → NullableSeq g.grammar rhs) := by
  constructor
  · intro n value steps h
    exact Derives.rec
      (motive_1 := fun n toks _ _ _ => toks = [] → Nullable g.grammar n)
      (motive_2 := fun rhs toks _ _ _ => toks = [] → NullableSeq g.grammar rhs)
      (fun p _ _ _ hmem _ _ ih heq => by
        obtain ⟨xs, hxs, hall⟩ := ih heq
        have hprod : (p.lhs, p.rhs) ∈ g.grammar.prods :=
          List.mem_map.mpr ⟨p, hmem, rfl⟩
        exact .intro (hxs ▸ hprod) hall)
      (fun _ => nullableSeq_nil g.grammar)
      (fun _ _ _ _ heq => by cases heq)
      (fun _ _ ihL ihR heq => by
        obtain ⟨h1, h2⟩ := List.append_eq_nil_iff.mp heq
        exact nullableSeq_cons (ihL h1) (ihR h2))
      h rfl
  · intro rhs args steps h
    exact DerivesArgs.rec
      (motive_1 := fun n toks _ _ _ => toks = [] → Nullable g.grammar n)
      (motive_2 := fun rhs toks _ _ _ => toks = [] → NullableSeq g.grammar rhs)
      (fun p _ _ _ hmem _ _ ih heq => by
        obtain ⟨xs, hxs, hall⟩ := ih heq
        have hprod : (p.lhs, p.rhs) ∈ g.grammar.prods :=
          List.mem_map.mpr ⟨p, hmem, rfl⟩
        exact .intro (hxs ▸ hprod) hall)
      (fun _ => nullableSeq_nil g.grammar)
      (fun _ _ _ _ heq => by cases heq)
      (fun _ _ ihL ihR heq => by
        obtain ⟨h1, h2⟩ := List.append_eq_nil_iff.mp heq
        exact nullableSeq_cons (ihL h1) (ihR h2))
      h rfl

private theorem derives_first_of_cons {count : Nat} {g : SharedGrammar count} {slice : Slice} :
    (∀ {n tok toks value steps}, Derives g slice n (tok :: toks) value steps →
      First g.grammar n tok.value.kind) ∧
    (∀ {rhs tok toks args steps}, DerivesArgs g slice rhs (tok :: toks) args steps →
      FirstSeq g.grammar rhs tok.value.kind) := by
  have stepApply :
      ∀ (p : Production g.value) (tokens : List SurfaceToken) (args : ActionArgs g.value p.rhs)
        (steps : Nat), p ∈ g.productions.toList → p.firstSlice.val ≤ slice.val →
        DerivesArgs g slice p.rhs tokens args steps →
        (∀ tok toks, tokens = tok :: toks → FirstSeq g.grammar p.rhs tok.value.kind) →
        ∀ tok toks, tokens = tok :: toks → First g.grammar p.lhs tok.value.kind := by
    intro p _ _ _ hmem _ _ ih tok toks heq
    obtain ⟨pre, post, hpre, hbranch⟩ := ih tok toks heq
    have hprod : (p.lhs, p.rhs) ∈ g.grammar.prods := List.mem_map.mpr ⟨p, hmem, rfl⟩
    rcases hbranch with hrhs | ⟨y, hrhs, hy⟩
    · exact .term (hrhs ▸ hprod) hpre
    · exact .nonterm (hrhs ▸ hprod) hpre hy
  have stepToken :
      ∀ {kind : Terminal} {token : SurfaceToken} {rhs : List (Sym Terminal (Fin count))}
        {tokens : List SurfaceToken} {args : ActionArgs g.value rhs} {steps : Nat},
        token.value.kind = kind → (g.required token.value).val ≤ slice.val →
        DerivesArgs g slice rhs tokens args steps →
        (∀ tok toks, tokens = tok :: toks → FirstSeq g.grammar rhs tok.value.kind) →
        ∀ tok toks, token :: tokens = tok :: toks → FirstSeq g.grammar (.t kind :: rhs) tok.value.kind := by
    intro kind token rhs _ _ _ hkind _ _ _ tok toks heq
    obtain ⟨rfl, rfl⟩ := List.cons.inj heq
    exact ⟨[], rhs, nullableSeq_nil g.grammar, Or.inl (by rw [hkind]; rfl)⟩
  have stepChild :
      ∀ {m : Fin count} {rhs : List (Sym Terminal (Fin count))} {head tail : List SurfaceToken}
        {val : g.value m} {args : ActionArgs g.value rhs} {left right : Nat},
        Derives g slice m head val left → DerivesArgs g slice rhs tail args right →
        (∀ tok toks, head = tok :: toks → First g.grammar m tok.value.kind) →
        (∀ tok toks, tail = tok :: toks → FirstSeq g.grammar rhs tok.value.kind) →
        ∀ tok toks, head ++ tail = tok :: toks → FirstSeq g.grammar (.n m :: rhs) tok.value.kind := by
    intro m rhs head tail _ _ _ _ hL _ ihL ihR tok toks heq
    cases head with
    | nil =>
      have hn : Nullable g.grammar m := derives_nullable_of_nil.1 hL
      obtain ⟨pre, post, hpre, hbranch⟩ := ihR tok toks (by simpa using heq)
      refine ⟨.n m :: pre, post, nullableSeq_cons hn hpre, ?_⟩
      rcases hbranch with hrhs | ⟨y, hrhs, hy⟩
      · exact Or.inl (congrArg (Sym.n m :: ·) hrhs)
      · exact Or.inr ⟨y, congrArg (Sym.n m :: ·) hrhs, hy⟩
    | cons t' head' =>
      obtain ⟨rfl, _⟩ := List.cons.inj heq
      exact ⟨[], rhs, nullableSeq_nil g.grammar, Or.inr ⟨m, rfl, ihL t' head' rfl⟩⟩
  constructor
  · intro n tok toks value steps h
    exact Derives.rec
      (motive_1 := fun n tokens _ _ _ => ∀ tok toks, tokens = tok :: toks → First g.grammar n tok.value.kind)
      (motive_2 := fun rhs tokens _ _ _ => ∀ tok toks, tokens = tok :: toks → FirstSeq g.grammar rhs tok.value.kind)
      stepApply (fun _ _ heq => by cases heq) stepToken stepChild h tok toks rfl
  · intro rhs tok toks args steps h
    exact DerivesArgs.rec
      (motive_1 := fun n tokens _ _ _ => ∀ tok toks, tokens = tok :: toks → First g.grammar n tok.value.kind)
      (motive_2 := fun rhs tokens _ _ _ => ∀ tok toks, tokens = tok :: toks → FirstSeq g.grammar rhs tok.value.kind)
      stepApply (fun _ _ heq => by cases heq) stepToken stepChild h tok toks rfl

private theorem derives_to_predictive_aux {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) {slice : Slice} {n : Fin count}
    {tokens : List SurfaceToken} {value : g.value n} {steps : Nat}
    (h : Derives g slice n tokens value steps) :
    ∀ rest, Follow g.grammar n (rest.head?.map fun t => t.value.kind) →
      Predictive table slice n tokens rest value steps :=
  Derives.rec
    (motive_1 := fun n tokens value steps _ =>
      ∀ rest, Follow g.grammar n (rest.head?.map fun t => t.value.kind) →
        Predictive table slice n tokens rest value steps)
    (motive_2 := fun rhs tokens args steps _ =>
      ∀ (lhs : Fin count) (pre : List (Sym Terminal (Fin count))) (rest : List SurfaceToken),
        (lhs, pre ++ rhs) ∈ g.grammar.prods →
        Follow g.grammar lhs (rest.head?.map fun t => t.value.kind) →
        PredictiveArgs table slice rhs tokens rest args steps)
    (fun p toks args _ hmem hslice hargs ihArgs rest hfol => by
      obtain ⟨idx, hget⟩ := List.mem_iff_getElem?.mp hmem
      have hgetArr : g.productions[idx]? = some p := by
        simpa [Array.getElem?_toList] using hget
      have hprodAt : g.grammar.prods[idx]? = some (p.lhs, p.rhs) := by
        simp only [SharedGrammar.grammar, List.getElem?_map, hget, Option.map_some]
      have hpred : Predicts g.grammar idx ((toks ++ rest).head?.map fun t => t.value.kind) := by
        refine ⟨p.lhs, p.rhs, hprodAt, ?_⟩
        cases toks with
        | nil =>
          exact Or.inr ⟨derives_nullable_of_nil.2 hargs, hfol⟩
        | cons tok toks' =>
          exact Or.inl ⟨tok.value.kind, rfl, derives_first_of_cons.2 hargs⟩
      have hlook : table.lookup p.lhs ((toks ++ rest).head?.map fun t => t.value.kind) = some idx :=
        (table.predicts p.lhs _ idx).mpr ⟨hpred, p.rhs, hprodAt⟩
      have hprodMem : (p.lhs, [] ++ p.rhs) ∈ g.grammar.prods :=
        List.mem_map.mpr ⟨p, hmem, rfl⟩
      exact .apply p idx toks rest args hgetArr hlook hslice (ihArgs p.lhs [] rest hprodMem hfol))
    (fun _ _ rest _ _ => .nil rest)
    (fun {kind _ rhs _ _ _} hkind hreq _ ihArgs lhs pre rest hmem hfol => by
      have hmem' : (lhs, (pre ++ [.t kind]) ++ rhs) ∈ g.grammar.prods := by
        simpa [List.append_assoc] using hmem
      exact .token hkind hreq (ihArgs lhs (pre ++ [.t kind]) rest hmem' hfol))
    (fun {child rhs head tail _ _ _ _} _ hR ihL ihR lhs pre rest hmem hfol => by
      have hchildFol : Follow g.grammar child ((tail ++ rest).head?.map fun t => t.value.kind) := by
        cases tail with
        | nil =>
          exact .tail hmem (derives_nullable_of_nil.2 hR) hfol
        | cons tok tail' =>
          exact .first hmem (derives_first_of_cons.2 hR)
      have hmem' : (lhs, (pre ++ [.n child]) ++ rhs) ∈ g.grammar.prods := by
        simpa [List.append_assoc] using hmem
      exact .child (ihL (tail ++ rest) hchildFol) (ihR lhs (pre ++ [.n child]) rest hmem' hfol))
    h

theorem derives_to_predictive {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) {slice : Slice} {tokens : List SurfaceToken}
    {tree : g.value g.start} {steps : Nat}
    (h : Derives g slice g.start tokens tree steps) :
    Predictive table slice g.start tokens [] tree steps :=
  derives_to_predictive_aux table h [] Follow.start

private theorem predictive_inv {count : Nat} {g : SharedGrammar count}
    {table : CheckedTable g} {slice : Slice} {n : Fin count}
    {tokens rest : List SurfaceToken} {val : g.value n} {steps : Nat}
    (h : Predictive table slice n tokens rest val steps) :
    ∃ (p : Production g.value) (idx : Nat) (args : ActionArgs g.value p.rhs) (dSteps : Nat)
      (hlhs : p.lhs = n),
      g.productions[idx]? = some p ∧
      table.lookup p.lhs ((tokens ++ rest).head?.map fun t => t.value.kind) = some idx ∧
      p.firstSlice.val ≤ slice.val ∧
      PredictiveArgs table slice p.rhs tokens rest args dSteps ∧
      val = hlhs ▸ (p.action.eval args).2 ∧
      steps = dSteps + 1 + (p.action.eval args).1 := by
  cases h with
  | apply p idx _ _ args hget hlook hslice hargs =>
    exact ⟨p, idx, args, _, rfl, hget, hlook, hslice, hargs, rfl, rfl⟩

private theorem predictive_unique_aux {count : Nat} {g : SharedGrammar count}
    {table : CheckedTable g} {s₁ s₂ : Slice} :
    (∀ {n head₁ rest₁ val₁ steps₁}, Predictive table s₁ n head₁ rest₁ val₁ steps₁ →
      ∀ {head₂ rest₂ val₂ steps₂}, Predictive table s₂ n head₂ rest₂ val₂ steps₂ →
        head₁ ++ rest₁ = head₂ ++ rest₂ →
        head₁ = head₂ ∧ rest₁ = rest₂ ∧ val₁ = val₂ ∧ steps₁ = steps₂) := by
  intro n head₁ rest₁ val₁ steps₁ h₁
  exact Predictive.rec
    (motive_1 := fun n head₁ rest₁ val₁ steps₁ _ =>
      ∀ {head₂ rest₂ val₂ steps₂}, Predictive table s₂ n head₂ rest₂ val₂ steps₂ →
        head₁ ++ rest₁ = head₂ ++ rest₂ →
        head₁ = head₂ ∧ rest₁ = rest₂ ∧ val₁ = val₂ ∧ steps₁ = steps₂)
    (motive_2 := fun rhs head₁ rest₁ args₁ steps₁ _ =>
      ∀ {head₂ rest₂ args₂ steps₂}, PredictiveArgs table s₂ rhs head₂ rest₂ args₂ steps₂ →
        head₁ ++ rest₁ = head₂ ++ rest₂ →
        head₁ = head₂ ∧ rest₁ = rest₂ ∧ args₁ = args₂ ∧ steps₁ = steps₂)
    (fun p₁ idx₁ toks₁ rest₁ args₁ _ hget₁ hlook₁ _ _ ihArgs head₂ rest₂ val₂ steps₂ h₂ hstr => by
      obtain ⟨p₂, idx₂, args₂, dSteps₂, hlhs₂, hget₂, hlook₂, _, hargs₂, rfl, rfl⟩ := predictive_inv h₂
      rw [hstr] at hlook₁
      have hlook₂' : table.lookup p₁.lhs ((head₂ ++ rest₂).head?.map fun t => t.value.kind) = some idx₂ := by
        simpa [hlhs₂] using hlook₂
      have hidx : idx₁ = idx₂ := Option.some.inj (hlook₁.symm.trans hlook₂')
      subst hidx
      have hp : p₁ = p₂ := Option.some.inj (hget₁.symm.trans hget₂)
      subst hp
      obtain ⟨rfl, rfl, rfl, rfl⟩ := ihArgs hargs₂ hstr
      exact ⟨rfl, rfl, rfl, rfl⟩)
    (fun rest₁ head₂ rest₂ args₂ steps₂ h₂ hstr => by
      cases h₂ with
      | nil _ =>
        exact ⟨rfl, hstr, rfl, rfl⟩)
    (fun hkind₁ _ _ ihArgs head₂ rest₂ args₂ steps₂ h₂ hstr => by
      cases h₂ with
      | token _ _ hargs₂ =>
        obtain ⟨rfl, htail⟩ := List.cons.inj hstr
        obtain ⟨rfl, rfl, rfl, rfl⟩ := ihArgs hargs₂ htail
        exact ⟨rfl, rfl, rfl, rfl⟩)
    (fun _ _ ihL ihR head₂ rest₂ args₂ steps₂ h₂ hstr => by
      cases h₂ with
      | child hL₂ hR₂ =>
        rw [List.append_assoc, List.append_assoc] at hstr
        obtain ⟨rfl, hmid, rfl, rfl⟩ := ihL hL₂ hstr
        obtain ⟨rfl, rfl, rfl, rfl⟩ := ihR hR₂ hmid
        exact ⟨rfl, rfl, rfl, rfl⟩)
    h₁

theorem derives_unique {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) {s₁ s₂ : Slice} {tokens : List SurfaceToken}
    {tree₁ tree₂ : g.value g.start} {steps₁ steps₂ : Nat}
    (h₁ : Derives g s₁ g.start tokens tree₁ steps₁)
    (h₂ : Derives g s₂ g.start tokens tree₂ steps₂) :
    tree₁ = tree₂ ∧ steps₁ = steps₂ := by
  obtain ⟨_, _, htree, hsteps⟩ :=
    predictive_unique_aux (derives_to_predictive table h₁) (derives_to_predictive table h₂) rfl
  exact ⟨htree, hsteps⟩

/-! ## Section 4: Cursor soundness on `.ok` -/

private theorem parseSymbol_parseArgs_sound_ok {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) (tokens : Array SurfaceToken) (targetSlice : Slice)
    (eof : FileSpan) (fuel : Nat) :
    (∀ (n : Fin count) (c : ParseCursor) (c' : ParseCursor) (val : g.value n),
      c.offset ≤ tokens.size →
      parseSymbol table tokens targetSlice eof n c fuel = (c', .ok val) →
      ∃ dSteps, dSteps ≤ c'.steps - c.steps ∧
        Derives g c'.required n (tokenSlice tokens c.offset c'.offset) val dSteps ∧
        Predictive table c'.required n (tokenSlice tokens c.offset c'.offset)
          (tokens.toList.drop c'.offset) val dSteps) ∧
    (∀ (prodId : ProductionId) (rhs : List (Sym Terminal (Fin count))) (c c' : ParseCursor)
      (args : ActionArgs g.value rhs),
      c.offset ≤ tokens.size →
      parseArgs table tokens targetSlice eof prodId fuel rhs c = (c', .ok args) →
      ∃ dSteps, dSteps ≤ c'.steps - c.steps ∧
        DerivesArgs g c'.required rhs (tokenSlice tokens c.offset c'.offset) args dSteps ∧
        PredictiveArgs table c'.required rhs (tokenSlice tokens c.offset c'.offset)
          (tokens.toList.drop c'.offset) args dSteps) := by
  induction fuel with
  | zero =>
    exact ⟨fun _ _ _ _ _ h => (by cases h), fun _ _ _ _ _ _ h => (by cases h)⟩
  | succ fuel ih =>
    obtain ⟨ihSym, ihArgs⟩ := ih
    constructor
    · intro n c c' val hoff hrun
      unfold parseSymbol at hrun
      dsimp only at hrun
      cases hlook : table.lookup n (tokens[c.offset]?.map fun t => t.value.kind) with
      | none => simp [hlook] at hrun
      | some index =>
        simp only [hlook] at hrun
        cases hprod : g.productions[index]? with
        | none => simp [hprod] at hrun
        | some production =>
          simp only [hprod] at hrun
          by_cases hlhs : production.lhs = n
          · simp only [hlhs, ↓reduceDIte] at hrun
            subst n
            let c₁ := ({c with steps := c.steps + 1}).require targetSlice production.firstSlice
              (.production production.id) ((tokens[c.offset]?.map Located.span).getD eof)
            cases hpa : parseArgs table tokens targetSlice eof production.id fuel production.rhs c₁ with
            | mk c₂ res =>
              dsimp only [c₁] at hpa
              rw [hpa] at hrun
              cases res with
              | error stop => cases hrun
              | ok args =>
                have hpair := Prod.mk.inj hrun
                have hc' : c' = {c₂ with steps := c₂.steps + (production.action.eval args).1} :=
                  hpair.1.symm
                have hval : val = (production.action.eval args).2 :=
                  (Except.ok.inj hpair.2).symm
                subst hc'
                subst hval
                have hmono := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel).2
                  production.id production.rhs c₁ hoff
                dsimp only [c₁] at hmono
                rw [hpa] at hmono
                dsimp only at hmono
                obtain ⟨dSteps, hleSteps, hder, hpred⟩ :=
                  ihArgs production.id production.rhs c₁ c₂ args hoff hpa
                have hreq₁ : production.firstSlice.val ≤ c₁.required.val := require_required_ge_right
                have hslice : production.firstSlice.val ≤ c₂.required.val :=
                  Nat.le_trans hreq₁ hmono.2.2.2.1
                have hmem : production ∈ g.productions.toList :=
                  Array.mem_toList_iff.mpr (Array.mem_of_getElem? hprod)
                have hc₁Off : c₁.offset = c.offset := rfl
                have hlook' : table.lookup production.lhs
                    ((tokenSlice tokens c.offset c₂.offset ++ tokens.toList.drop c₂.offset).head?.map
                      fun t => t.value.kind) = some index := by
                  rw [tokenSlice_append_drop tokens (hc₁Off ▸ hmono.1), ← getElem?_eq_drop_head?]
                  exact hlook
                refine ⟨dSteps + 1 + (production.action.eval args).1, ?_,
                  .apply production _ args hmem hslice hder,
                  .apply production index _ _ args hprod hlook' hslice hpred⟩
                have hc₁Steps : c₁.steps = c.steps + 1 := rfl
                have hge : c.steps + 1 ≤ c₂.steps := hc₁Steps ▸ hmono.2.2.1
                rw [hc₁Steps] at hleSteps
                dsimp only
                omega
          · simp only [hlhs, ↓reduceDIte] at hrun
            cases hrun
    · intro prodId rhs c c' args hoff hrun
      cases rhs with
      | nil =>
        unfold parseArgs at hrun
        obtain ⟨hc', hargs⟩ := Prod.mk.inj hrun
        subst hc'
        cases args
        refine ⟨0, by omega, ?_, ?_⟩
        · simpa [tokenSlice_self] using DerivesArgs.nil
        · simpa [tokenSlice_self] using PredictiveArgs.nil (tokens.toList.drop c.offset)
      | cons sym rest =>
        cases sym with
        | t kind =>
          unfold parseArgs at hrun
          dsimp only at hrun
          cases htok : tokens[c.offset]? with
          | none => simp [htok] at hrun
          | some token =>
            simp only [htok] at hrun
            split at hrun
            · cases hrun
            · rename_i hnotNe
              have hkindEq : token.value.kind = kind := by
                cases hdec : decide (token.value.kind = kind) with
                | true => exact of_decide_eq_true hdec
                | false => exact False.elim (hnotNe (of_decide_eq_false hdec))
              have hlt : c.offset < tokens.size := (Array.getElem?_eq_some_iff.mp htok).1
              let c₁ := ({c with steps := c.steps + 1}).require targetSlice (g.required token.value)
                (tokenUnsupportedCause prodId token.value) token.span
              let c₂ := {c₁ with offset := c₁.offset + 1}
              cases hpa : parseArgs table tokens targetSlice eof prodId fuel rest c₂ with
              | mk c₃ res =>
                dsimp only [c₂, c₁] at hpa
                rw [hpa] at hrun
                cases res with
                | error stop => cases hrun
                | ok restArgs =>
                  obtain ⟨hc', hargs⟩ := Prod.mk.inj hrun
                  subst hc'
                  have hargsEq : (token, restArgs) = args := Except.ok.inj hargs
                  subst hargsEq
                  have hmono := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel).2
                    prodId rest c₂ hlt
                  dsimp only [c₂, c₁] at hmono
                  rw [hpa] at hmono
                  dsimp only at hmono
                  obtain ⟨dSteps, hleSteps, hder, hpred⟩ :=
                    ihArgs prodId rest c₂ c₃ restArgs hlt hpa
                  have hreq₁ : (g.required token.value).val ≤ c₁.required.val := require_required_ge_right
                  have hreq : (g.required token.value).val ≤ c₃.required.val :=
                    Nat.le_trans hreq₁ hmono.2.2.2.1
                  have hc₂Off : c₂.offset = c.offset + 1 := rfl
                  have hts : tokenSlice tokens c.offset c₃.offset =
                      token :: tokenSlice tokens (c.offset + 1) c₃.offset :=
                    tokenSlice_cons tokens htok (hc₂Off ▸ hmono.1)
                  rw [hts]
                  refine ⟨dSteps + 1, ?_, .token hkindEq hreq hder, .token hkindEq hreq hpred⟩
                  have hc₂Steps : c₂.steps = c.steps + 1 := rfl
                  have hge : c.steps + 1 ≤ c₃.steps := hc₂Steps ▸ hmono.2.2.1
                  rw [hc₂Steps] at hleSteps
                  dsimp only
                  omega
        | n child =>
          unfold parseArgs at hrun
          dsimp only at hrun
          cases hps : parseSymbol table tokens targetSlice eof child {c with steps := c.steps + 1} fuel with
          | mk c₁ res₁ =>
            rw [hps] at hrun
            cases res₁ with
            | error stop => cases hrun
            | ok childVal =>
              dsimp only at hrun
              have hmono₁ := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel).1
                child {c with steps := c.steps + 1} hoff
              rw [hps] at hmono₁
              cases hpa : parseArgs table tokens targetSlice eof prodId fuel rest c₁ with
              | mk c₂ res₂ =>
                rw [hpa] at hrun
                cases res₂ with
                | error stop => cases hrun
                | ok restArgs =>
                  obtain ⟨hc', hargs⟩ := Prod.mk.inj hrun
                  subst hc'
                  have hargsEq : (childVal, restArgs) = args := Except.ok.inj hargs
                  subst hargsEq
                  have hmono₂ := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel).2
                    prodId rest c₁ hmono₁.2.1
                  rw [hpa] at hmono₂
                  obtain ⟨dL, hleL, hderL, hpredL⟩ :=
                    ihSym child {c with steps := c.steps + 1} c₁ childVal hoff hps
                  obtain ⟨dR, hleR, hderR, hpredR⟩ :=
                    ihArgs prodId rest c₁ c₂ restArgs hmono₁.2.1 hpa
                  have hderL' := derives_mono hmono₂.2.2.2.1 hderL
                  have hpredL' := predictive_mono hmono₂.2.2.2.1 hpredL
                  have hts : tokenSlice tokens c.offset c₂.offset =
                      tokenSlice tokens c.offset c₁.offset ++ tokenSlice tokens c₁.offset c₂.offset :=
                    (tokenSlice_append tokens hmono₁.1 hmono₂.1).symm
                  have hdrop : tokens.toList.drop c₁.offset =
                      tokenSlice tokens c₁.offset c₂.offset ++ tokens.toList.drop c₂.offset :=
                    (tokenSlice_append_drop tokens hmono₂.1).symm
                  rw [hts]
                  rw [hdrop] at hpredL'
                  refine ⟨dL + dR, ?_, .child hderL' hderR, .child hpredL' hpredR⟩
                  dsimp only at hmono₁ hmono₂ hleL ⊢
                  have hge₁ : c.steps + 1 ≤ c₁.steps := hmono₁.2.2.1
                  have hge₂ : c₁.steps ≤ c₂.steps := hmono₂.2.2.1
                  omega

private theorem parseSymbol_nullable_of_ok_zero {count : Nat} {g : SharedGrammar count}
    {table : CheckedTable g} {tokens : Array SurfaceToken} {targetSlice : Slice}
    {eof : FileSpan} {n : Fin count} {c c' : ParseCursor} {fuel : Nat} {val : g.value n}
    (hoff : c.offset ≤ tokens.size)
    (hrun : parseSymbol table tokens targetSlice eof n c fuel = (c', .ok val))
    (hzero : c'.offset = c.offset) :
    Nullable g.grammar n := by
  obtain ⟨_, _, hder, _⟩ :=
    (parseSymbol_parseArgs_sound_ok table tokens targetSlice eof fuel).1 n c c' val hoff hrun
  rw [hzero, tokenSlice_self] at hder
  exact derives_nullable_of_nil.1 hder

/-! ## Section 5: Linear work totality and exact step bound -/

private abbrev stepUnit {count : Nat} (g : SharedGrammar count) (maxRank : Nat) : Nat :=
  g.maxFrameCost + g.maxNonterminals * zeroTokenStepBound g.maxFrameCost g.maxNonterminals maxRank

private def SymStepBounded {count : Nat} {g : SharedGrammar count} (table : CheckedTable g)
    (tokens : Array SurfaceToken) (targetSlice : Slice) (eof : FileSpan) (maxRank : Nat)
    (n : Fin count) (c : ParseCursor) (fuel : Nat) : Prop :=
  let res := parseSymbol table tokens targetSlice eof n c fuel
  res.2 ≠ .error .exhausted ∧
  (res.1.offset = c.offset →
    res.1.steps - c.steps ≤ zeroTokenStepBound g.maxFrameCost g.maxNonterminals (table.rank n)) ∧
  (c.offset < res.1.offset →
    res.1.steps - c.steps ≤
      ((res.1.offset - c.offset - 1) * (maxRank + 1) + (table.rank n + 1)) * stepUnit g maxRank)

private theorem parseArgs_total_and_bounded {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) (tokens : Array SurfaceToken) (targetSlice : Slice)
    (eof : FileSpan) (maxRank : Nat)
    (hRank : ∀ m : Fin count, table.rank m ≤ maxRank)
    (p : Production g.value) (hp : p ∈ g.productions.toList)
    (c₀ : ParseCursor) (hc₀ : c₀.offset ≤ tokens.size)
    (ihSym : ∀ (m' : Nat),
      m' < (tokens.size - c₀.offset) * (maxRank + 1) + table.rank p.lhs →
      ∀ (n' : Fin count) (c' : ParseCursor) (fuel' : Nat),
        c'.offset ≤ tokens.size →
        (tokens.size - c'.offset) * (maxRank + 1) + table.rank n' = m' →
        (m' + 1) * g.maxFrameCost ≤ fuel' →
        SymStepBounded table tokens targetSlice eof maxRank n' c' fuel') :
    ∀ (rhs done : List (Sym Terminal (Fin count))) (c : ParseCursor) (fuel : Nat),
      p.rhs = done ++ rhs →
      c₀.offset ≤ c.offset →
      c.offset ≤ tokens.size →
      (c.offset = c₀.offset → NullableSeq g.grammar done) →
      (((tokens.size - c₀.offset) * (maxRank + 1) + table.rank p.lhs) + 1) * g.maxFrameCost ≤
        fuel + 1 + done.length →
      let out := parseArgs table tokens targetSlice eof p.id fuel rhs c
      out.2 ≠ .error .exhausted ∧
      (c.offset = c₀.offset → out.1.offset = c.offset →
        match table.rank p.lhs with
        | 0 => out.1.steps - c.steps ≤ rhs.length + 1
        | r' + 1 => out.1.steps - c.steps ≤
            (rhs.length + 1) + rhsNtCount rhs * zeroTokenStepBound g.maxFrameCost g.maxNonterminals r') ∧
      (c₀.offset < c.offset →
        out.1.steps - c.steps ≤
          (rhs.length + 1) + rhsNtCount rhs * zeroTokenStepBound g.maxFrameCost g.maxNonterminals maxRank +
            (out.1.offset - c.offset) * (maxRank + 1) * stepUnit g maxRank) ∧
      (c.offset = c₀.offset → c.offset < out.1.offset →
        out.1.steps - c.steps ≤
          (rhs.length + 1) + rhsNtCount rhs * zeroTokenStepBound g.maxFrameCost g.maxNonterminals maxRank +
            ((out.1.offset - c.offset - 1) * (maxRank + 1) + table.rank p.lhs) * stepUnit g maxRank) := by
  have hFge : p.rhs.length + 3 ≤ g.maxFrameCost :=
    Nat.le_trans (production_frameCost_ge p) (le_maxFrameCost hp)
  have hFm : g.maxFrameCost ≤
      (((tokens.size - c₀.offset) * (maxRank + 1) + table.rank p.lhs) + 1) * g.maxFrameCost :=
    Nat.le_mul_of_pos_left g.maxFrameCost (Nat.succ_pos _)
  intro rhs
  induction rhs with
  | nil =>
    intro done c fuel hrhs _ _ _ hfuel
    have hdoneLen : done.length = p.rhs.length := by
      have h := congrArg List.length hrhs
      simpa using h.symm
    cases fuel with
    | zero => exact False.elim (by omega)
    | succ fuel' =>
      unfold parseArgs
      refine ⟨(by intro h; cases h), ?_, ?_, ?_⟩
      · intro _ _
        cases table.rank p.lhs <;> simp
      · intro _
        simp
      · intro _ hlt
        dsimp only at hlt
        omega
  | cons sym rest ihRest =>
    intro done c fuel hrhs hc₀le hoff hnull hfuel
    have hdoneLt : done.length + 1 ≤ p.rhs.length := by
      have h := congrArg List.length hrhs
      simp only [List.length_append, List.length_cons] at h
      omega
    cases fuel with
    | zero => exact False.elim (by omega)
    | succ fuel' =>
      have hfuelNext :
          (((tokens.size - c₀.offset) * (maxRank + 1) + table.rank p.lhs) + 1) * g.maxFrameCost ≤
            fuel' + 1 + (done ++ [sym]).length := by
        simp only [List.length_append, List.length_singleton]
        omega
      cases sym with
      | t kind =>
        unfold parseArgs
        dsimp only
        cases htok : tokens[c.offset]? with
        | none =>
          refine ⟨(by intro h; cases h), ?_, ?_, ?_⟩
          · intro _ _
            cases table.rank p.lhs <;> (dsimp only; simp only [List.length_cons, rhsNtCount_cons_t]; omega)
          · intro _
            simp only [List.length_cons, rhsNtCount_cons_t]
            omega
          · intro _ hlt
            dsimp only at hlt
            omega
        | some token =>
          dsimp only
          split
          · refine ⟨(by intro h; cases h), ?_, ?_, ?_⟩
            · intro _ _
              cases table.rank p.lhs <;> (dsimp only; simp only [List.length_cons, rhsNtCount_cons_t]; omega)
            · intro _
              simp only [List.length_cons, rhsNtCount_cons_t]
              omega
            · intro _ hlt
              dsimp only at hlt
              omega
          · have hltTok : c.offset < tokens.size := (Array.getElem?_eq_some_iff.mp htok).1
            let c₁ := ({c with steps := c.steps + 1}).require targetSlice (g.required token.value)
              (tokenUnsupportedCause p.id token.value) token.span
            let c₂ := {c₁ with offset := c₁.offset + 1}
            have hrhs' : p.rhs = (done ++ [.t kind]) ++ rest := by
              simpa [List.append_assoc] using hrhs
            have hrest := ihRest (done ++ [.t kind]) c₂ fuel' hrhs' (by change c₀.offset ≤ c.offset + 1; omega) hltTok
              (fun heq => False.elim (by change c.offset + 1 = c₀.offset at heq; omega)) hfuelNext
            have hmono := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel').2
              p.id rest c₂ hltTok
            dsimp only at hrest hmono
            have hc₂Off : c₂.offset = c.offset + 1 := rfl
            have hc₂Steps : c₂.steps = c.steps + 1 := rfl
            have hstepGe : c.steps + 1 ≤ (parseArgs table tokens targetSlice eof p.id fuel' rest c₂).1.steps :=
              hc₂Steps ▸ hmono.2.2.1
            have hrestCase2 := hrest.2.2.1 (by change c₀.offset < c.offset + 1; omega)
            rw [hc₂Steps, hc₂Off] at hrestCase2
            have hoffGe : c.offset + 1 ≤ (parseArgs table tokens targetSlice eof p.id fuel' rest c₂).1.offset :=
              hc₂Off ▸ hmono.1
            dsimp only [c₂, c₁] at hrestCase2 hstepGe hoffGe
            refine ⟨?_, ?_, ?_, ?_⟩
            · intro hexh
              cases hres : (parseArgs table tokens targetSlice eof p.id fuel' rest c₂).2 with
              | error stop =>
                rw [hres] at hexh
                cases stop
                · cases hexh
                · exact hrest.1 hres
              | ok _ =>
                rw [hres] at hexh
                cases hexh
            · intro _ hzero
              dsimp only at hzero
              exact False.elim (by omega)
            · intro _
              have hmul : ((parseArgs table tokens targetSlice eof p.id fuel' rest c₂).1.offset - (c.offset + 1)) *
                  (maxRank + 1) * stepUnit g maxRank ≤
                  ((parseArgs table tokens targetSlice eof p.id fuel' rest c₂).1.offset - c.offset) *
                  (maxRank + 1) * stepUnit g maxRank :=
                Nat.mul_le_mul_right (stepUnit g maxRank) (Nat.mul_le_mul_right (maxRank + 1) (by dsimp only [c₂, c₁]; omega))
              dsimp only [c₂, c₁] at hmul ⊢
              simp only [List.length_cons, rhsNtCount_cons_t]
              omega
            · intro _ _
              have hsub : (parseArgs table tokens targetSlice eof p.id fuel' rest c₂).1.offset - (c.offset + 1) =
                  (parseArgs table tokens targetSlice eof p.id fuel' rest c₂).1.offset - c.offset - 1 := by
                dsimp only [c₂, c₁]; omega
              have hmul : ((parseArgs table tokens targetSlice eof p.id fuel' rest c₂).1.offset - (c.offset + 1)) *
                  (maxRank + 1) * stepUnit g maxRank ≤
                  (((parseArgs table tokens targetSlice eof p.id fuel' rest c₂).1.offset - c.offset - 1) *
                    (maxRank + 1) + table.rank p.lhs) * stepUnit g maxRank := by
                rw [hsub]
                exact Nat.mul_le_mul_right (stepUnit g maxRank) (Nat.le_add_right _ _)
              dsimp only [c₂, c₁] at hmul ⊢
              simp only [List.length_cons, rhsNtCount_cons_t]
              omega
      | n child =>
        unfold parseArgs
        dsimp only
        have hmChildLt : (tokens.size - c.offset) * (maxRank + 1) + table.rank child <
            (tokens.size - c₀.offset) * (maxRank + 1) + table.rank p.lhs := by
          by_cases hsame : c.offset = c₀.offset
          · have hrankLt : table.rank child < table.rank p.lhs :=
              table.acyclic p hp done child rest hrhs (hnull hsame)
            rw [hsame]
            omega
          · have hleR : table.rank child ≤ maxRank := hRank child
            have hmul : (tokens.size - c.offset + 1) * (maxRank + 1) ≤
                (tokens.size - c₀.offset) * (maxRank + 1) :=
              Nat.mul_le_mul_right (maxRank + 1) (by omega)
            rw [Nat.add_mul, Nat.one_mul] at hmul
            omega
        have hfuelChild : (((tokens.size - c.offset) * (maxRank + 1) + table.rank child) + 1) *
            g.maxFrameCost ≤ fuel' := by
          have h1 : (((tokens.size - c.offset) * (maxRank + 1) + table.rank child) + 1) * g.maxFrameCost ≤
              ((tokens.size - c₀.offset) * (maxRank + 1) + table.rank p.lhs) * g.maxFrameCost :=
            Nat.mul_le_mul_right g.maxFrameCost (by omega)
          have h2 : (((tokens.size - c₀.offset) * (maxRank + 1) + table.rank p.lhs) + 1) * g.maxFrameCost =
              ((tokens.size - c₀.offset) * (maxRank + 1) + table.rank p.lhs) * g.maxFrameCost + g.maxFrameCost := by
            rw [Nat.add_mul, Nat.one_mul]
          omega
        have hchildBnd := ihSym ((tokens.size - c.offset) * (maxRank + 1) + table.rank child) hmChildLt
          child {c with steps := c.steps + 1} fuel' hoff rfl hfuelChild
        have hchildMono := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel').1
          child {c with steps := c.steps + 1} hoff
        cases hps : parseSymbol table tokens targetSlice eof child {c with steps := c.steps + 1} fuel' with
        | mk c₁ res₁ =>
          dsimp only [SymStepBounded] at hchildBnd
          rw [hps] at hchildBnd hchildMono
          dsimp only at hchildBnd hchildMono
          have hc₁StepGe : c.steps + 1 ≤ c₁.steps := hchildMono.2.2.1
          have hZleMax : zeroTokenStepBound g.maxFrameCost g.maxNonterminals (table.rank child) ≤
              zeroTokenStepBound g.maxFrameCost g.maxNonterminals maxRank :=
            zeroTokenStepBound_mono g.maxFrameCost g.maxNonterminals (hRank child)
          cases res₁ with
          | error stop =>
            dsimp only
            refine ⟨(fun h => hchildBnd.1 (congrArg Except.error (Except.error.inj h))), ?_, ?_, ?_⟩
            · intro hsame hzero
              have hrankLt : table.rank child < table.rank p.lhs :=
                table.acyclic p hp done child rest hrhs (hnull hsame)
              cases hr : table.rank p.lhs with
              | zero => omega
              | succ r' =>
                have hcb := hchildBnd.2.1 hzero
                have hZle : zeroTokenStepBound g.maxFrameCost g.maxNonterminals (table.rank child) ≤
                    zeroTokenStepBound g.maxFrameCost g.maxNonterminals r' :=
                  zeroTokenStepBound_mono g.maxFrameCost g.maxNonterminals (by omega)
                simp only [List.length_cons, rhsNtCount_cons_n, Nat.add_mul, Nat.one_mul]
                omega
            · intro hgt
              by_cases hzero : c₁.offset = c.offset
              · have hcb := hchildBnd.2.1 hzero
                simp only [List.length_cons, rhsNtCount_cons_n, Nat.add_mul, Nat.one_mul]
                omega
              · have hlt : c.offset < c₁.offset := by omega
                have hcb := hchildBnd.2.2 hlt
                have hrankLe : table.rank child + 1 ≤ maxRank + 1 := Nat.succ_le_succ (hRank child)
                have hmul : ((c₁.offset - c.offset - 1) * (maxRank + 1) + (table.rank child + 1)) * stepUnit g maxRank ≤
                    (c₁.offset - c.offset) * (maxRank + 1) * stepUnit g maxRank := by
                  have hlin : (c₁.offset - c.offset - 1) * (maxRank + 1) + (table.rank child + 1) ≤
                      (c₁.offset - c.offset) * (maxRank + 1) := by
                    have hadd : (c₁.offset - c.offset) * (maxRank + 1) =
                        (c₁.offset - c.offset - 1) * (maxRank + 1) + (maxRank + 1) := by
                      have hsub : c₁.offset - c.offset = (c₁.offset - c.offset - 1) + 1 := by omega
                      conv => lhs; rw [hsub, Nat.add_mul, Nat.one_mul]
                    omega
                  exact Nat.mul_le_mul_right (stepUnit g maxRank) hlin
                simp only [List.length_cons, rhsNtCount_cons_n]
                omega
            · intro hsame hlt
              have hrankLt : table.rank child < table.rank p.lhs :=
                table.acyclic p hp done child rest hrhs (hnull hsame)
              have hcb := hchildBnd.2.2 hlt
              have hmul : ((c₁.offset - c.offset - 1) * (maxRank + 1) + (table.rank child + 1)) * stepUnit g maxRank ≤
                  ((c₁.offset - c.offset - 1) * (maxRank + 1) + table.rank p.lhs) * stepUnit g maxRank :=
                Nat.mul_le_mul_right (stepUnit g maxRank) (by omega)
              simp only [List.length_cons, rhsNtCount_cons_n]
              omega
          | ok childVal =>
            dsimp only
            have hrhs' : p.rhs = (done ++ [.n child]) ++ rest := by
              simpa [List.append_assoc] using hrhs
            have hnull' : c₁.offset = c₀.offset → NullableSeq g.grammar (done ++ [.n child]) := by
              intro hEq
              have hsame : c.offset = c₀.offset := by omega
              have hzero : c₁.offset = ({c with steps := c.steps + 1}).offset := by dsimp only; omega
              exact nullableSeq_append_single (hnull hsame)
                (parseSymbol_nullable_of_ok_zero (c := {c with steps := c.steps + 1}) hoff hps hzero)
            have hrest := ihRest (done ++ [.n child]) c₁ fuel' hrhs'
              (Nat.le_trans hc₀le hchildMono.1) hchildMono.2.1 hnull' hfuelNext
            have hrestMono := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel').2
              p.id rest c₁ hchildMono.2.1
            dsimp only at hrest hrestMono
            have hc₂StepGe : c₁.steps ≤ (parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.steps :=
              hrestMono.2.2.1
            have hc₂OffGe : c₁.offset ≤ (parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset :=
              hrestMono.1
            refine ⟨?_, ?_, ?_, ?_⟩
            · intro hexh
              cases hres₂ : (parseArgs table tokens targetSlice eof p.id fuel' rest c₁).2 with
              | error stop =>
                rw [hres₂] at hexh
                cases stop
                · cases hexh
                · exact hrest.1 hres₂
              | ok _ =>
                rw [hres₂] at hexh
                cases hexh
            · intro hsame hzero
              have hzero₁ : c₁.offset = c.offset := by omega
              have hzero₂ : (parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset = c₁.offset := by omega
              have hrankLt : table.rank child < table.rank p.lhs :=
                table.acyclic p hp done child rest hrhs (hnull hsame)
              have hcb := hchildBnd.2.1 hzero₁
              have hrb := hrest.2.1 (by omega) hzero₂
              cases hr : table.rank p.lhs with
              | zero => omega
              | succ r' =>
                rw [hr] at hrb
                have hZle : zeroTokenStepBound g.maxFrameCost g.maxNonterminals (table.rank child) ≤
                    zeroTokenStepBound g.maxFrameCost g.maxNonterminals r' :=
                  zeroTokenStepBound_mono g.maxFrameCost g.maxNonterminals (by omega)
                simp only [List.length_cons, rhsNtCount_cons_n, Nat.add_mul, Nat.one_mul]
                omega
            · intro hgt
              have hrb := hrest.2.2.1 (by omega)
              by_cases hzero₁ : c₁.offset = c.offset
              · have hcb := hchildBnd.2.1 hzero₁
                have hsub : (parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c₁.offset =
                    (parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c.offset := by omega
                rw [hsub] at hrb
                simp only [List.length_cons, rhsNtCount_cons_n, Nat.add_mul, Nat.one_mul]
                omega
              · have hlt₁ : c.offset < c₁.offset := by omega
                have hcb := hchildBnd.2.2 hlt₁
                have hmul₁ : ((c₁.offset - c.offset - 1) * (maxRank + 1) + (table.rank child + 1)) * stepUnit g maxRank ≤
                    (c₁.offset - c.offset) * (maxRank + 1) * stepUnit g maxRank := by
                  have hlin : (c₁.offset - c.offset - 1) * (maxRank + 1) + (table.rank child + 1) ≤
                      (c₁.offset - c.offset) * (maxRank + 1) := by
                    have hrankLe : table.rank child + 1 ≤ maxRank + 1 := Nat.succ_le_succ (hRank child)
                    have hadd : (c₁.offset - c.offset) * (maxRank + 1) =
                        (c₁.offset - c.offset - 1) * (maxRank + 1) + (maxRank + 1) := by
                      have hsub : c₁.offset - c.offset = (c₁.offset - c.offset - 1) + 1 := by omega
                      conv => lhs; rw [hsub, Nat.add_mul, Nat.one_mul]
                    omega
                  exact Nat.mul_le_mul_right (stepUnit g maxRank) hlin
                have hsumMul : (c₁.offset - c.offset) * (maxRank + 1) * stepUnit g maxRank +
                    ((parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c₁.offset) *
                      (maxRank + 1) * stepUnit g maxRank =
                    ((parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c.offset) *
                      (maxRank + 1) * stepUnit g maxRank := by
                  rw [← Nat.add_mul, ← Nat.add_mul]
                  have hadd : (c₁.offset - c.offset) +
                      ((parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c₁.offset) =
                      (parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c.offset := by
                    omega
                  rw [hadd]
                simp only [List.length_cons, rhsNtCount_cons_n, Nat.add_mul, Nat.one_mul]
                dsimp only [stepUnit] at hcb hrb hmul₁ hsumMul ⊢
                omega
            · intro hsame hlt
              have hrankLt : table.rank child < table.rank p.lhs :=
                table.acyclic p hp done child rest hrhs (hnull hsame)
              by_cases hzero₁ : c₁.offset = c.offset
              · have hcb := hchildBnd.2.1 hzero₁
                have hrb := hrest.2.2.2 (by omega) (by omega)
                have hsub : (parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c₁.offset =
                    (parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c.offset := by omega
                rw [hsub] at hrb
                rw [List.length_cons, rhsNtCount_cons_n, Nat.add_mul (rhsNtCount rest) 1, Nat.one_mul]
                omega
              · have hlt₁ : c.offset < c₁.offset := by omega
                have hcb := hchildBnd.2.2 hlt₁
                have hrb := hrest.2.2.1 (by omega)
                have hmul₁ : ((c₁.offset - c.offset - 1) * (maxRank + 1) + (table.rank child + 1)) * stepUnit g maxRank ≤
                    ((c₁.offset - c.offset - 1) * (maxRank + 1) + table.rank p.lhs) * stepUnit g maxRank :=
                  Nat.mul_le_mul_right (stepUnit g maxRank) (by omega)
                have hsumMul : ((c₁.offset - c.offset - 1) * (maxRank + 1) + table.rank p.lhs) * stepUnit g maxRank +
                    ((parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c₁.offset) *
                      (maxRank + 1) * stepUnit g maxRank =
                    (((parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c.offset - 1) *
                      (maxRank + 1) + table.rank p.lhs) * stepUnit g maxRank := by
                  rw [← Nat.add_mul]
                  congr 1
                  have hdist : (c₁.offset - c.offset - 1) * (maxRank + 1) +
                      ((parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c₁.offset) *
                        (maxRank + 1) =
                      ((parseArgs table tokens targetSlice eof p.id fuel' rest c₁).1.offset - c.offset - 1) *
                        (maxRank + 1) := by
                    rw [← Nat.add_mul]
                    congr 1
                    omega
                  omega
                rw [List.length_cons, rhsNtCount_cons_n, Nat.add_mul (rhsNtCount rest) 1, Nat.one_mul]
                omega

private theorem parseSymbol_total_and_bounded {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) (tokens : Array SurfaceToken) (targetSlice : Slice)
    (eof : FileSpan) (maxRank : Nat)
    (hRank : ∀ m : Fin count, table.rank m ≤ maxRank)
    (m : Nat) :
    ∀ (n : Fin count) (c : ParseCursor) (fuel : Nat),
      c.offset ≤ tokens.size →
      (tokens.size - c.offset) * (maxRank + 1) + table.rank n = m →
      (m + 1) * g.maxFrameCost ≤ fuel →
      SymStepBounded table tokens targetSlice eof maxRank n c fuel := by
  refine Nat.strongRecOn (motive := fun m =>
    ∀ (n : Fin count) (c : ParseCursor) (fuel : Nat),
      c.offset ≤ tokens.size →
      (tokens.size - c.offset) * (maxRank + 1) + table.rank n = m →
      (m + 1) * g.maxFrameCost ≤ fuel →
      SymStepBounded table tokens targetSlice eof maxRank n c fuel) m ?_
  intro m ih n c fuel hoff hm hfuel
  have hFpos : 1 ≤ g.maxFrameCost := one_le_maxFrameCost g
  have hfuelPos : 1 ≤ fuel := by
    have hm1 : g.maxFrameCost ≤ (m + 1) * g.maxFrameCost :=
      Nat.le_mul_of_pos_left g.maxFrameCost (Nat.succ_pos m)
    omega
  cases fuel with
  | zero => exact False.elim (by omega)
  | succ fuel' =>
    dsimp only [SymStepBounded]
    unfold parseSymbol
    dsimp only
    have h1leZ : 1 ≤ zeroTokenStepBound g.maxFrameCost g.maxNonterminals (table.rank n) := by
      have h0 : g.maxFrameCost ≤ zeroTokenStepBound g.maxFrameCost g.maxNonterminals (table.rank n) :=
        zeroTokenStepBound_mono g.maxFrameCost g.maxNonterminals (Nat.zero_le _)
      omega
    cases hlook : table.lookup n (tokens[c.offset]?.map fun t => t.value.kind) with
    | none =>
      exact ⟨(by intro h; cases h), fun _ => (by dsimp only; omega), fun hlt => (by dsimp only at hlt; omega)⟩
    | some index =>
      dsimp only
      cases hprod : g.productions[index]? with
      | none =>
        exact ⟨(by intro h; cases h), fun _ => (by dsimp only; omega), fun hlt => (by dsimp only at hlt; omega)⟩
      | some p =>
        dsimp only
        by_cases hlhs : p.lhs = n
        · simp only [hlhs, ↓reduceDIte]
          subst n
          have hpMem : p ∈ g.productions.toList :=
            Array.mem_toList_iff.mpr (Array.mem_of_getElem? hprod)
          have hFrameLe : p.frameCost ≤ g.maxFrameCost := le_maxFrameCost hpMem
          have hNtLe : p.nonterminalCount ≤ g.maxNonterminals := le_maxNonterminals hpMem
          let c₁ := ({c with steps := c.steps + 1}).require targetSlice p.firstSlice
            (.production p.id) ((tokens[c.offset]?.map Located.span).getD eof)
          have hargs := parseArgs_total_and_bounded table tokens targetSlice eof maxRank hRank
            p hpMem c hoff (hm ▸ ih) p.rhs [] c₁ fuel' rfl (Nat.le_refl _) hoff
            (fun _ => nullableSeq_nil g.grammar) (by simpa [hm] using hfuel)
          have hargsMono := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel').2
            p.id p.rhs c₁ hoff
          cases hpa : parseArgs table tokens targetSlice eof p.id fuel' p.rhs
              (({c with steps := c.steps + 1}).require targetSlice p.firstSlice
                (.production p.id) ((tokens[c.offset]?.map Located.span).getD eof)) with
          | mk c₂ res =>
            dsimp only [c₁] at hargs hargsMono
            rw [hpa] at hargs hargsMono
            dsimp only [ParseCursor.require] at hargs hargsMono
            have hc₂StepGe : c.steps + 1 ≤ c₂.steps := hargsMono.2.2.1
            have hntEq : rhsNtCount p.rhs = p.nonterminalCount := by
              unfold rhsNtCount Production.nonterminalCount
              induction p.rhs with
              | nil => rfl
              | cons sym rest ih => cases sym <;> simp [ih]
            cases res with
            | error stop =>
              refine ⟨(fun h => hargs.1 (congrArg Except.error (Except.error.inj h))), ?_, ?_⟩
              · intro hzero
                have hzb := hargs.2.1 rfl hzero
                cases hr : table.rank p.lhs with
                | zero =>
                  rw [hr] at hzb
                  change c₂.steps - c.steps ≤ g.maxFrameCost
                  unfold Production.frameCost at hFrameLe
                  omega
                | succ r' =>
                  rw [hr] at hzb
                  change c₂.steps - c.steps ≤
                    g.maxFrameCost + g.maxNonterminals * zeroTokenStepBound g.maxFrameCost g.maxNonterminals r'
                  have hmul : p.nonterminalCount * zeroTokenStepBound g.maxFrameCost g.maxNonterminals r' ≤
                      g.maxNonterminals * zeroTokenStepBound g.maxFrameCost g.maxNonterminals r' :=
                    Nat.mul_le_mul_right _ hNtLe
                  unfold Production.frameCost at hFrameLe
                  rw [hntEq] at hzb
                  omega
              · intro hlt
                have hcb := hargs.2.2.2 rfl hlt
                have hmul : p.nonterminalCount * zeroTokenStepBound g.maxFrameCost g.maxNonterminals maxRank ≤
                    g.maxNonterminals * zeroTokenStepBound g.maxFrameCost g.maxNonterminals maxRank :=
                  Nat.mul_le_mul_right _ hNtLe
                have hCadd : stepUnit g maxRank +
                    ((c₂.offset - c.offset - 1) * (maxRank + 1) + table.rank p.lhs) * stepUnit g maxRank =
                    ((c₂.offset - c.offset - 1) * (maxRank + 1) + (table.rank p.lhs + 1)) * stepUnit g maxRank := by
                  rw [← Nat.add_assoc, Nat.add_mul ((c₂.offset - c.offset - 1) * (maxRank + 1) + table.rank p.lhs) 1,
                    Nat.one_mul, Nat.add_comm]
                unfold Production.frameCost at hFrameLe
                rw [hntEq] at hcb
                dsimp only [stepUnit] at hCadd hcb ⊢
                omega
            | ok args =>
              have hcost : (p.action.eval args).1 = p.action.cost := Action.eval_cost args p.action
              refine ⟨(by intro h; cases h), ?_, ?_⟩
              · intro hzero
                have hzb := hargs.2.1 rfl hzero
                cases hr : table.rank p.lhs with
                | zero =>
                  rw [hr] at hzb
                  change c₂.steps + (p.action.eval args).1 - c.steps ≤ g.maxFrameCost
                  unfold Production.frameCost at hFrameLe
                  omega
                | succ r' =>
                  rw [hr] at hzb
                  change c₂.steps + (p.action.eval args).1 - c.steps ≤
                    g.maxFrameCost + g.maxNonterminals * zeroTokenStepBound g.maxFrameCost g.maxNonterminals r'
                  have hmul : p.nonterminalCount * zeroTokenStepBound g.maxFrameCost g.maxNonterminals r' ≤
                      g.maxNonterminals * zeroTokenStepBound g.maxFrameCost g.maxNonterminals r' :=
                    Nat.mul_le_mul_right _ hNtLe
                  unfold Production.frameCost at hFrameLe
                  rw [hntEq] at hzb
                  omega
              · intro hlt
                have hcb := hargs.2.2.2 rfl hlt
                have hmul : p.nonterminalCount * zeroTokenStepBound g.maxFrameCost g.maxNonterminals maxRank ≤
                    g.maxNonterminals * zeroTokenStepBound g.maxFrameCost g.maxNonterminals maxRank :=
                  Nat.mul_le_mul_right _ hNtLe
                have hCadd : stepUnit g maxRank +
                    ((c₂.offset - c.offset - 1) * (maxRank + 1) + table.rank p.lhs) * stepUnit g maxRank =
                    ((c₂.offset - c.offset - 1) * (maxRank + 1) + (table.rank p.lhs + 1)) * stepUnit g maxRank := by
                  rw [← Nat.add_assoc, Nat.add_mul ((c₂.offset - c.offset - 1) * (maxRank + 1) + table.rank p.lhs) 1,
                    Nat.one_mul, Nat.add_comm]
                unfold Production.frameCost at hFrameLe
                rw [hntEq] at hcb
                dsimp only [stepUnit] at hCadd hcb ⊢
                omega
        · simp only [hlhs, ↓reduceDIte]
          exact ⟨(by intro h; cases h), fun _ => (by omega), fun hlt => (by omega)⟩

private theorem start_fuel_sufficient {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (maxRank : Nat)
    (hRank : ∀ n : Fin count, parser.table.rank n ≤ maxRank)
    (hSteps : linearStepsPerToken g.maxFrameCost g.maxNonterminals maxRank ≤ parser.stepsPerToken)
    (nTokens : Nat) :
    ((nTokens * (maxRank + 1) + parser.table.rank g.start) + 1) * g.maxFrameCost ≤
      parser.stepsPerToken * (nTokens + 1) := by
  have h1 : nTokens * (maxRank + 1) + parser.table.rank g.start + 1 ≤ (nTokens + 1) * (maxRank + 1) := by
    have hR := hRank g.start
    rw [Nat.add_mul, Nat.one_mul]
    omega
  have h2 : ((nTokens * (maxRank + 1) + parser.table.rank g.start) + 1) * g.maxFrameCost ≤
      (nTokens + 1) * (maxRank + 1) * g.maxFrameCost :=
    Nat.mul_le_mul_right g.maxFrameCost h1
  have h3 : (maxRank + 1) * g.maxFrameCost ≤
      linearStepsPerToken g.maxFrameCost g.maxNonterminals maxRank := by
    unfold linearStepsPerToken
    have hFC : g.maxFrameCost ≤ stepUnit g maxRank := Nat.le_add_right _ _
    exact Nat.le_trans (Nat.mul_le_mul_left (maxRank + 1) hFC) (Nat.le_succ _)
  have h4 : (nTokens + 1) * ((maxRank + 1) * g.maxFrameCost) ≤ (nTokens + 1) * parser.stepsPerToken :=
    Nat.mul_le_mul_left (nTokens + 1) (Nat.le_trans h3 hSteps)
  rw [Nat.mul_assoc] at h2
  rw [Nat.mul_comm parser.stepsPerToken (nTokens + 1)]
  exact Nat.le_trans h2 h4

theorem parserEngine_nonExhausted {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (maxRank : Nat)
    (hRank : ∀ n : Fin count, parser.table.rank n ≤ maxRank)
    (hSteps : linearStepsPerToken g.maxFrameCost g.maxNonterminals maxRank ≤ parser.stepsPerToken)
    (input : ParserView) :
    (parser.run input).2.isSome = true := by
  unfold ParserEngine.run
  cases hinv : input.invalidLocation with
  | some bad => rfl
  | none =>
    dsimp only
    have hfuel : ((input.tokens.size - 0) * (maxRank + 1) + parser.table.rank g.start + 1) * g.maxFrameCost ≤
        parser.stepsPerToken * (input.tokens.size + 1) := by
      simpa using start_fuel_sufficient parser maxRank hRank hSteps input.tokens.size
    have hbnd := parseSymbol_total_and_bounded parser.table input.tokens input.source.slice
      (eofFileSpan input.source) maxRank hRank
      ((input.tokens.size - 0) * (maxRank + 1) + parser.table.rank g.start)
      g.start ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))
      (Nat.zero_le _) rfl hfuel
    cases hps : parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) with
    | mk c val =>
      dsimp only [SymStepBounded] at hbnd
      rw [hps] at hbnd
      cases val with
      | error stop =>
        cases stop with
        | exhausted => exact False.elim (hbnd.1 rfl)
        | malformed => rfl
      | ok ast =>
        dsimp only
        split
        · rfl
        · cases c.firstUnsupportedHit <;> rfl

theorem parserEngine_steps_le {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (maxRank : Nat)
    (hRank : ∀ n : Fin count, parser.table.rank n ≤ maxRank)
    (hSteps : linearStepsPerToken g.maxFrameCost g.maxNonterminals maxRank ≤ parser.stepsPerToken)
    (input : ParserView) :
    (parser.run input).1 ≤ parser.stepsPerToken * (input.tokens.size + 1) := by
  have hLge1 : 1 ≤ parser.stepsPerToken := by
    have h1 : 1 ≤ linearStepsPerToken g.maxFrameCost g.maxNonterminals maxRank := by
      unfold linearStepsPerToken
      exact Nat.le_add_left 1 _
    exact Nat.le_trans h1 hSteps
  unfold ParserEngine.run
  cases hinv : input.invalidLocation with
  | some bad =>
    dsimp only
    exact Nat.le_trans hLge1 (Nat.le_mul_of_pos_right parser.stepsPerToken (Nat.succ_pos _))
  | none =>
    dsimp only
    have hfuel : ((input.tokens.size - 0) * (maxRank + 1) + parser.table.rank g.start + 1) * g.maxFrameCost ≤
        parser.stepsPerToken * (input.tokens.size + 1) := by
      simpa using start_fuel_sufficient parser maxRank hRank hSteps input.tokens.size
    have hbnd := parseSymbol_total_and_bounded parser.table input.tokens input.source.slice
      (eofFileSpan input.source) maxRank hRank
      ((input.tokens.size - 0) * (maxRank + 1) + parser.table.rank g.start)
      g.start ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))
      (Nat.zero_le _) rfl hfuel
    have hmono := (parseSymbol_parseArgs_mono parser.table input.tokens input.source.slice
      (eofFileSpan input.source) (parser.stepsPerToken * (input.tokens.size + 1))).1
      g.start ⟨0, slice0, none, 0⟩ (Nat.zero_le _)
    cases hps : parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) with
    | mk c val =>
      dsimp only [SymStepBounded] at hbnd
      rw [hps] at hbnd hmono
      dsimp only at hbnd hmono
      have hcSteps : c.steps + 1 ≤ parser.stepsPerToken * (input.tokens.size + 1) := by
        have hstepBnd : c.steps ≤ (maxRank + 1) * stepUnit g maxRank * (input.tokens.size + 1) := by
          by_cases hzero : c.offset = 0
          · have hzb := hbnd.2.1 hzero
            have hZle : zeroTokenStepBound g.maxFrameCost g.maxNonterminals (parser.table.rank g.start) ≤
                stepUnit g maxRank :=
              Nat.le_trans (zeroTokenStepBound_mono g.maxFrameCost g.maxNonterminals (hRank g.start))
                (zeroTokenStepBound_le_unit g.maxFrameCost g.maxNonterminals maxRank)
            have hCle : stepUnit g maxRank ≤ (maxRank + 1) * stepUnit g maxRank :=
              Nat.le_mul_of_pos_left _ (Nat.succ_pos _)
            have hCleN : (maxRank + 1) * stepUnit g maxRank ≤
                (maxRank + 1) * stepUnit g maxRank * (input.tokens.size + 1) :=
              Nat.le_mul_of_pos_right _ (Nat.succ_pos _)
            omega
          · have hlt : 0 < c.offset := by omega
            have hcb := hbnd.2.2 hlt
            simp only [Nat.sub_zero] at hcb
            have hlin : (c.offset - 1) * (maxRank + 1) + (parser.table.rank g.start + 1) ≤
                c.offset * (maxRank + 1) := by
              have hR := hRank g.start
              have hadd : c.offset * (maxRank + 1) = (c.offset - 1) * (maxRank + 1) + (maxRank + 1) := by
                have hsub : c.offset = (c.offset - 1) + 1 := by omega
                conv => lhs; rw [hsub, Nat.add_mul, Nat.one_mul]
              omega
            have hmul1 : ((c.offset - 1) * (maxRank + 1) + (parser.table.rank g.start + 1)) * stepUnit g maxRank ≤
                c.offset * (maxRank + 1) * stepUnit g maxRank :=
              Nat.mul_le_mul_right (stepUnit g maxRank) hlin
            have hmul2 : c.offset * (maxRank + 1) * stepUnit g maxRank ≤
                (input.tokens.size + 1) * ((maxRank + 1) * stepUnit g maxRank) := by
              rw [Nat.mul_assoc]
              exact Nat.mul_le_mul_right ((maxRank + 1) * stepUnit g maxRank)
                (Nat.le_trans hmono.2.1 (Nat.le_succ _))
            rw [Nat.mul_comm (input.tokens.size + 1) ((maxRank + 1) * stepUnit g maxRank)] at hmul2
            omega
        have hLmul : ((maxRank + 1) * stepUnit g maxRank + 1) * (input.tokens.size + 1) ≤
            parser.stepsPerToken * (input.tokens.size + 1) := by
          unfold linearStepsPerToken at hSteps
          exact Nat.mul_le_mul_right (input.tokens.size + 1) hSteps
        rw [Nat.add_mul, Nat.one_mul] at hLmul
        dsimp only [stepUnit] at hstepBnd hLmul
        omega
      cases val with
      | error stop =>
        cases stop <;> exact hcSteps
      | ok ast =>
        dsimp only
        split
        · exact hcSteps
        · cases c.firstUnsupportedHit <;> exact hcSteps

/-! ## Section 6: Predictive completeness and soundness/completeness of `ParserEngine.run` -/

private theorem parseSymbol_of_predictive {count : Nat} {g : SharedGrammar count}
    {table : CheckedTable g} {tokens : Array SurfaceToken} {targetSlice : Slice}
    {eof : FileSpan} {slice : Slice} {n : Fin count}
    {head rest : List SurfaceToken} {val : g.value n} {dSteps : Nat}
    (hpred : Predictive table slice n head rest val dSteps) :
    ∀ (c : ParseCursor) (fuel : Nat),
      c.offset ≤ tokens.size →
      tokens.toList.drop c.offset = head ++ rest →
      c.required.val ≤ slice.val →
      (parseSymbol table tokens targetSlice eof n c fuel).2 ≠ .error .exhausted →
      let out := parseSymbol table tokens targetSlice eof n c fuel
      out.2 = .ok val ∧ out.1.offset = c.offset + head.length ∧ out.1.required.val ≤ slice.val :=
  Predictive.rec
    (motive_1 := fun n head rest val _ _ =>
      ∀ (c : ParseCursor) (fuel : Nat),
        c.offset ≤ tokens.size →
        tokens.toList.drop c.offset = head ++ rest →
        c.required.val ≤ slice.val →
        (parseSymbol table tokens targetSlice eof n c fuel).2 ≠ .error .exhausted →
        let out := parseSymbol table tokens targetSlice eof n c fuel
        out.2 = .ok val ∧ out.1.offset = c.offset + head.length ∧ out.1.required.val ≤ slice.val)
    (motive_2 := fun rhs head rest args _ _ =>
      ∀ (prodId : ProductionId) (c : ParseCursor) (fuel : Nat),
        c.offset ≤ tokens.size →
        tokens.toList.drop c.offset = head ++ rest →
        c.required.val ≤ slice.val →
        (parseArgs table tokens targetSlice eof prodId fuel rhs c).2 ≠ .error .exhausted →
        let out := parseArgs table tokens targetSlice eof prodId fuel rhs c
        out.2 = .ok args ∧ out.1.offset = c.offset + head.length ∧ out.1.required.val ≤ slice.val)
    (fun p idx toks rest args _ hget hlook hslice _ ihArgs c fuel hoff hdrop hreq hne => by
      cases fuel with
      | zero => exact False.elim (hne rfl)
      | succ fuel' =>
        have hlook' : table.lookup p.lhs (tokens[c.offset]?.map fun t => t.value.kind) = some idx := by
          rw [getElem?_eq_drop_head?, hdrop]
          exact hlook
        let c₁ := ({c with steps := c.steps + 1}).require targetSlice p.firstSlice
          (.production p.id) ((tokens[c.offset]?.map Located.span).getD eof)
        have hc₁Req : c₁.required.val ≤ slice.val := by
          rw [require_required_val]
          exact Nat.max_le.mpr ⟨hreq, hslice⟩
        have hneArgs : (parseArgs table tokens targetSlice eof p.id fuel' p.rhs c₁).2 ≠ .error .exhausted := by
          intro hexh
          apply hne
          unfold parseSymbol
          simp only [hlook', hget, ↓reduceDIte]
          cases hpa : parseArgs table tokens targetSlice eof p.id fuel' p.rhs c₁ with
          | mk c₂ res =>
            rw [hpa] at hexh
            dsimp only at hexh
            rw [hexh]
        have hargsRes := ihArgs p.id c₁ fuel' hoff hdrop hc₁Req hneArgs
        unfold parseSymbol
        simp only [hlook', hget, ↓reduceDIte]
        cases hpa : parseArgs table tokens targetSlice eof p.id fuel' p.rhs c₁ with
        | mk c₂ res =>
          rw [hpa] at hargsRes
          obtain ⟨hres, hoff₂, hreq₂⟩ := hargsRes
          dsimp only at hres hoff₂ hreq₂
          rw [hres]
          exact ⟨rfl, hoff₂, hreq₂⟩)
    (fun rest prodId c fuel _ _ hreq hne => by
      cases fuel with
      | zero => exact False.elim (hne rfl)
      | succ fuel' =>
        unfold parseArgs
        exact ⟨rfl, by simp, hreq⟩)
    (fun {kind tok rhs toks rest args _} hkind htokReq _ ihArgs prodId c fuel hoff hdrop hreq hne => by
      cases fuel with
      | zero => exact False.elim (hne rfl)
      | succ fuel' =>
        have hgetTok : tokens[c.offset]? = some tok := by
          rw [getElem?_eq_drop_head?, hdrop]
          rfl
        have hlt : c.offset < tokens.size := (Array.getElem?_eq_some_iff.mp hgetTok).1
        have hnotNe : ¬ (tok.value.kind ≠ kind) := fun h => h hkind
        let c₁ := ({c with steps := c.steps + 1}).require targetSlice (g.required tok.value)
          (tokenUnsupportedCause prodId tok.value) tok.span
        let c₂ := {c₁ with offset := c₁.offset + 1}
        have hdrop' : tokens.toList.drop c₂.offset = toks ++ rest := by
          change tokens.toList.drop (c.offset + 1) = toks ++ rest
          have hltL : c.offset < tokens.toList.length := by simpa [Array.length_toList] using hlt
          have hstep := List.drop_eq_getElem_cons hltL
          rw [hdrop] at hstep
          exact (List.cons.inj hstep).2.symm
        have hc₂Req : c₂.required.val ≤ slice.val := by
          change c₁.required.val ≤ slice.val
          rw [require_required_val]
          exact Nat.max_le.mpr ⟨hreq, htokReq⟩
        have hneRest : (parseArgs table tokens targetSlice eof prodId fuel' rhs c₂).2 ≠ .error .exhausted := by
          intro hexh
          apply hne
          unfold parseArgs
          simp only [hgetTok, hnotNe, ↓reduceIte]
          rw [hexh]
          rfl
        have hrestRes := ihArgs prodId c₂ fuel' hlt hdrop' hc₂Req hneRest
        unfold parseArgs
        simp only [hgetTok, hnotNe, ↓reduceIte]
        cases hpa : parseArgs table tokens targetSlice eof prodId fuel' rhs c₂ with
        | mk c₃ res =>
          rw [hpa] at hrestRes
          obtain ⟨hres, hoff₃, hreq₃⟩ := hrestRes
          dsimp only at hres hoff₃ hreq₃
          rw [hres]
          refine ⟨rfl, ?_, hreq₃⟩
          change c₃.offset = c.offset + (toks.length + 1)
          have hc₂Off : c₂.offset = c.offset + 1 := rfl
          omega)
    (fun {child rhs h1 h2 rest childVal restArgs _ _} _ _ ihL ihR prodId c fuel hoff hdrop hreq hne => by
      cases fuel with
      | zero => exact False.elim (hne rfl)
      | succ fuel' =>
        have hdrop₁ : tokens.toList.drop c.offset = h1 ++ (h2 ++ rest) := by
          simpa [List.append_assoc] using hdrop
        have hneChild : (parseSymbol table tokens targetSlice eof child {c with steps := c.steps + 1} fuel').2 ≠
            .error .exhausted := by
          intro hexh
          apply hne
          unfold parseArgs
          dsimp only
          cases hps : parseSymbol table tokens targetSlice eof child {c with steps := c.steps + 1} fuel' with
          | mk c₁ res₁ =>
            rw [hps] at hexh
            dsimp only at hexh
            rw [hexh]
        have hchildRes := ihL {c with steps := c.steps + 1} fuel' hoff hdrop₁ hreq hneChild
        have hchildMono := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel').1
          child {c with steps := c.steps + 1} hoff
        cases hps : parseSymbol table tokens targetSlice eof child {c with steps := c.steps + 1} fuel' with
        | mk c₁ res₁ =>
          rw [hps] at hchildRes hchildMono
          obtain ⟨hres₁, hoff₁, hreq₁⟩ := hchildRes
          dsimp only at hres₁ hoff₁ hreq₁
          have hdrop₂ : tokens.toList.drop c₁.offset = h2 ++ rest := by
            rw [hoff₁, ← List.drop_drop, hdrop₁, List.drop_left]
          have hneRest : (parseArgs table tokens targetSlice eof prodId fuel' rhs c₁).2 ≠ .error .exhausted := by
            intro hexh
            apply hne
            unfold parseArgs
            simp only [hps, hres₁]
            rw [hexh]
            rfl
          have hrestRes := ihR prodId c₁ fuel' hchildMono.2.1 hdrop₂ hreq₁ hneRest
          unfold parseArgs
          simp only [hps, hres₁]
          cases hpa : parseArgs table tokens targetSlice eof prodId fuel' rhs c₁ with
          | mk c₂ res₂ =>
            rw [hpa] at hrestRes
            obtain ⟨hres₂, hoff₂, hreq₂⟩ := hrestRes
            dsimp only at hres₂ hoff₂ hreq₂
            rw [hres₂]
            refine ⟨rfl, ?_, hreq₂⟩
            rw [hoff₂, hoff₁, List.length_append, Nat.add_assoc])
    hpred

theorem parserEngine_ok_sound {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (input : ParserView)
    (output : Parsed (g.value g.start))
    (hrun : (parser.run input).2 = some (.ok output)) :
    ParsesWith parser input output := by
  unfold ParserEngine.run at hrun
  cases hinv : input.invalidLocation with
  | some bad => simp [hinv] at hrun
  | none =>
    simp only [hinv] at hrun
    have hhit := parseSymbol_firstUnsupportedHit_isSome parser.table input.tokens
      input.source.slice (eofFileSpan input.source) g.start ⟨0, slice0, none, 0⟩
      (parser.stepsPerToken * (input.tokens.size + 1)) (by simp [slice0, Slice.val])
    cases hps : parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) with
    | mk c val =>
      simp only [hps] at hrun hhit
      cases val with
      | error stop => cases stop <;> simp at hrun
      | ok ast =>
        by_cases hoff : c.offset = input.tokens.size
        · have hbne : (c.offset != input.tokens.size) = false := by simp [hoff]
          simp only [hbne, Bool.false_eq_true, ↓reduceIte] at hrun
          cases hfirst : c.firstUnsupportedHit with
          | some hit => simp [hfirst] at hrun
          | none =>
            simp only [hfirst, Option.some.injEq, Except.ok.injEq] at hrun
            subst output
            rw [hfirst] at hhit
            have hleSlice : c.required.val ≤ input.source.slice.val := by
              have hfalse : decide (input.source.slice.val < c.required.val) = false := hhit.symm
              exact Nat.le_of_not_lt (of_decide_eq_false hfalse)
            obtain ⟨dSteps, hleSteps, hder, hpred⟩ :=
              (parseSymbol_parseArgs_sound_ok parser.table input.tokens input.source.slice
                (eofFileSpan input.source) (parser.stepsPerToken * (input.tokens.size + 1))).1
                g.start ⟨0, slice0, none, 0⟩ c ast (Nat.zero_le _) hps
            rw [hoff, tokenSlice_full] at hder hpred
            rw [List.drop_eq_nil_of_le (by simp)] at hpred
            have hrunSteps : (parser.run input).1 = c.steps + 1 := by
              unfold ParserEngine.run
              simp only [hinv, hps, hbne, Bool.false_eq_true, ↓reduceIte, hfirst]
            refine ⟨rfl, dSteps, derives_mono hleSlice hder, predictive_mono hleSlice hpred, ?_, hinv⟩
            rw [hrunSteps]
            dsimp only at hleSteps
            omega
        · have hbne : (c.offset != input.tokens.size) = true := bne_iff_ne.mpr hoff
          simp [hbne] at hrun

theorem parserEngine_error_sound {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (input : ParserView)
    (refusal : ParseRefusal)
    (hrun : (parser.run input).2 = some (.error refusal)) :
    Refuses g input refusal := by
  unfold ParserEngine.run at hrun
  cases hinv : input.invalidLocation with
  | some bad =>
    simp only [hinv, Option.some.injEq, Except.error.injEq] at hrun
    subst refusal
    exact hinv
  | none =>
    simp only [hinv] at hrun
    have hhit := parseSymbol_firstUnsupportedHit_isSome parser.table input.tokens
      input.source.slice (eofFileSpan input.source) g.start ⟨0, slice0, none, 0⟩
      (parser.stepsPerToken * (input.tokens.size + 1)) (by simp [slice0, Slice.val])
    cases hps : parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) with
    | mk c val =>
      simp only [hps] at hrun hhit
      cases val with
      | error stop =>
        cases stop with
        | exhausted => simp at hrun
        | malformed =>
          simp only [Option.some.injEq, Except.error.injEq] at hrun
          subst refusal
          rintro ⟨tree, req, steps, hder⟩
          have hpred := derives_to_predictive parser.table hder
          have hne : (parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
              ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))).2 ≠ .error .exhausted := by
            rw [hps]; intro h; cases h
          have hres := parseSymbol_of_predictive hpred ⟨0, slice0, none, 0⟩
            (parser.stepsPerToken * (input.tokens.size + 1))
            (Nat.zero_le _) (by simp) (Nat.zero_le _) hne
          rw [hps] at hres
          cases hres.1
      | ok ast =>
        by_cases hoff : c.offset = input.tokens.size
        · have hbne : (c.offset != input.tokens.size) = false := by simp [hoff]
          simp only [hbne, Bool.false_eq_true, ↓reduceIte] at hrun
          cases hfirst : c.firstUnsupportedHit with
          | none => simp [hfirst] at hrun
          | some hit =>
            simp only [hfirst, Option.some.injEq, Except.error.injEq] at hrun
            subst refusal
            rw [hfirst] at hhit
            have hltSlice : input.source.slice.val < c.required.val :=
              of_decide_eq_true hhit.symm
            obtain ⟨dSteps, _, hder, _⟩ :=
              (parseSymbol_parseArgs_sound_ok parser.table input.tokens input.source.slice
                (eofFileSpan input.source) (parser.stepsPerToken * (input.tokens.size + 1))).1
                g.start ⟨0, slice0, none, 0⟩ c ast (Nat.zero_le _) hps
            rw [hoff, tokenSlice_full] at hder
            refine ⟨hltSlice, ⟨ast, dSteps, hder⟩, ?_⟩
            intro earlier hltEarlier
            rintro ⟨tree, steps, hderEarlier⟩
            have hpred := derives_to_predictive parser.table hderEarlier
            have hne : (parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
                ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))).2 ≠ .error .exhausted := by
              rw [hps]; intro h; cases h
            have hres := parseSymbol_of_predictive hpred ⟨0, slice0, none, 0⟩
              (parser.stepsPerToken * (input.tokens.size + 1))
              (Nat.zero_le _) (by simp) (Nat.zero_le _) hne
            rw [hps] at hres
            dsimp only at hres
            omega
        · have hbne : (c.offset != input.tokens.size) = true := bne_iff_ne.mpr hoff
          simp only [hbne, ↓reduceIte, Option.some.injEq, Except.error.injEq] at hrun
          subst refusal
          rintro ⟨tree, req, steps, hder⟩
          have hpred := derives_to_predictive parser.table hder
          have hne : (parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
              ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))).2 ≠ .error .exhausted := by
            rw [hps]; intro h; cases h
          have hres := parseSymbol_of_predictive hpred ⟨0, slice0, none, 0⟩
            (parser.stepsPerToken * (input.tokens.size + 1))
            (Nat.zero_le _) (by simp) (Nat.zero_le _) hne
          rw [hps] at hres
          have hlen : c.offset = input.tokens.size := by
            simpa [Array.length_toList] using hres.2.1
          exact hoff hlen

theorem parserEngine_invalidStream_iff {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (input : ParserView)
    (span : FileSpan) (reason : LocationRefusal) :
    (parser.run input).2 = some (.error (.invalidStream span reason)) ↔
      input.invalidLocation = some (span, reason) := by
  constructor
  · exact parserEngine_error_sound parser input (.invalidStream span reason)
  · intro hinv
    unfold ParserEngine.run
    simp only [hinv]

theorem parserEngine_ok_derives_iff {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (maxRank : Nat)
    (hRank : ∀ n : Fin count, parser.table.rank n ≤ maxRank)
    (hSteps : linearStepsPerToken g.maxFrameCost g.maxNonterminals maxRank ≤ parser.stepsPerToken)
    (input : ParserView) (tree : g.value g.start) :
    (parser.run input).2 = some (.ok ⟨input.source, tree⟩) ↔
      input.WF ∧ ∃ steps, Derives g input.source.slice g.start input.tokens.toList tree steps := by
  constructor
  · intro hrun
    obtain ⟨_, steps, hder, _, _, hwf⟩ := parserEngine_ok_sound parser input ⟨input.source, tree⟩ hrun
    exact ⟨hwf, steps, hder⟩
  · rintro ⟨hwf, steps, hder⟩
    unfold ParserView.WF at hwf
    have hnonEx := parserEngine_nonExhausted parser maxRank hRank hSteps input
    unfold ParserEngine.run at hnonEx ⊢
    rw [hwf] at hnonEx ⊢
    dsimp only at hnonEx ⊢
    have hhit := parseSymbol_firstUnsupportedHit_isSome parser.table input.tokens
      input.source.slice (eofFileSpan input.source) g.start ⟨0, slice0, none, 0⟩
      (parser.stepsPerToken * (input.tokens.size + 1)) (by simp [slice0, Slice.val])
    cases hps : parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) with
    | mk c val =>
      rw [hps] at hnonEx hhit
      dsimp only at hnonEx hhit ⊢
      have hne : (parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
          ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))).2 ≠ .error .exhausted := by
        rw [hps]
        intro hexh
        dsimp only at hexh
        rw [hexh] at hnonEx
        cases hnonEx
      have hres := parseSymbol_of_predictive (derives_to_predictive parser.table hder)
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))
        (Nat.zero_le _) (by simp) (Nat.zero_le _) hne
      rw [hps] at hres
      obtain ⟨hval, hoff, hreq⟩ := hres
      dsimp only at hval hoff hreq
      rw [hval]
      have hoff' : c.offset = input.tokens.size := by simpa [Array.length_toList] using hoff
      have hbne : (c.offset != input.tokens.size) = false := by simp [hoff']
      simp only [hbne, Bool.false_eq_true, ↓reduceIte]
      have hnoHit : c.firstUnsupportedHit = none := by
        have hnotLt : ¬ (input.source.slice.val < c.required.val) := Nat.not_lt.mpr hreq
        have hfalse : c.firstUnsupportedHit.isSome = false := by
          rw [hhit]
          exact decide_eq_false hnotLt
        cases hfh : c.firstUnsupportedHit with
        | none => rfl
        | some _ => simp [hfh] at hfalse
      simp only [hnoHit]

theorem parserEngine_ok_iff {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (maxRank : Nat)
    (hRank : ∀ n : Fin count, parser.table.rank n ≤ maxRank)
    (hSteps : linearStepsPerToken g.maxFrameCost g.maxNonterminals maxRank ≤ parser.stepsPerToken)
    (input : ParserView) (output : Parsed (g.value g.start)) :
    (parser.run input).2 = some (.ok output) ↔ ParsesWith parser input output := by
  constructor
  · exact parserEngine_ok_sound parser input output
  · intro hp
    obtain ⟨hsrc, steps, hder, _, _, hwf⟩ := hp
    cases output with
    | mk src tree =>
      dsimp only at hsrc
      subst hsrc
      exact (parserEngine_ok_derives_iff parser maxRank hRank hSteps input tree).mpr ⟨hwf, steps, hder⟩

theorem parserEngine_unsupported_iff {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (maxRank : Nat)
    (hRank : ∀ n : Fin count, parser.table.rank n ≤ maxRank)
    (hSteps : linearStepsPerToken g.maxFrameCost g.maxNonterminals maxRank ≤ parser.stepsPerToken)
    (input : ParserView) (hwf : input.WF) (required : Slice) :
    (∃ rule cause span,
        (parser.run input).2 = some (.error (.unsupported rule cause span required))) ↔
      input.source.slice.val < required.val ∧
      (∃ tree steps, Derives g required g.start input.tokens.toList tree steps) ∧
      ∀ earlier, earlier.val < required.val →
        ¬ ∃ tree steps, Derives g earlier g.start input.tokens.toList tree steps := by
  constructor
  · rintro ⟨rule, cause, span, hrun⟩
    exact parserEngine_error_sound parser input (.unsupported rule cause span required) hrun
  · rintro ⟨hltSlice, ⟨tree, steps, hder⟩, hmin⟩
    unfold ParserView.WF at hwf
    have hnonEx := parserEngine_nonExhausted parser maxRank hRank hSteps input
    unfold ParserEngine.run at hnonEx ⊢
    rw [hwf] at hnonEx ⊢
    dsimp only at hnonEx ⊢
    have hhit := parseSymbol_firstUnsupportedHit_isSome parser.table input.tokens
      input.source.slice (eofFileSpan input.source) g.start ⟨0, slice0, none, 0⟩
      (parser.stepsPerToken * (input.tokens.size + 1)) (by simp [slice0, Slice.val])
    cases hps : parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) with
    | mk c val =>
      rw [hps] at hnonEx hhit
      dsimp only at hnonEx hhit ⊢
      have hne : (parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
          ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))).2 ≠ .error .exhausted := by
        rw [hps]
        intro hexh
        dsimp only at hexh
        rw [hexh] at hnonEx
        cases hnonEx
      have hres := parseSymbol_of_predictive (derives_to_predictive parser.table hder)
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))
        (Nat.zero_le _) (by simp) (Nat.zero_le _) hne
      rw [hps] at hres
      obtain ⟨hval, hoff, hreqLe⟩ := hres
      dsimp only at hval hoff hreqLe
      rw [hval]
      have hoff' : c.offset = input.tokens.size := by simpa [Array.length_toList] using hoff
      have hbne : (c.offset != input.tokens.size) = false := by simp [hoff']
      simp only [hbne, Bool.false_eq_true, ↓reduceIte]
      obtain ⟨dSteps, _, hderC, _⟩ :=
        (parseSymbol_parseArgs_sound_ok parser.table input.tokens input.source.slice
          (eofFileSpan input.source) (parser.stepsPerToken * (input.tokens.size + 1))).1
          g.start ⟨0, slice0, none, 0⟩ c tree (Nat.zero_le _) (hval ▸ hps)
      rw [hoff', tokenSlice_full] at hderC
      have hnotLt : ¬ (c.required.val < required.val) :=
        fun hlt => hmin c.required hlt ⟨tree, dSteps, hderC⟩
      have hreqEq : c.required = required := slice_eq_of_val_eq (by omega)
      have hhitSome : c.firstUnsupportedHit.isSome = true := by
        rw [hhit, hreqEq]
        exact decide_eq_true hltSlice
      cases hfh : c.firstUnsupportedHit with
      | none => simp [hfh] at hhitSome
      | some hit =>
        rcases hit with ⟨hitSpan, cause⟩
        rw [hreqEq]
        cases cause with
        | production pid => exact ⟨parser.unsupportedProductionRule, .production pid, hitSpan, rfl⟩
        | nonDecimal r => exact ⟨parser.unsupportedLiteralRule, .nonDecimal r, hitSpan, rfl⟩

theorem parserEngine_malformed_iff {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (maxRank : Nat)
    (hRank : ∀ n : Fin count, parser.table.rank n ≤ maxRank)
    (hSteps : linearStepsPerToken g.maxFrameCost g.maxNonterminals maxRank ≤ parser.stepsPerToken)
    (input : ParserView) (hwf : input.WF) :
    (∃ rule cause span,
        (parser.run input).2 = some (.error (.malformed rule cause span))) ↔
      ¬ ∃ tree required steps, Derives g required g.start input.tokens.toList tree steps := by
  constructor
  · rintro ⟨rule, cause, span, hrun⟩
    exact parserEngine_error_sound parser input (.malformed rule cause span) hrun
  · intro hno
    have hnonEx := parserEngine_nonExhausted parser maxRank hRank hSteps input
    cases hrun : (parser.run input).2 with
    | none => simp [hrun] at hnonEx
    | some res =>
      cases res with
      | ok output =>
        obtain ⟨_, steps, hder, _, _, _⟩ := parserEngine_ok_sound parser input output hrun
        exact False.elim (hno ⟨output.tree, input.source.slice, steps, hder⟩)
      | error ref =>
        cases ref with
        | invalidStream span reason =>
          have hinv := parserEngine_error_sound parser input (.invalidStream span reason) hrun
          have : some (span, reason) = none := hinv.symm.trans hwf
          cases this
        | unsupported rule cause span req =>
          have href := parserEngine_error_sound parser input (.unsupported rule cause span req) hrun
          obtain ⟨_, ⟨tree, steps, hder⟩, _⟩ := href
          exact False.elim (hno ⟨tree, req, steps, hder⟩)
        | malformed rule cause span =>
          exact ⟨rule, cause, span, rfl⟩

/-! ## Section 7: First-violation-in-source-order theorems (malformed and unsupported) -/

private theorem array_getElem?_take_append {α : Type} (xs : Array α) (k i : Nat) (tail : List α)
    (hk : k < xs.size) (hi : i ≤ k) :
    xs[i]? = (xs.toList.take (k + 1) ++ tail).toArray[i]? := by
  have hiSize : i < xs.size := by omega
  have htakeLen : (xs.toList.take (k + 1)).length = k + 1 := by
    rw [List.length_take, Array.length_toList]
    omega
  rw [Array.getElem?_eq_getElem hiSize, List.getElem?_toArray, List.getElem?_append_left (by omega),
    List.getElem?_take_of_lt (by omega), Array.getElem?_toList, Array.getElem?_eq_getElem hiSize]

private theorem parseSymbol_malformed_take_succ_not_viable {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (input : ParserView) (c : ParseCursor)
    (hps : parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) = (c, .error .malformed))
    (hlt : c.offset < input.tokens.size) :
    ∀ tail s tree steps,
      ¬ Derives g s g.start (input.tokens.toList.take (c.offset + 1) ++ tail) tree steps := by
  intro tail s tree steps hder
  let extTokens := (input.tokens.toList.take (c.offset + 1) ++ tail).toArray
  have hCongr : parseSymbol parser.table extTokens input.source.slice (eofFileSpan input.source) g.start
      ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) = (c, .error .malformed) := by
    rw [← hps]
    exact (parseSymbol_parseArgs_prefix_congr parser.table input.tokens extTokens
      input.source.slice (eofFileSpan input.source)
      (parser.stepsPerToken * (input.tokens.size + 1))).1 g.start ⟨0, slice0, none, 0⟩ (Nat.zero_le _)
      (fun i hi => array_getElem?_take_append input.tokens c.offset i tail hlt (by rw [hps] at hi; exact hi))
  have hneAtFuel : (parseSymbol parser.table extTokens input.source.slice (eofFileSpan input.source) g.start
      ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))).2 ≠ .error .exhausted := by
    rw [hCongr]; intro h; cases h
  have hpred := derives_to_predictive parser.table hder
  have hres := parseSymbol_of_predictive hpred ⟨0, slice0, none, 0⟩
    (parser.stepsPerToken * (input.tokens.size + 1))
    (Nat.zero_le _) (by simp [extTokens]) (Nat.zero_le _) hneAtFuel
  rw [hCongr] at hres
  cases hres.1

private theorem parseSymbol_trailing_take_succ_not_viable {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (input : ParserView) (c : ParseCursor) (ast : g.value g.start)
    (hps : parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) = (c, .ok ast))
    (hlt : c.offset < input.tokens.size) :
    ∀ tail s tree steps,
      ¬ Derives g s g.start (input.tokens.toList.take (c.offset + 1) ++ tail) tree steps := by
  intro tail s tree steps hder
  let extTokens := (input.tokens.toList.take (c.offset + 1) ++ tail).toArray
  have hCongr : parseSymbol parser.table extTokens input.source.slice (eofFileSpan input.source) g.start
      ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) = (c, .ok ast) := by
    rw [← hps]
    exact (parseSymbol_parseArgs_prefix_congr parser.table input.tokens extTokens
      input.source.slice (eofFileSpan input.source)
      (parser.stepsPerToken * (input.tokens.size + 1))).1 g.start ⟨0, slice0, none, 0⟩ (Nat.zero_le _)
      (fun i hi => array_getElem?_take_append input.tokens c.offset i tail hlt (by rw [hps] at hi; exact hi))
  have hneAtFuel : (parseSymbol parser.table extTokens input.source.slice (eofFileSpan input.source) g.start
      ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))).2 ≠ .error .exhausted := by
    rw [hCongr]; intro h; cases h
  have hpred := derives_to_predictive parser.table hder
  have hres := parseSymbol_of_predictive hpred ⟨0, slice0, none, 0⟩
    (parser.stepsPerToken * (input.tokens.size + 1))
    (Nat.zero_le _) (by simp [extTokens]) (Nat.zero_le _) hneAtFuel
  rw [hCongr] at hres
  have hlen : c.offset = (input.tokens.toList.take (c.offset + 1) ++ tail).length := by
    simpa using hres.2.1
  have htakeLen : (input.tokens.toList.take (c.offset + 1)).length = c.offset + 1 := by
    rw [List.length_take, Array.length_toList]
    omega
  rw [List.length_append, htakeLen] at hlen
  omega

theorem parserEngine_malformed_first_failure {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g)
    (input : ParserView) (rule : RegisteredRule) (cause : MalformedCause) (span : FileSpan)
    (hrun : (parser.run input).2 = some (.error (.malformed rule cause span))) :
    input.WF ∧ rule = parser.malformedRule ∧
    (cause = .unexpectedToken →
      ∃ k, ∃ hk : k < input.tokens.size,
        (input.tokens[k]'(hk)).span = span ∧
        ∀ tail s tree steps,
          ¬ Derives g s g.start (input.tokens.toList.take (k + 1) ++ tail) tree steps) ∧
    (cause = .unexpectedEnd → span = eofFileSpan input.source) ∧
    (cause = .trailingTokens →
      ∃ k, ∃ hk : k < input.tokens.size,
        (input.tokens[k]'(hk)).span = span ∧
        (∃ s tree steps, Derives g s g.start (input.tokens.toList.take k) tree steps) ∧
        ∀ tail s tree steps,
          ¬ Derives g s g.start (input.tokens.toList.take (k + 1) ++ tail) tree steps) := by
  unfold ParserEngine.run at hrun
  cases hinv : input.invalidLocation with
  | some bad => simp [hinv] at hrun
  | none =>
    simp only [hinv] at hrun
    have hmono := (parseSymbol_parseArgs_mono parser.table input.tokens input.source.slice
      (eofFileSpan input.source) (parser.stepsPerToken * (input.tokens.size + 1))).1
      g.start ⟨0, slice0, none, 0⟩ (Nat.zero_le _)
    cases hps : parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) with
    | mk c val =>
      simp only [hps] at hrun hmono
      cases val with
      | error stop =>
        cases stop with
        | exhausted => simp at hrun
        | malformed =>
          simp only [Option.some.injEq, Except.error.injEq, ParseRefusal.malformed.injEq] at hrun
          obtain ⟨rfl, rfl, rfl⟩ := hrun
          refine ⟨hinv, rfl, ?_, ?_, ?_⟩
          · intro hcause
            by_cases hlt : c.offset < input.tokens.size
            · exact ⟨c.offset, hlt, by simp [Array.getElem?_eq_getElem hlt],
                parseSymbol_malformed_take_succ_not_viable parser input c hps hlt⟩
            · simp [hlt] at hcause
          · intro hcause
            by_cases hlt : c.offset < input.tokens.size
            · simp [hlt] at hcause
            · have hge : input.tokens.size ≤ c.offset := Nat.le_of_not_lt hlt
              simp [Array.getElem?_eq_none hge]
          · intro hcause
            by_cases hlt : c.offset < input.tokens.size <;> simp [hlt] at hcause
      | ok ast =>
        by_cases hoff : c.offset = input.tokens.size
        · have hbne : (c.offset != input.tokens.size) = false := by simp [hoff]
          simp only [hbne, Bool.false_eq_true, ↓reduceIte] at hrun
          cases hfh : c.firstUnsupportedHit <;> simp [hfh] at hrun
        · have hbne : (c.offset != input.tokens.size) = true := bne_iff_ne.mpr hoff
          simp only [hbne, ↓reduceIte, Option.some.injEq, Except.error.injEq,
            ParseRefusal.malformed.injEq] at hrun
          obtain ⟨rfl, rfl, rfl⟩ := hrun
          have hlt : c.offset < input.tokens.size := by omega
          refine ⟨hinv, rfl, (fun h => by cases h), (fun h => by cases h), fun _ => ?_⟩
          obtain ⟨dSteps, _, hderTake, _⟩ :=
            (parseSymbol_parseArgs_sound_ok parser.table input.tokens input.source.slice
              (eofFileSpan input.source) (parser.stepsPerToken * (input.tokens.size + 1))).1
              g.start ⟨0, slice0, none, 0⟩ c ast (Nat.zero_le _) hps
          have hts : tokenSlice input.tokens 0 c.offset = input.tokens.toList.take c.offset := by
            simp [tokenSlice]
          rw [hts] at hderTake
          exact ⟨c.offset, hlt, by simp [Array.getElem?_eq_getElem hlt],
            ⟨c.required, ast, dSteps, hderTake⟩,
            parseSymbol_trailing_take_succ_not_viable parser input c ast hps hlt⟩

private theorem derivesArgs_suffix_of_append {count : Nat} {g : SharedGrammar count}
    {slice : Slice} {rhs pre post : List (Sym Terminal (Fin count))}
    {tokens : List SurfaceToken} {args : ActionArgs g.value rhs} {steps : Nat}
    (hrhs : rhs = pre ++ post)
    (h : DerivesArgs g slice rhs tokens args steps) :
    ∃ tail args' steps', DerivesArgs g slice post tail args' steps' := by
  subst hrhs
  induction pre generalizing tokens steps with
  | nil => exact ⟨tokens, args, steps, h⟩
  | cons sym rest ih =>
    cases sym with
    | t kind =>
      cases h with
      | token _ _ hrest => exact ih hrest
    | n child =>
      cases h with
      | child _ hrest => exact ih hrest

private theorem derivesArgs_productive_of_mem {count : Nat} {g : SharedGrammar count}
    (hprod : g.Productive) {p : Production g.value} (hp : p ∈ g.productions.toList)
    {pre post : List (Sym Terminal (Fin count))} (hrhs : p.rhs = pre ++ post) :
    ∃ tail args steps, DerivesArgs g ⟨Fin.last 21⟩ post tail args steps := by
  obtain ⟨toks, args, steps, hder⟩ := hprod p hp
  exact derivesArgs_suffix_of_append hrhs hder

private theorem parseSymbol_parseArgs_viable_prefix {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) (tokens : Array SurfaceToken) (targetSlice : Slice)
    (eof : FileSpan) (hprod : g.Productive) (fuel : Nat) :
    (∀ (n : Fin count) (c : ParseCursor) (c' : ParseCursor),
      c.offset ≤ tokens.size →
      (∃ p ∈ g.productions.toList, p.lhs = n) →
      parseSymbol table tokens targetSlice eof n c fuel = (c', .error .malformed) →
      ∃ tail val steps, Derives g ⟨Fin.last 21⟩ n
        (tokenSlice tokens c.offset c'.offset ++ tail) val steps) ∧
    (∀ (prodId : ProductionId) (p : Production g.value) (pre rhs : List (Sym Terminal (Fin count)))
      (c c' : ParseCursor),
      p ∈ g.productions.toList →
      p.rhs = pre ++ rhs →
      c.offset ≤ tokens.size →
      parseArgs table tokens targetSlice eof prodId fuel rhs c = (c', .error .malformed) →
      ∃ tail args steps, DerivesArgs g ⟨Fin.last 21⟩ rhs
        (tokenSlice tokens c.offset c'.offset ++ tail) args steps) := by
  induction fuel with
  | zero =>
    exact ⟨fun _ _ _ _ _ h => (by cases h), fun _ _ _ _ _ _ _ _ _ h => (by cases h)⟩
  | succ fuel ih =>
    obtain ⟨ihSym, ihArgs⟩ := ih
    constructor
    · intro n c c' hoff hhasP hrun
      unfold parseSymbol at hrun
      dsimp only at hrun
      cases hlook : table.lookup n (tokens[c.offset]?.map fun t => t.value.kind) with
      | none =>
        simp only [hlook, Prod.mk.injEq] at hrun
        have hc' : c'.offset = c.offset := by simpa using congrArg ParseCursor.offset hrun.1.symm
        rw [hc', tokenSlice_self]
        obtain ⟨p, hpMem, rfl⟩ := hhasP
        obtain ⟨toks, args, steps, hder⟩ := hprod p hpMem
        exact ⟨toks, _, _, .apply p toks args hpMem (slice_le_last p.firstSlice) hder⟩
      | some index =>
        simp only [hlook] at hrun
        cases hp : g.productions[index]? with
        | none =>
          simp only [hp, Prod.mk.injEq] at hrun
          have hc' : c'.offset = c.offset := by simpa using congrArg ParseCursor.offset hrun.1.symm
          rw [hc', tokenSlice_self]
          obtain ⟨p, hpMem, rfl⟩ := hhasP
          obtain ⟨toks, args, steps, hder⟩ := hprod p hpMem
          exact ⟨toks, _, _, .apply p toks args hpMem (slice_le_last p.firstSlice) hder⟩
        | some p =>
          simp only [hp] at hrun
          by_cases hlhs : p.lhs = n
          · simp only [hlhs, ↓reduceDIte] at hrun
            subst n
            let c₁ := ({c with steps := c.steps + 1}).require targetSlice p.firstSlice
              (.production p.id) ((tokens[c.offset]?.map Located.span).getD eof)
            cases hpa : parseArgs table tokens targetSlice eof p.id fuel p.rhs c₁ with
            | mk c₂ res =>
              dsimp only [c₁] at hpa
              rw [hpa] at hrun
              cases res with
              | ok _ => cases hrun
              | error stop =>
                cases stop with
                | exhausted => cases hrun
                | malformed =>
                  have hc' : c' = c₂ := (Prod.mk.inj hrun).1.symm
                  subst c'
                  have hpMem : p ∈ g.productions.toList :=
                    Array.mem_toList_iff.mpr (Array.mem_of_getElem? hp)
                  obtain ⟨tail, args, steps, hder⟩ :=
                    ihArgs p.id p [] p.rhs c₁ c₂ hpMem rfl hoff hpa
                  exact ⟨tail, _, _, .apply p _ args hpMem (slice_le_last p.firstSlice) hder⟩
          · simp only [hlhs, ↓reduceDIte, Prod.mk.injEq] at hrun
            have hc' : c'.offset = c.offset := by simpa using congrArg ParseCursor.offset hrun.1.symm
            rw [hc', tokenSlice_self]
            obtain ⟨p₀, hpMem, rfl⟩ := hhasP
            obtain ⟨toks, args, steps, hder⟩ := hprod p₀ hpMem
            exact ⟨toks, _, _, .apply p₀ toks args hpMem (slice_le_last p₀.firstSlice) hder⟩
    · intro prodId p pre rhs c c' hpMem hrhs hoff hrun
      cases rhs with
      | nil =>
        unfold parseArgs at hrun
        cases hrun
      | cons sym rest =>
        cases sym with
        | t kind =>
          unfold parseArgs at hrun
          dsimp only at hrun
          cases htok : tokens[c.offset]? with
          | none =>
            simp only [htok, Prod.mk.injEq] at hrun
            have hc' : c'.offset = c.offset := by simpa using congrArg ParseCursor.offset hrun.1.symm
            rw [hc', tokenSlice_self]
            exact derivesArgs_productive_of_mem hprod hpMem hrhs
          | some token =>
            simp only [htok] at hrun
            split at hrun
            · simp only [Prod.mk.injEq] at hrun
              have hc' : c'.offset = c.offset := by simpa using congrArg ParseCursor.offset hrun.1.symm
              rw [hc', tokenSlice_self]
              exact derivesArgs_productive_of_mem hprod hpMem hrhs
            · rename_i hnotNe
              have hkindEq : token.value.kind = kind := by
                cases hdec : decide (token.value.kind = kind) with
                | true => exact of_decide_eq_true hdec
                | false => exact False.elim (hnotNe (of_decide_eq_false hdec))
              have hlt : c.offset < tokens.size := (Array.getElem?_eq_some_iff.mp htok).1
              let c₁ := ({c with steps := c.steps + 1}).require targetSlice (g.required token.value)
                (tokenUnsupportedCause prodId token.value) token.span
              let c₂ := {c₁ with offset := c₁.offset + 1}
              cases hpa : parseArgs table tokens targetSlice eof prodId fuel rest c₂ with
              | mk c₃ res =>
                dsimp only [c₂, c₁] at hpa
                rw [hpa] at hrun
                cases res with
                | ok _ => cases hrun
                | error stop =>
                  cases stop with
                  | exhausted => cases hrun
                  | malformed =>
                    have hc' : c' = c₃ := (Prod.mk.inj hrun).1.symm
                    subst c'
                    have hmono := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel).2
                      prodId rest c₂ hlt
                    dsimp only [c₂, c₁] at hmono
                    rw [hpa] at hmono
                    dsimp only [ParseCursor.require] at hmono
                    have hrhs' : p.rhs = (pre ++ [.t kind]) ++ rest := by
                      simpa [List.append_assoc] using hrhs
                    obtain ⟨tail, args, steps, hder⟩ :=
                      ihArgs prodId p (pre ++ [.t kind]) rest c₂ c₃ hpMem hrhs' hlt hpa
                    rw [tokenSlice_cons tokens htok hmono.1]
                    exact ⟨tail, (token, args), steps + 1,
                      .token hkindEq (slice_le_last (g.required token.value)) hder⟩
        | n child =>
          unfold parseArgs at hrun
          dsimp only at hrun
          have hchildProd : ∃ p' ∈ g.productions.toList, p'.lhs = child := by
            obtain ⟨_, _, _, hderP⟩ := derivesArgs_productive_of_mem hprod hpMem hrhs
            cases hderP with
            | child hL _ =>
              cases hL with
              | apply p' _ _ hp' _ _ => exact ⟨p', hp', rfl⟩
          cases hps : parseSymbol table tokens targetSlice eof child {c with steps := c.steps + 1} fuel with
          | mk c₁ res₁ =>
            rw [hps] at hrun
            have hmono₁ := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel).1
              child {c with steps := c.steps + 1} hoff
            rw [hps] at hmono₁
            cases res₁ with
            | error stop =>
              cases stop with
              | exhausted => cases hrun
              | malformed =>
                have hc' : c' = c₁ := (Prod.mk.inj hrun).1.symm
                subst c'
                obtain ⟨tail₁, val₁, steps₁, hder₁⟩ :=
                  ihSym child {c with steps := c.steps + 1} c₁ hoff hchildProd hps
                have hrhs' : p.rhs = (pre ++ [.n child]) ++ rest := by
                  simpa [List.append_assoc] using hrhs
                obtain ⟨tail₂, args₂, steps₂, hder₂⟩ := derivesArgs_productive_of_mem hprod hpMem hrhs'
                refine ⟨tail₁ ++ tail₂, (val₁, args₂), steps₁ + steps₂, ?_⟩
                rw [← List.append_assoc]
                exact .child hder₁ hder₂
            | ok childVal =>
              dsimp only at hrun
              cases hpa : parseArgs table tokens targetSlice eof prodId fuel rest c₁ with
              | mk c₂ res₂ =>
                rw [hpa] at hrun
                cases res₂ with
                | ok _ => cases hrun
                | error stop =>
                  cases stop with
                  | exhausted => cases hrun
                  | malformed =>
                    have hc' : c' = c₂ := (Prod.mk.inj hrun).1.symm
                    subst c'
                    have hmono₂ := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel).2
                      prodId rest c₁ hmono₁.2.1
                    rw [hpa] at hmono₂
                    obtain ⟨dL, _, hderL, _⟩ :=
                      (parseSymbol_parseArgs_sound_ok table tokens targetSlice eof fuel).1
                        child {c with steps := c.steps + 1} c₁ childVal hoff hps
                    have hderL' := derives_mono (slice_le_last c₁.required) hderL
                    have hrhs' : p.rhs = (pre ++ [.n child]) ++ rest := by
                      simpa [List.append_assoc] using hrhs
                    obtain ⟨tail, args, stepsR, hderR⟩ :=
                      ihArgs prodId p (pre ++ [.n child]) rest c₁ c₂ hpMem hrhs' hmono₁.2.1 hpa
                    rw [← tokenSlice_append tokens hmono₁.1 hmono₂.1]
                    refine ⟨tail, (childVal, args), dL + stepsR, ?_⟩
                    rw [List.append_assoc]
                    exact .child hderL' hderR

theorem parserEngine_malformed_viable_prefix {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (hprod : g.Productive)
    (hstart : ∃ p ∈ g.productions.toList, p.lhs = g.start)
    (input : ParserView) (rule : RegisteredRule) (cause : MalformedCause) (span : FileSpan)
    (hrun : (parser.run input).2 = some (.error (.malformed rule cause span))) :
    ∃ k ≤ input.tokens.size,
      span = (input.tokens[k]?.map Located.span).getD (eofFileSpan input.source) ∧
      (∃ tail tree steps, Derives g ⟨Fin.last 21⟩ g.start (input.tokens.toList.take k ++ tail) tree steps) ∧
      (cause = .unexpectedEnd → k = input.tokens.size) ∧
      (cause ≠ .unexpectedEnd →
        k < input.tokens.size ∧
        ∀ tail s tree steps,
          ¬ Derives g s g.start (input.tokens.toList.take (k + 1) ++ tail) tree steps) := by
  unfold ParserEngine.run at hrun
  cases hinv : input.invalidLocation with
  | some bad => simp [hinv] at hrun
  | none =>
    simp only [hinv] at hrun
    have hmono := (parseSymbol_parseArgs_mono parser.table input.tokens input.source.slice
      (eofFileSpan input.source) (parser.stepsPerToken * (input.tokens.size + 1))).1
      g.start ⟨0, slice0, none, 0⟩ (Nat.zero_le _)
    cases hps : parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) with
    | mk c val =>
      simp only [hps] at hrun hmono
      cases val with
      | error stop =>
        cases stop with
        | exhausted => simp at hrun
        | malformed =>
          simp only [Option.some.injEq, Except.error.injEq, ParseRefusal.malformed.injEq] at hrun
          obtain ⟨_, rfl, rfl⟩ := hrun
          obtain ⟨tail, tree, steps, hder⟩ :=
            (parseSymbol_parseArgs_viable_prefix parser.table input.tokens input.source.slice
              (eofFileSpan input.source) hprod (parser.stepsPerToken * (input.tokens.size + 1))).1
              g.start ⟨0, slice0, none, 0⟩ c (Nat.zero_le _) hstart hps
          have hts : tokenSlice input.tokens 0 c.offset = input.tokens.toList.take c.offset := by
            simp [tokenSlice]
          rw [hts] at hder
          refine ⟨c.offset, hmono.2.1, rfl, ⟨tail, tree, steps, hder⟩, ?_, ?_⟩
          · intro hcause
            by_cases hlt : c.offset < input.tokens.size
            · simp only [hlt, ↓reduceIte] at hcause
              cases hcause
            · exact Nat.le_antisymm hmono.2.1 (Nat.le_of_not_lt hlt)
          · intro hne
            by_cases hlt : c.offset < input.tokens.size
            · exact ⟨hlt, parseSymbol_malformed_take_succ_not_viable parser input c hps hlt⟩
            · simp only [hlt, ↓reduceIte] at hne
              exact False.elim (hne rfl)
      | ok ast =>
        by_cases hoff : c.offset = input.tokens.size
        · have hbne : (c.offset != input.tokens.size) = false := by simp [hoff]
          simp only [hbne, Bool.false_eq_true, ↓reduceIte] at hrun
          cases hfh : c.firstUnsupportedHit <;> simp [hfh] at hrun
        · have hbne : (c.offset != input.tokens.size) = true := bne_iff_ne.mpr hoff
          simp only [hbne, ↓reduceIte, Option.some.injEq, Except.error.injEq,
            ParseRefusal.malformed.injEq] at hrun
          obtain ⟨_, rfl, rfl⟩ := hrun
          have hlt : c.offset < input.tokens.size := Nat.lt_of_le_of_ne hmono.2.1 hoff
          obtain ⟨dSteps, _, hder, _⟩ :=
            (parseSymbol_parseArgs_sound_ok parser.table input.tokens input.source.slice
              (eofFileSpan input.source) (parser.stepsPerToken * (input.tokens.size + 1))).1
              g.start ⟨0, slice0, none, 0⟩ c ast (Nat.zero_le _) hps
          have hts : tokenSlice input.tokens 0 c.offset = input.tokens.toList.take c.offset := by
            simp [tokenSlice]
          rw [hts] at hder
          refine ⟨c.offset, hmono.2.1, rfl, ⟨[], ast, dSteps, ?_⟩, (fun h => by cases h),
            fun _ => ⟨hlt, parseSymbol_trailing_take_succ_not_viable parser input c ast hps hlt⟩⟩
          simpa using derives_mono (slice_le_last c.required) hder

/-! ### Unsupported first-witness trace in source order -/

/-- One pre-order slice-requirement event recorded during a parse. -/
structure SliceEvent where
  offset : Nat
  span : FileSpan
  slice : Slice
  cause : UnsupportedCause
  deriving DecidableEq

/-- First event in a pre-order slice trace whose required slice exceeds `target`. -/
def firstOutOfSlice (target : Slice) (events : List SliceEvent) : Option SliceEvent :=
  events.find? fun ev => decide (target.val < ev.slice.val)

theorem firstOutOfSlice_eq_some_iff (target : Slice) (events : List SliceEvent) (hit : SliceEvent) :
    firstOutOfSlice target events = some hit ↔
      ∃ pre post, events = pre ++ hit :: post ∧
        (∀ ev ∈ pre, ev.slice.val ≤ target.val) ∧
        target.val < hit.slice.val := by
  constructor
  · intro h
    induction events with
    | nil => simp [firstOutOfSlice] at h
    | cons ev rest ih =>
      simp only [firstOutOfSlice, List.find?_cons] at h
      by_cases hlt : target.val < ev.slice.val
      · simp only [hlt, decide_true, Option.some.injEq] at h
        subst hit
        exact ⟨[], rest, rfl, (fun _ hm => nomatch hm), hlt⟩
      · simp only [hlt, decide_false] at h
        obtain ⟨pre, post, rfl, hpre, hhit⟩ := ih h
        refine ⟨ev :: pre, post, rfl, ?_, hhit⟩
        intro e he
        rcases List.mem_cons.mp he with rfl | hmem
        · exact Nat.le_of_not_lt hlt
        · exact hpre e hmem
  · rintro ⟨pre, post, rfl, hpre, hhit⟩
    induction pre with
    | nil =>
      simp [firstOutOfSlice, hhit]
    | cons ev pre ih =>
      have hle : ev.slice.val ≤ target.val := hpre ev List.mem_cons_self
      have hnot : ¬ (target.val < ev.slice.val) := Nat.not_lt.mpr hle
      simp only [firstOutOfSlice, List.cons_append, List.find?_cons, hnot, decide_false]
      exact ih (fun e he => hpre e (List.mem_cons_of_mem _ he))

mutual
  /-- Exact pre-order slice-requirement trace of a `Derives` derivation for nonterminal `n`
  consuming `tokens` starting at token offset `pos`, with unconsumed suffix `rest` and
  end-of-file span `eof`. Every production entry records `(pos, lookSpan, p.firstSlice, .production p.id)`
  and every terminal consumption records `(tokPos, tok.span, g.required tok.value, tokenUnsupportedCause prodId tok.value)`. -/
  inductive DerivesTrace {count : Nat} (g : SharedGrammar count) (slice : Slice)
      (eof : FileSpan) :
      (n : Fin count) → (pos : Nat) → (tokens rest : List SurfaceToken) →
      g.value n → Nat → List SliceEvent → Prop where
    | apply (p : Production g.value) (pos : Nat) (tokens rest : List SurfaceToken)
        (args : ActionArgs g.value p.rhs) {steps : Nat} {events : List SliceEvent} :
        p ∈ g.productions.toList →
        p.firstSlice.val ≤ slice.val →
        DerivesArgsTrace g slice eof p.id p.rhs pos tokens rest args steps events →
        DerivesTrace g slice eof p.lhs pos tokens rest
          (p.action.eval args).2 (steps + 1 + (p.action.eval args).1)
          (⟨pos, ((tokens ++ rest).head?.map Located.span).getD eof,
            p.firstSlice, .production p.id⟩ :: events)

  inductive DerivesArgsTrace {count : Nat} (g : SharedGrammar count) (slice : Slice)
      (eof : FileSpan) :
      (prodId : ProductionId) → (rhs : List (Sym Terminal (Fin count))) → (pos : Nat) →
      (tokens rest : List SurfaceToken) → ActionArgs g.value rhs → Nat → List SliceEvent → Prop where
    | nil (prodId : ProductionId) (pos : Nat) (rest : List SurfaceToken) :
        DerivesArgsTrace g slice eof prodId [] pos [] rest () 0 []
    | token {prodId : ProductionId} {kind : Terminal} {token : SurfaceToken}
        {rhs : List (Sym Terminal (Fin count))}
        {pos : Nat} {tokens rest : List SurfaceToken} {args : ActionArgs g.value rhs}
        {steps : Nat} {events : List SliceEvent} :
        token.value.kind = kind →
        (g.required token.value).val ≤ slice.val →
        DerivesArgsTrace g slice eof prodId rhs (pos + 1) tokens rest args steps events →
        DerivesArgsTrace g slice eof prodId (.t kind :: rhs) pos (token :: tokens) rest
          (token, args) (steps + 1)
          (⟨pos, token.span, g.required token.value, tokenUnsupportedCause prodId token.value⟩ :: events)
    | child {prodId : ProductionId} {n : Fin count} {rhs : List (Sym Terminal (Fin count))}
        {pos : Nat} {head tail rest : List SurfaceToken}
        {value : g.value n} {args : ActionArgs g.value rhs}
        {left right : Nat} {evsL evsR : List SliceEvent} :
        DerivesTrace g slice eof n pos head (tail ++ rest) value left evsL →
        DerivesArgsTrace g slice eof prodId rhs (pos + head.length) tail rest args right evsR →
        DerivesArgsTrace g slice eof prodId (.n n :: rhs) pos (head ++ tail) rest
          (value, args) (left + right) (evsL ++ evsR)
end

theorem derivesTrace_to_derives {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {eof : FileSpan} {n : Fin count} {pos : Nat} {tokens rest : List SurfaceToken}
    {val : g.value n} {steps : Nat} {events : List SliceEvent}
    (h : DerivesTrace g slice eof n pos tokens rest val steps events) :
    Derives g slice n tokens val steps :=
  DerivesTrace.rec
    (motive_1 := fun n _ toks _ val steps _ _ => Derives g slice n toks val steps)
    (motive_2 := fun _ rhs _ toks _ args steps _ _ => DerivesArgs g slice rhs toks args steps)
    (fun p _ _ _ _ _ _ hmem hfs _ ih => .apply p _ _ hmem hfs ih)
    (fun _ _ _ => .nil)
    (fun hkind hreq _ ih => .token hkind hreq ih)
    (fun _ _ ihL ihR => .child ihL ihR)
    h

private theorem derivesArgsTrace_to_derivesArgs {count : Nat} {g : SharedGrammar count}
    {slice : Slice} {eof : FileSpan} {prodId : ProductionId}
    {rhs : List (Sym Terminal (Fin count))} {pos : Nat} {tokens rest : List SurfaceToken}
    {args : ActionArgs g.value rhs} {steps : Nat} {events : List SliceEvent}
    (h : DerivesArgsTrace g slice eof prodId rhs pos tokens rest args steps events) :
    DerivesArgs g slice rhs tokens args steps :=
  DerivesArgsTrace.rec
    (motive_1 := fun n _ toks _ val steps _ _ => Derives g slice n toks val steps)
    (motive_2 := fun _ rhs _ toks _ args steps _ _ => DerivesArgs g slice rhs toks args steps)
    (fun p _ _ _ _ _ _ hmem hfs _ ih => .apply p _ _ hmem hfs ih)
    (fun _ _ _ => .nil)
    (fun hkind hreq _ ih => .token hkind hreq ih)
    (fun _ _ ihL ihR => .child ihL ihR)
    h

theorem derivesTrace_mono {count : Nat} {g : SharedGrammar count} {s₁ s₂ : Slice}
    (hle : s₁.val ≤ s₂.val) {eof : FileSpan} {n : Fin count} {pos : Nat}
    {tokens rest : List SurfaceToken} {val : g.value n} {steps : Nat} {events : List SliceEvent}
    (h : DerivesTrace g s₁ eof n pos tokens rest val steps events) :
    DerivesTrace g s₂ eof n pos tokens rest val steps events :=
  DerivesTrace.rec
    (motive_1 := fun n pos toks rest val steps evs _ =>
      DerivesTrace g s₂ eof n pos toks rest val steps evs)
    (motive_2 := fun prodId rhs pos toks rest args steps evs _ =>
      DerivesArgsTrace g s₂ eof prodId rhs pos toks rest args steps evs)
    (fun p pos toks rest args _ _ hmem hfs _ ih =>
      .apply p pos toks rest args hmem (Nat.le_trans hfs hle) ih)
    (fun prodId pos rest => .nil prodId pos rest)
    (fun hkind hreq _ ih => .token hkind (Nat.le_trans hreq hle) ih)
    (fun _ _ ihL ihR => .child ihL ihR)
    h

theorem derives_exists_trace {count : Nat} {g : SharedGrammar count} {slice : Slice}
    (eof : FileSpan) {n : Fin count} {tokens : List SurfaceToken}
    {val : g.value n} {steps : Nat}
    (h : Derives g slice n tokens val steps) :
    ∀ pos rest, ∃ events, DerivesTrace g slice eof n pos tokens rest val steps events :=
  Derives.rec
    (motive_1 := fun n toks val steps _ =>
      ∀ pos rest, ∃ events, DerivesTrace g slice eof n pos toks rest val steps events)
    (motive_2 := fun rhs toks args steps _ =>
      ∀ prodId pos rest, ∃ events, DerivesArgsTrace g slice eof prodId rhs pos toks rest args steps events)
    (fun p toks args _ hmem hfs _ ih pos rest => by
      obtain ⟨evs, hevs⟩ := ih p.id pos rest
      exact ⟨_, .apply p pos toks rest args hmem hfs hevs⟩)
    (fun prodId pos rest => ⟨[], .nil prodId pos rest⟩)
    (fun hkind hreq _ ih prodId pos rest => by
      obtain ⟨evs, hevs⟩ := ih prodId (pos + 1) rest
      exact ⟨_, .token hkind hreq hevs⟩)
    (fun {m _ head _ _ _ _ _} _ _ ihL ihR prodId pos rest => by
      obtain ⟨evsL, hevsL⟩ := ihL pos (_ ++ rest)
      obtain ⟨evsR, hevsR⟩ := ihR prodId (pos + head.length) rest
      exact ⟨evsL ++ evsR, .child hevsL hevsR⟩)
    h

private theorem pairwise_append {α : Type} {R : α → α → Prop} {xs ys : List α}
    (hx : xs.Pairwise R) (hy : ys.Pairwise R) (hxy : ∀ a ∈ xs, ∀ b ∈ ys, R a b) :
    (xs ++ ys).Pairwise R := by
  induction xs with
  | nil => exact hy
  | cons a xs ih =>
    rcases List.pairwise_cons.mp hx with ⟨ha, hxs⟩
    refine List.pairwise_cons.mpr ⟨fun b hb => ?_, ih hxs (fun x hx' y hy' => hxy x (List.mem_cons_of_mem _ hx') y hy')⟩
    rcases List.mem_append.mp hb with hb1 | hb2
    · exact ha b hb1
    · exact hxy a List.mem_cons_self b hb2

theorem derivesTrace_props {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {eof : FileSpan} {n : Fin count} {pos : Nat} {tokens rest : List SurfaceToken}
    {val : g.value n} {steps : Nat} {events : List SliceEvent}
    (h : DerivesTrace g slice eof n pos tokens rest val steps events) :
    events.Pairwise (fun a b => a.offset ≤ b.offset) ∧
    (∀ ev ∈ events, pos ≤ ev.offset ∧ ev.offset ≤ pos + tokens.length ∧ ev.slice.val ≤ slice.val) ∧
    (∀ i (hi : i < tokens.length),
      ∃ ev ∈ events, ev.offset = pos + i ∧ ev.slice = g.required (tokens[i]'(hi)).value) :=
  DerivesTrace.rec
    (motive_1 := fun _ pos toks _ _ _ evs _ =>
      evs.Pairwise (fun a b => a.offset ≤ b.offset) ∧
      (∀ ev ∈ evs, pos ≤ ev.offset ∧ ev.offset ≤ pos + toks.length ∧ ev.slice.val ≤ slice.val) ∧
      (∀ i (hi : i < toks.length),
        ∃ ev ∈ evs, ev.offset = pos + i ∧ ev.slice = g.required (toks[i]'(hi)).value))
    (motive_2 := fun _ _ pos toks _ _ _ evs _ =>
      evs.Pairwise (fun a b => a.offset ≤ b.offset) ∧
      (∀ ev ∈ evs, pos ≤ ev.offset ∧ ev.offset ≤ pos + toks.length ∧ ev.slice.val ≤ slice.val) ∧
      (∀ i (hi : i < toks.length),
        ∃ ev ∈ evs, ev.offset = pos + i ∧ ev.slice = g.required (toks[i]'(hi)).value))
    (fun p pos toks rest args _ evs _ hfs _ ih => by
      obtain ⟨hpw, hall, hcov⟩ := ih
      refine ⟨List.pairwise_cons.mpr ⟨fun ev hev => (hall ev hev).1, hpw⟩, ?_, ?_⟩
      · intro ev hev
        rcases List.mem_cons.mp hev with rfl | hmem
        · exact ⟨Nat.le_refl _, Nat.le_add_right _ _, hfs⟩
        · exact hall ev hmem
      · intro i hi
        obtain ⟨ev, hev, ho, hs⟩ := hcov i hi
        exact ⟨ev, List.mem_cons_of_mem _ hev, ho, hs⟩)
    (fun _ _ _ => ⟨.nil, (fun _ hm => nomatch hm), (fun _ hi => nomatch hi)⟩)
    (fun {_ _ tok _ pos toks _ _ _ _} _ hreq _ ih => by
      obtain ⟨hpw, hall, hcov⟩ := ih
      refine ⟨List.pairwise_cons.mpr ⟨fun ev hev => Nat.le_trans (Nat.le_succ _) (hall ev hev).1, hpw⟩, ?_, ?_⟩
      · intro ev hev
        rcases List.mem_cons.mp hev with rfl | hmem
        · exact ⟨Nat.le_refl _, by simp only [List.length_cons]; omega, hreq⟩
        · have h := hall ev hmem
          exact ⟨Nat.le_trans (Nat.le_succ _) h.1, by simp only [List.length_cons]; omega, h.2.2⟩
      · intro i hi
        cases i with
        | zero =>
          exact ⟨_, List.mem_cons_self, by simp, rfl⟩
        | succ j =>
          have hj : j < toks.length := Nat.lt_of_succ_lt_succ hi
          obtain ⟨ev, hev, ho, hs⟩ := hcov j hj
          exact ⟨ev, List.mem_cons_of_mem _ hev, by omega, hs⟩)
    (fun {_ _ _ pos head tail _ _ _ _ _ evsL evsR} _ _ ihL ihR => by
      obtain ⟨hpwL, hallL, hcovL⟩ := ihL
      obtain ⟨hpwR, hallR, hcovR⟩ := ihR
      refine ⟨pairwise_append hpwL hpwR (fun a ha b hb => Nat.le_trans (hallL a ha).2.1 (hallR b hb).1), ?_, ?_⟩
      · intro ev hev
        rcases List.mem_append.mp hev with h1 | h2
        · have h := hallL ev h1
          exact ⟨h.1, by simp only [List.length_append]; omega, h.2.2⟩
        · have h := hallR ev h2
          exact ⟨by omega, by simp only [List.length_append]; omega, h.2.2⟩
      · intro i hi
        by_cases hlt : i < head.length
        · obtain ⟨ev, hev, ho, hs⟩ := hcovL i hlt
          refine ⟨ev, List.mem_append_left _ hev, ho, ?_⟩
          simpa [List.getElem_append_left hlt] using hs
        · have hge : head.length ≤ i := Nat.le_of_not_lt hlt
          have hj : i - head.length < tail.length := by
            simp only [List.length_append] at hi
            omega
          obtain ⟨ev, hev, ho, hs⟩ := hcovR (i - head.length) hj
          refine ⟨ev, List.mem_append_right _ hev, by omega, ?_⟩
          simpa [List.getElem_append_right hge] using hs)
    h

private theorem derivesTrace_inv {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {eof : FileSpan} {n : Fin count} {pos : Nat} {tokens rest : List SurfaceToken}
    {val : g.value n} {steps : Nat} {events : List SliceEvent}
    (h : DerivesTrace g slice eof n pos tokens rest val steps events) :
    ∃ (p : Production g.value) (args : ActionArgs g.value p.rhs) (dSteps : Nat)
      (evs : List SliceEvent) (hlhs : p.lhs = n),
      p ∈ g.productions.toList ∧
      p.firstSlice.val ≤ slice.val ∧
      DerivesArgsTrace g slice eof p.id p.rhs pos tokens rest args dSteps evs ∧
      val = hlhs ▸ (p.action.eval args).2 ∧
      steps = dSteps + 1 + (p.action.eval args).1 ∧
      events = ⟨pos, ((tokens ++ rest).head?.map Located.span).getD eof,
        p.firstSlice, .production p.id⟩ :: evs := by
  cases h with
  | apply p _ _ _ args hmem hfs hargs =>
    exact ⟨p, args, _, _, rfl, hmem, hfs, hargs, rfl, rfl, rfl⟩

private theorem lookup_of_derivesArgs {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) {slice : Slice} {p : Production g.value}
    {toks rest : List SurfaceToken} {args : ActionArgs g.value p.rhs} {steps : Nat}
    (hmem : p ∈ g.productions.toList)
    (hargs : DerivesArgs g slice p.rhs toks args steps)
    (hfol : Follow g.grammar p.lhs (rest.head?.map fun t => t.value.kind)) :
    ∃ idx, g.productions[idx]? = some p ∧
      table.lookup p.lhs ((toks ++ rest).head?.map fun t => t.value.kind) = some idx := by
  obtain ⟨idx, hget⟩ := List.mem_iff_getElem?.mp hmem
  have hgetArr : g.productions[idx]? = some p := by
    simpa [Array.getElem?_toList] using hget
  have hprodAt : g.grammar.prods[idx]? = some (p.lhs, p.rhs) := by
    simp only [SharedGrammar.grammar, List.getElem?_map, hget, Option.map_some]
  have hpred : Predicts g.grammar idx ((toks ++ rest).head?.map fun t => t.value.kind) := by
    refine ⟨p.lhs, p.rhs, hprodAt, ?_⟩
    cases toks with
    | nil => exact Or.inr ⟨derives_nullable_of_nil.2 hargs, hfol⟩
    | cons tok toks' => exact Or.inl ⟨tok.value.kind, rfl, derives_first_of_cons.2 hargs⟩
  exact ⟨idx, hgetArr, (table.predicts p.lhs _ idx).mpr ⟨hpred, p.rhs, hprodAt⟩⟩

private theorem derivesTrace_unique_aux {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) {eof : FileSpan} {s₁ s₂ : Slice}
    {n : Fin count} {pos : Nat} {head₁ rest₁ : List SurfaceToken}
    {val₁ : g.value n} {steps₁ : Nat} {evs₁ : List SliceEvent}
    (h₁ : DerivesTrace g s₁ eof n pos head₁ rest₁ val₁ steps₁ evs₁) :
    Follow g.grammar n (rest₁.head?.map fun t => t.value.kind) →
    ∀ {head₂ rest₂ val₂ steps₂ evs₂},
      DerivesTrace g s₂ eof n pos head₂ rest₂ val₂ steps₂ evs₂ →
      Follow g.grammar n (rest₂.head?.map fun t => t.value.kind) →
      head₁ ++ rest₁ = head₂ ++ rest₂ →
      head₁ = head₂ ∧ rest₁ = rest₂ ∧ val₁ = val₂ ∧ steps₁ = steps₂ ∧ evs₁ = evs₂ :=
  DerivesTrace.rec
    (motive_1 := fun n pos head₁ rest₁ val₁ steps₁ evs₁ _ =>
      Follow g.grammar n (rest₁.head?.map fun t => t.value.kind) →
      ∀ {head₂ rest₂ val₂ steps₂ evs₂},
        DerivesTrace g s₂ eof n pos head₂ rest₂ val₂ steps₂ evs₂ →
        Follow g.grammar n (rest₂.head?.map fun t => t.value.kind) →
        head₁ ++ rest₁ = head₂ ++ rest₂ →
        head₁ = head₂ ∧ rest₁ = rest₂ ∧ val₁ = val₂ ∧ steps₁ = steps₂ ∧ evs₁ = evs₂)
    (motive_2 := fun prodId rhs pos head₁ rest₁ args₁ steps₁ evs₁ _ =>
      ∀ (lhs : Fin count) (pre : List (Sym Terminal (Fin count))),
        (lhs, pre ++ rhs) ∈ g.grammar.prods →
        Follow g.grammar lhs (rest₁.head?.map fun t => t.value.kind) →
        ∀ {head₂ rest₂ args₂ steps₂ evs₂},
          DerivesArgsTrace g s₂ eof prodId rhs pos head₂ rest₂ args₂ steps₂ evs₂ →
          Follow g.grammar lhs (rest₂.head?.map fun t => t.value.kind) →
          head₁ ++ rest₁ = head₂ ++ rest₂ →
          head₁ = head₂ ∧ rest₁ = rest₂ ∧ args₁ = args₂ ∧ steps₁ = steps₂ ∧ evs₁ = evs₂)
    (fun p₁ pos toks₁ rest₁ args₁ _ evs₁' hmem₁ _ hargs₁ ihArgs hfol₁ head₂ rest₂ val₂ steps₂ evs₂ h₂ hfol₂ hstr => by
      obtain ⟨idx₁, hget₁, hlook₁⟩ :=
        lookup_of_derivesArgs table hmem₁ (derivesArgsTrace_to_derivesArgs hargs₁) hfol₁
      obtain ⟨p₂, args₂, dSteps₂, evs₂', hlhs₂, hmem₂, _, hargs₂, rfl, rfl, rfl⟩ :=
        derivesTrace_inv h₂
      have hfol₂' : Follow g.grammar p₂.lhs (rest₂.head?.map fun t => t.value.kind) := by
        simpa [hlhs₂] using hfol₂
      obtain ⟨idx₂, hget₂, hlook₂⟩ :=
        lookup_of_derivesArgs table hmem₂ (derivesArgsTrace_to_derivesArgs hargs₂) hfol₂'
      rw [hstr] at hlook₁
      have hlook₂' : table.lookup p₁.lhs ((head₂ ++ rest₂).head?.map fun t => t.value.kind) = some idx₂ := by
        simpa [hlhs₂] using hlook₂
      have hidx : idx₁ = idx₂ := Option.some.inj (hlook₁.symm.trans hlook₂')
      subst hidx
      have hp : p₁ = p₂ := Option.some.inj (hget₁.symm.trans hget₂)
      subst hp
      have hprodMem : (p₁.lhs, [] ++ p₁.rhs) ∈ g.grammar.prods :=
        List.mem_map.mpr ⟨p₁, hmem₁, rfl⟩
      obtain ⟨rfl, rfl, rfl, rfl, rfl⟩ := ihArgs p₁.lhs [] hprodMem hfol₁ hargs₂ hfol₂ hstr
      exact ⟨rfl, rfl, rfl, rfl, rfl⟩)
    (fun _ _ rest₁ _ _ _ _ head₂ rest₂ args₂ steps₂ evs₂ h₂ _ hstr => by
      cases h₂ with
      | nil _ _ _ => exact ⟨rfl, hstr, rfl, rfl, rfl⟩)
    (fun {_ kind _ rhs _ _ _ _ _ _} _ _ _ ihArgs lhs pre hmem hfol₁ head₂ rest₂ args₂ steps₂ evs₂ h₂ hfol₂ hstr => by
      cases h₂ with
      | token _ _ hargs₂ =>
        obtain ⟨rfl, htail⟩ := List.cons.inj hstr
        have hmem' : (lhs, (pre ++ [.t kind]) ++ rhs) ∈ g.grammar.prods := by
          simpa [List.append_assoc] using hmem
        obtain ⟨rfl, rfl, rfl, rfl, rfl⟩ := ihArgs lhs (pre ++ [.t kind]) hmem' hfol₁ hargs₂ hfol₂ htail
        exact ⟨rfl, rfl, rfl, rfl, rfl⟩)
    (fun {_ child rhs _ _ tail₁ rest₁ _ _ _ _ _ _} _ hR₁ ihL ihR lhs pre hmem hfol₁ head₂ rest₂ args₂ steps₂ evs₂ h₂ hfol₂ hstr => by
      cases h₂ with
      | @child _ _ _ _ _ tail₂ _ _ _ _ _ _ _ hL₂ hR₂ =>
        have hchildFol₁ : Follow g.grammar child ((tail₁ ++ rest₁).head?.map fun t => t.value.kind) := by
          have hR₁' := derivesArgsTrace_to_derivesArgs hR₁
          cases tail₁ with
          | nil => exact .tail hmem (derives_nullable_of_nil.2 hR₁') hfol₁
          | cons tok tail₁' => exact .first hmem (derives_first_of_cons.2 hR₁')
        have hchildFol₂ : Follow g.grammar child ((tail₂ ++ rest₂).head?.map fun t => t.value.kind) := by
          have hR₂' := derivesArgsTrace_to_derivesArgs hR₂
          cases tail₂ with
          | nil => exact .tail hmem (derives_nullable_of_nil.2 hR₂') hfol₂
          | cons tok tail₂' => exact .first hmem (derives_first_of_cons.2 hR₂')
        rw [List.append_assoc, List.append_assoc] at hstr
        obtain ⟨rfl, hmid, rfl, rfl, rfl⟩ := ihL hchildFol₁ hL₂ hchildFol₂ hstr
        have hmem' : (lhs, (pre ++ [.n child]) ++ rhs) ∈ g.grammar.prods := by
          simpa [List.append_assoc] using hmem
        obtain ⟨rfl, rfl, rfl, rfl, rfl⟩ := ihR lhs (pre ++ [.n child]) hmem' hfol₁ hR₂ hfol₂ hmid
        exact ⟨rfl, rfl, rfl, rfl, rfl⟩)
    h₁

/-- For any LL(1) checked table, the derivation tree, step count, and exact pre-order
slice-requirement event trace of a token stream are uniquely determined across all slices. -/
theorem derivesTrace_unique {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) {eof : FileSpan} {s₁ s₂ : Slice} {tokens : List SurfaceToken}
    {tree₁ tree₂ : g.value g.start} {steps₁ steps₂ : Nat} {evs₁ evs₂ : List SliceEvent}
    (h₁ : DerivesTrace g s₁ eof g.start 0 tokens [] tree₁ steps₁ evs₁)
    (h₂ : DerivesTrace g s₂ eof g.start 0 tokens [] tree₂ steps₂ evs₂) :
    tree₁ = tree₂ ∧ steps₁ = steps₂ ∧ evs₁ = evs₂ := by
  obtain ⟨_, _, htree, hsteps, hevs⟩ := derivesTrace_unique_aux table h₁ Follow.start h₂ Follow.start rfl
  exact ⟨htree, hsteps, hevs⟩

private def replayEvents (target : Slice) (c : ParseCursor) (events : List SliceEvent) : ParseCursor :=
  events.foldl (fun acc ev => acc.require target ev.slice ev.cause ev.span) c

private theorem replayEvents_append (target : Slice) (c : ParseCursor) (xs ys : List SliceEvent) :
    replayEvents target c (xs ++ ys) = replayEvents target (replayEvents target c xs) ys :=
  List.foldl_append

private theorem replayEvents_only_req_hit (target : Slice) (c₁ c₂ : ParseCursor)
    (hreq : c₁.required = c₂.required) (hhit : c₁.firstUnsupportedHit = c₂.firstUnsupportedHit)
    (events : List SliceEvent) :
    (replayEvents target c₁ events).required = (replayEvents target c₂ events).required ∧
    (replayEvents target c₁ events).firstUnsupportedHit = (replayEvents target c₂ events).firstUnsupportedHit := by
  induction events generalizing c₁ c₂ with
  | nil => exact ⟨hreq, hhit⟩
  | cons ev rest ih =>
    simp only [replayEvents, List.foldl_cons]
    apply ih
    · simp only [ParseCursor.require, hreq]
    · simp only [ParseCursor.require, hhit]

private theorem tokenSlice_length (tokens : Array SurfaceToken) {lo hi : Nat}
    (h1 : lo ≤ hi) (h2 : hi ≤ tokens.size) :
    (tokenSlice tokens lo hi).length = hi - lo := by
  unfold tokenSlice
  rw [List.length_take, List.length_drop, Array.length_toList]
  omega

private theorem parseSymbol_parseArgs_ok_trace {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) (tokens : Array SurfaceToken) (targetSlice : Slice)
    (eof : FileSpan) (fuel : Nat) :
    (∀ (n : Fin count) (c c' : ParseCursor) (val : g.value n),
      c.offset ≤ tokens.size →
      parseSymbol table tokens targetSlice eof n c fuel = (c', .ok val) →
      ∃ dSteps events,
        DerivesTrace g c'.required eof n c.offset
          (tokenSlice tokens c.offset c'.offset) (tokens.toList.drop c'.offset)
          val dSteps events ∧
        (replayEvents targetSlice c events).required = c'.required ∧
        (replayEvents targetSlice c events).firstUnsupportedHit = c'.firstUnsupportedHit) ∧
    (∀ (prodId : ProductionId) (rhs : List (Sym Terminal (Fin count)))
      (c c' : ParseCursor) (args : ActionArgs g.value rhs),
      c.offset ≤ tokens.size →
      parseArgs table tokens targetSlice eof prodId fuel rhs c = (c', .ok args) →
      ∃ dSteps events,
        DerivesArgsTrace g c'.required eof prodId rhs c.offset
          (tokenSlice tokens c.offset c'.offset) (tokens.toList.drop c'.offset)
          args dSteps events ∧
        (replayEvents targetSlice c events).required = c'.required ∧
        (replayEvents targetSlice c events).firstUnsupportedHit = c'.firstUnsupportedHit) := by
  induction fuel with
  | zero =>
    exact ⟨fun _ _ _ _ _ h => (by cases h), fun _ _ _ _ _ _ h => (by cases h)⟩
  | succ fuel ih =>
    obtain ⟨ihSym, ihArgs⟩ := ih
    constructor
    · intro n c c' val hoff hrun
      unfold parseSymbol at hrun
      dsimp only at hrun
      cases hlook : table.lookup n (tokens[c.offset]?.map fun t => t.value.kind) with
      | none => simp [hlook] at hrun
      | some index =>
        simp only [hlook] at hrun
        cases hp : g.productions[index]? with
        | none => simp [hp] at hrun
        | some p =>
          simp only [hp] at hrun
          by_cases hlhs : p.lhs = n
          · simp only [hlhs, ↓reduceDIte] at hrun
            subst n
            let c₁ := ({c with steps := c.steps + 1}).require targetSlice p.firstSlice
              (.production p.id) ((tokens[c.offset]?.map Located.span).getD eof)
            cases hpa : parseArgs table tokens targetSlice eof p.id fuel p.rhs c₁ with
            | mk c₂ res =>
              dsimp only [c₁] at hpa
              rw [hpa] at hrun
              cases res with
              | error stop => cases hrun
              | ok args =>
                have hpair := Prod.mk.inj hrun
                have hc' : c' = {c₂ with steps := c₂.steps + (p.action.eval args).1} := hpair.1.symm
                have hval : val = (p.action.eval args).2 := (Except.ok.inj hpair.2).symm
                subst hc'
                subst hval
                have hmono := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel).2
                  p.id p.rhs c₁ hoff
                dsimp only [c₁] at hmono
                rw [hpa] at hmono
                dsimp only at hmono
                obtain ⟨dSteps, evs, htr, hreq, hhit⟩ :=
                  ihArgs p.id p.rhs c₁ c₂ args hoff hpa
                have hpMem : p ∈ g.productions.toList :=
                  Array.mem_toList_iff.mpr (Array.mem_of_getElem? hp)
                have hreq₁ : p.firstSlice.val ≤ c₁.required.val := require_required_ge_right
                have hfs : p.firstSlice.val ≤ c₂.required.val := Nat.le_trans hreq₁ hmono.2.2.2.1
                have hc₁Off : c₁.offset = c.offset := rfl
                have hhead :
                    ((tokenSlice tokens c.offset c₂.offset ++ tokens.toList.drop c₂.offset).head?.map
                      Located.span).getD eof =
                    (tokens[c.offset]?.map Located.span).getD eof := by
                  rw [tokenSlice_append_drop tokens (hc₁Off ▸ hmono.1), ← getElem?_eq_drop_head?]
                let ev₀ : SliceEvent :=
                  ⟨c.offset, ((tokenSlice tokens c.offset c₂.offset ++ tokens.toList.drop c₂.offset).head?.map
                    Located.span).getD eof, p.firstSlice, .production p.id⟩
                have hrep := replayEvents_only_req_hit targetSlice
                  (c.require targetSlice p.firstSlice (.production p.id) ev₀.span) c₁
                  rfl (by dsimp only [ev₀, c₁]; rw [hhead]; rfl) evs
                refine ⟨dSteps + 1 + (p.action.eval args).1, ev₀ :: evs,
                  .apply p c.offset _ _ args hpMem hfs htr,
                  hrep.1.trans hreq, hrep.2.trans hhit⟩
          · simp only [hlhs, ↓reduceDIte] at hrun
            cases hrun
    · intro prodId rhs c c' args hoff hrun
      cases rhs with
      | nil =>
        unfold parseArgs at hrun
        obtain ⟨hc', hargs⟩ := Prod.mk.inj hrun
        subst hc'
        cases args
        exact ⟨0, [], by simpa [tokenSlice_self] using DerivesArgsTrace.nil prodId c.offset (tokens.toList.drop c.offset),
          rfl, rfl⟩
      | cons sym rest =>
        cases sym with
        | t kind =>
          unfold parseArgs at hrun
          dsimp only at hrun
          cases htok : tokens[c.offset]? with
          | none => simp [htok] at hrun
          | some token =>
            simp only [htok] at hrun
            split at hrun
            · cases hrun
            · rename_i hnotNe
              have hkindEq : token.value.kind = kind := by
                cases hdec : decide (token.value.kind = kind) with
                | true => exact of_decide_eq_true hdec
                | false => exact False.elim (hnotNe (of_decide_eq_false hdec))
              have hlt : c.offset < tokens.size := (Array.getElem?_eq_some_iff.mp htok).1
              let c₁ := ({c with steps := c.steps + 1}).require targetSlice (g.required token.value)
                (tokenUnsupportedCause prodId token.value) token.span
              let c₂ := {c₁ with offset := c₁.offset + 1}
              cases hpa : parseArgs table tokens targetSlice eof prodId fuel rest c₂ with
              | mk c₃ res =>
                dsimp only [c₂, c₁] at hpa
                rw [hpa] at hrun
                cases res with
                | error stop => cases hrun
                | ok restArgs =>
                  obtain ⟨hc', hargs⟩ := Prod.mk.inj hrun
                  subst hc'
                  have hargsEq : (token, restArgs) = args := Except.ok.inj hargs
                  subst hargsEq
                  have hmono := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel).2
                    prodId rest c₂ hlt
                  dsimp only [c₂, c₁] at hmono
                  rw [hpa] at hmono
                  dsimp only at hmono
                  obtain ⟨dSteps, evs, htr, hreq, hhit⟩ :=
                    ihArgs prodId rest c₂ c₃ restArgs hlt hpa
                  have hreq₁ : (g.required token.value).val ≤ c₁.required.val := require_required_ge_right
                  have htokReq : (g.required token.value).val ≤ c₃.required.val :=
                    Nat.le_trans hreq₁ hmono.2.2.2.1
                  have hc₂Off : c₂.offset = c.offset + 1 := rfl
                  have hts : tokenSlice tokens c.offset c₃.offset =
                      token :: tokenSlice tokens (c.offset + 1) c₃.offset :=
                    tokenSlice_cons tokens htok (hc₂Off ▸ hmono.1)
                  rw [hts]
                  let ev₀ : SliceEvent :=
                    ⟨c.offset, token.span, g.required token.value, tokenUnsupportedCause prodId token.value⟩
                  have hrep := replayEvents_only_req_hit targetSlice
                    (c.require targetSlice (g.required token.value)
                      (tokenUnsupportedCause prodId token.value) token.span) c₂ rfl rfl evs
                  exact ⟨dSteps + 1, ev₀ :: evs, .token hkindEq htokReq htr,
                    hrep.1.trans hreq, hrep.2.trans hhit⟩
        | n child =>
          unfold parseArgs at hrun
          dsimp only at hrun
          cases hps : parseSymbol table tokens targetSlice eof child {c with steps := c.steps + 1} fuel with
          | mk c₁ res₁ =>
            rw [hps] at hrun
            cases res₁ with
            | error stop => cases hrun
            | ok childVal =>
              dsimp only at hrun
              have hmono₁ := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel).1
                child {c with steps := c.steps + 1} hoff
              rw [hps] at hmono₁
              cases hpa : parseArgs table tokens targetSlice eof prodId fuel rest c₁ with
              | mk c₂ res₂ =>
                rw [hpa] at hrun
                cases res₂ with
                | error stop => cases hrun
                | ok restArgs =>
                  obtain ⟨hc', hargs⟩ := Prod.mk.inj hrun
                  subst hc'
                  have hargsEq : (childVal, restArgs) = args := Except.ok.inj hargs
                  subst hargsEq
                  have hmono₂ := (parseSymbol_parseArgs_mono table tokens targetSlice eof fuel).2
                    prodId rest c₁ hmono₁.2.1
                  rw [hpa] at hmono₂
                  obtain ⟨dL, evsL, htrL, hreqL, hhitL⟩ :=
                    ihSym child {c with steps := c.steps + 1} c₁ childVal hoff hps
                  obtain ⟨dR, evsR, htrR, hreqR, hhitR⟩ :=
                    ihArgs prodId rest c₁ c₂ restArgs hmono₁.2.1 hpa
                  have htrL' := derivesTrace_mono hmono₂.2.2.2.1 htrL
                  have hts : tokenSlice tokens c.offset c₂.offset =
                      tokenSlice tokens c.offset c₁.offset ++ tokenSlice tokens c₁.offset c₂.offset :=
                    (tokenSlice_append tokens hmono₁.1 hmono₂.1).symm
                  have hdrop : tokens.toList.drop c₁.offset =
                      tokenSlice tokens c₁.offset c₂.offset ++ tokens.toList.drop c₂.offset :=
                    (tokenSlice_append_drop tokens hmono₂.1).symm
                  have hposR : c.offset + (tokenSlice tokens c.offset c₁.offset).length = c₁.offset := by
                    have h1 : c.offset ≤ c₁.offset := hmono₁.1
                    have h2 : c₁.offset ≤ tokens.size := hmono₁.2.1
                    rw [tokenSlice_length tokens h1 h2]
                    omega
                  have htrR' : DerivesArgsTrace g c₂.required eof prodId rest
                      (c.offset + (tokenSlice tokens c.offset c₁.offset).length)
                      (tokenSlice tokens c₁.offset c₂.offset) (tokens.toList.drop c₂.offset)
                      restArgs dR evsR := by
                    simpa [hposR] using htrR
                  rw [hts]
                  rw [hdrop] at htrL'
                  have hrep₁ := replayEvents_only_req_hit targetSlice c {c with steps := c.steps + 1} rfl rfl evsL
                  have hrep₂ := replayEvents_only_req_hit targetSlice
                    (replayEvents targetSlice c evsL) c₁
                    (hrep₁.1.trans hreqL) (hrep₁.2.trans hhitL) evsR
                  refine ⟨dL + dR, evsL ++ evsR, .child htrL' htrR', ?_, ?_⟩
                  · rw [replayEvents_append]
                    exact hrep₂.1.trans hreqR
                  · rw [replayEvents_append]
                    exact hrep₂.2.trans hhitR

private theorem replayEvents_required_spec (target : Slice) (c : ParseCursor) (events : List SliceEvent) :
    c.required.val ≤ (replayEvents target c events).required.val ∧
    (∀ ev ∈ events, ev.slice.val ≤ (replayEvents target c events).required.val) ∧
    ((replayEvents target c events).required = c.required ∨
      ∃ ev ∈ events, ev.slice = (replayEvents target c events).required) := by
  induction events generalizing c with
  | nil => exact ⟨Nat.le_refl _, (fun _ h => nomatch h), Or.inl rfl⟩
  | cons ev rest ih =>
    let c₁ := c.require target ev.slice ev.cause ev.span
    have hstep : replayEvents target c (ev :: rest) = replayEvents target c₁ rest := rfl
    rw [hstep]
    obtain ⟨hge₁, hall₁, hor₁⟩ := ih c₁
    have hcReq : c.required.val ≤ c₁.required.val := require_required_ge_left
    have hevReq : ev.slice.val ≤ c₁.required.val := require_required_ge_right
    refine ⟨Nat.le_trans hcReq hge₁, ?_, ?_⟩
    · intro e he
      rcases List.mem_cons.mp he with rfl | hmem
      · exact Nat.le_trans hevReq hge₁
      · exact hall₁ e hmem
    · rcases hor₁ with heq | ⟨e, he, hslice⟩
      · rw [heq]
        unfold c₁ ParseCursor.require
        dsimp only
        split
        · exact Or.inr ⟨ev, List.mem_cons_self, rfl⟩
        · exact Or.inl rfl
      · exact Or.inr ⟨e, List.mem_cons_of_mem _ he, hslice⟩

private theorem replayEvents_firstHit_spec (target : Slice) (c : ParseCursor)
    (hnone : c.firstUnsupportedHit = none) (events : List SliceEvent)
    {hitSpan : FileSpan} {cause : UnsupportedCause}
    (hsome : (replayEvents target c events).firstUnsupportedHit = some (hitSpan, cause)) :
    ∃ pre hit post,
      events = pre ++ hit :: post ∧
      (∀ ev ∈ pre, ev.slice.val ≤ target.val) ∧
      target.val < hit.slice.val ∧
      hit.span = hitSpan ∧
      hit.cause = cause := by
  induction events generalizing c with
  | nil =>
    simp [replayEvents, hnone] at hsome
  | cons ev rest ih =>
    simp only [replayEvents, List.foldl_cons] at hsome
    by_cases hlt : target.val < ev.slice.val
    · have hhit₁ : (c.require target ev.slice ev.cause ev.span).firstUnsupportedHit =
          some (ev.span, ev.cause) := by
        simp [ParseCursor.require, hnone, hlt]
      have hpres : ∀ (xs : List SliceEvent) (acc : ParseCursor),
          acc.firstUnsupportedHit = some (ev.span, ev.cause) →
          (replayEvents target acc xs).firstUnsupportedHit = some (ev.span, ev.cause) := by
        intro xs
        induction xs with
        | nil => intro acc h; exact h
        | cons x xs ihx =>
          intro acc h
          exact ihx _ (require_hit_some h)
      have heq := (hpres rest _ hhit₁).symm.trans hsome
      obtain ⟨hsp, hcs⟩ := Prod.mk.inj (Option.some.inj heq)
      exact ⟨[], ev, rest, rfl, (fun _ h => nomatch h), hlt, hsp, hcs⟩
    · have hhit₁ : (c.require target ev.slice ev.cause ev.span).firstUnsupportedHit = none := by
        simp [ParseCursor.require, hnone, hlt]
      obtain ⟨pre, hit, post, rfl, hall, hhitLt, hsp, hcs⟩ := ih _ hhit₁ hsome
      refine ⟨ev :: pre, hit, post, rfl, ?_, hhitLt, hsp, hcs⟩
      intro e he
      rcases List.mem_cons.mp he with rfl | hmem
      · exact Nat.le_of_not_lt hlt
      · exact hall e hmem

/-- Registered refusal rule selected by `ParserEngine.run` for an unsupported cause. -/
def ParserEngine.unsupportedRule {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) : UnsupportedCause → RegisteredRule
  | .production _ => parser.unsupportedProductionRule
  | .nonDecimal _ => parser.unsupportedLiteralRule

/-- First out-of-slice violation in source order: every unsupported refusal comes from
the unique pre-order `DerivesTrace` of the stream; every event in that trace before the
reported hit is in-slice, the hit is `firstOutOfSlice input.source.slice events`, its
rule, cause, and span are reported verbatim, and `required` is the exact maximum slice
across the trace. -/
theorem parserEngine_unsupported_first_witness {count : Nat} {g : SharedGrammar count}
    (parser : ParserEngine g) (input : ParserView)
    (rule : RegisteredRule) (cause : UnsupportedCause) (span : FileSpan) (required : Slice)
    (hrun : (parser.run input).2 = some (.error (.unsupported rule cause span required))) :
    input.WF ∧
    rule = parser.unsupportedRule cause ∧
    input.source.slice.val < required.val ∧
    (∃ tree steps events,
      DerivesTrace g required (eofFileSpan input.source) g.start 0 input.tokens.toList [] tree steps events) ∧
    ∀ s tree steps events,
      DerivesTrace g s (eofFileSpan input.source) g.start 0 input.tokens.toList [] tree steps events →
      events.Pairwise (fun a b => a.offset ≤ b.offset) ∧
      (∀ ev ∈ events, ev.slice.val ≤ required.val) ∧
      (∃ ev ∈ events, ev.slice = required) ∧
      ∃ pre hit post,
        events = pre ++ hit :: post ∧
        (∀ ev ∈ pre, ev.slice.val ≤ input.source.slice.val) ∧
        input.source.slice.val < hit.slice.val ∧
        hit.span = span ∧
        hit.cause = cause ∧
        firstOutOfSlice input.source.slice events = some hit ∧
        (∀ i, i < hit.offset → ∀ hi : i < input.tokens.size,
          (g.required (input.tokens[i]'(hi)).value).val ≤ input.source.slice.val) := by
  unfold ParserEngine.run at hrun
  cases hinv : input.invalidLocation with
  | some bad => simp [hinv] at hrun
  | none =>
    simp only [hinv] at hrun
    have hhitInv := parseSymbol_firstUnsupportedHit_isSome parser.table input.tokens
      input.source.slice (eofFileSpan input.source) g.start ⟨0, slice0, none, 0⟩
      (parser.stepsPerToken * (input.tokens.size + 1)) (by simp [slice0, Slice.val])
    cases hps : parseSymbol parser.table input.tokens input.source.slice (eofFileSpan input.source) g.start
        ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1)) with
    | mk c val =>
      simp only [hps] at hrun hhitInv
      cases val with
      | error stop => cases stop <;> simp at hrun
      | ok ast =>
        by_cases hoff : c.offset = input.tokens.size
        · have hbne : (c.offset != input.tokens.size) = false := by simp [hoff]
          simp only [hbne, Bool.false_eq_true, ↓reduceIte] at hrun
          cases hfh : c.firstUnsupportedHit with
          | none => simp [hfh] at hrun
          | some hitPair =>
            rcases hitPair with ⟨hitSpan, hitCause⟩
            simp only [hfh, Option.some.injEq, Except.error.injEq,
              ParseRefusal.unsupported.injEq] at hrun
            obtain ⟨rfl, rfl, rfl, rfl⟩ := hrun
            rw [hfh] at hhitInv
            have hltReq : input.source.slice.val < c.required.val :=
              of_decide_eq_true hhitInv.symm
            obtain ⟨dSteps, events₀, htr₀, hreq, hhit⟩ :=
              (parseSymbol_parseArgs_ok_trace parser.table input.tokens input.source.slice
                (eofFileSpan input.source) (parser.stepsPerToken * (input.tokens.size + 1))).1
                g.start ⟨0, slice0, none, 0⟩ c ast (Nat.zero_le _) hps
            rw [hoff, tokenSlice_full] at htr₀
            rw [List.drop_eq_nil_of_le (by simp)] at htr₀
            rw [hfh] at hhit
            refine ⟨hinv, by cases hitCause <;> rfl, hltReq, ⟨ast, dSteps, events₀, htr₀⟩, ?_⟩
            intro s tree steps events htr
            have hevsEq : events₀ = events := (derivesTrace_unique parser.table htr₀ htr).2.2
            subst hevsEq
            obtain ⟨hpw, _, hcov⟩ := derivesTrace_props htr₀
            obtain ⟨pre, hit, post, hevEq, hpreLe, hhitLt, hsp, hcs⟩ :=
              replayEvents_firstHit_spec input.source.slice ⟨0, slice0, none, 0⟩ rfl events₀ hhit
            obtain ⟨_, hallReq, horReq⟩ :=
              replayEvents_required_spec input.source.slice ⟨0, slice0, none, 0⟩ events₀
            rw [hreq] at hallReq horReq
            have hexMax : ∃ ev ∈ events₀, ev.slice = c.required := by
              rcases horReq with heq | hex
              · have h0 : c.required.val = 0 := congrArg Slice.val heq
                omega
              · exact hex
            have hfirst : firstOutOfSlice input.source.slice events₀ = some hit :=
              (firstOutOfSlice_eq_some_iff input.source.slice events₀ hit).mpr
                ⟨pre, post, hevEq, hpreLe, hhitLt⟩
            have htokBefore : ∀ i, i < hit.offset → ∀ hi : i < input.tokens.size,
                (g.required (input.tokens[i]'(hi)).value).val ≤ input.source.slice.val := by
              intro i hilt hi
              have hiList : i < input.tokens.toList.length := by simpa [Array.length_toList] using hi
              obtain ⟨ev, hev, hoffEv, hsliceEv⟩ := hcov i hiList
              rw [Nat.zero_add] at hoffEv
              rw [hevEq] at hev hpw
              have hnotPost : ∀ e ∈ hit :: post, hit.offset ≤ e.offset := by
                have hpwPost := (List.pairwise_append.mp hpw).2.1
                rcases List.pairwise_cons.mp hpwPost with ⟨hhead, _⟩
                intro e he
                rcases List.mem_cons.mp he with rfl | hmem
                · exact Nat.le_refl _
                · exact hhead e hmem
              rcases List.mem_append.mp hev with hpreMem | hpostMem
              · have hvalEq : (input.tokens.toList[i]'(hiList)) = input.tokens[i]'(hi) := by
                  simp [Array.getElem_toList]
                rw [hvalEq] at hsliceEv
                rw [← hsliceEv]
                exact hpreLe ev hpreMem
              · have hge := hnotPost ev hpostMem
                omega
            exact ⟨hpw, hallReq, hexMax, pre, hit, post, hevEq, hpreLe, hhitLt, hsp, hcs, hfirst, htokBefore⟩
        · have hbne : (c.offset != input.tokens.size) = true := bne_iff_ne.mpr hoff
          simp [hbne] at hrun

end ConcreteSpec.StageGrammar
