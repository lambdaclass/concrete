import Concrete.Interfaces.Pipeline

/-!
# Production SSA-to-BIR formation

Lowers any well-formed `SSA` program into `BIR` by:
1. enforcing the resolved profile's BIR frame-slot ceiling (`profile.maxBIRSlots`),
   returning the exact registered `ResourceRefusal` on `BIR.frameSlots` rev 1 when
   `profile.maxBIRSlots < ssa.frameSlots`;
2. deterministically allocating sequential `TrapIndex` slots in `BIR.sites` for every
   `.trap kind origin` terminator across all functions and blocks in stored order;
3. preserving zero-slot loops (including at `profile.maxBIRSlots = 0`), all block
   parameters, instructions, layouts, declarations, origins, and `mutableLocals`.

All theorems are constructive (no `Classical.choice`, no `sorry`) and validate
`(registry passes).buildBIR` universally over the frozen `SSA.WF` domain for every
slice and profile.
-/

namespace Concrete.BIR.Build

open ConcreteSpec.Core ConcreteSpec.StageInterface Concrete.Interfaces

/-- Abbreviation for a BIR CFG block. -/
abbrev BIRBlock {Ty : Type} (lang : LoweringLanguage Ty) :=
  Block (Sigma (SSAValue Ty)) (LocatedOp (SSAInst lang))
    (BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool))

/-- Registered resource refusal emitted when `profile.maxBIRSlots < ssa.frameSlots`. -/
def frameSlotRefusal (measured limit : Nat) : ResourceRefusal :=
  ⟨⟨``BIR.frameSlots, 1⟩, measured, limit⟩

/-- Rule identifier cited if an off-domain SSA graph with an `.invalid` terminator
is passed directly to `buildBIR`. Unreachable on any `SSA.WF` input. -/
def invalidTerminatorRule : RuleId :=
  ⟨``Terminator.invalid, 1⟩

/-- Lower one SSA terminator to a BIR terminator, appending a fresh `TrapEntry` to
`sites` for each `.trap kind origin` terminator. -/
def lowerTerminator {Ty : Type} {lang : LoweringLanguage Ty}
    (ending : Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool))
    (sites : TrapTable) :
    Except StageFailure
      (BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool) × TrapTable) :=
  match ending with
  | .jump edge => .ok (.jump edge, sites)
  | .branch test yes no => .ok (.branch test yes no, sites)
  | .ret value => .ok (.ret value, sites)
  | .trap kind origin => .ok (.trap ⟨sites.size⟩, sites.push ⟨kind, origin⟩)
  | .invalid => .error (.invalidInput invalidTerminatorRule)

/-- Tail-recursive block lowering accumulating lowered BIR blocks and trap sites. -/
def lowerBlocksAux {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : List (SSABlock lang)) (acc : Array (BIRBlock lang)) (sites : TrapTable) :
    Except StageFailure (Array (BIRBlock lang) × TrapTable) :=
  match blocks with
  | [] => .ok (acc, sites)
  | block :: rest =>
    match lowerTerminator block.ending sites with
    | .error e => .error e
    | .ok (ending, sites') =>
      let lowered : BIRBlock lang :=
        ⟨block.id, block.params, block.body, ending, block.boundaryOrigins⟩
      lowerBlocksAux rest (acc.push lowered) sites'

/-- Lower all blocks of an SSA CFG in stored order. -/
def lowerBlocks {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : Array (SSABlock lang)) (sites : TrapTable) :
    Except StageFailure (Array (BIRBlock lang) × TrapTable) :=
  lowerBlocksAux blocks.toList #[] sites

/-- Tail-recursive function-table lowering threading the global BIR trap table. -/
def lowerFunctionsAux {Ty : Type} {lang : LoweringLanguage Ty}
    (fns : AList FnId (LocalTypes Ty × SSAGraph lang))
    (revAcc : AList FnId (LocalTypes Ty × BIRGraph lang))
    (sites : TrapTable) :
    Except StageFailure (AList FnId (LocalTypes Ty × BIRGraph lang) × TrapTable) :=
  match fns with
  | [] => .ok (revAcc.reverse, sites)
  | (fn, locals, graph) :: rest =>
    match lowerBlocks graph.blocks sites with
    | .error e => .error e
    | .ok (blocks', sites') =>
      lowerFunctionsAux rest ((fn, locals, ⟨graph.entry, blocks'⟩) :: revAcc) sites'

/-- Lower an entire SSA function table to BIR graphs plus the allocated trap table. -/
def lowerFunctions {Ty : Type} {lang : LoweringLanguage Ty}
    (fns : AList FnId (LocalTypes Ty × SSAGraph lang)) (sites : TrapTable) :
    Except StageFailure (AList FnId (LocalTypes Ty × BIRGraph lang) × TrapTable) :=
  lowerFunctionsAux fns [] sites

/-- Production SSA-to-BIR lowering pass. Enforces `profile.maxBIRSlots`, allocates
sequential trap-table entries across all functions and blocks, and preserves zero-slot
loops, declarations, layouts, origins, and `mutableLocals`. -/
def buildBIR {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (ssa : SSA lang profile) : Except StageFailure (BIR lang profile) :=
  if profile.maxBIRSlots < ssa.frameSlots then
    .error (.resource (frameSlotRefusal ssa.frameSlots profile.maxBIRSlots))
  else
    match lowerFunctions ssa.functions #[] with
    | .error e => .error e
    | .ok (functions, sites) =>
      .ok
        { project := ssa.project
          entry := ssa.entry
          signatures := ssa.signatures
          functions := functions
          layouts := ssa.layouts
          sites := sites
          mutableLocals := ssa.mutableLocals
          declarations := ssa.declarations }

/-! ### Structural specification and relational invariants -/

private inductive ListRel {α β : Type} (R : α → β → Prop) : List α → List β → Prop where
  | nil : ListRel R [] []
  | cons {a : α} {b : β} {as : List α} {bs : List β} :
      R a b → ListRel R as bs → ListRel R (a :: as) (b :: bs)

private theorem listRel_imp {α β : Type} {R S : α → β → Prop}
    (hImp : ∀ a b, R a b → S a b) {xs : List α} {ys : List β}
    (h : ListRel R xs ys) : ListRel S xs ys := by
  induction h with
  | nil => exact .nil
  | cons hab _ ih => exact .cons (hImp _ _ hab) ih

private theorem listRel_map_eq {α β γ : Type} {R : α → β → Prop}
    {xs : List α} {ys : List β} (h : ListRel R xs ys)
    {f : α → γ} {g : β → γ} (heq : ∀ a b, R a b → f a = g b) :
    xs.map f = ys.map g := by
  induction h with
  | nil => rfl
  | cons hab _ ih =>
    simp only [List.map_cons, heq _ _ hab, ih]

private theorem listRel_flatMap_eq {α β γ : Type} {R : α → β → Prop}
    {xs : List α} {ys : List β} (h : ListRel R xs ys)
    {f : α → List γ} {g : β → List γ} (heq : ∀ a b, R a b → f a = g b) :
    xs.flatMap f = ys.flatMap g := by
  induction h with
  | nil => rfl
  | cons hab _ ih =>
    simp only [List.flatMap_cons, heq _ _ hab, ih]

private theorem listRel_foldl_eq {α β σ : Type} {R : α → β → Prop}
    {xs : List α} {ys : List β} (h : ListRel R xs ys)
    {f : σ → α → σ} {g : σ → β → σ} (heq : ∀ s a b, R a b → f s a = g s b)
    (init : σ) :
    xs.foldl f init = ys.foldl g init := by
  induction h generalizing init with
  | nil => rfl
  | cons hab _ ih =>
    simp only [List.foldl_cons, heq init _ _ hab]
    exact ih (g init _)

private theorem listRel_exists_left {α β : Type} {R : α → β → Prop}
    {xs : List α} {ys : List β} (h : ListRel R xs ys) {y : β} (hy : y ∈ ys) :
    ∃ x ∈ xs, R x y := by
  induction h with
  | nil => cases hy
  | cons hab _ ih =>
    cases hy with
    | head => exact ⟨_, List.mem_cons_self, hab⟩
    | tail _ htail =>
      obtain ⟨x, hx, hxy⟩ := ih htail
      exact ⟨x, List.mem_cons_of_mem _ hx, hxy⟩

private theorem listRel_exists_right {α β : Type} {R : α → β → Prop}
    {xs : List α} {ys : List β} (h : ListRel R xs ys) {x : α} (hx : x ∈ xs) :
    ∃ y ∈ ys, R x y := by
  induction h with
  | nil => cases hx
  | cons hab _ ih =>
    cases hx with
    | head => exact ⟨_, List.mem_cons_self, hab⟩
    | tail _ htail =>
      obtain ⟨y, hy, hxy⟩ := ih htail
      exact ⟨y, List.mem_cons_of_mem _ hy, hxy⟩

private theorem lt_size_of_getElem?_eq_some {α : Type} {xs : Array α} {i : Nat} {x : α}
    (h : xs[i]? = some x) : i < xs.size := by
  by_cases hlt : i < xs.size
  · exact hlt
  · rw [Array.getElem?_eq_none (Nat.le_of_not_lt hlt)] at h
    cases h

private def SiteExtends (oldSites newSites : TrapTable) : Prop :=
  ∀ (i : Nat) (entry : TrapEntry), oldSites[i]? = some entry → newSites[i]? = some entry

private theorem siteExtends_refl (sites : TrapTable) : SiteExtends sites sites :=
  fun _ _ h => h

private theorem siteExtends_trans {s₁ s₂ s₃ : TrapTable}
    (h₁ : SiteExtends s₁ s₂) (h₂ : SiteExtends s₂ s₃) : SiteExtends s₁ s₃ :=
  fun i e h => h₂ i e (h₁ i e h)

private theorem siteExtends_push (sites : TrapTable) (entry : TrapEntry) :
    SiteExtends sites (sites.push entry) := by
  intro i e hi
  have hlt := lt_size_of_getElem?_eq_some hi
  rw [Array.getElem?_push_lt hlt, ← Array.getElem?_eq_getElem hlt]
  exact hi

private def TermLowers {Ty : Type} {lang : LoweringLanguage Ty} (sites : TrapTable) :
    Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool) →
    BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool) → Prop
  | .jump e, .jump e' => e' = e
  | .branch c y n, .branch c' y' n' => c' = c ∧ y' = y ∧ n' = n
  | .ret v, .ret v' => v' = v
  | .trap k o, .trap slot => sites[slot.index]? = some ⟨k, o⟩
  | _, _ => False

private theorem termLowers_mono {Ty : Type} {lang : LoweringLanguage Ty}
    {s₁ s₂ : TrapTable} (hExt : SiteExtends s₁ s₂)
    {t₁ : Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)}
    {t₂ : BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)}
    (h : TermLowers s₁ t₁ t₂) : TermLowers s₂ t₁ t₂ := by
  cases t₁ <;> cases t₂ <;> first | exact h | exact hExt _ _ h

private def BlockLowers {Ty : Type} {lang : LoweringLanguage Ty} (sites : TrapTable)
    (src : SSABlock lang) (dst : BIRBlock lang) : Prop :=
  dst.id = src.id ∧
  dst.params = src.params ∧
  dst.body = src.body ∧
  dst.boundaryOrigins = src.boundaryOrigins ∧
  TermLowers sites src.ending dst.ending

private theorem blockLowers_mono {Ty : Type} {lang : LoweringLanguage Ty}
    {s₁ s₂ : TrapTable} (hExt : SiteExtends s₁ s₂)
    {b₁ : SSABlock lang} {b₂ : BIRBlock lang}
    (h : BlockLowers s₁ b₁ b₂) : BlockLowers s₂ b₁ b₂ :=
  ⟨h.1, h.2.1, h.2.2.1, h.2.2.2.1, termLowers_mono hExt h.2.2.2.2⟩

private def FnLowers {Ty : Type} {lang : LoweringLanguage Ty} (sites : TrapTable)
    (src : FnId × LocalTypes Ty × SSAGraph lang)
    (dst : FnId × LocalTypes Ty × BIRGraph lang) : Prop :=
  dst.1 = src.1 ∧
  dst.2.1 = src.2.1 ∧
  dst.2.2.entry = src.2.2.entry ∧
  ListRel (BlockLowers sites) src.2.2.blocks.toList dst.2.2.blocks.toList

private theorem fnLowers_mono {Ty : Type} {lang : LoweringLanguage Ty}
    {s₁ s₂ : TrapTable} (hExt : SiteExtends s₁ s₂)
    {f₁ : FnId × LocalTypes Ty × SSAGraph lang}
    {f₂ : FnId × LocalTypes Ty × BIRGraph lang}
    (h : FnLowers s₁ f₁ f₂) : FnLowers s₂ f₁ f₂ :=
  ⟨h.1, h.2.1, h.2.2.1, listRel_imp (fun _ _ hb => blockLowers_mono hExt hb) h.2.2.2⟩

private def lowerBlockList {Ty : Type} {lang : LoweringLanguage Ty} :
    List (SSABlock lang) → TrapTable → Except StageFailure (List (BIRBlock lang) × TrapTable)
  | [], sites => .ok ([], sites)
  | block :: rest, sites =>
    match lowerTerminator block.ending sites with
    | .error e => .error e
    | .ok (ending, sites₁) =>
      match lowerBlockList rest sites₁ with
      | .error e => .error e
      | .ok (rest', sites₂) =>
        .ok (⟨block.id, block.params, block.body, ending, block.boundaryOrigins⟩ :: rest', sites₂)

private def lowerFunctionsSpec {Ty : Type} {lang : LoweringLanguage Ty} :
    AList FnId (LocalTypes Ty × SSAGraph lang) → TrapTable →
    Except StageFailure (AList FnId (LocalTypes Ty × BIRGraph lang) × TrapTable)
  | [], sites => .ok ([], sites)
  | (fn, locals, graph) :: rest, sites =>
    match lowerBlockList graph.blocks.toList sites with
    | .error e => .error e
    | .ok (blocks', sites₁) =>
      match lowerFunctionsSpec rest sites₁ with
      | .error e => .error e
      | .ok (rest', sites₂) =>
        .ok ((fn, locals, ⟨graph.entry, blocks'.toArray⟩) :: rest', sites₂)

private theorem array_push_append_toArray {α : Type} (acc : Array α) (x : α) (xs : List α) :
    acc.push x ++ xs.toArray = acc ++ (x :: xs).toArray := by
  apply Array.ext'
  simp [Array.toList_append]

private theorem lowerBlocksAux_eq {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : List (SSABlock lang)) (acc : Array (BIRBlock lang)) (sites : TrapTable) :
    lowerBlocksAux blocks acc sites =
      (lowerBlockList blocks sites).map fun pair => (acc ++ pair.1.toArray, pair.2) := by
  induction blocks generalizing acc sites with
  | nil =>
    simp [lowerBlocksAux, lowerBlockList, Except.map]
  | cons block rest ih =>
    unfold lowerBlocksAux lowerBlockList
    cases ht : lowerTerminator block.ending sites with
    | error e =>
      simp [Except.map]
    | ok pair =>
      rcases pair with ⟨ending, sites₁⟩
      dsimp only
      rw [ih (acc.push ⟨block.id, block.params, block.body, ending, block.boundaryOrigins⟩) sites₁]
      cases hr : lowerBlockList rest sites₁ with
      | error e => simp [Except.map]
      | ok res =>
        rcases res with ⟨rest', sites₂⟩
        simp only [Except.map, Except.ok.injEq, Prod.mk.injEq]
        exact ⟨array_push_append_toArray acc _ rest', trivial⟩

private theorem lowerBlocks_eq {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : Array (SSABlock lang)) (sites : TrapTable) :
    lowerBlocks blocks sites =
      (lowerBlockList blocks.toList sites).map fun pair => (pair.1.toArray, pair.2) := by
  unfold lowerBlocks
  rw [lowerBlocksAux_eq]
  cases lowerBlockList blocks.toList sites with
  | error e => rfl
  | ok pair =>
    simp only [Except.map, Except.ok.injEq, Prod.mk.injEq]
    exact ⟨Array.empty_append, trivial⟩

private theorem lowerFunctionsAux_eq {Ty : Type} {lang : LoweringLanguage Ty}
    (fns : AList FnId (LocalTypes Ty × SSAGraph lang))
    (revAcc : AList FnId (LocalTypes Ty × BIRGraph lang)) (sites : TrapTable) :
    lowerFunctionsAux fns revAcc sites =
      (lowerFunctionsSpec fns sites).map fun pair => (revAcc.reverse ++ pair.1, pair.2) := by
  induction fns generalizing revAcc sites with
  | nil =>
    simp [lowerFunctionsAux, lowerFunctionsSpec, Except.map]
  | cons entry rest ih =>
    rcases entry with ⟨fn, locals, graph⟩
    unfold lowerFunctionsAux lowerFunctionsSpec
    rw [lowerBlocks_eq]
    cases hb : lowerBlockList graph.blocks.toList sites with
    | error e => simp [Except.map]
    | ok bpair =>
      rcases bpair with ⟨blocks', sites₁⟩
      simp only [Except.map]
      rw [ih ((fn, locals, ⟨graph.entry, blocks'.toArray⟩) :: revAcc) sites₁]
      cases hr : lowerFunctionsSpec rest sites₁ with
      | error e => simp [Except.map]
      | ok rpair =>
        rcases rpair with ⟨rest', sites₂⟩
        simp only [Except.map, List.reverse_cons, List.append_assoc, List.singleton_append]

private theorem lowerFunctions_eq {Ty : Type} {lang : LoweringLanguage Ty}
    (fns : AList FnId (LocalTypes Ty × SSAGraph lang)) (sites : TrapTable) :
    lowerFunctions fns sites = lowerFunctionsSpec fns sites := by
  unfold lowerFunctions
  rw [lowerFunctionsAux_eq]
  cases lowerFunctionsSpec fns sites with
  | error e => rfl
  | ok pair => simp [Except.map]

private theorem lowerTerminator_spec {Ty : Type} {lang : LoweringLanguage Ty}
    {ending : Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)}
    {sites sites' : TrapTable}
    {birEnding : BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)}
    (h : lowerTerminator ending sites = .ok (birEnding, sites')) :
    SiteExtends sites sites' ∧
    TermLowers sites' ending birEnding ∧
    (∀ entry ∈ sites'.toList, entry ∈ sites.toList ∨ ending = .trap entry.kind entry.origin) := by
  cases ending with
  | jump edge =>
    simp only [lowerTerminator, Except.ok.injEq, Prod.mk.injEq] at h
    rcases h with ⟨rfl, rfl⟩
    exact ⟨siteExtends_refl _, rfl, fun _ hm => Or.inl hm⟩
  | branch test yes no =>
    simp only [lowerTerminator, Except.ok.injEq, Prod.mk.injEq] at h
    rcases h with ⟨rfl, rfl⟩
    exact ⟨siteExtends_refl _, ⟨rfl, rfl, rfl⟩, fun _ hm => Or.inl hm⟩
  | ret value =>
    simp only [lowerTerminator, Except.ok.injEq, Prod.mk.injEq] at h
    rcases h with ⟨rfl, rfl⟩
    exact ⟨siteExtends_refl _, rfl, fun _ hm => Or.inl hm⟩
  | trap kind origin =>
    simp only [lowerTerminator, Except.ok.injEq, Prod.mk.injEq] at h
    rcases h with ⟨rfl, rfl⟩
    refine ⟨siteExtends_push sites ⟨kind, origin⟩, ?_, ?_⟩
    · change (sites.push ⟨kind, origin⟩)[sites.size]? = some ⟨kind, origin⟩
      rw [Array.getElem?_push_size]
    · intro entry hm
      rw [Array.toList_push, List.mem_append, List.mem_singleton] at hm
      rcases hm with hold | rfl
      · exact Or.inl hold
      · exact Or.inr rfl
  | invalid =>
    simp [lowerTerminator] at h

private theorem lowerTerminator_ok_of_ne_invalid {Ty : Type} {lang : LoweringLanguage Ty}
    {ending : Terminator (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool)}
    (hne : ending ≠ .invalid) (sites : TrapTable) :
    ∃ birEnding sites', lowerTerminator ending sites = .ok (birEnding, sites') := by
  cases ending with
  | jump edge => exact ⟨.jump edge, sites, rfl⟩
  | branch test yes no => exact ⟨.branch test yes no, sites, rfl⟩
  | ret value => exact ⟨.ret value, sites, rfl⟩
  | trap kind origin => exact ⟨.trap ⟨sites.size⟩, sites.push ⟨kind, origin⟩, rfl⟩
  | invalid => exact (hne rfl).elim

private theorem lowerBlockList_spec {Ty : Type} {lang : LoweringLanguage Ty}
    {blocks : List (SSABlock lang)} {sites sites' : TrapTable}
    {blocks' : List (BIRBlock lang)}
    (h : lowerBlockList blocks sites = .ok (blocks', sites')) :
    SiteExtends sites sites' ∧
    ListRel (BlockLowers sites') blocks blocks' ∧
    (∀ entry ∈ sites'.toList,
      entry ∈ sites.toList ∨ ∃ b ∈ blocks, b.ending = .trap entry.kind entry.origin) := by
  induction blocks generalizing sites blocks' sites' with
  | nil =>
    simp only [lowerBlockList, Except.ok.injEq, Prod.mk.injEq] at h
    rcases h with ⟨rfl, rfl⟩
    exact ⟨siteExtends_refl _, .nil, fun _ hm => Or.inl hm⟩
  | cons block rest ih =>
    unfold lowerBlockList at h
    cases ht : lowerTerminator block.ending sites with
    | error e => rw [ht] at h; cases h
    | ok tpair =>
      rcases tpair with ⟨ending, sites₁⟩
      rw [ht] at h
      dsimp only at h
      cases hr : lowerBlockList rest sites₁ with
      | error e => rw [hr] at h; cases h
      | ok rpair =>
        rcases rpair with ⟨rest', sites₂⟩
        rw [hr] at h
        simp only [Except.ok.injEq, Prod.mk.injEq] at h
        rcases h with ⟨rfl, rfl⟩
        obtain ⟨hExt₁, hTerm, hSrc₁⟩ := lowerTerminator_spec ht
        obtain ⟨hExt₂, hRel, hSrc₂⟩ := ih hr
        refine ⟨siteExtends_trans hExt₁ hExt₂,
          .cons ⟨rfl, rfl, rfl, rfl, termLowers_mono hExt₂ hTerm⟩ hRel, ?_⟩
        intro entry hm
        rcases hSrc₂ entry hm with hIn₁ | ⟨b, hb, hEnd⟩
        · rcases hSrc₁ entry hIn₁ with hIn₀ | hEnd
          · exact Or.inl hIn₀
          · exact Or.inr ⟨block, List.mem_cons_self, hEnd⟩
        · exact Or.inr ⟨b, List.mem_cons_of_mem _ hb, hEnd⟩

private theorem lowerBlockList_ok_of_no_invalid {Ty : Type} {lang : LoweringLanguage Ty}
    {blocks : List (SSABlock lang)}
    (hNoInv : ∀ b ∈ blocks, b.ending ≠ .invalid) (sites : TrapTable) :
    ∃ blocks' sites', lowerBlockList blocks sites = .ok (blocks', sites') := by
  induction blocks generalizing sites with
  | nil => exact ⟨[], sites, rfl⟩
  | cons block rest ih =>
    obtain ⟨ending, sites₁, ht⟩ :=
      lowerTerminator_ok_of_ne_invalid (hNoInv block List.mem_cons_self) sites
    obtain ⟨rest', sites₂, hr⟩ :=
      ih (fun b hb => hNoInv b (List.mem_cons_of_mem _ hb)) sites₁
    exact ⟨⟨block.id, block.params, block.body, ending, block.boundaryOrigins⟩ :: rest',
      sites₂, by simp [lowerBlockList, ht, hr]⟩

private theorem lowerFunctionsSpec_spec {Ty : Type} {lang : LoweringLanguage Ty}
    {fns : AList FnId (LocalTypes Ty × SSAGraph lang)} {sites sites' : TrapTable}
    {fns' : AList FnId (LocalTypes Ty × BIRGraph lang)}
    (h : lowerFunctionsSpec fns sites = .ok (fns', sites')) :
    SiteExtends sites sites' ∧
    ListRel (FnLowers sites') fns fns' ∧
    (∀ entry ∈ sites'.toList,
      entry ∈ sites.toList ∨
      ∃ fn ∈ fns, ∃ b ∈ fn.2.2.blocks.toList, b.ending = .trap entry.kind entry.origin) := by
  induction fns generalizing sites fns' sites' with
  | nil =>
    simp only [lowerFunctionsSpec, Except.ok.injEq, Prod.mk.injEq] at h
    rcases h with ⟨rfl, rfl⟩
    exact ⟨siteExtends_refl _, .nil, fun _ hm => Or.inl hm⟩
  | cons fnEntry rest ih =>
    rcases fnEntry with ⟨fn, locals, graph⟩
    unfold lowerFunctionsSpec at h
    cases hb : lowerBlockList graph.blocks.toList sites with
    | error e => rw [hb] at h; cases h
    | ok bpair =>
      rcases bpair with ⟨blocks', sites₁⟩
      rw [hb] at h
      dsimp only at h
      cases hr : lowerFunctionsSpec rest sites₁ with
      | error e => rw [hr] at h; cases h
      | ok rpair =>
        rcases rpair with ⟨rest', sites₂⟩
        rw [hr] at h
        simp only [Except.ok.injEq, Prod.mk.injEq] at h
        rcases h with ⟨rfl, rfl⟩
        obtain ⟨hExt₁, hBlkRel, hSrc₁⟩ := lowerBlockList_spec hb
        obtain ⟨hExt₂, hFnRel, hSrc₂⟩ := ih hr
        have hHead : FnLowers sites₂ (fn, locals, graph) (fn, locals, ⟨graph.entry, blocks'.toArray⟩) :=
          ⟨rfl, rfl, rfl, by
            simpa using
              listRel_imp (fun _ _ hbl => blockLowers_mono hExt₂ hbl) hBlkRel⟩
        refine ⟨siteExtends_trans hExt₁ hExt₂, .cons hHead hFnRel, ?_⟩
        intro entry hm
        rcases hSrc₂ entry hm with hIn₁ | ⟨fn', hfn', b, hb', hEnd⟩
        · rcases hSrc₁ entry hIn₁ with hIn₀ | ⟨b, hb', hEnd⟩
          · exact Or.inl hIn₀
          · exact Or.inr ⟨(fn, locals, graph), List.mem_cons_self, b, hb', hEnd⟩
        · exact Or.inr ⟨fn', List.mem_cons_of_mem _ hfn', b, hb', hEnd⟩

private theorem lowerFunctionsSpec_ok_of_no_invalid {Ty : Type} {lang : LoweringLanguage Ty}
    {fns : AList FnId (LocalTypes Ty × SSAGraph lang)}
    (hNoInv : ∀ fn ∈ fns, ∀ b ∈ fn.2.2.blocks.toList, b.ending ≠ .invalid)
    (sites : TrapTable) :
    ∃ fns' sites', lowerFunctionsSpec fns sites = .ok (fns', sites') := by
  induction fns generalizing sites with
  | nil => exact ⟨[], sites, rfl⟩
  | cons fnEntry rest ih =>
    rcases fnEntry with ⟨fn, locals, graph⟩
    obtain ⟨blocks', sites₁, hb⟩ :=
      lowerBlockList_ok_of_no_invalid (hNoInv (fn, locals, graph) List.mem_cons_self) sites
    obtain ⟨rest', sites₂, hr⟩ :=
      ih (fun f hf => hNoInv f (List.mem_cons_of_mem _ hf)) sites₁
    exact ⟨(fn, locals, ⟨graph.entry, blocks'.toArray⟩) :: rest', sites₂,
      by simp [lowerFunctionsSpec, hb, hr]⟩

/-! ### Decomposition of successful `buildBIR` -/

private theorem buildBIR_ok_inv {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile}
    (h : buildBIR ssa = .ok bir) :
    ssa.frameSlots ≤ profile.maxBIRSlots ∧
    bir.project = ssa.project ∧
    bir.entry = ssa.entry ∧
    bir.signatures = ssa.signatures ∧
    bir.layouts = ssa.layouts ∧
    bir.mutableLocals = ssa.mutableLocals ∧
    bir.declarations = ssa.declarations ∧
    ListRel (FnLowers bir.sites) ssa.functions bir.functions ∧
    (∀ entry ∈ bir.sites.toList,
      ∃ fn ∈ ssa.functions, ∃ b ∈ fn.2.2.blocks.toList, b.ending = .trap entry.kind entry.origin) := by
  unfold buildBIR at h
  split at h
  · cases h
  · rename_i hNotLt
    have hLe : ssa.frameSlots ≤ profile.maxBIRSlots := Nat.le_of_not_lt hNotLt
    rw [lowerFunctions_eq] at h
    cases hl : lowerFunctionsSpec ssa.functions #[] with
    | error e => rw [hl] at h; cases h
    | ok pair =>
      rcases pair with ⟨fns, sites⟩
      rw [hl] at h
      cases Except.ok.inj h
      obtain ⟨_, hRel, hSrc⟩ := lowerFunctionsSpec_spec hl
      refine ⟨hLe, rfl, rfl, rfl, rfl, rfl, rfl, hRel, ?_⟩
      intro entry hm
      rcases hSrc entry hm with hEmpty | hFound
      · cases hEmpty
      · exact hFound

/-! ### Core structural and semantic theorems -/

/-- Exceeding `profile.maxBIRSlots` produces the exact registered resource refusal. -/
theorem buildBIR_refusal_of_gt {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {ssa : SSA lang profile} (hGt : profile.maxBIRSlots < ssa.frameSlots) :
    buildBIR ssa = .error (.resource (frameSlotRefusal ssa.frameSlots profile.maxBIRSlots)) := by
  unfold buildBIR
  rw [ite_eq_left hGt]

/-- Any well-formed SSA program within `profile.maxBIRSlots` succeeds under `buildBIR`. -/
theorem buildBIR_ok_of_wf {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {ssa : SSA lang profile}
    (hWF : SSA.WF (language := language) ssa)
    (hLe : ssa.frameSlots ≤ profile.maxBIRSlots) :
    ∃ bir, buildBIR ssa = .ok bir := by
  have hNotLt : ¬ profile.maxBIRSlots < ssa.frameSlots := Nat.not_lt.mpr hLe
  have hNoInv : ∀ fn ∈ ssa.functions, ∀ b ∈ fn.2.2.blocks.toList, b.ending ≠ .invalid := by
    intro fn hfn b hb
    rcases fn with ⟨fnId, locals, graph⟩
    exact hWF.registerFormation.no_invalid_terminator hfn hb
  obtain ⟨fns', sites', hl⟩ := lowerFunctionsSpec_ok_of_no_invalid hNoInv #[]
  refine ⟨⟨ssa.project, ssa.entry, ssa.signatures, fns', ssa.layouts, sites',
    ssa.mutableLocals, ssa.declarations⟩, ?_⟩
  unfold buildBIR
  rw [ite_eq_right hNotLt, lowerFunctions_eq, hl]

/-- `buildBIR` preserves `mutableLocals` identically. -/
theorem buildBIR_mutableLocals_eq {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile} (h : buildBIR ssa = .ok bir) :
    bir.mutableLocals = ssa.mutableLocals :=
  (buildBIR_ok_inv h).2.2.2.2.2.1

/-- Every entry in `bir.sites` originates from a `.trap` terminator in `ssa`. -/
theorem buildBIR_sites_from_ssa {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile} (h : buildBIR ssa = .ok bir) :
    ∀ entry ∈ bir.sites.toList,
      ∃ fn ∈ ssa.functions, ∃ block ∈ fn.2.2.blocks.toList,
        block.ending = .trap entry.kind entry.origin :=
  (buildBIR_ok_inv h).2.2.2.2.2.2.2.2

/-- Every trap terminator in the produced BIR resolves in `bir.sites`. -/
theorem buildBIR_trapSlotsWF {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile} (h : buildBIR ssa = .ok bir) :
    bir.TrapSlotsWF := by
  have hRel := (buildBIR_ok_inv h).2.2.2.2.2.2.2.1
  intro fn' hfn' blk' hblk' slot hEnd
  obtain ⟨fn, _, hFn⟩ := listRel_exists_left hRel hfn'
  obtain ⟨blk, _, hBlk⟩ := listRel_exists_left hFn.2.2.2 hblk'
  have hTerm := hBlk.2.2.2.2
  rw [hEnd] at hTerm
  cases hSrcEnd : blk.ending with
  | jump _ => rw [hSrcEnd] at hTerm; exact False.elim hTerm
  | branch _ _ _ => rw [hSrcEnd] at hTerm; exact False.elim hTerm
  | ret _ => rw [hSrcEnd] at hTerm; exact False.elim hTerm
  | invalid => rw [hSrcEnd] at hTerm; exact False.elim hTerm
  | trap kind origin =>
    rw [hSrcEnd] at hTerm
    exact ⟨⟨kind, origin⟩, hTerm⟩

/-- Every SSA trap terminator maps to a matching block, trap slot, and `TrapEntry` in BIR. -/
theorem buildBIR_trapsMatch {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile} (h : buildBIR ssa = .ok bir) :
    bir.TrapsMatch ssa := by
  have hRel := (buildBIR_ok_inv h).2.2.2.2.2.2.2.1
  intro fn hfn blk hblk kind origin hEnd
  obtain ⟨fn', hfn', hFn⟩ := listRel_exists_right hRel hfn
  obtain ⟨blk', hblk', hBlk⟩ := listRel_exists_right hFn.2.2.2 hblk
  have hTerm := hBlk.2.2.2.2
  rw [hEnd] at hTerm
  cases hDstEnd : blk'.ending with
  | jump _ => rw [hDstEnd] at hTerm; exact False.elim hTerm
  | branch _ _ _ => rw [hDstEnd] at hTerm; exact False.elim hTerm
  | ret _ => rw [hDstEnd] at hTerm; exact False.elim hTerm
  | trap slot =>
    rw [hDstEnd] at hTerm
    exact ⟨fn', hfn', hFn.1, blk', hblk', hBlk.1, slot, hDstEnd, ⟨kind, origin⟩, hTerm, rfl, rfl⟩

/-- `buildBIR` preserves the exact list of origins across all functions and blocks. -/
theorem buildBIR_origins_eq {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile} (h : buildBIR ssa = .ok bir) :
    bir.origins = ssa.origins := by
  have hRel := (buildBIR_ok_inv h).2.2.2.2.2.2.2.1
  unfold BIR.origins SSA.origins CFG.origins
  symm
  refine listRel_flatMap_eq hRel fun fn fn' hFn => ?_
  refine listRel_flatMap_eq hFn.2.2.2 fun blk blk' hBlk => ?_
  have hTermEq :
      Terminator.origins blk.ending =
        (fun x => match x with
          | .trap slot => (bir.sites[slot.index]?.map TrapEntry.origin).toList
          | _ => []) blk'.ending := by
    have hTerm := hBlk.2.2.2.2
    cases hSrc : blk.ending <;> cases hDst : blk'.ending <;> rw [hSrc, hDst] at hTerm
    all_goals first
      | exact False.elim hTerm
      | rfl
      | (unfold TermLowers at hTerm; simp [Terminator.origins, hTerm])
  rw [hBlk.2.2.1, hBlk.2.2.2.1, hTermEq]
  rfl

/-- `buildBIR` preserves the exact frame-slot count `BIR.frameSlots = SSA.frameSlots`. -/
theorem buildBIR_frameSlots_eq {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile} (h : buildBIR ssa = .ok bir) :
    bir.frameSlots = ssa.frameSlots := by
  have hRel := (buildBIR_ok_inv h).2.2.2.2.2.2.2.1
  symm
  change
    ssa.functions.foldl (fun largest (_, _, graph) =>
      max largest (graph.blocks.foldl (fun slots block =>
        block.body.foldl (fun count inst => count + inst.instruction.slotCount)
          (slots + block.params.length)) 0)) 0 =
    bir.functions.foldl (fun largest (_, _, graph) =>
      max largest (graph.blocks.foldl (fun slots block =>
        block.body.foldl (fun count inst => count + inst.instruction.slotCount)
          (slots + block.params.length)) 0)) 0
  refine listRel_foldl_eq hRel (fun largest fn fn' hFn => ?_) 0
  rcases fn with ⟨_, _, graph⟩
  rcases fn' with ⟨_, _, graph'⟩
  dsimp only
  congr 1
  rw [← Array.foldl_toList, ← Array.foldl_toList]
  refine listRel_foldl_eq hFn.2.2.2 (fun slots blk blk' hBlk => ?_) 0
  rw [hBlk.2.1, hBlk.2.2.1]

/-- `buildBIR` preserves the exact `RegisterIR.normalize` image used by `RegisterFormation`. -/
theorem buildBIR_normalize_eq {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile} (h : buildBIR ssa = .ok bir) :
    (RegisterIR.bir bir).normalize = (RegisterIR.ssa ssa).normalize := by
  obtain ⟨_, _, hEntry, hSig, hLay, hMut, hDecl, hRel, _⟩ := buildBIR_ok_inv h
  unfold RegisterIR.normalize
  dsimp only
  rw [hEntry, hSig, hLay, hMut, hDecl]
  congr 1
  symm
  refine listRel_map_eq hRel fun fn fn' hFn => ?_
  rcases fn with ⟨fnId, locals, graph⟩
  rcases fn' with ⟨fnId', locals', graph'⟩
  obtain ⟨rfl, rfl, hGraphEntry, hBlocks⟩ := hFn
  dsimp only at hGraphEntry hBlocks ⊢
  congr 2
  cases graph; cases graph'
  dsimp only at hGraphEntry hBlocks ⊢
  rw [hGraphEntry]
  congr 1
  refine listRel_map_eq hBlocks fun blk blk' hBlk => ?_
  obtain ⟨hId, hParams, hBody, hBound, hTerm⟩ := hBlk
  rw [hId, hParams, hBody, hBound]
  congr 1
  cases hSrc : blk.ending <;> cases hDst : blk'.ending <;> rw [hSrc, hDst] at hTerm
  all_goals first
    | exact False.elim hTerm
    | (rcases hTerm with rfl; rfl)
    | (rcases hTerm with ⟨rfl, rfl, rfl⟩; rfl)
    | (unfold TermLowers at hTerm
       have hLt := lt_size_of_getElem?_eq_some hTerm
       simp [RegTerminator.ofSSA, RegTerminator.ofBIR, hLt])

private theorem array_map_eq_of_listRel {α β γ : Type} {R : α → β → Prop}
    {xs : Array α} {ys : Array β} (h : ListRel R xs.toList ys.toList)
    {f : α → γ} {g : β → γ} (heq : ∀ a b, R a b → f a = g b) :
    xs.map f = ys.map g := by
  apply Array.ext'
  rw [Array.toList_map, Array.toList_map]
  exact listRel_map_eq h heq

private theorem listRel_views_eq {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {ssa : SSA lang profile} {bir : BIR lang profile}
    (hRel : ListRel (FnLowers bir.sites) ssa.functions bir.functions)
    {γ : Type}
    {f : (FnId × LocalTypes language.operands.Ty × SSAGraph lang) → γ}
    {g : (FnId × LocalTypes language.operands.Ty × BIRGraph lang) → γ}
    (heq : ∀ fn fn', FnLowers bir.sites fn fn' → f fn = g fn') :
    List.map g bir.functions = List.map f ssa.functions :=
  (listRel_map_eq hRel heq).symm

/-- `buildBIR` produces a `BIR` whose decoded `memoryView` is identical to `ssa.memoryView`. -/
theorem buildBIR_memoryView_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile}
    (h : buildBIR ssa = .ok bir) :
    bir.memoryView (language := language) = ssa.memoryView (language := language) := by
  obtain ⟨_, hProj, hEntry, hSig, hLay, hMut, hDecl, hRel, _⟩ := buildBIR_ok_inv h
  have hBaseEq : ∀ {graph : SSAGraph lang} {graph' : BIRGraph lang}
      (step : Nat → LocatedOp (SSAInst lang) → Nat) (init : Nat),
      ListRel (BlockLowers bir.sites) graph.blocks.toList graph'.blocks.toList →
      graph.blocks.foldl (fun limit block => block.body.foldl step limit) init =
      graph'.blocks.foldl (fun limit block => block.body.foldl step limit) init := by
    intro graph graph' step init hBlks
    rw [← Array.foldl_toList, ← Array.foldl_toList]
    refine listRel_foldl_eq hBlks (fun limit b b' hb => ?_) init
    rw [hb.2.2.1]
  conv => lhs; whnf
  conv => rhs; whnf
  rw [hProj, hEntry, hSig, hLay, hMut, hDecl]
  let mkMem :
      List ((FnId × LocalTypes language.operands.Ty × MemoryGraph lang) ×
        (FnId × AList PlaceId StorageClass) × (FnId × List PlaceId)) →
      Memory lang profile := fun views =>
    let mutableLocals := views.foldl (fun table (_, _, fn, registers) =>
      if registers.isEmpty then table
      else (fn, registers ++ (AList.lookup table fn).getD []) :: AList.erase table fn) ssa.mutableLocals
    ⟨ssa.project, ssa.entry, ssa.signatures, views.map Prod.fst, ssa.layouts,
      views.map (·.2.1), mutableLocals, ssa.declarations⟩
  apply congrArg mkMem
  refine listRel_views_eq hRel fun fn fn' hFn => ?_
  rcases fn with ⟨fnId, locals, graph⟩
  rcases fn' with ⟨fnId', locals', graph'⟩
  obtain ⟨rfl, rfl, hGraphEntry, hBlks⟩ := hFn
  dsimp only at hGraphEntry hBlks ⊢
  rw [hBaseEq _ _ hBlks, hGraphEntry]
  refine Prod.ext
    (Prod.ext rfl (Prod.ext (congrArg (locals' ++ ·) ?hRegs) ?hBody))
    (Prod.ext
      (Prod.ext rfl (congrArg (List.map (fun p => (p.1, StorageClass.materialized)) locals' ++ ·)
        (congrArg (List.map (fun p => (p.1, StorageClass.virtual))) ?hRegs)))
      (Prod.ext rfl (congrArg (List.map Prod.fst) ?hRegs)))
  case hRegs =>
    refine listRel_flatMap_eq hBlks fun b b' hb => ?_
    rw [hb.2.1, hb.2.2.1]
  case hBody =>
    dsimp only
    congr 1
    refine array_map_eq_of_listRel hBlks fun b b' hb => ?_
    obtain ⟨hId, hParams, hBody, hBound, hTerm⟩ := hb
    rw [hId, hParams, hBody, hBound]
    congr 1
    cases hSrc : b.ending <;> cases hDst : b'.ending <;> rw [hSrc, hDst] at hTerm
    all_goals first
      | exact False.elim hTerm
      | (rcases hTerm with rfl; rfl)
      | (rcases hTerm with ⟨rfl, rfl, rfl⟩; rfl)
      | (unfold TermLowers at hTerm; dsimp only; conv => rhs; whnf; rw [hTerm])

/-- The decoded `SSA.memoryView` preserves the exact origin list of `ssa`. -/
theorem ssa_memoryView_origins_eq {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    (ssa : SSA lang profile) :
    (ssa.memoryView (language := language)).origins = ssa.origins := by
  unfold Memory.origins SSA.origins CFG.origins
  conv => lhs; arg 2; whnf
  rw [List.flatMap_map, List.flatMap_map]
  congr 1
  funext fn
  rcases fn with ⟨fnId, locals, graph⟩
  dsimp only
  rw [Array.toList_map, List.flatMap_map]
  congr 1
  funext blk
  dsimp only
  rw [List.map_map]
  cases blk.ending <;> rfl

/-- `buildBIR` establishes the full `BuildsBIR` contract on every successful output. -/
theorem buildBIR_buildsBIR {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile}
    (h : buildBIR ssa = .ok bir) :
    BuildsBIR (language := language) ssa bir := by
  obtain ⟨hLe, hProj, hEntry, _, _, _, hDecl, _, _⟩ := buildBIR_ok_inv h
  have hSlotsEq := buildBIR_frameSlots_eq h
  have hViewEq := buildBIR_memoryView_eq (language := language) h
  have hOrigEq := buildBIR_origins_eq h
  refine ⟨hProj, buildBIR_trapSlotsWF h, buildBIR_trapsMatch h,
    ⟨fun _ ho => hOrigEq ▸ ho, fun _ ho => hOrigEq.symm ▸ ho⟩,
    hSlotsEq.symm ▸ hLe,
    ⟨hEntry, hDecl, by rw [hViewEq, hEntry]⟩,
    BIR.refinesOfViewEq hViewEq.symm⟩

/-- `buildBIR` preserves well-formedness from `SSA.WF` to `BIR.WF`. -/
theorem buildBIR_wf {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile}
    (hWF : SSA.WF (language := language) ssa)
    (h : buildBIR ssa = .ok bir) :
    BIR.WF (language := language) bir := by
  obtain ⟨hLe, hProj, _, _, _, _, _, _, hSites⟩ := buildBIR_ok_inv h
  have hNormEq := buildBIR_normalize_eq h
  have hViewEq := buildBIR_memoryView_eq (language := language) h
  have hSlotsEq := buildBIR_frameSlots_eq h
  have hOrigEq := buildBIR_origins_eq h
  have hMemOrig := ssa_memoryView_origins_eq (language := language) ssa
  have hBindSSA : ∀ origin ∈ ssa.origins, ∃ span, origin.lexical.Binds ssa.project span := by
    intro origin ho
    exact hWF.memoryView.origins_bind origin (hMemOrig.symm ▸ ho)
  refine ⟨?_, buildBIR_trapSlotsWF h, hSlotsEq.symm ▸ hLe, ?_, ?_, ?_⟩
  · unfold BIR.RegisterFormation RegisterFormation
    rw [hNormEq]
    exact hWF.registerFormation
  · intro origin ho
    rw [hProj]
    exact hBindSSA origin (hOrigEq ▸ ho)
  · intro site hsite
    obtain ⟨fn, hfn, blk, hblk, hEnd⟩ := hSites site hsite
    have hInSSA : site.origin ∈ ssa.origins := by
      unfold SSA.origins CFG.origins
      refine List.mem_flatMap.mpr ⟨fn, hfn, List.mem_flatMap.mpr ⟨blk, hblk, ?_⟩⟩
      rw [hEnd]
      exact List.mem_append_right _ List.mem_cons_self
    rw [hProj]
    exact hBindSSA site.origin hInSSA
  · rw [hViewEq]
    exact hWF.memoryView

/-- On well-formed SSA, `buildBIR` can only fail by exceeding `profile.maxBIRSlots`,
returning the exact registered `BIR.frameSlots` rev 1 resource refusal. -/
theorem buildBIR_error_spec {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {ssa : SSA lang profile} {failure : StageFailure}
    (hWF : SSA.WF (language := language) ssa)
    (hErr : buildBIR ssa = .error failure) :
    ∃ bound, failure = .resource bound ∧
      bound.rule = ⟨``BIR.frameSlots, 1⟩ ∧
      bound.limit = profile.maxBIRSlots ∧
      bound.measured = ssa.frameSlots ∧
      bound.limit < bound.measured := by
  by_cases hGt : profile.maxBIRSlots < ssa.frameSlots
  · have hRef := buildBIR_refusal_of_gt hGt
    rw [hRef] at hErr
    cases Except.error.inj hErr
    exact ⟨frameSlotRefusal ssa.frameSlots profile.maxBIRSlots, rfl, rfl, rfl, rfl, hGt⟩
  · obtain ⟨bir, hOk⟩ := buildBIR_ok_of_wf hWF (Nat.le_of_not_lt hGt)
    rw [hOk] at hErr
    cases hErr

/-- Any well-formed zero-slot SSA program (including a one-block zero-slot self-loop)
succeeds under `buildBIR` at every profile, including `profile.maxBIRSlots = 0`. -/
theorem buildBIR_zero_slot_ok {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {ssa : SSA lang profile}
    (hWF : SSA.WF (language := language) ssa)
    (hZero : ssa.frameSlots = 0) :
    ∃ bir, buildBIR ssa = .ok bir ∧
      bir.frameSlots = 0 ∧
      BIR.WF (language := language) bir ∧
      BuildsBIR (language := language) ssa bir := by
  obtain ⟨bir, hOk⟩ := buildBIR_ok_of_wf hWF (hZero ▸ Nat.zero_le _)
  exact ⟨bir, hOk, (buildBIR_frameSlots_eq hOk).trans hZero,
    buildBIR_wf hWF hOk, buildBIR_buildsBIR hOk⟩

/-! ### Universal registered stage validation and soundness -/

/-- Universal validation of the registered `buildBIR` stage for any slice, any profile,
and any pass bundle wired to `Concrete.BIR.Build.buildBIR`. -/
theorem buildBIR_validated {slice : Slice} {profile : Profile}
    (passes : Passes slice profile)
    (hBuild : passes.buildBIR = buildBIR) :
    (registry passes).buildBIR.Validated := by
  constructor
  · intro ssa bir hWF hRan
    change passes.buildBIR ssa = .ok bir at hRan
    rw [hBuild] at hRan
    exact ⟨buildBIR_wf hWF hRan, buildBIR_buildsBIR hRan, buildBIR_mutableLocals_eq hRan⟩
  · intro ssa failure hWF hRan
    change passes.buildBIR ssa = .error failure at hRan
    rw [hBuild] at hRan
    obtain ⟨bound, rfl, hRule, hLim, hMeas, hLt⟩ := buildBIR_error_spec hWF hRan
    exact ⟨hRule, hLim, hMeas, hLt⟩

/-- Universal `Stage.Sound` for the registered `.buildBIR` edge. -/
theorem buildBIR_sound {slice : Slice} {profile : Profile}
    (passes : Passes slice profile)
    (hBuild : passes.buildBIR = buildBIR) :
    Stage.Sound passes .buildBIR :=
  ⟨buildBIR_validated passes hBuild, fun _ _ _ _ => trivial⟩

end Concrete.BIR.Build
