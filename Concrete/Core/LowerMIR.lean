import Concrete.Interfaces.Pipeline
import Concrete.IntegratedCore.Agreement
import ConcreteSpec.IntegratedCore

/-!
# Production Core-to-MIR Lowering (Checked and Unchecked Routes)

Production Core-to-MIR lowering for the frozen early Core fragments (Slices 0 and 1):
- `lowerCore` lowers any `Admitted (sliceLanguage slice)` respecting partial or full
  discharge maps (`Admitted.Valid`).
- `lowerUnchecked` lowers any `CoreInput (sliceLanguage slice)` on the unchecked route
  with empty discharges (`CoreInput.UncheckedWF`), keeping every addition overflow check
  `.residual`.
- Both share `lowerWith` and satisfy universal `MIR.HandoffWF`, `LowersCore` /
  `LowersUnchecked`, `Stage.Validated`, and `Stage.Sound` over the frozen contracts.
-/

namespace Concrete.Core.LowerMIR

open ConcreteSpec
open ConcreteSpec.Core
open ConcreteSpec.Core.SourcePathSpan
open ConcreteSpec.StageInterface
open Concrete.Contracts
open Concrete.Interfaces
open Concrete.IntegratedCore

/-! ### Representation helpers and production lowering -/

/-- The early `u8` scalar type in `EarlyTy`. -/
abbrev byteTy : EarlyTy := .scalar (.int .u8)

/-- Qualified nominal declaration identity in the standalone early entry module. -/
abbrev nominalId (name : ConcreteSpec.Name) : DeclId := ⟨earlyEntry.module, name⟩

/-- The early nominal type for a struct name in the standalone entry module. -/
abbrev nominalTy (name : ConcreteSpec.Name) : EarlyTy := .nominal (nominalId name)

/-- MIR terminator type for the slice lowering language. -/
abbrev MIRTerm (slice : Slice) :=
  Terminator (Sigma (Read (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).literal))
    (Read (sliceLanguage slice).operands.Ty (sliceLoweringLanguage slice).literal
      (sliceLoweringLanguage slice).bool)

/-- MIR block type for the slice lowering language. -/
abbrev MIRBlock (slice : Slice) :=
  Block (Sigma (Place (sliceLanguage slice).operands.Ty))
    (LocatedOp (MIRInst (sliceLoweringLanguage slice))) (MIRTerm slice)

/-- Core little-endian literal encoding for a `u8` value. -/
def encodeU8Literal (n : Int) : List UInt8 :=
  (Mem.leBytes Core.IntTy.u8.byteSize (toBits .u8 n)).map fun b => UInt8.ofNat b.val

/-- Lower a Slice 0/1 scalar atom to a typed MIR read. -/
def lowerAtom : Slice0.Atom → Read EarlyTy (Literal EarlyTy) byteTy
  | .lit n => .literal ⟨encodeU8Literal n⟩
  | .var x => .copy ⟨⟨x.id⟩, []⟩

/-- Select the check disposition for an addition at `site` from the supplied discharge map.
Only sites backed by a non-empty discharge group are emitted `.discharged`; every other
site retains a `.residual` overflow check. -/
def addCheck {slice : Slice} (discharges : Discharges (sliceLanguage slice)) (site : SiteId) :
    OperationCheck :=
  match AList.lookup discharges site with
  | some (_ :: _) => ⟨0, .overflow, .discharged⟩
  | _ => ⟨0, .overflow, .residual⟩

/-- Lowered contribution of a sequence of straight-line Core bindings. -/
structure LowerRow (slice : Slice) where
  locals : LocalTypes EarlyTy
  insts : List (LocatedOp (MIRInst (sliceLoweringLanguage slice)))
  boundary : List Origin

/-- Lower one straight-line Core binding at site `pos` with its located `origin`. -/
def lowerBinding {slice : Slice} (discharges : Discharges (sliceLanguage slice))
    (freshBase pos : Nat) (origin : Origin) :
    StraightBinding → Except StageFailure (LowerRow slice)
  | .slice0 ⟨x, .atom a⟩ =>
    .ok ⟨[(⟨x.id⟩, byteTy)],
      [⟨origin, .bind ⟨⟨x.id⟩, []⟩ (lowerAtom a)⟩],
      []⟩
  | .slice0 ⟨x, .add l r⟩ =>
    .ok ⟨[(⟨x.id⟩, byteTy)],
      [⟨origin, .op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨⟨x.id⟩, []⟩ ()
        (.cons (lowerAtom l) (.cons (lowerAtom r) .nil)) [addCheck discharges ⟨pos⟩] []⟩],
      []⟩
  | .bind binder .u8 (.scalar (.atom a)) =>
    let dst : PlaceId := match binder with
      | .name x => ⟨x.id⟩
      | .wild => ⟨freshBase + pos⟩
    .ok ⟨[(dst, byteTy)],
      [⟨origin, .bind ⟨dst, []⟩ (lowerAtom a)⟩],
      []⟩
  | .bind binder .u8 (.scalar (.add l r)) =>
    let dst : PlaceId := match binder with
      | .name x => ⟨x.id⟩
      | .wild => ⟨freshBase + pos⟩
    .ok ⟨[(dst, byteTy)],
      [⟨origin, .op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨dst, []⟩ ()
        (.cons (lowerAtom l) (.cons (lowerAtom r) .nil)) [addCheck discharges ⟨pos⟩] []⟩],
      []⟩
  | .bind (.name x) (.nominal name) (.move y) =>
    .ok ⟨[(⟨x.id⟩, nominalTy name)],
      [⟨origin, .bind (ty := nominalTy name) ⟨⟨x.id⟩, []⟩ (.move ⟨⟨y.id⟩, []⟩)⟩],
      []⟩
  | .bind (.name x) (.nominal name) (.construct ctor) =>
    if ctor = name then
      .ok ⟨[(⟨x.id⟩, nominalTy name)],
        [⟨origin, .op (opcode := .named ⟨"constructEmpty"⟩) (ty := nominalTy name) ⟨⟨x.id⟩, []⟩ () .nil [] []⟩],
        []⟩
    else
      .error (.invalidInput ⟨`Concrete.Core.LowerMIR.lowerBinding, 1⟩)
  | .destructure name (.move y) =>
    .ok ⟨[],
      [⟨origin, .destructure (ty := nominalTy name) ⟨⟨y.id⟩, []⟩ []⟩],
      []⟩
  | .destructure name (.construct ctor) =>
    if ctor = name then
      .ok ⟨[], [], [origin]⟩
    else
      .error (.invalidInput ⟨`Concrete.Core.LowerMIR.lowerBinding, 2⟩)
  | _ =>
    .error (.invalidInput ⟨`Concrete.Core.LowerMIR.lowerBinding, 3⟩)

/-- Lower a straight-line binding list together with its aligned binding origins. -/
def lowerBindings {slice : Slice} (discharges : Discharges (sliceLanguage slice)) (freshBase : Nat) :
    List LocatedStraightBinding → List (SiteId × Origin) → Nat → Except StageFailure (LowerRow slice)
  | [], [], _ => .ok ⟨[], [], []⟩
  | row :: rest, (_, origin) :: restOrigins, pos => do
    let head ← lowerBinding discharges freshBase pos origin row.statement
    let tail ← lowerBindings discharges freshBase rest restOrigins (pos + 1)
    pure ⟨head.locals ++ tail.locals, head.insts ++ tail.insts, head.boundary ++ tail.boundary⟩
  | _, _, _ => .error (.invalidInput ⟨`Concrete.Core.LowerMIR.lowerBindings, 1⟩)

/-- Lower a straight-line ending to a MIR terminator and any boundary origins not carried
by the terminator itself. -/
def lowerEnding {slice : Slice} (p : StraightProgram) :
    MIRTerm slice × List Origin :=
  match p.ending with
  | .ret a => (.ret (some ⟨byteTy, lowerAtom a⟩), [p.endingOrigin.2])
  | .abort => (.trap .abort p.endingOrigin.2, [])

/-- Lower a canonical `StraightProgram` and discharge table into a single-block MIR module. -/
def lowerProgram {slice : Slice} {profile : Profile} (project : Project) (p : StraightProgram)
    (discharges : Discharges (sliceLanguage slice)) :
    Except StageFailure (MIR (sliceLoweringLanguage slice) profile) := do
  let body ← lowerBindings discharges p.freshBase p.bindings p.bindingOrigins 0
  let (ending, endBoundary) := lowerEnding (slice := slice) p
  let block : MIRBlock slice := {
    id := ⟨0⟩
    params := []
    body := body.insts
    ending := ending
    boundaryOrigins := body.boundary ++ endBoundary
  }
  let graph : MIRGraph (sliceLoweringLanguage slice) := {
    entry := ⟨0⟩
    blocks := #[block]
  }
  pure {
    declarations := p.decls
    project := project
    entry := earlyEntry
    signatures := [(earlyEntry, ⟨[], some byteTy⟩)]
    functions := [(earlyEntry, (body.locals, graph))]
    mutableLocals := []
  }

/-- Shared lowering entry parameterized by the discharge map. -/
def lowerWith {slice : Slice} {profile : Profile} (core : CoreInput (sliceLanguage slice))
    (discharges : Discharges (sliceLanguage slice)) :
    Except StageFailure (MIR (sliceLoweringLanguage slice) profile) :=
  match core.decode with
  | .error reason => .error (.admission (.fragment reason))
  | .ok view => lowerProgram core.project view.located discharges

/-- Production checked Core-to-MIR lowering stage (`Admitted.Valid → MIR.HandoffWF`). -/
def lowerCore {slice : Slice} {profile : Profile} (input : Admitted (sliceLanguage slice)) :
    Except StageFailure (MIR (sliceLoweringLanguage slice) profile) :=
  lowerWith input.core input.discharges

/-- Production unchecked Core-to-MIR lowering stage (`CoreInput.UncheckedWF → MIR.HandoffWF`). -/
def lowerUnchecked {slice : Slice} {profile : Profile} (input : CoreInput (sliceLanguage slice)) :
    Except StageFailure (MIR (sliceLoweringLanguage slice) profile) :=
  lowerWith input []

/-! ### Constructive list and `AList` helpers -/

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
    (hnd : (List.map Prod.fst l).Nodup) {k : κ} {v : α} (hmem : (k, v) ∈ l) :
    AList.lookup l k = some v := by
  induction l with
  | nil => cases hmem
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    rw [List.map_cons, List.nodup_cons] at hnd
    simp only [AList.lookup]
    cases hmem with
    | head => simp only [↓reduceIte]
    | tail _ htail =>
      split
      · rename_i heq; subst heq
        exact absurd (List.mem_map.mpr ⟨(k', v), htail, rfl⟩) hnd.1
      · exact ih hnd.2 htail

private theorem alist_lookup_erase_ne {κ α : Type} [DecidableEq κ] (l : AList κ α) {k k' : κ}
    (hne : k ≠ k') : AList.lookup (AList.erase l k) k' = AList.lookup l k' := by
  induction l with
  | nil => rfl
  | cons hd tl ih =>
    rcases hd with ⟨x, v⟩
    simp only [AList.erase]
    by_cases hx : x = k
    · subst hx
      simp [hne, AList.lookup, ih]
    · simp only [hx, ↓reduceIte, AList.lookup]
      split
      · rfl
      · exact ih

private theorem alist_lookup_update {κ α : Type} [DecidableEq κ] (l : AList κ α)
    (x : κ) (f : α → α) (y : κ) :
    AList.lookup (AList.update l x f) y =
      if y = x then (AList.lookup l y).map f else AList.lookup l y := by
  induction l with
  | nil => simp [AList.update, AList.lookup]
  | cons p rest ih =>
    rcases p with ⟨k, e⟩
    simp only [AList.update]
    by_cases hk : k = x
    · subst hk
      simp only [↓reduceIte, AList.lookup]
      by_cases hy : k = y
      · subst hy; simp
      · have hy' : y ≠ k := fun h => hy h.symm
        simp [hy, hy']
    · simp only [hk, ↓reduceIte, AList.lookup]
      by_cases hy : k = y
      · subst hy
        have hyx : k ≠ x := hk
        simp [hyx]
      · simp [hy, ih]

private theorem alist_lookup_map {κ α β : Type} [DecidableEq κ] (l : AList κ α) (f : α → β) (k : κ) :
    AList.lookup (AList.map f l) k = (AList.lookup l k).map f := by
  induction l with
  | nil => rfl
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [AList.map, List.map_cons, AList.lookup]
    split
    · rfl
    · exact ih

private theorem list_nodup_map_inj {α β : Type} {f : α → β} (hinj : ∀ a b, f a = f b → a = b)
    {xs : List α} (hnd : xs.Nodup) : (xs.map f).Nodup := by
  induction xs with
  | nil => exact .nil
  | cons x xs ih =>
    rw [List.nodup_cons] at hnd
    rw [List.map_cons, List.nodup_cons]
    refine ⟨?_, ih hnd.2⟩
    intro hm
    obtain ⟨y, hyMem, hyEq⟩ := List.mem_map.mp hm
    exact hnd.1 (hinj y x hyEq ▸ hyMem)

private theorem list_nodup_erase {α : Type} [DecidableEq α] {l : List α} (hnd : l.Nodup) (a : α) :
    (l.erase a).Nodup := by
  induction l with
  | nil => exact .nil
  | cons x xs ih =>
    rw [List.nodup_cons] at hnd
    simp only [List.erase_cons]
    cases h : (x == a)
    · simp only [Bool.false_eq_true, ↓reduceIte, List.nodup_cons]
      exact ⟨fun hm => hnd.1 (List.mem_of_mem_erase hm), ih hnd.2⟩
    · simp only [↓reduceIte]; exact hnd.2

private theorem list_not_mem_erase_of_nodup {α : Type} [DecidableEq α] {l : List α}
    (hnd : l.Nodup) (a : α) : a ∉ l.erase a := by
  induction l with
  | nil => intro hm; cases hm
  | cons x xs ih =>
    rw [List.nodup_cons] at hnd
    simp only [List.erase_cons]
    cases h : (x == a)
    · simp only [Bool.false_eq_true, ↓reduceIte, List.mem_cons, not_or]
      have hne : a ≠ x := fun heq => by rw [heq, beq_self_eq_true] at h; cases h
      exact ⟨hne, ih hnd.2⟩
    · simp only [↓reduceIte]
      have hxeq : x = a := eq_of_beq h
      subst hxeq; exact hnd.1

private theorem list_mem_erase_of_ne {α : Type} [DecidableEq α] {l : List α} {a b : α}
    (hm : b ∈ l) (hne : b ≠ a) : b ∈ l.erase a := by
  induction l with
  | nil => cases hm
  | cons x xs ih =>
    simp only [List.erase_cons]
    rcases List.mem_cons.mp hm with rfl | hm'
    · have hbeq : (b == a) = false := beq_false_of_ne hne
      simp only [hbeq, Bool.false_eq_true, ↓reduceIte]
      exact List.mem_cons_self
    · cases hx : (x == a)
      · simp only [Bool.false_eq_true, ↓reduceIte]
        exact List.mem_cons_of_mem _ (ih hm')
      · simp only [↓reduceIte]; exact hm'

private theorem list_empty_eq_nil (xs : List Empty) : xs = [] := by
  cases xs with
  | nil => rfl
  | cons e _ => nomatch e

private theorem mem_of_getElem?_some {α : Type} {xs : List α} {i : Nat} {a : α}
    (h : xs[i]? = some a) : i < xs.length ∧ a ∈ xs := by
  have hlt : i < xs.length := by
    cases hc : compare i xs.length with
    | lt => exact Nat.compare_eq_lt.mp hc
    | eq =>
      have heq : i = xs.length := Nat.compare_eq_eq.mp hc
      rw [List.getElem?_eq_none (Nat.le_of_eq heq.symm)] at h; cases h
    | gt =>
      have hgt : xs.length < i := Nat.compare_eq_gt.mp hc
      rw [List.getElem?_eq_none (Nat.le_of_lt hgt)] at h; cases h
  exact ⟨hlt, List.mem_of_getElem? h⟩

private theorem lookup_ctxMark (Γ : Ctx) (x : Var) (mv : Bool) (y : Var) :
    AList.lookup (ctxMark Γ x mv) y =
      if y = x then (AList.lookup Γ y).map (Entry.read mv) else AList.lookup Γ y :=
  alist_lookup_update Γ x (Entry.read mv) y

/-! ### Straight-line program origins, operations, declarations, and mutability -/

private theorem mapSiblingKeys_length {α β : Type} (kind : ConstructKind) (name : α → ConstructName)
    (emit : α → FunctionRelativeKey → β) (preceding : List KeyConstruct) (items : List α) :
    (KeyConstruct.mapSiblingKeys kind name emit preceding items).length = items.length := by
  induction items generalizing preceding with
  | nil => rfl
  | cons _ rest ih => simp [KeyConstruct.mapSiblingKeys, ih]

private def mkEarlyOriginPair {α : Type} (siteOf : α → SiteId) (spanOf : α → Surface0.Span)
    (entry : FnId) (a : α) (key : FunctionRelativeKey) : SiteId × Origin :=
  (siteOf a, ⟨siteOf a, ⟨(spanOf a).fileId, (spanOf a).start, (spanOf a).stop,
    (spanOf a).line, (spanOf a).column⟩, ⟨.function entry, key⟩⟩)

private theorem mapSiblingKeys_getElem?_fst {α : Type} (kind : ConstructKind) (name : α → ConstructName)
    (siteOf : α → SiteId) (spanOf : α → Surface0.Span) (entry : FnId)
    (preceding : List KeyConstruct) (items : List α) (i : Nat) :
    ((KeyConstruct.mapSiblingKeys kind name (mkEarlyOriginPair siteOf spanOf entry) preceding items)[i]?).map
        (fun p : SiteId × Origin => (p.1, p.2.operation)) =
      (items[i]?).map (fun a => (siteOf a, siteOf a)) := by
  induction items generalizing preceding i with
  | nil => rfl
  | cons a rest ih =>
    cases i with
    | zero => rfl
    | succ j => exact ih _ j

private theorem bindingOrigins_length (p : StraightProgram) :
    p.bindingOrigins.length = p.bindings.length := by
  simp [StraightProgram.bindingOrigins, mapSiblingKeys_length]

private theorem bindingOrigins_getElem?_site (p : StraightProgram) (i : Nat) (hi : i < p.bindings.length) :
    (p.bindingOrigins[i]?).map (fun q => (q.1, q.2.operation)) = some (⟨i⟩, ⟨i⟩) := by
  have hfst := mapSiblingKeys_getElem?_fst .binding (fun row : LocatedStraightBinding × Nat => row.1.name)
    (fun pair => ⟨pair.2⟩) (fun pair => pair.1.operationSpan) earlyEntry [] p.bindings.zipIdx i
  change (p.bindingOrigins[i]?).map (fun q => (q.1, q.2.operation)) =
    ((p.bindings.zipIdx)[i]?).map (fun a => (⟨a.2⟩, ⟨a.2⟩)) at hfst
  rw [hfst, List.getElem?_zipIdx]
  simp [hi]

private theorem view_program_eq (v : AdmissionView) :
    v.program = ⟨List.map Prod.snd v.located.decls,
      v.located.bindings.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement),
      v.located.ending.toCore v.located.endingOrigin.1⟩ := by
  cases v with
  | zero p =>
    simp [AdmissionView.program, AdmissionView.located, Slice0.toCore, EarlyProgram.eraseLocations,
      StraightProgram.decls, StraightEnd.toCore, StraightBinding.toCore, List.map_map, Function.comp_def]
  | one p =>
    cases h : p.ending <;>
      simp [AdmissionView.program, AdmissionView.located, h, StraightEnd.toCore, StraightProgram.endingOrigin] <;>
      rfl

private theorem foldr_bound_ge {rows : List LocatedStraightBinding} {base : Nat}
    {row : LocatedStraightBinding} (hmem : row ∈ rows) :
    row.statement.bound ≤ rows.foldr (fun b n => max b.statement.bound n) base := by
  induction rows with
  | nil => cases hmem
  | cons b bs ih =>
    rcases List.mem_cons.mp hmem with rfl | htail
    · exact Nat.le_max_left _ _
    · exact Nat.le_trans (ih htail) (Nat.le_max_right _ _)

private theorem freshBase_bounds_binding (p : StraightProgram) {row : LocatedStraightBinding}
    (hmem : row ∈ p.bindings) : row.statement.bound ≤ p.freshBase :=
  foldr_bound_ge hmem

private theorem decls_wf_of_declsOk (p : StraightProgram)
    (hok : DeclsOk (List.map Prod.snd p.decls)) :
    DeclarationsWF p.decls := by
  refine ⟨?_, hok, ?_⟩
  · have hnames : (List.map StructDecl.name (List.map Prod.snd p.decls)).Nodup := hok.1
    have hmap : List.map Prod.fst p.decls =
        (List.map StructDecl.name (List.map Prod.snd p.decls)).map nominalId := by
      simp [StraightProgram.decls, List.map_map, Function.comp_def]
    rw [hmap]
    exact list_nodup_map_inj (fun _ _ h => congrArg DeclId.name h) hnames
  · intro id decl hmem
    simp only [StraightProgram.decls, List.mem_map] at hmem
    obtain ⟨⟨name, _⟩, _, heq⟩ := hmem
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj heq
    rfl

private theorem filter_unique_of_nodup
    {ds : List (Name × Surface0.Span)} {name : Name} {sp : Surface0.Span}
    (hnd : (ds.map Prod.fst).Nodup) (hmem : (name, sp) ∈ ds) :
    (ds.map fun (n, _) => (nominalId n, (⟨n, false, []⟩ : StructDecl))).filter
      (fun entry => entry.1 == nominalId name) = [(nominalId name, ⟨name, false, []⟩)] := by
  induction ds with
  | nil => cases hmem
  | cons pair rest ih =>
    rcases pair with ⟨m, msp⟩
    rw [List.map_cons, List.nodup_cons] at hnd
    simp only [List.map_cons, List.filter_cons]
    rcases List.mem_cons.mp hmem with heq | htail
    · obtain ⟨rfl, rfl⟩ := Prod.mk.inj heq
      simp only [beq_self_eq_true, ↓reduceIte, List.cons.injEq, true_and]
      have hfiltNil : ∀ {α : Type} (p : α → Bool) (xs : List α),
          (∀ x ∈ xs, p x = false) → xs.filter p = [] := by
        intro α p xs h
        induction xs with
        | nil => rfl
        | cons a as ih =>
          simp only [List.filter_cons, h a List.mem_cons_self, Bool.false_eq_true, ↓reduceIte]
          exact ih (fun x hx => h x (List.mem_cons_of_mem _ hx))
      apply hfiltNil
      intro x hx
      obtain ⟨⟨n', sp'⟩, hnMem, rfl⟩ := List.mem_map.mp hx
      have hne : n' ≠ name := fun heq' => hnd.1 (heq' ▸ List.mem_map.mpr ⟨(n', sp'), hnMem, rfl⟩)
      cases h : (nominalId n' == nominalId name)
      · rfl
      · exact False.elim (hne (congrArg DeclId.name (eq_of_beq h)))
    · have hne : m ≠ name := fun heq' => hnd.1 (heq' ▸ List.mem_map.mpr ⟨(name, sp), htail, rfl⟩)
      have hbf : (nominalId m == nominalId name) = false := by
        cases h : (nominalId m == nominalId name)
        · rfl
        · exact False.elim (hne (congrArg DeclId.name (eq_of_beq h)))
      simp only [hbf, Bool.false_eq_true, ↓reduceIte]
      exact ih hnd.2 htail

private theorem findDecl_emptyLinear (p : StraightProgram)
    (hok : DeclsOk (List.map Prod.snd p.decls))
    (name : ConcreteSpec.Name) (d : StructDecl)
    (hfind : findDecl (List.map Prod.snd p.decls) name = some d) :
    d = ⟨name, false, []⟩ ∧ p.decls.emptyLinear (nominalId name) = true := by
  have hmem : d ∈ List.map Prod.snd p.decls := List.mem_of_find?_eq_some hfind
  have hdname : d.name = name := eq_of_beq (List.find?_some (p := fun d : StructDecl => d.name == name) hfind)
  simp only [StraightProgram.decls, List.map_map, List.mem_map, Function.comp_def] at hmem
  obtain ⟨⟨n', sp⟩, hmemDecl, heq⟩ := hmem
  subst heq
  simp only at hdname
  subst hdname
  have hnd : (p.declarations.map Prod.fst).Nodup := by
    have h1 : (List.map StructDecl.name (List.map Prod.snd p.decls)).Nodup := hok.1
    simpa [StraightProgram.decls, List.map_map, Function.comp_def] using h1
  refine ⟨rfl, ?_⟩
  unfold Declarations.emptyLinear StraightProgram.decls
  rw [filter_unique_of_nodup hnd hmemDecl]
  simp

private theorem findDecl_not_copy (p : StraightProgram) (name : ConcreteSpec.Name) (d : StructDecl)
    (hfind : findDecl (List.map Prod.snd p.decls) name = some d) :
    (Ty.struct name).isCopy (List.map Prod.snd p.decls) = false := by
  have hmem : d ∈ List.map Prod.snd p.decls := List.mem_of_find?_eq_some hfind
  simp only [StraightProgram.decls, List.map_map, List.mem_map, Function.comp_def] at hmem
  obtain ⟨⟨_, _⟩, _, heq⟩ := hmem
  subst heq
  simp [Ty.isCopy, hfind]

private theorem emptyLinear_not_copy (p : StraightProgram) (name : ConcreteSpec.Name)
    (hemp : p.decls.emptyLinear (nominalId name) = true) :
    (Ty.struct name).isCopy (List.map Prod.snd p.decls) = false := by
  unfold Declarations.emptyLinear at hemp
  cases hf : p.decls.filter (·.1 == nominalId name) with
  | nil => rw [hf] at hemp; cases hemp
  | cons entry rest =>
    cases rest with
    | cons _ _ => rw [hf] at hemp; cases hemp
    | nil =>
      have hmemFilt : entry ∈ p.decls.filter (·.1 == nominalId name) := hf ▸ List.mem_cons_self
      obtain ⟨hmemP, hbeq⟩ := List.mem_filter.mp hmemFilt
      have hidEq : entry.1 = nominalId name := eq_of_beq hbeq
      simp only [StraightProgram.decls, List.mem_map] at hmemP
      obtain ⟨⟨n', sp⟩, hmemDecl, heqEntry⟩ := hmemP
      rw [← heqEntry] at hidEq
      have hnEq : n' = name := congrArg DeclId.name hidEq
      subst n'
      have hm2 : (⟨name, false, []⟩ : StructDecl) ∈ List.map Prod.snd p.decls :=
        List.mem_map.mpr ⟨(nominalId name, ⟨name, false, []⟩),
          List.mem_map.mpr ⟨(name, sp), hmemDecl, rfl⟩, rfl⟩
      cases hfind : findDecl (List.map Prod.snd p.decls) name with
      | some d' => exact findDecl_not_copy p name d' hfind
      | none =>
        have hall := List.find?_eq_none.mp hfind _ hm2
        simp at hall

private theorem owners_not_mutable (fresh : Nat) (rows : List LocatedStraightBinding) (i : Nat)
    (v : Var) (desc : Own.Descriptor) (hmem : (v, desc) ∈ StraightProgram.owners fresh rows i) :
    desc.mutable = false := by
  induction rows generalizing i with
  | nil => cases hmem
  | cons b rest ih =>
    rcases List.mem_cons.mp hmem with heq | htail
    · cases heq
      cases b.statement with
      | slice0 _ => rfl
      | bind _ ty _ => cases ty <;> rfl
      | destructure _ _ => rfl
    · exact ih (i + 1) htail

private def eraseExecOp {slice : Slice} {owners : AList Var Own.Descriptor} {decls : Declarations} :
    CoreOperation ((sliceLanguage slice).instrumented owners decls) →
    CoreOperation (sliceLanguage slice).operands
  | .expression e => .expression e
  | .call fn args => .call fn args
  | .primitive pr args => .primitive pr args
  | .abort => .abort

private theorem execution_operations_eq (v : AdmissionView) :
    v.execution.operations =
      (v.located.bindings.zipIdx.map fun (row, i) =>
        (⟨i⟩, CoreOperation.expression row.statement.expression)) ++
      [match v.located.ending with
       | .ret a => (v.located.endingOrigin.1, .expression (.read (earlyOperand a)))
       | .abort => (v.located.endingOrigin.1, .abort)] := by
  have hbody : ∀ (rows : List LocatedStraightBinding) (i : Nat)
      (endStmt : Exec.Stmt ((sliceLanguage v.slice).instrumented
        (StraightProgram.owners v.located.freshBase v.located.bindings 0) v.located.decls)),
      (stmtOperations (StraightProgram.machineBody v.located.freshBase endStmt rows i)).map
          (fun pair => (pair.1, eraseExecOp (slice := v.slice)
            (owners := StraightProgram.owners v.located.freshBase v.located.bindings 0)
            (decls := v.located.decls) pair.2)) =
        (rows.zipIdx i).map (fun (row, k) => (⟨k⟩, CoreOperation.expression row.statement.expression)) ++
        (stmtOperations endStmt).map
          (fun pair => (pair.1, eraseExecOp (slice := v.slice)
            (owners := StraightProgram.owners v.located.freshBase v.located.bindings 0)
            (decls := v.located.decls) pair.2)) := by
    intro rows
    induction rows with
    | nil => intro i endStmt; simp [StraightProgram.machineBody]
    | cons b rest ih =>
      intro i endStmt
      simp only [StraightProgram.machineBody, stmtOperations, List.map_cons, List.cons_append,
        List.nil_append, List.zipIdx_cons, ih (i + 1)]
      rfl
  unfold MachineProgram.operations AdmissionView.execution
  simp only [List.flatMap_cons, List.flatMap_nil, List.append_nil]
  refine Eq.trans (List.map_congr_left (g := fun pair => (pair.1, eraseExecOp (slice := v.slice)
      (owners := StraightProgram.owners v.located.freshBase v.located.bindings 0)
      (decls := v.located.decls) pair.2)) (fun ⟨_, op⟩ _ => by cases op <;> rfl)) ?_
  rw [hbody]
  cases v.located.ending <;> rfl

private theorem execution_operations_binding_mem (v : AdmissionView) {pos : Nat}
    {row : LocatedStraightBinding} (hget : v.located.bindings[pos]? = some row) :
    (⟨pos⟩, CoreOperation.expression row.statement.expression) ∈ v.execution.operations := by
  rw [execution_operations_eq]
  apply List.mem_append_left
  apply List.mem_map.mpr
  refine ⟨(row, pos), ?_, rfl⟩
  have hzip : (v.located.bindings.zipIdx)[pos]? = some (row, pos) := by
    rw [List.getElem?_zipIdx]; simp [hget]
  exact List.mem_of_getElem? hzip

private theorem execution_operations_nonAbort_at (v : AdmissionView)
    {site : SiteId} {op : CoreOperation (sliceLanguage v.slice).operands}
    (hnotAbort : op ≠ .abort)
    (hmem : (site, op) ∈ v.execution.operations) :
    (∃ pos row, site = ⟨pos⟩ ∧ v.located.bindings[pos]? = some row ∧
      op = .expression row.statement.expression) ∨
    (∃ a, v.located.ending = .ret a ∧ site = v.located.endingOrigin.1 ∧
      op = .expression (.read (earlyOperand a))) := by
  rw [execution_operations_eq] at hmem
  rcases List.mem_append.mp hmem with hbind | hend
  · left
    obtain ⟨⟨row, pos⟩, hzip, heq⟩ := List.mem_map.mp hbind
    obtain ⟨rfl, rfl⟩ := Prod.mk.inj heq
    have hspec := List.mem_zipIdx hzip
    have hlt : pos < v.located.bindings.length := by omega
    have hget : v.located.bindings[pos]? = some row := by
      rw [List.getElem?_eq_getElem hlt]
      exact congrArg some (by simpa using hspec.2.2.symm)
    exact ⟨pos, row, rfl, hget, rfl⟩
  · right
    cases hending : v.located.ending with
    | abort =>
      simp only [hending, List.mem_singleton, Prod.mk.injEq] at hend
      exact False.elim (hnotAbort hend.2)
    | ret a =>
      simp only [hending, List.mem_singleton, Prod.mk.injEq] at hend
      exact ⟨a, rfl, hend.1, hend.2⟩

private theorem earlyOperand_inj {a b : Slice0.Atom} (h : earlyOperand a = earlyOperand b) : a = b := by
  cases a with
  | lit n =>
    cases b with
    | lit m =>
      simp only [earlyOperand, EarlyOperand.literal.injEq, CVal.int.injEq] at h
      exact congrArg Slice0.Atom.lit (Int.ofNat.inj h.2)
    | var _ => simp [earlyOperand] at h
  | var x =>
    cases b with
    | lit _ => simp [earlyOperand] at h
    | var y => simp only [earlyOperand, EarlyOperand.copy.injEq] at h; rw [h]

private theorem add_safety_eq (slice : Slice) (l r : Slice0.Atom) :
    (sliceLanguage slice).safety (.expression (.add .u8 (earlyOperand l) (earlyOperand r))) =
      [⟨0, .overflow, (Args.cons (lowerAtom l) (Args.cons (lowerAtom r) .nil)).identities⟩] := by
  cases l <;> cases r <;> rfl

/-! ### Static context invariant and `lowerBindings` properties -/

/-- Every entry in `Γ` has a variable ID strictly below `p.freshBase` and is either `u8`
or an empty linear nominal struct declared in `p.decls`. -/
private def CtxInv (p : StraightProgram) (Γ : Ctx) : Prop :=
  ∀ x e, AList.lookup Γ x = some e →
    x.id < p.freshBase ∧
    (e.ty = .scalar (.int .u8) ∨
     ∃ name, e.ty = .struct name ∧ p.decls.emptyLinear (nominalId name) = true)

private theorem ctxInv_ctxMark {p : StraightProgram} {Γ : Ctx} (h : CtxInv p Γ)
    (x : Var) (mv : Bool) : CtxInv p (ctxMark Γ x mv) := by
  intro y e hlu
  rw [lookup_ctxMark] at hlu
  by_cases hy : y = x
  · subst hy
    simp only [↓reduceIte] at hlu
    cases h0 : AList.lookup Γ y with
    | none => rw [h0] at hlu; cases hlu
    | some e0 =>
      rw [h0, Option.map_some] at hlu
      cases Option.some.inj hlu
      exact h y e0 h0
  · simp only [hy, ↓reduceIte] at hlu
    exact h y e hlu

private theorem useAtom_preserves_ctxInv {p : StraightProgram} {Γ Γ' : Ctx} {a : ConcreteSpec.Atom}
    (h : CtxInv p Γ) (hu : UseAtom (List.map Prod.snd p.decls) Γ a Γ') :
    CtxInv p Γ' ∧ ∀ x, AList.lookup Γ' x = none ↔ AList.lookup Γ x = none := by
  cases hu with
  | int | bool => exact ⟨h, fun _ => Iff.rfl⟩
  | copy _ _ _ =>
    rename_i y _ _ _ _
    refine ⟨ctxInv_ctxMark h y false, fun z => ?_⟩
    rw [lookup_ctxMark]
    by_cases hz : z = y
    · subst hz; cases AList.lookup Γ z <;> simp
    · simp [hz]
  | move _ _ _ =>
    rename_i y _ _ _ _
    refine ⟨ctxInv_ctxMark h y true, fun z => ?_⟩
    rw [lookup_ctxMark]
    by_cases hz : z = y
    · subst hz; cases AList.lookup Γ z <;> simp
    · simp [hz]

private theorem useAtoms_preserves_ctxInv {p : StraightProgram} {Γ Γ' : Ctx}
    {as : List ConcreteSpec.Atom}
    (h : CtxInv p Γ) (hu : UseAtoms (List.map Prod.snd p.decls) Γ as Γ') :
    CtxInv p Γ' ∧ ∀ x, AList.lookup Γ' x = none ↔ AList.lookup Γ x = none := by
  induction hu with
  | nil => exact ⟨h, fun _ => Iff.rfl⟩
  | cons hu1 _ ih =>
    obtain ⟨h1, hnone1⟩ := useAtom_preserves_ctxInv h hu1
    obtain ⟨h2, hnone2⟩ := ih h1
    exact ⟨h2, fun x => (hnone2 x).trans (hnone1 x)⟩

private theorem uses_preserves_ctxInv {p : StraightProgram} {Γ Γ' : Ctx} {e : Expr}
    (h : CtxInv p Γ) (hu : Uses (List.map Prod.snd p.decls) Γ e Γ') :
    CtxInv p Γ' ∧ ∀ x, AList.lookup Γ' x = none ↔ AList.lookup Γ x = none := by
  cases hu with
  | atom ha => exact useAtom_preserves_ctxInv h ha
  | add hl hr =>
    obtain ⟨h1, hnone1⟩ := useAtom_preserves_ctxInv h hl
    obtain ⟨h2, hnone2⟩ := useAtom_preserves_ctxInv h1 hr
    exact ⟨h2, fun x => (hnone2 x).trans (hnone1 x)⟩
  | mk hus => exact useAtoms_preserves_ctxInv h hus

private theorem straightBinding_valid_shape {p : StraightProgram} {Γ Γ' : Ctx} {b : StraightBinding}
    (hdeclsOk : DeclsOk (List.map Prod.snd p.decls))
    (hctx : CtxInv p Γ) (hbound : b.bound ≤ p.freshBase)
    (hstmt : StmtOkWith ArithmeticPolicy.runtime.permits (List.map Prod.snd p.decls) Γ b.toCore Γ') :
    CtxInv p Γ' ∧
    (∀ x, AList.lookup Γ' x = none → AList.lookup Γ x = none) ∧
    ((∃ x a, b = .slice0 ⟨x, .atom a⟩ ∧ x.id < p.freshBase ∧ AList.lookup Γ x = none) ∨
     (∃ x l r, b = .slice0 ⟨x, .add l r⟩ ∧ x.id < p.freshBase ∧ AList.lookup Γ x = none) ∨
     (∃ binder a, b = .bind binder .u8 (.scalar (.atom a)) ∧
       ∀ x, binder = .name x → x.id < p.freshBase ∧ AList.lookup Γ x = none) ∨
     (∃ binder l r, b = .bind binder .u8 (.scalar (.add l r)) ∧
       ∀ x, binder = .name x → x.id < p.freshBase ∧ AList.lookup Γ x = none) ∨
     (∃ x name y, b = .bind (.name x) (.nominal name) (.move y) ∧
       x.id < p.freshBase ∧ AList.lookup Γ x = none ∧ p.decls.emptyLinear (nominalId name) = true) ∨
     (∃ x name, b = .bind (.name x) (.nominal name) (.construct name) ∧
       x.id < p.freshBase ∧ AList.lookup Γ x = none ∧ p.decls.emptyLinear (nominalId name) = true) ∨
     (∃ name y, b = .destructure name (.move y) ∧
       p.decls.emptyLinear (nominalId name) = true) ∨
     (∃ name, b = .destructure name (.construct name) ∧
       p.decls.emptyLinear (nominalId name) = true)) := by
  have hstructNotScalar : ∀ {name : ConcreteSpec.Name} {e : Slice0.Expr} {Γ₁},
      HasType (List.map Prod.snd p.decls) Γ.tys e.toCore (.struct name) →
      Uses (List.map Prod.snd p.decls) Γ e.toCore Γ₁ → False := by
    intro name e Γ₁ ht hu
    cases e with
    | atom a =>
      cases a with
      | lit _ => cases ht
      | var x =>
        cases ht with
        | copy hlu =>
          cases hu with
          | atom hua =>
            cases hua with
            | copy hfound _ hcopy =>
              rw [tys_lookup, hfound] at hlu
              have htyEq : _ = Ty.struct name := Option.some.inj hlu
              rw [htyEq] at hcopy
              simp only [Ty.isCopy] at hcopy
              cases hf : findDecl (List.map Prod.snd p.decls) name with
              | none => rw [hf] at hcopy; cases hcopy
              | some d =>
                obtain ⟨rfl, _⟩ := findDecl_emptyLinear p hdeclsOk name d hf
                rw [hf] at hcopy; cases hcopy
    | add _ _ => cases ht
  have hnotMoveU8 : ∀ {y Γ₁},
      HasType (List.map Prod.snd p.decls) Γ.tys (.atom (.move y)) (.scalar (.int .u8)) →
      Uses (List.map Prod.snd p.decls) Γ (.atom (.move y)) Γ₁ → False := by
    intro y Γ₁ ht hu
    cases ht with
    | move hlu =>
      cases hu with
      | atom hua =>
        cases hua with
        | move hfound _ hlin =>
          rw [tys_lookup, hfound] at hlu
          have htyEq : _ = Ty.scalar (.int .u8) := Option.some.inj hlu
          rw [htyEq] at hlin
          cases hlin
  have hctxCons : ∀ {x : Var} (ty : Ty) (lv us : Bool) (kn : Option Scalar)
      (kf : List (Name × Option Scalar)) {Γ₁ : Ctx},
      CtxInv p Γ₁ → (∀ z, AList.lookup Γ₁ z = none ↔ AList.lookup Γ z = none) →
      x.id < p.freshBase →
      (ty = .scalar (.int .u8) ∨ ∃ name, ty = .struct name ∧ p.decls.emptyLinear (nominalId name) = true) →
      CtxInv p ((x, { ty := ty, live := lv, used := us, known := kn, kfields := kf }) :: Γ₁) ∧
      ∀ z, AList.lookup ((x, { ty := ty, live := lv, used := us, known := kn, kfields := kf }) :: Γ₁) z = none →
        AList.lookup Γ z = none := by
    intro x ty lv us kn kf Γ₁ h1 hnone1 hlt hty
    refine ⟨?_, ?_⟩
    · intro z ez hlu
      simp only [AList.lookup] at hlu
      by_cases hz : x = z
      · subst hz; simp only [↓reduceIte] at hlu; cases Option.some.inj hlu; exact ⟨hlt, hty⟩
      · simp only [hz, ↓reduceIte] at hlu; exact h1 z ez hlu
    · intro z hlu
      simp only [AList.lookup] at hlu
      by_cases hz : x = z
      · subst hz; simp at hlu
      · simp only [hz, ↓reduceIte] at hlu; exact (hnone1 z).mp hlu
  cases b with
  | slice0 s =>
    cases s with
    | mk x e =>
      have hxlt : x.id < p.freshBase := by
        simp only [StraightBinding.bound, StraightBinding.destination, Option.map_some, Option.getD_some] at hbound
        omega
      cases hstmt with
      | let_ _ _ hu hfresh =>
        obtain ⟨h1, hnone1⟩ := uses_preserves_ctxInv hctx hu
        have hfresh0 : AList.lookup Γ x = none := (hnone1 x).mp hfresh
        obtain ⟨hctx', hmono⟩ := hctxCons (.scalar (.int .u8)) true false
          (knownExpr Γ (Slice0.Stmt.mk x e).e.toCore) (knownFields Γ (Slice0.Stmt.mk x e).e.toCore)
          h1 hnone1 hxlt (.inl rfl)
        cases e with
        | atom a => exact ⟨hctx', hmono, .inl ⟨x, a, rfl, hxlt, hfresh0⟩⟩
        | add l r => exact ⟨hctx', hmono, .inr (.inl ⟨x, l, r, rfl, hxlt, hfresh0⟩)⟩
  | bind binder ty rhs =>
    cases ty with
    | u8 =>
      cases rhs with
      | scalar e =>
        cases binder with
        | name x =>
          have hxlt : x.id < p.freshBase := by
            simp only [StraightBinding.bound, StraightBinding.destination, Option.map_some, Option.getD_some] at hbound
            omega
          cases hstmt with
          | let_ _ _ hu hfresh =>
            obtain ⟨h1, hnone1⟩ := uses_preserves_ctxInv hctx hu
            have hfresh0 : AList.lookup Γ x = none := (hnone1 x).mp hfresh
            obtain ⟨hctx', hmono⟩ := hctxCons (.scalar (.int .u8)) true false
              (knownExpr Γ (StraightExpr.scalar e).toCore) (knownFields Γ (StraightExpr.scalar e).toCore)
              h1 hnone1 hxlt (.inl rfl)
            cases e with
            | atom a =>
              exact ⟨hctx', hmono, .inr (.inr (.inl ⟨.name x, a, rfl, fun y hy => by cases hy; exact ⟨hxlt, hfresh0⟩⟩))⟩
            | add l r =>
              exact ⟨hctx', hmono, .inr (.inr (.inr (.inl ⟨.name x, l, r, rfl, fun y hy => by cases hy; exact ⟨hxlt, hfresh0⟩⟩)))⟩
        | wild =>
          cases hstmt with
          | letWild _ _ _ hu =>
            obtain ⟨h1, hnone1⟩ := uses_preserves_ctxInv hctx hu
            cases e with
            | atom a =>
              exact ⟨h1, fun z hz => (hnone1 z).mp hz, .inr (.inr (.inl ⟨.wild, a, rfl, fun _ h => by cases h⟩))⟩
            | add l r =>
              exact ⟨h1, fun z hz => (hnone1 z).mp hz, .inr (.inr (.inr (.inl ⟨.wild, l, r, rfl, fun _ h => by cases h⟩)))⟩
      | move y =>
        cases binder with
        | name _ => cases hstmt with | let_ ht _ hu _ => exact False.elim (hnotMoveU8 ht hu)
        | wild => cases hstmt with | letWild ht _ _ hu => exact False.elim (hnotMoveU8 ht hu)
      | construct ctor =>
        cases binder with
        | name _ => cases hstmt with | let_ ht _ _ _ => cases ht
        | wild => cases hstmt with | letWild ht _ _ _ => cases ht
    | nominal name =>
      cases binder with
      | wild =>
        cases hstmt with
        | letWild _ _ hcopy _ =>
          unfold WildcardPermitted at hcopy
          simp only [StraightType.toCore, Ty.isCopy] at hcopy
          cases hf : findDecl (List.map Prod.snd p.decls) name with
          | none => rw [hf] at hcopy; cases hcopy
          | some d =>
            obtain ⟨rfl, _⟩ := findDecl_emptyLinear p hdeclsOk name d hf
            rw [hf] at hcopy; cases hcopy
      | name x =>
        have hxlt : x.id < p.freshBase := by
          simp only [StraightBinding.bound, StraightBinding.destination, Option.map_some, Option.getD_some] at hbound
          omega
        cases hstmt with
        | let_ ht _ hu hfresh =>
          obtain ⟨h1, hnone1⟩ := uses_preserves_ctxInv hctx hu
          have hfresh0 : AList.lookup Γ x = none := (hnone1 x).mp hfresh
          cases rhs with
          | scalar e => exact False.elim (hstructNotScalar ht hu)
          | move y =>
            cases ht with
            | move hlu =>
              cases hfound : AList.lookup Γ y with
              | none => rw [tys_lookup, hfound] at hlu; cases hlu
              | some ey =>
                rw [tys_lookup, hfound] at hlu
                have htyEq : ey.ty = .struct name := Option.some.inj hlu
                obtain ⟨_, htyInfo⟩ := hctx y ey hfound
                rcases htyInfo with hu8 | ⟨name', hnm, hemp⟩
                · rw [hu8] at htyEq; cases htyEq
                · rw [hnm] at htyEq; cases htyEq
                  obtain ⟨hctx', hmono⟩ := hctxCons (.struct name) true false _ _ h1 hnone1 hxlt (.inr ⟨name, rfl, hemp⟩)
                  exact ⟨hctx', hmono, .inr (.inr (.inr (.inr (.inl ⟨x, name, y, rfl, hxlt, hfresh0, hemp⟩))))⟩
          | construct ctor =>
            cases ht with
            | mk hfind _ =>
              obtain ⟨_, hemp⟩ := findDecl_emptyLinear p hdeclsOk name _ hfind
              obtain ⟨hctx', hmono⟩ := hctxCons (.struct name) true false _ _ h1 hnone1 hxlt (.inr ⟨name, rfl, hemp⟩)
              exact ⟨hctx', hmono, .inr (.inr (.inr (.inr (.inr (.inl ⟨x, name, rfl, hxlt, hfresh0, hemp⟩)))))⟩
  | destructure name rhs =>
    cases hstmt with
    | letStruct ht _ hfind _ hu _ =>
      obtain ⟨_, hemp⟩ := findDecl_emptyLinear p hdeclsOk name _ hfind
      obtain ⟨h1, hnone1⟩ := uses_preserves_ctxInv hctx hu
      cases rhs with
      | scalar e => exact False.elim (hstructNotScalar ht hu)
      | move y =>
        exact ⟨h1, fun z hz => (hnone1 z).mp hz,
          .inr (.inr (.inr (.inr (.inr (.inr (.inl ⟨name, y, rfl, hemp⟩))))))⟩
      | construct ctor =>
        cases ht with
        | mk _ _ =>
          exact ⟨h1, fun z hz => (hnone1 z).mp hz,
            .inr (.inr (.inr (.inr (.inr (.inr (.inr ⟨name, rfl, hemp⟩))))))⟩

/-- Per-instruction static correspondence to a straight-line binding row. -/
private def InstMatchesRow {slice : Slice} (_p : StraightProgram)
    (discharges : Discharges (sliceLanguage slice)) (pos : Nat)
    (rows : List LocatedStraightBinding) (origins : List (SiteId × Origin))
    (inst : LocatedOp (MIRInst (sliceLoweringLanguage slice))) : Prop :=
  ∃ k < rows.length, ∃ row orig,
    rows[k]? = some row ∧
    origins[k]? = some (⟨pos + k⟩, orig) ∧
    inst.origin = orig ∧
    orig.operation = ⟨pos + k⟩ ∧
    match inst.instruction with
    | @MIRInst.op _ _ opcode _ _ _ _ args checks addrs =>
      addrs = [] ∧
      ((∃ l r, row.statement.expression = .add .u8 (earlyOperand l) (earlyOperand r) ∧
          opcode = .arithmetic (.plain .add) ∧
          args.identities = (Args.cons (lowerAtom l) (Args.cons (lowerAtom r) .nil)).identities ∧
          checks = [addCheck discharges ⟨pos + k⟩]) ∨
       (∃ name, row.statement.expression = .constructEmpty (nominalId name) ∧
          opcode = .named ⟨"constructEmpty"⟩ ∧
          args.identities = [] ∧
          checks = []))
    | .call .. => False
    | _ => True

private theorem instMatchesRow_shift {slice : Slice} {p : StraightProgram}
    {discharges : Discharges (sliceLanguage slice)} {pos : Nat}
    {row0 : LocatedStraightBinding} {rows : List LocatedStraightBinding}
    {o0 : SiteId × Origin} {origins : List (SiteId × Origin)}
    {inst : LocatedOp (MIRInst (sliceLoweringLanguage slice))}
    (h : InstMatchesRow p discharges (pos + 1) rows origins inst) :
    InstMatchesRow p discharges pos (row0 :: rows) (o0 :: origins) inst := by
  obtain ⟨k, hk, row, orig, hrow, horig, heq, hop, hinst⟩ := h
  refine ⟨k + 1, Nat.succ_lt_succ hk, row, orig, hrow, ?_, heq, ?_, ?_⟩
  · simpa [Nat.add_assoc, Nat.add_comm 1 k] using horig
  · simpa [Nat.add_assoc, Nat.add_comm 1 k] using hop
  · simpa [Nat.add_assoc, Nat.add_comm 1 k] using hinst

/-- Complete static specification of `lowerBindings` on a well-typed straight-line body. -/
private structure LowerBindingsSpec {slice : Slice} (p : StraightProgram)
    (discharges : Discharges (sliceLanguage slice)) (Γ : Ctx) (pos : Nat)
    (rows : List LocatedStraightBinding) (origins : List (SiteId × Origin))
    (res : LowerRow slice) : Prop where
  localsNodup : (List.map Prod.fst res.locals).Nodup
  localsBound : ∀ r ty, (r, ty) ∈ res.locals →
    (r.id < p.freshBase ∧ AList.lookup Γ ⟨r.id⟩ = none) ∨
    (p.freshBase + pos ≤ r.id ∧ ty = byteTy)
  localsTypes : ∀ r ty, (r, ty) ∈ res.locals →
    ty = byteTy ∨ ∃ name, ty = nominalTy name ∧ p.decls.emptyLinear (nominalId name) = true
  originsMatch : PreservesOrigins (List.map Prod.snd origins) (res.insts.map LocatedOp.origin ++ res.boundary)
  instSiteGe : ∀ inst ∈ res.insts, pos ≤ inst.origin.operation.id
  instSitesNodup : (res.insts.map (fun inst => inst.origin.operation)).Nodup
  instMatches : ∀ inst ∈ res.insts, InstMatchesRow p discharges pos rows origins inst
  addComplete : ∀ k < rows.length, ∀ row l r,
    rows[k]? = some row →
    row.statement.expression = .add .u8 (earlyOperand l) (earlyOperand r) →
    ∃ inst ∈ res.insts, ∃ dst : Place EarlyTy byteTy,
      inst.origin.operation = ⟨pos + k⟩ ∧
      inst.instruction = .op (opcode := .arithmetic (.plain .add)) (ty := byteTy) dst ()
        (.cons (lowerAtom l) (.cons (lowerAtom r) .nil)) [addCheck discharges ⟨pos + k⟩] []

private theorem lowerBindings_spec {slice : Slice} (p : StraightProgram)
    (discharges : Discharges (sliceLanguage slice))
    {rows : List LocatedStraightBinding} {origins : List (SiteId × Origin)}
    {pos : Nat} {Γ : Ctx} {ending : ConcreteSpec.Terminator}
    (hdeclsOk : DeclsOk (List.map Prod.snd p.decls))
    (hbounds : ∀ row ∈ rows, row.statement.bound ≤ p.freshBase)
    (hctx : CtxInv p Γ)
    (hbody : BodyOkWith ArithmeticPolicy.runtime.permits (List.map Prod.snd p.decls) Γ
      (rows.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) ending)
    (hlen : origins.length = rows.length)
    (hsites : ∀ k < rows.length, (origins[k]?).map (fun q => (q.1, q.2.operation)) = some (⟨pos + k⟩, ⟨pos + k⟩)) :
    ∃ res, lowerBindings discharges p.freshBase rows origins pos = .ok res ∧
      LowerBindingsSpec p discharges Γ pos rows origins res := by
  induction rows generalizing origins pos Γ with
  | nil =>
    cases origins with
    | nil =>
      refine ⟨⟨[], [], []⟩, rfl, {
        localsNodup := .nil
        localsBound := fun _ _ h => nomatch h
        localsTypes := fun _ _ h => nomatch h
        originsMatch := ⟨(fun _ h => nomatch h), (fun _ h => nomatch h)⟩
        instSiteGe := fun _ h => nomatch h
        instSitesNodup := .nil
        instMatches := fun _ h => nomatch h
        addComplete := fun _ hk => nomatch hk }⟩
    | cons _ _ => cases hlen
  | cons row rest ih =>
    cases origins with
    | nil => cases hlen
    | cons o0 restOrigins =>
      rcases o0 with ⟨site0, orig0⟩
      have hlen' : restOrigins.length = rest.length := Nat.succ.inj hlen
      have hsite0 := hsites 0 (Nat.zero_lt_succ _)
      simp only [List.getElem?_cons_zero, Option.map_some, Option.some.injEq, Prod.mk.injEq,
        Nat.add_zero] at hsite0
      obtain ⟨rfl, hop0⟩ := hsite0
      have hsites' : ∀ k < rest.length,
          (restOrigins[k]?).map (fun q => (q.1, q.2.operation)) = some (⟨pos + 1 + k⟩, ⟨pos + 1 + k⟩) := by
        intro k hk
        have h1 := hsites (k + 1) (Nat.succ_lt_succ hk)
        rw [List.getElem?_cons_succ] at h1
        rw [Nat.add_assoc, Nat.add_comm 1 k]
        exact h1
      cases hbody with
      | @cons _ Γ' _ _ _ hstmt htailBody =>
        have hrowBound := hbounds row List.mem_cons_self
        have hrestBounds : ∀ r ∈ rest, r.statement.bound ≤ p.freshBase :=
          fun r hr => hbounds r (List.mem_cons_of_mem _ hr)
        obtain ⟨hctx', hmono, hshape⟩ := straightBinding_valid_shape hdeclsOk hctx hrowBound hstmt
        obtain ⟨tail, htailOk, htailSpec⟩ := ih hrestBounds hctx' htailBody hlen' hsites'
        have finishOneLocal : ∀ (dst : PlaceId) (ty : EarlyTy)
            (inst : MIRInst (sliceLoweringLanguage slice))
            (hhead : lowerBinding discharges p.freshBase pos orig0 row.statement =
              .ok ⟨[(dst, ty)], [⟨orig0, inst⟩], []⟩)
            (hdstInfo : (dst.id < p.freshBase ∧ AList.lookup Γ ⟨dst.id⟩ = none ∧
                AList.lookup Γ' ⟨dst.id⟩ ≠ none) ∨
              (dst = ⟨p.freshBase + pos⟩ ∧ ty = byteTy))
            (htyInfo : ty = byteTy ∨ ∃ name, ty = nominalTy name ∧ p.decls.emptyLinear (nominalId name) = true)
            (hmatch0 : InstMatchesRow p discharges pos (row :: rest) ((⟨pos⟩, orig0) :: restOrigins) ⟨orig0, inst⟩)
            (hadd0 : ∀ l r, row.statement.expression = .add .u8 (earlyOperand l) (earlyOperand r) →
              ∃ d : Place EarlyTy byteTy, inst = .op (opcode := .arithmetic (.plain .add)) (ty := byteTy) d ()
                (.cons (lowerAtom l) (.cons (lowerAtom r) .nil)) [addCheck discharges ⟨pos⟩] []),
            ∃ res, lowerBindings discharges p.freshBase (row :: rest) ((⟨pos⟩, orig0) :: restOrigins) pos = .ok res ∧
              LowerBindingsSpec p discharges Γ pos (row :: rest) ((⟨pos⟩, orig0) :: restOrigins) res := by
          intro dst ty inst hhead hdstInfo htyInfo hmatch0 hadd0
          refine ⟨⟨(dst, ty) :: tail.locals, ⟨orig0, inst⟩ :: tail.insts, tail.boundary⟩, ?_, ?_⟩
          · simp [lowerBindings, hhead, htailOk, Bind.bind, Except.bind, Pure.pure, Except.pure]
          · refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
            · rw [List.map_cons, List.nodup_cons]
              refine ⟨?_, htailSpec.localsNodup⟩
              intro hm
              obtain ⟨⟨r', ty'⟩, hr'Mem, hr'Eq⟩ := List.mem_map.mp hm
              dsimp only at hr'Eq; subst r'
              rcases htailSpec.localsBound dst ty' hr'Mem with ⟨hlt', hnone'⟩ | ⟨hge', _⟩
              · rcases hdstInfo with ⟨_, _, hne⟩ | ⟨rfl, _⟩
                · exact hne hnone'
                · dsimp only at hlt'; omega
              · rcases hdstInfo with ⟨hlt, _, _⟩ | ⟨rfl, _⟩
                · omega
                · dsimp only at hge'; omega
            · intro r tr hm
              rcases List.mem_cons.mp hm with heq | htm
              · cases heq
                rcases hdstInfo with ⟨hlt, hnone, _⟩ | ⟨rfl, rfl⟩
                · exact .inl ⟨hlt, hnone⟩
                · exact .inr ⟨Nat.le_refl _, rfl⟩
              · rcases htailSpec.localsBound r tr htm with ⟨hlt, hnone⟩ | ⟨hge, hty⟩
                · exact .inl ⟨hlt, hmono _ hnone⟩
                · exact .inr ⟨by omega, hty⟩
            · intro r tr hm
              rcases List.mem_cons.mp hm with heq | htm
              · cases heq; exact htyInfo
              · exact htailSpec.localsTypes r tr htm
            · constructor
              · intro o ho
                simp only [List.map_cons, List.cons_append, List.mem_cons] at ho ⊢
                rcases ho with rfl | ho'
                · exact Or.inl rfl
                · exact Or.inr (htailSpec.originsMatch.1 o ho')
              · intro o ho
                simp only [List.map_cons, List.cons_append, List.mem_cons] at ho ⊢
                rcases ho with rfl | ho'
                · exact Or.inl rfl
                · exact Or.inr (htailSpec.originsMatch.2 o ho')
            · intro i hi
              rcases List.mem_cons.mp hi with rfl | hti
              · rw [hop0]; exact Nat.le_refl _
              · exact Nat.le_trans (Nat.le_succ _) (htailSpec.instSiteGe i hti)
            · rw [List.map_cons, List.nodup_cons]
              refine ⟨?_, htailSpec.instSitesNodup⟩
              intro hm
              obtain ⟨i', hi'Mem, hi'Eq⟩ := List.mem_map.mp hm
              have hge := htailSpec.instSiteGe i' hi'Mem
              rw [hi'Eq, hop0] at hge
              dsimp only at hge; omega
            · intro i hi
              rcases List.mem_cons.mp hi with rfl | hti
              · exact hmatch0
              · exact instMatchesRow_shift (htailSpec.instMatches i hti)
            · intro k hk r0 l r hget hexpr
              cases k with
              | zero =>
                cases Option.some.inj hget
                obtain ⟨d, hd⟩ := hadd0 l r hexpr
                exact ⟨⟨orig0, inst⟩, List.mem_cons_self, d, by rw [Nat.add_zero]; exact hop0, by rw [Nat.add_zero]; exact hd⟩
              | succ j =>
                obtain ⟨i', hi'Mem, d, hi'Op, hi'Inst⟩ :=
                  htailSpec.addComplete j (Nat.lt_of_succ_lt_succ hk) r0 l r hget hexpr
                refine ⟨i', List.mem_cons_of_mem _ hi'Mem, d, ?_, ?_⟩
                · rw [Nat.add_comm j 1, ← Nat.add_assoc]; exact hi'Op
                · rw [Nat.add_comm j 1, ← Nat.add_assoc]; exact hi'Inst
        rcases hshape with
          ⟨x, a, hst, hxlt, hfresh⟩ |
          ⟨x, l, r, hst, hxlt, hfresh⟩ |
          ⟨binder, a, hst, hbi⟩ |
          ⟨binder, l, r, hst, hbi⟩ |
          ⟨x, name, y, hst, hxlt, hfresh, hemp⟩ |
          ⟨x, name, hst, hxlt, hfresh, hemp⟩ |
          ⟨name, y, hst, hemp⟩ |
          ⟨name, hst, hemp⟩
        · -- 1. slice0 atom
          simp only [Function.comp_def, hst] at hstmt
          cases hstmt with
          | let_ _ _ _ _ =>
            exact finishOneLocal ⟨x.id⟩ byteTy (.bind (ty := byteTy) ⟨⟨x.id⟩, []⟩ (lowerAtom a))
              (by simp [lowerBinding, hst])
              (.inl ⟨hxlt, hfresh, by cases x; simp [AList.lookup]⟩) (.inl rfl)
              ⟨0, Nat.zero_lt_succ _, row, orig0, rfl, by simp only [List.getElem?_cons_zero, Nat.add_zero], rfl, by rw [Nat.add_zero]; exact hop0, trivial⟩
              (fun _ _ hex => by simp [StraightBinding.expression, StraightBinding.rhs, hst,
                StraightExpr.integrated, earlyExpression] at hex)
        · -- 2. slice0 add
          simp only [Function.comp_def, hst] at hstmt
          cases hstmt with
          | let_ _ _ _ _ =>
            exact finishOneLocal ⟨x.id⟩ byteTy
              (.op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨⟨x.id⟩, []⟩ ()
                (.cons (lowerAtom l) (.cons (lowerAtom r) .nil)) [addCheck discharges ⟨pos⟩] [])
              (by simp [lowerBinding, hst])
              (.inl ⟨hxlt, hfresh, by cases x; simp [AList.lookup]⟩) (.inl rfl)
              ⟨0, Nat.zero_lt_succ _, row, orig0, rfl, by simp only [List.getElem?_cons_zero, Nat.add_zero], rfl, by rw [Nat.add_zero]; exact hop0,
                rfl, .inl ⟨l, r, by simp [StraightBinding.expression, StraightBinding.rhs, hst,
                  StraightExpr.integrated, earlyExpression], rfl, rfl, by rw [Nat.add_zero]⟩⟩
              (fun l' r' hex => by
                simp only [StraightBinding.expression, StraightBinding.rhs, hst,
                  StraightExpr.integrated, earlyExpression, EarlyExpr.add.injEq] at hex
                obtain ⟨_, hl, hr⟩ := hex
                have hle := earlyOperand_inj hl
                have hre := earlyOperand_inj hr
                subst hle; subst hre; exact ⟨⟨⟨x.id⟩, []⟩, rfl⟩)
        · -- 3. bind u8 atom
          cases binder with
          | name x =>
            obtain ⟨hxlt, hfresh⟩ := hbi x rfl
            simp only [Function.comp_def, hst] at hstmt
            cases hstmt with
            | let_ _ _ _ _ =>
              exact finishOneLocal ⟨x.id⟩ byteTy (.bind (ty := byteTy) ⟨⟨x.id⟩, []⟩ (lowerAtom a))
                (by simp [lowerBinding, hst])
                (.inl ⟨hxlt, hfresh, by cases x; simp [AList.lookup]⟩) (.inl rfl)
                ⟨0, Nat.zero_lt_succ _, row, orig0, rfl, by simp only [List.getElem?_cons_zero, Nat.add_zero], rfl, by rw [Nat.add_zero]; exact hop0, trivial⟩
                (fun _ _ hex => by simp [StraightBinding.expression, StraightBinding.rhs, hst,
                  StraightExpr.integrated, earlyExpression] at hex)
          | wild =>
            exact finishOneLocal ⟨p.freshBase + pos⟩ byteTy (.bind (ty := byteTy) ⟨⟨p.freshBase + pos⟩, []⟩ (lowerAtom a))
              (by simp [lowerBinding, hst])
              (.inr ⟨rfl, rfl⟩) (.inl rfl)
              ⟨0, Nat.zero_lt_succ _, row, orig0, rfl, by simp only [List.getElem?_cons_zero, Nat.add_zero], rfl, by rw [Nat.add_zero]; exact hop0, trivial⟩
              (fun _ _ hex => by simp [StraightBinding.expression, StraightBinding.rhs, hst,
                StraightExpr.integrated, earlyExpression] at hex)
        · -- 4. bind u8 add
          cases binder with
          | name x =>
            obtain ⟨hxlt, hfresh⟩ := hbi x rfl
            simp only [Function.comp_def, hst] at hstmt
            cases hstmt with
            | let_ _ _ _ _ =>
              exact finishOneLocal ⟨x.id⟩ byteTy
                (.op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨⟨x.id⟩, []⟩ ()
                  (.cons (lowerAtom l) (.cons (lowerAtom r) .nil)) [addCheck discharges ⟨pos⟩] [])
                (by simp [lowerBinding, hst])
                (.inl ⟨hxlt, hfresh, by cases x; simp [AList.lookup]⟩) (.inl rfl)
                ⟨0, Nat.zero_lt_succ _, row, orig0, rfl, by simp only [List.getElem?_cons_zero, Nat.add_zero], rfl, by rw [Nat.add_zero]; exact hop0,
                  rfl, .inl ⟨l, r, by simp [StraightBinding.expression, StraightBinding.rhs, hst,
                    StraightExpr.integrated, earlyExpression], rfl, rfl, by rw [Nat.add_zero]⟩⟩
                (fun l' r' hex => by
                  simp only [StraightBinding.expression, StraightBinding.rhs, hst,
                    StraightExpr.integrated, earlyExpression, EarlyExpr.add.injEq] at hex
                  obtain ⟨_, hl, hr⟩ := hex
                  have hle := earlyOperand_inj hl
                  have hre := earlyOperand_inj hr
                  subst hle; subst hre; exact ⟨⟨⟨x.id⟩, []⟩, rfl⟩)
          | wild =>
            exact finishOneLocal ⟨p.freshBase + pos⟩ byteTy
              (.op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨⟨p.freshBase + pos⟩, []⟩ ()
                (.cons (lowerAtom l) (.cons (lowerAtom r) .nil)) [addCheck discharges ⟨pos⟩] [])
              (by simp [lowerBinding, hst])
              (.inr ⟨rfl, rfl⟩) (.inl rfl)
              ⟨0, Nat.zero_lt_succ _, row, orig0, rfl, by simp only [List.getElem?_cons_zero, Nat.add_zero], rfl, by rw [Nat.add_zero]; exact hop0,
                rfl, .inl ⟨l, r, by simp [StraightBinding.expression, StraightBinding.rhs, hst,
                  StraightExpr.integrated, earlyExpression], rfl, rfl, by rw [Nat.add_zero]⟩⟩
              (fun l' r' hex => by
                simp only [StraightBinding.expression, StraightBinding.rhs, hst,
                  StraightExpr.integrated, earlyExpression, EarlyExpr.add.injEq] at hex
                obtain ⟨_, hl, hr⟩ := hex
                have hle := earlyOperand_inj hl
                have hre := earlyOperand_inj hr
                subst hle; subst hre; exact ⟨⟨⟨p.freshBase + pos⟩, []⟩, rfl⟩)
        · -- 5. bind nominal move
          simp only [Function.comp_def, hst] at hstmt
          cases hstmt with
          | let_ _ _ _ _ =>
            exact finishOneLocal ⟨x.id⟩ (nominalTy name) (.bind (ty := nominalTy name) ⟨⟨x.id⟩, []⟩ (.move ⟨⟨y.id⟩, []⟩))
              (by simp [lowerBinding, hst])
              (.inl ⟨hxlt, hfresh, by cases x; simp [AList.lookup]⟩) (.inr ⟨name, rfl, hemp⟩)
              ⟨0, Nat.zero_lt_succ _, row, orig0, rfl, by simp only [List.getElem?_cons_zero, Nat.add_zero], rfl, by rw [Nat.add_zero]; exact hop0, trivial⟩
              (fun _ _ hex => by simp [StraightBinding.expression, StraightBinding.rhs, hst,
                StraightExpr.integrated] at hex)
        · -- 6. bind nominal construct
          simp only [Function.comp_def, hst] at hstmt
          cases hstmt with
          | let_ _ _ _ _ =>
            exact finishOneLocal ⟨x.id⟩ (nominalTy name)
              (.op (opcode := .named ⟨"constructEmpty"⟩) (ty := nominalTy name) ⟨⟨x.id⟩, []⟩ () .nil [] [])
              (by simp [lowerBinding, hst])
              (.inl ⟨hxlt, hfresh, by cases x; simp [AList.lookup]⟩) (.inr ⟨name, rfl, hemp⟩)
              ⟨0, Nat.zero_lt_succ _, row, orig0, rfl, by simp only [List.getElem?_cons_zero, Nat.add_zero], rfl, by rw [Nat.add_zero]; exact hop0,
                rfl, .inr ⟨name, by simp [StraightBinding.expression, StraightBinding.rhs, hst,
                  StraightExpr.integrated], rfl, rfl, rfl⟩⟩
              (fun _ _ hex => by simp [StraightBinding.expression, StraightBinding.rhs, hst,
                StraightExpr.integrated] at hex)
        · -- 7. destructure move (0 locals, 1 inst)
          let inst0 : MIRInst (sliceLoweringLanguage slice) :=
            .destructure (ty := nominalTy name) ⟨⟨y.id⟩, []⟩ []
          refine ⟨⟨tail.locals, ⟨orig0, inst0⟩ :: tail.insts, tail.boundary⟩, ?_, ?_⟩
          · simp [lowerBindings, lowerBinding, hst, htailOk, inst0,
              Bind.bind, Except.bind, Pure.pure, Except.pure]
          · refine ⟨htailSpec.localsNodup, ?_, htailSpec.localsTypes, ?_, ?_, ?_, ?_, ?_⟩
            · intro r tr hm
              rcases htailSpec.localsBound r tr hm with ⟨hlt, hnone⟩ | ⟨hge, hty⟩
              · exact .inl ⟨hlt, hmono _ hnone⟩
              · exact .inr ⟨by omega, hty⟩
            · constructor
              · intro o ho
                simp only [List.map_cons, List.cons_append, List.mem_cons] at ho ⊢
                rcases ho with rfl | ho'
                · exact Or.inl rfl
                · exact Or.inr (htailSpec.originsMatch.1 o ho')
              · intro o ho
                simp only [List.map_cons, List.cons_append, List.mem_cons] at ho ⊢
                rcases ho with rfl | ho'
                · exact Or.inl rfl
                · exact Or.inr (htailSpec.originsMatch.2 o ho')
            · intro i hi
              rcases List.mem_cons.mp hi with rfl | hti
              · rw [hop0]; exact Nat.le_refl _
              · exact Nat.le_trans (Nat.le_succ _) (htailSpec.instSiteGe i hti)
            · rw [List.map_cons, List.nodup_cons]
              refine ⟨?_, htailSpec.instSitesNodup⟩
              intro hm
              obtain ⟨i', hi'Mem, hi'Eq⟩ := List.mem_map.mp hm
              have hge := htailSpec.instSiteGe i' hi'Mem
              rw [hi'Eq, hop0] at hge
              dsimp only at hge; omega
            · intro i hi
              rcases List.mem_cons.mp hi with rfl | hti
              · exact ⟨0, Nat.zero_lt_succ _, row, orig0, rfl, by simp only [List.getElem?_cons_zero, Nat.add_zero], rfl, by rw [Nat.add_zero]; exact hop0, trivial⟩
              · exact instMatchesRow_shift (htailSpec.instMatches i hti)
            · intro k hk r0 l r hget hexpr
              cases k with
              | zero =>
                cases Option.some.inj hget
                simp [StraightBinding.expression, hst] at hexpr
              | succ j =>
                obtain ⟨i', hi'Mem, d, hi'Op, hi'Inst⟩ :=
                  htailSpec.addComplete j (Nat.lt_of_succ_lt_succ hk) r0 l r hget hexpr
                refine ⟨i', List.mem_cons_of_mem _ hi'Mem, d, ?_, ?_⟩
                · rw [Nat.add_comm j 1, ← Nat.add_assoc]; exact hi'Op
                · rw [Nat.add_comm j 1, ← Nat.add_assoc]; exact hi'Inst
        · -- 8. destructure construct (0 locals, 0 insts, 1 boundary origin)
          refine ⟨⟨tail.locals, tail.insts, orig0 :: tail.boundary⟩, ?_, ?_⟩
          · simp [lowerBindings, lowerBinding, hst, htailOk,
              Bind.bind, Except.bind, Pure.pure, Except.pure]
          · refine ⟨htailSpec.localsNodup, ?_, htailSpec.localsTypes, ?_, ?_, htailSpec.instSitesNodup, ?_, ?_⟩
            · intro r tr hm
              rcases htailSpec.localsBound r tr hm with ⟨hlt, hnone⟩ | ⟨hge, hty⟩
              · exact .inl ⟨hlt, hmono _ hnone⟩
              · exact .inr ⟨by omega, hty⟩
            · constructor
              · intro o ho
                rcases List.mem_append.mp ho with hi | hb
                · exact List.mem_cons_of_mem _ (htailSpec.originsMatch.1 o (List.mem_append_left _ hi))
                · rcases List.mem_cons.mp hb with rfl | hb'
                  · exact List.mem_cons_self
                  · exact List.mem_cons_of_mem _ (htailSpec.originsMatch.1 o (List.mem_append_right _ hb'))
              · intro o ho
                rcases List.mem_cons.mp ho with rfl | ho'
                · exact List.mem_append_right _ List.mem_cons_self
                · rcases List.mem_append.mp (htailSpec.originsMatch.2 o ho') with hi | hb
                  · exact List.mem_append_left _ hi
                  · exact List.mem_append_right _ (List.mem_cons_of_mem _ hb)
            · intro i hi
              exact Nat.le_trans (Nat.le_succ _) (htailSpec.instSiteGe i hi)
            · intro i hi
              exact instMatchesRow_shift (htailSpec.instMatches i hi)
            · intro k hk r0 l r hget hexpr
              cases k with
              | zero =>
                cases Option.some.inj hget
                simp [StraightBinding.expression, hst] at hexpr
              | succ j =>
                obtain ⟨i', hi'Mem, d, hi'Op, hi'Inst⟩ :=
                  htailSpec.addComplete j (Nat.lt_of_succ_lt_succ hk) r0 l r hget hexpr
                refine ⟨i', hi'Mem, d, ?_, ?_⟩
                · rw [Nat.add_comm j 1, ← Nat.add_assoc]; exact hi'Op
                · rw [Nat.add_comm j 1, ← Nat.add_assoc]; exact hi'Inst

/-! ### Static contracts: `MIR.HandoffWF`, `PreservesOrigins`, `PreservesOwnerMutability`, `ConsumesEvidence` -/

private theorem nodup_getElem_inj {α : Type} {xs : List α} (hnd : xs.Nodup)
    {i j : Nat} (hi : i < xs.length) (hj : j < xs.length)
    (heq : xs[i]'hi = xs[j]'hj) : i = j := by
  induction xs generalizing i j with
  | nil => cases hi
  | cons x rest ih =>
    rw [List.nodup_cons] at hnd
    cases i with
    | zero =>
      cases j with
      | zero => rfl
      | succ j' =>
        simp only [List.getElem_cons_zero, List.getElem_cons_succ] at heq
        exact False.elim (hnd.1 (heq ▸ List.getElem_mem (Nat.lt_of_succ_lt_succ hj)))
    | succ i' =>
      cases j with
      | zero =>
        simp only [List.getElem_cons_zero, List.getElem_cons_succ] at heq
        exact False.elim (hnd.1 (heq.symm ▸ List.getElem_mem (Nat.lt_of_succ_lt_succ hi)))
      | succ j' =>
        simp only [List.getElem_cons_succ] at heq
        exact congrArg Nat.succ (ih hnd.2 (Nat.lt_of_succ_lt_succ hi) (Nat.lt_of_succ_lt_succ hj) heq)

private theorem lowerProgram_origins {slice : Slice} {profile : Profile}
    (project : Project) (p : StraightProgram) (discharges : Discharges (sliceLanguage slice))
    {body : LowerRow slice}
    (hbind : lowerBindings discharges p.freshBase p.bindings p.bindingOrigins 0 = .ok body)
    (horig : PreservesOrigins (List.map Prod.snd p.bindingOrigins) (body.insts.map LocatedOp.origin ++ body.boundary)) :
    ∀ (mir : MIR (sliceLoweringLanguage slice) profile),
      lowerProgram project p discharges = .ok mir →
      PreservesOrigins (List.map Prod.snd (p.bindingOrigins ++ [p.endingOrigin])) mir.origins := by
  intro mir hok
  unfold lowerProgram at hok
  simp only [hbind, Bind.bind, Except.bind, Pure.pure, Except.pure, Except.ok.injEq] at hok
  have hmirOrig : mir.origins =
      (body.insts.map LocatedOp.origin ++ body.boundary) ++ [p.endingOrigin.2] := by
    rw [← hok]
    unfold MIR.origins CFG.origins lowerEnding
    cases p.ending <;>
      · dsimp only [List.flatMap, List.map, List.flatten, Terminator.origins]
        simp [List.append_assoc]
  rw [hmirOrig, List.map_append, List.map_singleton]
  constructor
  · intro o ho
    rcases List.mem_append.mp ho with h1 | h2
    · exact List.mem_append_left _ (horig.1 o h1)
    · exact List.mem_append_right _ h2
  · intro o ho
    rcases List.mem_append.mp ho with h1 | h2
    · exact List.mem_append_left _ (horig.2 o h1)
    · exact List.mem_append_right _ h2

private theorem mir_no_escapes {slice : Slice} {profile : Profile}
    {p : StraightProgram} {discharges : Discharges (sliceLanguage slice)}
    (mir : MIR (sliceLoweringLanguage slice) profile)
    {body : LowerRow slice}
    (hfns : mir.functions = [(earlyEntry, (body.locals, { entry := ⟨0⟩, blocks := #[{
      id := ⟨0⟩, params := [], body := body.insts,
      ending := (lowerEnding (slice := slice) p).1,
      boundaryOrigins := body.boundary ++ (lowerEnding (slice := slice) p).2 }] }))])
    (hmatches : ∀ inst ∈ body.insts, InstMatchesRow p discharges 0 p.bindings p.bindingOrigins inst)
    (fn : FnId) (place : PlaceId) :
    ¬ mir.Escapes (language := sliceLanguage slice) fn place := by
  intro hesc
  obtain ⟨op, hopMem, _, ty, path, haddr⟩ := hesc
  have hopMem' : op ∈ body.insts.zipIdx.filterMap (fun (inst, index) => match inst.instruction with
      | @MIRInst.op _ _ opcode _ _ _ _ args checks addresses =>
        some ⟨earlyEntry, ⟨0⟩, index, inst.origin, .operator opcode,
          args.identities ++ addresses.map (fun q => (q.1, .address q.2.root q.2.path)), checks⟩
      | .call callee _ args checks =>
        some ⟨earlyEntry, ⟨0⟩, index, inst.origin, .call callee,
          args.map (fun arg => (arg.1, arg.2.identity)), checks⟩
      | _ => none) := by
    have hflat : mir.operations = body.insts.zipIdx.filterMap (fun (inst, index) => match inst.instruction with
        | @MIRInst.op _ _ opcode _ _ _ _ args checks addresses =>
          some ⟨earlyEntry, ⟨0⟩, index, inst.origin, .operator opcode,
            args.identities ++ addresses.map (fun q => (q.1, .address q.2.root q.2.path)), checks⟩
        | .call callee _ args checks =>
          some ⟨earlyEntry, ⟨0⟩, index, inst.origin, .call callee,
            args.map (fun arg => (arg.1, arg.2.identity)), checks⟩
        | _ => none) := by
      unfold MIR.operations
      rw [hfns]
      dsimp only [List.flatMap, List.map, List.flatten]
      simp only [List.append_eq, List.append_nil]
      have hfm : ∀ (f g : LocatedOp (MIRInst (sliceLoweringLanguage slice)) × Nat →
          Option (MIR.Occurrence (sliceLanguage slice).operands.Ty)),
          (∀ x, f x = g x) → ∀ xs, List.filterMap f xs = List.filterMap g xs := by
        intro f g hfg xs; induction xs with
        | nil => rfl
        | cons x xs ih => simp only [List.filterMap_cons, hfg, ih]
      exact hfm _ _ (fun ⟨i, k⟩ => by cases i.instruction <;> rfl) body.insts.zipIdx
    exact hflat ▸ hopMem
  obtain ⟨⟨inst, idx⟩, hzipMem, hfilter⟩ := List.mem_filterMap.mp hopMem'
  have hinstMem : inst ∈ body.insts := by
    have hs := List.mem_zipIdx hzipMem
    have hlt : idx < body.insts.length := by omega
    exact List.mem_of_getElem? (by rw [List.getElem?_eq_getElem hlt]; exact congrArg some (by simpa using hs.2.2.symm))
  obtain ⟨_, _, _, _, _, _, _, _, hspec⟩ := hmatches inst hinstMem
  rcases inst with ⟨orig, ci⟩
  cases ci with
  | op dst opcode args checks addrs =>
    obtain ⟨rfl, _⟩ := hspec
    cases Option.some.inj hfilter
    simp only [List.map_nil, List.append_nil] at haddr
    have hnoAddr : ∀ {ts : List EarlyTy} (a : Args (Read EarlyTy (Literal EarlyTy)) ts),
        (ty, OperandRead.address place path) ∉ a.identities := by
      intro ts a
      induction a with
      | nil => intro h; cases h
      | cons rd rest ih =>
        intro hm'
        rcases List.mem_cons.mp hm' with heq | htail
        · cases rd <;> cases heq
        · exact ih htail
    exact hnoAddr args haddr
  | call _ _ _ _ => exact False.elim hspec
  | bind _ _ => cases hfilter
  | assign _ _ => cases hfilter
  | endLifetime _ => cases hfilter
  | destructure _ _ => cases hfilter

private theorem lowerWith_handoffWF {slice : Slice} {profile : Profile}
    {core : CoreInput (sliceLanguage slice)} {v : AdmissionView}
    {discharges : Discharges (sliceLanguage slice)}
    {body : LowerRow slice} {mir : MIR (sliceLoweringLanguage slice) profile}
    (hrep : v.Represents core) (hwf : core.WF)
    (hdeclsOk : DeclsOk (List.map Prod.snd v.located.decls))
    (hbind : lowerBindings discharges v.located.freshBase v.located.bindings v.located.bindingOrigins 0 = .ok body)
    (hspec : LowerBindingsSpec v.located discharges [] 0 v.located.bindings v.located.bindingOrigins body)
    (hmir : lowerProgram core.project v.located discharges = .ok mir) :
    MIR.HandoffWF (language := sliceLanguage slice) mir := by
  let p := v.located
  have hmirEq : mir = {
    declarations := p.decls, project := core.project, entry := earlyEntry,
    signatures := [(earlyEntry, ⟨[], some byteTy⟩)],
    functions := [(earlyEntry, (body.locals, { entry := ⟨0⟩, blocks := #[{
      id := ⟨0⟩, params := [], body := body.insts,
      ending := (lowerEnding (slice := slice) p).1,
      boundaryOrigins := body.boundary ++ (lowerEnding (slice := slice) p).2 }] }))],
    mutableLocals := [] } := by
    unfold lowerProgram at hmir
    simp only [hbind, Bind.bind, Except.bind, Pure.pure, Except.pure, Except.ok.injEq] at hmir
    exact hmir.symm
  have horigPres : PreservesOrigins (List.map Prod.snd (p.bindingOrigins ++ [p.endingOrigin])) mir.origins :=
    lowerProgram_origins core.project p discharges hbind hspec.originsMatch mir hmir
  have hcoreOrig : core.machineProgram.origins = p.bindingOrigins ++ [p.endingOrigin] := by
    obtain ⟨_, eq, hpay⟩ := hrep
    have heq : eq = rfl := rfl
    rw [heq] at hpay
    simp [CoreInput.machineProgram, hpay, Payload.execution, AdmissionView.execution, p]
  have hmirOriginsEq : (mir.logicalView (language := sliceLanguage slice)).origins = mir.origins := by
    subst hmirEq
    unfold Memory.origins MIR.origins MIR.logicalView CFG.origins lowerEnding
    cases hpe : p.ending with
    | ret a =>
      simp only [AList.map, List.map_cons, List.map_nil, List.flatMap_cons, List.flatMap_nil,
        List.append_nil, Array.toList_map, List.map_map, Function.comp_def, Terminator.origins]
      with_unfolding_all
        change List.map LocatedOp.origin body.insts ++ (body.boundary ++ [p.endingOrigin.2]) ++ [] =
          List.map LocatedOp.origin body.insts ++ (body.boundary ++ [p.endingOrigin.2])
      exact List.append_nil _
    | abort =>
      simp only [AList.map, List.map_cons, List.map_nil, List.flatMap_cons, List.flatMap_nil,
        List.append_nil, Array.toList_map, List.map_map, Function.comp_def, Terminator.origins]
      with_unfolding_all
        change List.map LocatedOp.origin body.insts ++ body.boundary ++ [p.endingOrigin.2] =
          List.map LocatedOp.origin body.insts ++ body.boundary ++ [p.endingOrigin.2]
      rfl
  constructor
  · -- logicalView.HandoffWF
    refine ⟨?_, ?_, ?_, ?_, ?_, ?_⟩
    · -- TablesWF
      subst hmirEq
      refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
      · simp [MIR.logicalView, AList.map]
      · simp [MIR.logicalView]
      · simp [MIR.logicalView, AList.map]
      · simp [MIR.logicalView]
      · intro fn classes hmem
        simp only [MIR.logicalView, AList.map, List.map_cons, List.map_nil, List.mem_singleton,
          Prod.mk.injEq] at hmem
        obtain ⟨rfl, rfl⟩ := hmem
        change (List.map Prod.fst (List.map (fun (e : PlaceId × EarlyTy) => (e.1, StorageClass.virtual)) body.locals)).Nodup
        simp only [List.map_map, Function.comp_def]
        exact hspec.localsNodup
      · intro _ _ hmem; simp [MIR.logicalView] at hmem
      · intro fn locals graph hmem
        simp only [MIR.logicalView, AList.map, List.map_cons, List.map_nil, List.mem_singleton,
          Prod.mk.injEq] at hmem
        obtain ⟨rfl, rfl, rfl⟩ := hmem
        refine ⟨hspec.localsNodup, ?_, ?_⟩
        · -- CFG.WF
          refine ⟨by simp, ?_, ?_⟩
          · simp only [Array.toList_map, List.map_cons, List.map_nil]
            exact ⟨_, List.mem_cons_self, rfl⟩
          · intro blk hblk edge hedge
            simp only [Array.toList_map, List.map_cons, List.map_nil, List.mem_singleton] at hblk
            subst hblk
            cases hpe : p.ending with
            | ret a =>
              simp only [lowerEnding, hpe] at hedge
              with_unfolding_all
                change edge ∈ [] at hedge
                cases hedge
            | abort =>
              simp only [lowerEnding, hpe] at hedge
              with_unfolding_all
                change edge ∈ [] at hedge
                cases hedge
        · -- SignatureMatches
          refine ⟨⟨[], some byteTy⟩, by simp [MIR.logicalView, AList.lookup],
            ⟨_, by simp only [Array.toList_map]; exact List.mem_cons_self, rfl, rfl⟩, ?_⟩
          intro blk hblk
          simp only [Array.toList_map, List.map_cons, List.map_nil, List.mem_singleton] at hblk
          subst hblk
          refine ⟨?_, ?_⟩
          · cases hpe : p.ending with
            | ret a =>
              simp only [lowerEnding, hpe]
              with_unfolding_all
                change some byteTy = some byteTy
                rfl
            | abort =>
              simp only [lowerEnding, hpe]
              with_unfolding_all
                change True
                trivial
          · intro op hop
            simp only [List.mem_map] at hop
            obtain ⟨inst, hinstMem, rfl⟩ := hop
            obtain ⟨_, _, _, _, _, _, _, _, hinstSpec⟩ := hspec.instMatches inst hinstMem
            rcases inst with ⟨orig, ci⟩
            cases ci with
            | call _ _ _ _ => exact False.elim hinstSpec
            | bind _ _ => with_unfolding_all trivial
            | assign _ _ => with_unfolding_all trivial
            | op _ _ _ _ _ => with_unfolding_all trivial
            | endLifetime _ => with_unfolding_all trivial
            | destructure _ _ => with_unfolding_all trivial
    · -- entry ∈ functions keys
      subst hmirEq; simp [MIR.logicalView, AList.map]
    · -- origins bind in project
      intro orig horig
      rw [hmirOriginsEq] at horig
      have hinCore : orig ∈ List.map Prod.snd core.machineProgram.origins := by
        rw [hcoreOrig]; exact horigPres.1 orig horig
      obtain ⟨⟨site, orig'⟩, hmemCore, heq⟩ := List.mem_map.mp hinCore
      dsimp only at heq; subst heq
      have hbindProj := (hwf.1.2.2.2.2.2 site orig' hmemCore).2
      have hprojEq : (mir.logicalView (language := sliceLanguage slice)).project = core.project := by
        subst hmirEq; rfl
      rw [hprojEq]; exact hbindProj
    · -- LayoutsWF
      intro tbl htbl; simp [MIR.logicalView] at htbl
    · -- DeclarationsWF
      have hdeclsEq : (mir.logicalView (language := sliceLanguage slice)).declarations = p.decls := by
        subst hmirEq; rfl
      rw [hdeclsEq]; exact decls_wf_of_declsOk p hdeclsOk
    · -- MutableLocalsWF
      have hmutEq : (mir.logicalView (language := sliceLanguage slice)).mutableLocals = [] := by
        subst hmirEq; rfl
      rw [hmutEq]; exact ⟨.nil, fun _ _ hm => nomatch hm⟩
  · -- Materializable
    intro fn locals graph hfnMem place ty hlocMem hneed
    have hfnEq : mir.functions = [(earlyEntry, (body.locals, { entry := ⟨0⟩, blocks := #[{
      id := ⟨0⟩, params := [], body := body.insts,
      ending := (lowerEnding (slice := slice) p).1,
      boundaryOrigins := body.boundary ++ (lowerEnding (slice := slice) p).2 }] }))] := by
      rw [hmirEq]
      rfl
    have hnoEsc : ¬ mir.Escapes (language := sliceLanguage slice) fn place :=
      mir_no_escapes mir hfnEq hspec.instMatches fn place
    rw [hfnEq] at hfnMem
    have hfnPair : fn = earlyEntry ∧ locals = body.locals := by
      rcases List.mem_singleton.mp hfnMem with heq
      have h1 := congrArg Prod.fst heq
      have h2 := congrArg (fun q => q.2.1) heq
      exact ⟨h1, h2⟩
    obtain ⟨rfl, rfl⟩ := hfnPair
    have hmirDecls : mir.declarations = p.decls := by rw [hmirEq]
    rcases hneed with hnonScalar | hesc
    · rcases hspec.localsTypes place ty hlocMem with rfl | ⟨name, rfl, hemp⟩
      · simp [sliceLanguage, byteTy] at hnonScalar
      · have htarget : profile.memoryTarget = linuxX8664 := by
          cases profile with | mk _ bt _ _ => cases bt <;> rfl
        have hlay : (sliceLanguage slice).layout mir.declarations profile.memoryTarget (nominalTy name) =
            some (.scalar 0 1) := by
          rw [hmirDecls, htarget]
          unfold sliceLanguage
          dsimp only
          have hprofEq : linuxX8664 = (Slice0Layout.linuxX8664Profile.withByteOrder linuxX8664.byteOrder).target := by
            decide
          split
          · decide
          · contradiction
        exact ⟨.scalar 0 1, hlay, sliceLanguage_layout_wf slice mir.declarations profile.memoryTarget
          (nominalTy name) (.scalar 0 1) hlay⟩
    · exact False.elim (hnoEsc hesc)

private theorem lowerWith_preservesOwnerMutability {slice : Slice} {profile : Profile}
    {core : CoreInput (sliceLanguage slice)} {v : AdmissionView}
    {discharges : Discharges (sliceLanguage slice)}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    (hrep : v.Represents core)
    (hmir : lowerProgram core.project v.located discharges = .ok mir) :
    PreservesOwnerMutability core mir := by
  have hmut : mir.mutableLocals = [] := by
    unfold lowerProgram at hmir
    cases hb : lowerBindings discharges v.located.freshBase v.located.bindings v.located.bindingOrigins 0 with
    | error _ => simp [hb, Bind.bind, Except.bind] at hmir
    | ok res =>
      simp only [hb, Bind.bind, Except.bind, Pure.pure, Except.pure, Except.ok.injEq] at hmir
      subst mir; rfl
  have hcoreOwn : core.machineProgram.ownership =
      StraightProgram.owners v.located.freshBase v.located.bindings 0 := by
    obtain ⟨_, eq, hpay⟩ := hrep
    have heq : eq = rfl := rfl
    rw [heq] at hpay
    simp [CoreInput.machineProgram, hpay, Payload.execution, AdmissionView.execution]
  constructor
  · intro fn locals graph _ root ty _
    rw [hmut]
    have hnotMut : (AList.lookup core.machineProgram.ownership ⟨root.id⟩).any (·.mutable) = false := by
      rw [hcoreOwn]
      cases hlu : AList.lookup (StraightProgram.owners v.located.freshBase v.located.bindings 0) ⟨root.id⟩ with
      | none => rfl
      | some desc =>
        have hmem := alist_lookup_mem hlu
        simp [owners_not_mutable v.located.freshBase v.located.bindings 0 ⟨root.id⟩ desc hmem]
    with_unfolding_all
      change false = ((AList.lookup core.machineProgram.ownership ⟨root.id⟩).any (·.mutable) && _)
      rw [hnotMut]
      rfl
  · intro fn roots root hmem _
    rw [hmut] at hmem
    cases hmem

private theorem lowerWith_consumesEvidence {slice : Slice} {profile : Profile}
    {core : CoreInput (sliceLanguage slice)} {v : AdmissionView}
    {discharges : Discharges (sliceLanguage slice)}
    {body : LowerRow slice} {mir : MIR (sliceLoweringLanguage slice) profile}
    (hrep : v.Represents core) (hwf : core.WF)
    (hdisch : ValidDischarges core discharges)
    (hbind : lowerBindings discharges v.located.freshBase v.located.bindings v.located.bindingOrigins 0 = .ok body)
    (hspec : LowerBindingsSpec v.located discharges [] 0 v.located.bindings v.located.bindingOrigins body)
    (hmir : lowerProgram core.project v.located discharges = .ok mir) :
    ConsumesEvidence core discharges mir := by
  let p := v.located
  obtain ⟨hsliceEq, hlangEq, hpay⟩ := hrep
  have hlangRfl : hlangEq = rfl := rfl
  rw [hlangRfl] at hpay
  have hmirEq : mir = {
    declarations := p.decls, project := core.project, entry := earlyEntry,
    signatures := [(earlyEntry, ⟨[], some byteTy⟩)],
    functions := [(earlyEntry, (body.locals, { entry := ⟨0⟩, blocks := #[{
      id := ⟨0⟩, params := [], body := body.insts,
      ending := (lowerEnding (slice := slice) p).1,
      boundaryOrigins := body.boundary ++ (lowerEnding (slice := slice) p).2 }] }))],
    mutableLocals := [] } := by
    unfold lowerProgram at hmir
    simp only [hbind, Bind.bind, Except.bind, Pure.pure, Except.pure, Except.ok.injEq] at hmir
    exact hmir.symm
  have hcoreOps : core.machineProgram.operations = v.execution.operations := by
    simp [CoreInput.machineProgram, hpay, Payload.execution]
    rfl
  have hcoreOrigs : core.machineProgram.origins = p.bindingOrigins ++ [p.endingOrigin] := by
    simp [CoreInput.machineProgram, hpay, Payload.execution, AdmissionView.execution, p]
  have hmirOps : mir.operations =
      body.insts.zipIdx.filterMap fun (inst, index) => match inst.instruction with
        | @MIRInst.op _ _ opcode _ _ _ _ args checks addresses =>
          some ⟨earlyEntry, ⟨0⟩, index, inst.origin, .operator opcode,
            args.identities ++ addresses.map (fun q => (q.1, .address q.2.root q.2.path)), checks⟩
        | .call callee _ args checks =>
          some ⟨earlyEntry, ⟨0⟩, index, inst.origin, .call callee,
            args.map (fun arg => (arg.1, arg.2.identity)), checks⟩
        | _ => none := by
    rw [hmirEq]
    unfold MIR.operations
    simp only [List.flatMap_cons, List.flatMap_nil, List.append_nil]
    congr 1
    funext ⟨inst, idx⟩
    cases inst.instruction <;> rfl
  have op_from_inst : ∀ op ∈ mir.operations,
      ∃ inst, body.insts[op.index]? = some inst ∧ inst ∈ body.insts ∧
        op.fn = earlyEntry ∧ op.block = ⟨0⟩ ∧ op.origin = inst.origin ∧
        ∃ opcode types ty dst opVal args checks addrs,
          inst.instruction = @MIRInst.op _ _ opcode types ty dst opVal args checks addrs ∧
          addrs = [] ∧
          op.opcode = .operator opcode ∧
          op.operands = args.identities ∧
          op.checks = checks := by
    intro op hopMem
    rw [hmirOps, List.mem_filterMap] at hopMem
    obtain ⟨⟨inst, idx⟩, hzipMem, hfilt⟩ := hopMem
    have hzs := List.mem_zipIdx hzipMem
    have hlt : idx < body.insts.length := by omega
    have hget : body.insts[idx]? = some inst := by
      rw [List.getElem?_eq_getElem hlt]; exact congrArg some (by simpa using hzs.2.2.symm)
    have hinstMem : inst ∈ body.insts := List.mem_of_getElem? hget
    obtain ⟨_, _, _, _, _, _, _, _, hinstSpec⟩ := hspec.instMatches inst hinstMem
    rcases inst with ⟨orig, ci⟩
    cases ci with
    | op dst opcode args checks addrs =>
      obtain ⟨rfl, _⟩ := hinstSpec
      cases Option.some.inj hfilt
      exact ⟨⟨orig, _⟩, hget, hinstMem, rfl, rfl, rfl, _, _, _, dst, opcode, args, checks, [],
        rfl, rfl, rfl, by simp, rfl⟩
    | call _ _ _ _ => exact False.elim hinstSpec
    | bind _ _ => cases hfilt
    | assign _ _ => cases hfilt
    | endLifetime _ => cases hfilt
    | destructure _ _ => cases hfilt
  have hidxNodup : (mir.operations.map fun op => (op.fn, op.block, op.index)).Nodup := by
    rw [hmirOps]
    have hgen : ∀ (xs : List (LocatedOp (MIRInst (sliceLoweringLanguage slice)))) (start : Nat),
        ((xs.zipIdx start).filterMap (fun (inst, index) => match inst.instruction with
          | @MIRInst.op _ _ opcode _ _ _ _ args checks addresses =>
            some (⟨earlyEntry, ⟨0⟩, index, inst.origin, .operator opcode,
              args.identities ++ addresses.map (fun q => (q.1, .address q.2.root q.2.path)), checks⟩ : MIR.Occurrence EarlyTy)
          | .call callee _ args checks =>
            some ⟨earlyEntry, ⟨0⟩, index, inst.origin, .call callee,
              args.map (fun arg => (arg.1, arg.2.identity)), checks⟩
          | _ => none)).map (fun op => (op.fn, op.block, op.index)) |>.Nodup := by
      intro xs
      induction xs with
      | nil => intro _; exact .nil
      | cons hd tl ih =>
        intro start
        have hge : ∀ op ∈ (tl.zipIdx (start + 1)).filterMap (fun (inst, index) => match inst.instruction with
            | @MIRInst.op _ _ opcode _ _ _ _ args checks addresses =>
              some (⟨earlyEntry, ⟨0⟩, index, inst.origin, .operator opcode,
                args.identities ++ addresses.map (fun q => (q.1, .address q.2.root q.2.path)), checks⟩ : MIR.Occurrence EarlyTy)
            | .call callee _ args checks =>
              some ⟨earlyEntry, ⟨0⟩, index, inst.origin, .call callee,
                args.map (fun arg => (arg.1, arg.2.identity)), checks⟩
            | _ => none), start + 1 ≤ op.index := by
          intro op hop
          rw [List.mem_filterMap] at hop
          obtain ⟨⟨⟨orig', ci'⟩, idx'⟩, hz', hf'⟩ := hop
          have hzs' := List.mem_zipIdx hz'
          cases ci' with
          | op _ _ _ _ _ => cases Option.some.inj hf'; dsimp only; omega
          | call _ _ _ _ => cases Option.some.inj hf'; dsimp only; omega
          | bind _ _ => cases hf'
          | assign _ _ => cases hf'
          | endLifetime _ => cases hf'
          | destructure _ _ => cases hf'
        simp only [List.zipIdx_cons, List.filterMap_cons]
        rcases hd with ⟨hdOrig, hdCi⟩
        cases hdCi with
        | bind _ _ | assign _ _ | endLifetime _ | destructure _ _ => exact ih (start + 1)
        | op _ _ _ _ _ | call _ _ _ _ =>
          simp only [List.map_cons, List.nodup_cons]
          refine ⟨fun hm => ?_, ih (start + 1)⟩
          obtain ⟨op', hop'Mem, hop'Eq⟩ := List.mem_map.mp hm
          have hle := hge op' hop'Mem
          have hidx := congrArg (fun t => t.2.2) hop'Eq
          dsimp only at hidx
          omega
    exact hgen body.insts 0
  have op_unique_by_site : ∀ op1 ∈ mir.operations, ∀ op2 ∈ mir.operations,
      op1.origin.operation = op2.origin.operation → op1 = op2 := by
    intro op1 h1 op2 h2 hsiteEq
    obtain ⟨i1, hg1, _, _, _, hor1, _⟩ := op_from_inst op1 h1
    obtain ⟨i2, hg2, _, _, _, hor2, _⟩ := op_from_inst op2 h2
    have hidxEq : op1.index = op2.index := by
      have hnd := hspec.instSitesNodup
      have h1lt := (mem_of_getElem?_some hg1).1
      have h2lt := (mem_of_getElem?_some hg2).1
      have hmapEq : (body.insts.map (fun i => i.origin.operation))[op1.index]'(by simpa using h1lt) =
          (body.insts.map (fun i => i.origin.operation))[op2.index]'(by simpa using h2lt) := by
        simp only [List.getElem_map]
        have hi1 : body.insts[op1.index]'h1lt = i1 := Option.some.inj ((List.getElem?_eq_getElem h1lt).symm.trans hg1)
        have hi2 : body.insts[op2.index]'h2lt = i2 := Option.some.inj ((List.getElem?_eq_getElem h2lt).symm.trans hg2)
        rw [hi1, hi2, ← hor1, ← hor2, hsiteEq]
      exact nodup_getElem_inj hnd (by simpa using h1lt) (by simpa using h2lt) hmapEq
    rw [hmirOps, List.mem_filterMap] at h1 h2
    obtain ⟨⟨⟨o1, ci1⟩, k1⟩, hz1, hf1⟩ := h1
    obtain ⟨⟨⟨o2, ci2⟩, k2⟩, hz2, hf2⟩ := h2
    have hk1 : k1 = op1.index := by
      cases ci1 with
      | op _ _ _ _ _ | call _ _ _ _ => exact congrArg MIR.Occurrence.index (Option.some.inj hf1)
      | bind _ _ | assign _ _ | endLifetime _ | destructure _ _ => cases hf1
    have hk2 : k2 = op2.index := by
      cases ci2 with
      | op _ _ _ _ _ | call _ _ _ _ => exact congrArg MIR.Occurrence.index (Option.some.inj hf2)
      | bind _ _ | assign _ _ | endLifetime _ | destructure _ _ => cases hf2
    have hkeq : k1 = k2 := hk1.trans (hidxEq.trans hk2.symm)
    subst hkeq
    have ha1 : body.insts[k1]? = some ⟨o1, ci1⟩ := by
      have hzs := List.mem_zipIdx hz1
      rw [List.getElem?_eq_getElem (by omega)]; exact congrArg some (by simpa using hzs.2.2.symm)
    have ha2 : body.insts[k1]? = some ⟨o2, ci2⟩ := by
      have hzs := List.mem_zipIdx hz2
      rw [List.getElem?_eq_getElem (by omega)]; exact congrArg some (by simpa using hzs.2.2.symm)
    cases Option.some.inj (ha1.symm.trans ha2)
    exact Option.some.inj (hf1.symm.trans hf2)
  have inst_op_in_mirOps : ∀ {inst : LocatedOp (MIRInst (sliceLoweringLanguage slice))},
      inst ∈ body.insts →
      ∀ {opcode types ty dst opVal args checks},
      inst.instruction = @MIRInst.op _ _ opcode types ty dst opVal args checks [] →
      ∃ op ∈ mir.operations, op.origin = inst.origin ∧ op.opcode = .operator opcode ∧
        op.operands = args.identities ∧ op.checks = checks := by
    intro inst hmem opcode types ty dst opVal args checks hci
    obtain ⟨idx, hlt, hget⟩ := List.mem_iff_getElem.mp hmem
    have hzip : (inst, idx) ∈ body.insts.zipIdx := by
      apply List.mem_of_getElem? (i := idx)
      rw [List.getElem?_zipIdx, List.getElem?_eq_getElem hlt]
      simp [hget]
    refine ⟨⟨earlyEntry, ⟨0⟩, idx, inst.origin, .operator opcode, args.identities, checks⟩, ?_, rfl, rfl, rfl, rfl⟩
    rw [hmirOps, List.mem_filterMap]
    refine ⟨(inst, idx), hzip, ?_⟩
    simp [hci]
  refine ⟨hidxNodup, ?_, ?_, ?_, hwf.2.1⟩
  · -- (2) Every Core `ConditionsAt` with nonempty `conditions` has a matching op in `mir.operations`
    intro site opcode conditions hcond hne
    obtain ⟨coreOp, hcoreOpMem, hnotAbort, rfl, rfl⟩ := hcond
    rw [hcoreOps] at hcoreOpMem
    rcases execution_operations_nonAbort_at v (fun h => by rw [h] at hnotAbort; exact hnotAbort) hcoreOpMem with
      ⟨pos, row, rfl, hget, rfl⟩ | ⟨a, _, _, rfl⟩
    · cases hex : row.statement.expression with
      | read _ | constructEmpty _ | consumeEmpty _ _ =>
        rw [hex] at hne; exact False.elim (hne rfl)
      | add ty l r =>
        have haddShape : ∃ l0 r0, ty = .u8 ∧ l = earlyOperand l0 ∧ r = earlyOperand r0 := by
          cases hst : row.statement with
          | slice0 s =>
            cases s with | mk _ e => cases e with
            | atom _ => simp [StraightBinding.expression, StraightBinding.rhs, hst, StraightExpr.integrated, earlyExpression] at hex
            | add l0 r0 =>
              simp only [StraightBinding.expression, StraightBinding.rhs, hst, StraightExpr.integrated,
                earlyExpression, EarlyExpr.add.injEq] at hex
              exact ⟨l0, r0, hex.1.symm, hex.2.1.symm, hex.2.2.symm⟩
          | bind _ _ rhs =>
            cases rhs with
            | scalar e => cases e with
              | atom _ => simp [StraightBinding.expression, StraightBinding.rhs, hst, StraightExpr.integrated, earlyExpression] at hex
              | add l0 r0 =>
                simp only [StraightBinding.expression, StraightBinding.rhs, hst, StraightExpr.integrated,
                  earlyExpression, EarlyExpr.add.injEq] at hex
                exact ⟨l0, r0, hex.1.symm, hex.2.1.symm, hex.2.2.symm⟩
            | move _ | construct _ =>
              simp [StraightBinding.expression, StraightBinding.rhs, hst, StraightExpr.integrated] at hex
          | destructure _ _ => simp [StraightBinding.expression, hst] at hex
        obtain ⟨l0, r0, rfl, rfl, rfl⟩ := haddShape
        have hlt := (mem_of_getElem?_some hget).1
        obtain ⟨inst, hinstMem, dst, hinstOp, hinstEq⟩ :=
          hspec.addComplete pos hlt row l0 r0 hget hex
        obtain ⟨op, hopMem, hopOrig, hopCode, _, _⟩ := inst_op_in_mirOps hinstMem hinstEq
        refine ⟨op, hopMem, ?_, ?_⟩
        · rw [hopOrig]; simpa using hinstOp
        · rw [hopCode]; rfl
    · exact False.elim (hne rfl)
  · -- (3) Every op in `mir.operations` matches `ConditionsAt`
    intro op hopMem
    obtain ⟨inst, _, hinstMem, _, _, horigEq, opcode, types, ty, dst, opVal, args, checks, addrs,
      hci, rfl, hopCode, hopOps, hopChecks⟩ := op_from_inst op hopMem
    obtain ⟨k, hk, row, orig, hrowGet, horigGet, hinstOrig, horigOp, hinstMatch⟩ :=
      hspec.instMatches inst hinstMem
    rw [hci] at hinstMatch
    obtain ⟨_, hcase⟩ := hinstMatch
    have hcoreOpMem : (⟨k⟩, CoreOperation.expression row.statement.expression) ∈ core.machineProgram.operations := by
      rw [hcoreOps]; exact execution_operations_binding_mem v hrowGet
    have horigSite : op.origin.operation = ⟨k⟩ := by
      rw [horigEq, hinstOrig]; simpa using horigOp
    have horigMemCore : (⟨k⟩, op.origin) ∈ core.machineProgram.origins := by
      rw [hcoreOrigs]
      apply List.mem_append_left
      rw [horigEq, hinstOrig]
      exact List.mem_of_getElem? (by simpa using horigGet)
    rcases hcase with ⟨l, r, hexpr, rfl, hargsId, rfl⟩ | ⟨name, hexpr, rfl, hargsId, rfl⟩
    · simp only [Nat.zero_add] at hopChecks hci
      have hcondAt : core.machineProgram.ConditionsAt op.origin.operation op.opcode
          [⟨0, .overflow, op.operands⟩] := by
        rw [horigSite, hopCode]
        refine ⟨.expression row.statement.expression, hcoreOpMem, trivial, ?_, ?_⟩
        · rw [hexpr]; rfl
        · rw [hexpr, add_safety_eq slice l r, hopOps, hargsId]
      refine ⟨[⟨0, .overflow, op.operands⟩], hcondAt, ?_, ?_, ?_, ?_⟩
      · rw [hopChecks]; simp [addCheck]
      · rw [hopChecks]
        simp only [List.map_cons, List.map_nil, addCheck]
        cases AList.lookup discharges ⟨k⟩ with
        | none => rfl
        | some gs => cases gs <;> rfl
      · intro callee hcal; rw [hopCode] at hcal; cases hcal
      · intro chk hchkMem hdisp
        rw [hopChecks, List.mem_singleton] at hchkMem
        subst hchkMem
        simp only [addCheck] at hdisp
        cases hlu : AList.lookup discharges ⟨k⟩ with
        | none => rw [hlu] at hdisp; cases hdisp
        | some gs =>
          cases gs with
          | nil => rw [hlu] at hdisp; cases hdisp
          | cons d ds =>
            have hcont : discharges.Contains ⟨k⟩ d := ⟨d :: ds, alist_lookup_mem hlu, List.mem_cons_self⟩
            obtain ⟨hdOp, hdObSite, hdObKind, ⟨opc', conds', hcondAt', hdInConds⟩,
              ⟨orig', horigMem', hdKey⟩, _⟩ := hdisch.2.2.2 ⟨k⟩ d hcont
            obtain ⟨coreOp', hcoreOp'Mem, _, _, rfl⟩ := hcondAt'
            have hlu1 := alist_lookup_of_mem hwf.1.2.1 hcoreOpMem
            have hlu2 := alist_lookup_of_mem hwf.1.2.1 hcoreOp'Mem
            cases Option.some.inj (hlu1.symm.trans hlu2)
            rw [hexpr, add_safety_eq slice l r, List.mem_singleton] at hdInConds
            have ho1 := alist_lookup_of_mem hwf.1.1 horigMemCore
            have ho2 := alist_lookup_of_mem hwf.1.1 horigMem'
            cases Option.some.inj (ho1.symm.trans ho2)
            refine ⟨d, horigSite ▸ hcont, ?_⟩
            unfold MIR.Occurrence.Justifies
            have hdDisc : d.obligation.discriminator = 0 := congrArg SafetyCondition.discriminator hdInConds
            have hdTrap : d.trap = .overflow := congrArg SafetyCondition.trap hdInConds
            have hdOps : d.operands = (Args.cons (lowerAtom l) (Args.cons (lowerAtom r) .nil)).identities :=
              congrArg SafetyCondition.operands hdInConds
            have hchkEq : addCheck discharges ⟨k⟩ = ⟨0, .overflow, .discharged⟩ := by simp [addCheck, hlu]
            rw [hchkEq]
            exact ⟨rfl, horigSite.trans hdOp, hdKey, hdDisc.symm, hdTrap.symm,
              hopOps.trans (hargsId.trans hdOps.symm)⟩
    · have hcondAt : core.machineProgram.ConditionsAt op.origin.operation op.opcode [] := by
        rw [horigSite, hopCode]
        refine ⟨.expression row.statement.expression, hcoreOpMem, trivial, ?_, ?_⟩
        · rw [hexpr]; rfl
        · rw [hexpr]; rfl
      refine ⟨[], hcondAt, ?_, ?_, ?_, ?_⟩
      · rw [hopChecks]; exact .nil
      · rw [hopChecks]; rfl
      · intro callee hcal; rw [hopCode] at hcal; cases hcal
      · intro chk hchkMem; rw [hopChecks] at hchkMem; cases hchkMem
  · -- (4) Every discharge in `discharges` is consumed uniquely at a matching op in `mir.operations`
    intro site d hcont
    obtain ⟨group, hgroupMem, hdInGroup⟩ := hcont
    obtain ⟨hdOp, hdObSite, hdObKind, ⟨opc', conds', hcondAt', hdInConds⟩,
      ⟨orig', horigMem', hdKey⟩, _⟩ := hdisch.2.2.2 site d ⟨group, hgroupMem, hdInGroup⟩
    obtain ⟨coreOp', hcoreOp'Mem, hnotAbort, _, rfl⟩ := hcondAt'
    have hcoreOp'V : (site, coreOp') ∈ v.execution.operations := hcoreOps ▸ hcoreOp'Mem
    rcases execution_operations_nonAbort_at v (fun h => by rw [h] at hnotAbort; exact hnotAbort) hcoreOp'V with
      ⟨pos, row, rfl, hrowGet, rfl⟩ | ⟨a, _, _, rfl⟩
    · cases hex : row.statement.expression with
      | read _ | constructEmpty _ | consumeEmpty _ _ =>
        rw [hex] at hdInConds; cases hdInConds
      | add ty l r =>
        have haddShape : ∃ l0 r0, ty = .u8 ∧ l = earlyOperand l0 ∧ r = earlyOperand r0 := by
          cases hst : row.statement with
          | slice0 s =>
            cases s with | mk _ e => cases e with
            | atom _ => simp [StraightBinding.expression, StraightBinding.rhs, hst, StraightExpr.integrated, earlyExpression] at hex
            | add l0 r0 =>
              simp only [StraightBinding.expression, StraightBinding.rhs, hst, StraightExpr.integrated,
                earlyExpression, EarlyExpr.add.injEq] at hex
              exact ⟨l0, r0, hex.1.symm, hex.2.1.symm, hex.2.2.symm⟩
          | bind _ _ rhs =>
            cases rhs with
            | scalar e => cases e with
              | atom _ => simp [StraightBinding.expression, StraightBinding.rhs, hst, StraightExpr.integrated, earlyExpression] at hex
              | add l0 r0 =>
                simp only [StraightBinding.expression, StraightBinding.rhs, hst, StraightExpr.integrated,
                  earlyExpression, EarlyExpr.add.injEq] at hex
                exact ⟨l0, r0, hex.1.symm, hex.2.1.symm, hex.2.2.symm⟩
            | move _ | construct _ =>
              simp [StraightBinding.expression, StraightBinding.rhs, hst, StraightExpr.integrated] at hex
          | destructure _ _ => simp [StraightBinding.expression, hst] at hex
        obtain ⟨l0, r0, rfl, rfl, rfl⟩ := haddShape
        have hlt := (mem_of_getElem?_some hrowGet).1
        obtain ⟨inst, hinstMem, dst, hinstOp, hinstEq⟩ :=
          hspec.addComplete pos hlt row l0 r0 hrowGet hex
        obtain ⟨op, hopMem, hopOrig, _, hopOps, hopChecks⟩ := inst_op_in_mirOps hinstMem hinstEq
        have hluDisch : AList.lookup discharges ⟨pos⟩ = some group :=
          alist_lookup_of_mem hdisch.2.1 hgroupMem
        have haddChk : addCheck discharges ⟨pos⟩ = ⟨0, .overflow, .discharged⟩ := by
          unfold addCheck; rw [hluDisch]
          cases group with
          | nil => cases hdInGroup
          | cons _ _ => rfl
        have hopSite : op.origin.operation = ⟨pos⟩ := by rw [hopOrig]; simpa using hinstOp
        have horigMemOp : (⟨pos⟩, op.origin) ∈ core.machineProgram.origins := by
          obtain ⟨k', _, _, orig'', _, horigGet', hinstOrig', horigOp', _⟩ := hspec.instMatches inst hinstMem
          have hkEq : k' = pos := by
            have := hinstOrig' ▸ hinstOp
            rw [horigOp'] at this; simpa using this
          subst hkEq
          rw [hcoreOrigs, hopOrig, hinstOrig']
          exact List.mem_append_left _ (List.mem_of_getElem? (by simpa using horigGet'))
        have ho1 := alist_lookup_of_mem hwf.1.1 horigMemOp
        have ho2 := alist_lookup_of_mem hwf.1.1 horigMem'
        have horigEq' : op.origin = orig' := Option.some.inj (ho1.symm.trans ho2)
        change d.condition ∈ (sliceLanguage slice).safety (.expression row.statement.expression) at hdInConds
        rw [hex, add_safety_eq slice l0 r0, List.mem_singleton] at hdInConds
        have hdDisc : d.obligation.discriminator = 0 := congrArg SafetyCondition.discriminator hdInConds
        have hdTrap : d.trap = .overflow := congrArg SafetyCondition.trap hdInConds
        have hdOps : d.operands = (Args.cons (lowerAtom l0) (Args.cons (lowerAtom r0) .nil)).identities :=
          congrArg SafetyCondition.operands hdInConds
        have hchkMem : ⟨0, .overflow, .discharged⟩ ∈ op.checks := by
          rw [hopChecks, Nat.zero_add, haddChk]; exact List.mem_cons_self
        refine ⟨op, hopMem, ⟨0, .overflow, .discharged⟩, hchkMem, ?_, ?_⟩
        · exact ⟨rfl, hopSite.trans hdOp, horigEq' ▸ hdKey, hdDisc.symm, hdTrap.symm,
            hopOps.trans hdOps.symm⟩
        · intro other hotherMem c hcMem hotherJust
          have hsameOp : other = op :=
            op_unique_by_site other hotherMem op hopMem (hotherJust.2.1.trans (hopSite.trans hdOp).symm)
          subst hsameOp
          rw [hopChecks, Nat.zero_add, haddChk, List.mem_singleton] at hcMem
          exact ⟨rfl, hcMem⟩
    · cases hdInConds

/-! ### Non-overflow of discharged sites -/

private theorem exec_final_trapped {O : Exec.Operands} {q : Exec.Config O}
    {k : TrapKind} {site : SiteId} (h : q.final? = some (.trapped k site)) :
    q.ctrl = .trapped k site := by
  cases q with
  | mk ctrl locs st fr =>
    cases ctrl with
    | trapped k' s' =>
      simp only [Exec.Config.final?, Option.some.injEq, Exec.Final.trapped.injEq] at h
      rw [h.1, h.2]
    | done => cases fr <;> simp [Exec.Config.final?] at h
    | exiting e => cases fr <;> (try cases e) <;> simp [Exec.Config.final?] at h
    | exec _ => simp [Exec.Config.final?] at h
    | refused => simp [Exec.Config.final?] at h

private theorem exec_step_to_overflow_active {language : CoreLanguage}
    (noPrim : language.operands.Prim → False)
    (payload : MachineProgram language)
    {prev next : Exec.Config payload.machine} {label : Exec.Label payload.machine}
    {site : SiteId}
    (hstep : Exec.Step payload.body prev label next)
    (_hnotPrev : prev.ctrl ≠ .trapped .overflow site)
    (hnext : next.ctrl = .trapped .overflow site) :
    ∃ op, payload.activeOperation prev = some (site, op) := by
  cases hstep with
  | prim h1 h2 h3 =>
    rename_i p _ _ _ _ _
    exact False.elim (noPrim p)
  | silent h =>
    have honRes_trapped : ∀ {V : Type} {r : Res (V × payload.machine.Store)} {st : SiteId}
        {L : AList Var payload.machine.Val} {store : payload.machine.Store}
        {k : List (Exec.Frame payload.machine)}
        {f : V → payload.machine.Store → Option (Exec.Out payload.machine)},
        Exec.onRes r st L store k f = some (.next next) →
        (∃ v st', r = .ok (v, st') ∧ f v st' = some (.next next)) ∨
        (st = site ∧ r = .trap .overflow) := by
      intro V r st L store k f hr
      cases r with
      | ok pair => exact .inl ⟨pair.1, pair.2, rfl, hr⟩
      | stuck => cases hr
      | trap kd =>
        simp only [Exec.onRes, Option.some.injEq, Exec.Out.next.injEq] at hr
        subst hr
        simp only [Exec.Ctrl.trapped.injEq] at hnext
        exact .inr ⟨hnext.2, hnext.1 ▸ rfl⟩
    cases prev with
    | mk ctrl locs store fr =>
      cases ctrl with
      | trapped _ _ => simp [Core.Exec.step] at h
      | refused => simp [Core.Exec.step] at h
      | exiting e =>
        cases fr with
        | nil => simp [Core.Exec.step, Core.Exec.stepExit] at h
        | cons f rest =>
          cases f with
          | seq _ => simp [Core.Exec.step, Core.Exec.stepExit] at h; subst h; cases hnext
          | body _ _ _ _ _ =>
            cases e <;> simp only [Core.Exec.step, Core.Exec.stepExit] at h
            · split at h <;> (cases Option.some.inj h; cases hnext)
            · split at h <;> (cases Option.some.inj h; cases hnext)
            · cases Option.some.inj h; cases hnext
          | scope _ dom defers =>
            cases defers with
            | cons d ds =>
              simp [Core.Exec.step, Core.Exec.stepExit, Core.Exec.startCleanup] at h
              subst h; cases hnext
            | nil =>
              simp only [Core.Exec.step, Core.Exec.stepExit, Option.map_eq_some_iff] at h
              obtain ⟨_, _, heq⟩ := h; cases heq; cases hnext
          | test _ _ _ _ _ _ =>
            simp only [Core.Exec.step, Core.Exec.stepExit, Option.map_eq_some_iff] at h
            obtain ⟨_, _, heq⟩ := h; cases heq; cases hnext
          | resume _ => simp [Core.Exec.step, Core.Exec.stepExit] at h
          | functionEnd =>
            cases e <;> simp only [Core.Exec.step, Core.Exec.stepExit, Option.map_eq_some_iff] at h
            · cases h
            · cases h
            · obtain ⟨_, _, heq⟩ := h; cases heq; cases hnext
          | ret dst caller =>
            cases e with
            | brk _ | cont _ => simp [Core.Exec.step, Core.Exec.stepExit] at h
            | ret v =>
              simp only [Core.Exec.step, Core.Exec.stepExit, Option.map_eq_some_iff] at h
              obtain ⟨_, _, heq⟩ := h; cases heq; cases hnext
      | done =>
        cases fr with
        | nil => simp [Core.Exec.step] at h
        | cons f rest =>
          cases f with
          | seq _ => simp [Core.Exec.step] at h; subst h; cases hnext
          | body _ _ _ _ _ => simp [Core.Exec.step] at h; subst h; cases hnext
          | scope _ dom defers =>
            cases defers with
            | cons _ _ =>
              simp [Core.Exec.step, Core.Exec.startCleanup] at h; subst h; cases hnext
            | nil =>
              simp only [Core.Exec.step, Option.map_eq_some_iff] at h
              obtain ⟨_, _, heq⟩ := h; cases heq; cases hnext
          | resume p =>
            cases p <;> simp [Core.Exec.step] at h <;> (subst h; cases hnext)
          | functionEnd =>
            simp only [Core.Exec.step, Option.map_eq_some_iff] at h
            obtain ⟨_, _, heq⟩ := h; cases heq; cases hnext
          | ret dst caller =>
            simp only [Core.Exec.step, Option.map_eq_some_iff] at h
            obtain ⟨_, _, heq⟩ := h; cases heq; cases hnext
          | test l c testExpr bodyStmt st dom =>
            simp only [Core.Exec.step] at h
            rcases honRes_trapped h with ⟨v, st1, _, hf⟩ | ⟨rfl, _⟩
            · cases htest : language.operands.test v with
              | none => simp [htest] at hf
              | some b =>
                rw [htest, Option.bind_some, Option.map_eq_some_iff] at hf
                obtain ⟨_, _, heq⟩ := hf
                cases b <;> (cases heq; cases hnext)
            · exact ⟨.expression testExpr, by simp [MachineProgram.activeOperation]⟩
      | exec stmt =>
        cases stmt with
        | skip => simp [Core.Exec.step] at h; subst h; cases hnext
        | seq _ _ => simp [Core.Exec.step] at h; subst h; cases hnext
        | block _ _ => simp [Core.Exec.step] at h; subst h; cases hnext
        | defer _ _ _ =>
          simp only [Core.Exec.step, Option.map_eq_some_iff] at h
          obtain ⟨_, _, heq⟩ := h; cases heq; cases hnext
        | loop _ _ _ _ _ => simp [Core.Exec.step] at h; subst h; cases hnext
        | brk _ => simp [Core.Exec.step] at h; subst h; cases hnext
        | cont _ => simp [Core.Exec.step] at h; subst h; cases hnext
        | abort st => simp [Core.Exec.step] at h; subst h; cases hnext
        | prim _ p _ _ => exact False.elim (noPrim p)
        | bind x e st =>
          simp only [Core.Exec.step] at h
          rcases honRes_trapped h with ⟨v, st1, _, hf⟩ | ⟨rfl, _⟩
          · simp only [Option.map_eq_some_iff] at hf
            obtain ⟨_, _, heq⟩ := hf; cases heq; cases hnext
          · exact ⟨.expression e, by simp [MachineProgram.activeOperation]⟩
        | assign x e st =>
          simp only [Core.Exec.step] at h
          rcases honRes_trapped h with ⟨v, st1, _, hf⟩ | ⟨rfl, _⟩
          · cases hlu : AList.lookup locs x with
            | none => rw [hlu] at hf; cases hf
            | some _ =>
              rw [hlu, Option.bind_some, Option.map_eq_some_iff] at hf
              obtain ⟨_, _, heq⟩ := hf; cases heq; cases hnext
          · exact ⟨.expression e, by simp [MachineProgram.activeOperation]⟩
        | cond e yes no st =>
          simp only [Core.Exec.step] at h
          rcases honRes_trapped h with ⟨v, st1, _, hf⟩ | ⟨rfl, _⟩
          · simp only [Option.map_eq_some_iff] at hf
            obtain ⟨_, _, heq⟩ := hf; cases heq; cases hnext
          · exact ⟨.expression e, by simp [MachineProgram.activeOperation]⟩
        | ret e st =>
          simp only [Core.Exec.step] at h
          rcases honRes_trapped h with ⟨v, st1, _, hf⟩ | ⟨rfl, _⟩
          · cases Option.some.inj hf; cases hnext
          · exact ⟨.expression e, by simp [MachineProgram.activeOperation]⟩
        | call dst fn args st =>
          simp only [Core.Exec.step] at h
          rcases honRes_trapped h with ⟨vs, st1, _, hf⟩ | ⟨rfl, _⟩
          · cases hfn : AList.lookup payload.body.fns fn with
            | none => rw [hfn] at hf; cases hf
            | some f =>
              rw [hfn, Option.bind_some] at hf
              split at hf
              · rw [Option.map_eq_some_iff] at hf
                obtain ⟨_, _, heq⟩ := hf; cases heq; cases hnext
              · cases hf
          · exact ⟨.call fn args, by simp [MachineProgram.activeOperation]⟩

private theorem exec_steps_to_overflow_active {language : CoreLanguage}
    (noPrim : language.operands.Prim → False)
    (payload : MachineProgram language)
    {start q : Exec.Config payload.machine} {events : List payload.machine.Event} {site : SiteId}
    (hsteps : Exec.Steps payload.body start events q)
    (hnotStart : start.ctrl ≠ .trapped .overflow site)
    (hq : q.ctrl = .trapped .overflow site) :
    ∃ prev evs label next op,
      Exec.Steps payload.body start evs prev ∧
      Exec.Step payload.body prev label next ∧
      payload.activeOperation prev = some (site, op) ∧
      next.ctrl = .trapped .overflow site := by
  induction hsteps with
  | refl => exact False.elim (hnotStart hq)
  | step hstep hrest ih =>
    rename_i c1 lbl c2 c3 evs
    have hcont : c2.ctrl ≠ .trapped .overflow site →
        ∃ prev evs label next op,
          Exec.Steps payload.body c1 evs prev ∧
          Exec.Step payload.body prev label next ∧
          payload.activeOperation prev = some (site, op) ∧
          next.ctrl = .trapped .overflow site := fun hne => by
      obtain ⟨prev, evs', label', next', op, hreach, hst', hop, hnxt⟩ := ih hne hq
      exact ⟨prev, lbl.events ++ evs', label', next', op, .step hstep hreach, hst', hop, hnxt⟩
    cases hc2 : c2.ctrl with
    | trapped k st =>
      cases k with
      | overflow =>
        if hst : st = site then
          subst hst
          obtain ⟨op, hop⟩ := exec_step_to_overflow_active noPrim payload hstep hnotStart hc2
          exact ⟨c1, [], lbl, c2, op, .refl c1, hstep, hop, hc2⟩
        else
          exact hcont (fun heq => by rw [hc2] at heq; cases heq; exact hst rfl)
      | _ => exact hcont (fun heq => by rw [hc2] at heq; cases heq)
    | _ => exact hcont (fun heq => by rw [hc2] at heq; cases heq)

private theorem core_observes_trapped_reaches {slice : Slice}
    {core : CoreInput (sliceLanguage slice)} {site : SiteId} {loc : DiagnosticLocation}
    {bytes : ByteArray}
    (hobs : CoreInput.Execution.observes core (.trapped 134 .overflow ⟨site, loc⟩ bytes)) :
    ∃ q events,
      Exec.Steps core.machineProgram.body core.machineProgram.initial events q ∧
      q.ctrl = .trapped .overflow site := by
  obtain ⟨s, ⟨start, hinit, hsteps⟩, hres⟩ := hobs
  have hlift : ∀ {a b : CoreInput.Execution.State},
      Steps (CoreInput.Execution.step core) a b →
      ∀ (p : Exec.Config core.machineProgram.machine) (evA : List core.machineProgram.machine.Event),
        a = (⟨(core.machineProgram.ownership, core.machineProgram.declarations), p⟩, evA) →
        ∃ (q : Exec.Config core.machineProgram.machine)
          (evB evs : List core.machineProgram.machine.Event),
          b = (⟨(core.machineProgram.ownership, core.machineProgram.declarations), q⟩, evB) ∧
          Exec.Steps core.machineProgram.body p evs q := by
    intro a b hpath
    induction hpath with
    | nil x =>
      intro p evA ha
      subst ha
      exact ⟨p, evA, [], rfl, .refl p⟩
    | @cons x y z hst _ ih =>
      intro p evA hx
      have ⟨p', q', lbl, hx', hy', hexec, _⟩ :
          ∃ (p' q' : Exec.Config core.machineProgram.machine) (lbl : Exec.Label core.machineProgram.machine),
            x.1 = ⟨(core.machineProgram.ownership, core.machineProgram.declarations), p'⟩ ∧
            y.1 = ⟨(core.machineProgram.ownership, core.machineProgram.declarations), q'⟩ ∧
            Exec.Step core.machineProgram.body p' lbl q' ∧
            y.2 = x.2 ++ lbl.events := by
        with_unfolding_all exact hst.2.1
      rw [hx] at hx'
      have hpEq : p = p' := by simpa using hx'
      subst hpEq
      have hyFull : y = (⟨(core.machineProgram.ownership, core.machineProgram.declarations), q'⟩, y.2) :=
        Prod.ext hy' rfl
      obtain ⟨q, evB, evs, hz, hrest⟩ := ih q' y.2 hyFull
      exact ⟨q, evB, lbl.events ++ evs, hz, Exec.Steps.step hexec hrest⟩
  have hinitEq : start = (⟨(core.machineProgram.ownership, core.machineProgram.declarations),
      core.machineProgram.initial⟩, []) := by
    with_unfolding_all exact hinit
  obtain ⟨q, evB, evs, hsEq, hreach⟩ := hlift hsteps core.machineProgram.initial [] hinitEq
  have hqFinal : q.ctrl = .trapped .overflow site := by
    have hcoreRes : (q.final?.bind fun outcome =>
        ((sliceLanguage slice).hostedResult outcome).bind (CoreInput.observeHosted core)) =
        some (.trapped 134 .overflow ⟨site, loc⟩ bytes) := by
      with_unfolding_all
        change (s.1.2.final?.bind fun outcome =>
          ((sliceLanguage slice).hostedResult outcome).bind (CoreInput.observeHosted core)) = _ at hres
        rw [hsEq] at hres
        exact hres
    cases hf : q.final? with
    | none => rw [hf] at hcoreRes; cases hcoreRes
    | some fin =>
      rw [hf, Option.bind_some] at hcoreRes
      cases hh : (sliceLanguage slice).hostedResult fin with
      | none => rw [hh] at hcoreRes; cases hcoreRes
      | some hosted =>
        rw [hh, Option.bind_some] at hcoreRes
        cases hosted with
        | returned _ => simp [CoreInput.observeHosted] at hcoreRes
        | trapped st kd si =>
          simp only [CoreInput.observeHosted] at hcoreRes
          cases ho : AList.lookup core.machineProgram.origins si with
          | none => simp [ho] at hcoreRes
          | some org =>
            simp only [ho, Bind.bind, Option.bind] at hcoreRes
            cases hr : runtimeSite core.project {org with operation := si} with
            | none => simp [hr] at hcoreRes
            | some resolved =>
              simp only [hr, Pure.pure, Option.some.injEq,
                Concrete.Contracts.Observation.trapped.injEq] at hcoreRes
              obtain ⟨_, rfl, hresEq, _⟩ := hcoreRes
              have hsiEq : si = site := by
                unfold runtimeSite at hr
                cases hfa : core.project.fileAt org.lexical.file with
                | none => simp [hfa] at hr
                | some f =>
                  simp only [hfa, Bind.bind, Option.bind, Pure.pure, Option.some.injEq] at hr
                  rw [← hr] at hresEq
                  exact congrArg DiagnosticSite.id hresEq
              subst hsiEq
              cases fin with
              | normal => simp [CoreLanguage.hostedResult] at hh
              | returned val =>
                cases val with
                | other _ => simp [CoreLanguage.hostedResult] at hh
                | scalar sv =>
                  cases sv with
                  | int t n =>
                    cases t with
                    | u8 =>
                      simp only [CoreLanguage.hostedResult] at hh
                      split at hh <;> cases hh
                    | _ => simp [CoreLanguage.hostedResult] at hh
                  | bool _ | unit | char _ => simp [CoreLanguage.hostedResult] at hh
              | trapped fk fsite =>
                simp only [CoreLanguage.hostedResult, Option.some.injEq, HostedResult.trapped.injEq] at hh
                obtain ⟨_, rfl, rfl⟩ := hh
                exact exec_final_trapped hf
  exact ⟨q, evs, hreach, hqFinal⟩

private theorem discharged_site_not_overflow {slice : Slice}
    {core : CoreInput (sliceLanguage slice)} {v : AdmissionView}
    {discharges : Discharges (sliceLanguage slice)} {site : SiteId}
    (hrep : v.Represents core) (hunchecked : core.UncheckedWF)
    (hdisch : ValidDischarges core discharges)
    (hrun : ConcreteSpec.run v.program (v.program.body.length + 1) = .trap .arithmeticOverflow)
    (hfirst : Agreement.firstTrapSite v.program.decls [] 0 v.program.body = some site) :
    AList.lookup discharges site = none := by
  obtain ⟨_, obs, hobsable, hobsIff⟩ := Agreement.unchecked_agreement hunchecked v hrep
  rw [hrun] at hobsable
  simp only [Agreement.Observable, hfirst, Option.bind_some] at hobsable
  have hobs : CoreInput.Execution.observes core obs := (hobsIff obs).mpr rfl
  cases ho : AList.lookup core.machineProgram.origins site with
  | none => simp [CoreInput.observeHosted, ho] at hobsable
  | some org =>
    simp only [CoreInput.observeHosted, ho, Bind.bind, Option.bind] at hobsable
    cases hr : runtimeSite core.project {org with operation := site} with
    | none => simp [hr] at hobsable
    | some resolved =>
      simp only [hr, Pure.pure, Option.some.injEq] at hobsable
      have hresId : resolved.id = site := by
        unfold runtimeSite at hr
        cases hfa : core.project.fileAt org.lexical.file with
        | none => simp [hfa] at hr
        | some f =>
          simp only [hfa, Bind.bind, Option.bind, Pure.pure, Option.some.injEq] at hr
          rw [← hr]
      have hobsTrapped : CoreInput.Execution.observes core
          (.trapped 134 .overflow ⟨site, resolved.location⟩ (coreTrapDiagnostic core .overflow site)) := by
        have hresEq : resolved = ⟨site, resolved.location⟩ := by
          cases resolved; simp only [DiagnosticSite.mk.injEq, and_true]; exact hresId
        rw [← hresEq, hobsable]; exact hobs
      obtain ⟨q, evs, hreach, hqCtrl⟩ := core_observes_trapped_reaches hobsTrapped
      have hinitNotTrap : core.machineProgram.initial.ctrl ≠ .trapped .overflow site := by
        obtain ⟨_, eq, hpay⟩ := hrep
        have heq : eq = rfl := rfl
        rw [heq] at hpay
        intro hbad
        unfold CoreInput.machineProgram at hbad
        rw [hpay] at hbad
        simp only [Payload.execution, AdmissionView.execution, MachineProgram.initial,
          MachineProgram.initial?, Exec.enter, CoreLanguage.emptyStore, Own.instrumentLife,
          Own.life, Own.enter, Bind.bind, Option.bind] at hbad
        cases hbad
      obtain ⟨prev, evs', lbl, next, actOp, hprevReach, hprevStep, hactive, hnextCtrl⟩ :=
        exec_steps_to_overflow_active (fun (p : Empty) => p.elim) core.machineProgram hreach hinitNotTrap hqCtrl
      cases hlu : AList.lookup discharges site with
      | none => rfl
      | some grp =>
        have hgrpMem := alist_lookup_mem hlu
        have hne := (hdisch.2.2.1 site grp hgrpMem).1
        cases grp with
        | nil => exact False.elim (hne rfl)
        | cons d ds =>
          have hcont : discharges.Contains site d := ⟨d :: ds, hgrpMem, List.mem_cons_self⟩
          obtain ⟨_, _, _, ⟨_, conds, hcondAt, hdInConds⟩, _, hvalidAt⟩ := hdisch.2.2.2 site d hcont
          have hdTrap : d.trap = .overflow := by
            obtain ⟨cop, _, hnotAb, _, rfl⟩ := hcondAt
            cases cop with
            | abort => exact False.elim hnotAb
            | call _ _ => cases hdInConds
            | primitive pr _ => nomatch pr
            | expression e =>
              cases e with
              | read _ | constructEmpty _ | consumeEmpty _ _ => cases hdInConds
              | add ty l r =>
                change d.condition ∈ (sliceLanguage slice).safety (.expression (.add ty l r)) at hdInConds
                simp only [sliceLanguage] at hdInConds
                split at hdInConds
                · exact congrArg SafetyCondition.trap (List.mem_singleton.mp hdInConds)
                · cases hdInConds
          have hnoTrap := (hvalidAt prev evs' actOp hprevReach hactive).2 lbl next hprevStep
          rw [hdTrap] at hnoTrap
          exact False.elim (hnoTrap hnextCtrl)

private theorem execStmt_trap_is_overflow {Δ : Decls} {env : Env} {s : ConcreteSpec.Stmt} {k : Trap}
    (_h : execStmt Δ env s = .trap k) : k = .arithmeticOverflow := by
  cases k; rfl

private theorem firstTrapSite_implies_run_overflow {Δ : Decls} {env : Env} {pos : Nat}
    {ss : List ConcreteSpec.Stmt} {ending : ConcreteSpec.Terminator} {site : SiteId}
    (hfirst : Agreement.firstTrapSite Δ env pos ss = some site) :
    ConcreteSpec.runBody Δ (ss.length + 1) env ss ending = .trap .arithmeticOverflow := by
  induction ss generalizing env pos with
  | nil => simp [Agreement.firstTrapSite] at hfirst
  | cons s rest ih =>
    simp only [Agreement.firstTrapSite] at hfirst
    cases he : execStmt Δ env s with
    | ok env' =>
      rw [he] at hfirst
      simp [ConcreteSpec.runBody, he, ih hfirst]
    | trap k =>
      have hk := execStmt_trap_is_overflow he
      subst hk
      simp [ConcreteSpec.runBody, he]
    | stuck _ => rw [he] at hfirst; cases hfirst

/-! ### Deterministic MIR simulation and `BehaviorRefinement` -/

/-- A deterministic finite execution chain in `MIR.Execution`. -/
private inductive DetChain {slice : Slice} {profile : Profile}
    (mir : MIR (sliceLoweringLanguage slice) profile) :
    MemoryRegisters (sliceLanguage slice) profile →
    MemoryRegisters (sliceLanguage slice) profile → Prop where
  | nil (s) : DetChain mir s s
  | cons {a b c} :
      (MIR.Execution (language := sliceLanguage slice)).step mir a b →
      (∀ d, (MIR.Execution (language := sliceLanguage slice)).step mir a d → d = b) →
      DetChain mir b c →
      DetChain mir a c

private theorem detChain_all_states {slice : Slice} {profile : Profile}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    {a b s : MemoryRegisters (sliceLanguage slice) profile} {obs : Concrete.Contracts.Observation}
    (h : DetChain mir a b)
    (hres : (MIR.Execution (language := sliceLanguage slice)).result mir b obs)
    (hreached : Steps ((MIR.Execution (language := sliceLanguage slice)).step mir) a s) :
    (s = b ∧ (MIR.Execution (language := sliceLanguage slice)).result mir s obs) ∨
    ∃ next, (MIR.Execution (language := sliceLanguage slice)).step mir s next := by
  induction h generalizing s with
  | nil x =>
    cases hreached with
    | nil => exact .inl ⟨rfl, hres⟩
    | cons hst _ => exact False.elim ((MIR.Execution (language := sliceLanguage slice)).terminal mir x obs hres _ hst)
  | cons hst huniq _ ih =>
    cases hreached with
    | nil => exact .inr ⟨_, hst⟩
    | cons hst' hrest =>
      have heq := huniq _ hst'
      subst heq
      exact ih hres hrest

private theorem detChain_no_diverges {slice : Slice} {profile : Profile}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    {a b : MemoryRegisters (sliceLanguage slice) profile} {obs : Concrete.Contracts.Observation}
    (h : DetChain mir a b)
    (hres : (MIR.Execution (language := sliceLanguage slice)).result mir b obs)
    (run : Nat → MemoryRegisters (sliceLanguage slice) profile)
    (hstart : run 0 = a)
    (hstep : ∀ i, (MIR.Execution (language := sliceLanguage slice)).step mir (run i) (run (i + 1))) :
    False := by
  have hfollow : ∀ {x y}, DetChain mir x y → ∀ k, run k = x → ∃ n, run n = y := by
    intro x y hc
    induction hc with
    | nil _ => intro k hk; exact ⟨k, hk⟩
    | cons _ huniq _ ih =>
      intro k hk
      exact ih (k + 1) (huniq _ (hk ▸ hstep k))
  obtain ⟨n, hn⟩ := hfollow h 0 hstart
  exact (MIR.Execution (language := sliceLanguage slice)).terminal mir b obs hres (run (n + 1)) (hn ▸ hstep n)

private theorem core_mir_trace_eq {slice : Slice} {profile : Profile}
    {core : CoreInput (sliceLanguage slice)}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    {p : CoreInput.Execution.State} {q : MemoryRegisters (sliceLanguage slice) profile}
    {obs : Concrete.Contracts.Observation}
    (hp : CoreInput.Execution.result core p obs)
    (hq : (MIR.Execution (language := sliceLanguage slice)).result mir q obs) :
    CoreInput.Execution.trace core p = (MIR.Execution (language := sliceLanguage slice)).trace mir q := by
  have heffP : p.2 = [] := list_empty_eq_nil p.2
  have heffQ : q.effects = [] := list_empty_eq_nil q.effects
  with_unfolding_all
    change p.2.map ProcessEvent.effect ++ _ = q.effects.map ProcessEvent.effect ++ _
    rw [heffP, heffQ, hp, hq]

private theorem behaviorRefinement_of_detChain {slice : Slice} {profile : Profile}
    {core : CoreInput (sliceLanguage slice)}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    {s0 sFinal : MemoryRegisters (sliceLanguage slice) profile}
    {obs : Concrete.Contracts.Observation}
    (hinit : ∀ s, (MIR.Execution (language := sliceLanguage slice)).initial mir s ↔ s = s0)
    (hchain : DetChain mir s0 sFinal)
    (hres : (MIR.Execution (language := sliceLanguage slice)).result mir sFinal obs)
    (hcoreObs : CoreInput.Execution.observes core obs) :
    BehaviorRefinement CoreInput.Execution (MIR.Execution (language := sliceLanguage slice))
      CoreInput.Execution.result (MIR.Execution (language := sliceLanguage slice)).result core mir := by
  intro _
  obtain ⟨p, hpFin, hpRes⟩ := hcoreObs
  have hresUniq : ∀ q o1 o2,
      (MIR.Execution (language := sliceLanguage slice)).result mir q o1 →
      (MIR.Execution (language := sliceLanguage slice)).result mir q o2 → o1 = o2 := by
    intro q o1 o2 h1 h2
    with_unfolding_all exact Option.some.inj (h1.symm.trans h2)
  refine ⟨?_, ?_, ?_⟩
  · intro q res hqFin hqRes
    obtain ⟨st, hstInit, hstSteps⟩ := hqFin
    have heq := (hinit st).mp hstInit
    subst heq
    rcases detChain_all_states hchain hres hstSteps with ⟨rfl, hfinalRes⟩ | ⟨next, hst⟩
    · have hresEq := hresUniq q res obs hqRes hfinalRes
      subst hresEq
      exact ⟨p, hpFin, hpRes, core_mir_trace_eq hpRes hqRes⟩
    · exact False.elim ((MIR.Execution (language := sliceLanguage slice)).terminal mir q res hqRes next hst)
  · intro run ⟨hr0, hrStep⟩
    exact False.elim (detChain_no_diverges hchain hres run ((hinit (run 0)).mp hr0) hrStep)
  · rintro q ⟨⟨st, hstInit, hstSteps⟩, hnoStep, hnoRes⟩
    have heq := (hinit st).mp hstInit
    subst heq
    rcases detChain_all_states hchain hres hstSteps with ⟨_, hqRes⟩ | ⟨next, hst⟩
    · exact hnoRes obs hqRes
    · exact hnoStep next hst

/-- Canonical flat logical MIR state during single-block straight-line execution. -/
private def flatState {slice : Slice} {profile : Profile}
    (pc : Nat) (live init : List PlaceId) (vals : AList PlaceId EarlyValue) :
    MemoryRegisters (sliceLanguage slice) profile :=
  ⟨⟨earlyEntry, ⟨0⟩, pc, vals, [], live, init, [], [], []⟩,
    Mem.Memory.empty, [], [], {}, 0, false⟩

/-- Values representable in the straight-line fragment. -/
private inductive RepVal : Ty → EarlyValue → Prop where
  | u8 {n : Int} : ConcreteSpec.IntTy.u8.inRange n = true →
      RepVal (.scalar (.int .u8)) (.scalar (.int .u8 n))
  | nominal (name : ConcreteSpec.Name) : RepVal (.struct name) (.other (nominalId name))

private def specValOf : EarlyValue → Val
  | .scalar (.int _ n) => .scalar (.int .u8 n)
  | .other id => .struct id.name []
  | _ => .scalar (.bool false)

private def tyToEarly : Ty → EarlyTy
  | .struct name => nominalTy name
  | _ => byteTy

/-- Simulation invariant relating Core's `(Γ, env)` to MIR's `flatState pc live init vals`. -/
private structure FlatInv (p : StraightProgram) (mirLocals : LocalTypes EarlyTy) (pos : Nat)
    (Γ : Ctx) (env : Env) (live init : List PlaceId) (vals : AList PlaceId EarlyValue) : Prop where
  initNodup : init.Nodup
  initSubLive : ∀ r ∈ init, r ∈ live
  liveLtInCtx : ∀ r ∈ live, r.id < p.freshBase → (AList.lookup Γ ⟨r.id⟩).isSome = true
  liveGeU8 : ∀ r ∈ live, p.freshBase ≤ r.id →
    r.id < p.freshBase + pos ∧ AList.lookup mirLocals r = some byteTy
  ctxAgree : ∀ x e, AList.lookup Γ x = some e →
    x.id < p.freshBase ∧
    ⟨x.id⟩ ∈ live ∧
    AList.lookup mirLocals ⟨x.id⟩ = some (tyToEarly e.ty) ∧
    (e.ty = .scalar (.int .u8) ∨
      ∃ name, e.ty = .struct name ∧ p.decls.emptyLinear (nominalId name) = true) ∧
    (e.live = true →
      ⟨x.id⟩ ∈ init ∧ ∃ v, AList.lookup vals ⟨x.id⟩ = some v ∧
        AList.lookup env x = some (.val (specValOf v)) ∧ RepVal e.ty v) ∧
    (e.live = false → ⟨x.id⟩ ∉ init)

private theorem flatInv_step_pos {p : StraightProgram} {mirLocals : LocalTypes EarlyTy} {pos : Nat}
    {Γ : Ctx} {env : Env} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    (h : FlatInv p mirLocals pos Γ env live init vals) :
    FlatInv p mirLocals (pos + 1) Γ env live init vals :=
  ⟨h.initNodup, h.initSubLive, h.liveLtInCtx,
    fun r hr hge => let ⟨hlt, hlu⟩ := h.liveGeU8 r hr hge; ⟨by omega, hlu⟩,
    h.ctxAgree⟩

private theorem flatInv_copy {p : StraightProgram} {mirLocals : LocalTypes EarlyTy} {pos : Nat}
    {Γ : Ctx} {env : Env} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    (h : FlatInv p mirLocals pos Γ env live init vals) (x : Var) :
    FlatInv p mirLocals pos (ctxMark Γ x false) env live init vals := by
  refine ⟨h.initNodup, h.initSubLive, ?_, h.liveGeU8, ?_⟩
  · intro r hr hlt
    rw [lookup_ctxMark]
    have hsome := h.liveLtInCtx r hr hlt
    by_cases hrx : (⟨r.id⟩ : Var) = x
    · simp only [hrx, ↓reduceIte]
      cases h0 : AList.lookup Γ x with
      | none => rw [hrx, h0] at hsome; cases hsome
      | some _ => rfl
    · simp [hrx, hsome]
  · intro y ey hlu
    rw [lookup_ctxMark] at hlu
    by_cases hyx : y = x
    · subst hyx
      simp only [↓reduceIte] at hlu
      cases h0 : AList.lookup Γ y with
      | none => rw [h0] at hlu; cases hlu
      | some e0 =>
        rw [h0, Option.map_some] at hlu
        cases Option.some.inj hlu
        simpa [Entry.read] using h.ctxAgree y e0 h0
    · simp only [hyx, ↓reduceIte] at hlu
      exact h.ctxAgree y ey hlu

private theorem flatInv_move {p : StraightProgram} {mirLocals : LocalTypes EarlyTy} {pos : Nat}
    {Γ : Ctx} {env : Env} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    (h : FlatInv p mirLocals pos Γ env live init vals) (x : Var) :
    FlatInv p mirLocals pos (ctxMark Γ x true) (AList.update env x (fun _ => .moved))
      live (init.erase ⟨x.id⟩) (AList.erase vals ⟨x.id⟩) := by
  refine ⟨list_nodup_erase h.initNodup ⟨x.id⟩,
    fun r hr => h.initSubLive r (List.mem_of_mem_erase hr),
    ?_, h.liveGeU8, ?_⟩
  · intro r hr hlt
    rw [lookup_ctxMark]
    have hsome := h.liveLtInCtx r hr hlt
    by_cases hrx : (⟨r.id⟩ : Var) = x
    · simp only [hrx, ↓reduceIte]
      cases h0 : AList.lookup Γ x with
      | none => rw [hrx, h0] at hsome; cases hsome
      | some _ => rfl
    · simp [hrx, hsome]
  · intro y ey hlu
    rw [lookup_ctxMark] at hlu
    by_cases hyx : y = x
    · subst hyx
      simp only [↓reduceIte] at hlu
      cases h0 : AList.lookup Γ y with
      | none => rw [h0] at hlu; cases hlu
      | some e0 =>
        rw [h0, Option.map_some] at hlu
        cases Option.some.inj hlu
        obtain ⟨h1, h2, h3, h4, _, _⟩ := h.ctxAgree y e0 h0
        refine ⟨h1, h2, h3, h4, fun hlv => by simp [Entry.read] at hlv,
          fun _ => list_not_mem_erase_of_nodup h.initNodup ⟨y.id⟩⟩
    · simp only [hyx, ↓reduceIte] at hlu
      obtain ⟨h1, h2, h3, h4, h5, h6⟩ := h.ctxAgree y ey hlu
      have hpne : (⟨y.id⟩ : PlaceId) ≠ ⟨x.id⟩ := fun heq => hyx (by cases x; cases y; cases heq; rfl)
      refine ⟨h1, h2, h3, h4, ?_, fun hfls hm => h6 hfls (List.mem_of_mem_erase hm)⟩
      intro hlv
      obtain ⟨hin, v, hv, henv, hrep⟩ := h5 hlv
      refine ⟨list_mem_erase_of_ne hin hpne, v, ?_, ?_, hrep⟩
      · rw [alist_lookup_erase_ne vals (Ne.symm hpne)]; exact hv
      · rw [alist_lookup_update]; simp only [hyx, ↓reduceIte]; exact henv

private theorem flatInv_bind_named {p : StraightProgram} {mirLocals : LocalTypes EarlyTy} {pos : Nat}
    {Γ : Ctx} {env : Env} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    (h : FlatInv p mirLocals pos Γ env live init vals)
    {x : Var} {e : Entry} {v : EarlyValue}
    (hxlt : x.id < p.freshBase) (hfresh : AList.lookup Γ x = none)
    (hliveEq : e.live = true)
    (hluLoc : AList.lookup mirLocals ⟨x.id⟩ = some (tyToEarly e.ty))
    (htyInfo : e.ty = .scalar (.int .u8) ∨ ∃ name, e.ty = .struct name ∧ p.decls.emptyLinear (nominalId name) = true)
    (hrep : RepVal e.ty v) :
    FlatInv p mirLocals (pos + 1) ((x, e) :: Γ) ((x, .val (specValOf v)) :: env)
      (⟨x.id⟩ :: live) (⟨x.id⟩ :: init) ((⟨x.id⟩, v) :: AList.erase vals ⟨x.id⟩) := by
  have hnotLive : (⟨x.id⟩ : PlaceId) ∉ live := by
    intro hm
    have hsome := h.liveLtInCtx ⟨x.id⟩ hm hxlt
    simp [hfresh] at hsome
  have hnotInit : (⟨x.id⟩ : PlaceId) ∉ init := fun hm => hnotLive (h.initSubLive _ hm)
  refine ⟨List.nodup_cons.mpr ⟨hnotInit, h.initNodup⟩, ?_, ?_, ?_, ?_⟩
  · intro r hr
    rcases List.mem_cons.mp hr with rfl | hr'
    · exact List.mem_cons_self
    · exact List.mem_cons_of_mem _ (h.initSubLive r hr')
  · intro r hr hlt
    rcases List.mem_cons.mp hr with rfl | hr'
    · simp [AList.lookup]
    · simp only [AList.lookup]
      by_cases hxr : x = ⟨r.id⟩
      · simp [hxr]
      · simp [hxr, h.liveLtInCtx r hr' hlt]
  · intro r hr hge
    rcases List.mem_cons.mp hr with rfl | hr'
    · dsimp only at hge; omega
    · obtain ⟨hlt, hlu⟩ := h.liveGeU8 r hr' hge
      exact ⟨by omega, hlu⟩
  · intro y ey hlu
    simp only [AList.lookup] at hlu
    by_cases hxy : x = y
    · subst hxy
      simp only [↓reduceIte, Option.some.injEq] at hlu
      subst hlu
      refine ⟨hxlt, List.mem_cons_self, hluLoc, htyInfo, ?_, fun hfls => by rw [hliveEq] at hfls; cases hfls⟩
      intro _
      exact ⟨List.mem_cons_self, v, by simp [AList.lookup], by simp [AList.lookup], hrep⟩
    · simp only [hxy, ↓reduceIte] at hlu
      obtain ⟨h1, h2, h3, h4, h5, h6⟩ := h.ctxAgree y ey hlu
      have hpne : (⟨x.id⟩ : PlaceId) ≠ ⟨y.id⟩ := fun heq => hxy (by cases x; cases y; cases heq; rfl)
      refine ⟨h1, List.mem_cons_of_mem _ h2, h3, h4, ?_, ?_⟩
      · intro hlv
        obtain ⟨hin, vy, hvy, henvy, hrepy⟩ := h5 hlv
        refine ⟨List.mem_cons_of_mem _ hin, vy, ?_, ?_, hrepy⟩
        · simp [AList.lookup, hpne, alist_lookup_erase_ne vals hpne, hvy]
        · simp [AList.lookup, hxy, henvy]
      · intro hfls hm
        rcases List.mem_cons.mp hm with heq | hm'
        · exact hpne heq.symm
        · exact h6 hfls hm'

private theorem flatInv_bind_wild {p : StraightProgram} {mirLocals : LocalTypes EarlyTy} {pos : Nat}
    {Γ : Ctx} {env : Env} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    (h : FlatInv p mirLocals pos Γ env live init vals)
    (v : EarlyValue)
    (hluLoc : AList.lookup mirLocals ⟨p.freshBase + pos⟩ = some byteTy) :
    FlatInv p mirLocals (pos + 1) Γ env
      (⟨p.freshBase + pos⟩ :: live) (⟨p.freshBase + pos⟩ :: init)
      ((⟨p.freshBase + pos⟩, v) :: AList.erase vals ⟨p.freshBase + pos⟩) := by
  have hnotLive : (⟨p.freshBase + pos⟩ : PlaceId) ∉ live := by
    intro hm
    have hlt := (h.liveGeU8 ⟨p.freshBase + pos⟩ hm (Nat.le_add_right _ _)).1
    dsimp only at hlt; omega
  have hnotInit : (⟨p.freshBase + pos⟩ : PlaceId) ∉ init := fun hm => hnotLive (h.initSubLive _ hm)
  refine ⟨List.nodup_cons.mpr ⟨hnotInit, h.initNodup⟩, ?_, ?_, ?_, ?_⟩
  · intro r hr
    rcases List.mem_cons.mp hr with rfl | hr'
    · exact List.mem_cons_self
    · exact List.mem_cons_of_mem _ (h.initSubLive r hr')
  · intro r hr hlt
    rcases List.mem_cons.mp hr with rfl | hr'
    · dsimp only at hlt; omega
    · exact h.liveLtInCtx r hr' hlt
  · intro r hr hge
    rcases List.mem_cons.mp hr with rfl | hr'
    · exact ⟨by dsimp only; omega, hluLoc⟩
    · obtain ⟨hlt, hlu⟩ := h.liveGeU8 r hr' hge
      exact ⟨by omega, hlu⟩
  · intro y ey hlu
    obtain ⟨h1, h2, h3, h4, h5, h6⟩ := h.ctxAgree y ey hlu
    have hpne : (⟨p.freshBase + pos⟩ : PlaceId) ≠ ⟨y.id⟩ := fun heq => by
      have : p.freshBase + pos = y.id := congrArg PlaceId.id heq
      omega
    refine ⟨h1, List.mem_cons_of_mem _ h2, h3, h4, ?_, ?_⟩
    · intro hlv
      obtain ⟨hin, vy, hvy, henvy, hrepy⟩ := h5 hlv
      refine ⟨List.mem_cons_of_mem _ hin, vy, ?_, henvy, hrepy⟩
      simp [AList.lookup, hpne, alist_lookup_erase_ne vals hpne, hvy]
    · intro hfls hm
      rcases List.mem_cons.mp hm with heq | hm'
      · exact hpne heq.symm
      · exact h6 hfls hm'

/-! ### `flatState` logical-view execution lemmas -/

private def toMemRead {Ty : Type} {Lit : Ty → Type} {ty : Ty} :
    Read Ty Lit ty → MemoryRead Ty Lit ty
  | .literal lit => .literal lit
  | .copy pl => .copy ⟨pl, .virtual⟩
  | .move pl => .move ⟨pl, .virtual⟩

private theorem mir_step_of_bind {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {mir : MIR lang profile}
    {s d : MemoryRegisters language profile}
    {origin : Origin} {ty : language.operands.Ty} {dst : Place language.operands.Ty ty}
    {src : Read language.operands.Ty lang.literal ty}
    (hinst : ((AList.lookup mir.functions s.frame).bind fun (_, g) =>
      (g.blocks.find? (·.id == s.block)).bind fun b => b.body[s.instruction]?) =
      some ⟨origin, .bind dst src⟩) :
    MIR.Step mir s d ↔
      mir.logicalView.instructionTrap s (.bind ⟨dst, .virtual⟩ (toMemRead src)) = none ∧
      mir.logicalView.InstructionStep s d true (.bind ⟨dst, .virtual⟩ (toMemRead src)) := by
  unfold MIR.Step
  with_unfolding_all
    change (match ((AList.lookup mir.functions s.frame).bind fun (_, g) =>
      (g.blocks.find? (·.id == s.block)).bind fun b => b.body[s.instruction]?) with
      | some op => _ | none => _) ↔ _
    rw [hinst]
    cases src <;> exact Iff.rfl

private theorem mir_step_of_op0 {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {mir : MIR lang profile}
    {s d : MemoryRegisters language profile}
    {origin : Origin} {opcode : Operator} {ty : language.operands.Ty}
    {dst : Place language.operands.Ty ty} {hasOp : lang.operation opcode [] ty}
    {checks : List OperationCheck}
    (hinst : ((AList.lookup mir.functions s.frame).bind fun (_, g) =>
      (g.blocks.find? (·.id == s.block)).bind fun b => b.body[s.instruction]?) =
      some ⟨origin, .op (opcode := opcode) (ty := ty) dst hasOp .nil checks []⟩) :
    MIR.Step mir s d ↔
      mir.logicalView.instructionTrap s (.op (opcode := opcode) (ty := ty) ⟨dst, .virtual⟩ hasOp .nil checks []) = none ∧
      mir.logicalView.InstructionStep s d true (.op (opcode := opcode) (ty := ty) ⟨dst, .virtual⟩ hasOp .nil checks []) := by
  unfold MIR.Step
  with_unfolding_all
    change (match ((AList.lookup mir.functions s.frame).bind fun (_, g) =>
      (g.blocks.find? (·.id == s.block)).bind fun b => b.body[s.instruction]?) with
      | some op => _ | none => _) ↔ _
    rw [hinst]
    exact Iff.rfl

private theorem mir_step_of_op2 {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {mir : MIR lang profile}
    {s d : MemoryRegisters language profile}
    {origin : Origin} {opcode : Operator} {t1 t2 ty : language.operands.Ty}
    {dst : Place language.operands.Ty ty} {hasOp : lang.operation opcode [t1, t2] ty}
    {a1 : Read language.operands.Ty lang.literal t1}
    {a2 : Read language.operands.Ty lang.literal t2}
    {checks : List OperationCheck}
    (hinst : ((AList.lookup mir.functions s.frame).bind fun (_, g) =>
      (g.blocks.find? (·.id == s.block)).bind fun b => b.body[s.instruction]?) =
      some ⟨origin, .op (opcode := opcode) (ty := ty) dst hasOp (.cons a1 (.cons a2 .nil)) checks []⟩) :
    MIR.Step mir s d ↔
      mir.logicalView.instructionTrap s
        (.op (opcode := opcode) (ty := ty) ⟨dst, .virtual⟩ hasOp (.cons (toMemRead a1) (.cons (toMemRead a2) .nil)) checks []) = none ∧
      mir.logicalView.InstructionStep s d true
        (.op (opcode := opcode) (ty := ty) ⟨dst, .virtual⟩ hasOp (.cons (toMemRead a1) (.cons (toMemRead a2) .nil)) checks []) := by
  unfold MIR.Step
  with_unfolding_all
    change (match ((AList.lookup mir.functions s.frame).bind fun (_, g) =>
      (g.blocks.find? (·.id == s.block)).bind fun b => b.body[s.instruction]?) with
      | some op => _ | none => _) ↔ _
    rw [hinst]
    cases a1 <;> cases a2 <;> exact Iff.rfl

private theorem mir_step_of_destructure0 {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {mir : MIR lang profile}
    {s d : MemoryRegisters language profile}
    {origin : Origin} {ty : language.operands.Ty} {src : Place language.operands.Ty ty}
    (hinst : ((AList.lookup mir.functions s.frame).bind fun (_, g) =>
      (g.blocks.find? (·.id == s.block)).bind fun b => b.body[s.instruction]?) =
      some ⟨origin, .destructure (ty := ty) src []⟩) :
    MIR.Step mir s d ↔
      mir.logicalView.instructionTrap s (.destructure (ty := ty) ⟨src, .virtual⟩ []) = none ∧
      mir.logicalView.InstructionStep s d true (.destructure (ty := ty) ⟨src, .virtual⟩ []) := by
  unfold MIR.Step
  with_unfolding_all
    change (match ((AList.lookup mir.functions s.frame).bind fun (_, g) =>
      (g.blocks.find? (·.id == s.block)).bind fun b => b.body[s.instruction]?) with
      | some op => _ | none => _) ↔ _
    rw [hinst]
    exact Iff.rfl

private theorem array_find?_map {α β : Type} (arr : Array α) (f : α → β) (q : β → Bool) :
    (arr.map f).find? q = (arr.find? (fun x => q (f x))).map f := by
  rw [← Array.find?_toList, ← Array.find?_toList, Array.toList_map]
  induction arr.toList with
  | nil => rfl
  | cons a rest ih =>
    simp only [List.map_cons, List.find?_cons]
    cases q (f a) <;> simp [ih]

private theorem mir_terminalObservation_ret {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {mir : MIR lang profile}
    {s : MemoryRegisters language profile} {locals mg mb}
    {ty : language.operands.Ty} {r : Read language.operands.Ty lang.literal ty}
    (hmg : AList.lookup mir.functions s.frame = some (locals, mg))
    (hmb : mg.blocks.find? (·.id == s.block) = some mb)
    (hlen : s.instruction = mb.body.length)
    (hend : mb.ending = .ret (some ⟨ty, r⟩))
    (hcall : s.callers = []) (hentry : s.frame = mir.entry) (hres : s.resourceExhausted = false) :
    mir.logicalView.terminalObservation s =
      (mir.logicalView.readValue s (toMemRead r)).bind fun (v, after) =>
        if !after.live.all (mir.logicalView.canEnd after) then none
        else (language.hostedResult (.returned v)).bind fun hr =>
          match hr with
          | .returned status => some (.returned status ByteArray.empty)
          | _ => none := by
  have hblk : ∃ lblock, mir.logicalView.blockAt s = some lblock ∧
      lblock.body.length = mb.body.length ∧
      lblock.body[mb.body.length]? = none ∧
      lblock.ending = .ret (some ⟨ty, toMemRead r⟩) := by
    unfold Memory.blockAt Memory.graphAt MIR.logicalView
    rw [alist_lookup_map, hmg]
    simp only [Option.map_some, Bind.bind, Option.bind]
    rw [array_find?_map]
    dsimp only
    rw [hmb]
    refine ⟨_, rfl, by simp, by simp, ?_⟩
    dsimp only
    rw [hend]
    cases r <;> (with_unfolding_all rfl)
  obtain ⟨lblock, hblkAt, hblen, hbnone, hbend⟩ := hblk
  have hviewEnt : mir.logicalView.entry = mir.entry := rfl
  unfold Memory.terminalObservation
  rw [hres, hblkAt]
  simp only [Bool.false_eq_true, ↓reduceIte, Bind.bind, Option.bind,
    hlen, hbnone, hblen, bne_self_eq_false, hbend, hcall, List.isEmpty_nil, hentry,
    hviewEnt, Bool.not_true, Bool.false_or]
  rfl

private theorem memory_validPlace_virtual {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {memory : Memory lang profile} {s : MemoryRegisters language profile}
    {r : PlaceId} {ty : language.operands.Ty}
    (hcls : memory.classAt s.frame r = some .virtual)
    (hrt : memory.rootType s.frame r = some ty) :
    memory.validPlace s (⟨⟨r, []⟩, .virtual⟩ : MemoryPlace language.operands.Ty ty) = true := by
  unfold Memory.validPlace
  simp only [hcls, hrt, Bind.bind, Option.bind, List.foldlM_nil, Pure.pure, and_self, decide_true]

section FlatLogical

variable {slice : Slice} {profile : Profile}
variable {mir : MIR (sliceLoweringLanguage slice) profile}
variable {mirLocals : LocalTypes EarlyTy} {block : MIRBlock slice}

private structure SingleBlockMir (mir : MIR (sliceLoweringLanguage slice) profile)
    (mirLocals : LocalTypes EarlyTy) (block : MIRBlock slice) : Prop where
  entryEq : mir.entry = earlyEntry
  mutEq : mir.mutableLocals = []
  fnsEq : mir.functions = [(earlyEntry, (mirLocals, { entry := ⟨0⟩, blocks := #[block] }))]
  blkId : block.id = ⟨0⟩
  blkParams : block.params = []
  localsNodup : (List.map Prod.fst mirLocals).Nodup

private theorem sbm_initial_iff (hsbm : SingleBlockMir mir mirLocals block)
    (s : MemoryRegisters (sliceLanguage slice) profile) :
    (MIR.Execution (language := sliceLanguage slice)).initial mir s ↔ s = flatState 0 [] [] [] := by
  rcases block with ⟨blkId, blkParams, blkBody, blkEnd, blkBnd⟩
  have hbid : blkId = ⟨0⟩ := hsbm.blkId
  have hbp : blkParams = [] := hsbm.blkParams
  subst hbid; subst hbp
  have hent := hsbm.entryEq
  have hfns := hsbm.fnsEq
  with_unfolding_all
    change s = _ ↔ _
    dsimp only
    unfold MIR.logicalView
    rw [hent, hfns]
    rfl

private theorem sbm_rootType (hsbm : SingleBlockMir mir mirLocals block) (r : PlaceId) :
    (mir.logicalView (language := sliceLanguage slice)).rootType earlyEntry r =
      AList.lookup mirLocals r := by
  unfold Memory.rootType MIR.logicalView
  rw [hsbm.fnsEq]
  dsimp only [AList.map, List.map, AList.lookup]
  simp only [↓reduceIte, Bind.bind, Option.bind]
  rfl

private theorem sbm_classAt (hsbm : SingleBlockMir mir mirLocals block) {r : PlaceId} {ty : EarlyTy}
    (hlu : AList.lookup mirLocals r = some ty) :
    (mir.logicalView (language := sliceLanguage slice)).classAt earlyEntry r = some .virtual := by
  unfold Memory.classAt MIR.logicalView
  rw [hsbm.fnsEq]
  dsimp only [AList.map, List.map, AList.lookup]
  simp only [↓reduceIte, Bind.bind, Option.bind]
  change AList.lookup (AList.map (fun _ => StorageClass.virtual) mirLocals) r = some .virtual
  rw [alist_lookup_map, hlu]; rfl

private theorem sbm_validPlace (hsbm : SingleBlockMir mir mirLocals block)
    (pc : Nat) (live init : List PlaceId) (vals : AList PlaceId EarlyValue)
    {r : PlaceId} {ty : EarlyTy} (hlu : AList.lookup mirLocals r = some ty) :
    (mir.logicalView (language := sliceLanguage slice)).validPlace
      (flatState (slice := slice) (profile := profile) pc live init vals)
      (⟨⟨r, []⟩, .virtual⟩ : MemoryPlace EarlyTy ty) = true :=
  memory_validPlace_virtual (s := flatState pc live init vals) (ty := ty)
    (sbm_classAt hsbm hlu) ((sbm_rootType hsbm r).trans hlu)

private theorem sbm_ownerView (hsbm : SingleBlockMir mir mirLocals block)
    (pc : Nat) (live init : List PlaceId) (vals : AList PlaceId EarlyValue)
    (r : PlaceId) (ty : EarlyTy) (st : Own.Status) :
    (mir.logicalView (language := sliceLanguage slice)).ownerView
      (flatState (slice := slice) (profile := profile) pc live init vals) r ty st =
      ({ slots := [⟨⟨0⟩, 0, ⟨((sliceLanguage slice).scalarType ty).getD .unit,
          (sliceLanguage slice).operands.isCopy ty, false⟩, st⟩] }, ⟨0⟩) := by
  simp [Memory.ownerView, MIR.logicalView, hsbm.mutEq, flatState, AList.lookup, Own.slot]

private theorem sbm_loadValue_virtual (hsbm : SingleBlockMir mir mirLocals block)
    (pc : Nat) (live init : List PlaceId) (vals : AList PlaceId EarlyValue)
    {r : PlaceId} {ty : EarlyTy} {v : EarlyValue}
    (hlive : r ∈ live) (hinit : r ∈ init)
    (hluLoc : AList.lookup mirLocals r = some ty)
    (hv : AList.lookup vals r = some v) :
    (mir.logicalView (language := sliceLanguage slice)).loadValue
      (flatState (slice := slice) (profile := profile) pc live init vals)
      (⟨⟨r, []⟩, .virtual⟩ : MemoryPlace EarlyTy ty) = some v := by
  have hvp := sbm_validPlace hsbm pc live init vals hluLoc
  unfold Memory.loadValue Memory.loadBacking
  simp only [hvp, Bool.not_true, Bool.false_eq_true, ↓reduceIte]
  simp only [Memory.initializedAt, flatState, List.contains_iff_mem.mpr hinit,
    List.contains_iff_mem.mpr hlive, List.any_nil, Bool.not_false, Bool.and_true,
    Bool.not_true, Bool.false_eq_true, ↓reduceIte, Bind.bind, Option.bind, List.isEmpty_nil]
  change (Option.bind (AList.lookup vals r) (fun root => some root)) = some v
  rw [hv]; rfl

private theorem sbm_readAtom (hsbm : SingleBlockMir mir mirLocals block)
    (pc : Nat) (live init : List PlaceId) (vals : AList PlaceId EarlyValue)
    {p : StraightProgram} {pos : Nat} {Γ : Ctx} {env : Env}
    (hinv : FlatInv p mirLocals pos Γ env live init vals)
    {a : Slice0.Atom} {Γ' : Ctx}
    (ht : AtomTy Γ.tys a.toCore (.int .u8))
    (hu : UseAtom (List.map Prod.snd p.decls) Γ a.toCore Γ') :
    ∃ n : Int, ConcreteSpec.IntTy.u8.inRange n = true ∧
      (mir.logicalView (language := sliceLanguage slice)).readValue
        (flatState (slice := slice) (profile := profile) pc live init vals)
        (toMemRead (lowerAtom a)) =
        some (.scalar (.int .u8 n), flatState pc live init vals) ∧
      evalAtom (List.map Prod.snd p.decls) env a.toCore = .ok (.int .u8 n, env) ∧
      FlatInv p mirLocals pos Γ' env live init vals := by
  cases a with
  | lit n =>
    cases ht with
    | int hrange =>
      cases hu with
      | int =>
        have hinR : Core.IntTy.u8.InRange n := of_decide_eq_true hrange
        have hrt := ConcreteSpec.StageInterface.earlyIntegerLiteralRoundTrip slice .u8 n hinR
        refine ⟨n, hrange, ?_, rfl, hinv⟩
        unfold Memory.readValue
        dsimp only [lowerAtom, toMemRead, encodeU8Literal]
        change Option.bind ((sliceLanguage slice).literal byteTy _)
          (fun decoded => some (decoded, flatState pc live init vals)) = _
        rw [hrt]; rfl
  | var x =>
    cases ht with
    | copy hluTy =>
      cases hu with
      | copy hfound hlive _ =>
        obtain ⟨_, hxLive, hxLoc, hxTy, hxIfLive, _⟩ := hinv.ctxAgree x _ hfound
        rw [tys_lookup, hfound] at hluTy
        have htyU8 : _ = Ty.scalar (.int .u8) := Option.some.inj hluTy
        rw [htyU8] at hxLoc hxIfLive
        obtain ⟨hxInit, v, hvVals, hvEnv, hrep⟩ := hxIfLive hlive
        cases hrep with
        | @u8 n hrange =>
          have hloc' : AList.lookup mirLocals ⟨x.id⟩ = some byteTy := by simpa [tyToEarly] using hxLoc
          have hld := sbm_loadValue_virtual hsbm pc live init vals hxLive hxInit hloc' hvVals
          have hov := sbm_ownerView hsbm pc live init vals ⟨x.id⟩ byteTy .owned
          refine ⟨n, hrange, ?_, ?_, flatInv_copy hinv x⟩
          · simp only [lowerAtom, toMemRead]
            unfold Memory.readValue
            dsimp only
            rw [hov]
            change (if !true then none else Option.bind
              ((mir.logicalView (language := sliceLanguage slice)).loadValue
                (flatState pc live init vals) (⟨⟨⟨x.id⟩, []⟩, .virtual⟩ : MemoryPlace EarlyTy byteTy))
              (fun v => some (v, flatState pc live init vals))) = _
            rw [hld]; rfl
          · simp [Slice0.Atom.toCore, evalAtom, readVar, hvEnv, specValOf, Val.isCopy]

private theorem sbm_readMove (hsbm : SingleBlockMir mir mirLocals block)
    (pc : Nat) (live init : List PlaceId) (vals : AList PlaceId EarlyValue)
    {p : StraightProgram} {pos : Nat} {Γ : Ctx} {env : Env}
    (hinv : FlatInv p mirLocals pos Γ env live init vals)
    {y : Var} {name : ConcreteSpec.Name} {Γ' : Ctx}
    (ht : HasType (List.map Prod.snd p.decls) Γ.tys (.atom (.move y)) (.struct name))
    (hu : UseAtom (List.map Prod.snd p.decls) Γ (.move y) Γ') :
    (mir.logicalView (language := sliceLanguage slice)).readValue
        (flatState (slice := slice) (profile := profile) pc live init vals)
        (.move (⟨⟨⟨y.id⟩, []⟩, .virtual⟩ : MemoryPlace EarlyTy (nominalTy name))) =
      some (.other (nominalId name),
        flatState pc live (init.erase ⟨y.id⟩) (AList.erase vals ⟨y.id⟩)) ∧
    readVar (List.map Prod.snd p.decls) env y true =
      .ok (.struct name [], AList.update env y (fun _ => .moved)) ∧
    FlatInv p mirLocals pos Γ' (AList.update env y (fun _ => .moved))
      live (init.erase ⟨y.id⟩) (AList.erase vals ⟨y.id⟩) := by
  cases ht with
  | move hluTy =>
    cases hu with
    | move hfound hlive hlin =>
      obtain ⟨_, hyLive, hyLoc, hyTy, hyIfLive, _⟩ := hinv.ctxAgree y _ hfound
      rw [tys_lookup, hfound] at hluTy
      have htyEq : _ = Ty.struct name := Option.some.inj hluTy
      rw [htyEq] at hyLoc hyIfLive hlin
      obtain ⟨hyInit, v, hvVals, hvEnv, hrep⟩ := hyIfLive hlive
      cases hrep with
      | nominal _ =>
        have hloc' : AList.lookup mirLocals ⟨y.id⟩ = some (nominalTy name) := by
          simpa [tyToEarly] using hyLoc
        have hld := sbm_loadValue_virtual hsbm pc live init vals hyLive hyInit hloc' hvVals
        have hov := sbm_ownerView hsbm pc live init vals ⟨y.id⟩ (nominalTy name) .owned
        have hvalNotCopy : (Val.struct name []).isCopy (List.map Prod.snd p.decls) = false := by
          unfold Val.isCopy
          unfold Ty.isCopy at hlin
          cases hf : findDecl (List.map Prod.snd p.decls) name <;> simp [hf] at hlin ⊢
          exact hlin
        refine ⟨?_, ?_, flatInv_move hinv y⟩
        · unfold Memory.readValue
          dsimp only
          rw [hov]
          dsimp only
          have hmv : Own.move ({ slots := [⟨⟨0⟩, 0,
              ⟨((sliceLanguage slice).scalarType (nominalTy name)).getD .unit,
               (sliceLanguage slice).operands.isCopy (nominalTy name), false⟩, .owned⟩] } : Own.State) ⟨0⟩ =
              some { slots := [⟨⟨0⟩, 0, ⟨.unit, false, false⟩, .moved⟩] } := rfl
          rw [hmv]
          dsimp only [Bind.bind, Option.bind]
          rw [hld]
          with_unfolding_all rfl
        · simp [readVar, hvEnv, specValOf, hvalNotCopy]

private theorem sbm_writeFresh_iff (hsbm : SingleBlockMir mir mirLocals block)
    (pc : Nat) (live init : List PlaceId) (vals : AList PlaceId EarlyValue)
    {r : PlaceId} {ty : EarlyTy} (v : EarlyValue)
    (hnotLive : r ∉ live) (hnotInit : r ∉ init)
    (hlu : AList.lookup mirLocals r = some ty)
    (stored : MemoryRegisters (sliceLanguage slice) profile) :
    (mir.logicalView (language := sliceLanguage slice)).ParamOutcome
        [⟨ty, ⟨⟨r, []⟩, .virtual⟩⟩] [v]
        (flatState (slice := slice) (profile := profile) pc live init vals) stored false ↔
      stored = flatState pc (r :: live) (r :: init) ((r, v) :: AList.erase vals r) := by
  let s := flatState (slice := slice) (profile := profile) pc live init vals
  let sLive := flatState (slice := slice) (profile := profile) pc (r :: live) init vals
  let sDone := flatState (slice := slice) (profile := profile) pc (r :: live) (r :: init)
    ((r, v) :: AList.erase vals r)
  have hcls := sbm_classAt hsbm hlu
  have hnotLiveB : live.contains r = false := by
    cases h : live.contains r
    · rfl
    · exact False.elim (hnotLive (List.contains_iff_mem.mp h))
  have hnotInitB : init.contains r = false := by
    cases h : init.contains r
    · rfl
    · exact False.elim (hnotInit (List.contains_iff_mem.mp h))
  have hbegIff : ∀ st, (mir.logicalView (language := sliceLanguage slice)).Begins s r st ↔ st = sLive := by
    intro st
    unfold Memory.Begins
    dsimp only [s, flatState]
    rw [hcls]
    constructor
    · rintro ⟨_, ⟨_, rfl⟩ | ⟨himp, _⟩⟩
      · rfl
      · cases himp
    · rintro rfl; exact ⟨hnotLive, .inl ⟨rfl, rfl⟩⟩
  have hwriteEq : (mir.logicalView (language := sliceLanguage slice)).writeValue sLive
      (⟨⟨r, []⟩, .virtual⟩ : MemoryPlace EarlyTy ty) v = some sDone := by
    unfold Memory.writeValue
    have hmay : (mir.logicalView (language := sliceLanguage slice)).mayInitialize sLive
        (⟨⟨r, []⟩, .virtual⟩ : MemoryPlace EarlyTy ty) = true := by
      unfold Memory.mayInitialize
      simp only [sLive, flatState, hnotInitB, Bool.not_false, ↓reduceIte, List.isEmpty_nil]
    have hdisc : (mir.logicalView (language := sliceLanguage slice)).discardRemainder sLive
        (⟨⟨r, []⟩, .virtual⟩ : MemoryPlace EarlyTy ty) true = false ∧
        (mir.logicalView (language := sliceLanguage slice)).discardRemainder sLive
        (⟨⟨r, []⟩, .virtual⟩ : MemoryPlace EarlyTy ty) false = false := by
      constructor <;> (unfold Memory.discardRemainder; with_unfolding_all rfl)
    have hauth : (mir.logicalView (language := sliceLanguage slice)).authorizeWrite sLive
        (⟨⟨r, []⟩, .virtual⟩ : MemoryPlace EarlyTy ty) =
        some { slots := [⟨⟨0⟩, 0, ⟨((sliceLanguage slice).scalarType ty).getD .unit,
            (sliceLanguage slice).operands.isCopy ty, false⟩, .owned⟩] } := by
      unfold Memory.authorizeWrite Memory.writeOwnerView
      rw [hdisc.1, hdisc.2]
      have hempty : (!sLive.initialized.contains r ||
          sLive.movedFields.any (fun p => p.1 == r && p.2.isPrefixOf []) || false) = true := by
        simp [sLive, flatState, hnotInit]
      dsimp only
      rw [hempty]
      simp only [↓reduceIte, Bool.false_and, Bool.false_eq_true]
      rw [sbm_ownerView hsbm pc (r :: live) init vals r ty .moved]
      rfl
    have hnotLiveF : (!sLive.live.contains r) = false := by simp [sLive, flatState]
    dsimp only
    rw [hnotLiveF, hmay, hauth]
    simp only [Bool.false_or, Bool.not_true, Bool.false_eq_true, ↓reduceIte, Bind.bind,
      Option.bind, Memory.markWritten, Memory.replaceLocal, sLive, sDone, flatState,
      List.isEmpty_nil, hnotInitB, List.filter_nil, Pure.pure, Option.some.injEq]
    with_unfolding_all rfl
  have hwritesIff : ∀ st, (mir.logicalView (language := sliceLanguage slice)).Writes sLive
      (⟨⟨r, []⟩, .virtual⟩ : MemoryPlace EarlyTy ty) v st ↔ st = sDone := by
    intro st
    unfold Memory.Writes
    constructor
    · rintro ⟨_, _, hw, _⟩
      rw [hwriteEq] at hw
      exact (Option.some.inj hw).symm
    · rintro rfl
      refine ⟨⟨sbm_validPlace hsbm pc (r :: live) init vals hlu,
        fun hinit => False.elim (hnotInit (by simpa [Memory.initializedAt, sLive, flatState] using hinit))⟩,
        (fun himp => by cases himp), hwriteEq, ⟨?_, (fun himp => by cases himp)⟩⟩
      exact sbm_loadValue_virtual hsbm pc (r :: live) (r :: init) ((r, v) :: AList.erase vals r)
        List.mem_cons_self List.mem_cons_self hlu (by simp [AList.lookup])
  unfold Memory.ParamOutcome
  simp only [Bool.false_eq_true, ↓reduceIte]
  constructor
  · intro hwp
    cases hwp with
    | cons hst hwr htail =>
      simp only [flatState, hnotLiveB, Bool.false_eq_true, ↓reduceIte] at hst
      have hsEq := (hbegIff _).mp hst
      subst hsEq
      have hwEq := (hwritesIff _).mp hwr
      subst hwEq
      have hnil : ∀ s1 s2, (mir.logicalView (language := sliceLanguage slice)).WriteParams [] [] s1 s2 → s2 = s1 := by
        intro s1 s2 hw; cases hw; rfl
      exact hnil _ _ htail
  · rintro rfl
    refine Memory.WriteParams.cons ?_ ((hwritesIff sDone).mpr rfl) (.nil sDone)
    simp only [flatState, hnotLiveB, Bool.false_eq_true, ↓reduceIte]
    exact (hbegIff sLive).mpr rfl

private theorem sbm_exec_step_of_raw (_hsbm : SingleBlockMir mir mirLocals block)
    {pc : Nat} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    {next : MemoryRegisters (sliceLanguage slice) profile}
    (htermNone : (mir.logicalView (language := sliceLanguage slice)).terminalObservation
      (flatState (slice := slice) (profile := profile) pc live init vals) = none)
    (hstep : MIR.Step mir (flatState pc live init vals) next) :
    (MIR.Execution (language := sliceLanguage slice)).step mir (flatState pc live init vals) next := by
  with_unfolding_all
    refine ⟨?_, hstep, ?_⟩
    · change Option.bind ((mir.logicalView (language := sliceLanguage slice)).terminalObservation _) _ = none
      rw [htermNone]; rfl
    · change TracePrefix ([] ++ Option.getD (Option.map _ (Option.bind (Memory.terminalObservation _ _) _)) []) _
      rw [htermNone]
      exact ⟨_, rfl⟩

private theorem sbm_exec_step_to_raw
    {s next : MemoryRegisters (sliceLanguage slice) profile}
    (h : (MIR.Execution (language := sliceLanguage slice)).step mir s next) :
    MIR.Step mir s next := by
  with_unfolding_all exact h.2.1

private theorem sbm_applyOperation_id
    (pc : Nat) (live init : List PlaceId) (vals : AList PlaceId EarlyValue) :
    Memory.applyOperation (flatState (slice := slice) (profile := profile) pc live init vals) true
      (Memory.operationStore (flatState (slice := slice) (profile := profile) pc live init vals) true) =
      flatState pc live init vals := by
  have hmap : List.map (fun (p : PlaceId × (sliceLanguage slice).operands.Val) => (p.1, p.2)) vals = vals := by
    induction vals with
    | nil => rfl
    | cons x xs ih => exact congrArg (List.cons x) ih
  unfold Memory.applyOperation Memory.operationStore
  dsimp only [flatState, ↓reduceIte]
  exact congrArg (fun v => flatState (slice := slice) (profile := profile) pc live init v) hmap

private theorem sbm_instAt (hsbm : SingleBlockMir mir mirLocals block)
    {pc : Nat} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    {op : LocatedOp (MIRInst (sliceLoweringLanguage slice))}
    (hget : block.body[pc]? = some op) :
    ((AList.lookup mir.functions (flatState (slice := slice) (profile := profile) pc live init vals).frame).bind
      fun (_, g) => (g.blocks.find? (·.id == (flatState (slice := slice) (profile := profile) pc live init vals).block)).bind
        fun b => b.body[(flatState (slice := slice) (profile := profile) pc live init vals).instruction]?) =
      some op := by
  rw [hsbm.fnsEq]
  dsimp only [flatState, AList.lookup]
  simp only [↓reduceIte]
  have hfind : #[block].find? (·.id == ⟨0⟩) = some block := by
    rcases block with ⟨bid, bp, bb, be, bnd⟩
    have hbid : bid = ⟨0⟩ := hsbm.blkId; subst hbid; rfl
  dsimp only [Option.bind]
  rw [hfind]
  exact hget

private theorem sbm_blockAt_find (hsbm : SingleBlockMir mir mirLocals block) :
    #[block].find? (·.id == ⟨0⟩) = some block := by
  rcases block with ⟨bid, bp, bb, be, bnd⟩
  have hbid : bid = ⟨0⟩ := hsbm.blkId; subst hbid; rfl

private theorem sbm_step_bind_det (hsbm : SingleBlockMir mir mirLocals block)
    {pc : Nat} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    {origin : Origin} {ty : EarlyTy} {dst : PlaceId}
    {src : Read EarlyTy (Literal EarlyTy) ty}
    {v : EarlyValue} {init1 : List PlaceId} {vals1 : AList PlaceId EarlyValue}
    (hget : block.body[pc]? = some ⟨origin, .bind ⟨dst, []⟩ src⟩)
    (hread : (mir.logicalView (language := sliceLanguage slice)).readValue
      (flatState (slice := slice) (profile := profile) pc live init vals) (toMemRead src) =
      some (v, flatState pc live init1 vals1))
    (hnotLive : dst ∉ live) (hnotInit1 : dst ∉ init1)
    (hlu : AList.lookup mirLocals dst = some ty) :
    let next : MemoryRegisters (sliceLanguage slice) profile :=
      flatState (pc + 1) (dst :: live) (dst :: init1) ((dst, v) :: AList.erase vals1 dst)
    (MIR.Execution (language := sliceLanguage slice)).step mir (flatState pc live init vals) next ∧
    ∀ d, (MIR.Execution (language := sliceLanguage slice)).step mir (flatState pc live init vals) d → d = next := by
  let s := flatState (slice := slice) (profile := profile) pc live init vals
  let sAfter := flatState (slice := slice) (profile := profile) pc live init1 vals1
  let sStored := flatState (slice := slice) (profile := profile) pc (dst :: live) (dst :: init1)
    ((dst, v) :: AList.erase vals1 dst)
  let next := flatState (slice := slice) (profile := profile) (pc + 1) (dst :: live) (dst :: init1)
    ((dst, v) :: AList.erase vals1 dst)
  have hstepRaw : ∀ d, MIR.Step mir s d ↔
      ((mir.logicalView (language := sliceLanguage slice)).instructionTrap s
          (.bind ⟨⟨dst, []⟩, .virtual⟩ (toMemRead src)) = none ∧
       (mir.logicalView (language := sliceLanguage slice)).InstructionStep s d true
          (.bind ⟨⟨dst, []⟩, .virtual⟩ (toMemRead src))) := fun _ =>
    mir_step_of_bind (sbm_instAt (pc := pc) (live := live) (init := init) (vals := vals) hsbm hget)
  have hstepIff : ∀ d, MIR.Step mir s d ↔ d = next := by
    intro d
    rw [hstepRaw d]
    simp only [Memory.instructionTrap, Memory.InstructionStep, Memory.InstructionStepWith,
      Bool.true_or, ↓reduceIte, true_and]
    constructor
    · rintro ⟨val', after', stored', hrd, hpo, rfl⟩
      rw [hread] at hrd
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hrd)
      have hsEq := (sbm_writeFresh_iff hsbm pc live init1 vals1 v hnotLive hnotInit1 hlu stored').mp hpo
      subst hsEq; rfl
    · rintro rfl
      refine ⟨v, sAfter, sStored, hread,
        (sbm_writeFresh_iff hsbm pc live init1 vals1 v hnotLive hnotInit1 hlu sStored).mpr rfl, rfl⟩
  have htermNone : (mir.logicalView (language := sliceLanguage slice)).terminalObservation s = none := by
    have hfind := sbm_blockAt_find hsbm
    unfold Memory.terminalObservation Memory.blockAt Memory.graphAt MIR.logicalView
    rw [hsbm.fnsEq]
    dsimp only [s, flatState, AList.map, List.map, AList.lookup]
    simp only [Bool.false_eq_true, ↓reduceIte, Option.map_some, Bind.bind, Option.bind]
    rw [array_find?_map]
    dsimp only
    rw [hfind]
    simp only [Option.map_some, List.getElem?_map, hget]
    cases src <;> (with_unfolding_all rfl)
  exact ⟨sbm_exec_step_of_raw hsbm htermNone ((hstepIff next).mpr rfl),
    fun d hd => (hstepIff d).mp (sbm_exec_step_to_raw hd)⟩

private theorem sbm_step_add_ok_det (hsbm : SingleBlockMir mir mirLocals block)
    {pc : Nat} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    {origin : Origin} {dst : PlaceId} {l r : Slice0.Atom} {chk : OperationCheck}
    {a b : Int}
    (hget : block.body[pc]? = some ⟨origin, .op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨dst, []⟩ ()
      (.cons (lowerAtom l) (.cons (lowerAtom r) .nil)) [chk] []⟩)
    (hreadL : (mir.logicalView (language := sliceLanguage slice)).readValue
      (flatState (slice := slice) (profile := profile) pc live init vals) (toMemRead (lowerAtom l)) =
      some (.scalar (.int .u8 a), flatState pc live init vals))
    (hreadR : (mir.logicalView (language := sliceLanguage slice)).readValue
      (flatState (slice := slice) (profile := profile) pc live init vals) (toMemRead (lowerAtom r)) =
      some (.scalar (.int .u8 b), flatState pc live init vals))
    (hfits : Core.IntTy.u8.InRange (a + b))
    (hnotLive : dst ∉ live) (hnotInit : dst ∉ init)
    (hlu : AList.lookup mirLocals dst = some byteTy) :
    let sum : EarlyValue := .scalar (.int .u8 (a + b))
    let next : MemoryRegisters (sliceLanguage slice) profile :=
      flatState (pc + 1) (dst :: live) (dst :: init) ((dst, sum) :: AList.erase vals dst)
    (MIR.Execution (language := sliceLanguage slice)).step mir (flatState pc live init vals) next ∧
    ∀ d, (MIR.Execution (language := sliceLanguage slice)).step mir (flatState pc live init vals) d → d = next := by
  let s := flatState (slice := slice) (profile := profile) pc live init vals
  let sum : EarlyValue := .scalar (.int .u8 (a + b))
  let sStored := flatState (slice := slice) (profile := profile) pc (dst :: live) (dst :: init)
    ((dst, sum) :: AList.erase vals dst)
  let next := flatState (slice := slice) (profile := profile) (pc + 1) (dst :: live) (dst :: init)
    ((dst, sum) :: AList.erase vals dst)
  have hreadArgs : (mir.logicalView (language := sliceLanguage slice)).readArgs
      (.cons (toMemRead (lowerAtom l)) (.cons (toMemRead (lowerAtom r)) .nil)) s =
      some ([.scalar (.int .u8 a), .scalar (.int .u8 b)], s) := by
    dsimp only [Memory.readArgs, s]
    rw [hreadL]; dsimp only [Bind.bind, Option.bind]
    rw [hreadR]; rfl
  have hnoOv : decide (checked .u8 .add a b = .error .overflow) = false :=
    (checked_add_no_overflow_iff .u8 a b).mpr hfits
  have hnoTrap : Memory.firstTrap (language := sliceLanguage slice) (.operator (.arithmetic (.plain .add)))
      [.scalar (.int .u8 a), .scalar (.int .u8 b)] [chk] = none := by
    unfold Memory.firstTrap
    cases hdisp : chk.disposition <;> cases hdisc : chk.discriminator <;>
      simp [List.find?_nil, sliceLanguage, hdisp, hdisc, hnoOv]
  have hplain : plain .u8 .add a b = .ok (a + b) := by
    simp [plain, checked, inRangeOr, hfits]
  have happlyId := sbm_applyOperation_id (slice := slice) (profile := profile) pc live init vals
  have hstepRaw : ∀ d, MIR.Step mir s d ↔
      ((mir.logicalView (language := sliceLanguage slice)).instructionTrap s
          (.op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨⟨dst, []⟩, .virtual⟩ ()
            (.cons (toMemRead (lowerAtom l)) (.cons (toMemRead (lowerAtom r)) .nil)) [chk] []) = none ∧
       (mir.logicalView (language := sliceLanguage slice)).InstructionStep s d true
          (.op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨⟨dst, []⟩, .virtual⟩ ()
            (.cons (toMemRead (lowerAtom l)) (.cons (toMemRead (lowerAtom r)) .nil)) [chk] [])) := fun _ =>
    mir_step_of_op2 (sbm_instAt (pc := pc) (live := live) (init := init) (vals := vals) hsbm hget)
  have hstepIff : ∀ d, MIR.Step mir s d ↔ d = next := by
    intro d
    rw [hstepRaw d]
    simp only [Memory.instructionTrap, hreadArgs, Bind.bind, Option.bind, hnoTrap, true_and,
      Memory.InstructionStep, Memory.InstructionStepWith, Bool.true_or, ↓reduceIte]
    constructor
    · rintro ⟨vs, afterR, ops, res, effs, chg, stored, hargs, _, hops, hopSem, _, hpo, rfl⟩
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hargs.symm)
      obtain ⟨_, hopOk, rfl, hstoreEq⟩ := hopSem
      subst hstoreEq
      have hresEq : res = sum := by
        with_unfolding_all
          change (if Core.IntTy.u8 = Core.IntTy.u8 ∧ Core.IntTy.u8 = Core.IntTy.u8 ∧
              Core.IntTy.u8 = Core.IntTy.u8 ∧ Core.IntTy.u8 = Core.IntTy.u8 then
            (plain .u8 .add a b).map (CoreValue.scalar ∘ CVal.int .u8) else Res.stuck) = Res.ok res at hopOk
        simp only [and_self, ↓reduceIte, hplain, Res.map, Res.bind, Function.comp_apply] at hopOk
        exact (Res.ok.inj hopOk).symm
      subst hresEq
      have hprep : Memory.prepareAddresses s [] = s := rfl
      rw [hprep, happlyId] at hpo
      have hsEq := (sbm_writeFresh_iff hsbm pc live init vals sum hnotLive hnotInit hlu stored).mp hpo
      subst hsEq; rfl
    · rintro rfl
      refine ⟨[.scalar (.int .u8 a), .scalar (.int .u8 b)], s, [], sum, [],
        Memory.operationStore s true, sStored, rfl, hnoTrap, rfl,
        ⟨rfl, ?_, rfl, rfl⟩, rfl, ?_, rfl⟩
      · with_unfolding_all
          change (if Core.IntTy.u8 = Core.IntTy.u8 ∧ Core.IntTy.u8 = Core.IntTy.u8 ∧
              Core.IntTy.u8 = Core.IntTy.u8 ∧ Core.IntTy.u8 = Core.IntTy.u8 then
            (plain .u8 .add a b).map (CoreValue.scalar ∘ CVal.int .u8) else Res.stuck) = Res.ok sum
        simp only [and_self, ↓reduceIte, hplain, Res.map, Res.bind, Function.comp_apply, sum]
      · have hprep : Memory.prepareAddresses s [] = s := rfl
        rw [hprep, happlyId]
        exact (sbm_writeFresh_iff hsbm pc live init vals sum hnotLive hnotInit hlu sStored).mpr rfl
  have htermNone : (mir.logicalView (language := sliceLanguage slice)).terminalObservation s = none := by
    have hinst : (mir.logicalView (language := sliceLanguage slice)).instructionTrap s
        (.op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨⟨dst, []⟩, .virtual⟩ ()
          (.cons (toMemRead (lowerAtom l)) (.cons (toMemRead (lowerAtom r)) .nil)) [chk] []) = none := by
      simp [Memory.instructionTrap, hreadArgs, hnoTrap]
    have hfind := sbm_blockAt_find hsbm
    have hstepTerm : (mir.logicalView (language := sliceLanguage slice)).terminalObservation s =
        ((mir.logicalView (language := sliceLanguage slice)).instructionTrap s
          (.op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨⟨dst, []⟩, .virtual⟩ ()
            (.cons (toMemRead (lowerAtom l)) (.cons (toMemRead (lowerAtom r)) .nil)) [chk] [])).bind
          (fun trap => Option.bind ((sliceLanguage slice).hostedResult (.trapped trap origin.operation))
            (fun r => match r with
              | .trapped status kind _ => some (.trapped status kind origin.lexical
                  (trapDiagnostic mir.project kind origin.operation origin.lexical))
              | _ => none)) := by
      unfold Memory.terminalObservation Memory.blockAt Memory.graphAt MIR.logicalView
      rw [hsbm.fnsEq]
      dsimp only [s, flatState, AList.map, List.map, AList.lookup]
      simp only [Bool.false_eq_true, ↓reduceIte, Option.map_some, Bind.bind, Option.bind]
      rw [array_find?_map]
      dsimp only
      rw [hfind]
      simp only [Option.map_some, List.getElem?_map, hget]
      cases l <;> cases r <;> (with_unfolding_all rfl)
    rw [hstepTerm, hinst]; rfl
  exact ⟨sbm_exec_step_of_raw hsbm htermNone ((hstepIff next).mpr rfl),
    fun d hd => (hstepIff d).mp (sbm_exec_step_to_raw hd)⟩

private theorem sbm_step_construct_det (hsbm : SingleBlockMir mir mirLocals block)
    {pc : Nat} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    {origin : Origin} {dst : PlaceId} {name : ConcreteSpec.Name}
    (hget : block.body[pc]? = some ⟨origin, .op (opcode := .named ⟨"constructEmpty"⟩)
      (ty := nominalTy name) ⟨dst, []⟩ () .nil [] []⟩)
    (hemp : mir.declarations.emptyLinear (nominalId name) = true)
    (hnotLive : dst ∉ live) (hnotInit : dst ∉ init)
    (hlu : AList.lookup mirLocals dst = some (nominalTy name)) :
    let tok : EarlyValue := .other (nominalId name)
    let next : MemoryRegisters (sliceLanguage slice) profile :=
      flatState (pc + 1) (dst :: live) (dst :: init) ((dst, tok) :: AList.erase vals dst)
    (MIR.Execution (language := sliceLanguage slice)).step mir (flatState pc live init vals) next ∧
    ∀ d, (MIR.Execution (language := sliceLanguage slice)).step mir (flatState pc live init vals) d → d = next := by
  let s := flatState (slice := slice) (profile := profile) pc live init vals
  let tok : EarlyValue := .other (nominalId name)
  let sStored := flatState (slice := slice) (profile := profile) pc (dst :: live) (dst :: init)
    ((dst, tok) :: AList.erase vals dst)
  let next := flatState (slice := slice) (profile := profile) (pc + 1) (dst :: live) (dst :: init)
    ((dst, tok) :: AList.erase vals dst)
  have happlyId := sbm_applyOperation_id (slice := slice) (profile := profile) pc live init vals
  have hstepRaw : ∀ d, MIR.Step mir s d ↔
      ((mir.logicalView (language := sliceLanguage slice)).instructionTrap s
          (.op (opcode := .named ⟨"constructEmpty"⟩) (ty := nominalTy name) ⟨⟨dst, []⟩, .virtual⟩ () .nil [] []) = none ∧
       (mir.logicalView (language := sliceLanguage slice)).InstructionStep s d true
          (.op (opcode := .named ⟨"constructEmpty"⟩) (ty := nominalTy name) ⟨⟨dst, []⟩, .virtual⟩ () .nil [] [])) := fun _ =>
    mir_step_of_op0 (sbm_instAt (pc := pc) (live := live) (init := init) (vals := vals) hsbm hget)
  have hstepIff : ∀ d, MIR.Step mir s d ↔ d = next := by
    intro d
    rw [hstepRaw d]
    simp only [Memory.instructionTrap, Memory.readArgs, Memory.firstTrap, List.find?_nil,
      Option.map_none, Bind.bind, Option.bind, true_and, Memory.InstructionStep,
      Memory.InstructionStepWith, Bool.true_or, ↓reduceIte]
    constructor
    · rintro ⟨vs, afterR, ops, res, effs, chg, stored, hargs, _, hopSem, _, hpo, rfl⟩
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj hargs.symm)
      obtain ⟨_, hopOk, rfl, hstoreEq⟩ := hopSem
      subst hstoreEq
      have hresEq : res = tok := by
        with_unfolding_all
          change (if mir.declarations.emptyLinear (nominalId name) then Res.ok tok else Res.stuck) = Res.ok res at hopOk
        simp only [hemp, ↓reduceIte] at hopOk
        exact (Res.ok.inj hopOk).symm
      subst hresEq
      have hprep : Memory.prepareAddresses s [] = s := rfl
      rw [hprep, happlyId] at hpo
      have hsEq := (sbm_writeFresh_iff hsbm pc live init vals tok hnotLive hnotInit hlu stored).mp hpo
      subst hsEq; rfl
    · rintro rfl
      refine ⟨[], s, [], tok, [], Memory.operationStore s true, sStored, rfl, rfl,
        ⟨rfl, ?_, rfl, rfl⟩, rfl, ?_, rfl⟩
      · with_unfolding_all
          change (if mir.declarations.emptyLinear (nominalId name) then Res.ok tok else Res.stuck) = Res.ok tok
        simp only [hemp, ↓reduceIte]
      · have hprep : Memory.prepareAddresses s [] = s := rfl
        rw [hprep, happlyId]
        exact (sbm_writeFresh_iff hsbm pc live init vals tok hnotLive hnotInit hlu sStored).mpr rfl
  have htermNone : (mir.logicalView (language := sliceLanguage slice)).terminalObservation s = none := by
    have hfind := sbm_blockAt_find hsbm
    unfold Memory.terminalObservation Memory.blockAt Memory.graphAt MIR.logicalView
    rw [hsbm.fnsEq]
    dsimp only [s, flatState, AList.map, List.map, AList.lookup]
    simp only [Bool.false_eq_true, ↓reduceIte, Option.map_some, Bind.bind, Option.bind]
    rw [array_find?_map]
    dsimp only
    rw [hfind]
    simp only [Option.map_some, List.getElem?_map, hget]
    with_unfolding_all rfl
  exact ⟨sbm_exec_step_of_raw hsbm htermNone ((hstepIff next).mpr rfl),
    fun d hd => (hstepIff d).mp (sbm_exec_step_to_raw hd)⟩

private theorem sbm_step_destructure_det (hsbm : SingleBlockMir mir mirLocals block)
    {pc : Nat} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    {origin : Origin} {name : ConcreteSpec.Name} {y : Var}
    (hget : block.body[pc]? = some ⟨origin, .destructure (ty := nominalTy name) ⟨⟨y.id⟩, []⟩ []⟩)
    (hemp : mir.declarations.emptyLinear (nominalId name) = true)
    (hread : (mir.logicalView (language := sliceLanguage slice)).readValue
      (flatState (slice := slice) (profile := profile) pc live init vals)
      (.move (⟨⟨⟨y.id⟩, []⟩, .virtual⟩ : MemoryPlace EarlyTy (nominalTy name))) =
      some (.other (nominalId name),
        flatState pc live (init.erase ⟨y.id⟩) (AList.erase vals ⟨y.id⟩))) :
    let next : MemoryRegisters (sliceLanguage slice) profile :=
      flatState (pc + 1) live (init.erase ⟨y.id⟩) (AList.erase vals ⟨y.id⟩)
    (MIR.Execution (language := sliceLanguage slice)).step mir (flatState pc live init vals) next ∧
    ∀ d, (MIR.Execution (language := sliceLanguage slice)).step mir (flatState pc live init vals) d → d = next := by
  let s := flatState (slice := slice) (profile := profile) pc live init vals
  let sAfter := flatState (slice := slice) (profile := profile) pc live (init.erase ⟨y.id⟩) (AList.erase vals ⟨y.id⟩)
  let next := flatState (slice := slice) (profile := profile) (pc + 1) live (init.erase ⟨y.id⟩) (AList.erase vals ⟨y.id⟩)
  have hstepRaw : ∀ d, MIR.Step mir s d ↔
      ((mir.logicalView (language := sliceLanguage slice)).instructionTrap s
          (.destructure (ty := nominalTy name) ⟨⟨⟨y.id⟩, []⟩, .virtual⟩ []) = none ∧
       (mir.logicalView (language := sliceLanguage slice)).InstructionStep s d true
          (.destructure (ty := nominalTy name) ⟨⟨⟨y.id⟩, []⟩, .virtual⟩ [])) := fun _ =>
    mir_step_of_destructure0 (sbm_instAt (pc := pc) (live := live) (init := init) (vals := vals) hsbm hget)
  have hstepIff : ∀ d, MIR.Step mir s d ↔ d = next := by
    intro d
    rw [hstepRaw d]
    simp only [Memory.instructionTrap, Memory.InstructionStep, Memory.InstructionStepWith,
      true_and, List.map_nil, List.mapM_nil, Pure.pure, Option.some.injEq]
    have hisCopyF : (sliceLanguage slice).operands.isCopy (nominalTy name) = false := rfl
    simp only [hisCopyF, Bool.false_eq_true, ↓reduceIte]
    constructor
    · rintro ⟨val', after', fields', stored', hrd, _, rfl, hpo, rfl⟩
      have heq := hread.symm.trans hrd
      obtain ⟨rfl, rfl⟩ := Prod.mk.inj (Option.some.inj heq)
      unfold Memory.ParamOutcome at hpo
      simp only [Bool.false_eq_true, ↓reduceIte] at hpo
      have hnil : ∀ s1 s2, (mir.logicalView (language := sliceLanguage slice)).WriteParams [] [] s1 s2 → s2 = s1 := by
        intro s1 s2 hw; cases hw; rfl
      rw [hnil _ _ hpo]; rfl
    · rintro rfl
      refine ⟨.other (nominalId name), sAfter, [], sAfter, hread, ?_, rfl, .nil sAfter, rfl⟩
      refine ⟨.nil, [], ?_, (fun _ hm => nomatch hm), (fun _ hm => nomatch hm)⟩
      simp [sliceLanguage, MIR.logicalView, hemp]
  have htermNone : (mir.logicalView (language := sliceLanguage slice)).terminalObservation s = none := by
    have hfind := sbm_blockAt_find hsbm
    unfold Memory.terminalObservation Memory.blockAt Memory.graphAt MIR.logicalView
    rw [hsbm.fnsEq]
    dsimp only [s, flatState, AList.map, List.map, AList.lookup]
    simp only [Bool.false_eq_true, ↓reduceIte, Option.map_some, Bind.bind, Option.bind]
    rw [array_find?_map]
    dsimp only
    rw [hfind]
    simp only [Option.map_some, List.getElem?_map, hget]
    with_unfolding_all rfl
  exact ⟨sbm_exec_step_of_raw hsbm htermNone ((hstepIff next).mpr rfl),
    fun d hd => (hstepIff d).mp (sbm_exec_step_to_raw hd)⟩

private theorem sbm_result_add_overflow (hsbm : SingleBlockMir mir mirLocals block)
    {pc : Nat} {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    {origin : Origin} {dst : PlaceId} {l r : Slice0.Atom} {a b : Int}
    {resolved : DiagnosticSite}
    (hget : block.body[pc]? = some ⟨origin, .op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨dst, []⟩ ()
      (.cons (lowerAtom l) (.cons (lowerAtom r) .nil)) [⟨0, .overflow, .residual⟩] []⟩)
    (hreadL : (mir.logicalView (language := sliceLanguage slice)).readValue
      (flatState (slice := slice) (profile := profile) pc live init vals) (toMemRead (lowerAtom l)) =
      some (.scalar (.int .u8 a), flatState pc live init vals))
    (hreadR : (mir.logicalView (language := sliceLanguage slice)).readValue
      (flatState (slice := slice) (profile := profile) pc live init vals) (toMemRead (lowerAtom r)) =
      some (.scalar (.int .u8 b), flatState pc live init vals))
    (hov : ¬ Core.IntTy.u8.InRange (a + b))
    (hsite : runtimeSite mir.project origin = some resolved) :
    (MIR.Execution (language := sliceLanguage slice)).result mir (flatState pc live init vals)
      (.trapped 134 .overflow resolved (trapDiagnostic mir.project .overflow origin.operation origin.lexical)) := by
  let s := flatState (slice := slice) (profile := profile) pc live init vals
  have hreadArgs : (mir.logicalView (language := sliceLanguage slice)).readArgs
      (.cons (toMemRead (lowerAtom l)) (.cons (toMemRead (lowerAtom r)) .nil)) s =
      some ([.scalar (.int .u8 a), .scalar (.int .u8 b)], s) := by
    dsimp only [Memory.readArgs, s]
    rw [hreadL]; dsimp only [Bind.bind, Option.bind]
    rw [hreadR]; rfl
  have hovB : decide (checked .u8 .add a b = .error .overflow) = true :=
    (checked_add_overflow_iff .u8 a b).mpr hov
  have htrap : Memory.firstTrap (language := sliceLanguage slice) (.operator (.arithmetic (.plain .add)))
      [.scalar (.int .u8 a), .scalar (.int .u8 b)] [⟨0, .overflow, .residual⟩] = some .overflow := by
    unfold Memory.firstTrap
    simp [sliceLanguage, hovB]
  have hinstTrap : (mir.logicalView (language := sliceLanguage slice)).instructionTrap s
      (.op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨⟨dst, []⟩, .virtual⟩ ()
        (.cons (toMemRead (lowerAtom l)) (.cons (toMemRead (lowerAtom r)) .nil))
        [⟨0, .overflow, .residual⟩] []) = some .overflow := by
    simp [Memory.instructionTrap, hreadArgs, htrap]
  have hfind := sbm_blockAt_find hsbm
  have hterm : (mir.logicalView (language := sliceLanguage slice)).terminalObservation s =
      some (.trapped 134 .overflow origin.lexical
        (trapDiagnostic mir.project .overflow origin.operation origin.lexical)) := by
    have hstepTerm : (mir.logicalView (language := sliceLanguage slice)).terminalObservation s =
        ((mir.logicalView (language := sliceLanguage slice)).instructionTrap s
          (.op (opcode := .arithmetic (.plain .add)) (ty := byteTy) ⟨⟨dst, []⟩, .virtual⟩ ()
            (.cons (toMemRead (lowerAtom l)) (.cons (toMemRead (lowerAtom r)) .nil))
            [⟨0, .overflow, .residual⟩] [])).bind
          (fun trap => Option.bind ((sliceLanguage slice).hostedResult (.trapped trap origin.operation))
            (fun r => match r with
              | .trapped status kind _ => some (.trapped status kind origin.lexical
                  (trapDiagnostic mir.project kind origin.operation origin.lexical))
              | _ => none)) := by
      unfold Memory.terminalObservation Memory.blockAt Memory.graphAt MIR.logicalView
      rw [hsbm.fnsEq]
      dsimp only [s, flatState, AList.map, List.map, AList.lookup]
      simp only [Bool.false_eq_true, ↓reduceIte, Option.map_some, Bind.bind, Option.bind]
      rw [array_find?_map]
      dsimp only
      rw [hfind]
      simp only [Option.map_some, List.getElem?_map, hget]
      cases l <;> cases r <;> (with_unfolding_all rfl)
    rw [hstepTerm, hinstTrap]; rfl
  with_unfolding_all
    change Option.bind ((mir.logicalView (language := sliceLanguage slice)).terminalObservation s) _ = _
    rw [hterm]
    dsimp only [Option.bind]
    unfold Memory.blockAt Memory.graphAt MIR.logicalView
    rw [hsbm.fnsEq]
    dsimp only [s, flatState, AList.map, List.map, AList.lookup]
    simp only [↓reduceIte, Option.map_some, Bind.bind, Option.bind]
    rw [array_find?_map]
    dsimp only
    rw [hfind]
    simp only [Option.map_some, List.getElem?_map, hget, hsite, Pure.pure]

private theorem sbm_result_abort (hsbm : SingleBlockMir mir mirLocals block)
    {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    {origin : Origin} {resolved : DiagnosticSite}
    (hend : block.ending = .trap .abort origin)
    (hsite : runtimeSite mir.project origin = some resolved) :
    (MIR.Execution (language := sliceLanguage slice)).result mir (flatState block.body.length live init vals)
      (.trapped 134 .abort resolved (trapDiagnostic mir.project .abort origin.operation origin.lexical)) := by
  let s := flatState (slice := slice) (profile := profile) block.body.length live init vals
  have hfind := sbm_blockAt_find hsbm
  have hnone : block.body[block.body.length]? = none := List.getElem?_eq_none (Nat.le_refl _)
  have hterm : (mir.logicalView (language := sliceLanguage slice)).terminalObservation s =
      some (.trapped 134 .abort origin.lexical
        (trapDiagnostic mir.project .abort origin.operation origin.lexical)) := by
    unfold Memory.terminalObservation Memory.blockAt Memory.graphAt MIR.logicalView
    rw [hsbm.fnsEq]
    dsimp only [s, flatState, AList.map, List.map, AList.lookup]
    simp only [Bool.false_eq_true, ↓reduceIte, Option.map_some, Bind.bind, Option.bind]
    rw [array_find?_map]
    dsimp only
    rw [hfind]
    simp only [Option.map_some, List.getElem?_map, hnone, List.length_map, bne_self_eq_false,
      Bool.false_eq_true, ↓reduceIte, hend]
    with_unfolding_all rfl
  with_unfolding_all
    change Option.bind ((mir.logicalView (language := sliceLanguage slice)).terminalObservation s) _ = _
    rw [hterm]
    dsimp only [Option.bind]
    unfold Memory.blockAt Memory.graphAt MIR.logicalView
    rw [hsbm.fnsEq]
    dsimp only [s, flatState, AList.map, List.map, AList.lookup]
    simp only [↓reduceIte, Option.map_some, Bind.bind, Option.bind]
    rw [array_find?_map]
    dsimp only
    rw [hfind]
    simp only [Option.map_some, List.getElem?_map, hnone, Option.map_none, hend]
    with_unfolding_all
      change Option.bind (runtimeSite mir.project origin) _ = _
      rw [hsite]; rfl

private theorem sbm_endOwnerView_eq (hsbm : SingleBlockMir mir mirLocals block)
    (pc : Nat) (live init : List PlaceId) (vals : AList PlaceId EarlyValue)
    {r : PlaceId} {ty : (sliceLanguage slice).operands.Ty} (hlu : AList.lookup mirLocals r = some ty) :
    (mir.logicalView (language := sliceLanguage slice)).endOwnerView
      (flatState (slice := slice) (profile := profile) pc live init vals) r =
      some ({ slots := [⟨⟨0⟩, 0, ⟨((sliceLanguage slice).scalarType ty).getD .unit,
        (sliceLanguage slice).operands.isCopy ty, false⟩,
        if init.contains r then .owned else .moved⟩] }, ⟨0⟩) := by
  have hrt : (mir.logicalView (language := sliceLanguage slice)).rootType earlyEntry r = some ty :=
    (sbm_rootType hsbm r).trans hlu
  unfold Memory.endOwnerView
  dsimp only [flatState]
  rw [hrt, Option.map_some]
  cases hci : init.contains r <;>
    simp only [List.filterMap_nil, List.isEmpty_nil, Bool.not_true, Bool.false_and,
      Bool.or_false, Bool.not_false, Bool.false_eq_true, ↓reduceIte]
  · exact congrArg some (sbm_ownerView hsbm pc live init vals r ty .moved)
  · exact congrArg some (sbm_ownerView hsbm pc live init vals r ty .owned)

private theorem sbm_canEnd_all (hsbm : SingleBlockMir mir mirLocals block)
    {p : StraightProgram} {pos : Nat} {Γ : Ctx} {env : Env}
    {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    (hinv : FlatInv p mirLocals pos Γ env live init vals)
    (hdisch : Discharged (List.map Prod.snd p.decls) Γ) (pc : Nat) :
    live.all ((mir.logicalView (language := sliceLanguage slice)).canEnd
      (flatState (slice := slice) (profile := profile) pc live init vals)) = true := by
  apply List.all_eq_true.mpr
  intro r hr
  rw [Memory.canEnd_iff]
  by_cases hlt : r.id < p.freshBase
  · have hsome := hinv.liveLtInCtx r hr hlt
    cases hlu : AList.lookup Γ ⟨r.id⟩ with
    | none => rw [hlu] at hsome; cases hsome
    | some e =>
      obtain ⟨_, _, hrLoc, htyInfo, _, hifFalse⟩ := hinv.ctxAgree ⟨r.id⟩ e hlu
      have hrEq : (⟨r.id⟩ : PlaceId) = r := by cases r; rfl
      rw [hrEq] at hrLoc hifFalse
      have hdischEntry := hdisch (⟨r.id⟩, e) (alist_lookup_mem hlu)
      rcases htyInfo with htyU8 | ⟨name, htyNom, hemp⟩
      · -- u8: Copy, always droppable
        have hrLoc' : AList.lookup mirLocals r = some byteTy := by simpa [tyToEarly, htyU8] using hrLoc
        refine ⟨_, sbm_endOwnerView_eq hsbm pc live init vals hrLoc', ?_⟩
        cases init.contains r <;> simp [Own.canDropPlaces, sliceLanguage, byteTy]
      · -- nominal: linear, must be moved (`e.live = false` → `r ∉ init`)
        have hnotCopy : (Ty.struct name).isCopy (List.map Prod.snd p.decls) = false :=
          emptyLinear_not_copy p name hemp
        have hnotLiveE : e.live = false := by
          rcases hdischEntry.1 with hcp | hlv
          · rw [htyNom, hnotCopy] at hcp; cases hcp
          · exact hlv
        have hnotInit : r ∉ init := hifFalse hnotLiveE
        have hnotInitB : init.contains r = false := by
          cases h : init.contains r
          · rfl
          · exact False.elim (hnotInit (List.contains_iff_mem.mp h))
        have hrLoc' : AList.lookup mirLocals r = some (nominalTy name) := by
          simpa [tyToEarly, htyNom] using hrLoc
        refine ⟨_, sbm_endOwnerView_eq hsbm pc live init vals hrLoc', ?_⟩
        simp [Own.canDropPlaces, hnotInit]
  · -- wildcard temp (`p.freshBase ≤ r.id`): `u8`, Copy
    have hge : p.freshBase ≤ r.id := Nat.le_of_not_lt hlt
    have hrLoc' := (hinv.liveGeU8 r hr hge).2
    refine ⟨_, sbm_endOwnerView_eq hsbm pc live init vals hrLoc', ?_⟩
    cases init.contains r <;> simp [Own.canDropPlaces, sliceLanguage, byteTy]

private theorem sbm_result_ret (hsbm : SingleBlockMir mir mirLocals block)
    {p : StraightProgram} {pos : Nat} {Γ Γ₁ : Ctx} {env : Env}
    {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    {a : Slice0.Atom}
    (hend : block.ending = .ret (some ⟨byteTy, lowerAtom a⟩))
    (hinv : FlatInv p mirLocals pos Γ env live init vals)
    (ht : HasType (List.map Prod.snd p.decls) Γ.tys (.atom a.toCore) (.scalar (.int .u8)))
    (hu : Uses (List.map Prod.snd p.decls) Γ (.atom a.toCore) Γ₁)
    (hdisch : Discharged (List.map Prod.snd p.decls) Γ₁) :
    ∃ n : Int, ConcreteSpec.IntTy.u8.inRange n = true ∧
      retOutcome (List.map Prod.snd p.decls) env a.toCore = .exit n.toNat ∧
      (MIR.Execution (language := sliceLanguage slice)).result mir (flatState block.body.length live init vals)
        (.returned (UInt8.ofNat n.toNat) ByteArray.empty) := by
  have hat : AtomTy Γ.tys a.toCore (.int .u8) := by
    cases a with
    | lit n => cases ht with | int hr => exact .int hr
    | var x => cases ht with | copy hl => exact .copy hl
  cases hu with
  | atom hua =>
    obtain ⟨n, hrange, hread, heval, hinv1⟩ := sbm_readAtom hsbm block.body.length live init vals hinv hat hua
    have hcanEnd := sbm_canEnd_all hsbm hinv1 hdisch block.body.length
    have hbounds : 0 ≤ n ∧ n < 256 := by
      have h : 0 ≤ n ∧ n ≤ 255 := of_decide_eq_true hrange
      exact ⟨h.1, Int.lt_add_one_of_le h.2⟩
    have hfind := sbm_blockAt_find hsbm
    refine ⟨n, hrange, by simp [retOutcome, heval], ?_⟩
    have hterm : (mir.logicalView (language := sliceLanguage slice)).terminalObservation
        (flatState block.body.length live init vals) =
        some (.returned (UInt8.ofNat n.toNat) ByteArray.empty) := by
      have hmg : AList.lookup mir.functions
          (flatState (slice := slice) (profile := profile) block.body.length live init vals).frame =
          some (mirLocals, { entry := ⟨0⟩, blocks := #[block] }) := by
        rw [hsbm.fnsEq]; rfl
      have hstepTerm := mir_terminalObservation_ret (s := flatState block.body.length live init vals)
        hmg hfind rfl hend rfl hsbm.entryEq.symm rfl
      rw [hstepTerm, hread]
      dsimp only [Option.bind]
      change (if !live.all ((mir.logicalView (language := sliceLanguage slice)).canEnd
          (flatState block.body.length live init vals)) then none else _) = _
      rw [hcanEnd]
      simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte,
        CoreLanguage.hostedResult, hbounds, and_self, Observe.Observation.status]
    with_unfolding_all
      change Option.bind ((mir.logicalView (language := sliceLanguage slice)).terminalObservation
        (flatState block.body.length live init vals)) _ = _
      rw [hterm]
      rfl

/-! ### Row-by-row simulation of `lowerBindings` against `ConcreteSpec.runBody` -/

private def ObservableAt {language : CoreLanguage} (core : CoreInput language)
    (Δ : Decls) (env : Env) (pos : Nat) (stmts : List ConcreteSpec.Stmt)
    (outcome : Outcome) (obs : Concrete.Contracts.Observation) : Prop :=
  match outcome with
  | .exit n => obs = .returned (UInt8.ofNat n) ByteArray.empty
  | .trap .arithmeticOverflow =>
    (Agreement.firstTrapSite Δ env pos stmts).bind
      (fun site => CoreInput.observeHosted core (.trapped 134 .overflow site)) = some obs
  | .aborted site => CoreInput.observeHosted core (.trapped 134 .abort site) = some obs
  | _ => False

private theorem mir_simulate_rows {core : CoreInput (sliceLanguage slice)} {v : AdmissionView}
    {discharges : Discharges (sliceLanguage slice)}
    (hsbm : SingleBlockMir mir mirLocals block)
    (hrep : v.Represents core) (hwf : core.WF)
    (hdeclsOk : DeclsOk (List.map Prod.snd v.located.decls))
    (hproj : mir.project = core.project)
    (hdecls : mir.declarations = v.located.decls)
    (hendEq : block.ending = (lowerEnding (slice := slice) v.located).1)
    {rows : List LocatedStraightBinding} {origins : List (SiteId × Origin)}
    {res : LowerRow slice} {pos pc : Nat} {Γ : Ctx} {env : Env}
    {live init : List PlaceId} {vals : AList PlaceId EarlyValue}
    (hrowsLower : lowerBindings discharges v.located.freshBase rows origins pos = .ok res)
    (hsubLocals : ∀ r ty, (r, ty) ∈ res.locals → (r, ty) ∈ mirLocals)
    (hinstsAt : ∀ j < res.insts.length, block.body[pc + j]? = res.insts[j]?)
    (hlenEnd : pc + res.insts.length = block.body.length)
    (horigLen : origins.length = rows.length)
    (horigCore : ∀ k < rows.length, ∃ orig, origins[k]? = some (⟨pos + k⟩, orig) ∧
      orig.operation = ⟨pos + k⟩ ∧ (⟨pos + k⟩, orig) ∈ core.machineProgram.origins)
    (hbounds : ∀ row ∈ rows, row.statement.bound ≤ v.located.freshBase)
    (hctx : CtxInv v.located Γ)
    (hbody : BodyOkWith ArithmeticPolicy.runtime.permits (List.map Prod.snd v.located.decls) Γ
      (rows.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
      (v.located.ending.toCore v.located.endingOrigin.1))
    (hinv : FlatInv v.located mirLocals pos Γ env live init vals)
    (hnoDischTrap : ∀ site, Agreement.firstTrapSite (List.map Prod.snd v.located.decls) env pos
        (rows.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) = some site →
      AList.lookup discharges site = none) :
    ∃ sFinal obs,
      DetChain mir (flatState pc live init vals) sFinal ∧
      (MIR.Execution (language := sliceLanguage slice)).result mir sFinal obs ∧
      ObservableAt core (List.map Prod.snd v.located.decls) env pos
        (rows.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
        (ConcreteSpec.runBody (List.map Prod.snd v.located.decls) (rows.length + 1) env
          (rows.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
          (v.located.ending.toCore v.located.endingOrigin.1))
        obs := by
  let p := v.located
  let Δ := List.map Prod.snd p.decls
  induction rows generalizing origins res pos pc Γ env live init vals with
  | nil =>
    cases origins with
    | cons _ _ => cases horigLen
    | nil =>
      simp only [lowerBindings, Except.ok.injEq] at hrowsLower
      subst hrowsLower
      have hpcEq : pc = block.body.length := by simpa using hlenEnd
      subst hpcEq
      cases hend : v.located.ending with
      | ret a =>
        rw [hend] at hbody
        cases hbody with
        | ret ht hu hdisch =>
          have hblkEnd : block.ending = .ret (some ⟨byteTy, lowerAtom a⟩) := by
            rw [hendEq]; simp [lowerEnding, hend]
          obtain ⟨n, hrange, hretOut, hres⟩ := sbm_result_ret hsbm hblkEnd hinv ht hu hdisch
          refine ⟨flatState block.body.length live init vals,
            .returned (UInt8.ofNat n.toNat) ByteArray.empty, .nil _, hres, ?_⟩
          simp [ConcreteSpec.runBody, finish, StraightEnd.toCore, hretOut, ObservableAt]
      | abort =>
        have hblkEnd : block.ending = .trap .abort p.endingOrigin.2 := by
          rw [hendEq]; simp [lowerEnding, hend, p]
        have hendOrigMem : (p.endingOrigin.1, p.endingOrigin.2) ∈ core.machineProgram.origins := by
          obtain ⟨_, eq, hpay⟩ := hrep
          have heq : eq = rfl := rfl
          rw [heq] at hpay
          simp [CoreInput.machineProgram, hpay, Payload.execution, AdmissionView.execution, p]
        have hendOrigOp : p.endingOrigin.2.operation = p.endingOrigin.1 :=
          (hwf.1.2.2.2.2.2 p.endingOrigin.1 p.endingOrigin.2 hendOrigMem).1.symm
        obtain ⟨span, file, hfileAt, _⟩ := (hwf.1.2.2.2.2.2 p.endingOrigin.1 p.endingOrigin.2 hendOrigMem).2
        have hfileAt' : core.project.fileAt p.endingOrigin.2.lexical.file = some file := by
          rw [Project.fileAt, ← Array.getElem?_toList]; exact hfileAt
        let resolved : DiagnosticSite :=
          ⟨p.endingOrigin.1, ⟨file.path.render.text, p.endingOrigin.2.lexical.line, p.endingOrigin.2.lexical.col⟩⟩
        have hsiteMir : runtimeSite mir.project p.endingOrigin.2 = some resolved := by
          rw [hproj]; simp [runtimeSite, hfileAt', hendOrigOp, resolved]
        have hres := sbm_result_abort (live := live) (init := init) (vals := vals) hsbm hblkEnd hsiteMir
        have hobsCore : CoreInput.observeHosted core (.trapped 134 .abort p.endingOrigin.1) =
            some (.trapped 134 .abort resolved
              (trapDiagnostic mir.project .abort p.endingOrigin.2.operation p.endingOrigin.2.lexical)) := by
          have hluO := alist_lookup_of_mem hwf.1.1 hendOrigMem
          have hsiteCore : runtimeSite core.project {p.endingOrigin.2 with operation := p.endingOrigin.1} = some resolved := by
            simp [runtimeSite, hfileAt', resolved]
          simp [CoreInput.observeHosted, hluO, hsiteCore, coreTrapDiagnostic, hproj,
            trapDiagnostic, hfileAt', hendOrigOp, resolved]
        refine ⟨flatState block.body.length live init vals, _, .nil _, hres, ?_⟩
        simp only [ConcreteSpec.runBody, finish, StraightEnd.toCore, ObservableAt, List.map_nil]
        exact hobsCore
  | cons row rest ih =>
    cases origins with
    | nil => cases horigLen
    | cons o0 restOrigins =>
      rcases o0 with ⟨site0, orig0⟩
      have hlen' : restOrigins.length = rest.length := Nat.succ.inj horigLen
      obtain ⟨orig0', horig0Eq, hop0, horig0Core⟩ := horigCore 0 (Nat.zero_lt_succ _)
      simp only [List.getElem?_cons_zero, Option.some.injEq, Prod.mk.injEq, Nat.add_zero] at horig0Eq
      obtain ⟨rfl, rfl⟩ := horig0Eq
      have hop0' : orig0.operation = ⟨pos⟩ := by rw [Nat.add_zero] at hop0; exact hop0
      have horig0Core' : (⟨pos⟩, orig0) ∈ core.machineProgram.origins := by rw [Nat.add_zero] at horig0Core; exact horig0Core
      have horigCore' : ∀ k < rest.length, ∃ orig, restOrigins[k]? = some (⟨pos + 1 + k⟩, orig) ∧
          orig.operation = ⟨pos + 1 + k⟩ ∧ (⟨pos + 1 + k⟩, orig) ∈ core.machineProgram.origins := by
        intro k hk
        obtain ⟨orig, hg, hop, hmem⟩ := horigCore (k + 1) (Nat.succ_lt_succ hk)
        rw [List.getElem?_cons_succ] at hg
        refine ⟨orig, ?_, ?_, ?_⟩
        · rw [Nat.add_assoc, Nat.add_comm 1 k]; exact hg
        · rw [Nat.add_assoc, Nat.add_comm 1 k]; exact hop
        · rw [Nat.add_assoc, Nat.add_comm 1 k]; exact hmem
      cases hbody with
      | @cons _ Γ₁ _ _ _ hstmt htailBody =>
        have hrowBound := hbounds row List.mem_cons_self
        have hrestBounds : ∀ r ∈ rest, r.statement.bound ≤ p.freshBase :=
          fun r hr => hbounds r (List.mem_cons_of_mem _ hr)
        obtain ⟨hctx', _, hshape⟩ := straightBinding_valid_shape hdeclsOk hctx hrowBound hstmt
        dsimp only [Function.comp_def] at hstmt
        unfold lowerBindings at hrowsLower
        cases hhead : lowerBinding discharges v.located.freshBase pos orig0 row.statement with
        | error _ => simp [hhead, Bind.bind, Except.bind] at hrowsLower
        | ok headRow =>
          cases htail : lowerBindings discharges v.located.freshBase rest restOrigins (pos + 1) with
          | error _ => simp [hhead, htail, Bind.bind, Except.bind] at hrowsLower
          | ok tailRow =>
            simp only [hhead, htail, Bind.bind, Except.bind, Pure.pure, Except.pure,
              Except.ok.injEq] at hrowsLower
            subst hrowsLower
            have hsubTail : ∀ r ty, (r, ty) ∈ tailRow.locals → (r, ty) ∈ mirLocals :=
              fun r ty hm => hsubLocals r ty (List.mem_append_right _ hm)
            have hsubHead : ∀ r ty, (r, ty) ∈ headRow.locals → (r, ty) ∈ mirLocals :=
              fun r ty hm => hsubLocals r ty (List.mem_append_left _ hm)
            have stepOne : ∀ (inst0 : LocatedOp (MIRInst (sliceLoweringLanguage slice)))
                (hheadInst : headRow.insts = [inst0])
                {env' : Env} {live' init' : List PlaceId} {vals' : AList PlaceId EarlyValue}
                (hexecOk : execStmt Δ env row.statement.toCore = .ok env')
                (hstepDet : (MIR.Execution (language := sliceLanguage slice)).step mir
                    (flatState pc live init vals) (flatState (pc + 1) live' init' vals') ∧
                  ∀ d, (MIR.Execution (language := sliceLanguage slice)).step mir
                    (flatState pc live init vals) d → d = flatState (pc + 1) live' init' vals')
                (hinv' : FlatInv p mirLocals (pos + 1) Γ₁ env' live' init' vals'),
                ∃ sFinal obs,
                  DetChain mir (flatState pc live init vals) sFinal ∧
                  (MIR.Execution (language := sliceLanguage slice)).result mir sFinal obs ∧
                  ObservableAt core Δ env pos
                    ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                    (ConcreteSpec.runBody Δ ((row :: rest).length + 1) env
                      ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                      (p.ending.toCore p.endingOrigin.1)) obs := by
              intro inst0 hheadInst env' live' init' vals' hexecOk hstepDet hinv'
              have hinstsTail : ∀ j < tailRow.insts.length, block.body[(pc + 1) + j]? = tailRow.insts[j]? := by
                intro j hj
                have hlt : j + 1 < (headRow.insts ++ tailRow.insts).length := by
                  rw [hheadInst]; simp; omega
                have h1 := hinstsAt (j + 1) hlt
                rw [hheadInst] at h1
                rw [List.cons_append, List.nil_append, List.getElem?_cons_succ] at h1
                rw [Nat.add_assoc, Nat.add_comm 1 j]; exact h1
              have hlenTail : (pc + 1) + tailRow.insts.length = block.body.length := by
                rw [hheadInst] at hlenEnd; simp at hlenEnd; omega
              have hnoDisch' : ∀ site, Agreement.firstTrapSite Δ env' (pos + 1)
                  (rest.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) = some site →
                  AList.lookup discharges site = none := by
                intro site hf
                apply hnoDischTrap site
                change Agreement.firstTrapSite Δ env pos
                  ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) = some site
                simp [Agreement.firstTrapSite, hexecOk, hf]
              obtain ⟨sFinal, obs, hchain', hres', hobs'⟩ :=
                ih htail hsubTail hinstsTail hlenTail hlen' horigCore' hrestBounds hctx' htailBody hinv' hnoDisch'
              refine ⟨sFinal, obs, .cons hstepDet.1 hstepDet.2 hchain', hres', ?_⟩
              have hrunEq : ConcreteSpec.runBody Δ ((row :: rest).length + 1) env
                  ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                  (p.ending.toCore p.endingOrigin.1) =
                  ConcreteSpec.runBody Δ (rest.length + 1) env'
                    (rest.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                    (p.ending.toCore p.endingOrigin.1) := by
                simp [ConcreteSpec.runBody, hexecOk]
              have hfirstEq : Agreement.firstTrapSite Δ env pos
                  ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) =
                  Agreement.firstTrapSite Δ env' (pos + 1)
                    (rest.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) := by
                simp [Agreement.firstTrapSite, hexecOk]
              unfold ObservableAt at hobs' ⊢
              rw [hrunEq, hfirstEq]
              exact hobs'
            -- Helper for getting the head instruction at `block.body[pc]?`
            have getHeadInst : ∀ (inst0 : LocatedOp (MIRInst (sliceLoweringLanguage slice))),
                headRow.insts = [inst0] → block.body[pc]? = some inst0 := by
              intro inst0 hhi
              have h0 := hinstsAt 0 (by rw [hhi]; simp)
              rw [hhi] at h0
              simpa using h0
            -- Helper: `evalAtom` ok implies `evalExpr (.atom a.toCore)` ok with the same scalar
            have evalAtom_to_expr : ∀ {a : Slice0.Atom} {n : Int},
                evalAtom Δ env a.toCore = .ok (.int .u8 n, env) →
                evalExpr Δ env (.atom a.toCore) = .ok (.scalar (.int .u8 n), env) := by
              intro a n heval
              cases a with
              | lit m =>
                simp only [Slice0.Atom.toCore, evalAtom, R.ok.injEq, Prod.mk.injEq, Scalar.int.injEq] at heval
                obtain ⟨⟨_, rfl⟩, _⟩ := heval; rfl
              | var y =>
                simp only [Slice0.Atom.toCore, evalAtom] at heval
                cases hrv : readVar Δ env y false with
                | ok res =>
                  rcases res with ⟨rv, e1⟩
                  rw [hrv] at heval
                  cases rv with
                  | struct _ _ => cases heval
                  | scalar sc =>
                    simp only [R.ok.injEq, Prod.mk.injEq] at heval
                    obtain ⟨rfl, rfl⟩ := heval
                    simp [evalExpr, Slice0.Atom.toCore, hrv]
                | trap _ => rw [hrv] at heval; cases heval
                | stuck _ => rw [hrv] at heval; cases heval
            let bDst : ConcreteSpec.Binder → PlaceId := fun b =>
              match b with | .name x => ⟨x.id⟩ | .wild => ⟨p.freshBase + pos⟩
            -- Helper for `.let_ binder u8 (.atom a)` (covers both `.slice0` and `.bind`)
            have handleAtomBind : ∀ (binder : ConcreteSpec.Binder) (a : Slice0.Atom)
                (hcoreEq : row.statement.toCore = .let_ binder (.scalar (.int .u8)) (.atom a.toCore))
                (hheadEq : headRow = ⟨[(bDst binder, byteTy)],
                  [⟨orig0, .bind ⟨bDst binder, []⟩ (lowerAtom a)⟩], []⟩)
                (hbi : ∀ x, binder = .name x → x.id < p.freshBase ∧ AList.lookup Γ x = none),
                ∃ sFinal obs,
                  DetChain mir (flatState pc live init vals) sFinal ∧
                  (MIR.Execution (language := sliceLanguage slice)).result mir sFinal obs ∧
                  ObservableAt core Δ env pos
                    ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                    (ConcreteSpec.runBody Δ ((row :: rest).length + 1) env
                      ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                      (p.ending.toCore p.endingOrigin.1)) obs := by
              intro binder a hcoreEq hheadEq hbi
              cases binder with
              | name x =>
                obtain ⟨hxlt, hfresh⟩ := hbi x rfl
                rw [hcoreEq] at hstmt
                cases hstmt with
                | let_ ht _ hu hfresh1 =>
                  have hat : AtomTy Γ.tys a.toCore (.int .u8) := by
                    cases a with
                    | lit n => cases ht with | int hr => exact .int hr
                    | var y => cases ht with | copy hl => exact .copy hl
                  cases hu with
                  | atom hua =>
                    obtain ⟨n, hrange, hread, heval, hinv1⟩ := sbm_readAtom hsbm pc live init vals hinv hat hua
                    have hxNotLive : (⟨x.id⟩ : PlaceId) ∉ live := fun hm => by
                      have := hinv.liveLtInCtx ⟨x.id⟩ hm hxlt; simp [hfresh] at this
                    have hxNotInit : (⟨x.id⟩ : PlaceId) ∉ init := fun hm => hxNotLive (hinv.initSubLive _ hm)
                    have hxLu : AList.lookup mirLocals ⟨x.id⟩ = some byteTy :=
                      alist_lookup_of_mem hsbm.localsNodup
                        (hsubHead ⟨x.id⟩ byteTy (by rw [hheadEq]; exact List.mem_cons_self))
                    have hstepDet := sbm_step_bind_det hsbm (getHeadInst _ (congrArg LowerRow.insts hheadEq))
                      hread hxNotLive hxNotInit hxLu
                    have hexecOk : execStmt Δ env row.statement.toCore =
                        .ok ((x, .val (specValOf (.scalar (.int .u8 n)))) :: env) := by
                      rw [hcoreEq]; simp [execStmt, evalAtom_to_expr heval, specValOf]
                    exact stepOne _ (congrArg LowerRow.insts hheadEq) hexecOk hstepDet
                      (flatInv_bind_named hinv1 hxlt hfresh1 rfl
                        (by simpa [tyToEarly] using hxLu) (.inl rfl) (.u8 hrange))
              | wild =>
                rw [hcoreEq] at hstmt
                cases hstmt with
                | letWild ht _ _ hu =>
                  have hat : AtomTy Γ.tys a.toCore (.int .u8) := by
                    cases a with
                    | lit n => cases ht with | int hr => exact .int hr
                    | var y => cases ht with | copy hl => exact .copy hl
                  cases hu with
                  | atom hua =>
                    obtain ⟨n, hrange, hread, heval, hinv1⟩ := sbm_readAtom hsbm pc live init vals hinv hat hua
                    have hwNotLive : (⟨p.freshBase + pos⟩ : PlaceId) ∉ live := fun hm => by
                      have := (hinv.liveGeU8 ⟨p.freshBase + pos⟩ hm (Nat.le_add_right _ _)).1
                      dsimp only [p] at this; omega
                    have hwNotInit : (⟨p.freshBase + pos⟩ : PlaceId) ∉ init := fun hm => hwNotLive (hinv.initSubLive _ hm)
                    have hwLu : AList.lookup mirLocals ⟨p.freshBase + pos⟩ = some byteTy :=
                      alist_lookup_of_mem hsbm.localsNodup
                        (hsubHead ⟨p.freshBase + pos⟩ byteTy (by rw [hheadEq]; exact List.mem_cons_self))
                    have hstepDet := sbm_step_bind_det hsbm (getHeadInst _ (congrArg LowerRow.insts hheadEq))
                      hread hwNotLive hwNotInit hwLu
                    have hexecOk : execStmt Δ env row.statement.toCore = .ok env := by
                      rw [hcoreEq]; simp [execStmt, evalAtom_to_expr heval]
                    have hinv' := flatInv_bind_wild hinv1 (.scalar (.int .u8 n)) hwLu
                    exact stepOne _ (congrArg LowerRow.insts hheadEq) hexecOk hstepDet hinv'
            -- Helper for `.let_ binder u8 (.add l r)` (covers both `.slice0` and `.bind`, both ok and overflow!)
            have handleAddBind : ∀ (binder : ConcreteSpec.Binder) (l r : Slice0.Atom)
                (hcoreEq : row.statement.toCore = .let_ binder (.scalar (.int .u8)) (.add l.toCore r.toCore))
                (hheadEq : headRow = ⟨[(bDst binder, byteTy)],
                  [⟨orig0, .op (opcode := .arithmetic (.plain .add)) (ty := byteTy)
                    ⟨bDst binder, []⟩ ()
                    (.cons (lowerAtom l) (.cons (lowerAtom r) .nil)) [addCheck discharges ⟨pos⟩] []⟩], []⟩)
                (hbi : ∀ x, binder = .name x → x.id < p.freshBase ∧ AList.lookup Γ x = none),
                ∃ sFinal obs,
                  DetChain mir (flatState pc live init vals) sFinal ∧
                  (MIR.Execution (language := sliceLanguage slice)).result mir sFinal obs ∧
                  ObservableAt core Δ env pos
                    ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                    (ConcreteSpec.runBody Δ ((row :: rest).length + 1) env
                      ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                      (p.ending.toCore p.endingOrigin.1)) obs := by
              intro binder l r hcoreEq hheadEq hbi
              let dst : PlaceId := bDst binder
              have hblkGet := getHeadInst _ (congrArg LowerRow.insts hheadEq)
              have hdstLu : AList.lookup mirLocals dst = some byteTy :=
                alist_lookup_of_mem hsbm.localsNodup
                  (hsubHead dst byteTy (by rw [hheadEq]; exact List.mem_cons_self))
              -- Shared overflow finish once we have `hevalL, hevalR, hreadL, hreadR, hfits : ¬ InRange`
              have finishOverflow : ∀ (a b : Int),
                  evalAtom Δ env l.toCore = .ok (.int .u8 a, env) →
                  evalAtom Δ env r.toCore = .ok (.int .u8 b, env) →
                  (mir.logicalView (language := sliceLanguage slice)).readValue
                    (flatState (slice := slice) (profile := profile) pc live init vals) (toMemRead (lowerAtom l)) =
                    some (.scalar (.int .u8 a), flatState pc live init vals) →
                  (mir.logicalView (language := sliceLanguage slice)).readValue
                    (flatState (slice := slice) (profile := profile) pc live init vals) (toMemRead (lowerAtom r)) =
                    some (.scalar (.int .u8 b), flatState pc live init vals) →
                  ¬ Core.IntTy.u8.InRange (a + b) →
                  ∃ sFinal obs,
                    DetChain mir (flatState pc live init vals) sFinal ∧
                    (MIR.Execution (language := sliceLanguage slice)).result mir sFinal obs ∧
                    ObservableAt core Δ env pos
                      ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                      (ConcreteSpec.runBody Δ ((row :: rest).length + 1) env
                        ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                        (p.ending.toCore p.endingOrigin.1)) obs := by
                intro a b hevalL hevalR hreadL hreadR hnotFits
                have hrangeF : ConcreteSpec.IntTy.u8.inRange (a + b) = false := decide_eq_false hnotFits
                have hexecTrap : execStmt Δ env row.statement.toCore = .trap .arithmeticOverflow := by
                  rw [hcoreEq]
                  cases binder <;> simp [execStmt, evalExpr, hevalL, hevalR, checkedAdd, hrangeF]
                have hfirstHere : Agreement.firstTrapSite Δ env pos
                    ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) =
                    some ⟨pos⟩ := by
                  simp [Agreement.firstTrapSite, hexecTrap]
                have hdischNone : AList.lookup discharges ⟨pos⟩ = none := hnoDischTrap ⟨pos⟩ hfirstHere
                have hchkRes : addCheck discharges ⟨pos⟩ = ⟨0, .overflow, .residual⟩ := by
                  simp [addCheck, hdischNone]
                have hblkGet' := hblkGet
                rw [hchkRes] at hblkGet'
                obtain ⟨span, file, hfileAt, _⟩ := (hwf.1.2.2.2.2.2 ⟨pos⟩ orig0 horig0Core').2
                have hfileAt' : core.project.fileAt orig0.lexical.file = some file := by
                  rw [Project.fileAt, ← Array.getElem?_toList]; exact hfileAt
                let resolved : DiagnosticSite :=
                  ⟨⟨pos⟩, ⟨file.path.render.text, orig0.lexical.line, orig0.lexical.col⟩⟩
                have hsiteMir : runtimeSite mir.project orig0 = some resolved := by
                  rw [hproj]; simp [runtimeSite, hfileAt', hop0', resolved]
                have hres := sbm_result_add_overflow hsbm hblkGet' hreadL hreadR hnotFits hsiteMir
                have hobsCore : CoreInput.observeHosted core (.trapped 134 .overflow ⟨pos⟩) =
                    some (.trapped 134 .overflow resolved
                      (trapDiagnostic mir.project .overflow orig0.operation orig0.lexical)) := by
                  have hluO := alist_lookup_of_mem hwf.1.1 horig0Core'
                  have hsiteCore : runtimeSite core.project {orig0 with operation := ⟨pos⟩} = some resolved := by
                    simp [runtimeSite, hfileAt', resolved]
                  simp [CoreInput.observeHosted, hluO, hsiteCore, coreTrapDiagnostic, hproj,
                    trapDiagnostic, hfileAt', hop0', resolved]
                refine ⟨flatState pc live init vals, _, .nil _, hres, ?_⟩
                have hrunTrap : ConcreteSpec.runBody Δ ((row :: rest).length + 1) env
                    ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                    (p.ending.toCore p.endingOrigin.1) = .trap .arithmeticOverflow := by
                  simp [ConcreteSpec.runBody, hexecTrap]
                rw [hrunTrap]
                unfold ObservableAt
                rw [hfirstHere]
                exact hobsCore
              cases binder with
              | name x =>
                obtain ⟨hxlt, hfresh⟩ := hbi x rfl
                rw [hcoreEq] at hstmt
                cases hstmt with
                | let_ ht _ hu hfresh2 =>
                  cases ht with | add htl htr =>
                  cases hu with | add hul hur =>
                  rename_i Γ_mid
                  obtain ⟨a, _, hreadL, hevalL, hinv1⟩ := sbm_readAtom hsbm pc live init vals hinv htl hul
                  have htr1 : AtomTy Γ_mid.tys r.toCore (.int .u8) := by rw [useAtom_tys hul]; exact htr
                  obtain ⟨b, _, hreadR, hevalR, hinv2⟩ := sbm_readAtom hsbm pc live init vals hinv1 htr1 hur
                  by_cases hfits : Core.IntTy.u8.InRange (a + b)
                  · have hrange : ConcreteSpec.IntTy.u8.inRange (a + b) = true := decide_eq_true hfits
                    have hxNotLive : (⟨x.id⟩ : PlaceId) ∉ live := fun hm => by
                      have := hinv.liveLtInCtx ⟨x.id⟩ hm hxlt; simp [hfresh] at this
                    have hxNotInit : (⟨x.id⟩ : PlaceId) ∉ init := fun hm => hxNotLive (hinv.initSubLive _ hm)
                    have hstepDet := sbm_step_add_ok_det hsbm hblkGet hreadL hreadR hfits
                      hxNotLive hxNotInit hdstLu
                    have hexecOk : execStmt Δ env row.statement.toCore =
                        .ok ((x, .val (specValOf (.scalar (.int .u8 (a + b))))) :: env) := by
                      rw [hcoreEq]; simp [execStmt, evalExpr, hevalL, hevalR, checkedAdd, hrange, specValOf, Δ, p]
                    exact stepOne _ (congrArg LowerRow.insts hheadEq) hexecOk hstepDet
                      (flatInv_bind_named hinv2 hxlt hfresh2 rfl
                        (by simpa [tyToEarly] using hdstLu) (.inl rfl) (.u8 hrange))
                  · exact finishOverflow a b hevalL hevalR hreadL hreadR hfits
              | wild =>
                rw [hcoreEq] at hstmt
                cases hstmt with
                | letWild ht _ _ hu =>
                  cases ht with | add htl htr =>
                  cases hu with | add hul hur =>
                  rename_i Γ_mid
                  obtain ⟨a, _, hreadL, hevalL, hinv1⟩ := sbm_readAtom hsbm pc live init vals hinv htl hul
                  have htr1 : AtomTy Γ_mid.tys r.toCore (.int .u8) := by rw [useAtom_tys hul]; exact htr
                  obtain ⟨b, _, hreadR, hevalR, hinv2⟩ := sbm_readAtom hsbm pc live init vals hinv1 htr1 hur
                  by_cases hfits : Core.IntTy.u8.InRange (a + b)
                  · have hrange : ConcreteSpec.IntTy.u8.inRange (a + b) = true := decide_eq_true hfits
                    have hwNotLive : (⟨p.freshBase + pos⟩ : PlaceId) ∉ live := fun hm => by
                      have := (hinv.liveGeU8 ⟨p.freshBase + pos⟩ hm (Nat.le_add_right _ _)).1
                      dsimp only [p] at this; omega
                    have hwNotInit : (⟨p.freshBase + pos⟩ : PlaceId) ∉ init := fun hm => hwNotLive (hinv.initSubLive _ hm)
                    have hstepDet := sbm_step_add_ok_det hsbm hblkGet hreadL hreadR hfits
                      hwNotLive hwNotInit hdstLu
                    have hexecOk : execStmt Δ env row.statement.toCore = .ok env := by
                      rw [hcoreEq]; simp [execStmt, evalExpr, hevalL, hevalR, checkedAdd, hrange, Δ, p]
                    have hinv' := flatInv_bind_wild hinv2 (.scalar (.int .u8 (a + b))) hdstLu
                    exact stepOne _ (congrArg LowerRow.insts hheadEq) hexecOk hstepDet hinv'
                  · exact finishOverflow a b hevalL hevalR hreadL hreadR hfits
            -- Now dispatch on the 8 valid shapes
            rcases hshape with
              ⟨x, a, hst, hxlt, hfresh⟩ |
              ⟨x, l, r, hst, hxlt, hfresh⟩ |
              ⟨binder, a, hst, hbi⟩ |
              ⟨binder, l, r, hst, hbi⟩ |
              ⟨x, name, y, hst, hxlt, hfresh, hemp⟩ |
              ⟨x, name, hst, hxlt, hfresh, hemp⟩ |
              ⟨name, y, hst, hemp⟩ |
              ⟨name, hst, hemp⟩
            · -- 1. slice0 atom
              exact handleAtomBind (.name x) a
                (by simp [hst, StraightBinding.toCore, Slice0.Stmt.toCore, Slice0.Expr.toCore])
                (by simp only [lowerBinding, hst, Except.ok.injEq] at hhead; exact hhead.symm)
                (fun y hy => by cases hy; exact ⟨hxlt, hfresh⟩)
            · -- 2. slice0 add
              exact handleAddBind (.name x) l r
                (by simp [hst, StraightBinding.toCore, Slice0.Stmt.toCore, Slice0.Expr.toCore])
                (by simp only [lowerBinding, hst, Except.ok.injEq] at hhead; exact hhead.symm)
                (fun y hy => by cases hy; exact ⟨hxlt, hfresh⟩)
            · -- 3. bind u8 atom
              exact handleAtomBind binder a
                (by simp [hst, StraightBinding.toCore, StraightType.toCore, StraightExpr.toCore, Slice0.Expr.toCore])
                (by simp only [lowerBinding, hst, Except.ok.injEq] at hhead; exact hhead.symm)
                hbi
            · -- 4. bind u8 add
              exact handleAddBind binder l r
                (by simp [hst, StraightBinding.toCore, StraightType.toCore, StraightExpr.toCore, Slice0.Expr.toCore])
                (by simp only [lowerBinding, hst, Except.ok.injEq] at hhead; exact hhead.symm)
                hbi
            · -- 5. bind nominal move
              have hheadEq : headRow = ⟨[(⟨x.id⟩, nominalTy name)],
                  [⟨orig0, .bind (ty := nominalTy name) ⟨⟨x.id⟩, []⟩ (.move ⟨⟨y.id⟩, []⟩)⟩], []⟩ := by
                simp only [lowerBinding, hst, Except.ok.injEq] at hhead; exact hhead.symm
              have hcoreEq : row.statement.toCore = .let_ (.name x) (.struct name) (.atom (.move y)) := by
                simp [hst, StraightBinding.toCore, StraightType.toCore, StraightExpr.toCore]
              rw [hcoreEq] at hstmt
              cases hstmt with
              | let_ ht _ hu hfresh1 =>
                cases hu with
                | atom hua =>
                  obtain ⟨hread, hrv, hinv1⟩ := sbm_readMove hsbm pc live init vals hinv ht hua
                  have hxNotLive : (⟨x.id⟩ : PlaceId) ∉ live := fun hm => by
                    have := hinv.liveLtInCtx ⟨x.id⟩ hm hxlt; simp [hfresh] at this
                  have hxNotInit1 : (⟨x.id⟩ : PlaceId) ∉ init.erase ⟨y.id⟩ := fun hm =>
                    hxNotLive (hinv.initSubLive _ (List.mem_of_mem_erase hm))
                  have hxLu : AList.lookup mirLocals ⟨x.id⟩ = some (nominalTy name) :=
                    alist_lookup_of_mem hsbm.localsNodup
                      (hsubHead ⟨x.id⟩ (nominalTy name) (by rw [hheadEq]; exact List.mem_cons_self))
                  have hstepDet := sbm_step_bind_det hsbm (getHeadInst _ (congrArg LowerRow.insts hheadEq))
                    hread hxNotLive hxNotInit1 hxLu
                  have hexecOk : execStmt Δ env row.statement.toCore =
                      .ok ((x, .val (specValOf (.other (nominalId name)))) ::
                        AList.update env y (fun _ => .moved)) := by
                    rw [hcoreEq]; simp only [execStmt, evalExpr, specValOf, Δ, p]; rw [hrv]
                  exact stepOne _ (congrArg LowerRow.insts hheadEq) hexecOk hstepDet
                    (flatInv_bind_named hinv1 hxlt hfresh1 rfl
                      (by simpa [tyToEarly] using hxLu) (.inr ⟨name, rfl, hemp⟩) (.nominal name))
            · -- 6. bind nominal construct
              have hheadEq : headRow = ⟨[(⟨x.id⟩, nominalTy name)],
                  [⟨orig0, .op (opcode := .named ⟨"constructEmpty"⟩) (ty := nominalTy name)
                    ⟨⟨x.id⟩, []⟩ () .nil [] []⟩], []⟩ := by
                simp only [lowerBinding, hst, ↓reduceIte, Except.ok.injEq] at hhead; exact hhead.symm
              have hcoreEq : row.statement.toCore = .let_ (.name x) (.struct name) (.mk name []) := by
                simp [hst, StraightBinding.toCore, StraightType.toCore, StraightExpr.toCore]
              rw [hcoreEq] at hstmt
              cases hstmt with
              | let_ _ _ hu hfresh1 =>
                cases hu with
                | mk hus =>
                  cases hus
                  have hxNotLive : (⟨x.id⟩ : PlaceId) ∉ live := fun hm => by
                    have := hinv.liveLtInCtx ⟨x.id⟩ hm hxlt; simp [hfresh] at this
                  have hxNotInit : (⟨x.id⟩ : PlaceId) ∉ init := fun hm => hxNotLive (hinv.initSubLive _ hm)
                  have hxLu : AList.lookup mirLocals ⟨x.id⟩ = some (nominalTy name) :=
                    alist_lookup_of_mem hsbm.localsNodup
                      (hsubHead ⟨x.id⟩ (nominalTy name) (by rw [hheadEq]; exact List.mem_cons_self))
                  have hstepDet := sbm_step_construct_det (vals := vals) hsbm
                    (getHeadInst _ (congrArg LowerRow.insts hheadEq))
                    (hdecls ▸ hemp) hxNotLive hxNotInit hxLu
                  have hexecOk : execStmt Δ env row.statement.toCore =
                      .ok ((x, .val (specValOf (.other (nominalId name)))) :: env) := by
                    rw [hcoreEq]; rfl
                  exact stepOne _ (congrArg LowerRow.insts hheadEq) hexecOk hstepDet
                    (flatInv_bind_named hinv hxlt hfresh1 rfl
                      (by simpa [tyToEarly] using hxLu) (.inr ⟨name, rfl, hemp⟩) (.nominal name))
            · -- 7. destructure move (1 inst)
              have hheadEq : headRow = ⟨[],
                  [⟨orig0, .destructure (ty := nominalTy name) ⟨⟨y.id⟩, []⟩ []⟩], []⟩ := by
                simp only [lowerBinding, hst, Except.ok.injEq] at hhead; exact hhead.symm
              have hcoreEq : row.statement.toCore = .letStruct name [] (.atom (.move y)) := by
                simp [hst, StraightBinding.toCore, StraightExpr.toCore]
              rw [hcoreEq] at hstmt
              cases hstmt with
              | letStruct ht _ _ _ hu _ =>
                cases hu with
                | atom hua =>
                  obtain ⟨hread, hrv, hinv1⟩ := sbm_readMove hsbm pc live init vals hinv ht hua
                  have hstepDet := sbm_step_destructure_det hsbm (getHeadInst _ (congrArg LowerRow.insts hheadEq))
                    (hdecls ▸ hemp) hread
                  have hexecOk : execStmt Δ env row.statement.toCore =
                      .ok (AList.update env y (fun _ => .moved)) := by
                    rw [hcoreEq]; simp only [execStmt, evalExpr, ConcreteSpec.destructure, Δ, p]
                    rw [hrv]; simp
                  exact stepOne _ (congrArg LowerRow.insts hheadEq) hexecOk hstepDet (flatInv_step_pos hinv1)
            · -- 8. destructure construct (0 insts — pure dead construct+consume!)
              have hheadEq : headRow = ⟨[], [], [orig0]⟩ := by
                simp only [lowerBinding, hst, ↓reduceIte, Except.ok.injEq] at hhead; exact hhead.symm
              have hcoreEq : row.statement.toCore = .letStruct name [] (.mk name []) := by
                simp [hst, StraightBinding.toCore, StraightExpr.toCore]
              rw [hcoreEq] at hstmt
              cases hstmt with
              | letStruct _ _ _ _ hu _ =>
                cases hu with
                | mk hus =>
                  cases hus
                  have hexecOk : execStmt Δ env row.statement.toCore = .ok env := by
                    rw [hcoreEq]; simp [execStmt, evalExpr, evalFields, ConcreteSpec.destructure]
                  have hinstsTail : ∀ j < tailRow.insts.length, block.body[pc + j]? = tailRow.insts[j]? := by
                    intro j hj
                    have h0 := hinstsAt j (by rw [hheadEq]; exact hj)
                    rw [hheadEq] at h0; simpa using h0
                  have hlenTail : pc + tailRow.insts.length = block.body.length := by
                    rw [hheadEq] at hlenEnd; simpa using hlenEnd
                  have hnoDisch' : ∀ site, Agreement.firstTrapSite Δ env (pos + 1)
                      (rest.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) = some site →
                      AList.lookup discharges site = none := by
                    intro site hf
                    apply hnoDischTrap site
                    change Agreement.firstTrapSite Δ env pos
                      ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) = some site
                    simp [Agreement.firstTrapSite, hexecOk, hf]
                  obtain ⟨sFinal, obs, hchain', hres', hobs'⟩ :=
                    ih htail hsubTail hinstsTail hlenTail hlen' horigCore' hrestBounds hctx' htailBody
                      (flatInv_step_pos hinv) hnoDisch'
                  refine ⟨sFinal, obs, hchain', hres', ?_⟩
                  have hrunEq : ConcreteSpec.runBody Δ ((row :: rest).length + 1) env
                      ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                      (p.ending.toCore p.endingOrigin.1) =
                      ConcreteSpec.runBody Δ (rest.length + 1) env
                        (rest.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
                        (p.ending.toCore p.endingOrigin.1) := by
                    simp [ConcreteSpec.runBody, hexecOk]
                  have hfirstEq : Agreement.firstTrapSite Δ env pos
                      ((row :: rest).map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) =
                      Agreement.firstTrapSite Δ env (pos + 1)
                        (rest.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) := by
                    simp [Agreement.firstTrapSite, hexecOk]
                  unfold ObservableAt at hobs' ⊢
                  rw [hrunEq, hfirstEq]
                  exact hobs'

end FlatLogical

/-! ### Behavior refinement and master specification theorems -/

private theorem lowerWith_behaviorRefinement {slice : Slice} {profile : Profile}
    {core : CoreInput (sliceLanguage slice)} {v : AdmissionView}
    {discharges : Discharges (sliceLanguage slice)}
    {body : LowerRow slice} {mir : MIR (sliceLoweringLanguage slice) profile}
    (hrep : v.Represents core) (hunchecked : core.UncheckedWF)
    (hdisch : ValidDischarges core discharges)
    (hdeclsOk : DeclsOk (List.map Prod.snd v.located.decls))
    (hbodyOk : BodyOkWith ArithmeticPolicy.runtime.permits (List.map Prod.snd v.located.decls) []
      (v.located.bindings.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
      (v.located.ending.toCore v.located.endingOrigin.1))
    (hbind : lowerBindings discharges v.located.freshBase v.located.bindings v.located.bindingOrigins 0 = .ok body)
    (hspec : LowerBindingsSpec v.located discharges [] 0 v.located.bindings v.located.bindingOrigins body)
    (hmir : lowerProgram core.project v.located discharges = .ok mir) :
    BehaviorRefinement CoreInput.Execution (MIR.Execution (language := sliceLanguage slice))
      CoreInput.Execution.result (MIR.Execution (language := sliceLanguage slice)).result core mir ∧
    ∀ o, (MIR.Execution (language := sliceLanguage slice)).observes mir o ↔
      CoreInput.Execution.observes core o := by
  let p := v.located
  let block : MIRBlock slice := {
    id := ⟨0⟩
    params := []
    body := body.insts
    ending := (lowerEnding (slice := slice) p).1
    boundaryOrigins := body.boundary ++ (lowerEnding (slice := slice) p).2
  }
  have hmirEq : mir = {
    declarations := p.decls, project := core.project, entry := earlyEntry,
    signatures := [(earlyEntry, ⟨[], some byteTy⟩)],
    functions := [(earlyEntry, (body.locals, { entry := ⟨0⟩, blocks := #[block] }))],
    mutableLocals := [] } := by
    unfold lowerProgram at hmir
    simp only [hbind, Bind.bind, Except.bind, Pure.pure, Except.pure, Except.ok.injEq] at hmir
    exact hmir.symm
  have hsbm : SingleBlockMir mir body.locals block :=
    ⟨by rw [hmirEq], by rw [hmirEq], by rw [hmirEq]; rfl, rfl, rfl, hspec.localsNodup⟩
  have hcoreOrigs : core.machineProgram.origins = p.bindingOrigins ++ [p.endingOrigin] := by
    obtain ⟨_, eq, hpay⟩ := hrep
    have heq : eq = rfl := rfl
    rw [heq] at hpay
    simp [CoreInput.machineProgram, hpay, Payload.execution, AdmissionView.execution, p]
  have horigCore : ∀ k < p.bindings.length, ∃ orig, p.bindingOrigins[k]? = some (⟨0 + k⟩, orig) ∧
      orig.operation = ⟨0 + k⟩ ∧ (⟨0 + k⟩, orig) ∈ core.machineProgram.origins := by
    intro k hk
    have hsite := bindingOrigins_getElem?_site p k hk
    have hltOrig : k < p.bindingOrigins.length := (bindingOrigins_length p).symm ▸ hk
    cases hg : p.bindingOrigins[k]? with
    | none => rw [List.getElem?_eq_getElem hltOrig] at hg; cases hg
    | some pair =>
      rw [hg, Option.map_some, Option.some.injEq, Prod.mk.injEq] at hsite
      obtain ⟨h1, h2⟩ := hsite
      have hpair : pair = (⟨k⟩, pair.2) := Prod.ext h1 rfl
      rw [hpair] at hg
      refine ⟨pair.2, by rw [Nat.zero_add]; exact congrArg some hpair, by rw [Nat.zero_add]; exact h2, ?_⟩
      rw [hcoreOrigs]
      exact List.mem_append_left _ (List.mem_of_getElem? (by rw [Nat.zero_add]; exact hg))
  have hnoDischTrap : ∀ site, Agreement.firstTrapSite (List.map Prod.snd p.decls) [] 0
      (p.bindings.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) = some site →
      AList.lookup discharges site = none := by
    intro site hfirst
    have hfirstV : Agreement.firstTrapSite v.program.decls [] 0 v.program.body = some site := by
      rw [view_program_eq]; exact hfirst
    have hrun : ConcreteSpec.run v.program (v.program.body.length + 1) = .trap .arithmeticOverflow := by
      unfold ConcreteSpec.run
      exact firstTrapSite_implies_run_overflow hfirstV
    exact discharged_site_not_overflow hrep hunchecked hdisch hrun hfirstV
  have hinv0 : FlatInv p body.locals 0 [] [] [] [] [] :=
    ⟨.nil, (fun _ h => nomatch h), (fun _ h => nomatch h), (fun _ h => nomatch h), (fun _ _ h => nomatch h)⟩
  obtain ⟨sFinal, obs, hchain, hres, hobsAt⟩ :=
    mir_simulate_rows (pc := 0) hsbm hrep hunchecked.1 hdeclsOk (by rw [hmirEq]) (by rw [hmirEq]) rfl
      hbind (fun _ _ h => h) (fun j _ => by simp [block]) (by simp [block])
      (bindingOrigins_length p) horigCore (fun _ h => freshBase_bounds_binding p h)
      (fun _ _ h => by cases h) hbodyOk hinv0 hnoDischTrap
  -- Match `obs` with Core's unique observation from `Agreement.unchecked_agreement`
  obtain ⟨_, coreObs, hcoreObsable, hcoreObsIff⟩ := Agreement.unchecked_agreement hunchecked v hrep
  have hobsEq : obs = coreObs := by
    have hprog := view_program_eq v
    have hobsAtV : Agreement.Observable core v.program
        (ConcreteSpec.run v.program (v.program.body.length + 1)) obs := by
      unfold ConcreteSpec.run Agreement.Observable
      rw [hprog]
      simp only [List.length_map] at hobsAt ⊢
      generalize ConcreteSpec.runBody (List.map Prod.snd v.located.decls)
        (v.located.bindings.length + 1) []
        (List.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement) v.located.bindings)
        (v.located.ending.toCore v.located.endingOrigin.1) = out at hobsAt ⊢
      cases out with
      | exit n => exact hobsAt
      | aborted site => exact hobsAt
      | stuck _ | outOfFuel => exact False.elim hobsAt
      | trap k =>
        cases k with
        | arithmeticOverflow => exact hobsAt
    generalize hout : ConcreteSpec.run v.program (v.program.body.length + 1) = out at hobsAtV hcoreObsable
    cases out with
    | exit n => exact hobsAtV.trans hcoreObsable.symm
    | aborted site => exact Option.some.inj (hobsAtV.symm.trans hcoreObsable)
    | stuck _ | outOfFuel => exact False.elim hobsAtV
    | trap k =>
      cases k with
      | arithmeticOverflow => exact Option.some.inj (hobsAtV.symm.trans hcoreObsable)
  subst hobsEq
  have hbeh := behaviorRefinement_of_detChain (sbm_initial_iff hsbm) hchain hres ((hcoreObsIff obs).mpr rfl)
  refine ⟨hbeh, fun o => ⟨?_, ?_⟩⟩
  · intro ⟨q, hfin, hresQ⟩
    obtain ⟨p', hpFin, hpRes, _⟩ := (hbeh (Agreement.unchecked_agreement hunchecked v hrep).1).1 q o hfin hresQ
    exact ⟨p', hpFin, hpRes⟩
  · intro hcoreO
    have hoEq : o = obs := (hcoreObsIff o).mp hcoreO
    subst hoEq
    have htoSteps : ∀ {a b}, DetChain mir a b →
        Steps ((MIR.Execution (language := sliceLanguage slice)).step mir) a b := by
      intro a b hc
      induction hc with
      | nil s => exact .nil s
      | cons hst _ _ ih => exact .cons hst ih
    exact ⟨sFinal, ⟨flatState 0 [] [] [], (sbm_initial_iff hsbm _).mpr rfl, htoSteps hchain⟩, hres⟩

/-- Every unchecked-well-formed early Core input satisfies `ValidDischarges core []`. -/
theorem validDischarges_nil_of_uncheckedWF {slice : Slice} {core : CoreInput (sliceLanguage slice)}
    (hunchecked : core.UncheckedWF) :
    ValidDischarges core [] := by
  obtain ⟨_, _, v, hrep, _⟩ := hunchecked
  obtain ⟨_, eq, hpay⟩ := hrep
  have heq : eq = rfl := rfl
  rw [heq] at hpay
  refine ⟨?_, .nil, (fun _ _ h => nomatch h), (fun _ _ h => by obtain ⟨_, hm, _⟩ := h; cases hm)⟩
  unfold CoreInput.machineProgram
  rw [hpay]
  simp [Payload.execution, AdmissionView.execution, MachineProgram.initial?, Exec.enter,
    CoreLanguage.emptyStore, Own.instrumentLife, Own.life, Own.enter,
    AList.lookup, earlyEntry, sliceLanguage]

/-- Master specification for `lowerWith`: on any `CoreInput.UncheckedWF` paired with any
`ValidDischarges core discharges` (full, partial, or empty), `lowerWith` succeeds and
produces a `MIR.HandoffWF` module satisfying `LowersCore ⟨core, discharges⟩ mir`. -/
theorem lowerWith_spec {slice : Slice} {profile : Profile}
    {core : CoreInput (sliceLanguage slice)} {discharges : Discharges (sliceLanguage slice)}
    (hunchecked : core.UncheckedWF) (hdisch : ValidDischarges core discharges) :
    ∃ mir : MIR (sliceLoweringLanguage slice) profile,
      lowerWith core discharges = .ok mir ∧
      MIR.HandoffWF (language := sliceLanguage slice) mir ∧
      LowersCore (language := sliceLanguage slice) ⟨core, discharges⟩ mir := by
  have huncheckedOrig : core.UncheckedWF := hunchecked
  obtain ⟨hwf, ⟨vt, hrepT, hdeclsT, _⟩, v, hrep, hbodyV⟩ := hunchecked
  have hvtEq : vt = v := vt.unique hrepT hrep
  subst vt
  have hdec : core.decode = .ok v := (core.decode_iff v).mpr hrep
  have hdeclsOk : DeclsOk (List.map Prod.snd v.located.decls) := by
    rw [view_program_eq] at hdeclsT; exact hdeclsT
  have hbodyOk : BodyOkWith ArithmeticPolicy.runtime.permits (List.map Prod.snd v.located.decls) []
      (v.located.bindings.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
      (v.located.ending.toCore v.located.endingOrigin.1) := by
    rw [view_program_eq] at hbodyV; exact hbodyV
  obtain ⟨body, hbind, hspec⟩ := lowerBindings_spec (pos := 0) v.located discharges hdeclsOk
    (fun _ h => freshBase_bounds_binding v.located h) (fun _ _ h => by cases h) hbodyOk
    (bindingOrigins_length v.located) (fun k hk => by simpa [Nat.zero_add] using bindingOrigins_getElem?_site v.located k hk)
  let p := v.located
  let ending := (lowerEnding (slice := slice) p).1
  let endBoundary := (lowerEnding (slice := slice) p).2
  let mir : MIR (sliceLoweringLanguage slice) profile := {
    declarations := p.decls
    project := core.project
    entry := earlyEntry
    signatures := [(earlyEntry, ⟨[], some byteTy⟩)]
    functions := [(earlyEntry, (body.locals, {
      entry := ⟨0⟩
      blocks := #[{
        id := ⟨0⟩
        params := []
        body := body.insts
        ending := ending
        boundaryOrigins := body.boundary ++ endBoundary }] }))]
    mutableLocals := []
  }
  have hprogOk : lowerProgram (profile := profile) core.project v.located discharges = .ok mir := by
    unfold lowerProgram
    simp [hbind, Bind.bind, Except.bind, Pure.pure, Except.pure, mir, ending, endBoundary, p]
  have hlowerOk : lowerWith (profile := profile) core discharges = .ok mir := by
    unfold lowerWith
    rw [hdec]
    exact hprogOk
  have hcoreEntry : core.machineProgram.entry = earlyEntry := by
    obtain ⟨_, eq, hpay⟩ := hrep
    have heq : eq = rfl := rfl
    rw [heq] at hpay
    simp [CoreInput.machineProgram, hpay, Payload.execution, AdmissionView.execution]
  have hcoreDecls : core.machineProgram.declarations = p.decls := by
    obtain ⟨_, eq, hpay⟩ := hrep
    have heq : eq = rfl := rfl
    rw [heq] at hpay
    simp [CoreInput.machineProgram, hpay, Payload.execution, AdmissionView.execution, p]
  have hcoreOrigs : core.machineProgram.origins = p.bindingOrigins ++ [p.endingOrigin] := by
    obtain ⟨_, eq, hpay⟩ := hrep
    have heq : eq = rfl := rfl
    rw [heq] at hpay
    simp [CoreInput.machineProgram, hpay, Payload.execution, AdmissionView.execution, p]
  have hhandWF := lowerWith_handoffWF hrep hwf hdeclsOk hbind hspec hprogOk
  have horigPres : PreservesOrigins (List.map Prod.snd core.machineProgram.origins) mir.origins := by
    rw [hcoreOrigs]
    exact lowerProgram_origins core.project p discharges hbind hspec.originsMatch mir hprogOk
  have hmutPres := lowerWith_preservesOwnerMutability hrep hprogOk
  have hconsEv := lowerWith_consumesEvidence hrep hwf hdisch hbind hspec hprogOk
  have hbehRef := (lowerWith_behaviorRefinement hrep huncheckedOrig hdisch hdeclsOk hbodyOk hbind hspec hprogOk).1
  refine ⟨mir, hlowerOk, hhandWF, rfl, hcoreEntry.symm, horigPres, hconsEv,
    ⟨hmutPres, hcoreDecls.symm⟩, hbehRef⟩

/-- Exact behavioral equivalence for `lowerWith`: on any `CoreInput.UncheckedWF` paired
with valid discharges, the lowered MIR observes the exact same observations as Core. -/
theorem lowerWith_observes_iff {slice : Slice} {profile : Profile}
    {core : CoreInput (sliceLanguage slice)} {discharges : Discharges (sliceLanguage slice)}
    {mir : MIR (sliceLoweringLanguage slice) profile}
    (hunchecked : core.UncheckedWF) (hdisch : ValidDischarges core discharges)
    (hok : lowerWith core discharges = .ok mir) :
    ∀ obs, (MIR.Execution (language := sliceLanguage slice)).observes mir obs ↔
      CoreInput.Execution.observes core obs := by
  have huncheckedOrig : core.UncheckedWF := hunchecked
  obtain ⟨_, ⟨vt, hrepT, hdeclsT, _⟩, v, hrep, hbodyV⟩ := hunchecked
  have hvtEq : vt = v := vt.unique hrepT hrep
  subst vt
  have hdec : core.decode = .ok v := (core.decode_iff v).mpr hrep
  have hdeclsOk : DeclsOk (List.map Prod.snd v.located.decls) := by
    rw [view_program_eq] at hdeclsT; exact hdeclsT
  have hbodyOk : BodyOkWith ArithmeticPolicy.runtime.permits (List.map Prod.snd v.located.decls) []
      (v.located.bindings.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
      (v.located.ending.toCore v.located.endingOrigin.1) := by
    rw [view_program_eq] at hbodyV; exact hbodyV
  obtain ⟨body, hbind, hspec⟩ := lowerBindings_spec (pos := 0) v.located discharges hdeclsOk
    (fun _ h => freshBase_bounds_binding v.located h) (fun _ _ h => by cases h) hbodyOk
    (bindingOrigins_length v.located) (fun k hk => by simpa [Nat.zero_add] using bindingOrigins_getElem?_site v.located k hk)
  have hprogOk : lowerProgram (profile := profile) core.project v.located discharges = .ok mir := by
    unfold lowerWith at hok; rw [hdec] at hok; exact hok
  exact (lowerWith_behaviorRefinement hrep huncheckedOrig hdisch hdeclsOk hbodyOk hbind hspec hprogOk).2

/-- Checked-route lowering specification: every `Admitted.Valid` input lowers to a
`MIR.HandoffWF` module satisfying `LowersCore`. -/
theorem lowerCore_spec {slice : Slice} {profile : Profile}
    {input : Admitted (sliceLanguage slice)} (hvalid : input.Valid) :
    ∃ mir : MIR (sliceLoweringLanguage slice) profile,
      lowerCore input = .ok mir ∧
      MIR.HandoffWF (language := sliceLanguage slice) mir ∧
      LowersCore (language := sliceLanguage slice) input mir :=
  lowerWith_spec hvalid.1.1 hvalid.2

/-- Unchecked-route lowering specification: every `CoreInput.UncheckedWF` input lowers to a
`MIR.HandoffWF` module satisfying `LowersUnchecked`. -/
theorem lowerUnchecked_spec {slice : Slice} {profile : Profile}
    {input : CoreInput (sliceLanguage slice)} (hunchecked : input.UncheckedWF) :
    ∃ mir : MIR (sliceLoweringLanguage slice) profile,
      lowerUnchecked input = .ok mir ∧
      MIR.HandoffWF (language := sliceLanguage slice) mir ∧
      LowersUnchecked (language := sliceLanguage slice) input mir := by
  obtain ⟨mir, hok, hwf, hlow⟩ := lowerWith_spec hunchecked (validDischarges_nil_of_uncheckedWF hunchecked)
  exact ⟨mir, hok, hwf, hunchecked, hlow⟩

/-- Universal stage validation for the checked `lowerCore` edge in `Registry`. -/
theorem lowerCore_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.lowerCore = lowerCore) :
    (registry passes).lowerCore.Validated := by
  constructor
  · intro input mir hvalid ran
    change passes.lowerCore input = .ok mir at ran
    rw [implementation] at ran
    obtain ⟨mir', hok, hwf, hlow⟩ := lowerCore_spec (profile := profile) hvalid
    cases Except.ok.inj (ran.symm.trans hok)
    exact ⟨hwf, hlow⟩
  · intro input failure hvalid ran
    change passes.lowerCore input = .error failure at ran
    rw [implementation] at ran
    obtain ⟨mir', hok, _, _⟩ := lowerCore_spec (profile := profile) hvalid
    rw [hok] at ran
    cases ran

/-- Constructive stage validation for `passes.lowerUncheckedEntry` (independent of
`registry`, zero `Classical.choice`). -/
theorem lowerUncheckedEntry_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.lowerUnchecked = lowerUnchecked) :
    passes.lowerUncheckedEntry.Validated := by
  constructor
  · intro input mir hunchecked ran
    change passes.lowerUnchecked input = .ok mir at ran
    rw [implementation] at ran
    obtain ⟨mir', hok, hwf, hlow⟩ := lowerUnchecked_spec (profile := profile) hunchecked
    cases Except.ok.inj (ran.symm.trans hok)
    exact ⟨hwf, hlow⟩
  · intro input failure hunchecked ran
    change passes.lowerUnchecked input = .error failure at ran
    rw [implementation] at ran
    obtain ⟨mir', hok, _, _⟩ := lowerUnchecked_spec (profile := profile) hunchecked
    rw [hok] at ran
    cases ran

/-- Universal stage validation for the unchecked `lowerUnchecked` edge in `Registry`. -/
theorem lowerUnchecked_validated {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.lowerUnchecked = lowerUnchecked) :
    (registry passes).lowerUnchecked.Validated :=
  lowerUncheckedEntry_validated passes implementation

/-- Full stage soundness (`Stage.Sound`) for the checked `lowerCore` edge. -/
theorem lowerCore_sound {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.lowerCore = lowerCore) :
    Stage.Sound passes .lowerCore :=
  ⟨lowerCore_validated passes implementation, fun _ _ _ _ => trivial⟩

/-- Full stage soundness (`Stage.Sound`) for the unchecked `lowerUnchecked` edge. -/
theorem lowerUnchecked_sound {slice : Slice} {profile : Profile} (passes : Passes slice profile)
    (implementation : passes.lowerUnchecked = lowerUnchecked) :
    Stage.Sound passes .lowerUnchecked :=
  ⟨lowerUnchecked_validated passes implementation, fun _ _ _ _ => trivial⟩

end Concrete.Core.LowerMIR
