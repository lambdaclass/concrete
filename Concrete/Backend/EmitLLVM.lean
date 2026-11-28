import Concrete.Interfaces.Pipeline
import Concrete.Backend.LLVMIR
import Concrete.Backend.Runtime

/-!
# Production two-module LLVM emitter

Lowers any well-formed `Concrete.Interfaces.BIR` program targeting an LLVM `Profile`
into a two-module `Concrete.Interfaces.LLVMInput`:

* **Component module (`LLVMInput.component`)**: structured `Concrete.Backend.LLVM.Item`
  list defining `define i32 @concrete_body()` with:
  - two-result `SSAInst.addWithOverflow` lowered via `@llvm.{s,u}add.with.overflow.i{bits}`
    and paired `extractvalue` instructions (`index 0` for wrapped sum, `index 1` for
    overflow flag), with deduplicated intrinsic declarations;
  - `SSAInst.op` for `.read` and `.arithmetic (.plain | .wrapping) (.add | .sub | .mul)`;
  - reachable-block pruning (so unreachable calls admitted by `BackendPolicy` are omitted),
    entry-loop trampoline (`Label.entry`) when the entry block has reachable predecessors,
    same-target parameterized branch edge splitting, `phi` nodes, and hosted `ret`/`trap`
    return-word encoding (`zext` to `i32` for `u8` returns, `256 + slot` for traps).
* **Launcher module (`LLVMInput.launcher`)**: exact fixed runtime entry produced by
  `Concrete.Backend.Runtime.llvmDiagnostics` from the ordered diagnostic lines of
  `bir.sites` resolved in `bir.project` via `TrapEntry.diagnosticLine`, establishing
  `LauncherMatches` and `LLVMMetadataMatches` directly. Target datalayout and triple
  headers are rendered exclusively from `profile` inside `LLVMInput.bytesAt`.

This module has zero imports of provisional `Concrete.BIR` or `Concrete.Backend.LLVM`.
All theorems are proved constructively without `Classical.choice` or `sorry`.
-/

namespace Concrete.Backend.EmitLLVM

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface
open Concrete.Interfaces
open Concrete.Backend.LLVM
open Concrete.Backend.Runtime (indexed)

/-- Abbreviation for a BIR CFG block. -/
abbrev BIRBlock {Ty : Type} (lang : LoweringLanguage Ty) :=
  Block (Sigma (SSAValue Ty)) (LocatedOp (SSAInst lang))
    (BIRTerm (Sigma (SSARead Ty lang.literal)) (SSARead Ty lang.literal lang.bool))

/-- Abbreviation for a BIR CFG edge carrying typed SSA reads. -/
abbrev BIREdge {Ty : Type} (lang : LoweringLanguage Ty) :=
  CFGEdge (Sigma (SSARead Ty lang.literal))

/-- Rule identifier cited when an off-domain (non-`BIR.WF`) input fails structural lowering. -/
def invalidInputRule : RuleId := ⟨``BIR.WF, 1⟩

/-- Rule identifier cited when `emit` is invoked directly on a non-LLVM profile. -/
def wrongBackendRule : RuleId := ⟨``BackendPolicy.Backend.llvm, 1⟩

/-! ## Diagnostics and launcher construction -/

/-- Resolve every trap entry in `bir.sites` to its canonical newline-terminated
diagnostic line in `bir.project`. -/
def siteDiagnostics {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    (bir : BIR lang profile) : Option (List String) :=
  bir.sites.toList.mapM fun entry => entry.diagnosticLine bir.project

/-- Build the fixed LLVM launcher module directly from the ordered trap diagnostic lines. -/
def launcherModule (diagnostics : List String) : LLVMModule :=
  ⟨Backend.Runtime.llvmDiagnostics diagnostics⟩

/-! ## Types, literals, and scalar operations -/

/-- Lower a Core language type to its LLVM `TypeCode`. -/
def llvmType (language : CoreLanguage) (ty : language.operands.Ty) : TypeCode :=
  match language.scalarType ty with
  | some (.int t) => .integer (Width.ofCore t)
  | some .bool => .flag
  | some .char => .integer .u32
  | some .unit | none => .integer .u8

/-- Map a supported Core `MachineOp` (plain or wrapping add/sub/mul) to its LLVM arithmetic op. -/
def arithmeticOfMachineOp : MachineOp → Option Arithmetic
  | .plain .add | .wrapping .add => some .add
  | .plain .sub | .wrapping .sub => some .sub
  | .plain .mul | .wrapping .mul => some .mul
  | _ => none

/-- Decode a Core literal in `language` into an LLVM immediate argument. -/
def decodeLiteralArg (language : CoreLanguage) {ty : language.operands.Ty}
    (lit : Literal language.operands.Ty ty) : Option Argument :=
  (language.literal ty lit.encoding).map fun v =>
    match language.valueShape ▸ v with
    | .scalar (.int t n) => .immediate (.natural (toBitsAtWidth (Width.ofCore t).bits n))
    | .scalar (.bool b) => .immediate (.natural (if b then 1 else 0))
    | .scalar (.char c) => .immediate (.natural c.val.toNat)
    | .scalar .unit | .other _ => .immediate (.natural 0)

/-- Lower a typed `SSARead` to an LLVM `Argument`. -/
def emitRead (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    {ty : language.operands.Ty} : SSARead language.operands.Ty lang.literal ty → Except StageFailure Argument
  | .value v => .ok (.local (.register v.id))
  | .literal lit =>
    match decodeLiteralArg language lit with
    | some arg => .ok arg
    | none => .error (.invalidInput invalidInputRule)

/-! ## Reachable-block normalization and edge-split planning -/

/-- Ordered list of blocks reachable from `graph.entry`, placing the entry block first
and preserving stored order for all other reachable blocks. -/
def orderedReachableBlocks {Ty : Type} {lang : LoweringLanguage Ty} (graph : BIRGraph lang) :
    Option (List (BIRBlock lang)) :=
  match graph.blocks.toList.find? (·.id == graph.entry) with
  | none => none
  | some entryBlk =>
    let rIds := BIR.reachableBlocks graph
    let rest := graph.blocks.toList.filter fun blk =>
      blk.id != graph.entry && rIds.contains blk.id
    some (entryBlk :: rest)

/-- Index lookup table mapping each reachable `BlockId` to its dense block label index. -/
def blockIndexTable {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : List (BIRBlock lang)) : AList BlockId Nat :=
  (indexed blocks).map fun (idx, blk) => (blk.id, idx)

/-- Whether any reachable block has an outgoing CFG edge targeting `targetId`. -/
def hasIncomingEdge {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : List (BIRBlock lang)) (targetId : BlockId) : Bool :=
  blocks.any fun blk => blk.ending.successors.any fun edge => edge.target == targetId

/-- Look up whether `targetId` in `blocks` declares at least one block parameter. -/
def targetHasParams {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : List (BIRBlock lang)) (targetId : BlockId) : Bool :=
  match blocks.find? (·.id == targetId) with
  | some tgt => !tgt.params.isEmpty
  | none => false

/-- Synthetic edge-split block created when a `branch` targets the same parameterized block
on both arms, giving LLVM `phi` nodes two distinct predecessor labels. -/
structure SplitEdge {Ty : Type} (lang : LoweringLanguage Ty) where
  predIndex : Nat
  splitIndex : Nat
  targetId : BlockId
  targetIndex : Nat
  arguments : List (Sigma (SSARead Ty lang.literal))

/-- Collect all synthetic split blocks needed for same-target parameterized branches. -/
def collectSplitEdges {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : List (BIRBlock lang)) (indices : AList BlockId Nat) :
    List (SplitEdge lang) :=
  let rec go : List (Nat × BIRBlock lang) → Nat → List (SplitEdge lang) → List (SplitEdge lang)
    | [], _, revAcc => revAcc.reverse
    | (j, blk) :: rest, nextIdx, revAcc =>
      match blk.ending with
      | .branch _ yes no =>
        if yes.target == no.target && targetHasParams blocks yes.target then
          match AList.lookup indices yes.target with
          | some tgtIdx =>
            go rest (nextIdx + 1) (⟨j, nextIdx, yes.target, tgtIdx, no.arguments⟩ :: revAcc)
          | none => go rest nextIdx revAcc
        else
          go rest nextIdx revAcc
      | _ => go rest nextIdx revAcc
  go (indexed blocks) blocks.length []

/-- Incoming edge record `(targetId, predecessorLabel, arguments)` for LLVM `phi` construction. -/
def incomingEdges {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : List (BIRBlock lang)) (splits : List (SplitEdge lang)) :
    List (BlockId × Label × List (Sigma (SSARead Ty lang.literal))) :=
  let direct := (indexed blocks).flatMap fun (j, blk) =>
    match blk.ending with
    | .jump edge => [(edge.target, Label.block j, edge.arguments)]
    | .branch _ yes no =>
      if yes.target == no.target && targetHasParams blocks yes.target then
        [(yes.target, Label.block j, yes.arguments)]
      else
        [(yes.target, Label.block j, yes.arguments), (no.target, Label.block j, no.arguments)]
    | .ret _ | .trap _ => []
  let fromSplits := splits.map fun sp => (sp.targetId, Label.block sp.splitIndex, sp.arguments)
  direct ++ fromSplits

/-! ## Instruction, phi, declaration, and terminator emission -/

/-- Emit zero-initialized definitions for any entry block parameters (unreachable on hosted
0-argument entry execution, retained so component emission is total on `BIR.WF`). -/
def emitEntryParamDefaults (language : CoreLanguage)
    (params : List (Sigma (SSAValue language.operands.Ty))) : List Item :=
  params.map fun ⟨pty, reg⟩ =>
    match llvmType language pty with
    | .flag => .instruction (.compare (.register reg.id) .eq .flag
        (.immediate (.natural 0)) (.immediate (.natural 1)))
    | tc => .instruction (.bin (.register reg.id) .add tc
        (.immediate (.natural 0)) (.immediate (.natural 0)))

/-- Emit LLVM `phi` instructions for block `blk` (skipping the entry block when it has no
incoming edges, and supplying a zero arm from `Label.entry` if an entry loop has parameters). -/
def emitPhis (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (edges : List (BlockId × Label × List (Sigma (SSARead language.operands.Ty lang.literal))))
    (isEntry : Bool) (entryHasPreds : Bool) (blk : BIRBlock lang) :
    Except StageFailure (List Item) := do
  if blk.params.isEmpty then
    return []
  if isEntry && !entryHasPreds then
    return emitEntryParamDefaults language blk.params
  let incoming := edges.filter fun e => e.1 == blk.id
  (indexed blk.params).mapM fun (k, ⟨pty, reg⟩) => do
    let predArms ← incoming.mapM fun (_, predLabel, args) => do
      match args[k]? with
      | some ⟨_, read⟩ =>
        let arg ← emitRead language read
        return (arg, predLabel)
      | none => throw (.invalidInput invalidInputRule)
    let arms := if isEntry && entryHasPreds then
        (.immediate (.natural 0), Label.entry) :: predArms
      else predArms
    return .instruction (.phi (.register reg.id) (llvmType language pty) arms)

/-- Emit LLVM instructions for one `SSAInst`. -/
def emitInst (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (inst : SSAInst lang) : Except StageFailure (List Item) :=
  match inst with
  | @SSAInst.op _ _ opcode _ ty dst _ args addresses =>
    if !addresses.isEmpty then
      .error (.invalidInput invalidInputRule)
    else match opcode with
      | .read =>
        match args with
        | .cons arg .nil => do
          let arg' ← emitRead language arg
          match llvmType language ty with
          | .flag => return [.instruction (.compare (.register dst.id) .eq .flag arg' (.immediate (.natural 1)))]
          | tc => return [.instruction (.bin (.register dst.id) .add tc arg' (.immediate (.natural 0)))]
        | _ => .error (.invalidInput invalidInputRule)
      | .arithmetic mOp =>
        match arithmeticOfMachineOp mOp with
        | none => .error (.invalidInput invalidInputRule)
        | some arith =>
          match language.scalarType ty with
          | some (.int t) =>
            match args with
            | .cons lhs r1 =>
              match r1 with
              | .cons rhs r2 =>
                match r2 with
                | .nil => do
                  let lhs' ← emitRead language lhs
                  let rhs' ← emitRead language rhs
                  let w := Width.ofCore t
                  return [.instruction (.bin (.register dst.id) arith (.integer w) lhs' rhs')]
                | .cons _ _ => .error (.invalidInput invalidInputRule)
              | .nil => .error (.invalidInput invalidInputRule)
            | .nil => .error (.invalidInput invalidInputRule)
          | _ => .error (.invalidInput invalidInputRule)
      | .named _ => .error (.invalidInput invalidInputRule)
  | .addWithOverflow t dst flag lhs rhs => do
    let lhs' ← emitRead language lhs
    let rhs' ← emitRead language rhs
    let w := Width.ofCore t
    let i := TypeCode.integer w
    let pair := TypeCode.overflowPair w
    return [.instruction (.call (some (.overflow dst.id)) pair (.overflow .add t) [(i, lhs'), (i, rhs')]),
            .instruction (.extract (.register dst.id) pair (.overflow dst.id) 0),
            .instruction (.extract (.register flag.id) pair (.overflow dst.id) 1)]
  | .load .. | .store .. | .beginLifetime _ | .endLifetime _ | .destructure .. | .call .. =>
    .error (.invalidInput invalidInputRule)

/-- Emit LLVM terminator items for block index `j`. -/
def emitTerm (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (indices : AList BlockId Nat) (splits : List (SplitEdge lang))
    (j : Nat) (ending : BIRTerm (Sigma (SSARead language.operands.Ty lang.literal))
      (SSARead language.operands.Ty lang.literal lang.bool)) :
    Except StageFailure (List Item) :=
  match ending with
  | .ret none =>
    .ok [.terminator (.ret (.integer .u32, .immediate (.natural 0)))]
  | .ret (some ⟨ty, arg⟩) => do
    let arg' ← emitRead language arg
    match llvmType language ty with
    | .integer .u32 =>
      return [.terminator (.ret (.integer .u32, arg'))]
    | .integer .u64 =>
      return [.instruction (.convert (.result j) .trunc (.integer .u64, arg') (.integer .u32)),
              .terminator (.ret (.integer .u32, .local (.result j)))]
    | .integer w =>
      return [.instruction (.convert (.result j) .zext (.integer w, arg') (.integer .u32)),
              .terminator (.ret (.integer .u32, .local (.result j)))]
    | .flag =>
      return [.instruction (.convert (.result j) .zext (.flag, arg') (.integer .u32)),
              .terminator (.ret (.integer .u32, .local (.result j)))]
    | _ =>
      return [.terminator (.ret (.integer .u32, .immediate (.natural 0)))]
  | .trap slot =>
    .ok [.terminator (.ret (.integer .u32, .immediate (.natural (toBits .u32 (Int.ofNat (256 + slot.index))))))]
  | .jump edge =>
    match AList.lookup indices edge.target with
    | some tgtIdx => .ok [.terminator (.branch (.block tgtIdx))]
    | none => .error (.invalidInput invalidInputRule)
  | .branch test yes no =>
    match AList.lookup indices yes.target, AList.lookup indices no.target with
    | some yesIdx, some rawNoIdx => do
      let noIdx := match splits.find? (·.predIndex == j) with
        | some sp => sp.splitIndex
        | none => rawNoIdx
      match test with
      | .value v =>
        return [.terminator (.cond (.register v.id) (.block yesIdx) (.block noIdx))]
      | .literal lit =>
        match decodeLiteralArg language lit with
        | some arg' =>
          return [.instruction (.compare (.result j) .eq .flag arg' (.immediate (.natural 1))),
                  .terminator (.cond (.result j) (.block yesIdx) (.block noIdx))]
        | none => throw (.invalidInput invalidInputRule)
    | _, _ => .error (.invalidInput invalidInputRule)

/-- Collect deduplicated overflow-add intrinsic integer types used in `blocks`. -/
def usedOverflowAddTypes {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : List (BIRBlock lang)) : List IntTy :=
  ((blocks.flatMap fun blk => blk.body.filterMap fun op =>
    match op.instruction with
    | .addWithOverflow t .. => some t
    | _ => none)).eraseDups

/-- Emit deduplicated LLVM intrinsic declarations for `@llvm.{s,u}add.with.overflow.i{bits}`. -/
def intrinsicDeclarations {Ty : Type} {lang : LoweringLanguage Ty}
    (blocks : List (BIRBlock lang)) : List Item :=
  (usedOverflowAddTypes blocks).map fun t =>
    let w := Width.ofCore t
    .declare (.overflowPair w) (.overflow .add t) [.integer w, .integer w]

/-- Emit the structured LLVM component module (`@concrete_body`) from a `BIRGraph`. -/
def emitComponentGraph (language : CoreLanguage) {lang : LoweringLanguage language.operands.Ty}
    (graph : BIRGraph lang) : Except StageFailure LLVMModule := do
  let some blocks := orderedReachableBlocks graph
    | throw (.invalidInput invalidInputRule)
  let indices := blockIndexTable blocks
  let entryHasPreds := hasIncomingEdge blocks graph.entry
  let splits := collectSplitEdges blocks indices
  let edges := incomingEdges blocks splits
  let blockItems ← (indexed blocks).mapM fun (j, blk) => do
    let phis ← emitPhis language edges (j == 0) entryHasPreds blk
    let insts ← blk.body.mapM fun op => emitInst language op.instruction
    let term ← emitTerm language indices splits j blk.ending
    return [Item.label (.block j)] ++ phis ++ insts.flatten ++ term
  let splitItems := splits.flatMap fun sp =>
    [Item.label (.block sp.splitIndex), .terminator (.branch (.block sp.targetIndex))]
  let decls := intrinsicDeclarations blocks
  let entryTrampoline :=
    if entryHasPreds then [Item.label .entry, .terminator (.branch (.block 0))] else []
  return ⟨decls ++ [.blank, .define false (.integer .u32) .body []] ++
    entryTrampoline ++ blockItems.flatten ++ splitItems ++ [.endFunction]⟩

/-- Production two-module LLVM emitter over frozen `BIR`.
Enforces `profile.backend = .llvm` and `firstUnsupported` before emitting the
component (`@concrete_body`) and launcher (`Backend.Runtime.llvmDiagnostics`) modules. -/
def emit {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} (bir : BIR lang profile) :
    Except StageFailure (LLVMInput profile) := do
  if profile.backend != .llvm then
    throw (.unsupportedProfile wrongBackendRule)
  match firstUnsupported (language := language) bir with
  | some cause => throw (.unsupportedFeature profile cause)
  | none =>
    let some diagnostics := siteDiagnostics bir
      | throw (.invalidInput invalidInputRule)
    let some (_, graph) := AList.lookup bir.functions bir.entry
      | throw (.invalidInput invalidInputRule)
    let component ← emitComponentGraph language graph
    return ⟨component, launcherModule diagnostics, bir.sites⟩

/-! ## Constructive structural and refusal theorems -/

private theorem list_mapM_option_some {α β : Type} {f : α → Option β} {xs : List α}
    (hAll : ∀ x ∈ xs, ∃ y, f x = some y) :
    ∃ ys, xs.mapM f = some ys := by
  induction xs with
  | nil => exact ⟨[], rfl⟩
  | cons a rest ih =>
    obtain ⟨b, hb⟩ := hAll a List.mem_cons_self
    obtain ⟨bs, hbs⟩ := ih fun x hx => hAll x (List.mem_cons_of_mem _ hx)
    refine ⟨b :: bs, ?_⟩
    simp only [List.mapM_cons, bind, Option.bind, pure, hb, hbs]

private theorem list_mapM_except_ok {α β ε : Type} {f : α → Except ε β} {xs : List α}
    (hAll : ∀ x ∈ xs, ∃ y, f x = .ok y) :
    ∃ ys, xs.mapM f = .ok ys := by
  induction xs with
  | nil => exact ⟨[], rfl⟩
  | cons a rest ih =>
    obtain ⟨b, hb⟩ := hAll a List.mem_cons_self
    obtain ⟨bs, hbs⟩ := ih fun x hx => hAll x (List.mem_cons_of_mem _ hx)
    refine ⟨b :: bs, ?_⟩
    simp only [List.mapM_cons, bind, Except.bind, pure, Except.pure, hb, hbs]

private theorem alist_lookup_mem_local {κ α : Type} [DecidableEq κ] {l : AList κ α}
    {k : κ} {v : α} (h : AList.lookup l k = some v) : (k, v) ∈ l := by
  induction l with
  | nil => cases h
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [AList.lookup] at h
    split at h
    · rename_i heq; subst heq; cases Option.some.inj h; exact List.mem_cons_self
    · exact List.mem_cons_of_mem _ (ih h)

private theorem alist_lookup_isSome_of_mem {κ α : Type} [DecidableEq κ] {l : AList κ α}
    {k : κ} {v : α} (h : (k, v) ∈ l) : ∃ v', AList.lookup l k = some v' := by
  induction l with
  | nil => cases h
  | cons hd tl ih =>
    rcases hd with ⟨k', v'⟩
    simp only [AList.lookup]
    split
    · exact ⟨v', rfl⟩
    · rename_i hne
      cases h with
      | head => exact absurd rfl hne
      | tail _ htl => exact ih htl

private theorem find?_some_of_mem {α : Type} {xs : List α} {p : α → Bool}
    (hEx : ∃ x ∈ xs, p x = true) :
    ∃ x, xs.find? p = some x ∧ x ∈ xs ∧ p x = true := by
  induction xs with
  | nil => obtain ⟨_, hm, _⟩ := hEx; cases hm
  | cons a rest ih =>
    simp only [List.find?]
    cases hpa : p a
    · obtain ⟨x, hx, hpx⟩ := hEx
      cases hx with
      | head => rw [hpa] at hpx; cases hpx
      | tail _ htl =>
        obtain ⟨y, hy, hymem, hpy⟩ := ih ⟨x, htl, hpx⟩
        exact ⟨y, hy, List.mem_cons_of_mem _ hymem, hpy⟩
    · exact ⟨a, rfl, List.mem_cons_self, hpa⟩

private theorem mem_indexed_left {α : Type} {l : List α} {p : Nat × α}
    (h : p ∈ indexed l) : p.2 ∈ l := by
  have go : ∀ n (xs : List α), p ∈ Backend.Runtime.indexedFrom n xs → p.2 ∈ xs := by
    intro n xs hm
    induction xs generalizing n with
    | nil => cases hm
    | cons a rest ih =>
      simp only [Backend.Runtime.indexedFrom, List.mem_cons] at hm
      rcases hm with rfl | htl
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (ih (n + 1) htl)
  exact go 0 l h

private theorem mem_indexed_exists {α : Type} {l : List α} {x : α} (h : x ∈ l) :
    ∃ n, (n, x) ∈ indexed l := by
  have go : ∀ k (xs : List α), x ∈ xs → ∃ n, (n, x) ∈ Backend.Runtime.indexedFrom k xs := by
    intro k xs hm
    induction xs generalizing k with
    | nil => cases hm
    | cons a rest ih =>
      cases hm with
      | head => exact ⟨k, List.mem_cons_self⟩
      | tail _ htl =>
        obtain ⟨n, hn⟩ := ih (k + 1) htl
        exact ⟨n, List.mem_cons_of_mem _ hn⟩
  exact go 0 l h

private theorem mem_zipIdx_of_mem {α : Type} {xs : List α} {x : α} (h : x ∈ xs) :
    ∃ idx, (x, idx) ∈ xs.zipIdx := by
  have go : ∀ k (ys : List α), x ∈ ys → ∃ idx, (x, idx) ∈ ys.zipIdx k := by
    intro k ys hm
    induction ys generalizing k with
    | nil => cases hm
    | cons a rest ih =>
      cases hm with
      | head => exact ⟨k, List.mem_cons_self⟩
      | tail _ htl =>
        obtain ⟨idx, hidx⟩ := ih (k + 1) htl
        exact ⟨idx, List.mem_cons_of_mem _ hidx⟩
  exact go 0 xs h

private theorem getElem?_some_of_lt {α : Type} {xs : List α} {k : Nat} (h : k < xs.length) :
    ∃ x, xs[k]? = some x ∧ x ∈ xs :=
  ⟨xs[k], List.getElem?_eq_getElem h, List.getElem_mem h⟩

private theorem getElem?_indexed_lt {α : Type} {xs : List α} {k : Nat} {x : α}
    (h : (k, x) ∈ indexed xs) : k < xs.length := by
  have go : ∀ n (ys : List α), (k, x) ∈ Backend.Runtime.indexedFrom n ys →
      n ≤ k ∧ k < n + ys.length := by
    intro n ys hm
    induction ys generalizing n with
    | nil => cases hm
    | cons a rest ih =>
      simp only [Backend.Runtime.indexedFrom, List.mem_cons, Prod.mk.injEq] at hm
      rcases hm with ⟨rfl, rfl⟩ | htl
      · exact ⟨Nat.le_refl _, by simp⟩
      · obtain ⟨hle, hlt⟩ := ih (n + 1) htl
        exact ⟨by omega, by simp; omega⟩
  have h0 := (go 0 xs h).2
  simpa using h0

/-- Every `BIR` whose trap entries all bind in `bir.project` resolves to exact diagnostic lines. -/
theorem siteDiagnostics_of_sites_bind {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {bir : BIR lang profile}
    (hBind : ∀ site ∈ bir.sites.toList, ∃ span, site.origin.lexical.Binds bir.project span) :
    ∃ diagnostics, siteDiagnostics bir = some diagnostics := by
  apply list_mapM_option_some
  intro entry hEntry
  obtain ⟨span, hSpan⟩ := hBind entry hEntry
  obtain ⟨file, _, hSite⟩ := runtimeSite_of_binds hSpan
  exact ⟨renderTrapLine entry.kind ⟨entry.origin.operation, ⟨file.path.render.text, entry.origin.lexical.line, entry.origin.lexical.col⟩⟩,
    by simp [TrapEntry.diagnosticLine, hSite]⟩

/-- Every successful `EmitLLVM.emit` implies `firstUnsupported = none`. -/
theorem emit_firstUnsupported_none {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {output : LLVMInput profile}
    (h : emit (language := language) bir = .ok output) :
    firstUnsupported (language := language) bir = none := by
  unfold emit at h
  cases hBack : (profile.backend != .llvm)
  · simp only [hBack, Bool.false_eq_true, ↓reduceIte] at h
    cases hSup : firstUnsupported (language := language) bir with
    | some cause =>
      simp only [hSup, throw, throwThe, MonadExceptOf.throw] at h
      cases h
    | none => rfl
  · simp only [hBack, ↓reduceIte, throw, throwThe, MonadExceptOf.throw] at h
    cases h

/-- Every successful `EmitLLVM.emit` satisfies `LauncherMatches`. -/
theorem emit_launcherMatches {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {output : LLVMInput profile}
    (h : emit (language := language) bir = .ok output) :
    LauncherMatches bir output := by
  unfold emit at h
  cases hBack : (profile.backend != .llvm)
  · simp only [hBack, Bool.false_eq_true, ↓reduceIte] at h
    cases hSup : firstUnsupported (language := language) bir with
    | some cause =>
      simp only [hSup, throw, throwThe, MonadExceptOf.throw] at h
      cases h
    | none =>
      simp only [hSup] at h
      cases hDiag : siteDiagnostics bir with
      | none =>
        simp only [hDiag, throw, throwThe, MonadExceptOf.throw] at h
        cases h
      | some diagnostics =>
        simp only [hDiag] at h
        cases hFn : AList.lookup bir.functions bir.entry with
        | none =>
          simp only [hFn, throw, throwThe, MonadExceptOf.throw] at h
          cases h
        | some fnPair =>
          rcases fnPair with ⟨locals, graph⟩
          simp only [hFn] at h
          cases hComp : emitComponentGraph language graph with
          | error e =>
            simp only [hComp, bind, Except.bind] at h
            cases h
          | ok component =>
            simp only [hComp, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at h
            subst h
            exact ⟨diagnostics, hDiag, rfl⟩
  · simp only [hBack, ↓reduceIte, throw, throwThe, MonadExceptOf.throw] at h
    cases h

/-- Every successful `EmitLLVM.emit` satisfies `LLVMMetadataMatches` (including `LauncherMatches`). -/
theorem emit_metadataMatches {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {output : LLVMInput profile}
    (h : emit (language := language) bir = .ok output) :
    LLVMMetadataMatches bir output := by
  have hLaunch := emit_launcherMatches h
  unfold emit at h
  cases hBack : (profile.backend != .llvm)
  · have hEq : profile.backend = .llvm := by
      cases hb : profile.backend
      · rfl
      · rw [hb] at hBack; cases hBack
    simp only [hBack, Bool.false_eq_true, ↓reduceIte] at h
    cases hSup : firstUnsupported (language := language) bir with
    | some cause =>
      simp only [hSup, throw, throwThe, MonadExceptOf.throw] at h
      cases h
    | none =>
      simp only [hSup] at h
      cases hDiag : siteDiagnostics bir with
      | none =>
        simp only [hDiag, throw, throwThe, MonadExceptOf.throw] at h
        cases h
      | some diagnostics =>
        simp only [hDiag] at h
        cases hFn : AList.lookup bir.functions bir.entry with
        | none =>
          simp only [hFn, throw, throwThe, MonadExceptOf.throw] at h
          cases h
        | some fnPair =>
          rcases fnPair with ⟨locals, graph⟩
          simp only [hFn] at h
          cases hComp : emitComponentGraph language graph with
          | error e =>
            simp only [hComp, bind, Except.bind] at h
            cases h
          | ok component =>
            simp only [hComp, bind, Except.bind, pure, Except.pure, Except.ok.injEq] at h
            subst h
            exact ⟨hEq, rfl, hLaunch⟩
  · simp only [hBack, ↓reduceIte, throw, throwThe, MonadExceptOf.throw] at h
    cases h

/-- When `firstUnsupported` detects an unsupported feature on an LLVM profile, `EmitLLVM.emit`
returns the exact `.unsupportedFeature profile cause` refusal. -/
theorem emit_refusal_of_firstUnsupported {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {cause : FeatureCause profile}
    (hBackend : profile.backend = .llvm)
    (hSup : firstUnsupported (language := language) bir = some cause) :
    emit (language := language) bir = .error (.unsupportedFeature profile cause) := by
  unfold emit
  simp [hBackend, hSup, throw, throwThe, MonadExceptOf.throw]

/-! ### Totality proof on `BIR.WF` + `firstUnsupported = none` -/

private theorem decodeLiteralArg_some_of_ok {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {ty : language.operands.Ty}
    {lit : Literal language.operands.Ty ty}
    (hOk : readLiteralOk language (lang := lang) ⟨ty, .literal lit⟩ = true) :
    ∃ arg, decodeLiteralArg language lit = some arg := by
  simp only [readLiteralOk] at hOk
  cases hl : language.literal ty lit.encoding with
  | none => rw [hl] at hOk; cases hOk
  | some v => exact ⟨_, by unfold decodeLiteralArg; rw [hl]; rfl⟩

private theorem emitRead_ok_of_literalOk {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {ty : language.operands.Ty}
    {read : SSARead language.operands.Ty lang.literal ty}
    (hOk : readLiteralOk language ⟨ty, read⟩ = true) :
    ∃ arg, emitRead language read = .ok arg := by
  cases read with
  | value v => exact ⟨.local (.register v.id), rfl⟩
  | literal lit =>
    obtain ⟨arg, harg⟩ := decodeLiteralArg_some_of_ok hOk
    exact ⟨arg, by simp [emitRead, harg]⟩

private theorem mem_reachableBlocks_entry {Ty : Type} {lang : LoweringLanguage Ty}
    {graph : BIRGraph lang} {entryBlk : BIRBlock lang}
    (hMem : entryBlk ∈ graph.blocks.toList) (hId : entryBlk.id = graph.entry) :
    graph.entry ∈ BIR.reachableBlocks graph := by
  unfold BIR.reachableBlocks
  rw [mem_reachGraph_iff]
  exact .refl (List.mem_map.mpr ⟨entryBlk, hMem, hId⟩) (by intro h; cases h)

private theorem mem_reachableBlocks_succ {Ty : Type} {lang : LoweringLanguage Ty}
    {graph : BIRGraph lang} {blk tgt : BIRBlock lang} {edge : BIREdge lang}
    (hReach : blk.id ∈ BIR.reachableBlocks graph)
    (hBlk : blk ∈ graph.blocks.toList) (hEdge : edge ∈ blk.ending.successors)
    (hTgt : tgt ∈ graph.blocks.toList) (hTgtId : tgt.id = edge.target) :
    edge.target ∈ BIR.reachableBlocks graph := by
  unfold BIR.reachableBlocks at hReach ⊢
  rw [mem_reachGraph_iff] at hReach ⊢
  refine .snoc hReach ?_ (List.mem_map.mpr ⟨tgt, hTgt, hTgtId⟩) (by intro h; cases h)
  exact List.mem_flatMap.mpr ⟨blk, hBlk, List.mem_map.mpr ⟨edge, hEdge, rfl⟩⟩

private theorem mem_reachableFunctions_entry {Ty : Type} {lang : LoweringLanguage Ty}
    {profile : Profile} {bir : BIR lang profile}
    (hEntry : bir.entry ∈ List.map Prod.fst bir.functions) :
    bir.entry ∈ bir.reachableFunctions := by
  unfold BIR.reachableFunctions
  rw [mem_reachGraph_iff]
  exact .refl hEntry (by intro h; cases h)

private theorem block_in_orderedReachable {Ty : Type} {lang : LoweringLanguage Ty}
    {graph : BIRGraph lang} {blocks : List (BIRBlock lang)}
    (hOrd : orderedReachableBlocks graph = some blocks)
    (hNodup : (graph.blocks.toList.map Block.id).Nodup)
    {blk : BIRBlock lang} (hBlk : blk ∈ graph.blocks.toList)
    (hReach : blk.id ∈ BIR.reachableBlocks graph) :
    blk ∈ blocks := by
  unfold orderedReachableBlocks at hOrd
  cases hFind : graph.blocks.toList.find? (·.id == graph.entry) with
  | none => rw [hFind] at hOrd; cases hOrd
  | some entryBlk =>
    rw [hFind] at hOrd
    cases Option.some.inj hOrd
    by_cases hEq : blk.id = graph.entry
    · have hEntryMem : entryBlk ∈ graph.blocks.toList := List.mem_of_find?_eq_some hFind
      have hEntryId : entryBlk.id = graph.entry := eq_of_beq (by simpa using List.find?_some hFind)
      have hBlkEq : blk = entryBlk := by
        have go : ∀ (xs : List (BIRBlock lang)), (xs.map Block.id).Nodup →
            blk ∈ xs → entryBlk ∈ xs → blk.id = entryBlk.id → blk = entryBlk := by
          intro xs hnd h1 h2 hid
          induction xs with
          | nil => cases h1
          | cons a rest ih =>
            rw [List.map_cons, List.nodup_cons] at hnd
            cases h1 with
            | head =>
              cases h2 with
              | head => rfl
              | tail _ h2t => exact absurd (hid ▸ List.mem_map.mpr ⟨entryBlk, h2t, rfl⟩) hnd.1
            | tail _ h1t =>
              cases h2 with
              | head => exact absurd (hid.symm ▸ List.mem_map.mpr ⟨blk, h1t, rfl⟩) hnd.1
              | tail _ h2t => exact ih hnd.2 h1t h2t
        exact go graph.blocks.toList hNodup hBlk hEntryMem (hEq.trans hEntryId.symm)
      subst hBlkEq
      exact List.mem_cons_self
    · apply List.mem_cons_of_mem
      exact List.mem_filter.mpr ⟨hBlk, by
        simp only [Bool.and_eq_true, bne_iff_ne, ne_eq, List.contains_eq_mem, decide_eq_true_eq]
        exact ⟨hEq, hReach⟩⟩

private theorem orderedReachable_mem_subset {Ty : Type} {lang : LoweringLanguage Ty}
    {graph : BIRGraph lang} {blocks : List (BIRBlock lang)}
    (hOrd : orderedReachableBlocks graph = some blocks)
    {blk : BIRBlock lang} (hBlk : blk ∈ blocks) :
    blk ∈ graph.blocks.toList ∧ blk.id ∈ BIR.reachableBlocks graph := by
  unfold orderedReachableBlocks at hOrd
  cases hFind : graph.blocks.toList.find? (·.id == graph.entry) with
  | none => rw [hFind] at hOrd; cases hOrd
  | some entryBlk =>
    rw [hFind] at hOrd
    cases Option.some.inj hOrd
    cases hBlk with
    | head =>
      have hMem := List.mem_of_find?_eq_some hFind
      have hId : blk.id = graph.entry := eq_of_beq (by simpa using List.find?_some hFind)
      exact ⟨hMem, hId ▸ mem_reachableBlocks_entry hMem hId⟩
    | tail _ hTail =>
      have hf := List.mem_filter.mp hTail
      simp only [Bool.and_eq_true, bne_iff_ne, List.contains_eq_mem, decide_eq_true_eq] at hf
      exact ⟨hf.1, hf.2.2⟩

private theorem emitInst_ok_of_supported {language : CoreLanguage}
    {lang : LoweringLanguage language.operands.Ty} {profile : Profile}
    {bir : BIR lang profile} {fn : FnId} {locals : LocalTypes language.operands.Ty}
    {graph : BIRGraph lang}
    {blk : BIRBlock lang} {op : LocatedOp (SSAInst lang)} {idx : Nat}
    (hFn : (fn, locals, graph) ∈ bir.functions)
    (hEntryReach : fn ∈ bir.reachableFunctions)
    (hBlk : blk ∈ graph.blocks.toList)
    (hBlkReach : blk.id ∈ BIR.reachableBlocks graph)
    (hOp : (op, idx) ∈ blk.body.zipIdx)
    (hLocal : instLocalOk language bir.declarations profile.memoryTarget bir.signatures locals op.instruction = true)
    (hAllSup : ∀ cause ∈ bir.featureCandidates (language := language),
      BackendPolicy.FeatureSupported profile.policyProfile cause.feature) :
    ∃ items, emitInst language op.instruction = .ok items := by
  have hReads : ∀ r ∈ op.instruction.reads, readLiteralOk language r = true := by
    have h := (Bool.and_eq_true_iff.mp hLocal).1
    exact List.all_eq_true.mp h
  have hBodyLocal := (Bool.and_eq_true_iff.mp hLocal).2
  have hER : (bir.reachableFunctions.contains fn && (BIR.reachableBlocks graph).contains blk.id) = true := by
    simp only [Bool.and_eq_true, List.contains_eq_mem, decide_eq_true_eq]
    exact ⟨hEntryReach, hBlkReach⟩
  let wCand : language.operands.Ty → List (FeatureCause profile) := fun ty =>
    match language.scalarType ty with
    | some (.int intTy) => [.unsupportedWidth intTy fn (.instruction blk.id idx)]
    | _ => []
  have hInstIn : ∀ c : FeatureCause profile,
      c ∈ ((match op.instruction with
        | @SSAInst.op _ _ opcode types ty _ _ _ addresses =>
          (if addresses.isEmpty then [.unsupportedStorage .registerOnly fn (.instruction blk.id idx)]
           else [.unsupportedStorage .addressOperand fn (.instruction blk.id idx)]) ++
          [.unsupportedOperation (Operator.policyOperation opcode) fn (.instruction blk.id idx)] ++
          (wCand ty ++ types.flatMap wCand ++ addresses.flatMap (fun p => wCand p.1))
        | .addWithOverflow t _ _ _ _ =>
          [.unsupportedOperation .addWithOverflow fn (.instruction blk.id idx),
           .unsupportedWidth t fn (.instruction blk.id idx)]
        | .load (ty := ty) _ _ =>
          .unsupportedStorage .load fn (.instruction blk.id idx) :: wCand ty
        | .store (ty := ty) _ _ =>
          .unsupportedStorage .store fn (.instruction blk.id idx) :: wCand ty
        | .beginLifetime _ => [.unsupportedStorage .beginLifetime fn (.instruction blk.id idx)]
        | .endLifetime _ => [.unsupportedStorage .endLifetime fn (.instruction blk.id idx)]
        | .destructure (ty := ty) src bindings =>
          let form := match src with
            | .register _ => BackendPolicy.StorageForm.destructureRegister
            | .materialized _ => .destructureMaterialized
          .unsupportedStorage form fn (.instruction blk.id idx) ::
            (wCand ty ++ bindings.flatMap (fun (_, ⟨fty, _⟩) => wCand fty))
        | .call callee dst args =>
          let form :=
            if !(bir.reachableFunctions.contains fn && (BIR.reachableBlocks graph).contains blk.id) then
              BackendPolicy.CallForm.unreachableCall
            else if (AList.lookup bir.functions callee).isSome then .reachableInternalCall
            else .foreignCall
          .unsupportedCall form (some callee) fn (.instruction blk.id idx) ::
            (dst.toList.flatMap (fun d => wCand d.1) ++ args.flatMap (fun a => wCand a.1))) :
        List (FeatureCause profile)) →
      BackendPolicy.FeatureSupported profile.policyProfile c.feature := by
    intro c hc
    apply hAllSup
    unfold BIR.featureCandidates
    refine List.mem_flatMap.mpr ⟨(fn, locals, graph), hFn, ?_⟩
    apply List.mem_append_right
    refine List.mem_flatMap.mpr ⟨blk, hBlk, ?_⟩
    apply List.mem_append_left
    apply List.mem_append_right
    exact List.mem_flatMap.mpr ⟨(op, idx), hOp, hc⟩
  cases hInst : op.instruction with
  | op dst fop args addresses =>
    rename_i opcode types ty
    rw [hInst] at hReads hBodyLocal hInstIn
    have hSig := (Bool.and_eq_true_iff.mp hBodyLocal).1
    have hOpSup := hInstIn (.unsupportedOperation (Operator.policyOperation opcode) fn (.instruction blk.id idx))
      (by simp)
    have hAddrEmpty : addresses.isEmpty = true := by
      cases hAe : addresses.isEmpty
      · have hBad := hInstIn (.unsupportedStorage .addressOperand fn (.instruction blk.id idx))
          (by simp [hAe])
        cases hBad
      · rfl
    cases opcode with
    | read =>
      simp only [opcodeSignatureOk, Bool.and_eq_true] at hSig
      cases types with
      | nil => simp [hAddrEmpty] at hSig
      | cons t0 rest =>
        cases rest with
        | nil =>
          cases args with
          | cons arg restArgs =>
            cases restArgs with
            | nil =>
              obtain ⟨arg', hArg⟩ := emitRead_ok_of_literalOk (hReads ⟨t0, arg⟩ List.mem_cons_self)
              unfold emitInst
              simp only [hAddrEmpty, Bool.not_true, Bool.false_eq_true, ↓reduceIte, hArg,
                bind, Except.bind, pure, Except.pure]
              cases llvmType language ty <;> exact ⟨_, rfl⟩
        | cons _ _ => simp [hAddrEmpty] at hSig
    | arithmetic mOp =>
      have hArith : ∃ arith, arithmeticOfMachineOp mOp = some arith := by
        cases mOp with
        | plain ao =>
          cases ao with
          | add => exact ⟨.add, rfl⟩
          | sub => exact ⟨.sub, rfl⟩
          | mul => exact ⟨.mul, rfl⟩
          | div | rem | shl | shr =>
            cases hOpSup
        | wrapping wo =>
          cases wo with
          | add => exact ⟨.add, rfl⟩
          | sub => exact ⟨.sub, rfl⟩
          | mul => exact ⟨.mul, rfl⟩
        | saturating _ =>
          cases hOpSup
      obtain ⟨arith, hArithEq⟩ := hArith
      simp only [opcodeSignatureOk, Bool.and_eq_true] at hSig
      cases types with
      | nil => simp [hAddrEmpty] at hSig
      | cons t0 rest =>
        cases rest with
        | nil => simp [hAddrEmpty] at hSig
        | cons t1 rest2 =>
          cases rest2 with
          | nil =>
            have hIsInt : isIntType language ty = true := by
              simp [hAddrEmpty] at hSig
              exact hSig.2.1.1
            cases hSc : language.scalarType ty with
            | none => simp [isIntType, hSc] at hIsInt
            | some ct =>
              cases ct with
              | int t =>
                cases args with
                | cons lhs r1 =>
                  cases r1 with
                  | cons rhs r2 =>
                    cases r2 with
                    | nil =>
                      obtain ⟨lhs', hLhs⟩ := emitRead_ok_of_literalOk (hReads ⟨t0, lhs⟩ List.mem_cons_self)
                      obtain ⟨rhs', hRhs⟩ := emitRead_ok_of_literalOk
                        (hReads ⟨t1, rhs⟩ (List.mem_cons_of_mem _ List.mem_cons_self))
                      unfold emitInst
                      simp only [hAddrEmpty, Bool.not_true, Bool.false_eq_true, ↓reduceIte,
                        hArithEq, hSc, hLhs, hRhs, bind, Except.bind, pure, Except.pure]
                      exact ⟨_, rfl⟩
              | bool | char | unit => simp [isIntType, hSc] at hIsInt
          | cons _ _ => simp [hAddrEmpty] at hSig
    | named _ =>
      cases hOpSup
  | addWithOverflow t dst flag lhs rhs =>
    rw [hInst] at hReads
    obtain ⟨lhs', hLhs⟩ := emitRead_ok_of_literalOk (hReads ⟨lang.intTy t, lhs⟩ List.mem_cons_self)
    obtain ⟨rhs', hRhs⟩ := emitRead_ok_of_literalOk
      (hReads ⟨lang.intTy t, rhs⟩ (List.mem_cons_of_mem _ List.mem_cons_self))
    unfold emitInst
    simp only [hLhs, hRhs, bind, Except.bind, pure, Except.pure]
    exact ⟨_, rfl⟩
  | load _ _ =>
    rw [hInst] at hInstIn
    exact absurd (hInstIn (.unsupportedStorage .load fn (.instruction blk.id idx)) List.mem_cons_self)
      (by intro h; cases h)
  | store _ _ =>
    rw [hInst] at hInstIn
    exact absurd (hInstIn (.unsupportedStorage .store fn (.instruction blk.id idx)) List.mem_cons_self)
      (by intro h; cases h)
  | beginLifetime _ =>
    rw [hInst] at hInstIn
    exact absurd (hInstIn (.unsupportedStorage .beginLifetime fn (.instruction blk.id idx)) List.mem_cons_self)
      (by intro h; cases h)
  | endLifetime _ =>
    rw [hInst] at hInstIn
    exact absurd (hInstIn (.unsupportedStorage .endLifetime fn (.instruction blk.id idx)) List.mem_cons_self)
      (by intro h; cases h)
  | destructure src _ =>
    rw [hInst] at hInstIn
    cases src with
    | register _ =>
      exact absurd (hInstIn (.unsupportedStorage .destructureRegister fn (.instruction blk.id idx)) List.mem_cons_self)
        (by intro h; cases h)
    | materialized _ =>
      exact absurd (hInstIn (.unsupportedStorage .destructureMaterialized fn (.instruction blk.id idx)) List.mem_cons_self)
        (by intro h; cases h)
  | call callee _ _ =>
    rw [hInst] at hInstIn
    cases hCal : (AList.lookup bir.functions callee).isSome with
    | true =>
      have hBad := hInstIn (.unsupportedCall .reachableInternalCall (some callee) fn (.instruction blk.id idx)) (by
        dsimp only
        rw [hER, hCal]
        exact List.mem_cons_self)
      cases hBad
    | false =>
      have hBad := hInstIn (.unsupportedCall .foreignCall (some callee) fn (.instruction blk.id idx)) (by
        dsimp only
        rw [hER, hCal]
        exact List.mem_cons_self)
      cases hBad

/-- Universal totality theorem for `EmitLLVM.emit`: every well-formed `BIR` targeting an LLVM
profile with `firstUnsupported = none` succeeds and satisfies `LLVMMetadataMatches` (including
`LauncherMatches`). -/
theorem emit_ok_of_wf {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {bir : BIR lang profile}
    (hBackend : profile.backend = .llvm)
    (hWF : BIR.WF (language := language) bir)
    (hSup : firstUnsupported (language := language) bir = none) :
    ∃ output, emit (language := language) bir = .ok output ∧
      LLVMMetadataMatches bir output := by
  obtain ⟨diagnostics, hDiag⟩ := siteDiagnostics_of_sites_bind hWF.sites_bind
  have hEntryMem := hWF.registerFormation.entry_mem
  have hEntryLook : ∃ entryVal, AList.lookup bir.functions bir.entry = some entryVal := by
    obtain ⟨⟨fn, locals, graph⟩, hMem, hEq⟩ := List.mem_map.mp hEntryMem
    dsimp only at hEq
    subst hEq
    exact alist_lookup_isSome_of_mem hMem
  obtain ⟨⟨locals, graph⟩, hFnLook⟩ := hEntryLook
  have hFnMem : (bir.entry, locals, graph) ∈ bir.functions := alist_lookup_mem_local hFnLook
  have hNormMem : (bir.entry, locals, (⟨graph.entry, graph.blocks.toList.map fun b =>
      ⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending, b.boundaryOrigins⟩⟩ :
      NormalizedCFG lang)) ∈ (RegisterIR.bir bir).normalize.functions := by
    unfold RegisterIR.normalize
    exact List.mem_map.mpr ⟨(bir.entry, locals, graph), hFnMem, rfl⟩
  have hFnFormed := hWF.registerFormation.2.2.2.2.2.2.2.2.2.2 _ hNormMem
  have hNodesNodup : (graph.blocks.toList.map Block.id).Nodup := by
    have hnd := hFnFormed.2.2.1
    rw [List.map_map] at hnd
    exact hnd
  obtain ⟨sig, _, ⟨normEntry, hNormEntryMem, hNormEntryId, _⟩, _, _, hNormBlks⟩ := hFnFormed.2.2.2
  obtain ⟨entryBlk, hEntryBlkMem, hEntryBlkEq⟩ := List.mem_map.mp hNormEntryMem
  have hEntryId : entryBlk.id = graph.entry := by
    have h1 := congrArg Block.id hEntryBlkEq
    exact h1.trans hNormEntryId
  obtain ⟨foundEntry, hFindEntry, _, _⟩ :=
    find?_some_of_mem (p := (·.id == graph.entry)) ⟨entryBlk, hEntryBlkMem, beq_iff_eq.mpr hEntryId⟩
  let blocks := foundEntry :: (graph.blocks.toList.filter fun blk =>
    blk.id != graph.entry && (BIR.reachableBlocks graph).contains blk.id)
  have hOrd : orderedReachableBlocks graph = some blocks := by
    simp [orderedReachableBlocks, hFindEntry, blocks]
  have hBlkSpec : ∀ blk ∈ graph.blocks.toList,
      (∀ edge ∈ blk.ending.successors,
        ∃ tgt ∈ graph.blocks.toList, tgt.id = edge.target ∧
          edge.arguments.map Sigma.fst = tgt.params.map Sigma.fst) ∧
      (∀ r ∈ blk.ending.regReads, readLiteralOk language r = true) ∧
      (∀ pair ∈ blk.body.zipIdx,
        instLocalOk language bir.declarations profile.memoryTarget bir.signatures locals pair.1.instruction = true) := by
    intro blk hBlk
    have hnb : (⟨blk.id, blk.params, blk.body, RegTerminator.ofBIR bir.sites.size blk.ending,
        blk.boundaryOrigins⟩ : Block _ _ (RegTerminator lang)) ∈
        graph.blocks.toList.map (fun b =>
          ⟨b.id, b.params, b.body, RegTerminator.ofBIR bir.sites.size b.ending, b.boundaryOrigins⟩) :=
      List.mem_map.mpr ⟨blk, hBlk, rfl⟩
    obtain ⟨hSucc, _, hTermReads, _, hBody⟩ := hNormBlks _ hnb
    refine ⟨?_, ?_, ?_⟩
    · intro edge hEdge
      have hEdgeNorm : edge ∈ (RegTerminator.ofBIR bir.sites.size blk.ending).successors := by
        cases hEnd : blk.ending with
        | jump e => rw [hEnd] at hEdge; exact hEdge
        | branch _ y n => rw [hEnd] at hEdge; exact hEdge
        | ret _ => rw [hEnd] at hEdge; cases hEdge
        | trap _ => rw [hEnd] at hEdge; cases hEdge
      obtain ⟨ntgt, hntgt, hntgtId, hntgtArgs⟩ := hSucc edge hEdgeNorm
      obtain ⟨tgt, hTgtMem, hTgtEq⟩ := List.mem_map.mp hntgt
      refine ⟨tgt, hTgtMem, ?_, ?_⟩
      · exact (congrArg Block.id hTgtEq).trans hntgtId
      · exact hntgtArgs.trans (congrArg (List.map Sigma.fst) (congrArg Block.params hTgtEq).symm)
    · intro r hr
      have hrNorm : r ∈ (RegTerminator.ofBIR bir.sites.size blk.ending).reads := by
        cases hEnd : blk.ending with
        | jump e => rw [hEnd] at hr; exact hr
        | branch _ y n => rw [hEnd] at hr; exact hr
        | ret v => cases v <;> (rw [hEnd] at hr; exact hr)
        | trap _ => rw [hEnd] at hr; cases hr
      exact hTermReads r hrNorm
    · intro pair hPair
      exact (hBody pair hPair).1
  have hAllSup := (firstUnsupported_none_iff (language := language) bir).mp hSup
  have hEntryReach := mem_reachableFunctions_entry hEntryMem
  let indices := blockIndexTable blocks
  let entryHasPreds := hasIncomingEdge blocks graph.entry
  let splits := collectSplitEdges blocks indices
  let edges := incomingEdges blocks splits
  have hIndexOfMem : ∀ blk ∈ blocks, ∃ idx, AList.lookup indices blk.id = some idx := by
    intro blk hBlk
    obtain ⟨n, hn⟩ := mem_indexed_exists hBlk
    exact alist_lookup_isSome_of_mem (List.mem_map.mpr ⟨(n, blk), hn, rfl⟩)
  have hSuccIndex : ∀ blk ∈ blocks, ∀ edge ∈ blk.ending.successors,
      ∃ idx, AList.lookup indices edge.target = some idx := by
    intro blk hBlk edge hEdge
    obtain ⟨hBlkGraph, hBlkReach⟩ := orderedReachable_mem_subset hOrd hBlk
    obtain ⟨tgt, hTgtGraph, hTgtId, _⟩ := (hBlkSpec blk hBlkGraph).1 edge hEdge
    have hTgtReach := mem_reachableBlocks_succ hBlkReach hBlkGraph hEdge hTgtGraph hTgtId
    have hTgtMem := block_in_orderedReachable hOrd hNodesNodup hTgtGraph (hTgtId.symm ▸ hTgtReach)
    rw [← hTgtId]
    exact hIndexOfMem tgt hTgtMem
  have hSplitEdgeOrigin : ∀ sp ∈ splits,
      ∃ blk ∈ blocks, ∃ edge ∈ blk.ending.successors,
        edge.target = sp.targetId ∧ edge.arguments = sp.arguments := by
    have go : ∀ (xs : List (Nat × BIRBlock lang)) (nextIdx : Nat) (revAcc : List (SplitEdge lang)),
        (∀ x ∈ xs, x.2 ∈ blocks) →
        (∀ sp ∈ revAcc, ∃ blk ∈ blocks, ∃ edge ∈ blk.ending.successors,
          edge.target = sp.targetId ∧ edge.arguments = sp.arguments) →
        ∀ sp ∈ collectSplitEdges.go blocks indices xs nextIdx revAcc,
          ∃ blk ∈ blocks, ∃ edge ∈ blk.ending.successors,
            edge.target = sp.targetId ∧ edge.arguments = sp.arguments := by
      intro xs
      induction xs with
      | nil =>
        intro nextIdx revAcc _ hAcc sp hsp
        exact hAcc sp (List.mem_reverse.mp hsp)
      | cons pair rest ih =>
        intro nextIdx revAcc hXs hAcc sp hsp
        rcases pair with ⟨j, blk⟩
        have hBlkMem : blk ∈ blocks := hXs (j, blk) List.mem_cons_self
        have hRestMem : ∀ x ∈ rest, x.2 ∈ blocks := fun x hx => hXs x (List.mem_cons_of_mem _ hx)
        unfold collectSplitEdges.go at hsp
        cases hEnd : blk.ending with
        | jump _ | ret _ | trap _ =>
          rw [hEnd] at hsp
          exact ih nextIdx revAcc hRestMem hAcc sp hsp
        | branch test yes no =>
          rw [hEnd] at hsp
          dsimp only at hsp
          split at hsp
          · rename_i hCond
            have hTgtEq : yes.target = no.target := eq_of_beq (Bool.and_eq_true_iff.mp hCond).1
            cases hLk : AList.lookup indices yes.target with
            | none =>
              rw [hLk] at hsp
              exact ih nextIdx revAcc hRestMem hAcc sp hsp
            | some tgtIdx =>
              rw [hLk] at hsp
              apply ih (nextIdx + 1) _ hRestMem _ sp hsp
              intro sp' hsp'
              cases hsp' with
              | head =>
                refine ⟨blk, hBlkMem, no, ?_, hTgtEq.symm, rfl⟩
                simp [hEnd, BIRTerm.successors]
              | tail _ htl => exact hAcc sp' htl
          · exact ih nextIdx revAcc hRestMem hAcc sp hsp
    exact go (indexed blocks) blocks.length [] (fun _ hx => mem_indexed_left hx) (fun _ hm => nomatch hm)
  have hEdgeOrigin : ∀ e ∈ edges,
      ∃ blk ∈ blocks, ∃ edge ∈ blk.ending.successors,
        edge.target = e.1 ∧ edge.arguments = e.2.2 := by
    intro e he
    rcases List.mem_append.mp he with hDir | hSp
    · obtain ⟨⟨j, blk⟩, hIdx, hIn⟩ := List.mem_flatMap.mp hDir
      have hBlkMem : blk ∈ blocks := mem_indexed_left hIdx
      dsimp only at hIn
      cases hEnd : blk.ending with
      | jump edge =>
        rw [hEnd] at hIn
        cases List.mem_singleton.mp hIn
        exact ⟨blk, hBlkMem, edge, by simp [hEnd, BIRTerm.successors], rfl, rfl⟩
      | branch test yes no =>
        rw [hEnd] at hIn
        dsimp only at hIn
        split at hIn
        · cases List.mem_singleton.mp hIn
          exact ⟨blk, hBlkMem, yes, by simp [hEnd, BIRTerm.successors], rfl, rfl⟩
        · simp only [List.mem_cons, List.not_mem_nil, or_false] at hIn
          rcases hIn with hEq | hEq
          · cases hEq; exact ⟨blk, hBlkMem, yes, by simp [hEnd, BIRTerm.successors], rfl, rfl⟩
          · cases hEq; exact ⟨blk, hBlkMem, no, by simp [hEnd, BIRTerm.successors], rfl, rfl⟩
      | ret _ => rw [hEnd] at hIn; cases hIn
      | trap _ => rw [hEnd] at hIn; cases hIn
    · obtain ⟨sp, hSpMem, rfl⟩ := List.mem_map.mp hSp
      exact hSplitEdgeOrigin sp hSpMem
  have hBlockItemsOk : ∃ items,
      (indexed blocks).mapM (fun (x : Nat × BIRBlock lang) => do
        let phis ← emitPhis language edges (x.1 == 0) entryHasPreds x.2
        let insts ← x.2.body.mapM fun (op : LocatedOp (SSAInst lang)) => emitInst language op.instruction
        let term ← emitTerm language indices splits x.1 x.2.ending
        return [Item.label (.block x.1)] ++ phis ++ insts.flatten ++ term) = .ok items := by
    apply list_mapM_except_ok
    intro pair hPair
    rcases pair with ⟨j, blk⟩
    have hBlkMem : blk ∈ blocks := mem_indexed_left hPair
    obtain ⟨hBlkGraph, hBlkReach⟩ := orderedReachable_mem_subset hOrd hBlkMem
    have hPhisOk : ∃ phis, emitPhis language edges (j == 0) entryHasPreds blk = .ok phis := by
      unfold emitPhis
      by_cases hEmpty : blk.params.isEmpty = true
      · simp [hEmpty, pure, Except.pure]
      · simp only [hEmpty, Bool.false_eq_true, ↓reduceIte]
        by_cases hEnt : ((j == 0) && !entryHasPreds) = true
        · simp [hEnt, pure, Except.pure]
        · simp only [hEnt, Bool.false_eq_true, ↓reduceIte]
          apply list_mapM_except_ok
          intro pPair hPPair
          rcases pPair with ⟨k, ⟨pty, reg⟩⟩
          have hkLt : k < blk.params.length := getElem?_indexed_lt hPPair
          have hArmsOk : ∃ predArms,
              (edges.filter fun e => e.1 == blk.id).mapM (fun (_, predLabel, args) => do
                match args[k]? with
                | some ⟨_, read⟩ =>
                  let arg ← emitRead language read
                  return (arg, predLabel)
                | none => throw (StageFailure.invalidInput invalidInputRule)) = .ok predArms := by
            apply list_mapM_except_ok
            intro eEntry hEEntry
            rcases eEntry with ⟨tgtId, predLabel, args⟩
            have hf := List.mem_filter.mp hEEntry
            have hTgtEq : tgtId = blk.id := eq_of_beq hf.2
            obtain ⟨srcBlk, hSrcMem, edge, hEdge, hEdgeTgt, hEdgeArgs⟩ := hEdgeOrigin _ hf.1
            dsimp only at hEdgeTgt hEdgeArgs
            obtain ⟨hSrcGraph, _⟩ := orderedReachable_mem_subset hOrd hSrcMem
            obtain ⟨tgtBlk, hTgtGraph, hTgtId, hArgTypes⟩ := (hBlkSpec srcBlk hSrcGraph).1 edge hEdge
            have hSameTgt : tgtBlk = blk := by
              have hIdEq : tgtBlk.id = blk.id := hTgtId.trans (hEdgeTgt.trans hTgtEq)
              have go : ∀ (xs : List (BIRBlock lang)), (xs.map Block.id).Nodup →
                  tgtBlk ∈ xs → blk ∈ xs → tgtBlk.id = blk.id → tgtBlk = blk := by
                intro xs hnd h1 h2 hid
                induction xs with
                | nil => cases h1
                | cons a rest ih =>
                  rw [List.map_cons, List.nodup_cons] at hnd
                  cases h1 with
                  | head =>
                    cases h2 with
                    | head => rfl
                    | tail _ h2t => exact absurd (hid ▸ List.mem_map.mpr ⟨blk, h2t, rfl⟩) hnd.1
                  | tail _ h1t =>
                    cases h2 with
                    | head => exact absurd (hid.symm ▸ List.mem_map.mpr ⟨tgtBlk, h1t, rfl⟩) hnd.1
                    | tail _ h2t => exact ih hnd.2 h1t h2t
              exact go graph.blocks.toList hNodesNodup hTgtGraph hBlkGraph hIdEq
            subst hSameTgt
            have hLenEq : args.length = tgtBlk.params.length := by
              rw [← hEdgeArgs]
              have h := congrArg List.length hArgTypes
              simpa only [List.length_map] using h
            obtain ⟨⟨rty, read⟩, hGet, hReadMem⟩ := getElem?_some_of_lt (hLenEq.symm ▸ hkLt)
            have hInReads : (⟨rty, read⟩ : Sigma (SSARead language.operands.Ty lang.literal)) ∈
                srcBlk.ending.regReads := by
              cases hSrcEnd : srcBlk.ending with
              | jump e' =>
                rw [hSrcEnd] at hEdge
                cases List.mem_singleton.mp hEdge
                simpa [BIRTerm.regReads, hEdgeArgs] using hReadMem
              | branch _ y n =>
                rw [hSrcEnd] at hEdge
                simp only [BIRTerm.successors, List.mem_cons, List.not_mem_nil, or_false] at hEdge
                rcases hEdge with hEq | hEq
                · cases hEq
                  apply List.mem_cons_of_mem
                  exact List.mem_append_left _ (hEdgeArgs ▸ hReadMem)
                · cases hEq
                  apply List.mem_cons_of_mem
                  exact List.mem_append_right _ (hEdgeArgs ▸ hReadMem)
              | ret _ => rw [hSrcEnd] at hEdge; cases hEdge
              | trap _ => rw [hSrcEnd] at hEdge; cases hEdge
            obtain ⟨arg, hArg⟩ := emitRead_ok_of_literalOk ((hBlkSpec srcBlk hSrcGraph).2.1 _ hInReads)
            exact ⟨(arg, predLabel), by simp [hGet, hArg, bind, Except.bind, pure, Except.pure]⟩
          obtain ⟨predArms, hPredArms⟩ := hArmsOk
          dsimp only
          rw [hPredArms]
          exact ⟨_, rfl⟩
    have hInstsOk : ∃ insts, (blk.body.mapM fun op => emitInst language op.instruction) = .ok insts := by
      apply list_mapM_except_ok
      intro op hOpMem
      obtain ⟨idx, hIdx⟩ := mem_zipIdx_of_mem hOpMem
      exact emitInst_ok_of_supported hFnMem hEntryReach hBlkGraph hBlkReach hIdx
        ((hBlkSpec blk hBlkGraph).2.2 (op, idx) hIdx) hAllSup
    have hTermOk : ∃ term, emitTerm language indices splits j blk.ending = .ok term := by
      have hTermReads := (hBlkSpec blk hBlkGraph).2.1
      cases hEnd : blk.ending with
      | ret ov =>
        cases ov with
        | none => exact ⟨_, rfl⟩
        | some pair =>
          rcases pair with ⟨rty, arg⟩
          have hLitOk : readLiteralOk language ⟨rty, arg⟩ = true :=
            hTermReads ⟨rty, arg⟩ (by simp [hEnd, BIRTerm.regReads])
          obtain ⟨arg', hArg⟩ := emitRead_ok_of_literalOk hLitOk
          unfold emitTerm
          simp only [hArg, bind, Except.bind, pure, Except.pure]
          cases llvmType language rty with
          | integer w => cases w <;> exact ⟨_, rfl⟩
          | flag | ptr | void | overflowPair _ => exact ⟨_, rfl⟩
      | trap slot => exact ⟨_, rfl⟩
      | jump edge =>
        obtain ⟨tgtIdx, hTgt⟩ := hSuccIndex blk hBlkMem edge (by simp [hEnd, BIRTerm.successors])
        unfold emitTerm
        simp only [hTgt]
        exact ⟨_, rfl⟩
      | branch test yes no =>
        obtain ⟨yesIdx, hYes⟩ := hSuccIndex blk hBlkMem yes (by simp [hEnd, BIRTerm.successors])
        obtain ⟨noIdx, hNo⟩ := hSuccIndex blk hBlkMem no (by simp [hEnd, BIRTerm.successors])
        have hTestOk : readLiteralOk language ⟨lang.bool, test⟩ = true :=
          hTermReads ⟨lang.bool, test⟩ (by simp [hEnd, BIRTerm.regReads])
        cases test with
        | value v =>
          unfold emitTerm
          simp only [hYes, hNo, pure, Except.pure]
          exact ⟨_, rfl⟩
        | literal lit =>
          obtain ⟨arg', hArg⟩ := decodeLiteralArg_some_of_ok hTestOk
          unfold emitTerm
          simp only [hYes, hNo, hArg, pure, Except.pure]
          exact ⟨_, rfl⟩
    obtain ⟨phis, hPhis⟩ := hPhisOk
    obtain ⟨insts, hInsts⟩ := hInstsOk
    obtain ⟨term, hTerm⟩ := hTermOk
    dsimp only
    rw [hPhis, hInsts, hTerm]
    exact ⟨_, rfl⟩
  obtain ⟨blockItems, hBlockItems⟩ := hBlockItemsOk
  have hCompOk : ∃ component, emitComponentGraph language graph = .ok component := by
    unfold emitComponentGraph
    simp only [hOrd]
    rw [hBlockItems]
    exact ⟨_, rfl⟩
  obtain ⟨component, hComp⟩ := hCompOk
  have hEmitOk : emit (language := language) bir = .ok ⟨component, launcherModule diagnostics, bir.sites⟩ := by
    unfold emit
    simp [hBackend, hSup, hDiag, hFnLook, hComp, bind, Except.bind, pure, Except.pure]
  exact ⟨⟨component, launcherModule diagnostics, bir.sites⟩, hEmitOk, emit_metadataMatches hEmitOk⟩

/-- On any well-formed LLVM `BIR`, `EmitLLVM.emit` can fail only via `firstUnsupported`. -/
theorem emit_error_of_wf {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {bir : BIR lang profile} {failure : StageFailure}
    (hBackend : profile.backend = .llvm)
    (hWF : BIR.WF (language := language) bir)
    (hErr : emit (language := language) bir = .error failure) :
    ∃ cause, firstUnsupported (language := language) bir = some cause ∧
      failure = .unsupportedFeature profile cause := by
  cases hSup : firstUnsupported (language := language) bir with
  | some cause =>
    have hRef := emit_refusal_of_firstUnsupported hBackend hSup
    rw [hRef] at hErr
    cases Except.error.inj hErr
    exact ⟨cause, rfl, rfl⟩
  | none =>
    obtain ⟨output, hOk, _⟩ := emit_ok_of_wf hBackend hWF hSup
    rw [hOk] at hErr
    cases hErr

/-! ## Focused kernel-checked controls -/

section Controls

private abbrev cLang : CoreLanguage := sliceLanguage slice0
private abbrev cLow : LoweringLanguage cLang.operands.Ty := sliceLoweringLanguage slice0
private abbrev cU8 : cLang.operands.Ty := cLow.intTy .u8
private abbrev cBool : cLang.operands.Ty := cLow.bool
private abbrev cU32 : cLang.operands.Ty := cLow.intTy .u32

private def ctrlMain : FnId := ⟨⟨⟨⟨"p6a"⟩, ⟨⟨"0"⟩⟩⟩, [⟨"main"⟩], by decide⟩, ⟨"main"⟩⟩
private def ctrlPath : SourcePath :=
  match SourcePath.parse (SourceText.ofBytes "src/main.con".toUTF8.data.toList) with
  | .ok p => p
  | .error _ => mainSourcePath

private def ctrlProject : Project :=
  Project.singleton ctrlPath (SourceText.ofBytes r#"fn main() -> u8 { return 255 + 1; }"#.toUTF8.data.toList)

private def ctrlOrigin : Origin :=
  ⟨⟨0⟩, ⟨⟨0⟩, 29, 30, 1, 30⟩, ⟨.function ctrlMain, ⟨[]⟩⟩⟩

private def ctrlProfile : Profile := ⟨.llvm, .x86_64Linux, 8, 4096⟩
private def ctrlMacProfile : Profile := ⟨.llvm, .aarch64Macos, 8, 4096⟩

private def addOvfBIR (p : Profile) (lhs rhs : UInt8) : BIR cLow p :=
  let b0 : BIRBlock cLow :=
    { id := ⟨0⟩
      params := []
      body := [⟨ctrlOrigin, .addWithOverflow .u8 ⟨0⟩ ⟨1⟩ (.literal ⟨[lhs]⟩) (.literal ⟨[rhs]⟩)⟩]
      ending := .branch (.value ⟨1⟩) ⟨⟨1⟩, []⟩ ⟨⟨2⟩, [⟨cU8, .value ⟨0⟩⟩]⟩
      boundaryOrigins := [] }
  let b1 : BIRBlock cLow :=
    { id := ⟨1⟩, params := [], body := [], ending := .trap ⟨0⟩, boundaryOrigins := [] }
  let b2 : BIRBlock cLow :=
    { id := ⟨2⟩, params := [⟨cU8, ⟨2⟩⟩], body := [], ending := .ret (some ⟨cU8, .value ⟨2⟩⟩), boundaryOrigins := [] }
  { project := ctrlProject
    entry := ctrlMain
    signatures := [(ctrlMain, ⟨[], some cU8⟩)]
    functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0, b1, b2]⟩)]
    layouts := []
    sites := #[⟨.overflow, ctrlOrigin⟩]
    mutableLocals := []
    declarations := [] }

-- Positive control: two-result `addWithOverflow` emits `@llvm.uadd.with.overflow.i8` +
-- `extractvalue` 0/1 + `phi` + `LauncherMatches` + exact profile-derived headers.
example :
    ∃ out,
      emit (language := cLang) (addOvfBIR ctrlProfile 20 22) = .ok out ∧
      LLVMMetadataMatches (addOvfBIR ctrlProfile 20 22) out ∧
      out.component.items.contains
        (Item.declare (.overflowPair .u8) (.overflow .add .u8) [.integer .u8, .integer .u8]) = true ∧
      out.component.items.contains
        (Item.instruction (.call (some (.overflow 0)) (.overflowPair .u8) (.overflow .add .u8)
          [(.integer .u8, .immediate (.natural 20)), (.integer .u8, .immediate (.natural 22))])) = true ∧
      out.component.items.contains
        (Item.instruction (.extract (.register 0) (.overflowPair .u8) (.overflow 0) 0)) = true ∧
      out.component.items.contains
        (Item.instruction (.extract (.register 1) (.overflowPair .u8) (.overflow 0) 1)) = true ∧
      out.launcher.items = Backend.Runtime.llvmDiagnostics
        ["concrete: trap: overflow at src/main.con:1:30 (site 0)\n"] := by
  refine ⟨_, rfl, ?_, rfl, rfl, rfl, rfl, rfl⟩
  exact emit_metadataMatches (language := cLang) (bir := addOvfBIR ctrlProfile 20 22) rfl

-- Positive control: LLVM supports 32-bit `addWithOverflow` (`@llvm.uadd.with.overflow.i32`)
-- and emits distinct target headers across `x86_64Linux` vs `aarch64Macos`.
example :
    let b0 : BIRBlock cLow :=
      { id := ⟨0⟩
        params := []
        body := [⟨ctrlOrigin, .addWithOverflow .u32 ⟨0⟩ ⟨1⟩
          (.literal ⟨[1, 0, 0, 0]⟩) (.literal ⟨[2, 0, 0, 0]⟩)⟩]
        ending := .ret (some ⟨cU32, .value ⟨0⟩⟩)
        boundaryOrigins := [] }
    let mkBir (p : Profile) : BIR cLow p :=
      { project := ctrlProject, entry := ctrlMain, signatures := [(ctrlMain, ⟨[], some cU32⟩)],
        functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], layouts := [], sites := #[],
        mutableLocals := [], declarations := [] }
    ∃ outL outM,
      emit (language := cLang) (mkBir ctrlProfile) = .ok outL ∧
      emit (language := cLang) (mkBir ctrlMacProfile) = .ok outM ∧
      outL.component.items.contains
        (Item.declare (.overflowPair .u32) (.overflow .add .u32) [.integer .u32, .integer .u32]) = true ∧
      outL.bytesAt .launcher ≠ outM.bytesAt .launcher := by
  refine ⟨_, _, rfl, rfl, rfl, ?_⟩
  intro h
  have hL := congrArg (fun b => b.data.toList) h
  exact (show _ ≠ _ from by decide) hL

-- Positive control: zero-slot self-loop on entry inserts `Label.entry` trampoline so `%b0`
-- is never the first basic block of `@concrete_body`.
example :
    let b0 : BIRBlock cLow :=
      { id := ⟨0⟩, params := [], body := [], ending := .jump ⟨⟨0⟩, []⟩, boundaryOrigins := [] }
    let bir : BIR cLow ctrlProfile :=
      { project := ctrlProject, entry := ctrlMain, signatures := [(ctrlMain, ⟨[], some cU8⟩)],
        functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], layouts := [], sites := #[],
        mutableLocals := [], declarations := [] }
    ∃ out,
      emit (language := cLang) bir = .ok out ∧
      out.component.items.contains (Item.label .entry) = true ∧
      out.component.items.contains (Item.terminator (.branch (.block 0))) = true := by
  exact ⟨_, rfl, rfl, rfl⟩

-- Positive control: an unreachable block containing a `.call` (admitted by `BackendPolicy`)
-- is pruned during component emission instead of causing emission failure.
example :
    let b0 : BIRBlock cLow :=
      { id := ⟨0⟩, params := [], body := [], ending := .ret (some ⟨cU8, .literal ⟨[42]⟩⟩),
        boundaryOrigins := [] }
    let bDead : BIRBlock cLow :=
      { id := ⟨1⟩, params := [],
        body := [⟨ctrlOrigin, .call ctrlMain (some ⟨cU8, ⟨0⟩⟩) []⟩],
        ending := .ret (some ⟨cU8, .value ⟨0⟩⟩), boundaryOrigins := [] }
    let bir : BIR cLow ctrlProfile :=
      { project := ctrlProject, entry := ctrlMain, signatures := [(ctrlMain, ⟨[], some cU8⟩)],
        functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0, bDead]⟩)], layouts := [], sites := #[],
        mutableLocals := [], declarations := [] }
    ∃ out,
      emit (language := cLang) bir = .ok out ∧
      out.component.items.contains (Item.label (.block 1)) = false := by
  exact ⟨_, rfl, rfl⟩

-- Negative control: unsupported storage form (`.load`) is refused with the exact
-- `firstUnsupported` feature cause.
example :
    let b0 : BIRBlock cLow :=
      { id := ⟨0⟩, params := [],
        body := [⟨ctrlOrigin, .load (ty := cU8) ⟨0⟩ ⟨⟨0⟩, []⟩⟩],
        ending := .ret (some ⟨cU8, .value ⟨0⟩⟩), boundaryOrigins := [] }
    let bir : BIR cLow ctrlProfile :=
      { project := ctrlProject, entry := ctrlMain, signatures := [(ctrlMain, ⟨[], some cU8⟩)],
        functions := [(ctrlMain, [], ⟨⟨0⟩, #[b0]⟩)], layouts := [], sites := #[],
        mutableLocals := [], declarations := [] }
    emit (language := cLang) bir =
      .error (.unsupportedFeature ctrlProfile (.unsupportedStorage .load ctrlMain (.instruction ⟨0⟩ 0))) := by
  rfl

-- Negative control: calling `EmitLLVM.emit` on an LCB profile fails closed.
example :
    let lcbProf : Profile := ⟨.lcb, .x86_64Linux, 8, 4096⟩
    emit (language := cLang) (addOvfBIR lcbProf 20 22) = .error (.unsupportedProfile wrongBackendRule) := by
  rfl

end Controls

end Concrete.Backend.EmitLLVM
