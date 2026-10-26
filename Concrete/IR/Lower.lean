import Std.Data.HashMap
import Std.Data.HashSet
import Std.Data.TreeMap
import Concrete.Elab.Core
import Concrete.IR.SSA
import Concrete.Check.Layout
import Concrete.Resolve.Intrinsic
import Concrete.Report.Diagnostic
import Concrete.Backend.Symbols
import Concrete.Semantics.BindingMode
import Concrete.Resolve.TypeId
import Concrete.Proof.CallCollect

namespace Concrete

-- R-0492 step 4: Lower keys locals by binding identity (`CLocal.key`). There is no CLocal -> String
-- coercion (removed at the end of step 4), so any remaining read of a local by its spelling is a compile error.

/-! ## Lowering: Core IR → SSA IR

Converts structured Core IR into SSA form with basic blocks,
conditional branches, and phi nodes.
-/

-- ============================================================
-- Helpers
-- ============================================================

private def enumerate {α : Type} (l : List α) : List (Nat × α) :=
  let rec go (i : Nat) : List α → List (Nat × α)
    | [] => []
    | a :: rest => (i, a) :: go (i + 1) rest
  go 0 l

-- ============================================================
-- The variable table
-- ============================================================

/-- A set of binding identities. -/
abbrev LocalKeySet := Std.TreeMap LocalKey Unit LocalKey.cmp

private def SVal.isReg : SVal → Bool
  | .reg .. => true
  | _ => false

/-- One binding of the variable table: its value, the table position it was first bound at
    (the order the table lists bindings in) and the tick of its latest write. -/
structure VarEntry where
  pos : Nat
  tick : Nat
  val : SVal

/-- The variable table: each live binding's current SSA value, listed in the order the bindings
    were first bound.

    Bug 262: this was an association list, searched by `lookupVar` and searched and rebuilt or
    appended by `setVar`, so a function of n `let`s took time quadratic in n to lower; and every
    branch merge visited every binding in scope (`reconcileBranchVars`, the scope cleanup after a
    merge, the `&&`/`||` merge), searching each arm's list for each, so n `let`s each followed by
    an `if` took time cubic in n. The table is now persistent search trees, so a lookup, a write
    and a snapshot (a branch keeps the table it started from) are logarithmic, and it records, per
    binding, the tick of its latest write: every write takes a fresh tick from one counter of the
    function (`LowerState.varTick`), so a table derived from a snapshot by writes holds exactly the
    snapshot's bindings at their old ticks plus the bindings written since at ticks above the
    snapshot's `watermark`. A merge visits only those, and the bindings whose value is not a
    register (`nonReg`), which the merge rules count as changed even when no arm wrote them.

    Positions are ticks too (a new binding's position is the tick it was first written at), so the
    listing order is the order the association list kept: a write to a bound name keeps its
    position, a new name goes last, and erasing a name drops it. -/
structure VarMap where
  entries : Std.TreeMap LocalKey VarEntry LocalKey.cmp := {}
  /-- position → binding: the listing order -/
  byPos : Std.TreeMap Nat LocalKey compare := {}
  /-- latest-write tick → binding -/
  byTick : Std.TreeMap Nat LocalKey compare := {}
  /-- position → binding, for the bindings whose value is not a register -/
  nonReg : Std.TreeMap Nat LocalKey compare := {}

namespace VarMap

def empty : VarMap := {}

def find? (m : VarMap) (k : LocalKey) : Option SVal := (m.entries.get? k).map (·.val)

def contains (m : VarMap) (k : LocalKey) : Bool := m.entries.contains k

/-- Bind `k` to `v` at write tick `tick` (fresh, above every tick in the table). A bound name keeps
    its position; a new one goes last. -/
def set (m : VarMap) (k : LocalKey) (v : SVal) (tick : Nat) : VarMap :=
  match m.entries.get? k with
  | some e =>
    { entries := m.entries.insert k { e with tick := tick, val := v }
      byPos := m.byPos
      byTick := (m.byTick.erase e.tick).insert tick k
      nonReg := if v.isReg then m.nonReg.erase e.pos else m.nonReg.insert e.pos k }
  | none =>
    { entries := m.entries.insert k { pos := tick, tick := tick, val := v }
      byPos := m.byPos.insert tick k
      byTick := m.byTick.insert tick k
      nonReg := if v.isReg then m.nonReg else m.nonReg.insert tick k }

def erase (m : VarMap) (k : LocalKey) : VarMap :=
  match m.entries.get? k with
  | some e =>
    { entries := m.entries.erase k
      byPos := m.byPos.erase e.pos
      byTick := m.byTick.erase e.tick
      nonReg := m.nonReg.erase e.pos }
  | none => m

/-- The bindings in listing order. -/
def toList (m : VarMap) : List (LocalKey × SVal) :=
  m.byPos.foldr (fun _ k acc => match m.entries.get? k with
    | some e => (k, e.val) :: acc
    | none => acc) []

/-- The highest write tick in the table (0 when it is empty; ticks start at 1). A table derived
    from this one by writes holds every binding written since at a tick above it. -/
def watermark (m : VarMap) : Nat := m.byTick.maxKey?.getD 0

/-- The bindings written at ticks above `w`, oldest write first. -/
partial def writtenAfter (m : VarMap) (w : Nat) : List LocalKey :=
  let rec go (t : Nat) (acc : List LocalKey) : List LocalKey :=
    match m.byTick.getEntryGT? t with
    | some (t', k) => go t' (k :: acc)
    | none => acc.reverse
  go w []

/-- `keys` restricted to the bindings of this table, in its listing order, with their values here. -/
def restrictOrdered (m : VarMap) (keys : List LocalKey) : List (LocalKey × SVal) :=
  let byPos : Std.TreeMap Nat LocalKey compare := keys.foldl (fun acc k =>
    match m.entries.get? k with
    | some e => acc.insert e.pos k
    | none => acc) {}
  byPos.foldr (fun _ k acc => match m.entries.get? k with
    | some e => (k, e.val) :: acc
    | none => acc) []

/-- The bindings of `pre` a merge of the tables `arms` (each derived from `pre` by writes) must
    visit, in `pre`'s listing order with their `pre` values: those some arm wrote since `pre`, and
    those whose `pre` value is not a register (the merge rules count such a binding as changed even
    when no arm wrote it). Every other binding holds its `pre` value, a register, in every arm. -/
def mergeCandidates (pre : VarMap) (arms : List VarMap) : List (LocalKey × SVal) :=
  let w := pre.watermark
  let written := arms.foldl (fun acc arm => arm.writtenAfter w ++ acc) []
  pre.restrictOrdered (pre.nonReg.foldl (fun acc _ k => k :: acc) written)

/-- `pre`'s bindings, each with its value in `cur` where `cur` still binds it: the scope cleanup
    after a merge (bindings declared inside the branches are dropped). `cur` is derived from `pre`
    by writes, so only the bindings it wrote since `pre` can differ from `pre`; they keep their
    `cur` ticks, which are above `pre`'s. -/
def restrictTo (pre cur : VarMap) : VarMap :=
  (cur.writtenAfter pre.watermark).foldl (fun acc k =>
    match pre.entries.get? k, cur.entries.get? k with
    | some _, some e => acc.set k e.val e.tick
    | _, _ => acc) pre

end VarMap

-- ============================================================
-- Lowering state
-- ============================================================

structure LoopInfo where
  headerLabel : String
  exitLabel : String
  /-- Phi registers created at header: (varName, phiReg, preLoopVal, ty) -/
  headerPhis : List (LocalKey × String × SVal × Ty) := []
  /-- Var snapshots at each break: (varsSnapshot, sourceLabel) -/
  breakEdges : List (VarMap × String) := []
  /-- Alloca slot for while-as-expression result (break stores value here). -/
  resultSlot : Option String := none
  /-- Type for the result slot. -/
  resultTy : Ty := .unit
  /-- Var snapshots at each continue: (varsSnapshot, sourceLabel) -/
  continueEdges : List (VarMap × String) := []
  /-- Target label for continue (step block for for-loops, header for while-loops). -/
  continueTarget : String := ""
  /-- Optional loop label for labeled break/continue. -/
  loopLabel : Option String := none

inductive ScopeKind where
  | function
  | block
  | loop
  deriving Inhabited, BEq

structure ScopeFrame where
  kind : ScopeKind
  deferred : List CExpr := []

structure LowerState where
  /-- Finished blocks, NEWEST FIRST (prepend is O(1); `lowerFn` reverses once).
      `blocks ++ [block]` per terminator was O(blocks) each, O(blocks²) per
      function, and `getLast?` per arm another O(blocks) (bug 178). -/
  blocksRev : List SBlock
  currentLabel : String
  currentInsts : List SInst
  labelCounter : Nat
  regCounter : Nat
  -- R-0492 step 4: keyed by binding identity (a compiler temporary by its Core name)
  vars : VarMap
  /-- The last write tick issued to the variable table (see `VarMap`). Never restored: a table
      reinstated from a snapshot keeps its ticks, and every later write is above them. -/
  varTick : Nat := 0
  stringLits : List (String × String)
  structDefs : List CStructDef
  enumDefs : List CEnumDef
  newtypes : List NewtypeDef := []
  loopStack : List LoopInfo
  constants : List (String × Ty × CExpr) := []
  /-- R-0492 step 4 (bug 233): every module constant of the program by its imported Core spelling
      (`ConstId.coreName`, `cfg::BASE`), with the constants of its defining module's tree, the
      scope its initializer is lowered in (the scope its own module's functions see). -/
  importedConstants : List (String × CExpr × List (String × Ty × CExpr)) := []
  /-- Names of user/Core-defined functions in the program. A call to a
      DEFINED function is never an intrinsic — intrinsic identity is
      by-resolution, not by raw name (audit 2026-07-16: user fns named
      sizeof/alloc/... were hijacked at this pass too). A set, asked per call (bug 279: a list
      searched per call was calls × functions). -/
  definedFns : Std.HashSet String := {}
  /-- Allocas that must be hoisted to the function entry block.
      Prevents unbounded stack growth when &mut borrows occur in loops. -/
  entryAllocasRev : List SInst := []
  /-- Aggregate variables promoted to stable allocas during loops.
      Maps (varName, allocaReg, ty). Field assignment GEPs directly into
      the alloca instead of phi-transporting whole struct values. -/
  promotedAllocas : Std.TreeMap LocalKey (String × Ty) LocalKey.cmp := {}
  scopeStack : List ScopeFrame := []
  /-- True after terminateBlock, cleared by startBlock. Used to detect
      dead code after early returns inside borrow blocks. -/
  blockTerminated : Bool := false
  /-- True while a module constant's initializer is being inlined (bug 123): a constant has no
      storage, and Check does not check initializers, so every borrow form is refused here. -/
  inConstInit : Bool := false
  /-- Bug 286: while a branch region (an arm, a loop body, a short-circuit right operand) is
      lowered, the names its source takes the address of (`regionAddrTaken`): every nested
      region's address-taken set is a subset. `none` outside every region, and while deferred
      calls are lowered (their source lies outside the region they are emitted in). -/
  addrBound : Option LocalKeySet := none
  /-- Bug 291: `structDefs`, `enumDefs` and `newtypes` by name (`Layout.Index`), built once per
      module by `lowerModule`; every lookup of a declaration by name asks it through
      `getLayoutCtx` (`none`: the lists are searched). -/
  layoutIndex : Option Layout.Index := none
  /-- Bug 292: instructions to add to finished blocks, by label: `(prefix chunks, suffix chunks)`,
      each newest first. A loop header's phis (`prependInstsToBlock`) and an aggregate merge's
      stores (`insertStoresBeforeTerm`) are recorded here and applied once when `lowerFn` assembles
      the blocks; mapping the whole finished-block list per patch made n nested loops take n². Both
      tables are persistent trees: a hash table here was copied whole on each insert whenever an
      outer frame still held the state (as the lowering of an enclosing loop does). -/
  blockPatches : Std.TreeMap String (List (List SInst) × List (List SInst)) compare := {}
  /-- Bug 292: the labels of the finished blocks; a patch names only one of them, as the map over
      the finished blocks it replaces touched only those. -/
  finishedLabels : Std.TreeSet String compare := {}

abbrev LowerM := ExceptT Diagnostics (StateM LowerState)

private def throwLower (msg : String) : LowerM α :=
  throw [{ severity := .error, message := msg, pass := "lower", span := none, hint := none, code := "E0602" }]

private def getState : LowerM LowerState := get

/-- Bug 123: no borrow in a module constant's initializer (see `inConstInit`). -/
private def refuseBorrowInConstInit : LowerM Unit := do
  if (← get).inConstInit then
    throwLower "a module constant's initializer may not borrow (a constant has no storage; bug 123)"
private def setState (s : LowerState) : LowerM Unit := set s

private def freshReg (pfx : String := "t") : LowerM String := do
  let s ← getState
  let name := s!"{pfx}{s.regCounter}"
  setState { s with regCounter := s.regCounter + 1 }
  return name

private def freshLabel (pfx : String := "bb") : LowerM String := do
  let s ← getState
  let name := s!"{pfx}{s.labelCounter}"
  setState { s with labelCounter := s.labelCounter + 1 }
  return name

private def emit (inst : SInst) : LowerM Unit := do
  let s ← getState
  -- Accumulate REVERSED (prepend is O(1)); `terminateBlock` reverses once when it
  -- freezes the block. The old `++ [inst]` was O(current length) per emit, i.e.
  -- O(n^2) to build a block of n instructions — the array/many-instruction O(n^2)
  -- (a large array literal lowers to n stores in one block). `currentInsts` is
  -- only appended here and read at `terminateBlock`, so the reverse is local.
  setState { s with currentInsts := inst :: s.currentInsts }

/-- Coerce an SSA value to `ty` with an explicit cast when its type differs
    (e.g. an `Int` branch value into an `i32` result slot). No-op when the types
    already match or the value is unit. This is the single place value-producing
    constructs — if-expression, while-expression, and match arms — normalize a
    branch/break/else value to the expected width before it is stored or fed to a
    phi. Keeping it in one spot is what prevents the "value stored at the wrong
    width" bug class (cf. the while-expression i32 miscompile). -/
private def coerceVal (val : SVal) (ty : Ty) (pfx : String := "cast.") : LowerM SVal := do
  if val.ty != ty && val.ty != .unit then do
    let castReg ← freshReg pfx
    emit (.cast castReg val ty)
    pure (.reg castReg ty)
  else pure val

/-- Allocate a result slot for a value-bearing control-flow construct (if-expr,
    while-expr), UNLESS the result type lowers to LLVM void: `unit`/`never` carry
    no runtime value and `alloca void` is illegal IR (the `%ifslot = alloca void`
    miscompile). Returns `none` when no slot is needed — the construct yields
    `.unit`, so the caller must skip the per-branch store and the merge load. This
    is the single unit/void guard for slot-based result materialization (the
    analogue of the `match_` phi guard), and the first shared piece of the
    structured control-flow builder (ROADMAP #5). -/
private def freshResultSlot (pfx : String) (ty : Ty) : LowerM (Option String) := do
  if ty == .unit || ty == .never then return none
  let slot ← freshReg pfx
  emit (.alloca slot ty)
  return some slot

/-- Load the final value of a value-bearing control-flow construct from its
    result slot; yields `.unit` when there is no slot (a unit/never result). The
    read counterpart of `freshResultSlot` — the single place if-expr and
    while-expr produce their merged value (ROADMAP #5 control-flow builder). -/
private def loadResult (slot? : Option String) (ty : Ty) (pfx : String) : LowerM SVal := do
  match slot? with
  | some slot =>
    let dst ← freshReg pfx
    emit (.load dst (.reg slot ty) ty)
    return .reg dst ty
  | none => return .unit

/-- Static length of an array type, peeling one ref/ptr/heap layer (so `&[T; N]`
    and `[T; N]` both yield `N`). `none` for non-array types. -/
private def arrayLenOfTy : Ty → Option Nat
  | .array _ n => some n
  | .ref t | .refMut t | .ptrMut t | .ptrConst t | .heap t => arrayLenOfTy t
  | _ => none

/-- Emit a runtime bounds check for an array access at `idxVal` against length
    `len` (KNOWN_HOLES H8). Calls the shared `@__cc_bounds_check` helper, which
    aborts (exit 134, same trap as checked arithmetic) when `(u64)idx >= len` —
    catching both negative and `>= len` in one unsigned compare. ALWAYS emitted:
    memory safety must not depend on the obligation engine; a provably in-bounds
    index folds the compare away, and a provably constant OOB is already a hard
    compile error. The index is widened to i64 for the check; the original `idxVal`
    is still used for the GEP. -/
private def emitBoundsCheck (idxVal : SVal) (len : Nat) : LowerM Unit := do
  let idxI64 ← coerceVal idxVal .int "boundsidx."
  emit (.call none (.direct "__cc_bounds_check" .free) [idxI64, .intConst (Int.ofNat len) .int] .unit)

/-- Emit an alloca that will be hoisted to the function entry block.
    This prevents dynamic stack growth when allocas occur inside loops. -/
private def emitEntryAlloca (inst : SInst) : LowerM Unit := do
  let s ← getState
  -- Newest first (O(1)); `lowerFn` reverses once (bug 178).
  setState { s with entryAllocasRev := inst :: s.entryAllocasRev }

-- Insert `store v → dst` at the end (before the terminator) of each already-emitted
-- block named in `stores` (label, value), in list order per block. Used by if/else
-- and match to store aggregate values into merge allocas. ONE pass over the blocks
-- for all arms: the old per-arm helper rescanned every block per store, O(arms ×
-- blocks) for an aggregate-valued wide match (bug 178).
private def insertStoresBeforeTerm (stores : List (String × SVal)) (dst : SVal) : LowerM Unit := do
  let s ← getState
  let byLabel : Std.HashMap String (List SInst) := stores.foldr (fun (l, v) m =>
    m.insert l (SInst.store v dst :: m.getD l [])) ∅
  if byLabel.isEmpty then return
  -- Bug 292: recorded, applied by `lowerFn` (`applyBlockPatches`), not a map over every block.
  let finished := s.finishedLabels
  let add (m : Std.TreeMap String (List (List SInst) × List (List SInst)) compare) (l : String)
      (ins : List SInst) :=
    if finished.contains l then
      let (pre, suf) := m.getD l ([], [])
      m.insert l (pre, ins :: suf)
    else m
  modify fun s => { s with blockPatches := byLabel.fold add s.blockPatches }

private def terminateBlock (term : STerm) : LowerM Unit :=
  -- `currentInsts` is accumulated reversed by `emit`; restore program order here.
  modify fun s =>
    let block : SBlock := { label := s.currentLabel, insts := s.currentInsts.reverse, term := term }
    { s with blocksRev := block :: s.blocksRev, currentInsts := [], blockTerminated := true,
             finishedLabels := s.finishedLabels.insert s.currentLabel }

private def startBlock (label : String) : LowerM Unit := do
  let s ← getState
  setState { s with currentLabel := label, blockTerminated := false }

private def setVar (name : LocalKey) (val : SVal) : LowerM Unit := do
  let s ← getState
  -- If promoted to stable alloca, store there instead of updating var map
  match s.promotedAllocas.get? name with
  | some (allocaReg, ty) =>
    emit (.store val (.reg allocaReg ty))
  | none =>
    let tick := s.varTick + 1
    setState { s with vars := s.vars.set name val tick, varTick := tick }

/-- A fresh variable table binding `bindings` in order, at fresh ticks (a loop header's table,
    a function's parameters). The first binding of a name wins, as `lookup` on a list found it. -/
private def freshVarMap (bindings : List (LocalKey × SVal)) : LowerM VarMap := do
  let s ← getState
  let (m, tick) := bindings.foldl (fun (acc : VarMap × Nat) (k, v) =>
    if acc.1.contains k then acc else (acc.1.set k v (acc.2 + 1), acc.2 + 1)) (VarMap.empty, s.varTick)
  setState { s with varTick := tick }
  return m

private def lookupVar (name : LocalKey) : LowerM (Option SVal) := do
  let s ← getState
  -- Check if this variable is promoted to a stable alloca (aggregate in loop)
  match s.promotedAllocas.get? name with
  | some (allocaReg, ty) =>
    match ty with
    | .array _ _ =>
      -- Arrays are pass-by-ptr: the alloca pointer IS the array value.
      -- No load needed — returning the alloca register directly.
      return some (.reg allocaReg ty)
    | _ =>
      -- Load current value from the alloca
      let loadDst ← freshReg "pload."
      emit (.load loadDst (.reg allocaReg ty) ty)
      return some (.reg loadDst ty)
  | none =>
    return s.vars.find? name

private def internString (val : String) : LowerM String := do
  let s ← getState
  match s.stringLits.find? fun (_, v) => v == val with
  | some (name, _) => return name
  | none =>
    let name := s!"str.{s.stringLits.length}"
    setState { s with stringLits := s.stringLits ++ [(name, val)] }
    return name

/-- Check if current block already has a terminator in the blocks list. -/
private def currentBlockTerminated : LowerM Bool := do
  let s ← getState
  match s.blocksRev.head? with
  | some b => return b.label == s.currentLabel && s.currentInsts.isEmpty
  | none => return false

/-- Get the current block label (may differ from startBlock label after lowering body). -/
private def getCurrentLabel : LowerM String := do
  let s ← getState
  -- If the current block was terminated, the last block's label is where we ended up
  -- Otherwise, we're still building the current label
  return s.currentLabel

-- ============================================================
-- Struct/enum definition lookup helpers
-- ============================================================

/-- Build a Layout.Ctx from the current LowerState. -/
private def getLayoutCtx : LowerM Layout.Ctx := do
  let s ← getState
  return { structDefs := s.structDefs, enumDefs := s.enumDefs, newtypes := s.newtypes,
           index := s.layoutIndex }

/-- Look up a struct definition's fields by type name. -/
private def lookupStructFields (tyName : String) : LowerM (List (String × Ty)) := do
  match Layout.lookupStruct (← getLayoutCtx) tyName with
  | some sd => return sd.fields
  | none =>
    throwLower s!"Lower.lookupStructFields: struct '{tyName}' not found in struct defs"

/-- Get field index within a struct definition. Returns 0 if not found. -/
private def fieldIndex (tyName : String) (fieldName : String) : LowerM Nat := do
  let fields ← lookupStructFields tyName
  match (enumerate fields).find? fun (_, (n, _)) => n == fieldName with
  | some (idx, _) => return idx
  | none =>
    throwLower s!"Lower.fieldIndex: field '{fieldName}' not found in struct '{tyName}'"

/-- Get variant index within an enum definition. Returns 0 if not found. -/
private def variantIndex (enumName : String) (variantName : String) : LowerM Nat := do
  match Layout.lookupEnum (← getLayoutCtx) enumName with
  | some ed =>
    match (enumerate ed.variants).find? fun (_, (vn, _)) => vn == variantName with
    | some (idx, _) => return idx
    | none =>
      throwLower s!"Lower.variantIndex: variant '{variantName}' not found in enum '{enumName}'"
  | none =>
    throwLower s!"Lower.variantIndex: enum '{enumName}' not found in enum defs"

/-- Get variant fields within an enum definition. -/
private def variantFields (enumName : String) (variantName : String) (typeArgs : List Ty := []) : LowerM (List (String × Ty)) := do
  match Layout.lookupEnum (← getLayoutCtx) enumName with
  | some ed =>
    let ed := Layout.substEnumTypeArgs ed typeArgs
    match ed.variants.find? fun (vn, _) => vn == variantName with
    | some (_, fields) => return fields
    | none =>
      throwLower s!"Lower.variantFields: variant '{variantName}' not found in enum '{enumName}'"
  | none =>
    throwLower s!"Lower.variantFields: enum '{enumName}' not found in enum defs"

/-- Extract struct type name from a Ty, unwrapping references/pointers. -/
private def structNameFromTy (ty : Ty) : LowerM String :=
  match ty with
  | .named n => return n
  | .generic n _ _ => return n
  | .string => return "String"
  | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner => structNameFromTy inner
  | other =>
    throwLower s!"Lower.structNameFromTy: unhandled type '{repr other}'"

/-- Compute byte size of a type (for malloc). Delegates to Layout.tySize. -/
private def computeTySize (ty : Ty) : LowerM Nat := do
  let ctx ← getLayoutCtx
  return Layout.tySize ctx ty

/-- Bug 190: an array VALUE is the address of its storage in Lower (an `arrayLit` is its alloca,
    a variable read is the variable's slot), so binding one from a place, or passing one by value,
    shared the source's storage: `let t = xs; xs[0] = 9;` changed `t`. Wherever an array is bound
    or passed BY VALUE from an expression that is not a fresh value (an array literal or a call
    result), the value is copied into its own storage first. Arrays are Copy, so a copy is what the
    program means; the interpreter already copies. -/
private def snapshotArrayValue (value : CExpr) (val : SVal) : LowerM SVal := do
  match value.ty with
  | .array _ _ =>
    match value with
    | .arrayLit .. | .call .. => return val
    | _ =>
      let dst ← freshReg "arrcopy."
      emitEntryAlloca (.alloca dst value.ty)
      emit (.store val (.reg dst value.ty))
      return .reg dst value.ty
  | _ => return val

/-- Is this type an aggregate that should use stable alloca storage
    instead of phi-transporting through loop headers?
    Only true value-typed aggregates (structs, enums, strings, vecs, arrays)
    should be promoted. References and pointers are 8-byte scalars — use phi. -/
private def isAggregateForPromotion (ty : Ty) : LowerM Bool := do
  -- A newtype has its inner type's representation (bug 110 follow-up: `Buf([..])`
  -- for `newtype Buf = [i32; 3]` arriving at a match merge or loop header was
  -- phi-transported, which SSA verification refuses, E0714).
  let ty := Layout.resolveNewtype (← getLayoutCtx) ty
  match ty with
  -- References/pointers are 8-byte scalars — NOT aggregates for promotion
  | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ => return false
  -- Function pointers, heap pointers are scalars
  | .fn_ _ _ _ | .heap _ | .heapArray _ => return false
  -- Primitive scalars
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => return false
  | .float32 | .float64 | .bool | .char | .unit => return false
  | .never | .placeholder | .typeVar _ => return false
  -- True aggregates: structs, enums, strings, vecs, hashmaps, arrays
  | .string => return true
  | .array _ _ => return true
  | .generic "Vec" _ _ | .generic "HashMap" _ _ => return true
  | .generic "Heap" _ _ | .generic "HeapArray" _ _ => return false
  | .named name =>
    let ctx ← getLayoutCtx
    return (Layout.lookupStruct ctx name).isSome || (Layout.lookupEnum ctx name).isSome
  | .generic name _ _ =>
    let ctx ← getLayoutCtx
    return (Layout.lookupStruct ctx name).isSome || (Layout.lookupEnum ctx name).isSome

/-- Check if a variable is currently promoted to a stable alloca. -/
private def isPromoted (name : LocalKey) : LowerM (Option (String × Ty)) := do
  let s ← getState
  return s.promotedAllocas.get? name

/-- Add a promoted alloca for a variable. The NEWEST declaration of a name wins: Elab
    renames every binding that shadows a VISIBLE name (bug 087), so a Core name declared
    again is a sibling scope reusing a DEAD name — `if c { let mut x: [i32; 1] = [1]; .. }
    else { let mut x: [i32; 1] = [2]; .. }`. Appending made `find?` return the then-branch's
    storage inside the else branch, which on that path was never written (Codex review of
    bug 087). Prepending made every lookup resolve the declaration in scope; the table is now a
    map from the name to its newest slot (bug 262), and no caller registers a name already
    registered (each checks `isPromoted`, or a declaration first removes the dead binding's). -/
private def addPromotedAlloca (name : LocalKey) (allocaReg : String) (ty : Ty) : LowerM Unit := do
  let s ← getState
  setState { s with promotedAllocas := s.promotedAllocas.insert name (allocaReg, ty) }

/-- Remove promoted allocas by name (at loop exit). -/
private def removePromotedAllocas (names : List LocalKey) : LowerM Unit := do
  let s ← getState
  setState { s with promotedAllocas := names.foldl (·.erase ·) s.promotedAllocas }

/-- Extract type args from a Ty, unwrapping references/pointers. -/
private def typeArgsFromTy : Ty → List Ty
  | .generic _ args _ => args
  | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner => typeArgsFromTy inner
  | _ => []

/-- Get byte offset of a field within a struct definition. Delegates to Layout.fieldOffset. -/
private def fieldByteOffset (tyName : String) (fieldName : String) (typeArgs : List Ty := []) : LowerM Nat := do
  let ctx ← getLayoutCtx
  return Layout.fieldOffset ctx tyName fieldName typeArgs

/-- Push loop info onto the loop stack. -/
private def pushLoop (info : LoopInfo) : LowerM Unit := do
  let s ← getState
  setState { s with loopStack := info :: s.loopStack }

/-- Pop loop info from the loop stack, returning it. -/
private def popLoop : LowerM (Option LoopInfo) := do
  let s ← getState
  match s.loopStack with
  | info :: rest =>
    setState { s with loopStack := rest }
    return some info
  | [] => return none

/-- Get the innermost loop info. -/
private def currentLoop : LowerM (Option LoopInfo) := do
  let s ← getState
  return s.loopStack.head?

/-- Find a loop by label, returning it and its index in the stack. -/
private def findLoopByLabel (label : Option String) : LowerM (Option LoopInfo) := do
  match label with
  | none => currentLoop
  | some lbl =>
    let s ← getState
    match s.loopStack.find? fun info => info.loopLabel == some lbl with
    | some info => return some info
    | none => currentLoop

/-- Bug 149: how many loops a `break`/`continue` with this label leaves (1 for an unlabelled one,
    or for a label naming the innermost loop). -/
private def loopsExitedBy (label : Option String) : LowerM Nat := do
  match label with
  | none => return 1
  | some lbl =>
    let s ← getState
    match s.loopStack.findIdx? (fun info => info.loopLabel == some lbl) with
    | some i => return i + 1
    | none => return 1

/-- Record a break edge on a specific loop (identified by exitLabel). -/
private def addBreakEdgeToLoop (vars : VarMap) (srcLabel : String) (targetExitLabel : String) : LowerM Unit := do
  let s ← getState
  let newStack := s.loopStack.map fun info =>
    if info.exitLabel == targetExitLabel then
      { info with breakEdges := info.breakEdges ++ [(vars, srcLabel)] }
    else info
  setState { s with loopStack := newStack }

/-- Record a break edge on the innermost loop. -/
private def addBreakEdge (vars : VarMap) (label : String) : LowerM Unit := do
  let s ← getState
  match s.loopStack with
  | info :: rest =>
    let info' := { info with breakEdges := info.breakEdges ++ [(vars, label)] }
    setState { s with loopStack := info' :: rest }
  | [] => pure ()

/-- Record a continue edge on a specific loop (identified by headerLabel). -/
private def addContinueEdgeToLoop (vars : VarMap) (srcLabel : String) (targetHeaderLabel : String) : LowerM Unit := do
  let s ← getState
  let newStack := s.loopStack.map fun info =>
    if info.headerLabel == targetHeaderLabel then
      { info with continueEdges := info.continueEdges ++ [(vars, srcLabel)] }
    else info
  setState { s with loopStack := newStack }

/-- Record a continue edge on the innermost loop. -/
private def addContinueEdge (vars : VarMap) (label : String) : LowerM Unit := do
  let s ← getState
  match s.loopStack with
  | info :: rest =>
    let info' := { info with continueEdges := info.continueEdges ++ [(vars, label)] }
    setState { s with loopStack := info' :: rest }
  | [] => pure ()

/-- Peek at the continue edges of the innermost loop without popping. -/
private def peekContinueEdges : LowerM (List (VarMap × String)) := do
  let s ← getState
  match s.loopStack with
  | info :: _ => return info.continueEdges
  | [] => return []

/-- Prepend instructions to an already-finalized block. Bug 292: recorded and applied by `lowerFn`
    (`applyBlockPatches`); mapping the whole finished-block list per loop header was quadratic in
    the loops of a function. -/
private def prependInstsToBlock (label : String) (newInsts : List SInst) : LowerM Unit := do
  if !(← getState).finishedLabels.contains label then return
  modify fun s =>
    let (pre, suf) := s.blockPatches.getD label ([], [])
    { s with blockPatches := s.blockPatches.insert label (newInsts :: pre, suf) }

/-- Bug 292: the finished blocks with their recorded patches applied: the prefixes newest first
    (each later prepend went in front), the suffixes oldest first (each later append went after). -/
def applyBlockPatches (patches : Std.TreeMap String (List (List SInst) × List (List SInst)) compare)
    (blocks : List SBlock) : List SBlock :=
  if patches.isEmpty then blocks else
  blocks.map fun b =>
    match patches.get? b.label with
    | some (pre, suf) => { b with insts := pre.flatten ++ b.insts ++ suf.reverse.flatten }
    | none => b

/-- Get current var map snapshot. -/
private def snapshotVars : LowerM VarMap := do
  let s ← getState
  -- `__last_expr` is the internal trailing-value pseudo-variable (set by `.expr`
  -- lowering); it is not a real local. It must never participate in merge/loop phi
  -- reconciliation — a leaked `__last_expr` from a previous value block would build
  -- a spurious, mistyped, or non-dominated phi (E0708/E0710/E0715). Snapshots feed
  -- exactly that reconciliation, so drop it here at the single source. The trailing
  -- value is read separately via `lookupVar (.name "__last_expr")` (lastExprVal).
  return s.vars.erase (.name "__last_expr")

-- ============================================================
-- Expression and statement lowering
-- ============================================================

/-- The pointee of a reference or raw-pointer type, if it is one. -/
private def refPointee? : Ty → Option Ty
  | .ref t | .refMut t | .ptrMut t | .ptrConst t => some t
  | _ => none

/-- Does a borrow of type `refTy` of a value of type `innerTy` denote the value's
    own storage (`&x : &T` for `x : T`)? False for the retagging shapes, where the
    borrow's type does not point at the operand's type. -/
private def borrowsSlot (innerTy refTy : Ty) : Bool :=
  refPointee? refTy == some innerTy

/-- Address-of a LOCAL variable. Returns a pointer (retyped to `refTy`) to the
    local's stable storage, promoting the local to a stack alloca on first
    address-take so that the pointer actually aliases the variable. Reads and
    writes of the local then route through the alloca via `lookupVar`/`setVar`.
    Returns `none` for non-local inners and arrays (an array's SVal is already
    its storage), which the caller handles by its existing logic.
    (ROADMAP Phase 4 #44d — fixes raw-pointer / `&mut`-to-local not aliasing.)

    A local whose VALUE is itself a pointer (`*mut T`, `*const T`, `Heap<T>`, a
    reference) is a place like any other: `&mut p` is the address of `p`'s slot,
    never `p`'s value (bug 102: `&mut err as *mut *mut u8` passed the null `err`;
    bug 126: `&h` / `&mut h` on a `Heap<B>` local denote the slot `h`, the one-hop
    rule of MEMORY_SEMANTICS §5). The borrow's type says which is meant: the
    slot when `refTy` points at the local's own type (`borrowsSlot`); any other
    retagging shape keeps the caller's value path. -/
private def addrOfLocal (inner : CExpr) (refTy : Ty) : LowerM (Option SVal) := do
  match inner with
  | .ident name varTy =>
    -- Bug 123: a module constant has no storage, whatever its type (an array constant's
    -- element borrow `&A[0]` reaches here too). Refused unless a local of that name shadows it.
    -- A pure state read: `lookupVar` would emit a load of a promoted local (a dead instruction
    -- that renumbered every later temporary, golden ssa/cap_basic).
    let st ← getState
    let isLocal := st.vars.contains name.key || st.promotedAllocas.contains name.key
    if !isLocal && ((st.constants.any fun (n, _, _) => n == name.name)
        || st.importedConstants.any fun (n, _, _) => n == name.name) then
      throwLower s!"cannot take the address of module constant '{name}': a constant has no storage (bug 123)"
    match varTy with
    -- arrays are already stack-allocated: the array SVal is its storage
    | .array _ _ => return none
    | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ | .heap _ =>
      if !borrowsSlot varTy refTy then return none
      promoteForAddr name.key varTy refTy
    | _ => promoteForAddr name.key varTy refTy
  | _ => return none
where
  promoteForAddr (name : LocalKey) (varTy refTy : Ty) : LowerM (Option SVal) := do
      match ← isPromoted name with
      | some (reg, _) => return some (.reg reg refTy)
      | none =>
        let cur ← lookupVar name
        let slot ← freshReg "addr."
        emitEntryAlloca (.alloca slot varTy)
        match cur with
        | some cv => emit (.store cv (.reg slot varTy))
        | none => pure ()
        addPromotedAlloca name slot varTy
        return some (.reg slot refTy)

-- Which locals do `&name` / `&mut name` (or a `borrow name as ...` block) name anywhere
-- in these expressions/statements? A scalar loop variable whose address is taken
-- in the loop body must be promoted to a stable alloca BEFORE the loop (like
-- aggregates) rather than phi-carried — otherwise the address-of promotes it to
-- memory mid-body while the loop still phi-tracks it, and the two representations
-- diverge (lost condition / re-initialized counter / silent infinite loop — C9).
-- Bug 263: these were predicates asked once per binding in scope (`does this body take
-- `&name`?`), a walk of the body per binding at every branch and loop; the walk now runs once
-- and collects the names. `name ∈ cexprAddrTaken acc e` iff `name ∈ acc` or the old
-- `cexprTakesAddrOf name e` held (each clause below is the old one's disjunction as a union).
-- Every constructor is listed (bug 285): a sub-expression the walk skips is an address-take no
-- site sees.
mutual
partial def cexprAddrTaken (acc : LocalKeySet) : CExpr → LocalKeySet
  | .borrow inner _ => cexprAddrTaken (match inner with | .ident n _ => acc.insert n.key () | _ => acc) inner
  | .borrowMut inner _ => cexprAddrTaken (match inner with | .ident n _ => acc.insert n.key () | _ => acc) inner
  | .binOp _ l r _ => cexprAddrTaken (cexprAddrTaken acc l) r
  | .unaryOp _ o _ => cexprAddrTaken acc o
  | .call _ _ args _ => args.foldl cexprAddrTaken acc
  | .structLit _ _ fields _ => fields.foldl (fun acc fe => cexprAddrTaken acc fe.2) acc
  | .fieldAccess o _ _ => cexprAddrTaken acc o
  | .match_ s arms _ => arms.foldl cmatchArmAddrTaken (cexprAddrTaken acc s)
  | .deref inner _ => cexprAddrTaken acc inner
  | .arrayLit elems _ => elems.foldl cexprAddrTaken acc
  | .arrayIndex a i _ => cexprAddrTaken (cexprAddrTaken acc a) i
  | .cast inner _ => cexprAddrTaken acc inner
  | .allocCall inner alloc _ => cexprAddrTaken (cexprAddrTaken acc inner) alloc
  | .ifExpr c t e _ => cstmtsAddrTaken (cstmtsAddrTaken (cexprAddrTaken acc c) t) e
  -- Bug 285: an enum literal's payload is evaluated like a struct literal's fields; the walk had
  -- no case for it, so an address taken there was invisible to every site that asks (a pre-branch
  -- local was promoted lazily inside the branch, a loop variable was phi-carried and promoted
  -- mid-body). No wildcard: a new expression form is a compile error here, not a silent miss.
  | .enumLit _ _ _ fields _ => fields.foldl (fun acc fe => cexprAddrTaken acc fe.2) acc
  | .intLit .. | .floatLit .. | .boolLit _ | .strLit _ | .charLit _ | .ident .. | .fnRef .. => acc

partial def cguardAddrTaken (acc : LocalKeySet) : Option CExpr → LocalKeySet
  | some g => cexprAddrTaken acc g
  | none => acc

partial def cmatchArmAddrTaken (acc : LocalKeySet) : CMatchArm → LocalKeySet
  | .enumArm _ _ _ guard body => cstmtsAddrTaken (cguardAddrTaken acc guard) body
  | .litArm v guard body => cstmtsAddrTaken (cguardAddrTaken (cexprAddrTaken acc v) guard) body
  | .varArm _ _ guard body => cstmtsAddrTaken (cguardAddrTaken acc guard) body
  | .rangeArm lo hi _ guard body =>
    cstmtsAddrTaken (cguardAddrTaken (cexprAddrTaken (cexprAddrTaken acc lo) hi) guard) body

partial def cstmtAddrTaken (acc : LocalKeySet) : CStmt → LocalKeySet
  | .letDecl _ _ _ v => cexprAddrTaken acc v
  | .assign _ v => cexprAddrTaken acc v
  | .return_ (some v) _ => cexprAddrTaken acc v
  | .return_ none _ => acc
  | .expr e _ => cexprAddrTaken acc e
  | .ifElse c t e =>
    let acc := cstmtsAddrTaken (cexprAddrTaken acc c) t
    match e with | some el => cstmtsAddrTaken acc el | none => acc
  | .while_ c b _ step => cstmtsAddrTaken (cstmtsAddrTaken (cexprAddrTaken acc c) b) step
  | .fieldAssign o _ v => cexprAddrTaken (cexprAddrTaken acc o) v
  | .derefAssign t v => cexprAddrTaken (cexprAddrTaken acc t) v
  | .arrayIndexAssign a i v => cexprAddrTaken (cexprAddrTaken (cexprAddrTaken acc a) i) v
  | .break_ (some v) _ => cexprAddrTaken acc v
  | .break_ none _ => acc
  | .defer b => cexprAddrTaken acc b
  | .borrowIn var _ _ _ _ body => cstmtsAddrTaken (acc.insert var.key ()) body
  | .continue_ _ => acc

partial def cstmtsAddrTaken (acc : LocalKeySet) (body : List CStmt) : LocalKeySet :=
  body.foldl cstmtAddrTaken acc
end

/-- Bug 031: pre-promote any pre-branch local whose address is taken inside a
    branch arm/body (incl. method-call autoborrows — `x.m()` is `M_m(&x)` in
    Core). Lazy `addrOfLocal` promotion emits the initializing store inside the
    branch where the FIRST address-take is lowered; the promotion registration
    is global, so a sibling branch (and post-merge code on that path) reads an
    UNINITIALIZED alloca. Call this in the still-dominating block BEFORE the
    conditional terminator — alloca goes to entry, the store lands here. Same
    rule the while-loop lowering applies to its body (C9). Skips arrays (the
    array SVal is its storage) and already-promoted names; pointer-valued locals
    are promoted too, since `&mut p` is `p`'s slot (bugs 102, 126). `taken` = the names some branch
    takes `&name` of (`cstmtsAddrTaken` / `cmatchArmAddrTaken`), less those `excluded` says to skip;
    the bindings of `preVars` among them are visited in `preVars`'s listing order (bug 263). -/
private def prePromoteAddrTaken (preVars : VarMap) (taken : LocalKeySet)
    (excluded : LocalKey → Bool := fun _ => false) : LowerM Unit := do
  for (name, val) in preVars.restrictOrdered taken.keys do
    match val.ty with
    | .array _ _ => pure ()
    | vty =>
      if (← isPromoted name).isNone && !excluded name then
        let allocaReg ← freshReg "ifpro."
        emitEntryAlloca (.alloca allocaReg vty)
        emit (.store val (.reg allocaReg vty))
        addPromotedAlloca name allocaReg vty

/-- Bug 286: the address-taken set a branch site acts on, without walking every enclosing
    region again. `walk` is the site's own walk (`cstmtsAddrTaken` / `cmatchArmAddrTaken` /
    `cexprAddrTaken` of its region); a site at depth d of a nest used to run it on the n - d levels
    below, so lowering n nested branches took time n².

    Inside an enclosing region whose set is `bound`, the site's exact set is a subset of `bound`
    (its region's source is part of the enclosing region's). The site acts only on the bindings of
    `preVars` in its set that `relevant` holds of (the very test it applies before promoting); when
    no binding of `preVars` in `bound` passes it, no binding in the exact set does either, and
    `bound` stands in for the exact set: the site does exactly what it did with the walked set.
    Otherwise it walks. -/
private def regionAddrTaken (preVars : VarMap) (walk : Unit → LocalKeySet)
    (relevant : LocalKey → SVal → LowerM Bool) : LowerM LocalKeySet := do
  match (← getState).addrBound with
  | none => return walk ()
  | some bound =>
    for (name, val) in preVars.restrictOrdered bound.keys do
      if ← relevant name val then return walk ()
    return bound

/-- Bug 286: the test `prePromoteAddrTaken` applies to a binding of its `taken` set. -/
private def prePromoteRelevant (excluded : LocalKey → Bool := fun _ => false)
    (name : LocalKey) (val : SVal) : LowerM Bool := do
  match val.ty with
  | .array _ _ => return false
  | _ => return (← isPromoted name).isNone && !excluded name

/-- Bug 286: lower `act` (a region's source) with `taken` as the bound of every site inside it. -/
private def withAddrBound (taken : LocalKeySet) (act : LowerM α) : LowerM α := do
  let saved := (← getState).addrBound
  modify fun s => { s with addrBound := some taken }
  let r ← act
  modify fun s => { s with addrBound := saved }
  return r

/-- Per-site rules for `reconcileBranchVars`. The three branch merge loops
    (statement-if, if-expr, match) grew these shapes separately — bugs
    029/031/033/038 were each "one site missed one invariant", so ROADMAP 1a
    consolidates them into one function. The differences below could NOT be
    unified without changing emitted IR, so they stay here as explicit fields;
    do not "simplify" them away without a full battery run. -/
private structure BranchMergeRules where
  /-- Statement-if void rule: an arm whose end value is `.unit` for a
      NON-unit var is not a real change, and such an arm contributes the
      PRE-branch value (not the unit) to the merge. ifExpr/match use the
      plain rule: a `.unit` end value counts as changed and flows into the
      merge as-is. -/
  unitArmUsesPreVal : Bool
  /-- Bug 029: after the alloca merge, an ARRAY variable rebinds the merge
      alloca's ADDRESS (arrays live at addresses everywhere); any other
      aggregate binds a fresh by-value load. The match loop predates this
      rule and binds the load for arrays too (false) — a real divergence,
      kept byte-identical rather than fixed in this refactor. -/
  arrayRebindsAllocaAddr : Bool
  /-- Skip phi emission for a unit-typed variable (statement-if/ifExpr:
      no-op). The match loop has no such guard. -/
  skipUnitPhi : Bool
  /-- Exactly one live arm carried the (changed) variable: bind that arm's
      value directly (statement-if/ifExpr). The match loop leaves the
      variable alone in that case (its single-live-arm shape is handled
      outside the merge loop). -/
  singleIncomingRebinds : Bool
  /-- Register-name prefixes for the merge alloca / merge load / merge phi.
      SSA names are part of the emitted IR — kept byte-identical per site. -/
  mergePfx : String
  loadPfx : String
  phiPfx : String

/-- Statement-if (`.ifElse`) merge rules. -/
private def stmtIfMergeRules : BranchMergeRules where
  unitArmUsesPreVal := true
  arrayRebindsAllocaAddr := true
  skipUnitPhi := true
  singleIncomingRebinds := true
  mergePfx := "if.merge."
  loadPfx := "if.load."
  phiPfx := "if.phi."

/-- If-expression (`.ifExpr`) merge rules. -/
private def ifExprMergeRules : BranchMergeRules where
  unitArmUsesPreVal := false
  arrayRebindsAllocaAddr := true
  skipUnitPhi := true
  singleIncomingRebinds := true
  mergePfx := "ifexpr.merge."
  loadPfx := "ifexpr.load."
  phiPfx := "ifphi."

/-- Match (`.match_`) merge rules. -/
private def matchMergeRules : BranchMergeRules where
  unitArmUsesPreVal := false
  arrayRebindsAllocaAddr := false
  skipUnitPhi := false
  singleIncomingRebinds := false
  mergePfx := "match.merge."
  loadPfx := "match.load."
  phiPfx := "match.phi."

/-- Reconcile live pre-branch variables across the arms of a branch
    (statement-if, if-expr, or match) at the merge block. For each pre-branch
    variable, scan the LIVE arm-end snapshots — `(endVars, endLabel)` per
    arm; the caller filters out diverged arms (return/break/continue), which
    contribute nothing — detect whether any arm changed the variable, and
    merge the differing values: a phi for scalars, an entry-block alloca +
    `insertStoresBeforeTerm` (one store per arm) for aggregates.

    Invariants held here (each maps to a regression test):
    * A promoted variable is SKIPPED — the promoted alloca is the single
      source of truth, written through by every arm; reconciling it would
      re-store the stale pre-branch snapshot over the arms' writes (bugs
      031/038 — aggregates included).
    * Aggregates merge via entry alloca + per-arm store, never a phi
      (E0714, bug 033); arrays then rebind the alloca ADDRESS, other
      aggregates bind a fresh load (bug 029) — per
      `rules.arrayRebindsAllocaAddr`.
    * "Changed" detection and the void rule differ per site and are encoded
      in `rules`, NOT unified away (no behavior change — ROADMAP 1a).
    * Bug 263: only the bindings `VarMap.mergeCandidates` names are visited (in
      `preVars`'s listing order, as before): a binding no arm wrote whose value is a
      register is unchanged in every arm under every rule set, and visiting it did
      nothing but cost a search of every arm per binding at every merge. -/
private def reconcileBranchVars (rules : BranchMergeRules)
    (preVars : VarMap)
    (liveArms : List (VarMap × String)) : LowerM Unit := do
  for (name, preVal) in preVars.mergeCandidates (liveArms.map (·.1)) do
    -- A promoted variable is memory-backed and written through its alloca in
    -- the branches, so the alloca is the single source of truth —
    -- reconciling it would re-store the stale pre-branch snapshot at the
    -- merge (C9-class miscompile, bug 031 family). This holds for AGGREGATES
    -- too (bug 038: a string mutated via &mut in a branch arm was clobbered
    -- by the merge's stale table value — audit 2026-07-16 fuzz).
    if (← isPromoted name).isSome then continue
    let ty := preVal.ty
    let mut incomingRev : List (SVal × String) := []
    let mut anyChanged := false
    for (endVars, endLabel) in liveArms do
      match endVars.find? name with
      | none => pure ()
      | some v =>
        let changed :=
          if rules.unitArmUsesPreVal then
            -- Statement-if: a `.unit` end value only counts as a change for
            -- a unit-typed var ("void in non-void context" is not a change).
            match v with
            | .unit => ty == .unit
            | .reg n1 _ => match preVal with
              | .reg n2 _ => n1 != n2
              | _ => true
            | _ => true
          else
            match v, preVal with
            | .reg n1 _, .reg n2 _ => n1 != n2
            | _, _ => true
        if changed then anyChanged := true
        let contrib :=
          if rules.unitArmUsesPreVal then
            -- Statement-if: use the pre-branch value for void in a non-void
            -- context.
            match v with
            | .unit => if ty == .unit then v else preVal
            | _ => v
          else v
        incomingRev := (contrib, endLabel) :: incomingRev
    let incoming := incomingRev.reverse  -- bug 297: consed per arm, in arm order here
    if anyChanged then
      if incoming.length >= 2 then
        let isAgg ← isAggregateForPromotion ty
        if isAgg then
          -- Aggregates may not phi (E0714, bug 033): merge through an
          -- entry-block alloca, storing each arm's value in its own end
          -- block before the branch to the merge.
          let allocaReg ← freshReg rules.mergePfx
          emitEntryAlloca (.alloca allocaReg ty)
          insertStoresBeforeTerm (incoming.map fun (v, l) => (l, v)) (.reg allocaReg ty)
          match ty with
          | .array _ _ =>
            if rules.arrayRebindsAllocaAddr then
              -- Bug 029: arrays live at ADDRESSES everywhere (arrayLit
              -- returns its alloca; addrOfLocal assumes it; EmitSSA stores
              -- memcpy address-form aggregates). A by-value load rebind
              -- breaks that convention — post-merge `&arr` would use a
              -- [N x T] VALUE where its address should flow (llvm-as
              -- reject). Bind the merge alloca's ADDRESS instead; the
              -- memcpy stores above filled it.
              setVar name (.reg allocaReg ty)
            else
              let loadReg ← freshReg rules.loadPfx
              emit (.load loadReg (.reg allocaReg ty) ty)
              setVar name (.reg loadReg ty)
          | _ =>
            let loadReg ← freshReg rules.loadPfx
            emit (.load loadReg (.reg allocaReg ty) ty)
            setVar name (.reg loadReg ty)
        else if !rules.skipUnitPhi || ty != .unit then
          let phiReg ← freshReg rules.phiPfx
          emit (.phi phiReg incoming ty)
          setVar name (.reg phiReg ty)
        else pure ()
      else match incoming with
        | [(val, _)] =>
          if rules.singleIncomingRebinds then setVar name val
        | _ => pure ()

mutual

partial def lowerExpr (e : CExpr) : LowerM SVal := do
  match e with
  | .intLit v ty => return .intConst v ty
  | .floatLit v ty => return .floatConst v ty
  | .boolLit b => return .boolConst b
  | .strLit s =>
    let name ← internString s
    return .strConst name
  | .charLit c =>
    return .intConst (Int.ofNat c.toNat) .char

  | .ident name ty =>
    match ← lookupVar name.key with
    | some val => return val
    | none =>
      -- Check module-level constants and inline their value
      let s ← getState
      -- Constants reach Lower flattened by bare name (Core carries no module path), so two
      -- modules defining one name are indistinguishable here: a submodule's `A` read the
      -- top-level `A`. Refused at the use rather than guessed (as Mono does for type names).
      -- A method value names a method, never a constant of its spelling (bugs 238 – 239, Codex r16).
      let consts := if name.space == .method then [] else s.constants
      if (consts.filter fun (n, _, _) => n == name.name).length > 1 then
        throwLower s!"module constant '{name}' is defined by more than one module, and Core cannot tell which one this use names; rename one of them"
      match consts.find? fun (n, _, _) => n == name.name with
      | some (_, _, constExpr) =>
        -- Hygiene (bug 123 review): the initializer is module-scope code. Inlined in the
        -- caller's environment, a local shadowing a constant it names captured it
        -- (`const Y = X + 1` read a local `X`; `const Y = *&X` borrowed one). The caller's
        -- locals and promoted slots are hidden while it lowers, so a name in it is a constant
        -- (and `&X` there reaches addrOfLocal's refusal), then restored.
        let saved ← getState
        set { saved with vars := VarMap.empty, promotedAllocas := {}, inConstInit := true }
        let v ← lowerExpr constExpr
        modify fun st => { st with vars := saved.vars, promotedAllocas := saved.promotedAllocas,
                                   inConstInit := saved.inConstInit }
        return v
      | none =>
        -- R-0492 step 4 (bug 233): an imported constant, by its identity's Core spelling. Its
        -- initializer is module-scope code of the DEFINING module: lowered with that module's
        -- constants in scope (a constant it names is that module's), and hygienic as above.
        match s.importedConstants.find? fun (n, _, _) => n == name.name with
        | some (_, constExpr, scope) =>
          -- the initializer and every constant it reaches, in the scope it is inlined in
          let lookup : String → Option CExpr := fun n =>
            match scope.find? (·.1 == n) with
            | some (_, _, e) => some e
            | none => (s.importedConstants.find? (·.1 == n)).map (·.2.1)
          if initReachesCallable lookup (scope.length + s.importedConstants.length + 1) constExpr then
            throwLower s!"imported constant '{name}' cannot be inlined outside its module: its initializer calls or names a function, which Core names by a spelling the using function would resolve in its own context (bug 233; callables get identities in a later R-0492 step)"
          let saved ← getState
          set { saved with vars := VarMap.empty, promotedAllocas := {}, inConstInit := true, constants := scope }
          let v ← lowerExpr constExpr
          modify fun st => { st with vars := saved.vars, promotedAllocas := saved.promotedAllocas,
                                     inConstInit := saved.inConstInit, constants := saved.constants }
          return v
        | none =>
        -- If it's a function type and not a local var, treat as global function reference
        match ty with
        | .fn_ _ _ _ => return .fnRef name.name ty name.space
        | _ => return .reg name.name ty

  | .binOp op lhs rhs ty =>
    match op with
    | .and_ | .or_ =>
      -- SHORT-CIRCUIT lowering: the RHS must not evaluate when the LHS decides
      -- the result — `x != 0 && (10 / x) > 0` is the blessed guard idiom, and
      -- the RHS division must not trap when x == 0. Matches the interpreter
      -- and ProofCore. (Historically `&&`/`||` lowered EAGERLY; DCE deleting
      -- the unused trapping RHS masked the divergence until checked arithmetic
      -- became side-effecting, exposed by the 2026-07-03 nightly fuzz run.)
      let lVal ← lowerExpr lhs
      let pfx := if op == .and_ then "scand" else "scor"
      let rhsLabel ← freshLabel s!"{pfx}.rhs"
      let mergeLabel ← freshLabel s!"{pfx}.merge"
      let resultSlot ← freshReg s!"{pfx}slot."
      emit (.alloca resultSlot .bool)
      emit (.store lVal (.reg resultSlot .bool))
      let preVars ← snapshotVars
      let entryEndLabel ← getCurrentLabel
      -- Bug 034 (= the 031 class, missed at THIS branch site): the RHS block
      -- may address-take a local (method call borrowing it). Without
      -- pre-promotion the lazy alloca+store lands inside scand.rhs and does
      -- not dominate later uses — silent neighbor corruption (found by
      -- std.cli's `a.len >= 2 && a.get_unchecked(0) == '-'`).
      let rhsTaken ← regionAddrTaken preVars (fun _ => cexprAddrTaken {} rhs) (prePromoteRelevant)
      prePromoteAddrTaken preVars rhsTaken
      if op == .and_ then
        terminateBlock (.condBr lVal rhsLabel mergeLabel)
      else
        terminateBlock (.condBr lVal mergeLabel rhsLabel)
      startBlock rhsLabel
      let rVal ← withAddrBound rhsTaken (lowerExpr rhs)
      emit (.store rVal (.reg resultSlot .bool))
      let rhsEndVars ← snapshotVars
      let rhsEndLabel ← getCurrentLabel
      terminateBlock (.br mergeLabel)
      -- Merge: phi any scalar var the RHS remapped (a match/if-expression in
      -- the RHS can contain arm statements), mirroring ifExpr's merge.
      startBlock mergeLabel
      -- Bug 263: only the bindings the RHS wrote, or whose value is not a register, can count
      -- as changed below (`VarMap.mergeCandidates`).
      for (name, preVal) in preVars.mergeCandidates [rhsEndVars] do
        -- Promoted vars are memory-backed (their alloca slot carries the value;
        -- reads reload) — never phi them. Aggregates are NEVER phi'd either:
        -- emitting `phi %String` is invalid IR (E0714). A var only "changes"
        -- across a boolean `&&`/`||` RHS via reassignment, which promotes it —
        -- so a still-unpromoted aggregate cannot have changed, and skipping it
        -- is correct. (Bug found by the #35 conlog workload: `a && b` with a
        -- promoted `String` in scope produced an aggregate phi.)
        if (← isPromoted name).isSome then continue
        if (← isAggregateForPromotion preVal.ty) then continue
        let rhsV := rhsEndVars.find? name
        let changed := match rhsV with
          | some v => match v, preVal with
            | .reg n1 _, .reg n2 _ => n1 != n2
            | _, _ => true
          | none => false
        if changed then
          match rhsV with
          | some v =>
            let phiReg ← freshReg s!"{pfx}phi."
            emit (.phi phiReg [(v, rhsEndLabel), (preVal, entryEndLabel)] preVal.ty)
            setVar name (.reg phiReg preVal.ty)
          | none => pure ()
      let loadDst ← freshReg s!"{pfx}load."
      emit (.load loadDst (.reg resultSlot .bool) .bool)
      return .reg loadDst .bool
    | _ =>
      let lVal ← lowerExpr lhs
      let rVal ← lowerExpr rhs
      let dst ← freshReg
      emit (.binOp dst op lVal rVal ty)
      return .reg dst ty

  | .unaryOp op operand ty =>
    let oVal ← lowerExpr operand
    let dst ← freshReg
    emit (.unaryOp dst op oVal ty)
    return .reg dst ty

  | .call callee _typeArgs args ty =>
    -- Intrinsic identity is by-resolution: a DEFINED function is never
    -- intercepted, whatever it is named (mirrors Check/Elab guards). An INDIRECT
    -- callee is a value in scope and can never be an intrinsic either — a local
    -- fn-pointer named `print_int` would otherwise be intercepted (bug 050's
    -- shape, one pass later).
    let st ← getState
    let intrinsic := match callee with
      | .indirect _ => none
      -- A method call names a program method its type's method table resolved (bugs 238 – 239,
      -- Codex review r9: a method spelled `read_owned` in another module was lowered as the
      -- intrinsic); compiler intrinsics are free calls.
      | .direct _ .method => none
      | .direct fn .free => if st.definedFns.contains fn then none else resolveIntrinsic fn
      -- A compiler-generated runtime call is its helper whatever the program defines (bug 165).
      | .runtime helper => resolveIntrinsic helper
    -- Handle sizeof::<T>() and alignof::<T>() → compile-time constants
    if intrinsic == some .sizeof then
      let argTy := match _typeArgs with | t :: _ => t | [] => Ty.int
      let sz ← computeTySize argTy
      return .intConst (Int.ofNat sz) .uint
    if intrinsic == some .alignof then
      let argTy := match _typeArgs with | t :: _ => t | [] => Ty.int
      let ctx ← getLayoutCtx
      return .intConst (Int.ofNat (Layout.tyAlign ctx argTy)) .uint
    -- T-RAWDUP: `read_owned(p)` moves the value out of the slot: the load `*p` is, stated as a
    -- transfer (the checker refuses the bare `*p` for a non-Copy pointee).
    if intrinsic == some .readOwned then
      match args.head? with
      | some arg =>
        let pv ← lowerExpr arg
        let dst ← freshReg
        emit (.load dst pv ty)
        return .reg dst ty
      | none => return .unit
    -- Handle alloc(val) → malloc + store
    if intrinsic == some .alloc then
      match args.head? with
      | some arg =>
        let aVal ← lowerExpr arg
        let innerTy := arg.ty
        let szDst ← freshReg
        let sz ← computeTySize innerTy
        emit (.call (some szDst) (.foreign "malloc") [.intConst (Int.ofNat sz) .int] (.ptrMut innerTy))
        let ptrVal := SVal.reg szDst (.ptrMut innerTy)
        emit (.store aVal ptrVal)
        return ptrVal
      | none => return .unit
    -- Handle free(ptr) → free the pointer, return loaded value
    else if intrinsic == some .free then
      match args.head? with
      | some arg =>
        let ptrVal ← lowerExpr arg
        let innerTy := match arg.ty with
          | .heap t => t
          | .generic "Heap" [t] _ => t
          | t => t
        if ty == .unit || ty == .never then
          emit (.call none (.foreign "free") [ptrVal] .unit)
          return .unit
        else
          -- Load value before freeing
          let loadDst ← freshReg
          emit (.load loadDst ptrVal innerTy)
          emit (.call none (.foreign "free") [ptrVal] .unit)
          return .reg loadDst innerTy
      | none => return .unit
    else
    -- Track borrowMut args that need write-back after the call
    let mut aVals : List SVal := []
    let mut mutBorrows : List (LocalKey × String × Ty) := []  -- (var key, allocaReg, innerTy)
    for arg in args do
      match arg with
      | .borrowMut (.ident varName innerTy) _ =>
        refuseBorrowInConstInit
        -- If the variable is already promoted to a stable alloca (address taken,
        -- e.g. an address-taken loop counter), pass that alloca directly — the
        -- alloca IS the storage, so no copy + write-back is needed (and a
        -- write-back via setVar would desync from the promoted alloca).
        match ← isPromoted varName.key with
        | some (allocaReg, _) =>
          aVals := .reg allocaReg (.refMut innerTy) :: aVals
        | none =>
          -- For &mut borrows of variables: alloca, store current value, pass ptr
          -- After the call we'll load back to propagate mutations.
          -- The alloca is hoisted to the entry block so that loops don't
          -- grow the stack on every iteration.
          let curVal ← lowerExpr (.ident varName innerTy)
          let slot ← freshReg "mutref."
          emitEntryAlloca (.alloca slot innerTy)
          emit (.store curVal (.reg slot innerTy))
          aVals := .reg slot (.refMut innerTy) :: aVals
          mutBorrows := (varName.key, slot, innerTy) :: mutBorrows
      | _ =>
        -- All other borrow/place args (incl. `&mut o.f`, `&mut a[i]`,
        -- `&mut a[i].f`) lower through `borrowMut`/`placeAddr`, which GEPs into a
        -- STABLE base address (promoted local or pointer) — never a loaded copy.
        -- The old `&mut field` special case GEP'd into `lowerExpr obj`, a loaded
        -- struct VALUE, so the field write was silently lost.
        -- A by-value array argument is the callee's own copy (bug 190).
        let v ← snapshotArrayValue arg (← lowerExpr arg)
        aVals := v :: aVals
    -- Bug 297: the arguments and write-backs were appended one at a time (quadratic in the
    -- call's arity); they are consed and put back in argument order once.
    aVals := aVals.reverse
    mutBorrows := mutBorrows.reverse
    -- Resolve fn-pointer variables: if the call target is a local variable
    -- holding a fn pointer, resolve it to the actual function / register name.
    -- A statically-known target is an `SVal.fnRef`, so devirtualization here is
    -- reading a value's constructor rather than decoding a register name.
    -- For runtime registers, prefix with "%" to mark as indirect call target.
    let callTarget : SCallee ← do
      match callee with
      | .direct name sp => pure (.direct name sp)
      -- The helper's reserved `_C$rt.` symbol, which no Concrete definition can take (bug 165).
      | .runtime helper => pure (.foreign (Symbols.runtimeFn helper))
      | .indirect binding =>
        match ← lookupVar binding.key with
        -- Devirtualization: the binding holds a statically-known function, so
        -- this becomes a direct call. It is now a decision about a VALUE's
        -- constructor, not a string prefix a later pass has to re-parse.
        | some (.fnRef fnName (.fn_ _ _ _) sp) => pure (.direct fnName sp)
        | some (v@(.reg _ (.fn_ _ _ _))) => pure (.indirect v)
        | _ =>
          -- The call form says this callee is a fn-typed binding, so failing to
          -- find one is a Core/Lower disagreement, not a program error. Emitting
          -- the bare name would silently produce a direct call to whatever global
          -- shares it — the bug-050 outcome.
          throwLower s!"Lower: indirect callee '{binding}' is not bound to a fn-pointer register"
    if ty == .unit || ty == .never then
      emit (.call none callTarget aVals ty)
      -- Write back mutably borrowed variables
      for (varName, slot, innerTy) in mutBorrows do
        let loadBack ← freshReg "wb."
        emit (.load loadBack (.reg slot innerTy) innerTy)
        setVar varName (.reg loadBack innerTy)
      return .unit
    else
      let dst ← freshReg
      emit (.call (some dst) callTarget aVals ty)
      -- Write back mutably borrowed variables
      for (varName, slot, innerTy) in mutBorrows do
        let loadBack ← freshReg "wb."
        emit (.load loadBack (.reg slot innerTy) innerTy)
        setVar varName (.reg loadBack innerTy)
      return .reg dst ty

  | .structLit name _typeArgs fields ty =>
    -- Bug 127: initializers are evaluated in SOURCE order (STATEMENT_EXPRESSION_MODEL,
    -- evaluation order rule 4), the order of `fields` as Elab built it and as
    -- `Interp.evalFields` folds it. Each value is stored at its field's DECLARED offset,
    -- looked up by name, so evaluation order and layout are independent. (This loop used
    -- to reorder `fields` into declaration order first — the "Bug #7" fix from when
    -- offsets were accumulated positionally — which made the binary run `P { b: f(), a: g() }`
    -- as `g()` then `f()`.)
    let dst ← freshReg
    emit (.alloca dst ty)
    let baseVal := SVal.reg dst ty
    -- Store each field at the SAME aligned offset that `.fieldAccess` reads it
    -- from (`Layout.fieldOffset`). Previously this packed fields tightly by
    -- summing `computeTySize`, which disagreed with the aligned read offsets
    -- for any struct with a sub-word field followed by a wider one — e.g.
    -- `{a: u8, b: i64}` stored `b` at offset 1 but read it from offset 8,
    -- a silent miscompile (ROADMAP Phase 4 #44e).
    let structTyArgs := typeArgsFromTy ty
    for (fname, fieldExpr) in fields do
      let fVal ← lowerExpr fieldExpr
      let byteOffset ← fieldByteOffset name fname structTyArgs
      let gepDst ← freshReg
      emit (.gep gepDst baseVal [.intConst (Int.ofNat byteOffset) .int] .i8)
      emit (.store fVal (.reg gepDst fieldExpr.ty))
    let loadDst ← freshReg
    emit (.load loadDst baseVal ty)
    return .reg loadDst ty

  | .fieldAccess obj field ty =>
    -- Bug 250: a field of a promoted local (or of a field chain of one) is read at its address;
    -- `lowerExpr obj` loaded the whole struct to read one field.
    let oVal ← if ← inPlaceRoot obj then
        match ← placeAddr obj (.ptrMut obj.ty) with
        | some p => pure p
        | none => lowerExpr obj
      else lowerExpr obj
    let tyName ← structNameFromTy obj.ty
    let byteOff ← fieldByteOffset tyName field (typeArgsFromTy obj.ty)
    let dst ← freshReg
    -- Use byte-offset GEP: gep i8, ptr, byteOffset
    emit (.gep dst oVal [.intConst (Int.ofNat byteOff) .int] .i8)
    let loadDst ← freshReg
    emit (.load loadDst (.reg dst ty) ty)
    return .reg loadDst ty

  | .enumLit enumName variant _typeArgs fields ty =>
    -- Bug #5 fix: store discriminant tag at index 0, fields starting at index 1
    let vidx ← variantIndex enumName variant
    let dst ← freshReg
    emit (.alloca dst ty)
    let baseVal := SVal.reg dst ty
    -- Store tag as i32 at offset 0
    emit (.store (.intConst (Int.ofNat vidx) .i32) baseVal)
    -- GEP to payload using aligned offset (after i32 tag, with padding)
    let layoutCtx ← getLayoutCtx
    let enumTypeArgs := typeArgsFromTy ty
    let vfields ← variantFields enumName variant enumTypeArgs
    let ed := Layout.lookupEnum layoutCtx enumName
    let payloadOff := match ed with
      | some ed => Layout.enumPayloadOffset layoutCtx ed enumTypeArgs
      | none => 4
    let payloadPtr ← freshReg
    emit (.gep payloadPtr baseVal [.intConst (Int.ofNat payloadOff) .int] .i8)
    for (idx, (_, fieldExpr)) in enumerate fields do
      let fVal ← lowerExpr fieldExpr   -- always evaluate (may have side effects)
      -- A Unit/void field is zero-size: evaluate it (e.g. a void-returning call)
      -- but store nothing — `store void ...` is not valid (e.g. Option<R> where a
      -- generic R was instantiated to Unit by a void-returning HOF callback).
      if fieldExpr.ty != .unit then
        let gepDst ← freshReg
        let foff := Layout.variantFieldOffset layoutCtx vfields idx
        emit (.gep gepDst (.reg payloadPtr .i8) [.intConst (Int.ofNat foff) .int] .i8)
        emit (.store fVal (.reg gepDst fieldExpr.ty))
    let loadDst ← freshReg
    emit (.load loadDst baseVal ty)
    return .reg loadDst ty

  | .match_ scrutinee arms ty =>
    -- Bug #3 fix: actual pattern comparison dispatch
    -- Bug #4 fix: capture real result values for phi nodes
    -- A binder arm binds the scrutinee BY VALUE (bug 190: an array scrutinee is copied).
    let scrVal ← snapshotArrayValue scrutinee (← lowerExpr scrutinee)
    let mergeLabel ← freshLabel "merge"
    let mergeDst ← freshReg
    let mut phiIncoming : List (SVal × String) := []
    let mut allArmsTerminated := true
    -- Snapshot vars before match so each arm starts from the same state
    let preMatchVars ← snapshotVars
    -- Collect (endVars, endLabel, terminated) for each arm for var merge (newest first)
    let mut armEndSnapshots : List (VarMap × String × Bool) := []

    -- Bug 031: see prePromoteAddrTaken — the store must dominate every arm.
    -- Exclude names any arm REBINDS (enum payload binding / var arm): promotion
    -- is keyed by name, so a shadowed binding's setVar would store the arm-local
    -- value into the OUTER variable's alloca.
    let armShadows (n : LocalKey) : Bool := arms.any fun arm => match arm with
      | .enumArm _ _ bindings _ _ => bindings.any fun (bn, _) => bn.key == n
      | .varArm v _ _ _ => v.key == n
      | _ => false
    let armsTaken ← regionAddrTaken preMatchVars (fun _ => arms.foldl cmatchArmAddrTaken {})
      (prePromoteRelevant armShadows)
    prePromoteAddrTaken preMatchVars armsTaken armShadows
    -- Bug 286: the arms (patterns, guards, bodies) are lowered with their set as the bound;
    -- restored at the merge, which every path of the arm loops reaches.
    let outerAddrBound := (← getState).addrBound
    modify fun s => { s with addrBound := some armsTaken }

    -- Determine if this is an enum match (check first arm)
    let isEnumMatch := arms.any fun arm => match arm with
      | .enumArm .. => true
      | _ => false

    if isEnumMatch then
      -- Load tag as i32 from offset 0, then extend to i64 for comparison
      let tagRaw ← freshReg
      emit (.load tagRaw scrVal .i32)
      let tagVal ← freshReg
      emit (.cast tagVal (.reg tagRaw .i32) .int)

      -- Generate comparison chain
      -- Bug 213: an arm after an unguarded binding (or `_`) arm can never be selected, and
      -- the block it would be lowered into has no predecessor (the binding arm branches
      -- unconditionally), so uses of earlier values there failed SSA verification (E0703).
      -- The source admits such arms (PAT-3 refuses only a repeated variant); they are not
      -- lowered.
      let mut catchAllSeen := false
      for (idx, arm) in enumerate arms do
        if catchAllSeen then continue
        if let .varArm _ _ none _ := arm then catchAllSeen := true
        -- Restore vars to pre-match state for each arm
        let st ← getState
        setState { st with vars := preMatchVars }
        let armLabel ← freshLabel s!"arm{idx}"
        let nextCheck ← freshLabel s!"check{idx + 1}"
        match arm with
        | .enumArm enumName variant bindings guard body =>
          let vidx ← variantIndex enumName variant
          let cmpDst ← freshReg
          emit (.binOp cmpDst .eq (.reg tagVal .int) (.intConst (Int.ofNat vidx) .int) .bool)
          terminateBlock (.condBr (.reg cmpDst .bool) armLabel nextCheck)
          startBlock armLabel
          -- GEP past i32 tag to payload using aligned offset
          let layoutCtx ← getLayoutCtx
          let enumTypeArgs := typeArgsFromTy scrutinee.ty
          let vfields ← variantFields enumName variant enumTypeArgs
          let ed := Layout.lookupEnum layoutCtx enumName
          let payloadOff := match ed with
            | some ed => Layout.enumPayloadOffset layoutCtx ed enumTypeArgs
            | none => 4
          let payloadGep ← freshReg
          emit (.gep payloadGep scrVal [.intConst (Int.ofNat payloadOff) .int] .i8)
          for (fieldIdx, (bname, bty)) in enumerate bindings do
            -- `_` is a wildcard field: skip the load/bind entirely (not read).
            if bname.name != "_" then
              let gepDst ← freshReg
              let foff := Layout.variantFieldOffset layoutCtx vfields fieldIdx
              -- The base operand carries the binder's type for a borrowed binder: SSA cleanup
              -- folds a zero-offset GEP into its base operand, which must then still be typed
              -- as the reference the binder is.
              let baseTy := if binderIsBorrow bty then bty else .i8
              emit (.gep gepDst (.reg payloadGep baseTy) [.intConst (Int.ofNat foff) .int] .i8)
              -- D3 (bug 181): a borrowed binder (reference-typed in Core, Concrete/Semantics/
              -- BindingMode.lean) is the payload field's ADDRESS in the scrutinee's storage; a
              -- by-value binder is its value.
              if binderIsBorrow bty then
                setVar bname.key (.reg gepDst bty)
              else
                let loadDst ← freshReg
                emit (.load loadDst (.reg gepDst bty) bty)
                setVar bname.key (.reg loadDst bty)
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := i :: phiIncoming
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := snap :: armEndSnapshots
          startBlock nextCheck
        | .varArm binding _bindTy guard body =>
          terminateBlock (.br armLabel)
          startBlock armLabel
          if binding.name != "_" then setVar binding.key scrVal
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := i :: phiIncoming
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := snap :: armEndSnapshots
          startBlock nextCheck
        | .litArm litVal guard body =>
          let litSVal ← lowerExpr litVal
          let cmpDst ← freshReg
          emit (.binOp cmpDst .eq scrVal litSVal .bool)
          terminateBlock (.condBr (.reg cmpDst .bool) armLabel nextCheck)
          startBlock armLabel
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := i :: phiIncoming
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := snap :: armEndSnapshots
          startBlock nextCheck
        | .rangeArm loE hiE inclusive guard body =>
          -- Range pattern: lo <= scr && scr (<|<=) hi. (On an enum scrutinee this
          -- is ill-typed and won't occur in valid code; lowered for totality.)
          let loV ← lowerExpr loE
          let hiV ← lowerExpr hiE
          let geLo ← freshReg
          emit (.binOp geLo .geq scrVal loV .bool)
          let leHi ← freshReg
          emit (.binOp leHi (if inclusive then .leq else .lt) scrVal hiV .bool)
          let inRange ← freshReg
          emit (.binOp inRange .and_ (.reg geLo .bool) (.reg leHi .bool) .bool)
          terminateBlock (.condBr (.reg inRange .bool) armLabel nextCheck)
          startBlock armLabel
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := i :: phiIncoming
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := snap :: armEndSnapshots
          startBlock nextCheck
      -- After all checks: fallthrough is unreachable (match is exhaustive or
      -- catch-all arms consume remaining cases). Mark as unreachable.
      let term ← currentBlockTerminated
      if !term then
        terminateBlock .unreachable
    else
      -- Non-enum match (literal/variable patterns). If the scrutinee is a
      -- reference (`match &x { 0 => … }`), deref it once so scalar comparisons
      -- and variable bindings see the value, mirroring Check's auto-deref of the
      -- scrutinee type. (The enum branch above already loads through the pointer.)
      -- A variable arm over a reference binds the reference itself (D3), so keep it.
      let scrRef := scrVal
      let scrVal ← match scrutinee.ty with
        | .ref t | .refMut t => do
          let d ← freshReg "mderef."
          emit (.load d scrVal t)
          pure (.reg d t)
        | _ => pure scrVal
      -- Bug 213: an arm after an unguarded binding (or `_`) arm can never be selected, and
      -- the block it would be lowered into has no predecessor (the binding arm branches
      -- unconditionally), so uses of earlier values there failed SSA verification (E0703).
      -- The source admits such arms (PAT-3 refuses only a repeated variant); they are not
      -- lowered.
      let mut catchAllSeen := false
      for (idx, arm) in enumerate arms do
        if catchAllSeen then continue
        if let .varArm _ _ none _ := arm then catchAllSeen := true
        -- Restore vars to pre-match state for each arm
        let st ← getState
        setState { st with vars := preMatchVars }
        let armLabel ← freshLabel s!"arm{idx}"
        let nextCheck ← freshLabel s!"check{idx + 1}"
        match arm with
        | .litArm litVal guard body =>
          let litSVal ← lowerExpr litVal
          let cmpDst ← freshReg
          emit (.binOp cmpDst .eq scrVal litSVal .bool)
          terminateBlock (.condBr (.reg cmpDst .bool) armLabel nextCheck)
          startBlock armLabel
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := i :: phiIncoming
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := snap :: armEndSnapshots
          startBlock nextCheck
        | .rangeArm loE hiE inclusive guard body =>
          -- Range pattern: matches when lo <= scr && scr (<= | <) hi. Comparison
          -- signedness follows the scrutinee's type in EmitSSA (u8 -> unsigned).
          let loV ← lowerExpr loE
          let hiV ← lowerExpr hiE
          let geLo ← freshReg
          emit (.binOp geLo .geq scrVal loV .bool)
          let leHi ← freshReg
          emit (.binOp leHi (if inclusive then .leq else .lt) scrVal hiV .bool)
          let inRange ← freshReg
          emit (.binOp inRange .and_ (.reg geLo .bool) (.reg leHi .bool) .bool)
          terminateBlock (.condBr (.reg inRange .bool) armLabel nextCheck)
          startBlock armLabel
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := i :: phiIncoming
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := snap :: armEndSnapshots
          startBlock nextCheck
        | .varArm binding _bindTy guard body =>
          terminateBlock (.br armLabel)
          startBlock armLabel
          if binding.name != "_" then setVar binding.key (if binderIsBorrow _bindTy then scrRef else scrVal)
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := i :: phiIncoming
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := snap :: armEndSnapshots
          startBlock nextCheck
        | .enumArm _ _ _ guard body =>
          terminateBlock (.br armLabel)
          startBlock armLabel
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := i :: phiIncoming
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := snap :: armEndSnapshots
          startBlock nextCheck
      let term ← currentBlockTerminated
      if !term then
        -- Fallthrough after all check blocks — if the match is exhaustive
        -- (catch-all or complete enum), this block is unreachable.
        -- Mark it as unreachable to avoid creating dead-code SSA artifacts.
        terminateBlock .unreachable

    modify fun s => { s with addrBound := outerAddrBound }
    startBlock mergeLabel
    -- Merge variables modified in match arms (shared with the if sites via
    -- reconcileBranchVars — ROADMAP 1a). Only arms that reached the merge
    -- contribute; diverged arms were recorded with an empty snapshot.
    -- Both arm lists were accumulated newest-first (prepend, O(1) per arm; the old
    -- `++ [x]` per arm was O(arms²), bug 178); restore arm order once here.
    let liveSnapshots := armEndSnapshots.reverse.filter fun (_, _, term) => !term
    if liveSnapshots.length >= 2 then
      -- Bug 271: the merge starts from the table of a
      -- LIVE arm. The state here is whatever the last-lowered arm left, and when that arm
      -- diverged (return/break/continue) its writes are on no path to the merge; the reconcile
      -- below rebinds only the bindings the live arms changed, so a binding only the diverged
      -- arm wrote kept the diverged arm's value (silent wrong code, or E0703).
      match armEndSnapshots.head?, liveSnapshots.getLast? with
      | some (_, _, true), some (endVars, _, _) => setState { (← getState) with vars := endVars }
      | _, _ => pure ()
      reconcileBranchVars matchMergeRules preMatchVars
        (liveSnapshots.map fun (endVars, endLabel, _) => (endVars, endLabel))
      -- WC-0004: arm-local enum-payload bindings (e.g. `Check::Fail { code }`)
      -- enter the var-table via `setVar` inside the arm body. After merge,
      -- those bindings are out of scope but still leak into vars; the next
      -- match's preMatchVars snapshot then sees them, builds a phi across
      -- arms that don't all bind them, and produces a dominator violation
      -- (E0708) when an arm without the binding pulls a value defined
      -- inside a different arm. Restrict vars back to preMatchVars's name
      -- set; merged updates from the phi pass survive because we just
      -- wrote them via `setVar`.
      let st ← getState
      setState { st with vars := preMatchVars.restrictTo st.vars }
    else if liveSnapshots.length == 1 then
      -- Only one arm reached merge — use its vars directly, then drop
      -- arm-local bindings (same WC-0004 leakage applies).
      match liveSnapshots with
      | [(endVars, _, _)] =>
        let st ← getState
        setState { st with vars := preMatchVars.restrictTo endVars }
      | _ => pure ()
    else
      -- Bug 175: every arm diverged (return/break/continue), so nothing reaches
      -- the merge. Restore the pre-match vars and terminate it HERE, before the
      -- result handling below: that handling returns early for a unit-typed
      -- match (or one with no real results), which used to leave the merge open.
      -- An open merge looked like a fall-through, so the statements after the
      -- match were lowered into an unreachable block, and inside a loop that
      -- block branched to the header and fed the header phi a value its
      -- definition cannot dominate (E0708). The same shape bug 078 closed for
      -- the two `if` forms.
      let st ← getState
      setState { st with vars := preMatchVars }
      terminateBlock .unreachable
      return .unit
    -- Handle the match result value phi
    -- Filter out .unit values (arms that produce no result, e.g. side-effect blocks)
    let realPhiIncoming := phiIncoming.reverse.filter fun (v, _) => match v with | .unit => false | _ => true
    if ty == .unit || ty == .never || realPhiIncoming.isEmpty then
      -- Void-typed match or no real result values — no result to merge
      return .unit
    else if realPhiIncoming.length > 1 then
      let isAgg ← isAggregateForPromotion ty
      if isAgg then
        let allocaReg ← freshReg "match.res."
        emitEntryAlloca (.alloca allocaReg ty)
        insertStoresBeforeTerm (realPhiIncoming.map fun (v, l) => (l, v)) (.reg allocaReg ty)
        let loadReg ← freshReg "match.rload."
        emit (.load loadReg (.reg allocaReg ty) ty)
        return .reg loadReg ty
      else
        emit (.phi mergeDst realPhiIncoming ty)
        return .reg mergeDst ty
    else
      -- Exactly one real incoming value (empty was handled above; an all-diverged
      -- match terminated its merge before reaching here — bug 175).
      match realPhiIncoming with
      | [(val, _)] => return val
      | _ => return .unit

  | .borrow inner ty =>
    refuseBorrowInConstInit
    -- Reborrow: `&(*r)` has the same address as `r`, so it must lower to the
    -- pointer `r` itself — never a copy. Without this, `&*ctx` would load the
    -- pointee into a temp and take *its* address, so the borrow would not alias
    -- the original storage (the mutable-context-threading miscompile: a callback
    -- given `&*ctx` repeatedly would see throwaway copies). Handle it before
    -- addrOfLocal, since `*e` is never a plain local name.
    match inner with
    | .deref refExpr _ =>
      let rVal ← lowerExpr refExpr
      let dst ← freshReg
      emit (.cast dst rVal ty)
      return .reg dst ty
    | _ =>
    -- Taking the address of a LOCAL must yield the address of that local's
    -- storage, not a fresh copy — otherwise a pointer/ref to a local does not
    -- alias the local (ROADMAP Phase 4 #44d / H5). Promote the local to a
    -- stable alloca; `lookupVar`/`setVar` then route all reads/writes through
    -- it, so the returned pointer aliases the variable. `placeAddr` also covers
    -- element/field places (`&a[i]`, `&o.f`) by GEP, not a copy.
    match ← placeAddr inner ty with
    | some p => return p
    | none =>
    let iVal ← lowerExpr inner
    let innerTy := iVal.ty
    -- Special case: borrowing a string literal → strConstRef (no heap alloc)
    match iVal with
    | .strConst name => return .strConstRef name
    | _ =>
    -- If the inner value is not already a pointer/ref, alloca + store to get an address.
    -- A pointer-valued operand borrowed AS ITSELF (`&e : &*mut T`) is a temporary place
    -- like any other value (bugs 102, 126); only a retagging borrow casts the pointer.
    match innerTy with
    | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ | .heap _ =>
      if borrowsSlot innerTy ty then
        let slot ← freshReg "borrow."
        emit (.alloca slot innerTy)
        emit (.store iVal (.reg slot innerTy))
        return .reg slot ty
      let dst ← freshReg
      emit (.cast dst iVal ty)
      return .reg dst ty
    | .array _ _ =>
      -- Arrays are always stack-allocated (the SVal is already a pointer).
      -- No codegen needed — just retype the register.
      match iVal with
      | .reg name _ => return .reg name ty
      | _ => return iVal
    | _ =>
      let slot ← freshReg "borrow."
      emit (.alloca slot innerTy)
      emit (.store iVal (.reg slot innerTy))
      return .reg slot ty

  | .borrowMut inner ty =>
    refuseBorrowInConstInit
    -- Reborrow: `&mut (*r)` has the same address as `r`. This is what makes a
    -- `&mut Ctx` threadable across repeated callback calls — each `&mut *ctx`
    -- is a fresh reborrow that ends when the callee returns, never a copy. See
    -- the `.borrow` case above for why the copy path would be a miscompile.
    match inner with
    | .deref refExpr _ =>
      let rVal ← lowerExpr refExpr
      let dst ← freshReg
      emit (.cast dst rVal ty)
      return .reg dst ty
    | _ =>
    match ← placeAddr inner ty with
    | some p => return p
    | none =>
    let iVal ← lowerExpr inner
    let innerTy := iVal.ty
    match innerTy with
    | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ | .heap _ =>
      if borrowsSlot innerTy ty then
        let slot ← freshReg "borrowmut."
        emit (.alloca slot innerTy)
        emit (.store iVal (.reg slot innerTy))
        return .reg slot ty
      let dst ← freshReg
      emit (.cast dst iVal ty)
      return .reg dst ty
    | .array _ _ =>
      match iVal with
      | .reg name _ => return .reg name ty
      | _ => return iVal
    | _ =>
      let slot ← freshReg "borrowmut."
      emit (.alloca slot innerTy)
      emit (.store iVal (.reg slot innerTy))
      return .reg slot ty

  | .deref inner ty =>
    let iVal ← lowerExpr inner
    let dst ← freshReg
    emit (.load dst iVal ty)
    return .reg dst ty

  | .arrayLit elems ty =>
    let dst ← freshReg
    emit (.alloca dst ty)
    let baseVal := SVal.reg dst ty
    let elemTy := match ty with | .array t _ => t | _ => .placeholder
    -- Bug 027: a long literal of one repeated constant (`[v; n]`, which the parser expands to n
    -- copies of `v`) is filled by a loop. One `store` per element made a block of n stores, and
    -- clang -O2's instruction scheduler is superlinear in such a block (two 100000-element
    -- literals and a loop: 28 s). The constant has no effects, so storing it n times in a loop
    -- writes the same bytes.
    let uniform : Option CExpr := match elems with
      | first :: rest =>
        let same : CExpr → Bool := match first with
          | .intLit v t => fun e => match e with | .intLit v' t' => v' == v && t' == t | _ => false
          | .boolLit b => fun e => match e with | .boolLit b' => b' == b | _ => false
          | .charLit c => fun e => match e with | .charLit c' => c' == c | _ => false
          | _ => fun _ => false
        if elems.length ≥ 64 && rest.all same && same first then some first else none
      | [] => none
    if let some c := uniform then
      let eVal ← lowerExpr c
      let n := elems.length
      let preLabel ← getCurrentLabel
      let hdr ← freshLabel "afill.hdr"
      let body ← freshLabel "afill.body"
      let exit ← freshLabel "afill.exit"
      terminateBlock (.br hdr)
      startBlock hdr
      let iReg ← freshReg "afill.i"
      let nextReg ← freshReg "afill.next"
      emit (.phi iReg [(.intConst 0 .int, preLabel), (.reg nextReg .int, body)] .int)
      let cmp ← freshReg
      emit (.binOp cmp .lt (.reg iReg .int) (.intConst (Int.ofNat n) .int) .bool)
      terminateBlock (.condBr (.reg cmp .bool) body exit)
      startBlock body
      let gepDst ← freshReg
      emit (.gep gepDst baseVal [.reg iReg .int] elemTy)
      emit (.store eVal (.reg gepDst elemTy))
      emit (.binOp nextReg .wrappingAdd (.reg iReg .int) (.intConst 1 .int) .int)
      terminateBlock (.br hdr)
      startBlock exit
      return baseVal
    for (idx, elem) in enumerate elems do
      let eVal ← lowerExpr elem
      let gepDst ← freshReg
      emit (.gep gepDst baseVal [.intConst (Int.ofNat idx) .int] elemTy)
      emit (.store eVal (.reg gepDst elemTy))
    -- Return alloca pointer directly (don't load) so mutations work
    return baseVal

  | .arrayIndex arr index ty =>
    -- Bug 250: an array field of a promoted local is read at its address; `lowerExpr arr` loaded
    -- the whole array (and the struct holding it) to read one element.
    let aVal ← match arr with
      | .fieldAccess .. =>
        if pureIndexExpr index && (← inPlaceRoot arr) then
          match ← placeAddr arr (.ptrMut arr.ty) with
          | some p => pure p
          | none => lowerExpr arr
        else lowerExpr arr
      | _ => lowerExpr arr
    let iVal ← lowerExpr index
    match arrayLenOfTy arr.ty with        -- H8: bounds-check the read
    | some n => emitBoundsCheck iVal n
    | none => pure ()
    let gepDst ← freshReg
    emit (.gep gepDst aVal [iVal] ty)
    let loadDst ← freshReg
    emit (.load loadDst (.reg gepDst ty) ty)
    return .reg loadDst ty

  | .cast inner targetTy =>
    let iVal ← lowerExpr inner
    let dst ← freshReg
    emit (.cast dst iVal targetTy)
    return .reg dst targetTy

  | .fnRef name ty =>
    return .fnRef name ty .free

  | .allocCall inner _allocExpr _ty =>
    lowerExpr inner


  | .ifExpr cond then_ else_ ty =>
    let condVal ← lowerExpr cond
    let thenLabel ← freshLabel "ifexpr.then"
    let elseLabel ← freshLabel "ifexpr.else"
    let mergeLabel ← freshLabel "ifexpr.merge"
    -- Create result slot (skipped for a unit/never-typed if-expr — no value to
    -- materialize, and `alloca void` is illegal IR).
    let resultSlot? ← freshResultSlot "ifslot." ty
    let preIfVars ← snapshotVars
    -- Bug 031: see prePromoteAddrTaken — the store must dominate both branches.
    let branchTaken ← regionAddrTaken preIfVars
      (fun _ => cstmtsAddrTaken (cstmtsAddrTaken {} then_) else_) (prePromoteRelevant)
    prePromoteAddrTaken preIfVars branchTaken
    terminateBlock (.condBr condVal thenLabel elseLabel)
    -- Then block. Compute termination BEFORE storing the branch value: a branch
    -- that diverged (ended in return/break/continue) has no value, so storing
    -- `lastExprVal` would emit `store void undef` into the result slot — dead code
    -- after the terminator, but LLVM still type-checks it and rejects the void
    -- store. Only the live, value-producing branch writes the slot.
    startBlock thenLabel
    withAddrBound branchTaken (lowerStmts then_)
    let term1 ← currentBlockTerminated
    storeBranchResult term1 resultSlot? then_ ty "ifcast."
    let thenEndVars ← snapshotVars
    let thenEndLabel ← getCurrentLabel
    if !term1 then
      terminateBlock (.br mergeLabel)
    -- Else block
    let s ← getState
    setState { s with vars := preIfVars }
    startBlock elseLabel
    withAddrBound branchTaken (lowerStmts else_)
    let term2 ← currentBlockTerminated
    storeBranchResult term2 resultSlot? else_ ty "ifcast."
    let elseEndVars ← snapshotVars
    let elseEndLabel ← getCurrentLabel
    if !term2 then
      terminateBlock (.br mergeLabel)
    -- Merge block: reconcile variables that differ between branches (shared
    -- with the statement-if and match via reconcileBranchVars — ROADMAP 1a).
    -- Diverged branches contribute nothing to the reconciliation.
    startBlock mergeLabel
    if term1 && term2 then
      -- Bug 078, the expression form: both arms diverged, so the merge is
      -- unreachable and loading the result slot there used a slot whose store
      -- never reaches it (E0703). Terminate it, as match lowering does.
      setState { (← getState) with vars := preIfVars }
      terminateBlock .unreachable
      return .unit
    let liveArms :=
      (if term1 then [] else [(thenEndVars, thenEndLabel)]) ++
        (if term2 then [] else [(elseEndVars, elseEndLabel)])
    -- Bug 271: the merge starts from the table of a
      -- LIVE arm. The state here is whatever the last-lowered arm left, and when that arm
      -- diverged (return/break/continue) its writes are on no path to the merge; the reconcile
      -- below rebinds only the bindings the live arms changed, so a binding only the diverged
      -- arm wrote kept the diverged arm's value (silent wrong code, or E0703).
    if term2 then setState { (← getState) with vars := thenEndVars }
    reconcileBranchVars ifExprMergeRules preIfVars liveArms
    -- Load result from slot (a unit/never if-expr has no slot — yields unit).
    loadResult resultSlot? ty "ifload."

/-- Extract a value from the last statement of a body, for phi nodes.
    Uses the __last_expr var that lowerStmt(.expr) sets. -/
partial def lastExprVal (body : List CStmt) (_ty : Ty) : LowerM SVal := do
  match body.getLast? with
  | some (.expr _ true) =>
    match ← lookupVar (.name "__last_expr") with
    | some val => pure val
    | none => pure .unit
  | some (.return_ (some _) _) => pure .unit  -- arm returned, won't reach phi
  | some (.letDecl name _ _ _) =>
    match ← lookupVar name.key with
    | some val => pure val
    | none => pure .unit
  | _ => pure .unit

/-- Store a value-bearing branch's trailing value into the shared result slot,
    coerced to the result type. No-op when the branch diverged (`term` — a
    divergent branch has no value, so a store would be `store void undef`) or
    when there is no slot (a unit/never result). This is the single place an
    if-expr branch or a while-expr `else` feeds its value to the slot; keeping
    it in one spot holds the void-store and wrong-width-store bug classes out of
    every value-bearing construct (part of the ROADMAP #5 control-flow builder). -/
partial def storeBranchResult (term : Bool) (slot? : Option String)
    (body : List CStmt) (ty : Ty) (pfx : String) : LowerM Unit := do
  if term then return
  match slot? with
  | none => pure ()
  | some slot =>
    let v ← lastExprVal body ty
    let v ← coerceVal v ty pfx
    emit (.store v (.reg slot ty))

/-- Emit all deferred calls in LIFO order. -/
private partial def emitFrameDeferredCalls (frame : ScopeFrame) : LowerM Unit := do
  -- Bug 286: a deferred call's source is where it was deferred, not inside the region it is
  -- emitted in (a `return` emits every enclosing scope's), so its sites walk afresh.
  let saved := (← getState).addrBound
  modify fun s => { s with addrBound := none }
  for body in frame.deferred do
    let _ ← lowerExpr body
  modify fun s => { s with addrBound := saved }

/-- Emit all deferred calls for all active scopes, from inner to outer. -/
private partial def emitAllDeferredCalls : LowerM Unit := do
  let s ← get
  for frame in s.scopeStack do
    emitFrameDeferredCalls frame

private partial def pushScope (kind : ScopeKind) : LowerM Unit := do
  modify fun s => { s with scopeStack := { kind := kind } :: s.scopeStack }

private partial def popScope : LowerM (Option ScopeFrame) := do
  let s ← get
  match s.scopeStack with
  | [] => return none
  | frame :: rest =>
    set { s with scopeStack := rest }
    return some frame

private partial def addDeferredToCurrentScope (body : CExpr) : LowerM Unit := do
  modify fun s =>
    match s.scopeStack with
    | [] => s
    | frame :: rest => { s with scopeStack := { frame with deferred := body :: frame.deferred } :: rest }

/-- Emit deferred calls for scopes being exited by break/continue.
    Stops before the nearest loop marker or outer function scope. -/
private partial def emitDeferredUntilLoop (loopsToExit : Nat := 1) : LowerM Unit := do
  -- Bug 149 (Codex review): a labelled `break 'outer` / `continue 'outer` leaves every loop up to
  -- the labelled one, so the deferred calls of every block inside it run, not only those inside
  -- the innermost loop. `loopsToExit` loop markers are crossed before stopping.
  let s ← get
  let mut remaining := loopsToExit
  for frame in s.scopeStack do
    match frame.kind with
    | .function => break
    | .loop =>
      remaining := remaining - 1
      if remaining == 0 then break
    | .block => emitFrameDeferredCalls frame

/-- Address of a place expression, for `&place` / `&mut place`. Returns the
    pointer to the actual storage so the borrow ALIASES it (a write through the
    borrow updates the original). Handles array elements (`&mut a[i]`) and struct
    fields (`&mut o.f`) by GEP-ing into a stable base address, recursing for
    nested places (`&mut a[i].f`, `&mut grid[i][j]`). `.ident`/scalar promotion is
    delegated to `addrOfLocal`. Without this, an element/field borrow fell through
    to `addrOfLocal`'s `none` path and was materialized as a pointer to a throwaway
    COPY — so the callee mutated the copy and the write was silently lost. -/
partial def placeAddr (place : CExpr) (refTy : Ty) : LowerM (Option SVal) := do
  match place with
  | .arrayIndex arr index _ =>
    let elemTy := match refTy with
      | .ref t | .refMut t | .ptrMut t | .ptrConst t => t
      | _ => refTy
    -- A stable base pointer to the array storage: recurse for a nested place,
    -- else `lowerExpr arr` (an array ident / `&[T;N]` already lowers to a pointer).
    -- A pointer-valued base (`&[T;N]`, `*mut [T;N]`, a heap array) IS the
    -- address: its value, never the address of the slot holding it (bug 102).
    let baseAddr ← match arr.ty with
      | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ | .heap _ => lowerExpr arr
      | _ => match ← placeAddr arr (.ptrMut arr.ty) with
             | some p => pure p
             | none => lowerExpr arr
    let iVal ← lowerExpr index
    match arrayLenOfTy arr.ty with        -- H8: bounds-check `&a[i]` / `&mut a[i]`
    | some n => emitBoundsCheck iVal n
    | none => pure ()
    let gepDst ← freshReg
    emit (.gep gepDst baseAddr [iVal] elemTy)
    return some (.reg gepDst refTy)
  | .fieldAccess obj field _ =>
    let tyName ← structNameFromTy obj.ty
    let byteOff ← fieldByteOffset tyName field (typeArgsFromTy obj.ty)
    let baseAddr ← match obj.ty with
      | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ | .heap _ => lowerExpr obj
      | _ => match ← placeAddr obj (.ptrMut obj.ty) with
             | some p => pure p
             | none => lowerExpr obj
    let gepDst ← freshReg
    emit (.gep gepDst baseAddr [.intConst (Int.ofNat byteOff) .int] .i8)
    return some (.reg gepDst refTy)
  -- `*r` as a place is the storage `r` points at: its address is `r` itself. Without
  -- this, `&mut (*p).v` loaded `*p` into a temporary and took the field's address in
  -- the COPY, so a write through the borrow was lost, and a first-field Vec reached
  -- `vec_push` typed as the loaded struct (bug 116, Codex round 2).
  | .deref inner _ =>
    match inner.ty with
    | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ | .heap _ =>
      let rVal ← lowerExpr inner
      let dst ← freshReg
      emit (.cast dst rVal refTy)
      return some (.reg dst refTy)
    | _ => addrOfLocal place refTy
  | _ => addrOfLocal place refTy

/-- Is `ty` a struct or a fixed-size array, held by value (no reference, pointer, heap cell or
    newtype in between)? -/
private partial def isValueAggregate (ty : Ty) : LowerM Bool := do
  match ty with
  | .array _ _ => return true
  | .named n | .generic n _ _ =>
    let ctx ← getLayoutCtx
    return (Layout.lookupStruct ctx n).isSome && (Layout.lookupNewtype ctx n).isNone
  | _ => return false

/-- Bug 250: is `place` a promoted local, or a chain of struct fields of one, each a struct or an
    array held by value? Such a place's storage is inside the local's stable alloca, so a scalar
    in it can be written or read at its address instead of through a copy of the whole aggregate.
    A field holding a reference, pointer or heap cell is not such a place: its storage is the
    pointee, not the slot. -/
private partial def inPlaceRoot (place : CExpr) : LowerM Bool := do
  match place with
  | .ident name ty =>
    if !(← isValueAggregate ty) then return false
    return (← isPromoted name.key).isSome
  | .fieldAccess obj _ ty =>
    if !(← isValueAggregate ty) then return false
    if !(← isValueAggregate obj.ty) then return false
    inPlaceRoot obj
  | _ => return false

/-- An index expression whose evaluation cannot change memory (no call, borrow or assignment
    inside): reading an element at its address after evaluating it reads what a copy taken before
    it would (bug 250). -/
private partial def pureIndexExpr : CExpr → Bool
  | .intLit .. | .boolLit _ | .charLit _ | .ident .. => true
  | .binOp _ a b _ => pureIndexExpr a && pureIndexExpr b
  | .unaryOp _ a _ => pureIndexExpr a
  | .cast a _ => pureIndexExpr a
  | .fieldAccess a _ _ => pureIndexExpr a
  | .arrayIndex a i _ => pureIndexExpr a && pureIndexExpr i
  | _ => false

/-- A scalar store is one `store` of a register-sized value (bug 250's in-place write). -/
private partial def isScalarStoreTy : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32
  | .float32 | .float64 | .bool | .char => true
  | _ => false

/-- Store `newVal` into the location denoted by the place expression `place`.
    This is the unified lvalue path: it recurses into compound places
    (`o.inner.v`, `a[i].x`, `m[i][j]`, `p.inner.v`) by value-writeback —
    `base.field = v` is `base = {base with field = v}` — terminating at a root
    variable (`setVar`/promoted alloca) or a reference/deref base (a real
    pointer store). Before this, only single-level places were handled and any
    deeper write was silently dropped (ROADMAP Phase 4 #44c). -/
partial def storeToPlace (place : CExpr) (newVal : SVal) : LowerM Unit := do
  match place with
  | .ident name _ =>
    match ← isPromoted name.key with
    | some (allocaReg, structTy) => emit (.store newVal (.reg allocaReg structTy))
    | none => setVar name.key newVal
  | .deref inner _ =>
    let p ← lowerExpr inner
    emit (.store newVal p)
  | .fieldAccess obj field _ =>
    let tyName ← structNameFromTy obj.ty
    let byteOff ← fieldByteOffset tyName field (typeArgsFromTy obj.ty)
    match obj.ty with
    | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ =>
      -- base is a pointer: GEP into the pointee and store in place
      let oVal ← lowerExpr obj
      let gepDst ← freshReg
      emit (.gep gepDst oVal [.intConst (Int.ofNat byteOff) .int] .i8)
      emit (.store newVal (.reg gepDst (SVal.ty newVal)))
    | _ =>
      -- Bug 250: a scalar field of a promoted local (or of a field chain of one) is written at
      -- its address. Rebuilding the struct and writing it back copied the whole struct per write.
      let inPlace ← if isScalarStoreTy (SVal.ty newVal) then inPlaceRoot obj else pure false
      if inPlace then
        let baseAddr ← match ← placeAddr obj (.ptrMut obj.ty) with
          | some p => pure p
          | none => lowerExpr obj
        let gepDst ← freshReg
        emit (.gep gepDst baseAddr [.intConst (Int.ofNat byteOff) .int] .i8)
        emit (.store newVal (.reg gepDst (SVal.ty newVal)))
      else
      -- base is a value: build the updated struct value, then write it back
      -- into the base place (recursing to any depth)
      let structTy := obj.ty
      let oVal ← lowerExpr obj
      let tmp ← freshReg
      emitEntryAlloca (.alloca tmp structTy)
      emit (.store oVal (.reg tmp structTy))
      let gepDst ← freshReg
      emit (.gep gepDst (.reg tmp structTy) [.intConst (Int.ofNat byteOff) .int] .i8)
      emit (.store newVal (.reg gepDst (SVal.ty newVal)))
      let newObjVal ← freshReg
      emit (.load newObjVal (.reg tmp structTy) structTy)
      storeToPlace obj (.reg newObjVal structTy)
  | .arrayIndex arr index _ =>
    -- Resolve the element type THROUGH one ref/ptr/heap layer: when the array is
    -- reached via `&mut [T; N]` (etc.), `arr.ty` is `.refMut (.array T N)`, not a
    -- bare `.array`. Without the deref this fell back to the stored value's type
    -- (i64 for an int literal), so `a[i] = v` through a `&mut [i32; N]` emitted an
    -- i64-strided GEP + `store i64` — wrong offset and a clobbering 8-byte store.
    -- This is the write-path analogue of the C10 read-path fix.
    let elemTy := match arr.ty with
      | .array t _ => t
      | .ref (.array t _) | .refMut (.array t _)
      | .ptrMut (.array t _) | .ptrConst (.array t _)
      | .heap (.array t _) => t
      | _ => SVal.ty newVal
    let iVal ← lowerExpr index
    match arrayLenOfTy arr.ty with        -- H8: bounds-check the write
    | some n => emitBoundsCheck iVal n
    | none => pure ()
    let storeVal ← if SVal.ty newVal == elemTy then pure newVal else do
      let castDst ← freshReg
      emit (.cast castDst newVal elemTy)
      pure (SVal.reg castDst elemTy)
    match arr.ty with
    | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ =>
      let aVal ← lowerExpr arr
      let gepDst ← freshReg
      emit (.gep gepDst aVal [iVal] elemTy)
      emit (.store storeVal (.reg gepDst elemTy))
    | _ =>
      -- Bug 250: a scalar element of a promoted array (a local carried through a loop, or a field
      -- chain of one) is written at its address. Rebuilding the array and writing it back copied
      -- the whole array per write (at run time, and an aggregate load and store per write that
      -- made clang -O2 quadratic in the array's length).
      let inPlace ← if isScalarStoreTy elemTy then inPlaceRoot arr else pure false
      if inPlace then
        let baseAddr ← match ← placeAddr arr (.ptrMut arr.ty) with
          | some p => pure p
          | none => lowerExpr arr
        let gepDst ← freshReg
        emit (.gep gepDst baseAddr [iVal] elemTy)
        emit (.store storeVal (.reg gepDst elemTy))
      else
      let arrTy := arr.ty
      let aVal ← lowerExpr arr
      let tmp ← freshReg
      emitEntryAlloca (.alloca tmp arrTy)
      emit (.store aVal (.reg tmp arrTy))
      let gepDst ← freshReg
      emit (.gep gepDst (.reg tmp arrTy) [iVal] elemTy)
      emit (.store storeVal (.reg gepDst elemTy))
      let newArrVal ← freshReg
      emit (.load newArrVal (.reg tmp arrTy) arrTy)
      storeToPlace arr (.reg newArrVal arrTy)
  | _ =>
    -- Fallback: lower the place as a pointer and store through it.
    let p ← lowerExpr place
    emit (.store newVal p)

/-- Lower a match-arm body once its guard branch has landed in the current block.
    Lowers the body, coerces its value to the match result type `ty`, and either
    branches to `mergeLabel` (returning the phi incoming `(value, block)` and the
    arm-end variable snapshot) or, if the body diverged (return/break/continue),
    returns `none` and an empty snapshot. Callers append the results to the
    match's `phiIncoming` / `armEndSnapshots` and clear `allArmsTerminated`. This
    is the single shared tail for every arm shape (literal, variable, range, and
    future guard/OR arms). -/
partial def finishMatchArmBody (guard : Option CExpr) (body : List CStmt) (ty : Ty)
    (mergeLabel : String) (nextCheck : String)
    : LowerM (Option (SVal × String) × (VarMap × String × Bool)) := do
  -- A guard is tested after the pattern bindings are live: if it is false, fall
  -- through to the next arm's check; otherwise run the body.
  match guard with
  | some g =>
    let gVal ← lowerExpr g
    let okLabel ← freshLabel "guard.ok"
    terminateBlock (.condBr gVal okLabel nextCheck)
    startBlock okLabel
  | none => pure ()
  lowerStmts body
  let bodyVal ← lastExprVal body ty
  let bodyVal ← coerceVal bodyVal ty "matchcast."
  let term ← currentBlockTerminated
  if !term then
    let curLabel ← getCurrentLabel
    let armEndVars ← snapshotVars
    terminateBlock (.br mergeLabel)
    return (some (bodyVal, curLabel), (armEndVars, curLabel, false))
  else
    return (none, (VarMap.empty, "", true))

partial def lowerStmt (stmt : CStmt) : LowerM Unit := do
  match stmt with
  | .letDecl name mutable ty value =>
    let val ← lowerExpr value
    -- A declaration BINDS; it never assigns into an earlier binding's storage. Elab renames
    -- every binding that shadows a VISIBLE name (bug 087), so a Core name declared again is
    -- a dead name being reused (a sibling branch, or code after the scope that declared it),
    -- and any promoted storage still registered under it belongs to that dead binding.
    -- `setVar` consults the promoted table first, so an UNPROMOTED re-declaration (immutable,
    -- or a scalar, or another type) was stored into — and read back from — the dead
    -- binding's storage at the dead binding's type (Codex round 2 of bug 087).
    removePromotedAllocas [name.key]
    -- A mutable VALUE aggregate (array, or a user struct/enum) needs a stable
    -- alloca so in-place element/field assignment AND `&mut place` borrows operate
    -- on real storage. This must happen at the declaration, not lazily mid-
    -- function: a borrow taken inside a conditional branch would otherwise promote
    -- the local there, and the post-branch merge would read an alloca only
    -- initialized on one path (a write applied on the wrong path). Heap handles
    -- (Vec/String/HashMap) are excluded — they are pointer-like and are not
    -- promoted today. (Was arrays-only; structs/enums joined once `&mut o.f`
    -- started GEP-ing into storage.)
    let promote ← if !mutable then pure false else
      match ty with
      | .array _ _ => pure true
      | .named n | .generic n _ _ => do
        let ctx ← getLayoutCtx
        pure ((Layout.lookupStruct ctx n).isSome || (Layout.lookupEnum ctx n).isSome)
      | _ => pure false
    if promote then
      let allocaReg ← freshReg s!"{name}.agg."
      -- Hoist the alloca to the ENTRY block so it dominates every later use: a
      -- `let mut` inside a branch would otherwise put the alloca in that branch
      -- block, and a read at the post-branch merge would not be dominated by it
      -- (E0703). Entry-hoisting also avoids per-iteration stack growth in loops.
      -- The initializing store stays here, at the declaration site.
      emitEntryAlloca (.alloca allocaReg ty)
      emit (.store val (.reg allocaReg ty))
      addPromotedAlloca name.key allocaReg ty
    else
      setVar name.key (← snapshotArrayValue value val)

  | .assign name value =>
    let val ← lowerExpr value
    setVar name.key val

  | .return_ (some value) _retTy =>
    -- The result is read BEFORE the deferred calls run, so an array result is copied first
    -- (bug 190, Codex review: a defer writing the array changed the returned value).
    let val ← snapshotArrayValue value (← lowerExpr value)
    emitAllDeferredCalls
    terminateBlock (.ret (some val))

  | .return_ none _retTy =>
    emitAllDeferredCalls
    terminateBlock (.ret none)

  | .expr e iv =>
    let v ← lowerExpr e
    -- Only a trailing value expression (no `;`) is the block's value; a discarded
    -- statement still lowers (side effects) but does not set the block value (#42).
    -- A block's value is taken before the block's deferred calls run (bug 190).
    if iv then setVar (.name "__last_expr") (← snapshotArrayValue e v) else pure ()

  | .ifElse cond then_ else_ =>
    let condVal ← lowerExpr cond
    let thenLabel ← freshLabel "then"
    let elseLabel ← freshLabel "else"
    let mergeLabel ← freshLabel "merge"
    let preIfVars ← snapshotVars
    -- Bug 031: see prePromoteAddrTaken — the store must dominate both branches.
    let branchTaken ← regionAddrTaken preIfVars
      (fun _ => match else_ with
        | some el => cstmtsAddrTaken (cstmtsAddrTaken {} then_) el
        | none => cstmtsAddrTaken {} then_) (prePromoteRelevant)
    prePromoteAddrTaken preIfVars branchTaken
    terminateBlock (.condBr condVal thenLabel elseLabel)
    -- Then block
    startBlock thenLabel
    withAddrBound branchTaken (lowerStmts then_)
    let thenEndVars ← snapshotVars
    let thenEndLabel ← getCurrentLabel
    let term1 ← currentBlockTerminated
    if !term1 then
      terminateBlock (.br mergeLabel)
    -- Else block
    -- Replace entire var map (removes then-branch locals)
    let s ← getState
    setState { s with vars := preIfVars }
    startBlock elseLabel
    match else_ with
    | some stmts => withAddrBound branchTaken (lowerStmts stmts)
    | none => pure ()
    let elseEndVars ← snapshotVars
    let elseEndLabel ← getCurrentLabel
    let term2 ← currentBlockTerminated
    if !term2 then
      terminateBlock (.br mergeLabel)
    -- Merge block: reconcile variables that differ between branches (shared
    -- with the if-expr and match via reconcileBranchVars — ROADMAP 1a).
    -- Diverged branches contribute nothing to the reconciliation.
    startBlock mergeLabel
    if term1 && term2 then
      -- Bug 078: both arms diverged, so nothing reaches the merge. Left open, the
      -- block looked like a fall-through: an enclosing `if` branched from it into
      -- its own merge and used the unreachable block as a phi source, which the
      -- verifier rejects (E0703, a use in a block its definition cannot dominate).
      -- Terminate it (match lowering does the same since bug 175), so the
      -- statements after it are skipped and enclosing constructs see a diverged arm.
      setState { (← getState) with vars := preIfVars }
      terminateBlock .unreachable
    else
      let liveArms :=
        (if term1 then [] else [(thenEndVars, thenEndLabel)]) ++
          (if term2 then [] else [(elseEndVars, elseEndLabel)])
      -- Bug 271: the merge starts from the table of a
        -- LIVE arm. The state here is whatever the last-lowered arm left, and when that arm
        -- diverged (return/break/continue) its writes are on no path to the merge; the reconcile
        -- below rebinds only the bindings the live arms changed, so a binding only the diverged
        -- arm wrote kept the diverged arm's value (silent wrong code, or E0703).
      if term2 then setState { (← getState) with vars := thenEndVars }
      reconcileBranchVars stmtIfMergeRules preIfVars liveArms
      -- Drop branch-local declarations (e.g. a loop counter `let mut k` declared
      -- inside a branch) so they do not leak into a later construct's variable
      -- snapshot. A leaked branch-local would make the next loop/merge build a
      -- spurious header phi whose pre-value is defined inside the branch and does
      -- not dominate the merge (H7: E0708). Keep the merged values for names that
      -- existed before the `if`; restrict the live set to exactly those names —
      -- the same scope cleanup the match lowering does (WC-0004).
      let st ← getState
      setState { st with vars := preIfVars.restrictTo st.vars }

  | .while_ cond body whileLabel step =>
    let headerLabel ← freshLabel "while.hdr"
    let bodyLabel ← freshLabel "while.body"
    let exitLabel ← freshLabel "while.exit"
    -- For for-loops with a step, create a step block that continue targets
    let hasStep := !step.isEmpty
    let stepLabel ← if hasStep then freshLabel "for.step" else pure headerLabel
    -- Snapshot pre-loop state
    let preLoopVars ← snapshotVars
    let preLoopLabel ← getCurrentLabel
    -- Partition live variables: aggregate types get stable allocas,
    -- scalar types get phi nodes as before.
    -- Bug 263: the loop visits every binding in scope (each gets a header phi or a slot), but
    -- with a walk of the condition and body for each (now one walk, `addrTaken`), a list search
    -- per binding (`allPromotedNames.contains`) and a list append per binding; the name sets are
    -- trees and the lists accumulate newest first.
    -- Bug 286: the loop acts on the set for each binding not yet promoted that is not an
    -- aggregate (an aggregate is promoted whatever the set says).
    let addrTaken ← regionAddrTaken preLoopVars (fun _ => cstmtsAddrTaken (cexprAddrTaken {} cond) body)
      (fun name val => do
        return (← isPromoted name).isNone && !(← isAggregateForPromotion val.ty))
    let preLoopList := preLoopVars.toList
    let mut headerPhisRev : List (LocalKey × String × SVal × Ty) := []
    let mut allPromotedNames : LocalKeySet := {}  -- all aggregate names (including outer)
    let mut newlyPromotedRev : List LocalKey := [] -- only promoted by THIS loop, newest first
    -- Promote aggregate variables BEFORE terminating the block (stores go here)
    for (name, val) in preLoopList do
      let ty := val.ty
      -- Skip if already promoted by an outer loop
      let alreadyPromoted ← isPromoted name
      if alreadyPromoted.isSome then
        allPromotedNames := allPromotedNames.insert name ()
      else
        let isAgg ← isAggregateForPromotion ty
        -- Also promote a scalar whose address is taken in the body: it must be
        -- memory-backed (single source of truth) rather than phi-carried, else
        -- the mid-body address-of promotion collides with the loop's phi for it
        -- (lost condition / re-init counter / infinite loop — C9).
        -- The condition counts too: it is lowered in the header on every iteration, so a
        -- lazy promotion there re-stores the pre-loop value each time round (Codex review
        -- of bug 102: `while next(&mut p as *mut *mut u8, ..) {}` never advanced `p`).
        if isAgg || addrTaken.contains name then
          let allocaReg ← freshReg "agg."
          emitEntryAlloca (.alloca allocaReg ty)
          emit (.store val (.reg allocaReg ty))
          addPromotedAlloca name allocaReg ty
          allPromotedNames := allPromotedNames.insert name ()
          newlyPromotedRev := name :: newlyPromotedRev
    terminateBlock (.br headerLabel)
    startBlock headerLabel
    -- Create phi nodes only for scalar variables
    for (name, val) in preLoopList do
      let ty := val.ty
      if !allPromotedNames.contains name then
        let phiReg ← freshReg "phi."
        setVar name (.reg phiReg ty)
        headerPhisRev := (name, phiReg, val, ty) :: headerPhisRev
    let headerPhis := headerPhisRev.reverse
    let newlyPromotedNames := newlyPromotedRev.reverse
    let loopInfo : LoopInfo := {
      headerLabel := headerLabel
      exitLabel := exitLabel
      headerPhis := headerPhis
      continueTarget := stepLabel
      loopLabel := whileLabel
    }
    pushLoop loopInfo
    pushScope .loop
    -- Lower condition (uses phi values)
    let condVal ← withAddrBound addrTaken (lowerExpr cond)
    -- A short-circuit `&&`/`||` in the condition ends in its scand/scor merge
    -- block, so THAT (not the header) is the exit edge's predecessor.
    let condEndLabel ← getCurrentLabel
    terminateBlock (.condBr condVal bodyLabel exitLabel)
    -- Lower body (without step, since step is separate for for-loops)
    startBlock bodyLabel
    if hasStep then
      -- Lower only non-step body
      let bodyWithoutStep := body.take (body.length - step.length)
      withAddrBound addrTaken (lowerStmts bodyWithoutStep)
    else
      withAddrBound addrTaken (lowerStmts body)
    let bodyEndVars ← snapshotVars
    let bodyEndLabel ← getCurrentLabel
    let term ← currentBlockTerminated
    if !term then
      if hasStep then
        terminateBlock (.br stepLabel)
      else
        terminateBlock (.br headerLabel)
    -- If for-loop has step, create the step block with phi nodes
    if hasStep then
      startBlock stepLabel
      -- Build phi nodes at step block for continue edges
      let continueEdges ← peekContinueEdges
      if !continueEdges.isEmpty then
        for (name, phiReg, _, ty) in headerPhis do
          let bodyVal := (bodyEndVars.find? name).getD (.reg phiReg ty)
          let mut incoming : List (SVal × String) := []
          if !term then
            incoming := incoming ++ [(bodyVal, bodyEndLabel)]
          for (contVars, contLabel) in continueEdges do
            let contVal := (contVars.find? name).getD (.reg phiReg ty)
            incoming := incoming ++ [(contVal, contLabel)]
          if incoming.length >= 2 then
            let stepPhiReg ← freshReg "step.phi."
            emit (.phi stepPhiReg incoming ty)
            setVar name (.reg stepPhiReg ty)
          else if incoming.length == 1 then
            match incoming.head? with
            | some (val, _) => setVar name val
            | none => pure ()
      withAddrBound addrTaken (lowerStmts step)
      let stepEndVars ← snapshotVars
      let stepEndLabel ← getCurrentLabel
      let stepTerm ← currentBlockTerminated
      if !stepTerm then
        terminateBlock (.br headerLabel)
      -- Pop loop to get break/continue edges
      let loopInfoFinal ← popLoop
      let _ ← popScope
      -- Build header phi instructions (step block is the back-edge source)
      let mut headerPhiInstsRev : List SInst := []
      for (name, phiReg, preVal, ty) in headerPhis do
        let mut incoming : List (SVal × String) := [(preVal, preLoopLabel)]
        if !stepTerm then
          let stepVal := (stepEndVars.find? name).getD (.reg phiReg ty)
          incoming := incoming ++ [(stepVal, stepEndLabel)]
        headerPhiInstsRev := .phi phiReg incoming ty :: headerPhiInstsRev
      prependInstsToBlock headerLabel headerPhiInstsRev.reverse
      -- Build exit block
      startBlock exitLabel
      let breakEdges := match loopInfoFinal with
        | some info => info.breakEdges
        | none => []
      -- Replace entire var map (removes body-local vars)
      let exitVars ← freshVarMap (headerPhis.map fun (name, phiReg, _, ty) => (name, SVal.reg phiReg ty))
      setState { (← getState) with vars := exitVars }
      if !breakEdges.isEmpty then
        for (name, phiReg, _, ty) in headerPhis do
          let headerVal := SVal.reg phiReg ty
          let mut exitIncoming : List (SVal × String) := [(headerVal, condEndLabel)]
          for (breakVars, breakLabel) in breakEdges do
            let breakVal := (breakVars.find? name).getD headerVal
            exitIncoming := exitIncoming ++ [(breakVal, breakLabel)]
          let allSame := exitIncoming.all fun (v, _) => match v, headerVal with
            | .reg n1 _, .reg n2 _ => n1 == n2
            | _, _ => false
          if !allSame then
            let exitPhiReg ← freshReg "exit."
            emit (.phi exitPhiReg exitIncoming ty)
            setVar name (.reg exitPhiReg ty)
      -- Un-promote aggregate variables at for-loop exit (only this loop's)
      for name in newlyPromotedNames do
        match ← isPromoted name with
        | some (allocaReg, ty) =>
          match ty with
          | .array _ _ =>
            -- Bug 029 (site 2): arrays keep ADDRESS identity — rebind the
            -- alloca, never a by-value load (see the if-merge fix).
            removePromotedAllocas [name]
            setVar name (.reg allocaReg ty)
          | _ =>
            let loadDst ← freshReg "unpro."
            emit (.load loadDst (.reg allocaReg ty) ty)
            removePromotedAllocas [name]
            setVar name (.reg loadDst ty)
        | none => pure ()
    else do
    -- Pop loop to get break/continue edges
    let loopInfoFinal ← popLoop
    let _ ← popScope
    -- Build header phi instructions (only for non-promoted scalar vars)
    let mut headerPhiInstsRev : List SInst := []
    for (name, phiReg, preVal, ty) in headerPhis do
      let mut incoming : List (SVal × String) := [(preVal, preLoopLabel)]
      -- Back-edge from body end (only if body didn't fully terminate)
      if !term then
        let backVal := (bodyEndVars.find? name).getD (.reg phiReg ty)
        incoming := incoming ++ [(backVal, bodyEndLabel)]
      -- Continue edges
      match loopInfoFinal with
      | some info =>
        for (contVars, contLabel) in info.continueEdges do
          let contVal := (contVars.find? name).getD (.reg phiReg ty)
          incoming := incoming ++ [(contVal, contLabel)]
      | none => pure ()
      headerPhiInstsRev := .phi phiReg incoming ty :: headerPhiInstsRev
    prependInstsToBlock headerLabel headerPhiInstsRev.reverse
    -- Build exit phis if there are break edges
    startBlock exitLabel
    let breakEdges := match loopInfoFinal with
      | some info => info.breakEdges
      | none => []
    -- Replace entire var map (removes body-local vars)
    let exitVars ← freshVarMap (headerPhis.map fun (name, phiReg, _, ty) => (name, SVal.reg phiReg ty))
    setState { (← getState) with vars := exitVars }
    -- If break edges exist, insert exit phis
    if !breakEdges.isEmpty then
      for (name, phiReg, _, ty) in headerPhis do
        let headerVal := SVal.reg phiReg ty
        let mut exitIncoming : List (SVal × String) := [(headerVal, condEndLabel)]
        for (breakVars, breakLabel) in breakEdges do
          let breakVal := (breakVars.find? name).getD headerVal
          exitIncoming := exitIncoming ++ [(breakVal, breakLabel)]
        -- Only emit phi if values actually differ
        let allSame := exitIncoming.all fun (v, _) => match v, headerVal with
          | .reg n1 _, .reg n2 _ => n1 == n2
          | _, _ => false
        if !allSame then
          let exitPhiReg ← freshReg "exit."
          emit (.phi exitPhiReg exitIncoming ty)
          setVar name (.reg exitPhiReg ty)
    -- Un-promote aggregate variables: load final values from allocas,
    -- restore them as SSA values, and remove from promoted set (only this loop's).
    for name in newlyPromotedNames do
      match ← isPromoted name with
      | some (allocaReg, ty) =>
        match ty with
        | .array _ _ =>
          -- Bug 029 (site 2): arrays keep ADDRESS identity — rebind the alloca.
          removePromotedAllocas [name]
          setVar name (.reg allocaReg ty)
        | _ =>
          let loadDst ← freshReg "unpro."
          emit (.load loadDst (.reg allocaReg ty) ty)
          -- Temporarily remove from promoted so setVar writes to var map
          removePromotedAllocas [name]
          setVar name (.reg loadDst ty)
      | none => pure ()

  | .fieldAssign obj field value =>
    -- `obj.field = value` — assign through the unified place path so compound
    -- bases (`o.inner.v`, `a[i].x`, `p.inner.v`) write in place instead of
    -- mutating a discarded copy (ROADMAP Phase 4 #44c).
    let fVal ← lowerExpr value
    storeToPlace (.fieldAccess obj field value.ty) fVal

  | .derefAssign target value =>
    -- Evaluation-order rule 7 (STATEMENT_EXPRESSION_MODEL): the right-hand side first, then
    -- the destination's subexpressions, as every other assignment form here and the
    -- interpreter do. Lowering `target` first ran its effects first (bug 183).
    let vVal ← lowerExpr value
    let tVal ← lowerExpr target
    emit (.store vVal tVal)

  | .arrayIndexAssign arr index value =>
    -- `arr[index] = value` — via the unified place path (handles nested
    -- bases like `m[i][j]` and `b.data[i]`).
    let vVal ← lowerExpr value
    storeToPlace (.arrayIndex arr index value.ty) vVal

  | .break_ value breakLabel =>
    match ← findLoopByLabel breakLabel with
    | some info =>
      -- Store break value into result slot (for while-as-expression), coerced to
      -- the loop's result type (see coerceVal — a bare `break 7` lowers as i64 and
      -- must not be stored into a narrower i32 result slot).
      match value, info.resultSlot with
      | some valExpr, some slot =>
        let bVal ← lowerExpr valExpr
        let bVal ← coerceVal bVal info.resultTy "bcast."
        emit (.store bVal (.reg slot info.resultTy))
      -- Bug 149 (Codex review): a statement loop has no result slot, but the checker credits the
      -- break operand's moves, so it is still evaluated (`break free(h);`).
      | some valExpr, none => let _ ← lowerExpr valExpr
      | _, _ => pure ()
      emitDeferredUntilLoop (← loopsExitedBy breakLabel)
      let vars ← snapshotVars
      let label ← getCurrentLabel
      addBreakEdgeToLoop vars label info.exitLabel
      terminateBlock (.br info.exitLabel)
    | none => pure ()

  | .continue_ contLabel =>
    match ← findLoopByLabel contLabel with
    | some info =>
      let target := if info.continueTarget != "" then info.continueTarget else info.headerLabel
      emitDeferredUntilLoop (← loopsExitedBy contLabel)
      let vars ← snapshotVars
      let label ← getCurrentLabel
      addContinueEdgeToLoop vars label info.headerLabel
      terminateBlock (.br target)
    | none => pure ()

  | .defer body =>
    -- Bug 287: the deferred call is emitted at every exit of its scope, and the first emission
    -- lowered (a `return` inside a branch, say) used to promote a local it borrows lazily there:
    -- its initializing store sat on that exit's path only, and every other exit read the slot.
    -- The registration point dominates every exit that emits the call, so the locals the body
    -- takes the address of are promoted here (a write after it goes through the slot, and the
    -- call reads the place when it runs: OWN-12).
    prePromoteAddrTaken (← snapshotVars) (cexprAddrTaken {} body)
    addDeferredToCurrentScope body

  | .borrowIn var ref _region isMut refTy body =>
    refuseBorrowInConstInit
    -- Create a memory slot for the borrowed variable, set ref to point to it.
    -- The ref variable is stored with the full reference type (refMut T / ref T)
    -- so that when passed as a function argument, the correct pointer type is emitted.
    -- The alloca is hoisted to the entry block so it dominates all uses, including
    -- across early returns inside the borrow block body.
    let curVal ← lookupVar var.key
    let innerTy := match refTy with
      | .ref t | .refMut t | .ptrMut t | .ptrConst t => t
      | _ => refTy
    let slot ← freshReg "borrow."
    emitEntryAlloca (.alloca slot innerTy)
    match curVal with
    | some cv => emit (.store cv (.reg slot innerTy))
    | none => pure ()
    setVar ref.key (.reg slot refTy)
    lowerStmts body
    -- For mutable borrows, load back the value and update the original variable.
    -- Skip write-back if the body terminated early (return/break) — the write-back
    -- would be dead code and reference registers from non-dominating blocks.
    if isMut then
      let s ← getState
      if !s.blockTerminated then
        let loadBack ← freshReg "wb."
        emit (.load loadBack (.reg slot innerTy) innerTy)
        setVar var.key (.reg loadBack innerTy)

partial def lowerStmts (stmts : List CStmt) : LowerM Unit := do
  pushScope .block
  for s in stmts do
    let term ← currentBlockTerminated
    if term then
      break
    lowerStmt s
  let term ← currentBlockTerminated
  match ← popScope with
  | some frame =>
    if !term then
      emitFrameDeferredCalls frame
  | none => pure ()

end

-- ============================================================
-- Function and module lowering
-- ============================================================

def lowerFn (f : CFnDef) (structDefs : List CStructDef) (enumDefs : List CEnumDef)
    (constants : List (String × Ty × CExpr) := [])
    (newtypes : List NewtypeDef := [])
    (definedFns : Std.HashSet String := {})
    (importedConstants : List (String × CExpr × List (String × Ty × CExpr)) := [])
    (layoutIndex : Option Layout.Index := none)
    : Except Diagnostics (SFnDef × List (String × String)) :=
  let initState : LowerState := {
    blocksRev := []
    currentLabel := "entry"
    currentInsts := []
    labelCounter := 0
    regCounter := 0
    vars := VarMap.empty
    stringLits := []
    structDefs := structDefs
    enumDefs := enumDefs
    newtypes := newtypes
    loopStack := []
    constants := constants
    importedConstants := importedConstants
    definedFns := definedFns
    scopeStack := [{ kind := .function }]
    layoutIndex := layoutIndex
  }
  let result := (do
    let params ← freshVarMap ((f.paramKeys.zip f.params).map fun (k, (n, ty)) => (k, SVal.reg n ty))
    setState { (← getState) with vars := params }
    lowerStmts f.body
    -- If the function hasn't terminated, add implicit return
    let term ← currentBlockTerminated
    if !term then
      emitAllDeferredCalls
      if f.retTy == Ty.unit then
        terminateBlock (.ret none)
      else
        terminateBlock (.ret (some SVal.unit))
  ).run initState |>.run
  match result with
  | ((.ok ()), finalState) =>
    -- Hoist entry allocas: prepend them to the first (entry) block
    let blocks' := match applyBlockPatches finalState.blockPatches finalState.blocksRev.reverse with
      | [] => []
      | entry :: rest =>
        { entry with insts := finalState.entryAllocasRev.reverse ++ entry.insts } :: rest
    .ok ({
      name := f.name
      params := f.params
      retTy := f.retTy
      blocks := blocks'
      isTest := f.isTest
      isEntryPoint := f.isEntryPoint
      declSpan := f.declSpan
      exportName := f.exportName
      foreignCalls := f.foreignCalls
      isMethod := f.isMethod
    }, finalState.stringLits)
  | ((.error e), _) => .error e

private partial def collectAllFunctions (m : CModule) : List CFnDef :=
  let own := m.functions
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllFunctions s) []
  own ++ sub

/-- Collect all functions with their dot-separated module path prefix. -/
private partial def collectAllFunctionsWithPath (m : CModule) (parentPath : String := "") : List (CFnDef × String) :=
  let path := if parentPath.isEmpty then m.name else parentPath ++ "." ++ m.name
  let own := m.functions.map fun f => (f, path)
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllFunctionsWithPath s path) []
  own ++ sub

private partial def collectAllStructs (m : CModule) : List CStructDef :=
  let own := m.structs
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllStructs s) []
  own ++ sub

private partial def collectAllEnums (m : CModule) : List CEnumDef :=
  let own := m.enums
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllEnums s) []
  own ++ sub

/-- The constants of `m`'s tree (`CModule.treeConstants`, shared with the interpreter). -/
private def collectAllConstants (m : CModule) : List (String × Ty × CExpr) := m.treeConstants

/-- R-0492 step 4 (bug 233): the constants of the tree `m`, whose path is `path`, by their imported
    Core spelling (`ConstId.coreNameAt`), each with `scope`, the constants of its top-level tree. -/
private partial def importedConstantsOf (scope : List (String × Ty × CExpr)) (path : String)
    (m : CModule) : List (String × CExpr × List (String × Ty × CExpr)) :=
  m.constants.map (fun (n, _, e) => (ConstId.coreNameAt path n, e, scope))
    ++ m.submodules.flatMap fun sub => importedConstantsOf scope (path ++ "." ++ sub.name) sub

private partial def collectAllExterns (m : CModule) : List (String × List (String × Ty) × Ty) :=
  let own := m.externFns.map fun (n, ps, rt, _) => (n, ps, rt)
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllExterns s) []
  own ++ sub

private partial def collectAllExternLinkNames (m : CModule) : List (String × String) :=
  m.externLinkNames ++ m.submodules.foldl (fun acc s => acc ++ collectAllExternLinkNames s) []

private partial def collectAllLinkerAliases (m : CModule) : List (String × String) :=
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllLinkerAliases s) []
  m.linkerAliases ++ sub

private partial def collectAllNewtypes (m : CModule) : List NewtypeDef :=
  let own := m.newtypes
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllNewtypes s) []
  own ++ sub

private def renameSVal (rmap : List (String × String)) : SVal → SVal
  | .strConst name => match rmap.lookup name with
    | some newName => .strConst newName
    | none => .strConst name
  | .strConstRef name => match rmap.lookup name with
    | some newName => .strConstRef newName
    | none => .strConstRef name
  | other => other

private def renameStrConstsInInst (rmap : List (String × String)) : SInst → SInst
  | .binOp dst op lhs rhs ty => .binOp dst op (renameSVal rmap lhs) (renameSVal rmap rhs) ty
  | .unaryOp dst op operand ty => .unaryOp dst op (renameSVal rmap operand) ty
  | .call dst fn args retTy => .call dst fn (args.map (renameSVal rmap)) retTy
  | .alloca dst ty => .alloca dst ty
  | .load dst ptr ty => .load dst (renameSVal rmap ptr) ty
  | .store val ptr => .store (renameSVal rmap val) (renameSVal rmap ptr)
  | .gep dst base indices ty => .gep dst (renameSVal rmap base) (indices.map (renameSVal rmap)) ty
  | .phi dst incoming ty => .phi dst (incoming.map fun (v, lbl) => (renameSVal rmap v, lbl)) ty
  | .cast dst val tgt => .cast dst (renameSVal rmap val) tgt
  | .memcpy dst src size => .memcpy (renameSVal rmap dst) (renameSVal rmap src) size

private def renameStrConstsInTerm (rmap : List (String × String)) : STerm → STerm
  | .ret (some v) => .ret (some (renameSVal rmap v))
  | .condBr cond tl el => .condBr (renameSVal rmap cond) tl el
  | other => other

def lowerModule (m : CModule) (program : List CModule := []) : Except Diagnostics SModule := do
  let allFunctionsWithPath := collectAllFunctionsWithPath m
  -- Add synthetic String struct so fieldOffset can compute offsets for built-in .string type
  let syntheticStringDef : CStructDef := { name := "String", fields := [("ptr", .ptrMut .u8), ("len", .uint), ("cap", .uint)] }
  -- Bug 035: layout is a PROGRAM-wide fact. A user module constructing an
  -- imported enum whose payload mentions another module's generic struct
  -- (CliResult's Vec<String>) panicked in Layout.fieldOffset because the
  -- template lived in a sibling module tree. Own-module defs keep lookup
  -- priority (name-collision safety: nothing that resolved before resolves
  -- differently now); program-wide defs only FILL names absent locally.
  let ownStructs := collectAllStructs m
  let ownEnums := collectAllEnums m
  let ownNewtypes := collectAllNewtypes m
  -- Bug 279: the own names as sets and the external declarations deduplicated by name in one
  -- pass (the first of each name, in order), where an `any` over the own declarations per
  -- external one and over the kept ones per candidate were n².
  let ownStructNames := Std.HashSet.ofList (ownStructs.map (·.name))
  let ownEnumNames := Std.HashSet.ofList (ownEnums.map (·.name))
  let ownNewtypeNames := Std.HashSet.ofList (ownNewtypes.map (·.name))
  let firstByName := fun {α : Type} (name : α → String) (xs : List α) =>
    (xs.foldl (fun (seen, acc) x =>
        if seen.contains (name x) then (seen, acc) else (seen.insert (name x), x :: acc))
      ((∅ : Std.HashSet String), ([] : List α))).2.reverse
  let extStructs := firstByName (·.name) ((program.flatMap collectAllStructs).filter (fun sd =>
    !(ownStructNames.contains sd.name) && sd.name != "String"))
  let extEnums := firstByName (·.name) ((program.flatMap collectAllEnums).filter (fun ed =>
    !(ownEnumNames.contains ed.name)))
  let extNewtypes := firstByName (·.name) ((program.flatMap collectAllNewtypes).filter (fun nt =>
    !(ownNewtypeNames.contains nt.name)))
  let allStructs := syntheticStringDef :: (ownStructs ++ extStructs)
  let allEnums := ownEnums ++ extEnums
  let allExterns := collectAllExterns m
  let allConstants := collectAllConstants m
  let importedConstants := (if program.isEmpty then [m] else program).flatMap fun pm =>
    importedConstantsOf (collectAllConstants pm) pm.name pm
  let allNewtypes := ownNewtypes ++ extNewtypes
  -- Skip generic functions (non-empty typeParams); only their monomorphized
  -- specializations should be lowered.
  let concreteFns := allFunctionsWithPath.filter fun (f, _) => f.typeParams.isEmpty
  let definedFnNames := Std.HashSet.ofList (allFunctionsWithPath.map fun (f, _) => f.name)
  -- Bug 251: a specialization of another tree's generic is lowered in that tree's context
  -- (`sourceTree`): its constants, and its defined function names (which decide whether a call
  -- spelling is an intrinsic). Bug 279: each such tree's context is built once, for the trees
  -- some function names, where it was rebuilt for every function of that tree.
  let foreignTrees := (concreteFns.filterMap fun (f, _) => f.sourceTree.filter (· != m.name)).foldl
    (fun (acc : Std.HashMap String (List (String × Ty × CExpr) × Std.HashSet String)) t =>
      if acc.contains t then acc else
      match program.find? (fun (pm : CModule) => pm.name == t) with
      | some pm => acc.insert t (collectAllConstants pm,
          Std.HashSet.ofList ((collectAllFunctionsWithPath pm).map fun (g, _) => g.name))
      | none => acc) {}
  let contextOf : CFnDef → List (String × Ty × CExpr) × Std.HashSet String := fun f =>
    match f.sourceTree with
    | some t => if t == m.name then (allConstants, definedFnNames) else
      (foreignTrees.get? t).getD (allConstants, definedFnNames)
    | none => (allConstants, definedFnNames)
  -- Bug 291: the module's declarations by name, built once for every function's lookups.
  let layoutIndex := Layout.Index.ofLists allStructs allEnums allNewtypes
  -- Newest first and reversed once (bug 279: an append per function was n²).
  let resultsRev ← concreteFns.foldlM (init := []) fun acc (f, path) =>
    let (fnConstants, fnDefinedNames) := contextOf f
    match lowerFn f allStructs allEnums fnConstants allNewtypes fnDefinedNames importedConstants
        (some layoutIndex) with
    | .ok (sfn, lits) =>
      .ok (({ sfn with modulePath := path, sourceTree := f.sourceTree.filter (· != m.name) }, lits) :: acc)
    | .error ds => .error (ds.map (·.addContext s!"while lowering function '{f.name}'"))
  let results := resultsRev.reverse
  -- Build deduplicated globals list (by string value)
  -- Prefix with module name to avoid collisions across modules
  -- Bug 279: a map from each string to its global name, the globals newest first and reversed
  -- once, where an `any` over the globals per literal, an append and a `length` were n².
  let (globalOf, globalsRev, _) := results.foldl (fun st (_, lits) =>
    lits.foldl (fun (st : Std.HashMap String String × List (String × String) × Nat) (_, strVal) =>
      let (globalOf, globalsRev, count) := st
      if globalOf.contains strVal then st
      else
        let canon := s!"{m.name}.str.{count}"
        (globalOf.insert strVal canon, (canon, strVal) :: globalsRev, count + 1)
    ) st
  ) (({} : Std.HashMap String String), ([] : List (String × String)), 0)
  let globals := globalsRev.reverse
  -- Rename strConst references per-function using its own string literals
  let fns := results.map fun (fn, lits) =>
    -- Build per-function rename map: old local name → global canonical name
    let renameMap := lits.filterMap fun (oldName, strVal) =>
      match (globalOf.get? strVal).map (·, strVal) with
      | some (canonName, _) => if oldName == canonName then none else some (oldName, canonName)
      | none => none
    if renameMap.isEmpty then fn else { fn with
      blocks := fn.blocks.map fun blk => { blk with
        insts := blk.insts.map fun inst => renameStrConstsInInst renameMap inst
        term := renameStrConstsInTerm renameMap blk.term
      }
    }
  let result : SModule := {
    name := m.name
    structs := allStructs
    enums := allEnums
    functions := fns
    externFns := allExterns
    globals := globals
    linkerAliases := collectAllLinkerAliases m
    newtypes := allNewtypes
    externLinkNames := collectAllExternLinkNames m
  }
  return result

end Concrete
