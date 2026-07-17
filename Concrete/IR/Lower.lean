import Concrete.Elab.Core
import Concrete.IR.SSA
import Concrete.Check.Layout
import Concrete.Resolve.Intrinsic
import Concrete.Report.Diagnostic

namespace Concrete

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
-- Lowering state
-- ============================================================

structure LoopInfo where
  headerLabel : String
  exitLabel : String
  /-- Phi registers created at header: (varName, phiReg, preLoopVal, ty) -/
  headerPhis : List (String × String × SVal × Ty) := []
  /-- Var snapshots at each break: (varsSnapshot, sourceLabel) -/
  breakEdges : List (List (String × SVal) × String) := []
  /-- Alloca slot for while-as-expression result (break stores value here). -/
  resultSlot : Option String := none
  /-- Type for the result slot. -/
  resultTy : Ty := .unit
  /-- Var snapshots at each continue: (varsSnapshot, sourceLabel) -/
  continueEdges : List (List (String × SVal) × String) := []
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
  blocks : List SBlock
  currentLabel : String
  currentInsts : List SInst
  labelCounter : Nat
  regCounter : Nat
  vars : List (String × SVal)
  stringLits : List (String × String)
  structDefs : List CStructDef
  enumDefs : List CEnumDef
  newtypes : List NewtypeDef := []
  loopStack : List LoopInfo
  constants : List (String × Ty × CExpr) := []
  /-- Names of user/Core-defined functions in the program. A call to a
      DEFINED function is never an intrinsic — intrinsic identity is
      by-resolution, not by raw name (audit 2026-07-16: user fns named
      sizeof/alloc/... were hijacked at this pass too). -/
  definedFns : List String := []
  /-- Allocas that must be hoisted to the function entry block.
      Prevents unbounded stack growth when &mut borrows occur in loops. -/
  entryAllocas : List SInst := []
  /-- Aggregate variables promoted to stable allocas during loops.
      Maps (varName, allocaReg, ty). Field assignment GEPs directly into
      the alloca instead of phi-transporting whole struct values. -/
  promotedAllocas : List (String × String × Ty) := []
  scopeStack : List ScopeFrame := []
  /-- True after terminateBlock, cleared by startBlock. Used to detect
      dead code after early returns inside borrow blocks. -/
  blockTerminated : Bool := false

abbrev LowerM := ExceptT Diagnostics (StateM LowerState)

private def throwLower (msg : String) : LowerM α :=
  throw [{ severity := .error, message := msg, pass := "lower", span := none, hint := none, code := "E0602" }]

private def getState : LowerM LowerState := get
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
  emit (.call none "__cc_bounds_check" [idxI64, .intConst (Int.ofNat len) .int] .unit)

/-- Emit an alloca that will be hoisted to the function entry block.
    This prevents dynamic stack growth when allocas occur inside loops. -/
private def emitEntryAlloca (inst : SInst) : LowerM Unit := do
  let s ← getState
  setState { s with entryAllocas := s.entryAllocas ++ [inst] }

-- Insert a store instruction into an already-emitted block (before its terminator).
-- Used by if/else and match to store aggregate values into merge allocas.
private def insertStoreBeforeTerm (blockLabel : String) (val : SVal) (dst : SVal) : LowerM Unit := do
  let s ← getState
  let storeInst := SInst.store val dst
  let blocks := s.blocks.map fun b =>
    if b.label == blockLabel then { b with insts := b.insts ++ [storeInst] }
    else b
  setState { s with blocks := blocks }

private def terminateBlock (term : STerm) : LowerM Unit := do
  let s ← getState
  -- `currentInsts` is accumulated reversed by `emit`; restore program order here.
  let block : SBlock := { label := s.currentLabel, insts := s.currentInsts.reverse, term := term }
  setState { s with blocks := s.blocks ++ [block], currentInsts := [], blockTerminated := true }

private def startBlock (label : String) : LowerM Unit := do
  let s ← getState
  setState { s with currentLabel := label, blockTerminated := false }

private def setVar (name : String) (val : SVal) : LowerM Unit := do
  let s ← getState
  -- If promoted to stable alloca, store there instead of updating var map
  match s.promotedAllocas.find? fun (n, _, _) => n == name with
  | some (_, allocaReg, ty) =>
    emit (.store val (.reg allocaReg ty))
  | none =>
    let vars' := if s.vars.any fun (n, _) => n == name then
      s.vars.map fun (n, v) => if n == name then (n, val) else (n, v)
    else
      s.vars ++ [(name, val)]
    setState { s with vars := vars' }

private def lookupVar (name : String) : LowerM (Option SVal) := do
  let s ← getState
  -- Check if this variable is promoted to a stable alloca (aggregate in loop)
  match s.promotedAllocas.find? fun (n, _, _) => n == name with
  | some (_, allocaReg, ty) =>
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
    return s.vars.lookup name

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
  match s.blocks.getLast? with
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

/-- Look up a struct definition's fields by type name. -/
private def lookupStructFields (tyName : String) : LowerM (List (String × Ty)) := do
  let s ← getState
  match s.structDefs.find? fun sd => sd.name == tyName with
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
  let s ← getState
  match s.enumDefs.find? fun ed => ed.name == enumName with
  | some ed =>
    match (enumerate ed.variants).find? fun (_, (vn, _)) => vn == variantName with
    | some (idx, _) => return idx
    | none =>
      throwLower s!"Lower.variantIndex: variant '{variantName}' not found in enum '{enumName}'"
  | none =>
    throwLower s!"Lower.variantIndex: enum '{enumName}' not found in enum defs"

/-- Get variant fields within an enum definition. -/
private def variantFields (enumName : String) (variantName : String) (typeArgs : List Ty := []) : LowerM (List (String × Ty)) := do
  let s ← getState
  match s.enumDefs.find? fun ed => ed.name == enumName with
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
  | .generic n _ => return n
  | .string => return "String"
  | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner => structNameFromTy inner
  | other =>
    throwLower s!"Lower.structNameFromTy: unhandled type '{repr other}'"

/-- Build a Layout.Ctx from the current LowerState. -/
private def getLayoutCtx : LowerM Layout.Ctx := do
  let s ← getState
  return { structDefs := s.structDefs, enumDefs := s.enumDefs, newtypes := s.newtypes }

/-- Compute byte size of a type (for malloc). Delegates to Layout.tySize. -/
private def computeTySize (ty : Ty) : LowerM Nat := do
  let ctx ← getLayoutCtx
  return Layout.tySize ctx ty

/-- Is this type an aggregate that should use stable alloca storage
    instead of phi-transporting through loop headers?
    Only true value-typed aggregates (structs, enums, strings, vecs, arrays)
    should be promoted. References and pointers are 8-byte scalars — use phi. -/
private def isAggregateForPromotion (ty : Ty) : LowerM Bool := do
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
  | .generic "Vec" _ | .generic "HashMap" _ => return true
  | .generic "Heap" _ | .generic "HeapArray" _ => return false
  | .named name =>
    let ctx ← getLayoutCtx
    return (Layout.lookupStruct ctx name).isSome || (Layout.lookupEnum ctx name).isSome
  | .generic name _ =>
    let ctx ← getLayoutCtx
    return (Layout.lookupStruct ctx name).isSome || (Layout.lookupEnum ctx name).isSome

/-- Check if a variable is currently promoted to a stable alloca. -/
private def isPromoted (name : String) : LowerM (Option (String × Ty)) := do
  let s ← getState
  match s.promotedAllocas.find? fun (n, _, _) => n == name with
  | some (_, reg, ty) => return some (reg, ty)
  | none => return none

/-- Add a promoted alloca for a variable. -/
private def addPromotedAlloca (name : String) (allocaReg : String) (ty : Ty) : LowerM Unit := do
  let s ← getState
  setState { s with promotedAllocas := s.promotedAllocas ++ [(name, allocaReg, ty)] }

/-- Remove promoted allocas by name (at loop exit). -/
private def removePromotedAllocas (names : List String) : LowerM Unit := do
  let s ← getState
  setState { s with promotedAllocas := s.promotedAllocas.filter fun (n, _, _) => !names.contains n }

/-- Extract type args from a Ty, unwrapping references/pointers. -/
private def typeArgsFromTy : Ty → List Ty
  | .generic _ args => args
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

/-- Record a break edge on a specific loop (identified by exitLabel). -/
private def addBreakEdgeToLoop (vars : List (String × SVal)) (srcLabel : String) (targetExitLabel : String) : LowerM Unit := do
  let s ← getState
  let newStack := s.loopStack.map fun info =>
    if info.exitLabel == targetExitLabel then
      { info with breakEdges := info.breakEdges ++ [(vars, srcLabel)] }
    else info
  setState { s with loopStack := newStack }

/-- Record a break edge on the innermost loop. -/
private def addBreakEdge (vars : List (String × SVal)) (label : String) : LowerM Unit := do
  let s ← getState
  match s.loopStack with
  | info :: rest =>
    let info' := { info with breakEdges := info.breakEdges ++ [(vars, label)] }
    setState { s with loopStack := info' :: rest }
  | [] => pure ()

/-- Record a continue edge on a specific loop (identified by headerLabel). -/
private def addContinueEdgeToLoop (vars : List (String × SVal)) (srcLabel : String) (targetHeaderLabel : String) : LowerM Unit := do
  let s ← getState
  let newStack := s.loopStack.map fun info =>
    if info.headerLabel == targetHeaderLabel then
      { info with continueEdges := info.continueEdges ++ [(vars, srcLabel)] }
    else info
  setState { s with loopStack := newStack }

/-- Record a continue edge on the innermost loop. -/
private def addContinueEdge (vars : List (String × SVal)) (label : String) : LowerM Unit := do
  let s ← getState
  match s.loopStack with
  | info :: rest =>
    let info' := { info with continueEdges := info.continueEdges ++ [(vars, label)] }
    setState { s with loopStack := info' :: rest }
  | [] => pure ()

/-- Peek at the continue edges of the innermost loop without popping. -/
private def peekContinueEdges : LowerM (List (List (String × SVal) × String)) := do
  let s ← getState
  match s.loopStack with
  | info :: _ => return info.continueEdges
  | [] => return []

/-- Prepend instructions to an already-finalized block. -/
private def prependInstsToBlock (label : String) (newInsts : List SInst) : LowerM Unit := do
  let s ← getState
  let blocks' := s.blocks.map fun b =>
    if b.label == label then { b with insts := newInsts ++ b.insts }
    else b
  setState { s with blocks := blocks' }

/-- Get current var map snapshot. -/
private def snapshotVars : LowerM (List (String × SVal)) := do
  let s ← getState
  -- `__last_expr` is the internal trailing-value pseudo-variable (set by `.expr`
  -- lowering); it is not a real local. It must never participate in merge/loop phi
  -- reconciliation — a leaked `__last_expr` from a previous value block would build
  -- a spurious, mistyped, or non-dominated phi (E0708/E0710/E0715). Snapshots feed
  -- exactly that reconciliation, so drop it here at the single source. The trailing
  -- value is read separately via `lookupVar "__last_expr"` (lastExprVal).
  return s.vars.filter fun (n, _) => n != "__last_expr"

-- ============================================================
-- Expression and statement lowering
-- ============================================================

/-- Address-of a LOCAL variable. Returns a pointer (retyped to `refTy`) to the
    local's stable storage, promoting the local to a stack alloca on first
    address-take so that the pointer actually aliases the variable. Reads and
    writes of the local then route through the alloca via `lookupVar`/`setVar`.
    Returns `none` for non-local or already-addressable inners (arrays, refs,
    pointers, heap), which the caller handles by its existing logic.
    (ROADMAP Phase 4 #44d — fixes raw-pointer / `&mut`-to-local not aliasing.) -/
private def addrOfLocal (inner : CExpr) (refTy : Ty) : LowerM (Option SVal) := do
  match inner with
  | .ident name varTy =>
    match varTy with
    -- arrays are already stack-allocated; refs/ptrs/heap are already pointers
    | .array _ _ | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ | .heap _ => return none
    | _ =>
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
  | _ => return none

-- Does `&name` / `&mut name` (or a `borrow name as ...` block) appear anywhere
-- in these expressions/statements? A scalar loop variable whose address is taken
-- in the loop body must be promoted to a stable alloca BEFORE the loop (like
-- aggregates) rather than phi-carried — otherwise the address-of promotes it to
-- memory mid-body while the loop still phi-tracks it, and the two representations
-- diverge (lost condition / re-initialized counter / silent infinite loop — C9).
mutual
partial def cexprTakesAddrOf (name : String) : CExpr → Bool
  | .borrow inner _ => (match inner with | .ident n _ => n == name | _ => false) || cexprTakesAddrOf name inner
  | .borrowMut inner _ => (match inner with | .ident n _ => n == name | _ => false) || cexprTakesAddrOf name inner
  | .binOp _ l r _ => cexprTakesAddrOf name l || cexprTakesAddrOf name r
  | .unaryOp _ o _ => cexprTakesAddrOf name o
  | .call _ _ args _ => args.any (cexprTakesAddrOf name)
  | .structLit _ _ fields _ => fields.any (fun fe => cexprTakesAddrOf name fe.2)
  | .fieldAccess o _ _ => cexprTakesAddrOf name o
  | .match_ s arms _ => cexprTakesAddrOf name s || arms.any (cmatchArmTakesAddrOf name)
  | .deref inner _ => cexprTakesAddrOf name inner
  | .arrayLit elems _ => elems.any (cexprTakesAddrOf name)
  | .arrayIndex a i _ => cexprTakesAddrOf name a || cexprTakesAddrOf name i
  | .cast inner _ => cexprTakesAddrOf name inner
  | .try_ inner _ => cexprTakesAddrOf name inner
  | .allocCall inner alloc _ => cexprTakesAddrOf name inner || cexprTakesAddrOf name alloc
  | .ifExpr c t e _ => cexprTakesAddrOf name c || cstmtsTakeAddrOf name t || cstmtsTakeAddrOf name e
  | _ => false

partial def cmatchArmTakesAddrOf (name : String) : CMatchArm → Bool
  | .enumArm _ _ _ guard body => ((guard.map (cexprTakesAddrOf name)).getD false) || cstmtsTakeAddrOf name body
  | .litArm v guard body => cexprTakesAddrOf name v || ((guard.map (cexprTakesAddrOf name)).getD false) || cstmtsTakeAddrOf name body
  | .varArm _ _ guard body => ((guard.map (cexprTakesAddrOf name)).getD false) || cstmtsTakeAddrOf name body
  | .rangeArm lo hi _ guard body => cexprTakesAddrOf name lo || cexprTakesAddrOf name hi || ((guard.map (cexprTakesAddrOf name)).getD false) || cstmtsTakeAddrOf name body

partial def cstmtTakesAddrOf (name : String) : CStmt → Bool
  | .letDecl _ _ _ v => cexprTakesAddrOf name v
  | .assign _ v => cexprTakesAddrOf name v
  | .return_ (some v) _ => cexprTakesAddrOf name v
  | .return_ none _ => false
  | .expr e _ => cexprTakesAddrOf name e
  | .ifElse c t e => cexprTakesAddrOf name c || cstmtsTakeAddrOf name t ||
      (match e with | some el => cstmtsTakeAddrOf name el | none => false)
  | .while_ c b _ step => cexprTakesAddrOf name c || cstmtsTakeAddrOf name b || cstmtsTakeAddrOf name step
  | .fieldAssign o _ v => cexprTakesAddrOf name o || cexprTakesAddrOf name v
  | .derefAssign t v => cexprTakesAddrOf name t || cexprTakesAddrOf name v
  | .arrayIndexAssign a i v => cexprTakesAddrOf name a || cexprTakesAddrOf name i || cexprTakesAddrOf name v
  | .break_ (some v) _ => cexprTakesAddrOf name v
  | .break_ none _ => false
  | .defer b => cexprTakesAddrOf name b
  | .borrowIn var _ _ _ _ body => var == name || cstmtsTakeAddrOf name body
  | .continue_ _ => false

partial def cstmtsTakeAddrOf (name : String) (body : List CStmt) : Bool :=
  body.any (cstmtTakesAddrOf name)
end

/-- Bug 031: pre-promote any pre-branch local whose address is taken inside a
    branch arm/body (incl. method-call autoborrows — `x.m()` is `M_m(&x)` in
    Core). Lazy `addrOfLocal` promotion emits the initializing store inside the
    branch where the FIRST address-take is lowered; the promotion registration
    is global, so a sibling branch (and post-merge code on that path) reads an
    UNINITIALIZED alloca. Call this in the still-dominating block BEFORE the
    conditional terminator — alloca goes to entry, the store lands here. Same
    rule the while-loop lowering applies to its body (C9). Skips types
    `addrOfLocal` never promotes (arrays/refs/ptrs/heap are already
    addressable) and already-promoted names. `takes name` = "some branch takes
    `&name`"; callers compose it from cstmtsTakeAddrOf / cmatchArmTakesAddrOf. -/
private def prePromoteAddrTaken (preVars : List (String × SVal))
    (takes : String → Bool) : LowerM Unit := do
  for (name, val) in preVars do
    match val.ty with
    | .array _ _ | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ | .heap _ => pure ()
    | vty =>
      if (← isPromoted name).isNone && takes name then
        let allocaReg ← freshReg "ifpro."
        emitEntryAlloca (.alloca allocaReg vty)
        emit (.store val (.reg allocaReg vty))
        addPromotedAlloca name allocaReg vty

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
    match ← lookupVar name with
    | some val => return val
    | none =>
      -- Check module-level constants and inline their value
      let s ← getState
      match s.constants.find? fun (n, _, _) => n == name with
      | some (_, _, constExpr) => lowerExpr constExpr
      | none =>
        -- If it's a function type and not a local var, treat as global function reference
        match ty with
        | .fn_ _ _ _ => return .reg ("@fnref." ++ name) ty
        | _ => return .reg name ty

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
      prePromoteAddrTaken preVars fun name => cexprTakesAddrOf name rhs
      if op == .and_ then
        terminateBlock (.condBr lVal rhsLabel mergeLabel)
      else
        terminateBlock (.condBr lVal mergeLabel rhsLabel)
      startBlock rhsLabel
      let rVal ← lowerExpr rhs
      emit (.store rVal (.reg resultSlot .bool))
      let rhsEndVars ← snapshotVars
      let rhsEndLabel ← getCurrentLabel
      terminateBlock (.br mergeLabel)
      -- Merge: phi any scalar var the RHS remapped (a match/if-expression in
      -- the RHS can contain arm statements), mirroring ifExpr's merge.
      startBlock mergeLabel
      for (name, preVal) in preVars do
        -- Promoted vars are memory-backed (their alloca slot carries the value;
        -- reads reload) — never phi them. Aggregates are NEVER phi'd either:
        -- emitting `phi %String` is invalid IR (E0714). A var only "changes"
        -- across a boolean `&&`/`||` RHS via reassignment, which promotes it —
        -- so a still-unpromoted aggregate cannot have changed, and skipping it
        -- is correct. (Bug found by the #35 conlog workload: `a && b` with a
        -- promoted `String` in scope produced an aggregate phi.)
        if (← isPromoted name).isSome then continue
        if (← isAggregateForPromotion preVal.ty) then continue
        let rhsV := (rhsEndVars.find? fun (n, _) => n == name).map (·.2)
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

  | .call fn _typeArgs args ty =>
    -- Intrinsic identity is by-resolution: a DEFINED function is never
    -- intercepted, whatever it is named (mirrors Check/Elab guards).
    let st ← getState
    let intrinsic := if st.definedFns.contains fn then none else resolveIntrinsic fn
    -- Handle sizeof::<T>() and alignof::<T>() → compile-time constants
    if intrinsic == some .sizeof then
      let argTy := match _typeArgs with | t :: _ => t | [] => Ty.int
      let sz ← computeTySize argTy
      return .intConst (Int.ofNat sz) .uint
    if intrinsic == some .alignof then
      let argTy := match _typeArgs with | t :: _ => t | [] => Ty.int
      let ctx ← getLayoutCtx
      return .intConst (Int.ofNat (Layout.tyAlign ctx argTy)) .uint
    -- Handle alloc(val) → malloc + store
    if intrinsic == some .alloc then
      match args.head? with
      | some arg =>
        let aVal ← lowerExpr arg
        let innerTy := arg.ty
        let szDst ← freshReg
        let sz ← computeTySize innerTy
        emit (.call (some szDst) "malloc" [.intConst (Int.ofNat sz) .int] (.ptrMut innerTy))
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
          | .generic "Heap" [t] => t
          | t => t
        if ty == .unit || ty == .never then
          emit (.call none "free" [ptrVal] .unit)
          return .unit
        else
          -- Load value before freeing
          let loadDst ← freshReg
          emit (.load loadDst ptrVal innerTy)
          emit (.call none "free" [ptrVal] .unit)
          return .reg loadDst innerTy
      | none => return .unit
    else
    -- Track borrowMut args that need write-back after the call
    let mut aVals : List SVal := []
    let mut mutBorrows : List (String × String × Ty) := []  -- (varName, allocaReg, innerTy)
    for arg in args do
      match arg with
      | .borrowMut (.ident varName innerTy) _ =>
        -- If the variable is already promoted to a stable alloca (address taken,
        -- e.g. an address-taken loop counter), pass that alloca directly — the
        -- alloca IS the storage, so no copy + write-back is needed (and a
        -- write-back via setVar would desync from the promoted alloca).
        match ← isPromoted varName with
        | some (allocaReg, _) =>
          aVals := aVals ++ [.reg allocaReg (.refMut innerTy)]
        | none =>
          -- For &mut borrows of variables: alloca, store current value, pass ptr
          -- After the call we'll load back to propagate mutations.
          -- The alloca is hoisted to the entry block so that loops don't
          -- grow the stack on every iteration.
          let curVal ← lowerExpr (.ident varName innerTy)
          let slot ← freshReg "mutref."
          emitEntryAlloca (.alloca slot innerTy)
          emit (.store curVal (.reg slot innerTy))
          aVals := aVals ++ [.reg slot (.refMut innerTy)]
          mutBorrows := mutBorrows ++ [(varName, slot, innerTy)]
      | _ =>
        -- All other borrow/place args (incl. `&mut o.f`, `&mut a[i]`,
        -- `&mut a[i].f`) lower through `borrowMut`/`placeAddr`, which GEPs into a
        -- STABLE base address (promoted local or pointer) — never a loaded copy.
        -- The old `&mut field` special case GEP'd into `lowerExpr obj`, a loaded
        -- struct VALUE, so the field write was silently lost.
        let v ← lowerExpr arg
        aVals := aVals ++ [v]
    -- Resolve fn-pointer variables: if the call target is a local variable
    -- holding a fn pointer, resolve it to the actual function / register name.
    -- For statically-known function references (@fnref.X), use the raw function name.
    -- For runtime registers, prefix with "%" to mark as indirect call target.
    let callTarget ← do
      match ← lookupVar fn with
      | some (.reg regName (.fn_ _ _ _)) =>
        if regName.startsWith "@fnref." then pure (regName.drop 7).toString
        else pure ("%" ++ regName)
      | _ => pure fn
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
    -- Bug #7 fix: reorder fields to match struct definition's canonical order
    let defFields ← lookupStructFields name
    let orderedFields := if defFields.isEmpty then fields
      else defFields.filterMap fun (defName, _) =>
        fields.find? fun (fname, _) => fname == defName
    let actualFields := if orderedFields.length == fields.length then orderedFields else fields
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
    for (fname, fieldExpr) in actualFields do
      let fVal ← lowerExpr fieldExpr
      let byteOffset ← fieldByteOffset name fname structTyArgs
      let gepDst ← freshReg
      emit (.gep gepDst baseVal [.intConst (Int.ofNat byteOffset) .int] .i8)
      emit (.store fVal (.reg gepDst fieldExpr.ty))
    let loadDst ← freshReg
    emit (.load loadDst baseVal ty)
    return .reg loadDst ty

  | .fieldAccess obj field ty =>
    let oVal ← lowerExpr obj
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
    let s ← getState
    let ed := s.enumDefs.find? fun ed => ed.name == enumName
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
    let scrVal ← lowerExpr scrutinee
    let mergeLabel ← freshLabel "merge"
    let mergeDst ← freshReg
    let mut phiIncoming : List (SVal × String) := []
    let mut allArmsTerminated := true
    -- Snapshot vars before match so each arm starts from the same state
    let preMatchVars ← snapshotVars
    -- Collect (endVars, endLabel, terminated) for each arm for var merge
    let mut armEndSnapshots : List (List (String × SVal) × String × Bool) := []

    -- Bug 031: see prePromoteAddrTaken — the store must dominate every arm.
    -- Exclude names any arm REBINDS (enum payload binding / var arm): promotion
    -- is keyed by name, so a shadowed binding's setVar would store the arm-local
    -- value into the OUTER variable's alloca.
    let armShadows (n : String) : Bool := arms.any fun arm => match arm with
      | .enumArm _ _ bindings _ _ => bindings.any fun (bn, _) => bn == n
      | .varArm v _ _ _ => v == n
      | _ => false
    prePromoteAddrTaken preMatchVars fun name =>
      !armShadows name && arms.any (cmatchArmTakesAddrOf name)

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
      for (idx, arm) in enumerate arms do
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
          let s ← getState
          let ed := s.enumDefs.find? fun ed => ed.name == enumName
          let payloadOff := match ed with
            | some ed => Layout.enumPayloadOffset layoutCtx ed enumTypeArgs
            | none => 4
          let payloadGep ← freshReg
          emit (.gep payloadGep scrVal [.intConst (Int.ofNat payloadOff) .int] .i8)
          for (fieldIdx, (bname, bty)) in enumerate bindings do
            -- `_` is a wildcard field: skip the load/bind entirely (not read).
            if bname != "_" then
              let gepDst ← freshReg
              let foff := Layout.variantFieldOffset layoutCtx vfields fieldIdx
              emit (.gep gepDst (.reg payloadGep .i8) [.intConst (Int.ofNat foff) .int] .i8)
              let loadDst ← freshReg
              emit (.load loadDst (.reg gepDst bty) bty)
              setVar bname (.reg loadDst bty)
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := phiIncoming ++ [i]
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := armEndSnapshots ++ [snap]
          startBlock nextCheck
        | .varArm binding _bindTy guard body =>
          terminateBlock (.br armLabel)
          startBlock armLabel
          if binding != "_" then setVar binding scrVal
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := phiIncoming ++ [i]
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := armEndSnapshots ++ [snap]
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
            phiIncoming := phiIncoming ++ [i]
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := armEndSnapshots ++ [snap]
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
            phiIncoming := phiIncoming ++ [i]
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := armEndSnapshots ++ [snap]
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
      let scrVal ← match scrutinee.ty with
        | .ref t | .refMut t => do
          let d ← freshReg "mderef."
          emit (.load d scrVal t)
          pure (.reg d t)
        | _ => pure scrVal
      for (idx, arm) in enumerate arms do
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
            phiIncoming := phiIncoming ++ [i]
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := armEndSnapshots ++ [snap]
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
            phiIncoming := phiIncoming ++ [i]
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := armEndSnapshots ++ [snap]
          startBlock nextCheck
        | .varArm binding _bindTy guard body =>
          terminateBlock (.br armLabel)
          startBlock armLabel
          if binding != "_" then setVar binding scrVal
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := phiIncoming ++ [i]
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := armEndSnapshots ++ [snap]
          startBlock nextCheck
        | .enumArm _ _ _ guard body =>
          terminateBlock (.br armLabel)
          startBlock armLabel
          let (inc?, snap) ← finishMatchArmBody guard body ty mergeLabel nextCheck
          match inc? with
          | some i =>
            phiIncoming := phiIncoming ++ [i]
            allArmsTerminated := false
          | none => pure ()
          armEndSnapshots := armEndSnapshots ++ [snap]
          startBlock nextCheck
      let term ← currentBlockTerminated
      if !term then
        -- Fallthrough after all check blocks — if the match is exhaustive
        -- (catch-all or complete enum), this block is unreachable.
        -- Mark it as unreachable to avoid creating dead-code SSA artifacts.
        terminateBlock .unreachable

    startBlock mergeLabel
    -- Merge variables modified in match arms (same logic as if/else merge)
    let liveSnapshots := armEndSnapshots.filter fun (_, _, term) => !term
    if liveSnapshots.length >= 2 then
      for (name, preVal) in preMatchVars do
        -- Promoted scalars are memory-backed; skip (see the ifElse-statement note).
        if (← isPromoted name).isSome && !(← isAggregateForPromotion preVal.ty) then continue
        let varTy := preVal.ty
        -- Collect (value, label) for arms that changed this variable
        let mut incoming : List (SVal × String) := []
        let mut anyChanged := false
        for (endVars, endLabel, _) in liveSnapshots do
          match endVars.find? fun (n, _) => n == name with
          | some (_, v) =>
            let changed := match v, preVal with
              | .reg n1 _, .reg n2 _ => n1 != n2
              | _, _ => true
            if changed then anyChanged := true
            incoming := incoming ++ [(v, endLabel)]
          | none => pure ()
        if anyChanged && incoming.length >= 2 then
          let isAgg ← isAggregateForPromotion varTy
          if isAgg then
            let allocaReg ← freshReg "match.merge."
            emitEntryAlloca (.alloca allocaReg varTy)
            for (v, fromLabel) in incoming do
              insertStoreBeforeTerm fromLabel v (.reg allocaReg varTy)
            let loadReg ← freshReg "match.load."
            emit (.load loadReg (.reg allocaReg varTy) varTy)
            setVar name (.reg loadReg varTy)
          else
            let phiReg ← freshReg "match.phi."
            emit (.phi phiReg incoming varTy)
            setVar name (.reg phiReg varTy)
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
      let cleaned := preMatchVars.map fun (n, preVal) =>
        match st.vars.find? fun (vn, _) => vn == n with
        | some (_, v) => (n, v)
        | none => (n, preVal)
      setState { st with vars := cleaned }
    else if liveSnapshots.length == 1 then
      -- Only one arm reached merge — use its vars directly, then drop
      -- arm-local bindings (same WC-0004 leakage applies).
      match liveSnapshots with
      | [(endVars, _, _)] =>
        let st ← getState
        let cleaned := preMatchVars.map fun (n, preVal) =>
          match endVars.find? fun (vn, _) => vn == n with
          | some (_, v) => (n, v)
          | none => (n, preVal)
        setState { st with vars := cleaned }
      | _ => pure ()
    else
      -- All arms terminated; merge is unreachable. Restore vars so any
      -- accidental downstream read sees the pre-match state instead of
      -- the last arm's leaked bindings.
      let st ← getState
      setState { st with vars := preMatchVars }
    -- Handle the match result value phi
    -- Filter out .unit values (arms that produce no result, e.g. side-effect blocks)
    let realPhiIncoming := phiIncoming.filter fun (v, _) => match v with | .unit => false | _ => true
    if ty == .unit || ty == .never || realPhiIncoming.isEmpty then
      -- Void-typed match or no real result values — no result to merge
      return .unit
    else if realPhiIncoming.length > 1 then
      let isAgg ← isAggregateForPromotion ty
      if isAgg then
        let allocaReg ← freshReg "match.res."
        emitEntryAlloca (.alloca allocaReg ty)
        for (v, fromLabel) in realPhiIncoming do
          insertStoreBeforeTerm fromLabel v (.reg allocaReg ty)
        let loadReg ← freshReg "match.rload."
        emit (.load loadReg (.reg allocaReg ty) ty)
        return .reg loadReg ty
      else
        emit (.phi mergeDst realPhiIncoming ty)
        return .reg mergeDst ty
    else if realPhiIncoming.length == 1 then
      match realPhiIncoming with
      | [(val, _)] => return val
      | _ => return .unit
    else
      -- All arms terminated (e.g. all return) — merge is unreachable
      terminateBlock .unreachable
      return .unit

  | .borrow inner ty =>
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
    -- If the inner value is not already a pointer/ref, alloca + store to get an address
    match innerTy with
    | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ | .heap _ =>
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
    for (idx, elem) in enumerate elems do
      let eVal ← lowerExpr elem
      let gepDst ← freshReg
      emit (.gep gepDst baseVal [.intConst (Int.ofNat idx) .int] elemTy)
      emit (.store eVal (.reg gepDst elemTy))
    -- Return alloca pointer directly (don't load) so mutations work
    return baseVal

  | .arrayIndex arr index ty =>
    let aVal ← lowerExpr arr
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
    return .reg ("@fnref." ++ name) ty

  | .try_ inner ty =>
    -- Try operator: unwrap Ok value or early-return Err
    let iVal ← lowerExpr inner
    -- Load tag as i32 from offset 0
    let tagRaw ← freshReg
    emit (.load tagRaw iVal .i32)
    let tagVal ← freshReg
    emit (.cast tagVal (.reg tagRaw .i32) .int)
    -- Compare tag == 0 (Ok variant)
    let cmpDst ← freshReg
    emit (.binOp cmpDst .eq (.reg tagVal .int) (.intConst 0 .int) .bool)
    let okLabel ← freshLabel "try.ok"
    let errLabel ← freshLabel "try.err"
    terminateBlock (.condBr (.reg cmpDst .bool) okLabel errLabel)
    -- Err path: return the whole enum (run deferred calls first)
    startBlock errLabel
    emitAllDeferredCalls
    terminateBlock (.ret (some iVal))
    -- Ok path: extract the Ok value from payload using aligned offset
    startBlock okLabel
    let layoutCtx ← getLayoutCtx
    let resultEnumName := match inner.ty with
      | .named n => n
      | .generic n _ => n
      | _ => resultEnumName
    let enumTypeArgs := typeArgsFromTy inner.ty
    let s ← getState
    let ed := s.enumDefs.find? fun ed => ed.name == resultEnumName
    let payloadOff := match ed with
      | some ed => Layout.enumPayloadOffset layoutCtx ed enumTypeArgs
      | none => 8
    let payloadGep ← freshReg
    emit (.gep payloadGep iVal [.intConst (Int.ofNat payloadOff) .int] .i8)
    let loadDst ← freshReg
    emit (.load loadDst (.reg payloadGep ty) ty)
    return .reg loadDst ty

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
    prePromoteAddrTaken preIfVars fun name =>
      cstmtsTakeAddrOf name then_ || cstmtsTakeAddrOf name else_
    terminateBlock (.condBr condVal thenLabel elseLabel)
    -- Then block. Compute termination BEFORE storing the branch value: a branch
    -- that diverged (ended in return/break/continue) has no value, so storing
    -- `lastExprVal` would emit `store void undef` into the result slot — dead code
    -- after the terminator, but LLVM still type-checks it and rejects the void
    -- store. Only the live, value-producing branch writes the slot.
    startBlock thenLabel
    lowerStmts then_
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
    lowerStmts else_
    let term2 ← currentBlockTerminated
    storeBranchResult term2 resultSlot? else_ ty "ifcast."
    let elseEndVars ← snapshotVars
    let elseEndLabel ← getCurrentLabel
    if !term2 then
      terminateBlock (.br mergeLabel)
    -- Merge block with phi nodes for variables that differ between branches
    startBlock mergeLabel
    if !term1 || !term2 then
      for (name, preVal) in preIfVars do
        -- Promoted scalars are memory-backed; skip (see the ifElse-statement note).
        if (← isPromoted name).isSome && !(← isAggregateForPromotion preVal.ty) then continue
        let thenV := if term1 then none
          else (thenEndVars.find? fun (n, _) => n == name).map (·.2)
        let elseV := if term2 then none
          else (elseEndVars.find? fun (n, _) => n == name).map (·.2)
        let vty := preVal.ty
        let thenChanged := match thenV with
          | some v => match v, preVal with
            | .reg n1 _, .reg n2 _ => n1 != n2
            | _, _ => true
          | none => false
        let elseChanged := match elseV with
          | some v => match v, preVal with
            | .reg n1 _, .reg n2 _ => n1 != n2
            | _, _ => true
          | none => false
        if thenChanged || elseChanged then
          let mut incoming : List (SVal × String) := []
          match thenV with
          | some v => incoming := incoming ++ [(v, thenEndLabel)]
          | none => pure ()
          match elseV with
          | some v => incoming := incoming ++ [(v, elseEndLabel)]
          | none => pure ()
          if incoming.length >= 2 then
            -- Bug 033: aggregates may not phi (E0714) — use the same
            -- alloca+store merge as the statement-if, including the
            -- bug-029 array rule (arrays rebind their alloca ADDRESS).
            -- Triggered by discard()'s `if true { e; }` desugar with any
            -- live String/struct var across it.
            let isAgg ← isAggregateForPromotion vty
            if isAgg then
              let allocaReg ← freshReg "ifexpr.merge."
              emitEntryAlloca (.alloca allocaReg vty)
              for (v, fromLabel) in incoming do
                insertStoreBeforeTerm fromLabel v (.reg allocaReg vty)
              match vty with
              | .array _ _ =>
                setVar name (.reg allocaReg vty)
              | _ =>
                let loadReg ← freshReg "ifexpr.load."
                emit (.load loadReg (.reg allocaReg vty) vty)
                setVar name (.reg loadReg vty)
            else if vty != .unit then
              let phiReg ← freshReg "ifphi."
              emit (.phi phiReg incoming vty)
              setVar name (.reg phiReg vty)
            else pure ()
          else match incoming with
            | [(val, _)] => setVar name val
            | _ => pure ()
    -- Load result from slot (a unit/never if-expr has no slot — yields unit).
    loadResult resultSlot? ty "ifload."

/-- Extract a value from the last statement of a body, for phi nodes.
    Uses the __last_expr var that lowerStmt(.expr) sets. -/
partial def lastExprVal (body : List CStmt) (_ty : Ty) : LowerM SVal := do
  match body.getLast? with
  | some (.expr _ true) =>
    match ← lookupVar "__last_expr" with
    | some val => pure val
    | none => pure .unit
  | some (.return_ (some _) _) => pure .unit  -- arm returned, won't reach phi
  | some (.letDecl name _ _ _) =>
    match ← lookupVar name with
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
  for body in frame.deferred do
    let _ ← lowerExpr body

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
private partial def emitDeferredUntilLoop : LowerM Unit := do
  let s ← get
  for frame in s.scopeStack do
    match frame.kind with
    | .loop | .function => break
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
    let baseAddr ← match ← placeAddr arr (.ptrMut arr.ty) with
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
  | _ => addrOfLocal place refTy

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
    match ← isPromoted name with
    | some (allocaReg, structTy) => emit (.store newVal (.reg allocaReg structTy))
    | none => setVar name newVal
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
    : LowerM (Option (SVal × String) × (List (String × SVal) × String × Bool)) := do
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
    return (none, ([], "", true))

partial def lowerStmt (stmt : CStmt) : LowerM Unit := do
  match stmt with
  | .letDecl name mutable ty value =>
    let val ← lowerExpr value
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
      | .named n | .generic n _ => do
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
      addPromotedAlloca name allocaReg ty
    else
      setVar name val

  | .assign name value =>
    let val ← lowerExpr value
    setVar name val

  | .return_ (some value) _retTy =>
    let val ← lowerExpr value
    emitAllDeferredCalls
    terminateBlock (.ret (some val))

  | .return_ none _retTy =>
    emitAllDeferredCalls
    terminateBlock (.ret none)

  | .expr e iv =>
    let v ← lowerExpr e
    -- Only a trailing value expression (no `;`) is the block's value; a discarded
    -- statement still lowers (side effects) but does not set the block value (#42).
    if iv then setVar "__last_expr" v else pure ()

  | .ifElse cond then_ else_ =>
    let condVal ← lowerExpr cond
    let thenLabel ← freshLabel "then"
    let elseLabel ← freshLabel "else"
    let mergeLabel ← freshLabel "merge"
    let preIfVars ← snapshotVars
    -- Bug 031: see prePromoteAddrTaken — the store must dominate both branches.
    prePromoteAddrTaken preIfVars fun name =>
      cstmtsTakeAddrOf name then_ ||
        (match else_ with | some el => cstmtsTakeAddrOf name el | none => false)
    terminateBlock (.condBr condVal thenLabel elseLabel)
    -- Then block
    startBlock thenLabel
    lowerStmts then_
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
    | some stmts => lowerStmts stmts
    | none => pure ()
    let elseEndVars ← snapshotVars
    let elseEndLabel ← getCurrentLabel
    let term2 ← currentBlockTerminated
    if !term2 then
      terminateBlock (.br mergeLabel)
    -- Merge with phi nodes for variables that differ between branches
    startBlock mergeLabel
    if !term1 || !term2 then
      for (name, preVal) in preIfVars do
        -- A promoted SCALAR is memory-backed and written through its alloca in
        -- the branches, so the alloca is the single source of truth — reconciling
        -- it would re-store the stale pre-if snapshot at the merge (C9-class
        -- miscompile). Promoted aggregates still use the isAgg merge path below.
        if (← isPromoted name).isSome && !(← isAggregateForPromotion preVal.ty) then continue
        let thenVal := if term1 then none
          else (thenEndVars.find? fun (n, _) => n == name).map (·.2)
        let elseVal := if term2 then none
          else (elseEndVars.find? fun (n, _) => n == name).map (·.2)
        let ty := preVal.ty
        -- Check if any branch modified this variable
        -- Check if a value truly changed (ignore void→non-void spurious changes)
        let isRealChange (newVal : SVal) (oldVal : SVal) : Bool :=
          match newVal with
          | SVal.unit => ty == .unit  -- void only counts as change for void vars
          | .reg n1 _ => match oldVal with
            | .reg n2 _ => n1 != n2
            | _ => true
          | _ => true
        let thenChanged := match thenVal with
          | some v => isRealChange v preVal
          | none => false
        let elseChanged := match elseVal with
          | some v => isRealChange v preVal
          | none => false
        if thenChanged || elseChanged then
          let mut incoming : List (SVal × String) := []
          match thenVal with
          | some v =>
            -- Use pre-if value for void in non-void context
            match v with
            | SVal.unit =>
              if ty == .unit then
                incoming := incoming ++ [(v, thenEndLabel)]
              else
                incoming := incoming ++ [(preVal, thenEndLabel)]
            | _ => incoming := incoming ++ [(v, thenEndLabel)]
          | none => pure ()
          match elseVal with
          | some v =>
            match v with
            | SVal.unit =>
              if ty == .unit then
                incoming := incoming ++ [(v, elseEndLabel)]
              else
                incoming := incoming ++ [(preVal, elseEndLabel)]
            | _ => incoming := incoming ++ [(v, elseEndLabel)]
          | none => pure ()
          if incoming.length >= 2 then
            let isAgg ← isAggregateForPromotion ty
            if isAgg then
              -- Aggregate: use entry-block alloca instead of phi
              -- Stores from each branch were already done by setVar→promoted path,
              -- or we retroactively insert an alloca + stores now.
              let allocaReg ← freshReg "if.merge."
              emitEntryAlloca (.alloca allocaReg ty)
              -- Re-emit stores from each branch by inserting store at end of each
              -- For correctness: we patch the last instruction of each branch block
              -- to store into the alloca before the terminator.
              -- Simpler approach: insert store-before-branch in each block.
              for (v, fromLabel) in incoming do
                insertStoreBeforeTerm fromLabel v (.reg allocaReg ty)
              match ty with
              | .array _ _ =>
                -- Bug 029: arrays live at ADDRESSES everywhere (arrayLit returns
                -- its alloca; addrOfLocal assumes it; EmitSSA stores memcpy
                -- address-form aggregates). A by-value `if.load.` rebind broke
                -- that convention — post-merge `&arr` then used a [N x T] VALUE
                -- where its address should flow (llvm-as reject). Bind the merge
                -- alloca's ADDRESS instead; the memcpy stores above filled it.
                setVar name (.reg allocaReg ty)
              | _ =>
                let loadReg ← freshReg "if.load."
                emit (.load loadReg (.reg allocaReg ty) ty)
                setVar name (.reg loadReg ty)
            else if ty != .unit then
              let phiReg ← freshReg "if.phi."
              emit (.phi phiReg incoming ty)
              setVar name (.reg phiReg ty)
            else pure ()
          else if incoming.length == 1 then
            match incoming.head? with
            | some (v, _) => setVar name v
            | none => pure ()
      -- Drop branch-local declarations (e.g. a loop counter `let mut k` declared
      -- inside a branch) so they do not leak into a later construct's variable
      -- snapshot. A leaked branch-local would make the next loop/merge build a
      -- spurious header phi whose pre-value is defined inside the branch and does
      -- not dominate the merge (H7: E0708). Keep the merged values for names that
      -- existed before the `if`; restrict the live set to exactly those names —
      -- the same scope cleanup the match lowering does (WC-0004).
      let st ← getState
      let cleaned := preIfVars.map fun (n, preVal) =>
        match st.vars.find? fun (vn, _) => vn == n with
        | some (_, v) => (n, v)
        | none => (n, preVal)
      setState { st with vars := cleaned }

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
    let mut headerPhis : List (String × String × SVal × Ty) := []
    let mut allPromotedNames : List String := []  -- all aggregate names (including outer)
    let mut newlyPromotedNames : List String := [] -- only promoted by THIS loop
    -- Promote aggregate variables BEFORE terminating the block (stores go here)
    for (name, val) in preLoopVars do
      let ty := val.ty
      -- Skip if already promoted by an outer loop
      let alreadyPromoted ← isPromoted name
      if alreadyPromoted.isSome then
        allPromotedNames := allPromotedNames ++ [name]
      else
        let isAgg ← isAggregateForPromotion ty
        -- Also promote a scalar whose address is taken in the body: it must be
        -- memory-backed (single source of truth) rather than phi-carried, else
        -- the mid-body address-of promotion collides with the loop's phi for it
        -- (lost condition / re-init counter / infinite loop — C9).
        if isAgg || cstmtsTakeAddrOf name body then
          let allocaReg ← freshReg "agg."
          emitEntryAlloca (.alloca allocaReg ty)
          emit (.store val (.reg allocaReg ty))
          addPromotedAlloca name allocaReg ty
          allPromotedNames := allPromotedNames ++ [name]
          newlyPromotedNames := newlyPromotedNames ++ [name]
    terminateBlock (.br headerLabel)
    startBlock headerLabel
    -- Create phi nodes only for scalar variables
    for (name, val) in preLoopVars do
      let ty := val.ty
      if !allPromotedNames.contains name then
        let phiReg ← freshReg "phi."
        setVar name (.reg phiReg ty)
        headerPhis := headerPhis ++ [(name, phiReg, val, ty)]
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
    let condVal ← lowerExpr cond
    -- A short-circuit `&&`/`||` in the condition ends in its scand/scor merge
    -- block, so THAT (not the header) is the exit edge's predecessor.
    let condEndLabel ← getCurrentLabel
    terminateBlock (.condBr condVal bodyLabel exitLabel)
    -- Lower body (without step, since step is separate for for-loops)
    startBlock bodyLabel
    if hasStep then
      -- Lower only non-step body
      let bodyWithoutStep := body.take (body.length - step.length)
      lowerStmts bodyWithoutStep
    else
      lowerStmts body
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
          let bodyVal := (bodyEndVars.find? fun (n, _) => n == name).map (·.2) |>.getD (.reg phiReg ty)
          let mut incoming : List (SVal × String) := []
          if !term then
            incoming := incoming ++ [(bodyVal, bodyEndLabel)]
          for (contVars, contLabel) in continueEdges do
            let contVal := (contVars.find? fun (n, _) => n == name).map (·.2) |>.getD (.reg phiReg ty)
            incoming := incoming ++ [(contVal, contLabel)]
          if incoming.length >= 2 then
            let stepPhiReg ← freshReg "step.phi."
            emit (.phi stepPhiReg incoming ty)
            setVar name (.reg stepPhiReg ty)
          else if incoming.length == 1 then
            match incoming.head? with
            | some (val, _) => setVar name val
            | none => pure ()
      lowerStmts step
      let stepEndVars ← snapshotVars
      let stepEndLabel ← getCurrentLabel
      let stepTerm ← currentBlockTerminated
      if !stepTerm then
        terminateBlock (.br headerLabel)
      -- Pop loop to get break/continue edges
      let loopInfoFinal ← popLoop
      let _ ← popScope
      -- Build header phi instructions (step block is the back-edge source)
      let mut headerPhiInsts : List SInst := []
      for (name, phiReg, preVal, ty) in headerPhis do
        let mut incoming : List (SVal × String) := [(preVal, preLoopLabel)]
        if !stepTerm then
          let stepVal := (stepEndVars.find? fun (n, _) => n == name).map (·.2) |>.getD (.reg phiReg ty)
          incoming := incoming ++ [(stepVal, stepEndLabel)]
        headerPhiInsts := headerPhiInsts ++ [.phi phiReg incoming ty]
      prependInstsToBlock headerLabel headerPhiInsts
      -- Build exit block
      startBlock exitLabel
      let breakEdges := match loopInfoFinal with
        | some info => info.breakEdges
        | none => []
      -- Replace entire var map (removes body-local vars)
      let s ← getState
      setState { s with vars := headerPhis.map fun (name, phiReg, _, ty) => (name, SVal.reg phiReg ty) }
      if !breakEdges.isEmpty then
        for (name, phiReg, _, ty) in headerPhis do
          let headerVal := SVal.reg phiReg ty
          let mut exitIncoming : List (SVal × String) := [(headerVal, condEndLabel)]
          for (breakVars, breakLabel) in breakEdges do
            let breakVal := (breakVars.find? fun (n, _) => n == name).map (·.2) |>.getD headerVal
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
    let mut headerPhiInsts : List SInst := []
    for (name, phiReg, preVal, ty) in headerPhis do
      let mut incoming : List (SVal × String) := [(preVal, preLoopLabel)]
      -- Back-edge from body end (only if body didn't fully terminate)
      if !term then
        let backVal := (bodyEndVars.find? fun (n, _) => n == name).map (·.2) |>.getD (.reg phiReg ty)
        incoming := incoming ++ [(backVal, bodyEndLabel)]
      -- Continue edges
      match loopInfoFinal with
      | some info =>
        for (contVars, contLabel) in info.continueEdges do
          let contVal := (contVars.find? fun (n, _) => n == name).map (·.2) |>.getD (.reg phiReg ty)
          incoming := incoming ++ [(contVal, contLabel)]
      | none => pure ()
      headerPhiInsts := headerPhiInsts ++ [.phi phiReg incoming ty]
    prependInstsToBlock headerLabel headerPhiInsts
    -- Build exit phis if there are break edges
    startBlock exitLabel
    let breakEdges := match loopInfoFinal with
      | some info => info.breakEdges
      | none => []
    -- Replace entire var map (removes body-local vars)
    let s ← getState
    setState { s with vars := headerPhis.map fun (name, phiReg, _, ty) => (name, SVal.reg phiReg ty) }
    -- If break edges exist, insert exit phis
    if !breakEdges.isEmpty then
      for (name, phiReg, _, ty) in headerPhis do
        let headerVal := SVal.reg phiReg ty
        let mut exitIncoming : List (SVal × String) := [(headerVal, condEndLabel)]
        for (breakVars, breakLabel) in breakEdges do
          let breakVal := (breakVars.find? fun (n, _) => n == name).map (·.2) |>.getD headerVal
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
    let tVal ← lowerExpr target
    let vVal ← lowerExpr value
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
      | _, _ => pure ()
      emitDeferredUntilLoop
      let vars ← snapshotVars
      let label ← getCurrentLabel
      addBreakEdgeToLoop vars label info.exitLabel
      terminateBlock (.br info.exitLabel)
    | none => pure ()

  | .continue_ contLabel =>
    match ← findLoopByLabel contLabel with
    | some info =>
      let target := if info.continueTarget != "" then info.continueTarget else info.headerLabel
      emitDeferredUntilLoop
      let vars ← snapshotVars
      let label ← getCurrentLabel
      addContinueEdgeToLoop vars label info.headerLabel
      terminateBlock (.br target)
    | none => pure ()

  | .defer body =>
    addDeferredToCurrentScope body

  | .borrowIn var ref _region isMut refTy body =>
    -- Create a memory slot for the borrowed variable, set ref to point to it.
    -- The ref variable is stored with the full reference type (refMut T / ref T)
    -- so that when passed as a function argument, the correct pointer type is emitted.
    -- The alloca is hoisted to the entry block so it dominates all uses, including
    -- across early returns inside the borrow block body.
    let curVal ← lookupVar var
    let innerTy := match refTy with
      | .ref t | .refMut t | .ptrMut t | .ptrConst t => t
      | _ => refTy
    let slot ← freshReg "borrow."
    emitEntryAlloca (.alloca slot innerTy)
    match curVal with
    | some cv => emit (.store cv (.reg slot innerTy))
    | none => pure ()
    setVar ref (.reg slot refTy)
    lowerStmts body
    -- For mutable borrows, load back the value and update the original variable.
    -- Skip write-back if the body terminated early (return/break) — the write-back
    -- would be dead code and reference registers from non-dominating blocks.
    if isMut then
      let s ← getState
      if !s.blockTerminated then
        let loadBack ← freshReg "wb."
        emit (.load loadBack (.reg slot innerTy) innerTy)
        setVar var (.reg loadBack innerTy)

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
    (definedFns : List String := []) : Except Diagnostics (SFnDef × List (String × String)) :=
  let initState : LowerState := {
    blocks := []
    currentLabel := "entry"
    currentInsts := []
    labelCounter := 0
    regCounter := 0
    vars := f.params.map fun (n, ty) => (n, SVal.reg n ty)
    stringLits := []
    structDefs := structDefs
    enumDefs := enumDefs
    newtypes := newtypes
    loopStack := []
    constants := constants
    definedFns := definedFns
    scopeStack := [{ kind := .function }]
  }
  let result := (do
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
    let blocks' := match finalState.blocks with
      | [] => []
      | entry :: rest =>
        { entry with insts := finalState.entryAllocas ++ entry.insts } :: rest
    .ok ({
      name := f.name
      params := f.params
      retTy := f.retTy
      blocks := blocks'
      isTest := f.isTest
      isEntryPoint := f.isEntryPoint
      declSpan := f.declSpan
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

private partial def collectAllConstants (m : CModule) : List (String × Ty × CExpr) :=
  let own := m.constants
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllConstants s) []
  own ++ sub

private partial def collectAllExterns (m : CModule) : List (String × List (String × Ty) × Ty) :=
  let own := m.externFns.map fun (n, ps, rt, _) => (n, ps, rt)
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllExterns s) []
  own ++ sub

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
  let extStructs := (program.flatMap collectAllStructs).filter (fun sd =>
    !(ownStructs.any (·.name == sd.name)) && sd.name != "String")
    |>.foldl (fun acc sd => if acc.any (·.name == sd.name) then acc else acc ++ [sd]) []
  let extEnums := (program.flatMap collectAllEnums).filter (fun ed =>
    !(ownEnums.any (·.name == ed.name)))
    |>.foldl (fun acc ed => if acc.any (·.name == ed.name) then acc else acc ++ [ed]) []
  let extNewtypes := (program.flatMap collectAllNewtypes).filter (fun nt =>
    !(ownNewtypes.any (·.name == nt.name)))
    |>.foldl (fun acc nt => if acc.any (·.name == nt.name) then acc else acc ++ [nt]) []
  let allStructs := syntheticStringDef :: (ownStructs ++ extStructs)
  let allEnums := ownEnums ++ extEnums
  let allExterns := collectAllExterns m
  let allConstants := collectAllConstants m
  let allNewtypes := ownNewtypes ++ extNewtypes
  -- Skip generic functions (non-empty typeParams); only their monomorphized
  -- specializations should be lowered.
  let concreteFns := allFunctionsWithPath.filter fun (f, _) => f.typeParams.isEmpty
  let definedFnNames := allFunctionsWithPath.map fun (f, _) => f.name
  let results ← concreteFns.foldlM (init := []) fun acc (f, path) =>
    match lowerFn f allStructs allEnums allConstants allNewtypes definedFnNames with
    | .ok (sfn, lits) => .ok (acc ++ [({ sfn with modulePath := path }, lits)])
    | .error ds => .error (ds.map (·.addContext s!"while lowering function '{f.name}'"))
  -- Build deduplicated globals list (by string value)
  -- Prefix with module name to avoid collisions across modules
  let globals := results.foldl (fun deduped (_, lits) =>
    lits.foldl (fun deduped (_, strVal) =>
      if deduped.any fun (_, v) => v == strVal then deduped
      else deduped ++ [(s!"{m.name}.str.{deduped.length}", strVal)]
    ) deduped
  ) ([] : List (String × String))
  -- Rename strConst references per-function using its own string literals
  let fns := results.map fun (fn, lits) =>
    -- Build per-function rename map: old local name → global canonical name
    let renameMap := lits.filterMap fun (oldName, strVal) =>
      match globals.find? fun (_, v) => v == strVal with
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
  }
  return result

end Concrete
