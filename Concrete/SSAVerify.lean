import Concrete.SSA
import Concrete.Diagnostic
import Concrete.Shared

namespace Concrete

/-! ## SSAVerify — SSA invariant validation

Runs both before and after SSACleanup (see `Pipeline.lower`), ensuring that
cleanup transformations preserve all SSA invariants. Validates:
- Every block has exactly one terminator (structurally enforced by `SBlock.term : STerm`)
- Every register used is defined before use (simplified dominance)
- Branch targets reference existing block labels
- Phi nodes have entries for all predecessor blocks
- No duplicate register definitions in same block
-/

-- ============================================================
-- Verification state
-- ============================================================

structure VerifyCtx where
  fnName : String
  /-- All block labels in the current function. -/
  blockLabels : List String
  /-- Map from block label to its predecessor labels. -/
  predecessors : List (String × List String)
  /-- Function parameter names. -/
  paramNames : List String
  /-- Dominator map: label → list of labels that dominate it. -/
  dominators : List (String × List String)
  /-- All blocks in the current function. -/
  blocks : List SBlock
  /-- Register → type mapping (params + instruction defs). -/
  regTypes : List (String × Ty)
  /-- Known function signatures: name → (paramTypes, returnType). -/
  fnSigs : List (String × List Ty × Ty)
  /-- Return type of the current function. -/
  retTy : Ty
  errors : Diagnostics

inductive SSAVerifyError where
  -- Register definitions
  | duplicateRegDef (block : String) (reg : String)
  | functionHasNoBlocks
  -- Use-def / dominance
  | useBeforeDef (block : String) (reg : String)
  | useInNonDominatingBlock (block : String) (reg : String) (defBlock : String)
  | undefinedRegister (block : String) (reg : String)
  -- Branch targets
  | branchToUnknownLabel (block : String) (label : String)
  -- Phi nodes
  | phiMissingPredecessor (block : String) (predecessor : String)
  | phiExtraPredecessor (block : String) (label : String)
  | phiNotDominatedBySrc (block : String) (reg : String) (srcLabel : String) (defBlock : String)
  | phiUndefinedRegister (block : String) (reg : String)
  | phiTypeMismatch (block : String) (dst : String) (expected : String) (got : String)
  -- Call validation
  | callArityMismatch (block : String) (fn : String) (got : Nat) (expected : Nat)
  -- Return validation
  | retVoidInNonVoidFn (block : String) (retTy : String)
  | retValueInVoidFn (block : String) (valTy : String)
  -- Aggregate phi
  | aggregatePhi (block : String) (dst : String) (ty : String)
  -- Binop validation
  | binopTypeMismatch (block : String) (dst : String) (lTy : String) (rTy : String)

def SSAVerifyError.message : SSAVerifyError → String
  | .duplicateRegDef block reg => s!"block '{block}': duplicate definition of %{reg}"
  | .functionHasNoBlocks => "function has no blocks"
  | .useBeforeDef block reg => s!"block '{block}': use of %{reg} before its definition in the same block"
  | .useInNonDominatingBlock block reg defBlock => s!"block '{block}': use of %{reg} defined in non-dominating block '{defBlock}'"
  | .undefinedRegister block reg => s!"block '{block}': use of undefined register %{reg}"
  | .branchToUnknownLabel block label => s!"block '{block}': branch to unknown label '{label}'"
  | .phiMissingPredecessor block predecessor => s!"block '{block}': phi missing entry for predecessor '{predecessor}'"
  | .phiExtraPredecessor block label => s!"block '{block}': phi has entry for non-predecessor '{label}'"
  | .phiNotDominatedBySrc block reg srcLabel defBlock => s!"block '{block}': phi operand %{reg} from '{srcLabel}' not dominated by def block '{defBlock}'"
  | .phiUndefinedRegister block reg => s!"block '{block}': phi uses undefined register %{reg}"
  | .phiTypeMismatch block dst expected got => s!"block '{block}': phi %{dst} expects type {expected} but incoming value has type {got}"
  | .callArityMismatch block fn got expected => s!"block '{block}': call @{fn} has {got} args but function expects {expected}"
  | .retVoidInNonVoidFn block retTy => s!"block '{block}': `ret void` in function returning {retTy}"
  | .retValueInVoidFn block valTy => s!"block '{block}': `ret {valTy}` in void function"
  | .aggregatePhi block dst ty => s!"block '{block}': phi %{dst} has aggregate type {ty} — use alloca+store instead"
  | .binopTypeMismatch block dst lTy rTy => s!"block '{block}': binop %{dst} has mismatched operand types: {lTy} vs {rTy}"

def SSAVerifyError.code : SSAVerifyError → String
  | .duplicateRegDef _ _ => "E0700"
  | .functionHasNoBlocks => "E0701"
  | .useBeforeDef _ _ => "E0702"
  | .useInNonDominatingBlock _ _ _ => "E0703"
  | .undefinedRegister _ _ => "E0704"
  | .branchToUnknownLabel _ _ => "E0705"
  | .phiMissingPredecessor _ _ => "E0706"
  | .phiExtraPredecessor _ _ => "E0707"
  | .phiNotDominatedBySrc _ _ _ _ => "E0708"
  | .phiUndefinedRegister _ _ => "E0709"
  | .phiTypeMismatch _ _ _ _ => "E0710"
  | .callArityMismatch _ _ _ _ => "E0711"
  | .retVoidInNonVoidFn _ _ => "E0712"
  | .retValueInVoidFn _ _ => "E0713"
  | .aggregatePhi _ _ _ => "E0714"
  | .binopTypeMismatch _ _ _ _ => "E0715"

private def addError (ctx : VerifyCtx) (msg : String) (code : String := "") : VerifyCtx :=
  { ctx with errors := ctx.errors ++ [{ severity := .error, message := s!"{ctx.fnName}: {msg}", pass := "ssa-verify", span := none, hint := none, code := code }] }

private def addSSAError (ctx : VerifyCtx) (e : SSAVerifyError) : VerifyCtx :=
  addError ctx e.message e.code

-- ============================================================
-- Collect defined registers
-- ============================================================

/-- Get the destination register of an instruction, if any. -/
private def instDst : SInst → Option String
  | .binOp dst _ _ _ _ => some dst
  | .unaryOp dst _ _ _ => some dst
  | .call dst _ _ _ => dst
  | .alloca dst _ => some dst
  | .load dst _ _ => some dst
  | .store _ _ => none
  | .gep dst _ _ _ => some dst
  | .phi dst _ _ => some dst
  | .cast dst _ _ => some dst
  | .memcpy _ _ _ => none

/-- Collect all registers defined in a block's instructions (including phis). -/
private def blockDefs (b : SBlock) : List String :=
  b.insts.filterMap instDst

/-- Collect registers defined by phi nodes in a block. -/
private def blockPhiDefs (b : SBlock) : List String :=
  b.insts.filterMap fun inst =>
    match inst with
    | .phi dst _ _ => some dst
    | _ => none

/-- Collect all registers used in an SVal. -/
private def svalRegs : SVal → List String
  | .reg name _ => [name]
  | _ => []

/-- Collect all register uses in an instruction. -/
private def instUses : SInst → List String
  | .binOp _ _ lhs rhs _ => svalRegs lhs ++ svalRegs rhs
  | .unaryOp _ _ operand _ => svalRegs operand
  | .call _ _ args _ => args.foldl (fun acc a => acc ++ svalRegs a) []
  | .alloca _ _ => []
  | .load _ ptr _ => svalRegs ptr
  | .store val ptr => svalRegs val ++ svalRegs ptr
  | .gep _ base indices _ => svalRegs base ++ indices.foldl (fun acc i => acc ++ svalRegs i) []
  | .phi _ incoming _ => incoming.foldl (fun acc (v, _) => acc ++ svalRegs v) []
  | .cast _ val _ => svalRegs val
  | .memcpy dst src _ => svalRegs dst ++ svalRegs src

/-- Collect register uses in a terminator. -/
private def termUses : STerm → List String
  | .ret (some v) => svalRegs v
  | .ret none => []
  | .br _ => []
  | .condBr cond _ _ => svalRegs cond
  | .unreachable => []

-- ============================================================
-- Build predecessor map
-- ============================================================

/-- Get successor labels from a terminator. -/
private def termSuccessors : STerm → List String
  | .br lbl => [lbl]
  | .condBr _ tl el => [tl, el]
  | .ret _ => []
  | .unreachable => []

/-- Build predecessor map: label → list of predecessor labels. -/
private def buildPredecessors (blocks : List SBlock) : List (String × List String) :=
  let allLabels := blocks.map (·.label)
  allLabels.map fun lbl =>
    let preds := blocks.filter (fun b => (termSuccessors b.term).contains lbl)
    (lbl, preds.map (·.label))

-- ============================================================
-- Dominator computation
-- ============================================================

/-- Compute dominators using iterative dataflow.
    Returns map: label → set of labels that dominate it.
    A block B dominates C if every path from entry to C goes through B. -/
private partial def computeDominators (blocks : List SBlock) (predecessors : List (String × List String)) : List (String × List String) :=
  match blocks with
  | [] => []
  | entry :: _ =>
    let allLabels := blocks.map (·.label)
    -- Initialize: entry dominates only itself, others dominated by all blocks
    let init := allLabels.map fun lbl =>
      if lbl == entry.label then (lbl, [lbl])
      else (lbl, allLabels)
    -- Iteratively refine until fixpoint
    let rec iterate (doms : List (String × List String)) (fuel : Nat) : List (String × List String) :=
      match fuel with
      | 0 => doms
      | fuel + 1 =>
        let newDoms := allLabels.map fun lbl =>
          if lbl == entry.label then (lbl, [lbl])
          else
            let preds := (predecessors.find? fun (l, _) => l == lbl).map (·.2) |>.getD []
            let predDomSets := preds.filterMap fun p =>
              (doms.find? fun (l, _) => l == p).map (·.2)
            -- Intersection of all predecessor dom sets
            let intersection := match predDomSets with
              | [] => []
              | first :: rest => rest.foldl (fun acc s => acc.filter (s.contains ·)) first
            (lbl, lbl :: intersection)
        if newDoms == doms then doms
        else iterate newDoms fuel
    iterate init (allLabels.length * 2 + 10)

/-- Look up which blocks a given block dominates. -/
private def dominatedBy (doms : List (String × List String)) (block : String) : List String :=
  (doms.find? fun (l, _) => l == block).map (·.2) |>.getD []

/-- Get which block defines a register. -/
private def regDefBlock (blocks : List SBlock) (reg : String) : Option String :=
  blocks.find? (fun b => (blockDefs b).contains reg) |>.map (·.label)

/-- Registers defined in blocks that strictly dominate the given block. -/
private def strictDomDefs (ctx : VerifyCtx) (blockLabel : String) : List String :=
  let doms := dominatedBy ctx.dominators blockLabel
  let strictDoms := doms.filter (· != blockLabel)
  ctx.blocks.filter (fun b => strictDoms.contains b.label)
    |>.foldl (fun acc b => acc ++ blockDefs b) []

-- ============================================================
-- Per-block validation
-- ============================================================

/-- Check for duplicate register definitions in a block. -/
private def checkDuplicateDefs (ctx : VerifyCtx) (b : SBlock) : VerifyCtx :=
  let defs := blockDefs b
  defs.foldl (fun (ctx, seen) d =>
    if seen.contains d then
      (addSSAError ctx (.duplicateRegDef b.label d), seen)
    else
      (ctx, d :: seen)
  ) (ctx, ([] : List String)) |>.1

/-- Check that all used registers are defined before use.
    Non-phi instructions are checked in program order within the block.
    Phi operands are checked against predecessor block dominator sets. -/
private def checkUsesAreDefined (ctx : VerifyCtx) (b : SBlock) : VerifyCtx :=
  let phiDefs := blockPhiDefs b
  let strictDomRegs := strictDomDefs ctx b.label
  -- Walk non-phi instructions in order, accumulating defs
  let nonPhiInsts := b.insts.filter fun inst => match inst with | .phi _ _ _ => false | _ => true
  let (ctx, runningDefs) := nonPhiInsts.foldl (fun (ctx, running) inst =>
    -- Check uses of this instruction against what's available so far
    let uses := instUses inst
    let ctx := uses.foldl (fun ctx u =>
      if ctx.paramNames.contains u then ctx
      else if u.startsWith "@fnref." then ctx
      else if phiDefs.contains u then ctx
      else if running.contains u then ctx
      else if strictDomRegs.contains u then ctx
      else
        match regDefBlock ctx.blocks u with
        | some defBlock =>
          if defBlock == b.label then
            addSSAError ctx (.useBeforeDef b.label u)
          else
            addSSAError ctx (.useInNonDominatingBlock b.label u defBlock)
        | none => addSSAError ctx (.undefinedRegister b.label u)
    ) ctx
    -- Add this instruction's def to running set
    let running := match instDst inst with
      | some dst => dst :: running
      | none => running
    (ctx, running)
  ) (ctx, ([] : List String))
  -- Check terminator uses: all block instructions have executed
  let allBlockDefs := phiDefs ++ runningDefs ++ strictDomRegs
  let ctx := (termUses b.term).foldl (fun ctx u =>
    if ctx.paramNames.contains u then ctx
    else if u.startsWith "@fnref." then ctx
    else if allBlockDefs.contains u then ctx
    else
      match regDefBlock ctx.blocks u with
      | some defBlock =>
        addSSAError ctx (.useInNonDominatingBlock b.label u defBlock)
      | none => addSSAError ctx (.undefinedRegister b.label u)
  ) ctx
  -- Check phi node uses: each operand must be defined in or dominating its source block
  b.insts.foldl (fun ctx inst =>
    match inst with
    | .phi _ incoming _ =>
      incoming.foldl (fun ctx (v, srcLabel) =>
        match v with
        | .reg name _ =>
          if ctx.paramNames.contains name then ctx
          else
            let srcDoms := dominatedBy ctx.dominators srcLabel
            match regDefBlock ctx.blocks name with
            | some defBlock =>
              if srcDoms.contains defBlock then ctx
              else addSSAError ctx (.phiNotDominatedBySrc b.label name srcLabel defBlock)
            | none => addSSAError ctx (.phiUndefinedRegister b.label name)
        | _ => ctx
      ) ctx
    | _ => ctx
  ) ctx

/-- Check that branch targets reference existing block labels. -/
private def checkBranchTargets (ctx : VerifyCtx) (b : SBlock) : VerifyCtx :=
  let successors := termSuccessors b.term
  successors.foldl (fun ctx lbl =>
    if ctx.blockLabels.contains lbl then ctx
    else addSSAError ctx (.branchToUnknownLabel b.label lbl)
  ) ctx

/-- Is this type an aggregate that should never appear in a phi node?
    Aggregates should be transported via alloca+store, not SSA phi.
    Generic heap types (Vec, HashMap, HashSet, Heap, HeapArray) are excluded
    because they are represented as fixed-size pointer-based structs (≤24 bytes),
    not variable-size aggregates — they are safe to pass through phi nodes. -/
private def isAggregateType : Ty → Bool
  | .named _ => true
  | .string => true
  | .array _ _ => true
  | .generic name _ =>
    name != "Vec" && name != "HashMap" && name != "HashSet" &&
    name != "Heap" && name != "HeapArray"
  | _ => false

/-- Check phi node predecessors. Each phi should have entries for all predecessor blocks. -/
private def checkPhiNodes (ctx : VerifyCtx) (b : SBlock) : VerifyCtx :=
  let preds := (ctx.predecessors.find? fun (l, _) => l == b.label).map (·.2) |>.getD []
  b.insts.foldl (fun ctx inst =>
    match inst with
    | .phi dst incoming ty =>
      -- Reject aggregate types in phi nodes (must use alloca+store)
      let ctx := if isAggregateType ty then
        addSSAError ctx (.aggregatePhi b.label dst (reprStr ty))
      else ctx
      let phiLabels := incoming.map (·.2)
      -- Check that each predecessor has an entry
      let ctx := preds.foldl (fun ctx p =>
        if phiLabels.contains p then ctx
        else addSSAError ctx (.phiMissingPredecessor b.label p)
      ) ctx
      -- Check for extra entries (non-predecessor labels)
      phiLabels.foldl (fun ctx lbl =>
        if preds.contains lbl then ctx
        else addSSAError ctx (.phiExtraPredecessor b.label lbl)
      ) ctx
    | _ => ctx
  ) ctx

-- ============================================================
-- Register type collection
-- ============================================================

/-- Get the type produced by an instruction (for its dst register). -/
private def instDefType : SInst → Option (String × Ty)
  | .binOp dst _ _ _ ty => some (dst, ty)
  | .unaryOp dst _ _ ty => some (dst, ty)
  | .call (some dst) _ _ retTy => some (dst, retTy)
  | .call none _ _ _ => none
  | .alloca dst ty => some (dst, .ptrMut ty)
  | .load dst _ ty => some (dst, ty)
  | .store _ _ => none
  | .gep dst _ _ ty => some (dst, ty)
  | .phi dst _ ty => some (dst, ty)
  | .cast dst _ tgt => some (dst, tgt)
  | .memcpy _ _ _ => none

/-- Build register type map from params and all block instructions. -/
private def buildRegTypes (params : List (String × Ty)) (blocks : List SBlock) : List (String × Ty) :=
  let paramTypes := params
  let instTypes := blocks.foldl (fun acc b =>
    acc ++ b.insts.filterMap instDefType) []
  paramTypes ++ instTypes

/-- Build function signature map from module functions + extern declarations. -/
private def buildFnSigs (modules : List SModule) : List (String × List Ty × Ty) :=
  modules.foldl (fun acc m =>
    let fnSigs := m.functions.map fun f => (f.name, f.params.map (·.2), f.retTy)
    let externSigs := m.externFns.map fun (name, params, retTy) =>
      (name, params.map (·.2), retTy)
    acc ++ fnSigs ++ externSigs) []

-- ============================================================
-- New validation checks
-- ============================================================

/-- Look up a register's type. -/
private def lookupRegType (ctx : VerifyCtx) (name : String) : Option Ty :=
  ctx.regTypes.find? (fun (n, _) => n == name) |>.map (·.2)

/-- Check phi type consistency: all incoming values should be compatible with the phi's type. -/
private def checkPhiTypes (ctx : VerifyCtx) (b : SBlock) : VerifyCtx :=
  b.insts.foldl (fun ctx inst =>
    match inst with
    | .phi dst incoming phiTy =>
      incoming.foldl (fun ctx (v, _srcLabel) =>
        let vTy := v.ty
        -- Skip unit/void values and bool↔i1 equivalence
        if phiTy == .unit || vTy == .unit then ctx
        else if phiTy == vTy then ctx
        -- Allow numeric width promotions and ref equivalences
        else if phiTy == .bool && vTy == .bool then ctx
        else addSSAError ctx (.phiTypeMismatch b.label dst (toString (repr phiTy)) (toString (repr vTy)))
      ) ctx
    | _ => ctx
  ) ctx

/-- Check call arity: argument count must match declared parameter count. -/
private def checkCallArity (ctx : VerifyCtx) (b : SBlock) : VerifyCtx :=
  b.insts.foldl (fun ctx inst =>
    match inst with
    | .call _ fn args _ =>
      match ctx.fnSigs.find? fun (n, _, _) => n == fn with
      | some (_, paramTys, _) =>
        if args.length != paramTys.length then
          addSSAError ctx (.callArityMismatch b.label fn args.length paramTys.length)
        else ctx
      | none => ctx  -- unknown function (e.g. indirect call, builtin), skip
    | _ => ctx
  ) ctx

/-- Check return coverage: non-void functions must return `some v` in every .ret. -/
private def checkReturnCoverage (ctx : VerifyCtx) (b : SBlock) : VerifyCtx :=
  match b.term with
  | .ret none =>
    if ctx.retTy != .unit && ctx.retTy != .never then
      addSSAError ctx (.retVoidInNonVoidFn b.label (toString (repr ctx.retTy)))
    else ctx
  | .ret (some v) =>
    if ctx.retTy == .unit then
      addSSAError ctx (.retValueInVoidFn b.label (toString (repr v.ty)))
    else ctx
  | _ => ctx

/-- Is this type a raw pointer? -/
private def isPointerTy : Ty → Bool
  | .ptrMut _ | .ptrConst _ => true
  | _ => false

private def intBitWidth : Ty → Nat
  | .i8 | .u8 => 8
  | .i16 | .u16 => 16
  | .i32 | .u32 => 32
  | .int | .uint => 64
  | _ => 0

/-- Check binop type consistency: both operands should have compatible types. -/
private def checkBinOpTypes (ctx : VerifyCtx) (b : SBlock) : VerifyCtx :=
  b.insts.foldl (fun ctx inst =>
    match inst with
    | .binOp dst _op lhs rhs _ty =>
      let lTy := lhs.ty
      let rTy := rhs.ty
      -- Skip if either is a constant (constants may have generic int type)
      if lTy == rTy then ctx
      else if lTy == .unit || rTy == .unit then ctx
      -- Pointer arithmetic: ptr + int or ptr - int is valid
      else if isPointerTy lTy && isInteger rTy then ctx
      -- Mixed int/uint arithmetic is allowed only if both have the same bit width
      else if isInteger lTy && isInteger rTy && intBitWidth lTy == intBitWidth rTy then ctx
      else addSSAError ctx (.binopTypeMismatch b.label dst (toString (repr lTy)) (toString (repr rTy)))
    | _ => ctx
  ) ctx

-- ============================================================
-- Function and module validation
-- ============================================================

private def verifyFn (f : SFnDef) (fnSigs : List (String × List Ty × Ty)) : Diagnostics :=
  if f.blocks.isEmpty then
    [{ severity := .error, message := s!"{f.name}: {SSAVerifyError.message .functionHasNoBlocks}", pass := "ssa-verify", span := none, hint := none }]
  else
    let blockLabels := f.blocks.map (·.label)
    let predecessors := buildPredecessors f.blocks
    let paramNames := f.params.map (·.1)
    let dominators := computeDominators f.blocks predecessors
    let regTypes := buildRegTypes f.params f.blocks
    let ctx : VerifyCtx := {
      fnName := f.name
      blockLabels := blockLabels
      predecessors := predecessors
      paramNames := paramNames
      dominators := dominators
      blocks := f.blocks
      regTypes := regTypes
      fnSigs := fnSigs
      retTy := f.retTy
      errors := []
    }
    let ctx := f.blocks.foldl (fun ctx b =>
      let ctx := checkDuplicateDefs ctx b
      let ctx := checkUsesAreDefined ctx b
      let ctx := checkBranchTargets ctx b
      let ctx := checkPhiNodes ctx b
      let ctx := checkPhiTypes ctx b
      let ctx := checkCallArity ctx b
      let ctx := checkReturnCoverage ctx b
      let ctx := checkBinOpTypes ctx b
      ctx
    ) ctx
    ctx.errors

private def verifyModule (m : SModule) (fnSigs : List (String × List Ty × Ty)) : Diagnostics :=
  m.functions.foldl (fun acc f => acc ++ verifyFn f fnSigs) []

def ssaVerifyProgram (modules : List SModule) : Except Diagnostics Unit :=
  let fnSigs := buildFnSigs modules
  let errors := modules.foldl (fun acc m => acc ++ verifyModule m fnSigs) []
  if errors.isEmpty then .ok ()
  else .error errors

end Concrete
