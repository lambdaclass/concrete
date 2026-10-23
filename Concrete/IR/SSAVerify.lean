import Std.Data.HashSet
import Std.Data.HashMap
import Concrete.IR.SSA
import Concrete.Report.Diagnostic
import Concrete.Resolve.Shared
import Concrete.Semantics.IntArith

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

/-- Dominance over one function's CFG, answered in O(1) per query (bug 178).

    Nodes are the DISTINCT block labels, numbered by first occurrence. `reach`
    says whether a node is reachable from a root (the entry block or a block with
    no predecessors); `pre`/`post` are entry/exit times of a DFS over the
    dominator tree, so `d` dominates `n` iff `d` is an ancestor-or-self of `n`.

    The relation is exactly the one the old list-based dataflow computed, which
    was the GREATEST fixpoint of `D(entry) = {entry}`, `D(r) = {r}` for a block
    `r` with no predecessors, and `D(n) = {n} ∪ ⋂ D(p)` over `n`'s predecessors:
    * a node reachable from a root: its classical dominators in the graph where a
      virtual root precedes the entry and every predecessor-less block;
    * a node reachable from NO root (only from cycles nothing enters): every
      label, because the greatest fixpoint never shrinks such a set. -/
structure DomInfo where
  index : Std.HashMap String Nat
  reach : Array Bool
  pre : Array Nat
  post : Array Nat

/-- Node-level dominance: `dn` dominates `ni` (reflexively). -/
def DomInfo.dominatesIdx (di : DomInfo) (dn ni : Nat) : Bool :=
  if !di.reach[ni]! then true
  else di.reach[dn]! && di.pre[dn]! ≤ di.pre[ni]! && di.post[ni]! ≤ di.post[dn]!

/-- Does block `d` dominate block `n` (reflexively)? `false` when either is not
    a block label (the old `dominatedBy` answered `[]` for an unknown block). -/
def DomInfo.dominates (di : DomInfo) (d n : String) : Bool :=
  match di.index.get? n, di.index.get? d with
  | some ni, some dn => di.dominatesIdx dn ni
  | _, _ => false

structure VerifyCtx where
  fnName : String
  /-- All block labels in the current function (membership only). -/
  blockLabels : Std.HashSet String
  /-- Map from block label to its predecessor labels (in block order). -/
  predecessors : Std.HashMap String (List String)
  /-- Function parameter names. -/
  paramNames : List String
  /-- Dominance relation (O(1) queries). -/
  dom : DomInfo
  /-- Register → dominator-graph nodes of every block that defines it. -/
  defNodes : Std.HashMap String (List Nat)
  /-- Register → label of the FIRST block (in block order) that defines it. -/
  firstDefBlock : Std.HashMap String String
  /-- All blocks in the current function. -/
  blocks : List SBlock
  /-- Known function signatures: name → (paramTypes, returnType); the FIRST
      signature of a name wins, as the old `find?` over the list did. -/
  fnSigs : Std.HashMap String (List Ty × Ty)
  /-- Return type of the current function. -/
  retTy : Ty
  /-- Declaration span of the current function, so verifier errors point at
      the source function instead of nowhere (ROADMAP Phase 4 #13b). -/
  declSpan : Option Span := none
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
  { ctx with errors := ctx.errors ++ [{ severity := .error, message := s!"{ctx.fnName}: {msg}", pass := "ssa-verify", span := ctx.declSpan, hint := none, code := code }] }

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
  -- The indirect target counts as a use. While the callee was a String the
  -- verifier could not see it at all, so a call through an undefined register
  -- passed verification and was caught downstream by llvm-as instead of here
  -- (bug 056's second half).
  | .call _ callee args _ =>
    let argRegs := args.foldl (fun acc a => acc ++ svalRegs a) []
    match callee with
    | .direct _ | .foreign _ => argRegs
    | .indirect target => svalRegs target ++ argRegs
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

/-- Build predecessor map: label → labels of the blocks (in block order, each
    once) whose terminator targets it. One pass over the blocks with a HashMap;
    the old per-label `blocks.filter` was O(blocks²) (bug 178). Only labels of
    existing blocks are keys, as before. -/
private def buildPredecessors (blocks : List SBlock) : Std.HashMap String (List String) :=
  let init : Std.HashMap String (List String) :=
    blocks.foldl (fun m b => if m.contains b.label then m else m.insert b.label []) ∅
  -- Accumulate reversed (prepend), reverse once at the end.
  let rev := blocks.foldl (fun m b =>
    (termSuccessors b.term).eraseDups.foldl (fun m s =>
      match m.get? s with
      | some ps => m.insert s (b.label :: ps)
      | none => m) m) init
  rev.fold (fun m k ps => m.insert k ps.reverse) ∅

-- ============================================================
-- Dominator computation
-- ============================================================

/-- Iterative DFS from `root`: returns (visited, postorder). -/
private partial def dfsPostorder (succ : Array (List Nat))
    (stack : List (Nat × List Nat)) (visited : Array Bool) (order : Array Nat) :
    Array Bool × Array Nat :=
  match stack with
  | [] => (visited, order)
  | (v, []) :: rest => dfsPostorder succ rest visited (order.push v)
  | (v, s :: ss) :: rest =>
    if visited[s]! then dfsPostorder succ ((v, ss) :: rest) visited order
    else dfsPostorder succ ((s, succ[s]!) :: (v, ss) :: rest) (visited.set! s true) order

/-- Cooper–Harvey–Kennedy `intersect`: walk the two fingers up the current
    dominator tree (by postorder number) until they meet. -/
private partial def domIntersect (idom : Array (Option Nat)) (po : Array Nat) (a b : Nat) : Nat :=
  if a == b then a
  else if po[a]! < po[b]! then domIntersect idom po ((idom[a]!).getD b) b
  else domIntersect idom po a ((idom[b]!).getD a)

/-- Cooper–Harvey–Kennedy fixpoint over the nodes in reverse postorder. A root
    (entry or predecessor-less block) is pinned to the virtual root `vroot`. -/
private partial def domFixpoint (preds : Array (List Nat)) (isRoot : Array Bool)
    (po : Array Nat) (rpo : List Nat) (vroot : Nat) (idom : Array (Option Nat)) :
    Array (Option Nat) :=
  let (idom', changed) := rpo.foldl (fun (idom, ch) v =>
    if v == vroot then (idom, ch)
    else
      let new? : Option Nat :=
        if isRoot[v]! then some vroot
        else
          match (preds[v]!).filter (fun p => (idom[p]!).isSome) with
          | [] => none
          | p :: ps => some (ps.foldl (fun a q => domIntersect idom po a q) p)
      match new? with
      | some new => if idom[v]! == some new then (idom, ch) else (idom.set! v (some new), true)
      | none => (idom, ch)) (idom, false)
  if changed then domFixpoint preds isRoot po rpo vroot idom' else idom'

/-- Entry/exit times of a DFS over the dominator tree (one shared counter). -/
private partial def domNumber (children : Array (List Nat)) (stack : List (Nat × Bool))
    (pre post : Array Nat) (c : Nat) : Array Nat × Array Nat :=
  match stack with
  | [] => (pre, post)
  | (v, true) :: rest => domNumber children rest pre (post.set! v c) (c + 1)
  | (v, false) :: rest =>
    let stack := (children[v]!).foldl (fun st ch => (ch, false) :: st) ((v, true) :: rest)
    domNumber children stack (pre.set! v c) post (c + 1)

/-- Compute the dominance relation of a function's CFG (bug 178).

    Replaces a list-based Gauss-Seidel dataflow whose sets were `List String`:
    every intersection was `a.filter (s.contains ·)`, every lookup a `find?`, so
    one pass cost O(blocks · |dom set|²) — cubic on a long block chain (a wide
    `match` lowers to one) — and `strictDomDefs` then materialized, per block, the
    defs of every strict dominator. Here: label → node index (HashMap), CHK
    immediate dominators over Arrays in reverse postorder, and O(1) ancestor
    queries by dominator-tree DFS times. See `DomInfo` for why the relation is
    the one the old fixpoint computed, unreachable regions included. -/
private def computeDomInfo (blocks : List SBlock)
    (predecessors : Std.HashMap String (List String)) : DomInfo :=
  -- Distinct labels, numbered by first occurrence (the old code's `find?` always
  -- resolved a label to its first occurrence too).
  let (index, n) := blocks.foldl (fun (m, k) b =>
    if m.contains b.label then (m, k) else (m.insert b.label k, k + 1))
    ((∅ : Std.HashMap String Nat), 0)
  let vroot := n
  let preds : Array (List Nat) := index.fold (fun a l i =>
    a.set! i ((predecessors.getD l []).filterMap index.get?)) (Array.replicate (n + 1) [])
  -- Roots: the entry (node 0) and every block with no predecessor.
  let isRoot : Array Bool := (Array.range (n + 1)).map fun i => i < n && (i == 0 || (preds[i]!).isEmpty)
  let succ0 : Array (List Nat) := (Array.range (n + 1)).foldl (fun a i =>
    (preds[i]!).foldl (fun a p => a.set! p (i :: a[p]!)) a) (Array.replicate (n + 1) [])
  let rootList := (List.range n).filter fun i => isRoot[i]!
  let succ := succ0.set! vroot rootList
  let (visited, order) := dfsPostorder succ [(vroot, rootList)]
    ((Array.replicate (n + 1) false).set! vroot true) #[]
  let po : Array Nat := (List.range order.size).foldl (fun a k => a.set! order[k]! k)
    (Array.replicate (n + 1) 0)
  let rpo := order.toList.reverse
  let idom := domFixpoint preds isRoot po rpo vroot
    ((Array.replicate (n + 1) none).set! vroot (some vroot))
  let children : Array (List Nat) := (List.range n).foldr (fun v a =>
    match idom[v]! with
    | some p => if visited[v]! then a.set! p (v :: a[p]!) else a
    | none => a) (Array.replicate (n + 1) [])
  let (pre, post) := domNumber children [(vroot, false)]
    (Array.replicate (n + 1) 0) (Array.replicate (n + 1) 0) 0
  { index := index, reach := visited.extract 0 n, pre := pre, post := post }

/-- Registers defined in a block that strictly dominates `blockLabel`: is `u`
    one of them? Same answer as the old `strictDomDefs ... |>.contains u`, which
    listed the defs of every block whose label is in the dominator set of
    `blockLabel` other than `blockLabel` itself. -/
private def definedInStrictDom (ctx : VerifyCtx) (blockLabel : String) (u : String) : Bool :=
  match ctx.dom.index.get? blockLabel, ctx.defNodes.get? u with
  | some bn, some ds => ds.any fun d => d != bn && ctx.dom.dominatesIdx d bn
  | _, _ => false

/-- Get which block defines a register (the first in block order). -/
private def regDefBlock (ctx : VerifyCtx) (reg : String) : Option String :=
  ctx.firstDefBlock.get? reg

-- ============================================================
-- Per-block validation
-- ============================================================

/-- Check for duplicate register definitions in a block. -/
private def checkDuplicateDefs (ctx : VerifyCtx) (b : SBlock) : VerifyCtx :=
  let defs := blockDefs b
  -- `seen` is a HashSet (O(1) membership): the list `.contains` was O(n) per def,
  -- O(n^2) for a block of n defs (large array literal → n gep defs in one block).
  defs.foldl (fun (ctx, seen) d =>
    if seen.contains d then
      (addSSAError ctx (.duplicateRegDef b.label d), seen)
    else
      (ctx, seen.insert d)
  ) (ctx, (∅ : Std.HashSet String)) |>.1

/-- Check that all used registers are defined before use.
    Non-phi instructions are checked in program order within the block.
    Phi operands are checked against predecessor block dominator sets. -/
private def checkUsesAreDefined (ctx : VerifyCtx) (b : SBlock) : VerifyCtx :=
  let phiDefs := blockPhiDefs b
  -- Membership in "defined in a strictly dominating block" is answered per use
  -- (`definedInStrictDom`, O(defs of u)), not by materializing every dominator's
  -- defs for every block — that list was O(blocks · defs) per block (bug 178).
  let strictDom := definedInStrictDom ctx b.label
  -- Walk non-phi instructions in order, accumulating defs
  let nonPhiInsts := b.insts.filter fun inst => match inst with | .phi _ _ _ => false | _ => true
  let (ctx, runningDefs) := nonPhiInsts.foldl (fun (ctx, running) inst =>
    -- Check uses of this instruction against what's available so far
    let uses := instUses inst
    let ctx := uses.foldl (fun ctx u =>
      if ctx.paramNames.contains u then ctx
      else if phiDefs.contains u then ctx
      else if running.contains u then ctx
      else if strictDom u then ctx
      else
        match regDefBlock ctx u with
        | some defBlock =>
          if defBlock == b.label then
            addSSAError ctx (.useBeforeDef b.label u)
          else
            addSSAError ctx (.useInNonDominatingBlock b.label u defBlock)
        | none => addSSAError ctx (.undefinedRegister b.label u)
    ) ctx
    -- Add this instruction's def to running set
    let running := match instDst inst with
      | some dst => running.insert dst
      | none => running
    (ctx, running)
  ) (ctx, (∅ : Std.HashSet String))
  -- Check terminator uses: all block instructions have executed. `running` is a
  -- HashSet (O(1) membership) — the list `.contains` here was O(n) per use, i.e.
  -- O(n^2) for a block of n instructions (a large array literal: n stores in one
  -- block). Semantics are identical; only membership got faster.
  let ctx := (termUses b.term).foldl (fun ctx u =>
    if ctx.paramNames.contains u then ctx
    else if phiDefs.contains u || runningDefs.contains u || strictDom u then ctx
    else
      match regDefBlock ctx u with
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
            match regDefBlock ctx name with
            | some defBlock =>
              if ctx.dom.dominates defBlock srcLabel then ctx
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
  | .generic name _ _ =>
    name != "Vec" && name != "HashMap" && name != "HashSet" &&
    name != "Heap" && name != "HeapArray"
  | _ => false

/-- Check phi node predecessors. Each phi should have entries for all predecessor blocks. -/
private def checkPhiNodes (ctx : VerifyCtx) (b : SBlock) : VerifyCtx :=
  let preds := ctx.predecessors.getD b.label []
  let predSet : Std.HashSet String := preds.foldl (·.insert ·) ∅
  b.insts.foldl (fun ctx inst =>
    match inst with
    | .phi dst incoming ty =>
      -- Reject aggregate types in phi nodes (must use alloca+store)
      let ctx := if isAggregateType ty then
        addSSAError ctx (.aggregatePhi b.label dst (reprStr ty))
      else ctx
      let phiLabelList := incoming.map (·.2)
      -- HashSets: a wide merge has one predecessor and one phi entry per arm, so
      -- list membership both ways was O(arms²) per phi (bug 178).
      let phiLabels : Std.HashSet String := phiLabelList.foldl (·.insert ·) ∅
      -- Check that each predecessor has an entry
      let ctx := preds.foldl (fun ctx p =>
        if phiLabels.contains p then ctx
        else addSSAError ctx (.phiMissingPredecessor b.label p)
      ) ctx
      -- Check for extra entries (non-predecessor labels)
      phiLabelList.foldl (fun ctx lbl =>
        if predSet.contains lbl then ctx
        else addSSAError ctx (.phiExtraPredecessor b.label lbl)
      ) ctx
    | _ => ctx
  ) ctx

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
    | .call _ (.direct fn) args _ =>
      match ctx.fnSigs.get? fn with
      | some (paramTys, _) =>
        if args.length != paramTys.length then
          addSSAError ctx (.callArityMismatch b.label fn args.length paramTys.length)
        else ctx
      | none => ctx  -- unknown function (e.g. builtin), skip
    -- An indirect target has no declared signature to compare against; arity is
    -- the fn type's business, checked before Lower.
    | .call _ (.indirect _) _ _ => ctx
    -- A C runtime symbol has no Concrete signature here.
    | .call _ (.foreign _) _ _ => ctx
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
      -- (width-only, from the IntArith reference — same width, differing sign,
      -- e.g. i8 vs u8, is intentionally allowed; SSAVerify keeps no width truth).
      else if isInteger lTy && isInteger rTy
              && (IntArith.intBitWidth lTy).map (·.1) == (IntArith.intBitWidth rTy).map (·.1) then ctx
      else addSSAError ctx (.binopTypeMismatch b.label dst (toString (repr lTy)) (toString (repr rTy)))
    | _ => ctx
  ) ctx

-- ============================================================
-- Function and module validation
-- ============================================================

private def verifyFn (f : SFnDef) (fnSigs : Std.HashMap String (List Ty × Ty)) : Diagnostics :=
  if f.blocks.isEmpty then
    [{ severity := .error, message := s!"{f.name}: {SSAVerifyError.message .functionHasNoBlocks}", pass := "ssa-verify", span := f.declSpan, hint := none }]
  else
    let blockLabels : Std.HashSet String := f.blocks.foldl (·.insert ·.label) ∅
    let predecessors := buildPredecessors f.blocks
    let paramNames := f.params.map (·.1)
    let dom := computeDomInfo f.blocks predecessors
    -- Register → defining blocks, built once (bug 178: the old per-use
    -- `blocks.find?` and per-block dominator-def lists were O(blocks²)+).
    let (defNodes, firstDefBlock) := f.blocks.foldl (fun (dn, fd) b =>
      let node := dom.index.getD b.label 0
      (blockDefs b).foldl (fun (dn, fd) r =>
        (dn.insert r (node :: dn.getD r []),
         if fd.contains r then fd else fd.insert r b.label)) (dn, fd))
      ((∅ : Std.HashMap String (List Nat)), (∅ : Std.HashMap String String))
    let ctx : VerifyCtx := {
      fnName := f.name
      blockLabels := blockLabels
      predecessors := predecessors
      paramNames := paramNames
      dom := dom
      defNodes := defNodes
      firstDefBlock := firstDefBlock
      blocks := f.blocks
      fnSigs := fnSigs
      retTy := f.retTy
      declSpan := f.declSpan
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

private def verifyModule (m : SModule) (fnSigs : Std.HashMap String (List Ty × Ty)) : Diagnostics :=
  m.functions.foldl (fun acc f => acc ++ verifyFn f fnSigs) []

def ssaVerifyProgram (modules : List SModule) : Except Diagnostics Unit :=
  -- First signature of a name wins, as the old list `find?` did.
  let fnSigs : Std.HashMap String (List Ty × Ty) := (buildFnSigs modules).foldl
    (fun m (n, ps, r) => if m.contains n then m else m.insert n (ps, r)) ∅
  let errors := modules.foldl (fun acc m => acc ++ verifyModule m fnSigs) []
  if errors.isEmpty then .ok ()
  else .error errors

end Concrete
