import Concrete.Core
import Concrete.Pipeline
import Concrete.Proof
import Concrete.Intrinsic

namespace Concrete

/-! ## ProofCore — the proof-oriented compiler pass

ProofCore is an explicit pipeline phase that runs after Core elaboration
and CoreCheck.  It produces a single artifact that every downstream
proof consumer reads from:

  1. Eligibility assessment (source + profile gates)
  2. Core→PExpr extraction (for functions that pass eligibility)
  3. Body fingerprinting (for proof identity)
  4. Call-graph / recursion / loop analysis (computed once, shared)

No downstream code should touch `CModule` directly for proof-related
questions.  ProofCore is the artifact boundary between Core and the
proof pipeline.

ProofCore does NOT define its own semantics.  It is a filter and
extractor, not a rival IR.  The semantic authority remains CoreCheck;
ProofCore identifies the subset of validated Core that the Lean proof
infrastructure can reason about today.
-/

-- ============================================================
-- Shared analysis helpers (used by eligibility + reports)
-- ============================================================

-- Call collection

mutual
partial def collectCallsExpr (e : CExpr) : List String :=
  match e with
  | .call fn _ args _ => [fn] ++ args.foldl (fun acc a => acc ++ collectCallsExpr a) []
  | .binOp _ l r _ => collectCallsExpr l ++ collectCallsExpr r
  | .unaryOp _ e _ => collectCallsExpr e
  | .structLit _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectCallsExpr v) []
  | .fieldAccess obj _ _ => collectCallsExpr obj
  | .enumLit _ _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectCallsExpr v) []
  | .match_ scrut arms _ => collectCallsExpr scrut ++ arms.foldl (fun acc a => acc ++ collectCallsArm a) []
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => collectCallsExpr inner
  | .arrayLit elems _ => elems.foldl (fun acc e => acc ++ collectCallsExpr e) []
  | .arrayIndex arr idx _ => collectCallsExpr arr ++ collectCallsExpr idx
  | .cast inner _ | .try_ inner _ => collectCallsExpr inner
  | .allocCall inner alloc _ => collectCallsExpr inner ++ collectCallsExpr alloc
  | .whileExpr cond body elseBody _ =>
    collectCallsExpr cond ++ collectCallsStmts body ++ collectCallsStmts elseBody
  | .ifExpr cond th el _ =>
    collectCallsExpr cond ++ collectCallsStmts th ++ collectCallsStmts el
  | _ => []

partial def collectCallsArm (arm : CMatchArm) : List String :=
  match arm with
  | .enumArm _ _ _ body => collectCallsStmts body
  | .litArm v body => collectCallsExpr v ++ collectCallsStmts body
  | .varArm _ _ body => collectCallsStmts body

partial def collectCallsStmt (s : CStmt) : List String :=
  match s with
  | .letDecl _ _ _ v => collectCallsExpr v
  | .assign _ v => collectCallsExpr v
  | .return_ (some v) _ => collectCallsExpr v
  | .return_ none _ => []
  | .expr e => collectCallsExpr e
  | .ifElse c t el =>
    collectCallsExpr c ++ collectCallsStmts t ++
    match el with | some stmts => collectCallsStmts stmts | none => []
  | .while_ c body _ step =>
    collectCallsExpr c ++ collectCallsStmts body ++ collectCallsStmts step
  | .fieldAssign obj _ v => collectCallsExpr obj ++ collectCallsExpr v
  | .derefAssign t v => collectCallsExpr t ++ collectCallsExpr v
  | .arrayIndexAssign arr idx v =>
    collectCallsExpr arr ++ collectCallsExpr idx ++ collectCallsExpr v
  | .break_ (some v) _ => collectCallsExpr v
  | .break_ none _ | .continue_ _ => []
  | .defer body => collectCallsExpr body
  | .borrowIn _ _ _ _ _ body => collectCallsStmts body

partial def collectCallsStmts (ss : List CStmt) : List String :=
  ss.foldl (fun acc s => acc ++ collectCallsStmt s) []
end

-- Defer collection

mutual
partial def collectDefersExpr (e : CExpr) : List String :=
  match e with
  | .call _ _ args _ => args.foldl (fun acc a => acc ++ collectDefersExpr a) []
  | .binOp _ l r _ => collectDefersExpr l ++ collectDefersExpr r
  | .unaryOp _ e _ => collectDefersExpr e
  | .structLit _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectDefersExpr v) []
  | .fieldAccess obj _ _ => collectDefersExpr obj
  | .enumLit _ _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectDefersExpr v) []
  | .match_ scrut arms _ =>
    collectDefersExpr scrut ++ arms.foldl (fun acc a => acc ++ collectDefersArm a) []
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => collectDefersExpr inner
  | .arrayLit elems _ => elems.foldl (fun acc e => acc ++ collectDefersExpr e) []
  | .arrayIndex arr idx _ => collectDefersExpr arr ++ collectDefersExpr idx
  | .cast inner _ | .try_ inner _ => collectDefersExpr inner
  | .allocCall inner alloc _ => collectDefersExpr inner ++ collectDefersExpr alloc
  | .whileExpr cond body elseBody _ =>
    collectDefersExpr cond ++ collectDefersStmts body ++ collectDefersStmts elseBody
  | _ => []

partial def collectDefersArm (arm : CMatchArm) : List String :=
  match arm with
  | .enumArm _ _ _ body => collectDefersStmts body
  | .litArm v body => collectDefersExpr v ++ collectDefersStmts body
  | .varArm _ _ body => collectDefersStmts body

partial def collectDefersStmt (s : CStmt) : List String :=
  match s with
  | .defer body =>
    let desc := match body with
      | .call fn _ _ _ => s!"defer {fn}(...)"
      | _ => "defer <expr>"
    [desc] ++ collectDefersExpr body
  | .letDecl _ _ _ v => collectDefersExpr v
  | .assign _ v => collectDefersExpr v
  | .return_ (some v) _ => collectDefersExpr v
  | .return_ none _ => []
  | .expr e => collectDefersExpr e
  | .ifElse c t el =>
    collectDefersExpr c ++ collectDefersStmts t ++
    match el with | some stmts => collectDefersStmts stmts | none => []
  | .while_ c body _ step =>
    collectDefersExpr c ++ collectDefersStmts body ++ collectDefersStmts step
  | .fieldAssign obj _ v => collectDefersExpr obj ++ collectDefersExpr v
  | .derefAssign t v => collectDefersExpr t ++ collectDefersExpr v
  | .arrayIndexAssign arr idx v =>
    collectDefersExpr arr ++ collectDefersExpr idx ++ collectDefersExpr v
  | .break_ (some v) _ => collectDefersExpr v
  | .break_ none _ | .continue_ _ => []
  | .borrowIn _ _ _ _ _ body => collectDefersStmts body

partial def collectDefersStmts (ss : List CStmt) : List String :=
  ss.foldl (fun acc s => acc ++ collectDefersStmt s) []
end

-- Raw pointer operation detection

mutual
partial def hasRawPtrOpsExpr (e : CExpr) : Bool :=
  match e with
  | .deref inner ty =>
    match ty with
    | .ptrMut _ | .ptrConst _ => true
    | _ => hasRawPtrOpsExpr inner
  | .call _ _ args _ => args.any hasRawPtrOpsExpr
  | .binOp _ l r _ => hasRawPtrOpsExpr l || hasRawPtrOpsExpr r
  | .unaryOp _ e _ => hasRawPtrOpsExpr e
  | .structLit _ _ fields _ => fields.any (fun (_, v) => hasRawPtrOpsExpr v)
  | .fieldAccess obj _ _ => hasRawPtrOpsExpr obj
  | .enumLit _ _ _ fields _ => fields.any (fun (_, v) => hasRawPtrOpsExpr v)
  | .match_ scrut arms _ =>
    hasRawPtrOpsExpr scrut || arms.any hasRawPtrOpsArm
  | .borrow inner _ | .borrowMut inner _ => hasRawPtrOpsExpr inner
  | .arrayLit elems _ => elems.any hasRawPtrOpsExpr
  | .arrayIndex arr idx _ => hasRawPtrOpsExpr arr || hasRawPtrOpsExpr idx
  | .cast inner _ | .try_ inner _ => hasRawPtrOpsExpr inner
  | .allocCall inner alloc _ => hasRawPtrOpsExpr inner || hasRawPtrOpsExpr alloc
  | .whileExpr cond body elseBody _ =>
    hasRawPtrOpsExpr cond || hasRawPtrOpsStmts body || hasRawPtrOpsStmts elseBody
  | _ => false

partial def hasRawPtrOpsArm (arm : CMatchArm) : Bool :=
  match arm with
  | .enumArm _ _ _ body => hasRawPtrOpsStmts body
  | .litArm v body => hasRawPtrOpsExpr v || hasRawPtrOpsStmts body
  | .varArm _ _ body => hasRawPtrOpsStmts body

partial def hasRawPtrOpsStmt (s : CStmt) : Bool :=
  match s with
  | .derefAssign _ _ => true
  | .letDecl _ _ _ v => hasRawPtrOpsExpr v
  | .assign _ v => hasRawPtrOpsExpr v
  | .return_ (some v) _ => hasRawPtrOpsExpr v
  | .return_ none _ => false
  | .expr e => hasRawPtrOpsExpr e
  | .ifElse c t el =>
    hasRawPtrOpsExpr c || hasRawPtrOpsStmts t ||
    match el with | some stmts => hasRawPtrOpsStmts stmts | none => false
  | .while_ c body _ step =>
    hasRawPtrOpsExpr c || hasRawPtrOpsStmts body || hasRawPtrOpsStmts step
  | .fieldAssign obj _ v => hasRawPtrOpsExpr obj || hasRawPtrOpsExpr v
  | .arrayIndexAssign arr idx v =>
    hasRawPtrOpsExpr arr || hasRawPtrOpsExpr idx || hasRawPtrOpsExpr v
  | .break_ (some v) _ => hasRawPtrOpsExpr v
  | .break_ none _ | .continue_ _ => false
  | .defer body => hasRawPtrOpsExpr body
  | .borrowIn _ _ _ _ _ body => hasRawPtrOpsStmts body

partial def hasRawPtrOpsStmts (ss : List CStmt) : Bool :=
  ss.any hasRawPtrOpsStmt
end

-- Extern name collection

partial def collectExternNames (m : CModule) : List String :=
  m.externFns.map (fun (n, _, _, _) => n) ++
  m.submodules.foldl (fun acc sub => acc ++ collectExternNames sub) []

-- Alloc intrinsic classification

private def allocIntrinsics : List String :=
  ["alloc", "vec_new", "Vec_new"]

private def freeIntrinsics : List String :=
  ["free", "destroy", "vec_free", "Vec_free", "drop_string", "String_drop"]

def isAllocCall (name : String) : Bool :=
  allocIntrinsics.contains name ||
  match resolveIntrinsic name with
  | some .alloc | some .vecNew => true
  | _ => false

def isFreeCall (name : String) : Bool :=
  freeIntrinsics.contains name ||
  name.endsWith "_destroy" ||
  match resolveIntrinsic name with
  | some .free | some .destroy | some .vecFree | some .dropString => true
  | _ => false

def returnsAllocation : Ty → Bool
  | .heap _ | .heapArray _ => true
  | .generic "Vec" _ => true
  | _ => false

-- ============================================================
-- Call graph and recursion analysis
-- ============================================================

abbrev CallGraph := List (String × List String)

/-- Collect all function names defined in a module tree (bare names). -/
private partial def allDefinedNames (m : CModule) : List String :=
  m.functions.map (·.name) ++ m.submodules.foldl (fun acc sub => acc ++ allDefinedNames sub) []

/-- Qualify a callee name: if the bare name is defined in this compilation unit,
    resolve it to qualified form. Otherwise keep it bare (it's an intrinsic or extern). -/
private def qualifyCallee (_qualPrefix : String) (definedNames : List (String × String))
    (bare : String) : String :=
  match definedNames.find? fun (b, _) => b == bare with
  | some (_, qual) => qual
  | none => bare

/-- Build qualified name map: bare name → qualified name for all functions. -/
private partial def buildQualNameMap (m : CModule) (pfx : String := "")
    : List (String × String) :=
  let qualPrefix := if pfx == "" then m.name else pfx ++ "." ++ m.name
  let entries := m.functions.map fun f => (f.name, qualPrefix ++ "." ++ f.name)
  entries ++ m.submodules.foldl (fun acc sub =>
    acc ++ buildQualNameMap sub qualPrefix) []

private partial def buildCallGraphModule (qualNameMap : List (String × String))
    (m : CModule) (pfx : String := "") : CallGraph :=
  let qualPrefix := if pfx == "" then m.name else pfx ++ "." ++ m.name
  -- Resolve a bare callee name to its qualified name.
  -- Prefer same-module match (qualPrefix.bare) over first global match.
  let resolveCallee (bare : String) : String :=
    let sameModule := qualPrefix ++ "." ++ bare
    if qualNameMap.any fun (_, q) => q == sameModule then sameModule
    else match qualNameMap.find? fun (b, _) => b == bare with
    | some (_, qual) => qual
    | none => bare  -- intrinsic, extern, or unknown
  let fnEntries := m.functions.map fun f =>
    let qualName := qualPrefix ++ "." ++ f.name
    let callees := collectCallsStmts f.body |>.eraseDups |>.map resolveCallee
    (qualName, callees)
  fnEntries ++ m.submodules.foldl (fun acc sub =>
    acc ++ buildCallGraphModule qualNameMap sub qualPrefix) []

def buildCallGraph (modules : List CModule) : CallGraph :=
  let qualNameMap := modules.foldl (fun acc m => acc ++ buildQualNameMap m) []
  modules.foldl (fun acc m => acc ++ buildCallGraphModule qualNameMap m) []

-- Tarjan's SCC

private structure TarjanState where
  index    : Nat
  stack    : List String
  onStack  : List String
  indices  : List (String × Nat)
  lowlinks : List (String × Nat)
  sccs     : List (List String)

private def TarjanState.empty : TarjanState :=
  { index := 0, stack := [], onStack := [], indices := [], lowlinks := [], sccs := [] }

private def lookupNat (assoc : List (String × Nat)) (key : String) : Nat :=
  match assoc.find? (fun (k, _) => k == key) with
  | some (_, v) => v
  | none => 0

private def setNat (assoc : List (String × Nat)) (key : String) (val : Nat) : List (String × Nat) :=
  match assoc.findIdx? (fun (k, _) => k == key) with
  | some idx => assoc.set idx (key, val)
  | none => assoc ++ [(key, val)]

def tarjanSCC (graph : CallGraph) : List (List String) :=
  let allNodes := graph.foldl (fun acc (fn, callees) =>
    let acc := if acc.contains fn then acc else acc ++ [fn]
    callees.foldl (fun a c => if a.contains c then a else a ++ [c]) acc) []
  let rec processStack
    (work : List (String × List String × Nat))
    (st : TarjanState)
    (fuel : Nat) : TarjanState :=
    match fuel with
    | 0 => st
    | fuel + 1 =>
      match work with
      | [] => st
      | (v, [], _vLow) :: rest =>
        let vLow := lookupNat st.lowlinks v
        let vIdx := lookupNat st.indices v
        let st := if vLow == vIdx then
          let rec popScc (stk : List String) (scc : List String) :=
            match stk with
            | [] => (scc, [])
            | w :: stk' =>
              let scc := scc ++ [w]
              if w == v then (scc, stk')
              else popScc stk' scc
          let (scc, newStack) := popScc st.stack []
          let newOnStack := st.onStack.filter (fun n => !scc.contains n)
          { st with stack := newStack, onStack := newOnStack, sccs := st.sccs ++ [scc] }
        else st
        match rest with
        | [] => processStack [] st fuel
        | (pv, pRemain, _pLow) :: grandRest =>
          let pLow := lookupNat st.lowlinks pv
          let newPLow := if vLow < pLow then vLow else pLow
          let st := { st with lowlinks := setNat st.lowlinks pv newPLow }
          processStack ((pv, pRemain, newPLow) :: grandRest) st fuel
      | (v, w :: ws, _vLow) :: rest =>
        if (st.indices.find? (fun (k, _) => k == w)).isNone then
          let wIdx := st.index
          let st := { st with
            index := st.index + 1
            indices := st.indices ++ [(w, wIdx)]
            lowlinks := st.lowlinks ++ [(w, wIdx)]
            stack := [w] ++ st.stack
            onStack := [w] ++ st.onStack }
          let wCallees := match graph.find? (fun (n, _) => n == w) with
            | some (_, cs) => cs
            | none => []
          processStack ((w, wCallees, wIdx) :: (v, ws, lookupNat st.lowlinks v) :: rest) st fuel
        else if st.onStack.contains w then
          let vLow := lookupNat st.lowlinks v
          let wIdx := lookupNat st.indices w
          let newLow := if wIdx < vLow then wIdx else vLow
          let st := { st with lowlinks := setNat st.lowlinks v newLow }
          processStack ((v, ws, newLow) :: rest) st fuel
        else
          processStack ((v, ws, lookupNat st.lowlinks v) :: rest) st fuel
  let st := allNodes.foldl (fun st v =>
    if (st.indices.find? (fun (k, _) => k == v)).isSome then st
    else
      let vIdx := st.index
      let st := { st with
        index := st.index + 1
        indices := st.indices ++ [(v, vIdx)]
        lowlinks := st.lowlinks ++ [(v, vIdx)]
        stack := [v] ++ st.stack
        onStack := [v] ++ st.onStack }
      let vCallees := match graph.find? (fun (n, _) => n == v) with
        | some (_, cs) => cs
        | none => []
      processStack [(v, vCallees, vIdx)] st (allNodes.length * allNodes.length + allNodes.length)
  ) TarjanState.empty
  st.sccs

inductive RecursionKind where
  | none
  | direct
  | mutual
  deriving BEq

def classifyRecursion (graph : CallGraph) (sccs : List (List String))
    : List (String × RecursionKind × List String) :=
  sccs.foldl (fun acc scc =>
    match scc with
    | [single] =>
      let callees := match graph.find? (fun (n, _) => n == single) with
        | some (_, cs) => cs
        | none => []
      if callees.contains single then
        acc ++ [(single, .direct, [single])]
      else
        acc ++ [(single, .none, [])]
    | members =>
      let entries := members.map fun m => (m, RecursionKind.mutual, members)
      acc ++ entries
  ) []

-- ============================================================
-- Loop-boundedness classification
-- ============================================================

private def isBoundedCond (cond : CExpr) : Bool :=
  match cond with
  | .binOp op _ _ _ =>
    op == .lt || op == .gt || op == .leq || op == .geq || op == .neq
  | _ => false

inductive LoopBound where
  | bounded
  | unbounded
  deriving BEq

mutual
partial def collectLoopBoundsExpr (e : CExpr) : List LoopBound :=
  match e with
  | .whileExpr cond body elseBody _ =>
    let thisBound := if isBoundedCond cond then .bounded else .unbounded
    [thisBound] ++ collectLoopBoundsStmts body ++ collectLoopBoundsStmts elseBody
  | .call _ _ args _ => args.foldl (fun acc a => acc ++ collectLoopBoundsExpr a) []
  | .binOp _ l r _ => collectLoopBoundsExpr l ++ collectLoopBoundsExpr r
  | .unaryOp _ e _ => collectLoopBoundsExpr e
  | .structLit _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectLoopBoundsExpr v) []
  | .fieldAccess obj _ _ => collectLoopBoundsExpr obj
  | .enumLit _ _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectLoopBoundsExpr v) []
  | .match_ scrut arms _ =>
    collectLoopBoundsExpr scrut ++ arms.foldl (fun acc a => acc ++ collectLoopBoundsArm a) []
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => collectLoopBoundsExpr inner
  | .arrayLit elems _ => elems.foldl (fun acc e => acc ++ collectLoopBoundsExpr e) []
  | .arrayIndex arr idx _ => collectLoopBoundsExpr arr ++ collectLoopBoundsExpr idx
  | .cast inner _ | .try_ inner _ => collectLoopBoundsExpr inner
  | .allocCall inner alloc _ => collectLoopBoundsExpr inner ++ collectLoopBoundsExpr alloc
  | .ifExpr c t e _ => collectLoopBoundsExpr c ++ collectLoopBoundsStmts t ++ collectLoopBoundsStmts e
  | _ => []

partial def collectLoopBoundsArm (arm : CMatchArm) : List LoopBound :=
  match arm with
  | .enumArm _ _ _ body => collectLoopBoundsStmts body
  | .litArm v body => collectLoopBoundsExpr v ++ collectLoopBoundsStmts body
  | .varArm _ _ body => collectLoopBoundsStmts body

partial def collectLoopBoundsStmt (s : CStmt) : List LoopBound :=
  match s with
  | .while_ cond body _ step =>
    let hasStep := !step.isEmpty
    let thisBound := if isBoundedCond cond && hasStep then .bounded else .unbounded
    [thisBound] ++ collectLoopBoundsStmts body
  | .letDecl _ _ _ v => collectLoopBoundsExpr v
  | .assign _ v => collectLoopBoundsExpr v
  | .return_ (some v) _ => collectLoopBoundsExpr v
  | .return_ none _ => []
  | .expr e => collectLoopBoundsExpr e
  | .ifElse c t el =>
    collectLoopBoundsExpr c ++ collectLoopBoundsStmts t ++
    match el with | some stmts => collectLoopBoundsStmts stmts | none => []
  | .fieldAssign obj _ v => collectLoopBoundsExpr obj ++ collectLoopBoundsExpr v
  | .derefAssign t v => collectLoopBoundsExpr t ++ collectLoopBoundsExpr v
  | .arrayIndexAssign arr idx v =>
    collectLoopBoundsExpr arr ++ collectLoopBoundsExpr idx ++ collectLoopBoundsExpr v
  | .break_ (some v) _ => collectLoopBoundsExpr v
  | .break_ none _ | .continue_ _ => []
  | .defer body => collectLoopBoundsExpr body
  | .borrowIn _ _ _ _ _ body => collectLoopBoundsStmts body

partial def collectLoopBoundsStmts (ss : List CStmt) : List LoopBound :=
  ss.foldl (fun acc s => acc ++ collectLoopBoundsStmt s) []
end

def classifyLoops (body : List CStmt) : String :=
  let bounds := collectLoopBoundsStmts body
  if bounds.isEmpty then "no loops"
  else if bounds.all (· == .bounded) then "bounded"
  else if bounds.all (· == .unbounded) then "unbounded"
  else "mixed"

-- ============================================================
-- Body fingerprinting
-- ============================================================

private partial def fingerprintExpr : CExpr → String
  | .intLit v _ => s!"(int {v})"
  | .floatLit v _ => s!"(float {v})"
  | .boolLit v => s!"(bool {v})"
  | .strLit v => s!"(str {repr v})"
  | .charLit v => s!"(char {repr v})"
  | .ident name _ => s!"(var {name})"
  | .binOp op lhs rhs _ => s!"(binop {repr op} {fingerprintExpr lhs} {fingerprintExpr rhs})"
  | .unaryOp op inner _ => s!"(unary {repr op} {fingerprintExpr inner})"
  | .call fn _ args _ => s!"(call {fn} {fingerprintExprs args})"
  | .structLit name _ fields _ =>
    let fs := fields.map fun (n, e) => s!"{n}={fingerprintExpr e}"
    s!"(struct {name} {" ".intercalate fs})"
  | .fieldAccess obj field _ => s!"(field {fingerprintExpr obj} {field})"
  | .enumLit en v _ fields _ =>
    let fs := fields.map fun (n, e) => s!"{n}={fingerprintExpr e}"
    s!"(enum {en}::{v} {" ".intercalate fs})"
  | .match_ scr arms _ =>
    let as_ := arms.map fingerprintArm
    s!"(match {fingerprintExpr scr} {" ".intercalate as_})"
  | .borrow inner _ => s!"(borrow {fingerprintExpr inner})"
  | .borrowMut inner _ => s!"(borrowmut {fingerprintExpr inner})"
  | .deref inner _ => s!"(deref {fingerprintExpr inner})"
  | .arrayLit elems _ => s!"(array {fingerprintExprs elems})"
  | .arrayIndex arr idx _ => s!"(index {fingerprintExpr arr} {fingerprintExpr idx})"
  | .cast inner ty => s!"(cast {fingerprintExpr inner} {repr ty})"
  | .fnRef name _ => s!"(fnref {name})"
  | .try_ inner _ => s!"(try {fingerprintExpr inner})"
  | .allocCall inner alloc _ => s!"(alloc {fingerprintExpr inner} {fingerprintExpr alloc})"
  | .whileExpr cond body els _ => s!"(while {fingerprintExpr cond} {fingerprintStmts body} {fingerprintStmts els})"
  | .ifExpr cond th el _ => s!"(if {fingerprintExpr cond} {fingerprintStmts th} {fingerprintStmts el})"
where
  fingerprintExprs (es : List CExpr) : String :=
    " ".intercalate (es.map fingerprintExpr)
  fingerprintArm : CMatchArm → String
    | .enumArm en v binds body => s!"(arm {en}::{v} [{" ".intercalate (binds.map Prod.fst)}] {fingerprintStmts body})"
    | .litArm val body => s!"(lit {fingerprintExpr val} {fingerprintStmts body})"
    | .varArm b _ body => s!"(var {b} {fingerprintStmts body})"
  fingerprintStmt : CStmt → String
    | .letDecl name _ _ val => s!"(let {name} {fingerprintExpr val})"
    | .assign name val => s!"(set {name} {fingerprintExpr val})"
    | .return_ (some val) _ => s!"(ret {fingerprintExpr val})"
    | .return_ none _ => "(ret)"
    | .expr e => fingerprintExpr e
    | .ifElse cond th (some el) => s!"(if {fingerprintExpr cond} {fingerprintStmts th} {fingerprintStmts el})"
    | .ifElse cond th none => s!"(if {fingerprintExpr cond} {fingerprintStmts th})"
    | .while_ cond body _ step => s!"(while {fingerprintExpr cond} {fingerprintStmts body} {fingerprintStmts step})"
    | .fieldAssign obj f val => s!"(setfield {fingerprintExpr obj} {f} {fingerprintExpr val})"
    | .derefAssign tgt val => s!"(setderef {fingerprintExpr tgt} {fingerprintExpr val})"
    | .arrayIndexAssign arr idx val => s!"(setindex {fingerprintExpr arr} {fingerprintExpr idx} {fingerprintExpr val})"
    | .break_ _ lbl => s!"(break {lbl})"
    | .continue_ lbl => s!"(continue {lbl})"
    | .defer body => s!"(defer {fingerprintExpr body})"
    | .borrowIn v r rg m _ body => s!"(borrowin {v} {r} {rg} {m} {fingerprintStmts body})"
  fingerprintStmts (ss : List CStmt) : String :=
    "[" ++ " ".intercalate (ss.map fingerprintStmt) ++ "]"

def bodyFingerprint (body : List CStmt) : String :=
  fingerprintExpr.fingerprintStmts body

-- ============================================================
-- PExpr normalization
-- ============================================================

/-- Check whether a variable name occurs free in a PExpr. -/
private partial def pexprFreeIn (name : String) : Proof.PExpr → Bool
  | .lit _ => false
  | .var n => n == name
  | .binOp _ l r => pexprFreeIn name l || pexprFreeIn name r
  | .letIn n v b => pexprFreeIn name v || (n != name && pexprFreeIn name b)
  | .ifThenElse c t e => pexprFreeIn name c || pexprFreeIn name t || pexprFreeIn name e
  | .call _ args => args.any (pexprFreeIn name)
  | .structLit _ fields => fields.any fun (_, fexpr) => pexprFreeIn name fexpr
  | .enumLit _ _ fields => fields.any fun (_, fexpr) => pexprFreeIn name fexpr
  | .fieldAccess obj _ => pexprFreeIn name obj

/-- Ordering key for commutative canonicalization.
    vars sort before lits; among vars, alphabetical; among lits, by value. -/
private def pexprSortKey : Proof.PExpr → (Nat × String)
  | .var n => (0, n)
  | .lit (.int n) => (1, toString n)
  | .lit (.bool b) => (1, toString b)
  | _ => (2, "")  -- compound exprs stay in place

private def isCommutative : Proof.PBinOp → Bool
  | .add | .mul | .eq | .ne => true
  | _ => false

/-- Normalize a PExpr to canonical form for stable proof attachment.
    Applied once after Core→PExpr extraction, before storage.

    Rewrites (applied bottom-up):
    1. Dead let elimination:  let x = v; body  →  body  (when x ∉ FV(body))
    2. Algebraic identities:  x+0→x, 0+x→x, x*1→x, 1*x→x, x*0→0, 0*x→0, x-0→x
    3. Boolean short-circuit: if true then a else b → a, if false … → b
    4. Let flattening:        let x = (let y=v; e); body → let y=v; let x=e; body
    5. Commutative ordering:  add/mul/eq/ne operands sorted by (kind, name/value) -/
partial def normalizePExpr : Proof.PExpr → Proof.PExpr
  | .lit v => .lit v
  | .var n => .var n
  | .binOp op lhs rhs =>
    let l := normalizePExpr lhs
    let r := normalizePExpr rhs
    -- Algebraic identities
    match op, l, r with
    | .add, .lit (.int 0), x | .add, x, .lit (.int 0) => x
    | .sub, x, .lit (.int 0) => x
    | .mul, .lit (.int 1), x | .mul, x, .lit (.int 1) => x
    | .mul, .lit (.int 0), _ | .mul, _, .lit (.int 0) => .lit (.int 0)
    | _, _, _ =>
      -- Commutative canonicalization: sort operands
      if isCommutative op then
        let (ln, ls) := pexprSortKey l
        let (rn, rs) := pexprSortKey r
        let swap := ln > rn || (ln == rn && ls > rs)
        if swap then .binOp op r l
        else .binOp op l r
      else .binOp op l r
  | .letIn name val body =>
    let v := normalizePExpr val
    let b := normalizePExpr body
    -- Dead let elimination
    if !pexprFreeIn name b then b
    -- Let flattening: let x = (let y = v'; e); body → let y = v'; let x = e; body
    else match v with
    | .letIn innerName innerVal innerBody =>
      normalizePExpr (.letIn innerName innerVal (.letIn name innerBody b))
    | _ => .letIn name v b
  | .ifThenElse cond thenBr elseBr =>
    let c := normalizePExpr cond
    let t := normalizePExpr thenBr
    let e := normalizePExpr elseBr
    -- Boolean short-circuit
    match c with
    | .lit (.bool true) => t
    | .lit (.bool false) => e
    | _ => .ifThenElse c t e
  | .call fn args =>
    .call fn (args.map normalizePExpr)
  | .structLit name fields =>
    .structLit name (fields.map fun (fname, fexpr) => (fname, normalizePExpr fexpr))
  | .enumLit enumName variant fields =>
    .enumLit enumName variant (fields.map fun (fname, fexpr) => (fname, normalizePExpr fexpr))
  | .fieldAccess obj field =>
    .fieldAccess (normalizePExpr obj) field

-- ============================================================
-- Core → PExpr extraction
-- ============================================================

private def binOpToPBinOp : BinOp → Option Proof.PBinOp
  | .add => some .add
  | .sub => some .sub
  | .mul => some .mul
  | .eq  => some .eq
  | .neq => some .ne
  | .lt  => some .lt
  | .leq => some .le
  | .gt  => some .gt
  | .geq => some .ge
  | _    => none

mutual
partial def cExprToPExpr : CExpr → Option Proof.PExpr
  | .intLit n _ => some (.lit (.int n))
  | .boolLit b => some (.lit (.bool b))
  | .ident name _ => some (.var name)
  | .binOp op lhs rhs _ => do
    let pop ← binOpToPBinOp op
    let pl ← cExprToPExpr lhs
    let pr ← cExprToPExpr rhs
    some (.binOp pop pl pr)
  | .call fn _ args _ => do
    let pargs ← args.mapM cExprToPExpr
    some (.call fn pargs)
  | .ifExpr cond thenBranch elseBranch _ => do
    let pc ← cExprToPExpr cond
    let pt ← cStmtsToPExpr thenBranch
    let pe ← cStmtsToPExpr elseBranch
    some (.ifThenElse pc pt pe)
  | .structLit name _typeArgs fields _ => do
    -- Each field's expression must extract; the resulting struct value
    -- pairs field names with extracted PExprs.
    let pfields ← fields.mapM fun (fname, fexpr) => do
      let pe ← cExprToPExpr fexpr
      some (fname, pe)
    some (.structLit name pfields)
  | .enumLit enumName variant _typeArgs fields _ => do
    -- Same shape as struct literal: each field's expression must
    -- extract; result is a tagged enum value (variant + fields).
    let pfields ← fields.mapM fun (fname, fexpr) => do
      let pe ← cExprToPExpr fexpr
      some (fname, pe)
    some (.enumLit enumName variant pfields)
  | .fieldAccess obj field _ => do
    let po ← cExprToPExpr obj
    some (.fieldAccess po field)
  | _ => none

/-- Extract a statement list to a pure PExpr, threading a
    continuation `k` that says "what does the function return if
    control falls off the end of these statements?"

    A return statement terminates the function and discards `k`. An
    if-without-else falls through to the surrounding scope: the
    inner if's "else" is exactly the outer if's continuation. This
    lets early-return chains (parse_validate's validator shape) and
    nested early returns (`if a { if b { return X; } } return Y;`)
    extract correctly.

    `k = none` means "no continuation, fail if control falls off."
    A function body extracts by calling this with `k = none`. -/
partial def cStmtsToPExprK : List CStmt → Option Proof.PExpr → Option Proof.PExpr
  | [], k => k
  | [.return_ (some e) _], _ => cExprToPExpr e
  | [.expr e], _ => cExprToPExpr e
  | (.letDecl name _ _ val) :: rest, k => do
    let pv ← cExprToPExpr val
    let pb ← cStmtsToPExprK rest k
    some (.letIn name pv pb)
  -- Singleton if-else (last stmt of body): each branch inherits the
  -- outer continuation. If a branch returns, k is dead; if it falls
  -- through, k is used.
  | [.ifElse cond thenBranch (some elseBranch)], k => do
    let pc ← cExprToPExpr cond
    let pt ← cStmtsToPExprK thenBranch k
    let pe ← cStmtsToPExprK elseBranch k
    some (.ifThenElse pc pt pe)
  -- If-else followed by more statements: both branches' fall-through
  -- continuation is `rest with the outer k`.
  | (.ifElse cond thenBranch (some elseBranch)) :: rest, k => do
    let pc ← cExprToPExpr cond
    let pkRest ← cStmtsToPExprK rest k
    let pt ← cStmtsToPExprK thenBranch (some pkRest)
    let pe ← cStmtsToPExprK elseBranch (some pkRest)
    some (.ifThenElse pc pt pe)
  -- If-without-else (early-return shape): then-branch's
  -- continuation is `rest with k`; the implicit else is the same.
  -- parse_validate's validator shape:
  --     if v == 1 { return 0; }
  --     return 1;
  -- becomes `if v == 1 then 0 else 1`. Nested early returns thread
  -- through because the inner if's continuation is the outer's
  -- continuation.
  | (.ifElse cond thenBranch none) :: rest, k => do
    let pc ← cExprToPExpr cond
    let pkRest ← cStmtsToPExprK rest k
    let pt ← cStmtsToPExprK thenBranch (some pkRest)
    some (.ifThenElse pc pt pkRest)
  | _, _ => none

partial def cStmtsToPExpr (stmts : List CStmt) : Option Proof.PExpr :=
  cStmtsToPExprK stmts none
end

-- Unsupported construct identification

mutual
private partial def identifyUnsupportedExpr : CExpr → List String
  | .floatLit .. => ["float literal"]
  | .strLit .. => ["string literal"]
  | .charLit .. => ["char literal"]
  -- structLit, enumLit, and fieldAccess are now supported by
  -- cExprToPExpr; their only unsupported residual is whatever's
  -- inside the field exprs / the object. Recurse so a literal of
  -- unsupported things is reported precisely, while a literal of
  -- pure-int things lists nothing.
  | .structLit _ _ fields _ =>
    fields.foldl (fun acc (_, fexpr) => acc ++ identifyUnsupportedExpr fexpr) []
  | .enumLit _ _ _ fields _ =>
    fields.foldl (fun acc (_, fexpr) => acc ++ identifyUnsupportedExpr fexpr) []
  | .fieldAccess obj _ _ => identifyUnsupportedExpr obj
  | .match_ .. => ["match expression"]
  | .borrow .. => ["borrow"]
  | .borrowMut .. => ["mutable borrow"]
  | .deref .. => ["deref"]
  | .arrayLit .. => ["array literal"]
  | .arrayIndex .. => ["array index"]
  | .cast .. => ["cast"]
  | .fnRef .. => ["function reference"]
  | .try_ .. => ["try expression"]
  | .allocCall .. => ["alloc call"]
  | .whileExpr .. => ["while expression"]
  | .unaryOp .. => ["unary operator"]
  | .binOp op lhs rhs _ =>
    let opUnsup := match binOpToPBinOp op with
      | none => [s!"unsupported operator: {repr op}"]
      | some _ => []
    opUnsup ++ identifyUnsupportedExpr lhs ++ identifyUnsupportedExpr rhs
  | .call _ _ args _ =>
    args.foldl (fun acc a => acc ++ identifyUnsupportedExpr a) []
  | .ifExpr cond thenBr elseBr _ =>
    identifyUnsupportedExpr cond ++
    thenBr.foldl (fun acc s => acc ++ identifyUnsupportedStmt s) [] ++
    elseBr.foldl (fun acc s => acc ++ identifyUnsupportedStmt s) []
  | _ => []

private partial def identifyUnsupportedStmt : CStmt → List String
  | .letDecl _ _ _ val => identifyUnsupportedExpr val
  | .return_ (some e) _ => identifyUnsupportedExpr e
  | .expr e => identifyUnsupportedExpr e
  | .ifElse cond thenBr elseBr =>
    -- if without else is supported by cStmtsToPExprK as
    -- early-return-with-fall-through. Do NOT flag it here.
    identifyUnsupportedExpr cond ++
    thenBr.foldl (fun acc s => acc ++ identifyUnsupportedStmt s) [] ++
    match elseBr with
    | some stmts => stmts.foldl (fun acc s => acc ++ identifyUnsupportedStmt s) []
    | none => []
  | _ => []
end

def identifyUnsupported (body : List CStmt) : List String :=
  -- Structural: empty body or void return
  let structural :=
    if body.isEmpty then ["empty body"]
    else match body with
    | [.return_ none _] => ["void return"]
    | _ =>
      -- Check for void return anywhere in body
      let hasVoidRet := body.any fun s => match s with
        | .return_ none _ => true
        | _ => false
      -- Check for multiple expression statements without final return
      let hasReturn := body.any fun s => match s with
        | .return_ .. => true
        | _ => false
      (if hasVoidRet then ["void return"] else []) ++
      (if !hasReturn then ["no return statement"] else [])
  let stmtKinds := body.filterMap fun s => match s with
    | .while_ .. => some "while loop"
    | .fieldAssign .. => some "field assignment"
    | .derefAssign .. => some "deref assignment"
    | .arrayIndexAssign .. => some "array index assignment"
    | .break_ .. => some "break"
    | .continue_ .. => some "continue"
    | .defer .. => some "defer"
    | .borrowIn .. => some "borrow region"
    | .assign .. => some "mutable assignment"
    | _ => none
  let exprKinds := body.foldl (fun acc s => acc ++ identifyUnsupportedStmt s) []
  (structural ++ stmtKinds ++ exprKinds).eraseDups

-- ============================================================
-- Eligibility predicates
-- ============================================================

/-- A function is proof-eligible when it is pure, not trusted, and has
    no type parameters (monomorphic or pre-mono with concrete types only). -/
def CFnDef.isProofEligible (f : CFnDef) : Bool :=
  f.capSet.isEmpty &&
  !f.isTrusted &&
  !f.isEntryPoint &&
  f.trustedImplOrigin.isNone

/-- A struct is proof-eligible when it has no FFI annotations. -/
def CStructDef.isProofEligible (s : CStructDef) : Bool :=
  !s.isReprC && !s.isPacked && s.reprAlign.isNone

/-- An enum is proof-eligible when it has no builtin override. -/
def CEnumDef.isProofEligible (e : CEnumDef) : Bool :=
  e.builtinId.isNone

-- ============================================================
-- Eligibility assessment (source + profile gates)
-- ============================================================

/-- Source location: (file, line). -/
abbrev SourceLoc := String × Nat

inductive ExclusionKind where
  | source
  | profile
  | both
  deriving Repr

structure EligibilityEntry where
  qualName       : String
  eligible       : Bool
  sourceReasons  : List String
  profileReasons : List String
  exclusionKind  : Option ExclusionKind
  isTrusted      : Bool
  loc            : Option SourceLoc

-- ============================================================
-- Proof registry types (moved from Report.lean)
-- ============================================================

/-- A single proof registry entry linking a Concrete function to its proof. -/
structure ProofRegistryEntry where
  function        : String  -- qualified name, e.g. "main.parse_byte"
  bodyFingerprint : String  -- expected body fingerprint
  proof           : String  -- Lean proof name, e.g. "Concrete.Proof.parse_byte_correct"
  spec            : String  -- spec name, e.g. "parse_byte_adds_offset"
  deriving Repr, Inhabited

abbrev ProofRegistry := List ProofRegistryEntry

/-- Parse a proof registry from a JSON string.
    Expected format:
    { "version": 1, "proofs": [ { "function": "...", "body_fingerprint": "...", "proof": "...", "spec": "..." }, ... ] }
    Returns (entries, warnings). Warnings are non-empty when the input is malformed. -/
def parseRegistryJson (input : String) : ProofRegistry × List String :=
  let extractStr (block : String) (key : String) : Option String :=
    let needle := s!"\"{key}\":"
    match block.splitOn needle with
    | [_, rest] =>
      let rest := rest.trimAsciiStart
      if rest.startsWith "\"" then
        match (rest.drop 1).toString.splitOn "\"" with
        | inner :: _ => some inner
        | [] => none
      else none
    | _ => none
  let trimmed := input.trimAscii.toString
  -- Empty file
  if trimmed.isEmpty then
    ([], ["warning: proof-registry.json is empty"])
  else
  let blocks := input.splitOn "\"function\":"
  let entryBlocks := blocks.drop 1
  -- No "function": tokens — check whether the content looks like valid-but-empty JSON
  if entryBlocks.isEmpty && trimmed.length > 2 then
    -- Accept: "[]", "[  ]", or objects/arrays that parse as valid empty structures
    let looksValidEmpty := trimmed == "[]" ||
      (trimmed.startsWith "[" && trimmed.endsWith "]") ||
      (trimmed.startsWith "{" && trimmed.endsWith "}")
    if !looksValidEmpty then
      ([], [s!"warning: proof-registry.json is malformed (no valid entries found)"])
    else
      ([], [])
  else
  let (entries, parseWarns) := entryBlocks.foldl (fun (acc : List ProofRegistryEntry × List String) block =>
    let (es, ws) := acc
    let fn := extractStr ("\"function\":" ++ block) "function"
    let fp := extractStr block "body_fingerprint"
    let pr := extractStr block "proof"
    let sp := extractStr block "spec"
    match fn with
    | none => (es, ws ++ ["warning: proof-registry.json entry has missing or malformed \"function\" field"])
    | some fnVal =>
      if fnVal.isEmpty then (es, ws ++ ["warning: proof-registry.json entry has empty \"function\" field"])
      else
        let entryWarns := (if fp.isNone then [s!"warning: proof-registry.json entry '{fnVal}' has missing \"body_fingerprint\" field"] else [])
          ++ (if pr.isNone then [s!"warning: proof-registry.json entry '{fnVal}' has missing \"proof\" field"] else [])
          ++ (if sp.isNone then [s!"warning: proof-registry.json entry '{fnVal}' has missing \"spec\" field"] else [])
        (es ++ [{ function := fnVal, bodyFingerprint := fp.getD "", proof := pr.getD "", spec := sp.getD "" }],
         ws ++ entryWarns)
  ) (([] : List ProofRegistryEntry), ([] : List String))
  -- Check for duplicates
  let dedupResult := entries.foldl (fun (acc : ProofRegistry × List String × List String) e =>
    let (ds, ws, seen) := acc
    if seen.contains e.function then
      (ds, ws ++ [s!"warning: proof-registry.json contains duplicate entry for '{e.function}'"], seen)
    else
      (ds ++ [e], ws, seen ++ [e.function])
  ) (([] : ProofRegistry), ([] : List String), ([] : List String))
  let deduped := dedupResult.1
  let dupeWarns := dedupResult.2.1
  -- Check for empty fingerprints
  let fpWarns := deduped.filterMap fun e =>
    if e.bodyFingerprint.isEmpty then
      some s!"warning: proof-registry.json entry '{e.function}' has empty body_fingerprint"
    else none
  (deduped, parseWarns ++ dupeWarns ++ fpWarns)

-- ============================================================
-- Identity and spec attachment model
-- ============================================================

/-- Canonical function identity in the proof pipeline. -/
structure FunctionIdentity where
  qualName    : String       -- e.g. "main.parse_byte"
  fingerprint : String       -- raw Core body fingerprint
  deriving BEq, Repr

/-- Spec identity — a named specification attached to a function. -/
structure SpecIdentity where
  name    : String           -- e.g. "parse_byte_adds_offset"
  version : Option String := none
  deriving BEq, Repr

/-- How a spec binding was established. -/
inductive SpecSource where
  | hardcoded   -- from Proof.provedFunctions
  | registry    -- from proof-registry.json
  deriving BEq, Repr

/-- Spec attachment for a function: identity binding only.
    Proof status (proved/stale/unproved) is derived downstream by comparing
    the attachment's expectedFp against the function's current fingerprint. -/
structure SpecAttachment where
  specId      : SpecIdentity
  proofName   : String       -- e.g. "Concrete.Proof.parse_byte_correct"
  source      : SpecSource
  expectedFp  : String       -- fingerprint the proof was written against

/-- Resolve spec attachment for a single function. Checks registry first,
    then Proof.provedFunctions. Returns none if no spec is attached. -/
private def resolveSpec (qualName : String)
    (registry : ProofRegistry) : Option SpecAttachment :=
  -- Check registry first
  match registry.find? fun re => re.function == qualName with
  | some re => some {
      specId := { name := re.spec }
      proofName := re.proof
      source := .registry
      expectedFp := re.bodyFingerprint }
  | none =>
    -- Check hardcoded
    match Proof.provedFunctions.find? fun (name, _, _) => name == qualName with
    | some (name, efp, theoremName) =>
      some {
        specId := { name := name ++ ".spec" }
        proofName := theoremName
        source := .hardcoded
        expectedFp := efp }
    | none => none

-- ============================================================
-- Obligation model
-- ============================================================

/-- Classify why a function is ineligible, based on source and profile reasons.
    Typed enum — drives failure/repair class without substring matching. -/
inductive IneligibleCategory where
  | entryPoint      -- is entry point (main)
  | effectBoundary  -- has capabilities
  | structuralGate  -- recursion, loops, allocation, FFI, blocking I/O, or combo
  deriving BEq, Repr

/-- Status of a proof obligation — derived from spec attachment,
    fingerprint comparison, and eligibility. -/
inductive ObligationStatus where
  | proved      -- spec attached, fingerprint matches, extraction succeeded
  | stale       -- spec attached, fingerprint changed
  | missing     -- passes profile, extractable, no spec attached
  | blocked     -- eligible but extraction failed (unsupported constructs)
  | ineligible  -- fails profile gates
  | trusted     -- marked trusted
  deriving BEq, Repr

/-- Canonical string representation of an ObligationStatus.
    This is the single source of truth for status terminology across
    all output surfaces: JSON facts, CLI reports, documentation, and
    release criteria. All renderers MUST use this function. -/
def ObligationStatus.canonical : ObligationStatus → String
  | .proved     => "proved"
  | .stale      => "stale"
  | .missing    => "missing"
  | .blocked    => "blocked"
  | .ineligible => "ineligible"
  | .trusted    => "trusted"

/-- All valid canonical status strings, for validation gates. -/
def ObligationStatus.allCanonical : List String :=
  ["proved", "stale", "missing", "blocked", "ineligible", "trusted"]

/-- A proof obligation generated by the proof pipeline.
    Each obligation has a stable identity (function + spec) and
    a mechanically derived status. -/
structure Obligation where
  functionId   : FunctionIdentity
  bareName     : String
  status       : ObligationStatus
  spec         : Option SpecAttachment
  expectedFp   : String           -- from attachment, or ""
  profileGates : List String      -- reasons for ineligibility (empty if eligible)
  ineligCat    : Option IneligibleCategory  -- typed ineligibility classification
  dependencies : List String      -- qualified names of proved callees
  staleDeps    : List String      -- proved callees whose proof has gone stale
  loc          : Option SourceLoc

-- ============================================================
-- Proof diagnostics
-- ============================================================

/-- Classification of proof-oriented diagnostic. -/
inductive ProofDiagnosticKind where
  | staleProof           -- spec attached, fingerprint changed
  | missingProof         -- eligible, no spec attached
  | ineligible           -- fails profile gates
  | unsupportedConstruct -- eligible, but extraction blocked by unsupported constructs
  | trusted              -- marked trusted (informational)
  | attachmentIntegrity  -- registry entry is invalid (unknown function, duplicate, etc.)
  | theoremLookup        -- Lean proof name not found
  | leanCheckFailure     -- Lean kernel rejected the proof
  deriving BEq, Repr

/-- Canonical string for diagnostic kind. Maps to the ObligationStatus
    terminology where applicable. -/
def ProofDiagnosticKind.canonical : ProofDiagnosticKind → String
  | .staleProof           => "stale"
  | .missingProof         => "missing"
  | .ineligible           => "ineligible"
  | .unsupportedConstruct => "blocked"
  | .trusted              => "trusted"
  | .attachmentIntegrity  => "attachment_integrity"
  | .theoremLookup        => "theorem_lookup"
  | .leanCheckFailure     => "lean_check_failure"

/-- Stable error code for proof diagnostic kinds. -/
def ProofDiagnosticKind.code : ProofDiagnosticKind → String
  | .staleProof           => "E0800"
  | .missingProof         => "E0801"
  | .ineligible           => "E0802"
  | .unsupportedConstruct => "E0803"
  | .trusted              => "E0804"
  | .attachmentIntegrity  => "E0805"
  | .theoremLookup        => "E0806"
  | .leanCheckFailure     => "E0807"

/-- Severity of a proof diagnostic. -/
inductive ProofDiagnosticSeverity where
  | error    -- blocks proof (stale, unsupported, attachment, lean check)
  | warning  -- needs attention (missing proof, theorem lookup)
  | info     -- informational (ineligible, trusted)
  deriving BEq, Repr

private def strContains (haystack : String) (needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

/-- Determine ineligible category from source and profile gate reasons.
    Source reasons (capabilities, entry point) take priority over profile. -/
def classifyIneligible (sourceReasons profileReasons : List String) : IneligibleCategory :=
  if sourceReasons.any (strContains · "entry point") then .entryPoint
  else if sourceReasons.any (strContains · "capabilities") then .effectBoundary
  else if profileReasons.any (strContains · "capabilities") then .effectBoundary
  else .structuralGate

/-- Failure class for an ineligible function, driven by typed category. -/
def IneligibleCategory.failureClass : IneligibleCategory → String
  | .entryPoint     => "entry_point"
  | .effectBoundary => "effect_boundary"
  | .structuralGate => "structural_gate"

/-- Repair class for an ineligible function, driven by typed category. -/
def IneligibleCategory.repairClass : IneligibleCategory → String
  | .entryPoint     => "none"
  | .effectBoundary => "policy_change"
  | .structuralGate => "code_rewrite"

/-- Failure class — what kind of failure prevents proof.
    Finer-grained than ProofDiagnosticKind: splits ineligible into
    effect_boundary, structural_gate, entry_point via typed IneligibleCategory;
    adds attachment_integrity, theorem_lookup, lean_check_failure. -/
def failureClassOf (kind : ProofDiagnosticKind)
    (ineligCat : Option IneligibleCategory := none) : String :=
  match kind with
  | .staleProof           => "stale_proof"
  | .missingProof         => "missing_proof"
  | .unsupportedConstruct => "unsupported_construct"
  | .trusted              => "trusted_boundary"
  | .attachmentIntegrity  => "attachment_integrity"
  | .theoremLookup        => "theorem_lookup"
  | .leanCheckFailure     => "lean_check_failure"
  | .ineligible           => (ineligCat.getD .structuralGate).failureClass

/-- Repair class — what action resolves this failure. -/
def repairClassOf (kind : ProofDiagnosticKind)
    (ineligCat : Option IneligibleCategory := none) : String :=
  match kind with
  | .staleProof           => "theorem_update"
  | .missingProof         => "add_proof"
  | .unsupportedConstruct => "code_rewrite"
  | .trusted              => "none"
  | .attachmentIntegrity  => "registry_update"
  | .theoremLookup        => "add_proof"
  | .leanCheckFailure     => "theorem_update"
  | .ineligible           => (ineligCat.getD .structuralGate).repairClass

/-- A proof-pipeline diagnostic — the canonical format for proof failures,
    warnings, and informational messages. Generated in ProofCore,
    consumed read-only by Report.lean renderers. -/
structure ProofDiagnostic where
  kind         : ProofDiagnosticKind
  severity     : ProofDiagnosticSeverity
  function     : String         -- qualified name
  message      : String         -- one-line summary
  hint         : String         -- actionable suggestion (empty if none)
  details      : List String    -- unsupported constructs, profile gates, etc.
  failureClass : String         -- fine-grained failure category
  repairClass  : String         -- what action resolves this
  fingerprint  : String         -- current body fingerprint
  expectedFp   : String         -- expected fingerprint (empty if none)
  loc          : Option SourceLoc

-- ============================================================
-- ProofCore artifact
-- ============================================================

/-- A function that passed eligibility and was extracted (or attempted). -/
structure ProofCoreEntry where
  qualName    : String
  bareName    : String
  fn          : CFnDef
  extracted   : Option Proof.PExpr
  unsupported : List String
  fingerprint : String
  params      : List String
  eligibility : EligibilityEntry
  loc         : Option SourceLoc
  spec        : Option SpecAttachment

/-- A function excluded from ProofCore with reasons. -/
structure ProofCoreExcluded where
  qualName    : String
  bareName    : String
  fn          : CFnDef
  fingerprint : String
  eligibility : EligibilityEntry
  loc         : Option SourceLoc
  spec        : Option SpecAttachment

/-- The proof-oriented fragment of validated Core.
    This is the single artifact boundary between Core and the proof pipeline. -/
structure ProofCore where
  /-- Eligible functions with extraction results. -/
  entries     : List ProofCoreEntry
  /-- Excluded functions with reasons. -/
  excluded    : List ProofCoreExcluded
  /-- Proof-eligible structs. -/
  structs     : List CStructDef
  /-- Proof-eligible enums. -/
  enums       : List CEnumDef
  /-- Trait definitions (for context). -/
  traitDefs   : List CTraitDef
  /-- Precomputed call graph. -/
  callGraph   : CallGraph
  /-- Precomputed recursion classification. -/
  recMap      : List (String × RecursionKind × List String)
  /-- Extern function names. -/
  externNames : List String
  /-- Proof obligations generated from the proof pipeline. -/
  obligations : List Obligation := []
  /-- Proof diagnostics generated from the proof pipeline. -/
  diagnostics : List ProofDiagnostic := []

-- ============================================================
-- Registry validation
-- ============================================================

/-- A registry validation issue. -/
inductive RegistryIssue where
  | unknownFunction (entry : ProofRegistryEntry)
  | renamedFunction (entry : ProofRegistryEntry) (newName : String)
  | duplicateEntry (function : String) (count : Nat)
  | conflictingEntry (function : String) (specs : List String)
  | staleFingerprint (entry : ProofRegistryEntry) (currentFp : String)
  | ineligibleFunction (entry : ProofRegistryEntry) (reasons : List String)
  | emptyProofName (entry : ProofRegistryEntry)
  | emptySpecName (entry : ProofRegistryEntry)
  | extractionBlocked (entry : ProofRegistryEntry) (unsupported : List String)
  deriving Repr

/-- Registry issues that are errors (attachment integrity violations)
    vs warnings (informational). -/
def RegistryIssue.isError : RegistryIssue → Bool
  | .unknownFunction _ => true
  | .renamedFunction _ _ => true
  | .duplicateEntry _ _ => true
  | .conflictingEntry _ _ => true
  | .staleFingerprint _ _ => false  -- stale is a warning, not an error
  | .ineligibleFunction _ _ => true
  | .emptyProofName _ => true
  | .emptySpecName _ => true
  | .extractionBlocked _ _ => true

/-- Validate a proof registry against a ProofCore artifact. -/
def validateRegistry (pc : ProofCore) (registry : ProofRegistry) : List RegistryIssue :=
  let allFns := pc.entries.map (·.qualName) ++ pc.excluded.map (·.qualName)
  let entryFps : List (String × String) := pc.entries.map fun e => (e.qualName, e.fingerprint)
  let exclFps : List (String × String) := pc.excluded.map fun e => (e.qualName, e.fingerprint)
  let allFps := entryFps ++ exclFps
  -- Check for unknown functions (with rename detection via fingerprint matching)
  let unknowns := registry.filterMap fun re =>
    if allFns.contains re.function then none
    else
      -- Fingerprint-based rename detection: if a current function has the same
      -- fingerprint as the orphaned registry entry, it was likely renamed.
      match allFps.find? fun (_, fp) => fp == re.bodyFingerprint with
      | some (newName, _) => some (.renamedFunction re newName)
      | none => some (.unknownFunction re)
  -- Check for duplicates
  let grouped := registry.foldl (fun acc re =>
    match acc.find? fun (f, _) => f == re.function with
    | some (f, _n) => acc.map fun (g, m) => if g == f then (g, m + 1) else (g, m)
    | none => acc ++ [(re.function, 1)]) ([] : List (String × Nat))
  let duplicates := grouped.filterMap fun (f, n) =>
    if n > 1 then some (.duplicateEntry f n) else none
  -- Check for conflicting specs (same function, different spec names)
  let conflicts := grouped.filterMap fun (f, n) =>
    if n <= 1 then none
    else
      let specs := (registry.filter fun re => re.function == f).map (·.spec) |>.eraseDups
      if specs.length > 1 then some (.conflictingEntry f specs) else none
  -- Check for stale fingerprints
  let stales := registry.filterMap fun re =>
    match allFps.find? fun (f, _) => f == re.function with
    | some (_, currentFp) =>
      if re.bodyFingerprint != currentFp then some (.staleFingerprint re currentFp)
      else none
    | none => none  -- already caught as unknown
  -- Check for entries targeting ineligible functions
  let ineligibles := registry.filterMap fun re =>
    match pc.excluded.find? fun e => e.qualName == re.function with
    | some ex =>
      let reasons := ex.eligibility.sourceReasons ++ ex.eligibility.profileReasons
      some (.ineligibleFunction re reasons)
    | none => none
  -- Check for entries targeting extraction-blocked functions
  let blocked := registry.filterMap fun re =>
    match pc.entries.find? fun e => e.qualName == re.function with
    | some entry =>
      if entry.extracted.isNone && !entry.unsupported.isEmpty then
        some (.extractionBlocked re entry.unsupported)
      else none
    | none => none
  -- Check for empty proof/spec names
  let emptyProofs := registry.filterMap fun re =>
    if re.proof.isEmpty then some (.emptyProofName re) else none
  let emptySpecs := registry.filterMap fun re =>
    if re.spec.isEmpty then some (.emptySpecName re) else none
  unknowns ++ duplicates ++ conflicts ++ stales ++ ineligibles ++ blocked ++ emptyProofs ++ emptySpecs

/-- Render a registry validation issue as a diagnostic string. -/
def renderRegistryIssue : RegistryIssue → String
  | .unknownFunction re =>
    s!"error: registry entry for unknown function '{re.function}' (function was removed or renamed — update or remove the registry entry)"
  | .renamedFunction re newName =>
    s!"error: registry entry for '{re.function}' appears renamed to '{newName}' (same fingerprint) — update the registry entry's function field to '{newName}'"
  | .duplicateEntry fn n =>
    s!"error: {n} duplicate registry entries for '{fn}'"
  | .conflictingEntry fn specs =>
    s!"error: conflicting specs for '{fn}': {", ".intercalate specs}"
  | .staleFingerprint re currentFp =>
    s!"warning: stale fingerprint for '{re.function}' (registry: {re.bodyFingerprint.take 40}…, current: {currentFp.take 40}…)"
  | .ineligibleFunction re reasons =>
    s!"error: registry entry for ineligible function '{re.function}' ({", ".intercalate reasons})"
  | .emptyProofName re =>
    s!"error: registry entry for '{re.function}' has empty proof name"
  | .emptySpecName re =>
    s!"error: registry entry for '{re.function}' has empty spec name"
  | .extractionBlocked re unsupported =>
    s!"error: registry entry for '{re.function}' targets extraction-blocked function (unsupported: {", ".intercalate unsupported})"

/-- Convert registry validation issues into proof diagnostics with
    the attachment_integrity failure class. The `locMap` lets us populate
    `loc` from the target function's source position when available. -/
def registryIssuesToDiagnostics (issues : List RegistryIssue)
    (locMap : List (String × SourceLoc) := []) : List ProofDiagnostic :=
  issues.filterMap fun issue =>
    let sev := if issue.isError then ProofDiagnosticSeverity.error else .warning
    let det : List String := match issue with
      | .unknownFunction _ => ["unknown function"]
      | .renamedFunction _ newName => [s!"renamed to {newName}"]
      | .duplicateEntry _ n => [s!"{n} duplicate entries"]
      | .conflictingEntry _ specs => specs
      | .staleFingerprint _ _ => ["stale fingerprint"]
      | .ineligibleFunction _ reasons => reasons
      | .emptyProofName _ => ["empty proof name"]
      | .emptySpecName _ => ["empty spec name"]
      | .extractionBlocked _ unsupported => unsupported
    let fn := match issue with
      | .unknownFunction re | .renamedFunction re _ | .staleFingerprint re _
      | .ineligibleFunction re _ | .emptyProofName re | .emptySpecName re
      | .extractionBlocked re _ => re.function
      | .duplicateEntry f _ | .conflictingEntry f _ => f
    let loc := (locMap.find? fun e => e.1 == fn).map (·.2)
    some { kind := .attachmentIntegrity, severity := sev, function := fn
         , message := renderRegistryIssue issue
         , hint := match issue with
           | .unknownFunction _ => "Remove the registry entry or update the function name."
           | .renamedFunction _ newName => s!"Update the registry entry's function field to '{newName}'."
           | .duplicateEntry _ _ => "Remove duplicate registry entries."
           | .conflictingEntry _ _ => "Ensure each function has exactly one spec."
           | .staleFingerprint _ _ => "Update the registry fingerprint to match the current body."
           | .ineligibleFunction _ _ => "Remove the registry entry or make the function eligible."
           | .emptyProofName _ => "Add a proof name to the registry entry."
           | .emptySpecName _ => "Add a spec name to the registry entry."
           | .extractionBlocked _ _ => "Remove the registry entry or fix unsupported constructs."
         , details := det
         , failureClass := failureClassOf .attachmentIntegrity
         , repairClass := repairClassOf .attachmentIntegrity
         , fingerprint := match issue with
           | .staleFingerprint _ currentFp => currentFp
           | _ => ""
         , expectedFp := match issue with
           | .staleFingerprint re _ => re.bodyFingerprint
           | _ => ""
         , loc }

/-- Convert check-proofs results into proof diagnostics. Each failed
    theorem produces either a theorem_lookup or lean_check_failure diagnostic. -/
def checkProofResultsToDiagnostics
    (failures : List (String × String × Bool))  -- (function, proofName, isLookupFailure)
    : List ProofDiagnostic :=
  failures.map fun (fn, proofName, isLookup) =>
    let kind := if isLookup then ProofDiagnosticKind.theoremLookup else .leanCheckFailure
    let det := [proofName]
    { kind, severity := if isLookup then .warning else .error, function := fn
    , message := if isLookup
        then s!"Lean theorem '{proofName}' not found for `{fn}`."
        else s!"Lean kernel rejected proof '{proofName}' for `{fn}`."
    , hint := if isLookup
        then s!"Ensure '{proofName}' is defined in Concrete/Proof.lean and imported."
        else s!"Fix the Lean proof '{proofName}' so it type-checks."
    , details := det, failureClass := failureClassOf kind
    , repairClass := repairClassOf kind
    , fingerprint := "", expectedFp := "", loc := none }

-- ============================================================
-- Extraction: Core modules → ProofCore
-- ============================================================

/-- Flatten a module tree into a list of all modules (pre-order). -/
private partial def flattenModules (m : CModule) : List CModule :=
  m :: List.flatten (m.submodules.map flattenModules)

/-- Assess eligibility for one function. Combines source-level checks
    (capabilities, trusted, entry point) with profile gates (recursion,
    loops, allocation, FFI, blocking I/O). -/
private def assessEligibility
    (f : CFnDef) (qualName : String)
    (externNames : List String)
    (recMap : List (String × RecursionKind × List String))
    (locMap : List (String × SourceLoc)) : EligibilityEntry :=
  let fnLoc := match locMap.find? fun (n, _) => n == qualName with
    | some (_, loc) => some loc
    | none => none
  let (concreteCaps, _) := f.capSet.normalize
  let callees := collectCallsStmts f.body |>.eraseDups
  let sourceReasons : List String :=
    (if !f.capSet.isEmpty then
      [s!"has capabilities: {", ".intercalate concreteCaps}"] else []) ++
    (if f.isTrusted then ["marked trusted"] else []) ++
    (if f.isEntryPoint then ["is entry point (main)"] else []) ++
    (if f.trustedImplOrigin.isSome then ["from trusted impl"] else [])
  let allocs := callees.filter isAllocCall
  let rec_ := match recMap.find? (fun (n, _, _) => n == qualName) with
    | some (_, .direct, _) => "direct"
    | some (_, .mutual, _) => "mutual"
    | some (_, .none, _) => "none"
    | none => "unclassified"  -- function missing from SCC analysis
  let crossesFfi := callees.any fun c => externNames.contains c
  let loopClass := classifyLoops f.body
  let profileReasons : List String :=
    (if rec_ != "none" && rec_ != "unclassified" then [s!"recursion ({rec_})"] else []) ++
    (if loopClass == "unbounded" || loopClass == "mixed" then ["unbounded loops"] else []) ++
    (if !allocs.isEmpty || concreteCaps.any (· == "Alloc") then ["allocation"] else []) ++
    (if crossesFfi then ["FFI"] else []) ++
    (if concreteCaps.any fun c => c == "File" || c == "Network" || c == "Process"
     then ["blocking I/O"] else [])
  let passesSource := sourceReasons.isEmpty
  let passesProfile := profileReasons.isEmpty
  let eligible := passesSource && passesProfile
  let exclusionKind := if eligible then none
    else if !passesSource && !passesProfile then some .both
    else if !passesSource then some .source
    else some .profile
  { qualName, eligible, sourceReasons, profileReasons, exclusionKind
  , isTrusted := f.isTrusted, loc := fnLoc }

/-- Walk a module tree collecting eligibility + extraction for each function.
    This produces one ProofCoreEntry or ProofCoreExcluded per function. -/
private partial def extractModule
    (externNames : List String)
    (recMap : List (String × RecursionKind × List String))
    (locMap : List (String × SourceLoc))
    (registry : ProofRegistry)
    (m : CModule) (modulePath : String := "")
    : List ProofCoreEntry × List ProofCoreExcluded :=
  let qualPrefix := if modulePath == "" then m.name else modulePath ++ "." ++ m.name
  let (entries, excluded) := m.functions.foldl (fun (accE, accX) f =>
    let qualName := qualPrefix ++ "." ++ f.name
    let bareName := f.name
    let fp := bodyFingerprint f.body
    let elig := assessEligibility f qualName externNames recMap locMap
    let sa := resolveSpec qualName registry
    if elig.isTrusted then
      (accE, accX ++ [{ qualName, bareName, fn := f, fingerprint := fp
                       , eligibility := elig, loc := elig.loc
                       , spec := sa : ProofCoreExcluded }])
    else if elig.eligible then
      let extracted := cStmtsToPExpr f.body |>.map normalizePExpr
      let unsup := if extracted.isNone then identifyUnsupported f.body else []
      (accE ++ [{ qualName, bareName, fn := f, extracted, unsupported := unsup
                 , fingerprint := fp, params := f.params.map Prod.fst
                 , eligibility := elig, loc := elig.loc
                 , spec := sa : ProofCoreEntry }], accX)
    else
      (accE, accX ++ [{ qualName, bareName, fn := f, fingerprint := fp
                       , eligibility := elig, loc := elig.loc
                       , spec := sa : ProofCoreExcluded }])
  ) ([], [])
  -- Recurse into submodules
  let (subEntries, subExcluded) := m.submodules.foldl (fun (accE, accX) sub =>
    let (e, x) := extractModule externNames recMap locMap registry sub qualPrefix
    (accE ++ e, accX ++ x)) ([], [])
  (entries ++ subEntries, excluded ++ subExcluded)

/-- Derive obligation status from eligibility, trust, extraction, and spec attachment. -/
private def deriveObligationStatus
    (eligible : Bool) (isTrusted : Bool) (extracted : Bool)
    (spec : Option SpecAttachment) (currentFp : String) : ObligationStatus :=
  if isTrusted then .trusted
  else if !eligible then
    match spec with
    | some a => if a.expectedFp != currentFp then .stale else .ineligible
    | none => .ineligible
  else match spec with
  | some a =>
    if a.expectedFp != currentFp then .stale
    else if a.source == .hardcoded then .proved  -- hardcoded proofs done in Lean, extraction not required
    else if !extracted then .blocked
    else .proved
  | none =>
    if !extracted then .blocked
    else .missing

/-- Generate proof obligations from extracted entries and excluded functions.
    Uses the call graph to compute proved-callee dependencies. -/
private def generateObligations
    (entries : List ProofCoreEntry)
    (excluded : List ProofCoreExcluded)
    (graph : CallGraph) : List Obligation :=
  -- Build obligations for extracted (eligible) entries
  let entryObls := entries.map fun e =>
    let extracted := e.extracted.isSome
    let status := deriveObligationStatus e.eligibility.eligible
        e.eligibility.isTrusted extracted e.spec e.fingerprint
    let cat := if status == .ineligible
      then some (classifyIneligible e.eligibility.sourceReasons e.eligibility.profileReasons)
      else none
    { functionId := { qualName := e.qualName, fingerprint := e.fingerprint }
    , bareName := e.bareName
    , status
    , spec := e.spec
    , expectedFp := match e.spec with | some a => a.expectedFp | none => ""
    , profileGates := e.eligibility.sourceReasons ++ e.eligibility.profileReasons
    , ineligCat := cat
    , dependencies := []  -- filled in second pass
    , staleDeps := []
    , loc := e.loc : Obligation }
  -- Build obligations for excluded entries (never extracted)
  let exclObls := excluded.map fun e =>
    let status := deriveObligationStatus e.eligibility.eligible
        e.eligibility.isTrusted false e.spec e.fingerprint
    let cat := if status == .ineligible
      then some (classifyIneligible e.eligibility.sourceReasons e.eligibility.profileReasons)
      else none
    { functionId := { qualName := e.qualName, fingerprint := e.fingerprint }
    , bareName := e.bareName
    , status
    , spec := e.spec
    , expectedFp := match e.spec with | some a => a.expectedFp | none => ""
    , profileGates := e.eligibility.sourceReasons ++ e.eligibility.profileReasons
    , ineligCat := cat
    , dependencies := []
    , staleDeps := []
    , loc := e.loc : Obligation }
  let allObls := entryObls ++ exclObls
  -- Second pass: fill in dependencies (proved callees) and stale dependencies
  let provedNames := allObls.filterMap fun o =>
    if o.status == .proved then some o.functionId.qualName else none
  let staleNames := allObls.filterMap fun o =>
    if o.status == .stale then some o.functionId.qualName else none
  allObls.map fun o =>
    let allCallees := match graph.find? fun (n, _) => n == o.functionId.qualName with
      | some (_, cs) => cs
      | none => []
    let provedCallees := allCallees.filter fun c => provedNames.contains c
    let staleCallees := allCallees.filter fun c => staleNames.contains c
    { o with dependencies := provedCallees, staleDeps := staleCallees }

/-- Generate proof diagnostics from obligations and extraction results. -/
private def generateDiagnostics
    (obligations : List Obligation)
    (entries : List ProofCoreEntry) : List ProofDiagnostic :=
  -- Diagnostics from obligation status
  let oblDiags := obligations.filterMap fun o =>
    let qn := o.functionId.qualName
    let fp := o.functionId.fingerprint
    match o.status with
    | .stale =>
      some { kind := .staleProof, severity := .error, function := qn
           , message := s!"`{qn}` has a registered proof, but the body changed."
           , hint := "Update the Lean proof to match the current body, or restore the proved implementation."
           , details := [], failureClass := failureClassOf .staleProof
           , repairClass := repairClassOf .staleProof
           , fingerprint := fp, expectedFp := o.expectedFp, loc := o.loc }
    | .missing =>
      some { kind := .missingProof, severity := .warning, function := qn
           , message := s!"`{qn}` passes the predictable profile but has no registered proof."
           , hint := "Add a Lean proof for this function with the current fingerprint."
           , details := [], failureClass := failureClassOf .missingProof
           , repairClass := repairClassOf .missingProof
           , fingerprint := fp, expectedFp := "", loc := o.loc }
    | .ineligible =>
      let det := o.profileGates
      some { kind := .ineligible, severity := .info, function := qn
           , message := s!"`{qn}` cannot be proved: fails predictable profile."
           , hint := if det.isEmpty then ""
               else s!"Address these constraints to make this function eligible: {", ".intercalate det}."
           , details := det, failureClass := failureClassOf .ineligible o.ineligCat
           , repairClass := repairClassOf .ineligible o.ineligCat
           , fingerprint := fp, expectedFp := "", loc := o.loc }
    | .blocked => none  -- handled by entry-level unsupported diagnostics with details
    | .trusted =>
      some { kind := .trusted, severity := .info, function := qn
           , message := s!"`{qn}` is marked trusted — proof is bypassed."
           , hint := "", details := [], failureClass := failureClassOf .trusted
           , repairClass := repairClassOf .trusted
           , fingerprint := fp, expectedFp := "", loc := o.loc }
    | .proved => none
  -- Diagnostics from unsupported constructs (eligible but extraction blocked)
  let unsupDiags := entries.filterMap fun e =>
    if e.extracted.isNone && !e.unsupported.isEmpty then
      some { kind := .unsupportedConstruct, severity := .error, function := e.qualName
           , message := s!"`{e.qualName}` is eligible but uses unsupported constructs."
           , hint := s!"Remove {", ".intercalate e.unsupported} to enable extraction."
           , details := e.unsupported, failureClass := failureClassOf .unsupportedConstruct
           , repairClass := repairClassOf .unsupportedConstruct
           , fingerprint := e.fingerprint, expectedFp := ""
           , loc := e.loc }
    else none
  oblDiags ++ unsupDiags

/-- Extract the proof-oriented fragment from validated Core.
    This is the primary entry point for the proof pipeline. -/
def extractProofCore (vc : ValidatedCore)
    (locMap : List (String × SourceLoc) := [])
    (registry : ProofRegistry := [])
    : ProofCore :=
  let modules := vc.coreModules
  let allModules := List.flatten (modules.map flattenModules)
  -- Precompute shared analysis
  let graph := buildCallGraph modules
  let sccs := tarjanSCC graph
  let recMap := classifyRecursion graph sccs
  let externNames := modules.foldl (fun acc m => acc ++ collectExternNames m) []
  -- Extract entries and excluded (with spec attachment)
  let (entries, excluded) := modules.foldl (fun (accE, accX) m =>
    let (e, x) := extractModule externNames recMap locMap registry m
    (accE ++ e, accX ++ x)) ([], [])
  -- Generate proof obligations and diagnostics
  let obligations := generateObligations entries excluded graph
  let oblDiags := generateDiagnostics obligations entries
  -- Generate attachment-integrity diagnostics from registry validation
  let regIssues := validateRegistry
    { entries, excluded, structs := [], enums := [], traitDefs := []
    , callGraph := graph, recMap, externNames, obligations := [], diagnostics := [] }
    registry
  let regDiags := registryIssuesToDiagnostics regIssues locMap
  let diagnostics := oblDiags ++ regDiags
  -- Collect eligible types
  let sts := List.flatten (allModules.map (·.structs)) |>.filter CStructDef.isProofEligible
  let ens := List.flatten (allModules.map (·.enums)) |>.filter CEnumDef.isProofEligible
  let tds := List.flatten (allModules.map (·.traitDefs))
  { entries, excluded, structs := sts, enums := ens, traitDefs := tds
  , callGraph := graph, recMap, externNames, obligations, diagnostics }

-- ============================================================
-- Pretty-printing (for --report proofcore)
-- ============================================================

def ProofCore.summary (pc : ProofCore) : String :=
  let eligibleNames := pc.entries.map (·.qualName)
  let excludedNames := pc.excluded.map (·.qualName)
  let extractedCount := (pc.entries.filter (·.extracted.isSome)).length
  s!"ProofCore fragment:\n" ++
  s!"  {pc.entries.length} eligible functions ({extractedCount} extracted to PExpr)\n" ++
  s!"  {pc.excluded.length} excluded functions\n" ++
  s!"  {pc.structs.length} proof-eligible structs\n" ++
  s!"  {pc.enums.length} proof-eligible enums\n" ++
  s!"  eligible:  {eligibleNames}\n" ++
  s!"  excluded:  {excludedNames}"

/-- Get all eligibility entries (both eligible and excluded). -/
def ProofCore.allEligibility (pc : ProofCore) : List EligibilityEntry :=
  pc.entries.map (·.eligibility) ++ pc.excluded.map (·.eligibility)

/-- Find a ProofCoreEntry by qualified name. -/
def ProofCore.findEntry (pc : ProofCore) (qualName : String) : Option ProofCoreEntry :=
  pc.entries.find? fun e => e.qualName == qualName

/-- Find an excluded entry by qualified name. -/
def ProofCore.findExcluded (pc : ProofCore) (qualName : String) : Option ProofCoreExcluded :=
  pc.excluded.find? fun e => e.qualName == qualName

-- ============================================================
-- Self-consistency checks
-- ============================================================

/-- A consistency violation found by self-check. -/
structure ConsistencyViolation where
  invariant : String   -- short invariant name (e.g., "INV-1a")
  function  : String   -- affected function (or "" for global)
  message   : String   -- human-readable description
  deriving Repr

/-- Verify internal consistency of a ProofCore artifact.
    Returns an empty list if all invariants hold. -/
def ProofCore.selfCheck (pc : ProofCore) : List ConsistencyViolation :=
  let allNames := (pc.entries.map (·.qualName)) ++ (pc.excluded.map (·.qualName))
  let provedNames := pc.obligations.filterMap fun o =>
    if o.status == .proved then some o.functionId.qualName else none

  -- INV-1: Every obligation references a known function
  let oblKnown := pc.obligations.filterMap fun o =>
    let qn := o.functionId.qualName
    if !allNames.contains qn then
      some { invariant := "OBL-KNOWN", function := qn
           , message := s!"obligation references unknown function '{qn}'" }
    else none

  -- INV-2: Obligation status agrees with re-derivation
  let oblStatus := pc.obligations.filterMap fun o =>
    let qn := o.functionId.qualName
    match pc.findEntry qn with
    | some e =>
      let expected := deriveObligationStatus e.eligibility.eligible
          e.eligibility.isTrusted e.extracted.isSome e.spec e.fingerprint
      if o.status != expected then
        some { invariant := "OBL-STATUS", function := qn
             , message := s!"obligation status '{repr o.status}' disagrees with re-derived '{repr expected}'" }
      else none
    | none =>
      match pc.findExcluded qn with
      | some x =>
        let expected := deriveObligationStatus x.eligibility.eligible
            x.eligibility.isTrusted false x.spec x.fingerprint
        if o.status != expected then
          some { invariant := "OBL-STATUS", function := qn
               , message := s!"obligation status '{repr o.status}' disagrees with re-derived '{repr expected}'" }
        else none
      | none => none  -- caught by OBL-KNOWN

  -- INV-3: Proved status requires extraction (unless proof source is hardcoded)
  let provedExtracted := pc.obligations.filterMap fun o =>
    if o.status != .proved then none
    else if o.spec.any (·.source == .hardcoded) then none  -- hardcoded proofs bypass extraction
    else match pc.findEntry o.functionId.qualName with
    | some e =>
      if e.extracted.isNone then
        some { invariant := "PROVED-EXTRACTED", function := o.functionId.qualName
             , message := "obligation is 'proved' but extraction is None" }
      else none
    | none =>
      some { invariant := "PROVED-ENTRY", function := o.functionId.qualName
           , message := "obligation is 'proved' but function is not in entries" }

  -- INV-4: Proved status requires matching fingerprint
  let provedFp := pc.obligations.filterMap fun o =>
    if o.status != .proved then none
    else match o.spec with
    | some a =>
      if a.expectedFp != o.functionId.fingerprint then
        some { invariant := "PROVED-FP", function := o.functionId.qualName
             , message := s!"obligation is 'proved' but fingerprints disagree" }
      else none
    | none =>
      some { invariant := "PROVED-SPEC", function := o.functionId.qualName
           , message := "obligation is 'proved' but has no spec attachment" }

  -- INV-5: Stale status requires spec with different fingerprint
  let staleFp := pc.obligations.filterMap fun o =>
    if o.status != .stale then none
    else match o.spec with
    | some a =>
      if a.expectedFp == o.functionId.fingerprint then
        some { invariant := "STALE-FP", function := o.functionId.qualName
             , message := "obligation is 'stale' but fingerprints match" }
      else none
    | none =>
      some { invariant := "STALE-SPEC", function := o.functionId.qualName
           , message := "obligation is 'stale' but has no spec attachment" }

  -- INV-6: Entry fingerprint matches obligation fingerprint
  let entryFp := pc.entries.filterMap fun e =>
    match pc.obligations.find? fun o => o.functionId.qualName == e.qualName with
    | some o =>
      if o.functionId.fingerprint != e.fingerprint then
        some { invariant := "ENTRY-FP", function := e.qualName
             , message := s!"entry fingerprint disagrees with obligation fingerprint" }
      else none
    | none => none

  -- INV-7: Entries with extracted=Some must have empty unsupported list
  let extractUnsup := pc.entries.filterMap fun e =>
    if e.extracted.isSome && !e.unsupported.isEmpty then
      some { invariant := "EXTRACT-UNSUP", function := e.qualName
           , message := "entry has extracted PExpr but also has unsupported constructs" }
    else none

  -- INV-8: Entries with extracted=None and eligible=true must have non-empty unsupported
  let blockedUnsup := pc.entries.filterMap fun e =>
    if e.extracted.isNone && e.eligibility.eligible && e.unsupported.isEmpty then
      some { invariant := "BLOCKED-UNSUP", function := e.qualName
           , message := "entry is eligible with no extraction but unsupported list is empty" }
    else none

  -- INV-9: Dependencies only reference proved obligations
  let depProved := pc.obligations.foldl (fun acc o =>
    acc ++ o.dependencies.filterMap fun dep =>
      if !provedNames.contains dep then
        some { invariant := "DEP-PROVED", function := o.functionId.qualName
             , message := s!"dependency '{dep}' is not proved" }
      else none) []

  -- INV-14: staleDeps only reference stale obligations
  let staleOblNames := (pc.obligations.filter fun o => o.status == .stale).map (·.functionId.qualName)
  let depStale := pc.obligations.foldl (fun acc o =>
    acc ++ o.staleDeps.filterMap fun dep =>
      if !staleOblNames.contains dep then
        some { invariant := "DEP-STALE", function := o.functionId.qualName
             , message := s!"stale dependency '{dep}' is not actually stale" }
      else none) []

  -- INV-10: No duplicate function names across entries and excluded
  let allPcNames := pc.entries.map (·.qualName) ++ pc.excluded.map (·.qualName)
  let dups := allPcNames.foldl (fun (seen, acc) name =>
    if seen.contains name then
      (seen, acc ++ [{ invariant := "DUP-NAME", function := name
                     , message := "duplicate function in ProofCore" }])
    else (name :: seen, acc)) ([], [])

  -- INV-11: Diagnostic kinds agree with obligation status
  let diagStatus := pc.diagnostics.filterMap fun d =>
    match pc.obligations.find? fun o => o.functionId.qualName == d.function with
    | some o =>
      let statusOk := match d.kind with
        | .staleProof => o.status == .stale
        | .missingProof => o.status == .missing
        | .ineligible => o.status == .ineligible
        | .trusted => o.status == .trusted
        | .unsupportedConstruct => o.status == .blocked || o.status == .stale
            || (o.status == .proved && o.spec.any (·.source == .hardcoded))
        | .attachmentIntegrity => true  -- registry issues may target any obligation status
        | .theoremLookup => true        -- generated from check-proofs, status may be proved/stale
        | .leanCheckFailure => true     -- generated from check-proofs, status may be proved/stale
      if !statusOk then
        some { invariant := "DIAG-STATUS", function := d.function
             , message := s!"diagnostic kind '{repr d.kind}' disagrees with obligation status '{repr o.status}'" }
      else none
    | none =>
      -- Registry/check-proofs diagnostics may reference functions without obligations
      if d.kind != .unsupportedConstruct && d.kind != .attachmentIntegrity
         && d.kind != .theoremLookup && d.kind != .leanCheckFailure then
        some { invariant := "DIAG-OBL", function := d.function
             , message := "diagnostic references function with no obligation" }
      else none

  -- INV-12: Every entry must have a corresponding obligation (no dropped obligations)
  let entryObl := pc.entries.filterMap fun e =>
    match pc.obligations.find? fun o => o.functionId.qualName == e.qualName with
    | some _ => none
    | none =>
      some { invariant := "ENTRY-OBL", function := e.qualName
           , message := "entry has no corresponding obligation — obligation generation may have dropped this function" }

  -- INV-13: Every excluded function must have a corresponding obligation
  let excludedObl := pc.excluded.filterMap fun x =>
    match pc.obligations.find? fun o => o.functionId.qualName == x.qualName with
    | some _ => none
    | none =>
      some { invariant := "EXCL-OBL", function := x.qualName
           , message := "excluded function has no corresponding obligation — obligation generation may have dropped this function" }

  oblKnown ++ oblStatus ++ provedExtracted ++ provedFp ++ staleFp
    ++ entryFp ++ extractUnsup ++ blockedUnsup ++ depProved ++ depStale
    ++ dups.2 ++ diagStatus ++ entryObl ++ excludedObl

/-- Format consistency violations as a human-readable report. -/
def ConsistencyViolation.render (vs : List ConsistencyViolation) : String :=
  if vs.isEmpty then "All consistency checks passed."
  else
    let header := s!"Found {vs.length} consistency violation(s):\n"
    let body := vs.map fun v =>
      s!"  [{v.invariant}] {v.function}: {v.message}"
    header ++ "\n".intercalate body

/-- Filter this ProofCore to only include functions from user/package modules.
    Dependency functions (whose qualName starts with a depName) are excluded. -/
def ProofCore.scopeToUser (pc : ProofCore) (depNames : List String) : ProofCore :=
  let isUser (qn : String) : Bool :=
    let topModule := match qn.splitOn "." with | m :: _ => m | [] => qn
    !depNames.contains topModule
  { pc with
    entries     := pc.entries.filter     fun e => isUser e.qualName
    excluded    := pc.excluded.filter    fun e => isUser e.qualName
    obligations := pc.obligations.filter fun o => isUser o.functionId.qualName
    diagnostics := pc.diagnostics.filter fun d => isUser d.function
  }

end Concrete
