import Concrete.Elab.Core
import Concrete.Pipeline.Pipeline
import Concrete.Proof.Proof
import Concrete.Resolve.Intrinsic
import Concrete.Proof.Sha256Spec
import Concrete.Proof.BodyIdentity
import Concrete.Proof.ImplementationIdentity
import Concrete.Proof.DefinitionIdentity
import Concrete.Proof.TableResolve
import Concrete.Proof.Correspondence
import Concrete.Proof.DependencyRoot
import Concrete.Proof.ClassificationTable

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
  -- Only a direct callee names a definition. An indirect callee is a fn-typed
  -- binding, so its statically-known target set is empty — which is also why
  -- it contributes no dependency edge. Extraction keeps it as `PExpr.applyVar`
  -- rather than refusing the body (which cost three real std proofs); the
  -- identity is the binding, not a global name.
  | .call callee _ args _ =>
    callee.directName?.toList ++ args.foldl (fun acc a => acc ++ collectCallsExpr a) []
  | .binOp _ l r _ => collectCallsExpr l ++ collectCallsExpr r
  | .unaryOp _ e _ => collectCallsExpr e
  | .structLit _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectCallsExpr v) []
  | .fieldAccess obj _ _ => collectCallsExpr obj
  | .enumLit _ _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectCallsExpr v) []
  | .match_ scrut arms _ => collectCallsExpr scrut ++ arms.foldl (fun acc a => acc ++ collectCallsArm a) []
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => collectCallsExpr inner
  | .arrayLit elems _ => elems.foldl (fun acc e => acc ++ collectCallsExpr e) []
  | .arrayIndex arr idx _ => collectCallsExpr arr ++ collectCallsExpr idx
  | .cast inner _ => collectCallsExpr inner
  | .allocCall inner alloc _ => collectCallsExpr inner ++ collectCallsExpr alloc
  | .ifExpr cond th el _ =>
    collectCallsExpr cond ++ collectCallsStmts th ++ collectCallsStmts el
  | _ => []

partial def collectCallsArm (arm : CMatchArm) : List String :=
  match arm with
  | .enumArm _ _ _ guard body => (guard.map collectCallsExpr).getD [] ++ collectCallsStmts body
  | .litArm v guard body => collectCallsExpr v ++ (guard.map collectCallsExpr).getD [] ++ collectCallsStmts body
  | .varArm _ _ guard body => (guard.map collectCallsExpr).getD [] ++ collectCallsStmts body
  | .rangeArm lo hi _ guard body => collectCallsExpr lo ++ collectCallsExpr hi ++ (guard.map collectCallsExpr).getD [] ++ collectCallsStmts body

partial def collectCallsStmt (s : CStmt) : List String :=
  match s with
  | .letDecl _ _ _ v => collectCallsExpr v
  | .assign _ v => collectCallsExpr v
  | .return_ (some v) _ => collectCallsExpr v
  | .return_ none _ => []
  | .expr e _ => collectCallsExpr e
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

/-! ### Function values

A function taken as a VALUE (`write_fn: console_write`) may be called later through that
value, by code that no direct-call edge connects to it. `collectCalls*` above deliberately
omits such references — extraction and proof dependency edges must not treat a mention as a
call. Reachability for REPORTING foreign assumptions (R-0484 R10) needs them: a
`Writer<Console>` reaches libc `write` only because its constructor stored `console_write`.
Counting a reference as a possible call is a sound over-approximation for that purpose,
and it is used only there. -/
mutual
partial def collectFnValueRefsExpr (e : CExpr) : List String :=
  match e with
  | .fnRef name _ => [name]
  | .call _ _ args _ => args.foldl (fun acc a => acc ++ collectFnValueRefsExpr a) []
  | .binOp _ l r _ => collectFnValueRefsExpr l ++ collectFnValueRefsExpr r
  | .unaryOp _ e _ => collectFnValueRefsExpr e
  | .structLit _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectFnValueRefsExpr v) []
  | .fieldAccess obj _ _ => collectFnValueRefsExpr obj
  | .enumLit _ _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectFnValueRefsExpr v) []
  | .match_ scrut arms _ =>
    collectFnValueRefsExpr scrut ++ arms.foldl (fun acc a => acc ++ collectFnValueRefsArm a) []
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => collectFnValueRefsExpr inner
  | .arrayLit elems _ => elems.foldl (fun acc e => acc ++ collectFnValueRefsExpr e) []
  | .arrayIndex arr idx _ => collectFnValueRefsExpr arr ++ collectFnValueRefsExpr idx
  | .cast inner _ => collectFnValueRefsExpr inner
  | .allocCall inner alloc _ => collectFnValueRefsExpr inner ++ collectFnValueRefsExpr alloc
  | .ifExpr cond th el _ =>
    collectFnValueRefsExpr cond ++ collectFnValueRefsStmts th ++ collectFnValueRefsStmts el
  | _ => []

partial def collectFnValueRefsArm (arm : CMatchArm) : List String :=
  match arm with
  | .enumArm _ _ _ guard body => (guard.map collectFnValueRefsExpr).getD [] ++ collectFnValueRefsStmts body
  | .litArm v guard body =>
    collectFnValueRefsExpr v ++ (guard.map collectFnValueRefsExpr).getD [] ++ collectFnValueRefsStmts body
  | .varArm _ _ guard body => (guard.map collectFnValueRefsExpr).getD [] ++ collectFnValueRefsStmts body
  | .rangeArm lo hi _ guard body =>
    collectFnValueRefsExpr lo ++ collectFnValueRefsExpr hi ++ (guard.map collectFnValueRefsExpr).getD []
      ++ collectFnValueRefsStmts body

partial def collectFnValueRefsStmt (s : CStmt) : List String :=
  match s with
  | .letDecl _ _ _ v => collectFnValueRefsExpr v
  | .assign _ v => collectFnValueRefsExpr v
  | .return_ (some v) _ => collectFnValueRefsExpr v
  | .return_ none _ => []
  | .expr e _ => collectFnValueRefsExpr e
  | .ifElse c t el =>
    collectFnValueRefsExpr c ++ collectFnValueRefsStmts t ++
    match el with | some stmts => collectFnValueRefsStmts stmts | none => []
  | .while_ c body _ step =>
    collectFnValueRefsExpr c ++ collectFnValueRefsStmts body ++ collectFnValueRefsStmts step
  | .fieldAssign obj _ v => collectFnValueRefsExpr obj ++ collectFnValueRefsExpr v
  | .derefAssign t v => collectFnValueRefsExpr t ++ collectFnValueRefsExpr v
  | .arrayIndexAssign arr idx v =>
    collectFnValueRefsExpr arr ++ collectFnValueRefsExpr idx ++ collectFnValueRefsExpr v
  | .break_ (some v) _ => collectFnValueRefsExpr v
  | .break_ none _ | .continue_ _ => []
  | .defer body => collectFnValueRefsExpr body
  | .borrowIn _ _ _ _ _ body => collectFnValueRefsStmts body

partial def collectFnValueRefsStmts (ss : List CStmt) : List String :=
  ss.foldl (fun acc s => acc ++ collectFnValueRefsStmt s) []
end

/-! ### Indirect calls

`collectCalls*` above records only DIRECT callees, and says so: an indirect callee is a fn-typed
binding whose statically-known target set is empty, so it contributes no dependency edge. That
is the right call for extraction. It is the WRONG call for two guarantees that were quietly
built on the same call graph:

* **`no recursion`** — a cycle that passes through a function pointer has no edge, so SCC finds
  no cycle, so the function reports `recursion: none` and `--check predictable` admits it. A
  genuinely recursive program passes the no-recursion gate.
* **`--report stack-depth`** — with no edge, the deepest chain is one frame, so the report
  states a specific `Max stack bound` in bytes for a function that recurses to an arbitrary
  depth. A false NUMBER, not merely a missing warning.

Both are fixed by refusing to certify: a body containing an indirect call cannot be shown
acyclic here, so it is excluded rather than assumed acyclic. Resolving the target set (every
call site of a combinator passes a known function) is a whole-program analysis and a real
project; assuming it is empty is not a conservative approximation of it, it is the opposite. -/
mutual
partial def hasIndirectCallExpr (e : CExpr) : Bool :=
  match e with
  | .call callee _ args _ =>
    callee.directName?.isNone || args.any hasIndirectCallExpr
  | .binOp _ l r _ => hasIndirectCallExpr l || hasIndirectCallExpr r
  | .unaryOp _ e _ => hasIndirectCallExpr e
  | .structLit _ _ fields _ => fields.any (fun (_, v) => hasIndirectCallExpr v)
  | .fieldAccess obj _ _ => hasIndirectCallExpr obj
  | .enumLit _ _ _ fields _ => fields.any (fun (_, v) => hasIndirectCallExpr v)
  | .match_ scrut arms _ => hasIndirectCallExpr scrut || arms.any hasIndirectCallArm
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => hasIndirectCallExpr inner
  | .arrayLit elems _ => elems.any hasIndirectCallExpr
  | .arrayIndex arr idx _ => hasIndirectCallExpr arr || hasIndirectCallExpr idx
  | .cast inner _ => hasIndirectCallExpr inner
  | .allocCall inner alloc _ => hasIndirectCallExpr inner || hasIndirectCallExpr alloc
  | .ifExpr cond th el _ =>
    hasIndirectCallExpr cond || hasIndirectCallStmts th || hasIndirectCallStmts el
  | _ => false

partial def hasIndirectCallArm (arm : CMatchArm) : Bool :=
  match arm with
  | .enumArm _ _ _ guard body =>
    (guard.map hasIndirectCallExpr).getD false || hasIndirectCallStmts body
  | .litArm v guard body =>
    hasIndirectCallExpr v || (guard.map hasIndirectCallExpr).getD false || hasIndirectCallStmts body
  | .varArm _ _ guard body =>
    (guard.map hasIndirectCallExpr).getD false || hasIndirectCallStmts body
  | .rangeArm lo hi _ guard body =>
    hasIndirectCallExpr lo || hasIndirectCallExpr hi
      || (guard.map hasIndirectCallExpr).getD false || hasIndirectCallStmts body

partial def hasIndirectCallStmt (s : CStmt) : Bool :=
  match s with
  | .letDecl _ _ _ v => hasIndirectCallExpr v
  | .assign _ v => hasIndirectCallExpr v
  | .return_ (some v) _ => hasIndirectCallExpr v
  | .return_ none _ => false
  | .expr e _ => hasIndirectCallExpr e
  | .ifElse c t el =>
    hasIndirectCallExpr c || hasIndirectCallStmts t
      || (match el with | some ss => hasIndirectCallStmts ss | none => false)
  | .while_ cond body _ step =>
    hasIndirectCallExpr cond || hasIndirectCallStmts body || hasIndirectCallStmts step
  | .fieldAssign obj _ v => hasIndirectCallExpr obj || hasIndirectCallExpr v
  | .derefAssign t v => hasIndirectCallExpr t || hasIndirectCallExpr v
  | .arrayIndexAssign arr idx v =>
    hasIndirectCallExpr arr || hasIndirectCallExpr idx || hasIndirectCallExpr v
  | .break_ (some v) _ => hasIndirectCallExpr v
  | .break_ none _ | .continue_ _ => false
  | .defer body => hasIndirectCallExpr body
  | .borrowIn _ _ _ _ _ body => hasIndirectCallStmts body

partial def hasIndirectCallStmts (ss : List CStmt) : Bool :=
  ss.any hasIndirectCallStmt
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
  | .cast inner _ => collectDefersExpr inner
  | .allocCall inner alloc _ => collectDefersExpr inner ++ collectDefersExpr alloc
  | _ => []

partial def collectDefersArm (arm : CMatchArm) : List String :=
  match arm with
  | .enumArm _ _ _ guard body => (guard.map collectDefersExpr).getD [] ++ collectDefersStmts body
  | .litArm v guard body => collectDefersExpr v ++ (guard.map collectDefersExpr).getD [] ++ collectDefersStmts body
  | .varArm _ _ guard body => (guard.map collectDefersExpr).getD [] ++ collectDefersStmts body
  | .rangeArm lo hi _ guard body => collectDefersExpr lo ++ collectDefersExpr hi ++ (guard.map collectDefersExpr).getD [] ++ collectDefersStmts body

partial def collectDefersStmt (s : CStmt) : List String :=
  match s with
  | .defer body =>
    let desc := match body with
      | .call callee _ _ _ => s!"defer {callee.spelling}(...)"
      | _ => "defer <expr>"
    [desc] ++ collectDefersExpr body
  | .letDecl _ _ _ v => collectDefersExpr v
  | .assign _ v => collectDefersExpr v
  | .return_ (some v) _ => collectDefersExpr v
  | .return_ none _ => []
  | .expr e _ => collectDefersExpr e
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
  | .cast inner _ => hasRawPtrOpsExpr inner
  | .allocCall inner alloc _ => hasRawPtrOpsExpr inner || hasRawPtrOpsExpr alloc
  | _ => false

partial def hasRawPtrOpsArm (arm : CMatchArm) : Bool :=
  match arm with
  | .enumArm _ _ _ guard body => ((guard.map hasRawPtrOpsExpr).getD false) || hasRawPtrOpsStmts body
  | .litArm v guard body => hasRawPtrOpsExpr v || ((guard.map hasRawPtrOpsExpr).getD false) || hasRawPtrOpsStmts body
  | .varArm _ _ guard body => ((guard.map hasRawPtrOpsExpr).getD false) || hasRawPtrOpsStmts body
  | .rangeArm lo hi _ guard body => hasRawPtrOpsExpr lo || hasRawPtrOpsExpr hi || ((guard.map hasRawPtrOpsExpr).getD false) || hasRawPtrOpsStmts body

partial def hasRawPtrOpsStmt (s : CStmt) : Bool :=
  match s with
  | .derefAssign _ _ => true
  | .letDecl _ _ _ v => hasRawPtrOpsExpr v
  | .assign _ v => hasRawPtrOpsExpr v
  | .return_ (some v) _ => hasRawPtrOpsExpr v
  | .return_ none _ => false
  | .expr e _ => hasRawPtrOpsExpr e
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
  | .generic "Vec" _ _ => true
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

/-! ### Effect opacity (R-0484)

AN EMPTY CAPABILITY SET MEANS "NOTHING WAS DECLARED", NOT "NOTHING HAPPENS", and the two
were equated. `examples/base64_cli`'s `print_bytes` takes a `&Writer`, calls
`Writer::write`, performs real I/O, and was admitted to the provable subset ON THE GROUNDS
OF PURITY — while `usage`, which only prints a string, was excluded for honestly declaring
`Console`.

The erasure is at the `trusted` boundary and travels on a function pointer.
`console_write` and `console_err_write` both call `libc_write` and declare nothing, so
their TYPE is capability-free, so they fit `Writer`'s `write_fn` field, so every call
through the handle is capability-free. `println`, in the same module, declares `Console`
for the same syscall; nothing checks the difference.

This refuses to certify rather than resolving the target set, which is a whole-program
analysis and a separate project. The argument is already made twice in this file for `no
recursion` and `--report stack-depth`: a body containing an indirect call cannot be shown
acyclic, so it is excluded rather than assumed acyclic. Effect-freedom was the third
guarantee built on this call graph and the only one still assuming.

IT AFFECTS ADMISSION ONLY, NEVER EXTRACTION. A first attempt folded this into
`eligible`, which decides BOTH whether a function is admissible as effect-free AND
whether it is extracted for subject facts — so refusing higher-order functions dropped
them from the evidence surface entirely and `check_shadow_body_v2.sh` caught it, because
its "a function used as a VALUE is an edge" assertion exists precisely so a higher-order
program does not look dependency-free. Trading a false purity claim for a missing
dependency edge relocates an R-0004 gap instead of closing one. Hence `admissible` below,
kept separate from `eligible`. -/

/-- Qualified names of functions whose OWN body makes an indirect call. -/
private partial def indirectSeedModule (m : CModule) (pfx : String := "") : List String :=
  let qualPrefix := if pfx == "" then m.name else pfx ++ "." ++ m.name
  let here := m.functions.filterMap fun f =>
    if hasIndirectCallStmts f.body then some (qualPrefix ++ "." ++ f.name) else none
  here ++ m.submodules.foldl (fun acc sub => acc ++ indirectSeedModule sub qualPrefix) []

/-- One round: a function is opaque if any direct callee is. -/
private def effectOpaqueStep (graph : CallGraph) (cur : List String) : List String :=
  graph.foldl (fun acc (fn, callees) =>
    if acc.contains fn then acc
    else if callees.any (fun c => acc.contains c) then fn :: acc
    else acc) cur

/-- Least fixpoint over the seed. Bounded by the node count: each round adds at least one
    name or stops, so the graph size is sound fuel. -/
private partial def effectOpaqueFix (graph : CallGraph) (fuel : Nat) (cur : List String) : List String :=
  match fuel with
  | 0 => cur
  | fuel' + 1 =>
    let nxt := effectOpaqueStep graph cur
    if nxt.length == cur.length then cur else effectOpaqueFix graph fuel' nxt

/-- Functions that can REACH an indirect call, transitively. Transitivity is the point:
    `print_bytes` makes no indirect call itself and reaches one two hops down, so a
    per-body predicate misses exactly the case this came from. -/
def effectOpaqueSet (modules : List CModule) (graph : CallGraph) : List String :=
  let seed := (modules.foldl (fun acc m => acc ++ indirectSeedModule m) []).eraseDups
  effectOpaqueFix graph (graph.length + 1) seed

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

/-- Does `step` move `v` in `dir` (true = up) by a constant?

    Only literal steps count. `i = i + k` for a variable `k` cannot be certified here — `k`
    could be 0 or negative — so it reads as no progress, which classifies the loop unbounded
    and gets it REJECTED by the predictable profile. That is the safe direction: a loop wrongly
    called unbounded is refused, a loop wrongly called bounded is admitted and may hang. -/
private def stepsToward (v : String) (dir : Bool) (step : List CStmt) : Bool :=
  step.any fun st => match st with
    | .assign nm (.binOp op (.ident nm2 _) (.intLit k _) _) =>
        nm == v && nm2 == v && k > 0 &&
          ((dir && op == .add) || (!dir && op == .sub))
    -- `i = 1 + i`, the commuted form
    | .assign nm (.binOp op (.intLit k _) (.ident nm2 _) _) =>
        nm == v && nm2 == v && k > 0 && dir && op == .add
    | _ => false

/-- A loop is BOUNDED only if a variable in its condition is stepped toward the bound.

    The previous version asked two much weaker questions — "is the condition a comparison?" and
    "is the step list non-empty?" — and tied them to nothing. Both of these passed, and both
    run forever:

        for (let mut i = 0; i < n; z = z + 1) { … }   -- step touches an unrelated variable
        for (let mut i = 0; i < n; i = i - 1) { … }   -- step moves AWAY from the bound

    `--check predictable` admitted a module containing them and reported
    `0 unbounded loops`, while `PREDICTABLE_BOUNDARIES.md` claims bounded iteration is
    compiler-enforced. This is a liveness claim, so the consequence of getting it wrong is a
    program that hangs rather than one that crashes — invisible to every safety obligation.

    `neq` is deliberately NOT accepted: `while (i != n)` terminates only for a starting value
    this analysis cannot see, so it cannot be certified locally. -/
private def isBoundedCond (cond : CExpr) (step : List CStmt) : Bool :=
  match cond with
  -- `i < n` / `i <= n`: i must increase.   `i > n` / `i >= n`: i must decrease.
  | .binOp op (.ident v _) _ _ =>
      if op == .lt || op == .leq then stepsToward v true step
      else if op == .gt || op == .geq then stepsToward v false step
      else false
  -- the commuted form: `n > i` means i increases; `n < i` means i decreases.
  | .binOp op _ (.ident v _) _ =>
      if op == .gt || op == .geq then stepsToward v true step
      else if op == .lt || op == .leq then stepsToward v false step
      else false
  | _ => false

inductive LoopBound where
  | bounded
  | unbounded
  deriving BEq

mutual
partial def collectLoopBoundsExpr (e : CExpr) : List LoopBound :=
  match e with
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
  | .cast inner _ => collectLoopBoundsExpr inner
  | .allocCall inner alloc _ => collectLoopBoundsExpr inner ++ collectLoopBoundsExpr alloc
  | .ifExpr c t e _ => collectLoopBoundsExpr c ++ collectLoopBoundsStmts t ++ collectLoopBoundsStmts e
  | _ => []

partial def collectLoopBoundsArm (arm : CMatchArm) : List LoopBound :=
  match arm with
  | .enumArm _ _ _ guard body => (guard.map collectLoopBoundsExpr).getD [] ++ collectLoopBoundsStmts body
  | .litArm v guard body => collectLoopBoundsExpr v ++ (guard.map collectLoopBoundsExpr).getD [] ++ collectLoopBoundsStmts body
  | .varArm _ _ guard body => (guard.map collectLoopBoundsExpr).getD [] ++ collectLoopBoundsStmts body
  | .rangeArm lo hi _ guard body => collectLoopBoundsExpr lo ++ collectLoopBoundsExpr hi ++ (guard.map collectLoopBoundsExpr).getD [] ++ collectLoopBoundsStmts body

partial def collectLoopBoundsStmt (s : CStmt) : List LoopBound :=
  match s with
  | .while_ cond body _ step =>
    let thisBound := if isBoundedCond cond step then .bounded else .unbounded
    [thisBound] ++ collectLoopBoundsStmts body
  | .letDecl _ _ _ v => collectLoopBoundsExpr v
  | .assign _ v => collectLoopBoundsExpr v
  | .return_ (some v) _ => collectLoopBoundsExpr v
  | .return_ none _ => []
  | .expr e _ => collectLoopBoundsExpr e
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
  | .ident name _ => s!"(var {stripAlpha name})"
  | .binOp op lhs rhs _ => s!"(binop {repr op} {fingerprintExpr lhs} {fingerprintExpr rhs})"
  | .unaryOp op inner _ => s!"(unary {repr op} {fingerprintExpr inner})"
  | .call (.direct fn) _ args _ => s!"(call {fn} {fingerprintExprs args})"
  -- Distinct prefix: a direct call and an indirect call through a same-named
  -- local are different programs, so they must not fingerprint alike.
  | .call (.indirect b) _ args _ => s!"(callptr {stripAlpha b} {fingerprintExprs args})"
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
  | .allocCall inner alloc _ => s!"(alloc {fingerprintExpr inner} {fingerprintExpr alloc})"
  | .ifExpr cond th el _ => s!"(if {fingerprintExpr cond} {fingerprintStmts th} {fingerprintStmts el})"
where
  fingerprintExprs (es : List CExpr) : String :=
    " ".intercalate (es.map fingerprintExpr)
  fingerprintArm : CMatchArm → String
    | .enumArm en v binds guard body => s!"(arm {en}::{v} [{" ".intercalate (binds.map fun b => stripAlpha b.fst)}]{(guard.map fun g => s!" if {fingerprintExpr g}").getD ""} {fingerprintStmts body})"
    | .litArm val guard body => s!"(lit {fingerprintExpr val}{(guard.map fun g => s!" if {fingerprintExpr g}").getD ""} {fingerprintStmts body})"
    | .varArm b _ guard body => s!"(var {stripAlpha b}{(guard.map fun g => s!" if {fingerprintExpr g}").getD ""} {fingerprintStmts body})"
    | .rangeArm lo hi incl guard body => s!"(range {fingerprintExpr lo} {fingerprintExpr hi} {incl}{(guard.map fun g => s!" if {fingerprintExpr g}").getD ""} {fingerprintStmts body})"
  fingerprintStmt : CStmt → String
    | .letDecl name _ _ val => s!"(let {stripAlpha name} {fingerprintExpr val})"
    | .assign name val => s!"(set {stripAlpha name} {fingerprintExpr val})"
    | .return_ (some val) _ => s!"(ret {fingerprintExpr val})"
    | .return_ none _ => "(ret)"
    | .expr e _ => fingerprintExpr e
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
-- Core → PExpr extraction
-- ============================================================

/-- Map a Concrete `BinOp` to a typed `PBinOp` at the operand
    width.  Width-sensitive ops (`mod`, `bitxor`) carry the
    operand width and signedness; comparisons and width-agnostic
    arithmetic (`add`/`sub`/`mul`) ignore the type.

    Only widths/signs that `evalBinOp` supports are emitted; other
    combinations return `none` (extraction fails with a precise
    blocker rather than silently using i32 semantics).  See
    `docs/verification/PROOF_OBLIGATIONS_REGISTER.md` R-16 and R-17. -/
def binOpToPBinOp : BinOp → Ty → Option Proof.PBinOp
  -- Explicit modular add is the ONLY mod-2^32 spelling now that ordinary `+`
  -- is CHECKED (ROADMAP #10): `wrapping_add` on u32 → `addw 32`. Ordinary `.add`
  -- traps on overflow, so it is the mathematical add in its non-trapping domain
  -- (the proof discharges the no-overflow side condition); modeling it as `addw`
  -- would be unsound — it would certify wrapping the runtime never performs.
  | .wrappingAdd, .u32 => some (.addw 32 false)
  | .add, _ => some .add                 -- Int/i32/u32 checked add: mathematical
  | .sub, _ => some .sub
  | .mul, _ => some .mul
  | .mod, .i32 => some (.mod 32 true)
  | .mod, .u32 => some (.mod 32 false)
  | .mod, _ => none  -- other widths await multi-width extension
  | .div, .i32 => some (.div 32 true)
  | .div, .u32 => some (.div 32 false)
  | .div, _ => none  -- other widths await multi-width extension
  | .bitxor, .i32 => some (.bitxor 32 true)
  | .bitxor, .u32 => some (.bitxor 32 false)
  | .bitxor, .u8 => some (.bitxor 8 false)
  | .bitxor, _ => none
  | .bitor, .u8 => some (.bitor 8 false)
  | .bitor, .u32 => some (.bitor 32 false)
  | .bitor, _ => none
  | .bitand, .u32 => some (.bitand 32 false)
  | .bitand, _ => none
  | .shr, .u32 => some (.shr 32 false)
  | .shr, _ => none
  | .shl, .u32 => some (.shl 32 false)
  | .shl, _ => none
  | .eq, _  => some .eq
  | .neq, _ => some .ne
  | .lt, _  => some .lt
  | .leq, _ => some .le
  | .gt, _  => some .gt
  | .geq, _ => some .ge
  | _, _    => none

/-- Non-partial extractor restricted to the LITERAL fragment of
    `CExpr`.  Mirrors the literal cases of `cExprToPExpr` (lines
    708–709) so the Phase 12 soundness proofs in
    `Concrete.ProofSoundness` can reason about it WITHOUT the
    `partial def` opacity barrier.

    Returns `none` on any non-literal `CExpr` shape — extending
    the helper to other constructs would require unwinding the
    mutual block that makes `cExprToPExpr` `partial def` in the
    first place; do not extend this helper construct-by-construct
    in lockstep with `cExprToPExpr`, that defeats the purpose.

    Phase 12 obligation: a future commit that lifts the entire
    mutual block out of `partial def` (by replacing the `mapM`
    over field/element/arm lists with explicit structural
    recursion) discharges
    `cExprToPExpr (.intLit n ty) = cExprLitToPExpr (.intLit n ty)`
    as a theorem; today it holds by source inspection. -/
def cExprLitToPExpr : CExpr → Option Proof.PExpr
  | .intLit n _ => some (.lit (.int n))
  | .boolLit b  => some (.lit (.bool b))
  | _           => none

/-- Non-partial extractor restricted to the IDENTIFIER fragment.
    Companion to `cExprLitToPExpr` for variable references; same
    motivation (`partial def` opacity) and same usage pattern. -/
def cExprIdentToPExpr : CExpr → Option Proof.PExpr
  | .ident name _ => some (.var name)
  | _             => none

mutual
def cExprToPExprImpl : CExpr → Option Proof.PExpr
  | .intLit n _ => some (.lit (.int n))
  | .boolLit b => some (.lit (.bool b))
  | .ident name _ => some (.var name)
  | .binOp op lhs rhs _ => do
    -- Pass operand type (from lhs.ty) — for width-sensitive ops
    -- (mod, bitxor) this picks the right typed PBinOp; for
    -- width-agnostic ops the type is ignored.
    let pop ← binOpToPBinOp op (CExpr.ty lhs)
    let pl ← cExprToPExprImpl lhs
    let pr ← cExprToPExprImpl rhs
    some (.binOp pop pl pr)
  | .call (.direct fn) _ args _ => do
    let pargs ← cExprListToPExpr args
    some (.call fn pargs)
  -- Indirect callee: a LOCALLY BOUND callable, not a definition. The model does
  -- not need the callee's definition — it needs an identity for the thing being
  -- applied, and inside this body the parameter name IS that identity, which the
  -- theorem quantifies over. Refusing to extract these was too strong: it cost
  -- std three real proofs (Option::map, Result::map, Result::map_err), whose
  -- statements hold for ANY `f` precisely because `f` is opaque.
  --
  -- Extracting them as `.call` was ALSO wrong (bug 061): a parameter named `f`
  -- and a definition named `f` became the SAME node, so the evaluator resolved a
  -- parameter application through the GLOBAL function table — a soundness hazard
  -- in the one place soundness claims are made. `.applyVar` keeps the identity
  -- local, answerable only by `FnTable.callables` (R-0442).
  | .call (.indirect binding) _ args _ => do
    let pargs ← cExprListToPExpr args
    some (.applyVar binding pargs)
  | .ifExpr cond thenBranch elseBranch _ => do
    let pc ← cExprToPExprImpl cond
    let pt ← cStmtsToPExpr thenBranch
    let pe ← cStmtsToPExpr elseBranch
    some (.ifThenElse pc pt pe)
  | .structLit name _typeArgs fields _ => do
    let pfields ← cFieldsToPExpr fields
    some (.structLit name pfields)
  | .enumLit enumName variant _typeArgs fields _ => do
    let pfields ← cFieldsToPExpr fields
    some (.enumLit enumName variant pfields)
  | .fieldAccess obj field _ => do
    let po ← cExprToPExprImpl obj
    some (.fieldAccess po field)
  | .arrayIndex arr idx _ => do
    let pa ← cExprToPExprImpl arr
    let pi ← cExprToPExprImpl idx
    some (.arrayIndex pa pi)
  | .match_ scrutinee arms _ => do
    let ps ← cExprToPExprImpl scrutinee
    let parms ← cMatchArmsToP arms
    some (.match_ ps parms)
  | .cast inner _ => do
    let pi ← cExprToPExprImpl inner
    some (.cast pi)
  | .arrayLit elems _ => do
    let pelems ← cExprListToPExpr elems
    some (.arrayLit pelems)
  | _ => none

/-- Translate one Concrete match arm into a `(pattern, body)` pair.
    The body is a CStmt list; we extract it via `cStmtsToPExpr`
    with `none` continuation — match arm bodies must terminate
    (return or yield a value), not fall through. -/
def cMatchArmToP : CMatchArm → Option (Proof.PMatchPat × Proof.PExpr)
  -- A guarded arm is not modelled in the proof path (V1); like range patterns,
  -- it makes the function non-proof-extractable (disclosed in identifyUnsupported).
  | .enumArm enumName variant bindings none body => do
    let pbody ← cStmtsToPExpr body
    some (.enumPat enumName variant (bindings.map Prod.fst), pbody)
  | .litArm value none body => do
    -- Literal patterns must extract to PExpr values; we then read
    -- the literal value back out for the pattern shape. Only int
    -- and bool literals are supported as match-arm values today.
    let pval ← cExprToPExprImpl value
    let v ← match pval with
      | .lit v => some v
      | _ => none
    let pbody ← cStmtsToPExpr body
    some (.litPat v, pbody)
  | .varArm binding _bindTy none body => do
    let pbody ← cStmtsToPExpr body
    some (.varPat binding, pbody)
  | .enumArm _ _ _ (some _) _ | .litArm _ (some _) _ | .varArm _ _ (some _) _ => none
  | .rangeArm _ _ _ _ _ =>
    -- Range patterns are not yet modelled in the proof path (V1). A function
    -- using one is simply not proof-extractable — disclosed via
    -- identifyUnsupported below — rather than silently mis-modelled.
    none

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
def cStmtsToPExprKImpl : List CStmt → Option Proof.PExpr → Option Proof.PExpr
  | [], k => k
  | [.return_ (some e) _], _ => cExprToPExprImpl e
  | [.expr e _], _ => cExprToPExprImpl e
  | (.letDecl name _ _ val) :: rest, k => do
    let pv ← cExprToPExprImpl val
    let pb ← cStmtsToPExprKImpl rest k
    some (.letIn name pv pb)
  -- Array index assignment `arr[i] = v`.  Only supported when
  -- `arr` is a simple identifier — we model the mutation as a
  -- shadowing letIn that rebinds the name to (arraySet arr idx
  -- val), which is the canonical functional-update encoding from
  -- docs/verification/PROOF_STATE_MODEL.md § 2.  More complex `arr` (e.g.
  -- `obj.field[i] = v`) needs structSet first; deferred.
  | (.arrayIndexAssign (.ident name _) idx val) :: rest, k => do
    let pi ← cExprToPExprImpl idx
    let pv ← cExprToPExprImpl val
    let pb ← cStmtsToPExprKImpl rest k
    some (.letIn name (.arraySet (.var name) pi pv) pb)
  -- Bounded while loop with flat-assign body.  CStmt.while_
  -- carries `body` containing the source body with the for-loop
  -- step already concatenated (see Elab.lean's forLoop desugar:
  -- `whileBody := cBody ++ cStep`).  The `step` field exists for
  -- other consumers (continue-target lowering) and is NOT
  -- iterated separately — using it here would double-step.
  -- Every member of `body` must be `CStmt.assign`; any nested
  -- `let`, `if`, or `return` fails extraction and falls back to
  -- identifyUnsupportedStmt (which reports the actual blocker,
  -- not "while loop").
  | (.while_ cond body _ _step) :: rest, k => do
    let pc ← cExprToPExprImpl cond
    let pCont ← cStmtsToPExprKImpl rest k
    -- First try flat-assign extraction (every body stmt is a
    -- CStmt.assign); fall back to while_step when body has
    -- richer control flow (let, if-with-return, ...).
    let flatUpdates : Option (List (String × Proof.PExpr)) :=
      cAssignBodyToUpdates body
    match flatUpdates with
    | some updates => some (.while_ pc updates pCont)
    | none => do
      let carried := extractCarried body
      let step ← cStmtsToStepExpr [] body
      some (.while_step pc carried step pCont)
  -- Singleton if-else (last stmt of body): each branch inherits the
  -- outer continuation. If a branch returns, k is dead; if it falls
  -- through, k is used.
  | [.ifElse cond thenBranch (some elseBranch)], k => do
    let pc ← cExprToPExprImpl cond
    let pt ← cStmtsToPExprKImpl thenBranch k
    let pe ← cStmtsToPExprKImpl elseBranch k
    some (.ifThenElse pc pt pe)
  -- If-else followed by more statements: both branches' fall-through
  -- continuation is `rest with the outer k`.
  | (.ifElse cond thenBranch (some elseBranch)) :: rest, k => do
    let pc ← cExprToPExprImpl cond
    let pkRest ← cStmtsToPExprKImpl rest k
    let pt ← cStmtsToPExprKImpl thenBranch (some pkRest)
    let pe ← cStmtsToPExprKImpl elseBranch (some pkRest)
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
    let pc ← cExprToPExprImpl cond
    let pkRest ← cStmtsToPExprKImpl rest k
    let pt ← cStmtsToPExprKImpl thenBranch (some pkRest)
    some (.ifThenElse pc pt pkRest)
  | _, _ => none

def cStmtsToPExpr (stmts : List CStmt) : Option Proof.PExpr :=
  cStmtsToPExprKImpl stmts none

/-- Collect names that are `assign`-ed (rebound) inside a CStmt list.
    Walks the body recursively, including inside `ifElse` branches.
    Used to populate the `carried` field of `PExpr.while_step` so
    the Phase 12 preservation argument can name exactly which env
    bindings the loop rewrites. -/
def extractCarried : List CStmt → List String
  | [] => []
  | (.assign name _) :: rest =>
    let r := extractCarried rest
    if r.contains name then r else name :: r
  | (.ifElse _ thenBr (some elseBr)) :: rest =>
    (extractCarried thenBr ++ extractCarried elseBr ++ extractCarried rest).eraseDups
  | (.ifElse _ thenBr none) :: rest =>
    (extractCarried thenBr ++ extractCarried rest).eraseDups
  | _ :: rest => extractCarried rest

/-- Extract a CStmt list (a while-loop body) into a PExpr that
    evaluates to a `PVal.enum_ "LoopStep" variant fields` value
    per `docs/verification/PROOF_STATE_MODEL.md` § 4.

    `assigns` accumulates loop-carried-variable updates from `assign`
    statements walked so far.  When control falls off the end of the
    body, the result is `Cont assigns`; an early `return e` aborts
    with `Break e`.  Local `let` bindings wrap the surrounding step
    PExpr.

    Supported body shapes (anything else returns `none`):
      - `letDecl name _ _ val`: wraps rest in `letIn`
      - `assign name val`: extends `assigns`, walks rest
      - `return_ (some e)`: produces `Break e`
      - `ifElse cond thenBr none`: branches; then is its own
        step-expr (may Break or fall-through), else is `rest`. -/
def cStmtsToStepExpr
    (assigns : List (String × Proof.PExpr)) :
    List CStmt → Option Proof.PExpr
  | [] =>
    some (.enumLit "LoopStep" "Cont" assigns)
  | (.return_ (some e) _) :: _ => do
    let pv ← cExprToPExprImpl e
    some (.enumLit "LoopStep" "Break" [("value", pv)])
  | (.letDecl name _ _ val) :: rest => do
    let pv ← cExprToPExprImpl val
    let pb ← cStmtsToStepExpr assigns rest
    some (.letIn name pv pb)
  | (.assign name val) :: rest => do
    let pv ← cExprToPExprImpl val
    let assigns' :=
      if assigns.any (·.1 == name) then
        assigns.map fun (n, e) => if n == name then (name, pv) else (n, e)
      else
        assigns ++ [(name, pv)]
    cStmtsToStepExpr assigns' rest
  | (.ifElse cond thenBr none) :: rest => do
    let pc ← cExprToPExprImpl cond
    let pt ← cStmtsToStepExpr assigns thenBr
    let pe ← cStmtsToStepExpr assigns rest
    some (.ifThenElse pc pt pe)
  | _ => none

/-- Helper: extract a list of CExprs into a list of PExprs.
    Replaces `args.mapM cExprToPExprImpl` and
    `elems.mapM cExprToPExprImpl` so the mutual block can be
    non-partial. -/
def cExprListToPExpr : List CExpr → Option (List Proof.PExpr)
  | [] => some []
  | e :: rest => do
    let pe ← cExprToPExprImpl e
    let prest ← cExprListToPExpr rest
    some (pe :: prest)

/-- Helper: extract a list of (name, CExpr) field pairs.
    Replaces both struct-literal and enum-literal field mapMs. -/
def cFieldsToPExpr :
    List (String × CExpr) → Option (List (String × Proof.PExpr))
  | [] => some []
  | (name, e) :: rest => do
    let pe ← cExprToPExprImpl e
    let prest ← cFieldsToPExpr rest
    some ((name, pe) :: prest)

/-- Helper: extract a list of match arms.
    Replaces `arms.mapM cMatchArmToP`. -/
def cMatchArmsToP :
    List CMatchArm → Option (List (Proof.PMatchPat × Proof.PExpr))
  | [] => some []
  | arm :: rest => do
    let parm ← cMatchArmToP arm
    let prest ← cMatchArmsToP rest
    some (parm :: prest)

/-- Helper: try flat-assign extraction over a while body.
    Returns `some updates` if every stmt is a flat update —
    either a scalar `.assign name val` or an array-element
    assignment `arr[i] = v` where `arr` is a simple identifier —
    `none` otherwise.  Replaces the `body.mapM` lambda in
    cStmtsToPExprKImpl's while_ case.

    An array-element assignment `name[idx] = val` is modelled as
    the functional update `(name, arraySet name idx val)`, the
    same encoding cStmtsToPExprKImpl uses at the top level
    (docs/verification/PROOF_STATE_MODEL.md § 2).  Because `while_` applies the
    update list IN ORDER with later updates seeing earlier ones
    (see PExpr.while_ semantics), several writes to the same array
    in one iteration (e.g. state_to_bytes' four byte stores) chain
    correctly, and the trailing for-loop counter step `i = i + 1`
    is just another scalar update at the end of the list. -/
def cAssignBodyToUpdates :
    List CStmt → Option (List (String × Proof.PExpr))
  | [] => some []
  | s :: rest =>
    match s with
    | .assign name val => do
      let pv ← cExprToPExprImpl val
      let prest ← cAssignBodyToUpdates rest
      some ((name, pv) :: prest)
    | .arrayIndexAssign (.ident name _) idx val => do
      let pi ← cExprToPExprImpl idx
      let pv ← cExprToPExprImpl val
      let prest ← cAssignBodyToUpdates rest
      some ((name, .arraySet (.var name) pi pv) :: prest)
    | _ => none
end

/-- Non-partial wrapper for `cExprToPExprImpl`.  The literal
    cases reduce by `rfl` so Phase 12 preservation theorems
    (R-01, R-02) can close their antecedent against the REAL
    extractor, not just a parallel helper.  All other cases
    delegate to the partial-def implementation in the mutual
    block above.

    Why: `cExprToPExprImpl` is `partial def` (the mutual block
    contains `mapM` calls Lean's structural-recursion checker
    can't see decreasing through).  Lean's kernel treats partial
    def as opaque — no equation lemmas, `rfl` cannot reduce.
    This wrapper sits OUTSIDE the mutual block, so its literal
    arms ARE reducible by definition.

    The wrapper preserves the public API: every caller of
    `cExprToPExpr` (Report.lean, the rest of ProofCore) gets
    the same behavior as before.  Internal recursive calls
    inside the mutual block use `cExprToPExprImpl` directly. -/
def cExprToPExpr : CExpr → Option Proof.PExpr
  | .intLit n _   => some (.lit (.int n))
  | .boolLit b    => some (.lit (.bool b))
  | .ident name _ => some (.var name)
  | .binOp op lhs rhs _ => do
    let pop ← binOpToPBinOp op (CExpr.ty lhs)
    let pl ← cExprToPExpr lhs
    let pr ← cExprToPExpr rhs
    some (.binOp pop pl pr)
  | .cast inner _ => do
    let pi ← cExprToPExpr inner
    some (.cast pi)
  | .fieldAccess obj field _ => do
    let po ← cExprToPExpr obj
    some (.fieldAccess po field)
  | .arrayIndex arr idx _ => do
    let pa ← cExprToPExpr arr
    let pi ← cExprToPExpr idx
    some (.arrayIndex pa pi)
  | .call (.direct fn) _ args _ => do
    let pargs ← cExprListToPExpr args
    some (.call fn pargs)
  -- Indirect callee: a LOCALLY BOUND callable, not a definition. The model does
  -- not need the callee's definition — it needs an identity for the thing being
  -- applied, and inside this body the parameter name IS that identity, which the
  -- theorem quantifies over. Refusing to extract these was too strong: it cost
  -- std three real proofs (Option::map, Result::map, Result::map_err), whose
  -- statements hold for ANY `f` precisely because `f` is opaque.
  --
  -- Extracting them as `.call` was ALSO wrong (bug 061): a parameter named `f`
  -- and a definition named `f` became the SAME node, so the evaluator resolved a
  -- parameter application through the GLOBAL function table — a soundness hazard
  -- in the one place soundness claims are made. `.applyVar` keeps the identity
  -- local, answerable only by `FnTable.callables` (R-0442).
  | .call (.indirect binding) _ args _ => do
    let pargs ← cExprListToPExpr args
    some (.applyVar binding pargs)
  | .structLit name _ fields _ => do
    let pfields ← cFieldsToPExpr fields
    some (.structLit name pfields)
  | .enumLit ename variant _ fields _ => do
    let pfields ← cFieldsToPExpr fields
    some (.enumLit ename variant pfields)
  | .arrayLit elems _ => do
    let pelems ← cExprListToPExpr elems
    some (.arrayLit pelems)
  | .match_ scrutinee arms _ => do
    let ps ← cExprToPExpr scrutinee
    let parms ← cMatchArmsToP arms
    some (.match_ ps parms)
  | e             => cExprToPExprImpl e

/-- Non-partial wrapper for `cStmtsToPExprKImpl`.  Handles
    `.letDecl :: rest` directly so Phase 12 R-06 (letIn
    preservation) can discharge against the REAL extractor.
    All other CStmt-list shapes delegate to the partial-def
    implementation in the mutual block above.

    Recursion is structural on `rest` (shorter than the
    input list).  The val arm uses `cExprToPExpr` (the
    expression wrapper) so the val side is also wrapper-
    reducible when val itself is in the supported fragment. -/
def cStmtsToPExprK : List CStmt → Option Proof.PExpr → Option Proof.PExpr
  | [], k => k
  | [.return_ (some e) _], _ => cExprToPExpr e
  | [.expr e _], _ => cExprToPExpr e
  | (.letDecl name _ _ val) :: rest, k => do
    let pv ← cExprToPExpr val
    let pb ← cStmtsToPExprK rest k
    some (.letIn name pv pb)
  | (.arrayIndexAssign (.ident name _) idx val) :: rest, k => do
    let pi ← cExprToPExpr idx
    let pv ← cExprToPExpr val
    let pb ← cStmtsToPExprK rest k
    some (.letIn name (.arraySet (.var name) pi pv) pb)
  -- if-without-else (early-return shape): then branch's cont
  -- is `rest with k`; implicit else is the same.
  | (.ifElse cond thenBranch none) :: rest, k => do
    let pc ← cExprToPExpr cond
    let pkRest ← cStmtsToPExprK rest k
    let pt ← cStmtsToPExprK thenBranch (some pkRest)
    some (.ifThenElse pc pt pkRest)
  -- Bounded while loop: flat-assign body extracts to PExpr.while_;
  -- richer body falls back to PExpr.while_step.
  | (.while_ cond body _ _step) :: rest, k => do
    let pc ← cExprToPExpr cond
    let pCont ← cStmtsToPExprK rest k
    match cAssignBodyToUpdates body with
    | some updates => some (.while_ pc updates pCont)
    | none => do
      let stepE ← cStmtsToStepExpr [] body
      some (.while_step pc (extractCarried body) stepE pCont)
  | stmts, k => cStmtsToPExprKImpl stmts k

-- Unsupported construct identification

/-- Floating-point types have no active proof profile (ProvableV1 is
    integer/bool/BitVec only). Any float value or float arithmetic makes a
    function unprovable until a float profile exists — see ROADMAP
    "Provable Float V1". Detecting this is what keeps the proof system from
    silently modeling float `+` as integer `.add`. -/
private def isFloatTy : Ty → Bool
  | .float32 | .float64 => true
  | _ => false

private def floatReason : String := "unprofiled floating-point arithmetic"

mutual
private partial def identifyUnsupportedExpr : CExpr → List String
  | .floatLit .. => [floatReason]
  | .ident _ ty => if isFloatTy ty then [floatReason] else []
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
  | .arrayIndex arr idx _ =>
    identifyUnsupportedExpr arr ++ identifyUnsupportedExpr idx
  | .match_ scrutinee arms _ =>
    -- Match itself is supported; recurse so any unsupported
    -- construct inside the scrutinee or arm bodies is reported.
    let scrutUns := identifyUnsupportedExpr scrutinee
    let armUns := arms.foldl (fun acc arm =>
      -- Range patterns and match guards are not modelled in the proof path yet
      -- (V1); disclose them so a function using one is flagged, not mis-modelled.
      let armMarker := match arm with | .rangeArm .. => ["range pattern"] | _ => []
      let guardMarker := match arm with
        | .enumArm _ _ _ (some _) _ | .litArm _ (some _) _
        | .varArm _ _ (some _) _ | .rangeArm _ _ _ (some _) _ => ["match guard"]
        | _ => []
      let body := match arm with
        | .enumArm _ _ _ _ b => b
        | .litArm _ _ b => b
        | .varArm _ _ _ b => b
        | .rangeArm _ _ _ _ b => b
      acc ++ armMarker ++ guardMarker ++ body.foldl (fun a s => a ++ identifyUnsupportedStmt s) []) []
    scrutUns ++ armUns
  | .borrow .. => ["borrow"]
  | .borrowMut .. => ["mutable borrow"]
  | .deref .. => ["deref"]
  -- `.arrayLit` extracts to `PExpr.arrayLit`; recurse to surface
  -- any unsupported construct inside individual elements.
  | .arrayLit elems _ =>
    elems.foldl (fun acc e => acc ++ identifyUnsupportedExpr e) []
  -- `.cast` extracts to `PExpr.cast` (identity semantics on
  -- mathematical `Int`); recurse so any unsupported construct
  -- inside the cast operand still surfaces.
  | .cast inner targetTy =>
    (if isFloatTy targetTy then [floatReason] else []) ++ identifyUnsupportedExpr inner
  | .fnRef .. => ["function reference"]
  | .allocCall .. => ["alloc call"]
  | .unaryOp .. => ["unary operator"]
  | .binOp op lhs rhs ty =>
    -- A float-typed result means float arithmetic — unprovable (no float
    -- profile). Caught here so float `+` is never silently the integer `.add`.
    let floatUnsup := if isFloatTy ty || isFloatTy (CExpr.ty lhs) then [floatReason] else []
    -- Pass operand type so width-sensitive ops (mod/bitxor) flag
    -- unsupported widths precisely instead of conflating with op support.
    let opUnsup := match binOpToPBinOp op (CExpr.ty lhs) with
      | none => [s!"unsupported operator: {repr op} at {repr (CExpr.ty lhs)}"]
      | some _ => []
    floatUnsup ++ opUnsup ++ identifyUnsupportedExpr lhs ++ identifyUnsupportedExpr rhs
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
  | .expr e _ => identifyUnsupportedExpr e
  | .ifElse cond thenBr elseBr =>
    -- if without else is supported by cStmtsToPExprK as
    -- early-return-with-fall-through. Do NOT flag it here.
    identifyUnsupportedExpr cond ++
    thenBr.foldl (fun acc s => acc ++ identifyUnsupportedStmt s) [] ++
    match elseBr with
    | some stmts => stmts.foldl (fun acc s => acc ++ identifyUnsupportedStmt s) []
    | none => []
  -- Bounded while is supported by cStmtsToPExprK in two forms:
  --   1. flat-assign body → PExpr.while_ (every body stmt is .assign)
  --   2. richer body  → PExpr.while_step (let/assign/return/if-no-else)
  -- Only flag the shape when neither form fits.
  | .while_ cond body _ _step =>
    -- Match the support criteria of cStmtsToStepExpr:
    let rec bodyFitsStep : List CStmt → Bool
      | [] => true
      | (.letDecl ..) :: rest => bodyFitsStep rest
      | (.assign ..) :: rest => bodyFitsStep rest
      | (.return_ (some _) _) :: _ => true  -- early Break, body terminates here
      | (.ifElse _ thenBr none) :: rest => bodyFitsStep thenBr && bodyFitsStep rest
      | _ :: _ => false
    let shapeReason :=
      if bodyFitsStep body then []
      else ["while loop body shape (only let/assign/return/if-no-else supported)"]
    let condUnsup := identifyUnsupportedExpr cond
    -- Recurse into body statements for nested unsupported exprs.
    let bodyUnsup := body.foldl (fun acc s => acc ++ identifyUnsupportedStmt s) []
    shapeReason ++ condUnsup ++ bodyUnsup
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
    -- `.while_` is supported when body + step are flat assigns;
    -- precise blockers (non-assign body shape, unsupported op
    -- inside cond/assign expr) surface via identifyUnsupportedStmt.
    | .fieldAssign .. => some "field assignment"
    | .derefAssign .. => some "deref assignment"
    -- `.arrayIndexAssign` is supported when arr is a simple ident;
    -- complex-arr forms (obj.field[i] = v) are not, and the precise
    -- blocker surfaces via identifyUnsupportedStmt below.
    | .arrayIndexAssign (.ident ..) .. => none
    | .arrayIndexAssign .. => some "array index assignment (complex receiver)"
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
  /-- R-0484: this function can reach an indirect call, so its effects are not
      determined by its declared capabilities. Reported and consumed by ADMISSION;
      deliberately NOT folded into `eligible`, which also gates extraction. -/
  effectOpaque   : Bool := false
  loc            : Option SourceLoc

/-- Admissible as EFFECT-FREE, which is strictly stronger than extractable.
    `eligible` decides whether a function is extracted for subject facts at all, so
    folding opacity into it removes higher-order functions from the evidence surface —
    measured, and caught by `check_shadow_body_v2.sh`. Obligation status derives from
    this; the entry/excluded split and INV-8 keep using `eligible`. -/
def EligibilityEntry.admissible (e : EligibilityEntry) : Bool :=
  -- LIVE since 2026-09-26. It was inert for one release, and the reason it could be
  -- enabled is worth recording, because the objection looked like a consequence of the
  -- rule and was not.
  --
  -- The observation: enabling this moved three `pureCoreFns` links from `needs_recheck`
  -- to `ineligible`, dropping them out of drift coverage (11 -> 8) and out of
  -- `replayTargetsOf`. `check_purecore_proofs.sh` argues replay must keep asking about
  -- pending claims "or the migration cannot clear them", so that looked like a reason to
  -- hold the rule back.
  --
  -- The actual cause was a COUPLING. Obligation status was derived from this predicate
  -- through a parameter merely NAMED `eligible`, so refusing admission also withdrew the
  -- obligation. Status now derives from EXTRACTABLE and admission is carried beside it
  -- (see `Obligation.admissible`), which keeps the four questions apart and leaves
  -- maintenance — replay, drift, staleness — untouched by an admission verdict. Drift
  -- coverage is 11 with the rule live.
  --
  -- An admission failure must never erase an artifact from MAINTENANCE: otherwise
  -- reclassifying an effect silently removes claims from replay and drift detection,
  -- which is the opposite of degrading honestly.
  e.eligible && !e.effectOpaque

-- ============================================================
-- Proof registry types (moved from Report.lean)
-- ============================================================

/-- A single proof registry entry linking a Concrete function to its proof.

    The `coverage` field classifies what KIND of theorem the proof
    is, so a reviewer reading "X is proved" can tell at a glance
    whether they're looking at a point proof or an iff theorem:

    * `point` — concrete-input case (e.g., zeros tag returns 0).
    * `one_direction` — universal but covers only one direction of
      an iff (e.g., "equal tags return 1" without "differing tags
      return 0"; "len < 5 returns TooShort" without
      "len ≥ 5 returns Ok").
    * `iff` — full functional specification or bidirectional iff
      (e.g., `validate_version` proves `eval = if v=1 then 0 else 1`).
    * `invariant` — loop / data-structure invariant theorem.
    * `runtime_error` — discharges a runtime-error obligation
      (bounds, div/mod-zero, overflow, cast, loop bound).
    * `full_contract` — a `#[requires]`/`#[ensures]` source contract
      discharged in full.

    Empty string means "unclassified" — back-compat for older
    registry entries written before this field existed.  Phase 1
    item 4 closes the gap by classifying every existing flagship
    proof.

    See `docs/verification/PROOF_OBLIGATIONS_REGISTER.md` for which proofs map to
    which classification, and "Phase 1 items 3/4" in the roadmap. -/
structure ProofRegistryEntry where
  function        : String  -- qualified name, e.g. "main.parse_byte"
  bodyFingerprint : String  -- expected body fingerprint
  proof           : String  -- Lean proof name, e.g. "Concrete.Proof.parse_byte_correct"
  spec            : String  -- spec name, e.g. "parse_byte_adds_offset"
  coverage        : String  -- point|one_direction|iff|invariant|runtime_error|full_contract|""
  ensuresProof    : Option String := none  -- theorem discharging the source `#[ensures(...)]` obligation, if any
  expectedHash    : Option String := none  -- stored short hash of the body fingerprint (from in-source
                                            -- #[proof_fingerprint]); when set, staleness compares
                                            -- hash(currentFingerprint) against this instead of the full string
  sourceLinked    : Bool := false          -- true if synthesized from an in-source proof link (vs JSON-backed)
  deriving Repr, Inhabited

/-- All canonical proof coverage classifications. -/
def ProofRegistryEntry.allCoverageKinds : List String :=
  ["point", "one_direction", "iff", "invariant", "runtime_error", "full_contract"]

abbrev ProofRegistry := List ProofRegistryEntry

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
  | registry    -- synthesized from in-source proof links
  deriving BEq, Repr

/-- Spec attachment for a function: identity binding only.
    Proof status (proved/stale/unproved) is derived downstream by comparing
    the attachment's expectedFp against the function's current fingerprint. -/
structure SpecAttachment where
  specId      : SpecIdentity
  proofName   : String       -- e.g. "Concrete.Proof.parse_byte_correct"
  source      : SpecSource
  expectedFp  : String       -- fingerprint the proof was written against
  expectedHash : Option String := none  -- stored short hash (in-source #[proof_fingerprint]);
                                         -- when set, staleness compares hash(currentFp) against it

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
      expectedFp := re.bodyFingerprint
      expectedHash := re.expectedHash }
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
  /-- The stored proof-subject digest was written under an EARLIER SCHEMA, so it
      cannot be compared against the current one. Distinct from `stale` in exactly
      the way that matters: `stale` asserts the subject CHANGED, and here the
      format changed while the program may not have. Reporting such an entry as
      `stale` would be a false claim about the body, and reporting it as `proved`
      would be a claim the comparison never made.

      The v1 digest hashed Core statements only; v2 covers identity, full typed
      signature, generics, capabilities and contracts. A v1 value therefore
      answers a strictly weaker question, and no comparison against it can
      establish v2 freshness — the honest verdict is that this claim needs
      re-verification, not that it failed one. -/
  | needsRecheck
  /-- A proof link exists but carries NO stored proof-subject digest, so there is
      nothing for the freshness check to compare the body against. Distinct from
      `stale`: `stale` means the subject was recorded and the body has since
      changed; `unbound` means the subject was never recorded, so the claim was
      never checkable and no edit could ever have revealed that. Reporting it as
      `stale` would assert a body change that did not happen. -/
  | unbound
  /-- The obligation's OWN subject is fresh, but a dependency reachable from it is
      not current (stale, unbound, missing, blocked, or ineligible). Distinct from
      `stale` and `unbound`, both of which describe this function's own subject:
      reporting either here would assert something false about this body. What is
      wrong is downstream, and the claim cannot contribute `proved_by_lean`
      evidence while it is.

      Introduced by R-0004 slice 3. Before it, `notCurrentDeps` was recorded on the
      obligation and never consulted, so a function whose proof rested on a stale
      one — at one hop or several — still reported `proved` (bug 062). -/
  | depsNotCurrent
  /-- The obligation's own subject is fresh and its dependencies are current, but the SCOPED
      correspondence for this subject is not usable: some compiler edge in its closure has no
      validated justification keyed by `DefinitionIdentity`, or a table it names could not be read.

      Distinct from every status above, and the distinction is the whole point of R-0004 package 2.
      `stale` and `unbound` describe this body's own subject digest; `depsNotCurrent` describes a
      callee's status. This describes the JUSTIFICATION: the proof claims a dependency closure whose
      edges are not each backed by exactly one witness naming the exact implementation. Reporting it
      as any of the others would assert something false about the body, the callee, or the freshness
      check — and reporting it as `proved` is precisely the false green this slice exists to remove.

      A subject reaching this state is not a proof that failed; it is a proof whose dependency
      justification was never established. -/
  | correspondenceUnjustified
  deriving BEq, Repr

/-- The marker for "no obligation record was found for this function".

    R-0479. Deliberately NOT a member of `ObligationStatus.canonical`'s vocabulary: a failed lookup
    must not render as a status, or "no record exists" becomes indistinguishable from "a record
    exists and its proof is missing". Three sites defaulted to `"missing"` and said the second thing
    while meaning the first.

    ONE DEFINITION, because three call sites spelled it independently after the fix, and a marker
    whose spelling can drift is a marker a consumer cannot match on. `check_one_producer.sh` pins
    that this literal appears in exactly one file — the same discipline applied to digests, since a
    string constant duplicated across surfaces is a flat-string fact with several producers
    (`docs/verification/EVIDENCE_ARCHITECTURE.md` names that shape as the thing typed evidence replaces). -/
def noObligationRecord : String := "no_obligation_record"

/-- The marker for "this capability's origin could not be read".

    R-0479, and the reason it is not `"transitive"` — which is what it used to be. `transitive` is a
    real origin with positive content, so an absent `origin` key or a malformed trace rendered as a
    claim that the capability arrived by that route. This value is outside the origin vocabulary and
    asserts nothing.

    Sits beside `noObligationRecord` so the markers have ONE home: they are the same kind of thing
    (an out-of-domain value meaning "not read"), and scattering them is how one gets renamed while
    a consumer keeps matching the old spelling. -/
def originUnavailable : String := "origin_unavailable"

/-- Canonical string representation of an ObligationStatus.
    This is the single source of truth for status terminology across
    all output surfaces: JSON facts, CLI reports, documentation, and
    release criteria. All renderers MUST use this function. -/
def ObligationStatus.canonical : ObligationStatus → String
  | .unbound        => "unbound"
  | .needsRecheck   => "needs_recheck"
  | .depsNotCurrent => "deps_not_current"
  | .correspondenceUnjustified => "correspondence_unjustified"
  | .proved     => "proved"
  | .stale      => "stale"
  | .missing    => "missing"
  | .blocked    => "blocked"
  | .ineligible => "ineligible"
  | .trusted    => "trusted"

/-- All valid canonical status strings, for validation gates.

    Kept in step with `canonical` by `allStatuses` below rather than by hand:
    this list had silently omitted `unbound` since that status was introduced,
    and nothing noticed because it has no consumers. A hand-maintained mirror of
    a constructor list is the same restated-fact hazard as the rest of this
    file's history — derive it.

    EVERY constructor, and the omissions were real: `needsRecheck` and `correspondenceUnjustified`
    were both missing, so any consumer enumerating the status vocabulary through this list — a
    schema, a summary, a policy table — silently did not know they existed. A list that must be
    exhaustive and is written by hand drifts the moment a constructor is added; `mem_all` below
    turns that into a compile error instead. -/
def ObligationStatus.allStatuses : List ObligationStatus :=
  [.proved, .stale, .missing, .blocked, .ineligible, .trusted, .unbound, .needsRecheck,
   .depsNotCurrent, .correspondenceUnjustified]

/-- COMPLETENESS IS A THEOREM, not a length check. A count agrees with whatever was written; this
    leaves an unsolved case naming the missing constructor. -/
theorem ObligationStatus.mem_all (s : ObligationStatus) : s ∈ ObligationStatus.allStatuses := by
  cases s <;> simp [ObligationStatus.allStatuses]

def ObligationStatus.allCanonical : List String :=
  ObligationStatus.allStatuses.map ObligationStatus.canonical

/-- May a dependent contribute `proved_by_lean` evidence while this status holds
    for something it reaches?

    ONE place decides, because the alternative is each consumer deciding — the
    defect class this file keeps hitting. `trusted` is current on purpose: a
    trusted boundary is a declared, audited escape hatch, not an unknown.
    Everything else is either not proved or not checkable, and a claim resting on
    it is not evidence. -/
def ObligationStatus.isCurrentForDependents : ObligationStatus → Bool
  | .proved | .trusted => true
  -- `needsRecheck` is NOT current: the stored digest answers an older, weaker
  -- question, so nothing downstream may treat this claim as established.
  | .stale | .missing | .blocked | .ineligible | .unbound | .needsRecheck
  -- A claim whose own dependency justification was never established is NOT current for anyone
  -- else's: propagating it would let an unjustified closure become the foundation of a second one.
  | .depsNotCurrent | .correspondenceUnjustified => false

/-- A proof obligation generated by the proof pipeline.
    Each obligation has a stable identity (function + spec) and
    a mechanically derived status. -/
structure Obligation where
  functionId   : FunctionIdentity
  bareName     : String
  status       : ObligationStatus
  spec         : Option SpecAttachment
  expectedFp   : String           -- from attachment, or ""
  -- Reasons a function is not eligible for PROOF. Named for what it holds: these are
  -- eligibility reasons (`is entry point (main)`, `has capabilities: Console`), not the
  -- predictable-profile gates. The old name was `profileGates`, and the message built from it
  -- claimed the function "passes the predictable profile" -- which stopped being true when
  -- profile admission became transitive, at which point `--check predictable` and
  -- `--report proof-status` printed opposite claims about the same function.
  eligibilityReasons : List String
  ineligCat    : Option IneligibleCategory  -- typed ineligibility classification
  /-- ADMISSION, kept separate from `status` because they answer different questions and
      one must not erase the other:

        extractable  the eligibility/extraction rules permit an obligation
        admissible   extractable AND every semantic admission gate passes
        replayable   extractable AND a claim/evidence link exists
        proved       replay passed AND admissible AND correspondence/dependency rules pass

      `status` is EXTRACTION-driven, so an admission failure leaves the claim visible,
      replayable and under drift monitoring — it simply stops counting toward admitted
      proof coverage. Deriving `status` from admission instead (as it did) meant an
      effect-classification change silently removed claims from replay and drift, which
      is the opposite of honest degradation: the artifact would vanish from maintenance
      at exactly the moment it needed to be tracked toward repair.

      A report may therefore say `replay: passed` and `admission: refused` at once. That
      is not a contradiction — the theorem artifact remains logically valid while it
      cannot currently justify the program-level claim. -/
  admissible   : Bool := true
  /-- Why admission was refused, when it was. Empty means admitted. -/
  admissionReasons : List String := []
  dependencies : List String      -- qualified names of proved callees
  /-- Reachable dependencies that are not current (stale / unbound / missing /
      blocked / ineligible). Named for what it holds: it was `staleDeps` while
      holding mostly `missing` entries, and the invariant checking it duly fired
      on correct output. -/
  notCurrentDeps    : List String
  /-- Reachable TRUSTED boundaries. Trusted counts as current for traversal — it
      is a declared, audited escape hatch — but it is an ASSUMPTION, and it has
      to travel with the claim. A Lean proof reaching a trusted boundary must not
      surface as an unqualified `proved_by_lean`, or the caller launders the
      trust: readers see a kernel-checked claim and cannot tell that part of it
      rests on an unproven boundary. -/
  trustedDeps  : List String := []
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
  /-- Link present, no stored proof-subject digest. Its own kind rather than
      `staleProof`: the obligation status is `unbound`, and the consistency
      invariant DIAG-STATUS requires the diagnostic to agree with it. Reusing
      `staleProof` here is what that invariant caught. -/
  | unboundProofLink
  /-- A reachable dependency is not current, so this claim cannot contribute
      `proved_by_lean` evidence. Its own kind for the same reason as
      `unboundProofLink`: the obligation status is `depsNotCurrent`, and
      DIAG-STATUS requires the diagnostic to agree with it. -/
  | dependencyNotCurrent
  /-- The dependency closure has no validated per-edge justification. Its own kind for the same
      reason as the two above: the obligation status is `correspondenceUnjustified`, DIAG-STATUS
      requires the diagnostic to agree with it, and reusing `dependencyNotCurrent` here made that
      invariant fire — while also telling an author that a callee needed re-verification when what
      was missing was this claim's own justification. -/
  | dependencyClosureUnjustified
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
  | .unboundProofLink     => "unbound"
  | .dependencyNotCurrent => "deps_not_current"
  | .dependencyClosureUnjustified => "correspondence_unjustified"

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
  | .unboundProofLink     => "E0810"
  | .dependencyNotCurrent => "E0811"
  | .dependencyClosureUnjustified => "E0812"

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
  | .unboundProofLink     => "unbound_proof_link"
  | .dependencyNotCurrent => "dependency_not_current"
  | .dependencyClosureUnjustified => "dependency_closure_unjustified"
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
  -- Not `theorem_update`: the theorem may be fine. What is missing is the
  -- recorded subject, and recording it requires re-verification first.
  | .unboundProofLink     => "record_proof_subject"
  -- The repair is downstream: make the dependency current. Nothing about THIS
  -- function needs changing, which is why it is not `record_proof_subject`.
  | .dependencyNotCurrent => "refresh_dependency"
  | .dependencyClosureUnjustified => "attest_dependency_closure"
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
  /-- The SCOPED identity for this definition, or a typed refusal.

      Constructed HERE, at the extraction boundary, where the package identity and the authoritative
      implementation digest are both in scope. Its implementation component is
      `implementationDigest` — the digest binding typed signature, capabilities, generics, contracts
      AND canonical body — not the V1 body digest, which binds only the body and would let a
      signature or contract change pass as the same definition.

      `Except` because an entry with incomplete facts or an unvalidated evidence body has no
      authoritative implementation digest, and therefore no scoped identity. That is a refusal to
      carry, not a value to default. -/
  definitionIdentity : Except Proof.DefinitionIdentityRefusal Proof.DefinitionIdentity
  qualName    : String
  bareName    : String
  /-- Semantic identity, minted HERE from resolved compiler facts — the defining
      module path and the checked declaration's own name — not reconstructed
      downstream by splitting `qualName`.

      A consumer that string-splits a qualified name has re-derived identity from
      a rendering, which is the drift `CallableId` exists to remove. The generator
      reads this field; it does not compute one. -/
  callableId  : CallableId
  /-- The facts Core erased, captured in Elab and threaded here rather than
      rebuilt. `none` only if the module carried no facts for this identity,
      which is itself a fault worth seeing rather than papering over. -/
  declFacts   : Option Proof.CheckedDeclFacts := none
  /-- The STRUCTURAL evidence body for this identity, threaded from Elab rather than
      rebuilt. Kept beside declFacts because the facts record cannot reference the tree
      (EvidenceTree imports SubjectFacts), and looked up by IDENTITY, never by position. -/
  evidenceBody : Option Proof.EvidenceBodyDraftV2 := none
  /-- The DEPENDENCY MATERIAL this entry can see: every constant the enclosing module
      binds, with a canonical encoding of its initializer. Carried per entry because the
      dependency axis is a property of the SUBJECT, and the subject is a declaration.

      Module-wide rather than pre-filtered to what this body references: filtering is the
      consumer's job, and pre-filtering here would make "referenced but unbound"
      indistinguishable from "not referenced". -/
  constBindings : List (ConstId × String) := []
  /-- `proofSubjectDigestV2` over those facts and the body fingerprint. `none`
      when the facts are absent or INCOMPLETE — an incomplete subject must not be
      representable as a digest string, or it becomes comparable as though it
      were complete. -/
  subjectDigest : Option String := none
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
  /-- The semantic identity, RETAINED. Excluding a function from the proof entries does not make
      it nameless: a trusted helper is still a real callable that other functions depend on.
      Dropping this was an identity loss at the earliest layer — the value was computed at the
      construction site and simply not stored — and it surfaced far downstream as an
      `«unresolved»` dependency edge on `composition_trusted_helper`, where a trusted callee
      became a `missing` edge instead of a `trusted` one. Fixed here rather than by teaching the
      edge layer to re-derive it, which would have been a second producer of an identity. -/
  callableId  : CallableId
  /-- The SCOPED identity, built by the same producer and the same rule as `ProofCoreEntry`'s.
      A trusted helper is excluded from the proof entries and is still a real callable other
      functions depend on: once the evidence join keys on `DefinitionIdentity`, an excluded callee
      without one cannot be keyed at all, and its dependents' edges would fall to a refusal for a
      reason that has nothing to do with them. `Except`, so a genuinely underivable identity is a
      named refusal rather than a fabricated value. -/
  definitionIdentity : Except Proof.DefinitionIdentityRefusal Proof.DefinitionIdentity
  fn          : CFnDef
  fingerprint : String
  eligibility : EligibilityEntry
  loc         : Option SourceLoc
  spec        : Option SpecAttachment

/-- The proof-oriented fragment of validated Core.
    This is the single artifact boundary between Core and the proof pipeline. -/
structure ProofCore where
  /-- The package this ProofCore's definitions belong to.

      REQUIRED, not defaulted. Every definition identity minted at this boundary is scoped by it, and
      a default would make definitions from unrelated packages compare equal — the collision the
      whole identity migration exists to close. A context that genuinely cannot supply one uses
      `extractProofCore?` and takes a typed refusal instead of a value. -/
  packageIdentity : Proof.PackageIdentity
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
  /-- Spec drift: the registry's spec name resolves to a `PExpr` (via
      `Concrete.Proof.specs`) that does NOT equal the source-extracted
      `PExpr`.  The theorem in the registry is about a different
      function than the source.  This is the spec-side counterpart of
      `staleFingerprint` (which catches the source-side equivalent).

      Only fires when the function appears in BOTH `Concrete.Proof.specs`
      and the registry.  A registry entry whose function is missing
      from `specs` is silently uncovered by this gate — that's by
      design: test programs and out-of-tree examples can register
      proofs by string name without forcing the compiler's spec table
      to know about them. -/
  | specDrift (entry : ProofRegistryEntry)
  /-- R-0004 containment (bug 058): an in-source `#[proof_by]` with no
      `#[proof_fingerprint]` has no STORED digest to compare against. Its
      `bodyFingerprint` was synthesized from the current body, so the freshness
      check compares that body with itself and can never fire — the claim would
      stay `proved` through any edit. A comparison whose two sides come from the
      same input is not a check, and must not be reported as one. -/
  | unboundProofSubject (entry : ProofRegistryEntry)
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
  | .specDrift _ => true            -- drifted spec invalidates the proof
  -- Warning at the REGISTRY-ISSUE level, matching `staleFingerprint`. Both are
  -- reported as errors in the proof-diagnostic stream (E0810 / E0800) and both
  -- block a release through `[policy] require-proofs`; neither should make an
  -- inspection command like `audit` refuse to run. Marking this an error made
  -- `audit` exit 1 on a project with unbound links — strictly harsher than the
  -- same command on a project with STALE proofs, which exits 0 — so you could
  -- not audit a project precisely when its evidence needed auditing.
  | .unboundProofSubject _ => false



/-- The versioned proof-subject digest, defined ONCE from the captured facts and
    the body fingerprint.

    This is what bugs 059 and 060 need and the legacy body fingerprint is not:
    that hashes statements only, so a whole-signature `i32 -> u32` change and a
    TRUE-versus-FALSE `#[ensures]` are both invisible to it.

    Two components, both required:
      * `CheckedDeclFacts.canonical` — identity, full typed signature, generics
        and bounds, capabilities, contracts, trust boundary. Captured in Elab
        BEFORE contract erasure, because Core drops them.
      * the VALIDATED STRUCTURAL V2 body (`bodyBytesV2` over a body with no gaps) — not the
        legacy Core-statement hash, which is the representation bugs 059/060 are filed against.
        An absent or refused body yields no digest at all.
      * the SELECTED SPECIFICATION's identity, since a proof established against one
        specification is not evidence for another.

    The facts' canonical form carries its own contract-COVERAGE flag, so a subject
    whose contracts could not be read cannot digest as one that has none.

    `v2:` is in the bytes. A stored `v1` (body-only) hash must therefore be
    recognised as a DIFFERENT SCHEMA rather than as a mismatch — the format
    changed, not necessarily the program, so such an entry is `needs_recheck`, not
    `stale`. Wiring that distinction into the freshness decision is the remaining
    step; this function only defines the value. -/
def proofSubjectDigestV2 (facts : Proof.CheckedDeclFacts)
    (evBody? : Option Proof.EvidenceBodyDraftV2)
    (selectedSpec? : Option String) (claimScope? : Option String) : Option String :=
  -- ENFORCED, not advisory. `isComplete` existed and nothing consulted it, so an
  -- uncovered subject — a contract the encoder could not read, an incomplete
  -- identity — still received an ordinary-looking digest. A digest that cannot be
  -- told from a complete one IS a claim of completeness, whatever a neighbouring
  -- flag says. Returning `none` makes the incomplete case unrepresentable rather
  -- than merely discouraged.
  if !facts.isComplete then none
  else match evBody? with
  -- No structural body threaded. Previously this position held the LEGACY body
  -- fingerprint, which hashes Core statements and is the very thing bugs 059/060
  -- are filed against; a subject built on it binds signature and contracts to a
  -- body representation weaker than the one the producer can already emit.
  -- Absent body is now `none` rather than a digest over nothing.
  | none => none
  | some d => match Proof.validate d with
    -- REFUSED bodies do not digest, for the reason `shadowBodyV2Line` refuses them:
    -- a body with gaps describes less than the program, and a digest over it is
    -- indistinguishable from one over a complete body. Fail-closed costs coverage
    -- (measured: 441 of 452 subjects have a complete structural body) and the
    -- alternative costs the meaning of the digest.
    | .error _ => none
    | .ok complete =>
      -- SELECTED SPECIFICATION is part of the subject: a proof established against one
      -- specification is not evidence for another, so changing which spec a function is
      -- proved against changes what was proved. Without it, re-pointing a `#[proof_by]` at a
      -- different theorem would leave the subject — and therefore the freshness verdict —
      -- untouched.
      --
      -- ABSENCE IS A VALUE, not a refusal. A function with no attached specification still
      -- has a perfectly good subject; it is simply unproved. Refusing here would conflate
      -- "nothing to prove it against yet" with "the subject cannot be described", which are
      -- different states with different repairs. It is rendered as a distinct marker rather
      -- than the empty string, so "no spec" cannot collide with a spec whose name is "".
      let specPart := match selectedSpec? with
        | some sp => "S" ++ toString sp.length ++ ":" ++ sp
        | none    => "S-none"
      -- CLAIM SCOPE (coverage): `iff`, `one_direction`, `invariant`, `full_contract`, … A proof
      -- covering ONE DIRECTION of a postcondition and a proof covering both are not the same
      -- claim about the same subject; without this, narrowing a claim from `iff` to
      -- `one_direction` would leave the subject — and the freshness verdict — untouched, which
      -- is the fail-open direction: less was proved and nothing said so.
      --
      -- Absent scope is a distinct marker, for the reason absent spec is: an unproved function
      -- has a subject, and refusing here would conflate "nothing claimed yet" with "cannot be
      -- described".
      let scopePart := match claimScope? with
        | some c => "C" ++ toString c.length ++ ":" ++ c
        | none   => "C-none"
      -- TWO IDENTITIES, sharing a preimage rather than nesting a hash.
      --
      -- The IMPLEMENTATION component — identity, typed signature, generics, capabilities,
      -- contracts, structural body — describes a callable independently of which proof or
      -- specification was selected for it. The SUBJECT adds the selected specification and claim
      -- scope, which describe what a particular proof LINK claims.
      --
      -- A function-table entry is an implementation, so table membership must bind the first and
      -- not the second: coupling membership to proof-link metadata would make a table entry
      -- change when a spec was re-pointed, which has nothing to do with the implementation.
      --
      -- The subject hashes the implementation PREIMAGE, not `implementationDigest`. Nesting the
      -- hash would change V2's bytes and break the freeze for a refactor that adds no
      -- information; sharing the preimage keeps every V2 value identical while making the
      -- implementation component reusable. The freeze gate is what proves that.
      some (shortHash (implementationPreimage facts complete
              ++ "|spec:" ++ specPart ++ "|scope:" ++ scopePart))


/-- The manifest attempt for a whole program: every entry accounted for, none dropped.

    REPLACED A `filterMap` 2026-08-12. That version silently omitted any entry it could not build a
    row for, so the result was a smaller manifest whose only record of how many rows there should
    have been was how many there were. The denominator is now recorded up front in `expected`, and
    each identity that produced no row carries a NAMED reason.

    EVERY entry is expected, deliberately, rather than pre-filtering to the ones that look
    tractable. Pre-filtering would reintroduce the same laundering one level up: the filter would
    define the denominator, and an entry excluded by it would be invisible rather than refused.

    Consequence, stated because it is the honest one: on a program where some entry lacks facts or an
    extracted body, `usable?` returns `none`. That is the true state — the manifest cannot account for
    every callable — and it is what the old producer concealed by returning the subset. -/
def implementationManifestResultOf (pc : ProofCore) : Proof.ManifestResult :=
  let step := fun (acc : Proof.ManifestResult) (e : ProofCoreEntry) =>
    let refuse := fun (why : Proof.ManifestRefusal) =>
      { acc with expected := acc.expected ++ [e.callableId],
                 refusals := acc.refusals ++ [(e.callableId, why)] }
    let expected := acc.expected ++ [e.callableId]
    match e.declFacts, e.evidenceBody with
    | none, _ => refuse .factsMissing
    | some _, none => refuse .evidenceMissing
    | some fx, some d =>
      if !fx.isComplete then refuse .factsIncomplete
      else match Proof.validate d with
        | .error _ => refuse .evidenceInvalid
        | .ok complete =>
          match e.extracted with
          | none => refuse .extractedMissing
          -- ALL FOUR PARTS PROJECTED FROM ONE ENTRY, in one expression. The record cannot verify
          -- that they share an origin — neither the evidence body nor the extracted expression
          -- carries identity — so the guarantee is structural here and named as a gap there.
          | some pe =>
            match CompleteImplementation.of? e.callableId fx complete pe with
            | none => refuse .inputsRefused
            | some ci => { acc with expected := expected, impls := acc.impls ++ [ci] }
  pc.entries.foldl (init := { expected := [], impls := [], refusals := [] }) step

/-- The manifest itself, or `none` when the program cannot be fully accounted for. -/
def implementationManifestOf (pc : ProofCore) : Option Proof.ImplementationManifest :=
  (implementationManifestResultOf pc).usable?

/-- The linked theorem for a function, or `""` when none. ONE producer: the correspondence input
    and the edge-kind derivation must agree about which theorem types a subject's dependencies, and
    two copies of this lookup is how they would stop agreeing. -/
def theoremNameOf (pc : ProofCore) (qualName : String) : String :=
  (pc.obligations.find? (fun o => o.functionId.qualName == qualName))
    |>.bind (·.spec) |>.map (·.proofName) |>.getD ""

/-- Dependency nodes over SEMANTIC IDENTITIES, built from ProofCore's own entries.

    R-0004 slice 6, step 4 — SHADOW. Produced here rather than in Report so both consumers read
    one set of nodes; two builders is how a second, weaker answer appears, which is the defect
    class this slice exists to close.

    **Every edge the compiler cannot classify is `unclassified`, and that is most of them.** The
    `contract` vs `body` split needs `classifyTheorem`, which reads a theorem's elaborated type
    in `MetaM`. The compiler can honestly mint `trusted` — a declared boundary is a compiler
    fact — and must not guess the rest. Since `dependencyRootMaterial` refuses any edge that is
    not current for dependents, most roots will REFUSE until the hand-back runs.

    That mass refusal is the expected first result, not a regression: coverage recovers by
    classifying more dependencies, never by weakening what a root requires.

    The call graph is name-keyed (`CallGraph = List (String × List String)`), which is the legacy
    representation slice 6 subordinates. Names are resolved to `CallableId` HERE and never
    escape into a node: a node keyed by name would reinstate exactly what R-0004 removes. A
    callee whose identity cannot be resolved is DROPPED from the edge list and the node is marked
    by its absence — it cannot be silently treated as an edge to nothing. -/
def dependencyNodesOf (pc : ProofCore) (graph : CallGraph) : List Proof.DepNode :=
  -- BOTH POPULATIONS. A callee excluded from the proof entries — a trusted helper, most often —
  -- still has a semantic identity, and resolving only against `entries` reported it as
  -- `«unresolved»`. Entries are consulted FIRST so an eligible function can never be shadowed by
  -- an excluded record of the same name.
  -- BOTH POPULATIONS, AND BOTH SCOPED. A callee excluded from the proof entries — a trusted helper,
  -- most often — is still a real definition, and it now carries the same scoped identity an entry
  -- does. Entries are consulted FIRST so an eligible function can never be shadowed by an excluded
  -- record of the same name.
  let scopedOf : String → Except Proof.DefinitionIdentityRefusal Proof.DefinitionIdentity := fun qn =>
    match pc.entries.find? (fun e => e.qualName == qn) with
    | some e => e.definitionIdentity
    | none   =>
      match pc.excluded.find? (fun x => x.qualName == qn) with
      | some x => x.definitionIdentity
      | none   => .error (.legacyNameOnly qn)
  let labelOf : String → CallableId := fun qn =>
    match (pc.entries.find? (fun e => e.qualName == qn)).map (·.callableId) with
    | some cid => cid
    | none => ((pc.excluded.find? (fun x => x.qualName == qn)).map (·.callableId)).getD
                (CallableId.ofUser "«unresolved»" qn)
  let trustedNames := pc.excluded.filterMap fun x =>
    if x.eligibility.isTrusted then some x.qualName else none
  -- A SUBJECT WITHOUT A SCOPED IDENTITY GETS NO NODE. It cannot be keyed, and a node keyed by name
  -- is exactly what this migration removes; `dependencyRootMaterial` then refuses `missingStart`
  -- for it, which is the honest answer — nothing about its closure was established.
  (pc.entries.filterMap fun e =>
    match e.definitionIdentity with
    | .error _ => none
    | .ok selfId =>
    let callees := (graph.find? (fun g => g.1 == e.qualName)).map (·.2) |>.getD []
    -- THE CALLER'S OWN THEOREM types its outgoing edges. `classifyTheorem` answers about a
    -- theorem: a `body` theorem depends on exact callee implementations, a `contract` theorem
    -- holds for any table meeting its hypotheses. That is a property of the PROOF, so it applies
    -- to every dependency that proof relies on.
    --
    -- No proof link means no classification, which is `unclassified` rather than a default. A
    -- trusted callee still overrides: a declared boundary is a compiler fact and does not depend
    -- on how the caller was proved.
    let thm := theoremNameOf pc e.qualName
    let callerEdge := if thm.isEmpty then Proof.DependencyEdge.unclassified
                      else Proof.classifiedEdgeOf thm
    -- EVERY call-graph edge is represented, and an unkeyable one is CARRIED rather than dropped.
    -- Dropping was fail-open in the worst way: the computed root then covers LESS than the actual
    -- dependency closure while looking like a complete answer. A dependency you cannot resolve is
    -- not a dependency you do not have.
    --
    -- The previous version synthesized a fake `CallableId` for the unresolved case so the refusal
    -- could name it. That is not available for a `DefinitionIdentity` — the constructor is private
    -- and there is nothing to forge from — which is a better outcome: the edge goes into `unscoped`,
    -- and `dependencyRootMaterial` refuses the root by naming it.
    let edges := callees.filterMap fun cn =>
      match scopedOf cn with
      | .ok d => some (if trustedNames.contains cn then Proof.DependencyEdge.trusted
                       else callerEdge, d)
      | .error _ => none
    let unscoped := callees.filterMap fun cn =>
      match scopedOf cn with
      | .ok _    => none
      | .error _ => some (labelOf cn, if trustedNames.contains cn then Proof.DependencyEdge.trusted
                                      else callerEdge)
    some { id := selfId, label := e.callableId, digest := e.subjectDigest
         , edges := edges, unscoped := unscoped }) ++
  -- EXCLUDED DEFINITIONS GET NODES TOO, and their absence was a real defect: an edge to a TRUSTED
  -- helper resolved to an identity with no node, so `dependencyRootMaterial` refused the whole
  -- closure with `unresolvedEdge`. `composition_trusted_helper`'s `calls.combine` is the measured
  -- case — a subject that corresponds perfectly and could not root, for a reason that has nothing to
  -- do with its evidence. Correspondence learned to consult both populations when the same defect hit
  -- it; the root layer never did.
  --
  -- THE DIGEST IS THE FINGERPRINT, because an excluded definition has no proof SUBJECT — that is
  -- what excluding it means — and a node needs a body-identifying digest to serialize. The
  -- fingerprint identifies the body, which is the question the root asks. It is not presented as a
  -- proof subject anywhere: `carriesTrust` still qualifies a closure that crosses a trusted
  -- boundary, and status containment independently refuses a caller reaching anything that is not
  -- current, so nothing is laundered by making the closure computable.
  pc.excluded.filterMap fun x =>
    -- ONLY A TRUSTED EXCLUSION MAY BE A LEAF BOUNDARY. Every excluded definition carries a scoped
    -- identity now, and giving them all nodes made closures computable over callees that are
    -- excluded for reasons carrying no evidence at all — ineligible ones, most of them. A trusted
    -- boundary is a DECLARED, audited escape hatch and `carriesTrust` qualifies any root that
    -- crosses it; an ineligible callee is simply unprovable, and a closure over it must refuse
    -- rather than serialize.
    --
    -- The containment pass would independently downgrade such a caller, because `ineligible` is not
    -- current for dependents — but relying on that would make the root's honesty a consequence of
    -- another check rather than a property of the root. Fail closed here, and let the two agree.
    if !x.eligibility.isTrusted then none else
    match x.definitionIdentity with
    | .error _ => none
    | .ok xid =>
      -- An excluded definition's own outgoing edges are not traversed: it has no theorem, so nothing
      -- types them. It is a LEAF in the evidence closure, which is exactly what a boundary is.
      some { id := xid, label := x.callableId, digest := some x.fingerprint
           , edges := [], unscoped := [] }

/-- The closed correspondence request for one subject, built from what the compiler actually has.

    REQUESTED EDGES are the subject's own dependency-node edges — the same node set the root reads,
    not a second derivation.

    CANDIDATE WITNESSES are derived from the subject's classification row: a row naming table `T`
    justifies an edge to each callee `T` actually contains, which is the question
    `scopedEntryEvidenceForTable` answers. A table that cannot be resolved contributes NO witnesses,
    so its edges fall to `missing` rather than being quietly justified — the fail-closed direction.

    SHADOW. Nothing consumes this. -/
def correspondenceInputOf (pc : ProofCore) (graph : CallGraph) (id : CallableId)
    : Except Proof.DefinitionIdentityRefusal Proof.CorrespondenceInput := do
  let nodes := dependencyNodesOf pc graph
  -- THE SUBJECT IS KEYED BY ITS SCOPED IDENTITY, and a subject without one cannot be corresponded
  -- at all. `Except` rather than a fabricated key: the whole point of the migration is that no
  -- evidence operation proceeds on a name.
  let subjEntry := pc.entries.find? (fun e => e.callableId == id)
  let subject ← match subjEntry with
    | none   => .error (.legacyNameOnly id.render)
    | some e => e.definitionIdentity
  -- ONE PRODUCER for a callee's scoped identity: entries first, then excluded records. Both now
  -- carry one, built by the same rule at the extraction boundary — a trusted helper is excluded
  -- from the proof entries and is still a real definition its dependents point at.
  -- The compiler-local NAME for a scoped identity, for diagnosis only. Looked up in both
  -- populations, and falling back to the identity's own local rendering when neither holds it.
  let labelFor : Proof.DefinitionIdentity → CallableId := fun d =>
    let match? := (pc.entries.find? (fun e =>
        match e.definitionIdentity with
        | .ok ed => ed.sameDefinition d
        | .error _ => false)).map (·.callableId)
    match match? with
    | some cid => cid
    | none =>
      match (pc.excluded.find? (fun x =>
        match x.definitionIdentity with
        | .ok xd => xd.sameDefinition d
        | .error _ => false)).map (·.callableId) with
      | some cid => cid
      | none => CallableId.ofUser d.moduleIdentity d.declarationIdentity
  -- THE NODE IS THE ONE PRODUCER, and it is already scoped: `dependencyNodesOf` resolves every
  -- callee's identity once. Re-resolving here would be a second answer to the same question.
  let node := nodes.find? (fun n => n.id.sameDefinition subject)
  let requested := (node.map (·.edges) |>.getD []).eraseDups.map (fun (k, tgt) =>
    ({ callee := tgt, label := labelFor tgt, kind := k } : Proof.RequestedEdge))
  -- AN EDGE WHOSE CALLEE HAS NO SCOPED IDENTITY IS CARRIED, NOT DROPPED, with the same refusal the
  -- root reports. Dropping it would leave every result set empty while the closure covered less
  -- than the compiler asked for.
  let unscoped := (node.map (·.unscoped) |>.getD []).map (fun (c, k) =>
    (c, k, Proof.DefinitionIdentityRefusal.legacyNameOnly c.render))
  let qual := subjEntry.map (·.qualName) |>.getD ""
  let thm := theoremNameOf pc qual
  -- A TRUSTED edge is justified by the DECLARED TRUST BOUNDARY, not by table membership. Its
  -- witness comes from the exclusion record that made it trusted — asking a classification row to
  -- justify it would be asking the wrong producer. The trust is a compiler fact and does not depend
  -- on how the caller was proved.
  let trustedWitnesses := requested.filterMap (fun r =>
    if r.kind == Proof.DependencyEdge.trusted then
      some ({ subject := subject, target := .edgeTo r.callee, kind := .trusted
            , source := "declared-trusted-boundary" } : Proof.EdgeWitness)
    else none)
  -- MEMBERSHIP IS SCOPED. The deleted `tableContainsCallee` asked whether a table held a callee by NAME;
  -- `scopedEvidenceContains` asks whether it holds an ATTESTED entry with that exact definition
  -- identity — package, module, declaration and implementation. This is where the attestations
  -- become load-bearing: a table that models a same-named function from another program no longer
  -- justifies this program's edge, and an unattested table justifies nothing at all because
  -- `scopedEntryEvidence` refuses it rather than returning a name-keyed answer.
  let tableWitnesses := match Proof.validatedRowOf thm with
    | .error _ => []
    | .ok row  => requested.filterMap (fun r =>
        if r.kind == Proof.DependencyEdge.trusted then none else
        if row.tables.any (fun (tn, _) =>
             match Proof.scopedEntryEvidenceForTable tn with
             | .ok rows => Proof.scopedEvidenceContains rows r.callee
             | .error _ => false)
        then some ({ subject := subject, target := .edgeTo r.callee, kind := row.edge, source := thm }
                    : Proof.EdgeWitness)
        else none)
  let witnesses := trustedWitnesses ++ tableWitnesses
  -- The refusals are collected HERE, where the tables are named, and travel in the input.
  -- Recomputing them at the report would be a second producer of the same fact.
  -- THE REFUSALS COME FROM THE SAME RESOLVER THE WITNESSES DO. They were collected through the
  -- UNSCOPED `entryEvidenceForTable` while witnesses were derived from the SCOPED one, and the two
  -- can disagree: a table whose entries read fine but whose ATTESTATIONS are missing or broken
  -- resolves unscoped and refuses scoped. With several tables named by one theorem, another table
  -- could then justify the edge while the broken attestation produced no refusal at all — so a
  -- subject could be `usable` with one of its named tables silently unexamined. Asking the scoped
  -- resolver closes that: whatever the witness derivation could not read is reported.
  let refusals := match Proof.validatedRowOf thm with
    | .error _ => []
    | .ok row  => row.tables.filterMap (fun (tn, _) =>
        match Proof.scopedEntryEvidenceForTable tn with
        | .error w => some w
        | .ok _    => none)
  .ok { subject := subject, requestedEdges := requested, unscopedEdges := unscoped
      , candidateWitnesses := witnesses, resolverRefusals := refusals }

/-- What a STORED `#[proof_fingerprint]` is worth against the program right now.

    THREE-VALUED, because two of these are not opposites. Under V2 a stored v1 value is neither
    fresh nor stale: it answers a weaker question (the body only) and cannot establish V2 freshness
    at all. Collapsing `notComparable` into `moved` would assert a body change that was never
    observed; collapsing it into `current` would accept a record that proves nothing.

    ONE PRODUCER, and it had to become one: the status deriver and the registry validator each
    carried their own copy of this comparison. Flipping only the deriver to V2 left the validator
    comparing a v2 stored value against a v1 fingerprint, so `elf_header` — a clean fixture, five
    proved claims — emitted five "stale fingerprint" warnings whose two sides were different kinds
    of digest. The activation exposed it; the duplication caused it. -/
inductive StoredFreshness where
  | current
  | moved
  | notComparable
  deriving BEq, Repr

def storedFreshness (stored : String) (subjectDigestV2 : Option String) : StoredFreshness :=
  if !("v2:".isPrefixOf stored) then .notComparable
  else match subjectDigestV2 with
    -- No computable digest is not agreement: incomplete facts must never read as freshness.
    | none => .moved
    | some d => if ("v2:" ++ shortHash d) == stored then .current else .moved

/-- Does this entry CLAIM a proof at all?

    A SPECIFICATION IS NOT A PROOF CLAIM, and conflating the two produced three wrong diagnostics at
    once. `synthesizeSourceLinks` builds a registry entry for any function carrying a proof-link
    attribute, and `#[spec(...)]` alone is one — so a function with a specification and no
    `#[proof_by]` became an entry with an empty proof name. The registry validator then reported it
    as malformed (`emptyProofName`), reported it as an unbound proof link telling the author to add
    `#[proof_fingerprint]` to a claim that does not exist, and the deriver reported `unbound`.

    The honest status is `missing`: a function nobody proved. That state is documented, reachable,
    and exactly what `proof_pressure.validate_header` is supposed to demonstrate after its wrong
    `#[proof_by]` was deleted — the repair that exposed all of this. -/
def ProofRegistryEntry.claimsProof (re : ProofRegistryEntry) : Bool := !re.proof.isEmpty

/-- Is the stored link STALE against the current subject — that is, did the thing it records move?

    LIFTED TO ONE DEFINITION because it was three. The status deriver, the registry validator and the
    consistency checker each carried a private copy of this comparison, which is survivable only while
    they all compare the same two things. The V2 activation broke that: the deriver moved to the
    subject digest while the consistency checker kept comparing the stored value against
    `functionId.fingerprint`, the V1 body fingerprint. It then accused nine `proved` obligations of
    disagreeing fingerprints — obligations whose reported status was correct. A consistency invariant
    that disagrees with the rule it audits is not a check, it is a second opinion presented as a
    defect, and the only durable fix is that there be nothing to disagree with. -/
def specIsStale (a : SpecAttachment) (currentFp : String)
    (subjectDigestV2 : Option String) : Bool :=
  match a.expectedHash with
  | some h => storedFreshness h subjectDigestV2 == StoredFreshness.moved
  | none   => a.expectedFp != currentFp

/-- Is the stored link NOT COMPARABLE against the current subject — a v1 record under a v2 authority?

    Neither fresh nor stale, and kept separate from `specIsStale` for that reason: collapsing it into
    staleness would assert a body change that was never observed. -/
def specIsNotComparable (a : SpecAttachment) (subjectDigestV2 : Option String) : Bool :=
  match a.expectedHash with
  | some h => storedFreshness h subjectDigestV2 == StoredFreshness.notComparable
  | none   => false

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
  -- Check for stale fingerprints. An in-source link with a stored
  -- `#[proof_fingerprint]` compares hash(currentFp) against that stored hash —
  -- this is how source-linked functions get staleness detection without a full
  -- fingerprint in source (their synthesized bodyFingerprint always equals the
  -- recomputed one, so the string compare below can never fire for them). A
  -- JSON entry (no expectedHash) keeps the full-string compare.
  let stales := registry.filterMap fun re =>
    match allFps.find? fun (f, _) => f == re.function with
    | some (_, currentFp) =>
      match re.expectedHash with
      -- THROUGH THE SHARED RULE. This site used to carry its own copy of the comparison, and after
      -- V2 activation it was comparing a v2 stored value against a v1 fingerprint — five "stale"
      -- warnings on a clean fixture whose two sides were different kinds of digest. Only `moved`
      -- is a stale WARNING: `notComparable` is already reported as `needsRecheck` by the status,
      -- and repeating it here would double-report one fact in two vocabularies.
      | some h =>
        let sd := (pc.entries.find? fun e => e.qualName == re.function).bind (·.subjectDigest)
        if storedFreshness h sd == StoredFreshness.moved then some (.staleFingerprint re currentFp)
        else none
      | none   =>
        -- No stored short hash. What that means depends on where the entry came
        -- from, and conflating the two is bug 058: a JSON entry carries a
        -- bodyFingerprint READ FROM THE FILE, so the full-string compare below
        -- is a real check; a source-linked entry had its bodyFingerprint
        -- SYNTHESIZED from the body being checked, so the same compare is the
        -- current body against itself and can never fail.
        -- ...and only when the entry CLAIMS a proof. A `#[spec(...)]` with no `#[proof_by]` has no
        -- link to be unbound, so reporting one told the author to pin a fingerprint for a proof
        -- that does not exist.
        if re.sourceLinked && re.claimsProof then some (.unboundProofSubject re)
        else if re.bodyFingerprint != currentFp then some (.staleFingerprint re currentFp)
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
  -- A SPEC-ONLY SOURCE LINK IS LEGITIMATE. An empty proof name is malformed for a JSON registry
  -- entry, which exists to record a proof; it is the normal shape for an entry synthesized from
  -- `#[spec(...)]` alone, which records a specification and asserts nothing about a proof. An entry
  -- claiming neither is still malformed.
  let emptyProofs := registry.filterMap fun re =>
    if re.proof.isEmpty && !(re.sourceLinked && !re.spec.isEmpty) then some (.emptyProofName re)
    else none
  let emptySpecs := registry.filterMap fun re =>
    if re.spec.isEmpty then some (.emptySpecName re) else none
  -- Spec-drift check (Phase 4 item 2): for each registry entry whose
  -- source extraction succeeded AND whose function is listed in
  -- `Concrete.Proof.specs`, compare the (already-normalized) extracted
  -- PExpr to the normalized registered spec PExpr.  Mismatch is an
  -- error; absence from `specs` is a warning.  Skip entries that are
  -- already stale (body_fingerprint mismatch) — those are reported by
  -- `staleFingerprint` and the user is already going to update both.
  let staleNames := stales.filterMap fun
    | .staleFingerprint re _ => some re.function
    | _ => none
  let specDrifts := registry.filterMap fun re =>
    if staleNames.contains re.function then none
    else match pc.entries.find? fun e => e.qualName == re.function with
      | none => none  -- excluded/blocked/unknown; covered elsewhere
      | some entry =>
        match entry.extracted with
        | none => none  -- extraction failed; covered by extractionBlocked
        | some extractedPExpr =>
          match Proof.specFor re.function with
          | none => none  -- not drift-covered; proof-status renders this state per entry
          | some specPExpr =>
            if extractedPExpr == normalizePExpr specPExpr then none
            else some (.specDrift re)
  unknowns ++ duplicates ++ conflicts ++ stales ++ ineligibles ++ blocked ++ emptyProofs ++ emptySpecs ++ specDrifts

/-- Render a registry validation issue as a diagnostic string. -/
def renderRegistryIssue : RegistryIssue → String
  | .unboundProofSubject re =>
    s!"error: proof link for '{re.function}' has no stored proof subject — #[proof_by] without #[proof_fingerprint] cannot detect a changed body (the freshness check would compare the current body with itself), so this claim is unbound, not proved. Add #[proof_fingerprint(\"...\")] after re-verifying"
  | .unknownFunction re =>
    s!"error: registry entry for unknown function '{re.function}' (function was removed or renamed — update or remove the registry entry)"
  | .renamedFunction re newName =>
    s!"error: registry entry for '{re.function}' appears renamed to '{newName}' (same fingerprint) — update the registry entry's function field to '{newName}'"
  | .duplicateEntry fn n =>
    s!"error: {n} duplicate registry entries for '{fn}'"
  | .conflictingEntry fn specs =>
    s!"error: conflicting specs for '{fn}': {", ".intercalate specs}"
  | .staleFingerprint re currentFp =>
    match re.expectedHash with
    | some h =>
      s!"warning: stale fingerprint for '{re.function}' (#[proof_fingerprint] \"{h}\" ≠ current \"{shortHash currentFp}\" — body changed since the proof was linked; re-verify and update the fingerprint)"
    | none =>
      s!"warning: stale fingerprint for '{re.function}' (registry: {re.bodyFingerprint.take 40}…, current: {currentFp.take 40}…)"
  | .ineligibleFunction re reasons =>
    s!"error: registry entry for ineligible function '{re.function}' ({", ".intercalate reasons})"
  | .emptyProofName re =>
    s!"error: registry entry for '{re.function}' has empty proof name"
  | .emptySpecName re =>
    s!"error: registry entry for '{re.function}' has empty spec name"
  | .extractionBlocked re unsupported =>
    s!"error: registry entry for '{re.function}' targets extraction-blocked function (unsupported: {", ".intercalate unsupported})"
  | .specDrift re =>
    s!"error: spec drift for '{re.function}' — registered spec '{re.spec}' (via Concrete.Proof.specs) does not match the source-extracted PExpr; the theorem '{re.proof}' is about a different function than the source"

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
      | .specDrift _ => ["spec drift"]
      | .unboundProofSubject _ => ["no stored proof subject"]
    let fn := match issue with
      | .unknownFunction re | .renamedFunction re _ | .staleFingerprint re _
      | .ineligibleFunction re _ | .emptyProofName re | .emptySpecName re
      | .extractionBlocked re _
      | .unboundProofSubject re
      | .specDrift re => re.function
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
           | .specDrift _ => "Update the Lean spec in Concrete/Proof.lean to match the source-extracted PExpr, or update the source to match the spec."
           | .unboundProofSubject _ => "Re-verify the proof against the current body, then record the result with #[proof_fingerprint(\"...\")]. Without a stored subject there is nothing for the freshness check to compare against."
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
    (opaqueSet : List String)
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
  -- Float arithmetic has no active proof profile (ProvableV1 is integer/bool
  -- only). A float param, return, or body op must NOT extract — otherwise float
  -- `+` is silently modeled as the integer `.add`. See ROADMAP "Provable Float
  -- V1". Detected from the type, which still exists at the Core level.
  let usesFloat := f.params.any (fun p => isFloatTy p.2) || isFloatTy f.retTy
    || (identifyUnsupported f.body).contains floatReason
  let profileReasons : List String :=
    (if rec_ != "none" && rec_ != "unclassified" then [s!"recursion ({rec_})"] else []) ++
    (if loopClass == "unbounded" || loopClass == "mixed" then ["unbounded loops"] else []) ++
    (if !allocs.isEmpty || concreteCaps.any (· == "Alloc") then ["allocation"] else []) ++
    (if crossesFfi then ["FFI"] else []) ++
    (if usesFloat then ["floating-point arithmetic has no active proof profile"] else []) ++
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
  , effectOpaque := opaqueSet.contains qualName
  , isTrusted := f.isTrusted, loc := fnLoc }

/-- Walk a module tree collecting eligibility + extraction for each function.
    This produces one ProofCoreEntry or ProofCoreExcluded per function. -/
private partial def extractModule
    (packageIdentity : Proof.PackageIdentity)
    (externNames : List String)
    (recMap : List (String × RecursionKind × List String))
    (opaqueSet : List String)
    (locMap : List (String × SourceLoc))
    (registry : ProofRegistry)
    (m : CModule) (modulePath : String := "")
    : List ProofCoreEntry × List ProofCoreExcluded :=
  let qualPrefix := if modulePath == "" then m.name else modulePath ++ "." ++ m.name
  let (entries, excluded) := m.functions.foldl (fun (accE, accX) f =>
    let qualName := qualPrefix ++ "." ++ f.name
    let bareName := f.name
    -- From the resolved module path and the checked declaration name, at the one
    -- point where both are facts rather than substrings. The type-parameter
    -- ARITY comes from the same declaration: without it, a generic extracted
    -- with its instantiation erased is indistinguishable from a non-generic
    -- callable, and one erased entry would answer for instantiations that do not
    -- agree (`i8` arithmetic wraps where `Int` does not). Carrying the arity
    -- makes `CallableId.isComplete` refuse it instead.
    let cid : CallableId := CallableId.ofUser qualPrefix f.name f.typeParams.length
    let fp := bodyFingerprint f.body
    -- The facts captured before contract erasure, looked up BY IDENTITY. They
    -- travel on the module as a parallel record, so nothing is recomputed here
    -- and nothing was added to CFnDef to carry them.
    -- Compare IDENTITIES. This compared renderings while the helper API in
    -- SubjectFacts was being corrected to compare identities — so the fix reached
    -- the function nobody calls and missed the one that mints every subject
    -- digest.
    let facts? := (m.declFacts.find? fun d => d.id == cid)
    let evBody? := (m.evidenceBodies.find? fun p => p.1 == cid).map Prod.snd
    -- `none` when the facts are absent OR incomplete. Never a string, so an
    -- absent subject cannot be compared as though it were a computed one.
    let elig := assessEligibility f qualName externNames recMap opaqueSet locMap
    let sa := resolveSpec qualName registry
    -- The spec IDENTITY, not the proof name: what the claim is about, rather than which Lean
    -- theorem happens to carry it. Re-pointing a link at a differently-named proof of the SAME
    -- specification is not a change of subject; the theorem's identity belongs in the receipt.
    let subjDigest : Option String :=
      -- Coverage comes from the registry entry rather than the attachment: `SpecAttachment`
      -- carries WHICH spec, the entry carries HOW MUCH of it the proof covers.
      let scope? := (registry.find? (fun re => re.function == qualName)).map (·.coverage)
      facts?.bind (fun fx => proofSubjectDigestV2 fx evBody? (sa.map (·.specId.name)) scope?)
    -- ONE PRODUCER for the scoped identity, used by both the entry and excluded paths. Written as a
    -- local so the excluded branches cannot drift from the entry rule — they did once already, when
    -- `callableId` was retained here and the identity was not.
    let scopedIdentityOf : Unit → Except Proof.DefinitionIdentityRefusal Proof.DefinitionIdentity :=
      fun _ =>
        match facts?, evBody? with
        | some fx, some d =>
          if !fx.isComplete then .error (.legacyNameOnly cid.render)
          else match Proof.validate d with
            | .error _ => .error (.legacyNameOnly cid.render)
            | .ok complete =>
              Proof.DefinitionIdentity.of? packageIdentity.digest cid.defModule cid.declName
                (implementationDigest fx complete)
        | _, _ => .error (.legacyNameOnly cid.render)
    if elig.isTrusted then
      (accE, accX ++ [{ qualName, bareName, callableId := cid
                       , definitionIdentity := scopedIdentityOf (), fn := f, fingerprint := fp
                       , eligibility := elig, loc := elig.loc
                       , spec := sa : ProofCoreExcluded }])
    else if elig.eligible then
      let extracted := cStmtsToPExpr f.body |>.map normalizePExpr
      -- Invariant: an eligible function whose extraction failed must ALWAYS
      -- disclose at least one reason, or the ProofCore self-consistency check
      -- fires [BLOCKED-UNSUP] "eligible with no extraction but unsupported list
      -- is empty". identifyUnsupported mirrors the extractor's structural cases,
      -- but the two can drift — e.g. a construct made "supported" in
      -- identifyUnsupported (match / struct literal / if-without-else, since
      -- 2026-05-23) while the extractor still rejects it in statement /
      -- non-terminal / nested-mutation position. Guard against that drift so we
      -- never silently report "eligible, no extraction, no reason".
      let unsup :=
        if extracted.isNone then
          match identifyUnsupported f.body with
          | [] => ["unmodelled statement or control-flow structure (no ProofCore form)"]
          | rs => rs
        else []
      -- THE SCOPED IDENTITY, from the AUTHORITATIVE implementation digest. Requires complete facts
      -- AND a validated evidence body: without both there is no authoritative digest, so there is no
      -- scoped identity and the entry carries the refusal instead. `implementationDigest` is used
      -- rather than the V1 body digest because the latter binds only the body — a signature,
      -- capability or contract change would otherwise read as the same definition.
      let definitionIdentity := scopedIdentityOf ()
      (accE ++ [{ definitionIdentity, qualName, bareName, callableId := cid,
                   declFacts := facts?, evidenceBody := evBody?,
                   constBindings := m.constBindings,
                   subjectDigest := subjDigest, fn := f, extracted
                 , unsupported := unsup
                 , fingerprint := fp, params := f.params.map Prod.fst
                 , eligibility := elig, loc := elig.loc
                 , spec := sa : ProofCoreEntry }], accX)
    else
      (accE, accX ++ [{ qualName, bareName, callableId := cid
                       , definitionIdentity := scopedIdentityOf (), fn := f, fingerprint := fp
                       , eligibility := elig, loc := elig.loc
                       , spec := sa : ProofCoreExcluded }])
  ) ([], [])
  -- Recurse into submodules
  let (subEntries, subExcluded) := m.submodules.foldl (fun (accE, accX) sub =>
    let (e, x) := extractModule packageIdentity externNames recMap opaqueSet locMap registry sub qualPrefix
    (accE ++ e, accX ++ x)) ([], [])
  (entries ++ subEntries, excluded ++ subExcluded)

/-- Derive obligation status from eligibility, trust, extraction, and spec attachment.

    `specDrifted = true` means the registered spec PExpr (from
    `Concrete.Proof.specs`) does NOT match the source-extracted
    PExpr.  A drifted spec invalidates the proof claim even when the
    body fingerprint still matches, so it's treated as `.stale`
    (the user must update the Lean spec or the source). -/
private def deriveObligationStatus
    (eligible : Bool) (isTrusted : Bool) (extracted : Bool)
    (specDrifted : Bool)
    (spec : Option SpecAttachment) (currentFp : String)
    -- V2 ACTIVATION (R-0004 package 3, 2026-08-17). The subject digest the stored value is now
    -- compared against. `none` means it is not computable — facts absent or incomplete — which
    -- cannot establish freshness and must not read as agreement.
    (subjectDigestV2 : Option String := none) : ObligationStatus :=
  -- An in-source link with a stored `#[proof_fingerprint]` compares hash(currentFp)
  -- against that hash; otherwise the full expected fingerprint is compared. This
  -- is what gives source-linked functions staleness detection (their expectedFp
  -- is recomputed from the current body, so the string compare can never fire).
  -- R-0004 containment (bug 058). A `.registry` attachment is synthesized from
  -- an in-source `#[proof_by]`, and when it carries no `#[proof_fingerprint]`
  -- its `expectedFp` was computed FROM THE BODY BEING CHECKED. The comparison
  -- below would then be the current body against itself: not a check, and it
  -- kept such claims `proved` through any edit. With no stored subject there is
  -- nothing to be fresh against, so the claim needs re-verification by
  -- construction — never `proved`. (`.hardcoded` attachments come from
  -- Proof.provedFunctions and carry a real expectedFp, so they keep the
  -- string compare.)
  let isUnbound := fun (a : SpecAttachment) =>
    a.source == .registry && a.expectedHash.isNone
  -- A STORED VALUE WITHOUT THE `v2:` PREFIX IS A V1 VALUE BY CONSTRUCTION, and the prefix is the
  -- discriminator precisely because nothing about the value's SHAPE distinguishes them: both are 16
  -- hex characters. A v1 value answers a weaker question — the body only — and cannot establish v2
  -- freshness, so it is NOT COMPARABLE rather than mismatched. That is `needsRecheck`, which is a
  -- claim about the RECORD, not about the program; reporting it as `stale` would assert a body
  -- change that was never observed.
  let storedIsV1 := fun (a : SpecAttachment) => specIsNotComparable a subjectDigestV2
  let isStale := fun (a : SpecAttachment) => specIsStale a currentFp subjectDigestV2
  if isTrusted then .trusted
  else if !eligible then
    match spec with
    -- INELIGIBILITY IS THE DOMINANT FACT for an excluded entry, and under V2 its freshness is not
    -- decidable at all: `ProofCoreExcluded` carries no subject digest, so the comparison has nothing
    -- to compare against. Reporting `stale` would assert a body change never observed, and
    -- `needsRecheck` would foreground a record issue over the reason the claim cannot be proved.
    -- `unbound` survives because it is about the LINK, not about freshness.
    | some a => if isUnbound a then .unbound else .ineligible
    | none => .ineligible
  else match spec with
  | some a =>
    -- Order matters, and spec drift comes first. Drift is AFFIRMATIVE evidence
    -- that the extracted body disagrees with the recorded spec; `unbound` is the
    -- ABSENCE of evidence about freshness. Reporting absence over a positive
    -- finding loses the stronger signal — it is why the `stale_proof` evidence
    -- exemplar, whose whole purpose is to demonstrate drift, briefly stopped
    -- demonstrating anything.
    if specDrifted then .stale
    -- A SPECIFICATION WITHOUT A PROOF NAME IS NOT A PROOF CLAIM, so there is no link to be unbound
    -- and nothing to be stale against. `missing` is the honest status: a function nobody proved.
    -- Ordered after drift because drift is affirmative evidence that the recorded spec disagrees
    -- with the body, which is worth reporting even when no proof was ever claimed.
    else if a.proofName.isEmpty then .missing
    -- Then unbound, reported as itself rather than as `stale`: with no stored
    -- subject the comparison below is the body against itself, so calling the
    -- result `stale` would claim a body change that was never observed.
    else if isUnbound a then .unbound
    -- BEFORE the staleness comparison, because the comparison cannot be performed at all against a
    -- v1 value. Ordering it after would run a v2 digest against a v1 hash and report the guaranteed
    -- mismatch as a body change.
    else if storedIsV1 a then .needsRecheck
    else if isStale a then .stale
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
  -- Build obligations for extracted (eligible) entries.
  -- specDrifted = registered spec from `Concrete.Proof.specs` doesn't
  -- match the extracted PExpr (when both exist).
  let entryObls := entries.map fun e =>
    let extracted := e.extracted.isSome
    let specDrifted := match e.extracted, Proof.specs.find? fun (n, _) => n == e.qualName with
      | some extractedPExpr, some (_, specPExpr) =>
        extractedPExpr != normalizePExpr specPExpr
      | _, _ => false  -- no extracted or no registered spec → no drift detectable here
    -- EXTRACTABLE drives status; admission is carried separately below. See
    -- `Obligation.admissible` for why the two must not be collapsed.
    let status := deriveObligationStatus e.eligibility.eligible
        e.eligibility.isTrusted extracted specDrifted e.spec e.fingerprint e.subjectDigest
    let cat := if status == .ineligible
      then some (classifyIneligible e.eligibility.sourceReasons e.eligibility.profileReasons)
      else none
    { functionId := { qualName := e.qualName, fingerprint := e.fingerprint }
    , bareName := e.bareName
    , status
    , spec := e.spec
    , expectedFp := match e.spec with | some a => a.expectedFp | none => ""
    , eligibilityReasons := e.eligibility.sourceReasons ++ e.eligibility.profileReasons
        -- R-0484: opacity refuses ADMISSION without touching `eligible`, so it is not in
        -- either reason list. It still has to be NAMED when it bites: a refusal that
        -- reports no reason is the defect this task exists to remove, not a smaller
        -- version of it.
        --
        -- Conditioned on it ACTUALLY changing the verdict, not merely on being true.
        -- Listing opacity beside an unrelated refusal — `main` is excluded for being the
        -- entry point — presents a fact that contributed nothing as though it were a
        -- cause, so a reason is recorded only where it is the reason.
        ++ (if e.eligibility.effectOpaque && e.eligibility.eligible
               && !e.eligibility.admissible then
              ["effects may enter through an indirect call (authority supplied by a handle is not visible in the header)"]
            else [])
    , ineligCat := cat
    , admissible := e.eligibility.admissible
    , admissionReasons :=
        if e.eligibility.effectOpaque && e.eligibility.eligible
           && !e.eligibility.admissible then
          ["effects may enter through an indirect call (authority supplied by a handle is not visible in the header)"]
        else []
    , dependencies := []  -- filled in second pass
    , notCurrentDeps := []
    , trustedDeps := []
    , loc := e.loc : Obligation }
  -- Build obligations for excluded entries (never extracted).
  -- Excluded functions have no extracted PExpr to compare, so
  -- specDrifted is always false here.
  let exclObls := excluded.map fun e =>
    let status := deriveObligationStatus e.eligibility.eligible
        e.eligibility.isTrusted false false e.spec e.fingerprint
    let cat := if status == .ineligible
      then some (classifyIneligible e.eligibility.sourceReasons e.eligibility.profileReasons)
      else none
    { functionId := { qualName := e.qualName, fingerprint := e.fingerprint }
    , bareName := e.bareName
    , status
    , spec := e.spec
    , expectedFp := match e.spec with | some a => a.expectedFp | none => ""
    , eligibilityReasons := e.eligibility.sourceReasons ++ e.eligibility.profileReasons
        -- R-0484: opacity refuses ADMISSION without touching `eligible`, so it is not in
        -- either reason list. It still has to be NAMED when it bites: a refusal that
        -- reports no reason is the defect this task exists to remove, not a smaller
        -- version of it.
        --
        -- Conditioned on it ACTUALLY changing the verdict, not merely on being true.
        -- Listing opacity beside an unrelated refusal — `main` is excluded for being the
        -- entry point — presents a fact that contributed nothing as though it were a
        -- cause, so a reason is recorded only where it is the reason.
        ++ (if e.eligibility.effectOpaque && e.eligibility.eligible
               && !e.eligibility.admissible then
              ["effects may enter through an indirect call (authority supplied by a handle is not visible in the header)"]
            else [])
    , ineligCat := cat
    , dependencies := []
    , notCurrentDeps := []
    , trustedDeps := []
    , loc := e.loc : Obligation }
  let allObls := entryObls ++ exclObls
  -- Second pass: dependencies, and — R-0004 slice 3 — dependency CONTAINMENT.
  --
  -- `notCurrentDeps` used to be filled here and never consulted, so a function whose
  -- proof rested on a stale one reported `proved` at one hop, and at two hops
  -- did not even list it (bug 062). Two things were missing: the closure, and
  -- any effect on status. Both are below.
  let statusOf : String → Option ObligationStatus := fun n =>
    (allObls.find? fun o => o.functionId.qualName == n).map (·.status)
  let directCalleesOf : String → List String := fun n =>
    match graph.find? fun (m, _) => m == n with
    | some (_, cs) => cs
    | none => []

  -- Reachable closure by worklist. `fuel` is the node count: each iteration
  -- either marks a new node or drops one, so this bound cannot be reached by a
  -- terminating walk — and a RECURSIVE or mutually-recursive chain simply stops
  -- instead of diverging. Slice 6 replaces this with a deterministic SCC/Merkle
  -- root; until then a conservative closure is what the roadmap asks for, and it
  -- must terminate on the recursive case rather than be assumed acyclic.
  let reachableFrom : String → List String := fun start =>
    let rec go (fuel : Nat) (frontier : List String) (seen : List String) : List String :=
      match fuel, frontier with
      | 0, _ => seen          -- fail-closed: whatever was found so far still counts
      | _, [] => seen
      | fuel + 1, n :: rest =>
        if seen.contains n then go fuel rest seen
        else go fuel (rest ++ directCalleesOf n) (n :: seen)
    -- Start from the callees, not from `start` itself: a self-recursive function
    -- must not be reported as its own not-current dependency.
    go (allObls.length * allObls.length + allObls.length + 1)
       (directCalleesOf start) []

  -- Over the CLOSURE, and excluding `self` so recursion is not self-blame.
  -- A name with no obligation (an extern, a builtin, an unresolved callee) is
  -- skipped rather than treated as not-current: this pass reports on evidence it
  -- has, and inventing a verdict for an unknown node would make every
  -- FFI-adjacent proof fail for the wrong reason.
  let notCurrentOf : String → List String := fun self =>
    (reachableFrom self).filter fun c =>
      c != self && (match statusOf c with
                    | some st => !st.isCurrentForDependents
                    | none    => false)
  -- Final statuses FIRST. Only a claim that would otherwise be `proved` is
  -- downgraded: a function already stale/unbound/missing keeps its own, more
  -- specific verdict, since `depsNotCurrent` would replace a fact about this
  -- body with a fact about someone else's.
  --
  -- One pass suffices, and that is a property of the closure rather than luck:
  -- if X reaches Y and Y is downgraded because Y reaches a non-current Z, then X
  -- reaches Z too, so X is downgraded by Z directly. Iterating to a fixpoint
  -- would find nothing new.
  let finalStatus : String → ObligationStatus := fun n =>
    match statusOf n with
    | some .proved => if (notCurrentOf n).isEmpty then .proved else .depsNotCurrent
    | some st => st
    | none => .missing
  let trustedOf : String → List String := fun self =>
    (reachableFrom self).filter fun c =>
      c != self && (match statusOf c with
                    | some .trusted => true
                    | _ => false)
  allObls.map fun o =>
    let self := o.functionId.qualName
    -- `dependencies` means "proved callees" and must read the FINAL status: a
    -- callee downgraded by this very pass is no longer proved, and listing it
    -- here tripped INV-9 (DEP-PROVED) while also telling readers a contained
    -- claim was evidence.
    let provedCallees := (directCalleesOf self).filter fun c => finalStatus c == .proved
    { o with dependencies := provedCallees
           , notCurrentDeps := notCurrentOf self
           , trustedDeps := trustedOf self
           , status := finalStatus self }

/-- Generate proof diagnostics from obligations and extraction results. -/
private def generateDiagnostics
    (obligations : List Obligation)
    (entries : List ProofCoreEntry) : List ProofDiagnostic :=
  -- Diagnostics from obligation status
  let oblDiags := obligations.filterMap fun o =>
    let qn := o.functionId.qualName
    let fp := o.functionId.fingerprint
    match o.status with
    | .unbound =>
      some { kind := .unboundProofLink, severity := .error, function := qn
           , message := s!"proof link unbound: no stored proof-subject digest for `{qn}`."
           , hint := "Re-verify the proof against the current body and record the result as #[proof_fingerprint(\"...\")]. Until a subject is stored there is nothing for the freshness check to compare against, so this claim is unbound — not proved, and not stale either: the body has not been shown to change, it has never been pinned."
           , details := ["no stored proof-subject digest"], failureClass := failureClassOf .unboundProofLink
           , repairClass := repairClassOf .unboundProofLink
           , fingerprint := fp, expectedFp := "", loc := o.loc }
    | .needsRecheck =>
      -- Deliberately NOT reported as `staleProof`. That diagnostic says "the body
      -- changed", which this has not established: the stored digest was written
      -- under the v1 schema (Core statements only) and simply cannot answer the
      -- v2 question (identity, typed signature, generics, capabilities,
      -- contracts). The repair is re-verification, not "restore the proved
      -- implementation" — a hint that would send the author looking for a change
      -- that may not exist.
      some { kind := .unboundProofLink, severity := .error, function := qn
           , message := s!"`{qn}` has a proof link recorded under an earlier digest schema; it must be re-verified."
           , hint := "The stored #[proof_fingerprint] is a v1 (body-only) digest and covers no signature, capability or contract. It cannot establish freshness against the v2 subject digest, so this claim is neither proved nor shown stale. Re-verify and record the new v2 value."
           , details := ["stored digest is v1 (body-only); current schema is v2"]
           , failureClass := failureClassOf .unboundProofLink
           , repairClass := repairClassOf .unboundProofLink
           , fingerprint := fp, expectedFp := "", loc := o.loc }
    | .depsNotCurrent =>
      some { kind := .dependencyNotCurrent, severity := .error, function := qn
           , message := s!"`{qn}` cannot contribute proved evidence: a dependency it reaches is not current."
           , hint := "This function's own subject is fresh; the problem is downstream. Make the listed dependencies current (re-verify and update their fingerprints, attach their missing proofs, or mark a boundary trusted), then this claim recovers on its own."
           , details := o.notCurrentDeps.map (fun d => s!"reaches `{d}`, which is not current")
           , failureClass := failureClassOf .dependencyNotCurrent
           , repairClass := repairClassOf .dependencyNotCurrent
           , fingerprint := fp, expectedFp := "", loc := o.loc }
    | .correspondenceUnjustified =>
      some { kind := .dependencyClosureUnjustified, severity := .error, function := qn
           , message := s!"`{qn}` cannot contribute proved evidence: its dependency closure has no validated per-edge justification."
           , hint := "This function's own subject is fresh and its dependencies are current; what is missing is the JUSTIFICATION. Every compiler edge in the closure must be witnessed by exactly one entry naming that exact implementation — package, module, declaration and body — in a table the linked theorem names. Attest the table's entries, or repair a proof link that names a table describing a different program's function."
           , details := o.notCurrentDeps.map (fun d => s!"edge to `{d}` has no validated justification")
           , failureClass := failureClassOf .dependencyNotCurrent
           , repairClass := repairClassOf .dependencyNotCurrent
           , fingerprint := fp, expectedFp := "", loc := o.loc }
    | .stale =>
      some { kind := .staleProof, severity := .error, function := qn
           , message := s!"`{qn}` has a registered proof, but the body changed."
           , hint := "Update the Lean proof to match the current body, or restore the proved implementation."
           , details := [], failureClass := failureClassOf .staleProof
           , repairClass := repairClassOf .staleProof
           , fingerprint := fp, expectedFp := o.expectedFp, loc := o.loc }
    | .missing =>
      some { kind := .missingProof, severity := .warning, function := qn
           -- NOT "passes the predictable profile". That phrase now means something different:
           -- profile admission is TRANSITIVE (a caller of a recursive function fails it), while
           -- proof eligibility is per-BODY and deliberately stays that way -- calling a recursive
           -- function does not stop this function's own obligations from being provable. Two
           -- notions, one phrase, and `--check predictable` and this report disagreed out loud:
           -- `caller` was reported as PASSING the profile here while the gate rejected it.
           -- NOR "is eligible for proof", for a second and separate reason. `eligible`
           -- means only that an obligation can be EXTRACTED; a reader takes "eligible for
           -- proof" to mean the claim would be ADMITTED, which is the distinction R-0484
           -- separated. This diagnostic is a second producer of the fact `--report
           -- proof-status` renders, so it must speak that report's vocabulary or the two
           -- disagree in the same way `--check predictable` once did.
           , message := s!"`{qn}` has an extractable obligation and no registered proof."
           , hint := "Add a Lean proof for this function with the current fingerprint."
           , details := [], failureClass := failureClassOf .missingProof
           , repairClass := repairClassOf .missingProof
           , fingerprint := fp, expectedFp := "", loc := o.loc }
    | .ineligible =>
      let det := o.eligibilityReasons
      some { kind := .ineligible, severity := .info, function := qn
           , message := s!"`{qn}` cannot be proved: fails the proof-eligibility gates."
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
  -- Diagnostics from unsupported constructs (extractable, but extraction blocked).
  --
  -- GATED ON EXTRACTABLE, NOT ADMISSIBLE. This read `e.eligibility.admissible` while
  -- obligation status was ALSO derived from admission: a non-admissible function had
  -- status `ineligible`, and DIAG-STATUS requires the diagnostic to agree with the
  -- status, so emitting "blocked" would have contradicted it — which the self-consistency
  -- check caught on `Scheduler.execute_task`.
  --
  -- That premise died when status moved to EXTRACTABLE (2026-09-26). A function that is
  -- extractable, effect-opaque and blocked by an unsupported construct now has a status
  -- that is NOT `ineligible`, so the agreement argument no longer licenses suppression —
  -- and suppressing it means an author with a real unsupported construct is told nothing
  -- at all. Losing the diagnostic is a worse failure than the wording it was avoiding.
  --
  -- The wording objection was right and is answered directly instead: "eligible but uses
  -- unsupported constructs" did imply the construct was the only thing in the way, which
  -- is false when the function would be refused admission anyway. The message now claims
  -- only what it knows — extraction is blocked — and the admission verdict is carried
  -- beside it rather than folded into it.
  let unsupDiags := entries.filterMap fun e =>
    if e.extracted.isNone && !e.unsupported.isEmpty && e.eligibility.eligible then
      some { kind := .unsupportedConstruct, severity := .error, function := e.qualName
           , message := s!"`{e.qualName}` has an extractable obligation, but extraction failed on unsupported constructs."
           , hint := s!"Remove {", ".intercalate e.unsupported} to enable extraction."
           , details := e.unsupported, failureClass := failureClassOf .unsupportedConstruct
           , repairClass := repairClassOf .unsupportedConstruct
           , fingerprint := e.fingerprint, expectedFp := ""
           , loc := e.loc }
    else none
  oblDiags ++ unsupDiags

/-- THE AUTHORITY PASS: a friendly verdict must survive its own dependency JUSTIFICATION.

    R-0004 package 2, and the point at which the scoped join stops being shadow. A claim may be
    `proved` only if every compiler edge in its dependency closure is witnessed exactly once by an
    entry naming that exact implementation — package, module, declaration and body — in a table its
    linked theorem names. Freshness of this body and currency of its callees are necessary and were
    never sufficient: that gap is what let a shared proof table justify another program's edges on
    name agreement alone, measured on `elf_header/main_drifted`.

    APPLIED AFTER the dependency-currency downgrade, deliberately. A stale callee is a more specific
    and more actionable fact than an unjustified closure, so a claim that is both reports the callee
    — sending the author to a fingerprint rather than to the evidence.

    A SEPARATE PASS rather than a line inside `generateObligations`, because the correspondence input
    is built from the assembled `ProofCore` and the obligations are generated before it exists.
    Inlining a second, lighter correspondence derivation there would be two answers to one question.

    ITS LIVE CASE IS `tests/programs/proof_decode_header.con`, whose hardcoded proof link names
    `proofFnsExt` — a table with ZERO canonical entries. A table with no entries can describe no
    definitions, so nothing states that the theorem is about THAT program's `parse_byte`, and both
    body edges are unjustified. Measured across the fixture corpus: 35 proved, 0 unjustified.

    THIS COMMENT PREVIOUSLY CLAIMED SOMETHING FALSE, TWICE, and the corrections are kept because the
    reasoning matters more than the conclusion.

    First it said the pass would change no verdict, because a corpus survey pattern matched
    `-- proof link unbound` and `-- proof stale` but not `-- proved` — no space after "proof" — and
    read "nothing is currently proved".

    Then it credited the pass with catching a cross-program substitution on
    `elf_header/main_drifted.con`. It had not. That file sat beside `main.con` in ONE package and both
    resolve to module `main`, so the project loader picked `main.con` and the drifted bodies were
    never analyzed at all; the refusal came from package identity being synthesized from one file's
    TEXT while another file's BODIES were analyzed. Both drift fixtures now live in their own packages
    and are caught by STALENESS — a more specific fact that wins ordering — so this pass never sees
    them. The cross-program discrimination is real and measured elsewhere: the drifted package's
    `validate_header` corresponds 0/4 while the real one corresponds 4/4. -/
def applyCorrespondenceAuthority (pc : ProofCore) (graph : CallGraph) : ProofCore :=
  -- ROOT MATERIAL IS THE SECOND DIMENSION, and it answers a different question from correspondence.
  -- Corresponding says every edge has exactly one validated justification; rooting says the closure
  -- can be COMPUTED at all — no duplicate identity, no absent subject digest, no edge that is not
  -- current for dependents, no callee without a node, no unkeyable edge. A subject can correspond
  -- while its closure cannot be serialized, and a receipt over such a closure would bind material
  -- that was never assembled.
  --
  -- ON TODAY'S CORPUS THIS CONJUNCT REFUSES NOTHING THAT CORRESPONDENCE DOES NOT ALREADY REFUSE, and
  -- saying so is the point: it is consumed, and it is currently redundant. Measured — with the root
  -- requirement the verdict census is 37 proved / 1 unjustified, identical to correspondence alone.
  -- It is wired anyway because the conditions it checks are ones correspondence never asks about,
  -- and the alternative is adding the check after the first receipt is minted over a closure that
  -- could not be assembled. Its refusal conditions are controlled synthetically in
  -- `check_dependency_edges.sh`; the redundancy itself is pinned there too, so a corpus that starts
  -- exercising it is noticed rather than absorbed.
  let rooted : ProofCoreEntry → Bool := fun e =>
    match e.definitionIdentity with
    | .error _ => false
    | .ok d    => (Proof.dependencyRootMaterial (dependencyNodesOf pc graph) d).toOption.isSome
  let justified : ProofCoreEntry → Bool := fun e =>
    match correspondenceInputOf pc graph e.callableId with
    -- A SUBJECT WITH NO SCOPED IDENTITY CANNOT BE JUSTIFIED. Fail closed: nothing about its closure
    -- was established, and a friendly verdict over it would rest on material never examined.
    | .error _ => false
    | .ok i =>
      -- No outgoing edges means a vacuously justified closure — there is nothing to justify — and
      -- must not be downgraded. `usable` compares the matched COUNT against the requested count, so
      -- the empty case is only vacuous here, never a way to pass with dropped requests.
      if i.requestedEdges.isEmpty && i.unscopedEdges.isEmpty then true
      else (Proof.correspond i).usable i.requestedEdges.length
  -- STEP 1: downgrade the claims whose own justification fails.
  let downgraded := pc.obligations.map fun o =>
    if o.status != .proved then o else
    match pc.entries.find? (fun e => e.qualName == o.functionId.qualName) with
    | none   => o
    | some e => if justified e && rooted e then o else { o with status := .correspondenceUnjustified }
  -- STEP 2: PROPAGATE. A caller reaching a newly-unjustified callee is no longer proved either, and
  -- without this pass it stayed `proved` over a dependency that had just stopped being current —
  -- the exact containment defect (bug 062) that `depsNotCurrent` was introduced to close, reopened
  -- from a new direction because the downgrade happened after the containment pass had already run.
  --
  -- ONE ROUND SUFFICES, for the same reason it does in `generateObligations`: if X reaches Y and Y
  -- is downgraded because it reaches a non-current Z, then X reaches Z too and is downgraded by Z
  -- directly.
  let statusOf : String → Option ObligationStatus := fun n =>
    (downgraded.find? fun o => o.functionId.qualName == n).map (·.status)
  let directCalleesOf : String → List String := fun n =>
    match graph.find? fun (m, _) => m == n with
    | some (_, cs) => cs
    | none => []
  let reachableFrom : String → List String := fun start =>
    let rec go (fuel : Nat) (frontier acc : List String) : List String :=
      match fuel with
      | 0 => acc
      | Nat.succ f =>
        let next := (frontier.flatMap directCalleesOf).eraseDups
        let fresh := next.filter (fun n => !acc.contains n)
        if fresh.isEmpty then acc else go f fresh (acc ++ fresh)
    go (downgraded.length + 1) [start] []
  let notCurrentOf : String → List String := fun self =>
    (reachableFrom self).filter fun c =>
      c != self && (match statusOf c with
                    | some st => !st.isCurrentForDependents
                    | none    => false)
  let propagated0 := downgraded.map fun o =>
    if o.status != .proved then o else
      let nc := notCurrentOf o.functionId.qualName
      if nc.isEmpty then o else { o with status := .depsNotCurrent, notCurrentDeps := nc }
  -- AND `dependencies` IS RECOMPUTED, because it means "proved callees" and was computed before this
  -- pass ran. Left stale it listed a callee this pass had just downgraded, which is both a false
  -- statement to a reader and an INV-9 (DEP-PROVED) violation — a contained claim presented as
  -- evidence, which is exactly what that invariant exists to catch.
  let finalStatusOf : String → Option ObligationStatus := fun n =>
    (propagated0.find? fun o => o.functionId.qualName == n).map (·.status)
  let propagated := propagated0.map fun o =>
    { o with dependencies := (directCalleesOf o.functionId.qualName).filter fun c =>
        finalStatusOf c == some .proved }
  -- STEP 3: REGENERATE DIAGNOSTICS from the final statuses. They were produced before this pass ran,
  -- so `--report proof-diagnostics` reported ZERO errors for a subject `--report proof-status`
  -- called unjustified, and the consistency check saw OBL-STATUS and DEP-PROVED violations between
  -- two surfaces reading the same artifact. Regenerating here is what makes them one answer.
  let regenerated := generateDiagnostics propagated pc.entries
  -- Registry-derived diagnostics are NOT regenerated: they are about attachment integrity, not
  -- status, and re-deriving them would need the registry this function does not take. They are
  -- preserved by keeping every diagnostic whose kind `generateDiagnostics` does not emit.
  let statusKinds := (generateDiagnostics pc.obligations pc.entries).map (·.kind) |>.eraseDups
  let preserved := pc.diagnostics.filter fun d => !statusKinds.contains d.kind
  { pc with obligations := propagated, diagnostics := regenerated ++ preserved }

/-- Extract the proof-oriented fragment from validated Core.
    This is the primary entry point for the proof pipeline. -/
def extractProofCore (vc : ValidatedCore) (packageIdentity : Proof.PackageIdentity)
    (locMap : List (String × SourceLoc) := [])
    (registry : ProofRegistry := [])
    : ProofCore :=
  let modules := vc.coreModules
  let allModules := List.flatten (modules.map flattenModules)
  -- Precompute shared analysis
  let graph := buildCallGraph modules
  let sccs := tarjanSCC graph
  let recMap := classifyRecursion graph sccs
  let opaqueSet := effectOpaqueSet modules graph
  let externNames := modules.foldl (fun acc m => acc ++ collectExternNames m) []
  -- Extract entries and excluded (with spec attachment)
  let (entries, excluded) := modules.foldl (fun (accE, accX) m =>
    let (e, x) := extractModule packageIdentity externNames recMap opaqueSet locMap registry m
    (accE ++ e, accX ++ x)) ([], [])
  -- Generate proof obligations and diagnostics
  let obligations := generateObligations entries excluded graph
  let oblDiags := generateDiagnostics obligations entries
  -- Generate attachment-integrity diagnostics from registry validation
  let regIssues := validateRegistry
    { entries, excluded, structs := [], enums := [], traitDefs := []
    , callGraph := graph, recMap, externNames, obligations := [], diagnostics := []
    , packageIdentity }
    registry
  let regDiags := registryIssuesToDiagnostics regIssues locMap
  let diagnostics := oblDiags ++ regDiags
  -- Collect eligible types
  let sts := List.flatten (allModules.map (·.structs)) |>.filter CStructDef.isProofEligible
  let ens := List.flatten (allModules.map (·.enums)) |>.filter CEnumDef.isProofEligible
  let tds := List.flatten (allModules.map (·.traitDefs))
  -- THE AUTHORITY PASS RUNS ON THE ASSEMBLED ARTIFACT, so every consumer of a `ProofCore` sees
  -- verdicts that already survived their dependency justification. Applying it in a renderer would
  -- put the authority in one surface and leave the others reporting the unchecked verdict.
  applyCorrespondenceAuthority
    { entries, excluded, structs := sts, enums := ens, traitDefs := tds
    , callGraph := graph, recMap, externNames, obligations, diagnostics, packageIdentity }
    graph

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

/-- Why a ProofCore could not be extracted. -/
inductive ProofIdentityRefusal where
  /-- No package identity could be formed for this compilation, so no definition in it can be
      scoped. Carries the underlying reason rather than restating it. -/
  | noPackageIdentity (why : Proof.PackageIdentityRefusal)
deriving Repr, BEq

def ProofIdentityRefusal.explain : ProofIdentityRefusal → String
  | .noPackageIdentity w => s!"no package identity: {w.explain}"

/-- Extract a ProofCore, or refuse because no package identity is available.

    The entry point for contexts that cannot guarantee an identity. Deliberately separate from
    `extractProofCore` rather than folded into it with a default: a caller that has an identity
    should not be able to accidentally proceed without one, and a caller that lacks one should have
    to say so. -/
def extractProofCore? (vc : ValidatedCore)
    (packageIdentity : Except Proof.PackageIdentityRefusal Proof.PackageIdentity)
    (locMap : List (String × SourceLoc) := [])
    (registry : ProofRegistry := [])
    : Except ProofIdentityRefusal ProofCore :=
  match packageIdentity with
  | .error w => .error (.noPackageIdentity w)
  | .ok pkg  => .ok (extractProofCore vc pkg locMap registry)

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
      let specDrifted := match e.extracted, Proof.specs.find? fun (n, _) => n == qn with
        | some extractedPExpr, some (_, specPExpr) =>
          extractedPExpr != normalizePExpr specPExpr
        | _, _ => false
      let expected0 := deriveObligationStatus e.eligibility.eligible
          e.eligibility.isTrusted e.extracted.isSome specDrifted e.spec e.fingerprint e.subjectDigest
      -- Dependency containment (R-0004 slice 3) is applied AFTER derivation, so
      -- re-deriving from this function's own facts alone cannot reproduce it.
      -- Re-apply the same rule here rather than exempting the status: an
      -- exemption would stop this invariant checking anything for contained
      -- claims, which are exactly the ones whose status was just recomputed.
      -- The AUTHORITY pass is applied after derivation too, for the same reason and with the same
      -- treatment: re-apply the rule rather than exempting the status. Exempting would stop this
      -- invariant checking anything for exactly the claims whose status was just recomputed — and an
      -- earlier version did exactly that by omission, reporting OBL-STATUS violations against its
      -- own pipeline for every downgraded subject.
      let expected1 := if expected0 == .proved && !o.notCurrentDeps.isEmpty
                       then ObligationStatus.depsNotCurrent else expected0
      let expected := if expected1 == .proved && o.status == .correspondenceUnjustified
                      then ObligationStatus.correspondenceUnjustified else expected1
      if o.status != expected then
        some { invariant := "OBL-STATUS", function := qn
             , message := s!"obligation status '{repr o.status}' disagrees with re-derived '{repr expected}'" }
      else none
    | none =>
      match pc.findExcluded qn with
      | some x =>
        let expected := deriveObligationStatus x.eligibility.eligible
            x.eligibility.isTrusted false false x.spec x.fingerprint
        if o.status != expected then
          some { invariant := "OBL-STATUS", function := qn
               , message := s!"obligation status '{repr o.status}' disagrees with re-derived '{repr expected}'" }
        else none
      | none => none  -- caught by OBL-KNOWN

  -- INV-PROVED-ROOTS: a `proved` verdict requires a COMPUTABLE dependency closure.
  --
  -- This is the invariant `applyCorrespondenceAuthority`'s root conjunct exists to enforce, asserted
  -- where the producers live rather than by correlating two report surfaces in shell. A gate tried
  -- the latter first and was INERT: it paired subject headers with `depRoot` lines using `grep -B1`,
  -- and the two are many lines apart, so it extracted no subject name and reported success on a
  -- corpus where roots visibly refuse. A structured check cannot drift from the artifact it reads.
  --
  -- The root conjunct is currently REDUNDANT on this corpus — every subject whose root refuses is
  -- already not `proved` — so this invariant is expected to hold vacuously today. That is exactly
  -- why it belongs here: it is the thing that would notice if it stopped being vacuous.
  -- ANNOTATED, so this list stands on its own. Without the type the record literals below take their
  -- expected type from the concatenation at the end of the function, which means REMOVING this
  -- invariant from that concatenation does not compile — and a mutation that cannot compile tests
  -- nothing. The annotation is what lets the mutation be valid and therefore lets the control prove
  -- the invariant is load-bearing.
  let provedRoots : List ConsistencyViolation := pc.obligations.filterMap fun o =>
    if o.status != .proved then none
    else match pc.findEntry o.functionId.qualName with
      | none => none
      | some e =>
        match e.definitionIdentity with
        | .error w =>
          some { invariant := "PROVED-ROOTS", function := o.functionId.qualName
               , message := s!"obligation is 'proved' but has no scoped identity: {w.explain}" }
        | .ok d =>
          match Proof.dependencyRootMaterial (dependencyNodesOf pc pc.callGraph) d with
          | .ok _ => none
          | .error re =>
            some { invariant := "PROVED-ROOTS", function := o.functionId.qualName
                 , message := s!"obligation is 'proved' but its dependency closure does not compute: {re.explain}" }

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

  -- THE SUBJECT THIS OBLIGATION'S STORED LINK IS COMPARED AGAINST. Read from the entry, because the
  -- entry is where the V2 digest lives and `functionId.fingerprint` is the V1 body fingerprint — a
  -- different question. This invariant compared the stored value against that fingerprint and so
  -- audited the deriver with a rule the deriver had stopped using.
  let digestFor := fun (qn : String) => (pc.findEntry qn).bind (·.subjectDigest)
  -- FRESH means neither moved nor incomparable, evaluated by the SAME predicates the deriver uses.
  -- Not a local re-implementation: a private copy is exactly how this drifted.
  let fpFresh := fun (a : SpecAttachment) (qn : String) (currentFp : String) =>
    let sd := digestFor qn
    !(specIsStale a currentFp sd) && !(specIsNotComparable a sd)
  -- INV-4: Proved status requires a fresh fingerprint
  let provedFp := pc.obligations.filterMap fun o =>
    if o.status != .proved then none
    else match o.spec with
    | some a =>
      if !fpFresh a o.functionId.qualName o.functionId.fingerprint then
        some { invariant := "PROVED-FP", function := o.functionId.qualName
             , message := s!"obligation is 'proved' but fingerprints disagree" }
      else none
    | none =>
      some { invariant := "PROVED-SPEC", function := o.functionId.qualName
           , message := "obligation is 'proved' but has no spec attachment" }

  -- INV-5: Stale status requires a spec whose fingerprint no longer matches
  let staleFp := pc.obligations.filterMap fun o =>
    if o.status != .stale then none
    else match o.spec with
    | some a =>
      if fpFresh a o.functionId.qualName o.functionId.fingerprint then
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

  -- INV-14: every name in notCurrentDeps really is not current.
  -- Was "…is actually STALE", which is narrower than the field now means: since
  -- R-0004 slice 3 it holds the reachable closure of anything not current —
  -- stale, unbound, missing, blocked or ineligible. Leaving the stale-only test
  -- in place made the invariant fire on correct output (a `missing` callee), and
  -- the field was renamed from `staleDeps` for the same reason: a name saying
  -- "stale" while holding "missing" is the restated-fact hazard again.
  let notCurrentOblNames := (pc.obligations.filter fun o =>
    !o.status.isCurrentForDependents).map (·.functionId.qualName)
  let depStale := pc.obligations.foldl (fun acc o =>
    acc ++ o.notCurrentDeps.filterMap fun dep =>
      if !notCurrentOblNames.contains dep then
        some { invariant := "DEP-NOT-CURRENT", function := o.functionId.qualName
             , message := s!"dependency '{dep}' is listed as not current, but its status is current" }
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
        | .unboundProofLink => o.status == .unbound
        | .dependencyNotCurrent => o.status == .depsNotCurrent
        | .dependencyClosureUnjustified => o.status == .correspondenceUnjustified
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

  oblKnown ++ oblStatus ++ provedRoots ++ provedExtracted ++ provedFp ++ staleFp
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

/-- Table-resolution refusals encountered while building a subject's correspondence witnesses.

    The witness derivation cannot act on these — an unreadable table justifies nothing either way —
    but the REASON must survive, or an unjustified edge cannot be told apart from an unreadable
    dependency. Reported beside the correspondence line rather than folded into it. -/
def tableResolutionRefusalsOf (pc : ProofCore) (graph : CallGraph) (id : CallableId) : List String :=
  -- A SUBJECT WITHOUT A SCOPED IDENTITY HAS NO CORRESPONDENCE OPERATION AT ALL, so it has no table
  -- refusals either — reporting an empty list here says "none encountered", and the reason the
  -- subject could not be corresponded is reported by the correspondence line itself.
  match correspondenceInputOf pc graph id with
  | .error _ => []
  | .ok i    => i.resolverRefusals.map (·.explain)

end Concrete
