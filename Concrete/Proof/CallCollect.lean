import Concrete.Elab.Core

/-!
Call collection over Core function bodies: direct calls, indirect calls through fn-typed
bindings, and functions taken as values.

Its own module so that the call graph (ProofCore) and the assumption summary read call edges
from ONE definition, and so that ProofCore can consume the summary without an import cycle.
-/

namespace Concrete

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

/-! ### Indirect calls

The calls `collectCalls*` deliberately drops: a call through a fn-typed binding has no
statically known target. The assumption summary (R-0484 R10) must not drop them — an
indirect call is exactly where its "which foreign bindings can this reach" answer becomes
UNKNOWN rather than empty — so it collects the binding names here, from the same walk
shape as `collectCalls*` so the two cannot disagree about which expressions are visited.
-/
mutual
partial def collectIndirectCallsExpr (e : CExpr) : List String :=
  match e with
  | .call callee _ args _ =>
    (if callee.isIndirect then [callee.spelling] else []) ++
      args.foldl (fun acc a => acc ++ collectIndirectCallsExpr a) []
  | .binOp _ l r _ => collectIndirectCallsExpr l ++ collectIndirectCallsExpr r
  | .unaryOp _ e _ => collectIndirectCallsExpr e
  | .structLit _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectIndirectCallsExpr v) []
  | .fieldAccess obj _ _ => collectIndirectCallsExpr obj
  | .enumLit _ _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectIndirectCallsExpr v) []
  | .match_ scrut arms _ => collectIndirectCallsExpr scrut ++ arms.foldl (fun acc a => acc ++ collectIndirectCallsArm a) []
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => collectIndirectCallsExpr inner
  | .arrayLit elems _ => elems.foldl (fun acc e => acc ++ collectIndirectCallsExpr e) []
  | .arrayIndex arr idx _ => collectIndirectCallsExpr arr ++ collectIndirectCallsExpr idx
  | .cast inner _ => collectIndirectCallsExpr inner
  | .allocCall inner alloc _ => collectIndirectCallsExpr inner ++ collectIndirectCallsExpr alloc
  | .ifExpr cond th el _ =>
    collectIndirectCallsExpr cond ++ collectIndirectCallsStmts th ++ collectIndirectCallsStmts el
  | _ => []

partial def collectIndirectCallsArm (arm : CMatchArm) : List String :=
  match arm with
  | .enumArm _ _ _ guard body => (guard.map collectIndirectCallsExpr).getD [] ++ collectIndirectCallsStmts body
  | .litArm v guard body => collectIndirectCallsExpr v ++ (guard.map collectIndirectCallsExpr).getD [] ++ collectIndirectCallsStmts body
  | .varArm _ _ guard body => (guard.map collectIndirectCallsExpr).getD [] ++ collectIndirectCallsStmts body
  | .rangeArm lo hi _ guard body => collectIndirectCallsExpr lo ++ collectIndirectCallsExpr hi ++ (guard.map collectIndirectCallsExpr).getD [] ++ collectIndirectCallsStmts body

partial def collectIndirectCallsStmt (s : CStmt) : List String :=
  match s with
  | .letDecl _ _ _ v => collectIndirectCallsExpr v
  | .assign _ v => collectIndirectCallsExpr v
  | .return_ (some v) _ => collectIndirectCallsExpr v
  | .return_ none _ => []
  | .expr e _ => collectIndirectCallsExpr e
  | .ifElse c t el =>
    collectIndirectCallsExpr c ++ collectIndirectCallsStmts t ++
    match el with | some stmts => collectIndirectCallsStmts stmts | none => []
  | .while_ c body _ step =>
    collectIndirectCallsExpr c ++ collectIndirectCallsStmts body ++ collectIndirectCallsStmts step
  | .fieldAssign obj _ v => collectIndirectCallsExpr obj ++ collectIndirectCallsExpr v
  | .derefAssign t v => collectIndirectCallsExpr t ++ collectIndirectCallsExpr v
  | .arrayIndexAssign arr idx v =>
    collectIndirectCallsExpr arr ++ collectIndirectCallsExpr idx ++ collectIndirectCallsExpr v
  | .break_ (some v) _ => collectIndirectCallsExpr v
  | .break_ none _ | .continue_ _ => []
  | .defer body => collectIndirectCallsExpr body
  | .borrowIn _ _ _ _ _ body => collectIndirectCallsStmts body

partial def collectIndirectCallsStmts (ss : List CStmt) : List String :=
  ss.foldl (fun acc s => acc ++ collectIndirectCallsStmt s) []
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

end Concrete
