import Concrete.Elab.Core

/-!
Call collection over Core function bodies: direct calls, indirect calls through fn-typed
bindings, and functions taken as values.

Its own module so that the call graph (ProofCore) and the assumption summary read call edges
from ONE definition, and so that ProofCore can consume the summary without an import cycle.
-/

namespace Concrete

-- Call collection

-- Every callee of every call in a body, in walk order: THE one walk. The views below select
-- by the callee's form, so they cannot disagree about which expressions are visited.
mutual
partial def collectCalleesExpr (e : CExpr) : List Callee :=
  match e with
  | .call callee _ args _ =>
    callee :: args.foldl (fun acc a => acc ++ collectCalleesExpr a) []
  | .binOp _ l r _ => collectCalleesExpr l ++ collectCalleesExpr r
  | .unaryOp _ e _ => collectCalleesExpr e
  | .structLit _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectCalleesExpr v) []
  | .fieldAccess obj _ _ => collectCalleesExpr obj
  | .enumLit _ _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectCalleesExpr v) []
  | .match_ scrut arms _ => collectCalleesExpr scrut ++ arms.foldl (fun acc a => acc ++ collectCalleesArm a) []
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => collectCalleesExpr inner
  | .arrayLit elems _ => elems.foldl (fun acc e => acc ++ collectCalleesExpr e) []
  | .arrayIndex arr idx _ => collectCalleesExpr arr ++ collectCalleesExpr idx
  | .cast inner _ => collectCalleesExpr inner
  | .allocCall inner alloc _ => collectCalleesExpr inner ++ collectCalleesExpr alloc
  | .ifExpr cond th el _ =>
    collectCalleesExpr cond ++ collectCalleesStmts th ++ collectCalleesStmts el
  | _ => []

partial def collectCalleesArm (arm : CMatchArm) : List Callee :=
  match arm with
  | .enumArm _ _ _ guard body => (guard.map collectCalleesExpr).getD [] ++ collectCalleesStmts body
  | .litArm v guard body => collectCalleesExpr v ++ (guard.map collectCalleesExpr).getD [] ++ collectCalleesStmts body
  | .varArm _ _ guard body => (guard.map collectCalleesExpr).getD [] ++ collectCalleesStmts body
  | .rangeArm lo hi _ guard body => collectCalleesExpr lo ++ collectCalleesExpr hi ++ (guard.map collectCalleesExpr).getD [] ++ collectCalleesStmts body

partial def collectCalleesStmt (s : CStmt) : List Callee :=
  match s with
  | .letDecl _ _ _ v => collectCalleesExpr v
  | .assign _ v => collectCalleesExpr v
  | .return_ (some v) _ => collectCalleesExpr v
  | .return_ none _ => []
  | .expr e _ => collectCalleesExpr e
  | .ifElse c t el =>
    collectCalleesExpr c ++ collectCalleesStmts t ++
    match el with | some stmts => collectCalleesStmts stmts | none => []
  | .while_ c body _ step =>
    collectCalleesExpr c ++ collectCalleesStmts body ++ collectCalleesStmts step
  | .fieldAssign obj _ v => collectCalleesExpr obj ++ collectCalleesExpr v
  | .derefAssign t v => collectCalleesExpr t ++ collectCalleesExpr v
  | .arrayIndexAssign arr idx v =>
    collectCalleesExpr arr ++ collectCalleesExpr idx ++ collectCalleesExpr v
  | .break_ (some v) _ => collectCalleesExpr v
  | .break_ none _ | .continue_ _ => []
  | .defer body => collectCalleesExpr body
  | .borrowIn _ _ _ _ _ body => collectCalleesStmts body

partial def collectCalleesStmts (ss : List CStmt) : List Callee :=
  ss.foldl (fun acc s => acc ++ collectCalleesStmt s) []
end

/-- Direct callees. Only a direct callee names a definition. An indirect callee is a fn-typed
    binding, so its statically-known target set is empty — which is also why it contributes no
    dependency edge. Extraction keeps it as `PExpr.applyVar` rather than refusing the body
    (which cost three real std proofs); the identity is the binding, not a global name. A
    compiler-generated runtime-helper call (`Callee.runtime`, bug 165) names no program
    definition either, so it is no edge: resolving its spelling would find a module's own
    `fn print_int`, which the call never reaches. -/
def collectCallsExpr (e : CExpr) : List String := (collectCalleesExpr e).filterMap (·.directName?)
def collectCallsStmts (ss : List CStmt) : List String := (collectCalleesStmts ss).filterMap (·.directName?)

/-- The free direct calls of a function that its resolver bound to a foreign declaration
    (`foreignCalls`, not a runtime helper), with that declaration (bugs 238 – 239, Codex review
    r21): the only calls that are extern calls. A free call of a program function is none,
    whatever extern elsewhere shares its spelling. -/
def foreignBoundCalls (f : CFnDef) : List (String × String) :=
  ((collectCalleesStmts f.body).filterMap fun c => match c with
    | .direct n .free =>
      match f.foreignCalls.lookup n with
      | some d => if d.startsWith "_C$rt." then none else some (n, d)
      | none => none
    | _ => none).eraseDups

/-- The extern calls of a function, by the spelling written, among `externNames`. -/
def externCallsIn (externNames : List String) (f : CFnDef) : List String :=
  (foreignBoundCalls f).filterMap fun (n, d) =>
    if externNames.contains n || externNames.contains d then some n else none

/-- The FREE direct calls of a body (`CallSpace.free`, bugs 238 – 239): the only calls a foreign
    binding of the spelling can answer, so the ones an "is this an extern call?" test reads. -/
def collectFreeCallsStmts (ss : List CStmt) : List String :=
  (collectCalleesStmts ss).filterMap fun c => match c with
    | .direct n .free => some n
    | _ => none

/-! ### Indirect calls

The calls `collectCalls*` deliberately drops: a call through a fn-typed binding has no
statically known target. The assumption summary (R-0484 R10) must not drop them — an
indirect call is exactly where its "which foreign bindings can this reach" answer becomes
UNKNOWN rather than empty — so it collects the binding names here, from the same walk
shape as `collectCalls*` so the two cannot disagree about which expressions are visited.
-/
def collectIndirectCallsExpr (e : CExpr) : List String :=
  (collectCalleesExpr e).filterMap fun | .indirect b => some b.name | _ => none
def collectIndirectCallsStmts (ss : List CStmt) : List String :=
  (collectCalleesStmts ss).filterMap fun | .indirect b => some b.name | _ => none

/-- The runtime helpers a body calls through compiler-generated calls (`println`'s
    `print_int`, bug 165): the helper names, for reports that explain which intrinsic supplies
    a capability. Never resolved against program definitions. -/
def collectRuntimeCallsStmts (ss : List CStmt) : List String :=
  (collectCalleesStmts ss).filterMap fun | .runtime h => some h | _ => none

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
  -- A global function used as a value is a function-typed `.ident` with no binding identity
  -- (Elab's form for `let f = g;`), which this walk missed (Codex review r7 of bugs 238 – 239: a
  -- returned method value reached no foreign assumption, and neither did a free one).
  -- A METHOD value (`CLocal.space = .method`) is collected by `collectMethodValueRefs*`: it names
  -- a method, never a foreign binding of the spelling (Codex review r8).
  | .ident n (.fn_ ..) => if n.id.isNone && n.space == .free then [n.name] else []
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

-- The METHOD values a body names (bugs 238 – 239): methods used as values by their definition
-- name, which reach that method and never a foreign binding of the spelling.
mutual
partial def collectMethodValueRefsExpr (e : CExpr) : List String :=
  match e with
  | .fnRef _ _ => []
  -- A global function used as a value is a function-typed `.ident` with no binding identity
  -- (Elab's form for `let f = g;`), which this walk missed (Codex review r7 of bugs 238 – 239: a
  -- returned method value reached no foreign assumption, and neither did a free one).
  | .ident n (.fn_ ..) => if n.id.isNone && n.space == .method then [n.name] else []
  | .call _ _ args _ => args.foldl (fun acc a => acc ++ collectMethodValueRefsExpr a) []
  | .binOp _ l r _ => collectMethodValueRefsExpr l ++ collectMethodValueRefsExpr r
  | .unaryOp _ e _ => collectMethodValueRefsExpr e
  | .structLit _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectMethodValueRefsExpr v) []
  | .fieldAccess obj _ _ => collectMethodValueRefsExpr obj
  | .enumLit _ _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectMethodValueRefsExpr v) []
  | .match_ scrut arms _ =>
    collectMethodValueRefsExpr scrut ++ arms.foldl (fun acc a => acc ++ collectMethodValueRefsArm a) []
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => collectMethodValueRefsExpr inner
  | .arrayLit elems _ => elems.foldl (fun acc e => acc ++ collectMethodValueRefsExpr e) []
  | .arrayIndex arr idx _ => collectMethodValueRefsExpr arr ++ collectMethodValueRefsExpr idx
  | .cast inner _ => collectMethodValueRefsExpr inner
  | .allocCall inner alloc _ => collectMethodValueRefsExpr inner ++ collectMethodValueRefsExpr alloc
  | .ifExpr cond th el _ =>
    collectMethodValueRefsExpr cond ++ collectMethodValueRefsStmts th ++ collectMethodValueRefsStmts el
  | _ => []

partial def collectMethodValueRefsArm (arm : CMatchArm) : List String :=
  match arm with
  | .enumArm _ _ _ guard body => (guard.map collectMethodValueRefsExpr).getD [] ++ collectMethodValueRefsStmts body
  | .litArm v guard body =>
    collectMethodValueRefsExpr v ++ (guard.map collectMethodValueRefsExpr).getD [] ++ collectMethodValueRefsStmts body
  | .varArm _ _ guard body => (guard.map collectMethodValueRefsExpr).getD [] ++ collectMethodValueRefsStmts body
  | .rangeArm lo hi _ guard body =>
    collectMethodValueRefsExpr lo ++ collectMethodValueRefsExpr hi ++ (guard.map collectMethodValueRefsExpr).getD []
      ++ collectMethodValueRefsStmts body

partial def collectMethodValueRefsStmt (s : CStmt) : List String :=
  match s with
  | .letDecl _ _ _ v => collectMethodValueRefsExpr v
  | .assign _ v => collectMethodValueRefsExpr v
  | .return_ (some v) _ => collectMethodValueRefsExpr v
  | .return_ none _ => []
  | .expr e _ => collectMethodValueRefsExpr e
  | .ifElse c t el =>
    collectMethodValueRefsExpr c ++ collectMethodValueRefsStmts t ++
    match el with | some stmts => collectMethodValueRefsStmts stmts | none => []
  | .while_ c body _ step =>
    collectMethodValueRefsExpr c ++ collectMethodValueRefsStmts body ++ collectMethodValueRefsStmts step
  | .fieldAssign obj _ v => collectMethodValueRefsExpr obj ++ collectMethodValueRefsExpr v
  | .derefAssign t v => collectMethodValueRefsExpr t ++ collectMethodValueRefsExpr v
  | .arrayIndexAssign arr idx v =>
    collectMethodValueRefsExpr arr ++ collectMethodValueRefsExpr idx ++ collectMethodValueRefsExpr v
  | .break_ (some v) _ => collectMethodValueRefsExpr v
  | .break_ none _ | .continue_ _ => []
  | .defer body => collectMethodValueRefsExpr body
  | .borrowIn _ _ _ _ _ body => collectMethodValueRefsStmts body

partial def collectMethodValueRefsStmts (ss : List CStmt) : List String :=
  ss.foldl (fun acc s => acc ++ collectMethodValueRefsStmt s) []
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
    callee.isIndirect || args.any hasIndirectCallExpr
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

/-! ### Code a constant initializer reaches

R-0492 step 4 (bug 233): an imported constant's initializer is inlined where it is used, outside
the module that elaborated it. Its constants are found there by identity, but a callee or a
function value is still a spelling resolved in the USING function's context (its foreign
bindings, its module's call aliases), which may bind it to something else. Such an initializer
is not inlined across modules until callables carry their `DefId` (a later R-0492 step).

`viaConst n` answers for a non-function `.ident n`: whether `n` is a constant whose own
initializer reaches code (the caller follows constants, Codex review); a local is answered `false`
or conservatively `true`. -/
mutual
partial def mentionsCallableExpr (viaConst : String → Bool) (e : CExpr) : Bool :=
  let go := mentionsCallableExpr viaConst
  match e with
  | .call _ _ _ _ | .fnRef _ _ => true
  | .ident _ (.fn_ _ _ _) => true
  | .ident n _ => viaConst n.name
  | .binOp _ l r _ | .arrayIndex l r _ | .allocCall l r _ => go l || go r
  | .unaryOp _ x _ | .fieldAccess x _ _ | .borrow x _ | .borrowMut x _ | .deref x _ | .cast x _ => go x
  | .structLit _ _ fs _ | .enumLit _ _ _ fs _ => fs.any fun (_, v) => go v
  | .arrayLit es _ => es.any go
  | .match_ scrut arms _ => go scrut || arms.any (mentionsCallableArm viaConst)
  | .ifExpr c t el _ => go c || mentionsCallableStmts viaConst t || mentionsCallableStmts viaConst el
  | _ => false

partial def mentionsCallableArm (viaConst : String → Bool) (arm : CMatchArm) : Bool :=
  let go := mentionsCallableExpr viaConst
  match arm with
  | .enumArm _ _ _ g body | .varArm _ _ g body =>
    (g.map go).getD false || mentionsCallableStmts viaConst body
  | .litArm v g body => go v || (g.map go).getD false || mentionsCallableStmts viaConst body
  | .rangeArm lo hi _ g body =>
    go lo || go hi || (g.map go).getD false || mentionsCallableStmts viaConst body

partial def mentionsCallableStmts (viaConst : String → Bool) (ss : List CStmt) : Bool :=
  let go := mentionsCallableExpr viaConst
  let gs := mentionsCallableStmts viaConst
  ss.any fun s => match s with
    | .letDecl _ _ _ v | .assign _ v | .expr v _ | .defer v => go v
    | .return_ v _ | .break_ v _ => (v.map go).getD false
    | .continue_ _ => false
    | .ifElse c t el => go c || gs t || (el.map gs).getD false
    | .while_ c body _ step => go c || gs body || gs step
    | .fieldAssign o _ v | .derefAssign o v => go o || go v
    | .arrayIndexAssign a i v => go a || go i || go v
    | .borrowIn _ _ _ _ _ body => gs body
end

/-- An initializer reaches code: it calls or names a function, or names a constant (found by
    `lookup`, the inlining scope's table) whose initializer does, transitively. `fuel` bounds the
    walk over constants; exhausted, the answer is the conservative `true`. -/
def initReachesCallable (lookup : String → Option CExpr) : Nat → CExpr → Bool
  | 0, _ => true
  | fuel + 1, e => mentionsCallableExpr (fun n => match lookup n with
      | some init => initReachesCallable lookup fuel init
      | none => false) e

end Concrete
