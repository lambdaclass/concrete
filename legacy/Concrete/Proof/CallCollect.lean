import Concrete.Elab.Core

/-!
Call collection over Core function bodies: direct calls, indirect calls through fn-typed
bindings, and functions taken as values.

Its own module so that the call graph (ProofCore) and the assumption summary read call edges
from ONE definition, and so that ProofCore can consume the summary without an import cycle.
-/

namespace Concrete

-- Call collection

-- Bug 296: ONE accumulating walk serves every collection below. Each family gives the items an
-- expression contributes itself (`here`), before its sub-expressions, and the walk visits the
-- sub-expressions, arms and statements in source order, prepending to an accumulator that is
-- reversed once. The walks it replaces built each list with `acc ++ child` per statement,
-- argument, field, arm and element, so a function of n calls copied its prefix n times (n²).
mutual
partial def collectGoExpr {α} (here : CExpr → List α) (acc : List α) (e : CExpr) : List α :=
  let acc := (here e).foldl (fun a x => x :: a) acc
  let go := collectGoExpr here
  match e with
  | .call _ _ args _ => args.foldl go acc
  | .binOp _ l r _ => go (go acc l) r
  | .unaryOp _ e _ => go acc e
  | .structLit _ _ fields _ => fields.foldl (fun a (_, v) => go a v) acc
  | .fieldAccess obj _ _ => go acc obj
  | .enumLit _ _ _ fields _ => fields.foldl (fun a (_, v) => go a v) acc
  | .match_ scrut arms _ => arms.foldl (collectGoArm here) (go acc scrut)
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => go acc inner
  | .arrayLit elems _ => elems.foldl go acc
  | .arrayIndex arr idx _ => go (go acc arr) idx
  | .cast inner _ => go acc inner
  | .allocCall inner alloc _ => go (go acc inner) alloc
  | .ifExpr cond th el _ => collectGoStmts here (collectGoStmts here (go acc cond) th) el
  | _ => acc

partial def collectGoGuard {α} (here : CExpr → List α) (acc : List α) : Option CExpr → List α
  | some g => collectGoExpr here acc g
  | none => acc

partial def collectGoArm {α} (here : CExpr → List α) (acc : List α) (arm : CMatchArm) : List α :=
  let go := collectGoExpr here
  match arm with
  | .enumArm _ _ _ guard body => collectGoStmts here (collectGoGuard here acc guard) body
  | .litArm v guard body => collectGoStmts here (collectGoGuard here (go acc v) guard) body
  | .varArm _ _ guard body => collectGoStmts here (collectGoGuard here acc guard) body
  | .rangeArm lo hi _ guard body =>
    collectGoStmts here (collectGoGuard here (go (go acc lo) hi) guard) body

partial def collectGoStmt {α} (here : CExpr → List α) (acc : List α) (s : CStmt) : List α :=
  let go := collectGoExpr here
  let gs := collectGoStmts here
  match s with
  | .letDecl _ _ _ v => go acc v
  | .assign _ v => go acc v
  | .return_ (some v) _ => go acc v
  | .return_ none _ => acc
  | .expr e _ => go acc e
  | .ifElse c t el =>
    let acc := gs (go acc c) t
    match el with | some stmts => gs acc stmts | none => acc
  | .while_ c body _ step => gs (gs (go acc c) body) step
  | .fieldAssign obj _ v => go (go acc obj) v
  | .derefAssign t v => go (go acc t) v
  | .arrayIndexAssign arr idx v => go (go (go acc arr) idx) v
  | .break_ (some v) _ => go acc v
  | .break_ none _ | .continue_ _ => acc
  | .defer body => go acc body
  | .borrowIn _ _ _ _ _ body => gs acc body

partial def collectGoStmts {α} (here : CExpr → List α) (acc : List α) (ss : List CStmt) : List α :=
  ss.foldl (collectGoStmt here) acc
end

/-- The callee a call names: a call's own contribution to the callee walk. -/
def calleeHere : CExpr → List Callee
  | .call callee _ _ _ => [callee]
  | _ => []

-- Every callee of every call in a body, in walk order: THE one walk. The views below select
-- by the callee's form, so they cannot disagree about which expressions are visited.
def collectCalleesExpr (e : CExpr) : List Callee := (collectGoExpr calleeHere [] e).reverse
def collectCalleesArm (arm : CMatchArm) : List Callee := (collectGoArm calleeHere [] arm).reverse
def collectCalleesStmt (s : CStmt) : List Callee := (collectGoStmt calleeHere [] s).reverse
def collectCalleesStmts (ss : List CStmt) : List Callee := (collectGoStmts calleeHere [] ss).reverse

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
/-- A function value's own contribution: a `fnRef`, or a free global function used as a value
    (a function-typed `.ident` with no binding identity, Elab's form for `let f = g;`), which the
    walk once missed (Codex review r7 of bugs 238 – 239: a returned method value reached no
    foreign assumption, and neither did a free one). A METHOD value (`CLocal.space = .method`) is
    collected by `collectMethodValueRefs*`: it names a method, never a foreign binding of the
    spelling (Codex review r8). -/
def fnValueHere : CExpr → List String
  | .fnRef name _ => [name]
  | .ident n (.fn_ ..) => if n.id.isNone && n.space == .free then [n.name] else []
  | _ => []

def collectFnValueRefsExpr (e : CExpr) : List String := (collectGoExpr fnValueHere [] e).reverse
def collectFnValueRefsArm (arm : CMatchArm) : List String := (collectGoArm fnValueHere [] arm).reverse
def collectFnValueRefsStmt (s : CStmt) : List String := (collectGoStmt fnValueHere [] s).reverse
def collectFnValueRefsStmts (ss : List CStmt) : List String :=
  (collectGoStmts fnValueHere [] ss).reverse

-- The METHOD values a body names (bugs 238 – 239): methods used as values by their definition
-- name, which reach that method and never a foreign binding of the spelling.
/-- A method value's own contribution: a function-typed `.ident` with no binding identity in the
    method namespace, by its definition name. -/
def methodValueHere : CExpr → List String
  | .ident n (.fn_ ..) => if n.id.isNone && n.space == .method then [n.name] else []
  | _ => []

def collectMethodValueRefsExpr (e : CExpr) : List String := (collectGoExpr methodValueHere [] e).reverse
def collectMethodValueRefsArm (arm : CMatchArm) : List String :=
  (collectGoArm methodValueHere [] arm).reverse
def collectMethodValueRefsStmt (s : CStmt) : List String :=
  (collectGoStmt methodValueHere [] s).reverse
def collectMethodValueRefsStmts (ss : List CStmt) : List String :=
  (collectGoStmts methodValueHere [] ss).reverse

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
