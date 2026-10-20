import Concrete.Frontend.AST
import Concrete.Resolve.BuiltinSigs
import Concrete.Report.Diagnostic
import Concrete.Resolve.FileSummary
import Concrete.Resolve.Intrinsic
import Concrete.Resolve.BuiltinEnums
import Concrete.Resolve.Resolve
import Concrete.Resolve.Shared
import Concrete.Semantics.TypeJudgment
import Concrete.Check.CheckHelpers

namespace Concrete

/-- Expression checking mode (docs/language/VALUE_FLOW_SPEC.md — the auto-consume
    inversion, 2026-07-06). Consumption is decided HERE, in ONE place, not in
    every AST handler; a new syntax form that forgets to pick a mode gets
    `value` and fails closed (over-rejects) instead of silently leaking.

    `value` — the default. An ident use MOVES a non-Copy binding (the read IS
        the consumption); a by-value read of a non-Copy sub-place (`w.f`,
        `arr[i]`) rejects (E0290/H11); Copy reads copy.
    `callArg` — call/method/static-call ARGUMENTS ONLY. H11 projection rules
        apply, but idents are NOT auto-consumed: the call site decides from
        the PARAMETER type (`&T` never consumes; `&mut T` consumes only
        borrow-block refs — reborrowable otherwise; owned params consume).
        Do NOT use this anywhere else — it would recreate the forgot-to-
        consume bug class (H13/H14).
    `place` — addressable expression: projection bases, borrow targets,
        assignment targets, `..base`. No consumption, no H11 rejection. -/
inductive UseMode where
  | value | callArg | place
  deriving Repr, BEq

/-- CapabilityJudgment slice 5: is `e` a LOCALLY-PROVABLE pure, trap-free,
    value-producing expression whose silent discard (`e;`) is a dead computation?

    Conservative — `true` only when the discard provably has no effect and no
    runtime trap-assertion, so it never fires on an effectful or assertion
    statement: literals, variable reads, field reads, and arithmetic /
    comparison / logical / bitwise / shift operators over such. `+ - *` can
    overflow, but that is incidental (not the point of the statement), so they
    stay flaggable; `/` and `%` ARE trap-assertions (division by zero) and are
    NOT flagged.

    Function / method calls are deliberately NOT treated as pure. Concrete's
    capability model does not track mutation through `&mut` parameters — nor the
    effects of `trusted` / `extern` FFI — as a capability, so an EMPTY capability
    set does not imply an effect-free call: `env_assign(&mut e, …)` (mutation)
    and `fclose(fp)` (extern) both carry empty caps yet have real effects.
    Soundly flagging a pure call would need an inter-procedural effect analysis
    Concrete does not have; it is deferred until the effect model tracks mutation
    (or a workload pulls the safe-fn + no-`&mut`-param + non-trusted refinement).
    So every call, cast, index, deref, borrow, and control-flow expression is
    conservatively NOT flagged. -/
def exprPureDiscardable : Expr → Bool
  | .intLit .. | .floatLit .. | .boolLit .. | .strLit .. | .charLit .. => true
  | .ident .. => true
  | .paren _ inner => exprPureDiscardable inner
  | .fieldAccess _ obj _ => exprPureDiscardable obj
  | .unaryOp _ _ operand => exprPureDiscardable operand
  | .binOp _ op l r =>
    match op with
    | .div | .mod => false  -- trap-assertion: division by zero
    | _ => exprPureDiscardable l && exprPureDiscardable r
  | _ => false

/-- The receiver form a method signature's leading parameter declares. -/
def selfKindOfParams (params : List (String × Ty)) : Option SelfKind :=
  match params.head? with
  | some ("self", .ref _) => some .ref
  | some ("self", .refMut _) => some .refMut
  | some ("self", _) => some .value
  | _ => none

/-- E0293: the auto-borrowed receiver counts as a borrow of the receiver PLACE. -/
def receiverBorrowParts (selfKind : Option SelfKind) (obj : Expr) : List ((String × List String) × Bool) :=
  match selfKind with
  | some .ref => (match borrowPathOf obj with | some p => [(p, false)] | none => [])
  | some .refMut => (match borrowPathOf obj with | some p => [(p, true)] | none => [])
  | _ => []

/-- An expression with its enclosing parentheses removed. -/
partial def stripParens : Expr → Expr
  | .paren _ inner => stripParens inner
  | e => e

/-- What a PLACE is reached through (bug 091 round 3). Field/index projections and `*`
    look through one layer of their base exactly as Check types them (field access
    auto-derefs), so the walk ends where the place's storage is named:
      * `owned root`: an owned binding (its mutability and live borrows govern the place);
      * `shared desc`: a `&T` — a binding, a `&` borrow, a cast to `&`, or an expression the
        walk does not classify (fail closed: `*(if c { &r } else { &r })`);
      * `exclusiveRef`: a `&mut T` binding or cast;
      * `rawPtr`: a raw pointer (trusted code's own responsibility);
      * `temp`: an owned temporary (a call result or literal — no code returns a reference).
    A `&mut e` borrow is the place `e` itself: its exclusivity is only as good as `e`'s. -/
inductive PlaceVia where
  | owned (root : String)
  | shared (desc : String)
  | exclusiveRef
  | rawPtr
  | temp

/-- Two branches' classifications joined: a branch reached through `&` makes the whole
    possibly shared; then `&mut`, then a raw pointer; otherwise an owned temporary. -/
def joinVia : PlaceVia → PlaceVia → PlaceVia
  | .shared d, _ | _, .shared d => .shared d
  | .exclusiveRef, _ | _, .exclusiveRef => .exclusiveRef
  | .rawPtr, _ | _, .rawPtr => .rawPtr
  | _, _ => .temp

mutual
/-- The classification of the VALUE a block yields: its trailing value expression, where a
    moved owned binding is an owned temporary. A tail naming a binding the block itself
    declared is classified by that `let` (its declared type, else its value). A block without
    a value tail diverges, contributing nothing; one this walk cannot read fails closed. -/
partial def tailVia (stmts : List Stmt) (owned : List String := []) : CheckM PlaceVia := do
  let unclassified : PlaceVia :=
    .shared "the place is reached through a block the checker does not classify; bind it to a name first"
  match stmts.getLast? with
  | some (.expr _ e true) => blockValueVia (stmts.dropLast) e owned
  | some (.return_ ..) | some (.break_ ..) | some (.continue_ ..) => return .temp
  | some (.expr _ e false) =>
    match e with
    | .call _ fn _ _ => if resolveIntrinsic fn == some .abort then return .temp else return unclassified
    | _ => return unclassified
  | _ => return unclassified

/-- The classification of value `e` yielded at the end of a block whose earlier statements are
    `before`: a name the block itself bound is followed to its `let` (its declared type, else
    its initializer, classified against the statements before THAT `let`), so a chain
    `let a = W {..}; let w = a; w` is an owned temporary. A moved owned binding is a temporary. -/
partial def blockValueVia (before : List Stmt) (e : Expr) (owned : List String := []) : CheckM PlaceVia := do
  let local? : Option (Nat × Option Ty × Expr) := match stripParens e with
    | .ident _ n =>
      ((List.range before.length).zip before).foldl (fun acc (i, st) => match st with
        | .letDecl _ m _ ty v _ => if m == n then some (i, ty, v) else acc
        | _ => acc) none
    | _ => none
  match local? with
  | some (_, some ty, _) =>
    match ty with
    | .ref _ => return .shared "a branch yields a `&` binding"
    | .refMut _ => return .exclusiveRef
    | .ptrMut _ | .ptrConst _ => return .rawPtr
    | _ => return .temp
  | some (i, none, v) => blockValueVia (before.take i) v owned
  | none =>
    -- an arm binder of a match over an OWNED scrutinee holds an owned payload
    match stripParens e with
    | .ident _ n =>
      if owned.contains n then return .temp
      match ← placeVia e with
      | .owned _ => return .temp
      | v => return v
    | _ =>
      match ← placeVia e with
      | .owned _ => return .temp
      | v => return v

/-- What the REFERENCE (or pointer) an expression yields points through: the operand of `*`.
    A `&mut e` / `&e` / cast is that reference itself; a binding by its type; a field or
    element holding a pointer by its peeked type (fail closed when unknown). -/
partial def refVia (e : Expr) : CheckM PlaceVia := do
  match stripParens e with
  | .ident .. => placeVia e
  | .borrow .. => return .shared "the place is reached through a `&` borrow"
  | .borrowMut .. => return .exclusiveRef
  | .cast .. | .ifExpr .. | .match_ .. => placeVia e
  | inner@(.fieldAccess ..) | inner@(.arrayIndex ..) =>
    match ← peekExprType inner with
    | .ref _ => return .shared "the place is reached through a `&` stored in a field"
    | .refMut _ => return .exclusiveRef
    | .ptrMut _ | .ptrConst _ => return .rawPtr
    | .heap _ | .heapArray _ => placeVia inner
    | _ => return .shared "the place is reached through a dereference the checker does not classify; bind the reference to a name first"
  | _ => return .shared "the place is reached through a dereference the checker does not classify; bind the reference to a name first"

partial def placeVia (e : Expr) : CheckM PlaceVia := do
  match e with
  | .paren _ inner => placeVia inner
  | .ident _ n =>
    match ← lookupVarInfo n with
    | some info =>
      match info.ty with
      | .ref _ => return .shared s!"'{n}' is a `&` reference"
      | .refMut _ => return .exclusiveRef
      | .ptrMut _ | .ptrConst _ => return .rawPtr
      | _ => return .owned n
    -- A name the walk cannot see here (a match-arm binder in a branch tail) may be a
    -- reference: fail closed.
    | none => return .shared s!"'{n}' is bound inside the expression; bind the value to a name first"
  | .fieldAccess _ obj _ => placeVia obj
  | .arrayIndex _ arr _ => placeVia arr
  -- `*x` is reached through whatever reference `x` is — never "owned" because `x` names an
  -- owned root behind a borrow (`*(&mut p)` traverses a reference; Codex review).
  | .deref _ inner => refVia inner
  -- A borrow expression as a place's base is a temporary reference; the borrow itself was
  -- checked where it was written (explicit `&mut` follows `checkMutBorrowPlace`).
  | .borrow _ _ => return .shared "the place is reached through a `&` borrow"
  | .borrowMut _ _ => return .exclusiveRef
  | .cast _ _ ty =>
    match ty with
    | .ref _ => return .shared "the place is reached through a cast to `&`"
    | .refMut _ => return .exclusiveRef
    | .ptrMut _ | .ptrConst _ => return .rawPtr
    | _ => return .temp
  | .call .. | .methodCall .. | .staticMethodCall .. | .structLit .. | .enumLit ..
  | .arrayLit .. | .intLit .. | .floatLit .. | .boolLit .. | .strLit .. | .charLit .. =>
    return .temp
  -- A conditional is whatever its branches yield: `if c { W {..} } else { W {..} }` is an
  -- owned temporary, `*(if c { &r } else { &r })` is reached through `&` (Codex review).
  | .ifExpr _ _ t el => return joinVia (← tailVia t) (← tailVia el)
  | .match_ _ scrut arms =>
    -- Arm binders of a match over an owned value (a binding it moves, or a temporary) are
    -- owned payloads; over anything reached through a reference they are borrowed views,
    -- which the walk fails closed on (Codex review of round 3).
    let scrutOwned : Bool := match ← placeVia scrut with
      | .owned _ | .temp => true
      | _ => false
    let mut acc : PlaceVia := .temp
    for arm in arms do
      let (body, binders) : List Stmt × List String := match arm with
        | .mk _ _ _ bs _ b => (b, bs)
        | .varArm _ v _ b => (b, [v])
        | .litArm _ _ _ b | .rangeArm _ _ _ _ _ b => (b, [])
      acc := joinVia acc (← tailVia body (if scrutOwned then binders else []))
    return acc
  | _ => return .shared "the place is reached through an expression that may be a `&` reference; bind it to a name first"
end

/-- What a reference's storage belongs to, for borrow provenance (bug 091 round 3):
    a local owned ROOT binding, storage outside this function's bindings (a parameter's
    pointee, a raw pointer, a temporary), or unknown. -/
inductive BorrowProv where
  | root (name : String)
  | external
  | unknown

mutual
/-- The root of a PLACE. -/
partial def placeProv (e : Expr) : CheckM BorrowProv := do
  match e with
  | .paren _ inner => placeProv inner
  | .ident _ n =>
    match ← lookupVarInfo n with
    | some info =>
      match info.ty with
      | .ref _ | .refMut _ =>
        -- a place reached through a reference belongs to whatever that reference borrows;
        -- a reference with no local source (a parameter) is itself the root, so a reborrow
        -- `&*r` is a live borrow OF `r` (Codex review of round 3)
        return match info.borrowedFrom with | some s => .root s | none => .root n
      | .ptrMut _ | .ptrConst _ => return .external
      | _ => return .root n
    | none => return .unknown
  | .fieldAccess _ obj _ => placeProv obj
  | .arrayIndex _ arr _ => placeProv arr
  | .deref _ inner => refProv inner
  | .call .. | .methodCall .. | .staticMethodCall .. | .structLit .. | .enumLit ..
  | .arrayLit .. => return .external
  | _ => return .unknown

/-- The root of the storage a REFERENCE-valued expression points to. -/
partial def refProv (e : Expr) : CheckM BorrowProv := do
  match e with
  | .paren _ inner => refProv inner
  | .borrow _ inner | .borrowMut _ inner => placeProv inner
  | .ident .. | .fieldAccess .. | .arrayIndex .. => placeProv e
  | .deref _ inner => refProv inner
  | .cast .. | .call .. | .methodCall .. | .staticMethodCall .. => return .external
  | _ => return .unknown
end

/-- A binding of reference type is a live borrow of its source's ROOT binding: `&p`, `(&p)`,
    `&(p)`, `&w.p`, `&*(&p)`, `&*r` and a copy of a tracked reference all record provenance
    (bug 091 round 3), at `let` and at reassignment alike, so a later `&mut` of the root —
    explicit or an implicit receiver — sees it. A reference whose source cannot be traced
    (an `if` choosing between borrows) is refused rather than left untracked (E0238). -/
def recordRefProvenance (name : String) (ty : Ty) (value : Expr) (span : Option Span)
    (reassign : Bool := false) : CheckM Unit := do
  match ty with
  | .ref _ | .refMut _ =>
    let prov ← match stripParens value with
      | .borrow _ inner | .borrowMut _ inner =>
        match stripParens inner with
        | .ident _ n => pure (BorrowProv.root n)  -- the binding itself, whatever its type
        | _ => placeProv inner
      | v => refProv v
    let setSource : Option String → CheckM Unit := fun src =>
      modify fun env =>
        { env with vars := env.vars.map fun (n, info) =>
            if n == name then (n, { info with borrowedFrom := src }) else (n, info) }
    -- Provenance is one source per binding and is not merged across branches, so a
    -- reassignment may not RE-POINT a reference at different storage (`if c { r = &p; }`
    -- would leave `r` borrowing `p` invisibly after the branch): E0238, use a new binding.
    let current := ((← getEnv).vars.lookup name).bind (·.borrowedFrom)
    let newSrc : Option String := match prov with
      | .root s => if s != name then some s else current
      | _ => none
    if reassign && newSrc != current then
      throwCheck (.borrowProvenanceUnknown name) span
    match prov with
    | .root sourceName => if sourceName != name then setSource (some sourceName) else pure ()
    | .external => setSource none
    | .unknown => throwCheck (.borrowProvenanceUnknown name) span
  | _ => pure ()

/-- A RECEIVER's place. At the top level an unclassified expression (`if`/`match`) whose
    checked type is a value (the callers handle `&T`/`&mut T` receivers first) is an owned
    temporary, not a possible reference — only bases under a projection or `*` fail closed. -/
def receiverVia (obj : Expr) : CheckM PlaceVia := do
  match stripParens obj with
  | .ident .. | .fieldAccess .. | .arrayIndex .. | .deref .. | .borrow .. | .borrowMut ..
  | .cast .. => placeVia obj
  | _ => return .temp

/-- The binding a place path is rooted at, through parentheses, projections and `*`. -/
partial def placePathRoot : Expr → Option String
  | .paren _ i => placePathRoot i
  | .ident _ n => some n
  | .fieldAccess _ o _ => placePathRoot o
  | .arrayIndex _ a _ => placePathRoot a
  | .deref _ i => placePathRoot i
  | _ => none

/-- Using an exclusive reference binding `r` (`&mut T`) mutably while another live reference
    borrows the same storage — a reborrow of `r` (`let s = &*r`) or another borrow of `r`'s
    source — is the aliasing E0232 forbids for an owned binding (Codex review of round 3). -/
def checkExclusiveRefUse (r : String) (span : Option Span) : CheckM Unit := do
  match ← lookupVarInfo r with
  | some info =>
    match info.ty with
    | .refMut _ =>
      let env ← getEnv
      let roots := r :: info.borrowedFrom.toList
      let others := env.vars.filter fun (n, vi) =>
        n != r && vi.state != .consumed &&
          (match vi.borrowedFrom with | some src => roots.contains src | none => false)
      if !others.isEmpty then
        throwCheck (.cannotMutBorrowAlreadyBorrowed r) span
    | _ => pure ()
  | none => pure ()

/-- The explicit-borrow rules of `&mut place`, shared by explicit `&mut e` and an implicit
    `&mut self` receiver borrow (bug 091 round 3). A bare binding: not moved, not borrowed
    (E0232), declared `mut` (E0233). Any other place: never reached through a shared
    reference (E0237); an owned root must be `mut` and unborrowed, as the binding itself. -/
def checkMutBorrowPlace (place : Expr) (subject : String) (span : Option Span) : CheckM Unit := do
  let checkRoot (root : String) : CheckM Unit := do
    match ← lookupVarInfo root with
    | none => pure ()
    | some info =>
      if !info.isCopy && info.state == .consumed then
        throwCheck (.cannotBorrowMoved root) span
      let activeRefs := activeBorrowRefs (← getEnv) root
      if info.borrowCount > 0 || activeRefs.any (fun r => match r.ty with | .ref _ | .refMut _ => true | _ => false) then
        throwCheck (.cannotMutBorrowAlreadyBorrowed root) span
      if !info.mutable then
        throwCheck (.cannotMutBorrowImmutable root) span
  match stripParens place with
  | .ident _ v =>
    match ← lookupVarInfo v with
    | some _ => checkRoot v
    | none => throwCheck (.undeclaredVariable v) span
  | other =>
    match ← placeVia other with
    | .shared desc => throwCheck (.mutReceiverThroughSharedRef subject desc) span
    | .owned root => checkRoot root
    | .exclusiveRef =>
      match placePathRoot other with
      | some r => checkExclusiveRefUse r span
      | none => pure ()
    | _ => pure ()

/-- The explicit-borrow rule of `&place`: an owned root is not under a live `&mut` (E0231). -/
def checkSharedBorrowPlace (place : Expr) (span : Option Span) : CheckM Unit := do
  match ← placeVia place with
  | .owned root =>
    match ← lookupVarInfo root with
    | some info =>
      if !info.isCopy && info.state == .consumed then
        throwCheck (.cannotBorrowMoved root) span
      let activeRefs := activeBorrowRefs (← getEnv) root
      if activeRefs.any (fun r => match r.ty with | .refMut _ => true | _ => false) then
        throwCheck (.cannotBorrowMutablyBorrowed root) span
    | none => pure ()
  | _ => pure ()

/-- Receiver ownership of a method call, ONE rule for a concrete receiver and a receiver typed
    by a trait-bounded type parameter (Codex round 2 of bug 091: the bound path never applied
    the receiver form, so `x.consume(); return x;` returned an owner its impl had destroyed):
      * `&self`: never consumes (a shared reference is Copy);
      * `&mut self`: consumes only a borrow-block exclusive ref (a scoped linear view);
      * `self`: MOVES the receiver. An owned binding is consumed; a projection, or a
        reference, leaves the place owning the same value, so a non-Copy receiver there is
        refused (H11) — moving out through `&T` destroyed a value its owner still held. -/
def consumeReceiver (selfKind : Option SelfKind) (obj0 : Expr) (objTy innerTy : Ty)
    (methodName : String) (span : Option Span) : CheckM Unit := do
  -- `(x).m()` is `x.m()`: parentheses never hide the receiver place (Codex round 3).
  let obj := stripParens obj0
  match selfKind with
  | some .refMut =>
    match objTy, obj with
    | .refMut _, .ident _ varName =>
      if (← getEnv).borrowRefs.contains varName then consumeVarIfExists varName span
    | _, _ => pure ()
  | some .value =>
    let refusedRef := throwCheck (.nonCopyProjection (tyToString innerTy)
      s!"a reference (by-value `self` receiver of '{methodName}')") span
    let refused := throwCheck (.nonCopyProjection (tyToString innerTy)
      s!"place projection (by-value `self` receiver of '{methodName}')") span
    match objTy with
    | .ref _ | .refMut _ =>
      if !(← isCopyType innerTy) then refusedRef
    | _ =>
      -- A value reached through a reference (`(*r).m()`, `w.0.m()` with `w: &W`, ...)
      -- moves out of the reference whatever the spelling (Codex review of round 3).
      match ← receiverVia obj with
      | .shared _ | .exclusiveRef =>
        if !(← isCopyType innerTy) then refusedRef
      | _ =>
        let obj := match obj with
          | .fieldAccess sp base f => Expr.fieldAccess sp (stripParens base) f
          | o => o
        match obj with
        | .ident _ varName => consumeVarIfExists varName span
        | .fieldAccess _ (.ident _ _) f =>
          -- `v.0.drop()` on an OWNED newtype ident is a whole-owner move: the fieldAccess
          -- already consumed `v`. Any other field projection leaves the place owning the
          -- same value.
          if f != newtypeFieldName && !(← isCopyType innerTy) then refused
        | .fieldAccess .. | .arrayIndex .. =>
          if !(← isCopyType innerTy) then refused
        | _ => pure ()
  | _ => pure ()

/-- Bug 091 round 3: a method call's IMPLICIT receiver borrow obeys the rules of the explicit
    `&place` / `&mut place` it stands for (one rule for concrete and trait-bound receivers):
      * `&mut self` through a shared reference — the receiver itself (`x: &T`, `self` in a
        `&self` method) or any `&` on the place's path (`w.p` with `w: &W`, `*(&p)`) — is
        E0237. For `x: &T` Elab built a `&mut &T`, so the callee read and wrote the pointer's
        slot as if it held the value (out of bounds);
      * `&mut self` on an owned place needs a `mut` binding (E0233) with no live borrow of it
        (E0232), exactly as `&mut x` does (`checkMutBorrowPlace`, shared with explicit `&mut`);
      * `&self` on an owned place cannot overlap a live `&mut` of it (E0231), as `&x` cannot.
    A receiver that already is `&mut T` is passed as-is; a temporary has no binding to check. -/
def checkImplicitReceiverBorrow (selfKind : Option SelfKind) (obj : Expr) (objTy : Ty)
    (methodName : String) (span : Option Span) : CheckM Unit := do
  let subject := s!"the receiver of `&mut self` method '{methodName}'"
  match selfKind with
  | some .refMut =>
    match objTy with
    | .refMut _ =>
      -- Passed as-is, but the `&mut` must be reachable without crossing a `&` (`(*s).m()`
      -- with `s: &&mut P`), and no other live reference may share its storage.
      match stripParens obj with
      | .ident _ n => checkExclusiveRefUse n span
      | _ =>
        match ← placeVia obj with
        | .shared desc => throwCheck (.mutReceiverThroughSharedRef subject desc) span
        | _ =>
          match placePathRoot obj with
          | some r => checkExclusiveRefUse r span
          | none => pure ()
    | .ref _ =>
      throwCheck (.mutReceiverThroughSharedRef subject "the receiver is a `&` reference") span
    | _ =>
      match ← receiverVia obj with
      | .temp => pure ()
      | _ => checkMutBorrowPlace obj subject span
  | some .ref =>
    match objTy with
    | .ref _ | .refMut _ => pure ()
    | _ =>
      match ← receiverVia obj with
      | .temp => pure ()
      | _ => checkSharedBorrowPlace obj span
  | _ => pure ()

/-- Bug 091 round 3: `Destroy::destroy` is the PUBLIC destruction entry point, and its
    ownership contract is CONSUMING whichever receiver an impl declares (`&self` or `self`):
    calling it ends the receiver's life exactly as `destroy(x)` does. Admitting a borrowed
    receiver let `kill<T: Destroy>(x: &T) { x.destroy(); }` (or a concrete `t.destroy()`
    through `&T`) run an owner's destructor while that owner stayed live, so it was later
    destroyed again (double free for a resource-releasing destructor). A reference receiver
    is admitted only in `trusted` code — internal drop glue (std's collections destroying an
    element through `(p as &T)`) whose owner was already consumed by the container. -/
def destroyThroughRefRefused (innerTy : Ty) (span : Option Span) : CheckM Unit := do
  if !(← getEnv).currentFnTrusted then
    throwCheck (.nonCopyProjection (tyToString innerTy)
      "a reference (`Destroy::destroy` consumes its receiver; destroying through a reference is reserved for trusted drop glue whose owner was already consumed)") span

def consumeDestroyReceiver (obj0 : Expr) (objTy innerTy : Ty) (span : Option Span) : CheckM Unit := do
  let obj := stripParens obj0
  match objTy with
  | .ref _ | .refMut _ => destroyThroughRefRefused innerTy span
  | _ =>
    -- `(*r).destroy()`, `w.0.destroy()` with `w: &W`, ...: destroying a value reached
    -- through a reference or raw pointer is drop glue, trusted only.
    match ← receiverVia obj with
    | .shared _ | .exclusiveRef | .rawPtr => destroyThroughRefRefused innerTy span
    | _ => consumeReceiver (some .value) obj objTy innerTy destroyMethodName span

/-- A Destroy impl's `&self` destructor named DIRECTLY — `Type::destroy(&x)`, its definition
    name `Type_destroy(&x)`, or a function reference to it — runs the destructor through a
    borrow and leaves the owner live (Codex round 3: printed 12111 for 11111). Outside trusted
    drop glue that is refused like a borrowed `x.destroy()`; a by-value impl's definition takes
    its argument by value and is an ordinary consuming call. -/
def checkDirectDestroyImplUse (fnName : String) (span : Option Span) : CheckM Unit := do
  let env ← getEnv
  if env.currentFnTrusted then return
  match env.traitImpls.find? fun (t, tr) => tr == destroyTraitName && destroyFnNameFor t == fnName with
  | none => pure ()
  | some (t, _) =>
    match ← lookupFn fnName with
    | some sig =>
      match sig.params.head? with
      | some (_, .ref _) | some (_, .refMut _) => destroyThroughRefRefused (tyFromName t) span
      | _ => pure ()
    | none => pure ()

-- LEAN 4.34.1 `do` ELABORATOR (R-0208 upgrade). 4.34.1 elaborates `do` blocks with a new elaborator
-- by default, and on this very large mutual block it exceeded the default 200 000 heartbeats (it fit
-- under 4.28.0). The legacy elaborator handles it within the default budget. Elaboration strategy
-- only: the definitions are unchanged. Remove when the new elaborator copes with blocks this size.
set_option backward.do.legacy true in
mutual

partial def checkExpr (e : Expr) (hint : Option Ty := none) (mode : UseMode := .value) : CheckM Ty := do
  match e with
  | .intLit sp n =>
    -- One shared decision (TypeJudgment): the adopted type + the range
    -- obligation. Check stamps the type AND enforces the range; Elab stamps the
    -- same type from the same judgment, so the two cannot disagree (E0228).
    -- Rejecting an out-of-range literal here prevents silent truncation
    -- (e.g. `let a: u8 = 300` must not become 44).
    let hintR ← match hint with
      | some ty => do let t ← resolveType ty; pure (some t)
      | none => pure none
    let d := TypeJudgment.intLitDecision hintR
    match d.range with
    | some (lo, hi, nm) =>
      if n < lo || n > hi then
        throwCheck (.intLiteralOutOfRange n nm lo hi) (some sp)
    | none => pure ()
    return d.ty
  | .floatLit _ _ =>
    let hintR ← match hint with
      | some ty => do let t ← resolveType ty; pure (some t)
      | none => pure none
    return TypeJudgment.floatLitType hintR
  | .boolLit _ _ => return .bool
  | .strLit _ _ => return .string
  | .charLit _ _ => return .char
  | .ident _ name =>
    -- First check if it's a constant
    let env ← getEnv
    match env.constants.lookup name with
    | some ty => return ty
    | none =>
    match ← lookupVarInfo name with
    | some info =>
      if mode == .value then
        -- AUTO-CONSUME (the inversion): in value position, the ident read IS
        -- the move of a non-Copy binding. useVar first — the FROZEN check
        -- applies to Copy reads too (consumeVar early-returns for Copy);
        -- then consumeVar handles every non-Copy state (use-after-move
        -- E0205, reserved, the loop rule). `breakConsumeExempt` is the H14
        -- one-level loop-depth exemption, set only around a `break value;`.
        useVar name (some e.getSpan)
        let env ← getEnv
        consumeVarIfExists name (some e.getSpan)
          (breakDepthExempt := if env.breakConsumeExempt then 1 else 0)
      else
        -- place/callArg: reading, not consuming. Still reject a read of an
        -- already-moved value.
        if !info.isCopy && info.state == .consumed then
          -- secondary span: where the value was moved (Phase 4 #11).
          throwCheck (.variableUsedAfterMove name) (some e.getSpan)
            (related := info.movedAt.toList.map (fun sp => (sp, s!"'{name}' moved here")))
        useVar name (some e.getSpan)
      return info.ty
    | none =>
      -- Check if it's a function name (first-class function reference)
      match ← lookupFn name with
      | some sig =>
        checkDirectDestroyImplUse name (some e.getSpan)
        let paramTys := sig.params.map fun (_, t) => t
        return .fn_ paramTys sig.capSet sig.retTy
      | none => throwCheck (.undeclaredVariable name) (some e.getSpan)
  | .binOp _ op lhs rhs =>
    -- A literal-only side is flexible: it adopts the type of the OTHER operand
    -- (mirroring Core elaboration), not the surrounding hint — `inner[64 + i]`
    -- with i: i32 types 64 as i32 even though the index hint is Int. The
    -- operand-order decision is shared with Elab (TypeJudgment.binOpOperandOrder)
    -- so both front-end passes type the two sides identically.
    let (lTy, lTyR, rTyR) ←
      match TypeJudgment.binOpOperandOrder
              (TypeJudgment.isFlexibleLit lhs) (TypeJudgment.isFlexibleLit rhs) with
      | .rhsFirst => do
        let rTy ← checkExpr rhs hint
        let rTyR ← resolveType rTy
        let lTy ← checkExpr lhs (some rTyR)
        let lTyR ← resolveType lTy
        pure (lTy, lTyR, rTyR)
      | .lhsFirst => do
        let lTy ← checkExpr lhs hint
        let lTyR ← resolveType lTy
        let rTy ← checkExpr rhs (some lTyR)
        let rTyR ← resolveType rTy
        pure (lTy, lTyR, rTyR)
    let isTypeVarL := match lTyR with | .typeVar _ => true | _ => false
    let isTypeVarR := match rTyR with | .typeVar _ => true | _ => false
    -- Concrete numeric operands must agree EXACTLY (width and signedness).
    -- A mixed-width pair has no single-width SSA lowering; it used to slip
    -- past check (and run under --interp) only to die at SSA-verify (E0715).
    if isNumeric lTyR && isNumeric rTyR && lTyR != rTyR then
      match op with
      | .and_ | .or_ => pure ()  -- bool-only ops; the non-bool operand is the real error
      | _ => throwCheck (.binOpOperandMismatch (binOpSymbol op) (tyToString lTyR) (tyToString rTyR)) (some e.getSpan)
    match op with
    | .add | .sub | .mul | .div | .mod =>
      if isInteger lTyR && lTyR == rTyR then return lTy
      else if isFloatType lTyR && lTyR == rTyR then return lTy
      else if lTyR == .char && rTyR == .char then return .char
      else if isPointerType lTyR && isInteger rTyR then return lTy
      else if isTypeVarL || isTypeVarR then return lTy
      else return lTy  -- CoreCheck validates operator type constraints
    | .wrappingAdd | .wrappingSub | .wrappingMul
    | .saturatingAdd | .saturatingSub | .saturatingMul =>
      -- wrapping_*/saturating_* never reach here via AST `binaryOp` — they are
      -- call-syntax intrinsics, type-checked in the call path. Defensive.
      return lTy
    | .eq | .neq | .lt | .gt | .leq | .geq =>
      return .bool  -- CoreCheck validates operand type compatibility
    | .and_ | .or_ =>
      return .bool  -- CoreCheck validates logical operator types
    | .bitand | .bitor | .bitxor | .shl | .shr =>
      if isInteger lTyR && lTyR == rTyR then return lTy
      else if isTypeVarL || isTypeVarR then return lTy
      else return lTy  -- CoreCheck validates bitwise operator types
  | .unaryOp usp op operand =>
    match op with
    | .neg =>
      -- A negated integer literal (`-N`) must fit the target type: reject
      -- `let a: u8 = -1` (would wrap to 255) and `let a: i8 = -200`. Range-check
      -- the *negated* value `-N` directly — do NOT range-check the positive inner
      -- literal, since e.g. i8's valid minimum -128 has inner 128 > i8 max (127).
      match operand with
      | .intLit _ litN =>
        let ty ← match hint with
          | some h => do let hr ← resolveType h; pure (if isInteger hr then hr else Ty.int)
          | none => pure Ty.int
        match intTyRange ty with
        | some (lo, hi, nm) =>
          if (-litN) < lo || (-litN) > hi then
            throwCheck (.intLiteralOutOfRange (-litN) nm lo hi) (some usp)
        | none => pure ()
        return ty
      | _ =>
        let ty ← checkExpr operand hint
        return ty  -- CoreCheck validates negation types
    | .not_ =>
      let _ ← checkExpr operand hint
      return .bool  -- CoreCheck validates logical not types
    | .bitnot =>
      let ty ← checkExpr operand hint
      return ty  -- CoreCheck validates bitwise not types
  | .allocCall _ inner allocExpr =>
    -- Check the allocator expression is valid
    let _allocTy ← checkExpr allocExpr
    -- Check the inner call expression
    checkExpr inner hint
  | .ifExpr _ cond then_ else_ =>
    -- if-as-expression: if cond { then_ } else { else_ }
    let condTy ← checkExpr cond
    if condTy != .bool then
      throwCheck (.ifCondNotBool (tyToString condTy)) (some e.getSpan)
    let env ← getEnv
    -- BRANCH ISOLATION (found by the linearity fuzzer, 2026-07-06): the two
    -- arms were checked SEQUENTIALLY against the same env, so
    -- `if c { v } else { v }` — both arms moving the SAME var, which is the
    -- agreement-correct form — was a spurious E0205 (the else-arm saw the
    -- then-arm's consume). Mirror the `.ifElse` statement machinery:
    -- snapshot, check each arm from the snapshot, then merge with agreement.
    let envBefore ← getEnv
    -- Check then branch: all stmts except the last, then check last for its type
    if blockExitsFunction then_ then
      setEnv { envBefore with inFnExitingBranch := true }
    let thenInit := then_.dropLast
    checkStmts thenInit env.currentRetTy
    let thenTy ← match then_.getLast? with
      | some (.expr _ tExpr true) => do
        let ty ← checkExpr tExpr hint
        -- value mode auto-consumes a trailing ident (VALUE_FLOW_SPEC)
        pure ty
      | some (.return_ _ v) =>
        match v with
        | some rv => let _ ← checkExpr rv; pure Ty.never
        | none => pure Ty.never
      | some other =>
        checkStmt other env.currentRetTy
        pure Ty.unit
      | none => pure Ty.unit
    let envAfterThen ← getEnv
    checkBlockLocalsConsumed envBefore.vars envAfterThen.vars (blockNonTerminating then_) (some e.getSpan)
    -- Check else branch from the SNAPSHOT (not the then-arm's aftermath)
    setEnv envBefore
    if blockExitsFunction else_ then
      setEnv { envBefore with inFnExitingBranch := true }
    let elseInit := else_.dropLast
    checkStmts elseInit env.currentRetTy
    let elseTy ← match else_.getLast? with
      | some (.expr _ eExpr true) => do
        let ty ← checkExpr eExpr hint
        pure ty
      | some (.return_ _ v) =>
        match v with
        | some rv => let _ ← checkExpr rv; pure Ty.never
        | none => pure Ty.never
      | some other =>
        checkStmt other env.currentRetTy
        pure Ty.unit
      | none => pure Ty.unit
    let envAfterElse ← getEnv
    checkBlockLocalsConsumed envBefore.vars envAfterElse.vars (blockNonTerminating else_) (some e.getSpan)
    if blockExitsFunction then_ then
      checkReturnPathConsumed envBefore.vars envAfterThen.vars (some e.getSpan)
    if blockExitsFunction else_ then
      checkReturnPathConsumed envBefore.vars envAfterElse.vars (some e.getSpan)
    -- Merge: fall-through arms must AGREE on consumption of outer linears.
    mergeVarStates envBefore.vars envAfterThen.vars envAfterElse.vars
      (blockDiverges then_) (blockDiverges else_) (some e.getSpan)
    -- Drop branch-locals; restore the exit flag.
    let envM ← getEnv
    setEnv { envM with vars := envM.vars.filter fun (_, vi) => hasUid envBefore.vars vi.uid,
                       inFnExitingBranch := envBefore.inFnExitingBranch }
    -- Both branches must agree on type
    if thenTy != elseTy && thenTy != .never && elseTy != .never then
      throwCheck (.ifBranchTypeMismatch (tyToString thenTy) (tyToString elseTy)) (some e.getSpan)
    if thenTy == .never then return elseTy else return thenTy
  | .call _sp fnName typeArgs args =>
    -- E0293: no overlapping borrows of one place within a single call (any &mut).
    checkCallBorrowConflicts (← borrowArgParts args) (some e.getSpan)
    checkDirectDestroyImplUse fnName (some e.getSpan)
    -- Intercept newtype wrapping: NewtypeName(expr)
    match ← lookupNewtype fnName with
    | some nt =>
      -- 0b construction rights: a newtype's direct constructor is private
      -- to its defining module. External callers use its public
      -- constructor fn (e.g. try_new) — this is what makes smart-constructor
      -- invariants (NonZeroU32's non-zero property) load-bearing rather
      -- than advisory.
      if !(← getEnv).localNewtypeNames.contains fnName then
        throwCheck (.newtypeConstructPrivate fnName) (some e.getSpan)
      if args.length != 1 then throwCheckMsg s!"newtype '{fnName}' constructor takes exactly 1 argument"
      if !typeArgs.isEmpty then throwCheckMsg s!"newtype '{fnName}' constructor does not take type arguments"
      -- For generic newtypes, infer type args from hint
      let inferredTypeArgs := if nt.typeParams.isEmpty then []
        else match hint with
          | some (.generic n hintArgs _) => if n == fnName then hintArgs else []
          | _ => []
      let mapping := nt.typeParams.zip inferredTypeArgs
      let resolvedInnerTy := substTy mapping nt.innerTy
      let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let argTy ← checkExpr arg (some resolvedInnerTy)
      expectTy resolvedInnerTy argTy s!"newtype '{fnName}' constructor" (some e.getSpan)
      if inferredTypeArgs.isEmpty then return .named fnName
      else return .generic fnName inferredTypeArgs []
    | none =>
    let env ← getEnv
    let isUserFn := env.userFnNames.contains fnName
    let intrinsic := if isUserFn then none else resolveIntrinsic fnName
    -- Intercept sizeof::<T>() and alignof::<T>() builtins
    if intrinsic == some .sizeof || intrinsic == some .alignof then
      if args.length != 0 then throwCheckMsg s!"{fnName} takes no value arguments"
      if typeArgs.length != 1 then throwCheckMsg s!"{fnName} requires exactly 1 type argument: {fnName}::<T>()"
      return .uint
    -- Intercept unwrap(x) for newtype unwrapping (only if not a user-defined function)
    if intrinsic == some .unwrap && args.length == 1 then
        let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
        let argTy ← checkExpr arg
        let ntName := match argTy with | .named n => n | _ => ""
        if ntName == "" then throwCheckMsg s!"unwrap() requires a newtype argument, got {tyToString argTy}"
        match ← lookupNewtype ntName with
        | some nt =>
          return nt.innerTy
        | none => throwCheckMsg s!"unwrap() requires a newtype argument, '{ntName}' is not a newtype"
    -- Intercept wrapping_*/saturating_*(a, b) — explicit modular/clamping
    -- arithmetic. Integer-only, both operands the same type; result is that type.
    if intrinsic == some .wrappingAdd || intrinsic == some .wrappingSub
       || intrinsic == some .wrappingMul || intrinsic == some .saturatingAdd
       || intrinsic == some .saturatingSub || intrinsic == some .saturatingMul then
      if args.length != 2 then
        throwCheckMsg s!"{fnName} takes exactly 2 arguments, got {args.length}"
      let a := match args with | a :: _ => a | [] => Expr.intLit default 0
      let b := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let aTy ← checkExpr a hint
      let aTyR ← resolveType aTy
      if !isInteger aTyR then
        throwCheckMsg s!"{fnName} requires integer operands, got {tyToString aTyR}"
      let bTy ← checkExpr b (some aTyR)
      let bTyR ← resolveType bTy
      if aTyR != bTyR then
        throwCheckMsg s!"{fnName} operands must have the same integer type, got {tyToString aTyR} and {tyToString bTyR}"
      return aTy
    -- Intercept print(...) / println(...) — variadic mixed-arg output
    if intrinsic == some .print || intrinsic == some .println then
      if args.isEmpty then throwCheckMsg s!"{fnName}() requires at least 1 argument"
      for arg in args do
        -- callArg: a String ident is AUTO-BORROWED by print (not moved)
        let argTy ← checkExpr arg none .callArg
        -- Accept: String (auto-borrowed), integer types, bool, char
        match argTy with
        | .string | .ref .string | .refMut .string => pure ()
        | .int | .uint | .i32 | .u32 | .i16 | .u16 | .i8 | .u8 => pure ()
        | .bool => pure ()
        | .char => pure ()
        | _ => throwCheckMsg s!"{fnName}() argument has unsupported type '{tyToString argTy}'; expected String/&String/&mut String, Int/Uint/i8..i32/u8..u32, bool, or char"
      return .unit
    -- Intercept append(&mut buf, ...) — variadic mixed-arg buffer append
    if intrinsic == some .append then
      match args with
      | bufArg :: rest =>
        let bufTy ← checkExpr bufArg none .callArg
        match bufTy with
        | .refMut .string => pure ()
        | _ => throwCheckMsg s!"append() first argument must be &mut String, got '{tyToString bufTy}'"
        if rest.isEmpty then throwCheckMsg s!"append() requires at least 2 arguments (&mut String, ...values)"
        for arg in rest do
          let argTy ← checkExpr arg
          match argTy with
          | .string | .ref .string | .refMut .string => pure ()
          | .int | .uint | .i32 | .u32 | .i16 | .u16 | .i8 | .u8 => pure ()
          | .bool => pure ()
          | .char => pure ()
          | _ => throwCheckMsg s!"append() argument has unsupported type '{tyToString argTy}'; expected String/&String/&mut String, Int/Uint/i8..i32/u8..u32, bool, or char"
        return .unit
      | [] => throwCheckMsg s!"append() requires at least 2 arguments (&mut String, ...values)"
    -- Intercept abort() calls
    if intrinsic == some .abort then
      if args.length != 0 then throwCheck (.builtinWrongArgCount "abort" 0) (some e.getSpan)
      return .never
    -- Intercept destroy() calls
    if intrinsic == some .destroy then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "destroy" 1) (some e.getSpan)
      let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let argTy ← checkExpr arg
      -- Look up impl Destroy for the type
      let typeName := match argTy with
        | .named n => n
        | .generic n _ _ => n
        | _ => ""
      if typeName == "" then throwCheck (.destroyRequiresNamed (tyToString argTy)) (some e.getSpan)
      -- Search function signatures for TypeName_destroy
      let destroyFn ← lookupFn (destroyFnNameFor typeName)
      match destroyFn with
      | some _ =>
        return .unit
      | none => throwCheck (.typeDoesNotImplDestroy typeName) (some e.getSpan)
    -- Intercept discard() calls — the acknowledged-discard escape (slice 5).
    -- `discard(e)` evaluates `e` and drops its value, marking the discard
    -- intentional so a pure statement is not flagged as a dead computation
    -- (E0294). Only a Copy value may be discarded this way; a non-Copy resource
    -- must be consumed or `destroy()`d (dropping it would leak it).
    if intrinsic == some .discard then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "discard" 1) (some e.getSpan)
      let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let argTy ← checkExpr arg
      if !(← isCopyType argTy) then throwCheck (.discardNonCopy (tyToString argTy)) (some e.getSpan)
      return .unit
    -- Intercept alloc(val) calls
    if intrinsic == some .alloc then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "alloc" 1) (some e.getSpan)
      let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let argTy ← checkExpr arg
      return .heap argTy
    -- Intercept free(ptr) calls
    if intrinsic == some .free then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "free" 1) (some e.getSpan)
      let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let argTy ← checkExpr arg
      match argTy with
      | .heap innerTy =>
        return innerTy
      | _ => throwCheck (.freeRequiresHeap (tyToString argTy)) (some e.getSpan)
    -- Intercept vec_new::<T>()
    if intrinsic == some .vecNew then
      if args.length != 0 then throwCheck (.builtinWrongArgCount "vec_new" 0) (some e.getSpan)
      if typeArgs.length != 1 then throwCheck (.builtinWrongTypeArgCount "vec_new" "1 type argument: vec_new::<T>()") (some e.getSpan)
      let elemTy := match typeArgs with | t :: _ => t | [] => Ty.int
      return .generic "Vec" [elemTy] []
    -- Intercept string_push_char(&mut s, ch)
    if intrinsic == some .stringPushChar then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "string_push_char" 2) (some e.getSpan)
      let strArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let chArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let strTy ← checkExpr strArg none .callArg
      match strTy with
      | .refMut .string => pure ()
      | _ => throwCheck (.builtinWrongFirstArg "string_push_char" "&mut String as first argument" (tyToString strTy)) (some e.getSpan)
      let chTy ← checkExpr chArg (some .int)
      expectTy .int chTy "string_push_char() char argument" (some e.getSpan)
      return .unit
    -- Intercept string_append(&mut s, other)
    if intrinsic == some .stringAppend then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "string_append" 2) (some e.getSpan)
      let strArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let otherArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let strTy ← checkExpr strArg none .callArg
      match strTy with
      | .refMut .string => pure ()
      | _ => throwCheck (.builtinWrongFirstArg "string_append" "&mut String as first argument" (tyToString strTy)) (some e.getSpan)
      let otherTy ← checkExpr otherArg (some (.ref .string))
      expectTy (.ref .string) otherTy "string_append() second argument" (some e.getSpan)
      return .unit
    -- Intercept string_append_int(&mut s, n)
    if intrinsic == some .stringAppendInt then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "string_append_int" 2) (some e.getSpan)
      let strArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let nArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let strTy ← checkExpr strArg none .callArg
      match strTy with
      | .refMut .string => pure ()
      | _ => throwCheck (.builtinWrongFirstArg "string_append_int" "&mut String as first argument" (tyToString strTy)) (some e.getSpan)
      let nTy ← checkExpr nArg (some .int)
      expectTy .int nTy "string_append_int() second argument" (some e.getSpan)
      return .unit
    -- Intercept string_append_bool(&mut s, b)
    if intrinsic == some .stringAppendBool then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "string_append_bool" 2) (some e.getSpan)
      let strArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let bArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let strTy ← checkExpr strArg none .callArg
      match strTy with
      | .refMut .string => pure ()
      | _ => throwCheck (.builtinWrongFirstArg "string_append_bool" "&mut String as first argument" (tyToString strTy)) (some e.getSpan)
      let bTy ← checkExpr bArg (some .bool)
      expectTy .bool bTy "string_append_bool() second argument" (some e.getSpan)
      return .unit
    -- Intercept string_reserve(&mut s, cap)
    if intrinsic == some .stringReserve then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "string_reserve" 2) (some e.getSpan)
      let strArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let capArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let strTy ← checkExpr strArg none .callArg
      match strTy with
      | .refMut .string => pure ()
      | _ => throwCheck (.builtinWrongFirstArg "string_reserve" "&mut String as first argument" (tyToString strTy)) (some e.getSpan)
      let capTy ← checkExpr capArg (some .int)
      expectTy .int capTy "string_reserve() capacity argument" (some e.getSpan)
      return .unit
    -- Intercept vec_push(&mut v, val)
    if intrinsic == some .vecPush then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "vec_push" 2) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let valArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let vecTy ← checkExpr vecArg none .callArg
      let elemTy := match vecTy with
        | .refMut (.generic "Vec" [et] _) => et
        | _ => Ty.placeholder
      if elemTy == .placeholder then throwCheck (.builtinWrongFirstArg "vec_push" "&mut Vec<T> as first argument" (tyToString vecTy)) (some e.getSpan)
      let valTy ← checkExpr valArg (some elemTy)
      expectTy elemTy valTy "vec_push() element argument" (some e.getSpan)
      return .unit
    -- Intercept vec_get(&v, idx)
    if intrinsic == some .vecGet then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "vec_get" 2) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let idxArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let vecTy ← checkExpr vecArg none .callArg
      let elemTy := match vecTy with
        | .ref (.generic "Vec" [et] _) => et
        | .refMut (.generic "Vec" [et] _) => et
        | _ => Ty.placeholder
      if elemTy == .placeholder then throwCheck (.builtinWrongFirstArg "vec_get" "&Vec<T> or &mut Vec<T> as first argument" (tyToString vecTy)) (some e.getSpan)
      let idxTy ← checkExpr idxArg (some .int)
      expectTy .int idxTy "vec_get() index argument" (some e.getSpan)
      return elemTy
    -- Intercept vec_set(&mut v, idx, val)
    if intrinsic == some .vecSet then
      if args.length != 3 then throwCheck (.builtinWrongArgCount "vec_set" 3) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let idxArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let valArg := match args with | _ :: _ :: c :: _ => c | _ => Expr.intLit default 0
      let vecTy ← checkExpr vecArg none .callArg
      let elemTy := match vecTy with
        | .refMut (.generic "Vec" [et] _) => et
        | _ => Ty.placeholder
      if elemTy == .placeholder then throwCheck (.builtinWrongFirstArg "vec_set" "&mut Vec<T> as first argument" (tyToString vecTy)) (some e.getSpan)
      let idxTy ← checkExpr idxArg (some .int)
      expectTy .int idxTy "vec_set() index argument" (some e.getSpan)
      let valTy ← checkExpr valArg (some elemTy)
      expectTy elemTy valTy "vec_set() value argument" (some e.getSpan)
      return .unit
    -- Intercept vec_len(&v)
    if intrinsic == some .vecLen then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "vec_len" 1) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let vecTy ← checkExpr vecArg none .callArg
      let ok := match vecTy with
        | .ref (.generic "Vec" _ _) => true
        | .refMut (.generic "Vec" _ _) => true
        | _ => false
      if !ok then throwCheck (.builtinWrongFirstArg "vec_len" "&Vec<T> or &mut Vec<T> as argument" (tyToString vecTy)) (some e.getSpan)
      return .int
    -- Intercept vec_pop(&mut v)
    if intrinsic == some .vecPop then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "vec_pop" 1) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let vecTy ← checkExpr vecArg none .callArg
      let elemTy := match vecTy with
        | .refMut (.generic "Vec" [et] _) => et
        | _ => Ty.placeholder
      if elemTy == .placeholder then throwCheck (.builtinWrongFirstArg "vec_pop" "&mut Vec<T> as argument" (tyToString vecTy)) (some e.getSpan)
      return .generic optionEnumName [elemTy] []
    -- Intercept vec_free(v)
    if intrinsic == some .vecFree then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "vec_free" 1) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let vecTy ← checkExpr vecArg none .callArg
      let ok := match vecTy with
        | .generic "Vec" _ _ => true
        | _ => false
      if !ok then throwCheck (.builtinWrongFirstArg "vec_free" "Vec<T> as argument" (tyToString vecTy)) (some e.getSpan)
      match vecArg with
      | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
      | _ => pure ()
      return .unit
    -- Check if this is a function pointer call (variable with fn_ type)
    let fnPtrVarTy ← lookupVarTy fnName
    match fnPtrVarTy with
    | some (.fn_ paramTys fnPtrCapSet fnPtrRetTy) =>
      -- Calling through a function pointer exercises the authority its type
      -- declares: the caller must hold the fn type's capability set, exactly
      -- as for a direct call. Without this, a function with no `with(...)`
      -- could accept `f: fn(i32) with(Network) -> i32` and call it —
      -- capability smuggling (ROADMAP Phase 5 #24a red-team gate).
      do
        let env ← getEnv
        -- Anti-smuggling: an indirect call must hold the pointer type's caps AND
        -- its cap variables (Capabilities.missingCapsThroughPtr) — the stricter
        -- through-pointer variant of the one shared missing-caps decision.
        for cap in Capabilities.missingCapsThroughPtr env.currentCapSet fnPtrCapSet do
          throwCheck (.missingCapability fnName cap env.currentFnName) (some e.getSpan)
      -- Check argument count
      if args.length != paramTys.length then
        throwCheck (.wrongArgCount s!"function pointer '{fnName}'" paramTys.length args.length) (some e.getSpan)
      -- Check each argument type
      for (arg, pTy) in args.zip paramTys do
        let argTy ← checkExpr arg (some pTy) .callArg
        expectTy pTy argTy s!"argument of function pointer call '{fnName}'" (some e.getSpan)
        match stripParens arg with
        | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
      -- Function pointers are Copy, no need to consume
      useVar fnName (some e.getSpan)
      return fnPtrRetTy
    | _ =>
    match ← lookupFn fnName with
    | some sig =>
      -- Infer type arguments if not explicitly provided
      let inferredTypeArgs ← do
        if !typeArgs.isEmpty || sig.typeParams.isEmpty then
          pure typeArgs
        else
          -- Infer types from argument types (without consuming)
          let mut inferred : List (String × Ty) := []
          for (arg, (_, pTy)) in args.zip sig.params do
            let argTy ← peekExprType arg
            -- Try to unify pTy with argTy to learn type variables
            let bindings := unifyTypes pTy argTy sig.typeParams
            for (name, ty) in bindings do
              if !(inferred.any fun (n, _) => n == name) then
                inferred := inferred ++ [(name, ty)]
          -- Build ordered type args from inferred mapping
          pure (sig.typeParams.map fun tp =>
            match inferred.lookup tp with
            | some ty => ty
            | none => .typeVar tp)
      -- Build type substitution
      let mapping := sig.typeParams.zip inferredTypeArgs
      -- References are second-class: a type parameter instantiated to a
      -- reference may not occur in the return type (VALUE_MODEL.md).
      for (pName, pTy) in mapping do
        if tyContainsRef pTy && tyParamOccursIn pName sig.retTy then
          throwCheckMsg s!"generic function '{fnName}': type parameter '{pName}' is instantiated to a reference ('{tyToString pTy}') and occurs in the return type; references may not be returned (VALUE_MODEL.md). Return a value, an owned view, or use a scoped callback."
      -- Check trait bounds
      if !sig.typeBounds.isEmpty then
        checkTraitBounds sig.typeBounds mapping s!"generic function '{fnName}'"
      let paramTypes := sig.params.map fun (n, t) => (n, substTy mapping t)
      let retTy := substTy mapping sig.retTy
      -- Resolve capability variables from argument types
      -- The inferred bindings are returned alongside the resolved set: substituting a
      -- capability parameter inside a capability argument (`&Reader<C>`) needs what C
      -- was bound to, which cannot be recovered from the resolved set (that also holds
      -- the signature's own capabilities, and drops names that coincide with C).
      let (resolvedCapSet, inferredCapBindings) ← do
        if sig.capParams.isEmpty then
          pure (sig.capSet, ([] : List (String × List String)))
        else
          let mut capBindings : List (String × List String) := []
          -- Infer cap variable bindings from fn-typed arguments
          for (arg, (_, pTy)) in args.zip paramTypes do
            match pTy with
            | .fn_ _ (.concrete caps) _ =>
              for cap in caps do
                if sig.capParams.contains cap then
                  -- Get actual argument's cap set
                  -- BUG 063. `none` means "this argument's capability set is not known
                  -- here", and it must stay distinguishable from `some .empty`, which
                  -- means "this callback requires no authority". Recording the first as
                  -- the second is absence of information stored as a positive fact, and
                  -- it is not a conservative approximation of the missing analysis — it
                  -- is the opposite one.
                  --
                  -- The consequence was not a wrong answer but a wrong ACCUSATION:
                  -- `missingCaps` ran against the fabricated `C := {}` and found nothing
                  -- missing, so the authority check passed, and the program was then
                  -- rejected downstream by `expectTy` comparing `with()` against
                  -- `with(Console)` — a type-equality check that has nothing to do with
                  -- capabilities and happened to sit in the way. Relaxing fn-type cap
                  -- comparison to subsetting, which is the natural direction for passing
                  -- a low-authority callback where a high-authority one is expected,
                  -- would have removed the only thing rejecting these programs and
                  -- turned this into an authority hole.
                  --
                  -- Contributing nothing lets `resolveCaps` throw the variable it could
                  -- not infer, which is `cannotInferCapVariable` naming the actual
                  -- problem at the actual place.
                  let argCapSet? ← do
                    let argTy ← peekExprType arg
                    match argTy with
                    | .fn_ _ cs _ => pure (some cs)
                    | _ =>
                      match stripParens arg with
                      | .ident _ varName =>
                        match ← lookupFn varName with
                        | some argSig => pure (some argSig.capSet)
                        | none => pure none
                      | _ => pure none
                  match argCapSet? with
                  | some cs =>
                    let (argCaps, _) := cs.normalize
                    capBindings := capBindings ++ [(cap, argCaps)]
                  | none => pure ()
            | _ =>
              -- R-0484: a capability parameter carried by a capability ARGUMENT
              -- (`s: &Sink<C>`) is read off the argument's type (`&Sink<{}>`). The
              -- argument is only peeked here; it is checked, once, below.
              if (tyCapNames pTy).any sig.capParams.contains then
                let argTy ← peekExprType arg
                for (k, v) in inferCapBindings sig.capParams pTy argTy do
                  if !(capBindings.any (·.1 == k)) then capBindings := (k, v.allNames) :: capBindings
          -- Resolve the cap-poly signature against the bindings (shared with the
          -- method-call path: Capabilities.resolveCaps). Error carries the cap
          -- variable that could not be inferred.
          match Capabilities.resolveCaps sig.capParams capBindings sig.capSet with
          | .ok resolvedCaps => pure (CapSet.concrete resolvedCaps, capBindings)
          | .error cv => throwCheck (.cannotInferCapVariable cv fnName) (some e.getSpan)
      -- Resolve cap variables in parameter types for type comparison
      let capBindings' := if sig.capParams.isEmpty then [] else
        sig.capParams.map fun cp =>
          match resolvedCapSet with
          | .concrete caps => (cp, caps.filter fun c => !sig.capParams.contains c)
          | _ => (cp, ([] : List String))
      let resolveCapInTy : Ty → Ty := fun ty =>
        match ty with
        | .fn_ params (.concrete caps) ret =>
          let newCaps := caps.foldl (fun acc cap =>
            if sig.capParams.contains cap then
              match capBindings'.find? fun (n, _) => n == cap with
              | some (_, resolved) => acc ++ resolved
              | none => acc
            else acc ++ [cap]) []
          .fn_ params (.concrete newCaps) ret
        | t => t
      -- Capability parameters inside capability arguments (`&Sink<C>`) get the same
      -- bindings, so the parameter is compared at its instantiated type.
      let capArgMap : List (String × CapSet) := sig.capParams.filterMap fun cp =>
        (inferredCapBindings.find? (·.1 == cp)).map fun (_, caps) =>
          (cp, (if caps.isEmpty then CapSet.empty else CapSet.concrete caps).canonArg)
      let paramTypes := paramTypes.map fun (n, t) => (n, substCapNamesTy capArgMap (resolveCapInTy t))
      -- Check resolved capabilities for cap-polymorphic calls (cap variable inference)
      if !sig.capParams.isEmpty then
        let env ← getEnv
        -- One shared direct-call decision (Capabilities.missingCaps): the caps the
        -- caller lacks for this call. Same computation CoreCheck's satisfaction
        -- check and the reports read, so they cannot disagree.
        -- `Unsafe` is left to CoreCheck, which owns the trusted-body discharge and checks
        -- every call (see the method path in CheckHelpers).
        for cap in (Capabilities.missingCaps env.currentCapSet resolvedCapSet).filter (· != unsafeCapName) do
          throwCheck (.missingCapability fnName cap env.currentFnName) (some e.getSpan)
      if args.length != paramTypes.length then
        throwCheck (.wrongArgCount s!"function '{fnName}'" paramTypes.length args.length) (some e.getSpan)
      for (arg, (pName, pTy)) in args.zip paramTypes do
        let argTy ← checkExpr arg (some pTy) .callArg
        expectTy pTy argTy s!"argument '{pName}' of '{fnName}'" (some e.getSpan)
        -- If arg is a bare identifier of a linear type, consume it —
        -- but NOT if the parameter type is a reference (borrow, not move).
        -- Exception: borrow-block exclusive refs (&mut T) must be consumed on call,
        -- since they are scoped linear views into an alloca.
        -- Function parameter &mut T refs are reborrowable and not consumed.
        match stripParens arg with
        | .ident _ varName =>
          match pTy with
          | .ref _ => pure ()  -- shared reference: Copy, no consume needed
          | .refMut _ =>
            let env ← getEnv
            if env.borrowRefs.contains varName then
              consumeVarIfExists varName (some e.getSpan)
            else pure ()
          | _ => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
      return retTy
    | none =>
      -- sizeof intrinsic
      if intrinsic == some .sizeof || fnName.endsWith sizeofSuffix then return .uint
      else throwCheck (.undeclaredFunction fnName) (some e.getSpan)
  | .paren _ inner => checkExpr inner hint mode
  | .structLit _ name typeArgs fields base =>
    match ← lookupStruct name with
    | some sd =>
      -- 0b slice 2: a struct literal on a NON-local struct is allowed only
      -- if every field is `pub` (a private field can't be initialized from
      -- another module, so its representation can't be forged).
      if !(← getEnv).localStructNames.contains name then
        match sd.fields.find? (fun sf => !sf.isPublic) with
        | some priv => throwCheck (.structLiteralPrivateField name priv.name) (some e.getSpan)
        | none => pure ()
      -- Build type substitution from struct type params + provided type args
      let mapping := sd.typeParams.zip typeArgs
      -- Capability arguments (R-0484): taken from the expected type when it names this
      -- struct, otherwise read off the values of the fields whose types mention them.
      -- Each field expression is checked exactly once — the ones used for inference
      -- here, the rest below — so linearity is never counted twice.
      let capParams := sd.capParams
      let hintCaps : Option (List CapSet) := match hint with
        | some (.generic hn _ hc) =>
          if hn == name && !capParams.isEmpty && hc.length == capParams.length then some hc else none
        | _ => none
      let mut capMap : List (String × CapSet) := []
      let mut prechecked : List (String × Ty) := []
      if !capParams.isEmpty then
        match hintCaps with
        | some hc => capMap := capParams.zip hc
        | none =>
          for sf in sd.fields do
            if (tyCapNames sf.ty).any capParams.contains then
              match fields.find? fun (fn, _) => fn == sf.name with
              | some (_, fexpr) =>
                let exprTy ← checkExpr fexpr none
                prechecked := (sf.name, exprTy) :: prechecked
                let raw ← resolveType (substTy mapping sf.ty)
                for (k, v) in inferCapBindings capParams raw exprTy do
                  if !(capMap.any (·.1 == k)) then capMap := (k, v) :: capMap
              | none => pure ()
          for c in capParams do
            if !(capMap.any (·.1 == c)) then
              throwCheck (.cannotInferCapVariable c name) (some e.getSpan)
      let capArgs := capParams.map fun c => ((capMap.lookup c).getD .empty).canonArg
      let structTy := if typeArgs.isEmpty && capParams.isEmpty then Ty.named name
                      else .generic name typeArgs capArgs
      -- A `..base` functional-update source must itself be this struct type.
      match base with
      | some b =>
        -- `..base` reads Copy fields OUT of base (E0220 guards non-Copy);
        -- base itself stays live — a place read, not a move.
        let bTy ← checkExpr b (some structTy) .place
        expectTy structTy bTy s!"`..base` of struct '{name}'" (some e.getSpan)
      | none => pure ()
      for sf in sd.fields do
        let fieldTy ← resolveType (substCapNamesTy capMap (substTy mapping sf.ty))
        match fields.find? fun (fn, _) => fn == sf.name with
        | some (_, expr) =>
          let exprTy ← match prechecked.lookup sf.name with
            | some t => pure t
            | none => checkExpr expr (some fieldTy)
          expectTy fieldTy exprTy s!"field '{sf.name}' of struct '{name}'" (some e.getSpan)
          -- Bug 092: a generic field instantiated to a reference (`Box { v: &s }`).
          if tyStoresRef exprTy then
            throwCheck (.refStoredInAggregate s!"field '{sf.name}' of struct literal '{name}'" (tyToString exprTy)) (some expr.getSpan)
        | none =>
          -- A missing field is supplied by `..base` if present; otherwise it is a
          -- real omission (unions allow partial initialization).
          if base.isSome then
            -- `..base` copies this field FROM base. If it is non-Copy, the value would
            -- be owned by both `base` and the result — a duplication (double-free).
            if !(← isCopyType fieldTy) then
              throwCheck (.functionalUpdateCopiesNonCopy sf.name) (some e.getSpan)
          else if !sd.isUnion then
            throwCheck (.missingFieldInLiteral sf.name s!"struct literal '{name}'") (some e.getSpan)
      for (fn, _) in fields do
        match sd.fields.find? fun sf => sf.name == fn with
        | some _ => pure ()
        | none => throwCheck (.unknownFieldInLiteral fn s!"struct literal '{name}'") (some e.getSpan)
      return structTy
    | none => throwCheck (.unknownStructType name) (some e.getSpan)
  | .fieldAccess _ obj field =>
    let objTy ← checkExpr obj none .place
    -- 6D#3: field access auto-derefs ONE permitted layer — & / &mut, and the
    -- heap shells Heap<T> / HeapArray<T> / &Heap<T> / &mut Heap<T> (the old
    -- `->` semantics folded into `.`; heap interiors are not linearity-tracked,
    -- same as the blessed `->` destructure).
    let innerTy := match objTy with
      | .heap t => t
      | .heapArray t => t
      | .ref (.heap t) => t
      | .refMut (.heap t) => t
      | .ref t => t
      | .refMut t => t
      | t => t
    -- Heap-shell access keeps the blessed `->` semantics: heap interiors are
    -- not linearity-tracked, so `h.next` + `free(h)` (the heap-node
    -- destructure) stays legal — H11 below is skipped for it.
    -- Only an OWNED shell: through `&Heap<W>` / `&mut Heap<W>` the owner is elsewhere and a
    -- by-value non-Copy field read would hand out a payload it still holds (bug 091 round 3).
    let isHeapShell := match objTy with
      | .heap _ | .heapArray _ => true
      | _ => false
    -- Extract struct name, type args and capability args for substitution
    let (structName, typeArgs, capArgs) := match innerTy with
      | .named n => (n, ([] : List Ty), ([] : List CapSet))
      | .generic n args caps => (n, args, caps)
      | .string => ("String", [], [])
      | _ => ("", [], [])
    if structName == "" then throwCheck .fieldAccessNonStruct (some e.getSpan)
    else
      -- Check for newtype .0 unwrap
      match ← lookupNewtype structName with
      | some nt =>
        if field == newtypeFieldName then
          -- Substitute type params for generic newtypes
          let mapping := nt.typeParams.zip typeArgs
          let innerFieldTy := substTy mapping nt.innerTy
          -- `.0` on an ident is a whole-owner move: the newtype var is consumed
          -- and the inner value is moved out (legal for non-Copy). On any other
          -- base (`w.nt.0`) nothing is consumed, so a non-Copy inner value
          -- would be owned twice — the H11 projection rule applies.
          -- Through a REFERENCE (`w: &W`) the owner is elsewhere and nothing is consumed:
          -- a by-value `w.0` would duplicate a non-Copy payload (bug 091 round 3).
          let viaRef ← match stripParens obj with
            | .ident _ varName => do
              match ← lookupVarInfo varName with
              | some info => pure (match info.ty with | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ => true | _ => false)
              | none => pure false
            | _ => pure false
          match stripParens obj with
          | .ident _ varName =>
            if viaRef then
              if mode != .place && !(← isCopyType innerFieldTy) then
                throwCheck (.nonCopyProjection (tyToString innerFieldTy) s!"newtype field '.0' of '{structName}' through a reference") (some e.getSpan)
            else consumeVarIfExists varName (some e.getSpan)
          | _ =>
            if mode != .place && !(← isCopyType innerFieldTy) then
              throwCheck (.nonCopyProjection (tyToString innerFieldTy) s!"newtype field '.0' of '{structName}'") (some e.getSpan)
          return innerFieldTy
        else throwCheckMsg s!"newtype '{structName}' only supports .0 field access"
      | none =>
      match ← lookupStruct structName with
      | some sd =>
        match sd.fields.find? fun f => f.name == field with
        | some f =>
          -- 0b slice 2: reading a PRIVATE field of a non-local struct is
          -- rejected (the `b.len` cross-module leak). Same-module reads and
          -- `pub` fields are fine.
          if !f.isPublic && !(← getEnv).localStructNames.contains structName then
            throwCheck (.fieldAccessPrivate structName field) (some e.getSpan)
          let mapping := sd.typeParams.zip typeArgs
          -- R-0484: the field's type at THIS instantiation — `s.f` on a `&Sink<Console>`
          -- is `fn(..) with(Console)`, not the declared `with(C)`.
          let fieldTy ← resolveType (substCapNamesTy (sd.capParams.zip capArgs) (substTy mapping f.ty))
          -- H11: a by-value read of a non-Copy field duplicates it (the struct
          -- still owns the same value). Legal forms: `&w.f`, `&mut w.f`, Copy
          -- fields, further projection (`w.f.g` — the OUTERMOST read decides),
          -- and destructuring the whole owner.
          if mode != .place && !isHeapShell && !(← isCopyType fieldTy) then
            throwCheck (.nonCopyProjection (tyToString fieldTy) s!"field '{field}' of '{structName}'") (some e.getSpan)
          return fieldTy
        | none => throwCheck (.structHasNoField structName field) (some e.getSpan)
      | none => throwCheck .fieldAccessNonStruct (some e.getSpan)
  | .enumLit _ enumName variant typeArgs fields =>
    match ← lookupEnum enumName with
    | some ed =>
      -- Infer type args from hint if not explicitly provided
      let effectiveTypeArgs := if typeArgs.isEmpty && !ed.typeParams.isEmpty then
        match hint with
        | some (.generic n args _) => if n == enumName then args else []
        | _ => []
      else typeArgs
      let mapping := ed.typeParams.zip effectiveTypeArgs
      match ed.variants.find? fun v => v.name == variant with
      | some ev =>
        for sf in ev.fields do
          let fieldTy := substTy mapping sf.ty
          match fields.find? fun (fn, _) => fn == sf.name with
          | some (_, expr) =>
            let exprTy ← checkExpr expr (some fieldTy)
            expectTy fieldTy exprTy s!"field '{sf.name}' of {enumName}::{variant}" (some e.getSpan)
            -- Bug 092: a payload holding a reference (`Option::Some { value: &s }`).
            if tyStoresRef exprTy then
              throwCheck (.refStoredInAggregate s!"payload '{sf.name}' of {enumName}::{variant}" (tyToString exprTy)) (some expr.getSpan)
          | none => throwCheck (.missingFieldInLiteral sf.name s!"{enumName}::{variant}") (some e.getSpan)
        for (fn, _) in fields do
          match ev.fields.find? fun sf => sf.name == fn with
          | some _ => pure ()
          | none => throwCheck (.unknownFieldInLiteral fn s!"{enumName}::{variant}") (some e.getSpan)
        if effectiveTypeArgs.isEmpty then return .named enumName
        else return .generic enumName effectiveTypeArgs [] -- enums take no capability parameters
      | none => throwCheck (.unknownVariant variant enumName) (some e.getSpan)
    | none => throwCheck (.unknownEnumType enumName) (some e.getSpan)
  | .match_ _ scrutinee arms =>
    -- `match *r` reads the pointee BY VALUE: a non-Copy pointee is E0290 (its payloads
    -- would bind as owned values the owner still holds); `match r` matches through the
    -- reference and binds non-Copy payloads as borrowed views (Codex review of round 3).
    let scrTy ← checkExpr scrutinee
    -- Auto-deref through references for match
    let innerTy := match scrTy with
      | .ref t => t
      | .refMut t => t
      | t => t
    let innerTyR ← resolveType innerTy
    let (enumName, enumTypeArgs) := match innerTyR with
      | .named n => (n, ([] : List Ty))
      | .generic n args _ => (n, args)
      | _ => ("", [])
    if enumName != "" then
      match ← lookupEnum enumName with
      | some ed =>
        -- (scrutinee ident already moved by value-mode checkExpr)
        -- CoreCheck validates exhaustiveness, duplicates, field counts, wrong-enum
        -- Linearity across arms: snapshot env, check each arm, ensure all agree
        let envBefore ← getEnv
        let mut firstArmVars : Option (List (String × VarInfo)) := none
        let mut divergingArmVars : Option (List (String × VarInfo)) := none
        let mut matchResultTy : Ty := .unit
        let mut firstArmDone := false
        for arm in arms do
          setEnv envBefore
          let body ← match arm with
          | .mk _ _armEnum armVariant bindings guard body => do
            -- Bind variant fields in scope (substitute generic type args)
            let ev := (ed.variants.find? fun v => v.name == armVariant).get!
            let typeMapping := ed.typeParams.zip enumTypeArgs
            let isRefScrutinee := match scrTy with | .ref _ | .refMut _ => true | _ => false
            for (binding, sf) in bindings.zip ev.fields do
              let fieldTy := substTy typeMapping sf.ty
              -- Matching THROUGH a reference must not move payloads out of the
              -- borrow: a non-Copy payload binds as a borrowed view (&T — Copy,
              -- freely droppable), a Copy payload binds by value. An OWNED
              -- scrutinee binds payloads owned, as before.
              let bindTy ← do
                if isRefScrutinee && !(← isCopyType fieldTy) then pure (Ty.ref fieldTy)
                else pure fieldTy
              -- An arm binder may shadow a live binding: it is arm-scoped, so
              -- the outer binding is visible again (with its own obligation)
              -- after the match; state updates key on uid (bug 094).
              if binding != "_" then addVar binding bindTy
              -- Linear: a `_` payload field may ignore ONLY a Copy field (or a
              -- borrowed view). A non-Copy OWNED field must be bound and consumed
              -- exactly once (destructure it, don't drop it).
              else if !isRefScrutinee && !(← isCopyType fieldTy) then
                throwCheck (.wildcardDiscardsNonCopy (tyToString fieldTy)) (some e.getSpan)
            checkGuard guard
            pure body
          | .litArm _ _val guard body => do
            checkGuard guard
            pure body
          | .varArm _ binding guard body => do
            if binding != "_" then addVar binding innerTyR
            -- Linear: a wildcard arm `_ => …` drops the whole scrutinee without
            -- accounting for it. That is allowed ONLY if the scrutinee is Copy; a
            -- non-Copy value must be destructured exhaustively, not swallowed by `_`.
            else if !(← isCopyType innerTyR) then
              throwCheck (.wildcardDiscardsNonCopy (tyToString innerTyR)) (some e.getSpan)
            checkGuard guard
            pure body
          | .rangeArm _ lo hi _ guard body => do
            -- Endpoints take the scrutinee's type, as in Elab (bug 088: an
            -- unhinted endpoint defaults to `Int` and is range-checked there).
            let _ ← checkExpr lo (some innerTyR)
            let _ ← checkExpr hi (some innerTyR)
            checkGuard guard
            pure body
          -- Function-exiting arms relax the consume-inside-loop rule (see
          -- TypeEnv.inFnExitingBranch); per-arm setEnv envBefore resets it.
          if blockExitsFunction body then
            let cur ← getEnv
            setEnv { cur with inFnExitingBranch := true }
          -- Check all stmts except the last, then extract type from last
          let bodyInit := body.dropLast
          let curEnv ← getEnv
          checkStmts bodyInit curEnv.currentRetTy
          let armTy ← match body.getLast? with
            | some (.expr _ armExpr true) => do
              let ty ← checkExpr armExpr hint
              pure ty
            | some (.return_ rsp v) =>
              -- A `return` inside an arm consumes the returned value (and checks it
              -- against the return type) exactly like a statement-level return —
              -- reuse checkStmt so `return value;` moves `value`. Without this, the
              -- returned payload looked unconsumed to the H9 block-local check.
              checkStmt (.return_ rsp v) curEnv.currentRetTy
              pure Ty.never
            | some other =>
              checkStmt other curEnv.currentRetTy
              pure Ty.unit
            | none => pure Ty.unit
          -- Unify arm types
          if !firstArmDone then
            matchResultTy := armTy
            firstArmDone := true
          else
            if armTy != matchResultTy && armTy != .never && matchResultTy != .never then
              throwCheck (.matchArmTypeMismatch (tyToString matchResultTy) (tyToString armTy)) (some e.getSpan)
            if matchResultTy == .never then matchResultTy := armTy
          let envAfterArm ← getEnv
          -- H9: a linear value declared in this arm — a matched payload binding or
          -- a `let` in the arm body — must be consumed before the arm exits (unless
          -- the arm is non-terminating). This is the matched-payload half of H9.
          checkBlockLocalsConsumed envBefore.vars envAfterArm.vars (blockNonTerminating body) (some e.getSpan)
          -- A DIVERGING arm (return/break/continue/abort) never reaches the
          -- code after the match: it cannot disagree with the other arms and
          -- contributes nothing to the merged state. Only fall-through arms
          -- participate — `Some{v} => { use(v) }, None => { drop(x); return 1 }`
          -- is sound. An arm that exits the FUNCTION must still have consumed
          -- everything live (the exit is a leak point, not a merge point).
          if blockExitsFunction body then
            checkReturnPathConsumed envBefore.vars envAfterArm.vars (some e.getSpan)
          if !(blockDiverges body) then
            match firstArmVars with
            | none => firstArmVars := some envAfterArm.vars
            | some firstVars =>
              -- Check agreement on pre-existing variables
              for (name, infoBefore) in envBefore.vars do
                if infoBefore.isCopy then continue
                let state1 := match lookupByUid firstVars infoBefore.uid with
                  | some info => info.state
                  | none => infoBefore.state
                let state2 := match lookupByUid envAfterArm.vars infoBefore.uid with
                  | some info => info.state
                  | none => infoBefore.state
                let consumed1 := state1 == .consumed
                let consumed2 := state2 == .consumed
                if consumed1 != consumed2 then
                  throwCheck (.matchConsumptionDisagreement name) (some e.getSpan)
          else if divergingArmVars.isNone then
            divergingArmVars := some envAfterArm.vars
        -- Apply the final state from the first FALL-THROUGH arm (they all
        -- agree). If EVERY arm diverges, the code after the match is
        -- unreachable — apply the first (diverging) arm's state so values it
        -- consumed don't re-surface as spurious E0208 at function exit.
        -- Rebuild from envBefore, NOT the last arm's env: arm binders are
        -- arm-scoped, and keeping them let a stale Copy `value` from one
        -- match poison a later match's same-named linear binder through the
        -- by-name state patch (bug 041's false E0208).
        match firstArmVars.orElse (fun () => divergingArmVars) with
        | some vars =>
          let vars' := envBefore.vars.map fun (n, vi) =>
            match lookupByUid vars vi.uid with
            | some info => (n, { vi with state := info.state })
            | none => (n, vi)
          setEnv { envBefore with vars := vars' }
        | none => setEnv envBefore
        return matchResultTy
      | none => throwCheck (.unknownEnumType enumName) (some e.getSpan)
    else
      -- Value-pattern match (integer/bool literals, variable bindings)
      match scrutinee with
      | .ident _ varName => useVar varName (some e.getSpan)
      | _ => pure ()
      let envBefore ← getEnv
      let mut matchResultTy : Ty := .unit
      let mut firstArmDone := false
      let mut firstArmVars : Option (List (String × VarInfo)) := none
      let mut divergingArmVars : Option (List (String × VarInfo)) := none
      for arm in arms do
        setEnv envBefore
        let body ← match arm with
        | .litArm _ _val guard body => do
          checkGuard guard
          pure body
        | .varArm _ binding guard body => do
          -- Value-pattern match auto-derefs the scrutinee (see innerTy above),
          -- and the runtime loads the value through the reference — so a
          -- variable arm over `&i32` binds the VALUE (i32), not `&i32`. Only a
          -- Copy inner type binds by value: binding a non-Copy value out from
          -- behind a reference would duplicate it, so those keep the ref type.
          let isRefScrutinee := match scrTy with | .ref _ | .refMut _ => true | _ => false
          let bindTy ← do
            if isRefScrutinee && (← isCopyType innerTyR) then pure innerTyR else pure scrTy
          let scrCopy ← isCopyType bindTy
          if binding != "_" then
            -- A named value pattern over a non-Copy scrutinee is a move into the
            -- arm-local binding. Without consuming the original identifier here,
            -- `match h { y => free(y) } ; free(h);` aliases one linear handle
            -- through two names.
            -- (scrutinee ident already moved by value-mode checkExpr)
            addVar binding bindTy
          else if !scrCopy then
            throwCheck (.wildcardDiscardsNonCopy (tyToString bindTy)) (some e.getSpan)
          checkGuard guard
          pure body
        | .rangeArm _ lo hi _ guard body => do
          -- Endpoints take the scrutinee's type, as in Elab (bug 088: an
          -- unhinted endpoint defaults to `Int` and is range-checked there).
          let _ ← checkExpr lo (some innerTyR)
          let _ ← checkExpr hi (some innerTyR)
          checkGuard guard
          pure body
        | .mk _ _ _ _ guard body => do
          checkGuard guard
          pure body
        -- Function-exiting arms relax the consume-inside-loop rule (see enum path).
        if blockExitsFunction body then
          let cur ← getEnv
          setEnv { cur with inFnExitingBranch := true }
        -- Check all stmts except the last, then extract type from last
        let bodyInit := body.dropLast
        checkStmts bodyInit envBefore.currentRetTy
        let armTy ← match body.getLast? with
          | some (.expr _ armExpr true) => do
            let ty ← checkExpr armExpr hint
            pure ty
          | some (.return_ rsp v) =>
            checkStmt (.return_ rsp v) envBefore.currentRetTy
            pure Ty.never
          | some other =>
            checkStmt other envBefore.currentRetTy
            pure Ty.unit
          | none => pure Ty.unit
        if !firstArmDone then
          matchResultTy := armTy
          firstArmDone := true
        else
          if armTy != matchResultTy && armTy != .never && matchResultTy != .never then
            throwCheck (.matchArmTypeMismatch (tyToString matchResultTy) (tyToString armTy)) (some e.getSpan)
          if matchResultTy == .never then matchResultTy := armTy
        let envAfterArm ← getEnv
        -- H9 for value-pattern arms too: a named arm binding over a non-Copy
        -- scrutinee is an arm-local linear value and must be consumed before the
        -- arm exits. Enum payload arms already enforce this above.
        checkBlockLocalsConsumed envBefore.vars envAfterArm.vars (blockNonTerminating body) (some e.getSpan)
        -- Diverging arms never reach the code after the match — only
        -- fall-through arms participate in the consumption merge (see the
        -- enum-path comment above); a function-exiting arm must still have
        -- consumed everything live.
        if blockExitsFunction body then
          checkReturnPathConsumed envBefore.vars envAfterArm.vars (some e.getSpan)
        if !(blockDiverges body) then
          match firstArmVars with
          | none => firstArmVars := some envAfterArm.vars
          | some firstVars =>
            -- Check agreement on pre-existing variables
            for (name, infoBefore) in envBefore.vars do
              if infoBefore.isCopy then continue
              let state1 := match lookupByUid firstVars infoBefore.uid with
                | some info => info.state
                | none => infoBefore.state
              let state2 := match lookupByUid envAfterArm.vars infoBefore.uid with
                | some info => info.state
                | none => infoBefore.state
              let consumed1 := state1 == .consumed
              let consumed2 := state2 == .consumed
              if consumed1 != consumed2 then
                throwCheck (.matchConsumptionDisagreement name) (some e.getSpan)
        else if divergingArmVars.isNone then
          divergingArmVars := some envAfterArm.vars
      -- Apply the final state from the first FALL-THROUGH arm (they all
      -- agree); if EVERY arm diverges, fall back to the first diverging
      -- arm's state (code after the match is unreachable).
      -- envBefore-based rebuild for the same reason as the enum path above
      -- (bug 041: arm binders are arm-scoped and must not survive the match).
      match firstArmVars.orElse (fun () => divergingArmVars) with
      | some vars =>
        let vars' := envBefore.vars.map fun (n, vi) =>
          match lookupByUid vars vi.uid with
          | some info => (n, { vi with state := info.state })
          | none => (n, vi)
        setEnv { envBefore with vars := vars' }
      | none => setEnv envBefore
      return matchResultTy
  | .borrow _ inner =>
    let innerTy ← checkExpr inner none .place
    -- Check the variable is not moved or already mutably borrowed. `&(x)` is `&x`, and a
    -- projection's root obeys the same rule (bug 091 round 3).
    if !(stripParens inner matches .ident ..) then checkSharedBorrowPlace inner (some e.getSpan)
    match stripParens inner with
    | .ident _ varName =>
      match ← lookupVarInfo varName with
      | some info =>
        if !info.isCopy && info.state == .consumed then
          throwCheck (.cannotBorrowMoved varName) (some e.getSpan)
        let env ← getEnv
        let activeRefs := activeBorrowRefs env varName
        if activeRefs.any (fun refInfo => match refInfo.ty with | .refMut _ => true | _ => false) then
          throwCheck (.cannotBorrowMutablyBorrowed varName) (some e.getSpan)
      | none => throwCheck (.undeclaredVariable varName) (some e.getSpan)
    | _ => pure ()
    return .ref innerTy
  | .borrowMut _ inner =>
    let innerTy ← checkExpr inner none .place
    -- One rule for `&mut x`, `&mut (x)`, `&mut w.f` and `&mut (*r)`: never through a shared
    -- reference (E0237), and an owned root is `mut` and unborrowed (bug 091 round 3).
    checkMutBorrowPlace inner "a place" (some e.getSpan)
    return .refMut innerTy
  | .deref _ inner =>
    -- Reading THROUGH a ref/ptr does not consume the binding (place read);
    -- only the Heap load-and-free below consumes.
    let innerTy ← checkExpr inner none .place
    -- H11 (bug 091 round 3): `*r` read BY VALUE through a reference duplicates a non-Copy
    -- pointee its owner still holds (`P_destroy(*r)`, `let q = *r`). Place uses
    -- (`(*r).f`, `&*r`, a receiver) and Copy pointees are unaffected; raw pointers stay the
    -- trusted collection idiom.
    let refusedDeref (t : Ty) : CheckM Unit := do
      if mode != .place && !(← isCopyType t) then
        throwCheck (.nonCopyProjection (tyToString t) "a dereference of a reference") (some e.getSpan)
    match innerTy with
    | .ref t => refusedDeref t; return t
    | .refMut t => refusedDeref t; return t
    | .ptrMut t =>
      return t
    | .ptrConst t =>
      return t
    | .heap t =>
      -- *heap_ptr: loads value from heap, frees memory, consumes the Heap<T>
      match inner with
      | .ident _ varName => consumeVar varName (some e.getSpan)
      | _ => pure ()
      return t
    | _ => throwCheck .cannotDerefNonRef (some e.getSpan)
  | .arrayLit _ elems =>
    match elems with
    | [] => return .array .placeholder 0  -- CoreCheck validates empty array literals
    | first :: rest =>
      -- Use hint to determine element type (e.g. [i32; N] → elements are i32)
      let elemHint := match hint with
        | some (.array t _) => some t
        | _ => none
      let firstTy ← checkExpr first elemHint
      -- Bug 092: an array of references (`[&a, &b]`).
      if tyStoresRef firstTy then
        throwCheck (.refStoredInAggregate "array literal" (tyToString firstTy)) (some first.getSpan)
      -- value mode moves each element in (H10) — auto-consumed at checkExpr
      for e in rest do
        let eTy ← checkExpr e (some firstTy)
        expectTy firstTy eTy "array element" (some e.getSpan)
      return .array firstTy elems.length
  | .arrayIndex _ arr index =>
    let arrTy ← checkExpr arr none .place
    let _idxTy ← checkExpr index
    -- CoreCheck validates index type and array type. Resolve the element type
    -- through a reference/pointer to the array (`&[T;N]`, `&mut [T;N]`, raw ptr,
    -- heap) — indexing auto-derefs, so `arr[i]` / `&arr[i]` work when `arr` is a
    -- reference to an array, not only a bare array (C10; sibling of #6b).
    let elemTy? := match arrTy with
      | .array elemTy _ => some elemTy
      | .ref (.array elemTy _) | .refMut (.array elemTy _)
      | .ptrConst (.array elemTy _) | .ptrMut (.array elemTy _)
      | .heap (.array elemTy _) => some elemTy
      | _ => none
    match elemTy? with
    | some elemTy =>
      -- H11: a by-value read of a non-Copy element duplicates it (the array
      -- still owns the same value). `&arr[i]`, Copy elements, `arr[i].f`
      -- projection bases, and `arr[i] = v` stay legal.
      if mode != .place && !(← isCopyType elemTy) then
        throwCheck (.nonCopyProjection (tyToString elemTy) "array element") (some e.getSpan)
      return elemTy
    | none => return .placeholder
  | .cast _ inner targetTy =>
    -- Legal cast operands are numeric (Copy): nothing to consume. Place mode
    -- so an ILLEGAL `s as Int` reports "cannot cast", not a consume error.
    -- A literal operand gets the shared cast-operand hint (bug 088): `Uint` for a
    -- literal only `Uint` can hold, otherwise none (defaulted, range-checked `Int`).
    let _innerTy ← checkExpr inner (TypeJudgment.castOperandHint inner) .place
    -- CoreCheck validates cast legality and capability requirements
    return targetTy
  | .methodCall _ obj methodName typeArgs args =>
    -- The receiver is a place: `w.f.method()` auto-borrows for `&self` /
    -- `&mut self`. A by-value `self` on a projection receiver is re-checked
    -- below once the signature is known.
    let objTy ← checkExpr obj none .place
    let innerTy0 := match objTy with
      | .ref t => t
      | .refMut t => t
      | t => t
    -- Normalize: a receiver whose type NAMES one of the current fn's own type
    -- params (e.g. the `p as &T` cast target arrives as `.named "T"`) is a
    -- typeVar receiver — same normalization checkTraitBounds applies to
    -- turbofish args. Without it the concrete path looks up `T_destroy`
    -- literally and throws E0264 instead of consulting the trait bounds.
    let envN ← getEnv
    let innerTy := match innerTy0 with
      | .named n => if envN.currentTypeParams.contains n then .typeVar n else innerTy0
      | t => t
    let typeName := tyName innerTy
    if typeName == "" then
      -- Check if this is a type variable with trait bounds
      match innerTy with
      | .typeVar n =>
        let env ← getEnv
        let bounds := (env.currentTypeBounds.find? fun (name, _) => name == n).map Prod.snd |>.getD []
        -- Find the method in one of the bound traits
        let mut foundSig : Option FnSigDef := none
        let mut foundTrait : String := ""
        let mut foundBuiltin : Option BuiltinTraitId := none
        for traitName in bounds do
          match env.traits.find? fun td => td.name == traitName with
          | some td =>
            match td.methods.find? fun ms => ms.name == methodName with
            | some ms => foundSig := some ms; foundTrait := traitName; foundBuiltin := td.builtinId; break
            | none => pure ()
          | none => pure ()
        match foundSig with
        | none => throwCheck (.noMethodOnTypeVar methodName n) (some e.getSpan)
        | some sig =>
          -- Bug 091: a call through a trait bound exercises the authority the TRAIT
          -- method declares — the only set every implementation is bounded by
          -- (CoreCheck E0586) — so the generic caller must hold it. The call reaches
          -- Core as an unresolved `T$method` that CoreCheck cannot look up, and Mono
          -- specializes it without re-checking, so this is the one place the decision
          -- is made; `Unsafe` included, since nothing downstream would see it. A
          -- trusted body discharges the `Unsafe` obligation, exactly as at a direct call.
          let envC ← getEnv
          for cap in Capabilities.missingCaps envC.currentCapSet sig.capSet do
            if !(cap == unsafeCapName && envC.currentFnTrusted) then
              throwCheck (.missingCapability s!"{foundTrait}::{methodName}" cap envC.currentFnName) (some e.getSpan)
          -- Replace Self with the type variable in the trait signature
          let selfTy := Ty.typeVar n
          let retTy := substSelf sig.retTy selfTy
          let params := sig.params.map fun p => { p with ty := substSelf p.ty selfTy }
          -- Type check arguments (params in FnSigDef excludes self)
          if args.length != params.length then
            throwCheck (.wrongArgCount s!"method '{methodName}'" params.length args.length) (some e.getSpan)
          if sig.selfKind.isNone then
            throwCheckMsg s!"trait method '{foundTrait}::{methodName}' has no `self` receiver and cannot be called as a method"
          -- The receiver obeys the TRAIT's declared form exactly as a concrete receiver obeys
          -- its method's (Codex round 2 of bug 091): borrow conflicts, then consumption.
          checkCallBorrowConflicts (receiverBorrowParts sig.selfKind obj ++ (← borrowArgParts args)) (some e.getSpan)
          -- `Destroy::destroy` consumes its receiver (`consumeDestroyReceiver` below); there
          -- is no implicit borrow to check, and the owner may already be moved by `w.0`.
          if !(foundBuiltin == some .destroy && methodName == destroyMethodName) then
            checkImplicitReceiverBorrow sig.selfKind obj objTy methodName (some e.getSpan)
          for (arg, p) in args.zip params do
            let argTy ← checkExpr arg (some p.ty) .callArg
            expectTy p.ty argTy s!"argument '{p.name}' of '{methodName}'" (some e.getSpan)
            match stripParens arg with
            | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
            | _ => pure ()
          if foundBuiltin == some .destroy && methodName == destroyMethodName then
            consumeDestroyReceiver obj objTy innerTy (some e.getSpan)
          else
            consumeReceiver sig.selfKind obj objTy innerTy methodName (some e.getSpan)
          return retTy
      | _ => throwCheck .methodCallOnNonNamedType (some e.getSpan)
    else
    let mangledName := typeName ++ "_" ++ methodName
    match ← lookupFn mangledName with
    | some sig =>
      -- E0293: the auto-borrowed receiver counts as a borrow of the receiver
      -- PLACE — `m.with_value_mut(&k, &mut m, f)` and `c.scoped(&mut c.i, f)`
      -- alias the container through the context and must reject
      -- (container-not-in-context, #18 §5.1).
      checkCallBorrowConflicts (receiverBorrowParts (selfKindOfParams sig.params) obj
        ++ (← borrowArgParts args)) (some e.getSpan)
      let isDestroyImplCall := methodName == destroyMethodName &&
        (← getEnv).traitImpls.any (fun (t, tr) => t == typeName && tr == destroyTraitName)
      if !isDestroyImplCall then
        checkImplicitReceiverBorrow (selfKindOfParams sig.params) obj objTy methodName (some e.getSpan)
      -- Build type mapping from object's generic type args + explicit call typeArgs
      let objTypeArgs := match innerTy with
        | .generic _ args _ => args
        | _ => []
      let implTypeParams := sig.typeParams.take objTypeArgs.length
      let methodTypeParams := sig.typeParams.drop objTypeArgs.length
      let implMapping := implTypeParams.zip objTypeArgs
      -- Infer the method's own type params + cap params from argument types
      -- (mirrors free-fn inference), so capability-polymorphic methods work
      -- without turbofish.
      let (methodParams, retTy) ←
        inferMethodParamAndRetTys sig implMapping methodTypeParams typeArgs args methodName e.getSpan innerTy
      if args.length != methodParams.length then
        throwCheck (.wrongArgCount s!"method '{methodName}'" methodParams.length args.length) (some e.getSpan)
      for (arg, (pName, pTy)) in args.zip methodParams do
        let argTy ← checkExpr arg (some pTy) .callArg
        expectTy pTy argTy s!"argument '{pName}' of '{methodName}'" (some e.getSpan)
        match stripParens arg with
        | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
      -- A concrete call of a type's Destroy impl is the same public destruction as the
      -- bound call: it consumes (bug 091 round 3).
      if methodName == destroyMethodName &&
          (← getEnv).traitImpls.any (fun (t, tr) => t == typeName && tr == destroyTraitName) then
        consumeDestroyReceiver obj objTy innerTy (some e.getSpan)
      else
        consumeReceiver (selfKindOfParams sig.params) obj objTy innerTy methodName (some e.getSpan)
      return retTy
    | none => throwCheck (.noMethodOnType methodName typeName) (some e.getSpan)
  | .staticMethodCall _ typeName methodName typeArgs args =>
    -- E0293: no overlapping borrows of one place within a single call (any &mut).
    checkCallBorrowConflicts (← borrowArgParts args) (some e.getSpan)
    let mangledName := typeName ++ "_" ++ methodName
    checkDirectDestroyImplUse mangledName (some e.getSpan)
    match ← lookupFn mangledName with
    | some sig =>
      let mapping := sig.typeParams.zip typeArgs
      for (pName, pTy) in mapping do
        if tyContainsRef pTy && tyParamOccursIn pName sig.retTy then
          throwCheckMsg s!"static method '{methodName}': type parameter '{pName}' is instantiated to a reference ('{tyToString pTy}') and occurs in the return type; references may not be returned (VALUE_MODEL.md)."
      let paramTypes := sig.params.map fun (n, t) => (n, substTy mapping t)
      let retTy := substTy mapping sig.retTy
      if args.length != paramTypes.length then
        throwCheck (.wrongArgCount s!"static method '{methodName}'" paramTypes.length args.length) (some e.getSpan)
      for (arg, (pName, pTy)) in args.zip paramTypes do
        let argTy ← checkExpr arg (some pTy) .callArg
        expectTy pTy argTy s!"argument '{pName}' of '{typeName}::{methodName}'" (some e.getSpan)
        match stripParens arg with
        | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
      return retTy
    | none => throwCheck (.noMethodOnType methodName typeName) (some e.getSpan)
  | .fnRef _ fnName =>
    checkDirectDestroyImplUse fnName (some e.getSpan)
    -- Look up the function signature to build the fn pointer type
    let env ← getEnv
    match env.allFnSummarys.lookup fnName with
    | some sig =>
      let paramTys := sig.params.map Prod.snd
      return .fn_ paramTys sig.capSet sig.retTy
    | none => throwCheck (.unknownFunctionRef fnName) (some e.getSpan)

/-- A match guard is READ-ONLY (bug 090): it is checked as a bool expression,
    and moving a non-Copy value in it is E0300. A false guard falls through to
    the next arm, which is checked from the same pre-arm env and may move the
    same value — the guard's move would then be a second one at run time. -/
partial def checkGuard (guard : Option Expr) : CheckM Unit := do
  match guard with
  | none => pure ()
  | some g =>
    let before ← getEnv
    -- Every non-Copy binding live here is frozen for the guard: consumeVar
    -- rejects moving it (also inside nested blocks and `defer`).
    let live := before.vars.filterMap fun (_, vi) =>
      if !vi.isCopy && vi.state != .consumed then some vi.uid else none
    setEnv { before with guardUids := live ++ before.guardUids }
    discard (checkExpr g (some .bool))
    let after0 ← getEnv
    setEnv { after0 with guardUids := before.guardUids }
    let after ← getEnv
    match guardMovedName? before.vars after.vars with
    | some n => throwCheck (.guardMovesLinear n) (some g.getSpan)
    | none => pure ()

partial def checkStmt (stmt : Stmt) (retTy : Ty) : CheckM Unit := do
  match stmt with
  | .letDecl _ name mutable ty value _isGhost =>
    -- Escape analysis: prevent storing a borrow ref into a new binding
    let env ← getEnv
    match value with
    | .ident _ vn =>
      if env.borrowRefs.contains vn then
        throwCheck (.referenceEscapesBorrowBlock vn) (some stmt.getSpan)
    | _ => pure ()
    let valTy ← checkExpr value ty
    match ty with
    | some declTy => expectTy declTy valTy s!"let binding '{name}'" (some stmt.getSpan)
    | none => pure ()
    let finalTy ← match ty with
      | some t => resolveType t
      | none => pure valTy
    -- `let _ = expr;` is removed: `_` is a pattern wildcard, not a discard device
    -- that makes an owned value vanish. Ignore a non-resource value with
    -- `match e { _ => {} }` (gated so it can't silently drop a resource owner);
    -- consume or `destroy()` anything else.
    if name == "_" then
      throwCheck .letUnderscoreRemoved (some stmt.getSpan)
    else
      -- H16: shadowing a still-LIVE non-Copy binding would silently drop the
      -- shadowed value — scope exit resolves locals by name, so the older
      -- entry's obligation vanishes behind the new one. Consume it first
      -- (`let s = transform(s);` is fine: the RHS consumed the old `s`).
      checkNoLiveShadow name (some stmt.getSpan)
      addVar name finalTy mutable (declSpan := some stmt.getSpan)
      recordRefProvenance name finalTy value (some stmt.getSpan)
  | .assign _ name value =>
    -- Escape analysis: prevent storing a borrow ref into an outer variable
    let env ← getEnv
    match value with
    | .ident _ vn =>
      if env.borrowRefs.contains vn then
        throwCheck (.referenceEscapesBorrowBlock vn) (some stmt.getSpan)
    | _ => pure ()
    match ← lookupVarInfo name with
    | some info =>
      if !info.mutable then
        throwCheck (.assignToImmutable name) (some stmt.getSpan)
      if info.state == .frozen then
        throwCheck (.assignToFrozen name) (some stmt.getSpan)
      -- Check the RHS FIRST: `acc = f(acc, x)` consumes `acc` while
      -- evaluating the RHS, which is exactly what makes the rebind legal.
      -- While doing so, mark `name` as the rebind target so consuming it in
      -- the RHS is exempt from the consume-inside-loop rule (the assignment
      -- restores the binding within the same statement).
      let envPre ← getEnv
      setEnv { envPre with rebindingVar := some name }
      -- Self-assign `a = a` checks the RHS as a PLACE: auto-consuming `a`
      -- would re-arm the binding and legalize the no-op; the rebind rule
      -- below must still see the OLD value live (E0219). Any other RHS is a
      -- plain value move (H13) — auto-consumed at checkExpr.
      let selfAssign := match value with
        | .ident _ vn => vn == name
        | _ => false
      let valTy ← checkExpr value (some info.ty) (if selfAssign then .place else .value)
      let envPost ← getEnv
      setEnv { envPost with rebindingVar := envPre.rebindingVar }
      expectTy info.ty valTy s!"assignment to '{name}'" (some stmt.getSpan)
      -- `r = &p` re-points `r`: its provenance is the NEW source (Codex review of round 3).
      if !selfAssign then recordRefProvenance name info.ty value (some stmt.getSpan) (reassign := true)
      -- Linear REBIND rule: a linear variable may be reassigned only once its
      -- OLD value has been consumed (one binding, one LIVE resource) — the
      -- fold/accumulate pattern `acc = f(acc, x)` requires it. Overwriting a
      -- still-live linear value stays E0219 (it would leak the old value).
      if !info.isCopy then
        let infoNow ← lookupVarInfo name
        match infoNow with
        | some inf =>
          if inf.state != .consumed then
            throwCheck (.assignOverwritesLinear name) (some stmt.getSpan)
          else
            -- The binding now holds a NEW live value.
            let env ← getEnv
            -- Only the binding being assigned (innermost), by identity: marking
            -- every `name` live resurrected a shadowed, consumed outer binding
            -- (bug 094; masked while consumeVar also marked every `name`).
            let vars' := env.vars.map fun (n, vi) =>
              if vi.uid == inf.uid then (n, { vi with state := .unconsumed, movedAt := none })
              else (n, vi)
            setEnv { env with vars := vars' }
        | none => pure ()
    | none => throwCheck (.assignToUndeclaredVariable name) (some stmt.getSpan)
  | .return_ _ (some value) =>
    -- Escape analysis: prevent returning a borrow ref
    let env ← getEnv
    match value with
    | .ident _ vn =>
      if env.borrowRefs.contains vn then
        throwCheck (.referenceEscapesBorrowBlock vn) (some stmt.getSpan)
    | _ => pure ()
    let valTy ← checkExpr value (some retTy)
    expectTy retTy valTy "return value" (some stmt.getSpan)
  | .return_ _ none =>
    expectTy .unit retTy "return (void)" (some stmt.getSpan)
  | .expr _ e isValue =>
    let eTy ← checkExpr e
    -- Phase 6 #13: a `;`-terminated statement expression (`isValue := false`)
    -- discards its value. If that value is a fallible result (`Result`/`Option`),
    -- the discard silently throws away a possible failure/absence — flag it. A
    -- trailing value expression (`isValue := true`) is the block's value, not a
    -- discard, so it is never flagged.
    -- H6: a discarded statement value that is non-Copy (linear) is silently
    -- dropped without being consumed — an error. `Result`/`Option` get the
    -- must-use message; other linear values get the general one. Copy values,
    -- `unit`, and `never` (diverging, e.g. `abort()`) are fine to discard.
    -- `free(box)` is itself the consumption (it deallocates the Heap and hands
    -- back the moved-out pointee); discarding that returned pointee is the
    -- established free-and-drop idiom, so it is exempt from the discard check.
    let isFreeCall := match e with
      | .call _ fn _ _ => resolveIntrinsic fn == some .free
      | _ => false
    if !isValue && !isFreeCall then
      match eTy with
      | .unit | .never | .placeholder => pure ()
      | _ =>
        -- Must-use keys on FALLIBILITY, not linearity: a discarded Result /
        -- Option silently drops a possible failure even when the instantiation
        -- is Copy (Option<i32> under conditional Copy). Check it first.
        match mustUseEnumName? eTy with
        | some _ => throwCheck (.discardedMustUse (tyToString eTy)) (some stmt.getSpan)
        | none =>
          if !(← isCopyType eTy) then
            throwCheck (.discardedLinear (tyToString eTy)) (some stmt.getSpan)
          -- Slice 5: the value is Copy and non-fallible. If the expression is
          -- also locally-provable pure and trap-free, computing it and dropping
          -- the result is a dead computation with no effect — flag it (acknowledge
          -- intent with `discard(expr)`, which is Unit-typed and reaches the
          -- branch above).
          else if exprPureDiscardable e then
            throwCheck (.discardedPureValue (tyToString eTy)) (some stmt.getSpan)
    pure ()
  | .ifElse _ cond thenBody elseBody =>
    let _condTy ← checkExpr cond
    -- Snapshot variable states before branches
    let envBefore ← getEnv
    -- Check then branch (flagging it if it exits the function, which relaxes
    -- the consume-inside-loop rule — see TypeEnv.inFnExitingBranch)
    if blockExitsFunction thenBody then
      setEnv { envBefore with inFnExitingBranch := true }
    checkStmts thenBody retTy
    let envAfterThen ← getEnv
    -- H9: a linear value declared in the then-branch must be consumed before the
    -- branch exits — including on a `return`/`break` path (that's a real leak).
    -- Only a truly non-terminating branch (infinite loop / abort) is exempt.
    checkBlockLocalsConsumed envBefore.vars envAfterThen.vars (blockNonTerminating thenBody) (some stmt.getSpan)
    -- Restore env and check else branch
    setEnv envBefore
    match elseBody with
    | some stmts =>
      if blockExitsFunction stmts then
        setEnv { envBefore with inFnExitingBranch := true }
      checkStmts stmts retTy
      let envAfterElse ← getEnv
      -- H9: same for else-branch locals.
      checkBlockLocalsConsumed envBefore.vars envAfterElse.vars (blockNonTerminating stmts) (some stmt.getSpan)
      -- A branch that exits the function must consume everything live — the
      -- divergence exemption below is about merge AGREEMENT, not the exit.
      if blockExitsFunction thenBody then
        checkReturnPathConsumed envBefore.vars envAfterThen.vars (some stmt.getSpan)
      if blockExitsFunction stmts then
        checkReturnPathConsumed envBefore.vars envAfterElse.vars (some stmt.getSpan)
      -- Merge: the branches that FALL THROUGH must agree on consumption state
      -- of linear vars (a diverging branch never reaches the merge point).
      mergeVarStates envBefore.vars envAfterThen.vars envAfterElse.vars
        (blockDiverges thenBody) (blockDiverges stmts) (some stmt.getSpan)
      -- Drop branch-locals — they are out of scope after the `if`. Only pre-existing
      -- names survive (their merged consumption states were applied above). Without
      -- this, else-branch locals leaked into the outer env (the accidental, lopsided
      -- way they used to be caught at function scope).
      let env ← getEnv
      setEnv { env with vars := env.vars.filter fun (_, vi) => hasUid envBefore.vars vi.uid,
                        inFnExitingBranch := envBefore.inFnExitingBranch }
    | none =>
      -- No else branch: then branch must not consume any linear var
      -- Exception: if then-branch diverges, consumption is fine (control never falls through)
      let thenDiverges := blockDiverges thenBody
      if !thenDiverges then
        checkNoBranchConsumption envBefore.vars envAfterThen.vars "if-without-else" (some stmt.getSpan)
      else
        -- Exiting the function inside the branch requires everything live to
        -- be consumed on that path (`if bad { return 1; }` with a live linear
        -- `s` leaks it on the early-return path).
        if blockExitsFunction thenBody then
          checkReturnPathConsumed envBefore.vars envAfterThen.vars (some stmt.getSpan)
        -- The then-branch never falls through, so the code AFTER the `if` sees
        -- the pre-branch state — a consume inside the diverging branch must
        -- not poison the fall-through path (`if bad { drop(s); return 1; }
        -- use(s)` is sound). Without this restore, the consumed state leaked
        -- and the later use was a spurious E0205.
        setEnv envBefore
  | .while_ _ cond body lbl =>
    let _condTy ← checkExpr cond
    -- Increment loop depth for the body, push label if present
    let env ← getEnv
    let labels := match lbl with
      | some l => l :: env.loopLabels
      | none => env.loopLabels
    setEnv { env with loopDepth := env.loopDepth + 1, loopLabels := labels, inFnExitingBranch := false }
    checkStmts body retTy
    -- Restore loop depth and labels
    let env' ← getEnv
    setEnv { env' with loopDepth := env.loopDepth, loopLabels := env.loopLabels }
  | .forLoop _ init cond step body lbl =>
    -- Init
    match init with
    | some initStmt => checkStmt initStmt retTy
    | none => pure ()
    -- Condition
    let _condTy ← checkExpr cond
    -- Body + step in loop scope, push label if present
    let env ← getEnv
    let labels := match lbl with
      | some l => l :: env.loopLabels
      | none => env.loopLabels
    setEnv { env with loopDepth := env.loopDepth + 1, loopLabels := labels, inFnExitingBranch := false }
    checkStmts body retTy
    match step with
    | some stepStmt => checkStmt stepStmt retTy
    | none => pure ()
    let env' ← getEnv
    setEnv { env' with loopDepth := env.loopDepth, loopLabels := env.loopLabels }
  | .fieldAssign _ obj field value =>
    -- Escape analysis: prevent storing a borrow ref into a struct field
    let env ← getEnv
    match value with
    | .ident _ vn =>
      if env.borrowRefs.contains vn then
        throwCheck (.referenceEscapesBorrowBlock vn) (some stmt.getSpan)
    | _ => pure ()
    let objTy ← checkExpr obj none .place
    -- Auto-deref one layer: references, and (6D#3) the heap shells — the old
    -- `->` assignment semantics folded into `.`.
    let innerTy := match objTy with
      | .heap t => t
      | .heapArray t => t
      | .ref (.heap t) => t
      | .refMut (.heap t) => t
      | .ref t => t
      | .refMut t => t
      | t => t
    -- Extract struct name and type args, mirroring the field-READ path:
    -- field assignment on a GENERIC struct (`self.len = …` in `impl<T> Vec<T>`)
    -- substitutes the type args, and `String` is std's struct behind the
    -- builtin type. The old `.named`-only match threw a wrong E0254
    -- ("non-struct") for both — surfaced by the H12 std migration.
    let (structName, typeArgs, capArgs) := match innerTy with
      | .named n => (n, ([] : List Ty), ([] : List CapSet))
      | .generic n args caps => (n, args, caps)
      | .string => ("String", [], [])
      | _ => ("", [], [])
    if structName == "" then throwCheck .fieldAccessNonStruct (some stmt.getSpan)
    else
      match ← lookupStruct structName with
      | some sd =>
        match sd.fields.find? fun f => f.name == field with
        | some f =>
          -- 0b slice 2: WRITING a private field of a non-local struct is
          -- rejected (the `b.len = 999` cross-module forge).
          if !f.isPublic && !(← getEnv).localStructNames.contains structName then
            throwCheck (.fieldAccessPrivate structName field) (some stmt.getSpan)
          let mapping := sd.typeParams.zip typeArgs
          -- R-0484: assignment compares against the field's type at this instantiation.
          let fieldTy ← resolveType (substCapNamesTy (sd.capParams.zip capArgs) (substTy mapping f.ty))
          -- Overwriting a non-Copy field would leak the old linear value AND cannot
          -- soundly move the RHS in (a linear value must be used exactly once). Reject
          -- it (mirrors the "linear variables cannot be reassigned" rule for places).
          if !(← isCopyType fieldTy) then
            throwCheck (.cannotOverwriteLinearField field) (some stmt.getSpan)
          let valTy ← checkExpr value (some fieldTy)
          expectTy fieldTy valTy s!"field assignment '{structName}.{field}'" (some stmt.getSpan)
        | none => throwCheck (.structHasNoField structName field) (some stmt.getSpan)
      | none => throwCheck (.structHasNoField structName field) (some stmt.getSpan)
  | .derefAssign _ target value =>
    let targetTy ← checkExpr target none .place
    match targetTy with
    | .refMut inner =>
      -- H15: `*r = v` through `&mut T` overwrites an INITIALIZED value — a
      -- non-Copy pointee would leak (the E0219 rule). Raw-pointer stores
      -- (`*mut`, below) stay exempt: they are the trusted collection idiom
      -- for writing uninitialized slots.
      if !(← isCopyType inner) then
        throwCheck (.cannotOverwriteLinearPlace "pointee (through &mut)" (tyToString inner)) (some stmt.getSpan)
      let valTy ← checkExpr value (some inner)
      expectTy inner valTy "deref assignment" (some stmt.getSpan)
    | .ptrMut inner =>
      let valTy ← checkExpr value (some inner)
      expectTy inner valTy "deref assignment" (some stmt.getSpan)
    | _ => pure ()  -- CoreCheck validates deref-assign target type
    -- Conservation: `*p = v` MOVES a linear `v` into the slot — the ident
    -- move happens in value-mode checkExpr (auto-consume).
  | .arrayIndexAssign _ arr index value =>
    let arrTy ← checkExpr arr none .place
    -- Bug 030: writing an element of a DIRECTLY-NAMED array requires the
    -- binding to be `mut` (same rule as scalar assignment). Writes through
    -- refs/ptrs are governed by their own &mut/(*mut) rules downstream.
    (match arr with
     | .ident _ arrName => do
       match ← lookupVarInfo arrName with
       | some info =>
         match info.ty with
         | .array _ _ =>
           if !info.mutable then
             throwCheck (.assignToImmutable arrName) (some stmt.getSpan)
         | _ => pure ()
       | none => pure ()
     | _ => pure ())
    let _idxTy ← checkExpr index
    -- CoreCheck validates index type and array type
    match arrTy with
    | .array elemTy _ =>
      -- H15: overwriting a non-Copy element leaks the OLD value (arrays are
      -- always fully initialized) — the E0219 field rule, applied to elements.
      if !(← isCopyType elemTy) then
        throwCheck (.cannotOverwriteLinearPlace "array element" (tyToString elemTy)) (some stmt.getSpan)
      let valTy ← checkExpr value (some elemTy)
      expectTy elemTy valTy "array element assignment" (some stmt.getSpan)
    | _ => let _ ← checkExpr value; pure ()
    -- Conservation: `arr[i] = v` moves a linear `v` in — value-mode
    -- checkExpr auto-consumes the ident.
  | .defer _ body =>
    -- Verify body is a call expression
    match body with
    | .call _ _ _ _ => pure ()
    | _ => throwCheck .deferBodyNotCall (some stmt.getSpan)
    let bodyTy ← checkExpr body
    -- H6: a deferred call's return value is discarded at scope exit, exactly like
    -- a bare statement expression — a non-Copy result leaks. (`free()` is exempt,
    -- as in the statement-discard check.)
    let isFreeCall := match body with
      | .call _ fn _ _ => resolveIntrinsic fn == some .free
      | _ => false
    if !isFreeCall then
      match bodyTy with
      | .unit | .never | .placeholder => pure ()
      | _ =>
        -- Must-use keys on FALLIBILITY, not linearity (see the statement-
        -- discard check): a deferred Result/Option is flagged even when Copy.
        match mustUseEnumName? bodyTy with
        | some _ => throwCheck (.discardedMustUse (tyToString bodyTy)) (some stmt.getSpan)
        | none =>
          if !(← isCopyType bodyTy) then
            throwCheck (.discardedLinear (tyToString bodyTy)) (some stmt.getSpan)
    -- Any variable consumed by the deferred call should be reserved, not consumed.
    -- The actual consumption happens at scope exit.
    match body with
    | .call _ _fname _ args =>
      for arg in args do
        match stripParens arg with
        | .ident _ varName =>
          let env ← getEnv
          match env.vars.lookup varName with
          | some info =>
            if info.state == .consumed then
              let vars' := env.vars.map fun (n, vi) =>
                if vi.uid == info.uid then (n, { vi with state := .reserved })
                else (n, vi)
              setEnv { env with vars := vars' }
          | none => pure ()
        | _ => pure ()
    | _ => pure ()
  | .borrowIn _ var ref region isMut body =>
    -- Check that var exists
    match ← lookupVarInfo var with
    | none => throwCheck (.undeclaredVariable var) (some stmt.getSpan)
    | some varInfo =>
      -- Check no shadowing of ref and region names
      let env ← getEnv
      if (env.vars.lookup ref).isSome then
        throwCheck (.borrowRefShadows ref) (some stmt.getSpan)
      if (env.vars.lookup region).isSome then
        throwCheck (.borrowRegionShadows region) (some stmt.getSpan)
      -- Check if variable is consumed (moved)
      if !varInfo.isCopy && varInfo.state == .consumed then
        throwCheck (.cannotBorrowMoved var) (some stmt.getSpan)
      -- Check if variable is frozen (already inside another borrow block)
      if varInfo.state == .frozen then
        throwCheck (.variableFrozenByBorrow var) (some stmt.getSpan)
      -- Check for mutable borrow conflict: if var is already immutably borrowed, error
      if isMut && varInfo.borrowCount > 0 then
        throwCheck (.cannotMutBorrowImmBorrowed var) (some stmt.getSpan)
      -- Save state and freeze the original variable
      let savedState := varInfo.state
      let vars' := env.vars.map fun (n, vi) =>
        if vi.uid == varInfo.uid then (n, { vi with state := .frozen })
        else (n, vi)
      setEnv { env with vars := vars' }
      -- Add reference binding and track for escape analysis
      let refTy := if isMut then Ty.refMut varInfo.ty else Ty.ref varInfo.ty
      addVar ref refTy true
      let envWithRef ← getEnv
      setEnv { envWithRef with borrowRefs := ref :: envWithRef.borrowRefs }
      -- Check body
      checkStmts body env.currentRetTy
      -- Clean up: remove ref from borrowRefs and unfreeze original variable
      let env' ← getEnv
      -- The body is a block: its non-Copy locals must be consumed in it (H9).
      -- They are matched by identity, so a body local that shadows the Copy
      -- ref name is still checked (bug 094).
      checkBlockLocalsConsumed envWithRef.vars env'.vars (blockNonTerminating body) (some stmt.getSpan)
      -- Drop the ref and the body's locals by identity, never by name: a
      -- name filter also deleted a body local named like the ref (bug 094).
      let vars'' := (env'.vars.map fun (n, vi) =>
        if vi.uid == varInfo.uid then (n, { vi with state := savedState })
        else (n, vi)).filter fun (_, vi) => hasUid env.vars vi.uid
      let cleanedRefs := env'.borrowRefs.filter (· != ref)
      setEnv { env' with vars := vars'', borrowRefs := cleanedRefs }
  | .break_ _ value lbl =>
    let env ← getEnv
    if env.loopDepth == 0 then
      throwCheck .breakOutsideLoop (some stmt.getSpan)
    -- Validate label if present
    match lbl with
    | some l =>
      if !env.loopLabels.contains l then
        throwCheck (.unknownLoopLabel l) (some stmt.getSpan)
    | none => pure ()
    -- Check break value if present (for while-as-expression) — BEFORE the
    -- skip check, so `break f;` over a loop-local linear counts as its
    -- consumption.
    match value with
    | some expr =>
      -- H14: breaking with a linear ident MOVES it out as the loop result —
      -- value-mode checkExpr auto-consumes it. `breakConsumeExempt` grants
      -- the one-level loop-depth exemption (a break fires at most once per
      -- loop entry); restore the flag right after.
      let envB ← getEnv
      setEnv { envB with breakConsumeExempt := true }
      let valTy ← checkExpr expr
      modify fun envA => { envA with breakConsumeExempt := envB.breakConsumeExempt }
      let env2 ← getEnv
      match env2.loopBreakTy with
      | none => setEnv { env2 with loopBreakTy := some valTy }
      | some prevTy =>
        if prevTy != valTy then
          throwCheck (.breakTypeMismatch (tyToString valTy) (tyToString prevTy)) (some stmt.getSpan)
    | none => pure ()
    -- Check all linear variables declared in the loop body are consumed or reserved by defer
    for (name, info) in (← getEnv).vars do
      if !info.isCopy && info.state != .consumed && info.state != .reserved && info.loopDepth >= env.loopDepth then
        throwCheck (.breakSkipsUnconsumedLinear name) (some stmt.getSpan)
  | .continue_ _ lbl =>
    let env ← getEnv
    if env.loopDepth == 0 then
      throwCheck .continueOutsideLoop (some stmt.getSpan)
    -- Validate label if present
    match lbl with
    | some l =>
      if !env.loopLabels.contains l then
        throwCheck (.unknownLoopLabel l) (some stmt.getSpan)
    | none => pure ()
    -- Check all linear variables declared in the loop body are consumed or reserved by defer
    for (name, info) in env.vars do
      if !info.isCopy && info.state != .consumed && info.state != .reserved && info.loopDepth >= env.loopDepth then
        throwCheck (.continueSkipsUnconsumedLinear name) (some stmt.getSpan)
  -- assert(e)/assume(e): the condition must be a boolean. Both are proof-only
  -- (erased in Elab); the obligation (assert) / assumption taint (assume) is
  -- surfaced by the contracts report.
  | .assert_ _ cond | .assume_ _ cond =>
    let t ← checkExpr cond none
    expectTy .bool t "assert/assume condition" (some stmt.getSpan)
  -- These are desugared before reaching Check; should never appear
  | .letDestructure _ _ _ _ _ _ => pure ()
  | .letStructDestructure _ structName bindings value =>
    -- Linear move-destructure: `let Struct { a, b } = value` CONSUMES `value` and
    -- moves each named field out into an owned binding. Checked natively (not via
    -- `let __destr = value; let a = __destr.a`, which is unsound for a linear struct
    -- — field access doesn't move, so the temp would leak). Elab expands it to the
    -- temp+field form for codegen, past this checker. See docs/language/OWNERSHIP_MODEL.md.
    let valTy ← checkExpr value (some (.named structName))
    -- The destructure moves the source — value-mode checkExpr auto-consumes.
    -- Resolve any generic type arguments so field types are concrete.
    let typeArgs := match ← resolveType valTy with
      | .generic _ args _ => args
      | _ => []
    let typeParams := match ← lookupStruct structName with
      | some sd => sd.typeParams
      | none => []
    let mapping := typeParams.zip typeArgs
    let structIsCopy ← isCopyType valTy
    if !structIsCopy then
      match ← lookupStruct structName with
      | some sd =>
        for sf in sd.fields do
          if !(bindings.contains sf.name) then
            throwCheck (.missingFieldInLiteral sf.name s!"struct destructure '{structName}'") (some stmt.getSpan)
      | none => pure ()
    for b in bindings do
      match ← lookupStructField structName b with
      | some fieldTy =>
        -- H16 for destructure binders too (bug 094).
        checkNoLiveShadow b (some stmt.getSpan)
        addVar b (substTy mapping fieldTy) true (declSpan := some stmt.getSpan)
      | none => throwCheck (.structHasNoField structName b) (some stmt.getSpan)

partial def checkStmts (stmts : List Stmt) (retTy : Ty) : CheckM Unit := do
  let mut accumulated : Diagnostics := []
  for stmt in stmts do
    let envBefore ← getEnv
    let result := (checkStmt stmt retTy).run envBefore |>.run
    match result with
    | (.ok (), envAfter) => setEnv envAfter
    | (.error ds, _) =>
      accumulated := accumulated ++ ds
      -- Restore env so subsequent statements see a consistent state.
      -- For let-declarations, add the variable with its declared type (or placeholder)
      -- so later statements referencing it don't cascade spurious errors.
      setEnv envBefore
      match stmt with
      | .letDecl _ name _ ty _ _ =>
        let placeholderTy := ty.getD .placeholder
        -- A declared type addVar itself rejects (E0239) would throw from the
        -- recovery path and drop the diagnostics already accumulated.
        let placeholderTy := if tyHoldsRefInAggregate placeholderTy then .placeholder else placeholderTy
        addVar name placeholderTy false
      | _ => pure ()
  if !accumulated.isEmpty then
    throw accumulated

/-- After if/else, check both branches agree on linear var consumption.
    A DIVERGING branch (ends in return/break/continue/abort) never reaches the
    merge point: it cannot disagree with the other branch, and it contributes
    nothing to the merged state — `if c { consume(x); return 1; } else { … }
    use(x)` is sound. Only the fall-through branches must agree. -/
partial def mergeVarStates
    (before : List (String × VarInfo))
    (afterThen : List (String × VarInfo))
    (afterElse : List (String × VarInfo))
    (thenDiverges : Bool := false)
    (elseDiverges : Bool := false)
    (span : Option Span := none) : CheckM Unit := do
  for (name, infoBefore) in before do
    if infoBefore.isCopy then continue
    let thenState := match lookupByUid afterThen infoBefore.uid with
      | some info => info.state
      | none => infoBefore.state
    let elseState := match lookupByUid afterElse infoBefore.uid with
      | some info => info.state
      | none => infoBefore.state
    -- Both consumed or both not-consumed (used/unconsumed are equivalent here)
    let thenConsumed := thenState == .consumed
    let elseConsumed := elseState == .consumed
    if !thenDiverges && !elseDiverges && thenConsumed != elseConsumed then
      throwCheck (.linearConsumedOneBranchNotOther name) span
    -- Apply the most progressed state (consumed > used > unconsumed) of the
    -- branches that actually FALL THROUGH to the merge point.
    let mergedState :=
      if thenDiverges && elseDiverges then thenState  -- unreachable after; arbitrary
      else if thenDiverges then elseState
      else if elseDiverges then thenState
      else if thenState == .consumed then .consumed
      else if thenState == .used || elseState == .used then .used
      else infoBefore.state
    if mergedState != infoBefore.state then
      let env ← getEnv
      let vars' := env.vars.map fun (n, vi) =>
        if vi.uid == infoBefore.uid then (n, { vi with state := mergedState })
        else (n, vi)
      setEnv { env with vars := vars' }

/-- For if-without-else: the then branch must not consume any linear var
    that existed before the if. -/
partial def checkNoBranchConsumption
    (before : List (String × VarInfo))
    (afterThen : List (String × VarInfo))
    (ctx : String)
    (span : Option Span := none) : CheckM Unit := do
  for (name, infoBefore) in before do
    if infoBefore.isCopy then continue
    if infoBefore.state == .consumed then continue
    let thenState := match lookupByUid afterThen infoBefore.uid with
      | some info => info.state
      | none => infoBefore.state
    if thenState == .consumed then
      throwCheck (.linearConsumedNoBranch name ctx) span

end

private def resolveTypeParams (ty : Ty) (typeParams : List String) : Ty :=
  match ty with
  | .named n => if typeParams.contains n then .typeVar n else ty
  | .ref t => .ref (resolveTypeParams t typeParams)
  | .refMut t => .refMut (resolveTypeParams t typeParams)
  | .ptrMut t => .ptrMut (resolveTypeParams t typeParams)
  | .ptrConst t => .ptrConst (resolveTypeParams t typeParams)
  | .array t n => .array (resolveTypeParams t typeParams) n
  | .generic name args caps => .generic name (args.map fun a => resolveTypeParams a typeParams) caps
  | _ => ty

def checkFn (f : FnDef) : CheckM Unit := do
  -- Validate #[test] constraints
  if f.isTest then
    if !f.params.isEmpty then
      throwCheckMsg s!"#[test] function '{f.name}' must have no parameters" f.span
    if !f.typeParams.isEmpty then
      throwCheckMsg s!"#[test] function '{f.name}' must not be generic" f.span
    if f.retTy != .i32 then
      throwCheckMsg s!"#[test] function '{f.name}' must return i32" f.span
  -- Save env state (vars from previous functions shouldn't leak)
  let envBefore ← getEnv
  -- Resolve type parameter names: .named "T" -> .typeVar "T"
  let retTyRaw := resolveTypeParams f.retTy f.typeParams
  -- Set current return type, type params, capability context, and type bounds
  let env1 := { envBefore with currentRetTy := retTyRaw, currentTypeParams := f.typeParams }
  let env2 := { env1 with currentCapSet := f.capSet, currentFnTrusted := f.isTrusted }
  let env3 := { env2 with currentTypeBounds := f.typeBounds }
  setEnv { env3 with currentFnName := f.name }
  -- Resolve Self in return type (needs currentImplType from env)
  let retTy ← resolveType retTyRaw
  -- References are second-class: NO function may return a reference, directly or
  -- nested (VALUE_MODEL.md). This is the blanket signature rule that completes
  -- the "never returned" invariant — references flow downward only. Return a
  -- value, an owned view, a scoped callback (with_value/with_at), or a raw
  -- pointer (*const/*mut) for low-level/unsafe access.
  if tyContainsRef retTy then
    throwCheckMsg s!"function '{f.name}' may not return a reference ('{tyToString retTy}'); references are second-class and cannot be returned — use a value, an owned view, a scoped callback (with_value), or a raw pointer for low-level access (VALUE_MODEL.md)" f.span
  modify fun env => { env with currentRetTy := retTy }
  -- Add params to env. H17 (ruled 2026-07-05): params are OWNED LOCALS — the
  -- caller moved the value in, so the callee holds its obligation and must
  -- consume it like any other binding. (The old policy — "linear params are
  -- consumed by being received" — made `fn drop_it(f: File) {}` a universal
  -- silent-drop escape, enforced only for generic-typed untouched params.)
  -- `&mut T` params are BORROWS: the caller owns the pointee, and the
  -- reference itself is exclusive (non-Copy, can't be duplicated) but carries
  -- no consume obligation — dropping a borrow releases it. Only OWNED params
  -- take the obligation. (`&T` params are Copy and never tracked.)
  -- By binding identity (bug 094): a local that shadows a `&mut` parameter
  -- is not exempt from the consume obligation.
  let mut borrowedParams : List Nat := []
  for p in f.params do
    let paramTyRaw := resolveTypeParams p.ty f.typeParams
    let paramTy ← resolveType paramTyRaw
    -- Bug 092: checked here (not only in addVar) so the error carries the fn span.
    if tyHoldsRefInAggregate paramTy then
      throwCheck (.refStoredInAggregate s!"parameter '{p.name}'" (tyToString paramTy)) (some f.span)
    addVar p.name paramTy true  -- params are always mutable for now
    match paramTy with
    | .refMut _ =>
      match (← getEnv).vars.head? with
      | some (_, vi) => borrowedParams := borrowedParams ++ [vi.uid]
      | none => pure ()
    | _ => pure ()
  -- Check body
  checkStmts f.body retTy
  -- Check local bindings AND owned params for the consume obligation.
  let envAfter ← getEnv
  let localVars := envAfter.vars.filter fun (_, vi) =>
    !hasUid envBefore.vars vi.uid && !borrowedParams.contains vi.uid
  -- H9: if the body never terminates (a server's `while true {…}` accept loop, an
  -- unconditional abort), its textual end is unreachable, so a still-live linear
  -- local there is not a leak. `return`/`break` do NOT exempt — those exit the
  -- function and a resource owned at that point genuinely leaks.
  if !(blockNonTerminating f.body) then
    checkScopeExit localVars
  -- Restore env (remove this function's locals)
  setEnv envBefore


/-- Coarse SORT of a spec-body expression: `some true` = boolean, `some false` = integer,
    `none` = cannot tell.

    Deliberately a sort check, not type inference. `#[requires]`/`#[ensures]` expressions are not
    type-checked anywhere either — they are erased metadata — so there is no existing machinery to
    reuse, and wiring the full `CheckM` checker in here would be a much larger change than the
    defect warrants.

    What it catches is the failure that actually happened: `spec fn f(x: i32) -> i32 = true;` was
    accepted, and the refinement tier then emitted `x = true` to three kernels where it fails as a
    type error — right diagnosis, wrong layer. What it does NOT catch is a width mismatch
    (`i32` vs `i64`), and it says so rather than implying otherwise. -/
private partial def specBodySort (paramTys : List (String × Ty)) (specRets : List (String × Ty)) :
    Expr → Option Bool
  | .boolLit _ _ => some true
  | .intLit _ _ => some false
  | .ident _ n => (paramTys.lookup n).map (fun t => t == .bool)
  | .paren _ e => specBodySort paramTys specRets e
  | .unaryOp _ .not_ _ => some true
  | .unaryOp _ _ e => specBodySort paramTys specRets e
  | .binOp _ op l r =>
      match op with
      | .and_ | .or_ => some true
      | .eq | .neq | .lt | .leq | .gt | .geq => some true
      | .add | .sub | .mul | .div | .mod => some false
      | _ => match specBodySort paramTys specRets l with
             | some b => some b
             | none => specBodySort paramTys specRets r
  | .call _ f _ _ => (specRets.lookup f).map (fun t => t == .bool)
  | _ => none

/-- Capability-requiring (impure) function names, bare and qualified. Mirrors `impureFnNames` in
    ReportVC; duplicated rather than imported because Check must not depend on Report, and the two
    are held together by an assertion in `check_contract_negatives.sh` that both derive impurity
    from the same criterion (a non-empty capability set) rather than by a shared definition. The
    behavioural path cannot compare them: Check rejects the program before Report runs. -/
private partial def impureFnsIn (pfx : String) (m : Module) : List String :=
  let q (n : String) := if pfx.isEmpty then n else pfx ++ "." ++ n
  let here := m.functions.filterMap (fun fd =>
    if fd.capSet.isEmpty then none else some [fd.name, q fd.name])
  let subPfx := if pfx.isEmpty then m.name else pfx ++ "." ++ m.name
  here.flatten ++ m.submodules.flatMap (impureFnsIn subPfx)

-- Every name BOUND anywhere inside a spec body, innermost included.

--  Returns the names rather than a `Bool` so the diagnostic can point at the binding that
--  caused the rejection; a bare "contains a binder" sends the author looking. See the soundness
--  note at the use site.

--  Two properties this must have, and an earlier version had neither:

--  * It recurses through STATEMENT containers, not only expressions. An if-expression's branches
--    are `List Stmt`, so a `let` nested three blocks deep is the likely regression, and a
--    shallow check would pass it.
--  * It reports only ACTUAL bindings. Rejecting every if-expression was over-containment: a
--    binder-free `if x > 0 { 1 } else { 2 }` is perfectly safe for by-name substitution, and
--    banning it would have thrown out the whole conditional fragment to contain a defect that
--    only shadowing can trigger.
mutual
private partial def specBinders : Expr → List String
  | .ifExpr _ c t e => specBinders c ++ (t ++ e).flatMap specBindersStmt
  | .match_ _ sc arms => specBinders sc ++ arms.flatMap specBindersArm
  | .paren _ e | .unaryOp _ _ e | .cast _ e _ => specBinders e
  | .binOp _ _ l r => specBinders l ++ specBinders r
  | .call _ _ _ args => args.flatMap specBinders
  | .fieldAccess _ o _ => specBinders o
  | .arrayIndex _ a i => specBinders a ++ specBinders i
  | _ => []

private partial def specBindersArm : MatchArm → List String
  | .mk _ _ _ bs g b => bs ++ (g.map specBinders).getD [] ++ b.flatMap specBindersStmt
  | .varArm _ n g b => n :: (g.map specBinders).getD [] ++ b.flatMap specBindersStmt
  | .litArm _ v g b => specBinders v ++ (g.map specBinders).getD [] ++ b.flatMap specBindersStmt
  | .rangeArm _ lo hi _ g b =>
      specBinders lo ++ specBinders hi ++ (g.map specBinders).getD [] ++ b.flatMap specBindersStmt

private partial def specBindersStmt : Stmt → List String
  | .letDecl _ n _ _ v _ => n :: specBinders v
  | .letDestructure _ _ _ bs v eb => bs ++ specBinders v ++ (eb.map (·.flatMap specBindersStmt)).getD []
  | .letStructDestructure _ _ bs v => bs ++ specBinders v
  | .borrowIn _ _ r _ _ b => r :: b.flatMap specBindersStmt
  | .forLoop _ init c st b _ =>
      (init.map specBindersStmt).getD [] ++ specBinders c
        ++ (st.map specBindersStmt).getD [] ++ b.flatMap specBindersStmt
  | .ifElse _ c t e => specBinders c ++ t.flatMap specBindersStmt ++ (e.map (·.flatMap specBindersStmt)).getD []
  | .while_ _ c b _ => specBinders c ++ b.flatMap specBindersStmt
  | .expr _ e _ | .assert_ _ e | .assume_ _ e | .defer _ e => specBinders e
  | .assign _ _ v => specBinders v
  | .return_ _ v => (v.map specBinders).getD []
  | .break_ _ v _ => (v.map specBinders).getD []
  | .fieldAssign _ o _ v => specBinders o ++ specBinders v
  | .derefAssign _ t v => specBinders t ++ specBinders v
  | .arrayIndexAssign _ a i v => specBinders a ++ specBinders i ++ specBinders v
  | .continue_ _ _ => []
end

/-- Function names called anywhere in an expression — for validating `spec fn` bodies. -/
private partial def specBodyCalls : Expr → List String
  | .call _ f _ args => f :: args.flatMap specBodyCalls
  | .paren _ e | .unaryOp _ _ e | .cast _ e _ => specBodyCalls e
  | .binOp _ _ l r => specBodyCalls l ++ specBodyCalls r
  | .fieldAccess _ o _ => specBodyCalls o
  | .arrayIndex _ a i => specBodyCalls a ++ specBodyCalls i
  | _ => []

/-- Bug 082: every name a module can CALL has exactly one binding in that module.

    `import a.{check_version as check_class}` beside another module's
    `check_class` compiled, and both the binary and `--interp` called the OTHER
    `check_class`: the alias was a lookup-order preference, and each consumer had
    its own order. Check searched imports before local functions, Elab's callee
    identity searched local functions first, and the backends searched every
    definition in the program before any alias. A rename that only wins some of
    those races is not a binding.

    Two shapes are refused here, where the module's import list and its own
    definitions are both in view:
      * an imported FUNCTION whose local name is also a function or extern
        defined in this module;
      * two imports binding one local name to different definitions.
    Re-importing the SAME definition twice stays legal. Elab then spells an
    aliased call by the definition it names, so no global table is consulted
    with the alias spelling (see `Elab.importCallSpellings`). -/
def importBindingCollisions (m : Module) (imports : ResolvedImports) : Diagnostics :=
  -- The module's whole lowered callable namespace: free functions, externs, AND impl
  -- methods (inherent and trait), which Core spells `Type_method`. An import aliased to
  -- `S_value` beside a local `impl S { fn value }` bound one name twice: the call took the
  -- import's signature and spelling while the proof's callee identity took the local method
  -- (Codex review of bug 082).
  let methodNames := m.implBlocks.flatMap (fun ib => ib.methods.map fun f => mangledMethodName ib.typeName f.name)
    ++ m.traitImpls.flatMap (fun tb => tb.methods.map fun f => mangledMethodName tb.typeName f.name)
  let localFnNames := m.functions.map (·.name) ++ m.externFns.map (·.name) ++ methodNames
  -- `imports.functions` is keyed by local name and covers functions AND externs.
  let importedFnNames := imports.functions.map (·.1)
  let syms : List (String × String × String × Span) := m.imports.flatMap fun imp =>
    imp.symbols.map fun sym => (sym.effectiveName, imp.moduleName, sym.name, imp.span)
  let fnSyms := syms.filter fun (local_, _, _, _) => importedFnNames.contains local_
  let vsLocal : Diagnostics := fnSyms.filterMap fun (local_, modName, orig, sp) =>
    if localFnNames.contains local_ then
      let what := if methodNames.contains local_ then "an impl method defined in this module"
        else "a function defined in this module"
      let e := CheckError.importBindingCollision local_
        s!"is also {what} (import of '{modName}.{orig}')"
      some { severity := .error, message := e.message, pass := "check", span := some sp,
             hint := e.hint, code := e.code }
    else none
  -- Duplicates: report the SECOND binding of a name, once per name.
  let (_, dups) := syms.foldl (init := (([] : List (String × String × String)), ([] : Diagnostics)))
    fun (seen, ds) (local_, modName, orig, sp) =>
      match seen.find? fun (l, _, _) => l == local_ with
      | some (_, m0, o0) =>
        if m0 == modName && o0 == orig then (seen, ds)
        else
          let e := CheckError.importBindingCollision local_
            s!"is already bound to '{m0}.{o0}' by another import (here: '{modName}.{orig}')"
          (seen, { severity := .error, message := e.message, pass := "check", span := some sp,
                   hint := e.hint, code := e.code } :: ds)
      | none => ((local_, modName, orig) :: seen, ds)
  vsLocal ++ dups.reverse

def checkModule (m : Module) (summary : FileSummary)
    (imports : ResolvedImports := {}) : Except Diagnostics Unit :=
  let collisions := importBindingCollisions m imports
  if !collisions.isEmpty then .error collisions else
  -- Use pre-built summaries from FileSummary
  let fnSigs : List FnSummary := summary.functions.map Prod.snd
  let externSigs : List FnSummary := summary.externFnSigs.map Prod.snd
  let importedSigList := imports.functions.map Prod.snd
  let baseOffset := importedSigList.length
  -- Built-in functions for strings and I/O (shared definition in BuiltinSigs.lean)
  let builtinSigs := builtinFnSigs.map Prod.snd
  let builtinOffset := baseOffset + fnSigs.length
  let builtinNames : List (String × Nat) :=
    (enumerateList builtinFnSigs).map fun (idx, (name, _)) => (name, builtinOffset + idx)
  -- Add submodule functions/extern fns with qualified names (mod_fn)
  let submoduleSigs : List FnSummary := summary.submoduleSummaries.foldl (fun acc (_, subSummary) =>
    acc ++ subSummary.functions.map Prod.snd
    ++ subSummary.externFnSigs.map Prod.snd
  ) []
  let submoduleNames : List (String × Nat) := summary.submoduleSummaries.foldl (fun (acc : List (String × Nat)) (subName, subSummary) =>
    let baseIdx := baseOffset + fnSigs.length + builtinSigs.length + externSigs.length + acc.length
    let fnNames' : List (String × Nat) := (enumerateList subSummary.functions).map fun (idx, (name, _)) =>
      (subName ++ "_" ++ name, baseIdx + idx)
    let efNames : List (String × Nat) := (enumerateList subSummary.externFnSigs).map fun (idx, (name, _)) =>
      (subName ++ "_" ++ name, baseIdx + subSummary.functions.length + idx)
    acc ++ fnNames' ++ efNames
  ) []
  let externOffset := builtinOffset + builtinSigs.length
  let externNames : List (String × Nat) :=
    (enumerateList summary.externFnSigs).map fun (idx, (name, _)) => (name, externOffset + idx)
  -- Collect all impl block methods (pre-built + imported, then resolve Self)
  let localImplSigs := summary.implMethodSigs
  let allImplBlocks := imports.implBlocks ++ m.implBlocks
  let allTraitImpls := imports.traitImpls ++ m.traitImpls
  let implMethodSigs := resolveImplMethodSigs (imports.implMethodSigs ++ localImplSigs) allImplBlocks allTraitImpls
  let traitImplMethodSigs : List (String × FnSummary) := []
  let implSigList := (implMethodSigs ++ traitImplMethodSigs).map Prod.snd
  -- allSigs order is: imported ++ fnSigs ++ builtin ++ extern ++ submodule ++ impl.
  -- implOffset MUST include submoduleSigs.length — otherwise, when any submodule
  -- exists, every impl-method name (e.g. `String_drop`) indexes into the
  -- submodule-sigs region instead of the impl region, so `s.drop()` resolves to
  -- a submodule function's signature and stops being recognized as a by-value-
  -- self consume (a linear value is then falsely E0208). Found by the #35
  -- log-analyzer workload: any multi-module project owning a linear value.
  let implOffset := externOffset + externSigs.length + submoduleSigs.length
  let implNames : List (String × Nat) :=
    (enumerateList (implMethodSigs ++ traitImplMethodSigs)).map fun (idx, (name, _)) => (name, implOffset + idx)
  let allSigs := importedSigList ++ fnSigs ++ builtinSigs ++ externSigs ++ submoduleSigs ++ implSigList
  let importedNames : List (String × Nat) :=
    (enumerateList imports.functions).map fun (idx, (name, _)) => (name, idx)
  let fnNames : List (String × Nat) :=
    (enumerateList summary.functions).map fun (idx, (name, _)) => (name, baseOffset + idx)
  let allNames := importedNames ++ fnNames ++ builtinNames ++ externNames ++ submoduleNames ++ implNames
  let userFnNamesList := (importedNames ++ fnNames ++ externNames ++ submoduleNames ++ implNames).map Prod.fst
  -- Build named function signature map for fnRef resolution
  let fnSigPairs : List (String × FnSummary) :=
    summary.functions ++ (implMethodSigs ++ traitImplMethodSigs)
  -- Use resolved declarations so definition-site type provenance is present on
  -- the same lookup path Elab and the evidence producer consume.
  let allStructs := imports.structs ++ summary.structs
  -- Built-in Option<T> enum (Some { value: T }, None {})
  -- One owner: `Concrete.Resolve.BuiltinEnums` (bug 065). Both the builtin
  -- enums and the shadowing rule were stated twice, and Check consulted
  -- different inputs than Elab.
  let builtinEnumList := builtinEnums summary.enums imports.enums
  let allEnums := builtinEnumList ++ imports.enums ++ summary.enums
  -- Build type aliases map
  let localTypeAliases : List (String × Ty) := m.typeAliases.map fun ta => (ta.name, ta.targetTy)
  -- Transitively close so chains (`type B = A; type A = i32`) resolve in one lookup.
  let typeAliasMap : List (String × Ty) := closeAliasMap (imports.typeAliases ++ localTypeAliases)
  -- Build constants map
  let constantsMap : List (String × Ty) := m.constants.map fun c => (c.name, c.ty)
  -- Build trait impl pairs for bound checking
  let traitImplPairs : List (String × String) := allTraitImpls.map fun tb => (tb.typeName, tb.traitName)
  -- Collect newtypes from module, submodules, and imports.
  -- Imported newtypes participate in type identity (Port ≠ u16 must hold
  -- across module boundaries) and reach Layout via the elaborated CModule.
  let allNewtypes := m.newtypes ++ imports.newtypes
                     ++ m.submodules.foldl (fun acc sub => acc ++ sub.newtypes) []
  -- 0b construction rights: newtypes DEFINED here (module + submodules),
  -- NOT the imported ones — direct `N(v)` construction is private to the
  -- defining module.
  let localNewtypeDefs := m.newtypes
                     ++ m.submodules.foldl (fun acc sub => acc ++ sub.newtypes) []
  let localNewtypeNamesList := localNewtypeDefs.map (fun nt => nt.name)
  -- 0b slice 2: structs defined HERE (module + submodules). Field privacy
  -- is enforced against non-local structs.
  let localStructDefs := m.structs
                     ++ m.submodules.foldl (fun acc sub => acc ++ sub.structs) []
  let localStructNamesList := localStructDefs.map (fun sd => sd.name)
  let initEnv : TypeEnv :=
    { vars := [], structs := allStructs, enums := allEnums, functions := allSigs,
      fnNames := allNames, loopDepth := 0, typeAliases := typeAliasMap, constants := constantsMap,
      traitImpls := traitImplPairs, allFnSummarys := fnSigPairs, newtypes := allNewtypes,
      userFnNames := userFnNamesList, localNewtypeNames := localNewtypeNamesList,
      localStructNames := localStructNamesList }
  -- Module-level declaration checks (Copy/Destroy, repr, FFI, traits) moved to CoreCheck.lean.
  -- Reserved name check stays here because reserved names conflict with builtins in Check.
  let reservedNameCheck := m.functions.foldl (init := (Except.ok () : Except Diagnostics Unit)) fun acc f =>
    match acc with
    | .error e => .error e
    | .ok () =>
      if isReservedFnName f.name then
        .error [{ severity := .error, message := CheckError.message (.reservedName f.name), pass := "check", span := some f.span, hint := none }]
      else .ok ()
  match reservedNameCheck with
  | .error e => .error e
  | .ok () =>
  -- Bug 092: references are second-class — a struct or enum field may not hold
  -- one (VALUE_MODEL.md, BYTE_VIEW.md). Rejected at the declaration so the type
  -- is not expressible; literals and bindings are checked as well (generic
  -- instantiations).
  -- A declaration's own type parameters shadow module aliases of the same name
  -- (`type T = &Int; struct Box<T> { value: T }` stores a `T`, not a `&Int`):
  -- they are made type variables before the alias expansion below.
  let ownParams (ps : List String) (ty : Ty) : Ty := substTy (ps.map fun n => (n, Ty.typeVar n)) ty
  let refFields : List (String × Ty × Span) :=
    (m.structs.flatMap fun sd => sd.fields.map fun f =>
        (s!"field '{f.name}' of struct '{sd.name}'", ownParams sd.typeParams f.ty, sd.span))
    ++ (m.enums.flatMap fun ed => ed.variants.flatMap fun v =>
          v.fields.map fun f => (s!"field '{f.name}' of {ed.name}::{v.name}", ownParams ed.typeParams f.ty, ed.span))
    -- A newtype is a one-field wrapper: its inner type is a stored field too.
    ++ (m.newtypes.map fun nt => (s!"inner type of newtype '{nt.name}'", ownParams nt.typeParams nt.innerTy, nt.span))
  -- Through type aliases (`type Ref = &Int; newtype W = Ref;`).
  match refFields.find? (fun (_, ty, _) =>
      tyStoresRef (expandAliasDeep typeAliasMap (typeAliasMap.length + 64) ty)) with
  | some (desc, ty, sp) =>
    .error [{ severity := .error, message := CheckError.message (.refStoredInAggregate desc (tyToString ty)),
              pass := "check", span := some sp, hint := CheckError.hint (.refStoredInAggregate desc (tyToString ty)),
              code := CheckError.code (.refStoredInAggregate desc (tyToString ty)) }]
  | none =>
  -- A `spec fn` with a DEFINITIONAL body must have that body checked. The body-less form has
  -- nothing to check, so nothing did -- and when bodies were added, a spec could reference an
  -- undeclared variable or a nonexistent function and be accepted silently. The refinement tier
  -- would then render that nonsense into three kernels, where it fails as an unbound identifier:
  -- loud, but at the wrong layer and long after the mistake.
  --
  -- Two rules, both from what a spec IS: pure and total. Its body may mention only its own
  -- parameters, and may call only other spec functions (a call to an executable function would
  -- make the spec depend on runtime behaviour, which is the thing specs exist to be independent
  -- of).
  let specNames := m.specFns.map (·.name)
  let specBodyCheck := m.specFns.foldl (init := (Except.ok () : Except Diagnostics Unit)) fun acc sd =>
    match acc with
    | .error e => .error e
    | .ok () =>
      match sd.body with
      | none => .ok ()
      | some b =>
        let paramNames := sd.params.map (·.name)
        let free := (collectFreeVarsExpr b paramNames).eraseDups
        let unknownCalls := (specBodyCalls b).filter (fun c => !specNames.contains c)
        -- BINDER-FREE, and this is a soundness requirement rather than a simplification.
        -- `refineObligations` substitutes the spec's parameters with the call's arguments using
        -- `substExpr`, which replaces by NAME with no capture avoidance. A body such as
        --     spec fn s(x: i32) -> i32 = if x > 0 { let x: i32 = 100; x } else { x };
        -- would have its inner, let-bound `x` substituted too, corrupting the obligation.
        --
        -- Today that produces no wrong evidence only because `renderTerm` cannot render an
        -- if-expression, so the obligation reports "outside the fragment" and no kernel sees it.
        -- That is safety by ACCIDENT: it rests on the renderer's narrowness, and the first person
        -- to add `.ifExpr` to `renderTerm` -- an obvious extension -- makes the corruption live.
        -- Rejecting binders here makes the property checked rather than incidental.
        let binders := (specBinders b).eraseDups
        if !binders.isEmpty then
          .error [{ severity := .error
                  , message := s!"spec fn '{sd.name}' body binds name(s): {", ".intercalate binders}; spec bodies must be binder-free"
                  , pass := "check", span := some sd.span
                  , hint := some "TEMPORARY: parameter substitution is by name and would capture a shadowing binding. The restriction lifts once substitution is capture-avoiding — until then keep the body free of `let`, match patterns, and loops." }]
        else if !free.isEmpty then
          .error [{ severity := .error
                  , message := s!"spec fn '{sd.name}' body references unknown name(s): {", ".intercalate free}"
                  , pass := "check", span := some sd.span
                  , hint := some "a spec body may only mention its own parameters" }]
        else if
            let paramTys := sd.params.map (fun p => (p.name, p.ty))
            let specRets := m.specFns.map (fun d => (d.name, d.retTy))
            match specBodySort paramTys specRets b with
            | some isBool => isBool != (sd.retTy == (Ty.bool))
            | none => false
          then
          .error [{ severity := .error
                  , message := s!"spec fn '{sd.name}' body has the wrong sort: declared -> {tyToString sd.retTy}, but the body is {if sd.retTy == (Ty.bool) then "an integer" else "a boolean"} expression"
                  , pass := "check", span := some sd.span
                  , hint := some "a spec body must produce the declared return type" }]
        else if !unknownCalls.isEmpty then
          .error [{ severity := .error
                  , message := s!"spec fn '{sd.name}' body calls non-spec function(s): {", ".intercalate unknownCalls}"
                  , pass := "check", span := some sd.span
                  , hint := some "a spec body may only call other `spec fn`s — calling executable code would make the spec depend on runtime behaviour" }]
        else .ok ()
  match specBodyCheck with
  | .error e => .error e
  | .ok () =>
  -- CONTRACT NAME-AND-SORT VALIDATION. Called that rather than "contract type checking", which
  -- would overstate it: what runs here is unknown-name rejection plus a COARSE sort check, and
  -- the following are measured as NOT checked (see H27) --
  --   * operand type compatibility (`x < 9999999999` against an i32 is accepted);
  --   * spec-function argument and return types;
  --   * `result`'s type (its SCOPE is enforced, its type is not);
  --   * that a `#[variant]` is a well-founded measure (a boolean variant is accepted);
  --   * exclusion of runtime/effectful calls from a contract;
  --   * precise loop-binder scope (the bound set is over-approximate; see below).
  -- Real type checking needs the per-function environment and belongs with the typed contract
  -- record, not here. Naming this honestly is the difference between a known gap and a false
  -- impression that contracts are typed.
  --
  -- `#[requires]`/`#[ensures]` were not validated at all, anywhere -- they are erased
  -- metadata, so no pass had reason to look at them, and each of the seven consumers took the
  -- raw `List Expr` on faith. Measured before writing this: `#[requires(nosuchvar > 0)]` is
  -- accepted, and `nosuchvar` then appears as a free variable in the HYPOTHESES of every
  -- obligation in the function, including ones reported `proved_by_kernel_decision`.
  --
  -- That particular case is not unsound -- a hypothesis over a fresh universally quantified
  -- variable is discharged at the call site, where it is unprovable, so it fails closed. The
  -- defect is that the failure surfaces as an unprovable call-site obligation somewhere else
  -- rather than as a typo where the typo is, and that a precondition nobody can establish reads
  -- in the report exactly like one nobody has established YET.
  --
  -- The rules are the spec-body rules, for the same reasons: a contract may mention only names
  -- that exist, and it must be a proposition rather than a number.
  let contractCheck :=
    let constNames := constantsMap.map (·.1)
    let impureFns := (impureFnsIn "" m).eraseDups
    let specRets := m.specFns.map (fun d => (d.name, d.retTy))
    let allContractFns := m.functions ++ m.implBlocks.flatMap (·.methods)
    allContractFns.foldl (init := (Except.ok () : Except Diagnostics Unit)) fun acc f =>
      match acc with
      | .error e => .error e
      | .ok () =>
        let paramTys := f.params.map (fun p => (p.name, p.ty))
        -- `result` is bound in an `ensures` and nowhere else; that asymmetry is the whole
        -- reason the two clause lists cannot share one bound-name set.
        let boundReq := (f.params.map (·.name)) ++ constNames
        let boundEns := "result" :: boundReq
        let badName (bound : List String) (e : Expr) : List String :=
          (collectFreeVarsExpr e bound).eraseDups
        let freeR := f.requires.flatMap (badName boundReq)
        let freeE := f.ensures.flatMap (badName boundEns)
        -- LOOP CONTRACTS. An unbound name in an `#[invariant]` is worse-looking than in a
        -- `requires`: it is universally quantified into the initiation obligation, which then
        -- reads `forall (bogusname : Int), 0 <= 0 /\ 0 <= bogusname`. That is false, so the loop
        -- fails closed -- but only by luck of where the name landed, and the report shows a loop
        -- whose invariant cannot be established rather than a typo.
        --
        -- OVER-APPROXIMATE SCOPE, said plainly: the bound set is every name bound ANYWHERE in the
        -- function body, not the names in scope at the loop. So a local declared in an unrelated
        -- branch is wrongly admitted here. That accepts too much, never too little, and it
        -- catches the defect that actually occurs (a name that exists nowhere). Enforcing real
        -- scoping needs the per-function environment, which this module-level pass does not have;
        -- it belongs with the typed contract record rather than being faked here.
        let localNames := f.body.flatMap specBindersStmt
        let boundLoop := boundReq ++ localNames
        let loopExprs := f.loopContracts.flatMap (fun lc => lc.invariants ++ lc.variant.toList)
        let freeL := loopExprs.flatMap (badName boundLoop)
        let free := (freeR ++ freeE ++ freeL).eraseDups
        -- SHADOWING is the gap unknown-name rejection cannot close, and it is the one place where
        -- "every name resolved" is not the same as "the contract means what it says".
        --
        -- A local may shadow a parameter (measured, not assumed). In
        --     #[ensures(result == x)]
        --     fn f(x: i32) -> i32 { let x: i32 = 99; return x; }
        -- every name resolves, no diagnostic fires, and nothing in the pipeline records WHICH `x`
        -- the contract means. A contract talks about the function's interface, so the parameter is
        -- the only defensible reading -- but the obligation carries the bare name `x`, and the
        -- reading is therefore established nowhere.
        --
        -- Contained the same way as H25, and for the same reason: the ambiguity is not currently
        -- exploitable (the postcondition is not discharged), and relying on that is relying on a
        -- downstream stage's incompleteness rather than on a rule here. Rejecting is cheap, and it
        -- keeps the refinement tier from beginning to discharge these while the meaning is
        -- undecided. The restriction lifts when contract expressions carry binding identities
        -- rather than names.
        let contractNames :=
          (f.requires ++ f.ensures ++ loopExprs).flatMap (fun e => collectFreeVarsExpr e []) |>.eraseDups
        let contractCallNames :=
          (f.requires ++ f.ensures ++ loopExprs).flatMap specBodyCalls |>.eraseDups
        let shadowedParams := (f.params.map (·.name)).filter fun pn =>
          localNames.contains pn && contractNames.contains pn
        -- EFFECTFUL-CALL CONTAINMENT. Named narrowly on purpose: this is NOT purity or totality
        -- checking, and calling it that would overstate it. A capability-requiring function
        -- performs I/O, so `#[ensures(result == tick())]` states a postcondition whose truth
        -- depends on when it is evaluated and what the outside world did — not a proposition
        -- about the program. That case is now rejected.
        --
        -- What "capability-free" does NOT establish, each measured against the compiler rather
        -- than reasoned about (see H27):
        --   * `fn d(n) { return 100 / n; }` — can TRAP; admitted in a contract today;
        --   * a self-recursive function — may DIVERGE; admitted;
        --   * a `#[trusted]` function — admitted, and its body is outside the checked language.
        -- So the boundary enforced here is "no declared effects", and the boundary that would be
        -- correct is "resolves to a spec fn, or to an executable function with an explicit,
        -- checked logical interpretation". The latter notion does not exist yet; inventing a
        -- weaker one and naming it purity is how a gap becomes invisible.
        --
        -- `--report contracts` already detected this ("impure call 'tick' — spec/ghost must be
        -- pure and total") and could not reject it, so the call still reached the obligation. Same
        -- reported-but-not-rejected shape as the unbound-name defect, promoted the same way, with
        -- the wording kept identical so it reads as one rule rather than two.
        --
        -- Scoped to IMPURE calls deliberately: a contract calling a PURE helper is legal and
        -- covered by a positive control in `check_contract_negatives.sh`. Requiring spec-only
        -- calls (the rule for spec BODIES) would break real contracts; purity is the property
        -- that actually matters here.
        let impureCalls := contractCallNames.filter impureFns.contains
        -- Sort reuses `specBodySort`, so a contract and a spec body are held to one standard.
        -- `result` is typed by the function's return type; a contract mentioning it is otherwise
        -- indistinguishable from one that does not.
        let sortTys := ("result", f.retTy) :: paramTys
        -- A VARIANT must be an integer MEASURE, so it gets the opposite sort rule from everything
        -- else here: a boolean variant is meaningless (`#[variant(i < n)]` does not decrease, it
        -- flips), and it was accepted because the proposition check simply skipped variants
        -- instead of holding them to their own rule. Absence of a rule read as absence of a
        -- requirement.
        let wrongVariant := f.loopContracts.filterMap (fun lc => lc.variant) |>.filter fun e =>
          match specBodySort sortTys specRets e with
          | some isBool => isBool
          | none => false
        -- Invariants join the proposition check; variants are excluded from it and checked above.
        let wrongSort := (f.requires ++ f.ensures ++ f.loopContracts.flatMap (·.invariants)).filter fun e =>
          match specBodySort sortTys specRets e with
          | some isBool => !isBool
          | none => false
        if !impureCalls.isEmpty then
          .error [{ severity := .error
                  , message := s!"contract on '{f.name}': impure call '{impureCalls.headD ""}' — spec/ghost must be pure and total"
                  , pass := "check", span := some f.span
                  , hint := some "a contract may call `spec fn`s and capability-free functions; a capability-requiring function performs I/O, so the contract's meaning would depend on runtime effects" }]
        else if !shadowedParams.isEmpty then
          .error [{ severity := .error
                  , message := s!"contract on '{f.name}' names parameter(s) shadowed by a local: {", ".intercalate shadowedParams}"
                  , pass := "check", span := some f.span
                  , hint := some "TEMPORARY: the contract carries the bare name, so which binding it means is recorded nowhere. Rename the local until contract expressions carry binding identities." }]
        else if !free.isEmpty then
          .error [{ severity := .error
                    -- Wording matches `validateContractExpr` in ReportVC deliberately. That pass
                    -- already reported this exact defect under `--report contracts`; it just could
                    -- not REJECT it, so the name still reached the VC hypotheses. Two vocabularies
                    -- for one defect would read as two different problems.
                  , message := s!"contract on '{f.name}': {", ".intercalate (free.map (fun n => s!"unknown identifier '{n}'"))}"
                  , pass := "check", span := some f.span
                  , hint := some "a contract may mention parameters, constants, `result` (in `ensures`), and locals (in loop invariants)" }]
        else if !wrongVariant.isEmpty then
          .error [{ severity := .error
                  , message := s!"contract on '{f.name}': `variant` is a boolean, not a decreasing measure"
                  , pass := "check", span := some f.span
                  , hint := some "a `#[variant]` must be an integer expression that strictly decreases each iteration (e.g. `n - i`), not a condition" }]
        else if !wrongSort.isEmpty then
          .error [{ severity := .error
                  , message := s!"contract on '{f.name}' is an integer expression, not a proposition"
                  , pass := "check", span := some f.span
                  , hint := some "a `requires`/`ensures` clause must be a boolean condition" }]
        else .ok ()
  match contractCheck with
  | .error e => .error e
  | .ok () =>
  -- Built-in Destroy trait (needed for expression-level trait method resolution)
  let builtinDestroyTrait : TraitDef := {
    name := destroyTraitName
    methods := [{ name := destroyMethodName, params := [], retTy := .unit, selfKind := some .ref
                  -- The H18 v1 fence (CoreCheck E0584) bounds every Destroy impl by
                  -- Alloc, so a destroy() through a `T: Destroy` bound requires Alloc
                  -- of its caller (bug 091) — the same as a direct call would.
                  capSet := .concrete ["Alloc"] }]
    builtinId := some .destroy
  }
  let allTraits := builtinDestroyTrait :: m.traits
  -- Merge impl block type params into each method's typeParams, track impl type for Self
  -- Only check THIS module's impl/traitImpl methods (not imported ones whose bodies may
  -- reference functions not in scope here).
  let regularFns : List (FnDef × Option Ty) := m.functions.map fun f => (f, none)
  let implMethodPairs : List (FnDef × Option Ty) := m.implBlocks.foldl (fun acc ib =>
    -- `impl<cap C> Writer<C>`: the self type carries the impl's capability parameters
    -- by name (R-0484); the parser already joined them to each method's own.
    let implTy := if ib.typeParams.isEmpty && ib.capParams.isEmpty then tyFromName ib.typeName
                  else Ty.generic ib.typeName (ib.typeParams.map Ty.typeVar)
                         (ib.capParams.map fun c => CapSet.concrete [c])
    -- A method of a `trusted impl` is trusted (as Elab records it): the trusted-only
    -- rules (borrowed drop glue, bug 091 round 3) read `currentFnTrusted`.
    acc ++ ib.methods.map fun f =>
      ({ f with typeParams := ib.typeParams ++ f.typeParams
              , typeBounds := ib.typeBounds ++ f.typeBounds
              , isTrusted := f.isTrusted || ib.isTrusted }, some implTy)
  ) []
  let traitImplMethodPairs : List (FnDef × Option Ty) := m.traitImpls.foldl (fun acc tb =>
    let implTy := if tb.typeParams.isEmpty then tyFromName tb.typeName
                  -- Trait impls take no capability parameters (refused at parse).
                  else Ty.generic tb.typeName (tb.typeParams.map Ty.typeVar) []
    acc ++ tb.methods.map fun f =>
      ({ f with typeParams := tb.typeParams ++ f.typeParams
              , typeBounds := tb.typeBounds ++ f.typeBounds
              , isTrusted := f.isTrusted || tb.isTrusted }, some implTy)
  ) []
  let allFnPairs := regularFns ++ implMethodPairs ++ traitImplMethodPairs
  let (allErrors, _) := allFnPairs.foldl (fun (errs, env) (f, implTy) =>
    let env' := { env with currentImplType := implTy, traits := allTraits }
    let result := (checkFn f).run env' |>.run
    match result with
    | (.ok (), finalEnv) => (errs, finalEnv)
    | (.error ds, _) => (errs ++ ds.addContext s!"while checking function '{f.name}'", env)
  ) (([] : Diagnostics), initEnv)
  if allErrors.isEmpty then .ok ()
  else .error allErrors

/-- Check every submodule's function BODIES, recursively, mirroring Elab's
    submodule context (sibling submodule types injected, imports resolved
    against the global table). Submodule bodies — every `mod x;` file in a
    project, including the whole stdlib, and inline `mod x { … }` nests — were
    previously NEVER front-end checked: only their signatures were consumed, so
    type errors, linearity violations, and immutable assignments in them
    compiled silently (CoreCheck's coarser Core-level rules were the only net). -/
partial def checkSubmodules (m : Module) (summary : FileSummary)
    (summaryTable : List (String × FileSummary)) : Diagnostics :=
  let siblingStructs := summary.submoduleSummaries.foldl (fun acc (_, s) =>
    acc ++ s.structs) ([] : List StructDef)
  let siblingEnums := summary.submoduleSummaries.foldl (fun acc (_, s) =>
    acc ++ s.enums) ([] : List EnumDef)
  let siblingImplMethodSigs := summary.submoduleSummaries.foldl (fun acc (_, s) =>
    acc ++ (s.implMethodSigs.filter fun (name, _) =>
      s.publicNames.contains name)) ([] : List (String × FnSummary))
  -- H12: inside `std`, only migrated submodules are checked (burn-down list
  -- above); everywhere else, every submodule is.
  -- Every submodule is checked — std included. (H12 CLOSED 2026-07-02: the
  -- burn-down list that lived here is gone; std carries zero front-end
  -- violations and stays that way like any other code.)
  m.submodules.foldl (fun errs sub =>
    let subSummary := match summary.submoduleSummaries.find? fun (n, _) => n == sub.name with
      | some (_, s) => s
      | none => buildFileSummary sub
    match resolveImports sub.imports summaryTable
        (fun modName => CheckError.message (.unknownModule modName))
        (fun sym modName => CheckError.message (.notPublicInModule sym modName))
        (pass := "check") with
    | .error ds => errs ++ (ds.addContext s!"while checking module '{sub.name}'").stampFile sub.sourceFile
    | .ok subImports =>
      -- Inject sibling submodule types (filtered against local names), exactly
      -- like Elab does, so cross-submodule type references check.
      let localStructNames := sub.structs.map (·.name)
      let localEnumNames := sub.enums.map (·.name)
      let subImports := { subImports with
        structs := subImports.structs ++ (siblingStructs.filter fun sd =>
          !(localStructNames.contains sd.name))
        enums := subImports.enums ++ (siblingEnums.filter fun ed =>
          !(localEnumNames.contains ed.name))
        implMethodSigs := subImports.implMethodSigs ++ siblingImplMethodSigs }
      let subErrs := match checkModule sub subSummary subImports with
        | .ok () => ([] : Diagnostics)
        | .error ds => (ds.addContext s!"while checking module '{sub.name}'").stampFile sub.sourceFile
      errs ++ subErrs ++ checkSubmodules sub subSummary summaryTable
  ) ([] : Diagnostics)

/-- Check a multi-module program. Consumes resolved modules (proving name resolution
    happened) and uses summary table for import resolution. -/
def checkProgram (resolved : List ResolvedModule)
    (summaryTable : List (String × FileSummary) := []) : Except Diagnostics Unit :=
  -- Build sibling module summaries for inline modules (mod A {} mod B {}).
  -- Each module's summary is indexed by name for qualified :: access.
  let moduleSummaryList : List (String × FileSummary) := resolved.map fun rm =>
    let m := rm.module
    (m.name, match summaryTable.find? fun (n, _) => n == m.name with
      | some (_, s) => s
      | none => buildFileSummary m)
  let allErrors := resolved.foldl (fun errs rm =>
    let m := rm.module
    let summary := match moduleSummaryList.find? fun (n, _) => n == m.name with
      | some (_, s) => s
      | none => buildFileSummary m
    match resolveImports m.imports summaryTable
        (fun modName => CheckError.message (.unknownModule modName))
        (fun sym modName => CheckError.message (.notPublicInModule sym modName))
        (pass := "check") with
    | .error ds => errs ++ ds
    | .ok imports =>
      -- Inject sibling module functions for qualified :: access
      let siblingFns : List (String × FnSummary) := moduleSummaryList.foldl (fun acc (sibName, sibSummary) =>
        if sibName == m.name || sibName == "main" then acc
        else acc ++ (sibSummary.functions.filter fun (name, _) =>
          sibSummary.publicNames.contains name).map fun (name, fs) =>
            (sibName ++ "_" ++ name, fs)
      ) []
      let imports := { imports with functions := imports.functions ++ siblingFns }
      let topErrs := match checkModule m summary imports with
        | .ok () => ([] : Diagnostics)
        | .error ds => ds.addContext s!"while checking module '{m.name}'"
      errs ++ topErrs ++ checkSubmodules m summary summaryTable
  ) ([] : Diagnostics)
  if allErrors.isEmpty then .ok () else .error allErrors

end Concrete
