/-!
# Design example 2: downward borrows and reborrowing

A tiny model of second-class references (VERIFIED_PIPELINE.md, "Language decisions":
references are locals and downward parameters only — never stored, never returned) with
shared and exclusive borrows, a reborrow, and an exclusive borrow passed down across a call.

* Storage has **allocation identities** (`Loc`); an owner's storage is freed when its scope
  ends or when it is explicitly released. A dereference of freed storage is `stuck` — this is a
  real memory error, not instrumentation.
* **Loans** are instrumentation (plan: Core's "instrumented mode with ghost ownership (moved
  places, loans, frames, capability use)"). Aliasing violations — a second exclusive borrow, a
  write through a shared reference, using a parent while it is reborrowed — are `stuck` only
  because the instrumented semantics says so. See design question B4 in the README.
* `check` is the static checker in miniature. It rejects storing a reference into a
  longer-lived slot (`stashRef`, the stand-in for "stored in an aggregate or returned").

A borrow here **aliases the owner's storage** (one storage identity). Today's codegen copies
the owner into a temporary and writes it back at block exit (REF-10 of docs/spec/references.md
§8); design question B3 records the equivalence obligation that choice creates.

This is a design model, not Core: it imports nothing from the compiler.
-/

namespace DesignExamples.Borrow

abbrev Var := Nat
abbrev Loc := Nat

inductive Kind where
  | shr
  | excl
  deriving Repr, DecidableEq

/-- What a reference may do right now. A reference is `frozen` while an exclusive reborrow
of it is live and `readOnly` while a shared reborrow of an exclusive reference is live. -/
inductive Use where
  | full
  | readOnly
  | frozen
  deriving Repr, DecidableEq

structure RefB where
  loc : Loc
  kind : Kind
  use : Use
  deriving Repr, DecidableEq

inductive Expr where
  | lit (n : Int)
  /-- read the owner directly. -/
  | owner (x : Var)
  /-- `*r` -/
  | deref (r : Var)
  | add (a b : Expr)
  deriving Repr, DecidableEq

inductive Stmt where
  /-- `{ let mut x = e; body }` — `x`'s storage is freed when the scope ends. -/
  | own (x : Var) (e : Expr) (body : Stmt)
  /-- `x = e;` -/
  | assign (x : Var) (e : Expr)
  /-- release (destroy/move out) the owner early: its storage is freed now. -/
  | release (x : Var)
  /-- `borrow x as r { body }` (shared) / `borrow mut x as r { body }` (exclusive). -/
  | borrow (k : Kind) (x r : Var) (body : Stmt)
  /-- a reborrow through `r`, scoped to `body`: the Core form of `&*r` / `&mut *r`. -/
  | reborrow (k : Kind) (r r2 : Var) (body : Stmt)
  /-- `*r = e;` -/
  | write (r : Var) (e : Expr)
  /-- `f(r)` where `fn f(p: &[mut] T) { callee }`. The callee sees only its parameter. -/
  | call (p : Var) (callee : Stmt) (r : Var)
  /-- `print_int(e)`: the observable event. -/
  | out (e : Expr)
  /-- a reference slot that lives longer than the borrow put into it; it exists only so
  the escape counterexample can be written. -/
  | declRef (y : Var) (body : Stmt)
  /-- `y = r;` — store a reference in a longer-lived slot (the escape). -/
  | stashRef (y r : Var)
  | seq (a b : Stmt)
  | skip
  deriving Repr, DecidableEq

def get {α} (k : Nat) : List (Nat × α) → Option α
  | [] => none
  | (j, a) :: rest => if j = k then some a else get k rest

def del {α} (k : Nat) : List (Nat × α) → List (Nat × α)
  | [] => []
  | (j, a) :: rest => if j = k then del k rest else (j, a) :: del k rest

def put {α} (k : Nat) (a : α) (l : List (Nat × α)) : List (Nat × α) := (k, a) :: del k l

/-- Restore a scoped binding to what it was before the scope opened. -/
def restore {α} (k : Nat) (old : Option α) (l : List (Nat × α)) : List (Nat × α) :=
  match old with
  | some a => put k a l
  | none => del k l

structure St where
  /-- live storage only; freed storage is absent. -/
  mem : List (Loc × Int)
  next : Loc
  /-- active loans (instrumentation). -/
  loans : List (Loc × Kind)
  owners : List (Var × Loc)
  refs : List (Var × RefB)
  outs : List Int
  deriving Repr, DecidableEq

def hasLoan (l : Loc) (loans : List (Loc × Kind)) : Bool := loans.any (·.1 == l)
def hasExclLoan (l : Loc) (loans : List (Loc × Kind)) : Bool :=
  loans.any (fun p => p.1 == l && p.2 == .excl)

def dropLoan (p : Loc × Kind) : List (Loc × Kind) → List (Loc × Kind)
  | [] => []
  | q :: rest => if q = p then rest else q :: dropLoan p rest

def evalE (s : St) : Expr → Option Int
  | .lit n => some n
  | .owner x => do
    let l ← get x s.owners
    if hasExclLoan l s.loans then none else get l s.mem
  | .deref r => do
    let b ← get r s.refs
    if b.use = .frozen then none else get b.loc s.mem
  | .add a b => do
    let x ← evalE s a
    let y ← evalE s b
    pure (x + y)

/-- Instrumented semantics; `none` is stuck. -/
def exec : Stmt → St → Option St
  | .own x e body, s => do
    let v ← evalE s e
    let l := s.next
    let old := get x s.owners
    let s1 ← exec body { s with mem := put l v s.mem, next := l + 1, owners := put x l s.owners }
    pure { s1 with mem := del l s1.mem, owners := restore x old s1.owners }
  | .assign x e, s => do
    let v ← evalE s e
    let l ← get x s.owners
    if hasLoan l s.loans then none
    else if (get l s.mem).isNone then none else pure { s with mem := put l v s.mem }
  | .release x, s => do
    let l ← get x s.owners
    -- erased behaviour: the storage is simply freed (no loan check here; see `check`)
    pure { s with mem := del l s.mem, owners := del x s.owners }
  | .borrow k x r body, s => do
    let l ← get x s.owners
    if (get l s.mem).isNone then none
    else if (match k with | .excl => hasLoan l s.loans | .shr => hasExclLoan l s.loans) then none
    else
      let old := get r s.refs
      let s1 ← exec body { s with loans := (l, k) :: s.loans,
                                  refs := put r ⟨l, k, .full⟩ s.refs }
      pure { s1 with loans := dropLoan (l, k) s1.loans, refs := restore r old s1.refs }
  | .reborrow k r r2 body, s => do
    let b ← get r s.refs
    let ok : Bool := match k with
      | .excl => b.kind == .excl && b.use == .full
      | .shr => b.use != .frozen
    if !ok then none
    else
      let parentUse : Use := match k, b.kind with
        | .excl, _ => .frozen
        | .shr, .excl => .readOnly
        | .shr, .shr => b.use
      let old2 := get r2 s.refs
      let s1 ← exec body { s with refs := put r2 ⟨b.loc, k, .full⟩ (put r { b with use := parentUse } s.refs) }
      pure { s1 with refs := restore r2 old2 (put r b s1.refs) }
  | .write r e, s => do
    let v ← evalE s e
    let b ← get r s.refs
    if b.kind != .excl || b.use != .full then none
    else if (get b.loc s.mem).isNone then none   -- dangling
    else pure { s with mem := put b.loc v s.mem }
  | .call p callee r, s => do
    let b ← get r s.refs
    if b.use = .frozen then none
    else
      -- an implicit reborrow: the callee gets a fresh reference to the same place; the
      -- caller's environment is invisible to it and is restored on return
      let k : Kind := if b.use = .readOnly then .shr else b.kind
      let s1 ← exec callee { s with owners := [], refs := [(p, ⟨b.loc, k, .full⟩)] }
      pure { s1 with owners := s.owners, refs := s.refs }
  | .out e, s => do
    let v ← evalE s e
    pure { s with outs := s.outs ++ [v] }
  | .declRef y body, s => do
    let old := get y s.refs
    let s1 ← exec body { s with refs := del y s.refs }
    pure { s1 with refs := restore y old s1.refs }
  | .stashRef y r, s => do
    let b ← get r s.refs
    pure { s with refs := put y b s.refs }
  | .seq a b, s => do
    let s1 ← exec a s
    exec b s1
  | .skip, s => pure s

def init : St := { mem := [], next := 0, loans := [], owners := [], refs := [], outs := [] }

/-- The observable result: the `print_int` events, or `none` if stuck. -/
def run (p : Stmt) : Option (List Int) := (exec p init).map (·.outs)

/-! ## The checker -/

/-- Static environment: owners in scope, static loans, references with kind and use. -/
structure Env where
  owners : List Var
  loans : List (Var × Kind)
  refs : List (Var × (Kind × Use))
  deriving Repr, DecidableEq

def sHasLoan (x : Var) (e : Env) : Bool := e.loans.any (·.1 == x)
def sHasExcl (x : Var) (e : Env) : Bool := e.loans.any (fun p => p.1 == x && p.2 == .excl)

def checkE (e : Env) : Expr → Bool
  | .lit _ => true
  | .owner x => e.owners.contains x && !sHasExcl x e
  | .deref r => match get r e.refs with
    | some (_, u) => u != .frozen
    | none => false
  | .add a b => checkE e a && checkE e b

/-- Which rules the checker enforces. `strict` is the model's checker; each permissive variant
drops exactly one rule, so a program it accepts shows what that one rule prevents. -/
structure Rules where
  /-- allow storing a reference into a longer-lived slot (drops the second-class rule) -/
  stash : Bool := false
  /-- allow releasing an owner while it is borrowed (drops the S8 liveness rule) -/
  releaseBorrowed : Bool := false
  deriving Repr, DecidableEq

def strict : Rules := {}

/-- Returns the environment after the statement, or `none` when rejected. -/
def check (R : Rules) : Stmt → Env → Option Env
  | .own x e body, env =>
    if !checkE env e || env.owners.contains x then none
    else do
      let env1 ← check R body { env with owners := x :: env.owners }
      pure { env1 with owners := env1.owners.erase x }
  | .assign x e, env =>
    if checkE env e && env.owners.contains x && !sHasLoan x env then some env else none
  | .release x, env =>
    -- an owner may not be destroyed or moved while a reference to it is live (S8)
    if env.owners.contains x && (R.releaseBorrowed || !sHasLoan x env) then
      some { env with owners := env.owners.erase x }
    else none
  | .borrow k x r body, env =>
    let conflict := match k with | .excl => sHasLoan x env | .shr => sHasExcl x env
    if !env.owners.contains x || conflict || (get r env.refs).isSome then none
    else do
      let env1 ← check R body { env with loans := (x, k) :: env.loans,
                                         refs := put r (k, .full) env.refs }
      pure { env1 with loans := env1.loans.erase (x, k), refs := del r env1.refs }
  | .reborrow k r r2 body, env =>
    match get r env.refs with
    | none => none
    | some (rk, u) =>
      let ok : Bool := match k with
        | .excl => rk == .excl && u == .full
        | .shr => u != .frozen
      if !ok || (get r2 env.refs).isSome then none
      else
        let pu : Use := match k, rk with
          | .excl, _ => .frozen
          | .shr, .excl => .readOnly
          | .shr, .shr => u
        do
          let env1 ← check R body
            { env with refs := put r2 (k, .full) (put r (rk, pu) env.refs) }
          pure { env1 with refs := del r2 (put r (rk, u) env1.refs) }
  | .write r e, env =>
    match get r env.refs with
    | some (.excl, .full) => if checkE env e then some env else none
    | _ => none
  | .call p callee r, env =>
    match get r env.refs with
    | none => none
    | some (rk, u) =>
      if u = .frozen then none
      else
        let k : Kind := if u = .readOnly then .shr else rk
        -- the callee is checked on its own: it sees only its parameter
        match check R callee { owners := [], loans := [], refs := [(p, (k, .full))] } with
        | some _ => some env
        | none => none
  | .out e, env => if checkE env e then some env else none
  | .declRef y body, env =>
    -- the slot starts empty, as in `exec`; it may not shadow a live reference
    if (get y env.refs).isSome then none
    else do
      let env1 ← check R body env
      pure { env1 with refs := del y env1.refs }
  | .stashRef y r, env =>
    -- second-class: a reference is never stored or returned
    if R.stash then
      match get r env.refs with
      | some b => some { env with refs := put y b env.refs }
      | none => none
    else none
  | .seq a b, env => do
    let env1 ← check R a env
    check R b env1
  | .skip, env => some env

def checkWith (R : Rules) (p : Stmt) : Bool :=
  (check R p { owners := [], loans := [], refs := [] }).isSome

def checkProg (p : Stmt) : Bool := checkWith strict p

/-! ## Accepted: reborrowing, downward exclusive borrow across calls, shared borrows -/

open Stmt Expr

/-- `fn inc(p: &mut i64) { *p = *p + 1; }` -/
def incBody : Stmt := write 9 (add (deref 9) (lit 1))

/-- `fn show(p: &i64) { print_int(*p); }` -/
def showBody : Stmt := out (deref 9)

/--
```
let mut x = 1;
borrow mut x as r {
  *r = 5;
  reborrow mut r as r2 {      // &mut *r: r is frozen while r2 is live
    *r2 = *r2 + 2;            // 7
    inc(r2);                  // downward exclusive borrow across a call: 8
  }
  *r = *r + 1;                // r is usable again: 9
  inc(r);                     // passing the block reference: implicit reborrow, 10
  inc(r);                     // ... and again — it was not consumed: 11
  reborrow r as s {           // shared reborrow of an exclusive reference: r is read-only
    show(s);                  // prints 11
    print_int(*r + *s);       // reading the parent is allowed: 22
  }
}
print_int(x);                 // 11
``` -/
def reborrowEx : Stmt :=
  own 0 (lit 1) <|
  seq
    (borrow .excl 0 1 <|
      seq (write 1 (lit 5)) <|
      seq (reborrow .excl 1 2 <|
             seq (write 2 (add (deref 2) (lit 2))) (call 9 incBody 2)) <|
      seq (write 1 (add (deref 1) (lit 1))) <|
      seq (call 9 incBody 1) <|
      seq (call 9 incBody 1) <|
      reborrow .shr 1 3 (seq (call 9 showBody 3) (out (add (deref 1) (deref 3)))))
    (out (owner 0))

theorem reborrowEx_accepted : checkProg reborrowEx = true := by decide
theorem reborrowEx_runs : run reborrowEx = some [11, 22, 11] := by decide

/--
```
let mut x = 4;
borrow x as a {
  borrow x as b { print_int(*a + *b + x); }   // two shared borrows and an owner read coexist
  show(a);
}
x = 6;                                       // the loans have ended
print_int(x);
``` -/
def sharedEx : Stmt :=
  own 0 (lit 4) <|
  seq (borrow .shr 0 1 <|
        seq (borrow .shr 0 2 (out (add (add (deref 1) (deref 2)) (owner 0))))
            (call 9 showBody 1)) <|
  seq (assign 0 (lit 6)) (out (owner 0))

theorem sharedEx_accepted : checkProg sharedEx = true := by decide
theorem sharedEx_runs : run sharedEx = some [12, 4, 6] := by decide

/-! ## Rejected: escape, owner death while borrowed, aliasing violations -/

/-- The escape: a reference stored in a slot that outlives its owner.
```
let y: &i64;                         // longer-lived slot
{ let x = 1; borrow x as r { y = r; } }   // x's storage dies here
print_int(*y);                       // dangling
``` -/
def escapeEx : Stmt :=
  declRef 5 <| seq (own 0 (lit 1) (borrow .shr 0 1 (stashRef 5 1))) (out (deref 5))

theorem escapeEx_rejected : checkProg escapeEx = false := by decide
/-- A genuine memory error: the dereference reads freed storage. -/
theorem escapeEx_dangles : run escapeEx = none := by decide
/-- Rule isolation: dropping only the second-class rule, the checker accepts the escape — so
that rule alone is what rejects it. -/
theorem escapeEx_needs_second_class : checkWith { stash := true } escapeEx = true := by decide

/-- A callee cannot store its parameter either. This is the blanket storage rule: the model has
no slot that outlives a call (no globals, and the caller's environment is invisible to the
callee), which is why second-class references make escape across a call impossible rather
than merely checked. Even the permissive variant's stored copy dies with the callee.
`fn keep(p: &i64) { let slot: &i64; slot = p; }` -/
def storeParamInCallee : Stmt :=
  own 0 (lit 1) <| borrow .shr 0 1 (call 9 (declRef 5 (stashRef 5 9)) 1)

theorem storeParamInCallee_rejected : checkProg storeParamInCallee = false := by decide
theorem storeParamInCallee_harmless : run storeParamInCallee = some [] := by decide

/-- Destroying the owner while a reference to it is live: `borrow x as r { destroy(x); *r }`. -/
def releaseWhileBorrowed : Stmt :=
  own 0 (lit 1) <| borrow .shr 0 1 (seq (release 0) (out (deref 1)))

theorem releaseWhileBorrowed_rejected : checkProg releaseWhileBorrowed = false := by decide
theorem releaseWhileBorrowed_dangles : run releaseWhileBorrowed = none := by decide
/-- Rule isolation: dropping only the owner-liveness rule, the checker accepts it. -/
theorem releaseWhileBorrowed_needs_liveness :
    checkWith { releaseBorrowed := true } releaseWhileBorrowed = true := by decide

/-- A reference slot may not shadow a live reference: the slot starts empty in `exec`, so
accepting this would let the checker approve a stuck program (Codex round 1). -/
def slotShadowsRef : Stmt :=
  own 0 (lit 1) <| borrow .shr 0 1 (declRef 1 (out (deref 1)))

theorem slotShadowsRef_rejected : checkProg slotShadowsRef = false := by decide
theorem slotShadowsRef_stuck : run slotShadowsRef = none := by decide

/-- Using the parent while an exclusive reborrow of it is live. -/
def parentDuringReborrow : Stmt :=
  own 0 (lit 1) <| borrow .excl 0 1 (reborrow .excl 1 2 (write 1 (lit 3)))

theorem parentDuringReborrow_rejected : checkProg parentDuringReborrow = false := by decide
theorem parentDuringReborrow_stuck : run parentDuringReborrow = none := by decide

/-- Two live exclusive borrows of one owner. -/
def twoExclusive : Stmt :=
  own 0 (lit 1) <| borrow .excl 0 1 (borrow .excl 0 2 (write 2 (lit 3)))

theorem twoExclusive_rejected : checkProg twoExclusive = false := by decide
theorem twoExclusive_stuck : run twoExclusive = none := by decide

/-- Reading the owner while it is exclusively borrowed (frozen owner). -/
def ownerReadWhileExcl : Stmt :=
  own 0 (lit 1) <| borrow .excl 0 1 (out (owner 0))

theorem ownerReadWhileExcl_rejected : checkProg ownerReadWhileExcl = false := by decide
theorem ownerReadWhileExcl_stuck : run ownerReadWhileExcl = none := by decide

/-- Writing through a shared reference, and writing through a shared reborrow's parent. -/
def writeThroughShared : Stmt :=
  own 0 (lit 1) <| borrow .shr 0 1 (write 1 (lit 3))

def writeParentDuringSharedReborrow : Stmt :=
  own 0 (lit 1) <| borrow .excl 0 1 (reborrow .shr 1 2 (write 1 (lit 3)))

theorem writeThroughShared_rejected : checkProg writeThroughShared = false := by decide
theorem writeThroughShared_stuck : run writeThroughShared = none := by decide
theorem writeParentDuringSharedReborrow_rejected :
    checkProg writeParentDuringSharedReborrow = false := by decide
theorem writeParentDuringSharedReborrow_stuck :
    run writeParentDuringSharedReborrow = none := by decide

end DesignExamples.Borrow
