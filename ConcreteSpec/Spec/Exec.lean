import ConcreteSpec.Spec.Scalar

/-!
# Execution, calls and cleanup

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C1 (the foundation proposal approved by the
owner on 2026-11-08, "ok all recommended", question 1: the interfaces and their laws are written,
proved and tested before the freeze).

This module is the shape of every Core execution: a machine configuration, one labeled step, and
the laws that sequencing, scopes, exits, calls and cleanup obey. It is *parametric* in the operand
language (`Operands`): what a value is, how an operand is evaluated, which types exist, what the
shared state is (memory, C4; the loan and ownership instrumentation, C5) and which primitive
operations read and change it or talk to the environment. The scalar operations (C2), memory (C4)
and the environment (C6) are instances; they add operations, never change what the statements below
mean (the freeze boundary: an extension preserves the old fragment's executions).

**The configuration.** A `Config` holds the current control (a statement to run, a statement just
completed, an exit in progress, or a trap), the locals, the shared state, and the stack of
continuation frames that says where execution resumes. Call frames are frames of the same stack. A
call saves its caller's locals and restores them on return; the shared state is *not* saved, so
what a callee (or a cleanup call) does to memory is seen after it returns.

**Places and moves.** Writing a place other than a local (a field, an element, memory behind a
pointer) is a primitive operation of the instance: its operands, the place's index operands and the
value, are evaluated before it runs, and in A-normal form elaboration has already evaluated the
right-hand side before any destination subexpression. Core's operands are explicit `copy x` and
`move x` (Q9.1): which a type allows, and that a moved value is used exactly once, is the ownership
instrumentation (C5), which is part of the instance's state; a move has no other run-time effect.

**Evaluation order** (C1, STMT-4/5). Core is in A-normal form (Q9.1): an operand has no run-time
effects, so after erasure (C9) the only order that can be observed among operands is which failure
is reported first; before erasure, the instrumentation also sees the left-to-right order (a later
operand sees what an earlier one moved). Arguments
are evaluated left to right, and the first that traps decides the trap (`evalArgs`); every argument
is evaluated before the callee is entered. An assignment evaluates its right-hand side before
storing it. Source-level orders (STMT-4: receiver before arguments, left operand before right, a
base before its index, an assignment's right-hand side before its destination's subexpressions,
fields and elements in written order, `&&` and `||` evaluating their right operand only when needed) are
elaboration's obligation: it lowers them to this sequential Core, and the checker checks both
operands of a short-circuit operator (Q6.5). That belongs to the elaboration slices (README O1).

**Calls.** A call names its callee by typed identity (`FnId`, never a string), evaluates its
arguments, and enters the callee with its parameters bound in order. There is no receiver: what a
source method calls its receiver is the callee's ordinary, typed first parameter (owner decision
2026-11-08, item 7). Destruction is one consuming protocol (`DestroyProtocol`): for a type with
`Destroy`, exactly one function, taking that type's value as its only parameter and returning
`Unit` (OD60, Q2.7).

**Exits and cleanup** (Q9.4, Q9.8). `break L` and `continue L` name their loop (Q9.3); `return`
evaluates its value *before* leaving any scope. On every structured exit (falling off the end of a
block, `break`, `continue`, `return`) the deferred calls of each scope the exit leaves run, the
innermost scope first and, within a scope, the newest registration first; their operands are read
when they run (late reads, Q9.8). A deferred call is an ordinary call: if it traps, the run stops at
once and no remaining cleanup runs. An explicit `abort()` is terminal: it runs no deferred call and
does not unwind. Ordinary locals get no implicit destructor: cleanup happens only through what a
scope registered (Q2.1 keeps release, destruction and resource cleanup apart).

**The loop** (Q9.3) is `while L { C } test t do { B }`: the condition region `C` runs, then the test
`t`; if it holds, the body runs and the loop starts again from `C`; `continue L` also starts again
from `C`. A counted loop is lowered to this form by elaboration, with its bound snapshot taken
before the loop and its counter advanced in `C` (slice 4). Exits inside a condition region are
restricted by the checker (Q9.3); the machine passes such an exit through the loop's test frame
without matching it. The temporaries the condition region binds (Q9.3's `t1 = next(...)`,
`t2 = t1 != 0`, test `t2`) are visible to the test and are dropped right after it, so every
iteration, the body and the code after the loop see only the locals the loop started with.

The laws are in `Proofs/Exec.lean`: sequencing, scope restoration (`lookup_restrict`), that an exit
carries its value unchanged through cleanup, that the deferred calls run in the stated order
(`exit_scope_runs_defers`, `unwind_runs_plan`), that a trap is terminal and that `abort()` runs no
cleanup.

`run` is the fuel-indexed driver of this step relation, the skeleton of the reference evaluator
(Q9.9); its adequacy (C8) is stated against `Step`.
-/

namespace ConcreteSpec.Core.Exec
open ConcreteSpec.Core

/-- How a primitive operation was answered: the event the environment saw, if any, the result, and
the new shared state. -/
structure Answer (Val Event Store : Type) where
  event : Option Event
  result : Except TrapKind Val
  store : Store

/-- A change in the life of locals, which an instrumentation may track (C5): a local is initialized
(by `bind`, by `assign`, or as the result of a call or a primitive operation), an activation starts
with its parameters initialized, the current activation ends, or locals go out of scope. The
machine reports each change to the instance (`Operands.life`), so ownership state can be kept per
activation and a reinitialized local can be told apart from a moved one. -/
inductive Life where
  | bind (x : Var)
  | enter (params : List Var)
  | leave
  | drop (xs : List Var)

/-- What an operand language supplies to the machine. An instance is a lawful extension: the
statements keep their meaning whatever the instance.

- `Val`, `Expr`: values and operands. `eval` evaluates an operand against the locals and the
  shared state: a value and the state after it, a trap, or unsafe stuckness (`Res`). An operand
  has no run-time effect (Core is in A-normal form, Q9.1); the state it returns may differ only in
  instrumentation, such as the ownership state C5 adds, where `move x` marks `x` moved. Erasure
  (C9) removes the instrumentation and must show the erased runs agree.
- `Store`: the shared state, threaded through operands, calls and cleanup.
- `life`: how a change in the life of locals (`Life`) changes the state. Like the state an
  operand returns, it may change only instrumentation; an instance without instrumentation
  ignores it by returning `some st`. `none` refuses the event: execution is stuck, including
  cleanup steps (owner decision 2026-11-08, 15:08 UTC, item 21).
- `test`: the truth value of a condition (`none`: not a condition; unsafe stuckness).
- `Ty`, `unitTy`, `unitVal`, `isCopy`: the types, `Unit` and its value, and which types are `Copy`
  (a declared and checked instance; the checker owns that check, C9).
- `Prim`, `allows`: the primitive operations and their contracts. `allows p args st a` says that a
  request `p args` in state `st` may be answered by `a`: an event the environment sees (or none,
  for an internal operation such as a memory access), a result (a value or a trap) and the new
  state. An answer outside the contract is never taken (C6): `Step` has no transition for it, so
  an operation with no allowed answer has none. The driver `runFrom` reports an oracle that gives
  no answer (`noAnswer`) apart from one whose answer is refused (`badAnswer`). -/
structure Operands where
  Val : Type
  Expr : Type
  Ty : Type
  Store : Type
  Prim : Type
  Event : Type
  unitVal : Val
  unitTy : Ty
  isCopy : Ty → Bool
  eval : AList Var Val → Store → Expr → Res (Val × Store)
  life : Life → Store → Option Store
  test : Val → Option Bool
  allows : Prim → List Val → Store → Answer Val Event Store → Bool

variable {O : Operands} {V : Type}

/-- A Core statement. Every operation that can fail carries its site (for its trap and its
diagnostics). Locals are fresh identities (Q9.1): `bind` introduces a new one, `assign` changes an
existing one. -/
inductive Stmt (O : Operands) where
  /-- Do nothing. -/
  | skip
  /-- Bind a fresh local to an operand's value. -/
  | bind (x : Var) (e : O.Expr) (site : SiteId)
  /-- Store an operand's value in an existing local (the right-hand side first). -/
  | assign (x : Var) (e : O.Expr) (site : SiteId)
  /-- Run `s`, then `t` if `s` completed normally. -/
  | seq (s t : Stmt O)
  /-- A block: a scope for the locals bound in it and the calls deferred in it. -/
  | block (b : BlockId) (s : Stmt O)
  /-- Register a cleanup call in the innermost block; its operands are read when it runs. -/
  | defer (f : FnId) (args : List O.Expr) (site : SiteId)
  /-- Choose a branch by a condition. -/
  | cond (c : O.Expr) (s t : Stmt O) (site : SiteId)
  /-- The one loop form (Q9.3): condition region `c`, test `test`, body `body`. The region's
  locals are visible to the test, then dropped. -/
  | loop (l : LoopId) (c : Stmt O) (test : O.Expr) (body : Stmt O) (site : SiteId)
  /-- Leave loop `l`. -/
  | brk (l : LoopId)
  /-- Start loop `l` again from its condition region. -/
  | cont (l : LoopId)
  /-- Return an operand's value, evaluated before any scope is left. -/
  | ret (e : O.Expr) (site : SiteId)
  /-- Call `f`; bind the result to `dst`, or drop it (a `Unit` call). -/
  | call (dst : Option Var) (f : FnId) (args : List O.Expr) (site : SiteId)
  /-- A primitive operation of the instance (an environment request, or an internal operation such
  as a memory access); bind its result to `dst`, or drop it. -/
  | prim (dst : Option Var) (p : O.Prim) (args : List O.Expr) (site : SiteId)
  /-- The program's own `abort()`: terminal, no cleanup. -/
  | abort (site : SiteId)

/-- A registered cleanup call: the function and its operands, read when it runs. -/
structure Deferred (O : Operands) where
  f : FnId
  args : List O.Expr
  site : SiteId

/-- The statement a deferred call runs: an ordinary call whose `Unit` result is dropped. -/
def Deferred.stmt (d : Deferred O) : Stmt O := .call none d.f d.args d.site

/-- A structured exit in progress. -/
inductive Exit (V : Type) where
  | brk (l : LoopId)
  | cont (l : LoopId)
  | ret (v : V)
  deriving DecidableEq, Repr

/-- What continues after a cleanup call: the normal end of the block, or an exit. -/
inductive Pending (V : Type) where
  | fall
  | exit (e : Exit V)
  deriving DecidableEq, Repr

/-- A continuation frame: where execution resumes.

- `seq t`: then run `t`.
- `scope b dom defers`: the scope of block `b`. `dom` are the locals visible when it was entered,
  `defers` its registered cleanup calls, newest first.
- `test l c t body site dom`: loop `l`'s condition region is running; then evaluate its test and
  drop the region's locals, keeping those in `dom`, the locals the loop started with.
- `body l c t body site`: loop `l`'s body is running; then start the loop again.
- `resume p`: a cleanup call is running; then continue with `p`.
- `ret dst caller`: a call is running; on its return, restore the caller's locals and bind the
  result to `dst`. -/
inductive Frame (O : Operands) where
  | seq (t : Stmt O)
  | scope (b : BlockId) (dom : List Var) (defers : List (Deferred O))
  | test (l : LoopId) (c : Stmt O) (t : O.Expr) (body : Stmt O) (site : SiteId) (dom : List Var)
  | body (l : LoopId) (c : Stmt O) (t : O.Expr) (body : Stmt O) (site : SiteId)
  | resume (p : Pending O.Val)
  | ret (dst : Option Var) (caller : AList Var O.Val)

/-- What the machine is doing. -/
inductive Ctrl (O : Operands) where
  /-- Run a statement. -/
  | exec (s : Stmt O)
  /-- The current statement completed normally. -/
  | done
  /-- An exit is unwinding the frames. -/
  | exiting (e : Exit O.Val)
  /-- The run trapped: terminal. -/
  | trapped (k : TrapKind) (site : SiteId)

/-- A machine configuration. -/
structure Config (O : Operands) where
  ctrl : Ctrl O
  locals : AList Var O.Val
  store : O.Store
  frames : List (Frame O)

/-- A function: ordinary typed parameters (the first is not special), a result type, the block of
its body, and the body. -/
structure Fn (O : Operands) where
  params : List (Var × O.Ty)
  result : O.Ty
  block : BlockId
  body : Stmt O

/-- A program: its functions, by typed identity. -/
structure Program (O : Operands) where
  fns : AList FnId (Fn O)

/-- How a finished run ended: it completed, it returned a value, or it trapped. -/
inductive Final (V : Type) where
  | normal
  | returned (v : V)
  | trapped (k : TrapKind) (site : SiteId)
  deriving DecidableEq, Repr

/-- The configurations a run ends in. A trap is final whatever frames remain: they are not
unwound. -/
def Config.final? : Config O → Option (Final O.Val)
  | ⟨.done, _, _, []⟩ => some .normal
  | ⟨.exiting (.ret v), _, _, []⟩ => some (.returned v)
  | ⟨.trapped k site, _, _, _⟩ => some (.trapped k site)
  | _ => none

/-- The locals of `L` whose identity is in `dom`: what a scope restores when it is left. -/
def restrict (dom : List Var) (L : AList Var V) : AList Var V :=
  L.filter (fun e => decide (e.1 ∈ dom))

/-- The identities bound in `L`. -/
def dom (L : AList Var V) : List Var := List.map Prod.fst L

/-- Bind a call's or a primitive operation's result to `dst`, a fresh local of the current scope
(as `bind` does), or drop it. -/
def bindDst (dst : Option Var) (v : V) (L : AList Var V) : AList Var V :=
  match dst with
  | none => L
  | some x => (x, v) :: L

/-- The state after `bindDst`: the instrumentation hears of the new local, if there is one. -/
def bindLife (O : Operands) (dst : Option Var) (st : O.Store) : Option O.Store :=
  match dst with
  | none => some st
  | some x => O.life (.bind x) st

/-- The locals of `L` that leaving a scope entered with `dm` drops. -/
def dead (dm : List Var) (L : AList Var V) : List Var := (dom L).filter (fun x => decide (x ∉ dm))

/-- The state after leaving a scope entered with `dm`: the instrumentation hears which locals
went out of scope. -/
def dropOut (O : Operands) (dm : List Var) (L : AList Var O.Val) (st : O.Store) : Option O.Store :=
  O.life (.drop (dead dm L)) st

/-- Evaluate arguments left to right, each in the state the one before it left; the first that
traps (or is stuck) decides. -/
def evalArgs (O : Operands) (L : AList Var O.Val) :
    O.Store → List O.Expr → Res (List O.Val × O.Store)
  | st, [] => .ok ([], st)
  | st, e :: es => (O.eval L st e).bind fun (v, st₁) =>
      (evalArgs O L st₁ es).map fun (vs, st₂) => (v :: vs, st₂)

/-- Register a cleanup call in the innermost scope, through the statements that follow it in that
block. A `defer` anywhere else (directly in a loop's condition region, say) has no meaning; the
checker refuses it. -/
def register (d : Deferred O) : List (Frame O) → Option (List (Frame O))
  | .seq t :: k => (register d k).map (.seq t :: ·)
  | .scope b dom ds :: k => some (.scope b dom (d :: ds) :: k)
  | _ => none

/-- Request continuations are partial: `none` refuses result binding through lifecycle
instrumentation. This is unsafe stuckness, not a different primitive answer or a trap;
`Step.prim` requires success, and `runFrom` reports the requesting configuration as stuck
without accepting the response event when continuation fails.

What one step does: move to a configuration, or make a request of a primitive operation (its
arguments, its site, the state it is made in, and the configuration each answer leads to). -/
inductive Out (O : Operands) where
  | next (c : Config O)
  | request (p : O.Prim) (args : List O.Val) (site : SiteId) (st : O.Store)
      (k : Answer O.Val O.Event O.Store → Option (Config O))

/-- Continue with an operand's value and the state after it; a trap stops at `site` in the state
before the operand (a trap is terminal, so nothing reads that state's instrumentation again),
stuckness has no step. -/
def onRes (r : Res (V × O.Store)) (site : SiteId) (L : AList Var O.Val) (st : O.Store)
    (k : List (Frame O)) (f : V → O.Store → Option (Out O)) : Option (Out O) :=
  match r with
  | .ok (v, st') => f v st'
  | .trap kd => some (.next ⟨.trapped kd site, L, st, k⟩)
  | .stuck => none

/-- Start the next cleanup call of a scope, then continue with `p`. -/
def startCleanup (L : AList Var O.Val) (st : O.Store) (b : BlockId) (dm : List Var)
    (d : Deferred O) (ds : List (Deferred O)) (p : Pending O.Val) (k : List (Frame O)) : Out O :=
  .next ⟨.exec d.stmt, L, st, .resume p :: .scope b dm ds :: k⟩

/-- One step of an exit `e` unwinding frames `k` with locals `L`: the frame decides, and only a
loop's body frame and a call frame look at the exit itself. -/
def stepExit (L : AList Var O.Val) (st : O.Store) (e : Exit O.Val) :
    List (Frame O) → Option (Out O)
  | .seq _ :: k => some (.next ⟨.exiting e, L, st, k⟩)
  | .scope b dm (d :: ds) :: k => some (startCleanup L st b dm d ds (.exit e) k)
  | .scope _ dm [] :: k => (dropOut O dm L st).map fun st' => .next ⟨.exiting e, restrict dm L, st', k⟩
  | .test _ _ _ _ _ dm :: k => (dropOut O dm L st).map fun st' => .next ⟨.exiting e, restrict dm L, st', k⟩
  | .body l cr t b site :: k => match e with
    | .brk l' =>
      if l' = l then some (.next ⟨.done, L, st, k⟩) else some (.next ⟨.exiting e, L, st, k⟩)
    | .cont l' =>
      if l' = l then some (.next ⟨.exec (.loop l cr t b site), L, st, k⟩)
      else some (.next ⟨.exiting e, L, st, k⟩)
    | .ret _ => some (.next ⟨.exiting e, L, st, k⟩)
  | .ret dst caller :: k => match e with
    | .ret v => (O.life .leave st).bind fun st' =>
        (bindLife O dst st').map fun st'' => .next ⟨.done, bindDst dst v caller, st'', k⟩
    | _ => none
  | .resume _ :: _ => none
  | [] => none

/-- One step of the machine. `none`: the configuration is final, or no rule applies (unsafe
stuckness). -/
def step (P : Program O) (c : Config O) : Option (Out O) :=
  let L := c.locals
  let st := c.store
  match c.ctrl, c.frames with
  -- running a statement
  | .exec .skip, k => some (.next ⟨.done, L, st, k⟩)
  | .exec (.bind x e site), k => onRes (O.eval L st e) site L st k fun v st' =>
      (O.life (.bind x) st').map fun st'' => .next ⟨.done, (x, v) :: L, st'', k⟩
  | .exec (.assign x e site), k => onRes (O.eval L st e) site L st k fun v st' =>
      (AList.lookup L x).bind fun _ =>
        (O.life (.bind x) st').map fun st'' =>
          .next ⟨.done, AList.update L x (fun _ => v), st'', k⟩
  | .exec (.seq s t), k => some (.next ⟨.exec s, L, st, .seq t :: k⟩)
  | .exec (.block b s), k => some (.next ⟨.exec s, L, st, .scope b (dom L) [] :: k⟩)
  | .exec (.defer f args site), k =>
      (register ⟨f, args, site⟩ k).map fun k' => .next ⟨.done, L, st, k'⟩
  | .exec (.cond e s t site), k => onRes (O.eval L st e) site L st k fun v st' =>
      (O.test v).map fun b => .next ⟨.exec (if b then s else t), L, st', k⟩
  | .exec (.loop l cr t b site), k =>
      some (.next ⟨.exec cr, L, st, .test l cr t b site (dom L) :: k⟩)
  | .exec (.brk l), k => some (.next ⟨.exiting (.brk l), L, st, k⟩)
  | .exec (.cont l), k => some (.next ⟨.exiting (.cont l), L, st, k⟩)
  | .exec (.ret e site), k => onRes (O.eval L st e) site L st k fun v st' =>
      some (.next ⟨.exiting (.ret v), L, st', k⟩)
  | .exec (.call dst f args site), k => onRes (evalArgs O L st args) site L st k fun vs st' =>
      (AList.lookup P.fns f).bind fun fn =>
        if fn.params.length = vs.length then
          (O.life (.enter (fn.params.map Prod.fst)) st').map fun st'' =>
            .next ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, st'',
              .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst L :: k⟩
        else none
  | .exec (.prim x p args site), k => onRes (evalArgs O L st args) site L st k fun vs st' =>
      some (.request p vs site st' fun a => match a.result with
        | .ok v => (bindLife O x a.store).map fun st' => ⟨.done, bindDst x v L, st', k⟩
        | .error kd => some ⟨.trapped kd site, L, a.store, k⟩)
  | .exec (.abort site), k => some (.next ⟨.trapped .abort site, L, st, k⟩)
  -- a statement completed normally
  | .done, .seq t :: k => some (.next ⟨.exec t, L, st, k⟩)
  | .done, .scope b dm (d :: ds) :: k => some (startCleanup L st b dm d ds .fall k)
  | .done, .scope _ dm [] :: k => (dropOut O dm L st).map fun st' => .next ⟨.done, restrict dm L, st', k⟩
  | .done, .test l cr t b site dm :: k => onRes (O.eval L st t) site L st k fun v st' =>
      (O.test v).bind fun go =>
        (dropOut O dm L st').map fun st'' =>
          if go then .next ⟨.exec b, restrict dm L, st'', .body l cr t b site :: k⟩
          else .next ⟨.done, restrict dm L, st'', k⟩
  | .done, .body l cr t b site :: k => some (.next ⟨.exec (.loop l cr t b site), L, st, k⟩)
  | .done, .resume .fall :: k => some (.next ⟨.done, L, st, k⟩)
  | .done, .resume (.exit e) :: k => some (.next ⟨.exiting e, L, st, k⟩)
  -- a function body that ends without `return`: only a `Unit` function may (the checker refuses
  -- any other, C9), and its result is `()`
  | .done, .ret dst caller :: k =>
      (O.life .leave st).bind fun st' =>
        (bindLife O dst st').map fun st'' => .next ⟨.done, bindDst dst O.unitVal caller, st'', k⟩
  | .done, [] => none
  -- an exit unwinding
  | .exiting e, k => stepExit L st e k
  -- a trap is terminal
  | .trapped .., _ => none

/-- A step's label: silent, or an event the environment saw. -/
inductive Label (O : Operands) where
  | silent
  | emit (ev : O.Event)

/-- The label of an answer: its event, if it has one. -/
def Answer.label (a : Answer O.Val O.Event O.Store) : Label O :=
  match a.event with
  | none => .silent
  | some ev => .emit ev

/-- The events a label contributes to a trace. -/
def Label.events : Label O → List O.Event
  | .silent => []
  | .emit ev => [ev]

/-- The labeled single-step relation: a step of the machine itself (silent), or a primitive
operation answered within its contract (labeled with the answer's event, if any). -/
inductive Step (P : Program O) : Config O → Label O → Config O → Prop where
  | silent {c c'} : step P c = some (.next c') → Step P c .silent c'
  | prim {c c' p vs site st k a} : step P c = some (.request p vs site st k) →
      O.allows p vs st a = true → k a = some c' → Step P c a.label c'

/-- Many steps, with the events they emitted in order. -/
inductive Steps (P : Program O) : Config O → List O.Event → Config O → Prop where
  | refl (c : Config O) : Steps P c [] c
  | step {c l c' tr c''} : Step P c l c' → Steps P c' tr c'' → Steps P c (l.events ++ tr) c''

/-- The configuration that runs function `fn` on argument values `vs`, in state `st`, as the whole
program: its `return` ends the run with the returned value (`Final.returned`), and a body that ends
without one ends it normally (`Final.normal`). How either becomes the process's exit is C6. -/
def enter (fn : Fn O) (vs : List O.Val) (st : O.Store) : Option (Config O) :=
  (O.life (.enter (fn.params.map Prod.fst)) st).map fun st' =>
    ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, st',
      [.scope fn.block (fn.params.map Prod.fst) []]⟩

/-! ## Cleanup plans

What an exit will run, read from the frames: the frames an exit passes on its way to the frame
that receives it, and the deferred calls of their scopes in the order they run. -/

/-- The frames exit `e` passes through without being received: the rest of a block, a scope, a
loop's condition region, and the body of another loop (or any loop, for a `return`). -/
def Passes (e : Exit O.Val) : Frame O → Prop
  | .seq _ => True
  | .scope .. => True
  | .test .. => True
  | .body l .. => match e with
    | .brk l' => l' ≠ l
    | .cont l' => l' ≠ l
    | .ret _ => True
  | .resume _ => False
  | .ret .. => False

/-- The deferred calls frames hold, in the order an exit through them runs them: the innermost
scope first, and within a scope the newest registration first. -/
def plan : List (Frame O) → List (Deferred O)
  | [] => []
  | .scope _ _ ds :: k => ds ++ plan k
  | _ :: k => plan k

/-- The locals left after an exit passes these frames: each scope restores its own. -/
def unwindLocals : List (Frame O) → AList Var O.Val → AList Var O.Val
  | [], L => L
  | .scope _ dm _ :: k, L => unwindLocals k (restrict dm L)
  | .test _ _ _ _ _ dm :: k, L => unwindLocals k (restrict dm L)
  | _ :: k, L => unwindLocals k L

/-- A cleanup call that completes: from locals `L` and state `st`, on any frames, it runs to its
normal end with events `tr` and state `st'`, and gives the locals back unchanged (a call restores
its caller's locals). -/
def Completes (P : Program O) (L : AList Var O.Val) (d : Deferred O) (st : O.Store)
    (tr : List O.Event) (st' : O.Store) : Prop :=
  ∀ k, Steps P ⟨.exec d.stmt, L, st, k⟩ tr ⟨.done, L, st', k⟩

/-- The calls `ds` run one after another from locals `L`, each completing, the state threaded from
each to the next: in list order, with the events of each in turn. -/
inductive RunsAll (P : Program O) (L : AList Var O.Val) :
    List (Deferred O) → O.Store → List O.Event → O.Store → Prop where
  | nil (st : O.Store) : RunsAll P L [] st [] st
  | cons {d ds st tr₁ st₁ tr₂ st₂} : Completes P L d st tr₁ st₁ → RunsAll P L ds st₁ tr₂ st₂ →
      RunsAll P L (d :: ds) st (tr₁ ++ tr₂) st₂

/-- The successful cleanup an exit through frames `pre` performs, from locals `L` and state `st`: each scope's
calls run in turn (`RunsAll`), with the locals that scope restores passed on to the next, the
innermost scope first. The calls it runs are exactly `plan pre`, in that order
(certificate helper `cleans_iff_runs_plan_inv`, `planWith_calls`). -/
inductive Cleans (P : Program O) :
    List (Frame O) → AList Var O.Val → O.Store → List O.Event → O.Store → Prop where
  | nil (L : AList Var O.Val) (st : O.Store) : Cleans P [] L st [] st
  | scope {b dm ds pre L st tr₁ st₁ stDrop tr₂ st₂} : RunsAll P L ds st tr₁ st₁ →
      dropOut O dm L st₁ = some stDrop →
      Cleans P pre (restrict dm L) stDrop tr₂ st₂ →
      Cleans P (.scope b dm ds :: pre) L st (tr₁ ++ tr₂) st₂
  | seq {t pre L st tr st'} : Cleans P pre L st tr st' → Cleans P (.seq t :: pre) L st tr st'
  | test {l cr t b site dm pre L st stDrop tr st'} : dropOut O dm L st = some stDrop →
      Cleans P pre (restrict dm L) stDrop tr st' →
      Cleans P (.test l cr t b site dm :: pre) L st tr st'
  | body {l cr t b site pre L st tr st'} : Cleans P pre L st tr st' →
      Cleans P (.body l cr t b site :: pre) L st tr st'

/-! ## Destruction and receivers -/

/-- Whether function `f` in `P` is a valid `Destroy` implementation for type `T` (OD60, Q2.7,
Q2.8, owner decision 2026-11-08 item 7): `T` is not `Copy`, and `f` takes a single ordinary
parameter of type `T` and returns `Unit`. -/
def validDestroyFn [DecidableEq O.Ty] (P : Program O) (T : O.Ty) (f : FnId) : Bool :=
  !O.isCopy T && match AList.lookup P.fns f with
    | some fn => match fn.params with
      | [(_, paramTy)] => decide (paramTy = T) && decide (fn.result = O.unitTy)
      | _ => false
    | none => false

/-- The cleanup call that destroys a value of type `T`, given by the operand that moves it, when
the candidate table `impl` assigns `T` a valid `Destroy` implementation in `P`. -/
def checkedDestroyCall [DecidableEq O.Ty] (P : Program O) (impl : O.Ty → Option FnId) (T : O.Ty)
    (arg : O.Expr) (site : SiteId) : Option (Deferred O) :=
  (impl T).bind fun f => if validDestroyFn P T f then some ⟨f, [arg], site⟩ else none

/-- The one consuming destruction protocol (OD60, Q2.7, owner decision 2026-11-08 item 7). For each
type with `Destroy`, `impl` names the function that destroys it. That function's only parameter
is an ordinary parameter of that type, taken by value (it consumes the value), and it returns
`Unit`; there is no receiver form. `Copy` and `Destroy` are disjoint (Q2.8). A scope destroys a
value only through a call it registered (`destroyCall`): no destructor runs implicitly. -/
structure DestroyProtocol (P : Program O) where
  impl : O.Ty → Option FnId
  sig : ∀ T f, impl T = some f →
    ∃ fn x, AList.lookup P.fns f = some fn ∧ fn.params = [(x, T)] ∧ fn.result = O.unitTy
  notCopy : ∀ T f, impl T = some f → O.isCopy T = false

/-- The cleanup call that destroys a value of type `T`, given by the operand that moves it. -/
def DestroyProtocol.destroyCall {P : Program O} (D : DestroyProtocol P) (T : O.Ty)
    (arg : O.Expr) (site : SiteId) : Option (Deferred O) :=
  (D.impl T).map fun f => ⟨f, [arg], site⟩

/-! ## Destruction order (Q2.3, Q3.8)

The order in which the container `Destroy` implementations visit the parts of a value. They are
ordinary checked functions (Q2.7); these schedules are what their bodies must follow, stated once
here so every implementation and its proof refer to the same order. A zero-sized element is
visited in its index position like any other (Q3.8): the schedules do not look at sizes. -/

/-- A part of a value that destruction visits. -/
inductive Part where
  /-- Element `i` of a sequence. -/
  | elem (i : Nat)
  /-- The key, or the value, of the entry with the `i`-th smallest key of an ordered map. -/
  | key (i : Nat)
  | value (i : Nat)
  /-- The payload of a heap cell, an `Option` or a `Result`. -/
  | payload
  /-- A sequence's backing storage, a heap cell, or a map's nodes: released last. -/
  | storage
  deriving DecidableEq, Repr

/-- A sequence of `n` elements: indices `0 … n−1` in increasing order, then the backing (Q2.3). -/
def seqDestroyOrder (n : Nat) : List Part := (List.range n).map .elem ++ [.storage]

/-- An ordered map of `n` entries: ascending key order, each key before its value, then the nodes
(Q2.3). -/
def mapDestroyOrder (n : Nat) : List Part :=
  (List.range n).flatMap (fun i => [.key i, .value i]) ++ [.storage]

/-- A heap cell: its payload, then the cell. -/
def heapDestroyOrder : List Part := [.payload, .storage]

/-- An `Option` or `Result`: the payload that is present, if any. -/
def payloadDestroyOrder (present : Bool) : List Part := if present then [.payload] else []

/-! ## Running

`run` drives the step function with an oracle for the environment's answers, for at most `fuel`
steps. It is the skeleton of the reference evaluator (Q9.9, C8): an answer outside the operation's
contract is refused, never taken, and running out of fuel is reported as such, never as a result
of the program. -/

/-- The answer to a request: given the events so far (oldest first), the operation, its arguments,
its site and the current state. -/
abbrev Oracle (O : Operands) :=
  List O.Event → O.Prim → List O.Val → SiteId → O.Store → Option (Answer O.Val O.Event O.Store)

/-- How a run of `run` ended. -/
inductive Ending (O : Operands) where
  /-- The program finished. -/
  | final (f : Final O.Val)
  /-- No rule applies: unsafe stuckness. -/
  | stuck (c : Config O)
  /-- The oracle gave no answer: the run waits on the environment. C8 refines this (a run that
  blocks forever keeps this classification), and C6 splits a request from its response. -/
  | noAnswer (c : Config O)
  /-- The oracle gave an answer outside the operation's contract: refused, never taken. -/
  | badAnswer (c : Config O)
  /-- The fuel ran out: a statement about the budget only. -/
  | outOfFuel (c : Config O)

/-- A run's events, in order, and how it ended. -/
structure Report (O : Operands) where
  events : List O.Event
  ending : Ending O

/-- Run for at most `fuel` steps; `hist` are the events so far. A final configuration is reported
as final whatever fuel is left, so a run that ends on its last unit of fuel is not "out of fuel". -/
def runFrom (P : Program O) (ρ : Oracle O) : Nat → List O.Event → Config O → Report O
  | fuel, hist, c =>
    match c.final?, fuel with
    | some f, _ => ⟨hist, .final f⟩
    | none, 0 => ⟨hist, .outOfFuel c⟩
    | none, n + 1 =>
      match step P c with
      | none => ⟨hist, .stuck c⟩
      | some (.next c') => runFrom P ρ n hist c'
      | some (.request p vs site st k) =>
        match ρ hist p vs site st with
        | none => ⟨hist, .noAnswer c⟩
        | some a =>
          if O.allows p vs st a then
            match k a with
            | some c' => runFrom P ρ n (hist ++ a.event.toList) c'
            | none => ⟨hist, .stuck c⟩
          else ⟨hist, .badAnswer c⟩

/-- Run from `c` with no events yet. -/
def run (P : Program O) (ρ : Oracle O) (fuel : Nat) (c : Config O) : Report O :=
  runFrom P ρ fuel [] c

/-- Enter `fn` on argument values `vs` in state `st` and run for at most `fuel` steps, when the
argument count matches `fn.params` and initial activation succeeds. -/
def runFn (P : Program O) (ρ : Oracle O) (fuel : Nat) (fn : Fn O) (vs : List O.Val)
    (st : O.Store) : Option (Report O) :=
  if fn.params.length = vs.length then (enter fn vs st).map (run P ρ fuel) else none

/-! ## Examples

A small operand language: integer values; literals, locals and a shared counter as operands; the
plain `i32` arithmetic of C2, which traps on failure; one operand that traps with a given kind (to
show which failure is reported first); and two primitives:
`log`, which emits its argument as an event, and `bump`, an internal operation that adds one to the
counter without an event. -/

namespace Demo

inductive E where
  | lit (n : Int)
  | var (x : Var)
  | counter
  | fail (k : TrapKind)
  /-- `move x`: the value of `x`, which the instrumentation then records as moved. -/
  | move (x : Var)
  /-- Plain `i32` arithmetic (C2): a failure is a trap of its kind. -/
  | arith (op : ArithOp) (a b : E)

/-- The types: only `Unit` and integers matter here. -/
inductive T where
  | unit
  | int
  /-- `Tok`, a non-`Copy` type holding an integer: the type the `move` examples move (Q9.1 allows
  `move` only on a non-`Copy` type). The demo machine does not check types, so `log` takes the
  integer a `Tok` holds. -/
  | tok
  deriving DecidableEq

/-- The demo's primitives. -/
inductive Pr where
  | log
  | bump

/-- The demo's shared state: a counter, and a toy ownership instrumentation (C5 defines the real
one): for each activation, innermost first, the locals moved and not reinitialized since. -/
structure DState where
  counter : Int
  moved : List (List Var)
  deriving DecidableEq

/-- The state a run starts in. -/
def DState.init : DState := ⟨0, []⟩

/-- The locals moved in the current activation. -/
def DState.top (st : DState) : List Var := st.moved.headD []

/-- Change the current activation's moved locals. -/
def DState.onTop (f : List Var → List Var) (st : DState) : DState :=
  { st with moved := match st.moved with
    | [] => []
    | m :: rest => f m :: rest }

/-- The toy instrumentation's life: a new activation starts with nothing moved, an initialized
local is not moved, a local out of scope is forgotten. -/
def DState.life : Life → DState → DState
  | .bind x, st => st.onTop (·.filter (· ≠ x))
  | .enter _, st => { st with moved := [] :: st.moved }
  | .leave, st => { st with moved := st.moved.tail }
  | .drop xs, st => st.onTop (·.filter (· ∉ xs))

/-- Evaluate an operand. Only `move` changes the state, and only the instrumentation: reading a
moved local is stuck (the checker refuses such a program, C5). A failing operation traps, an
unbound local is stuck. -/
def evalE (L : AList Var Int) (st : DState) : E → Res (Int × DState)
  | .lit n => .ok (n, st)
  | .var x => match AList.lookup L x with
    | some v => if x ∈ st.top then .stuck else .ok (v, st)
    | none => .stuck
  | .move x => match AList.lookup L x with
    | some v => if x ∈ st.top then .stuck else .ok (v, st.onTop (x :: ·))
    | none => .stuck
  | .counter => .ok (st.counter, st)
  | .fail k => .trap k
  | .arith op a b => (evalE L st a).bind fun (x, st₁) => (evalE L st₁ b).bind fun (y, st₂) =>
      (plain .i32 op x y).map fun r => (r, st₂)

abbrev ops : Operands where
  Val := Int
  Expr := E
  Ty := T
  Store := DState
  Prim := Pr
  Event := Int
  unitVal := 0
  unitTy := .unit
  isCopy := fun t => t == .int
  eval := evalE
  life := fun e st => some (DState.life e st)
  test := fun v => some (v ≠ 0)
  allows := fun p vs st a => match p, vs, a.event, a.result with
    | .log, [v], some ev, .ok 0 => decide (ev = v) && decide (a.store = st)
    | .bump, [], none, .ok 0 => decide (a.store = { st with counter := st.counter + 1 })
    | _, _, _, _ => false

/-- The environment logs every `log` argument; `bump` adds one to the counter. -/
def env : Oracle ops := fun _ p vs _ st => match p, vs with
  | .log, [v] => some ⟨some v, .ok 0, st⟩
  | .bump, [] => some ⟨none, .ok 0, { st with counter := st.counter + 1 }⟩
  | _, _ => none

def x : Var := ⟨"x", 0⟩
def y : Var := ⟨"y", 1⟩
def a : Var := ⟨"a", 2⟩
def t : Var := ⟨"t", 3⟩
def p : Var := ⟨"p", 4⟩
def s : SiteId := ⟨0⟩
def logF : FnId := ⟨⟨[]⟩, "log"⟩
def boomF : FnId := ⟨⟨[]⟩, "boom"⟩
def mainF : FnId := ⟨⟨[]⟩, "main"⟩

/-- `fn log(x: Int) -> Unit { log x }` -/
def logFn : Fn ops := ⟨[(x, .int)], .unit, ⟨100⟩, .prim none .log [.var x] s⟩

/-- `fn boom() -> Unit { abort() }` -/
def boomFn : Fn ops := ⟨[], .unit, ⟨101⟩, .abort ⟨9⟩⟩

def bumpF : FnId := ⟨⟨[]⟩, "bump"⟩

/-- `fn bump() -> Unit { bump }` -/
def bumpFn : Fn ops := ⟨[], .unit, ⟨102⟩, .prim none .bump [] s⟩

def eatF : FnId := ⟨⟨[]⟩, "eat"⟩

/-- `fn eat(p: Tok) -> Unit { log(move p) }` -/
def eatFn : Fn ops := ⟨[(p, .tok)], .unit, ⟨103⟩, .prim none .log [.move p] s⟩

def prog : Program ops := ⟨[(logF, logFn), (boomF, boomFn), (bumpF, bumpFn), (eatF, eatFn)]⟩

def dlog (n : Int) : Stmt ops := .defer logF [.lit n] s
def callLog (n : Int) : Stmt ops := .call none logF [.lit n] s

/-- Demo entry: its instrumentation always accepts the initial activation. -/
def start (fn : Fn ops) (vs : List Int) (st : DState) : Config ops :=
  ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs,
    DState.life (.enter (fn.params.map Prod.fst)) st,
    [.scope fn.block (fn.params.map Prod.fst) []]⟩

example (fn : Fn ops) (vs : List Int) (st : DState) :
    enter fn vs st = some (start fn vs st) := rfl

/-- Run a body as `main`. -/
def go (body : Stmt ops) : Report ops := run prog env 200 (start ⟨[], .int, ⟨0⟩, body⟩ [] .init)

/-- Refusing instrumentation: a local flagged by identity `x` may not be dropped.
This only illustrates C1's refusal channel; C5 supplies the actual loan discipline. -/
abbrev refusingOps : Operands := { ops with
  life := fun e st => match e with
    | .drop xs => if x ∈ xs then none else some (DState.life e st)
    | _ => some (DState.life e st) }

/-- Accepted: unflagged locals leave scope normally. Refused: dropping the flagged local
ends in unsafe stuckness, including at the final scope exit. -/
example : (match (run (O := refusingOps) ⟨[]⟩ (fun _ _ _ _ _ => none) 10
    ⟨.exec (.block ⟨0⟩ (.bind x (.lit 1) s)), [], .init, []⟩).ending with
    | .stuck _ => true
    | _ => false) = true := by decide

example : (match (run (O := refusingOps) ⟨[]⟩ (fun _ _ _ _ _ => none) 10
    ⟨.exec (.block ⟨0⟩ (.bind y (.lit 1) s)), [], .init, []⟩).ending with
    | .final .normal => true
    | _ => false) = true := by decide

/-- A report's events and, if the program finished, how. -/
def brief (r : Report ops) : List Int × Option (Final Int) :=
  (r.events, match r.ending with
    | .final f => some f
    | _ => none)

/-- The report says the run was stuck. -/
def isStuck (r : Report ops) : Bool := match r.ending with
  | .stuck _ => true
  | _ => false

/-- The report says the fuel ran out. -/
def isOutOfFuel (r : Report ops) : Bool := match r.ending with
  | .outOfFuel _ => true
  | _ => false

/-- Normal block exit runs the innermost scope's calls first, and the newest registration of each
scope first:
```
{ defer log(1); defer log(2); { defer log(3); } } log(4);
```
logs 3, 2, 1, then 4. -/
example : (go (.seq (.block ⟨1⟩ (.seq (dlog 1) (.seq (dlog 2) (.block ⟨2⟩ (dlog 3)))))
    (callLog 4))).events = [3, 2, 1, 4] := by decide

/-- `return` evaluates its value first, then runs the cleanup of every scope it leaves, and the
value comes out unchanged:
```
{ defer log(1); { defer log(2); return 7; } }
```
logs 2, 1 and returns 7. -/
example : brief (go (.block ⟨1⟩ (.seq (dlog 1) (.block ⟨2⟩ (.seq (dlog 2) (.ret (.lit 7) s)))))) =
    ([2, 1], some (.returned 7)) := by decide

/-- `abort()` runs no deferred call:
```
{ defer log(1); abort(); }
```
logs nothing and traps with `abort`. -/
example : brief (go (.block ⟨1⟩ (.seq (dlog 1) (.abort ⟨5⟩)))) = ([], some (.trapped .abort ⟨5⟩)) := by
  decide

/-- A cleanup call that traps stops the run: no remaining cleanup runs.
```
{ defer log(1); defer boom(); }
```
`boom` runs first (the newest registration) and aborts, so `log(1)` never runs. -/
example : brief (go (.block ⟨1⟩ (.seq (dlog 1) (.defer boomF [] s)))) =
    ([], some (.trapped .abort ⟨9⟩)) := by decide

/-- `break` runs the cleanup of the scopes it leaves, then continues after the loop:
```
while 'l { } test 1 do { defer log(5); break 'l; } log(6);
```
logs 5, then 6. -/
example : (go (.seq (.loop ⟨0⟩ .skip (.lit 1) (.block ⟨1⟩ (.seq (dlog 5) (.brk ⟨0⟩))) s)
    (callLog 6))).events = [5, 6] := by decide

/-- A deferred call reads its operands when it runs (late reads, Q9.8):
```
let x = 1; { defer log(x); x = 2; }
```
logs 2. -/
example : (go (.seq (.bind x (.lit 1) s)
    (.block ⟨1⟩ (.seq (.defer logF [.var x] s) (.assign x (.lit 2) s))))).events = [2] := by
  decide

/-- Arguments are evaluated left to right; the first that traps decides the trap. -/
example : brief (go (.call none logF [.fail .overflow, .fail .divisionByZero] ⟨3⟩)) =
    ([], some (.trapped .overflow ⟨3⟩)) := by decide

/-- A block's locals are gone after it; an assignment to an outer local stays:
```
let x = 1; { let y = 5; x = 2; } return x;
```
returns 2; reading `y` after the block is stuck. -/
example : brief (go (.seq (.bind x (.lit 1) s)
    (.seq (.block ⟨1⟩ (.seq (.bind y (.lit 5) s) (.assign x (.lit 2) s))) (.ret (.var x) s)))) =
    ([], some (.returned 2)) := by decide

example : isStuck (go (.seq (.block ⟨1⟩ (.bind y (.lit 5) s)) (.ret (.var y) s))) = true := by
  decide

/-- The shared state is not restored on return: what a cleanup call does to it is seen after the
block.
```
{ defer bump(); defer bump(); } log(counter);
```
logs 2. -/
example : (go (.seq (.block ⟨1⟩ (.seq (.defer bumpF [] s) (.defer bumpF [] s)))
    (.prim none .log [.counter] s))).events = [2] := by decide

/-- Plain arithmetic of C2 runs inside the machine: `log(2147483647 + 1)` traps with kind
`overflow` at its site, and `log(7 / 2)` logs 3. -/
example : brief (go (.prim none .log [.arith .add (.lit 2147483647) (.lit 1)] ⟨4⟩)) =
    ([], some (.trapped .overflow ⟨4⟩)) := by decide

example : (go (.prim none .log [.arith .div (.lit 7) (.lit 2)] s)).events = [3] := by decide

/-- A cleanup call whose argument overflows traps when it runs, after the newer calls ran:
```
{ defer log(2147483647 + 1); defer log(1); }
```
logs 1, then traps with `overflow`. -/
example : brief (go (.block ⟨1⟩ (.seq (.defer logF [.arith .add (.lit 2147483647) (.lit 1)] ⟨6⟩)
    (dlog 1)))) = ([1], some (.trapped .overflow ⟨6⟩)) := by decide

/-- A run that is final on its last unit of fuel is final, not out of fuel. -/
example : brief (run prog env 0 ⟨.done, [], .init, []⟩) = ([], some .normal) := by decide

/-- An oracle that does not answer leaves the run waiting, which is not a refused answer; an answer
outside the contract is refused. -/
example : (match (run prog (fun _ _ _ _ _ => none) 10 (start ⟨[], .int, ⟨0⟩, callLog 1⟩ [] .init)).ending with
    | .noAnswer _ => true
    | _ => false) = true := by decide

example : (match (run prog (fun _ _ _ _ st => some ⟨some 5, .ok 0, st⟩) 10
    (start ⟨[], .int, ⟨0⟩, callLog 1⟩ [] .init)).ending with
    | .badAnswer _ => true
    | _ => false) = true := by decide

/-- Operands thread the instrumentation, left to right. Moving a local into a call transfers it:
the caller cannot read it afterwards, and a second move of it in the same argument list already
sees the first.
```
let a: Tok = 7; log(move a); return a;   // logs 7, then reading a is stuck
let a: Tok = 7; log(move a + move a);    // stuck before the call: a is moved by the first operand
```
-/
example : (fun r : Report ops => (r.events, isStuck r)) (go (.seq (.bind a (.lit 7) s)
    (.seq (.call none logF [.move a] s) (.ret (.var a) s)))) = ([7], true) := by decide

example : (fun r : Report ops => (r.events, isStuck r)) (go (.seq (.bind a (.lit 7) s)
    (.call none logF [.arith .add (.move a) (.move a)] s))) = ([], true) := by decide

/-- Assigning a moved local reinitializes it; a function that moves its parameter can be called
again, each activation having its own instrumentation.
```
let a: Tok = 7; log(move a); a = 8; log(move a);   // logs 7, 8
fn eat(p: Tok) { log(move p) }   eat(1); eat(2);    // logs 1, 2
```
-/
example : (go (.seq (.bind a (.lit 7) s) (.seq (.call none logF [.move a] s)
    (.seq (.assign a (.lit 8) s) (.call none logF [.move a] s))))).events = [7, 8] := by decide

example : brief (go (.seq (.call none eatF [.lit 1] s) (.call none eatF [.lit 2] s))) =
    ([1, 2], some .normal) := by decide

/-- A condition region's temporaries are visible to the test and gone after it:
```
while 'l { bump(); let t = counter - 3; } test t do { log(counter); continue 'l; }
log(counter);
```
logs 1, 2 (the test is `t ≠ 0`), then 3; reading `t` after the loop is stuck. -/
example : brief (go (.seq (.loop ⟨0⟩ (.seq (.call none bumpF [] s) (.bind t (.arith .sub .counter
    (.lit 3)) s)) (.var t) (.block ⟨1⟩ (.seq (.prim none .log [.counter] s) (.cont ⟨0⟩))) s)
    (.prim none .log [.counter] s))) = ([1, 2, 3], some .normal) := by decide

example : isStuck (go (.seq (.loop ⟨0⟩ (.bind t (.lit 0) s) (.var t) .skip s) (.ret (.var t) s)))
    = true := by decide

/-- Running out of fuel is reported as such: a loop that never ends is not a result. -/
example : isOutOfFuel (run prog env 50 (start ⟨[], .int, ⟨0⟩, .loop ⟨0⟩ .skip (.lit 1) .skip s⟩ [] .init))
    = true := by decide

end Demo

end ConcreteSpec.Core.Exec
