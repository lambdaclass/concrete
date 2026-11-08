import ConcreteSpec.Spec.Outcome

/-!
# Execution, calls and cleanup

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C1 (the foundation proposal approved by the
owner on 2026-11-08, "ok all recommended", question 1: the interfaces and their laws are written,
proved and tested before the freeze).

This module is the shape of every Core execution: a machine configuration, one labeled step, and
the laws that sequencing, scopes, exits, calls and cleanup obey. It is *parametric* in the operand
language (`Operands`): what a value is, how an operand is evaluated, which types exist and which
primitive operations talk to the environment. The scalar operations (C2), memory (C4) and the
environment (C6) are instances; they add operations, never change what the statements below mean
(the freeze boundary: an extension preserves the old fragment's executions).

**The configuration.** A `Config` holds the current control (a statement to run, a statement just
completed, an exit in progress, or a trap), the locals, and the stack of continuation frames that
says where execution resumes. Call frames are frames of the same stack.

**Evaluation order** (C1, STMT-4/5). Core is in A-normal form (Q9.1): an operand has no effects, so
the only order that can be observed among operands is which failure is reported first. Arguments
are evaluated left to right, and the first that traps decides the trap (`evalArgs`); every argument
is evaluated before the callee is entered. An assignment evaluates its right-hand side before
storing it. Source-level orders (receiver before arguments, left operand before right, fields and
elements in written order, `&&` and `||` evaluating their right operand only when needed) are
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
without matching it.

The laws are in `Proofs/Exec.lean`: sequencing, scope restoration (`lookup_restrict`), that an exit
carries its value unchanged through cleanup, that the deferred calls run in the stated order
(`exit_scope_runs_defers`, `unwind_runs_plan`), that a trap is terminal and that `abort()` runs no
cleanup.

`run` is the fuel-indexed driver of this step relation, the skeleton of the reference evaluator
(Q9.9); its adequacy (C8) is stated against `Step`.
-/

namespace ConcreteSpec.Core.Exec
open ConcreteSpec.Core

/-- What an operand language supplies to the machine. An instance is a lawful extension: the
statements keep their meaning whatever the instance.

- `Val`, `Expr`: values and operands. `eval` evaluates an operand against the locals, with no
  effect: a value, a trap, or unsafe stuckness (`Res`).
- `test`: the truth value of a condition (`none`: not a condition; unsafe stuckness).
- `Ty`, `unitTy`, `unitVal`, `isCopy`: the types, `Unit` and its value, and which types are `Copy`
  (a declared and checked instance; the checker owns that check, C9).
- `Prim`, `Event`, `allows`: the operations that talk to the environment, the events they emit, and
  their contract: `allows p args ev r` says the environment may answer a request `p args` by
  emitting `ev` with result `r` (a value or a trap). An environment answer outside the contract
  is never taken (C6). -/
structure Operands where
  Val : Type
  Expr : Type
  Ty : Type
  Prim : Type
  Event : Type
  unitVal : Val
  unitTy : Ty
  isCopy : Ty → Bool
  eval : AList Var Val → Expr → Res Val
  test : Val → Option Bool
  allows : Prim → List Val → Event → Except TrapKind Val → Bool

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
  /-- The one loop form (Q9.3): condition region `c`, test `test`, body `body`. -/
  | loop (l : LoopId) (c : Stmt O) (test : O.Expr) (body : Stmt O) (site : SiteId)
  /-- Leave loop `l`. -/
  | brk (l : LoopId)
  /-- Start loop `l` again from its condition region. -/
  | cont (l : LoopId)
  /-- Return an operand's value, evaluated before any scope is left. -/
  | ret (e : O.Expr) (site : SiteId)
  /-- Call `f`; bind the result to `dst`, or drop it (a `Unit` call). -/
  | call (dst : Option Var) (f : FnId) (args : List O.Expr) (site : SiteId)
  /-- A primitive operation that talks to the environment. -/
  | prim (dst : Var) (p : O.Prim) (args : List O.Expr) (site : SiteId)
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
- `test l c t body site`: loop `l`'s condition region is running; then evaluate its test.
- `body l c t body site`: loop `l`'s body is running; then start the loop again.
- `resume p`: a cleanup call is running; then continue with `p`.
- `ret dst caller`: a call is running; on its return, restore the caller's locals and bind the
  result to `dst`. -/
inductive Frame (O : Operands) where
  | seq (t : Stmt O)
  | scope (b : BlockId) (dom : List Var) (defers : List (Deferred O))
  | test (l : LoopId) (c : Stmt O) (t : O.Expr) (body : Stmt O) (site : SiteId)
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
  | ⟨.done, _, []⟩ => some .normal
  | ⟨.exiting (.ret v), _, []⟩ => some (.returned v)
  | ⟨.trapped k site, _, _⟩ => some (.trapped k site)
  | _ => none

/-- The locals of `L` whose identity is in `dom`: what a scope restores when it is left. -/
def restrict (dom : List Var) (L : AList Var V) : AList Var V :=
  L.filter (fun e => decide (e.1 ∈ dom))

/-- The identities bound in `L`. -/
def dom (L : AList Var V) : List Var := List.map Prod.fst L

/-- Bind a call's result: to `dst`, or not at all. -/
def bindDst (dst : Option Var) (v : V) (L : AList Var V) : AList Var V :=
  match dst with
  | none => L
  | some x => (x, v) :: L

/-- Evaluate arguments left to right; the first that traps (or is stuck) decides. -/
def evalArgs (O : Operands) (L : AList Var O.Val) : List O.Expr → Res (List O.Val)
  | [] => .ok []
  | e :: es => (O.eval L e).bind fun v => (evalArgs O L es).map (v :: ·)

/-- Register a cleanup call in the innermost scope, through the statements that follow it in that
block. A `defer` anywhere else (directly in a loop's condition region, say) has no meaning; the
checker refuses it. -/
def register (d : Deferred O) : List (Frame O) → Option (List (Frame O))
  | .seq t :: k => (register d k).map (.seq t :: ·)
  | .scope b dom ds :: k => some (.scope b dom (d :: ds) :: k)
  | _ => none

/-- What one step does: move to a configuration, or ask the environment (a primitive operation:
the request, its site, and the configuration each allowed result leads to). -/
inductive Out (O : Operands) where
  | next (c : Config O)
  | request (p : O.Prim) (args : List O.Val) (site : SiteId) (k : Except TrapKind O.Val → Config O)

/-- Continue with an operand's value; a trap stops at `site`, stuckness has no step. -/
def onRes (r : Res V) (site : SiteId) (L : AList Var O.Val) (k : List (Frame O))
    (f : V → Option (Out O)) : Option (Out O) :=
  match r with
  | .ok v => f v
  | .trap kd => some (.next ⟨.trapped kd site, L, k⟩)
  | .stuck => none

/-- Start the next cleanup call of a scope, then continue with `p`. -/
def startCleanup (L : AList Var O.Val) (b : BlockId) (dm : List Var) (d : Deferred O)
    (ds : List (Deferred O)) (p : Pending O.Val) (k : List (Frame O)) : Out O :=
  .next ⟨.exec d.stmt, L, .resume p :: .scope b dm ds :: k⟩

/-- One step of an exit `e` unwinding frames `k` with locals `L`: the frame decides, and only a
loop's body frame and a call frame look at the exit itself. -/
def stepExit (L : AList Var O.Val) (e : Exit O.Val) : List (Frame O) → Option (Out O)
  | .seq _ :: k => some (.next ⟨.exiting e, L, k⟩)
  | .scope b dm (d :: ds) :: k => some (startCleanup L b dm d ds (.exit e) k)
  | .scope _ dm [] :: k => some (.next ⟨.exiting e, restrict dm L, k⟩)
  | .test .. :: k => some (.next ⟨.exiting e, L, k⟩)
  | .body l cr t b site :: k => match e with
    | .brk l' => if l' = l then some (.next ⟨.done, L, k⟩) else some (.next ⟨.exiting e, L, k⟩)
    | .cont l' =>
      if l' = l then some (.next ⟨.exec (.loop l cr t b site), L, k⟩)
      else some (.next ⟨.exiting e, L, k⟩)
    | .ret _ => some (.next ⟨.exiting e, L, k⟩)
  | .ret dst caller :: k => match e with
    | .ret v => some (.next ⟨.done, bindDst dst v caller, k⟩)
    | _ => none
  | .resume _ :: _ => none
  | [] => none

/-- One step of the machine. `none`: the configuration is final, or no rule applies (unsafe
stuckness). -/
def step (P : Program O) (c : Config O) : Option (Out O) :=
  let L := c.locals
  match c.ctrl, c.frames with
  -- running a statement
  | .exec .skip, k => some (.next ⟨.done, L, k⟩)
  | .exec (.bind x e site), k => onRes (O.eval L e) site L k fun v =>
      some (.next ⟨.done, (x, v) :: L, k⟩)
  | .exec (.assign x e site), k => onRes (O.eval L e) site L k fun v =>
      (AList.lookup L x).map fun _ => .next ⟨.done, AList.update L x (fun _ => v), k⟩
  | .exec (.seq s t), k => some (.next ⟨.exec s, L, .seq t :: k⟩)
  | .exec (.block b s), k => some (.next ⟨.exec s, L, .scope b (dom L) [] :: k⟩)
  | .exec (.defer f args site), k =>
      (register ⟨f, args, site⟩ k).map fun k' => .next ⟨.done, L, k'⟩
  | .exec (.cond e s t site), k => onRes (O.eval L e) site L k fun v =>
      (O.test v).map fun b => .next ⟨.exec (if b then s else t), L, k⟩
  | .exec (.loop l cr t b site), k => some (.next ⟨.exec cr, L, .test l cr t b site :: k⟩)
  | .exec (.brk l), k => some (.next ⟨.exiting (.brk l), L, k⟩)
  | .exec (.cont l), k => some (.next ⟨.exiting (.cont l), L, k⟩)
  | .exec (.ret e site), k => onRes (O.eval L e) site L k fun v =>
      some (.next ⟨.exiting (.ret v), L, k⟩)
  | .exec (.call dst f args site), k => onRes (evalArgs O L args) site L k fun vs =>
      (AList.lookup P.fns f).bind fun fn =>
        if fn.params.length = vs.length then
          some (.next ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs,
            .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst L :: k⟩)
        else none
  | .exec (.prim x p args site), k => onRes (evalArgs O L args) site L k fun vs =>
      some (.request p vs site fun
        | .ok v => ⟨.done, (x, v) :: L, k⟩
        | .error kd => ⟨.trapped kd site, L, k⟩)
  | .exec (.abort site), k => some (.next ⟨.trapped .abort site, L, k⟩)
  -- a statement completed normally
  | .done, .seq t :: k => some (.next ⟨.exec t, L, k⟩)
  | .done, .scope b dm (d :: ds) :: k => some (startCleanup L b dm d ds .fall k)
  | .done, .scope _ dm [] :: k => some (.next ⟨.done, restrict dm L, k⟩)
  | .done, .test l cr t b site :: k => onRes (O.eval L t) site L k fun v =>
      (O.test v).map fun go =>
        if go then .next ⟨.exec b, L, .body l cr t b site :: k⟩ else .next ⟨.done, L, k⟩
  | .done, .body l cr t b site :: k => some (.next ⟨.exec (.loop l cr t b site), L, k⟩)
  | .done, .resume .fall :: k => some (.next ⟨.done, L, k⟩)
  | .done, .resume (.exit e) :: k => some (.next ⟨.exiting e, L, k⟩)
  | .done, .ret dst caller :: k => some (.next ⟨.done, bindDst dst O.unitVal caller, k⟩)
  | .done, [] => none
  -- an exit unwinding
  | .exiting e, k => stepExit L e k
  -- a trap is terminal
  | .trapped .., _ => none

/-- A step's label: silent, or an event the environment saw. -/
inductive Label (O : Operands) where
  | silent
  | emit (ev : O.Event)

/-- The events a label contributes to a trace. -/
def Label.events : Label O → List O.Event
  | .silent => []
  | .emit ev => [ev]

/-- The labeled single-step relation: a silent step, or a primitive operation answered within its
contract. -/
inductive Step (P : Program O) : Config O → Label O → Config O → Prop where
  | silent {c c'} : step P c = some (.next c') → Step P c .silent c'
  | emit {c p vs site k ev r} : step P c = some (.request p vs site k) →
      O.allows p vs ev r = true → Step P c (.emit ev) (k r)

/-- Many steps, with the events they emitted in order. -/
inductive Steps (P : Program O) : Config O → List O.Event → Config O → Prop where
  | refl (c : Config O) : Steps P c [] c
  | step {c l c' tr c''} : Step P c l c' → Steps P c' tr c'' → Steps P c (l.events ++ tr) c''

/-- The configuration that runs function `fn` on argument values `vs` as the whole program: its
`return` ends the run with the returned value. -/
def enter (fn : Fn O) (vs : List O.Val) : Config O :=
  ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, [.scope fn.block (fn.params.map Prod.fst) []]⟩

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
  | _ :: k, L => unwindLocals k L

/-- A cleanup call that completes: from locals `L`, on any frames, it runs to its normal end with
events `tr`, and gives the locals back unchanged (a call restores its caller's locals). -/
def Completes (P : Program O) (L : AList Var O.Val) (d : Deferred O) (tr : List O.Event) : Prop :=
  ∀ k, Steps P ⟨.exec d.stmt, L, k⟩ tr ⟨.done, L, k⟩

/-- Every cleanup call of these frames completes, each from the locals it runs with, with the
events `tr d`. -/
def Cleans (P : Program O) (tr : Deferred O → List O.Event) :
    List (Frame O) → AList Var O.Val → Prop
  | [], _ => True
  | .scope _ dm ds :: k, L => (∀ d ∈ ds, Completes P L d (tr d)) ∧ Cleans P tr k (restrict dm L)
  | _ :: k, L => Cleans P tr k L

/-! ## Destruction and receivers -/

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

/-! ## Running

`run` drives the step function with an oracle for the environment's answers, for at most `fuel`
steps. It is the skeleton of the reference evaluator (Q9.9, C8): an answer outside the operation's
contract is refused, never taken, and running out of fuel is reported as such, never as a result
of the program. -/

/-- The environment's answer to a request: given the events so far (oldest first), the operation,
its arguments and its site, the event it emits and the result. -/
abbrev Oracle (O : Operands) :=
  List O.Event → O.Prim → List O.Val → SiteId → Option (O.Event × Except TrapKind O.Val)

/-- How a run of `run` ended. -/
inductive Ending (O : Operands) where
  /-- The program finished. -/
  | final (f : Final O.Val)
  /-- No rule applies: unsafe stuckness. -/
  | stuck (c : Config O)
  /-- The oracle gave no answer, or one outside the operation's contract. -/
  | badAnswer (c : Config O)
  /-- The fuel ran out: a statement about the budget only. -/
  | outOfFuel (c : Config O)

/-- A run's events, in order, and how it ended. -/
structure Report (O : Operands) where
  events : List O.Event
  ending : Ending O

/-- Run for at most `fuel` steps; `hist` are the events so far. -/
def runFrom (P : Program O) (ρ : Oracle O) : Nat → List O.Event → Config O → Report O
  | 0, hist, c => ⟨hist, .outOfFuel c⟩
  | n + 1, hist, c =>
    match c.final? with
    | some f => ⟨hist, .final f⟩
    | none =>
      match step P c with
      | none => ⟨hist, .stuck c⟩
      | some (.next c') => runFrom P ρ n hist c'
      | some (.request p vs site k) =>
        match ρ hist p vs site with
        | none => ⟨hist, .badAnswer c⟩
        | some (ev, r) =>
          if O.allows p vs ev r then runFrom P ρ n (hist ++ [ev]) (k r) else ⟨hist, .badAnswer c⟩

/-- Run from `c` with no events yet. -/
def run (P : Program O) (ρ : Oracle O) (fuel : Nat) (c : Config O) : Report O :=
  runFrom P ρ fuel [] c

/-! ## Examples

A small operand language: integer values, literals and locals as operands, one operand that traps
with a given kind (to show which failure is reported first), and one primitive, `log`, that emits
its argument as an event. -/

namespace Demo

inductive E where
  | lit (n : Int)
  | var (x : Var)
  | fail (k : TrapKind)

/-- The types: only `Unit` and integers matter here. -/
inductive T where
  | unit
  | int
  deriving DecidableEq

/-- The demo's one primitive. -/
inductive Pr where
  | log

abbrev ops : Operands where
  Val := Int
  Expr := E
  Ty := T
  Prim := Pr
  Event := Int
  unitVal := 0
  unitTy := .unit
  isCopy := fun t => t == .int
  eval := fun L e => match e with
    | .lit n => .ok n
    | .var x => match AList.lookup L x with
      | some v => .ok v
      | none => .stuck
    | .fail k => .trap k
  test := fun v => some (v ≠ 0)
  allows := fun _ vs ev r => match vs, r with
    | [v], .ok 0 => decide (ev = v)
    | _, _ => false

/-- The environment logs every request's argument and answers `0`. -/
def env : Oracle ops := fun _ _ vs _ => match vs with
  | [v] => some (v, .ok 0)
  | _ => none

def x : Var := ⟨"x", 0⟩
def y : Var := ⟨"y", 1⟩
def s : SiteId := ⟨0⟩
def logF : FnId := ⟨⟨[]⟩, "log"⟩
def boomF : FnId := ⟨⟨[]⟩, "boom"⟩
def mainF : FnId := ⟨⟨[]⟩, "main"⟩

/-- `fn log(x: Int) -> Unit { log x }` -/
def logFn : Fn ops := ⟨[(x, .int)], .unit, ⟨100⟩, .prim y .log [.var x] s⟩

/-- `fn boom() -> Unit { abort() }` -/
def boomFn : Fn ops := ⟨[], .unit, ⟨101⟩, .abort ⟨9⟩⟩

def prog : Program ops := ⟨[(logF, logFn), (boomF, boomFn)]⟩

def dlog (n : Int) : Stmt ops := .defer logF [.lit n] s
def callLog (n : Int) : Stmt ops := .call none logF [.lit n] s

/-- Run a body as `main`. -/
def go (body : Stmt ops) : Report ops := run prog env 200 (enter ⟨[], .int, ⟨0⟩, body⟩ [])

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

/-- Running out of fuel is reported as such: a loop that never ends is not a result. -/
example : isOutOfFuel (run prog env 50 (enter ⟨[], .int, ⟨0⟩, .loop ⟨0⟩ .skip (.lit 1) .skip s⟩ []))
    = true := by decide

end Demo

end ConcreteSpec.Core.Exec
