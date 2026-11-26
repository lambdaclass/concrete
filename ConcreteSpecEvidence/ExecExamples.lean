import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Exec

/-! ## Examples

A small operand language: integer values; literals, locals and a shared counter as operands; the
plain `i32` arithmetic of C2, which traps on failure; one operand that traps with a given kind (to
show which failure is reported first); and two primitives:
`log`, which emits its argument as an event, and `bump`, an internal operation that adds one to the
counter without an event. -/

namespace ConcreteSpec.Core.Exec
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

def x : Var := ⟨0⟩
def y : Var := ⟨1⟩
def a : Var := ⟨2⟩
def t : Var := ⟨3⟩
def p : Var := ⟨4⟩
def s : SiteId := ⟨0⟩
def logF : FnId := FnId.mk (ModuleId.mk ⟨⟨"exec"⟩, ⟨⟨"0"⟩⟩⟩ [⟨"exec"⟩] (by decide)) ⟨"log"⟩
def boomF : FnId := FnId.mk (ModuleId.mk ⟨⟨"exec"⟩, ⟨⟨"0"⟩⟩⟩ [⟨"exec"⟩] (by decide)) ⟨"boom"⟩
def mainF : FnId := FnId.mk (ModuleId.mk ⟨⟨"exec"⟩, ⟨⟨"0"⟩⟩⟩ [⟨"exec"⟩] (by decide)) ⟨"main"⟩

/-- `fn log(x: Int) -> Unit { log x }` -/
def logFn : Fn ops := ⟨[(x, .int)], .unit, ⟨100⟩, .prim none .log [.var x] s⟩

/-- `fn boom() -> Unit { abort() }` -/
def boomFn : Fn ops := ⟨[], .unit, ⟨101⟩, .abort ⟨9⟩⟩

def bumpF : FnId := FnId.mk (ModuleId.mk ⟨⟨"exec"⟩, ⟨⟨"0"⟩⟩⟩ [⟨"exec"⟩] (by decide)) ⟨"bump"⟩

/-- `fn bump() -> Unit { bump }` -/
def bumpFn : Fn ops := ⟨[], .unit, ⟨102⟩, .prim none .bump [] s⟩

def eatF : FnId := FnId.mk (ModuleId.mk ⟨⟨"exec"⟩, ⟨⟨"0"⟩⟩⟩ [⟨"exec"⟩] (by decide)) ⟨"eat"⟩

/-- `fn eat(p: Tok) -> Unit { log(move p) }` -/
def eatFn : Fn ops := ⟨[(p, .tok)], .unit, ⟨103⟩, .prim none .log [.move p] s⟩

def prog : Program ops := ⟨[(logF, logFn), (boomF, boomFn), (bumpF, bumpFn), (eatF, eatFn)]⟩

def dlog (n : Int) : Stmt ops := .defer logF [.lit n] s
def callLog (n : Int) : Stmt ops := .call none logF [.lit n] s

/-- Demo entry: its instrumentation always accepts the initial activation. -/
def start (fn : Fn ops) (vs : List Int) (st : DState) : Config ops :=
  ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs,
    DState.life (.enter (fn.params.map Prod.fst)) st,
    [.scope fn.block (fn.params.map Prod.fst) [], .functionEnd]⟩

/-- Run a body as `main`. -/
def go (body : Stmt ops) : Report ops := run prog env 200 (start ⟨[], .int, ⟨0⟩, body⟩ [] .init)

/-- Refusing instrumentation: a local flagged by identity `x` may not be dropped.
This only illustrates C1's refusal channel; C5 supplies the actual loan discipline. -/
abbrev refusingOps : Operands := { ops with
  life := fun e st => match e with
    | .drop xs => if x ∈ xs then none else some (DState.life e st)
    | _ => some (DState.life e st) }

/-- A report's events and, if the program finished, how. -/
def brief (r : Report ops) : List Int × Option (Final Int) :=
  (r.events, match r.ending with
    | .final f => some f
    | _ => none)

/-- The report says the run was stuck. -/
def isStuck (r : Report ops) : Bool := match r.ending with
  | .stuck => true
  | _ => false

/-- The report says the fuel ran out. -/
def isOutOfFuel (r : Report ops) : Bool := match r.ending with
  | .outOfFuel => true
  | _ => false

/-- A toy admission policy refusing entry only. -/
abbrev noEntry : Operands := { ops with life := fun e st => match e with
  | .enter _ => none
  | _ => some (DState.life e st) }

/-- Refuse only initialization of x; all unrelated lifecycle events succeed. -/
abbrev noBind : Operands := { ops with life := fun e st => match e with
  | .bind v => if v = x then none else some (DState.life e st)
  | _ => some (DState.life e st) }

/-- Return admission is independent of admission to bind its result. -/
abbrev noLeave : Operands := { ops with life := fun e st => match e with
  | .leave => none
  | _ => some (DState.life e st) }

-- Representation lemma: demo entry expands the chosen encoding.
theorem example_fn_Fn_ops_vs_List (fn : Fn ops) (vs : List Int) (st : DState) :
    enter fn vs st = some (start fn vs st) := rfl

/-- Accepted: unflagged locals leave scope normally. Refused: dropping the flagged local
ends in unsafe stuckness, including at the final scope exit. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_match_run_O : (match (run (O := refusingOps) ⟨[]⟩ (fun _ _ _ _ _ => none) 10
    ⟨.exec (.block ⟨0⟩ (.bind x (.lit 1) s)), [], .init, []⟩).ending with
    | .stuck => true
    | _ => false) = true := by decide

-- Instance: concrete execution case; not a general safety law.
theorem example_match_run_O_2 : (match (run (O := refusingOps) ⟨[]⟩ (fun _ _ _ _ _ => none) 10
    ⟨.exec (.block ⟨0⟩ (.bind y (.lit 1) s)), [], .init, []⟩).ending with
    | .final .normal => true
    | _ => false) = true := by decide

/-- Normal block exit runs the innermost scope's calls first, and the newest registration of each
scope first:
```
{ defer log(1); defer log(2); { defer log(3); } } log(4);
```
logs 3, 2, 1, then 4. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_go_seq_block_seq_dlog : (go (.seq (.block ⟨1⟩ (.seq (dlog 1) (.seq (dlog 2) (.block ⟨2⟩ (dlog 3)))))
    (callLog 4))).events = [3, 2, 1, 4] := by decide

/-- `return` evaluates its value first, then runs the cleanup of every scope it leaves, and the
value comes out unchanged:
```
{ defer log(1); { defer log(2); return 7; } }
```
logs 2, 1 and returns 7. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_brief_go_block_seq_dlog : brief (go (.block ⟨1⟩ (.seq (dlog 1) (.block ⟨2⟩ (.seq (dlog 2) (.ret (.lit 7) s)))))) =
    ([2, 1], some (.returned 7)) := by decide

/-- `abort()` runs no deferred call:
```
{ defer log(1); abort(); }
```
logs nothing and traps with `abort`. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_brief_go_block_seq_dlog_2 : brief (go (.block ⟨1⟩ (.seq (dlog 1) (.abort ⟨5⟩)))) = ([], some (.trapped .abort ⟨5⟩)) := by
  decide

/-- A cleanup call that traps stops the run: no remaining cleanup runs.
```
{ defer log(1); defer boom(); }
```
`boom` runs first (the newest registration) and aborts, so `log(1)` never runs. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_brief_go_block_seq_dlog_3 : brief (go (.block ⟨1⟩ (.seq (dlog 1) (.defer boomF [] s)))) =
    ([], some (.trapped .abort ⟨9⟩)) := by decide

/-- `break` runs the cleanup of the scopes it leaves, then continues after the loop:
```
while 'l { } test 1 do { defer log(5); break 'l; } log(6);
```
logs 5, then 6. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_go_seq_loop_skip_lit : (go (.seq (.loop ⟨0⟩ .skip (.lit 1) (.block ⟨1⟩ (.seq (dlog 5) (.brk ⟨0⟩))) s)
    (callLog 6))).events = [5, 6] := by decide

/-- A deferred call reads its operands when it runs (late reads, Q9.8):
```
let x = 1; { defer log(x); x = 2; }
```
logs 2. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_go_seq_bind_x_lit : (go (.seq (.bind x (.lit 1) s)
    (.block ⟨1⟩ (.seq (.defer logF [.var x] s) (.assign x (.lit 2) s))))).events = [2] := by
  decide

/-- Arguments are evaluated left to right; the first that traps decides the trap. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_brief_go_call_logF_fail : brief (go (.call none logF [.fail .overflow, .fail .divisionByZero] ⟨3⟩)) =
    ([], some (.trapped .overflow ⟨3⟩)) := by decide

/-- A block's locals are gone after it; an assignment to an outer local stays:
```
let x = 1; { let y = 5; x = 2; } return x;
```
returns 2; reading `y` after the block is stuck. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_brief_go_seq_bind_x : brief (go (.seq (.bind x (.lit 1) s)
    (.seq (.block ⟨1⟩ (.seq (.bind y (.lit 5) s) (.assign x (.lit 2) s))) (.ret (.var x) s)))) =
    ([], some (.returned 2)) := by decide

-- Instance: concrete execution case; not a general safety law.
theorem example_isStuck_go_seq_block_bind : isStuck (go (.seq (.block ⟨1⟩ (.bind y (.lit 5) s)) (.ret (.var y) s))) = true := by
  decide

/-- The shared state is not restored on return: what a cleanup call does to it is seen after the
block.
```
{ defer bump(); defer bump(); } log(counter);
```
logs 2. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_go_seq_block_seq_defer : (go (.seq (.block ⟨1⟩ (.seq (.defer bumpF [] s) (.defer bumpF [] s)))
    (.prim none .log [.counter] s))).events = [2] := by decide

/-- Plain arithmetic of C2 runs inside the machine: `log(2147483647 + 1)` traps with kind
`overflow` at its site, and `log(7 / 2)` logs 3. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_brief_go_prim_log_arith : brief (go (.prim none .log [.arith .add (.lit 2147483647) (.lit 1)] ⟨4⟩)) =
    ([], some (.trapped .overflow ⟨4⟩)) := by decide

-- Instance: concrete execution case; not a general safety law.
theorem example_go_prim_log_arith_div : (go (.prim none .log [.arith .div (.lit 7) (.lit 2)] s)).events = [3] := by decide

/-- A cleanup call whose argument overflows traps when it runs, after the newer calls ran:
```
{ defer log(2147483647 + 1); defer log(1); }
```
logs 1, then traps with `overflow`. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_brief_go_block_seq_defer : brief (go (.block ⟨1⟩ (.seq (.defer logF [.arith .add (.lit 2147483647) (.lit 1)] ⟨6⟩)
    (dlog 1)))) = ([1], some (.trapped .overflow ⟨6⟩)) := by decide

/-- A run that is final on its last unit of fuel is final, not out of fuel. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_brief_run_prog_env_done : brief (run prog env 0 ⟨.done, [], .init, []⟩) = ([], some .normal) := by decide

/-- An oracle that does not answer leaves the run waiting, which is not a refused answer; an answer
outside the contract is refused. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_match_run_prog_start_int : (match (run prog (fun _ _ _ _ _ => none) 10 (start ⟨[], .int, ⟨0⟩, callLog 1⟩ [] .init)).ending with
    | .noAnswer => true
    | _ => false) = true := by decide

-- Instance: concrete execution case; not a general safety law.
theorem example_match_run_prog_st_ok : (match (run prog (fun _ _ _ _ st => some ⟨some 5, .ok 0, st⟩) 10
    (start ⟨[], .int, ⟨0⟩, callLog 1⟩ [] .init)).ending with
    | .badAnswer => true
    | _ => false) = true := by decide

/-- Operands thread the instrumentation, left to right. Moving a local into a call transfers it:
the caller cannot read it afterwards, and a second move of it in the same argument list already
sees the first.
```
let a: Tok = 7; log(move a); return a;   // logs 7, then reading a is stuck
let a: Tok = 7; log(move a + move a);    // stuck before the call: a is moved by the first operand
```
-/
-- Instance: concrete execution case; not a general safety law.
theorem example_r_Report_ops_r_events : (fun r : Report ops => (r.events, isStuck r)) (go (.seq (.bind a (.lit 7) s)
    (.seq (.call none logF [.move a] s) (.ret (.var a) s)))) = ([7], true) := by decide

-- Instance: concrete execution case; not a general safety law.
theorem example_r_Report_ops_r_events_2 : (fun r : Report ops => (r.events, isStuck r)) (go (.seq (.bind a (.lit 7) s)
    (.call none logF [.arith .add (.move a) (.move a)] s))) = ([], true) := by decide

/-- Assigning a moved local reinitializes it; a function that moves its parameter can be called
again, each activation having its own instrumentation.
```
let a: Tok = 7; log(move a); a = 8; log(move a);   // logs 7, 8
fn eat(p: Tok) { log(move p) }   eat(1); eat(2);    // logs 1, 2
```
-/
-- Instance: concrete execution case; not a general safety law.
theorem example_go_seq_bind_a_lit : (go (.seq (.bind a (.lit 7) s) (.seq (.call none logF [.move a] s)
    (.seq (.assign a (.lit 8) s) (.call none logF [.move a] s))))).events = [7, 8] := by decide

-- Instance: concrete execution case; not a general safety law.
theorem example_brief_go_seq_call_eatF : brief (go (.seq (.call none eatF [.lit 1] s) (.call none eatF [.lit 2] s))) =
    ([1, 2], some .normal) := by decide

/-- A condition region's temporaries are visible to the test and gone after it:
```
while 'l { bump(); let t = counter - 3; } test t do { log(counter); continue 'l; }
log(counter);
```
logs 1, 2 (the test is `t ≠ 0`), then 3; reading `t` after the loop is stuck. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_brief_go_seq_loop_seq : brief (go (.seq (.loop ⟨0⟩ (.seq (.call none bumpF [] s) (.bind t (.arith .sub .counter
    (.lit 3)) s)) (.var t) (.block ⟨1⟩ (.seq (.prim none .log [.counter] s) (.cont ⟨0⟩))) s)
    (.prim none .log [.counter] s))) = ([1, 2, 3], some .normal) := by decide

-- Instance: concrete execution case; not a general safety law.
theorem example_isStuck_go_seq_loop_bind : isStuck (go (.seq (.loop ⟨0⟩ (.bind t (.lit 0) s) (.var t) .skip s) (.ret (.var t) s)))
    = true := by decide

/-- Running out of fuel is reported as such: a loop that never ends is not a result. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_isOutOfFuel_run_prog_env_start : isOutOfFuel (run prog env 50 (start ⟨[], .int, ⟨0⟩, .loop ⟨0⟩ .skip (.lit 1) .skip s⟩ [] .init))
    = true := by decide

/-! ### Lifecycle admission and residual boundaries

Each pair changes only admission at the named site. These are executable
examples, not ownership safety theorems. The real ownership instance and its
accepted-program non-refusal proof arrive later. Exact report equalities also
pin history, store, locals, frames and fuel, rather than merely testing stuckness.
-/

/-- Successful entry runs the body and final storage teardown. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_runFn_O : runFn (O := ops) ⟨[]⟩ env 9 ⟨[], .unit, ⟨0⟩, .skip⟩ [] .init =
    some ⟨[], .final .normal, ⟨.done, [], .init, []⟩, 6⟩ := rfl

/-- Refused initial entry is unsafe stuckness, even with zero fuel. Its state
has not been admitted and cannot be resumed as a function body. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_runFn_O_2 : runFn (O := noEntry) ⟨[]⟩ (fun _ _ _ _ _ => none) 0
    ⟨[], .unit, ⟨0⟩, .skip⟩ [] .init =
    some ⟨[], .stuck, ⟨.refused, [], .init, [.scope ⟨0⟩ [] [], .functionEnd]⟩, 0⟩ := rfl

/-- Argument-count mismatch remains distinguishable from refused entry. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_runFn_O_3 : runFn (O := noEntry) ⟨[]⟩ (fun _ _ _ _ _ => none) 9
    ⟨[(x, .int)], .unit, ⟨0⟩, .skip⟩ [] .init = none := rfl

/-- Binding succeeds and spends one transition. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_run_O : run (O := ops) ⟨[]⟩ env 4
    ⟨.exec (.bind x (.lit 8) s), [], .init, []⟩ =
    ⟨[], .final .normal, ⟨.done, [(x, 8)], .init, []⟩, 3⟩ := rfl

/-- Refused binding preserves the entire source configuration and budget. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_run_O_2 : run (O := noBind) ⟨[]⟩ (fun _ _ _ _ _ => none) 4
    ⟨.exec (.bind x (.lit 8) s), [], .init, []⟩ =
    ⟨[], .stuck, ⟨.exec (.bind x (.lit 8) s), [], .init, []⟩, 4⟩ := rfl

/-- A permitted primitive response binds its result and records its event. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_run_O_3 : run (O := ops) ⟨[]⟩ env 4
    ⟨.exec (.prim (some x) .log [.lit 8] s), [], .init, []⟩ =
    ⟨[8], .final .normal, ⟨.done, [(x, 0)], .init, []⟩, 3⟩ := rfl

/-- Refused primitive continuation has no accepted response transition or
accepted event. The oracle may already have performed an external effect:
this residual is a semantic boundary, not an assertion of rollback. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_run_O_4 : run (O := noBind) ⟨[]⟩ env 4
    ⟨.exec (.prim (some x) .log [.lit 8] s), [], .init, []⟩ =
    ⟨[], .stuck, ⟨.exec (.prim (some x) .log [.lit 8] s), [], .init, []⟩, 4⟩ := rfl

/-- The very last scope exit succeeds only after its drop is admitted. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_run_O_5 : run (O := refusingOps) ⟨[]⟩ env 4
    ⟨.done, [(y, 8)], .init, [.scope ⟨0⟩ [] []]⟩ =
    ⟨[], .final .normal, ⟨.done, [], .init, []⟩, 3⟩ := rfl

/-- Refusing that last drop cannot turn into normal completion. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_run_O_6 : run (O := refusingOps) ⟨[]⟩ env 4
    ⟨.done, [(x, 8)], .init, [.scope ⟨0⟩ [] []]⟩ =
    ⟨[], .stuck, ⟨.done, [(x, 8)], .init, [.scope ⟨0⟩ [] []]⟩, 4⟩ := rfl

/-- No fuel: preserve a nonfinal configuration and an existing history. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_runFrom_prog_env_exec_skip : runFrom prog env 0 [9] ⟨.exec .skip, [(x, 2)], .init, []⟩ =
    ⟨[9], .outOfFuel, ⟨.exec .skip, [(x, 2)], .init, []⟩, 0⟩ := rfl

/-- Finality takes precedence over zero fuel and preserves locals and frames. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_runFrom_prog_env_trapped_abort : runFrom prog env 0 [9] ⟨.trapped .abort s, [(x, 2)], .init, [.seq .skip]⟩ =
    ⟨[9], .final (.trapped .abort s),
      ⟨.trapped .abort s, [(x, 2)], .init, [.seq .skip]⟩, 0⟩ := rfl

/-- One silent step spends exactly one unit, retaining the next continuation. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_runFrom_prog_env_exec_skip_2 : runFrom prog env 1 [9] ⟨.exec .skip, [(x, 2)], .init, [.seq .skip]⟩ =
    ⟨[9], .outOfFuel, ⟨.done, [(x, 2)], .init, [.seq .skip]⟩, 0⟩ := rfl

/-- Silence without an event still accepts a primitive response and its store. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_runFrom_prog_env_exec_prim : runFrom prog env 4 [9] ⟨.exec (.prim none .bump [] s), [], .init, []⟩ =
    ⟨[9], .final .normal, ⟨.done, [], ⟨1, []⟩, []⟩, 3⟩ := rfl

/-- Append, never replace or reverse, the previously accepted events. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_runFrom_prog_env_exec_prim_2 : runFrom prog env 4 [9] ⟨.exec (.prim none .log [.lit 8] s), [], .init, []⟩ =
    ⟨[9, 8], .final .normal, ⟨.done, [], .init, []⟩, 3⟩ := rfl

/-- An unanswered request spends no transition budget. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_runFrom_prog : runFrom prog (fun _ _ _ _ _ => none) 4 [9]
    ⟨.exec (.prim none .log [.lit 8] s), [], .init, []⟩ =
    ⟨[9], .noAnswer, ⟨.exec (.prim none .log [.lit 8] s), [], .init, []⟩, 4⟩ := rfl

/-- An answer violating the operation's contract is distinct from no answer. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_runFrom_prog_st_ok_st : runFrom prog (fun _ _ _ _ st => some ⟨some 7, .ok 0, st⟩) 4 [9]
    ⟨.exec (.prim none .log [.lit 8] s), [], .init, []⟩ =
    ⟨[9], .badAnswer, ⟨.exec (.prim none .log [.lit 8] s), [], .init, []⟩, 4⟩ := rfl

/-- The same accepted store update survives even when the budget ends at it. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_runFrom_prog_env : runFrom prog env 1 [9]
    ⟨.exec (.prim none .bump [] s), [], .init, [.seq .skip]⟩ =
    ⟨[9], .outOfFuel, ⟨.done, [], ⟨1, []⟩, [.seq .skip]⟩, 0⟩ := rfl

-- Instance: concrete execution case; not a general safety law.
theorem example_run_O_7 : run (O := noLeave) ⟨[]⟩ env 4
    ⟨.exiting (.ret 8), [(y, 3)], ⟨0, [[], []]⟩, [.functionEnd, .ret none [(x, 2)]]⟩ =
    ⟨[], .stuck, ⟨.exiting (.ret 8), [(y, 3)], ⟨0, [[], []]⟩,
      [.functionEnd, .ret none [(x, 2)]]⟩, 4⟩ := rfl

-- Instance: concrete execution case; not a general safety law.
theorem example_run_O_8 : run (O := ops) ⟨[]⟩ env 4
    ⟨.exiting (.ret 8), [(y, 3)], ⟨0, [[], []]⟩, [.functionEnd, .ret (some x) [(y, 2)]]⟩ =
    ⟨[], .final .normal, ⟨.done, [(x, 8), (y, 2)], ⟨0, [[]]⟩, []⟩, 2⟩ := rfl

/-- A return whose leave succeeds can still be refused at destination binding;
the caller remains unrestored; the admitted leave remains in the residual store. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_run_O_9 : run (O := noBind) ⟨[]⟩ env 4
    ⟨.exiting (.ret 8), [(y, 3)], ⟨0, [[], []]⟩, [.functionEnd, .ret (some x) [(y, 2)]]⟩ =
    ⟨[], .stuck, ⟨.exiting (.ret 8), [(y, 3)], ⟨0, [[]]⟩,
      [.ret (some x) [(y, 2)]]⟩, 3⟩ := rfl

/-- Fallthrough return also admits leave before restoring the caller. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_run_O_10 : run (O := noLeave) ⟨[]⟩ env 4
    ⟨.done, [], ⟨0, [[], []]⟩, [.functionEnd, .ret none [(x, 2)]]⟩ =
    ⟨[], .stuck, ⟨.done, [], ⟨0, [[], []]⟩, [.functionEnd, .ret none [(x, 2)]]⟩, 4⟩ := rfl

/-- Nested function entry uses the same refusal channel as initial entry. -/
-- Instance: concrete execution case; not a general safety law.
theorem example_run_O_11 : run (O := noEntry)
    ⟨[(logF, ⟨[(x, .int)], .unit, ⟨100⟩, .skip⟩)]⟩ env 4
    ⟨.exec (.call none logF [.lit 8] s), [], .init, []⟩ =
    ⟨[], .stuck, ⟨.exec (.call none logF [.lit 8] s), [], .init, []⟩, 4⟩ := rfl

/-! Function-end admission: the body has no bindings, nested scopes, calls or
primitives. Each case isolates leave from every other refusal site. The same
`functionEnd` rules apply with or without a caller continuation. -/

/-- Instance: accepting fallthrough tears down the last activation. -/
theorem example_accepting_fallthrough_tears_down_last_activation : runFn (O := ops) ⟨[]⟩ env 9 ⟨[], .unit, ⟨0⟩, .skip⟩ [] .init =
    some ⟨[], .final .normal, ⟨.done, [], .init, []⟩, 6⟩ := rfl

/-- Instance: refusing fallthrough is unsafe stuck before finality. -/
theorem example_refusing_fallthrough_is_unsafe_stuck_before : runFn (O := noLeave) ⟨[]⟩ env 9 ⟨[], .unit, ⟨0⟩, .skip⟩ [] .init =
    some ⟨[], .stuck, ⟨.done, [], ⟨0, [[]]⟩, [.functionEnd]⟩, 7⟩ := rfl

/-- Instance: accepting explicit return preserves its value and classification. -/
theorem example_accepting_explicit_return_preserves_its_value : runFn (O := ops) ⟨[]⟩ env 9 ⟨[], .int, ⟨0⟩, .ret (.lit 8) s⟩ [] .init =
    some ⟨[], .final (.returned 8), ⟨.exiting (.ret 8), [], .init, []⟩, 6⟩ := rfl

/-- Instance: refusing explicit return cannot publish a returned value. -/
theorem example_refusing_explicit_return_cannot_publish_returned : runFn (O := noLeave) ⟨[]⟩ env 9 ⟨[], .int, ⟨0⟩, .ret (.lit 8) s⟩ [] .init =
    some ⟨[], .stuck, ⟨.exiting (.ret 8), [], ⟨0, [[]]⟩, [.functionEnd]⟩, 7⟩ := rfl

/-- Instance: independent fallthrough rule, with exactly one leave premise. -/
theorem example_independent_fallthrough_rule_exactly_one_leave : Rules (O := ops) ⟨[]⟩ ⟨.done, [], ⟨0, [[]]⟩, [.functionEnd]⟩
    none .silent ⟨.done, [], .init, []⟩ := .endFall rfl

/-- Instance: independent explicit-return rule preserves returned control. -/
theorem example_independent_explicit_return_rule_preserves_returned : Rules (O := ops) ⟨[]⟩ ⟨.exiting (.ret 8), [], ⟨0, [[]]⟩, [.functionEnd]⟩
    none .silent ⟨.exiting (.ret 8), [], .init, []⟩ := .endReturn rfl

/-- Instance: no relational label, answer or destination can bypass refused fallthrough. -/
theorem example_no_relational_label_answer_or_destination (a : Option (Answer Int Int DState)) (label : Label noLeave) (c : Config noLeave) :
    ¬ Rules (O := noLeave) ⟨[]⟩ ⟨.done, [], ⟨0, [[]]⟩, [.functionEnd]⟩ a label c := by
  intro h
  cases h with
  | endFall admission => cases admission

/-- Instance: no relational label, answer or destination can bypass refused return. -/
theorem example_no_relational_label_answer_or_destination_2 (a : Option (Answer Int Int DState)) (label : Label noLeave) (c : Config noLeave) :
    ¬ Rules (O := noLeave) ⟨[]⟩ ⟨.exiting (.ret 8), [], ⟨0, [[]]⟩, [.functionEnd]⟩
      a label c := by
  intro h
  cases h with
  | endReturn admission => cases admission

end Demo
end ConcreteSpec.Core.Exec
