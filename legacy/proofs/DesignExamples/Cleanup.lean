/-!
# Design example 3: structured cleanup (`defer`) on every exit path

Core has "structured control, scoped `defer`"; Place MIR has "cleanup/`defer` edges on every
exit" (VERIFIED_PIPELINE.md, IR chain), and S10's exit asks for cleanup expansion "proved for
one representative function with early exits and `defer`". This file models both sides in
miniature:

* `exec`  — the Core-level meaning: each block keeps its own deferred list; on fall-through,
            `break`, `continue` and `return` the list runs newest-first (LIFO), inner scopes
            before outer ones; a fatal `abort` runs nothing (plan, "Resources and failure"),
            and a cleanup that traps is fatal too: the cleanups after it do not run.
* `lower` — a Place-MIR-like expansion with **no defer and no scopes**: every exit edge is
            prefixed with the cleanup calls it must run, computed statically.
* theorems — the Core meaning on normal, early-return, break, continue, abort and
            trapping-cleanup paths, for every input; and agreement of `lower` with `exec` on
            every example and input.

The lowering is possible as a *static* expansion only because a `defer` statement registers
in its enclosing block, so the pending cleanups at every program point are known from the
block structure (design question C1).

Deferred calls evaluate their arguments **at exit**, as `Lower.lean` does today (it lowers the
deferred `CExpr` again at each exit). `deferPrint` makes that visible (design question C2).

This is a design model, not Core: it imports nothing from the compiler.
-/

namespace DesignExamples.Cleanup

/-- A deferred call: `cleanup(n)`, `print_int(i)` (reads the counter when it runs), or a
cleanup call that traps. -/
inductive Deferred where
  | call (n : Nat)
  | printI
  | trap
  deriving Repr, DecidableEq

inductive Cond where
  /-- the `n`-th input bit. -/
  | input (n : Nat)
  /-- `i >= k` -/
  | ge (k : Nat)
  deriving Repr, DecidableEq

inductive Stmt where
  /-- an observable call, `f_n()`. -/
  | emit (n : Nat)
  /-- `print_int(i)` -/
  | printI
  /-- `i = i + 1` -/
  | incr
  /-- `defer d;` registers `d` in the enclosing block. -/
  | defer (d : Deferred)
  /-- `{ ss }` — a scope with its own deferred list. -/
  | block (ss : List Stmt)
  | ite (c : Cond) (a b : Stmt)
  /-- `loop { body }` — leaves only by `break`, `return` or `abort`. -/
  | loop (body : Stmt)
  | brk
  | cont
  | ret
  | abort
  deriving Repr

inductive Event where
  | call (n : Nat)
  | print (v : Nat)
  deriving Repr, DecidableEq

inductive Outcome where
  | normal
  | brk
  | cont
  | ret
  | abort
  /-- the clock ran out: inconclusive, never a behaviour. -/
  | fuel
  deriving Repr, DecidableEq

structure St where
  i : Nat
  trace : List Event
  deriving Repr, DecidableEq

def evalC (inp : List Bool) (s : St) : Cond → Bool
  | .input n => inp.getD n false
  | .ge k => decide (s.i ≥ k)

def runDeferred (s : St) : Deferred → St
  | .call n => { s with trace := s.trace ++ [.call n] }
  | .printI => { s with trace := s.trace ++ [.print s.i] }
  | .trap => s

/-- Run a deferred list, stored newest-first, in that order (LIFO). A trapping cleanup is
fatal: the cleanups after it do not run (`true` = trapped). -/
def runDefers : List Deferred → St → Bool × St
  | [], s => (false, s)
  | .trap :: _, s => (true, s)
  | d :: rest, s => runDefers rest (runDeferred s d)

/-- Unwind a scope: run its cleanups, then continue with `o` unless a cleanup trapped. -/
def unwind (o : Outcome) (ds : List Deferred) (s : St) : Outcome × St :=
  match runDefers ds s with
  | (true, s') => (.abort, s')
  | (false, s') => (o, s')

mutual
/-- The Core-level meaning, clock-indexed. -/
def exec (inp : List Bool) : Nat → Stmt → St → Outcome × St
  | 0, _, s => (.fuel, s)
  | _ + 1, .emit n, s => (.normal, { s with trace := s.trace ++ [.call n] })
  | _ + 1, .printI, s => (.normal, { s with trace := s.trace ++ [.print s.i] })
  | _ + 1, .incr, s => (.normal, { s with i := s.i + 1 })
  -- a `defer` that is a whole branch or loop body is a block of one statement
  | _ + 1, .defer d, s => unwind .normal [d] s
  | f + 1, .block ss, s => execBlock inp f ss [] s
  | f + 1, .ite c a b, s => if evalC inp s c then exec inp f a s else exec inp f b s
  | f + 1, .loop body, s =>
    match exec inp f body s with
    | (.normal, s') | (.cont, s') => exec inp f (.loop body) s'
    | (.brk, s') => (.normal, s')
    | r => r
  | _ + 1, .brk, s => (.brk, s)
  | _ + 1, .cont, s => (.cont, s)
  | _ + 1, .ret, s => (.ret, s)
  | _ + 1, .abort, s => (.abort, s)

/-- A block: `ds` is its deferred list, newest first. -/
def execBlock (inp : List Bool) : Nat → List Stmt → List Deferred → St → Outcome × St
  | 0, _, _, s => (.fuel, s)
  | _ + 1, [], ds, s => unwind .normal ds s
  | f + 1, .defer d :: rest, ds, s => execBlock inp f rest (d :: ds) s
  | f + 1, st :: rest, ds, s =>
    match exec inp f st s with
    | (.normal, s') => execBlock inp f rest ds s'
    | (.abort, s') => (.abort, s')          -- fatal: no cleanup
    | (.fuel, s') => (.fuel, s')
    | (o, s') => unwind o ds s'              -- break / continue / return unwind this scope
end

/-! ## The lowered form: explicit cleanup on every exit edge -/

inductive LStmt where
  | emit (n : Nat)
  | printI
  | incr
  | seq (ss : List LStmt)
  | ite (c : Cond) (a b : LStmt)
  | loop (body : LStmt)
  | brk
  | cont
  | ret
  | abort
  deriving Repr

def cleanupCode (ds : List Deferred) : List LStmt :=
  ds.map fun | .call n => .emit n | .printI => .printI | .trap => .abort

/-- Pending cleanups at a program point: all of them up to the function boundary (for
`return`) and those registered since the innermost loop body began (for `break`/`continue`). -/
structure Pending where
  fn : List Deferred
  lp : List Deferred

mutual
def lower (P : Pending) : Stmt → LStmt
  | .emit n => .emit n
  | .printI => .printI
  | .incr => .incr
  | .defer d => .seq (cleanupCode [d])
  | .block ss => lowerBlock P ss []
  | .ite c a b => .ite c (lower P a) (lower P b)
  | .loop body => .loop (lower { P with lp := [] } body)
  | .brk => .seq (cleanupCode P.lp ++ [.brk])
  | .cont => .seq (cleanupCode P.lp ++ [.cont])
  | .ret => .seq (cleanupCode P.fn ++ [.ret])
  | .abort => .abort

def lowerBlock (P : Pending) : List Stmt → List Deferred → LStmt
  | [], ds => .seq (cleanupCode ds)
  | .defer d :: rest, ds => lowerBlock P rest (d :: ds)
  | st :: rest, ds =>
    .seq [lower { fn := ds ++ P.fn, lp := ds ++ P.lp } st, lowerBlock P rest ds]
end

mutual
def execL (inp : List Bool) : Nat → LStmt → St → Outcome × St
  | 0, _, s => (.fuel, s)
  | _ + 1, .emit n, s => (.normal, { s with trace := s.trace ++ [.call n] })
  | _ + 1, .printI, s => (.normal, { s with trace := s.trace ++ [.print s.i] })
  | _ + 1, .incr, s => (.normal, { s with i := s.i + 1 })
  | f + 1, .seq ss, s => execLs inp f ss s
  | f + 1, .ite c a b, s => if evalC inp s c then execL inp f a s else execL inp f b s
  | f + 1, .loop body, s =>
    match execL inp f body s with
    | (.normal, s') | (.cont, s') => execL inp f (.loop body) s'
    | (.brk, s') => (.normal, s')
    | r => r
  | _ + 1, .brk, s => (.brk, s)
  | _ + 1, .cont, s => (.cont, s)
  | _ + 1, .ret, s => (.ret, s)
  | _ + 1, .abort, s => (.abort, s)

def execLs (inp : List Bool) : Nat → List LStmt → St → Outcome × St
  | 0, _, s => (.fuel, s)
  | _ + 1, [], s => (.normal, s)
  | f + 1, st :: rest, s =>
    match execL inp f st s with
    | (.normal, s') => execLs inp f rest s'
    | r => r
end

def s0 : St := { i := 0, trace := [] }

/-- Run a function body; a `return` is a normal function exit. -/
def run (inp : List Bool) (fuel : Nat) (p : Stmt) : Outcome × List Event :=
  let (o, s) := exec inp fuel p s0
  (o, s.trace)

def runL (inp : List Bool) (fuel : Nat) (p : Stmt) : Outcome × List Event :=
  let (o, s) := execL inp fuel (lower { fn := [], lp := [] } p) s0
  (o, s.trace)

/-! ## Examples -/

open Stmt

/-- Normal exit, LIFO.
```
{ f1(); defer c10(); defer c11(); f2(); }   // f1 f2 c11 c10
``` -/
def normalEx : Stmt := block [emit 1, defer (.call 10), defer (.call 11), emit 2]

theorem normalEx_order :
    run [] 20 normalEx = (.normal, [.call 1, .call 2, .call 11, .call 10]) := by decide

/-- Early return from a nested scope runs inner cleanups, then outer ones.
```
{ defer c10();
  { defer c20();
    if in0 { return; }
    f3(); }               // c20 runs at the inner scope's end
  f4(); }                 // c10 runs at the outer scope's end
``` -/
def earlyReturnEx : Stmt :=
  block [defer (.call 10),
         block [defer (.call 20), ite (.input 0) ret (block []), emit 3],
         emit 4]

theorem earlyReturnEx_paths :
    run [true] 20 earlyReturnEx = (.ret, [.call 20, .call 10]) ∧
    run [false] 20 earlyReturnEx = (.normal, [.call 3, .call 20, .call 4, .call 10]) := by
  decide

/-- `break` and `continue` unwind only the loop body's scopes; a defer inside the loop body
runs once per iteration, the function-level defer once.
```
{ defer c10();
  loop {
    defer c30();
    if i >= 2 { break; }
    i = i + 1;
    if in0 { continue; }
    f5();
  }
  f6(); }
``` -/
def loopEx : Stmt :=
  block [defer (.call 10),
         loop (block [defer (.call 30), ite (.ge 2) brk (block []), incr,
                      ite (.input 0) cont (block []), emit 5]),
         emit 6]

theorem loopEx_paths :
    run [false] 60 loopEx =
      (.normal, [.call 5, .call 30, .call 5, .call 30, .call 30, .call 6, .call 10]) ∧
    run [true] 60 loopEx = (.normal, [.call 30, .call 30, .call 30, .call 6, .call 10]) := by
  decide

/-- `abort` is terminal: pending cleanups do not run.
```
{ defer c10(); f1(); abort(); }
``` -/
def abortEx : Stmt := block [defer (.call 10), emit 1, abort]

theorem abortEx_no_cleanup : run [] 20 abortEx = (.abort, [.call 1]) := by decide

/-- Deferred arguments are evaluated when the cleanup runs, not when it is registered.
```
{ defer print_int(i); i = i + 1; i = i + 1; }   // prints 2, not 0
``` -/
def deferPrint : Stmt := block [defer .printI, incr, incr]

theorem deferPrint_exit_time : run [] 20 deferPrint = (.normal, [.print 2]) := by decide

/-- A trap inside a cleanup during an early return is fatal: earlier cleanup effects remain,
later pending cleanups (here the outer `c10`) do not run.
```
{ defer c10();
  { defer trapping_cleanup(); defer c20(); return; } }   // c20, then the trap: abort
``` -/
def trapCleanupEx : Stmt :=
  block [defer (.call 10), block [defer .trap, defer (.call 20), ret]]

theorem trapCleanupEx_fatal : run [] 20 trapCleanupEx = (.abort, [.call 20]) := by decide

/-! ## The static expansion agrees with the scoped meaning -/

theorem normalEx_lowered : runL [] 40 normalEx = run [] 20 normalEx := by decide

theorem earlyReturnEx_lowered :
    ∀ b : Bool, runL [b] 40 earlyReturnEx = run [b] 20 earlyReturnEx := by decide

theorem loopEx_lowered : ∀ b : Bool, runL [b] 120 loopEx = run [b] 60 loopEx := by decide

theorem abortEx_lowered : runL [] 40 abortEx = run [] 20 abortEx := by decide

theorem trapCleanupEx_lowered : runL [] 40 trapCleanupEx = run [] 20 trapCleanupEx := by decide

theorem deferPrint_lowered : runL [] 40 deferPrint = run [] 20 deferPrint := by decide

end DesignExamples.Cleanup
