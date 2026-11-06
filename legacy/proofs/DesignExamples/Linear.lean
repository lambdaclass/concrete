/-!
# Design example 1: linear move and destroy

A deliberately tiny model of Concrete's ownership law (docs/spec/ownership.md):
a non-Copy value is used exactly once, and its cleanup (`destroy`) is an explicit,
consuming verb — never inserted implicitly. It tests the semantic contract of
docs/compiler/VERIFIED_PIPELINE.md ("Resources and failure", "No implicit destruction",
Core's "ownership instrumentation with a proved erasure theorem") on concrete programs with
branches, early `return` and a fatal `abort`.

Three pieces, each small enough to review by eye:

* `exec`      — the *instrumented* semantics. Ownership is ghost state: a moved or destroyed
                variable no longer owns anything, and using it is `stuck`.
* `execErased`— the *erased* semantics: what the machine does. A move copies bits and leaves
                the source's bits in place; `destroy` frees whatever resource the variable's
                bits name. Nothing ever gets stuck on ownership.
* `chk`       — the static checker (the Core checker's linearity rules, in miniature):
                use-once, no overwrite of a live owner, branch agreement at merges, every
                owner consumed on a returning path, `abort` exempt.

The theorems are stated over the *semantics* (trace counts of `alloc`/`free` events), not over
the checker, so they are not true by construction of `chk`. Rejected programs are shown to
misbehave in the erased semantics, which demonstrates that each rule is load-bearing.

`LinearSound.lean` proves the general statement for this model: every program `chk` accepts
conserves every resource on every input and every normal or `return` exit, never destroys twice
even on `abort`, and its erased run produces the instrumented run's trace.

This is a design model, not Core: it imports nothing from the compiler.
-/

namespace DesignExamples.Linear

/-! Variables and resources are `Nat`s (resource `r` is the `r`-th acquisition). -/

/-- Statements of the model. `ite c` branches on the `c`-th input bit, so a theorem that
quantifies over the input bits covers every path. -/
inductive Stmt where
  /-- `let x = acquire();` — a fresh linear resource. -/
  | acquire (x : Nat)
  /-- `let y = x;` — moves (consumes) `x`. -/
  | move (x y : Nat)
  /-- `destroy(x);` — consumes `x` and runs its cleanup. -/
  | destroy (x : Nat)
  | seq (a b : Stmt)
  | ite (c : Nat) (a b : Stmt)
  /-- early `return` from the function. -/
  | ret
  /-- a fatal trap: the process ends; no cleanup is promised (plan, "Resources and failure"). -/
  | abort
  | skip
  deriving Repr, DecidableEq

/-- Ghost resource events. Whether these are *observable* is design question L1 (README). -/
inductive Event where
  | alloc (r : Nat)
  | free (r : Nat)
  deriving Repr, DecidableEq

structure St where
  /-- variable ↦ the resource it currently owns (instrumented) or names (erased). -/
  own : List (Nat × Nat)
  next : Nat
  trace : List Event
  deriving Repr, DecidableEq

inductive Out where
  /-- fell through the end of the statement. -/
  | fall (s : St)
  /-- left the function by `return`. -/
  | ret (s : St)
  | abort (s : St)
  /-- an ownership violation in the instrumented semantics. -/
  | stuck
  deriving Repr, DecidableEq

def lookup (x : Nat) : List (Nat × Nat) → Option Nat
  | [] => none
  | (y, r) :: rest => if y = x then some r else lookup x rest

def erase (x : Nat) : List (Nat × Nat) → List (Nat × Nat)
  | [] => []
  | (y, r) :: rest => if y = x then erase x rest else (y, r) :: erase x rest

def input (inp : List Bool) (c : Nat) : Bool := inp.getD c false

/-- Instrumented semantics: ownership is tracked, and a violation is `stuck`. -/
def exec (inp : List Bool) : Stmt → St → Out
  | .acquire x, s =>
    -- binding over a still-live owner would silently drop it (E0292)
    if (lookup x s.own).isSome then .stuck
    else .fall { own := (x, s.next) :: s.own, next := s.next + 1,
                 trace := s.trace ++ [.alloc s.next] }
  | .move x y, s =>
    match lookup x s.own with
    | none => .stuck                                   -- use after move (E0205)
    | some r =>
      let o := erase x s.own
      if (lookup y o).isSome then .stuck else .fall { s with own := (y, r) :: o }
  | .destroy x, s =>
    match lookup x s.own with
    | none => .stuck
    | some r => .fall { s with own := erase x s.own, trace := s.trace ++ [.free r] }
  | .seq a b, s =>
    match exec inp a s with
    | .fall s' => exec inp b s'
    | o => o
  | .ite c a b, s => if input inp c then exec inp a s else exec inp b s
  | .ret, s => .ret s
  | .abort, s => .abort s
  | .skip, s => .fall s

/-- Erased semantics: the machine. A move copies the bits and leaves the source intact, so a
use after move or a second `destroy` really does free the same resource again. -/
def execErased (inp : List Bool) : Stmt → St → Out
  | .acquire x, s =>
    .fall { own := (x, s.next) :: erase x s.own, next := s.next + 1,
            trace := s.trace ++ [.alloc s.next] }
  | .move x y, s =>
    match lookup x s.own with
    | none => .stuck                                   -- an unbound name: not a program
    | some r => .fall { s with own := (y, r) :: erase y s.own }
  | .destroy x, s =>
    match lookup x s.own with
    | none => .stuck
    | some r => .fall { s with trace := s.trace ++ [.free r] }
  | .seq a b, s =>
    match execErased inp a s with
    | .fall s' => execErased inp b s'
    | o => o
  | .ite c a b, s => if input inp c then execErased inp a s else execErased inp b s
  | .ret, s => .ret s
  | .abort, s => .abort s
  | .skip, s => .fall s

def init : St := { own := [], next := 0, trace := [] }

def run (inp : List Bool) (p : Stmt) : Out := exec inp p init
def runErased (inp : List Bool) (p : Stmt) : Out := execErased inp p init

/-! ## The properties, stated over traces -/

def frees (t : List Event) (r : Nat) : Nat := t.count (.free r)

/-- No resource is destroyed twice, and nothing unallocated is destroyed. -/
def noDoubleDestroy (s : St) : Bool :=
  (List.range s.next).all (fun r => frees s.trace r ≤ 1) &&
  s.trace.all (fun e => match e with | .free r => decide (r < s.next) | .alloc _ => true)

/-- Every allocated resource is destroyed (exactly once, with `noDoubleDestroy`). -/
def noLeak (s : St) : Bool := (List.range s.next).all (fun r => frees s.trace r ≥ 1)

/-- What a function's outcome must satisfy. Normal and structured exits conserve every
resource; a fatal `abort` promises no cleanup but still never double-destroys; `stuck`
is never acceptable. -/
def safeOutcome : Out → Bool
  | .fall s | .ret s => noDoubleDestroy s && noLeak s
  | .abort s => noDoubleDestroy s
  | .stuck => false

def traceOf : Out → Option (List Event)
  | .fall s | .ret s | .abort s => some s.trace
  | .stuck => none

/-! ## The checker -/

inductive CRes where
  /-- falls through with this set of live owners. -/
  | fall (live : List Nat)
  /-- every path leaves the function (`return` or `abort`). -/
  | exits
  deriving Repr, DecidableEq

def setEq (a b : List Nat) : Bool := a.all (b.contains ·) && b.all (a.contains ·)

def chk : Stmt → List Nat → Option CRes
  | .acquire x, L => if L.contains x then none else some (.fall (x :: L))
  | .move x y, L =>
    if L.contains x then
      let L' := L.erase x
      if L'.contains y then none else some (.fall (y :: L'))
    else none
  | .destroy x, L => if L.contains x then some (.fall (L.erase x)) else none
  | .seq a b, L =>
    match chk a L with
    | some (.fall L') => chk b L'
    | some .exits => some .exits
    | none => none
  | .ite _ a b, L =>
    match chk a L, chk b L with
    | some (.fall La), some (.fall Lb) => if setEq La Lb then some (.fall La) else none
    | some (.fall La), some .exits => some (.fall La)    -- a diverging arm is exempt
    | some .exits, some (.fall Lb) => some (.fall Lb)
    | some .exits, some .exits => some .exits
    | _, _ => none
  | .ret, L => if L.isEmpty then some .exits else none  -- a returning path consumes all (E0208)
  | .abort, _ => some .exits                             -- abort is exempt
  | .skip, L => some (.fall L)

/-- A function body is accepted when nothing is live at its end. -/
def checkFn (p : Stmt) : Bool :=
  match chk p [] with
  | some (.fall L) => L.isEmpty
  | some .exits => true
  | none => false

/-! ## Accepted programs -/

open Stmt

/-- `let a = acquire(); let b = a; destroy(b);` -/
def straight : Stmt := seq (acquire 0) (seq (move 0 1) (destroy 1))

/-- Branches that consume differently but agree at the merge.
```
let a = acquire();
if in0 { destroy(a); } else { let b = a; destroy(b); }
``` -/
def branches : Stmt :=
  seq (acquire 0) (ite 0 (destroy 0) (seq (move 0 1) (destroy 1)))

/-- Early return and abort.
```
let a = acquire(); let b = acquire();
if in0 { destroy(a); destroy(b); return; }
if in1 { abort(); }          // exempt: no cleanup promised
destroy(b); destroy(a);
``` -/
def earlyReturn : Stmt :=
  seq (acquire 0) <| seq (acquire 1) <|
  seq (ite 0 (seq (destroy 0) (seq (destroy 1) ret)) skip) <|
  seq (ite 1 abort skip) <|
  seq (destroy 1) (destroy 0)

theorem straight_accepted : checkFn straight = true := by decide
theorem branches_accepted : checkFn branches = true := by decide
theorem earlyReturn_accepted : checkFn earlyReturn = true := by decide

theorem straight_safe : safeOutcome (run [] straight) = true := by decide

theorem branches_safe : ∀ b0 : Bool, safeOutcome (run [b0] branches) = true := by decide

theorem earlyReturn_safe :
    ∀ b0 b1 : Bool, safeOutcome (run [b0, b1] earlyReturn) = true := by decide

/-- Non-vacuity: the early-return path really returns with resource 0 and 1 each freed once,
and the abort path really aborts holding both (abort promises no cleanup). -/
theorem earlyReturn_paths :
    run [true, false] earlyReturn =
      .ret { own := [], next := 2, trace := [.alloc 0, .alloc 1, .free 0, .free 1] } ∧
    run [false, true] earlyReturn =
      .abort { own := [(1, 1), (0, 0)], next := 2, trace := [.alloc 0, .alloc 1] } := by
  decide

/-- Erasure on accepted programs: the machine produces exactly the instrumented trace. -/
theorem earlyReturn_erasure :
    ∀ b0 b1 : Bool,
      traceOf (runErased [b0, b1] earlyReturn) = traceOf (run [b0, b1] earlyReturn) := by
  decide

theorem branches_erasure :
    ∀ b0 : Bool, traceOf (runErased [b0] branches) = traceOf (run [b0] branches) := by decide

/-! ## Rejected programs: each rule is load-bearing -/

/-- `let a = acquire(); if in0 { return; } destroy(a);` — leaks on the returning path. -/
def leakOnReturn : Stmt := seq (acquire 0) (seq (ite 0 ret skip) (destroy 0))

theorem leakOnReturn_rejected : checkFn leakOnReturn = false := by decide

theorem leakOnReturn_leaks :
    runErased [true] leakOnReturn =
      .ret { own := [(0, 0)], next := 1, trace := [.alloc 0] } ∧
    noLeak { own := [(0, 0)], next := 1, trace := [.alloc 0] } = false := by decide

/-- `let a = acquire(); let b = a; destroy(a); destroy(b);` — use after move. -/
def useAfterMove : Stmt := seq (acquire 0) (seq (move 0 1) (seq (destroy 0) (destroy 1)))

theorem useAfterMove_rejected : checkFn useAfterMove = false := by decide

/-- The instrumented semantics gets stuck; the machine frees resource 0 twice. -/
theorem useAfterMove_double_free :
    run [] useAfterMove = .stuck ∧
    traceOf (runErased [] useAfterMove) = some [.alloc 0, .free 0, .free 0] := by decide

/-- Branches disagree at the merge: `if in0 { destroy(a); } destroy(a);`. -/
def disagree : Stmt := seq (acquire 0) (seq (ite 0 (destroy 0) skip) (destroy 0))

theorem disagree_rejected : checkFn disagree = false := by decide

theorem disagree_double_free :
    traceOf (runErased [true] disagree) = some [.alloc 0, .free 0, .free 0] := by decide

/-- Rebinding a live owner: `let a = acquire(); let a = acquire(); destroy(a);` (E0292). -/
def shadowLive : Stmt := seq (acquire 0) (seq (acquire 0) (destroy 0))

theorem shadowLive_rejected : checkFn shadowLive = false := by decide

theorem shadowLive_leaks :
    traceOf (runErased [] shadowLive) = some [.alloc 0, .alloc 1, .free 1] := by decide

end DesignExamples.Linear
