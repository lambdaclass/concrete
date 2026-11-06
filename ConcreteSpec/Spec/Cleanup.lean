/-!
# Part 3: structured cleanup

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Cleanup

/-! ## Part 3. Structured cleanup (Q9.8): `defer_cleanup_releases`

Core keeps `defer` as nested deferred scopes (Q9.8). A block registers Unit-only cleanup calls
(`destroy(x)` for a destroyable value, `give_up(x)` for resource cleanup; Q2.1, Q2.7); each call
may hold loans, released after the call. On a structured exit (falling off the end of a block,
`break L`, `continue L`, `return`) the registered calls of every scope the exit leaves run in
reverse registration order, innermost scope first. An abort runs no cleanup, and a cleanup call
that traps stops the run (Q9.8). A value's cleanup must run before the cleanup of an allocator it
depends on (Q1.5, Q1.3: resources are consumed before their allocator); registration checks this.

The model is the cleanup stack itself: frames of registered calls and the exits that unwind them.
What a deferred call does is abstracted to its identity, the resource it consumes, the resources
it needs (allocator dependencies and the sources of its loans), its loans, and whether it traps
(`ok`). Registration tracks which resources are still owned: a call may consume only an owned
resource, each resource at most once, and may need only resources that are owned or consumed by
an earlier-registered (so later-running) call. Capability checking of the calls (Q5, Q2.7) is not
modelled here. Not modelled here: the statements between registrations, late reads of the
deferred arguments, and evaluating a `return` value into an owned temporary before cleanup (Q9.8).
-/

/-- A resource in this part: a value to destroy or give up, or an allocator. -/
abbrev Res := Nat

/-- A loan held by a deferred call, and the resource it borrows from. -/
structure Loan where
  id : Nat
  src : Res
  deriving DecidableEq, Repr

/-- The kind of a deferred call (Q2.1): `destroy` consumes a destroyable value, `give_up` cleans up
a resource (an allocator's release included). Both are Unit-only (Q9.8). -/
inductive Kind where
  | destroy
  | giveUp
  deriving DecidableEq, Repr

/-- A registered cleanup call: its kind, the resource it consumes, the allocators that resource
depends on (Q1.5) and the loans it holds until it has run (Q9.8). -/
structure Deferred where
  kind : Kind
  target : Res
  deps : List Res
  loans : List Loan
  deriving DecidableEq, Repr

/-- The resources a call needs to stay unconsumed until it and its loan releases have run: the
allocators its resource depends on and the sources of its loans. -/
def Deferred.needs (d : Deferred) : List Res := d.deps ++ d.loans.map Loan.src

/-- What a scope is: a plain block, a loop body (with its loop identity, F7) or a function body. -/
inductive FrameKind where
  | block
  | loopBody (l : Nat)
  | fnBody
  deriving DecidableEq, Repr

/-- A scope and the calls it has registered, newest first. -/
structure Frame where
  kind : FrameKind
  defers : List Deferred
  deriving DecidableEq, Repr

/-- The scopes at a point, innermost first. -/
abbrev Stack := List Frame

/-- The calls the frames hold, in the order they run: innermost frame first, and within a frame
the newest registration first (reverse registration order). -/
def pending (fs : List Frame) : List Deferred := fs.flatMap Frame.defers

/-- A structured exit (F7: `break` and `continue` name their loop), or an abort. -/
inductive Exit where
  | fallthrough
  | brk (l : Nat)
  | cont (l : Nat)
  | ret
  | abort
  deriving DecidableEq, Repr

/-- The frames up to and including the innermost one of the given kind; `none` when a frame of
kind `stop` comes first (an exit does not search past its function's body). -/
def unwindTo (p stop : FrameKind → Bool) : Stack → Option (List Frame)
  | [] => none
  | f :: st =>
    if p f.kind then some [f] else if stop f.kind then none else (unwindTo p stop st).map (f :: ·)

/-- The frames an exit leaves, innermost first: falling off a block leaves that block; `break L`
and `continue L` leave every scope up to and including loop `L`'s body (`continue` then tests the
condition again, F7), and target only a loop of the same function: the search stops at the
function's body, so a loop of a caller is never the target; `return` leaves every scope of the
function. An abort runs no cleanup. -/
def unwound : Exit → Stack → Option (List Frame)
  | .fallthrough, f :: _ => some [f]
  | .fallthrough, [] => none
  | .brk l, st => unwindTo (fun k => k == .loopBody l) (fun k => k == .fnBody) st
  | .cont l, st => unwindTo (fun k => k == .loopBody l) (fun k => k == .fnBody) st
  | .ret, st => unwindTo (fun k => k == .fnBody) (fun _ => false) st
  | .abort, _ => some []

/-- An observable cleanup event: a call, or the release of a loan. -/
inductive Event where
  | call (d : Deferred)
  | release (l : Loan)
  deriving DecidableEq, Repr

/-- How cleanup ends. -/
inductive Result where
  | done
  | trapped
  deriving DecidableEq, Repr

/-- Running cleanup calls in order (Q9.8). `ok d` says whether call `d` returns or traps. A call
that returns is followed by the release of each loan it held; a call that traps ends the run,
and no later cleanup runs. -/
inductive RunCleanup (ok : Deferred → Bool) : List Deferred → List Event → Result → Prop where
  | nil : RunCleanup ok [] [] .done
  | step {d ds tr r} : ok d = true → RunCleanup ok ds tr r →
      RunCleanup ok (d :: ds) (.call d :: d.loans.map .release ++ tr) r
  | trap {d ds} : ok d = false → RunCleanup ok (d :: ds) [.call d] .trapped

/-- The events of calls that all return: each call followed by its loan releases. -/
def expected (ds : List Deferred) : List Event :=
  ds.flatMap (fun d => .call d :: d.loans.map .release)

/-- The dependency order of a run list (Q1.5): no call consumes a resource that a later call
needs, and no call needs its own resource. -/
inductive DepOrder : List Deferred → Prop where
  | nil : DepOrder []
  | cons {d ds} : d.target ∉ d.needs → (∀ d' ∈ ds, d.target ∉ d'.needs) → DepOrder ds →
      DepOrder (d :: ds)

/-- The resources the calls consume. -/
def targets (ds : List Deferred) : List Res := ds.map Deferred.target

/-- How a block builds its cleanup stack, starting from the resources it owns (`live`). Entering
a scope pushes an empty frame. `defer` adds a call to the innermost frame, provided: the call
consumes a resource still owned and not yet scheduled (so no resource is consumed twice); it does
not need its own resource; everything it needs is owned or consumed by an earlier-registered call
(which runs later); and no call already registered, in any enclosing scope, needs the resource it
consumes (Q1.5: a value is cleaned up before its allocator, and a loan's source outlives the
loan). The resource is then no longer owned. Leaving a scope removes its frame after its cleanup
ran. -/
inductive Reg : (Res → Bool) → Stack → Prop where
  | empty {live} : Reg live []
  | enter {live st k} : Reg live st → Reg live (⟨k, []⟩ :: st)
  | push {live k ds st d} : Reg live (⟨k, ds⟩ :: st) → live d.target = true →
      d.target ∉ d.needs →
      (∀ r ∈ d.needs, live r = true ∨ r ∈ targets (pending (⟨k, ds⟩ :: st))) →
      (∀ d' ∈ pending (⟨k, ds⟩ :: st), d.target ∉ d'.needs) →
      Reg (fun r => live r && r != d.target) (⟨k, d :: ds⟩ :: st)
  | leave {live f st} : Reg live (f :: st) → Reg live st

/-! ### Examples

A function owns allocator `a` (resource 0), a value `v` (resource 1) allocated from `a`, and a
buffer `b` (resource 2). The body registers `defer give_up(a)`; then, inside loop `L1`, `defer
destroy(v)`, a call that holds loan 7 borrowed from `b`; then, in a block inside the loop,
`defer destroy(b)`. -/

def relA : Deferred := { kind := .giveUp, target := 0, deps := [], loans := [] }

def desV : Deferred := { kind := .destroy, target := 1, deps := [0], loans := [⟨7, 2⟩] }

def desB : Deferred := { kind := .destroy, target := 2, deps := [], loans := [] }

def owned : Res → Bool := fun r => decide (r ∈ [0, 1, 2])

/-- `desB` in the inner block would consume `b` while `desV`'s loan from `b` is still held after
`desV` runs: registration refuses it. -/
example : ¬ (∀ d' ∈ pending [⟨.loopBody 1, [desV]⟩, ⟨.fnBody, [relA]⟩], desB.target ∉ d'.needs) := by
  decide

/-- Registering `destroy(b)` in the function body before the loop is accepted instead. -/
def stack : Stack := [⟨.block, []⟩, ⟨.loopBody 1, [desV]⟩, ⟨.fnBody, [desB, relA]⟩]

/-- What the function still owns afterwards: nothing of the three. -/
def ownedAfter : Res → Bool := fun r => ((owned r && r != 0) && r != 2) && r != 1

/-- The registration is accepted (`Reg`). -/
theorem stack_reg : Reg ownedAfter stack :=
  .enter (k := .block) <|
    .push (k := .loopBody 1) (d := desV)
      (.enter (k := .loopBody 1) <|
        .push (k := .fnBody) (d := desB)
          (.push (k := .fnBody) (d := relA) (.enter (k := .fnBody) .empty)
            rfl (by decide) (by decide) (by decide))
          rfl (by decide) (by decide) (by decide))
      rfl (by decide) (by decide) (by decide)

/-- On `return` with every call returning, the cleanup run is `destroy(v)`, the release of its loan
7, `destroy(b)` and `give_up(a)`, in that order. -/
example : RunCleanup (fun _ => true) (pending stack)
    [.call desV, .release ⟨7, 2⟩, .call desB, .call relA] .done :=
  .step rfl (.step rfl (.step rfl .nil))

/-- If `destroy(b)` traps on `return`, `destroy(v)` ran and released its loan first, and
`give_up(a)` never runs. -/
example : RunCleanup (fun d => d != desB) (pending stack)
    [.call desV, .release ⟨7, 2⟩, .call desB] .trapped :=
  .step rfl (.trap rfl)

/-- If `destroy(v)` itself traps while holding loan 7, the trace stops at that call: loan 7 is not
released by cleanup, and `destroy(b)` and `give_up(a)` never run. -/
example : RunCleanup (fun d => d != desV) (pending stack) [.call desV] .trapped :=
  .trap rfl

/-- Registering `give_up(a)` after `destroy(v)` is refused: `a` would be released first. -/
example : ¬ (∀ d' ∈ pending [⟨.fnBody, [desV]⟩], relA.target ∉ d'.needs) := by decide

/-- Falling off the inner block runs nothing; `break L1` and `continue L1` run `destroy(v)` and
release loan 7; `return` then also runs `destroy(b)` and `give_up(a)`, in that order. -/
example : (unwound .fallthrough stack).map pending = some [] := rfl

example : (unwound (.brk 1) stack).map (expected ∘ pending) =
    some [.call desV, .release ⟨7, 2⟩] := rfl

example : (unwound (.cont 1) stack).map (expected ∘ pending) =
    some [.call desV, .release ⟨7, 2⟩] := rfl

/-- `break` and `continue` never cross a function's body: a loop of the caller is no target. -/
example : unwound (.brk 1) [⟨.fnBody, []⟩, ⟨.loopBody 1, []⟩] = none := rfl
example : unwound (.cont 1) [⟨.block, []⟩, ⟨.fnBody, []⟩, ⟨.loopBody 1, []⟩] = none := rfl

example : (unwound .ret stack).map (expected ∘ pending) =
    some [.call desV, .release ⟨7, 2⟩, .call desB, .call relA] := rfl

/-- Nested labelled loops: inside loop `L1`, loop `L2` registers `destroy(d)` (resource 4), and
a block inside it registers `destroy(c)` (resource 3). -/
def desC : Deferred := { kind := .destroy, target := 3, deps := [], loans := [] }

def desD : Deferred := { kind := .destroy, target := 4, deps := [], loans := [] }

def owned2 : Res → Bool := fun r => decide (r ∈ [0, 1, 2, 3, 4])

def ownedAfter2 : Res → Bool :=
  fun r => ((((owned2 r && r != 0) && r != 2) && r != 1) && r != 4) && r != 3

def stack2 : Stack :=
  [⟨.block, [desC]⟩, ⟨.loopBody 2, [desD]⟩, ⟨.loopBody 1, [desV]⟩, ⟨.fnBody, [desB, relA]⟩]

/-- The registration is accepted (`Reg`). -/
theorem stack2_reg : Reg ownedAfter2 stack2 :=
  .push (k := .block) (d := desC)
    (.enter (k := .block) <|
      .push (k := .loopBody 2) (d := desD)
        (.enter (k := .loopBody 2) <|
          .push (k := .loopBody 1) (d := desV)
            (.enter (k := .loopBody 1) <|
              .push (k := .fnBody) (d := desB)
                (.push (k := .fnBody) (d := relA) (.enter (k := .fnBody) .empty)
                  rfl (by decide) (by decide) (by decide))
                rfl (by decide) (by decide) (by decide))
            rfl (by decide) (by decide) (by decide))
        rfl (by decide) (by decide) (by decide))
    rfl (by decide) (by decide) (by decide)

/-- Falling off the block runs `destroy(c)`; `continue L2` also runs `destroy(d)`; `break L1` and
`continue L1` (the outer label) also run `destroy(v)` and release its loan. `continue L1` then
tests `L1`'s condition again (F7). -/
example : (unwound .fallthrough stack2).map (expected ∘ pending) = some [.call desC] := rfl

example : (unwound (.cont 2) stack2).map (expected ∘ pending) = some [.call desC, .call desD] := rfl

example : (unwound (.brk 1) stack2).map (expected ∘ pending) =
    some [.call desC, .call desD, .call desV, .release ⟨7, 2⟩] := rfl

example : (unwound (.cont 1) stack2).map (expected ∘ pending) =
    some [.call desC, .call desD, .call desV, .release ⟨7, 2⟩] := rfl

end ConcreteSpec.Cleanup
