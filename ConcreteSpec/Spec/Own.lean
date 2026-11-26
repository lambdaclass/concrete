import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Exec

/-!
# C5: ownership instrumentation

PROPOSED, before Core freeze. Authority on main: `docs/decisions.md`, Q3.1–Q3.4
(second-class downward views, root plus path, whole-root conflicts), Q3.8 (zero-byte
obligations), Q9.10 (ghost instrumentation and proved erasure), OD15/OD23 (root exclusion
including re-lending), OD60/OD62/OD69 (explicit consumption, no implicit destruction).
The recovered C5 answers at `#decided-2026-11-08-recovered-d3` authorize current/callee
loan destinations, call-duration owner lending, and conditional converse erasure.
`#decided-2026-11-13-lifecycle-refusal` authorizes every optional lifecycle result.
Capability permissions and grant values remain separate under Q5/Q7 and
`#decided-2026-11-08-capabilities-kept`; this module supplies no new authority rule.

OD62's source-name usage and Result/Option discard checks belong to typed checker admission;
`Descriptor.copy` alone does not implement them. OD64's borrow-derived raw-pointer confinement
remains an obligation of the raw/foreign operand language; no pointer escape rule is invented
here. Aggregate layout and pointer representation remain held under ISS-0070; this scalar
instrumentation resolves no aggregate projection. There are no new policy choices here.

The examples are Instances (including the unsafe erasure counterexamples), not the future
preservation/erasure Laws of F14–F16. `CheckerAccepted` states that future checker obligation;
it is not itself a checker implementation or a safety theorem.

Roots reuse `Hybrid.Place`, not addresses or a third place identity. The monotonically increasing place supply distinguishes
recursive activations. A slot carries its type and checked Copy/mutability facts; C9 must bind
those facts to its richer type identity when the scalar-only Hybrid universe is extended.

Transitions check operational permissions, resource status and issued identities only.
The later preservation laws must start from a well-formed input; transitions do not
recheck the invariant at run time.
Parent links record lending dependencies only: no stacked/tree borrow validity model is assumed.
Projection paths are logical selectors; resolving their types/values belongs to aggregate slices.
-/
namespace ConcreteSpec.Core.Own

inductive Status where
  | owned | moved
  deriving DecidableEq

inductive Mode where
  | shared | mutable
  deriving DecidableEq

/-- A logical projection, retaining field identity or element index (Q3.3). -/
inductive Projection where
  | field (id : FieldId)
  | index (n : Nat)
  deriving DecidableEq

/-- The checked binding descriptor supplied by the operand instance, not guessed from a value.
`ty` describes its C4 scalar payload representation: a linear token may have an integer or
zero-byte payload, without becoming Copy. Semantic Copy/mutability evidence and the richer
nominal type identity are the operand instance/C9 admission obligation. -/
structure Descriptor where
  ty : CTy
  copy : Bool
  mutable : Bool
  deriving DecidableEq

structure Slot where
  place : Hybrid.Place
  activation : Nat
  desc : Descriptor
  status : Status
  deriving DecidableEq

/-- A loan retains its original owner and full projection. `ancestors` caches the parent chain;
the invariant checks the cache, including strict birth order. No loan identity is recycled. -/
structure Loan where
  id : LoanId
  root : Hybrid.Place
  path : List Projection
  mode : Mode
  parent : Option LoanId
  ancestors : List LoanId
  activation : Nat
  pendingArgument : Bool := false
  callRoot : Bool := false
  deriving DecidableEq

structure Activation where
  id : Nat
  locals : AList Var Hybrid.Place := []
  views : AList Var LoanId := []
  deriving DecidableEq

structure State where
  slots : List Slot := []
  loans : List Loan := []
  frames : List Activation := []
  nextPlace : Nat := 0
  nextLoan : Nat := 0
  nextActivation : Nat := 0
  deriving DecidableEq

def slot (s : State) (p : Hybrid.Place) : Option Slot := s.slots.find? (·.place == p)
def loan (s : State) (i : LoanId) : Option Loan := s.loans.find? (·.id == i)

/-- An exclusive conflict is whole-root, even for disjoint paths. Ancestor loans are suspended
rather than competing grants; their access is checked separately by `loanAccess`. -/
def Compatible (a b : Loan) : Prop :=
  a.root ≠ b.root ∨ a.id = b.id ∨ a.id ∈ b.ancestors ∨ b.id ∈ a.ancestors ∨
    (a.mode = .shared ∧ b.mode = .shared)
instance (a b : Loan) : Decidable (Compatible a b) := by unfold Compatible; infer_instance

/-- Check a lending edge against the active loan table. -/
def ParentValid (s : State) (l : Loan) : Prop :=
  match l.parent with
  | none => l.ancestors = []
  | some p => ∃ a ∈ s.loans, a.id = p ∧ a.root = l.root ∧ a.id.id < l.id.id ∧
      l.ancestors = a.id :: a.ancestors ∧
      (a.mode = .shared → l.mode = .shared) ∧ a.path <+: l.path

instance (s : State) (l : Loan) : Decidable (ParentValid s l) := by
  unfold ParentValid
  split <;> infer_instance

/-- Every live loan has an owned root; every parent exists, predates its child and has the
same root. Shared access cannot be upgraded. Cached chains agree exactly. Distinct competing
loans are compatible. Supplies bound all issued identities, including moved slots. Active
frames have distinct issued identities and their `locals` tables index every slot belonging
to an active frame's activation. Every slot belongs to an active frame; a frame's local
index may designate only slots owned by that frame. -/
structure WellFormed (s : State) : Prop where
  slotsNodup : (s.slots.map Slot.place).Nodup
  loansNodup : (s.loans.map Loan.id).Nodup
  slotIdsIssued : ∀ x ∈ s.slots, x.place.id < s.nextPlace ∧ x.activation < s.nextActivation
  loansValid : ∀ l ∈ s.loans,
    l.id.id < s.nextLoan ∧ l.activation < s.nextActivation ∧
    (∃ x ∈ s.slots, x.place = l.root ∧ x.status = .owned) ∧
    ParentValid s l ∧ (l.pendingArgument = true → (l.parent.isSome = true ∨ l.callRoot = true))
  loansCompatible : ∀ a ∈ s.loans, ∀ b ∈ s.loans, Compatible a b
  framesNodup : s.frames.Pairwise (fun a b => a.id ≠ b.id)
  framesIndexSlots : ∀ f ∈ s.frames, f.id < s.nextActivation ∧
    ∀ x ∈ s.slots, x.activation = f.id → x.place ∈ List.map Prod.snd f.locals
  slotsHaveFrame : ∀ x ∈ s.slots, (∃ f ∈ s.frames, f.id = x.activation)
  frameLocalsValid : ∀ f ∈ s.frames, ∀ p ∈ (List.map Prod.snd f.locals),
    ∃ x ∈ s.slots, x.place = p ∧ x.activation = f.id

instance (s : State) : Decidable (WellFormed s) :=
  decidable_of_iff _ <| Iff.intro
    (fun ⟨slots, loans, issued, valid, compatible, frames, indexed, active, locals⟩ =>
      ⟨slots, loans, issued, valid, compatible, frames, indexed, active, locals⟩)
    (fun h => And.intro h.slotsNodup <| And.intro h.loansNodup <| And.intro h.slotIdsIssued <|
      And.intro h.loansValid <| And.intro h.loansCompatible <| And.intro h.framesNodup <|
      And.intro h.framesIndexSlots <| And.intro h.slotsHaveFrame h.frameLocalsValid)

/-- Loan destinations refer to frames still active in the call stack.
This lifetime invariant is independent of the monotonically increasing activation supply. -/
def LiveDestinations (s : State) : Prop :=
  ∀ l ∈ s.loans, ∃ f ∈ s.frames, f.id = l.activation

instance (s : State) : Decidable (LiveDestinations s) := by
  unfold LiveDestinations
  infer_instance

/-- Owner accesses cannot bypass whole-root loans. Reads may coexist with shared loans (Q3.4, OD15). -/
def ownerAccess (s : State) (p : Hybrid.Place) (m : Mode) : Bool :=
  (slot s p).any fun x => x.status == .owned &&
    (m == .shared || x.desc.mutable) && s.loans.all (fun l =>
      l.root != p || (m == .shared && l.mode == .shared))

/-- Access through a loan ignores its suspended ancestors. A conflicting child suspends access
through the parent until it ends; unrelated accesses still obey whole-root exclusion (Q3.2, Q3.4). -/
def loanAccess (s : State) (i : LoanId) (m : Mode) : Bool :=
  (loan s i).any fun a => !a.pendingArgument && (m == .shared || a.mode == .mutable) &&
    s.loans.all (fun b => b.id == i || decide (b.id ∈ a.ancestors) || b.root != a.root ||
      (m == .shared && b.mode == .shared))

/-- OD60/OD62, Q3.8: move is only for non-Copy owners; it changes no concrete value. -/
def move (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if x.desc.copy || x.status != .owned || s.loans.any (·.root == p) then none else
    some { s with slots := s.slots.map fun x =>
      if x.place = p then { x with status := .moved } else x }

/-- OD62/OD69: reinitialization requires no active loan. Replacing an owned linear value is refused:
consume it first, rather than silently discarding its obligation. -/
def reinitialize (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if (x.status == .moved || (x.desc.copy && x.desc.mutable)) &&
      s.loans.all (·.root != p) then
    some { s with slots := s.slots.map fun x =>
      if x.place = p then { x with status := .owned } else x }
  else none

/-- Open an owner loan. The fresh identity is the old counter. The frame identity must
have been issued by `enter`; this is an identity guard, not an invariant recheck. -/
def borrow (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode) :
    Option State := do
  let f ← s.frames.head?
  if ownerAccess s p m && decide (f.id < s.nextActivation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, p, path, m, none, [], f.id, false, false⟩ :: s.loans }
  else none

/-- Reborrow appends a projection, retains its root and cannot upgrade shared access.
Owner decision 2026-11-08 Q1: the requested destination must be the current active frame.
`enterDown` first enters the callee, making that newly entered frame current. There is no
operation that lends upward or targets an inactive, previously issued activation. -/
def reborrowTo (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m && decide (activation < s.nextActivation ∧
      s.frames.head?.map Activation.id = some activation) then
    some { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, activation, false, false⟩ :: s.loans }
  else none

def reborrow (s : State) (i : LoanId) (suffix : List Projection) (m : Mode) :
    Option State := do
  let f ← s.frames.head?
  reborrowTo s i suffix m f.id

/-- Children must end before parents (Q3.2); ending a loan neither destroys nor moves its owner. -/
def endLoan (s : State) (i : LoanId) : Option State := do
  let _ ← loan s i
  if s.loans.any (·.parent == some i) then none else
    some { s with loans := s.loans.filter (·.id != i) }

/-- The public loan-creation and ending operations, as a finite lifetime protocol. -/
inductive LoanAction where
  | borrow (root : Hybrid.Place) (path : List Projection) (mode : Mode)
  | reborrow (parent : LoanId) (path : List Projection) (mode : Mode) (activation : Nat)
  | endLoan (loan : LoanId)

def loanAction (s : State) : LoanAction → Option State
  | .borrow p path m => borrow s p path m
  | .reborrow i path m a => reborrowTo s i path m a
  | .endLoan i => endLoan s i

def runLoanSequence (s : State) (ops : List LoanAction) : Option State :=
  ops.foldlM loanAction s

/-- The identity was issued, and its active loan has ended. Ending its root is not required. -/
def RetiredLoan (s : State) (i : LoanId) : Prop :=
  i.id < s.nextLoan ∧ ∀ l ∈ s.loans, l.id ≠ i
instance (s : State) (i : LoanId) : Decidable (RetiredLoan s i) := by
  unfold RetiredLoan
  infer_instance

/-- Read/write permissions are partial identity transitions on the instrumentation. Concrete
reads/writes are supplied separately by `instrument`; they cannot manufacture permission. -/
def accessOwner (s : State) (p : Hybrid.Place) (m : Mode) : Option State :=
  if ownerAccess s p m then some s else none

def accessLoan (s : State) (i : LoanId) (m : Mode) : Option State :=
  if loanAccess s i m then some s else none

/-- A descriptor is supplied by the checked operand language. A fresh binding gets a fresh place;
an assignment to an existing binding must discharge/reinitialize its previous obligation.
New slots require an activation issued by `enter` (Q3.2's activation/lifetime boundary). -/
def bind (types : Var → Option Descriptor) (x : Var) (s : State) : Option State := do
  let f ← s.frames.head?
  match AList.lookup f.locals x with
  | some p => reinitialize s p
  | none =>
    let d ← types x
    if f.id < s.nextActivation then some { s with
      nextPlace := s.nextPlace + 1
      slots := ⟨⟨s.nextPlace⟩, f.id, d, .owned⟩ :: s.slots
      frames := { f with locals := (x, ⟨s.nextPlace⟩) :: f.locals } :: s.frames.tail }
    else none

/-- The lifetime permission on exact owner identities, independent of their local names. -/
def canDropPlaces (s : State) (ps : List Hybrid.Place) : Bool :=
  s.slots.all (fun x => decide (x.place ∉ ps) || x.desc.copy || x.status == .moved) &&
    s.loans.all (fun l => decide (l.root ∉ ps))

/-- Remove ended owner identities and their frame bindings. Callers establish
`canDropPlaces` before erasing. -/
def erasePlaces (s : State) (ps : List Hybrid.Place) : State :=
  {s with
    slots := s.slots.filter (fun x => decide (x.place ∉ ps))
    frames := s.frames.map fun f => {f with locals := f.locals.filter (fun e => decide (e.2 ∉ ps))}}

/-- A local can leave scope only after its linear obligation is discharged and all its loans
end. Copy permits duplication, and does not justify discarding a non-Copy value (Q3.8). -/
def drop (xs : List Var) (s : State) : Option State := do
  let f ← s.frames.head?
  let ps := List.map Prod.snd (f.locals.filter fun e => decide (e.1 ∈ xs))
  if canDropPlaces s ps then
    some { s with
      slots := s.slots.filter (fun x => decide (x.place ∉ ps))
      frames := { f with locals := f.locals.filter (fun e => decide (e.2 ∉ ps)) } :: s.frames.tail }
  else none

/-- Enter owns a new activation. Downward loans use `enterDown` below; ordinary values use bind. -/
def enter (types : Var → Option Descriptor) (params : List Var) (s : State) : Option State := do
  let s := { s with
    nextActivation := s.nextActivation + 1
    frames := ⟨s.nextActivation, [], []⟩ :: s.frames }
  params.foldlM (fun s x => bind types x s) s

/-- End all loans created by this activation in reverse birth order (children first).
Loans are newest first. A loan of an outer root passed into this call is a callee child,
not the caller's parent loan. -/
def endLocals (activation : Nat) (s : State) : Option State :=
  (s.loans.filter (·.activation == activation)).foldlM (fun s l => endLoan s l.id) s

/-- Leave consumes no resource implicitly: all remaining local owners must be Copy or moved.
Local loans end before local roots are removed. By the frame-local completeness clause of
`WellFormed`, every slot belonging to the departing activation is indexed in `f.locals`, so
dropping `List.map Prod.fst f.locals` discharges and removes all slots of `f.id`. -/
def leave (s : State) : Option State := do
  let f ← s.frames.head?
  let s ← endLocals f.id s
  let s ← drop (List.map Prod.fst f.locals) s
  some { s with frames := s.frames.tail }

/-- Downward-only lending: each parameter gets a new child loan. Views never become owner slots,
so cannot be returned/stored/moved by the owner operations. This standalone call-boundary transition supports the preservation laws. C1 uses `callLife`
below to adopt loans that were opened earlier, during typed argument evaluation.
No reference-valued concrete storage is introduced. -/
def enterDown (s : State) (args : List (Var × LoanId)) : Option State := do
  let s ← enter (fun _ => none) [] s
  args.foldlM (fun s (x, i) => do
    let a ← loan s i
    let f ← s.frames.head?
    let s ← reborrowTo s i [] a.mode f.id
    some { s with frames := { f with views := (x, ⟨s.nextLoan - 1⟩) :: f.views } :: s.frames.tail }) s

/-- The Exec lifecycle callback: optional admission at every site, including the final
function boundary (`docs/decisions.md#decided-2026-11-13-lifecycle-refusal`). -/
def life (types : Var → Option Descriptor) : Exec.Life → State → Option State
  | .bind x, s => bind types x s
  | .enter ps, s => enter types ps s
  | .leave, s => leave s
  | .drop xs, s => drop xs s

/-- A call's view arguments have already opened their loans while arguments were evaluated,
so later arguments observe their freezes (OD23). Transfer only those fresh children to the
callee activation; do not reborrow again or move the caller's original view.
The current activation must have been issued by `enter` (Q3.2, OD23). -/
def adoptDown (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := f.id, pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Metadata for loan arguments waiting for C1's enter event. No reference is stored in a
concrete value: the callee parameter is associated with its loan only in this instrumentation. -/
structure CallState where
  own : State
  pending : List (Var × LoanId) := []
  deriving DecidableEq

/-- Call argument bookkeeping: names/identities are unique, every pending identity exists,
and precisely the listed loans carry the pending flag. This is instrumentation metadata,
not reference-valued Concrete storage (Q3.1–Q3.3, OD23). -/
def CallState.WellFormed (s : CallState) : Prop :=
  Own.WellFormed s.own ∧
  (s.pending.map Prod.fst).Nodup ∧ (s.pending.map Prod.snd).Nodup ∧
  (∀ e ∈ s.pending, ∃ l ∈ s.own.loans, l.id = e.2) ∧
  (∀ l ∈ s.own.loans, l.pendingArgument = true ↔ l.id ∈ s.pending.map Prod.snd)
instance (s : CallState) : Decidable (CallState.WellFormed s) := by
  unfold CallState.WellFormed
  infer_instance

/-- Evaluate a downward view argument now, before the next argument. Target binder identities
come from the checked call signature. Shared views may be passed repeatedly, each with a fresh
loan; a mutable view's first lending freezes it immediately. A signature parameter cannot
be supplied twice; unfinished argument evaluation cannot cross another lifecycle event
(Q3.2 downward parameters, OD23 left-to-right argument freezes). -/
def passDown (s : CallState) (source : LoanId) (parameter : Var) : Option CallState := do
  if parameter ∈ s.pending.map Prod.fst then none else do
  let l ← loan s.own source
  let own ← reborrow s.own source [] l.mode
  let own := { own with loans := own.loans.map fun l =>
    if l.id.id = s.own.nextLoan then { l with pendingArgument := true } else l }
  pure ⟨own, s.pending ++ [(parameter, ⟨s.own.nextLoan⟩)]⟩

/-- Direct owner argument (owner Q2, 2026-11-08): open a fresh root loan during
argument evaluation. It has no caller parent. Entry adopts it and return ends it with the
callee's loans. Existing caller loans retain their identities and activation. A deferred
call evaluates its operands when it actually runs (Q9.8, OD2); registering a defer never
ends a loan or evaluates these argument operands. -/
def passOwner (s : CallState) (root : Hybrid.Place) (path : List Projection)
    (mode : Mode) (parameter : Var) : Option CallState := do
  if parameter ∈ s.pending.map Prod.fst then none else do
  let f ← s.own.frames.head?
  if ownerAccess s.own root mode && decide (f.id < s.own.nextActivation) then
    pure ⟨{ s.own with
      nextLoan := s.own.nextLoan + 1
      loans := ⟨⟨s.own.nextLoan⟩, root, path, mode, none, [], f.id, true, true⟩ :: s.own.loans },
      s.pending ++ [(parameter, ⟨s.own.nextLoan⟩)]⟩
  else none

/-- A C1 lifecycle adapter for mixed owning/view parameters. Owning parameters use descriptors;
view parameters adopt the loans opened during left-to-right argument evaluation. There is no
loan-valued result-binding case. Every other lifecycle event uses the same ownership rules. -/
def callLife (types : Var → Option Descriptor) : Exec.Life → CallState → Option CallState
  | .enter params, s => do
    if s.pending.all (fun e => decide (e.1 ∈ params)) then
      let own ← enter types (params.filter fun x => decide (x ∉ s.pending.map Prod.fst)) s.own
      let own ← adoptDown own s.pending
      pure ⟨own, []⟩
    else none
  | e, s => if s.pending = [] then
      (life types e s.own).map fun own => { s with own := own }
    else none

/-- The call adapter also carries an arbitrary concrete memory/resource store unchanged. -/
def callLifeStore {σ : Type} (types : Var → Option Descriptor) (e : Exec.Life)
    (s : σ × CallState) : Option (σ × CallState) :=
  (callLife types e s.2).map fun own => (s.1, own)

/-- Erasure forgets the instrumentation, keeping exactly the operand instance's concrete store. -/
structure Store (σ : Type) where
  concrete : σ
  own : State
  deriving DecidableEq

def erase {σ : Type} (s : Store σ) : σ := s.concrete

/-- Couple a permission transition to an uninstrumented concrete operation. Neither permission
checking nor erasure can select a different value, result or concrete branch. -/
def instrument {σ α : Type} (permit : State → Option State)
    (operation : σ → Option (α × σ)) (s : Store σ) : Option (α × Store σ) := do
  let own ← permit s.own
  let (a, concrete) ← operation s.concrete
  some (a, ⟨concrete, own⟩)

def lifeStore {σ : Type} (types : Var → Option Descriptor) (e : Exec.Life)
    (s : Store σ) : Option (Store σ) :=
  (life types e s.own).map fun own => ⟨s.concrete, own⟩

/-- Evaluate an operand under ownership instrumentation (Q9.10, owner decision 2026-11-08 Q3):
the permission transition must succeed on the ownership state, and the concrete operand
evaluation then runs on the concrete store. Refused permissions yield unsafe stuckness;
concrete traps and results pass through unchanged. -/
def instrumentEval (O : Exec.Operands)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (locals : AList Var O.Val) (s : Store O.Store) (e : O.Expr) :
    Res (O.Val × Store O.Store) :=
  match permit locals e s.own with
  | none => .stuck
  | some own => (O.eval locals s.concrete e).map fun (v, concrete) => (v, ⟨concrete, own⟩)

/-- Apply a C1 lifecycle event to both the ownership state and the concrete store. -/
def instrumentLife (O : Exec.Operands) (types : Var → Option Descriptor)
    (e : Exec.Life) (s : Store O.Store) : Option (Store O.Store) := do
  let own ← life types e s.own
  let concrete ← O.life e s.concrete
  some ⟨concrete, own⟩

/-- Forget the ownership state carried by a primitive operation's answer. -/
def eraseAnswer {Val Event σ : Type} (a : Exec.Answer Val Event (Store σ)) :
    Exec.Answer Val Event σ :=
  ⟨a.event, a.result, erase a.store⟩

/-- Lift a concrete primitive answer into an instrumented store by keeping the current
ownership state; result binding updates ownership separately through `bindLife`. -/
def liftAnswer {Val Event σ : Type} (own : State) (a : Exec.Answer Val Event σ) :
    Exec.Answer Val Event (Store σ) :=
  ⟨a.event, a.result, ⟨a.store, own⟩⟩

/-- A primitive answer is allowed on the instrumented store when its erased answer
satisfies the concrete contract and the primitive does not fabricate ownership state. -/
def instrumentAllows (O : Exec.Operands) (p : O.Prim) (vs : List O.Val)
    (s : Store O.Store) (a : Exec.Answer O.Val O.Event (Store O.Store)) : Bool :=
  O.allows p vs (erase s) (eraseAnswer a) && decide (a.store.own = s.own)

/-- Equip a concrete C1 operand instance `O` with C5 ownership and loan instrumentation
(Q9.10, owner decision 2026-11-08 Q3). Values, expressions, types, primitives, events
and branch tests are shared with the erased concrete instance `O`. -/
abbrev instrumentOperands (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State) : Exec.Operands where
  Val := O.Val
  Expr := O.Expr
  Ty := O.Ty
  Store := Store O.Store
  Prim := O.Prim
  Event := O.Event
  unitVal := O.unitVal
  unitTy := O.unitTy
  isCopy := O.isCopy
  eval := instrumentEval O permit
  life := instrumentLife O types
  test := O.test
  allows := instrumentAllows O

/-- Execute one C1 machine step on the ownership-instrumented operand instance. -/
def stepInstrumented (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) :
    Option (Exec.Out (instrumentOperands O types permit)) :=
  Exec.step P c

section
variable {O : Exec.Operands} {types : Var → Option Descriptor}
  {permit : AList Var O.Val → O.Expr → State → Option State}

/-- Finite execution of a supplied machine transition. This separates the execution subject
from erasure: mutation of instrumentation may change transitions, never the expected concrete
semantics. Primitive answers obey the operand contract, exactly as in C1 `Step`/`Steps`. -/
inductive FiniteExecution {A : Exec.Operands}
    (advance : Exec.Config A → Option (Exec.Out A)) :
    Exec.Config A → List A.Event → Exec.Config A → Prop where
  | refl (c : Exec.Config A) : FiniteExecution advance c [] c
  | silent {c d e tr} : advance c = some (.next d) →
      FiniteExecution advance d tr e → FiniteExecution advance c tr e
  | prim {c d e tr p vs site st k a} :
      advance c = some (.request p vs site st k) →
      A.allows p vs st a = true → k a = some d →
      FiniteExecution advance d tr e → FiniteExecution advance c (a.label.events ++ tr) e

/-- Erase statement typing from the instrumented operand instance to `O`. -/
def eraseStmt : Exec.Stmt (instrumentOperands O types permit) → Exec.Stmt O
  | .skip => .skip
  | .bind x (e : O.Expr) site => .bind x e site
  | .assign x (e : O.Expr) site => .assign x e site
  | .seq s t => .seq (eraseStmt s) (eraseStmt t)
  | .block b s => .block b (eraseStmt s)
  | .defer f (args : List O.Expr) site => .defer f args site
  | .cond (c : O.Expr) s t site => .cond c (eraseStmt s) (eraseStmt t) site
  | .loop l c (t : O.Expr) b site => .loop l (eraseStmt c) t (eraseStmt b) site
  | .brk l => .brk l
  | .cont l => .cont l
  | .ret (e : O.Expr) site => .ret e site
  | .call dst f (args : List O.Expr) site => .call dst f args site
  | .prim dst (p : O.Prim) (args : List O.Expr) site => .prim dst p args site
  | .abort site => .abort site

/-- Erase a deferred cleanup call to `O`. -/
def eraseDeferred (d : Exec.Deferred (instrumentOperands O types permit)) : Exec.Deferred O :=
  ⟨d.f, (d.args : List O.Expr), d.site⟩

/-- Erase a continuation frame to `O`. -/
def eraseFrame : Exec.Frame (instrumentOperands O types permit) → Exec.Frame O
  | .seq t => .seq (eraseStmt t)
  | .scope b dom defers => .scope b dom (defers.map eraseDeferred)
  | .test l c t body site dom => .test l (eraseStmt c) t (eraseStmt body) site dom
  | .body l c t body site => .body l (eraseStmt c) t (eraseStmt body) site
  | .resume p => .resume p
  | .functionEnd => .functionEnd
  | .ret dst caller => .ret dst caller

/-- Erase machine control to `O`. -/
def eraseCtrl : Exec.Ctrl (instrumentOperands O types permit) → Exec.Ctrl O
  | .exec s => .exec (eraseStmt s)
  | .done => .done
  | .exiting e => .exiting e
  | .trapped k site => .trapped k site
  | .refused => .refused

/-- Erase an instrumented C1 configuration by projecting its store to `erase c.store`. -/
def eraseConfig (c : Exec.Config (instrumentOperands O types permit)) : Exec.Config O :=
  ⟨eraseCtrl c.ctrl, (c.locals : AList Var O.Val), erase c.store, c.frames.map eraseFrame⟩

/-- Erase an instrumented function declaration to `O`. -/
def eraseFn (fn : Exec.Fn (instrumentOperands O types permit)) : Exec.Fn O :=
  ⟨(fn.params : List (Var × O.Ty)), (fn.result : O.Ty), fn.block, eraseStmt fn.body⟩

/-- Erase an instrumented program to `O`. -/
def eraseProgram (P : Exec.Program (instrumentOperands O types permit)) : Exec.Program O :=
  ⟨List.map (fun e => (e.1, eraseFn e.2)) P.fns⟩

/-- Unconditional finite simulation from a configuration: every finite instrumented execution
has an erased C1 execution with the same ordered events and erased endpoint. No oracle, fuel,
checker acceptance or non-stuck premise restricts this protocol. -/
def FiniteSimulation (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (advance : Exec.Config (instrumentOperands O types permit) →
      Option (Exec.Out (instrumentOperands O types permit)))
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit)) : Prop :=
  ∀ tr c', FiniteExecution advance c tr c' →
    Exec.Steps (eraseProgram P) (eraseConfig c) tr (eraseConfig c')

/-- Erase a step label to `O`, preserving its emitted event. -/
def eraseLabel : Exec.Label (instrumentOperands O types permit) → Exec.Label O
  | .silent => .silent
  | .emit ev => .emit ev

/-- Erase a run ending to `O`. -/
def eraseEnding : Exec.Ending (instrumentOperands O types permit) → Exec.Ending O
  | .final f => .final f
  | .stuck => .stuck
  | .noAnswer => .noAnswer
  | .badAnswer => .badAnswer
  | .outOfFuel => .outOfFuel

/-- Erase a report, retaining its residual configuration and unused budget. -/
def eraseReport (r : Exec.Report (instrumentOperands O types permit)) : Exec.Report O :=
  ⟨r.events, eraseEnding r.ending, eraseConfig r.residual, r.fuel⟩

end

/-- Lift a concrete C1 oracle to the instrumented machine by querying the concrete store
and preserving the current ownership state across the environment's answer. -/
def liftOracle (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (ρ : Exec.Oracle O) : Exec.Oracle (instrumentOperands O types permit) :=
  fun hist p vs site s => (ρ hist p vs site (erase s)).map (liftAnswer s.own)

/-- Whether a C1 run ending is `stuck`. -/
def Ending.IsStuck {O : Exec.Operands} : Exec.Ending O → Bool
  | .stuck => true
  | _ => false

/-- A C1 run ending that is not `stuck` (`final`, `noAnswer`, `badAnswer` or `outOfFuel`). -/
def Ending.NotStuck {O : Exec.Operands} (e : Exec.Ending O) : Prop :=
  Ending.IsStuck e = false

/-- Local operand permission: when the concrete operand evaluation is not stuck,
the ownership transition does not refuse. -/
def EvalPermitted (O : Exec.Operands)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (L : AList Var O.Val) (s : Store O.Store) (e : O.Expr) : Prop :=
  O.eval L s.concrete e ≠ .stuck → (permit L e s.own).isSome = true

/-- Left-to-right argument permission: each argument whose concrete evaluation is not
stuck is permitted in the state left by the preceding arguments. The optional evaluator
is the one being tested, so substitutions also govern every intermediate store. -/
def ArgsPermitted (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (L : AList Var O.Val) (s : Store O.Store) (args : List O.Expr)
    (evaluate : AList Var O.Val → Store O.Store → O.Expr → Res (O.Val × Store O.Store) :=
      (instrumentOperands O types permit).eval) : Prop :=
  match args with
  | [] => True
  | e :: es =>
    EvalPermitted O permit L s e ∧
      ∀ v s₁, evaluate L s e = .ok (v, s₁) →
        ArgsPermitted O types permit L s₁ es evaluate

/-- Local lifecycle permission: when the concrete lifecycle transition succeeds,
the ownership lifecycle transition also succeeds. -/
def LifePermitted (O : Exec.Operands) (types : Var → Option Descriptor)
    (ev : Exec.Life) (s : Store O.Store) : Prop :=
  (O.life ev s.concrete).isSome = true → (life types ev s.own).isSome = true

/-- Result-binding permission for a call or primitive destination. -/
def BindDstPermitted (O : Exec.Operands) (types : Var → Option Descriptor)
    (dst : Option Var) (s : Store O.Store) : Prop :=
  match dst with
  | none => True
  | some x => LifePermitted O types (.bind x) s

/-- Call-return lifecycle permission: leaving the activation and binding the destination. -/
def ReturnPermitted (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (dst : Option Var) (s : Store O.Store) : Prop :=
  LifePermitted O types .leave s ∧
    ∀ s', (instrumentOperands O types permit).life .leave s = some s' →
      BindDstPermitted O types dst s'

/-- Statement-level non-refusal at the active control point. Operand evaluation,
including argument lists, uses the same optional evaluator as the machine dispatcher. -/
def StmtPermitted (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (P : Exec.Program (instrumentOperands O types permit))
    (L : AList Var O.Val) (s : Store O.Store) (active : Exec.Stmt (instrumentOperands O types permit))
    (evaluate : AList Var O.Val → Store O.Store → O.Expr → Res (O.Val × Store O.Store) :=
      (instrumentOperands O types permit).eval) : Prop :=
  match active with
  | .bind x e _ =>
    EvalPermitted O permit L s e ∧
      ∀ v s', evaluate L s e = .ok (v, s') →
        LifePermitted O types (.bind x) s'
  | .assign x e _ =>
    EvalPermitted O permit L s e ∧
      ∀ v s', evaluate L s e = .ok (v, s') →
        (AList.lookup L x).isSome = true →
          LifePermitted O types (.bind x) s'
  | .cond e _ _ _ => EvalPermitted O permit L s e
  | .ret e _ => EvalPermitted O permit L s e
  | .call _ f args _ =>
    ArgsPermitted O types permit L s args evaluate ∧
      ∀ vs s' fn,
        Exec.evalArgs { instrumentOperands O types permit with eval := evaluate } L s args =
          .ok (vs, s') →
        AList.lookup P.fns f = some fn → fn.params.length = vs.length →
          LifePermitted O types (.enter (fn.params.map Prod.fst)) s'
  | .prim x p args _ =>
    ArgsPermitted O types permit L s args evaluate ∧
      ∀ vs s' a,
        Exec.evalArgs { instrumentOperands O types permit with eval := evaluate } L s args =
          .ok (vs, s') →
        (instrumentOperands O types permit).allows p vs s' a = true →
          a.result.isOk = true →
            BindDstPermitted O types x a.store
  | _ => True

/-- Frame-level non-refusal when the current statement has completed normally.
A test frame uses the supplied evaluator to select its post-evaluation lifecycle state. -/
def DoneFramePermitted (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (L : AList Var O.Val) (s : Store O.Store) (active : Exec.Frame (instrumentOperands O types permit))
    (evaluate : AList Var O.Val → Store O.Store → O.Expr → Res (O.Val × Store O.Store) :=
      (instrumentOperands O types permit).eval) : Prop :=
  match active with
  | .scope _ dm [] => LifePermitted O types (.drop (Exec.dead dm L)) s
  | .test _ _ t _ _ dm =>
    EvalPermitted O permit L s t ∧
      ∀ v s', evaluate L s t = .ok (v, s') →
        (O.test v).isSome = true →
          LifePermitted O types (.drop (Exec.dead dm L)) s'
  | .functionEnd => LifePermitted O types .leave s
  | .ret dst _ => BindDstPermitted O types dst s
  | _ => True

/-- Frame-level non-refusal when a structured exit is unwinding. -/
def ExitFramePermitted (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (L : AList Var O.Val) (s : Store O.Store) (e : Exec.Exit O.Val) :
    Exec.Frame (instrumentOperands O types permit) → Prop
  | .scope _ dm [] => LifePermitted O types (.drop (Exec.dead dm L)) s
  | .test _ _ _ _ _ dm => LifePermitted O types (.drop (Exec.dead dm L)) s
  | .functionEnd => LifePermitted O types .leave s
  | .ret dst _ => match e with
    | .ret _ => BindDstPermitted O types dst s
    | _ => True
  | _ => True

/-- Single-step non-refusal at a configuration: the ownership and lifecycle guards of
the active control point do not refuse when the concrete operation is defined. -/
def StepPermitted (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (P : Exec.Program (instrumentOperands O types permit))
    (c : Exec.Config (instrumentOperands O types permit))
    (evaluate : AList Var O.Val → Store O.Store → O.Expr → Res (O.Val × Store O.Store) :=
      (instrumentOperands O types permit).eval) : Prop :=
  match c.ctrl with
  | .exec s => StmtPermitted O types permit P c.locals c.store s evaluate
  | .done => match c.frames with
    | f :: _ => DoneFramePermitted O types permit c.locals c.store f evaluate
    | [] => True
  | .exiting e => match c.frames with
    | f :: _ => ExitFramePermitted O types permit c.locals c.store e f
    | [] => True
  | .trapped .. => True
  | .refused => False

/-- Obligation (discharged by the ownership checker, slice 1, R-0571).
A program and initial configuration accepted by the C5/C9 ownership and borrow checker
maintain a well-formed ownership state and never fail an ownership or lifecycle guard at
any reachable configuration where the concrete operation is defined. The default closure is
`Exec.Steps P`; substitution controls supply the closure of the same mutated transition
used in their conclusion, together with its operand evaluator for local permission.
Lifecycle-only obligations do not evaluate operands and need no evaluator parameter.
Authority: docs/decisions.md#decided-2026-11-08-recovered-d3 (C5) and
#decided-2026-11-14-trap-sites-and-law-tests (uniform substitution). -/
def CheckerAccepted (O : Exec.Operands) (types : Var → Option Descriptor)
    (permit : AList Var O.Val → O.Expr → State → Option State)
    (P : Exec.Program (instrumentOperands O types permit))
    (c₀ : Exec.Config (instrumentOperands O types permit))
    (reachable : Exec.Config (instrumentOperands O types permit) → List O.Event →
      Exec.Config (instrumentOperands O types permit) → Prop := Exec.Steps P)
    (evaluate : AList Var O.Val → Store O.Store → O.Expr → Res (O.Val × Store O.Store) :=
      (instrumentOperands O types permit).eval) : Prop :=
  ∀ tr c, reachable c₀ tr c → WellFormed c.store.own ∧ StepPermitted O types permit P c evaluate

/-- Typed C4 reads and writes use exactly the original Hybrid operations after permission. -/
def readOwner (tgt : Target) (p : Hybrid.Place) (s : Store Hybrid.State) :
    Option (Hybrid.Value × Store Hybrid.State) :=
  instrument (fun own => accessOwner own p .shared)
    (fun st => (st.read tgt p).map fun v => (v, st)) s

def writeOwner (tgt : Target) (p : Hybrid.Place) (v : Hybrid.Value)
    (s : Store Hybrid.State) : Option (Unit × Store Hybrid.State) :=
  instrument (fun own => accessOwner own p .mutable)
    (fun st => (st.write tgt p v).map fun st => ((), st)) s

/-- C4 currently resolves only whole scalar cells. A nonempty projection remains a logical
access permission; its concrete operation waits for the aggregate resolver, rather than reading
or overwriting the entire root as if it were that projection. -/
def readLoan (tgt : Target) (i : LoanId) (s : Store Hybrid.State) :
    Option (Hybrid.Value × Store Hybrid.State) := do
  let l ← loan s.own i
  if l.path = [] then
    instrument (fun own => accessLoan own i .shared)
      (fun st => (st.read tgt l.root).map fun v => (v, st)) s
  else none

def writeLoan (tgt : Target) (i : LoanId) (v : Hybrid.Value) (s : Store Hybrid.State) :
    Option (Unit × Store Hybrid.State) := do
  let l ← loan s.own i
  if l.path = [] then
    instrument (fun own => accessLoan own i .mutable)
      (fun st => (st.write tgt l.root v).map fun st => ((), st)) s
  else none


/-- Owner permission judgment (Q3.4, OD15): the read/write check on a concrete ledger/root/mode. -/
def OwnerPermitted (input : State × Hybrid.Place × Mode) : Prop :=
  ownerAccess input.1 input.2.1 input.2.2 = true

/-- Loan permission judgment (Q3.2, Q3.4, OD23): lending freezes apply through aliases too. -/
def LoanPermitted (input : State × LoanId × Mode) : Prop :=
  loanAccess input.1 input.2.1 input.2.2 = true

/-- Ending judgment (Q3.2): only a live loan without live children may end. -/
def EndPermitted (input : State × LoanId) : Prop :=
  (endLoan input.1 input.2).isSome = true

/-- Consuming judgment (OD60, OD62, Q3.8): consume an available unborrowed linear owner once. -/
def MovePermitted (input : State × Hybrid.Place) : Prop :=
  (move input.1 input.2).isSome = true

/-- Scope-exit judgment (OD62, OD69, Q3.8): storage teardown cannot discard a linear
obligation or invalidate a live loan. The lifecycle-refusal record requires refusal here. -/
def DropPermitted (input : List Var × State) : Prop :=
  (drop input.1 input.2).isSome = true

/-- Function-exit judgment (OD69): end this activation's loans before tearing down
its storage, without consuming any remaining linear owner implicitly. Lifecycle refusal
is recorded in `docs/decisions.md#decided-2026-11-13-lifecycle-refusal`. -/
def LeavePermitted (s : State) : Prop := (leave s).isSome = true


end ConcreteSpec.Core.Own
