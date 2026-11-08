import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Exec

/-!
# C5: ownership instrumentation

PROPOSED, before Core freeze. Q3.2–Q3.4, OD15 and OD23. Roots reuse `Hybrid.Place`,
not addresses or a third place identity. The monotonically increasing place supply distinguishes
recursive activations. A slot carries its type and checked Copy/mutability facts; C9 must bind
those facts to its richer type identity when the scalar-only Hybrid universe is extended.

This executable reference model validates a candidate transition against the finite invariant.
Validation refuses the entire transition; it never repairs slots, ends loans or changes values.
Parent links record lending dependencies only: no stacked/tree borrow validity model is assumed.
Projection paths are logical selectors; resolving their types/values belongs to aggregate slices.
-/
namespace ConcreteSpec.Core.Own

inductive Status where
  | owned | moved
  deriving DecidableEq, Repr

inductive Mode where
  | shared | mutable
  deriving DecidableEq, Repr

/-- A logical projection, retaining field identity or element index (Q3.3). -/
inductive Projection where
  | field (id : FieldId)
  | index (n : Nat)
  deriving DecidableEq, Repr

/-- The checked binding descriptor supplied by the operand instance, not guessed from a value.
`ty` describes its C4 scalar payload representation: a linear token may have an integer or
zero-byte payload, without becoming Copy. Semantic Copy/mutability evidence and the richer
nominal type identity are the operand instance/C9 admission obligation. -/
structure Descriptor where
  ty : CTy
  copy : Bool
  mutable : Bool
  deriving DecidableEq, Repr

structure Slot where
  place : Hybrid.Place
  activation : Nat
  desc : Descriptor
  status : Status
  deriving DecidableEq, Repr

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
  deriving DecidableEq, Repr

structure Activation where
  id : Nat
  locals : AList Var Hybrid.Place := []
  views : AList Var LoanId := []
  deriving DecidableEq, Repr

structure State where
  slots : List Slot := []
  loans : List Loan := []
  frames : List Activation := []
  nextPlace : Nat := 0
  nextLoan : Nat := 0
  nextActivation : Nat := 0
  deriving DecidableEq, Repr

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
loans are compatible. Supplies bound all issued identities, including moved slots. -/
def WellFormed (s : State) : Prop :=
  (s.slots.map Slot.place).Nodup ∧ (s.loans.map Loan.id).Nodup ∧
  (∀ x ∈ s.slots, x.place.id < s.nextPlace ∧ x.activation < s.nextActivation) ∧
  (∀ l ∈ s.loans,
    l.id.id < s.nextLoan ∧ l.activation < s.nextActivation ∧
    (∃ x ∈ s.slots, x.place = l.root ∧ x.status = .owned) ∧
    ParentValid s l) ∧
  (∀ a ∈ s.loans, ∀ b ∈ s.loans, Compatible a b)

instance (s : State) : Decidable (WellFormed s) := by
  unfold WellFormed
  infer_instance

/-- The reference transition checker: accept the candidate unchanged, or refuse it. -/
def validate (s : State) : Option State := if WellFormed s then some s else none

/-- Owner accesses cannot bypass whole-root loans. Reads may coexist with shared loans (Q3.4, OD15). -/
def ownerAccess (s : State) (p : Hybrid.Place) (m : Mode) : Bool :=
  (slot s p).any fun x => x.status == .owned &&
    (m == .shared || x.desc.mutable) && s.loans.all (fun l =>
      l.root != p || (m == .shared && l.mode == .shared))

/-- Access through a loan ignores its suspended ancestors. A conflicting child suspends access
through the parent until it ends; unrelated accesses still obey whole-root exclusion (Q3.2, Q3.4). -/
def loanAccess (s : State) (i : LoanId) (m : Mode) : Bool :=
  (loan s i).any fun a => (m == .shared || a.mode == .mutable) &&
    s.loans.all (fun b => b.id == i || decide (b.id ∈ a.ancestors) || b.root != a.root ||
      (m == .shared && b.mode == .shared))

/-- Move is only for non-Copy owners; it changes no concrete value. -/
def move (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if x.desc.copy || x.status != .owned || s.loans.any (·.root == p) then none else
    validate { s with slots := s.slots.map fun x =>
      if x.place = p then { x with status := .moved } else x }

/-- Reinitialization requires no active loan. Replacing an owned linear value is refused:
consume it first, rather than silently discarding its obligation. -/
def reinitialize (s : State) (p : Hybrid.Place) : Option State := do
  let x ← slot s p
  if (x.status == .moved || (x.desc.copy && x.desc.mutable)) &&
      s.loans.all (·.root != p) then
    validate { s with slots := s.slots.map fun x =>
      if x.place = p then { x with status := .owned } else x }
  else none

/-- Open an owner loan. The fresh identity is the old counter. -/
def borrow (s : State) (p : Hybrid.Place) (path : List Projection) (m : Mode) :
    Option State := do
  let f ← s.frames.head?
  if ownerAccess s p m then
    validate { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, p, path, m, none, [], f.id⟩ :: s.loans }
  else none

/-- Reborrow appends a projection, never changes the original root and never upgrades shared.
The explicit activation is the destination activation for a downward call. -/
def reborrowAt (s : State) (i : LoanId) (suffix : List Projection) (m : Mode)
    (activation : Nat) : Option State := do
  let a ← loan s i
  if loanAccess s i m then
    validate { s with
      nextLoan := s.nextLoan + 1
      loans := ⟨⟨s.nextLoan⟩, a.root, a.path ++ suffix, m, some i,
        i :: a.ancestors, activation⟩ :: s.loans }
  else none

def reborrow (s : State) (i : LoanId) (suffix : List Projection) (m : Mode) :
    Option State := do
  let f ← s.frames.head?
  reborrowAt s i suffix m f.id

/-- Children must end before parents (Q3.2); ending a loan neither destroys nor moves its owner. -/
def endLoan (s : State) (i : LoanId) : Option State := do
  let _ ← loan s i
  if s.loans.any (·.parent == some i) then none else
    validate { s with loans := s.loans.filter (·.id != i) }

/-- Read/write permissions are partial identity transitions on the instrumentation. Concrete
reads/writes are supplied separately by `instrument`; they cannot manufacture permission. -/
def accessOwner (s : State) (p : Hybrid.Place) (m : Mode) : Option State :=
  if ownerAccess s p m then validate s else none

def accessLoan (s : State) (i : LoanId) (m : Mode) : Option State :=
  if loanAccess s i m then validate s else none

/-- A descriptor is supplied by the checked operand language. A fresh binding gets a fresh place;
an assignment to an existing binding must discharge/reinitialize its previous obligation. -/
def bind (types : Var → Option Descriptor) (x : Var) (s : State) : Option State := do
  let f ← s.frames.head?
  match AList.lookup f.locals x with
  | some p => reinitialize s p
  | none =>
    let d ← types x
    validate { s with
      nextPlace := s.nextPlace + 1
      slots := ⟨⟨s.nextPlace⟩, f.id, d, .owned⟩ :: s.slots
      frames := { f with locals := (x, ⟨s.nextPlace⟩) :: f.locals } :: s.frames.tail }

/-- A local can leave scope only after its linear obligation is discharged and all its loans
end. Copy permits duplication, and does not justify discarding a non-Copy value (Q3.8). -/
def drop (xs : List Var) (s : State) : Option State := do
  let f ← s.frames.head?
  let ps := List.map Prod.snd (f.locals.filter fun e => decide (e.1 ∈ xs))
  if s.slots.all (fun x => decide (x.place ∉ ps) || x.desc.copy || x.status == .moved) &&
      s.loans.all (fun l => decide (l.root ∉ ps)) then
    validate { s with
      slots := s.slots.filter (fun x => decide (x.place ∉ ps))
      frames := { f with locals := f.locals.filter (fun e => decide (e.1 ∉ xs)) } :: s.frames.tail }
  else none

/-- Enter owns a new activation. Downward loans use `enterDown` below; ordinary values use bind. -/
def enter (types : Var → Option Descriptor) (params : List Var) (s : State) : Option State := do
  let s ← validate { s with
    nextActivation := s.nextActivation + 1
    frames := ⟨s.nextActivation, [], []⟩ :: s.frames }
  params.foldlM (fun s x => bind types x s) s

/-- End all loans created by this activation in reverse birth order (children first).
Loans are newest first. A loan of an outer root passed into this call is a callee child,
not the caller's parent loan. -/
def endLocals (activation : Nat) (s : State) : Option State :=
  (s.loans.filter (·.activation == activation)).foldlM (fun s l => endLoan s l.id) s

/-- Leave consumes no resource implicitly: all remaining local owners must be Copy or moved.
Local loans end before local roots are removed. -/
def leave (s : State) : Option State := do
  let f ← s.frames.head?
  let s ← endLocals f.id s
  let s ← drop (List.map Prod.fst f.locals) s
  validate { s with frames := s.frames.tail }

/-- Downward-only lending: each parameter gets a new child loan. Views never become owner slots,
so cannot be returned/stored/moved by the owner operations. This standalone call-boundary transition supports the preservation laws. C1 uses `callLife`
below to adopt loans that were opened earlier, during typed argument evaluation.
No reference-valued concrete storage is introduced. -/
def enterDown (s : State) (args : List (Var × LoanId)) : Option State := do
  let s ← enter (fun _ => none) [] s
  args.foldlM (fun s (x, i) => do
    let a ← loan s i
    let f ← s.frames.head?
    let s ← reborrowAt s i [] a.mode f.id
    validate { s with frames := { f with views := (x, ⟨s.nextLoan - 1⟩) :: f.views } :: s.frames.tail }) s

/-- The C1-compatible lifecycle callback. -/
def life (types : Var → Option Descriptor) : Exec.Life → State → Option State
  | .bind x, s => bind types x s
  | .enter ps, s => enter types ps s
  | .leave, s => leave s
  | .drop xs, s => drop xs s

/-- A call's view arguments have already opened their loans while arguments were evaluated,
so later arguments observe their freezes (OD23). Transfer only those fresh children to the
callee activation; do not reborrow again or move the caller's original view. -/
def adoptDown (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    validate { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := f.id } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Metadata for loan arguments waiting for C1's enter event. No reference is stored in a
concrete value: the callee parameter is associated with its loan only in this instrumentation. -/
structure CallState where
  own : State
  pending : List (Var × LoanId) := []
  deriving DecidableEq, Repr

/-- Evaluate a downward view argument now, before the next argument. Target binder identities
come from the checked call signature. Shared views may be passed repeatedly, each with a fresh
loan; a mutable view's first lending freezes it immediately. -/
def passDown (s : CallState) (source : LoanId) (parameter : Var) : Option CallState := do
  let l ← loan s.own source
  let own ← reborrow s.own source [] l.mode
  pure ⟨own, s.pending ++ [(parameter, ⟨s.own.nextLoan⟩)]⟩

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
  | e, s => (life types e s.own).map fun own => { s with own := own }

/-- The call adapter also carries an arbitrary concrete memory/resource store unchanged. -/
def callLifeStore {σ : Type} (types : Var → Option Descriptor) (e : Exec.Life)
    (s : σ × CallState) : Option (σ × CallState) :=
  (callLife types e s.2).map fun own => (s.1, own)

/-- Erasure forgets the instrumentation, keeping exactly the operand instance's concrete store. -/
structure Store (σ : Type) where
  concrete : σ
  own : State

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

namespace Examples

def x : Var := ⟨"x", 0⟩
def y : Var := ⟨"y", 1⟩
def types (_ : Var) : Option Descriptor := some ⟨.int .i32, false, true⟩
def initial : State :=
  { slots := [⟨⟨0⟩, 0, ⟨.int .i32, false, true⟩, .owned⟩]
    frames := [⟨0, [(x, ⟨0⟩)], []⟩]
    nextPlace := 1
    nextActivation := 1 }

def shared : State := (borrow initial ⟨0⟩ [.index 0] .shared).getD initial
def exclusive : State := (borrow initial ⟨0⟩ [.index 0] .mutable).getD initial
def child : State := (reborrow exclusive ⟨0⟩ [.index 1] .mutable).getD initial

/-- Accepted (rule `ConcreteSpec.Core.Own.ownerAccess`): shared loans allow owner reads. -/
example : ownerAccess shared ⟨0⟩ .shared = true := by decide
/-- Refused (rule `ConcreteSpec.Core.Own.ownerAccess`): shared loans freeze owner writes. -/
example : ownerAccess shared ⟨0⟩ .mutable = false := by decide
/-- Refused: shared loans also prevent moving the owner. -/
example : move shared ⟨0⟩ = none := by decide
/-- Accepted: shared loans of disjoint or overlapping projections coexist. -/
example : (borrow shared ⟨0⟩ [.index 1] .shared).isSome = true := by decide
/-- Refused: disjoint indices do not bypass mutable whole-root conflicts. -/
example : borrow exclusive ⟨0⟩ [.index 999] .shared = none := by decide
/-- Refused: an exclusive loan blocks even owner reads. -/
example : ownerAccess exclusive ⟨0⟩ .shared = false := by decide
/-- Accepted (rule `ConcreteSpec.Core.Own.loanAccess`): the mutable child accesses its root. -/
example : loanAccess child ⟨1⟩ .mutable = true := by decide
/-- Refused (rule `ConcreteSpec.Core.Own.loanAccess`): a mutable child suspends its parent. -/
example : loanAccess child ⟨0⟩ .shared = false := by decide
/-- Refused: shared lending cannot upgrade to mutable access. -/
example : reborrow shared ⟨0⟩ [] .mutable = none := by decide
/-- Refused: a shared child freezes writes through its mutable parent, but permits reads. -/
example : (do
    let s ← reborrow exclusive ⟨0⟩ [] .shared
    pure (loanAccess s ⟨0⟩ .shared, loanAccess s ⟨0⟩ .mutable)) = some (true, false) := by decide
/-- Accepted (rule `ConcreteSpec.Core.Own.endLoan`): end the child, then use the parent. -/
example : ((endLoan child ⟨1⟩).map fun s => loanAccess s ⟨0⟩ .mutable) = some true := by decide
/-- Refused (rule `ConcreteSpec.Core.Own.endLoan`): parents cannot end before their children. -/
example : endLoan child ⟨0⟩ = none := by decide
/-- Refused: moving twice or reading a moved slot; reinitializing makes it usable again. -/
example : (do
    let s ← move initial ⟨0⟩
    pure ((move s ⟨0⟩).isNone, ownerAccess s ⟨0⟩ .shared,
      (reinitialize s ⟨0⟩).isSome)) = some (true, false, true) := by decide
/-- Refused: leaving a live linear owner would silently discard its obligation. -/
example : leave initial = none := by decide
/-- Accepted: downward lending preserves the root; leaving restores the caller's parent access. -/
example : (do
    let s ← enterDown exclusive [(y, ⟨0⟩)]
    let frozen := loanAccess s ⟨0⟩ .shared
    let s ← leave s
    pure (frozen, loanAccess s ⟨0⟩ .mutable, s.nextLoan)) = some (false, true, 2) := by decide
/-- Refused: a root with a loan cannot leave scope. -/
example : drop [x] { shared with slots := shared.slots.map fun x =>
    { x with desc := { x.desc with copy := true } } } = none := by decide
/-- Accepted: identities are not reused even after the prior loan ends. -/
example : (do
    let s ← endLoan shared ⟨0⟩
    let s ← borrow s ⟨0⟩ [] .shared
    pure (s.loans.map Loan.id)) = some [⟨1⟩] := by decide

/-- Refused: a zero-byte representation still has a linear ownership obligation (Q3.8). -/
example : Hybrid.sizeof .unit = 0 ∧
    drop [x] { initial with slots := [⟨⟨0⟩, 0, ⟨.unit, false, true⟩, .owned⟩] } = none := by decide
/-- Accepted: explicitly consuming the zero-byte obligation permits its slot to end. -/
example : (do
    let s ← move { initial with slots := [⟨⟨0⟩, 0, ⟨.unit, false, true⟩, .owned⟩] } ⟨0⟩
    drop [x] s).isSome = true := by decide

/-- A small operand instance: all loan commands modify instrumentation only. There are no
reference-valued fields, returns or aggregate operands. -/
inductive E where
  | lit (n : Int)
  | move (x : Var)
  | read (x : Var)
  | lend (root : Hybrid.Place) (mode : Mode)
  | reborrow (parent : LoanId)
  | endLoan (id : LoanId)
  | use (id : LoanId)

def eval (locals : AList Var Int) (s : State) : E → Res (Int × State)
  | .lit n => .ok (n, s)
  | .move x => match s.frames.head?.bind (fun f => AList.lookup f.locals x),
      AList.lookup locals x with
    | some p, some n => match Own.move s p with
      | some s => .ok (n, s)
      | none => .stuck
    | _, _ => .stuck
  | .read x => match s.frames.head?.bind (fun f => AList.lookup f.locals x),
      AList.lookup locals x with
    | some p, some n => if ownerAccess s p .shared then .ok (n, s) else .stuck
    | _, _ => .stuck
  | .lend p m => match borrow s p [] m with
    | some s => .ok (0, s)
    | none => .stuck
  | .reborrow i => match Own.reborrow s i [] .mutable with
    | some s => .ok (0, s)
    | none => .stuck
  | .endLoan i => match Own.endLoan s i with
    | some s => .ok (0, s)
    | none => .stuck
  | .use i => if loanAccess s i .mutable then .ok (0, s) else .stuck

abbrev ops : Exec.Operands where
  Val := Int
  Expr := E
  Ty := Unit
  Store := State
  Prim := Empty
  Event := Empty
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => false
  eval := eval
  life := Own.life types
  test := fun n => some (n != 0)
  allows := fun p => nomatch p

def runBody (body : Exec.Stmt ops) : Exec.Report ops :=
  Exec.run ⟨[]⟩ (fun _ p => nomatch p) 100 ⟨.exec body, [(x, 7)], initial, []⟩

def stuck (r : Exec.Report ops) : Bool := match r.ending with
  | .stuck _ => true
  | _ => false

/-- Refused: an actual C1 run gets stuck when its next operand reads a moved owner. -/
example : stuck (runBody (.seq (.bind y (.move x) ⟨0⟩) (.ret (.read x) ⟨1⟩))) = true := by decide

/-- Accepted: a C1 run lends, reborrows, ends the child, then uses its parent and ends it.
Conditions evaluate these instrumentation-only operands without creating ordinary bindings. -/
example : (match (runBody
    (.cond (.lend ⟨0⟩ .mutable) .skip
      (.cond (.reborrow ⟨0⟩) .skip
        (.cond (.endLoan ⟨1⟩) .skip
          (.cond (.use ⟨0⟩) .skip
            (.cond (.endLoan ⟨0⟩) .skip .skip ⟨0⟩) ⟨0⟩) ⟨0⟩) ⟨0⟩) ⟨0⟩)).ending with
    | .final .normal => true
    | _ => false) = true := by decide

/-- The following C1 instance exercises actual calls with downward view parameters. -/
inductive CE where
  | value (e : E)
  | argument (source : LoanId) (parameter : Var)
  | useView (parameter : Var)

def callEval (locals : AList Var Int) (s : CallState) : CE → Res (Int × CallState)
  | .value e => (eval locals s.own e).map fun (v, own) => (v, { s with own := own })
  | .argument i x => match passDown s i x with
    | some s => .ok (0, s)
    | none => .stuck
  | .useView x => match s.own.frames.head?.bind (fun f => AList.lookup f.views x) with
    | some i => if loanAccess s.own i .mutable then .ok (0, s) else .stuck
    | none => .stuck

abbrev callOps : Exec.Operands where
  Val := Int
  Expr := CE
  Ty := Unit
  Store := CallState
  Prim := Empty
  Event := Empty
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => false
  eval := callEval
  life := callLife types
  test := fun n => some (n != 0)
  allows := fun p => nomatch p

def callee : FnId := ⟨⟨[]⟩, "callee"⟩
def callProgram : Exec.Program callOps :=
  ⟨[(callee, ⟨[(y, ())], (), ⟨1⟩,
    .cond (.useView y) .skip .skip ⟨0⟩⟩)]⟩

def callRun (body : Exec.Stmt callOps) : Exec.Report callOps :=
  Exec.run callProgram (fun _ p => nomatch p) 100
    ⟨.exec body, [(x, 7)], ⟨exclusive, []⟩, []⟩

/-- Accepted: a real C1 call lends the mutable parent to its parameter; return ends the child,
then the caller can use its original parent again. No reference is a returned/stored value. -/
example : (match (callRun (.seq (.call none callee [.argument ⟨0⟩ y] ⟨0⟩)
    (.cond (.value (.use ⟨0⟩)) .skip .skip ⟨0⟩))).ending with
    | .final .normal => true
    | _ => false) = true := by decide

/-- Refused: OD23 applies during argument evaluation, before entry. The second argument
cannot use the origin frozen by the first argument's mutable lending. -/
example : (match (callRun (.call none callee
    [.argument ⟨0⟩ y, .value (.use ⟨0⟩)] ⟨0⟩)).ending with
    | .stuck _ => true
    | _ => false) = true := by decide

end Examples
end ConcreteSpec.Core.Own
