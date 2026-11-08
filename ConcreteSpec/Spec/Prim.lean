import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Target
import ConcreteSpec.Spec.Authority

/-!
# Identities, primitive contracts, evidence and ghost erasure

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C9 (the foundation proposal approved by the owner
on 2026-11-08, "ok all recommended").

C1 leaves its operand expressions abstract. The first section below therefore supplies a minimal
explicit term language, including a binder, for the identity laws. Its `copy` and `move` forms have
the same value semantics here; their ownership distinction belongs to C5. The adapter `operands`
plugs this language into C1 without changing C1.

A renaming is a bijection of numeric binder identities. It changes binders and all their occurrences
together, so injectivity prevents capture. Source-name hints are presentation only and are treated
by a separate transformation.
-/

namespace ConcreteSpec.Core
namespace Prim

/-- A bijection of numeric binder identities, with both inverse laws available to proofs. -/
structure Renaming where
  toFun : Nat → Nat
  inv : Nat → Nat
  toFun_inv : ∀ n, toFun (inv n) = n
  inv_toFun : ∀ n, inv (toFun n) = n

/-- The identity renaming. -/
def Renaming.id : Renaming where
  toFun := _root_.id
  inv := _root_.id
  toFun_inv := fun _ => rfl
  inv_toFun := fun _ => rfl

/-- The inverse renaming. -/
def Renaming.symm (r : Renaming) : Renaming where
  toFun := r.inv
  inv := r.toFun
  toFun_inv := r.inv_toFun
  inv_toFun := r.toFun_inv

/-- The minimal operand language used by the identity laws: integer literals, local reads, explicit
`copy`/`move`, addition, and a lexically scoped binder. -/
inductive Term where
  | lit (n : Int)
  | var (x : Var)
  | copy (x : Var)
  | move (x : Var)
  | add (a b : Term)
  | bind (x : Var) (value body : Term)
  deriving DecidableEq, Repr

/-- Rename a binder identity without changing its presentation hint. -/
def renameVar (r : Renaming) (x : Var) : Var := { x with id := r.toFun x.id }

/-- Rename every binder and occurrence. Because the same injective map is used everywhere, a free
occurrence cannot be captured by a renamed binder. -/
def renameTerm (r : Renaming) : Term → Term
  | .lit n => .lit n
  | .var x => .var (renameVar r x)
  | .copy x => .copy (renameVar r x)
  | .move x => .move (renameVar r x)
  | .add a b => .add (renameTerm r a) (renameTerm r b)
  | .bind x value body => .bind (renameVar r x) (renameTerm r value) (renameTerm r body)

/-- Rename the keys of a local environment, preserving entry order and values. -/
def renameLocals (r : Renaming) (L : AList Var Int) : AList Var Int :=
  List.map (fun e => (renameVar r e.1, e.2)) L

/-- Change a local's presentation hint without changing its numeric identity. -/
def rehintVar (h : Var → String) (x : Var) : Var := { x with hint := h x }

/-- Change every presentation hint in a term, leaving its binders' numeric identities untouched. -/
def rehintTerm (h : Var → String) : Term → Term
  | .lit n => .lit n
  | .var x => .var (rehintVar h x)
  | .copy x => .copy (rehintVar h x)
  | .move x => .move (rehintVar h x)
  | .add a b => .add (rehintTerm h a) (rehintTerm h b)
  | .bind x value body => .bind (rehintVar h x) (rehintTerm h value) (rehintTerm h body)

/-- Rehint every key of a local environment, preserving entry order and values. -/
def rehintLocals (h : Var → String) (L : AList Var Int) : AList Var Int :=
  List.map (fun e => (rehintVar h e.1, e.2)) L

/-- Look up a numeric local identity. Presentation hints are not consulted. -/
def lookupId : AList Var Int → Nat → Option Int
  | [], _ => none
  | (x, v) :: L, n => if x.id = n then some v else lookupId L n

/-- Evaluate a term against locals. An unbound local is unsafe stuckness; this fragment adds no
failure of its own. -/
def Term.eval (L : AList Var Int) : Term → Res Int
  | .lit n => .ok n
  | .var x | .copy x | .move x => match lookupId L x.id with
    | some v => .ok v
    | none => .stuck
  | .add a b => (eval L a).bind fun x => (eval L b).map fun y => x + y
  | .bind x value body => (eval L value).bind fun v => eval ((x, v) :: L) body

/-- The minimal terms as C1 operands. There is no primitive operation, instrumentation or event in
this fragment; C9's registry below supplies primitive identities to richer operand languages. -/
def operands : Exec.Operands where
  Val := Int
  Expr := Term
  Ty := Unit
  Store := Unit
  Prim := Empty
  Event := Empty
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => true
  eval := fun L st e => (Term.eval L e).map fun v => (v, st)
  life := fun _ st => some st
  test := fun v => some (v ≠ 0)
  allows := fun _ _ _ _ => false

/-! ### Examples -/

/-- Accepted: a bound local evaluates to its value, including through the C1 adapter. -/
example : Term.eval [(⟨"x", 0⟩, 3)] (.var ⟨"x", 0⟩) = .ok 3 := rfl

example : operands.eval ([(⟨"x", 0⟩, 3)] : AList Var Int) () (.var ⟨"x", 0⟩) =
    (.ok ((3 : Int), ()) : Res (Int × Unit)) := rfl

/-- Refused: reading an unbound local is stuck, never a fabricated zero. -/
example : Term.eval [(⟨"x", 0⟩, 3)] (.var ⟨"y", 1⟩) = .stuck := rfl

/-- A binder scopes its body and does not erase the outer local after the body finishes. -/
example : Term.eval [(⟨"x", 0⟩, 1)]
    (.bind ⟨"y", 1⟩ (.lit 2) (.add (.var ⟨"x", 0⟩) (.var ⟨"y", 1⟩))) = .ok 3 := rfl

end Prim

/-! ## Primitive contracts and registries

A primitive is admitted only by its `PrimId` in a finite registry. Its contract contains both the
semantic predicates and an executable answer checker. The registry's completeness law proves the
checker accepts exactly the answers satisfying every predicate (Q9.7, Q9.13).

`ensures` deliberately receives only the result value and the *surviving* store, not entry
arguments, an entry store or a snapshot. At a function boundary it is evaluated after all deferred
cleanup (Q9.11, OD117, OD144). A consumed or cleaned parameter has no representation in that
surviving store.
Failures named by `failures` are values of the contracted result; C1's implicit trap answer is not
an allowed primitive result. Explicit `abort()` remains C1's separate terminal statement.
-/

/-- An externally relevant event classification used by a primitive contract. -/
structure EventTag where
  id : Nat
  deriving DecidableEq, Repr

/-- A leakage classification, separate from public events (C7). -/
structure LeakageTag where
  id : Nat
  deriving DecidableEq, Repr

/-- A typed error alternative carried by a primitive's ordinary result value. -/
structure Failure (Val Ty : Type) where
  errorTy : Ty
  holds : Val → Prop

/-- The complete interface of one primitive (Q9.6, Q9.7). `authority` is erased compile-time
capability data. Ownership, dependencies and resources are relations between the entry store and
the surviving store. `footprint` names logical places for C4/C10. `termination` is the obligation
under which an answer is admitted, not an evaluator-fuel claim. -/
structure PrimContract (Val Ty Store Event : Type) where
  id : ContractId
  parameters : List Ty
  result : Ty
  authority : List ConcreteSpec.Authority.Cap
  ownership : List Val → Store → Store → Prop
  dependencies : List Val → Store → Store → Prop
  requires : List Val → Store → Prop
  ensures : Val → Store → Prop
  footprint : List Val → Store → List PlaceId
  initialized : Val → Prop
  valid : Val → Prop
  failures : List (Failure Val Ty)
  events : Option Event → Prop
  eventTags : Option Event → List EventTag
  leakageTags : Option Event → List LeakageTag
  resources : List Val → Store → Store → Prop
  termination : List Val → Store → Prop
  answerOk : List Val → Store → Exec.Answer Val Event Store → Bool

/-- Every semantic condition an accepted answer must satisfy. A successful ordinary value satisfies
`ensures`; a typed failure instead matches a declared alternative with the contracted result type.
The common initialization, validity, ownership, dependency and resource conditions apply to both.
In particular, an implicit trap is never accepted. -/
def PrimContract.Allowed {Val Ty Store Event : Type} (c : PrimContract Val Ty Store Event)
    (args : List Val) (st : Store) (a : Exec.Answer Val Event Store) : Prop :=
  args.length = c.parameters.length ∧ c.requires args st ∧ c.termination args st ∧
    c.events a.event ∧
    match a.result with
    | .error _ => False
    | .ok v => (c.ensures v a.store ∨ ∃ f ∈ c.failures, f.errorTy = c.result ∧ f.holds v) ∧
        c.initialized v ∧ c.valid v ∧ c.ownership args st a.store ∧
        c.dependencies args st a.store ∧ c.resources args st a.store

/-- A finite registry with no catch-all entry. `answerOk_iff` is the completeness law for every
registered contract; `contractId` prevents filing one primitive's contract under another's key. -/
structure PrimRegistry (Val Ty Store Event : Type) where
  entries : AList PrimId (PrimContract Val Ty Store Event)
  contractId : ∀ p c, AList.lookup entries p = some c → c.id.prim = p
  answerOk_iff : ∀ p c, AList.lookup entries p = some c → ∀ args st a,
    c.answerOk args st a = true ↔ c.Allowed args st a

/-- Answer only through the contract registered for this exact primitive identity. -/
def PrimRegistry.allows {Val Ty Store Event : Type} (R : PrimRegistry Val Ty Store Event)
    (p : PrimId) (args : List Val) (st : Store) (a : Exec.Answer Val Event Store) : Bool :=
  match AList.lookup R.entries p with
  | none => false
  | some c => c.answerOk args st a

/-- Build C1 operands from an existing operand language and a registry. Only the primitive identity
and answer contract change; values, expressions, lifecycle and state remain those of the instance. -/
def PrimRegistry.operands (O : Exec.Operands) (R : PrimRegistry O.Val O.Ty O.Store O.Event) :
    Exec.Operands where
  Val := O.Val
  Expr := O.Expr
  Ty := O.Ty
  Store := O.Store
  Prim := PrimId
  Event := O.Event
  unitVal := O.unitVal
  unitTy := O.unitTy
  isCopy := O.isCopy
  eval := O.eval
  life := O.life
  test := O.test
  allows := R.allows

/-! ## Evidence binding

Evidence is accepted only for the exact context it was proved against. A context contains the typed
obligation identity, target profile, primitive-contract revision, checker version and specification
version (Q6.3, Q9.12, the pipeline's one evidence graph). The fact and its proof are present in the
record; matching a display name or a contract's current meaning is not enough.
-/

/-- The version of the checker that produced evidence. -/
structure CheckerVersion where
  major : Nat
  minor : Nat
  patch : Nat
  deriving DecidableEq, Repr

/-- Every independently changeable input to a safety proof. -/
structure EvidenceContext where
  obligation : ObligationId
  target : Target
  contract : ContractId
  checker : CheckerVersion
  spec : SpecVersion
  deriving DecidableEq, Repr

/-- A safety fact with its proof and the exact context in which that proof is valid. -/
structure SafetyEvidence where
  fact : Prop
  context : EvidenceContext
  proof : fact

/-- A fact is usable only when every component of its context matches the current one. -/
def SafetyEvidence.Usable (e : SafetyEvidence) (current : EvidenceContext) : Prop :=
  e.context = current

instance (e : SafetyEvidence) (current : EvidenceContext) : Decidable (e.Usable current) := by
  unfold SafetyEvidence.Usable
  infer_instance

namespace EvidenceDemo

/-- One current evidence context. -/
def current : EvidenceContext := ⟨⟨0⟩, linuxX8664, ⟨⟨0⟩, 1⟩, ⟨0, 1, 0⟩, specVersion⟩

/-- A proof of a trivial safety fact in exactly that context. -/
def evidence : SafetyEvidence := ⟨True, current, trivial⟩

/-- Accepted: every component matches. -/
example : evidence.Usable current := rfl

/-- Refused: a different contract revision invalidates the old evidence. -/
example : ¬ evidence.Usable { current with contract := ⟨⟨0⟩, 2⟩ } := by decide

/-- Refused: a different checker or specification also invalidates it. -/
example : ¬ evidence.Usable { current with checker := ⟨0, 2, 0⟩ } ∧
    ¬ evidence.Usable { current with spec := ⟨0, 2⟩ } := by decide

end EvidenceDemo

/-! ## Ghost statements and erasure

Ghost evaluation is total (OD115) and returns only ghost locals. It may read concrete locals and
store but has no way to return a concrete store, emit an event or request a primitive. Assertions
are propositions erased without execution (OD125). A ghost conditional's branches are themselves
only ghost statements, so no concrete branch depends on ghost state (OD81, Q9.10, Q9.11).

`Piece` composes these pure ghost statements with arbitrary C1 statements. The drivers below retain
the final configuration and remaining fuel that C1's `Report` does not expose. `drive` otherwise
has exactly C1's `runFrom` cases, including request refusal and all `Ending` classifications.
-/

namespace Ghost
variable {O : Exec.Operands}

/-- A total ghost expression language over one C1 operand language. `eval` can read ghost locals,
concrete locals and concrete store, but its result contains only a ghost value. -/
structure Language (O : Exec.Operands) where
  Val : Type
  Expr : Type
  eval : AList Var Val → AList Var O.Val → O.Store → Expr → Val
  test : Val → Bool

/-- Pure ghost statements. There is no concrete assignment, primitive request or authority field. -/
inductive Stmt {O : Exec.Operands} (G : Language O) where
  | assert (obligation : ObligationId) (fact : Prop)
  | bind (x : Var) (e : G.Expr) (site : SiteId)
  | seq (s t : Stmt G)
  | cond (e : G.Expr) (s t : Stmt G) (site : SiteId)

/-- Evaluate ghost statements, changing only ghost locals. Assertions disappear. -/
def evalStmt (G : Language O) (L : AList Var O.Val) (st : O.Store) :
    Stmt G → AList Var G.Val → AList Var G.Val
  | .assert .., Γ => Γ
  | .bind x e _, Γ => (x, G.eval Γ L st e) :: Γ
  | .seq s t, Γ => evalStmt G L st t (evalStmt G L st s Γ)
  | .cond e s t _, Γ => if G.test (G.eval Γ L st e) then evalStmt G L st s Γ else evalStmt G L st t Γ

/-- One component of a straight-line program: an arbitrary C1 statement or a pure ghost statement. -/
inductive Piece {O : Exec.Operands} (G : Language O) where
  | concrete (s : Exec.Stmt O)
  | ghost (s : Stmt G)

/-- A C1 run report together with the final configuration and unused fuel. -/
structure SegmentReport (O : Exec.Operands) where
  events : List O.Event
  ending : Exec.Ending O
  config : Exec.Config O
  remaining : Nat

/-- C1's `runFrom` with the final configuration and remaining fuel retained. -/
def drive (P : Exec.Program O) (ρ : Exec.Oracle O) :
    Nat → List O.Event → Exec.Config O → SegmentReport O
  | fuel, hist, c =>
    match c.final?, fuel with
    | some f, _ => ⟨hist, .final f, c, fuel⟩
    | none, 0 => ⟨hist, .outOfFuel c, c, 0⟩
    | none, n + 1 =>
      match Exec.step P c with
      | none => ⟨hist, .stuck c, c, n + 1⟩
      | some (.next c') => drive P ρ n hist c'
      | some (.request p vs site st k) =>
        match ρ hist p vs site st with
        | none => ⟨hist, .noAnswer c, c, n + 1⟩
        | some a =>
          if O.allows p vs st a then
            match k a with
            | some c' => drive P ρ n (hist ++ a.event.toList) c'
            | none => ⟨hist, .stuck c, c, n + 1⟩
          else ⟨hist, .badAnswer c, c, n + 1⟩

/-- Run a list of C1 statements, continuing only after ordinary completion. -/
def runStmts (P : Exec.Program O) (ρ : Exec.Oracle O) (fuel : Nat) (hist : List O.Event) :
    List (Exec.Stmt O) → Exec.Config O → SegmentReport O
  | [], c => drive P ρ fuel hist c
  | s :: ss, c =>
    match c.final? with
    | some .normal =>
      let r := drive P ρ fuel hist ⟨.exec s, c.locals, c.store, c.frames⟩
      match r.ending with
      | .final .normal => runStmts P ρ r.remaining r.events ss r.config
      | _ => r
    | _ => drive P ρ fuel hist c

/-- Run mixed ghost/concrete statements. Ghost evaluation consumes no concrete fuel and leaves the
C1 configuration untouched. -/
def runPieces (G : Language O) (P : Exec.Program O) (ρ : Exec.Oracle O)
    (fuel : Nat) (hist : List O.Event) :
    List (Piece G) → AList Var G.Val → Exec.Config O → SegmentReport O
  | [], _, c => drive P ρ fuel hist c
  | .ghost g :: ps, Γ, c =>
    match c.final? with
    | some .normal => runPieces G P ρ fuel hist ps (evalStmt G c.locals c.store g Γ) c
    | _ => drive P ρ fuel hist c
  | .concrete s :: ps, Γ, c =>
    match c.final? with
    | some .normal =>
      let r := drive P ρ fuel hist ⟨.exec s, c.locals, c.store, c.frames⟩
      match r.ending with
      | .final .normal => runPieces G P ρ r.remaining r.events ps Γ r.config
      | _ => r
    | _ => drive P ρ fuel hist c

variable {G : Language O}

/-- The concrete content of one piece: ghost statements have none. -/
def erasePiece : Piece G → Option (Exec.Stmt O)
  | .concrete s => some s
  | .ghost _ => none

/-- Erase every ghost statement, preserving every concrete statement in order. -/
def erase (ps : List (Piece G)) : List (Exec.Stmt O) := ps.filterMap erasePiece

end Ghost

namespace PrimDemo

/-- A minimal base language used to exercise registry admission, not a concrete language primitive. -/
abbrev base : Exec.Operands where
  Val := Int
  Expr := Unit
  Ty := Unit
  Store := Int
  Prim := Empty
  Event := Int
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => true
  eval := fun _ st _ => .ok (0, st)
  life := fun _ st => some st
  test := fun v => some (v ≠ 0)
  allows := fun _ _ _ _ => false

/-- A complete nullary contract whose success is `0`, whose sole declared failure is `1`, and whose
answers emit no event and preserve state. -/
def unitContract : PrimContract Int Unit Int Int where
  id := ⟨⟨0⟩, 1⟩
  parameters := []
  result := ()
  authority := []
  ownership := fun _ before after => after = before
  dependencies := fun _ _ _ => True
  requires := fun _ _ => True
  ensures := fun v _ => v = 0
  footprint := fun _ _ => []
  initialized := fun _ => True
  valid := fun _ => True
  failures := [⟨(), fun v => v = 1⟩]
  events := fun ev => ev = none
  eventTags := fun _ => []
  leakageTags := fun _ => []
  resources := fun _ before after => after = before
  termination := fun _ _ => True
  answerOk := fun args st a => match a.result with
    | .ok v => decide (args = [] ∧ a.event = none ∧ (v = 0 ∨ v = 1) ∧ a.store = st)
    | .error _ => false

/-- The registry containing exactly primitive zero. -/
def registry : PrimRegistry Int Unit Int Int where
  entries := [(⟨0⟩, unitContract)]
  contractId := by
    intro p c h
    by_cases hp : (⟨0⟩ : PrimId) = p
    · subst p
      simp [AList.lookup] at h
      cases h
      rfl
    · simp [AList.lookup, hp] at h
  answerOk_iff := by
    intro p c h
    by_cases hp : (⟨0⟩ : PrimId) = p
    · subst p
      simp [AList.lookup] at h
      cases h
      intro args st a
      cases a with
      | mk event result store => cases result <;> simp [PrimContract.Allowed, unitContract]
    · simp [AList.lookup, hp] at h

/-- C1 operands using that registry. -/
abbrev machine : Exec.Operands := registry.operands base

/-- Accepted: the registered primitive's successful answer, also through the C1 adapter. -/
example : registry.allows ⟨0⟩ [] 7 ⟨none, .ok 0, 7⟩ = true := by decide

example : machine.allows ⟨0⟩ [] (7 : Int)
    (⟨none, .ok (0 : Int), (7 : Int)⟩ : Exec.Answer Int Int Int) = true := by decide

/-- Accepted: `1` is the declared typed failure alternative. -/
example : registry.allows ⟨0⟩ [] 7 ⟨none, .ok 1, 7⟩ = true := by decide

/-- Refused: no unknown primitive receives the registered primitive's contract. -/
example : registry.allows ⟨1⟩ [] 7 ⟨none, .ok 0, 7⟩ = false := by decide

/-- Refused: `2` is neither a success nor a declared failure. -/
example : registry.allows ⟨0⟩ [] 7 ⟨none, .ok 2, 7⟩ = false := by decide

/-- Refused: an event or a state change violates the registered contract. -/
example : registry.allows ⟨0⟩ [] 7 ⟨some 0, .ok 0, 7⟩ = false := by decide

example : registry.allows ⟨0⟩ [] 7 ⟨none, .ok 0, 8⟩ = false := by decide

/-- Refused: an implicit trap is not a typed failure result. -/
example : registry.allows ⟨0⟩ [] 7 ⟨none, .error .abort, 7⟩ = false := by decide

end PrimDemo

namespace GhostDemo

/-- Ghost integer expressions read the concrete counter but never return a concrete store. -/
abbrev language : Ghost.Language PrimDemo.base where
  Val := Int
  Expr := Int
  eval := fun _ _ st n => n + st
  test := fun n => n ≠ 0

/-- An empty C1 program is enough for concrete statements that make no calls. -/
abbrev program : Exec.Program PrimDemo.base := ⟨[]⟩

/-- No environment answers are needed by this example. -/
abbrev oracle : Exec.Oracle PrimDemo.base := fun _ _ _ _ _ => none

/-- A completed concrete configuration with counter `5`. -/
abbrev start : Exec.Config PrimDemo.base := ⟨.done, [], 5, []⟩

/-- Ghost computation, concrete `skip`, a ghost-only conditional, and an erased assertion. -/
def pieces : List (Ghost.Piece language) :=
  [.ghost (.bind ⟨"g", 0⟩ 1 ⟨0⟩), .concrete .skip,
   .ghost (.cond 0 (.assert ⟨0⟩ True) (.bind ⟨"h", 1⟩ 2 ⟨1⟩) ⟨2⟩),
   .ghost (.assert ⟨1⟩ (1 + 1 = 2))]

/-- Accepted: erasure preserves the complete run, including its concrete store and ending. -/
example : Ghost.runPieces language program oracle 20 [] pieces [] start =
    Ghost.runStmts program oracle 20 [] [.skip] start := rfl

/-- The ghost computation read the counter but did not write it. -/
example : (Ghost.runPieces language program oracle 20 [] pieces [] start).config.store = 5 := rfl

end GhostDemo
end ConcreteSpec.Core
