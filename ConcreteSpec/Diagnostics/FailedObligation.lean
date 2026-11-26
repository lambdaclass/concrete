import ConcreteSpec.Spec.Slice1Semantics

/-!
# Failed-obligation diagnostics (R-0599)

Search runs after a checker refusal. Its candidate generator and path constraints can suggest
inputs; only replay with the reference semantics can produce evidence. An unsuccessful search
makes no claim about the obligation. The slice-1 adapter exercises the closed case, with no
parameters, external events, or resource inputs beyond evaluator fuel.

The production consumer is the typed refusal from step 4(c), with source origins from 4(d)
and replay against the integrated Core from 4(e). The manifest control depends on the fixture
format in `origin/wip/typed-fixtures:tests/manifest.tsv` and its B4 extension for this payload.
The kernel controls here exercise the current slice-1 evaluator directly.
-/

namespace ConcreteSpec.FailedObligation

inductive SearchResult (Evidence : Type) where
  | refutedReplayed (evidence : Evidence)
  | noWitness (casesChecked : Nat)
  deriving DecidableEq, Repr

/-- The two R-0599 statuses, shared by diagnostic renderers. -/
def SearchResult.message {Evidence : Type} : SearchResult Evidence → String
  | .refutedReplayed _ => "refuted, replayed"
  | .noWitness n => s!"no witness within {n} cases"

/-- Put the type extremes first, then in-range path constants in their original order,
with duplicates removed. -/
def boundaryValues (lower upper : Int) (constants : List Int) : Array Int :=
  ((lower :: upper :: constants).filter (fun n => lower ≤ n && n ≤ upper)).eraseDups.toArray

/-- Index a Cartesian product without constructing it. The first axis varies fastest;
the empty product contains exactly one input (the closed program's empty argument list). -/
private def candidateAt {Input : Type} : List (Array Input) → Nat → Option (List Input)
  | [], index => if index = 0 then some [] else none
  | axis :: rest, index => do
    let value ← axis[index % axis.size]?
    let values ← candidateAt rest (index / axis.size)
    pure (value :: values)

/-- The budget counts candidates examined, including ones excluded by path constraints.
The result records the actual number examined when the product runs out first. -/
def search {Input Evidence : Type} (axes : List (Array Input)) (budget : Nat)
    (onPath : List Input → Bool) (replay : List Input → Option Evidence) : SearchResult Evidence :=
  go 0 budget
where
  go (index : Nat) : Nat → SearchResult Evidence
    | 0 => .noWitness index
    | remaining + 1 =>
      match candidateAt axes index with
      | none => .noWitness index
      | some input =>
        match (if onPath input then replay input else none) with
        | some evidence => .refutedReplayed evidence
        | none => go (index + 1) remaining

/-- Missing range obligation for one checked addition in a closed slice-1 program. The statement
index is zero-based within the exact semantic program supplied to replay. Source origins come
from the typed refusal, since this index is not a lexical site. -/
structure ClosedAdd where
  statementIndex : Nat
  type : IntTy
  deriving DecidableEq, Repr

structure AddWitness where
  left : Int
  right : Int
  deriving DecidableEq, Repr

structure ClosedAddReport where
  obligation : ClosedAdd
  caseBudget : Nat
  replayFuel : Nat
  result : SearchResult AddWitness
  deriving DecidableEq, Repr

/-- Replay the prefix under the same statement semantics as a full run. A prior trap, stuck
state, out-of-fuel run, or wrong target statement cannot witness this addition's failure. -/
private def reachStatement (decls : Decls) :
    Nat → Nat → Env → List Stmt → Option (Env × Stmt)
  | _ + 1, 0, env, stmt :: _ => some (env, stmt)
  | fuel + 1, index + 1, env, stmt :: rest =>
    match execStmt decls env stmt with
    | .ok next => reachStatement decls fuel index next rest
    | _ => none
  | _, _, _, _ => none

/-- Replay one closed program from the empty environment. Record the actual operands only
when the selected addition traps in the evaluator with operands in the obligation's type. -/
def replayClosedAdd (program : Program) (obligation : ClosedAdd) (fuel : Nat) : Option AddWitness := do
  let (env, stmt) ← reachStatement program.decls fuel obligation.statementIndex [] program.body
  let .let_ _ _ (.add left right) := stmt | none
  let .ok (.int leftTy a, next) := evalAtom program.decls env left | none
  let .ok (.int rightTy b, _) := evalAtom program.decls next right | none
  if leftTy = obligation.type && rightTy = obligation.type &&
      obligation.type.inRange a && obligation.type.inRange b then
    match execStmt program.decls env stmt with
    | .trap .arithmeticOverflow => some ⟨a, b⟩
    | _ => none
  else none

/-- Produce the diagnostic payload for a closed addition whose range obligation failed. -/
def diagnoseClosedAdd (program : Program) (obligation : ClosedAdd)
    (caseBudget replayFuel : Nat) : ClosedAddReport :=
  { obligation, caseBudget, replayFuel
    result := search ([] : List (Array Int)) caseBudget (fun _ => true)
      (fun _ => replayClosedAdd program obligation replayFuel) }

end ConcreteSpec.FailedObligation
