import ConcreteSpec.Spec.Slice0
import Concrete.SpecDiffRandom

/-!
Seeded slice-0 Core generation for the spec leg. Each slice brief must extend
`weightedProductions` here with its newly accepted constructs; this is the one production table
for this leg. Generated candidates remain `Slice0.Program`s whether or not `check` accepts them.
-/

namespace ConcreteSpecDiff.SpecGen
open ConcreteSpec ConcreteSpec.Slice0
open ConcreteSpecDiff.Random

inductive Production where
  | letLiteral | letCopy | addLiteral | addVariable | returnLiteral | returnVariable
  deriving DecidableEq, Repr

def Production.name : Production → String
  | .letLiteral => "let_literal"
  | .letCopy => "let_copy"
  | .addLiteral => "add_literal"
  | .addVariable => "add_variable"
  | .returnLiteral => "return_literal"
  | .returnVariable => "return_variable"

def weightedProductions : List (Nat × Production) :=
  [(4, .letLiteral), (2, .letCopy), (4, .addLiteral), (3, .addVariable),
   (1, .returnLiteral), (5, .returnVariable)]

structure Generated where
  program : Slice0.Program
  constructs : List String

def literal (r : Rng) : Slice0.Atom × Rng :=
  let (n, r') := below 32 r
  (.lit n, r')

def statementProductions : List (Nat × Production) :=
  weightedProductions.filter fun (_, p) => p != .returnLiteral && p != .returnVariable

def go (fuel : Nat) (previous : Var) (vars : List Var) (r : Rng) :
    List Slice0.Stmt × List Production × Rng :=
  match fuel with
  | 0 => ([], [], r)
  | fuel + 1 =>
    let (choice, r₁) := pick statementProductions r
    let production := choice.getD .addLiteral
    let (rhs, r₂) :=
      match production with
      | .letCopy => (Slice0.Expr.atom (.var previous), r₁)
      | .addVariable =>
        let (which, r') := below vars.length r₁
        (.add (.var previous) (.var (vars.getD which previous)), r')
      | _ =>
        let (a, r') := literal r₁
        (.add (.var previous) a, r')
    let x : Var := ⟨"v", vars.length⟩
    let (rest, productions, r₃) := go fuel x (vars ++ [x]) r₂
    (⟨x, rhs⟩ :: rest, production :: productions, r₃)

/-- One candidate, with no hidden retry: `seed` and `index` alone determine it. -/
def generate (seed : String) (index : Nat) : Generated :=
  let r := forSeed seed index
  let (count, r₁) := below 6 r
  if count = 0 then
    let (a, _) := literal r₁
    ⟨{ body := [], ret := a }, [Production.name .returnLiteral]⟩
  else
    let (a, r₂) := literal r₁
    let x : Var := ⟨"v", 0⟩
    let first : Slice0.Stmt := ⟨x, .atom a⟩
    let (rest, productions, _) := go (count - 1) x [x] r₂
    ⟨{ body := first :: rest, ret := .var (if rest.isEmpty then x else ⟨"v", count - 1⟩) },
      (Production.name .letLiteral) ::
        (productions.map Production.name ++ [Production.name .returnVariable])⟩

end ConcreteSpecDiff.SpecGen
