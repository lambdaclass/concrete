import ConcreteSpec.StageInterface

namespace ConcreteSpec.StageGrammar

open ConcreteSpec.StageInterface
open Concrete.LL1

abbrev Terminal := ConcreteSpec.Surface0.Terminal
abbrev SurfaceToken := Located ConcreteSpec.Surface0.TokenValue

/-- The semantic type of a rhs follows its exact symbols, including token payloads. -/
def ActionArgs {N : Type} (value : N → Type) : List (Sym Terminal N) → Type
  | [] => Unit
  | .t _ :: rhs => SurfaceToken × ActionArgs value rhs
  | .n n :: rhs => value n × ActionArgs value rhs

structure Production {N : Type} (value : N → Type) where
  lhs : N
  rhs : List (Sym Terminal N)
  firstSlice : Slice
  action : ActionArgs value rhs → value lhs

structure SharedGrammar (N : Type) where
  value : N → Type
  start : N
  productions : List (Production value)

def SharedGrammar.grammar {N : Type} (g : SharedGrammar N) : Grammar Terminal N :=
  ⟨g.start, g.productions.map fun p => (p.lhs, p.rhs)⟩

/-- Rows name a production, so the executable parser and the typed action share the same lookup. -/
structure CheckedTable {N : Type} (g : SharedGrammar N) where
  lookup : N → Option Terminal → Option Nat
  predicts : ∀ n look i, lookup n look = some i ↔
    Predicts g.grammar i look ∧ ∃ rhs, g.grammar.prods[i]? = some (n, rhs)
  rank : N → Nat
  -- Entering a nullable prefix consumes no lookahead and must decrease rank.
  acyclic : ∀ p ∈ g.productions, ∀ before child after,
    p.rhs = before ++ .n child :: after → NullableSeq g.grammar before →
    rank child < rank p.lhs

mutual
  /-- Declarative derivation with the exact ordered token leaves and typed action result. -/
  inductive Derives {N : Type} (g : SharedGrammar N) (slice : Slice) :
      (n : N) → List SurfaceToken → g.value n → Prop where
    | apply (production : Production g.value) (tokens : List SurfaceToken)
        (args : ActionArgs g.value production.rhs) :
        production ∈ g.productions → production.firstSlice.val ≤ slice.val →
        DerivesArgs g slice production.rhs tokens args →
        Derives g slice production.lhs tokens (production.action args)

  inductive DerivesArgs {N : Type} (g : SharedGrammar N) (slice : Slice) :
      (rhs : List (Sym Terminal N)) → List SurfaceToken → ActionArgs g.value rhs → Prop where
    | nil : DerivesArgs g slice [] [] ()
    | token {kind : Terminal} {token : SurfaceToken} {rhs : List (Sym Terminal N)}
        {tokens : List SurfaceToken} {args : ActionArgs g.value rhs} :
        token.value.kind = kind → DerivesArgs g slice rhs tokens args →
        DerivesArgs g slice (.t kind :: rhs) (token :: tokens) (token, args)
    | child {n : N} {rhs : List (Sym Terminal N)} {head tail : List SurfaceToken}
        {value : g.value n} {args : ActionArgs g.value rhs} :
        Derives g slice n head value → DerivesArgs g slice rhs tail args →
        DerivesArgs g slice (.n n :: rhs) (head ++ tail) (value, args)
end

def SharedGrammar.Productive {N : Type} (g : SharedGrammar N) : Prop :=
  ∀ p ∈ g.productions, ∃ tokens args, DerivesArgs g ⟨21, by decide⟩ p.rhs tokens args

/-- The cost witness counts actual verifier operations on supplied table proposals. -/
structure TableVerifier (N : Type) where
  Proposal : Type
  verify : (g : SharedGrammar N) → Proposal → Option (CheckedTable g)
  steps : SharedGrammar N → Proposal → Nat
  terminals : List Terminal
  terminalCoverage : ∀ t, t ∈ terminals
  coefficient : Nat
  bounded : ∀ g proposal, steps g proposal ≤
    coefficient * (g.productions.length + 1) * (terminals.length + 1)
  productive : ∀ g proposal table, verify g proposal = some table → g.Productive

structure ParserContract {N : Type} {g : SharedGrammar N} (table : CheckedTable g) where
  parse : CheckedTable g → ParserView → Except ParseRefusal (Parsed (g.value g.start))
  sound : ∀ input output, parse table input = .ok output →
    output.source = input.source ∧
    Derives g input.source.slice g.start input.tokens.toList output.tree
  unsupported : ∀ input rule span required,
    parse table input = .error (.unsupported rule span required) →
    input.source.slice.val < required.val ∧
    (∃ tree, Derives g required g.start input.tokens.toList tree) ∧
    (∀ earlier, earlier.val < required.val →
      ¬ ∃ tree, Derives g earlier g.start input.tokens.toList tree)
  malformed : ∀ input rule span, parse table input = .error (.malformed rule span) →
    ¬ ∃ tree required, Derives g required g.start input.tokens.toList tree
  steps : ParserView → Nat
  stepBound : ParserView → Nat
  bounded : ∀ input, steps input ≤ stepBound input

end ConcreteSpec.StageGrammar
