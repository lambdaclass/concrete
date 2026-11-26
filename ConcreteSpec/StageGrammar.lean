import ConcreteSpec.StageInterface

namespace ConcreteSpec.StageGrammar

open ConcreteSpec.StageInterface
open Concrete.LL1

abbrev SurfaceToken := Located TokenValue

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
  /-- Declarative derivation with exact ordered leaves, action result, and number of
  production and token visits. The cost is an index of the same derivation. -/
  inductive Derives {N : Type} (g : SharedGrammar N) (slice : Slice) :
      (n : N) → List SurfaceToken → g.value n → Nat → Prop where
    | apply (production : Production g.value) (tokens : List SurfaceToken)
        (args : ActionArgs g.value production.rhs) {steps : Nat} :
        production ∈ g.productions → production.firstSlice.val ≤ slice.val →
        DerivesArgs g slice production.rhs tokens args steps →
        Derives g slice production.lhs tokens (production.action args) (steps + 1)

  inductive DerivesArgs {N : Type} (g : SharedGrammar N) (slice : Slice) :
      (rhs : List (Sym Terminal N)) → List SurfaceToken → ActionArgs g.value rhs → Nat → Prop where
    | nil : DerivesArgs g slice [] [] () 0
    | token {kind : Terminal} {token : SurfaceToken} {rhs : List (Sym Terminal N)}
        {tokens : List SurfaceToken} {args : ActionArgs g.value rhs} {steps : Nat} :
        token.value.kind = kind → DerivesArgs g slice rhs tokens args steps →
        DerivesArgs g slice (.t kind :: rhs) (token :: tokens) (token, args) (steps + 1)
    | child {n : N} {rhs : List (Sym Terminal N)} {head tail : List SurfaceToken}
        {value : g.value n} {args : ActionArgs g.value rhs} {left right : Nat} :
        Derives g slice n head value left → DerivesArgs g slice rhs tail args right →
        DerivesArgs g slice (.n n :: rhs) (head ++ tail) (value, args) (left + right)
end

mutual
  /-- Predictive visits also name the exact table row for the lookahead at entry.
  The unconsumed suffix matters, including for nullable nonterminals. -/
  inductive Predictive {N : Type} {g : SharedGrammar N} (table : CheckedTable g) (slice : Slice) :
      (n : N) → List SurfaceToken → List SurfaceToken → g.value n → Nat → Prop where
    | apply (p : Production g.value) (index : Nat) (tokens rest : List SurfaceToken)
        (args : ActionArgs g.value p.rhs) {steps : Nat} :
        g.productions[index]? = some p →
        table.lookup p.lhs ((tokens ++ rest).head?.map fun t => t.value.kind) = some index →
        p.firstSlice.val ≤ slice.val →
        PredictiveArgs table slice p.rhs tokens rest args steps →
        Predictive table slice p.lhs tokens rest (p.action args) (steps + 1)

  inductive PredictiveArgs {N : Type} {g : SharedGrammar N} (table : CheckedTable g)
      (slice : Slice) : (rhs : List (Sym Terminal N)) → List SurfaceToken → List SurfaceToken →
        ActionArgs g.value rhs → Nat → Prop where
    | nil (rest : List SurfaceToken) : PredictiveArgs table slice [] [] rest () 0
    | token {kind : Terminal} {token : SurfaceToken} {rhs : List (Sym Terminal N)}
        {tokens rest : List SurfaceToken} {args : ActionArgs g.value rhs} {steps : Nat} :
        token.value.kind = kind → PredictiveArgs table slice rhs tokens rest args steps →
        PredictiveArgs table slice (.t kind :: rhs) (token :: tokens) rest (token, args) (steps + 1)
    | child {n : N} {rhs : List (Sym Terminal N)} {head tail rest : List SurfaceToken}
        {value : g.value n} {args : ActionArgs g.value rhs} {left right : Nat} :
        Predictive table slice n head (tail ++ rest) value left →
        PredictiveArgs table slice rhs tail rest args right →
        PredictiveArgs table slice (.n n :: rhs) (head ++ tail) rest (value, args) (left + right)
end

def SharedGrammar.Productive {N : Type} (g : SharedGrammar N) : Prop :=
  ∀ p ∈ g.productions, ∃ tokens args steps, DerivesArgs g ⟨21, by decide⟩ p.rhs tokens args steps

/-- A charged computation is interpreted once for both execution and cost.
Each `tick` is one transition of the verifier or parser machine. -/
inductive Work (A : Type) where
  | done (result : A)
  | tick (next : Unit → Work A)

def Work.run {A : Type} : Work A → Nat × A
  | .done result => (0, result)
  | .tick next => let (steps, result) := (next ()).run; (steps + 1, result)

def SharedGrammar.size {N : Type} (g : SharedGrammar N) : Nat :=
  g.productions.foldl (fun size p => size + 1 + p.rhs.length) 0

structure TableVerifier {N : Type} (g : SharedGrammar N) where
  Proposal : Type
  verify : Proposal → Work (Option (CheckedTable g))
  terminals : List Terminal
  terminalCoverage : ∀ t, t ∈ terminals
  coefficient : Nat
  bounded : ∀ proposal, (verify proposal).run.1 ≤
    coefficient * (g.size + 1) * (terminals.length + 1)
  productive : ∀ proposal table, (verify proposal).run.2 = some table →
    g.Productive ∧ g.size ≤ (verify proposal).run.1

/-- Every invocation uses the registered table. Result and measured transitions come from
the same computation, including on refusal. The coefficient is fixed for the parser. -/
structure Parser {N : Type} (g : SharedGrammar N) where
  table : CheckedTable g
  verifier : TableVerifier g
  proposal : verifier.Proposal
  verified : (verifier.verify proposal).run.2 = some table
  parse : CheckedTable g → ParserView → Work (Except ParseRefusal (Parsed (g.value g.start)))
  stepsPerToken : Nat

def Parser.bound {N : Type} {g : SharedGrammar N} (parser : Parser g) (input : ParserView) : Nat :=
  parser.stepsPerToken * (input.tokens.size + 1)

def Parser.run {N : Type} {g : SharedGrammar N} (parser : Parser g) (input : ParserView) :
    Nat × Except ParseRefusal (Parsed (g.value g.start)) :=
  (parser.parse parser.table input).run

def Parses {N : Type} {g : SharedGrammar N} (parser : Parser g) (input : ParserView)
    (output : Parsed (g.value g.start)) : Prop :=
  output.source = input.source ∧
  ∃ steps, Derives g input.source.slice g.start input.tokens.toList output.tree steps ∧
    Predictive parser.table input.source.slice g.start input.tokens.toList [] output.tree steps ∧
    steps ≤ (parser.run input).1

def Refuses {N : Type} (g : SharedGrammar N) (input : ParserView) : ParseRefusal → Prop
  | .unsupported _ _ required =>
    input.source.slice.val < required.val ∧
    (∃ tree steps, Derives g required g.start input.tokens.toList tree steps) ∧
    ∀ earlier, earlier.val < required.val →
      ¬ ∃ tree steps, Derives g earlier g.start input.tokens.toList tree steps
  | .malformed _ _ =>
    ¬ ∃ tree required steps, Derives g required g.start input.tokens.toList tree steps

end ConcreteSpec.StageGrammar
