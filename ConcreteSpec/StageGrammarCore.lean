import ConcreteSpec.StageInterface
import ConcreteSpec.Spec.GrammarData
import ConcreteSpec.Spec.LL1
import ConcreteSpec.Spec.TableVerifier
import ConcreteSpec.Spec.VerifyMachine

/-! Generic certified LL(1) grammar, verifier, and parser core.
Data structures, semantics, and executable cursor machine live here;
production grammar data lives in `ConcreteSpec/Spec/SliceGrammar.lean`
and closed selectors live in `ConcreteSpec/StageGrammar.lean`. -/

namespace ConcreteSpec.StageGrammar

open ConcreteSpec.StageInterface
open ConcreteSpec.Core (RuleId RegisteredRule)
open ConcreteSpec.GrammarData (ProductionId)
open Concrete.LL1

abbrev SurfaceToken := Located TokenValue

/-- Grammar actions are first-order constructor expressions. A fetched child or
token is shared, and the only way to traverse action syntax is the interpreter. -/
inductive Action : Type → Type → Type 1 where
  | constant {In Out : Type} (value : Out) : Action In Out
  | input {In : Type} : Action In In
  | left {In A B : Type} (pair : Action In (A × B)) : Action In A
  | right {In A B : Type} (pair : Action In (A × B)) : Action In B
  | pair {In A B : Type} (left : Action In A) (right : Action In B) : Action In (A × B)
  | some {In A : Type} (value : Action In A) : Action In (Option A)
  | cons {In A : Type} (head : Action In A) (tail : Action In (List A)) : Action In (List A)
  | inl {In A B : Type} (value : Action In A) : Action In (A ⊕ B)
  | inr {In A B : Type} (value : Action In B) : Action In (A ⊕ B)
  | node {In : Type} (tag : SurfaceNode) (fields : Action In (List SyntaxTree)) : Action In SyntaxTree
  | leaf {In : Type} (token : Action In SurfaceToken) : Action In SyntaxTree

/-- The semantic type of a rhs follows its exact symbols, including token payloads. -/
def ActionArgs {N : Type} (value : N → Type) : List (Sym Terminal N) → Type
  | [] => Unit
  | .t _ :: rhs => SurfaceToken × ActionArgs value rhs
  | .n n :: rhs => value n × ActionArgs value rhs

def Action.eval {In Out : Type} (input : In) : Action In Out → Nat × Out
  | .constant value => (1, value)
  | .input => (1, input)
  | .left expr => let (cost, value) := expr.eval input; (cost + 1, value.1)
  | .right expr => let (cost, value) := expr.eval input; (cost + 1, value.2)
  | .pair lhs rhs =>
    let (lc, lv) := lhs.eval input
    let (rc, rv) := rhs.eval input
    (lc + rc + 1, (lv, rv))
  | .some value => let (cost, v) := value.eval input; (cost + 1, Option.some v)
  | .cons head tail =>
    let (hc, hv) := head.eval input
    let (tc, tv) := tail.eval input
    (hc + tc + 1, hv :: tv)
  | .inl value => let (cost, v) := value.eval input; (cost + 1, Sum.inl v)
  | .inr value => let (cost, v) := value.eval input; (cost + 1, Sum.inr v)
  | .node tag fields => let (cost, values) := fields.eval input; (cost + 1, .node tag values)
  | .leaf token => let (cost, value) := token.eval input; (cost + 1, .leaf value)

/-- Static constructor-step count of a first-order action, independent of input. -/
def Action.cost {In Out : Type} : Action In Out → Nat
  | .constant _ | .input => 1
  | .left expr | .right expr | .some expr | .inl expr | .inr expr
  | .node _ expr | .leaf expr => expr.cost + 1
  | .pair lhs rhs | .cons lhs rhs => lhs.cost + rhs.cost + 1

theorem Action.eval_cost {In Out : Type} (input : In) (action : Action In Out) :
    (action.eval input).1 = action.cost := by
  induction action with
  | constant _ | input => rfl
  | left _ ih | right _ ih | some _ ih | inl _ ih | inr _ ih | node _ _ ih | leaf _ ih =>
    simp only [Action.eval, Action.cost, ih]
  | pair _ _ ihl ihr | cons _ _ ihl ihr =>
    simp only [Action.eval, Action.cost, ihl, ihr]

structure Production {N : Type} (value : N → Type) where
  lhs : N
  rhs : List (Sym Terminal N)
  firstSlice : Slice
  action : Action (ActionArgs value rhs) (value lhs)
  id : ProductionId := .program

/-- Maximum non-recursive cursor steps spent inside one production frame. -/
def Production.frameCost {N : Type} {value : N → Type} (p : Production value) : Nat :=
  1 + (p.rhs.length + 1) + p.action.cost

/-- Number of nonterminal symbols on a production's right-hand side. -/
def Production.nonterminalCount {N : Type} {value : N → Type} (p : Production value) : Nat :=
  (p.rhs.filter fun | .n _ => true | .t _ => false).length

def integerForm (radix : Surface0.Radix) (suffix : Option IntTy) : Nat :=
  radix.ctorIdx * 9 + (suffix.map fun ty => ty.ctorIdx + 1).getD 0

structure SharedGrammar (count : Nat) where
  terminalSlices : Array Slice
  terminalSliceCoverage : ∀ kind : Terminal, kind.ctorIdx < terminalSlices.size
  terminalSliceExact : ∀ i : Fin terminalSlices.size, ∃ kind : Terminal, kind.ctorIdx = i.val
  integerSlices : Array Slice
  integerSliceCoverage : ∀ radix suffix, integerForm radix suffix < integerSlices.size
  nonDecimalLater : ∀ radix suffix (_ : radix ≠ .decimal),
    0 < (integerSlices[integerForm radix suffix]'(integerSliceCoverage radix suffix)).val
  start : Fin count
  value : Fin count → Type
  extraAst : Type
  startShape : value start = SurfaceAST (fun _ => extraAst) slice0
  productions : Array (Production value)

def SharedGrammar.grammar {count : Nat} (g : SharedGrammar count) : Grammar Terminal (Fin count) :=
  ⟨g.start, g.productions.toList.map fun p => (p.lhs, p.rhs)⟩

/-- Numeric CFG projection consumed by `compileTableVerifier`. -/
def SharedGrammar.rawGrammar {count : Nat} (g : SharedGrammar count) :
    GrammarVerification.RawGrammar where
  nonterminals := count
  terminals := g.terminalSlices.size
  cfg := ⟨g.start.val, g.productions.toList.map fun p =>
    (p.lhs.val, p.rhs.map fun
      | .t t => .t t.ctorIdx
      | .n x => .n x.val)⟩

def terminalColumn (look : Option Terminal) : Nat :=
  match look with
  | none => 0
  | some terminal => terminal.ctorIdx + 1

def SharedGrammar.terminalColumns {count : Nat} (g : SharedGrammar count) : Nat :=
  g.terminalSlices.size + 1

def SharedGrammar.required {count : Nat} (g : SharedGrammar count) (token : TokenValue) : Slice :=
  let base := g.terminalSlices[token.kind.ctorIdx]'(g.terminalSliceCoverage token.kind)
  match token with
  | .integer _ radix suffix =>
    let integer := g.integerSlices[integerForm radix suffix]'(g.integerSliceCoverage radix suffix)
    if base.val < integer.val then integer else base
  | _ => base

def tableLookup {count : Nat} (columns : Nat) (cells : Array (Option Nat))
    (n : Fin count) (look : Option Terminal) : Option Nat :=
  cells[n.val * columns + terminalColumn look]?.join

/-- Rows name a production, so the executable parser and the typed action share the same lookup. -/
structure CheckedTable {count : Nat} (g : SharedGrammar count) where
  cells : Array (Option Nat)
  dimensions : cells.size = count * g.terminalColumns
  predicts : ∀ n look i, tableLookup g.terminalColumns cells n look = some i ↔
    Predicts g.grammar i look ∧ ∃ rhs, g.grammar.prods[i]? = some (n, rhs)
  rank : Fin count → Nat
  -- Entering a nullable prefix consumes no lookahead and must decrease rank.
  acyclic : ∀ p ∈ g.productions.toList, ∀ before child after,
    p.rhs = before ++ .n child :: after → NullableSeq g.grammar before →
    rank child < rank p.lhs

def CheckedTable.lookup {count : Nat} {g : SharedGrammar count} (table : CheckedTable g) :=
  tableLookup (count := count) g.terminalColumns table.cells

mutual
  /-- A derivation records exact ordered leaves and its action result. Its index
  counts production visits, token visits and interpreted action constructors. -/
  inductive Derives {count : Nat} (g : SharedGrammar count) (slice : Slice) :
      (n : Fin count) → List SurfaceToken → g.value n → Nat → Prop where
    | apply (production : Production g.value) (tokens : List SurfaceToken)
        (args : ActionArgs g.value production.rhs) {steps : Nat} :
        production ∈ g.productions.toList → production.firstSlice.val ≤ slice.val →
        DerivesArgs g slice production.rhs tokens args steps →
        Derives g slice production.lhs tokens (production.action.eval args).2 (steps + 1 + (production.action.eval args).1)

  inductive DerivesArgs {count : Nat} (g : SharedGrammar count) (slice : Slice) :
      (rhs : List (Sym Terminal (Fin count))) → List SurfaceToken → ActionArgs g.value rhs → Nat → Prop where
    | nil : DerivesArgs g slice [] [] () 0
    | token {kind : Terminal} {token : SurfaceToken} {rhs : List (Sym Terminal (Fin count))}
        {tokens : List SurfaceToken} {args : ActionArgs g.value rhs} {steps : Nat} :
        token.value.kind = kind → (g.required token.value).val ≤ slice.val →
        DerivesArgs g slice rhs tokens args steps →
        DerivesArgs g slice (.t kind :: rhs) (token :: tokens) (token, args) (steps + 1)
    | child {n : Fin count} {rhs : List (Sym Terminal (Fin count))} {head tail : List SurfaceToken}
        {value : g.value n} {args : ActionArgs g.value rhs} {left right : Nat} :
        Derives g slice n head value left → DerivesArgs g slice rhs tail args right →
        DerivesArgs g slice (.n n :: rhs) (head ++ tail) (value, args) (left + right)
end

mutual
  /-- Predictive visits also name the exact table row for the lookahead at entry.
  The unconsumed suffix matters, including for nullable nonterminals. -/
  inductive Predictive {count : Nat} {g : SharedGrammar count} (table : CheckedTable g) (slice : Slice) :
      (n : Fin count) → List SurfaceToken → List SurfaceToken → g.value n → Nat → Prop where
    | apply (p : Production g.value) (index : Nat) (tokens rest : List SurfaceToken)
        (args : ActionArgs g.value p.rhs) {steps : Nat} :
        g.productions[index]? = some p →
        table.lookup p.lhs ((tokens ++ rest).head?.map fun t => t.value.kind) = some index →
        p.firstSlice.val ≤ slice.val →
        PredictiveArgs table slice p.rhs tokens rest args steps →
        Predictive table slice p.lhs tokens rest (p.action.eval args).2 (steps + 1 + (p.action.eval args).1)

  inductive PredictiveArgs {count : Nat} {g : SharedGrammar count} (table : CheckedTable g)
      (slice : Slice) : (rhs : List (Sym Terminal (Fin count))) → List SurfaceToken → List SurfaceToken →
        ActionArgs g.value rhs → Nat → Prop where
    | nil (rest : List SurfaceToken) : PredictiveArgs table slice [] [] rest () 0
    | token {kind : Terminal} {token : SurfaceToken} {rhs : List (Sym Terminal (Fin count))}
        {tokens rest : List SurfaceToken} {args : ActionArgs g.value rhs} {steps : Nat} :
        token.value.kind = kind → (g.required token.value).val ≤ slice.val →
        PredictiveArgs table slice rhs tokens rest args steps →
        PredictiveArgs table slice (.t kind :: rhs) (token :: tokens) rest (token, args) (steps + 1)
    | child {n : Fin count} {rhs : List (Sym Terminal (Fin count))} {head tail rest : List SurfaceToken}
        {value : g.value n} {args : ActionArgs g.value rhs} {left right : Nat} :
        Predictive table slice n head (tail ++ rest) value left →
        PredictiveArgs table slice rhs tail rest args right →
        PredictiveArgs table slice (.n n :: rhs) (head ++ tail) rest (value, args) (left + right)
end

theorem Derives.tokensEligible {count : Nat} {g : SharedGrammar count} {slice : Slice}
    {n : Fin count} {tokens : List SurfaceToken} {tree : g.value n} {steps : Nat}
    (derivation : Derives g slice n tokens tree steps) :
    ∀ t ∈ tokens, (g.required t.value).val ≤ slice.val := by
  refine Derives.rec (motive_1 := fun _ tokens _ _ _ =>
      ∀ t ∈ tokens, (g.required t.value).val ≤ slice.val)
    (motive_2 := fun _ tokens _ _ _ => ∀ t ∈ tokens, (g.required t.value).val ≤ slice.val)
    (fun _ _ _ _ _ _ _ ih => ih) (by simp) ?_ ?_ derivation
  · intro _ tok _ _ _ _ _ eligible _ ih t mem
    rcases List.mem_cons.mp mem with rfl | mem
    · exact eligible
    · exact ih t mem
  · intro _ _ _ _ _ _ _ _ _ _ left right t mem
    rcases List.mem_append.mp mem with mem | mem
    · exact left t mem
    · exact right t mem

def SharedGrammar.Productive {count : Nat} (g : SharedGrammar count) : Prop :=
  ∀ p ∈ g.productions.toList, ∃ tokens args steps, DerivesArgs g ⟨Fin.last _⟩ p.rhs tokens args steps

def SharedGrammar.size {count : Nat} (g : SharedGrammar count) : Nat :=
  g.productions.foldl (fun size p => size + 1 + p.rhs.length) 0

private theorem foldl_production_size {N : Type} {value : N → Type}
    (ps : List (Production value)) (acc : Nat) :
    ps.foldl (fun size p => size + 1 + p.rhs.length) acc =
      acc + (ps.map fun p => p.rhs.length + 1).sum := by
  induction ps generalizing acc with
  | nil => simp
  | cons p ps ih =>
    simp only [List.foldl_cons, List.map_cons, List.sum_cons]
    rw [ih]
    omega

theorem SharedGrammar.rawGrammar_columns {count : Nat} (g : SharedGrammar count) :
    g.rawGrammar.columns = g.terminalColumns := rfl

theorem SharedGrammar.rawGrammar_positions {count : Nat} (g : SharedGrammar count) :
    g.rawGrammar.positions = g.size := by
  unfold SharedGrammar.size SharedGrammar.rawGrammar GrammarVerification.RawGrammar.positions
  rw [← Array.foldl_toList, foldl_production_size, Nat.zero_add]
  simp only [List.map_map, Function.comp_def, List.length_map]

/-- A fixed word-operation budget per grammar position and lookahead column.
Keeping the multiplier independent of the grammar prevents a verifier from
admitting quadratic work by raising the multiplier as its grammar grows. -/
def SharedGrammar.verificationBudget {count : Nat} (g : SharedGrammar count) : Nat :=
  256 * (g.size + 1) * g.terminalColumns

theorem SharedGrammar.rawGrammar_budget {count : Nat} (g : SharedGrammar count) :
    g.rawGrammar.budget = g.verificationBudget := by
  unfold GrammarVerification.RawGrammar.budget SharedGrammar.verificationBudget
  rw [g.rawGrammar_positions, g.rawGrammar_columns]

def SharedGrammar.maxFrameCost {count : Nat} (g : SharedGrammar count) : Nat :=
  g.productions.foldl (fun acc p => max acc p.frameCost) 1

def SharedGrammar.maxNonterminals {count : Nat} (g : SharedGrammar count) : Nat :=
  g.productions.foldl (fun acc p => max acc p.nonterminalCount) 0

/-- Static upper bound on zero-token subtree steps for a nonterminal of rank `r`. -/
def zeroTokenStepBound (frameCost maxNt : Nat) : Nat → Nat
  | 0 => frameCost
  | r + 1 => frameCost + maxNt * zeroTokenStepBound frameCost maxNt r

/-- Static linear-work multiplier computed solely from frame cost, max RHS
nonterminals, and nonterminal rank bound. -/
def linearStepsPerToken (frameCost maxNt maxRank : Nat) : Nat :=
  let zeroMax := zeroTokenStepBound frameCost maxNt maxRank
  (maxRank + 1) * (frameCost + maxNt * zeroMax) + 1

structure TableVerifier {count : Nat} (g : SharedGrammar count) where
  program : Array VerifyInstruction
  words : Nat
  spaceBound : words ≤ g.verificationBudget
  completes : ∀ memory : Array UInt64, memory.size = words →
    (verifyRun program ⟨0, memory⟩ g.verificationBudget).2.isSome
  /-- The initial memory starts with the proposed row cells. Zero is empty, and
  other words encode a production index plus one. The remaining words carry
  verification data such as the nullable, FIRST, FOLLOW and rank certificate. -/
  sound : ∀ memory : Array UInt64, memory.size = words →
    let result := verifyRun program ⟨0, memory⟩ g.verificationBudget
    result.2 = some true →
    ∃ table : CheckedTable g, g.Productive ∧ g.size ≤ result.1 ∧
      ∀ i : Fin table.cells.size, ∃ word, memory[i.val]? = some word ∧
        table.cells[i.val]? = some (if word == 0 then none else some (word.toNat - 1))

def TableVerifier.run {count : Nat} {g : SharedGrammar count} (verifier : TableVerifier g)
    (memory : Array UInt64) : Nat × Option Bool :=
  verifyRun verifier.program ⟨0, memory⟩ g.verificationBudget

structure Parser {count : Nat} (g : SharedGrammar count) where
  table : CheckedTable g
  verifier : TableVerifier g
  proposal : Array UInt64
  proposalSize : proposal.size = verifier.words
  verified : (verifier.run proposal).2 = some true
  exactTable : ∀ i : Fin table.cells.size, ∃ word, proposal[i.val]? = some word ∧
    table.cells[i.val]? = some (if word == 0 then none else some (word.toNat - 1))
  stepsPerToken : Nat
  malformedRule : RegisteredRule
  unsupportedProductionRule : RegisteredRule
  unsupportedLiteralRule : RegisteredRule

def Parser.bound {count : Nat} {g : SharedGrammar count} (parser : Parser g) (input : ParserView) : Nat :=
  parser.stepsPerToken * (input.tokens.size + 1)

/-- Cursor state for the LL(1) machine. `required` tracks the whole-file least
admitting slice while `firstUnsupportedHit` is write-once and preserves the
earliest out-of-slice witness relative to the requested slice in source order. -/
structure ParseCursor where
  offset : Nat
  required : Slice
  firstUnsupportedHit : Option (FileSpan × UnsupportedCause)
  steps : Nat

def ParseCursor.require (c : ParseCursor) (target slice : Slice)
    (cause : UnsupportedCause) (span : FileSpan) : ParseCursor :=
  let required := if c.required.val < slice.val then slice else c.required
  let firstUnsupportedHit :=
    match c.firstUnsupportedHit with
    | some hit => some hit
    | none => if target.val < slice.val then some (span, cause) else none
  { c with required, firstUnsupportedHit }

/-- Classify a token-level slice restriction: non-decimal integer literals cite
`DecimalLiteral`, while any other terminal restriction cites its owning production. -/
def tokenUnsupportedCause (prodId : ProductionId) : TokenValue → UnsupportedCause
  | .integer _ radix _ =>
    match radix with
    | .decimal => .production prodId
    | _ => .nonDecimal radix
  | _ => .production prodId

inductive ParseStop where
  | malformed
  | exhausted

abbrev ParseWork (A : Type) := ParseCursor × Except ParseStop A

mutual
  def parseSymbol {count : Nat} {g : SharedGrammar count} (table : CheckedTable g)
      (tokens : Array SurfaceToken) (targetSlice : Slice) (eof : FileSpan)
      (n : Fin count) (c : ParseCursor) : Nat → ParseWork (g.value n)
    | 0 => (c, .error .exhausted)
    | fuel + 1 =>
      let c := {c with steps := c.steps + 1}
      match table.lookup n (tokens[c.offset]?.map fun t => t.value.kind) with
      | none => (c, .error .malformed)
      | some index =>
        match g.productions[index]? with
        | none => (c, .error .malformed)
        | some production =>
          if h : production.lhs = n then
            let lookSpan := (tokens[c.offset]?.map Located.span).getD eof
            let c := c.require targetSlice production.firstSlice (.production production.id) lookSpan
            match parseArgs table tokens targetSlice eof production.id fuel production.rhs c with
            | (c, .error stop) => (c, .error stop)
            | (c, .ok args) =>
              let (cost, value) := production.action.eval args
              ({c with steps := c.steps + cost}, .ok (h ▸ value))
          else (c, .error .malformed)
  termination_by structural fuel => fuel

  def parseArgs {count : Nat} {g : SharedGrammar count} (table : CheckedTable g)
      (tokens : Array SurfaceToken) (targetSlice : Slice) (eof : FileSpan) (prodId : ProductionId) :
      (fuel : Nat) → (rhs : List (Sym Terminal (Fin count))) → ParseCursor →
        ParseWork (ActionArgs g.value rhs)
    | 0, _, c => (c, .error .exhausted)
    | _ + 1, [], c => ({c with steps := c.steps + 1}, .ok ())
    | fuel + 1, .t kind :: rest, c =>
      let c := {c with steps := c.steps + 1}
      match tokens[c.offset]? with
      | none => (c, .error .malformed)
      | some token =>
        if token.value.kind ≠ kind then (c, .error .malformed)
        else
          let c := c.require targetSlice (g.required token.value)
            (tokenUnsupportedCause prodId token.value) token.span
          let c := {c with offset := c.offset + 1}
          let (c, args) := parseArgs table tokens targetSlice eof prodId fuel rest c
          (c, args.map fun args => (token, args))
    | fuel + 1, .n n :: rest, c =>
      let c := {c with steps := c.steps + 1}
      match parseSymbol table tokens targetSlice eof n c fuel with
      | (c, .error stop) => (c, .error stop)
      | (c, .ok child) =>
        let (c, args) := parseArgs table tokens targetSlice eof prodId fuel rest c
        (c, args.map fun args => (child, args))
  termination_by structural fuel => fuel
end

theorem ParseCursor.require_firstUnsupportedHit_isSome (c : ParseCursor)
    (target slice : Slice) (cause : UnsupportedCause) (span : FileSpan)
    (h : c.firstUnsupportedHit.isSome = decide (target.val < c.required.val)) :
    (c.require target slice cause span).firstUnsupportedHit.isSome =
      decide (target.val < (c.require target slice cause span).required.val) := by
  unfold ParseCursor.require
  cases hhit : c.firstUnsupportedHit with
  | some hit =>
    rw [hhit] at h
    have hlt : target.val < c.required.val := of_decide_eq_true h.symm
    dsimp only [Option.isSome]
    split
    · next hcmp => exact (decide_eq_true (Nat.lt_trans hlt hcmp)).symm
    · exact (decide_eq_true hlt).symm
  | none =>
    rw [hhit] at h
    have hle : ¬ (target.val < c.required.val) := of_decide_eq_false h.symm
    by_cases hsl : target.val < slice.val
    · have hcmp : c.required.val < slice.val := Nat.lt_of_le_of_lt (Nat.le_of_not_lt hle) hsl
      simp only [hsl, hcmp, ↓reduceIte, Option.isSome, decide_true]
    · simp only [hsl, ↓reduceIte, Option.isSome]
      split
      · exact (decide_eq_false hsl).symm
      · exact (decide_eq_false hle).symm

private def cursorHitInv (target : Slice) (c : ParseCursor) : Prop :=
  c.firstUnsupportedHit.isSome = decide (target.val < c.required.val)

private theorem parseSymbol_parseArgs_firstUnsupportedHit_isSome {count : Nat}
    {g : SharedGrammar count} (table : CheckedTable g) (tokens : Array SurfaceToken)
    (targetSlice : Slice) (eof : FileSpan) (fuel : Nat) :
    (∀ (n : Fin count) (c : ParseCursor),
      cursorHitInv targetSlice c →
      cursorHitInv targetSlice (parseSymbol table tokens targetSlice eof n c fuel).1) ∧
    (∀ (prodId : ProductionId) (rhs : List (Sym Terminal (Fin count))) (c : ParseCursor),
      cursorHitInv targetSlice c →
      cursorHitInv targetSlice (parseArgs table tokens targetSlice eof prodId fuel rhs c).1) := by
  induction fuel with
  | zero =>
    exact ⟨fun _ _ h => h, fun _ _ _ h => h⟩
  | succ fuel ih =>
    obtain ⟨ihSym, ihArgs⟩ := ih
    constructor
    · intro n c hc
      unfold parseSymbol
      dsimp only
      cases table.lookup n (tokens[c.offset]?.map fun t => t.value.kind) with
      | none => exact hc
      | some index =>
        dsimp only
        cases g.productions[index]? with
        | none => exact hc
        | some production =>
          dsimp only
          by_cases hlhs : production.lhs = n
          · simp only [hlhs, ↓reduceDIte]
            have hreq : cursorHitInv targetSlice _ :=
              ParseCursor.require_firstUnsupportedHit_isSome
                {c with steps := c.steps + 1} targetSlice production.firstSlice
                (.production production.id) ((tokens[c.offset]?.map Located.span).getD eof) hc
            have hres := ihArgs production.id production.rhs _ hreq
            cases hpa : parseArgs table tokens targetSlice eof production.id fuel production.rhs
                (({c with steps := c.steps + 1}).require targetSlice production.firstSlice
                  (.production production.id) ((tokens[c.offset]?.map Located.span).getD eof)) with
            | mk c' res =>
              rw [hpa] at hres
              cases res with
              | error _ => exact hres
              | ok _ => exact hres
          · simp only [hlhs, ↓reduceDIte]
            exact hc
    · intro prodId rhs c hc
      cases rhs with
      | nil => exact hc
      | cons sym rest =>
        cases sym with
        | t kind =>
          unfold parseArgs
          dsimp only
          cases tokens[c.offset]? with
          | none => exact hc
          | some token =>
            dsimp only
            split
            · exact hc
            · have hreq : cursorHitInv targetSlice _ :=
                ParseCursor.require_firstUnsupportedHit_isSome
                  {c with steps := c.steps + 1} targetSlice (g.required token.value)
                  (tokenUnsupportedCause prodId token.value) token.span hc
              exact ihArgs prodId rest _ hreq
        | n child =>
          unfold parseArgs
          dsimp only
          have hchild := ihSym child {c with steps := c.steps + 1} hc
          cases hps : parseSymbol table tokens targetSlice eof child {c with steps := c.steps + 1} fuel with
          | mk c' res =>
            rw [hps] at hchild
            cases res with
            | error _ => exact hchild
            | ok _ => exact ihArgs prodId rest c' hchild

theorem parseSymbol_firstUnsupportedHit_isSome {count : Nat} {g : SharedGrammar count}
    (table : CheckedTable g) (tokens : Array SurfaceToken) (targetSlice : Slice)
    (eof : FileSpan) (n : Fin count) (c : ParseCursor) (fuel : Nat)
    (h : c.firstUnsupportedHit.isSome = decide (targetSlice.val < c.required.val)) :
    let c' := (parseSymbol table tokens targetSlice eof n c fuel).1
    c'.firstUnsupportedHit.isSome = decide (targetSlice.val < c'.required.val) :=
  (parseSymbol_parseArgs_firstUnsupportedHit_isSome table tokens targetSlice eof fuel).1 n c h

/-- Executable inputs of the cursor machine, backed by a checked table and the
three exact registered refusal rules. -/
structure ParserEngine {count : Nat} (g : SharedGrammar count) where
  table : CheckedTable g
  stepsPerToken : Nat
  malformedRule : RegisteredRule
  unsupportedProductionRule : RegisteredRule
  unsupportedLiteralRule : RegisteredRule

def ParserEngine.run {count : Nat} {g : SharedGrammar count} (parser : ParserEngine g) (input : ParserView) :
    Nat × Option (Except ParseRefusal (Parsed (g.value g.start))) :=
  match input.invalidLocation with
  | some (span, reason) => (1, some (.error (.invalidStream span reason)))
  | none =>
    let eof := eofFileSpan input.source
    let (c, value) := parseSymbol parser.table input.tokens input.source.slice eof g.start
      ⟨0, slice0, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))
    let span := (input.tokens[c.offset]?.map Located.span).getD eof
    match value with
    | .error .exhausted => (c.steps + 1, none)
    | .error .malformed =>
      let cause := if c.offset < input.tokens.size then MalformedCause.unexpectedToken else .unexpectedEnd
      (c.steps + 1, some (.error (.malformed parser.malformedRule cause span)))
    | .ok ast =>
      if c.offset != input.tokens.size then
        (c.steps + 1, some (.error (.malformed parser.malformedRule .trailingTokens span)))
      else
        match c.firstUnsupportedHit with
        | some (hitSpan, cause) =>
          let rule := match cause with
            | .production _ => parser.unsupportedProductionRule
            | .nonDecimal _ => parser.unsupportedLiteralRule
          (c.steps + 1, some (.error (.unsupported rule cause hitSpan c.required)))
        | none =>
          (c.steps + 1, some (.ok ⟨input.source, ast⟩))

def ParsesWith {count : Nat} {g : SharedGrammar count} (parser : ParserEngine g) (input : ParserView)
    (output : Parsed (g.value g.start)) : Prop :=
  output.source = input.source ∧
  ∃ steps, Derives g input.source.slice g.start input.tokens.toList output.tree steps ∧
    Predictive parser.table input.source.slice g.start input.tokens.toList [] output.tree steps ∧
    steps ≤ (parser.run input).1 ∧ input.WF

def Parser.engine {count : Nat} {g : SharedGrammar count} (parser : Parser g) : ParserEngine g :=
  ⟨parser.table, parser.stepsPerToken, parser.malformedRule,
    parser.unsupportedProductionRule, parser.unsupportedLiteralRule⟩

def Parser.run {count : Nat} {g : SharedGrammar count} (parser : Parser g) :=
  parser.engine.run

def Parses {count : Nat} {g : SharedGrammar count} (parser : Parser g) :=
  ParsesWith parser.engine

def Refuses {count : Nat} (g : SharedGrammar count) (input : ParserView) : ParseRefusal → Prop
  | .invalidStream span reason => input.invalidLocation = some (span, reason)
  | .unsupported _ _ _ required =>
    input.source.slice.val < required.val ∧
    (∃ tree steps, Derives g required g.start input.tokens.toList tree steps) ∧
    ∀ earlier, earlier.val < required.val →
      ¬ ∃ tree steps, Derives g earlier g.start input.tokens.toList tree steps
  | .malformed _ _ _ =>
    ¬ ∃ tree required steps, Derives g required g.start input.tokens.toList tree steps

/-- Decode the prediction-table cell region from an accepted certificate array. -/
def acceptedCells {count : Nat} (proposal : Array UInt64) (grammar : SharedGrammar count) :
    Array (Option Nat) :=
  (proposal.extract 0 (count * grammar.terminalColumns)).map fun word =>
    if word == 0 then none else some (word.toNat - 1)

/-- Decode one nonterminal rank from an accepted certificate array. -/
def acceptedRank {count : Nat} (proposal : Array UInt64) (grammar : SharedGrammar count)
    (n : Fin count) : Nat :=
  (proposal.getD (grammar.rawGrammar.layout.rank n.val) 0).toNat

/-- Closed input to the compiled word verifier. Binding `verifier` to
`compileTableVerifier grammar.rawGrammar`, acceptance on `proposal`, and
`engine.table.cells = acceptedCells proposal grammar` certifies the exact
prediction table the parser consumes. -/
structure VerificationInputs {count : Nat} (grammar : SharedGrammar count) where
  verifier : TableVerifier grammar
  verifierCompiled :
    let compiled := GrammarVerification.compileTableVerifier grammar.rawGrammar
    verifier.program = compiled.program ∧ verifier.words = compiled.words
  proposal : Array UInt64
  proposalSize : proposal.size = verifier.words
  accepted : (verifier.run proposal).2 = some true
  engine : ParserEngine grammar
  engineTableEq : engine.table.cells = acceptedCells proposal grammar

def VerificationInputs.program {count : Nat} {grammar : SharedGrammar count}
    (inputs : VerificationInputs grammar) : Array VerifyInstruction :=
  inputs.verifier.program

theorem VerificationInputs.spaceBound {count : Nat} {grammar : SharedGrammar count}
    (inputs : VerificationInputs grammar) :
    inputs.proposal.size ≤ grammar.verificationBudget :=
  inputs.proposalSize ▸ inputs.verifier.spaceBound

private theorem array_extract_loop_getElem_zero {α : Type} (as : Array α) :
    ∀ (i : Nat) (bs : Array α) (hle : bs.size + i ≤ as.size),
      (∀ k (hk : k < bs.size), bs[k] = as[k]'(Nat.lt_of_lt_of_le hk (Nat.le_trans (Nat.le_add_right _ _) hle))) →
      ∃ hsz : (Array.extract.loop as i bs.size bs).size = bs.size + i,
        ∀ k (hk : k < bs.size + i),
          (Array.extract.loop as i bs.size bs)[k]'(hsz ▸ hk) = as[k]'(Nat.lt_of_lt_of_le hk hle)
  | 0, bs, _, hbs => by
    have hloop : Array.extract.loop as 0 bs.size bs = bs := by
      unfold Array.extract.loop
      split <;> rfl
    rw [hloop]
    exact ⟨rfl, hbs⟩
  | i + 1, bs, hle, hbs => by
    have hj : bs.size < as.size := by omega
    have hstep : Array.extract.loop as (i + 1) bs.size bs =
        Array.extract.loop as i (bs.push as[bs.size]).size (bs.push as[bs.size]) := by
      rw [Array.size_push]
      conv => lhs; unfold Array.extract.loop
      simp only [hj, ↓reduceDIte]
      rfl
    have hle' : (bs.push as[bs.size]).size + i ≤ as.size := by rw [Array.size_push]; omega
    have hbs' : ∀ k (hk : k < (bs.push as[bs.size]).size),
        (bs.push as[bs.size])[k] = as[k]'(Nat.lt_of_lt_of_le hk (Nat.le_trans (Nat.le_add_right _ _) hle')) := by
      intro k hk
      rw [Array.size_push] at hk
      by_cases hlt : k < bs.size
      · rw [Array.getElem_push_lt hlt, hbs k hlt]
      · have hkEq : k = bs.size := by omega
        subst hkEq
        exact Array.getElem_push_eq
    obtain ⟨hsz, hget⟩ := array_extract_loop_getElem_zero as i (bs.push as[bs.size]) hle' hbs'
    have hszFinal : (Array.extract.loop as (i + 1) bs.size bs).size = bs.size + (i + 1) := by
      rw [hstep, hsz, Array.size_push]; omega
    refine ⟨hszFinal, fun k hk => ?_⟩
    have hk' : k < (bs.push as[bs.size]).size + i := by rw [Array.size_push]; omega
    have h1 : (Array.extract.loop as (i + 1) bs.size bs)[k]'(hszFinal ▸ hk) =
        (Array.extract.loop as i (bs.push as[bs.size]).size (bs.push as[bs.size]))[k]'(hsz ▸ hk') := by
      simp only [hstep]
    rw [h1]
    exact hget k hk'

private theorem array_getElem?_extract_zero {α : Type} (as : Array α) (stop k : Nat)
    (hk : k < min stop as.size) :
    (as.extract 0 stop)[k]? = some (as[k]'(Nat.lt_of_lt_of_le hk (Nat.min_le_right _ _))) := by
  let n := min stop as.size
  have hn : n ≤ as.size := Nat.min_le_right stop as.size
  have hmin : (min stop as.size).sub 0 = n := Nat.sub_zero _
  unfold Array.extract
  dsimp only
  rw [hmin]
  let emp : Array α := Array.emptyWithCapacity n
  have hle : emp.size + n ≤ as.size := by
    change 0 + n ≤ as.size
    rw [Nat.zero_add]; exact hn
  obtain ⟨hsz, hget⟩ := array_extract_loop_getElem_zero as n emp hle
    (fun _ hj => absurd hj (Nat.not_lt_zero _))
  have hsz' : (Array.extract.loop as n 0 emp).size = n := by
    change (Array.extract.loop as n emp.size emp).size = n
    rw [hsz]; exact Nat.zero_add n
  have hlt : k < (Array.extract.loop as n 0 emp).size := by
    rw [hsz']; exact hk
  rw [Array.getElem?_eq_getElem hlt]
  exact congrArg some (hget k (by change k < 0 + n; rw [Nat.zero_add]; exact hk))

theorem VerificationInputs.exactTable {count : Nat} {grammar : SharedGrammar count}
    (inputs : VerificationInputs grammar) :
    ∀ i : Fin inputs.engine.table.cells.size, ∃ word,
      inputs.proposal[i.val]? = some word ∧
        inputs.engine.table.cells[i.val]? = some (if word == 0 then none else some (word.toNat - 1)) := by
  intro i
  have hlen : inputs.engine.table.cells.size =
      min (count * grammar.terminalColumns) inputs.proposal.size := by
    have h := congrArg Array.size inputs.engineTableEq
    simpa only [acceptedCells, Array.size_map, Array.size_extract, Nat.sub_zero] using h
  have hiMin : i.val < min (count * grammar.terminalColumns) inputs.proposal.size :=
    hlen ▸ i.isLt
  have hiProp : i.val < inputs.proposal.size := Nat.lt_of_lt_of_le hiMin (Nat.min_le_right _ _)
  refine ⟨inputs.proposal[i.val]'(hiProp), Array.getElem?_eq_getElem hiProp, ?_⟩
  have hcell := congrArg (fun a => a[i.val]?) inputs.engineTableEq
  rw [hcell]
  simp only [acceptedCells, Array.getElem?_map,
    array_getElem?_extract_zero inputs.proposal (count * grammar.terminalColumns) i.val hiMin,
    Option.map_some]

end ConcreteSpec.StageGrammar
