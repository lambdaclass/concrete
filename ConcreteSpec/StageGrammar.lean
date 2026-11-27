import ConcreteSpec.SurfacePrefix

namespace ConcreteSpec.StageGrammar

open ConcreteSpec.StageInterface
open ConcreteSpec.Core (RuleId)
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

structure Production {N : Type} (value : N → Type) where
  lhs : N
  rhs : List (Sym Terminal N)
  firstSlice : Slice
  action : Action (ActionArgs value rhs) (value lhs)

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
  /-- The source language owns the judgment on a recognized prefix. Small
  standalone grammars with only grammatical constraints use the cursor cause. -/
  diagnose : SurfacePreparation.Purpose → ParserView → FileSpan → RefusalCause → Nat × RefusalCause :=
    fun _ _ _ cause => (0, cause)

def SharedGrammar.grammar {count : Nat} (g : SharedGrammar count) : Grammar Terminal (Fin count) :=
  ⟨g.start, g.productions.toList.map fun p => (p.lhs, p.rhs)⟩

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

/-- The verifier is a closed word machine. Indexed word reads/writes, comparisons,
branches and arithmetic are instructions. Program and proposal are data. -/
inductive VerifyInstruction where
  | constant (dst : Nat) (value : UInt64)
  | read (dst address : Nat)
  | write (address src : Nat)
  | add (dst left right : Nat)
  | mul (dst left right : Nat)
  | sub (dst left right : Nat)
  | ifEqual (left right yes no : Nat)
  | ifLess (left right yes no : Nat)
  | halt (accept : Bool)

structure VerifyState where
  pc : Nat
  memory : Array UInt64

def VerifyState.advance (s : VerifyState) : VerifyState := {s with pc := s.pc + 1}

def VerifyState.step (s : VerifyState) (instruction : VerifyInstruction) :
    Option (Except Bool VerifyState) := do
  let read := fun i => s.memory[i]?
  let write := fun dst word => do
    if dst < s.memory.size then
      some (.ok {s.advance with memory := s.memory.set! dst word})
    else none
  match instruction with
  | .constant dst word => write dst word
  | .read dst addr => write dst (← read (← read addr).toNat)
  | .write addr src => write (← read addr).toNat (← read src)
  | .add dst a b => write dst ((← read a) + (← read b))
  | .mul dst a b => write dst ((← read a) * (← read b))
  | .sub dst a b => write dst ((← read a) - (← read b))
  | .ifEqual a b yes no => return .ok {s with pc := if (← read a) == (← read b) then yes else no}
  | .ifLess a b yes no => return .ok {s with pc := if (← read a) < (← read b) then yes else no}
  | .halt accept => return .error accept

def verifyRun (program : Array VerifyInstruction) (s : VerifyState) : Nat → Nat × Option Bool
  | 0 => (0, none)
  | fuel + 1 =>
    match program[s.pc]?.bind s.step with
    | none => (1, none)
    | some (.error accept) => (1, some accept)
    | some (.ok next) =>
      let (steps, result) := verifyRun program next fuel
      (steps + 1, result)

/-- A fixed word-operation budget per grammar position and lookahead column.
Keeping the multiplier independent of the grammar prevents a verifier from
admitting quadratic work by raising the multiplier as its grammar grows. -/
def SharedGrammar.verificationBudget {count : Nat} (g : SharedGrammar count) : Nat :=
  256 * (g.size + 1) * g.terminalColumns

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

def Parser.bound {count : Nat} {g : SharedGrammar count} (parser : Parser g) (input : ParserView) : Nat :=
  parser.stepsPerToken * (input.tokens.size + 1)

structure ParseCursor where
  offset : Nat
  required : Slice
  eof : FileSpan
  selected : Slice
  firstViolation : Option RefusalCause
  steps : Nat

def ParseCursor.require (c : ParseCursor) (slice : Slice) (issue : ParserIssue) (span : FileSpan) : ParseCursor :=
  {c with
    required := if c.required.val < slice.val then slice else c.required
    firstViolation := c.firstViolation.orElse fun _ =>
      if c.selected.val < slice.val then some (.parser issue span.surface) else none }

inductive ParseStop where
  | malformed
  | exhausted

abbrev ParseWork (A : Type) := ParseCursor × Except ParseStop A

mutual
  def parseSymbol {count : Nat} {g : SharedGrammar count} (table : CheckedTable g)
      (tokens : Array SurfaceToken) (n : Fin count) (c : ParseCursor) :
      Nat → ParseWork (g.value n)
    | 0 => (c, .error .exhausted)
    | fuel + 1 => Id.run do
      let c := {c with steps := c.steps + 1}
      let some index := table.lookup n (tokens[c.offset]?.map fun t => t.value.kind)
        | return (c, .error .malformed)
      let some production := g.productions[index]?
        | return (c, .error .malformed)
      if h : production.lhs = n then
        let c := c.require production.firstSlice .outsideProduction
          ((tokens[c.offset]?.map Located.span).getD c.eof)
        let (c, args) := parseArgs table tokens fuel production.rhs c
        match args with
        | .error stop => return (c, .error stop)
        | .ok args =>
          let (cost, value) := production.action.eval args
          return ({c with steps := c.steps + cost}, .ok (h ▸ value))
      else return (c, .error .malformed)
  termination_by structural fuel => fuel

  def parseArgs {count : Nat} {g : SharedGrammar count} (table : CheckedTable g)
      (tokens : Array SurfaceToken) :
      (fuel : Nat) → (rhs : List (Sym Terminal (Fin count))) → ParseCursor →
        ParseWork (ActionArgs g.value rhs)
    | 0, _, c => (c, .error .exhausted)
    | _ + 1, [], c => ({c with steps := c.steps + 1}, .ok ())
    | fuel + 1, .t kind :: rest, c => Id.run do
      let c := {c with steps := c.steps + 1}
      let some token := tokens[c.offset]? | return (c, .error .malformed)
      if token.value.kind ≠ kind then return (c, .error .malformed)
      let issue : ParserIssue := match token.value with
        | .integer _ radix _ => if radix = .decimal then .outsideProduction else .nonDecimal
        | _ => .outsideProduction
      let c := (c.require (g.required token.value) issue token.span)
        |> fun c => {c with offset := c.offset + 1}
      let (c, args) := parseArgs table tokens fuel rest c
      return (c, args.map fun args => (token, args))
    | fuel + 1, .n n :: rest, c => Id.run do
      let c := {c with steps := c.steps + 1}
      let (c, child) := parseSymbol table tokens n c fuel
      match child with
      | .error stop => return (c, .error stop)
      | .ok child =>
        let (c, args) := parseArgs table tokens fuel rest c
        return (c, args.map fun args => (child, args))
  termination_by structural fuel => fuel
end

/-- Executable inputs of the cursor machine, backed by a checked table. -/
structure ParserEngine {count : Nat} (g : SharedGrammar count) where
  table : CheckedTable g
  stepsPerToken : Nat

private def ParserEngine.recognize {count : Nat} {g : SharedGrammar count} (parser : ParserEngine g) (input : ParserView) (eof : FileSpan) :
    Nat × Option (Except ParseRefusal (Parsed (g.value g.start))) := Id.run do
  let (c, value) := parseSymbol parser.table input.tokens g.start
    ⟨0, slice0, eof, input.source.slice, none, 0⟩ (parser.stepsPerToken * (input.tokens.size + 1))
  let token := input.tokens[c.offset]?
  let span := (token.map Located.span).getD eof
  match value with
  | .error .exhausted => return (c.steps + 1, none)
  | .error .malformed =>
    let issue : ParserIssue := if token.isSome then .unexpectedToken else .unexpectedEnd
    return (c.steps + 1, some (.error (.malformed (c.firstViolation.getD (.parser issue span.surface)))))
  | .ok ast =>
    if c.offset != input.tokens.size then
      return (c.steps + 1, some (.error (.malformed (c.firstViolation.getD (.parser .trailingTokens span.surface)))))
    if let some cause := c.firstViolation then
      return (c.steps + 1, some (.error (.unsupported cause c.required)))
    return (c.steps + 1, some (.ok ⟨input.source, ast⟩))

private def ParseRefusal.diagnose {count} (g : SharedGrammar count) (input : ParserView)
    (eof : FileSpan) (purpose : SurfacePreparation.Purpose) : ParseRefusal → Nat × ParseRefusal
  | .unsupported cause required =>
    let (work, first) := g.diagnose purpose input eof cause
    (work, .unsupported first required)
  | .malformed cause =>
    let (work, first) := g.diagnose purpose input eof cause
    (work, .malformed first)
  | .invalidStream span => (0, .invalidStream span)

def ParserEngine.run {count : Nat} {g : SharedGrammar count} (parser : ParserEngine g) (input : ParserView)
    (purpose : SurfacePreparation.Purpose := .admission) :
    Nat × Option (Except ParseRefusal (Parsed (g.value g.start))) :=
  match input.validateCoordinates with
  | .error span => (1, some (.error (.invalidStream span)))
  | .ok eof =>
    let (steps, result) := parser.recognize input eof
    match result with
    | some (.error raw) =>
      let (work, refusal) := ParseRefusal.diagnose g input eof purpose raw
      (steps + work, some (.error refusal))
    | other => (steps, other)

def ParsesWith {count : Nat} {g : SharedGrammar count} (parser : ParserEngine g) (input : ParserView)
    (output : Parsed (g.value g.start)) : Prop :=
  output.source = input.source ∧
  ∃ steps, Derives g input.source.slice g.start input.tokens.toList output.tree steps ∧
    Predictive parser.table input.source.slice g.start input.tokens.toList [] output.tree steps ∧
    steps ≤ (parser.run input).1 ∧ input.WF

def Parser.engine {count : Nat} {g : SharedGrammar count} (parser : Parser g) : ParserEngine g :=
  ⟨parser.table, parser.stepsPerToken⟩

def Parser.run {count : Nat} {g : SharedGrammar count} (parser : Parser g) :=
  parser.engine.run

def Parses {count : Nat} {g : SharedGrammar count} (parser : Parser g) :=
  ParsesWith parser.engine

/-- Grammar recognition establishes the least required slice or malformation.
The language judgment independently fixes the reported cause and location. -/
def Refuses {count : Nat} {g : SharedGrammar count} (parser : ParserEngine g)
    (input : ParserView) (failure : ParseRefusal) (purpose : SurfacePreparation.Purpose := .admission) : Prop :=
  let diagnosed := ∃ eof raw, input.validateCoordinates = .ok eof ∧
    (parser.recognize input eof).2 = some (.error raw) ∧
    failure = (ParseRefusal.diagnose g input eof purpose raw).2
  match failure with
  | .invalidStream span => input.invalidSpan = some span
  | .unsupported _ required => diagnosed ∧
    input.source.slice.val < required.val ∧
    (∃ tree steps, Derives g required g.start input.tokens.toList tree steps) ∧
    ∀ earlier, earlier.val < required.val →
      ¬ ∃ tree steps, Derives g earlier g.start input.tokens.toList tree steps
  | .malformed _ => diagnosed ∧
    ¬ ∃ tree required steps, Derives g required g.start input.tokens.toList tree steps

theorem Refuses.unsupported_run {count : Nat} {g : SharedGrammar count} {parser : ParserEngine g}
    {input : ParserView} {cause required purpose}
    (h : Refuses parser input (.unsupported cause required) purpose) :
    (parser.run input purpose).2 = some (.error (.unsupported cause required)) := by
  obtain ⟨⟨eof, raw, coords, recognized, reported⟩, _⟩ := h
  simp only [ParserEngine.run, coords, recognized, ← reported]

private def SurfaceToken.toSurface (t : SurfaceToken) : Surface0.Token :=
  let value : Surface0.TokenValue := match t.value with
    | .fixed kind proof => .fixed kind (by constructor <;> intro h <;> subst kind <;> cases proof)
    | .name name => .name name
    | .integer n radix suffix => .integer n radix suffix
    | .stringLiteral _ => .fixed .stringLiteral
    | .byteLiteral _ => .fixed .byteLiteral
    | .label _ => .fixed .label
  ⟨value, t.span.surface⟩

private def surfaceDiagnosis (purpose : SurfacePreparation.Purpose) (input : ParserView) (eof : FileSpan) (boundary : RefusalCause) : Nat × RefusalCause :=
  if input.source.slice = slice0 then
    SurfacePrefix.diagnoseCounted eof.surface (input.tokens.toList.map SurfaceToken.toSurface) purpose (.parser .unexpectedEnd eof.surface) boundary
  else (0, boundary)

private def early := slice0
private def later : Slice := ⟨⟨2, by decide⟩⟩

private def bootstrapGrammar (production : Production (fun (_ : Fin 1) => SyntaxTree ⊕ Bool)) : SharedGrammar 1 :=
  { terminalSlices := Array.replicate Surface0.Terminal.count early
    terminalSliceCoverage := Surface0.Terminal.index_lt_count
    terminalSliceExact := fun i =>
      ⟨Surface0.Terminal.ofNat i.val, Surface0.Terminal.index_ofNat ⟨i.val, i.isLt⟩⟩
    integerSlices := Array.replicate 9 early ++ Array.replicate 27 later
    integerSliceCoverage := by
      intro radix suffix
      cases radix <;> cases suffix <;> first | (rename_i ty; cases ty <;> decide) | decide
    nonDecimalLater := by
      intro radix suffix nonDecimal
      cases radix <;> cases suffix <;> first
        | contradiction
        | (rename_i ty; cases ty <;> decide)
        | decide
    start := 0
    value := fun _ => SyntaxTree ⊕ Bool
    extraAst := Bool
    startShape := rfl
    productions := #[production]
    diagnose := surfaceDiagnosis }

private def returnRhs : List (Sym Terminal (Fin 1)) :=
  [.t .fn, .t .ident, .t .lparen, .t .rparen, .t .arrow, .t .u8,
    .t .lbrace, .t .return_, .t .integer, .t .semi, .t .rbrace]

private def returnProduction : Production (fun (_ : Fin 1) => SyntaxTree ⊕ Bool) :=
  let afterFn := Action.right .input
  let name := Action.left afterFn
  let afterName := Action.right afterFn
  let afterOpen := Action.right afterName
  let afterClose := Action.right afterOpen
  let afterArrow := Action.right afterClose
  let afterType := Action.right afterArrow
  let afterBrace := Action.right afterType
  let ret := Action.left afterBrace
  let afterReturn := Action.right afterBrace
  let value := Action.left afterReturn
  let semi := Action.left (.right afterReturn)
  let body := Action.node .return_ (.cons (.leaf ret) (.cons (.leaf value) (.cons (.leaf semi) (.constant []))))
  ⟨0, returnRhs, early, .inl (.node .program (.cons (.leaf name)
    (.cons (.node .body (.cons body (.constant []))) (.constant []))))⟩

abbrev sliceGrammarCount (_slice : Slice) : Nat := 1

def sliceASTExtra (_slice : Slice) : Type := Bool

def sliceGrammar (slice : Slice) : SharedGrammar (sliceGrammarCount slice) :=
  bootstrapGrammar returnProduction

theorem sliceGrammarStartShape (slice : Slice) :
    (sliceGrammar slice).value (sliceGrammar slice).start = SurfaceAST sliceASTExtra slice := rfl

private def returnCells : Array (Option Nat) :=
  (Array.replicate (sliceGrammar slice0).terminalColumns none).set! (terminalColumn (some .fn)) (some 0)

private def bootstrapTable : CheckedTable (sliceGrammar slice0) where
  cells := returnCells
  dimensions := rfl
  predicts := by
    intro n look i
    have hn : n = 0 := Subsingleton.elim _ _
    subst n
    have entry : tableLookup (sliceGrammar slice0).terminalColumns returnCells (0 : Fin 1) look =
        if look = some Surface0.Terminal.fn then some 0 else none := by
      cases look with
      | none => rfl
      | some t => cases t <;> rfl
    rw [entry]
    change _ ↔ Predicts (sliceGrammar slice0).grammar i look ∧
      ∃ rhs, [((0 : Fin 1), returnRhs)][i]? = some (0, rhs)
    cases i with
    | zero =>
      simp only [List.getElem?_cons_zero, Option.some.injEq, Prod.mk.injEq, true_and, exists_eq', and_true]
      constructor
      · intro hit
        have h : look = some Surface0.Terminal.fn := by
          split at hit
          · assumption
          · contradiction
        exact ⟨0, returnRhs, rfl, .inl ⟨.fn, h, [], returnRhs.drop 1,
          ⟨[], rfl, by simp⟩, .inl rfl⟩⟩
      · rintro ⟨n, rhs, row, first | ⟨nullable, _⟩⟩
        · change some ((0 : Fin 1), returnRhs) = some (n, rhs) at row
          have hr := (Prod.mk.inj (Option.some.inj row)).2
          subst rhs
          obtain ⟨a, rfl, pre, post, ⟨xs, rfl, _⟩, same | ⟨_, same, _⟩⟩ := first
          · cases xs <;> simp_all [returnRhs]
          · cases xs <;> simp_all [returnRhs]
        · change some ((0 : Fin 1), returnRhs) = some (n, rhs) at row
          have hr := (Prod.mk.inj (Option.some.inj row)).2
          subst rhs
          obtain ⟨xs, eq, _⟩ := nullable
          cases xs <;> simp [returnRhs] at eq
    | succ i => split <;> simp
  rank _ := 0
  acyclic := by
    intro p member before child after rhs _
    have eq : p = returnProduction := List.mem_singleton.mp member
    subst p
    have childMem : Sym.n child ∈ returnRhs := by
      rw [show returnRhs = before ++ .n child :: after from rhs]
      exact List.mem_append_right _ (List.mem_cons_self ..)
    simp [returnRhs] at childMem

/-- Closed input to the word verifier. Acceptance and table identity certify
the specific execution the parser consumes. Fix (f) can obtain these proofs
from the general verifier on an independently produced proposal. -/
structure VerificationInputs {count : Nat} (grammar : SharedGrammar count) where
  program : Array VerifyInstruction
  proposal : Array UInt64
  engine : ParserEngine grammar
  spaceBound : proposal.size ≤ grammar.verificationBudget
  accepted : (verifyRun program ⟨0, proposal⟩ grammar.verificationBudget).2 = some true
  exactTable : ∀ i : Fin engine.table.cells.size, ∃ word,
    proposal[i.val]? = some word ∧
      engine.table.cells[i.val]? = some (if word == 0 then none else some (word.toNat - 1))

private def bootstrapWords : Array UInt64 :=
  returnCells.map fun cell => cell.elim 0 (fun i => UInt64.ofNat (i + 1))

/-- With one production every canonical cell encodes either zero or one.
Initialize both comparison words once, then check each proposed cell. -/
private def bootstrapVerifier : Array VerifyInstruction :=
  let size := bootstrapWords.size
  let checks := bootstrapWords.mapIdx fun i word =>
    let expected := if word == 0 then size else size + 1
    VerifyInstruction.ifEqual i expected (i + 3) (size + 3)
  #[.constant size 0, .constant (size + 1) 1] ++ checks ++ #[.halt true, .halt false]

private def bootstrapVerification : VerificationInputs (sliceGrammar slice0) where
  program := bootstrapVerifier
  proposal := (bootstrapWords.push 0).push 0
  -- Include both the typed constructor actions and the shared prefix machine on refusal.
  engine := ⟨bootstrapTable, 64⟩
  spaceBound := of_decide_eq_true (by with_unfolding_all rfl)
  accepted := by with_unfolding_all rfl
  exactTable := by
    intro i
    have hi : i.val < (sliceGrammar slice0).terminalColumns := by
      simpa [bootstrapTable, returnCells, Array.set!_eq_setIfInBounds] using i.isLt
    have entry : returnCells[i.val]? =
        some (if terminalColumn (some .fn) = i.val then some 0 else none) := by
      have fits : terminalColumn (some .fn) < (sliceGrammar slice0).terminalColumns := by decide
      simp only [returnCells, Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds,
        Array.getElem?_replicate, Array.size_replicate, fits, hi, ↓reduceIte, apply_ite some]
    have within : i.val < bootstrapWords.size := by
      simpa only [bootstrapWords, Array.size_map] using (show i.val < returnCells.size from i.isLt)
    have within' : i.val < (bootstrapWords.push 0).size := by simpa using Nat.lt_succ_of_lt within
    change ∃ word, ((bootstrapWords.push 0).push 0)[i.val]? = some word ∧ returnCells[i.val]? = _
    rw [Array.getElem?_push, ite_eq_right (Nat.ne_of_lt within'),
      Array.getElem?_push, ite_eq_right (Nat.ne_of_lt within), bootstrapWords, Array.getElem?_map, entry]
    split <;> first | exact ⟨1, rfl, rfl⟩ | exact ⟨0, rfl, rfl⟩

def sliceVerifierInputs (slice : Slice) : VerificationInputs (sliceGrammar slice) :=
  bootstrapVerification

def sliceParser (slice : Slice) : ParserEngine (sliceGrammar slice) :=
  (sliceVerifierInputs slice).engine

end ConcreteSpec.StageGrammar
