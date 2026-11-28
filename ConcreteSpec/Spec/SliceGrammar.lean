import ConcreteSpec.StageGrammarCore
import ConcreteSpec.StageInterface
import ConcreteSpec.Spec.EarlyProposal
import ConcreteSpec.Spec.GrammarData
import ConcreteSpec.Spec.GrammarVocab
import ConcreteSpec.Spec.LL1
import ConcreteSpec.Spec.Rules
import ConcreteSpec.Spec.Slice0Rules
import ConcreteSpec.Spec.Surface0
import ConcreteSpec.Surface1

/-! Pinned 78-production early recognition grammar, first-order constructor
actions, static step-per-token budget, registered parser refusal rules, and
authoritative single-pass left-folding CST decoders for Slice 0 and Slice 1. -/

namespace ConcreteSpec.StageGrammar

open ConcreteSpec.StageInterface
open ConcreteSpec.Core (RegisteredRule)
open ConcreteSpec.GrammarData (Nonterminal ProductionId)
open Concrete.LL1

/-- Canonical `Fin Nonterminal.count` index for each grammar nonterminal. -/
def earlyNt (n : Nonterminal) : Fin Nonterminal.count :=
  ⟨n.ctorIdx, by cases n <;> decide⟩

/-- Semantic value family of the 78-production grammar: the start nonterminal
(`program`, index 0) produces `SurfaceAST (fun _ => Empty) slice0`, and every
other nonterminal produces a located `SyntaxTree`. -/
def earlyValue (n : Fin Nonterminal.count) : Type :=
  if n.val = 0 then SurfaceAST (fun _ => Empty) slice0 else SyntaxTree

/-- Right-hand side of a catalog production indexed by `Fin Nonterminal.count`. -/
def earlyRhs (id : ProductionId) : List (Sym Terminal (Fin Nonterminal.count)) :=
  id.row.rhs.map fun
    | .t t => .t t
    | .n x => .n (earlyNt x)

private theorem terminalFirstSlice_lt_22 (t : Terminal) :
    Surface0.terminalFirstSlice t < 22 := by
  cases t <;> decide

private theorem radixFirstSlice_lt_22 (r : Surface0.Radix) :
    r.firstSlice < 22 := by
  cases r <;> decide

private theorem productionFirstSlice_lt_22 (id : ProductionId) :
    id.row.firstSlice < 22 := by
  cases id <;> decide

/-- Pinned terminal first-slice table matching `GrammarData.terminalSlices`. -/
def earlyTerminalSlices : Array Slice :=
  Array.ofFn fun i : Fin Surface0.Terminal.count =>
    ⟨⟨Surface0.terminalFirstSlice (Surface0.Terminal.ofNat i.val),
      terminalFirstSlice_lt_22 (Surface0.Terminal.ofNat i.val)⟩⟩

/-- Pinned integer-form first-slice table matching `GrammarData.integerSlices`. -/
def earlyIntegerSlices : Array Slice :=
  Array.ofFn fun i : Fin ((Surface0.Radix.hexadecimal.ctorIdx + 1) * 9) =>
    ⟨⟨(Surface0.Radix.ofNat (i.val / 9)).firstSlice,
      radixFirstSlice_lt_22 (Surface0.Radix.ofNat (i.val / 9))⟩⟩

private abbrev a0 {A B : Type} : Action (A × B) A :=
  .left .input
private abbrev a1 {A B C : Type} : Action (A × B × C) B :=
  .left (.right .input)
private abbrev a2 {A B C D : Type} : Action (A × B × C × D) C :=
  .left (.right (.right .input))
private abbrev a3 {A B C D E : Type} : Action (A × B × C × D × E) D :=
  .left (.right (.right (.right .input)))
private abbrev a4 {A B C D E F : Type} : Action (A × B × C × D × E × F) E :=
  .left (.right (.right (.right (.right .input))))
private abbrev a5 {A B C D E F G : Type} : Action (A × B × C × D × E × F × G) F :=
  .left (.right (.right (.right (.right (.right .input)))))
private abbrev a6 {A B C D E F G H : Type} : Action (A × B × C × D × E × F × G × H) G :=
  .left (.right (.right (.right (.right (.right (.right .input))))))
private abbrev a8 {A B C D E F G H I J : Type} :
    Action (A × B × C × D × E × F × G × H × I × J) I :=
  .left (.right (.right (.right (.right (.right (.right (.right (.right .input))))))))
private abbrev a9 {A B C D E F G H I J K : Type} :
    Action (A × B × C × D × E × F × G × H × I × J × K) J :=
  .left (.right (.right (.right (.right (.right (.right (.right (.right (.right .input)))))))))

private def mkNode {In : Type} (tag : SurfaceNode) (fields : List (Action In SyntaxTree)) :
    Action In SyntaxTree :=
  .node tag (fields.foldr Action.cons (.constant []))

/-- Strictly constructor-only first-order CST action for each of the 78 catalog
productions. Emits flat located tail spines (`sumTail`, `productTail`) and
distinct `letAnnotated` vs `letDestructure` nodes. -/
def earlyAction : (id : ProductionId) →
    Action (ActionArgs earlyValue (earlyRhs id)) (earlyValue (earlyNt id.row.lhs))
  -- Program & struct declarations
  | .program => .inl (mkNode .program [a0, .leaf a2, a6, a8, .leaf a9])
  | .emptyStruct => mkNode .declList [mkNode .structDecl [.leaf a1], a4]
  | .endDecls => mkNode .declList []
  -- Return types
  | .retU8 | .retI8 | .retI16 | .retI32 | .retI64
  | .retU16 | .retU32 | .retU64 | .retBool | .retUnit | .retChar | .retNamed =>
    .leaf a0
  -- Local types
  | .localU8 | .localI8 | .localI16 | .localI32 | .localI64
  | .localU16 | .localU32 | .localU64 | .localBool | .localUnit | .localChar | .localNamed =>
    .leaf a0
  -- Body statements
  | .binding => mkNode .body [mkNode .binding [.leaf a0, a1, a3, .leaf a4], a5]
  | .retStmt => mkNode .body [mkNode .return_ [.leaf a0, a1, .leaf a2], a3]
  | .callStmt => mkNode .body [mkNode .callStmt [.leaf a0, .leaf a3], a4]
  | .ifStmt => mkNode .body [a0, a1]
  | .endBody => mkNode .body []
  -- Let heads and tails
  | .named => mkNode .letHead [.leaf a0, a1]
  | .wildcard => mkNode .letHead [.leaf a0, mkNode .letAnnotated [a2]]
  | .mutable => mkNode (.extension 0) [.leaf a0, .leaf a1, a3]
  | .annotated => mkNode .letAnnotated [a1]
  | .destructure => mkNode .letDestructure []
  -- Conditionals (Slice 3+)
  | .conditional => mkNode (.extension 1) [.leaf a0, a1, a3, a5]
  | .otherwise => mkNode (.extension 2) [.leaf a0, a1]
  | .endIf => mkNode (.extension 2) []
  | .elseBlock => mkNode (.extension 3) [a1]
  | .elseIf => mkNode (.extension 4) [a0]
  -- Expressions
  | .expr => mkNode .expr [a0, a1]
  | .plus | .minus => mkNode .sumTail [.leaf a0, a1, a2]
  | .endSum => mkNode .sumTail []
  | .product => mkNode .product [a0, a1]
  | .times | .divide | .remainder => mkNode .productTail [.leaf a0, a1, a2]
  | .endProduct => mkNode .productTail []
  | .integer => .leaf a0
  | .name => mkNode .construct [.leaf a0, a1]
  | .group => mkNode .group [.leaf a0, a1, .leaf a2]
  | .move => mkNode .move_ [.leaf a0, .leaf a1]
  | .byte | .trueLit | .falseLit | .stringLit => .leaf a0
  -- Conditional-head expressions (Slice 3+)
  | .headExpr => mkNode .expr [a0, a1]
  | .headPlus | .headMinus => mkNode .sumTail [.leaf a0, a1, a2]
  | .headEndSum => mkNode .sumTail []
  | .headProduct => mkNode .product [a0, a1]
  | .headTimes | .headDivide | .headRemainder => mkNode .productTail [.leaf a0, a1, a2]
  | .headEndProduct => mkNode .productTail []
  | .headInteger | .headName => .leaf a0
  | .headGroup => mkNode .group [.leaf a0, a1, .leaf a2]
  | .headMove => mkNode .move_ [.leaf a0, .leaf a1]
  | .headByte | .headTrueLit | .headFalseLit | .headStringLit => .leaf a0
  -- Name tails
  | .literal => mkNode .construct [.leaf a0, .leaf a1]
  | .endName => mkNode .construct []

/-- Build one typed `Production` from its catalog `ProductionId`. -/
def earlyProduction (id : ProductionId) : Production earlyValue where
  lhs := earlyNt id.row.lhs
  rhs := earlyRhs id
  firstSlice := ⟨⟨id.row.firstSlice, productionFirstSlice_lt_22 id⟩⟩
  action := earlyAction id
  id := id

/-- All 78 typed productions in canonical `GrammarData.productionIds` order. -/
def earlyProductions : Array (Production earlyValue) :=
  (GrammarData.productionIds.map earlyProduction).toArray

/-- The pinned 78-production shared recognition grammar for early slices. -/
def earlySharedGrammar : SharedGrammar Nonterminal.count where
  terminalSlices := earlyTerminalSlices
  terminalSliceCoverage := fun kind => by
    simpa [earlyTerminalSlices] using Surface0.Terminal.index_lt_count kind
  terminalSliceExact := fun i =>
    ⟨Surface0.Terminal.ofNat i.val,
      Surface0.Terminal.index_ofNat ⟨i.val, by simpa [earlyTerminalSlices] using i.isLt⟩⟩
  integerSlices := earlyIntegerSlices
  integerSliceCoverage := fun radix suffix => by
    simpa [earlyIntegerSlices] using
      (show integerForm radix suffix < (Surface0.Radix.hexadecimal.ctorIdx + 1) * 9 by
        cases radix <;> cases suffix <;> first | (rename_i ty; cases ty <;> decide) | decide)
  nonDecimalLater := fun radix suffix nonDecimal => by
    cases radix <;> cases suffix <;> first
      | contradiction
      | (rename_i ty; cases ty <;> decide)
      | decide
  start := earlyNt .program
  value := earlyValue
  extraAst := Empty
  startShape := rfl
  productions := earlyProductions

theorem earlySharedGrammar_rawGrammar :
    earlySharedGrammar.rawGrammar = GrammarVerification.earlyRawGrammar := by
  unfold SharedGrammar.rawGrammar earlySharedGrammar GrammarVerification.earlyRawGrammar
    GrammarData.numberedGrammar GrammarData.erase earlyProductions
  dsimp only [earlyTerminalSlices]
  rw [Array.size_ofFn]
  simp only [List.map_map, Function.comp_def, earlyProduction, earlyRhs, earlyNt]
  rfl

theorem earlySharedGrammar_budget :
    earlySharedGrammar.verificationBudget = GrammarVerification.earlyRawGrammar.budget := by
  rw [← earlySharedGrammar.rawGrammar_budget, earlySharedGrammar_rawGrammar]

/-- Maximum nonterminal rank across all 21 nonterminals in `earlyProposal`. -/
def earlyMaxRank : Nat := 3

/-- Pinned linear step-per-token budget derived statically from frame cost,
maximum RHS nonterminal count, and `earlyMaxRank`. -/
def earlyStepsPerToken : Nat :=
  linearStepsPerToken earlySharedGrammar.maxFrameCost earlySharedGrammar.maxNonterminals earlyMaxRank

/-- Pinned registered rules cited by the early parser engine. -/
def earlyMalformedRule : RegisteredRule :=
  ⟨⟨``ConcreteSpec.Core.Slice0Rules.Parsed, 1⟩, by decide⟩

def earlyUnsupportedProductionRule : RegisteredRule :=
  ⟨⟨``ConcreteSpec.Core.Slice0Rules.AdmittedProduction, 3⟩, by decide⟩

def earlyUnsupportedLiteralRule : RegisteredRule :=
  ⟨⟨``ConcreteSpec.Core.Slice0Rules.DecimalLiteral, 1⟩, by decide⟩

/-! ## Authoritative one-pass left-folding CST decoders -/

private def toSurfaceSpan (span : FileSpan) : Surface0.Span :=
  ⟨span.startOffset, span.endOffset, span.line, span.col, span.file⟩

mutual
  /-- Decode a Slice 0 atom node (decimal integer, local read, or grouped expr). -/
  def decodeAtom0 : SyntaxTree → Option Surface0.Expr
    | .leaf ⟨span, .integer value .decimal suffix⟩ =>
      some (.integer value suffix (toSurfaceSpan span))
    | .node .construct [.leaf ⟨span, .name name⟩, .node .construct []] =>
      some (.local name (toSurfaceSpan span))
    | .node .group [.leaf ⟨_, .fixed .lparen _⟩, inner, .leaf ⟨_, .fixed .rparen _⟩] =>
      decodeExpr0 inner
    | _ => none

  /-- Decode a Slice 0 product node (empty `productTail` only). -/
  def decodeProduct0 : SyntaxTree → Option Surface0.Expr
    | .node .product [atomTree, .node .productTail []] => decodeAtom0 atomTree
    | _ => none

  /-- Left-fold a flat `sumTail` spine of `+` operands in one linear pass. -/
  def decodeSumTail0 (lhs : Surface0.Expr) : SyntaxTree → Option Surface0.Expr
    | .node .sumTail [] => some lhs
    | .node .sumTail [.leaf ⟨opSpan, .fixed .plus _⟩, rhsProd, rest] =>
      match decodeProduct0 rhsProd with
      | none => none
      | some rhs => decodeSumTail0 (.add lhs rhs (toSurfaceSpan opSpan)) rest
    | _ => none

  /-- Decode a Slice 0 expression node. -/
  def decodeExpr0 : SyntaxTree → Option Surface0.Expr
    | .node .expr [headProd, tail] =>
      match decodeProduct0 headProd with
      | none => none
      | some head => decodeSumTail0 head tail
    | _ => none
end

/-- Decode a Slice 0 statement node (`let` binding or `return`). -/
def decodeStatement0 : SyntaxTree → Option Surface0.Statement
  | .node .binding [.leaf ⟨letSpan, .fixed .let_ _⟩,
      .node .letHead [.leaf ⟨nameSpan, .name name⟩,
        .node .letAnnotated [.leaf ⟨_, .fixed .u8 _⟩]],
      rhsTree, .leaf ⟨semiSpan, .fixed .semi _⟩] =>
    match decodeExpr0 rhsTree with
    | none => none
    | some rhs =>
      some (.bind ⟨name, toSurfaceSpan nameSpan, rhs⟩
        (Surface0.statementSpan (toSurfaceSpan letSpan) (toSurfaceSpan semiSpan)))
  | .node .return_ [.leaf ⟨retSpan, .fixed .return_ _⟩, valTree,
      .leaf ⟨semiSpan, .fixed .semi _⟩] =>
    match decodeExpr0 valTree with
    | none => none
    | some val =>
      some (.ret val (Surface0.statementSpan (toSurfaceSpan retSpan) (toSurfaceSpan semiSpan)))
  | _ => none

/-- Decode a Slice 0 right-recursive `body` spine. -/
def decodeBody0 : SyntaxTree → Option (List Surface0.Statement)
  | .node .body [] => some []
  | .node .body [stmtTree, rest] =>
    match decodeStatement0 stmtTree, decodeBody0 rest with
    | some stmt, some stmts => some (stmt :: stmts)
    | _, _ => none
  | _ => none

/-- Single authoritative one-pass left-folding decoder from the shared CST to
`Surface0.Program`, retaining `closeBrace`, literal spans, and full statement spans. -/
def decodeSurface0 : SyntaxTree → Option Surface0.Program
  | .node .program [.node .declList [], .leaf ⟨nameSpan, .name name⟩,
      .leaf ⟨_, .fixed .u8 _⟩, bodyTree, .leaf ⟨closeSpan, .fixed .rbrace _⟩] =>
    match decodeBody0 bodyTree with
    | none => none
    | some stmts =>
      some ⟨name, toSurfaceSpan nameSpan, stmts, toSurfaceSpan closeSpan⟩
  | _ => none

mutual
  /-- Decode a Slice 1 atom node (decimal integer, local read, struct construct,
  move, or grouped expression). -/
  def decodeAtom1 : SyntaxTree → Option Surface1.Expr
    | .leaf ⟨_, .integer value .decimal suffix⟩ =>
      some (.integer value suffix)
    | .node .construct [.leaf ⟨_, .name name⟩, .node .construct []] =>
      some (.local name)
    | .node .construct [.leaf ⟨_, .name name⟩,
        .node .construct [.leaf ⟨_, .fixed .lbrace _⟩, .leaf ⟨_, .fixed .rbrace _⟩]] =>
      some (.construct name)
    | .node .move_ [.leaf ⟨_, .fixed .move_ _⟩, .leaf ⟨_, .name name⟩] =>
      some (.move name)
    | .node .group [.leaf ⟨_, .fixed .lparen _⟩, inner, .leaf ⟨_, .fixed .rparen _⟩] =>
      decodeExpr1 inner
    | _ => none

  /-- Decode a Slice 1 product node (empty `productTail` only). -/
  def decodeProduct1 : SyntaxTree → Option Surface1.Expr
    | .node .product [atomTree, .node .productTail []] => decodeAtom1 atomTree
    | _ => none

  /-- Left-fold a Slice 1 `sumTail` spine of `+` operands in one linear pass. -/
  def decodeSumTail1 (lhs : Surface1.Expr) : SyntaxTree → Option Surface1.Expr
    | .node .sumTail [] => some lhs
    | .node .sumTail [.leaf ⟨opSpan, .fixed .plus _⟩, rhsProd, rest] =>
      match decodeProduct1 rhsProd with
      | none => none
      | some rhs => decodeSumTail1 (.add lhs rhs (toSurfaceSpan opSpan)) rest
    | _ => none

  /-- Decode a Slice 1 expression node. -/
  def decodeExpr1 : SyntaxTree → Option Surface1.Expr
    | .node .expr [headProd, tail] =>
      match decodeProduct1 headProd with
      | none => none
      | some head => decodeSumTail1 head tail
    | _ => none
end

/-- Decode a Slice 1 local type leaf (`u8` or nominal struct name). -/
def decodeType1 : SyntaxTree → Option ConcreteSpec.Ty
  | .leaf ⟨_, .fixed .u8 _⟩ => some Surface1.u8
  | .leaf ⟨_, .name name⟩ => some (.struct ⟨name⟩)
  | _ => none

/-- Decode a Slice 1 statement node (typed binding, destructure, wildcard,
return, or zero-argument call statement). -/
def decodeStatement1 : SyntaxTree → Option Surface1.Statement
  | .node .binding [.leaf ⟨_, .fixed .let_ _⟩,
      .node .letHead [.leaf ⟨nameSpan, .name name⟩, .node .letAnnotated [tyTree]],
      rhsTree, .leaf ⟨_, .fixed .semi _⟩] =>
    match decodeType1 tyTree, decodeExpr1 rhsTree with
    | some ty, some rhs => some (.bind name (.typed ty) rhs (toSurfaceSpan nameSpan))
    | _, _ => none
  | .node .binding [.leaf ⟨_, .fixed .let_ _⟩,
      .node .letHead [.leaf ⟨nameSpan, .name name⟩, .node .letDestructure []],
      rhsTree, .leaf ⟨_, .fixed .semi _⟩] =>
    match decodeExpr1 rhsTree with
    | some rhs => some (.bind name .destructure rhs (toSurfaceSpan nameSpan))
    | none => none
  | .node .binding [.leaf ⟨_, .fixed .let_ _⟩,
      .node .letHead [.leaf ⟨markSpan, .fixed .wildcard _⟩, .node .letAnnotated [tyTree]],
      rhsTree, .leaf ⟨_, .fixed .semi _⟩] =>
    match decodeType1 tyTree, decodeExpr1 rhsTree with
    | some ty, some rhs => some (.bind "_" (.typed ty) rhs (toSurfaceSpan markSpan))
    | _, _ => none
  | .node .return_ [.leaf ⟨_, .fixed .return_ _⟩, valTree, .leaf ⟨_, .fixed .semi _⟩] =>
    match decodeExpr1 valTree with
    | some val => some (.ret val)
    | none => none
  | .node .callStmt [.leaf ⟨siteSpan, .name name⟩, .leaf ⟨_, .fixed .semi _⟩] =>
    some (.call name (toSurfaceSpan siteSpan))
  | _ => none

/-- Decode a Slice 1 right-recursive `body` spine. -/
def decodeBody1 : SyntaxTree → Option (List Surface1.Statement)
  | .node .body [] => some []
  | .node .body [stmtTree, rest] =>
    match decodeStatement1 stmtTree, decodeBody1 rest with
    | some stmt, some stmts => some (stmt :: stmts)
    | _, _ => none
  | _ => none

/-- Decode a Slice 1 right-recursive `declList` spine of empty struct names. -/
def decodeDecls1 : SyntaxTree → Option (List String)
  | .node .declList [] => some []
  | .node .declList [.node .structDecl [.leaf ⟨_, .name name⟩], rest] =>
    match decodeDecls1 rest with
    | some names => some (name :: names)
    | none => none
  | _ => none

/-- Single authoritative one-pass left-folding decoder from the shared CST to
`Surface1.SourceProgram`. -/
def decodeSurface1 : SyntaxTree → Option Surface1.SourceProgram
  | .node .program [declsTree, .leaf ⟨_, .name name⟩, .leaf ⟨_, .fixed .u8 _⟩,
      bodyTree, .leaf ⟨_, .fixed .rbrace _⟩] =>
    match decodeDecls1 declsTree, decodeBody1 bodyTree with
    | some decls, some stmts => some ⟨decls, name, stmts⟩
    | _, _ => none
  | _ => none

end ConcreteSpec.StageGrammar
