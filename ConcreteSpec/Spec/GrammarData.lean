import ConcreteSpec.Spec.GrammarVocab
import ConcreteSpec.Spec.LL1

/-! Factored recognition grammar for the early parser. Later requirements identify
decided refusal forms. They do not authorize a later slice parser. -/

namespace ConcreteSpec.GrammarData
open Concrete.LL1 ConcreteSpec.Surface0

inductive Nonterminal where
  | program
  | structs
  | returnType
  | localType
  | body
  | letHead
  | letTail
  | conditional
  | alternative
  | elseBody
  | expr
  | sumTail
  | product
  | productTail
  | atom
  | headExpr
  | headSumTail
  | headProduct
  | headProductTail
  | headAtom
  | nameTail
  deriving DecidableEq, Repr

def Nonterminal.count := Nonterminal.nameTail.ctorIdx + 1

def Nonterminal.spelling : Nonterminal → String
  | .program => "Program"
  | .structs => "Structs"
  | .returnType => "ReturnType"
  | .localType => "Type"
  | .body => "Body"
  | .letHead => "LetHead"
  | .letTail => "LetTail"
  | .conditional => "Conditional"
  | .alternative => "Alternative"
  | .elseBody => "ElseBody"
  | .expr => "Expr"
  | .sumTail => "SumTail"
  | .product => "Product"
  | .productTail => "ProductTail"
  | .atom => "Atom"
  | .headExpr => "HeadExpr"
  | .headSumTail => "HeadSumTail"
  | .headProduct => "HeadProduct"
  | .headProductTail => "HeadProductTail"
  | .headAtom => "HeadAtom"
  | .nameTail => "NameTail"

/-- Identities survive projection and changes to the table ordering. -/
inductive ProductionId where
  | program
  | emptyStruct
  | endDecls
  | retU8
  | retI8
  | retI16
  | retI32
  | retI64
  | retU16
  | retU32
  | retU64
  | retBool
  | retUnit
  | retChar
  | retNamed
  | localU8
  | localI8
  | localI16
  | localI32
  | localI64
  | localU16
  | localU32
  | localU64
  | localBool
  | localUnit
  | localChar
  | localNamed
  | binding
  | retStmt
  | callStmt
  | ifStmt
  | endBody
  | named
  | wildcard
  | mutable
  | annotated
  | destructure
  | conditional
  | otherwise
  | endIf
  | elseBlock
  | elseIf
  | expr
  | plus
  | minus
  | endSum
  | product
  | times
  | divide
  | remainder
  | endProduct
  | integer
  | name
  | group
  | move
  | byte
  | trueLit
  | falseLit
  | stringLit
  | headExpr
  | headPlus
  | headMinus
  | headEndSum
  | headProduct
  | headTimes
  | headDivide
  | headRemainder
  | headEndProduct
  | headInteger
  | headName
  | headGroup
  | headMove
  | headByte
  | headTrueLit
  | headFalseLit
  | headStringLit
  | literal
  | endName
  deriving DecidableEq, Repr

/-- The approved construct or token that dates a recognition row. A position
can require both the type or operand and its enclosing construct. -/
inductive Authority where
  | slice0 | component | keyword (t : Terminal) | inResult (type : Authority)
  | numeric | arithmetic | staticStr | inHead (operand : Authority)
  deriving DecidableEq, Repr

def Authority.firstSlice : Authority → Nat
  | .slice0 => 0
  | .component => 1
  | .keyword t => terminalFirstSlice t
  | .inResult type => max 5 type.firstSlice  -- Ordinary functions enter at slice 5.
  | .numeric | .arithmetic => 2
  | .staticStr => 11   -- Q4.1 gives the literal its type. The ballot dates StaticStr.
  | .inHead operand => max (terminalFirstSlice .if_) operand.firstSlice

private def keywordBallot := "docs/grammar/TOKEN-BALLOT.md#reservation-at-the-pre-slice-2-lock"

def Authority.references : Authority → List String
  | .slice0 => ["docs/decisions.md#decided-2026-11-25-freeze-and-grammar"]
  | .component => ["docs/decisions.md#q8-2-revision", "docs/decisions.md#od65",
      "docs/decisions.md#decided-project-vertical-slices-2026-11-06"]
  | .keyword _ => [keywordBallot]
  | .inResult type => "docs/decisions.md#decided-2026-11-27-result-position" :: type.references
  | .numeric => ["docs/grammar/TOKEN-BALLOT.md#slice-2-literals"]
  | .arithmetic => ["docs/grammar/TOKEN-BALLOT.md#other-punctuation-and-boundaries"]
  | .staticStr => ["docs/decisions.md#decided-2026-11-05-core",
      "docs/grammar/TOKEN-BALLOT.md#builtin-names-by-first-consumer"]
  | .inHead operand => keywordBallot :: "docs/decisions.md#od16" :: "docs/decisions.md#od34" :: operand.references

structure ProductionRow where
  lhs : Nonterminal
  rhs : List (Sym Terminal Nonterminal)
  authority : Authority
  /-- Distinguishing lookahead of a restricted form. Scaffolding inherits
  its requirement from the containing form and has no separate diagnostic. -/
  witness : Option Terminal

def ProductionRow.firstSlice (row : ProductionRow) := row.authority.firstSlice

def primitiveReturn (t : Terminal) : ProductionRow :=
  ⟨.returnType, [.t t], .inResult (.keyword t), some t⟩

def ProductionId.row : ProductionId → ProductionRow
  | .program => ⟨.program, [.n .structs, .t .fn, .t .ident, .t .lparen, .t .rparen, .t .arrow, .n .returnType, .t .lbrace, .n .body, .t .rbrace], .slice0, none⟩
  | .emptyStruct => ⟨.structs, [.t .struct_, .t .ident, .t .lbrace, .t .rbrace, .n .structs], .component, some .struct_⟩
  | .endDecls => ⟨.structs, [], .slice0, none⟩
  | .retU8 => ⟨.returnType, [.t .u8], .slice0, none⟩
  | .retI8 => primitiveReturn .i8
  | .retI16 => primitiveReturn .i16
  | .retI32 => primitiveReturn .i32
  | .retI64 => primitiveReturn .i64
  | .retU16 => primitiveReturn .u16
  | .retU32 => primitiveReturn .u32
  | .retU64 => primitiveReturn .u64
  | .retBool => primitiveReturn .bool_
  | .retUnit => primitiveReturn .Unit_
  | .retChar => primitiveReturn .char_
  | .retNamed => ⟨.returnType, [.t .ident], .inResult .component, some .ident⟩
  | .localU8 => ⟨.localType, [.t .u8], .keyword .u8, none⟩
  | .localI8 => ⟨.localType, [.t .i8], .keyword .i8, some .i8⟩
  | .localI16 => ⟨.localType, [.t .i16], .keyword .i16, some .i16⟩
  | .localI32 => ⟨.localType, [.t .i32], .keyword .i32, some .i32⟩
  | .localI64 => ⟨.localType, [.t .i64], .keyword .i64, some .i64⟩
  | .localU16 => ⟨.localType, [.t .u16], .keyword .u16, some .u16⟩
  | .localU32 => ⟨.localType, [.t .u32], .keyword .u32, some .u32⟩
  | .localU64 => ⟨.localType, [.t .u64], .keyword .u64, some .u64⟩
  | .localBool => ⟨.localType, [.t .bool_], .keyword .bool_, some .bool_⟩
  | .localUnit => ⟨.localType, [.t .Unit_], .keyword .Unit_, some .Unit_⟩
  | .localChar => ⟨.localType, [.t .char_], .keyword .char_, some .char_⟩
  | .localNamed => ⟨.localType, [.t .ident], .component, some .ident⟩
  | .binding => ⟨.body, [.t .let_, .n .letHead, .t .equal, .n .expr, .t .semi, .n .body], .slice0, none⟩
  | .retStmt => ⟨.body, [.t .return_, .n .expr, .t .semi, .n .body], .slice0, none⟩
  | .callStmt => ⟨.body, [.t .ident, .t .lparen, .t .rparen, .t .semi, .n .body], .component, some .ident⟩
  | .ifStmt => ⟨.body, [.n .conditional, .n .body], .keyword .if_, none⟩
  | .endBody => ⟨.body, [], .slice0, none⟩
  | .named => ⟨.letHead, [.t .ident, .n .letTail], .slice0, none⟩
  | .wildcard => ⟨.letHead, [.t .wildcard, .t .colon, .n .localType], .keyword .wildcard, some .wildcard⟩
  | .mutable => ⟨.letHead, [.t .mut_, .t .ident, .t .colon, .n .localType], .keyword .mut_, some .mut_⟩
  | .annotated => ⟨.letTail, [.t .colon, .n .localType], .slice0, none⟩
  | .destructure => ⟨.letTail, [.t .lbrace, .t .rbrace], .component, some .lbrace⟩
  | .conditional => ⟨.conditional, [.t .if_, .n .headExpr, .t .lbrace, .n .body, .t .rbrace, .n .alternative], .keyword .if_, some .if_⟩
  | .otherwise => ⟨.alternative, [.t .else_, .n .elseBody], .keyword .else_, none⟩
  | .endIf => ⟨.alternative, [], .keyword .if_, none⟩
  | .elseBlock => ⟨.elseBody, [.t .lbrace, .n .body, .t .rbrace], .keyword .else_, none⟩
  | .elseIf => ⟨.elseBody, [.n .conditional], .keyword .else_, none⟩
  | .expr => ⟨.expr, [.n .product, .n .sumTail], .slice0, none⟩
  | .plus => ⟨.sumTail, [.t .plus, .n .product, .n .sumTail], .slice0, none⟩
  | .minus => ⟨.sumTail, [.t .minus, .n .product, .n .sumTail], .arithmetic, some .minus⟩
  | .endSum => ⟨.sumTail, [], .slice0, none⟩
  | .product => ⟨.product, [.n .atom, .n .productTail], .slice0, none⟩
  | .times => ⟨.productTail, [.t .star, .n .atom, .n .productTail], .arithmetic, some .star⟩
  | .divide => ⟨.productTail, [.t .slash, .n .atom, .n .productTail], .arithmetic, some .slash⟩
  | .remainder => ⟨.productTail, [.t .percent, .n .atom, .n .productTail], .arithmetic, some .percent⟩
  | .endProduct => ⟨.productTail, [], .slice0, none⟩
  | .integer => ⟨.atom, [.t .integer], .slice0, none⟩
  | .name => ⟨.atom, [.t .ident, .n .nameTail], .slice0, none⟩
  | .group => ⟨.atom, [.t .lparen, .n .expr, .t .rparen], .slice0, none⟩
  | .move => ⟨.atom, [.t .move_, .t .ident], .component, some .move_⟩
  | .byte => ⟨.atom, [.t .byteLiteral], .numeric, some .byteLiteral⟩
  | .trueLit => ⟨.atom, [.t .true_], .keyword .true_, some .true_⟩
  | .falseLit => ⟨.atom, [.t .false_], .keyword .false_, some .false_⟩
  | .stringLit => ⟨.atom, [.t .stringLiteral], .staticStr, some .stringLiteral⟩
  | .headExpr => ⟨.headExpr, [.n .headProduct, .n .headSumTail], .inHead .slice0, none⟩
  | .headPlus => ⟨.headSumTail, [.t .plus, .n .headProduct, .n .headSumTail], .inHead .slice0, none⟩
  | .headMinus => ⟨.headSumTail, [.t .minus, .n .headProduct, .n .headSumTail], .inHead .arithmetic, some .minus⟩
  | .headEndSum => ⟨.headSumTail, [], .inHead .slice0, none⟩
  | .headProduct => ⟨.headProduct, [.n .headAtom, .n .headProductTail], .inHead .slice0, none⟩
  | .headTimes => ⟨.headProductTail, [.t .star, .n .headAtom, .n .headProductTail], .inHead .arithmetic, some .star⟩
  | .headDivide => ⟨.headProductTail, [.t .slash, .n .headAtom, .n .headProductTail], .inHead .arithmetic, some .slash⟩
  | .headRemainder => ⟨.headProductTail, [.t .percent, .n .headAtom, .n .headProductTail], .inHead .arithmetic, some .percent⟩
  | .headEndProduct => ⟨.headProductTail, [], .inHead .slice0, none⟩
  | .headInteger => ⟨.headAtom, [.t .integer], .inHead .slice0, none⟩
  | .headName => ⟨.headAtom, [.t .ident], .inHead .slice0, none⟩
  | .headGroup => ⟨.headAtom, [.t .lparen, .n .expr, .t .rparen], .inHead .slice0, none⟩
  | .headMove => ⟨.headAtom, [.t .move_, .t .ident], .inHead .component, some .move_⟩
  | .headByte => ⟨.headAtom, [.t .byteLiteral], .inHead .numeric, some .byteLiteral⟩
  | .headTrueLit => ⟨.headAtom, [.t .true_], .inHead (.keyword .true_), some .true_⟩
  | .headFalseLit => ⟨.headAtom, [.t .false_], .inHead (.keyword .false_), some .false_⟩
  | .headStringLit => ⟨.headAtom, [.t .stringLiteral], .inHead .staticStr, some .stringLiteral⟩
  | .literal => ⟨.nameTail, [.t .lbrace, .t .rbrace], .component, some .lbrace⟩
  | .endName => ⟨.nameTail, [], .slice0, none⟩

def productionIds : List ProductionId :=
  List.ofFn fun i : Fin (ProductionId.endName.ctorIdx + 1) => ProductionId.ofNat i

def AdmittedAt (slice : Nat) (id : ProductionId) : Prop := id.row.firstSlice ≤ slice

def projection (slice : Nat) : List ProductionId :=
  productionIds.filter fun id => id.row.firstSlice ≤ slice

def erase (ids : List ProductionId) : Grammar Terminal Nonterminal :=
  ⟨.program, ids.map fun id => (id.row.lhs, id.row.rhs)⟩

/-- Canonical numeric export for the certificate producer and the word checker.
Production order and repeated alternatives remain visible to indexed prediction. -/
def numberedGrammar (ids : List ProductionId) : Grammar Nat Nat :=
  let g := erase ids
  ⟨g.start.ctorIdx, g.prods.map fun (n,rhs) => (n.ctorIdx, rhs.map fun
    | .t t => .t t.ctorIdx | .n x => .n x.ctorIdx)⟩

/-- Layout for the registered terminal columns. EOF has its own column before
these entries. Integer forms have nine slots per radix (unsuffixed and the
eight integer suffixes). -/
def terminalSlices : Array Nat :=
  Array.ofFn fun i : Fin Terminal.count => terminalFirstSlice (Terminal.ofNat i.val)

def integerSlices : Array Nat :=
  Array.ofFn fun i : Fin ((Radix.hexadecimal.ctorIdx + 1) * 9) =>
    (Radix.ofNat (i.val / 9)).firstSlice

def terminalSpelling (terminal : Terminal) : String :=
  match terminal with
  | .ident => "IDENT"
  | .integer => "INTEGER"
  | .byteLiteral => "BYTE"
  | .stringLiteral => "STRING"
  | .label => "LABEL"
  | .wildcard => "WILDCARD"
  | _ => (((wordTokens ++ punctuation).find? (·.2 == terminal)).map Prod.fst).getD ""

def ProductionId.render (id : ProductionId) : String × List String :=
  (id.row.lhs.spelling, id.row.rhs.map fun
    | .t t => terminalSpelling t
    | .n n => "@" ++ n.spelling)

def renderGrammarForSlice (slice : Nat) : List (String × List String) :=
  (projection slice).map ProductionId.render

end ConcreteSpec.GrammarData
