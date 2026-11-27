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

/-- Decision references are shared by rows with the same production authority. -/
inductive Authority where
  | slice0 | slice1 | ballot | numeric | arithmetic | conditional | functions | strings
  deriving DecidableEq, Repr

def Authority.reference : Authority → String
  | .slice0 => "docs/decisions.md#decided-2026-11-25-freeze-and-grammar"
  | .slice1 => "docs/grammar/README.md#production-provenance-and-remaining-approvals"
  | .ballot => "docs/decisions.md#decided-2026-11-27-token-ballot"
  | .numeric => "docs/grammar/TOKEN-BALLOT.md#slice-2-literals"
  -- The ballot records the arithmetic use and the Slice 2 row of ROADMAP.md#vertical-slices
  -- lists the binary operators. The precise precedence productions await slice approval.
  | .arithmetic => "docs/grammar/TOKEN-BALLOT.md#other-punctuation-and-boundaries"
  | .conditional => "docs/grammar/slices-0-11.grammar [3]"
  | .functions => "docs/grammar/slices-0-11.grammar [5] (compatibility draft)"
  | .strings => "docs/grammar/slices-0-11.grammar [11]"

structure ProductionRow where
  lhs : Nonterminal
  rhs : List (Sym Terminal Nonterminal)
  firstSlice : Nat
  authority : Authority
  /-- Distinguishing lookahead of a restricted form. Scaffolding inherits
  its requirement from the containing form and has no separate diagnostic. -/
  witness : Option Terminal

/-- The approved early signature returns u8. Section 5 of the compatibility
draft opens the general type position. Q1 supplies the keyword's own first
consumer. The exact Slice 5 production delta still awaits owner approval. -/
def primitiveReturn (t : Terminal) : ProductionRow :=
  ⟨.returnType, [.t t], max 5 (terminalFirstSlice t), .functions, some t⟩

def ProductionId.row : ProductionId → ProductionRow
  | .program => ⟨.program, [.n .structs, .t .fn, .t .ident, .t .lparen, .t .rparen, .t .arrow, .n .returnType, .t .lbrace, .n .body, .t .rbrace], 0, .slice0, none⟩
  | .emptyStruct => ⟨.structs, [.t .struct_, .t .ident, .t .lbrace, .t .rbrace, .n .structs], 1, .slice1, some .struct_⟩
  | .endDecls => ⟨.structs, [], 0, .slice0, none⟩
  | .retU8 => ⟨.returnType, [.t .u8], 0, .slice0, none⟩
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
  | .retNamed => ⟨.returnType, [.t .ident], 5, .functions, some .ident⟩
  | .localU8 => ⟨.localType, [.t .u8], terminalFirstSlice .u8, .ballot, none⟩
  | .localI8 => ⟨.localType, [.t .i8], terminalFirstSlice .i8, .ballot, some .i8⟩
  | .localI16 => ⟨.localType, [.t .i16], terminalFirstSlice .i16, .ballot, some .i16⟩
  | .localI32 => ⟨.localType, [.t .i32], terminalFirstSlice .i32, .ballot, some .i32⟩
  | .localI64 => ⟨.localType, [.t .i64], terminalFirstSlice .i64, .ballot, some .i64⟩
  | .localU16 => ⟨.localType, [.t .u16], terminalFirstSlice .u16, .ballot, some .u16⟩
  | .localU32 => ⟨.localType, [.t .u32], terminalFirstSlice .u32, .ballot, some .u32⟩
  | .localU64 => ⟨.localType, [.t .u64], terminalFirstSlice .u64, .ballot, some .u64⟩
  | .localBool => ⟨.localType, [.t .bool_], terminalFirstSlice .bool_, .ballot, some .bool_⟩
  | .localUnit => ⟨.localType, [.t .Unit_], terminalFirstSlice .Unit_, .ballot, some .Unit_⟩
  | .localChar => ⟨.localType, [.t .char_], terminalFirstSlice .char_, .ballot, some .char_⟩
  | .localNamed => ⟨.localType, [.t .ident], 1, .slice1, some .ident⟩
  | .binding => ⟨.body, [.t .let_, .n .letHead, .t .equal, .n .expr, .t .semi, .n .body], 0, .slice0, none⟩
  | .retStmt => ⟨.body, [.t .return_, .n .expr, .t .semi, .n .body], 0, .slice0, none⟩
  | .callStmt => ⟨.body, [.t .ident, .t .lparen, .t .rparen, .t .semi, .n .body], 1, .slice1, some .ident⟩
  | .ifStmt => ⟨.body, [.n .conditional, .n .body], 3, .conditional, none⟩
  | .endBody => ⟨.body, [], 0, .slice0, none⟩
  | .named => ⟨.letHead, [.t .ident, .n .letTail], 0, .slice0, none⟩
  | .wildcard => ⟨.letHead, [.t .wildcard, .t .colon, .n .localType], 1, .slice1, some .wildcard⟩
  | .mutable => ⟨.letHead, [.t .mut_, .t .ident, .t .colon, .n .localType], 3, .conditional, some .mut_⟩
  | .annotated => ⟨.letTail, [.t .colon, .n .localType], 0, .slice0, none⟩
  | .destructure => ⟨.letTail, [.t .lbrace, .t .rbrace], 1, .slice1, some .lbrace⟩
  | .conditional => ⟨.conditional, [.t .if_, .n .headExpr, .t .lbrace, .n .body, .t .rbrace, .n .alternative], 3, .conditional, some .if_⟩
  | .otherwise => ⟨.alternative, [.t .else_, .n .elseBody], 3, .conditional, none⟩
  | .endIf => ⟨.alternative, [], 3, .conditional, none⟩
  | .elseBlock => ⟨.elseBody, [.t .lbrace, .n .body, .t .rbrace], 3, .conditional, none⟩
  | .elseIf => ⟨.elseBody, [.n .conditional], 3, .conditional, none⟩
  | .expr => ⟨.expr, [.n .product, .n .sumTail], 0, .slice0, none⟩
  | .plus => ⟨.sumTail, [.t .plus, .n .product, .n .sumTail], 0, .slice0, none⟩
  | .minus => ⟨.sumTail, [.t .minus, .n .product, .n .sumTail], 2, .arithmetic, some .minus⟩
  | .endSum => ⟨.sumTail, [], 0, .slice0, none⟩
  | .product => ⟨.product, [.n .atom, .n .productTail], 0, .slice0, none⟩
  | .times => ⟨.productTail, [.t .star, .n .atom, .n .productTail], 2, .arithmetic, some .star⟩
  | .divide => ⟨.productTail, [.t .slash, .n .atom, .n .productTail], 2, .arithmetic, some .slash⟩
  | .remainder => ⟨.productTail, [.t .percent, .n .atom, .n .productTail], 2, .arithmetic, some .percent⟩
  | .endProduct => ⟨.productTail, [], 0, .slice0, none⟩
  | .integer => ⟨.atom, [.t .integer], 0, .slice0, none⟩
  | .name => ⟨.atom, [.t .ident, .n .nameTail], 0, .slice0, none⟩
  | .group => ⟨.atom, [.t .lparen, .n .expr, .t .rparen], 0, .slice0, none⟩
  | .move => ⟨.atom, [.t .move_, .t .ident], 1, .slice1, some .move_⟩
  | .byte => ⟨.atom, [.t .byteLiteral], 2, .numeric, some .byteLiteral⟩
  | .trueLit => ⟨.atom, [.t .true_], 3, .conditional, some .true_⟩
  | .falseLit => ⟨.atom, [.t .false_], 3, .conditional, some .false_⟩
  | .stringLit => ⟨.atom, [.t .stringLiteral], 11, .strings, some .stringLiteral⟩
  | .headExpr => ⟨.headExpr, [.n .headProduct, .n .headSumTail], 3, .conditional, none⟩
  | .headPlus => ⟨.headSumTail, [.t .plus, .n .headProduct, .n .headSumTail], 3, .conditional, none⟩
  | .headMinus => ⟨.headSumTail, [.t .minus, .n .headProduct, .n .headSumTail], 3, .arithmetic, some .minus⟩
  | .headEndSum => ⟨.headSumTail, [], 3, .conditional, none⟩
  | .headProduct => ⟨.headProduct, [.n .headAtom, .n .headProductTail], 3, .conditional, none⟩
  | .headTimes => ⟨.headProductTail, [.t .star, .n .headAtom, .n .headProductTail], 3, .arithmetic, some .star⟩
  | .headDivide => ⟨.headProductTail, [.t .slash, .n .headAtom, .n .headProductTail], 3, .arithmetic, some .slash⟩
  | .headRemainder => ⟨.headProductTail, [.t .percent, .n .headAtom, .n .headProductTail], 3, .arithmetic, some .percent⟩
  | .headEndProduct => ⟨.headProductTail, [], 3, .conditional, none⟩
  | .headInteger => ⟨.headAtom, [.t .integer], 3, .conditional, none⟩
  | .headName => ⟨.headAtom, [.t .ident], 3, .conditional, none⟩
  | .headGroup => ⟨.headAtom, [.t .lparen, .n .expr, .t .rparen], 3, .conditional, none⟩
  | .headMove => ⟨.headAtom, [.t .move_, .t .ident], 3, .slice1, some .move_⟩
  | .headByte => ⟨.headAtom, [.t .byteLiteral], 3, .numeric, some .byteLiteral⟩
  | .headTrueLit => ⟨.headAtom, [.t .true_], 3, .conditional, some .true_⟩
  | .headFalseLit => ⟨.headAtom, [.t .false_], 3, .conditional, some .false_⟩
  | .headStringLit => ⟨.headAtom, [.t .stringLiteral], 11, .strings, some .stringLiteral⟩
  | .literal => ⟨.nameTail, [.t .lbrace, .t .rbrace], 1, .slice1, some .lbrace⟩
  | .endName => ⟨.nameTail, [], 0, .slice0, none⟩

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

def renderGrammarForSlice (slice : Nat) : List (String × List String) :=
  (projection slice).map fun id =>
    (id.row.lhs.spelling, id.row.rhs.map fun
      | .t t => terminalSpelling t
      | .n n => "@" ++ n.spelling)

end ConcreteSpec.GrammarData
