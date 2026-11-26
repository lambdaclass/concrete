import ConcreteSpec.Spec.Surface0
import Std.Data.HashMap

/-! Standalone draft qualification. Run from the repository root. -/
open Concrete.LL1

abbrev Rule := String × List String
abbrev DraftGrammar := Grammar String String

def words (line : String) : List String :=
  (line.splitOn " ").filter (!·.isEmpty)

def readSections (source : String) : IO (Array (List Rule)) := do
  let mut sections := #[]
  for raw in source.splitOn "\n" do
    let line := raw.trimAscii.toString
    if line.isEmpty || line.startsWith "#" then continue
    if line == s!"[{sections.size}]" then
      sections := sections.push []
    else
      match words line with
      | lhs :: "=" :: rhs =>
        if sections.isEmpty then throw (IO.userError "missing slice header")
        sections := sections.modify (sections.size - 1) (· ++ [(lhs, rhs)])
      | _ => throw (IO.userError s!"invalid production: {line}")
  if sections.size != 12 || sections.any List.isEmpty then
    throw (IO.userError "expected twelve nonempty slices")
  pure sections

def extend (prior delta : List Rule) : List Rule :=
  prior.filter (fun (lhs, _) => !delta.any (fun rule => rule.1 == lhs)) ++ delta

def headLadder : List String :=
  ["Expr", "LogicOr", "LogicOrTail", "LogicAnd", "LogicAndTail", "Equality", "EqualityTail", "Compare", "CompareTail",
   "CompareOp", "BitOr", "OrBits", "BitXor", "XorBits", "BitAnd", "AndBits", "Shift",
   "ShiftTail", "Sum", "SumTail", "Product", "ProductTail", "Unary"]

def withExpressionLadder (namePrefix : String) (rules : List Rule) : List Rule :=
  let renameSymbol (sym : String) :=
    if sym == "@Atom" then "@" ++ namePrefix ++ "Atom"
    else if headLadder.any (fun name => sym == "@" ++ name) then
      "@" ++ namePrefix ++ (sym.drop 1).toString
    else sym
  rules ++ rules.filterMap fun (lhs, rhs) =>
    if headLadder.contains lhs then some (namePrefix ++ lhs, rhs.map renameSymbol) else none

-- Rename only the left edge. Operands reached after a prefix or binary
-- operator still use the full value-expression grammar.
def withLeadingOperand (namePrefix : String) (rules : List Rule) : List Rule :=
  let spine := ["Expr", "LogicOr", "LogicAnd", "Equality", "Compare", "BitOr", "BitXor",
    "BitAnd", "Shift", "Sum", "Product", "Unary", "Atom"]
  rules.filterMap fun (lhs, rhs) =>
    match rhs with
    | sym :: tail =>
      if !spine.contains lhs || lhs == "Atom" then none
      else
        let leading := if spine.any (fun name => sym == "@" ++ name) then
          "@" ++ namePrefix ++ (sym.drop 1).toString else sym
        some (namePrefix ++ lhs, leading :: tail)
    | [] => none

def addExpressionLadders (slice : Nat) (rules : List Rule) : List Rule := Id.run do
  if slice < 3 then return rules
  let tails := ["ProductTail", "SumTail", "ShiftTail", "AndBits", "XorBits", "OrBits",
    "CompareTail", "EqualityTail", "LogicAndTail", "LogicOrTail"]
  let continuation := ("ExprContinuation", tails.map ("@" ++ ·))
  let bodyRules := rules.filter fun (lhs, rhs) =>
    (lhs != "Unary" || !["*", "@AttributePrefix"].contains (rhs.headD "")) &&
    (slice < 7 || (lhs, rhs) != ("CommonAtom", ["@GroupAtom"]))
  -- A match can continue as a trailing value on a binary-only token. A
  -- token that also begins a body item takes the statement boundary.
  let itemStarters := if slice >= 7 then ["-", "*", "&"] else ["-"]
  let matchContinuations := tails.zipIdx.flatMap fun (tail, i) =>
    rules.filterMap fun (lhs, rhs) => do
      let token ← rhs.head?
      if lhs == tail && !itemStarters.contains token then
        some ("MatchEnd", rhs ++ ((tails.drop (i + 1)).map ("@" ++ ·)))
      else none
  let bodyAtoms := if slice >= 7 then
      bodyRules.filterMap fun (lhs, rhs) => if lhs == "CommonAtom" then some ("BodyAtom", rhs) else none
    else [("BodyAtom", ["@CommonAtom"])]
  let body := withLeadingOperand "Body" bodyRules ++ bodyAtoms
  let body := if slice >= 7 then body ++ [("Body", ["@GroupedBody"])] else body
  let expanded := withExpressionLadder "Head" rules ++ body ++ [continuation] ++
    (if slice >= 6 then matchContinuations else [])
  if slice < 8 then return expanded
  let scalarRules := rules.filter fun (lhs, rhs) =>
    lhs != "Unary" || ["-", "!", "BIT_NOT", "@Atom"].contains (rhs.headD "")
  return expanded ++ ((withExpressionLadder "Const" scalarRules).drop scalarRules.length)

structure Options where
  bitNot : String
  primitiveKeyword : Bool
  wildcardToken : Bool
  alternateAttribute : Bool
  borrowWord : String
  range : String × String
  binderMark : String
  importWord : String
  prefixBorrowBinder : Bool
  builtinTargetType : Bool
  earlyTrustWords : Bool
  mathKeyword : Bool
  deriving Inhabited, Repr

def representative : Options :=
  ⟨"~", true, true, false, "borrow", ("from", "until"), "->", "import", false, false, false, true⟩

-- Share the braces with ordinary named literals. After an identifier
-- inside the braces, a colon starts an initializer and the ordinary
-- expression tails continue an exact-arithmetic operand.
def withIdentifierMath (rules : List Rule) : List Rule :=
  let hasFields := rules.contains ("NameLiteral", ["{", "@Initializers", "}"])
  let namedTails := rules.filterMap fun (lhs, rhs) =>
    match lhs, rhs with
    | "Atom", "IDENT" :: tail => some ("MathValue", tail ++ ["@ExprContinuation"])
    | _, _ => none
  let otherAtoms := rules.filterMap fun (lhs, rhs) =>
    if lhs == "Atom" && rhs.head? != some "IDENT" then some ("MathOtherAtom", rhs) else none
  rules.filter (fun (lhs, rhs) => lhs != "NameLiteral" && rhs != ["@ExactArithmetic"]) ++
    withLeadingOperand "MathOther" rules ++ otherAtoms ++ namedTails ++ [
      ("NameLiteral", ["{", "@BraceContents", "}"]),
      ("BraceContents", ["IDENT", "@BraceAfterName"]),
      ("BraceContents", ["@MathOtherValue"]),
      ("BraceContents", []),
      ("BraceAfterName", ["@MathValue"]),
      ("MathOtherValue", ["@MathOtherExpr"])] ++
    (if hasFields then [("BraceAfterName", [":", "@Expr", "@InitializerTail"])] else [])

def instantiate (options : Options) (rules : List Rule) : DraftGrammar :=
  let rules := if !options.mathKeyword && rules.any (·.1 == "ExactArithmetic")
    then withIdentifierMath rules else rules
  let replace (sym : String) :=
    match sym with
    | "BIT_NOT" => options.bitNot
    | "WILDCARD" => if options.wildcardToken then "_" else "RESERVED_UNDERSCORE"
    | "BORROW_WORD" => options.borrowWord
    | "RANGE_START" => options.range.1
    | "RANGE_END" => options.range.2
    | "BINDER_MARK" => options.binderMark
    | "IMPORT_WORD" => options.importWord
    | _ => sym
  let allows : Rule → Bool := fun (lhs, rhs) =>
    (options.primitiveKeyword || rhs.head? != some "PRIMITIVE") &&
    !(options.bitNot == "!" && rhs.head? == some "BIT_NOT" &&
      rules.contains (lhs, "!" :: rhs.drop 1))
  let rules := rules.filter allows
  let rules := if options.alternateAttribute then
    rules ++ [("Attribute", ["@[", "IDENT", "@AttrArgs", "]"])]
    else rules
  let rules := if options.builtinTargetType then rules ++ [
    ("EarlyArgs", ["@TargetType", "@Expr", "@SecondArg"]),
    ("CallArgs", ["@TargetType", "@Expr", "@ArgTail"]),
    ("CallArgs", ["@Args"]),
    ("ConstArgs", ["@TargetType", "@ConstExpr", "@ConstSecondArg"]),
    ("TargetType", ["type", ":", "@Type", ","])]
    else rules
  let rules := rules.map fun rule =>
    if options.builtinTargetType && rule == ("NameCall", ["(", "@Args", ")"]) then
      ("NameCall", ["(", "@CallArgs", ")"])
    else rule
  let rules := rules.map fun (lhs, rhs) =>
    let replaceForm (sym : String) :=
      if sym == "@BorrowForm" then
        if options.prefixBorrowBinder then "@PrefixBorrow" else "@SuffixBorrow"
      else replace sym
    (lhs, rhs.map replaceForm)
  ⟨"Program", rules.map fun (lhs, rhs) => (lhs, rhs.map fun sym =>
    if sym.startsWith "@" && sym != "@[" then .n (sym.drop 1).toString else .t sym)⟩

def reachableProjection (g : DraftGrammar) : DraftGrammar :=
  let reachable := saturate (fun seen => g.prods.flatMap fun (lhs, rhs) =>
    if seen.contains lhs then rhs.filterMap (fun | .n name => some name | .t _ => none)
    else []) g.prods.length [g.start]
  { g with prods := g.prods.filter fun (lhs, _) => reachable.contains lhs }

-- The repository's ll1 checks conflict freedom. Also check definedness and productivity
-- so a missing or circular draft rule cannot make a conflict disappear.
def productive (g : DraftGrammar) : Bool :=
  let defined := (g.prods.map Prod.fst).eraseDups
  let live := saturate (fun live => g.prods.filterMap fun (lhs, rhs) =>
    if rhs.all (fun | .t _ => true | .n sym => live.contains sym) then some lhs else none)
    defined.length []
  live.contains g.start && g.prods.all fun (lhs, rhs) =>
    live.contains lhs && rhs.all (fun | .t _ => true | .n sym => live.contains sym)

abbrev PredictionTable := Std.HashMap (String × Option String) (List (Sym String String))

def requireLL1 (label : String) (g : DraftGrammar) : IO PredictionTable := do
  unless productive g do throw (IO.userError s!"{label}: undefined or unproductive")
  let (nu, fi, fo) := compute g
  let predictions := g.prods.map fun (lhs, rhs) =>
    (lhs, rhs, (predictB nu fi fo lhs rhs).eraseDups)
  let entries := predictions.flatMap fun (lhs, rhs, look) =>
    look.map fun token => ((lhs, token), rhs)
  let table := Std.HashMap.ofList entries
  unless nuClosed g nu && fiClosed g nu fi && foClosed g nu fi fo && table.size == entries.length do
    for ((lhs, rhs, look), i) in predictions.zipIdx do
      for (other, alt, next) in predictions.drop (i+1) do
        let overlap := look.filter next.contains
        if lhs == other && !overlap.isEmpty then
          IO.eprintln s!"{lhs}: {repr rhs} vs {repr alt}, lookahead {repr overlap}"
    throw (IO.userError s!"{label}: LL(1) conflict")
  return table

structure Token where
  kind : String
  spelling : String
  deriving Inhabited

-- Reserved source words are independent of grammar reachability. This
-- includes every non-identifier spelling of Surface0.word on main.
def mainReservedWords : List String :=
  ["fn", "let", "return", "u8", "struct", "move", "spec", "by", "kept", "Self",
   "while", "Entropy", "from", "until", "cap", "type", "ghost", "Copy",
   "Fs", "Network", "Env", "Time", "Alloc", "Device", "Console", "Process", "Unsafe"]

def draftReservedWords (slice : Nat) (options : Options) : List String :=
  mainReservedWords ++ ["for", "break", "continue", "pub", "const", "mod"] ++
    (if slice >= 3 then ["if", "else", "mut", "true", "false"] else []) ++
    (if slice >= 4 && options.mathKeyword then ["math"] else []) ++
    (if slice >= 6 then ["enum", "match"] else []) ++
    (if slice >= 7 then [options.borrowWord] else []) ++
    (if slice >= 8 then [options.importWord] else []) ++
    (if slice >= 9 then ["trait", "impl", "defer"] else []) ++
    (if slice >= 10 then ["assert", "assume"] else []) ++
    (if slice >= 11 then ["with"] else []) ++
    (if options.earlyTrustWords then ["trusted", "extern"] else [])

structure LiteralOptions where
  digitSeparators : Bool := false
  separatorAfterPrefix : Bool := false
  upperHex : Bool := false
  unicodeEscape : Bool := false
  stringHexEscape : Bool := false
  byteHexEscape : Bool := false

def hexDigit (upper : Bool) (c : Char) : Option Nat :=
  if c.isDigit then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else if upper && 'A' ≤ c && c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

def integerSuffix (policy : LiteralOptions) (word : List Char) : Option (List Char) := do
  let (base, digits) := match word with
    | '0' :: 'x' :: rest => (16, rest)
    | '0' :: 'o' :: rest => (8, rest)
    | '0' :: 'b' :: rest => (2, rest)
    | _ => (10, word)
  let isDigit := fun c => (hexDigit policy.upperHex c).any (· < base)
  let first := if base != 10 && policy.digitSeparators && policy.separatorAfterPrefix &&
      digits.head? == some '_' then digits.drop 1 else digits
  let rec consume : List Char → List Char
    | c :: next :: rest =>
      if isDigit c then
        if next == '_' && policy.digitSeparators && (rest.head?).any isDigit then consume rest
        else consume (next :: rest)
      else c :: next :: rest
    | [c] => if isDigit c then [] else [c]
    | [] => []
  guard ((first.head?).any isDigit)
  let suffix := consume first
  guard (suffix.isEmpty ||
    ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"].contains (String.ofList suffix))
  guard (base != 10 || first.head? != some '0' || first == '0' :: suffix)
  return suffix

def hexValue (digits : List Char) : Option Nat :=
  digits.foldlM (fun acc digit => (hexDigit true digit).map (acc * 16 + ·)) 0

def escapeTail (policy : LiteralOptions) (quote : Char) (input : List Char) : Option (List Char) := do
  match input with
  | 'x' :: a :: b :: rest =>
    let value ← hexValue [a, b]
    guard (if quote == '"' then policy.stringHexEscape && value ≤ 127 else policy.byteHexEscape)
    return rest
  | 'u' :: '{' :: rest =>
    guard (quote == '"' && policy.unicodeEscape)
    let digits := rest.takeWhile (hexDigit true · |>.isSome)
    let '}' :: tail := rest.drop digits.length | none
    let value ← hexValue digits
    guard (!digits.isEmpty && value < 0x110000 && !(0xd800 ≤ value && value < 0xe000))
    return tail
  | escaped :: rest =>
    guard (['n', 't', 'r', '0', '\\', quote].contains escaped)
    return rest
  | [] => none

partial def quotedBody (policy : LiteralOptions) (input acc : List Char) :
    Option (List Char × List Char) :=
  match input with
  | [] => none
  | '\\' :: rest => do
    let tail ← escapeTail policy '"' rest
    let escape := '\\' :: rest.take (rest.length - tail.length)
    quotedBody policy tail (escape.reverse ++ acc)
  | next :: rest =>
    if next == '"' then some (acc.reverse, rest)
    else if ConcreteSpec.Surface0.StringRaw next.toNat then quotedBody policy rest (next :: acc)
    else none

def draftTokens (slice : Nat) (options : Options) (source : String)
    (literals : LiteralOptions := {}) : List Token := Id.run do
  let fixed := draftReservedWords slice options
  let primitives := (if slice >= 2 then ["i8", "i16", "i32", "i64", "u16", "u32", "u64"] else []) ++
    (if slice >= 3 then ["bool", "Unit"] else []) ++ (if slice >= 11 then ["char"] else [])
  let wordChar (c : Char) := c.isAlphanum || c == '_'
  let punct := [">>", "<<", "::", "=>", "->", "<=", ">=", "==", "!=", "&&", "||",
    "..", "#[", "@["]
  let mut remaining := source.toList
  -- Lean Strings already contain valid Unicode scalars. Refuse the remaining
  -- SourceOk violations before tokenizing, including inside any literal.
  if remaining.any (fun c => c.toNat == 0 || c.toNat == 0xFEFF) then
    return [⟨"INVALID", ""⟩]
  let mut output := []
  for _ in [:remaining.length] do
    match remaining with
    | [] => break
    | c :: tail =>
      if c.isWhitespace then remaining := tail; continue
      let (spelling, kind, rest) :=
        if wordChar c then
          let word := remaining.takeWhile wordChar
          let w := String.ofList word
          let k :=
            if w == "_" && slice >= 1 then
              if options.wildcardToken then "_" else "RESERVED_UNDERSCORE"
            else if fixed.contains w then w
            else if options.primitiveKeyword && primitives.contains w then "PRIMITIVE"
            else if c.isDigit then if (integerSuffix literals word).isSome then "INTEGER" else "INVALID"
            else "IDENT"
          (w, k, remaining.drop word.length)
        else if c == '\'' || c == '"' then
          if c == '"' then
            match quotedBody literals tail [] with
            | some (content, rest) => (String.ofList content, "STRING", rest)
            | none => ("", "INVALID", [])
          else
            match tail with
            | '\\' :: escaped =>
              match escapeTail literals c escaped with
              | some ('\'' :: rest) =>
                (String.ofList (tail.take (tail.length - rest.length - 1)), "BYTE", rest)
              | _ => ("", "INVALID", [])
            | value :: '\'' :: rest =>
              (String.singleton value, if 32 ≤ value.toNat && value.toNat ≤ 126 then "BYTE" else "INVALID", rest)
            | first :: _ =>
              if first.isAlpha || first == '_' then
                let label := tail.takeWhile wordChar
                (String.ofList label, "LABEL", tail.drop label.length)
              else ("", "INVALID", tail)
            | [] => ("", "INVALID", [])
        else
          let w := (punct.find? (fun w => w.toList.isPrefixOf remaining)).getD (String.singleton c)
          (w, w, remaining.drop w.length)
      output := ⟨kind, spelling⟩ :: output
      remaining := rest
  return output.reverse

inductive Tree where
  | leaf (token : Token)
  | node (name : String) (children : List Tree)
  deriving Inhabited

-- Maximal munch keeps >> intact. Fall back to single-angle prediction
-- through nullable tails, but split only at a generic closing delimiter.
partial def parseSymbols (table : PredictionTable) (splitClosers genericRhs : Bool)
    (fuel : Nat) (symbols : List (Sym String String)) (input : List Token) :
    Option (List Tree × List Token) := do
  if fuel == 0 then none else
    match symbols with
    | [] => some ([], input)
    | .t kind :: rest =>
      let token :: tail := input | none
      let tail ← if token.kind == kind then some tail
        else if splitClosers && genericRhs && kind == ">" && token.kind == ">>" then
          some (⟨">", ">"⟩ :: tail)
        else none
      let (children, remaining) ← parseSymbols table splitClosers genericRhs (fuel - 1) rest tail
      some (.leaf { token with kind } :: children, remaining)
    | .n name :: rest =>
      let look := input.head?.map (·.kind)
      let rhs ← table[(name, look)]?.orElse fun _ =>
        if splitClosers && look == some ">>" then table[(name, some ">")]? else none
      let closesGenerics := ["TypeArgs", "Generics", "ImplParams", "ImplTypeParamTail",
        "ImplAfterComma"].contains name
      let (children, tail) ← parseSymbols table splitClosers closesGenerics (fuel - 1) rhs input
      let (siblings, remaining) ← parseSymbols table splitClosers genericRhs (fuel - 1) rest tail
      some (.node name children :: siblings, remaining)

partial def Tree.hasTokens : Tree → Bool
  | .leaf _ => true
  | .node _ children => children.any Tree.hasTokens

def Tree.isNode (name : String) : Tree → Bool
  | .node actual _ => name == actual
  | .leaf _ => false

partial def traceableGroupBase : Tree → Bool
  | .leaf token => token.kind == "IDENT"
  | .node name children =>
    match children.filter Tree.hasTokens with
    | [child] => traceableGroupBase child
    | [.leaf openParen, inner, .leaf closeParen] =>
      name == "GroupAtom" && openParen.kind == "(" && closeParen.kind == ")" &&
        traceableGroupBase inner
    | [.leaf openParen, inner, .leaf closeParen, suffixTree] =>
      name == "GroupAtom" && openParen.kind == "(" && closeParen.kind == ")" &&
        suffixTree.isNode "Postfix" && traceableGroupBase inner
    | [.leaf first, .leaf binding] =>
      name == "Unary" && first.kind == "*" && binding.kind == "IDENT"
    | [.leaf ident, suffixTree] =>
      name == "Atom" && ident.kind == "IDENT" &&
        (match suffixTree with
        | .node "NumericNameTail" [suffix] => suffix.isNode "Postfix"
        | _ => false)
    | _ => false

partial def Tree.tokens : Tree → List Token
  | .leaf t => [t]
  | .node _ children => children.flatMap Tree.tokens

def contractAttribute (slice : Nat) (name : String) : Bool :=
  ["invariant", "variant"].contains name ||
    (slice >= 5 && ["requires", "ensures"].contains name)

-- Listed proof attributes accept the same qualified path as the expression
-- grammar, with an empty value suffix (no call, literal or projection).
partial def isProofPath : Tree → Bool
  | .node "Atom" [.leaf root, path, suffixTree] =>
    root.kind == "IDENT" && path.isNode "PathTail" && !suffixTree.hasTokens
  | .node _ children =>
    match children.filter Tree.hasTokens with
    | [child] => isProofPath child
    | _ => false
  | .leaf _ => false

def attributeValid (slice : Nat) (options : Options) (opener name : String) (args : Tree) :
    Bool :=
  let kinds := args.tokens.map (·.kind)
  let oneExpr := match args with
    | .node "AttrArgs" [_, .node "AttributeArgs" [.node "Args" [expr, tail]], _] =>
      if expr.hasTokens && !tail.hasTokens then some expr else none
    | _ => none
  let listedShape :=
    if contractAttribute slice name then some oneExpr.isSome
    else if name == "may_not_terminate" then
      some (match args.tokens with
        | [openParen, reason, closeParen] =>
          openParen.kind == "(" && reason.kind == "STRING" && !reason.spelling.isEmpty &&
            closeParen.kind == ")"
        | _ => false)
    else if slice >= 10 && name == "test" then some kinds.isEmpty
    else if slice >= 10 && name == "proof" then some (oneExpr.any isProofPath)
    else none
  match listedShape with
  | some valid => opener == "#[" && valid
  | none => !options.alternateAttribute || opener == "@["

inductive Attachment where
  | loop (form : String)
  | operand (tree : Tree)
  | function
  | structDecl
  | declaration
  | invalid

-- The call must be on the operand's postfix spine. A call inside a group,
-- argument, index, literal or branching expression cannot qualify the operand.
partial def isPostfixCall : Tree → Bool
  | .node "NameCall" _ => true
  | .node name children =>
    if ["Atom", "HeadAtom", "NumericNameTail", "HeadNameTail"].contains name then children.any isPostfixCall else false
  | .leaf _ => false

def attachmentValid (slice : Nat) (name : String) (target : Attachment) : Bool :=
  let generalDeclaration := name != "may_not_terminate" && !contractAttribute slice name &&
    !(slice >= 10 && ["proof", "test"].contains name)
  match target with
  | .loop form =>
    if ["may_not_terminate", "variant"].contains name then form == "while"
    else !["requires", "ensures", "proof", "test"].contains name
  | .operand operand => name == "may_not_terminate" && isPostfixCall operand
  | .function => name != "invariant"
  | .structDecl => (slice >= 8 && name == "invariant") || generalDeclaration
  | .declaration => generalDeclaration
  | .invalid => false

def attributeAttachment (owner : String) (following : List Tree) : Attachment :=
  match owner, following with
  | "Body", .node "AttributedBody" (.node "Loop" [_, form] :: _) :: _ =>
    .loop ((form.tokens.head?).map (·.kind) |>.getD "")
  | "Body", .node "AttributedBody" (operand :: _) :: _ => .operand operand
  | name, operand :: _ =>
    if ["Unary", "HeadUnary"].contains name then .operand operand
    else if name == "Functions" || name == "ImplMembers" then .function
    else if name == "TraitMembers" then .declaration
    else if name == "Item" then
      match following.find? (·.isNode "Decl") with
      | some (.node "Decl" (.leaf first :: _)) =>
        if first.kind == "fn" then .function
        else if first.kind == "struct" then .structDecl
        else .declaration
      | some (.node "Decl" [child]) =>
        if child.isNode "Function" then .function else .declaration
      | _ => .declaration
    else .invalid
  | _, [] => .invalid

def placeTailProjectsOrAssigns : Tree → Bool
  | .node "PlaceTail" (.node "Projection" _ :: _) => true
  | .node "PlaceTail" (.leaf token :: _) => token.kind == "="
  | _ => false

mutual
  partial def draftRestrictions (slice : Nat) (options : Options) (inContract : Bool) :
      Tree → Bool
    | .leaf _ => true
    | .node "GroupAtom" [_, group, _, suffixTree] =>
      (!suffixTree.hasTokens || traceableGroupBase group) &&
        draftRestrictions slice options inContract group &&
        draftRestrictions slice options inContract suffixTree
    | .node "GroupedBody" [_, group, _, suffixTree] =>
      (!placeTailProjectsOrAssigns suffixTree || traceableGroupBase group) &&
        draftRestrictions slice options inContract group &&
        draftRestrictions slice options inContract suffixTree
    | .node name (leading :: following) =>
      let inProof := inContract || ["ProofExpr", "ProofArgs"].contains name
      (!["ExactArithmetic", "MathValue", "MathOtherValue"].contains name || inProof) &&
        (if leading.isNode "Attrs" || leading.isNode "AttributePrefix" then
          attributesValid slice options (attributeAttachment name following) leading
        else draftRestrictions slice options inProof leading) &&
        following.all (draftRestrictions slice options inProof)
    | .node _ [] => true

  partial def attributesValid (slice : Nat) (options : Options) (target : Attachment) :
      Tree → Bool
    | .node "Attribute" [.leaf opener, .leaf name, args, _] =>
      attributeValid slice options opener.kind name.spelling args &&
        attachmentValid slice name.spelling target &&
        draftRestrictions slice options (contractAttribute slice name.spelling) args
    | .node name children =>
      ["Attrs", "AttributePrefix"].contains name &&
        children.all (attributesValid slice options target)
    | _ => false
end

-- Count only arguments at this call level. Nested calls and commas inside
-- expressions or types never contribute to the enclosing call's arity.
partial def extraArguments : Tree → Nat
  | .node "ArgTail" [_, .node "Args" [expr, tail]] =>
    (if expr.hasTokens then 1 else 0) + extraArguments tail
  | .node name [_, _] => if ["SecondArg", "ConstSecondArg"].contains name then 1 else 0
  | _ => 0

def targetArity (args : Tree) : Option Nat :=
  match args with
  | .node _ [target, _, tail] =>
    if target.isNode "TargetType" then some (1 + extraArguments tail) else none
  | _ => none

-- Named operations represented by CExpr.convert, .wrapping and .saturating.
-- "widen" is the E4 draft spelling. Plain checked operations use operators.
def constPrimitiveArity (callee : String) : Nat :=
  if callee == "widen" then 1
  else if ["wrapping_add", "wrapping_sub", "wrapping_mul",
    "saturating_add", "saturating_sub", "saturating_mul"].contains callee then 2
  else 0

def expectedTargetArity (callee : String) : Nat :=
  if ["truncate", "reinterpret"].contains callee then 1
  else constPrimitiveArity callee

def braceUsesMath : Tree → Bool
  | .node "BraceContents" [.node "MathOtherValue" _] => true
  | .node "BraceContents" [_, .node "BraceAfterName" [.node "MathValue" _]] => true
  | _ => false

-- At a named suffix the two preceding tokens distinguish a bare
-- identifier from a path segment. Validation follows the parsed suffix.
partial def validateNamedSuffixes (previous : Token × Token) (tree : Tree) :
    Option (Token × Token) := do
  match tree with
  | .leaf token => some (previous.2, token)
  | .node name children =>
    let bareName := previous.1.kind != "::" && previous.2.kind == "IDENT"
    match name, children with
    | "NameCall", [_, args, _] =>
      if let some arity := targetArity args then
        guard (bareName && expectedTargetArity previous.2.spelling == arity)
    | "ConstNameTail", [.leaf openParen, .node "ConstArgs" args, _] =>
      let some tail := args.getLast? | none
      guard (openParen.kind == "(" && bareName &&
        constPrimitiveArity previous.2.spelling == 1 + extraArguments tail)
    | "NameLiteral", [_, contents, _] =>
      if braceUsesMath contents then
        guard (bareName && previous.2.spelling == "math")
    | _, _ => pure ()
    children.foldlM validateNamedSuffixes previous

def parseSource (slice : Nat) (options : Options) (g : DraftGrammar) (table : PredictionTable)
    (source : String) : Option Tree := do
  let input := draftTokens slice options source
  let some ([tree], []) :=
    parseSymbols table (slice >= 9) false ((input.length + 1) * (g.prods.length + 1)) [.n g.start] input
    | none
  return tree

def parseDraft (slice : Nat) (options : Options) (g : DraftGrammar) (table : PredictionTable)
    (source : String) : Option Tree := do
  let tree ← parseSource slice options g table source
  guard (draftRestrictions slice options false tree &&
    (validateNamedSuffixes (default, default) tree).isSome)
  return tree

def recognizes (slice : Nat) (options : Options) (g : DraftGrammar) (table : PredictionTable)
    (source : String) : Bool :=
  (parseDraft slice options g table source).isSome

partial def permissionOperands : Tree → List (List String)
  | .node name (leading :: following) =>
    let nested := (leading :: following).flatMap permissionOperands
    if (leading.isNode "AttributePrefix") && leading.tokens.any (·.spelling == "may_not_terminate") then
      match attributeAttachment name following with
      | .operand operand => (operand.tokens.map (·.spelling)) :: nested
      | _ => nested
    else nested
  | _ => []

partial def matchEnds : Tree → List String
  | .node "MatchEnd" (.leaf token :: _) => [token.kind]
  | .node "MatchEnd" (.node "Body" (body :: _) :: _) =>
    [(body.tokens.head?).map (·.kind) |>.getD "" |>.append " body"]
  | .node _ children => children.flatMap matchEnds
  | .leaf _ => []

structure Example where
  label : String
  input : String
  expected : Bool := true

def fnBody (body : String) : String :=
  "fn x ( ) -> u8 { " ++ body ++ " }"

def examples (slice currentSlice : Nat) (options : Options) : List Example :=
  let wildcard := "_"
  let attr := if options.alternateAttribute then "@[" else "#["
  let arm (pattern body : String) := s!"{pattern} => {body} ,"
  let borrow := options.borrowWord
  let binding := if options.prefixBorrowBinder then s!"{borrow} x = x"
    else s!"{borrow} x {options.binderMark} x"
  match slice with
  | 0 =>
    [⟨"typed binding and addition", fnBody "let x : u8 = 1; return x + 2;", true⟩,
     ⟨"main capability reserved before its first use", "fn Fs() -> u8 { return 1; }", false⟩,
     ⟨"main struct keyword reserved before its first use", "fn struct() -> u8 { return 1; }", false⟩] ++
    (mainReservedWords ++ ["for", "break", "continue", "pub", "const", "mod"]).map (fun name =>
      ⟨s!"reserved binder {name}", s!"fn {name}() -> u8 \{ return 1; }", false⟩) ++
    ["main", "self", "as", "in", "newtype", "union", "total"].map (fun name =>
      ⟨s!"identifier binder {name}", s!"fn {name}() -> u8 \{ return 1; }", true⟩) ++
    [(options.borrowWord, decide (currentSlice < 7)), (options.importWord, decide (currentSlice < 8)),
      (if options.borrowWord == "borrow" then "loan" else "borrow", true),
      (if options.importWord == "import" then "use" else "import", true)].map (fun (name, allowed) =>
      ⟨s!"draft reservation for {name}", s!"fn {name}() -> u8 \{ return 1; }", allowed⟩) ++
    ["trusted", "extern"].map (fun name =>
      ⟨s!"trust reservation for {name}", s!"fn {name}() -> u8 \{ return 1; }",
        !options.earlyTrustWords⟩)
  | 1 => [
    ⟨"typed and destructuring lets share a prefix",
      "struct x { } " ++ fnBody "let x : x = x { } ; let x { } = move x ; return 1 ;", true⟩,
    ⟨"typed wildcard let", fnBody "let _ : u8 = 1; return 2;", true⟩,
    ⟨"wildcard token cannot bind a function name", "fn _() -> u8 { return 1; }", false⟩,
    ⟨"incomplete let", fnBody "let x = 1 ;", false⟩]
  | 2 => [
    ⟨"target-type conversion", fnBody "return widen(type: u32, x);", options.builtinTargetType⟩,
    ⟨"target-type binary arithmetic", fnBody "return wrapping_add(type: u32, x, y);", options.builtinTargetType⟩,
    ⟨"target-type conversion rejects two operands", fnBody "return widen(type: u32, x, y);", false⟩,
    ⟨"target-type binary arithmetic rejects one operand", fnBody "return wrapping_add(type: u32, x);", false⟩,
    ⟨"ordinary per-target conversion", fnBody "return to_u32(x);", true⟩,
    ⟨"ordinary expected-type conversion", fnBody "return widen(x);", true⟩,
    ⟨"ordinary binary arithmetic", fnBody "return wrapping_add(x, y);", true⟩,
    ⟨"eligible builtin name stays an identifier in a value position",
      fnBody "return wrapping_add;", true⟩,
    ⟨"ordinary call cannot take a target type",
      fnBody "return x ( type : u8 , 1 ) ;", false⟩,
    ⟨"bitwise NOT in a return at first use", fnBody s!"return {options.bitNot} 1 ;", true⟩,
    ⟨"unselected bitwise NOT spelling",
      fnBody (if options.bitNot == "!" then "return ~1;" else "return !1;"),
      options.bitNot == "~" && currentSlice >= 3⟩,
    ⟨"primitive keyword classification",
      "fn u32() -> u8 { return 1; }", !options.primitiveKeyword⟩,
    ⟨"primitive identifier classification", fnBody "let x : x = 1 ;", true⟩]
  | 3 => [
    ⟨"semicolon-free if statement", fnBody "if true { return 1 ; } return 1 ;", true⟩,
    ⟨"value if with an operator continuation",
      fnBody "return if true { 1 } else { 1 } + 1 ;", true⟩,
    ⟨"statement if followed by a trailing unary minus",
      fnBody "if true { x ( ) ; } - 1", true⟩,
    ⟨"statement if followed by a unary-minus statement",
      fnBody "if true { x ( ) ; } - 1 ; return 1 ;", true⟩,
    ⟨"statement if followed by a trailing bitwise not",
      fnBody s!"if true \{ x ( ) ; } {options.bitNot} 1", true⟩,
    ⟨"else-less if has no value",
      fnBody "let x : u8 = if true { 1 } ; 1", false⟩,
    ⟨"statement if does not continue with binary plus",
      fnBody "if true { 1 } else { 1 } + 1", false⟩,
    ⟨"assignment to local", fnBody "x = 1 ;", true⟩,
    ⟨"assignment to arithmetic result", fnBody "1 + 1 = 1 ;", false⟩,
    ⟨"assignment to call result", fnBody "x ( ) = 1 ;", false⟩,
    ⟨"expression statements and a final expression", fnBody "x ( ) ; x = 1 ; x", true⟩,
    ⟨"final expression requires closing brace", fnBody "x return 1 ;", false⟩]
  | 4 => [
    ⟨"explicit exact arithmetic in a loop contract",
      fnBody "#[invariant(math { x + 1 } > x)] while true {}", true⟩,
    ⟨"listed names are identifiers in ordinary code", fnBody "let invariant : u8 = 1; return invariant;", true⟩,
    ⟨"termination permission before a loop",
      fnBody "#[ may_not_terminate ( \"reason\" ) ] while true { }", true⟩,
    ⟨"termination permission before an expression statement",
      fnBody "#[ may_not_terminate ( \"reason\" ) ] x ( x ) + x ( ) ;", true⟩,
    ⟨"termination permission on the right operand of addition",
      fnBody "return 1 + #[ may_not_terminate ( \"reason\" ) ] x ( ) ;", true⟩,
    ⟨"termination permission consumes a postfix operand at its own precedence",
      fnBody "return #[ may_not_terminate ( \"reason\" ) ] - 1 ;", false⟩,
    ⟨"termination permission needs an operand after the prefix",
      fnBody "#[ may_not_terminate ( \"reason\" ) ] + x ( ) ;", false⟩,
    ⟨"shielded exact arithmetic in a condition head",
      fnBody "#[invariant(if (math { x + 1 } > x) { true } else { false })] while true { }", true⟩,
    ⟨"unshielded exact arithmetic in a condition head",
      fnBody "#[invariant(if math { x + 1 } > x { true } else { false })] while true { }",
      options.mathKeyword⟩,
    ⟨"exact arithmetic with a literal-leading operand",
      fnBody "#[invariant(math { 1 + x } > x)] while true { }", true⟩,
    ⟨"other identifiers cannot introduce exact arithmetic",
      fnBody "#[invariant(other { x + 1 } > x)] while true { }", false⟩,
    ⟨"exact arithmetic outside a proof context",
      fnBody "return math { x + 1 };", false⟩,
    ⟨"loop invariant refuses a call attachment",
      fnBody "#[invariant(true)] f();", false⟩,
    ⟨"termination permission refuses an empty reason",
      fnBody "#[may_not_terminate(\"\")] f();", false⟩,
    ⟨"termination permission requires a call operand",
      fnBody "#[may_not_terminate(\"reason\")] 1;", false⟩,
    ⟨"termination permission refuses a counted loop",
      fnBody "#[may_not_terminate(\"reason\")] for i from 0 until 1 { }", false⟩,
    ⟨"termination permission refuses a call nested inside its operand",
      fnBody "#[may_not_terminate(\"reason\")] (1 + f());", false⟩,
    ⟨"listed and general attributes at first use",
      fnBody s!"#[ invariant ( true ) ] {attr} x ( \"reason\" ) ] for {wildcard} from 1 until 1 \{ } 1", true⟩,
    ⟨"listed attribute uses the listed envelope",
      fnBody "@[ invariant ( true ) ] while true { }", false⟩,
    ⟨"general attribute uses its selected envelope",
      fnBody "#[ x ] while true { }", !options.alternateAttribute⟩,
    ⟨"labelled loop and labelled transfer",
      fnBody "'outer: while true { break 'outer; } 1", true⟩,
    ⟨"colonless loop label", fnBody "'outer while true { }", false⟩,
    ⟨"wildcard let", fnBody s!"let {wildcard} : u8 = 1 ; 1", true⟩]
  | 5 => [
    ⟨"exact arithmetic in a function contract",
      "#[ensures(math { result + 1 } > result)] fn f() -> u8 { 1 }", true⟩,
    ⟨"optional trailing commas in parameters and calls",
      "fn x ( x : u8 , ) -> u8 { x ( 1 , ) }", true⟩,
    ⟨"invariant refuses a function attachment",
      "#[invariant(true)] fn f() -> u8 { 1 }", false⟩,
    ⟨"requires refuses a loop attachment",
      fnBody "#[requires(true)] while true { }", false⟩,
    ⟨"function attribute and wildcard parameter",
      s!"{attr} x ( \"reason\" ) ] fn x ( {wildcard} : u8 ) -> u8 \{ 1 }", true⟩]
  | 6 => [
    ⟨"grouped named place projection", fnBody "return (x).field;", true⟩,
    ⟨"grouped conditional projection", fnBody "return (if true { a } else { b }).field;", false⟩,
    ⟨"grouped match projection",
      fnBody s!"return (match x \{ {arm wildcard "a"} }).field;", false⟩,
    ⟨"binary multiply after body-leading match",
      fnBody s!"match x \{ {arm wildcard "1"} } * 2", currentSlice == 6⟩,
    ⟨"match followed by a unary borrow statement",
      fnBody s!"match x \{ {arm wildcard "1"} } & x;", currentSlice >= 7⟩,
    ⟨"match followed by a unary dereference statement",
      fnBody s!"match x \{ {arm wildcard "1"} } * x;", currentSlice >= 7⟩,
    ⟨"grouped match permits both binary operators",
      fnBody s!"(match x \{ {arm wildcard "1"} }) * 2 & 3", true⟩,
    ⟨"bare return in an unbraced arm", "fn f() -> Unit { match x { _ => return, } }", true⟩,
    ⟨"bare return in a braced arm", "fn f() -> Unit { match x { _ => { return; }, } }", true⟩,
    ⟨"explicit Unit return in a body", "fn f() -> Unit { return (); }", true⟩,
    ⟨"semicolon arm terminator conflicts with the recorded comma rule",
      fnBody "match x { _ => 1; }", false⟩,
    ⟨"nonempty enum", "enum x { x , } " ++ fnBody "1", true⟩,
    ⟨"empty enum", "enum x { } " ++ fnBody "1", false⟩,
    ⟨"negative integer pattern",
      fnBody s!"match x \{ {arm "- 1" "1"} }", true⟩,
    ⟨"alternative patterns",
      fnBody s!"match x \{ {arm "1 | 1" "1"} }", true⟩,
    ⟨"assignment to named field", fnBody "x . x = 1 ;", true⟩,
    ⟨"field projection of a call result", fnBody "return f().field;", false⟩,
    ⟨"assignment to field of call result", fnBody "x ( ) . x = 1 ;", false⟩,
    ⟨"head brace opens the block", fnBody "if x { 1 } else { 1 }", true⟩,
    ⟨"field initializer survives the shared braced suffix",
      fnBody "let value: S = S { field: x + 1 }; return 1;", true⟩,
    ⟨"grouped struct literal in a head", fnBody "if ( x { } ) { 1 }", true⟩,
    ⟨"call argument shields struct literal", fnBody "if x ( x { } ) { 1 }", true⟩,
    ⟨"unshielded struct literal in a head", fnBody "if x { } { 1 }", false⟩,
    ⟨"named variant declaration and pattern",
      "enum x { x { x : u8 , x : u8 } } " ++
        fnBody ("match x { " ++ arm s!"x :: x \{ x : x , x : {wildcard} }" "1" ++ " }"), true⟩,
    ⟨"tuple variant is excluded", "enum x { x ( u8 ) }", false⟩,
    ⟨"tuple pattern is excluded",
      fnBody ("match x { " ++ arm s!"x :: x ( {wildcard} )" "1" ++ " }"), false⟩,
    ⟨"ignored payload", fnBody ("match x { " ++ arm "x :: x { .. }" "1" ++ " }"), true⟩,
    ⟨"braced match arm",
      fnBody ("match x { " ++ arm wildcard "{ return 1 ; }" ++ " }"), true⟩,
    ⟨"unbraced return arm",
      fnBody ("match x { " ++ arm wildcard "return 1" ++ " }"), true⟩,
    ⟨"match with addition as a trailing value",
      fnBody s!"match x \{ {arm wildcard "1"} } + 1", true⟩,
    ⟨"match with addition cannot become an expression statement",
      fnBody s!"match x \{ {arm wildcard "1"} } + 1 ;", false⟩,
    ⟨"unary minus can start the next statement after a match",
      fnBody s!"match x \{ {arm wildcard "1"} } - 1 ;", true⟩,
    ⟨"bare match statement refuses a semicolon",
      fnBody s!"match x \{ {arm wildcard "1"} } ;", false⟩,
    ⟨"semicolon-free match statement",
      fnBody ("match x { " ++ arm wildcard "1" ++ " } return 1 ;"), true⟩,
    ⟨"bare struct match pattern", fnBody ("match x { " ++ arm "x { x }" "1" ++ " }"), false⟩,
    ⟨"qualified field shorthand", fnBody ("match x { " ++ arm "x :: x { x }" "1" ++ " }"), false⟩,
    ⟨"reserved kept refused as a match pattern", fnBody ("match x { " ++ arm "kept x" "1" ++ " }"), false⟩,
    ⟨"explicit struct field binding",
      fnBody s!"let x \{ x : x , x : {wildcard} } = x ; 1", true⟩,
    ⟨"struct let rejects field shorthand", fnBody "let x { x } = x ; 1", false⟩,
    ⟨"qualified destructuring let", fnBody s!"let x :: x \{ x : x , x : {wildcard} } = x ; 1", true⟩]
  | 7 => (
    ["if true { x ( ) ; }", "if true { x ( ) ; } else { x ( ) ; }",
     s!"match x \{ {arm wildcard "{ x ( ) ; }"} }", s!"{binding} \{ x ( ) ; }"].flatMap fun block => [
      ⟨"dereference assignment after block", fnBody (block ++ " * x = 1 ;"), true⟩,
      ⟨"trailing unary minus after block", fnBody (block ++ " - 1"), true⟩]) ++ [
    ⟨"grouped projection can root in a named dereference",
      fnBody "return (*pointer).field[0];", true⟩,
    ⟨"index of grouped conditional", fnBody "return (if true { a } else { b })[0];", false⟩,
    ⟨"assignment through direct dereference", fnBody "*r = 1;", true⟩,
    ⟨"assignment through grouped dereference field", fnBody "(*r).f = 1;", true⟩,
    ⟨"assignment through grouped dereference index", fnBody "(*r)[0] = 1;", true⟩,
    ⟨"assignment through grouped binding index", fnBody "(r)[0] = 1;", true⟩,
    ⟨"assignment to index of grouped value", fnBody "(1 + 2)[0] = 1;", false⟩,
    ⟨"dereference of field is refused", fnBody "return *w.r;", false⟩,
    ⟨"grouping does not authorize dereference of a field", fnBody "return (*w.r).f;", false⟩,
    ⟨"bare dereference cannot take a projection before assignment", fnBody "*r[0].f = 1;", false⟩,
    ⟨"index brackets shield struct literal", fnBody "if x [ x { } ] { 1 }", true⟩,
    ⟨"scoped borrow and block else", fnBody s!"{binding} \{ x } else \{ 1 }", true⟩,
    ⟨"semicolon-free borrow statement", fnBody s!"{binding} \{ x } return 1 ;", true⟩,
    ⟨"borrow else accepts only a block", fnBody s!"{binding} \{ x } else if true \{ 1 }", false⟩,
    ⟨"array repetition", fnBody "[ 1 ; 1 ]", true⟩,
    ⟨"empty array literal", fnBody "[ ]", false⟩] ++
    [⟨"borrow range and binder",
      let place := s!"x {options.range.1} 1 {options.range.2} 2"
      let head := if options.prefixBorrowBinder then s!"view = {place}"
        else s!"{place} {options.binderMark} view"
      fnBody s!"{borrow} mut {head} \{ view }", true⟩]
  | 8 => [
    ⟨"import requires an item list", s!"{options.importWord} x ;", false⟩,
    ⟨"qualified import requires an item list", s!"{options.importWord} x :: x ;", false⟩,
    ⟨"empty import list", s!"{options.importWord} x :: x :: \{ } ;", false⟩,
    ⟨"qualified function call in constant initializer",
      "const x : u8 = x :: x ( 1 ) ;", false⟩,
    ⟨"nonempty import with trailing comma",
      s!"{options.importWord} x :: \{ x , } ;", true⟩,
    ⟨"constant target-type conversion",
      "const X : u32 = widen(type: u32, 1);", options.builtinTargetType⟩,
    ⟨"constant target-type binary arithmetic",
      "const X : u32 = wrapping_add(type: u32, 1, 2);", options.builtinTargetType⟩,
    ⟨"constant target-type binary arithmetic rejects one operand",
      "const X : u32 = wrapping_add(type: u32, 1);", false⟩,
    ⟨"public struct field", "struct x { pub x : u8 }", true⟩,
    ⟨"import at first use", s!"{options.importWord} x :: x :: \{ x , x } ;", true⟩,
    ⟨"constant arithmetic and reference", "const x : u8 = x :: x + 1 ;", true⟩,
    ⟨"byte literal is a scalar constant", "const x : u8 = 'a' ;", true⟩,
    ⟨"named wrapping primitive", "const x : u8 = wrapping_add(1, 1);", true⟩,
    ⟨"named saturating primitive", "const x : u8 = saturating_mul(1, 1);", true⟩,
    ⟨"draft spelling for lossless conversion", "const x : u32 = widen(1);", true⟩,
    ⟨"constant initializer excludes empty calls", "const x : u8 = x ( ) ;", false⟩,
    ⟨"constant initializer excludes exclusive borrow", "const x : u8 = & mut 1 ;", false⟩,
    ⟨"constant initializer excludes dereference", "const x : u8 = * 1 ;", false⟩]
  | 9 => [
    ⟨"inherent impl with capability after type parameter",
      "impl < x , cap x > x { }", true⟩,
    ⟨"trait impl with only type parameters", "impl < x > x for x { }", true⟩,
    ⟨"trait impl with capability parameter", "impl < cap x > x for x { }", false⟩,
    ⟨"trait impl with capability after type parameter",
      "impl < x , cap x > x for x { }", false⟩,
    ⟨"qualified generic call, field selection and comparison",
      fnBody "x :: x :: < x < u8 > , Fs + Console , { } > ( 1 ) < 1", true⟩,
    ⟨"generic type with capability arguments",
      "type x = x :: x < u8 , x + Console , { } > ;", true⟩,
    ⟨"plain angle call collides with comparison", fnBody "x < u8 > ( 1 )", false⟩,
    ⟨"nested generic closers from source", "fn f(x: Outer<Inner<u8>>) -> u8 { 1 }", true⟩,
    ⟨"shift retains maximal-munch token", fnBody "x >> y", true⟩,
    ⟨"consecutive turbofish lists", fnBody "f::<u8>::<u8>()", false⟩,
    ⟨"consecutive turbofish lists in defer", fnBody "defer f::<u8>::<u8>();", false⟩,
    ⟨"another application follows a named segment", fnBody "Option::<u8>::Some::<u8>(1)", true⟩,
    ⟨"named segment follows a type application", fnBody "Option::<u8>::Some(1)", true⟩,
    ⟨"qualified trait bound", "fn f<T: m::Tr>() -> u8 { 1 }", true⟩,
    ⟨"generic trait bound", "fn f<T: m::Tr<u8>>() -> u8 { 1 }", false⟩,
    ⟨"generic unqualified trait bound", "fn f<T: Tr<u8>>() -> u8 { 1 }", false⟩,
    ⟨"single generic type argument list", "type T = S<u8>;", true⟩,
    ⟨"multiple trait bounds", "fn x < x : x + Copy > ( ) -> u8 { 1 }", true⟩,
    ⟨"unbounded capability parameters follow type parameters",
      "fn x < x , cap x , cap x > ( ) -> u8 { 1 }", true⟩,
    ⟨"type parameter after a capability parameter",
      "fn x < cap x , x > ( ) -> u8 { 1 }", false⟩,
    ⟨"capability bound", "fn x < cap x : Fs > ( ) -> u8 { 1 }", false⟩,
    ⟨"call-site cap marker", fnBody "x :: < cap Fs > ( )", false⟩,
    ⟨"qualified call cannot use the unqualified builtin target marker",
      fnBody "module::wrapping_add(type: u8, x, y)", false⟩,
    ⟨"type argument after a capability argument", "type x = x < Fs , u8 > ;", false⟩,
    ⟨"generic trait declaration", "trait x < x > { }", false⟩,
    ⟨"generic trait method", "trait x { fn x < x > ( ) -> u8 ; }", false⟩,
    ⟨"trailing comma in generic parameter and type arguments",
      "fn x < x , cap x , > ( x : x < u8 , > ) -> u8 { 1 }", true⟩,
    ⟨"impl functions", "impl x { " ++ fnBody "1" ++ " }", true⟩,
    ⟨"impl rejects module declaration", "impl x { mod x ; }", false⟩,
    ⟨"duplicate generic type argument lists", "type T = S<u8><u8>;", false⟩]
  | 10 => [
    ⟨"listed proof attribute retains its qualified path", "#[proof(module::lemma::<u8>)] " ++ fnBody "1", true⟩,
    ⟨"listed proof attribute rejects a call", "#[proof(lemma())] " ++ fnBody "1", false⟩,
    ⟨"parenthesized assertion and unparenthesized assumption",
      fnBody "assert ( x == 1 ) ; assume x == 1 ; 1", true⟩,
    ⟨"exact arithmetic in specification function", "spec fn f() -> u8 = math { 1 + 2 };", true⟩,
    ⟨"exact arithmetic in assertions and assumptions",
      fnBody "assert(math { x + 1 } > x); assume math { x + 1 } > x; return 1;", true⟩,
    ⟨"exact arithmetic in theorem argument",
      fnBody "assert(true) by lemma(math { x + 1 }); return 1;", true⟩,
    ⟨"proof context does not extend to the following runtime statement",
      fnBody "assert(true); return math { 1 + 2 };", false⟩,
    ⟨"typed ghost binding", fnBody "ghost let x: u8 = 1; return 1;", true⟩,
    ⟨"ghost destructuring is refused", fnBody "ghost let S { a: x } = s; return 1;", false⟩,
    ⟨"assertion requires parentheses",
      fnBody "assert x == 1 ; 1", false⟩,
    ⟨"theorem-backed assertion",
      fnBody "assert ( x == 1 ) by x :: x ( x ) ; 1", true⟩,
    ⟨"unparenthesized theorem-backed assertion",
      fnBody "assert x == 1 by x ( x ) ; 1", false⟩,
    ⟨"expression-bodied specification function", "spec fn x ( ) -> u8 = 1 ;", true⟩,
    ⟨"specification function rejects a block body", "spec " ++ fnBody "1", false⟩]
  | 11 => [
    ⟨"char methods with both primitive classifications",
      "fn f(c: char) -> u8 { char::from_ascii('a'); char::from_code_point(65); return char::code_point(c); }", true⟩,
    ⟨"qualified u8 call", fnBody "u8::example(1)", true⟩,
    ⟨"module, import and grant clause",
      s!"mod x ; {options.importWord} x :: x :: \{ x , x } ; pub fn x ( ) with ( Fs , Console ) -> u8 \{ 1 }", true⟩,
    ⟨"optional trailing grant comma",
      "fn x ( ) with ( Fs , ) -> u8 { 1 }", true⟩,
    ⟨"attribute string as an expression at slice 11",
      s!"{attr} x ( \"reason\" ) ] " ++ fnBody "1", true⟩]
  | _ => []

-- Only branch on choices with a consumer in this projection. Three choices
-- rename fresh tokens bijectively. Exercise the joint rename on an alternate
-- grammar shape in each slice where it applies.
def optionsAt (slice : Nat) : List Options := Id.run do
  let builtinTargets := if slice >= 2 then [false, true] else [false]
  let bitNots := if slice >= 2 then ["~", "!"] else ["~"]
  let primitives := if slice >= 2 then [true, false] else [true]
  let attributes := if slice >= 4 then [false, true] else [false]
  let borrowBindings := if slice >= 7 then
    [(false, ("from", "until"), "->"), (false, ("from", "until"), ":"),
     (false, ("from", ".."), "->"), (false, ("from", ".."), ":"),
     (true, ("from", "until"), "->"), (true, ("from", ".."), "->")]
    else [(false, ("from", "until"), "->")]
  let mathWords := if slice >= 4 then [true, false] else [true]
  let mut cases := []
  for bitNot in bitNots do
    for primitiveKeyword in primitives do
      for alternateAttribute in attributes do
        for (prefixBorrowBinder, range, binderMark) in borrowBindings do
          for builtinTargetType in builtinTargets do
            for mathKeyword in mathWords do
              cases := { representative with
                  bitNot, primitiveKeyword, alternateAttribute,
                  prefixBorrowBinder, range, binderMark, builtinTargetType, mathKeyword } :: cases
  cases := cases.reverse
  if slice < 1 then return cases
  return cases ++ [{ representative with
    builtinTargetType := slice >= 2
    bitNot := if slice >= 2 then "!" else "~"
    primitiveKeyword := slice < 2
    alternateAttribute := slice >= 4
    wildcardToken := false
    range := if slice >= 7 then ("from", "..") else representative.range
    binderMark := if slice >= 7 then ":" else "->"
    borrowWord := if slice >= 7 then "loan" else "borrow"
    importWord := if slice >= 8 then "use" else "import"
    mathKeyword := slice < 4 }]

def project (options : Options) (rules : List Rule) : DraftGrammar :=
  reachableProjection (instantiate options rules)

def precedenceSource : String :=
  fnBody "if x < x == true { 1 } else { 1 }"

partial def hasOrderingBeforeEquality : Tree → Bool
  | .node "HeadEquality" [left, right] =>
    left.isNode "HeadCompare" && (left.tokens.map (·.kind)) == ["IDENT", "<", "IDENT"] &&
      right.isNode "HeadEqualityTail" && (right.tokens.map (·.kind)) == ["==", "true"]
  | .node _ children => children.any hasOrderingBeforeEquality
  | .leaf _ => false

-- Swap the two fixture operators' levels while preserving the input and LL(1).
def reversedComparisonPrecedence (rules : List Rule) : List Rule :=
  rules.map fun
    | ("EqualityTail", ["==", "@Compare"]) => ("EqualityTail", ["<", "@Compare"])
    | ("CompareOp", ["<"]) => ("CompareOp", ["=="])
    | rule => rule

def checkDraftBoundaries (slice : Nat) (options : Options) (g : DraftGrammar)
    (table : PredictionTable) (label : String) : IO Unit := do
  let parse := parseDraft slice options g table
  let raw := parseSource slice options g table
  let require (name : String) (ok : Bool) : IO Unit :=
    unless ok do throw (IO.userError s!"{label}: {name}")
  require "uniform reservation of pub, const and mod"
    ((draftTokens slice options "pub const mod").map (·.kind) == ["pub", "const", "mod"])
  if slice >= 3 then
    require "ordering binds more tightly than equality in the parsed head"
      ((parse precedenceSource).any hasOrderingBeforeEquality)
  if slice >= 4 then
    for (body, accepted) in [("#[variant(1)] while true {}", true),
        ("#[variant(1)] for i from 0 until 1 {}", false)] do
      let source := fnBody body
      require s!"OD104 attachment: {body}"
        ((raw source).any (fun tree => draftRestrictions slice options false tree == accepted) &&
          (parse source).isSome == accepted)
    let permission := "#[may_not_terminate(\"reason\")] "
    let initial := [("if " ++ permission ++ "fp(x) { 1 } else { 1 }", ["fp", "(", "x", ")"]),
      ("while " ++ permission ++ "fp(x) {}", ["fp", "(", "x", ")"]),
      ("for i from 0 until " ++ permission ++ "fp(x) {}", ["fp", "(", "x", ")"])]
    let matchHeads := if slice >= 6 then [
      ("match " ++ permission ++ "u8::f() { _ => 1, }", ["u8", "::", "f", "(", ")"])] else []
    let borrows := if slice >= 7 then
      let range := s!" {options.range.1} 0 {options.range.2} "
      let place := "x" ++ range ++ permission ++ "fp(x)"
      let binding := if options.prefixBorrowBinder then s!"view = {place}"
        else s!"{place} {options.binderMark} view"
      [(s!"{options.borrowWord} {binding} \{ view }", ["fp", "(", "x", ")"])]
      else []
    let generics := if slice >= 9 then [
      ("if " ++ permission ++ "m::fp::<u8>(x) { 1 } else { 1 }",
        ["m", "::", "fp", "::", "<", "u8", ">", "(", "x", ")"])] else []
    for (body, operand) in initial ++ matchHeads ++ borrows ++ generics do
      require s!"termination permission on the head call: {body}"
        ((parse (fnBody body)).any (fun tree => permissionOperands tree == [operand]))
    let grouped := fnBody ("if " ++ permission ++ "(fp(x)) { 1 } else { 1 }")
    require "termination permission refuses a call nested in a head group"
      ((raw grouped).any (fun tree => !draftRestrictions slice options false tree) &&
        (parse grouped).isNone)
  if slice >= 6 then
    for body in ["let mut P { a: a, b: b } = p; a = 1;", "let mut One::A { v: v } = o; v = 1;"] do
      require s!"PAT-11 mutable destructuring: {body}" (parse (fnBody body)).isSome
    let structInvariant := "#[invariant(math { field + 1 } > 0)] "
    let visibilities := if slice >= 8 then ["", "pub "] else [""]
    for visibility in visibilities do
      let source := structInvariant ++ visibility ++ "struct S { field: u8 }"
      require "struct invariant enters proof context at Slice 8"
        ((raw source).any (fun tree => draftRestrictions slice options false tree == (slice >= 8)) &&
          (parse source).isSome == (slice >= 8))
    let unrelated := [structInvariant ++ "enum E { A, }", "#[variant(1)] struct S { field: u8 }"]
    for misplaced in unrelated do
      require s!"contract attribute refuses unrelated declaration attachment: {misplaced}"
        ((raw misplaced).any (fun tree => !draftRestrictions slice options false tree) &&
          (parse misplaced).isNone)
  if slice >= 8 then
    let excluded := ["try_convert(1)", "checked_add(1, 2)", "checked_neg(1)",
      "truncate(1)", "reinterpret(1)", "discard(1)", "ordinary(1)", "wrapping_add(1)",
      "widen(1, 2)", "wrapping_add(1, try_convert(2))"]
    let targeted := if options.builtinTargetType then
      ["try_convert(type: u8, 1)", "truncate(type: u8, 1)", "reinterpret(type: u8, 1)"] else []
    for formula in excluded ++ targeted do
      let source := s!"const X : u8 = {formula};"
      require s!"closed constant primitive set and arity: {formula}"
        ((raw source).any (fun tree => (validateNamedSuffixes (default, default) tree).isNone) &&
          (parse source).isNone)

def checkOptions (slice : Nat) (rules : List Rule) : IO Unit := do
  let cases := optionsAt slice
  for options in cases do
    let label := s!"slice {slice}, {reprStr options}"
    let g := project options rules
    let table ← requireLL1 label g
    checkDraftBoundaries slice options g table label
    if slice >= 1 then
      let wildcardKind := if options.wildcardToken then "_" else "RESERVED_UNDERSCORE"
      unless (draftTokens slice options "_").map (·.kind) == [wildcardKind] do
        throw (IO.userError s!"{label}: wildcard token classification")
    for earlyTrustWords in [false, true] do
      let options := { options with earlyTrustWords }
      for fixture in (List.range (slice + 1)).flatMap (fun introduced => examples introduced slice options) do
        unless recognizes slice options g table fixture.input == fixture.expected do
          throw (IO.userError s!"{label}, early trust reservation {earlyTrustWords}, {fixture.label}: expected {fixture.expected}: {fixture.input}")
    if options.builtinTargetType then
      for malformed in ["wrapping_add(type: u32, x)", "widen(type: u32, x, y)",
        "ordinary(type: u32, x, y)"] do
        unless (parseSource slice options g table (fnBody ("return " ++ malformed ++ ";"))).any
            (fun tree => (validateNamedSuffixes (default, default) tree).isNone) do
          throw (IO.userError s!"{label}: target argument control must reach the parsed call: {malformed}")
    if slice >= 4 then
      for misplaced in ["#[invariant(true)] f();", "#[may_not_terminate(\"\")] f();",
        "#[may_not_terminate(\"reason\")] 1;"] do
        unless (parseSource slice options g table (fnBody misplaced)).any
            (fun tree => !draftRestrictions slice options false tree) do
          throw (IO.userError s!"{label}: attribute control must reach the parsed attachment: {misplaced}")
      let mathBinder := "fn math() -> u8 { return 1; }"
      unless recognizes slice options g table mathBinder == !options.mathKeyword do
        throw (IO.userError s!"{label}: math binder classification")
      let source := fnBody "#[may_not_terminate(\"reason\")] fp(x) + g();"
      unless (parseDraft slice options g table source).any
          (fun tree => permissionOperands tree == [["fp", "(", "x", ")"]]) do
        throw (IO.userError s!"{label}: termination permission must attach to fp(x)")
    if slice >= 6 then
      let operand := if slice == 6 then "2" else "x"
      for operator in ["*", "&"] do
        let source := fnBody s!"match x \{ _ => 1, } {operator} {operand}"
        let expected := if slice == 6 then operator else operator ++ " body"
        unless (parseDraft slice options g table source).any (fun tree => matchEnds tree == [expected]) do
          throw (IO.userError s!"{label}: incorrect match boundary for {operator}")
  IO.println s!"pass: slice {slice}, {cases.length} prediction cases, both trust reservations"
  (← IO.getStdout).flush

def requireConflict (label : String) (g : DraftGrammar) : IO Unit := do
  if ll1 g then throw (IO.userError s!"missing planted conflict: {label}")
  IO.println s!"pass: planted conflict for {label}"

-- These extensions share terminal kinds. Exercise the existing and proposed
-- tokenizers on the same source and retain domain-boundary refusals.
-- An expected type is supplied by the downstream typing context. This
-- probe exercises the two admission policies without inferring that context.
def literalSuffixAllowed (allowRedundant : Bool) (expected : Option String) (source : String) : Bool :=
  match integerSuffix {} source.toList, expected with
  | some [], some _ => true
  | some (_ :: _), none => true
  | some (first :: rest), some ty => allowRedundant && String.ofList (first :: rest) == ty
  | _, _ => false

def checkRawStrings : IO Unit := do
  let forbidden := List.range 32 ++ List.range' 0x7F 33 ++
    [0x061C, 0x200E, 0x200F, 0x2028, 0x2029, 0xFEFF] ++
    List.range' 0x202A 5 ++ List.range' 0x2066 4
  for unicodeEscape in [false, true] do
    for stringHexEscape in [false, true] do
      let policy : LiteralOptions := { unicodeEscape, stringHexEscape }
      let kind (source : String) := (draftTokens 11 representative source policy).map (·.kind)
      for scalar in forbidden do
        let source := "\"" ++ String.singleton (Char.ofNat scalar) ++ "\""
        unless kind source == ["INVALID"] do
          throw (IO.userError s!"forbidden raw string scalar accepted: {scalar}")
      unless kind (String.singleton (Char.ofNat 0xFEFF) ++ "fn") == ["INVALID"] && kind "\"é\\n\\0\"" == ["STRING"] &&
          kind "\"\\u{202e}\\u{feff}\"" == [if unicodeEscape then "STRING" else "INVALID"] &&
          kind "\"\\x7f\"" == [if stringHexEscape then "STRING" else "INVALID"] do
        throw (IO.userError "raw source refusal or escaped string admission diverged")

def checkLiteralOptions : IO Unit := do
  checkRawStrings
  let kind (policy : LiteralOptions) (source : String) :=
    (draftTokens 2 representative source policy).map (·.kind)
  let base : LiteralOptions := {}
  for (policy, token, accepted, refused) in [
      ({ base with digitSeparators := true }, "INTEGER", "1_000", "0x_ff"),
      ({ base with digitSeparators := true, separatorAfterPrefix := true },
        "INTEGER", "0x_ff", "0x__ff"),
      ({ base with upperHex := true }, "INTEGER", "0xFF", "0XFF"),
      ({ base with unicodeEscape := true }, "STRING", "\"\\u{10ffff}\"", "\"\\u{d800}\""),
      ({ base with stringHexEscape := true }, "STRING", "\"\\x7F\"", "\"\\x80\""),
      ({ base with byteHexEscape := true }, "BYTE", "'\\xFF'", "'\\x100'")] do
    unless kind base accepted == ["INVALID"] && kind policy accepted == [token] &&
        kind policy refused == ["INVALID"] do
      throw (IO.userError s!"literal option does not distinguish {accepted} from {refused}")
  unless kind base "255u8" == ["INTEGER"] && kind base "255u9" == ["INVALID"] do
    throw (IO.userError "integer suffixes must retain their existing lexical classification")
  for redundant in [false, true] do
    unless literalSuffixAllowed redundant (some "u8") "255u8" == redundant &&
        literalSuffixAllowed redundant none "255u8" &&
        literalSuffixAllowed redundant (some "u8") "255" &&
        !literalSuffixAllowed redundant (some "i16") "255u8" do
      throw (IO.userError "redundant suffix policy does not distinguish matching and mismatched types")
  IO.println "pass: literal extensions, boundaries and direct expected-type suffix controls"

def main : IO Unit := do
  checkLiteralOptions
  let sections ← readSections (← IO.FS.readFile "docs/grammar/slices-0-11.grammar")
  let mut cumulative := []
  for (delta, slice) in sections.toList.zipIdx do
    cumulative := extend cumulative delta
    let rules := addExpressionLadders slice cumulative
    checkOptions slice rules
    if slice == 3 then
      let reversed := project representative (addExpressionLadders slice (reversedComparisonPrecedence cumulative))
      let table ← requireLL1 "reversed comparison precedence" reversed
      unless (parseDraft slice representative reversed table precedenceSource).any
          (fun tree => !hasOrderingBeforeEquality tree) do
        throw (IO.userError "reversed precedence must still parse and fail the grouping assertion")
      IO.println "pass: parsed grouping catches reversed comparison precedence"
    if slice == 4 then
      -- Duplicate the STRING path only in the alternate envelope. The default
      -- projection cannot see it, and slice 11 replaces the argument grammar.
      let planted := rules ++ [
        ("Attribute", ["@[", "IDENT", "@PlantedAttrArgs", "]"]),
        ("PlantedAttrArgs", ["(", "STRING", ")"])]
      let alternate := { representative with alternateAttribute := true }
      let conflicts := (optionsAt slice).filter fun options => !(ll1 (project options planted))
      unless !conflicts.isEmpty && conflicts.all (·.alternateAttribute) &&
          ll1 (project alternate rules) do
        throw (IO.userError "missing slice 4 alternate attribute conflict")
      IO.println "pass: planted slice 4 alternate attribute conflict"
      let ordinaryMath := { representative with mathKeyword := false }
      -- Even after factoring named braced suffixes, opting into them in
      -- an unshielded head makes the opening brace compete with the block.
      requireConflict "ordinary math suffix in an unshielded head"
        (project ordinaryMath (rules ++ [("HeadNameTail", ["@NameLiteral"])]))
    if slice == 7 then
      -- Reintroducing binary continuations at the block boundary collides
      -- with the same spellings at the beginning of the next body item.
      let planted := rules.map fun rule =>
        if rule == ("BlockItem", ["@BlockStmt", "@Body"]) then
          ("BlockItem", ["@BlockStmt", "@BlockBoundary"])
        else rule
      let planted := planted ++ [
        ("BlockBoundary", ["@Body"]),
        ("BlockBoundary", ["-", "@Product", "@ExprContinuation"]),
        ("BlockBoundary", ["*", "@Unary", "@ExprContinuation"]),
        ("BlockBoundary", ["&", "@Shift", "@ExprContinuation"])]
      requireConflict "operator continuation at block statement boundary" (project representative planted)
      requireConflict "shared minus at the match boundary" (project representative (rules ++ [
        ("MatchEnd", ["-", "@Product", "@SumTail", "@ShiftTail", "@AndBits", "@XorBits",
          "@OrBits", "@CompareTail", "@EqualityTail", "@LogicAndTail", "@LogicOrTail"])]))
    if slice == 9 then
      let g := project representative rules
      requireConflict "unmarked generics" { g with prods := g.prods ++ [("PathTail", [.n "TypeArgs"])] }
      let table ← requireLL1 "slice 9 lexer adapter" g
      let nested := draftTokens slice representative "fn f(x: Outer<Inner<u8>>) -> u8 { 1 }"
      let shift := draftTokens slice representative (fnBody "x>>y")
      let parse input split := parseSymbols table split false 2000 [.n g.start] input
      unless nested.any (·.kind == ">>") && (parse nested false).isNone &&
          (parse shift false).any (fun (_, rest) => rest.isEmpty) do
        throw (IO.userError "missing maximal-munch or generic-splitting control")
      IO.println "pass: shift uses maximal munch and nested types require the closer adapter"
    if slice == 6 then
      let g := project representative rules
      requireConflict "unshielded head literal" { g with prods := g.prods ++
        [("HeadNameTail", [.t "{", .n "Initializers", .t "}"])] }
      let broken := rules.filter (· != ("LetTail", ["{", "@PatternFields", "}"]))
      let mutant := project representative broken
      let table ← requireLL1 "slice 6 with missing destructuring let" mutant
      let earlier := examples 1 slice representative
      unless earlier.any (fun fixture => fixture.expected &&
          !recognizes slice representative mutant table fixture.input) do
        throw (IO.userError "missing cumulative destructuring control")
      IO.println "pass: earlier recognition control catches an LL(1) slice 6 regression"
