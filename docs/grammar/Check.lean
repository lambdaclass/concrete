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

def grammar (rules : List Rule) : DraftGrammar :=
  ⟨"Program", rules.map fun (lhs, rhs) => (lhs, rhs.map fun sym =>
    if sym.startsWith "@" then .n (sym.drop 1).toString else .t sym)⟩

def reachableProjection (g : DraftGrammar) : DraftGrammar :=
  let reachable := saturate (fun seen => g.prods.flatMap fun (lhs, rhs) =>
    if seen.contains lhs then rhs.filterMap (fun | .n name => some name | .t _ => none)
    else []) g.prods.length [g.start]
  { g with prods := g.prods.filter fun (lhs, _) => reachable.contains lhs }

-- Check all written rules before projecting, including rules unreachable in this slice.
def productive (g : DraftGrammar) : Bool :=
  let defined := (g.prods.map Prod.fst).eraseDups
  let live := saturate (fun live => g.prods.filterMap fun (lhs, rhs) =>
    if rhs.all (fun | .t _ => true | .n sym => live.contains sym) then some lhs else none)
    defined.length []
  live.contains g.start && g.prods.all fun (lhs, rhs) =>
    live.contains lhs && rhs.all (fun | .t _ => true | .n sym => live.contains sym)

abbrev PredictionTable := Std.HashMap (String × Option String) (List (Sym String String))

inductive ProjectionError where
  | unproductive
  | notClosed
  | conflict (lhs : String) (lookahead : Option String) (prior next : List (Sym String String))
  deriving Repr

-- Qualify the same written rules for both real slices and planted-conflict controls.
def qualifyProjection (rules : List Rule) : Except ProjectionError (DraftGrammar × PredictionTable) := do
  let written := grammar rules
  unless productive written do throw .unproductive
  let g := reachableProjection written
  let (nu, fi, fo) := compute g
  unless nuClosed g nu && fiClosed g nu fi && foClosed g nu fi fo do throw .notClosed
  let mut table : PredictionTable := {}
  for (lhs, rhs) in g.prods do
    for lookahead in (predictB nu fi fo lhs rhs).eraseDups do
      let key := (lhs, lookahead)
      if let some prior := table[key]? then
        throw (.conflict lhs lookahead prior rhs)
      table := table.insert key rhs
  return (g, table)

def requireProjection (label : String) (rules : List Rule) : IO (DraftGrammar × PredictionTable) :=
  match qualifyProjection rules with
  | .ok result => pure result
  | .error reason => throw (IO.userError s!"{label}: {repr reason}")

structure Token where
  kind : String
  spelling : String
  primitiveSlice : Option Nat := none
  deriving Inhabited

private def Token.invalid (lexeme : String := "") : Token :=
  { kind := "INVALID", spelling := lexeme }

open ConcreteSpec.Surface0 (Terminal wordTokens wordValue punctuation)

-- Prediction uses the emitted terminal. The lexeme is retained for payload checks.
private def Token.ofTerminal (terminal : Terminal) (lexeme : String) : Token :=
  let kind := match terminal with
    | .ident => "IDENT"
    | .integer => "INTEGER"
    | .byteLiteral => "BYTE"
    | .stringLiteral => "STRING"
    | .label => "LABEL"
    | .wildcard => "WILDCARD"
    | .u8 => "u8"
    | _ =>
      if terminal.primitiveSlice.isSome then "PRIMITIVE"
      else (((punctuation ++ wordTokens).find? (fun entry => entry.2 == terminal)).map Prod.fst).getD "INVALID"
  { kind, spelling := lexeme, primitiveSlice := terminal.primitiveSlice }

def hexDigit (c : Char) : Option Nat :=
  if c.isDigit then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c && c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else none

def integerSuffix (slice : Nat) (word : List Char) : Option (List Char) := do
  let (base, digits) := match word with
    | '0' :: 'x' :: rest => (16, rest)
    | '0' :: 'o' :: rest => (8, rest)
    | '0' :: 'b' :: rest => (2, rest)
    | _ => (10, word)
  let isDigit := fun c => (hexDigit c).any (· < base)
  let rec consume : List Char → List Char
    | c :: next :: rest =>
      if isDigit c then
        if next == '_' && slice >= 2 && (rest.head?).any isDigit then consume rest
        else consume (next :: rest)
      else c :: next :: rest
    | [c] => if isDigit c then [] else [c]
    | [] => []
  guard (digits.head?.any isDigit)
  let suffix := consume digits
  guard (suffix.isEmpty ||
    ["i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"].contains (String.ofList suffix))
  guard (base != 10 || digits.head? != some '0' || digits == '0' :: suffix)
  return suffix

def hexValue (digits : List Char) : Option Nat :=
  digits.foldlM (fun acc digit => (hexDigit digit).map (acc * 16 + ·)) 0

def escapeTail (slice : Nat) (quote : Char) (input : List Char) : Option (List Char) := do
  match input with
  | 'x' :: a :: b :: rest =>
    guard (slice >= 2 && quote == '\'' && (hexValue [a, b]).isSome)
    return rest
  | 'u' :: '{' :: rest =>
    guard (slice >= 2 && quote == '"')
    let digits := rest.takeWhile (hexDigit · |>.isSome)
    let '}' :: tail := rest.drop digits.length | none
    let value ← hexValue digits
    guard (!digits.isEmpty && decide (ConcreteSpec.Surface0.Utf8.Scalar value))
    return tail
  | escaped :: rest =>
    guard (['n', 't', 'r', '0', '\\', quote].contains escaped)
    return rest
  | [] => none

partial def quotedBody (slice : Nat) (input acc : List Char) :
    Option (List Char × List Char) :=
  match input with
  | [] => none
  | '\\' :: rest => do
    let tail ← escapeTail slice '"' rest
    let escape := '\\' :: rest.take (rest.length - tail.length)
    quotedBody slice tail (escape.reverse ++ acc)
  | next :: rest =>
    if next == '"' then some (acc.reverse, rest)
    else if ConcreteSpec.Surface0.StringRaw next.toNat then quotedBody slice rest (next :: acc)
    else none

def draftTokens (slice : Nat) (source : String) : List Token := Id.run do
  let wordChar (c : Char) := decide (ConcreteSpec.Surface0.IdentContinue c.toNat)
  let compound := [">>", "<<", "::", "=>", "<=", ">=", "==", "!=", "&&", "||", "..", "#["] ++
    ((punctuation.map Prod.fst).filter (·.length > 1))
  let mut remaining := source.toList
  -- Lean Strings already contain valid Unicode scalars. Refuse the remaining
  -- SourceOk violations before tokenizing, including inside any literal.
  if remaining.any (fun c => c.toNat == 0 || c.toNat == 0xFEFF) then
    return [Token.invalid]
  let mut output := []
  for _ in [:remaining.length] do
    match remaining with
    | [] => break
    | c :: tail =>
      if ConcreteSpec.Surface0.Whitespace c.toNat then remaining := tail; continue
      let (token, rest) : Token × List Char :=
        if wordChar c then
          let word := remaining.takeWhile wordChar
          let w := String.ofList word
          let token := if c.isDigit then
              if (integerSuffix slice word).isSome then Token.ofTerminal .integer w else Token.invalid w
            else Token.ofTerminal (wordValue w).kind w
          (token, remaining.drop word.length)
        else if c == '\'' || c == '"' then
          if c == '"' then
            match quotedBody slice tail [] with
            | some (content, rest) => (Token.ofTerminal .stringLiteral (String.ofList content), rest)
            | none => (Token.invalid, [])
          else
            match tail with
            | '\\' :: escaped =>
              match escapeTail slice c escaped with
              | some ('\'' :: rest) =>
                (Token.ofTerminal .byteLiteral (String.ofList (tail.take (tail.length - rest.length - 1))), rest)
              | _ => (Token.invalid, [])
            | value :: '\'' :: rest =>
              let token := if ConcreteSpec.Surface0.ByteRaw value.toNat then
                  Token.ofTerminal .byteLiteral (String.singleton value) else Token.invalid (String.singleton value)
              (token, rest)
            | first :: _ =>
              if ConcreteSpec.Surface0.IdentStart first.toNat then
                let label := tail.takeWhile wordChar
                (Token.ofTerminal .label (String.ofList label), tail.drop label.length)
              else (Token.invalid, tail)
            | [] => (Token.invalid, [])
        else
          let w := (compound.find? (fun w => w.toList.isPrefixOf remaining)).getD (String.singleton c)
          let token := match punctuation.lookup w with
            | some terminal => Token.ofTerminal terminal w
            | none => ⟨w, w, none⟩
          (token, remaining.drop w.length)
      output := token :: output
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
          some (⟨">", ">", none⟩ :: tail)
        else none
      let (children, remaining) ← parseSymbols table splitClosers genericRhs (fuel - 1) rest tail
      some (.leaf { token with kind } :: children, remaining)
    | .n name :: rest =>
      let look := input.head?.map (·.kind)
      let rhs ← table[(name, look)]?.orElse fun _ =>
        if splitClosers && look == some ">>" then table[(name, some ">")]? else none
      let closesGenerics := ["TypeArgs", "Generics", "ImplTypeClose", "ImplCapClose"].contains name
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
        | .node "NameTail" [suffix] => suffix.isNode "Postfix"
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

def attributeValid (slice : Nat) (name : String) (args : Tree) :
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
  listedShape.getD true

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
    if ["Atom", "HeadAtom", "NameTail", "HeadNameTail"].contains name then children.any isPostfixCall else false
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
  partial def draftRestrictions (slice : Nat) (inContract : Bool) :
      Tree → Bool
    | .leaf token =>
      token.kind != "PRIMITIVE" ||
        (token.primitiveSlice.any (· ≤ slice))
    | .node "GroupAtom" [_, group, _, suffixTree] =>
      (!suffixTree.hasTokens || traceableGroupBase group) &&
        draftRestrictions slice inContract group &&
        draftRestrictions slice inContract suffixTree
    | .node "GroupedBody" [_, group, _, suffixTree] =>
      (!placeTailProjectsOrAssigns suffixTree || traceableGroupBase group) &&
        draftRestrictions slice inContract group &&
        draftRestrictions slice inContract suffixTree
    | .node name (leading :: following) =>
      let inProof := inContract || ["ProofExpr", "ProofArgs"].contains name
      (name != "ExactArithmetic" || inProof) &&
        (if leading.isNode "Attrs" || leading.isNode "AttributePrefix" then
          attributesValid slice (attributeAttachment name following) leading
        else draftRestrictions slice inProof leading) &&
        following.all (draftRestrictions slice inProof)
    | .node _ [] => true

  partial def attributesValid (slice : Nat) (target : Attachment) :
      Tree → Bool
    | .node "Attribute" [_, .leaf name, args, _] =>
      attributeValid slice name.spelling args &&
        attachmentValid slice name.spelling target &&
        draftRestrictions slice (contractAttribute slice name.spelling) args
    | .node name children =>
      ["Attrs", "AttributePrefix"].contains name &&
        children.all (attributesValid slice target)
    | _ => false
end

-- Stop at the value argument. Nested calls, target types and trailing commas
-- cannot add operands to this call's arity or consume one of its formals.
private partial def callOperands : Tree → List Tree
  | t@(.node "Expr" _) | t@(.node "ConstExpr" _) => [t]
  | .node "TargetArgument" _ => []
  | .node _ parts => parts.flatMap callOperands
  | .leaf _ => []

private def targetArgument? : Tree → Option Tree
  | .node _ (t@(.node "TargetArgument" _) :: _) => some t
  | _ => none

def targetArity (args : Tree) : Option Nat :=
  (targetArgument? args).map fun _ => (callOperands args).length

-- Named operations represented by CExpr.convert, .wrapping and .saturating.
def constPrimitiveArity (callee : String) : Nat :=
  if callee == "widen" then 1
  else if ["wrapping_add", "wrapping_sub", "wrapping_mul",
    "saturating_add", "saturating_sub", "saturating_mul"].contains callee then 2
  else 0

def targetBuiltin (name : String) : Option (Nat × Nat) :=
  if ["try_convert", "checked_neg"].contains name then some (6, 1)
  else if ["checked_add", "checked_sub", "checked_mul", "checked_div", "checked_rem",
    "checked_shl", "checked_shr"].contains name then some (6, 2)
  else if ["truncate", "reinterpret"].contains name then some (2, 1)
  else match constPrimitiveArity name with
    | 0 => none
    | arity => some (2, arity)

-- At a named suffix the two preceding tokens distinguish a bare
-- identifier from a path segment. Validation follows the parsed suffix.
partial def validateNamedSuffixes (slice : Nat) (previous : Token × Token) (tree : Tree) :
    Option (Token × Token) := do
  match tree with
  | .leaf token => some (previous.2, token)
  | .node name children =>
    let bareName := previous.1.kind != "::" && previous.2.kind == "IDENT"
    match name, children with
    | "Body", .leaf callee :: .leaf opener :: _ =>
      if opener.kind == "(" && callee.kind == "IDENT" then guard ((targetBuiltin callee.spelling).isNone)
    | "NameCall", [_, args, _] =>
      match (if bareName then targetBuiltin previous.2.spelling else none), targetArity args with
      | some (firstSlice, arity), some actual => guard (slice >= firstSlice && arity == actual)
      | none, none => pure ()
      | _, _ => none
    | "ConstNameTail", [_, args@(.node "ConstArgs" _), _] =>
      guard (bareName &&
        (targetArity args).any (· == constPrimitiveArity previous.2.spelling))
    | _, .leaf root :: path :: _ =>
      if root.kind == "IDENT" && (targetBuiltin root.spelling).isSome &&
          (path.isNode "PathTail" || path.isNode "BodyNameTail") then
        guard ((path.tokens.take 2 |>.map (·.kind)) != ["::", "<"])
    | _, _ => pure ()
    children.foldlM (validateNamedSuffixes slice) previous

def parseSource (slice : Nat) (g : DraftGrammar) (table : PredictionTable)
    (source : String) : Option Tree := do
  let input := draftTokens slice source
  let some ([tree], []) :=
    parseSymbols table (slice >= 9) false ((input.length + 1) * (g.prods.length + 1)) [.n g.start] input
    | none
  return tree

-- An annotation belongs to the binding itself. Colons nested in a destructuring
-- pattern introduce field binders and supply no expected type for its initializer.
private def bindingAnnotation : Tree → Bool
  | .node "LetHead" parts => parts.any fun
    | .node "LetTail" tail => tail.any (Tree.isNode "Type")
    | child => child.isNode "Type"
  | .leaf token => token.kind == ":"
  | _ => false

private partial def Tree.collectNodes {α : Type}
    (select : String → List Tree → List α) : Tree → List α
  | .leaf _ => []
  | .node name parts => select name parts ++ parts.flatMap (Tree.collectNodes select)

-- A generic argument is outside the path that names a callee or constructor.
private partial def pathNames : Tree → List String
  | .leaf t => if t.kind == "IDENT" || t.primitiveSlice.isSome then [t.spelling] else []
  | .node "TypeArgs" _ => []
  | .node _ parts => parts.flatMap pathNames

private partial def parameterCount : Tree → Nat
  | .node "Params" [_, _, _, tail] => 1 + parameterCount tail
  | .node "ParamTail" [_, params] => parameterCount params
  | _ => 0

-- OD138 depends on the presence of a declared type. Its particular width and
-- the value's range are checked by typing. Keep each formal and field distinct.
private inductive DeclaredSlot where
  | parameter (path : List String) (index : Nat)
  | field (path : List String) (name : String)
  deriving BEq

private def directIdentifier (parts : List Tree) : Option String :=
  parts.findSome? fun
    | .leaf t => if t.kind == "IDENT" then some t.spelling else none
    | _ => none

private def fieldSlots (path : List String) (fields : Tree) : List DeclaredSlot :=
  fields.collectNodes fun name parts =>
    if name == "Fields" || name == "VariantFields" then
      (directIdentifier parts).toList.map (.field path ·)
    else []

private partial def declaredSlots (tree : Tree) (owner : List String := []) : List DeclaredSlot :=
  match tree with
  | .node "Decl" (.leaf kw :: parts) =>
    match kw.kind, directIdentifier parts with
    | "struct", some name =>
      (parts.find? (·.isNode "Fields")).toList.flatMap (fieldSlots [name])
    | "enum", some name =>
      tree.collectNodes fun tag variant =>
        match tag, variant with
        | "Variants", .leaf v :: fields :: _ => fieldSlots [name, v.spelling] fields
        | _, _ => []
    | "impl", _ =>
      -- The final outer Type in the header is the receiver in both impl forms.
      let rec receivers : Tree → List Tree
        | t@(.node "Type" _) => [t]
        | .node _ children => children.flatMap receivers
        | _ => []
      match parts with
      | header :: _ :: members :: _ =>
        (receivers header).getLast?.toList.flatMap fun t => declaredSlots members (pathNames t)
      | _ => []
    | "fn", _ | "spec", _ => parameters (.leaf kw :: parts) owner
    | _, _ => []
  | .node name parts =>
    if ["Program", "Functions", "MoreFunctions", "Items", "Item", "Decl", "ImplMembers", "Function"].contains name
    then parameters parts owner ++ parts.flatMap (declaredSlots · owner)
    else []
  | _ => []
where
  parameters (parts : List Tree) (owner : List String) :=
    match parts.dropWhile (fun | .leaf t => t.kind != "fn" | _ => true) with
    | _ :: .leaf binder :: _ =>
      let count := (parts.find? (·.isNode "Params")).elim 0 parameterCount
      (List.range count).map (.parameter (owner ++ [binder.spelling]) ·)
    | _ => []

-- Carry just the callee or constructor path through its factored suffix.
private def suffixPath (name : String) (parts : List Tree) (base : List String) : List String :=
  match name, parts with
  | "Atom", .leaf root :: rest | "HeadAtom", .leaf root :: rest | "NamedBody", .leaf root :: rest =>
    if root.kind == "IDENT" then
      root.spelling :: ((rest.find? (·.isNode "PathTail")).toList.flatMap pathNames)
    else []
  | "BodyNameTail", .leaf sep :: path :: _ =>
    if sep.kind == "::" then base ++ pathNames path else base
  | "Body", .leaf kw :: path :: _ => if kw.kind == "defer" then pathNames path else []
  | _, path :: _ => if path.isNode "PrimitivePath" then pathNames path else base
  | _, _ => base

-- For a parsed operand, record whether its literals pass in either context and
-- whether its result can supply a peer's width. A grammar-only example may
-- contain unresolved names. Qualification checks the type sources visible here.
private inductive WidthSource where
  | unknown | needsWidth | suppliesWidth
  deriving BEq, Inhabited

private structure OperandWidth where
  free : Bool := true
  expected : Bool := true
  source : WidthSource := .unknown
  deriving Inhabited

private def OperandWidth.atPosition (w : OperandWidth) (supplied : Bool) :=
  if supplied then w.expected else w.free

-- Consume a child at its own position; its literals cannot inherit any later expectation.
private def OperandWidth.fixed (w : OperandWidth) (supplied : Bool) : OperandWidth :=
  let ok := w.atPosition supplied
  ⟨ok, ok, .unknown⟩

private def OperandWidth.and (a b : OperandWidth) : OperandWidth :=
  let source := match a.source, b.source with
    | .suppliesWidth, _ | _, .suppliesWidth => WidthSource.suppliesWidth
    | .needsWidth, _ | _, .needsWidth => .needsWidth
    | _, _ => .unknown
  ⟨a.free && b.free, a.expected && b.expected, source⟩

private def combineWidths (ws : List OperandWidth) : OperandWidth :=
  ws.foldl OperandWidth.and {}

private def peerOperands (a b : OperandWidth) : Bool :=
  a.atPosition (b.source == .suppliesWidth) && b.atPosition (a.source == .suppliesWidth)

-- OD48: two literal loop bounds need exactly one width source, in either order.
-- OD138 gives comparison operands the same relation; their Boolean result has
-- no bearing on either operand's integer width.
private def independentPair (a b : OperandWidth) : OperandWidth :=
  let ok := peerOperands a b && !(a.source == .needsWidth && b.source == .needsWidth)
  ⟨ok, ok, .suppliesWidth⟩

private def binaryWidth (op : String) (a b : OperandWidth) : OperandWidth :=
  if ["<", ">", "<=", ">=", "==", "!=", "&&", "||"].contains op then independentPair a b
  else { a.and b with free := peerOperands a b }

private def operatorName : Tree → Option String
  | .leaf t | .node "CompareOp" [.leaf t] => some t.kind
  | _ => none

private def isOperatorTail : Tree → Bool
  | .node name _ =>
    let level := ((name.dropPrefix "Head").dropPrefix "Const").toString
    ["AddTail", "LogicOrTail", "LogicAndTail", "EqualityTail", "CompareTail",
     "OrBits", "XorBits", "AndBits", "ShiftTail", "SumTail", "ProductTail"].contains level
  | _ => false

-- Fold the operators in the order given by the parsed precedence productions.
-- A body continuation resumes this same fold with its first operand already read.
private partial def continueWidth (visit : Tree → OperandWidth) (lhs : OperandWidth) : Tree → OperandWidth
  | .node _ [] => lhs
  | .node _ [first, rest] =>
    if let some op := operatorName first then binaryWidth op lhs (visit rest)
    else continueWidth visit (continueWidth visit lhs first) rest
  | .node _ [op, rhs, rest] =>
    match operatorName op with
    | some name => continueWidth visit (binaryWidth name lhs (visit rhs)) rest
    | none => ⟨false, false, .unknown⟩
  | _ => ⟨false, false, .unknown⟩

private def followedBy (visit : Tree → OperandWidth) (value : OperandWidth) (body : Tree) :=
  if body.hasTokens then (value.fixed false).and (visit body) else value

private partial def finishBodyValue (visit : Tree → OperandWidth) (value : OperandWidth) : Tree → OperandWidth
  | .node "ExprEnd" [] => value
  | .node "ExprEnd" [_, body] => (value.fixed false).and (visit body)
  | .node "MatchEnd" [body@(.node "Body" _)] => followedBy visit value body
  | .node "MatchEnd" [continuation] => continueWidth visit value continuation
  | t@(.node "MatchEnd" _) => continueWidth visit value t
  | .node _ [ops, ending@(.node "ExprEnd" _)] =>
    finishBodyValue visit (continueWidth visit value ops) ending
  | .node "PlaceTail" [proj, rest] =>
    finishBodyValue visit { (value.and (visit proj)).fixed false with source := .suppliesWidth } rest
  | .node _ [.leaf _, rhs, _, body] =>
    ((value.fixed false).and ((visit rhs).fixed true)).and (visit body)
  | _ => ⟨false, false, .unknown⟩

private partial def operandWidths (slice : Nat) (decls : List DeclaredSlot) (tree : Tree)
    (base : List String := []) : OperandWidth :=
  match tree with
  | .leaf t =>
    if t.kind == "INTEGER" then
      let bare := (integerSuffix slice t.spelling.toList).any List.isEmpty
      ⟨true, bare, if bare then .needsWidth else .suppliesWidth⟩
    else if ["IDENT", "BYTE", "true", "false"].contains t.kind then {source := .suppliesWidth}
    else {}
  | .node name children =>
    let path := suffixPath name children base
    let visit := fun child =>
      let named := ["BodyNameTail", "NameTail", "HeadNameTail", "NameCall", "NameLiteral",
        "Initializers", "InitializerTail"].any child.isNode
      operandWidths slice decls child (if named then path else [])
    let positions := fun (supplies : Tree → Bool) =>
      combineWidths (children.map fun child => (visit child).fixed (supplies child))
    match name, children with
    | "Program", _ | "Functions", _ | "Function", _ | "Decl", _ =>
      positions fun child => ["Block", "ConstExpr", "ProofExpr"].any child.isNode
    | "Body", .leaf first :: _ =>
      let typed := first.kind == "return" || children.any bindingAnnotation
      combineWidths (children.map fun child =>
        if child.isNode "Body" then visit child
        else (visit child).fixed (typed && ["Expr", "ReturnValue", "ProofExpr"].any child.isNode))
    | "ArmBody", .leaf first :: _ => positions (fun _ => first.kind == "return")
    | "Arms", _ =>
      combineWidths (children.map fun child =>
        if child.isNode "ArmBody" || child.isNode "Arms" then visit child else (visit child).fixed false)
    | "Initializers", [.leaf field, _, rhs, tail] =>
      ((visit rhs).fixed (decls.contains (.field base field.spelling))).and (visit tail)
    | "NamedBody", [.leaf _, tail] => visit tail
    | "NameCall", [_, args, _] =>
      let ok := (targetArgument? args).all (fun target => (visit target).free) &&
        (callOperands args).zipIdx.all fun (arg, i) =>
          (visit arg).atPosition (decls.contains (.parameter base i))
      ⟨ok, ok, .suppliesWidth⟩
    | "LoopForm", [_, _, _, lo, _, hi, body] =>
      (independentPair (visit lo) (visit hi)).and ((visit body).fixed false)
    | "Match", _ | "BlockExpr", _ | "BlockStmt", _ | "ValueElse", _ | "StmtElseBody", _ =>
      combineWidths (children.map fun child =>
        if child.isNode "Head" then (visit child).fixed false else visit child)
    | "Projection", _ | "Loop", _ | "BorrowPlace", _ | "BorrowRange", _
    | "AttributePrefix", _ | "Type", _ | "ConstNameTail", _ => positions (fun _ => false)
    | "ExactArithmetic", _ => {combineWidths (children.map visit) with source := .suppliesWidth}
    | _, [lhs, tail] =>
      if isOperatorTail tail then continueWidth visit (visit lhs) tail
      else bodyOrValue name visit children
    | _, _ => bodyOrValue name visit children
where
  bodyOrValue (name : String) (visit : Tree → OperandWidth) (children : List Tree) : OperandWidth :=
    match children.reverse with
    | ending :: earlier =>
      let first := earlier.reverse
      let value := fun (parts : List Tree) => combineWidths (parts.map visit)
      if ending.isNode "Body" then followedBy visit (value first) ending
      else if ["ExprEnd", "MatchEnd", "PlaceTail", "DerefTail"].any ending.isNode then
        let lhs := match earlier with
          | ops :: before =>
            if ops.isNode "ExprContinuation" then continueWidth visit (value before.reverse) ops
            else value first
          | [] => if name == "BodyNameTail" then {source := .suppliesWidth} else {}
        finishBodyValue visit lhs ending
      else value children
    | [] => {}

def literalSuffixesValid (slice : Nat) (tree : Tree) : Bool :=
  (operandWidths slice (declaredSlots tree) tree).free

def parseDraft (slice : Nat) (g : DraftGrammar) (table : PredictionTable)
    (source : String) : Option Tree := do
  let tree ← parseSource slice g table source
  guard (literalSuffixesValid slice tree && draftRestrictions slice false tree &&
    (validateNamedSuffixes slice (default, default) tree).isSome)
  return tree

def recognizes (slice : Nat) (g : DraftGrammar) (table : PredictionTable)
    (source : String) : Bool :=
  (parseDraft slice g table source).isSome

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
  | .node "MatchEnd" [.node "MatchContinuation" (first :: _)] =>
    (first.tokens.head?.map (·.kind)).toList
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

-- Independent ballot expectations. The production lexer alone owns wordTokens.
private def decidedWords : List (String × Terminal) := [
  ("fn", .fn), ("let", .let_), ("return", .return_), ("kept", .kept), ("_", .wildcard),
  ("u8", .u8), ("u16", .u16), ("u32", .u32), ("u64", .u64),
  ("i8", .i8), ("i16", .i16), ("i32", .i32), ("i64", .i64),
  ("Unit", .Unit_), ("bool", .bool_), ("char", .char_), ("Copy", .Copy), ("Self", .Self),
  ("struct", .struct_), ("enum", .enum_), ("type", .type_), ("trait", .trait_), ("impl", .impl_),
  ("move", .move_), ("mut", .mut_), ("borrow", .borrow_), ("defer", .defer_),
  ("if", .if_), ("else", .else_), ("match", .match_), ("true", .true_), ("false", .false_),
  ("while", .while_), ("for", .for_), ("from", .from_), ("until", .until),
  ("break", .break_), ("continue", .continue_),
  ("mod", .mod_), ("import", .import_), ("pub", .pub_), ("const", .const_),
  ("spec", .spec_), ("by", .by_), ("ghost", .ghost), ("assert", .assert_), ("assume", .assume_),
  ("exact", .exact_), ("trusted", .trusted_), ("extern", .extern_), ("cap", .cap), ("with", .with_),
  ("Fs", .Fs), ("Network", .Network), ("Env", .Env), ("Time", .Time), ("Alloc", .Alloc),
  ("Device", .Device), ("Console", .Console), ("Process", .Process), ("Unsafe", .Unsafe),
  ("Entropy", .Entropy)]

private def checkWordIdentities : IO Unit := do
  let spellings := decidedWords.map Prod.fst
  unless spellings.Nodup && spellings.mergeSort == (wordTokens.map Prod.fst).mergeSort do
    throw (IO.userError "decided word checks must cover the whole fixed word catalog")
  for (spelling, expected) in decidedWords do
    let actual := ConcreteSpec.Surface0.word spelling
    unless actual == expected do
      throw (IO.userError s!"keyword {spelling}: expected {repr expected}, got {repr actual}")
  IO.println s!"pass: {decidedWords.length} decided word identities"

-- Both sides of each pair must parse. checkProjection also checks the verdict
-- of the specific restriction, so a syntax error cannot satisfy a refusal.
private def operandExamples : Nat → List Example
  | 3 => [
    ⟨"an if condition supplies no integer width", fnBody "if 255u8 == 0 { return 1; }", true⟩,
    ⟨"the other literal supplies the width in an if condition",
      fnBody "if 255u8 == 0u8 { return 1; }", false⟩,
    ⟨"comparison in a typed Boolean binding", fnBody "let b: bool = 255u8 == 0; return 1;", true⟩,
    ⟨"assignment type reaches the RHS before another statement",
      fnBody "let mut x: u8 = 0; x = 1u8; return x;", false⟩,
    ⟨"assignment type reaches the last statement equally",
      fnBody "let mut x: u8 = 0; x = 1u8;", false⟩,
    ⟨"an unsuffixed assignment uses the place type",
      fnBody "let mut x: u8 = 0; x = 1; return x;", true⟩]
  | 4 => [
    ⟨"two bare loop bounds lack a width", fnBody "for i from 0 until 5 {}", false⟩,
    ⟨"lower loop bound supplies the width", fnBody "for i from 0u8 until 5 {}", true⟩,
    ⟨"upper loop bound supplies the width", fnBody "for i from 0 until 5u8 {}", true⟩,
    ⟨"two suffixed loop bounds duplicate the width", fnBody "for i from 0u8 until 5u8 {}", false⟩,
    ⟨"grouping and a minus preserve a literal bound", fnBody "for i from (-2i8) until (5i8) {}", false⟩]
  | 5 => [
    ⟨"declared argument supplies the width", "fn g(x: u8) -> u8 { x } fn f() -> u8 { g(255) }", true⟩,
    ⟨"declared argument refuses a suffix", "fn g(x: u8) -> u8 { x } fn f() -> u8 { g(255u8) }", false⟩,
    ⟨"forward declaration types the second argument in a head",
      "fn f() -> u8 { if g(0, 255u8) { 1 } else { 0 } } fn g(a: u8, b: u8) -> bool { true }", false⟩,
    ⟨"a nested call checks its own arguments",
      "fn g(x: u8) -> u8 { x } fn f() -> u8 { g(unknown(255u8)) }", true⟩,
    ⟨"Boolean result supplies no integer width", "fn f() -> bool { return 255u8 == 0; }", true⟩,
    ⟨"right comparison operand supplies the width", "fn f() -> bool { return 255 == 0u8; }", true⟩,
    ⟨"two widths in a Boolean return", "fn f() -> bool { return 255u8 == 0u8; }", false⟩,
    ⟨"no width in an integer comparison", "fn f() -> bool { return 255 == 0; }", false⟩,
    ⟨"comparison as a trailing expression", "fn f() -> bool { 255u8 == 0 }", true⟩,
    ⟨"condition compares with a typed parameter", "fn f(x: u8) -> bool { if x == 255u8 { true } else { false } }", false⟩,
    ⟨"condition uses the parameter width", "fn f(x: u8) -> bool { if x == 255 { true } else { false } }", true⟩]
  | 6 => [
    ⟨"declared field supplies the width", "struct S { a: u8 } fn f() -> S { return S { a: 200 }; }", true⟩,
    ⟨"declared field refuses a suffix", "struct S { a: u8 } fn f() -> S { return S { a: 200u8 }; }", false⟩,
    ⟨"each field checks its own operand", "struct S { a: u8, b: bool } fn f() -> S { S { a: 200, b: 255u8 == 0 } }", true⟩,
    ⟨"a field in a qualified variant has its declared width",
      "enum E { A { n: u8 } } fn f() -> E { E::A { n: 1u8 } }", false⟩,
    ⟨"a module function supplies no parameter to a different qualified call",
      "fn g(x: u8) -> u8 { x } fn f() -> u8 { m::g(255u8) }", true⟩]
  | 7 => [
    ⟨"dereference assignment before another statement",
      "fn f(p: &mut u8) -> u8 { *p = 255u8; return 0; }", false⟩,
    ⟨"dereference assignment supplies the width",
      "fn f(p: &mut u8) -> u8 { *p = 255; return 0; }", true⟩,
    ⟨"grouped leading comparison operand supplies the width", "fn f() -> bool { (255u8) == 0 }", true⟩,
    ⟨"grouping preserves both comparison operands' positions", "fn f() -> bool { (255u8) == 0u8 }", false⟩]
  | 8 => [
    ⟨"constant Boolean result supplies no integer width", "const OK: bool = 255u8 == 0;", true⟩]
  | 9 => [
    ⟨"qualified method supplies its parameter width",
      "impl S { fn g(x: u8) -> u8 { x } } fn f() -> u8 { S::g(255u8) }", false⟩,
    ⟨"method parameter uses the declared width",
      "impl S { fn g(x: u8) -> u8 { x } } fn f() -> u8 { S::g(255) }", true⟩]
  | 10 => [
    ⟨"Boolean spec result supplies no integer width", "spec fn ok() -> bool = 255u8 < 256;", true⟩]
  | _ => []

private def ghostExamples : List Example := [
  ⟨"ghost initializer is a proof expression",
    "fn f(x: u8) -> bool { ghost let ok: bool = exact { x + 1 } > x; return true; }", true⟩,
  ⟨"runtime initializer refuses the same exact expression",
    "fn f(x: u8) -> bool { let ok: bool = exact { x + 1 } > x; return true; }", false⟩,
  ⟨"proof context ends with the ghost initializer",
    "fn f(x: u8) -> bool { ghost let ok: bool = exact { x + 1 } > x; return exact { x + 1 } > x; }", false⟩,
  ⟨"enclosing proof context resumes after a ghost initializer",
    "spec fn f(x: u8) -> bool = if true { ghost let ok: bool = true; exact { x + 1 } > x } else { false };", true⟩]

def examples (slice currentSlice : Nat) : List Example :=
  let arm (pattern body : String) := s!"{pattern} => {body} ,"
  let binding := "borrow view = x"
  match slice with
  | 0 =>
    [⟨"typed binding and addition", fnBody "let x : u8 = 1; return x + 2;", true⟩,
     ⟨"main capability reserved before its first use", "fn Fs() -> u8 { return 1; }", false⟩,
     ⟨"main struct keyword reserved before its first use", "fn struct() -> u8 { return 1; }", false⟩] ++
    decidedWords.map (fun (name, _) =>
      ⟨s!"reserved binder {name}", s!"fn {name}() -> u8 \{ return 1; }", false⟩) ++
    (words "main self as in newtype union total math loan use Option Result Vec String Heap widen").map (fun name =>
      ⟨s!"identifier token {name}", s!"fn {name}() -> u8 \{ return 1; }", true⟩)
  | 1 => [
    ⟨"typed and destructuring lets share a prefix",
      "struct x { } " ++ fnBody "let x : x = x { } ; let x { } = move x ; return 1 ;", true⟩,
    ⟨"typed wildcard let", fnBody "let _ : u8 = 1; return 2;", true⟩,
    ⟨"wildcard token cannot bind a function name", "fn _() -> u8 { return 1; }", false⟩,
    ⟨"incomplete let", fnBody "let x = 1 ;", false⟩]
  | 2 => [
    ⟨"separators between digits", fnBody "return 0xf_f;", true⟩,
    ⟨"uppercase integer hex", fnBody "return 0xFF;", false⟩,
    ⟨"separator adjacent to radix prefix", fnBody "return 0x_ff;", false⟩,
    ⟨"redundant suffix in return", fnBody "return 255u8;", false⟩,
    ⟨"redundant suffix in typed let", fnBody "let x: u8 = (255u8); return x;", false⟩,
    ⟨"suffix without a supplied argument type", fnBody "return id(255u8);", true⟩,
    ⟨"lowercase hex byte", fnBody "return '\\xff';", true⟩,
    ⟨"target-type conversion", fnBody "return widen(type: u32, x);", true⟩,
    ⟨"target-type binary arithmetic", fnBody "return wrapping_add(type: u32, x, y);", true⟩,
    ⟨"target-type conversion rejects two operands", fnBody "return widen(type: u32, x, y);", false⟩,
    ⟨"target-type binary arithmetic rejects one operand", fnBody "return wrapping_add(type: u32, x);", false⟩,
    ⟨"conversion requires a target type", fnBody "return widen(x);", false⟩,
    ⟨"binary builtin requires a target type", fnBody "return wrapping_add(x, y);", false⟩,
    ⟨"eligible builtin name stays an identifier in a value position",
      fnBody "return wrapping_add;", true⟩,
    ⟨"ordinary call cannot take a target type",
      fnBody "return x ( type : u8 , 1 ) ;", false⟩,
    ⟨"bitwise NOT", fnBody "return ~x;", true⟩,
    ⟨"boolean NOT begins at slice 3", fnBody "return !x;", currentSlice >= 3⟩,
    ⟨"primitive keyword classification",
      "fn u32() -> u8 { return 1; }", false⟩,
    ⟨"ordinary named type", fnBody "let x : T = 1 ;", true⟩,
    ⟨"char type begins in slice 11", fnBody "let x: char = c; return 1;", currentSlice >= 11⟩,
    ⟨"bool type begins in slice 3", fnBody "let x: bool = b; return 1;", currentSlice >= 3⟩] ++
    [(2, "x", words "truncate reinterpret"),
     (2, "x, y", words "wrapping_sub wrapping_mul saturating_add saturating_sub saturating_mul"),
     (6, "x", words "checked_neg try_convert"),
     (6, "x, y", words "checked_add checked_sub checked_mul checked_div checked_rem checked_shl checked_shr")].flatMap
      fun (first, args, names) => names.flatMap fun name => [
        ⟨s!"target of {name}", fnBody s!"return {name}(type: u32, {args});", currentSlice >= first⟩,
        ⟨s!"missing target of {name}", fnBody s!"return {name}({args});", false⟩]
  | 3 => [
    ⟨"semicolon-free if statement", fnBody "if true { return 1 ; } return 1 ;", true⟩,
    ⟨"result type reaches the then branch", fnBody "return if true { 255u8 } else { 1 };", false⟩,
    ⟨"unsuffixed result in the then branch", fnBody "return if true { 255 } else { 1 };", true⟩,
    ⟨"result type reaches the else branch after a statement",
      fnBody "let x: u8 = if true { 0 } else { f(); 255u8 };", false⟩,
    ⟨"branch result type does not reach an earlier statement",
      fnBody "return if true { f(255u8); 1 } else { 0 };", true⟩,
    ⟨"value if with an operator continuation",
      fnBody "return if true { 1 } else { 1 } + 1 ;", true⟩,
    ⟨"statement if followed by a trailing unary minus",
      fnBody "if true { x ( ) ; } - 1", true⟩,
    ⟨"statement if followed by a unary-minus statement",
      fnBody "if true { x ( ) ; } - 1 ; return 1 ;", true⟩,
    ⟨"statement if followed by a trailing bitwise not",
      fnBody s!"if true \{ x ( ) ; } ~ 1", true⟩,
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
      fnBody "#[invariant(exact { x + 1 } > x)] while true {}", true⟩,
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
    ⟨"parenthesized exact arithmetic in a condition head",
      fnBody "#[invariant(if (exact { x + 1 } > x) { true } else { false })] while true { }", true⟩,
    ⟨"unshielded exact arithmetic in a condition head",
      fnBody "#[invariant(if exact { x + 1 } > x { true } else { false })] while true { }",
      true⟩,
    ⟨"exact arithmetic starts a proof branch",
      fnBody "#[invariant(if true { exact { 1 + 2 } > 0 } else { false })] while true {}", true⟩,
    ⟨"exact arithmetic with a literal-leading operand",
      fnBody "#[invariant(exact { 1 + x } > x)] while true { }", true⟩,
    ⟨"other identifiers cannot introduce exact arithmetic",
      fnBody "#[invariant(other { x + 1 } > x)] while true { }", false⟩,
    ⟨"unshielded exact head outside a proof context",
      fnBody "if exact { 1 + 2 } > 0 { return 1; }", false⟩,
    ⟨"exact arithmetic outside a proof context",
      fnBody "return exact { x + 1 };", false⟩,
    ⟨"loop invariant refuses a call attachment",
      fnBody "#[invariant(true)] f();", false⟩,
    ⟨"termination permission refuses an empty reason",
      fnBody "#[may_not_terminate(\"\")] f();", false⟩,
    ⟨"termination permission requires a call operand",
      fnBody "#[may_not_terminate(\"reason\")] 1;", false⟩,
    ⟨"termination permission refuses a counted loop",
      fnBody "#[may_not_terminate(\"reason\")] for i from 0u8 until 1 { }", false⟩,
    ⟨"termination permission refuses a call nested inside its operand",
      fnBody "#[may_not_terminate(\"reason\")] (1 + f());", false⟩,
    ⟨"string refuses a byte escape", fnBody "#[note(\"\\x41\")] while true {}", false⟩,
    ⟨"string refuses a surrogate", fnBody "#[note(\"\\u{d800}\")] while true {}", false⟩,
    ⟨"scalar escape in UTF-8 text", fnBody "#[note(\"\\u{10ffff}\")] while true {}", true⟩,
    ⟨"listed and general attributes at first use",
      fnBody s!"#[ invariant ( true ) ] #[ x ( \"reason\" ) ] for _ from 1u8 until 1 \{ } 1", true⟩,
    ⟨"listed attribute uses the listed envelope",
      fnBody "@[ invariant ( true ) ] while true { }", false⟩,
    ⟨"general attribute uses its selected envelope",
      fnBody "#[ x ] while true { }", true⟩,
    ⟨"labelled loop and labelled transfer",
      fnBody "'outer: while true { break 'outer; } 1", true⟩,
    ⟨"colonless loop label", fnBody "'outer while true { }", false⟩,
    ⟨"wildcard let", fnBody "let _ : u8 = 1 ; 1", true⟩]
  | 5 => [
    ⟨"exact arithmetic in a function contract",
      "#[ensures(exact { result + 1 } > result)] fn f() -> u8 { 1 }", true⟩,
    ⟨"optional trailing commas in parameters and calls",
      "fn x ( x : u8 , ) -> u8 { x ( 1 , ) }", true⟩,
    ⟨"invariant refuses a function attachment",
      "#[invariant(true)] fn f() -> u8 { 1 }", false⟩,
    ⟨"requires refuses a loop attachment",
      fnBody "#[requires(true)] while true { }", false⟩,
    ⟨"function attribute and wildcard parameter",
      s!"#[ x ( \"reason\" ) ] fn x ( _ : u8 ) -> u8 \{ 1 }", true⟩]
  | 6 => [
    ⟨"grouped named place projection", fnBody "return (x).field;", true⟩,
    ⟨"grouped conditional projection", fnBody "return (if true { a } else { b }).field;", false⟩,
    ⟨"grouped match projection",
      fnBody s!"return (match x \{ {arm "_" "a"} }).field;", false⟩,
    ⟨"binary multiply after body-leading match",
      fnBody s!"match x \{ {arm "_" "1"} } * 2", currentSlice == 6⟩,
    ⟨"match followed by a unary borrow statement",
      fnBody s!"match x \{ {arm "_" "1"} } & x;", currentSlice >= 7⟩,
    ⟨"match followed by a unary dereference statement",
      fnBody s!"match x \{ {arm "_" "1"} } * x;", currentSlice >= 7⟩,
    ⟨"grouped match permits both binary operators",
      fnBody s!"(match x \{ {arm "_" "1"} }) * 2 & 3", true⟩,
    ⟨"bare return in an unbraced arm", "fn f() -> Unit { match x { _ => return, } }", true⟩,
    ⟨"bare return in a braced arm", "fn f() -> Unit { match x { _ => { return; }, } }", true⟩,
    ⟨"explicit Unit return in a body", "fn f() -> Unit { return (); }", true⟩,
    ⟨"missing last arm comma", fnBody "match x { 0 => 1, _ => 2 }", false⟩,
    ⟨"last arm comma", fnBody "match x { 0 => 1, _ => 2, }", true⟩,
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
        fnBody ("match x { " ++ arm s!"x :: x \{ x : x , x : _ }" "1" ++ " }"), true⟩,
    ⟨"tuple variant is excluded", "enum x { x ( u8 ) }", false⟩,
    ⟨"tuple pattern is excluded",
      fnBody ("match x { " ++ arm s!"x :: x ( _ )" "1" ++ " }"), false⟩,
    ⟨"ignored payload", fnBody ("match x { " ++ arm "x :: x { .. }" "1" ++ " }"), true⟩,
    ⟨"match result reaches a braced arm",
      fnBody "return match x { 0 => { f(); (255u8) }, _ => 1, };", false⟩,
    ⟨"match result reaches an unbraced arm",
      fnBody "return match x { 0 => 1, _ => 255u8, };", false⟩,
    ⟨"braced match arm",
      fnBody ("match x { " ++ arm "_" "{ return 1 ; }" ++ " }"), true⟩,
    ⟨"unbraced return arm",
      fnBody ("match x { " ++ arm "_" "return 1" ++ " }"), true⟩,
    ⟨"match with addition as a trailing value",
      fnBody s!"match x \{ {arm "_" "1"} } + 1", true⟩,
    ⟨"match with addition cannot become an expression statement",
      fnBody s!"match x \{ {arm "_" "1"} } + 1 ;", false⟩,
    ⟨"unary minus can start the next statement after a match",
      fnBody s!"match x \{ {arm "_" "1"} } - 1 ;", true⟩,
    ⟨"bare match statement refuses a semicolon",
      fnBody s!"match x \{ {arm "_" "1"} } ;", false⟩,
    ⟨"semicolon-free match statement",
      fnBody ("match x { " ++ arm "_" "1" ++ " } return 1 ;"), true⟩,
    ⟨"bare struct match pattern", fnBody ("match x { " ++ arm "x { x }" "1" ++ " }"), false⟩,
    ⟨"qualified field shorthand", fnBody ("match x { " ++ arm "x :: x { x }" "1" ++ " }"), false⟩,
    ⟨"reserved kept refused as a match pattern", fnBody ("match x { " ++ arm "kept x" "1" ++ " }"), false⟩,
    ⟨"explicit struct field binding",
      fnBody s!"let x \{ x : x , x : _ } = x ; 1", true⟩,
    ⟨"struct let rejects field shorthand", fnBody "let x { x } = x ; 1", false⟩,
    ⟨"qualified destructuring let", fnBody s!"let x :: x \{ x : x , x : _ } = x ; 1", true⟩]
  | 7 => (
    ["if true { x ( ) ; }", "if true { x ( ) ; } else { x ( ) ; }",
     s!"match x \{ {arm "_" "{ x ( ) ; }"} }", s!"{binding} \{ x ( ) ; }"].flatMap fun block => [
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
    [⟨"borrow place with projections", fnBody "borrow v = (*p).a[i] { v }", true⟩,
     ⟨"borrow refuses a temporary", fnBody "borrow v = make() { v }", false⟩,
     ⟨"borrow refuses a sum", fnBody "borrow v = a + b { v }", false⟩,
     ⟨"borrow range and binder", fnBody "borrow mut v = a from 0 until n { v }", true⟩,
     ⟨"borrow range needs until", fnBody "borrow mut v = a from 0 .. n { v }", false⟩,
     ⟨"borrow binder comes first", fnBody "borrow mut a from 0 until n -> v { v }", false⟩]
  | 8 => [
    ⟨"import requires an item list", s!"import x ;", false⟩,
    ⟨"qualified import requires an item list", s!"import x :: x ;", false⟩,
    ⟨"empty import list", s!"import x :: x :: \{ } ;", false⟩,
    ⟨"qualified function call in constant initializer",
      "const x : u8 = x :: x ( 1 ) ;", false⟩,
    ⟨"nonempty import with trailing comma",
      s!"import x :: \{ x , } ;", true⟩,
    ⟨"constant target-type conversion",
      "const X : u32 = widen(type: u32, 1);", true⟩,
    ⟨"constant target-type binary arithmetic",
      "const X : u32 = wrapping_add(type: u32, 1, 2);", true⟩,
    ⟨"constant target-type binary arithmetic rejects one operand",
      "const X : u32 = wrapping_add(type: u32, 1);", false⟩,
    ⟨"public struct field", "struct x { pub x : u8 }", true⟩,
    ⟨"import at first use", "import a::b::{x, y};", true⟩,
    ⟨"constant arithmetic and reference", "const x : u8 = x :: x + 1 ;", true⟩,
    ⟨"byte literal is a scalar constant", "const x : u8 = 'a' ;", true⟩,
    ⟨"constant wrapping requires a target", "const x : u8 = wrapping_add(1, 1);", false⟩,
    ⟨"named saturating primitive", "const x : u8 = saturating_mul(type: u8, 1, 1);", true⟩,
    ⟨"constant conversion requires a target", "const x : u32 = widen(1);", false⟩,
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
    ⟨"widen uses the leading type marker", fnBody "widen::<u32>(x)", false⟩,
    ⟨"deferred numeric call requires a target", fnBody "defer wrapping_add(x, y);", false⟩,
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
    ⟨"exact arithmetic in specification function", "spec fn f() -> u8 = exact { 1 + 2 };", true⟩,
    ⟨"exact arithmetic starts a spec function branch",
      "spec fn f(x: u8) -> u8 = if x > 0 { exact { x + 1 } } else { 0 };", true⟩,
    ⟨"exact arithmetic starts a braced match arm",
      "spec fn f(x: u8) -> u8 = match x { 0 => { exact { x + 1 } }, _ => 0, };", true⟩,
    ⟨"exact arithmetic in assertions and assumptions",
      fnBody "assert(exact { x + 1 } > x); assume exact { x + 1 } > x; return 1;", true⟩,
    ⟨"exact arithmetic in theorem argument",
      fnBody "assert(true) by lemma(exact { x + 1 }); return 1;", true⟩,
    ⟨"proof context does not extend to the following runtime statement",
      fnBody "assert(true); return exact { 1 + 2 };", false⟩,
    ⟨"typed ghost binding refuses a redundant suffix", fnBody "ghost let x: u8 = 255u8;", false⟩,
    ⟨"typed ghost binding supplies a literal type", fnBody "ghost let x: u8 = 255;", true⟩,
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
    ⟨"qualified char methods",
      "fn f(c: char) -> u8 { char::from_ascii('a'); char::from_code_point(65); return char::code_point(c); }", true⟩,
    ⟨"qualified u8 call", fnBody "u8::example(1)", true⟩,
    ⟨"module, import and grant clause",
      s!"mod x ; import x :: x :: \{ x , x } ; pub fn x ( ) with ( Fs , Console ) -> u8 \{ 1 }", true⟩,
    ⟨"optional trailing grant comma",
      "fn x ( ) with ( Fs , ) -> u8 { 1 }", true⟩,
    ⟨"attribute string as an expression at slice 11",
      s!"#[ x ( \"reason\" ) ] " ++ fnBody "1", true⟩]
  | _ => []

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
    | ("HeadEqualityTail", ["==", "@HeadCompare"]) => ("HeadEqualityTail", ["<", "@HeadCompare"])
    | ("CompareOp", ["<"]) => ("CompareOp", ["=="])
    | rule => rule

-- Slice 8 call-resolution controls use the declarations and calls in the parsed module.
-- Each entry maps a visible path to the full path of the function it denotes.
private def identifierNames (tokens : List Token) : List String :=
  (tokens.filter (·.kind == "IDENT")).map (·.spelling)

private def functionBindings (name : String) (parts : List Tree) :
    List (List String × List String) :=
  match name, parts with
  | "Import", [_, .leaf root, _, path, _] =>
    let (modules, items) := path.tokens.span (·.kind != "{")
    let segments := identifierNames modules
    let qualifier := segments.getLastD root.spelling
    (identifierNames items).map fun item =>
      ([qualifier, item], root.spelling :: segments ++ [item])
  | "Decl", .leaf keyword :: .leaf binder :: _
  | "Decl", [.node "Function" (.leaf keyword :: .leaf binder :: _)] =>
    if keyword.kind == "fn" then [([binder.spelling], [binder.spelling])] else []
  | _, _ => []

private def valueCallees (name : String) (parts : List Tree) : List (List String) :=
  match name, parts with
  | "Atom", [.leaf root, path, .node "NameTail" [.node "NameCall" _]] =>
    [root.spelling :: identifierNames path.tokens]
  | _, _ => []

private def checkImportedCalls (parse : String → Option Tree) : IO Unit := do
  for (decls, callee, target) in [
      ("", "x", none), ("", "b::x", some ["a", "b", "x"]), ("", "b::y", none),
      ("fn x() -> u8 { return 0; }", "x", some ["x"])] do
    let source := "import a::b::{x}; " ++ decls ++ " fn f() -> u8 { return " ++ callee ++ "(); }"
    let some tree := parse source | throw (IO.userError s!"import control did not parse: {source}")
    let [path] := tree.collectNodes valueCallees | throw (IO.userError s!"expected one call: {source}")
    let scope := tree.collectNodes functionBindings
    unless scope.lookup path == target do throw (IO.userError s!"incorrect resolution for {source}")

def checkDraftBoundaries (slice : Nat) (g : DraftGrammar)
    (table : PredictionTable) (label : String) : IO Unit := do
  let parse := parseDraft slice g table
  let raw := parseSource slice g table
  let require (name : String) (ok : Bool) : IO Unit :=
    unless ok do throw (IO.userError s!"{label}: {name}")
  require "break and continue emit distinct loop-transfer terminals"
    ((draftTokens slice "break continue").map (·.kind) == ["break", "continue"] &&
      (Token.ofTerminal .continue_ "break").kind == "continue")
  require "uniform reservation of pub, const and mod"
    ((draftTokens slice "pub const mod").map (·.kind) == ["pub", "const", "mod"])
  if slice >= 3 then
    require "ordering binds more tightly than equality in the parsed head"
      ((parse precedenceSource).any hasOrderingBeforeEquality)
  if slice >= 4 then
    for (body, accepted) in [("#[variant(1)] while true {}", true),
        ("#[variant(1)] for i from 0u8 until 1 {}", false)] do
      let source := fnBody body
      require s!"OD104 attachment: {body}"
        ((raw source).any (fun tree => draftRestrictions slice false tree == accepted) &&
          (parse source).isSome == accepted)
    let permission := "#[may_not_terminate(\"reason\")] "
    let initial := [("if " ++ permission ++ "fp(x) { 1 } else { 1 }", ["fp", "(", "x", ")"]),
      ("while " ++ permission ++ "fp(x) {}", ["fp", "(", "x", ")"]),
      ("for i from 0 until " ++ permission ++ "fp(x) {}", ["fp", "(", "x", ")"])]
    let matchHeads := if slice >= 6 then [
      ("match " ++ permission ++ "u8::f() { _ => 1, }", ["u8", "::", "f", "(", ")"])] else []
    let borrows := if slice >= 7 then
      [("borrow view = x from 0 until " ++ permission ++ "fp(x) { view }", ["fp", "(", "x", ")"])]
      else []
    let generics := if slice >= 9 then [
      ("if " ++ permission ++ "m::fp::<u8>(x) { 1 } else { 1 }",
        ["m", "::", "fp", "::", "<", "u8", ">", "(", "x", ")"])] else []
    for (body, operand) in initial ++ matchHeads ++ borrows ++ generics do
      require s!"termination permission on the head call: {body}"
        ((parse (fnBody body)).any (fun tree => permissionOperands tree == [operand]))
    let grouped := fnBody ("if " ++ permission ++ "(fp(x)) { 1 } else { 1 }")
    require "termination permission refuses a call nested in a head group"
      ((raw grouped).any (fun tree => !draftRestrictions slice false tree) &&
        (parse grouped).isNone)
  if slice >= 6 then
    for body in ["let mut P { a: a, b: b } = p; a = 1;", "let mut One::A { v: v } = o; v = 1;"] do
      require s!"PAT-11 mutable destructuring: {body}" (parse (fnBody body)).isSome
    let structInvariant := "#[invariant(exact { field + 1 } > 0)] "
    let visibilities := if slice >= 8 then ["", "pub "] else [""]
    for visibility in visibilities do
      let source := structInvariant ++ visibility ++ "struct S { field: u8 }"
      require "struct invariant enters proof context at Slice 8"
        ((raw source).any (fun tree => draftRestrictions slice false tree == (slice >= 8)) &&
          (parse source).isSome == (slice >= 8))
    let unrelated := [structInvariant ++ "enum E { A, }", "#[variant(1)] struct S { field: u8 }"]
    for misplaced in unrelated do
      require s!"contract attribute refuses unrelated declaration attachment: {misplaced}"
        ((raw misplaced).any (fun tree => !draftRestrictions slice false tree) &&
          (parse misplaced).isNone)
  if slice >= 8 then
    checkImportedCalls parse
    for formula in ["try_convert(type: u8, 1)", "checked_add(type: u8, 1, 2)",
        "truncate(type: u8, 1)", "reinterpret(type: u8, 1)", "ordinary(type: u8, 1)",
        "wrapping_add(type: u8, 1)", "widen(type: u8, 1, 2)",
        "wrapping_add(type: u8, 1, try_convert(type: u8, 2))"] do
      let source := s!"const X : u8 = {formula};"
      require s!"closed constant primitive set and arity: {formula}"
        ((raw source).any (fun tree => (validateNamedSuffixes slice (default, default) tree).isNone) &&
          (parse source).isNone)

def checkProjection (slice : Nat) (rules : List Rule) : IO Unit := do
  let label := s!"slice {slice}"
  let (g, table) ← requireProjection label rules
  checkDraftBoundaries slice g table label
  let slices := List.range (slice + 1)
  let operands := slices.flatMap operandExamples
  let ghosts := if slice >= 10 then ghostExamples else []
  for (group, check) in [(operands, literalSuffixesValid slice), (ghosts, draftRestrictions slice false)] do
    for fixture in group do
      unless (parseSource slice g table fixture.input).any (fun t => check t == fixture.expected) do
        throw (IO.userError s!"{label}, {fixture.label}: wrong parsed operand or proof context: {fixture.input}")
  let fixtures := slices.flatMap (fun first => examples first slice) ++ operands ++ ghosts
  for fixture in fixtures do
    unless recognizes slice g table fixture.input == fixture.expected do
      throw (IO.userError s!"{label}, {fixture.label}: expected {fixture.expected}: {fixture.input}")
  if slice >= 2 then
    for malformed in ["wrapping_add(type: u32, x)", "widen(type: u32, x, y)",
      "ordinary(type: u32, x, y)"] do
      unless (parseSource slice g table (fnBody ("return " ++ malformed ++ ";"))).any
          (fun tree => (validateNamedSuffixes slice (default, default) tree).isNone) do
        throw (IO.userError s!"{label}: target argument control must reach the parsed call: {malformed}")
  if slice >= 4 then
    for misplaced in ["#[invariant(true)] f();", "#[may_not_terminate(\"\")] f();",
      "#[may_not_terminate(\"reason\")] 1;"] do
      unless (parseSource slice g table (fnBody misplaced)).any
          (fun tree => !draftRestrictions slice false tree) do
        throw (IO.userError s!"{label}: attribute control must reach the parsed attachment: {misplaced}")
    let source := fnBody "#[may_not_terminate(\"reason\")] fp(x) + g();"
    unless (parseDraft slice g table source).any
        (fun tree => permissionOperands tree == [["fp", "(", "x", ")"]]) do
      throw (IO.userError s!"{label}: termination permission must attach to fp(x)")
  if slice >= 6 then
    let operand := if slice == 6 then "2" else "x"
    for operator in ["*", "&"] do
      let source := fnBody s!"match x \{ _ => 1, } {operator} {operand}"
      let expected := if slice == 6 then operator else operator ++ " body"
      unless (parseDraft slice g table source).any (fun tree => matchEnds tree == [expected]) do
        throw (IO.userError s!"{label}: incorrect match boundary for {operator}")
  let accepts := fixtures.countP (·.expected)
  IO.println s!"pass: slice {slice}, {g.prods.length} productions, 0 conflicts, {accepts} accept / {fixtures.length - accepts} refuse examples"
  (← IO.getStdout).flush

def requireConflict (label : String) (rules : List Rule) : IO Unit := do
  match qualifyProjection rules with
  | .error (.conflict lhs lookahead ..) =>
    IO.println s!"pass: planted conflict for {label} at {lhs}, lookahead {repr lookahead}"
  | .error other => throw (IO.userError s!"{label}: expected a prediction conflict, got {repr other}")
  | .ok _ => throw (IO.userError s!"missing planted conflict: {label}")

-- OD138 supplies the typing context. A suffix is required exactly when the
-- surrounding expression supplies no type. The lexer retains the suffix.
def literalInContext (source : String) (expectedType : Bool) : Bool :=
  (integerSuffix 2 source.toList).any (fun suffix => suffix.isEmpty == expectedType)

def checkLiterals : IO Unit := do
  let kind (slice : Nat) (source : String) := (draftTokens slice source).map (·.kind)
  let cases := [
    ("1_000", "INTEGER"), ("0xff_ff", "INTEGER"), ("0b1_0", "INTEGER"),
    ("0xFF", "INVALID"), ("0x_ff", "INVALID"), ("1__0", "INVALID"), ("1_", "INVALID"),
    ("00_1", "INVALID"), ("255u9", "INVALID"), ("'\\x00'", "BYTE"), ("'\\xff'", "BYTE"),
    ("'\\xFF'", "INVALID"), ("'\\x100'", "INVALID"), ("\"\\x41\"", "INVALID"),
    ("\"\\u{d800}\"", "INVALID"), ("\"\\u{dfff}\"", "INVALID"), ("\"\\u{10FFFF}\"", "INVALID"),
    ("\"\\u{}\"", "INVALID"), ("\"\\u{110000}\"", "INVALID"), ("\"\\u{10ffff}\"", "STRING"),
    ("\"é\\n\\0\"", "STRING"), ("\"\\u{202e}\\u{feff}\"", "STRING")]
  for (source, token) in cases do
    unless kind 2 source == [token] do throw (IO.userError s!"literal {source}: expected {token}")
  for source in ["1_000", "'\\xff'", "\"\\u{1}\""] do
    unless kind 1 source == ["INVALID"] do throw (IO.userError s!"early literal: {source}")
  let forbidden := List.range 32 ++ List.range' 0x7F 33 ++
    [0x061C, 0x200E, 0x200F, 0x2028, 0x2029, 0xFEFF] ++
    List.range' 0x202A 5 ++ List.range' 0x2066 4
  for scalar in forbidden do
    unless kind 11 ("\"" ++ String.singleton (Char.ofNat scalar) ++ "\"") == ["INVALID"] do
      throw (IO.userError s!"forbidden raw string scalar accepted: {scalar}")
  unless kind 11 (String.singleton (Char.ofNat 0xFEFF) ++ "fn") == ["INVALID"] &&
      kind 2 "255u8" == ["INTEGER"] && !literalInContext "255u8" true &&
      literalInContext "255u8" false && literalInContext "255" true && !literalInContext "255" false do
    throw (IO.userError "raw BOM or expected-type suffix control failed")
  IO.println s!"pass: {cases.length} literal examples, slice boundary, raw scalars and expected-type suffix controls"

def main : IO Unit := do
  unless (wordTokens.map Prod.fst).Nodup && (wordTokens.map Prod.snd).Nodup do
    throw (IO.userError "word catalog requires one spelling per fixed terminal")
  checkWordIdentities
  checkLiterals
  let sections ← readSections (← IO.FS.readFile "docs/grammar/slices-0-11.grammar")
  let mut rules := []
  for (delta, slice) in sections.toList.zipIdx do
    rules := extend rules delta
    checkProjection slice rules
    if slice == 0 then
      requireConflict "duplicate integer atom" (rules ++ [("Atom", ["INTEGER"])])
    if slice == 3 then
      let (reversed, table) ← requireProjection "reversed comparison precedence" (reversedComparisonPrecedence rules)
      unless (parseDraft slice reversed table precedenceSource).any
          (fun tree => !hasOrderingBeforeEquality tree) do
        throw (IO.userError "reversed precedence must still parse and fail the grouping assertion")
      IO.println "pass: parsed grouping catches reversed comparison precedence"
    if slice == 4 then
      requireConflict "duplicate attribute prefix"
        (rules ++ [("Attribute", ["#[", "IDENT", "]"])])
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
      requireConflict "operator continuation at block statement boundary" planted
      requireConflict "shared minus at the match boundary" (rules ++ [
        ("MatchEnd", ["-", "@Product", "@AfterProduct"])])
    if slice == 9 then
      requireConflict "unmarked generics" (rules ++ [("PathTail", ["@TypeArgs"])])
      let (g, table) ← requireProjection "slice 9 lexer adapter" rules
      let nested := draftTokens slice "fn f(x: Outer<Inner<u8>>) -> u8 { 1 }"
      let shift := draftTokens slice (fnBody "x>>y")
      let parse input split := parseSymbols table split false 2000 [.n g.start] input
      unless nested.any (·.kind == ">>") && (parse nested false).isNone &&
          (parse shift false).any (fun (_, rest) => rest.isEmpty) do
        throw (IO.userError "missing maximal-munch or generic-splitting control")
      IO.println "pass: shift uses maximal munch and nested types require the closer adapter"
    if slice == 6 then
      requireConflict "unshielded head literal" (rules ++ [("HeadNameTail", ["{", "@Initializers", "}"])])
      let broken := rules.filter (· != ("LetTail", ["{", "@PatternFields", "}"]))
      let (mutant, table) ← requireProjection "slice 6 with missing destructuring let" broken
      let earlier := examples 1 slice
      unless earlier.any (fun fixture => fixture.expected &&
          !recognizes slice mutant table fixture.input) do
        throw (IO.userError "missing cumulative destructuring control")
      IO.println "pass: earlier recognition control catches an LL(1) slice 6 regression"
