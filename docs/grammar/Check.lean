import ConcreteSpec.Spec.LL1

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
  ["Expr", "LogicOr", "LogicOrTail", "LogicAnd", "LogicAndTail", "Compare", "CompareTail",
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

-- Walk the left edge of the precedence ladder. Only the first operand of
-- a body expression needs a different atom (or a different leading unary).
def bodyLadder (namePrefix : String) (excludeUnary : List String) (rules : List Rule) : List Rule :=
  let spine := ["Expr", "LogicOr", "LogicAnd", "Compare", "BitOr", "BitXor", "BitAnd",
    "Shift", "Sum", "Product", "Unary", "Atom"]
  rules.filterMap fun (lhs, rhs) => do
    if !spine.contains lhs || rhs == ["@BlockExpr"] ||
        (lhs == "Unary" && excludeUnary.contains (rhs.headD "")) then none
    else
      let rhs := match rhs with
        | sym :: tail =>
          if spine.any (fun name => sym == "@" ++ name) then
            ("@" ++ namePrefix ++ (sym.drop 1).toString) :: tail
          else rhs
        | [] => []
      some (namePrefix ++ lhs, rhs)

def addExpressionLadders (slice : Nat) (rules : List Rule) : List Rule := Id.run do
  if slice < 3 then return rules
  let tails := ["ProductTail", "SumTail", "ShiftTail", "AndBits", "XorBits", "OrBits",
    "CompareTail", "LogicAndTail", "LogicOrTail"]
  -- A block-leading expression can continue with an operator, from the
  -- tightest precedence outwards. Otherwise the following token starts a body.
  let continuations := tails.zipIdx.flatMap fun (name, i) =>
    rules.filterMap fun (lhs, rhs) =>
      if lhs == name && !rhs.isEmpty then
        some ("BlockEnd", rhs ++ ((tails.drop (i + 1)).map ("@" ++ ·)))
      else none
  let afterBlock := rules.filterMap fun (lhs, rhs) =>
    if lhs == "Body" && rhs != ["@BlockExpr", "@BlockEnd"] then
      some ("AfterBlock", if rhs == ["@BodyExpr", "@ExprEnd"] then
        ["@AfterBlockExpr", "@ExprEnd"] else rhs)
    else none
  let body := bodyLadder "Body" [] rules
  let continuedBy := continuations.map fun (_, rhs) => rhs.headD ""
  let after := bodyLadder "AfterBlock" continuedBy rules
  let expanded := withExpressionLadder "Head" rules ++ body ++ after ++ afterBlock ++ continuations ++
    [("BlockEnd", ["@AfterBlock"]), ("BlockEnd", ["@BlockExpr", "@BlockEnd"])]
  if slice < 8 then return expanded
  let scalarRules := rules.filter fun (lhs, rhs) =>
    lhs != "Unary" || ["-", "!", "BIT_NOT", "@Atom"].contains (rhs.headD "")
  return expanded ++ ((withExpressionLadder "Const" scalarRules).drop scalarRules.length)

structure Options where
  bitNot : String
  primitiveKeyword : Bool
  wildcardToken : Bool
  alternateAttribute : Bool
  armSeparator : String
  borrowWord : String
  range : String × String
  binderMark : String
  importWord : String
  prefixBorrowBinder : Bool
  deriving Inhabited, Repr

def representative : Options :=
  ⟨"~", true, true, false, ",", "borrow", ("from", "until"), "->", "import", false⟩

def instantiate (options : Options) (rules : List Rule) : DraftGrammar :=
  let replace (sym : String) :=
    match sym with
    | "BIT_NOT" => options.bitNot
    | "WILDCARD" => if options.wildcardToken then "_" else "RESERVED_UNDERSCORE"
    | "ARM_SEPARATOR" => options.armSeparator
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
  let attributes := if options.alternateAttribute then
      rules.filterMap fun (lhs, rhs) => match lhs, rhs with
        | "Attrs", "#[" :: tail => some (lhs, "@[" :: tail)
        | _, _ => none
    else []
  let rules := ((rules.filter allows) ++ attributes).map fun (lhs, rhs) =>
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

abbrev PredictionTable := List ((String × Option String) × List (Sym String String))

def requireLL1 (label : String) (g : DraftGrammar) : IO PredictionTable := do
  unless productive g do throw (IO.userError s!"{label}: undefined or unproductive")
  let (nu, fi, fo) := compute g
  let predictions := g.prods.map fun (lhs, rhs) =>
    (lhs, rhs, (predictB nu fi fo lhs rhs).eraseDups)
  unless check g nu fi fo do
    for ((lhs, rhs, look), i) in predictions.zipIdx do
      for (other, alt, next) in predictions.drop (i+1) do
        let overlap := look.filter next.contains
        if lhs == other && !overlap.isEmpty then
          IO.eprintln s!"{lhs}: {repr rhs} vs {repr alt}, lookahead {repr overlap}"
    throw (IO.userError s!"{label}: LL(1) conflict")
  return predictions.flatMap fun (lhs, rhs, look) =>
    look.map fun token => ((lhs, token), rhs)

def recognizes (g : DraftGrammar) (table : PredictionTable) (input : List String) : Bool := Id.run do
  let mut stack := [Sym.n g.start]
  let mut remaining := input
  for _ in [: (input.length + 1) * (g.prods.length + 1)] do
    match stack with
    | [] => return remaining.isEmpty
    | .t sym :: tail =>
      if remaining.head? != some sym then return false
      remaining := remaining.drop 1
      stack := tail
    | .n name :: tail =>
      match table.lookup (name, remaining.head?) with
      | none => return false
      | some rhs => stack := rhs ++ tail
  return false

structure Example where
  label : String
  input : String
  expected : Bool := true

def fnBody (body : String) : String :=
  "fn IDENT ( ) -> u8 { " ++ body ++ " }"

def examples (slice : Nat) (options : Options) : List Example :=
  let wildcard := if options.wildcardToken then "_" else "RESERVED_UNDERSCORE"
  let attr := if options.alternateAttribute then "@[" else "#["
  let sep := options.armSeparator
  let arm (pattern body : String) := s!"{pattern} => {body} {sep}"
  let borrow := options.borrowWord
  let binding := if options.prefixBorrowBinder then s!"{borrow} IDENT = IDENT"
    else s!"{borrow} IDENT {options.binderMark} IDENT"
  match slice with
  | 1 => [
    ⟨"typed and destructuring lets share a prefix",
      "struct IDENT { } " ++ fnBody "let IDENT : IDENT = IDENT { } ; let IDENT { } = move IDENT ; return INTEGER ;", true⟩,
    ⟨"incomplete let", fnBody "let IDENT = INTEGER ;", false⟩]
  | 2 => [
    ⟨"bitwise NOT in a return at first use", fnBody s!"return {options.bitNot} INTEGER ;", true⟩,
    ⟨"primitive keyword classification",
      "struct IDENT { } fn IDENT ( ) -> u8 { let IDENT : PRIMITIVE = INTEGER ; }", options.primitiveKeyword⟩,
    ⟨"primitive identifier classification", fnBody "let IDENT : IDENT = INTEGER ;", true⟩]
  | 3 => [
    ⟨"semicolon-free if statement", fnBody "if true { return INTEGER ; } return INTEGER ;", true⟩,
    ⟨"block expression with an operator continuation", fnBody "if true { INTEGER } else { INTEGER } + INTEGER", true⟩,
    ⟨"expression statements and a final expression", fnBody "IDENT ( ) ; IDENT = INTEGER ; IDENT", true⟩,
    ⟨"final expression requires closing brace", fnBody "IDENT return INTEGER ;", false⟩]
  | 4 => [
    ⟨"listed and general attributes at first use",
      fnBody s!"#[ IDENT ] {attr} IDENT ( STRING ) ] for {wildcard} from INTEGER until INTEGER \{ } INTEGER", true⟩,
    ⟨"labelled loop and labelled transfer",
      fnBody "LABEL : while true { break LABEL ; } INTEGER", true⟩,
    ⟨"colonless loop label", fnBody "LABEL while true { }", false⟩,
    ⟨"wildcard let", fnBody s!"let {wildcard} : u8 = INTEGER ; INTEGER", true⟩]
  | 5 => [
    ⟨"function attribute and wildcard parameter",
      s!"{attr} IDENT ( STRING ) ] fn IDENT ( {wildcard} : u8 ) -> u8 \{ INTEGER }", true⟩]
  | 6 => [
    ⟨"head brace opens the block", fnBody "if IDENT { INTEGER } else { INTEGER }", true⟩,
    ⟨"grouped struct literal in a head", fnBody "if ( IDENT { } ) { INTEGER }", true⟩,
    ⟨"call argument shields struct literal", fnBody "if IDENT ( IDENT { } ) { INTEGER }", true⟩,
    ⟨"unshielded struct literal in a head", fnBody "if IDENT { } { INTEGER }", false⟩,
    ⟨"named variant declaration and pattern",
      "enum IDENT { IDENT { IDENT : u8 , IDENT : u8 } } " ++
        fnBody ("match IDENT { " ++ arm s!"IDENT :: IDENT \{ IDENT : IDENT , IDENT : {wildcard} }" "INTEGER" ++ " }"), true⟩,
    ⟨"tuple variant and pattern",
      "enum IDENT { IDENT ( u8 ) } " ++
        fnBody ("match IDENT { " ++ arm s!"IDENT :: IDENT ( {wildcard} )" "INTEGER" ++ " }"), true⟩,
    ⟨"ignored payload", fnBody ("match IDENT { " ++ arm "IDENT :: IDENT { .. }" "INTEGER" ++ " }"), true⟩,
    ⟨"braced match arm",
      fnBody ("match IDENT { " ++ arm wildcard "{ return INTEGER ; }" ++ " }"), true⟩,
    ⟨"unbraced return arm",
      fnBody ("match IDENT { " ++ arm wildcard "return INTEGER" ++ " }"), true⟩,
    ⟨"semicolon-free match statement",
      fnBody ("match IDENT { " ++ arm wildcard "INTEGER" ++ " } return INTEGER ;"), true⟩,
    ⟨"bare struct match pattern", fnBody ("match IDENT { " ++ arm "IDENT { IDENT }" "INTEGER" ++ " }"), false⟩,
    ⟨"qualified field shorthand", fnBody ("match IDENT { " ++ arm "IDENT :: IDENT { IDENT }" "INTEGER" ++ " }"), false⟩,
    ⟨"reserved kept refused as a match pattern", fnBody ("match IDENT { " ++ arm "kept IDENT" "INTEGER" ++ " }"), false⟩,
    ⟨"explicit struct field binding",
      fnBody s!"let IDENT \{ IDENT : IDENT , IDENT : {wildcard} } = IDENT ; INTEGER", true⟩,
    ⟨"struct let rejects field shorthand", fnBody "let IDENT { IDENT } = IDENT ; INTEGER", false⟩,
    ⟨"qualified destructuring let", fnBody s!"let IDENT :: IDENT \{ IDENT : IDENT , IDENT : {wildcard} } = IDENT ; INTEGER", true⟩]
  | 7 => [
    ⟨"index brackets shield struct literal", fnBody "if IDENT [ IDENT { } ] { INTEGER }", true⟩,
    ⟨"scoped borrow and block else", fnBody s!"{binding} \{ IDENT } else \{ INTEGER }", true⟩,
    ⟨"semicolon-free borrow statement", fnBody s!"{binding} \{ IDENT } return INTEGER ;", true⟩,
    ⟨"borrow else accepts only a block", fnBody s!"{binding} \{ IDENT } else if true \{ INTEGER }", false⟩,
    ⟨"array repetition", fnBody "[ INTEGER ; INTEGER ]", true⟩,
    ⟨"empty array literal", fnBody "[ ]", false⟩] ++
    (if options.prefixBorrowBinder then [] else [
      ⟨"borrow range and binder",
        fnBody s!"{borrow} mut IDENT {options.range.1} INTEGER {options.range.2} INTEGER {options.binderMark} IDENT \{ IDENT }", true⟩])
  | 8 => [
    ⟨"public struct field", "struct IDENT { pub IDENT : u8 }", true⟩,
    ⟨"import at first use", s!"{options.importWord} IDENT :: IDENT :: \{ IDENT , IDENT } ;", true⟩,
    ⟨"constant arithmetic and reference", "const IDENT : u8 = IDENT :: IDENT + INTEGER ;", true⟩,
    ⟨"byte literal is a scalar constant", "const IDENT : u8 = BYTE ;", true⟩,
    ⟨"named binary primitive shape", "const IDENT : u8 = IDENT ( INTEGER , INTEGER ) ;", true⟩,
    ⟨"named unary primitive shape", "const IDENT : u8 = IDENT ( INTEGER ) ;", true⟩,
    ⟨"constant initializer excludes empty calls", "const IDENT : u8 = IDENT ( ) ;", false⟩,
    ⟨"constant initializer excludes exclusive borrow", "const IDENT : u8 = & mut INTEGER ;", false⟩,
    ⟨"constant initializer excludes dereference", "const IDENT : u8 = * INTEGER ;", false⟩]
  | 9 => [
    ⟨"qualified generic call, field selection and comparison",
      fnBody "IDENT :: IDENT :: < IDENT < u8 > , Fs + Console , { } > ( INTEGER ) . IDENT < INTEGER", true⟩,
    ⟨"generic type with capability arguments",
      "type IDENT = IDENT :: IDENT < u8 , IDENT + Console , { } > ;", true⟩,
    ⟨"plain angle call collides with comparison", fnBody "IDENT < u8 > ( INTEGER )", false⟩,
    ⟨"nested generic closers are separate from a shift token",
      "fn IDENT ( IDENT : IDENT < IDENT < u8 >> ) -> u8 { INTEGER }", false⟩,
    ⟨"shift still uses the shift token", fnBody "INTEGER >> INTEGER", true⟩,
    ⟨"multiple trait bounds", "fn IDENT < IDENT : IDENT + Copy > ( ) -> u8 { INTEGER }", true⟩,
    ⟨"unbounded capability parameters follow type parameters",
      "fn IDENT < IDENT , cap IDENT , cap IDENT > ( ) -> u8 { INTEGER }", true⟩,
    ⟨"type parameter after a capability parameter",
      "fn IDENT < cap IDENT , IDENT > ( ) -> u8 { INTEGER }", false⟩,
    ⟨"capability bound", "fn IDENT < cap IDENT : Fs > ( ) -> u8 { INTEGER }", false⟩,
    ⟨"call-site cap marker", fnBody "IDENT :: < cap Fs > ( )", false⟩,
    ⟨"type argument after a capability argument", "type IDENT = IDENT < Fs , u8 > ;", false⟩,
    ⟨"generic trait declaration", "trait IDENT < IDENT > { }", false⟩,
    ⟨"generic trait method", "trait IDENT { fn IDENT < IDENT > ( ) -> u8 ; }", false⟩,
    ⟨"impl functions", "impl IDENT { " ++ fnBody "INTEGER" ++ " }", true⟩,
    ⟨"impl rejects module declaration", "impl IDENT { mod IDENT ; }", false⟩,
    ⟨"duplicate generic type argument lists", "type IDENT = IDENT :: < u8 > < u8 > ;", false⟩]
  | 10 => [
    ⟨"expression-bodied specification function", "spec fn IDENT ( ) -> u8 = INTEGER ;", true⟩,
    ⟨"specification function rejects a block body", "spec " ++ fnBody "INTEGER", false⟩]
  | 11 => [
    ⟨"module, import and grant clause",
      s!"mod IDENT ; {options.importWord} IDENT :: IDENT :: \{ IDENT , IDENT } ; pub fn IDENT ( ) with ( Fs , Console ) -> u8 \{ INTEGER }", true⟩,
    ⟨"attribute string as an expression at slice 11",
      s!"{attr} IDENT ( STRING ) ] " ++ fnBody "INTEGER", true⟩]
  | _ => []

-- Only branch on choices with a consumer in this projection. Three choices
-- rename fresh tokens bijectively. Exercise the joint rename on an alternate
-- grammar shape in each slice where it applies.
def optionsAt (slice : Nat) : List Options := Id.run do
  let bitNots := if slice >= 2 then ["~", "!"] else ["~"]
  let primitives := if slice >= 2 then [true, false] else [true]
  let attributes := if slice >= 4 then [false, true] else [false]
  let separators := if slice >= 6 then [",", ";"] else [","]
  let borrowBindings := if slice >= 7 then
    [(false, ("from", "until"), "->"), (false, ("from", "until"), ":"),
     (false, ("from", ".."), "->"), (false, ("from", ".."), ":"),
     (true, ("from", "until"), "->")]
    else [(false, ("from", "until"), "->")]
  let mut cases := []
  for bitNot in bitNots do
    for primitiveKeyword in primitives do
      for alternateAttribute in attributes do
        for armSeparator in separators do
          for (prefixBorrowBinder, range, binderMark) in borrowBindings do
            cases := { representative with
              bitNot, primitiveKeyword, alternateAttribute, armSeparator,
              prefixBorrowBinder, range, binderMark } :: cases
  cases := cases.reverse
  if slice < 4 then return cases
  return cases ++ [{ representative with
    bitNot := "!"
    primitiveKeyword := false
    alternateAttribute := true
    armSeparator := if slice >= 6 then ";" else ","
    wildcardToken := false
    range := if slice >= 7 then ("from", "..") else representative.range
    binderMark := if slice >= 7 then ":" else "->"
    borrowWord := if slice >= 7 then "loan" else "borrow"
    importWord := if slice >= 8 then "use" else "import" }]

def project (options : Options) (rules : List Rule) : DraftGrammar :=
  reachableProjection (instantiate options rules)

def checkOptions (slice : Nat) (rules : List Rule) : IO Unit := do
  let cases := optionsAt slice
  for options in cases do
    let label := s!"slice {slice}, {reprStr options}"
    let g := project options rules
    let table ← requireLL1 label g
    for fixture in examples slice options do
      unless recognizes g table (words fixture.input) == fixture.expected do
        throw (IO.userError s!"{label}, {fixture.label}: expected {fixture.expected}: {fixture.input}")
  IO.println s!"pass: slice {slice}, {cases.length} option cases"

def requireConflict (label : String) (g : DraftGrammar) : IO Unit := do
  if ll1 g then throw (IO.userError s!"missing planted conflict: {label}")
  IO.println s!"pass: planted conflict for {label}"

def main : IO Unit := do
  let sections ← readSections (← IO.FS.readFile "docs/grammar/slices-0-11.grammar")
  let mut cumulative := []
  for (delta, slice) in sections.toList.zipIdx do
    cumulative := extend cumulative delta
    let rules := addExpressionLadders slice cumulative
    checkOptions slice rules
    if slice == 4 then
      -- Duplicate the STRING path only in the alternate envelope. The default
      -- projection cannot see it, and slice 11 replaces the argument grammar.
      let planted := rules ++ [
        ("Attrs", ["@[", "IDENT", "@PlantedAttrArgs", "]", "@Attrs"]),
        ("PlantedAttrArgs", ["(", "STRING", ")"])]
      let alternate := { representative with alternateAttribute := true }
      let conflicts := (optionsAt slice).filter fun options => !(ll1 (project options planted))
      unless !conflicts.isEmpty && conflicts.all (·.alternateAttribute) &&
          ll1 (project alternate rules) do
        throw (IO.userError "missing slice 4 alternate attribute conflict")
      IO.println "pass: planted slice 4 alternate attribute conflict"
    if slice == 9 then
      let g := project representative rules
      requireConflict "unmarked generics" { g with prods := g.prods ++ [("PathTail", [.n "TypeArgs"])] }
    if slice == 6 then
      let g := project representative rules
      requireConflict "unshielded head literal" { g with prods := g.prods ++
        [("HeadNameTail", [.t "{", .n "Initializers", .t "}"])] }
