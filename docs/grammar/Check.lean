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

def addExpressionLadders (slice : Nat) (rules : List Rule) : List Rule :=
  let rules := if slice >= 8 then withExpressionLadder "Const" rules else rules
  if slice >= 3 then withExpressionLadder "Head" rules else rules

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
  deriving Inhabited, Repr

def representative : Options :=
  ⟨"~", true, true, false, ",", "borrow", ("from", "until"), "->", "import"⟩

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
    (options.primitiveKeyword || rhs != ["PRIMITIVE"]) &&
    !(options.bitNot == "!" && rhs == ["BIT_NOT", "@" ++ lhs] &&
      rules.contains (lhs, ["!", "@" ++ lhs]))
  let attributes := if options.alternateAttribute then
      rules.filterMap fun (lhs, rhs) => match lhs, rhs with
        | "Attrs", "#[" :: tail => some (lhs, "@[" :: tail)
        | _, _ => none
    else []
  let rules := ((rules.filter allows) ++ attributes).map fun (lhs, rhs) =>
    (lhs, rhs.map replace)
  ⟨"Program", rules.map fun (lhs, rhs) => (lhs, rhs.map fun sym =>
    if sym.startsWith "@" && sym != "@[" then .n (sym.drop 1).toString else .t sym)⟩

-- The repository's ll1 checks conflict freedom. Also check definedness and productivity
-- so a missing or circular draft rule cannot make a conflict disappear.
def productive (g : DraftGrammar) : Bool :=
  let defined := (g.prods.map Prod.fst).eraseDups
  let live := saturate (fun live => g.prods.filterMap fun (lhs, rhs) =>
    if rhs.all (fun | .t _ => true | .n sym => live.contains sym) then some lhs else none)
    defined.length []
  live.contains g.start && g.prods.all fun (lhs, rhs) =>
    live.contains lhs && rhs.all (fun | .t _ => true | .n sym => live.contains sym)

def requireLL1 (label : String) (g : DraftGrammar) : IO Unit := do
  unless productive g do throw (IO.userError s!"{label}: undefined or unproductive")
  unless ll1 g do
    let (nu, fi, fo) := compute g
    let predictions := g.prods.map fun (lhs, rhs) => (lhs, rhs, predictB nu fi fo lhs rhs)
    for ((lhs, rhs, look), i) in predictions.zipIdx do
      for (other, alt, next) in predictions.drop (i+1) do
        let overlap := (look.filter next.contains).eraseDups
        if lhs == other && !overlap.isEmpty then
          IO.eprintln s!"{lhs}: {repr rhs} vs {repr alt}, lookahead {repr overlap}"
    throw (IO.userError s!"{label}: LL(1) conflict")

def recognizes (g : DraftGrammar) (input : List String) : Bool := Id.run do
  let (nu, fi, fo) := compute g
  let table := g.prods.flatMap fun (lhs, rhs) =>
    ((predictB nu fi fo lhs rhs).eraseDups).map fun look => ((lhs, look), rhs)
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

def checkExample (label : String) (g : DraftGrammar) (input : String) (expected := true) : IO Unit := do
  unless recognizes g (words input) == expected do
    throw (IO.userError s!"{label}: expected recognition = {expected}: {input}")
  IO.println s!"pass: {label}"

def main : IO Unit := do
  let sections ← readSections (← IO.FS.readFile "docs/grammar/slices-0-11.grammar")
  let mut rules := []
  for (delta, slice) in sections.toList.zipIdx do
    rules := extend rules delta
    let g := instantiate representative (addExpressionLadders slice rules)
    requireLL1 s!"slice {slice}" g
    IO.println s!"pass: slice {slice}"
    if slice == 1 then
      checkExample "typed let and destructuring share a prefix" g
        "struct IDENT { } fn IDENT ( ) -> u8 { let IDENT : IDENT = IDENT { } ; let IDENT { } = move IDENT ; return INTEGER ; }"
      checkExample "incomplete let" g "fn IDENT ( ) -> u8 { let IDENT = INTEGER ; }" false
  rules := addExpressionLadders 11 rules
  let g := instantiate representative rules
  -- Enumerate shape changes and choices that reuse a token elsewhere in the grammar.
  -- The remaining choices bijectively rename fresh terminals.
  let mut count := 0
  for (bitNot, alternateAttribute) in [("~", false), ("!", false), ("~", true), ("!", true)] do
    for primitiveKeyword in [true, false] do
      for armSeparator in [",", ";"] do
        for range in [("from", "until"), ("from", "..")] do
          for binderMark in ["->", ":"] do
            let options : Options :=
              { representative with
                bitNot := bitNot
                alternateAttribute := alternateAttribute
                primitiveKeyword := primitiveKeyword
                armSeparator := armSeparator
                range := range
                binderMark := binderMark }
            requireLL1 (reprStr options) (instantiate options rules)
            count := count + 1
  IO.println s!"pass: {count} combined option classes"
  for options in [
      { representative with wildcardToken := false },
      { representative with borrowWord := "loan" },
      { representative with importWord := "use" }] do
    requireLL1 (reprStr options) (instantiate options rules)
  let fnBody (body : String) := "fn IDENT ( ) -> u8 { " ++ body ++ " }"
  checkExample "head brace opens the block" g (fnBody "if IDENT { INTEGER } else { INTEGER }")
  checkExample "grouped struct literal in a head" g (fnBody "if ( IDENT { } ) { INTEGER }")
  checkExample "call argument shields struct literal" g (fnBody "if IDENT ( IDENT { } ) { INTEGER }")
  checkExample "index brackets shield struct literal" g (fnBody "if IDENT [ IDENT { } ] { INTEGER }")
  checkExample "unshielded struct literal in a head" g (fnBody "if IDENT { } { INTEGER }") false
  checkExample "qualified call, field selection and comparison" g
    (fnBody "IDENT :: IDENT :: < IDENT < u8 > > ( INTEGER ) . IDENT < INTEGER")
  checkExample "plain angle call collides with comparison" g
    (fnBody "IDENT < u8 > ( INTEGER )") false
  checkExample "nested generic closers are separate from a shift token" g
    "fn IDENT ( IDENT : IDENT < IDENT < u8 >> ) -> u8 { INTEGER }" false
  checkExample "shift still uses the shift token" g (fnBody "INTEGER >> INTEGER")
  checkExample "attribute names and repeated attributes" g
    "#[ IDENT ( STRING ) ] #[ IDENT ] fn IDENT ( ) -> u8 { #[ IDENT ] while true { } INTEGER }"
  checkExample "expression statements and a final expression" g (fnBody "IDENT ( ) ; IDENT = INTEGER ; IDENT")
  checkExample "final expression requires closing brace" g (fnBody "IDENT return INTEGER ;") false
  checkExample "match, struct pattern and separators" g
    (fnBody "match IDENT { IDENT :: IDENT ( _ ) => INTEGER , kept IDENT => INTEGER }")
  checkExample "borrow range and binder" g
    (fnBody "borrow mut IDENT from INTEGER until INTEGER -> IDENT { IDENT } else { INTEGER }")
  let alternate := instantiate {
    representative with
    alternateAttribute := true
    borrowWord := "loan"
    importWord := "use"
    range := ("from", "..")
    binderMark := ":"
    wildcardToken := false
    armSeparator := ";"
    bitNot := "!"
    primitiveKeyword := false
  } rules
  checkExample "listed attribute and general attribute coexist" alternate
    "@[ IDENT ] #[ IDENT ] fn IDENT ( ) -> u8 { ! INTEGER }"
  checkExample "alternate borrow, range and binder" alternate
    (fnBody "loan IDENT from INTEGER .. INTEGER : IDENT { IDENT }")
  checkExample "alternate wildcard and arm separator" alternate
    (fnBody "match IDENT { RESERVED_UNDERSCORE => INTEGER ; }")
  checkExample "alternate import spelling" alternate "use IDENT :: { IDENT } ;"
  checkExample "constant initializer uses primitive operations" g "const IDENT : u8 = INTEGER + INTEGER ;"
  checkExample "constant initializer excludes calls" g "const IDENT : u8 = IDENT ( ) ;" false
  checkExample "module, import and grant clause" g
    "mod IDENT ; import IDENT :: IDENT :: { IDENT , IDENT } ; pub fn IDENT ( ) with ( Fs , Console ) -> u8 { INTEGER }"
  let rawGeneric := g.prods ++ [("PathTail", [.n "TypeArgs"])]
  if ll1 { g with prods := rawGeneric } then
    throw (IO.userError "unmarked expression generics should collide with comparison")
  IO.println "pass: planted unmarked generics conflict"
  let literalHead := g.prods ++ [("HeadNameTail", [.t "{", .n "Initializers", .t "}"])]
  if ll1 { g with prods := literalHead } then
    throw (IO.userError "unshielded head literals should collide with the block")
  IO.println "pass: planted head literal conflict"
