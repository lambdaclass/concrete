namespace Concrete.Driver.Manifest

/-! A bounded syntax recognizer for the project configuration boundary. Admission and
wire-schema interpretation are separate. Container recursion spends the same budget as
table and dotted-key paths. Quoted keys are single path segments. -/

inductive Error where
  | malformed
  | depth
  deriving DecidableEq, Repr

private inductive Token where
  | mark (c : Char)
  | bare (text : String)
  | quoted (text : String) (multi : Bool)
  deriving DecidableEq

private def punctuation (c : Char) : Bool :=
  r"[]{}.,=".contains c

private def hex (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c.toLower && c.toLower ≤ 'f' then some (c.toLower.toNat - 'a'.toNat + 10)
  else none

private def escape (cs : List Char) : Option (Char × List Char) := do
  let c :: tail := cs | none
  match c with
  | '"' | '\\' => some (c, tail)
  | 'b' => some (Char.ofNat 8, tail)
  | 't' => some ('\t', tail)
  | 'n' => some ('\n', tail)
  | 'f' => some (Char.ofNat 12, tail)
  | 'r' => some ('\r', tail)
  | 'u' | 'U' =>
    let n := if c == 'u' then 4 else 8
    let digits := tail.take n
    if digits.length != n then none else do
      let code ← digits.foldlM (fun acc d => (hex d).map fun x => 16 * acc + x) 0
      if code > 0x10ffff || (0xd800 ≤ code && code ≤ 0xdfff) then none
      else some (Char.ofNat code, tail.drop n)
  | _ => none

private def continuationPadding : List Char → List Char
  | ' ' :: rest | '\t' :: rest | '\n' :: rest | '\r' :: '\n' :: rest =>
    continuationPadding rest
  | rest => rest

private def quoted (delim : Char) (multi : Bool) :
    Nat → List Char → List Char → Option (String × List Char)
  | 0, _, _ | _, [], _ => none
  | fuel + 1, c :: cs, acc =>
    if c == delim then
      if !multi then some (String.ofList acc.reverse, cs)
      else if cs.take 2 == [delim, delim] then
        let rest := cs.drop 2
        let extra := (rest.take 2).takeWhile (· == delim)
        some (String.ofList (extra ++ acc).reverse, rest.drop extra.length)
      else quoted delim multi fuel cs (c :: acc)
    else if c == '\\' && delim == '"' then
      let padding := cs.dropWhile (fun d => d == ' ' || d == '\t')
      if multi && (padding.head? == some '\n' || padding.take 2 == ['\r', '\n']) then
        quoted delim multi fuel (continuationPadding padding) acc
      else do
        let (decoded, rest) ← escape cs
        quoted delim multi fuel rest (decoded :: acc)
    else if c == '\r' && multi && cs.head? == some '\n' then
      quoted delim multi fuel (cs.drop 1) ('\n' :: acc)
    else if (c < ' ' && c != '\t' && !(multi && c == '\n')) || c.toNat == 127 then none
    else quoted delim multi fuel cs (c :: acc)

private def lex : Nat → List Char → List Token → Except Error (List Token)
  | _, [], acc => .ok acc.reverse
  | 0, _, _ => .error .malformed
  | fuel + 1, c :: cs, acc =>
    if c == ' ' || c == '\t' then lex fuel cs acc
    else if c == '\n' then lex fuel cs (.mark '\n' :: acc)
    else if c == '\r' && cs.head? == some '\n' then
      lex fuel (cs.drop 1) (.mark '\n' :: acc)
    else if c == '#' then
      let comment := cs.takeWhile (fun d => d != '\n' && d != '\r')
      if comment.any (fun d => (d < ' ' && d != '\t') || d.toNat == 127) then .error .malformed
      else lex fuel (cs.drop comment.length) acc
    else if c == '"' || c == '\'' then do
      let multi := cs.take 2 == [c, c]
      let body := if multi then cs.drop 2 else cs
      let body := if multi && body.head? == some '\n' then body.drop 1
        else if multi && body.take 2 == ['\r', '\n'] then body.drop 2 else body
      let some (str, rest) := quoted c multi (fuel + 1) body [] | throw .malformed
      lex fuel rest (.quoted str multi :: acc)
    else if punctuation c then lex fuel cs (.mark c :: acc)
    else
      let tail := cs.takeWhile fun d =>
        !d.isWhitespace && !punctuation d && d != '#' && d != '"' && d != '\''
      if c.isWhitespace then .error .malformed
      else lex fuel (cs.drop tail.length) (.bare (String.ofList (c :: tail)) :: acc)

private def key : Token → Except Error String
  | .quoted value false => .ok value
  | .bare value =>
    if value.toList.all (fun c => c.toNat < 128 && (c.isAlphanum || c == '_' || c == '-'))
    then .ok value else .error .malformed
  | _ => .error .malformed

private def path : Nat → List Token → Except Error (List String × List Token)
  | 0, _ => .error .depth
  | _, [] => .error .malformed
  | budget + 1, tok :: .mark '.' :: rest => do
    let segment ← key tok
    let (suffix, rest) ← path budget rest
    return (segment :: suffix, rest)
  | _ + 1, tok :: rest => do return ([← key tok], rest)

private def skipLines (tokens : List Token) : List Token :=
  tokens.dropWhile (· == .mark '\n')

private def decimalInteger (text : String) : Bool :=
  let digits := match text.toList with
    | '+' :: rest | '-' :: rest => rest
    | rest => rest
  match digits with
  | [] => false
  | '0' :: rest => rest.isEmpty
  | _ => digits.all (fun c => '0' ≤ c && c ≤ '9')

-- Container children use a strictly smaller budget, while siblings use token fuel.
private def value : Nat → Nat → List Token → Except Error (List Token)
  | 0, _, _ => .error .malformed
  | fuel + 1, budget, tokens =>
    match tokens with
    | .quoted _ _ :: rest => .ok rest
    | .bare atom :: rest =>
      if atom == "true" || atom == "false" || decimalInteger atom then .ok rest
      else .error .malformed
    | .mark '[' :: rest => array fuel budget (skipLines rest)
    | .mark '{' :: rest => inlineTable fuel budget [] rest
    | _ => .error .malformed
termination_by fuel _ _ => fuel
where
  array : Nat → Nat → List Token → Except Error (List Token)
    | _, 0, _ => .error .depth
    | 0, _, _ => .error .malformed
    | _ + 1, _ + 1, .mark ']' :: rest => .ok rest
    | fuel + 1, budget + 1, tokens => do
      match skipLines (← value fuel budget tokens) with
      | .mark ']' :: rest => return rest
      | .mark ',' :: rest => array fuel (budget + 1) (skipLines rest)
      | _ => throw .malformed
  termination_by fuel _ _ => fuel
  inlineTable : Nat → Nat → List (List String) → List Token → Except Error (List Token)
    | _, 0, _, _ => .error .depth
    | 0, _, _, _ => .error .malformed
    | _ + 1, _ + 1, [], .mark '}' :: rest => .ok rest
    | fuel + 1, remaining + 1, seen, tokens => do
      let (segments, .mark '=' :: rest) ← path remaining tokens | throw .malformed
      if seen.any (fun p => p.isPrefixOf segments || segments.isPrefixOf p) then throw .malformed
      match ← value fuel (remaining - segments.length) rest with
      | .mark '}' :: rest => return rest
      | .mark ',' :: rest => inlineTable fuel (remaining + 1) (segments :: seen) rest
      | _ => throw .malformed
  termination_by fuel _ _ _ => fuel

private structure Definitions where
  explicitTables : List (List String) := []
  dottedTables : List (List String) := []
  keys : List (List String) := []

-- Only proper prefixes inside the active table are defined by a dotted assignment.
private def dottedPrefixes (header : List String) : List String → List (List String)
  | [] | [_] => []
  | segment :: next :: rest =>
    let tablePath := header ++ [segment]
    tablePath :: dottedPrefixes tablePath (next :: rest)

private def document (limit : Nat) :
    Nat → List String → Definitions → List Token → Except Error Unit
  | _, _, _, [] => .ok ()
  | 0, _, _, _ => .error .malformed
  | fuel + 1, header, defs, tokens =>
    match tokens with
    | .mark '\n' :: rest => document limit fuel header defs rest
    | .mark '[' :: rest => do
      let (segments, .mark ']' :: tail) ← path limit rest | throw .malformed
      if defs.explicitTables.contains segments || defs.dottedTables.contains segments ||
          defs.keys.any (·.isPrefixOf segments) then throw .malformed
      if tail != [] && tail.head? != some (.mark '\n') then throw .malformed
      document limit fuel segments { defs with explicitTables := segments :: defs.explicitTables } tail
    | _ => do
      let (segments, .mark '=' :: rest) ← path (limit - header.length) tokens | throw .malformed
      let full := header ++ segments
      if defs.keys.any (fun p => p.isPrefixOf full || full.isPrefixOf p) ||
          defs.explicitTables.any (fun table =>
            full.isPrefixOf table ||
              (header != table && header.isPrefixOf table && table.isPrefixOf full)) then throw .malformed
      let tail ← value (fuel + 1) (limit - full.length) rest
      if tail != [] && tail.head? != some (.mark '\n') then throw .malformed
      document limit fuel header
        { defs with keys := full :: defs.keys
                    dottedTables := dottedPrefixes header segments ++ defs.dottedTables } tail

def validate (limit : Nat) (text : String) : Except Error Unit := do
  let fuel := text.utf8ByteSize + 1
  document limit fuel [] {} (← lex fuel text.toList [])
