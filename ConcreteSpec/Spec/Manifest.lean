import ConcreteSpec.Spec.Ident

namespace ConcreteSpec.Project.Manifest

/-! A bounded syntax recognizer for the project configuration boundary. Admission and
wire-schema interpretation are separate. Container recursion spends the same budget as
table and dotted-key paths. Quoted keys are single path segments. -/

/-- Zero-based, half-open byte offsets in Concrete.toml. -/
structure ByteSpan where
  start : Nat
  stop : Nat
  deriving DecidableEq, Repr

inductive Error where
  | malformed (span : ByteSpan)
  | depth (span : ByteSpan)
  deriving DecidableEq, Repr

private inductive Kind where
  | invalid
  | mark (c : Char)
  | bare (text : String)
  | quoted (multi : Bool) (contents : Except ByteSpan String)

private structure Token where
  kind : Kind
  span : ByteSpan

private def look (eof : ByteSpan) (tokens : List Token) : ByteSpan :=
  (tokens.head?.map Token.span).getD eof

private def punctuation (c : Char) : Bool :=
  r"[]{}.,=".contains c

private def hex (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'a' ≤ c.toLower && c.toLower ≤ 'f' then some (c.toLower.toNat - 'a'.toNat + 10)
  else none

private def charSpan (offset : Nat) (input : List Char) : ByteSpan :=
  ⟨offset, offset + (input.head?.map Char.utf8Size).getD 0⟩

private def escape (offset : Nat) (cs : List Char) : Except ByteSpan (Char × Nat × List Char) := do
  let c :: tail := cs | throw (charSpan offset cs)
  let next := offset + c.utf8Size
  match c with
  | '"' | '\\' => return (c, next, tail)
  | 'b' => return (Char.ofNat 8, next, tail)
  | 't' => return ('\t', next, tail)
  | 'n' => return ('\n', next, tail)
  | 'f' => return (Char.ofNat 12, next, tail)
  | 'r' => return ('\r', next, tail)
  | 'u' | 'U' =>
    let n := if c == 'u' then 4 else 8
    let (digits, rest) := tail.splitAt n
    let mut code := 0
    let mut pos := next
    for d in digits do
      let some v := hex d | throw ⟨pos, pos + d.utf8Size⟩
      code := 16 * code + v
      pos := pos + 1
    if digits.length < n then throw ⟨pos, pos⟩
    if code > 0x10ffff || (0xd800 ≤ code && code ≤ 0xdfff) then throw ⟨next, pos⟩
    return (Char.ofNat code, pos, rest)
  | _ => throw (charSpan offset cs)

private def continuationPadding (pos : Nat) : List Char → Nat × List Char
  | ' ' :: rest | '\t' :: rest | '\n' :: rest => continuationPadding (pos + 1) rest
  | '\r' :: '\n' :: rest => continuationPadding (pos + 2) rest
  | rest => (pos, rest)

private def quoted (delim : Char) (multi : Bool) :
    Nat → Nat → List Char → List Char → Except ByteSpan (String × Nat × List Char)
  | 0, pos, cs, _ => .error (charSpan pos cs)
  | _, pos, [], _ => .error ⟨pos, pos⟩
  | fuel + 1, pos, c :: cs, acc => do
    let next := pos + c.utf8Size
    if c == delim then
      if !multi then return (String.ofList acc.reverse, next, cs)
      else if cs.take 2 == [delim, delim] then
        let rest := cs.drop 2
        let extra := (rest.take 2).takeWhile (· == delim)
        return (String.ofList (extra ++ acc).reverse, next + 2 + extra.length, rest.drop extra.length)
      else quoted delim multi fuel next cs (c :: acc)
    else if c == '\\' && delim == '"' then
      let padding := cs.dropWhile (fun d => d == ' ' || d == '\t')
      if multi && (padding.head? == some '\n' || padding.take 2 == ['\r', '\n']) then
        let (pos, rest) := continuationPadding next cs
        quoted delim multi fuel pos rest acc
      else
        let (decoded, pos, rest) ← escape next cs
        quoted delim multi fuel pos rest (decoded :: acc)
    else if c == '\r' && multi && cs.head? == some '\n' then
      quoted delim multi fuel (next + 1) (cs.drop 1) ('\n' :: acc)
    else if (c < ' ' && c != '\t' && !(multi && c == '\n')) || c.toNat == 127 then
      throw ⟨pos, next⟩
    else quoted delim multi fuel next cs (c :: acc)

-- A quoted token starts at its delimiter. Its contents carry their own failure
-- span, which matters only where the grammar accepts a quoted key or value.
private def lex : Nat → Nat → List Char → List Token → List Token
  | _, _, [], acc => acc.reverse
  | 0, pos, cs, acc => (⟨.invalid, charSpan pos cs⟩ :: acc).reverse
  | fuel + 1, pos, c :: cs, acc =>
    let next := pos + c.utf8Size
    if c == ' ' || c == '\t' then lex fuel next cs acc
    else if c == '\n' then lex fuel next cs (⟨.mark '\n', ⟨pos, next⟩⟩ :: acc)
    else if c == '\r' && cs.head? == some '\n' then
      lex fuel (next + 1) (cs.drop 1) (⟨.mark '\n', ⟨pos, next + 1⟩⟩ :: acc)
    else if c == '#' then
      match comment next cs with
      | .error span => (⟨.invalid, span⟩ :: acc).reverse
      | .ok (stop, rest) => lex fuel stop rest acc
    else if c == '"' || c == '\'' then
      let multi := cs.take 2 == [c, c]
      let (start, body) := if multi then (next + 2, cs.drop 2) else (next, cs)
      let (start, body) := if multi && body.head? == some '\n' then (start + 1, body.drop 1)
        else if multi && body.take 2 == ['\r', '\n'] then (start + 2, body.drop 2) else (start, body)
      match quoted c multi (fuel + 1) start body [] with
      | .error span => (⟨.quoted multi (.error span), ⟨pos, next⟩⟩ :: acc).reverse
      | .ok (str, stop, rest) => lex fuel stop rest (⟨.quoted multi (.ok str), ⟨pos, next⟩⟩ :: acc)
    else if punctuation c then lex fuel next cs (⟨.mark c, ⟨pos, next⟩⟩ :: acc)
    else if c.isWhitespace then (⟨.invalid, ⟨pos, next⟩⟩ :: acc).reverse
    else
      let tail := cs.takeWhile fun d =>
        !d.isWhitespace && !punctuation d && d != '#' && d != '"' && d != '\''
      let atom := String.ofList (c :: tail)
      let stop := pos + atom.utf8ByteSize
      lex fuel stop (cs.drop tail.length) (⟨.bare atom, ⟨pos, stop⟩⟩ :: acc)
where
  comment (pos : Nat) : List Char → Except ByteSpan (Nat × List Char)
    | [] => .ok (pos, [])
    | c :: cs =>
      if c == '\n' || c == '\r' then .ok (pos, c :: cs)
      else if (c < ' ' && c != '\t') || c.toNat == 127 then .error ⟨pos, pos + c.utf8Size⟩
      else comment (pos + c.utf8Size) cs

private def bareKeyChar (c : Char) : Bool :=
  c.toNat < 128 && (c.isAlphanum || c == '_' || c == '-')

-- Charge path depth only when a key segment actually starts. In particular,
-- an invalid scalar in skipped comment text cannot open another segment.
private def pathLimitError (tok : Token) : Error :=
  match tok.kind with
  | .quoted false _ => .depth tok.span
  | .bare text =>
    let first := text.front
    let span := ⟨tok.span.start, tok.span.start + first.utf8Size⟩
    if bareKeyChar first then .depth span else .malformed span
  | _ => .malformed tok.span

private def key (tok : Token) : Except Error String :=
  match tok.kind with
  | .quoted false contents => contents.mapError Error.malformed
  | .bare value =>
    match invalid tok.span.start value.toList with
    | none => .ok value
    | some span => .error (.malformed span)
  | _ => .error (.malformed tok.span)
where
  invalid (offset : Nat) : List Char → Option ByteSpan
    | [] => none
    | c :: cs =>
      if bareKeyChar c then invalid (offset + 1) cs
      else some ⟨offset, offset + c.utf8Size⟩

private def path (eof : ByteSpan) : Nat → List Token → Except Error (List String × List Token)
  | _, [] => .error (.malformed eof)
  | 0, tok :: _ => .error (pathLimitError tok)
  | budget + 1, tok :: ⟨.mark '.', _⟩ :: rest => do
    let segment ← key tok
    let (suffix, rest) ← path eof budget rest
    return (segment :: suffix, rest)
  | _ + 1, tok :: rest => do return ([← key tok], rest)

private def skipLines (tokens : List Token) : List Token :=
  tokens.dropWhile (fun tok => tok.kind matches .mark '\n')

private def decimalInteger (text : String) : Bool :=
  let digits := match text.toList with
    | '+' :: rest | '-' :: rest => rest
    | rest => rest
  match digits with
  | [] => false
  | '0' :: rest => rest.isEmpty
  | _ => digits.all (fun c => '0' ≤ c && c ≤ '9')

-- Container children use a strictly smaller budget, while siblings use token fuel.
private def value (eof : ByteSpan) : Nat → Nat → List Token → Except Error (List Token)
  | 0, _, tokens => .error (.malformed (look eof tokens))
  | fuel + 1, budget, tokens =>
    match tokens with
    | ⟨.quoted _ contents, _⟩ :: rest => contents.mapError Error.malformed *> .ok rest
    | ⟨.bare atom, span⟩ :: rest =>
      if atom == "true" || atom == "false" || decimalInteger atom then .ok rest
      else .error (.malformed span)
    | ⟨.mark '[', span⟩ :: rest => array span fuel budget (skipLines rest)
    | ⟨.mark '{', span⟩ :: rest => inlineTable span fuel budget [] rest
    | _ => .error (.malformed (look eof tokens))
termination_by fuel _ _ => fuel
where
  array (opener : ByteSpan) : Nat → Nat → List Token → Except Error (List Token)
    | _, 0, _ => .error (.depth opener)
    | 0, _, tokens => .error (.malformed (look eof tokens))
    | _ + 1, _ + 1, ⟨.mark ']', _⟩ :: rest => .ok rest
    | fuel + 1, budget + 1, tokens => do
      match skipLines (← value eof fuel budget tokens) with
      | ⟨.mark ']', _⟩ :: rest => return rest
      | ⟨.mark ',', _⟩ :: rest => array opener fuel (budget + 1) (skipLines rest)
      | tail => throw (.malformed (look eof tail))
  termination_by fuel _ _ => fuel
  inlineTable (opener : ByteSpan) : Nat → Nat → List (List String) → List Token → Except Error (List Token)
    | _, 0, _, _ => .error (.depth opener)
    | 0, _, _, tokens => .error (.malformed (look eof tokens))
    | _ + 1, _ + 1, [], ⟨.mark '}', _⟩ :: rest => .ok rest
    | fuel + 1, remaining + 1, seen, tokens => do
      let (segments, tail) ← path eof remaining tokens
      let ⟨.mark '=', _⟩ :: rest := tail | throw (.malformed (look eof tail))
      if seen.any (fun p => p.isPrefixOf segments || segments.isPrefixOf p) then
        throw (.malformed (look eof tokens))
      match ← value eof fuel (remaining - segments.length) rest with
      | ⟨.mark '}', _⟩ :: rest => return rest
      | ⟨.mark ',', _⟩ :: rest => inlineTable opener fuel (remaining + 1) (segments :: seen) rest
      | tail => throw (.malformed (look eof tail))
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

private def document (eof : ByteSpan) (limit : Nat) :
    Nat → List String → Definitions → List Token → Except Error Unit
  | _, _, _, [] => .ok ()
  | 0, _, _, tokens => .error (.malformed (look eof tokens))
  | fuel + 1, header, defs, tokens =>
    match tokens with
    | ⟨.mark '\n', _⟩ :: rest => document eof limit fuel header defs rest
    | ⟨.mark '[', _⟩ :: rest => do
      let (segments, afterPath) ← path eof limit rest
      let ⟨.mark ']', _⟩ :: tail := afterPath | throw (.malformed (look eof afterPath))
      if defs.explicitTables.contains segments || defs.dottedTables.contains segments ||
          defs.keys.any (·.isPrefixOf segments) then throw (.malformed (look eof rest))
      unless tail.isEmpty || (tail.head?.map Token.kind matches some (.mark '\n')) do
        throw (.malformed (look eof tail))
      document eof limit fuel segments { defs with explicitTables := segments :: defs.explicitTables } tail
    | _ => do
      let (segments, afterPath) ← path eof (limit - header.length) tokens
      let ⟨.mark '=', _⟩ :: rest := afterPath | throw (.malformed (look eof afterPath))
      let full := header ++ segments
      if defs.keys.any (fun p => p.isPrefixOf full || full.isPrefixOf p) ||
          defs.explicitTables.any (fun table =>
            full.isPrefixOf table ||
              (header != table && header.isPrefixOf table && table.isPrefixOf full)) then throw (.malformed (look eof tokens))
      let tail ← value eof (fuel + 1) (limit - full.length) rest
      unless tail.isEmpty || (tail.head?.map Token.kind matches some (.mark '\n')) do
        throw (.malformed (look eof tail))
      document eof limit fuel header
        { defs with keys := full :: defs.keys
                    dottedTables := dottedPrefixes header segments ++ defs.dottedTables } tail

def validate (limit : Nat) (text : String) : Except Error Unit :=
  let fuel := text.utf8ByteSize + 1
  document ⟨text.utf8ByteSize, text.utf8ByteSize⟩ limit fuel [] {} (lex fuel 0 text.toList [])

def byteLimit : Nat := 64 * 1024

def depthLimit : Nat := 16

/-- The provisional reader accepts at most 64 KiB of manifest bytes.
R-0589 and docs/decisions.md#decided-project-concrete-toml own the manifest boundary. -/
def SizeAccepted (bytes : Nat) : Prop := bytes ≤ byteLimit

instance (bytes : Nat) : Decidable (SizeAccepted bytes) :=
  inferInstanceAs (Decidable (bytes ≤ byteLimit))

/-- The manifest bytes must decode as UTF-8.
R-0589 and docs/decisions.md#decided-project-concrete-toml own the manifest boundary. -/
def EncodingAccepted (bytes : ByteArray) : Prop := (String.fromUTF8? bytes).isSome = true

instance (bytes : ByteArray) : Decidable (EncodingAccepted bytes) :=
  inferInstanceAs (Decidable ((String.fromUTF8? bytes).isSome = true))

/-- The bounded syntax recognizer must not exhaust its nesting budget of 16.
Malformed input has its own judgment. This check classifies the first refusal from the
recognizer, so it does not assign a depth to text that fails syntax before a depth check.
R-0589 and docs/decisions.md#decided-project-concrete-toml own the manifest boundary. -/
def DepthAccepted (text : String) : Prop :=
  (match validate depthLimit text with | .error (.depth _) => false | _ => true) = true

instance (text : String) : Decidable (DepthAccepted text) :=
  inferInstanceAs (Decidable (_ = true))

/-- The bounded recognizer must accept the supported manifest syntax.
R-0589 and docs/decisions.md#decided-project-concrete-toml track full TOML and schema decoding. -/
def SyntaxAccepted (text : String) : Prop :=
  (match validate depthLimit text with | .ok () => true | .error _ => false) = true

instance (text : String) : Decidable (SyntaxAccepted text) :=
  inferInstanceAs (Decidable (_ = true))

def sizeRule : Core.RuleId := ⟨``SizeAccepted, 1⟩
def encodingRule : Core.RuleId := ⟨``EncodingAccepted, 1⟩
def depthRule : Core.RuleId := ⟨``DepthAccepted, 1⟩
def syntaxRule : Core.RuleId := ⟨``SyntaxAccepted, 1⟩

end ConcreteSpec.Project.Manifest
