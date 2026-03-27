import Concrete.Token

namespace Concrete

structure LexerState where
  source : Array Char
  pos : Nat
  line : Nat
  col : Nat
  deriving Repr, Inhabited

def LexerState.init (source : String) : LexerState :=
  { source := source.toList.toArray, pos := 0, line := 1, col := 1 }

def LexerState.peek (s : LexerState) : Option Char :=
  if h : s.pos < s.source.size then some s.source[s.pos]
  else none

def LexerState.peekAt (s : LexerState) (offset : Nat) : Option Char :=
  let idx := s.pos + offset
  if h : idx < s.source.size then some s.source[idx]
  else none

def LexerState.advance (s : LexerState) : LexerState :=
  match s.peek with
  | some '\n' => { s with pos := s.pos + 1, line := s.line + 1, col := 1 }
  | some _ => { s with pos := s.pos + 1, col := s.col + 1 }
  | none => s

def LexerState.span (s : LexerState) : Span :=
  { line := s.line, col := s.col }

def LexerState.atEnd (s : LexerState) : Bool :=
  s.pos ≥ s.source.size

def lookupKeyword : String → Option TokenKind
  | "fn" => some .fn
  | "let" => some .«let»
  | "mut" => some .mut
  | "if" => some .if_
  | "else" => some .else_
  | "while" => some .while_
  | "for" => some .for_
  | "return" => some .return_
  | "true" => some .true_
  | "false" => some .false_
  | "mod" => some .«mod»
  | "struct" => some .struct_
  | "enum" => some .enum_
  | "match" => some .match_
  | "pub" => some .pub_
  | "import" => some .import_
  | "as" => some .as_
  | "impl" => some .impl_
  | "trait" => some .trait_
  | "const" => some .const_
  | "type" => some .type_
  | "extern" => some .extern_
  | "with" => some .with_
  | "cap" => some .cap_
  | "break" => some .break_
  | "continue" => some .continue_
  | "defer" => some .defer_
  | "borrow" => some .borrow_
  | "in" => some .in_
  | "newtype" => some .newtype_
  | "trusted" => some .trusted_
  | _ => none

private def isIdentStart (c : Char) : Bool :=
  c.isAlpha || c == '_'

private def isIdentCont (c : Char) : Bool :=
  c.isAlphanum || c == '_'

/-- Skip whitespace and line comments. -/
partial def skipWhitespace (s : LexerState) : LexerState :=
  match s.peek with
  | some ' ' | some '\t' | some '\n' | some '\r' =>
    skipWhitespace s.advance
  | some '/' =>
    let s2 := s.advance
    match s2.peek with
    | some '/' => skipLineComment s2.advance
    | _ => s
  | _ => s
where
  skipLineComment (s : LexerState) : LexerState :=
    match s.peek with
    | some '\n' => skipWhitespace s.advance
    | some _ => skipLineComment s.advance
    | none => s

/-- Lex an identifier or keyword. -/
partial def lexIdentLoop (s : LexerState) (acc : String) : LexerState × TokenKind :=
  match s.peek with
  | some c =>
    if isIdentCont c then
      lexIdentLoop s.advance (acc.push c)
    else
      (s, (lookupKeyword acc).getD (.ident acc))
  | none =>
    (s, (lookupKeyword acc).getD (.ident acc))

/-- Lex hex digits after 0x/0X prefix. -/
partial def lexHexLoop (s : LexerState) (acc : Nat) : LexerState × TokenKind :=
  match s.peek with
  | some c =>
    if c.isDigit then
      lexHexLoop s.advance (acc * 16 + (c.toNat - '0'.toNat))
    else if c >= 'a' && c <= 'f' then
      lexHexLoop s.advance (acc * 16 + (c.toNat - 'a'.toNat + 10))
    else if c >= 'A' && c <= 'F' then
      lexHexLoop s.advance (acc * 16 + (c.toNat - 'A'.toNat + 10))
    else
      (s, .intLit acc)
  | none => (s, .intLit acc)

/-- Lex binary digits after 0b/0B prefix. -/
partial def lexBinLoop (s : LexerState) (acc : Nat) : LexerState × TokenKind :=
  match s.peek with
  | some c =>
    if c == '0' || c == '1' then
      lexBinLoop s.advance (acc * 2 + (c.toNat - '0'.toNat))
    else
      (s, .intLit acc)
  | none => (s, .intLit acc)

/-- Lex octal digits after 0o/0O prefix. -/
partial def lexOctLoop (s : LexerState) (acc : Nat) : LexerState × TokenKind :=
  match s.peek with
  | some c =>
    if c >= '0' && c <= '7' then
      lexOctLoop s.advance (acc * 8 + (c.toNat - '0'.toNat))
    else
      (s, .intLit acc)
  | none => (s, .intLit acc)

/-- Lex a number literal (integer or float), including hex/bin/oct. -/
partial def lexNumberLoop (s : LexerState) (acc : Nat) : LexerState × TokenKind :=
  -- If acc is 0, check for hex/bin/oct prefix
  if acc == 0 then
    match s.peek with
    | some 'x' | some 'X' => lexHexLoop s.advance 0
    | some 'b' | some 'B' => lexBinLoop s.advance 0
    | some 'o' | some 'O' => lexOctLoop s.advance 0
    | _ => lexDecLoop s acc
  else
    lexDecLoop s acc
where
  lexDecLoop (s : LexerState) (acc : Nat) : LexerState × TokenKind :=
    match s.peek with
    | some c =>
      if c.isDigit then
        lexDecLoop s.advance (acc * 10 + (c.toNat - '0'.toNat))
      else if c == '.' then
        -- Check next char is a digit (to distinguish from field access)
        match s.peekAt 1 with
        | some c2 =>
          if c2.isDigit then
            lexFloatFrac s.advance (Float.ofNat acc) 0.1
          else
            (s, .intLit acc)
        | none => (s, .intLit acc)
      else
        (s, .intLit acc)
    | none => (s, .intLit acc)
  lexFloatFrac (s : LexerState) (acc : Float) (place : Float) : LexerState × TokenKind :=
    match s.peek with
    | some c =>
      if c.isDigit then
        let digit := Float.ofNat (c.toNat - '0'.toNat)
        lexFloatFrac s.advance (acc + digit * place) (place * 0.1)
      else
        (s, .floatLit acc)
    | none => (s, .floatLit acc)

/-- Lex a string literal (after opening quote). -/
partial def lexStringLoop (s : LexerState) (acc : String) : LexerState × TokenKind :=
  match s.peek with
  | some '"' => (s.advance, .strLit acc)
  | some '\\' =>
    let s := s.advance
    match s.peek with
    | some 'n' => lexStringLoop s.advance (acc.push '\n')
    | some 't' => lexStringLoop s.advance (acc.push '\t')
    | some 'r' => lexStringLoop s.advance (acc.push (Char.ofNat 13))
    | some '\\' => lexStringLoop s.advance (acc.push '\\')
    | some '"' => lexStringLoop s.advance (acc.push '"')
    | some '0' => lexStringLoop s.advance (acc.push (Char.ofNat 0))
    | some c => lexStringLoop s.advance (acc.push c)
    | none => (s, .strLit acc)
  | some c => lexStringLoop s.advance (acc.push c)
  | none => (s, .strLit acc)

/-- Lex a char literal (after opening single quote). -/
def lexCharLit (s : LexerState) : LexerState × TokenKind :=
  match s.peek with
  | some '\\' =>
    let s := s.advance
    match s.peek with
    | some 'n' =>
      let s := s.advance
      match s.peek with
      | some '\'' => (s.advance, .charLit '\n')
      | _ => (s, .eof)
    | some 't' =>
      let s := s.advance
      match s.peek with
      | some '\'' => (s.advance, .charLit '\t')
      | _ => (s, .eof)
    | some 'r' =>
      let s := s.advance
      match s.peek with
      | some '\'' => (s.advance, .charLit (Char.ofNat 13))
      | _ => (s, .eof)
    | some '\\' =>
      let s := s.advance
      match s.peek with
      | some '\'' => (s.advance, .charLit '\\')
      | _ => (s, .eof)
    | some '\'' =>
      let s := s.advance
      match s.peek with
      | some '\'' => (s.advance, .charLit '\'')
      | _ => (s, .eof)
    | some '0' =>
      let s := s.advance
      match s.peek with
      | some '\'' => (s.advance, .charLit (Char.ofNat 0))
      | _ => (s, .eof)
    | _ => (s, .eof)
  | some c =>
    let s := s.advance
    match s.peek with
    | some '\'' => (s.advance, .charLit c)
    | _ => (s, .eof)
  | none => (s, .eof)

/-- Lex a label after the opening tick: 'name -/
partial def lexLabel (s : LexerState) : LexerState × TokenKind :=
  match s.peek with
  | some c =>
    if isIdentStart c then lexLabelLoop s.advance (String.singleton c)
    else (s, .eof)
  | none => (s, .eof)
where
  lexLabelLoop (s : LexerState) (acc : String) : LexerState × TokenKind :=
    match s.peek with
    | some c =>
      if isIdentCont c then lexLabelLoop s.advance (acc.push c)
      else (s, .label acc)
    | none => (s, .label acc)

/-- Lex a single token. -/
partial def lexToken (s : LexerState) : LexerState × TokenKind :=
  let s := skipWhitespace s
  if s.atEnd then (s, .eof)
  else
    match s.peek with
    | some c =>
      if isIdentStart c then lexIdentLoop s.advance (String.singleton c)
      else if c.isDigit then lexNumberLoop s.advance (c.toNat - '0'.toNat)
      else if c == '"' then lexStringLoop s.advance ""
      else if c == '\'' then
        -- Disambiguate: 'c' is char literal, 'ident is a label
        match s.peekAt 1 with
        | some c1 =>
          if isIdentStart c1 then
            match s.peekAt 2 with
            | some '\'' => lexCharLit s.advance  -- 'c' is a char literal
            | _ => lexLabel s.advance            -- 'ident is a label
          else lexCharLit s.advance
        | none => lexCharLit s.advance
      else
        let s := s.advance
        match c with
        | '+' => (s, .plus)
        | '*' => (s, .star)
        | '/' => (s, .slash)
        | '%' => (s, .percent)
        | '(' => (s, .lparen)
        | ')' => (s, .rparen)
        | '{' => (s, .lbrace)
        | '}' => (s, .rbrace)
        | '[' => (s, .lbracket)
        | ']' => (s, .rbracket)
        | ',' => (s, .comma)
        | ':' =>
          match s.peek with
          | some ':' => (s.advance, .doubleColon)
          | _ => (s, .colon)
        | ';' => (s, .semicolon)
        | '.' => (s, .dot)
        | '-' =>
          match s.peek with
          | some '>' => (s.advance, .arrow)
          | _ => (s, .minus)
        | '#' => (s, .hash)
        | '?' => (s, .question)
        | '=' =>
          match s.peek with
          | some '=' => (s.advance, .eq)
          | some '>' => (s.advance, .fatArrow)
          | _ => (s, .assign)
        | '!' =>
          match s.peek with
          | some '=' => (s.advance, .neq)
          | _ => (s, .not_)
        | '<' =>
          match s.peek with
          | some '<' => (s.advance, .shl)
          | some '=' => (s.advance, .leq)
          | _ => (s, .lt)
        | '>' =>
          match s.peek with
          | some '>' => (s.advance, .shr)
          | some '=' => (s.advance, .geq)
          | _ => (s, .gt)
        | '&' =>
          match s.peek with
          | some '&' => (s.advance, .and_)
          | _ => (s, .ampersand)
        | '|' =>
          match s.peek with
          | some '|' => (s.advance, .or_)
          | _ => (s, .pipe)
        | '^' => (s, .caret)
        | '~' => (s, .tilde)
        | _ => (s, .eof)
    | none => (s, .eof)

/-- Tokenize entire source string. -/
partial def tokenize (source : String) : List Token :=
  go (LexerState.init source) []
where
  go (s : LexerState) (acc : List Token) : List Token :=
    let sp := (skipWhitespace s).span
    let (s', kind) := lexToken s
    let tok : Token := { kind, span := sp }
    match kind with
    | .eof => acc ++ [tok]
    | _ => go s' (acc ++ [tok])

end Concrete
