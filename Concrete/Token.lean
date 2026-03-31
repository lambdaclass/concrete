namespace Concrete

inductive TokenKind where
  -- Literals
  | intLit (val : Int)
  | floatLit (val : Float)
  | boolLit (val : Bool)
  | strLit (val : String)
  | charLit (val : Char)
  -- Identifier
  | ident (name : String)
  -- Keywords
  | fn | «let» | mut | if_ | else_ | while_ | for_ | return_
  | true_ | false_ | «mod» | struct_ | enum_ | match_ | pub_ | import_
  | as_ | impl_ | trait_ | const_ | type_ | extern_ | with_ | cap_ | break_ | continue_
  | defer_ | borrow_ | in_ | newtype_ | trusted_
  | label (name : String)  -- 'outer (labeled loop)
  -- Types
  | arrow  -- ->
  -- Operators
  | plus | minus | star | slash | percent
  | eq | neq | lt | gt | leq | geq
  | and_ | or_ | not_
  | ampersand  -- &
  | pipe       -- |
  | caret      -- ^
  | tilde      -- ~
  | shl        -- <<
  | shr        -- >>
  | assign  -- =
  -- Delimiters
  | lparen | rparen | lbrace | rbrace | lbracket | rbracket
  | comma | colon | semicolon | dot
  | hash      -- #
  | fatArrow  -- =>
  | doubleColon  -- ::
  | question     -- ?
  -- Special
  | eof
  deriving Repr, BEq, Inhabited

structure Span where
  line : Nat
  col : Nat
  endLine : Nat := 0
  endCol : Nat := 0
  deriving Repr, Inhabited

structure Token where
  kind : TokenKind
  span : Span
  deriving Repr, Inhabited

def TokenKind.toString : TokenKind → String
  | .intLit v => s!"int({v})"
  | .floatLit v => s!"float({v})"
  | .boolLit v => s!"bool({v})"
  | .strLit v => s!"str(\"{v}\")"
  | .charLit v => s!"char('{v}')"
  | .ident n => s!"ident({n})"
  | .fn => "fn"
  | .«let» => "let"
  | .mut => "mut"
  | .if_ => "if"
  | .else_ => "else"
  | .while_ => "while"
  | .for_ => "for"
  | .return_ => "return"
  | .true_ => "true"
  | .false_ => "false"
  | .«mod» => "mod"
  | .struct_ => "struct"
  | .enum_ => "enum"
  | .match_ => "match"
  | .pub_ => "pub"
  | .import_ => "import"
  | .as_ => "as"
  | .impl_ => "impl"
  | .trait_ => "trait"
  | .const_ => "const"
  | .type_ => "type"
  | .extern_ => "extern"
  | .with_ => "with"
  | .cap_ => "cap"
  | .break_ => "break"
  | .continue_ => "continue"
  | .defer_ => "defer"
  | .borrow_ => "borrow"
  | .in_ => "in"
  | .newtype_ => "newtype"
  | .trusted_ => "trusted"
  | .label n => s!"'{n}"
  | .arrow => "->"
  | .plus => "+"
  | .minus => "-"
  | .star => "*"
  | .slash => "/"
  | .percent => "%"
  | .eq => "=="
  | .neq => "!="
  | .lt => "<"
  | .gt => ">"
  | .leq => "<="
  | .geq => ">="
  | .and_ => "&&"
  | .or_ => "||"
  | .not_ => "!"
  | .ampersand => "&"
  | .pipe => "|"
  | .caret => "^"
  | .tilde => "~"
  | .shl => "<<"
  | .shr => ">>"
  | .assign => "="
  | .lparen => "("
  | .rparen => ")"
  | .lbrace => "{"
  | .rbrace => "}"
  | .lbracket => "["
  | .rbracket => "]"
  | .comma => ","
  | .colon => ":"
  | .semicolon => ";"
  | .dot => "."
  | .hash => "#"
  | .fatArrow => "=>"
  | .doubleColon => "::"
  | .question => "?"
  | .eof => "<eof>"

instance : ToString TokenKind := ⟨TokenKind.toString⟩

end Concrete
