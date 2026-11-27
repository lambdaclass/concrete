namespace ConcreteSpec.Surface0

/-- The four integer bases. Only lowercase `0b`, `0o` and `0x` introduce non-decimal digits. -/
inductive Radix where
  | decimal | binary | octal | hexadecimal
  deriving DecidableEq, Repr

namespace Radix

/-- The positional base. -/
def base : Radix → Nat
  | .decimal => 10 | .binary => 2 | .octal => 8 | .hexadecimal => 16

/-- The exact lowercase basePrefix; decimal has none. -/
def basePrefix : Radix → List Nat
  | .decimal => [] | .binary => [48, 98] | .octal => [48, 111] | .hexadecimal => [48, 120]

end Radix

inductive Terminal where
  | fn | let_ | return_ | ident | integer | kept | u8
  | lparen | rparen | lbrace | rbrace | arrow | colon | equal | semi | plus
  | minus | star | slash | percent | stringLiteral | byteLiteral | label
  | mut_ | if_ | else_ | struct_ | move_
  | i8 | i16 | i32 | i64 | u16 | u32 | u64 | bool_ | Unit_ | char_
  | spec_ | by_ | Self | while_ | Entropy | from_ | until | cap | type_ | ghost | Copy
  | Fs | Network | Env | Time | Alloc | Device | Console | Process | Unsafe
  | true_ | false_ | enum_ | match_ | trait_ | impl_ | defer_ | assert_ | assume_ | with_
  | for_ | break_ | continue_ | pub_ | const_ | mod_ | import_ | borrow_ | exact_ | trusted_ | extern_
  | wildcard
  deriving DecidableEq, Repr

def Terminal.count := Terminal.wildcard.ctorIdx + 1

theorem Terminal.index_lt_count (kind : Terminal) : kind.ctorIdx < count := by
  cases kind <;> decide

theorem Terminal.index_ofNat : ∀ i : Fin count, (Terminal.ofNat i.val).ctorIdx = i.val := by
  decide

/-- The first consuming slice of a terminal. The ballot reserves the entire word
vocabulary even in files targeting slice zero. -/
def terminalFirstSlice : Terminal → Nat
  | .fn | .let_ | .return_ | .ident | .integer | .u8 | .lparen | .rparen
  | .lbrace | .rbrace | .arrow | .colon | .equal | .semi | .plus => 0
  | .struct_ | .move_ | .wildcard => 1
  | .i8 | .i16 | .i32 | .i64 | .u16 | .u32 | .u64 | .type_
  | .minus | .star | .slash | .percent | .byteLiteral => 2
  | .if_ | .else_ | .mut_ | .true_ | .false_ | .bool_ | .Unit_ => 3
  | .exact_ | .while_ | .from_ | .until | .for_ | .break_ | .continue_ | .label => 4
  | .Copy | .enum_ | .match_ => 6
  | .borrow_ => 7
  | .pub_ | .const_ | .mod_ | .import_ => 8
  | .trait_ | .impl_ | .defer_ | .Self | .cap | .Fs | .Network | .Env
  | .Time | .Alloc | .Device | .Console | .Process | .Unsafe | .Entropy => 9
  | .spec_ | .ghost | .assert_ | .assume_ | .by_ => 10
  | .with_ | .char_ | .stringLiteral => 11
  | .trusted_ => 16
  | .extern_ | .kept => 17

/-- First type production for each primitive keyword. -/
def Terminal.primitiveSlice (kind : Terminal) : Option Nat :=
  match kind with
  | .u8 | .i8 | .i16 | .i32 | .i64 | .u16 | .u32 | .u64 | .bool_ | .Unit_ | .char_ =>
    some (terminalFirstSlice kind)
  | _ => none

/-- The expected-type rule handles decimal suffixes after parsing. -/
def Radix.firstSlice : Radix → Nat
  | .decimal => 0
  | .binary | .octal | .hexadecimal => 2

/-- Fixed word tokens at the pre-slice-2 lock. The lexer looks up a spelling and the
draft grammar renders the resulting terminal from this same catalog.
Decision: docs/decisions.md#decided-2026-11-27-token-ballot. -/
def wordTokens : List (String × Terminal) :=
  [("fn", .fn), ("let", .let_), ("return", .return_), ("u8", .u8), ("struct", .struct_),
   ("move", .move_), ("if", .if_), ("else", .else_), ("mut", .mut_), ("kept", .kept),
   ("i8", .i8), ("i16", .i16), ("i32", .i32), ("i64", .i64), ("u16", .u16), ("u32", .u32),
   ("u64", .u64), ("bool", .bool_), ("Unit", .Unit_), ("char", .char_), ("spec", .spec_),
   ("by", .by_), ("Self", .Self), ("while", .while_), ("Entropy", .Entropy), ("from", .from_),
   ("until", .until), ("cap", .cap), ("type", .type_), ("ghost", .ghost), ("Copy", .Copy),
   ("Fs", .Fs), ("Network", .Network), ("Env", .Env), ("Time", .Time), ("Alloc", .Alloc),
   ("Device", .Device), ("Console", .Console), ("Process", .Process), ("Unsafe", .Unsafe),
   ("true", .true_), ("false", .false_), ("enum", .enum_), ("match", .match_),
   ("trait", .trait_), ("impl", .impl_), ("defer", .defer_), ("assert", .assert_),
   ("assume", .assume_), ("with", .with_), ("for", .for_), ("break", .break_),
   ("continue", .continue_), ("pub", .pub_), ("const", .const_), ("mod", .mod_),
   ("import", .import_), ("borrow", .borrow_), ("exact", .exact_), ("trusted", .trusted_),
   ("extern", .extern_), ("_", .wildcard)]

def word (name : String) : Terminal :=
  ((wordTokens.find? (fun entry => entry.1 == name)).map Prod.snd).getD .ident

def punctuation : List (String × Terminal) :=
  [("(", .lparen), (")", .rparen), (r#"{"#, .lbrace), (r#"}"#, .rbrace), ("->", .arrow),
   (":", .colon), ("=", .equal), (";", .semi), ("+", .plus),
   ("-", .minus), ("*", .star), ("/", .slash), ("%", .percent)]

end ConcreteSpec.Surface0
