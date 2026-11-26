import ConcreteSpec.Spec.Scalar

namespace Concrete.Backend.LLVM

/-- Render an unsigned LLVM integer token in canonical decimal. -/
def decimal (n : Nat) : String := n.repr

open ConcreteSpec.Core (IntTy)

/-- The numeric value recoverable from an LLVM integer token. -/
structure Immediate where
  value : Nat
  deriving BEq

def Immediate.natural (n : Nat) : Immediate := ⟨n⟩

def Immediate.token (v : Immediate) : String := decimal v.value

inductive Arithmetic where
  | add | sub | mul
  deriving BEq

inductive Conversion where
  | trunc | sext | zext
  deriving BEq

inductive Comparison where
  | eq | sle | ult
  deriving BEq

/-- LLVM integer types carry a width. Signedness belongs to operations. -/
inductive Width where
  | u8 | u16 | u32 | u64
  deriving DecidableEq

def Width.ofCore : IntTy → Width
  | .u8 | .i8 => .u8
  | .u16 | .i16 => .u16
  | .u32 | .i32 => .u32
  | .u64 | .i64 => .u64

def Width.bits : Width → Nat
  | .u8 => 8
  | .u16 => 16
  | .u32 => 32
  | .u64 => 64

inductive TypeCode where
  | integer (ty : Width)
  | flag | ptr | void
  | overflowPair (ty : Width)
  deriving BEq

inductive Global where
  | body | main | writeAll | write | exit | abort | bad
  | site (index : Nat)
  | overflow (op : Arithmetic) (ty : IntTy)
  deriving BEq

inductive Label where
  | block (index : Nat)
  | trapSite (index : Nat)
  | entry | loop | out | wr | more | exit | trap | bad
  deriving BEq, DecidableEq, Repr

inductive Local where
  | register (index : Nat)
  | overflow (index : Nat)
  | result (block : Nat)
  | p | n | pp | np | nn | rest | done | k | bad | w | ok | i
  deriving BEq

inductive Argument where
  | local (name : Local)
  | global (name : Global)
  | immediate (value : Immediate)
  deriving BEq

abbrev TypedArgument := TypeCode × Argument

inductive Instruction where
  | bin (dst : Local) (op : Arithmetic) (ty : TypeCode) (lhs rhs : Argument)
  | convert (dst : Local) (op : Conversion) (source : TypedArgument) (target : TypeCode)
  | call (dst : Option Local) (result : TypeCode) (fn : Global) (args : List TypedArgument)
  | extract (dst : Local) (ty : TypeCode) (aggregate : Local) (index : Nat)
  | phi (dst : Local) (ty : TypeCode) (arms : List (Argument × Label))
  | compare (dst : Local) (op : Comparison) (ty : TypeCode) (lhs rhs : Argument)
  | gep (dst : Local) (element : TypeCode) (base offset : TypedArgument)
  deriving BEq

inductive Terminator where
  | ret (arg : TypedArgument)
  | retVoid
  | branch (target : Label)
  | cond (flag : Local) (yes no : Label)
  | switch (ty : Width) (value : Local) (fallback : Label) (cases : List (Immediate × Label))
  | unreachable
  deriving BEq

/-- The LLVM body and runtime share these assembly items. The only arbitrary byte payload
is byte data, accepted solely in a constant global initializer. -/
inductive Item where
  | declare (result : TypeCode) (fn : Global) (args : List TypeCode)
  | define (internal : Bool) (result : TypeCode) (fn : Global) (args : List (TypeCode × Local))
  | constant (name : Global) (bytes : ByteArray)
  | label (name : Label)
  | instruction (inst : Instruction)
  | terminator (term : Terminator)
  | endFunction
  | blank
  deriving BEq

def TypeCode.integerWidth : TypeCode → Option Nat
  | .integer w => some w.bits
  | .flag => some 1
  | _ => none

/-- Punctuation on the right of a numeric operand. The left delimiter is
part of the same rendering piece. -/
inductive Opening where
  | space | bracket

def Opening.char : Opening → Char
  | .space => ' '
  | .bracket => '['

inductive Separator where
  | space | comma | closeParen | newline

def Separator.char : Separator → Char
  | .space => ' '
  | .comma => ','
  | .closeParen => ')'
  | .newline => '\n'

inductive NumberPrefix where
  | integer | site | block | trapSite | register | overflow | result

private def NumberPrefix.text : NumberPrefix → String
  | .integer => "i"
  | .site => "@.site"
  | .block => "b"
  | .trapSite => "t"
  | .register => "%r"
  | .overflow => "%o"
  | .result => "%ret"

abbrev SyntaxFragment :=
  {s : String // s.toList.all (fun c => !c.isDigit && c != '"' && c != ';') = true}

/-- Internal rendering pieces. An immediate owns both delimiters. Digits in
names, integer widths, and escaped bytes have their own grammar positions. -/
inductive Piece where
  | fragment (text : SyntaxFragment)
  | punctuation (after : Separator)
  | numbered (kind : NumberPrefix) (value : Nat)
  | bytes (value : ByteArray)
  | functionEnd (fn : Global)
  | immediate (value : Immediate) (before : Opening) (after : Separator)

abbrev Doc := List Piece

private def numbered (p : NumberPrefix) (n : Nat) : Doc := [.numbered p n]
private def imm (v : Immediate) (after : Separator) : Doc := [.immediate v .space after]

private def text (s : String)
    (valid : s.toList.all (fun c => !c.isDigit && c != '"' && c != ';') = true := by decide) : Doc :=
  [.fragment ⟨s, valid⟩]

private def arithmetic : Arithmetic → Doc
  | .add => text "add"
  | .sub => text "sub"
  | .mul => text "mul"

private def conversion : Conversion → Doc
  | .trunc => text "trunc"
  | .sext => text "sext"
  | .zext => text "zext"

private def comparison : Comparison → Doc
  | .eq => text "eq"
  | .sle => text "sle"
  | .ult => text "ult"

private def typeCode : TypeCode → Doc
  | .integer ty => numbered .integer ty.bits
  | .flag => numbered .integer 1
  | .ptr => text "ptr"
  | .void => text "void"
  | .overflowPair ty => text r"{" ++ numbered .integer ty.bits ++ text ", " ++ numbered .integer 1 ++ text "}"

private def global : Global → Doc
  | .body => text "@concrete_body"
  | .main => text "@main"
  | .writeAll => text "@concrete_rt_write_all"
  | .write => text "@write"
  | .exit => text "@_exit"
  | .abort => text "@abort"
  | .bad => text "@.bad"
  | .site n => numbered .site n
  | .overflow op ty =>
    text "@llvm." ++ (if ty.signed then text "s" else text "u") ++ arithmetic op ++
      text ".with.overflow." ++ typeCode (.integer (Width.ofCore ty))

private def labelName : Label → Doc
  | .block n => numbered .block n
  | .trapSite n => numbered .trapSite n
  | .entry => text "entry"
  | .loop => text "loop"
  | .out => text "out"
  | .wr => text "wr"
  | .more => text "more"
  | .exit => text "exit"
  | .trap => text "trap"
  | .bad => text "bad"

private def localName : Local → Doc
  | .register n => numbered .register n
  | .overflow n => numbered .overflow n
  | .result n => numbered .result n
  | .p => text "%p"
  | .n => text "%n"
  | .pp => text "%pp"
  | .np => text "%np"
  | .nn => text "%nn"
  | .rest => text "%rest"
  | .done => text "%done"
  | .k => text "%k"
  | .bad => text "%bad"
  | .w => text "%w"
  | .ok => text "%ok"
  | .i => text "%i"

private def framed (body : Doc) (after : Separator) : Doc :=
  text " " ++ body ++ [.punctuation after]

private def arg : Argument → Separator → Doc
  | .local name, after => framed (localName name) after
  | .global name, after => framed (global name) after
  | .immediate value, after => imm value after

private def typed (a : TypedArgument) (after : Separator) : Doc :=
  typeCode a.1 ++ arg a.2 after

private def callArgs : List TypedArgument → Doc
  | [] => text ")"
  | [a] => typed a .closeParen
  | a :: b :: rest => typed a .comma ++ text " " ++ callArgs (b :: rest)

private def commaSeparated {α : Type} (f : α → Doc) (xs : List α) : Doc :=
  List.intercalate (text ", ") (xs.map f)

private def assign (dst : Local) : Doc := localName dst ++ text " = "

private def phiArmPieces (arm : Argument × Label) : Doc :=
  text "[" ++ arg arm.1 .comma ++ text " %" ++ labelName arm.2 ++ text " ]"

private def callPieces (result : TypeCode) (fn : Global) (args : List TypedArgument) : Doc :=
  text "call " ++ typeCode result ++ text " " ++ global fn ++ text "(" ++ callArgs args ++ text "\n"

private def instructionPieces : Instruction → Doc
  | .bin dst op ty lhs rhs =>
    assign dst ++ (arithmetic op ++ text " " ++ typeCode ty ++ arg lhs .comma ++ arg rhs .newline)
  | .convert dst op source target =>
    assign dst ++ (conversion op ++ text " " ++ typed source .space ++ text "to " ++ typeCode target ++ text "\n")
  | .call dst result fn args => dst.toList.flatMap assign ++ callPieces result fn args
  | .extract dst ty aggregate index =>
    assign dst ++ (text "extractvalue " ++ typeCode ty ++ arg (.local aggregate) .comma ++
      imm (.natural index) .newline)
  | .phi dst ty arms =>
    assign dst ++ (text "phi " ++ typeCode ty ++ text " " ++
      commaSeparated phiArmPieces arms ++ text "\n")
  | .compare dst op ty lhs rhs =>
    assign dst ++ (text "icmp " ++ comparison op ++ text " " ++ typeCode ty ++ arg lhs .comma ++ arg rhs .newline)
  | .gep dst element base offset =>
    assign dst ++ (text "getelementptr " ++ typeCode element ++ text ", " ++ typed base .comma ++
      text " " ++ typed offset .newline)

private def switchCasePieces (ty : Width) (entry : Immediate × Label) : Doc :=
  typed (.integer ty, .immediate entry.1) .comma ++ text " label %" ++ labelName entry.2

private def terminatorPieces : Terminator → Doc
  | .ret value => text "ret " ++ typed value .newline
  | .retVoid => text "ret void\n"
  | .branch target => text "br label %" ++ labelName target ++ text "\n"
  | .cond flag yes no =>
    text "br " ++ typeCode .flag ++ arg (.local flag) .comma ++ text " label %" ++ labelName yes ++
      text ", label %" ++ labelName no ++ text "\n"
  | .switch ty value fallback cases =>
    text "switch " ++ typeCode (.integer ty) ++ arg (.local value) .comma ++ text " label %" ++ labelName fallback ++ text " [" ++
      (cases.flatMap fun entry => text " " ++ switchCasePieces ty entry) ++ text " ]\n"
  | .unreachable => text "unreachable\n"

-- The stack bound is derived from direct control flow, including dense trap dispatch.
private def defineEndTail : Global → String
  | .main => r#" "no-jump-tables"="true" {"# ++ "\n"
  | _ => r" {" ++ "\n"

private def defineEnd (fn : Global) : String := ")" ++ defineEndTail fn

private def parameterPieces (a : TypeCode × Local) : Doc :=
  typeCode a.1 ++ text " " ++ localName a.2

def Item.pieces : Item → Doc
  | .declare result fn args =>
    text "declare " ++ typeCode result ++ text " " ++ global fn ++ text "(" ++
      commaSeparated typeCode args ++ text ")\n"
  | .define internal result fn args =>
    text "define " ++ (if internal then text "internal " else []) ++ typeCode result ++ text " " ++ global fn ++ text "(" ++
      commaSeparated parameterPieces args ++ [.functionEnd fn]
  | .constant name bytes =>
    global name ++ text " = private constant " ++ [.immediate (.natural bytes.size) .bracket .space] ++
      text "x " ++ typeCode (.integer .u8) ++ text "] " ++ [.bytes bytes] ++ text "\n"
  | .label name => labelName name ++ text ":\n"
  | .instruction inst => text "  " ++ instructionPieces inst
  | .terminator term => text "  " ++ terminatorPieces term
  | .endFunction => text "}\n"
  | .blank => text "\n"

private def byteText (b : UInt8) : String :=
  let n := b.toNat
  if 32 ≤ n ∧ n < 127 ∧ n ≠ 34 ∧ n ≠ 92 then (Char.ofNat n).toString
  else
    let hex := Nat.toDigits 16 n
    "\\" ++ String.ofList ((if hex.length < 2 then ['0'] else []) ++ hex.map Char.toUpper)

/-- Encode a byte global inside a quoted LLVM data literal. -/
private def byteString (bytes : ByteArray) : String :=
  "c\"" ++ String.join (bytes.data.toList.map byteText) ++ "\""

def Piece.render : Piece → String
  | .fragment text => text.val
  | .punctuation after => String.singleton after.char
  | .numbered p n => p.text ++ decimal n
  | .bytes data => byteString data
  | .functionEnd fn => defineEnd fn
  | .immediate v before after => String.singleton before.char ++ v.token ++ String.singleton after.char

def renderPieces (pieces : List Piece) : String := String.join (pieces.map Piece.render)

/-- The sole text renderer for the LLVM instruction and operand structure. -/
def render (items : List Item) : String := renderPieces (items.flatMap Item.pieces)

end Concrete.Backend.LLVM
