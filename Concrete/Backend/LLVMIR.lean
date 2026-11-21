import Concrete.Backend.Decimal
import ConcreteSpec.Spec.Scalar

namespace Concrete.Backend.LLVM

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

/-- Numeric positions in assembly order. Operand positions carry their LLVM type;
extract indices and byte lengths have no explicit integer type. -/
inductive IntegerRole where
  | left | right | conversionSource | callArgument (position : Nat) | extractIndex
  | phiArm (position : Nat) (predecessor : Label)
  | comparisonLeft | comparisonRight | gepBase | gepOffset | returnValue
  | switchKey (position : Nat) (destination : Label) | byteLength
  deriving DecidableEq, Repr

abbrev IntegerField := IntegerRole × Option Nat × Nat

def TypeCode.integerWidth : TypeCode → Option Nat
  | .integer w => some w.bits
  | .flag => some 1
  | _ => none

def Argument.inventory (role : IntegerRole) (ty : TypeCode) : Argument → List IntegerField
  | .immediate v => [(role, ty.integerWidth, v.value)]
  | _ => []

def Instruction.inventory : Instruction → List IntegerField
  | .bin _ _ ty a b => a.inventory .left ty ++ b.inventory .right ty
  | .convert _ _ (ty, a) _ => a.inventory .conversionSource ty
  | .call _ _ _ args => args.zipIdx.flatMap fun ((ty, a), index) => a.inventory (.callArgument index) ty
  | .extract _ _ _ i => [(.extractIndex, none, i)]
  | .phi _ ty arms => arms.zipIdx.flatMap fun ((a, pred), index) => a.inventory (.phiArm index pred) ty
  | .compare _ _ ty a b => a.inventory .comparisonLeft ty ++ b.inventory .comparisonRight ty
  | .gep _ _ (baseTy, base) (offsetTy, offset) =>
    base.inventory .gepBase baseTy ++ offset.inventory .gepOffset offsetTy

def Item.inventory : Item → List IntegerField
  | .instruction i => i.inventory
  | .terminator (.ret (ty, a)) => a.inventory .returnValue ty
  | .terminator (.switch w _ _ cases) => cases.zipIdx.map fun ((v, destination), position) =>
      (.switchKey position destination, some w.bits, v.value)
  | .constant _ data => [(.byteLength, none, data.size)]
  | _ => []

private def Argument.immediates : Argument → List Immediate
  | .immediate v => [v]
  | .local _ | .global _ => []

private def Instruction.immediates : Instruction → List Immediate
  | .bin _ _ _ lhs rhs | .compare _ _ _ lhs rhs => lhs.immediates ++ rhs.immediates
  | .convert _ _ source _ => source.2.immediates
  | .call _ _ _ args => args.flatMap fun a => a.2.immediates
  | .extract _ _ _ index => [.natural index]
  | .phi _ _ arms => arms.flatMap fun arm => arm.1.immediates
  | .gep _ _ base offset => base.2.immediates ++ offset.2.immediates

private def Terminator.immediates : Terminator → List Immediate
  | .ret value => value.2.immediates
  | .switch _ _ _ cases => cases.map Prod.fst
  | .retVoid | .branch _ | .cond _ _ _ | .unreachable => []

def Item.immediates : Item → List Immediate
  | .constant _ data => [.natural data.size]
  | .instruction inst => inst.immediates
  | .terminator term => term.immediates
  | .declare .. | .define .. | .label _ | .endFunction | .blank => []

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

private def instructionPieces : Instruction → Doc
  | .bin dst op ty lhs rhs =>
    assign dst ++ arithmetic op ++ text " " ++ typeCode ty ++ arg lhs .comma ++ arg rhs .newline
  | .convert dst op source target =>
    assign dst ++ conversion op ++ text " " ++ typed source .space ++ text "to " ++ typeCode target ++ text "\n"
  | .call dst result fn args =>
    (dst.toList.flatMap assign) ++ text "call " ++ typeCode result ++ text " " ++ global fn ++
      text "(" ++ callArgs args ++ text "\n"
  | .extract dst ty aggregate index =>
    assign dst ++ text "extractvalue " ++ typeCode ty ++ arg (.local aggregate) .comma ++
      imm (.natural index) .newline
  | .phi dst ty arms =>
    assign dst ++ text "phi " ++ typeCode ty ++ text " " ++
      commaSeparated (fun (value, pred) =>
        text "[" ++ arg value .comma ++ text " %" ++ labelName pred ++ text " ]") arms ++ text "\n"
  | .compare dst op ty lhs rhs =>
    assign dst ++ text "icmp " ++ comparison op ++ text " " ++ typeCode ty ++ arg lhs .comma ++ arg rhs .newline
  | .gep dst element base offset =>
    assign dst ++ text "getelementptr " ++ typeCode element ++ text ", " ++ typed base .comma ++ text " " ++ typed offset .newline

private def terminatorPieces : Terminator → Doc
  | .ret value => text "ret " ++ typed value .newline
  | .retVoid => text "ret void\n"
  | .branch target => text "br label %" ++ labelName target ++ text "\n"
  | .cond flag yes no =>
    text "br " ++ typeCode .flag ++ arg (.local flag) .comma ++ text " label %" ++ labelName yes ++
      text ", label %" ++ labelName no ++ text "\n"
  | .switch ty value fallback cases =>
    text "switch " ++ typeCode (.integer ty) ++ arg (.local value) .comma ++ text " label %" ++ labelName fallback ++ text " [" ++
      (cases.flatMap fun (v, dst) => text " " ++ typed (.integer ty, .immediate v) .comma ++ text " label %" ++ labelName dst) ++ text " ]\n"
  | .unreachable => text "unreachable\n"

def Item.pieces : Item → Doc
  | .declare result fn args =>
    text "declare " ++ typeCode result ++ text " " ++ global fn ++ text "(" ++
      commaSeparated typeCode args ++ text ")\n"
  | .define internal result fn args =>
    text "define " ++ (if internal then text "internal " else []) ++ typeCode result ++ text " " ++ global fn ++ text "(" ++
      commaSeparated (fun (ty, name) => typeCode ty ++ text " " ++ localName name) args ++ text (r") {" ++ "\n")
  | .constant name bytes =>
    global name ++ text " = private constant " ++ [.immediate (.natural bytes.size) .bracket .space] ++
      text "x " ++ typeCode (.integer .u8) ++ text "] " ++ [.bytes bytes] ++ text "\n"
  | .label name => labelName name ++ text ":\n"
  | .instruction inst => text "  " ++ instructionPieces inst
  | .terminator term => text "  " ++ terminatorPieces term
  | .endFunction => text "}\n"
  | .blank => text "\n"

def Piece.immediate? : Piece → Option Immediate
  | .immediate v _ _ => some v
  | .fragment _ | .punctuation _ | .numbered .. | .bytes _ => none

private theorem immediates_typeCode (ty : TypeCode) :
    (typeCode ty).filterMap Piece.immediate? = [] := by
  cases ty <;> rfl

private theorem immediates_arithmetic (op : Arithmetic) :
    (arithmetic op).filterMap Piece.immediate? = [] := by
  cases op <;> rfl

private theorem immediates_conversion (op : Conversion) :
    (conversion op).filterMap Piece.immediate? = [] := by
  cases op <;> rfl

private theorem immediates_comparison (op : Comparison) :
    (comparison op).filterMap Piece.immediate? = [] := by
  cases op <;> rfl

private theorem immediates_global (name : Global) :
    (global name).filterMap Piece.immediate? = [] := by
  cases name <;> try rfl
  rename_i op ty
  cases op <;> cases ty <;> rfl

private theorem immediates_local (name : Local) :
    (localName name).filterMap Piece.immediate? = [] := by
  cases name <;> rfl

private theorem immediates_label (name : Label) :
    (labelName name).filterMap Piece.immediate? = [] := by
  cases name <;> rfl

attribute [local simp] List.filterMap_cons List.filterMap_append List.filterMap_flatMap
attribute [local simp] text Piece.immediate? immediates_typeCode immediates_arithmetic
  immediates_conversion immediates_comparison immediates_global immediates_local immediates_label

private theorem immediates_arg (a : Argument) (sep : Separator) :
    (arg a sep).filterMap Piece.immediate? = a.immediates := by
  cases a <;> simp [arg, Argument.immediates, framed, imm]

private theorem immediates_typed (a : TypedArgument) (sep : Separator) :
    (typed a sep).filterMap Piece.immediate? = a.2.immediates := by
  simp [typed, immediates_arg]

private theorem immediates_callArgs (args : List TypedArgument) :
    (callArgs args).filterMap Piece.immediate? = args.flatMap (fun a => a.2.immediates) := by
  match args with
  | [] => rfl
  | [_] => simp [callArgs, immediates_typed]
  | a :: b :: rest => simp [callArgs, immediates_typed, immediates_callArgs (b :: rest)]

private theorem immediates_commaSeparated {α : Type} (args : List α) (f : α → Doc) :
    (commaSeparated f args).filterMap Piece.immediate? =
      args.flatMap (fun a => (f a).filterMap Piece.immediate?) := by
  match args with
  | [] => rfl
  | [_] => simp [commaSeparated]
  | a :: b :: rest =>
    have tail := immediates_commaSeparated (b :: rest) f
    simp only [commaSeparated, text, List.map_cons, List.flatMap_cons] at tail
    simp [commaSeparated, List.intercalate_cons_cons, tail]

attribute [local simp] immediates_arg immediates_typed immediates_callArgs immediates_commaSeparated

private theorem immediates_instruction (inst : Instruction) :
    (instructionPieces inst).filterMap Piece.immediate? = inst.immediates := by
  cases inst <;> simp [instructionPieces, Instruction.immediates, assign, imm, Argument.immediates]

private theorem immediates_terminator (term : Terminator) :
    (terminatorPieces term).filterMap Piece.immediate? = term.immediates := by
  cases term <;> simp [terminatorPieces, Terminator.immediates, Argument.immediates]
  case switch ty value fallback cases =>
    induction cases with
    | nil => rfl
    | cons head tail ih => simp [ih]

private theorem immediates_item (item : Item) :
    item.pieces.filterMap Piece.immediate? = item.immediates := by
  cases item <;> simp [Item.pieces, Item.immediates, immediates_instruction, immediates_terminator]

/-- Law. Rendering visits every immediate field exactly once, in assembly order.
Authority: docs/decisions.md#od126. -/
theorem render_immediate_fields (items : List Item) :
    (items.flatMap Item.pieces).filterMap Piece.immediate? = items.flatMap Item.immediates := by
  induction items with
  | nil => rfl
  | cons item items ih => simp [immediates_item, ih]

/-- Encode a byte global inside a quoted LLVM data literal. -/
private def byteString (bytes : ByteArray) : String :=
  "c\"" ++ String.join (bytes.toList.map fun b =>
    let n := b.toNat
    if 32 ≤ n ∧ n < 127 ∧ n ≠ 34 ∧ n ≠ 92 then (Char.ofNat n).toString
    else
      let hex := Nat.toDigits 16 n
      "\\" ++ String.ofList ((if hex.length < 2 then ['0'] else []) ++ hex.map Char.toUpper)) ++ "\""

def Piece.render : Piece → String
  | .fragment text => text.val
  | .punctuation after => String.singleton after.char
  | .numbered p n => p.text ++ decimal n
  | .bytes data => byteString data
  | .immediate v before after => String.singleton before.char ++ v.token ++ String.singleton after.char

def renderPieces (pieces : List Piece) : String := String.join (pieces.map Piece.render)

/-- The sole text renderer for the LLVM instruction and operand structure. -/
def render (items : List Item) : String := renderPieces (items.flatMap Item.pieces)

/-- Character layout of the i8 additions used by the rendering controls. -/
theorem render_i8_add_register_chars (dst src right : Nat) :
    (render [.instruction (.bin (.register dst) .add (.integer .u8)
      (.local (.register src)) (.immediate (.natural right)))]).toList =
    "  %r".toList ++ (decimal dst).toList ++ " = add i8 %r".toList ++
      (decimal src).toList ++ ", ".toList ++ (decimal right).toList ++ ['\n'] := by
  simp [render, Item.pieces, instructionPieces, assign, localName, arithmetic, typeCode,
    renderPieces, Piece.render, arg, framed, imm, text, numbered, Opening.char,
    Separator.char, NumberPrefix.text, Immediate.token, Width.bits]
  rfl

/-- Character layout of an i8 addition with two immediate operands. -/
theorem render_i8_add_immediate_chars (dst left right : Nat) :
    (render [.instruction (.bin (.register dst) .add (.integer .u8)
      (.immediate (.natural left)) (.immediate (.natural right)))]).toList =
    "  %r".toList ++ (decimal dst).toList ++ " = add i8 ".toList ++
      (decimal left).toList ++ ", ".toList ++ (decimal right).toList ++ ['\n'] := by
  simp [render, Item.pieces, instructionPieces, assign, localName, arithmetic, typeCode,
    renderPieces, Piece.render, arg, imm, text, numbered, Opening.char,
    Separator.char, NumberPrefix.text, Immediate.token, Width.bits]
  rfl

/-- Law. At every immediate position in the renderer's assembly of a module,
the output contains one exact decimal token framed by nondigits. The prefix and
suffix are rendered from exactly the pieces before and after that position.
Authority: docs/decisions.md#od126. -/
theorem render_immediate (items : List Item) (before after : List Piece)
    (v : Immediate) (opening : Opening) (sep : Separator)
    (position : items.flatMap Item.pieces = before ++ .immediate v opening sep :: after) :
    ∃ token, DecimalToken token (Int.ofNat v.value) ∧
      opening.char.isDigit = false ∧ sep.char.isDigit = false ∧
      render items = renderPieces before ++ String.singleton opening.char ++ token ++
        String.singleton sep.char ++ renderPieces after := by
  refine ⟨decimal v.value, decimal_token _, ?_, ?_, ?_⟩
  · cases opening <;> rfl
  · cases sep <;> rfl
  · simp only [render, position, renderPieces, List.map_append, List.map_cons,
      String.join_append, String.join_cons, Piece.render, Immediate.token, String.append_assoc]


/-!
The assembly reader follows the instruction, terminator and lexical productions in
the [LLVM Language Reference](https://llvm.org/docs/LangRef.html), restricted to
the opcodes, type widths, identifiers and layout used by this backend. It reads
prefixed names, complete unsigned decimal tokens, and `c"..."` byte constants
with printable ASCII and two-digit hex escapes. A byte constant must match the
declared array length. This reader reconstructs the assembly syntax tree.
-/
namespace Assembly
variable {α β : Type}

private abbrev Parser (α : Type) := List Char → Option (α × List Char)

private instance : Monad Parser where
  pure a := fun input => some (a, input)
  map f p := fun input => (p input).map fun (a, rest) => (f a, rest)
  bind p f := fun input => do
    let (a, rest) ← p input
    f a rest

private def alt (p q : Parser α) : Parser α := fun input =>
  (p input).orElse fun _ => q input

private def consume : List Char → List Char → Option (List Char)
  | [], input => some input
  | c :: cs, d :: ds => if c = d then consume cs ds else none
  | _ :: _, [] => none

private def literal (s : String) : Parser Unit := fun input =>
  (consume s.toList input).map fun rest => ((), rest)

private def keyword (s : String) (value : α) : Parser α := do
  literal s
  pure value

private def choices (xs : List (String × α)) : Parser α :=
  xs.foldr (fun (spelling, value) rest => alt (keyword spelling value) rest) (fun _ => none)

private def symbolChar (c : Char) : Bool :=
  c.isAlphanum || c == '-' || c == '_' || c == '.' || c == '$'

private def word (decode : List Char → Option α) : Parser α := fun input => do
  let chars := input.takeWhile symbolChar
  let value ← decode chars
  pure (value, input.drop chars.length)

private def decodeUnsigned (chars : List Char) : Option Nat :=
  match parseDecimal (String.ofList chars) with
  | some (.ofNat n) => some n
  | _ => none

private def unsigned : Parser Nat := word decodeUnsigned

private def numberedName (stem : String) (make : Nat → α) : List Char → Option α :=
  fun chars => do
    let suffix ← consume stem.toList chars
    let (n, []) ← unsigned suffix | none
    pure (make n)

private def namedWord (fixed : List (String × α)) (numbered : List (String × (Nat → α)))
    (chars : List Char) : Option α :=
  ((fixed.find? fun (s, _) => s.toList == chars).map Prod.snd).orElse fun _ =>
    numbered.findSome? fun (stem, make) => numberedName stem make chars

private def named (fixed : List (String × α)) (numbered : List (String × (Nat → α))) :
    Parser α := word (namedWord fixed numbered)

private def localWord : List Char → Option Local
  | ['p'] => some .p
  | ['n'] => some .n
  | ['p', 'p'] => some .pp
  | ['n', 'p'] => some .np
  | ['n', 'n'] => some .nn
  | ['r', 'e', 's', 't'] => some .rest
  | ['d', 'o', 'n', 'e'] => some .done
  | ['k'] => some .k
  | ['b', 'a', 'd'] => some .bad
  | ['w'] => some .w
  | ['o', 'k'] => some .ok
  | ['i'] => some .i
  | 'r' :: 'e' :: 't' :: digits => (decodeUnsigned digits).map Local.result
  | 'r' :: digits => (decodeUnsigned digits).map Local.register
  | 'o' :: digits => (decodeUnsigned digits).map Local.overflow
  | _ => none

private def localName : Parser Local := do
  literal "%"
  word localWord

private def labelName : Parser Label :=
  named [("entry", .entry), ("loop", .loop), ("out", .out), ("wr", .wr),
    ("more", .more), ("exit", .exit), ("trap", .trap), ("bad", .bad)]
    [("b", .block), ("t", .trapSite)]

private def decodeWidth : List Char → Option Width
  | ['i', '8'] => some .u8
  | ['i', '1', '6'] => some .u16
  | ['i', '3', '2'] => some .u32
  | ['i', '6', '4'] => some .u64
  | _ => none

private def width : Parser Width := word decodeWidth

private def arithmetic : Parser Arithmetic := choices [("add", .add), ("sub", .sub), ("mul", .mul)]
private def conversion : Parser Conversion := choices [("trunc", .trunc), ("sext", .sext), ("zext", .zext)]
private def comparison : Parser Comparison := choices [("eq", .eq), ("sle", .sle), ("ult", .ult)]

private def intrinsicType : Width → Bool → IntTy
  | .u8, true => .i8
  | .u16, true => .i16
  | .u32, true => .i32
  | .u64, true => .i64
  | .u8, false => .u8
  | .u16, false => .u16
  | .u32, false => .u32
  | .u64, false => .u64

private def intrinsic : Parser Global := do
  literal "llvm."
  let signed ← choices [("s", true), ("u", false)]
  let op ← arithmetic
  literal ".with.overflow."
  let ty ← width
  pure (.overflow op (intrinsicType ty signed))

private def globalPlain : Parser Global :=
  named [("concrete_body", .body), ("main", .main),
    ("concrete_rt_write_all", .writeAll), ("write", .write), ("_exit", .exit),
    ("abort", .abort), (".bad", .bad)] [(".site", .site)]

private def globalName : Parser Global := do
  literal "@"
  alt intrinsic globalPlain

private def pairType : Parser TypeCode := do
  literal r"{"
  let ty ← width
  literal r", i1}"
  pure (.overflowPair ty)

private def atomicType : List Char → Option TypeCode
  | ['i', '1'] => some .flag
  | ['p', 't', 'r'] => some .ptr
  | ['v', 'o', 'i', 'd'] => some .void
  | chars => (decodeWidth chars).map TypeCode.integer

private def typeCode : Parser TypeCode
  | input@('{' :: _) => pairType input
  | input => word atomicType input

private def argument : Parser Argument :=
  alt (Argument.local <$> localName) <|
  alt (Argument.global <$> globalName) <|
  (fun n => Argument.immediate (.natural n)) <$> unsigned

private def typed : Parser TypedArgument := do
  let ty ← typeCode
  literal " "
  let arg ← argument
  pure (ty, arg)

private def itemsUntil (entry : Parser α) (sep ending : String) :
    Nat → Parser (List α)
  | 0 => fun _ => none
  | fuel + 1 => do
    let first ← entry
    alt (keyword ending [first]) <| do
      literal sep
      let rest ← itemsUntil entry sep ending fuel
      pure (first :: rest)

private def series (entry : Parser α) (sep ending : String) : Parser (List α) :=
  fun input => (alt (keyword ending []) (itemsUntil entry sep ending (input.length + 1))) input

private def call (dst : Option Local) : Parser Instruction := do
  literal "call "
  let ty ← typeCode
  literal " "
  let fn ← globalName
  literal "("
  let args ← series typed ", " ")\n"
  pure (.call dst ty fn args)

private def binary (dst : Local) (opcode : Parser α)
    (make : Local → α → TypeCode → Argument → Argument → Instruction) : Parser Instruction := do
  let op ← opcode
  literal " "
  let (ty, left) ← typed
  literal ", "
  let right ← argument
  literal "\n"
  pure (make dst op ty left right)

private def convert (dst : Local) : Parser Instruction := do
  let op ← conversion
  literal " "
  let source ← typed
  literal " to "
  let target ← typeCode
  literal "\n"
  pure (.convert dst op source target)

private def extract (dst : Local) : Parser Instruction := do
  literal "extractvalue "
  let ty ← typeCode
  literal " "
  let aggregate ← localName
  literal ", "
  let index ← unsigned
  literal "\n"
  pure (.extract dst ty aggregate index)

private def phi (dst : Local) : Parser Instruction := do
  literal "phi "
  let ty ← typeCode
  literal " "
  let arms ← series (do
    literal "[ "
    let value ← argument
    literal ", %"
    let pred ← labelName
    literal " ]"
    pure (value, pred)) ", " "\n"
  pure (.phi dst ty arms)

private def compare (dst : Local) : Parser Instruction := do
  literal "icmp "
  binary dst comparison Instruction.compare

private def gep (dst : Local) : Parser Instruction := do
  literal "getelementptr "
  let ty ← typeCode
  literal ", "
  let base ← typed
  literal ", "
  let offset ← typed
  literal "\n"
  pure (.gep dst ty base offset)

private def instruction : Parser Instruction :=
  alt (call none) <| do
    let dst ← localName
    literal " = "
    alt (binary dst arithmetic Instruction.bin) <|
    alt (convert dst) <| alt (call (some dst)) <| alt (extract dst) <|
    alt (phi dst) <| alt (compare dst) (gep dst)

private def labelRef : Parser Label := do
  literal "label %"
  labelName

private def branch : Parser Terminator := do
  literal "br "
  alt (do
    let target ← labelRef
    literal "\n"
    pure (.branch target)) <| do
    literal "i1 "
    let flag ← localName
    literal ", "
    let yes ← labelRef
    literal ", "
    let no ← labelRef
    literal "\n"
    pure (.cond flag yes no)

private def switchCase (ty : Width) : Parser (Immediate × Label) := do
  let (caseTy, value) ← typed
  -- The emitted switch uses only scalar integer widths.
  let .immediate value ← pure value | fun _ => none
  let .integer caseTy ← pure caseTy | fun _ => none
  if caseTy != ty then fun _ => none else
    literal ", " *> ((fun dest => (value, dest)) <$> labelRef)

private def switch : Parser Terminator := do
  literal "switch "
  let ty ← width
  literal " "
  let value ← localName
  literal ", "
  let fallback ← labelRef
  literal " ["
  let cases ← alt (keyword " ]\n" []) (do
    literal " "
    fun input => itemsUntil (switchCase ty) " " " ]\n" (input.length + 1) input)
  pure (.switch ty value fallback cases)

private def returnValue : Parser Terminator := do
  let value ← typed
  literal "\n"
  pure (.ret value)

private def terminator : Parser Terminator
  | 'r' :: 'e' :: 't' :: ' ' :: input =>
    (alt (keyword "void\n" .retVoid) returnValue) input
  | input@('b' :: _) => branch input
  | input@('s' :: _) => switch input
  | input => keyword "unreachable\n" .unreachable input

private def hex (c : Char) : Option Nat :=
  if c.isDigit then some (c.toNat - '0'.toNat)
  else if 'A' ≤ c ∧ c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else none

private def quotedBytes : Parser (List UInt8)
  | '"' :: rest => some ([], rest)
  | '\\' :: hi :: lo :: rest => do
    let hi ← hex hi
    let lo ← hex lo
    let (bytes, rest) ← quotedBytes rest
    pure (UInt8.ofNat (16 * hi + lo) :: bytes, rest)
  | c :: rest =>
    if 32 ≤ c.toNat ∧ c.toNat < 127 ∧ c ≠ '\\' then do
      let (bytes, rest) ← quotedBytes rest
      pure (UInt8.ofNat c.toNat :: bytes, rest)
    else none
  | [] => none

private def constant : Parser Item := do
  let name ← globalName
  literal " = private constant ["
  let size ← unsigned
  literal " x i8] c\""
  let bytes ← quotedBytes
  literal "\n"
  if size = bytes.length then pure (.constant name ⟨bytes.toArray⟩) else fun _ => none

private def declare : Parser Item := do
  literal "declare "
  let result ← typeCode
  literal " "
  let fn ← globalName
  literal "("
  let args ← series typeCode ", " ")\n"
  pure (.declare result fn args)

private def define : Parser Item := do
  literal "define "
  let internal ← alt (keyword "internal " true) (pure false)
  let result ← typeCode
  literal " "
  let fn ← globalName
  literal "("
  let args ← series (do
    let ty ← typeCode
    literal " "
    let name ← localName
    pure (ty, name)) ", " (r") {" ++ "\n")
  pure (.define internal result fn args)

private def indented : Parser Item
  | input@('%' :: _) => (Item.instruction <$> instruction) input
  | input@('c' :: _) => (Item.instruction <$> instruction) input
  | input => (Item.terminator <$> terminator) input

private def item : Parser Item
  | ' ' :: ' ' :: input => indented input
  | input@('@' :: _) => constant input
  | '}' :: '\n' :: input => some (.endFunction, input)
  | '\n' :: input => some (.blank, input)
  | input => (alt declare <| alt define <| do
      let name ← labelName
      literal ":\n"
      pure (.label name)) input

private def moduleItems : Nat → List Char → Option (List Item)
  | _, [] => some []
  | 0, _ :: _ => none
  | fuel + 1, input@(_ :: _) => do
    let (first, rest) ← item input
    return first :: (← moduleItems fuel rest)

/-- Parse the entire assembly string. Any unconsumed or malformed item fails. -/
def parse (input : String) : Option (List Item) :=
  moduleItems input.toList.length input.toList

/-- Read ordered integer positions from the complete input, after grammar validation. -/
def inventory (input : String) : Option (List IntegerField) :=
  (parse input).map (·.flatMap Item.inventory)

private theorem consume_append (token rest : List Char) :
    consume token (token ++ rest) = some rest := by
  induction token with
  | nil => rfl
  | cons c cs ih => simp [consume, ih]

private theorem word_append (decode : List Char → Option α) (chars rest : List Char)
    (valid : ∀ c ∈ chars, symbolChar c = true)
    (boundary : ∀ c, rest.head? = some c → symbolChar c = false) :
    word decode (chars ++ rest) = (decode chars).map (·, rest) := by
  have stop : rest.takeWhile symbolChar = [] := by
    cases rest with
    | nil => rfl
    | cons c cs => simp [boundary c rfl]
  simp [word, List.takeWhile_append_of_pos valid, stop]
  cases decode chars <;> rfl

private theorem decimal_chars (n : Nat) :
    ∀ c ∈ (decimal n).toList, symbolChar c = true := by
  intro c hc
  have hd : c.isDigit = true := by
    rw [decimal, Nat.toList_repr] at hc
    exact Nat.isDigit_of_mem_toDigits (by decide) (by decide) hc
  simp [symbolChar, Char.isAlphanum, hd]

private theorem unsigned_append (n : Nat) (rest : List Char)
    (boundary : ∀ c, rest.head? = some c → symbolChar c = false) :
    unsigned ((decimal n).toList ++ rest) = some (n, rest) := by
  have exactValue := (parseDecimal_exact _ _).2 (decimal_token n)
  simp [unsigned, decodeUnsigned, word_append _ _ _ (decimal_chars n) boundary, exactValue]

private theorem argument_unsigned (n : Nat) (rest : List Char)
    (boundary : ∀ c, rest.head? = some c → symbolChar c = false) :
    argument ((decimal n).toList ++ rest) = some (.immediate (.natural n), rest) := by
  have valid := (decimal_token n)
  have magnitude : DecimalMagnitude (decimal n).toList := by
    rcases valid with h | ⟨digits, h, _⟩
    · exact h.1
    · have bad := Nat.isDigit_of_mem_toDigits (b := 10) (by decide) (by decide) (n := n) (c := '-')
      rw [← Nat.toList_repr, ← decimal, h] at bad
      have := bad (by simp)
      contradiction
  have numeric := unsigned_append n rest boundary
  cases hc : (decimal n).toList with
  | nil => exact (magnitude.1 hc).elim
  | cons c cs =>
    rw [hc] at numeric
    have digit := magnitude.2.1 c (by simp [hc])
    have notPercent : '%' ≠ c := by rintro rfl; contradiction
    have notAt : '@' ≠ c := by rintro rfl; contradiction
    have noLocal : localName ((c :: cs) ++ rest) = none := by
      change ((if '%' = c then some (cs ++ rest) else none).map ((), ·)).bind _ = _
      simp [notPercent]
    have noGlobal : globalName ((c :: cs) ++ rest) = none := by
      change ((if '@' = c then some (cs ++ rest) else none).map ((), ·)).bind _ = _
      simp [notAt]
    change ((localName ((c :: cs) ++ rest)).map _).orElse (fun _ =>
      ((globalName ((c :: cs) ++ rest)).map _).orElse fun _ =>
        (unsigned ((c :: cs) ++ rest)).map _) = _
    rw [noLocal, Option.map_none, Option.orElse_none, noGlobal, Option.map_none,
      Option.orElse_none, numeric]
    rfl

private theorem literal_append (s : String) (rest : List Char) :
    literal s (s.toList ++ rest) = some ((), rest) := by
  simp [literal, consume_append]

private theorem pure_run (a : α) (input : List Char) :
    (pure a : Parser α) input = some (a, input) := rfl

private theorem bind_run (p : Parser α) (f : α → Parser β) (input : List Char) :
    (p >>= f) input = (p input).bind (fun (a, rest) => f a rest) := rfl

private theorem map_run (p : Parser α) (f : α → β) (input : List Char) :
    (f <$> p) input = (p input).map (fun (a, rest) => (f a, rest)) := rfl

private theorem space_run (rest : List Char) :
    literal " " (' ' :: rest) = some ((), rest) :=
  literal_append " " rest

private theorem type_u32 (rest : List Char) :
    typeCode (['i', '3', '2', ' '] ++ rest) = some (.integer .u32, ' ' :: rest) := by
  have lexed := word_append atomicType ['i', '3', '2'] (' ' :: rest)
    (by simp [symbolChar]) (by simp [symbolChar])
  rw [show typeCode (['i', '3', '2', ' '] ++ rest) =
    word atomicType (['i', '3', '2'] ++ ' ' :: rest) from rfl, lexed]
  rfl

private theorem returnValue_i32 (chars rest : List Char) (value : Argument)
    (argumentStep : argument (chars ++ '\n' :: rest) = some (value, '\n' :: rest)) :
    returnValue (['i', '3', '2', ' '] ++ (chars ++ '\n' :: rest)) =
      some (.ret (.integer .u32, value), rest) := by
  simp only [returnValue, typed, bind_run, type_u32, Option.bind_some, space_run, argumentStep]
  rfl

private theorem item_ret_i32 (chars rest : List Char) (value : Argument)
    (argumentStep : argument (chars ++ '\n' :: rest) = some (value, '\n' :: rest)) :
    item ("  ret i32 ".toList ++ (chars ++ '\n' :: rest)) =
      some (.terminator (.ret (.integer .u32, value)), rest) := by
  have route (suffix : List Char) :
      item ("  ret i32 ".toList ++ suffix) =
        (returnValue (['i', '3', '2', ' '] ++ suffix)).map (fun (value, tail) =>
          (Item.terminator value, tail)) := rfl
  rw [route, returnValue_i32 _ _ _ argumentStep]
  rfl

private theorem parse_single (c : Char) (tail : List Char) (value : Item)
    (parsed : item (c :: tail) = some (value, [])) :
    parse (String.ofList (c :: tail)) = some [value] := by
  unfold parse
  rw [String.toList_ofList, List.length_cons, moduleItems.eq_3]
  change (item (c :: tail)).bind _ = _
  rw [parsed, Option.bind_some]
  change (moduleItems tail.length []).bind _ = _
  rw [moduleItems.eq_1]
  rfl

private theorem parse_ret_i32 (chars : List Char) (value : Argument)
    (argumentStep : argument (chars ++ ['\n']) = some (value, ['\n'])) :
    parse (String.ofList ("  ret i32 ".toList ++ (chars ++ ['\n']))) =
      some [.terminator (.ret (.integer .u32, value))] := by
  have step := item_ret_i32 chars [] value argumentStep
  let tail := " ret i32 ".toList ++ (chars ++ ['\n'])
  change item (' ' :: tail) = _ at step
  exact parse_single ' ' tail _ step

private theorem argument_result (n : Nat) (rest : List Char)
    (boundary : ∀ c, rest.head? = some c → symbolChar c = false) :
    argument (['%', 'r', 'e', 't'] ++ ((decimal n).toList ++ rest)) =
      some (.local (.result n), rest) := by
  let digits := (decimal n).toList
  have unsignedStep : decodeUnsigned digits = some n := by
    simp [digits, decodeUnsigned, (parseDecimal_exact _ _).2 (decimal_token n)]
  have decode : localWord (['r', 'e', 't'] ++ digits) = some (.result n) := by
    rw [show localWord (['r', 'e', 't'] ++ digits) =
      (decodeUnsigned digits).map Local.result from rfl, unsignedStep]
    rfl
  have wordStep := word_append localWord (['r', 'e', 't'] ++ digits) rest (by
    intro c hc
    rcases List.mem_append.mp hc with h | h
    · simp [symbolChar] at h ⊢
      rcases h with rfl | rfl | rfl <;> decide
    · exact decimal_chars n c h) boundary
  rw [decode] at wordStep
  have nameStep : localName (['%', 'r', 'e', 't'] ++ (digits ++ rest)) =
      some (.result n, rest) := by
    simp only [localName, bind_run]
    rw [show literal "%" (['%', 'r', 'e', 't'] ++ (digits ++ rest)) =
      some ((), (['r', 'e', 't'] ++ digits) ++ rest) from rfl]
    rw [Option.bind_some]
    exact wordStep
  dsimp only [digits] at nameStep
  simp only [argument, alt, map_run, nameStep, Option.map_some, Option.orElse_some]

/-- Singleton instance for an i32 return with an arbitrary immediate. -/
theorem parse_render_i32_return (v : Immediate) :
    parse (render [.terminator (.ret (.integer .u32, .immediate v))]) =
      some [.terminator (.ret (.integer .u32, .immediate v))] := by
  have chars : (render [.terminator (.ret (.integer .u32, .immediate v))]).toList =
      "  ret i32 ".toList ++ ((decimal v.value).toList ++ ['\n']) := by
    simp [render, Item.pieces, terminatorPieces, Concrete.Backend.LLVM.typed, Concrete.Backend.LLVM.typeCode,
      renderPieces, Piece.render, Concrete.Backend.LLVM.arg, imm, text, numbered,
      Opening.char, Separator.char, NumberPrefix.text, Immediate.token, Width.bits]
    rfl
  have full := parse_ret_i32 (decimal v.value).toList (.immediate (.natural v.value))
    (argument_unsigned v.value ['\n'] (by simp [symbolChar]))
  rw [← chars, String.ofList_toList] at full
  cases v
  exact full

/-- Singleton instance for an i32 return with an arbitrary numbered result local. -/
theorem parse_render_i32_result (block : Nat) :
    parse (render [.terminator (.ret (.integer .u32, .local (.result block)))]) =
      some [.terminator (.ret (.integer .u32, .local (.result block)))] := by
  have chars : (render [.terminator (.ret (.integer .u32, .local (.result block)))]).toList =
      "  ret i32 ".toList ++ ((['%', 'r', 'e', 't'] ++ (decimal block).toList) ++ ['\n']) := by
    simp [render, Item.pieces, terminatorPieces, Concrete.Backend.LLVM.typed,
      Concrete.Backend.LLVM.typeCode, renderPieces, Piece.render, Concrete.Backend.LLVM.arg,
      Concrete.Backend.LLVM.localName, framed, text, numbered, Separator.char,
      NumberPrefix.text, Width.bits]
    rfl
  have step := argument_result block ['\n'] (by simp [symbolChar])
  rw [← List.append_assoc] at step
  have full := parse_ret_i32 _ (.local (.result block)) step
  rw [← chars, String.ofList_toList] at full
  exact full

private theorem local_register (n : Nat) (rest : List Char)
    (boundary : ∀ c, rest.head? = some c → symbolChar c = false) :
    localName (['%', 'r'] ++ ((decimal n).toList ++ rest)) = some (.register n, rest) := by
  let digits := (decimal n).toList
  have decode : decodeUnsigned digits = some n := by
    simp [digits, decodeUnsigned, (parseDecimal_exact _ _).2 (decimal_token n)]
  have nameDecode : localWord ('r' :: digits) = some (.register n) := by
    have digitsOnly : ∀ c ∈ digits, c.isDigit = true := by
      intro c hc
      change c ∈ (decimal n).toList at hc
      rw [decimal, Nat.toList_repr] at hc
      exact Nat.isDigit_of_mem_toDigits (by decide) (by decide) hc
    cases h : digits with
    | nil => simp [digits, decimal, Nat.toList_repr, Nat.toDigits_ne_nil] at h
    | cons c cs =>
      have digit := digitsOnly c (by simp [h])
      have ne : c ≠ 'e' := by rintro rfl; contradiction
      have eqn : localWord ('r' :: c :: cs) = (decodeUnsigned (c :: cs)).map Local.register := by
        simp [localWord, ne]
      rw [h] at decode
      simp [eqn, decode]
  have wordStep := word_append localWord ('r' :: digits) rest (by
    intro c hc
    simp only [List.mem_cons] at hc
    rcases hc with rfl | hc
    · decide
    · exact decimal_chars n c hc) boundary
  rw [nameDecode] at wordStep
  simp only [localName, bind_run]
  rw [show literal "%" (['%', 'r'] ++ ((decimal n).toList ++ rest)) =
    some ((), ('r' :: digits) ++ rest) from rfl, Option.bind_some]
  exact wordStep

private theorem type_u8 (rest : List Char) :
    typeCode (['i', '8', ' '] ++ rest) = some (.integer .u8, ' ' :: rest) := by
  have lexed := word_append atomicType ['i', '8'] (' ' :: rest)
    (by simp [symbolChar]) (by simp [symbolChar])
  rw [show typeCode (['i', '8', ' '] ++ rest) =
    word atomicType (['i', '8'] ++ ' ' :: rest) from rfl, lexed]
  rfl

private theorem binary_add (dst : Local) (left right : Nat) :
    binary dst arithmetic Instruction.bin
        ("add i8 ".toList ++ ((decimal left).toList ++
          ',' :: ' ' :: ((decimal right).toList ++ ['\n']))) =
      some (.bin dst .add (.integer .u8) (.immediate (.natural left))
        (.immediate (.natural right)), []) := by
  let rhs := (decimal right).toList ++ ['\n']
  have leftStep := argument_unsigned left (',' :: ' ' :: rhs) (by simp [symbolChar])
  have rightStep := argument_unsigned right ['\n'] (by simp [symbolChar])
  let args := (decimal left).toList ++ ',' :: ' ' :: rhs
  have route :
     binary dst arithmetic Instruction.bin ("add i8 ".toList ++ args) =
      ((do
          let (ty, lhs) ← typed
          literal ", "
          let rhs ← argument
          literal "\n"
          pure (Instruction.bin dst .add ty lhs rhs)) : Parser Instruction)
       (['i', '8', ' '] ++ args) := rfl
  change binary dst arithmetic Instruction.bin ("add i8 ".toList ++ args) = _
  rw [route]
  have sep := literal_append ", " rhs
  simp only [typed, bind_run, type_u8, Option.bind_some, space_run]
  change ((argument args).bind _).bind _ = _
  change argument args = _ at leftStep
  rw [leftStep]
  simp only [Option.bind_some, pure_run]
  change ((literal ", " (", ".toList ++ rhs)).bind _) = _
  rw [sep, Option.bind_some]
  change (argument ((decimal right).toList ++ ['\n'])).bind _ = _
  rw [rightStep]
  rfl
/-- Singleton instance for an i8 addition with two arbitrary immediates. -/
theorem parse_render_i8_add (dst : Nat) (left right : Immediate) :
    parse (render [.instruction (.bin (.register dst) .add (.integer .u8)
      (.immediate left) (.immediate right))]) =
      some [.instruction (.bin (.register dst) .add (.integer .u8)
        (.immediate left) (.immediate right))] := by
  let suffix := "add i8 ".toList ++ ((decimal left.value).toList ++
    ',' :: ' ' :: ((decimal right.value).toList ++ ['\n']))
  let afterDst := " = ".toList ++ suffix
  let line := ['%', 'r'] ++ ((decimal dst).toList ++ afterDst)
  have nameStep := local_register dst afterDst (by simp [afterDst, symbolChar])
  have assignStep := literal_append " = " suffix
  have binStep := binary_add (.register dst) left.value right.value
  have route (tail : List Char) : item (' ' :: ' ' :: '%' :: tail) =
      (instruction ('%' :: tail)).map (fun (ins, rest) => (Item.instruction ins, rest)) := rfl
  have instructionStep :
      instruction line = some (.bin (.register dst) .add (.integer .u8)
        (.immediate (.natural left.value)) (.immediate (.natural right.value)), []) := by
    have start (tail : List Char) : call none ('%' :: tail) = none := rfl
    have noCall : call none line = none := by apply start
    simp only [instruction, alt, noCall, Option.orElse_none, bind_run]
    change (localName line).bind _ = _
    rw [nameStep, Option.bind_some]
    change (literal " = " afterDst).bind _ = _
    rw [assignStep, Option.bind_some]
    dsimp only [suffix]
    rw [binStep, Option.orElse_some]
  have itemStep : item (' ' :: ' ' :: line) =
      some (.instruction (.bin (.register dst) .add (.integer .u8)
        (.immediate (.natural left.value)) (.immediate (.natural right.value))), []) := by
    change item (' ' :: ' ' :: '%' :: ('r' :: ((decimal dst).toList ++ afterDst))) = _
    rw [route]
    change (instruction line).map _ = _
    rw [instructionStep]
    rfl
  have chars : (render [.instruction (.bin (.register dst) .add (.integer .u8)
      (.immediate left) (.immediate right))]).toList = ' ' :: ' ' :: line := by
    simp [render, Item.pieces, instructionPieces, assign, Concrete.Backend.LLVM.localName,
      Concrete.Backend.LLVM.arithmetic, Concrete.Backend.LLVM.typeCode, renderPieces, Piece.render,
      Concrete.Backend.LLVM.arg, imm, text, numbered, Opening.char, Separator.char,
      NumberPrefix.text, Immediate.token, Width.bits, line, afterDst, suffix]
    rfl
  have parsed := parse_single _ _ _ itemStep
  rw [← chars, String.ofList_toList] at parsed
  cases left
  cases right
  exact parsed

private def chars (doc : Doc) : List Char := (renderPieces doc).toList

@[local simp] private theorem chars_append (a b : Doc) :
    chars (a ++ b) = chars a ++ chars b := by
  simp [chars, renderPieces, String.join_append]

private theorem typeCode_append (ty : TypeCode) (c : Char) (rest : List Char)
    (boundary : symbolChar c = false) :
    typeCode (chars (LLVM.typeCode ty) ++ c :: rest) = some (ty, c :: rest) := by
  have endWord : ∀ d, (c :: rest).head? = some d → symbolChar d = false := by
    intro d hd
    simpa only [List.head?_cons, Option.some.injEq] using
      (show c = d → symbolChar d = false from fun h => h ▸ boundary) (Option.some.inj hd)
  cases ty with
  | integer w =>
    cases w
    · exact word_append atomicType ['i','8'] _ (by simp [symbolChar]) endWord
    · exact word_append atomicType ['i','1','6'] _ (by simp [symbolChar]) endWord
    · exact word_append atomicType ['i','3','2'] _ (by simp [symbolChar]) endWord
    · exact word_append atomicType ['i','6','4'] _ (by simp [symbolChar]) endWord
  | flag => exact word_append atomicType ['i','1'] _ (by simp [symbolChar]) endWord
  | ptr => exact word_append atomicType ['p','t','r'] _ (by simp [symbolChar]) endWord
  | void => exact word_append atomicType ['v','o','i','d'] _ (by simp [symbolChar]) endWord
  | overflowPair w =>
    have go (w : Width) (digits : List Char)
        (hw : decodeWidth digits = some w) (valid : ∀ d ∈ digits, symbolChar d = true)
        (shape : chars (LLVM.typeCode (.overflowPair w)) = '{' :: digits ++ ", i1}".toList) :
        typeCode (chars (LLVM.typeCode (.overflowPair w)) ++ c :: rest) =
          some (.overflowPair w, c :: rest) := by
      rw [shape]
      have step := word_append decodeWidth digits (", i1}".toList ++ c :: rest) valid
        (by simp [symbolChar])
      rw [hw] at step
      simp only [List.cons_append, List.append_assoc, typeCode, pairType, bind_run]
      rw [show literal r"{" ('{' :: (digits ++ (", i1}".toList ++ c :: rest))) =
        some ((), digits ++ (", i1}".toList ++ c :: rest)) from rfl, Option.bind_some]
      rw [show width (digits ++ (", i1}".toList ++ c :: rest)) = _ from step]
      rfl
    cases w
    · exact go _ ['i','8'] rfl (by simp [symbolChar]) rfl
    · exact go _ ['i','1','6'] rfl (by simp [symbolChar]) rfl
    · exact go _ ['i','3','2'] rfl (by simp [symbolChar]) rfl
    · exact go _ ['i','6','4'] rfl (by simp [symbolChar]) rfl

private def WordBoundary (rest : List Char) : Prop :=
  ∀ c, rest.head? = some c → symbolChar c = false

private theorem decodeUnsigned_decimal (n : Nat) :
    decodeUnsigned (decimal n).toList = some n := by
  simp [decodeUnsigned, (parseDecimal_exact _ _).2 (decimal_token n)]

private theorem word_exact (decode : List Char → Option α) (token rest : List Char) (a : α)
    (valid : ∀ c ∈ token, symbolChar c = true) (finish : WordBoundary rest)
    (decoded : decode token = some a) :
    word decode (token ++ rest) = some (a, rest) := by
  rw [word_append _ _ _ valid finish, decoded]
  rfl

private theorem word_numbered (decode : List Char → Option α) (stemChars rest : List Char)
    (n : Nat) (a : α) (valid : ∀ c ∈ stemChars, symbolChar c = true) (finish : WordBoundary rest)
    (decoded : decode (stemChars ++ (decimal n).toList) = some a) :
    word decode (stemChars ++ ((decimal n).toList ++ rest)) = some (a, rest) := by
  rw [← List.append_assoc]
  apply word_exact _ _ _ _ _ finish decoded
  intro c hc
  exact (List.mem_append.mp hc).elim (valid c) (decimal_chars n c)

private theorem width_append (w : Width) (rest : List Char) (finish : WordBoundary rest) :
    width (chars (LLVM.typeCode (.integer w)) ++ rest) = some (w, rest) := by
  cases w
  · exact word_exact _ ['i','8'] _ _ (by simp [symbolChar]) finish rfl
  · exact word_exact _ ['i','1','6'] _ _ (by simp [symbolChar]) finish rfl
  · exact word_exact _ ['i','3','2'] _ _ (by simp [symbolChar]) finish rfl
  · exact word_exact _ ['i','6','4'] _ _ (by simp [symbolChar]) finish rfl

private theorem arithmetic_append (op : Arithmetic) (rest : List Char) :
    arithmetic (chars (LLVM.arithmetic op) ++ rest) = some (op, rest) := by
  cases op <;> rfl

private theorem conversion_append (op : Conversion) (rest : List Char) :
    conversion (chars (LLVM.conversion op) ++ rest) = some (op, rest) := by
  cases op <;> rfl

private theorem comparison_append (op : Comparison) (rest : List Char) :
    comparison (chars (LLVM.comparison op) ++ rest) = some (op, rest) := by
  cases op <;> rfl

@[local simp] private theorem chars_text (s : String) (valid) :
    chars (text s valid) = s.toList := by
  simp [chars, renderPieces, text, Piece.render]

@[local simp] private theorem chars_numbered (kind : NumberPrefix) (n : Nat) :
    chars (numbered kind n) = kind.text.toList ++ (decimal n).toList := by
  simp [chars, renderPieces, numbered, Piece.render]

private theorem arithmetic_rejects_conversion (op : Conversion) (rest : List Char) :
    arithmetic (chars (LLVM.conversion op) ++ rest) = none := by
  cases op <;> rfl

private theorem conversion_rejects_arithmetic (op : Arithmetic) (rest : List Char) :
    conversion (chars (LLVM.arithmetic op) ++ rest) = none := by
  cases op <;> rfl

private theorem decimal_head (n : Nat) :
    ∃ c cs, (decimal n).toList = c :: cs ∧ c.isDigit = true := by
  have valid (c : Char) (hc : c ∈ (decimal n).toList) : c.isDigit = true := by
    rw [decimal, Nat.toList_repr] at hc
    exact Nat.isDigit_of_mem_toDigits (by decide) (by decide) hc
  cases h : (decimal n).toList with
  | nil => simp [decimal, Nat.toList_repr, Nat.toDigits_ne_nil] at h
  | cons c cs => exact ⟨c, cs, rfl, valid c (by simp [h])⟩

private theorem localName_append (name : Local) (rest : List Char) (finish : WordBoundary rest) :
    localName (chars (LLVM.localName name) ++ rest) = some (name, rest) := by
  have run (token : List Char) :
      localName (('%' :: token) ++ rest) = word localWord (token ++ rest) := rfl
  cases name <;> simp only [LLVM.localName, chars_numbered, chars_text, NumberPrefix.text]
  case register n =>
    rw [show "%r".toList = ['%', 'r'] from rfl, List.append_assoc]
    exact local_register n rest finish
  case overflow n =>
    rw [show "%o".toList = '%' :: ['o'] from rfl, List.cons_append, run, List.append_assoc]
    apply word_numbered _ _ _ _ _ (by simp [symbolChar]) finish
    obtain ⟨c, cs, digits, digit⟩ := decimal_head n
    have ne : c ≠ 'k' := by rintro rfl; contradiction
    have decoded := decodeUnsigned_decimal n
    rw [digits] at decoded ⊢
    simp [localWord, ne, decoded]
  case result n =>
    rw [show "%ret".toList = '%' :: ['r','e','t'] from rfl, List.cons_append, run, List.append_assoc]
    apply word_numbered _ _ _ _ _ (by simp [symbolChar]) finish
    change (decodeUnsigned (decimal n).toList).map Local.result = _
    rw [decodeUnsigned_decimal]
    rfl
  case p =>
    rw [show "%p".toList = '%' :: ['p'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case n =>
    rw [show "%n".toList = '%' :: ['n'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case pp =>
    rw [show "%pp".toList = '%' :: ['p','p'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case np =>
    rw [show "%np".toList = '%' :: ['n','p'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case nn =>
    rw [show "%nn".toList = '%' :: ['n','n'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case rest =>
    rw [show "%rest".toList = '%' :: ['r','e','s','t'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case done =>
    rw [show "%done".toList = '%' :: ['d','o','n','e'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case k =>
    rw [show "%k".toList = '%' :: ['k'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case bad =>
    rw [show "%bad".toList = '%' :: ['b','a','d'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case w =>
    rw [show "%w".toList = '%' :: ['w'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case ok =>
    rw [show "%ok".toList = '%' :: ['o','k'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case i =>
    rw [show "%i".toList = '%' :: ['i'] from rfl, run]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl

private theorem numberedName_append (stem : String) (make : Nat → α) (n : Nat) :
    numberedName stem make (stem.toList ++ (decimal n).toList) = some (make n) := by
  have step := unsigned_append n [] (by simp)
  simp only [List.append_nil] at step
  simp [numberedName, consume_append, step]

private theorem labelName_append (name : Label) (rest : List Char) (finish : WordBoundary rest) :
    labelName (chars (LLVM.labelName name) ++ rest) = some (name, rest) := by
  simp only [labelName, named]
  cases name <;> simp only [LLVM.labelName, chars_numbered, chars_text, NumberPrefix.text,
    List.append_assoc]
  case block n =>
    rw [show "b".toList = ['b'] from rfl]
    apply word_numbered _ _ _ _ _ (by simp [symbolChar]) finish
    have value := numberedName_append "b" Label.block n
    obtain ⟨c, cs, digits, digit⟩ := decimal_head n
    have ne : 'a' ≠ c := by rintro rfl; contradiction
    rw [digits] at value ⊢
    have miss : ('a' == c) = false := by simpa using ne
    simp [namedWord, List.find?, List.findSome?, miss]
    rw [show numberedName "b" Label.block ('b' :: c :: cs) = some (.block n) from value]
  case trapSite n =>
    rw [show "t".toList = ['t'] from rfl]
    apply word_numbered _ _ _ _ _ (by simp [symbolChar]) finish
    have value := numberedName_append "t" Label.trapSite n
    obtain ⟨c, cs, digits, digit⟩ := decimal_head n
    have ne : 'r' ≠ c := by rintro rfl; contradiction
    rw [digits] at value ⊢
    have miss : ('r' == c) = false := by simpa using ne
    simp [namedWord, List.find?, List.findSome?, miss]
    rw [show numberedName "b" Label.block ('t' :: c :: cs) = none from rfl,
      show numberedName "t" Label.trapSite ('t' :: c :: cs) = some (.trapSite n) from value]
  case entry =>
    rw [show "entry".toList = ['e','n','t','r','y'] from rfl]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case loop =>
    rw [show "loop".toList = ['l','o','o','p'] from rfl]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case out =>
    rw [show "out".toList = ['o','u','t'] from rfl]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case wr =>
    rw [show "wr".toList = ['w','r'] from rfl]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case more =>
    rw [show "more".toList = ['m','o','r','e'] from rfl]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case exit =>
    rw [show "exit".toList = ['e','x','i','t'] from rfl]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case trap =>
    rw [show "trap".toList = ['t','r','a','p'] from rfl]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case bad =>
    rw [show "bad".toList = ['b','a','d'] from rfl]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl

private theorem intrinsic_append (op : Arithmetic) (ty : IntTy) (rest : List Char)
    (finish : WordBoundary rest) :
    globalName (chars (LLVM.global (.overflow op ty)) ++ rest) =
      some (.overflow op ty, rest) := by
  let tail := chars (LLVM.typeCode (.integer (Width.ofCore ty))) ++ rest
  have step : width tail = some (Width.ofCore ty, rest) := width_append _ _ finish
  have route (signed : Bool) (op : Arithmetic) (input : List Char) :
      intrinsic ("llvm.".toList ++ ((if signed then "s" else "u").toList ++
        (chars (LLVM.arithmetic op) ++ (".with.overflow.".toList ++ input)))) =
      (width input).bind (fun (w, rest) => some (Global.overflow op (intrinsicType w signed), rest)) := by
    cases signed <;> cases op <;>
      simp only [LLVM.arithmetic, chars_text, Bool.false_eq_true, ite_false, ite_true] <;> rfl
  have shape :
      chars (LLVM.global (.overflow op ty)) ++ rest =
        "@llvm.".toList ++ ((if ty.signed then "s" else "u").toList ++
          (chars (LLVM.arithmetic op) ++ (".with.overflow.".toList ++ tail))) := by
    simp only [LLVM.global, chars_append, chars_text, List.append_assoc, tail]
    cases ty <;> rfl
  rw [shape]
  change (intrinsic ("llvm.".toList ++ _)).orElse _ = _
  rw [route, step]
  cases ty <;> rfl

private theorem globalName_append (name : Global) (rest : List Char) (finish : WordBoundary rest) :
    globalName (chars (LLVM.global name) ++ rest) = some (name, rest) := by
  have run (token : List Char) (miss : consume "llvm.".toList (token ++ rest) = none) :
      globalName (('@' :: token) ++ rest) = globalPlain (token ++ rest) := by
    change (intrinsic (token ++ rest)).orElse _ = _
    simp only [intrinsic, bind_run, literal, miss, Option.map_none, Option.bind_none,
      Option.orElse_none]
    rfl
  cases name
  case overflow op ty => exact intrinsic_append op ty rest finish
  case site n =>
    simp only [LLVM.global, chars_numbered, NumberPrefix.text]
    rw [show "@.site".toList = '@' :: ['.','s','i','t','e'] from rfl, List.cons_append,
      run (['.','s','i','t','e'] ++ (decimal n).toList) rfl, List.append_assoc]
    simp only [globalPlain, named]
    apply word_numbered _ _ _ _ _ (by simp [symbolChar]) finish
    have decoded := numberedName_append ".site" Global.site n
    generalize (decimal n).toList = digits at decoded ⊢
    change numberedName ".site" Global.site ('.' :: 's' :: 'i' :: 't' :: 'e' :: digits) =
      some (.site n) at decoded
    simp [namedWord, List.find?, List.findSome?, decoded]
  case body =>
    simp only [LLVM.global, chars_text]
    rw [show "@concrete_body".toList = '@' :: ['c','o','n','c','r','e','t','e','_','b','o','d','y'] from rfl, run ['c','o','n','c','r','e','t','e','_','b','o','d','y'] rfl]
    simp only [globalPlain, named]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case main =>
    simp only [LLVM.global, chars_text]
    rw [show "@main".toList = '@' :: ['m','a','i','n'] from rfl, run ['m','a','i','n'] rfl]
    simp only [globalPlain, named]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case writeAll =>
    simp only [LLVM.global, chars_text]
    rw [show "@concrete_rt_write_all".toList = '@' :: ['c','o','n','c','r','e','t','e','_','r','t','_','w','r','i','t','e','_','a','l','l'] from rfl, run ['c','o','n','c','r','e','t','e','_','r','t','_','w','r','i','t','e','_','a','l','l'] rfl]
    simp only [globalPlain, named]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case write =>
    simp only [LLVM.global, chars_text]
    rw [show "@write".toList = '@' :: ['w','r','i','t','e'] from rfl, run ['w','r','i','t','e'] rfl]
    simp only [globalPlain, named]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case exit =>
    simp only [LLVM.global, chars_text]
    rw [show "@_exit".toList = '@' :: ['_','e','x','i','t'] from rfl, run ['_','e','x','i','t'] rfl]
    simp only [globalPlain, named]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case abort =>
    simp only [LLVM.global, chars_text]
    rw [show "@abort".toList = '@' :: ['a','b','o','r','t'] from rfl, run ['a','b','o','r','t'] rfl]
    simp only [globalPlain, named]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl
  case bad =>
    simp only [LLVM.global, chars_text]
    rw [show "@.bad".toList = '@' :: ['.','b','a','d'] from rfl, run ['.','b','a','d'] rfl]
    simp only [globalPlain, named]
    exact word_exact _ _ _ _ (by simp [symbolChar]) finish rfl

private theorem global_starts (name : Global) : ∃ token, chars (LLVM.global name) = '@' :: token := by
  cases name <;> simp only [LLVM.global, chars_append, chars_text, chars_numbered] <;>
    exact ⟨_, rfl⟩

private theorem argument_local_append (name : Local) (rest : List Char)
    (finish : WordBoundary rest) :
    argument (chars (LLVM.localName name) ++ rest) = some (.local name, rest) := by
  simp only [argument, alt, map_run, localName_append name rest finish, Option.map_some,
    Option.orElse_some]

private theorem argument_global_append (name : Global) (rest : List Char)
    (finish : WordBoundary rest) :
    argument (chars (LLVM.global name) ++ rest) = some (.global name, rest) := by
  obtain ⟨token, shape⟩ := global_starts name
  have miss : localName (chars (LLVM.global name) ++ rest) = none := by
    rw [shape]
    rfl
  simp only [argument, alt, map_run, miss, Option.map_none, Option.orElse_none,
    globalName_append name rest finish, Option.map_some, Option.orElse_some]

@[local simp] private theorem chars_punctuation (after : Separator) :
    chars [.punctuation after] = [after.char] := by
  simp [chars, renderPieces, Piece.render]

private theorem space_chars : " ".toList = [' '] := rfl

private theorem argument_append (a : Argument) (after : Separator) (rest : List Char) :
    argument ((chars (LLVM.arg a after)).tail ++ rest) = some (a, after.char :: rest) := by
  have finish : WordBoundary (after.char :: rest) := by
    cases after <;> simp [WordBoundary, Separator.char, symbolChar]
  cases a with
  | «local» name =>
    simpa only [LLVM.arg, framed, chars_append, chars_text, chars_punctuation, space_chars, List.append_assoc,
      List.cons_append, List.nil_append, List.tail_cons] using argument_local_append name _ finish
  | global name =>
    simpa only [LLVM.arg, framed, chars_append, chars_text, chars_punctuation, space_chars, List.append_assoc,
      List.cons_append, List.nil_append, List.tail_cons] using argument_global_append name _ finish
  | immediate value =>
    cases value with
    | mk n =>
      simpa [LLVM.arg, imm, chars, renderPieces, Piece.render, Opening.char, Immediate.token,
        Immediate.natural, List.append_assoc] using argument_unsigned n _ finish

private theorem typed_append (a : TypedArgument) (after : Separator) (rest : List Char) :
    typed (chars (LLVM.typed a after) ++ rest) = some (a, after.char :: rest) := by
  obtain ⟨ty, value⟩ := a
  have starts : ∃ token, chars (LLVM.arg value after) = ' ' :: token := by
    cases value with
    | «local» _ | global _ =>
      simp only [LLVM.arg, framed, chars_append, chars_text, space_chars, List.cons_append,
        List.nil_append]
      exact ⟨_, rfl⟩
    | immediate v =>
      simp [LLVM.arg, imm, chars, renderPieces, Piece.render, Opening.char, Immediate.token]
  obtain ⟨token, shape⟩ := starts
  have step := argument_append value after rest
  rw [shape, List.tail_cons] at step
  simp only [LLVM.typed, chars_append, List.append_assoc, shape, List.cons_append]
  have tyStep := typeCode_append ty ' ' (token ++ rest) (by decide)
  simp only [typed, bind_run, tyStep, Option.bind_some, space_run, step, pure_run]

private theorem keyword_append (s : String) (a : α) (rest : List Char) :
    keyword s a (s.toList ++ rest) = some (a, rest) := by
  simp [keyword, bind_run, literal_append, pure_run]

private theorem keyword_fail (s : String) (a : α) (input : List Char)
    (miss : consume s.toList input = none) : keyword s a input = none := by
  simp [keyword, bind_run, literal, miss]

private theorem labelRef_append (name : Label) (rest : List Char) (finish : WordBoundary rest) :
    labelRef ("label %".toList ++ (chars (LLVM.labelName name) ++ rest)) =
      some (name, rest) := by
  simp only [labelRef, bind_run, literal_append, Option.bind_some, labelName_append name rest finish]

/-- Entry spellings joined by separators and followed by the closing token. -/
private def seriesChars (entry : α → List Char) (sep ending : String) :
    List α → List Char
  | [] => ending.toList
  | a :: as => entry a ++ (if as.isEmpty then [] else sep.toList) ++ seriesChars entry sep ending as

private theorem seriesChars_length (entry : α → List Char) (sep ending : String)
    (nonempty : ∀ a, entry a ≠ []) (xs : List α) :
    xs.length ≤ (seriesChars entry sep ending xs).length := by
  induction xs with
  | nil => simp
  | cons a as ih =>
    have positive : 0 < (entry a).length := List.length_pos_iff.mpr (nonempty a)
    simp only [seriesChars, List.length_cons, List.length_append]
    omega

private theorem itemsUntil_append (entry : Parser α) (charsOf : α → List Char)
    (sep ending : String) (rest : List Char)
    (step : ∀ a as, entry (seriesChars charsOf sep ending (a :: as) ++ rest) =
      some (a, (if as.isEmpty then [] else sep.toList) ++
        (seriesChars charsOf sep ending as ++ rest)))
    (missAfterSep : ∀ a as, consume ending.toList
      (sep.toList ++ (seriesChars charsOf sep ending (a :: as) ++ rest)) = none)
    (a : α) (as : List α) (fuel : Nat) (enough : as.length + 1 ≤ fuel) :
    itemsUntil entry sep ending fuel (seriesChars charsOf sep ending (a :: as) ++ rest) =
      some (a :: as, rest) := by
  induction as generalizing a fuel with
  | nil =>
    cases fuel with
    | zero => omega
    | succ fuel =>
      rw [itemsUntil, bind_run, step, Option.bind_some]
      simp [seriesChars, alt, keyword_append]
  | cons b bs ih =>
    cases fuel with
    | zero => omega
    | succ fuel =>
      have bound : bs.length + 1 ≤ fuel := by simp only [List.length_cons] at enough; omega
      simp [itemsUntil, bind_run, step, alt, keyword_fail _ _ _ (missAfterSep b bs),
        literal_append, pure_run, ih b fuel bound]

/-- The closing token wins at the start and after each entry. The two failure
conditions ensure it wins exactly at the end of the rendered list. -/
private theorem series_append (entry : Parser α) (charsOf : α → List Char)
    (sep ending : String) (rest : List Char)
    (nonempty : ∀ a, charsOf a ≠ [])
    (step : ∀ a as, entry (seriesChars charsOf sep ending (a :: as) ++ rest) =
      some (a, (if as.isEmpty then [] else sep.toList) ++
        (seriesChars charsOf sep ending as ++ rest)))
    (start : ∀ a as, consume ending.toList
      (seriesChars charsOf sep ending (a :: as) ++ rest) = none)
    (missAfterSep : ∀ a as, consume ending.toList
      (sep.toList ++ (seriesChars charsOf sep ending (a :: as) ++ rest)) = none)
    (xs : List α) :
    series entry sep ending (seriesChars charsOf sep ending xs ++ rest) = some (xs, rest) := by
  cases xs with
  | nil => simp [series, seriesChars, alt, keyword_append]
  | cons a as =>
    have enough := seriesChars_length charsOf sep ending nonempty (a :: as)
    simp only [List.length_cons] at enough
    simp only [series, alt, keyword_fail _ _ _ (start a as), Option.orElse_none]
    apply itemsUntil_append entry charsOf sep ending rest step missAfterSep
    simp only [List.length_append]
    omega

private theorem chars_items (items : List Item) :
    (render items).toList = items.flatMap (fun i => chars i.pieces) := by
  induction items with
  | nil => rfl
  | cons i items ih =>
    change chars (i.pieces ++ items.flatMap Item.pieces) = _
    rw [chars_append]
    exact congrArg (chars i.pieces ++ ·) ih

private theorem moduleItems_append (items : List Item) (fuel : Nat)
    (consumeItem : ∀ i ∈ items, ∀ rest, item (chars i.pieces ++ rest) = some (i, rest))
    (nonempty : ∀ i ∈ items, chars i.pieces ≠ [])
    (enough : items.length ≤ fuel) :
    moduleItems fuel (items.flatMap (fun i => chars i.pieces)) = some items := by
  induction items generalizing fuel with
  | nil => simp [moduleItems]
  | cons i items ih =>
    cases fuel with
    | zero => simp at enough
    | succ fuel =>
      have first := consumeItem i (by simp) (items.flatMap fun i => chars i.pieces)
      have tail := ih fuel (fun j hj => consumeItem j (by simp [hj]))
        (fun j hj => nonempty j (by simp [hj])) (by simpa using enough)
      have headNonempty := nonempty i (by simp)
      cases hc : chars i.pieces with
      | nil => exact (headNonempty hc).elim
      | cons c cs =>
        simp only [List.flatMap_cons, hc, List.cons_append] at first ⊢
        rw [moduleItems.eq_3, first]
        change (moduleItems fuel (items.flatMap fun i => chars i.pieces)).bind _ = _
        rw [tail]
        rfl

private theorem item_count_le_chars (items : List Item)
    (nonempty : ∀ i ∈ items, chars i.pieces ≠ []) :
    items.length ≤ (items.flatMap (fun i => chars i.pieces)).length := by
  induction items with
  | nil => simp
  | cons i items ih =>
    have positive := List.length_pos_iff.mpr (nonempty i (by simp))
    have tail := ih (fun j hj => nonempty j (by simp [hj]))
    simp only [List.length_cons, List.flatMap_cons, List.length_append]
    omega

/-- List composition for the assembly reader. The item consumption hypotheses
are obligations for its callers. -/
private theorem inventory_of_item_consumption (items : List Item)
    (consumeItem : ∀ i ∈ items, ∀ rest, item (chars i.pieces ++ rest) = some (i, rest))
    (nonempty : ∀ i ∈ items, chars i.pieces ≠ []) :
    inventory (render items) = some (items.flatMap Item.inventory) := by
  unfold inventory parse
  rw [chars_items, moduleItems_append items _ consumeItem nonempty (item_count_le_chars items nonempty)]
  rfl

private theorem invalid_first (c : Char) (rest : List Char)
    (bad : item (c :: rest) = none) :
    inventory (String.ofList (c :: rest)) = none := by
  unfold inventory parse
  rw [String.toList_ofList, List.length_cons, moduleItems.eq_3]
  change ((item (c :: rest)).bind _).map _ = _
  rw [bad]
  rfl

private theorem malformed_inventory (input : String) (c : Char) (rest : List Char)
    (spell : input = String.ofList (c :: rest)) (bad : item (c :: rest) = none) :
    inventory input = none := spell ▸ invalid_first c rest bad

theorem inventory_malformed_returns :
    inventory "  ret i32 02\n" = none ∧
    inventory "  ret i32 zeroinitializer\n" = none ∧
    inventory "  ret i32 -0\n" = none ∧
    inventory "  ret i32 1_7\n" = none := by
  repeat' apply And.intro
  all_goals apply malformed_inventory _ ' ' _ rfl
  all_goals rfl

theorem inventory_malformed_additions :
    inventory "  %r0 = add i8 %r1,-17\n" = none ∧
    inventory "  %r0 = add i8 2_ 56\n" = none := by
  constructor <;> apply malformed_inventory _ ' ' _ rfl <;> rfl

end Assembly

end Concrete.Backend.LLVM
