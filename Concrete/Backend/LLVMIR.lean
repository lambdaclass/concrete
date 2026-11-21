import Concrete.Backend.Decimal
import ConcreteSpec.Spec.Scalar

namespace Concrete.Backend.LLVM

open ConcreteSpec.Core (IntTy toBits)

/-- LLVM-specific counts and result words are naturals. Concrete scalar operands
carry their Core type and mathematical value until rendering. -/
inductive Immediate where
  | natural (value : Nat)
  | scalar (ty : IntTy) (value : Int)

def Immediate.value : Immediate → Nat
  | .natural n => n
  | .scalar t v => toBits t v

def Immediate.token (v : Immediate) : String := decimal v.value

inductive Arithmetic where
  | add | sub | mul

inductive Conversion where
  | trunc | sext | zext

inductive Comparison where
  | eq | sle | ult

inductive TypeCode where
  | integer (ty : IntTy)
  | flag | ptr | void
  | overflowPair (ty : IntTy)

inductive Global where
  | body | main | writeAll | write | exit | abort | bad
  | site (index : Nat)
  | overflow (op : Arithmetic) (ty : IntTy)

inductive Label where
  | block (index : Nat)
  | trapSite (index : Nat)
  | entry | loop | out | wr | more | exit | trap | bad

inductive Local where
  | register (index : Nat)
  | overflow (index : Nat)
  | result (block : Nat)
  | p | n | pp | np | nn | rest | done | k | bad | w | ok | i

inductive Argument where
  | local (name : Local)
  | global (name : Global)
  | immediate (value : Immediate)

abbrev TypedArgument := TypeCode × Argument

inductive Instruction where
  | bin (dst : Local) (op : Arithmetic) (ty : TypeCode) (lhs rhs : Argument)
  | convert (dst : Local) (op : Conversion) (source : TypedArgument) (target : TypeCode)
  | call (dst : Option Local) (result : TypeCode) (fn : Global) (args : List TypedArgument)
  | extract (dst : Local) (ty : TypeCode) (aggregate : Local) (index : Nat)
  | phi (dst : Local) (ty : TypeCode) (arms : List (Argument × Label))
  | compare (dst : Local) (op : Comparison) (ty : TypeCode) (lhs rhs : Argument)
  | gep (dst : Local) (element : TypeCode) (base offset : TypedArgument)

inductive Terminator where
  | ret (arg : TypedArgument)
  | retVoid
  | branch (target : Label)
  | cond (flag : Local) (yes no : Label)
  | switch (ty : TypeCode) (value : Local) (fallback : Label) (cases : List (Immediate × Label))
  | unreachable

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
      text ".with.overflow." ++ typeCode (.integer ty)

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
    text "switch " ++ typeCode ty ++ arg (.local value) .comma ++ text " label %" ++ labelName fallback ++ text " [" ++
      (cases.flatMap fun (v, dst) => text " " ++ typed (ty, .immediate v) .comma ++ text " label %" ++ labelName dst) ++ text " ]\n"
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

end Concrete.Backend.LLVM
