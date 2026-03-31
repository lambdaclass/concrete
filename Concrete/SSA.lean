import Concrete.AST
import Concrete.Core

namespace Concrete

/-! ## SSA IR — explicit control flow, registers, phi nodes

Represents programs as a control-flow graph with basic blocks.
No structured control flow (if/while are gone — replaced by conditional branches).
-/

-- ============================================================
-- SSA Values
-- ============================================================

inductive SVal where
  | reg (name : String) (ty : Ty)
  | intConst (val : Int) (ty : Ty)
  | floatConst (val : Float) (ty : Ty)
  | boolConst (val : Bool)
  | strConst (val : String)
  | strConstRef (val : String)  -- borrowed string literal: no heap alloc, points at global
  | unit

-- ============================================================
-- SSA Instructions
-- ============================================================

inductive SInst where
  | binOp (dst : String) (op : BinOp) (lhs rhs : SVal) (ty : Ty)
  | unaryOp (dst : String) (op : UnaryOp) (operand : SVal) (ty : Ty)
  | call (dst : Option String) (fn : String) (args : List SVal) (retTy : Ty)
  | alloca (dst : String) (ty : Ty)
  | load (dst : String) (ptr : SVal) (ty : Ty)
  | store (val : SVal) (ptr : SVal)
  | gep (dst : String) (base : SVal) (indices : List SVal) (ty : Ty)
  | phi (dst : String) (incoming : List (SVal × String)) (ty : Ty)
  | cast (dst : String) (val : SVal) (targetTy : Ty)
  | memcpy (dst src : SVal) (size : Nat)

-- ============================================================
-- Block terminators
-- ============================================================

inductive STerm where
  | ret (val : Option SVal)
  | br (label : String)
  | condBr (cond : SVal) (thenLabel elseLabel : String)
  | unreachable

-- ============================================================
-- Basic blocks and functions
-- ============================================================

structure SBlock where
  label : String
  params : List (String × Ty) := []
  insts : List SInst
  term : STerm

structure SFnDef where
  name : String
  params : List (String × Ty)
  retTy : Ty
  blocks : List SBlock
  isTest : Bool := false
  isEntryPoint : Bool := false
  modulePath : String := ""

structure SModule where
  name : String
  structs : List CStructDef
  enums : List CEnumDef
  functions : List SFnDef
  externFns : List (String × List (String × Ty) × Ty)
  globals : List (String × String)
  /-- Maps local alias name → original linker symbol for aliased imports. -/
  linkerAliases : List (String × String) := []

-- ============================================================
-- SVal.ty accessor
-- ============================================================

def SVal.ty : SVal → Ty
  | .reg _ t => t
  | .intConst _ t => t
  | .floatConst _ t => t
  | .boolConst _ => .bool
  | .strConst _ => .string
  | .strConstRef _ => .ref .string
  | .unit => .unit

-- ============================================================
-- Pretty-printer
-- ============================================================

private def ssaTyToStr : Ty → String
  | .int => "i64"
  | .uint => "u64"
  | .i8 => "i8"
  | .i16 => "i16"
  | .i32 => "i32"
  | .u8 => "u8"
  | .u16 => "u16"
  | .u32 => "u32"
  | .bool => "i1"
  | .float64 => "f64"
  | .float32 => "f32"
  | .char => "i8"
  | .unit => "void"
  | .named n => s!"%{n}"
  | .string => "%String"
  | .ref inner => s!"ptr({ssaTyToStr inner})"
  | .refMut inner => s!"ptr(mut {ssaTyToStr inner})"
  | .generic n args => s!"%{n}<{", ".intercalate (args.map ssaTyToStr)}>"
  | .typeVar n => s!"%{n}"
  | .array elem size => s!"[{size} x {ssaTyToStr elem}]"
  | .ptrMut _ => "ptr"
  | .ptrConst _ => "ptr"
  | .fn_ params _ ret => s!"fn({", ".intercalate (params.map ssaTyToStr)}) -> {ssaTyToStr ret}"
  | .never => "void"
  | .heap inner => s!"ptr({ssaTyToStr inner})"
  | .heapArray inner => s!"ptr({ssaTyToStr inner})"
  | .placeholder => "?"

private def ppSVal : SVal → String
  | .reg n _ => s!"%{n}"
  | .intConst v _ => toString v
  | .floatConst v _ => toString v
  | .boolConst b => if b then "1" else "0"
  | .strConst s => s!"@str.{s.hash}"
  | .strConstRef s => s!"&@str.{s.hash}"
  | .unit => "void"

private def binOpToStr : BinOp → String
  | .add => "add" | .sub => "sub" | .mul => "mul" | .div => "sdiv" | .mod => "srem"
  | .eq => "eq" | .neq => "ne" | .lt => "slt" | .gt => "sgt" | .leq => "sle" | .geq => "sge"
  | .and_ => "and" | .or_ => "or"
  | .bitand => "and" | .bitor => "or" | .bitxor => "xor" | .shl => "shl" | .shr => "ashr"

private def unaryOpToStr : UnaryOp → String
  | .neg => "neg" | .not_ => "not" | .bitnot => "not"

private def ppSInst (inst : SInst) : String :=
  match inst with
  | .binOp dst op lhs rhs ty =>
    s!"  %{dst} = {binOpToStr op} {ssaTyToStr ty} {ppSVal lhs}, {ppSVal rhs}"
  | .unaryOp dst op operand ty =>
    s!"  %{dst} = {unaryOpToStr op} {ssaTyToStr ty} {ppSVal operand}"
  | .call (some dst) fn args retTy =>
    let argsStr := args.map fun a => s!"{ssaTyToStr a.ty} {ppSVal a}"
    s!"  %{dst} = call {ssaTyToStr retTy} @{fn}({", ".intercalate argsStr})"
  | .call none fn args retTy =>
    let argsStr := args.map fun a => s!"{ssaTyToStr a.ty} {ppSVal a}"
    s!"  call {ssaTyToStr retTy} @{fn}({", ".intercalate argsStr})"
  | .alloca dst ty =>
    s!"  %{dst} = alloca {ssaTyToStr ty}"
  | .load dst ptr ty =>
    s!"  %{dst} = load {ssaTyToStr ty}, {ppSVal ptr}"
  | .store val ptr =>
    s!"  store {ssaTyToStr val.ty} {ppSVal val}, {ppSVal ptr}"
  | .gep dst base indices ty =>
    let idxStr := indices.map fun i => s!"{ssaTyToStr i.ty} {ppSVal i}"
    s!"  %{dst} = gep {ssaTyToStr ty} {ppSVal base}, {", ".intercalate idxStr}"
  | .phi dst incoming ty =>
    let pairs := incoming.map fun (v, lbl) => s!"[{ppSVal v}, %{lbl}]"
    s!"  %{dst} = phi {ssaTyToStr ty} {", ".intercalate pairs}"
  | .cast dst val tgt =>
    s!"  %{dst} = cast {ssaTyToStr val.ty} {ppSVal val} to {ssaTyToStr tgt}"
  | .memcpy dst src size =>
    s!"  memcpy {ppSVal dst}, {ppSVal src}, {size}"

private def ppSTerm (t : STerm) : String :=
  match t with
  | .ret (some v) => s!"  ret {ssaTyToStr v.ty} {ppSVal v}"
  | .ret none => "  ret void"
  | .br lbl => s!"  br label %{lbl}"
  | .condBr cond tl el => s!"  br i1 {ppSVal cond}, label %{tl}, label %{el}"
  | .unreachable => "  unreachable"

private def ppSBlock (b : SBlock) : String :=
  let instsStr := b.insts.map ppSInst
  let termStr := ppSTerm b.term
  s!"{b.label}:\n{"\n".intercalate instsStr}\n{termStr}"

def ppSFnDef (f : SFnDef) : String :=
  let paramsStr := f.params.map fun (n, t) => s!"{ssaTyToStr t} %{n}"
  let blocksStr := f.blocks.map ppSBlock
  s!"define {ssaTyToStr f.retTy} @{f.name}({", ".intercalate paramsStr}) \{\n{"\n".intercalate blocksStr}\n}"

def ppSModule (m : SModule) : String :=
  let parts : List String := []
  let parts := parts ++ m.globals.map fun (name, val) =>
    s!"@{name} = private constant \"{val}\""
  let parts := parts ++ m.externFns.map fun (n, ps, rt) =>
    let paramsStr := ps.map fun (_, pt) => ssaTyToStr pt
    s!"declare {ssaTyToStr rt} @{n}({", ".intercalate paramsStr})"
  let parts := parts ++ m.functions.map ppSFnDef
  s!"; module {m.name}\n{"\n\n".intercalate parts}"

end Concrete
