import Concrete.LLVM

namespace Concrete

/-! ## EmitLLVM — LLVM IR text printer

Pure function: LLVMModule → String.
Single concern: formatting structured LLVM IR as valid LLVM assembly text.
-/

-- ============================================================
-- Type printing
-- ============================================================

partial def printLLVMTy : LLVMTy → String
  | .void => "void"
  | .i1 => "i1"
  | .i8 => "i8"
  | .i16 => "i16"
  | .i32 => "i32"
  | .i64 => "i64"
  | .float_ => "float"
  | .double => "double"
  | .ptr => "ptr"
  | .array size elem => s!"[{size} x {printLLVMTy elem}]"
  | .struct_ name => s!"%struct.{name}"
  | .enum_ name => s!"%enum.{name}"
  | .packedStruct fields =>
    let parts := fields.map printLLVMTy
    "<{ " ++ ", ".intercalate parts ++ " }>"
  | .fnTy ret params variadic =>
    let paramStrs := params.map printLLVMTy
    let paramStr := if variadic then ", ".intercalate paramStrs ++ ", ..." else ", ".intercalate paramStrs
    printLLVMTy ret ++ " (" ++ paramStr ++ ")"

-- ============================================================
-- Operand printing
-- ============================================================

def printLLVMOperand : LLVMOperand → String
  | .reg name => "%" ++ name
  | .intLit val => toString val
  | .floatLit val =>
    let str := toString val
    if str.any (· == '.') || str.any (· == 'e') || str.any (· == 'E') then str else str ++ ".0"
  | .boolLit b => if b then "1" else "0"
  | .null_ => "null"
  | .global name => "@" ++ name
  | .undef => "undef"

/-- Print a typed operand: `<ty> <operand>` -/
def printTypedOperand (ty : LLVMTy) (op : LLVMOperand) : String :=
  printLLVMTy ty ++ " " ++ printLLVMOperand op

-- ============================================================
-- Binary operation printing
-- ============================================================

private def printBinOp : LLVMBinOp → String
  | .add => "add" | .sub => "sub" | .mul => "mul"
  | .sdiv => "sdiv" | .udiv => "udiv" | .srem => "srem" | .urem => "urem"
  | .icmpEq => "icmp eq" | .icmpNe => "icmp ne"
  | .icmpSlt => "icmp slt" | .icmpSgt => "icmp sgt"
  | .icmpSle => "icmp sle" | .icmpSge => "icmp sge"
  | .icmpUlt => "icmp ult" | .icmpUgt => "icmp ugt"
  | .icmpUle => "icmp ule" | .icmpUge => "icmp uge"
  | .fadd => "fadd" | .fsub => "fsub" | .fmul => "fmul"
  | .fdiv => "fdiv" | .frem => "frem"
  | .fcmpOeq => "fcmp oeq" | .fcmpUne => "fcmp une"
  | .fcmpOlt => "fcmp olt" | .fcmpOgt => "fcmp ogt"
  | .fcmpOle => "fcmp ole" | .fcmpOge => "fcmp oge"
  | .and_ => "and" | .or_ => "or" | .xor_ => "xor"
  | .shl => "shl" | .ashr => "ashr" | .lshr => "lshr"

private def printCastOp : LLVMCastOp → String
  | .sext => "sext" | .zext => "zext" | .trunc => "trunc"
  | .sitofp => "sitofp" | .uitofp => "uitofp"
  | .fptosi => "fptosi" | .fptoui => "fptoui"
  | .fpext => "fpext" | .fptrunc => "fptrunc"
  | .ptrtoint => "ptrtoint" | .inttoptr => "inttoptr"
  | .bitcast => "bitcast"

-- ============================================================
-- Instruction printing
-- ============================================================

private def printInstr (inst : LLVMInstr) : String :=
  match inst with
  | .binOp dst op ty lhs rhs =>
    s!"  %{dst} = {printBinOp op} {printLLVMTy ty} {printLLVMOperand lhs}, {printLLVMOperand rhs}"
  | .fneg dst ty operand =>
    s!"  %{dst} = fneg {printLLVMTy ty} {printLLVMOperand operand}"
  | .call dst retTy target args =>
    let argStr := ", ".intercalate (args.map fun (t, o) => printTypedOperand t o)
    let retStr := printLLVMTy retTy
    let targetStr := printLLVMOperand target
    match dst with
    | some d => s!"  %{d} = call {retStr} {targetStr}({argStr})"
    | none => s!"  call {retStr} {targetStr}({argStr})"
  | .callVariadic dst retTy target args fnTyParams =>
    let argStr := ", ".intercalate (args.map fun (t, o) => printTypedOperand t o)
    let retStr := printLLVMTy retTy
    let targetStr := printLLVMOperand target
    let paramStr := ", ".intercalate (fnTyParams.map printLLVMTy)
    let sigStr := if paramStr.isEmpty then "..." else paramStr ++ ", ..."
    match dst with
    | some d => s!"  %{d} = call {retStr} ({sigStr}) {targetStr}({argStr})"
    | none => s!"  call {retStr} ({sigStr}) {targetStr}({argStr})"
  | .alloca dst ty =>
    s!"  %{dst} = alloca {printLLVMTy ty}"
  | .load dst ty src =>
    s!"  %{dst} = load {printLLVMTy ty}, ptr {printLLVMOperand src}"
  | .store ty val dst =>
    s!"  store {printTypedOperand ty val}, ptr {printLLVMOperand dst}"
  | .gep dst baseTy base indices =>
    let idxStr := ", ".intercalate (indices.map fun (t, o) => printTypedOperand t o)
    s!"  %{dst} = getelementptr {printLLVMTy baseTy}, ptr {printLLVMOperand base}, {idxStr}"
  | .phi dst ty incoming =>
    let pairs := incoming.map fun (v, lbl) => s!"[{printLLVMOperand v}, %{lbl}]"
    s!"  %{dst} = phi {printLLVMTy ty} {", ".intercalate pairs}"
  | .cast dst op srcTy val dstTy =>
    s!"  %{dst} = {printCastOp op} {printTypedOperand srcTy val} to {printLLVMTy dstTy}"
  | .memcpy dst src size =>
    s!"  call void @llvm.memcpy.p0.p0.i64(ptr {printLLVMOperand dst}, ptr {printLLVMOperand src}, i64 {size}, i1 false)"
  | .select dst cond ty thenVal elseVal =>
    s!"  %{dst} = select i1 {printLLVMOperand cond}, {printTypedOperand ty thenVal}, {printTypedOperand ty elseVal}"
  | .raw line => line
  | .comment text => s!"  ; {text}"

-- ============================================================
-- Terminator printing
-- ============================================================

private def printTerm : LLVMTerm → String
  | .ret ty (some val) =>
    if ty == .void then "  ret void"
    else s!"  ret {printTypedOperand ty val}"
  | .ret _ none => "  ret void"
  | .br label => s!"  br label %{label}"
  | .condBr cond thenLabel elseLabel =>
    s!"  br i1 {printLLVMOperand cond}, label %{thenLabel}, label %{elseLabel}"
  | .unreachable => "  unreachable"

-- ============================================================
-- Block printing
-- ============================================================

private def printBlock (b : LLVMBlock) : String :=
  let instLines := b.instrs.map printInstr
  let termLine := printTerm b.term
  s!"{b.label}:\n" ++ "\n".intercalate instLines ++ (if instLines.isEmpty then "" else "\n") ++ termLine

-- ============================================================
-- Function printing
-- ============================================================

private def printFnDecl (d : LLVMFnDecl) : String :=
  let paramStr := ", ".intercalate (d.params.map printLLVMTy)
  let paramStr := if d.variadic then (if paramStr.isEmpty then "..." else paramStr ++ ", ...") else paramStr
  s!"declare {printLLVMTy d.retTy} @{d.name}({paramStr})"

private def printFnDef (f : LLVMFnDef) : String :=
  let paramStr := ", ".intercalate (f.params.map fun (n, t) => printLLVMTy t ++ " %" ++ n)
  let blockStrs := f.blocks.map printBlock
  let inlineAttr := if f.alwaysInline then " alwaysinline" else ""
  s!"define {printLLVMTy f.retTy} @{f.name}({paramStr}){inlineAttr} \{\n" ++
    "\n".intercalate blockStrs ++ "\n}\n"

-- ============================================================
-- Global printing
-- ============================================================

private def printGlobal (g : LLVMGlobal) : String :=
  let kind := if g.mutable then "global" else "constant"
  s!"@{g.name} = {g.linkage} {kind} {printLLVMTy g.ty} {g.value}"

-- ============================================================
-- Module printing
-- ============================================================

/-- Print a complete LLVM module to text. -/
def printLLVMModule (m : LLVMModule) : String :=
  let parts : List String := []
  -- Header comments
  let parts := parts ++ m.header.map (· ++ "\n")
  -- Type definitions
  let parts := if m.typeDefs.isEmpty then parts
    else parts ++ m.typeDefs.map (·.line ++ "\n")
  -- Globals
  let parts := if m.globals.isEmpty then parts
    else parts ++ m.globals.map (fun g => printGlobal g ++ "\n")
  -- Declarations
  let parts := if m.declarations.isEmpty then parts
    else parts ++ m.declarations.map (fun d => printFnDecl d ++ "\n")
  -- Functions
  let parts := if m.functions.isEmpty then parts
    else parts ++ m.functions.map (fun f => printFnDef f ++ "\n")
  -- Join everything
  "".intercalate parts

end Concrete
