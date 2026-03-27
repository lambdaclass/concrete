namespace Concrete

/-! ## LLVM — Structured LLVM IR representation

A typed AST for LLVM IR that replaces direct string concatenation in the backend.
The SSA→LLVM translation builds this structure, then a separate printer emits text.

Benefits over string emission:
- Type-safe construction (invalid IR is harder to produce)
- Inspectable/transformable before text output
- Single concern for text formatting (in EmitLLVM.lean)
- Foundation for future serialization, caching, and backend plurality
-/

-- ============================================================
-- LLVM Types
-- ============================================================

/-- LLVM IR types. Opaque pointers (just `ptr`) per LLVM 15+. -/
inductive LLVMTy where
  | void
  | i1
  | i8
  | i16
  | i32
  | i64
  | float_
  | double
  | ptr
  | array (size : Nat) (elem : LLVMTy)
  | struct_ (name : String)          -- named struct: %struct.Foo
  | enum_ (name : String)            -- named enum: %enum.Foo
  | packedStruct (fields : List LLVMTy) -- anonymous packed struct: <{ ... }>
  | fnTy (ret : LLVMTy) (params : List LLVMTy) (variadic : Bool := false)
  deriving BEq, Inhabited, Repr

-- ============================================================
-- LLVM Operands
-- ============================================================

/-- An LLVM operand: a register, constant, or global reference. -/
inductive LLVMOperand where
  | reg (name : String)
  | intLit (val : Int)
  | floatLit (val : Float)
  | boolLit (val : Bool)
  | null_
  | global (name : String)
  | undef
  deriving BEq, Inhabited, Repr

-- ============================================================
-- LLVM Instructions
-- ============================================================

/-- Binary integer/float operations. -/
inductive LLVMBinOp where
  -- Integer arithmetic
  | add | sub | mul | sdiv | udiv | srem | urem
  -- Integer comparison
  | icmpEq | icmpNe | icmpSlt | icmpSgt | icmpSle | icmpSge
  | icmpUlt | icmpUgt | icmpUle | icmpUge
  -- Float arithmetic
  | fadd | fsub | fmul | fdiv | frem
  -- Float comparison
  | fcmpOeq | fcmpUne | fcmpOlt | fcmpOgt | fcmpOle | fcmpOge
  -- Bitwise
  | and_ | or_ | xor_
  | shl | ashr | lshr
  deriving BEq, Inhabited, Repr

/-- Cast operations. -/
inductive LLVMCastOp where
  | sext | zext | trunc
  | sitofp | uitofp | fptosi | fptoui
  | fpext | fptrunc
  | ptrtoint | inttoptr
  | bitcast
  deriving BEq, Inhabited, Repr

/-- A single LLVM IR instruction. -/
inductive LLVMInstr where
  /-- `%dst = <op> <ty> <lhs>, <rhs>` -/
  | binOp (dst : String) (op : LLVMBinOp) (ty : LLVMTy) (lhs rhs : LLVMOperand)
  /-- `%dst = fneg <ty> <operand>` -/
  | fneg (dst : String) (ty : LLVMTy) (operand : LLVMOperand)
  /-- `%dst = call <retTy> <target>(<args>)` or `call void <target>(<args>)` -/
  | call (dst : Option String) (retTy : LLVMTy) (target : LLVMOperand) (args : List (LLVMTy × LLVMOperand))
  /-- `%dst = call <retTy> (<fnTyParams>, ...) <target>(<args>)` — variadic call (printf etc.)
      `fnTyParams` lists the non-variadic parameter types for the function type signature.
      Default `[.ptr]` matches printf's `(ptr, ...)`. -/
  | callVariadic (dst : Option String) (retTy : LLVMTy) (target : LLVMOperand) (args : List (LLVMTy × LLVMOperand)) (fnTyParams : List LLVMTy := [.ptr])
  /-- `%dst = alloca <ty>` -/
  | alloca (dst : String) (ty : LLVMTy)
  /-- `%dst = load <ty>, ptr <src>` -/
  | load (dst : String) (ty : LLVMTy) (src : LLVMOperand)
  /-- `store <ty> <val>, ptr <dst>` -/
  | store (ty : LLVMTy) (val : LLVMOperand) (dst : LLVMOperand)
  /-- `%dst = getelementptr <baseTy>, ptr <base>, <indices>` -/
  | gep (dst : String) (baseTy : LLVMTy) (base : LLVMOperand) (indices : List (LLVMTy × LLVMOperand))
  /-- `%dst = phi <ty> [<val1>, %<lbl1>], ...` -/
  | phi (dst : String) (ty : LLVMTy) (incoming : List (LLVMOperand × String))
  /-- `%dst = <castOp> <srcTy> <val> to <dstTy>` -/
  | cast (dst : String) (op : LLVMCastOp) (srcTy : LLVMTy) (val : LLVMOperand) (dstTy : LLVMTy)
  /-- `call void @llvm.memcpy.p0.p0.i64(ptr <dst>, ptr <src>, i64 <size>, i1 false)` -/
  | memcpy (dst src : LLVMOperand) (size : Nat)
  /-- `%dst = select i1 <cond>, <ty> <thenVal>, <ty> <elseVal>` -/
  | select (dst : String) (cond : LLVMOperand) (ty : LLVMTy) (thenVal elseVal : LLVMOperand)
  /-- A raw LLVM IR line (escape hatch for builtins, format strings, etc.) -/
  | raw (line : String)
  /-- Comment line: `; <text>` -/
  | comment (text : String)
  deriving Inhabited, Repr

-- ============================================================
-- Block Terminators
-- ============================================================

/-- Block terminators. -/
inductive LLVMTerm where
  | ret (ty : LLVMTy) (val : Option LLVMOperand)
  | br (label : String)
  | condBr (cond : LLVMOperand) (thenLabel elseLabel : String)
  | unreachable
  deriving Inhabited, Repr

-- ============================================================
-- Basic Blocks and Functions
-- ============================================================

structure LLVMBlock where
  label : String
  instrs : List LLVMInstr
  term : LLVMTerm
  deriving Inhabited, Repr

structure LLVMFnDecl where
  name : String
  retTy : LLVMTy
  params : List LLVMTy
  variadic : Bool := false
  deriving Inhabited, Repr

structure LLVMFnDef where
  name : String
  retTy : LLVMTy
  params : List (String × LLVMTy)
  blocks : List LLVMBlock
  alwaysInline : Bool := false
  deriving Inhabited, Repr

-- ============================================================
-- Global Definitions
-- ============================================================

/-- A global constant (string literal, format string, etc.) -/
structure LLVMGlobal where
  name : String
  ty : LLVMTy
  value : String    -- raw LLVM constant expression (e.g. `c"hello\00"`)
  linkage : String := "private"
  mutable : Bool := false  -- if true, emit `global` instead of `constant`
  deriving Inhabited, Repr

/-- A named type definition (struct or enum). -/
structure LLVMTypeDef where
  /-- Raw LLVM type definition line, e.g. `%struct.Foo = type { i64, i64 }` -/
  line : String
  deriving Inhabited, Repr

-- ============================================================
-- Module
-- ============================================================

structure LLVMModule where
  header : List String := []
  typeDefs : List LLVMTypeDef := []
  globals : List LLVMGlobal := []
  declarations : List LLVMFnDecl := []
  functions : List LLVMFnDef := []
  deriving Inhabited, Repr

end Concrete
