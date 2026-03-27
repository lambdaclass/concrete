import Concrete.SSA
import Concrete.Core
import Concrete.Layout
import Concrete.Intrinsic
import Concrete.LLVM
import Concrete.EmitLLVM
import Concrete.EmitBuiltins

namespace Concrete

/-! ## EmitSSA — SSA→LLVM IR codegen

New codegen that walks SModule / SFnDef / SBlock / SInst / STerm and emits LLVM IR text.
Fundamentally simpler than the AST-based codegen because:
- No structured control flow to lower (already done by Lower.lean)
- No type inference needed (every SVal carries its Ty)
- No monomorphization (already done by Mono.lean)
- Direct 1:1 mapping from SSA instructions to LLVM IR
-/

-- ============================================================
-- Emit state
-- ============================================================

structure EmitSSAState where
  /-- Instructions for the current block being built. -/
  currentInstrs : Array LLVMInstr := #[]
  /-- Completed blocks for the current function being built. -/
  currentBlocks : Array LLVMBlock := #[]
  /-- Completed function definitions. -/
  moduleFunctions : Array LLVMFnDef := #[]
  /-- Module header comment lines. -/
  moduleHeader : Array String := #[]
  /-- Structured type definitions (%struct.Foo, %enum.Bar, etc.). -/
  moduleTypeDefs : Array LLVMTypeDef := #[]
  /-- Structured global constants (string literals, format strings). -/
  moduleGlobals : Array LLVMGlobal := #[]
  /-- Structured extern function declarations. -/
  moduleDeclarations : Array LLVMFnDecl := #[]
  structDefs : List CStructDef := []
  enumDefs : List CEnumDef := []
  stringLitCounter : Nat := 0
  localCounter : Nat := 0
  /-- Registers known to be LLVM pointers (alloca/gep/struct params). -/
  ptrRegs : List String := []
  /-- Type names already emitted (for dedup across modules). -/
  emittedTypes : List String := []
  /-- String literal name → length (for building %struct.String at use sites). -/
  stringLengths : List (String × Nat) := []
  /-- Parameter names of the current function (for indirect call detection). -/
  fnParams : List (String × Ty) := []
  /-- Registers holding function-pointer values loaded from memory (e.g. struct fields). -/
  fnTypeRegs : List String := []
  /-- Maps local alias name → original linker symbol for aliased imports. -/
  linkerAliases : List (String × String) := []
  /-- Distinct Vec element specs (elemSize, optionPayloadOffset) used in the program.
      Used to generate per-size vec builtin implementations. -/
  vecElemSpecs : List (Nat × Nat) := []
  /-- Names of extern functions (for C ABI calling convention at call sites). -/
  externFnNames : List String := []
  /-- Alloca instructions to hoist to the function entry block.
      Allocas emitted inside loop bodies would otherwise grow the stack
      on every iteration; collecting them here and prepending to the
      entry block makes LLVM reuse the same stack slot. -/
  entryAllocas : Array LLVMInstr := #[]

/-- Append a structured instruction to the current block. -/
private def emitStructured (s : EmitSSAState) (instr : LLVMInstr) : EmitSSAState :=
  { s with currentInstrs := s.currentInstrs.push instr }

/-- Record an alloca to be hoisted to the function entry block.
    This prevents stack growth when the alloca textually appears in a loop. -/
private def emitEntryAlloca (s : EmitSSAState) (instr : LLVMInstr) : EmitSSAState :=
  { s with entryAllocas := s.entryAllocas.push instr }

/-- Append a type definition to the module. -/
private def emitTypeDef (s : EmitSSAState) (line : String) : EmitSSAState :=
  { s with moduleTypeDefs := s.moduleTypeDefs.push { line := line } }

/-- Append a global constant to the module. -/
private def emitGlobal (s : EmitSSAState) (g : LLVMGlobal) : EmitSSAState :=
  { s with moduleGlobals := s.moduleGlobals.push g }

/-- Append an extern function declaration to the module. -/
private def emitDecl (s : EmitSSAState) (d : LLVMFnDecl) : EmitSSAState :=
  { s with moduleDeclarations := s.moduleDeclarations.push d }

private def freshLocal (s : EmitSSAState) : EmitSSAState × String :=
  let name := "%ssa.t" ++ toString s.localCounter
  ({ s with localCounter := s.localCounter + 1 }, name)

private def markPtr (s : EmitSSAState) (name : String) : EmitSSAState :=
  { s with ptrRegs := name :: s.ptrRegs }

private def isKnownPtr (s : EmitSSAState) (v : SVal) : Bool :=
  match v with
  | .reg name _ => s.ptrRegs.contains name
  | _ => false

-- ============================================================
-- Ty → LLVM type string
-- ============================================================

/-- Build a Layout.Ctx from the current emit state. -/
private def layoutCtxOf (s : EmitSSAState) : Layout.Ctx :=
  { structDefs := s.structDefs, enumDefs := s.enumDefs }

private def ssaLookupStruct (s : EmitSSAState) (name : String) : Option CStructDef :=
  Layout.lookupStruct (layoutCtxOf s) name

private def ssaLookupEnum (s : EmitSSAState) (name : String) : Option CEnumDef :=
  Layout.lookupEnum (layoutCtxOf s) name

/-- Is this type passed by pointer in function calls? Delegates to Layout.isPassByPtr. -/
private def ssaIsPassByPtr (s : EmitSSAState) (ty : Ty) : Bool :=
  Layout.isPassByPtr (layoutCtxOf s) ty


/-- Map a Concrete type to a structured LLVMTy. Mirrors Layout.tyToLLVM. -/
partial def tyToLLVMTy (s : EmitSSAState) : Ty → LLVMTy
  | .int | .uint => .i64
  | .i8 | .u8 => .i8
  | .i16 | .u16 => .i16
  | .i32 | .u32 => .i32
  | .bool => .i1
  | .float64 => .double
  | .float32 => .float_
  | .char => .i8
  | .unit | .never => .void
  | .string => .struct_ "String"
  | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ => .ptr
  | .generic "Heap" _ | .heap _ => .ptr
  | .generic "HeapArray" _ | .heapArray _ => .ptr
  | .generic "Vec" _ => .struct_ "Vec"
  | .generic "HashMap" _ => .struct_ "HashMap"
  | .generic name _ =>
    match ssaLookupEnum s name with
    | some _ => .enum_ name
    | none => .struct_ name
  | .typeVar _ => .i64
  | .array elem n => .array n (tyToLLVMTy s elem)
  | .fn_ _ _ _ => .ptr
  | .placeholder => .i64
  | .named name =>
    match ssaLookupStruct s name with
    | some _ => .struct_ name
    | none =>
      match ssaLookupEnum s name with
      | some _ => .enum_ name
      | none =>
        panic! s!"EmitSSA.tyToLLVMTy: unknown named type '{name}'"

/-- Map integer Concrete type to structured LLVM type. -/
private def intTyToLLVMTy : Ty → LLVMTy
  | .int | .uint => .i64
  | .i8 | .u8 | .char => .i8
  | .i16 | .u16 => .i16
  | .i32 | .u32 => .i32
  | .bool => .i1
  | _ => .i64

/-- Map float Concrete type to structured LLVM type. -/
private def floatTyToLLVMTy : Ty → LLVMTy
  | .float32 => .float_
  | _ => .double

/-- Convert an SVal to a structured LLVM operand. -/
private def svalToOperand (s : EmitSSAState) (v : SVal) : LLVMOperand :=
  match v with
  | .reg name _ =>
    if name.startsWith "@fnref." then
      let bareName := (name.drop 7).toString
      -- Resolve linker aliases for function pointer references (e.g., hash_i32 → hash_hash_i32)
      let resolved := match s.linkerAliases.lookup bareName with
        | some orig => orig
        | none => bareName
      .global resolved
    else .reg name
  | .intConst val _ => .intLit val
  | .floatConst val _ => .floatLit val
  | .boolConst b => .boolLit b
  | .strConst name => .global name
  | .strConstRef name => .global name  -- fallback; normally handled by ensurePtrOp
  | .unit => .undef

/-- LLVM type for function parameters (pass-by-ptr types → ptr). -/
private def paramTyToLLVMTy (s : EmitSSAState) (ty : Ty) : LLVMTy :=
  if ssaIsPassByPtr s ty then .ptr else tyToLLVMTy s ty

/-- Is this type a #[repr(C)] struct? -/
private def isReprCStruct (s : EmitSSAState) : Ty → Bool
  | .named name => (Layout.lookupStruct (layoutCtxOf s) name).any (·.isReprC)
  | _ => false

/-- LLVM type for extern function parameters: #[repr(C)] structs are passed by value
    (the actual LLVM struct type) so LLVM can apply the platform C calling convention.
    All other types use the normal paramTyToLLVMTy logic. -/
private def externParamTyToLLVMTy (s : EmitSSAState) (ty : Ty) : LLVMTy :=
  if isReprCStruct s ty then tyToLLVMTy s ty
  else paramTyToLLVMTy s ty

private def ssaIsSignedInt : Ty → Bool
  | .int | .i8 | .i16 | .i32 => true
  | _ => false

private def isIntegerTy : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 | .char | .bool => true
  | _ => false

private def isFloatTy : Ty → Bool
  | .float32 | .float64 => true
  | _ => false

/-- Get byte size of a type. Delegates to Layout.tySize with current state's defs. -/
private def ssaTySize (s : EmitSSAState) (ty : Ty) : Nat :=
  Layout.tySize (layoutCtxOf s) ty

/-- Get byte alignment of a type. -/
private def ssaTyAlign (s : EmitSSAState) (ty : Ty) : Nat :=
  Layout.tyAlign (layoutCtxOf s) ty

/-- Flatten a repr(C) struct parameter for the platform C ABI.
    ARM64 AAPCS: structs ≤ 8 bytes → one i64, 9-16 bytes → two i64s, > 16 bytes → ptr.
    Returns a list of LLVM types that replace the single struct parameter. -/
private def externABIFlattenParam (s : EmitSSAState) (ty : Ty) : List LLVMTy :=
  if !isReprCStruct s ty then [paramTyToLLVMTy s ty]
  else
    let size := ssaTySize s ty
    if size ≤ 8 then [.i64]
    else if size ≤ 16 then [.i64, .i64]
    else [.ptr]

/-- Flatten a repr(C) struct return type for the platform C ABI.
    ARM64: return structs ≤ 8 bytes in one i64, > 8 bytes as-is (handled by sret). -/
private def externABIFlattenRet (s : EmitSSAState) (ty : Ty) : LLVMTy :=
  if !isReprCStruct s ty then tyToLLVMTy s ty
  else
    let size := ssaTySize s ty
    if size ≤ 8 then .i64
    else tyToLLVMTy s ty

/-- Extract the Vec element type from a type like `Vec<T>`, `&Vec<T>`, or `&mut Vec<T>`. -/
private def vecElemTy : Ty → Option Ty
  | .generic "Vec" (t :: _) => some t
  | .ref (.generic "Vec" (t :: _)) => some t
  | .refMut (.generic "Vec" (t :: _)) => some t
  | _ => none

/-- The set of vec intrinsic names that need per-size specialization.
    vec_len and vec_free are size-independent and stay unspecialized. -/
private def vecSizedOps : List String :=
  ["vec_new", "vec_push", "vec_get", "vec_set", "vec_pop"]

/-- Resolve the Vec element size for a call to a vec intrinsic.
    Returns `(specializedName, elemSize, optionPayloadOffset)` or none if not a vec op.
    The payload offset is needed for vec_pop's Option construction. -/
private def resolveVecCall (s : EmitSSAState) (fn : String) (args : List SVal) (retTy : Ty) : Option (String × Nat × Nat) :=
  if !vecSizedOps.contains fn then none
  else
    -- Extract element type from args or return type
    let elemTy? : Option Ty :=
      if fn == "vec_new" then vecElemTy retTy
      else if fn == "vec_get" then some retTy
      else if fn == "vec_pop" then
        match retTy with
        | .generic _ (t :: _) => some t  -- Option<T> → T
        | _ => none
      else
        -- vec_push, vec_set: get from first arg (the Vec ref)
        match args.head? with
        | some v => vecElemTy v.ty
        | none => none
    match elemTy? with
    | some elemTy =>
      let sz := ssaTySize s elemTy
      let al := ssaTyAlign s elemTy
      let payOff := Layout.alignUp 4 al
      -- vec_pop is named by size_payoff (different alignments need different Option layouts)
      let name := if fn == "vec_pop" then s!"{fn}_{sz}_{payOff}" else s!"{fn}_{sz}"
      some (name, sz, payOff)
    | none => none

/-- Record a Vec element spec as used (for builtin generation). -/
private def recordVecElemSpec (s : EmitSSAState) (sz : Nat) (payOff : Nat) : EmitSSAState :=
  if s.vecElemSpecs.any fun (s, p) => s == sz && p == payOff then s
  else { s with vecElemSpecs := s.vecElemSpecs ++ [(sz, payOff)] }

private def ssaEscapeCharForLLVM (c : Char) : String :=
  if c == '\n' then "\\0A"
  else if c == '\t' then "\\09"
  else if c == '\\' then "\\5C"
  else if c == '"' then "\\22"
  else if c.toNat == 0 then "\\00"
  else if c.toNat >= 32 && c.toNat <= 126 then String.singleton c
  else
    let n := c.toNat
    let hi := n / 16
    let lo := n % 16
    let hexDigit (d : Nat) : Char :=
      if d < 10 then Char.ofNat (d + '0'.toNat)
      else Char.ofNat (d - 10 + 'A'.toNat)
    "\\" ++ String.ofList [hexDigit hi, hexDigit lo]

private def ssaEscapeStringForLLVM (str : String) : String :=
  str.foldl (fun acc c => acc ++ ssaEscapeCharForLLVM c) ""

-- ============================================================
-- Materialize string constants and ensure pointers
-- ============================================================

/-- Materialize a string constant as a %struct.String pointer.
    Allocates a %struct.String, stores {ptr to chars, length}, returns ptr. -/
private def materializeStrConst (s : EmitSSAState) (name : String) : EmitSSAState × String :=
  let strLen := (s.stringLengths.find? fun (n, _) => n == name).map (·.2) |>.getD 0
  let arrLen := strLen + 1  -- includes null terminator in the global
  -- GEP into the global char array
  let (s, gepTmp) := freshLocal s
  let gepName := (gepTmp.drop 1).toString
  let s := emitStructured s (.gep gepName (.array arrLen .i8) (.global name) [(.i32, .intLit 0), (.i32, .intLit 0)])
  -- Heap-allocate a copy so drop_string can safely free it
  let (s, heapBuf) := freshLocal s
  let heapName := (heapBuf.drop 1).toString
  let s := emitStructured s (.call (some heapName) .ptr (.global "malloc") [(.i64, .intLit arrLen)])
  let s := emitStructured s (.memcpy (.reg heapName) (.reg gepName) arrLen)
  -- Allocate %struct.String on stack
  let (s, strTmp) := freshLocal s
  let strName := (strTmp.drop 1).toString
  let s := emitEntryAlloca s (.alloca strName (.struct_ "String"))
  -- Store ptr field (index 0)
  let (s, ptrField) := freshLocal s
  let ptrFieldName := (ptrField.drop 1).toString
  let s := emitStructured s (.gep ptrFieldName (.struct_ "String") (.reg strName) [(.i32, .intLit 0), (.i32, .intLit 0)])
  let s := emitStructured s (.store .ptr (.reg heapName) (.reg ptrFieldName))
  -- Store len field (index 1)
  let (s, lenField) := freshLocal s
  let lenFieldName := (lenField.drop 1).toString
  let s := emitStructured s (.gep lenFieldName (.struct_ "String") (.reg strName) [(.i32, .intLit 0), (.i32, .intLit 1)])
  let s := emitStructured s (.store .i64 (.intLit strLen) (.reg lenFieldName))
  -- Store cap field (index 2)
  let (s, capField) := freshLocal s
  let capFieldName := (capField.drop 1).toString
  let s := emitStructured s (.gep capFieldName (.struct_ "String") (.reg strName) [(.i32, .intLit 0), (.i32, .intLit 2)])
  let s := emitStructured s (.store .i64 (.intLit arrLen) (.reg capFieldName))
  (s, strTmp)

/-- Materialize a borrowed string constant as a %struct.String pointer.
    Points directly at the global constant — no malloc, no memcpy.
    Cap is set to 0 to signal this is not a heap-owned buffer. -/
private def materializeStrConstRef (s : EmitSSAState) (name : String) : EmitSSAState × String :=
  let strLen := (s.stringLengths.find? fun (n, _) => n == name).map (·.2) |>.getD 0
  let arrLen := strLen + 1
  -- GEP into the global char array (read-only pointer)
  let (s, gepTmp) := freshLocal s
  let gepName := (gepTmp.drop 1).toString
  let s := emitStructured s (.gep gepName (.array arrLen .i8) (.global name) [(.i32, .intLit 0), (.i32, .intLit 0)])
  -- Allocate %struct.String on stack (no heap allocation)
  let (s, strTmp) := freshLocal s
  let strName := (strTmp.drop 1).toString
  let s := emitEntryAlloca s (.alloca strName (.struct_ "String"))
  -- Store ptr field (index 0) — points directly at global constant
  let (s, ptrField) := freshLocal s
  let ptrFieldName := (ptrField.drop 1).toString
  let s := emitStructured s (.gep ptrFieldName (.struct_ "String") (.reg strName) [(.i32, .intLit 0), (.i32, .intLit 0)])
  let s := emitStructured s (.store .ptr (.reg gepName) (.reg ptrFieldName))
  -- Store len field (index 1)
  let (s, lenField) := freshLocal s
  let lenFieldName := (lenField.drop 1).toString
  let s := emitStructured s (.gep lenFieldName (.struct_ "String") (.reg strName) [(.i32, .intLit 0), (.i32, .intLit 1)])
  let s := emitStructured s (.store .i64 (.intLit strLen) (.reg lenFieldName))
  -- Store cap field (index 2) — 0 signals non-owned / non-freeable
  let (s, capField) := freshLocal s
  let capFieldName := (capField.drop 1).toString
  let s := emitStructured s (.gep capFieldName (.struct_ "String") (.reg strName) [(.i32, .intLit 0), (.i32, .intLit 2)])
  let s := emitStructured s (.store .i64 (.intLit 0) (.reg capFieldName))
  (s, strTmp)

/-- If the SVal is not known to be a ptr but has a pass-by-ptr type,
    emit alloca+store to convert it. Returns (state, ptrString). -/
private def isRefOrPtrTy : Ty → Bool
  | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ => true
  | _ => false

/-- If the SVal is not known to be a ptr but has a pass-by-ptr type,
    emit alloca+store to convert it. Returns (state, LLVMOperand). -/
private def ensurePtrOp (s : EmitSSAState) (v : SVal) : EmitSSAState × LLVMOperand :=
  match v with
  | .strConst name =>
    let (s, strTmp) := materializeStrConst s name
    (s, .reg (strTmp.drop 1).toString)
  | .strConstRef name =>
    let (s, strTmp) := materializeStrConstRef s name
    (s, .reg (strTmp.drop 1).toString)
  | _ =>
  if isKnownPtr s v then
    (s, svalToOperand s v)
  else if isRefOrPtrTy v.ty then
    (s, svalToOperand s v)
  else if ssaIsPassByPtr s v.ty then
    let llTy := tyToLLVMTy s v.ty
    let (s, tmp) := freshLocal s
    let tmpName := (tmp.drop 1).toString
    let s := emitEntryAlloca s (.alloca tmpName llTy)
    let s := emitStructured s (.store llTy (svalToOperand s v) (.reg tmpName))
    (s, .reg tmpName)
  else
    (s, svalToOperand s v)

/-- Ensure any value is available as a pointer.
    Unlike ensurePtrOp, this also wraps scalars via alloca+store. -/
private def ensureValAsPtr (s : EmitSSAState) (v : SVal) : EmitSSAState × LLVMOperand :=
  match v with
  | .strConst name =>
    let (s, strTmp) := materializeStrConst s name
    (s, .reg (strTmp.drop 1).toString)
  | .strConstRef name =>
    let (s, strTmp) := materializeStrConstRef s name
    (s, .reg (strTmp.drop 1).toString)
  | _ =>
  if isKnownPtr s v then
    (s, svalToOperand s v)
  else if isRefOrPtrTy v.ty then
    (s, svalToOperand s v)
  else
    let llTy := tyToLLVMTy s v.ty
    let (s, tmp) := freshLocal s
    let tmpName := (tmp.drop 1).toString
    let s := emitEntryAlloca s (.alloca tmpName llTy)
    let s := emitStructured s (.store llTy (svalToOperand s v) (.reg tmpName))
    (s, .reg tmpName)

-- ============================================================
-- Emit SInst
-- ============================================================

/-- Is this type a raw pointer? -/
private def isPointerTy : Ty → Bool
  | .ptrMut _ | .ptrConst _ => true
  | _ => false

/-- Emit a binary operation. Uses operand type (lhs.ty) for type annotations,
    since comparison results are i1 but operate on the operand type. -/
private def emitBinOp (s : EmitSSAState) (dst : String) (op : BinOp) (lhs rhs : SVal) (_ty : Ty) : EmitSSAState :=
  let operandTy := lhs.ty
  -- Pointer arithmetic: ptr + int → getelementptr <pointee>, ptr %p, i64 %n
  -- GEP scales the offset by the pointee element size automatically
  if isPointerTy operandTy && (op == .add || op == .sub) then
    let lOp := svalToOperand s lhs
    let rOp := svalToOperand s rhs
    let pointeeTy := match operandTy with
      | .ptrMut t | .ptrConst t => tyToLLVMTy s t
      | _ => .i8
    let (s, idxOp) := if op == .sub then
      let negReg := s!"{dst}.neg"
      let s := emitStructured s (.binOp negReg .sub .i64 (.intLit 0) rOp)
      (s, LLVMOperand.reg negReg)
    else (s, rOp)
    emitStructured s (.gep dst pointeeTy lOp [(.i64, idxOp)])
  else if isFloatTy operandTy then
    let fTy := floatTyToLLVMTy operandTy
    let lOp := svalToOperand s lhs
    let rOp := svalToOperand s rhs
    let llOp := match op with
      | .add => LLVMBinOp.fadd | .sub => .fsub | .mul => .fmul | .div => .fdiv | .mod => .frem
      | .eq => .fcmpOeq | .neq => .fcmpUne
      | .lt => .fcmpOlt | .gt => .fcmpOgt | .leq => .fcmpOle | .geq => .fcmpOge
      | _ => .fadd
    emitStructured s (.binOp dst llOp fTy lOp rOp)
  else
    let isPtrTy := match operandTy with | .ptrMut _ | .ptrConst _ | .ref _ | .refMut _ => true | _ => false
    let iTy := if isPtrTy then LLVMTy.ptr else intTyToLLVMTy operandTy
    let lOp := svalToOperand s lhs
    let rOp := svalToOperand s rhs
    match op with
    | .add => emitStructured s (.binOp dst .add iTy lOp rOp)
    | .sub => emitStructured s (.binOp dst .sub iTy lOp rOp)
    | .mul => emitStructured s (.binOp dst .mul iTy lOp rOp)
    | .div =>
      if ssaIsSignedInt operandTy then emitStructured s (.binOp dst .sdiv iTy lOp rOp)
      else emitStructured s (.binOp dst .udiv iTy lOp rOp)
    | .mod =>
      if ssaIsSignedInt operandTy then emitStructured s (.binOp dst .srem iTy lOp rOp)
      else emitStructured s (.binOp dst .urem iTy lOp rOp)
    | .eq => emitStructured s (.binOp dst .icmpEq iTy lOp rOp)
    | .neq => emitStructured s (.binOp dst .icmpNe iTy lOp rOp)
    | .lt =>
      if ssaIsSignedInt operandTy then emitStructured s (.binOp dst .icmpSlt iTy lOp rOp)
      else emitStructured s (.binOp dst .icmpUlt iTy lOp rOp)
    | .gt =>
      if ssaIsSignedInt operandTy then emitStructured s (.binOp dst .icmpSgt iTy lOp rOp)
      else emitStructured s (.binOp dst .icmpUgt iTy lOp rOp)
    | .leq =>
      if ssaIsSignedInt operandTy then emitStructured s (.binOp dst .icmpSle iTy lOp rOp)
      else emitStructured s (.binOp dst .icmpUle iTy lOp rOp)
    | .geq =>
      if ssaIsSignedInt operandTy then emitStructured s (.binOp dst .icmpSge iTy lOp rOp)
      else emitStructured s (.binOp dst .icmpUge iTy lOp rOp)
    | .and_ => emitStructured s (.binOp dst .and_ .i1 lOp rOp)
    | .or_ => emitStructured s (.binOp dst .or_ .i1 lOp rOp)
    | .bitand => emitStructured s (.binOp dst .and_ iTy lOp rOp)
    | .bitor => emitStructured s (.binOp dst .or_ iTy lOp rOp)
    | .bitxor => emitStructured s (.binOp dst .xor_ iTy lOp rOp)
    | .shl => emitStructured s (.binOp dst .shl iTy lOp rOp)
    | .shr =>
      if ssaIsSignedInt operandTy then emitStructured s (.binOp dst .ashr iTy lOp rOp)
      else emitStructured s (.binOp dst .lshr iTy lOp rOp)

private def emitSInst (s : EmitSSAState) (inst : SInst) : EmitSSAState :=
  match inst with
  | .binOp dst op lhs rhs ty => emitBinOp s dst op lhs rhs ty
  | .unaryOp dst op operand ty =>
    let valOp := svalToOperand s operand
    match op with
    | .neg =>
      if isFloatTy ty then
        emitStructured s (.fneg dst (floatTyToLLVMTy ty) valOp)
      else
        emitStructured s (.binOp dst .sub (intTyToLLVMTy ty) (.intLit 0) valOp)
    | .not_ => emitStructured s (.binOp dst .xor_ .i1 valOp (.intLit 1))
    | .bitnot => emitStructured s (.binOp dst .xor_ (intTyToLLVMTy ty) valOp (.intLit (-1)))
  | .call dst fn args retTy =>
    -- Intercept vec sized operations for per-element-size dispatch
    match resolveVecCall s fn args retTy with
    | some (specFn, es, payOff) =>
      let s := recordVecElemSpec s es payOff
      if fn == "vec_new" then
        emitStructured s (.call dst (.struct_ "Vec") (.global specFn) [])
      else if fn == "vec_get" then
        match args with
        | [vecArg, idxArg] =>
          let (s, vecPtr) := ensurePtrOp s vecArg
          let idxOp := svalToOperand s idxArg
          let (s, slotTmp) := freshLocal s
          let slotName := (slotTmp.drop 1).toString
          let s := emitStructured s (.call (some slotName) .ptr (.global specFn) [(.ptr, vecPtr), (.i64, idxOp)])
          let s := markPtr s slotName
          match dst with
          | some d => emitStructured s (.load d (tyToLLVMTy s retTy) (.reg slotName))
          | none => s
        | _ => s
      else if fn == "vec_push" then
        match args with
        | [vecArg, valArg] =>
          let (s, vecPtr) := ensurePtrOp s vecArg
          let (s, valPtr) := ensureValAsPtr s valArg
          emitStructured s (.call none .void (.global specFn) [(.ptr, vecPtr), (.ptr, valPtr)])
        | _ => s
      else if fn == "vec_set" then
        match args with
        | [vecArg, idxArg, valArg] =>
          let (s, vecPtr) := ensurePtrOp s vecArg
          let idxOp := svalToOperand s idxArg
          let (s, valPtr) := ensureValAsPtr s valArg
          emitStructured s (.call none .void (.global specFn) [(.ptr, vecPtr), (.i64, idxOp), (.ptr, valPtr)])
        | _ => s
      else if fn == "vec_pop" then
        -- vec_pop returns %enum.Option by value, same calling convention as before
        match args with
        | [vecArg] =>
          let (s, vecPtr) := ensurePtrOp s vecArg
          let retLLTy := tyToLLVMTy s retTy
          emitStructured s (.call dst retLLTy (.global specFn) [(.ptr, vecPtr)])
        | _ => s
      else s
    | none =>
    -- General call path (non-vec or unspecialized vec ops like vec_len/vec_free)
    -- For extern fn calls, #[repr(C)] struct args use C ABI (by-value) passing.
    let isExternCall := s.externFnNames.contains fn
    -- Build typed argument list
    let (s, argOps) := args.foldl (fun (s, ops) a =>
      let paramTy := if isExternCall then externParamTyToLLVMTy s a.ty
                     else paramTyToLLVMTy s a.ty
      match a with
      | .strConst name =>
        -- String constants always passed as ptr to %struct.String
        let (s, strPtr) := materializeStrConst s name
        (s, ops ++ [(.ptr, .reg (strPtr.drop 1).toString)])
      | .strConstRef name =>
        -- Borrowed string constants: no heap alloc, points at global
        let (s, strPtr) := materializeStrConstRef s name
        (s, ops ++ [(.ptr, .reg (strPtr.drop 1).toString)])
      | _ =>
      let valTy := tyToLLVMTy s a.ty
      -- Extern call with repr(C) struct: flatten to integer registers per C ABI.
      -- ARM64 AAPCS: ≤ 8 bytes → load as i64, 9-16 bytes → load as two i64s.
      if isExternCall && isReprCStruct s a.ty then
        let flatTys := externABIFlattenParam s a.ty
        if flatTys == [.i64] then
          -- Small struct (≤ 8 bytes): ensure pointer, load as i64
          let (s, ptrOp) := if isKnownPtr s a then (s, svalToOperand s a)
            else
              let (s, tmp) := freshLocal s
              let tmpName := (tmp.drop 1).toString
              let s := emitEntryAlloca s (.alloca tmpName valTy)
              let s := emitStructured s (.store valTy (svalToOperand s a) (.reg tmpName))
              (s, .reg tmpName)
          let (s, flat) := freshLocal s
          let flatName := (flat.drop 1).toString
          let s := emitStructured s (.load flatName .i64 ptrOp)
          (s, ops ++ [(.i64, .reg flatName)])
        else if flatTys == [.i64, .i64] then
          -- Medium struct (9-16 bytes): ensure pointer, load two i64 halves
          let (s, ptrOp) := if isKnownPtr s a then (s, svalToOperand s a)
            else
              let (s, tmp) := freshLocal s
              let tmpName := (tmp.drop 1).toString
              let s := emitEntryAlloca s (.alloca tmpName valTy)
              let s := emitStructured s (.store valTy (svalToOperand s a) (.reg tmpName))
              (s, .reg tmpName)
          let (s, lo) := freshLocal s
          let loName := (lo.drop 1).toString
          let s := emitStructured s (.load loName .i64 ptrOp)
          -- GEP to byte offset 8 for the second i64
          let (s, hiPtr) := freshLocal s
          let hiPtrName := (hiPtr.drop 1).toString
          let s := emitStructured s (.gep hiPtrName .i8 ptrOp [(.i32, .intLit 8)])
          let (s, hi) := freshLocal s
          let hiName := (hi.drop 1).toString
          let s := emitStructured s (.load hiName .i64 (.reg hiPtrName))
          (s, ops ++ [(.i64, .reg loName), (.i64, .reg hiName)])
        else
          -- Large struct (> 16 bytes): pass by pointer
          if isKnownPtr s a then (s, ops ++ [(.ptr, svalToOperand s a)])
          else
            let (s, tmp) := freshLocal s
            let tmpName := (tmp.drop 1).toString
            let s := emitEntryAlloca s (.alloca tmpName valTy)
            let s := emitStructured s (.store valTy (svalToOperand s a) (.reg tmpName))
            (s, ops ++ [(.ptr, .reg tmpName)])
      else if paramTy == .ptr && valTy != .ptr then
        -- If the register is already known to be a pointer (e.g. a struct
        -- parameter passed by ptr), pass it directly instead of
        -- alloca+store which would misuse the ptr as a struct value.
        if isKnownPtr s a then
          (s, ops ++ [(.ptr, svalToOperand s a)])
        else
          let (s, tmp) := freshLocal s
          let tmpName := (tmp.drop 1).toString
          let s := emitEntryAlloca s (.alloca tmpName valTy)
          let s := emitStructured s (.store valTy (svalToOperand s a) (.reg tmpName))
          (s, ops ++ [(.ptr, .reg tmpName)])
      else
        (s, ops ++ [(paramTy, svalToOperand s a)])
    ) (s, ([] : List (LLVMTy × LLVMOperand)))
    -- For extern calls, flatten the return type per C ABI
    let retLLTy := if isExternCall then externABIFlattenRet s retTy
                   else tyToLLVMTy s retTy
    -- Indirect call: function is a parameter with fn type, a register loaded
    -- from memory (fnTypeRegs), or a %-prefixed register from the Lower pass.
    let isIndirect := fn.startsWith "%"
      || (s.fnParams.any fun (n, t) =>
        n == fn && match t with | .fn_ _ _ _ => true | _ => false)
      || s.fnTypeRegs.contains fn
    -- Resolve aliased imports to their real linker symbol
    let linkerFn := match s.linkerAliases.lookup fn with
      | some orig => orig
      | none => fn
    let callTarget : LLVMOperand := if isIndirect then
      if fn.startsWith "%" then .reg (fn.drop 1).toString
      else .reg fn
    else .global linkerFn
    -- For extern calls returning small repr(C) structs: call returns i64,
    -- store through pointer so downstream code sees the struct correctly.
    -- Downstream code expects dst to be a pointer (pass-by-ptr for structs).
    if isExternCall && isReprCStruct s retTy && retLLTy == .i64 then
      let (s, flatRet) := freshLocal s
      let flatRetName := (flatRet.drop 1).toString
      let s := emitStructured s (.call flatRetName .i64 callTarget argOps)
      -- Alloca struct, store i64 into it, use alloca as the dst register
      match dst with
      | some dstName =>
        let s := emitEntryAlloca s (.alloca dstName (tyToLLVMTy s retTy))
        let s := emitStructured s (.store .i64 (.reg flatRetName) (.reg dstName))
        markPtr s dstName
      | none => s
    else
      emitStructured s (.call dst retLLTy callTarget argOps)
  | .alloca dst ty =>
    let s := emitEntryAlloca s (.alloca dst (tyToLLVMTy s ty))
    markPtr s dst
  | .load dst ptr ty =>
    let (s, ptrOp) := ensurePtrOp s ptr
    let s := emitStructured s (.load dst (tyToLLVMTy s ty) ptrOp)
    -- Track registers that hold function pointers (loaded from struct fields etc.)
    match ty with
    | .fn_ _ _ _ => { s with fnTypeRegs := s.fnTypeRegs ++ [dst] }
    | _ => s
  | .store val ptr =>
    let (s, ptrOp) := ensurePtrOp s ptr
    match val with
    | .strConst name =>
      -- Copy string struct from materialized temp to destination
      let (s, srcPtr) := materializeStrConst s name
      let sz := ssaTySize s .string
      emitStructured s (.memcpy ptrOp (.reg (srcPtr.drop 1).toString) sz)
    | .strConstRef name =>
      -- Copy borrowed string struct (no heap alloc) to destination
      let (s, srcPtr) := materializeStrConstRef s name
      let sz := ssaTySize s .string
      emitStructured s (.memcpy ptrOp (.reg (srcPtr.drop 1).toString) sz)
    | _ =>
    -- If the value is a known pointer but typed as a struct, it is
    -- actually a pointer to the struct (e.g. a pass-by-ptr param).
    -- Use memcpy rather than a store that would misinterpret ptr as struct.
    if isKnownPtr s val && ssaIsPassByPtr s val.ty then
      let sz := ssaTySize s val.ty
      emitStructured s (.memcpy ptrOp (svalToOperand s val) sz)
    else
      emitStructured s (.store (tyToLLVMTy s val.ty) (svalToOperand s val) ptrOp)
  | .gep dst base indices ty =>
    let (s, basePtrOp) := ensurePtrOp s base
    let idxOps := indices.map fun i => (tyToLLVMTy s i.ty, svalToOperand s i)
    let s := emitStructured s (.gep dst (tyToLLVMTy s ty) basePtrOp idxOps)
    markPtr s dst
  | .phi dst incoming ty =>
    let pairs := incoming.map fun (v, lbl) => (svalToOperand s v, lbl)
    emitStructured s (.phi dst (tyToLLVMTy s ty) pairs)
  | .cast dst val targetTy =>
    match val with
    | .strConst name =>
      -- String constant → ptr: materialize the %struct.String and return ptr
      let (s, strPtr) := materializeStrConst s name
      let s := emitStructured s (.gep dst .i8 (.reg (strPtr.drop 1).toString) [(.i32, .intLit 0)])
      markPtr s dst
    | .strConstRef name =>
      -- Borrowed string constant → ptr: materialize without heap alloc
      let (s, strPtr) := materializeStrConstRef s name
      let s := emitStructured s (.gep dst .i8 (.reg (strPtr.drop 1).toString) [(.i32, .intLit 0)])
      markPtr s dst
    | _ =>
    let srcTy := val.ty
    let srcLLTy := tyToLLVMTy s srcTy
    let dstLLTy := tyToLLVMTy s targetTy
    let valOp := svalToOperand s val
    if srcLLTy == dstLLTy then
      -- Same type, just alias
      if srcLLTy == .ptr then emitStructured s (.gep dst .i8 valOp [(.i32, .intLit 0)])
      else emitStructured s (.binOp dst .add srcLLTy valOp (.intLit 0))
    else if srcLLTy == .ptr || dstLLTy == .ptr then
      if srcLLTy == .ptr && isIntegerTy targetTy then
        emitStructured s (.cast dst .ptrtoint .ptr valOp dstLLTy)
      else if dstLLTy == .ptr && isIntegerTy srcTy then
        emitStructured s (.cast dst .inttoptr srcLLTy valOp .ptr)
      else if srcLLTy == .ptr then
        -- ptr → non-int (e.g. ptr → struct): ptrtoint
        emitStructured s (.cast dst .ptrtoint .ptr valOp dstLLTy)
      else
        -- non-ptr → ptr: use inttoptr for ints, alloca+store for structs
        if ssaIsPassByPtr s srcTy then
          let (s, tmp) := freshLocal s
          let tmpName := (tmp.drop 1).toString
          let s := emitEntryAlloca s (.alloca tmpName srcLLTy)
          let s := emitStructured s (.store srcLLTy valOp (.reg tmpName))
          let s := emitStructured s (.gep dst .i8 (.reg tmpName) [(.i32, .intLit 0)])
          markPtr s dst
        else
          emitStructured s (.cast dst .inttoptr srcLLTy valOp .ptr)
    else if isIntegerTy srcTy && isIntegerTy targetTy then
      let srcBits := match srcTy with
        | .i8 | .u8 | .char => 8 | .i16 | .u16 => 16 | .i32 | .u32 => 32 | _ => 64
      let dstBits := match targetTy with
        | .i8 | .u8 | .char => 8 | .i16 | .u16 => 16 | .i32 | .u32 => 32 | _ => 64
      if srcBits < dstBits then
        if ssaIsSignedInt srcTy then emitStructured s (.cast dst .sext srcLLTy valOp dstLLTy)
        else emitStructured s (.cast dst .zext srcLLTy valOp dstLLTy)
      else if srcBits > dstBits then
        emitStructured s (.cast dst .trunc srcLLTy valOp dstLLTy)
      else
        emitStructured s (.cast dst .bitcast srcLLTy valOp dstLLTy)
    else if isIntegerTy srcTy && isFloatTy targetTy then
      if ssaIsSignedInt srcTy then emitStructured s (.cast dst .sitofp srcLLTy valOp dstLLTy)
      else emitStructured s (.cast dst .uitofp srcLLTy valOp dstLLTy)
    else if isFloatTy srcTy && isIntegerTy targetTy then
      if ssaIsSignedInt targetTy then emitStructured s (.cast dst .fptosi srcLLTy valOp dstLLTy)
      else emitStructured s (.cast dst .fptoui srcLLTy valOp dstLLTy)
    else if isFloatTy srcTy && isFloatTy targetTy then
      let srcBits := if srcTy == .float32 then 32 else 64
      let dstBits := if targetTy == .float32 then 32 else 64
      if srcBits < dstBits then emitStructured s (.cast dst .fpext srcLLTy valOp dstLLTy)
      else emitStructured s (.cast dst .fptrunc srcLLTy valOp dstLLTy)
    else
      -- Fallback: alloca+store+load to "bitcast"
      let (s, tmp) := freshLocal s
      let tmpName := (tmp.drop 1).toString
      let s := emitEntryAlloca s (.alloca tmpName srcLLTy)
      let s := emitStructured s (.store srcLLTy valOp (.reg tmpName))
      emitStructured s (.load dst dstLLTy (.reg tmpName))
  | .memcpy dst src size =>
    emitStructured s (.memcpy (svalToOperand s dst) (svalToOperand s src) size)

-- ============================================================
-- Emit SBlock / SFnDef
-- ============================================================

/-- Emit a block terminator. Returns the (possibly modified) state and a structured LLVMTerm.
    May add pre-terminator instructions (e.g. loads for struct return values). -/
private def emitSTerm (s : EmitSSAState) (t : STerm) : EmitSSAState × LLVMTerm :=
  match t with
  | .ret (some v) =>
    let llTy := tyToLLVMTy s v.ty
    if llTy == .void then (s, .ret .void none)
    else match v with
    | .strConst _ =>
      -- String constant: materialize as struct, load, return by value
      let (s, ptr) := materializeStrConst s (match v with | .strConst n => n | _ => "")
      let (s, tmp) := freshLocal s
      let tmpName := (tmp.drop 1).toString
      let s := emitStructured s (.load tmpName llTy (.reg (ptr.drop 1).toString))
      (s, .ret llTy (some (.reg tmpName)))
    | .strConstRef name =>
      -- Borrowed string constant ref: materialize without heap alloc, return ptr
      let (s, ptr) := materializeStrConstRef s name
      (s, .ret .ptr (some (.reg (ptr.drop 1).toString)))
    | _ =>
    if isKnownPtr s v && ssaIsPassByPtr s v.ty then
      -- Value is a pointer to a struct (e.g. pass-by-ptr param); load it
      -- so we return the struct by value as the LLVM signature expects.
      let (s, tmp) := freshLocal s
      let tmpName := (tmp.drop 1).toString
      let s := emitStructured s (.load tmpName llTy (svalToOperand s v))
      (s, .ret llTy (some (.reg tmpName)))
    else (s, .ret llTy (some (svalToOperand s v)))
  | .ret none => (s, .ret .void none)
  | .br lbl => (s, .br lbl)
  | .condBr cond tl el =>
    (s, .condBr (svalToOperand s cond) tl el)
  | .unreachable => (s, .unreachable)

private def emitSBlock (s : EmitSSAState) (b : SBlock) : EmitSSAState :=
  -- Start fresh instruction list for this block
  let s := { s with currentInstrs := #[] }
  -- Emit all instructions
  let s := b.insts.foldl emitSInst s
  -- Emit terminator (may add pre-terminator instructions to currentInstrs)
  let (s, term) := emitSTerm s b.term
  -- LLVM requires all PHI nodes at the top of a basic block.
  -- Lower.lean can interleave PHIs with other instructions (e.g. when multiple
  -- if-statements appear in a match arm), so partition and reorder here.
  let isPhi : LLVMInstr → Bool
    | .phi .. => true
    | _ => false
  let allInstrs := s.currentInstrs.toList
  let phis := allInstrs.filter isPhi
  let nonPhis := allInstrs.filter (fun i => !isPhi i)
  -- Create the block with PHIs first, then other instructions
  let block : LLVMBlock := {
    label := b.label
    instrs := phis ++ nonPhis
    term := term
  }
  { s with
    currentBlocks := s.currentBlocks.push block
    currentInstrs := #[]
  }

private def emitSFnDef (s : EmitSSAState) (f : SFnDef) (isUserMain : Bool) : EmitSSAState :=
  let retTy := tyToLLVMTy s f.retTy
  let fnName := if isUserMain then "user_main" else f.name
  let params := f.params.map fun (n, t) => (n, paramTyToLLVMTy s t)
  -- Reset per-function state
  let s := { s with currentBlocks := #[], fnParams := f.params, entryAllocas := #[] }
  -- Mark struct-type params as pointers, track fn params for indirect calls
  let s := f.params.foldl (fun s (n, t) =>
    match t with
    | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ => markPtr s n
    | _ => if ssaIsPassByPtr s t then markPtr s n else s
  ) s
  -- Emit all blocks
  let s := f.blocks.foldl emitSBlock s
  -- Hoist all allocas to the entry block so they are not repeated inside loops
  let blocks := s.currentBlocks.toList
  let blocks := match blocks with
    | entry :: rest =>
      { entry with instrs := s.entryAllocas.toList ++ entry.instrs } :: rest
    | [] => []
  -- Build structured function definition
  let fnDef : LLVMFnDef := {
    name := fnName
    retTy := retTy
    params := params
    blocks := blocks
  }
  let s := { s with moduleFunctions := s.moduleFunctions.push fnDef }
  -- Reset per-function state
  { s with ptrRegs := [], fnParams := [], fnTypeRegs := [], currentBlocks := #[], currentInstrs := #[], entryAllocas := #[] }

-- ============================================================
-- Emit struct/enum type definitions
-- ============================================================

-- ============================================================
-- Emit external declarations and builtins
-- ============================================================

private def emitExternDecls (s : EmitSSAState) (externFns : List (String × List (String × Ty) × Ty))
    (definedFns : List String := []) : EmitSSAState :=
  -- Standard C runtime declarations (used by remaining builtins)
  let s := emitDecl s { name := "malloc", retTy := .ptr, params := [.i64] }
  let s := emitDecl s { name := "free", retTy := .void, params := [.ptr] }
  let s := emitDecl s { name := "realloc", retTy := .ptr, params := [.ptr, .i64] }
  let s := emitDecl s { name := "llvm.memcpy.p0.p0.i64", retTy := .void, params := [.ptr, .ptr, .i64, .i1] }
  let s := emitDecl s { name := "write", retTy := .i64, params := [.i32, .ptr, .i64] }
  let s := emitDecl s { name := "abort", retTy := .void, params := [] }
  let s := emitDecl s { name := "printf", retTy := .i32, params := [.ptr], variadic := true }
  let s := emitDecl s { name := "strlen", retTy := .i64, params := [.ptr] }
  let s := emitDecl s { name := "memset", retTy := .ptr, params := [.ptr, .i32, .i64] }
  let s := emitDecl s { name := "memcmp", retTy := .i32, params := [.ptr, .ptr, .i64] }
  -- Conversion builtin dependencies
  let s := emitDecl s { name := "snprintf", retTy := .i32, params := [.ptr, .i64, .ptr], variadic := true }
  let s := emitDecl s { name := "strtol", retTy := .i64, params := [.ptr, .ptr, .i32] }
  -- Names already declared above — skip duplicates from user extern fns
  let builtinNames : List String := [
    "malloc", "free", "realloc", "write", "abort", "printf", "strlen",
    "memset", "memcmp", "snprintf", "strtol"
  ]
  -- User extern function declarations (skip if already defined as a concrete function).
  -- Uses ABI flattening so #[repr(C)] structs are passed in registers per the C ABI.
  externFns.foldl (fun s (name, params, retTy) =>
    if builtinNames.contains name || definedFns.contains name then s
    else
      let retLLTy := externABIFlattenRet s retTy
      let paramTys := params.foldl (fun acc (_, t) => acc ++ externABIFlattenParam s t) []
      emitDecl s { name := name, retTy := retLLTy, params := paramTys }
  ) s

/-- Emit the main wrapper that calls user_main and prints the result.
    For void/unit return types, the wrapper just calls user_main without printing.
    For int/bool/other scalar types, it prints the result.
    The wrapper accepts (argc, argv) from the C runtime and saves them
    to globals so user code can access them via __concrete_get_argc/argv. -/
private def emitMainWrapper (s : EmitSSAState) (retTy : Ty) : EmitSSAState :=
  let retLLTy := tyToLLVMTy s retTy
  let ret0 : LLVMTerm := .ret .i32 (some (.intLit 0))
  let printfTarget : LLVMOperand := .global "printf"
  -- Emit globals and accessor functions for argc/argv
  let s := emitGlobal s { name := "__concrete_argc", ty := .i32, value := "0", mutable := true }
  let s := emitGlobal s { name := "__concrete_argv", ty := .ptr, value := "null", mutable := true }
  -- __concrete_get_argc() -> i32
  let getArgcFn : LLVMFnDef :=
    { name := "__concrete_get_argc", retTy := .i32, params := [], blocks := [
      ⟨"entry", [
        .load "argc" .i32 (.global "__concrete_argc")
      ], .ret .i32 (some (.reg "argc"))⟩] }
  let s := { s with moduleFunctions := s.moduleFunctions.push getArgcFn }
  -- __concrete_get_argv(idx: i32) -> ptr
  let getArgvFn : LLVMFnDef :=
    { name := "__concrete_get_argv", retTy := .ptr, params := [("idx", .i32)], blocks := [
      ⟨"entry", [
        .load "argv" .ptr (.global "__concrete_argv"),
        .cast "idx64" .sext .i32 (.reg "idx") .i64,
        .gep "argp" .ptr (.reg "argv") [(.i64, .reg "idx64")],
        .load "arg" .ptr (.reg "argp")
      ], .ret .ptr (some (.reg "arg"))⟩] }
  let s := { s with moduleFunctions := s.moduleFunctions.push getArgvFn }
  -- Save argc/argv at the start of main, then call user_main
  let saveArgcArgv : List LLVMInstr := [
    .store .i32 (.reg "argc") (.global "__concrete_argc"),
    .store .ptr (.reg "argv") (.global "__concrete_argv")
  ]
  let mkMainFn (blk : LLVMBlock) : LLVMFnDef :=
    { name := "main", retTy := .i32, params := [("argc", .i32), ("argv", .ptr)], blocks := [blk] }
  if retLLTy == .void then
    -- Unit/void return: just call, no print
    let instrs : List LLVMInstr := saveArgcArgv ++ [.call none .void (.global "user_main") []]
    let mainFn := mkMainFn ⟨"entry", instrs, ret0⟩
    { s with moduleFunctions := s.moduleFunctions.push mainFn }
  else if retLLTy == .i1 then
    -- Bool return: print "true" or "false"
    let s := emitGlobal s { name := "fmt.true", ty := .array 5 .i8, value := "c\"true\\00\"" }
    let s := emitGlobal s { name := "fmt.false", ty := .array 6 .i8, value := "c\"false\\00\"" }
    let s := emitGlobal s { name := "fmt.main.s", ty := .array 4 .i8, value := "c\"%s\\0A\\00\"" }
    let instrs : List LLVMInstr := saveArgcArgv ++ [
      .call (some "result") .i1 (.global "user_main") [],
      .gep "true_str" (.array 5 .i8) (.global "fmt.true") [(.i32, .intLit 0), (.i32, .intLit 0)],
      .gep "false_str" (.array 6 .i8) (.global "fmt.false") [(.i32, .intLit 0), (.i32, .intLit 0)],
      .select "str" (.reg "result") .ptr (.reg "true_str") (.reg "false_str"),
      .gep "fmt" (.array 4 .i8) (.global "fmt.main.s") [(.i32, .intLit 0), (.i32, .intLit 0)],
      .callVariadic none .i32 printfTarget [(.ptr, .reg "fmt"), (.ptr, .reg "str")]
    ]
    let mainFn := mkMainFn ⟨"entry", instrs, ret0⟩
    { s with moduleFunctions := s.moduleFunctions.push mainFn }
  else if retLLTy == .i64 then
    -- i64 return: print with %lld
    let s := emitGlobal s { name := "fmt.main", ty := .array 6 .i8, value := "c\"%lld\\0A\\00\"" }
    let instrs : List LLVMInstr := saveArgcArgv ++ [
      .call (some "result") .i64 (.global "user_main") [],
      .gep "fmt" (.array 6 .i8) (.global "fmt.main") [(.i32, .intLit 0), (.i32, .intLit 0)],
      .callVariadic none .i32 printfTarget [(.ptr, .reg "fmt"), (.i64, .reg "result")]
    ]
    let mainFn := mkMainFn ⟨"entry", instrs, ret0⟩
    { s with moduleFunctions := s.moduleFunctions.push mainFn }
  else if retLLTy == .i32 || retLLTy == .i16 || retLLTy == .i8 then
    -- Smaller integer return: widen to i64, then print
    let castOp : LLVMCastOp := if ssaIsSignedInt retTy then .sext else .zext
    let s := emitGlobal s { name := "fmt.main", ty := .array 6 .i8, value := "c\"%lld\\0A\\00\"" }
    let instrs : List LLVMInstr := saveArgcArgv ++ [
      .call (some "result") retLLTy (.global "user_main") [],
      .cast "result64" castOp retLLTy (.reg "result") .i64,
      .gep "fmt" (.array 6 .i8) (.global "fmt.main") [(.i32, .intLit 0), (.i32, .intLit 0)],
      .callVariadic none .i32 printfTarget [(.ptr, .reg "fmt"), (.i64, .reg "result64")]
    ]
    let mainFn := mkMainFn ⟨"entry", instrs, ret0⟩
    { s with moduleFunctions := s.moduleFunctions.push mainFn }
  else
    -- For other types (structs, strings, etc.), just call and return 0
    let instrs : List LLVMInstr := saveArgcArgv ++ [.call (some "result") retLLTy (.global "user_main") []]
    let mainFn := mkMainFn ⟨"entry", instrs, ret0⟩
    { s with moduleFunctions := s.moduleFunctions.push mainFn }

-- ============================================================
-- Emit string literal globals
-- ============================================================

/-- Emit builtin implementations needed by the program.
    Skips builtins whose name collides with a user-defined function. -/
private def emitBuiltins (s : EmitSSAState) : EmitSSAState :=
  let (builtinFns, builtinGlobals, builtinDecls) := getBuiltinFns
  let vecFns := getVecBuiltinFns s.vecElemSpecs
  -- Filter out builtins that are already defined by user/extern code
  let userFnNames := s.moduleFunctions.toList.map (·.name)
  let allFns := (builtinFns ++ vecFns).filter fun f => !userFnNames.contains f.name
  { s with
    moduleFunctions := s.moduleFunctions ++ allFns.toArray,
    moduleGlobals := s.moduleGlobals ++ builtinGlobals.toArray,
    moduleDeclarations := s.moduleDeclarations ++ builtinDecls.toArray }

/-- Collect all types referenced in an SVal. -/
private def collectSValTys (v : SVal) : List Ty :=
  match v with
  | .reg _ t => [t]
  | .intConst _ t => [t]
  | .floatConst _ t => [t]
  | _ => []

/-- Collect all types referenced in an SInst. -/
private def collectSInstTys (inst : SInst) : List Ty :=
  match inst with
  | .binOp _ _ lhs rhs ty => collectSValTys lhs ++ collectSValTys rhs ++ [ty]
  | .unaryOp _ _ operand ty => collectSValTys operand ++ [ty]
  | .call _ _ args retTy => args.foldl (fun acc a => acc ++ collectSValTys a) [] ++ [retTy]
  | .alloca _ ty => [ty]
  | .load _ ptr ty => collectSValTys ptr ++ [ty]
  | .store val ptr => collectSValTys val ++ collectSValTys ptr
  | .gep _ base indices ty => collectSValTys base ++ indices.foldl (fun acc i => acc ++ collectSValTys i) [] ++ [ty]
  | .phi _ incoming ty => incoming.foldl (fun acc (v, _) => acc ++ collectSValTys v) [] ++ [ty]
  | .cast _ val targetTy => collectSValTys val ++ [targetTy]
  | .memcpy dst src _ => collectSValTys dst ++ collectSValTys src

-- ============================================================
-- Entry point: emit full SSA program as LLVM IR
-- ============================================================

def emitSModule (s : EmitSSAState) (m : SModule) (testMode : Bool := false) : EmitSSAState :=
  let s := { s with structDefs := s.structDefs ++ m.structs, enumDefs := s.enumDefs ++ m.enums,
                     linkerAliases := s.linkerAliases ++ m.linkerAliases,
                     externFnNames := s.externFnNames ++ m.externFns.map (·.1) }
  -- Collect all types from this module's functions for generic type arg lookup
  let moduleTys := m.functions.foldl (fun acc f =>
    let retTys := [f.retTy]
    let paramTys := f.params.map (·.2)
    f.blocks.foldl (fun acc b =>
      b.insts.foldl (fun acc inst => acc ++ collectSInstTys inst) acc
    ) (acc ++ retTys ++ paramTys)
  ) ([] : List Ty)
  -- User types (dedup across modules, substitute type args for generic definitions)
  let s := m.structs.foldl (fun s sd =>
    if s.emittedTypes.contains sd.name then s
    else if sd.typeParams.length > 0 then
      -- Generic struct: find concrete type args from function types
      match moduleTys.findSome? fun t =>
        match t with
        | .generic n args => if n == sd.name then some args else none
        | _ => none
      with
      | some args =>
        let sd := Layout.substStructTypeArgs sd args
        let s := { s with emittedTypes := sd.name :: s.emittedTypes }
        emitTypeDef s (Layout.structTypeDef (layoutCtxOf s) sd)
      | none => s  -- no instantiation found, skip
    else
      let s := { s with emittedTypes := sd.name :: s.emittedTypes }
      emitTypeDef s (Layout.structTypeDef (layoutCtxOf s) sd)
  ) s
  let ctx := layoutCtxOf s
  let s := m.enums.foldl (fun s ed =>
    if s.emittedTypes.contains ed.name then s
    else if ed.typeParams.length > 0 then
      -- Generic enum: find concrete type args from function types
      match moduleTys.findSome? fun t =>
        match t with
        | .generic n args => if n == ed.name then some args else none
        | _ => none
      with
      | some args =>
        let s := { s with emittedTypes := ed.name :: s.emittedTypes }
        (Layout.enumTypeDefs ctx ed args).foldl (fun s line => emitTypeDef s line) s
      | none => s  -- no instantiation found, skip
    else
      let s := { s with emittedTypes := ed.name :: s.emittedTypes }
      (Layout.enumTypeDefs ctx ed).foldl (fun s line => emitTypeDef s line) s
  ) s
  -- String literal globals
  let s := m.globals.foldl (fun s (name, val) =>
    let escaped := ssaEscapeStringForLLVM val
    let len := val.length + 1
    let s := emitGlobal s { name := name, ty := .array len .i8, value := s!"c\"{escaped}\\00\"" }
    { s with stringLengths := s.stringLengths ++ [(name, val.length)] }
  ) s
  -- Functions
  let hasMain := m.functions.any fun f => f.isEntryPoint
  let s := m.functions.foldl (fun s f =>
    emitSFnDef s f f.isEntryPoint
  ) s
  -- Main wrapper (skip in test mode — test runner provides main)
  if testMode then
    -- Provide stubs for __concrete_get_argc/argv so std.args links in test mode
    let s := emitGlobal s { name := "__concrete_argc", ty := .i32, value := "0", mutable := true }
    let s := emitGlobal s { name := "__concrete_argv", ty := .ptr, value := "null", mutable := true }
    let getArgcFn : LLVMFnDef :=
      { name := "__concrete_get_argc", retTy := .i32, params := [], blocks := [
        ⟨"entry", [], .ret .i32 (some (.intLit 0))⟩] }
    let s := { s with moduleFunctions := s.moduleFunctions.push getArgcFn }
    let getArgvFn : LLVMFnDef :=
      { name := "__concrete_get_argv", retTy := .ptr, params := [("idx", .i32)], blocks := [
        ⟨"entry", [], .ret .ptr (some (.null_))⟩] }
    { s with moduleFunctions := s.moduleFunctions.push getArgvFn }
  else if hasMain then
    match m.functions.find? fun f => f.isEntryPoint with
    | some mainFn => emitMainWrapper s mainFn.retTy
    | none => s
  else s


/-- Scan all SSA modules for concrete type arguments used with Option and Result.
    Returns (largest Option payload type, largest Result payload types (ok, err)). -/
private def scanBuiltinEnumArgs (ctx : Layout.Ctx) (modules : List SModule) : (Option Ty) × (Option (Ty × Ty)) :=
  let allTys := modules.foldl (fun acc m =>
    m.functions.foldl (fun acc f =>
      f.blocks.foldl (fun acc b =>
        b.insts.foldl (fun acc inst => acc ++ collectSInstTys inst) acc
      ) acc
    ) acc
  ) ([] : List Ty)
  -- Find all Option<T> and Result<T, E> instantiations
  let optPayloads := allTys.filterMap fun t =>
    match t with
    | .generic n [arg] => if n == optionEnumName then some arg else none
    | _ => none
  let resPayloads := allTys.filterMap fun t =>
    match t with
    | .generic n [ok, err] => if n == resultEnumName then some (ok, err) else none
    | _ => none
  -- Pick the largest payload type for Option
  let bestOpt := optPayloads.foldl (fun best t =>
    match best with
    | none => some t
    | some prev => if Layout.tySize ctx t > Layout.tySize ctx prev then some t else best
  ) none
  -- Pick the largest ok/err payload types for Result
  let bestRes := resPayloads.foldl (fun best (ok, err) =>
    match best with
    | none => some (ok, err)
    | some (prevOk, prevErr) =>
      let newOk := if Layout.tySize ctx ok > Layout.tySize ctx prevOk then ok else prevOk
      let newErr := if Layout.tySize ctx err > Layout.tySize ctx prevErr then err else prevErr
      some (newOk, newErr)
  ) none
  (bestOpt, bestRes)

private def emitTestRunner (s : EmitSSAState) (modules : List SModule) (moduleFilter : Option String := none) : EmitSSAState :=
  -- Collect all test functions across all modules
  let testFns := modules.foldl (fun acc m =>
    acc ++ (m.functions.filter fun f => f.isTest)
  ) []
  -- If a module filter is given, only keep tests whose modulePath starts with the filter
  let testFns := match moduleFilter with
    | none => testFns
    | some modPrefix => testFns.filter fun f =>
        f.modulePath == modPrefix || f.modulePath.startsWith (modPrefix ++ ".")
  let printfOp : LLVMOperand := .global "printf"
  let gep32 (dst : String) (arrTy : LLVMTy) (base : LLVMOperand) : LLVMInstr :=
    .gep dst arrTy base [(.i32, .intLit 0), (.i32, .intLit 0)]
  if testFns.isEmpty then
    -- No tests found: emit a main that prints a message and returns 0
    let s := emitGlobal s { name := "fmt.test.none", ty := .array 15 .i8, value := "c\"No tests found\\00\"" }
    let s := emitGlobal s { name := "fmt.test.nl", ty := .array 2 .i8, value := "c\"\\0A\\00\"" }
    let instrs : List LLVMInstr := [
      gep32 "fmt" (.array 15 .i8) (.global "fmt.test.none"),
      .callVariadic none .i32 printfOp [(.ptr, .reg "fmt")],
      gep32 "nl" (.array 2 .i8) (.global "fmt.test.nl"),
      .callVariadic none .i32 printfOp [(.ptr, .reg "nl")]
    ]
    let blk : LLVMBlock := ⟨"entry", instrs, .ret .i32 (some (.intLit 0))⟩
    let mainFn : LLVMFnDef := { name := "main", retTy := .i32, params := [], blocks := [blk] }
    { s with moduleFunctions := s.moduleFunctions.push mainFn }
  else
    -- Emit globals for test name strings
    let s := testFns.foldl (fun s f =>
      let nameLen := f.name.length + 1
      let escaped := ssaEscapeStringForLLVM f.name
      emitGlobal s { name := s!"test.name.{f.name}", ty := .array nameLen .i8, value := s!"c\"{escaped}\\00\"" }
    ) s
    -- Emit format string globals
    let s := emitGlobal s { name := "fmt.test.pass", ty := .array 10 .i8, value := "c\"PASS: %s\\0A\\00\"" }
    let s := emitGlobal s { name := "fmt.test.fail", ty := .array 10 .i8, value := "c\"FAIL: %s\\0A\\00\"" }
    -- Helper: build test dispatch instructions for a given test at index i
    let mkTestDispatch (f : SFnDef) (i : String) : List LLVMInstr :=
      let nameLen := f.name.length + 1
      [ .comment s!"Test: {f.name}",
        .call (some s!"result.{i}") .i32 (.global f.name) [],
        .binOp s!"is_pass.{i}" .icmpEq .i32 (.reg s!"result.{i}") (.intLit 0),
        gep32 s!"name.{i}" (.array nameLen .i8) (.global s!"test.name.{f.name}") ]
    -- Build all blocks via fold over (remaining tests, index, accumulated blocks)
    -- Entry block gets alloca + store + first test dispatch
    let (blocks, _, _) := testFns.foldl (fun (acc, idx, rest) _f =>
      let i := toString idx
      -- pass.i: print PASS, branch to next.i
      let passInstrs : List LLVMInstr := [
        gep32 s!"pfmt.{i}" (.array 10 .i8) (.global "fmt.test.pass"),
        .callVariadic none .i32 printfOp [(.ptr, .reg s!"pfmt.{i}"), (.ptr, .reg s!"name.{i}")]
      ]
      let passBlock : LLVMBlock := ⟨s!"pass.{i}", passInstrs, .br s!"next.{i}"⟩
      -- fail.i: print FAIL, increment failures, branch to next.i
      let failInstrs : List LLVMInstr := [
        gep32 s!"ffmt.{i}" (.array 10 .i8) (.global "fmt.test.fail"),
        .callVariadic none .i32 printfOp [(.ptr, .reg s!"ffmt.{i}"), (.ptr, .reg s!"name.{i}")],
        .load s!"old_fail.{i}" .i32 (.reg "failures"),
        .binOp s!"new_fail.{i}" .add .i32 (.reg s!"old_fail.{i}") (.intLit 1),
        .store .i32 (.reg s!"new_fail.{i}") (.reg "failures")
      ]
      let failBlock : LLVMBlock := ⟨s!"fail.{i}", failInstrs, .br s!"next.{i}"⟩
      -- next.i: dispatch next test, or return exit code if last
      let tail := rest.drop 1
      let nextBlock : LLVMBlock := match tail with
        | nextF :: _ =>
          let nextI := toString (idx + 1)
          ⟨s!"next.{i}", mkTestDispatch nextF nextI,
           .condBr (.reg s!"is_pass.{nextI}") s!"pass.{nextI}" s!"fail.{nextI}"⟩
        | [] =>
          let footerInstrs : List LLVMInstr := [
            .load "total_fail" .i32 (.reg "failures"),
            .binOp "any_fail" .icmpSgt .i32 (.reg "total_fail") (.intLit 0),
            .select "exit" (.reg "any_fail") .i32 (.intLit 1) (.intLit 0)
          ]
          ⟨s!"next.{i}", footerInstrs, .ret .i32 (some (.reg "exit"))⟩
      (acc ++ [passBlock, failBlock, nextBlock], idx + 1, tail)
    ) ([], 0, testFns)
    -- Entry block: alloca failures + first test dispatch
    let entryBlock : LLVMBlock := match testFns with
      | f0 :: _ =>
        let entryInstrs : List LLVMInstr :=
          [.alloca "failures" .i32, .store .i32 (.intLit 0) (.reg "failures")]
          ++ mkTestDispatch f0 "0"
        ⟨"entry", entryInstrs, .condBr (.reg "is_pass.0") "pass.0" "fail.0"⟩
      | [] => ⟨"entry", [], .ret .i32 (some (.intLit 0))⟩  -- unreachable
    let allBlocks := [entryBlock] ++ blocks
    let mainFn : LLVMFnDef := { name := "main", retTy := .i32, params := [], blocks := allBlocks }
    { s with moduleFunctions := s.moduleFunctions.push mainFn }

def emitSSAProgram (modules : List SModule) (testMode : Bool := false) (moduleFilter : Option String := none) : String :=
  let s : EmitSSAState := {}
  -- Collect all structs and enums for type resolution
  let allStructs := modules.foldl (fun acc m => acc ++ m.structs) []
  let allEnums := modules.foldl (fun acc m => acc ++ m.enums) []
  -- Canonical builtin enum definitions with type parameters
  let optionDef : CEnumDef :=
    { name := optionEnumName, typeParams := ["T"],
      variants := [("Some", [("value", .typeVar "T")]), ("None", [])],
      builtinId := some .option }
  let resultDef : CEnumDef :=
    { name := resultEnumName, typeParams := ["T", "E"],
      variants := [(okVariantName, [("value", .typeVar "T")]), (errVariantName, [("value", .typeVar "E")])],
      builtinId := some .result }
  let builtinEnums : List CEnumDef := [optionDef, resultDef]
  let s := { s with structDefs := allStructs, enumDefs := builtinEnums ++ allEnums }
  -- Header
  let s := { s with moduleHeader := s.moduleHeader.push "; Generated by Concrete compiler (SSA path)" }
  -- Emit target triple so LLVM applies the correct ABI (struct passing, alignment, etc.)
  let s := { s with moduleHeader := s.moduleHeader.push "target datalayout = \"e-m:o-i64:64-i128:128-n32:64-S128-Fn32\"" }
  let s := { s with moduleHeader := s.moduleHeader.push "target triple = \"arm64-apple-macosx14.0.0\"" }
  -- Well-known struct types (String, Vec)
  let s := Layout.builtinTypeDefs.foldl (fun s line => emitTypeDef s line) s
  -- Mark builtins as emitted so user-defined versions don't duplicate them
  let s := { s with emittedTypes := ["String", "Vec"] ++ s.emittedTypes }
  -- Whole-program monomorphic ABI for builtin generic enums:
  -- Scan all SSA modules for concrete type arguments, then emit a single LLVM type
  -- definition sized to the largest payload across all instantiations.
  -- Smaller payloads under-fill the slot (wasted padding) but are correct.
  -- Builtin functions (vec_pop, etc.) store i64 payloads, which always fit.
  -- NOTE: GEP offsets in Lower.lean assume tyAlign(.typeVar "T") = 8, which is correct
  -- only because all current Concrete types have alignment ≤ 8. If larger-aligned types
  -- are added, Lower.lean will need to thread concrete type args through offset computation.
  let ctx := layoutCtxOf s
  let (bestOpt, bestRes) := scanBuiltinEnumArgs ctx modules
  -- Generate dynamic Option type def
  let optTypeArgs := match bestOpt with
    | some t => [t]
    | none => [Ty.int]  -- fallback: i64 payload
  let optTypeDefs := Layout.enumTypeDefs ctx optionDef optTypeArgs
  let s := optTypeDefs.foldl (fun s line => emitTypeDef s line) s
  -- Generate dynamic Result type def
  let resTypeArgs := match bestRes with
    | some (ok, err) => [ok, err]
    | none => [Ty.int, Ty.int]  -- fallback: i64 payloads
  let resTypeDefs := Layout.enumTypeDefs ctx resultDef resTypeArgs
  let s := resTypeDefs.foldl (fun s line => emitTypeDef s line) s
  -- Mark these as emitted so user enums with the same names won't duplicate
  let s := { s with emittedTypes := [resultEnumName, optionEnumName] ++ s.emittedTypes }
  -- External declarations (skip externs that shadow defined functions)
  let allExternFns := modules.foldl (fun acc m => acc ++ m.externFns) []
  let allDefinedFns := modules.foldl (fun acc m => acc ++ m.functions.map (·.name)) []
  let s := emitExternDecls s allExternFns allDefinedFns
  -- Emit each module
  let s := modules.foldl (fun s m => emitSModule s m testMode) s
  -- In test mode, emit the test runner instead of the normal main wrapper
  let s := if testMode then emitTestRunner s modules moduleFilter else s
  -- Emit builtin function implementations
  let s := emitBuiltins s
  -- Assemble the final LLVMModule and print it
  -- Deduplicate declarations: remove duplicates by name and declarations
  -- that collide with a defined function (user code takes precedence)
  let fnNames := s.moduleFunctions.toList.map (·.name)
  let dedupDecls := s.moduleDeclarations.toList.foldl (fun (acc : List LLVMFnDecl) d =>
    if acc.any (·.name == d.name) || fnNames.contains d.name then acc
    else acc ++ [d]) []
  let llvmModule : LLVMModule := {
    header := s.moduleHeader.toList
    typeDefs := s.moduleTypeDefs.toList
    globals := s.moduleGlobals.toList
    declarations := dedupDecls
    functions := s.moduleFunctions.toList
  }
  printLLVMModule llvmModule

end Concrete
