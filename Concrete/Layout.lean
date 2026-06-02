import Concrete.Core

namespace Concrete
namespace Layout

/-! ## Layout — unified type layout and ABI helpers

Single source of truth for type sizes, field offsets, pass-by-pointer decisions,
and Ty→LLVM type mappings. Used by both Lower.lean and EmitSSA.lean.
-/

-- ============================================================
-- Layout context
-- ============================================================

structure Ctx where
  structDefs : List CStructDef
  enumDefs   : List CEnumDef
  /-- Newtypes visible to layout. A named/generic type whose name matches a
      newtype is transparently unwrapped before size/alignment is computed.
      Elab already erases newtypes inside struct/enum fields; this handles
      the remaining cases (generic args, function bodies, enum payloads). -/
  newtypes   : List NewtypeDef := []

def lookupStruct (ctx : Ctx) (name : String) : Option CStructDef :=
  ctx.structDefs.find? fun sd => sd.name == name

def lookupEnum (ctx : Ctx) (name : String) : Option CEnumDef :=
  ctx.enumDefs.find? fun ed => ed.name == name

def lookupNewtype (ctx : Ctx) (name : String) : Option NewtypeDef :=
  ctx.newtypes.find? fun nt => nt.name == name

/-- Substitute type variables inside a newtype's inner type. -/
partial def ntSubstTy (mapping : List (String × Ty)) : Ty → Ty
  | .named n => match mapping.lookup n with | some t => t | none => .named n
  | .typeVar n => match mapping.lookup n with | some t => t | none => .typeVar n
  | .ref i => .ref (ntSubstTy mapping i)
  | .refMut i => .refMut (ntSubstTy mapping i)
  | .ptrMut i => .ptrMut (ntSubstTy mapping i)
  | .ptrConst i => .ptrConst (ntSubstTy mapping i)
  | .heap i => .heap (ntSubstTy mapping i)
  | .heapArray i => .heapArray (ntSubstTy mapping i)
  | .array e n => .array (ntSubstTy mapping e) n
  | .generic n args => .generic n (args.map (ntSubstTy mapping))
  | .fn_ ps c r => .fn_ (ps.map (ntSubstTy mapping)) c (ntSubstTy mapping r)
  | t => t

/-- If `ty` is a newtype-named or newtype-generic, unwrap it to its inner type.
    Recurses to handle newtype-of-newtype. Returns `ty` unchanged otherwise. -/
partial def resolveNewtype (ctx : Ctx) : Ty → Ty
  | .named name =>
    match lookupNewtype ctx name with
    | some nt => resolveNewtype ctx nt.innerTy
    | none => .named name
  | .generic name args =>
    match lookupNewtype ctx name with
    | some nt =>
      let mapping := nt.typeParams.zip args
      resolveNewtype ctx (ntSubstTy mapping nt.innerTy)
    | none => .generic name args
  | t => t

-- ============================================================
-- Builtin type layout constants
-- ============================================================

namespace Builtin
  def stringSize : Nat := 24   -- ptr + i64 + i64
  def stringAlign : Nat := 8
  def vecSize : Nat := 24      -- ptr + i64 + i64
  def vecAlign : Nat := 8
  def hashmapSize : Nat := 40  -- ptr + ptr + ptr + i64 + i64
  def hashmapAlign : Nat := 8
end Builtin

-- ============================================================
-- Type argument substitution for generic enums
-- ============================================================

/-- Substitute type variables in a type using a mapping. -/
partial def substTyVars (subst : List (String × Ty)) : Ty → Ty
  | .typeVar name => match subst.find? fun (p, _) => p == name with
    | some (_, actual) => actual
    | none => .typeVar name
  | .named name => match subst.find? fun (p, _) => p == name with
    | some (_, actual) => actual
    | none => .named name
  | .ref inner => .ref (substTyVars subst inner)
  | .refMut inner => .refMut (substTyVars subst inner)
  | .ptrMut inner => .ptrMut (substTyVars subst inner)
  | .ptrConst inner => .ptrConst (substTyVars subst inner)
  | .heap inner => .heap (substTyVars subst inner)
  | .heapArray inner => .heapArray (substTyVars subst inner)
  | .generic name args => .generic name (args.map (substTyVars subst))
  | .array elem n => .array (substTyVars subst elem) n
  | .fn_ params capSet retTy => .fn_ (params.map (substTyVars subst)) capSet (substTyVars subst retTy)
  | ty => ty

/-- Substitute type variables in a CStructDef with concrete type arguments. -/
def substStructTypeArgs (sd : CStructDef) (typeArgs : List Ty) : CStructDef :=
  if sd.typeParams.isEmpty || typeArgs.isEmpty then sd
  else
    let subst := sd.typeParams.zip typeArgs
    { sd with fields := sd.fields.map fun (fn, ft) => (fn, substTyVars subst ft) }

/-- Substitute type variables in a CEnumDef with concrete type arguments. -/
def substEnumTypeArgs (ed : CEnumDef) (typeArgs : List Ty) : CEnumDef :=
  if ed.typeParams.isEmpty || typeArgs.isEmpty then ed
  else
    let subst := ed.typeParams.zip typeArgs
    { ed with variants := ed.variants.map fun (vn, fields) =>
      (vn, fields.map fun (fn, ft) => (fn, substTyVars subst ft)) }

-- ============================================================
-- Type size (bytes)
-- ============================================================

/-- Compute alignment for a named or generic type (struct or enum). -/
private partial def tyAlign_namedOrGeneric (ctx : Ctx) (tyAlignFn : Ctx → Ty → Nat) (name : String) (typeArgs : List Ty) : Nat :=
  match lookupStruct ctx name with
  | some sd =>
    let sd := substStructTypeArgs sd typeArgs
    if sd.isPacked then 1
    else
      let natural := sd.fields.foldl (fun maxA (_, ft) => Nat.max maxA (tyAlignFn ctx ft)) 1
      match sd.reprAlign with
      | some a => Nat.max natural a
      | none => natural
  | none =>
    match lookupEnum ctx name with
    | some ed =>
      let ed := substEnumTypeArgs ed typeArgs
      Nat.max 4 (ed.variants.foldl (fun maxA (_, vfields) =>
        vfields.foldl (fun a (_, ft) => Nat.max a (tyAlignFn ctx ft)) maxA) 1)
    | none =>
      panic! s!"Layout.tyAlign: unknown named type '{name}'"

/-- Natural alignment of a type in bytes. -/
partial def tyAlign (ctx : Ctx) : Ty → Nat
  | .int | .uint | .float64 => 8
  | .i32 | .u32 | .float32 => 4
  | .i16 | .u16 => 2
  | .i8 | .u8 | .char | .bool => 1
  | .unit => 1
  | .string => Builtin.stringAlign
  | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ => 8
  | .fn_ _ _ _ | .heap _ | .heapArray _ => 8
  | .generic "Heap" _ | .generic "HeapArray" _ => 8
  | .generic "Vec" _ => Builtin.vecAlign
  | .generic "HashMap" _ => Builtin.hashmapAlign
  | .named name =>
    match lookupNewtype ctx name with
    | some _ => tyAlign ctx (resolveNewtype ctx (.named name))
    | none => tyAlign_namedOrGeneric ctx tyAlign name []
  | .generic name args =>
    match lookupNewtype ctx name with
    | some _ => tyAlign ctx (resolveNewtype ctx (.generic name args))
    | none => tyAlign_namedOrGeneric ctx tyAlign name args
  | .array elem _ => tyAlign ctx elem
  | .never | .placeholder => 1
  | .typeVar _ => 8

/-- Round `n` up to the next multiple of `align`. -/
def alignUp (n : Nat) (align : Nat) : Nat :=
  if align <= 1 then n
  else ((n + align - 1) / align) * align

/-- Compute size for a named or generic type (struct or enum). -/
private partial def tySize_namedOrGeneric (ctx : Ctx) (tySizeFn : Ctx → Ty → Nat) (tyAlignFn : Ctx → Ty → Nat) (name : String) (typeArgs : List Ty) : Nat :=
  match lookupStruct ctx name with
  | some sd =>
    let sd := substStructTypeArgs sd typeArgs
    if sd.isPacked then
      sd.fields.foldl (fun acc (_, ft) => acc + tySizeFn ctx ft) 0
    else
      let (sz, _) := sd.fields.foldl (fun (acc, _) (_, ft) =>
        let aligned := alignUp acc (tyAlignFn ctx ft)
        (aligned + tySizeFn ctx ft, ())) (0, ())
      let structAlign := if typeArgs.isEmpty then tyAlignFn ctx (.named name) else tyAlignFn ctx (.generic name typeArgs)
      alignUp sz structAlign
  | none =>
    match lookupEnum ctx name with
    | some ed =>
      let ed := substEnumTypeArgs ed typeArgs
      let maxPayload := ed.variants.foldl (fun maxSz (_, vfields) =>
        let sz := vfields.foldl (fun (acc, _) (_, ft) =>
          let aligned := alignUp acc (tyAlignFn ctx ft)
          (aligned + tySizeFn ctx ft, ())) (0, ())
        Nat.max maxSz sz.1) 0
      let payloadAlign := ed.variants.foldl (fun maxA (_, vfields) =>
        vfields.foldl (fun a (_, ft) => Nat.max a (tyAlignFn ctx ft)) maxA) 1
      let payloadStart := alignUp 4 payloadAlign
      alignUp (payloadStart + maxPayload) (Nat.max 4 payloadAlign)
    | none =>
      panic! s!"Layout.tySize: unknown named type '{name}'"

/-- Byte size of a type. Used for malloc/alloca sizing and enum layout. -/
partial def tySize (ctx : Ctx) : Ty → Nat
  | .int | .uint | .float64 => 8
  | .i32 | .u32 | .float32 => 4
  | .i16 | .u16 => 2
  | .i8 | .u8 | .char | .bool => 1
  | .unit => 0
  | .string => Builtin.stringSize
  | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ => 8
  | .fn_ _ _ _ | .heap _ | .heapArray _ => 8
  | .generic "Heap" _ | .generic "HeapArray" _ => 8
  | .generic "Vec" _ => Builtin.vecSize
  | .generic "HashMap" _ => Builtin.hashmapSize
  | .named name =>
    match lookupNewtype ctx name with
    | some _ => tySize ctx (resolveNewtype ctx (.named name))
    | none => tySize_namedOrGeneric ctx tySize tyAlign name []
  | .generic name args =>
    match lookupNewtype ctx name with
    | some _ => tySize ctx (resolveNewtype ctx (.generic name args))
    | none => tySize_namedOrGeneric ctx tySize tyAlign name args
  | .array elem n => tySize ctx elem * n
  | .never | .placeholder => 0
  | .typeVar _ => 8

/-- Byte offset of a field within a struct. Accepts optional type args for generic structs. -/
def fieldOffset (ctx : Ctx) (structName fieldName : String) (typeArgs : List Ty := []) : Nat :=
  match lookupStruct ctx structName with
  | some sd =>
    let sd := substStructTypeArgs sd typeArgs
    if sd.isPacked then
      -- Packed: no alignment padding
      let (offset, _) := sd.fields.foldl (fun (acc : Nat × Bool) (n, t) =>
        let (off, found) := acc
        if found then (off, true)
        else if n == fieldName then (off, true)
        else (off + tySize ctx t, false)) (0, false)
      offset
    else
      let (offset, _) := sd.fields.foldl (fun (acc : Nat × Bool) (n, t) =>
        let (off, found) := acc
        if found then (off, true)
        else
          let aligned := alignUp off (tyAlign ctx t)
          if n == fieldName then (aligned, true)
          else (aligned + tySize ctx t, false)) (0, false)
      offset
  | none =>
    panic! s!"Layout.fieldOffset: struct '{structName}' not found"

/-- Maximum payload size across all variants of an enum. -/
def enumPayloadSize (ctx : Ctx) (ed : CEnumDef) : Nat :=
  ed.variants.foldl (fun maxSz (_, vfields) =>
    let sz := vfields.foldl (fun (acc, _) (_, ft) =>
      let aligned := alignUp acc (tyAlign ctx ft)
      (aligned + tySize ctx ft, ())) (0, ())
    Nat.max maxSz sz.1) 0

/-- Byte offset from start of enum to payload (after i32 tag, aligned). -/
def enumPayloadOffset (ctx : Ctx) (ed : CEnumDef) (typeArgs : List Ty := []) : Nat :=
  let ed := substEnumTypeArgs ed typeArgs
  let payloadAlign := ed.variants.foldl (fun maxA (_, vfields) =>
    vfields.foldl (fun a (_, ft) => Nat.max a (tyAlign ctx ft)) maxA) 1
  alignUp 4 payloadAlign

/-- Byte offset of a field within a variant's payload (aligned).
    Fields should already be substituted (use substEnumTypeArgs before extracting). -/
def variantFieldOffset (ctx : Ctx) (fields : List (String × Ty)) (idx : Nat) : Nat :=
  let (off, _, _) := fields.foldl (fun (acc, i, done) (_, ft) =>
    if done then (acc, i + 1, true)
    else
      let aligned := alignUp acc (tyAlign ctx ft)
      if i == idx then (aligned, i + 1, true)
      else (aligned + tySize ctx ft, i + 1, false)) (0, 0, false)
  off

-- ============================================================
-- Pass-by-pointer ABI
-- ============================================================

/-- Is this type passed by pointer in function calls? -/
partial def isPassByPtr (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .string => true
  | .ref _ | .refMut _ => true
  | .array _ _ => true
  | .fn_ _ _ _ | .heap _ | .heapArray _ => false
  | .named name =>
    match lookupStruct ctx name with
    | some _ => true
    | none =>
      match lookupEnum ctx name with
      | some _ => true
      | none =>
        match lookupNewtype ctx name with
        | some _ => isPassByPtr ctx (resolveNewtype ctx (.named name))
        | none =>
          panic! s!"Layout.isPassByPtr: unknown named type '{name}'"
  | .generic "Vec" _ | .generic "HashMap" _ => true
  | .generic name args =>
    match lookupStruct ctx name with
    | some _ => true
    | none =>
      match lookupEnum ctx name with
      | some _ => true
      | none =>
        match lookupNewtype ctx name with
        | some _ => isPassByPtr ctx (resolveNewtype ctx (.generic name args))
        | none =>
          panic! s!"Layout.isPassByPtr: unknown generic type '{name}'"
  | _ => false

-- ============================================================
-- Ty → LLVM type string
-- ============================================================

/-- Map a Concrete type to its LLVM IR type string. -/
partial def tyToLLVM (ctx : Ctx) : Ty → String
  | .int => "i64"
  | .uint => "i64"
  | .i8 | .u8 => "i8"
  | .i16 | .u16 => "i16"
  | .i32 | .u32 => "i32"
  | .bool => "i1"
  | .float64 => "double"
  | .float32 => "float"
  | .char => "i8"
  | .unit => "void"
  | .string => "%struct.String"
  | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ => "ptr"
  | .generic "Heap" _ | .heap _ => "ptr"
  | .generic "HeapArray" _ | .heapArray _ => "ptr"
  | .generic "Vec" _ => "%struct.Vec"
  | .generic "HashMap" _ => "%struct.HashMap"
  | .generic name args =>
    match lookupEnum ctx name with
    | some _ => "%enum." ++ name
    | none =>
      match lookupStruct ctx name with
      | some _ => "%struct." ++ name
      | none =>
        match lookupNewtype ctx name with
        | some _ => tyToLLVM ctx (resolveNewtype ctx (.generic name args))
        | none => "%struct." ++ name
  | .typeVar _ => "i64"
  | .array elem n => "[" ++ toString n ++ " x " ++ tyToLLVM ctx elem ++ "]"
  | .fn_ _ _ _ => "ptr"
  | .never => "void"
  | .placeholder => "i64"
  | .named name =>
    match lookupStruct ctx name with
    | some _ => "%struct." ++ name
    | none =>
      match lookupEnum ctx name with
      | some _ => "%enum." ++ name
      | none =>
        match lookupNewtype ctx name with
        | some _ => tyToLLVM ctx (resolveNewtype ctx (.named name))
        | none =>
          panic! s!"Layout.tyToLLVM: unknown named type '{name}'"

/-- LLVM type for function parameters (pass-by-ptr types become ptr). -/
def paramTyToLLVM (ctx : Ctx) (ty : Ty) : String :=
  if isPassByPtr ctx ty then "ptr"
  else tyToLLVM ctx ty

-- ============================================================
-- LLVM type definition generators
-- ============================================================

/-- Generate LLVM type definition for a struct. -/
def structTypeDef (ctx : Ctx) (sd : CStructDef) : String :=
  let fieldTypes := ", ".intercalate (sd.fields.map fun (_, t) => tyToLLVM ctx t)
  if sd.isPacked then s!"%struct.{sd.name} = type <\{ {fieldTypes} }>"
  else s!"%struct.{sd.name} = type \{ {fieldTypes} }"

/-- Generate LLVM type definitions for an enum (variant types + tagged union).
    For generic enums, pass typeArgs to get the correct payload size from substitution. -/
def enumTypeDefs (ctx : Ctx) (ed : CEnumDef) (typeArgs : List Ty := []) : List String :=
  let substEd := substEnumTypeArgs ed typeArgs
  let variantDefs := substEd.variants.map fun (vn, fields) =>
    if fields.isEmpty then s!"%variant.{ed.name}.{vn} = type \{}"
    else
      let fieldTypes := ", ".intercalate (fields.map fun (_, t) => tyToLLVM ctx t)
      s!"%variant.{ed.name}.{vn} = type \{ {fieldTypes} }"
  -- Compute total size using the substituted enum's fields directly
  let maxPayload := substEd.variants.foldl (fun maxSz (_, vfields) =>
    let sz := vfields.foldl (fun (acc, _) (_, ft) =>
      let aligned := alignUp acc (tyAlign ctx ft)
      (aligned + tySize ctx ft, ())) (0, ())
    Nat.max maxSz sz.1) 0
  let payloadAlign := substEd.variants.foldl (fun maxA (_, vfields) =>
    vfields.foldl (fun a (_, ft) => Nat.max a (tyAlign ctx ft)) maxA) 1
  let payloadStart := alignUp 4 payloadAlign
  let totalSize := alignUp (payloadStart + maxPayload) (Nat.max 4 payloadAlign)
  let payloadBytes := if totalSize <= 4 then 1 else totalSize - 4
  let enumDef := s!"%enum.{ed.name} = type \{ i32, [{payloadBytes} x i8] }"
  variantDefs ++ [enumDef]

/-- LLVM type definitions for builtin types (String, Vec). -/
def builtinTypeDefs : List String :=
  [ "%struct.String = type { ptr, i64, i64 }"
  , "%struct.Vec = type { ptr, i64, i64 }" ]

-- ============================================================
-- FFI safety
-- ============================================================

/-- Is a type FFI-safe? Used by CoreCheck for extern fn and repr(C) validation. -/
def isFFISafe (ctx : Ctx) (ty : Ty) : Bool :=
  match ty with
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | .float32 | .float64 => true
  | .bool | .char | .unit => true
  | .ptrMut _ | .ptrConst _ => true
  | .named name => (lookupStruct ctx name).any fun sd => sd.isReprC
  | _ => false

end Layout
end Concrete
