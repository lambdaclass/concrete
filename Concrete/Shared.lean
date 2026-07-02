import Concrete.AST
import Concrete.Intrinsic

namespace Concrete

/-! ## Shared semantic helpers

Used by Check, CoreCheck, and other passes that need type classification
or capability comparison.
-/

/-- Is this a numeric type (supports arithmetic operators)? -/
def isNumeric : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | .float64 | .float32 => true
  | _ => false

/-- Is this an integer type (supports comparison and bitwise operators)? -/
def isInteger : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | _ => false

/-- Inclusive `(min, max, display-name)` for a fixed-width integer type, used to
    reject literals that cannot fit (e.g. `let a: u8 = 300`). `none` for
    non-integer types (and `char`, which is not literal-range-checked). -/
def intTyRange : Ty → Option (Int × Int × String)
  | .i8   => some (-128, 127, "i8")
  | .i16  => some (-32768, 32767, "i16")
  | .i32  => some (-2147483648, 2147483647, "i32")
  | .int  => some (-9223372036854775808, 9223372036854775807, "Int")
  | .u8   => some (0, 255, "u8")
  | .u16  => some (0, 65535, "u16")
  | .u32  => some (0, 4294967295, "u32")
  | .uint => some (0, 18446744073709551615, "Uint")
  | _ => none

/-- Check if two types are compatible (equal or both numeric). -/
def typesCompatible (a b : Ty) : Bool :=
  a == b || (isNumeric a && isNumeric b) ||
  -- ptrMut and ptrConst with same inner type are compatible
  (match a, b with
   | .ptrMut t1, .ptrConst t2 | .ptrConst t1, .ptrMut t2 => t1 == t2
   | _, _ => false)

/-- Strict operand agreement for binary operators: exactly the same type, or the
    mut/const pointer pairing. Unlike `typesCompatible`, numeric operands must
    match in BOTH width and signedness — a mixed pair (i8 + i32, u8 < Int, ...)
    has no single-width SSA lowering and used to escape the front-end only to
    die at SSA-verify (E0715). -/
def binOpOperandsAgree (a b : Ty) : Bool :=
  a == b ||
  (match a, b with
   | .ptrMut t1, .ptrConst t2 | .ptrConst t1, .ptrMut t2 => t1 == t2
   | _, _ => false)

/-- Check if capSet `caller` is a superset of `callee`. -/
def capsContain (caller callee : CapSet) : Bool :=
  match callee with
  | .empty => true
  | .concrete calleeCaps =>
    match caller with
    | .empty => calleeCaps.isEmpty
    | .concrete callerCaps => calleeCaps.all fun c => callerCaps.contains c
    | .var _ => true  -- capability variable assumed to satisfy
    | .union a b => capsContain a callee || capsContain b callee
  | .var _ => true  -- capability variable, can't check statically here
  | .union a b => capsContain caller a && capsContain caller b

/-- Resolve `Self` to the concrete impl type in a Ty (pure, for signature building).
    Handles all type constructors including ptrMut, ptrConst, fn_. -/
def resolveSelfTy : Ty → Ty → Ty
  | .named "Self", implTy => implTy
  | .ref inner, implTy => .ref (resolveSelfTy inner implTy)
  | .refMut inner, implTy => .refMut (resolveSelfTy inner implTy)
  | .ptrMut inner, implTy => .ptrMut (resolveSelfTy inner implTy)
  | .ptrConst inner, implTy => .ptrConst (resolveSelfTy inner implTy)
  | .generic name args, implTy => .generic name (args.map (resolveSelfTy · implTy))
  | .array elem n, implTy => .array (resolveSelfTy elem implTy) n
  | .fn_ params cs ret, implTy => .fn_ (params.map (resolveSelfTy · implTy)) cs (resolveSelfTy ret implTy)
  | .heap inner, implTy => .heap (resolveSelfTy inner implTy)
  | .heapArray inner, implTy => .heapArray (resolveSelfTy inner implTy)
  | other, _ => other

/-- Map a type name string to its primitive Ty, or `.named` for user types.
    Mirrors the parser's type name → Ty mapping. -/
def tyFromName (name : String) : Ty :=
  match name with
  | "Int" | "i64" => .int
  | "Uint" | "u64" => .uint
  | "i8"  => .i8  | "i16" => .i16 | "i32" => .i32
  | "u8"  => .u8  | "u16" => .u16 | "u32" => .u32
  | "Bool" | "bool" => .bool
  | "Float64" | "f64" => .float64
  | "Float32" | "f32" => .float32
  | "Char" | "char" => .char
  | "String" => .string
  | n => .named n

/-- Extract the type name string from a Ty (inverse of tyFromName for primitives). -/
def tyName : Ty → String
  | .int => "Int" | .uint => "Uint"
  | .i8 => "i8" | .i16 => "i16" | .i32 => "i32"
  | .u8 => "u8" | .u16 => "u16" | .u32 => "u32"
  | .float64 => "Float64" | .float32 => "Float32"
  | .bool => "Bool" | .char => "Char" | .string => "String"
  | .named n => n | .generic n _ => n
  | _ => ""

/-- Replace every occurrence of `.named "Self"` with `replacement` inside a Ty. -/
partial def substSelf (ty : Ty) (replacement : Ty) : Ty :=
  match ty with
  | .named n => if n == selfTypeName then replacement else .named n
  | .ref t => .ref (substSelf t replacement)
  | .refMut t => .refMut (substSelf t replacement)
  | .ptrMut t => .ptrMut (substSelf t replacement)
  | .ptrConst t => .ptrConst (substSelf t replacement)
  | .array t n => .array (substSelf t replacement) n
  | .generic n args => .generic n (args.map (substSelf · replacement))
  | .fn_ params caps ret => .fn_ (params.map (substSelf · replacement)) caps (substSelf ret replacement)
  | t => t

end Concrete
