import Concrete.Frontend.AST
import Concrete.Resolve.Intrinsic
import Concrete.Semantics.IntArith
import Concrete.Semantics.Capabilities

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

/-- Display name of a fixed-width integer type, for range-error messages. -/
private def intTyDisplayName : Ty → Option String
  | .i8 => "i8" | .i16 => "i16" | .i32 => "i32" | .int => "Int"
  | .u8 => "u8" | .u16 => "u16" | .u32 => "u32" | .uint => "Uint"
  | _ => none

/-- Inclusive `(min, max, display-name)` for a fixed-width integer type, used to
    reject literals that cannot fit (e.g. `let a: u8 = 300`). `none` for
    non-integer types (and `char`, which is not literal-range-checked). The
    numeric range comes from the arithmetic reference (`IntArith.intRange`); only
    the display name is owned here. -/
def intTyRange (ty : Ty) : Option (Int × Int × String) :=
  match IntArith.intRange ty, intTyDisplayName ty with
  | some (lo, hi), some nm => some (lo, hi, nm)
  | _, _ => none

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

-- The capability superset check now lives in the one capability-fact source
-- (`Concrete.Capabilities`); re-exported here so existing `capsContain` callers
-- are unchanged (Phase 6.5 #5).
export Concrete.Capabilities (capsContain)

/-- Resolve `Self` to the concrete impl type in a Ty (pure, for signature building).
    Handles all type constructors including ptrMut, ptrConst, fn_. -/
def resolveSelfTy : Ty → Ty → Ty
  | .named "Self", implTy => implTy
  | .ref inner, implTy => .ref (resolveSelfTy inner implTy)
  | .refMut inner, implTy => .refMut (resolveSelfTy inner implTy)
  | .ptrMut inner, implTy => .ptrMut (resolveSelfTy inner implTy)
  | .ptrConst inner, implTy => .ptrConst (resolveSelfTy inner implTy)
  | .generic name args caps, implTy => .generic name (args.map (resolveSelfTy · implTy)) caps
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
  | .named n => n | .generic n _ _ => n
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
  | .generic n args caps => .generic n (args.map (substSelf · replacement)) caps
  | .fn_ params caps ret => .fn_ (params.map (substSelf · replacement)) caps (substSelf ret replacement)
  | t => t

/-- Every capability name a type mentions, in function-pointer sets and capability
    arguments, at any depth. -/
partial def tyCapNames : Ty → List String
  | .fn_ ps cs r => cs.allNames ++ ps.flatMap tyCapNames ++ tyCapNames r
  | .generic _ args caps => caps.flatMap CapSet.allNames ++ args.flatMap tyCapNames
  | .ref t | .refMut t | .ptrMut t | .ptrConst t | .heap t | .heapArray t => tyCapNames t
  | .array t _ => tyCapNames t
  | _ => []

/-- Substitute capability parameters, by name, inside function-pointer capability sets
    and nested capability arguments. `m` maps a parameter name to the set it stands for.
    A name not in `m` is kept. -/
partial def substCapNamesTy (m : List (String × CapSet)) : Ty → Ty
  | .fn_ ps cs r =>
    .fn_ (ps.map (substCapNamesTy m)) (substCapNamesSet m cs) (substCapNamesTy m r)
  | .generic n args caps =>
    .generic n (args.map (substCapNamesTy m)) (caps.map fun c => (substCapNamesSet m c).canonArg)
  | .ref t => .ref (substCapNamesTy m t)
  | .refMut t => .refMut (substCapNamesTy m t)
  | .ptrMut t => .ptrMut (substCapNamesTy m t)
  | .ptrConst t => .ptrConst (substCapNamesTy m t)
  | .heap t => .heap (substCapNamesTy m t)
  | .heapArray t => .heapArray (substCapNamesTy m t)
  | .array t k => .array (substCapNamesTy m t) k
  | t => t
where
  substCapNamesSet (m : List (String × CapSet)) (cs : CapSet) : CapSet :=
    let names := cs.allNames.flatMap fun n =>
      match m.lookup n with
      | some sub => sub.allNames
      | none => [n]
    let names := names.mergeSort (· < ·) |>.eraseDups
    match cs with
    | .empty => .empty
    | _ => if names.isEmpty then .empty else .concrete names

/-- Read capability-parameter bindings off `actual` by matching it against `pattern`
    (R-0484). Where the pattern has `with(C)` (or `with(C, Alloc)`) and the actual
    function-pointer type has `with(Console, Alloc)`, `C` is bound to what the actual set
    has beyond the pattern's own concrete names. Where the pattern has capability argument
    `C` (`Sink<C>`) and the actual has `Sink<Console>`, `C` is bound to `Console`. A
    parameter that cannot be read off is simply not bound: the caller decides whether
    that is an error. -/
partial def inferCapBindings (capParams : List String) (pattern actual : Ty) : List (String × CapSet) :=
  match pattern, actual with
  | .fn_ pps pcs pr, .fn_ aps acs ar =>
    let pNames := pcs.allNames
    let vars := pNames.filter capParams.contains
    let fixed := pNames.filter (fun n => !capParams.contains n)
    let here := match vars with
      | [v] =>
        let rest := acs.allNames.filter (fun n => !fixed.contains n)
        [(v, (if rest.isEmpty then CapSet.empty else CapSet.concrete rest).canonArg)]
      | _ => []
    here ++ (pps.zip aps).flatMap (fun (p, a) => inferCapBindings capParams p a)
         ++ inferCapBindings capParams pr ar
  | .generic pn pargs pcaps, .generic an aargs acaps =>
    if pn != an then [] else
    let here := (pcaps.zip acaps).filterMap fun (pc, ac) =>
      match pc.allNames with
      | [v] => if capParams.contains v then some (v, ac.canonArg) else none
      | _ => none
    here ++ (pargs.zip aargs).flatMap (fun (p, a) => inferCapBindings capParams p a)
  | .ref p, .ref a | .refMut p, .refMut a | .ref p, .refMut a => inferCapBindings capParams p a
  | .ptrMut p, .ptrMut a | .ptrConst p, .ptrConst a => inferCapBindings capParams p a
  | .heap p, .heap a | .heapArray p, .heapArray a => inferCapBindings capParams p a
  | .array p _, .array a _ => inferCapBindings capParams p a
  | _, _ => []

/-- Infer generic type-argument bindings by structurally matching a parameter
    `pattern` type against the `actual` argument type, collecting `(typeParam,
    ty)` pairs (Phase 6.5 InstantiationJudgment axis). This is the ONE type-arg
    inference used by both Check (call type-checking) and Elab (`CExpr` type-arg
    stamping) — it was duplicated as two hand-maintained copies that could drift
    on which constructors unify. Capability sets carry no type-var bindings, so
    `fn_`'s cap set is not matched. -/
partial def unifyTypes (pattern actual : Ty) (typeParams : List String) : List (String × Ty) :=
  match pattern with
  | .named name => if typeParams.contains name then [(name, actual)] else []
  | .typeVar name => if typeParams.contains name then [(name, actual)] else []
  | .ref inner => match actual with
    | .ref a => unifyTypes inner a typeParams
    | _ => []
  | .refMut inner => match actual with
    | .refMut a => unifyTypes inner a typeParams
    | _ => []
  -- Type parameters only; capability arguments bind capability variables, a separate step.
  | .generic _ pArgs _ => match actual with
    | .generic _ aArgs _ =>
      (pArgs.zip aArgs).foldl (fun acc (pp, ap) => acc ++ unifyTypes pp ap typeParams) []
    | _ => []
  | .heap inner => match actual with
    | .heap a => unifyTypes inner a typeParams
    | _ => []
  | .array elem _ => match actual with
    | .array aElem _ => unifyTypes elem aElem typeParams
    | _ => []
  | .fn_ pParams _ pRet => match actual with
    | .fn_ aParams _ aRet =>
      let pb := (pParams.zip aParams).foldl (fun acc (pp, ap) => acc ++ unifyTypes pp ap typeParams) []
      pb ++ unifyTypes pRet aRet typeParams
    | _ => []
  | _ => []

end Concrete
