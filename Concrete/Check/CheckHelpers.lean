import Concrete.Frontend.AST
import Concrete.Semantics.RefWellFormed
import Concrete.Resolve.BuiltinSigs
import Concrete.Report.Diagnostic
import Concrete.Resolve.FileSummary
import Concrete.Resolve.Intrinsic
import Concrete.Resolve.Resolve
import Concrete.Resolve.Shared
import Concrete.Resolve.PeekType
import Concrete.Check.CheckError
import Concrete.Check.Layout

namespace Concrete

/-- Source spelling of a binary operator, for diagnostics (E0228). -/
def binOpSymbol : BinOp → String
  | .add => "+" | .sub => "-" | .mul => "*" | .div => "/" | .mod => "%"
  | .eq => "==" | .neq => "!=" | .lt => "<" | .gt => ">" | .leq => "<=" | .geq => ">="
  | .and_ => "&&" | .or_ => "||"
  | .bitand => "&" | .bitor => "|" | .bitxor => "^" | .shl => "<<" | .shr => ">>"
  | .wrappingAdd => "wrapping_add" | .wrappingSub => "wrapping_sub" | .wrappingMul => "wrapping_mul"
  | .saturatingAdd => "saturating_add" | .saturatingSub => "saturating_sub" | .saturatingMul => "saturating_mul"

-- Bug 206: there is no message-only refusal. Every Check refusal is a `CheckError`, whose
-- constructor fixes its code (`CheckError.code`), and `Diagnostic.code` has no default, so a
-- diagnostic without a code does not typecheck.

-- ============================================================
-- Helpers
-- ============================================================

def enumerateList (l : List α) (idx : Nat := 0) : List (Nat × α) :=
  match l with
  | [] => []
  | a :: rest => (idx, a) :: enumerateList rest (idx + 1)

def listGetIdx (l : List α) (idx : Nat) : Option α :=
  match l, idx with
  | [], _ => none
  | a :: _, 0 => some a
  | _ :: rest, n + 1 => listGetIdx rest n

def tyToString : Ty → String
  | .int => "i64"
  | .uint => "u64"
  | .i8 => "i8"
  | .i16 => "i16"
  | .i32 => "i32"
  | .u8 => "u8"
  | .u16 => "u16"
  | .u32 => "u32"
  | .bool => "bool"
  | .float64 => "f64"
  | .float32 => "f32"
  | .char => "char"
  | .unit => "()"
  | .string => "String"
  | .named n => n
  | .ref inner => "&" ++ tyToString inner
  | .refMut inner => "&mut " ++ tyToString inner
  | .generic name args caps => name ++ "<" ++ ", ".intercalate (args.map tyToString ++ caps.map CapSet.toTypeArg) ++ ">"
  | .typeVar name => name
  | .array elem size => "[" ++ tyToString elem ++ "; " ++ toString size ++ "]"
  | .ptrMut inner => "*mut " ++ tyToString inner
  | .ptrConst inner => "*const " ++ tyToString inner
  | .fn_ params capSet retTy =>
    let paramStr := ", ".intercalate (params.map tyToString)
    let capStr := match capSet with
      | .empty => ""
      | .concrete caps => " with(" ++ ", ".intercalate caps ++ ")"
      | .var name => " with(" ++ name ++ ")"
      | _ => " with(...)"
    "fn(" ++ paramStr ++ ")" ++ capStr ++ " -> " ++ tyToString retTy
  | .never => "!"
  | .heap inner => "Heap<" ++ tyToString inner ++ ">"
  | .heapArray inner => "HeapArray<" ++ tyToString inner ++ ">"
  | .placeholder => "<unknown>"

/-- Must-use types whose silent discard is flagged (Phase 6 #13). The canonical
    fallible results — `Result<…>` and `Option<…>`, however spelled (`.named` when
    written without type arguments, `.generic` with them). A discarded statement
    expression of one of these types ignores a possible failure/absence, so it is
    an error unless explicitly acknowledged. -/
def mustUseEnumName? : Ty → Option String
  | .named n => if n == resultEnumName || n == optionEnumName then some n else none
  | .generic n _ _ => if n == resultEnumName || n == optionEnumName then some n else none
  | _ => none

/-- Is this a float type? -/
def isFloatType : Ty → Bool
  | .float32 | .float64 => true
  | _ => false

/-- Is this a pointer type? -/
def isPointerType : Ty → Bool
  | .ptrMut _ | .ptrConst _ => true
  | _ => false

/-- Is this a reference type? -/
def isReferenceType : Ty → Bool
  | .ref _ | .refMut _ => true
  | _ => false

/-- Bug 092: does a value of this type HOLD a reference — the type is a
    reference, or an aggregate (array, `Heap`, generic argument) that holds one?
    Function types do not: a fn pointer taking `&T` stores no reference. Raw
    pointers are the trusted escape and are not looked through. Named types are
    not looked through either: a struct/enum field may not hold a reference
    (rejected at the declaration), so a named type can only hold one through
    its generic arguments. -/
-- spec: REF-D2 (docs/spec/references.md; this declaration enforces the rule)
partial def tyStoresRef : Ty → Bool
  | .ref _ | .refMut _ => true
  | .heap inner | .heapArray inner => tyStoresRef inner
  | .array elem _ => tyStoresRef elem
  | .generic _ args _ => args.any tyStoresRef
  | _ => false

/-- Bug 092: a binding of this type would keep a reference stored INSIDE an
    aggregate (looking through a bare outer reference: `&[&T; 2]` too). A bare
    `&T` binding is not an aggregate — its source is frozen by `borrowedFrom`. -/
-- spec: REF-D2 (docs/spec/references.md; this declaration enforces the rule)
partial def tyHoldsRefInAggregate : Ty → Bool
  | .ref inner | .refMut inner => tyHoldsRefInAggregate inner
  | .heap inner | .heapArray inner => tyStoresRef inner
  | .array elem _ => tyStoresRef elem
  | .generic _ args _ => args.any tyStoresRef
  | _ => false

/-- Could `ty` hold a reference once the type parameters `tps` are instantiated (one of them to a
    reference)? A bare type parameter can become one; so can an aggregate or generic argument
    holding one (REF-3). A raw pointer or a function type stays what it is under substitution, so a
    type parameter behind one does not count (exit 2i review). -/
partial def tyMayHoldRefAt (tps : List String) : Ty → Bool
  | .typeVar n => tps.contains n
  | .named n => tps.contains n
  | .ref _ | .refMut _ => true
  | .ptrMut _ | .ptrConst _ | .fn_ _ _ _ => false
  | .heap inner | .heapArray inner => tyMayHoldRefAt tps inner
  | .array inner _ => tyMayHoldRefAt tps inner
  | .generic _ args _ => args.any (tyMayHoldRefAt tps)
  | _ => false

/-- Does the named type parameter occur anywhere in this type? -/
partial def tyParamOccursIn (name : String) : Ty → Bool
  | .typeVar n => n == name
  | .named n => n == name
  | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner | .heap inner | .heapArray inner =>
    tyParamOccursIn name inner
  | .array elem _ => tyParamOccursIn name elem
  | .generic _ args _ => args.any (tyParamOccursIn name)
  | .fn_ params _ retTy => params.any (tyParamOccursIn name) || tyParamOccursIn name retTy
  | _ => false

def getEnv : CheckM TypeEnv := get
def setEnv (env : TypeEnv) : CheckM Unit := set env

/-- Resolve type aliases. -/
-- spec: REF-2 (docs/spec/references.md; this declaration enforces the rule)
def resolveType (ty : Ty) : CheckM Ty := do
  match ty with
  | .named name =>
    let env ← getEnv
    -- Resolve Self to the current impl type
    if name == selfTypeName then
      match env.currentImplType with
      | some t => return t
      | none => throwCheck .selfOutsideImpl
    -- Check if it's a type parameter first
    else if env.currentTypeParams.contains name then return .typeVar name
    else
      match env.typeAliases.lookup name with
      -- The alias map is transitively pre-closed at build time (`closeAliasMap`),
      -- so this single lookup already yields the fully-expanded target
      -- (`type B = A; type A = i32` => B resolves straight to i32).
      | some resolved =>
        -- An alias is checked after expansion like the type it names (G16): `type Cb = fn(&Int)
        -- -> &Int;` is the same ill-formed function type as the spelled-out one.
        if tyHasRefReturningFnType resolved then
          throwCheck (.returnsReference s!"type alias '{name}' names a function type that returns a reference ('{tyToString resolved}'); a function type may not return a reference (REF-2, VALUE_MODEL.md)" "REF-2@1")
        return resolved
      | none => return ty
  | .ref inner =>
    let inner' ← resolveType inner
    return .ref inner'
  | .refMut inner =>
    let inner' ← resolveType inner
    return .refMut inner'
  | .ptrMut inner =>
    let inner' ← resolveType inner
    return .ptrMut inner'
  | .ptrConst inner =>
    let inner' ← resolveType inner
    return .ptrConst inner'
  | .array elem n =>
    let elem' ← resolveType elem
    return .array elem' n
  | .generic "Heap" [inner] _ =>
    let inner' ← resolveType inner
    return .heap inner'
  | .generic "HeapArray" [inner] _ =>
    let inner' ← resolveType inner
    return .heapArray inner'
  | .generic name args caps =>
    let args' ← args.mapM resolveType
    return .generic name args' caps
  | .fn_ params capSet retTy =>
    let params' ← params.mapM resolveType
    let retTy' ← resolveType retTy
    -- References are second-class: a function TYPE may not return a reference,
    -- directly or nested (docs/language/VALUE_MODEL.md). This makes ref-returning
    -- callbacks unconstructable, which keeps scoped callbacks (`with_value`)
    -- sound — the callback cannot return the borrowed element it was handed.
    if tyContainsRef retTy' then
      throwCheck (.returnsReference s!"function type may not return a reference ('{tyToString retTy'}'); references are second-class and may not be returned (see VALUE_MODEL.md). Use a value, an owned view, or a scoped callback; for low-level access return a raw pointer (*const/*mut)." "REF-2@1")
    return .fn_ params' capSet retTy'
  | _ => return ty

/-- `name` occurs in `ty` behind a reference or pointer — a position the generic body can load a
    `name` value out of (`r: &T`, `w: &W<T>`). -/
partial def tyParamOccursNested (name : String) : Ty → Bool
  -- behind a reference or pointer: the body can load a `name` out of the pointee
  | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner => tyParamOccursIn name inner
  -- inside a storage type, behind such a reference (`W<&T>`); a `name` stored directly in an
  -- aggregate, array or heap cell is batch 4's E0239 domain (refused at the declaration and,
  -- after monomorphization, by `verifyNoStoredRefs`), and a function type is not storage
  | .generic _ args _ => args.any (tyParamOccursNested name)
  | .array elem _ => tyParamOccursNested name elem
  | .heap inner | .heapArray inner => tyParamOccursNested name inner
  -- a callback parameter or result: `fn(&mut T)` lets the callee write a `name` into the
  -- body's storage, so the reference/pointer rule applies inside the function type too
  | .fn_ params _ ret => params.any (tyParamOccursNested name) || tyParamOccursNested name ret
  | _ => false

/-- Bug 091 round 3 (normalized place rule): the place rule checks a generic body ONCE, at its
    type parameters, where `T` is opaque. A `T` instantiated to a REFERENCE in a parameter
    position the body can read a `T` out of (`w: &W<T>`, `h: Heap<T>`, `a: [T; N]`) would let
    the body read a stored reference the rule never saw. Refused at the instantiation; a
    top-level `x: T` (the caller's own reference, checked at the call) stays allowed. -/
-- spec: REF-4 (docs/spec/references.md; this declaration enforces the rule)
def refuseNestedRefInstantiation (what : String) (mapping : List (String × Ty))
    (paramTys : List Ty) (retTy : Ty := .unit) : CheckM Unit := do
  for (pName, pTy0) in mapping do
    -- aliases resolved: `f::<R>` with `type R = &P` is a reference instantiation
    let pTy ← tryCatch (resolveType pTy0) (fun _ => pure pTy0)
    -- inside erased code the instantiation must be closed and reference-free: an opaque
    -- type parameter forwarded here could be a reference at the caller (review round 8)
    let envE ← getEnv
    if envE.erasedDepth > 0 && (tyContainsRef pTy || tyMayHoldRefAt envE.currentTypeParams pTy) then
      throwCheck (.erasedRefInstantiation what pName (tyToString pTy))
    if tyContainsRef pTy && tyParamOccursIn pName retTy then
      throwCheck (.returnsReference s!"{what}: type parameter '{pName}' is instantiated to a reference ('{tyToString pTy}') and occurs in the return type; references may not be returned (VALUE_MODEL.md). Return a value, an owned view, or use a scoped callback." "REF-4@1")
    if tyContainsRef pTy && paramTys.any (tyParamOccursNested pName) then
      throwCheck (.untraceablePlace s!"what the reference instantiating '{pName}' of {what} borrows"
        s!"'{pName}' := '{tyToString pTy}' occurs inside a parameter type, so the generic body could read a stored reference the place rule never saw; pass the reference directly or a value") none (rule := some "REF-4@1")

/-- Type-variable substitution over surface types. Delegates to the one shared
    definition (`Layout.substTyVars`); the two were byte-for-byte equivalent
    (the old local copy applied `substCapSet` on `.fn_`, but that was identity
    for every cap form), so this keeps a single substitution implementation. -/
def substTy (mapping : List (String × Ty)) (ty : Ty) : Ty :=
  Layout.substTyVars mapping ty

/-- Is this type Copy (non-linear)? Primitives are Copy; structs are linear.

    This is the *surface* Copy judgment: it runs during checking over surface
    `StructDef`/`EnumDef` (env-based, monadic) and handles in-progress inference
    forms the Core judgment never sees — `newtype` unwrapping, `.typeVar` with an
    env Copy-bound, `.placeholder`. The *Core* counterpart is
    `Layout.isCopyTyCore` (pure, over `CStructDef`/`CEnumDef`), shared by
    CoreCheck/Mono/Verify. The two are kept separate only because they operate on
    different representations at different phases; the primitive- and
    conditional-generic policy is intentionally identical, so a change to what
    "Copy" means must be mirrored in both (tracked toward Phase 14 #13b, one
    source of typing truth). -/
-- The Copy judgment is single-sourced in `Layout.isCopyTyGeneric` (Phase 6.5
-- CopyJudgment axis). This is the front-end entry point: a thin monadic wrapper
-- that feeds the shared judgment its lookups from the checker env — struct/enum
-- defs, newtype inner types (recursed), and a bounds-based `typeVar` policy
-- (`T: Copy`). Core stages (Mono/Verify/CoreCheck) call `Layout.isCopyTyCore`,
-- the same judgment with Core defs. There is now ONE Copy recursion, so the two
-- cannot drift (they previously disagreed on typeVar/newtype).
-- spec: OWN-D1, spec: TYPE-13 (docs/spec/ownership.md; this declaration enforces the rule)
def isCopyType (ty : Ty) : CheckM Bool := do
  let env ← getEnv
  -- A type alias names its target (`type Num = i64; newtype N = Num;` is Copy): resolve the
  -- head alias of a newtype payload and of the type itself (a few levels; aliases are acyclic).
  let deAlias : Ty → Ty := fun t =>
    let step : Ty → Ty := fun t => match t with
      | .named a => (env.typeAliases.lookup a).getD t
      | t => t
    step (step (step (step t)))
  let ty := deAlias ty
  return Layout.isCopyTyGeneric
    (fun name =>
      match env.structs.find? fun sd => sd.name == name with
      | some sd => some (sd.isCopy, sd.typeParams, sd.fields.map (fun f => f.ty))
      | none => match env.enums.find? fun ed => ed.name == name with
        | some ed => some (ed.isCopy, ed.typeParams, ed.variants.flatMap (fun v => v.fields.map (fun f => f.ty)))
        | none => none)
    (fun name => (env.newtypes.find? fun nt => nt.name == name).map (fun nt => deAlias nt.innerTy))
    (fun name => (((env.currentTypeBounds.find? fun b => b.1 == name).map Prod.snd).getD []).contains "Copy")
    ty

/-! ### Local bindings (R-0492)

The checker never decides by spelling which binding a name denotes. A USE of a local name at a
source position is resolved through the function's local table (`localAt?`), which the lexical
walk in `Resolve/Locals.lean` filled; a BINDER takes its identity from the same table
(`binderRef`). The environment is keyed by that identity, so a binding left in it after its
scope closed cannot answer for a later use of the same spelling. -/

/-- The binding the local name `name` at `sp` denotes, or `none` when `name` there is not a local
    (a function, a constant). -/
def localAt? (sp : Span) (name : String) : CheckM (Option LocalRef) := do
  return ((← getEnv).locals.useAt? sp name).map fun id => { id, name }

/-- The binding introduced at `key` (a parameter index, or a binder position and spelling).
    Every binder the checker registers was minted by the local walk; one it did not mint is an
    internal error, not a fresh binding. -/
def binderRef (key : BinderKey) (name : String) (span : Option Span := none) : CheckM LocalRef := do
  match (← getEnv).locals.binderAt? key with
  | some id => return { id, name }
  | none => throwCheck (.binderTableMissing s!"the binder '{name}'" "has no identity in the local table") span

/-- The binding introduced by the construct at `sp` spelled `name`. -/
def binderAt (sp : Span) (name : String) : CheckM LocalRef :=
  binderRef (.site (SiteKey.ofSpan sp) name) name (some sp)

def lookupLocal (r : LocalRef) : CheckM (Option VarInfo) := do
  return (← getEnv).vars.lookup r

/-- The binding a use of `name` at `sp` denotes, with its current state. -/
def lookupUse (sp : Span) (name : String) : CheckM (Option (LocalRef × VarInfo)) := do
  match ← localAt? sp name with
  | some r => return (← lookupLocal r).map (r, ·)
  | none => return none

def addVar (r : LocalRef) (ty : Ty) (mutable : Bool := true) (declSpan : Option Span := none) : CheckM Unit := do
  -- Bug 092: no binding (let, parameter, match binder) may hold a reference
  -- inside an aggregate — nothing would keep the reference's source frozen.
  if tyHoldsRefInAggregate ty then
    throwCheck (.refStoredInAggregate s!"binding '{r.name}'" (tyToString ty)) declSpan
  let copy ← isCopyType ty
  let env ← getEnv
  let info : VarInfo := { ty, state := .unconsumed, isCopy := copy, loopDepth := env.loopDepth, mutable, declSpan,
                          uid := r.id }
  setEnv { env with vars := (r, info) :: env.vars.filter (fun (k, _) => k != r) }

/- H16 (E0292, `.shadowsLiveLinear`) used to search the environment for a live binding of the
   new binder's spelling. SCOPE-1@1 (E0301), computed by the local walk, refuses every binder that
   reuses a visible spelling before the body is checked, and a spelling that is not visible names
   a different binding (R-0492), so that search could only find a binding whose scope had closed:
   a false refusal of the bug-176 kind. The constructor stays as a definition only. -/

/-- The entry for binding `uid` (bugs 093/094: identity, not name). -/
def lookupByUid (vars : List (LocalRef × VarInfo)) (uid : LocalId) : Option VarInfo :=
  (vars.find? fun (_, vi) => vi.uid == uid).map Prod.snd

/-- Is binding `uid` present in `vars`? -/
def hasUid (vars : List (LocalRef × VarInfo)) (uid : LocalId) : Bool :=
  vars.any fun (_, vi) => vi.uid == uid

def activeBorrowRefs (env : TypeEnv) (var : LocalRef) : List VarInfo :=
  env.vars.foldl (fun acc (_, info) =>
    match info.borrowedFrom with
    | some source =>
      if source == var && info.state != .consumed then info :: acc else acc
    | none => acc) []

partial def tyContainsTypeVar : Ty → Bool
  | .typeVar _ => true
  | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner | .heap inner | .heapArray inner =>
    tyContainsTypeVar inner
  | .array elem _ => tyContainsTypeVar elem
  | .generic _ args _ => args.any tyContainsTypeVar
  | .fn_ params _ retTy => params.any tyContainsTypeVar || tyContainsTypeVar retTy
  | _ => false

def lookupStruct (name : String) : CheckM (Option StructDef) := do
  let env ← getEnv
  return env.structs.find? fun sd => sd.name == name

def lookupStructField (structName : String) (fieldName : String) : CheckM (Option Ty) := do
  match ← lookupStruct structName with
  | some sd =>
    match sd.fields.find? fun f => f.name == fieldName with
    | some f => return some f.ty
    | none => return none
  | none => return none

def lookupEnum (name : String) : CheckM (Option EnumDef) := do
  let env ← getEnv
  return env.enums.find? fun ed => ed.name == name

def lookupEnumVariant (enumName : String) (variantName : String) : CheckM (Option EnumVariant) := do
  match ← lookupEnum enumName with
  | some ed => return ed.variants.find? fun v => v.name == variantName
  | none => return none

def lookupNewtype (name : String) : CheckM (Option NewtypeDef) := do
  let env ← getEnv
  return env.newtypes.find? fun nt => nt.name == name

def lookupFn (name : String) : CheckM (Option FnSummary) := do
  let env ← getEnv
  match env.fnNames.lookup name with
  | some idx => return listGetIdx env.functions idx
  | none => return none

/-- Bug 253: is the newtype a spelling resolved to defined by the module being checked? By its
    identity; a declaration with none (parsed, not summarized) is one of the module's own. -/
def TypeEnv.newtypeIsLocal (env : TypeEnv) (nt : NewtypeDef) : Bool :=
  match nt.typeId? with
  | some id => env.localNewtypeIds.contains id
  | none => env.localNewtypeNames.contains nt.name

/-- Bug 253: is the struct a spelling resolved to defined by the module being checked? -/
def TypeEnv.structIsLocal (env : TypeEnv) (sd : StructDef) : Bool :=
  match sd.typeId? with
  | some id => env.localStructIds.contains id
  | none => env.localStructNames.contains sd.name

/-- A method by its definition name `Type_method`, in the method table only (bugs 238 – 239):
    a receiver call `x.m()` and a `T::m()` call mean the type's method, never a free function
    of the module that happens to be spelled `T_m`. -/
def lookupMethodByName (name : String) : CheckM (Option FnSummary) := do
  let env ← getEnv
  match env.methodNames.lookup name with
  | some idx => return listGetIdx env.functions idx
  | none => return none
/-- R-0492 step 3: the declaration a type spelling denotes in the module being checked
    (`typeIdOfSpelling` over its environment); a type parameter of the function denotes none. -/
def TypeEnv.typeIdOf (env : TypeEnv) (name : String) : Option TypeId :=
  if env.currentTypeParams.contains name then none
  else typeIdOfSpelling env.structs env.enums env.newtypes name

/-- R-0492 step 3: type equality in the module being checked, by declaration identity
    (`Ty.sameByIdentity`): an import alias and the definition's spelling are one type (bug 223). -/
def sameTy (a b : Ty) : CheckM Bool := do
  if a == b then return true
  return Ty.sameByIdentity (← getEnv).typeIdOf a b

/-- R-0492 step 3: the type spelled `typeName` here has an impl of `traitName`: by the target's
    declaration identity (`traitImplIds`), or, for a target with none, by spelling. -/
def TypeEnv.hasTraitImpl (env : TypeEnv) (typeName traitName : String) : Bool :=
  env.traitImpls.any (fun (t, tr) => t == typeName && tr == traitName)
    || (match env.typeIdOf typeName with
        | some tid => env.traitImplIds.any fun (t, tr) => t == tid && tr == traitName
        | none => false)

/-- R-0492 step 3: the method `methodName` of the type spelled `typeName`, as its mangled name and
    signature. A type with a declaration identity finds it by that identity
    (`methodByIdentity`), so an import alias `Q` of `P` finds `P_mk` (bug 223); a method no
    stamped impl declares (an impl on a builtin or an imported type) is found by its spelling. -/
def lookupMethod (typeName methodName : String) (span : Option Span := none) :
    CheckM (Option (String × FnSummary)) := do
  let env ← getEnv
  if let some owner := env.typeIdOf typeName then
    if let some (mangled, sig) := methodByIdentity env.allFnSummarys { owner, method := methodName } then
      -- The call is a METHOD call in Core (`CallSpace.method`), resolved below among methods
      -- only, so a free function of the mangled spelling (`P_mk` beside `P::mk`) cannot answer
      -- it and needs no refusal (bugs 238 – 239).
      let _ := span
      return some (mangled, sig)
  -- A method no stamped impl declares: by its spelling, in the method table alone.
  let mangled := mangledMethodName typeName methodName
  return (← lookupMethodByName mangled).map (mangled, ·)

/-- Normalize a type for comparison (normalize empty capsets in fn types). -/
def normalizeTyForCmp : Ty → Ty
  | .fn_ params capSet retTy =>
    -- Canonicalize the capability set so equal sets compare equal regardless of
    -- order (e.g. a `with(Std)`-expanded callback vs an inferred capset that
    -- holds the same caps in a different order). CapSet.normalize sorts the
    -- concrete caps; rebuild a canonical CapSet from the sorted caps + vars.
    let (cs, vars) := capSet.normalize
    let svars := vars.mergeSort (· < ·)
    let base := if cs.isEmpty then CapSet.empty else CapSet.concrete cs
    let normCap := svars.foldl
      (fun acc v => match acc with | .empty => CapSet.var v | other => CapSet.union other (CapSet.var v)) base
    .fn_ (params.map normalizeTyForCmp) normCap (normalizeTyForCmp retTy)
  | .ref t => .ref (normalizeTyForCmp t)
  | .refMut t => .refMut (normalizeTyForCmp t)
  | .heap t => .heap (normalizeTyForCmp t)
  | .heapArray t => .heapArray (normalizeTyForCmp t)
  | .generic n args caps => .generic n (args.map normalizeTyForCmp) caps
  | .array t n => .array (normalizeTyForCmp t) n
  | t => t

-- spec: TYPE-4 (docs/spec/types.md; this declaration enforces the rule)
def expectTy (expected actual : Ty) (ctx : String) (span : Option Span := none) : CheckM Unit := do
  if expected == actual then return ()
  -- Never type is compatible with anything (bottom type)
  if actual == .never then return ()
  -- Resolve type aliases and try again
  let expectedR ← resolveType expected
  let actualR ← resolveType actual
  if expectedR == actualR then return ()
  -- Normalize fn types (empty capsets) and try again
  let expectedN := normalizeTyForCmp expectedR
  let actualN := normalizeTyForCmp actualR
  if ← sameTy expectedN actualN then return ()
  -- .string is compatible with .named "String"
  else if (expectedR == .string && actualR == .named "String")
       || (expectedR == .named "String" && actualR == .string) then return ()
  else throwCheck (.typeMismatch ctx (tyToString expected) (tyToString actual)) span


-- ============================================================
-- Linearity: consume and check
-- ============================================================

/-- Mark a linear variable as used (read/borrowed but not moved). -/
def useVar (r : LocalRef) (span : Option Span := none) : CheckM Unit := do
  let name := r.name
  let env ← getEnv
  match env.vars.lookup r with
  | none => pure ()  -- not found (might be a constant or function)
  | some info =>
    if info.state == .frozen then
      throwCheck (.variableFrozenByBorrow name) span
    if info.isCopy then return ()
    if info.state == .unconsumed || info.state == .reserved then
      -- Only the innermost binding (`info`), by identity (bug 094).
      let vars' := env.vars.map fun (n, vi) =>
        if vi.uid == info.uid then
          (n, { vi with
            state := if info.state == .reserved then .reserved else .used,
            movedAt := if vi.movedAt.isSome then vi.movedAt else span })
        else (n, vi)
      setEnv { env with vars := vars' }

/-- Consume a linear variable (mark it as consumed).
    Errors on use-after-move, or consuming an outer var inside a loop.
    `breakDepthExempt`: consuming via `break value;` exits the loop, so a value
    declared IMMEDIATELY outside the broken loop is consumed at most once per
    entry to that loop — pass 1 to relax the loop-depth rule by one level
    (H14). Deeper outer values stay rejected: an enclosing loop could re-enter
    the broken loop and re-consume. -/
-- spec: REF-8, spec: OWN-1, spec: OWN-4, spec: OWN-12 (docs/spec/references.md; this declaration enforces the rule)
def consumeVar (r : LocalRef) (span : Option Span := none) (breakDepthExempt : Nat := 0) : CheckM Unit := do
  let name := r.name
  let env ← getEnv
  match env.vars.lookup r with
  | none => throwCheck (.undeclaredVariable name) span
  | some info =>
    if info.isCopy then return ()  -- Copy types are never consumed
    -- Bug 090: a match guard may not move a binding that was live at its start.
    if env.guardUids.contains info.uid then
      throwCheck (.guardMovesLinear name) span
    let activeRefs := activeBorrowRefs env r
    if activeRefs.any (fun refInfo => match refInfo.ty with | .ref _ | .refMut _ => true | _ => false) then
      throwCheck (.cannotMoveLinearBorrowed name) span
    -- Defer rule (a) (bug 143): a pending deferred call borrows this binding.
    if info.pinnedByDefer then
      throwCheck (.variablePinnedByDefer name) span
    match info.state with
    | .consumed =>
      -- point the secondary span at where the value was moved (Phase 4 #11).
      throwCheck (.variableUsedAfterMove name) span
        (related := info.movedAt.toList.map (fun sp => (sp, s!"'{name}' moved here")))
    | .reserved =>
      throwCheck (.variableReservedByDefer name) span
    | .frozen =>
      throwCheck (.variableFrozenByBorrow name) span
    | .unconsumed | .used =>
      -- Loop depth check: linear values from outer scope cannot be consumed
      -- inside a loop — except inside a branch that exits the function
      -- (`while … { if bad { s.drop(); return 1; } }`): the return-path rule
      -- then guarantees everything live is consumed, and no iteration follows.
      if info.loopDepth + breakDepthExempt < env.loopDepth && !env.inFnExitingBranch
          && env.rebindingVar != some r then
        throwCheck (.cannotConsumeLinearInLoop name) span
      -- Mark consumed
      -- Mark ONLY the binding the name resolves to (the innermost), by
      -- identity. Marking every same-named entry consumed a shadowed outer
      -- binding too (bug 094): an arm binder `value` consumed the outer `value`.
      let vars' := env.vars.map fun (n, vi) =>
        if vi.uid == info.uid then (n, { vi with state := .consumed, movedAt := span })
        else (n, vi)
      setEnv { env with vars := vars' }

/-- Consume a variable if it exists. Skips function names (not in var scope). -/
def consumeVarIfExists (r : LocalRef) (span : Option Span := none) (breakDepthExempt : Nat := 0) : CheckM Unit := do
  match ← lookupLocal r with
  | some _ => consumeVar r span breakDepthExempt
  | none => pure ()

/-- Consume the binding the use of `name` at `sp` denotes, if it is a local (not a function). -/
def consumeUseIfLocal (sp : Span) (name : String) (span : Option Span := none) (breakDepthExempt : Nat := 0) : CheckM Unit := do
  match ← localAt? sp name with
  | some r => consumeVarIfExists r span breakDepthExempt
  | none => pure ()

/-- Check that every binding in `entries` is consumed at scope exit.
    `reserved` is allowed because the deferred destroy will run at scope exit.
    Each binding is looked up by IDENTITY (bug 093): a shadowed entry is
    checked as itself, not through the innermost binding sharing its name. -/
-- spec: OWN-2 (docs/spec/ownership.md; this declaration enforces the rule)
def checkScopeExit (entries : List (LocalRef × VarInfo)) (span : Option Span := none) : CheckM Unit := do
  let env ← getEnv
  for (r, entry) in entries do
    let name := r.name
    match lookupByUid env.vars entry.uid with
    | some info =>
      if !info.isCopy && info.state != .consumed && info.state != .reserved then
        -- Point at the variable's declaration when we have it (E0208 used to be
        -- spanless); fall back to the scope-exit span.
        throwCheck (.linearVariableNeverConsumed name) (info.declSpan.orElse (fun _ => span))
    | none => pure ()

-- ============================================================
-- Block divergence (KNOWN_HOLES H9 support)
-- ============================================================

/-- A literal `true` (through parens), for spotting a non-terminating `while true`. -/
partial def isLitTrueExpr : Expr → Bool
  | .boolLit _ b => b
  | .paren _ inner => isLitTrueExpr inner
  | _ => false

/-- Where the loop-exit search stands (bug 176). `own` is the loop's label; `inner`
    the labels of loops nested inside it at this point; `direct` is false inside a
    nested loop, where an unlabelled `break` targets that loop. `anyExit` picks the
    question: true asks "can control leave this loop at all" (also to an enclosing
    loop, through a labelled `break`/`continue`), false asks "can control reach the
    statement after this loop" (only a `break` that targets it). -/
structure LoopExitCtx where
  own : Option String
  inner : List String := []
  direct : Bool := true
  anyExit : Bool

-- Does an exit of the kind `ctx.anyExit` asks for appear in a loop's body?
-- Searches through `if` and value-`if` branches, `match` arms, borrow blocks,
-- `let`-`else` and statement expressions (bug 176, Codex review: a `while let`,
-- whose `break` sits in the desugared match's fallback arm, read as infinite, and
-- a labelled `break` out of a nested loop was ignored, so a loop body that exits
-- was taken as one that never ends and its locals were exempted from H9). Labels
-- are resolved: an exit to a nested loop's label stays inside, a `continue` to
-- the loop's own label stays in it.
mutual
partial def loopBlockExits (ctx : LoopExitCtx) (stmts : List Stmt) : Bool :=
  stmts.any (loopStmtExits ctx)
partial def loopStmtExits (ctx : LoopExitCtx) : Stmt → Bool
  | .break_ _ v lbl =>
    let leaves := match lbl with
      | none => ctx.direct
      | some l => !ctx.inner.contains l && (ctx.anyExit || ctx.own == some l)
    leaves || (match v with | some e => loopExprExits ctx e | none => false)
  | .continue_ _ lbl =>
    match lbl with
    | none => false
    | some l => ctx.anyExit && !ctx.inner.contains l && ctx.own != some l
  | .ifElse _ c t e =>
    loopExprExits ctx c || loopBlockExits ctx t
      || (match e with | some es => loopBlockExits ctx es | none => false)
  | .while_ _ c body lbl =>
    loopExprExits ctx c
      || loopBlockExits { ctx with inner := lbl.toList ++ ctx.inner, direct := false } body
  | .forLoop _ init c step body lbl =>
    let ctx' : LoopExitCtx := { ctx with inner := lbl.toList ++ ctx.inner, direct := false }
    (match init with | some s => loopStmtExits ctx s | none => false) || loopExprExits ctx c
      || (match step with | some s => loopStmtExits ctx' s | none => false)
      || loopBlockExits ctx' body
  | .expr _ e _ => loopExprExits ctx e
  | .letDecl _ _ _ _ v _ => loopExprExits ctx v
  | .assign _ _ v => loopExprExits ctx v
  | .return_ _ (some v) => loopExprExits ctx v
  | .borrowIn _ _ _ _ _ body => loopBlockExits ctx body
  | .letDestructure _ _ _ _ v eb =>
    loopExprExits ctx v || (match eb with | some b => loopBlockExits ctx b | none => false)
  | _ => false
partial def loopExprExits (ctx : LoopExitCtx) : Expr → Bool
  | .paren _ e => loopExprExits ctx e
  | .match_ _ s arms => loopExprExits ctx s || arms.any (loopArmExits ctx)
  | .ifExpr _ c t e => loopExprExits ctx c || loopBlockExits ctx t || loopBlockExits ctx e
  | _ => false
partial def loopArmExits (ctx : LoopExitCtx) : MatchArm → Bool
  | .mk _ _ _ _ _ body => loopBlockExits ctx body
  | .litArm _ _ _ body => loopBlockExits ctx body
  | .varArm _ _ _ body => loopBlockExits ctx body
  | .rangeArm _ _ _ _ _ body => loopBlockExits ctx body
end

/-- Can control leave the loop whose body is `stmts` and whose label is `own`?
    With `anyExit := false` (divergence: does the loop fall through to the next
    statement) only a `break` targeting this loop counts; with `anyExit := true`
    (H9: does the body's scope ever close) a labelled `break`/`continue` to an
    enclosing loop counts too. -/
def blockHasBreak (stmts : List Stmt) (own : Option String := none) (anyExit : Bool := false) : Bool :=
  loopBlockExits { own, anyExit } stmts

-- Control cannot fall off the end of this block — it returns, breaks, continues,
-- `abort()`s, or loops forever (`while true {}` with no break). When true, a
-- block-local linear value left unconsumed at the block's textual end is exempt:
-- that end is unreachable, so it is not a leak (KNOWN_HOLES H9). This is the
-- conservative direction — we exempt on any non-fall-through exit, so a leak on a
-- `return`/`break` path is *missed* rather than falsely flagged.
mutual
partial def blockDiverges (stmts : List Stmt) : Bool :=
  match stmts.getLast? with
  | none => false
  | some s => stmtDiverges s
partial def stmtDiverges : Stmt → Bool
  | .return_ _ _ => true
  | .break_ _ _ _ => true
  | .continue_ _ _ => true
  | .while_ _ cond body lbl => isLitTrueExpr cond && !blockHasBreak body lbl
  | .forLoop _ _ cond step body lbl =>
    isLitTrueExpr cond && !blockHasBreak (body ++ step.toList) lbl
  | .ifElse _ _ t (some e) => blockDiverges t && blockDiverges e
  | .expr _ e _ => exprDiverges e
  | _ => false
partial def exprDiverges : Expr → Bool
  | .paren _ inner => exprDiverges inner
  | .call _ fnName _ _ => resolveIntrinsic fnName == some .abort
  | .match_ _ _ arms => !arms.isEmpty && arms.all armDiverges
  | _ => false
partial def armDiverges : MatchArm → Bool
  | .mk _ _ _ _ _ body => blockDiverges body
  | .litArm _ _ _ body => blockDiverges body
  | .varArm _ _ _ body => blockDiverges body
  | .rangeArm _ _ _ _ _ body => blockDiverges body
end

-- Control NEVER leaves this block alive — it loops forever (`while true {}` with no
-- break) or `abort()`s. Unlike `blockDiverges`, this EXCLUDES `return`/`break`/
-- `continue`: those exit the scope, so a linear value still owned at that point
-- genuinely leaks and must be consumed. This is the predicate that exempts a block
-- from scope-exit: only a truly-unreachable end (the body of a server's accept loop,
-- a `panic`) may leave a resource live. Fixes the `let l = bind(); while true {…}`
-- false-positive without masking `let r = make(); return 0;` leaks (KNOWN_HOLES H9).
mutual
partial def blockNonTerminating (stmts : List Stmt) : Bool :=
  match stmts.getLast? with
  | none => false
  | some s => stmtNonTerminating s
partial def stmtNonTerminating : Stmt → Bool
  | .while_ _ cond body lbl => isLitTrueExpr cond && !blockHasBreak body lbl (anyExit := true)
  | .forLoop _ _ cond step body lbl =>
    isLitTrueExpr cond && !blockHasBreak (body ++ step.toList) lbl (anyExit := true)
  | .ifElse _ _ t (some e) => blockNonTerminating t && blockNonTerminating e
  | .expr _ e _ => exprNonTerminating e
  | _ => false
partial def exprNonTerminating : Expr → Bool
  | .paren _ inner => exprNonTerminating inner
  | .call _ fnName _ _ => resolveIntrinsic fnName == some .abort
  | .match_ _ _ arms => !arms.isEmpty && arms.all armNonTerminating
  | _ => false
partial def armNonTerminating : MatchArm → Bool
  | .mk _ _ _ _ _ body => blockNonTerminating body
  | .litArm _ _ _ body => blockNonTerminating body
  | .varArm _ _ _ body => blockNonTerminating body
  | .rangeArm _ _ _ _ _ body => blockNonTerminating body
end

-- Bugs 194 / 195: the END of a block is unreachable — some statement of it, at any position,
-- never completes: it returns, breaks, continues, `abort()`s, loops forever, or is a construct
-- whose every branch never completes (an `if` with both branches, a `match` with every arm, a
-- borrow block, a `let` whose initializer never completes). Unlike `blockDiverges` (which looks only
-- at the LAST statement, for the linearity exemptions), a diverging statement anywhere makes the
-- rest of the block dead, so `{ return 1; let x: Int = 0; }` cannot reach its end. User functions
-- cannot declare `-> !`, so the only call that never returns is the `abort` intrinsic.
mutual
partial def blockEndUnreachable : List Stmt → Bool
  | [] => false
  | s :: rest => stmtNeverCompletes s || blockEndUnreachable rest
partial def stmtNeverCompletes : Stmt → Bool
  | .return_ _ _ => true
  | .break_ _ _ _ => true
  | .continue_ _ _ => true
  | .while_ _ cond body lbl => isLitTrueExpr cond && !blockHasBreak body lbl
  | .forLoop _ _ cond step body lbl => isLitTrueExpr cond && !blockHasBreak (body ++ step.toList) lbl
  | .ifElse _ c t (some e) => exprNeverCompletes c || (blockEndUnreachable t && blockEndUnreachable e)
  | .ifElse _ c _ none => exprNeverCompletes c
  | .borrowIn _ _ _ _ _ body => blockEndUnreachable body
  | .letDecl _ _ _ _ v _ => exprNeverCompletes v
  | .expr _ e _ => exprNeverCompletes e
  | _ => false
partial def exprNeverCompletes : Expr → Bool
  | .paren _ inner => exprNeverCompletes inner
  | .call _ fnName _ _ => resolveIntrinsic fnName == some .abort
  | .match_ _ scr arms => exprNeverCompletes scr || (!arms.isEmpty && arms.all armNeverCompletes)
  | .ifExpr _ c t e => exprNeverCompletes c || (blockEndUnreachable t && blockEndUnreachable e)
  | _ => false
partial def armNeverCompletes : MatchArm → Bool
  | .mk _ _ _ _ _ body => blockEndUnreachable body
  | .litArm _ _ _ body => blockEndUnreachable body
  | .varArm _ _ _ body => blockEndUnreachable body
  | .rangeArm _ _ _ _ _ body => blockEndUnreachable body
end

-- Rule (d), the operand-escape rule (bug 142; EVALUATION2 D3/D15): a `return`, `break` or
-- `continue` may not occur inside a call argument (receivers included), a struct, enum or
-- array literal element, a functional-update base, or an operator operand (binary, unary,
-- cast, index). Such an exit leaves a construct whose earlier operands were already
-- evaluated and handed to it, so the values moved into it were never destroyed. An exit
-- stays legal in statement position, in a tail, and in a let or assignment initializer's
-- branch chain (`let v = match o { Some { value } => value, None => { return 1; } };`).
-- `operandEscape? depth labels e` is the first exit in `e` that leaves `e`: a `return`, or a
-- `break`/`continue` that is not bound by a loop entered inside `e` (`depth` such loops,
-- `labels` their labels). The rule covers `break`/`continue` even while a `{ break; }`
-- branch types as `()` (D15), so typing them as diverging later cannot reopen the hole.
mutual
partial def operandEscape? (depth : Nat) (labels : List String) : Expr → Option (String × Span)
  | .binOp _ _ l r => firstOperandEscape? depth labels [l, r]
  | .unaryOp _ _ x | .paren _ x | .fieldAccess _ x _ | .borrow _ x | .borrowMut _ x
  | .deref _ x | .cast _ x _ => operandEscape? depth labels x
  | .call _ _ _ args | .staticMethodCall _ _ _ _ args => firstOperandEscape? depth labels args
  | .methodCall _ obj _ _ args => firstOperandEscape? depth labels (obj :: args)
  | .structLit _ _ _ fields base => firstOperandEscape? depth labels (fields.map (·.2) ++ base.toList)
  | .enumLit _ _ _ _ fields => firstOperandEscape? depth labels (fields.map (·.2))
  | .arrayLit _ elems => firstOperandEscape? depth labels elems
  | .arrayIndex _ a i => firstOperandEscape? depth labels [a, i]
  | .allocCall _ inner a => firstOperandEscape? depth labels [inner, a]
  | .ifExpr _ c t e =>
    (operandEscape? depth labels c).orElse fun _ =>
      (stmtsEscape? depth labels t).orElse fun _ => stmtsEscape? depth labels e
  | .match_ _ scr arms =>
    (operandEscape? depth labels scr).orElse fun _ => armsEscape? depth labels arms
  | _ => none
partial def firstOperandEscape? (depth : Nat) (labels : List String) : List Expr → Option (String × Span)
  | [] => none
  | e :: es => (operandEscape? depth labels e).orElse fun _ => firstOperandEscape? depth labels es
partial def armsEscape? (depth : Nat) (labels : List String) : List MatchArm → Option (String × Span)
  | [] => none
  | arm :: arms =>
    let (guard, body) := match arm with
      | .mk _ _ _ _ g b | .litArm _ _ g b | .varArm _ _ g b | .rangeArm _ _ _ _ g b => (g, b)
    ((guard.bind (operandEscape? depth labels)).orElse fun _ => stmtsEscape? depth labels body).orElse
      fun _ => armsEscape? depth labels arms
partial def stmtsEscape? (depth : Nat) (labels : List String) : List Stmt → Option (String × Span)
  | [] => none
  | st :: sts => (stmtEscape? depth labels st).orElse fun _ => stmtsEscape? depth labels sts
partial def stmtEscape? (depth : Nat) (labels : List String) : Stmt → Option (String × Span)
  | .return_ sp _ => some ("return", sp)
  | .break_ sp v lbl =>
    let leaves := match lbl with
      | none => depth == 0
      | some l => !labels.contains l
    if leaves then some ("break", sp) else v.bind (operandEscape? depth labels)
  | .continue_ sp lbl =>
    let leaves := match lbl with
      | none => depth == 0
      | some l => !labels.contains l
    if leaves then some ("continue", sp) else none
  | .while_ _ c body lbl =>
    (operandEscape? depth labels c).orElse fun _ =>
      stmtsEscape? (depth + 1) (lbl.toList ++ labels) body
  | .forLoop _ init c step body lbl =>
    let labels' := lbl.toList ++ labels
    -- The condition is evaluated outside the loop's scope (Check and Elab agree), so a
    -- `break`/`continue` there does not target this loop (Codex review).
    ((init.bind (stmtEscape? depth labels)).orElse fun _ => operandEscape? depth labels c).orElse fun _ =>
      ((step.bind (stmtEscape? (depth + 1) labels')).orElse fun _ => stmtsEscape? (depth + 1) labels' body)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v | .assert_ _ v | .assume_ _ v
  | .letStructDestructure _ _ _ v => operandEscape? depth labels v
  | .ifElse _ c t e =>
    (operandEscape? depth labels c).orElse fun _ =>
      (stmtsEscape? depth labels t).orElse fun _ => e.bind (stmtsEscape? depth labels)
  | .fieldAssign _ o _ v => firstOperandEscape? depth labels [o, v]
  | .derefAssign _ t v => firstOperandEscape? depth labels [t, v]
  | .arrayIndexAssign _ a i v => firstOperandEscape? depth labels [a, i, v]
  | .borrowIn _ _ _ _ _ body => stmtsEscape? depth labels body
  | .letDestructure _ _ _ _ v elseBody =>
    (operandEscape? depth labels v).orElse fun _ => elseBody.bind (stmtsEscape? depth labels)
end

-- Bug 149: does this block contain a `break`/`continue` that can leave a loop enclosing it
-- (an unlabelled one not inside a loop of its own, or a labelled one naming no loop inside it)? Such a
-- block does not only exit the function, so it does not earn the consume-inside-loop
-- exemption even when it ends in `return`.
mutual
partial def exprLeavesLoop (depth : Nat) (labels : List String) : Expr → Bool
  | .binOp _ _ l r => exprLeavesLoop depth labels l || exprLeavesLoop depth labels r
  | .unaryOp _ _ x | .paren _ x | .fieldAccess _ x _ | .borrow _ x | .borrowMut _ x
  | .deref _ x | .cast _ x _ => exprLeavesLoop depth labels x
  | .call _ _ _ args | .staticMethodCall _ _ _ _ args => args.any (exprLeavesLoop depth labels)
  | .methodCall _ obj _ _ args => (obj :: args).any (exprLeavesLoop depth labels)
  | .structLit _ _ _ fields base => (fields.map (·.2) ++ base.toList).any (exprLeavesLoop depth labels)
  | .enumLit _ _ _ _ fields => (fields.map (·.2)).any (exprLeavesLoop depth labels)
  | .arrayLit _ elems => elems.any (exprLeavesLoop depth labels)
  | .arrayIndex _ a i => exprLeavesLoop depth labels a || exprLeavesLoop depth labels i
  | .allocCall _ inner a => exprLeavesLoop depth labels inner || exprLeavesLoop depth labels a
  | .ifExpr _ c t e => exprLeavesLoop depth labels c || stmtsLeaveLoop depth labels t || stmtsLeaveLoop depth labels e
  | .match_ _ scr arms => exprLeavesLoop depth labels scr || arms.any fun arm =>
      match arm with
      | .mk _ _ _ _ g b | .litArm _ _ g b | .varArm _ _ g b | .rangeArm _ _ _ _ g b =>
        (g.map (exprLeavesLoop depth labels)).getD false || stmtsLeaveLoop depth labels b
  | _ => false
partial def stmtsLeaveLoop (depth : Nat) (labels : List String) (stmts : List Stmt) : Bool :=
  stmts.any (stmtLeavesLoop depth labels)
partial def stmtLeavesLoop (depth : Nat) (labels : List String) : Stmt → Bool
  | .break_ _ v lbl =>
    let leaves := match lbl with
      | some l => !labels.contains l
      | none => depth == 0
    leaves || (v.map (exprLeavesLoop depth labels)).getD false
  | .continue_ _ lbl =>
    match lbl with
    | some l => !labels.contains l
    | none => depth == 0
  | .while_ _ c body lbl => exprLeavesLoop depth labels c || stmtsLeaveLoop (depth + 1) (lbl.toList ++ labels) body
  | .forLoop _ init c step body lbl =>
    let labels' := lbl.toList ++ labels
    (init.map (stmtLeavesLoop depth labels)).getD false || exprLeavesLoop depth labels c
      || (step.map (stmtLeavesLoop (depth + 1) labels')).getD false || stmtsLeaveLoop (depth + 1) labels' body
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v | .assert_ _ v | .assume_ _ v
  | .letStructDestructure _ _ _ v => exprLeavesLoop depth labels v
  | .return_ _ v => (v.map (exprLeavesLoop depth labels)).getD false
  | .ifElse _ c t e => exprLeavesLoop depth labels c || stmtsLeaveLoop depth labels t || (e.map (stmtsLeaveLoop depth labels)).getD false
  | .fieldAssign _ o _ v => exprLeavesLoop depth labels o || exprLeavesLoop depth labels v
  | .derefAssign _ t v => exprLeavesLoop depth labels t || exprLeavesLoop depth labels v
  | .arrayIndexAssign _ a i v => exprLeavesLoop depth labels a || exprLeavesLoop depth labels i || exprLeavesLoop depth labels v
  | .borrowIn _ _ _ _ _ body => stmtsLeaveLoop depth labels body
  | .letDestructure _ _ _ _ v elseBody => exprLeavesLoop depth labels v || (elseBody.map (stmtsLeaveLoop depth labels)).getD false
end

/-- The operands of `e` that rule (d) protects, with a description of the construct. An
    expression form not listed has no operand position (a match scrutinee, an `if`
    condition and branch, and a block are statement-like). -/
def protectedOperands : Expr → Option (String × List Expr)
  | .call _ fn _ args => some (s!"an argument of '{fn}'", args)
  | .staticMethodCall _ ty m _ args => some (s!"an argument of '{ty}::{m}'", args)
  | .methodCall _ obj m _ args => some (s!"the receiver or an argument of '.{m}'", obj :: args)
  | .structLit _ n _ fields base => some (s!"a field of the '{n}' literal", fields.map (·.2) ++ base.toList)
  | .enumLit _ en v _ fields => some (s!"a field of the '{en}::{v}' literal", fields.map (·.2))
  | .arrayLit _ elems => some ("an array literal element", elems)
  | .binOp _ _ l r => some ("an operand of a binary operator", [l, r])
  | .unaryOp _ _ x => some ("the operand of a unary operator", [x])
  | .deref _ x => some ("the operand of a dereference", [x])
  | .borrow _ x | .borrowMut _ x => some ("the operand of a borrow", [x])
  | .fieldAccess _ x f => some (s!"the base of the field access '.{f}'", [x])
  | .cast _ x _ => some ("the operand of a cast", [x])
  | .arrayIndex _ a i => some ("an index expression", [a, i])
  | .allocCall _ _ a => some ("an allocator argument", [a])
  | _ => none

-- Control leaves this block by EXITING THE FUNCTION: a `return` on every path
-- (directly, or through an if/else or match whose arms all return). A
-- returning path is a function exit — every linear value still owned there
-- leaks. EXCLUDES `break`/`continue` (the value stays live at the loop
-- boundary) and abort/infinite-loop (process never proceeds; exempt exactly
-- like H9's scope-exit).
mutual
partial def blockExitsFunction (stmts : List Stmt) : Bool :=
  match stmts.getLast? with
  | none => false
  | some s => stmtExitsFunction s
partial def stmtExitsFunction : Stmt → Bool
  | .return_ _ _ => true
  | .ifElse _ _ t (some e) => blockExitsFunction t && blockExitsFunction e
  | .expr _ e _ => exprExitsFunction e
  | _ => false
partial def exprExitsFunction : Expr → Bool
  | .paren _ inner => exprExitsFunction inner
  | .match_ _ _ arms => !arms.isEmpty && arms.all armExitsFunction
  | _ => false
partial def armExitsFunction : MatchArm → Bool
  | .mk _ _ _ _ _ body => blockExitsFunction body
  | .litArm _ _ _ body => blockExitsFunction body
  | .varArm _ _ _ body => blockExitsFunction body
  | .rangeArm _ _ _ _ _ body => blockExitsFunction body
end

/-- Bug 149: a branch earns the consume-inside-loop exemption only when it exits the function
    and contains no `break`/`continue` that leaves the loop around it (an earlier conditional
    `continue` would repeat the consumption on the next iteration; Codex review). -/
def exemptExitBranch (stmts : List Stmt) : Bool :=
  blockExitsFunction stmts && !stmtsLeaveLoop 0 [] stmts


/-- A branch or arm that EXITS THE FUNCTION must have consumed every linear
    value that was live when it started — `if c { return 0; } … drop(s);`
    leaks `s` on the returning path even though the fall-through is fine.
    Enforced wherever a diverging branch is exempted from the consumption
    MERGE: the exemption is about agreement at the merge point, not about the
    exit itself. -/
-- spec: OWN-2 (docs/spec/ownership.md; this declaration enforces the rule)
def checkReturnPathConsumed (before after : List (LocalRef × VarInfo))
    (span : Option Span := none) : CheckM Unit := do
  for (nameR, infoBefore) in before do
    let name := nameR.name
    if infoBefore.isCopy then continue
    -- References are BORROWS: letting one go out of scope on a return path is
    -- not a leak (&mut is non-Copy only for aliasing exclusivity).
    match infoBefore.ty with
    | .ref _ | .refMut _ => continue
    | _ => pure ()
    -- Bug 149: a borrow block's frozen owner is judged by its state before the freeze.
    let seen : VarInfo → VarState := fun i =>
      if i.state == .frozen then i.frozenFrom.getD i.state else i.state
    if seen infoBefore == .consumed || seen infoBefore == .reserved then continue
    let afterState := match lookupByUid after infoBefore.uid with
      | some info => seen info
      | none => seen infoBefore
    -- `.consumed` = moved on this path; `.reserved` = a pending `defer` will
    -- consume it when this return unwinds — both are accounted for.
    if afterState != .consumed && afterState != .reserved then
      throwCheck (.linearVariableNeverConsumed name) span

/-- Bug 090: the first non-Copy binding a match GUARD moved — live in `before`
    (the env just ahead of the guard), consumed in `after`. A guard runs before
    its arm commits and a false guard falls through to the next arm, so any move
    there happens on a path where a later arm can move the same value again.
    The guard is an expression: it adds no bindings, so the two lists hold the
    same entries in the same order and are compared position by position. -/
def guardMovedName? (before after : List (LocalRef × VarInfo)) : Option String :=
  (before.zip after).findSome? fun ((n, vb), (_, va)) =>
    if !vb.isCopy && vb.state != .consumed && vb.state != .reserved
        && (va.state == .consumed || va.state == .reserved) then some n.name else none

/-- KNOWN_HOLES H9: a non-Copy (linear) value DECLARED inside a block — present in
    `after`, absent from `before` — must be consumed before the block exits. Skipped
    when the block diverges (its textual end is unreachable). This is what the
    function-level `checkScopeExit` could not see for `if`/`else` branch locals and
    matched payload bindings, which are dropped at the branch/arm merge. -/
-- spec: OWN-2 (docs/spec/ownership.md; this declaration enforces the rule)
def checkBlockLocalsConsumed
    (before after : List (LocalRef × VarInfo))
    (diverges : Bool)
    (span : Option Span := none) : CheckM Unit := do
  if diverges then return ()
  for (nameR, info) in after do
    let name := nameR.name
    -- Pre-existing, not block-local. By IDENTITY (bug 093): a block-local that
    -- shadows an outer name (`let r = …` over a consumed outer `r`, a match
    -- binder `value` over an outer `value`) is a different binding and must
    -- itself be consumed.
    if hasUid before info.uid then continue
    if info.isCopy then continue
    if info.state == .consumed || info.state == .reserved then continue
    throwCheck (.linearVariableNeverConsumed name) (info.declSpan.orElse (fun _ => span))

/-- Bug 176: the ONE lexical-scope discipline of Check's binding environment.
    Closes a block opened when the environment's bindings were `outer`: every
    binding the block introduced (by identity — a uid absent from `outer`) is
    dropped, so an outer binding that an inner `let` shadowed is the visible one
    again, with its own type and mutability. Outer bindings keep the states the
    block left them in (a consume inside the block is not undone). Every block
    construct closes through here: `if`/`else` branches (statement and value),
    loop bodies, `for` headers and borrow blocks. -/
def closeBlockScope (outer : List (LocalRef × VarInfo)) : CheckM Unit := do
  let env ← getEnv
  setEnv { env with vars := env.vars.filter fun (_, vi) => hasUid outer vi.uid }

/-- Bug 176: run `k`, which checks `body` as a lexical block nested in the current
    environment, then apply H9 to the block's own bindings and close it
    (`closeBlockScope`). Before this, a `while`/`for` body never closed: its
    bindings stayed in the environment after the loop, so an inner `let v0: u8`
    replaced the outer `v0: i32` for the rest of the function (the checker typed
    and mutability-checked later code against a binding that no longer existed,
    while Elab, the interpreter and the binary read the outer one). -/
def inBlockScope {α : Type} (body : List Stmt) (span : Option Span) (k : CheckM α) : CheckM α := do
  let outer := (← getEnv).vars
  let r ← k
  checkBlockLocalsConsumed outer (← getEnv).vars (blockNonTerminating body) span
  closeBlockScope outer
  return r

-- ============================================================
-- Type substitution for generics
-- ============================================================

/-- Check's view of the environment for the shared argument-type peek
    (`Resolve.PeekType`, single-sourced with Elab — bug 099). -/
def checkPeekCtx : PeekCtx CheckM where
  var sp name := do return ((← lookupUse sp name).map (·.2.ty))
  constant name := do return (← getEnv).constants.lookup name
  fnSig name := lookupFn name
  method t m := do return (← lookupMethod t m).map (·.2)
  typeIdOf := do return (← getEnv).typeIdOf
  fnRefSig name := do return (← getEnv).allFnSummarys.lookup name
  methodSig name := lookupMethodByName name
  struct name := lookupStruct name
  newtype name := lookupNewtype name
  boundMethod n methodName := do
    let env ← getEnv
    let bounds := (env.currentTypeBounds.find? fun (name, _) => name == n).map Prod.snd |>.getD []
    return (boundMethodUnique env.traits bounds methodName).map Prod.snd
  typeParams := do return (← getEnv).currentTypeParams
  resolve ty := try resolveType ty catch _ => pure .placeholder
  subst := substTy

/-- Peek at an expression's type without consuming any linear variables. -/
def peekExprType (e : Expr) : CheckM Ty := peekExprTypeIn checkPeekCtx e

-- Type-arg inference (`unifyTypes`) is single-sourced in `Shared.unifyTypes`
-- (Phase 6.5 InstantiationJudgment axis); Check and Elab share the one algorithm.

/-- Does `ty` (transitively) own a resource — would silently dropping a value of
    this type leak? True if it has a `Destroy` impl, is a heap-owning builtin
    (String / Vec / HashMap / HashSet / Heap / HeapArray), or is a struct / enum /
    array any of whose components owns a resource. False for Copy primitives,
    borrows/pointers, and aggregates built only from trivially-droppable parts
    (e.g. `Option<i32>`). A `_` pattern may silently consume a value ONLY when this
    is false. `fuel` bounds recursion on recursive types (which necessarily reach a
    heap handle and short-circuit to `true`); on exhaustion we answer `true`
    (fail-closed). -/
partial def ownsResource (fuel : Nat) (ty : Ty) : CheckM Bool := do
  match fuel with
  | 0 => return true
  | fuel + 1 =>
    let ty ← resolveType ty
    match ty with
    | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32
    | .bool | .float32 | .float64 | .char | .unit | .never | .placeholder
    | .typeVar _ | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ | .fn_ _ _ _ =>
      return false
    | .string | .heap _ | .heapArray _ => return true
    | .array elem _ => ownsResource fuel elem
    | .named n =>
      if (← lookupMethodByName (destroyFnNameFor n)).isSome then return true
      match ← lookupStruct n with
      | some sd => sd.fields.anyM (fun f => ownsResource fuel f.ty)
      | none => match ← lookupEnum n with
        | some ed => ed.variants.anyM (fun v => v.fields.anyM (fun f => ownsResource fuel f.ty))
        | none => return false
    | .generic n args _ =>
      if n == "Vec" || n == "HashMap" || n == "HashSet" || n == "Heap" || n == "HeapArray" then
        return true
      if (← lookupMethodByName (destroyFnNameFor n)).isSome then return true
      match ← lookupStruct n with
      | some sd =>
        let m := sd.typeParams.zip args
        sd.fields.anyM (fun f => ownsResource fuel (substTy m f.ty))
      | none => match ← lookupEnum n with
        | some ed =>
          let m := ed.typeParams.zip args
          ed.variants.anyM (fun v => v.fields.anyM (fun f => ownsResource fuel (substTy m f.ty)))
        | none => return false

/-- Check trait/`Copy` bounds: each bound type param's concrete instantiation
    must satisfy the bound. `Copy` is the builtin marker (checked via
    `isCopyType`, so Copy structs and primitives both satisfy it); other traits
    require a matching trait impl. Used for free-function bounds AND for
    impl-block bounds enforced at method-call sites (so `impl<V: Copy>` methods
    are not callable on a non-Copy instantiation — closing the decorative-bound
    soundness gap). -/
-- spec: TRAIT-10 (docs/spec/traits.md; this declaration enforces the rule)
partial def checkTraitBounds (bounds : List (String × List String)) (mapping : List (String × Ty))
    (context : String) (span : Option Span := none) : CheckM Unit := do
  let env ← getEnv
  -- Bug 225: one rendering of the type a refused bound names, and every refusal at the call.
  -- The Copy branch kept its own rendering, which printed every builtin type as `<type>`, and
  -- no branch passed a position, so an E0241 named no line.
  let boundSubject : Ty → String := fun
    | .named n | .generic n _ _ | .typeVar n => n
    | t => tyToString t
  let refuse (tn traitName : String) : CheckM Unit :=
    throwCheck (.traitBoundNotSatisfied tn traitName context) span
  for (paramName, requiredTraits) in bounds do
    match mapping.lookup paramName with
    | some concreteType0 =>
      -- A turbofish arg naming one of the CALLER's own type params arrives as
      -- `.named "T"` — normalize to `.typeVar` so the caller's bounds are
      -- consulted (`fib::<T>(n - 1)` inside `fn fib<T: Copy>` satisfies Copy).
      -- RECURSIVELY: `kill::<[T; 2]>(x)` inside `outer<T: Destroy>` supplies an array whose
      -- ELEMENT is the caller's `T`; normalizing only the top level read that element as a
      -- concrete named type `T` with no impl and refused the bound (Codex review of bug 052).
      let tps := env.currentTypeParams
      let rec norm : Ty → Ty
        | .named n => if tps.contains n then .typeVar n else .named n
        | .ref t => .ref (norm t)
        | .refMut t => .refMut (norm t)
        | .heap t => .heap (norm t)
        | .heapArray t => .heapArray (norm t)
        | .array t k => .array (norm t) k
        | .ptrMut t => .ptrMut (norm t)
        | .ptrConst t => .ptrConst (norm t)
        | .generic n args caps => .generic n (args.map norm) caps
        | .fn_ ps cs r => .fn_ (ps.map norm) cs (norm r)
        | t => t
      let concreteType := norm concreteType0
      for traitName in requiredTraits do
        if traitName == "Copy" then
          if !(← isCopyType concreteType) then
            refuse (boundSubject concreteType) "Copy"
        else if traitName == "Destroy" then
          -- H18 drop-glue: a `Destroy` BOUND means "destroyable" — satisfied by
          -- Copy (trivial destruction; mono elides the T_destroy call), by an
          -- explicit `impl Destroy`, or by a type param that itself carries a
          -- Copy or Destroy bound. A non-Copy type WITHOUT Destroy (fallible
          -- cleanup: File/Writer/socket) fails — by design, those are drained
          -- and closed explicitly, never dropped (RUNTIME_COLLECTIONS.md
          -- drop-glue rules 1-2). For a GENERIC target the registered impl is
          -- treated as CONDITIONAL: all type args must be destroyable too
          -- (`Vec<Vec<String>>` composes recursively; `Vec<Writer>` fails at
          -- any depth). Conservative: an unconditional generic Destroy impl
          -- would be over-restricted — refine to bound-aware registry entries
          -- if a workload ever writes one.
          let rec destroyable (fuel : Nat) (t : Ty) : CheckM Bool := do
            match fuel with
            | 0 => return false
            | fuel+1 =>
              if ← isCopyType t then return true
              else match t with
              | .named tn =>
                return env.hasTraitImpl tn "Destroy"
              | .generic tn args _ =>
                if env.hasTraitImpl tn "Destroy" then
                  args.allM (destroyable fuel)
                else return false
              | .typeVar n =>
                let callerBounds := (env.currentTypeBounds.find? fun (bn, _) => bn == n).map Prod.snd |>.getD []
                return callerBounds.contains "Destroy" || callerBounds.contains "Copy"
              -- bug 052: an array is destroyable iff its element is (mono
              -- synthesizes per-element glue). The old catch-all accepted ANY
              -- unmatched type, so a non-Copy array, Heap, or a String with no
              -- `Destroy` impl satisfied the bound and mono answered the
              -- destroy call with a silent no-op.
              | .array elem _ => destroyable fuel elem
              | .string =>
                return env.traitImpls.any fun (ty, tr) => ty == "String" && tr == "Destroy"
              | .heap _ | .heapArray _ | .refMut _ => return false
              | _ => return true  -- remaining shapes are Copy (handled above)
          if !(← destroyable 8 concreteType) then
            refuse (boundSubject concreteType) "Destroy"
        else
          match concreteType with
          | .named tn | .generic tn _ _ =>
            if !(env.hasTraitImpl tn traitName) then
              refuse tn traitName
          | .typeVar n =>
            -- caller's own type param: satisfied if the caller declares the
            -- same trait bound on it
            let callerBounds := (env.currentTypeBounds.find? fun (bn, _) => bn == n).map Prod.snd |>.getD []
            if !callerBounds.contains traitName then
              refuse n traitName
          | t =>
            -- Bug 204: a primitive (or any other unnamed) type argument needs an impl too. An
            -- impl of a primitive names it by any spelling of its canonical type (`Int`/`i64`,
            -- the TRAIT-D1 reading), so the impl's head is compared canonically. This arm
            -- skipped the check, and the missing method surfaced only in the backend.
            if !(env.traitImpls.any fun (tn, tr) => tr == traitName && tyCanonical (tyFromName tn) == tyCanonical t) then
              refuse (boundSubject t) traitName
    | none => pure ()

/-- Infer a method's OWN type params and capability params from its argument
    types, mirroring the free-function call inference. `implMapping` already
    binds the impl's type params (e.g. K, V) from the receiver's type args.
    Returns the method's parameter types (self dropped) and return type with the
    full type mapping AND resolved capability variables applied, and checks the
    caller holds the resolved capabilities. This is what makes capability-
    polymorphic methods (`fn m<…, cap C>(…) with(C)`) inferable without
    turbofish — the prerequisite for the HOF/scoped-callback stdlib surface
    (ROADMAP Phase 5 #24). -/
partial def inferMethodParamAndRetTys
    (sig : FnSummary) (implMapping : List (String × Ty)) (methodTypeParams : List String)
    (explicitTypeArgs : List Ty) (args : List Expr) (callName : String) (sp : Span)
    (recvTy : Ty := .placeholder)
    : CheckM (List (String × Ty) × Ty) := do
  -- Method param types (self dropped) with the impl mapping applied.
  let methodParamTys := (sig.params.drop 1).map fun (n, t) => (n, substTy implMapping t)
  -- 1. Infer the method's own type params from argument types (unless turbofished).
  let methodArgs : List Ty ←
    if !explicitTypeArgs.isEmpty || methodTypeParams.isEmpty then
      pure explicitTypeArgs
    else do
      let mut inferred : List (String × Ty) := []
      for (arg, (_, pTy)) in args.zip methodParamTys do
        let argTy ← peekExprType arg
        for (name, ty) in unifyTypes pTy argTy methodTypeParams do
          inferred := recordInferred inferred name ty
      pure (methodTypeParams.map fun tp => (inferred.lookup tp).getD (.typeVar tp))
  let fullMapping := implMapping ++ methodTypeParams.zip methodArgs
  -- Enforce the impl's + method's trait/Copy bounds at the call site (the impl's
  -- bounds were prepended to sig.typeBounds). This makes e.g. a `Copy`-bounded
  -- value accessor uncallable on a non-Copy container — closing the
  -- decorative-impl-bound soundness gap.
  if !sig.typeBounds.isEmpty then
    checkTraitBounds sig.typeBounds fullMapping s!"method '{callName}'" (some sp)
  -- References are second-class: a type parameter instantiated to a reference
  -- may not occur in the return type (else `m<R>(...) -> Option<R>` with R=&V is
  -- a generic backdoor to returning a reference). VALUE_MODEL.md.
  for (pName, pTy) in fullMapping do
    if tyContainsRef pTy && tyParamOccursIn pName sig.retTy then
      throwCheck (.returnsReference s!"method '{callName}': type parameter '{pName}' is instantiated to a reference ('{tyToString pTy}') and occurs in the return type; references may not be returned (VALUE_MODEL.md). Return a value, an owned view, or use a scoped callback." "REF-4@1")
  refuseNestedRefInstantiation s!"method '{callName}'" fullMapping (sig.params.map (·.2)) sig.retTy
  -- 2. Infer capability-variable bindings: from the RECEIVER first (R-0484 — on
  --    `impl<cap C> Sink<C>`, `s.run(..)` with `s: &Sink<Console>` binds C := Console),
  --    then from the arguments.
  let mut capBindings : List (String × List String) := []
  if !sig.capParams.isEmpty then
    match sig.params.head? with
    | some (_, selfTy) =>
      let selfInner := match selfTy with | .ref t | .refMut t => t | t => t
      for (k, v) in inferCapBindings sig.capParams selfInner recvTy (sameTypeName (← getEnv).typeIdOf) do
        if !(capBindings.any (·.1 == k)) then capBindings := (k, v.allNames) :: capBindings
    | none => pure ()
    for (arg, (_, pTy)) in args.zip methodParamTys do
      match pTy with
      | .fn_ _ (.concrete caps) _ =>
        for cap in caps do
          if sig.capParams.contains cap then
            -- BUG 063, METHOD PATH. Identical to the function-call path in
            -- `Check.lean`, and missed by the first fix, which is why the sweep for
            -- "absence recorded as a positive fact" found it: the shape recurs, so
            -- fixing one instance is not fixing the defect.
            --
            -- It matters more here than the count suggests. The capability-polymorphic
            -- combinators that actually exist in `std` — `Set::fold`, `for_each`,
            -- `with_value` — are METHODS, so real callers reach this path and not the
            -- other one.
            --
            -- `none` means "not known here" and must stay distinguishable from
            -- `some .empty`, "requires no authority". Contributing nothing lets
            -- `resolveCaps` name the variable it could not infer, instead of a
            -- fabricated `C := {}` passing the authority check and leaving `expectTy`
            -- to reject the program for an unrelated reason.
            let argCapSet? ← do
              let argTy ← peekExprType arg
              match argTy with
              | .fn_ _ cs _ => pure (some cs)
              | _ =>
                match arg with
                | .ident _ varName =>
                  match ← lookupFn varName with
                  | some argSig => pure (some argSig.capSet)
                  | none => pure none
                | _ => pure none
            match argCapSet? with
            | some cs =>
              let (argCaps, _) := cs.normalize
              capBindings := capBindings ++ [(cap, argCaps)]
            | none => pure ()
      | _ =>
        -- A capability parameter carried by a capability argument (`s: &Sink<C>`).
        if (tyCapNames pTy).any sig.capParams.contains then
          let argTy ← peekExprType arg
          for (k, v) in inferCapBindings sig.capParams pTy argTy (sameTypeName (← getEnv).typeIdOf) do
            if !(capBindings.any (·.1 == k)) then capBindings := (k, v.allNames) :: capBindings
  -- 3. Resolve the method's declared capset against the inferred bindings, and
  --    check the caller holds the result.
  if !sig.capParams.isEmpty then
    -- Same cap-poly resolution as the call path (Capabilities.resolveCaps).
    let resolvedCaps ← match Capabilities.resolveCaps sig.capParams capBindings sig.capSet with
      | .ok caps => pure caps
      | .error cv => throwCheck (.cannotInferCapVariable cv callName) (some sp)
    let env ← getEnv
    -- One entailment rule on the INSTANTIATED set (Capabilities.missingCaps) — see
    -- Check.lean. `Unsafe` included (bug 188): CoreCheck cannot see the instantiation,
    -- so a callback's `Unsafe` reaching the method through `C` is decided here. A
    -- trusted body discharges it exactly as CoreCheck's call-site discharge does.
    for cap in Capabilities.missingCaps env.currentCapSet (.concrete resolvedCaps) do
      if !Capabilities.trustedDischarges env.currentFnTrusted cap then
        throwCheck (.missingCapability callName cap env.currentFnName) (some sp)
  -- 4. Resolve cap variables inside fn-typed param types so a pure/empty-cap
  --    callback argument matches the declared `with(C)` parameter.
  let resolveCapInTy : Ty → Ty := fun ty =>
    match ty with
    | .fn_ ps (.concrete caps) ret =>
      let newCaps := caps.foldl (fun acc cap =>
        if sig.capParams.contains cap then
          match capBindings.find? fun (n, _) => n == cap with
          | some (_, resolved) => acc ++ resolved
          | none => acc
        else acc ++ [cap]) []
      .fn_ ps (.concrete newCaps) ret
    | t => t
  -- ...and inside capability arguments (`&Sink<C>`), by the same bindings.
  let capArgMap : List (String × CapSet) := capBindings.map fun (k, names) =>
    (k, (if names.isEmpty then CapSet.empty else CapSet.concrete names).canonArg)
  let resolvedParamTys := (sig.params.drop 1).map fun (n, t) =>
    (n, substCapNamesTy capArgMap (resolveCapInTy (substTy fullMapping t)))
  let resolvedRetTy := substCapNamesTy capArgMap (resolveCapInTy (substTy fullMapping sig.retTy))
  return (resolvedParamTys, resolvedRetTy)

-- ============================================================
-- Type checking expressions and statements
-- ============================================================

/-- An expression with its enclosing parentheses removed. -/
partial def unparenExpr : Expr → Expr
  | .paren _ inner => unparenExpr inner
  | e => e

/-! ## The normalized place rule (bug 091 round 3)

ONE rule decides what storage a place expression names, used by every check that cares:
explicit `&e` / `&mut e`, implicit receiver borrows, reference-typed call arguments,
reference provenance (`let r = ..`, `r = ..`), the call-overlap check (E0293), by-value
moves out of a place, and destruction. A canonical place is a binding followed by field /
index / newtype `.0` projections, where each `*` dereferences a reference (or pointer) whose
own source is traced: a binding of reference type (its recorded provenance), a borrow of a
canonical place, or a cast of one. Parentheses and identity dereferences normalize away, so
`(*r).p` and `r.p` are one place and `&*r` names `r`'s source. Anything else — a reference
produced by an `if`/`match`, `*` over a non-place, `**r` — does not normalize and is refused
(E0238) wherever a place is required: the checker never guesses an alias it cannot see. -/

/-- How a canonical place is reached. -/
inductive Reach where
  | owned
  | exclusive
  | raw
  | shared (desc : String)
  deriving Inhabited

/-- Reaching a place through two links: a `&` anywhere makes it shared; then `&mut`; then
    a raw pointer; otherwise it is owned storage. -/
def Reach.join : Reach → Reach → Reach
  | .shared d, _ | _, .shared d => .shared d
  | .exclusive, _ | _, .exclusive => .exclusive
  | .raw, _ | _, .raw => .raw
  | _, _ => .owned

def Reach.throughRef : Reach → Bool
  | .shared _ | .exclusive => true
  | _ => false

/-- A normalized place. `root` is the conflict key (the binding whose storage this is);
    `steps` are the projections from it, exact unless a dereference crossed a reference
    whose source path is not recorded (then the place is the whole root, conservatively).
    `binding` is set when the place IS a bare binding; `viaRef` names the innermost
    reference binding dereferenced to reach it; `heapLoad` marks `*h` of an owned heap. -/
structure NPlace where
  root : Option LocalRef
  steps : List String := []
  exact : Bool := true
  reach : Reach := .owned
  binding : Option LocalRef := none
  viaRef : Option LocalRef := none
  heapLoad : Bool := false
  deriving Inhabited

inductive Norm where
  | place (p : NPlace)
  | temp
  | reject (why : String)
  deriving Inhabited

def NPlace.project (p : NPlace) (step : String) : NPlace :=
  { p with steps := if p.exact then p.steps ++ [step] else p.steps,
           binding := none, heapLoad := false }

def Norm.project : Norm → String → Norm
  | .place p, step => .place (p.project step)
  | n, _ => n

private def Norm.reachedThrough : Norm → Reach → Norm
  | .place p, r => .place { p with reach := p.reach.join r, binding := none }
  | n, _ => n

/-- `peekExprType` with type aliases (and `Self`) resolved, so the place rule recognizes a
    reference spelled through an alias (`type R = &P`). -/
def peekResolved (e : Expr) : CheckM Ty := do
  let t ← peekExprType e
  tryCatch (resolveType t) (fun _ => pure t)

def resolvedOr (t : Ty) : CheckM Ty := tryCatch (resolveType t) (fun _ => pure t)

/-- The place a reference-, pointer- or heap-typed local `r` (with state `info`) points to. -/
def normRefLocal (r : LocalRef) (info : VarInfo) : Norm :=
  let n := r.name
  let src := info.borrowedFrom.getD r
  -- the recorded source PATH makes `*r` key exactly like the place it borrows
  let (steps, exact) := if info.borrowedFrom.isSome then (info.borrowedPath, info.borrowedExact) else ([], true)
  match info.ty with
  | .ref _ =>
    .place { root := some src, steps := steps, exact := exact, viaRef := some r,
             reach := .shared s!"'{n}' is a `&` reference" }
  | .refMut _ =>
    .place { root := some src, steps := steps, exact := exact, viaRef := some r, reach := .exclusive }
  | .ptrMut _ | .ptrConst _ => .place { root := some r, reach := .raw }
  | .heap _ | .heapArray _ => .place { root := some r, heapLoad := true }
  | _ => .reject s!"'{n}' is not a reference"

mutual
/-- The storage the place expression `e` names. -/
-- spec: REF-5 (docs/spec/references.md; this declaration enforces the rule)
partial def normPlace (e : Expr) : CheckM Norm := do
  match e with
  | .paren _ inner => normPlace inner
  | .ident sp n =>
    match ← lookupUse sp n with
    | some (r, _) => return .place { root := some r, binding := some r }
    | none =>
      if ((← getEnv).constants.lookup n).isSome then return .temp
      return .reject s!"'{n}' is not a local binding"
  | .fieldAccess _ base f => return (← normProjBase base).project f
  | .arrayIndex _ base _ => return (← normProjBase base).project "[]"
  | .deref _ inner => normRef inner
  | .call .. | .methodCall .. | .staticMethodCall .. | .structLit .. | .enumLit ..
  | .arrayLit .. | .intLit .. | .floatLit .. | .boolLit .. | .strLit .. | .charLit .. =>
    return .temp
  | .borrow .. | .borrowMut .. =>
    return .reject "a borrow is a reference, not a place; dereference a named reference instead"
  | .ifExpr .. | .match_ .. =>
    return .reject "an `if`/`match` result is not a named place; bind it to a name first"
  | _ => return .reject "the expression is not a named place; bind it to a name first"

/-- A projection's base: field access dereferences one layer of reference, pointer or heap
    shell implicitly, exactly as `*` does, so `r.p` normalizes as `(*r).p`. -/
partial def normProjBase (base : Expr) : CheckM Norm := do
  match unparenExpr base with
  | .ident sp n =>
    match ← lookupUse sp n with
    | some (r, info) =>
      match info.ty with
      | .ref _ | .refMut _ | .ptrMut _ | .ptrConst _ | .heap _ | .heapArray _ =>
        return normRefLocal r info
      | _ => normPlace base
    | none => normPlace base
  | b =>
    match ← peekResolved b with
    | .ref _ | .refMut _ => normRef b
    | .placeholder | .typeVar _ =>
      -- the base's type is not known here, so it might be a reference: only a fresh
      -- temporary (a call result, a literal) is safe to treat as owned (fail closed)
      match ← normPlace b with
      | .temp => return .temp
      | .reject why => return .reject why
      | .place _ => return .reject "the type of this projection's base is not known to the place rule; bind it to a typed name first"
    | _ => normPlace b

/-- The place a REFERENCE-valued (or pointer-valued) expression points to. -/
partial def normRef (e : Expr) : CheckM Norm := do
  match e with
  | .paren _ inner => normRef inner
  -- D2: a string literal is a static, immutable root, so `&"text"` points at storage, and a
  -- reborrow or projection through it (`&*(&"ab")`) keeps that root (Codex review).
  | .borrow _ inner =>
    match unparenExpr inner with
    | .strLit .. => return .place { root := none, reach := .shared "a string literal is static, immutable storage" }
    | _ => return (← normPlace inner).reachedThrough (.shared "the place is reached through a `&` borrow")
  | .borrowMut _ inner => return (← normPlace inner).reachedThrough .exclusive
  | .ident sp n =>
    match ← lookupUse sp n with
    | some (r, info) => return normRefLocal r info
    | none => return .reject s!"'{n}' is not a local binding"
  | .cast _ inner ty0 =>
    let ty ← resolvedOr ty0
    match ty with
    | .ref _ | .refMut _ =>
      let r : Reach := match ty with
        | .ref _ => .shared "the place is reached through a cast to `&`"
        | _ => .exclusive
      -- the operand is itself a reference or pointer value (`(r as &T) as &T`, `&x as &T`):
      -- the cast points where the operand points
      let base ← match unparenExpr inner with
        | .cast .. | .borrow .. | .borrowMut .. => normRef inner
        | _ => normProjBase inner
      match base with
      | .place p => return .place { p with reach := p.reach.join r, binding := none, heapLoad := false }
      | .temp => return .place { root := none, reach := r }
      | .reject why =>
        -- a reference cast from a computed ADDRESS (trusted raw code): untracked storage;
        -- anything else that does not normalize stays refused
        match ← peekResolved inner with
        | .ptrMut _ | .ptrConst _ | .int | .uint | .u32 | .i32 | .u16 | .i16 | .u8 | .i8 =>
          return .place { root := none, reach := r }
        | _ => return .reject why
    -- a cast to a raw pointer names raw storage, whatever the operand (`(base + i) as *mut u8`)
    | _ => return .place { root := none, reach := .raw }
  | .fieldAccess .. | .arrayIndex .. =>
    -- a pointer or heap shell stored in a field (`*self.ptr`)
    match ← peekResolved e with
    -- the pointee of a raw pointer READ out of a place is raw storage, however that place
    -- was reached (copying a pointer out of `&self` does not make its target shared)
    | .ptrMut _ | .ptrConst _ => return .place { root := none, reach := .raw }
    | .heap _ | .heapArray _ => return (← normPlace e)
    | _ => return .reject "a reference stored in a place cannot be traced; bind it to a name first"
  | .deref .. => return .reject "a dereference of a dereference is not traced; bind the inner reference to a name first"
  | .ifExpr .. | .match_ .. =>
    return .reject "a reference produced by an `if`/`match` cannot be traced; borrow a named place in each branch's caller instead"
  | other =>
    -- pointer arithmetic and other raw-pointer-valued expressions name raw storage
    match ← peekResolved other with
    | .ptrMut _ | .ptrConst _ => return .place { root := none, reach := .raw }
    | _ => return .reject "the reference's source cannot be traced; bind it to a name first"
end

/-- The conflict key of a normalized place: root + projection path (whole root when the
    path through a dereference is not exact). `none` for storage outside this function's
    bindings and for temporaries. -/
def NPlace.key (p : NPlace) : Option (LocalRef × List String) :=
  p.root.map fun r => (r, if p.exact then p.steps else [])

/-- D2 (the Austral model): loans are keyed by ROOT variable only. A loan on `&s.f` is a loan on
    `s`, so two borrows within one call overlap whenever they share a root, disjoint fields
    included (`upd(&mut s.a, &s.b)` is refused). This matches the whole-owner freezing a borrow
    block and an arm binder already do. Field-precise loans are deferred until a workload needs
    two simultaneous borrows of different fields of one owner. -/
-- spec: REF-11 (docs/spec/references.md; this declaration enforces the rule)
def pathsOverlap (a b : LocalRef × List String) : Bool :=
  a.1 == b.1

/-- #18 container-not-in-context: within one call's arguments (plus the
    auto-borrowed method receiver), two borrows may not OVERLAP when either
    is `&mut` (E0293). Covers `&x`/`&mut x`, projections (`&mut w.f` twice,
    or `&mut w` + `&mut w.f`), and single-hop ALIASES: an ident argument
    whose binding is a ref/&mut with a tracked `borrowedFrom` root counts as
    a borrow of that root. Shared+shared overlap is fine. -/
-- spec: REF-8 (docs/spec/references.md; this declaration enforces the rule)
def checkCallBorrowConflicts (parts : List ((LocalRef × List String) × Bool)) (span : Option Span) : CheckM Unit := do
  let mut seen : List ((LocalRef × List String) × Bool) := []
  for (path, isMut) in parts do
    for (prevPath, prevMut) in seen do
      if pathsOverlap path prevPath && (isMut || prevMut) then
        -- REF-8 when one path is a prefix of the other (they overlap as places); REF-11 when
        -- they are disjoint projections that conflict only through their shared root.
        let nested := path.2.isPrefixOf prevPath.2 || prevPath.2.isPrefixOf path.2
        throwCheck (.conflictingCallBorrows path.1.name !nested) span
    seen := (path, isMut) :: seen

mutual
/-- Every variable name an expression mentions, with the position of the use (R-0492: a use resolves
    through the local table by position, never by spelling), nested blocks included (`if` / `match`
    branches, guards and arm bodies): a read inside a branch is still a read of the root (Codex review). -/
partial def exprMentionedNames : Expr → List (Span × String)
  | .ident sp n => [(sp, n)]
  | .paren _ e | .unaryOp _ _ e | .fieldAccess _ e _ | .borrow _ e | .borrowMut _ e | .deref _ e
  | .cast _ e _ => exprMentionedNames e
  | .binOp _ _ l r | .arrayIndex _ l r | .allocCall _ l r => exprMentionedNames l ++ exprMentionedNames r
  | .call _ _ _ args | .staticMethodCall _ _ _ _ args | .arrayLit _ args => args.flatMap exprMentionedNames
  | .methodCall _ o _ _ args => exprMentionedNames o ++ args.flatMap exprMentionedNames
  | .structLit _ _ _ fs b => fs.flatMap (fun (_, e) => exprMentionedNames e) ++ (b.map exprMentionedNames).getD []
  | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, e) => exprMentionedNames e)
  | .match_ _ scr arms => exprMentionedNames scr ++ arms.flatMap armMentionedNames
  | .ifExpr _ c t e => exprMentionedNames c ++ stmtsMentionedNames t ++ stmtsMentionedNames e
  | _ => []

partial def armMentionedNames : MatchArm → List (Span × String)
  | .mk _ _ _ _ g body | .varArm _ _ g body =>
    ((g.map exprMentionedNames).getD []) ++ stmtsMentionedNames body
  | .litArm _ v g body => exprMentionedNames v ++ ((g.map exprMentionedNames).getD []) ++ stmtsMentionedNames body
  | .rangeArm _ lo hi _ g body =>
    exprMentionedNames lo ++ exprMentionedNames hi ++ ((g.map exprMentionedNames).getD []) ++ stmtsMentionedNames body

partial def stmtsMentionedNames (ss : List Stmt) : List (Span × String) :=
  ss.flatMap fun s => match s with
    | .letDecl _ _ _ _ v _ | .defer _ v | .assert_ _ v | .assume_ _ v
    | .letStructDestructure _ _ _ v => exprMentionedNames v
    | .assign sp n v => (sp, n) :: exprMentionedNames v
    | .return_ _ v | .break_ _ v _ => (v.map exprMentionedNames).getD []
    | .expr _ e _ => exprMentionedNames e
    | .ifElse _ c t e => exprMentionedNames c ++ stmtsMentionedNames t ++ ((e.map stmtsMentionedNames).getD [])
    | .while_ _ c body _ => exprMentionedNames c ++ stmtsMentionedNames body
    | .forLoop _ i c st body _ =>
      ((i.map fun x => stmtsMentionedNames [x]).getD []) ++ exprMentionedNames c
        ++ ((st.map fun x => stmtsMentionedNames [x]).getD []) ++ stmtsMentionedNames body
    | .fieldAssign _ o _ v | .derefAssign _ o v => exprMentionedNames o ++ exprMentionedNames v
    | .arrayIndexAssign _ a i v => exprMentionedNames a ++ exprMentionedNames i ++ exprMentionedNames v
    | .borrowIn sp v _ _ _ body => (sp, v) :: stmtsMentionedNames body
    | .letDestructure _ _ _ _ v e => exprMentionedNames v ++ ((e.map stmtsMentionedNames).getD [])
    | _ => []
end

/-- D2 / D1: the root of the NEW mutable loan an operand makes, if it makes one. A plain reborrow of
    a reference binding (`r`, `&mut *r`, a receiver `r` whatever kind of binding `r` is: a parameter,
    a borrow-block binder, an arm binder) re-lends a reference the caller already holds and makes no
    new loan (D1); anything else that is borrowed mutably (`&mut s.a`, a `self.offs` receiver, an
    owned `s`) is a loan on its root (Codex review: decided from the operand, not from its root). -/
def mutLoanRoot? (operand : Expr) (explicitBorrow : Bool) : CheckM (Option LocalRef) := do
  -- An explicit `&mut e` (an argument, or a receiver spelled `(&mut s.a).m()`) loans `e`'s root
  -- (Codex review: the receiver form was left unnormalized and slipped through).
  let _ := explicitBorrow
  let inner := match unparenExpr operand with
    | .borrowMut _ x => x
    | x => x
  let rec stripDerefs : Expr → Expr
    | .paren _ x | .deref _ x => stripDerefs x
    | x => x
  match stripDerefs inner with
  | .ident sp n =>
    match ← lookupUse sp n with
    | some (_, info) =>
      match info.ty with
      | .ref _ | .refMut _ => return none
      | _ => pure ()
    | none => pure ()
  | _ => pure ()
  match ← normPlace inner with
  | .place p => return p.key.map (·.1)
  | _ => return none

/-- D2: loans are keyed by root, so while one call borrows a root mutably, no OTHER argument (or
    the receiver) of that call may mention that root at all: `add(&mut s.a, s.b)` and
    `self.offs.push(self.names.len())` (the `&mut` receiver loan on `self` and a read of
    `self.names`) are E0293, where a field-precise rule accepted them. A reference binding counts
    as its recorded source. `operands` are the receiver (if any) and the arguments, in order;
    `parts` are the call's borrows. A mutable borrow counts when it is a NEW loan on a root: of an
    owned variable, or a projection (`&mut s.a`, a `self.offs` receiver). A plain reborrow of a
    reference binding (`self.sift_up(self.len - 1)` with `self: &mut Self`) makes no new loan on the
    root: it re-lends the reference the caller already holds, so reading through that reference in
    another argument is not a second loan (D1: every `&mut` argument is an implicit reborrow). -/
-- spec: REF-11 (docs/spec/references.md; this declaration enforces the rule)
def checkCallRootReads (mutRoots : List LocalRef) (operands : List Expr) (span : Option Span) : CheckM Unit := do
  if mutRoots.isEmpty then return
  let mut counts : List (LocalRef × Nat) := []
  for o in operands do
    let mut rootsHere : List LocalRef := []
    for (sp, n) in exprMentionedNames o do
      -- a name that is not a local (a function, a constant) is no loan's root
      match ← lookupUse sp n with
      | some (r, info) =>
        let root := info.borrowedFrom.getD r
        if mutRoots.contains root && !rootsHere.contains root then rootsHere := root :: rootsHere
      | none => pure ()
    for r in rootsHere do
      counts := match counts.lookup r with
        | some k => (r, k + 1) :: counts.filter (·.1 != r)
        | none => (r, 1) :: counts
  match counts.find? (fun (_, k) => k ≥ 2) with
  | some (r, _) => throwCheck (.conflictingCallBorrows r.name true) span
  | none => pure ()

/-- The borrows a call makes, as NORMALIZED places (E0293): a `&e` / `&mut e` argument
    borrows `e`'s place, a reference binding argument borrows its source. Every spelling of
    one place (`(&p)`, `&*(&*r)`, `&(*r).p`) yields the same key. -/
def argBorrowParts (args : List Expr) : CheckM (List ((LocalRef × List String) × Bool)) := do
  let mut out : List ((LocalRef × List String) × Bool) := []
  for a in args do
    let part? : Option (Norm × Bool) ← match unparenExpr a with
      | .borrow _ inner => do pure (some (← normPlace inner, false))
      | .borrowMut _ inner => do pure (some (← normPlace inner, true))
      | .ident sp n => do
        match ← lookupUse sp n with
        | some (r, info) =>
          match info.ty with
          | .ref _ => pure (some (normRefLocal r info, false))
          | .refMut _ => pure (some (normRefLocal r info, true))
          | _ => pure none
        | none => pure none
      -- a cast to a reference borrows what its operand points to (`f(r as &mut P)`)
      | c@(.cast _ _ ty0) => do
        match ← resolvedOr ty0 with
        | .ref _ => pure (some (← normRef c, false))
        | .refMut _ => pure (some (← normRef c, true))
        | _ => pure none
      | _ => pure none
    match part? with
    | some (.place p, isMut) =>
      match p.key with
      | some k => out := (k, isMut) :: out
      | none => pure ()
    | _ => pure ()
  return out.reverse

end Concrete
