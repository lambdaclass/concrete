import Concrete.Core
import Concrete.AST
import Concrete.Diagnostic
import Concrete.Shared
import Concrete.Layout
import Concrete.Intrinsic

namespace Concrete

/-! ## Core IR Validation — post-elaboration semantic authority

Runs after CoreCanonicalize, before Mono/Lower. Recursively processes
top-level modules and all nested submodules.

Function-body validation:
- Capability discipline: caller's capSet ⊇ callee's capSet
- Type consistency: operand types match operators, arguments match parameters
- Structural invariants: break/continue inside loops, match coverage

Declaration-level validation (on Core IR, not surface AST):
- Copy/Destroy conflicts, Copy field validation
- repr(C)/packed/align validation, FFI safety
- Trait impl completeness with signature matching
- Builtin trait redeclaration, reserved function names
-/

-- ============================================================
-- Validation environment
-- ============================================================

structure CoreCheckEnv where
  fnSigs : List (String × CapSet × List (String × Ty) × Ty)
  structDefs : List CStructDef
  enumDefs : List CEnumDef
  vars : List (String × Ty)
  currentCapSet : CapSet
  currentRetTy : Ty
  inLoop : Bool
  inTrusted : Bool
  errors : Diagnostics

abbrev CoreCheckM := StateM CoreCheckEnv Unit

inductive CoreCheckError where
  -- Type consistency
  | typeMismatchVariable (name : String) (declared : String) (used : String)
  | arithmeticOnNonNumeric (ty : String)
  | binaryOperandMismatch (lTy : String) (rTy : String)
  | comparisonOperandMismatch (lTy : String) (rTy : String)
  | comparisonResultNotBool (ty : String)
  | logicalOnNonBool (lTy : String) (rTy : String)
  | bitwiseOnNonInteger (ty : String)
  | negationOnNonNumeric (ty : String)
  | logicalNotOnNonBool (ty : String)
  | bitwiseNotOnNonInteger (ty : String)
  -- Capability discipline
  | insufficientCapabilities (fn : String) (required : String) (available : String)
  | missingCapability (callee : String) (cap : String) (caller : String)
  | argCountMismatch (fn : String) (expected : Nat) (got : Nat)
  -- Match coverage
  | matchMissingVariant (enumName : String) (variant : String)
  | matchArmWrongEnum (armEnum : String) (scrutineeEnum : String)
  | duplicateMatchArm (variant : String)
  | variantFieldCountMismatch (variant : String) (expected : Nat) (actual : Nat)
  | matchNonEnumNoDefault
  -- Control flow
  | whileCondNotBool (ty : String)
  | ifCondNotBool (ty : String)
  | breakOutsideLoop
  | continueOutsideLoop
  -- Type legality
  | arrayLiteralEmpty
  | arrayIndexNotInteger (ty : String)
  | indexingNonArray (ty : String)
  | cannotCast (fromTy : String) (toTy : String)
  | cannotDerefNonRef (ty : String)
  | cannotAssignThroughNonMutRef (ty : String)
  -- Return type
  | returnTypeMismatch (expected : String) (got : String)
  -- Module-level validation (moved from Check)
  | copyDestroyConflict (typeName : String)
  | copyFieldNotCopy (structName : String) (fieldName : String)
  | reprCHasGenerics (structName : String)
  | reprCFieldNotFFISafe (structName fieldName fieldTy : String)
  | externFnParamNotFFISafe (fnName paramName paramTy : String)
  | externFnReturnNotFFISafe (fnName retTy : String)
  | reprPackedAndAlignConflict (structName : String)
  | reprAlignNotPowerOfTwo (structName : String) (n : Nat)
  | reservedFnName (name : String)
  | builtinTraitRedeclared
  | unknownTrait (traitName : String)
  | missingTraitMethod (typeName methodName : String)
  | traitMethodRetTyMismatch (methodName expectedRetTy actualRetTy : String)

def CoreCheckError.message : CoreCheckError → String
  | .typeMismatchVariable name declared used => s!"type mismatch for variable '{name}': declared {declared}, used as {used}"
  | .arithmeticOnNonNumeric ty => s!"arithmetic operator on non-numeric type: {ty}"
  | .binaryOperandMismatch lTy rTy => s!"binary operand type mismatch: {lTy} vs {rTy}"
  | .comparisonOperandMismatch lTy rTy => s!"comparison operand type mismatch: {lTy} vs {rTy}"
  | .comparisonResultNotBool ty => s!"comparison result should be Bool, got {ty}"
  | .logicalOnNonBool lTy rTy => s!"logical operator on non-Bool types: {lTy}, {rTy}"
  | .bitwiseOnNonInteger ty => s!"type mismatch in bitwise op: expected integer type, got {ty}"
  | .negationOnNonNumeric ty => s!"negation on non-numeric type: {ty}"
  | .logicalNotOnNonBool ty => s!"logical not on non-Bool type: {ty}"
  | .bitwiseNotOnNonInteger ty => s!"type mismatch in bitwise not: expected integer type, got {ty}"
  | .insufficientCapabilities fn required available => s!"function '{fn}' requires {required} but caller has {available}"
  | .missingCapability callee cap _caller => s!"function '{callee}' requires capability '{cap}' but caller does not declare it"
  | .argCountMismatch fn expected got => s!"function '{fn}' expects {expected} args, got {got}"
  | .matchMissingVariant enumName variant => s!"non-exhaustive match: missing variant '{variant}' in enum '{enumName}'"
  | .matchArmWrongEnum armEnum scrutineeEnum => s!"match arm has enum '{armEnum}' but scrutinee is '{scrutineeEnum}'"
  | .duplicateMatchArm variant => s!"duplicate match arm for variant '{variant}'"
  | .variantFieldCountMismatch variant expected actual => s!"variant '{variant}' has {expected} fields but arm binds {actual}"
  | .matchNonEnumNoDefault => "non-exhaustive match on non-enum type requires a default '_' arm"
  | .whileCondNotBool ty => s!"while condition must be Bool, got {ty}"
  | .ifCondNotBool ty => s!"if condition must be Bool, got {ty}"
  | .breakOutsideLoop => "break outside of loop"
  | .continueOutsideLoop => "continue outside of loop"
  | .arrayLiteralEmpty => "array literal cannot be empty"
  | .arrayIndexNotInteger ty => s!"type mismatch: array index must be an integer type, got {ty}"
  | .indexingNonArray ty => s!"type mismatch: indexing into non-array type {ty}"
  | .cannotCast fromTy toTy => s!"cannot cast {fromTy} to {toTy}"
  | .cannotDerefNonRef ty => s!"cannot dereference non-reference type {ty}"
  | .cannotAssignThroughNonMutRef ty => s!"cannot assign through non-mutable reference type {ty}"
  | .returnTypeMismatch expected got => s!"return type mismatch: expected {expected}, got {got}"
  | .copyDestroyConflict typeName => s!"type '{typeName}' implements Destroy and cannot be Copy"
  | .copyFieldNotCopy structName fieldName => s!"Copy struct '{structName}' contains non-copy field '{fieldName}'"
  | .reprCHasGenerics structName => s!"#[repr(C)] struct '{structName}' cannot have type parameters"
  | .reprCFieldNotFFISafe structName fieldName fieldTy => s!"#[repr(C)] struct '{structName}' has non-FFI-safe field '{fieldName}' of type {fieldTy}"
  | .externFnParamNotFFISafe fnName paramName paramTy => s!"extern fn '{fnName}' has non-FFI-safe parameter '{paramName}' of type {paramTy}"
  | .externFnReturnNotFFISafe fnName retTy => s!"extern fn '{fnName}' has non-FFI-safe return type {retTy}"
  | .reprPackedAndAlignConflict structName => s!"struct '{structName}' cannot have both #[repr(packed)] and #[repr(align(...))]"
  | .reprAlignNotPowerOfTwo structName n => s!"#[repr(align({n}))] on struct '{structName}' must be a power of two"
  | .reservedFnName name => s!"'{name}' is a reserved identifier"
  | .builtinTraitRedeclared => "'Destroy' is a built-in trait"
  | .unknownTrait traitName => s!"unknown trait '{traitName}'"
  | .missingTraitMethod typeName methodName => s!"trait impl for '{typeName}' is missing method '{methodName}'"
  | .traitMethodRetTyMismatch methodName expectedRetTy actualRetTy => s!"method '{methodName}' signature does not match trait definition: expected return type {expectedRetTy}, got {actualRetTy}"

private def getEnv : StateM CoreCheckEnv CoreCheckEnv := get
private def setEnv (env : CoreCheckEnv) : StateM CoreCheckEnv Unit := set env

def CoreCheckError.hint : CoreCheckError → Option String
  | .breakOutsideLoop => some "break can only be used inside while or for loops"
  | .continueOutsideLoop => some "continue can only be used inside while or for loops"
  | .copyDestroyConflict _ => some "remove the Destroy impl or remove #[copy]"
  | .copyFieldNotCopy _ _ => some "mark the field type as #[copy] or remove #[copy] from the struct"
  | .insufficientCapabilities _ required _ => some s!"add 'with({required})' to the calling function, or wrap the call in a trusted function"
  | .missingCapability callee cap _ =>
    match callee with
    | "ptr_arith" | "*raw_ptr" | "*raw_ptr=" | "unsafe_cast" =>
      some "add 'with(Unsafe)' to the enclosing function, or wrap in a trusted function"
    | "*heap_ptr" | "alloc" =>
      some s!"add 'with({cap})' to the enclosing function"
    | _ => some s!"add 'with({cap})' to the enclosing function, or wrap the call in a trusted function"
  | _ => none

private def capSetToString : CapSet → String
  | .empty => "(none)"
  | .concrete caps => if caps.isEmpty then "(none)" else String.intercalate ", " caps
  | .var name => name
  | .union a b => s!"{capSetToString a} + {capSetToString b}"

private def addError (msg : String) (hint : Option String := none) : StateM CoreCheckEnv Unit := do
  let env ← getEnv
  setEnv { env with errors := env.errors ++ [{ severity := .error, message := msg, pass := "core-check", span := none, hint := hint }] }

private def addCCError (e : CoreCheckError) : StateM CoreCheckEnv Unit :=
  addError e.message e.hint

private def addVar (name : String) (ty : Ty) : StateM CoreCheckEnv Unit := do
  let env ← getEnv
  setEnv { env with vars := env.vars ++ [(name, ty)] }

private def lookupVar (name : String) : StateM CoreCheckEnv (Option Ty) := do
  let env ← getEnv
  return env.vars.lookup name

/-- Look up the capability required by a builtin via IntrinsicId. -/
private def lookupBuiltinCap (name : String) : Option CapSet :=
  match resolveIntrinsic name with
  | some id => match id.capability with
    | some cap => some (.concrete [cap])
    | none => none
  | none => none

private def lookupFnCaps (name : String) : StateM CoreCheckEnv (Option CapSet) := do
  let env ← getEnv
  match env.fnSigs.find? fun (n, _, _, _) => n == name with
  | some (_, caps, _, _) => return some caps
  | none =>
    -- Fall back to intrinsic capability lookup
    match lookupBuiltinCap name with
    | some caps => return some caps
    | none => return none

private def lookupFnSig (name : String) : StateM CoreCheckEnv (Option (List (String × Ty) × Ty)) := do
  let env ← getEnv
  match env.fnSigs.find? fun (n, _, _, _) => n == name with
  | some (_, _, params, retTy) => return some (params, retTy)
  | none => return none

private def lookupStruct (name : String) : StateM CoreCheckEnv (Option CStructDef) := do
  let env ← getEnv
  return env.structDefs.find? fun sd => sd.name == name

private def lookupEnum (name : String) : StateM CoreCheckEnv (Option CEnumDef) := do
  let env ← getEnv
  return env.enumDefs.find? fun ed => ed.name == name


-- ============================================================
-- Expression validation
-- ============================================================

mutual

partial def ccCheckExpr (e : CExpr) : StateM CoreCheckEnv Unit := do
  match e with
  | .intLit _ _ | .floatLit _ _ | .boolLit _ | .strLit _ | .charLit _ => pure ()

  | .ident name ty =>
    match ← lookupVar name with
    | some varTy =>
      -- Skip check when types involve named/generic/typeVar or ref/deref differences
      -- (match arms rebind names with different types; auto-deref also causes ref vs value differences)
      let isLenient := fun (t : Ty) => match t with
        | .named _ | .generic _ _ | .typeVar _ => true
        | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner => match inner with
          | .named _ | .generic _ _ | .typeVar _ => true | _ => false
        | _ => false
      let isRefCompat := match varTy, ty with
        | .ref inner, t | t, .ref inner => typesCompatible inner t
        | .refMut inner, t | t, .refMut inner => typesCompatible inner t
        | _, _ => false
      if !typesCompatible varTy ty && !isLenient varTy && !isLenient ty && !isRefCompat then
        addCCError (.typeMismatchVariable name (toString (repr varTy)) (toString (repr ty)))
    | none => pure ()  -- may be a parameter or external

  | .binOp op lhs rhs ty =>
    ccCheckExpr lhs
    ccCheckExpr rhs
    let lTy := lhs.ty
    let rTy := rhs.ty
    match op with
    | .add | .sub | .mul | .div | .mod =>
      let isPtr := fun (t : Ty) => match t with | .ptrMut _ | .ptrConst _ => true | _ => false
      let isPtrArith := isPtr lTy && isInteger rTy
      if isPtrArith then
        -- Pointer arithmetic requires trusted or Unsafe
        let env ← getEnv
        if !env.inTrusted && !capsContain env.currentCapSet (.concrete ["Unsafe"]) then
          addCCError (.missingCapability "ptr_arith" "Unsafe" "")
      else
        let hasTypeVar := fun (t : Ty) => match t with | .typeVar _ | .named _ => true | _ => false
        if !isNumeric lTy && !hasTypeVar lTy then
          addCCError (.arithmeticOnNonNumeric (toString (repr lTy)))
        if !typesCompatible lTy rTy && !hasTypeVar lTy && !hasTypeVar rTy then
          addCCError (.binaryOperandMismatch (toString (repr lTy)) (toString (repr rTy)))
    | .eq | .neq | .lt | .gt | .leq | .geq =>
      if !typesCompatible lTy rTy then
        addCCError (.comparisonOperandMismatch (toString (repr lTy)) (toString (repr rTy)))
      if ty != .bool then
        addCCError (.comparisonResultNotBool (toString (repr ty)))
    | .and_ | .or_ =>
      if lTy != .bool || rTy != .bool then
        addCCError (.logicalOnNonBool (toString (repr lTy)) (toString (repr rTy)))
    | .bitand | .bitor | .bitxor | .shl | .shr =>
      if !isInteger lTy then
        addCCError (.bitwiseOnNonInteger (toString (repr lTy)))

  | .unaryOp op operand _ty =>
    ccCheckExpr operand
    match op with
    | .neg =>
      if !isNumeric operand.ty then
        addCCError (.negationOnNonNumeric (toString (repr operand.ty)))
    | .not_ =>
      if operand.ty != .bool then
        addCCError (.logicalNotOnNonBool (toString (repr operand.ty)))
    | .bitnot =>
      if !isInteger operand.ty then
        addCCError (.bitwiseNotOnNonInteger (toString (repr operand.ty)))

  | .call fn _typeArgs args _ty =>
    -- Check capability discipline
    match ← lookupFnCaps fn with
    | some calleeCaps =>
      let env ← getEnv
      if !capsContain env.currentCapSet calleeCaps then
        addCCError (.insufficientCapabilities fn (capSetToString calleeCaps) (capSetToString env.currentCapSet))
    | none => pure ()  -- builtin or extern, skip cap check
    -- Check argument types
    match ← lookupFnSig fn with
    | some (params, _retTy) =>
      if args.length != params.length then
        addCCError (.argCountMismatch fn params.length args.length)
    | none => pure ()
    for arg in args do
      ccCheckExpr arg

  | .structLit _name _typeArgs fields _ty =>
    for (_, fieldExpr) in fields do
      ccCheckExpr fieldExpr

  | .fieldAccess obj _field _ty =>
    ccCheckExpr obj

  | .enumLit _enumName _variant _typeArgs fields _ty =>
    for (_, fieldExpr) in fields do
      ccCheckExpr fieldExpr

  | .match_ scrutinee arms _ty =>
    ccCheckExpr scrutinee
    -- Check match arm coverage for enums
    -- Auto-deref through references (match on &T should check T)
    let scrTy := match scrutinee.ty with
      | .ref t => t | .refMut t => t | t => t
    let tyName := match scrTy with | .named n => some n | .generic n _ => some n | _ => none
    let hasWildcard := arms.any fun arm =>
      match arm with | .varArm _ _ _ => true | _ => false
    match tyName with
    | some name =>
      match ← lookupEnum name with
      | some ed =>
        let variantNames := ed.variants.map fun (vn, _) => vn
        let mut seenVariants : List String := []
        for arm in arms do
          match arm with
          | .enumArm armEnum variant bindings _ =>
            -- Check arm references the right enum
            if armEnum != name then
              addCCError (.matchArmWrongEnum armEnum name)
            -- Check for duplicate arms
            if seenVariants.contains variant then
              addCCError (.duplicateMatchArm variant)
            seenVariants := seenVariants ++ [variant]
            -- Check field count matches variant
            match ed.variants.find? fun (vn, _) => vn == variant with
            | some (_, vfields) =>
              if bindings.length != 0 && bindings.length != vfields.length then
                addCCError (.variantFieldCountMismatch variant vfields.length bindings.length)
            | none => pure ()
          | _ => pure ()
        if !hasWildcard then
          for vn in variantNames do
            if !seenVariants.contains vn then
              addCCError (.matchMissingVariant name vn)
      | none =>
        -- Named type but not an enum: require default arm
        if !hasWildcard then
          addCCError .matchNonEnumNoDefault
    | none =>
      -- Non-named type (Int, Bool, etc.): require default arm unless Bool is fully covered
      if !hasWildcard then
        let boolExhaustive := scrTy == .bool &&
          (arms.any fun a => match a with | .litArm (.boolLit true) _ => true | _ => false) &&
          (arms.any fun a => match a with | .litArm (.boolLit false) _ => true | _ => false)
        if !boolExhaustive then
          addCCError .matchNonEnumNoDefault
    for arm in arms do
      ccCheckMatchArm arm

  | .borrow inner _ => ccCheckExpr inner
  | .borrowMut inner _ => ccCheckExpr inner
  | .deref inner _ =>
    ccCheckExpr inner
    -- Check that inner is a dereferenceable type
    let env ← getEnv
    match inner.ty with
    | .ref _ | .refMut _ => pure ()
    | .ptrMut _ | .ptrConst _ =>
      if !env.inTrusted && !capsContain env.currentCapSet (.concrete ["Unsafe"]) then
        addCCError (.missingCapability "*raw_ptr" "Unsafe" "")
    | .heap _ =>
      if !capsContain env.currentCapSet (.concrete ["Alloc"]) then
        addCCError (.missingCapability "*heap_ptr" "Alloc" "")
    | _ => addCCError (.cannotDerefNonRef (toString (repr inner.ty)))
  | .arrayLit elems _ =>
    if elems.isEmpty then
      addCCError .arrayLiteralEmpty
    for elem in elems do ccCheckExpr elem
  | .arrayIndex arr index _ =>
    ccCheckExpr arr
    ccCheckExpr index
    if !isInteger index.ty then
      addCCError (.arrayIndexNotInteger (toString (repr index.ty)))
    match arr.ty with
    | .array _ _ => pure ()
    | _ => addCCError (.indexingNonArray (toString (repr arr.ty)))
  | .cast inner targetTy =>
    ccCheckExpr inner
    let innerTy := inner.ty
    let isPtr := fun (t : Ty) => match t with | .ptrMut _ | .ptrConst _ => true | _ => false
    let isRef := fun (t : Ty) => match t with | .ref _ | .refMut _ => true | _ => false
    let isFloat := fun (t : Ty) => match t with | .float32 | .float64 => true | _ => false
    -- Cast validity check
    let valid :=
      (isInteger innerTy && isInteger targetTy) ||
      (isInteger innerTy && targetTy == .bool) ||
      (innerTy == .bool && isInteger targetTy) ||
      (isInteger innerTy && isFloat targetTy) ||
      (isFloat innerTy && isInteger targetTy) ||
      (isFloat innerTy && isFloat targetTy) ||
      (isInteger innerTy && targetTy == .char) ||
      (innerTy == .char && isInteger targetTy) ||
      (isPtr innerTy && isPtr targetTy) ||
      (isPtr innerTy && isInteger targetTy) ||
      (isInteger innerTy && isPtr targetTy) ||
      (match innerTy with | .array _ _ => isPtr targetTy | _ => false) ||
      (isPtr innerTy && isRef targetTy) ||
      (isRef innerTy && isPtr targetTy) ||
      (innerTy == targetTy)
    -- Skip cast validation for type variables / named generic params
    let hasTypeVar := fun (t : Ty) => match t with | .typeVar _ | .named _ => true | _ => false
    if !valid && !hasTypeVar innerTy && !hasTypeVar targetTy then
      addCCError (.cannotCast (toString (repr innerTy)) (toString (repr targetTy)))
    -- Unsafe capability check for pointer-involving casts (except safe ref-to-ptr)
    let isRefToPtr := isRef innerTy && isPtr targetTy
    let involvesPointer := isPtr innerTy || isPtr targetTy
    if involvesPointer && !isRefToPtr then
      let env ← getEnv
      if !env.inTrusted && !capsContain env.currentCapSet (.concrete ["Unsafe"]) then
        addCCError (.missingCapability "unsafe_cast" "Unsafe" "")
  | .fnRef _ _ => pure ()
  | .try_ inner _ => ccCheckExpr inner
  | .allocCall inner allocExpr _ =>
    -- Verify caller has Alloc capability
    let env ← getEnv
    if !capsContain env.currentCapSet (.concrete ["Alloc"]) then
      addCCError (.missingCapability "alloc" "Alloc" "")
    ccCheckExpr inner
    ccCheckExpr allocExpr
  | .whileExpr cond body elseBody _ =>
    ccCheckExpr cond
    if cond.ty != .bool && !isInteger cond.ty then
      addCCError (.whileCondNotBool (toString (repr cond.ty)))
    let env ← getEnv
    setEnv { env with inLoop := true }
    for s in body do ccCheckStmt s
    for s in elseBody do ccCheckStmt s
    let env' ← getEnv
    setEnv { env' with inLoop := env.inLoop }
  | .ifExpr cond then_ else_ _ =>
    ccCheckExpr cond
    if cond.ty != .bool && !isInteger cond.ty then
      addCCError (.whileCondNotBool (toString (repr cond.ty)))
    for s in then_ do ccCheckStmt s
    for s in else_ do ccCheckStmt s

partial def ccCheckMatchArm (arm : CMatchArm) : StateM CoreCheckEnv Unit := do
  match arm with
  | .enumArm _ _ bindings body =>
    for (bname, bty) in bindings do
      addVar bname bty
    for s in body do ccCheckStmt s
  | .litArm value body =>
    ccCheckExpr value
    for s in body do ccCheckStmt s
  | .varArm binding bindTy body =>
    addVar binding bindTy
    for s in body do ccCheckStmt s

partial def ccCheckStmt (stmt : CStmt) : StateM CoreCheckEnv Unit := do
  match stmt with
  | .letDecl name _mutable ty value =>
    ccCheckExpr value
    addVar name ty

  | .assign _name value =>
    ccCheckExpr value

  | .return_ (some value) _retTy =>
    ccCheckExpr value
    let env ← getEnv
    let valueTy := value.ty
    -- Skip check for types containing named/generic/typeVar (could be newtypes, aliases, or polymorphic)
    let rec containsResolvable : Ty → Bool
      | .named _ | .generic _ _ | .typeVar _ | .unit | .placeholder => true
      | .ptrMut inner | .ptrConst inner | .ref inner | .refMut inner => containsResolvable inner
      | .array inner _ => containsResolvable inner
      | _ => false
    if !typesCompatible valueTy env.currentRetTy && !containsResolvable valueTy && !containsResolvable env.currentRetTy then
      addCCError (.returnTypeMismatch (toString (repr env.currentRetTy)) (toString (repr valueTy)))

  | .return_ none _ => pure ()

  | .expr e => ccCheckExpr e

  | .ifElse cond then_ else_ =>
    ccCheckExpr cond
    if cond.ty != .bool && !isInteger cond.ty then
      addCCError (.ifCondNotBool (toString (repr cond.ty)))
    for s in then_ do ccCheckStmt s
    match else_ with
    | some stmts => for s in stmts do ccCheckStmt s
    | none => pure ()

  | .while_ cond body _label _ =>
    ccCheckExpr cond
    if cond.ty != .bool && !isInteger cond.ty then
      addCCError (.whileCondNotBool (toString (repr cond.ty)))
    let env ← getEnv
    setEnv { env with inLoop := true }
    for s in body do ccCheckStmt s
    let env' ← getEnv
    setEnv { env' with inLoop := env.inLoop }

  | .fieldAssign obj _field value =>
    ccCheckExpr obj
    ccCheckExpr value

  | .derefAssign target value =>
    ccCheckExpr target
    ccCheckExpr value
    -- Check target is a mutable ref/pointer
    let env ← getEnv
    match target.ty with
    | .refMut _ => pure ()
    | .ptrMut _ =>
      if !env.inTrusted && !capsContain env.currentCapSet (.concrete ["Unsafe"]) then
        addCCError (.missingCapability "*raw_ptr=" "Unsafe" "")
    | _ => addCCError (.cannotAssignThroughNonMutRef (toString (repr target.ty)))

  | .arrayIndexAssign arr index value =>
    ccCheckExpr arr
    ccCheckExpr index
    ccCheckExpr value
    if !isInteger index.ty then
      addCCError (.arrayIndexNotInteger (toString (repr index.ty)))
    match arr.ty with
    | .array _ _ => pure ()
    | _ => addCCError (.indexingNonArray (toString (repr arr.ty)))

  | .break_ _value _label =>
    let env ← getEnv
    if !env.inLoop then
      addCCError .breakOutsideLoop

  | .continue_ _label =>
    let env ← getEnv
    if !env.inLoop then
      addCCError .continueOutsideLoop

  | .defer body => ccCheckExpr body

  | .borrowIn _var _ref _region _isMut _refTy body =>
    for s in body do ccCheckStmt s

end

-- ============================================================
-- Module-level declaration validation (moved from Check)
-- ============================================================

private def tyToString : Ty → String
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
  | .generic name args => name ++ "<" ++ ", ".intercalate (args.map tyToString) ++ ">"
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

private def isCopyTy (allStructs : List CStructDef) (allEnums : List CEnumDef) (ty : Ty) : Bool :=
  match ty with
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | .bool | .float64 | .float32 | .char | .unit => true
  | .ref _ | .ptrMut _ | .ptrConst _ | .never => true
  | .fn_ _ _ _ => true
  | .named name =>
    match allStructs.find? fun sd => sd.name == name with
    | some sd => sd.isCopy
    | none => match allEnums.find? fun ed => ed.name == name with
      | some ed => ed.isCopy
      | none => false
  | _ => false

private def mkDeclDiag (e : CoreCheckError) : Diagnostic :=
  { severity := .error, message := e.message, pass := "core-check", span := none, hint := e.hint }

def ccCheckModuleDecls (m : CModule)
    (allStructs : List CStructDef) (allEnums : List CEnumDef) : Diagnostics :=
  Id.run do
  let mut errors : Diagnostics := []
  let lctx : Layout.Ctx := { structDefs := allStructs, enumDefs := allEnums }
  -- 1. Copy/Destroy conflict + Copy field check for structs
  for sd in m.structs do
    if sd.isCopy then
      if m.traitImpls.any fun ti => ti.builtinTraitId == some .destroy && ti.typeName == sd.name then
        errors := errors ++ [mkDeclDiag (.copyDestroyConflict sd.name)]
      for (fname, fty) in sd.fields do
        if !isCopyTy allStructs allEnums fty then
          errors := errors ++ [mkDeclDiag (.copyFieldNotCopy sd.name fname)]
  -- 2. Copy/Destroy conflict for enums
  for ed in m.enums do
    if ed.isCopy then
      if m.traitImpls.any fun ti => ti.builtinTraitId == some .destroy && ti.typeName == ed.name then
        errors := errors ++ [mkDeclDiag (.copyDestroyConflict ed.name)]
  -- 3. repr(C) validation
  for sd in m.structs do
    if sd.isReprC then
      if !sd.typeParams.isEmpty then
        errors := errors ++ [mkDeclDiag (.reprCHasGenerics sd.name)]
      for (fname, fty) in sd.fields do
        if !Layout.isFFISafe lctx fty then
          errors := errors ++ [mkDeclDiag (.reprCFieldNotFFISafe sd.name fname (tyToString fty))]
  -- 4. repr(packed) + repr(align) conflict
  for sd in m.structs do
    if sd.isPacked && sd.reprAlign.isSome then
      errors := errors ++ [mkDeclDiag (.reprPackedAndAlignConflict sd.name)]
    match sd.reprAlign with
    | some n =>
      if n == 0 || (n &&& (n - 1)) != 0 then
        errors := errors ++ [mkDeclDiag (.reprAlignNotPowerOfTwo sd.name n)]
    | none => ()
  -- 5. Extern fn FFI safety
  for (efName, efParams, efRetTy, _) in m.externFns do
    for (pName, pTy) in efParams do
      if !Layout.isFFISafe lctx pTy then
        errors := errors ++ [mkDeclDiag (.externFnParamNotFFISafe efName pName (tyToString pTy))]
    if !Layout.isFFISafe lctx efRetTy && efRetTy != .unit then
      errors := errors ++ [mkDeclDiag (.externFnReturnNotFFISafe efName (tyToString efRetTy))]
  -- 6. Builtin trait redeclaration — name collision check at validation boundary
  --    (user-defined traits have builtinId = none; the name check detects collisions)
  for td in m.traitDefs do
    if td.name == destroyTraitName then
      errors := errors ++ [mkDeclDiag .builtinTraitRedeclared]
  -- 7. Reserved function names
  for f in m.functions do
    if isReservedFnName f.name then
      errors := errors ++ [mkDeclDiag (.reservedFnName f.name)]
  -- 8. Trait impl validation
  let builtinDestroyTrait : CTraitDef := { name := destroyTraitName, methods := [{ name := destroyMethodName, retTy := .unit }], builtinId := some .destroy }
  let allTraits := builtinDestroyTrait :: m.traitDefs
  for ti in m.traitImpls do
    match allTraits.find? fun td => td.name == ti.traitName with
    | none => errors := errors ++ [mkDeclDiag (.unknownTrait ti.traitName)]
    | some td =>
      for sig in td.methods do
        match ti.methodRetTys.find? fun (mn, _) => mn == sig.name with
        | none => errors := errors ++ [mkDeclDiag (.missingTraitMethod ti.typeName sig.name)]
        | some (_, actualRetTy) =>
          let implTy := tyFromName ti.typeName
          let expectedRetTy := resolveSelfTy sig.retTy implTy
          if expectedRetTy != actualRetTy then
            errors := errors ++ [mkDeclDiag (.traitMethodRetTyMismatch sig.name (tyToString expectedRetTy) (tyToString actualRetTy))]
  errors

-- ============================================================
-- Function and module validation
-- ============================================================

def ccCheckFn (f : CFnDef) : StateM CoreCheckEnv Unit := do
  let env ← getEnv
  setEnv { env with
    vars := f.params
    currentCapSet := f.capSet
    currentRetTy := f.retTy
    inLoop := false
    inTrusted := f.isTrusted
  }
  for s in f.body do
    ccCheckStmt s

private partial def collectAllStructs (m : CModule) : List CStructDef :=
  m.structs ++ m.submodules.foldl (fun acc s => acc ++ collectAllStructs s) []

private partial def collectAllEnums (m : CModule) : List CEnumDef :=
  m.enums ++ m.submodules.foldl (fun acc s => acc ++ collectAllEnums s) []

partial def ccCheckModule (m : CModule)
    (allStructs : List CStructDef) (allEnums : List CEnumDef) : Diagnostics :=
  let declErrors := ccCheckModuleDecls m allStructs allEnums
  let fnSigs := m.functions.map fun f =>
    (f.name, f.capSet, f.params, f.retTy)
  -- Extern functions: trusted ones need no cap, others require Unsafe
  let externSigs := m.externFns.map fun (name, params, retTy, isTrusted) =>
    let cap := if isTrusted then CapSet.empty else CapSet.concrete ["Unsafe"]
    (name, cap, params, retTy)
  let initEnv : CoreCheckEnv := {
    fnSigs := fnSigs ++ externSigs
    structDefs := m.structs
    enumDefs := m.enums
    vars := []
    currentCapSet := .empty
    currentRetTy := .unit
    inLoop := false
    inTrusted := false
    errors := []
  }
  let finalEnv := m.functions.foldl (fun env f =>
    let ((), env') := (ccCheckFn f).run env
    env'
  ) initEnv
  let subErrors := m.submodules.foldl (fun acc sub =>
    acc ++ (ccCheckModule sub allStructs allEnums).map fun d =>
      { d with message := s!"[{sub.name}] {d.message}" }
  ) ([] : Diagnostics)
  declErrors ++ finalEnv.errors ++ subErrors

/-- Validate all Core modules. Returns the first error or Ok. -/
def coreCheckProgram (modules : List CModule) : Except Diagnostics Unit :=
  let allStructs := modules.foldl (fun acc m => acc ++ collectAllStructs m) []
  let allEnums := modules.foldl (fun acc m => acc ++ collectAllEnums m) []
  let allErrors := modules.foldl (fun acc m =>
    acc ++ (ccCheckModule m allStructs allEnums).map fun d => { d with message := s!"[{m.name}] {d.message}" }
  ) ([] : Diagnostics)
  if allErrors.isEmpty then .ok ()
  else .error allErrors

end Concrete
