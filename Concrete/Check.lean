import Concrete.AST
import Concrete.BuiltinSigs
import Concrete.Diagnostic
import Concrete.FileSummary
import Concrete.Intrinsic
import Concrete.Resolve
import Concrete.Shared

namespace Concrete

/-! ## Type Checker with Linear Variable Tracking

Pipeline: Source → Parse → Resolve → **Check** → Elab → CoreCheck → Mono → Lower → SSAVerify → EmitSSA → clang

Linearity rules (matching Concrete/Rust design):
- Primitives (Int, Bool, Uint, Float64, i32, etc.) are implicitly Copy.
- Struct-typed variables are linear by default.
- A linear variable must be consumed exactly once before scope exit.
- Consuming = passing as a function argument (by value).
- Field access does NOT consume the struct.
- Double-consume (use after move) is an error.
- Unconsumed linear variable at scope exit is an error.
- In if/else: both branches must agree on consumption state.
- Cannot consume a linear variable declared outside a loop from inside a loop.
-/

-- ============================================================
-- Types and Environment
-- ============================================================

inductive VarState where
  | unconsumed  -- never touched
  | used        -- read/borrowed but not moved
  | consumed    -- moved by value
  | reserved    -- reserved by defer (cannot be moved, can be read)
  | frozen      -- frozen by borrow block (cannot be used at all)
  deriving Repr, BEq


structure VarInfo where
  ty : Ty
  state : VarState
  isCopy : Bool
  loopDepth : Nat
  borrowCount : Nat := 0
  borrowedFrom : Option String := none
  mutable : Bool := true  -- whether the variable was declared with mut
  deriving Repr

structure TypeEnv where
  vars : List (String × VarInfo)
  structs : List StructDef
  enums : List EnumDef
  functions : List FnSummary
  fnNames : List (String × Nat)
  loopDepth : Nat
  currentRetTy : Ty := .unit
  typeAliases : List (String × Ty) := []
  constants : List (String × Ty) := []
  currentTypeParams : List String := []  -- active function's type params
  currentCapSet : CapSet := .empty       -- current function's capability set
  currentFnName : String := ""           -- current function name (for error messages)
  allFnSummarys : List (String × FnSummary) := []  -- all function signatures for fnRef resolution
  borrowRefs : List String := []          -- names of refs created by borrow blocks (for escape analysis)
  loopBreakTy : Option Ty := none         -- collects type from break-with-value in while-as-expression
  currentImplType : Option Ty := none     -- the Self type when inside an impl block
  loopLabels : List String := []          -- stack of active loop labels
  traitImpls : List (String × String) := []  -- (typeName, traitName) pairs for bound checking
  traits : List TraitDef := []             -- all trait definitions (for method lookup on type vars)
  currentTypeBounds : List (String × List String) := []  -- current function's type param bounds
  newtypes : List NewtypeDef := []         -- all newtype definitions
  userFnNames : List String := []          -- names of user-defined, imported, and extern functions (NOT builtins)
  deriving Repr

abbrev CheckM := ExceptT Diagnostics (StateM TypeEnv)

inductive CheckError where
  -- Slice 1: Name/variable/linearity
  | selfOutsideImpl
  | undeclaredVariable (name : String)
  | assignToUndeclaredVariable (name : String)
  | variableFrozenByBorrow (name : String)
  | cannotMoveLinearBorrowed (name : String)
  | variableUsedAfterMove (name : String)
  | variableReservedByDefer (name : String)
  | cannotConsumeLinearInLoop (name : String)
  | linearVariableNeverConsumed (name : String)
  | matchConsumptionDisagreement (name : String)
  | breakSkipsUnconsumedLinear (name : String)
  | continueSkipsUnconsumedLinear (name : String)
  | linearConsumedOneBranchNotOther (name : String)
  | linearConsumedNoBranch (name : String) (ctx : String)
  | borrowRefShadows (ref : String)
  | borrowRegionShadows (region : String)
  | unknownLoopLabel (label : String)
  | assignToImmutable (name : String)
  | assignToFrozen (name : String)
  | assignOverwritesLinear (name : String)
  -- Slice 2: Type mismatch / operator
  | typeMismatch (ctx : String) (expected : String) (actual : String)
  | cannotDerefNonRef
  | whileBreakTypeMismatch (breakTy : String) (elseTy : String)
  | ifCondNotBool (ty : String)
  | ifBranchTypeMismatch (thenTy : String) (elseTy : String)
  | matchArmTypeMismatch (firstTy : String) (armTy : String)
  | breakTypeMismatch (valTy : String) (prevTy : String)
  -- arrayLiteralEmpty, cannotAssignThroughNonMutRef moved to CoreCheck
  -- Slice 3: Borrow/escape/freeze
  | cannotBorrowMoved (name : String)
  | cannotBorrowMutablyBorrowed (name : String)
  | cannotMutBorrowAlreadyBorrowed (name : String)
  | cannotMutBorrowImmutable (name : String)
  | referenceEscapesBorrowBlock (name : String)
  | cannotMutBorrowImmBorrowed (name : String)
  -- Slice 4: Capability (only cap-poly resolution, simple checks in CoreCheck)
  | missingCapability (callee : String) (cap : String) (caller : String)
  | traitBoundNotSatisfied (typeName : String) (traitName : String) (context : String)
  | cannotInferCapVariable (cap : String) (fnName : String)
  -- Slice 5: Struct/enum/field + function calls
  | unknownStructType (name : String)
  | structHasNoField (structName : String) (fieldName : String)
  | missingFieldInLiteral (fieldName : String) (containerDesc : String)
  | unknownFieldInLiteral (fieldName : String) (containerDesc : String)
  | fieldAccessNonStruct
  | heapAccessRequired (field : String) (ty : String)
  | arrowAccessNotHeap (ty : String)
  | arrowAccessNonStruct
  | arrowAssignNotHeap (ty : String)
  | arrowAssignNonStruct
  | unknownVariant (variant : String) (enumName : String)
  | unknownEnumType (name : String)
  | wrongArgCount (calleeDesc : String) (expected : Nat) (actual : Nat)
  | undeclaredFunction (name : String)
  | noMethodOnType (method : String) (typeName : String)
  | noMethodOnTypeVar (method : String) (typeVar : String)
  | methodCallOnNonNamedType
  | unknownFunctionRef (name : String)
  | builtinWrongArgCount (fnName : String) (expected : Nat)
  | builtinWrongTypeArgCount (fnName : String) (desc : String)
  | builtinWrongFirstArg (fnName : String) (expectedDesc : String) (actualTy : String)
  | builtinBadKeyType (fnName : String) (ty : String)
  | destroyRequiresNamed (ty : String)
  | typeDoesNotImplDestroy (typeName : String)
  | freeRequiresHeap (ty : String)
  | tryRequiresResult
  | tryRequiresOkErrVariants
  | tryOkNoField (enumName : String)
  -- Slice 6: Control flow/defer
  | breakOutsideLoop
  | continueOutsideLoop
  | deferBodyNotCall
  | reservedName (name : String)
  | unknownModule (name : String)
  | notPublicInModule (symbol : String) (moduleName : String)
  -- Slices 7-9: Module-level validation (Copy/Destroy, repr, FFI, traits) moved to CoreCheck

def CheckError.message : CheckError → String
  -- Slice 1
  | .selfOutsideImpl => "Self can only be used inside impl blocks"
  | .undeclaredVariable name => s!"use of undeclared variable '{name}'"
  | .assignToUndeclaredVariable name => s!"assignment to undeclared variable '{name}'"
  | .variableFrozenByBorrow name => s!"variable '{name}' is frozen by borrow block"
  | .cannotMoveLinearBorrowed name => s!"cannot move linear variable '{name}': variable is borrowed"
  | .variableUsedAfterMove name => s!"linear variable '{name}' used after move"
  | .variableReservedByDefer name => s!"variable '{name}' is reserved by defer"
  | .cannotConsumeLinearInLoop name => s!"cannot consume linear variable '{name}' inside a loop (declared outside the loop)"
  | .linearVariableNeverConsumed name => s!"linear variable '{name}' was never consumed"
  | .matchConsumptionDisagreement name => s!"match arms disagree on consumption of '{name}'"
  | .breakSkipsUnconsumedLinear name => s!"break would skip unconsumed linear variable '{name}'"
  | .continueSkipsUnconsumedLinear name => s!"continue would skip unconsumed linear variable '{name}'"
  | .linearConsumedOneBranchNotOther name => s!"linear variable '{name}' consumed in one branch of if/else but not the other"
  | .linearConsumedNoBranch name ctx => s!"linear variable '{name}' consumed in {ctx} then-branch (no else branch to match)"
  | .borrowRefShadows ref => s!"borrow ref '{ref}' shadows existing name"
  | .borrowRegionShadows region => s!"borrow region '{region}' shadows existing name"
  | .unknownLoopLabel label => s!"unknown loop label '{label}'"
  | .assignToImmutable name => s!"cannot assign to immutable variable '{name}'"
  | .assignToFrozen name => s!"cannot assign to '{name}': variable is frozen by borrow block"
  | .assignOverwritesLinear name => s!"cannot reassign linear variable '{name}'"
  -- Slice 2
  | .typeMismatch ctx expected actual => s!"type mismatch in {ctx}: expected {expected}, got {actual}"
  -- arrayIndexNotInteger, indexingNonArray, cannotCast moved to CoreCheck
  | .cannotDerefNonRef => "cannot dereference non-reference type"
  | .whileBreakTypeMismatch breakTy elseTy => s!"while-expression break type '{breakTy}' does not match else type '{elseTy}'"
  | .ifCondNotBool ty => s!"if condition must be Bool, got {ty}"
  | .ifBranchTypeMismatch thenTy elseTy => s!"if-expression then type '{thenTy}' does not match else type '{elseTy}'"
  | .matchArmTypeMismatch firstTy armTy => s!"match arm type '{armTy}' does not match first arm type '{firstTy}'"
  | .breakTypeMismatch valTy prevTy => s!"break value type '{valTy}' does not match previous break type '{prevTy}'"
  -- cannotAssignThroughNonMutRef, arrayLiteralEmpty moved to CoreCheck
  -- Slice 3
  | .cannotBorrowMoved name => s!"cannot borrow '{name}': already moved"
  | .cannotBorrowMutablyBorrowed name => s!"cannot borrow '{name}': already mutably borrowed"
  | .cannotMutBorrowAlreadyBorrowed name => s!"cannot mutably borrow '{name}': already borrowed"
  | .cannotMutBorrowImmutable name => s!"cannot take mutable borrow of immutable variable '{name}'"
  | .referenceEscapesBorrowBlock name => s!"reference '{name}' cannot escape its borrow block"
  | .cannotMutBorrowImmBorrowed name => s!"cannot mutably borrow '{name}': already immutably borrowed"
  -- Slice 4
  | .missingCapability callee cap caller => s!"function '{callee}' requires capability '{cap}' but '{caller}' does not declare it"
  | .traitBoundNotSatisfied typeName traitName context => s!"type '{typeName}' does not implement trait '{traitName}' required by {context}"
  | .cannotInferCapVariable cap fnName => s!"cannot infer capability variable '{cap}' for call to '{fnName}'"
  -- Slice 5
  | .unknownStructType name => s!"unknown struct type '{name}'"
  | .structHasNoField structName fieldName => s!"struct '{structName}' has no field '{fieldName}'"
  | .missingFieldInLiteral fieldName containerDesc => s!"missing field '{fieldName}' in {containerDesc}"
  | .unknownFieldInLiteral fieldName containerDesc => s!"unknown field '{fieldName}' in {containerDesc}"
  | .fieldAccessNonStruct => "field access on non-struct type"
  | .heapAccessRequired field ty => s!"cannot access field '{field}' on {ty} with '.'; use '->' for heap access"
  | .arrowAccessNotHeap ty => s!"arrow access '->' requires Heap<T> or HeapArray<T> type, got {ty}"
  | .arrowAccessNonStruct => "arrow access '->' on non-struct inner type"
  | .arrowAssignNotHeap ty => s!"arrow assign '->' requires Heap<T> type, got {ty}"
  | .arrowAssignNonStruct => "arrow assign on non-struct inner type"
  | .unknownVariant variant enumName => s!"unknown variant '{variant}' in enum '{enumName}'"
  | .unknownEnumType name => s!"unknown enum type '{name}'"
  | .wrongArgCount calleeDesc expected actual => s!"{calleeDesc} expects {expected} arguments, got {actual}"
  | .undeclaredFunction name => s!"call to undeclared function '{name}'"
  | .noMethodOnType method typeName => s!"no method '{method}' on type '{typeName}'"
  | .noMethodOnTypeVar method typeVar => s!"no method '{method}' for type variable '{typeVar}'"
  | .methodCallOnNonNamedType => "method call on non-named type"
  | .unknownFunctionRef name => s!"unknown function '{name}' in function reference"
  | .builtinWrongArgCount fnName expected =>
    if expected == 0 then s!"{fnName}() takes no arguments"
    else if expected == 1 then s!"{fnName}() takes exactly 1 argument"
    else s!"{fnName}() takes exactly {expected} arguments"
  | .builtinWrongTypeArgCount fnName desc => s!"{fnName} requires exactly {desc}"
  | .builtinWrongFirstArg fnName expectedDesc actualTy => s!"{fnName}() requires {expectedDesc}, got {actualTy}"
  | .builtinBadKeyType fnName ty => s!"{fnName}() key type must be Int or String, got {ty}"
  | .destroyRequiresNamed ty => s!"destroy() requires a named type, got {ty}"
  | .typeDoesNotImplDestroy typeName => s!"type '{typeName}' does not implement Destroy"
  | .freeRequiresHeap ty => s!"free() requires Heap<T> type, got {ty}"
  | .tryRequiresResult => "? operator requires a Result enum type"
  | .tryRequiresOkErrVariants => "? operator requires an enum with Ok and Err variants"
  | .tryOkNoField enumName => s!"Ok variant of '{enumName}' has no value field"
  -- Slice 6
  | .breakOutsideLoop => "break outside of loop"
  | .continueOutsideLoop => "continue outside of loop"
  | .deferBodyNotCall => "defer body must be a function call"
  | .reservedName name => s!"'{name}' is a reserved identifier"
  | .unknownModule name => s!"unknown module '{name}'"
  | .notPublicInModule symbol moduleName => s!"'{symbol}' is not public in module '{moduleName}'"

def CheckError.hint : CheckError → Option String
  | .variableUsedAfterMove _ => some "consider cloning or restructuring to avoid the move"
  | .linearVariableNeverConsumed _ => some "pass it to a function, return it, or use destroy()"
  | .assignToImmutable _ => some "declare with 'let mut' to make it mutable"
  | .cannotConsumeLinearInLoop _ => some "move the declaration inside the loop or clone before the loop"
  | .arrowAccessNotHeap _ => some "use '.' for direct field access"
  | .destroyRequiresNamed _ => some "only struct and enum types can be destroyed"
  | .referenceEscapesBorrowBlock _ => some "copy the data before the borrow block ends"
  | .cannotMutBorrowImmutable _ => some "declare with 'let mut' to allow mutable borrowing"
  | .assignToFrozen _ => some "the variable is frozen by an active borrow block"
  | .assignOverwritesLinear _ => some "linear variables cannot be reassigned — use a new binding instead"
  | .missingCapability _ cap caller => some s!"add 'with({cap})' to '{caller}', or wrap the call in a function that declares it"
  | .cannotInferCapVariable cap _ => some s!"provide an explicit capability for '{cap}' at the call site"
  | _ => none

def CheckError.code : CheckError → String
  -- Slice 1: Name/variable/linearity (E0200–E0219)
  | .selfOutsideImpl => "E0200"
  | .undeclaredVariable _ => "E0201"
  | .assignToUndeclaredVariable _ => "E0202"
  | .variableFrozenByBorrow _ => "E0203"
  | .cannotMoveLinearBorrowed _ => "E0204"
  | .variableUsedAfterMove _ => "E0205"
  | .variableReservedByDefer _ => "E0206"
  | .cannotConsumeLinearInLoop _ => "E0207"
  | .linearVariableNeverConsumed _ => "E0208"
  | .matchConsumptionDisagreement _ => "E0209"
  | .breakSkipsUnconsumedLinear _ => "E0210"
  | .continueSkipsUnconsumedLinear _ => "E0211"
  | .linearConsumedOneBranchNotOther _ => "E0212"
  | .linearConsumedNoBranch _ _ => "E0213"
  | .borrowRefShadows _ => "E0214"
  | .borrowRegionShadows _ => "E0215"
  | .unknownLoopLabel _ => "E0216"
  | .assignToImmutable _ => "E0217"
  | .assignToFrozen _ => "E0218"
  | .assignOverwritesLinear _ => "E0219"
  -- Slice 2: Type mismatch (E0220–E0229)
  | .typeMismatch _ _ _ => "E0220"
  | .cannotDerefNonRef => "E0221"
  | .whileBreakTypeMismatch _ _ => "E0222"
  | .ifCondNotBool _ => "E0223"
  | .ifBranchTypeMismatch _ _ => "E0224"
  | .matchArmTypeMismatch _ _ => "E0225"
  | .breakTypeMismatch _ _ => "E0226"
  -- Slice 3: Borrow/escape (E0230–E0239)
  | .cannotBorrowMoved _ => "E0230"
  | .cannotBorrowMutablyBorrowed _ => "E0231"
  | .cannotMutBorrowAlreadyBorrowed _ => "E0232"
  | .cannotMutBorrowImmutable _ => "E0233"
  | .referenceEscapesBorrowBlock _ => "E0234"
  | .cannotMutBorrowImmBorrowed _ => "E0235"
  -- Slice 4: Capability (E0240–E0249)
  | .missingCapability _ _ _ => "E0240"
  | .traitBoundNotSatisfied _ _ _ => "E0241"
  | .cannotInferCapVariable _ _ => "E0242"
  -- Slice 5: Struct/enum/function (E0250–E0279)
  | .unknownStructType _ => "E0250"
  | .structHasNoField _ _ => "E0251"
  | .missingFieldInLiteral _ _ => "E0252"
  | .unknownFieldInLiteral _ _ => "E0253"
  | .fieldAccessNonStruct => "E0254"
  | .heapAccessRequired _ _ => "E0255"
  | .arrowAccessNotHeap _ => "E0256"
  | .arrowAccessNonStruct => "E0257"
  | .arrowAssignNotHeap _ => "E0258"
  | .arrowAssignNonStruct => "E0259"
  | .unknownVariant _ _ => "E0260"
  | .unknownEnumType _ => "E0261"
  | .wrongArgCount _ _ _ => "E0262"
  | .undeclaredFunction _ => "E0263"
  | .noMethodOnType _ _ => "E0264"
  | .noMethodOnTypeVar _ _ => "E0265"
  | .methodCallOnNonNamedType => "E0266"
  | .unknownFunctionRef _ => "E0267"
  | .builtinWrongArgCount _ _ => "E0268"
  | .builtinWrongTypeArgCount _ _ => "E0269"
  | .builtinWrongFirstArg _ _ _ => "E0270"
  | .builtinBadKeyType _ _ => "E0271"
  | .destroyRequiresNamed _ => "E0272"
  | .typeDoesNotImplDestroy _ => "E0273"
  | .freeRequiresHeap _ => "E0274"
  | .tryRequiresResult => "E0275"
  | .tryRequiresOkErrVariants => "E0276"
  | .tryOkNoField _ => "E0277"
  -- Slice 6: Control flow (E0280–E0289)
  | .breakOutsideLoop => "E0280"
  | .continueOutsideLoop => "E0281"
  | .deferBodyNotCall => "E0282"
  | .reservedName _ => "E0283"
  | .unknownModule _ => "E0284"
  | .notPublicInModule _ _ => "E0285"

def throwCheck (e : CheckError) (span : Option Span := none) : CheckM α :=
  throw [{ severity := .error, message := e.message, pass := "check", span := span, hint := e.hint, code := e.code }]

private def throwCheckMsg (msg : String) (span : Option Span := none) : CheckM α :=
  throw [{ severity := .error, message := msg, pass := "check", span := span, hint := none }]

-- ============================================================
-- Helpers
-- ============================================================

private def enumerateList (l : List α) (idx : Nat := 0) : List (Nat × α) :=
  match l with
  | [] => []
  | a :: rest => (idx, a) :: enumerateList rest (idx + 1)

private def listGetIdx (l : List α) (idx : Nat) : Option α :=
  match l, idx with
  | [], _ => none
  | a :: _, 0 => some a
  | _ :: rest, n + 1 => listGetIdx rest n

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

/-- Is this a signed integer type? -/
def isSignedInt : Ty → Bool
  | .int | .i8 | .i16 | .i32 => true
  | _ => false

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

def getEnv : CheckM TypeEnv := get
def setEnv (env : TypeEnv) : CheckM Unit := set env

/-- Resolve type aliases. -/
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
      | some resolved => return resolved
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
  | .generic "Heap" [inner] =>
    let inner' ← resolveType inner
    return .heap inner'
  | .generic "HeapArray" [inner] =>
    let inner' ← resolveType inner
    return .heapArray inner'
  | .generic name args =>
    let args' ← args.mapM resolveType
    return .generic name args'
  | .fn_ params capSet retTy =>
    let params' ← params.mapM resolveType
    let retTy' ← resolveType retTy
    return .fn_ params' capSet retTy'
  | _ => return ty

/-- Is this type Copy (non-linear)? Primitives are Copy; structs are linear. -/
partial def isCopyType (ty : Ty) : CheckM Bool := do
  match ty with
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => return true
  | .bool | .float64 | .float32 | .char | .unit => return true
  | .string => return false    -- String is linear
  | .ref _ => return true      -- References are Copy
  | .refMut _ => return false  -- Mutable refs are not Copy (exclusive)
  | .ptrMut _ | .ptrConst _ => return true  -- Raw pointers are Copy
  | .fn_ _ _ _ => return true  -- Function pointers are Copy (no captures, just a code address)
  | .placeholder => return true
  | .never => return true      -- Never type is compatible with anything
  | .heap _ => return false    -- Heap pointers are linear
  | .heapArray _ => return false
  | .named name =>
    -- Check if the struct/enum has isCopy = true, or newtype wraps a Copy type
    let env ← getEnv
    match env.structs.find? fun sd => sd.name == name with
    | some sd => return sd.isCopy
    | none =>
      match env.enums.find? fun ed => ed.name == name with
      | some ed => return ed.isCopy
      | none =>
        match env.newtypes.find? fun nt => nt.name == name with
        | some nt => isCopyType nt.innerTy
        | none => return false
  | .generic name _args =>
    -- Look up the struct/enum definition — if it's declared Copy, the instantiation is Copy
    let env ← getEnv
    match env.structs.find? fun sd => sd.name == name with
    | some sd => return sd.isCopy
    | none =>
      match env.enums.find? fun ed => ed.name == name with
      | some ed => return ed.isCopy
      | none => return false
  | .typeVar name =>
    -- Check if the type parameter has a Copy bound
    let env ← getEnv
    let bounds := (env.currentTypeBounds.find? fun (n, _) => n == name).map Prod.snd |>.getD []
    return bounds.contains "Copy"
  | .array t _ => isCopyType t  -- Array of copy types is copy

def lookupVarInfo (name : String) : CheckM (Option VarInfo) := do
  let env ← getEnv
  return env.vars.lookup name

def lookupVarTy (name : String) : CheckM (Option Ty) := do
  match ← lookupVarInfo name with
  | some info => return some info.ty
  | none => return none

def addVar (name : String) (ty : Ty) (mutable : Bool := true) : CheckM Unit := do
  let env ← getEnv
  let copy ← isCopyType ty
  let info : VarInfo := { ty, state := .unconsumed, isCopy := copy, loopDepth := env.loopDepth, mutable }
  let env ← getEnv
  setEnv { env with vars := (name, info) :: env.vars }

private def activeBorrowRefs (env : TypeEnv) (varName : String) : List VarInfo :=
  env.vars.foldl (fun acc (_, info) =>
    match info.borrowedFrom with
    | some sourceName =>
      if sourceName == varName && info.state != .consumed then info :: acc else acc
    | none => acc) []

private partial def tyContainsTypeVar : Ty → Bool
  | .typeVar _ => true
  | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner | .heap inner | .heapArray inner =>
    tyContainsTypeVar inner
  | .array elem _ => tyContainsTypeVar elem
  | .generic _ args => args.any tyContainsTypeVar
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

/-- Normalize a type for comparison (normalize empty capsets in fn types). -/
private def normalizeTyForCmp : Ty → Ty
  | .fn_ params capSet retTy =>
    let normCap := match capSet with
      | .concrete [] => .empty
      | .empty => .empty
      | cs => cs
    .fn_ (params.map normalizeTyForCmp) normCap (normalizeTyForCmp retTy)
  | .ref t => .ref (normalizeTyForCmp t)
  | .refMut t => .refMut (normalizeTyForCmp t)
  | .heap t => .heap (normalizeTyForCmp t)
  | .heapArray t => .heapArray (normalizeTyForCmp t)
  | .generic n args => .generic n (args.map normalizeTyForCmp)
  | .array t n => .array (normalizeTyForCmp t) n
  | t => t

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
  if expectedN == actualN then return ()
  -- .string is compatible with .named "String"
  else if (expectedR == .string && actualR == .named "String")
       || (expectedR == .named "String" && actualR == .string) then return ()
  else throwCheck (.typeMismatch ctx (tyToString expected) (tyToString actual)) span


-- ============================================================
-- Linearity: consume and check
-- ============================================================

/-- Mark a linear variable as used (read/borrowed but not moved). -/
def useVar (name : String) (span : Option Span := none) : CheckM Unit := do
  let env ← getEnv
  match env.vars.lookup name with
  | none => pure ()  -- not found (might be a constant or function)
  | some info =>
    if info.state == .frozen then
      throwCheck (.variableFrozenByBorrow name) span
    if info.isCopy then return ()
    if info.state == .unconsumed || info.state == .reserved then
      let vars' := env.vars.map fun (n, vi) =>
        if n == name then (n, { vi with state := if info.state == .reserved then .reserved else .used })
        else (n, vi)
      setEnv { env with vars := vars' }

/-- Consume a linear variable (mark it as consumed).
    Errors on use-after-move, or consuming an outer var inside a loop. -/
def consumeVar (name : String) (span : Option Span := none) : CheckM Unit := do
  let env ← getEnv
  match env.vars.lookup name with
  | none => throwCheck (.undeclaredVariable name) span
  | some info =>
    if info.isCopy then return ()  -- Copy types are never consumed
    let activeRefs := activeBorrowRefs env name
    if activeRefs.any (fun refInfo => match refInfo.ty with | .ref _ | .refMut _ => true | _ => false) then
      throwCheck (.cannotMoveLinearBorrowed name) span
    match info.state with
    | .consumed =>
      throwCheck (.variableUsedAfterMove name) span
    | .reserved =>
      throwCheck (.variableReservedByDefer name) span
    | .frozen =>
      throwCheck (.variableFrozenByBorrow name) span
    | .unconsumed | .used =>
      -- Loop depth check: linear values from outer scope cannot be consumed inside a loop
      if info.loopDepth < env.loopDepth then
        throwCheck (.cannotConsumeLinearInLoop name) span
      -- Mark consumed
      let vars' := env.vars.map fun (n, vi) =>
        if n == name then (n, { vi with state := .consumed })
        else (n, vi)
      setEnv { env with vars := vars' }

/-- Consume a variable if it exists. Skips function names (not in var scope). -/
def consumeVarIfExists (name : String) (span : Option Span := none) : CheckM Unit := do
  match ← lookupVarInfo name with
  | some _ => consumeVar name span
  | none => pure ()  -- function reference, not a variable

/-- Check that all tracked linear variables in the given name list are consumed.
    `reserved` is allowed because the deferred destroy will run at scope exit. -/
def checkScopeExit (varNames : List String) (span : Option Span := none) : CheckM Unit := do
  let env ← getEnv
  for name in varNames do
    match env.vars.lookup name with
    | some info =>
      if !info.isCopy && info.state != .consumed && info.state != .reserved then
        throwCheck (.linearVariableNeverConsumed name) span
    | none => pure ()

-- ============================================================
-- Type substitution for generics
-- ============================================================

/-- Peek at an expression's type without consuming any linear variables. -/
def peekExprType (e : Expr) : CheckM Ty := do
  match e with
  | .intLit _ _ => return .int
  | .floatLit _ _ => return .float64
  | .boolLit _ _ => return .bool
  | .strLit _ _ => return .string
  | .charLit _ _ => return .char
  | .ident _ name =>
    let env ← getEnv
    match env.constants.lookup name with
    | some ty => return ty
    | none =>
    match env.vars.lookup name with
    | some info => return info.ty
    | none =>
      match ← lookupFn name with
      | some sig =>
        let paramTys := sig.params.map fun (_, t) => t
        return .fn_ paramTys sig.capSet sig.retTy
      | none => return .placeholder
  | .structLit _ name typeArgs _ =>
    if typeArgs.isEmpty then return .named name
    else return .generic name typeArgs
  | .enumLit _ enumName _ typeArgs _ =>
    if typeArgs.isEmpty then return .named enumName
    else return .generic enumName typeArgs
  | .fnRef _ name =>
    let env ← getEnv
    match env.allFnSummarys.lookup name with
    | some sig =>
      let paramTys := sig.params.map Prod.snd
      return .fn_ paramTys sig.capSet sig.retTy
    | none => return .placeholder
  | .paren _ inner => peekExprType inner
  | .binOp _ _ lhs _ => peekExprType lhs
  | _ => return .placeholder

/-- Unify a pattern type with an actual type to discover type variable bindings. -/
private partial def unifyTypes (pattern actual : Ty) (typeParams : List String) : List (String × Ty) :=
  match pattern with
  | .named name =>
    if typeParams.contains name then [(name, actual)]
    else []
  | .typeVar name =>
    if typeParams.contains name then [(name, actual)]
    else []
  | .ref inner =>
    match actual with
    | .ref aInner => unifyTypes inner aInner typeParams
    | _ => []
  | .refMut inner =>
    match actual with
    | .refMut aInner => unifyTypes inner aInner typeParams
    | _ => []
  | .fn_ pParams pCapSet pRet =>
    match actual with
    | .fn_ aParams _aCapSet aRet =>
      let paramBindings := (pParams.zip aParams).foldl (fun acc (pp, ap) =>
        acc ++ unifyTypes pp ap typeParams) []
      let retBindings := unifyTypes pRet aRet typeParams
      -- Also try to unify cap set names
      let capBindings := match pCapSet with
        | .concrete _ => []  -- concrete caps don't bind type vars
        | _ => []
      paramBindings ++ retBindings ++ capBindings
    | _ => []
  | .generic _name pArgs =>
    match actual with
    | .generic _aName aArgs =>
      (pArgs.zip aArgs).foldl (fun acc (pp, ap) =>
        acc ++ unifyTypes pp ap typeParams) []
    | _ => []
  | .heap inner =>
    match actual with
    | .heap aInner => unifyTypes inner aInner typeParams
    | _ => []
  | .array elem _ =>
    match actual with
    | .array aElem _ => unifyTypes elem aElem typeParams
    | _ => []
  | _ => []

private def substCapSet (mapping : List (String × Ty)) : CapSet → CapSet
  | .concrete caps =>
    -- Cap variable names that map to types are not relevant here, keep as-is
    .concrete caps
  | .var name => .var name
  | .union a b => .union (substCapSet mapping a) (substCapSet mapping b)
  | .empty => .empty

private def substTy (mapping : List (String × Ty)) : Ty → Ty
  | .named name => match mapping.lookup name with | some t => t | none => .named name
  | .typeVar name => match mapping.lookup name with | some t => t | none => .typeVar name
  | .ref inner => .ref (substTy mapping inner)
  | .refMut inner => .refMut (substTy mapping inner)
  | .ptrMut inner => .ptrMut (substTy mapping inner)
  | .ptrConst inner => .ptrConst (substTy mapping inner)
  | .array elem n => .array (substTy mapping elem) n
  | .generic name args => .generic name (args.map (substTy mapping))
  | .fn_ params capSet retTy =>
    .fn_ (params.map (substTy mapping)) (substCapSet mapping capSet) (substTy mapping retTy)
  | .heap inner => .heap (substTy mapping inner)
  | .heapArray inner => .heapArray (substTy mapping inner)
  | ty => ty

/-- Check trait bounds: for each type param with bounds, verify the concrete type implements the required traits. -/
private def checkTraitBounds (bounds : List (String × List String)) (mapping : List (String × Ty))
    (context : String) : CheckM Unit := do
  let env ← getEnv
  for (paramName, requiredTraits) in bounds do
    match mapping.lookup paramName with
    | some concreteType =>
      let typeName := match concreteType with
        | .named n => some n
        | .generic n _ => some n
        | _ => none
      match typeName with
      | some tn =>
        for traitName in requiredTraits do
          if !(env.traitImpls.any fun (t, tr) => t == tn && tr == traitName) then
            throwCheck (.traitBoundNotSatisfied tn traitName context)
      | none => pure ()  -- primitive types, skip bound checking
    | none => pure ()

-- ============================================================
-- Type checking expressions and statements
-- ============================================================

mutual

partial def checkExpr (e : Expr) (hint : Option Ty := none) : CheckM Ty := do
  match e with
  | .intLit _ _ =>
    -- Use hint to infer integer literal type (resolve aliases first)
    match hint with
    | some ty =>
      let tyR ← resolveType ty
      if isInteger tyR || tyR == .char then return tyR
      else
        match tyR with
        | .typeVar _ => return tyR  -- Type variables accept integer literals
        | _ => return .int
    | none => return .int
  | .floatLit _ _ =>
    match hint with
    | some ty =>
      let tyR ← resolveType ty
      if isFloatType tyR then return tyR else return .float64
    | none => return .float64
  | .boolLit _ _ => return .bool
  | .strLit _ _ => return .string
  | .charLit _ _ => return .char
  | .ident _ name =>
    -- First check if it's a constant
    let env ← getEnv
    match env.constants.lookup name with
    | some ty => return ty
    | none =>
    match ← lookupVarInfo name with
    | some info =>
      -- Reading a variable (not consuming). Check it's not already consumed.
      if !info.isCopy && info.state == .consumed then
        throwCheck (.variableUsedAfterMove name) (some e.getSpan)
      useVar name (some e.getSpan)
      return info.ty
    | none =>
      -- Check if it's a function name (first-class function reference)
      match ← lookupFn name with
      | some sig =>
        let paramTys := sig.params.map fun (_, t) => t
        return .fn_ paramTys sig.capSet sig.retTy
      | none => throwCheck (.undeclaredVariable name) (some e.getSpan)
  | .binOp _ op lhs rhs =>
    -- Check lhs first (with hint), then use its type as hint for rhs
    let lTy ← checkExpr lhs hint
    let lTyR ← resolveType lTy
    let rTy ← checkExpr rhs (some lTyR)
    let rTyR ← resolveType rTy
    let isTypeVarL := match lTyR with | .typeVar _ => true | _ => false
    let isTypeVarR := match rTyR with | .typeVar _ => true | _ => false
    match op with
    | .add | .sub | .mul | .div | .mod =>
      if isInteger lTyR && lTyR == rTyR then return lTy
      else if isFloatType lTyR && lTyR == rTyR then return lTy
      else if lTyR == .char && rTyR == .char then return .char
      else if isPointerType lTyR && isInteger rTyR then return lTy
      else if isTypeVarL || isTypeVarR then return lTy
      else return lTy  -- CoreCheck validates operator type constraints
    | .eq | .neq | .lt | .gt | .leq | .geq =>
      return .bool  -- CoreCheck validates operand type compatibility
    | .and_ | .or_ =>
      return .bool  -- CoreCheck validates logical operator types
    | .bitand | .bitor | .bitxor | .shl | .shr =>
      if isInteger lTyR && lTyR == rTyR then return lTy
      else if isTypeVarL || isTypeVarR then return lTy
      else return lTy  -- CoreCheck validates bitwise operator types
  | .unaryOp _ op operand =>
    let ty ← checkExpr operand hint
    match op with
    | .neg =>
      if isInteger ty || isFloatType ty then return ty
      else return ty  -- CoreCheck validates negation types
    | .not_ =>
      return .bool  -- CoreCheck validates logical not types
    | .bitnot =>
      if isInteger ty then return ty
      else return ty  -- CoreCheck validates bitwise not types
  | .arrowAccess _ obj field =>
    let objTy ← checkExpr obj
    -- obj must be Heap<T> or HeapArray<T>
    let innerTy := match objTy with
      | .heap t => t
      | .heapArray t => t
      | .ref (.heap t) => t
      | .refMut (.heap t) => t
      | _ => .placeholder
    if innerTy == .placeholder then
      throwCheck (.arrowAccessNotHeap (tyToString objTy)) (some e.getSpan)
    -- Look up field on the inner type
    let structName := match innerTy with
      | .named n => n
      | .generic n _ => n
      | _ => ""
    if structName == "" then throwCheck .arrowAccessNonStruct (some e.getSpan)
    match ← lookupStruct structName with
    | some sd =>
      match sd.fields.find? fun f => f.name == field with
      | some f => resolveType f.ty
      | none => throwCheck (.structHasNoField structName field) (some e.getSpan)
    | none => throwCheck (.unknownStructType structName) (some e.getSpan)
  | .allocCall _ inner allocExpr =>
    -- Check the allocator expression is valid
    let _allocTy ← checkExpr allocExpr
    -- Check the inner call expression
    checkExpr inner hint
  | .whileExpr _ cond body elseBody =>
    -- while-as-expression: while cond { body } else { elseBody }
    let _condTy ← checkExpr cond
    -- Save and set up loop context
    let env ← getEnv
    let savedLoopDepth := env.loopDepth
    let savedBreakTy := env.loopBreakTy
    setEnv { env with loopDepth := env.loopDepth + 1, loopBreakTy := none }
    -- Check body
    checkStmts body env.currentRetTy
    -- Get break type if any
    let envAfterBody ← getEnv
    let breakTy := envAfterBody.loopBreakTy
    -- Restore loop depth and break ty
    setEnv { envAfterBody with loopDepth := savedLoopDepth, loopBreakTy := savedBreakTy }
    -- Check else body: all stmts except the last, then check last for its type
    let elseInit := elseBody.dropLast
    checkStmts elseInit env.currentRetTy
    let elseTy ← match elseBody.getLast? with
      | some (.expr _ e) => checkExpr e hint
      | some (.return_ _ v) =>
        match v with
        | some rv => let _ ← checkExpr rv; pure Ty.never
        | none => pure Ty.never
      | some other =>
        checkStmt other env.currentRetTy
        pure Ty.unit
      | none => pure Ty.unit
    -- The result type: if break had a value, verify it matches else type
    match breakTy with
    | some bTy =>
      if bTy != elseTy && elseTy != .never && bTy != .never then
        throwCheck (.whileBreakTypeMismatch (tyToString bTy) (tyToString elseTy)) (some e.getSpan)
      return elseTy
    | none => return elseTy
  | .ifExpr _ cond then_ else_ =>
    -- if-as-expression: if cond { then_ } else { else_ }
    let condTy ← checkExpr cond
    if condTy != .bool then
      throwCheck (.ifCondNotBool (tyToString condTy)) (some e.getSpan)
    let env ← getEnv
    -- Check then branch: all stmts except the last, then check last for its type
    let thenInit := then_.dropLast
    checkStmts thenInit env.currentRetTy
    let thenTy ← match then_.getLast? with
      | some (.expr _ tExpr) => do
        let ty ← checkExpr tExpr hint
        -- Trailing expression is a value move — consume linear variables
        match tExpr with
        | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
        pure ty
      | some (.return_ _ v) =>
        match v with
        | some rv => let _ ← checkExpr rv; pure Ty.never
        | none => pure Ty.never
      | some other =>
        checkStmt other env.currentRetTy
        pure Ty.unit
      | none => pure Ty.unit
    -- Check else branch
    let elseInit := else_.dropLast
    checkStmts elseInit env.currentRetTy
    let elseTy ← match else_.getLast? with
      | some (.expr _ eExpr) => do
        let ty ← checkExpr eExpr hint
        match eExpr with
        | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
        pure ty
      | some (.return_ _ v) =>
        match v with
        | some rv => let _ ← checkExpr rv; pure Ty.never
        | none => pure Ty.never
      | some other =>
        checkStmt other env.currentRetTy
        pure Ty.unit
      | none => pure Ty.unit
    -- Both branches must agree on type
    if thenTy != elseTy && thenTy != .never && elseTy != .never then
      throwCheck (.ifBranchTypeMismatch (tyToString thenTy) (tyToString elseTy)) (some e.getSpan)
    if thenTy == .never then return elseTy else return thenTy
  | .call _sp fnName typeArgs args =>
    -- Intercept newtype wrapping: NewtypeName(expr)
    match ← lookupNewtype fnName with
    | some nt =>
      if args.length != 1 then throwCheckMsg s!"newtype '{fnName}' constructor takes exactly 1 argument"
      if !typeArgs.isEmpty then throwCheckMsg s!"newtype '{fnName}' constructor does not take type arguments"
      -- For generic newtypes, infer type args from hint
      let inferredTypeArgs := if nt.typeParams.isEmpty then []
        else match hint with
          | some (.generic n hintArgs) => if n == fnName then hintArgs else []
          | _ => []
      let mapping := nt.typeParams.zip inferredTypeArgs
      let resolvedInnerTy := substTy mapping nt.innerTy
      let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let argTy ← checkExpr arg (some resolvedInnerTy)
      expectTy resolvedInnerTy argTy s!"newtype '{fnName}' constructor" (some e.getSpan)
      -- Consume linear variables passed to newtype constructor (ownership moves)
      match arg with
      | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
      | _ => pure ()
      if inferredTypeArgs.isEmpty then return .named fnName
      else return .generic fnName inferredTypeArgs
    | none =>
    let env ← getEnv
    let isUserFn := env.userFnNames.contains fnName
    let intrinsic := if isUserFn then none else resolveIntrinsic fnName
    -- Intercept sizeof::<T>() and alignof::<T>() builtins
    if intrinsic == some .sizeof || intrinsic == some .alignof then
      if args.length != 0 then throwCheckMsg s!"{fnName} takes no value arguments"
      if typeArgs.length != 1 then throwCheckMsg s!"{fnName} requires exactly 1 type argument: {fnName}::<T>()"
      return .uint
    -- Intercept unwrap(x) for newtype unwrapping (only if not a user-defined function)
    if intrinsic == some .unwrap && args.length == 1 then
        let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
        let argTy ← checkExpr arg
        let ntName := match argTy with | .named n => n | _ => ""
        if ntName == "" then throwCheckMsg s!"unwrap() requires a newtype argument, got {tyToString argTy}"
        match ← lookupNewtype ntName with
        | some nt =>
          match arg with
          | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
          | _ => pure ()
          return nt.innerTy
        | none => throwCheckMsg s!"unwrap() requires a newtype argument, '{ntName}' is not a newtype"
    -- Intercept print(...) / println(...) — variadic mixed-arg output
    if intrinsic == some .print || intrinsic == some .println then
      if args.isEmpty then throwCheckMsg s!"{fnName}() requires at least 1 argument"
      for arg in args do
        let argTy ← checkExpr arg
        -- Accept: String (auto-borrowed), integer types, bool, char
        match argTy with
        | .string | .ref .string | .refMut .string => pure ()
        | .int | .uint | .i32 | .u32 | .i16 | .u16 | .i8 | .u8 => pure ()
        | .bool => pure ()
        | .char => pure ()
        | _ => throwCheckMsg s!"{fnName}() argument has unsupported type '{tyToString argTy}'; expected String/&String/&mut String, Int/Uint/i8..i32/u8..u32, bool, or char"
      return .unit
    -- Intercept append(&mut buf, ...) — variadic mixed-arg buffer append
    if intrinsic == some .append then
      match args with
      | bufArg :: rest =>
        let bufTy ← checkExpr bufArg
        match bufTy with
        | .refMut .string => pure ()
        | _ => throwCheckMsg s!"append() first argument must be &mut String, got '{tyToString bufTy}'"
        if rest.isEmpty then throwCheckMsg s!"append() requires at least 2 arguments (&mut String, ...values)"
        for arg in rest do
          let argTy ← checkExpr arg
          match argTy with
          | .string | .ref .string | .refMut .string => pure ()
          | .int | .uint | .i32 | .u32 | .i16 | .u16 | .i8 | .u8 => pure ()
          | .bool => pure ()
          | .char => pure ()
          | _ => throwCheckMsg s!"append() argument has unsupported type '{tyToString argTy}'; expected String/&String/&mut String, Int/Uint/i8..i32/u8..u32, bool, or char"
        return .unit
      | [] => throwCheckMsg s!"append() requires at least 2 arguments (&mut String, ...values)"
    -- Intercept abort() calls
    if intrinsic == some .abort then
      if args.length != 0 then throwCheck (.builtinWrongArgCount "abort" 0) (some e.getSpan)
      return .never
    -- Intercept destroy() calls
    if intrinsic == some .destroy then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "destroy" 1) (some e.getSpan)
      let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let argTy ← checkExpr arg
      -- Look up impl Destroy for the type
      let typeName := match argTy with
        | .named n => n
        | .generic n _ => n
        | _ => ""
      if typeName == "" then throwCheck (.destroyRequiresNamed (tyToString argTy)) (some e.getSpan)
      -- Search function signatures for TypeName_destroy
      let destroyFn ← lookupFn (destroyFnNameFor typeName)
      match destroyFn with
      | some _ =>
        -- Consume the argument
        match arg with
        | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
        return .unit
      | none => throwCheck (.typeDoesNotImplDestroy typeName) (some e.getSpan)
    -- Intercept alloc(val) calls
    if intrinsic == some .alloc then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "alloc" 1) (some e.getSpan)
      let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let argTy ← checkExpr arg
      -- Consume linear variables passed to alloc (ownership moves to heap)
      match arg with
      | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
      | _ => pure ()
      return .heap argTy
    -- Intercept free(ptr) calls
    if intrinsic == some .free then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "free" 1) (some e.getSpan)
      let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let argTy ← checkExpr arg
      match argTy with
      | .heap innerTy =>
        -- Consume the argument (Heap<T> is linear)
        match arg with
        | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
        return innerTy
      | _ => throwCheck (.freeRequiresHeap (tyToString argTy)) (some e.getSpan)
    -- Intercept vec_new::<T>()
    if intrinsic == some .vecNew then
      if args.length != 0 then throwCheck (.builtinWrongArgCount "vec_new" 0) (some e.getSpan)
      if typeArgs.length != 1 then throwCheck (.builtinWrongTypeArgCount "vec_new" "1 type argument: vec_new::<T>()") (some e.getSpan)
      let elemTy := match typeArgs with | t :: _ => t | [] => Ty.int
      return .generic "Vec" [elemTy]
    -- Intercept string_push_char(&mut s, ch)
    if intrinsic == some .stringPushChar then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "string_push_char" 2) (some e.getSpan)
      let strArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let chArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let strTy ← checkExpr strArg
      match strTy with
      | .refMut .string => pure ()
      | _ => throwCheck (.builtinWrongFirstArg "string_push_char" "&mut String as first argument" (tyToString strTy)) (some e.getSpan)
      let chTy ← checkExpr chArg (some .int)
      expectTy .int chTy "string_push_char() char argument" (some e.getSpan)
      return .unit
    -- Intercept string_append(&mut s, other)
    if intrinsic == some .stringAppend then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "string_append" 2) (some e.getSpan)
      let strArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let otherArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let strTy ← checkExpr strArg
      match strTy with
      | .refMut .string => pure ()
      | _ => throwCheck (.builtinWrongFirstArg "string_append" "&mut String as first argument" (tyToString strTy)) (some e.getSpan)
      let otherTy ← checkExpr otherArg (some (.ref .string))
      expectTy (.ref .string) otherTy "string_append() second argument" (some e.getSpan)
      return .unit
    -- Intercept string_append_int(&mut s, n)
    if intrinsic == some .stringAppendInt then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "string_append_int" 2) (some e.getSpan)
      let strArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let nArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let strTy ← checkExpr strArg
      match strTy with
      | .refMut .string => pure ()
      | _ => throwCheck (.builtinWrongFirstArg "string_append_int" "&mut String as first argument" (tyToString strTy)) (some e.getSpan)
      let nTy ← checkExpr nArg (some .int)
      expectTy .int nTy "string_append_int() second argument" (some e.getSpan)
      return .unit
    -- Intercept string_append_bool(&mut s, b)
    if intrinsic == some .stringAppendBool then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "string_append_bool" 2) (some e.getSpan)
      let strArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let bArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let strTy ← checkExpr strArg
      match strTy with
      | .refMut .string => pure ()
      | _ => throwCheck (.builtinWrongFirstArg "string_append_bool" "&mut String as first argument" (tyToString strTy)) (some e.getSpan)
      let bTy ← checkExpr bArg (some .bool)
      expectTy .bool bTy "string_append_bool() second argument" (some e.getSpan)
      return .unit
    -- Intercept string_reserve(&mut s, cap)
    if intrinsic == some .stringReserve then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "string_reserve" 2) (some e.getSpan)
      let strArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let capArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let strTy ← checkExpr strArg
      match strTy with
      | .refMut .string => pure ()
      | _ => throwCheck (.builtinWrongFirstArg "string_reserve" "&mut String as first argument" (tyToString strTy)) (some e.getSpan)
      let capTy ← checkExpr capArg (some .int)
      expectTy .int capTy "string_reserve() capacity argument" (some e.getSpan)
      return .unit
    -- Intercept vec_push(&mut v, val)
    if intrinsic == some .vecPush then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "vec_push" 2) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let valArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let vecTy ← checkExpr vecArg
      let elemTy := match vecTy with
        | .refMut (.generic "Vec" [et]) => et
        | _ => Ty.placeholder
      if elemTy == .placeholder then throwCheck (.builtinWrongFirstArg "vec_push" "&mut Vec<T> as first argument" (tyToString vecTy)) (some e.getSpan)
      let valTy ← checkExpr valArg (some elemTy)
      expectTy elemTy valTy "vec_push() element argument" (some e.getSpan)
      match valArg with
      | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
      | _ => pure ()
      return .unit
    -- Intercept vec_get(&v, idx)
    if intrinsic == some .vecGet then
      if args.length != 2 then throwCheck (.builtinWrongArgCount "vec_get" 2) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let idxArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let vecTy ← checkExpr vecArg
      let elemTy := match vecTy with
        | .ref (.generic "Vec" [et]) => et
        | .refMut (.generic "Vec" [et]) => et
        | _ => Ty.placeholder
      if elemTy == .placeholder then throwCheck (.builtinWrongFirstArg "vec_get" "&Vec<T> or &mut Vec<T> as first argument" (tyToString vecTy)) (some e.getSpan)
      let idxTy ← checkExpr idxArg (some .int)
      expectTy .int idxTy "vec_get() index argument" (some e.getSpan)
      return elemTy
    -- Intercept vec_set(&mut v, idx, val)
    if intrinsic == some .vecSet then
      if args.length != 3 then throwCheck (.builtinWrongArgCount "vec_set" 3) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let idxArg := match args with | _ :: b :: _ => b | _ => Expr.intLit default 0
      let valArg := match args with | _ :: _ :: c :: _ => c | _ => Expr.intLit default 0
      let vecTy ← checkExpr vecArg
      let elemTy := match vecTy with
        | .refMut (.generic "Vec" [et]) => et
        | _ => Ty.placeholder
      if elemTy == .placeholder then throwCheck (.builtinWrongFirstArg "vec_set" "&mut Vec<T> as first argument" (tyToString vecTy)) (some e.getSpan)
      let idxTy ← checkExpr idxArg (some .int)
      expectTy .int idxTy "vec_set() index argument" (some e.getSpan)
      let valTy ← checkExpr valArg (some elemTy)
      expectTy elemTy valTy "vec_set() value argument" (some e.getSpan)
      match valArg with
      | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
      | _ => pure ()
      return .unit
    -- Intercept vec_len(&v)
    if intrinsic == some .vecLen then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "vec_len" 1) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let vecTy ← checkExpr vecArg
      let ok := match vecTy with
        | .ref (.generic "Vec" _) => true
        | .refMut (.generic "Vec" _) => true
        | _ => false
      if !ok then throwCheck (.builtinWrongFirstArg "vec_len" "&Vec<T> or &mut Vec<T> as argument" (tyToString vecTy)) (some e.getSpan)
      return .int
    -- Intercept vec_pop(&mut v)
    if intrinsic == some .vecPop then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "vec_pop" 1) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let vecTy ← checkExpr vecArg
      let elemTy := match vecTy with
        | .refMut (.generic "Vec" [et]) => et
        | _ => Ty.placeholder
      if elemTy == .placeholder then throwCheck (.builtinWrongFirstArg "vec_pop" "&mut Vec<T> as argument" (tyToString vecTy)) (some e.getSpan)
      return .generic optionEnumName [elemTy]
    -- Intercept vec_free(v)
    if intrinsic == some .vecFree then
      if args.length != 1 then throwCheck (.builtinWrongArgCount "vec_free" 1) (some e.getSpan)
      let vecArg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let vecTy ← checkExpr vecArg
      let ok := match vecTy with
        | .generic "Vec" _ => true
        | _ => false
      if !ok then throwCheck (.builtinWrongFirstArg "vec_free" "Vec<T> as argument" (tyToString vecTy)) (some e.getSpan)
      match vecArg with
      | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
      | _ => pure ()
      return .unit
    -- Check if this is a function pointer call (variable with fn_ type)
    let fnPtrVarTy ← lookupVarTy fnName
    match fnPtrVarTy with
    | some (.fn_ paramTys _fnPtrCapSet fnPtrRetTy) =>
      -- Check argument count
      if args.length != paramTys.length then
        throwCheck (.wrongArgCount s!"function pointer '{fnName}'" paramTys.length args.length) (some e.getSpan)
      -- Check each argument type
      for (arg, pTy) in args.zip paramTys do
        let argTy ← checkExpr arg (some pTy)
        expectTy pTy argTy s!"argument of function pointer call '{fnName}'" (some e.getSpan)
        match arg with
        | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
      -- Function pointers are Copy, no need to consume
      useVar fnName (some e.getSpan)
      return fnPtrRetTy
    | _ =>
    match ← lookupFn fnName with
    | some sig =>
      -- Infer type arguments if not explicitly provided
      let inferredTypeArgs ← do
        if !typeArgs.isEmpty || sig.typeParams.isEmpty then
          pure typeArgs
        else
          -- Infer types from argument types (without consuming)
          let mut inferred : List (String × Ty) := []
          for (arg, (_, pTy)) in args.zip sig.params do
            let argTy ← peekExprType arg
            -- Try to unify pTy with argTy to learn type variables
            let bindings := unifyTypes pTy argTy sig.typeParams
            for (name, ty) in bindings do
              if !(inferred.any fun (n, _) => n == name) then
                inferred := inferred ++ [(name, ty)]
          -- Build ordered type args from inferred mapping
          pure (sig.typeParams.map fun tp =>
            match inferred.lookup tp with
            | some ty => ty
            | none => .typeVar tp)
      -- Build type substitution
      let mapping := sig.typeParams.zip inferredTypeArgs
      -- Check trait bounds
      if !sig.typeBounds.isEmpty then
        checkTraitBounds sig.typeBounds mapping s!"generic function '{fnName}'"
      let paramTypes := sig.params.map fun (n, t) => (n, substTy mapping t)
      let retTy := substTy mapping sig.retTy
      -- Resolve capability variables from argument types
      let resolvedCapSet ← do
        if sig.capParams.isEmpty then
          pure sig.capSet
        else
          let mut capBindings : List (String × List String) := []
          -- Infer cap variable bindings from fn-typed arguments
          for (arg, (_, pTy)) in args.zip paramTypes do
            match pTy with
            | .fn_ _ (.concrete caps) _ =>
              for cap in caps do
                if sig.capParams.contains cap then
                  -- Get actual argument's cap set
                  let argCapSet ← do
                    let argTy ← peekExprType arg
                    match argTy with
                    | .fn_ _ cs _ => pure cs
                    | _ =>
                      match arg with
                      | .ident _ varName =>
                        match ← lookupFn varName with
                        | some argSig => pure argSig.capSet
                        | none => pure CapSet.empty
                      | _ => pure CapSet.empty
                  let (argCaps, _) := argCapSet.normalize
                  capBindings := capBindings ++ [(cap, argCaps)]
            | _ => pure ()
          -- Build resolved capSet
          let (concreteCaps, capVars) := sig.capSet.normalize
          let mut resolvedCaps : List String := []
          for cap in concreteCaps do
            if sig.capParams.contains cap then
              match capBindings.find? fun (name, _) => name == cap with
              | some (_, caps) => resolvedCaps := resolvedCaps ++ caps
              | none => throwCheck (.cannotInferCapVariable cap fnName) (some e.getSpan)
            else
              resolvedCaps := resolvedCaps ++ [cap]
          -- Also resolve cap variables (e.g. .var "C" → bound caps)
          for cv in capVars do
            match capBindings.find? fun (name, _) => name == cv with
            | some (_, caps) => resolvedCaps := resolvedCaps ++ caps
            | none => throwCheck (.cannotInferCapVariable cv fnName) (some e.getSpan)
          pure (CapSet.concrete resolvedCaps)
      -- Resolve cap variables in parameter types for type comparison
      let capBindings' := if sig.capParams.isEmpty then [] else
        sig.capParams.map fun cp =>
          match resolvedCapSet with
          | .concrete caps => (cp, caps.filter fun c => !sig.capParams.contains c)
          | _ => (cp, ([] : List String))
      let resolveCapInTy : Ty → Ty := fun ty =>
        match ty with
        | .fn_ params (.concrete caps) ret =>
          let newCaps := caps.foldl (fun acc cap =>
            if sig.capParams.contains cap then
              match capBindings'.find? fun (n, _) => n == cap with
              | some (_, resolved) => acc ++ resolved
              | none => acc
            else acc ++ [cap]) []
          .fn_ params (.concrete newCaps) ret
        | t => t
      let paramTypes := paramTypes.map fun (n, t) => (n, resolveCapInTy t)
      -- Check resolved capabilities for cap-polymorphic calls (cap variable inference)
      if !sig.capParams.isEmpty then
        let env ← getEnv
        let (resolvedCaps, _) := resolvedCapSet.normalize
        let (callerCaps, callerVars) := env.currentCapSet.normalize
        for cap in resolvedCaps do
          unless callerCaps.contains cap || callerVars.contains cap do
            throwCheck (.missingCapability fnName cap env.currentFnName) (some e.getSpan)
      if args.length != paramTypes.length then
        throwCheck (.wrongArgCount s!"function '{fnName}'" paramTypes.length args.length) (some e.getSpan)
      for (arg, (pName, pTy)) in args.zip paramTypes do
        let argTy ← checkExpr arg (some pTy)
        expectTy pTy argTy s!"argument '{pName}' of '{fnName}'" (some e.getSpan)
        -- If arg is a bare identifier of a linear type, consume it —
        -- but NOT if the parameter type is a reference (borrow, not move).
        -- Exception: borrow-block exclusive refs (&mut T) must be consumed on call,
        -- since they are scoped linear views into an alloca.
        -- Function parameter &mut T refs are reborrowable and not consumed.
        match arg with
        | .ident _ varName =>
          match pTy with
          | .ref _ => pure ()  -- shared reference: Copy, no consume needed
          | .refMut _ =>
            let env ← getEnv
            if env.borrowRefs.contains varName then
              consumeVarIfExists varName (some e.getSpan)
            else pure ()
          | _ => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
      return retTy
    | none =>
      -- sizeof intrinsic
      if intrinsic == some .sizeof || fnName.endsWith sizeofSuffix then return .uint
      else throwCheck (.undeclaredFunction fnName) (some e.getSpan)
  | .paren _ inner => checkExpr inner hint
  | .structLit _ name typeArgs fields =>
    match ← lookupStruct name with
    | some sd =>
      -- Build type substitution from struct type params + provided type args
      let mapping := sd.typeParams.zip typeArgs
      for sf in sd.fields do
        let fieldTy ← resolveType (substTy mapping sf.ty)
        match fields.find? fun (fn, _) => fn == sf.name with
        | some (_, expr) =>
          let exprTy ← checkExpr expr (some fieldTy)
          expectTy fieldTy exprTy s!"field '{sf.name}' of struct '{name}'" (some e.getSpan)
          -- Consume linear variables used as struct fields
          match expr with
          | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
          | _ => pure ()
        | none =>
          -- Unions allow partial initialization (only one field set)
          if !sd.isUnion then
            throwCheck (.missingFieldInLiteral sf.name s!"struct literal '{name}'") (some e.getSpan)
      for (fn, _) in fields do
        match sd.fields.find? fun sf => sf.name == fn with
        | some _ => pure ()
        | none => throwCheck (.unknownFieldInLiteral fn s!"struct literal '{name}'") (some e.getSpan)
      if typeArgs.isEmpty then return .named name
      else return .generic name typeArgs
    | none => throwCheck (.unknownStructType name) (some e.getSpan)
  | .fieldAccess _ obj field =>
    let objTy ← checkExpr obj
    -- Prevent direct field access on Heap<T> — must use ->
    match objTy with
    | .heap _ => throwCheck (.heapAccessRequired field (tyToString objTy)) (some e.getSpan)
    | .heapArray _ => throwCheck (.heapAccessRequired field (tyToString objTy)) (some e.getSpan)
    | _ => pure ()
    -- Auto-deref through references
    let innerTy := match objTy with
      | .ref t => t
      | .refMut t => t
      | t => t
    -- Extract struct name and type args for generic type substitution
    let (structName, typeArgs) := match innerTy with
      | .named n => (n, ([] : List Ty))
      | .generic n args => (n, args)
      | .string => ("String", [])
      | _ => ("", [])
    if structName == "" then throwCheck .fieldAccessNonStruct (some e.getSpan)
    else
      -- Check for newtype .0 unwrap
      match ← lookupNewtype structName with
      | some nt =>
        if field == newtypeFieldName then
          -- Consume the newtype variable if linear
          match obj with
          | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
          | _ => pure ()
          -- Substitute type params for generic newtypes
          let mapping := nt.typeParams.zip typeArgs
          return substTy mapping nt.innerTy
        else throwCheckMsg s!"newtype '{structName}' only supports .0 field access"
      | none =>
      match ← lookupStruct structName with
      | some sd =>
        match sd.fields.find? fun f => f.name == field with
        | some f =>
          let mapping := sd.typeParams.zip typeArgs
          resolveType (substTy mapping f.ty)
        | none => throwCheck (.structHasNoField structName field) (some e.getSpan)
      | none => throwCheck .fieldAccessNonStruct (some e.getSpan)
  | .enumLit _ enumName variant typeArgs fields =>
    match ← lookupEnum enumName with
    | some ed =>
      -- Infer type args from hint if not explicitly provided
      let effectiveTypeArgs := if typeArgs.isEmpty && !ed.typeParams.isEmpty then
        match hint with
        | some (.generic n args) => if n == enumName then args else []
        | _ => []
      else typeArgs
      let mapping := ed.typeParams.zip effectiveTypeArgs
      match ed.variants.find? fun v => v.name == variant with
      | some ev =>
        for sf in ev.fields do
          let fieldTy := substTy mapping sf.ty
          match fields.find? fun (fn, _) => fn == sf.name with
          | some (_, expr) =>
            let exprTy ← checkExpr expr (some fieldTy)
            expectTy fieldTy exprTy s!"field '{sf.name}' of {enumName}::{variant}" (some e.getSpan)
            -- Consume linear variables used as enum fields
            match expr with
            | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
            | _ => pure ()
          | none => throwCheck (.missingFieldInLiteral sf.name s!"{enumName}::{variant}") (some e.getSpan)
        for (fn, _) in fields do
          match ev.fields.find? fun sf => sf.name == fn with
          | some _ => pure ()
          | none => throwCheck (.unknownFieldInLiteral fn s!"{enumName}::{variant}") (some e.getSpan)
        if effectiveTypeArgs.isEmpty then return .named enumName
        else return .generic enumName effectiveTypeArgs
      | none => throwCheck (.unknownVariant variant enumName) (some e.getSpan)
    | none => throwCheck (.unknownEnumType enumName) (some e.getSpan)
  | .match_ _ scrutinee arms =>
    let scrTy ← checkExpr scrutinee
    -- Auto-deref through references for match
    let innerTy := match scrTy with
      | .ref t => t
      | .refMut t => t
      | t => t
    let innerTyR ← resolveType innerTy
    let (enumName, enumTypeArgs) := match innerTyR with
      | .named n => (n, ([] : List Ty))
      | .generic n args => (n, args)
      | _ => ("", [])
    if enumName != "" then
      match ← lookupEnum enumName with
      | some ed =>
        -- Consume scrutinee if it's a linear ident
        match scrutinee with
        | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
        -- CoreCheck validates exhaustiveness, duplicates, field counts, wrong-enum
        -- Linearity across arms: snapshot env, check each arm, ensure all agree
        let envBefore ← getEnv
        let mut firstArmVars : Option (List (String × VarInfo)) := none
        let mut matchResultTy : Ty := .unit
        let mut firstArmDone := false
        for arm in arms do
          setEnv envBefore
          let body ← match arm with
          | .mk _ _armEnum armVariant bindings body => do
            -- Bind variant fields in scope (substitute generic type args)
            let ev := (ed.variants.find? fun v => v.name == armVariant).get!
            let typeMapping := ed.typeParams.zip enumTypeArgs
            for (binding, sf) in bindings.zip ev.fields do
              addVar binding (substTy typeMapping sf.ty)
            pure body
          | .litArm _ _val body => pure body
          | .varArm _ binding body => do
            if binding != "_" then addVar binding innerTyR
            pure body
          -- Check all stmts except the last, then extract type from last
          let bodyInit := body.dropLast
          let curEnv ← getEnv
          checkStmts bodyInit curEnv.currentRetTy
          let armTy ← match body.getLast? with
            | some (.expr _ armExpr) => do
              let ty ← checkExpr armExpr hint
              -- Trailing expression is a value move — consume linear variables
              match armExpr with
              | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
              | _ => pure ()
              pure ty
            | some (.return_ _ v) =>
              match v with
              | some rv => let _ ← checkExpr rv; pure Ty.never
              | none => pure Ty.never
            | some other =>
              checkStmt other curEnv.currentRetTy
              pure Ty.unit
            | none => pure Ty.unit
          -- Unify arm types
          if !firstArmDone then
            matchResultTy := armTy
            firstArmDone := true
          else
            if armTy != matchResultTy && armTy != .never && matchResultTy != .never then
              throwCheck (.matchArmTypeMismatch (tyToString matchResultTy) (tyToString armTy)) (some e.getSpan)
            if matchResultTy == .never then matchResultTy := armTy
          let envAfterArm ← getEnv
          match firstArmVars with
          | none => firstArmVars := some envAfterArm.vars
          | some firstVars =>
            -- Check agreement on pre-existing variables
            for (name, infoBefore) in envBefore.vars do
              if infoBefore.isCopy then continue
              let state1 := match firstVars.lookup name with
                | some info => info.state
                | none => infoBefore.state
              let state2 := match envAfterArm.vars.lookup name with
                | some info => info.state
                | none => infoBefore.state
              let consumed1 := state1 == .consumed
              let consumed2 := state2 == .consumed
              if consumed1 != consumed2 then
                throwCheck (.matchConsumptionDisagreement name) (some e.getSpan)
        -- Apply the final state from first arm (they all agree)
        match firstArmVars with
        | some vars =>
          let env ← getEnv
          let vars' := env.vars.map fun (n, vi) =>
            match vars.lookup n with
            | some info => (n, { vi with state := info.state })
            | none => (n, vi)
          setEnv { envBefore with vars := vars' }
        | none => setEnv envBefore
        return matchResultTy
      | none => throwCheck (.unknownEnumType enumName) (some e.getSpan)
    else
      -- Value-pattern match (integer/bool literals, variable bindings)
      match scrutinee with
      | .ident _ varName => useVar varName (some e.getSpan)
      | _ => pure ()
      let envBefore ← getEnv
      let mut matchResultTy : Ty := .unit
      let mut firstArmDone := false
      let mut firstArmVars : Option (List (String × VarInfo)) := none
      for arm in arms do
        setEnv envBefore
        let body ← match arm with
        | .litArm _ _val body => pure body
        | .varArm _ binding body => do
          if binding != "_" then addVar binding scrTy
          pure body
        | .mk _ _ _ _ body => pure body
        -- Check all stmts except the last, then extract type from last
        let bodyInit := body.dropLast
        checkStmts bodyInit envBefore.currentRetTy
        let armTy ← match body.getLast? with
          | some (.expr _ armExpr) => do
            let ty ← checkExpr armExpr hint
            -- Trailing expression is a value move — consume linear variables
            match armExpr with
            | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
            | _ => pure ()
            pure ty
          | some (.return_ _ v) =>
            match v with
            | some rv => let _ ← checkExpr rv; pure Ty.never
            | none => pure Ty.never
          | some other =>
            checkStmt other envBefore.currentRetTy
            pure Ty.unit
          | none => pure Ty.unit
        if !firstArmDone then
          matchResultTy := armTy
          firstArmDone := true
        else
          if armTy != matchResultTy && armTy != .never && matchResultTy != .never then
            throwCheck (.matchArmTypeMismatch (tyToString matchResultTy) (tyToString armTy)) (some e.getSpan)
          if matchResultTy == .never then matchResultTy := armTy
        let envAfterArm ← getEnv
        match firstArmVars with
        | none => firstArmVars := some envAfterArm.vars
        | some firstVars =>
          -- Check agreement on pre-existing variables
          for (name, infoBefore) in envBefore.vars do
            if infoBefore.isCopy then continue
            let state1 := match firstVars.lookup name with
              | some info => info.state
              | none => infoBefore.state
            let state2 := match envAfterArm.vars.lookup name with
              | some info => info.state
              | none => infoBefore.state
            let consumed1 := state1 == .consumed
            let consumed2 := state2 == .consumed
            if consumed1 != consumed2 then
              throwCheck (.matchConsumptionDisagreement name) (some e.getSpan)
      -- Apply the final state from first arm (they all agree)
      match firstArmVars with
      | some vars =>
        let env ← getEnv
        let vars' := env.vars.map fun (n, vi) =>
          match vars.lookup n with
          | some info => (n, { vi with state := info.state })
          | none => (n, vi)
        setEnv { envBefore with vars := vars' }
      | none => setEnv envBefore
      return matchResultTy
  | .borrow _ inner =>
    let innerTy ← checkExpr inner
    -- Check the variable is not moved or already mutably borrowed
    match inner with
    | .ident _ varName =>
      match ← lookupVarInfo varName with
      | some info =>
        if !info.isCopy && info.state == .consumed then
          throwCheck (.cannotBorrowMoved varName) (some e.getSpan)
        let env ← getEnv
        let activeRefs := activeBorrowRefs env varName
        if activeRefs.any (fun refInfo => match refInfo.ty with | .refMut _ => true | _ => false) then
          throwCheck (.cannotBorrowMutablyBorrowed varName) (some e.getSpan)
      | none => throwCheck (.undeclaredVariable varName) (some e.getSpan)
    | _ => pure ()
    return .ref innerTy
  | .borrowMut _ inner =>
    let innerTy ← checkExpr inner
    match inner with
    | .ident _ varName =>
      match ← lookupVarInfo varName with
      | some info =>
        if !info.isCopy && info.state == .consumed then
          throwCheck (.cannotBorrowMoved varName) (some e.getSpan)
        let env ← getEnv
        let activeRefs := activeBorrowRefs env varName
        if info.borrowCount > 0 || activeRefs.any (fun refInfo => match refInfo.ty with | .ref _ | .refMut _ => true | _ => false) then
          throwCheck (.cannotMutBorrowAlreadyBorrowed varName) (some e.getSpan)
        if !info.mutable then
          throwCheck (.cannotMutBorrowImmutable varName) (some e.getSpan)
      | none => throwCheck (.undeclaredVariable varName) (some e.getSpan)
    | _ => pure ()
    return .refMut innerTy
  | .deref _ inner =>
    let innerTy ← checkExpr inner
    match innerTy with
    | .ref t => return t
    | .refMut t => return t
    | .ptrMut t =>
      return t
    | .ptrConst t =>
      return t
    | .heap t =>
      -- *heap_ptr: loads value from heap, frees memory, consumes the Heap<T>
      match inner with
      | .ident _ varName => consumeVar varName (some e.getSpan)
      | _ => pure ()
      return t
    | _ => throwCheck .cannotDerefNonRef (some e.getSpan)
  | .try_ _ inner =>
    let innerTy ← checkExpr inner
    -- Consume the inner expression if it's a variable
    match inner with
    | .ident _ name => consumeVar name (some e.getSpan)
    | _ => pure ()
    let (enumName, typeArgs) := match innerTy with
      | .named n => (n, ([] : List Ty))
      | .generic n args => (n, args)
      | _ => ("", [])
    if enumName == "" then
      throwCheck .tryRequiresResult (some e.getSpan)
    else
      match ← lookupEnum enumName with
      | some ed =>
        let okVariant := ed.variants.find? fun v => v.name == okVariantName
        let errVariant := ed.variants.find? fun v => v.name == errVariantName
        match okVariant, errVariant with
        | some ok, some _ =>
          -- Function must return the same Result type
          let env ← getEnv
          expectTy innerTy env.currentRetTy "try (?) operator: function must return same Result type" (some e.getSpan)
          -- Return the type of the first field in Ok variant, with type substitution for generics
          let mapping := ed.typeParams.zip typeArgs
          match ok.fields.head? with
          | some f => return (substTy mapping f.ty)
          | none => throwCheck (.tryOkNoField enumName) (some e.getSpan)
        | _, _ => throwCheck .tryRequiresOkErrVariants (some e.getSpan)
      | none => throwCheck (.unknownEnumType enumName) (some e.getSpan)
  | .arrayLit _ elems =>
    match elems with
    | [] => return .array .placeholder 0  -- CoreCheck validates empty array literals
    | first :: rest =>
      -- Use hint to determine element type (e.g. [i32; N] → elements are i32)
      let elemHint := match hint with
        | some (.array t _) => some t
        | _ => none
      let firstTy ← checkExpr first elemHint
      for e in rest do
        let eTy ← checkExpr e (some firstTy)
        expectTy firstTy eTy "array element" (some e.getSpan)
      return .array firstTy elems.length
  | .arrayIndex _ arr index =>
    let arrTy ← checkExpr arr
    let _idxTy ← checkExpr index
    -- CoreCheck validates index type and array type
    match arrTy with
    | .array elemTy _ => return elemTy
    | _ => return .placeholder
  | .cast _ inner targetTy =>
    let _innerTy ← checkExpr inner
    -- CoreCheck validates cast legality and capability requirements
    return targetTy
  | .methodCall _ obj methodName typeArgs args =>
    let objTy ← checkExpr obj
    let innerTy := match objTy with
      | .ref t => t
      | .refMut t => t
      | t => t
    let typeName := tyName innerTy
    if typeName == "" then
      -- Check if this is a type variable with trait bounds
      match innerTy with
      | .typeVar n =>
        let env ← getEnv
        let bounds := (env.currentTypeBounds.find? fun (name, _) => name == n).map Prod.snd |>.getD []
        -- Find the method in one of the bound traits
        let mut foundSig : Option FnSigDef := none
        for traitName in bounds do
          match env.traits.find? fun td => td.name == traitName with
          | some td =>
            match td.methods.find? fun ms => ms.name == methodName with
            | some ms => foundSig := some ms; break
            | none => pure ()
          | none => pure ()
        match foundSig with
        | none => throwCheck (.noMethodOnTypeVar methodName n) (some e.getSpan)
        | some sig =>
          -- Replace Self with the type variable in the trait signature
          let selfTy := Ty.typeVar n
          let retTy := substSelf sig.retTy selfTy
          let params := sig.params.map fun p => { p with ty := substSelf p.ty selfTy }
          -- Type check arguments (params in FnSigDef excludes self)
          if args.length != params.length then
            throwCheck (.wrongArgCount s!"method '{methodName}'" params.length args.length) (some e.getSpan)
          for (arg, p) in args.zip params do
            let argTy ← checkExpr arg (some p.ty)
            expectTy p.ty argTy s!"argument '{p.name}' of '{methodName}'" (some e.getSpan)
            match arg with
            | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
            | _ => pure ()
          return retTy
      | _ => throwCheck .methodCallOnNonNamedType (some e.getSpan)
    else
    let mangledName := typeName ++ "_" ++ methodName
    match ← lookupFn mangledName with
    | some sig =>
      -- Build type mapping from object's generic type args + explicit call typeArgs
      let objTypeArgs := match innerTy with
        | .generic _ args => args
        | _ => []
      let implTypeParams := sig.typeParams.take objTypeArgs.length
      let methodTypeParams := sig.typeParams.drop objTypeArgs.length
      let mapping := implTypeParams.zip objTypeArgs ++ methodTypeParams.zip typeArgs
      let methodParams := (sig.params.drop 1).map fun (n, t) => (n, substTy mapping t)
      let retTy := substTy mapping sig.retTy
      if args.length != methodParams.length then
        throwCheck (.wrongArgCount s!"method '{methodName}'" methodParams.length args.length) (some e.getSpan)
      for (arg, (pName, pTy)) in args.zip methodParams do
        let argTy ← checkExpr arg (some pTy)
        expectTy pTy argTy s!"argument '{pName}' of '{methodName}'" (some e.getSpan)
        match arg with
        | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
      -- Consume the receiver when appropriate:
      -- - &self: never consume (shared ref is Copy)
      -- - &mut self: consume only if receiver is a borrow-block exclusive ref
      --   (function param &mut refs are reborrowable; auto-borrowed values don't consume)
      -- - self (by value): always consume
      match sig.params.head? with
      | some ("self", selfTy) =>
        match selfTy with
        | .ref _ => pure ()  -- shared reference: Copy, no consume needed
        | .refMut _ =>
          -- Only consume borrow-block refs (scoped linear views into an alloca).
          match objTy with
          | .refMut _ =>
            match obj with
            | .ident _ varName =>
              let env ← getEnv
              if env.borrowRefs.contains varName then
                consumeVarIfExists varName (some e.getSpan)
              else pure ()
            | _ => pure ()
          | _ => pure ()
        | _ =>
          -- Self is by value — this method consumes the receiver
          match obj with
          | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
          | _ => pure ()
      | _ => pure ()
      return retTy
    | none => throwCheck (.noMethodOnType methodName typeName) (some e.getSpan)
  | .staticMethodCall _ typeName methodName typeArgs args =>
    let mangledName := typeName ++ "_" ++ methodName
    match ← lookupFn mangledName with
    | some sig =>
      let mapping := sig.typeParams.zip typeArgs
      let paramTypes := sig.params.map fun (n, t) => (n, substTy mapping t)
      let retTy := substTy mapping sig.retTy
      if args.length != paramTypes.length then
        throwCheck (.wrongArgCount s!"static method '{methodName}'" paramTypes.length args.length) (some e.getSpan)
      for (arg, (pName, pTy)) in args.zip paramTypes do
        let argTy ← checkExpr arg (some pTy)
        expectTy pTy argTy s!"argument '{pName}' of '{typeName}::{methodName}'" (some e.getSpan)
        match arg with
        | .ident _ varName => consumeVarIfExists varName (some e.getSpan)
        | _ => pure ()
      return retTy
    | none => throwCheck (.noMethodOnType methodName typeName) (some e.getSpan)
  | .fnRef _ fnName =>
    -- Look up the function signature to build the fn pointer type
    let env ← getEnv
    match env.allFnSummarys.lookup fnName with
    | some sig =>
      let paramTys := sig.params.map Prod.snd
      return .fn_ paramTys sig.capSet sig.retTy
    | none => throwCheck (.unknownFunctionRef fnName) (some e.getSpan)

partial def checkStmt (stmt : Stmt) (retTy : Ty) : CheckM Unit := do
  match stmt with
  | .letDecl _ name mutable ty value =>
    -- Escape analysis: prevent storing a borrow ref into a new binding
    let env ← getEnv
    match value with
    | .ident _ vn =>
      if env.borrowRefs.contains vn then
        throwCheck (.referenceEscapesBorrowBlock vn) (some stmt.getSpan)
    | _ => pure ()
    let valTy ← checkExpr value ty
    match ty with
    | some declTy => expectTy declTy valTy s!"let binding '{name}'" (some stmt.getSpan)
    | none => pure ()
    let finalTy ← match ty with
      | some t => resolveType t
      | none => pure valTy
    addVar name finalTy mutable
    match value with
    | .borrow _ (.ident _ sourceName) =>
      modify fun env =>
        { env with vars := env.vars.map fun (n, info) =>
            if n == name then (n, { info with borrowedFrom := some sourceName }) else (n, info) }
    | .borrowMut _ (.ident _ sourceName) =>
      modify fun env =>
        { env with vars := env.vars.map fun (n, info) =>
            if n == name then (n, { info with borrowedFrom := some sourceName }) else (n, info) }
    | _ => pure ()
  | .assign _ name value =>
    -- Escape analysis: prevent storing a borrow ref into an outer variable
    let env ← getEnv
    match value with
    | .ident _ vn =>
      if env.borrowRefs.contains vn then
        throwCheck (.referenceEscapesBorrowBlock vn) (some stmt.getSpan)
    | _ => pure ()
    match ← lookupVarInfo name with
    | some info =>
      if !info.mutable then
        throwCheck (.assignToImmutable name) (some stmt.getSpan)
      if info.state == .frozen then
        throwCheck (.assignToFrozen name) (some stmt.getSpan)
      -- Linear variables cannot be reassigned. One binding, one resource.
      if !info.isCopy then
        throwCheck (.assignOverwritesLinear name) (some stmt.getSpan)
      let valTy ← checkExpr value (some info.ty)
      expectTy info.ty valTy s!"assignment to '{name}'" (some stmt.getSpan)
    | none => throwCheck (.assignToUndeclaredVariable name) (some stmt.getSpan)
  | .return_ _ (some value) =>
    -- Escape analysis: prevent returning a borrow ref
    let env ← getEnv
    match value with
    | .ident _ vn =>
      if env.borrowRefs.contains vn then
        throwCheck (.referenceEscapesBorrowBlock vn) (some stmt.getSpan)
    | _ => pure ()
    let valTy ← checkExpr value (some retTy)
    expectTy retTy valTy "return value" (some stmt.getSpan)
    -- Returning a linear variable consumes it
    match value with
    | .ident _ varName => consumeVar varName (some stmt.getSpan)
    | _ => pure ()
  | .return_ _ none =>
    expectTy .unit retTy "return (void)" (some stmt.getSpan)
  | .expr _ e =>
    let _ ← checkExpr e
    pure ()
  | .ifElse _ cond thenBody elseBody =>
    let _condTy ← checkExpr cond
    -- Snapshot variable states before branches
    let envBefore ← getEnv
    -- Check then branch
    checkStmts thenBody retTy
    let envAfterThen ← getEnv
    -- Restore env and check else branch
    setEnv envBefore
    match elseBody with
    | some stmts =>
      checkStmts stmts retTy
      let envAfterElse ← getEnv
      -- Merge: both branches must agree on consumption state of linear vars
      mergeVarStates envBefore.vars envAfterThen.vars envAfterElse.vars (some stmt.getSpan)
    | none =>
      -- No else branch: then branch must not consume any linear var
      -- Exception: if then-branch always returns, consumption is fine (control never falls through)
      let thenDiverges := match thenBody.getLast? with
        | some (.return_ _ _) => true
        | _ => false
      if !thenDiverges then
        checkNoBranchConsumption envBefore.vars envAfterThen.vars "if-without-else" (some stmt.getSpan)
  | .while_ _ cond body lbl =>
    let _condTy ← checkExpr cond
    -- Increment loop depth for the body, push label if present
    let env ← getEnv
    let labels := match lbl with
      | some l => l :: env.loopLabels
      | none => env.loopLabels
    setEnv { env with loopDepth := env.loopDepth + 1, loopLabels := labels }
    checkStmts body retTy
    -- Restore loop depth and labels
    let env' ← getEnv
    setEnv { env' with loopDepth := env.loopDepth, loopLabels := env.loopLabels }
  | .forLoop _ init cond step body lbl =>
    -- Init
    match init with
    | some initStmt => checkStmt initStmt retTy
    | none => pure ()
    -- Condition
    let _condTy ← checkExpr cond
    -- Body + step in loop scope, push label if present
    let env ← getEnv
    let labels := match lbl with
      | some l => l :: env.loopLabels
      | none => env.loopLabels
    setEnv { env with loopDepth := env.loopDepth + 1, loopLabels := labels }
    checkStmts body retTy
    match step with
    | some stepStmt => checkStmt stepStmt retTy
    | none => pure ()
    let env' ← getEnv
    setEnv { env' with loopDepth := env.loopDepth, loopLabels := env.loopLabels }
  | .fieldAssign _ obj field value =>
    -- Escape analysis: prevent storing a borrow ref into a struct field
    let env ← getEnv
    match value with
    | .ident _ vn =>
      if env.borrowRefs.contains vn then
        throwCheck (.referenceEscapesBorrowBlock vn) (some stmt.getSpan)
    | _ => pure ()
    let objTy ← checkExpr obj
    -- Auto-deref through references
    let innerTy := match objTy with
      | .ref t => t
      | .refMut t => t
      | t => t
    match innerTy with
    | .named structName =>
      match ← lookupStructField structName field with
      | some fieldTy =>
        let valTy ← checkExpr value (some fieldTy)
        expectTy fieldTy valTy s!"field assignment '{structName}.{field}'" (some stmt.getSpan)
      | none => throwCheck (.structHasNoField structName field) (some stmt.getSpan)
    | _ => throwCheck .fieldAccessNonStruct (some stmt.getSpan)
  | .derefAssign _ target value =>
    let targetTy ← checkExpr target
    match targetTy with
    | .refMut inner =>
      let valTy ← checkExpr value (some inner)
      expectTy inner valTy "deref assignment" (some stmt.getSpan)
    | .ptrMut inner =>
      let valTy ← checkExpr value (some inner)
      expectTy inner valTy "deref assignment" (some stmt.getSpan)
    | _ => pure ()  -- CoreCheck validates deref-assign target type
  | .arrayIndexAssign _ arr index value =>
    let arrTy ← checkExpr arr
    let _idxTy ← checkExpr index
    -- CoreCheck validates index type and array type
    match arrTy with
    | .array elemTy _ =>
      let valTy ← checkExpr value (some elemTy)
      expectTy elemTy valTy "array element assignment" (some stmt.getSpan)
    | _ => let _ ← checkExpr value; pure ()
  | .defer _ body =>
    -- Verify body is a call expression
    match body with
    | .call _ _ _ _ => pure ()
    | _ => throwCheck .deferBodyNotCall (some stmt.getSpan)
    let _ ← checkExpr body
    -- Any variable consumed by the deferred call should be reserved, not consumed.
    -- The actual consumption happens at scope exit.
    match body with
    | .call _ _fname _ args =>
      for arg in args do
        match arg with
        | .ident _ varName =>
          let env ← getEnv
          match env.vars.lookup varName with
          | some info =>
            if info.state == .consumed then
              let vars' := env.vars.map fun (n, vi) =>
                if n == varName then (n, { vi with state := .reserved })
                else (n, vi)
              setEnv { env with vars := vars' }
          | none => pure ()
        | _ => pure ()
    | _ => pure ()
  | .borrowIn _ var ref region isMut body =>
    -- Check that var exists
    match ← lookupVarInfo var with
    | none => throwCheck (.undeclaredVariable var) (some stmt.getSpan)
    | some varInfo =>
      -- Check no shadowing of ref and region names
      let env ← getEnv
      if (env.vars.lookup ref).isSome then
        throwCheck (.borrowRefShadows ref) (some stmt.getSpan)
      if (env.vars.lookup region).isSome then
        throwCheck (.borrowRegionShadows region) (some stmt.getSpan)
      -- Check if variable is consumed (moved)
      if !varInfo.isCopy && varInfo.state == .consumed then
        throwCheck (.cannotBorrowMoved var) (some stmt.getSpan)
      -- Check if variable is frozen (already inside another borrow block)
      if varInfo.state == .frozen then
        throwCheck (.variableFrozenByBorrow var) (some stmt.getSpan)
      -- Check for mutable borrow conflict: if var is already immutably borrowed, error
      if isMut && varInfo.borrowCount > 0 then
        throwCheck (.cannotMutBorrowImmBorrowed var) (some stmt.getSpan)
      -- Save state and freeze the original variable
      let savedState := varInfo.state
      let vars' := env.vars.map fun (n, vi) =>
        if n == var then (n, { vi with state := .frozen })
        else (n, vi)
      setEnv { env with vars := vars' }
      -- Add reference binding and track for escape analysis
      let refTy := if isMut then Ty.refMut varInfo.ty else Ty.ref varInfo.ty
      addVar ref refTy true
      let envWithRef ← getEnv
      setEnv { envWithRef with borrowRefs := ref :: envWithRef.borrowRefs }
      -- Check body
      checkStmts body env.currentRetTy
      -- Clean up: remove ref from borrowRefs and unfreeze original variable
      let env' ← getEnv
      let vars'' := (env'.vars.map fun (n, vi) =>
        if n == var then (n, { vi with state := savedState })
        else (n, vi)).filter fun (n, _) => n != ref
      let cleanedRefs := env'.borrowRefs.filter (· != ref)
      setEnv { env' with vars := vars'', borrowRefs := cleanedRefs }
  | .arrowAssign _ obj field value =>
    let objTy ← checkExpr obj
    let innerTy := match objTy with
      | .heap t => t
      | .heapArray t => t
      | .ref (.heap t) | .refMut (.heap t) => t
      | _ => .placeholder
    if innerTy == .placeholder then
      throwCheck (.arrowAssignNotHeap (tyToString objTy)) (some stmt.getSpan)
    let structName := match innerTy with
      | .named n => n
      | _ => ""
    if structName == "" then throwCheck .arrowAssignNonStruct (some stmt.getSpan)
    match ← lookupStructField structName field with
    | some fieldTy =>
      let valTy ← checkExpr value (some fieldTy)
      expectTy fieldTy valTy s!"arrow field assignment '{structName}->{field}'" (some stmt.getSpan)
    | none => throwCheck (.structHasNoField structName field) (some stmt.getSpan)
  | .break_ _ value lbl =>
    let env ← getEnv
    if env.loopDepth == 0 then
      throwCheck .breakOutsideLoop (some stmt.getSpan)
    -- Validate label if present
    match lbl with
    | some l =>
      if !env.loopLabels.contains l then
        throwCheck (.unknownLoopLabel l) (some stmt.getSpan)
    | none => pure ()
    -- Check all linear variables declared in the loop body are consumed or reserved by defer
    for (name, info) in env.vars do
      if !info.isCopy && info.state != .consumed && info.state != .reserved && info.loopDepth >= env.loopDepth then
        throwCheck (.breakSkipsUnconsumedLinear name) (some stmt.getSpan)
    -- Check break value if present (for while-as-expression)
    match value with
    | some expr =>
      let valTy ← checkExpr expr
      let env2 ← getEnv
      match env2.loopBreakTy with
      | none => setEnv { env2 with loopBreakTy := some valTy }
      | some prevTy =>
        if prevTy != valTy then
          throwCheck (.breakTypeMismatch (tyToString valTy) (tyToString prevTy)) (some stmt.getSpan)
    | none => pure ()
  | .continue_ _ lbl =>
    let env ← getEnv
    if env.loopDepth == 0 then
      throwCheck .continueOutsideLoop (some stmt.getSpan)
    -- Validate label if present
    match lbl with
    | some l =>
      if !env.loopLabels.contains l then
        throwCheck (.unknownLoopLabel l) (some stmt.getSpan)
    | none => pure ()
    -- Check all linear variables declared in the loop body are consumed or reserved by defer
    for (name, info) in env.vars do
      if !info.isCopy && info.state != .consumed && info.state != .reserved && info.loopDepth >= env.loopDepth then
        throwCheck (.continueSkipsUnconsumedLinear name) (some stmt.getSpan)
  -- These are desugared before reaching Check; should never appear
  | .letDestructure _ _ _ _ _ _ => pure ()
  | .letStructDestructure _ _ _ _ => pure ()

partial def checkStmts (stmts : List Stmt) (retTy : Ty) : CheckM Unit := do
  let mut accumulated : Diagnostics := []
  for stmt in stmts do
    let envBefore ← getEnv
    let result := (checkStmt stmt retTy).run envBefore |>.run
    match result with
    | (.ok (), envAfter) => setEnv envAfter
    | (.error ds, _) =>
      accumulated := accumulated ++ ds
      -- Restore env so subsequent statements see a consistent state.
      -- For let-declarations, add the variable with its declared type (or placeholder)
      -- so later statements referencing it don't cascade spurious errors.
      setEnv envBefore
      match stmt with
      | .letDecl _ name _ ty _ =>
        let placeholderTy := ty.getD .placeholder
        addVar name placeholderTy false
      | _ => pure ()
  if !accumulated.isEmpty then
    throw accumulated

/-- After if/else, check both branches agree on linear var consumption. -/
partial def mergeVarStates
    (before : List (String × VarInfo))
    (afterThen : List (String × VarInfo))
    (afterElse : List (String × VarInfo))
    (span : Option Span := none) : CheckM Unit := do
  for (name, infoBefore) in before do
    if infoBefore.isCopy then continue
    let thenState := match afterThen.lookup name with
      | some info => info.state
      | none => infoBefore.state
    let elseState := match afterElse.lookup name with
      | some info => info.state
      | none => infoBefore.state
    -- Both consumed or both not-consumed (used/unconsumed are equivalent here)
    let thenConsumed := thenState == .consumed
    let elseConsumed := elseState == .consumed
    if thenConsumed != elseConsumed then
      throwCheck (.linearConsumedOneBranchNotOther name) span
    -- Apply the most progressed state (consumed > used > unconsumed)
    let mergedState := if thenState == .consumed then .consumed
      else if thenState == .used || elseState == .used then .used
      else infoBefore.state
    if mergedState != infoBefore.state then
      let env ← getEnv
      let vars' := env.vars.map fun (n, vi) =>
        if n == name then (n, { vi with state := mergedState })
        else (n, vi)
      setEnv { env with vars := vars' }

/-- For if-without-else: the then branch must not consume any linear var
    that existed before the if. -/
partial def checkNoBranchConsumption
    (before : List (String × VarInfo))
    (afterThen : List (String × VarInfo))
    (ctx : String)
    (span : Option Span := none) : CheckM Unit := do
  for (name, infoBefore) in before do
    if infoBefore.isCopy then continue
    if infoBefore.state == .consumed then continue
    let thenState := match afterThen.lookup name with
      | some info => info.state
      | none => infoBefore.state
    if thenState == .consumed then
      throwCheck (.linearConsumedNoBranch name ctx) span

end

private def resolveTypeParams (ty : Ty) (typeParams : List String) : Ty :=
  match ty with
  | .named n => if typeParams.contains n then .typeVar n else ty
  | .ref t => .ref (resolveTypeParams t typeParams)
  | .refMut t => .refMut (resolveTypeParams t typeParams)
  | .ptrMut t => .ptrMut (resolveTypeParams t typeParams)
  | .ptrConst t => .ptrConst (resolveTypeParams t typeParams)
  | .array t n => .array (resolveTypeParams t typeParams) n
  | .generic name args => .generic name (args.map fun a => resolveTypeParams a typeParams)
  | _ => ty

def checkFn (f : FnDef) : CheckM Unit := do
  -- Validate #[test] constraints
  if f.isTest then
    if !f.params.isEmpty then
      throwCheckMsg s!"#[test] function '{f.name}' must have no parameters" f.span
    if !f.typeParams.isEmpty then
      throwCheckMsg s!"#[test] function '{f.name}' must not be generic" f.span
    if f.retTy != .i32 then
      throwCheckMsg s!"#[test] function '{f.name}' must return i32" f.span
  -- Save env state (vars from previous functions shouldn't leak)
  let envBefore ← getEnv
  -- Resolve type parameter names: .named "T" -> .typeVar "T"
  let retTyRaw := resolveTypeParams f.retTy f.typeParams
  -- Set current return type, type params, capability context, and type bounds
  let env1 := { envBefore with currentRetTy := retTyRaw, currentTypeParams := f.typeParams }
  let env2 := { env1 with currentCapSet := f.capSet }
  let env3 := { env2 with currentTypeBounds := f.typeBounds }
  setEnv { env3 with currentFnName := f.name }
  -- Resolve Self in return type (needs currentImplType from env)
  let retTy ← resolveType retTyRaw
  modify fun env => { env with currentRetTy := retTy }
  -- Add params to env. Linear params are "consumed" by being received.
  let mut paramNames : List String := []
  for p in f.params do
    let paramTyRaw := resolveTypeParams p.ty f.typeParams
    let paramTy ← resolveType paramTyRaw
    addVar p.name paramTy true  -- params are always mutable for now
    paramNames := paramNames ++ [p.name]
  -- Check body
  checkStmts f.body retTy
  -- Check local bindings, plus generic by-value params whose linearity is otherwise erased.
  let envAfter ← getEnv
  let localVars := envAfter.vars.filter fun (name, _) =>
    match envBefore.vars.lookup name with
    | some _ => false
    | none =>
      if paramNames.contains name then
        -- For params with generic types, only flag if completely untouched (.unconsumed).
        -- .used is acceptable — the function used the parameter (borrowed, field-accessed, etc).
        match envAfter.vars.lookup name with
        | some info => tyContainsTypeVar info.ty && info.state == .unconsumed
        | none => false
      else true
  let localNames := localVars.map fun (name, _) => name
  checkScopeExit localNames
  -- Restore env (remove this function's locals)
  setEnv envBefore


def checkModule (m : Module) (summary : FileSummary)
    (imports : ResolvedImports := {}) : Except Diagnostics Unit :=
  -- Use pre-built summaries from FileSummary
  let fnSigs : List FnSummary := summary.functions.map Prod.snd
  let externSigs : List FnSummary := summary.externFnSigs.map Prod.snd
  let importedSigList := imports.functions.map Prod.snd
  let baseOffset := importedSigList.length
  -- Built-in functions for strings and I/O (shared definition in BuiltinSigs.lean)
  let builtinSigs := builtinFnSigs.map Prod.snd
  let builtinOffset := baseOffset + fnSigs.length
  let builtinNames : List (String × Nat) :=
    (enumerateList builtinFnSigs).map fun (idx, (name, _)) => (name, builtinOffset + idx)
  -- Add submodule functions/extern fns with qualified names (mod_fn)
  let submoduleSigs : List FnSummary := summary.submoduleSummaries.foldl (fun acc (_, subSummary) =>
    acc ++ subSummary.functions.map Prod.snd
    ++ subSummary.externFnSigs.map Prod.snd
  ) []
  let submoduleNames : List (String × Nat) := summary.submoduleSummaries.foldl (fun (acc : List (String × Nat)) (subName, subSummary) =>
    let baseIdx := baseOffset + fnSigs.length + builtinSigs.length + externSigs.length + acc.length
    let fnNames' : List (String × Nat) := (enumerateList subSummary.functions).map fun (idx, (name, _)) =>
      (subName ++ "_" ++ name, baseIdx + idx)
    let efNames : List (String × Nat) := (enumerateList subSummary.externFnSigs).map fun (idx, (name, _)) =>
      (subName ++ "_" ++ name, baseIdx + subSummary.functions.length + idx)
    acc ++ fnNames' ++ efNames
  ) []
  let externOffset := builtinOffset + builtinSigs.length
  let externNames : List (String × Nat) :=
    (enumerateList summary.externFnSigs).map fun (idx, (name, _)) => (name, externOffset + idx)
  -- Collect all impl block methods (pre-built + imported, then resolve Self)
  let localImplSigs := summary.implMethodSigs
  let allImplBlocks := imports.implBlocks ++ m.implBlocks
  let allTraitImpls := imports.traitImpls ++ m.traitImpls
  let implMethodSigs := resolveImplMethodSigs (imports.implMethodSigs ++ localImplSigs) allImplBlocks allTraitImpls
  let traitImplMethodSigs : List (String × FnSummary) := []
  let implSigList := (implMethodSigs ++ traitImplMethodSigs).map Prod.snd
  let implOffset := externOffset + externSigs.length
  let implNames : List (String × Nat) :=
    (enumerateList (implMethodSigs ++ traitImplMethodSigs)).map fun (idx, (name, _)) => (name, implOffset + idx)
  let allSigs := importedSigList ++ fnSigs ++ builtinSigs ++ externSigs ++ submoduleSigs ++ implSigList
  let importedNames : List (String × Nat) :=
    (enumerateList imports.functions).map fun (idx, (name, _)) => (name, idx)
  let fnNames : List (String × Nat) :=
    (enumerateList summary.functions).map fun (idx, (name, _)) => (name, baseOffset + idx)
  let allNames := importedNames ++ fnNames ++ builtinNames ++ externNames ++ submoduleNames ++ implNames
  let userFnNamesList := (importedNames ++ fnNames ++ externNames ++ submoduleNames ++ implNames).map Prod.fst
  -- Build named function signature map for fnRef resolution
  let fnSigPairs : List (String × FnSummary) :=
    summary.functions ++ (implMethodSigs ++ traitImplMethodSigs)
  let allStructs := imports.structs ++ m.structs
  -- Built-in Option<T> enum (Some { value: T }, None {})
  let builtinOptionEnum : EnumDef := {
    name := optionEnumName
    typeParams := ["T"]
    variants := [
      { name := "Some", fields := [{ name := "value", ty := .typeVar "T" }] },
      { name := "None", fields := [] }
    ]
    isCopy := false
    builtinId := some .option
  }
  let builtinResultEnum : EnumDef := {
    name := resultEnumName
    typeParams := ["T", "E"]
    variants := [
      { name := okVariantName, fields := [{ name := "value", ty := .typeVar "T" }] },
      { name := errVariantName, fields := [{ name := "error", ty := .typeVar "E" }] }
    ]
    isCopy := false
    builtinId := some .result
  }
  let hasUserResult := m.enums.any fun ed => ed.name == resultEnumName
  let builtinEnumList := [builtinOptionEnum] ++ (if hasUserResult then [] else [builtinResultEnum])
  let allEnums := builtinEnumList ++ imports.enums ++ m.enums
  -- Build type aliases map
  let localTypeAliases : List (String × Ty) := m.typeAliases.map fun ta => (ta.name, ta.targetTy)
  let typeAliasMap : List (String × Ty) := imports.typeAliases ++ localTypeAliases
  -- Build constants map
  let constantsMap : List (String × Ty) := m.constants.map fun c => (c.name, c.ty)
  -- Build trait impl pairs for bound checking
  let traitImplPairs : List (String × String) := allTraitImpls.map fun tb => (tb.typeName, tb.traitName)
  -- Collect newtypes from module, submodules, and imports.
  -- Imported newtypes participate in type identity (Port ≠ u16 must hold
  -- across module boundaries) and reach Layout via the elaborated CModule.
  let allNewtypes := m.newtypes ++ imports.newtypes
                     ++ m.submodules.foldl (fun acc sub => acc ++ sub.newtypes) []
  let initEnv : TypeEnv :=
    { vars := [], structs := allStructs, enums := allEnums, functions := allSigs,
      fnNames := allNames, loopDepth := 0, typeAliases := typeAliasMap, constants := constantsMap,
      traitImpls := traitImplPairs, allFnSummarys := fnSigPairs, newtypes := allNewtypes,
      userFnNames := userFnNamesList }
  -- Module-level declaration checks (Copy/Destroy, repr, FFI, traits) moved to CoreCheck.lean.
  -- Reserved name check stays here because reserved names conflict with builtins in Check.
  let reservedNameCheck := m.functions.foldl (init := (Except.ok () : Except Diagnostics Unit)) fun acc f =>
    match acc with
    | .error e => .error e
    | .ok () =>
      if isReservedFnName f.name then
        .error [{ severity := .error, message := CheckError.message (.reservedName f.name), pass := "check", span := none, hint := none }]
      else .ok ()
  match reservedNameCheck with
  | .error e => .error e
  | .ok () =>
  -- Built-in Destroy trait (needed for expression-level trait method resolution)
  let builtinDestroyTrait : TraitDef := {
    name := destroyTraitName
    methods := [{ name := destroyMethodName, params := [], retTy := .unit, selfKind := some .ref }]
    builtinId := some .destroy
  }
  let allTraits := builtinDestroyTrait :: m.traits
  -- Merge impl block type params into each method's typeParams, track impl type for Self
  -- Only check THIS module's impl/traitImpl methods (not imported ones whose bodies may
  -- reference functions not in scope here).
  let regularFns : List (FnDef × Option Ty) := m.functions.map fun f => (f, none)
  let implMethodPairs : List (FnDef × Option Ty) := m.implBlocks.foldl (fun acc ib =>
    let implTy := if ib.typeParams.isEmpty then tyFromName ib.typeName
                  else Ty.generic ib.typeName (ib.typeParams.map Ty.typeVar)
    acc ++ ib.methods.map fun f =>
      ({ f with typeParams := ib.typeParams ++ f.typeParams }, some implTy)
  ) []
  let traitImplMethodPairs : List (FnDef × Option Ty) := m.traitImpls.foldl (fun acc tb =>
    let implTy := if tb.typeParams.isEmpty then tyFromName tb.typeName
                  else Ty.generic tb.typeName (tb.typeParams.map Ty.typeVar)
    acc ++ tb.methods.map fun f =>
      ({ f with typeParams := tb.typeParams ++ f.typeParams }, some implTy)
  ) []
  let allFnPairs := regularFns ++ implMethodPairs ++ traitImplMethodPairs
  let (allErrors, _) := allFnPairs.foldl (fun (errs, env) (f, implTy) =>
    let env' := { env with currentImplType := implTy, traits := allTraits }
    let result := (checkFn f).run env' |>.run
    match result with
    | (.ok (), finalEnv) => (errs, finalEnv)
    | (.error ds, _) => (errs ++ ds.addContext s!"while checking function '{f.name}'", env)
  ) (([] : Diagnostics), initEnv)
  if allErrors.isEmpty then .ok ()
  else .error allErrors

/-- Check a multi-module program. Consumes resolved modules (proving name resolution
    happened) and uses summary table for import resolution. -/
def checkProgram (resolved : List ResolvedModule)
    (summaryTable : List (String × FileSummary) := []) : Except Diagnostics Unit :=
  -- Build sibling module summaries for inline modules (mod A {} mod B {}).
  -- Each module's summary is indexed by name for qualified :: access.
  let moduleSummaryList : List (String × FileSummary) := resolved.map fun rm =>
    let m := rm.module
    (m.name, match summaryTable.find? fun (n, _) => n == m.name with
      | some (_, s) => s
      | none => buildFileSummary m)
  let allErrors := resolved.foldl (fun errs rm =>
    let m := rm.module
    let summary := match moduleSummaryList.find? fun (n, _) => n == m.name with
      | some (_, s) => s
      | none => buildFileSummary m
    match resolveImports m.imports summaryTable
        (fun modName => CheckError.message (.unknownModule modName))
        (fun sym modName => CheckError.message (.notPublicInModule sym modName))
        (pass := "check") with
    | .error ds => errs ++ ds
    | .ok imports =>
      -- Inject sibling module functions for qualified :: access
      let siblingFns : List (String × FnSummary) := moduleSummaryList.foldl (fun acc (sibName, sibSummary) =>
        if sibName == m.name || sibName == "main" then acc
        else acc ++ (sibSummary.functions.filter fun (name, _) =>
          sibSummary.publicNames.contains name).map fun (name, fs) =>
            (sibName ++ "_" ++ name, fs)
      ) []
      let imports := { imports with functions := imports.functions ++ siblingFns }
      match checkModule m summary imports with
      | .ok () => errs
      | .error ds => errs ++ ds.addContext s!"while checking module '{m.name}'"
  ) ([] : Diagnostics)
  if allErrors.isEmpty then .ok () else .error allErrors

end Concrete
