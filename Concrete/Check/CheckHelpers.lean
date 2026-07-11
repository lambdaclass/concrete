import Concrete.Frontend.AST
import Concrete.Resolve.BuiltinSigs
import Concrete.Report.Diagnostic
import Concrete.Resolve.FileSummary
import Concrete.Resolve.Intrinsic
import Concrete.Resolve.Resolve
import Concrete.Resolve.Shared
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

def throwCheckMsg (msg : String) (span : Option Span := none) : CheckM α :=
  throw [{ severity := .error, message := msg, pass := "check", span := span, hint := none }]

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

/-- Must-use types whose silent discard is flagged (Phase 6 #13). The canonical
    fallible results — `Result<…>` and `Option<…>`, however spelled (`.named` when
    written without type arguments, `.generic` with them). A discarded statement
    expression of one of these types ignores a possible failure/absence, so it is
    an error unless explicitly acknowledged. -/
def mustUseEnumName? : Ty → Option String
  | .named n => if n == resultEnumName || n == optionEnumName then some n else none
  | .generic n _ => if n == resultEnumName || n == optionEnumName then some n else none
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

/-- Does this type contain a reference (`&T`/`&mut T`) anywhere? Used to enforce
    the "references are second-class — never returned" invariant
    (docs/VALUE_MODEL.md). Raw pointers (`*const`/`*mut`) are NOT references and
    are allowed in return position (they are the audit-visible unsafe escape). -/
partial def tyContainsRef : Ty → Bool
  | .ref _ | .refMut _ => true
  | .ptrMut inner | .ptrConst inner | .heap inner | .heapArray inner => tyContainsRef inner
  | .array elem _ => tyContainsRef elem
  | .generic _ args => args.any tyContainsRef
  | .fn_ params _ retTy => params.any tyContainsRef || tyContainsRef retTy
  | _ => false

/-- Does the named type parameter occur anywhere in this type? -/
partial def tyParamOccursIn (name : String) : Ty → Bool
  | .typeVar n => n == name
  | .named n => n == name
  | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner | .heap inner | .heapArray inner =>
    tyParamOccursIn name inner
  | .array elem _ => tyParamOccursIn name elem
  | .generic _ args => args.any (tyParamOccursIn name)
  | .fn_ params _ retTy => params.any (tyParamOccursIn name) || tyParamOccursIn name retTy
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
      -- The alias map is transitively pre-closed at build time (`closeAliasMap`),
      -- so this single lookup already yields the fully-expanded target
      -- (`type B = A; type A = i32` => B resolves straight to i32).
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
    -- References are second-class: a function TYPE may not return a reference,
    -- directly or nested (docs/VALUE_MODEL.md). This makes ref-returning
    -- callbacks unconstructable, which keeps scoped callbacks (`with_value`)
    -- sound — the callback cannot return the borrowed element it was handed.
    if tyContainsRef retTy' then
      throwCheckMsg s!"function type may not return a reference ('{tyToString retTy'}'); references are second-class and may not be returned (see VALUE_MODEL.md). Use a value, an owned view, or a scoped callback; for low-level access return a raw pointer (*const/*mut)."
    return .fn_ params' capSet retTy'
  | _ => return ty

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
def isCopyType (ty : Ty) : CheckM Bool := do
  let env ← getEnv
  return Layout.isCopyTyGeneric
    (fun name =>
      match env.structs.find? fun sd => sd.name == name with
      | some sd => some (sd.isCopy, sd.typeParams, sd.fields.map (fun f => f.ty))
      | none => match env.enums.find? fun ed => ed.name == name with
        | some ed => some (ed.isCopy, ed.typeParams, ed.variants.flatMap (fun v => v.fields.map (fun f => f.ty)))
        | none => none)
    (fun name => (env.newtypes.find? fun nt => nt.name == name).map (fun nt => nt.innerTy))
    (fun name => (((env.currentTypeBounds.find? fun b => b.1 == name).map Prod.snd).getD []).contains "Copy")
    ty

def lookupVarInfo (name : String) : CheckM (Option VarInfo) := do
  let env ← getEnv
  return env.vars.lookup name

def lookupVarTy (name : String) : CheckM (Option Ty) := do
  match ← lookupVarInfo name with
  | some info => return some info.ty
  | none => return none

def addVar (name : String) (ty : Ty) (mutable : Bool := true) (declSpan : Option Span := none) : CheckM Unit := do
  let env ← getEnv
  let copy ← isCopyType ty
  let info : VarInfo := { ty, state := .unconsumed, isCopy := copy, loopDepth := env.loopDepth, mutable, declSpan }
  let env ← getEnv
  setEnv { env with vars := (name, info) :: env.vars }

/-- Look up the LAST (outermost) binding with this name. Branch/arm envs
    PREPEND their locals, so a shadowing local — e.g. a nested match's
    field-named `value` binding over an outer `value` — must not be mistaken
    for the pre-existing variable during consumption merges. -/
def lookupOutermost (vars : List (String × VarInfo)) (name : String) : Option VarInfo :=
  vars.foldl (fun acc (n, vi) => if n == name then some vi else acc) none

def activeBorrowRefs (env : TypeEnv) (varName : String) : List VarInfo :=
  env.vars.foldl (fun acc (_, info) =>
    match info.borrowedFrom with
    | some sourceName =>
      if sourceName == varName && info.state != .consumed then info :: acc else acc
    | none => acc) []

partial def tyContainsTypeVar : Ty → Bool
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
        if n == name then
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
def consumeVar (name : String) (span : Option Span := none) (breakDepthExempt : Nat := 0) : CheckM Unit := do
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
          && env.rebindingVar != some name then
        throwCheck (.cannotConsumeLinearInLoop name) span
      -- Mark consumed
      let vars' := env.vars.map fun (n, vi) =>
        if n == name then (n, { vi with state := .consumed, movedAt := span })
        else (n, vi)
      setEnv { env with vars := vars' }

/-- Consume a variable if it exists. Skips function names (not in var scope). -/
def consumeVarIfExists (name : String) (span : Option Span := none) (breakDepthExempt : Nat := 0) : CheckM Unit := do
  match ← lookupVarInfo name with
  | some _ => consumeVar name span breakDepthExempt
  | none => pure ()  -- function reference, not a variable

/-- Check that all tracked linear variables in the given name list are consumed.
    `reserved` is allowed because the deferred destroy will run at scope exit. -/
def checkScopeExit (varNames : List String) (span : Option Span := none) : CheckM Unit := do
  let env ← getEnv
  for name in varNames do
    match env.vars.lookup name with
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

-- Does a `break` appear directly in this block (not inside a nested loop, whose
-- break targets that inner loop)? Distinguishes a truly infinite `while true {}`
-- from one that can exit. Labeled breaks to an outer loop are treated as inner
-- (conservative: at worst we under-report a leak, never false-positive).
mutual
partial def blockHasBreak (stmts : List Stmt) : Bool :=
  stmts.any stmtHasBreak
partial def stmtHasBreak : Stmt → Bool
  | .break_ _ _ _ => true
  | .ifElse _ _ t e => blockHasBreak t || (match e with | some es => blockHasBreak es | none => false)
  | .while_ _ _ _ _ | .forLoop _ _ _ _ _ _ => false  -- break here targets the inner loop
  | _ => false
end

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
  | .while_ _ cond body _ => isLitTrueExpr cond && !blockHasBreak body
  | .ifElse _ _ t (some e) => blockDiverges t && blockDiverges e
  | .expr _ e _ => exprDiverges e
  | _ => false
partial def exprDiverges : Expr → Bool
  | .paren _ inner => exprDiverges inner
  | .call _ fnName _ _ => resolveIntrinsic fnName == some .abort
  | .whileExpr _ cond _ _ => isLitTrueExpr cond
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
  | .while_ _ cond body _ => isLitTrueExpr cond && !blockHasBreak body
  | .ifElse _ _ t (some e) => blockNonTerminating t && blockNonTerminating e
  | .expr _ e _ => exprNonTerminating e
  | _ => false
partial def exprNonTerminating : Expr → Bool
  | .paren _ inner => exprNonTerminating inner
  | .call _ fnName _ _ => resolveIntrinsic fnName == some .abort
  | .whileExpr _ cond _ _ => isLitTrueExpr cond
  | .match_ _ _ arms => !arms.isEmpty && arms.all armNonTerminating
  | _ => false
partial def armNonTerminating : MatchArm → Bool
  | .mk _ _ _ _ _ body => blockNonTerminating body
  | .litArm _ _ _ body => blockNonTerminating body
  | .varArm _ _ _ body => blockNonTerminating body
  | .rangeArm _ _ _ _ _ body => blockNonTerminating body
end

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

/-- A branch or arm that EXITS THE FUNCTION must have consumed every linear
    value that was live when it started — `if c { return 0; } … drop(s);`
    leaks `s` on the returning path even though the fall-through is fine.
    Enforced wherever a diverging branch is exempted from the consumption
    MERGE: the exemption is about agreement at the merge point, not about the
    exit itself. -/
def checkReturnPathConsumed (before after : List (String × VarInfo))
    (span : Option Span := none) : CheckM Unit := do
  for (name, infoBefore) in before do
    if infoBefore.isCopy then continue
    -- References are BORROWS: letting one go out of scope on a return path is
    -- not a leak (&mut is non-Copy only for aliasing exclusivity).
    match infoBefore.ty with
    | .ref _ | .refMut _ => continue
    | _ => pure ()
    if infoBefore.state == .consumed || infoBefore.state == .reserved then continue
    let afterState := match lookupOutermost after name with
      | some info => info.state
      | none => infoBefore.state
    -- `.consumed` = moved on this path; `.reserved` = a pending `defer` will
    -- consume it when this return unwinds — both are accounted for.
    if afterState != .consumed && afterState != .reserved then
      throwCheck (.linearVariableNeverConsumed name) span

/-- KNOWN_HOLES H9: a non-Copy (linear) value DECLARED inside a block — present in
    `after`, absent from `before` — must be consumed before the block exits. Skipped
    when the block diverges (its textual end is unreachable). This is what the
    function-level `checkScopeExit` could not see for `if`/`else` branch locals and
    matched payload bindings, which are dropped at the branch/arm merge. -/
def checkBlockLocalsConsumed
    (before after : List (String × VarInfo))
    (diverges : Bool)
    (span : Option Span := none) : CheckM Unit := do
  if diverges then return ()
  for (name, info) in after do
    if before.any (fun (n, _) => n == name) then continue  -- pre-existing, not block-local
    if info.isCopy then continue
    if info.state == .consumed || info.state == .reserved then continue
    throwCheck (.linearVariableNeverConsumed name) (info.declSpan.orElse (fun _ => span))

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
  | .structLit _ name typeArgs _ _ =>
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
  -- Borrows must carry the reference wrapper so generic inference can unify
  -- `&T` against `&i64` (ROADMAP Phase 5 #6b). Without these, `peekExprType
  -- (&w)` fell through to `.placeholder` and `id(&w)` could not infer `T`,
  -- forcing an explicit turbofish.
  | .borrow _ inner => return .ref (← peekExprType inner)
  | .borrowMut _ inner => return .refMut (← peekExprType inner)
  | .deref _ inner =>
    match ← peekExprType inner with
    | .ref t | .refMut t | .ptrMut t | .ptrConst t | .heap t => return t
    | _ => return .placeholder
  | _ => return .placeholder

/-- Unify a pattern type with an actual type to discover type variable bindings. -/
partial def unifyTypes (pattern actual : Ty) (typeParams : List String) : List (String × Ty) :=
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
      if (← lookupFn (destroyFnNameFor n)).isSome then return true
      match ← lookupStruct n with
      | some sd => sd.fields.anyM (fun f => ownsResource fuel f.ty)
      | none => match ← lookupEnum n with
        | some ed => ed.variants.anyM (fun v => v.fields.anyM (fun f => ownsResource fuel f.ty))
        | none => return false
    | .generic n args =>
      if n == "Vec" || n == "HashMap" || n == "HashSet" || n == "Heap" || n == "HeapArray" then
        return true
      if (← lookupFn (destroyFnNameFor n)).isSome then return true
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
partial def checkTraitBounds (bounds : List (String × List String)) (mapping : List (String × Ty))
    (context : String) : CheckM Unit := do
  let env ← getEnv
  for (paramName, requiredTraits) in bounds do
    match mapping.lookup paramName with
    | some concreteType0 =>
      -- A turbofish arg naming one of the CALLER's own type params arrives as
      -- `.named "T"` — normalize to `.typeVar` so the caller's bounds are
      -- consulted (`fib::<T>(n - 1)` inside `fn fib<T: Copy>` satisfies Copy).
      let concreteType := match concreteType0 with
        | .named n => if env.currentTypeParams.contains n then .typeVar n else concreteType0
        | t => t
      for traitName in requiredTraits do
        if traitName == "Copy" then
          if !(← isCopyType concreteType) then
            let tn := match concreteType with | .named n => n | .generic n _ => n | .typeVar n => n | _ => "<type>"
            throwCheck (.traitBoundNotSatisfied tn "Copy" context)
        else
          match concreteType with
          | .named tn | .generic tn _ =>
            if !(env.traitImpls.any fun (t, tr) => t == tn && tr == traitName) then
              throwCheck (.traitBoundNotSatisfied tn traitName context)
          | .typeVar n =>
            -- caller's own type param: satisfied if the caller declares the
            -- same trait bound on it
            let callerBounds := (env.currentTypeBounds.find? fun (bn, _) => bn == n).map Prod.snd |>.getD []
            if !callerBounds.contains traitName then
              throwCheck (.traitBoundNotSatisfied n traitName context)
          | _ => pure ()  -- primitive types, skip non-Copy bound checking
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
          if !(inferred.any fun (n, _) => n == name) then
            inferred := inferred ++ [(name, ty)]
      pure (methodTypeParams.map fun tp => (inferred.lookup tp).getD (.typeVar tp))
  let fullMapping := implMapping ++ methodTypeParams.zip methodArgs
  -- Enforce the impl's + method's trait/Copy bounds at the call site (the impl's
  -- bounds were prepended to sig.typeBounds). This makes e.g. a `Copy`-bounded
  -- value accessor uncallable on a non-Copy container — closing the
  -- decorative-impl-bound soundness gap.
  if !sig.typeBounds.isEmpty then
    checkTraitBounds sig.typeBounds fullMapping s!"method '{callName}'"
  -- References are second-class: a type parameter instantiated to a reference
  -- may not occur in the return type (else `m<R>(...) -> Option<R>` with R=&V is
  -- a generic backdoor to returning a reference). VALUE_MODEL.md.
  for (pName, pTy) in fullMapping do
    if tyContainsRef pTy && tyParamOccursIn pName sig.retTy then
      throwCheckMsg s!"method '{callName}': type parameter '{pName}' is instantiated to a reference ('{tyToString pTy}') and occurs in the return type; references may not be returned (VALUE_MODEL.md). Return a value, an owned view, or use a scoped callback."
  -- 2. Infer capability-variable bindings from fn-typed arguments.
  let mut capBindings : List (String × List String) := []
  if !sig.capParams.isEmpty then
    for (arg, (_, pTy)) in args.zip methodParamTys do
      match pTy with
      | .fn_ _ (.concrete caps) _ =>
        for cap in caps do
          if sig.capParams.contains cap then
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
  -- 3. Resolve the method's declared capset against the inferred bindings, and
  --    check the caller holds the result.
  if !sig.capParams.isEmpty then
    let (concreteCaps, capVars) := sig.capSet.normalize
    let mut resolvedCaps : List String := []
    for cap in concreteCaps do
      if sig.capParams.contains cap then
        match capBindings.find? fun (n, _) => n == cap with
        | some (_, caps) => resolvedCaps := resolvedCaps ++ caps
        | none => throwCheck (.cannotInferCapVariable cap callName) (some sp)
      else resolvedCaps := resolvedCaps ++ [cap]
    for cv in capVars do
      match capBindings.find? fun (n, _) => n == cv with
      | some (_, caps) => resolvedCaps := resolvedCaps ++ caps
      | none => throwCheck (.cannotInferCapVariable cv callName) (some sp)
    let env ← getEnv
    let (callerCaps, callerVars) := env.currentCapSet.normalize
    for cap in resolvedCaps do
      unless callerCaps.contains cap || callerVars.contains cap do
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
  let resolvedParamTys := (sig.params.drop 1).map fun (n, t) => (n, resolveCapInTy (substTy fullMapping t))
  let resolvedRetTy := resolveCapInTy (substTy fullMapping sig.retTy)
  return (resolvedParamTys, resolvedRetTy)

-- ============================================================
-- Type checking expressions and statements
-- ============================================================

/-- A borrow PATH: root variable + projection steps (field names; an array
    index is the wildcard step "[]" — two indexings of one root are treated
    as overlapping since indices are not statically comparable). -/
partial def borrowPathOf : Expr → Option (String × List String)
  | .ident _ n => some (n, [])
  | .fieldAccess _ obj f =>
    (borrowPathOf obj).map fun (r, steps) => (r, steps ++ [f])
  | .arrayIndex _ arr _ =>
    (borrowPathOf arr).map fun (r, steps) => (r, steps ++ ["[]"])
  | .paren _ inner => borrowPathOf inner
  | _ => none

/-- Path `a` overlaps path `b` iff one is a prefix of the other (equal roots).
    Disjoint fields (`w.a` vs `w.b`) do NOT overlap — precise, no over-reject;
    `w` vs `w.f` DO (the whole overlaps the part). -/
def pathsOverlap (a b : String × List String) : Bool :=
  a.1 == b.1 && (a.2.isPrefixOf b.2 || b.2.isPrefixOf a.2)

/-- #18 container-not-in-context: within one call's arguments (plus the
    auto-borrowed method receiver), two borrows may not OVERLAP when either
    is `&mut` (E0293). Covers `&x`/`&mut x`, projections (`&mut w.f` twice,
    or `&mut w` + `&mut w.f`), and single-hop ALIASES: an ident argument
    whose binding is a ref/&mut with a tracked `borrowedFrom` root counts as
    a borrow of that root. Shared+shared overlap is fine. -/
def checkCallBorrowConflicts (parts : List ((String × List String) × Bool)) (span : Option Span) : CheckM Unit := do
  let mut seen : List ((String × List String) × Bool) := []
  for (path, isMut) in parts do
    for (prevPath, prevMut) in seen do
      if pathsOverlap path prevPath && (isMut || prevMut) then
        throwCheck (.conflictingCallBorrows path.1) span
    seen := (path, isMut) :: seen

/-- Collect (path, isMut) pairs from syntactic borrow arguments AND from
    ident arguments that are themselves live borrows (alias-through-binding:
    `let r = &mut c; c.scoped(r, cb)` — `r` counts as a borrow of `c`). -/
def borrowArgParts (args : List Expr) : CheckM (List ((String × List String) × Bool)) := do
  let mut out : List ((String × List String) × Bool) := []
  for a in args do
    match a with
    | .borrow _ inner =>
      match borrowPathOf inner with
      | some p => out := out ++ [(p, false)]
      | none => pure ()
    | .borrowMut _ inner =>
      match borrowPathOf inner with
      | some p => out := out ++ [(p, true)]
      | none => pure ()
    | .ident _ n =>
      match ← lookupVarInfo n with
      | some info =>
        match info.borrowedFrom with
        | some root =>
          let isMut := match info.ty with | .refMut _ => true | _ => false
          out := out ++ [((root, []), isMut)]
        | none => pure ()
      | none => pure ()
    | _ => pure ()
  return out

end Concrete
