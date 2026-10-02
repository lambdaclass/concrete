import Concrete.Frontend.AST
import Concrete.Proof.BodyScope
import Concrete.Proof.EvidenceTree
import Concrete.Proof.EvidenceBuild
-- for `flatUsesOf`: the flat identity-use view is DERIVED from the structural body here,
-- so elaboration has exactly one producer of it.
import Concrete.Proof.IdentityUseBytes
import Concrete.Resolve.BuiltinSigs
import Concrete.Elab.Core
import Concrete.Report.Diagnostic
import Concrete.Resolve.FileSummary
import Concrete.Resolve.Intrinsic
import Concrete.Resolve.BuiltinEnums
import Concrete.Resolve.TypeId
import Concrete.Resolve.Resolve
import Concrete.Resolve.Shared
import Concrete.Semantics.TypeJudgment

namespace Concrete

/-! ## Elaboration: surface AST → Core IR

Type-annotates and desugars the surface AST into Core IR.
No linearity checking, no borrow checking, no capability validation.
-/

-- ============================================================
-- Elaboration environment
-- ============================================================

structure ElabEnv where
  vars : List (String × Ty)
  structs : List StructDef
  enums : List EnumDef
  fnSigs : List (String × FnSummary)
  typeAliases : List (String × Ty)
  constants : List (String × Ty)
  currentTypeParams : List String := []
  currentTypeBounds : List (String × List String) := []
  currentRetTy : Ty := .unit
  currentImplType : Option Ty := none
  traits : List TraitDef := []
  allFnSigPairs : List (String × FnSummary) := []
  newtypes : List NewtypeDef := []
  -- Names bound by `ghost let`. Tracked but NOT in `vars`: ghost bindings are
  -- erased before Core, so a runtime expression referencing one resolves to
  -- nothing — we report it as a ghost-in-runtime leak rather than a generic
  -- undeclared variable.
  ghostVars : List String := []
  -- Lexical frame stack for ProofBodyCanonicalV2. PARALLEL to `vars`, and
  -- deliberately not derived from it: `vars` is a flat list whose indices shift as
  -- branches and loops push and pop, so a position read off it is not a stable
  -- lexical identity. `restoreScope` restores this from the saved env, which gives
  -- frame POP for free and matches the language's own scoping.
  bodyScope : Proof.BodyScope := {}
  -- The declaration's own proof path, needed to MINT identities inside expression
  -- elaboration. `thisProofPath` is computed in elabModule, so without threading it
  -- here a constant or function reference has no defining module and cannot become a
  -- ConstId or CallableId — it would have to be emitted as a gap purely for want of
  -- context it could have had.
  proofPath : String := ""
  -- Callee identity, as an OUTPUT of the resolution that also selects runtime
  -- behaviour. Not a second lookup: two lookups can be handed different spellings or
  -- observe different table state and diverge while both claim to share a precedence
  -- rule. Installed at ElabEnv CONSTRUCTION, so there is no window in which this
  -- silently answers `none` and callers emit gaps for a reason that is not real.
  resolveCallee : String → Option CallableId := fun _ => none
  -- Enclosing loops, INNERMOST FIRST, each with its optional label. `break`/`continue`
  -- evidence carries a RELATIVE target (0 = innermost), never a label string: renaming
  -- a label must not move a digest. Restored by restoreScope with the rest of the
  -- lexical state, so exiting a loop pops its frame for free.
  loopFrames : List (Option String) := []
  -- A scope has been ENTERED but no frame materialized yet. Frames open LAZILY on
  -- the first binder, so a scope that binds nothing does not shift `framesOut` for
  -- references inside it — an empty `if` must not move a digest.
  pendingFrame : Bool := false
  -- Bug 045: match payload binders are ALPHA-RENAMED to unique Core names
  -- (`value` → `value.b7`). Surface identifiers cannot contain '.', so the
  -- fresh names never collide with user names. Without this, every stage
  -- below Elab (Interp, Lower, CoreCheck) keyed bindings by the bare
  -- surface name in a function-flat table: a nested same-named binder
  -- silently CLOBBERED the outer one on both backends (rc=4 shadow probe),
  -- invisible to differential testing because interp and compiled agreed.
  -- `renames` maps surface name → current Core name (prepend = shadow);
  -- both this list and `vars` are snapshot-restored around each arm.
  renames : List (String × String) := []
  freshBinder : Nat := 0
  /-- Did every identity in the current body resolve? `false` records a normal language
      resolution whose EVIDENCE identity could not be produced, so the subject fails
      closed instead of describing less than the program does.

      There is no parallel list of uses beside this flag: the uses are the structural
      evidence body, and the flat view is derived from it. -/
  bodyIdentityCovered : Bool := true

abbrev ElabM := ExceptT Diagnostics (StateM ElabEnv)

inductive ElabError where
  -- Name resolution
  | selfOutsideImpl
  | undeclaredVariable (name : String)
  | undeclaredFunction (name : String)
  | unknownFunctionRef (name : String)
  | assignToUndeclaredVariable (name : String)
  | borrowUndeclaredVariable (name : String)
  | ghostInRuntime (name : String)
  -- Struct/field
  | unknownStructType (name : String)
  | structHasNoField (structName : String) (fieldName : String)
  | fieldAccessNonStruct
  -- Enum/variant
  | unknownEnumType (name : String)
  | unknownVariant (variant : String) (enumName : String)
  | missingFieldInVariant (fieldName : String) (enumName : String) (variant : String)
  -- Method resolution
  | noMethodOnTypeVar (method : String) (typeVar : String)
  | noMethodOnType (method : String) (typeName : String)
  | methodCallOnNonNamedType
  -- Validation
  | arrayLiteralEmpty
  -- Module/import
  | inSubmodule (subName : String) (innerError : String)
  | unknownModule (name : String)
  | notPublicInModule (symbol : String) (moduleName : String)

def ElabError.message : ElabError → String
  | .selfOutsideImpl => "Self can only be used inside impl blocks"
  | .undeclaredVariable name => s!"use of undeclared variable '{name}'"
  | .undeclaredFunction name => s!"call to undeclared function '{name}'"
  | .unknownFunctionRef name => s!"unknown function '{name}' in function reference"
  | .assignToUndeclaredVariable name => s!"assignment to undeclared variable '{name}'"
  | .borrowUndeclaredVariable name => s!"borrow: undeclared variable '{name}'"
  | .ghostInRuntime name => s!"ghost value '{name}' cannot be used in runtime code"
  | .unknownStructType name => s!"unknown struct type '{name}'"
  | .structHasNoField structName fieldName => s!"struct '{structName}' has no field '{fieldName}'"
  | .fieldAccessNonStruct => "field access on non-struct type"
  | .unknownEnumType name => s!"unknown enum type '{name}'"
  | .unknownVariant variant enumName => s!"unknown variant '{variant}' in enum '{enumName}'"
  | .missingFieldInVariant fieldName enumName variant => s!"missing field '{fieldName}' in {enumName}::{variant}"
  | .noMethodOnTypeVar method typeVar => s!"no method '{method}' for type variable '{typeVar}'"
  | .noMethodOnType method typeName => s!"no method '{method}' on type '{typeName}'"
  | .methodCallOnNonNamedType => "method call on non-named type"
  | .arrayLiteralEmpty => "array literal cannot be empty"
  | .inSubmodule subName innerError => s!"in submodule '{subName}': {innerError}"
  | .unknownModule name => s!"unknown module '{name}'"
  | .notPublicInModule symbol moduleName => s!"'{symbol}' is not public in module '{moduleName}'"

def ElabError.hint : ElabError → Option String
  | .fieldAccessNonStruct => some "field access requires a struct type"
  | .arrayLiteralEmpty => some "provide at least one element"
  | .methodCallOnNonNamedType => some "method calls require a named type"
  | .ghostInRuntime _ => some "ghost bindings are proof-only and erased before codegen; reference them only in contracts (#[ensures]/#[invariant]) or other ghost code"
  | _ => none

def ElabError.code : ElabError → String
  | .selfOutsideImpl => "E0400"
  | .undeclaredVariable _ => "E0401"
  | .undeclaredFunction _ => "E0402"
  | .unknownFunctionRef _ => "E0403"
  | .assignToUndeclaredVariable _ => "E0404"
  | .borrowUndeclaredVariable _ => "E0405"
  | .ghostInRuntime _ => "E0420"
  | .unknownStructType _ => "E0406"
  | .structHasNoField _ _ => "E0408"
  | .fieldAccessNonStruct => "E0409"
  | .unknownEnumType _ => "E0410"
  | .unknownVariant _ _ => "E0411"
  | .missingFieldInVariant _ _ _ => "E0412"
  | .noMethodOnTypeVar _ _ => "E0413"
  | .noMethodOnType _ _ => "E0414"
  | .methodCallOnNonNamedType => "E0415"
  | .arrayLiteralEmpty => "E0416"
  | .inSubmodule _ _ => "E0417"
  | .unknownModule _ => "E0418"
  | .notPublicInModule _ _ => "E0419"

def throwElab (e : ElabError) (span : Option Span := none) : ElabM α :=
  throw [{ severity := .error, message := e.message, pass := "elab", span := span, hint := e.hint, code := e.code }]

private def getEnv : ElabM ElabEnv := get
private def setEnv (env : ElabEnv) : ElabM Unit := set env

/-! ### Coverage, not collection

There is ONE producer of identity uses: the structural evidence body, from which the flat
view is derived in `elabFn`. What remains here is the COVERAGE axis — a separate fact, and
the reason these helpers did not disappear with the accumulator.

Coverage answers "did every identity in this body resolve?", which the use list cannot: an
unresolved identity contributes NOTHING to the list, so a body missing an identity and a
body that never mentioned one produce the same uses. Only an explicit mark distinguishes
them, and the distinction is what makes the subject fail closed instead of quietly
describing less than the program does. -/

private def markBodyIdentityUncovered : ElabM Unit := do
  let env ← getEnv
  setEnv { env with bodyIdentityCovered := false }

/-- A struct type must have a resolved `TypeId`, or the body is uncovered. The identity
    itself reaches evidence through the structural node. -/
private def recordStructTypeUse (sd : StructDef) : ElabM Unit :=
  match sd.typeId? with
  | some _ => pure ()
  | none => markBodyIdentityUncovered

/-- A field's OWNER must be resolved: an owner-less field name is a spelling, and two
    same-spelled fields on different types would share one identity. -/
private def recordFieldUse (sd : StructDef) (_field : String) : ElabM Unit :=
  match sd.typeId? with
  | some _ => pure ()
  | none => markBodyIdentityUncovered

/-- The nominal-type resolver handed to `evTypeRef`: a declared type name to its
    compiler-owned `TypeId`. Structs and enums are searched together because the surface
    has one type namespace, and a name that resolves in neither is `none` — which the
    builder turns into a gap rather than a spelling. -/
private def nominalTypeId? (env : ElabEnv) (n : String) : Option TypeId :=
  match (env.structs.find? fun sd => sd.name == n).bind StructDef.typeId? with
  | some id => some id
  | none => (env.enums.find? fun ed => ed.name == n).bind EnumDef.typeId?

/-- A surface type as evidence, resolved against the CURRENT declaration's type
    parameters so a type variable is a binder position and not a spelling. -/
private def typeRefOf (ty : Ty) : ElabM Proof.EvidenceTypeRef := do
  let env ← getEnv
  pure (Proof.evTypeRef (nominalTypeId? env) env.currentTypeParams ty)

/-- Same rule for a variant's owning enum. -/
private def recordVariantUse (ed : EnumDef) (_variant : String) : ElabM Unit :=
  match ed.typeId? with
  | some _ => pure ()
  | none => markBodyIdentityUncovered

-- ============================================================
-- Helpers
-- ============================================================

private def isIntLit : Expr → Bool
  | .intLit _ _ => true
  | .paren _ inner => isIntLit inner
  | _ => false

private def isPointerType : Ty → Bool
  | .ptrMut _ | .ptrConst _ => true
  | _ => false

/-- Pure newtype erasure: resolve any `.named` / `.generic` whose name matches a newtype
    to its (possibly substituted) inner type. Used when building Core struct/enum definitions
    so that layout, copy-checking, and lowering see the erased type. -/
private partial def eraseNewtypeTy (newtypes : List NewtypeDef) : Ty → Ty
  | .named name =>
    match newtypes.find? fun nt => nt.name == name with
    | some nt => eraseNewtypeTy newtypes nt.innerTy
    | none => .named name
  | .generic name args caps =>
    let args' := args.map (eraseNewtypeTy newtypes)
    match newtypes.find? fun nt => nt.name == name with
    | some nt =>
      let mapping := nt.typeParams.zip args'
      let rec subst : Ty → Ty
        | .named n => match mapping.lookup n with | some t => t | none => .named n
        | .typeVar n => match mapping.lookup n with | some t => t | none => .typeVar n
        | .ref i => .ref (subst i)
        | .refMut i => .refMut (subst i)
        | .ptrMut i => .ptrMut (subst i)
        | .ptrConst i => .ptrConst (subst i)
        | .array e n => .array (subst e) n
        | .generic n as caps => .generic n (as.map subst) caps
        | .fn_ ps c r => .fn_ (ps.map subst) c (subst r)
        | t => t
      eraseNewtypeTy newtypes (subst nt.innerTy)
    | none => .generic name args' caps
  | .ref inner => .ref (eraseNewtypeTy newtypes inner)
  | .refMut inner => .refMut (eraseNewtypeTy newtypes inner)
  | .ptrMut inner => .ptrMut (eraseNewtypeTy newtypes inner)
  | .ptrConst inner => .ptrConst (eraseNewtypeTy newtypes inner)
  | .array elem n => .array (eraseNewtypeTy newtypes elem) n
  | .fn_ ps c r => .fn_ (ps.map (eraseNewtypeTy newtypes)) c (eraseNewtypeTy newtypes r)
  | t => t

/-- Substitute type variables in a type. -/
private def substTy (mapping : List (String × Ty)) : Ty → Ty
  | .named name => match mapping.lookup name with | some t => t | none => .named name
  | .typeVar name => match mapping.lookup name with | some t => t | none => .typeVar name
  | .ref inner => .ref (substTy mapping inner)
  | .refMut inner => .refMut (substTy mapping inner)
  | .ptrMut inner => .ptrMut (substTy mapping inner)
  | .ptrConst inner => .ptrConst (substTy mapping inner)
  | .array elem n => .array (substTy mapping elem) n
  | .generic name args caps => .generic name (args.map (substTy mapping)) caps
  | .fn_ params capSet retTy => .fn_ (params.map (substTy mapping)) capSet (substTy mapping retTy)
  | .heap inner => .heap (substTy mapping inner)
  | .heapArray inner => .heapArray (substTy mapping inner)
  | ty => ty

private partial def resolveTypeE (ty : Ty) : ElabM Ty := do
  match ty with
  | .named name =>
    let env ← getEnv
    if name == selfTypeName then
      match env.currentImplType with
      | some t => return t
      | none => throwElab .selfOutsideImpl
    else if env.currentTypeParams.contains name then return .typeVar name
    else
      match env.typeAliases.lookup name with
      -- Alias map is transitively pre-closed at build time (`closeAliasMap`).
      | some resolved => return resolved
      | none =>
        -- Newtypes are NOT erased here: type identity is preserved through
        -- elaboration so `p.value()` on `p: Port` resolves against `Port`'s
        -- inherent impl, not the inner `u16`. Layout resolves through
        -- newtypes natively (Layout.Ctx.newtypes), so codegen still sees
        -- the right size/alignment. eraseNewtypeTy is still applied at
        -- module-build time to struct/enum field types so CoreCheck's
        -- Copy/repr invariants run on the inner type as before.
        return ty
  | .ref inner => return .ref (← resolveTypeE inner)
  | .refMut inner => return .refMut (← resolveTypeE inner)
  | .ptrMut inner => return .ptrMut (← resolveTypeE inner)
  | .ptrConst inner => return .ptrConst (← resolveTypeE inner)
  | .array elem n => return .array (← resolveTypeE elem) n
  | .generic "Heap" [inner] _ => return .heap (← resolveTypeE inner)
  | .generic "HeapArray" [inner] _ => return .heapArray (← resolveTypeE inner)
  | .generic name args caps =>
    -- Same: newtype generics survive here so method dispatch on
    -- e.g. `Wrapper<T>` instances reaches `Wrapper`'s inherent impls.
    return .generic name (← args.mapM resolveTypeE) caps
  | .fn_ params capSet retTy =>
    return .fn_ (← params.mapM resolveTypeE) capSet (← resolveTypeE retTy)
  | _ => return ty

private def lookupVar (name : String) : ElabM (Option Ty) := do
  let env ← getEnv
  return env.vars.lookup name

private def lookupFnSig (name : String) : ElabM (Option FnSummary) := do
  let env ← getEnv
  return (env.fnSigs.find? fun (n, _) => n == name).map Prod.snd

private def lookupStruct (name : String) : ElabM (Option StructDef) := do
  let env ← getEnv
  return env.structs.find? fun sd => sd.name == name

private def lookupEnum (name : String) : ElabM (Option EnumDef) := do
  let env ← getEnv
  return env.enums.find? fun ed => ed.name == name

/-- Record `name` as a binder. `pending` means a scope was entered and its frame has
    not been materialized: open it now, with this binder first. Otherwise extend the
    innermost live frame in source order. -/
private def extendFrame (sc : Proof.BodyScope) (pending : Bool) (name : String)
    : Proof.BodyScope :=
  if pending then sc.push [name]
  else match sc.frames with
    | []      => sc.push [name]
    | f :: fs => { frames := (f.concat name) :: fs }

private def addBinder (name : String) : ElabM Unit := do
  let env ← getEnv
  setEnv { env with bodyScope := extendFrame env.bodyScope env.pendingFrame name
                    pendingFrame := false }

/-- Mark a scope as entered. Idempotent, and cheap: nothing is allocated until a
    binder actually arrives. -/
private def openScope : ElabM Unit := do
  let env ← getEnv
  setEnv { env with pendingFrame := true }

private def addVar (name : String) (ty : Ty) : ElabM Unit := do
  addBinder name
  let env ← getEnv
  setEnv { env with vars := (name, ty) :: env.vars }

/-- Bug 045: bind a match-arm payload/pattern variable under a FRESH Core
    name. Registers the type under the surface name (ident type lookup is
    surface-keyed) and pushes surface→fresh into `renames` (prepend =
    innermost shadows). Returns the Core name to put in the arm's binding
    list. `_` stays `_`. -/
private def bindArmVar (binding : String) (ty : Ty) : ElabM String := do
  if binding == "_" then return binding
  let env ← getEnv
  let fresh := s!"{binding}.b{env.freshBinder}"
  setEnv { env with
    vars := (binding, ty) :: env.vars
    bodyScope := extendFrame env.bodyScope env.pendingFrame binding
    pendingFrame := false
    renames := (binding, fresh) :: env.renames
    freshBinder := env.freshBinder + 1 }
  return fresh

/-- Current Core name for a surface identifier (identity unless an
    enclosing match arm renamed it). -/
private def coreNameOf (name : String) : ElabM String := do
  let env ← getEnv
  return (env.renames.lookup name).getD name

/-- Scope restore that keeps `freshBinder` MONOTONE: sibling and
    sequential arms must never mint the same Core name (Lower's slot table
    is function-flat, which is the whole reason bug 045 existed). -/
private def restoreScope (saved : ElabEnv) : ElabM Unit := do
  let cur ← getEnv
  -- Coverage is an elaboration verdict, not lexical scope: an identity that failed to
  -- resolve inside an arm must stay unresolved after the arm closes. Restore it
  -- alongside the fresh-name counter, or a branch could launder its own gap.
  setEnv { saved with
    freshBinder := cur.freshBinder
    bodyIdentityCovered := cur.bodyIdentityCovered }

private def addGhostVar (name : String) : ElabM Unit := do
  let env ← getEnv
  setEnv { env with ghostVars := name :: env.ghostVars }



-- Type-arg inference is single-sourced in `Shared.unifyTypes` (Phase 6.5
-- InstantiationJudgment axis); this pass and Check share that one algorithm.

/-- Peek at an expression's type without any side effects, for type inference. -/
private partial def peekExprType (e : Expr) : ElabM Ty := do
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
    | some ty => return ty
    | none =>
      match ← lookupFnSig name with
      | some sig =>
        let paramTys := sig.params.map Prod.snd
        return .fn_ paramTys sig.capSet sig.retTy
      | none => return .placeholder
  | .structLit _ name typeArgs fields _ =>
    -- R-0484: capability arguments read off the fields' peeked types (same rule as Check).
    match ← lookupStruct name with
    | some sd =>
      if sd.capParams.isEmpty then
        if typeArgs.isEmpty then return .named name else return .generic name typeArgs []
      else
        let mapping := sd.typeParams.zip typeArgs
        let mut capMap : List (String × CapSet) := []
        for sf in sd.fields do
          if (tyCapNames sf.ty).any sd.capParams.contains then
            match fields.find? fun (fn, _) => fn == sf.name with
            | some (_, fe) =>
              let fty ← peekExprType fe
              for (k, v) in inferCapBindings sd.capParams (substTy mapping sf.ty) fty do
                if !(capMap.any (·.1 == k)) then capMap := (k, v) :: capMap
            | none => pure ()
        return .generic name typeArgs (sd.capParams.map fun c => ((capMap.lookup c).getD .empty).canonArg)
    | none =>
      if typeArgs.isEmpty then return .named name else return .generic name typeArgs []
  | .enumLit _ enumName _ typeArgs _ =>
    -- Enums take no capability parameters.
    if typeArgs.isEmpty then return .named enumName else return .generic enumName typeArgs []
  | .fnRef _ name =>
    let env ← getEnv
    match env.allFnSigPairs.lookup name with
    | some sig => return .fn_ (sig.params.map Prod.snd) sig.capSet sig.retTy
    | none => return .placeholder
  | .paren _ inner => peekExprType inner
  | .binOp _ _ lhs _ => peekExprType lhs
  | .borrow _ inner => return .ref (← peekExprType inner)
  | .borrowMut _ inner => return .refMut (← peekExprType inner)
  | .deref _ inner =>
    match ← peekExprType inner with
    | .ref t | .refMut t | .ptrMut t | .ptrConst t | .heap t => return t
    | _ => return .placeholder
  | _ => return .placeholder

-- ============================================================
-- Core elaboration
-- ============================================================

/-- An elaborated expression: Core output paired with its evidence draft.

    Defined HERE, not in the Proof layer. `CExpr` is not in scope in
    `Proof/EvidenceTree.lean`, so declaring the field there made Lean AUTO-BIND `CExpr`
    as an implicit type variable — the structure was silently polymorphic over a
    made-up type, and its `core` field landed in `Prop`. Nothing caught it because
    nothing constructed the structure until the producer did.

    A named structure rather than a tuple: dropping a named `evidence` field is
    conspicuous, whereas `let (c, _) := ...` reads as ordinary destructuring. -/
structure ElaboratedExprV2 where
  core     : CExpr
  evidence : Proof.EvidenceExprV2

/-- An elaborated statement and its evidence draft. -/
structure ElaboratedStmtV2 where
  core     : List CStmt
  evidence : Proof.EvidenceStmtV2

mutual

partial def elabExprEv (e : Expr) (hint : Option Ty := none) : ElabM ElaboratedExprV2 := do
  match e with
  | .intLit _ v =>
    -- Same shared decision as Check (TypeJudgment): Elab stamps CExpr.ty from the
    -- judgment's `.ty`, so Check's type and Elab's stamp cannot disagree (E0228).
    -- The hint is already resolved at let/call sites (elab skips re-resolving).
    let ty := TypeJudgment.intLitType hint
    return ElaboratedExprV2.mk (CExpr.intLit v ty) (Proof.evIntLit v ty)
  | .floatLit _ v =>
    let ty := TypeJudgment.floatLitType hint
    return ElaboratedExprV2.mk (CExpr.floatLit v ty) (Proof.evFloatLit v ty)
  | .boolLit _ b => return ElaboratedExprV2.mk (CExpr.boolLit b) (Proof.evBoolLit b)
  | .strLit _ s => return ElaboratedExprV2.mk (CExpr.strLit s) (Proof.evStrLit s)
  | .charLit _ c => return ElaboratedExprV2.mk (CExpr.charLit c) (Proof.evCharLit c)

  | .ident _ name =>
    let env ← getEnv
    match env.constants.lookup name with
    | some ty => return ElaboratedExprV2.mk (CExpr.ident name ty) (Proof.evConstRef env.proofPath name)
    | none =>
    match env.vars.lookup name with
    | some ty => do
      -- COVERAGE ONLY. The binder reference itself is emitted structurally by
      -- `evBinderRef` on the next line; this arm no longer records a second copy of
      -- it. If the frame stack cannot place the local, the body is uncovered rather
      -- than encoded against a guessed position.
      match env.bodyScope.resolve? name with
      | some _ => pure ()
      | none   => markBodyIdentityUncovered
      return ElaboratedExprV2.mk (CExpr.ident (← coreNameOf name) ty) (Proof.evBinderRef env.bodyScope name)
    | none =>
      match ← lookupFnSig name with
      | some sig =>
        let paramTys := sig.params.map Prod.snd
        return ElaboratedExprV2.mk (CExpr.ident name (.fn_ paramTys sig.capSet sig.retTy)) (match env.resolveCallee name with
                 | some id => Proof.evFnRef id
                 | none    => Proof.evUnhandledExpr "fn value: callee not resolvable")
      | none =>
        -- A name bound by `ghost let` is erased: reading it from runtime code is
        -- a leak, reported precisely instead of as a generic undeclared var.
        if env.ghostVars.contains name then
          throwElab (.ghostInRuntime name) (some e.getSpan)
        throwElab (.undeclaredVariable name) (some e.getSpan)

  | .paren _ inner => elabExprEv inner hint

  | .binOp _ op lhs rhs =>
    -- Same operand-order judgment as Check (TypeJudgment.binOpOperandOrder): type
    -- the concrete side first so a flexible literal tree adopts its width
    -- top-down. This replaces the old bottom-up re-elaborate repair that
    -- special-cased `(8 + 5) + x` (x: i32) — the top-down hint reaches the entire
    -- flexible subtree, so `int + i32` mismatches (E0715) cannot arise. A genuine
    -- `Int` value ignores the hint and a real width mismatch still surfaces at
    -- Check/CoreCheck, exactly as before.
    -- The operand-order block must yield EVIDENCE as well as Core: the branch decides
    -- which side is typed first, and both children's evidence is needed at the return.
    let (cLhsEv, cRhsEv) ←
      match TypeJudgment.binOpOperandOrder
              (TypeJudgment.isFlexibleLit lhs) (TypeJudgment.isFlexibleLit rhs) with
      | .rhsFirst => do
        let cRhsEv ← elabExprEv rhs hint
        let cLhsEv ← elabExprEv lhs (some cRhsEv.core.ty)
        pure (cLhsEv, cRhsEv)
      | .lhsFirst => do
        let cLhsEv ← elabExprEv lhs hint
        let cRhsEv ← elabExprEv rhs (some cLhsEv.core.ty)
        pure (cLhsEv, cRhsEv)
    let cLhs := cLhsEv.core
    let cRhs := cRhsEv.core
    let resultTy := match op with
      | .eq | .neq | .lt | .gt | .leq | .geq => .bool
      | .and_ | .or_ => .bool
      | _ => cLhs.ty
    return ElaboratedExprV2.mk (CExpr.binOp op cLhs cRhs resultTy) (Proof.evBinary op cLhsEv.evidence cRhsEv.evidence)

  | .unaryOp _ op operand =>
    let cOpEv ← elabExprEv operand hint
    let cOp := cOpEv.core
    let resultTy := match op with
      | .not_ => Ty.bool
      | _ => cOp.ty
    return ElaboratedExprV2.mk (CExpr.unaryOp op cOp resultTy) (Proof.evUnary op cOpEv.evidence)


  | .allocCall _ inner allocExpr =>
    let cInnerEv ← elabExprEv inner hint
    let cInner := cInnerEv.core
    let cAllocEv ← elabExprEv allocExpr
    let cAlloc := cAllocEv.core
    return ElaboratedExprV2.mk (CExpr.allocCall cInner cAlloc cInner.ty) (Proof.evUnhandledExpr "allocator-specialized call")


  | .ifExpr _ cond then_ else_ =>
    let cCondEv ← elabExprEv cond
    let cCond := cCondEv.core
    -- Flow the if-expression's own hint into each branch so a flexible
    -- literal/binop trailing value adopts the result width (matches Check).
    let cThenEv ← elabStmtsEv then_ (valueHint := hint)
    let cThen := cThenEv.flatMap (·.core)
    let cElseEv ← elabStmtsEv else_ (valueHint := hint)
    let cElse := cElseEv.flatMap (·.core)
    -- Result type: an explicit hint wins (the branches were just typed under it);
    -- otherwise infer from a branch's elaborated TRAILING VALUE type. Use the
    -- stamped Core type (`e.ty`), NOT a shallow surface `peekExprType`, so Elab's
    -- result cannot disagree with the type it actually gave the branch. A branch
    -- with no trailing value (diverging or `;`-ended) contributes nothing, so the
    -- other branch wins; if neither yields a value the if-expression is Unit
    -- (#42 — avoids the `alloca void` result slot).
    let trailingTy := fun (cs : List CStmt) =>
      cs.reverse.findSome? fun s => match s with | .expr e true => some e.ty | _ => none
    let resultTy := match hint with
      | some t => t
      | none => (trailingTy cThen <|> trailingTy cElse).getD .unit
    -- Both branches in full, as STATEMENT lists. The value is the trailing expression
    -- of whichever branch runs, and `exprStmt`'s isValue flag already records which
    -- statement that is — so the branches need no separate value slot, and a branch
    -- that diverges or ends in `;` correctly contributes none.
    return ElaboratedExprV2.mk (CExpr.ifExpr cCond cThen cElse resultTy)
      (Proof.EvidenceExprV2.ifExpr cCondEv.evidence
        (cThenEv.map (·.evidence)) (cElseEv.map (·.evidence)))

  | .call _ fnName typeArgs args =>
    elabCallEv fnName typeArgs args hint (some e.getSpan)

  | .structLit _ name typeArgs fields base =>
    match ← lookupStruct name with
    | some sd =>
      recordStructTypeUse sd
      let typeArgs ← typeArgs.mapM resolveTypeE
      let mapping := sd.typeParams.zip typeArgs
      -- Capability arguments (R-0484), by the same rule as Check: from the expected type
      -- when it names this struct, otherwise read off the elaborated field values below.
      let hintCapMap : Option (List (String × CapSet)) := match hint with
        | some (.generic hn _ hc) =>
          if hn == name && !sd.capParams.isEmpty && hc.length == sd.capParams.length
          then some (sd.capParams.zip hc) else none
        | _ => none
      let provisionalCaps := sd.capParams.map fun c =>
        (((hintCapMap.getD []).lookup c).getD .empty).canonArg
      let resultTy := if typeArgs.isEmpty && sd.capParams.isEmpty then Ty.named name
                      else Ty.generic name typeArgs provisionalCaps
      -- Functional update `..base`: elaborate the base once; any field not given
      -- explicitly is filled with `base.field`. (Use a variable as the base — a
      -- complex base expression is re-read per copied field.)
      let cBase ← base.mapM (fun b => do pure (← elabExprEv b (some resultTy)).core)
      let mut cFields : List (String × CExpr) := []
      -- SOURCE ORDER (language decision, 2026-08-08). Initializers evaluate left-to-right as
      -- WRITTEN, so this walks the literal's own field list rather than `sd.fields`.
      --
      -- The list order IS the evaluation order: `Interp.evalFields` folds the list left to
      -- right, so emitting source order here is what makes `P { y: f(), x: g() }` run `f()`
      -- first. `CoreCanonicalize` previously reordered this list back to declaration order and
      -- no longer does — a normalization that erases an observable distinction is not a
      -- normalization, and under source-order semantics two literals differing only in written
      -- order ARE different programs whenever an initializer traps or has effects.
      --
      -- Fields omitted from the literal (filled from `..base`) are appended afterwards in
      -- declaration order: they have no written position to preserve, and `base` was already
      -- evaluated once above, so their relative order is unobservable.
      let mut structFieldEvsRev : List (String × Proof.EvidenceExprV2) := []
      for (fname, expr) in fields do
        match sd.fields.find? fun sf => sf.name == fname with
        | some sf =>
          let fieldTy := substCapNamesTy (hintCapMap.getD []) (substTy mapping sf.ty)
          recordFieldUse sd sf.name
          let cExprEv ← elabExprEv expr (some fieldTy)
          cFields := cFields ++ [(sf.name, cExprEv.core)]
          structFieldEvsRev := (sf.name, cExprEv.evidence) :: structFieldEvsRev
        | none => pure ()  -- unknown field: reported by the checker, not here
      -- Without an expected type, read the capability arguments off the elaborated
      -- fields (each was elaborated exactly once, above).
      let capMap : List (String × CapSet) := match hintCapMap with
        | some m => m
        | none => sd.fields.foldl (fun acc sf =>
            match cFields.lookup sf.name with
            | some ce =>
              let found := inferCapBindings sd.capParams (substTy mapping sf.ty) ce.ty
              acc ++ found.filter fun (k, _) => !(acc.any (·.1 == k))
            | none => acc) []
      let capArgs := sd.capParams.map fun c => ((capMap.lookup c).getD .empty).canonArg
      let resultTy := if typeArgs.isEmpty && sd.capParams.isEmpty then Ty.named name
                      else Ty.generic name typeArgs capArgs
      for sf in sd.fields do
        if !(fields.any fun (fn, _) => fn == sf.name) then
          let fieldTy := substCapNamesTy capMap (substTy mapping sf.ty)
          match cBase with
          | some cb =>
            recordFieldUse sd sf.name
            cFields := cFields ++ [(sf.name, .fieldAccess cb sf.name fieldTy)]
          | none => pure ()  -- union partial init
      return ElaboratedExprV2.mk (CExpr.structLit name typeArgs cFields resultTy) (match sd.typeId? with
           | some owner =>
             .structLit owner (structFieldEvsRev.reverse.map fun fe =>
               ({ owner := owner, field := fe.1 }, fe.2))
           | none => Proof.evUnhandledExpr "struct literal: owner declaration has no identity")
    | none => throwElab (.unknownStructType name) (some e.getSpan)

  | .fieldAccess _ obj field =>
    let cObjEv ← elabExprEv obj
    let cObj := cObjEv.core
    -- 6D#3: `.field` on a heap shell derefs first (p.f ≡ (*p).f — the old
    -- `->` desugar folded into `.`), so downstream sees the plain struct.
    let cObj := match cObj.ty with
      | .heap t | .heapArray t => CExpr.deref cObj t
      | .ref (.heap t) | .refMut (.heap t) => CExpr.deref cObj t
      | _ => cObj
    let objTy := cObj.ty
    let innerTy := match objTy with
      | .ref t => t | .refMut t => t | t => t
    let (structName, typeArgs, capArgs) := match innerTy with
      | .named n => (n, ([] : List Ty), ([] : List CapSet))
      | .generic n args caps => (n, args, caps)
      | .string => ("String", [], [])
      | _ => ("", [], [])
    -- For `.0` on a borrowed newtype (`&Port`, `&mut Port`), deref to the
    -- newtype value first so the rebrand cast is `Newtype -> Inner`, not
    -- `&Newtype -> Inner` (which fails CoreCheck and confuses codegen).
    let isBorrowed := match objTy with | .ref _ | .refMut _ => true | _ => false
    let derefIfBorrowed (cObj : CExpr) (newtypeTy : Ty) : CExpr :=
      if isBorrowed then CExpr.deref cObj newtypeTy else cObj
    match ← lookupStruct structName with
    | some sd =>
      let mapping := sd.typeParams.zip typeArgs
      match sd.fields.find? fun f => f.name == field with
      | some f =>
        recordFieldUse sd field
        -- R-0484: the field's type at this instantiation (same rule as Check).
        let fieldTy := substCapNamesTy (sd.capParams.zip capArgs) (substTy mapping f.ty)
        let fieldTy ← resolveTypeE fieldTy
        return ElaboratedExprV2.mk (CExpr.fieldAccess cObj field fieldTy)
          (match sd.typeId? with
           | some owner => Proof.evField { owner := owner, field := field } cObjEv.evidence
           | none => Proof.evUnhandledExpr "field access: owner declaration has no identity")
      | none =>
        -- Newtype wrapping a struct: .0 unwraps to the inner type.
        if field == newtypeFieldName then
          let env ← getEnv
          match env.newtypes.find? fun nt => nt.name == structName with
          | some nt =>
            let mapping := nt.typeParams.zip typeArgs
            let innerTy ← resolveTypeE (substTy mapping nt.innerTy)
            let newtypeTy : Ty := if typeArgs.isEmpty then .named structName
                                   else .generic structName typeArgs [] -- newtypes take no capability parameters
            return ElaboratedExprV2.mk (CExpr.cast (derefIfBorrowed cObj newtypeTy) innerTy) (Proof.evUnhandledExpr "newtype rebrand cast")
          | none => return ElaboratedExprV2.mk (cObj) (cObjEv.evidence)
        else throwElab (.structHasNoField structName field) (some e.getSpan)
    | none =>
      -- Newtype over a primitive (or any non-struct inner type): .0 unwraps.
      -- For generic newtypes (`Wrapper<T> = T;`), substitute the obj's
      -- type args so the unwrapped value carries the concrete inner type.
      if field == newtypeFieldName then
        let env ← getEnv
        match env.newtypes.find? fun nt => nt.name == structName with
        | some nt =>
          let mapping := nt.typeParams.zip typeArgs
          let innerTy ← resolveTypeE (substTy mapping nt.innerTy)
          let newtypeTy : Ty := if typeArgs.isEmpty then .named structName
                                 else .generic structName typeArgs [] -- newtypes take no capability parameters
          return ElaboratedExprV2.mk (CExpr.cast (derefIfBorrowed cObj newtypeTy) innerTy) (Proof.evUnhandledExpr "newtype rebrand cast")
        | none => return ElaboratedExprV2.mk (cObj) (cObjEv.evidence)
      else throwElab .fieldAccessNonStruct (some e.getSpan)

  | .enumLit _ enumName variant typeArgs fields =>
    match ← lookupEnum enumName with
    | some ed =>
      let typeArgs ← typeArgs.mapM resolveTypeE
      let effectiveTypeArgs := if typeArgs.isEmpty && !ed.typeParams.isEmpty then
        match hint with
        | some (.generic n args _) => if n == enumName then args else []
        | some (.named n) => if n == enumName then [] else []
        | _ => []
      else typeArgs
      let mapping := ed.typeParams.zip effectiveTypeArgs
      match ed.variants.find? fun v => v.name == variant with
      | some ev =>
        recordVariantUse ed variant
        let mut cFields : List (String × CExpr) := []
        let mut fieldEvsRev : List (String × Proof.EvidenceExprV2) := []
        for sf in ev.fields do
          let fieldTy := substTy mapping sf.ty
          match fields.find? fun (fn, _) => fn == sf.name with
          | some (_, expr) =>
            let cExprEv ← elabExprEv expr (some fieldTy)
            let cExpr := cExprEv.core
            cFields := cFields ++ [(sf.name, cExpr)]
            fieldEvsRev := (sf.name, cExprEv.evidence) :: fieldEvsRev
          | none => throwElab (.missingFieldInVariant sf.name enumName variant) (some e.getSpan)
        let resultTy := if effectiveTypeArgs.isEmpty then Ty.named enumName
                         else Ty.generic enumName effectiveTypeArgs [] -- enums take no capability parameters
        return ElaboratedExprV2.mk (CExpr.enumLit enumName variant effectiveTypeArgs cFields resultTy)
          (match ed.typeId? with
           | some owner =>
             .variantLit { owner := owner, variant := variant }
               (fieldEvsRev.reverse.map fun fe => ({ owner := owner, field := fe.1 }, fe.2))
           | none => Proof.evUnhandledExpr "enum literal: owner declaration has no identity")
      | none => throwElab (.unknownVariant variant enumName) (some e.getSpan)
    | none => throwElab (.unknownEnumType enumName) (some e.getSpan)

  | .match_ _ scrutinee arms =>
    let cScrutEv ← elabExprEv scrutinee
    let cScrut := cScrutEv.core
    let scrTy := cScrut.ty
    let innerTy := match scrTy with
      | .ref t => t | .refMut t => t | t => t
    let innerTyR ← resolveTypeE innerTy
    let (enumName, enumTypeArgs) := match innerTyR with
      | .named n => (n, ([] : List Ty))
      | .generic n args _ => (n, args)
      | _ => ("", [])
    let mut cArms : List CMatchArm := []
    -- Arm evidence, PREPENDED and reversed once: arm ORDER is semantic (first match wins).
    let mut armEvsRev : List Proof.EvidenceArmV2 := []
    if enumName != "" then
      match ← lookupEnum enumName with
      | some ed =>
        let envBefore ← getEnv
        for arm in arms do
          restoreScope envBefore
          -- An arm's pattern bindings are their OWN construct, so each arm enters a
          -- scope. Lazy: an arm with no bindings materializes no frame.
          openScope
          match arm with
          | .mk _ _armEnum armVariant bindings guard body =>
            let ev ← match ed.variants.find? fun v => v.name == armVariant with
              | some ev => pure ev
              | none => throwElab (.unknownVariant armVariant enumName) (some e.getSpan)
            recordVariantUse ed armVariant
            let typeMapping := ed.typeParams.zip enumTypeArgs
            let mut typedBindings : List (String × Ty) := []
            for (binding, sf) in bindings.zip ev.fields do
              let bty := substTy typeMapping sf.ty
              -- Keep `_` in the binding list (positional field mapping) but do not
              -- bind it in scope — it is a wildcard, not a readable variable.
              -- Bug 045: the arm carries the FRESH (alpha-renamed) name.
              let coreBinding ← bindArmVar binding bty
              typedBindings := typedBindings ++ [(coreBinding, bty)]
            let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
            let cGuard := cGuardEv.map (·.core)
            let cBodyEv ← elabStmtsEv body (valueHint := hint)
            let cBody := cBodyEv.flatMap (·.core)
            cArms := cArms ++ [.enumArm enumName armVariant typedBindings cGuard cBody]
            armEvsRev := (match ed.typeId? with
              | some owner =>
                .arm (Proof.evVariantPattern { owner := owner, variant := armVariant }
                        (ev.fields.zip bindings |>.map fun fb =>
                           ({ owner := owner, field := fb.1.name }, fb.2 != "_")))
                     (cGuardEv.map (·.evidence)) (cBodyEv.map (·.evidence))
              | none => .gap { code := .unresolvedVariant,
                               detail := "arm owner declaration has no identity" }) :: armEvsRev
          | .litArm _ val guard body =>
            let cValEv ← elabExprEv val
            let cVal := cValEv.core
            let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
            let cGuard := cGuardEv.map (·.core)
            let cBodyEv ← elabStmtsEv body (valueHint := hint)
            let cBody := cBodyEv.flatMap (·.core)
            cArms := cArms ++ [.litArm cVal cGuard cBody]
            armEvsRev := (.arm (Proof.evLitPattern cValEv.evidence)
                            (cGuardEv.map (·.evidence)) (cBodyEv.map (·.evidence))) :: armEvsRev
          | .varArm _ binding guard body =>
            let coreBinding ← bindArmVar binding innerTyR
            let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
            let cGuard := cGuardEv.map (·.core)
            let cBodyEv ← elabStmtsEv body (valueHint := hint)
            let cBody := cBodyEv.flatMap (·.core)
            cArms := cArms ++ [.varArm coreBinding innerTyR cGuard cBody]
            armEvsRev := (.arm (if binding == "_" then .wildcard else .binder)
                            (cGuardEv.map (·.evidence)) (cBodyEv.map (·.evidence))) :: armEvsRev
          | .rangeArm _ lo hi incl guard body =>
            let cLoEv ← elabExprEv lo (some innerTyR)
            let cLo := cLoEv.core
            let cHiEv ← elabExprEv hi (some innerTyR)
            let cHi := cHiEv.core
            let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
            let cGuard := cGuardEv.map (·.core)
            let cBodyEv ← elabStmtsEv body (valueHint := hint)
            let cBody := cBodyEv.flatMap (·.core)
            cArms := cArms ++ [.rangeArm cLo cHi incl cGuard cBody]
        restoreScope envBefore
      | none =>
        let envBefore ← getEnv
        for arm in arms do
          restoreScope envBefore
          -- An arm's pattern bindings are their OWN construct, so each arm enters a
          -- scope. Lazy: an arm with no bindings materializes no frame.
          openScope
          match arm with
          | .litArm _ val guard body =>
            let cValEv ← elabExprEv val
            let cVal := cValEv.core
            let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
            let cGuard := cGuardEv.map (·.core)
            let cBodyEv ← elabStmtsEv body (valueHint := hint)
            let cBody := cBodyEv.flatMap (·.core)
            cArms := cArms ++ [.litArm cVal cGuard cBody]
          | .varArm _ binding guard body =>
            let coreBinding ← bindArmVar binding innerTyR
            let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
            let cGuard := cGuardEv.map (·.core)
            let cBodyEv ← elabStmtsEv body (valueHint := hint)
            let cBody := cBodyEv.flatMap (·.core)
            cArms := cArms ++ [.varArm coreBinding innerTyR cGuard cBody]
          | .mk _ en v _ guard body =>
            let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
            let cGuard := cGuardEv.map (·.core)
            let cBodyEv ← elabStmtsEv body (valueHint := hint)
            let cBody := cBodyEv.flatMap (·.core)
            cArms := cArms ++ [.enumArm en v [] cGuard cBody]
          | .rangeArm _ lo hi incl guard body =>
            let cLoEv ← elabExprEv lo (some innerTyR)
            let cLo := cLoEv.core
            let cHiEv ← elabExprEv hi (some innerTyR)
            let cHi := cHiEv.core
            let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
            let cGuard := cGuardEv.map (·.core)
            let cBodyEv ← elabStmtsEv body (valueHint := hint)
            let cBody := cBodyEv.flatMap (·.core)
            cArms := cArms ++ [.rangeArm cLo cHi incl cGuard cBody]
        restoreScope envBefore
    else
      let envBefore ← getEnv
      for arm in arms do
        restoreScope envBefore
        openScope
        match arm with
        | .litArm _ val guard body =>
          let cValEv ← elabExprEv val (some innerTyR)
          let cVal := cValEv.core
          let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
          let cGuard := cGuardEv.map (·.core)
          let cBodyEv ← elabStmtsEv body (valueHint := hint)
          let cBody := cBodyEv.flatMap (·.core)
          cArms := cArms ++ [.litArm cVal cGuard cBody]
        | .varArm _ binding guard body =>
          let coreBinding ← bindArmVar binding innerTyR
          let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
          let cGuard := cGuardEv.map (·.core)
          let cBodyEv ← elabStmtsEv body (valueHint := hint)
          let cBody := cBodyEv.flatMap (·.core)
          cArms := cArms ++ [.varArm coreBinding innerTyR cGuard cBody]
        | .mk _ en v _ guard body =>
          let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
          let cGuard := cGuardEv.map (·.core)
          let cBodyEv ← elabStmtsEv body (valueHint := hint)
          let cBody := cBodyEv.flatMap (·.core)
          cArms := cArms ++ [.enumArm en v [] cGuard cBody]
        | .rangeArm _ lo hi incl guard body =>
          let cLoEv ← elabExprEv lo (some innerTyR)
          let cLo := cLoEv.core
          let cHiEv ← elabExprEv hi (some innerTyR)
          let cHi := cHiEv.core
          let cGuardEv ← guard.mapM (fun g => elabExprEv g (some Ty.bool))
          let cGuard := cGuardEv.map (·.core)
          let cBodyEv ← elabStmtsEv body (valueHint := hint)
          let cBody := cBodyEv.flatMap (·.core)
          cArms := cArms ++ [.rangeArm cLo cHi incl cGuard cBody]
      restoreScope envBefore
    -- Result type: prefer the caller's hint; otherwise infer it from the arm
    -- bodies' trailing value expressions. A nested match used as a statement
    -- expression (e.g. another match's arm body) is elaborated with no hint, and
    -- defaulting to `.unit` made its value vanish — the arm literals were cast to
    -- `void` and the outer match collapsed to a single non-dominating value
    -- (an SSA dominance error / miscompile). Check verifies the arms agree.
    let armBodyStmts : CMatchArm → List CStmt := fun a => match a with
      | .enumArm _ _ _ _ b => b
      | .litArm _ _ b => b
      | .varArm _ _ _ b => b
      | .rangeArm _ _ _ _ b => b
    let armValueTy : List CStmt → Ty := fun body => match body.getLast? with
      | some (.expr e true) => e.ty
      | _ => .unit
    let resultTy := match hint with
      | some t => t
      | none =>
        let armTys := (cArms.map (fun a => armValueTy (armBodyStmts a))).filter
          (fun t => t != .unit && t != .never)
        armTys.head?.getD .unit
    return ElaboratedExprV2.mk (CExpr.match_ cScrut cArms resultTy) (Proof.EvidenceExprV2.matchExpr cScrutEv.evidence armEvsRev.reverse)

  | .borrow _ inner =>
    let cInnerEv ← elabExprEv inner
    let cInner := cInnerEv.core
    return ElaboratedExprV2.mk (CExpr.borrow cInner (.ref cInner.ty)) (Proof.evBorrow false cInnerEv.evidence)

  | .borrowMut _ inner =>
    let cInnerEv ← elabExprEv inner
    let cInner := cInnerEv.core
    return ElaboratedExprV2.mk (CExpr.borrowMut cInner (.refMut cInner.ty)) (Proof.evBorrow true cInnerEv.evidence)

  | .deref _ inner =>
    let cInnerEv ← elabExprEv inner
    let cInner := cInnerEv.core
    let resultTy := match cInner.ty with
      | .ref t => t | .refMut t => t
      | .ptrMut t => t | .ptrConst t => t | .heap t => t
      | _ => .placeholder
    return ElaboratedExprV2.mk (CExpr.deref cInner resultTy) (Proof.evDeref cInnerEv.evidence)

  | .arrayLit _ elems =>
    match elems with
    | [] => throwElab .arrayLiteralEmpty (some e.getSpan)
    | first :: rest =>
      let elemHint := match hint with | some (.array t _) => some t | _ => none
      let cFirstEv ← elabExprEv first elemHint
      let cFirst := cFirstEv.core
      let elemTy := cFirst.ty
      let mut cElems : List CExpr := [cFirst]
      -- Prepended and reversed once: appending in a loop is quadratic, and the ratchet
      -- has caught that three times in this arc already.
      let mut elemEvsRev : List Proof.EvidenceExprV2 := [cFirstEv.evidence]
      for e in rest do
        let cEEv ← elabExprEv e (some elemTy)
        let cE := cEEv.core
        cElems := cElems ++ [cE]
        elemEvsRev := cEEv.evidence :: elemEvsRev
      let elemEvs := elemEvsRev.reverse
      let elemRef ← typeRefOf elemTy
      return ElaboratedExprV2.mk (CExpr.arrayLit cElems (.array elemTy elems.length))
        (Proof.evArrayLit elemRef elemEvs)

  | .arrayIndex _ arr index =>
    let cArrEv ← elabExprEv arr
    let cArr := cArrEv.core
    let cIdxEv ← elabExprEv index (some .int)
    let cIdx := cIdxEv.core
    -- Indexing auto-derefs a reference/pointer to an array (`&[T;N]` etc.), so
    -- the element type resolves through one ref/ptr/heap layer (C10).
    let elemTy := match cArr.ty with
      | .array t _ => t
      | .ref (.array t _) | .refMut (.array t _)
      | .ptrConst (.array t _) | .ptrMut (.array t _)
      | .heap (.array t _) => t
      | _ => .placeholder
    return ElaboratedExprV2.mk (CExpr.arrayIndex cArr cIdx elemTy) (Proof.evIndex cArrEv.evidence cIdxEv.evidence)

  | .cast _ inner targetTy =>
    -- Do NOT pass any hint: the point of `as` is to convert between types.
    -- Passing targetTy would mistype literals in `(100 + m) as i32` where m is Int.
    -- Passing the outer hint could also leak i32 context into the inner expression.
    let cInnerEv ← elabExprEv inner none
    let cInner := cInnerEv.core
    -- The cast TARGET is the whole point of the expression: `x as i32` and `x as i64`
    -- are different programs, and before this the two were one gap.
    let targetRef ← typeRefOf targetTy
    return ElaboratedExprV2.mk (CExpr.cast cInner targetTy) (Proof.evCast targetRef cInnerEv.evidence)

  | .fnRef _ fnName =>
    let env ← getEnv
    match env.allFnSigPairs.lookup fnName with
    | some sig =>
      let paramTys := sig.params.map Prod.snd
      return ElaboratedExprV2.mk (CExpr.fnRef fnName (.fn_ paramTys sig.capSet sig.retTy)) (match env.resolveCallee fnName with
                 | some id => Proof.evFnRef id
                 | none    => Proof.evUnhandledExpr "fn reference: callee not resolvable")
    | none => throwElab (.unknownFunctionRef fnName) (some e.getSpan)

  | .methodCall _ obj methodName typeArgs args =>
    -- Desugar: obj.method(args) → Type_method(&obj, args) or Type_method(&mut obj, args)
    let typeArgs ← typeArgs.mapM resolveTypeE
    let cObjEv ← elabExprEv obj
    let cObj := cObjEv.core
    let objTy := cObj.ty
    let innerTy0 := match objTy with
      | .ref t => t | .refMut t => t | t => t
    -- Normalize `.named T` where T is a current type param (mirror of the
    -- Check-side methodCall normalization; see checkTraitBounds precedent).
    let envN ← getEnv
    let innerTy := match innerTy0 with
      | .named n => if envN.currentTypeParams.contains n then .typeVar n else innerTy0
      | t => t
    let typeName := tyName innerTy
    if typeName == "" then
      -- Type variable with trait bounds
      match innerTy with
      | .typeVar n =>
        let env ← getEnv
        let bounds := (env.currentTypeBounds.find? fun (name, _) => name == n).map Prod.snd |>.getD []
        let mut foundSig : Option FnSigDef := none
        for traitName in bounds do
          match env.traits.find? fun td => td.name == traitName with
          | some td =>
            match td.methods.find? fun ms => ms.name == methodName with
            | some ms => foundSig := some ms; break
            | none => pure ()
          | none => pure ()
        match foundSig with
        | none => throwElab (.noMethodOnTypeVar methodName n) (some e.getSpan)
        | some sig =>
          -- Replace Self with the type variable
          let selfTy := Ty.typeVar n
          let retTy := substSelf sig.retTy selfTy
          let params := sig.params.map fun p => { p with ty := substSelf p.ty selfTy }
          let mut cArgs : List CExpr := [cObj]
          -- Accumulated by PREPEND and reversed at the consumer, so the receiver seed goes
          -- in last to land first. Appending in a loop is quadratic; the ratchet caught it.
          let mut argEvs : List Proof.EvidenceExprV2 := [cObjEv.evidence]
          for (arg, p) in args.zip params do
            let cArgEv ← elabExprEv arg (some p.ty)
            let cArg := cArgEv.core
            cArgs := cArgs ++ [cArg]
            argEvs := cArgEv.evidence :: argEvs
          return ElaboratedExprV2.mk (CExpr.call (mangledMethodName n methodName) typeArgs cArgs retTy)
            (match env.resolveCallee (n ++ "_" ++ methodName) with
             | some id => Proof.evCall id argEvs.reverse
             | none    => Proof.evTraitMethodOnTypeParam)
      | _ => throwElab .methodCallOnNonNamedType (some e.getSpan)
    else
      let mangledName := mangledMethodName typeName methodName
      match ← lookupFnSig mangledName with
      | some sig =>
        let objTypeArgs := match innerTy with | .generic _ args _ => args | _ => []
        let implTypeParams := sig.typeParams.take objTypeArgs.length
        let methodTypeParams := sig.typeParams.drop objTypeArgs.length
        let implMapping := implTypeParams.zip objTypeArgs
        -- Infer the method's OWN type params from argument types when not
        -- turbofished, so the Core call carries concrete type args and Mono
        -- specializes without leaking a `Ty.typeVar` (mirror of the Check-phase
        -- method inference; required for capability-polymorphic / scoped-callback
        -- methods — ROADMAP Phase 5 #24).
        let methodArgs ← do
          if !typeArgs.isEmpty || methodTypeParams.isEmpty then
            pure typeArgs
          else
            let methodParamTys := (sig.params.drop 1).map fun (_, t) => substTy implMapping t
            let mut inferred : List (String × Ty) := []
            for (arg, pTy) in args.zip methodParamTys do
              let argTy ← peekExprType arg
              for (name, ty) in unifyTypes pTy argTy methodTypeParams do
                if !(inferred.any fun (n, _) => n == name) then
                  inferred := inferred ++ [(name, ty)]
            pure (methodTypeParams.map fun tp => (inferred.lookup tp).getD (.typeVar tp))
        let mapping := implMapping ++ methodTypeParams.zip methodArgs
        let methodParams := (sig.params.drop 1).map fun (_, t) => (substTy mapping t)
        let retTy := substTy mapping sig.retTy
        -- Wrap object with borrow/borrowMut if method expects a reference self
        -- and the object is not already a reference
        -- ONE match decides both the Core receiver and its evidence. Written as two
        -- matches they drift: the Core would borrow while the evidence recorded a
        -- by-value receiver, and `&self` and `self` are different calls.
        --
        -- `false` means no wrapper is applied, so the evidence is the receiver's own.
        let selfShape : Option Bool := match sig.params.head? with
          | some (_, selfTy) =>
            match substTy mapping selfTy, objTy with
            | .ref _, .ref _ => none          -- already a ref, pass as-is
            | .ref _, .refMut _ => none       -- already a ref, pass as-is
            | .refMut _, .refMut _ => none    -- already a mut ref, pass as-is
            | .ref _, _ => some false         -- borrow
            | .refMut _, _ => some true       -- borrowMut
            | _, _ => none                    -- by-value self, pass as-is
          | none => none
        let selfArg := match selfShape with
          | some false => CExpr.borrow cObj (.ref objTy)
          | some true  => CExpr.borrowMut cObj (.refMut objTy)
          | none       => cObj
        let selfEv := match selfShape with
          | some isMut => Proof.evBorrow isMut cObjEv.evidence
          | none       => cObjEv.evidence
        let mut cArgs : List CExpr := [selfArg]
        let mut argEvs : List Proof.EvidenceExprV2 := [selfEv]
        for (arg, pTy) in args.zip methodParams do
          let cArgEv ← elabExprEv arg (some pTy)
          let cArg := cArgEv.core
          cArgs := cArgs ++ [cArg]
          argEvs := cArgEv.evidence :: argEvs
        return ElaboratedExprV2.mk (CExpr.call mangledName (objTypeArgs ++ methodArgs) cArgs retTy)
          (match (← getEnv).resolveCallee mangledName with
           | some id => Proof.evCall id argEvs.reverse
           | none    => Proof.evUnresolvedCall "method call: mangled name has no CallableId")
      | none => throwElab (.noMethodOnType methodName typeName) (some e.getSpan)

  | .staticMethodCall _ typeName methodName typeArgs args =>
    let mangledName := mangledMethodName typeName methodName
    match ← lookupFnSig mangledName with
    | some sig =>
      let mapping := sig.typeParams.zip typeArgs
      let paramTypes := sig.params.map fun (_, t) => substTy mapping t
      let retTy := substTy mapping sig.retTy
      let mut cArgs : List CExpr := []
      let mut argEvs : List Proof.EvidenceExprV2 := []
      for (arg, pTy) in args.zip paramTypes do
        let cArgEv ← elabExprEv arg (some pTy)
        let cArg := cArgEv.core
        cArgs := cArgs ++ [cArg]
        argEvs := cArgEv.evidence :: argEvs
      return ElaboratedExprV2.mk (CExpr.call mangledName typeArgs cArgs retTy)
        (match (← getEnv).resolveCallee mangledName with
         | some id => Proof.evCall id argEvs.reverse
         | none    => Proof.evUnresolvedCall "static method call: mangled name has no CallableId")
    | none => throwElab (.noMethodOnType methodName typeName) (some e.getSpan)

/-- Elaborate a function call (regular, builtins, intercepted). -/
partial def elabCallEv (fnName : String) (typeArgs : List Ty) (args : List Expr)
    (hint : Option Ty) (span : Option Span := none) : ElabM ElaboratedExprV2 := do
  let typeArgs ← typeArgs.mapM resolveTypeE
  -- Intrinsic IDENTITY, not raw name (audit 2026-07-16): a name that
  -- resolves to a USER function is never an intrinsic — mirrors Check's
  -- userFnNames guard (Check.lean:302). Without this, user fns named
  -- sizeof/wrapping_add/... were silently hijacked at elaboration
  -- (sizeof dropped its args; wrapping_add(2,3) became the 5-valued
  -- binop regardless of the user body).
  let userSig ← lookupFnSig fnName
  let intrinsic := if userSig.isSome then none else resolveIntrinsic fnName
  -- Intercept abort()
  if intrinsic == some .abort then
    return ElaboratedExprV2.mk (CExpr.call "abort" [] [] .never)
      (Proof.evCall (CallableId.ofIntrinsic "abort") [])
  -- Intercept destroy(arg)
  if intrinsic == some .destroy then
    let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
    let cArgEv ← elabExprEv arg
    let cArg := cArgEv.core
    let typeName := match cArg.ty with
      | .named n => n | .generic n _ _ => n | _ => ""
    return ElaboratedExprV2.mk (CExpr.call (destroyFnNameFor typeName) [] [cArg] .unit)
      (Proof.evCall (CallableId.ofIntrinsic (destroyFnNameFor typeName)) [cArgEv.evidence])
  -- Intercept discard(arg) — acknowledged discard of a Copy value (slice 5).
  -- Desugar to a unit-valued `if true { arg; }`: evaluate `arg` (preserving any
  -- effect) and drop its Copy result, yielding `.unit`. This keeps `discard(e)`
  -- typed `Unit` in EVERY position — including value position (`let u =
  -- discard(e)`) — matching Check's `.unit`, rather than erasing to the inner
  -- expr (which would give `u` the inner type: a silent Check↔Core type
  -- disagreement). The unit-typed if-expr lowers cleanly now that result slots
  -- guard unit/never (`freshResultSlot`); Check already required `e` to be Copy,
  -- so the discarded statement needs no destructor.
  if intrinsic == some .discard then
    let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
    let cArgEv ← elabExprEv arg
    let cArg := cArgEv.core
    -- Described as the INTRINSIC CALL it is, not as the `if true { arg; }` it lowers
    -- to. The desugaring is a lowering detail with no expression-level node, and
    -- describing the surface intent keeps `discard(e)` distinct from a real branch.
    --
    -- The ARGUMENT is carried. `discard(f())` and `discard(g())` run different code,
    -- and an intrinsic call recorded with an empty argument list would merge them.
    return ElaboratedExprV2.mk (CExpr.ifExpr (.boolLit true) [.expr cArg false] [] .unit)
      (Proof.evCall (CallableId.ofIntrinsic "discard") [cArgEv.evidence])
  -- Intercept alloc(val)
  if intrinsic == some .alloc then
    let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
    let cArgEv ← elabExprEv arg
    let cArg := cArgEv.core
    -- The argument is NOT carried, unlike `discard` above. `alloc(1)` and `alloc(2)`
    -- therefore share evidence. Unreachable today: `alloc` requires the `Alloc`
    -- capability and any capability-bearing function is excluded from the provable
    -- subset, so no eligible subject can contain this node. If that exclusion is ever
    -- relaxed, carry `cArgEv.evidence` here before it becomes a live collision.
    return ElaboratedExprV2.mk (CExpr.call "alloc" [] [cArg] (.heap cArg.ty))
      (Proof.evCall (CallableId.ofIntrinsic "alloc") [])
  -- Intercept free(ptr)
  if intrinsic == some .free then
    let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
    let cArgEv ← elabExprEv arg
    let cArg := cArgEv.core
    let innerTy := match cArg.ty with | .heap t => t | _ => .placeholder
    return ElaboratedExprV2.mk (CExpr.call "free" [] [cArg] innerTy)
      (Proof.evCall (CallableId.ofIntrinsic "free") [])
  -- Intercept newtype constructor: keep the wrapper's name in the type so
  -- `obj.method()` later resolves against the wrapper's inherent impl, not
  -- the inner type. The runtime representation is identical to the inner
  -- value (.cast is a no-op at codegen time once Layout resolves through
  -- the newtype), so this is purely a type-level distinction. For generic
  -- newtypes, infer type args from explicit `::<...>` first, otherwise
  -- from the call hint (`let w: Wrapper<Int> = Wrapper(100);`).
  let env ← getEnv
  match env.newtypes.find? fun nt => nt.name == fnName with
  | some nt =>
    let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
    let effectiveTypeArgs :=
      if !typeArgs.isEmpty then typeArgs
      else match hint with
        | some (.generic n args _) => if n == fnName then args else []
        | _ => []
    let mapping := nt.typeParams.zip effectiveTypeArgs
    let innerTy ← resolveTypeE (substTy mapping nt.innerTy)
    let cArgEv ← elabExprEv arg (some innerTy)
    let cArg := cArgEv.core
    let resultTy := if effectiveTypeArgs.isEmpty then Ty.named fnName
                     else Ty.generic fnName effectiveTypeArgs []
    return ElaboratedExprV2.mk (CExpr.cast cArg resultTy)
      (Proof.evUnhandledExpr "intrinsic cast: target TypeId not minted here")
  | none => pure ()
  -- Intercept unwrap(x): erase to inner expression (only if not a user-defined function)
  if intrinsic == some .unwrap && args.length == 1 then
    let isUserFn ← lookupFnSig "unwrap"
    if isUserFn.isNone then
      let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let cArgEv ← elabExprEv arg
      let cArg := cArgEv.core
      -- newtype erasure: the inner value IS the result, so its evidence passes through
      return ElaboratedExprV2.mk cArg cArgEv.evidence
  -- Intercept wrapping_add/sub/mul(a, b) → explicit modular CExpr.binOp.
  -- Check has already validated 2 integer operands of the same type, so the
  -- result type is the operand type. Lowers to plain LLVM add/sub/mul.
  if intrinsic == some .wrappingAdd || intrinsic == some .wrappingSub
     || intrinsic == some .wrappingMul || intrinsic == some .saturatingAdd
     || intrinsic == some .saturatingSub || intrinsic == some .saturatingMul then
    let cAEv ← elabExprEv (match args with | a :: _ => a | [] => Expr.intLit default 0) hint
    let cA := cAEv.core
    let cBEv ← elabExprEv (match args with | _ :: b :: _ => b | _ => Expr.intLit default 0) (some cA.ty)
    let cB := cBEv.core
    let bop := match intrinsic with
      | some .wrappingSub   => BinOp.wrappingSub
      | some .wrappingMul   => BinOp.wrappingMul
      | some .saturatingAdd => BinOp.saturatingAdd
      | some .saturatingSub => BinOp.saturatingSub
      | some .saturatingMul => BinOp.saturatingMul
      | _                   => BinOp.wrappingAdd
    return ElaboratedExprV2.mk (CExpr.binOp bop cA cB cA.ty)
      (Proof.evBinary bop cAEv.evidence cBEv.evidence)
  -- Intercept sizeof/alignof
  if intrinsic == some .sizeof || intrinsic == some .alignof
     || (userSig.isNone && fnName.endsWith sizeofSuffix) then
    return ElaboratedExprV2.mk (CExpr.call fnName typeArgs [] .uint)
      (Proof.evCall (CallableId.ofIntrinsic fnName) [])
  -- Intercept vec_new::<T>()
  if intrinsic == some .vecNew then
    let elemTy := match typeArgs with | t :: _ => t | [] => .int
    return ElaboratedExprV2.mk (CExpr.call "vec_new" typeArgs [] (.generic "Vec" [elemTy] []))
      (Proof.evCall (CallableId.ofIntrinsic "vec_new") [])
  -- Intercept string_push_char(&mut s, ch)
  if intrinsic == some .stringPushChar then
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    for arg in args do
      let cArgEv ← elabExprEv arg
      let cArg := cArgEv.core
      cArgs := cArgs ++ [cArg]
      argEvs := cArgEv.evidence :: argEvs
    return ElaboratedExprV2.mk (CExpr.call "string_push_char" [] cArgs .unit)
      (Proof.evCall (CallableId.ofIntrinsic "string_push_char") [])
  -- Intercept string_append(&mut s, other)
  if intrinsic == some .stringAppend then
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    for arg in args do
      let cArgEv ← elabExprEv arg
      let cArg := cArgEv.core
      cArgs := cArgs ++ [cArg]
      argEvs := cArgEv.evidence :: argEvs
    return ElaboratedExprV2.mk (CExpr.call "string_append" [] cArgs .unit)
      (Proof.evCall (CallableId.ofIntrinsic "string_append") [])
  -- Intercept string_append_int(&mut s, n)
  if intrinsic == some .stringAppendInt then
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    for arg in args do
      let cArgEv ← elabExprEv arg
      let cArg := cArgEv.core
      cArgs := cArgs ++ [cArg]
      argEvs := cArgEv.evidence :: argEvs
    return ElaboratedExprV2.mk (CExpr.call "string_append_int" [] cArgs .unit)
      (Proof.evCall (CallableId.ofIntrinsic "string_append_int") [])
  -- Intercept string_append_bool(&mut s, b)
  if intrinsic == some .stringAppendBool then
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    for arg in args do
      let cArgEv ← elabExprEv arg
      let cArg := cArgEv.core
      cArgs := cArgs ++ [cArg]
      argEvs := cArgEv.evidence :: argEvs
    return ElaboratedExprV2.mk (CExpr.call "string_append_bool" [] cArgs .unit)
      (Proof.evCall (CallableId.ofIntrinsic "string_append_bool") [])
  -- Intercept string_reserve(&mut s, cap)
  if intrinsic == some .stringReserve then
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    for arg in args do
      let cArgEv ← elabExprEv arg
      let cArg := cArgEv.core
      cArgs := cArgs ++ [cArg]
      argEvs := cArgEv.evidence :: argEvs
    return ElaboratedExprV2.mk (CExpr.call "string_reserve" [] cArgs .unit)
      (Proof.evCall (CallableId.ofIntrinsic "string_reserve") [])
  -- Intercept vec_push
  if intrinsic == some .vecPush then
    -- Elaborate vec arg first to extract element type for value hint
    let elemTy := match typeArgs with | t :: _ => t | [] => .int
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    match args with
    | vecArg :: valArg :: rest =>
      let cVecEv ← elabExprEv vecArg
      let cVec := cVecEv.core
      cArgs := cArgs ++ [cVec]
      let cValEv ← elabExprEv valArg (some elemTy)
      let cVal := cValEv.core
      cArgs := cArgs ++ [cVal]
      for arg in rest do
        let cArgEv ← elabExprEv arg
        let cArg := cArgEv.core
        cArgs := cArgs ++ [cArg]
        argEvs := cArgEv.evidence :: argEvs
    | _ =>
      for arg in args do
        let cArgEv ← elabExprEv arg
        let cArg := cArgEv.core
        cArgs := cArgs ++ [cArg]
        argEvs := cArgEv.evidence :: argEvs
    return ElaboratedExprV2.mk (CExpr.call "vec_push" [] cArgs .unit)
      (Proof.evCall (CallableId.ofIntrinsic "vec_push") [])
  -- Intercept vec_get
  if intrinsic == some .vecGet then
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    match args with
    | vecArg :: idxArg :: rest =>
      let cVecEv ← elabExprEv vecArg
      let cVec := cVecEv.core
      cArgs := cArgs ++ [cVec]
      let cIdxEv ← elabExprEv idxArg (some .int)
      let cIdx := cIdxEv.core
      cArgs := cArgs ++ [cIdx]
      for arg in rest do
        let cArgEv ← elabExprEv arg
        let cArg := cArgEv.core
        cArgs := cArgs ++ [cArg]
        argEvs := cArgEv.evidence :: argEvs
    | _ =>
      for arg in args do
        let cArgEv ← elabExprEv arg
        let cArg := cArgEv.core
        cArgs := cArgs ++ [cArg]
        argEvs := cArgEv.evidence :: argEvs
    let elemTy := match (cArgs.head?.map CExpr.ty) with
      | some (.ref (.generic "Vec" [et] _)) => et
      | some (.refMut (.generic "Vec" [et] _)) => et
      | _ => .placeholder
    return ElaboratedExprV2.mk (CExpr.call "vec_get" [] cArgs elemTy)
      (Proof.evCall (CallableId.ofIntrinsic "vec_get") [])
  -- Intercept vec_set
  if intrinsic == some .vecSet then
    let elemTy := match typeArgs with | t :: _ => t | [] => .int
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    match args with
    | vecArg :: idxArg :: valArg :: rest =>
      let cVecEv ← elabExprEv vecArg
      let cVec := cVecEv.core
      cArgs := cArgs ++ [cVec]
      let cIdxEv ← elabExprEv idxArg (some .int)
      let cIdx := cIdxEv.core
      cArgs := cArgs ++ [cIdx]
      let cValEv ← elabExprEv valArg (some elemTy)
      let cVal := cValEv.core
      cArgs := cArgs ++ [cVal]
      for arg in rest do
        let cArgEv ← elabExprEv arg
        let cArg := cArgEv.core
        cArgs := cArgs ++ [cArg]
        argEvs := cArgEv.evidence :: argEvs
    | _ =>
      for arg in args do
        let cArgEv ← elabExprEv arg
        let cArg := cArgEv.core
        cArgs := cArgs ++ [cArg]
        argEvs := cArgEv.evidence :: argEvs
    return ElaboratedExprV2.mk (CExpr.call "vec_set" [] cArgs .unit)
      (Proof.evCall (CallableId.ofIntrinsic "vec_set") [])
  -- Intercept vec_len
  if intrinsic == some .vecLen then
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    for arg in args do
      let cArgEv ← elabExprEv arg
      let cArg := cArgEv.core
      cArgs := cArgs ++ [cArg]
      argEvs := cArgEv.evidence :: argEvs
    return ElaboratedExprV2.mk (CExpr.call "vec_len" [] cArgs .int)
      (Proof.evCall (CallableId.ofIntrinsic "vec_len") [])
  -- Intercept vec_pop
  if intrinsic == some .vecPop then
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    for arg in args do
      let cArgEv ← elabExprEv arg
      let cArg := cArgEv.core
      cArgs := cArgs ++ [cArg]
      argEvs := cArgEv.evidence :: argEvs
    let elemTy := match (cArgs.head?.map CExpr.ty) with
      | some (.refMut (.generic "Vec" [et] _)) => et
      | _ => .placeholder
    return ElaboratedExprV2.mk (CExpr.call "vec_pop" [] cArgs (.generic optionEnumName [elemTy] []))
      (Proof.evCall (CallableId.ofIntrinsic "vec_pop") [])
  -- Intercept vec_free
  if intrinsic == some .vecFree then
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    for arg in args do
      let cArgEv ← elabExprEv arg
      let cArg := cArgEv.core
      cArgs := cArgs ++ [cArg]
      argEvs := cArgEv.evidence :: argEvs
    return ElaboratedExprV2.mk (CExpr.call "vec_free" [] cArgs .unit)
      (Proof.evCall (CallableId.ofIntrinsic "vec_free") [])
  -- Call through a fn-typed LOCAL or parameter. This is the one place that knows
  -- the callee is a value in scope rather than a global definition, so it is the
  -- one place that can record it (bug 050): downstream passes must never
  -- re-decide by looking the name up in a global fn/alias map.
  match ← lookupVar fnName with
  | some (.fn_ paramTys _ retTy) =>
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    for (arg, pTy) in args.zip paramTys do
      let cArgEv ← elabExprEv arg (some pTy)
      let cArg := cArgEv.core
      cArgs := cArgs ++ [cArg]
      argEvs := cArgEv.evidence :: argEvs
    return ElaboratedExprV2.mk (CExpr.call (.indirect fnName) [] cArgs retTy)
      (match (← getEnv).resolveCallee fnName with
       | some id => Proof.evCall id argEvs.reverse
       | none    => Proof.evUnresolvedCall "call: name has no CallableId")
  | _ => pure ()
  -- Regular function call
  match ← lookupFnSig fnName with
  | some sig =>
    -- Infer type arguments if not explicitly provided
    let inferredTypeArgs ← do
      if !typeArgs.isEmpty || sig.typeParams.isEmpty then
        pure typeArgs
      else
        let mut inferred : List (String × Ty) := []
        for (arg, (_, pTy)) in args.zip sig.params do
          let argTy ← peekExprType arg
          let bindings := unifyTypes pTy argTy sig.typeParams
          for (name, ty) in bindings do
            if !(inferred.any fun (n, _) => n == name) then
              inferred := inferred ++ [(name, ty)]
        pure (sig.typeParams.map fun tp =>
          match inferred.lookup tp with
          | some ty => ty
          | none => .typeVar tp)
    let mapping := sig.typeParams.zip inferredTypeArgs
    let paramTypes := sig.params.map fun (_, t) => substTy mapping t
    let retTy := substTy mapping sig.retTy
    let mut cArgs : List CExpr := []
    let mut argEvs : List Proof.EvidenceExprV2 := []
    for (arg, pTy) in args.zip paramTypes do
      let cArgEv ← elabExprEv arg (some pTy)
      let cArg := cArgEv.core
      cArgs := cArgs ++ [cArg]
      argEvs := cArgEv.evidence :: argEvs
    -- Use canonical name for intrinsics (e.g., string_substr → string_slice)
    let callName := match intrinsic with
      | some id => id.canonicalName
      | none => fnName
    return ElaboratedExprV2.mk (CExpr.call callName inferredTypeArgs cArgs retTy)
      (match (← getEnv).resolveCallee fnName with
       | some id => Proof.evCall id argEvs.reverse
       | none    => Proof.evUnresolvedCall "call: name has no CallableId")
  | none => throwElab (.undeclaredFunction fnName) span

partial def elabStmtEv (stmt : Stmt) : ElabM ElaboratedStmtV2 := do
  match stmt with
  | .letDecl _ name mutable ty value isGhost =>
    let valHint ← match ty with
      | some t => do let t' ← resolveTypeE t; pure (some t')
      | none => pure none
    -- Elaborate the RHS for validation (it may read runtime state). For a ghost
    -- let we then ERASE it: emit no Core, and record the name as ghost rather
    -- than a runtime var so any later runtime read is reported as a leak.
    let cValEv ← elabExprEv value valHint
    let cVal := cValEv.core
    let finalTy ← match ty with
      | some t => resolveTypeE t
      | none => pure cVal.ty
    if isGhost then
      addGhostVar name
      let declRef ← match ty with
        | some t => do pure (some (← typeRefOf (← resolveTypeE t)))
        | none => pure none
      return ElaboratedStmtV2.mk [] (Proof.EvidenceStmtV2.letBind true declRef cValEv.evidence)
    addVar name finalTy
    let declRef ← match ty with
      | some t => do pure (some (← typeRefOf (← resolveTypeE t)))
      | none => pure none
    return ElaboratedStmtV2.mk [.letDecl name mutable finalTy cVal]
      (Proof.EvidenceStmtV2.letBind false declRef cValEv.evidence)

  | .assign _ name value =>
    match ← lookupVar name with
    | some varTy =>
      let cValEv ← elabExprEv value (some varTy)
      let cVal := cValEv.core
      return ElaboratedStmtV2.mk [.assign (← coreNameOf name) cVal]
        (Proof.EvidenceStmtV2.assign (Proof.evBinderRef (← getEnv).bodyScope name) cValEv.evidence)
    | none => throwElab (.assignToUndeclaredVariable name) (some stmt.getSpan)

  | .return_ _ (some value) =>
    let env ← getEnv
    let cValEv ← elabExprEv value (some env.currentRetTy)
    let cVal := cValEv.core
    return ElaboratedStmtV2.mk [.return_ (some cVal) env.currentRetTy] (Proof.EvidenceStmtV2.ret (some cValEv.evidence))

  | .return_ _ none =>
    let env ← getEnv
    return ElaboratedStmtV2.mk [.return_ none env.currentRetTy] (Proof.EvidenceStmtV2.ret none)

  | .expr sp (.call _sp fnName _typeArgs args) iv =>
    -- Desugar print/println into individual typed print calls
    -- Only if not shadowed by a user/stdlib function with the same name
    let existingFn ← lookupFnSig fnName
    if existingFn.isNone && (fnName == "print" || fnName == "println") then
      let mut stmts : List CStmt := []
      for arg in args do
        let cArgEv ← elabExprEv arg
        let cArg := cArgEv.core
        let printCall := match cArg.ty with
          | .string =>
            CStmt.expr (CExpr.call "print_string" [] [CExpr.borrow cArg (.ref .string)] .unit) false
          | .ref .string | .refMut .string =>
            CStmt.expr (CExpr.call "print_string" [] [cArg] .unit) false
          | .int =>
            CStmt.expr (CExpr.call "print_int" [] [cArg] .unit) false
          | .uint | .i32 | .i16 | .i8 | .u32 | .u16 | .u8 =>
            CStmt.expr (CExpr.call "print_int" [] [CExpr.cast cArg .int] .unit) false
          | .bool =>
            CStmt.expr (CExpr.call "print_bool" [] [cArg] .unit) false
          | .char =>
            CStmt.expr (CExpr.call "print_char" [] [CExpr.cast cArg .int] .unit) false
          | _ =>
            CStmt.expr (CExpr.call "print_string" [] [CExpr.strLit "<unprintable>"] .unit) false
        stmts := stmts ++ [printCall]
      if fnName == "println" then
        stmts := stmts ++ [CStmt.expr (CExpr.call "print_char" [] [CExpr.intLit 10 .int] .unit) false]
      return ElaboratedStmtV2.mk stmts (Proof.evUnhandledStmt "desugared statement sequence")
    -- Desugar variadic append(&mut buf, ...) into typed string_append calls.
    -- Only fires if (a) not shadowed by a user fn, (b) at least one arg,
    -- (c) first arg elaborates to &mut String. Otherwise fall through and
    -- let normal elaboration produce the usual "undeclared function" error.
    else if existingFn.isNone && fnName == "append" then
    match args with
    | bufArg :: rest =>
      let cBufEv ← elabExprEv bufArg
      let cBuf := cBufEv.core
      match cBuf.ty with
      | .refMut .string =>
        let mut stmts : List CStmt := []
        for arg in rest do
          let cArgEv ← elabExprEv arg
          let cArg := cArgEv.core
          let call ← match cArg.ty with
            | .string =>
              pure (CStmt.expr (CExpr.call "string_append" [] [cBuf, CExpr.borrow cArg (.ref .string)] .unit) false)
            | .ref .string | .refMut .string =>
              pure (CStmt.expr (CExpr.call "string_append" [] [cBuf, cArg] .unit) false)
            | .int =>
              pure (CStmt.expr (CExpr.call "string_append_int" [] [cBuf, cArg] .unit) false)
            | .uint | .i32 | .i16 | .i8 | .u32 | .u16 | .u8 =>
              pure (CStmt.expr (CExpr.call "string_append_int" [] [cBuf, CExpr.cast cArg .int] .unit) false)
            | .bool =>
              pure (CStmt.expr (CExpr.call "string_append_bool" [] [cBuf, cArg] .unit) false)
            | .char =>
              pure (CStmt.expr (CExpr.call "string_push_char" [] [cBuf, CExpr.cast cArg .int] .unit) false)
            | _ =>
              throw [{ severity := .error
                     , message := s!"append() argument has unsupported type; expected String/&String/&mut String, Int/Uint/i8..i32/u8..u32, bool, or char"
                     , pass := "elab"
                     , span := some sp
                     , hint := some "pass primitive values or string references; complex values must be formatted first"
                     , code := "E0420" }]
          stmts := stmts ++ [call]
        return ElaboratedStmtV2.mk stmts (Proof.evUnhandledStmt "desugared statement sequence")
      | _ =>
        let cEEv ← elabExprEv (.call _sp fnName _typeArgs args)
        let cE := cEEv.core
        return ElaboratedStmtV2.mk [.expr cE iv] (Proof.EvidenceStmtV2.exprStmt cEEv.evidence iv)
    | [] =>
      let cEEv ← elabExprEv (.call _sp fnName _typeArgs args)
      let cE := cEEv.core
      return ElaboratedStmtV2.mk [.expr cE iv] (Proof.EvidenceStmtV2.exprStmt cEEv.evidence iv)
    else
      let cEEv ← elabExprEv (.call _sp fnName _typeArgs args)
      let cE := cEEv.core
      return ElaboratedStmtV2.mk [.expr cE iv] (Proof.EvidenceStmtV2.exprStmt cEEv.evidence iv)

  | .expr _ e iv =>
    let cEEv ← elabExprEv e
    let cE := cEEv.core
    return ElaboratedStmtV2.mk [.expr cE iv] (Proof.EvidenceStmtV2.exprStmt cEEv.evidence iv)

  | .ifElse _ cond then_ else_ =>
    let cCondEv ← elabExprEv cond (some .bool)
    let cCond := cCondEv.core
    let cThenEv ← elabStmtsEv then_
    let cThen := cThenEv.flatMap (·.core)
    let cElseEv ← match else_ with
      | some stmts => do let cs ← elabStmtsEv stmts; pure (some cs)
      | none => pure none
    let cElse := cElseEv.map (fun l => l.flatMap (·.core))
    return ElaboratedStmtV2.mk [.ifElse cCond cThen cElse]
      (Proof.EvidenceStmtV2.branch cCondEv.evidence (cThenEv.map (·.evidence))
        ((cElseEv.map (fun l => l.map (·.evidence))).getD []))

  | .while_ _ cond body label =>
    let cCondEv ← elabExprEv cond (some .bool)
    let cCond := cCondEv.core
    -- The frame covers the BODY only. A `break` in the condition would not be inside
    -- this loop, and pushing before the condition would mis-target it.
    let outer ← getEnv
    setEnv { outer with loopFrames := label :: outer.loopFrames }
    let cBodyEv ← elabStmtsEv body
    let cBody := cBodyEv.flatMap (·.core)
    let inner ← getEnv
    setEnv { inner with loopFrames := outer.loopFrames }
    return ElaboratedStmtV2.mk [.while_ cCond cBody label []]
      (Proof.EvidenceStmtV2.loop cCondEv.evidence [] none (cBodyEv.map (·.evidence)))

  | .forLoop _ init cond step body label =>
    -- Desugar: for (init; cond; step) { body } → init; while cond { body; step }
    --
    -- The evidence MIRRORS THE LOWERING rather than the surface form: a block holding
    -- the init, then a loop whose body is `body ++ step`. That is what the program
    -- does, and describing the surface instead would record a loop whose body omits
    -- the step — a body that runs code the evidence does not mention.
    let mut result : List CStmt := []
    let mut initEv : List Proof.EvidenceStmtV2 := []
    match init with
    | some initStmt =>
      let cInitEv ← elabStmtEv initStmt
      result := result ++ cInitEv.core
      initEv := [cInitEv.evidence]
    | none => pure ()
    let cCondEv ← elabExprEv cond (some .bool)
    let cCond := cCondEv.core
    -- The frame covers the BODY and the STEP, matching `while_`: a `break` in the
    -- CONDITION is not inside this loop. Without this push a `break` in a for-loop
    -- body resolves against the enclosing loop, or against none at all — a defect
    -- that was invisible while the whole construct was a gap, because a gap node
    -- discards its subtree.
    let outer ← getEnv
    setEnv { outer with loopFrames := label :: outer.loopFrames }
    let cBodyEv ← elabStmtsEv body
    let cBody := cBodyEv.flatMap (·.core)
    let stepRes ← match step with
      | some stepStmt => do
        let e ← elabStmtEv stepStmt
        pure (e.core, [e.evidence])
      | none => pure ([], [])
    let cStep := stepRes.1
    let stepEv := stepRes.2
    let inner ← getEnv
    setEnv { inner with loopFrames := outer.loopFrames }
    let whileBody := cBody ++ cStep
    result := result ++ [.while_ cCond whileBody label cStep]
    return ElaboratedStmtV2.mk result
      (Proof.EvidenceStmtV2.block
        (initEv ++ [Proof.EvidenceStmtV2.loop cCondEv.evidence [] none
                      (cBodyEv.map (·.evidence) ++ stepEv)]))

  | .fieldAssign _ obj field value =>
    let cObjEv ← elabExprEv obj
    let cObj := cObjEv.core
    -- 6D#3: `.field =` on a heap shell derefs first (p.f = v ≡ (*p).f = v —
    -- the old `->` desugar folded into `.`).
    --
    -- The evidence tracks the deref, because the place is what the program writes
    -- through: `p.f = v` on a heap shell writes through a dereference and `q.f = v` on
    -- a plain struct does not. Describing both as a bare field projection would give
    -- one encoding to two different writes.
    let derefed := match cObj.ty with
      | .heap _ | .heapArray _ | .ref (.heap _) | .refMut (.heap _) => true
      | _ => false
    let cObjPlaceEv := if derefed then Proof.evDeref cObjEv.evidence else cObjEv.evidence
    let cObj := match cObj.ty with
      | .heap t | .heapArray t => CExpr.deref cObj t
      | .ref (.heap t) | .refMut (.heap t) => CExpr.deref cObj t
      | _ => cObj
    -- Pass the field's declared type as the value hint so integer literals
    -- pick the right width. Without this, `c.n = 100` where `n: i32` would
    -- elaborate `100` as `Int` (i64) and codegen would emit `store i64 100`
    -- to a 4-byte field — UB that the LLVM optimiser deletes at -O2.
    let innerObjTy := match cObj.ty with
      | .ref t | .refMut t | .ptrMut t | .ptrConst t => t
      | t => t
    let (sName, tArgs) := match innerObjTy with
      | .named n => (n, ([] : List Ty))
      | .generic n a _ => (n, a)
      | _ => ("", [])
    let env ← getEnv
    -- Resolve the field's TYPE and its IDENTITY in one pass. Two lookups keyed on the
    -- same name is how the owner and the type drift apart; the identity is an output of
    -- this resolution, not a second search for it.
    let mut fieldTy : Option Ty := none
    let mut fieldOwner : Option TypeId := none
    match env.structs.find? fun s => s.name == sName with
    | some sd =>
      match sd.fields.find? fun f => f.name == field with
      | some f =>
        recordFieldUse sd field
        let mapping := sd.typeParams.zip tArgs
        fieldTy := some (substTy mapping f.ty)
        fieldOwner := sd.typeId?
      | none => pure ()
    | none => pure ()
    let cValEv ← elabExprEv value fieldTy
    let cVal := cValEv.core
    -- An owner-less field is REFUSED, not encoded under its spelling: two same-spelled
    -- fields on different types would otherwise share one identity in the place.
    let placeEv := match fieldOwner with
      | some owner => Proof.evField { owner := owner, field := field } cObjPlaceEv
      | none => Proof.evUnhandledExpr "field place: owning type has no TypeId"
    return ElaboratedStmtV2.mk [.fieldAssign cObj field cVal]
      (Proof.EvidenceStmtV2.assign placeEv cValEv.evidence)

  | .derefAssign _ target value =>
    let cTargetEv ← elabExprEv target
    let cTarget := cTargetEv.core
    let innerTy := match cTarget.ty with
      | .ref t => t | .refMut t => t
      | .ptrMut t => t | .ptrConst t => t
      | _ => .placeholder
    let cValEv ← elabExprEv value (some innerTy)
    let cVal := cValEv.core
    return ElaboratedStmtV2.mk [.derefAssign cTarget cVal]
      (Proof.EvidenceStmtV2.assign (Proof.evDeref cTargetEv.evidence) cValEv.evidence)

  | .arrayIndexAssign _ arr index value =>
    let cArrEv ← elabExprEv arr
    let cArr := cArrEv.core
    let cIdxEv ← elabExprEv index (some .int)
    let cIdx := cIdxEv.core
    let cValEv ← elabExprEv value
    let cVal := cValEv.core
    return ElaboratedStmtV2.mk [.arrayIndexAssign cArr cIdx cVal]
      (Proof.EvidenceStmtV2.assign (Proof.evIndex cArrEv.evidence cIdxEv.evidence) cValEv.evidence)

  | .break_ _ value label =>
    match value with
    | some v =>
      let cVEv ← elabExprEv v
      let cV := cVEv.core
      return ElaboratedStmtV2.mk [.break_ (some cV) label]
        (match Proof.evLoopTarget? (← getEnv).loopFrames label with
         | some t => Proof.EvidenceStmtV2.breakStmt t (some cVEv.evidence)
         | none   => Proof.evUnhandledStmt "break with no resolvable loop target")
    | none =>
      return ElaboratedStmtV2.mk [.break_ none label]
        (match Proof.evLoopTarget? (← getEnv).loopFrames label with
         | some t => Proof.EvidenceStmtV2.breakStmt t none
         | none   => Proof.evUnhandledStmt "break with no resolvable loop target")

  | .continue_ _ label =>
    return ElaboratedStmtV2.mk [.continue_ label]
      (match Proof.evLoopTarget? (← getEnv).loopFrames label with
       | some t => Proof.EvidenceStmtV2.continueStmt t
       | none   => Proof.evUnhandledStmt "continue with no resolvable loop target")

  | .defer _ body =>
    let cBodyEv ← elabExprEv body
    let cBody := cBodyEv.core
    return ElaboratedStmtV2.mk [.defer cBody] (Proof.EvidenceStmtV2.deferStmt cBodyEv.evidence)

  | .borrowIn _ var ref region isMut body =>
    let varTy ← match ← lookupVar var with
      | some ty => pure ty
      | none => throwElab (.borrowUndeclaredVariable var) (some stmt.getSpan)
    let refTy := if isMut then Ty.refMut varTy else Ty.ref varTy
    addVar ref refTy
    let cBodyEv ← elabStmtsEv body
    let cBody := cBodyEv.flatMap (·.core)
    return ElaboratedStmtV2.mk [.borrowIn var ref region isMut refTy cBody]
      (Proof.EvidenceStmtV2.block (cBodyEv.map (·.evidence)))


  -- These are desugared by desugarStmts before elabStmtEv is called.
  -- Catch-all for exhaustiveness — should never fire.
  | .letDestructure sp _ _ _ _ _ =>
    throwElab (.unknownEnumType "internal: letDestructure not desugared") (some sp)
  | .letStructDestructure sp structName bindings value =>
    -- Linear-checked natively in Check; expanded here (past the linear checker) to a
    -- hidden temp + field-access binds. Sound at runtime: the source is moved into the
    -- temp, each field value is copied into its binding, and the temp is dead storage
    -- afterward (its resource has moved to the bindings). See docs/language/OWNERSHIP_MODEL.md.
    let tmpName := "__destr_" ++ structName
    let tmpLet := Stmt.letDecl sp tmpName false none value false
    let fieldLets := bindings.map fun b =>
      Stmt.letDecl sp b false none (Expr.fieldAccess sp (Expr.ident sp tmpName) b) false
    -- A destructure desugars to several statements; evidence keeps them as ONE block so
    -- the tree mirrors the source construct rather than the desugaring.
    let parts ← elabStmtsEv ([tmpLet] ++ fieldLets)
    return ElaboratedStmtV2.mk (parts.flatMap (·.core))
      (Proof.EvidenceStmtV2.block (parts.map (·.evidence)))
  -- assert(e)/assume(e): proof-only, ERASED before Core (like contracts/ghost).
  -- Not elaborated — the condition may legally read ghost bindings (it is a proof
  -- context), which elabExprEv would otherwise reject as a runtime ghost leak. The
  -- condition is type-checked in Check and scope/purity-checked in the report.
  -- assert/assume are ERASED and deliberately NOT elaborated (their condition may read
  -- ghost bindings legally). So no evidence exists for the predicate, and both must
  -- REFUSE rather than emit an empty statement: an `assume` that the subject does not
  -- record would let a proof lean on it and still report unqualified. A gap makes the
  -- subject incomplete, which is the honest answer until the predicate is encodable in
  -- a proof context.
  | .assert_ _ pred =>
    return ElaboratedStmtV2.mk [] (Proof.EvidenceStmtV2.assertStmt (← proofPredicateEv pred))
  | .assume_ _ pred =>
    return ElaboratedStmtV2.mk [] (Proof.EvidenceStmtV2.assumeStmt (← proofPredicateEv pred))

/-- A proof-only predicate, as evidence.

    `assert` and `assume` are ERASED before Core and were never elaborated, so their
    predicates reached no byte at all — 39 of the corpus's refusals. They are elaborated
    here for EVIDENCE ONLY: the Core is discarded, because emitting it would make a
    proof-only construct generate code.

    Two properties this must have, and neither is optional:

    1. IT CANNOT FAIL THE COMPILATION. Nothing elaborated these before, so any throw is a
       program that used to build and now does not. A predicate that will not elaborate —
       most importantly one reading a `ghost let`, which is legal here and rejected by
       `elabExprEv` — becomes a typed GAP, refusing the subject rather than breaking the
       build.
    2. IT CANNOT LEAK STATE. The environment is restored afterwards, so a proof-only
       expression cannot add runtime bindings or advance anything a later statement reads.
       `freshBinder` is deliberately carried forward instead, on the same grounds as
       `restoreScope`: Core names must stay unique across the whole function even when the
       expression that minted them was thrown away.

    The coverage flag is restored too. A predicate that could not be described makes the
    subject incomplete through its GAP, which is the honest channel; letting it also flip
    `bodyIdentityCovered` would report a second, different fault for one cause. -/
partial def proofPredicateEv (pred : Expr) : ElabM Proof.EvidenceExprV2 := do
  let saved ← getEnv
  match (elabExprEv pred none).run saved with
  | (.ok r, after) =>
    setEnv { saved with freshBinder := after.freshBinder }
    pure r.evidence
  | (.error _, after) =>
    setEnv { saved with freshBinder := after.freshBinder }
    pure (Proof.evUnhandledExpr "proof predicate does not elaborate (e.g. reads a ghost binding)")

partial def elabStmtsEv (stmts : List Stmt) (valueHint : Option Ty := none) : ElabM (List ElaboratedStmtV2) := do
  let mut result : List ElaboratedStmtV2 := []
  let mut accumulated : Diagnostics := []
  let lastIdx := stmts.length - 1
  let mut idx := 0
  for s in stmts do
    let isLast := idx == lastIdx
    idx := idx + 1
    let envBefore ← getEnv
    -- Flow an enclosing value hint (from an if/match used as a value) into the
    -- block's TRAILING value expression only, so a flexible literal/binop in a
    -- branch adopts the result width instead of defaulting to Int — the same
    -- decision Check makes when it types each branch's trailing expr with the
    -- hint. Without this, `let x: i32 = if c { 2e9 + 2e9 } else { 0 }` typed the
    -- branch Int (i64) while stamping the node i32, so interp (i64, no truncation)
    -- and the compiled binary (i32 slot, truncates) diverged. Calls keep their
    -- own elaboration path (print/append desugaring); their type is fixed by the
    -- signature, so no hint is needed.
    let action : ElabM ElaboratedStmtV2 :=
      match s, valueHint, isLast with
      | .expr _ e true, some h, true =>
        match e with
        | .call .. => elabStmtEv s
        | _ => do
          let cEEv ← elabExprEv e (some h)
          pure (ElaboratedStmtV2.mk [CStmt.expr cEEv.core true]
                  (Proof.EvidenceStmtV2.exprStmt cEEv.evidence true))
      | _, _, _ => elabStmtEv s
    let r := action.run envBefore |>.run
    match r with
    | (.ok cs, envAfter) =>
      setEnv envAfter
      -- PREPEND, reversed once below. Appending per statement is quadratic in body length.
      result := cs :: result
    | (.error ds, _) =>
      accumulated := accumulated ++ ds
      -- Restore env so subsequent statements see a consistent state.
      -- For let-declarations, add the variable with its declared type (or placeholder)
      -- so later statements referencing it don't cascade spurious errors.
      setEnv envBefore
      match s with
      | .letDecl _ name _ ty _ _ =>
        let placeholderTy := ty.getD .placeholder
        addVar name placeholderTy
      | _ => pure ()
  if !accumulated.isEmpty then
    throw accumulated
  -- Statement ORDER is semantic, so the accumulator is reversed exactly once here.
  return result.reverse

end

-- ============================================================
-- Function and module elaboration
-- ============================================================

def elabFn (f : FnDef) (implTy : Option Ty := none)
    : ElabM (CFnDef × Proof.ProofBodyIdentityInputsV2 × Proof.EvidenceBodyDraftV2) := do
  let env ← getEnv
  -- FRESH LEXICAL SCOPE PER FUNCTION. Without this the binder frame ACCUMULATES across
  -- declarations in a module, so a parameter's resolved index depends on what was elaborated
  -- before it. Measured: `calls.inc(x)` — a single parameter — encoded as `r3:0.0` in
  -- proof_patterns/composition and `r3:0.1` in composition_trusted_helper, from identical
  -- source. `x` cannot be at index 1 of its own parameter frame.
  --
  -- That made the subject digest a function of DECLARATION ORDER rather than of the function,
  -- which is why the V2 freeze was revoked: two projects, and even one project with an unrelated
  -- declaration inserted above, disagreed about the same callable's body.
  --
  -- Reset here rather than at the end of the previous function: an early return or an error path
  -- would skip a teardown, and the whole class of defect is a frame that outlives its owner.
  setEnv { env with bodyScope := {}, pendingFrame := false }
  let env ← getEnv
  -- Set up type params and return type
  let allTypeParams := f.typeParams
  let params := f.params.map fun p =>
    let pty := match implTy with | some it => resolveSelfTy p.ty it | none => p.ty
    (p.name, pty)
  let retTy := match implTy with | some it => resolveSelfTy f.retTy it | none => f.retTy
  -- Resolve type params in param/return types
  let resolveTP (ty : Ty) : Ty :=
    let rec go : Ty → Ty
      | .named n => if allTypeParams.contains n then .typeVar n else .named n
      | .ref t => .ref (go t)
      | .refMut t => .refMut (go t)
      | .ptrMut t => .ptrMut (go t)
      | .ptrConst t => .ptrConst (go t)
      | .generic "Heap" [inner] _ => .heap (go inner)
      | .generic "HeapArray" [inner] _ => .heapArray (go inner)
      | .generic n args caps => .generic n (args.map go) caps
      | .array t n => .array (go t) n
      | .fn_ ps cs rt => .fn_ (ps.map go) cs (go rt)
      | .heap t => .heap (go t)
      | .heapArray t => .heapArray (go t)
      | t => t
    go ty
  let params := params.map fun (n, t) => (n, resolveTP t)
  let retTy := resolveTP retTy
  setEnv { env with
    currentTypeParams := allTypeParams
    currentTypeBounds := f.typeBounds
    currentRetTy := retTy
    currentImplType := implTy
    bodyIdentityCovered := true }
  -- Add parameters to scope (resolve type aliases so params don't carry unresolved alias names)
  for (pname, pty) in params do
    let resolvedPty ← resolveTypeE pty
    addVar pname resolvedPty
  -- Elaborate body
  -- THE COLLECTION POINT. The function's evidence body is assembled STRUCTURALLY here,
  -- from the same traversal that produced Core — not accumulated into a side channel,
  -- which would lose branch, loop, pattern and defer nesting and so fix only the
  -- shallow collisions while the structural ones survive invisibly.
  let cBodyEv ← elabStmtsEv f.body
  let cBody := cBodyEv.flatMap (·.core)
  let evidenceBody : Proof.EvidenceBodyDraftV2 :=
    { statements := cBodyEv.map (·.evidence) }
  -- Restore env
  let envAfter ← getEnv
  -- ONE PRODUCER. The flat view is DERIVED from the structural body rather than
  -- accumulated beside it: two producers of one fact is the defect class R-0004 exists to
  -- close, and a side-channel accumulator drifts silently because nothing compares them.
  --
  -- The derived view is a SUPERSET, not a copy — measured, not assumed. It carries pattern
  -- field identities the accumulator never recorded, which is why the containment gate
  -- checks subsequence rather than equality.
  let bodyIdentityInputs : Proof.ProofBodyIdentityInputsV2 :=
    { uses := Proof.flatUsesOf evidenceBody
      covered := envAfter.bodyIdentityCovered }
  setEnv { envAfter with
    vars := env.vars
    currentTypeParams := env.currentTypeParams
    currentTypeBounds := env.currentTypeBounds
    currentRetTy := env.currentRetTy
    currentImplType := env.currentImplType
    bodyIdentityCovered := env.bodyIdentityCovered }
  -- Resolve type aliases in output param/return types so Core IR doesn't carry alias names
  let resolvedParams ← params.mapM fun (n, t) => do pure (n, ← resolveTypeE t)
  let resolvedRetTy ← resolveTypeE retTy
  let cfn : CFnDef := {
    name := f.name
    typeParams := allTypeParams
    params := resolvedParams
    retTy := resolvedRetTy
    body := cBody
    isPublic := f.isPublic
    isTest := f.isTest
    isTrusted := f.isTrusted
    isEntryPoint := f.name == mainFnName
    capSet := f.capSet
    declSpan := some f.span
  }
  return (cfn, bodyIdentityInputs, evidenceBody)

-- ============================================================
-- Submodule function name prefixing
-- ============================================================

mutual
/-- Rename function references in a CExpr tree using a lookup table. -/
partial def renameFnExpr (rmap : List (String × String)) : CExpr → CExpr
  | .call callee targs args ty =>
    -- Only a DIRECT callee names a global function that submodule prefixing may
    -- rewrite. An indirect callee names a local binding, which lives in no
    -- module namespace: renaming it would point the call at a submodule
    -- function that merely shares the local's name — bug 050's mistake, one
    -- pass earlier.
    let callee' := match callee with
      | .direct name => Callee.direct (rmap.lookup name |>.getD name)
      | .indirect binding => Callee.indirect binding
    .call callee' targs (args.map (renameFnExpr rmap)) ty
  | .fnRef name ty =>
    let name' := rmap.lookup name |>.getD name
    .fnRef name' ty
  | .ident name ty =>
    -- Only rename function-typed idents (fn refs used as values), not local variables
    match ty with
    | .fn_ .. =>
      -- KNOWN GAP (bug 050's family, tracked in its doc): this cannot tell a
      -- global fn used as a value from a LOCAL of fn type, so a local whose name
      -- collides with a submodule function is renamed to that function. Unlike
      -- the call case above, Core carries no marker for it — `.ident` has only a
      -- name and a type. Closing it needs the same treatment as the callee:
      -- record at elaboration, where the scope is still known.
      let name' := rmap.lookup name |>.getD name
      .ident name' ty
    | _ => .ident name ty
  | .binOp op l r ty => .binOp op (renameFnExpr rmap l) (renameFnExpr rmap r) ty
  | .unaryOp op e ty => .unaryOp op (renameFnExpr rmap e) ty
  | .structLit n ta fs ty =>
    .structLit n ta (fs.map fun (fn, e) => (fn, renameFnExpr rmap e)) ty
  | .fieldAccess obj f ty => .fieldAccess (renameFnExpr rmap obj) f ty
  | .enumLit en v ta fs ty =>
    .enumLit en v ta (fs.map fun (fn, e) => (fn, renameFnExpr rmap e)) ty
  | .match_ scrut arms ty =>
    .match_ (renameFnExpr rmap scrut) (arms.map (renameFnArm rmap)) ty
  | .borrow inner ty => .borrow (renameFnExpr rmap inner) ty
  | .borrowMut inner ty => .borrowMut (renameFnExpr rmap inner) ty
  | .deref inner ty => .deref (renameFnExpr rmap inner) ty
  | .arrayLit elems ty => .arrayLit (elems.map (renameFnExpr rmap)) ty
  | .arrayIndex arr idx ty =>
    .arrayIndex (renameFnExpr rmap arr) (renameFnExpr rmap idx) ty
  | .cast inner t => .cast (renameFnExpr rmap inner) t
  | .allocCall inner alloc ty =>
    .allocCall (renameFnExpr rmap inner) (renameFnExpr rmap alloc) ty
  | .ifExpr cond then_ else_ ty =>
    .ifExpr (renameFnExpr rmap cond)
      (renameFnStmts rmap then_) (renameFnStmts rmap else_) ty
  | e => e

partial def renameFnArm (rmap : List (String × String)) : CMatchArm → CMatchArm
  | .enumArm en v binds guard body => .enumArm en v binds (guard.map (renameFnExpr rmap)) (renameFnStmts rmap body)
  | .litArm val guard body =>
    .litArm (renameFnExpr rmap val) (guard.map (renameFnExpr rmap)) (renameFnStmts rmap body)
  | .varArm b ty guard body => .varArm b ty (guard.map (renameFnExpr rmap)) (renameFnStmts rmap body)
  | .rangeArm lo hi incl guard body =>
    .rangeArm (renameFnExpr rmap lo) (renameFnExpr rmap hi) incl (guard.map (renameFnExpr rmap)) (renameFnStmts rmap body)

partial def renameFnStmt (rmap : List (String × String)) : CStmt → CStmt
  | .letDecl n m ty val => .letDecl n m ty (renameFnExpr rmap val)
  | .assign n val => .assign n (renameFnExpr rmap val)
  | .return_ (some v) ty => .return_ (some (renameFnExpr rmap v)) ty
  | .expr e iv => .expr (renameFnExpr rmap e) iv
  | .ifElse c t el =>
    .ifElse (renameFnExpr rmap c)
      (renameFnStmts rmap t) (el.map (renameFnStmts rmap))
  | .while_ c body lbl step =>
    .while_ (renameFnExpr rmap c)
      (renameFnStmts rmap body) lbl (renameFnStmts rmap step)
  | .fieldAssign obj f val =>
    .fieldAssign (renameFnExpr rmap obj) f (renameFnExpr rmap val)
  | .derefAssign target val =>
    .derefAssign (renameFnExpr rmap target) (renameFnExpr rmap val)
  | .arrayIndexAssign arr idx val =>
    .arrayIndexAssign (renameFnExpr rmap arr)
      (renameFnExpr rmap idx) (renameFnExpr rmap val)
  | .break_ (some v) lbl => .break_ (some (renameFnExpr rmap v)) lbl
  | .defer body => .defer (renameFnExpr rmap body)
  | .borrowIn v r reg isMut ty body =>
    .borrowIn v r reg isMut ty (renameFnStmts rmap body)
  | s => s

partial def renameFnStmts (rmap : List (String × String))
    (stmts : List CStmt) : List CStmt :=
  stmts.map (renameFnStmt rmap)
end

/-- Prefix all function definitions and internal call sites in a CModule.
    Used to give submodule functions unique LLVM symbols
    (e.g., `add` in submodule `math` becomes `math_add`).
    Extern functions are NOT prefixed (they reference real C symbols). -/
partial def prefixModuleFnNames (pfx : String) (cm : CModule) : CModule :=
  -- Build rename map: bare name → prefixed name for all non-extern functions
  let fnRenames : List (String × String) :=
    cm.functions.map fun f => (f.name, pfx ++ "_" ++ f.name)
  -- Also prefix impl method names referenced in traitImpls
  let implRenames : List (String × String) :=
    cm.traitImpls.foldl (fun acc ti =>
      acc ++ (ti.methodNames.map fun mn => (mn, pfx ++ "_" ++ mn))
    ) []
  let rmap := fnRenames ++ implRenames
  -- Prefix function definitions and rewrite their bodies
  let prefixedFns := cm.functions.map fun f =>
    { f with
      name := pfx ++ "_" ++ f.name
      body := renameFnStmts rmap f.body }
  -- Prefix trait impl method names
  let prefixedTraitImpls := cm.traitImpls.map fun ti =>
    { ti with methodNames := ti.methodNames.map fun mn =>
        rmap.lookup mn |>.getD mn }
  -- Recursively prefix nested submodules
  let prefixedSubs := cm.submodules.map fun sub =>
    prefixModuleFnNames (pfx ++ "_" ++ sub.name) sub
  { cm with
    functions := prefixedFns
    traitImpls := prefixedTraitImpls
    submodules := prefixedSubs }

-- ============================================================
-- Build environment from module (mirrors checkModule setup)
-- ============================================================

-- Builtin function signatures: shared definition in BuiltinSigs.lean (builtinFnSigs)

partial def elabModule (m : Module) (summary : FileSummary)
    (imports : ResolvedImports := {})
    (summaryTable : List (String × FileSummary) := [])
    (prefixSubs : Bool := true)
    (proofModulePath : String := "")
    (proofFnPrefix : String := "") : Except Diagnostics CModule :=
  -- The final definition path/name used by ProofCore. Recursive elaboration
  -- happens before `prefixModuleFnNames`, so carrying these two facts explicitly
  -- prevents declaration facts from being keyed under the pre-prefix spelling.
  let thisProofPath := if proofModulePath.isEmpty then m.name else proofModulePath
  -- Use pre-built summaries from FileSummary
  let userFnSigs := summary.functions
  let externSigs := summary.externFnSigs
  -- Submodule fn sigs from pre-built submodule summaries
  let submoduleSigs : List (String × FnSummary) := summary.submoduleSummaries.foldl (fun acc (subName, subSummary) =>
    let fnSigs := subSummary.functions.map fun (fnName, sig) =>
      (subName ++ "_" ++ fnName, sig)
    let efSigs := subSummary.externFnSigs.map fun (efName, sig) =>
      (subName ++ "_" ++ efName, sig)
    acc ++ fnSigs ++ efSigs
  ) []
  -- Impl method sigs (pre-built + imported, then resolve Self)
  let localImplSigs := summary.implMethodSigs
  let allImplBlocks := imports.implBlocks ++ m.implBlocks
  let allTraitImpls := imports.traitImpls ++ m.traitImpls
  let implMethodSigs := resolveImplMethodSigs (imports.implMethodSigs ++ localImplSigs)
      allImplBlocks allTraitImpls
  let traitImplMethodSigs : List (String × FnSummary) := []
  -- Combine all sigs
  let allSigs := imports.functions ++ userFnSigs ++ builtinFnSigs ++ externSigs
                 ++ submoduleSigs ++ implMethodSigs ++ traitImplMethodSigs
  -- Build structs / enums
  -- One owner: `Concrete.Resolve.BuiltinEnums` (bug 065). Both the builtin
  -- enums and the shadowing rule were stated twice, and Check consulted
  -- different inputs than Elab.
  let builtinEnumList := builtinEnums summary.enums imports.enums
  -- The summary carries definition-site TypeId provenance. Raw parsed
  -- declarations deliberately do not: absence before resolution fails closed.
  let allStructs := imports.structs ++ summary.structs
  let allEnums := builtinEnumList ++ imports.enums ++ summary.enums
  let localTypeAliases := m.typeAliases.map fun ta => (ta.name, ta.targetTy)
  -- Transitively close alias chains (see closeAliasMap); cycles rejected upstream.
  let typeAliasMap := closeAliasMap (imports.typeAliases ++ localTypeAliases)
  let constantsMap := m.constants.map fun c => (c.name, c.ty)
  let builtinDestroyTrait : TraitDef := {
    name := destroyTraitName
    methods := [{ name := destroyMethodName, params := [], retTy := .unit, selfKind := some .ref }]
    builtinId := some .destroy
  }
  let allTraits := builtinDestroyTrait :: m.traits
  -- All named fn sigs for fnRef
  let fnSigPairs : List (String × FnSummary) :=
    userFnSigs ++ implMethodSigs ++ traitImplMethodSigs
  let finalDeclName := fun (f : FnDef) (implTy : Option Ty) =>
    let localName := match implTy with
      | some it =>
        let tn := tyName it
        if tn != "" then tn ++ "_" ++ f.name else f.name
      | none => f.name
    if proofFnPrefix.isEmpty then localName else proofFnPrefix ++ "_" ++ localName
  let regularFns := m.functions.map fun f => (f, (none : Option Ty))
  let implMethodPairs := m.implBlocks.foldl (fun acc ib =>
    -- `impl<cap C> Writer<C>`: the self type carries the impl's capability parameters
    -- by name (R-0484); the parser already joined them to each method's own.
    let implTy := if ib.typeParams.isEmpty && ib.capParams.isEmpty then tyFromName ib.typeName
                  else Ty.generic ib.typeName (ib.typeParams.map Ty.typeVar)
                         (ib.capParams.map fun c => CapSet.concrete [c])
    acc ++ ib.methods.map fun f =>
      ({ f with typeParams := ib.typeParams ++ f.typeParams,
                typeBounds := ib.typeBounds ++ f.typeBounds,
                isTrusted := f.isTrusted || ib.isTrusted }, some implTy)
  ) ([] : List (FnDef × Option Ty))
  let traitImplMethodPairs := m.traitImpls.foldl (fun acc tb =>
    let implTy := if tb.typeParams.isEmpty then tyFromName tb.typeName
                  -- Trait impls take no capability parameters (refused at parse).
                  else Ty.generic tb.typeName (tb.typeParams.map Ty.typeVar) []
    acc ++ tb.methods.map fun f =>
      ({ f with typeParams := tb.typeParams ++ f.typeParams,
                typeBounds := tb.typeBounds ++ f.typeBounds,
                isTrusted := f.isTrusted || tb.isTrusted }, some implTy)
  ) ([] : List (FnDef × Option Ty))
  let allFnPairs := regularFns ++ implMethodPairs ++ traitImplMethodPairs
  let localCallIds : List (String × CallableId) := allFnPairs.map fun (f, implTy) =>
    let localKey := match implTy with
      | some it =>
        let tn := tyName it
        if tn != "" then tn ++ "_" ++ f.name else f.name
      | none => f.name
    (localKey, CallableId.ofUser thisProofPath (finalDeclName f implTy) f.typeParams.length)
  let importedCallIds : List (String × CallableId) := m.imports.flatMap fun imp =>
    let summary? := match summary.submoduleSummaries.find? fun (n, _) => n == imp.moduleName with
      | some (_, s) => some (s, true)
      | none => (summaryTable.lookup imp.moduleName).map fun s => (s, false)
    match summary? with
    | none => []
    | some (s, isLocalSubmodule) => imp.symbols.filterMap fun sym =>
      let localName := sym.effectiveName
      match s.functions.find? fun (n, _) => n == sym.name with
      | some (_, sig) =>
        let defModule := if isLocalSubmodule then thisProofPath ++ "." ++ imp.moduleName else s.name
        let declName := if isLocalSubmodule then
            let p := if proofFnPrefix.isEmpty then imp.moduleName
                     else proofFnPrefix ++ "_" ++ imp.moduleName
            p ++ "_" ++ sym.name
          else sym.name
        some (localName, CallableId.ofUser defModule declName sig.typeParams.length)
      | none =>
        if s.externFnSigs.any fun (n, _) => n == sym.name then
          some (localName, CallableId.ofExtern sym.name)
        else none
  -- IMPORTED IMPL METHODS. `importedCallIds` searches only `s.functions`, so a method on
  -- an imported TYPE — `w.write_str(..)` where `Writer` comes from `std.io` — had no
  -- CallableId and every call to one refused the subject. 18 of the corpus's remaining
  -- 21 unresolvable callees were this, and none of them were a resolution defect: the
  -- table simply never contained the entries.
  --
  -- Keyed by the LOCAL name, IDENTIFIED by the defining module and the ORIGINAL mangled
  -- name — the same split `importedCallIds` already makes for functions, so a spelling
  -- cannot become identity. Verified: `a.P_get` and `b.P_get` stay distinct for
  -- same-spelled methods on same-spelled types in two modules.
  --
  -- The ALIAS half of that split is currently unreachable rather than tested: Check
  -- rejects a method call on an aliased imported type outright (E0264, `no method on
  -- type Q`), before and after this change alike. The split is written the correct way
  -- round anyway, so it is right if that limitation is lifted.
  let importedMethodCallIds : List (String × CallableId) := m.imports.flatMap fun imp =>
    let summary? := match summary.submoduleSummaries.find? fun (n, _) => n == imp.moduleName with
      | some (_, s) => some (s, true)
      | none => (summaryTable.lookup imp.moduleName).map fun s => (s, false)
    match summary? with
    | none => []
    | some (s, isLocalSubmodule) => imp.symbols.flatMap fun sym =>
      let defModule := if isLocalSubmodule then thisProofPath ++ "." ++ imp.moduleName else s.name
      let prefixOrig := sym.name ++ "_"
      s.implMethodSigs.filterMap fun (mangled, sig) =>
        if mangled.startsWith prefixOrig then
          let methodName := mangled.drop prefixOrig.length
          let localKey := sym.effectiveName ++ "_" ++ methodName
          let declName := if isLocalSubmodule then
              let pfx := if proofFnPrefix.isEmpty then imp.moduleName
                         else proofFnPrefix ++ "_" ++ imp.moduleName
              pfx ++ "_" ++ mangled
            else mangled
          some (localKey, CallableId.ofUser defModule declName sig.typeParams.length)
        else none
  -- THE CAPABILITY REQUIREMENT OF EVERY IMPORTED CALLABLE, keyed by the local
  -- spelling a call site here uses. Same import walk as `importedCallIds` and
  -- `importedMethodCallIds` above, for the same reason: the dependency's Core is
  -- not in this unit, so the requirement has to travel with the module or it is
  -- simply not enforced (bug 071 — `std.env.get` is `with(Alloc, Env, Unsafe)`
  -- and a caller declaring nothing could call it and read the environment).
  --
  -- Functions, externs and impl methods all, because the hole was never specific
  -- to one of them: four of the six measured escapes were free functions.
  let importedFnCaps : List (String × CapSet) := m.imports.flatMap fun imp =>
    let summary? := match summary.submoduleSummaries.find? fun (n, _) => n == imp.moduleName with
      | some (_, s) => some s
      | none => summaryTable.lookup imp.moduleName
    match summary? with
    | none => []
    | some s => imp.symbols.flatMap fun sym =>
      let localName := sym.effectiveName
      let fnCap := match s.functions.find? fun (n, _) => n == sym.name with
        | some (_, sig) => [(localName, sig.capSet)]
        | none => []
      let extCap := match s.externFnSigs.find? fun (n, _) => n == sym.name with
        | some (_, sig) => [(localName, sig.capSet)]
        | none => []
      -- Methods arrive mangled as `Type_method`; the call site spells them
      -- `localType_method`, so the prefix is re-based onto the local name.
      let prefixOrig := sym.name ++ "_"
      let methCaps := s.implMethodSigs.filterMap fun (mangled, sig) =>
        if mangled.startsWith prefixOrig then
          some (localName ++ "_" ++ mangled.drop prefixOrig.length, sig.capSet)
        else none
      fnCap ++ extCap ++ methCaps
  -- PRELUDE / IMPLICITLY-AVAILABLE TYPES. The walk above is driven by `m.imports`, so a
  -- method on a type that needs NO import statement — `String`, and anything else the
  -- prelude supplies — never entered the table. `String::drop` declares `with(Alloc)`
  -- and a function declaring nothing could still call it: the program built and ran.
  -- That was bug 071's last open path, and it survived two rounds of gating because the
  -- newer gate only exercised IMPORTED methods (`RawCursor::read_u8`) and ASSOCIATED
  -- calls (`TcpStream::connect`), both of which do have an import to walk.
  --
  -- Keyed by the MANGLED spelling the call site emits, `<module>_<Type>_<method>`, which
  -- is what `--report trust-edges` shows for these calls (`string_String_drop`). That is
  -- specific enough not to collide the way a bare method name would: `drop` alone is
  -- declared on a dozen std types.
  -- RECURSING INTO SUBMODULES is the whole trick. `summaryTable` is keyed at the PACKAGE
  -- level, so the `std` entry carries no `implMethodSigs` of its own — `String`'s methods
  -- live in the `string` SUBMODULE summary, and the mangled call spelling uses that
  -- submodule's name (`string_String_drop`), not the package's.
  -- Keyed by the MANGLED name alone — `String_drop`, not `string_String_drop`. The call
  -- site emits `<Type>_<method>` with no module component, which is the same spelling
  -- `importedMethodCallIds` above builds from `sym.effectiveName`; verified by giving the
  -- reproducer an explicit `import std.string.{String}`, at which point the diagnostic
  -- reads `function 'String_drop' requires Alloc`.
  --
  -- Recursion into `submoduleSummaries` is still needed: `summaryTable` is keyed at the
  -- PACKAGE level, so the `std` entry has no `implMethodSigs` of its own — `String`'s
  -- live in the `string` submodule summary.
  let rec collectMethodCaps : FileSummary → List (String × CapSet) :=
    fun s =>
      s.implMethodSigs.map (fun (mangled, sig) => (mangled, sig.capSet))
      ++ s.submoduleSummaries.flatMap (fun (_, sub) => collectMethodCaps sub)
  let preludeMethodCaps : List (String × CapSet) :=
    summaryTable.flatMap fun (_, s) => collectMethodCaps s
  -- `spec fn` declarations are resolvable targets in contracts. They were in none
  -- of the tables below, so every contract mentioning one (hmac_sha256's
  -- `result == ch_spec(x, y, z)`, for instance) resolved to nothing and made the
  -- whole subject UNCOVERED — a flagship's contracts silently outside the digest.
  let specCallIds : List (String × CallableId) :=
    m.specFns.map fun sf => (sf.name, CallableId.ofSpec thisProofPath sf.name)
  let resolveContractCall : String → Option CallableId := fun name =>
    -- AMBIGUITY FAILS CLOSED. A spec fn and a function of the same name are two
    -- different callables, and preferring one by list order would silently pick
    -- an abstraction over an implementation (or the reverse) inside evidence
    -- bytes. Unresolvable is the honest answer; it makes the subject uncovered
    -- rather than confidently wrong.
    match specCallIds.lookup name, localCallIds.lookup name with
    | some _, some _ => none
    | some id, none  => some id
    | none, _ =>
    match localCallIds.lookup name with
    | some id => some id
    | none => match importedCallIds.lookup name with
      | some id => some id
      -- AFTER imported functions, BEFORE intrinsics and builtins. A plain imported
      -- function must keep priority over a same-named impl method, and an impl method
      -- must not be shadowed by a builtin of the same mangled spelling.
      | none => match importedMethodCallIds.lookup name with
        | some id => some id
        -- INTRINSICS BEFORE BUILTINS. Many names (`string_length` among them) appear
        -- in both tables, and builtins-first classified compiler intrinsics as
        -- `.builtin` — a wrong NAMESPACE in the identity, which is the one field
        -- that exists to keep two same-named callables apart.
        | none => match resolveIntrinsic name with
          | some iid => some (CallableId.ofIntrinsic iid.canonicalName)
          | none => match builtinFnSigs.find? fun (n, _) => n == name with
            | some (_, sig) => some { CallableId.ofBuiltin name with typeParams := sig.typeParams.length }
            | none => if m.externFns.any fun ef => ef.name == name
                      then some (CallableId.ofExtern name) else none
  -- Constant environment for contracts: identity AND meaning, built once per
  -- module. Only the module's OWN constants: `ConstSummary` carries no initializer
  -- and no defining module, so an imported constant has neither half available and
  -- stays UNCOVERED — refusing rather than encoding a local guess about a foreign
  -- definition. Declaration order lets a constant's initializer name an earlier
  -- constant; a forward reference is not resolved and so fails closed.
  -- Accumulated by PREPEND and reversed once. Appending a singleton to the
  -- accumulator inside a fold over the module's constants is quadratic, which the
  -- quadratic-append ratchet correctly rejected. Lookup here is by name, so prepend
  -- order does not affect resolution; the single reverse restores declaration order
  -- for anything that iterates. (Phrased without the literal operator: that ratchet
  -- counts textual occurrences, so a comment naming the pattern trips it.)
  -- CALLABLE RESOLUTION IS BUILT BEFORE ElabEnv, deliberately.
  -- The CallableId must be an OUTPUT of the resolution that also selects runtime
  -- behaviour, not a second lookup performed later: two lookups can be handed
  -- different spellings or observe different table state and diverge while both
  -- "use the shared helper". Installing the resolver into ElabEnv after construction
  -- would also leave a transient window where callee identity silently resolves to
  -- none, so every path running in it would emit gaps for a reason that is not real.
  let initEnv : ElabEnv := {
    vars := []
    proofPath := thisProofPath
    resolveCallee := resolveContractCall
    structs := allStructs
    enums := allEnums
    fnSigs := allSigs
    typeAliases := typeAliasMap
    constants := constantsMap
    traits := allTraits
    allFnSigPairs := fnSigPairs
    newtypes := m.newtypes ++ imports.newtypes
  }
  -- Elaborate only LOCAL functions (imported impl bodies are already elaborated in their module)
  let (fnResults, fnErrors, _) := allFnPairs.foldl (fun (acc, errs, env) (f, implTy) =>
    let env' := { env with currentImplType := implTy, traits := allTraits }
    let result := (do
      let (cfn, bodyIdentityInputs, evidenceBody) ← elabFn f implTy
      let finalName := match implTy with
        | some it =>
          let tn := tyName it
          if tn != "" then tn ++ "_" ++ f.name else f.name
        | none => f.name
      let implOrigin := if cfn.isTrusted then
        match implTy with
        | some it =>
          let tn := tyName it
          if tn != "" then some tn else none
        | none => none
      else none
      pure ({ cfn with name := finalName, trustedImplOrigin := implOrigin },
             bodyIdentityInputs, evidenceBody)
        : ElabM (CFnDef × Proof.ProofBodyIdentityInputsV2 × Proof.EvidenceBodyDraftV2)).run env' |>.run
    match result with
    | (.ok (cfn, bodyIdentityInputs, evidenceBody), finalEnv) =>
        (acc ++ [((f, implTy), cfn, bodyIdentityInputs, evidenceBody)], errs, finalEnv)
    | (.error ds, _) => (acc, errs ++ ds.addContext s!"while elaborating function '{f.name}'", env)
  ) (([] : List ((FnDef × Option Ty) × CFnDef × Proof.ProofBodyIdentityInputsV2 × Proof.EvidenceBodyDraftV2)),
     ([] : Diagnostics), initEnv)
  if !fnErrors.isEmpty then .error fnErrors
  else
  let fns := fnResults.map fun (_, cfn, _) => cfn
  -- Build Core structs (local definitions). Expand type aliases AND erase
  -- newtypes in field types so that layout, copy-checking, and lowering see the
  -- underlying type — a `Copy` struct field typed by an alias to a Copy type
  -- (`type Id = i32; struct Copy S { a: Id }`) must read as Copy, not opaque.
  let eraseTy := fun (t : Ty) =>
    eraseNewtypeTy m.newtypes (expandAliasDeep typeAliasMap (typeAliasMap.length + 64) t)
  let cStructs := m.structs.map fun sd =>
    { name := sd.name, typeParams := sd.typeParams,
      fields := sd.fields.map fun f => (f.name, eraseTy f.ty),
      isPublic := sd.isPublic, isCopy := sd.isCopy, isReprC := sd.isReprC,
      isPacked := sd.isPacked, reprAlign := sd.reprAlign,
      declSpan := some sd.span : CStructDef }
  -- Also convert imported structs so cross-module field offsets work in Lower/Layout
  let localStructNames := m.structs.map (·.name)
  let cImportedStructs := (imports.structs.filter fun sd =>
      !(localStructNames.contains sd.name)).map fun sd =>
    { name := sd.name, typeParams := sd.typeParams,
      fields := sd.fields.map fun f => (f.name, eraseTy f.ty),
      isPublic := sd.isPublic, isCopy := sd.isCopy, isReprC := sd.isReprC,
      isPacked := sd.isPacked, reprAlign := sd.reprAlign,
      declSpan := some sd.span : CStructDef }
  -- Build extern fns
  let cExterns := m.externFns.map fun ef =>
    (ef.name, ef.params.map fun p => (p.name, p.ty), ef.retTy, ef.isTrusted)
  let cExternCaps := m.externFns.map fun ef => (ef.name, ef.capSet.getD .empty)
  -- Build constants
  let cConstants := m.constants.map fun c =>
    -- Uses elabExprEv, the single producer. Its EVIDENCE is dropped here only because
    -- constant dependency binding does not exist yet: the constant's initializer digest
    -- is what a `constRef` must eventually resolve to, so this is the site that will
    -- feed it. Recorded rather than silent.
    let constResult := ((·.core) <$> elabExprEv c.value (some c.ty)).run initEnv |>.run
    match constResult with
    | ((.ok cExpr), _) => (c.name, c.ty, cExpr)
    | ((.error _), _) => (c.name, c.ty, CExpr.intLit 0 c.ty)
  -- Elaborate submodules recursively
  -- Collect sibling submodule type definitions so each submodule can reference sibling types.
  -- Only inject struct/enum definitions and method SIGNATURES (not full impl blocks with bodies).
  let siblingStructs := summary.submoduleSummaries.foldl (fun acc (_, subSummary) =>
    acc ++ subSummary.structs) ([] : List StructDef)
  let siblingEnums := summary.submoduleSummaries.foldl (fun acc (_, subSummary) =>
    acc ++ subSummary.enums) ([] : List EnumDef)
  let siblingImplMethodSigs := summary.submoduleSummaries.foldl (fun acc (_, subSummary) =>
    acc ++ (subSummary.implMethodSigs.filter fun (name, _) =>
      subSummary.publicNames.contains name)) ([] : List (String × FnSummary))
  let cSubmodules := m.submodules.foldl (init := (Except.ok [] : Except Diagnostics (List CModule))) fun acc sub =>
    match acc with
    | .error e => .error e
    | .ok lst =>
      let subSummary := match summary.submoduleSummaries.find? fun (n, _) => n == sub.name with
        | some (_, s) => s
        | none => buildFileSummary sub
      let subImports := match resolveImports sub.imports summaryTable
          (fun modName => ElabError.message (.unknownModule modName))
          (fun sym modName => ElabError.message (.notPublicInModule sym modName))
          (pass := "elab") with
        | .ok imp => imp
        | .error _ => {}
      -- Inject sibling module types so submodules can reference each other's types
      -- Filter out siblings that conflict with locally-defined names
      let localStructNames := sub.structs.map (·.name)
      let localEnumNames := sub.enums.map (·.name)
      let filteredStructs := siblingStructs.filter fun sd =>
        !(localStructNames.contains sd.name)
      let filteredEnums := siblingEnums.filter fun ed =>
        !(localEnumNames.contains ed.name)
      let subImports := { subImports with
        structs := subImports.structs ++ filteredStructs
        enums := subImports.enums ++ filteredEnums
        implMethodSigs := subImports.implMethodSigs ++ siblingImplMethodSigs }
      let childPath := thisProofPath ++ "." ++ sub.name
      let childPrefix := if proofFnPrefix.isEmpty then sub.name
                         else proofFnPrefix ++ "_" ++ sub.name
      match elabModule sub subSummary subImports summaryTable
          (prefixSubs := false) (proofModulePath := childPath)
          (proofFnPrefix := childPrefix) with
      | .ok csub => .ok (lst ++ [{ csub with sourceFile := sub.sourceFile }])
      | .error ds => .error (Diagnostics.stampFile (ds.map fun d => { d with message := s!"in submodule '{sub.name}': {d.message}" }) sub.sourceFile)
  match cSubmodules with
  | .error e => .error e
  | .ok rawSubs =>
  -- Apply prefixing to each submodule (only at the outermost elabModule call).
  -- prefixModuleFnNames recursively handles nested submodules in one pass.
  let subs := if prefixSubs then
    rawSubs.zip (m.submodules.map (·.name)) |>.map fun (csub, subName) =>
      prefixModuleFnNames subName csub
  else rawSubs
  -- Cross-module rename: rewrite call sites so the monomorphizer sees prefixed names.
  -- Build global rename map: bare submodule fn name → prefixed name.
  let allSubFnPairs : List (String × String) := if !prefixSubs then [] else
    summary.submoduleSummaries.foldl (fun acc (subName, subSummary) =>
      acc
      ++ (subSummary.functions.map fun (fnName, _) => (fnName, subName ++ "_" ++ fnName))
      ++ (subSummary.implMethodSigs.map fun (msName, _) => (msName, subName ++ "_" ++ msName))
    ) []
  -- Count bare-name occurrences to detect ambiguity (same name in multiple submodules).
  let bareCounts : List (String × Nat) := allSubFnPairs.foldl (fun acc (bare, _) =>
    match acc.find? fun (n, _) => n == bare with
    | some _ => acc.map fun (n, c) => if n == bare then (n, c + 1) else (n, c)
    | none => acc ++ [(bare, 1)]
  ) []
  -- Keep only unambiguous mappings; exclude names the parent module defines itself.
  let parentFnNames := fns.map fun f => f.name
  let crossModuleRenames := allSubFnPairs.filter fun (bare, _) =>
    (match bareCounts.find? fun (n, _) => n == bare with
     | some (_, c) => c == 1
     | none => true)
    && !(parentFnNames.contains bare)
  -- Rewrite cross-module call sites in the parent's function bodies.
  let fns := fns.map fun f =>
    { f with body := renameFnStmts crossModuleRenames f.body }
  -- Rewrite cross-module call sites in submodule function bodies (sibling references).
  let subs := subs.map fun csub =>
    { csub with functions := csub.functions.map fun f =>
        { f with body := renameFnStmts crossModuleRenames f.body } }
  -- SUBJECT FACTS, captured HERE because this is the last point at which they all
  -- exist: `requires`/`ensures`, type bounds and capability parameters are on the
  -- AST `FnDef` and are gone from `CFnDef` below. Computing them downstream would
  -- mean either widening Core with data codegen never reads, or defining a
  -- semantic fact inside the layer that only renders semantic facts.
  --
  -- Keyed by `CallableId`, not by name: two callables can share a name, and a
  -- rename must not move a subject.
  -- Resolve contract calls at the same point. A textual call name is never put
  -- directly into evidence bytes: unresolved means uncovered, not guessed.
  let constEnv : List (String × ConstId × String) :=
    (m.constants.foldl (init := []) fun acc c =>
      match Proof.contractCanonicalIn [] [] [] [] acc false (fun _ => none) c.value with
      | some enc => (c.name, { defModule := m.name, declName := c.name }, enc) :: acc
      | none     => acc).reverse
  let declFacts : List Proof.CheckedDeclFacts :=
    fnResults.map fun ((f, implTy), _cfn, bodyIdentityInputs, _evidenceBody) =>
      let (concreteCaps, capVars) := f.capSet.normalize
      let capParamNames := f.capParams.eraseDups
      let capVarCanonical := capVars.map fun v =>
        match capParamNames.findIdx? fun n => n == v with
        | some i => s!"var:{i}"
        | none => "free:" ++ v
      let bounds := f.typeParams.map fun tp =>
        let bs := (f.typeBounds.find? fun (n, _) => n == tp).map Prod.snd |>.getD []
        ("", bs.eraseDups.mergeSort (· ≤ ·))
      { id := CallableId.ofUser thisProofPath (finalDeclName f implTy) f.typeParams.length
        params := f.params.map fun p =>
          (p.name, Proof.boundTyCanonical f.typeParams capParamNames p.ty)
        retTy := Proof.boundTyCanonical f.typeParams capParamNames f.retTy
        typeParams := f.typeParams
        typeBounds := bounds
        -- Both capability lists normalized, so `with(File, Net)` and
        -- `with(Net) ∪ with(File)` cannot yield two facts for one declaration.
        capParams := capParamNames
        capSet := (concreteCaps.map ("cap:" ++ ·)) ++ capVarCanonical
        contracts := Proof.ContractFacts.ofResolved (f.params.map (·.name))
          f.typeParams capParamNames (Proof.ghostBindersOf f.body) constEnv
          resolveContractCall f.requires f.ensures
          f.loopContracts
        bodyIdentityInputs := bodyIdentityInputs
        isTrusted := f.isTrusted
        overflowChecked := f.overflowChecked }
  .ok {
    name := m.name
    declFacts := declFacts
    -- Reuses constEnv, which already holds (name, ConstId, canonical initializer
    -- encoding). Dropping the name: identity is the key, and a spelling must not be.
    constBindings := constEnv.map fun (_, cid, enc) => (cid, enc)
    evidenceBodies := fnResults.map fun ((f, implTy), _cfn, _bii, evidenceBody) =>
      (CallableId.ofUser thisProofPath (finalDeclName f implTy) f.typeParams.length,
       evidenceBody)
    structs := cStructs ++ cImportedStructs
    enums := allEnums.map fun ed =>
      { name := ed.name, typeParams := ed.typeParams,
        variants := ed.variants.map fun v =>
          (v.name, v.fields.map fun f => (f.name, eraseTy f.ty)),
        isPublic := ed.isPublic, isCopy := ed.isCopy, builtinId := ed.builtinId,
        declSpan := some ed.span : CEnumDef }
    functions := fns
    externFns := cExterns
    externFnCaps := cExternCaps
    constants := cConstants
    submodules := subs
    newtypes := m.newtypes ++ imports.newtypes
    traitDefs := m.traits.map fun td =>
      { name := td.name,
        methods := td.methods.map fun sig =>
          { name := sig.name, retTy := sig.retTy },
        builtinId := td.builtinId, declSpan := some td.span : CTraitDef }
    traitImpls := m.traitImpls.map fun tb =>
      let traitBuiltinId := match allTraits.find? fun td => td.name == tb.traitName with
        | some td => td.builtinId
        | none => none
      { traitName := tb.traitName,
        typeName := tb.typeName,
        methodNames := tb.methods.map (·.name),
        methodRetTys := tb.methods.map fun f => (f.name, f.retTy),
        builtinTraitId := traitBuiltinId, declSpan := some tb.span : CTraitImpl }
    importedFnCaps := importedFnCaps ++ preludeMethodCaps
    linkerAliases :=
      -- Import aliases: imported bare name → prefixed definition (subName_fnName)
      -- When user writes `import math.{add}` and calls `add(...)`, the call emits `@add`
      -- but the definition is `@math_add`. This alias bridges the gap.
      m.imports.foldl (fun acc imp =>
        match summary.submoduleSummaries.find? fun (n, _) => n == imp.moduleName with
        | some (subName, subSummary) =>
          acc ++ imp.symbols.foldl (fun acc sym =>
            let origName := sym.name
            let localName := match sym.alias with | some a => a | none => origName
            -- Only alias regular functions (not externs — those keep bare C names)
            if subSummary.functions.any fun (n, _) => n == origName then
              acc ++ [(localName, subName ++ "_" ++ origName)]
            else if subSummary.implMethodSigs.any fun (n, _) => n == origName then
              acc ++ [(localName, subName ++ "_" ++ origName)]
            else acc
          ) []
        | none => acc
      ) []
      ++ imports.linkerAliases
      -- Impl method aliases: TypeName_method → subName_TypeName_method
      -- Method dispatch produces `Bytes_drop` but definition is `bytes_Bytes_drop`.
      ++ summary.submoduleSummaries.foldl (fun acc (subName, subSummary) =>
        acc
        ++ (subSummary.implMethodSigs.map fun (msName, _) => (msName, subName ++ "_" ++ msName))
      ) []
      -- Extern fn aliases: qualified call (subName_efName) → bare C symbol (efName)
      -- Extern functions are NOT prefixed (they reference real C symbols).
      ++ summary.submoduleSummaries.foldl (fun acc (subName, subSummary) =>
        acc
        ++ (subSummary.externFnSigs.map fun (efName, _) => (subName ++ "_" ++ efName, efName))
      ) []
      -- Nested submodule import aliases: when importing from a nested module path
      -- (e.g., `import std.fs.{read_file}` or `import mymod.sub.{test}`), the
      -- function definition is prefixed with the submodule path. Generate aliases
      -- so calls using the imported bare name resolve to the prefixed definition.
      -- For local nested imports (first component is a local submodule), use the
      -- full path as prefix. For cross-package imports, drop the package name.
      ++ m.imports.foldl (fun acc imp =>
        let parts := imp.moduleName.splitOn "."
        if parts.length < 2 then acc
        else
          -- Already handled by local submodule aliases above?
          match summary.submoduleSummaries.find? fun (n, _) => n == imp.moduleName with
          | some _ => acc  -- already handled above
          | none =>
            -- Determine prefix: if first component is a local submodule, use full path;
            -- otherwise (cross-package), drop the first component (package name).
            let isLocalNested := summary.submoduleSummaries.any fun (n, _) =>
              n == (parts.head?.getD "")
            let subPath := if isLocalNested then parts else parts.drop 1
            let subPrefix := "_".intercalate subPath ++ "_"
            -- Look up the module summary to check which symbols are functions
            match summaryTable.find? fun (n, _) => n == imp.moduleName with
            | some (_, modSummary) =>
              acc ++ imp.symbols.foldl (fun acc sym =>
                let origName := sym.name
                let localName := match sym.alias with | some a => a | none => origName
                if modSummary.functions.any fun (n, _) => n == origName then
                  acc ++ [(localName, subPrefix ++ origName)]
                else if modSummary.implMethodSigs.any fun (n, _) => n == origName then
                  acc ++ [(localName, subPrefix ++ origName)]
                else acc
              ) []
            | none => acc
      ) []
  }

-- ============================================================
-- Program elaboration
-- ============================================================

def elabProgram (resolved : List ResolvedModule)
    (summaryTable : List (String × FileSummary) := []) : Except Diagnostics (List CModule) :=
  -- Build sibling module summaries for inline modules (mod A {} mod B {}).
  let moduleSummaryList : List (String × FileSummary) := resolved.map fun rm =>
    let m := rm.module
    (m.name, match summaryTable.find? fun (n, _) => n == m.name with
      | some (_, s) => s
      | none => buildFileSummary m)
  let (cms, allErrors) := resolved.foldl (fun (acc, errs) rm =>
    let m := rm.module
    let summary := match moduleSummaryList.find? fun (n, _) => n == m.name with
      | some (_, s) => s
      | none => buildFileSummary m
    match resolveImports m.imports summaryTable
        (fun modName => ElabError.message (.unknownModule modName))
        (fun sym modName => ElabError.message (.notPublicInModule sym modName))
        (pass := "elab") with
    | .error ds => (acc, errs ++ ds)
    | .ok imports =>
      -- Inject sibling module functions for qualified :: access
      let siblingFns : List (String × FnSummary) := moduleSummaryList.foldl (fun acc (sibName, sibSummary) =>
        if sibName == m.name || sibName == "main" then acc
        else acc ++ (sibSummary.functions.filter fun (name, _) =>
          sibSummary.publicNames.contains name).map fun (name, fs) =>
            (sibName ++ "_" ++ name, fs)
      ) []
      -- Linker aliases: math_add → add (qualified call name → bare definition name)
      let siblingAliases : List (String × String) := moduleSummaryList.foldl (fun acc (sibName, sibSummary) =>
        if sibName == m.name || sibName == "main" then acc
        else acc ++ (sibSummary.functions.filter fun (name, _) =>
          sibSummary.publicNames.contains name).map fun (name, _) =>
            (sibName ++ "_" ++ name, name)
      ) []
      let imports := { imports with
        functions := imports.functions ++ siblingFns
        linkerAliases := imports.linkerAliases ++ siblingAliases }
      match elabModule m summary imports summaryTable with
      | .ok cm => (acc ++ [cm], errs)
      | .error ds => (acc, errs ++ ds.addContext s!"while elaborating module '{m.name}'")
  ) (([] : List CModule), ([] : Diagnostics))
  if allErrors.isEmpty then .ok cms else .error allErrors

end Concrete
