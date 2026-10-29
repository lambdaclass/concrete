import Std.Data.HashMap
import Std.Data.TreeMap
import Concrete.Frontend.AST
import Concrete.Report.Diagnostic
import Concrete.Frontend.Token
import Concrete.Resolve.FileSummary
import Concrete.Resolve.BuiltinEnums
import Concrete.Resolve.Intrinsic
import Concrete.Backend.Symbols

namespace Concrete

/-! ## Resolve — shallow/interface name resolution pass

Runs after Parse, before Check. Strictly a shallow/interface validation pass.
Consumes `FileSummary` artifacts for declaration-level information. Does not
inspect function bodies for interface data and does not perform post-elaboration
semantic checks.

Owns: module existence, import validity, top-level name visibility, deep type
name validation, `Self` scoping, function/static-method/enum-variant resolution.

Does NOT own: trait impl completeness (CoreCheck), trait signature matching
(CoreCheck), FFI/repr validation (CoreCheck), type correctness (Check),
instance method resolution (Check — needs receiver type).

Approach: side-table symbol resolution — AST types are not modified.
-/

-- ============================================================
-- Symbol tables
-- ============================================================

inductive SymKind where
  | fn (params : List Param) (retTy : Ty)
  | struct (def_ : StructDef)
  | enum (def_ : EnumDef)
  | trait (def_ : TraitDef)
  | var (ty : Option Ty) (mutable : Bool)
  | typeAlias (target : Ty)
  | externFn (params : List Param) (retTy : Ty)
  | const (ty : Ty)
  /-- `methodOf`: the declaration and method this is (`FnSummary.methodOf`, R-0492 step 3). -/
  | implMethod (typeName : String) (params : List Param) (retTy : Ty) (methodOf : Option MethodKey)
  | newtype (def_ : NewtypeDef)

inductive ResolveError where
  | undeclaredVariable (name : String)
  | unknownFunction (name : String)
  | unknownStructType (name : String)
  | unknownEnumVariant (variant : String) (enumName : String)
  | notAnEnum (name : String)
  | unknownEnum (name : String)
  | unknownStaticMethod (typeName : String) (method : String)
  | unknownFunctionRef (name : String)
  | unknownType (name : String)
  | selfOutsideImpl
  | unknownModule (name : String)
  | notPublicInModule (symbol : String) (moduleName : String)
  -- Bug 226 (MOD-6, OD20): a module outside std imports a binding of std.libc.
  | stdPrivateImport (symbol : String) (moduleName : String)
  | recursiveTypeAlias (name : String)
  | wrongTypeArgCount (name : String) (expected : Nat) (got : Nat)
  | capArgKindMismatch (name : String) (detail : String)
  | wrongCapArgCount (name : String) (expected : Nat) (got : Nat)
  | externEffectsUndeclared (name : String)
  | effectfulSymbolDeclaredPure (name : String)
  | invalidLinkName (name : String) (symbol : String) (reason : String)
  | invalidExport (name : String) (symbol : String) (reason : String)
  | importAliasIsIntrinsic (name : String) (alias : String)
  | capParamNamesCapability (owner : String) (param : String)
  | unknownBoundTrait (traitName : String) (param : String) (owner : String)
  | unimportable (kind : String) (symbol : String) (moduleName : String)
  | traitIsNotAType (name : String)

def ResolveError.message : ResolveError → String
  | .undeclaredVariable name => s!"undeclared variable '{name}'"
  | .unknownFunction name => s!"unknown function '{name}'"
  | .unknownStructType name => s!"unknown struct type '{name}'"
  | .unknownEnumVariant variant enumName => s!"unknown variant '{variant}' in enum '{enumName}'"
  | .notAnEnum name => s!"'{name}' is not an enum"
  | .unknownEnum name => s!"unknown enum '{name}'"
  | .unknownStaticMethod typeName method => s!"unknown static method '{typeName}::{method}'"
  | .unknownFunctionRef name => s!"unknown function reference '{name}'"
  | .unknownType name => s!"unknown type '{name}'"
  | .traitIsNotAType name =>
    s!"'{name}' is a trait, not a type: there are no trait-object types (TYPE-2), so a trait names no value; bound a type parameter by it instead (`fn f<T: {name}>(x: T)`)"
  | .selfOutsideImpl => "Self can only be used inside impl blocks"
  | .unknownModule name => s!"unknown module '{name}'"
  | .notPublicInModule symbol moduleName => s!"'{symbol}' is not public in module '{moduleName}'"
  | .stdPrivateImport symbol moduleName => stdPrivateImportMsg symbol moduleName
  | .recursiveTypeAlias name => s!"recursive type alias '{name}' (a type alias may not refer to itself, directly or through other aliases)"
  | .capArgKindMismatch name detail => s!"wrong kind of argument for '{name}': {detail}"
  | .externEffectsUndeclared name =>
    s!"foreign binding '{name}' does not declare its effects: write `with(...)` naming the capabilities it exercises, or `with()` if it has none — an undeclared binding is never read as having no effects"
  | .effectfulSymbolDeclaredPure name =>
    s!"foreign binding '{name}' is declared with no effects, but '{name}' is a known effectful C symbol; declare the capability it exercises"
  | .invalidLinkName name symbol reason =>
    s!"foreign binding '{name}' cannot bind the C symbol '{symbol}': {reason}"
  | .capParamNamesCapability owner param =>
    s!"capability parameter '{param}' of '{owner}' is spelled like a capability: a capability variable entails only itself, so a parameter named '{param}' would stand for the real capability '{param}' without being granted it; rename the parameter"
  | .unimportable kind symbol moduleName => unimportableMsg kind symbol moduleName
  | .unknownBoundTrait traitName param owner =>
    s!"unknown trait '{traitName}' in the bound on '{param}' of '{owner}': a bound names `Copy`, `Destroy` or a trait declared in this module (traits are module-local and cannot be imported)"
  | .importAliasIsIntrinsic name alias =>
    s!"'{name}' may not be imported as '{alias}': '{alias}' is the name of a compiler intrinsic, so a call spelled '{alias}' is compiled as the intrinsic and never reaches '{name}'"
  | .invalidExport name symbol reason =>
    s!"#[export_name = \"{symbol}\"] on '{name}' cannot be exported: {reason}"
  | .wrongCapArgCount name expected got =>
    s!"'{name}' takes {expected} capability argument{if expected == 1 then "" else "s"}, but {got} {if got == 1 then "was" else "were"} given"
  | .wrongTypeArgCount name expected got =>
    s!"'{name}' takes {expected} type argument{if expected == 1 then "" else "s"}, but {got} {if got == 1 then "was" else "were"} given"

structure Scope where
  symbols : List (String × SymKind)
  /-- name → kind, built once from `symbols` (keeps the FIRST occurrence, matching
      the old `symbols.find?`/`.any` semantics). Global-symbol lookup was an
      assoc-list scan PER identifier reference — O(symbols) each, i.e. O(N²) for a
      module of N mutually-referencing functions (`--emit-core` alone was O(N²)).
      Built at scope-assembly time (bounded), read O(1) per reference. -/
  symbolMap : Std.HashMap String SymKind := ∅
  /-- name → every TYPE declaration of that name (struct, enum, trait, alias, newtype), in scope
      order. Types and values are separate namespaces (MOD-3; bug 218's
      `regress_218_type_and_value_namespaces`), so a type lookup must not meet a function or
      constant of the same spelling, which `symbolMap` returns when it was registered first; and
      Check and Elab consult aliases, structs and enums in separate tables, so which declaration
      answers depends on the position, not on scope order alone (bug 229). -/
  typeKinds : Std.HashMap String (Array SymKind) := ∅

/-- A declaration in the type namespace. -/
def SymKind.isType : SymKind → Bool
  | .struct _ | .enum _ | .trait _ | .typeAlias _ | .newtype _ => true
  | _ => false

/-- Build a `Scope` from a symbol list, materializing the O(1) lookup maps (first
    occurrence wins, matching `find?`). Use instead of `{ symbols := … }` so every
    scope carries a consistent map. -/
def Scope.ofSymbols (symbols : List (String × SymKind)) : Scope :=
  { symbols := symbols,
    symbolMap := symbols.foldl (fun m (n, k) => if m.contains n then m else m.insert n k) ∅,
    typeKinds := symbols.foldl (fun m (n, k) =>
      if !k.isType then m else m.insert n ((m.getD n #[]).push k)) ∅ }

/-- A scope whose type namespace is `typeSymbols` alone, while values are looked up in `symbols`
    (R-0492 slice 6). A top-level module's type namespace is its own environment
    (`moduleTypeEnvironment` and its imports); its value namespace still falls back to the
    program-wide scope, which holds the other top-level modules' functions that Check also injects. -/
def Scope.ofSymbolsTyped (symbols typeSymbols : List (String × SymKind)) : Scope :=
  { (Scope.ofSymbols symbols) with
    typeKinds := typeSymbols.foldl (fun m (n, k) =>
      if !k.isType then m else m.insert n ((m.getD n #[]).push k)) ∅ }

structure ResolvedModule where
  module : Module
  globalScope : Scope

-- ============================================================
-- Resolution state
-- ============================================================

structure ResolveCtx where
  globalScope : Scope
  /-- Stack of local scopes, innermost first. Each scope is a map, not a list searched per name:
      a lookup walked every binding of the enclosing scopes, so a function of n bindings read
      far from their use was O(n^2) to resolve (bug 257). -/
  localScopes : List (Std.TreeMap String SymKind compare)
  /-- For each name bound in some scope of `localScopes`, how many of those scopes bind it, so a
      lookup asks one map instead of walking the stack: a body nested n blocks deep reading a
      binding of its outermost block walked n scopes per use, n^2 for the nest (bug 282). Kept
      by `addLocal` and `popScope`, the only writers of `localScopes` below the function level. -/
  visibleLocals : Std.TreeMap String Nat compare := {}
  errors : Diagnostics
  /-- Known type names (structs, enums, type aliases, traits), a set built once per module: a
      list was searched per written type, n² for n types each named once (bug 291). -/
  knownTypes : Std.HashSet String
  /-- The functions and its impls type parameters, known types inside its body (bug 291: the
      list `knownTypes` was extended with them per function). -/
  extraKnownTypes : List String := []
  /-- Current impl type name, for Self resolution. -/
  currentImplType : Option String := none
  /-- Trait name → list of method names. -/
  traitMethods : List (String × List String) := []
  /-- (typeName, traitName) pairs from trait impl blocks. -/
  traitImpls : List (String × String) := []
  /-- True when resolving inside a trait definition (Self is allowed). -/
  inTraitDef : Bool := false
  /-- The type parameters in scope (the function's and its impl's). A type parameter shadows a
      declaration or builtin of the same name, so no arity is looked up for it (bug 200). -/
  typeParams : List String := []

/-- Result of the shallow (declaration-level) resolution phase.
    Carries everything the body phase needs plus any errors found during the shallow phase. -/
structure ShallowResult where
  globalScope : Scope
  knownTypes : List String
  traitMethods : List (String × List String)
  traitImpls : List (String × String)
  errors : Diagnostics
  /-- Bug 096: each top-level module's OWN declarations and imports, searched
      before `globalScope`. The global scope is every module's declarations in
      one first-wins map, so `Shape::Line` in a module whose `Shape` has a `Line`
      variant was refused (E0103) when another module's `Shape`, registered
      first, had none. -/
  moduleScopes : List (String × List (String × SymKind)) := []
  /-- R-0492 slice 6: each top-level module's type environment, the type declarations and the
      type names it may write: its imports' and its own (`moduleTypeEnvironment`), nothing of
      another module. Check gives a top-level module no other module's types (only their
      functions), so neither does Resolve (bugs 227, 230). -/
  moduleTypeEnvs : List (String × List (String × SymKind) × List String) := []
  /-- The program's summary table, against which a nested submodule's imports are resolved in
      the body phase (bug 217). -/
  summaryTable : List (String × FileSummary) := []

def ResolveError.code : ResolveError → String
  | .undeclaredVariable _ => "E0100"
  | .unknownFunction _ => "E0101"
  | .unknownStructType _ => "E0102"
  | .unknownEnumVariant _ _ => "E0103"
  | .notAnEnum _ => "E0104"
  | .unknownEnum _ => "E0105"
  | .unknownStaticMethod _ _ => "E0106"
  | .unknownFunctionRef _ => "E0107"
  | .unknownType _ => "E0108"
  | .selfOutsideImpl => "E0109"
  | .unknownModule _ => "E0110"
  | .notPublicInModule _ _ => "E0111"
  | .stdPrivateImport _ _ => "E0326"
  | .recursiveTypeAlias _ => "E0112"
  | .wrongTypeArgCount _ _ _ => "E0113"
  | .capArgKindMismatch _ _ => "E0114"
  | .wrongCapArgCount _ _ _ => "E0115"
  | .externEffectsUndeclared _ => "E0116"
  | .effectfulSymbolDeclaredPure _ => "E0117"
  | .invalidLinkName _ _ _ => "E0118"
  | .invalidExport _ _ _ => "E0119"
  | .importAliasIsIntrinsic _ _ => "E0120"
  | .capParamNamesCapability _ _ => "E0121"
  | .unknownBoundTrait _ _ _ => "E0125"
  | .unimportable _ _ _ => "E0126"
  | .traitIsNotAType _ => "E0108"

private def addError (ctx : ResolveCtx) (err : ResolveError) (span : Option Span := none) : ResolveCtx :=
  { ctx with errors := ctx.errors ++ [{ severity := .error, message := err.message, pass := "resolve", span := span, hint := none, code := err.code }] }

private def mkResolveDiag (err : ResolveError) (span : Option Span := none) : Diagnostic :=
  { severity := .error, message := err.message, pass := "resolve", span := span, hint := none, code := err.code }

private def pushScope (ctx : ResolveCtx) : ResolveCtx :=
  { ctx with localScopes := {} :: ctx.localScopes }

private def popScope (ctx : ResolveCtx) : ResolveCtx :=
  match ctx.localScopes with
  | scope :: rest =>
    -- Each name the closing scope bound is visible in one scope fewer (bug 282).
    let visible := scope.foldl (fun (m : Std.TreeMap String Nat compare) n _ =>
      match m.get? n with
      | some k => if k ≤ 1 then m.erase n else m.insert n (k - 1)
      | none => m) ctx.visibleLocals
    { ctx with localScopes := rest, visibleLocals := visible }
  | [] => ctx

private def addLocal (ctx : ResolveCtx) (name : String) (kind : SymKind) : ResolveCtx :=
  match ctx.localScopes with
  | scope :: rest =>
    let visible := if scope.contains name then ctx.visibleLocals
      else ctx.visibleLocals.insert name (ctx.visibleLocals.getD name 0 + 1)
    { ctx with localScopes := scope.insert name kind :: rest, visibleLocals := visible }
  | [] => ctx  -- should not happen

private def lookupName (ctx : ResolveCtx) (name : String) : Bool :=
  -- A local of any enclosing scope (one map, bug 282), then the global scope (O(1) via symbolMap)
  ctx.visibleLocals.contains name ||
  ctx.globalScope.symbolMap.contains name

/-- The builtin enum a name denotes in an enum table, as `BuiltinEnums.builtinEnums` orders it
    for Check and Elab: `Option` comes before every user enum and is never withheld; `Result` is
    the builtin unless a user enum of that name is in scope (`hasUserResultEnum`). -/
private def builtinEnumFor (name : String) (userEnumInScope : Bool) : Option EnumDef :=
  if name == optionEnumName then some builtinOptionEnum
  else if name == resultEnumName && !userEnumInScope then some builtinResultEnum
  else none

/-- The declaration a WRITTEN type name denotes (an annotation, a signature, a type argument), in
    Check's order (bug 229): an alias is expanded first (`closeAliasMap` runs before any struct or
    enum lookup), then the struct, enum or newtype; among enums the builtin `Option` comes first.
    Only the type namespace is searched (local scopes hold values only). -/
private def lookupWrittenType (ctx : ResolveCtx) (name : String) : Option SymKind :=
  let ks := ctx.globalScope.typeKinds.getD name #[]
  match ks.find? (fun k => match k with | .typeAlias _ => true | _ => false) with
  | some k => some k
  | none =>
    let userEnum := ks.any fun k => match k with | .enum _ => true | _ => false
    let first : Option SymKind := ks[0]?
    match first, builtinEnumFor name userEnum with
    | some (SymKind.enum _), some b => some (SymKind.enum b)
    | k, _ => k

/-- The enum an enum literal or pattern head `E::V` names, from the enum table as Check and Elab
    build it (builtins first, then the scope's enums), whatever else of that name is declared: a
    `type Option = i32` does not hide the builtin's `Option::Some` (bug 229, Codex review r5).
    `some` of a non-enum when the name is declared only as another type (E0104). -/
private def lookupEnumForLiteral (ctx : ResolveCtx) (name : String) : Option SymKind :=
  let ks := ctx.globalScope.typeKinds.getD name #[]
  let userEnum := ks.find? fun k => match k with | .enum _ => true | _ => false
  let first : Option SymKind := ks[0]?
  match builtinEnumFor name userEnum.isSome with
  | some b => some (SymKind.enum b)
  | none => userEnum <|> first

/-- R-0492 step 3: the type spelled `typeName` here has a method `method`, found by the type's
    declaration identity as Check and Elab select it (`methodByIdentity`): `Q::mk` after
    `import m.{P as Q}` names `P`'s `mk`, registered as `P_mk` (bug 223). -/
private def hasMethodByIdentity (ctx : ResolveCtx) (typeName method : String) : Bool :=
  let owner? : Option TypeId := if ctx.typeParams.contains typeName then none else
    match lookupWrittenType ctx typeName with
    | some (.struct sd) => sd.typeId?
    | some (.enum ed) => ed.typeId?
    | some (.newtype nt) => nt.typeId?
    | _ => none
  match owner? with
  | none => false
  | some owner =>
    let key : MethodKey := { owner, method }
    ctx.globalScope.symbols.any fun (_, k) => match k with
      | .implMethod _ _ _ (some k') => k' == key
      | _ => false

private def isKnownType (ctx : ResolveCtx) (name : String) : Bool :=
  ctx.knownTypes.contains name || ctx.extraKnownTypes.contains name

/-- Bug 240: whether a written type name denotes a trait. A trait is in the type namespace
    (`SymKind.isType`: a bound and an impl name it there) but is not a type: there are no
    trait-object types (TYPE-2), so a trait in a type position (a cast target, an annotation, a
    signature, a field, a type argument) denotes no value and no layout. Before this the name passed
    as a known type, and the backend met an unknown named type (`x as Marker`: an `EmitSSA` panic).
    A type parameter of that spelling shadows the trait. -/
private def denotesTrait (ctx : ResolveCtx) (name : String) : Bool :=
  !ctx.typeParams.contains name &&
  match lookupWrittenType ctx name with
  | some (SymKind.trait _) => true
  | _ => false

-- ============================================================
-- Builtin names
-- ============================================================

-- Builtin name tables are centralized in Intrinsic.lean.
-- Resolve uses isKnownBuiltinFnFn, builtinTypeNames, etc. from there.

-- ============================================================
-- Deep type validation
-- ============================================================

/-- How many type arguments a generic type name takes, when that is known.

    The declaration wins over the builtin table, because a program may define its own
    `Result` (`hasUserResultEnum`); an enum named `Option` is always the builtin
    (`lookupWrittenType`). `none` means "not known here" — an imported
    or otherwise unresolved name — and the arity check is skipped rather than guessed:
    rejecting a valid program is a different bug, not a safer one. -/
private def declaredCapArity (ctx : ResolveCtx) (name : String) : Option Nat :=
  if ctx.typeParams.contains name then none else
  match lookupWrittenType ctx name with
  | some (.struct d) => some d.capParams.length
  | some (.enum _) | some (.newtype _) => some 0
  -- A type declared under the name (an alias, a trait) is not the builtin. A value binding is
  -- in another namespace and does not hide it.
  | some (.typeAlias _) | some (.trait _) => none
  | _ => match name with
    | "Heap" | "HeapArray" | "Vec" | "Option" | "Result" => some 0
    | _ => none

private def declaredTypeArity (ctx : ResolveCtx) (name : String) : Option Nat :=
  if ctx.typeParams.contains name then none else
  match lookupWrittenType ctx name with
  | some (.struct d) => some d.typeParams.length
  | some (.enum d) => some d.typeParams.length
  | some (.newtype d) => some d.typeParams.length
  -- A type declared under the name (an alias, a trait) is not the builtin: `type Vec = ..` takes
  -- no arguments (bug 200 review). A value binding is in another namespace and does not hide it.
  | some (.typeAlias _) | some (.trait _) => none
  | _ => match name with
    | "Heap" | "HeapArray" | "Vec" | "Option" => some 1
    | "Result" => some 2
    | _ => none

/-- An explicit type-argument list must match the declaration's arity (bug 073). Before
    this, `Box1<i32, bool>` for a one-parameter struct compiled and ran with the extra
    argument silently dropped, and a missing argument surfaced only as an unrelated
    linearity error on the unresolved parameter. -/
private def checkTypeArgCount (ctx : ResolveCtx) (name : String) (got : Nat)
    (span : Option Span) : ResolveCtx :=
  match declaredTypeArity ctx name with
  | some expected => if expected == got then ctx
                     else addError ctx (.wrongTypeArgCount name expected got) span
  | none => ctx

/-- Type and capability arguments against the declaration. Normalization leaves an
    argument in the type list only when it could not be a capability, so a type list
    LONGER than the declaration's, on a struct that takes capabilities, is a type
    written where a capability is expected. -/
private def checkGenericArgs (ctx : ResolveCtx) (name : String) (args : List Ty)
    (caps : List CapSet) (span : Option Span) : ResolveCtx :=
  match declaredTypeArity ctx name, declaredCapArity ctx name with
  | some tp, some cp =>
    if args.length > tp && cp > 0 then
      addError ctx (.capArgKindMismatch name
        s!"a type was given where a capability is expected (it takes {tp} type argument{if tp == 1 then "" else "s"} and {cp} capability argument{if cp == 1 then "" else "s"})") span
    else if !caps.isEmpty && cp == 0 then
      addError ctx (.capArgKindMismatch name
        "a capability was given, but it takes no capability arguments") span
    else
      let ctx := if args.length == tp then ctx else addError ctx (.wrongTypeArgCount name tp args.length) span
      if caps.length == cp then ctx else addError ctx (.wrongCapArgCount name cp caps.length) span
  | some _, none => checkTypeArgCount ctx name args.length span
  | _, _ => ctx

/-- Recursively check that all type names in a Ty are known. -/
private def checkTyDeep (ctx : ResolveCtx) (ty : Ty) (span : Option Span := none) : ResolveCtx :=
  match ty with
  | .named name =>
    if name == selfTypeName then
      match ctx.currentImplType with
      | some _ => ctx
      | none => addError ctx .selfOutsideImpl span
    else if isKnownType ctx name && denotesTrait ctx name then
      addError ctx (.traitIsNotAType name) span
    else if isKnownType ctx name then
      -- Bug 200: a generic type named bare (`B` for `B<T>`) is E0113 (0 type arguments
      -- given), as an explicit list of the wrong length is. Before this the missing
      -- argument was accepted and left an unresolved parameter for later passes.
      let ctx := match declaredTypeArity ctx name with
        | some tp => if tp > 0 then addError ctx (.wrongTypeArgCount name tp 0) span else ctx
        | none => ctx
      -- A struct with capability parameters cannot be named bare: `Writer` alone says
      -- nothing about what the handle may do, so it is an E0115 (0 capability
      -- arguments given), not something to infer later.
      match declaredCapArity ctx name with
      | some cp => if cp > 0 then addError ctx (.wrongCapArgCount name cp 0) span else ctx
      | none => ctx
    else addError ctx (.unknownType name) span
  -- Capability arguments were already classified by position (Frontend/CapArgs), so
  -- what remains here is the kind and count check, in both directions (R-0484).
  | .generic name args caps =>
    let ctx := if isKnownType ctx name && denotesTrait ctx name then addError ctx (.traitIsNotAType name) span
               else if isKnownType ctx name then checkGenericArgs ctx name args caps span
               else addError ctx (.unknownType name) span
    args.foldl (fun ctx ty => checkTyDeep ctx ty span) ctx
  | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner
  | .heap inner | .heapArray inner => checkTyDeep ctx inner span
  | .array elem _ => checkTyDeep ctx elem span
  | .fn_ params _capSet retTy =>
    let ctx := params.foldl (fun ctx ty => checkTyDeep ctx ty span) ctx
    checkTyDeep ctx retTy span
  | _ => ctx

/-- Bug 215: a type written inside an expression (a cast target, a type-argument list) names
    known types like every other written type. Resolve walked only the expression parts, so
    `x as Nope` reached the backend (an `EmitSSA` panic) and `f::<[Nope; 3]>()` compiled. -/
private def checkWrittenTys (ctx : ResolveCtx) (tys : List Ty) (sp : Span) : ResolveCtx :=
  tys.foldl (fun ctx ty => checkTyDeep ctx ty (some sp)) ctx

-- ============================================================
-- Walk expressions and statements
-- ============================================================

mutual
/-- Walk an expression, checking all name references. -/
partial def resolveExpr (ctx : ResolveCtx) (e : Expr) : ResolveCtx :=
  match e with
  | .ident sp name =>
    -- Only flag identifiers that aren't in any scope and aren't known functions
    if lookupName ctx name || isKnownBuiltinFn name then ctx
    else addError ctx (.undeclaredVariable name) (some sp)
  | .intLit _ _ | .floatLit _ _ | .boolLit _ _ | .strLit _ _ | .charLit _ _ => ctx
  | .binOp _ _ lhs rhs => resolveExpr (resolveExpr ctx lhs) rhs
  | .unaryOp _ _ operand => resolveExpr ctx operand
  | .call sp fn typeArgs args =>
    let ctx := if lookupName ctx fn || isKnownBuiltinFn fn then ctx
               else addError ctx (.unknownFunction fn) (some sp)
    let ctx := checkWrittenTys ctx typeArgs sp
    args.foldl resolveExpr ctx
  | .paren _ inner => resolveExpr ctx inner
  | .structLit sp name typeArgs fields base =>
    let ctx := if isKnownType ctx name then
                 (if typeArgs.isEmpty then ctx else checkTypeArgCount ctx name typeArgs.length (some sp))
               else addError ctx (.unknownStructType name) (some sp)
    let ctx := checkWrittenTys ctx typeArgs sp
    let ctx := fields.foldl (fun ctx (_, e) => resolveExpr ctx e) ctx
    match base with | some b => resolveExpr ctx b | none => ctx
  | .fieldAccess _ obj _ => resolveExpr ctx obj
  | .enumLit sp enumName variant typeArgs fields =>
    let ctx := checkWrittenTys ctx typeArgs sp
    let ctx := match lookupEnumForLiteral ctx enumName with
      | some (.enum def_) =>
        if def_.variants.any (fun v => v.name == variant) then ctx
        else addError ctx (.unknownEnumVariant variant enumName) (some sp)
      | some _ => addError ctx (.notAnEnum enumName) (some sp)
      | none => if isKnownType ctx enumName then ctx
                else addError ctx (.unknownEnum enumName) (some sp)
    fields.foldl (fun ctx (_, e) => resolveExpr ctx e) ctx
  | .match_ _ scrutinee arms =>
    let ctx := resolveExpr ctx scrutinee
    let resolveGuard := fun ctx (g : Option Expr) => match g with | some e => resolveExpr ctx e | none => ctx
    arms.foldl (fun ctx arm =>
      match arm with
      | .mk _ _ _ bindings guard body =>
        let ctx := pushScope ctx
        -- `_` is a wildcard field binding: positional, but not added to scope.
        let ctx := (PatBind.names bindings).foldl (fun ctx b => if isWildcardBinder b then ctx else addLocal ctx b (.var none false)) ctx
        let ctx := resolveGuard ctx guard
        let ctx := resolveStmts ctx body
        popScope ctx
      | .litArm _ val guard body =>
        let ctx := resolveExpr ctx val
        let ctx := resolveGuard ctx guard
        resolveStmts ctx body
      | .varArm _ binding guard body =>
        let ctx := pushScope ctx
        let ctx := addLocal ctx binding (.var none false)
        let ctx := resolveGuard ctx guard
        let ctx := resolveStmts ctx body
        popScope ctx
      | .rangeArm _ lo hi _ guard body =>
        let ctx := resolveExpr ctx lo
        let ctx := resolveExpr ctx hi
        let ctx := resolveGuard ctx guard
        resolveStmts ctx body
    ) ctx
  | .borrow _ inner | .borrowMut _ inner | .deref _ inner =>
    resolveExpr ctx inner
  | .arrayLit _ elems => elems.foldl resolveExpr ctx
  | .arrayIndex _ arr idx => resolveExpr (resolveExpr ctx arr) idx
  | .cast sp inner targetTy => checkTyDeep (resolveExpr ctx inner) targetTy (some sp)
  | .methodCall sp obj _ typeArgs args =>
    -- Keep skipping method name validation (needs type info to resolve receiver)
    let ctx := checkWrittenTys ctx typeArgs sp
    let ctx := resolveExpr ctx obj
    args.foldl resolveExpr ctx
  | .staticMethodCall sp typeName method typeArgs args =>
    let ctx := checkWrittenTys ctx typeArgs sp
    let mangledName := s!"{typeName}_{method}"
    let ctx := if lookupName ctx mangledName || hasMethodByIdentity ctx typeName method then ctx
               else addError ctx (.unknownStaticMethod typeName method) (some sp)
    args.foldl resolveExpr ctx
  | .fnRef sp name =>
    if lookupName ctx name || isKnownBuiltinFn name then ctx
    else addError ctx (.unknownFunctionRef name) (some sp)
  | .allocCall _ inner allocExpr =>
    resolveExpr (resolveExpr ctx inner) allocExpr
  | .ifExpr _ cond then_ else_ =>
    let ctx := resolveExpr ctx cond
    let ctx := resolveStmts ctx then_
    resolveStmts ctx else_

/-- Walk a list of statements, updating the context with let bindings. -/
partial def resolveStmts (ctx : ResolveCtx) (stmts : List Stmt) : ResolveCtx :=
  stmts.foldl resolveStmt ctx

/-- Walk a single statement. -/
partial def resolveStmt (ctx : ResolveCtx) (stmt : Stmt) : ResolveCtx :=
  match stmt with
  | .letDecl sp name _mutable ty value _isGhost =>
    -- A ghost let is in scope here like any binding; its erasure (and the rule
    -- that runtime code may not read it) is enforced in Elab.
    let ctx := resolveExpr ctx value
    let ctx := match ty with
      | some t => checkTyDeep ctx t (some sp)
      | none => ctx
    addLocal ctx name (.var ty _mutable)
  | .assign sp name value =>
    let ctx := if lookupName ctx name then ctx
               else addError ctx (.undeclaredVariable name) (some sp)
    resolveExpr ctx value
  | .return_ _ (some e) => resolveExpr ctx e
  | .return_ _ none => ctx
  | .expr _ e _ => resolveExpr ctx e
  | .ifElse _ cond then_ else_ =>
    let ctx := resolveExpr ctx cond
    let ctx := pushScope ctx
    let ctx := resolveStmts ctx then_
    let ctx := popScope ctx
    match else_ with
    | some elseStmts =>
      let ctx := pushScope ctx
      let ctx := resolveStmts ctx elseStmts
      popScope ctx
    | none => ctx
  | .while_ _ cond body _ =>
    let ctx := resolveExpr ctx cond
    let ctx := pushScope ctx
    let ctx := resolveStmts ctx body
    popScope ctx
  | .forLoop _ init cond step body _ =>
    let ctx := pushScope ctx
    let ctx := match init with
      | some s => resolveStmt ctx s
      | none => ctx
    let ctx := resolveExpr ctx cond
    let ctx := match step with
      | some s => resolveStmt ctx s
      | none => ctx
    let ctx := resolveStmts ctx body
    popScope ctx
  | .fieldAssign _ obj _ value =>
    resolveExpr (resolveExpr ctx obj) value
  | .derefAssign _ target value =>
    resolveExpr (resolveExpr ctx target) value
  | .arrayIndexAssign _ arr idx value =>
    resolveExpr (resolveExpr (resolveExpr ctx arr) idx) value
  | .break_ _ (some e) _ => resolveExpr ctx e
  | .break_ _ none _ => ctx
  | .continue_ _ _ => ctx
  | .defer _ body => resolveExpr ctx body
  | .borrowIn _ var ref _region _isMut body =>
    let ctx := addLocal ctx var (.var none false)
    let ctx := addLocal ctx ref (.var none false)
    resolveStmts ctx body
  | .letDestructure _ _ _ bindings value elseBody =>
    let ctx := resolveExpr ctx value
    let ctx := match elseBody with
      | some body => resolveStmts ctx body
      | none => ctx
    (PatBind.names bindings).foldl (fun c b => addLocal c b (.var none false)) ctx
  | .letStructDestructure _ _ bindings value =>
    let ctx := resolveExpr ctx value
    (PatBind.names bindings).foldl (fun c b => addLocal c b (.var none false)) ctx
  | .assert_ _ cond | .assume_ _ cond => resolveExpr ctx cond
end

-- ============================================================
-- Build global scope from a module
-- ============================================================

/-- Convert pre-built impl method signatures to scope symbols. -/
private def implSigsToSymbols (sigs : List (String × FnSummary)) : List (String × SymKind) :=
  sigs.map fun (mangledName, fs) =>
    -- Extract typeName from mangled "TypeName_methodName" (best-effort for SymKind)
    let typeName := match mangledName.splitOn "_" with
      | t :: _ => t
      | [] => mangledName
    (mangledName, SymKind.implMethod typeName
      (fs.params.map fun (n, ty) => { name := n, ty := ty }) fs.retTy fs.methodOf)

/-- Register all top-level definitions from a FileSummary into a scope. -/
private def buildGlobalScopeFromSummary (s : FileSummary) : Scope × List String :=
  let symbols : List (String × SymKind) := []
  let types : List String := builtinTypeNames
  -- Functions
  let symbols := symbols ++ s.functions.map fun (name, fs) =>
    (name, SymKind.fn (fs.params.map fun (n, ty) => { name := n, ty := ty }) fs.retTy)
  -- Structs
  let symbols := symbols ++ s.structs.map fun st => (st.name, SymKind.struct st)
  let types := types ++ s.structs.map (·.name)
  -- Enums
  let symbols := symbols ++ s.enums.map fun e => (e.name, SymKind.enum e)
  -- One walk (bug 274): a fold that appended each enum's variants to the symbols built so far
  -- copied them once per enum, quadratic in a module's enums.
  let symbols := symbols ++ s.enums.flatMap fun e =>
    e.variants.map fun v => (s!"{e.name}::{v.name}", SymKind.fn [] .unit)
  let types := types ++ s.enums.map (·.name)
  -- Traits
  let symbols := symbols ++ s.traits.map fun t => (t.name, SymKind.trait t)
  let types := types ++ s.traits.map (·.name)
  -- Constants
  let symbols := symbols ++ s.constants.map fun c => (c.name, SymKind.const c.ty)
  -- Type aliases
  let symbols := symbols ++ s.typeAliases.map fun ta => (ta.name, SymKind.typeAlias ta.targetTy)
  let types := types ++ s.typeAliases.map (·.name)
  -- Newtypes
  let symbols := symbols ++ s.newtypes.map fun nt => (nt.name, SymKind.newtype nt)
  let types := types ++ s.newtypes.map (·.name)
  -- Extern functions
  let symbols := symbols ++ s.externFns.map fun ef => (ef.name, SymKind.externFn ef.params ef.retTy)
  -- Impl method signatures (pre-built in FileSummary, not rebuilt from raw blocks)
  let symbols := symbols ++ implSigsToSymbols s.implMethodSigs
  -- Submodule definitions (via submoduleSummaries)
  let symbols := symbols ++ s.submoduleSummaries.flatMap fun (subName, subS) =>
    (subS.functions.map fun (name, fs) =>
      (s!"{subName}_{name}", SymKind.fn (fs.params.map fun (n, ty) => { name := n, ty := ty }) fs.retTy))
    ++ (subS.structs.map fun st => (st.name, SymKind.struct st))
    ++ (subS.enums.map fun e => (e.name, SymKind.enum e))
    ++ (subS.externFns.map fun ef => (s!"{subName}_{ef.name}", SymKind.externFn ef.params ef.retTy))
    ++ (subS.constants.map fun c => (c.name, SymKind.const c.ty))
    ++ (subS.newtypes.map fun nt => (nt.name, SymKind.newtype nt))
    ++ (implSigsToSymbols (subS.implMethodSigs.filter fun (name, _) =>
        subS.publicNames.contains name))
  let types := types ++ s.submoduleSummaries.flatMap fun (_, subS) =>
    (subS.structs.map (·.name))
    ++ (subS.enums.map (·.name))
    ++ (subS.typeAliases.map (·.name))
    ++ (subS.traits.map (·.name))
    -- Bug 229: a child's newtype is in Check's environment (`allNewtypes`), so it is a known type.
    ++ (subS.newtypes.map (·.name))
  (Scope.ofSymbols symbols, types)

/-- R-0492 slice 6: the type declarations a module may write by its own spelling, as Check's
    environment for it holds them (`checkModule`: `summary.structs`/`enums`, its aliases, and
    `allNewtypes`, which includes its children's newtypes): its own types, without its children's
    other types, plus the builtins (`builtinTypeNames`). Imports and, for a nested submodule, its
    siblings' types are added by the caller. One definition for top-level and nested modules:
    a top-level module used the program-wide list, so a type name of another module passed
    Resolve and reached the backend (bug 230), and another module's `enum Result` hid the
    builtin (bug 227). -/
private def moduleTypeEnvironment (s : FileSummary) : List (String × SymKind) × List String :=
  let (declScope, declTypes) := buildGlobalScopeFromSummary { s with submoduleSummaries := [] }
  let childNewtypes : List (String × SymKind) := s.submoduleSummaries.foldl
    (fun acc (_, c) => acc ++ c.newtypes.map (fun nt => (nt.name, SymKind.newtype nt))) []
  (declScope.symbols.filter (·.2.isType) ++ childNewtypes, declTypes ++ childNewtypes.map (·.1))

-- ============================================================
-- Resolve a module
-- ============================================================

/-- Resolve a single function body. -/
private def resolveFnBody (globalScope : Scope) (knownTypes : Std.HashSet String) (f : FnDef)
    (implType : Option String := none) (implTypeParams : List String := [])
    (traitMethods : List (String × List String) := [])
    (traitImpls : List (String × String) := []) : Diagnostics :=
  let ctx : ResolveCtx := {
    globalScope := globalScope
    localScopes := [{}]
    errors := []
    knownTypes := knownTypes
    extraKnownTypes := implTypeParams ++ f.typeParams
    typeParams := implTypeParams ++ f.typeParams
    currentImplType := implType
    traitMethods := traitMethods
    traitImpls := traitImpls
  }
  -- Validate parameter types and add params to scope
  let ctx := f.params.foldl (fun ctx p =>
    let ctx := addLocal ctx p.name (.var (some p.ty) false)
    -- The parser writes a method's `self` as the impl's bare type name (`&Box2` in
    -- `impl Tr for Box2<i32>`); its arguments are the impl target's, so it is not a bare
    -- generic type and no arity is looked up for it (bug 200).
    match implType with
    | some tn =>
      if p.name == selfParamName then
        let saved := ctx.typeParams
        let ctx := checkTyDeep { ctx with typeParams := tn :: saved } p.ty none
        { ctx with typeParams := saved }
      else checkTyDeep ctx p.ty none
    | none => checkTyDeep ctx p.ty none) ctx
  -- Validate return type
  let ctx := checkTyDeep ctx f.retTy none
  -- Walk body
  let ctx := resolveStmts ctx f.body
  ctx.errors

/-- Immediate bare-name target of an alias, if the target is a plain `.named`. -/
private def aliasNamedTarget : Ty → Option String
  | .named n => some n
  | _ => none

/-- True if following the alias-name chain from `cur` revisits a name (the alias
    is part of a cycle). `aliasMap` maps each alias name to its bare-named target,
    restricted to targets that are themselves aliases. `fuel` bounds recursion for
    totality (set to #aliases+1, longer than any acyclic chain). -/
private def aliasChaseCyclic (aliasMap : List (String × String)) : Nat → List String → String → Bool
  | 0, _, _ => true
  | fuel+1, seen, cur =>
    match aliasMap.lookup cur with
    | some next => if seen.contains next then true else aliasChaseCyclic aliasMap fuel (cur :: seen) next
    | none => false

/-- Validate a module's type-alias targets: every name in a target must be known
    (`type A = Nope` → E0108), and no alias may be recursive (`type A = A` or
    `type A = B; type B = A` → E0112). -/
private def resolveTypeAliases (m : Module) (globalScope : Scope) (knownTypes : Std.HashSet String) : Diagnostics :=
  let aliasCtx : ResolveCtx :=
    { globalScope := globalScope, localScopes := [{}], errors := [], knownTypes := knownTypes }
  let aliasNames := m.typeAliases.map (·.name)
  let aliasMapAliasesOnly : List (String × String) := m.typeAliases.filterMap (fun ta =>
    match aliasNamedTarget ta.targetTy with
    | some tgt => if aliasNames.contains tgt then some (ta.name, tgt) else none
    | none => none)
  m.typeAliases.foldl (fun acc ta =>
    let tErrs := (checkTyDeep { aliasCtx with errors := [] } ta.targetTy (some ta.span)).errors
    let recErrs := if aliasChaseCyclic aliasMapAliasesOnly (aliasNames.length + 1) [] ta.name
                   then [mkResolveDiag (.recursiveTypeAlias ta.name) (some ta.span)] else []
    acc ++ tErrs ++ recErrs) []

/-- A compiler intrinsic the compiler actually implements: `#[intrinsic = "n"]` on a declaration
    named `n` that `resolveIntrinsic` knows, with no `#[link_name]`. Any other annotation is not an
    implementation: the declaration stays a foreign binding under every rule (R3, link names). -/
def isCompilerIntrinsic (ef : ExternFnDecl) : Bool :=
  match ef.intrinsic with
  | some v => v == ef.name && (resolveIntrinsic v).isSome && ef.linkName.isNone
  | none => false

/-- R-0484 R3 over one module (the body walk visits every nested submodule: bugs 113, 217). A `#[intrinsic]`
    declaration is not a foreign binding (the compiler implements it, no C code stands behind
    it), so R3 does not apply to it; std.mem's `sizeof`/`alignof` are the instances, and they
    reach this walk because std's modules are submodules in a project build. -/
private def externEffectsCheck (m : Module) (ctx : ResolveCtx) : ResolveCtx :=
  let ctx := m.externFns.foldl (fun ctx ef =>
    let sym := ef.linkName.getD ef.name
    if isCompilerIntrinsic ef then ctx else
    let ctx := match ef.linkName with
      | some l => if Symbols.isForeignSymbolSpelling l then ctx
                  else addError ctx (.invalidLinkName ef.name l "a link name is ASCII letters, digits, '_', '$' and '.', does not start with a digit, and is outside the Concrete symbol namespace ('_C.' / '_C$')") (some ef.span)
      | none => ctx
    -- A call spelled like a compiler intrinsic is compiled as the intrinsic and never reaches
    -- C, so a foreign binding of that name is refused, except `free` and `abort` without a
    -- link name, whose intrinsics ARE the C functions of those names (std binds both).
    let ctx := if (resolveIntrinsic ef.name).isSome
        && !(ef.linkName.isNone && (match resolveIntrinsic ef.name with
              | some .free | some .abort => true
              | _ => false)) then
        addError ctx (.invalidLinkName ef.name sym s!"'{ef.name}' is the name of a compiler intrinsic, so a call to it is compiled as the intrinsic and never reaches the C function") (some ef.span)
      else ctx
    let ctx := if Symbols.isRuntimeHelperName sym then
        addError ctx (.invalidLinkName ef.name sym "it is the name of a compiler runtime helper, which the backend moves into its reserved namespace; the binding would be captured by the helper") (some ef.span)
      else ctx
    -- The effectful-symbol judgment is about the C function actually called.
    match ef.capSet with
    | none => addError ctx (.externEffectsUndeclared ef.name) (some ef.span)
    | some cs =>
      if cs.isEmpty && Capabilities.knownEffectfulForeignSymbols.contains sym
      then addError ctx (.effectfulSymbolDeclaredPure sym) (some ef.span)
      else ctx) ctx
  -- An import renamed to a compiler intrinsic's name would be compiled as the intrinsic: an
  -- imported foreign binding (`import c.{abs as sizeof}`) would never reach C (Codex review).
  m.imports.foldl (fun ctx imp =>
    imp.symbols.foldl (fun ctx sym =>
      match sym.alias with
      | some a => if a != sym.name && (resolveIntrinsic a).isSome
                  then addError ctx (.importAliasIsIntrinsic sym.name a) (some imp.span) else ctx
      | none => ctx) ctx) ctx

/-- Bug 188: a capability parameter may not be spelled like a capability (a built-in
    capability, `Std`, or one of the module's capability aliases). Inside a fn-pointer
    type a capability variable is spelled as a plain name (`fn() with(C)`), and the one
    entailment rule (`Capabilities.missingCaps`) matches a requirement against the
    caller's variables by name, so `fn f<cap Unsafe>(..) with(Unsafe)` would hold the
    real `Unsafe` without being granted it. Keeping the two namespaces disjoint is what
    makes name matching sound. Over one module; the body walk visits every submodule. -/
private def capParamNamesCheck (m : Module) (ctx : ResolveCtx) : ResolveCtx :=
  let aliases := m.capAliases.map (·.name)
  let check (owner : String) (params : List String) (sp : Span) (ctx : ResolveCtx) : ResolveCtx :=
    params.foldl (fun ctx p =>
      if Capabilities.capParamNameReserved aliases p
      then addError ctx (.capParamNamesCapability owner p) (some sp) else ctx) ctx
  let ctx := m.functions.foldl (fun ctx f => check f.name f.capParams f.span ctx) ctx
  let ctx := m.structs.foldl (fun ctx sd => check sd.name sd.capParams sd.span ctx) ctx
  let ctx := m.implBlocks.foldl (fun ctx ib =>
    let ctx := check ib.typeName ib.capParams ib.span ctx
    ib.methods.foldl (fun ctx f => check s!"{ib.typeName}::{f.name}" f.capParams f.span ctx) ctx) ctx
  m.traitImpls.foldl (fun ctx ti =>
    ti.methods.foldl (fun ctx f => check s!"{ti.typeName}::{f.name}" f.capParams f.span ctx) ctx) ctx

/-- The trait names a type mentions where a type is meant (bug 240), with `params` shadowing. -/
private partial def traitNamesInTy (ctx : ResolveCtx) : Ty → List String
  | .named n => if denotesTrait ctx n then [n] else []
  | .generic n args _ => (if denotesTrait ctx n then [n] else []) ++ args.flatMap (traitNamesInTy ctx)
  | .ref t | .refMut t | .ptrMut t | .ptrConst t | .heap t | .heapArray t | .array t _ =>
    traitNamesInTy ctx t
  | .fn_ ps _ r => ps.flatMap (traitNamesInTy ctx) ++ traitNamesInTy ctx r
  | _ => []

/-- Bug 240: a declaration's own types (a struct's fields, an enum's payloads, a newtype's
    representation, a trait method's signature, a foreign binding's signature) may not name a
    trait. Resolve walks these with no `checkTyDeep` (Check validates them), so the trait half of
    TYPE-2 is enforced here; function and impl-method signatures, bodies, aliases and constants
    already meet `checkTyDeep`. -/
private def declTraitTypesCheck (m : Module) (ctx : ResolveCtx) : ResolveCtx :=
  let check (params : List String) (ty : Ty) (sp : Span) (ctx : ResolveCtx) : ResolveCtx :=
    (traitNamesInTy { ctx with typeParams := params } ty).eraseDups.foldl
      (fun ctx n => addError ctx (.traitIsNotAType n) (some sp)) ctx
  let ctx := m.structs.foldl (fun ctx sd =>
    sd.fields.foldl (fun ctx f => check sd.typeParams f.ty sd.span ctx) ctx) ctx
  let ctx := m.enums.foldl (fun ctx ed =>
    ed.variants.foldl (fun ctx v =>
      v.fields.foldl (fun ctx f => check ed.typeParams f.ty ed.span ctx) ctx) ctx) ctx
  let ctx := m.newtypes.foldl (fun ctx nt => check nt.typeParams nt.innerTy nt.span ctx) ctx
  -- A trait's method signatures and a foreign binding's signature are not walked by any
  -- `resolveFnBody` either (Codex review r2).
  let ctx := m.traits.foldl (fun ctx td =>
    td.methods.foldl (fun ctx ms =>
      let ctx := ms.params.foldl (fun ctx p => check td.typeParams p.ty td.span ctx) ctx
      check td.typeParams ms.retTy td.span ctx) ctx) ctx
  m.externFns.foldl (fun ctx ef =>
    let ctx := ef.params.foldl (fun ctx p => check [] p.ty ef.span ctx) ctx
    check [] ef.retTy ef.span ctx) ctx

/-- Bug 231: every trait a bound names resolves (MOD-1). The traits a module's bounds may name are
    the ones Check gives that module (`allTraits`: the builtin `Destroy` and the module's own
    traits, with `Copy` read by `checkTraitBounds`); traits are module-local and cannot be imported
    (TRAIT spec). Nothing checked the names, so a bound on an undeclared trait was accepted: a
    generic function nobody called compiled, and a bound naming an enclosing module's trait in a
    nested module was satisfied by spelling at the call while its methods were unknown in the body. -/
private def boundTraitsCheck (m : Module) (ctx : ResolveCtx) : ResolveCtx :=
  let known : List String := "Copy" :: destroyTraitName :: m.traits.map (·.name)
  let check (owner : String) (bounds : List (String × List String)) (sp : Span) (ctx : ResolveCtx)
      : ResolveCtx :=
    bounds.foldl (fun ctx (param, traits) =>
      traits.foldl (fun ctx tr =>
        if known.contains tr then ctx
        else addError ctx (.unknownBoundTrait tr param owner) (some sp)) ctx) ctx
  let ctx := m.functions.foldl (fun ctx f => check f.name f.typeBounds f.span ctx) ctx
  let ctx := m.structs.foldl (fun ctx sd => check sd.name sd.typeBounds sd.span ctx) ctx
  let ctx := m.enums.foldl (fun ctx ed => check ed.name ed.typeBounds ed.span ctx) ctx
  let ctx := m.newtypes.foldl (fun ctx nt => check nt.name nt.typeBounds nt.span ctx) ctx
  let ctx := m.implBlocks.foldl (fun ctx ib =>
    let ctx := check ib.typeName ib.typeBounds ib.span ctx
    ib.methods.foldl (fun ctx f => check s!"{ib.typeName}::{f.name}" f.typeBounds f.span ctx) ctx) ctx
  m.traitImpls.foldl (fun ctx ti =>
    let ctx := check s!"impl {ti.traitName} for {ti.typeName}" ti.typeBounds ti.span ctx
    ti.methods.foldl (fun ctx f => check s!"{ti.typeName}::{f.name}" f.typeBounds f.span ctx) ctx) ctx

/-- Resolve all names in one module's declarations and bodies (not its submodules: see
    `resolveModuleTreeErrors`). -/
private def resolveModule (m : Module) (globalScope : Scope) (knownTypeList : List String)
    (traitMethods : List (String × List String))
    (traitImpls_ : List (String × String)) : Diagnostics :=
  let knownTypes := Std.HashSet.ofList knownTypeList
  -- Validate type-alias targets (unknown target types, recursive aliases)
  let aliasErrors := resolveTypeAliases m globalScope knownTypes
  -- Check top-level functions
  let fnErrors := m.functions.foldl (fun acc f =>
    acc ++ resolveFnBody globalScope knownTypes f none [] traitMethods traitImpls_) []
  -- Check impl block methods (with Self)
  let implErrors := m.implBlocks.foldl (fun acc ib =>
    acc ++ ib.methods.foldl (fun acc method =>
      acc ++ resolveFnBody globalScope knownTypes method (some ib.typeName) ib.typeParams traitMethods traitImpls_) []) []
  -- Check trait impl methods (with Self)
  let traitImplErrors := m.traitImpls.foldl (fun acc ti =>
    acc ++ ti.methods.foldl (fun acc method =>
      acc ++ resolveFnBody globalScope knownTypes method (some ti.typeName) ti.typeParams traitMethods traitImpls_) []) []
  -- R-0484 R3: every foreign binding declares its effects, and a known-effectful
  -- symbol may not be declared effect-free. The rule covers EVERY module because
  -- `resolveModuleTreeErrors` visits every nested submodule (bug 113: it read only
  -- `m.externFns`, so an undeclared `extern fn` inside `mod t { mod ffi { .. } }` was accepted).
  let externCtx0 : ResolveCtx :=
    { globalScope := globalScope, localScopes := [{}], errors := [], knownTypes := knownTypes }
  let externErrors := (externEffectsCheck m externCtx0).errors
  let capParamErrors := (capParamNamesCheck m externCtx0).errors
  let boundErrors := (boundTraitsCheck m externCtx0).errors
  let declTraitErrors := (declTraitTypesCheck m externCtx0).errors
  -- Bug 215 (Codex review): a constant's declared type and initializer are written types and
  -- an expression like any function body's; the initializer was never walked, so
  -- `const X: Int = (7 as Nope) as Int` reached the backend.
  let constErrors := m.constants.foldl (fun acc c =>
    let ctx := checkTyDeep externCtx0 c.ty (some c.span)
    acc ++ (resolveExpr ctx c.value).errors) []
  let allErrors := aliasErrors ++ fnErrors ++ implErrors ++ traitImplErrors ++ externErrors ++ capParamErrors
    ++ boundErrors ++ declTraitErrors ++ constErrors
  allErrors

/-- The names a nested submodule's imports bring into its scope, by the rule Check and Elab
    apply to that submodule (`resolveImports`), so the Resolve walk admits exactly the imports the
    later passes admit. `none` when the imports do not resolve: Check reports that refusal for a
    submodule (`checkSubmodules`, E0284/E0285) and does not descend past it, and Resolve, whose
    scope for the submodule would be incomplete, leaves it to Check the same way. -/
private def importScope (summaryTable : List (String × FileSummary)) (imports : List ImportDecl)
    (importer : FileSummary) : Option (List (String × SymKind) × List String) :=
  match resolveImports imports summaryTable (fun _ => "") (fun _ _ => "") importer with
  | .error _ => none
  | .ok ri =>
    let syms : List (String × SymKind) :=
      ri.functions.map (fun (n, fs) =>
        (n, SymKind.fn (fs.params.map fun (pn, ty) => { name := pn, ty := ty }) fs.retTy))
      ++ ri.structs.map (fun sd => (sd.name, SymKind.struct sd))
      ++ ri.enums.map (fun ed => (ed.name, SymKind.enum ed))
      ++ ri.typeAliases.map (fun (n, ty) => (n, SymKind.typeAlias ty))
      ++ ri.newtypes.map (fun nt => (nt.name, SymKind.newtype nt))
      ++ ri.constants.map (fun c => (c.localName, SymKind.const c.ty))
      ++ implSigsToSymbols ri.implMethodSigs
    let tys := ri.structs.map (·.name) ++ ri.enums.map (·.name) ++ ri.typeAliases.map (·.1)
      ++ ri.newtypes.map (·.name)
    some (syms, tys)

/-- Bug 217: resolve a module and, recursively, every nested submodule (inline `mod x { .. }`
    and every `mod x;` file of a project, std included). Before this only the modules of the
    top-level list were walked, so a submodule produced no Resolve diagnostics and an unknown
    cast target there reached the backend.

    A submodule's symbol scope is the environment Check's `checkSubmodules`/`checkModule` give it,
    in the same precedence: its imports (`importScope`); its own declarations, with its
    children's functions and foreign bindings by qualified name (`leaf_two`) and its children's
    newtypes but not their other types or constants; then the structs, enums and public methods of the modules
    declared beside it (which Check and Elab inject). Nothing of an enclosing module is in it:
    an enclosing `enum E` or `enum Result` would otherwise answer, by first-wins lookup, where
    Check selects a sibling's `E` or the builtin `Result` (Codex reviews of this fix), and the
    program-wide `globalScope` would answer a user's non-generic `Result` inside `std.option`.
    Its known type names are the same environment's, with nothing of an enclosing module either:
    a type name Resolve admits and Check cannot expand reaches the backend. -/
private partial def resolveModuleTreeErrors (summaryTable : List (String × FileSummary))
    (m : Module) (summary : FileSummary) (scope : Scope) (knownTypes : List String)
    (traitMethods : List (String × List String)) (traitImpls_ : List (String × String))
    : Diagnostics :=
  let errs := resolveModule m scope knownTypes traitMethods traitImpls_
  let siblingSyms : List (String × SymKind) := summary.submoduleSummaries.foldl (fun acc (_, s) =>
    acc ++ s.structs.map (fun sd => (sd.name, SymKind.struct sd))
      ++ s.enums.map (fun ed => (ed.name, SymKind.enum ed))
      ++ implSigsToSymbols (s.implMethodSigs.filter fun (name, _) => s.publicNames.contains name)) []
  let siblingTypes : List String := summary.submoduleSummaries.foldl (fun acc (_, s) =>
    acc ++ s.structs.map (·.name) ++ s.enums.map (·.name)) []
  m.submodules.foldl (fun acc sub =>
    let subSummary := match summary.submoduleSummaries.find? fun (n, _) => n == sub.name with
      | some (_, s) => s
      | none => buildFileSummary sub
    match importScope summaryTable sub.imports subSummary with
    | none => acc
    | some (impSyms, impTys) =>
      -- Its own declarations without its children's (Check's `summary.structs`/`enums`/...).
      let (declScope, _) := buildGlobalScopeFromSummary { subSummary with submoduleSummaries := [] }
      let (_, ownTypes) := moduleTypeEnvironment subSummary
      -- Its children's functions and foreign bindings by qualified name (Check's `submoduleNames`).
      let childFns : List (String × SymKind) := subSummary.submoduleSummaries.foldl
        (fun acc (childName, c) => acc
          ++ c.functions.map (fun (n, fs) => (s!"{childName}_{n}",
              SymKind.fn (fs.params.map fun (pn, ty) => { name := pn, ty := ty }) fs.retTy))
          ++ c.externFns.map (fun ef => (s!"{childName}_{ef.name}",
              SymKind.externFn ef.params ef.retTy))
          -- and its children's newtypes, which Check's `allNewtypes` includes (bug 229).
          ++ c.newtypes.map (fun nt => (nt.name, SymKind.newtype nt))) []
      let subSyms := impSyms ++ declScope.symbols ++ childFns ++ siblingSyms
      -- Its known types are the same environment's: its own (with the builtins), its children's
      -- newtypes, its imports' and its siblings' (Codex review r4: inheriting the enclosing
      -- module's let `outer`'s `type Nope` pass in `inner`, where Check cannot expand it, and the
      -- cast reached the backend again).
      let subTypes := ownTypes ++ impTys ++ siblingTypes
      let sErrs := resolveModuleTreeErrors summaryTable sub subSummary (Scope.ofSymbols subSyms)
        subTypes traitMethods traitImpls_
      acc ++ sErrs.stampFile sub.sourceFile) errs

-- ============================================================
-- Shallow (declaration-level) resolution
-- ============================================================

/-- One module's imports, validated against the summary table (E0110 unknown module, E0111
    not public), with the symbols and type names they bring into its scope. For each
    top-level module; a nested submodule's imports go through `importScope`. -/
private def summaryImports (summaryTable : List (String × FileSummary)) (s : FileSummary)
    : Diagnostics × List (String × SymKind) × List String :=
  let fullExportTable := summaryTable.map fun (n, s) => (n, s.publicNames)
  -- Validate imports from summaries and add imported symbols to scope
  [s].foldl (fun (errs, syms, tys) s =>
    s.imports.foldl (fun (errs, syms, tys) imp =>
      match fullExportTable.find? (fun (n, _) => n == imp.moduleName) with
      | none =>
        -- First-contact friction (base64_cli FRICTION #1): `concrete file.con`
        -- can't resolve std.* — say why instead of a bare "unknown module".
        let d := mkResolveDiag (.unknownModule imp.moduleName) (some imp.span)
        let d := if imp.moduleName.startsWith "std." then
            { d with hint := some "std.* imports resolve in project mode — create a project (add Concrete.toml + src/main.con) and build with `concrete build`; inside a project, check the module name for a typo" }
          else d
        (errs ++ [d], syms, tys)
      | some (_, pubNames) =>
        -- Look up the full module summary to get type info for imported symbols
        let modSummary := summaryTable.find? fun (n, _) => n == imp.moduleName
        imp.symbols.foldl (fun (errs, syms, tys) sym =>
          -- Bug 233: a public trait is in `pubNames` but no import binds it; it was
          -- admitted here, bound nothing, and Check refused it as "not public".
          let unimportable := modSummary.bind fun (_, ms) => unimportableKind ms sym.name
          -- Bug 226 (MOD-2, MOD-6): the import rule `resolveImports` applies to every module.
          -- std.libc's bindings, which are not `pub`, are importable by std's modules only.
          let refusal := modSummary.bind fun (_, ms) => importRefusal s ms sym.name
          let stdOwn := modSummary.any fun (_, ms) =>
            ms.definitionPath == stdPrivateModulePath && inStdPackage s.definitionPath
              && (ms.functions ++ ms.externFnSigs).any (·.1 == sym.name)
          if refusal == some .stdPrivate then
            (errs ++ [mkResolveDiag (.stdPrivateImport sym.name imp.moduleName) (some imp.span)], syms, tys)
          else if (pubNames.contains sym.name || stdOwn) && unimportable.isNone then
            let localName := sym.effectiveName
            -- Try to bring the symbol into scope from the module summary
            match modSummary with
            | some (_, ms) =>
              -- Check if it's a function
              match ms.functions.find? fun (n, _) => n == sym.name with
              | some (_, fs) =>
                let entry := (localName, SymKind.fn (fs.params.map fun (n, ty) => { name := n, ty := ty }) fs.retTy)
                (errs, syms ++ [entry], tys)
              | none =>
              -- Check if it's an extern fn
              match ms.externFnSigs.find? fun (n, _) => n == sym.name with
              | some (_, fs) =>
                let entry := (localName, SymKind.fn (fs.params.map fun (n, ty) => { name := n, ty := ty }) fs.retTy)
                (errs, syms ++ [entry], tys)
              | none =>
              -- Check if it's a struct
              match ms.structs.find? fun st => st.name == sym.name with
              | some st =>
                (errs, syms ++ [(localName, SymKind.struct st)], tys ++ [localName])
              | none =>
              -- Check if it's an enum
              match ms.enums.find? fun e => e.name == sym.name with
              | some e =>
                (errs, syms ++ [(localName, SymKind.enum e)], tys ++ [localName])
              | none =>
              -- Newtypes and type aliases are importable types too (Elab's
              -- resolveImports already carried them; Resolve silently dropped
              -- them, so `import std.numeric.{NonZeroU32}` hit E0108 at the
              -- first use — found by the std compiled-coverage gate).
              -- Prepend (imported names are distinct, order is immaterial):
              -- the append-single shape is ratcheted by check_no_quadratic_append.
              match ms.newtypes.find? fun nt => nt.isPublic && nt.name == sym.name with
              | some nt =>
                (errs, (localName, SymKind.newtype nt) :: syms, localName :: tys)
              | none =>
              match ms.typeAliases.find? fun ta => ta.isPublic && ta.name == sym.name with
              | some ta =>
                (errs, (localName, SymKind.typeAlias ta.targetTy) :: syms, localName :: tys)
              | none =>
              -- R-0492 step 4 (bug 233): a public constant, as `resolveImports` binds it.
              match ms.constants.find? fun c => c.isPublic && c.name == sym.name with
              | some c => (errs, (localName, SymKind.const c.ty) :: syms, tys)
              | none => (errs, syms, tys)
            | none => (errs, syms, tys)
          else
            let err := match unimportable with
              | some kind => ResolveError.unimportable kind sym.name imp.moduleName
              | none => .notPublicInModule sym.name imp.moduleName
            (errs ++ [mkResolveDiag err (some imp.span)], syms, tys)
        ) (errs, syms, tys)
    ) (errs, syms, tys)
  ) (([] : Diagnostics), ([] : List (String × SymKind)), ([] : List String))

/-- Perform declaration-level resolution: build global scope, collect trait info,
    validate imports, and check trait impl completeness. Works entirely from FileSummary artifacts. -/
def resolveShallow (moduleSummaries : List FileSummary)
    (summaryTable : List (String × FileSummary) := []) : ShallowResult :=
  -- Each module's declaration scope, built once (bug 274): the combined scope and the
  -- per-module scopes below both read it.
  let declScopes := moduleSummaries.map fun s => (s, buildGlobalScopeFromSummary s)
  -- Build combined global scope from all module summaries
  let (combinedScope, combinedTypes) := declScopes.foldl (fun (scope, types) (_, sScope, sTypes) =>
    (Scope.ofSymbols (scope.symbols ++ sScope.symbols), types ++ sTypes)
  ) (Scope.ofSymbols [], ([] : List String))
  -- For inline sibling modules (mod A {} mod B {}), register qualified names (A_fn)
  -- so that B can use A::fn() syntax (parsed as A_fn).
  let siblingQualified : List (String × SymKind) := moduleSummaries.foldl (fun acc s =>
    if s.name == rootModuleName then acc
    else
      let qualFns : List (String × SymKind) :=
        (s.functions.filter fun (name, _) => s.publicNames.contains name).map fun (name, fs) =>
          (s.name ++ "_" ++ name, SymKind.fn (fs.params.map fun (n, ty) => { name := n, ty := ty }) fs.retTy)
      let qualExterns : List (String × SymKind) :=
        (s.externFns.filter fun ef => s.publicNames.contains ef.name).map fun ef =>
          (s.name ++ "_" ++ ef.name, SymKind.externFn ef.params ef.retTy)
      let qualImpls : List (String × SymKind) :=
        (s.implMethodSigs.filter fun (name, _) => s.publicNames.contains name).map fun (name, fs) =>
          (s.name ++ "_" ++ name, SymKind.fn (fs.params.map fun (n, ty) => { name := n, ty := ty }) fs.retTy)
      acc ++ qualFns ++ qualExterns ++ qualImpls
  ) []
  let combinedScope : Scope := Scope.ofSymbols (combinedScope.symbols ++ siblingQualified)
  -- Collect trait methods and trait impls from all summaries
  let traitMethods := moduleSummaries.foldl (fun acc s =>
    acc ++ s.traits.map fun t => (t.name, t.methods.map (·.name))) []
  let traitImpls_ := moduleSummaries.foldl (fun acc s =>
    acc ++ s.traitImpls.map fun ti => (ti.typeName, ti.traitName)) []
  let importsOf := summaryImports summaryTable
  let perModule := declScopes.map fun (s, sScope, _) => (s, sScope, importsOf s)
  let importErrors := perModule.flatMap fun (_, _, (e, _, _)) => e
  let importedSymbols := perModule.flatMap fun (_, _, (_, syms, _)) => syms
  let importedTypes := perModule.flatMap fun (_, _, (_, _, tys)) => tys
  let moduleScopes := perModule.map fun (s, sScope, (_, syms, _)) =>
    (s.name, syms ++ sScope.symbols)
  -- The imported types are what import resolution transports, as Check's `resolveImports`
  -- gives them: the named types and the types their signatures and fields reach (the import
  -- closure, bug 096). When an import fails, `summaryImports` has reported it; its own bindings
  -- stand in so the module's other diagnostics are still produced.
  let moduleTypeEnvs := perModule.map fun (s, _, (_, syms, tys)) =>
    let (ownSyms, ownTys) := moduleTypeEnvironment s
    let (impSyms, impTys) := (importScope summaryTable s.imports s).getD (syms, tys)
    (s.name, impSyms.filter (·.2.isType) ++ ownSyms, impTys ++ ownTys)
  let combinedScope := Scope.ofSymbols (combinedScope.symbols ++ importedSymbols)
  let combinedTypes := combinedTypes ++ importedTypes
  { globalScope := combinedScope
  , knownTypes := combinedTypes
  , traitMethods := traitMethods
  , traitImpls := traitImpls_
  , errors := importErrors
  , moduleScopes := moduleScopes
  , moduleTypeEnvs := moduleTypeEnvs
  , summaryTable := summaryTable }

-- ============================================================
-- Body-level resolution
-- ============================================================

/-- A type an exported function may take or return: one passed identically by the C ABI and
    by Concrete's calling convention (32- and 64-bit integers, floats, raw pointers; `()` as a
    return). 8- and 16-bit integers are excluded: the C ABI extends them (`signext`/`zeroext`),
    which emission does not do, so a C caller would read `(i8)-1` as 255 (Codex review). -/
private def isExportScalarTy : Ty → Bool
  | .int | .uint | .i32 | .u32 | .float64 | .float32 => true
  | .ptrMut _ | .ptrConst _ => true
  | _ => false

private partial def collectFns (m : Module) : List FnDef :=
  m.functions ++ m.submodules.foldl (fun acc sub => acc ++ collectFns sub) []

private partial def collectExterns (m : Module) : List ExternFnDecl :=
  m.externFns ++ m.submodules.foldl (fun acc sub => acc ++ collectExterns sub) []

/-- `#[export_name]` (FFI.md "Symbol Names", rule 2; R-0286): the one way a Concrete-defined
    function gets a C name. Refused (E0119) unless the symbol is a C identifier that the
    program does not already use for something else — the process entry, a compiler runtime
    or C runtime symbol the emitted code calls (exporting `malloc` would replace the
    allocator: bug 129 spelled explicitly), a foreign symbol an `extern fn` imports, or
    another export — and the function has a C-compatible shape: not generic, not `main`, not
    a test, scalar parameters and result. Program-wide, over every module and submodule. -/
def exportErrors (modules : List Module) : Diagnostics :=
  let fns := modules.foldl (fun acc m => acc ++ collectFns m) []
  let externSyms := (modules.foldl (fun acc m => acc ++ collectExterns m) []).map
    fun ef => ef.linkName.getD ef.name
  let exported := fns.filterMap fun f => f.exportName.map (f, ·)
  exported.foldl (fun errs (f, sym) =>
    -- Prepended, reversed once below (the quadratic-append ratchet).
    let bad (reason : String) := mkResolveDiag (.invalidExport f.name sym reason) (some f.span) :: errs
    if !Symbols.isCIdentifier sym then bad "the symbol must be a C identifier (letters, digits and '_', not starting with a digit)"
    else if sym == cEntrySymbol then bad "'main' is the process entry the compiler generates"
    else if sym.startsWith "__" then bad "the '__' prefix is reserved for the compiler runtime"
    else if Symbols.isRuntimeHelperName sym then
      bad s!"'{sym}' is the name of a compiler runtime helper"
    else if Symbols.cRuntimeSymbols.contains sym then
      bad s!"'{sym}' is a C runtime symbol the compiled program itself calls; exporting a definition under it would replace it for the whole program"
    else if externSyms.contains sym then
      bad s!"'{sym}' is imported by an `extern fn` in this program; one symbol cannot be both imported and defined"
    else if (exported.filter (·.2 == sym)).length > 1 then bad s!"several functions are exported as '{sym}'"
    else if !f.typeParams.isEmpty then bad "a generic function has no single C signature"
    else if f.name == mainFnName then bad "the program's `main` is called by the generated entry, not exported"
    else if f.isTest then bad "a #[test] function is not exported"
    else if !(f.params.all fun p => isExportScalarTy p.ty) then
      bad "every parameter must be a 32- or 64-bit integer, a float or a raw pointer (the types C and Concrete pass alike)"
    else if !(f.retTy == .unit || isExportScalarTy f.retTy) then
      bad "the result must be (), a 32- or 64-bit integer, a float or a raw pointer (the types C and Concrete pass alike)"
    else errs) [] |>.reverse

/-- A foreign binding's identity is its name (`CallableId.ofExtern`), so one name must bind one
    C symbol across the program: two `extern fn f` declarations with different `#[link_name]`s
    (or one with and one without) are refused (E0118) rather than resolved by declaration order. -/
def linkNameConflicts (modules : List Module) : Diagnostics :=
  let exts := modules.foldl (fun acc m => acc ++ collectExterns m) []
  let exts := exts.filter fun ef => !isCompilerIntrinsic ef
  let (_, errs) := exts.foldl (fun (seen, errs) ef =>
    let sym := ef.linkName.getD ef.name
    match seen.lookup ef.name with
    | some other =>
      if other == sym then (seen, errs)
      else (seen, mkResolveDiag (.invalidLinkName ef.name sym s!"another declaration of '{ef.name}' binds '{other}'; a foreign binding's identity is its name, so one name binds one C symbol") (some ef.span) :: errs)
    | none => ((ef.name, sym) :: seen, errs)) (([] : List (String × String)), ([] : Diagnostics))
  errs.reverse

/-- Resolve bodies, returning the (always structurally complete, side-table-resolved)
    modules ALONGSIDE any diagnostics, rather than discarding the modules on error.
    Resolution never modifies the AST (it validates via side tables), so the returned
    modules are safe to feed to a later *diagnostic* pass even when diagnostics are
    non-empty. Callers that feed proof/codegen must reject non-empty diagnostics.
    (ROADMAP Phase 4 #12a.) -/
def resolveBodiesPartial (modules : List Module) (shallow : ShallowResult) : (List ResolvedModule × Diagnostics) :=
  let (resolved, bodyErrors) := modules.foldl (fun (acc, errs) m =>
    -- Values: the module's own and imported symbols, then the program-wide scope. Types: the
    -- module's own type environment only (R-0492 slice 6).
    let typeEnv := shallow.moduleTypeEnvs.lookup m.name
    let scope := match shallow.moduleScopes.lookup m.name, typeEnv with
      | some own, some (typeSyms, _) =>
        Scope.ofSymbolsTyped (own ++ shallow.globalScope.symbols) typeSyms
      | some own, none => Scope.ofSymbols (own ++ shallow.globalScope.symbols)
      | none, _ => shallow.globalScope
    let knownTypes := match typeEnv with
      | some (_, tys) => tys
      | none => shallow.knownTypes
    let summary := match shallow.summaryTable.lookup m.name with
      | some s => s
      | none => buildFileSummary m
    let mErrs := resolveModuleTreeErrors shallow.summaryTable m summary scope knownTypes
      shallow.traitMethods shallow.traitImpls
    let rm : ResolvedModule := { module := m, globalScope := scope }
    (acc ++ [rm], errs ++ mErrs)
  ) ([], [])
  (resolved, shallow.errors ++ bodyErrors ++ exportErrors modules ++ linkNameConflicts modules)

def resolveBodies (modules : List Module) (shallow : ShallowResult) : Except Diagnostics (List ResolvedModule) :=
  let (resolved, allErrors) := resolveBodiesPartial modules shallow
  if hasErrors allErrors then
    .error allErrors
  else
    .ok resolved

-- ============================================================
-- Entry point
-- ============================================================

/-- Resolve all modules. Returns resolved modules or diagnostics on failure. -/
-- spec: TYPE-15 (docs/spec/types.md; this declaration enforces the rule)
def resolveProgram (modules : List Module) (summaryTable : List (String × FileSummary) := []) : Except Diagnostics (List ResolvedModule) :=
  let moduleSummaries := modules.map fun m =>
    match summaryTable.find? fun (n, _) => n == m.name with
    | some (_, s) => s
    | none => buildFileSummary m
  let shallow := resolveShallow moduleSummaries summaryTable
  resolveBodies modules shallow

/-- Like `resolveProgram` but returns the (always structurally complete) resolved
    modules ALONGSIDE diagnostics, never discarding them. For the error-tolerant
    diagnostics path only (ROADMAP Phase 4 #12a) — proof/codegen use `resolveProgram`. -/
def resolveProgramPartial (modules : List Module) (summaryTable : List (String × FileSummary) := []) : (List ResolvedModule × Diagnostics) :=
  let moduleSummaries := modules.map fun m =>
    match summaryTable.find? fun (n, _) => n == m.name with
    | some (_, s) => s
    | none => buildFileSummary m
  let shallow := resolveShallow moduleSummaries summaryTable
  resolveBodiesPartial modules shallow

end Concrete
