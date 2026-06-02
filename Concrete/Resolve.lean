import Concrete.AST
import Concrete.Diagnostic
import Concrete.Token
import Concrete.FileSummary
import Concrete.Intrinsic

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
  | implMethod (typeName : String) (params : List Param) (retTy : Ty)
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
  | .selfOutsideImpl => "Self can only be used inside impl blocks"
  | .unknownModule name => s!"unknown module '{name}'"
  | .notPublicInModule symbol moduleName => s!"'{symbol}' is not public in module '{moduleName}'"

structure Scope where
  symbols : List (String × SymKind)

structure ResolvedModule where
  module : Module
  globalScope : Scope

-- ============================================================
-- Resolution state
-- ============================================================

structure ResolveCtx where
  globalScope : Scope
  localScopes : List (List (String × SymKind))  -- stack of local scopes
  errors : Diagnostics
  /-- Known type names (structs, enums, type aliases, traits, type params). -/
  knownTypes : List String
  /-- Current impl type name, for Self resolution. -/
  currentImplType : Option String := none
  /-- Trait name → list of method names. -/
  traitMethods : List (String × List String) := []
  /-- (typeName, traitName) pairs from trait impl blocks. -/
  traitImpls : List (String × String) := []
  /-- True when resolving inside a trait definition (Self is allowed). -/
  inTraitDef : Bool := false

/-- Result of the shallow (declaration-level) resolution phase.
    Carries everything the body phase needs plus any errors found during the shallow phase. -/
structure ShallowResult where
  globalScope : Scope
  knownTypes : List String
  traitMethods : List (String × List String)
  traitImpls : List (String × String)
  errors : Diagnostics

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

private def addError (ctx : ResolveCtx) (err : ResolveError) (span : Option Span := none) : ResolveCtx :=
  { ctx with errors := ctx.errors ++ [{ severity := .error, message := err.message, pass := "resolve", span := span, hint := none, code := err.code }] }

private def mkResolveDiag (err : ResolveError) (span : Option Span := none) : Diagnostic :=
  { severity := .error, message := err.message, pass := "resolve", span := span, hint := none, code := err.code }

private def pushScope (ctx : ResolveCtx) : ResolveCtx :=
  { ctx with localScopes := [] :: ctx.localScopes }

private def popScope (ctx : ResolveCtx) : ResolveCtx :=
  { ctx with localScopes := ctx.localScopes.drop 1 }

private def addLocal (ctx : ResolveCtx) (name : String) (kind : SymKind) : ResolveCtx :=
  match ctx.localScopes with
  | scope :: rest => { ctx with localScopes := ((name, kind) :: scope) :: rest }
  | [] => ctx  -- should not happen

private def lookupName (ctx : ResolveCtx) (name : String) : Bool :=
  -- Check local scopes (innermost first)
  ctx.localScopes.any (fun scope => scope.any fun (n, _) => n == name) ||
  -- Check global scope
  ctx.globalScope.symbols.any fun (n, _) => n == name

private def lookupSymKind (ctx : ResolveCtx) (name : String) : Option SymKind :=
  (ctx.localScopes.findSome? (fun scope => scope.find? (fun (n, _) => n == name) |>.map (·.2)))
  <|> (ctx.globalScope.symbols.find? (fun (n, _) => n == name) |>.map (·.2))

private def isKnownType (ctx : ResolveCtx) (name : String) : Bool :=
  ctx.knownTypes.contains name

-- ============================================================
-- Builtin names
-- ============================================================

-- Builtin name tables are centralized in Intrinsic.lean.
-- Resolve uses isKnownBuiltinFnFn, builtinTypeNames, etc. from there.

-- ============================================================
-- Deep type validation
-- ============================================================

/-- Recursively check that all type names in a Ty are known. -/
private def checkTyDeep (ctx : ResolveCtx) (ty : Ty) (span : Option Span := none) : ResolveCtx :=
  match ty with
  | .named name =>
    if name == selfTypeName then
      match ctx.currentImplType with
      | some _ => ctx
      | none => addError ctx .selfOutsideImpl span
    else if isKnownType ctx name then ctx
    else addError ctx (.unknownType name) span
  | .generic name args =>
    let ctx := if isKnownType ctx name then ctx
               else addError ctx (.unknownType name) span
    args.foldl (fun ctx ty => checkTyDeep ctx ty span) ctx
  | .ref inner | .refMut inner | .ptrMut inner | .ptrConst inner
  | .heap inner | .heapArray inner => checkTyDeep ctx inner span
  | .array elem _ => checkTyDeep ctx elem span
  | .fn_ params _capSet retTy =>
    let ctx := params.foldl (fun ctx ty => checkTyDeep ctx ty span) ctx
    checkTyDeep ctx retTy span
  | _ => ctx

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
  | .call sp fn _typeArgs args =>
    let ctx := if lookupName ctx fn || isKnownBuiltinFn fn then ctx
               else addError ctx (.unknownFunction fn) (some sp)
    args.foldl resolveExpr ctx
  | .paren _ inner => resolveExpr ctx inner
  | .structLit sp name _typeArgs fields =>
    let ctx := if isKnownType ctx name then ctx
               else addError ctx (.unknownStructType name) (some sp)
    fields.foldl (fun ctx (_, e) => resolveExpr ctx e) ctx
  | .fieldAccess _ obj _ => resolveExpr ctx obj
  | .enumLit sp enumName variant _typeArgs fields =>
    let ctx := match lookupSymKind ctx enumName with
      | some (.enum def_) =>
        if def_.variants.any (fun v => v.name == variant) then ctx
        else addError ctx (.unknownEnumVariant variant enumName) (some sp)
      | some _ => addError ctx (.notAnEnum enumName) (some sp)
      | none => if isKnownType ctx enumName then ctx
                else addError ctx (.unknownEnum enumName) (some sp)
    fields.foldl (fun ctx (_, e) => resolveExpr ctx e) ctx
  | .match_ _ scrutinee arms =>
    let ctx := resolveExpr ctx scrutinee
    arms.foldl (fun ctx arm =>
      match arm with
      | .mk _ _ _ bindings body =>
        let ctx := pushScope ctx
        let ctx := bindings.foldl (fun ctx b => addLocal ctx b (.var none false)) ctx
        let ctx := resolveStmts ctx body
        popScope ctx
      | .litArm _ val body =>
        let ctx := resolveExpr ctx val
        resolveStmts ctx body
      | .varArm _ binding body =>
        let ctx := pushScope ctx
        let ctx := addLocal ctx binding (.var none false)
        let ctx := resolveStmts ctx body
        popScope ctx
    ) ctx
  | .borrow _ inner | .borrowMut _ inner | .deref _ inner | .try_ _ inner =>
    resolveExpr ctx inner
  | .arrayLit _ elems => elems.foldl resolveExpr ctx
  | .arrayIndex _ arr idx => resolveExpr (resolveExpr ctx arr) idx
  | .cast _ inner _ => resolveExpr ctx inner
  | .methodCall _ obj _ _ args =>
    -- Keep skipping method name validation (needs type info to resolve receiver)
    let ctx := resolveExpr ctx obj
    args.foldl resolveExpr ctx
  | .staticMethodCall sp typeName method _ args =>
    let mangledName := s!"{typeName}_{method}"
    let ctx := if lookupName ctx mangledName then ctx
               else addError ctx (.unknownStaticMethod typeName method) (some sp)
    args.foldl resolveExpr ctx
  | .fnRef sp name =>
    if lookupName ctx name || isKnownBuiltinFn name then ctx
    else addError ctx (.unknownFunctionRef name) (some sp)
  | .arrowAccess _ obj _ => resolveExpr ctx obj
  | .allocCall _ inner allocExpr =>
    resolveExpr (resolveExpr ctx inner) allocExpr
  | .whileExpr _ cond body elseBody =>
    let ctx := resolveExpr ctx cond
    let ctx := resolveStmts ctx body
    resolveStmts ctx elseBody
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
  | .letDecl sp name _mutable ty value =>
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
  | .expr _ e => resolveExpr ctx e
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
  | .arrowAssign _ obj _ value =>
    resolveExpr (resolveExpr ctx obj) value
  | .letDestructure _ _ _ bindings value elseBody =>
    let ctx := resolveExpr ctx value
    let ctx := match elseBody with
      | some body => resolveStmts ctx body
      | none => ctx
    bindings.foldl (fun c b => addLocal c b (.var none false)) ctx
  | .letStructDestructure _ _ bindings value =>
    let ctx := resolveExpr ctx value
    bindings.foldl (fun c b => addLocal c b (.var none false)) ctx
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
      (fs.params.map fun (n, ty) => { name := n, ty := ty }) fs.retTy)

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
  let symbols := symbols ++ s.enums.foldl (fun acc e =>
    acc ++ e.variants.map fun v => (s!"{e.name}::{v.name}", SymKind.fn [] .unit)) []
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
  let symbols := symbols ++ s.submoduleSummaries.foldl (fun acc (subName, subS) =>
    acc
    ++ (subS.functions.map fun (name, fs) =>
      (s!"{subName}_{name}", SymKind.fn (fs.params.map fun (n, ty) => { name := n, ty := ty }) fs.retTy))
    ++ (subS.structs.map fun st => (st.name, SymKind.struct st))
    ++ (subS.enums.map fun e => (e.name, SymKind.enum e))
    ++ (subS.externFns.map fun ef => (s!"{subName}_{ef.name}", SymKind.externFn ef.params ef.retTy))
    ++ (subS.constants.map fun c => (c.name, SymKind.const c.ty))
    ++ (implSigsToSymbols (subS.implMethodSigs.filter fun (name, _) =>
        subS.publicNames.contains name))
  ) []
  let types := types ++ s.submoduleSummaries.foldl (fun acc (_, subS) =>
    acc
    ++ (subS.structs.map (·.name))
    ++ (subS.enums.map (·.name))
    ++ (subS.typeAliases.map (·.name))
    ++ (subS.traits.map (·.name))
  ) []
  ({ symbols := symbols }, types)

-- ============================================================
-- Resolve a module
-- ============================================================

/-- Resolve a single function body. -/
private def resolveFnBody (globalScope : Scope) (knownTypes : List String) (f : FnDef)
    (implType : Option String := none)
    (traitMethods : List (String × List String) := [])
    (traitImpls : List (String × String) := []) : Diagnostics :=
  let ctx : ResolveCtx := {
    globalScope := globalScope
    localScopes := [[]]
    errors := []
    knownTypes := knownTypes ++ f.typeParams
    currentImplType := implType
    traitMethods := traitMethods
    traitImpls := traitImpls
  }
  -- Validate parameter types and add params to scope
  let ctx := f.params.foldl (fun ctx p =>
    let ctx := addLocal ctx p.name (.var (some p.ty) false)
    checkTyDeep ctx p.ty none) ctx
  -- Validate return type
  let ctx := checkTyDeep ctx f.retTy none
  -- Walk body
  let ctx := resolveStmts ctx f.body
  ctx.errors

/-- Resolve all names in a module's function bodies. -/
private def resolveModule (m : Module) (globalScope : Scope) (knownTypes : List String)
    (traitMethods : List (String × List String))
    (traitImpls_ : List (String × String)) : ResolvedModule × Diagnostics :=
  -- Check top-level functions
  let fnErrors := m.functions.foldl (fun acc f =>
    acc ++ resolveFnBody globalScope knownTypes f none traitMethods traitImpls_) []
  -- Check impl block methods (with Self)
  let implErrors := m.implBlocks.foldl (fun acc ib =>
    acc ++ ib.methods.foldl (fun acc method =>
      acc ++ resolveFnBody globalScope (knownTypes ++ ib.typeParams) method (some ib.typeName) traitMethods traitImpls_) []) []
  -- Check trait impl methods (with Self)
  let traitImplErrors := m.traitImpls.foldl (fun acc ti =>
    acc ++ ti.methods.foldl (fun acc method =>
      acc ++ resolveFnBody globalScope (knownTypes ++ ti.typeParams) method (some ti.typeName) traitMethods traitImpls_) []) []
  let allErrors := fnErrors ++ implErrors ++ traitImplErrors
  ({ module := m, globalScope := globalScope }, allErrors)

-- ============================================================
-- Shallow (declaration-level) resolution
-- ============================================================

/-- Perform declaration-level resolution: build global scope, collect trait info,
    validate imports, and check trait impl completeness. Works entirely from FileSummary artifacts. -/
def resolveShallow (moduleSummaries : List FileSummary)
    (summaryTable : List (String × FileSummary) := []) : ShallowResult :=
  -- Build combined global scope from all module summaries
  let (combinedScope, combinedTypes) := moduleSummaries.foldl (fun (scope, types) s =>
    let (sScope, sTypes) := buildGlobalScopeFromSummary s
    ({ symbols := scope.symbols ++ sScope.symbols }, types ++ sTypes)
  ) ({ symbols := [] : Scope }, ([] : List String))
  -- For inline sibling modules (mod A {} mod B {}), register qualified names (A_fn)
  -- so that B can use A::fn() syntax (parsed as A_fn).
  let siblingQualified : List (String × SymKind) := moduleSummaries.foldl (fun acc s =>
    if s.name == "main" then acc
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
  let combinedScope : Scope := { symbols := combinedScope.symbols ++ siblingQualified }
  -- Collect trait methods and trait impls from all summaries
  let traitMethods := moduleSummaries.foldl (fun acc s =>
    acc ++ s.traits.map fun t => (t.name, t.methods.map (·.name))) []
  let traitImpls_ := moduleSummaries.foldl (fun acc s =>
    acc ++ s.traitImpls.map fun ti => (ti.typeName, ti.traitName)) []
  -- Build per-module export tables (public symbols only) from summaryTable
  let fullExportTable := summaryTable.map fun (n, s) => (n, s.publicNames)
  -- Validate imports from summaries and add imported symbols to scope
  let (importErrors, importedSymbols, importedTypes) := moduleSummaries.foldl (fun (errs, syms, tys) s =>
    s.imports.foldl (fun (errs, syms, tys) imp =>
      match fullExportTable.find? (fun (n, _) => n == imp.moduleName) with
      | none => (errs ++ [mkResolveDiag (.unknownModule imp.moduleName) (some imp.span)], syms, tys)
      | some (_, pubNames) =>
        -- Look up the full module summary to get type info for imported symbols
        let modSummary := summaryTable.find? fun (n, _) => n == imp.moduleName
        imp.symbols.foldl (fun (errs, syms, tys) sym =>
          if pubNames.contains sym.name then
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
              | none => (errs, syms, tys)
            | none => (errs, syms, tys)
          else (errs ++ [mkResolveDiag (.notPublicInModule sym.name imp.moduleName) (some imp.span)], syms, tys)
        ) (errs, syms, tys)
    ) (errs, syms, tys)
  ) (([] : Diagnostics), ([] : List (String × SymKind)), ([] : List String))
  let combinedScope := { symbols := combinedScope.symbols ++ importedSymbols }
  let combinedTypes := combinedTypes ++ importedTypes
  { globalScope := combinedScope
  , knownTypes := combinedTypes
  , traitMethods := traitMethods
  , traitImpls := traitImpls_
  , errors := importErrors }

-- ============================================================
-- Body-level resolution
-- ============================================================

/-- Resolve all module bodies using the result of the shallow phase. -/
def resolveBodies (modules : List Module) (shallow : ShallowResult) : Except Diagnostics (List ResolvedModule) :=
  let (resolved, bodyErrors) := modules.foldl (fun (acc, errs) m =>
    let (rm, mErrs) := resolveModule m shallow.globalScope shallow.knownTypes shallow.traitMethods shallow.traitImpls
    (acc ++ [rm], errs ++ mErrs)
  ) ([], [])
  let allErrors := shallow.errors ++ bodyErrors
  if hasErrors allErrors then
    .error allErrors
  else
    .ok resolved

-- ============================================================
-- Entry point
-- ============================================================

/-- Resolve all modules. Returns resolved modules or diagnostics on failure. -/
def resolveProgram (modules : List Module) (summaryTable : List (String × FileSummary) := []) : Except Diagnostics (List ResolvedModule) :=
  let moduleSummaries := modules.map fun m =>
    match summaryTable.find? fun (n, _) => n == m.name with
    | some (_, s) => s
    | none => buildFileSummary m
  let shallow := resolveShallow moduleSummaries summaryTable
  resolveBodies modules shallow

end Concrete
