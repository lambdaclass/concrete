import Concrete.Core
import Concrete.Shared
import Concrete.Layout
import Concrete.Diagnostic

namespace Concrete

/-! ## Mono — Monomorphization pass (Core→Core)

Walks all `CExpr.call` with non-empty `typeArgs`, instantiates concrete
versions of generic functions with type variables substituted, and replaces
call sites with the monomorphized name.

Runs after CoreCanonicalize, before Lower.
-/

-- ============================================================
-- Type substitution
-- ============================================================

/-- Substitute type variables in a type using a mapping.
    Handles both .typeVar and .named (parser sometimes produces .named "T" for type params). -/
private def substTy (typeParams : List String) (mapping : List (String × Ty)) : Ty → Ty
  | .typeVar n => (mapping.lookup n).getD (.typeVar n)
  | .named n => if typeParams.contains n then (mapping.lookup n).getD (.named n) else .named n
  | .ref t => .ref (substTy typeParams mapping t)
  | .refMut t => .refMut (substTy typeParams mapping t)
  | .heap t => .heap (substTy typeParams mapping t)
  | .heapArray t => .heapArray (substTy typeParams mapping t)
  | .array t n => .array (substTy typeParams mapping t) n
  | .generic name args => .generic name (args.map (substTy typeParams mapping))
  | .ptrMut t => .ptrMut (substTy typeParams mapping t)
  | .ptrConst t => .ptrConst (substTy typeParams mapping t)
  | .fn_ ps cs ret => .fn_ (ps.map (substTy typeParams mapping)) cs (substTy typeParams mapping ret)
  | t => t

-- ============================================================
-- Body-level type substitution (substitute types in CExpr/CStmt)
-- ============================================================

mutual
private partial def substExpr (sub : Ty → Ty) : CExpr → CExpr
  | .intLit v ty => .intLit v (sub ty)
  | .floatLit v ty => .floatLit v (sub ty)
  | .boolLit b => .boolLit b
  | .strLit s => .strLit s
  | .charLit c => .charLit c
  | .ident n ty => .ident n (sub ty)
  | .binOp op l r ty => .binOp op (substExpr sub l) (substExpr sub r) (sub ty)
  | .unaryOp op e ty => .unaryOp op (substExpr sub e) (sub ty)
  | .call fn targs args ty =>
    .call fn (targs.map sub) (args.map (substExpr sub)) (sub ty)
  | .structLit n targs fields ty =>
    .structLit n (targs.map sub) (fields.map fun (fn, fe) => (fn, substExpr sub fe)) (sub ty)
  | .fieldAccess obj f ty => .fieldAccess (substExpr sub obj) f (sub ty)
  | .enumLit en v targs fields ty =>
    .enumLit en v (targs.map sub) (fields.map fun (fn, fe) => (fn, substExpr sub fe)) (sub ty)
  | .match_ scrut arms ty =>
    .match_ (substExpr sub scrut) (arms.map (substArm sub)) (sub ty)
  | .borrow inner ty => .borrow (substExpr sub inner) (sub ty)
  | .borrowMut inner ty => .borrowMut (substExpr sub inner) (sub ty)
  | .deref inner ty => .deref (substExpr sub inner) (sub ty)
  | .arrayLit elems ty => .arrayLit (elems.map (substExpr sub)) (sub ty)
  | .arrayIndex arr idx ty => .arrayIndex (substExpr sub arr) (substExpr sub idx) (sub ty)
  | .cast inner t => .cast (substExpr sub inner) (sub t)
  | .fnRef n ty => .fnRef n (sub ty)
  | .try_ inner ty => .try_ (substExpr sub inner) (sub ty)
  | .allocCall inner alloc ty => .allocCall (substExpr sub inner) (substExpr sub alloc) (sub ty)
  | .whileExpr cond body elseBody ty =>
    .whileExpr (substExpr sub cond) (substStmts sub body) (substStmts sub elseBody) (sub ty)
  | .ifExpr cond then_ else_ ty =>
    .ifExpr (substExpr sub cond) (substStmts sub then_) (substStmts sub else_) (sub ty)

private partial def substArm (sub : Ty → Ty) : CMatchArm → CMatchArm
  | .enumArm en v binds body =>
    .enumArm en v (binds.map fun (n, t) => (n, sub t)) (substStmts sub body)
  | .litArm val body => .litArm (substExpr sub val) (substStmts sub body)
  | .varArm b ty body => .varArm b (sub ty) (substStmts sub body)

private partial def substStmt (sub : Ty → Ty) : CStmt → CStmt
  | .letDecl n m ty val => .letDecl n m (sub ty) (substExpr sub val)
  | .assign n val => .assign n (substExpr sub val)
  | .return_ (some v) ty => .return_ (some (substExpr sub v)) (sub ty)
  | .return_ none ty => .return_ none (sub ty)
  | .expr e => .expr (substExpr sub e)
  | .ifElse c t el =>
    .ifElse (substExpr sub c) (substStmts sub t) (el.map (substStmts sub))
  | .while_ c body lbl step => .while_ (substExpr sub c) (substStmts sub body) lbl (substStmts sub step)
  | .fieldAssign obj f val => .fieldAssign (substExpr sub obj) f (substExpr sub val)
  | .derefAssign target val => .derefAssign (substExpr sub target) (substExpr sub val)
  | .arrayIndexAssign arr idx val =>
    .arrayIndexAssign (substExpr sub arr) (substExpr sub idx) (substExpr sub val)
  | .break_ (some v) lbl => .break_ (some (substExpr sub v)) lbl
  | .break_ none lbl => .break_ none lbl
  | .continue_ lbl => .continue_ lbl
  | .defer body => .defer (substExpr sub body)
  | .borrowIn v r reg isMut ty body =>
    .borrowIn v r reg isMut (sub ty) (substStmts sub body)

private partial def substStmts (sub : Ty → Ty) : List CStmt → List CStmt :=
  List.map (substStmt sub)
end

-- ============================================================
-- Rewrite trait-method call names after type substitution
-- ============================================================

-- Given a mapping from type-parameter names to concrete types, rewrite
-- function names like "T_method" to "Point_method" in all call nodes.
-- This is needed because the elaborator emits `TypeVar_method` for
-- trait method calls on generic type parameters.
mutual
private partial def rewriteCallNames (nameMap : List (String × String)) : CExpr → CExpr
  | .call fn targs args ty =>
    let fn' := nameMap.foldl (fun acc (paramName, concreteName) =>
      let pfx := paramName ++ "_"
      if acc.startsWith pfx then concreteName ++ "_" ++ acc.drop pfx.length
      else acc
    ) fn
    .call fn' targs (args.map (rewriteCallNames nameMap)) ty
  | .binOp op l r ty => .binOp op (rewriteCallNames nameMap l) (rewriteCallNames nameMap r) ty
  | .unaryOp op e ty => .unaryOp op (rewriteCallNames nameMap e) ty
  | .structLit n ta fs ty => .structLit n ta (fs.map fun (n, e) => (n, rewriteCallNames nameMap e)) ty
  | .fieldAccess obj f ty => .fieldAccess (rewriteCallNames nameMap obj) f ty
  | .enumLit en v ta fs ty => .enumLit en v ta (fs.map fun (n, e) => (n, rewriteCallNames nameMap e)) ty
  | .match_ scrut arms ty => .match_ (rewriteCallNames nameMap scrut) (arms.map (rewriteCallNamesArm nameMap)) ty
  | .borrow inner ty => .borrow (rewriteCallNames nameMap inner) ty
  | .borrowMut inner ty => .borrowMut (rewriteCallNames nameMap inner) ty
  | .deref inner ty => .deref (rewriteCallNames nameMap inner) ty
  | .arrayLit elems ty => .arrayLit (elems.map (rewriteCallNames nameMap)) ty
  | .arrayIndex arr idx ty => .arrayIndex (rewriteCallNames nameMap arr) (rewriteCallNames nameMap idx) ty
  | .cast inner t => .cast (rewriteCallNames nameMap inner) t
  | .try_ inner ty => .try_ (rewriteCallNames nameMap inner) ty
  | .allocCall inner alloc ty => .allocCall (rewriteCallNames nameMap inner) (rewriteCallNames nameMap alloc) ty
  | .whileExpr cond body elseBody ty =>
    .whileExpr (rewriteCallNames nameMap cond) (rewriteCallNamesStmts nameMap body) (rewriteCallNamesStmts nameMap elseBody) ty
  | .ifExpr cond then_ else_ ty =>
    .ifExpr (rewriteCallNames nameMap cond) (rewriteCallNamesStmts nameMap then_) (rewriteCallNamesStmts nameMap else_) ty
  | e => e

private partial def rewriteCallNamesArm (nameMap : List (String × String)) : CMatchArm → CMatchArm
  | .enumArm en v binds body => .enumArm en v binds (rewriteCallNamesStmts nameMap body)
  | .litArm val body => .litArm (rewriteCallNames nameMap val) (rewriteCallNamesStmts nameMap body)
  | .varArm b ty body => .varArm b ty (rewriteCallNamesStmts nameMap body)

private partial def rewriteCallNamesStmt (nameMap : List (String × String)) : CStmt → CStmt
  | .letDecl n m ty val => .letDecl n m ty (rewriteCallNames nameMap val)
  | .assign n val => .assign n (rewriteCallNames nameMap val)
  | .return_ (some v) ty => .return_ (some (rewriteCallNames nameMap v)) ty
  | .expr e => .expr (rewriteCallNames nameMap e)
  | .ifElse c t el =>
    .ifElse (rewriteCallNames nameMap c) (rewriteCallNamesStmts nameMap t) (el.map (rewriteCallNamesStmts nameMap))
  | .while_ c body lbl step =>
    .while_ (rewriteCallNames nameMap c) (rewriteCallNamesStmts nameMap body) lbl (rewriteCallNamesStmts nameMap step)
  | .fieldAssign obj f val => .fieldAssign (rewriteCallNames nameMap obj) f (rewriteCallNames nameMap val)
  | .derefAssign target val => .derefAssign (rewriteCallNames nameMap target) (rewriteCallNames nameMap val)
  | .arrayIndexAssign arr idx val =>
    .arrayIndexAssign (rewriteCallNames nameMap arr) (rewriteCallNames nameMap idx) (rewriteCallNames nameMap val)
  | .break_ (some v) lbl => .break_ (some (rewriteCallNames nameMap v)) lbl
  | .defer body => .defer (rewriteCallNames nameMap body)
  | .borrowIn v r reg isMut ty body =>
    .borrowIn v r reg isMut ty (rewriteCallNamesStmts nameMap body)
  | s => s

private partial def rewriteCallNamesStmts (nameMap : List (String × String)) : List CStmt → List CStmt :=
  List.map (rewriteCallNamesStmt nameMap)
end

mutual
/-- Given a set of known generic function names and type args to inject,
    walk the body and add typeArgs to any call targeting these functions
    that currently has empty typeArgs. -/
partial def injectTypeArgsExpr (genericNames : List String) (typeArgs : List Ty) : CExpr → CExpr
  | .call fn [] args ty =>
    let args' := args.map (injectTypeArgsExpr genericNames typeArgs)
    if genericNames.contains fn then .call fn typeArgs args' ty
    else .call fn [] args' ty
  | .call fn ta args ty => .call fn ta (args.map (injectTypeArgsExpr genericNames typeArgs)) ty
  | .binOp op l r ty => .binOp op (injectTypeArgsExpr genericNames typeArgs l) (injectTypeArgsExpr genericNames typeArgs r) ty
  | .unaryOp op inner ty => .unaryOp op (injectTypeArgsExpr genericNames typeArgs inner) ty
  | .structLit n ta fields ty => .structLit n ta (fields.map fun (n, e) => (n, injectTypeArgsExpr genericNames typeArgs e)) ty
  | .fieldAccess obj f ty => .fieldAccess (injectTypeArgsExpr genericNames typeArgs obj) f ty
  | .enumLit en v ta fields ty => .enumLit en v ta (fields.map fun (n, e) => (n, injectTypeArgsExpr genericNames typeArgs e)) ty
  | .match_ scrut arms ty => .match_ (injectTypeArgsExpr genericNames typeArgs scrut) (arms.map (injectTypeArgsArm genericNames typeArgs)) ty
  | .borrow inner ty => .borrow (injectTypeArgsExpr genericNames typeArgs inner) ty
  | .borrowMut inner ty => .borrowMut (injectTypeArgsExpr genericNames typeArgs inner) ty
  | .deref inner ty => .deref (injectTypeArgsExpr genericNames typeArgs inner) ty
  | .arrayLit elems ty => .arrayLit (elems.map (injectTypeArgsExpr genericNames typeArgs)) ty
  | .arrayIndex arr idx ty => .arrayIndex (injectTypeArgsExpr genericNames typeArgs arr) (injectTypeArgsExpr genericNames typeArgs idx) ty
  | .cast inner t => .cast (injectTypeArgsExpr genericNames typeArgs inner) t
  | .try_ inner ty => .try_ (injectTypeArgsExpr genericNames typeArgs inner) ty
  | .allocCall inner alloc ty => .allocCall (injectTypeArgsExpr genericNames typeArgs inner) (injectTypeArgsExpr genericNames typeArgs alloc) ty
  | .whileExpr cond body elseBody ty =>
    .whileExpr (injectTypeArgsExpr genericNames typeArgs cond) (injectTypeArgsStmts genericNames typeArgs body) (injectTypeArgsStmts genericNames typeArgs elseBody) ty
  | .ifExpr cond then_ else_ ty =>
    .ifExpr (injectTypeArgsExpr genericNames typeArgs cond) (injectTypeArgsStmts genericNames typeArgs then_) (injectTypeArgsStmts genericNames typeArgs else_) ty
  | e => e

partial def injectTypeArgsArm (genericNames : List String) (typeArgs : List Ty) : CMatchArm → CMatchArm
  | .enumArm en v binds body => .enumArm en v binds (injectTypeArgsStmts genericNames typeArgs body)
  | .litArm val body => .litArm (injectTypeArgsExpr genericNames typeArgs val) (injectTypeArgsStmts genericNames typeArgs body)
  | .varArm b ty body => .varArm b ty (injectTypeArgsStmts genericNames typeArgs body)

partial def injectTypeArgsStmt (genericNames : List String) (typeArgs : List Ty) : CStmt → CStmt
  | .letDecl n m ty val => .letDecl n m ty (injectTypeArgsExpr genericNames typeArgs val)
  | .assign n val => .assign n (injectTypeArgsExpr genericNames typeArgs val)
  | .return_ (some v) ty => .return_ (some (injectTypeArgsExpr genericNames typeArgs v)) ty
  | .expr e => .expr (injectTypeArgsExpr genericNames typeArgs e)
  | .ifElse c t el =>
    .ifElse (injectTypeArgsExpr genericNames typeArgs c) (injectTypeArgsStmts genericNames typeArgs t) (el.map (injectTypeArgsStmts genericNames typeArgs))
  | .while_ c body lbl step =>
    .while_ (injectTypeArgsExpr genericNames typeArgs c) (injectTypeArgsStmts genericNames typeArgs body) lbl (injectTypeArgsStmts genericNames typeArgs step)
  | .fieldAssign obj f val => .fieldAssign (injectTypeArgsExpr genericNames typeArgs obj) f (injectTypeArgsExpr genericNames typeArgs val)
  | .derefAssign target val => .derefAssign (injectTypeArgsExpr genericNames typeArgs target) (injectTypeArgsExpr genericNames typeArgs val)
  | .break_ (some v) lbl => .break_ (some (injectTypeArgsExpr genericNames typeArgs v)) lbl
  | .defer body => .defer (injectTypeArgsExpr genericNames typeArgs body)
  | .borrowIn v r reg isMut ty body =>
    .borrowIn v r reg isMut ty (injectTypeArgsStmts genericNames typeArgs body)
  | s => s

partial def injectTypeArgsStmts (genericNames : List String) (typeArgs : List Ty) : List CStmt → List CStmt :=
  List.map (injectTypeArgsStmt genericNames typeArgs)
end

-- ============================================================
-- Monomorphized name computation
-- ============================================================

/-- Produce a human-readable type suffix for mono name. -/
private def tyToSuffix : Ty → String
  | .int => "Int"
  | .uint => "Uint"
  | .i8 => "i8"
  | .i16 => "i16"
  | .i32 => "i32"
  | .u8 => "u8"
  | .u16 => "u16"
  | .u32 => "u32"
  | .bool => "Bool"
  | .float64 => "Float64"
  | .float32 => "Float32"
  | .char => "Char"
  | .string => "String"
  | .named n => n
  | .generic n _ => n
  | .heap t => "Heap_" ++ tyToSuffix t
  | .heapArray t => "HeapArray_" ++ tyToSuffix t
  | _ => "unknown"

/-- Compute monomorphized function name: `fnName_for_T1_T2`. -/
private def monoNameFor (fnName : String) (typeArgs : List Ty) : String :=
  fnName ++ "_for_" ++ "_".intercalate (typeArgs.map tyToSuffix)

/-- Infer type arguments by matching formal parameter types against concrete argument types.
    For a generic fn like `push(self: &mut BinaryHeap<T>, value: T)` called with
    concrete args of types `(&mut BinaryHeap<i32>, i32)`, infer `T = i32`. -/
private partial def cexprTy (e : CExpr) : Ty := match e with
  | .intLit _ ty | .floatLit _ ty | .ident _ ty | .binOp _ _ _ ty
  | .unaryOp _ _ ty | .call _ _ _ ty | .structLit _ _ _ ty
  | .fieldAccess _ _ ty | .enumLit _ _ _ _ ty | .match_ _ _ ty
  | .borrow _ ty | .borrowMut _ ty | .deref _ ty | .arrayLit _ ty
  | .arrayIndex _ _ ty | .fnRef _ ty | .try_ _ ty
  | .allocCall _ _ ty | .whileExpr _ _ _ ty | .ifExpr _ _ _ ty => ty
  | .cast _ t => t
  | .boolLit _ => .bool
  | .strLit _ => .string
  | .charLit _ => .char

private partial def matchFormalActual (typeParams : List String) (formal : Ty) (actual : Ty) (acc : List (String × Ty)) : List (String × Ty) :=
  match formal with
  | .typeVar n =>
    if typeParams.contains n && !acc.any (·.1 == n) then acc ++ [(n, actual)]
    else acc
  | .named n =>
    if typeParams.contains n && !acc.any (·.1 == n) then acc ++ [(n, actual)]
    else acc
  | .ref f => match actual with
    | .ref a | .ptrConst a => matchFormalActual typeParams f a acc
    | _ => acc
  | .refMut f => match actual with
    | .refMut a | .ptrMut a => matchFormalActual typeParams f a acc
    | _ => acc
  | .ptrMut f => match actual with
    | .ptrMut a | .refMut a => matchFormalActual typeParams f a acc
    | _ => acc
  | .ptrConst f => match actual with
    | .ptrConst a | .ref a => matchFormalActual typeParams f a acc
    | _ => acc
  | .generic _ fArgs => match actual with
    | .generic _ aArgs =>
      fArgs.zip aArgs |>.foldl (fun acc (f, a) => matchFormalActual typeParams f a acc) acc
    | _ => acc
  | _ => acc

private def inferTypeArgs (typeParams : List String) (formalParams : List (String × Ty))
    (args : List CExpr) : List Ty :=
  -- Build a mapping from type param name → concrete type by matching formal/actual
  let mapping := formalParams.zip args |>.foldl (fun (acc : List (String × Ty)) ((_, formalTy), argExpr) =>
    let argTy := cexprTy argExpr
    matchFormalActual typeParams formalTy argTy acc
  ) []
  -- Return the type args in order of typeParams
  typeParams.filterMap fun p => mapping.lookup p

-- ============================================================
-- Monomorphization state
-- ============================================================

structure MonoState where
  /-- All original function definitions (for lookup). -/
  allFns : List CFnDef
  /-- Linker aliases from all modules (local name → prefixed definition name). -/
  linkerAliases : List (String × String) := []
  /-- Queue of monomorphized functions to process. -/
  queue : List (String × CFnDef) := []
  /-- Already-generated mono names (avoid duplicates). -/
  generated : List String := []

abbrev MonoM := ExceptT Diagnostics (StateM MonoState)

private def lookupFn (name : String) : MonoM (Option CFnDef) := do
  let st ← get
  -- Try direct lookup first
  match st.allFns.find? fun f => f.name == name with
  | some f => return some f
  | none =>
    -- Try resolving through linker aliases (e.g., HashMap_contains → map_HashMap_contains)
    match st.linkerAliases.lookup name with
    | some resolvedName => return st.allFns.find? fun f => f.name == resolvedName
    | none => return none

/-- Get names of all generic functions (non-empty typeParams). -/
private def getGenericFnNames : MonoM (List String) := do
  let st ← get
  return st.allFns.filter (fun f => !f.typeParams.isEmpty) |>.map (·.name)

private def enqueueMono (monoName : String) (monoFn : CFnDef) : MonoM Unit := do
  let st ← get
  if st.generated.contains monoName then return
  set { st with
    queue := st.queue ++ [(monoName, monoFn)]
    generated := monoName :: st.generated }

-- ============================================================
-- Core expression/statement rewriting
-- ============================================================

mutual
partial def monoExpr (e : CExpr) : MonoM CExpr := do
  match e with
  | .call fn typeArgs args ty =>
    let args' ← args.mapM monoExpr
    if typeArgs.isEmpty then
      -- Even with no explicit typeArgs, the callee might be generic.
      -- Check if it's a generic function we need to monomorphize.
      let fnDef? ← lookupFn fn
      match fnDef? with
      | some fnDef =>
        if fnDef.typeParams.isEmpty then
          return .call fn [] args' ty  -- truly non-generic
        else
          -- Generic function called without type args (e.g., sibling method call).
          -- Try to infer type args from concrete argument types.
          let inferredArgs := inferTypeArgs fnDef.typeParams fnDef.params args'
          if inferredArgs.isEmpty then
            return .call fn [] args' ty  -- couldn't infer, leave as-is
          else
            -- Re-process as a generic call with inferred type args
            let genericNames ← getGenericFnNames
            let name := monoNameFor fn inferredArgs
            let mapping := fnDef.typeParams.zip inferredArgs
            let sub := substTy fnDef.typeParams mapping
            let callNameMap := mapping.filterMap fun (paramName, ty) =>
              let concreteName := tyName ty
              if concreteName == "" then none else some (paramName, concreteName)
            -- Inject type args into calls to sibling generic functions before type subst
            let bodyWithTypeArgs := injectTypeArgsStmts genericNames inferredArgs fnDef.body
            let monoFn : CFnDef := {
              name := name
              typeParams := []
              params := fnDef.params.map fun (n, t) => (n, sub t)
              retTy := sub fnDef.retTy
              body := rewriteCallNamesStmts callNameMap (substStmts sub bodyWithTypeArgs)
              isPublic := false
              isTest := false
              capSet := fnDef.capSet
            }
            enqueueMono name monoFn
            return .call name [] args' (sub ty)
      | none => return .call fn [] args' ty
    -- Look up the generic function
    let fnDef? ← lookupFn fn
    match fnDef? with
    | none => return .call fn typeArgs args' ty  -- extern or unknown, leave as-is
    | some fnDef =>
      if fnDef.typeParams.isEmpty then
        return .call fn typeArgs args' ty  -- not actually generic
      let genericNames ← getGenericFnNames
      let name := monoNameFor fn typeArgs
      let mapping := fnDef.typeParams.zip typeArgs
      let sub := substTy fnDef.typeParams mapping
      -- Build a name map for rewriting trait method calls like T_describe → Point_describe
      let callNameMap := mapping.filterMap fun (paramName, ty) =>
        let concreteName := tyName ty
        if concreteName == "" then none else some (paramName, concreteName)
      -- Inject type args into calls to sibling generic functions before type subst
      let bodyWithTypeArgs := injectTypeArgsStmts genericNames typeArgs fnDef.body
      let monoFn : CFnDef := {
        name := name
        typeParams := []
        params := fnDef.params.map fun (n, t) => (n, sub t)
        retTy := sub fnDef.retTy
        body := rewriteCallNamesStmts callNameMap (substStmts sub bodyWithTypeArgs)
        isPublic := false
        isTest := false
        capSet := fnDef.capSet
      }
      enqueueMono name monoFn
      return .call name [] args' (sub ty)
  | .intLit _ _ | .floatLit _ _ | .boolLit _ | .strLit _ | .charLit _ => return e
  | .ident n ty => return .ident n ty
  | .binOp op l r ty => return .binOp op (← monoExpr l) (← monoExpr r) ty
  | .unaryOp op inner ty => return .unaryOp op (← monoExpr inner) ty
  | .structLit n targs fields ty =>
    let fields' ← fields.mapM fun (n, e) => return (n, ← monoExpr e)
    return .structLit n targs fields' ty
  | .fieldAccess obj f ty => return .fieldAccess (← monoExpr obj) f ty
  | .enumLit en v targs fields ty =>
    let fields' ← fields.mapM fun (n, e) => return (n, ← monoExpr e)
    return .enumLit en v targs fields' ty
  | .match_ scrut arms ty =>
    let scrut' ← monoExpr scrut
    let arms' ← arms.mapM monoArm
    return .match_ scrut' arms' ty
  | .borrow inner ty => return .borrow (← monoExpr inner) ty
  | .borrowMut inner ty => return .borrowMut (← monoExpr inner) ty
  | .deref inner ty => return .deref (← monoExpr inner) ty
  | .arrayLit elems ty =>
    let elems' ← elems.mapM monoExpr
    return .arrayLit elems' ty
  | .arrayIndex arr idx ty => return .arrayIndex (← monoExpr arr) (← monoExpr idx) ty
  | .cast inner t => return .cast (← monoExpr inner) t
  | .fnRef n ty => return .fnRef n ty
  | .try_ inner ty => return .try_ (← monoExpr inner) ty
  | .allocCall inner alloc ty => return .allocCall (← monoExpr inner) (← monoExpr alloc) ty
  | .whileExpr cond body elseBody ty =>
    return .whileExpr (← monoExpr cond) (← monoStmts body) (← monoStmts elseBody) ty
  | .ifExpr cond then_ else_ ty =>
    return .ifExpr (← monoExpr cond) (← monoStmts then_) (← monoStmts else_) ty

partial def monoArm (arm : CMatchArm) : MonoM CMatchArm := do
  match arm with
  | .enumArm en v binds body => return .enumArm en v binds (← monoStmts body)
  | .litArm val body => return .litArm (← monoExpr val) (← monoStmts body)
  | .varArm b ty body => return .varArm b ty (← monoStmts body)

partial def monoStmt (s : CStmt) : MonoM CStmt := do
  match s with
  | .letDecl n m ty val => return .letDecl n m ty (← monoExpr val)
  | .assign n val => return .assign n (← monoExpr val)
  | .return_ (some v) ty => return .return_ (some (← monoExpr v)) ty
  | .return_ none ty => return .return_ none ty
  | .expr e => return .expr (← monoExpr e)
  | .ifElse c t el =>
    let el' ← match el with
      | none => pure none
      | some stmts => do pure (some (← monoStmts stmts))
    return .ifElse (← monoExpr c) (← monoStmts t) el'
  | .while_ c body lbl step => return .while_ (← monoExpr c) (← monoStmts body) lbl (← monoStmts step)
  | .fieldAssign obj f val => return .fieldAssign (← monoExpr obj) f (← monoExpr val)
  | .derefAssign target val => return .derefAssign (← monoExpr target) (← monoExpr val)
  | .arrayIndexAssign arr idx val =>
    return .arrayIndexAssign (← monoExpr arr) (← monoExpr idx) (← monoExpr val)
  | .break_ (some v) lbl => return .break_ (some (← monoExpr v)) lbl
  | .break_ none lbl => return .break_ none lbl
  | .continue_ lbl => return .continue_ lbl
  | .defer body => return .defer (← monoExpr body)
  | .borrowIn v r reg isMut ty body =>
    return .borrowIn v r reg isMut ty (← monoStmts body)

partial def monoStmts (stmts : List CStmt) : MonoM (List CStmt) :=
  stmts.mapM monoStmt
end

-- ============================================================
-- Function and module monomorphization
-- ============================================================

private def monoFn (f : CFnDef) : MonoM CFnDef := do
  -- Skip generic functions; they are templates, not concrete code.
  -- Only their monomorphized specializations should be processed.
  if !f.typeParams.isEmpty then return f
  let body' ← monoStmts f.body
  return { f with body := body' }

/-- Process the mono queue until empty. Each mono'd function may enqueue more. -/
private partial def drainQueue : MonoM (List CFnDef) := do
  let st ← get
  if st.queue.isEmpty then return []
  -- Take the current queue and clear it
  let batch := st.queue
  set { st with queue := [] }
  let mut result : List CFnDef := []
  for (_, fn) in batch do
    let fn' ← monoFn fn
    result := result ++ [fn']
  -- Recurse to handle any new entries added during processing
  let rest ← drainQueue
  return result ++ rest

partial def monoModule (m : CModule) : MonoM CModule := do
  -- Mono all existing functions
  let fns' ← m.functions.mapM monoFn
  -- Drain mono queue to get all generated specializations
  let monoFns ← drainQueue
  -- Recursively process submodules
  let subs' ← m.submodules.mapM monoModule
  let subMonoFns ← drainQueue
  return { m with functions := fns' ++ monoFns ++ subMonoFns, submodules := subs' }

/-- Recursively collect all functions from a module and its submodules. -/
private partial def collectAllModuleFns (m : CModule) : List CFnDef :=
  let own := m.functions
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllModuleFns s) []
  own ++ sub

private partial def collectAllModuleAliases (m : CModule) : List (String × String) :=
  let own := m.linkerAliases
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllModuleAliases s) []
  own ++ sub

-- ============================================================
-- Post-mono struct monomorphization
-- ============================================================

/-- Builtin generic type names that have hardcoded layouts and should NOT be
    monomorphized into concrete struct defs. -/
private def builtinGenericNames : List String :=
  ["Vec", "HashMap", "HashSet", "BinaryHeap", "Heap", "HeapArray"]

/-- Compute a mangled struct name for a generic struct instantiation.
    E.g., `Box` with `[.i32]` → `"Box_i32"`, `Box` with `[.int]` → `"Box_Int"`. -/
private def monoStructName (baseName : String) (args : List Ty) : String :=
  baseName ++ "_" ++ "_".intercalate (args.map tyToSuffix)

/-- Collect all generic struct names (those with non-empty typeParams) from all modules. -/
private partial def collectGenericStructNames (modules : List CModule) : List String :=
  let rec go (m : CModule) : List String :=
    let own := m.structs.filter (fun sd => !sd.typeParams.isEmpty) |>.map (·.name)
    let sub := m.submodules.foldl (fun acc s => acc ++ go s) []
    own ++ sub
  modules.foldl (fun acc m => acc ++ go m) []

/-- Collect all unique (name, args) pairs of generic struct instantiations from a Ty. -/
private partial def collectGenericTyInstances (genericNames : List String) : Ty → List (String × List Ty)
  | .generic name args =>
    let self := if genericNames.contains name && !builtinGenericNames.contains name
                then [(name, args)]
                else []
    let inner := args.foldl (fun acc a => acc ++ collectGenericTyInstances genericNames a) []
    self ++ inner
  | .ref t | .refMut t | .ptrMut t | .ptrConst t | .heap t | .heapArray t =>
    collectGenericTyInstances genericNames t
  | .array t _ => collectGenericTyInstances genericNames t
  | .fn_ ps _ ret =>
    ps.foldl (fun acc p => acc ++ collectGenericTyInstances genericNames p) [] ++
    collectGenericTyInstances genericNames ret
  | _ => []

mutual
/-- Collect generic struct instances from a CExpr. -/
private partial def collectExprInstances (gn : List String) : CExpr → List (String × List Ty)
  | .intLit _ ty | .floatLit _ ty | .ident _ ty | .binOp _ _ _ ty
  | .unaryOp _ _ ty | .fnRef _ ty | .try_ _ ty | .allocCall _ _ ty => collectGenericTyInstances gn ty
  | .boolLit _ | .strLit _ | .charLit _ => []
  | .call _ targs args ty =>
    targs.foldl (fun acc t => acc ++ collectGenericTyInstances gn t) [] ++
    args.foldl (fun acc a => acc ++ collectExprInstances gn a) [] ++
    collectGenericTyInstances gn ty
  | .structLit _ targs fields ty =>
    targs.foldl (fun acc t => acc ++ collectGenericTyInstances gn t) [] ++
    fields.foldl (fun acc (_, e) => acc ++ collectExprInstances gn e) [] ++
    collectGenericTyInstances gn ty
  | .fieldAccess obj _ ty => collectExprInstances gn obj ++ collectGenericTyInstances gn ty
  | .enumLit _ _ targs fields ty =>
    targs.foldl (fun acc t => acc ++ collectGenericTyInstances gn t) [] ++
    fields.foldl (fun acc (_, e) => acc ++ collectExprInstances gn e) [] ++
    collectGenericTyInstances gn ty
  | .match_ scrut arms ty =>
    collectExprInstances gn scrut ++
    arms.foldl (fun acc a => acc ++ collectArmInstances gn a) [] ++
    collectGenericTyInstances gn ty
  | .borrow inner ty | .borrowMut inner ty | .deref inner ty =>
    collectExprInstances gn inner ++ collectGenericTyInstances gn ty
  | .arrayLit elems ty =>
    elems.foldl (fun acc e => acc ++ collectExprInstances gn e) [] ++ collectGenericTyInstances gn ty
  | .arrayIndex arr idx ty =>
    collectExprInstances gn arr ++ collectExprInstances gn idx ++ collectGenericTyInstances gn ty
  | .cast inner t => collectExprInstances gn inner ++ collectGenericTyInstances gn t
  | .whileExpr cond body elseBody ty =>
    collectExprInstances gn cond ++ collectStmtsInstances gn body ++
    collectStmtsInstances gn elseBody ++ collectGenericTyInstances gn ty
  | .ifExpr cond then_ else_ ty =>
    collectExprInstances gn cond ++ collectStmtsInstances gn then_ ++
    collectStmtsInstances gn else_ ++ collectGenericTyInstances gn ty

private partial def collectArmInstances (gn : List String) : CMatchArm → List (String × List Ty)
  | .enumArm _ _ binds body =>
    binds.foldl (fun acc (_, t) => acc ++ collectGenericTyInstances gn t) [] ++
    collectStmtsInstances gn body
  | .litArm val body => collectExprInstances gn val ++ collectStmtsInstances gn body
  | .varArm _ ty body => collectGenericTyInstances gn ty ++ collectStmtsInstances gn body

private partial def collectStmtInstances (gn : List String) : CStmt → List (String × List Ty)
  | .letDecl _ _ ty val => collectGenericTyInstances gn ty ++ collectExprInstances gn val
  | .assign _ val => collectExprInstances gn val
  | .return_ (some v) ty => collectExprInstances gn v ++ collectGenericTyInstances gn ty
  | .return_ none ty => collectGenericTyInstances gn ty
  | .expr e => collectExprInstances gn e
  | .ifElse c t el =>
    collectExprInstances gn c ++ collectStmtsInstances gn t ++
    match el with | some s => collectStmtsInstances gn s | none => []
  | .while_ c body _ step =>
    collectExprInstances gn c ++ collectStmtsInstances gn body ++ collectStmtsInstances gn step
  | .fieldAssign obj _ val => collectExprInstances gn obj ++ collectExprInstances gn val
  | .derefAssign target val => collectExprInstances gn target ++ collectExprInstances gn val
  | .arrayIndexAssign arr idx val =>
    collectExprInstances gn arr ++ collectExprInstances gn idx ++ collectExprInstances gn val
  | .break_ (some v) _ => collectExprInstances gn v
  | .break_ none _ | .continue_ _ => []
  | .defer body => collectExprInstances gn body
  | .borrowIn _ _ _ _ ty body =>
    collectGenericTyInstances gn ty ++ collectStmtsInstances gn body

private partial def collectStmtsInstances (gn : List String) : List CStmt → List (String × List Ty) :=
  fun stmts => stmts.foldl (fun acc s => acc ++ collectStmtInstances gn s) []
end

/-- Collect all generic struct instances from a function.
    Only collects from non-generic (concrete) functions — generic templates
    still have unsubstituted type variables and should be ignored. -/
private def collectFnInstances (gn : List String) (f : CFnDef) : List (String × List Ty) :=
  -- Skip generic template functions (they have unsubstituted type vars)
  if !f.typeParams.isEmpty then []
  else
  let paramInsts := f.params.foldl (fun acc (_, t) => acc ++ collectGenericTyInstances gn t) []
  let retInst := collectGenericTyInstances gn f.retTy
  let bodyInsts := collectStmtsInstances gn f.body
  paramInsts ++ retInst ++ bodyInsts

/-- Check if a type contains any unresolved type variables. -/
private partial def hasTypeVar : Ty → Bool
  | .typeVar _ => true
  | .named _ => false  -- named types are concrete
  | .generic _ args => args.any hasTypeVar
  | .ref t | .refMut t | .ptrMut t | .ptrConst t | .heap t | .heapArray t => hasTypeVar t
  | .array t _ => hasTypeVar t
  | .fn_ ps _ ret => ps.any hasTypeVar || hasTypeVar ret
  | _ => false

/-- Deduplicate instances by (name, args) equality, filtering out any with type variables. -/
private def dedupInstances (insts : List (String × List Ty)) : List (String × List Ty) :=
  insts.foldl (fun acc (n, args) =>
    if args.any hasTypeVar then acc  -- skip instances with unresolved type vars
    else if acc.any (fun (n2, args2) => n == n2 && args == args2) then acc
    else acc ++ [(n, args)]) []

/-- Rewrite a Ty, replacing generic struct references with named monomorphized types.
    `mapping` is a list of (baseName, args, mangledName). -/
private partial def rewriteTy (mapping : List (String × List Ty × String)) : Ty → Ty
  | .generic name args =>
    match mapping.find? (fun (n, a, _) => n == name && a == args) with
    | some (_, _, mangledName) => .named mangledName
    | none => .generic name (args.map (rewriteTy mapping))
  | .ref t => .ref (rewriteTy mapping t)
  | .refMut t => .refMut (rewriteTy mapping t)
  | .ptrMut t => .ptrMut (rewriteTy mapping t)
  | .ptrConst t => .ptrConst (rewriteTy mapping t)
  | .heap t => .heap (rewriteTy mapping t)
  | .heapArray t => .heapArray (rewriteTy mapping t)
  | .array t n => .array (rewriteTy mapping t) n
  | .fn_ ps cs ret => .fn_ (ps.map (rewriteTy mapping)) cs (rewriteTy mapping ret)
  | t => t

mutual
/-- Rewrite all types in a CExpr. -/
private partial def rewriteExprTys (m : List (String × List Ty × String)) : CExpr → CExpr
  | .intLit v ty => .intLit v (rewriteTy m ty)
  | .floatLit v ty => .floatLit v (rewriteTy m ty)
  | .boolLit b => .boolLit b
  | .strLit s => .strLit s
  | .charLit c => .charLit c
  | .ident n ty => .ident n (rewriteTy m ty)
  | .binOp op l r ty => .binOp op (rewriteExprTys m l) (rewriteExprTys m r) (rewriteTy m ty)
  | .unaryOp op e ty => .unaryOp op (rewriteExprTys m e) (rewriteTy m ty)
  | .call fn targs args ty =>
    .call fn (targs.map (rewriteTy m)) (args.map (rewriteExprTys m)) (rewriteTy m ty)
  | .structLit name targs fields ty =>
    -- If this struct lit targets a generic struct being monomorphized, update the name
    let ty' := rewriteTy m ty
    let name' := match ty' with
      | .named n => n   -- rewritten to mangled name
      | _ => name
    .structLit name' (targs.map (rewriteTy m)) (fields.map fun (n, e) => (n, rewriteExprTys m e)) ty'
  | .fieldAccess obj f ty => .fieldAccess (rewriteExprTys m obj) f (rewriteTy m ty)
  | .enumLit en v targs fields ty =>
    .enumLit en v (targs.map (rewriteTy m)) (fields.map fun (n, e) => (n, rewriteExprTys m e)) (rewriteTy m ty)
  | .match_ scrut arms ty =>
    .match_ (rewriteExprTys m scrut) (arms.map (rewriteArmTys m)) (rewriteTy m ty)
  | .borrow inner ty => .borrow (rewriteExprTys m inner) (rewriteTy m ty)
  | .borrowMut inner ty => .borrowMut (rewriteExprTys m inner) (rewriteTy m ty)
  | .deref inner ty => .deref (rewriteExprTys m inner) (rewriteTy m ty)
  | .arrayLit elems ty => .arrayLit (elems.map (rewriteExprTys m)) (rewriteTy m ty)
  | .arrayIndex arr idx ty => .arrayIndex (rewriteExprTys m arr) (rewriteExprTys m idx) (rewriteTy m ty)
  | .cast inner t => .cast (rewriteExprTys m inner) (rewriteTy m t)
  | .fnRef n ty => .fnRef n (rewriteTy m ty)
  | .try_ inner ty => .try_ (rewriteExprTys m inner) (rewriteTy m ty)
  | .allocCall inner alloc ty => .allocCall (rewriteExprTys m inner) (rewriteExprTys m alloc) (rewriteTy m ty)
  | .whileExpr cond body elseBody ty =>
    .whileExpr (rewriteExprTys m cond) (rewriteStmtsTys m body) (rewriteStmtsTys m elseBody) (rewriteTy m ty)
  | .ifExpr cond then_ else_ ty =>
    .ifExpr (rewriteExprTys m cond) (rewriteStmtsTys m then_) (rewriteStmtsTys m else_) (rewriteTy m ty)

private partial def rewriteArmTys (m : List (String × List Ty × String)) : CMatchArm → CMatchArm
  | .enumArm en v binds body =>
    .enumArm en v (binds.map fun (n, t) => (n, rewriteTy m t)) (rewriteStmtsTys m body)
  | .litArm val body => .litArm (rewriteExprTys m val) (rewriteStmtsTys m body)
  | .varArm b ty body => .varArm b (rewriteTy m ty) (rewriteStmtsTys m body)

private partial def rewriteStmtTys (m : List (String × List Ty × String)) : CStmt → CStmt
  | .letDecl n mu ty val => .letDecl n mu (rewriteTy m ty) (rewriteExprTys m val)
  | .assign n val => .assign n (rewriteExprTys m val)
  | .return_ (some v) ty => .return_ (some (rewriteExprTys m v)) (rewriteTy m ty)
  | .return_ none ty => .return_ none (rewriteTy m ty)
  | .expr e => .expr (rewriteExprTys m e)
  | .ifElse c t el =>
    .ifElse (rewriteExprTys m c) (rewriteStmtsTys m t) (el.map (rewriteStmtsTys m))
  | .while_ c body lbl step =>
    .while_ (rewriteExprTys m c) (rewriteStmtsTys m body) lbl (rewriteStmtsTys m step)
  | .fieldAssign obj f val => .fieldAssign (rewriteExprTys m obj) f (rewriteExprTys m val)
  | .derefAssign target val => .derefAssign (rewriteExprTys m target) (rewriteExprTys m val)
  | .arrayIndexAssign arr idx val =>
    .arrayIndexAssign (rewriteExprTys m arr) (rewriteExprTys m idx) (rewriteExprTys m val)
  | .break_ (some v) lbl => .break_ (some (rewriteExprTys m v)) lbl
  | .break_ none lbl => .break_ none lbl
  | .continue_ lbl => .continue_ lbl
  | .defer body => .defer (rewriteExprTys m body)
  | .borrowIn v r reg isMut ty body =>
    .borrowIn v r reg isMut (rewriteTy m ty) (rewriteStmtsTys m body)

private partial def rewriteStmtsTys (m : List (String × List Ty × String)) : List CStmt → List CStmt :=
  List.map (rewriteStmtTys m)
end

/-- Rewrite a function def: replace all generic struct types with named monomorphized types. -/
private def rewriteFnTys (mapping : List (String × List Ty × String)) (f : CFnDef) : CFnDef :=
  { f with
    params := f.params.map fun (n, t) => (n, rewriteTy mapping t)
    retTy := rewriteTy mapping f.retTy
    body := rewriteStmtsTys mapping f.body }

/-- Recursively collect all struct defs from a module and its submodules. -/
private partial def collectAllModuleStructs (m : CModule) : List CStructDef :=
  let own := m.structs
  let sub := m.submodules.foldl (fun acc s => acc ++ collectAllModuleStructs s) []
  own ++ sub

/-- Monomorphize generic struct definitions in a program.
    Collects all usages of generic struct types, creates concrete struct defs,
    and rewrites all references from `Ty.generic` to `Ty.named`. -/
private partial def monoStructsInProgram (modules : List CModule) : List CModule :=
  -- 1. Collect all generic struct names from all modules
  let genericNames := collectGenericStructNames modules
  if genericNames.isEmpty then modules
  else
  -- 2. Collect all struct defs (for substitution)
  let allStructs := modules.foldl (fun acc m => acc ++ collectAllModuleStructs m) []
  -- 3. Collect all generic struct instances from all functions across all modules
  let allInsts := modules.foldl (fun acc m =>
    (collectAllModuleFns m).foldl (fun acc f => acc ++ collectFnInstances genericNames f) acc) []
  let uniqueInsts := dedupInstances allInsts
  if uniqueInsts.isEmpty then modules
  else
  -- 4. Build the full mapping first: (baseName, args, mangledName)
  let mapping := uniqueInsts.filterMap fun (name, args) =>
    match allStructs.find? (fun sd => sd.name == name) with
    | some _ => some (name, args, monoStructName name args)
    | none => none
  -- Then create concrete struct defs using the full mapping (so nested generics resolve)
  let newStructs := mapping.filterMap fun (name, args, mangledName) =>
    match allStructs.find? (fun sd => sd.name == name) with
    | some sd =>
      let substSd := Layout.substStructTypeArgs sd args
      -- Rewrite field types: replace nested generic struct refs with their mangled names
      let rewrittenFields := substSd.fields.map fun (fn, ft) => (fn, rewriteTy mapping ft)
      some ({
        name := mangledName
        typeParams := []
        fields := rewrittenFields
        isPublic := sd.isPublic
        isCopy := sd.isCopy
        isReprC := sd.isReprC
        isPacked := sd.isPacked
        reprAlign := sd.reprAlign } : CStructDef)
    | none => none
  -- 5. Rewrite all modules: rewrite types in functions, add new struct defs to first module only
  let rec rewriteModule (m : CModule) : CModule :=
    { m with
      functions := m.functions.map (rewriteFnTys mapping)
      submodules := m.submodules.map rewriteModule }
  match modules with
  | [] => []
  | first :: rest =>
    let first' := rewriteModule first
    let first' := { first' with structs := first'.structs ++ newStructs }
    first' :: rest.map rewriteModule

-- ============================================================
-- Entry point
-- ============================================================

def monoProgram (modules : List CModule) : Except Diagnostics (List CModule) :=
  let allFns := modules.foldl (fun acc m => acc ++ collectAllModuleFns m) []
  let allAliases := modules.foldl (fun acc m => acc ++ collectAllModuleAliases m) []
  let initState : MonoState := { allFns := allFns, linkerAliases := allAliases }
  let (result, _) := (modules.mapM monoModule).run initState |>.run
  match result with
  | .ok ms => .ok (monoStructsInProgram ms)
  | .error e => .error e

end Concrete
