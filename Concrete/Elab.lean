import Concrete.AST
import Concrete.BuiltinSigs
import Concrete.Core
import Concrete.Diagnostic
import Concrete.FileSummary
import Concrete.Intrinsic
import Concrete.Resolve
import Concrete.Shared

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

abbrev ElabM := ExceptT Diagnostics (StateM ElabEnv)

inductive ElabError where
  -- Name resolution
  | selfOutsideImpl
  | undeclaredVariable (name : String)
  | undeclaredFunction (name : String)
  | unknownFunctionRef (name : String)
  | assignToUndeclaredVariable (name : String)
  | borrowUndeclaredVariable (name : String)
  -- Struct/field
  | unknownStructType (name : String)
  | arrowAccessUnknownStruct (name : String)
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
  | .unknownStructType name => s!"unknown struct type '{name}'"
  | .arrowAccessUnknownStruct name => s!"arrow access on unknown struct type '{name}'"
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
  | _ => none

def throwElab (e : ElabError) (span : Option Span := none) : ElabM α :=
  throw [{ severity := .error, message := e.message, pass := "elab", span := span, hint := e.hint }]

private def getEnv : ElabM ElabEnv := get
private def setEnv (env : ElabEnv) : ElabM Unit := set env

-- ============================================================
-- Helpers
-- ============================================================

private def isIntegerType : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | _ => false

private def isIntLit : Expr → Bool
  | .intLit _ _ => true
  | .paren _ inner => isIntLit inner
  | _ => false

private def isFloatType : Ty → Bool
  | .float32 | .float64 => true
  | _ => false

private def isPointerType : Ty → Bool
  | .ptrMut _ | .ptrConst _ => true
  | _ => false

/-- Substitute type variables in a type. -/
private def substTy (mapping : List (String × Ty)) : Ty → Ty
  | .named name => match mapping.lookup name with | some t => t | none => .named name
  | .typeVar name => match mapping.lookup name with | some t => t | none => .typeVar name
  | .ref inner => .ref (substTy mapping inner)
  | .refMut inner => .refMut (substTy mapping inner)
  | .ptrMut inner => .ptrMut (substTy mapping inner)
  | .ptrConst inner => .ptrConst (substTy mapping inner)
  | .array elem n => .array (substTy mapping elem) n
  | .generic name args => .generic name (args.map (substTy mapping))
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
      | some resolved => return resolved
      | none =>
        -- Erase newtypes: resolve to inner type
        match env.newtypes.find? fun nt => nt.name == name with
        | some nt => resolveTypeE nt.innerTy
        | none => return ty
  | .ref inner => return .ref (← resolveTypeE inner)
  | .refMut inner => return .refMut (← resolveTypeE inner)
  | .ptrMut inner => return .ptrMut (← resolveTypeE inner)
  | .ptrConst inner => return .ptrConst (← resolveTypeE inner)
  | .array elem n => return .array (← resolveTypeE elem) n
  | .generic "Heap" [inner] => return .heap (← resolveTypeE inner)
  | .generic "HeapArray" [inner] => return .heapArray (← resolveTypeE inner)
  | .generic name args =>
    let env ← getEnv
    match env.newtypes.find? fun nt => nt.name == name with
    | some nt =>
      let resolvedArgs ← args.mapM resolveTypeE
      let mapping := nt.typeParams.zip resolvedArgs
      resolveTypeE (substTy mapping nt.innerTy)
    | none => return .generic name (← args.mapM resolveTypeE)
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

private def addVar (name : String) (ty : Ty) : ElabM Unit := do
  let env ← getEnv
  setEnv { env with vars := (name, ty) :: env.vars }



/-- Unify a pattern type with an actual type to discover type variable bindings. -/
private partial def unifyTypes (pattern actual : Ty) (typeParams : List String) : List (String × Ty) :=
  match pattern with
  | .named name => if typeParams.contains name then [(name, actual)] else []
  | .typeVar name => if typeParams.contains name then [(name, actual)] else []
  | .ref inner => match actual with
    | .ref a => unifyTypes inner a typeParams
    | _ => []
  | .refMut inner => match actual with
    | .refMut a => unifyTypes inner a typeParams
    | _ => []
  | .generic _ pArgs => match actual with
    | .generic _ aArgs =>
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
  | .structLit _ name typeArgs _ =>
    if typeArgs.isEmpty then return .named name else return .generic name typeArgs
  | .enumLit _ enumName _ typeArgs _ =>
    if typeArgs.isEmpty then return .named enumName else return .generic enumName typeArgs
  | .fnRef _ name =>
    let env ← getEnv
    match env.allFnSigPairs.lookup name with
    | some sig => return .fn_ (sig.params.map Prod.snd) sig.capSet sig.retTy
    | none => return .placeholder
  | .paren _ inner => peekExprType inner
  | .binOp _ _ lhs _ => peekExprType lhs
  | _ => return .placeholder

-- ============================================================
-- Core elaboration
-- ============================================================

mutual

partial def elabExpr (e : Expr) (hint : Option Ty := none) : ElabM CExpr := do
  match e with
  | .intLit _ v =>
    let ty := match hint with
      | some ty =>
        let tyR := ty  -- skip resolve in elab for perf; already resolved at let/call sites
        if isIntegerType tyR || tyR == .char then tyR
        else match tyR with | .typeVar _ => tyR | _ => .int
      | none => .int
    return .intLit v ty
  | .floatLit _ v =>
    let ty := match hint with
      | some ty => if isFloatType ty then ty else .float64
      | none => .float64
    return .floatLit v ty
  | .boolLit _ b => return .boolLit b
  | .strLit _ s => return .strLit s
  | .charLit _ c => return .charLit c

  | .ident _ name =>
    let env ← getEnv
    match env.constants.lookup name with
    | some ty => return .ident name ty
    | none =>
    match env.vars.lookup name with
    | some ty => return .ident name ty
    | none =>
      match ← lookupFnSig name with
      | some sig =>
        let paramTys := sig.params.map Prod.snd
        return .ident name (.fn_ paramTys sig.capSet sig.retTy)
      | none => throwElab (.undeclaredVariable name) (some e.getSpan)

  | .paren _ inner => elabExpr inner hint

  | .binOp _ op lhs rhs =>
    let cLhs ← elabExpr lhs hint
    let lTy := cLhs.ty
    let cRhs ← elabExpr rhs (some lTy)
    let rTy := cRhs.ty
    -- If one operand is a default-typed literal (Int/i64) and the other has a concrete
    -- smaller integer type, re-elaborate the literal with the concrete type as hint.
    -- This fixes `0 - x` where x: i32 producing `sub i64 0, %i32_val`.
    let (cLhs, cRhs, opTy) ← do
      let lhsIsDefaultInt := lTy == .int && isIntLit lhs
      let rhsIsDefaultInt := rTy == .int && isIntLit rhs
      if lhsIsDefaultInt && isIntegerType rTy && rTy != .int then do
        let cLhs' ← elabExpr lhs (some rTy)
        pure (cLhs', cRhs, rTy)
      else if rhsIsDefaultInt && isIntegerType lTy && lTy != .int then
        pure (cLhs, cRhs, lTy)
      else
        pure (cLhs, cRhs, lTy)
    let resultTy := match op with
      | .eq | .neq | .lt | .gt | .leq | .geq => .bool
      | .and_ | .or_ => .bool
      | _ => opTy
    return .binOp op cLhs cRhs resultTy

  | .unaryOp _ op operand =>
    let cOp ← elabExpr operand hint
    let resultTy := match op with
      | .not_ => Ty.bool
      | _ => cOp.ty
    return .unaryOp op cOp resultTy

  | .arrowAccess _ obj field =>
    -- Desugar: p->field → (*p).field
    let cObj ← elabExpr obj
    let objTy := cObj.ty
    let innerTy := match objTy with
      | .heap t => t | .heapArray t => t
      | .ref (.heap t) => t | .refMut (.heap t) => t
      | _ => .placeholder
    let derefTy := innerTy
    let cDeref := CExpr.deref cObj derefTy
    let structName := match innerTy with
      | .named n => n | .generic n _ => n | _ => ""
    match ← lookupStruct structName with
    | some sd =>
      let typeArgs := match innerTy with | .generic _ args => args | _ => []
      let mapping := sd.typeParams.zip typeArgs
      match sd.fields.find? fun f => f.name == field with
      | some f =>
        let fieldTy := substTy mapping f.ty
        let fieldTy ← resolveTypeE fieldTy
        return .fieldAccess cDeref field fieldTy
      | none => throwElab (.structHasNoField structName field) (some e.getSpan)
    | none => throwElab (.arrowAccessUnknownStruct structName) (some e.getSpan)

  | .allocCall _ inner allocExpr =>
    let cInner ← elabExpr inner hint
    let cAlloc ← elabExpr allocExpr
    return .allocCall cInner cAlloc cInner.ty

  | .whileExpr _ cond body elseBody =>
    let cCond ← elabExpr cond
    let cBody ← elabStmts body
    let cElse ← elabStmts elseBody
    -- Result type comes from else body (last expression)
    let resultTy := match hint with | some t => t | none => .unit
    return .whileExpr cCond cBody cElse resultTy

  | .ifExpr _ cond then_ else_ =>
    let cCond ← elabExpr cond
    let cThen ← elabStmts then_
    let cElse ← elabStmts else_
    -- Result type comes from the last statement in the then branch
    let resultTy := match hint with | some t => t | none => .unit
    return .ifExpr cCond cThen cElse resultTy

  | .call _ fnName typeArgs args =>
    elabCall fnName typeArgs args hint (some e.getSpan)

  | .structLit _ name typeArgs fields =>
    match ← lookupStruct name with
    | some sd =>
      let mapping := sd.typeParams.zip typeArgs
      let mut cFields : List (String × CExpr) := []
      for sf in sd.fields do
        let fieldTy := substTy mapping sf.ty
        match fields.find? fun (fn, _) => fn == sf.name with
        | some (_, expr) =>
          let cExpr ← elabExpr expr (some fieldTy)
          cFields := cFields ++ [(sf.name, cExpr)]
        | none => pure ()  -- union partial init
      let resultTy := if typeArgs.isEmpty then Ty.named name else Ty.generic name typeArgs
      return .structLit name typeArgs cFields resultTy
    | none => throwElab (.unknownStructType name) (some e.getSpan)

  | .fieldAccess _ obj field =>
    let cObj ← elabExpr obj
    let objTy := cObj.ty
    let innerTy := match objTy with
      | .ref t => t | .refMut t => t | t => t
    let (structName, typeArgs) := match innerTy with
      | .named n => (n, ([] : List Ty))
      | .generic n args => (n, args)
      | .string => ("String", [])
      | _ => ("", [])
    match ← lookupStruct structName with
    | some sd =>
      let mapping := sd.typeParams.zip typeArgs
      match sd.fields.find? fun f => f.name == field with
      | some f =>
        let fieldTy := substTy mapping f.ty
        let fieldTy ← resolveTypeE fieldTy
        return .fieldAccess cObj field fieldTy
      | none =>
        -- Erased newtype wrapping a struct: .0 is identity
        if field == newtypeFieldName then return cObj
        else throwElab (.structHasNoField structName field) (some e.getSpan)
    | none =>
      -- Erased newtype: .0 on a primitive type is identity
      if field == newtypeFieldName then return cObj
      else throwElab .fieldAccessNonStruct (some e.getSpan)

  | .enumLit _ enumName variant typeArgs fields =>
    match ← lookupEnum enumName with
    | some ed =>
      let effectiveTypeArgs := if typeArgs.isEmpty && !ed.typeParams.isEmpty then
        match hint with
        | some (.generic n args) => if n == enumName then args else []
        | some (.named n) => if n == enumName then [] else []
        | _ => []
      else typeArgs
      let mapping := ed.typeParams.zip effectiveTypeArgs
      match ed.variants.find? fun v => v.name == variant with
      | some ev =>
        let mut cFields : List (String × CExpr) := []
        for sf in ev.fields do
          let fieldTy := substTy mapping sf.ty
          match fields.find? fun (fn, _) => fn == sf.name with
          | some (_, expr) =>
            let cExpr ← elabExpr expr (some fieldTy)
            cFields := cFields ++ [(sf.name, cExpr)]
          | none => throwElab (.missingFieldInVariant sf.name enumName variant) (some e.getSpan)
        let resultTy := if effectiveTypeArgs.isEmpty then Ty.named enumName
                         else Ty.generic enumName effectiveTypeArgs
        return .enumLit enumName variant effectiveTypeArgs cFields resultTy
      | none => throwElab (.unknownVariant variant enumName) (some e.getSpan)
    | none => throwElab (.unknownEnumType enumName) (some e.getSpan)

  | .match_ _ scrutinee arms =>
    let cScrut ← elabExpr scrutinee
    let scrTy := cScrut.ty
    let innerTy := match scrTy with
      | .ref t => t | .refMut t => t | t => t
    let innerTyR ← resolveTypeE innerTy
    let (enumName, enumTypeArgs) := match innerTyR with
      | .named n => (n, ([] : List Ty))
      | .generic n args => (n, args)
      | _ => ("", [])
    let mut cArms : List CMatchArm := []
    if enumName != "" then
      match ← lookupEnum enumName with
      | some ed =>
        let envBefore ← getEnv
        for arm in arms do
          setEnv envBefore
          match arm with
          | .mk _ _armEnum armVariant bindings body =>
            let ev := (ed.variants.find? fun v => v.name == armVariant).getD
              { name := armVariant, fields := [] }
            let typeMapping := ed.typeParams.zip enumTypeArgs
            let mut typedBindings : List (String × Ty) := []
            for (binding, sf) in bindings.zip ev.fields do
              let bty := substTy typeMapping sf.ty
              typedBindings := typedBindings ++ [(binding, bty)]
              addVar binding bty
            let cBody ← elabStmts body
            cArms := cArms ++ [.enumArm enumName armVariant typedBindings cBody]
          | .litArm _ val body =>
            let cVal ← elabExpr val
            let cBody ← elabStmts body
            cArms := cArms ++ [.litArm cVal cBody]
          | .varArm _ binding body =>
            addVar binding innerTyR
            let cBody ← elabStmts body
            cArms := cArms ++ [.varArm binding innerTyR cBody]
        setEnv envBefore
      | none =>
        let envBefore ← getEnv
        for arm in arms do
          setEnv envBefore
          match arm with
          | .litArm _ val body =>
            let cVal ← elabExpr val
            let cBody ← elabStmts body
            cArms := cArms ++ [.litArm cVal cBody]
          | .varArm _ binding body =>
            addVar binding innerTyR
            let cBody ← elabStmts body
            cArms := cArms ++ [.varArm binding innerTyR cBody]
          | .mk _ en v _ body =>
            let cBody ← elabStmts body
            cArms := cArms ++ [.enumArm en v [] cBody]
        setEnv envBefore
    else
      let envBefore ← getEnv
      for arm in arms do
        setEnv envBefore
        match arm with
        | .litArm _ val body =>
          let cVal ← elabExpr val (some innerTyR)
          let cBody ← elabStmts body
          cArms := cArms ++ [.litArm cVal cBody]
        | .varArm _ binding body =>
          addVar binding innerTyR
          let cBody ← elabStmts body
          cArms := cArms ++ [.varArm binding innerTyR cBody]
        | .mk _ en v _ body =>
          let cBody ← elabStmts body
          cArms := cArms ++ [.enumArm en v [] cBody]
      setEnv envBefore
    -- Result type comes from the arm bodies (checked by Check), not the scrutinee
    let resultTy := match hint with | some t => t | none => .unit
    return .match_ cScrut cArms resultTy

  | .borrow _ inner =>
    let cInner ← elabExpr inner
    return .borrow cInner (.ref cInner.ty)

  | .borrowMut _ inner =>
    let cInner ← elabExpr inner
    return .borrowMut cInner (.refMut cInner.ty)

  | .deref _ inner =>
    let cInner ← elabExpr inner
    let resultTy := match cInner.ty with
      | .ref t => t | .refMut t => t
      | .ptrMut t => t | .ptrConst t => t | .heap t => t
      | _ => .placeholder
    return .deref cInner resultTy

  | .try_ _ inner =>
    let cInner ← elabExpr inner
    let resultTy := match cInner.ty with
      | .named _enumName => .placeholder  -- would need enum lookup for Ok field
      | .generic _ [okTy, _] => okTy
      | _ => .placeholder
    return .try_ cInner resultTy

  | .arrayLit _ elems =>
    match elems with
    | [] => throwElab .arrayLiteralEmpty (some e.getSpan)
    | first :: rest =>
      let elemHint := match hint with | some (.array t _) => some t | _ => none
      let cFirst ← elabExpr first elemHint
      let elemTy := cFirst.ty
      let mut cElems : List CExpr := [cFirst]
      for e in rest do
        let cE ← elabExpr e (some elemTy)
        cElems := cElems ++ [cE]
      return .arrayLit cElems (.array elemTy elems.length)

  | .arrayIndex _ arr index =>
    let cArr ← elabExpr arr
    let cIdx ← elabExpr index (some .int)
    let elemTy := match cArr.ty with
      | .array t _ => t
      | _ => .placeholder
    return .arrayIndex cArr cIdx elemTy

  | .cast _ inner targetTy =>
    -- Do NOT pass any hint: the point of `as` is to convert between types.
    -- Passing targetTy would mistype literals in `(100 + m) as i32` where m is Int.
    -- Passing the outer hint could also leak i32 context into the inner expression.
    let cInner ← elabExpr inner none
    return .cast cInner targetTy

  | .fnRef _ fnName =>
    let env ← getEnv
    match env.allFnSigPairs.lookup fnName with
    | some sig =>
      let paramTys := sig.params.map Prod.snd
      return .fnRef fnName (.fn_ paramTys sig.capSet sig.retTy)
    | none => throwElab (.unknownFunctionRef fnName) (some e.getSpan)

  | .methodCall _ obj methodName typeArgs args =>
    -- Desugar: obj.method(args) → Type_method(&obj, args) or Type_method(&mut obj, args)
    let cObj ← elabExpr obj
    let objTy := cObj.ty
    let innerTy := match objTy with
      | .ref t => t | .refMut t => t | t => t
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
          for (arg, p) in args.zip params do
            let cArg ← elabExpr arg (some p.ty)
            cArgs := cArgs ++ [cArg]
          return .call (mangledMethodName n methodName) typeArgs cArgs retTy
      | _ => throwElab .methodCallOnNonNamedType (some e.getSpan)
    else
      let mangledName := mangledMethodName typeName methodName
      match ← lookupFnSig mangledName with
      | some sig =>
        let objTypeArgs := match innerTy with | .generic _ args => args | _ => []
        let implTypeParams := sig.typeParams.take objTypeArgs.length
        let methodTypeParams := sig.typeParams.drop objTypeArgs.length
        let mapping := implTypeParams.zip objTypeArgs ++ methodTypeParams.zip typeArgs
        let methodParams := (sig.params.drop 1).map fun (_, t) => (substTy mapping t)
        let retTy := substTy mapping sig.retTy
        -- Wrap object with borrow/borrowMut if method expects a reference self
        -- and the object is not already a reference
        let selfArg := match sig.params.head? with
          | some (_, selfTy) =>
            let resolvedSelfTy := substTy mapping selfTy
            match resolvedSelfTy, objTy with
            | .ref _, .ref _ => cObj          -- already a ref, pass as-is
            | .ref _, .refMut _ => cObj       -- already a ref, pass as-is
            | .refMut _, .refMut _ => cObj    -- already a mut ref, pass as-is
            | .ref _, _ => CExpr.borrow cObj (.ref objTy)
            | .refMut _, _ => CExpr.borrowMut cObj (.refMut objTy)
            | _, _ => cObj                    -- by-value self, pass as-is
          | none => cObj
        let mut cArgs : List CExpr := [selfArg]
        for (arg, pTy) in args.zip methodParams do
          let cArg ← elabExpr arg (some pTy)
          cArgs := cArgs ++ [cArg]
        return .call mangledName (objTypeArgs ++ typeArgs) cArgs retTy
      | none => throwElab (.noMethodOnType methodName typeName) (some e.getSpan)

  | .staticMethodCall _ typeName methodName typeArgs args =>
    let mangledName := mangledMethodName typeName methodName
    match ← lookupFnSig mangledName with
    | some sig =>
      let mapping := sig.typeParams.zip typeArgs
      let paramTypes := sig.params.map fun (_, t) => substTy mapping t
      let retTy := substTy mapping sig.retTy
      let mut cArgs : List CExpr := []
      for (arg, pTy) in args.zip paramTypes do
        let cArg ← elabExpr arg (some pTy)
        cArgs := cArgs ++ [cArg]
      return .call mangledName typeArgs cArgs retTy
    | none => throwElab (.noMethodOnType methodName typeName) (some e.getSpan)

/-- Elaborate a function call (regular, builtins, intercepted). -/
partial def elabCall (fnName : String) (typeArgs : List Ty) (args : List Expr)
    (_hint : Option Ty) (span : Option Span := none) : ElabM CExpr := do
  let intrinsic := resolveIntrinsic fnName
  -- Intercept abort()
  if intrinsic == some .abort then
    return .call "abort" [] [] .never
  -- Intercept destroy(arg)
  if intrinsic == some .destroy then
    let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
    let cArg ← elabExpr arg
    let typeName := match cArg.ty with
      | .named n => n | .generic n _ => n | _ => ""
    return .call (destroyFnNameFor typeName) [] [cArg] .unit
  -- Intercept alloc(val)
  if intrinsic == some .alloc then
    let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
    let cArg ← elabExpr arg
    return .call "alloc" [] [cArg] (.heap cArg.ty)
  -- Intercept free(ptr)
  if intrinsic == some .free then
    let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
    let cArg ← elabExpr arg
    let innerTy := match cArg.ty with | .heap t => t | _ => .placeholder
    return .call "free" [] [cArg] innerTy
  -- Intercept newtype constructor: erase to inner expression
  let env ← getEnv
  match env.newtypes.find? fun nt => nt.name == fnName with
  | some _nt =>
    let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
    let cArg ← elabExpr arg
    return cArg  -- newtype erasure: just return the inner value
  | none => pure ()
  -- Intercept unwrap(x): erase to inner expression (only if not a user-defined function)
  if intrinsic == some .unwrap && args.length == 1 then
    let isUserFn ← lookupFnSig "unwrap"
    if isUserFn.isNone then
      let arg := match args with | a :: _ => a | [] => Expr.intLit default 0
      let cArg ← elabExpr arg
      return cArg  -- newtype erasure: just return the inner value
  -- Intercept sizeof/alignof
  if intrinsic == some .sizeof || intrinsic == some .alignof || fnName.endsWith sizeofSuffix then
    return .call fnName typeArgs [] .uint
  -- Intercept vec_new::<T>()
  if intrinsic == some .vecNew then
    let elemTy := match typeArgs with | t :: _ => t | [] => .int
    return .call "vec_new" typeArgs [] (.generic "Vec" [elemTy])
  -- Intercept string_push_char(&mut s, ch)
  if intrinsic == some .stringPushChar then
    let mut cArgs : List CExpr := []
    for arg in args do
      let cArg ← elabExpr arg
      cArgs := cArgs ++ [cArg]
    return .call "string_push_char" [] cArgs .unit
  -- Intercept string_append(&mut s, other)
  if intrinsic == some .stringAppend then
    let mut cArgs : List CExpr := []
    for arg in args do
      let cArg ← elabExpr arg
      cArgs := cArgs ++ [cArg]
    return .call "string_append" [] cArgs .unit
  -- Intercept string_append_int(&mut s, n)
  if intrinsic == some .stringAppendInt then
    let mut cArgs : List CExpr := []
    for arg in args do
      let cArg ← elabExpr arg
      cArgs := cArgs ++ [cArg]
    return .call "string_append_int" [] cArgs .unit
  -- Intercept string_append_bool(&mut s, b)
  if intrinsic == some .stringAppendBool then
    let mut cArgs : List CExpr := []
    for arg in args do
      let cArg ← elabExpr arg
      cArgs := cArgs ++ [cArg]
    return .call "string_append_bool" [] cArgs .unit
  -- Intercept string_reserve(&mut s, cap)
  if intrinsic == some .stringReserve then
    let mut cArgs : List CExpr := []
    for arg in args do
      let cArg ← elabExpr arg
      cArgs := cArgs ++ [cArg]
    return .call "string_reserve" [] cArgs .unit
  -- Intercept vec_push
  if intrinsic == some .vecPush then
    -- Elaborate vec arg first to extract element type for value hint
    let elemTy := match typeArgs with | t :: _ => t | [] => .int
    let mut cArgs : List CExpr := []
    match args with
    | vecArg :: valArg :: rest =>
      let cVec ← elabExpr vecArg
      cArgs := cArgs ++ [cVec]
      let cVal ← elabExpr valArg (some elemTy)
      cArgs := cArgs ++ [cVal]
      for arg in rest do
        let cArg ← elabExpr arg
        cArgs := cArgs ++ [cArg]
    | _ =>
      for arg in args do
        let cArg ← elabExpr arg
        cArgs := cArgs ++ [cArg]
    return .call "vec_push" [] cArgs .unit
  -- Intercept vec_get
  if intrinsic == some .vecGet then
    let mut cArgs : List CExpr := []
    match args with
    | vecArg :: idxArg :: rest =>
      let cVec ← elabExpr vecArg
      cArgs := cArgs ++ [cVec]
      let cIdx ← elabExpr idxArg (some .int)
      cArgs := cArgs ++ [cIdx]
      for arg in rest do
        let cArg ← elabExpr arg
        cArgs := cArgs ++ [cArg]
    | _ =>
      for arg in args do
        let cArg ← elabExpr arg
        cArgs := cArgs ++ [cArg]
    let elemTy := match (cArgs.head?.map CExpr.ty) with
      | some (.ref (.generic "Vec" [et])) => et
      | some (.refMut (.generic "Vec" [et])) => et
      | _ => .placeholder
    return .call "vec_get" [] cArgs elemTy
  -- Intercept vec_set
  if intrinsic == some .vecSet then
    let elemTy := match typeArgs with | t :: _ => t | [] => .int
    let mut cArgs : List CExpr := []
    match args with
    | vecArg :: idxArg :: valArg :: rest =>
      let cVec ← elabExpr vecArg
      cArgs := cArgs ++ [cVec]
      let cIdx ← elabExpr idxArg (some .int)
      cArgs := cArgs ++ [cIdx]
      let cVal ← elabExpr valArg (some elemTy)
      cArgs := cArgs ++ [cVal]
      for arg in rest do
        let cArg ← elabExpr arg
        cArgs := cArgs ++ [cArg]
    | _ =>
      for arg in args do
        let cArg ← elabExpr arg
        cArgs := cArgs ++ [cArg]
    return .call "vec_set" [] cArgs .unit
  -- Intercept vec_len
  if intrinsic == some .vecLen then
    let mut cArgs : List CExpr := []
    for arg in args do
      let cArg ← elabExpr arg
      cArgs := cArgs ++ [cArg]
    return .call "vec_len" [] cArgs .int
  -- Intercept vec_pop
  if intrinsic == some .vecPop then
    let mut cArgs : List CExpr := []
    for arg in args do
      let cArg ← elabExpr arg
      cArgs := cArgs ++ [cArg]
    let elemTy := match (cArgs.head?.map CExpr.ty) with
      | some (.refMut (.generic "Vec" [et])) => et
      | _ => .placeholder
    return .call "vec_pop" [] cArgs (.generic optionEnumName [elemTy])
  -- Intercept vec_free
  if intrinsic == some .vecFree then
    let mut cArgs : List CExpr := []
    for arg in args do
      let cArg ← elabExpr arg
      cArgs := cArgs ++ [cArg]
    return .call "vec_free" [] cArgs .unit
  -- Check if function pointer call
  match ← lookupVar fnName with
  | some (.fn_ paramTys _ retTy) =>
    let mut cArgs : List CExpr := []
    for (arg, pTy) in args.zip paramTys do
      let cArg ← elabExpr arg (some pTy)
      cArgs := cArgs ++ [cArg]
    return .call fnName [] cArgs retTy
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
    for (arg, pTy) in args.zip paramTypes do
      let cArg ← elabExpr arg (some pTy)
      cArgs := cArgs ++ [cArg]
    -- Use canonical name for intrinsics (e.g., string_substr → string_slice)
    let callName := match intrinsic with
      | some id => id.canonicalName
      | none => fnName
    return .call callName inferredTypeArgs cArgs retTy
  | none => throwElab (.undeclaredFunction fnName) span

partial def elabStmt (stmt : Stmt) : ElabM (List CStmt) := do
  match stmt with
  | .letDecl _ name mutable ty value =>
    let valHint ← match ty with
      | some t => do let t' ← resolveTypeE t; pure (some t')
      | none => pure none
    let cVal ← elabExpr value valHint
    let finalTy ← match ty with
      | some t => resolveTypeE t
      | none => pure cVal.ty
    addVar name finalTy
    return [.letDecl name mutable finalTy cVal]

  | .assign _ name value =>
    match ← lookupVar name with
    | some varTy =>
      let cVal ← elabExpr value (some varTy)
      return [.assign name cVal]
    | none => throwElab (.assignToUndeclaredVariable name) (some stmt.getSpan)

  | .return_ _ (some value) =>
    let env ← getEnv
    let cVal ← elabExpr value (some env.currentRetTy)
    return [.return_ (some cVal) env.currentRetTy]

  | .return_ _ none =>
    let env ← getEnv
    return [.return_ none env.currentRetTy]

  | .expr _ (.call _sp fnName _typeArgs args) =>
    -- Desugar print/println into individual typed print calls
    -- Only if not shadowed by a user/stdlib function with the same name
    let existingFn ← lookupFnSig fnName
    if existingFn.isNone && (fnName == "print" || fnName == "println") then
      let mut stmts : List CStmt := []
      for arg in args do
        let cArg ← elabExpr arg
        let printCall := match cArg.ty with
          | .string =>
            CStmt.expr (CExpr.call "print_string" [] [CExpr.borrow cArg (.ref .string)] .unit)
          | .ref .string | .refMut .string =>
            CStmt.expr (CExpr.call "print_string" [] [cArg] .unit)
          | .int =>
            CStmt.expr (CExpr.call "print_int" [] [cArg] .unit)
          | .uint | .i32 | .i16 | .i8 | .u32 | .u16 | .u8 =>
            CStmt.expr (CExpr.call "print_int" [] [CExpr.cast cArg .int] .unit)
          | .bool =>
            CStmt.expr (CExpr.call "print_bool" [] [cArg] .unit)
          | .char =>
            CStmt.expr (CExpr.call "print_char" [] [CExpr.cast cArg .int] .unit)
          | _ =>
            CStmt.expr (CExpr.call "print_string" [] [CExpr.strLit "<unprintable>"] .unit)
        stmts := stmts ++ [printCall]
      if fnName == "println" then
        stmts := stmts ++ [CStmt.expr (CExpr.call "print_char" [] [CExpr.intLit 10 .int] .unit)]
      return stmts
    else
      let cE ← elabExpr (.call _sp fnName _typeArgs args)
      return [.expr cE]

  | .expr _ e =>
    let cE ← elabExpr e
    return [.expr cE]

  | .ifElse _ cond then_ else_ =>
    let cCond ← elabExpr cond (some .bool)
    let cThen ← elabStmts then_
    let cElse ← match else_ with
      | some stmts => do let cs ← elabStmts stmts; pure (some cs)
      | none => pure none
    return [.ifElse cCond cThen cElse]

  | .while_ _ cond body label =>
    let cCond ← elabExpr cond (some .bool)
    let cBody ← elabStmts body
    return [.while_ cCond cBody label []]

  | .forLoop _ init cond step body label =>
    -- Desugar: for (init; cond; step) { body } → init; while cond { body; step }
    let mut result : List CStmt := []
    match init with
    | some initStmt =>
      let cInit ← elabStmt initStmt
      result := result ++ cInit
    | none => pure ()
    let cCond ← elabExpr cond (some .bool)
    let cBody ← elabStmts body
    let cStep ← match step with
      | some stepStmt => elabStmt stepStmt
      | none => pure []
    let whileBody := cBody ++ cStep
    result := result ++ [.while_ cCond whileBody label cStep]
    return result

  | .fieldAssign _ obj field value =>
    let cObj ← elabExpr obj
    let cVal ← elabExpr value
    return [.fieldAssign cObj field cVal]

  | .derefAssign _ target value =>
    let cTarget ← elabExpr target
    let innerTy := match cTarget.ty with
      | .ref t => t | .refMut t => t
      | .ptrMut t => t | .ptrConst t => t
      | _ => .placeholder
    let cVal ← elabExpr value (some innerTy)
    return [.derefAssign cTarget cVal]

  | .arrayIndexAssign _ arr index value =>
    let cArr ← elabExpr arr
    let cIdx ← elabExpr index (some .int)
    let cVal ← elabExpr value
    return [.arrayIndexAssign cArr cIdx cVal]

  | .break_ _ value label =>
    match value with
    | some v =>
      let cV ← elabExpr v
      return [.break_ (some cV) label]
    | none => return [.break_ none label]

  | .continue_ _ label =>
    return [.continue_ label]

  | .defer _ body =>
    let cBody ← elabExpr body
    return [.defer cBody]

  | .borrowIn _ var ref region isMut body =>
    let varTy ← match ← lookupVar var with
      | some ty => pure ty
      | none => throwElab (.borrowUndeclaredVariable var) (some stmt.getSpan)
    let refTy := if isMut then Ty.refMut varTy else Ty.ref varTy
    addVar ref refTy
    let cBody ← elabStmts body
    return [.borrowIn var ref region isMut refTy cBody]

  | .arrowAssign _ obj field value =>
    -- Desugar: p->field = val → (*p).field = val
    let cObj ← elabExpr obj
    let objTy := cObj.ty
    let innerTy := match objTy with
      | .heap t => t | .heapArray t => t
      | .ref (.heap t) => t | .refMut (.heap t) => t
      | _ => .placeholder
    let cDeref := CExpr.deref cObj innerTy
    let cVal ← elabExpr value
    return [.fieldAssign cDeref field cVal]

partial def elabStmts (stmts : List Stmt) : ElabM (List CStmt) := do
  let mut result : List CStmt := []
  let mut accumulated : Diagnostics := []
  for s in stmts do
    let envBefore ← getEnv
    let r := (elabStmt s).run envBefore |>.run
    match r with
    | (.ok cs, envAfter) =>
      setEnv envAfter
      result := result ++ cs
    | (.error ds, _) =>
      accumulated := accumulated ++ ds
      -- Restore env so subsequent statements see a consistent state.
      -- For let-declarations, add the variable with its declared type (or placeholder)
      -- so later statements referencing it don't cascade spurious errors.
      setEnv envBefore
      match s with
      | .letDecl _ name _ ty _ =>
        let placeholderTy := ty.getD .placeholder
        addVar name placeholderTy
      | _ => pure ()
  if !accumulated.isEmpty then
    throw accumulated
  return result

end

-- ============================================================
-- Function and module elaboration
-- ============================================================

def elabFn (f : FnDef) (implTy : Option Ty := none) : ElabM CFnDef := do
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
      | .generic "Heap" [inner] => .heap (go inner)
      | .generic "HeapArray" [inner] => .heapArray (go inner)
      | .generic n args => .generic n (args.map go)
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
    currentImplType := implTy }
  -- Add parameters to scope (resolve type aliases so params don't carry unresolved alias names)
  for (pname, pty) in params do
    let resolvedPty ← resolveTypeE pty
    addVar pname resolvedPty
  -- Elaborate body
  let cBody ← elabStmts f.body
  -- Restore env
  let envAfter ← getEnv
  setEnv { envAfter with
    vars := env.vars
    currentTypeParams := env.currentTypeParams
    currentTypeBounds := env.currentTypeBounds
    currentRetTy := env.currentRetTy
    currentImplType := env.currentImplType }
  -- Resolve type aliases in output param/return types so Core IR doesn't carry alias names
  let resolvedParams ← params.mapM fun (n, t) => do pure (n, ← resolveTypeE t)
  let resolvedRetTy ← resolveTypeE retTy
  return {
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
  }

-- ============================================================
-- Submodule function name prefixing
-- ============================================================

mutual
/-- Rename function references in a CExpr tree using a lookup table. -/
partial def renameFnExpr (rmap : List (String × String)) : CExpr → CExpr
  | .call fn targs args ty =>
    let fn' := rmap.lookup fn |>.getD fn
    .call fn' targs (args.map (renameFnExpr rmap)) ty
  | .fnRef name ty =>
    let name' := rmap.lookup name |>.getD name
    .fnRef name' ty
  | .ident name ty =>
    -- Only rename function-typed idents (fn refs used as values), not local variables
    match ty with
    | .fn_ .. =>
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
  | .try_ inner ty => .try_ (renameFnExpr rmap inner) ty
  | .allocCall inner alloc ty =>
    .allocCall (renameFnExpr rmap inner) (renameFnExpr rmap alloc) ty
  | .whileExpr cond body elseBody ty =>
    .whileExpr (renameFnExpr rmap cond)
      (renameFnStmts rmap body) (renameFnStmts rmap elseBody) ty
  | .ifExpr cond then_ else_ ty =>
    .ifExpr (renameFnExpr rmap cond)
      (renameFnStmts rmap then_) (renameFnStmts rmap else_) ty
  | e => e

partial def renameFnArm (rmap : List (String × String)) : CMatchArm → CMatchArm
  | .enumArm en v binds body => .enumArm en v binds (renameFnStmts rmap body)
  | .litArm val body =>
    .litArm (renameFnExpr rmap val) (renameFnStmts rmap body)
  | .varArm b ty body => .varArm b ty (renameFnStmts rmap body)

partial def renameFnStmt (rmap : List (String × String)) : CStmt → CStmt
  | .letDecl n m ty val => .letDecl n m ty (renameFnExpr rmap val)
  | .assign n val => .assign n (renameFnExpr rmap val)
  | .return_ (some v) ty => .return_ (some (renameFnExpr rmap v)) ty
  | .expr e => .expr (renameFnExpr rmap e)
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
    (prefixSubs : Bool := true) : Except Diagnostics CModule :=
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
  let builtinOptionEnum : EnumDef := {
    name := optionEnumName, typeParams := ["T"],
    variants := [
      { name := "Some", fields := [{ name := "value", ty := .typeVar "T" }] },
      { name := "None", fields := [] }
    ], isCopy := false, builtinId := some .option
  }
  let builtinResultEnum : EnumDef := {
    name := resultEnumName, typeParams := ["T", "E"],
    variants := [
      { name := okVariantName, fields := [{ name := "value", ty := .typeVar "T" }] },
      { name := errVariantName, fields := [{ name := "value", ty := .typeVar "E" }] }
    ], isCopy := false, builtinId := some .result
  }
  let hasUserResult := m.enums.any fun ed => ed.name == resultEnumName
    || imports.enums.any fun ed => ed.name == resultEnumName
  let builtinEnumList := [builtinOptionEnum] ++ (if hasUserResult then [] else [builtinResultEnum])
  let allStructs := imports.structs ++ m.structs
  let allEnums := builtinEnumList ++ imports.enums ++ m.enums
  let localTypeAliases := m.typeAliases.map fun ta => (ta.name, ta.targetTy)
  let typeAliasMap := imports.typeAliases ++ localTypeAliases
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
  let initEnv : ElabEnv := {
    vars := []
    structs := allStructs
    enums := allEnums
    fnSigs := allSigs
    typeAliases := typeAliasMap
    constants := constantsMap
    traits := allTraits
    allFnSigPairs := fnSigPairs
    newtypes := m.newtypes
  }
  -- Elaborate only LOCAL functions (imported impl bodies are already elaborated in their module)
  let regularFns := m.functions.map fun f => (f, (none : Option Ty))
  let implMethodPairs := m.implBlocks.foldl (fun acc ib =>
    let implTy := if ib.typeParams.isEmpty then tyFromName ib.typeName
                  else Ty.generic ib.typeName (ib.typeParams.map Ty.typeVar)
    acc ++ ib.methods.map fun f =>
      ({ f with typeParams := ib.typeParams ++ f.typeParams,
                isTrusted := f.isTrusted || ib.isTrusted }, some implTy)
  ) ([] : List (FnDef × Option Ty))
  let traitImplMethodPairs := m.traitImpls.foldl (fun acc tb =>
    let implTy := if tb.typeParams.isEmpty then tyFromName tb.typeName
                  else Ty.generic tb.typeName (tb.typeParams.map Ty.typeVar)
    acc ++ tb.methods.map fun f =>
      ({ f with typeParams := tb.typeParams ++ f.typeParams,
                isTrusted := f.isTrusted || tb.isTrusted }, some implTy)
  ) ([] : List (FnDef × Option Ty))
  let allFnPairs := regularFns ++ implMethodPairs ++ traitImplMethodPairs
  let (fns, fnErrors, _) := allFnPairs.foldl (fun (acc, errs, env) (f, implTy) =>
    let env' := { env with currentImplType := implTy, traits := allTraits }
    let result := (do
      let cfn ← elabFn f implTy
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
      pure { cfn with name := finalName, trustedImplOrigin := implOrigin } : ElabM CFnDef).run env' |>.run
    match result with
    | (.ok cfn, finalEnv) => (acc ++ [cfn], errs, finalEnv)
    | (.error ds, _) => (acc, errs ++ ds, env)
  ) (([] : List CFnDef), ([] : Diagnostics), initEnv)
  if !fnErrors.isEmpty then .error fnErrors
  else
  -- Build Core structs (local definitions)
  let cStructs := m.structs.map fun sd =>
    { name := sd.name, typeParams := sd.typeParams,
      fields := sd.fields.map fun f => (f.name, f.ty),
      isPublic := sd.isPublic, isCopy := sd.isCopy, isReprC := sd.isReprC,
      isPacked := sd.isPacked, reprAlign := sd.reprAlign : CStructDef }
  -- Also convert imported structs so cross-module field offsets work in Lower/Layout
  let localStructNames := m.structs.map (·.name)
  let cImportedStructs := (imports.structs.filter fun sd =>
      !(localStructNames.contains sd.name)).map fun sd =>
    { name := sd.name, typeParams := sd.typeParams,
      fields := sd.fields.map fun f => (f.name, f.ty),
      isPublic := sd.isPublic, isCopy := sd.isCopy, isReprC := sd.isReprC,
      isPacked := sd.isPacked, reprAlign := sd.reprAlign : CStructDef }
  -- Build extern fns
  let cExterns := m.externFns.map fun ef =>
    (ef.name, ef.params.map fun p => (p.name, p.ty), ef.retTy, ef.isTrusted)
  -- Build constants
  let cConstants := m.constants.map fun c =>
    let constResult := (elabExpr c.value (some c.ty)).run initEnv |>.run
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
      let subImports := match liftStringError "elab" (resolveImports sub.imports summaryTable
          (fun modName => ElabError.message (.unknownModule modName))
          (fun sym modName => ElabError.message (.notPublicInModule sym modName))) with
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
      match elabModule sub subSummary subImports summaryTable (prefixSubs := false) with
      | .ok csub => .ok (lst ++ [csub])
      | .error ds => .error (ds.map fun d => { d with message := s!"in submodule '{sub.name}': {d.message}" })
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
  .ok {
    name := m.name
    structs := cStructs ++ cImportedStructs
    enums := allEnums.map fun ed =>
      { name := ed.name, typeParams := ed.typeParams,
        variants := ed.variants.map fun v =>
          (v.name, v.fields.map fun f => (f.name, f.ty)),
        isPublic := ed.isPublic, isCopy := ed.isCopy, builtinId := ed.builtinId : CEnumDef }
    functions := fns
    externFns := cExterns
    constants := cConstants
    submodules := subs
    traitDefs := m.traits.map fun td =>
      { name := td.name,
        methods := td.methods.map fun sig =>
          { name := sig.name, retTy := sig.retTy },
        builtinId := td.builtinId : CTraitDef }
    traitImpls := m.traitImpls.map fun tb =>
      let traitBuiltinId := match allTraits.find? fun td => td.name == tb.traitName with
        | some td => td.builtinId
        | none => none
      { traitName := tb.traitName,
        typeName := tb.typeName,
        methodNames := tb.methods.map (·.name),
        methodRetTys := tb.methods.map fun f => (f.name, f.retTy),
        builtinTraitId := traitBuiltinId : CTraitImpl }
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
    match liftStringError "elab" (resolveImports m.imports summaryTable
        (fun modName => ElabError.message (.unknownModule modName))
        (fun sym modName => ElabError.message (.notPublicInModule sym modName))) with
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
      | .error ds => (acc, errs ++ ds)
  ) (([] : List CModule), ([] : Diagnostics))
  if allErrors.isEmpty then .ok cms else .error allErrors

end Concrete
