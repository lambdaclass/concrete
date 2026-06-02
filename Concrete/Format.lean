import Concrete.AST

namespace Concrete

/-! ## Format — surface-syntax pretty-printer

Reprints a parsed AST with consistent 4-space indentation,
canonical brace placement, and normalized whitespace.
-/

-- ============================================================
-- Helpers
-- ============================================================

private def indent (n : Nat) : String :=
  String.ofList (List.replicate (n * 4) ' ')

private def binOpToStr : BinOp → String
  | .add => "+" | .sub => "-" | .mul => "*" | .div => "/" | .mod => "%"
  | .eq => "==" | .neq => "!=" | .lt => "<" | .gt => ">" | .leq => "<=" | .geq => ">="
  | .and_ => "&&" | .or_ => "||"
  | .bitand => "&" | .bitor => "|" | .bitxor => "^" | .shl => "<<" | .shr => ">>"

private def unaryOpToStr : UnaryOp → String
  | .neg => "-" | .not_ => "!" | .bitnot => "~"

-- ============================================================
-- Type formatting
-- ============================================================

partial def fmtTy : Ty → String
  | .int => "Int"
  | .uint => "Uint"
  | .i8 => "i8" | .i16 => "i16" | .i32 => "i32"
  | .u8 => "u8" | .u16 => "u16" | .u32 => "u32"
  | .bool => "bool"
  | .float64 => "f64"
  | .float32 => "f32"
  | .char => "char"
  | .unit => "()"
  | .named n => n
  | .string => "String"
  | .ref inner => s!"&{fmtTy inner}"
  | .refMut inner => s!"&mut {fmtTy inner}"
  | .generic n args => s!"{n}<{", ".intercalate (args.map fmtTy)}>"
  | .typeVar n => n
  | .array elem size => s!"[{fmtTy elem}; {size}]"
  | .ptrMut inner => s!"*mut {fmtTy inner}"
  | .ptrConst inner => s!"*const {fmtTy inner}"
  | .fn_ params cs ret =>
    let capsStr := fmtCapSet cs
    let capsStr := if capsStr.isEmpty then "" else s!" {capsStr}"
    s!"fn({", ".intercalate (params.map fmtTy)}){capsStr} -> {fmtTy ret}"
  | .never => "!"
  | .heap inner => s!"Heap<{fmtTy inner}>"
  | .heapArray inner => s!"HeapArray<{fmtTy inner}>"
  | .placeholder => "_"

where
  fmtCapSet : CapSet → String
    | .empty => ""
    | .concrete caps => s!"with({", ".intercalate caps})"
    | .var name => s!"with({name})"
    | .union a b =>
      let aCaps := collectCaps a
      let bCaps := collectCaps b
      s!"with({", ".intercalate (aCaps ++ bCaps)})"
  collectCaps : CapSet → List String
    | .empty => []
    | .concrete caps => caps
    | .var name => [name]
    | .union a b => collectCaps a ++ collectCaps b

private def fmtCapSetTop : CapSet → String
  | .empty => ""
  | .concrete caps => s!" with({", ".intercalate caps})"
  | .var name => s!" with({name})"
  | .union a b =>
    let rec collectCaps : CapSet → List String
      | .empty => []
      | .concrete caps => caps
      | .var name => [name]
      | .union a b => collectCaps a ++ collectCaps b
    s!" with({", ".intercalate (collectCaps a ++ collectCaps b)})"

-- ============================================================
-- Type param / bound formatting
-- ============================================================

private def fmtTypeParams (typeParams : List String) (typeBounds : List (String × List String) := []) : String :=
  if typeParams.isEmpty then ""
  else
    let parts := typeParams.map fun tp =>
      match typeBounds.find? fun (n, _) => n == tp with
      | some (_, bounds) => s!"{tp}: {" + ".intercalate bounds}"
      | none => tp
    s!"<{", ".intercalate parts}>"

-- ============================================================
-- Expression formatting
-- ============================================================

mutual
partial def fmtExprAt (ind : Nat) : Expr → String
  | .intLit _ val => toString val
  | .floatLit _ val => toString val
  | .boolLit _ val => if val then "true" else "false"
  | .strLit _ val => s!"\"{val}\""
  | .charLit _ val => s!"'{val}'"
  | .ident _ name => name
  | .binOp _ op lhs rhs => s!"{fmtExprAt ind lhs} {binOpToStr op} {fmtExprAt ind rhs}"
  | .unaryOp _ op operand => s!"{unaryOpToStr op}{fmtExprParensAt ind operand}"
  | .call _ fn typeArgs args =>
    let targsStr := if typeArgs.isEmpty then "" else s!"::<{", ".intercalate (typeArgs.map fmtTy)}>"
    s!"{fn}{targsStr}({", ".intercalate (args.map (fmtExprAt ind))})"
  | .paren _ inner => s!"({fmtExprAt ind inner})"
  | .structLit _ name typeArgs fields =>
    let targsStr := if typeArgs.isEmpty then "" else s!"::<{", ".intercalate (typeArgs.map fmtTy)}>"
    let fs := fields.map fun (k, v) =>
      match v with
      | .ident _ n => if n == k then k else s!"{k}: {fmtExprAt ind v}"
      | _ => s!"{k}: {fmtExprAt ind v}"
    s!"{name}{targsStr} \{ {", ".intercalate fs} }"
  | .fieldAccess _ obj field => s!"{fmtExprAt ind obj}.{field}"
  | .enumLit _ enumName variant typeArgs fields =>
    let targsStr := if typeArgs.isEmpty then "" else s!"::<{", ".intercalate (typeArgs.map fmtTy)}>"
    if fields.isEmpty then s!"{enumName}{targsStr}::{variant}"
    else
      let fs := fields.map fun (k, v) =>
        match v with
        | .ident _ n => if n == k then k else s!"{k}: {fmtExprAt ind v}"
        | _ => s!"{k}: {fmtExprAt ind v}"
      s!"{enumName}{targsStr}::{variant} \{ {", ".intercalate fs} }"
  | .match_ _ scrutinee arms =>
    let pfx := indent ind
    let armsStr := arms.map (fmtMatchArm ind)
    s!"match {fmtExprAt ind scrutinee} \{\n{"\n".intercalate armsStr}\n{pfx}}"
  | .borrow _ inner => s!"&{fmtExprAt ind inner}"
  | .borrowMut _ inner => s!"&mut {fmtExprAt ind inner}"
  | .deref _ inner => s!"*{fmtExprParensAt ind inner}"
  | .try_ _ inner => s!"{fmtExprAt ind inner}?"
  | .arrayLit _ elems => s!"[{", ".intercalate (elems.map (fmtExprAt ind))}]"
  | .arrayIndex _ arr index => s!"{fmtExprAt ind arr}[{fmtExprAt ind index}]"
  | .cast _ inner targetTy => s!"{fmtExprAt ind inner} as {fmtTy targetTy}"
  | .methodCall _ obj method typeArgs args =>
    let targsStr := if typeArgs.isEmpty then "" else s!"::<{", ".intercalate (typeArgs.map fmtTy)}>"
    s!"{fmtExprAt ind obj}.{method}{targsStr}({", ".intercalate (args.map (fmtExprAt ind))})"
  | .staticMethodCall _ typeName method typeArgs args =>
    let targsStr := if typeArgs.isEmpty then "" else s!"::<{", ".intercalate (typeArgs.map fmtTy)}>"
    s!"{typeName}{targsStr}::{method}({", ".intercalate (args.map (fmtExprAt ind))})"
  | .fnRef _ name => name
  | .arrowAccess _ obj field => s!"{fmtExprAt ind obj}->{field}"
  | .allocCall _ inner allocExpr => s!"{fmtExprAt ind inner} with(Alloc = {fmtExprAt ind allocExpr})"
  | .whileExpr _ cond body elseBody =>
    let pfx := indent ind
    let bodyStr := body.map (fmtStmt (ind + 1))
    let elseStr := if elseBody.isEmpty then ""
      else s!" else \{\n{"\n".intercalate (elseBody.map (fmtStmt (ind + 1)))}\n{pfx}}"
    s!"while {fmtExprAt ind cond} \{\n{"\n".intercalate bodyStr}\n{pfx}}{elseStr}"
  | .ifExpr _ cond then_ else_ =>
    let pfx := indent ind
    let thenStr := then_.map (fmtStmt (ind + 1))
    let elseStr := else_.map (fmtStmt (ind + 1))
    s!"if {fmtExprAt ind cond} \{\n{"\n".intercalate thenStr}\n{pfx}} else \{\n{"\n".intercalate elseStr}\n{pfx}}"

partial def fmtExprParensAt (ind : Nat) (e : Expr) : String :=
  match e with
  | .binOp .. | .cast .. => s!"({fmtExprAt ind e})"
  | _ => fmtExprAt ind e

-- ============================================================
-- Match arm formatting
-- ============================================================

partial def fmtMatchArm (baseInd : Nat) (arm : MatchArm) : String :=
  let pfx := indent (baseInd + 1)
  let bodyInd := baseInd + 2
  match arm with
  | .mk _ enumName variant bindings body =>
    let bindsStr := if bindings.isEmpty then ""
      else s!" \{ {", ".intercalate bindings} }"
    let bodyStr := body.map (fmtStmt bodyInd)
    s!"{pfx}{enumName}::{variant}{bindsStr} => \{\n{"\n".intercalate bodyStr}\n{pfx}},"
  | .litArm _ value body =>
    let bodyStr := body.map (fmtStmt bodyInd)
    s!"{pfx}{fmtExprAt baseInd value} => \{\n{"\n".intercalate bodyStr}\n{pfx}},"
  | .varArm _ binding body =>
    let bodyStr := body.map (fmtStmt bodyInd)
    s!"{pfx}{binding} => \{\n{"\n".intercalate bodyStr}\n{pfx}},"

-- ============================================================
-- Statement formatting
-- ============================================================

partial def fmtStmt (ind : Nat) (s : Stmt) : String :=
  let pfx := indent ind
  match s with
  | .letDecl _ name mutable ty value =>
    let mutStr := if mutable then "mut " else ""
    let tyStr := match ty with
      | some t => s!": {fmtTy t}"
      | none => ""
    s!"{pfx}let {mutStr}{name}{tyStr} = {fmtExprAt ind value};"
  | .assign _ name value => s!"{pfx}{name} = {fmtExprAt ind value};"
  | .return_ _ (some value) => s!"{pfx}return {fmtExprAt ind value};"
  | .return_ _ none => s!"{pfx}return;"
  | .expr _ e =>
    -- match/while expressions used as statements don't need trailing semicolons
    let needsSemi := match e with
      | .match_ .. | .whileExpr .. | .ifExpr .. => false
      | _ => true
    let semi := if needsSemi then ";" else ""
    s!"{pfx}{fmtExprAt ind e}{semi}"
  | .ifElse _ cond then_ else_ =>
    let thenStr := then_.map (fmtStmt (ind + 1))
    let elseStr := match else_ with
      | none => ""
      | some stmts => s!" else \{\n{"\n".intercalate (stmts.map (fmtStmt (ind + 1)))}\n{pfx}}"
    s!"{pfx}if {fmtExprAt ind cond} \{\n{"\n".intercalate thenStr}\n{pfx}}{elseStr}"
  | .while_ _ cond body label =>
    let lblStr := match label with | some l => s!"'{l}: " | none => ""
    s!"{pfx}{lblStr}while {fmtExprAt ind cond} \{\n{"\n".intercalate (body.map (fmtStmt (ind + 1)))}\n{pfx}}"
  | .forLoop _ init cond step body label =>
    let lblStr := match label with | some l => s!"'{l}: " | none => ""
    let initStr := match init with
      | some (.letDecl _ n m t v) =>
        let mutStr := if m then "mut " else ""
        let tyStr := match t with | some t => s!": {fmtTy t}" | none => ""
        s!"let {mutStr}{n}{tyStr} = {fmtExprAt ind v}"
      | some (.assign _ n v) => s!"{n} = {fmtExprAt ind v}"
      | some (.expr _ e) => fmtExprAt ind e
      | _ => ""
    let stepStr := match step with
      | some (.assign _ n v) => s!"{n} = {fmtExprAt ind v}"
      | some (.expr _ e) => fmtExprAt ind e
      | _ => ""
    s!"{pfx}{lblStr}for ({initStr}; {fmtExprAt ind cond}; {stepStr}) \{\n{"\n".intercalate (body.map (fmtStmt (ind + 1)))}\n{pfx}}"
  | .fieldAssign _ obj field value => s!"{pfx}{fmtExprAt ind obj}.{field} = {fmtExprAt ind value};"
  | .derefAssign _ target value => s!"{pfx}*{fmtExprAt ind target} = {fmtExprAt ind value};"
  | .arrayIndexAssign _ arr index value => s!"{pfx}{fmtExprAt ind arr}[{fmtExprAt ind index}] = {fmtExprAt ind value};"
  | .break_ _ (some v) label =>
    let lblStr := match label with | some l => s!" '{l}" | none => ""
    s!"{pfx}break{lblStr} {fmtExprAt ind v};"
  | .break_ _ none label =>
    let lblStr := match label with | some l => s!" '{l}" | none => ""
    s!"{pfx}break{lblStr};"
  | .continue_ _ label =>
    let lblStr := match label with | some l => s!" '{l}" | none => ""
    s!"{pfx}continue{lblStr};"
  | .defer _ body => s!"{pfx}defer {fmtExprAt ind body};"
  | .borrowIn _ var ref region isMut body =>
    let mutStr := if isMut then "mut " else ""
    s!"{pfx}borrow {mutStr}{var} as {ref} in {region} \{\n{"\n".intercalate (body.map (fmtStmt (ind + 1)))}\n{pfx}}"
  | .arrowAssign _ obj field value => s!"{pfx}{fmtExprAt ind obj}->{field} = {fmtExprAt ind value};"
  | .letDestructure _ enumName variant bindings value elseBody =>
    let bs := ", ".intercalate bindings
    let elseStr := match elseBody with
      | some body => s!" else \{\n{"\n".intercalate (body.map (fmtStmt (ind + 1)))}\n{pfx}}"
      | none => ""
    s!"{pfx}let {enumName}::{variant} \{ {bs} } = {fmtExprAt ind value}{elseStr};"
  | .letStructDestructure _ structName bindings value =>
    let bs := ", ".intercalate bindings
    s!"{pfx}let {structName} \{ {bs} } = {fmtExprAt ind value};"
end

/-- Format an expression (convenience wrapper, uses indent 0). -/
def fmtExpr (e : Expr) : String := fmtExprAt 0 e

-- ============================================================
-- Param formatting (handles self specially)
-- ============================================================

private def fmtParam (p : Param) : String :=
  if p.name == "self" then
    match p.ty with
    | .refMut _ => "&mut self"
    | .ref _ => "&self"
    | _ => "self"
  else s!"{p.name}: {fmtTy p.ty}"

-- ============================================================
-- Top-level definition formatting
-- ============================================================

def fmtImport (imp : ImportDecl) (ind : Nat) : String :=
  let pfx := indent ind
  let syms := imp.symbols.map fun s =>
    match s.alias with
    | some a => s!"{s.name} as {a}"
    | none => s.name
  s!"{pfx}import {imp.moduleName}.\{{", ".intercalate syms}};"

def fmtConstDef (c : ConstDef) (ind : Nat) : String :=
  let pfx := indent ind
  let pubStr := if c.isPublic then "pub " else ""
  s!"{pfx}{pubStr}const {c.name}: {fmtTy c.ty} = {fmtExpr c.value};"

def fmtTypeAlias (ta : TypeAlias) (ind : Nat) : String :=
  let pfx := indent ind
  let pubStr := if ta.isPublic then "pub " else ""
  s!"{pfx}{pubStr}type {ta.name} = {fmtTy ta.targetTy};"

def fmtExternFn (ext : ExternFnDecl) (ind : Nat) : String :=
  let pfx := indent ind
  let pubStr := if ext.isPublic then "pub " else ""
  let trustedStr := if ext.isTrusted then "trusted " else ""
  let paramsStr := ext.params.map fun p => s!"{p.name}: {fmtTy p.ty}"
  let retStr := if ext.retTy == .unit then "" else s!" -> {fmtTy ext.retTy}"
  s!"{pfx}{pubStr}{trustedStr}extern fn {ext.name}({", ".intercalate paramsStr}){retStr};"

def fmtNewtypeDef (nt : NewtypeDef) (ind : Nat) : String :=
  let pfx := indent ind
  let pubStr := if nt.isPublic then "pub " else ""
  let tparamsStr := fmtTypeParams nt.typeParams nt.typeBounds
  s!"{pfx}{pubStr}newtype {nt.name}{tparamsStr} = {fmtTy nt.innerTy};"

def fmtStructDef (s : StructDef) (ind : Nat) : String :=
  let pfx := indent ind
  let fpfx := indent (ind + 1)
  let pubStr := if s.isPublic then "pub " else ""
  let copyStr := if s.isCopy then "Copy " else ""
  let unionStr := if s.isUnion then "union " else "struct "
  let tparamsStr := fmtTypeParams s.typeParams s.typeBounds
  -- repr attributes
  let reprParts : List String := []
  let reprParts := if s.isReprC then reprParts ++ ["C"] else reprParts
  let reprParts := if s.isPacked then reprParts ++ ["packed"] else reprParts
  let reprParts := match s.reprAlign with
    | some n => reprParts ++ [s!"align({n})"]
    | none => reprParts
  let reprStr := if reprParts.isEmpty then "" else s!"{pfx}#[repr({", ".intercalate reprParts})]\n"
  let fields := s.fields.map fun f => s!"{fpfx}{f.name}: {fmtTy f.ty},"
  s!"{reprStr}{pfx}{pubStr}{unionStr}{copyStr}{s.name}{tparamsStr} \{\n{"\n".intercalate fields}\n{pfx}}"

def fmtEnumDef (e : EnumDef) (ind : Nat) : String :=
  let pfx := indent ind
  let vpfx := indent (ind + 1)
  let pubStr := if e.isPublic then "pub " else ""
  let tparamsStr := fmtTypeParams e.typeParams e.typeBounds
  let variants := e.variants.map fun v =>
    if v.fields.isEmpty then s!"{vpfx}{v.name},"
    else
      let fpfx := indent (ind + 2)
      let fs := v.fields.map fun f => s!"{fpfx}{f.name}: {fmtTy f.ty},"
      s!"{vpfx}{v.name} \{\n{"\n".intercalate fs}\n{vpfx}},"
  s!"{pfx}{pubStr}enum {e.name}{tparamsStr} \{\n{"\n".intercalate variants}\n{pfx}}"

def fmtFnDef (f : FnDef) (ind : Nat) : String :=
  let pfx := indent ind
  let pubStr := if f.isPublic then "pub " else ""
  let trustedStr := if f.isTrusted then "trusted " else ""
  let testStr := if f.isTest then s!"{pfx}#[test]\n" else ""
  let tparamsStr := fmtTypeParams f.typeParams f.typeBounds
  let paramsStr := f.params.map fmtParam
  let capStr := fmtCapSetTop f.capSet
  let retStr := if f.retTy == .unit then "" else s!" -> {fmtTy f.retTy}"
  let bodyStr := f.body.map (fmtStmt (ind + 1))
  s!"{testStr}{pfx}{pubStr}{trustedStr}fn {f.name}{tparamsStr}({", ".intercalate paramsStr}){capStr}{retStr} \{\n{"\n".intercalate bodyStr}\n{pfx}}"

def fmtTraitDef (t : TraitDef) (ind : Nat) : String :=
  let pfx := indent ind
  let mpfx := indent (ind + 1)
  let pubStr := if t.isPublic then "pub " else ""
  let tparamsStr := fmtTypeParams t.typeParams
  let methods := t.methods.map fun m =>
    let selfStr := match m.selfKind with
      | some .value => "self"
      | some .ref => "&self"
      | some .refMut => "&mut self"
      | none => ""
    let otherParams := m.params.map fun p => s!"{p.name}: {fmtTy p.ty}"
    let allParams := if selfStr.isEmpty then otherParams else [selfStr] ++ otherParams
    let capStr := fmtCapSetTop m.capSet
    let retStr := if m.retTy == .unit then "" else s!" -> {fmtTy m.retTy}"
    s!"{mpfx}fn {m.name}({", ".intercalate allParams}){capStr}{retStr};"
  s!"{pfx}{pubStr}trait {t.name}{tparamsStr} \{\n{"\n".intercalate methods}\n{pfx}}"

def fmtImplBlock (ib : ImplBlock) (ind : Nat) : String :=
  let pfx := indent ind
  let trustedStr := if ib.isTrusted then "trusted " else ""
  let tparamsStr := fmtTypeParams ib.typeParams
  -- Include type params on the type name: impl<T, E> Result<T, E> { ... }
  let typeNameArgs := if ib.typeParams.isEmpty then ""
    else s!"<{", ".intercalate ib.typeParams}>"
  let methods := ib.methods.map fun m => fmtFnDef m (ind + 1)
  s!"{pfx}{trustedStr}impl{tparamsStr} {ib.typeName}{typeNameArgs} \{\n{"\n\n".intercalate methods}\n{pfx}}"

def fmtImplTraitBlock (itb : ImplTraitBlock) (ind : Nat) : String :=
  let pfx := indent ind
  let trustedStr := if itb.isTrusted then "trusted " else ""
  let tparamsStr := fmtTypeParams itb.typeParams
  let typeNameArgs := if itb.typeParams.isEmpty then ""
    else s!"<{", ".intercalate itb.typeParams}>"
  let capStr := fmtCapSetTop itb.capSet
  let methods := itb.methods.map fun m => fmtFnDef m (ind + 1)
  s!"{pfx}{trustedStr}impl{tparamsStr} {itb.traitName} for {itb.typeName}{typeNameArgs}{capStr} \{\n{"\n\n".intercalate methods}\n{pfx}}"

-- ============================================================
-- Module formatting
-- ============================================================

mutual
partial def fmtModuleBody (m : Module) (ind : Nat) : String :=
  let groups : List (List String) := []
  let imps := m.imports.map fun i => fmtImport i ind
  let groups := if imps.isEmpty then groups else groups ++ [imps]
  let exts := m.externFns.map fun e => fmtExternFn e ind
  let groups := if exts.isEmpty then groups else groups ++ [exts]
  let aliases := m.typeAliases.map fun ta => fmtTypeAlias ta ind
  let groups := if aliases.isEmpty then groups else groups ++ [aliases]
  let nts := m.newtypes.map fun nt => fmtNewtypeDef nt ind
  let groups := if nts.isEmpty then groups else groups ++ [nts]
  let consts := m.constants.map fun c => fmtConstDef c ind
  let groups := if consts.isEmpty then groups else groups ++ [consts]
  let ss := m.structs.map fun s => fmtStructDef s ind
  let groups := if ss.isEmpty then groups else groups ++ [ss]
  let es := m.enums.map fun e => fmtEnumDef e ind
  let groups := if es.isEmpty then groups else groups ++ [es]
  let ts := m.traits.map fun t => fmtTraitDef t ind
  let groups := if ts.isEmpty then groups else groups ++ [ts]
  let ibs := m.implBlocks.map fun ib => fmtImplBlock ib ind
  let groups := if ibs.isEmpty then groups else groups ++ [ibs]
  let tis := m.traitImpls.map fun ti => fmtImplTraitBlock ti ind
  let groups := if tis.isEmpty then groups else groups ++ [tis]
  let fns := m.functions.map fun f => fmtFnDef f ind
  let groups := if fns.isEmpty then groups else groups ++ [fns]
  let subs := m.submodules.map fun sub => fmtModule sub ind
  let groups := if subs.isEmpty then groups else groups ++ [subs]
  "\n\n".intercalate (groups.map fun g =>
    let isCompact := match g with
      | s :: _ => let t := s.dropWhile (· == ' '); t.startsWith "import " || t.startsWith "extern "
      | [] => false
    if isCompact then "\n".intercalate g
    else "\n\n".intercalate g)

partial def fmtModule (m : Module) (ind : Nat) : String :=
  let pfx := indent ind
  let body := fmtModuleBody m (ind + 1)
  s!"{pfx}mod {m.name} \{\n{body}\n{pfx}}"
end

/-- Format a list of top-level modules.
    If the program is a single bare module named "main" (no explicit mod wrapper),
    output its contents directly without the mod wrapper. -/
def formatProgram (modules : List Module) : String :=
  match modules with
  | [m] =>
    if m.name == "main" then
      -- Bare file — output contents at indent 0 (no mod wrapper)
      fmtModuleBody m 0 ++ "\n"
    else
      fmtModule m 0 ++ "\n"
  | _ => "\n\n".intercalate (modules.map fun m => fmtModule m 0) ++ "\n"

end Concrete
