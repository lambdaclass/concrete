import Concrete.AST

namespace Concrete

/-! ## Core IR — typed, desugared intermediate representation

Surface forms removed:
- `paren(inner)` → structural, removed
- `obj.method(args)` → `call "Type_method" [&obj, args]`
- `Type::method(args)` → `call "Type_method" [args]`
- `p->field` → `deref(p).field`
- `p->field = val` → `deref(p).field = val`
- `for (init; cond; step) { body }` → `init; while cond { body; step }`
- `expr?` → kept as `try_` in Core v1
-/

-- ============================================================
-- Core IR Types
-- ============================================================

mutual
inductive CExpr where
  | intLit (val : Int) (ty : Ty)
  | floatLit (val : Float) (ty : Ty)
  | boolLit (val : Bool)
  | strLit (val : String)
  | charLit (val : Char)
  | ident (name : String) (ty : Ty)
  | binOp (op : BinOp) (lhs rhs : CExpr) (ty : Ty)
  | unaryOp (op : UnaryOp) (operand : CExpr) (ty : Ty)
  | call (fn : String) (typeArgs : List Ty) (args : List CExpr) (ty : Ty)
  | structLit (name : String) (typeArgs : List Ty) (fields : List (String × CExpr)) (ty : Ty)
  | fieldAccess (obj : CExpr) (field : String) (ty : Ty)
  | enumLit (enumName variant : String) (typeArgs : List Ty)
           (fields : List (String × CExpr)) (ty : Ty)
  | match_ (scrutinee : CExpr) (arms : List CMatchArm) (ty : Ty)
  | borrow (inner : CExpr) (ty : Ty)
  | borrowMut (inner : CExpr) (ty : Ty)
  | deref (inner : CExpr) (ty : Ty)
  | arrayLit (elems : List CExpr) (ty : Ty)
  | arrayIndex (arr : CExpr) (index : CExpr) (ty : Ty)
  | cast (inner : CExpr) (targetTy : Ty)
  | fnRef (name : String) (ty : Ty)
  | try_ (inner : CExpr) (ty : Ty)
  | allocCall (inner : CExpr) (allocExpr : CExpr) (ty : Ty)
  | whileExpr (cond : CExpr) (body : List CStmt) (elseBody : List CStmt) (ty : Ty)
  | ifExpr (cond : CExpr) (then_ : List CStmt) (else_ : List CStmt) (ty : Ty)

inductive CMatchArm where
  | enumArm (enumName variant : String) (bindings : List (String × Ty)) (body : List CStmt)
  | litArm (value : CExpr) (body : List CStmt)
  | varArm (binding : String) (bindTy : Ty) (body : List CStmt)

inductive CStmt where
  | letDecl (name : String) (mutable : Bool) (ty : Ty) (value : CExpr)
  | assign (name : String) (value : CExpr)
  | return_ (value : Option CExpr) (retTy : Ty)
  | expr (e : CExpr)
  | ifElse (cond : CExpr) (then_ : List CStmt) (else_ : Option (List CStmt))
  | while_ (cond : CExpr) (body : List CStmt) (label : Option String)
           (step : List CStmt)
  | fieldAssign (obj : CExpr) (field : String) (value : CExpr)
  | derefAssign (target : CExpr) (value : CExpr)
  | arrayIndexAssign (arr : CExpr) (index : CExpr) (value : CExpr)
  | break_ (value : Option CExpr) (label : Option String)
  | continue_ (label : Option String)
  | defer (body : CExpr)
  | borrowIn (var : String) (ref : String) (region : String)
             (isMut : Bool) (refTy : Ty) (body : List CStmt)
end

-- ============================================================
-- Top-level definitions
-- ============================================================

structure CFnDef where
  name : String
  typeParams : List String := []
  params : List (String × Ty)
  retTy : Ty
  body : List CStmt
  isPublic : Bool := false
  isTest : Bool := false
  isTrusted : Bool := false
  isEntryPoint : Bool := false  -- tagged by Elab when name == mainFnName
  trustedImplOrigin : Option String := none  -- "TypeName" if from a trusted impl/trait-impl
  capSet : CapSet := .empty

structure CStructDef where
  name : String
  typeParams : List String := []
  fields : List (String × Ty)
  isPublic : Bool := false
  isCopy : Bool := false
  isReprC : Bool := false
  isPacked : Bool := false
  reprAlign : Option Nat := none

structure CEnumDef where
  name : String
  typeParams : List String := []
  variants : List (String × List (String × Ty))
  isPublic : Bool := false
  isCopy : Bool := false
  builtinId : Option BuiltinEnumId := none

structure CTraitMethodSig where
  name : String
  retTy : Ty

structure CTraitDef where
  name : String
  methods : List CTraitMethodSig
  builtinId : Option BuiltinTraitId := none

structure CTraitImpl where
  traitName : String
  typeName : String
  methodNames : List String
  methodRetTys : List (String × Ty)
  builtinTraitId : Option BuiltinTraitId := none

structure CModule where
  name : String
  structs : List CStructDef
  enums : List CEnumDef
  functions : List CFnDef
  externFns : List (String × List (String × Ty) × Ty × Bool)  -- (name, params, retTy, isTrusted)
  constants : List (String × Ty × CExpr)
  submodules : List CModule := []
  traitDefs : List CTraitDef := []
  traitImpls : List CTraitImpl := []
  /-- Maps local alias name → original linker symbol for aliased imports. -/
  linkerAliases : List (String × String) := []
  /-- Newtype definitions from this module. Layout resolves these so that
      newtype names reaching SSA/codegen are transparently unwrapped. Elab
      erases newtypes inside struct/enum fields, but function bodies still
      carry the wrapper names (e.g. `Option<Port>`), so Layout needs them. -/
  newtypes : List NewtypeDef := []

-- ============================================================
-- CExpr.ty accessor
-- ============================================================

def CExpr.ty : CExpr → Ty
  | .intLit _ t => t
  | .floatLit _ t => t
  | .boolLit _ => .bool
  | .strLit _ => .string
  | .charLit _ => .char
  | .ident _ t => t
  | .binOp _ _ _ t => t
  | .unaryOp _ _ t => t
  | .call _ _ _ t => t
  | .structLit _ _ _ t => t
  | .fieldAccess _ _ t => t
  | .enumLit _ _ _ _ t => t
  | .match_ _ _ t => t
  | .borrow _ t => t
  | .borrowMut _ t => t
  | .deref _ t => t
  | .arrayLit _ t => t
  | .arrayIndex _ _ t => t
  | .cast _ t => t
  | .fnRef _ t => t
  | .try_ _ t => t
  | .allocCall _ _ t => t
  | .whileExpr _ _ _ t => t
  | .ifExpr _ _ _ t => t

-- ============================================================
-- Pretty-printer
-- ============================================================

def tyToStr : Ty → String
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
  | .unit => "()"
  | .named n => n
  | .string => "String"
  | .ref inner => s!"&{tyToStr inner}"
  | .refMut inner => s!"&mut {tyToStr inner}"
  | .generic n args => s!"{n}<{", ".intercalate (args.map tyToStr)}>"
  | .typeVar n => n
  | .array elem size => s!"[{tyToStr elem}; {size}]"
  | .ptrMut inner => s!"*mut {tyToStr inner}"
  | .ptrConst inner => s!"*const {tyToStr inner}"
  | .fn_ params cs ret =>
    let capsStr := match cs with
      | .empty => ""
      | _ => " with(...)"
    s!"fn({", ".intercalate (params.map tyToStr)}){capsStr} -> {tyToStr ret}"
  | .never => "!"
  | .heap inner => s!"Heap<{tyToStr inner}>"
  | .heapArray inner => s!"HeapArray<{tyToStr inner}>"
  | .placeholder => "_"

private def indent (n : Nat) : String := String.ofList (List.replicate (n * 2) ' ')

private def binOpToStr : BinOp → String
  | .add => "+" | .sub => "-" | .mul => "*" | .div => "/" | .mod => "%"
  | .eq => "==" | .neq => "!=" | .lt => "<" | .gt => ">" | .leq => "<=" | .geq => ">="
  | .and_ => "&&" | .or_ => "||"
  | .bitand => "&" | .bitor => "|" | .bitxor => "^" | .shl => "<<" | .shr => ">>"

private def unaryOpToStr : UnaryOp → String
  | .neg => "-" | .not_ => "!" | .bitnot => "~"

mutual
partial def ppCExpr (e : CExpr) : String :=
  match e with
  | .intLit v _ => toString v
  | .floatLit v _ => toString v
  | .boolLit b => toString b
  | .strLit s => s!"\"{s}\""
  | .charLit c => s!"'{c}'"
  | .ident n _ => n
  | .binOp op l r _ => s!"({ppCExpr l} {binOpToStr op} {ppCExpr r})"
  | .unaryOp op e _ => s!"{unaryOpToStr op}{ppCExpr e}"
  | .call fn targs args _ =>
    let targsStr := if targs.isEmpty then "" else s!"<{", ".intercalate (targs.map tyToStr)}>"
    s!"{fn}{targsStr}({", ".intercalate (args.map ppCExpr)})"
  | .structLit n targs fields _ =>
    let targsStr := if targs.isEmpty then "" else s!"<{", ".intercalate (targs.map tyToStr)}>"
    let fs := fields.map fun (k, v) => s!"{k}: {ppCExpr v}"
    s!"{n}{targsStr} \{ {", ".intercalate fs} }"
  | .fieldAccess obj f _ => s!"{ppCExpr obj}.{f}"
  | .enumLit en v targs fields _ =>
    let targsStr := if targs.isEmpty then "" else s!"<{", ".intercalate (targs.map tyToStr)}>"
    if fields.isEmpty then s!"{en}::{v}{targsStr}"
    else
      let fs := fields.map fun (k, val) => s!"{k}: {ppCExpr val}"
      s!"{en}::{v}{targsStr} \{ {", ".intercalate fs} }"
  | .match_ scrut arms _ =>
    let armsStr := arms.map ppCMatchArm
    s!"match {ppCExpr scrut} \{\n{"\n".intercalate armsStr}\n}"
  | .borrow inner _ => s!"&{ppCExpr inner}"
  | .borrowMut inner _ => s!"&mut {ppCExpr inner}"
  | .deref inner _ => s!"*{ppCExpr inner}"
  | .arrayLit elems _ => s!"[{", ".intercalate (elems.map ppCExpr)}]"
  | .arrayIndex arr idx _ => s!"{ppCExpr arr}[{ppCExpr idx}]"
  | .cast inner t => s!"{ppCExpr inner} as {tyToStr t}"
  | .fnRef n _ => n
  | .try_ inner _ => s!"{ppCExpr inner}?"
  | .allocCall inner alloc _ => s!"{ppCExpr inner} with(Alloc = {ppCExpr alloc})"
  | .whileExpr cond body elseBody _ =>
    let bodyStr := body.map (ppCStmt 2)
    let elseStr := if elseBody.isEmpty then "" else s!" else \{\n{"\n".intercalate (elseBody.map (ppCStmt 2))}\n  }"
    s!"while {ppCExpr cond} \{\n{"\n".intercalate bodyStr}\n  }{elseStr}"
  | .ifExpr cond then_ else_ _ =>
    let thenStr := then_.map (ppCStmt 2)
    let elseStr := else_.map (ppCStmt 2)
    s!"if {ppCExpr cond} \{\n{"\n".intercalate thenStr}\n  } else \{\n{"\n".intercalate elseStr}\n  }"

partial def ppCMatchArm (arm : CMatchArm) : String :=
  match arm with
  | .enumArm en v binds body =>
    let bindsStr := if binds.isEmpty then ""
                    else s!" \{ {", ".intercalate (binds.map fun (n, t) => s!"{n}: {tyToStr t}")} }"
    s!"  {en}::{v}{bindsStr} -> \{\n{"\n".intercalate (body.map (ppCStmt 3))}\n  }"
  | .litArm val body =>
    s!"  {ppCExpr val} -> \{\n{"\n".intercalate (body.map (ppCStmt 3))}\n  }"
  | .varArm b _ body =>
    s!"  {b} -> \{\n{"\n".intercalate (body.map (ppCStmt 3))}\n  }"

partial def ppCStmt (ind : Nat) (s : CStmt) : String :=
  let pfx := indent ind
  match s with
  | .letDecl n m t v =>
    let mutStr := if m then "mut " else ""
    s!"{pfx}let {mutStr}{n}: {tyToStr t} = {ppCExpr v};"
  | .assign n v => s!"{pfx}{n} = {ppCExpr v};"
  | .return_ (some v) _ => s!"{pfx}return {ppCExpr v};"
  | .return_ none _ => s!"{pfx}return;"
  | .expr e => s!"{pfx}{ppCExpr e};"
  | .ifElse c t el =>
    let thenStr := t.map (ppCStmt (ind + 1))
    let elseStr := match el with
      | none => ""
      | some stmts => s!" else \{\n{"\n".intercalate (stmts.map (ppCStmt (ind + 1)))}\n{pfx}}"
    s!"{pfx}if {ppCExpr c} \{\n{"\n".intercalate thenStr}\n{pfx}}{elseStr}"
  | .while_ c body lbl _ =>
    let lblStr := match lbl with | some l => s!"'{l}: " | none => ""
    s!"{pfx}{lblStr}while {ppCExpr c} \{\n{"\n".intercalate (body.map (ppCStmt (ind + 1)))}\n{pfx}}"
  | .fieldAssign obj f v => s!"{pfx}{ppCExpr obj}.{f} = {ppCExpr v};"
  | .derefAssign t v => s!"{pfx}*{ppCExpr t} = {ppCExpr v};"
  | .arrayIndexAssign arr idx v => s!"{pfx}{ppCExpr arr}[{ppCExpr idx}] = {ppCExpr v};"
  | .break_ (some v) lbl =>
    let lblStr := match lbl with | some l => s!" '{l}" | none => ""
    s!"{pfx}break{lblStr} {ppCExpr v};"
  | .break_ none lbl =>
    let lblStr := match lbl with | some l => s!" '{l}" | none => ""
    s!"{pfx}break{lblStr};"
  | .continue_ lbl =>
    let lblStr := match lbl with | some l => s!" '{l}" | none => ""
    s!"{pfx}continue{lblStr};"
  | .defer body => s!"{pfx}defer {ppCExpr body};"
  | .borrowIn v r reg isMut _ body =>
    let mutStr := if isMut then "mut " else ""
    s!"{pfx}borrow {mutStr}{r} = &{v} in '{reg} \{\n{"\n".intercalate (body.map (ppCStmt (ind + 1)))}\n{pfx}}"
end

def ppCFnDef (f : CFnDef) : String :=
  let pubStr := if f.isPublic then "pub " else ""
  let pubStr := if f.isTrusted then pubStr ++ "trusted " else pubStr
  let tparamsStr := if f.typeParams.isEmpty then "" else s!"<{", ".intercalate f.typeParams}>"
  let paramsStr := f.params.map fun (n, t) => s!"{n}: {tyToStr t}"
  let capStr := match f.capSet with
    | .empty => ""
    | .concrete caps => s!" with({", ".intercalate caps})"
    | _ => ""
  let bodyStr := f.body.map (ppCStmt 1)
  s!"{pubStr}fn {f.name}{tparamsStr}({", ".intercalate paramsStr}) -> {tyToStr f.retTy}{capStr} \{\n{"\n".intercalate bodyStr}\n}"

partial def ppCModule (m : CModule) : String :=
  let parts : List String := []
  let parts := parts ++ m.structs.map fun s =>
    let tparamsStr := if s.typeParams.isEmpty then "" else s!"<{", ".intercalate s.typeParams}>"
    let copyStr := if s.isCopy then " copy" else ""
    let fields := s.fields.map fun (n, t) => s!"  {n}: {tyToStr t},"
    s!"struct {s.name}{tparamsStr}{copyStr} \{\n{"\n".intercalate fields}\n}"
  let parts := parts ++ m.enums.map fun e =>
    let tparamsStr := if e.typeParams.isEmpty then "" else s!"<{", ".intercalate e.typeParams}>"
    let variants := e.variants.map fun (vn, fields) =>
      if fields.isEmpty then s!"  {vn},"
      else
        let fs := fields.map fun (fn, ft) => s!"{fn}: {tyToStr ft}"
        s!"  {vn} \{ {", ".intercalate fs} },"
    s!"enum {e.name}{tparamsStr} \{\n{"\n".intercalate variants}\n}"
  let parts := parts ++ m.externFns.map fun (n, ps, rt, trusted) =>
    let paramsStr := ps.map fun (pn, pt) => s!"{pn}: {tyToStr pt}"
    let kw := if trusted then "trusted extern fn" else "extern fn"
    s!"{kw} {n}({", ".intercalate paramsStr}) -> {tyToStr rt};"
  let parts := parts ++ m.functions.map ppCFnDef
  let parts := parts ++ m.submodules.map fun sub =>
    s!"mod {sub.name} \{\n{ppCModule sub}\n}"
  s!"// module {m.name}\n{"\n\n".intercalate parts}"

end Concrete
