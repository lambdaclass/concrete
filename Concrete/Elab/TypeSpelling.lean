import Concrete.Elab.Core

namespace Concrete

/-! ## TypeSpelling — Core names a type by the definition it resolved to

Bug 096. Core, Layout, Mono, Lower, EmitSSA and the interpreter all identify a
struct, enum or newtype by its name: `Ty.named "P"`, `structLit "P"`, a flat
`structDefs.find? (·.name == "P")`. Two modules that each declared `struct P`
with different fields therefore shared ONE layout program-wide, and a module
importing the other `P` read its fields at the wrong offsets (measured: a
garbage `i64` where 7 was stored; `--interp`, which carries fields by name, gave
7).

Elab is the last stage that knows which declaration a spelling resolved to
(`StructDef.definedIn`/`definitionName`, stamped by `buildFileSummary`). So the
spelling is resolved there and Core carries the definition's canonical spelling:

  * the definition's own name when no other module declares a type of that name
    (the common case, so goldens and evidence over it do not move);
  * `<defining module path, '.' → '_'>_<name>` when two or more modules declare
    it — the same shape a submodule function's symbol has (`math_add`).

An import alias is resolved at the same point: `import c.{P as Q}` spells `c`'s
`P` in Core, not `Q`, so the alias can never meet another module's `Q`.

This module only RENAMES; deciding the spelling is Elab's job. Every Core
position that holds a type or a type's name goes through `CoreTypeRename`, so a
position cannot keep the old spelling by being forgotten here and remembered in
a consumer. -/

/-- Rename the type names a `Ty` mentions. Builtin shapes are untouched. -/
partial def renameTyNames (tn : String → String) : Ty → Ty
  | .named n => .named (tn n)
  | .generic n args caps => .generic (tn n) (args.map (renameTyNames tn)) caps
  | .ref t => .ref (renameTyNames tn t)
  | .refMut t => .refMut (renameTyNames tn t)
  | .heap t => .heap (renameTyNames tn t)
  | .heapArray t => .heapArray (renameTyNames tn t)
  | .array t n => .array (renameTyNames tn t) n
  | .ptrMut t => .ptrMut (renameTyNames tn t)
  | .ptrConst t => .ptrConst (renameTyNames tn t)
  | .fn_ ps cs ret => .fn_ (ps.map (renameTyNames tn)) cs (renameTyNames tn ret)
  | t => t

/-- What a Core rename changes: type names, and the names of global functions (direct
    callees, function references, and function values) that are renamed (`P_get` →
    `a_P_get`, bugs 096 - 098). Core spells a function value as `.ident` with a function
    type, as it spells a local of function type; `locals` are the names bound at the point
    being renamed (parameters, earlier `let`s of the enclosing blocks, match and borrow
    binders), which are never renamed. -/
structure CoreRename where
  tyName : String → String
  fnName : String → String
  locals : List String := []

namespace CoreRename

/-- The same rename with `params` shadowed: inside `fn f<P>(..)` the spelling
    `P` is the type parameter, not a struct that happens to share it. -/
def shadow (r : CoreRename) (params : List String) : CoreRename :=
  if params.isEmpty then r
  else { r with tyName := fun n => if params.contains n then n else r.tyName n }

def ty (r : CoreRename) : Ty → Ty := renameTyNames r.tyName

/-- The same rename with `names` bound as locals. -/
def bind (r : CoreRename) (names : List String) : CoreRename := { r with locals := names ++ r.locals }

mutual
partial def expr (r : CoreRename) : CExpr → CExpr
  | .intLit v t => .intLit v (r.ty t)
  | .floatLit v t => .floatLit v (r.ty t)
  | .boolLit b => .boolLit b
  | .strLit s => .strLit s
  | .charLit c => .charLit c
  | .ident n t =>
    -- A function VALUE is `.ident` with a function type; a local of function type is too,
    -- and keeps its name (bugs 050, 096 - 098).
    match t with
    | .fn_ .. => .ident (if r.locals.contains n then n else r.fnName n) (r.ty t)
    | _ => .ident n (r.ty t)
  | .binOp op l rh t => .binOp op (expr r l) (expr r rh) (r.ty t)
  | .unaryOp op e t => .unaryOp op (expr r e) (r.ty t)
  | .call callee targs args t =>
    let callee' := match callee with
      | .direct n => Callee.direct (r.fnName n)
      | .indirect b => Callee.indirect b
    .call callee' (targs.map r.ty) (args.map (expr r)) (r.ty t)
  | .structLit n targs fs t =>
    .structLit (r.tyName n) (targs.map r.ty) (fs.map fun (f, e) => (f, expr r e)) (r.ty t)
  | .fieldAccess obj f t => .fieldAccess (expr r obj) f (r.ty t)
  | .enumLit en v targs fs t =>
    .enumLit (r.tyName en) v (targs.map r.ty) (fs.map fun (f, e) => (f, expr r e)) (r.ty t)
  | .match_ s arms t => .match_ (expr r s) (arms.map (arm r)) (r.ty t)
  | .borrow e t => .borrow (expr r e) (r.ty t)
  | .borrowMut e t => .borrowMut (expr r e) (r.ty t)
  | .deref e t => .deref (expr r e) (r.ty t)
  | .arrayLit es t => .arrayLit (es.map (expr r)) (r.ty t)
  | .arrayIndex a i t => .arrayIndex (expr r a) (expr r i) (r.ty t)
  | .cast e t => .cast (expr r e) (r.ty t)
  | .fnRef n t => .fnRef (r.fnName n) (r.ty t)
  | .allocCall e a t => .allocCall (expr r e) (expr r a) (r.ty t)
  | .ifExpr c th el t => .ifExpr (expr r c) (stmts r th) (stmts r el) (r.ty t)

partial def arm (r : CoreRename) : CMatchArm → CMatchArm
  | .enumArm en v binds g body =>
    -- the arm's binders are in scope in its guard and body
    let r' := r.bind (binds.map (·.1))
    .enumArm (r.tyName en) v (binds.map fun (n, t) => (n, r.ty t)) (g.map (expr r')) (stmts r' body)
  | .litArm v g body => .litArm (expr r v) (g.map (expr r)) (stmts r body)
  | .varArm b t g body =>
    let r' := r.bind [b]
    .varArm b (r.ty t) (g.map (expr r')) (stmts r' body)
  | .rangeArm lo hi incl g body =>
    .rangeArm (expr r lo) (expr r hi) incl (g.map (expr r)) (stmts r body)

partial def stmt (r : CoreRename) : CStmt → CStmt
  | .letDecl n m t v => .letDecl n m (r.ty t) (expr r v)
  | .assign n v => .assign n (expr r v)
  | .return_ v t => .return_ (v.map (expr r)) (r.ty t)
  | .expr e iv => .expr (expr r e) iv
  | .ifElse c t el => .ifElse (expr r c) (stmts r t) (el.map (stmts r))
  | .while_ c body lbl step => .while_ (expr r c) (stmts r body) lbl (stmts r step)
  | .fieldAssign o f v => .fieldAssign (expr r o) f (expr r v)
  | .derefAssign t v => .derefAssign (expr r t) (expr r v)
  | .arrayIndexAssign a i v => .arrayIndexAssign (expr r a) (expr r i) (expr r v)
  | .break_ v lbl => .break_ (v.map (expr r)) lbl
  | .continue_ lbl => .continue_ lbl
  | .defer e => .defer (expr r e)
  | .borrowIn v rf reg isMut t body => .borrowIn v rf reg isMut (r.ty t) (stmts (r.bind [v, rf]) body)

/-- A block, scoped: a `let`'s binder is in scope AFTER its initializer, to the end of the
    block (`let h = h` reads the global `h`, then binds the local). -/
partial def stmts (r : CoreRename) (ss : List CStmt) : List CStmt :=
  (ss.foldl (fun (acc, r) st =>
    let st' := stmt r st
    match st with
    | .letDecl n .. => (st' :: acc, r.bind [n])
    | _ => (st' :: acc, r)) ([], r)).1.reverse
end

def fnDef (r : CoreRename) (f : CFnDef) : CFnDef :=
  let r' := (r.shadow f.typeParams).bind (f.params.map (·.1))
  let name' := r.fnName f.name
  { f with
    name := name'
    declName := if name' != f.name then some f.sourceName else f.declName
    params := f.params.map fun (n, t) => (n, r'.ty t)
    retTy := r'.ty f.retTy
    body := stmts r' f.body
    trustedImplOrigin := f.trustedImplOrigin.map r.tyName }

def structDef (r : CoreRename) (sd : CStructDef) : CStructDef :=
  let r' := r.shadow sd.typeParams
  { sd with name := r.tyName sd.name, fields := sd.fields.map fun (n, t) => (n, r'.ty t)
            nominalFields := sd.nominalFields.map fun (n, t) => (n, r'.ty t) }

def enumDef (r : CoreRename) (ed : CEnumDef) : CEnumDef :=
  -- A compiler-builtin enum is identified by `builtinId`, not by its spelling.
  if ed.builtinId.isSome then ed else
  let r' := r.shadow ed.typeParams
  { ed with
    name := r.tyName ed.name
    variants := ed.variants.map fun (v, fs) => (v, fs.map fun (n, t) => (n, r'.ty t))
    nominalVariants := ed.nominalVariants.map fun (v, fs) => (v, fs.map fun (n, t) => (n, r'.ty t)) }

def newtypeDef (r : CoreRename) (nt : NewtypeDef) : NewtypeDef :=
  let r' := r.shadow nt.typeParams
  { nt with name := r.tyName nt.name, innerTy := r'.ty nt.innerTy }

def traitDef (r : CoreRename) (td : CTraitDef) : CTraitDef :=
  { td with methods := td.methods.map fun ms =>
      { ms with retTy := r.ty ms.retTy, paramTys := ms.paramTys.map r.ty } }

/-- An impl's type parameters are `typeVar`s in every type it carries, which a rename
    never touches; only the implementing type's own name moves. -/
def traitImpl (r : CoreRename) (ti : CTraitImpl) : CTraitImpl :=
  { ti with
    typeName := r.tyName ti.typeName
    methodRetTys := ti.methodRetTys.map fun (n, t) => (n, r.ty t)
    selfTy := r.ty ti.selfTy
    methodSigs := ti.methodSigs.map fun ms => { ms with paramTys := ms.paramTys.map r.ty } }

end CoreRename

/-- Bug 096: every `.named`/`.generic` type name a type mentions. -/
partial def tyNamesIn : Ty → List String
  | .named n => [n]
  | .generic n args _ => n :: args.flatMap tyNamesIn
  | .ref t | .refMut t | .heap t | .heapArray t | .ptrMut t | .ptrConst t | .array t _ => tyNamesIn t
  | .fn_ ps _ r => ps.flatMap tyNamesIn ++ tyNamesIn r
  | _ => []

/-- Does a type mention a type variable? -/
partial def tyHasTypeVar : Ty → Bool
  | .typeVar _ => true
  | .generic _ args _ => args.any tyHasTypeVar
  | .ref t | .refMut t | .heap t | .heapArray t | .ptrMut t | .ptrConst t | .array t _ => tyHasTypeVar t
  | .fn_ ps _ r => ps.any tyHasTypeVar || tyHasTypeVar r
  | _ => false

/-- Bug 096: what the Core type-agreement walk reads at a point of a body. -/
structure TyAgreeCtx where
  /-- A global function by Core name (linker aliases resolved). -/
  fns : String → Option CFnDef
  /-- This module's copy of a struct / enum. -/
  structOf : String → Option CStructDef
  enumOf : String → Option CEnumDef
  /-- This module's newtypes (a constructor or `.0` is a cast between a newtype and its inner type). -/
  newtypeOf : String → Option NewtypeDef := fun _ => none
  /-- Every respelled type name (the names `famAgree` compares). -/
  family : List String := []
  /-- Names bound at this point (a `.ident` of one is the local, never a global function). -/
  locals : List String := []
  retTy : Option Ty := none
  /-- The enclosing function's type parameters. -/
  tps : List String := []

/-- A type position the body asserts: (declared, actual, where, wildcard type parameters). -/
abbrev TyAgreePair := Ty × Ty × String × List String

/-- A struct copy's field types as values carry them (nominal when recorded). -/
def structFieldsNominal (sd : CStructDef) : List (String × Ty) :=
  if sd.nominalFields.isEmpty then sd.fields else sd.nominalFields
/-- An enum copy's payload types as values carry them (nominal when recorded). -/
def enumVariantsNominal (ed : CEnumDef) : List (String × List (String × Ty)) :=
  if ed.nominalVariants.isEmpty then ed.variants else ed.nominalVariants

/-- The name of the struct a value of this type is (through one reference or heap layer). -/
def tyStructName : Ty → String
  | .named n | .generic n _ _ | .ref (.named n) | .refMut (.named n) | .heap (.named n)
  | .ref (.generic n _ _) | .refMut (.generic n _ _) | .heap (.generic n _ _) => n
  | _ => ""

/-- Bug 096: do a declared type and an actual type agree on every respelled (`fam`) type they
    name? Recursive: a type parameter (`wild`, or a type variable) on the declared side matches
    anything, while the nominal constructors around it are still compared. Positions of
    different shapes (an implicit borrow) compare the names they carry. -/
partial def famAgree (fam wild : List String) (exp act : Ty) : Bool :=
  let isWild : Ty → Bool := fun t => match t with
    | .typeVar _ => true
    | .named n => wild.contains n
    | _ => false
  let hasWild : Ty → Bool := fun t => tyHasTypeVar t || (tyNamesIn t).any wild.contains
  let famNames : Ty → List String := fun t => (tyNamesIn t).filter fam.contains
  let nameOk := fun (n m : String) => !(fam.contains n || fam.contains m) || n == m
  -- a diverging value (`abort()`) has every type, as Check admits it
  if isWild exp || act == .never then true else
  match exp, act with
  | .named n, .named m => nameOk n m
  | .generic n as _, .generic m bs _ =>
    nameOk n m && (as.length != bs.length || (as.zip bs).all fun (a, b) => famAgree fam wild a b)
  | .ref a, .ref b | .refMut a, .refMut b | .heap a, .heap b | .heapArray a, .heapArray b
  | .ptrMut a, .ptrMut b | .ptrConst a, .ptrConst b => famAgree fam wild a b
  | .array a _, .array b _ => famAgree fam wild a b
  | .fn_ ps _ r, .fn_ qs _ q =>
    (ps.length != qs.length || (ps.zip qs).all fun (a, b) => famAgree fam wild a b) && famAgree fam wild r q
  | _, _ => hasWild exp || famNames exp == famNames act

end Concrete
