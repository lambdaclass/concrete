import Concrete.Frontend.AST

namespace Concrete

/-! ## TypeMap — apply one function to every type a module mentions

One traversal over every position a `Ty` can appear in source: signatures, struct and
enum fields, aliases, constants, newtypes, externs, spec functions, and inside bodies
(`let` annotations, casts, and the type arguments of calls, literals and methods).

It exists so a whole-module type rewrite cannot quietly miss a position. Before it, the
only module-wide type rewrite (`Module.expandCapAliases`) reached signatures only, which
is right for capability aliases (they appear in `with(...)`) and wrong for anything that
can be written in a body. `f` is applied to each position's whole type; recursing into
that type is `f`'s job.

The same traversal optionally rewrites the name each call, function value and function
reference is spelled by (`g`, the identity by default): `Module.flattenQualifiedPaths` uses
it to give Check and Elab the flat spelling of a qualified path once Resolve has resolved it.
-/

mutual
partial def mapTyExpr (f : Ty → Ty) (callee : String → String := id) : Expr → Expr
  | .intLit sp v => .intLit sp v
  | .floatLit sp v => .floatLit sp v
  | .boolLit sp v => .boolLit sp v
  | .strLit sp v => .strLit sp v
  | .charLit sp v => .charLit sp v
  | .ident sp n => .ident sp (callee n)
  | .binOp sp op l r => .binOp sp op (mapTyExpr f callee l) (mapTyExpr f callee r)
  | .unaryOp sp op e => .unaryOp sp op (mapTyExpr f callee e)
  | .call sp fn tas args => .call sp (callee fn) (tas.map f) (args.map (mapTyExpr f callee))
  | .paren sp e => .paren sp (mapTyExpr f callee e)
  | .structLit sp n tas fields base =>
    .structLit sp n (tas.map f) (fields.map fun (k, e) => (k, mapTyExpr f callee e)) (base.map (mapTyExpr f callee))
  | .fieldAccess sp o fld => .fieldAccess sp (mapTyExpr f callee o) fld
  | .enumLit sp en v tas fields =>
    .enumLit sp en v (tas.map f) (fields.map fun (k, e) => (k, mapTyExpr f callee e))
  | .match_ sp s arms => .match_ sp (mapTyExpr f callee s) (arms.map (mapTyArm f callee))
  | .borrow sp e => .borrow sp (mapTyExpr f callee e)
  | .borrowMut sp e => .borrowMut sp (mapTyExpr f callee e)
  | .deref sp e => .deref sp (mapTyExpr f callee e)
  | .arrayLit sp es => .arrayLit sp (es.map (mapTyExpr f callee))
  | .arrayIndex sp a i => .arrayIndex sp (mapTyExpr f callee a) (mapTyExpr f callee i)
  | .cast sp e t => .cast sp (mapTyExpr f callee e) (f t)
  | .methodCall sp o m tas args => .methodCall sp (mapTyExpr f callee o) m (tas.map f) (args.map (mapTyExpr f callee))
  | .staticMethodCall sp tn m tas args => .staticMethodCall sp tn m (tas.map f) (args.map (mapTyExpr f callee))
  | .fnRef sp n => .fnRef sp (callee n)
  | .allocCall sp e a => .allocCall sp (mapTyExpr f callee e) (mapTyExpr f callee a)
  | .ifExpr sp c t e => .ifExpr sp (mapTyExpr f callee c) (t.map (mapTyStmt f callee)) (e.map (mapTyStmt f callee))

partial def mapTyArm (f : Ty → Ty) (callee : String → String := id) : MatchArm → MatchArm
  | .mk sp en v bs g body => .mk sp en v bs (g.map (mapTyExpr f callee)) (body.map (mapTyStmt f callee))
  | .litArm sp v g body => .litArm sp (mapTyExpr f callee v) (g.map (mapTyExpr f callee)) (body.map (mapTyStmt f callee))
  | .varArm sp b g body => .varArm sp b (g.map (mapTyExpr f callee)) (body.map (mapTyStmt f callee))
  | .rangeArm sp lo hi inc g body =>
    .rangeArm sp (mapTyExpr f callee lo) (mapTyExpr f callee hi) inc (g.map (mapTyExpr f callee)) (body.map (mapTyStmt f callee))

partial def mapTyStmt (f : Ty → Ty) (callee : String → String := id) : Stmt → Stmt
  | .letDecl sp n m t v g => .letDecl sp n m (t.map f) (mapTyExpr f callee v) g
  | .assign sp n v => .assign sp n (mapTyExpr f callee v)
  | .return_ sp v => .return_ sp (v.map (mapTyExpr f callee))
  | .expr sp e isV => .expr sp (mapTyExpr f callee e) isV
  | .ifElse sp c t e => .ifElse sp (mapTyExpr f callee c) (t.map (mapTyStmt f callee)) (e.map (·.map (mapTyStmt f callee)))
  | .while_ sp c b l => .while_ sp (mapTyExpr f callee c) (b.map (mapTyStmt f callee)) l
  | .forLoop sp i c s b l =>
    .forLoop sp (i.map (mapTyStmt f callee)) (mapTyExpr f callee c) (s.map (mapTyStmt f callee)) (b.map (mapTyStmt f callee)) l
  | .fieldAssign sp o fld v => .fieldAssign sp (mapTyExpr f callee o) fld (mapTyExpr f callee v)
  | .derefAssign sp t v => .derefAssign sp (mapTyExpr f callee t) (mapTyExpr f callee v)
  | .arrayIndexAssign sp a i v => .arrayIndexAssign sp (mapTyExpr f callee a) (mapTyExpr f callee i) (mapTyExpr f callee v)
  | .break_ sp v l => .break_ sp (v.map (mapTyExpr f callee)) l
  | .continue_ sp l => .continue_ sp l
  | .defer sp b => .defer sp (mapTyExpr f callee b)
  | .assert_ sp c => .assert_ sp (mapTyExpr f callee c)
  | .assume_ sp c => .assume_ sp (mapTyExpr f callee c)
  | .borrowIn sp v r rg m b => .borrowIn sp v r rg m (b.map (mapTyStmt f callee))
  | .letDestructure sp en v bs val el =>
    .letDestructure sp en v bs (mapTyExpr f callee val) (el.map (·.map (mapTyStmt f callee)))
  | .letStructDestructure sp sn bs val => .letStructDestructure sp sn bs (mapTyExpr f callee val)
end

def mapTyParam (f : Ty → Ty) (p : Param) : Param := { p with ty := f p.ty }

def mapTyFnDef (f : Ty → Ty) (fd : FnDef) (callee : String → String := id) : FnDef :=
  { fd with
    params := fd.params.map (mapTyParam f)
    retTy := f fd.retTy
    body := fd.body.map (mapTyStmt f callee)
    requires := fd.requires.map (mapTyExpr f callee)
    ensures := fd.ensures.map (mapTyExpr f callee)
    loopContracts := fd.loopContracts.map fun lc =>
      { lc with invariants := lc.invariants.map (mapTyExpr f callee)
                entrySubst := lc.entrySubst.map fun (k, e) => (k, mapTyExpr f callee e) } }

def mapTyFnSig (f : Ty → Ty) (s : FnSigDef) : FnSigDef :=
  { s with params := s.params.map (mapTyParam f), retTy := f s.retTy }

def mapTyFields (f : Ty → Ty) (fs : List StructField) : List StructField :=
  fs.map fun fld => { fld with ty := f fld.ty }

/-- Apply `f` to every type position in the module and its submodules. -/
partial def Module.mapTypes (f : Ty → Ty) (m : Module) (callee : String → String := id) : Module :=
  { m with
    structs := m.structs.map fun sd => { sd with fields := mapTyFields f sd.fields }
    enums := m.enums.map fun ed =>
      { ed with variants := ed.variants.map fun v => { v with fields := mapTyFields f v.fields } }
    functions := m.functions.map (fun fd => mapTyFnDef f fd callee)
    implBlocks := m.implBlocks.map fun ib => { ib with methods := ib.methods.map (fun fd => mapTyFnDef f fd callee) }
    traits := m.traits.map fun td => { td with methods := td.methods.map (mapTyFnSig f) }
    traitImpls := m.traitImpls.map fun tb => { tb with methods := tb.methods.map (fun fd => mapTyFnDef f fd callee) }
    constants := m.constants.map fun c => { c with ty := f c.ty, value := mapTyExpr f callee c.value }
    typeAliases := m.typeAliases.map fun ta => { ta with targetTy := f ta.targetTy }
    externFns := m.externFns.map fun e => { e with params := e.params.map (mapTyParam f), retTy := f e.retTy }
    specFns := m.specFns.map fun sf =>
      { sf with params := sf.params.map (mapTyParam f), retTy := f sf.retTy, body := sf.body.map (mapTyExpr f callee) }
    newtypes := m.newtypes.map fun nt => { nt with innerTy := f nt.innerTy }
    submodules := m.submodules.map (fun sm => sm.mapTypes f callee) }

/-- Bug 343 (MOD-1): the parser spells a qualified path `m::f` with its separator, a spelling no
    identifier can have, so Resolve looks it up among qualified paths only: a declaration, local or
    import spelled `m_f` cannot capture it, and an unqualified `m_f` cannot reach `m.f`. Check, Elab
    and the consumers below them still name a submodule function by its flat spelling `m_f`, so
    once Resolve has accepted the program (and refused every qualified path whose flat spelling
    names anything else, `checkQualifiedPath`), each qualified path is respelled flat here.
    Structural owner of the flat spelling: R-0492 (S5 `DefId`). -/
def Module.flattenQualifiedPaths (m : Module) : Module :=
  m.mapTypes id flattenQualifiedPath

end Concrete
