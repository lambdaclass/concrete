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
-/

mutual
partial def mapTyExpr (f : Ty → Ty) : Expr → Expr
  | .intLit sp v => .intLit sp v
  | .floatLit sp v => .floatLit sp v
  | .boolLit sp v => .boolLit sp v
  | .strLit sp v => .strLit sp v
  | .charLit sp v => .charLit sp v
  | .ident sp n => .ident sp n
  | .binOp sp op l r => .binOp sp op (mapTyExpr f l) (mapTyExpr f r)
  | .unaryOp sp op e => .unaryOp sp op (mapTyExpr f e)
  | .call sp fn tas args => .call sp fn (tas.map f) (args.map (mapTyExpr f))
  | .paren sp e => .paren sp (mapTyExpr f e)
  | .structLit sp n tas fields base =>
    .structLit sp n (tas.map f) (fields.map fun (k, e) => (k, mapTyExpr f e)) (base.map (mapTyExpr f))
  | .fieldAccess sp o fld => .fieldAccess sp (mapTyExpr f o) fld
  | .enumLit sp en v tas fields =>
    .enumLit sp en v (tas.map f) (fields.map fun (k, e) => (k, mapTyExpr f e))
  | .match_ sp s arms => .match_ sp (mapTyExpr f s) (arms.map (mapTyArm f))
  | .borrow sp e => .borrow sp (mapTyExpr f e)
  | .borrowMut sp e => .borrowMut sp (mapTyExpr f e)
  | .deref sp e => .deref sp (mapTyExpr f e)
  | .arrayLit sp es => .arrayLit sp (es.map (mapTyExpr f))
  | .arrayIndex sp a i => .arrayIndex sp (mapTyExpr f a) (mapTyExpr f i)
  | .cast sp e t => .cast sp (mapTyExpr f e) (f t)
  | .methodCall sp o m tas args => .methodCall sp (mapTyExpr f o) m (tas.map f) (args.map (mapTyExpr f))
  | .staticMethodCall sp tn m tas args => .staticMethodCall sp tn m (tas.map f) (args.map (mapTyExpr f))
  | .fnRef sp n => .fnRef sp n
  | .allocCall sp e a => .allocCall sp (mapTyExpr f e) (mapTyExpr f a)
  | .ifExpr sp c t e => .ifExpr sp (mapTyExpr f c) (t.map (mapTyStmt f)) (e.map (mapTyStmt f))

partial def mapTyArm (f : Ty → Ty) : MatchArm → MatchArm
  | .mk sp en v bs g body => .mk sp en v bs (g.map (mapTyExpr f)) (body.map (mapTyStmt f))
  | .litArm sp v g body => .litArm sp (mapTyExpr f v) (g.map (mapTyExpr f)) (body.map (mapTyStmt f))
  | .varArm sp b g body => .varArm sp b (g.map (mapTyExpr f)) (body.map (mapTyStmt f))
  | .rangeArm sp lo hi inc g body =>
    .rangeArm sp (mapTyExpr f lo) (mapTyExpr f hi) inc (g.map (mapTyExpr f)) (body.map (mapTyStmt f))

partial def mapTyStmt (f : Ty → Ty) : Stmt → Stmt
  | .letDecl sp n m t v g => .letDecl sp n m (t.map f) (mapTyExpr f v) g
  | .assign sp n v => .assign sp n (mapTyExpr f v)
  | .return_ sp v => .return_ sp (v.map (mapTyExpr f))
  | .expr sp e isV => .expr sp (mapTyExpr f e) isV
  | .ifElse sp c t e => .ifElse sp (mapTyExpr f c) (t.map (mapTyStmt f)) (e.map (·.map (mapTyStmt f)))
  | .while_ sp c b l => .while_ sp (mapTyExpr f c) (b.map (mapTyStmt f)) l
  | .forLoop sp i c s b l =>
    .forLoop sp (i.map (mapTyStmt f)) (mapTyExpr f c) (s.map (mapTyStmt f)) (b.map (mapTyStmt f)) l
  | .fieldAssign sp o fld v => .fieldAssign sp (mapTyExpr f o) fld (mapTyExpr f v)
  | .derefAssign sp t v => .derefAssign sp (mapTyExpr f t) (mapTyExpr f v)
  | .arrayIndexAssign sp a i v => .arrayIndexAssign sp (mapTyExpr f a) (mapTyExpr f i) (mapTyExpr f v)
  | .break_ sp v l => .break_ sp (v.map (mapTyExpr f)) l
  | .continue_ sp l => .continue_ sp l
  | .defer sp b => .defer sp (mapTyExpr f b)
  | .assert_ sp c => .assert_ sp (mapTyExpr f c)
  | .assume_ sp c => .assume_ sp (mapTyExpr f c)
  | .borrowIn sp v r rg m b => .borrowIn sp v r rg m (b.map (mapTyStmt f))
  | .letDestructure sp en v bs val el =>
    .letDestructure sp en v bs (mapTyExpr f val) (el.map (·.map (mapTyStmt f)))
  | .letStructDestructure sp sn bs val => .letStructDestructure sp sn bs (mapTyExpr f val)
end

def mapTyParam (f : Ty → Ty) (p : Param) : Param := { p with ty := f p.ty }

def mapTyFnDef (f : Ty → Ty) (fd : FnDef) : FnDef :=
  { fd with
    params := fd.params.map (mapTyParam f)
    retTy := f fd.retTy
    body := fd.body.map (mapTyStmt f)
    requires := fd.requires.map (mapTyExpr f)
    ensures := fd.ensures.map (mapTyExpr f)
    loopContracts := fd.loopContracts.map fun lc =>
      { lc with invariants := lc.invariants.map (mapTyExpr f)
                entrySubst := lc.entrySubst.map fun (k, e) => (k, mapTyExpr f e) } }

def mapTyFnSig (f : Ty → Ty) (s : FnSigDef) : FnSigDef :=
  { s with params := s.params.map (mapTyParam f), retTy := f s.retTy }

def mapTyFields (f : Ty → Ty) (fs : List StructField) : List StructField :=
  fs.map fun fld => { fld with ty := f fld.ty }

/-- Apply `f` to every type position in the module and its submodules. -/
partial def Module.mapTypes (f : Ty → Ty) (m : Module) : Module :=
  { m with
    structs := m.structs.map fun sd => { sd with fields := mapTyFields f sd.fields }
    enums := m.enums.map fun ed =>
      { ed with variants := ed.variants.map fun v => { v with fields := mapTyFields f v.fields } }
    functions := m.functions.map (mapTyFnDef f)
    implBlocks := m.implBlocks.map fun ib => { ib with methods := ib.methods.map (mapTyFnDef f) }
    traits := m.traits.map fun td => { td with methods := td.methods.map (mapTyFnSig f) }
    traitImpls := m.traitImpls.map fun tb => { tb with methods := tb.methods.map (mapTyFnDef f) }
    constants := m.constants.map fun c => { c with ty := f c.ty, value := mapTyExpr f c.value }
    typeAliases := m.typeAliases.map fun ta => { ta with targetTy := f ta.targetTy }
    externFns := m.externFns.map fun e => { e with params := e.params.map (mapTyParam f), retTy := f e.retTy }
    specFns := m.specFns.map fun sf =>
      { sf with params := sf.params.map (mapTyParam f), retTy := f sf.retTy, body := sf.body.map (mapTyExpr f) }
    newtypes := m.newtypes.map fun nt => { nt with innerTy := f nt.innerTy }
    submodules := m.submodules.map (Module.mapTypes f) }

end Concrete
