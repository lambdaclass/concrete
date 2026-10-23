import Concrete.Frontend.AST
import Concrete.Report.Diagnostic
import Concrete.Resolve.FileSummary
import Concrete.Resolve.Intrinsic
import Concrete.Resolve.TypeId
import Concrete.Resolve.CallableId
import Concrete.Resolve.Resolve

/-! # Trait coherence (bug 186) — impl selection is a function, not a search

Before this pass, trait validation asked only whether an impl's trait exists and whether its
methods match the trait. Nothing asked whether the impl was the ONLY one, so impl selection was a
search whose answer depended on declaration order:

* two `impl Tr for Foo` blocks both entered the tables; native code failed at `llvm-as`
  ("invalid redefinition") and the interpreter refused at run time;
* `impl Tr for Box<i32>` dropped its target's type arguments at parse, so it silently implemented
  `Tr` for every `Box<_>` (a `Box<bool>` satisfied the bound and ran the `i32` body);
* `impl<T> Tr for Box<T>` beside `impl Tr for Box<i32>` compiled natively to whichever body was
  emitted first;
* two traits of one type declaring the same method name collided on the method's `Type_method`
  spelling: the interpreter picked by declaration order, native code failed at `llvm-as`.

This pass runs over the WHOLE program before Check builds any method table, and refuses each of
these. Identity is typed: a trait is `TraitId` (compiler builtin, or defining module + name), an
impl target is `ImplHead` (a `TypeId` minted from the declaration it resolves to, never from the
spelling at the impl site), and a method is a `MethodKey` (owner, target, name).

The rules (docs/language/TRAITS.md, rule IDs TC-1 … TC-6):

* **TC-1 (E0590)** one trait has at most one impl for one type.
* **TC-2 (E0591)** no two impls of one trait have unifiable targets.
* **TC-3 (E0592)** instance locality (Austral's rule, at module granularity): an impl of trait `Tr`
  for type `X` lives in the module that defines `Tr` or the module that defines `X`. The compiler's
  builtin `Destroy` is defined in no module, so its impls live with their type.
* **TC-4 (E0593)** an impl's target is its type's declaration applied to exactly the impl's own
  parameters, in order: no specialized arguments (`Box<i32>`), no blanket target (`impl<T> Tr for
  T`), no alias, no missing or extra argument. Every impl therefore covers every instantiation of
  its type, which is what makes TC-1 and TC-2 decidable from the type's identity alone.
* **TC-5 (E0594)** one type has at most one method of a given name across its inherent impls and
  its trait impls, so a method call on a known type has exactly one candidate.
* **TC-6 (E0595, Check)** a call through a type parameter's bounds is refused when more than one
  bound trait declares the method. Order never decides.

TC-3 makes TC-1/TC-2 module-local in principle (Austral's argument: uniqueness never needs the
whole program); the pass still compares every impl in the program, so a violation is reported
even when the locality rule is the one broken.
-/

namespace Concrete

namespace Coherence

/-- A trait, by what it IS. User traits are module-local (they are not importable), so the
    defining module plus the name is unique. -/
inductive TraitId where
  | builtin (id : BuiltinTraitId)
  | user (defModule name : String)
  deriving BEq, Repr

def TraitId.display : TraitId → String
  | .builtin .destroy => destroyTraitName
  | .user m n => if m.isEmpty then n else s!"{m}::{n}"

/-- What an impl's target names. `decl` is the resolved declaration; `prim` a compiler type
    with no source declaration (`Int`, `u8`); `unresolved` a name no declaration answers (its
    own pass reports it), kept so it is still compared with itself. -/
inductive ImplHead where
  | decl (id : TypeId)
  | prim (name : String)
  | unresolved (inModule name : String)
  deriving BEq, Repr

def ImplHead.key : ImplHead → String
  | .decl id => id.render
  | .prim n => "prim:" ++ n
  | .unresolved m n => "unresolved:" ++ m ++ ":" ++ n

/-- Who provides a method: an inherent impl, or the impl of one trait. -/
inductive ImplOwner where
  | inherent
  | trait (t : TraitId)
  deriving BEq, Repr

def ImplOwner.display : ImplOwner → String
  | .inherent => "an inherent impl"
  | .trait t => s!"impl {t.display}"

/-- A method's identity: (owner, target type, method name). TC-5 makes `(target, name)` alone
    determine the owner, which is why the derived `Type_method` spelling cannot collide within
    one type. -/
structure MethodKey where
  owner : ImplOwner
  target : ImplHead
  method : String
  deriving BEq, Repr

/-- A target as a first-order term: the impl's own parameters are variables. -/
inductive Term where
  | var (n : String)
  | app (head : String) (args : List Term)
  deriving BEq, Repr, Inhabited

/-- One impl, as coherence sees it. -/
structure ImplInfo where
  owner : ImplOwner
  head : Option ImplHead      -- `none`: the target is a type parameter (blanket)
  target : Term
  typeName : String           -- the spelling at the impl site, for messages
  module : String
  span : Span
  file : String
  methods : List String

/-- A declaration a type name can resolve to: its identity and how many arguments it takes. -/
structure TypeDecl where
  id : TypeId
  arity : Nat

private def structDecl (sd : StructDef) (inModule : String) : TypeDecl :=
  let defIn := if sd.definedIn.isEmpty then inModule else sd.definedIn
  let defName := if sd.definitionName.isEmpty then sd.name else sd.definitionName
  { id := .user defIn defName, arity := sd.typeParams.length + sd.capParams.length }

private def enumDecl (ed : EnumDef) (inModule : String) : TypeDecl :=
  match ed.builtinId with
  | some b => { id := .builtin b, arity := ed.typeParams.length }
  | none =>
    let defIn := if ed.definedIn.isEmpty then inModule else ed.definedIn
    let defName := if ed.definitionName.isEmpty then ed.name else ed.definitionName
    { id := .user defIn defName, arity := ed.typeParams.length }

private def newtypeDecl (nt : NewtypeDef) (inModule : String) : TypeDecl :=
  let defIn := if nt.definedIn.isEmpty then inModule else nt.definedIn
  { id := .user defIn nt.name, arity := nt.typeParams.length }

/-- Find a declaration named `n` in one module summary (its own declarations, by their
    definition-site name). -/
private def declIn (s : FileSummary) (n : String) (inModule : String) : Option TypeDecl :=
  match s.structs.find? (·.name == n) with
  | some sd => some (structDecl sd inModule)
  | none =>
    match s.enums.find? (·.name == n) with
    | some ed => some (enumDecl ed inModule)
    | none => (s.newtypes.find? (·.name == n)).map (newtypeDecl · inModule)

/-- Primitive and compiler type names with no source declaration. -/
private def primNames : List String :=
  [ "Int", "Uint", "Bool", "Float64", "Float32", "Char",
    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "Heap", "HeapArray" ]

/-- The module scope a target name is resolved in. -/
structure Scope where
  path : String
  module : Module
  summaryTable : List (String × FileSummary)

/-- Resolve the name at an impl site to the declaration it denotes: the module's own
    declarations, then its imports (by the IMPORTED declaration's identity, so `import
    m.{Foo as Bar}; impl Destroy for Bar` names `m`'s `Foo`), then the always-visible std types
    (`String`, `Vec`, …), then primitives. -/
def resolveHead (sc : Scope) (n : String) : Option TypeDecl ⊕ ImplHead :=
  let m := sc.module
  match m.structs.find? (·.name == n) with
  | some sd => .inl (some (structDecl { sd with definedIn := sc.path, definitionName := sd.name } sc.path))
  | none =>
  match m.enums.find? (·.name == n) with
  | some ed => .inl (some (enumDecl { ed with definedIn := sc.path, definitionName := ed.name } sc.path))
  | none =>
  match m.newtypes.find? (·.name == n) with
  | some nt => .inl (some (newtypeDecl { nt with definedIn := sc.path } sc.path))
  | none =>
  let imported := m.imports.findSome? fun imp =>
    (imp.symbols.find? (·.effectiveName == n)).bind fun sym =>
      (sc.summaryTable.lookup imp.moduleName).bind fun s => declIn s sym.name imp.moduleName
  match imported with
  | some d => .inl (some d)
  | none =>
  if builtinTypeNames.contains n && !primNames.contains n then
    match sc.summaryTable.findSome? fun (k, s) => declIn s n k with
    | some d => .inl (some d)
    | none => .inr (.prim n)
  else if primNames.contains n then .inr (.prim n)
  else .inr (.unresolved sc.path n)

/-- `Ty` → `Term`, with the impl's own parameters as variables. Nested heads are compared by
    spelling; TC-4 refuses every target whose arguments are not bare parameters, so a nested
    head never decides a well-formed program. -/
partial def tyTerm (params : List String) : Ty → Term
  | .typeVar n => if params.contains n then .var n else .app ("'" ++ n) []
  | .named n => if params.contains n then .var n else .app n []
  | .generic n args _ => .app n (args.map (tyTerm params))
  | t => .app (tyCanonical t) []

/-- Rename every variable apart (`tag`). -/
partial def Term.tag (tag : String) : Term → Term
  | .var n => .var (tag ++ n)
  | .app h as => .app h (as.map (Term.tag tag))

partial def Term.subst (s : List (String × Term)) : Term → Term
  | .var n => match s.lookup n with
    | some t => Term.subst s t
    | none => .var n
  | .app h as => .app h (as.map (Term.subst s))

partial def Term.occurs (n : String) : Term → Bool
  | .var m => n == m
  | .app _ as => as.any (Term.occurs n)

/-- First-order unification (with occurs check). -/
partial def unify (s : List (String × Term)) : List (Term × Term) → Option (List (String × Term))
  | [] => some s
  | (a, b) :: rest =>
    let a := a.subst s
    let b := b.subst s
    match a, b with
    | .var x, .var y => if x == y then unify s rest else unify ((x, .var y) :: s) rest
    | .var x, t | t, .var x =>
      if t.occurs x then none else unify ((x, t) :: s) rest
    | .app h1 as1, .app h2 as2 =>
      if h1 != h2 || as1.length != as2.length then none
      else unify s (as1.zip as2 ++ rest)

/-- Canonical form: variables renamed by first occurrence, so two targets equal up to the
    naming of the impl's parameters compare equal. -/
partial def Term.canon (t : Term) : Term :=
  let rec go (seen : List String) : Term → Term × List String
    | .var n =>
      match seen.idxOf? n with
      | some i => (.var s!"v{i}", seen)
      | none => (.var s!"v{seen.length}", seen ++ [n])
    | .app h as =>
      let (as', seen') := as.foldl (fun (acc, sn) a =>
        let (a', sn') := go sn a
        (acc ++ [a'], sn')) (([] : List Term), seen)
      (.app h as', seen')
  (go [] t).1

def mkDiag (code msg : String) (span : Span) (file : String) (hint : Option String := none) : Diagnostic :=
  { severity := .error, message := msg, pass := "check", span := some span, hint := hint,
    code := code, file := file }

/-- The judgment over one module: collect its impls (with identities) and its TC-3/TC-4
    refusals. Submodules are walked by the caller. -/
def moduleImpls (sc : Scope) (file : String) : List ImplInfo × Diagnostics := Id.run do
  let m := sc.module
  let localTraits := m.traits.map (·.name)
  let mut infos : List ImplInfo := []
  let mut ds : Diagnostics := []
  -- Shape (TC-4): the target is the head applied to exactly `expected`.
  let shapeCheck := fun (what typeName : String) (typeParams expected : List String)
      (targetArgs : Option (List Ty)) (decl : Option TypeDecl) =>
    let args := targetArgs.getD []
    let argNames := args.map fun
      | .named n => some n
      | .typeVar n => some n
      | _ => none
    let isAlias := m.typeAliases.any (·.name == typeName)
    if typeParams.contains typeName then
      some s!"{what} targets its own type parameter '{typeName}': a blanket impl would cover every type, and impl selection would no longer be decided by the type's identity"
    else if isAlias then
      some s!"{what} targets the type alias '{typeName}'; write the type it names"
    else if argNames != expected.map some then
      let shown := if args.isEmpty then typeName
        else s!"{typeName}<{", ".intercalate (args.map tyCanonical)}>"
      let want := if expected.isEmpty then typeName
        else s!"{typeName}<{", ".intercalate expected}>"
      some s!"{what} targets '{shown}', but an impl's target must be its type applied to exactly the impl's own parameters, in order ('{want}'): a specialized impl would be taken for every instantiation of '{typeName}'"
    else match decl with
      | some d =>
        if d.arity != expected.length then
          some s!"{what} gives '{typeName}' {expected.length} argument(s), but its declaration takes {d.arity}"
        else none
      | none => none
  for ti in m.traitImpls do
    let traitId? : Option TraitId :=
      if ti.traitName == destroyTraitName then some (.builtin .destroy)
      else if localTraits.contains ti.traitName then some (.user sc.path ti.traitName)
      else none   -- unknown trait: CoreCheck's E0580 owns it
    match traitId? with
    | none => pure ()
    | some tid =>
    let what := s!"impl {ti.traitName} for {ti.typeName}"
    let (decl, head) : Option TypeDecl × Option ImplHead :=
      if ti.typeParams.contains ti.typeName then (none, none)
      else match resolveHead sc ti.typeName with
        | .inl (some d) => (some d, some (.decl d.id))
        | .inl none => (none, some (.unresolved sc.path ti.typeName))
        | .inr h => (none, some h)
    let target : Term :=
      if ti.typeParams.contains ti.typeName then .var ti.typeName
      else .app ((head.map (·.key)).getD ti.typeName)
             ((ti.targetArgs.getD []).map (tyTerm ti.typeParams))
    infos := infos ++ [{ owner := .trait tid, head, target, typeName := ti.typeName,
                         module := sc.path, span := ti.span, file,
                         methods := ti.methods.map (·.name) }]
    if let some why := shapeCheck what ti.typeName ti.typeParams ti.typeParams ti.targetArgs decl then
      ds := ds ++ [mkDiag "E0593" why ti.span file
        (some "write `impl<T, ..> Trait for Type<T, ..>` with the type's own parameters (docs/language/TRAITS.md TC-4)")]
    -- TC-3: instance locality.
    let traitLocal := match tid with
      | .user dm _ => dm == sc.path
      | .builtin _ => false
    let typeLocal := match head with
      | some (.decl (.user dm _)) => dm == sc.path
      | _ => false
    let unresolved := match head with
      | some (.unresolved _ _) => true
      | _ => false
    if !traitLocal && !typeLocal && !unresolved && head.isSome then
      let where_ := match head with
        | some (.decl (.user dm _)) => s!"defined in module '{dm}'"
        | some (.decl (.builtin _)) => "a compiler type"
        | _ => "a primitive type"
      ds := ds ++ [mkDiag "E0592"
        s!"orphan impl: {what} is in module '{sc.path}', which defines neither the trait '{tid.display}' nor the type '{ti.typeName}' ({where_}); an impl lives with its trait or with its type, so no two modules can both supply it"
        ti.span file
        (some "move the impl into the module that defines the type (or the trait) (docs/language/TRAITS.md TC-3)")]
  for ib in m.implBlocks do
    let what := s!"impl {ib.typeName}"
    let (decl, head) : Option TypeDecl × Option ImplHead :=
      if ib.typeParams.contains ib.typeName then (none, none)
      else match resolveHead sc ib.typeName with
        | .inl (some d) => (some d, some (.decl d.id))
        | .inl none => (none, some (.unresolved sc.path ib.typeName))
        | .inr h => (none, some h)
    infos := infos ++ [{ owner := .inherent, head, target := .app ((head.map (·.key)).getD ib.typeName) [],
                         typeName := ib.typeName, module := sc.path, span := ib.span, file,
                         methods := ib.methods.map (·.name) }]
    if let some why := shapeCheck what ib.typeName ib.typeParams (ib.typeParams ++ ib.capParams) ib.targetArgs decl then
      ds := ds ++ [mkDiag "E0593" why ib.span file
        (some "write `impl<T, ..> Type<T, ..>` with the type's own parameters (docs/language/TRAITS.md TC-4)")]
  return (infos, ds)

/-- Every module of the program, with its definition path and source file. -/
partial def allScopes (summaryTable : List (String × FileSummary)) (path : String) (file : String)
    (m : Module) : List (Scope × String) :=
  let file := if m.sourceFile.isEmpty then file else m.sourceFile
  ({ path, module := m, summaryTable }, file) ::
    m.submodules.flatMap fun sub => allScopes summaryTable (path ++ "." ++ sub.name) file sub

/-- TC-1 / TC-2 / TC-5 over every impl of the program. Pairs are judged by identity, so the
    verdict does not depend on the order impls or imports were written in. -/
def pairwise (infos : List ImplInfo) : Diagnostics := Id.run do
  let mut ds : Diagnostics := []
  -- Indices of impls shadowed by an earlier conflicting one: excluded from TC-5 so one
  -- duplicate is reported once (as TC-1/TC-2), not again as a method collision.
  let mut shadowed : List Nat := []
  let indexed := (List.range infos.length).zip infos
  for (j, b) in indexed do
    match b.owner with
    | .inherent => pure ()
    | .trait tb =>
      for (i, a) in indexed do
        if i >= j || shadowed.contains j then continue
        match a.owner with
        | .trait ta =>
          if ta != tb then continue
          match unify [] [(a.target.tag "a:", b.target.tag "b:")] with
          | none => pure ()
          | some _ =>
            shadowed := shadowed ++ [j]
            if a.target.canon == b.target.canon then
              ds := ds ++ [mkDiag "E0590"
                s!"duplicate impl: trait '{tb.display}' is implemented twice for '{b.typeName}' (the other impl is in module '{a.module}' at {a.span.line}:{a.span.col}); impl selection would depend on which one is found first"
                b.span b.file (some "delete one of the two impls (docs/language/TRAITS.md TC-1)")]
            else
              ds := ds ++ [mkDiag "E0591"
                s!"overlapping impls: this impl of trait '{tb.display}' for '{b.typeName}' and the one in module '{a.module}' at {a.span.line}:{a.span.col} both apply to some type; impl selection would depend on which one is found first"
                b.span b.file (some "keep one impl that covers the type (docs/language/TRAITS.md TC-2)")]
        | .inherent => pure ()
  -- TC-5: one method name per type across all of its impls.
  let live := indexed.filter fun (i, _) => !shadowed.contains i
  let mut seen : List (String × (MethodKey × ImplInfo)) := []
  for (_, info) in live do
    match info.head with
    | none => pure ()
    | some head =>
      for mn in info.methods.eraseDups do
        let k := head.key ++ "\x00" ++ mn
        match seen.lookup k with
        | some (prev, prevInfo) =>
          if prevInfo.span.line == info.span.line && prevInfo.span.col == info.span.col
              && prevInfo.module == info.module then continue
          ds := ds ++ [mkDiag "E0594"
            s!"method '{mn}' of '{info.typeName}' is provided by both {info.owner.display} (here) and {prev.owner.display} (module '{prevInfo.module}' at {prevInfo.span.line}:{prevInfo.span.col}); a call `x.{mn}()` would have two candidates"
            info.span info.file (some "rename one of the methods (docs/language/TRAITS.md TC-5)")]
        | none => seen := seen ++ [(k, ({ owner := info.owner, target := head, method := mn }, info))]
  return ds

/-- The coherence judgment for a whole program. Empty means every impl is unique, local and
    fully generic, and every (type, method) pair names one method. -/
def checkProgram (modules : List Module) (summaryTable : List (String × FileSummary)) : Diagnostics :=
  let scopes := modules.flatMap fun m => allScopes summaryTable m.name m.sourceFile m
  let per := scopes.map fun (sc, file) => moduleImpls sc file
  let infos := per.flatMap (·.1)
  per.flatMap (·.2) ++ pairwise infos

end Coherence

end Concrete
