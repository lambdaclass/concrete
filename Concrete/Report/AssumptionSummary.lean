import Std.Data.HashMap
import Concrete.Elab.Core
import Concrete.Proof.ProofCore
import Concrete.Semantics.Capabilities

/-!
# Assumption summaries (R-0484 R10)

Checking that callers respect a foreign declaration does not make the declaration true. This
module computes, ONCE per build and over every loaded module (the program and its
dependencies), which ASSUMPTIONS each function's conclusions rest on, so that every report,
JSON fact, query and explanation reads the same semantic facts instead of walking bodies or
re-deriving them.

* **Identity** (`AssumptionId`) names exactly one Concrete declaration: its kind, module path and declared
  name. Two modules that each bind C's `realloc` are two identities; an import rename
  (`write as libc_write`) is the same identity.
* **Facts** (`Facts`) are attached to an identity: a foreign binding's declared effects and
  whether it is a `trusted extern`. A trusted function carries no further facts: its body's
  memory safety is ASSUMED as a whole, and no specific discharged obligation is recorded.
* **Per function** (`FnSummary`): the identities it may reach, each with ONE provenance hop
  toward the declaration (`Via`), and the UNRESOLVED edges — indirect calls through a fn-typed
  binding, whose targets are not known statically. An empty `reaches` list means "none
  reached" only when `gaps` is empty; otherwise the answer is incomplete, and every
  conclusion drawn from it must say so.

Propagation is conservative through recursion: functions in one strongly connected call group
receive the union of the group's facts, and provenance inside the group is assigned
breadth-first from the members that hold a fact directly, so an explanation chain always
terminates at the declaration.

Reached means MAY reach: a direct call, or a function taken as a value (which may be called
later by code no direct edge connects to it). It is never a claim that the effect happens.
-/

namespace Concrete
namespace Assumptions

/-- What kind of assumption a declaration carries. -/
inductive Kind where
  /-- An `extern` binding: its declared effects are assumed honest (R3). -/
  | foreignBinding
  /-- A `trusted` function: its body's memory safety is assumed, not checked. -/
  | trustedBoundary
  deriving BEq, Repr, Inhabited

def Kind.tag : Kind → String
  | .foreignBinding => "foreign-binding"
  | .trustedBoundary => "trusted-boundary"

/-- Exactly one Concrete declaration: package scope, module path, declared name. The package is
    the package's canonical identity (`PackageIdentity.digest` — from its manifest, or the
    canonical synthetic identity over its module inventory and content when it has none). Only
    when NO identity can be formed is it `unidentified:<name>`, and then every report marks the
    attribution ambiguous (`AssumptionId.ambiguous`). Module names do not scope a declaration — two packages may each define
    a module `util` binding `putchar`. -/
structure AssumptionId where
  kind : Kind
  package : String
  packageName : String
  module : String
  name : String
  deriving BEq, Repr, Inhabited

def AssumptionId.qualified (i : AssumptionId) : String := i.module ++ "." ++ i.name
/-- No package identity could be formed, so two declarations may share this attribution. -/
def AssumptionId.ambiguous (i : AssumptionId) : Bool := i.package.startsWith "unidentified:"
/-- The identity key: length-prefixed so no split of the components collides. -/
def AssumptionId.key (i : AssumptionId) : String :=
  s!"{i.kind.tag}:P{i.package.length}:{i.package}|{i.qualified}"

/-- Facts attached to an identity. -/
structure Facts where
  id : AssumptionId
  /-- Foreign bindings: the declared `with(...)`. -/
  effects : CapSet := .empty
  /-- Foreign bindings: declared `trusted extern`. -/
  trustedExtern : Bool := false
  deriving Repr

instance : Inhabited Facts := ⟨{ id := default }⟩

/-- One provenance hop: why an assumption reaches a function. -/
inductive Via where
  /-- The function is itself the trusted boundary. -/
  | declaredHere
  /-- The function calls the foreign binding directly. -/
  | callsBinding
  /-- The function takes the foreign binding as a value. -/
  | refersToBinding
  /-- Inherited through a direct call to this function (qualified name). -/
  | throughCall (callee : String)
  /-- Inherited through this function (qualified name) taken as a value. -/
  | throughValue (fn : String)
  deriving BEq, Repr, Inhabited

structure Reach where
  key : String
  via : Via
  deriving Repr, Inhabited

/-- An unresolved edge: an indirect call, whose targets are not known statically. -/
structure Gap where
  /-- Qualified function containing the indirect call (display). -/
  site : String
  /-- Its scoped key, so two packages' same-named functions do not merge into one gap. -/
  siteKey : String
  /-- The fn-typed binding called through. -/
  binding : String
  deriving BEq, Repr, Inhabited

def Gap.render (g : Gap) : String := s!"indirect call through `{g.binding}` in {g.site}"

structure FnSummary where
  /-- Display name: module path and function name. -/
  fn : String
  /-- Scoped key: package plus display name — the table key. -/
  fnKey : String
  packageName : String
  module : String
  reaches : Array Reach := #[]
  gaps : Array Gap := #[]
  deriving Inhabited

def FnSummary.complete (s : FnSummary) : Bool := s.gaps.isEmpty

def FnSummary.reachesKey (s : FnSummary) (key : String) : Bool :=
  s.reaches.any (·.key == key)

structure Table where
  facts : Std.HashMap String Facts := {}
  fns : Std.HashMap String FnSummary := {}
  /-- Function order, for deterministic rendering. -/
  order : Array String := #[]
  deriving Inhabited

/-! ## Collection -/

/-- The spellings a definition can be called by from elsewhere in the program: its own
    name, and the module-prefixed forms Elab gives submodule members. -/
def callSpellings (path name : String) : List String :=
  let segs := path.splitOn "."
  let suffixes := (List.range segs.length).map fun i => "_".intercalate (segs.drop i)
  -- A dependency's definitions are stored already prefixed (`io_Writer_write` in `std.io`),
  -- while a call from another package spells the unprefixed form (`Writer_write`). Without
  -- the base spelling such calls resolved to nothing — and the indirect call inside
  -- `Writer::write` never reached the program, which then read as COMPLETE.
  let base := suffixes.foldl (fun acc sfx =>
    if acc == name && name.startsWith (sfx ++ "_") then (name.drop (sfx.length + 1)).toString else acc) name
  let forms (n : String) := n :: suffixes.map (· ++ "_" ++ n)
  (forms name ++ (if base == name then [] else forms base)).eraseDups

private structure Node where
  fn : String
  fnKey : String
  packageName : String
  package : String
  name : String
  module : String
  spellings : List String
  direct : List String
  values : List String
  indirect : List String
  trusted : Bool
  deriving Inhabited

private structure Binding where
  key : String
  module : String
  spellings : List String
  deriving Inhabited

/-- The package scope of a module: by its source file, else inherited from its parent. -/
private def scopeOf (packageOf : String → Option (String × String)) (m : CModule)
    (parent : String × String) : String × String :=
  (packageOf m.sourceFile).getD parent

private partial def collect (packageOf : String → Option (String × String))
    (m : CModule) (path : String) (parent : String × String)
    : Array Node × Array Binding × Array Facts :=
  let (pkg, pkgName) := scopeOf packageOf m parent
  let norm (n : String) : String := (m.linkerAliases.lookup n).getD n
  let nodes := m.functions.toArray.map fun f =>
    { fn := path ++ "." ++ f.name, fnKey := s!"P{pkg.length}:{pkg}|{path}.{f.name}"
      packageName := pkgName, package := pkg
      name := f.name, module := path, spellings := callSpellings path f.name
      direct := (collectCallsStmts f.body).eraseDups.map norm
      values := (collectFnValueRefsStmts f.body).eraseDups.map norm
      indirect := (collectIndirectCallsStmts f.body).eraseDups
      trusted := f.isTrusted : Node }
  let externs := m.externFns.toArray.map fun (n, _, _, t) =>
    let id : AssumptionId := { kind := .foreignBinding, package := pkg, packageName := pkgName, module := path, name := n }
    ({ key := id.key, module := path, spellings := callSpellings path n : Binding },
     { id, effects := (m.externFnCaps.lookup n).getD .empty, trustedExtern := t : Facts })
  let trustedFacts := m.functions.toArray.filterMap fun f =>
    if f.isTrusted then
      some ({ id := { kind := .trustedBoundary, package := pkg, packageName := pkgName, module := path, name := f.name } } : Facts)
    else none
  m.submodules.foldl (fun (ns, bs, fs) sub =>
      let (ns', bs', fs') := collect packageOf sub (path ++ "." ++ sub.name) (pkg, pkgName)
      (ns ++ ns', bs ++ bs', fs ++ fs'))
    (nodes, externs.map (·.1), externs.map (·.2) ++ trustedFacts)

/-- Index: spelling → indices, so name resolution is not a scan of the whole program. -/
private def indexBy {α} [Inhabited α] (xs : Array α) (spellings : α → List String)
    : Std.HashMap String (Array Nat) := Id.run do
  let mut idx : Std.HashMap String (Array Nat) := {}
  for i in [0:xs.size] do
    for sp in spellings xs[i]! do
      idx := idx.insert sp ((idx.getD sp #[]).push i)
  return idx

/-- A name resolves like a call does: to a definition in the CALLING module when one has
    that spelling, and only otherwise to definitions elsewhere. -/
private def resolve {α} [Inhabited α] (xs : Array α) (idx : Std.HashMap String (Array Nat))
    (moduleOf : α → String) (fromModule name : String) : Array Nat :=
  let all := idx.getD name #[]
  let local_ := all.filter fun i => moduleOf xs[i]! == fromModule
  if local_.isEmpty then all else local_

/-! ## Strongly connected components (Tarjan) -/

private structure TState where
  index : Array (Option Nat)
  low : Array Nat
  onStack : Array Bool
  stack : Array Nat := #[]
  next : Nat := 0
  sccs : Array (Array Nat) := #[]

private partial def strongConnect (succ : Array (Array Nat)) (v : Nat) (st : TState) : TState :=
  let st := { st with index := st.index.set! v (some st.next), low := st.low.set! v st.next,
                      next := st.next + 1, stack := st.stack.push v,
                      onStack := st.onStack.set! v true }
  let st := succ[v]!.foldl (fun st w =>
    match st.index[w]! with
    | none =>
      let st := strongConnect succ w st
      { st with low := st.low.set! v (min st.low[v]! st.low[w]!) }
    | some wi =>
      if st.onStack[w]! then { st with low := st.low.set! v (min st.low[v]! wi) } else st) st
  if st.low[v]! == (st.index[v]!).getD 0 then
    let rec pop (st : TState) (acc : Array Nat) : TState × Array Nat :=
      match st.stack.back? with
      | none => (st, acc)
      | some w =>
        let st := { st with stack := st.stack.pop, onStack := st.onStack.set! w false }
        if w == v then (st, acc.push w) else pop st (acc.push w)
    let (st, comp) := pop st #[]
    { st with sccs := st.sccs.push comp }
  else st

/-- SCCs in reverse topological order: every SCC appears after all SCCs it calls into. -/
private def sccsOf (succ : Array (Array Nat)) : Array (Array Nat) :=
  let n := succ.size
  let st0 : TState := { index := Array.replicate n none, low := Array.replicate n 0,
                        onStack := Array.replicate n false }
  let st := (List.range n).foldl (fun st v =>
    if st.index[v]!.isNone then strongConnect succ v st else st) st0
  st.sccs

/-! ## Building the table -/

/-- Compute assumption summaries for every function in `modules` (the program and every
    loaded dependency). Pure and deterministic. -/
def build (modules : List CModule)
    (packageOf : String → Option (String × String) := fun _ => none)
    (defaultPackage : String × String := ("unidentified:program", "program")) : Table := Id.run do
  let (nodes, bindings, facts) := modules.foldl (fun (ns, bs, fs) m =>
      let (ns', bs', fs') := collect packageOf m m.name defaultPackage
      (ns ++ ns', bs ++ bs', fs ++ fs')) (#[], #[], #[])
  let fnIdx := indexBy nodes (·.spellings)
  let bIdx := indexBy bindings (·.spellings)
  let n := nodes.size
  -- Outgoing function edges (target, isValue) and each node's own reaches and gaps.
  let mut succ : Array (Array Nat) := Array.replicate n #[]
  let mut edgeKind : Array (Array (Nat × Bool)) := Array.replicate n #[]
  let mut own : Array (Array Reach) := Array.replicate n #[]
  let mut ownGaps : Array (Array Gap) := Array.replicate n #[]
  for i in [0:n] do
    let nd := nodes[i]!
    let mut es : Array (Nat × Bool) := #[]
    let mut rs : Array Reach := #[]
    if nd.trusted then
      rs := rs.push { key := ({ kind := .trustedBoundary, package := nd.package,
                                packageName := nd.packageName, module := nd.module,
                                name := nd.name } : AssumptionId).key
                      via := .declaredHere }
    for (name, isValue) in nd.direct.map (·, false) ++ nd.values.map (·, true) do
      for b in resolve bindings bIdx (·.module) nd.module name do
        let key := bindings[b]!.key
        if !(rs.any (·.key == key)) then
          rs := rs.push { key, via := if isValue then .refersToBinding else .callsBinding }
      for t in resolve nodes fnIdx (·.module) nd.module name do
        if t != i && !(es.any (·.1 == t)) then es := es.push (t, isValue)
    edgeKind := edgeKind.set! i es
    succ := succ.set! i (es.map (·.1))
    own := own.set! i rs
    ownGaps := ownGaps.set! i (nd.indirect.toArray.map fun b => { site := nd.fn, siteKey := nd.fnKey, binding := b })
  -- Resolve in reverse topological order of SCCs.
  let mut reach : Array (Array Reach) := Array.replicate n #[]
  let mut gaps : Array (Array Gap) := Array.replicate n #[]
  for comp in sccsOf succ do
    let inComp (j : Nat) : Bool := comp.contains j
    -- Keys and gaps the whole group may reach.
    let mut keys : Array String := #[]
    let mut gs : Array Gap := #[]
    for v in comp do
      for r in own[v]! do
        if !keys.contains r.key then keys := keys.push r.key
      for g in ownGaps[v]! do
        if !gs.contains g then gs := gs.push g
      for (t, _) in edgeKind[v]! do
        if !inComp t then
          for r in reach[t]! do
            if !keys.contains r.key then keys := keys.push r.key
          for g in gaps[t]! do
            if !gs.contains g then gs := gs.push g
    -- Provenance: members holding a key directly (own or through an edge leaving the group)
    -- first, then breadth-first through edges inside the group.
    let mut assigned : Array (Array Reach) := comp.map fun _ => #[]
    for key in keys do
      let mut via : Array (Option Via) := comp.map fun v =>
        match own[v]!.find? (·.key == key) with
        | some r => some r.via
        | none =>
          match edgeKind[v]!.find? (fun (t, _) => !inComp t && reach[t]!.any (·.key == key)) with
          | some (t, isValue) => some (if isValue then .throughValue nodes[t]!.fnKey else .throughCall nodes[t]!.fnKey)
          | none => none
      let mut changed := true
      while changed do
        changed := false
        for ci in [0:comp.size] do
          if via[ci]!.isNone then
            let v := comp[ci]!
            match edgeKind[v]!.find? (fun (t, _) =>
                match comp.findIdx? (· == t) with
                | some cj => via[cj]!.isSome
                | none => false) with
            | some (t, isValue) =>
              via := via.set! ci (some (if isValue then .throughValue nodes[t]!.fnKey else .throughCall nodes[t]!.fnKey))
              changed := true
            | none => pure ()
      for ci in [0:comp.size] do
        if let some v := via[ci]! then
          assigned := assigned.set! ci (assigned[ci]!.push { key, via := v })
    for ci in [0:comp.size] do
      reach := reach.set! comp[ci]! assigned[ci]!
      gaps := gaps.set! comp[ci]! gs
  let mut table : Table := {}
  for f in facts do
    table := { table with facts := table.facts.insert f.id.key f }
  for i in [0:n] do
    let nd := nodes[i]!
    table := { table with
      fns := table.fns.insert nd.fnKey { fn := nd.fn, fnKey := nd.fnKey, packageName := nd.packageName,
                                         module := nd.module, reaches := reach[i]!, gaps := gaps[i]! }
      order := table.order.push nd.fnKey }
  return table

/-! ## Queries shared by every consumer -/

def Table.summary? (t : Table) (fn : String) : Option FnSummary := t.fns.get? fn

/-- ONE WITNESS path from the function with key `fnKey` to the declaration of `key`, as display
    names. It follows the single recorded hop per function, so it is a valid call path that
    exists — not the list of every path. The summary itself keeps every reachable assumption;
    only this rendering chooses one route. Bounded by the number of functions. -/
partial def Table.explain (t : Table) (fnKey key : String) : List String :=
  let display (k : String) : String := ((t.fns.get? k).map (·.fn)).getD k
  let rec go (cur : String) (seen : List String) (fuel : Nat) : List String :=
    if fuel == 0 || seen.contains cur then [display cur] else
    match t.fns.get? cur with
    | none => [display cur]
    | some s =>
      match s.reaches.find? (·.key == key) with
      | none => [display cur]
      | some r =>
        match r.via with
        | .declaredHere => [s.fn]
        | .callsBinding | .refersToBinding =>
          [s.fn, ((t.facts.get? key).map (·.id.qualified)).getD key]
        | .throughCall c | .throughValue c => s.fn :: go c (cur :: seen) (fuel - 1)
  go fnKey [] (t.fns.size + 1)

/-- Foreign bindings a function may reach (identity keys), in table order. -/
def FnSummary.foreignBindings (s : FnSummary) : Array String :=
  s.reaches.filterMap fun r => if r.key.startsWith "foreign-binding:" then some r.key else none

/-- Trusted boundaries a function may reach (identity keys). -/
def FnSummary.trustedBoundaries (s : FnSummary) : Array String :=
  s.reaches.filterMap fun r => if r.key.startsWith "trusted-boundary:" then some r.key else none

end Assumptions
end Concrete

namespace Concrete
namespace Assumptions

/-- Is this module path inside one of `roots` (a root module or its submodules)? -/
def underRoots (roots : List String) (module : String) : Bool :=
  roots.any fun r => module == r || module.startsWith (r ++ ".")

/-- Foreign bindings declared OUTSIDE `programRoots` that some function inside them may reach,
    with the reaching program functions — the "inherited" assumptions of a program. -/
def Table.inheritedForeign (t : Table) (programRoots : List String)
    : Array (Facts × Array String) := Id.run do
  let programFns := t.order.filter fun fn =>
    match t.fns.get? fn with
    | some s => underRoots programRoots s.module
    | none => false
  let mut out : Array (Facts × Array String) := #[]
  let keys := (t.facts.toArray.map (·.1)).qsort (· < ·)
  for key in keys do
    let some f := t.facts.get? key | continue
    if f.id.kind != .foreignBinding || underRoots programRoots f.id.module then continue
    let users := programFns.filter fun fn => ((t.fns.get? fn).map (·.reachesKey key)).getD false
    if !users.isEmpty then out := out.push (f, users)
  -- `users` are scoped keys; callers render them with `Table.displayOf`.
  return out

/-- Display name for a scoped function key. -/
def Table.displayOf (t : Table) (fnKey : String) : String := ((t.fns.get? fnKey).map (·.fn)).getD fnKey

/-- Program functions whose summary is INCOMPLETE (scoped keys), with their unresolved edges. -/
def Table.incompleteProgramFns (t : Table) (programRoots : List String)
    : Array (String × Array Gap) :=
  t.order.filterMap fun fn =>
    match t.fns.get? fn with
    | some s => if underRoots programRoots s.module && !s.complete then some (fn, s.gaps) else none
    | none => none

end Assumptions
end Concrete

namespace Concrete
namespace Assumptions

private def jsonEsc (s : String) : String :=
  s.foldl (fun acc c =>
    match c with
    | '"' => acc ++ "\\\""
    | '\\' => acc ++ "\\\\"
    | '\n' => acc ++ "\\n"
    | '\t' => acc ++ "\\t"
    | c => if c.toNat < 0x20 then acc ++ "?" else acc.push c) ""

private def jq (s : String) : String := "\"" ++ jsonEsc s ++ "\""
private def jarr (xs : List String) : String := "[" ++ ",".intercalate xs ++ "]"

/-- The machine-readable view of the same facts the text reports render (`--report
    assumptions`). One object per PROGRAM function: what it may reach, with the provenance
    path, and whether that answer is complete. Schema `concrete.assumptions.v1`. -/
def Table.toJson (t : Table) (programRoots : List String) (depsLoaded : Bool) : String :=
  let fns := t.order.toList.filterMap fun fn =>
    match t.fns.get? fn with
    | some s => if underRoots programRoots s.module then some s else none
    | none => none
  let fnJson (s : FnSummary) : String :=
    let assumes := s.reaches.toList.filterMap fun r =>
      match t.facts.get? r.key with
      | none => none
      | some f =>
        let (names, vars) := f.effects.normalize
        let effects := if f.id.kind == .foreignBinding then jarr ((names ++ vars).map jq) else "null"
        some ("{" ++ ",".intercalate [
          s!"\"kind\":{jq f.id.kind.tag}",
          s!"\"declaration\":{jq f.id.qualified}",
          s!"\"effects\":{effects}",
          s!"\"trusted_extern\":{if f.trustedExtern then "true" else "false"}",
          s!"\"package\":{jq f.id.package}",
          s!"\"package_name\":{jq f.id.packageName}",
          s!"\"package_identity_ambiguous\":{if f.id.ambiguous then "true" else "false"}",
          s!"\"witness_path\":{jarr ((t.explain s.fnKey r.key).map jq)}" ] ++ "}")
    let gaps := s.gaps.toList.map fun g =>
      "{" ++ s!"\"site\":{jq g.site},\"binding\":{jq g.binding}" ++ "}"
    "{" ++ ",".intercalate [
      s!"\"fn\":{jq s.fn}",
      s!"\"package_name\":{jq s.packageName}",
      s!"\"complete\":{if s.complete then "true" else "false"}",
      s!"\"gaps\":{jarr gaps}",
      s!"\"assumes\":{jarr assumes}" ] ++ "}"
  "{" ++ ",".intercalate [
    "\"schema\":\"concrete.assumptions.v1\"",
    s!"\"dependencies_analysed\":{if depsLoaded then "true" else "false"}",
    s!"\"functions\":{jarr (fns.map fnJson)}" ] ++ "}"

end Assumptions
end Concrete

namespace Concrete
namespace Assumptions

/-! ## Package scope is part of identity — checked at build time

Two packages may define the same module path and the same declaration names. The frontend
refuses that collision for a whole program (bug 074), but the summary must not depend on that:
built directly from two modules both named `util`, each binding `putchar`, mapped to different
packages, it must hold TWO foreign-binding identities. A key without the package would merge
them, and this build would fail. -/
private def packageScopeProbe : Table :=
  let mk (file : String) : CModule :=
    { name := "util", structs := [], enums := [], functions := [], constants := []
      externFns := [("putchar", [], .i32, false)]
      externFnCaps := [("putchar", .concrete ["Console"])], sourceFile := file }
  build [mk "p1/src/lib.con", mk "p2/src/lib.con"] (fun f =>
    if f == "p1/src/lib.con" then some ("pkgA", "p1")
    else if f == "p2/src/lib.con" then some ("pkgB", "p2") else none)

#guard (packageScopeProbe.facts.toList.filter fun (_, f) =>
          f.id.kind == .foreignBinding && f.id.qualified == "util.putchar").length == 2
#guard ((packageScopeProbe.facts.toList.filter fun (_, f) => f.id.qualified == "util.putchar").map
          (·.2.id.packageName)).mergeSort (· ≤ ·) == ["p1", "p2"]

end Assumptions
end Concrete
