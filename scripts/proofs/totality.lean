import Lean

open Lean

def moduleOf (env : Environment) (n : Name) : Name :=
  ((env.getModuleIdxFor? n).bind fun i => env.header.moduleNames[i.toNat]?).getD .anonymous

-- The pinned Init and Std implementations provide the runtime primitives.
def runtimeModule (m : Name) : Bool :=
  m.getRoot == `Init || m.getRoot == `Std

def violation (env : Environment) (n : Name) (ci : ConstantInfo) : Option String := do
  if runtimeModule (moduleOf env n) then none
  else if ci.isUnsafe then some "unsafe"
  else if ci matches .defnInfo { safety := .partial, .. } then some "partial"
  else if Compiler.getImplementedBy? env n |>.isSome then some "implemented_by"
  else if getExternAttrData? env n |>.isSome then some "extern"
  else if isNoncomputable env n then some "noncomputable"
  else if ci matches .opaqueInfo _ then some "opaque"
  else if ci matches .axiomInfo _ then some "axiom"
  else none

-- Compiler-generated recursion helpers implement the checked kernel definition.
-- Audit their callers through the safe parent; handwritten lookalikes have a range.
def recursiveParent (env : Environment) (n : Name) : Option Name := do
  let parent ← Compiler.isUnsafeRecName? n
  let .defnInfo d ← env.find? parent | none
  if d.safety == .safe && moduleOf env parent == moduleOf env n &&
      (declRangeExt.find? (level := .private) env n).isNone &&
      (declRangeExt.find? (level := .server) env n).isNone then some parent
  else none

def proofOnly (ci : ConstantInfo) : Bool :=
  (ci matches .thmInfo _) || ci.type.getForallBody == .sort .zero

def effectfulExtern (data : ExternAttrData) : Bool :=
  data.entries.any fun entry =>
    let symbol := match entry with
      | .standard _ symbol | .inline _ symbol => symbol
      | _ => ""
    ["lean_io_", "lean_dbg_", "lean_task_"].any (fun stem => (symbol.splitOn stem).length > 1)

def effectful (env : Environment) (n : Name) : Bool :=
  [`IO, `BaseIO, `EIO, `EST, `Task].any (·.isPrefixOf n) ||
    (getExternAttrData? env n).any effectfulExtern

abbrev Rewrites := Std.HashMap Name (Array (Name × Compiler.CSimp.Entry))
abbrev NativeDecls := Std.HashMap Name (Name × IR.Decl)

-- Local compiler rewrites can disappear from extension metadata at end of
-- scope. Follow calls and closures in the emitted IR as well as kernel edges.
partial def nativeCalls (body : IR.FnBody) (calls : Array Name := #[]) : Array Name :=
  match body with
  | .vdecl _ _ (.fap n _) next | .vdecl _ _ (.pap n _) next => nativeCalls next (calls.push n)
  | .jdecl _ _ body next => nativeCalls next (nativeCalls body calls)
  | .case _ _ _ branches => branches.foldl (fun acc alt => nativeCalls alt.body acc) calls
  | .ret _ | .jmp .. | .unreachable => calls
  | other => nativeCalls other.body calls

def inspect (env : Environment) (rewrites : Rewrites) (native : NativeDecls) (roots : Array Name)
    (pureStage : Bool) : IO (Array String) := do
  let mut pending := roots.map fun n => (n, n)
  let mut seen : Std.HashSet Name := {}
  let mut failures := #[]
  let mut idx := 0
  while idx < pending.size do
    let (n, caller) := pending[idx]!
    idx := idx + 1
    if seen.contains n then continue
    seen := seen.insert n
    let ir := native[n]?
    if let some (m, decl) := ir then
      match decl with
      | .fdecl _ _ _ body _ =>
        unless runtimeModule m do
          for dep in nativeCalls body do pending := pending.push (dep, n)
      | .extern _ _ _ data =>
        if pureStage && effectfulExtern data then
          failures := failures.push s!"{caller} reaches IO or task declaration {n}"
        if !runtimeModule m && !data.entries.all (· == .opaque) then
          failures := failures.push s!"{caller} reaches native extern {n}"
    let some ci := env.find? n | do
      if ir.isNone then
        failures := failures.push s!"missing declaration {n} from {caller}"
      continue
    if proofOnly ci then continue
    for (m, rewrite) in rewrites[n]?.getD #[] do
      if !runtimeModule m then
        failures := failures.push s!"{n} has csimp replacement {rewrite.toDeclName} registered by {rewrite.thmName} in {m}"
      pending := pending.push (rewrite.toDeclName, n)
    if let some implementation := Compiler.getImplementedBy? env n then
      pending := pending.push (implementation, n)
    if pureStage && effectful env n then
      failures := failures.push s!"{caller} reaches IO or task declaration {n}"
      continue
    if let some parent := recursiveParent env n then
      pending := pending.push (parent, n)
      continue
    if let some reason := violation env n ci then
      failures := failures.push s!"{caller} reaches {reason} declaration {n}"
    for dep in ci.getUsedConstantsAsSet do pending := pending.push (dep, n)
    match ci with
    | .recInfo r =>
      for rule in r.rules do
        for dep in rule.rhs.getUsedConstants do pending := pending.push (dep, n)
    | _ => pure ()
  return failures

def moduleClosure (env : Environment) (root : Name) : IO (Std.HashSet Name) := do
  let indices := Std.HashMap.ofList (env.header.moduleNames.toList.zipIdx)
  let mut pending := #[root]
  let mut visited : Std.HashSet Name := {}
  let mut idx := 0
  while idx < pending.size do
    let m := pending[idx]!
    idx := idx + 1
    if visited.contains m then continue
    visited := visited.insert m
    let some i := indices[m]? | throw (IO.userError s!"missing module {m}")
    for dep in env.header.moduleData[i]!.imports do pending := pending.push dep.module
  return visited

def main (args : List String) : IO UInt32 := do
  let [target, stage] := args | throw (IO.userError "expected driver and compiler roots")
  let sysroot ← findSysroot
  initSearchPath sysroot
  let runtimeRoot ← IO.FS.realPath (sysroot / "lib" / "lean")
  let sourcePath ← getSrcSearchPath
  let sourceManifest ← IO.getEnv "PROOF_TOTALITY_SOURCES"
  -- Read serialized extension entries without initializing imported modules.
  -- Examine every exported rewrite: a later registration can shadow one that
  -- was used when an earlier module in the closure compiled.
  let env ← importModules #[{module := target.toName}, {module := stage.toName}] {}
  let stageModules ← moduleClosure env stage.toName
  let driverModules ← moduleClosure env target.toName
  let mut failures := #[]
  let mut rewrites : Rewrites := {}
  let mut native : NativeDecls := {}
  let mut sources := #[]
  for (m, idx) in env.header.moduleNames.toList.zipIdx do
    if !stageModules.contains m && !driverModules.contains m then continue
    if runtimeModule m then
      let artifact ← IO.FS.realPath (← findOLean m)
      if !artifact.toString.startsWith (runtimeRoot.toString ++ "/") then
        failures := failures.push s!"{m} shadows the pinned runtime: {artifact}"
    else if sourceManifest.isSome then
      sources := sources.push (← findLean sourcePath m).toString
    if m.getRoot == `Lean || m.getRoot == `ConcreteTooling then
      failures := failures.push s!"compiler imports forbidden module {m}"
    for d in IR.declMapExt.getModuleEntries env idx ++ IR.declMapExt.getModuleIREntries env idx do
      native := native.insert d.name (m, d)
    for entry in Compiler.CSimp.ext.ext.getModuleEntries env idx do
      let rewrite := match entry with
        | .global e | .scoped _ e => e
      rewrites := rewrites.insert rewrite.fromDeclName
        ((rewrites[rewrite.fromDeclName]?.getD #[]).push (m, rewrite))
    -- These can run at program startup even when no declaration calls them.
    -- Exported symbols can also provide an implementation at native link time.
    unless runtimeModule m do
      for (label, attr) in [("init", regularInitAttr), ("builtin_init", builtinInitAttr),
          ("implemented_by", Compiler.implementedByAttr), ("export", exportAttr)] do
        let entries := attr.ext.getModuleEntries env idx
        let irEntries := attr.ext.getModuleIREntries env idx
        for (decl, _) in entries ++ irEntries.filter (!entries.contains ·) do
          failures := failures.push s!"{m} registers {label} for {decl}"
      for decl in IR.UnboxResult.unboxAttr.ext.getModuleEntries env idx do
        failures := failures.push s!"{m} registers unbox for {decl}"
  let mut stages := #[]
  let mut commands := #[]
  for (n, ci) in env.constants.toList do
    let m := moduleOf env n
    if recursiveParent env n |>.isSome then continue
    if ci matches .thmInfo _ then continue
    if m == target.toName then commands := commands.push n
    if stageModules.contains m && (m.getRoot == `Concrete || m == stage.toName) &&
        n != `Concrete.BIR.behaviour then
      stages := stages.push n
    if driverModules.contains m && !stageModules.contains m && !runtimeModule m &&
        m != target.toName then
      commands := commands.push n
  if stages.isEmpty || commands.isEmpty then
    throw (IO.userError "empty stage or driver definition population")
  failures := failures ++ (← inspect env rewrites native stages true) ++ (← inspect env rewrites native commands false)
  for msg in failures do IO.eprintln msg
  if !failures.isEmpty then return 1
  if let some manifest := sourceManifest then
    IO.FS.writeFile manifest (String.intercalate "\n" sources.toList ++ "\n")
  return 0
