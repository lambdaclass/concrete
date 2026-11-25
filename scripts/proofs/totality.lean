import Lean

open Lean

def moduleOf (env : Environment) (n : Name) : Name :=
  ((env.getModuleIdxFor? n).bind fun i => env.header.moduleNames[i.toNat]?).getD .anonymous

def violation (env : Environment) (n : Name) (ci : ConstantInfo) : Option String := do
  let root := (moduleOf env n).getRoot
  -- Runtime primitives from the pinned Lean toolchain are the execution base.
  -- Their dependency edges are still followed. For example, the compiler's
  -- uses of ByteArray and UInt8 depend on the toolchain's native implementations.
  if root == `Init || root == `Std || root == `Lean then none
  else if ci.isUnsafe then some "unsafe"
  else if ci matches .defnInfo { safety := .partial, .. } then some "partial"
  else if Compiler.getImplementedBy? env n |>.isSome then some "implemented_by"
  else if getExternAttrData? env n |>.isSome then some "extern"
  else if isNoncomputable env n then some "noncomputable"
  else if ci matches .opaqueInfo _ then some "opaque"
  else if ci matches .axiomInfo _ then some "axiom"
  else none

-- Lean attaches a native recursion helper only after proving the kernel
-- definition terminates. Audit that kernel definition, not the generated helper.
def recursiveImplementation (env : Environment) (n : Name) : Bool :=
  match Compiler.isUnsafeRecName? n with
  | some parent =>
    match env.find? parent with
    | some (.defnInfo d) => d.safety == .safe && moduleOf env parent == moduleOf env n &&
        (declRangeExt.find? (level := .private) env n).isNone &&
        (declRangeExt.find? (level := .server) env n).isNone
    | _ => false
  | _ => false

def proofOnly (ci : ConstantInfo) : Bool :=
  (ci matches .thmInfo _) || ci.type.getForallBody == .sort .zero

def inspect (env : Environment) (roots : Array Name) (pureStage : Bool) : IO (Array String) := do
  let mut pending := roots.map fun n => (n, n)
  let mut seen : Std.HashSet Name := {}
  let mut failures := #[]
  let mut idx := 0
  while idx < pending.size do
    let (n, caller) := pending[idx]!
    idx := idx + 1
    if seen.contains n then continue
    seen := seen.insert n
    let some ci := env.find? n | throw (IO.userError s!"missing declaration {n} from {caller}")
    -- Proof terms do not run. In particular, proof-only semantic definitions may
    -- refer to classical choice without becoming part of the executable closure.
    if proofOnly ci then continue
    if pureStage && [`IO, `BaseIO, `EIO, `EST].any (·.isPrefixOf n) then
      failures := failures.push s!"{caller} reaches IO declaration {n}"
      continue
    if let some reason := violation env n ci then
      failures := failures.push s!"{caller} reaches {reason} declaration {n}"
      continue
    if ci matches .defnInfo _ then
      for dep in ci.getUsedConstantsAsSet do pending := pending.push (dep, n)
  return failures

def main (args : List String) : IO UInt32 := do
  let [target, stage] := args | throw (IO.userError "expected driver and compiler roots")
  unsafe enableInitializersExecution
  initSearchPath (← findSysroot)
  let env ← importModules #[{module := target.toName}, {module := stage.toName}] {} (loadExts := true)
  let mut failures := #[]
  let mut pending := #[stage.toName]
  let mut modules : Std.HashSet Name := {}
  let mut idx := 0
  let moduleIndices := Std.HashMap.ofList (env.header.moduleNames.toList.zipIdx)
  while idx < pending.size do
    let m := pending[idx]!
    idx := idx + 1
    if modules.contains m then continue
    modules := modules.insert m
    if m.getRoot == `Lean || m.getRoot == `ConcreteTooling then
      failures := failures.push s!"compiler imports forbidden module {m}"
      continue
    let some i := moduleIndices[m]? | throw (IO.userError s!"missing module {m}")
    for dep in env.header.moduleData[i]!.imports do pending := pending.push dep.module
  let mut stages := #[]
  let mut commands := #[]
  for (n, ci) in env.constants.toList do
    let m := moduleOf env n
    if recursiveImplementation env n then continue
    if !(ci matches .defnInfo _ | .opaqueInfo _ | .axiomInfo _) then continue
    if m == target.toName then commands := commands.push n
    if (m.getRoot == `Concrete && modules.contains m || m == stage.toName) &&
        n != `Concrete.BIR.behaviour then
      stages := stages.push n
  if stages.isEmpty || commands.isEmpty then
    throw (IO.userError "empty stage or driver definition population")
  failures := failures ++ (← inspect env stages true) ++ (← inspect env commands false)
  for msg in failures do IO.eprintln msg
  return if failures.isEmpty then 0 else 1
