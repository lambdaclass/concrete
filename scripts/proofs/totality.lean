import Lean

open Lean

def moduleOf (env : Environment) (n : Name) : Name :=
  ((env.getModuleIdxFor? n).bind fun i => env.header.moduleNames[i.toNat]?).getD .anonymous

-- The pinned Init and Std implementations provide the runtime primitives.
def runtimeModule (m : Name) : Bool :=
  m.getRoot == `Init || m.getRoot == `Std

def violation (env : Environment) (n : Name) (ci : ConstantInfo) : Option String := do
  if ci.isUnsafe then some "unsafe"
  else if ci matches .defnInfo { safety := .partial, .. } then some "partial"
  else if Compiler.getImplementedBy? env n |>.isSome then some "implemented_by"
  else if getExternAttrData? env n |>.isSome then some "extern"
  else if isNoncomputable env n then some "noncomputable"
  else if ci matches .opaqueInfo _ then some "opaque"
  else if ci matches .axiomInfo _ then some "axiom"
  else none

-- Each exception is tied to a declaration, its observed restriction, and
-- the pinned runtime artifact. These are the scalar/container primitives,
-- proof-erasure supports, and optimized implementations used by this compiler.
-- Adding another opaque implementation or substitution requires auditing it.
def namedRuntimeDependency (n : Name) (allowed : Array String) : Bool :=
  allowed.contains n.toString

def runtimeException (n : Name) (reason : String) : Bool :=
  match reason with
  | "extern" => namedRuntimeDependency n #["Array.emptyWithCapacity", "Array.getInternal", "Array.mk", "Array.mkEmpty", "Array.propagateMark",
    "Array.push", "Array.replicate", "Array.set", "Array.size", "Array.swap", "Array.toList", "Array.uget",
    "Array.uset", "Array.usize", "ByteArray.beq", "ByteArray.copySlice", "ByteArray.data",
    "ByteArray.emptyWithCapacity", "ByteArray.get", "ByteArray.mk", "ByteArray.push", "ByteArray.size",
    "ByteArray.validateUTF8", "Char.ofNatAux", "Int.add", "Int.decEq", "Int.decLe", "Int.decLt",
    "Int.decNonneg", "Int.ediv", "Int.emod", "Int.mul", "Int.natAbs", "Int.neg", "Int.negSucc", "Int.ofNat",
    "Int.sub", "Nat.add", "Nat.beq", "Nat.ble", "Nat.decEq", "Nat.decLe", "Nat.decLt", "Nat.div", "Nat.land",
    "Nat.log2", "Nat.lor", "Nat.mod", "Nat.modCore", "Nat.mul", "Nat.pow", "Nat.pred", "Nat.shiftLeft",
    "Nat.shiftRight", "Nat.sub", "Nat.xor", "String.Internal.append", "String.Internal.foldl",
    "String.Internal.isEmpty", "String.Internal.length", "String.Internal.pushn",
    "String.Pos.Raw.atEnd", "String.Pos.Raw.extract", "String.Pos.Raw.get", "String.Pos.Raw.next",
    "String.Pos.Raw.set", "String.Pos.next", "String.Slice.Pattern.Internal.memcmpStr", "String.append",
    "String.decEq", "String.decodeChar", "String.extract", "String.getUTF8Byte", "String.hash",
    "String.length", "String.ofByteArray", "String.ofList", "String.push", "String.toByteArray",
    "String.toList", "String.toUTF8", "String.utf8ByteSize",
    "UInt32.add", "UInt32.decEq", "UInt32.decLe", "UInt32.decLt",
    "UInt32.lor", "UInt32.mod", "UInt32.ofBitVec", "UInt32.ofNat", "UInt32.ofNatLT", "UInt32.shiftLeft",
    "UInt32.shiftRight", "UInt32.sub", "UInt32.toBitVec", "UInt32.toNat", "UInt32.toUInt8",
    "UInt64.decEq", "UInt64.decLe", "UInt64.decLt", "UInt64.land", "UInt64.mod", "UInt64.ofBitVec",
    "UInt64.ofNat", "UInt64.ofNatLT", "UInt64.shiftRight", "UInt64.sub", "UInt64.toBitVec",
    "UInt64.toNat", "UInt64.toUSize", "UInt64.xor", "UInt8.decEq", "UInt8.land", "UInt8.lor",
    "UInt8.ofBitVec", "UInt8.ofNat", "UInt8.ofNatLT", "UInt8.toBitVec", "UInt8.toNat", "UInt8.toUInt32",
    "USize.add", "USize.decEq", "USize.decLt", "USize.land", "USize.ofBitVec", "USize.ofNat",
    "USize.ofNatLT", "USize.repr", "USize.sub", "USize.toBitVec", "USize.toNat", "mixHash"]
  | "noncomputable" => namedRuntimeDependency n #["Bool.and", "Bool.not", "Bool.or", "Classical.ofNonempty", "Classical.propDecidable",
    "List.filterMap", "List.flatMap", "List.flatten", "List.intercalate", "List.utf8Encode",
    "Nat.modCore.go", "WellFounded.fix", "WellFounded.fixF"]
  | "axiom" => namedRuntimeDependency n #["Classical.choice", "propext"]
  | "implemented_by" => namedRuntimeDependency n #["Array.anyM", "Array.foldlM", "Array.foldrM", "Array.forIn'", "Array.mapFinIdxM", "Array.mapM",
    "Array.modifyM", "Lean.Name.anonymous", "Lean.Name.casesOn", "Lean.Name.hash", "Lean.Name.num",
    "Lean.Name.str", "List.attachWith", "Nat.repr"]
  | "opaque" => namedRuntimeDependency n #["_private.Init.Data.Format.Basic.0.Std.Format.be"]
  | "unsafe" => namedRuntimeDependency n #["Array.anyMUnsafe", "Array.foldlMUnsafe", "Array.foldrMUnsafe", "Array.forIn'Unsafe",
    "Array.getInternalBorrowed", "Array.mapFinIdxMUnsafe", "Array.mapMUnsafe", "Array.modifyMUnsafe",
    "Array.ugetBorrowed", "Lean.Name.anonymous._override", "Lean.Name.casesOn._override",
    "Lean.Name.hash._override", "Lean.Name.num._override", "Lean.Name.str._override",
    "_private.Init.Data.Array.Basic.0.Array.anyMUnsafe.any",
    "_private.Init.Data.Array.Basic.0.Array.foldlMUnsafe.fold",
    "_private.Init.Data.Array.Basic.0.Array.foldrMUnsafe.fold",
    "_private.Init.Data.Array.Basic.0.Array.forIn'Unsafe.loop",
    "_private.Init.Data.Array.Basic.0.Array.mapFinIdxMUnsafe.map",
    "_private.Init.Data.Array.Basic.0.Array.mapMUnsafe.map",
    "_private.Init.Data.Array.Basic.0.Array.modifyMUnsafe._proof_1",
    "_private.Init.Data.List.Attach.0.List.attachWithImpl", "lcProof", "unsafeCast",
    "unsafeCast._proof_1"]
  | _ => false

-- File reading and resource management are part of the executable boundary.
def driverRuntimeException (n : Name) (reason : String) : Bool :=
  match reason with
  | "extern" => namedRuntimeDependency n #["BaseIO.asTask", "IO.FS.Handle.flush", "IO.FS.Handle.mk", "IO.FS.Handle.putStr", "IO.FS.Handle.read",
    "IO.FS.realPath", "IO.Process.Child.takeStdin", "IO.Process.Child.wait", "IO.Process.spawn",
    "IO.getStderr", "IO.getStdout", "System.FilePath.metadata", "System.Platform.getIsWindows", "Task.get", "Task.pure"]
  | "opaque" => namedRuntimeDependency n #["IO.FS.Handle", "IO.FS.removeDirAll", "IO.RealWorld.nonemptyType", "Void.nonemptyType",
    "_private.Init.System.IO.0.IO.FS.Handle.readBinToEndInto.loop"]
  | _ => false

-- The pinned backend and the driver path routines use bounded string
-- iterators. Grant their implementation dependencies only on those paths.
def iteratorRuntimeException (n : Name) : Bool :=
  namedRuntimeDependency n #["WellFounded.extrinsicFix", "WellFounded.extrinsicFix₂", "WellFounded.extrinsicFix₃",
    "WellFounded.opaqueFix₃", "_private.Init.Data.Iterators.Basic.0.Std.Internal.idOpaque",
    "_private.Init.WFExtrinsicFix.0.WellFounded.opaqueFix",
    "_private.Init.WFExtrinsicFix.0.WellFounded.opaqueFix₂"]

-- These containers bundle checked and panic-on-failure lookup in one instance.
-- Check elimination of the class and selection of its unchecked field
-- separately, including when Lean reduces it to an internal native primitive.
def auditedRuntimeEdge (caller dep : Name) : Bool :=
  (caller == `Array.instGetElem?NatLtSize && dep == `Array.get!Internal) ||
  (caller == `ByteArray.toList.loop._unary && dep == `ByteArray.get!) ||
  (caller == `List.instGetElem?NatLtLength && dep == `List.get!Internal) ||
  (caller == `Std.HashMap.instGetElem?Mem && dep == `Std.HashMap.get!)

def wordSized (env : Environment) (n : Name) : Bool :=
  [`USize, `ISize].any (·.isPrefixOf n) ||
    (env.find? n).any fun ci =>
      let constants := ci.type.getUsedConstants
      constants.contains `USize || constants.contains `ISize

-- Only these pinned implementations can enter the word-sized operations.
-- Array traversal uses in-bounds indices, hash-table operations use bucket
-- indices internally, and decimal rendering falls back to Nat for wide input.
def auditedWordSizeCaller (n : Name) : Bool :=
  namedRuntimeDependency n #[
    "Array.anyMUnsafe", "Array.foldlMUnsafe", "Array.foldrMUnsafe", "Array.forIn'Unsafe",
    "Array.mapFinIdxMUnsafe", "Array.mapMUnsafe",
    "_private.Init.Data.Array.Basic.0.Array.anyMUnsafe.any",
    "_private.Init.Data.Array.Basic.0.Array.foldlMUnsafe.fold",
    "_private.Init.Data.Array.Basic.0.Array.foldrMUnsafe.fold",
    "_private.Init.Data.Array.Basic.0.Array.forIn'Unsafe.loop",
    "_private.Init.Data.Array.Basic.0.Array.mapFinIdxMUnsafe.map",
    "_private.Init.Data.Array.Basic.0.Array.mapMUnsafe.map",
    "Nat.reprFast", "_private.Init.Data.Repr.0.Nat.reprArray",
    "Std.DHashMap.Internal.Raw₀.Const.get", "Std.DHashMap.Internal.Raw₀.Const.get?",
    "Std.DHashMap.Internal.Raw₀.contains", "Std.DHashMap.Internal.Raw₀.insert",
    "Std.DHashMap.Internal.Raw₀.insertIfNew", "Std.DHashMap.Internal.Raw₀.reinsertAux"]

-- These primitives account for the audited internal uses of the width value.
-- Any other path to a platform query is checked by the ordinary effect audit.
def wordSizeRepresentationEdge (caller dep : Name) : Bool :=
  dep == `System.Platform.numBits && namedRuntimeDependency caller #[
    "USize.add", "USize.decEq", "USize.decLt", "USize.land", "USize.ofBitVec", "USize.ofNat",
    "USize.ofNatLT", "USize.size", "USize.sub", "USize.toBitVec", "USize.toNat"]

def panicEntry (n : Name) : Bool :=
  #[`panicCore, `GetElem?.getElem!, `GetElem?.casesOn, `GetElem?.rec, `Array.get!Internal, `Array.get!InternalBorrowed,
    `Array.set!, `ByteArray.get!].contains n

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

-- Lean lifts lambdas and matches from theorem bodies and types into named
-- kernel helpers. Only reach these from an executable root that uses them.
def proofAuxiliary (env : Environment) : Name → Bool
  | .anonymous => false
  | .str parent _ | .num parent _ =>
    (env.find? parent).any proofOnly || proofAuxiliary env parent

def effectfulExtern (data : ExternAttrData) : Bool :=
  data.entries.any fun entry =>
    let symbol := match entry with
      | .standard _ symbol | .inline _ symbol => symbol
      | _ => ""
    ["lean_io_", "lean_dbg_", "lean_task_", "lean_st_", "lean_panic",
      "lean_internal_get_hardware_concurrency"].any (fun stem => (symbol.splitOn stem).length > 1)

def effectful (env : Environment) (n : Name) : Bool :=
  [`IO, `BaseIO, `EIO, `ST, `EST, `Task].any (·.isPrefixOf n) ||
    n == `runST || n == `runEST ||
    (`System.Platform).isPrefixOf n ||
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
  | .vdecl _ _ _ next | .set _ _ _ next | .setTag _ _ next |
    .uset _ _ _ next | .sset _ _ _ _ _ next | .inc _ _ _ _ next |
    .dec _ _ _ _ next | .del _ next => nativeCalls next calls

def inspect (env : Environment) (rewrites : Rewrites) (native : NativeDecls)
    (backendModules : Std.HashSet Name) (roots : Array Name)
    (pureStage : Bool) : IO (Array String) := do
  let mut pending := roots.map fun n => (n, n, false, false)
  let mut seen : Std.HashSet (Name × Bool × Bool) := {}
  let mut failures := #[]
  let mut idx := 0
  while idx < pending.size do
    let (n, caller, inheritedBackend, inheritedIterator) := pending[idx]!
    idx := idx + 1
    let ir := native[n]?
    let owner := moduleOf env n
    let module := if owner.isAnonymous then (ir.map Prod.fst).getD owner else owner
    let fromBackend := if runtimeModule module then inheritedBackend else backendModules.contains module
    let boundedIterator := fromBackend || (runtimeModule module &&
      (inheritedIterator || (!pureStage && [`System.FilePath.isAbsolute, `System.FilePath.extension].contains n)))
    if seen.contains (n, fromBackend, boundedIterator) then continue
    seen := seen.insert (n, fromBackend, boundedIterator)
    if let some (m, decl) := ir then
      match decl with
      | .fdecl _ _ _ body _ =>
        unless runtimeModule m do
          for dep in nativeCalls body do pending := pending.push (dep, n, fromBackend, boundedIterator)
      | .extern _ _ _ data =>
        if pureStage && effectfulExtern data && !(fromBackend && panicEntry n) then
          failures := failures.push s!"{caller} reaches stage effect {n}"
        if !runtimeModule m && !data.entries.all (· == .opaque) then
          failures := failures.push s!"{caller} reaches native extern {n}"
    let some ci := env.find? n | do
      if ir.isNone then
        failures := failures.push s!"missing declaration {n} from {caller}"
      continue
    if proofOnly ci then continue
    -- A projection inserted by elaboration can select the unchecked field
    -- without referencing its named accessor.
    if pureStage && !fromBackend &&
        (ci.value? false).any (fun value =>
          (value.find? fun expr => expr matches .proj `GetElem? 2 _).isSome) then
      failures := failures.push s!"{n} selects the unchecked GetElem? field"
    for (m, rewrite) in rewrites[n]?.getD #[] do
      if !runtimeModule m then
        failures := failures.push s!"{n} has csimp replacement {rewrite.toDeclName} registered by {rewrite.thmName} in {m}"
      pending := pending.push (rewrite.toDeclName, n, fromBackend, boundedIterator)
    if let some implementation := Compiler.getImplementedBy? env n then
      pending := pending.push (implementation, n, fromBackend, boundedIterator)
    if pureStage && (effectful env n || panicEntry n) && !(fromBackend && panicEntry n) then
      failures := failures.push s!"{caller} reaches stage effect {n}"
      continue
    if let some parent := recursiveParent env n then
      pending := pending.push (parent, n, fromBackend, boundedIterator)
      continue
    if let some reason := violation env n ci then
      unless runtimeModule (moduleOf env n) &&
          (runtimeException n reason || (!pureStage && driverRuntimeException n reason) ||
            (fromBackend && panicEntry n && (reason == "extern" || reason == "unsafe")) ||
            (boundedIterator && iteratorRuntimeException n) ||
            -- The driver's file-path routines advance string positions under a bounds check.
            (!pureStage && boundedIterator && n == `panicCore && reason == "extern")) do
        failures := failures.push s!"{caller} reaches {reason} declaration {n}"
    let mut kernelDeps := ci.getUsedConstantsAsSet
    if let .recInfo r := ci then
      for rule in r.rules do
        for dep in rule.rhs.getUsedConstants do kernelDeps := kernelDeps.insert dep
    -- Check each logical edge, even when another caller already reached the
    -- target. IR specializations of pinned container code can also contain
    -- machine-word calls, so their logical implementation is checked here.
    let pinnedCaller := runtimeModule module
    let allowsWordSize := pinnedCaller && (wordSized env n || auditedWordSizeCaller n)
    for dep in kernelDeps do
      if pureStage && !allowsWordSize && runtimeModule (moduleOf env dep) && wordSized env dep then
        failures := failures.push s!"{n} reaches host-word-sized declaration {dep}"
      unless pinnedCaller && (auditedRuntimeEdge n dep || wordSizeRepresentationEdge n dep) do
        pending := pending.push (dep, n, fromBackend, boundedIterator)
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
  let backendRoot ← IO.getEnv "PROOF_TOTALITY_BACKEND"
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
  let mut backendModules : Std.HashSet Name := {}
  for (m, idx) in env.header.moduleNames.toList.zipIdx do
    if !stageModules.contains m && !driverModules.contains m then continue
    if runtimeModule m then
      let artifact ← IO.FS.realPath (← findOLean m)
      if !artifact.toString.startsWith (runtimeRoot.toString ++ "/") then
        failures := failures.push s!"{m} shadows the pinned runtime: {artifact}"
    else
      if m.getRoot == `LeanCompilerBackend then
        if let some root := backendRoot then
          let artifact ← IO.FS.realPath (← findOLean m)
          let source ← IO.FS.realPath (← findLean sourcePath m)
          if artifact.toString.startsWith (root ++ "/.lake/build/lib/lean/") &&
              source.toString.startsWith (root ++ "/LeanCompilerBackend/") then
            backendModules := backendModules.insert m
      if sourceManifest.isSome then
        sources := sources.push (← IO.FS.realPath (← findLean sourcePath m)).toString
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
    if proofOnly ci then continue
    if proofAuxiliary env n &&
        (declRangeExt.find? (level := .private) env n).isNone &&
        (declRangeExt.find? (level := .server) env n).isNone then continue
    if m == target.toName then commands := commands.push n
    if stageModules.contains m && (m.getRoot == `Concrete || m == stage.toName) &&
        n != `Concrete.BIR.behaviour then
      stages := stages.push n
    if driverModules.contains m && !stageModules.contains m && !runtimeModule m &&
        m != target.toName then
      commands := commands.push n
  if stages.isEmpty || commands.isEmpty then
    throw (IO.userError "empty stage or driver definition population")
  failures := failures ++ (← inspect env rewrites native backendModules stages true) ++ (← inspect env rewrites native backendModules commands false)
  for msg in failures do IO.eprintln msg
  if !failures.isEmpty then return 1
  if let some manifest := sourceManifest then
    if sources.isEmpty then throw (IO.userError "empty closure source population")
    IO.FS.writeFile manifest (String.intercalate "\n" sources.toList ++ "\n")
  return 0
