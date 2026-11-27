import Lake.Load.Workspace

open Lake System

private def fileType? (path : FilePath) : IO (Option IO.FS.FileType) := do
  try return some (← path.metadata).type catch
    | .noFileOrDirectory .. => return none
    | e => throw e

/-- Inspect each package separately. Two packages can both own a module called Main. -/
private def prunePackage (pkg : Package) : IO Unit := do
  if (← fileType? pkg.leanLibDir).isNone then return
  let artifacts ← pkg.leanLibDir.walkDir fun dir => do
    if (← dir.symlinkMetadata).type == .symlink then
      throw <| IO.userError s!"cannot prune through directory symlink {dir}"
    return true
  for artifact in artifacts do
    unless artifact.extension == some "olean" do continue
    let rel := (relPathFrom pkg.leanLibDir artifact).withExtension ""
    let name := rel.components.foldl Lean.Name.str .anonymous
    if let some mod := pkg.findTargetModule? name then
      if (← fileType? mod.leanFile) == some .file then continue
    -- Invalidate the trace first, so reintroducing the source rebuilds its facets.
    removeFileIfExists (artifact.withExtension "trace")
    for ext in ["olean.private", "olean.server", "olean"] do
      let part := artifact.withExtension ext
      removeFileIfExists (part.addExtension "hash")
      removeFileIfExists part
    IO.println s!"pruned orphan {artifact}"

/-- Run after cache restore with LEAN_PATH unset, using only the pinned toolchain. -/
def main : IO Unit := do
  let (elan, some lean, some lake) ← findInstall?
    | throw <| IO.userError "could not locate Lean and Lake"
  let lakeEnv ← (Env.compute lake lean elan).toIO IO.userError
  let some ws ← (loadWorkspace {lakeEnv, wsDir := ← IO.currentDir}).toBaseIO
    | throw <| IO.userError "could not load the Lake workspace"
  for pkg in ws.packages do
    prunePackage pkg
