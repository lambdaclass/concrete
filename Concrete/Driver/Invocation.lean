import Concrete.Driver.Manifest
import ConcreteSpec.Spec.ProjectPolicy

/-! Project-file I/O at the invocation boundary. -/

namespace Concrete.Driver.Project
open ConcreteSpec.Project

-- A successful nonempty read consumes at least one unit of fuel. With one byte
-- beyond the manifest limit, exhausting that budget proves the file is too large.
private def readManifestBytes (handle : IO.FS.Handle) :
    Nat → ByteArray → IO (Except ManifestRefusal (Option String))
  | 0, _ => pure (.error .size)
  | fuel + 1, bytes => do
    let remaining := manifestByteLimit + 1 - bytes.size
    if remaining == 0 then return .error .size
    let block ← handle.read remaining.toUSize
    if block.isEmpty then return (manifestText bytes).map some
    readManifestBytes handle fuel (bytes ++ block)

/-- Only an absent directory entry denotes single-file mode. Symlinks, inaccessible
entries and special files are refused. -/
def readManifest (path : System.FilePath) : IO (Except ManifestRefusal (Option String)) := do
  let metadata ← path.symlinkMetadata.toBaseIO
  match metadata with
  | .error (.noFileOrDirectory ..) => return .ok none
  | .error e => return .error (.read (toString e))
  | .ok entry =>
    if entry.type != .file then return .error (.read "expected a regular file")
    try
      let handle ← IO.FS.Handle.mk path .read
      return ← readManifestBytes handle (manifestByteLimit + 1) ByteArray.empty
    catch e =>
      return .error (.read (toString e))

/-- A present manifest cannot be treated as a single-file build while wire decoding is pending. -/
def checkInvocation (path : System.FilePath) :
    IO (Except ManifestRefusal LanguageRulesVersion) := do
  match ← readManifest path with
  | .error e => return .error e
  | .ok none => return (selectLanguage supportedLanguage .standalone).mapError fun _ => .schemaPending
  | .ok (some text) =>
    return parseManifest text >>= fun _ => .error .schemaPending

end Concrete.Driver.Project
