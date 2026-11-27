import Concrete.Lex
import Concrete.MIR
import ConcreteSpec.Spec.ProjectPolicy

/-! Project-file I/O at the compiler boundary. Wire decoding is held until the schema review. -/

namespace Concrete.Driver.Project
open ConcreteSpec.Core (PackageId FnId)
open ConcreteSpec.Project

def manifestByteLimit : Nat := Manifest.byteLimit

def manifestDepthLimit : Nat := Manifest.depthLimit

/-- Validate the byte envelope before any decoder can inspect it. -/
def manifestText (bytes : ByteArray) : Except ManifestRefusal String := do
  if ¬ Manifest.SizeAccepted bytes.size then throw .size
  match Lex.decodeUtf8 bytes.data.toList with
  | .ok scalars => return ConcreteSpec.Surface0.scalarText scalars
  | .error error => throw (.encoding ⟨error.offset, min (error.offset + 1) bytes.size⟩)


/-- Check the bounded syntax supported by the configuration boundary. -/
def parseManifest (text : String) : Except ManifestRefusal Unit :=
  if ¬ Manifest.SizeAccepted text.utf8ByteSize then .error .size
  else (Manifest.validate manifestDepthLimit text).mapError ManifestRefusal.invalid

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
      let mut bytes := ByteArray.empty
      repeat
        let remaining := manifestByteLimit + 1 - bytes.size
        if remaining == 0 then return .error .size
        let block ← handle.read remaining.toUSize
        if block.isEmpty then break
        bytes := bytes ++ block
      return (manifestText bytes).map some
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

/-- Slice 0 MIR contains a single straight-line function. Classify its operations exhaustively
so that adding an operation requires revisiting the policy facts. Stack usage requires backend
evidence, so it is explicitly unknown here. -/
def mirFacts (id : FnId) (program : MIR.Program) : FunctionFacts :=
  let straightLine := program.body.all fun assignment =>
    match assignment.op with
    | .copy _ | .add .. => true
  ⟨id, .established (!straightLine) [] .function, .established (!straightLine) [] .function,
    .established (!straightLine) [] .function, .established straightLine [] .function,
    .unknown .function⟩

/-- A coverage entry records the complete conjunction checked for the function. -/
structure Coverage where
  function : FnId
  requirements : List Requirement
  deriving Repr

/-- Consumer requirements are inherited without allowing a lower scope to remove or weaken them.
An absent function inventory must fail closed even when no local violation has been observed. -/
inductive Scope where
  | unresolved (package : PackageId)
  | resolved (requirements : List Requirement) (functions : List FunctionFacts)
      (dependencies : List Scope)
  deriving Repr

inductive ScopeRefusal where
  | unresolved (package : PackageId)
  | policy (refusal : PolicyRefusal)
  deriving Repr

def checkScope (inherited : List Requirement) : Scope → Except ScopeRefusal (List Coverage)
  | .unresolved name => .error (.unresolved name)
  | .resolved requirements functions dependencies => do
    let effective := inherited ++ requirements
    for facts in functions do
      for requirement in effective do
        (checkClause requirement facts).mapError ScopeRefusal.policy
    return functions.map (fun f => ⟨f.name, effective⟩) ++ (← visit effective dependencies)
where
  visit (effective : List Requirement) : List Scope → Except ScopeRefusal (List Coverage)
    | [] => .ok []
    | dependency :: rest => do
      return (← checkScope effective dependency) ++ (← visit effective rest)

end Concrete.Driver.Project
