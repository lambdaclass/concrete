import ConcreteSpec.Spec.ProjectPolicy

/-! Project-file I/O at the compiler boundary. Wire decoding is held until the schema review. -/

namespace Concrete.Driver.Project
open ConcreteSpec.Core (RuleId)
open ConcreteSpec.Project

def manifestByteLimit : Nat := 64 * 1024

def manifestDepthLimit : Nat := 16

inductive Refusal where
  | read (detail : String)
  | size
  | encoding
  | depth
  | malformed
  | schemaPending
  deriving DecidableEq, Repr

def Refusal.ruleId : Refusal → RuleId
  | .read _ => ⟨``Refusal.read, 1⟩
  | .size => ⟨``Refusal.size, 1⟩
  | .encoding => ⟨``Refusal.encoding, 1⟩
  | .depth => ⟨``Refusal.depth, 1⟩
  | .malformed => ⟨``Refusal.malformed, 1⟩
  | .schemaPending => ⟨``Refusal.schemaPending, 1⟩

def Refusal.message : Refusal → String
  | .read detail => s!"cannot read Concrete.toml ({detail})"
  | .size => s!"Concrete.toml exceeds {manifestByteLimit} bytes"
  | .encoding => "Concrete.toml must be UTF-8"
  | .depth => s!"Concrete.toml exceeds the nesting limit of {manifestDepthLimit}"
  | .malformed => "Concrete.toml is malformed"
  | .schemaPending => "Concrete.toml requires an approved manifest schema"

/-- Validate the byte envelope before any decoder can inspect it. -/
def manifestText (bytes : ByteArray) : Except Refusal String := do
  if bytes.size > manifestByteLimit then throw .size
  let some value := String.fromUTF8? bytes | throw .encoding
  return value


-- A depth preflight for Lake's TOML parser. Keep strings and comments opaque.
-- Dotted paths are counted separately because elaboration expands them into nested tables.
private inductive Scan where
  | code
  | comment
  | quoted (delimiter : Char) (multiline escaped : Bool)

private def withinDepth : Nat → List Char → Scan → Nat → Nat → Bool
  | _, [], _, _, _ => true
  | 0, _ :: _, _, _, _ => false
  | fuel + 1, ch :: rest, .comment, depth, path =>
    if ch == '\n' then withinDepth fuel rest .code depth 1
    else withinDepth fuel rest .comment depth path
  | fuel + 1, ch :: rest, .quoted delim multiline escaped, depth, path =>
    if escaped then withinDepth fuel rest (.quoted delim multiline false) depth path
    else if delim == '"' && ch == '\\' then withinDepth fuel rest (.quoted delim multiline true) depth path
    else if ch != delim then withinDepth fuel rest (.quoted delim multiline false) depth path
    else if !multiline then withinDepth fuel rest .code depth path
    else
      match rest with
      | a :: b :: tail =>
        if a == delim && b == delim then
          -- A closing run of 4 or 5 quotes also includes literal quotes.
          match tail with
          | c :: d :: after =>
            if c == delim && d == delim then withinDepth fuel after .code depth path
            else if c == delim then withinDepth fuel (d :: after) .code depth path
            else withinDepth fuel tail .code depth path
          | [c] => if c == delim then true else withinDepth fuel tail .code depth path
          | [] => true
        else withinDepth fuel rest (.quoted delim true false) depth path
      | _ => withinDepth fuel rest (.quoted delim true false) depth path
  | fuel + 1, ch :: rest, .code, depth, path =>
    if ch == '#' then withinDepth fuel rest .comment depth path
    else if ch == '"' || ch == '\'' then
      match rest with
      | a :: b :: tail =>
        if a == ch && b == ch then withinDepth fuel tail (.quoted ch true false) depth path
        else withinDepth fuel rest (.quoted ch false false) depth path
      | _ => withinDepth fuel rest (.quoted ch false false) depth path
    else if ch == '[' || ch == '{' then
      depth < manifestDepthLimit && withinDepth fuel rest .code (depth + 1) 1
    else if ch == ']' || ch == '}' then withinDepth fuel rest .code (depth - 1) 1
    else if ch == '.' then
      path < manifestDepthLimit && withinDepth fuel rest .code depth (path + 1)
    else if ch == '\n' || ch == '=' || ch == ',' then withinDepth fuel rest .code depth 1
    else withinDepth fuel rest .code depth path

/-- Apply the executable's TOML parser only after the resource preflight. The project driver
and the specification have no dependency on that parser's environment. -/
def parseManifest {α : Type} (parse : String → IO (Option α)) (text : String) :
    IO (Except Refusal α) := do
  if text.utf8ByteSize > manifestByteLimit then return .error .size
  unless withinDepth manifestByteLimit text.toList .code 0 1 do return .error .depth
  let some table ← parse text | return .error .malformed
  return .ok table

/-- Only an absent directory entry denotes single-file mode. A broken link, inaccessible
entry or special file must not silently bypass project selection. -/
def readManifest (path : System.FilePath) : IO (Except Refusal (Option String)) := do
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
def checkInvocation {α : Type} (parse : String → IO (Option α)) (path : System.FilePath) :
    IO (Except Refusal Unit) := do
  match ← readManifest path with
  | .error e => return .error e
  | .ok none => return .ok ()
  | .ok (some text) =>
    return (← parseManifest parse text) >>= fun _ => .error .schemaPending

/-- Consumer requirements are inherited without allowing a lower scope to remove or weaken them.
An absent function inventory must fail closed even when no local violation has been observed. -/
inductive Scope where
  | unresolved (name : String)
  | resolved (requirements : List Requirement) (functions : List FunctionFacts)
      (dependencies : List Scope)
  deriving Repr

inductive ScopeRefusal where
  | unresolved (name : String)
  | policy (refusal : PolicyRefusal)
  deriving Repr

def checkScope (inherited : List Requirement) : Scope → Except ScopeRefusal Unit
  | .unresolved name => .error (.unresolved name)
  | .resolved requirements functions dependencies => do
    let effective := inherited ++ requirements
    for facts in functions do
      for requirement in effective do
        (checkClause requirement facts).mapError ScopeRefusal.policy
    visit effective dependencies
where
  visit (effective : List Requirement) : List Scope → Except ScopeRefusal Unit
    | [] => .ok ()
    | dependency :: rest => do
      checkScope effective dependency
      visit effective rest

end Concrete.Driver.Project
