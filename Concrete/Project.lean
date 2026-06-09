import Concrete

/-! ## Project — the project-loading boundary (ROADMAP Phase 4 #16b)

`ProjectContext` and `loadProject`, plus the dependency / TOML / module-resolution
helpers they need, live here as a **boundary** library module so external tooling
(editor/LSP, MCP, package managers) can discover and load a Concrete project
without shelling out to the CLI or importing compiler internals.

Built up in small verified cuts (#16b): path/IO leaves first, then module
resolution, then TOML/deps/registry, then `ProjectContext` / `loadProject`. -/

namespace Concrete

-- Path / IO leaf helpers (#16b stage 1).

def readFile (path : String) : IO String := do
  IO.FS.readFile ⟨path⟩

/-- Check if a module is an empty stub from `mod X;` declaration. -/
def isModuleStub (m : Module) : Bool :=
  m.functions.isEmpty && m.structs.isEmpty && m.enums.isEmpty &&
  m.imports.isEmpty && m.implBlocks.isEmpty && m.traits.isEmpty &&
  m.traitImpls.isEmpty && m.constants.isEmpty && m.typeAliases.isEmpty &&
  m.externFns.isEmpty && m.newtypes.isEmpty && m.submodules.isEmpty

/-- Get directory of a file path. -/
def dirOf (path : String) : String :=
  let parts := path.splitOn "/"
  match parts.reverse with
  | _ :: rest => "/".intercalate rest.reverse
  | [] => "."

end Concrete
