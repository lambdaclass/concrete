import Concrete.Pipeline.Pipeline

/-! Browser entry point. Uses the production passes; no file resolution, subprocesses,
    stdlib loading, execution or proof issuance. The same function is tested natively. -/
open Concrete

namespace Concrete.Browser

private def quote (s : String) : String :=
  "\"" ++ s.foldl (fun a c => a ++ match c with
    | '"' => "\\\"" | '\\' => "\\\\" | '\n' => "\\n" | '\r' => "\\r" | '\t' => "\\t"
    | c => if c.toNat < 32 then " " else String.singleton c) "" ++ "\""

private def field (name value : String) := quote name ++ ":" ++ value
private def obj (fields : List (String × String)) :=
  "{" ++ ",".intercalate (fields.map fun (k,v) => field k v) ++ "}"
private def arr (xs : List String) := "[" ++ ",".intercalate xs ++ "]"

private def fnJson (moduleName : String) (f : CFnDef) : String := obj [
  ("name", quote (moduleName ++ "::" ++ f.name)), ("text", quote (ppCFnDef f)),
  ("line", toString (f.declSpan.map (·.line) |>.getD 0)),
  ("endLine", toString (f.declSpan.map (·.endLine) |>.getD 0))]

private partial def functions (m : CModule) : List String :=
  m.functions.map (fnJson m.name) ++ m.submodules.flatMap functions

private def coreStage (name : String) (ms : List CModule) := obj [
  ("name", quote name), ("text", quote ("\n\n".intercalate (ms.map ppCModule))),
  ("functions", arr (ms.flatMap functions))]

private def result (source : String) (ok : Bool) (stages : List String) (ds : Diagnostics) := obj [
  ("schemaVersion", "1"), ("ok", if ok then "true" else "false"),
  ("target", quote Backend.targetTriple),
  ("stages", arr stages), ("diagnostics", arr (ds.map Diagnostic.toJson)),
  ("diagnosticsText", quote ("\n\n".intercalate (ds.map (·.render [("main.con", source)]))))]

private abbrev BrowserM := ExceptT Diagnostics (StateM (List String))

private def run (source : String) : BrowserM Unit := do
  let parsed ← liftExcept (Pipeline.parse source)
  let summary := Pipeline.buildSummary parsed
  let resolved ← liftExcept (Pipeline.resolve parsed summary)
  let resolved := Pipeline.desugar resolved
  liftExcept (Pipeline.check resolved summary)
  let elaborated ← liftExcept (Pipeline.elaborate resolved summary)
  let core ← liftExcept (Pipeline.coreCheck elaborated)
  modify (· ++ [coreStage "Core" core.coreModules])
  let mono ← liftExcept (Pipeline.monomorphize core)
  modify (· ++ [coreStage "Specialized Core" mono.coreModules])
  let ssa ← liftExcept (Pipeline.lower mono)
  modify (· ++ [obj [("name", quote "SSA"),
      ("text", quote ("\n\n".intercalate (ssa.ssaModules.map ppSModule))),
      ("functions", arr (ssa.ssaModules.flatMap fun m => m.functions.map fun f => obj [
        ("name", quote (m.name ++ "::" ++ f.name)), ("text", quote (ppSFnDef f)),
        ("line", toString (f.declSpan.map (·.line) |>.getD 0))]))]])
  modify (· ++ [obj [("name", quote "LLVM"), ("text", quote (Pipeline.emit ssa)), ("functions", "[]")]])

/-- JSON for one self-contained source file. Successful checking is not a proof verdict.
    Completed stages survive downstream errors, but are never described as full success. -/
@[export concrete_browser_compile]
def compile (source : String) : String :=
  let (verdict, stages) := (run source).run.run []
  match verdict with
  | .ok () => result source true stages []
  | .error ds => result source false stages ds

end Concrete.Browser

def main (args : List String) : IO Unit := do
  let source ← match args with
    | [path] => IO.FS.readFile path
    | _ => (← IO.getStdin).readToEnd
  IO.println (Concrete.Browser.compile source)
