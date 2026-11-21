/- The runner supplies the imports. Walk elaborated definitions reachable from the module
   printer, including generated match helpers and any new project-local printing helpers.
   The decimal formatter is justified by decimal_token. The escape routine is used for
   the contents of quoted runtime byte strings and renders hexadecimal byte escapes. -/
open Lean

namespace LLVMDecimalAudit

partial def check (root : Name) : CoreM Unit := do
  let env ← getEnv
  let rec visit (name : Name) : StateRefT NameSet CoreM Unit := do
    if (← get).contains name then return
    modify (·.insert name)
    if name == `Concrete.Backend.LLVM.decimal then return
    let some info := env.find? name | throwError "missing definition {name}"
    if let .thmInfo _ := info then return
    let some body := info.value? (allowOpaque := true) | return
    body.forEach fun e => do
      if let .lit (.strVal s) := e then
        if s.toList.any Char.isDigit then
          throwError "LLVM decimal bypass in {name}: digit in literal {repr s}"
      if e.getAppFn.isConstOf `ToString.toString && e.getAppNumArgs > 0 then
        unless e.getAppArgs[0]!.isConstOf `String do
          throwError "LLVM decimal bypass in {name}: non-String interpolation"
      if let .const dep _ := e then
        if dep == `Concrete.Backend.Runtime.llvmEscape then
          unless name == `Concrete.Backend.Runtime.llvmByteString do
            throwError "LLVM decimal bypass in {name}: unquoted byte escape"
        else
          let projectDependency : Bool :=
            match env.getModuleIdxFor? dep with
            | some i => (env.header.moduleNames[i.toNat]?.map (·.getRoot)) == some `Concrete
            | none => true
          if projectDependency then
            visit dep
          else
            let some target := env.find? dep | throwError "missing dependency {dep}"
            if target.type.getForallBody.getAppFn.isConstOf `String then
              unless [`ToString.toString, `String.append, `String.join, `String.intercalate].contains dep do
                throwError "LLVM decimal bypass in {name}: external String producer {dep}"
  (visit root).run' {}

end LLVMDecimalAudit

example (s : String) :
    Concrete.Backend.Runtime.llvmByteString s =
      "c\"" ++ Concrete.Backend.Runtime.llvmEscape s ++ "\"" := rfl

run_cmd Lean.Elab.Command.liftCoreM <| LLVMDecimalAudit.check `Concrete.Backend.LLVM.module
