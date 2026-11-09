import scripts.checks.compiler.slice0_examples

-- Test-only entry: production never imports or selects this file.
#eval show IO Unit from do
  let mut cs := Concrete.Premises.components
  for entry in (← IO.getEnv "CONCRETE_TRUSTED_DECLARATIONS").getD "" |>.splitOn ";" do
    if let [id, decls] := entry.splitOn ":" then
      cs := cs.map fun c => if c.id == id then
        { c with declarations := c.declarations ++ decls.splitOn "," } else c
  let file := (← IO.getEnv "TRUSTED_TEST_FILE").getD ""
  let rc ← auditMain cs [file]
  unless rc == 0 do throw <| IO.userError "test example audit failed"
