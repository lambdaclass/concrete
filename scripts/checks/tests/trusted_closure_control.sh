#!/usr/bin/env bash
# A path longer than the former fixed queue budget must reach its loop helper.
set -euo pipefail
source scripts/checks/lib/bounded.sh
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
bounded_run 20 -- python3 - "$work/Closure.lean" <<'PYGEN'
import sys
with open(sys.argv[1], 'w') as f:
    f.write("import Lean\nimport Concrete.Premises\nprivate def hiddenLoop : IO Unit := do\n  while true do\n    pure ()\ndef hop0 : IO Unit := hiddenLoop\n")
    for i in range(1, 1101):
        f.write(f"def hop{i} : IO Unit := hop{i-1}\n")
    f.write('''open Lean Elab Command in
#eval show CommandElabM Unit from do
  let env ← getEnv
  for n in [`hop0, `hop1100] do
    let some ci := env.find? n | throwError "missing control declaration"
    let kinds ← liftCoreM <| Concrete.Premises.trustedDeclarationKinds env n ci
    unless kinds == ["partial-loop"] do
      throwError "closure control classification mismatch: {n}: {kinds}"
  let n := `hop1100
  let kinds ← liftCoreM <| Concrete.Premises.trustedDeclarationKinds env n (env.find? n).get!
  unless kinds.isEmpty || !(Concrete.Premises.declarationOwners n Concrete.Premises.components).isEmpty do
    IO.println s!"FAIL unlisted trusted declaration {n} [{String.intercalate ", " kinds}]"
    throwError "trusted closure control rejected"
''')
PYGEN
rc=0
bounded_run 120 -- lake env lean "$work/Closure.lean" >"$work/out" 2>&1 || rc=$?
if bounded_is_incomplete "$rc"; then cat "$work/out"; exit 1; fi
if [ "$rc" -eq 0 ]; then cat "$work/out"; echo 'FAIL accepted long closure'; exit 1; fi
grep -qxF 'FAIL unlisted trusted declaration hop1100 [partial-loop]' "$work/out" || { cat "$work/out"; exit 1; }
echo 'ok 1100-hop closure rejected for exact planted reason'
