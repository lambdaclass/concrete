#!/usr/bin/env bash
# The parser and publication controls use disposable inputs. Run the full migration
# census separately via scripts/spec/f31_census.py --output NEW_DIRECTORY.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../lib/gate_env.sh"
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."
source scripts/checks/lib/git_sandbox.sh
git_sandbox
work=$(mktemp -d)
BOUNDED_PARENT="$work"
source scripts/checks/lib/bounded.sh
trap 'bounded_summary; rm -rf "$work"' EXIT
bounded_run -i /dev/null 120 -- python3 -B scripts/spec/f31_census_controls.py

# Dynamic imports are invisible to Lake's import scan. Build the exact roots
# the probe loads, including the integrated Core controls imported by Concrete.
bounded_run -i /dev/null 1800 -- lake build ConcreteSpec ConcreteSpecEvidence Concrete

cat >"$work/import_fidelity.lean" <<'LEAN'
import Lean

open Lean

-- ISS-0085: compare imported declarations without triggering lazy realization
-- in the probe. Scanning the entire spec also checks its private support.
def main (args : List String) : IO UInt32 := do
  let exceptions :: reference :: consumer :: others := args
    | throw (IO.userError "expected an exception list, a reference module and at least one consumer")
  let mut pending : NameSet := {}
  for line in (← IO.FS.lines exceptions) do
    let [decl, "ISS-0085", cause] := line.splitOn "\t"
      | throw (IO.userError s!"malformed ISS-0085 exception in {exceptions}: {line}")
    let name := decl.toName
    if name.isAnonymous || cause.trimAscii.isEmpty || pending.contains name then
      throw (IO.userError s!"invalid or duplicate ISS-0085 exception in {exceptions}: {line}")
    pending := pending.insert name
  initSearchPath (← findSysroot)
  let spec ← importModules #[{module := reference.toName}] {}
  let roots := reference :: consumer :: others
  let combined ← importModules (roots.toArray.map fun root => {module := root.toName}) {}
  let mut status := 0
  for (name, original) in spec.constants do
    match combined.find? name with
    | none =>
      IO.eprintln s!"missing combined declaration {name}"
      status := 1
    | some imported =>
      unless original == imported do
        if pending.contains name then
          pending := pending.erase name
        else
          IO.eprintln s!"shared declaration differs at {name}"
          status := 1
  for name in pending do
    IO.eprintln s!"stale ISS-0085 exception at {name}"
    status := 1
  return status
LEAN
probe=(lean -DwarningAsError=true -DautoImplicit=false --run "$work/import_fidelity.lean")

# Both disposable modules independently realize the same spec equation using
# module-local private support. Use a different equation from the original
# ISS-0085 reproducer so a check restricted to that one equation cannot pass.
for module in CensusReference CensusConsumer; do
  cat >"$work/$module.lean" <<LEAN
import ConcreteSpec.Spec.Scalar
open ConcreteSpec.Core
theorem ${module}_countOperand : (MachineOp.wrapping .add).countOperand = false := by
  simp only [MachineOp.countOperand]
LEAN
  bounded_run -i /dev/null 120 -- lake env lean --root="$work" \
    -DwarningAsError=true -DautoImplicit=false -o "$work/$module.olean" "$work/$module.lean"
done

# Lake supplies the dependency search path. Add the disposable modules to it
# inside the Lake environment, leaving the repository build untouched.
control_probe=(lake env bash -c 'export LEAN_PATH="$1:$LEAN_PATH"; shift; exec "$@"' _ "$work" "${probe[@]}")
expect_rejection() {
  local expected=$1
  shift
  local rc=0
  bounded_run -i /dev/null 120 -- "${control_probe[@]}" "$@" >"$work/control.log" 2>&1 || rc=$?
  printf '%s\n' "$expected" >"$work/expected.log"
  if [ "$rc" != 1 ] || ! cmp -s "$work/expected.log" "$work/control.log"; then
    cat "$work/control.log" >&2
    echo "ISS-0085: expected '$expected' (status $rc)" >&2
    exit 1
  fi
  echo "ISS-0085 control: $expected"
}

: >"$work/empty.tsv"
equation=ConcreteSpec.Core.MachineOp.countOperand.eq_3
expect_rejection "shared declaration differs at $equation" \
  "$work/empty.tsv" CensusReference CensusConsumer
printf '%s\tISS-0085\tDisposable modules realize the equation with distinct private support.\n' \
  "$equation" >"$work/control.tsv"
bounded_run -i /dev/null 120 -- "${control_probe[@]}" "$work/control.tsv" CensusReference CensusConsumer

# The equation still exists but now both roots load the same realization.
expect_rejection "stale ISS-0085 exception at $equation" \
  "$work/control.tsv" CensusReference CensusReference

bounded_run -i /dev/null 120 -- lake env "${probe[@]}" \
  scripts/checks/compiler/import_fidelity_exceptions.tsv ConcreteSpec ConcreteSpecEvidence Concrete
