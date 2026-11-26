#!/usr/bin/env bash
# The parser and publication controls use disposable inputs. Run the full migration
# census separately via scripts/spec/f31_census.py --output NEW_DIRECTORY.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../lib/gate_env.sh"
cd "$(dirname "${BASH_SOURCE[0]}")/../../.."
source scripts/checks/lib/git_sandbox.sh
git_sandbox
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
BOUNDED_PARENT="$work"
source scripts/checks/lib/bounded.sh
rc=0
bounded_run -i /dev/null 120 -- python3 -B scripts/spec/f31_census_controls.py || rc=$?
# Compare imported scalar declarations and check the new proof roots in a
# standalone probe. At cutover this probe supersedes the migration census.
if [ "$rc" = 0 ]; then
  cat >"$work/integrated_core_proofs.lean" <<'LEAN'
import Lean
import Concrete.IntegratedCore.Controls

open Lean Concrete.IntegratedCore ConcreteSpec.StageInterface

-- Check the shared equation and its transitive support across the two import
-- closures. Missing declarations are failures even if the statements match.
private def checkEquationSupport (spec combined : Environment) (root : Name) : IO Unit := do
  let mut seen : NameSet := {}
  let mut pending := [root]
  while let name :: rest := pending do
    pending := rest
    if seen.contains name then continue
    let some left := spec.find? name
      | throw (IO.userError s!"missing spec declaration {name}")
    let some right := combined.find? name
      | throw (IO.userError s!"missing combined declaration {name}")
    unless left == right do
      throw (IO.userError s!"shared equation support differs at {name}")
    seen := seen.insert name
    pending := left.getUsedConstantsAsSet.toList ++ pending

-- ISS-0085 compares already-imported proof bodies, so the control does not
-- trigger local lazy equation realization itself.
#eval do
  let spec ← importModules #[{module := `ConcreteSpec}, {module := `ConcreteSpec.IntegratedCore}] {}
  let combined ← importModules #[{module := `ConcreteSpec}, {module := `ConcreteSpec.IntegratedCore},
    {module := `Concrete}, {module := `Concrete.IntegratedCore}] {}
  checkEquationSupport spec combined `ConcreteSpec.Core.IntTy.signed.eq_5

run_cmd do
  for theoremName in [``validated, ``earlyAddLiteralCondition,
      ``earlyIntegerLiteralRoundTrip, ``earlyLiteralMatchesStorage, ``earlyAddOverflow,
      ``lcb_return_word, ``lcb_trap_word] do
    for axiomName in ← collectAxioms theoremName do
      unless [``propext, ``Classical.choice, ``Quot.sound].contains axiomName do
        throwError "unexpected axiom {axiomName} in {theoremName}"
LEAN
  bounded_run -i /dev/null 120 -- lake lean "$work/integrated_core_proofs.lean" \
    -- -DwarningAsError=true -DautoImplicit=false || rc=$?
fi
bounded_summary
exit "$rc"
