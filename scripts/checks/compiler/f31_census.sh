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
module
import Lean
import all Lean.Elab.Idbg

open Lean

-- Use the private kernel data, as Lean does for a non-module import.
-- Read it directly so duplicate realizations are still attributed to their
-- exporting modules. Older, non-module files store all declarations in one part.
def readDeclarations (moduleName : Name) : IO ModuleData := do
  let path ← findOLean moduleName
  let (data, _) ← readModuleData path
  if !data.isModule then
    return data
  let parts ← readModuleDataParts #[path, OLeanLevel.server.adjustFileName path,
    OLeanLevel.private.adjustFileName path]
  let some (privateData, _) := parts[2]?
    | throw (IO.userError s!"missing private declarations for {moduleName}")
  return privateData

def bodyDigest (decl : ConstantInfo) : IO String := do
  let some body := decl.value? (allowOpaque := true)
    | throw (IO.userError s!"divergent declaration {decl.name} has no body")
  -- Serialize the expression tree, including private names and universe levels.
  -- Lean's serializer omits source metadata.
  IO.FS.withTempFile fun handle path => do
    handle.putStr (Idbg.exprToJson body).compress
    handle.flush
    let output ← IO.Process.output { cmd := "sha256sum", args := #[path.toString] }
    if output.exitCode != 0 then
      throw (IO.userError s!"cannot hash {decl.name}: {output.stderr}")
    return (output.stdout.splitOn " ").head!

def isDigest (value : String) : Bool :=
  value.length == 64 && value.toList.all (fun c => c.isDigit || ('a' ≤ c && c ≤ 'f'))

def sameTheoremStatement : ConstantInfo → ConstantInfo → Bool
  | .thmInfo left, .thmInfo right => left.levelParams == right.levelParams && left.type == right.type
  | _, _ => false

public def main (args : List String) : IO UInt32 := do
  let exceptions :: root :: others := args
    | throw (IO.userError "expected an exception list and at least one root")
  let mut pending : Std.HashSet String := {}
  -- Each row names the declaration, the sorted module pair, and their respective
  -- body digests, followed by the tracking issue and an explanation.
  for line in (← IO.FS.lines exceptions) do
    let [decl, left, right, leftDigest, rightDigest, "ISS-0085", cause] := line.splitOn "\t"
      | throw (IO.userError s!"malformed ISS-0085 exception in {exceptions}: {line}")
    let key := String.intercalate "\t" [decl, left, right, leftDigest, rightDigest]
    if decl.isEmpty || left.isEmpty || left >= right || !isDigest leftDigest ||
        !isDigest rightDigest || cause.trimAscii.isEmpty || pending.contains key then
      throw (IO.userError s!"invalid or duplicate ISS-0085 exception in {exceptions}: {line}")
    pending := pending.insert key

  initSearchPath (← findSysroot)
  let mut seen : NameSet := {}
  let mut todo := (root :: others).map String.toName
  let mut declarations : Std.HashMap Name (List (String × ConstantInfo)) := {}
  let mut status : UInt32 := 0
  while let moduleName :: rest := todo do
    todo := rest
    if seen.contains moduleName then
      continue
    seen := seen.insert moduleName
    let data ← readDeclarations moduleName
    todo := data.imports.toList.map (·.module) ++ todo
    let moduleLabel := moduleName.toString
    for decl in data.constants do
      let previous := declarations[decl.name]?.getD []
      for (otherLabel, other) in previous do
        if decl == other then
          continue
        let ((left, leftDecl), (right, rightDecl)) :=
          if moduleLabel < otherLabel then ((moduleLabel, decl), (otherLabel, other))
          else ((otherLabel, other), (moduleLabel, decl))
        let key := String.intercalate "\t"
          [decl.name.toString, left, right, ← bodyDigest leftDecl, ← bodyDigest rightDecl]
        if sameTheoremStatement leftDecl rightDecl && pending.contains key then
          pending := pending.erase key
        else
          IO.eprintln s!"shared declaration differs at {key}"
          status := 1
      declarations := declarations.insert decl.name ((moduleLabel, decl) :: previous)
  for key in pending do
    IO.eprintln s!"stale ISS-0085 exception at {key}"
    status := 1
  return status
LEAN
probe=(lean -DwarningAsError=true -DautoImplicit=false --run "$work/import_fidelity.lean")

# Keep the fixture closure small so it has no unrelated baseline divergences.
# Importing this module alone does not realize countOperand.eq_2.
cat >"$work/CensusNeither.lean" <<'LEAN'
prelude
import Init.Tactics
import Init.SimpLemmas

inductive Op where
  | plain : Bool → Op
  | wrapping : Op

def countOperand : Op → Bool
  | .plain true => true
  | _ => false
LEAN
for module in CensusReference CensusConsumer CensusThird; do
  cat >"$work/$module.lean" <<LEAN
prelude
import CensusNeither

theorem ${module}_countOperand : countOperand .wrapping = false := by
  simp only [countOperand]
LEAN
done

for module in CensusNeither CensusReference CensusConsumer CensusThird; do
  bounded_run -i /dev/null 120 -- lake env bash -c '
    export LEAN_PATH="$1:$LEAN_PATH"
    exec lean --root="$1" -DwarningAsError=true -DautoImplicit=false -o "$1/$2.olean" "$1/$2.lean"
  ' _ "$work" "$module"
done

# Lake supplies the dependency search path. Add the disposable modules to it
# inside the Lake environment, leaving the repository build untouched.
control_probe=(lake env bash -c 'export LEAN_PATH="$1:$LEAN_PATH"; shift; exec "$@"' _ "$work" "${probe[@]}")
expect_rejection() {
  local control=$1
  local expected=$2
  shift 2
  local rc=0
  bounded_run -i /dev/null 120 -- "${control_probe[@]}" "$@" >"$work/control.log" 2>&1 || rc=$?
  printf '%s\n' "$expected" | sort >"$work/expected.log"
  sort -o "$work/control.log" "$work/control.log"
  if [ "$rc" != 1 ] || ! cmp -s "$work/expected.log" "$work/control.log"; then
    cat "$work/control.log" >&2
    echo "ISS-0085: expected '$expected' (status $rc)" >&2
    exit 1
  fi
  echo "ISS-0085 control: $control"
}

: >"$work/empty.tsv"
equation=countOperand.eq_2
consumer_digest=2a9aac725ce467f0dd2c721c4923f78acfb51b2221ba2913f279f7f36131e5df
reference_digest=81be9b010655cb7d05c6230fd127addffeaad7ae4f7087f2fa0f0366b8e7b918
third_digest=bad5bcfdfcdbb00532fac6438e536ca37fbbb819df29da92238821f66b2007d0
pair="$equation"$'\t'"CensusConsumer"$'\t'"CensusReference"$'\t'"$consumer_digest"$'\t'"$reference_digest"
expect_rejection unlisted "shared declaration differs at $pair" \
  "$work/empty.tsv" CensusReference CensusConsumer
expect_rejection absent-from-reference "shared declaration differs at $pair" \
  "$work/empty.tsv" CensusNeither CensusReference CensusConsumer

printf '%s\tISS-0085\tDisposable modules realize the equation with distinct private support.\n' \
  "$pair" >"$work/control.tsv"
bounded_run -i /dev/null 120 -- "${control_probe[@]}" "$work/control.tsv" CensusReference CensusConsumer

consumer_third="$equation"$'\t'"CensusConsumer"$'\t'"CensusThird"$'\t'"$consumer_digest"$'\t'"$third_digest"
reference_third="$equation"$'\t'"CensusReference"$'\t'"CensusThird"$'\t'"$reference_digest"$'\t'"$third_digest"
expect_rejection third-realization "shared declaration differs at $consumer_third
shared declaration differs at $reference_third" \
  "$work/control.tsv" CensusReference CensusConsumer CensusThird

# With only one realization the original exception is stale.
expect_rejection stale "stale ISS-0085 exception at $pair" \
  "$work/control.tsv" CensusReference CensusReference

# A matching module pair with a different digest is both unlisted and stale.
sed "s/$reference_digest/$third_digest/" "$work/control.tsv" >"$work/wrong-digest.tsv"
wrong_pair=${pair/"$reference_digest"/"$third_digest"}
expect_rejection changed-digest "shared declaration differs at $pair
stale ISS-0085 exception at $wrong_pair" \
  "$work/wrong-digest.tsv" CensusReference CensusConsumer

bounded_run -i /dev/null 120 -- lake env "${probe[@]}" \
  scripts/checks/compiler/import_fidelity_exceptions.tsv ConcreteSpec ConcreteSpecEvidence Concrete
