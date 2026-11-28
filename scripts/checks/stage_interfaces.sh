#!/usr/bin/env bash
# Pin the Slice 0 frozen interface round (exact 22 files) byte for byte, independently of Git index state.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
cd "$(dirname "${BASH_SOURCE[0]}")/../.."

export LC_ALL=C
manifest=scripts/checks/lib/stage_interfaces.sha256
runtime=Concrete/Backend/Runtime.lean
proposal=ConcreteSpec/Spec/EarlyProposal.lean
grammar_data=ConcreteSpec/Spec/GrammarData.lean

# Exact 22 pinned files matching docs/compiler/STAGE_INTERFACES.md (LC_ALL=C sorted).
files=(
  Concrete/Backend/LCBJob.lean
  Concrete/Backend/LLVMIR.lean
  Concrete/Backend/Observation.lean
  "$runtime"
  Concrete/Contracts.lean
  Concrete/Interfaces/Behavior.lean
  Concrete/Interfaces/Carriers.lean
  Concrete/Interfaces/Contracts.lean
  Concrete/Interfaces/Formation.lean
  Concrete/Interfaces/Pipeline.lean
  Concrete/Interfaces/SurfaceElaboration.lean
  "$proposal"
  "$grammar_data"
  ConcreteSpec/Spec/GrammarVocab.lean
  ConcreteSpec/Spec/LL1.lean
  ConcreteSpec/Spec/Slice0Syntax.lean
  ConcreteSpec/Spec/SliceGrammar.lean
  ConcreteSpec/Spec/TableVerifier.lean
  ConcreteSpec/Spec/VerifyMachine.lean
  ConcreteSpec/StageGrammar.lean
  ConcreteSpec/StageGrammarCore.lean
  ConcreteSpec/StageInterface.lean
)

if (( ${#files[@]} != 22 )); then
  echo "FAIL stage interface inventory must pin exactly 22 files, found ${#files[@]}." >&2
  exit 1
fi

# Ensure no unpinned module has been added under Concrete/Interfaces/.
iface_actual=$(printf '%s\n' Concrete/Interfaces/*.lean | sort)
iface_expected=$(printf '%s\n' "${files[@]}" | grep '^Concrete/Interfaces/' | sort)
if [[ "$iface_actual" != "$iface_expected" ]]; then
  echo "FAIL Concrete/Interfaces/*.lean inventory differs from the pinned 22-file manifest." >&2
  diff -u <(printf '%s\n' "$iface_expected") <(printf '%s\n' "$iface_actual") >&2 || true
  exit 1
fi

digest() (
  cd "$1"
  sha256sum "${files[@]}"
)
actual=$(digest .)
if [[ ${1:-} == --update ]]; then
  expected=$actual
else
  expected=$(cat "$manifest")
fi
if [[ "$actual" != "$expected" ]]; then
  echo "FAIL stage interfaces changed. Open an interface round before updating $manifest." >&2
  diff -u "$manifest" <(printf '%s\n' "$actual") >&2 || true
  exit 1
fi

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cp --parents "${files[@]}" "$work"
if [[ "$(digest "$work")" != "$expected" ]]; then
  echo "FAIL stage interface digest control could not reproduce the baseline." >&2
  exit 1
fi

# Mutation control 1: QBE trap exit in Concrete/Backend/Runtime.lean.
sed 's/\$_exit(w 134)/$_exit(w 135)/' "$runtime" >"$work/$runtime"
if cmp -s "$runtime" "$work/$runtime" || [[ "$(digest "$work")" == "$expected" ]]; then
  echo "FAIL stage interface digest control did not detect a changed QBE trap exit." >&2
  exit 1
fi
cp "$runtime" "$work/$runtime"

# Mutation control 2: grammar table certificate entry in ConcreteSpec/Spec/EarlyProposal.lean.
sed 's/\.node 5651 1/.node 5651 2/' "$proposal" >"$work/$proposal"
if cmp -s "$proposal" "$work/$proposal" || [[ "$(digest "$work")" == "$expected" ]]; then
  echo "FAIL stage interface digest control did not detect a mutated EarlyProposal grammar table." >&2
  exit 1
fi
cp "$proposal" "$work/$proposal"

# Mutation control 3: shared grammar catalog entry in ConcreteSpec/Spec/GrammarData.lean.
sed 's/| \.program => "Program"/| .program => "MutatedProgram"/' "$grammar_data" >"$work/$grammar_data"
if cmp -s "$grammar_data" "$work/$grammar_data" || [[ "$(digest "$work")" == "$expected" ]]; then
  echo "FAIL stage interface digest control did not detect a mutated GrammarData production catalog." >&2
  exit 1
fi
cp "$grammar_data" "$work/$grammar_data"

if [[ "$(digest "$work")" != "$expected" ]]; then
  echo "FAIL stage interface digest control could not restore the baseline after mutations." >&2
  exit 1
fi

if [[ ${1:-} == --update ]]; then
  printf '%s\n' "$actual" >"$manifest"
fi
echo "PASS frozen stage interfaces (22 pinned files) and QBE/grammar mutation controls"
