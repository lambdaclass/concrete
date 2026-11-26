#!/usr/bin/env bash
# Pin the interface round byte for byte, independently of Git index state.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
cd "$(dirname "${BASH_SOURCE[0]}")/../.."

export LC_ALL=C
manifest=scripts/checks/lib/stage_interfaces.sha256
runtime=Concrete/Backend/Runtime.lean
files=(ConcreteSpec/StageInterface.lean ConcreteSpec/StageGrammar.lean ConcreteSpec/Spec/Slice0Syntax.lean
  Concrete/Backend/Observation.lean "$runtime" Concrete/Interfaces/*.lean)
digest() (
  cd "$1"
  sha256sum "${files[@]}"
)
expected=$(cat "$manifest")
actual=$(digest .)
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
sed 's/\$_exit(w 134)/$_exit(w 135)/' "$runtime" >"$work/$runtime"
if cmp -s "$runtime" "$work/$runtime" || [[ "$(digest "$work")" == "$expected" ]]; then
  echo "FAIL stage interface digest control did not detect a changed QBE trap exit." >&2
  exit 1
fi
echo "PASS frozen stage interfaces and QBE trap exit mutation control"
