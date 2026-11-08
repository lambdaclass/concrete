#!/usr/bin/env bash
# Focused real-frontend controls; no source-token selection is permitted.
set -euo pipefail
source scripts/checks/lib/bounded.sh
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cat >"$work/Seed.lean" <<'EOF'
import Lean
axiom planted : False
macro "sprout" : command => `(example : False := planted)
macro "sproutUnsafe" : command => `(unsafe example : Nat := 0)
macro "sproutPartial" : command => `(partial example : Nat := let rec loop (n : Nat) : Nat := loop n; loop 0)
macro "sproutExtern" : command => `(@[extern "c_sprout"] example : Nat := 0)
macro "bloom" : command => `(unsafe def $(Lean.mkIdent `hiddenEscape) : Nat := 0)
EOF
bounded_run 60 -- lake env lean --root="$work" -o "$work/Seed.olean" "$work/Seed.lean"
for invocation in sprout sproutUnsafe sproutPartial sproutExtern bloom; do
  printf 'import Seed\n%s\n' "$invocation" >"$work/Invoke.lean"
  printf 'import Lean\n' >"$work/Other.lean"
  # Neither importing nor invoking file contains any former candidate token.
  if grep -Ew 'example|partial|while|repeat|Loop|unsafe|extern|implemented_by|csimp|opaque' "$work/Invoke.lean" "$work/Other.lean"; then
    echo 'FAIL control: fixture contains candidate tokens'; exit 1
  fi
  rc=0
  bounded_run 90 -- env LEAN_PATH="$work:$(lake env printenv LEAN_PATH)" lake env lean --run \
    scripts/checks/compiler/slice0_examples.lean "$work/Other.lean" "$work/Invoke.lean" >"$work/out" 2>&1 || rc=$?
  if bounded_is_incomplete "$rc"; then cat "$work/out"; exit 1; fi
  if [ "$rc" -eq 0 ]; then cat "$work/out"; echo "FAIL control: accepted $invocation"; exit 1; fi
  case "$invocation" in
    sprout)
      grep -qxF "FAIL unlisted example axiom planted in $work/Invoke.lean:2" "$work/out" || { cat "$work/out"; exit 1; } ;;
    sproutUnsafe)
      grep -Eq "^FAIL unlisted example trusted declaration «?⟪example⟫.*Invoke\.lean:2:0»? \[unsafe\]$" "$work/out" || { cat "$work/out"; exit 1; } ;;
    sproutPartial)
      grep -Eq "^FAIL unlisted example trusted declaration «?⟪example⟫.*Invoke\.lean:2:0»? \[partial\]$" "$work/out" || { cat "$work/out"; exit 1; } ;;
    sproutExtern)
      grep -Eq "^FAIL unlisted example trusted declaration «?⟪example⟫.*Invoke\.lean:2:0»? \[extern\]$" "$work/out" || { cat "$work/out"; exit 1; } ;;
    bloom)
      grep -qxF 'FAIL unlisted example trusted declaration hiddenEscape [unsafe]' "$work/out" || { cat "$work/out"; exit 1; } ;;
  esac
  echo "ok imported $invocation rejected for exact planted reason"
done
