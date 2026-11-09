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
macro "sproutCPass" : command => `(@[cpass] meta example : Lean.Compiler.LCNF.PassInstaller := { phase := .base, install := pure })
macro "bloom" : command => `(unsafe def $(Lean.mkIdent `hiddenEscape) : Nat := 0)
macro "bloomPartial" : command => `(partial def $(Lean.mkIdent `hiddenPartial) (n : Nat) : Nat := $(Lean.mkIdent `hiddenPartial) n)
macro "bloomHygienicPartial" : command => `(partial def hygPartial (n : Nat) : Nat := hygPartial n)
macro "bloomCPass" : command => `(@[cpass] public meta def $(Lean.mkIdent `hiddenCPass) : Lean.Compiler.LCNF.PassInstaller := { phase := .base, install := pure })
macro "plantNeutral" : command => `(
  def $(Lean.mkIdent `lateExternTarget) : Nat := 0
  def $(Lean.mkIdent `lateImplReplacement) : Nat := 0
  def $(Lean.mkIdent `lateImplTarget) : Nat := 1
  def $(Lean.mkIdent `lateCsimpOrig) (n : Nat) : Nat := n
  def $(Lean.mkIdent `lateCsimpRepl) (n : Nat) : Nat := n
  theorem $(Lean.mkIdent `lateCsimpThm) :
      $(Lean.mkIdent `lateCsimpOrig) = $(Lean.mkIdent `lateCsimpRepl) := rfl
  public meta def $(Lean.mkIdent `lateCPassTarget) : Lean.Compiler.LCNF.PassInstaller :=
      { phase := .base, install := pure }
)
macro "tagLateExtern" : command => `(attribute [extern "c_late_macro"] $(Lean.mkIdent `lateExternTarget))
macro "tagLateImpl" : command => `(attribute [implemented_by $(Lean.mkIdent `lateImplReplacement)] $(Lean.mkIdent `lateImplTarget))
macro "tagLateCsimp" : command => `(attribute [csimp] $(Lean.mkIdent `lateCsimpThm))
macro "tagLateCPass" : command => `(attribute [cpass] $(Lean.mkIdent `lateCPassTarget))
macro "tagInLocalCsimp" : command => `(
  attribute [local csimp] $(Lean.mkIdent `lateCsimpThm) in
  def $(Lean.mkIdent `inLocalCsimpCaller) (n : Nat) : Nat := $(Lean.mkIdent `lateCsimpOrig) n
)
macro "tagScopedLocalCsimp" : command => `(
  section
  attribute [local csimp] $(Lean.mkIdent `lateCsimpThm)
  def $(Lean.mkIdent `scopedLocalCsimpCaller) (n : Nat) : Nat := $(Lean.mkIdent `lateCsimpOrig) n
  end
)
EOF
bounded_run 60 -- lake env lean --root="$work" -o "$work/Seed.olean" "$work/Seed.lean"
BASE_LEAN_PATH=$(lake env printenv LEAN_PATH)
LEAN_BIN=$(lake env which lean)
for invocation in sprout sproutUnsafe sproutPartial sproutExtern sproutCPass bloom bloomPartial bloomHygienicPartial bloomCPass tagLateExtern tagLateImpl tagLateCsimp tagLateCPass tagInLocalCsimp tagScopedLocalCsimp; do
  case "$invocation" in
    tag*)
      printf 'import Seed\nplantNeutral\n%s\n' "$invocation" >"$work/Invoke.lean" ;;
    *)
      printf 'import Seed\n%s\n' "$invocation" >"$work/Invoke.lean" ;;
  esac
  printf 'import Lean\n' >"$work/Other.lean"
  # Neither importing nor invoking file contains any former candidate token.
  if grep -Ew 'example|partial|while|repeat|Loop|unsafe|extern|implemented_by|csimp|cpass|opaque|attribute' "$work/Invoke.lean" "$work/Other.lean"; then
    echo 'FAIL control: fixture contains candidate tokens'; exit 1
  fi
  rc=0
  bounded_run 90 -- env LEAN_PATH="$work:$BASE_LEAN_PATH" "$LEAN_BIN" --run \
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
    sproutCPass)
      grep -Eq "^FAIL unlisted example trusted declaration «?⟪example⟫.*Invoke\.lean:2:0»? \[cpass\]$" "$work/out" || { cat "$work/out"; exit 1; } ;;
    bloom)
      grep -qxF 'FAIL unlisted example trusted declaration hiddenEscape [unsafe]' "$work/out" || { cat "$work/out"; exit 1; } ;;
    bloomPartial)
      grep -qxF 'FAIL unlisted example trusted declaration hiddenPartial [partial]' "$work/out" || { cat "$work/out"; exit 1; } ;;
    bloomHygienicPartial)
      grep -Eq '^FAIL unlisted example trusted declaration hygPartial\._@\.[^ ]+ \[partial\]$' "$work/out" || { cat "$work/out"; exit 1; } ;;
    bloomCPass)
      grep -qxF 'FAIL unlisted example trusted declaration hiddenCPass [cpass]' "$work/out" || { cat "$work/out"; exit 1; } ;;
    tagLateExtern)
      grep -qxF 'FAIL unlisted example trusted declaration lateExternTarget [extern]' "$work/out" || { cat "$work/out"; exit 1; } ;;
    tagLateImpl)
      grep -qxF 'FAIL unlisted example trusted declaration lateImplTarget [implemented_by]' "$work/out" || { cat "$work/out"; exit 1; } ;;
    tagLateCsimp)
      grep -qxF 'FAIL unlisted example trusted declaration lateCsimpThm [csimp]' "$work/out" || { cat "$work/out"; exit 1; } ;;
    tagLateCPass)
      grep -qxF 'FAIL unlisted example trusted declaration lateCPassTarget [cpass]' "$work/out" || { cat "$work/out"; exit 1; } ;;
    tagInLocalCsimp)
      grep -qxF 'FAIL unlisted example trusted declaration lateCsimpThm [csimp]' "$work/out" || { cat "$work/out"; exit 1; } ;;
    tagScopedLocalCsimp)
      grep -qxF 'FAIL unlisted example trusted declaration lateCsimpThm [csimp]' "$work/out" || { cat "$work/out"; exit 1; } ;;
  esac
  echo "ok imported $invocation rejected for exact planted reason"
done

cat >"$work/QuotedTemplate.lean" <<'EOF'
import Lean
macro "uninvokedPartial" : command => `(partial def quotedP (n : Nat) : Nat := quotedP n)
macro "uninvokedUnsafe" : command => `(unsafe def quotedU : Nat := 0)
example : 1 = 1 := rfl
EOF
rc=0
bounded_run 90 -- lake env lean --run \
  scripts/checks/compiler/slice0_examples.lean "$work/QuotedTemplate.lean" >"$work/quoted.out" 2>&1 || rc=$?
if bounded_is_incomplete "$rc" || [ "$rc" -ne 0 ]; then
  cat "$work/quoted.out"
  echo 'FAIL control: uninvoked quoted command template was falsely rejected'
  exit 1
fi
grep -Eq 'example-json .*"name":".*⟪example⟫' "$work/quoted.out" || { cat "$work/quoted.out"; exit 1; }
echo "ok uninvoked quoted command templates are not flagged"
