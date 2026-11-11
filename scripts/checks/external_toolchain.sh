#!/usr/bin/env bash
# Run the production external driver in a private fixture. Only the expensive executables are
# stubs: pin loading, cloning, publishing, dispatch and cache validation are the production code.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
source "$ROOT/scripts/checks/lib/git_sandbox.sh"
git_sandbox
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
BOUNDED_PARENT="$TMP"
source "$ROOT/scripts/checks/lib/bounded.sh"
mkdir -p "$TMP/source" "$TMP/bin" "$TMP/project/scripts/checks/lib" "$TMP/project/scripts/proofs"
cp "$ROOT/scripts/proofs/"{external.sh,external.pins,lake_tool.sh,lib.sh,theorems.lean} "$TMP/project/scripts/proofs/"
cp "$ROOT/scripts/checks/lib/"{gate_env.sh,git_sandbox.sh,has_match.sh,limits.sh} "$TMP/project/scripts/checks/lib/"
printf '%s\n' leanprover/lean4:v4.35.0 > "$TMP/project/lean-toolchain"
# lib.sh's project build and orphan-pruning prerequisites remain real code.
touch "$TMP/project/ConcreteSpec.lean" "$TMP/project/Concrete.lean"
mkdir "$TMP/project/ConcreteSpec" "$TMP/project/Concrete"
git -C "$TMP/source" init -q
cp "$TMP/project/lean-toolchain" "$TMP/source/lean-toolchain"
mkdir "$TMP/source/src"
printf 'const STACK_SIZE: usize = 16_777_216;\n' > "$TMP/source/src/lib.rs"
git -C "$TMP/source" add lean-toolchain src/lib.rs
git -C "$TMP/source" -c user.name=Control -c user.email=control@example.invalid commit -qm fixture
REV=$(git -C "$TMP/source" rev-parse HEAD)
# Retain the production pin names and syntax, replacing only fixture addresses/revisions.
python3 - "$TMP/project/scripts/proofs/external.pins" "$TMP/source" "$REV" <<'PY'
from pathlib import Path
import sys
p = Path(sys.argv[1])
s = p.read_text()
import re
s = re.sub(r'(?m)^([A-Z0-9]+)_URL=.*$', lambda m: m[1]+'_URL='+sys.argv[2], s)
s = re.sub(r'(?m)^([A-Z0-9]+)_REV=.*$', lambda m: m[1]+'_REV='+sys.argv[3], s)
p.write_text(s)
PY
cp "$TMP/project/scripts/proofs/external.pins" "$TMP/pins"
cat > "$TMP/bin/elan" <<'CONTROL'
#!/usr/bin/env bash
set -eu
[ "$ELAN_HOME" = "$CONCRETE_EXT_TOOLS/elan" ]
[ "$ELAN_TOOLCHAIN" = deliberately-conflicting-ambient-toolchain ]
[ "$#" = 6 ]
[ "$1 $2 $3 $4 $5" = 'run --install leanprover/lean4:v4.35.0 lake build' ]
case "$6" in lean4export|con-leche) ;; *) exit 90 ;; esac
[ "$(cat lean-toolchain)" = leanprover/lean4:v4.35.0 ]
echo "$6" >> "$DISPATCH_LOG"
CONTROL
cat > "$TMP/bin/lake" <<'CONTROL'
#!/usr/bin/env bash
# Only lib.sh's project prerequisite may use ambient Lake. A tool build must use elan.
[ "$*" = 'build ConcreteSpec Concrete' ] && exit 0
echo 'FAIL: ambient lake used for an external tool' >&2
exit 91
CONTROL
cat > "$TMP/bin/cargo" <<'CONTROL'
#!/usr/bin/env bash
# Stop at the Rust boundary, after the actual driver has verified both Lake tools.
echo 'CONTROL: reached Rust build boundary' >&2
exit 92
CONTROL
chmod +x "$TMP/bin/"*
export PATH="$TMP/bin:$PATH" DISPATCH_LOG="$TMP/dispatched"
export CONCRETE_EXT_TOOLS="$TMP/tools" ELAN_HOME="$TMP/shared-elan"
export ELAN_TOOLCHAIN=deliberately-conflicting-ambient-toolchain
mkdir -p "$ELAN_HOME"
echo untouched > "$ELAN_HOME/sentinel"
run_driver() {
  local diagnostic=$1 rc=0
  bounded_run 30 -- bash "$TMP/project/scripts/proofs/external.sh" > "$TMP/out" 2>&1 || rc=$?
  if bounded_is_incomplete "$rc" || [ "$rc" != 1 ] || ! grep -qF "$diagnostic" "$TMP/out"; then
    cat "$TMP/out"; echo "FAIL: expected refusal at $diagnostic, rc=$rc"; exit 1
  fi
}
run_driver 'could not build nanoda'
grep -qF 'CONTROL: reached Rust build boundary' "$TMP/out"
printf 'lean4export\ncon-leche\n' > "$TMP/expected"
cmp "$TMP/expected" "$DISPATCH_LOG"
# A second invocation must validate the same cache entries without rebuilding.
run_driver 'could not build nanoda'
cmp "$TMP/expected" "$DISPATCH_LOG"
# Corrupt each existing cache entry without changing its key: exercise the cache-hit check.
for tool in lean4export con-leche; do
  cached="$CONCRETE_EXT_TOOLS/$tool-$REV-leanprover_lean4_v4.35.0"
  printf '%s\n' leanprover/lean4:v4.34.1 > "$cached/lean-toolchain"
  run_driver 'toolchain mismatch'
  cmp "$TMP/expected" "$DISPATCH_LOG"
  cp "$TMP/project/lean-toolchain" "$cached/lean-toolchain"
done
# A wrong con-leche pin must be checked against the source revision before dispatch.
sed 's/^CONLECHE_TOOLCHAIN=.*/CONLECHE_TOOLCHAIN=leanprover\/lean4:v4.34.1/' "$TMP/pins" > "$TMP/project/scripts/proofs/external.pins"
run_driver 'toolchain mismatch'
cmp "$TMP/expected" "$DISPATCH_LOG"
# Exporter and project must agree even before source checkout/build.
sed 's/^LEAN4EXPORT_TOOLCHAIN=.*/LEAN4EXPORT_TOOLCHAIN=leanprover\/lean4:v4.34.1/' "$TMP/pins" > "$TMP/project/scripts/proofs/external.pins"
run_driver 'differs from project'
cmp "$TMP/expected" "$DISPATCH_LOG"
[ "$(cat "$ELAN_HOME/sentinel")" = untouched ]
[ "$(find "$ELAN_HOME" -type f | wc -l)" -eq 1 ]
bounded_gate_status
echo 'external toolchain: production dispatch, isolated elan, cache hits and pin mismatch controls pass'
