#!/usr/bin/env bash
# Run the production external driver in a private fixture. Only the expensive executables are
# stubs: pin loading, cloning, publishing, dispatch and cache validation are the production code.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
source "$ROOT/scripts/checks/lib/git_sandbox.sh"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
git_sandbox "$TMP/gitconfig"
BOUNDED_PARENT="$TMP"
source "$ROOT/scripts/checks/lib/bounded.sh"
mkdir -p "$TMP/source" "$TMP/bin" "$TMP/project/scripts/checks/lib" "$TMP/project/scripts/proofs"
cp "$ROOT/scripts/proofs/"{external.sh,external.pins,lake_tool.sh,lib.sh,theorems.lean,export_inventory.py} "$TMP/project/scripts/proofs/"
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
# A separate source exercises dispatch when the tool differs from the project.
git clone -q "$TMP/source" "$TMP/conleche-source"
printf '%s\n' leanprover/lean4:v4.35.0-fixture > "$TMP/conleche-source/lean-toolchain"
git -C "$TMP/conleche-source" add lean-toolchain
git -C "$TMP/conleche-source" -c user.name=Control -c user.email=control@example.invalid commit -qm conleche-toolchain
CONLECHE_REV=$(git -C "$TMP/conleche-source" rev-parse HEAD)
# Retain the production pin names and syntax, replacing only fixture addresses/revisions.
python3 - "$TMP/project/scripts/proofs/external.pins" "$TMP/source" "$REV" "$TMP/conleche-source" "$CONLECHE_REV" <<'PY'
from pathlib import Path
import sys
p = Path(sys.argv[1])
s = p.read_text()
import re
s = re.sub(r'(?m)^([A-Z0-9]+)_URL=.*$', lambda m: m[1]+'_URL='+sys.argv[2], s)
s = re.sub(r'(?m)^([A-Z0-9]+)_REV=.*$', lambda m: m[1]+'_REV='+sys.argv[3], s)
s = re.sub(r'(?m)^CONLECHE_URL=.*$', 'CONLECHE_URL='+sys.argv[4], s)
s = re.sub(r'(?m)^CONLECHE_REV=.*$', 'CONLECHE_REV='+sys.argv[5], s)
s = re.sub(r'(?m)^CONLECHE_TOOLCHAIN=.*$', 'CONLECHE_TOOLCHAIN=leanprover/lean4:v4.35.0-fixture', s)
p.write_text(s)
PY
cp "$TMP/project/scripts/proofs/external.pins" "$TMP/pins"
# A small synthetic stream lets the production inventory and all later driver phases run.
# Only Lean and the independent checkers are stubbed; this is an environment/locking test.
python3 - "$TMP/stream.ndjson" <<'CONTROL'
import json, sys
rows=[{"meta": {k: {"version": v} for k,v in [("lean","4.35.0"),("exporter","fixture"),("format","3.1.0")] }},
      {"in":1,"str":{"pre":0,"str":"ConcreteSpec"}},
      {"ie":1,"sort":0}, {"ie":2,"natVal":0}, {"ie":3,"natVal":1},
      {"in":2,"str":{"pre":1,"str":"definition"}},
      {"def":{"name":2,"levelParams":[],"type":1,"value":2,"hints":"abbrev","safety":"safe","all":[]}}]
for i in range(100):
    rows += [{"in":i+3,"str":{"pre":1,"str":f"theorem{i:03}"}},
             {"thm":{"name":i+3,"levelParams":[],"type":1,"value":2+i%2,"all":[]}}]
with open(sys.argv[1], 'w') as f:
    for r in rows: f.write(json.dumps(r)+'\n')
CONTROL
cat > "$TMP/artifact" <<'CONTROL'
#!/usr/bin/env bash
set -eu
[ "$ELAN_HOME" = "$CALLER_ELAN_HOME" ] || { echo 'CONTROL: artifact home changed' >&2; exit 98; }
flock -n "$CONCRETE_EXT_TOOLS/.lock" true || { echo 'CONTROL: artifact lock held' >&2; exit 99; }
case "$0" in
  */lean4export) echo exporter >> "$PHASE_LOG"; cat "$FIXTURE_STREAM"; exit 0 ;;
  */con-leche) file=$3; kind=conleche ;;
  */nanoda_bin) file=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["export_file_path"])' "$1"); kind=nanoda ;;
esac
echo "$kind-$(basename "$file")" >> "$PHASE_LOG"
case "$file" in
  */swapped.ndjson)
    if [ "$kind" = conleche ]; then echo 'type mismatch in theorem ConcreteSpec.theorem001 '; else echo 'assertion failed: self.def_eq'; fi
    exit 1 ;;
  */cut.ndjson|*/dropped.ndjson) echo 'fixture checker refuses corruption'; exit 1 ;;
esac
if [ "$kind" = conleche ]; then echo 'con-leche: accepted 101 declarations (--verified)'; else echo 'Checked 101 declarations with no errors'; fi
CONTROL
chmod +x "$TMP/artifact"
cat > "$TMP/bin/elan" <<'CONTROL'
#!/usr/bin/env bash
set -eu
if [ "$1" = --version ]; then
  [ "$ELAN_HOME" = "$CALLER_ELAN_HOME" ] || { echo 'CONTROL: caller home changed' >&2; exit 94; }
  if flock -n "$CONCRETE_EXT_TOOLS/.lock" true; then
  echo 'CONTROL: build lock missing' >&2; exit 99
fi
  echo 'elan fixture 1'; exit 0
fi
identity=$(printf '%s\n%s\n' "$(realpath "$0")" 'elan fixture 1' | sha256sum | cut -d' ' -f1)
[ "$ELAN_HOME" = "$CONCRETE_EXT_TOOLS/elan-$identity" ] || { echo 'CONTROL: tool home wrong' >&2; exit 93; }
if flock -n "$CONCRETE_EXT_TOOLS/.lock" true; then
  echo 'CONTROL: build lock missing' >&2; exit 99
fi
[ "$CALLER_ELAN_HOME" != "$ELAN_HOME" ]
[ "${ELAN_TOOLCHAIN+x}" != x ]
[ "${LEAN_PATH+x}" != x ]
[ "$#" = 6 ]
[ "$1 $2 $4 $5" = 'run --install lake build' ]
case "$6" in
  lean4export) expected=leanprover/lean4:v4.35.0 ;;
  con-leche) expected=leanprover/lean4:v4.35.0-fixture ;;
  *) exit 90 ;;
esac
[ "$3" = "$expected" ] && [ "$(cat lean-toolchain)" = "$expected" ] || {
  echo 'CONTROL: per-tool toolchain dispatch wrong' >&2; exit 90;
}
mkdir -p .lake/build/bin
cp "$FIXTURE_ARTIFACT" ".lake/build/bin/$6"
chmod +x ".lake/build/bin/$6"
mkdir -p "$ELAN_HOME"
touch "$ELAN_HOME/tool-build"
echo "$6" >> "$DISPATCH_LOG"
CONTROL
cat > "$TMP/bin/lake" <<'CONTROL'
#!/usr/bin/env bash
set -eu
[ "$ELAN_HOME" = "$CALLER_ELAN_HOME" ] || { echo 'CONTROL: caller home changed' >&2; exit 94; }
[ "${ELAN_TOOLCHAIN+x}" != x ]
[ "${LEAN_PATH+x}" != x ]
mkdir -p "$CONCRETE_EXT_TOOLS"
flock -n "$CONCRETE_EXT_TOOLS/.lock" true || { echo 'CONTROL: caller lock held' >&2; exit 95; }
case "$*" in
  'build ConcreteSpec Concrete') echo project >> "$PHASE_LOG" ;;
  'env lean '*)
    echo "population-$PROOF_MODE" >> "$PHASE_LOG"
    case "$PROOF_MODE" in
      theorems) for i in $(seq -w 0 99); do printf 'ConcreteSpec.theorem%03d\t\n' "$((10#$i))"; done ;;
      encoded)
        if [[ "$3" == */control-* ]]; then
          printf '%s\n' 'ConcreteSpecControl.«a.b»	s19:ConcreteSpecControls3:a.b' 'ConcreteSpecControl.a.b	s19:ConcreteSpecControls1:as1:b' 'ConcreteSpecControl.«λ»	s19:ConcreteSpecControls2:λ'
        else
          for i in $(seq 0 99); do printf 'ConcreteSpec.theorem%03d\ts12:ConcreteSpecs10:theorem%03d\n' "$i" "$i"; done
        fi ;;
      decls) python3 scripts/proofs/export_inventory.py decls "$FIXTURE_STREAM" > "$PROOF_OUT" ;;
      heads)
        if [[ "$3" == */control-* ]]; then echo 'control character' >&2; exit 1; fi
        printf 's12:ConcreteSpecs10:theorem000\tA\ns12:ConcreteSpecs10:theorem001\tB\n' ;;
      *) exit 91 ;;
    esac ;;
  env*/lean4export*) shift; exec "$@" ;;
  *) echo 'FAIL: ambient lake used for an external tool' >&2; exit 91 ;;
esac
CONTROL
cat > "$TMP/bin/cargo" <<'CONTROL'
#!/usr/bin/env bash
set -eu
[ "$ELAN_HOME" = "$CALLER_ELAN_HOME" ] || { echo 'CONTROL: cargo home changed' >&2; exit 97; }
if flock -n "$CONCRETE_EXT_TOOLS/.lock" true; then
  echo 'CONTROL: build lock missing' >&2; exit 99
fi
grep -qF "STACK_SIZE: usize = ${FIXTURE_STACK_SIZE:-268_435_456};" src/lib.rs
grep -qF 'STACK_SIZE: usize = 16_777_216;' src/lib.rs && exit 98
mkdir -p target/release
cp "$FIXTURE_ARTIFACT" target/release/nanoda_bin
chmod +x target/release/nanoda_bin
echo nanoda >> "$DISPATCH_LOG"
CONTROL
cat > "$TMP/bin/rustc" <<'CONTROL'
#!/usr/bin/env bash
set -eu
[ "$1" = -vV ]
echo "rustc fixture ${FIXTURE_RUST_VERSION:-1}"
CONTROL
chmod +x "$TMP/bin/"*
cat > "$TMP/check-path" <<'CONTROL'
#!/usr/bin/env bash
set -eu
for tool in lake lean leanchecker; do
  [ "$(command -v "$tool")" = "$CALLER_BIN/$tool" ] || {
    echo "proxy shadows caller: $tool" >&2; exit 1;
  }
done
CONTROL
chmod +x "$TMP/check-path"
ln -s lake "$TMP/bin/lean"
ln -s lake "$TMP/bin/leanchecker"
mkdir "$TMP/proxies"
for tool in lake lean leanchecker; do ln -s "$TMP/bin/elan" "$TMP/proxies/$tool"; done
export CALLER_BIN="$TMP/bin" FIXTURE_STREAM="$TMP/stream.ndjson" FIXTURE_ARTIFACT="$TMP/artifact"
if PATH="$TMP/proxies:$TMP/bin:$PATH" bash "$TMP/check-path" > "$TMP/negative" 2>&1; then
  echo 'FAIL: proxy-leak negative control accepted'; exit 1
fi
grep -qF 'proxy shadows caller: lake' "$TMP/negative"
# Supply the caller PATH before entering Nix; checking inside observes Nix's additions.
rc=0
PATH="$TMP/bin:$PATH" bounded_run 120 -- nix develop "$ROOT#extcheck" -c bash "$TMP/check-path" || rc=$?
if bounded_is_incomplete "$rc"; then bounded_gate_status; exit 1; fi
[ "$rc" = 0 ]
export PATH="$TMP/bin:$PATH" DISPATCH_LOG="$TMP/dispatched" PHASE_LOG="$TMP/phases"
export CONCRETE_EXT_TOOLS="$TMP/tools" ELAN_HOME="$TMP/shared-elan"
export ELAN_TOOLCHAIN=deliberately-conflicting-ambient-toolchain LEAN_PATH=/conflicting/lean/path
export CONCRETE_EXT_ELAN="$TMP/bin/elan" CALLER_ELAN_HOME="$ELAN_HOME"
mkdir -p "$ELAN_HOME"
echo untouched > "$ELAN_HOME/sentinel"
run_driver() {
  local diagnostic=$1 expected_rc=${2:-1} rc=0
  bounded_run 30 -- bash "$TMP/project/scripts/proofs/external.sh" > "$TMP/out" 2>&1 || rc=$?
  if bounded_is_incomplete "$rc"; then bounded_gate_status; exit 1; fi
  if [ "$rc" != "$expected_rc" ] || ! grep -qF "$diagnostic" "$TMP/out"; then
    cat "$TMP/out"; echo "FAIL: expected refusal at $diagnostic, rc=$rc"; exit 1
  fi
}
# Round-3 negative control: a global isolated home must fail the caller proxy.
cp "$TMP/project/scripts/proofs/external.sh" "$TMP/driver"
sed '/^CHECK_NAME=PROOF-EXTERNAL/i export ELAN_HOME="$TOOLS/elan"' "$TMP/driver" > "$TMP/project/scripts/proofs/external.sh"
run_driver 'CONTROL: caller home changed' 2
cp "$TMP/driver" "$TMP/project/scripts/proofs/external.sh"
# Round-2 negative control: unscoped tool elan writes must fail before dispatch.
cp "$TMP/project/scripts/proofs/lake_tool.sh" "$TMP/builder"
sed 's/ELAN_HOME="$tools\/elan-$identity" //' "$TMP/builder" > "$TMP/project/scripts/proofs/lake_tool.sh"
run_driver 'could not build lean4export'
[ ! -e "$DISPATCH_LOG" ]
cp "$TMP/builder" "$TMP/project/scripts/proofs/lake_tool.sh"
# An isolated home exported after locking must be rejected by cargo/population.
sed '/^# A pre-push/i export ELAN_HOME="$TOOLS/elan-bad"' "$TMP/driver" > "$TMP/project/scripts/proofs/external.sh"
run_driver 'CONTROL: caller home changed'
cp "$TMP/driver" "$TMP/project/scripts/proofs/external.sh"
# Moving the lock before project preparation must be rejected by the project Lake.
python3 - "$TMP/driver" "$TMP/project/scripts/proofs/external.sh" <<'CONTROL'
from pathlib import Path
import sys
s=Path(sys.argv[1]).read_text()
a=s.index('mkdir -p "$TOOLS"'); b=s.index('# A pre-push', a)
lock=s[a:b]; s=s[:a]+s[b:]
s=s.replace('source scripts/proofs/lib.sh',lock+'source scripts/proofs/lib.sh')
Path(sys.argv[2]).write_text(s)
CONTROL
run_driver 'CONTROL: caller lock held' 2
cp "$TMP/driver" "$TMP/project/scripts/proofs/external.sh"
rm -rf "$CONCRETE_EXT_TOOLS"
: > "$DISPATCH_LOG"
: > "$PHASE_LOG"
run_driver 'PROOF-EXTERNAL:' 0
# Check obligations by phase, without relying on the placement of source lines.
for phase in project population-theorems population-encoded exporter population-decls population-heads conleche-spec.ndjson nanoda-spec.ndjson; do
  grep -qxF "$phase" "$PHASE_LOG"
done
printf 'lean4export\ncon-leche\nnanoda\n' > "$TMP/expected"
cmp "$TMP/expected" "$DISPATCH_LOG"
run_driver 'PROOF-EXTERNAL:' 0
cmp "$TMP/expected" "$DISPATCH_LOG"
# A different resolved elan path must produce distinct homes and Lake tool trees.
cp "$TMP/bin/elan" "$TMP/bin/elan-other"
CONCRETE_EXT_ELAN="$TMP/bin/elan-other" run_driver 'PROOF-EXTERNAL:' 0
[ "$(find "$CONCRETE_EXT_TOOLS" -maxdepth 1 -type d -name 'elan-*' | wc -l)" -eq 2 ]
for tool in lean4export con-leche; do
  [ "$(find "$CONCRETE_EXT_TOOLS" -maxdepth 1 -type d -name "$tool-*" | wc -l)" -eq 2 ]
done
[ "$(find "$CONCRETE_EXT_TOOLS" -maxdepth 1 -type d -name 'nanoda-*' | wc -l)" -eq 1 ]
# Each Rust builder identity component independently invalidates nanoda.
mkdir "$TMP/rust-other"
cp "$TMP/bin/cargo" "$TMP/rust-other/cargo"
PATH="$TMP/rust-other:$PATH" run_driver 'PROOF-EXTERNAL:' 0
[ "$(find "$CONCRETE_EXT_TOOLS" -maxdepth 1 -type d -name 'nanoda-*' | wc -l)" -eq 2 ]
cp "$TMP/bin/rustc" "$TMP/rust-other/rustc"
PATH="$TMP/rust-other:$PATH" run_driver 'PROOF-EXTERNAL:' 0
[ "$(find "$CONCRETE_EXT_TOOLS" -maxdepth 1 -type d -name 'nanoda-*' | wc -l)" -eq 3 ]
FIXTURE_RUST_VERSION=2 run_driver 'PROOF-EXTERNAL:' 0
[ "$(find "$CONCRETE_EXT_TOOLS" -maxdepth 1 -type d -name 'nanoda-*' | wc -l)" -eq 4 ]
# Changing the nanoda patch changes every recipe-keyed tree, without changing elan homes.
sed 's/268_435_456/536_870_912/g' "$TMP/builder" > "$TMP/project/scripts/proofs/lake_tool.sh"
FIXTURE_STACK_SIZE=536_870_912 run_driver 'PROOF-EXTERNAL:' 0
for tool in lean4export con-leche; do
  [ "$(find "$CONCRETE_EXT_TOOLS" -maxdepth 1 -type d -name "$tool-*" | wc -l)" -eq 3 ]
done
[ "$(find "$CONCRETE_EXT_TOOLS" -maxdepth 1 -type d -name 'elan-*' | wc -l)" -eq 2 ]
[ "$(find "$CONCRETE_EXT_TOOLS" -maxdepth 1 -type d -name 'nanoda-*' | wc -l)" -eq 5 ]
cp "$TMP/builder" "$TMP/project/scripts/proofs/lake_tool.sh"
# All three project-toolchain substitutions must fail accepted distinct-tool dispatch.
for replacement in 'leanprover/lean4:v4.35.0' '"$TOOLCHAIN"' '"$(cat lean-toolchain)"'; do
  python3 - "$TMP/builder" "$TMP/project/scripts/proofs/lake_tool.sh" "$replacement" <<'CONTROL'
from pathlib import Path
import sys
s = Path(sys.argv[1]).read_text()
old = '"$elan" run --install "$tc" lake build'
assert s.count(old) == 1
Path(sys.argv[2]).write_text(s.replace(old, '"$elan" run --install '+sys.argv[3]+' lake build'))
CONTROL
  run_driver 'CONTROL: per-tool toolchain dispatch wrong'
done
cp "$TMP/builder" "$TMP/project/scripts/proofs/lake_tool.sh"
# A retained lock must fail the first post-build population query.
sed 's/^flock -u 9.*/: # negative control: retain lock/' "$TMP/driver" > "$TMP/project/scripts/proofs/external.sh"
run_driver 'CONTROL: caller lock held'
cp "$TMP/driver" "$TMP/project/scripts/proofs/external.sh"
cp "$DISPATCH_LOG" "$TMP/expected"
# Corrupt each existing cache entry without changing its key: exercise the cache-hit check.
for tool in lean4export con-leche; do
  # Corrupt all recipe/identity trees, so the selected identity must refuse.
  for tree in "$CONCRETE_EXT_TOOLS"/"$tool"-*; do printf '%s\n' leanprover/lean4:v4.34.1 > "$tree/lean-toolchain"; done
  run_driver 'worktree=leanprover/lean4:v4.34.1'
  cmp "$TMP/expected" "$DISPATCH_LOG"
  for tree in "$CONCRETE_EXT_TOOLS"/"$tool"-*; do git -C "$tree" show HEAD:lean-toolchain > "$tree/lean-toolchain"; done
done
# A wrong con-leche pin must be checked against the source revision before dispatch.
sed 's/^CONLECHE_TOOLCHAIN=.*/CONLECHE_TOOLCHAIN=leanprover\/lean4:v4.34.1/' "$TMP/pins" > "$TMP/project/scripts/proofs/external.pins"
run_driver 'toolchain mismatch'
cmp "$TMP/expected" "$DISPATCH_LOG"
# Exporter and project must agree even before source checkout/build.
sed 's/^LEAN4EXPORT_TOOLCHAIN=.*/LEAN4EXPORT_TOOLCHAIN=leanprover\/lean4:v4.34.1/' "$TMP/pins" > "$TMP/project/scripts/proofs/external.pins"
run_driver 'differs from project'
cmp "$TMP/expected" "$DISPATCH_LOG"
[ "$(cat "$TMP/shared-elan/sentinel")" = untouched ]
[ "$(find "$TMP/shared-elan" -type f | wc -l)" -eq 1 ]
bounded_gate_status
echo 'external toolchain: full driver, home/lock phases, elan/recipe cache isolation and pin controls pass'
