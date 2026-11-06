#!/usr/bin/env bash
# BUILD-CACHE GATE (R-0546 part 2): scripts/dev/seed-lake.sh and the workflow steps that call it.
#
# A seeded build is a warm START. What this gate holds:
#   1. the key binds platform, lean-toolchain, lake-manifest.json and the lean EXECUTABLE (lake's own
#      traces record only the Lean version, so two binaries of one release would otherwise accept
#      each other's .olean files), and not the sources (a seed from other sources is the useful case);
#   2. seeding REFUSES a toolchain, manifest or lean-executable mismatch and leaves no .lake/build;
#   3. seeding never replaces the build: with --build, `lake build` runs exactly once whatever
#      seeding concluded, and its exit status is the script's; in the workflow, every seed step is
#      self-hosted-only and is followed in its job by the lean-action step that builds;
#   4. a seed is a copy, never a hardlink (lake rewrites trace files in place);
#   5. publish/consume through a cache root picks the exact-sources entry, else the newest.
# Build-free: a fake lean and a fake lake stand in for the toolchain.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SEED="$ROOT_DIR/scripts/dev/seed-lake.sh"
WF="$ROOT_DIR/.github/workflows/lean_action_ci.yml"
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT

PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
[ -f "$SEED" ] || { echo "FAIL: $SEED missing"; exit 1; }

# --- fixtures ---------------------------------------------------------------------------------
COMMIT=0123456789abcdef0123456789abcdef01234567
mklean() { # mklean <dir> <variant>: a fake lean reporting COMMIT, bytes differing per variant
  mkdir -p "$1/bin"
  cat > "$1/bin/lean" <<EOF
#!/usr/bin/env bash
# variant $2
case "\${1:-}" in
  --version) echo "Lean (version 4.34.1, x86_64-unknown-linux-gnu, commit $COMMIT, Release)";;
  --print-prefix) echo "$1";;
esac
EOF
  chmod +x "$1/bin/lean"
}
mklean "$T/leanA" A
mklean "$T/leanB" B
LA="$T/leanA/bin/lean"; LB="$T/leanB/bin/lean"

mkdir -p "$T/fakebin"
cat > "$T/fakebin/lake" <<'EOF'
#!/usr/bin/env bash
echo "lake $*" >> "$LAKE_LOG"
exit "${LAKE_RC:-0}"
EOF
chmod +x "$T/fakebin/lake"
export LAKE_LOG="$T/lake.log"

trace_for() { # trace_for <lean-binary> <commit>
  printf '{"log":[{"message":".> LEAN_PATH=/x/lib/lean %s /src/A.lean -o /x/A.olean"}],\n "inputs":[["Lean 4.34.1, commit %s","e9f3"]]}\n' "$1" "$2"
}
g() { git -c user.name=t -c user.email=t@t -c init.defaultBranch=main "$@"; }

M="$T/main"
mkdir -p "$M/Concrete" "$M/.lake/build/lib/lean/Concrete"
echo "leanprover/lean4:v4.34.1" > "$M/lean-toolchain"
echo '{"version": "1.1.0", "packages": []}' > "$M/lake-manifest.json"
echo 'name = "concrete"' > "$M/lakefile.toml"
echo 'def a := 1' > "$M/Concrete/A.lean"
echo 'def m := 1' > "$M/Main.lean"
echo '.lake/' > "$M/.gitignore"
( cd "$M" && g init -q && g add -A && g commit -qm init ) || { echo "FAIL: fixture repo"; exit 1; }
trace_for "$LA" "$COMMIT" > "$M/.lake/build/lib/lean/Concrete/A.trace"
echo olean > "$M/.lake/build/lib/lean/Concrete/A.olean"
# An orphan trace from another Lean version, naming a binary that no longer exists: ignored.
trace_for /nix/store/gone-lean4-4.28.0/bin/lean fedcba9876543210 > "$M/.lake/build/lib/lean/Concrete/Old.trace"

# Seeding from a checkout demands a quiet source (nothing written in the last 10 s); age the
# fixture build so that only the tests that write to it on purpose see a recent write.
age() { find "$M/.lake" -exec perl -e '$t = time - 60; utime $t, $t, @ARGV' {} +; }
age
newwt() { rm -rf "$T/wt"; ( cd "$M" && g worktree prune && g worktree add -q --detach "$T/wt" HEAD ) >/dev/null 2>&1; }
seed() { LEAN_BIN="${LEAN_BIN_OVERRIDE:-$LA}" bash "$SEED" --checkout "$T/wt" "$@" >"$T/out" 2>&1; }
key_of() { LEAN_BIN="$2" bash "$SEED" --checkout "$1" --print-key 2>/dev/null | awk -F= '$1=="key"{print $2}'; }
src_of() { LEAN_BIN="$LA" bash "$SEED" --checkout "$1" --print-key 2>/dev/null | awk -F= '$1=="source-hash"{print $2}'; }

echo "=== key computation ==="
newwt
K0="$(key_of "$T/wt" "$LA")"; S0="$(src_of "$T/wt")"
[ -n "$K0" ] && [ "$K0" = "$(key_of "$T/wt" "$LA")" ] && ok "key is deterministic ($K0)" || no "key missing or unstable"
[ "$K0" = "$(key_of "$M" "$LA")" ] && ok "a worktree and its main checkout share a key" || no "worktree/main key differ"
[ "$K0" != "$(key_of "$T/wt" "$LB")" ] && ok "a different lean executable (same release) changes the key" || no "key ignores the lean executable"
echo "leanprover/lean4:v4.35.0" > "$T/wt/lean-toolchain"
[ "$K0" != "$(key_of "$T/wt" "$LA")" ] && ok "lean-toolchain changes the key" || no "key ignores lean-toolchain"
( cd "$T/wt" && g checkout -q -- lean-toolchain )
echo '{"version": "1.1.0", "packages": [{"name":"x"}]}' > "$T/wt/lake-manifest.json"
[ "$K0" != "$(key_of "$T/wt" "$LA")" ] && ok "lake-manifest.json changes the key" || no "key ignores the manifest"
( cd "$T/wt" && g checkout -q -- lake-manifest.json )
echo 'def a := 2' > "$T/wt/Concrete/A.lean"
[ "$K0" = "$(key_of "$T/wt" "$LA")" ] && ok "a source edit keeps the key (lake rebuilds what changed)" || no "key depends on sources"
[ "$S0" != "$(src_of "$T/wt")" ] && ok "a source edit changes the source hash" || no "source hash ignores Concrete/*.lean"

echo "=== seeding from the main checkout ==="
newwt
if seed && [ -f "$T/wt/.lake/build/lib/lean/Concrete/A.olean" ]; then ok "matching worktree is seeded"; else no "matching seed failed: $(cat "$T/out")"; fi
if [ "$T/wt/.lake/build/lib/lean/Concrete/A.trace" -ef "$M/.lake/build/lib/lean/Concrete/A.trace" ]; then
  no "seeded trace is a hardlink into the source build"
else ok "seeded files are copies, not hardlinks"; fi
seed; rc=$?
[ $rc -eq 3 ] && ok "an existing .lake/build is not replaced without --force (exit 3)" || no "existing build replaced (exit $rc)"

newwt; echo "leanprover/lean4:v4.35.0" > "$T/wt/lean-toolchain"
seed; rc=$?
[ $rc -eq 3 ] && [ ! -e "$T/wt/.lake/build" ] && ok "toolchain mismatch is refused, nothing copied" || no "toolchain mismatch seeded (exit $rc)"

newwt; echo '{"version": "1.1.0", "packages": [{"name":"x"}]}' > "$T/wt/lake-manifest.json"
seed; rc=$?
[ $rc -eq 3 ] && [ ! -e "$T/wt/.lake/build" ] && ok "manifest mismatch is refused, nothing copied" || no "manifest mismatch seeded (exit $rc)"

newwt; LEAN_BIN_OVERRIDE="$LB" seed; rc=$?
[ $rc -eq 3 ] && [ ! -e "$T/wt/.lake/build" ] && ok "a build made by another lean executable is refused" || no "foreign-executable build seeded (exit $rc)"

echo "=== provenance and consistency ==="
newwt
printf '{"inputs":[["Lean 4.34.1, commit %s","e9f3"]]}\n' "$COMMIT" > "$M/.lake/build/lib/lean/Concrete/Fetched.trace"; age
seed; rc=$?
[ $rc -eq 3 ] && [ ! -e "$T/wt/.lake/build" ] && ok "a same-version trace that names no executable refuses the whole build" || no "unprovenanced trace accepted (exit $rc)"
rm -f "$M/.lake/build/lib/lean/Concrete/Fetched.trace"; age

newwt; touch "$M/.lake/build/lib/lean/Concrete/A.trace"
seed; rc=$?
[ $rc -eq 3 ] && [ ! -e "$T/wt/.lake/build" ] && ok "a source written to in the last 10 s (a build just ran or is running) is refused" || no "recently written source seeded (exit $rc)"
age

newwt
mkdir -p "$T/fakecp"
cat > "$T/fakecp/cp" <<EOF
#!/usr/bin/env bash
PATH="$PATH" command cp "\$@"; rc=\$?
touch "$M/.lake/build/lib/lean/Concrete/A.olean"
exit \$rc
EOF
chmod +x "$T/fakecp/cp"
PATH="$T/fakecp:$PATH" seed; rc=$?
[ $rc -eq 3 ] && [ ! -e "$T/wt/.lake/build" ] \
  && ok "a source written to during the copy discards the copy (a build was running there)" \
  || no "copy of a changing source kept (exit $rc)"
ls "$T/wt/.lake" 2>/dev/null | has_match 'build.seeding\|seed.stamp\|seed.lock' && no "a refused seed left temporary files" || ok "a refused seed leaves no temporary files or lock"
age

newwt
perl -e '$t = time - 30; utime $t, $t, @ARGV' "$M/.lake/build/lib/lean/Concrete/A.olean"
perl -e '$t = time - 90; utime $t, $t, @ARGV' "$M/.lake/build/lib/lean/Concrete/A.trace"
seed; rc=$?
[ $rc -eq 3 ] && [ ! -e "$T/wt/.lake/build" ] && grep -q 'interrupted' "$T/out" \
  && ok "an .olean newer than its trace (a build interrupted mid-module) is refused" || no "interrupted module seeded (exit $rc)"
age

newwt; mkdir -p "$T/wt/.lake/seed.lock"
seed; rc=$?
[ $rc -eq 3 ] && [ ! -e "$T/wt/.lake/build" ] && [ -d "$T/wt/.lake/seed.lock" ] && ok "a concurrent seeder's lock is respected (and not removed)" || no "lock ignored (exit $rc)"

newwt
mkdir -p "$T/home/.elan/bin" "$T/pathlean"
ln -s "$LA" "$T/home/.elan/bin/lean"; ln -s "$LB" "$T/pathlean/lean"
PATH="$T/pathlean:$PATH" HOME="$T/home" bash "$SEED" --checkout "$T/wt" --elan >"$T/out" 2>&1; rc=$?
[ $rc -eq 0 ] && ok "--elan checks ~/.elan's lean even when another lean is first on PATH" || no "--elan did not use ~/.elan (exit $rc): $(cat "$T/out")"
newwt
PATH="$T/pathlean:$PATH" HOME="$T/home" bash "$SEED" --checkout "$T/wt" >"$T/out" 2>&1; rc=$?
[ $rc -eq 3 ] && ok "without --elan the PATH lean is checked (and here refused)" || no "PATH lean not used (exit $rc)"

echo "=== seeding never skips the build ==="
for case in match mismatch; do
  newwt; : > "$LAKE_LOG"
  [ "$case" = mismatch ] && echo "leanprover/lean4:v4.35.0" > "$T/wt/lean-toolchain"
  PATH="$T/fakebin:$PATH" LAKE_RC=7 seed --build --log-level=warning; rc=$?
  n="$(grep -c '^lake build' "$LAKE_LOG")"
  if [ "$n" = 1 ] && [ $rc -eq 7 ] && grep -q '^lake build --log-level=warning$' "$LAKE_LOG"; then
    ok "--build ($case seed): lake build ran once with its args, its exit status (7) is the script's"
  else no "--build ($case seed): lake ran $n times, exit $rc"; fi
done
newwt; : > "$LAKE_LOG"
PATH="$T/fakebin:$PATH" seed --build; rc=$?
[ $rc -eq 0 ] && [ "$(cat "$LAKE_LOG")" = "lake build" ] && ok "bare --build (no lake args) runs lake build" || no "bare --build failed (exit $rc): $(cat "$T/out")"
if [ -x /bin/bash ] && /bin/bash -c '[ "${BASH_VERSINFO[0]}" -lt 4 ]'; then
  newwt; : > "$LAKE_LOG"
  PATH="$T/fakebin:$PATH" LEAN_BIN="$LA" /bin/bash "$SEED" --checkout "$T/wt" --build >"$T/out" 2>&1; rc=$?
  [ $rc -eq 0 ] && [ "$(cat "$LAKE_LOG")" = "lake build" ] && ok "bare --build under bash 3 (/bin/bash)" || no "bare --build under bash 3 failed (exit $rc): $(cat "$T/out")"
fi
grep -qE 'exit 0' <(sed -n '/^run_build_after()/,/^}/p' "$SEED") \
  && no "run_build_after has an exit-0 path that could stand in for the build" \
  || ok "run_build_after has no success exit besides exec lake build"

echo "=== cache root: publish and consume ==="
CR="$T/cache"
PATH="$T/fakebin:$PATH" LEAN_BIN="$LA" bash "$SEED" --checkout "$M" --publish "$CR" >"$T/out" 2>&1; rc=$?
E="$CR/$K0/$S0"
[ $rc -eq 0 ] && [ -f "$E/seed.key" ] && [ -d "$E/build" ] && [ -L "$CR/$K0/latest" ] \
  && ok "publish writes <key>/<srchash>/{build,seed.key} and latest" || no "publish failed (exit $rc): $(cat "$T/out")"
PATH="$T/fakebin:$PATH" LAKE_RC=1 LEAN_BIN="$LA" bash "$SEED" --checkout "$M" --publish "$T/cache2" >"$T/out" 2>&1; rc=$?
[ $rc -ne 0 ] && [ ! -d "$T/cache2/$K0" ] && ok "a failed lake build publishes nothing" || no "published after a failed build"
touch "$E/build/lib/lean/Concrete/A.trace"   # a freshly published entry: no quiet window applies
newwt; seed --cache-root "$CR"; rc=$?
[ $rc -eq 0 ] && grep -q 'exact sources' "$T/out" && ok "consumer picks the exact-sources entry" || no "exact entry not used: $(cat "$T/out")"
newwt; echo 'def a := 3' > "$T/wt/Concrete/A.lean"; seed --cache-root "$CR"; rc=$?
[ $rc -eq 0 ] && grep -q 'newest entry' "$T/out" && ok "other sources fall back to the newest entry" || no "newest fallback failed: $(cat "$T/out")"
newwt; LEAN_BIN_OVERRIDE="$LB" seed --cache-root "$CR"; rc=$?
[ $rc -eq 3 ] && [ ! -e "$T/wt/.lake/build" ] && ok "cache lookup under another lean executable misses" || no "cache served a foreign key (exit $rc)"

echo "=== workflow wiring ==="
python3 - "$WF" > "$T/wf.out" 2>&1 <<'PY'
import sys, yaml
wf = yaml.safe_load(open(sys.argv[1]))
bad, seeded_jobs = [], 0
for name, job in wf["jobs"].items():
    steps = job.get("steps", [])
    idx = [i for i, s in enumerate(steps) if "seed-lake.sh" in str(s.get("run", ""))]
    for i in idx:
        seeded_jobs += 1
        s = steps[i]
        if s.get("if") != "runner.environment == 'self-hosted'":
            bad.append(f"{name}: seed step not self-hosted-only")
        if "--elan" not in str(s.get("run", "")):
            bad.append(f"{name}: seed step does not check against ~/.elan's lean (--elan)")
        if s.get("id") != "lake_seed":
            bad.append(f"{name}: seed step id is not lake_seed")
        later = [t for t in steps[i+1:] if str(t.get("uses", "")).startswith("leanprover/lean-action")]
        if not later:
            bad.append(f"{name}: no lean-action (lake build) after the seed step")
        for t in later:
            w = t.get("with", {}) or {}
            if str(w.get("build", "true")).lower() == "false" or str(w.get("auto-config", "true")).lower() == "false":
                bad.append(f"{name}: lean-action after the seed does not build")
            if w.get("use-github-cache") != "${{ steps.lake_seed.outputs.seeded != 'true' }}":
                bad.append(f"{name}: lean-action does not skip its GitHub .lake cache when seeded")
print(f"seeded_jobs={seeded_jobs}")
for b in bad: print("BAD", b)
sys.exit(1 if bad or seeded_jobs == 0 else 0)
PY
if [ $? -eq 0 ]; then ok "every seed step is self-hosted-only and followed by a building lean-action ($(grep -o 'seeded_jobs=[0-9]*' "$T/wf.out"))"
else no "workflow wiring: $(tr '\n' ';' < "$T/wf.out")"; fi

echo ""
echo "BUILD-CACHE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
