#!/usr/bin/env bash
# SHARED COMPILE CACHE (R-0546): a hit is byte-identical to a fresh compile, and every input the
# key claims to cover really invalidates it (negative controls), so a stale result cannot be served.
#
# Runs in a SANDBOX repository under mktemp: a wrapper "compiler" (whose bytes are the compiler
# identity the key hashes) execs the real binary with CONCRETE_STD pointing at a copied std, and the
# probe project lives inside the sandbox. Nothing in the real tree or the real cache is touched.
#
#   K1  a miss then a hit: stdout, stderr and exit status of the hit equal a fresh run's
#   K2  a rejected program: the cached nonzero status and diagnostics replay exactly
#   K3  editing the input file invalidates (miss, and the new output)
#   K4  editing another file of the project invalidates
#   K5  editing a std file invalidates
#   K6  changing the compiler's bytes invalidates
#   K7  changing the working directory or the argument vector is a different key
#   K8  not cached: a non-allowlisted report, `-o`, extra flags, CONCRETE_COMPILE_CACHE=0,
#       a project with [dependencies], an input outside the binary's repository
#   K9  CONCRETE_COMPILE_CACHE=refresh recomputes
#   K10 an entry is published whole: no temporary is left behind
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "$ROOT_DIR/scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
REAL="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$REAL" ] || { echo "error: build first ($REAL missing)" >&2; exit 2; }
TMP="$(mktemp -d)" || { echo "FATAL: mktemp" >&2; exit 2; }
[ -n "$TMP" ] && [ -d "$TMP" ] || { echo "FATAL: sandbox unusable" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT
TMP="$(cd "$TMP" && pwd -P)"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# --- sandbox repository -------------------------------------------------------------------------
SB="$TMP/repo"
mkdir -p "$SB/.lake/build/bin" "$SB/Concrete" "$SB/proj/src" "$SB/loose"
: > "$SB/lakefile.toml"
cp Concrete/BuildIdentity.lean "$SB/Concrete/"
cp -R std "$SB/std"
COMP="$SB/.lake/build/bin/concrete"
printf '#!/bin/sh\n# sandbox compiler v1\nexec "%s" "$@"\n' "$REAL" > "$COMP"; chmod +x "$COMP"
export CONCRETE_STD="$SB/std"
export CONCRETE_COMPILE_CACHE_DIR="$TMP/cache"
unset CONCRETE_COMPILE_CACHE CCACHE_REPORTS
printf '[package]\nname = "probe"\n' > "$SB/proj/Concrete.toml"
cat > "$SB/proj/src/main.con" <<'EOF'
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

fn main() -> i32 {
    return add(1, 2);
}
EOF
printf 'probe notes v1\n' > "$SB/proj/NOTES.md"
cat > "$SB/loose/bad.con" <<'EOF'
fn main() -> i32 {
    return undefined_name;
}
EOF
source "$ROOT_DIR/scripts/tests/lib/compile_cache.sh"
F="$SB/proj/src/main.con"
cd "$SB"

run_c() { # -> files $TMP/c.out $TMP/c.err $TMP/c.rc and CCACHE_LAST
  ccache_run "$COMP" "$@" >"$TMP/c.out" 2>"$TMP/c.err"; echo $? > "$TMP/c.rc"; }
run_f() { "$COMP" "$@" >"$TMP/f.out" 2>"$TMP/f.err"; echo $? > "$TMP/f.rc"; }
same() { cmp -s "$TMP/c.out" "$TMP/f.out" && cmp -s "$TMP/c.err" "$TMP/f.err" && cmp -s "$TMP/c.rc" "$TMP/f.rc"; }

echo "=== K1: miss, then a hit identical to a fresh run ==="
run_c "$F" --report audit; first="$CCACHE_LAST"
run_c "$F" --report audit; second="$CCACHE_LAST"
run_f "$F" --report audit
[ "$first" = miss ] && [ "$second" = hit ] && ok "first call misses, second hits" || no "expected miss,hit; got $first,$second"
same && ok "the hit's stdout, stderr and exit status equal a fresh run" || no "the hit differs from a fresh run"
[ -s "$TMP/c.out" ] && ok "...and the report is not empty" || no "the cached report is empty"

echo "=== K2: a failing compile replays its status and diagnostics ==="
B="$SB/loose/bad.con"
run_c "$B" --report audit; run_c "$B" --report audit; h="$CCACHE_LAST"
run_f "$B" --report audit
[ "$h" = hit ] && [ "$(cat "$TMP/c.rc")" != 0 ] && same \
  && ok "a rejected program's nonzero status and diagnostics are replayed exactly" \
  || no "failing compile: last=$h rc=$(cat "$TMP/c.rc") fresh rc=$(cat "$TMP/f.rc")"

echo "=== K3-K6: every covered input invalidates ==="
inval() { # label, then a mutation command string
  run_c "$F" --report audit; [ "$CCACHE_LAST" = hit ] || run_c "$F" --report audit
  local before; before="$(cat "$TMP/c.out")"
  eval "$2"
  run_c "$F" --report audit; local l="$CCACHE_LAST"
  run_f "$F" --report audit
  if [ "$l" = miss ] && same; then ok "$1 invalidates the entry (miss, output equals a fresh run)"
  else no "$1 did NOT invalidate (got $l)"; fi
}
inval "editing the input"          "printf '\nfn extra() -> i32 { return 7; }\n' >> '$F'"
inval "editing another project file" "printf 'probe notes v2\n' > '$SB/proj/NOTES.md'"
inval "editing a std file"          "printf '\n// probe\n' >> '$SB/std/src/lib.con'"
inval "changing the compiler bytes" "sed -i.bak 's/sandbox compiler v1/sandbox compiler v2/' '$COMP' && rm -f '$COMP.bak'"
# The input edit must change the REPORT too, or K3 proves only that the key moved.
grep -q extra "$TMP/c.out" && ok "...and the post-edit report mentions the new function (a real recompute)" \
  || no "the post-edit report does not mention the added function"

echo "=== K7: cwd and argv are part of the key ==="
run_c "$F" --report audit; [ "$CCACHE_LAST" = hit ] || run_c "$F" --report audit
( cd "$SB/proj" && ccache_run "$COMP" "$F" --report audit >/dev/null 2>&1; echo "$CCACHE_LAST" > "$TMP/k7" )
[ "$(cat "$TMP/k7")" = miss ] && ok "another working directory is another entry" || no "cwd change: $(cat "$TMP/k7")"
run_c "$F" --report audit --json; [ "$CCACHE_LAST" = miss ] && ok "--json is another entry" || no "--json: $CCACHE_LAST"

echo "=== K8: invocations the key cannot cover are not cached ==="
bypass() { local want="$1"; shift; run_c "$@"; case "$CCACHE_LAST" in bypass:*) ok "$want: ran uncached ($CCACHE_LAST)" ;; *) no "$want was cached ($CCACHE_LAST)" ;; esac; }
bypass "a non-allowlisted report (caps)" "$F" --report caps
bypass "an -o build" "$F" -o "$TMP/a.out"
bypass "extra flags" "$F" --report audit --json --verbose
CONCRETE_COMPILE_CACHE=0 bypass "CONCRETE_COMPILE_CACHE=0" "$F" --report audit
printf '[package]\nname = "probe"\n[dependencies]\nother = { path = "../other" }\n' > "$SB/proj/Concrete.toml"
bypass "a project with [dependencies]" "$F" --report audit
printf '[package]\nname = "probe"\n' > "$SB/proj/Concrete.toml"
mkdir -p "$TMP/outside"; cp "$F" "$TMP/outside/x.con"
bypass "an input outside the binary's repository" "$TMP/outside/x.con" --report audit
same_out_bypass() { run_c "$F" --report caps; run_f "$F" --report caps; same; }
same_out_bypass && ok "a bypassed call still returns exactly what the compiler returns" || no "a bypassed call altered the output"

echo "=== K9: refresh recomputes ==="
run_c "$F" --report audit; [ "$CCACHE_LAST" = hit ] || run_c "$F" --report audit
CONCRETE_COMPILE_CACHE=refresh run_c "$F" --report audit; r="$CCACHE_LAST"
[ "$r" = miss ] && ok "CONCRETE_COMPILE_CACHE=refresh recomputes a present entry" || no "refresh: $r"

echo "=== K10: no temporaries are left behind ==="
left="$(find "$CONCRETE_COMPILE_CACHE_DIR" -name '.tmp.*' 2>/dev/null | wc -l | tr -d ' ')"
[ "$left" = 0 ] && ok "every entry was published by rename (no .tmp.* left)" || no "$left temporaries left in the cache"

echo
echo "COMPILE-CACHE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
