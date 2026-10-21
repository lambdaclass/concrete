#!/usr/bin/env bash
# RocksDB slice 1 acceptance gate (ROADMAP row 5, R-0285; "Feature prerequisites").
#
# The acceptance, verbatim from the roadmap: build and run examples/rocksdb_kv from a fresh clone,
# assert the written value, the typed error and one destruction per handle, and keep two
# compile-fail fixtures rejected: a handle never destroyed and a handle used after it is closed.
# It must build "with the normal commands and no manual flags".
#
# WHAT EACH LEG ESTABLISHES
#   A  `concrete build` in the fresh clone, nothing else. This is the acceptance leg. It links
#      librocksdb because the binding package declares it (`[native] pkg-config = ["rocksdb"]`,
#      docs/platform/NATIVE_LIBRARIES.md); before that declaration existed it failed (gap G1 in
#      docs/platform/ROCKSDB_SLICE1.md, closed). The gate stays red whenever it fails.
#   B  Interim harness, NOT acceptance evidence: when A fails, the same IR (`concrete build
#      --emit-llvm`) is linked by this script with pkg-config's flags so the behaviour legs can run.
#      Every behaviour result below says which binary it ran.
#   C  Behaviour: exit 0, the exact stdout, the typed error (kind Open) for an invalid path, the
#      value read back, an absent key, empty stderr, and the database really created on disk.
#   D  Exactly-once release, observed OUTSIDE the program: a preloaded observer
#      (tests/regressions/rocksdb_slice1/release_observer.c) records every RocksDB handle/buffer handed out
#      and every release. Each class must be acquired once and released once, none live, none bad.
#   E  Observer has teeth: a binding mutated so Value::free never calls rocksdb_free must be
#      reported as a live value by the same observer.
#   F  Compile-fail fixtures: a Db never closed is E0208 and a Db used after close is E0205, each
#      naming `db`, each the only error; each fixture compiles once its marked defect is repaired,
#      so the rejection is caused by the defect and nothing else.
#
# DEPENDENCY: librocksdb found through pkg-config (the repo devshell provides it: `nix develop`).
# A missing library FAILS this gate (exit 1) — it never skips. The repository's skip convention for
# absent tools (check_bv_certificates.sh, check_multi_kernel.sh) covers opt-in prover shells; RocksDB
# is the dependency this acceptance is about, so its absence is a failure to accept, not a skip.
#
# FRESH CLONE: sources come from `git clone` of HEAD (committed content only) and the clone's own
# std (CONCRETE_STD); the compiler binary is this checkout's, verified fresh by require_fresh_binary.
# Rebuilding the compiler inside the clone is the fresh-consumer CI case R-0285 schedules separately.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
finish(){ echo ""; echo "ROCKSDB-SLICE1: PASS=$PASS FAIL=$FAIL"; [ "$FAIL" -eq 0 ] || exit 1; exit 0; }
if command -v timeout >/dev/null 2>&1; then TO="timeout 120"; else TO=""; echo "  warn 'timeout' not found — running without a hang watchdog"; fi

echo "=== prerequisites: RocksDB through pkg-config, a C compiler ==="
if ! command -v pkg-config >/dev/null 2>&1; then
  no "pkg-config not found — run under the repo devshell (nix develop), which provides it and RocksDB"
  finish
fi
if ! pkg-config --exists rocksdb; then
  no "pkg-config cannot find rocksdb — run under the repo devshell (nix develop) or install the RocksDB C library with its rocksdb.pc (e.g. apt-get install librocksdb-dev)"
  finish
fi
ok "rocksdb $(pkg-config --modversion rocksdb) found through pkg-config"
command -v clang >/dev/null 2>&1 && ok "clang found (links the emitted IR, as the compiler does)" || { no "clang not found"; finish; }
# The observer is C, compiled with the platform C compiler: the clang the compiler links with may be
# Lean's bundled one, which ships no resource headers (stdbool.h), and c.h needs them.
command -v cc >/dev/null 2>&1 && ok "cc found (builds the observer)" || { no "cc not found"; finish; }
SYSROOT=()
if [ "$(uname -s)" = "Darwin" ]; then SYSROOT=("--sysroot=$(xcrun --show-sdk-path)"); fi
read -r -a RLIBS <<<"$(pkg-config --libs rocksdb)"
read -r -a RCFLAGS <<<"$(pkg-config --cflags rocksdb)"
RLIBDIR="$(pkg-config --variable=libdir rocksdb)"

# Uncommitted edits are not in the clone; say so rather than certify stale content.
if [ -n "$(git status --porcelain -- examples/rocksdb_kv tests/regressions/rocksdb_slice1 std 2>/dev/null)" ]; then
  echo "  warn uncommitted changes under examples/rocksdb_kv, tests/regressions/rocksdb_slice1 or std are NOT tested (the gate builds a clone of HEAD)"
fi

echo "=== fresh clone ==="
if git clone --quiet --no-hardlinks "$ROOT_DIR" "$TMP/clone" 2>/dev/null; then
  ok "cloned HEAD ($(git -C "$TMP/clone" rev-parse --short HEAD)) into a fresh directory"
else
  no "git clone of the checkout failed"; finish
fi
CL="$TMP/clone"
export CONCRETE_STD="$CL/std"
EX="$CL/examples/rocksdb_kv"
[ -f "$EX/Concrete.toml" ] && [ -f "$EX/rocksdb/src/lib.con" ] \
  && ok "the clone has the example and its binding package" \
  || { no "examples/rocksdb_kv or its rocksdb binding package is not committed"; finish; }
grep -qE '^rocksdb = \{ path = "rocksdb" \}' "$EX/Concrete.toml" \
  && ok "the example declares rocksdb under [dependencies]" \
  || no "examples/rocksdb_kv/Concrete.toml does not declare the rocksdb dependency"
grep -qE '^[[:space:]]*import rocksdb\.\{' "$EX/src/main.con" \
  && ok "the example imports rocksdb with a normal import" \
  || no "examples/rocksdb_kv/src/main.con has no normal 'import rocksdb.{...}'"

# build_example <dir> <out-binary> <tag>: leg A then, if A fails, leg B. Prints the binary's
# provenance into $TMP/<tag>.how ("normal" or "manual-link").
build_example(){ local dir="$1" bin="$2" tag="$3"
  if ( cd "$dir" && $TO "$C" build -o "$bin" ) > "$TMP/$tag.buildA" 2>&1 && [ -x "$bin" ]; then
    echo normal > "$TMP/$tag.how"; return 0
  fi
  if ( cd "$dir" && $TO "$C" build --emit-llvm ) > "$TMP/$tag.ll" 2> "$TMP/$tag.emit.err" \
     && clang "$TMP/$tag.ll" -o "$bin" -w -O2 "${SYSROOT[@]}" "${RLIBS[@]}" -Wl,-rpath,"$RLIBDIR" \
          > "$TMP/$tag.buildB" 2>&1; then
    echo manual-link > "$TMP/$tag.how"; return 0
  fi
  echo none > "$TMP/$tag.how"; return 1
}

echo "=== A: concrete build with no manual flags (the acceptance leg) ==="
BIN="$TMP/rocksdb_kv"
build_example "$EX" "$BIN" kv
HOW="$(cat "$TMP/kv.how")"
if [ "$HOW" = "normal" ]; then
  ok "concrete build produced a runnable binary with no manual flags"
else
  missing="$(grep -oE '_?rocksdb_[a-z_]+' "$TMP/kv.buildA" | sed 's/^_//' | LC_ALL=C sort -u | tr '\n' ' ')"
  no "concrete build failed — gap G1, no native-library declaration (unresolved: ${missing:-see below})"
  grep -qE 'rocksdb_' "$TMP/kv.buildA" || sed 's/^/       /' "$TMP/kv.buildA" | head -10
fi

echo "=== B: interim manual link (NOT acceptance evidence) ==="
case "$HOW" in
  normal)      ok "not needed: leg A's binary is used below" ;;
  manual-link) ok "linked the emitted IR with pkg-config's flags; behaviour legs below run THIS binary" ;;
  *)           no "neither build path produced a binary"; sed 's/^/       /' "$TMP/kv.emit.err" "$TMP/kv.buildB" 2>/dev/null | head -20; finish ;;
esac

echo "=== B2: every foreign declaration stays foreign in the emitted IR ==="
# A binding's `extern fn` must reach LLVM as a `declare`. A `define` of the same symbol means the
# compiler substituted its own body for the C function — gap G3 (bug 103, fixed by 0c507fb9): Mono's
# drop-glue synthesized an empty body for any unresolved call whose name ended in `_destroy`, so
# rocksdb_*_destroy never ran. This leg keeps that from coming back.
( cd "$EX" && $TO "$C" build --emit-llvm ) > "$TMP/ir.ll" 2> "$TMP/ir.err" \
  || { no "concrete build --emit-llvm failed: $(head -3 "$TMP/ir.err" | tr '\n' ' ')"; finish; }
EXTERNS="$(sed -nE 's/^[[:space:]]*extern fn ([A-Za-z_][A-Za-z0-9_]*)\(.*/\1/p' "$EX/rocksdb/src/lib.con")"
[ -n "$EXTERNS" ] || { no "found no extern fn declarations in the binding package"; finish; }
# Each extern must be exactly one `declare` and no `define`: a missing declaration (a call removed
# or renamed by a compiler defect) fails as surely as a substituted body.
n_ext=0; shadowed=""; undeclared=""
for f in $EXTERNS; do
  n_ext=$((n_ext+1))
  if grep -qE "^[[:space:]]*define [^@]*@$f\(" "$TMP/ir.ll"; then
    shadowed="$shadowed $f"
  elif [ "$(grep -cE "^[[:space:]]*declare [^@]*@$f\(" "$TMP/ir.ll")" != "1" ]; then
    undeclared="$undeclared $f"
  fi
done
[ -z "$shadowed" ] \
  && ok "none of the $n_ext extern fns is defined by the compiler" \
  || no "the compiler DEFINED foreign symbols with its own body (gap G3):$shadowed"
[ -z "$undeclared" ] \
  && ok "every extern that is not compiler-defined is exactly one foreign declaration" \
  || no "extern fns without exactly one 'declare' in the IR:$undeclared"

echo "=== D setup: the release observer ==="
OBS_SRC="$CL/tests/regressions/rocksdb_slice1/release_observer.c"
if [ "$(uname -s)" = "Darwin" ]; then
  OBS="$TMP/release_observer.dylib"; OBS_FLAGS=(-dynamiclib); PRELOAD=DYLD_INSERT_LIBRARIES
else
  OBS="$TMP/release_observer.so"; OBS_FLAGS=(-shared -fPIC); PRELOAD=LD_PRELOAD
fi
if cc "${OBS_FLAGS[@]}" -O1 -Wall -Werror "${RCFLAGS[@]}" "$OBS_SRC" -o "$OBS" \
     "${RLIBS[@]}" -Wl,-rpath,"$RLIBDIR" -ldl > "$TMP/obs.err" 2>&1; then
  ok "observer built against rocksdb/c.h (also checks the binding's size_t/u8/pointer width assumptions)"
else
  no "observer failed to build"; sed 's/^/       /' "$TMP/obs.err" | head -20; finish
fi

# run_observed <binary> <tag> <db-dir> <invalid-path>
# The preload variable is set by `env`, which execs the program, AFTER the watchdog: a `timeout`
# that inherited it would load the observer too and append a second (empty) record at its own exit.
run_observed(){ local bin="$1" tag="$2"
  rm -f "$TMP/$tag.obs"
  $TO env "$PRELOAD=$OBS" ROCKSDB_OBSERVER_OUT="$TMP/$tag.obs" "$bin" "$3" "$4" \
    > "$TMP/$tag.out" 2> "$TMP/$tag.err"
  echo $? > "$TMP/$tag.rc"
}
# one_record <tag>: the observer wrote exactly one record (one process observed).
one_record(){ [ -s "$TMP/$1.obs" ] && [ "$(grep -cx record "$TMP/$1.obs")" = "1" ]; }

echo "=== C: behaviour ($HOW binary) ==="
touch "$TMP/a_regular_file"
BAD="$TMP/a_regular_file/db"
DB="$TMP/db"
run_observed "$BIN" run "$DB" "$BAD"
RC="$(cat "$TMP/run.rc")"
[ "$RC" = "0" ] && ok "exit 0" || no "exit $RC (stderr: $(head -3 "$TMP/run.err" | tr '\n' ' '))"
L1="$(sed -n 1p "$TMP/run.out")"
case "$L1" in
  "open-invalid: error kind=Open message="?*)
    ok "invalid path -> typed DbError, kind Open, with RocksDB's message" ;;
  *) no "first line is not the typed Open error: [$L1]" ;;
esac
printf '%s' "$L1" | grep -qF "$BAD" \
  && ok "the error message names the invalid path" \
  || no "the error message does not name $BAD: [$L1]"
EXPECT_REST="open: ok
put: ok bytes=30
get: linear handles, destroyed once
get-absent: none
done: all handles released"
# Byte-for-byte, through files: a command substitution would strip trailing blank lines.
printf '%s\n' "$EXPECT_REST" > "$TMP/expected-rest.out"
sed -n '2,$p' "$TMP/run.out" > "$TMP/actual-rest.out"
cmp -s "$TMP/actual-rest.out" "$TMP/expected-rest.out" \
  && ok "stdout: open ok, put 30 bytes, the written value read back, absent key is None, done (byte-exact)" \
  || { no "stdout differs from the expected transcript"; sed 's/^/       /' "$TMP/run.out"; }
[ ! -s "$TMP/run.err" ] && ok "stderr empty" || no "stderr: $(head -3 "$TMP/run.err" | tr '\n' ' ')"
[ -f "$DB/CURRENT" ] && ok "the database exists on disk ($DB/CURRENT)" || no "no RocksDB database at $DB"
[ ! -e "$BAD" ] && ok "nothing was created at the invalid path" || no "something exists at $BAD"

echo "=== D: each handle released exactly once (observed outside the program) ==="
if one_record run; then
  ok "observer wrote exactly one record (the program's)"
  for cls in options writeoptions readoptions db value errmsg; do
    line="$(grep -E "^$cls " "$TMP/run.obs")"
    [ "$line" = "$cls acquired=1 released=1 live=0 bad=0" ] \
      && ok "$cls: acquired once, released once" \
      || no "$cls: [$line] (want acquired=1 released=1 live=0 bad=0)"
  done
  grep -qx "overflow=0" "$TMP/run.obs" && ok "observer table did not overflow" || no "observer overflowed"
else
  no "observer record missing or not exactly one (was it loaded? $PRELOAD=$OBS): $(grep -c . "$TMP/run.obs" 2>/dev/null) lines"
fi

echo "=== E: the observer has teeth (binding that never frees a value) ==="
MUT="$TMP/mut"; cp -R "$EX" "$MUT"
LIB="$MUT/rocksdb/src/lib.con"
# Value::free is the only consuming release of a value buffer; drop its foreign call.
python3 - "$LIB" <<'PYEOF'
import sys, re
p = sys.argv[1]; s = open(p).read()
pat = r'(pub fn free\(self\) with\(Alloc\) \{\s*let Value \{ ptr, len \} = self;\s*)rocksdb_free\(ptr\);'
s2, n = re.subn(pat, r'\1', s)
if n != 1: sys.exit("mutation site not found exactly once")
open(p, 'w').write(s2)
PYEOF
if [ $? -eq 0 ] && build_example "$MUT" "$TMP/mut_kv" mut; then
  run_observed "$TMP/mut_kv" mutrun "$TMP/mutdb" "$BAD"
  mline="$(grep -E '^value ' "$TMP/mutrun.obs" 2>/dev/null)"
  # The mutant must complete exactly like the baseline (same exit, byte-identical transcript —
  # both runs get the same invalid path —, empty stderr) and its record may differ from the baseline's ONLY in
  # the value line: the control then shows the observer sees this one leak and nothing else moved.
  same_run=false
  [ "$(cat "$TMP/mutrun.rc")" = "$(cat "$TMP/run.rc")" ] && [ ! -s "$TMP/mutrun.err" ] \
    && cmp -s "$TMP/mutrun.out" "$TMP/run.out" && same_run=true
  same_rest=false
  one_record run && one_record mutrun \
    && [ "$(grep -v '^value ' "$TMP/mutrun.obs")" = "$(grep -v '^value ' "$TMP/run.obs")" ] && same_rest=true
  $same_run && ok "the mutant completes like the baseline (exit, transcript, empty stderr)" \
    || no "the mutant did not complete like the baseline (rc=$(cat "$TMP/mutrun.rc"); stderr: $(head -2 "$TMP/mutrun.err" | tr '\n' ' '))"
  $same_rest && [ "$mline" = "value acquired=1 released=0 live=1 bad=0" ] \
    && ok "a leaked value buffer is reported live and nothing else changes (observer is not vacuous)" \
    || no "mutated binding not caught as exactly one leaked value: [$mline]"
else
  no "could not apply or build the observer mutation"
fi

echo "=== F: compile-fail fixtures ==="
# fixture <dir> <code> <message> <repair-python>: rejected with exactly that error, then accepted
# once repaired.
fixture(){ local name="$1" code="$2" msg="$3" repair="$4"
  local d="$CL/tests/regressions/rocksdb_slice1/$name"
  [ -f "$d/src/main.con" ] || { no "$name: fixture missing from the clone"; return; }
  ( cd "$d" && $TO "$C" check ) > "$TMP/$name.out" 2>&1; local rc=$?
  local nerr; nerr="$(grep -c 'error\[' "$TMP/$name.out")"
  if [ "$rc" -ne 0 ] && grep -qF "($code) $msg" "$TMP/$name.out" && [ "$nerr" = "1" ]; then
    ok "$name: rejected with $code ($msg), the only error"
  else
    no "$name: want exactly one '($code) $msg' and a nonzero exit; rc=$rc errors=$nerr: $(grep 'error\[' "$TMP/$name.out" | head -2 | tr '\n' ' ')"
  fi
  local fixed="$TMP/fixed_$name"; cp -R "$d" "$fixed"
  # The copy sits elsewhere, so point its dependency back at the clone's binding package.
  sed -i.bak "s#path = \".*\"#path = \"$EX/rocksdb\"#" "$fixed/Concrete.toml"
  if python3 -c "$repair" "$fixed/src/main.con" && ( cd "$fixed" && $TO "$C" check ) > "$TMP/$name.fixed" 2>&1; then
    ok "$name: compiles once the marked defect is repaired (the rejection is the defect's)"
  else
    no "$name: still rejected after repair: $(grep 'error\[' "$TMP/$name.fixed" | head -2 | tr '\n' ' ')"
  fi
}
fixture handle_never_destroyed E0208 "linear variable 'db' was never consumed" '
import sys; p=sys.argv[1]; s=open(p).read()
assert s.count("// FIX: db.close();")==1
open(p,"w").write(s.replace("// FIX: db.close();","db.close();"))'
fixture handle_used_after_close E0205 "linear variable 'db' used after move" '
import sys; p=sys.argv[1]; ls=open(p).read().split("\n")
keep=[l for l in ls if not l.rstrip().endswith("// DEFECT")]
assert len(keep)==len(ls)-1
open(p,"w").write("\n".join(keep))'

finish
