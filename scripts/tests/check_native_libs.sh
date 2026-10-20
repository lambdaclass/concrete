#!/usr/bin/env bash
# NATIVE LIBRARIES DECLARED IN Concrete.toml [native] (gap G1, docs/platform/NATIVE_LIBRARIES.md).
#
# A package declares the C libraries its extern fns resolve against; `concrete build` (and
# `concrete test`) link them into the final binary, for the root package and for every dependency
# package. Before this, the link line was fixed and every foreign symbol outside libc was
# unresolved (docs/platform/ROCKSDB_SLICE1.md, G1).
#
# WHAT EACH SECTION ESTABLISHES
#   1  a root package's `libs` + `lib-dirs` link a static C library built here, and the program runs;
#      the same project WITHOUT the declaration fails to link (the declaration is load-bearing)
#   2  propagation: a dependency package declares a SHARED library; the application declares
#      nothing, builds, and runs without any loader variable (the lib-dir's rpath is on the link)
#   3  pkg-config, through a stub pkg-config on PATH (deterministic, no system package needed):
#      a known package links; an unknown one fails closed with the diagnostic; a missing pkg-config
#      fails closed; a .pc emitting a flag outside the accepted set is refused, not forwarded
#   4  fail closed on malformed declarations, and no flag injection: flag-shaped names, a comma in
#      a directory, unknown keys, [native.*] tables, multi-line arrays and missing directories are
#      each refused by name, and never produce a binary
#   5  trust visibility: --report unsafe and the compiler ledger list every declared library
#   6  `concrete test` links the declared libraries too (with a failing-test control), and so does a
#      single-file compile inside a package (`concrete <file> -o`), which refuses a malformed
#      [native] exactly as project mode does
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
if command -v timeout >/dev/null 2>&1; then TO="timeout 120"; else TO=""; echo "  warn 'timeout' not found — running without a hang watchdog"; fi
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

CCX=""
for c in cc clang; do command -v "$c" >/dev/null 2>&1 && { CCX="$c"; break; }; done
[ -n "$CCX" ] || { echo "FATAL: no C compiler (cc or clang) to build the test library" >&2; exit 2; }
SYSROOT=()
if [ "$(uname -s)" = "Darwin" ]; then
  # @rpath install name: the binary can only find the library through the rpath the build adds,
  # not through an absolute path recorded at library build time.
  SYSROOT=("--sysroot=$(xcrun --show-sdk-path)"); SHEXT=dylib; SHFLAGS=(-dynamiclib -Wl,-install_name,@rpath/libnlshared.dylib)
else
  SHEXT=so; SHFLAGS=(-shared -fPIC)
fi

# The tiny C library: nl_add(a, b) = a + b. A static archive and a shared library, each in its own dir.
cat > "$TMP/nl.c" <<'EOF'
long long nl_add(long long a, long long b) { return a + b; }
EOF
mkdir -p "$TMP/static" "$TMP/shared"
"$CCX" "${SYSROOT[@]}" -c -fPIC "$TMP/nl.c" -o "$TMP/nl.o" \
  && ar rcs "$TMP/static/libnladd.a" "$TMP/nl.o" \
  && "$CCX" "${SYSROOT[@]}" "${SHFLAGS[@]}" "$TMP/nl.c" -o "$TMP/shared/libnlshared.$SHEXT" \
  || { echo "FATAL: could not build the test C library" >&2; exit 2; }

# mkapp <dir> <native-section-or-empty>: an application calling nl_add directly; exits 42.
mkapp(){ local d="$1" native="$2"
  mkdir -p "$d/src"
  printf '[package]\nname = "nlapp"\nversion = "0.1.0"\n%s\n' "$native" > "$d/Concrete.toml"
  cat > "$d/src/main.con" <<'EOF'
mod nlapp {
    trusted extern fn nl_add(a: i64, b: i64) with() -> i64;

    fn main() -> Int {
        return nl_add(40, 2);
    }
}
EOF
}
# build_run <dir> <tag>: concrete build then run; records build rc, run rc and output.
build_run(){ local d="$1" tag="$2"
  rm -f "$TMP/$tag.bin"
  ( cd "$d" && $TO "$C" build -o "$TMP/$tag.bin" ) > "$TMP/$tag.build" 2>&1; echo $? > "$TMP/$tag.brc"
  if [ -x "$TMP/$tag.bin" ]; then
    # No loader variables: the binary must find its libraries on its own.
    env -u DYLD_LIBRARY_PATH -u LD_LIBRARY_PATH "$TMP/$tag.bin" > "$TMP/$tag.out" 2>&1; echo $? > "$TMP/$tag.rrc"
  else
    echo none > "$TMP/$tag.rrc"
  fi
}
# refused <dir> <tag> <expected-diagnostic-fragment> <label> [build env...]
refused(){ local d="$1" tag="$2" frag="$3" label="$4"; shift 4
  rm -f "$TMP/$tag.bin"
  # The environment is set by `env` AFTER the watchdog, so a test PATH cannot hide `timeout` itself.
  ( cd "$d" && $TO env "$@" "$C" build -o "$TMP/$tag.bin" ) > "$TMP/$tag.build" 2>&1; local rc=$?
  if [ "$rc" -ne 0 ] && [ ! -e "$TMP/$tag.bin" ] && grep -qF -- "$frag" "$TMP/$tag.build"; then
    ok "$label"
  else
    no "$label (rc=$rc, binary $( [ -e "$TMP/$tag.bin" ] && echo produced || echo absent)): $(head -3 "$TMP/$tag.build" | tr '\n' ' ')"
  fi
}

echo "=== 1: a root package declares a static library ==="
mkapp "$TMP/app1" '
[native]
libs = ["nladd"]
lib-dirs = ["lib"]'
mkdir -p "$TMP/app1/lib" && cp "$TMP/static/libnladd.a" "$TMP/app1/lib/"
build_run "$TMP/app1" app1
[ "$(cat "$TMP/app1.brc")" = "0" ] && ok "concrete build links the declared library (lib-dirs relative to the package root)" \
  || no "build failed: $(head -5 "$TMP/app1.build" | tr '\n' ' ')"
[ "$(cat "$TMP/app1.rrc")" = "42" ] && ok "the program runs and the foreign call returns 40 + 2" \
  || no "run: rc=$(cat "$TMP/app1.rrc") $(head -2 "$TMP/app1.out" 2>/dev/null | tr '\n' ' ')"
mkapp "$TMP/app1none" ''
mkdir -p "$TMP/app1none/lib" && cp "$TMP/static/libnladd.a" "$TMP/app1none/lib/"
refused "$TMP/app1none" app1none "no native library is declared" \
  "the same project without [native] fails to link, and the note points at [native] (the declaration is load-bearing)"
grep -q "nl_add" "$TMP/app1none.build" && ok "  ... the link error names the unresolved symbol" \
  || no "  ... the link error does not name nl_add"

echo "=== 2: propagation from a dependency package (shared library, rpath) ==="
DEP="$TMP/app2/nlbind"
mkdir -p "$DEP/src" "$DEP/native"
# MOVED, not copied: no other copy exists for the loader to find by accident.
mv "$TMP/shared/libnlshared.$SHEXT" "$DEP/native/"
printf '[package]\nname = "nlbind"\nversion = "0.1.0"\n\n[native]\nlibs = ["nlshared"]   # comment after the array\nlib-dirs = ["native"]\n' > "$DEP/Concrete.toml"
cat > "$DEP/src/lib.con" <<'EOF'
mod nlbind {
    trusted extern fn nl_add(a: i64, b: i64) with() -> i64;

    pub fn add(a: i64, b: i64) -> i64 {
        return nl_add(a, b);
    }
}
EOF
mkdir -p "$TMP/app2/src"
printf '[package]\nname = "nlapp2"\nversion = "0.1.0"\n\n[dependencies]\nnlbind = { path = "nlbind" }\n' > "$TMP/app2/Concrete.toml"
cat > "$TMP/app2/src/main.con" <<'EOF'
mod nlapp2 {
    import nlbind.{add};

    fn main() -> Int {
        return add(30, 12);
    }
}
EOF
grep -q '^\[native\]' "$TMP/app2/Concrete.toml" && no "fixture error: the application must declare nothing" || ok "the application's own manifest declares no native library"
build_run "$TMP/app2" app2
[ "$(cat "$TMP/app2.brc")" = "0" ] && ok "the dependency's declaration reaches the application's link" \
  || no "build failed: $(head -5 "$TMP/app2.build" | tr '\n' ' ')"
[ "$(cat "$TMP/app2.rrc")" = "42" ] && ok "the program runs with no loader variable (the declared lib-dir is on the rpath)" \
  || no "run: rc=$(cat "$TMP/app2.rrc") $(head -2 "$TMP/app2.out" 2>/dev/null | tr '\n' ' ')"
# CONTROL: the binary really loads the shared library from that directory — with it moved away
# the same binary no longer runs.
if [ -x "$TMP/app2.bin" ]; then
  mv "$DEP/native" "$DEP/native.away"
  # In a subshell so the loader's abort is not reported on the gate's own stderr.
  ( env -u DYLD_LIBRARY_PATH -u LD_LIBRARY_PATH "$TMP/app2.bin" ) > /dev/null 2>&1; rca=$?
  mv "$DEP/native.away" "$DEP/native"
  [ "$rca" != "42" ] && ok "CONTROL: with the declared lib-dir moved away the same binary fails (rc=$rca): the library is loaded from it" \
    || no "CONTROL: the binary still ran with the library moved away — the rpath leg proves nothing"
else
  no "CONTROL: no binary to run the moved-library control against"
fi

echo "=== 3: pkg-config (stub on PATH) ==="
STUB="$TMP/stubbin"; mkdir -p "$STUB" "$TMP/pc_static"
cp "$TMP/static/libnladd.a" "$TMP/pc_static/"
cat > "$STUB/pkg-config" <<EOF
#!/bin/sh
# Stub pkg-config: knows 'nlstub' (links libnladd) and 'nlevil' (emits a flag outside the allow-list).
case "\$*" in
  --version) echo 0.29.2 ;;
  "--print-errors --exists nlstub"|"--print-errors --exists nlevil") exit 0 ;;
  "--print-errors --exists "*) echo "Package \${3} was not found in the pkg-config search path." >&2
                               echo "No package '\${3}' found" >&2; exit 1 ;;
  "--libs nlstub") echo "-L$TMP/pc_static -lnladd" ;;
  "--libs nlevil") echo "-L$TMP/pc_static -lnladd -Wl,-undefined,dynamic_lookup" ;;
  *) exit 1 ;;
esac
EOF
chmod +x "$STUB/pkg-config"
mkapp "$TMP/app3" '
[native]
pkg-config = ["nlstub"]'
( export PATH="$STUB:$PATH"; build_run "$TMP/app3" app3 )
[ "$(cat "$TMP/app3.brc")" = "0" ] && [ "$(cat "$TMP/app3.rrc")" = "42" ] \
  && ok "a pkg-config package's --libs link the library and the program runs" \
  || no "pkg-config build: brc=$(cat "$TMP/app3.brc") rrc=$(cat "$TMP/app3.rrc"): $(head -5 "$TMP/app3.build" | tr '\n' ' ')"
mkapp "$TMP/app3missing" '
[native]
pkg-config = ["concrete-no-such-package"]'
refused "$TMP/app3missing" app3missing "pkg-config cannot find 'concrete-no-such-package'" \
  "an unknown pkg-config package fails closed, naming the package" PATH="$STUB:$PATH"
grep -qF "package 'nlapp' declares [native]" "$TMP/app3missing.build" && ok "  ... and naming the declaring package" \
  || no "  ... the diagnostic does not name the declaring package"
grep -qF "No package 'concrete-no-such-package' found" "$TMP/app3missing.build" && ok "  ... and carrying pkg-config's own explanation" \
  || no "  ... pkg-config's explanation was dropped"
refused "$TMP/app3" app3nopc "pkg-config was not found" \
  "no pkg-config on PATH fails closed (never silently skipped)" PATH="/nonexistent-concrete-path"
mkapp "$TMP/app3evil" '
[native]
pkg-config = ["nlevil"]'
refused "$TMP/app3evil" app3evil "emitted '-Wl,-undefined,dynamic_lookup', which is not an accepted link flag" \
  "a pkg-config flag outside -L/-l/-pthread/-framework is refused, not forwarded" PATH="$STUB:$PATH"

echo "=== 4: malformed declarations fail closed; names cannot inject flags ==="
bad(){ local tag="$1" native="$2" frag="$3" label="$4"
  mkapp "$TMP/$tag" "$native"
  refused "$TMP/$tag" "$tag" "$frag" "$label"
  # Refused at load, so `check` refuses too: no command reads the program while ignoring the manifest.
  ( cd "$TMP/$tag" && $TO "$C" check ) > "$TMP/$tag.check" 2>&1 \
    && no "  ... '$tag': concrete check accepted the malformed manifest" || true
}
bad flagname '
[native]
libs = ["-fplugin=/tmp/evil.so"]' "is not a library name" "a flag-shaped library name is refused"
bad flagpc '
[native]
pkg-config = ["--define-variable=prefix=/tmp"]' "is not a library name" "a flag-shaped pkg-config name is refused"
bad spacename '
[native]
libs = ["nladd -lc"]' "is not a library name" "a name with a space is refused (one name is one -l argument)"
bad commadir '
[native]
lib-dirs = ["lib,-undefined,dynamic_lookup"]' "is not an accepted library directory" "a comma in a lib-dir is refused (it would split -Wl,-rpath)"
bad dashdir '
[native]
lib-dirs = ["-Lother"]' "is not an accepted library directory" "a flag-shaped lib-dir is refused"
bad unknownkey '
[native]
ldflags = ["-static"]' "unknown key 'ldflags'" "an unknown key is refused"
bad subtable '
[native.rocksdb]
pkg-config = ["rocksdb"]' "unsupported table '[native.rocksdb]'" "a [native.*] table is refused rather than ignored"
bad multiline '
[native]
libs = [
  "nladd",
]' "arrays must be on one line" "a multi-line array is refused rather than half-read"
bad notarray '
[native]
libs = "nladd"' "expected an array of strings" "a bare string is refused"
BS="$(printf '%b' '\0134')"   # one backslash, built rather than typed
bad escape "
[native]
libs = [\"nl${BS}u0041dd\"]" "escape sequences are not accepted" "an escape sequence is refused"
grep -qF "nl${BS}u0041dd" "$TMP/escape/Concrete.toml" && ok "  ... (the fixture really carries a backslash escape)" \
  || no "  ... fixture error: the escape fixture has no backslash"
# Spellings of the native table this line reader does not read are refused, not ignored.
bad quotedhdr '
["native"]
libs = ["nladd"]' "unsupported table" 'a quoted header ["native"] is refused'
bad arraytable '
[[native]]
libs = ["nladd"]' "unsupported table" "an array table [[native]] is refused"
bad spacedhdr '
[ native ]
libs = ["nladd"]' "unsupported table" "a spaced header [ native ] is refused"
cat > "$TMP/topkey.toml" <<'EOF'
native.libs = ["nladd"]
[package]
name = "nlapp"
version = "0.1.0"
EOF
mkapp "$TMP/topkey" ''; cp "$TMP/topkey.toml" "$TMP/topkey/Concrete.toml"
refused "$TMP/topkey" topkey "unsupported top-level key 'native.libs'" "a top-level dotted key native.libs is refused"
cat > "$TMP/topinline.toml" <<'EOF'
native = { libs = ["nladd"] }
[package]
name = "nlapp"
version = "0.1.0"
EOF
mkapp "$TMP/topinline" ''; cp "$TMP/topinline.toml" "$TMP/topinline/Concrete.toml"
refused "$TMP/topinline" topinline "unsupported top-level key 'native'" "a top-level inline table native = {...} is refused"
bad dupsection '
[native]
libs = ["nladd"]
[native]
lib-dirs = ["lib"]' "a second [native] section" "a second [native] section is refused"
bad dupkey '
[native]
libs = ["nladd"]
libs = ["m"]' "key 'libs' is given twice" "a repeated key is refused"
mkapp "$TMP/nodir" '
[native]
libs = ["nladd"]
lib-dirs = ["does-not-exist"]'
refused "$TMP/nodir" nodir "/nodir/does-not-exist' does not exist" "a missing lib-dir is refused, naming it"
# A malformed dependency manifest is refused naming the dependency.
cp -R "$TMP/app2" "$TMP/app2bad"
printf '[package]\nname = "nlbind"\nversion = "0.1.0"\n\n[native]\nlibs = ["-lc"]\n' > "$TMP/app2bad/nlbind/Concrete.toml"
refused "$TMP/app2bad" app2bad "error: dependency 'nlbind': Concrete.toml [native]" "a dependency's malformed [native] is refused, naming the dependency"
# An existing but unreadable dependency manifest is refused, not treated as absent (which would
# drop its declarations). Not testable as root, which reads any file.
if [ "$(id -u)" != "0" ]; then
  cp -R "$TMP/app2" "$TMP/app2unread"
  chmod 000 "$TMP/app2unread/nlbind/Concrete.toml"
  refused "$TMP/app2unread" app2unread "error: dependency 'nlbind': cannot read" "an unreadable dependency manifest is refused, not treated as absent"
  chmod 644 "$TMP/app2unread/nlbind/Concrete.toml"
else
  ok "unreadable-manifest leg not applicable as root (stated, not skipped silently)"
fi

echo "=== 5: native libraries are visible trust dependencies ==="
( cd "$TMP/app2" && $TO "$C" src/main.con --report unsafe ) > "$TMP/unsafe.out" 2>&1; rcu=$?
[ "$rcu" = "0" ] && grep -qF "=== Unsafe Signature Summary ===" "$TMP/unsafe.out" \
  && grep -qF "Native libraries (trusted foreign dependencies" "$TMP/unsafe.out" \
  && grep -qF "package 'nlbind': library 'nlshared' (-lnlshared)" "$TMP/unsafe.out" \
  && grep -qF "package 'nlbind': library directory 'native'" "$TMP/unsafe.out" \
  && ok "--report unsafe lists the dependency's library and directory as trusted foreign dependencies" \
  || no "--report unsafe (rc=$rcu) does not list the native library: $(tail -4 "$TMP/unsafe.out" | tr '\n' ' ')"
( cd "$TMP/app2" && $TO "$C" --report compiler-ledger ) > "$TMP/ledger.out" 2>&1; rcl=$?
[ "$rcl" = "0" ] && grep -qF "[native-library] nlbind = libs:nlshared (trusted foreign dependency)" "$TMP/ledger.out" \
  && grep -qF "[native-library] nlbind = lib-dirs:native (trusted foreign dependency)" "$TMP/ledger.out" \
  && ok "the compiler ledger records the native library and its directory" \
  || no "the compiler ledger (rc=$rcl) does not record the native library: $(grep -i native "$TMP/ledger.out" | head -2 | tr '\n' ' ')"
( cd "$TMP/app1none" && $TO "$C" src/main.con --report unsafe ) > "$TMP/unsafe_none.out" 2>&1; rcn=$?
# The control must be a REAL report: a crash or a timeout would also print no native section.
if [ "$rcn" = "0" ] && grep -qF "=== Unsafe Signature Summary ===" "$TMP/unsafe_none.out" \
   && grep -qF "nl_add" "$TMP/unsafe_none.out"; then
  grep -qF "Native libraries" "$TMP/unsafe_none.out" \
    && no "a project declaring no native library still prints a native section" \
    || ok "a project declaring none prints a full report with no native section (the section is not boilerplate)"
else
  no "the no-native control report did not complete (rc=$rcn): $(head -3 "$TMP/unsafe_none.out" | tr '\n' ' ')"
fi

echo "=== 6: concrete test and single-file compiles link the declared libraries ==="
# mktest <dir> <expected-sum>: a #[test] that passes iff nl_add(40, 2) equals the expected sum.
mktest(){ local d="$1" want="$2"
  mkdir -p "$d/src" "$d/lib"; cp "$TMP/static/libnladd.a" "$d/lib/"
  printf '[package]\nname = "nlapp6"\nversion = "0.1.0"\n\n[native]\nlibs = ["nladd"]\nlib-dirs = ["lib"]\n' > "$d/Concrete.toml"
  cat > "$d/src/main.con" <<EOF
mod nlapp6 {
    trusted extern fn nl_add(a: i64, b: i64) with() -> i64;

    #[test]
    fn test_nl_add() -> i32 {
        if nl_add(40, 2) == $want {
            return 0;
        }
        return 1;
    }

    fn main() -> Int {
        return 0;
    }
}
EOF
}
mktest "$TMP/app6" 42
( cd "$TMP/app6" && $TO "$C" test ) > "$TMP/app6.out" 2>&1; rc6=$?
[ "$rc6" = "0" ] && grep -qF "PASS: test_nl_add" "$TMP/app6.out" \
  && ok "concrete test links the library and the test calling it runs and passes" \
  || no "concrete test: rc=$rc6 $(head -5 "$TMP/app6.out" | tr '\n' ' ')"
# Control: the same test expecting the wrong sum must run and FAIL, so the pass above is the call's.
mktest "$TMP/app6bad" 43
( cd "$TMP/app6bad" && $TO "$C" test ) > "$TMP/app6bad.out" 2>&1; rc6b=$?
[ "$rc6b" != "0" ] && grep -qF "FAIL: test_nl_add" "$TMP/app6bad.out" \
  && ok "CONTROL: the same test expecting 43 links, runs and fails (the test really executes the call)" \
  || no "CONTROL: wrong-sum test: rc=$rc6b $(head -5 "$TMP/app6bad.out" | tr '\n' ' ')"
# Single-file mode inside a package links the package's own declarations too.
rm -f "$TMP/app1file.bin"
( cd "$TMP/app1" && $TO "$C" src/main.con -o "$TMP/app1file.bin" ) > "$TMP/app1file.build" 2>&1
if [ -x "$TMP/app1file.bin" ]; then "$TMP/app1file.bin"; rcf=$?; else rcf=none; fi
[ "$rcf" = "42" ] && ok "concrete <file> -o inside a package links the package's [native] and runs" \
  || no "single-file build: rc=$rcf $(head -4 "$TMP/app1file.build" | tr '\n' ' ')"
( cd "$TMP/flagname" && $TO "$C" src/main.con -o "$TMP/flagfile.bin" ) > "$TMP/flagfile.build" 2>&1; rcff=$?
[ "$rcff" != "0" ] && [ ! -e "$TMP/flagfile.bin" ] && grep -qF "is not a library name" "$TMP/flagfile.build" \
  && ok "single-file mode refuses a malformed [native] exactly as project mode does" \
  || no "single-file mode with a malformed [native]: rc=$rcff $(head -3 "$TMP/flagfile.build" | tr '\n' ' ')"

echo ""
echo "NATIVE-LIBS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
