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
# Hang watchdog (scripts/tests/lib/bounded.sh): a timeout is re-run once alone with twice the
# budget; a second timeout prints INCOMPLETE on stderr and returns 124, never a pass.
TO="bash $(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/bounded.sh 120"
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
  rca="$( { env -u DYLD_LIBRARY_PATH -u LD_LIBRARY_PATH "$TMP/app2.bin" > /dev/null 2>&1; echo $?; } 2>/dev/null )"
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
  "--print-errors --exists nlstub"|"--print-errors --exists nlevil"|"--print-errors --exists nlopt"|"--print-errors --exists nld64"|"--print-errors --exists nlesc") exit 0 ;;
  "--print-errors --exists "*) echo "Package \${3} was not found in the pkg-config search path." >&2
                               echo "No package '\${3}' found" >&2; exit 1 ;;
  "--libs nlstub") echo "-L$TMP/pc_static -lnladd" ;;
  "--libs nlevil") echo "-L$TMP/pc_static -lnladd -Wl,-undefined,dynamic_lookup" ;;
  "--libs nlopt") echo "-L$TMP/pc_static -lnladd -ld_classic" ;;
  "--libs nld64") echo "-L$TMP/pc_static -lnladd -ld64" ;;
  "--libs nlesc") echo "-L$TMP/pc_esc\\ -lnladd" ;;
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
mkapp "$TMP/app3opt" '
[native]
pkg-config = ["nlopt"]'
refused "$TMP/app3opt" app3opt "emitted '-ld_classic', which is not an accepted link flag" \
  "a pkg-config -l token that is really a linker option (-ld_classic) is refused" PATH="$STUB:$PATH"
mkapp "$TMP/app3d64" '
[native]
pkg-config = ["nld64"]'
refused "$TMP/app3d64" app3d64 "emitted '-ld64', which is not an accepted link flag" \
  "a pkg-config -ld64 token (ld64's classic-linker switch) is refused" PATH="$STUB:$PATH"
# pkg-config escapes a space in a path as a backslash: "-L<dir>\ -lx" is ONE argument. The
# backslash-ending directory EXISTS here, so an existence check cannot be what refuses it.
mkdir -p "$TMP/pc_esc\\" && cp "$TMP/static/libnladd.a" "$TMP/pc_esc\\/"
mkapp "$TMP/app3esc" '
[native]
pkg-config = ["nlesc"]'
refused "$TMP/app3esc" app3esc "emitted quoting or escaping" \
  "pkg-config output with shell escaping is refused rather than split into extra arguments" PATH="$STUB:$PATH"

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
bad escapedhdr "
[\"${BS}u006eative\"]
libs = [\"nladd\"]" "uses a backslash escape" "an escaped table header (which could spell native) is refused"
cat > "$TMP/topesc.toml" <<EOF
"${BS}u006eative".libs = ["nladd"]
[package]
name = "nlapp"
version = "0.1.0"
EOF
mkapp "$TMP/topesc" ''; cp "$TMP/topesc.toml" "$TMP/topesc/Concrete.toml"
refused "$TMP/topesc" topesc "uses a backslash escape" "an escaped top-level key is refused"
# Text inside a multi-line string is not structure: a description quoting a [native] section
# declares nothing (no -lnosuchlib reaches the link) and is not a second section.
mkapp "$TMP/multistr" ''
cat > "$TMP/multistr/Concrete.toml" <<'EOF'
[package]
name = "nlapp"
version = "0.1.0"
description = """
An example manifest:
[native]
libs = ["nosuchlib"]
"""
notes = '''
[native.x]
'''

[native]
libs = ["nladd"]
lib-dirs = ["lib"]
EOF
mkdir -p "$TMP/multistr/lib" && cp "$TMP/static/libnladd.a" "$TMP/multistr/lib/"
build_run "$TMP/multistr" multistr
[ "$(cat "$TMP/multistr.brc")" = "0" ] && [ "$(cat "$TMP/multistr.rrc")" = "42" ] \
  && ok "[native] text inside multi-line strings is ignored; the real section still links" \
  || no "multi-line string manifest: brc=$(cat "$TMP/multistr.brc") rrc=$(cat "$TMP/multistr.rrc"): $(head -3 "$TMP/multistr.build" | tr '\n' ' ')"
# Delimiters inside a single-line string or a comment do not open a multi-line string, so the
# real section after them is still read (a mis-toggle would silently drop it).
mkapp "$TMP/delimtext" ''
cat > "$TMP/delimtext/Concrete.toml" <<'EOF'
[package]
name = "nlapp"
version = "0.1.0"
description = "''' and \"\"\" inside a plain string"
# a comment with ''' and """ in it

[native] # '''
libs = ["nladd"]
lib-dirs = ["lib"]
EOF
mkdir -p "$TMP/delimtext/lib" && cp "$TMP/static/libnladd.a" "$TMP/delimtext/lib/"
build_run "$TMP/delimtext" delimtext
[ "$(cat "$TMP/delimtext.brc")" = "0" ] && [ "$(cat "$TMP/delimtext.rrc")" = "42" ] \
  && ok "delimiters inside a plain string or a comment do not hide the [native] section after them" \
  || no "delimiter-in-text manifest: brc=$(cat "$TMP/delimtext.brc") rrc=$(cat "$TMP/delimtext.rrc"): $(head -3 "$TMP/delimtext.build" | tr '\n' ' ')"
# A multi-line string may close with four or five quotes; the section after it is still read.
mkapp "$TMP/fourq" ''
cat > "$TMP/fourq/Concrete.toml" <<'EOF'
[package]
name = "nlapp"
version = "0.1.0"
description = """ends with a quote""""
notes = '''ends with two'''''

[native]
libs = ["nladd"]
lib-dirs = ["lib"]
EOF
mkdir -p "$TMP/fourq/lib" && cp "$TMP/static/libnladd.a" "$TMP/fourq/lib/"
build_run "$TMP/fourq" fourq
[ "$(cat "$TMP/fourq.brc")" = "0" ] && [ "$(cat "$TMP/fourq.rrc")" = "42" ] \
  && ok "multi-line strings closed by four or five quotes are read correctly; [native] after them links" \
  || no "four-quote manifest: brc=$(cat "$TMP/fourq.brc") rrc=$(cat "$TMP/fourq.rrc"): $(head -3 "$TMP/fourq.build" | tr '\n' ' ')"
# A bracket line inside a multi-line ARRAY value is not a table header, so it cannot end the top
# level and let a later top-level native key through.
mkapp "$TMP/arrval" ''
cat > "$TMP/arrval/Concrete.toml" <<'EOF'
metadata = [
  [1],
]
native.libs = ["nosuchlib"]
[package]
name = "nlapp"
version = "0.1.0"
EOF
refused "$TMP/arrval" arrval "unsupported top-level key 'native.libs'" "a top-level native key after a multi-line array holding [1] is still refused"
# A bracket closed by the wrong delimiter is malformed; it must not end the value early or late
# and hide the [native] lines it encloses.
mkapp "$TMP/mismatch" ''
cat > "$TMP/mismatch/Concrete.toml" <<'EOF'
metadata = [
  1,
}
[package]
name = "nlapp"
version = "0.1.0"

[native]
libs = ["m"]
EOF
refused "$TMP/mismatch" mismatch "mismatched '}'" "a '[' closed by '}' is refused (a mismatched value cannot end early or late and hide lines)"
# The balanced counterpart: brackets match, but a [native] section inside an array value is not
# TOML. Refused, not skipped as value text.
mkapp "$TMP/inarray" ''
cat > "$TMP/inarray/Concrete.toml" <<'EOF'
[package]
name = "nlapp"
version = "0.1.0"
metadata = [
[native]
libs = ["nosuchlib"]
]
EOF
refused "$TMP/inarray" inarray "unexpected text in or after a value" "a [native] section hidden inside a balanced multi-line array is refused"
# Its string-quoted variant: the header is a valid string element, but the key line after it is an
# assignment inside an array, which no TOML array holds.
mkapp "$TMP/inarrayq" ''
cat > "$TMP/inarrayq/Concrete.toml" <<'EOF'
[package]
name = "nlapp"
version = "0.1.0"
metadata = [
["native"]
libs = ["lost"]
]
EOF
refused "$TMP/inarrayq" inarrayq "unexpected text in or after a value" "a quoted [\"native\"] plus key hidden inside a multi-line array is refused"
# Structure after a multi-line string closes, on the closing line, is refused rather than skipped.
mkapp "$TMP/afterclose" ''
cat > "$TMP/afterclose/Concrete.toml" <<'EOF'
[package]
name = "nlapp"
version = "0.1.0"
description = """text
""" [native]
libs = ["lost"]
EOF
refused "$TMP/afterclose" afterclose "unexpected text in or after a value" "a [native] header after a multi-line string's closing delimiter is refused"
# The same with the string closing on its OPENING line, and after a multi-line array closes.
mkapp "$TMP/sameline" ''
printf '[package]\nname = "nlapp"\nversion = "0.1.0"\ndescription = """text""" [native]\nlibs = ["lost"]\n' > "$TMP/sameline/Concrete.toml"
refused "$TMP/sameline" sameline "unexpected text in or after a value" "a [native] header after a same-line triple-quoted string is refused"
mkapp "$TMP/afterarr" ''
printf '[package]\nname = "nlapp"\nversion = "0.1.0"\nmetadata = [\n] ["native"]\nlibs = ["lost"]\n' > "$TMP/afterarr/Concrete.toml"
refused "$TMP/afterarr" afterarr "unexpected text in or after a value" 'a ["native"] header after a multi-line array closes is refused'
mkapp "$TMP/afterinl" ''
printf '[package]\nname = "nlapp"\nversion = "0.1.0"\nmeta = { a = [\n1] } ["native"]\nlibs = ["lost"]\n' > "$TMP/afterinl/Concrete.toml"
refused "$TMP/afterinl" afterinl "unexpected text in or after a value" 'a ["native"] header after an inline table closes is refused'
# A header spanning lines or holding a comment, and a header hidden in a multi-line inline table.
mkapp "$TMP/hdrlines" ''
printf '[package]\nname = "nlapp"\nversion = "0.1.0"\n[\nnative\n]\nlibs = ["m"]\n' > "$TMP/hdrlines/Concrete.toml"
refused "$TMP/hdrlines" hdrlines "unexpected text in or after a table header" "a header split across lines is refused"
mkapp "$TMP/hdrcomment" ''
printf '[package]\nname = "nlapp"\nversion = "0.1.0"\n[ # comment\nnative]\nlibs = ["m"]\n' > "$TMP/hdrcomment/Concrete.toml"
refused "$TMP/hdrcomment" hdrcomment "unexpected text in or after a table header" "a header holding a comment is refused"
mkapp "$TMP/inltable" ''
printf '[package]\nname = "nlapp"\nversion = "0.1.0"\nmetadata = {\n["native"]\nlibs = ["lost"]\n}\n' > "$TMP/inltable/Concrete.toml"
refused "$TMP/inltable" inltable "unexpected text in or after a value" "a quoted header hidden in a multi-line inline table is refused"
# Control: ordinary values of every shape the reader validates still build.
mkapp "$TMP/shapes" '
[native]
libs = ["nladd"]
lib-dirs = ["lib"]'
python3 - "$TMP/shapes/Concrete.toml" <<'PYEOF'
import sys
p = sys.argv[1]; s = open(p).read()
s = s.replace('version = "0.1.0"\n', 'version = "0.1.0"\nmeta = { a = 1, "b c" = [1, 2], d = { e = "f" } }\nlist = [\n  { x = 1 },  # comment\n  { y = [2, 3] },\n]\nempty = []\nwhen = 1979-05-27 07:32:00Z\nflag = true\n', 1)
open(p, 'w').write(s)
PYEOF
mkdir -p "$TMP/shapes/lib" && cp "$TMP/static/libnladd.a" "$TMP/shapes/lib/"
build_run "$TMP/shapes" shapes
[ "$(cat "$TMP/shapes.brc")" = "0" ] && [ "$(cat "$TMP/shapes.rrc")" = "42" ] \
  && ok "CONTROL: inline tables, arrays of tables, empty arrays, dates and booleans are read and the manifest builds" \
  || no "CONTROL: value-shapes manifest: brc=$(cat "$TMP/shapes.brc"): $(head -3 "$TMP/shapes.build" | tr '\n' ' ')"
# Text trailing a header or a complete value is not TOML; skipping it would hide what it spells.
mkapp "$TMP/trailhdr" ''
printf '[package] [native]\nlibs = ["lost"]\nname = "nlapp"\n' > "$TMP/trailhdr/Concrete.toml"
refused "$TMP/trailhdr" trailhdr "unexpected text in or after a table header" "a [native] header trailing another header is refused"
mkapp "$TMP/trailval" ''
printf '[package]\nname = "nlapp" [native]\nlibs = ["lost"]\n' > "$TMP/trailval/Concrete.toml"
refused "$TMP/trailval" trailval "unexpected text in or after a value" "a [native] header trailing a value is refused"
# A bare carriage return is refused; CRLF line endings are ordinary and still load.
mkapp "$TMP/barecr" ''
printf '[package]\nname = "nlapp"\nversion = "0.1.0"\n[native\r]\nlibs = ["m"]\n' > "$TMP/barecr/Concrete.toml"
refused "$TMP/barecr" barecr "a carriage return outside a CRLF line ending" "a header hiding a bare carriage return ([native<CR>]) is refused"
mkapp "$TMP/crlf" ''
printf '[package]\r\nname = "nlapp"\r\nversion = "0.1.0"\r\n\r\n[native]\r\nlibs = ["nladd"]\r\nlib-dirs = ["lib"]\r\n' > "$TMP/crlf/Concrete.toml"
mkdir -p "$TMP/crlf/lib" && cp "$TMP/static/libnladd.a" "$TMP/crlf/lib/"
build_run "$TMP/crlf" crlf
[ "$(cat "$TMP/crlf.brc")" = "0" ] && [ "$(cat "$TMP/crlf.rrc")" = "42" ] \
  && ok "CONTROL: a CRLF manifest loads and its [native] section links" \
  || no "CONTROL: CRLF manifest: brc=$(cat "$TMP/crlf.brc"): $(head -3 "$TMP/crlf.build" | tr '\n' ' ')"
# The name inside a header's brackets must be a key.
bad hdrcomma '
[native,]
libs = ["m"]' "unexpected text in or after a table header" "a header [native,] is refused"
bad hdreq '
[native = foo]
libs = ["m"]' "unexpected text in or after a table header" "a header [native = foo] is refused"
# The text before '=' must be a key: a header embedded there is refused.
mkapp "$TMP/keyhdr" ''
printf '[package]\nname = "nlapp"\nversion = "0.1.0"\nfoo [native] = ["lost"]\n' > "$TMP/keyhdr/Concrete.toml"
refused "$TMP/keyhdr" keyhdr "not a TOML key before '='" "a header embedded in a key (foo [native] = ...) is refused"
# STRICT ONLY WHERE A NATIVE DECLARATION IS POSSIBLE. A manifest with no "native" text and no
# backslash cannot declare a native library, so its malformed lines keep the existing contract
# (other readers warn, the build proceeds); the same garbage next to a [native] section is refused.
mkapp "$TMP/garbled" ''
printf '[package]\nname = "nlapp"\nversion = "0.1.0"\n\n[dependencies]\ngarbled line without equals\n\n[policy]\nunknown_future_key = true\n' > "$TMP/garbled/Concrete.toml"
( cd "$TMP/garbled" && $TO "$C" check ) > "$TMP/garbled.out" 2>&1; rcg=$?
if grep -q "warning.*Concrete.toml.*policy.*unrecognized" "$TMP/garbled.out" && ! grep -q "Concrete.toml \[native\]" "$TMP/garbled.out"; then
  ok "CONTROL: a manifest that cannot declare native keeps the existing warnings (rc=$rcg; the [native] reader stays out of it)"
else
  no "CONTROL: manifest without native: rc=$rcg $(head -4 "$TMP/garbled.out" | tr '\n' ' ')"
fi
mkapp "$TMP/garblednative" ''
printf '[package]\nname = "nlapp"\nversion = "0.1.0"\n\n[dependencies]\ngarbled line without equals\n\n[native]\nlibs = ["nladd"]\n' > "$TMP/garblednative/Concrete.toml"
refused "$TMP/garblednative" garblednative "neither a table header nor a key = value" "the same garbled line in a manifest WITH a [native] section is refused"
# Keys are matched exactly: "lib s" is not libs.
bad spacedkey '
[native]
"lib s" = ["nladd"]' "unknown key" 'a key spelled "lib s" is refused, not normalised to libs'
bad quotedkey '
[native]
"libs" = ["nladd"]' "unknown key" 'a quoted key "libs" is refused (only the plain spelling is read)'
# Control: an array whose STRING elements say native is ordinary data and builds.
mkapp "$TMP/inarrayok" '
[native]
libs = ["nladd"]
lib-dirs = ["lib"]'
python3 - "$TMP/inarrayok/Concrete.toml" <<'PYEOF'
import sys
p = sys.argv[1]; s = open(p).read()
s = s.replace('version = "0.1.0"\n', 'version = "0.1.0"\nkeywords = [\n  "native",\n  "[native]",\n]\n', 1)
open(p, 'w').write(s)
PYEOF
mkdir -p "$TMP/inarrayok/lib" && cp "$TMP/static/libnladd.a" "$TMP/inarrayok/lib/"
build_run "$TMP/inarrayok" inarrayok
[ "$(cat "$TMP/inarrayok.brc")" = "0" ] && [ "$(cat "$TMP/inarrayok.rrc")" = "42" ] \
  && ok "CONTROL: string elements mentioning native inside a multi-line array are data; the manifest builds" \
  || no "CONTROL: string-element array manifest: brc=$(cat "$TMP/inarrayok.brc"): $(head -3 "$TMP/inarrayok.build" | tr '\n' ' ')"
bad rpathcolon '
[native]
lib-dirs = ["lib:other"]' "is not an accepted library directory" "a ':' in a lib-dir is refused (an ELF rpath list separator)"
bad rpathtoken '
[native]
lib-dirs = ["$ORIGIN/lib"]' "is not an accepted library directory" "a '\$' in a lib-dir is refused (an rpath token such as \$ORIGIN)"
bad unterminated '
notes = """
[native]
libs = ["nladd"]' "unterminated multi-line string" "an unterminated multi-line string is refused"
# -l<name> is not always a library: these spellings are linker options (ld64, clang, GNU ld).
for nm in to_library d_classic d64 azy_framework oad_hidden; do
  bad "lopt_$nm" "
[native]
libs = [\"$nm\"]" "would be read as a linker option" "libs = [\"$nm\"] is refused (-l$nm is a linker option)"
done
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
# An enclosing manifest that exists but cannot be read is a refusal, never a fallback to a
# readable ancestor manifest (or to standalone mode) that lacks its declarations.
if [ "$(id -u)" != "0" ]; then
  mkdir -p "$TMP/anc"
  printf '[package]\nname = "ancestor"\nversion = "0.1.0"\n' > "$TMP/anc/Concrete.toml"
  mkapp "$TMP/anc/inner" '
[native]
libs = ["nladd"]
lib-dirs = ["lib"]'
  mkdir -p "$TMP/anc/inner/lib" && cp "$TMP/static/libnladd.a" "$TMP/anc/inner/lib/"
  chmod 000 "$TMP/anc/inner/Concrete.toml"
  rm -f "$TMP/ancfile.bin"
  ( cd "$TMP/anc/inner" && $TO "$C" src/main.con -o "$TMP/ancfile.bin" ) > "$TMP/ancfile.build" 2>&1; rca2=$?
  [ "$rca2" != "0" ] && [ ! -e "$TMP/ancfile.bin" ] && grep -qF "cannot read" "$TMP/ancfile.build" \
    && ok "single-file mode refuses an unreadable enclosing manifest (no fallback to the ancestor's)" \
    || no "single-file mode with an unreadable manifest: rc=$rca2 $(head -3 "$TMP/ancfile.build" | tr '\n' ' ')"
  ( cd "$TMP/anc/inner" && $TO "$C" build -o "$TMP/ancbuild.bin" ) > "$TMP/ancbuild.build" 2>&1; rca3=$?
  [ "$rca3" != "0" ] && [ ! -e "$TMP/ancbuild.bin" ] \
    && ok "concrete build refuses an unreadable manifest rather than building the ancestor project" \
    || no "concrete build with an unreadable manifest: rc=$rca3 $(head -3 "$TMP/ancbuild.build" | tr '\n' ' ')"
  chmod 644 "$TMP/anc/inner/Concrete.toml"
  # A manifest that is a symlink into a directory without search permission is present, not absent.
  mkdir -p "$TMP/locked"
  cp "$TMP/anc/inner/Concrete.toml" "$TMP/locked/real.toml"
  mv "$TMP/anc/inner/Concrete.toml" "$TMP/anc/inner/Concrete.toml.orig"
  ln -s "$TMP/locked/real.toml" "$TMP/anc/inner/Concrete.toml"
  chmod 000 "$TMP/locked"
  rm -f "$TMP/anclink.bin"
  ( cd "$TMP/anc/inner" && $TO "$C" src/main.con -o "$TMP/anclink.bin" ) > "$TMP/anclink.build" 2>&1; rca4=$?
  [ "$rca4" != "0" ] && [ ! -e "$TMP/anclink.bin" ] && grep -qF "cannot read" "$TMP/anclink.build" \
    && ok "a manifest symlinked into an unsearchable directory is refused, not treated as absent" \
    || no "symlinked inaccessible manifest: rc=$rca4 $(head -3 "$TMP/anclink.build" | tr '\n' ' ')"
  chmod 755 "$TMP/locked"
else
  ok "unreadable enclosing-manifest legs not applicable as root (stated, not skipped silently)"
fi

echo ""
echo "NATIVE-LIBS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
