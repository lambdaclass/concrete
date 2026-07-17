#!/usr/bin/env bash
# Module / import / visibility gate (ROADMAP Phase 5 #1).
#
# Locks the module-system semantics so they cannot silently regress as packages
# grow: cross-module imports of public names work; non-`pub` names are not
# importable; unknown modules and circular file-imports produce clean
# diagnostics; and `--report interface` summarizes only the public surface.
# Behavioral (drives the real compiler over small fixtures); see also
# docs/VISIBILITY_AND_MODULE_HYGIENE.md.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
mkproj(){ mkdir -p "$TMP/$1/src"; printf '[package]\nname = "%s"\nversion = "0.1.0"\n' "$1" > "$TMP/$1/Concrete.toml"; }

echo "=== 1. cross-module import of a public name builds and runs ==="
mkproj p1
cat > "$TMP/p1/src/main.con" <<'EOF'
mod lib {
    pub fn pub_fn() -> i64 { return 41; }
    fn priv_fn() -> i64 { return 99; }
}
mod Main {
    import lib.{ pub_fn };
    fn main() -> i64 { return pub_fn() + 1; }
}
EOF
( cd "$TMP/p1" && "$COMPILER" build >/dev/null 2>&1 && ./p1 >/dev/null 2>&1; [ $? -eq 42 ] ) \
  && ok "public cross-module import builds and runs (rc 42)" \
  || no "public cross-module import failed to build/run"

echo "=== 2. importing a non-pub name is rejected (E0111) ==="
out="$(cd "$TMP/p1" && cat > src/main.con <<'EOF'
mod lib { pub fn pub_fn() -> i64 { return 1; } fn priv_fn() -> i64 { return 2; } }
mod Main { import lib.{ pub_fn, priv_fn }; fn main() -> i64 { return pub_fn(); } }
EOF
"$COMPILER" build 2>&1)"
grep -qE <<<"$out" "E0111" && grep -qiE <<<"$out" "priv_fn.*not public|not public.*priv_fn" \
  && ok "import of a private name rejected with E0111" \
  || no "import of a private name not rejected as expected"

echo "=== 3. importing from an unknown module is rejected (E0110) ==="
out="$(cd "$TMP/p1" && cat > src/main.con <<'EOF'
mod Main { import nosuch.{ x }; fn main() -> i64 { return 0; } }
EOF
"$COMPILER" build 2>&1)"
grep -qE <<<"$out" "E0110" \
  && ok "import from unknown module rejected with E0110" \
  || no "unknown-module import not rejected with E0110"

echo "=== 4. circular file-module import produces a clean diagnostic ==="
mkproj p4
printf 'mod aa;\nfn main() -> Int { return 0; }\n' > "$TMP/p4/src/main.con"
printf 'mod bb;\npub fn fa() -> Int { return 1; }\n' > "$TMP/p4/src/aa.con"
printf 'mod aa;\npub fn fb() -> Int { return 2; }\n' > "$TMP/p4/src/bb.con"
out="$(cd "$TMP/p4" && "$COMPILER" build 2>&1)"
grep -qiE <<<"$out" "circular module import" \
  && ok "circular file-module import diagnosed" \
  || no "circular file-module import not diagnosed"

echo "=== 5. --report interface summarizes only the public surface ==="
out="$(cd "$TMP/p1" && cat > src/main.con <<'EOF'
mod lib { pub fn pub_fn() -> i64 { return 1; } fn priv_fn() -> i64 { return 2; } }
mod Main { import lib.{ pub_fn }; fn main() -> i64 { return pub_fn(); } }
EOF
"$COMPILER" src/main.con --report interface 2>&1)"
if grep -qE <<<"$out" "pub_fn" \
   && ! grep -qE <<<"$out" "priv_fn" \
   && grep -qiE <<<"$out" "interface|exports"; then
  ok "interface report lists public exports and hides private names"
else
  no "interface report did not summarize the public surface correctly"
fi

echo ""
echo "MODULE-VISIBILITY: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
