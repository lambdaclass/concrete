#!/usr/bin/env bash
# Bug 065 — the compiler-provided enums and the shadowing rule have ONE owner,
# and it BEHAVES the same for every way a user `Result` can arrive.
#
# Centralising the code is not the property; agreeing on the answer is. Before
# the fix, Elab consulted `imports.enums` and Check did not, so an IMPORTED user
# `Result` was visible to one pass and not the other. A structural check alone
# would have passed the moment the code moved, without testing that.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
[ -x ".lake/build/bin/concrete" ] || { echo "error: build first" >&2; exit 2; }
CC=".lake/build/bin/concrete"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Each case is a way `Result` can (or cannot) reach a module. All four must
# compile: the builtin is present when unshadowed, and a user declaration
# shadows it without either pass objecting.
mk() { printf '%s\n' "$2" > "$TMP/$1.con"; }

mk absent 'mod app {
    fn f(x: Int) -> Int { return x; }
    pub fn main() -> Int { return f(1); }
}'
mk local 'mod app {
    enum Copy Result { Yes, No }
    fn f() -> Result { return Result::Yes; }
    pub fn main() -> Int { return 0; }
}'
mk imported_plain 'mod lib { pub enum Copy Result { Yes, No } }
mod app {
    import lib.{ Result };
    pub fn main() -> Int { return 0; }
}'
mk imported_aliased 'mod lib { pub enum Copy Result { Yes, No } }
mod app {
    import lib.{ Result as Outcome };
    pub fn main() -> Int { return 0; }
}'

for c in absent local imported_plain imported_aliased; do
  if out=$("$CC" "$TMP/$c.con" 2>&1); then
    ok "Result via '$c': both passes agree (compiles)"
  else
    no "Result via '$c': $(printf '%s' "$out" | grep -oE 'E0[0-9]+' | head -1) — the passes disagree about the builtin"
  fi
done

# STRUCTURAL, secondary to the above: exactly one definition of each, and no
# caller may compute the shadowing rule itself.
n_opt=$( { grep -rn "builtinOptionEnum : EnumDef" --include="*.lean" "$ROOT_DIR/Concrete" || true; } | wc -l | tr -d ' ')
n_res=$( { grep -rn "builtinResultEnum : EnumDef" --include="*.lean" "$ROOT_DIR/Concrete" || true; } | wc -l | tr -d ' ')
[ "$n_opt" = "1" ] && [ "$n_res" = "1" ] \
  && ok "one definition of each builtin enum" \
  || no "builtinOptionEnum x$n_opt, builtinResultEnum x$n_res — a second definition can drift"
own=$( { grep -rln "def builtinEnums" --include="*.lean" "$ROOT_DIR/Concrete" || true; } )
[ "$own" = "$ROOT_DIR/Concrete/Resolve/BuiltinEnums.lean" ] \
  && ok "the owner is Resolve/BuiltinEnums (construction policy, not AST)" \
  || no "builtinEnums lives in '$own'"
# A caller must not be able to supply its own Boolean — that is how the inputs
# diverged in the first place.
local_rule=$( { grep -rn "hasUserResult :=" --include="*.lean" "$ROOT_DIR/Concrete" || true; } | wc -l | tr -d ' ')
[ "$local_rule" = "0" ] \
  && ok "no caller computes the shadowing rule itself" \
  || no "$local_rule caller(s) still compute hasUserResult locally"

echo ""
echo "BUILTIN-ENUM-OWNER: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
