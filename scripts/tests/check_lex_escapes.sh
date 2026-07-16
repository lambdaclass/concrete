#!/usr/bin/env bash
# String/char escape gate.
#
# The lexer must REJECT unknown escapes and unterminated literals instead of
# silently mangling them (the old behavior dropped the backslash: "test\x"
# lexed as "testx" — silent data corruption). Only \n \t \r \0 \\ \" (strings)
# and \n \t \r \0 \\ \' (chars) exist; anything else after '\' is E0001 with
# the offending escape named in the message.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# rejects <label> <file> <needle>: compile must fail with E0001 and the message
# must name the problem (never a downstream "expected X, got Y").
rejects(){ local label="$1" F="$2" needle="$3"
  local OUT; OUT="$("$COMPILER" "$F" --interp 2>&1)"
  if [ $? -ne 0 ] && grep -q <<<"$OUT" "E0001" \
     && grep -qF <<<"$OUT" "$needle"; then
    ok "$label"
  else
    no "$label (got: $(printf '%s' "$OUT" | head -1))"
  fi; }

echo "=== unknown escapes are lex errors, not silent mangling ==="

cat > "$TMPDIR/str_unknown.con" <<'EOF'
mod m {
    fn main() -> Int {
        let s: String = "test\x";
        drop_string(s);
        return 0;
    }
}
EOF
rejects "unknown string escape \\x" "$TMPDIR/str_unknown.con" "unknown string escape '\\x'"

cat > "$TMPDIR/char_unknown.con" <<'EOF'
mod m {
    fn main() -> Int {
        let c: char = '\q';
        return 0;
    }
}
EOF
rejects "unknown char escape \\q" "$TMPDIR/char_unknown.con" "unknown character escape '\\q'"

printf 'mod m {\n    fn main() -> Int {\n        let s: String = "abc' > "$TMPDIR/unterminated.con"
rejects "unterminated string literal" "$TMPDIR/unterminated.con" "unterminated string literal"

printf "mod m {\n    fn main() -> Int {\n        let c: char = 'a" > "$TMPDIR/unterminated_char.con"
rejects "unterminated char literal" "$TMPDIR/unterminated_char.con" "E0001"

echo "=== valid escapes still lex, compile, and print correctly ==="

cat > "$TMPDIR/valid.con" <<'EOF'
mod m {
    pub fn main() with(Console) -> i32 {
        let s: String = "a\tb\n\"c\"\\d\r\0";
        print_string(&s);
        drop_string(s);
        return 0;
    }
}
EOF
if "$COMPILER" "$TMPDIR/valid.con" -o "$TMPDIR/valid.bin" >/dev/null 2>&1; then
  OUT="$("$TMPDIR/valid.bin")"
  # tab, newline, quotes, backslash all survive; \r and \0 are in the bytes.
  if grep -q <<<"$OUT" 'a	b' && grep -qF <<<"$OUT" '"c"\d'; then
    ok "all valid escapes round-trip through compile+run"
  else
    no "valid-escape output wrong (got: $(printf '%q' "$OUT"))"
  fi
else
  no "valid-escape program failed to compile"
fi

echo
echo "check_lex_escapes: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
