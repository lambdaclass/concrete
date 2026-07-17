#!/usr/bin/env bash
# Builtin semantics differential gate (audit follow-up: interp and
# EmitBuiltins are hand-written twice; string_char_at drifted 0-vs--1 and
# codepoint-vs-byte before anyone noticed). Signatures are single-sourced
# in Concrete/Resolve/BuiltinSigs.lean; THIS gate single-sources BEHAVIOR
# by detection: for every shared builtin, an edge-heavy fixture runs under
# the interpreter and as a compiled binary (self-printing wrapper), and
# the outputs must be BYTE-EQUAL. A trap must trap on both sides.
# Backend-only builtins (interp PENDING) are pinned as an explicit list so
# additions are deliberate.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
source "$ROOT_DIR/scripts/tests/lib/selfprint.sh"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first" >&2; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# agree <label>: fixture in $TMP/f.con — interp output == wrapped-compiled output
agree(){ local label="$1" I CO
  I="$("$C" "$TMP/f.con" --interp 2>&1)"; local irc=$?
  gate_selfprint_wrap "$TMP/f.con" "$TMP/f.w.con"
  if ! "$C" "$TMP/f.w.con" -o "$TMP/f.bin" >"$TMP/f.err" 2>&1; then
    no "$label (compile failed: $(head -1 "$TMP/f.err"))"; return; fi
  CO="$("$TMP/f.bin" 2>&1)"; local crc=$?
  if [ $irc -eq 0 ] && [ $crc -eq 0 ] && [ "$I" = "$CO" ]; then ok "$label"
  else no "$label (interp rc=$irc vs compiled rc=$crc; diff: $(diff <(printf '%s' "$I") <(printf '%s' "$CO") | head -2 | tr '\n' ' '))"; fi; }

echo "=== string builtins: interp == compiled, edge-heavy ==="

cat > "$TMP/f.con" <<'EOF'
mod m {
    fn main() with(Console) -> Int {
        let e: String = "";
        let a: String = "abc";
        let u: String = "héllo";
        print_int(string_length(&e)); print_char(10);
        print_int(string_length(&a)); print_char(10);
        print_int(string_length(&u)); print_char(10);
        drop_string(e); drop_string(a); drop_string(u);
        return 0;
    }
}
EOF
agree "string_length: empty / ascii / non-ascii (BYTES)"

cat > "$TMP/f.con" <<'EOF'
mod m {
    fn main() with(Console) -> Int {
        let u: String = "héllo";
        print_int(string_char_at(&u, 0)); print_char(10);
        print_int(string_char_at(&u, 1)); print_char(10);
        print_int(string_char_at(&u, 2)); print_char(10);
        print_int(string_char_at(&u, 99)); print_char(10);
        print_int(string_char_at(&u, 0 - 1)); print_char(10);
        drop_string(u);
        return 0;
    }
}
EOF
agree "string_char_at: byte-indexed, -1 on OOB and negative"

cat > "$TMP/f.con" <<'EOF'
mod m {
    fn main() with(Console) -> Int {
        let a: String = "ab";
        let b: String = "";
        let c: String = string_concat(a, b);
        let d: String = "cd";
        let e: String = string_concat(c, d);
        print_string(&e); print_char(10);
        print_int(string_length(&e)); print_char(10);
        drop_string(e);
        return 0;
    }
}
EOF
agree "string_concat: with-empty + chain"

cat > "$TMP/f.con" <<'EOF'
mod m {
    fn main() with(Console) -> Int {
        let mut s: String = "x";
        string_push_char(&mut s, 121);
        string_push_char(&mut s, 122);
        let t: String = "AB";
        string_append(&mut s, &t);
        print_string(&s); print_char(10);
        print_int(string_length(&s)); print_char(10);
        drop_string(s); drop_string(t);
        return 0;
    }
}
EOF
agree "string_push_char + string_append: build-up"

cat > "$TMP/f.con" <<'EOF'
mod m {
    fn main() with(Console) -> Int {
        let s: String = "hello world";
        let a: String = string_slice(&s, 0, 5);
        let b: String = string_slice(&s, 6, 11);
        let c: String = string_slice(&s, 3, 3);
        print_string(&a); print_char(10);
        print_string(&b); print_char(10);
        print_int(string_length(&c)); print_char(10);
        drop_string(s); drop_string(a); drop_string(b); drop_string(c);
        return 0;
    }
}
EOF
agree "string_slice: prefix / suffix / empty-range"

cat > "$TMP/f.con" <<'EOF'
mod m {
    fn main() with(Console) -> Int {
        let h: String = "haystack";
        let n1: String = "hay";
        let n2: String = "stack";
        let n3: String = "z";
        let n4: String = "";
        if string_contains(&h, &n1) { print_int(1); } else { print_int(0); } print_char(10);
        if string_contains(&h, &n2) { print_int(1); } else { print_int(0); } print_char(10);
        if string_contains(&h, &n3) { print_int(1); } else { print_int(0); } print_char(10);
        if string_contains(&h, &n4) { print_int(1); } else { print_int(0); } print_char(10);
        drop_string(h); drop_string(n1); drop_string(n2); drop_string(n3); drop_string(n4);
        return 0;
    }
}
EOF
agree "string_contains: prefix / suffix / absent / empty needle"

cat > "$TMP/f.con" <<'EOF'
mod m {
    fn main() with(Console) -> Int {
        let a: String = "same";
        let b: String = "same";
        let c: String = "Same";
        let d: String = "";
        let e: String = "";
        if string_eq(&a, &b) { print_int(1); } else { print_int(0); } print_char(10);
        if string_eq(&a, &c) { print_int(1); } else { print_int(0); } print_char(10);
        if string_eq(&d, &e) { print_int(1); } else { print_int(0); } print_char(10);
        drop_string(a); drop_string(b); drop_string(c); drop_string(d); drop_string(e);
        return 0;
    }
}
EOF
agree "string_eq: equal / case-differs / both-empty"

cat > "$TMP/f.con" <<'EOF'
mod m {
    fn main() with(Console) -> Int {
        let a: String = "  padded  ";
        let b: String = "none";
        let c: String = "   ";
        let ta: String = string_trim(&a);
        let tb: String = string_trim(&b);
        let tc: String = string_trim(&c);
        print_string(&ta); print_char(10);
        print_string(&tb); print_char(10);
        print_int(string_length(&tc)); print_char(10);
        drop_string(a); drop_string(b); drop_string(c);
        drop_string(ta); drop_string(tb); drop_string(tc);
        return 0;
    }
}
EOF
agree "string_trim: both-sides / none / all-space"

cat > "$TMP/f.con" <<'EOF'
mod m {
    fn main() with(Console) -> Int {
        let a: String = int_to_string(0);
        let b: String = int_to_string(0 - 12345);
        let c: String = int_to_string(9007199254740993);
        print_string(&a); print_char(10);
        print_string(&b); print_char(10);
        print_string(&c); print_char(10);
        drop_string(a); drop_string(b); drop_string(c);
        return 0;
    }
}
EOF
agree "int_to_string: zero / negative / wide"

echo "=== print builtins: bool/char/int rendering identical ==="
cat > "$TMP/f.con" <<'EOF'
mod m {
    fn main() with(Console) -> Int {
        print_bool(true); print_char(10);
        print_bool(false); print_char(10);
        print_char(65); print_char(10);
        print_int(0 - 1); print_char(10);
        return 0;
    }
}
EOF
agree "print_bool/print_char/print_int renderings"

echo "=== inventory pins (drift-by-addition is deliberate, not silent) ==="
grep -oE '\| "[a-z_0-9]+"' Concrete/Interp/Interp.lean | grep -oE '"[a-z_0-9]+"' | tr -d '"' | sort -u > "$TMP/interp.txt"
grep -oE 'name := "[a-z_0-9]+"' Concrete/Backend/EmitBuiltins.lean | grep -oE '"[a-z_0-9]+"' | tr -d '"' | sort -u > "$TMP/backend.txt"
# Backend-only = interp-PENDING: the EXPLICIT list. A new backend builtin
# missing from interp must be added here (deliberately) or implemented.
cat > "$TMP/pending_expected.txt" <<'EOF'
__concrete_check_oom
bool_to_string
clock_gettime
clock_monotonic_ns
float_to_string
putchar
string_append_bool
string_append_int
string_reserve
string_substr
string_to_int
EOF
comm -13 "$TMP/interp.txt" "$TMP/backend.txt" > "$TMP/pending_actual.txt"
if diff -q "$TMP/pending_expected.txt" "$TMP/pending_actual.txt" >/dev/null; then
  ok "interp-PENDING list is exactly the documented 11 (new builtins must land on both sides or extend this pin)"
else
  no "interp-PENDING drift:"; diff "$TMP/pending_expected.txt" "$TMP/pending_actual.txt" | sed 's/^/       /'
fi

echo
echo "BUILTIN-SEMANTICS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
