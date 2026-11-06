#!/usr/bin/env bash
# Every diagnostic names its code (bug 206).
#
# Eighteen Check refusals were built from a bare message (`throwCheckMsg`) and rendered with no
# code, so no rule could be attached to them and the specification's code mapping (check_spec S6)
# could not see them. The fix makes a code-less diagnostic unrepresentable: `Diagnostic.code` has
# no default, so every construction site must name one, and Check has no message-only helper. This
# gate holds the parts the type system cannot:
#   A. `Diagnostic.code` stays a field without a default, and no message-only helper returns;
#   B. no source spells an empty or malformed code (`code := ""`, `code := "X12"`);
#   C. the shared output check (lib/diagnostic_codes.sh), which run_tests.sh applies to every
#      run_err fixture, flags an error line without a code and passes a coded one.
# The run-time half is in run_tests.sh: a run_err fixture whose output has a code-less error fails.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source scripts/tests/lib/diagnostic_codes.sh

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# A. the structure field and the absent helper
field_has_no_default() {  # $1 = path to Diagnostic.lean
  awk '/^structure Diagnostic where/{s=1;next} s && /^[a-z]/{s=0} s' "$1" \
    | grep -E '^\s+code\s+:' | has_match -v ':='
}
no_message_only_helper() {  # $1 = source root
  # Any pass's message-only throw helper (throwCheckMsg, throwElabMsg, throwMsg, ...): train 8
  # found an Elab one (R-0492 step 2) that only the build caught, because this matched two names.
  ! grep -rqE 'def throw[A-Za-z]*Msg\b' "$1"
}
echo "=== A. a diagnostic cannot be built without a code ==="
field_has_no_default Concrete/Report/Diagnostic.lean \
  && ok "Diagnostic.code has no default" || no "Diagnostic.code has a default value (a code-less diagnostic is representable)"
no_message_only_helper Concrete \
  && ok "no message-only refusal helper" || no "a message-only refusal helper (throwCheckMsg or similar) exists"

# B. no empty or malformed code literal
# A Diagnostic literal is a `{ ... }` whose fields include `severity := .<ctor>` (the report-side
# records with string severities are other structures). Its code, when a string literal, must be
# E<4 digits>; a computed code (`e.code`, `importCode pass true`) is left to the types.
bad_code_literals() {  # $1 = source root; prints offending sites
  python3 -I - "$1" <<'PY'
import glob, re, sys

def blocks(s):
    """Every { ... } span, matched while skipping string literals and comments."""
    stack, out, i, n = [], [], 0, len(s)
    while i < n:
        c = s[i]
        if c == '"':
            i += 1
            while i < n and s[i] != '"':
                i += 2 if s[i] == '\\' else 1
        elif s.startswith('--', i):
            i = s.find('\n', i); i = n if i < 0 else i
        elif c == '{': stack.append(i)
        elif c == '}' and stack: out.append((stack.pop(), i))
        i += 1
    return out

for p in sorted(glob.glob(sys.argv[1] + '/**/*.lean', recursive=True)):
    s = open(p, encoding='utf-8').read()
    for a, b in blocks(s):
        body = s[a:b + 1]
        # a Diagnostic record: a `severity := .<ctor>` field of this block (not of a nested one)
        inner = re.sub(r'\{[^{}]*\}', '', body[1:-1])
        if not re.search(r'\bseverity\s*:=\s*\.', inner): continue
        for c in re.findall(r'\bcode\s*:=\s*"([^"]*)"', inner):
            if not re.fullmatch(r'E[0-9]{4}', c):
                print(f'{p}:{s.count(chr(10), 0, a) + 1}: code "{c}"')
    # a helper that lets its caller leave the code out
    for m in re.finditer(r'\(code\s*:\s*String\s*:=\s*"[^"]*"\)', s):
        print(f'{p}:{s.count(chr(10), 0, m.start()) + 1}: a code parameter with a default')
PY
}
echo "=== B. every code literal is E<4 digits> ==="
bad="$(bad_code_literals Concrete)"
[ -z "$bad" ] && ok "no empty or malformed code literal" || { no "malformed code literal(s):"; echo "$bad" | sed 's/^/      /'; }

# Negative controls for A and B, on copies
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
cp Concrete/Report/Diagnostic.lean "$TMP/D.lean"
python3 -I - "$TMP/D.lean" <<'PY'
import re, sys
p = sys.argv[1]; s = open(p).read()
s2 = re.sub(r'(\n\s+code\s+:\s*String)(\s*\n)', r'\1 := ""\2', s, count=1)
assert s2 != s, "control setup: no code field found"
open(p, 'w').write(s2)
PY
if field_has_no_default "$TMP/D.lean"; then no "control A1: a restored default was not detected"; else ok "control A1: a restored default is detected"; fi
mkdir -p "$TMP/src"; printf 'def throwCheckMsg (msg : String) : Nat := 0\n' > "$TMP/src/X.lean"
if no_message_only_helper "$TMP/src"; then no "control A2: a message-only helper was not detected"; else ok "control A2: a message-only helper is detected"; fi
printf 'def throwElabMsg (msg : String) : Nat := 0\n' > "$TMP/src/X.lean"
if no_message_only_helper "$TMP/src"; then no "control A2b: another pass's message-only helper (throwElabMsg) was not detected"; else ok "control A2b: another pass's message-only helper is detected"; fi
printf 'def d := { severity := .error, message := "m", pass := "check", span := none, hint := none, code := "" }\n' > "$TMP/src/X.lean"
[ -n "$(bad_code_literals "$TMP/src")" ] && ok "control B1: an empty code literal is detected" || no "control B1: an empty code literal was not detected"
printf 'def d := { message := "m {x}", severity := .error, pass := "check", span := none, hint := none, code := "E12" }\n' > "$TMP/src/X.lean"
[ -n "$(bad_code_literals "$TMP/src")" ] && ok "control B2: a malformed code in a reordered record is detected" || no "control B2: a malformed code in a reordered record was not detected"
printf 'private def addError (msg : String) (code : String := "") : Nat := 0\n' > "$TMP/src/X.lean"
[ -n "$(bad_code_literals "$TMP/src")" ] && ok "control B3: a defaulted code parameter is detected" || no "control B3: a defaulted code parameter was not detected"

# C. the output check run_tests.sh applies
echo "=== C. the run_err output check ==="
coded='a.con: error[check]: (E0201) undeclared variable '"'"'x'"'"''
codeless='a.con: error[check]: undeclared variable '"'"'x'"'"''
[ -z "$(codeless_error_lines <<<"$coded")" ] && ok "a coded error line passes" || no "a coded error line was flagged"
[ -n "$(codeless_error_lines <<<"$codeless")" ] && ok "control C: a code-less error line is flagged" || no "control C: a code-less error line was not flagged"
quoted='a.con: error[check]: see b.con: error[check]: (E0201) there'
[ -n "$(codeless_error_lines <<<"$quoted")" ] && ok "control C2: a code-less header quoting a coded one is flagged" || no "control C2: a code-less header quoting a coded one passed"
snippet=' 3 |     let s = "error[check]: not a header";'
[ -z "$(codeless_error_lines <<<"$snippet")" ] && ok "a source snippet that spells a header is not read as one" || no "a source snippet was read as a header"
snippet2=' 3 | let s = "a.con: error[check]: missing";'
[ -z "$(codeless_error_lines <<<"$snippet2")" ] && ok "a source snippet quoting a path and header is not read as one" || no "a source snippet quoting a path and header was read as one"
spaced='build dir/my project/a.con:1:1: error[check]: missing code'
[ -n "$(codeless_error_lines <<<"$spaced")" ] && ok "control C3: a code-less header after a path with spaces is flagged" || no "control C3: a path with spaces hid a code-less header"
spacedok='build dir/my project/a.con:1:1: error[check]: (E0201) coded'
[ -z "$(codeless_error_lines <<<"$spacedok")" ] && ok "a coded header after a path with spaces passes" || no "a coded header after a path with spaces was flagged"
grep -q 'codeless_error_lines' scripts/tests/run_tests.sh \
  && ok "run_tests.sh applies the check to run_err output" || no "run_tests.sh does not apply the check"

echo ""
echo "DIAGNOSTIC-CODES-PRESENT: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
