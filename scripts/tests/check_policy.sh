#!/usr/bin/env bash
# Policy-file CI gate.
#
# Walks every Concrete.toml under examples/ that has a [policy]
# section, compiles the project, runs the relevant reports, and
# asserts every policy is met. Drift fails the gate.
#
# Contract: docs/project/POLICY_FILES.md

set -uo pipefail
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

COMPILER=".lake/build/bin/concrete"
if [ ! -x "$COMPILER" ]; then
  echo "error: compiler not found at $COMPILER. Run 'make build' first." >&2
  exit 2
fi

mapfile -t TOML_FILES < <(find examples -name Concrete.toml | sort)

if [ ${#TOML_FILES[@]} -eq 0 ]; then
  echo "No Concrete.toml files found under examples/. Nothing to check."
  exit 0
fi

PASS=0
FAIL=0
TMP_AF="$(mktemp -d)"; trap 'rm -rf "$TMP_AF"' EXIT

# The program's DECLARED external authority, from structured facts (R-0484: declared capabilities
# are the compiler-enforced, complete list). Prints the used capabilities, space-separated, on
# stdout and any coverage note on stderr; returns 2 when the facts cannot establish an answer, so
# a missing or malformed report is never read as "no capabilities in use".
authority_used() {
  local src="$1" tag="$2" f="$TMP_AF/$2.json" out
  "$COMPILER" "$src" --report diagnostics-json > "$f" 2>/dev/null
  out=$(python3 "$ROOT_DIR/scripts/tests/lib/authority_facts.py" "$f" 2>&1) || { echo "$out" >&2; return 2; }
  python3 - "$out" <<'PY'
import json, sys
d = json.loads(sys.argv[1])
print(" ".join(d["used"]))
note = f"declared by {d['functions']} function(s), {d['empty_declarations']} declaring none"
if d["incomplete"]:
    note += f"; call-graph coverage INCOMPLETE for {len(d['incomplete'])} ({', '.join(d['incomplete'][:3])}...) — declared capabilities are still compiler-enforced, but the foreign assumptions behind them may not all be listed"
if not d["dependencies_analysed"]:
    note += "; dependencies NOT analysed"
print(note, file=sys.stderr)
PY
}

# Read a TOML scalar (string / int / bool) or array out of the
# [policy] section. Returns the value verbatim; arrays come back
# space-separated.
policy_get() {
  local file="$1" key="$2"
  python3 - "$file" "$key" <<'PYEOF'
import re, sys
path, key = sys.argv[1], sys.argv[2]
src = open(path).read()
m = re.search(r'^\[policy\]\s*$', src, re.MULTILINE)
if not m:
    sys.exit(0)
start = m.end()
next_sec = re.search(r'^\[', src[start:], re.MULTILINE)
body = src[start:start + (next_sec.start() if next_sec else len(src) - start)]
kv_re = re.compile(r'^\s*' + re.escape(key) + r'\s*=\s*(.+?)\s*$', re.MULTILINE)
km = kv_re.search(body)
if not km:
    sys.exit(0)
val = re.sub(r'\s+#.*$', '', km.group(1)).strip()
if val.startswith('"') and val.endswith('"'):
    print(val[1:-1])
elif val.startswith('['):
    inner = val.strip('[]').strip()
    if not inner:
        print("")
    else:
        items = [s.strip().strip('"') for s in inner.split(',')]
        print(' '.join(items))
else:
    print(val)
PYEOF
}

has_policy_section() {
  grep -q '^\[policy\]' "$1"
}

# Check one project's policy.
check_project() {
  local toml="$1"
  has_policy_section "$toml" || { echo "  SKIP $toml — no [policy] section"; return; }

  local project_dir
  project_dir=$(dirname "$toml")
  local source="$project_dir/src/main.con"
  if [ ! -f "$source" ]; then
    echo "  SKIP $toml — source not found at $source"
    return
  fi

  local label="${project_dir#examples/}"
  echo "=== $label ==="

  local report_alloc report_caps report_unsafe report_stack
  report_alloc=$("$COMPILER" "$source" --report alloc 2>&1)
  report_caps=$("$COMPILER" "$source" --report caps 2>&1)
  report_unsafe=$("$COMPILER" "$source" --report unsafe 2>&1)
  report_stack=$("$COMPILER" "$source" --report stack-depth 2>&1)

  local errs=0

  # --- predictable ---
  local predictable
  predictable=$(policy_get "$toml" predictable)
  if [ "$predictable" = "true" ]; then
    if ! "$COMPILER" "$source" --check predictable >/dev/null 2>&1; then
      echo "  FAIL predictable=true but --check predictable refused the source"
      errs=$((errs + 1))
    else
      echo "  ok   predictable=true"
    fi
  fi

  # --- no_alloc ---
  local no_alloc
  no_alloc=$(policy_get "$toml" no_alloc)
  if [ "$no_alloc" = "true" ]; then
    if ! grep -q "No allocation activity found" <<<"$report_alloc"; then
      echo "  FAIL no_alloc=true but --report alloc shows activity"
      echo "$report_alloc" | head -5 | sed 's/^/    /'
      errs=$((errs + 1))
    else
      echo "  ok   no_alloc=true"
    fi
  fi

  # --- no_unsafe ---
  local no_unsafe
  no_unsafe=$(policy_get "$toml" no_unsafe)
  if [ "$no_unsafe" = "true" ]; then
    if ! grep -q "No unsafe signatures found" <<<"$report_unsafe"; then
      # Allow trusted-only output to pass no_unsafe (different policy).
      if grep -qE 'unsafe\b' <<<"$report_unsafe"; then
        echo "  FAIL no_unsafe=true but --report unsafe lists signatures"
        errs=$((errs + 1))
      else
        echo "  ok   no_unsafe=true"
      fi
    else
      echo "  ok   no_unsafe=true"
    fi
  fi

  # --- no_trusted ---
  local no_trusted
  no_trusted=$(policy_get "$toml" no_trusted)
  if [ "$no_trusted" = "true" ]; then
    if grep -qiE 'trusted' <<<"$report_unsafe"; then
      echo "  FAIL no_trusted=true but --report unsafe mentions trusted"
      errs=$((errs + 1))
    else
      echo "  ok   no_trusted=true"
    fi
  fi

  # --- no_externs ---
  local no_externs
  no_externs=$(policy_get "$toml" no_externs)
  if [ "$no_externs" = "true" ]; then
    # Check source for `extern` keyword (cheap structural check —
    # would be better with a fact CLI, deferred to Phase 1 D.19).
    if grep -qE '^\s*(trusted\s+)?extern\b' "$source"; then
      echo "  FAIL no_externs=true but source declares extern"
      errs=$((errs + 1))
    else
      echo "  ok   no_externs=true"
    fi
  fi

  # --- max_stack_bytes ---
  local max_stack actual_max
  max_stack=$(policy_get "$toml" max_stack_bytes)
  if [ -n "$max_stack" ]; then
    actual_max=$(grep -oE 'Max stack bound:\s*[0-9]+' <<<"$report_stack" | grep -oE '[0-9]+' | head -1)
    if [ -z "$actual_max" ]; then
      echo "  FAIL max_stack_bytes=$max_stack but stack-depth report has no max bound"
      errs=$((errs + 1))
    elif [ "$actual_max" -gt "$max_stack" ]; then
      echo "  FAIL max_stack_bytes=$max_stack but actual max is $actual_max"
      errs=$((errs + 1))
    else
      echo "  ok   max_stack_bytes=$max_stack (actual=$actual_max)"
    fi
  fi

  # --- forbidden_capabilities / allowed_capabilities ---
  local forbidden allowed used
  forbidden=$(policy_get "$toml" forbidden_capabilities)
  allowed=$(policy_get "$toml" allowed_capabilities)
  # Policy names are validated and the Std alias expanded by the compiler's own definitions. A
  # name the language does not define is an ERROR: `Net` sat in ten policy files, unmatchable.
  local raw_f="$forbidden" raw_a="$allowed" xerr
  if ! forbidden=$(python3 "$ROOT_DIR/scripts/tests/lib/authority_facts.py" --expand "$ROOT_DIR" $raw_f 2>"$TMP_AF/x.err"); then
    echo "  FAIL forbidden_capabilities: $(cat "$TMP_AF/x.err")"; errs=$((errs + 1)); forbidden=""
  fi
  if ! allowed=$(python3 "$ROOT_DIR/scripts/tests/lib/authority_facts.py" --expand "$ROOT_DIR" $raw_a 2>"$TMP_AF/x.err"); then
    echo "  FAIL allowed_capabilities: $(cat "$TMP_AF/x.err")"; errs=$((errs + 1)); allowed=""
  fi
  # From STRUCTURED facts, not report punctuation: the old scrape read only parenthesised tokens
  # while a capability headline prints bare (`fn : Console`), so it never saw a capability and
  # these checks were vacuous. Unavailable facts FAIL the policy rather than read as "none used".
  local au_note au_ok=1
  au_note="$TMP_AF/${label//\//_}.note"
  if ! used=$(authority_used "$source" "${label//\//_}" 2>"$au_note"); then
    echo "  FAIL authority facts unavailable: $(cat "$au_note")"
    errs=$((errs + 1))
    used=""; au_ok=0
  else
    echo "  info authority: [${used}] — $(cat "$au_note")"
  fi

  # With no facts there is nothing to evaluate: the policy has already FAILED above, and an empty
  # `used` must not go on to print "ok — no capability declared".
  if [ "$au_ok" -eq 1 ] && [ -n "$forbidden" ] && [ -n "$used" ]; then
    for cap in $used; do
      for fb in $forbidden; do
        if [ "$cap" = "$fb" ]; then
          echo "  FAIL forbidden_capabilities contains '$cap' but it is in use"
          errs=$((errs + 1))
        fi
      done
    done
  fi

  # allowed_capabilities = [] enforces "pure-only" (no caps allowed).
  if [ "$au_ok" -eq 1 ] && { [ -n "$allowed" ] || policy_get "$toml" allowed_capabilities >/dev/null 2>&1; }; then
    # If allowed_capabilities is declared (even empty), enforce subset.
    if grep -qE '^\s*allowed_capabilities\s*=' "$toml"; then
      local outside=0
      if [ -n "$used" ]; then
        for cap in $used; do
          local found=0
          for a in $allowed; do
            if [ "$cap" = "$a" ]; then found=1; break; fi
          done
          if [ "$found" -eq 0 ]; then
            echo "  FAIL capability '$cap' in use but allowed_capabilities=[$allowed]"
            errs=$((errs + 1)); outside=1
          fi
        done
      fi
      if [ "$outside" -eq 0 ]; then
        if [ -z "$used" ]; then
          echo "  ok   allowed_capabilities=[$allowed] (no capability declared by any function)"
        else
          echo "  ok   allowed_capabilities=[$allowed] (declared [$used] ⊆ allowed)"
        fi
      fi
    fi
  fi

  if [ "$au_ok" -eq 1 ] && [ -n "$forbidden" ] && [ -z "$used" ]; then
    echo "  ok   forbidden_capabilities=[$forbidden] (none in use)"
  fi

  if [ "$errs" -eq 0 ]; then
    PASS=$((PASS + 1))
  else
    FAIL=$((FAIL + 1))
  fi
}

for toml in "${TOML_FILES[@]}"; do
  check_project "$toml"
done

# ---------------------------------------------------------------------------
# CONTROLS: the authority checks must be able to FAIL. Each control is a throwaway project run
# through the same check_project, in a subshell so the example counters above are untouched.
echo "=== controls: authority checks read real capabilities ==="
CTL="$TMP_AF/ctl"
ctl_project() { # name policy-lines source
  local d="$CTL/$1"; mkdir -p "$d/src"
  printf '[package]\nname = "%s"\nversion = "0.1.0"\n\n[policy]\n%s\n' "$1" "$2" > "$d/Concrete.toml"
  printf '%s\n' "$3" > "$d/src/main.con"
  ( check_project "$d/Concrete.toml" ) 2>&1
}
CONSOLE_SRC='extern fn putchar(c: i32) with(Console) -> i32;
fn shout() with(Console, Unsafe) -> i32 { return putchar(65); }
fn main() with(Console, Unsafe) -> i32 { return shout() - 65; }'
PURE_SRC='fn add(a: i32, b: i32) -> i32 { return a + b; }
fn main() -> i32 { return add(1, 2) - 3; }'
GAP_SRC='fn apply(f: fn(i32) -> i32, x: i32) -> i32 { return f(x); }
fn inc(x: i32) -> i32 { return x + 1; }
fn main() -> i32 { return apply(inc, 1) - 2; }'
ctl_expect() { # label expect-regex reject-regex output
  if printf '%s' "$4" | grep -qE "$2" && ! printf '%s' "$4" | grep -qE "$3"; then
    echo "  ok   CONTROL $1"; PASS=$((PASS + 1))
  else
    echo "  FAIL CONTROL $1"; printf '%s\n' "$4" | grep -E 'FAIL|ok|info' | head -4 | sed 's/^/       /'; FAIL=$((FAIL + 1))
  fi
}
out="$(ctl_project forbid_console 'forbidden_capabilities = ["Console"]' "$CONSOLE_SRC")"
ctl_expect "forbidden Console that IS declared fails" "FAIL forbidden_capabilities contains 'Console'" '^$' "$out"
out="$(ctl_project allow_console 'allowed_capabilities = ["Console", "Unsafe"]' "$CONSOLE_SRC")"
ctl_expect "allowed Console passes" "ok   allowed_capabilities=\[Console Unsafe\]" "FAIL" "$out"
out="$(ctl_project allow_std 'allowed_capabilities = ["Std", "Unsafe"]' "$CONSOLE_SRC")"
ctl_expect "Std alias expands to include Console" "ok   allowed_capabilities" "FAIL" "$out"
out="$(ctl_project pure_only 'allowed_capabilities = []' "$PURE_SRC")"
ctl_expect "empty declarations pass allowed=[] and say so" "no capability declared by any function" "FAIL" "$out"
out="$(ctl_project gap 'allowed_capabilities = []' "$GAP_SRC")"
ctl_expect "an indirect call is reported as INCOMPLETE coverage, explicitly" "coverage INCOMPLETE" "FAIL" "$out"
out="$(ctl_project misspelled 'forbidden_capabilities = ["Net"]' "$PURE_SRC")"
ctl_expect "an unknown capability name (Net) fails instead of never matching" "'Net' is not a capability" '^$' "$out"
out="$(ctl_project broken 'allowed_capabilities = []' 'fn main( -> i32 { return 0; }')"
ctl_expect "a program whose facts cannot be produced FAILS, not 'no capabilities'" "FAIL authority facts unavailable" "no capability declared" "$out"

echo ""
echo "POLICY: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
