#!/usr/bin/env bash
# Assumption-file CI gate.
#
# Walks every example with an assumptions.toml, compiles the example,
# runs the audit reports (caps, alloc, stack-depth, unsafe), and
# asserts the assumption file's declared values match the compiler's
# actual output. Drift fails the gate.
#
# Contract: docs/verification/ASSUMPTION_FILES.md

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

# Find every assumptions.toml under examples/.
mapfile -t ASSUMPTION_FILES < <(find examples -name assumptions.toml | sort)

if [ ${#ASSUMPTION_FILES[@]} -eq 0 ]; then
  echo "No assumption files found under examples/. Nothing to check."
  exit 0
fi

PASS=0
FAIL=0

# Parse a TOML value out of a flat file. Handles strings, integers,
# and inline arrays of strings. Returns the raw value as a single
# string; arrays come back space-separated.
toml_get() {
  local file="$1" section="$2" key="$3"
  python3 - "$file" "$section" "$key" <<'PYEOF'
import re, sys
path, section, key = sys.argv[1], sys.argv[2], sys.argv[3]
src = open(path).read()
# Find the target section.
sec_re = re.compile(r'^\[' + re.escape(section) + r'\]\s*$', re.MULTILINE)
m = sec_re.search(src)
if not m:
    sys.exit(0)
start = m.end()
next_sec = re.search(r'^\[', src[start:], re.MULTILINE)
body = src[start:start + (next_sec.start() if next_sec else len(src) - start)]
# Find the key. Allow leading whitespace.
kv_re = re.compile(r'^\s*' + re.escape(key) + r'\s*=\s*(.+?)\s*$', re.MULTILINE)
km = kv_re.search(body)
if not km:
    sys.exit(0)
val = km.group(1).strip()
# Strip trailing comments.
val = re.sub(r'\s+#.*$', '', val).strip()
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

# Check one example against its assumption file.
check_example() {
  local af="$1"
  local example_dir
  example_dir=$(dirname "$af")
  local source="$example_dir/src/main.con"
  if [ ! -f "$source" ]; then
    echo "  SKIP $af — source not found at $source"
    return
  fi

  local label="${example_dir#examples/}"
  echo "=== $label ==="

  # Schema version sanity.
  local schema
  schema=$(toml_get "$af" "" schema_version 2>/dev/null || echo "")
  # toml_get expects a section; for top-level use special handling.
  schema=$(grep -E '^schema_version\s*=' "$af" | head -1 | sed -E 's/.*=\s*([0-9]+).*/\1/')
  if [ "$schema" != "1" ]; then
    echo "  FAIL $af — unsupported schema_version='$schema' (expected 1)"
    FAIL=$((FAIL + 1))
    return
  fi

  # Compile once and pull each report.
  local report_alloc report_caps report_unsafe report_stack
  report_alloc=$("$COMPILER" "$source" --report alloc 2>&1)
  report_caps=$("$COMPILER" "$source" --report caps 2>&1)
  report_unsafe=$("$COMPILER" "$source" --report unsafe 2>&1)
  report_stack=$("$COMPILER" "$source" --report stack-depth 2>&1)

  local errs=0

  # --- allocation.heap ---
  local heap_assumed
  heap_assumed=$(toml_get "$af" allocation heap)
  case "$heap_assumed" in
    none)
      if ! grep -q "No allocation activity found" <<<"$report_alloc"; then
        echo "  FAIL allocation.heap='none' but --report alloc shows activity"
        echo "$report_alloc" | head -5 | sed 's/^/    /'
        errs=$((errs + 1))
      else
        echo "  ok   allocation.heap=none"
      fi
      ;;
    bounded|unrestricted)
      echo "  note allocation.heap='$heap_assumed' — no enforcement at v1"
      ;;
    *)
      echo "  FAIL allocation.heap='$heap_assumed' — unknown value"
      errs=$((errs + 1))
      ;;
  esac

  # --- allocation.stack_max_bytes ---
  local stack_budget actual_max
  stack_budget=$(toml_get "$af" allocation stack_max_bytes)
  actual_max=$(grep -oE 'Max stack bound:\s*[0-9]+' <<<"$report_stack" | grep -oE '[0-9]+' | head -1)
  if [ -z "$actual_max" ]; then
    echo "  FAIL stack-depth report did not produce a max bound"
    errs=$((errs + 1))
  elif [ "$actual_max" -gt "$stack_budget" ]; then
    echo "  FAIL allocation.stack_max_bytes=$stack_budget but actual max is $actual_max"
    errs=$((errs + 1))
  else
    echo "  ok   allocation.stack_max_bytes=$stack_budget (actual=$actual_max)"
  fi

  # --- authority.required + authority.forbidden ---
  local errs_before_authority=$errs
  local required_caps forbidden_caps
  required_caps=$(toml_get "$af" authority required)
  forbidden_caps=$(toml_get "$af" authority forbidden)
  # Names validated and the Std alias expanded by the compiler's own definitions; an unknown name
  # (e.g. `Net` for `Network`) is an error, never a silently unmatchable entry.
  local raw_r="$required_caps" raw_fb="$forbidden_caps" xerrf
  xerrf="$(mktemp)"
  if ! required_caps=$(python3 "$ROOT_DIR/scripts/tests/lib/authority_facts.py" --expand "$ROOT_DIR" $raw_r 2>"$xerrf"); then
    echo "  FAIL authority.required: $(cat "$xerrf")"; errs=$((errs + 1)); required_caps=""
  fi
  if ! forbidden_caps=$(python3 "$ROOT_DIR/scripts/tests/lib/authority_facts.py" --expand "$ROOT_DIR" $raw_fb 2>"$xerrf"); then
    echo "  FAIL authority.forbidden: $(cat "$xerrf")"; errs=$((errs + 1)); forbidden_caps=""
  fi
  rm -f "$xerrf"
  # Pull every (...) cap-set token out of --report caps. The format is
  # "  fn_name : (cap1, cap2)" or "  fn_name : (pure)".
  local used_caps
  # From STRUCTURED facts (scripts/tests/lib/authority_facts.py), not report punctuation: the old
  # scrape read only parenthesised tokens while a capability headline prints bare (`fn : Console`),
  # so it never saw a capability and these checks were vacuous. Unavailable facts FAIL rather than
  # read as "no capabilities used".
  local facts_file="$(mktemp)" au_out au_fail=0
  "$COMPILER" "$source" --report diagnostics-json > "$facts_file" 2>/dev/null
  if ! au_out=$(python3 "$ROOT_DIR/scripts/tests/lib/authority_facts.py" "$facts_file" 2>&1); then
    echo "  FAIL authority facts unavailable: $au_out"
    errs=$((errs + 1)); au_fail=1
    used_caps=""
  else
    used_caps=$(python3 -c 'import json,sys; print(" ".join(json.loads(sys.argv[1])["used"]))' "$au_out")
    echo "  info authority: declared [$used_caps]; $(python3 -c 'import json,sys; d=json.loads(sys.argv[1]); print(f"{d["functions"]} function(s), {d["empty_declarations"]} declaring none" + (f"; coverage INCOMPLETE for {len(d["incomplete"])} — declared capabilities stay compiler-enforced, listed foreign assumptions may be partial" if d["incomplete"] else "") + ("" if d["dependencies_analysed"] else "; dependencies NOT analysed"))' "$au_out")"
  fi
  rm -f "$facts_file"

  # Check forbidden ∩ used == ∅
  if [ "$au_fail" -eq 0 ] && [ -n "$used_caps" ] && [ -n "$forbidden_caps" ]; then
    for cap in $used_caps; do
      for fb in $forbidden_caps; do
        if [ "$cap" = "$fb" ]; then
          echo "  FAIL forbidden capability '$cap' is in use"
          errs=$((errs + 1))
        fi
      done
    done
  fi

  # Check used ⊆ required
  if [ "$au_fail" -eq 0 ] && [ -n "$used_caps" ]; then
    for cap in $used_caps; do
      local found=0
      for req in $required_caps; do
        if [ "$cap" = "$req" ]; then found=1; break; fi
      done
      if [ "$found" -eq 0 ]; then
        echo "  FAIL capability '$cap' is used but not in authority.required=[$required_caps]"
        errs=$((errs + 1))
      fi
    done
  fi
  # Reported OK only when no authority check above failed (this used to read `|| true`, so it
  # printed ok next to its own failures).
  if [ "$errs" -eq "$errs_before_authority" ] && [ "$au_fail" -eq 0 ]; then
    if [ -z "$used_caps" ]; then
      echo "  ok   authority — no capability declared by any function (required=[$required_caps])"
    else
      echo "  ok   authority — declared caps ($used_caps) within required ($required_caps), none forbidden"
    fi
  fi

  # --- ffi.externs ---
  local externs_assumed
  externs_assumed=$(toml_get "$af" ffi externs)
  if [ -z "$externs_assumed" ]; then
    # No externs assumed: --report unsafe should say no unsafe signatures.
    if ! grep -q "No unsafe signatures found" <<<"$report_unsafe"; then
      # Could be trusted without being extern; only fail if extern is present.
      if grep -qE 'extern\b' <<<"$report_unsafe"; then
        echo "  FAIL ffi.externs=[] but --report unsafe lists extern signatures"
        errs=$((errs + 1))
      fi
    fi
    echo "  ok   ffi.externs=[]"
  else
    echo "  note ffi.externs=[$externs_assumed] — no detailed enforcement at v1"
  fi

  # --- trusted.functions + trusted.shells ---
  local trusted_fns trusted_shells
  trusted_fns=$(toml_get "$af" trusted functions)
  trusted_shells=$(toml_get "$af" trusted shells)
  if [ -z "$trusted_fns" ] && [ -z "$trusted_shells" ]; then
    if ! grep -q "No unsafe signatures found" <<<"$report_unsafe"; then
      echo "  FAIL trusted.{functions,shells}=[] but --report unsafe lists signatures"
      errs=$((errs + 1))
    else
      echo "  ok   trusted — no trusted boundaries"
    fi
  else
    # Non-empty: declared trusted list must match what --report unsafe lists.
    # The report's "trusted fn <name>" lines name the actual trusted functions.
    local actual_trusted
    actual_trusted=$(grep -oE 'trusted fn [a-zA-Z_][a-zA-Z0-9_]*' <<<"$report_unsafe" \
                     | awk '{print $3}' | sort -u | tr '\n' ' ' | sed 's/ $//')
    local declared_sorted
    declared_sorted=$(echo "$trusted_fns $trusted_shells" | tr ' ' '\n' | grep -v '^$' | sort -u | tr '\n' ' ' | sed 's/ $//')
    if [ "$actual_trusted" = "$declared_sorted" ]; then
      echo "  ok   trusted list matches declared ($actual_trusted)"
    else
      echo "  FAIL trusted list drift"
      echo "    declared: $declared_sorted"
      echo "    actual:   $actual_trusted"
      errs=$((errs + 1))
    fi
  fi

  # --- arithmetic.{overflow,divide_by_zero,shift_oversize} ---
  # The frozen policy (docs/language/ARITHMETIC_POLICY.md): ordinary integer ops are
  # checked and TRAP on overflow / div-zero / over-width shift; only explicit
  # wrapping_*/saturating_* intrinsics do otherwise. An assumption file is the
  # evidence surface the example's bundle is read under — it must not declare
  # semantics the language does not have (audit 2026-07-16: all five files
  # declared overflow="wrapping" months after the language started trapping).
  local report_arith
  report_arith=$("$COMPILER" "$source" --report arithmetic 2>&1) || {
    echo "  FAIL --report arithmetic errored: $(head -1 <<<"$report_arith")"
    errs=$((errs + 1))
  }
  local ovf dbz shf
  ovf=$(toml_get "$af" arithmetic overflow)
  dbz=$(toml_get "$af" arithmetic divide_by_zero)
  shf=$(toml_get "$af" arithmetic shift_oversize)
  [ "$ovf" = "trap" ] && echo "  ok   arithmetic.overflow=trap" || {
    echo "  FAIL arithmetic.overflow='$ovf' — language policy is checked/trapping (only explicit wrapping_* intrinsics wrap)"
    errs=$((errs + 1)); }
  [ "$dbz" = "trap" ] && echo "  ok   arithmetic.divide_by_zero=trap" || {
    echo "  FAIL arithmetic.divide_by_zero='$dbz' — language policy is trap"
    errs=$((errs + 1)); }
  [ "$shf" = "trap" ] && echo "  ok   arithmetic.shift_oversize=trap" || {
    echo "  FAIL arithmetic.shift_oversize='$shf' — language policy is trap (over-width shift aborts)"
    errs=$((errs + 1)); }
  # Cross-check the declared trap semantics against the site classification:
  # no site may be reported without a checked/proved/explicit class.
  if grep -qE 'unchecked|implicit-wrapping' <<<"$report_arith"; then
    echo "  FAIL --report arithmetic shows unchecked/implicit-wrapping sites under declared trap semantics"
    errs=$((errs + 1))
  fi

  if [ "$errs" -eq 0 ]; then
    PASS=$((PASS + 1))
  else
    FAIL=$((FAIL + 1))
  fi
}

for af in "${ASSUMPTION_FILES[@]}"; do
  check_example "$af"
done

echo ""
# CONTROLS: the authority checks must be able to FAIL. Throwaway examples run through the same
# check_example, in a subshell so the counters above are untouched; only authority lines are judged.
echo "=== controls: authority checks read real capabilities ==="
CTLA="$(mktemp -d)"; trap 'rm -rf "$CTLA"' EXIT
ctl_example() { # name authority-lines source
  local d="$CTLA/$1"; mkdir -p "$d/src"
  printf 'schema_version = 1\n\n[authority]\n%s\n' "$2" > "$d/assumptions.toml"
  printf '%s\n' "$3" > "$d/src/main.con"
  ( check_example "$d/assumptions.toml" ) 2>&1
}
actl() { # label expect reject output
  if printf '%s' "$4" | grep -qE "$2" && ! printf '%s' "$4" | grep -qE "$3"; then
    echo "  ok   CONTROL $1"; PASS=$((PASS + 1))
  else
    echo "  FAIL CONTROL $1"; printf '%s\n' "$4" | grep -E 'authority' | head -3 | sed 's/^/       /'; FAIL=$((FAIL + 1))
  fi
}
A_CONSOLE='extern fn putchar(c: i32) with(Console) -> i32;
fn main() with(Console, Unsafe) -> i32 { return putchar(65) - 65; }'
A_PURE='fn main() -> i32 { return 0; }'
out="$(ctl_example forbid 'required = ["Console", "Unsafe"]
forbidden = ["Console"]' "$A_CONSOLE")"
actl "a forbidden Console that IS declared fails" "FAIL forbidden capability 'Console' is in use" "ok   authority" "$out"
out="$(ctl_example allow 'required = ["Console", "Unsafe"]
forbidden = ["File"]' "$A_CONSOLE")"
actl "a required Console passes" "ok   authority — declared caps \(Console Unsafe\)" "FAIL (forbidden capability|capability .* is used but not)" "$out"
out="$(ctl_example undeclared 'required = []
forbidden = []' "$A_CONSOLE")"
actl "a declared Console missing from required fails" "FAIL capability 'Console' is used but not in authority.required" "ok   authority" "$out"
out="$(ctl_example empty 'required = []
forbidden = ["Console"]' "$A_PURE")"
actl "empty declarations pass and say so" "no capability declared by any function" "FAIL (forbidden capability|capability .* is used)" "$out"
out="$(ctl_example misspelled 'required = []
forbidden = ["Net"]' "$A_PURE")"
actl "an unknown capability name (Net) fails" "'Net' is not a capability" '^$' "$out"
out="$(ctl_example broken 'required = []
forbidden = []' 'fn main( -> i32 { return 0; }')"
actl "a program whose facts cannot be produced FAILS, not 'no capabilities'" "FAIL authority facts unavailable" "ok   authority" "$out"

echo "ASSUMPTIONS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
