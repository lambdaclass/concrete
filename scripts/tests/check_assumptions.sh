#!/usr/bin/env bash
# Assumption-file CI gate.
#
# Walks every example with an assumptions.toml, compiles the example,
# runs the audit reports (caps, alloc, stack-depth, unsafe), and
# asserts the assumption file's declared values match the compiler's
# actual output. Drift fails the gate.
#
# Contract: docs/ASSUMPTION_FILES.md

set -uo pipefail

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
  local required_caps forbidden_caps
  required_caps=$(toml_get "$af" authority required)
  forbidden_caps=$(toml_get "$af" authority forbidden)
  # Pull every (...) cap-set token out of --report caps. The format is
  # "  fn_name : (cap1, cap2)" or "  fn_name : (pure)".
  local used_caps
  used_caps=$(grep -oE ':\s*\([^)]*\)' <<<"$report_caps" | tr -d ':() ' | tr ',' '\n' | sort -u | grep -v '^$' | grep -v '^pure$' || true)

  # Check forbidden ∩ used == ∅
  if [ -n "$used_caps" ] && [ -n "$forbidden_caps" ]; then
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
  if [ -n "$used_caps" ]; then
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
  if [ "$errs" -eq 0 ] || true; then
    if [ -z "$used_caps" ] && [ -z "$required_caps" ]; then
      echo "  ok   authority — no capabilities used or required"
    elif [ -n "$used_caps" ]; then
      echo "  ok   authority — used caps ($used_caps) within required ($required_caps)"
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
echo "ASSUMPTIONS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
