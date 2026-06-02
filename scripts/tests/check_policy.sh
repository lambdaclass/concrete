#!/usr/bin/env bash
# Policy-file CI gate.
#
# Walks every Concrete.toml under examples/ that has a [policy]
# section, compiles the project, runs the relevant reports, and
# asserts every policy is met. Drift fails the gate.
#
# Contract: docs/POLICY_FILES.md

set -uo pipefail

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
  used=$(grep -oE ':\s*\([^)]*\)' <<<"$report_caps" | tr -d ':() ' | tr ',' '\n' | sort -u | grep -v '^$' | grep -v '^pure$' || true)

  if [ -n "$forbidden" ] && [ -n "$used" ]; then
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
  if [ -n "$allowed" ] || policy_get "$toml" allowed_capabilities >/dev/null 2>&1; then
    # If allowed_capabilities is declared (even empty), enforce subset.
    if grep -qE '^\s*allowed_capabilities\s*=' "$toml"; then
      if [ -n "$used" ]; then
        for cap in $used; do
          local found=0
          for a in $allowed; do
            if [ "$cap" = "$a" ]; then found=1; break; fi
          done
          if [ "$found" -eq 0 ]; then
            echo "  FAIL capability '$cap' in use but allowed_capabilities=[$allowed]"
            errs=$((errs + 1))
          fi
        done
      fi
      if [ -z "$used" ]; then
        echo "  ok   allowed_capabilities=[$allowed] (no caps in use)"
      else
        echo "  ok   allowed_capabilities=[$allowed] (used ⊆ allowed)"
      fi
    fi
  fi

  if [ -n "$forbidden" ] && [ -z "$used" ]; then
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

echo ""
echo "POLICY: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -gt 0 ] && exit 1 || exit 0
