#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
# Backend-contract gate (ROADMAP Phase 4 #17a; #17b expands the fixture matrix).
#
# `--report backend-contracts` states the guarantees the code generator commits to
# (overflow, division, layout/ABI, panic, optimization, target, runtime, trusted
# boundaries). This gate proves the report is well-formed, honest about which
# clauses are proof-linked, populates this program's trusted boundaries, and —
# critically — that the contract CANNOT DRIFT from what the backend actually emits
# (the report's target must equal the emitted LLVM module header, since both come
# from Concrete.Backend).

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
C="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$C" ] || { echo "error: build first ($C missing)" >&2; exit 2; }
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

F="examples/source_maps/obligation_located.con"   # arithmetic + division
ELF="examples/elf_header/src/main.con"            # trusted fns + externs

echo "=== the report is a well-formed, versioned JSON envelope ==="
J="$("$C" "$F" --report backend-contracts --json 2>/dev/null)"
printf '%s' "$J" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if d['schema_kind']=='backend_contract' and d['schema_version']>=1 else 1)" \
  && ok "schema_kind=backend_contract, versioned" || no "backend-contract JSON envelope malformed"

echo "=== the contract covers the required topics ==="
covered="$(printf '%s' "$J" | python3 -c "
import json,sys
d=json.load(sys.stdin)
topics={c['topic'] for c in d['clauses']}
need={'integer_overflow','division','layout_abi','panic_assert','optimization','target'}
print('yes' if need<=topics else 'no:'+','.join(need-topics))")"
[ "$covered" = "yes" ] && ok "covers overflow/division/layout/panic/optimization/target" || no "missing topics ($covered)"

echo "=== proof-linked clauses are flagged honestly (overflow/division/panic) ==="
pl="$(printf '%s' "$J" | python3 -c "
import json,sys
d=json.load(sys.stdin)
pl={c['topic'] for c in d['clauses'] if c['proof_linked']}
print('yes' if {'integer_overflow','division','panic_assert'}<=pl else 'no')")"
[ "$pl" = "yes" ] && ok "overflow/division/panic marked [proof-linked]" || no "proof-linked clauses not flagged"
# division clause must be honest about the no-runtime-zero-check / UB reality.
printf '%s' "$J" | python3 -c "import json,sys;d=json.load(sys.stdin);c=next(c for c in d['clauses'] if c['topic']=='division');sys.exit(0 if 'zero-check' in c['guarantee'] and 'undefined behavior' in c['guarantee'] else 1)" \
  && ok "division clause is honest about div-by-zero being a proof obligation (UB if undischarged)" \
  || no "division clause hides the div-by-zero reality"

echo "=== program-derived trusted boundaries are reported ==="
TJ="$("$C" "$ELF" --report backend-contracts --json 2>/dev/null)"
printf '%s' "$TJ" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if len(d['trusted_functions'])>=1 and len(d['extern_declarations'])>=1 else 1)" \
  && ok "a program with trusted fns + externs lists both" || no "trusted boundaries not reported"

echo "=== NO DRIFT: the reported target == the emitted LLVM module header ==="
rTriple="$(printf '%s' "$J" | python3 -c "import json,sys;print(json.load(sys.stdin)['target_triple'])")"
rLayout="$(printf '%s' "$J" | python3 -c "import json,sys;print(json.load(sys.stdin)['data_layout'])")"
LL="$("$C" "$F" --emit-llvm 2>/dev/null)"
eTriple="$(printf '%s' "$LL" | sed -n 's/^target triple = "\(.*\)"/\1/p' | head -1)"
eLayout="$(printf '%s' "$LL" | sed -n 's/^target datalayout = "\(.*\)"/\1/p' | head -1)"
[ -n "$rTriple" ] && [ "$rTriple" = "$eTriple" ] && ok "report triple == emitted triple ($rTriple)" \
  || no "target triple drift: report=$rTriple emitted=$eTriple"
[ -n "$rLayout" ] && [ "$rLayout" = "$eLayout" ] && ok "report data layout == emitted data layout" \
  || no "data layout drift: report=$rLayout emitted=$eLayout"

echo "=== human and JSON agree on the target ==="
"$C" "$F" --report backend-contracts 2>/dev/null | grep -qF "$rTriple" \
  && ok "human report shows the same target triple" || no "human/JSON target disagree"

echo "=== fixture matrix: the report is robust + honest across program shapes (#17b) ==="
# Each fixture exercises a different backend-contract surface. The static clauses
# are program-independent, so per-shape we assert the report runs cleanly, is
# well-formed, and stays drift-free (target == emitted) regardless of program shape.
for fx in arithmetic layout assert_path capability; do
  FF="examples/backend_contracts/$fx.con"
  [ -f "$FF" ] || { no "fixture $fx missing"; continue; }
  FJ="$("$C" "$FF" --report backend-contracts --json 2>/dev/null)"
  rt="$(printf '%s' "$FJ" | python3 -c "import json,sys
d=json.load(sys.stdin)
need={'integer_overflow','division','layout_abi','panic_assert','optimization','target'}
ok = d.get('schema_kind')=='backend_contract' and need<={c['topic'] for c in d['clauses']}
print(d['target_triple'] if ok else '')" 2>/dev/null)"
  et="$("$C" "$FF" --emit-llvm 2>/dev/null | sed -n 's/^target triple = "\(.*\)"/\1/p' | head -1)"
  { [ -n "$rt" ] && [ "$rt" = "$et" ]; } \
    && ok "$fx: report well-formed, all topics, target drift-free ($rt)" \
    || no "$fx: report malformed or target drift (report=$rt emitted=$et)"
done

echo "=== honest target surface: one declared target, no target-selection flag yet ==="
# An "unsupported/unknown target" negative is only meaningful if a target can be
# requested. There is no `--target` surface today, so the contract honestly
# advertises exactly ONE target and we assert we are not silently multi-target.
ntargets="$("$C" "$F" --report backend-contracts --json 2>/dev/null | python3 -c "import json,sys;d=json.load(sys.stdin);print(1 if d['target_triple'] and d['data_layout'] else 0)")"
[ "$ntargets" = "1" ] && ok "exactly one declared target/data-layout (single-target backend)" || no "target surface inconsistent"
if grep -qE '"--target"' Main.lean; then
  no "a --target flag exists but the contract still claims a single target (revisit 17b negative)"
else
  ok "no --target selection flag yet → unknown-target negative is NOT-YET (honestly skipped)"
fi

echo ""
echo "BACKEND-CONTRACTS: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
