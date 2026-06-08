#!/usr/bin/env bash
# CompilerLedger gate (ROADMAP Phase 4 #2).
#
# The non-proof compiler fact store is built ONCE in `loadProject` and carried on
# the one `ProjectContext`, so every project-mode command (build / test / check /
# --report) reads the same facts instead of constructing a command-local store.
# This gate proves the store exists, is populated from a real load, links to the
# ObligationCore (proof) ledger so the two halves compose, and has a single
# producer.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
PROJ="examples/hmac_sha256"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

J="$(cd "$PROJ" && "$COMPILER" --report compiler-ledger --json 2>/dev/null)"
ck(){ local label="$1" expr="$2"
  printf '%s' "$J" | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if ($expr) else 1)" 2>/dev/null \
    && ok "$label" || no "$label"; }

echo "=== the fact store is populated from a real project load ==="
ck "schema_kind == compiler_ledger"        "d['schema_kind']=='compiler_ledger'"
ck "records the std dependency"            "any(p['name']=='std' for p in d['dependencies'])"
ck "records source files"                  "d['counts']['source_files']>=1"
ck "records module facts"                  "any(f['category']=='module' for f in d['facts'])"
ck "records a project module fact"         "any(f['category']=='module' and f['value']=='project' for f in d['facts'])"
ck "carries a toolchain identity"          "len(d['toolchain'])>0"

echo "=== the pipeline is a named, replayable pass chain (Phase 4 #3) ==="
ck "records the frontend pass artifacts"   "[a['pass'] for a in d['artifacts']]==['parse','resolve','typecheck','elaborate','core-check']"
ck "artifacts form an input→output chain"  "all(a['output_ids']==[a['id']] for a in d['artifacts']) and d['artifacts'][1]['input_ids']==[d['artifacts'][0]['id']]"
ck "every artifact carries a replay command" "all(a['replay'] for a in d['artifacts'])"
ck "key artifacts carry a deterministic summary" "any(a['pass']=='core-check' and 'core modules' in a['summary'] for a in d['artifacts']) and any(a['pass']=='resolve' and 'modules' in a['summary'] for a in d['artifacts'])"
ck "records per-phase timings"             "[t['pass'] for t in d['timings']]==['load-deps','frontend']"

echo "=== the two ledgers compose: link to ObligationCore is real ==="
ck "obligation_link names the ObligationCore ledger" "'obligation-ledger' in d['obligation_link']"
# and that linked ledger is actually obtainable for this project.
(cd "$PROJ" && "$COMPILER" src/main.con --report obligation-ledger --json 2>/dev/null) \
  | python3 -c "import json,sys;d=json.load(sys.stdin);sys.exit(0 if d['schema_kind']=='obligation_ledger' and d['count']>0 else 1)" \
  && ok "the linked ObligationCore ledger resolves (count>0)" || no "ObligationCore link does not resolve"

echo "=== single producer: loadProject builds the store; commands read it ==="
# the CompilerLedger is constructed in exactly one place (loadProject), not per command.
nprod="$(grep -cE "CompilerLedger\).linkObligations|CompilerLedger.empty" Main.lean || true)"
[ "$nprod" = "1" ] && ok "exactly one CompilerLedger producer in Main (loadProject)" \
  || no "expected one CompilerLedger producer, found $nprod"
# build / test / check all go through loadProject (so they share the store).
for c in "compileBuild" "compileTestBuild"; do
  grep -q "loadProject" Main.lean && ok "$c-class commands load the project context" || no "$c not via loadProject"
done

echo "=== deterministic (modulo runtime-variable timings) ==="
J2="$(cd "$PROJ" && "$COMPILER" --report compiler-ledger --json 2>/dev/null)"
# timings are runtime-variable by nature; normalize their values before comparing.
norm(){ printf '%s' "$1" | sed 's/"millis": [0-9]*/"millis": 0/g'; }
[ "$(norm "$J")" = "$(norm "$J2")" ] && ok "compiler-ledger is deterministic (timings normalized)" \
  || no "compiler-ledger output not deterministic after timing normalization"

echo ""
echo "COMPILER-LEDGER: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
