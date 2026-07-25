#!/usr/bin/env bash
# ObligationCore ledger gate (ROADMAP Phase 3 #1/#2 — schema + vocabulary).
#
# Pins the v1 obligation ledger: every obligation carries the full schema-v1
# field set; every status and kind comes from the ONE canonical vocabulary; the
# allowed-engines map matches the semantic profile; and the ledger AGREES with
# `--report vcs` (same ids and statuses) — proving it is a VIEW over the same
# discharge, not a parallel recompute. This is the foundation the rest of the
# Phase 3 migration builds on.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
HMAC="examples/hmac_sha256/src/main.con"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
ck(){ local label="$1" file="$2" expr="$3"
  "$COMPILER" "$file" --report obligation-ledger --json 2>/dev/null \
    | python3 -c "import json,sys; d=json.load(sys.stdin); sys.exit(0 if ($expr) else 1)" 2>/dev/null \
    && ok "$label" || no "$label"; }

echo "=== envelope + schema-v1 fields ==="
ck "schema_kind == obligation_ledger" "$HMAC" "d['schema_kind']=='obligation_ledger'"
ck "ledger_schema_version == 1"       "$HMAC" "d['ledger_schema_version']==1"
ck "count matches obligations length" "$HMAC" "d['count']==len(d['obligations'])"
ck "generates obligations"            "$HMAC" "d['count']>0"
FIELDS="'id','kind','function','loc','origin','variables','hypotheses','conclusion','semantic_profile','dependencies','allowed_engines','status','engine','counterexample','replay','policy_impact'"
ck "every obligation has all schema-v1 fields" "$HMAC" \
  "all(all(k in o for k in [$FIELDS]) for o in d['obligations'])"

echo "=== single canonical vocabulary (Phase 3 #2) ==="
# DERIVE both vocabularies from Concrete/Proof/ObligationCore.lean rather than
# restating them here. This gate previously carried its own copy of each list,
# which is the anti-pattern principle 12 names and bug 057 demonstrated: a check
# that restates the thing it checks cannot notice the thing changing. Adding
# `unbound`/`unbound_proof_link` to the Lean source left this copy behind and the
# gate failed in CI for the wrong reason — not "an obligation used a
# non-canonical status" but "the gate's private list is stale".
VOCAB_SRC="Concrete/Proof/ObligationCore.lean"
extract_vocab() { # <defName> -> python set literal
  python3 - "$VOCAB_SRC" "$1" <<'PYX'
import re,sys
src, name = open(sys.argv[1]).read(), sys.argv[2]
m = re.search(r'def\s+' + re.escape(name) + r'\s*:\s*List String\s*:=\s*\[(.*?)\]', src, re.S)
assert m, "could not find " + name + " in " + sys.argv[1]
body = re.sub(r'--[^\n]*', '', m.group(1))          # strip Lean comments
items = re.findall(r'"([^"]+)"', body)
assert items, "no entries parsed from " + name
print("{" + ",".join("'%s'" % i for i in items) + "}")
PYX
}
STATUS_SET="$(extract_vocab statusVocabulary)" || { echo "  FAIL could not derive statusVocabulary"; FAIL=$((FAIL+1)); STATUS_SET="set()"; }
KIND_SET="$(extract_vocab kindVocabulary)"     || { echo "  FAIL could not derive kindVocabulary";   FAIL=$((FAIL+1)); KIND_SET="set()"; }

ck "every status ∈ canonical statusVocabulary (derived from $VOCAB_SRC)" "$HMAC" \
  "set(o['status'] for o in d['obligations']) <= $STATUS_SET"
ck "every kind ∈ canonical kindVocabulary (derived from $VOCAB_SRC)" "$HMAC" \
  "set(o['kind'] for o in d['obligations']) <= $KIND_SET"
ck "allowed_engines come from the tier set" "$HMAC" \
  "all(set(o['allowed_engines']) <= {'constant_fold','omega','bv_decide','smt','lean'} for o in d['obligations'])"
ck "kernel-decided obligations are owned by a kernel engine" "$HMAC" \
  "all(set(o['allowed_engines']) <= {'constant_fold','omega','bv_decide'} for o in d['obligations'] if o['status']=='proved_by_kernel_decision')"

echo "=== the ledger's VC subset is a VIEW over the VC discharge (agrees with --report vcs) ==="
vcs_json="$("$COMPILER" "$HMAC" --report vcs --json 2>/dev/null)"
led_json="$("$COMPILER" "$HMAC" --report obligation-ledger --json 2>/dev/null)"
# The ledger = VC-projected obligations + the proof-link family (#11). Compare the
# VC ids+statuses against the ledger MINUS the #prooflink entries: that subset must
# match exactly (a view, not a recompute); proof-links are an additional family.
vmap="$(printf '%s' "$vcs_json" | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(v['id']+':'+v['status'] for v in d['vcs'])))")"
lmap="$(printf '%s' "$led_json" | python3 -c "import json,sys;d=json.load(sys.stdin);print('|'.join(sorted(o['id']+':'+o['status'] for o in d['obligations'] if not o['id'].endswith('#prooflink'))))")"
[ -n "$vmap" ] && [ "$vmap" = "$lmap" ] && ok "ledger VC subset == VC ids+statuses (a view, not a recompute)" \
  || no "ledger VC subset drifted from the VC view"
# the proof-link family IS present (Phase 3 #11): proof-status now lives in the one ledger.
plinks="$(printf '%s' "$led_json" | python3 -c "import json,sys;d=json.load(sys.stdin);print(len([o for o in d['obligations'] if o['id'].endswith('#prooflink')]))")"
[ "${plinks:-0}" -ge 1 ] && ok "proof-link family present in the ledger (#11)" || no "no proof-link obligations in the ledger"

echo "=== default: ledger carries no external-solver data unless --smt path engaged ==="
ck "no solver_trusted/counterexample by default (no --smt)" "$HMAC" \
  "all(o['status'] not in ('solver_trusted','counterexample') for o in d['obligations'])"

echo ""
echo "OBLIGATION-CORE: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
