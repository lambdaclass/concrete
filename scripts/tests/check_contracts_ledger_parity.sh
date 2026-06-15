#!/usr/bin/env bash
# Contracts-as-ledger-view parity gate (ROADMAP Phase 3 #15 / #18e).
#
# `--report contracts` (`renderContracts`) still walks and re-discharges every
# obligation family on its OWN path, independent of the ObligationCore ledger —
# the last open dual-truth-source in Phase 3. This gate is the interim lock
# before the renderer is refactored to consume the ledger: it answers, on a
# fixture covering every family,
#
#   For every obligation family --report contracts renders, does the ledger hold
#   the SAME obligations with the SAME discharge verdict?
#
# It compares, per (function, family), the MULTISET of normalized discharge
# statuses in --report contracts against the obligation-ledger JSON. A drift —
# a contracts proof the ledger doesn't hold, a ledger violation contracts hides,
# a count mismatch, or a status contracts invents — fails the gate.
#
# Families covered by the probe fixture: requires/vacuity, ensures/postcondition,
# call-site preconditions, loop O1-O5, array bounds, div/mod nonzero, overflow,
# assert, assume. (The ledger's proof-status-only kinds — missing_theorem,
# ineligible_construct, blocked_proof, source_proof_link — are NOT rendered by
# contracts and are excluded from the comparison.)

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER=".lake/build/bin/concrete"
PROBE="${1:-examples/obligation_core_probe/src/main.con}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
[ -f "$PROBE" ] || { echo "error: fixture not found: $PROBE" >&2; exit 2; }

tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
"$COMPILER" "$PROBE" --report contracts          > "$tmp/contracts.txt" 2>/dev/null
"$COMPILER" "$PROBE" --report obligation-ledger --json > "$tmp/ledger.json" 2>/dev/null

python3 - "$tmp/contracts.txt" "$tmp/ledger.json" <<'PY'
import json, re, sys
from collections import Counter
contracts_path, ledger_path = sys.argv[1], sys.argv[2]

PASS=[]; FAIL=[]
def ok(m): PASS.append(m)
def no(m): FAIL.append(m)

# ---- family -> the ledger kinds it renders ----
FAMILY_KINDS = {
  "postcondition": {"postcondition"},
  "callsite":      {"precondition"},
  "assert":        {"assert"},
  "assume":        {"assume"},
  "bounds":        {"array_bounds"},
  "div":           {"div_nonzero"},
  "overflow":      {"no_overflow"},
  "loop":          {"invariant_init","invariant_preservation",
                    "loop_exit_implies_post","variant_nonnegative","variant_decreases"},
}
# ledger kinds contracts does NOT render (proof-status / eligibility surface)
SKIP_KINDS = {"missing_theorem","ineligible_construct","blocked_proof",
              "source_proof_link","spec_drift","proof_fingerprint"}

CANON = {"proved_by_kernel_decision","proved_by_lean","proved_by_lean_replay",
         "solver_trusted","counterexample","assumed","missing","partial",
         "unproven","vacuous","ineligible","runtime_checked","enforced"}

def normalize(text):
    """Map a contracts status line to a canonical token, or None to skip."""
    t = text.strip()
    if "n/a" in t or "planned" in t: return None
    if "checked: divisor is a nonzero" in t: return "proved_by_kernel_decision"
    for tok in ("proved_by_kernel_decision","proved_by_lean_replay","proved_by_lean",
                "solver_trusted","counterexample","partial","ineligible",
                "runtime_checked","enforced"):
        if tok in t: return tok
    if "VIOLATION" in t: return "counterexample"
    if "vacuous" in t: return "vacuous"
    if "assumed_at_entry" in t: return None        # clause-view marker, not a status
    if t.startswith("assumed") or "assumed (trust" in t: return "assumed"
    if "missing" in t: return "missing"
    if "unproven" in t: return "unproven"
    return "__UNKNOWN__"

# ---- parse --report contracts into per-(fn, family) status multisets ----
con = Counter()              # (fn, family) -> Counter of tokens  (stored flat: (fn,family,tok)->n)
con_vacuous = set()          # fns rendered VACUOUS
seen_fns = set()
section=None; fn=None
FN_RE = re.compile(r'^([a-z_][A-Za-z0-9_]*\.[A-Za-z0-9_]+)\b')
with open(contracts_path) as f:
    for line in f:
        s=line.rstrip("\n")
        if s.startswith("=== "):
            h=s.lower()
            if "loop contracts" in h: section="loop"
            elif "call-site" in h: section="callsite"
            elif "assert / assume" in h: section="assertassume"
            elif "array bounds" in h: section="bounds"
            elif "division" in h: section="div"
            elif "integer overflow" in h: section="overflow"
            elif "source contracts" in h: section="source"
            else: section=None
            fn=None
            continue
        m=FN_RE.match(s)
        if m and not s.startswith(" "):
            fn=m.group(1); seen_fns.add(fn); continue
        if fn is None: continue
        if "VACUOUS" in s: con_vacuous.add(fn); continue
        # which family does this status line belong to?
        if "status:" not in s and "step:" not in s:
            # remember the most recent assert/assume sub-kind in the assertassume section
            if section=="assertassume":
                if s.strip().startswith("assume"): con[(fn,"__pending","assume")] += 0; con_last=("assume",)
                elif s.strip().startswith("assert"): con_last=("assert",)
            continue
        tok=normalize(s)
        if tok is None: continue
        if tok=="__UNKNOWN__":
            no("INVENTED VOCABULARY in contracts: %r (fn=%s)" % (s.strip(), fn)); continue
        if section=="source":
            fam="postcondition"            # ensures rows; requires->vacuity handled via VACUOUS set
            if tok=="vacuous": con_vacuous.add(fn); continue
        elif section=="loop": fam="loop"
        elif section=="callsite": fam="callsite"
        elif section=="bounds": fam="bounds"
        elif section=="div": fam="div"
        elif section=="overflow": fam="overflow"
        elif section=="assertassume":
            fam = con_last[0] if 'con_last' in dir() else "assert"
        else: continue
        con[(fn,fam,tok)] += 1

# ---- load the ledger into per-(fn, family) status multisets ----
d=json.load(open(ledger_path))
KIND_FAMILY={k:fam for fam,ks in FAMILY_KINDS.items() for k in ks}
led=Counter(); led_vacuous=set(); led_cx=set(); led_fns=set()
for o in d["obligations"]:
    fn=o["function"]; kind=o["kind"]; st=o["status"]; led_fns.add(fn)
    if st not in CANON: no("ledger status not in vocabulary: %r (%s)" % (st, o["id"]))
    if kind=="vacuity":
        if st=="proved_by_kernel_decision": led_vacuous.add(fn)
        continue
    if st=="counterexample": led_cx.add(fn)
    if kind in SKIP_KINDS: continue
    fam=KIND_FAMILY.get(kind)
    if fam is None: continue
    led[(fn,fam,st)] += 1

# ---- compare ----
# 1. vacuity both directions
for fn in seen_fns|led_fns:
    if (fn in con_vacuous) != (fn in led_vacuous):
        no("vacuity drift for %s: contracts_vacuous=%s ledger_vacuity_proved=%s"
           % (fn, fn in con_vacuous, fn in led_vacuous))
if not FAIL: ok("vacuity agrees both directions across %d functions" % len(seen_fns|led_fns))

# 2+3. per-(fn,family) status multiset parity (catches missing obligations,
#      status mismatch, and contracts-claims-the-ledger-lacks)
keys = set((fn,fam) for (fn,fam,_) in con) | set((fn,fam) for (fn,fam,_) in led)
mismatches=0
for (fn,fam) in sorted(keys):
    cset=Counter({t:n for (f,fa,t),n in con.items() if f==fn and fa==fam})
    lset=Counter({t:n for (f,fa,t),n in led.items() if f==fn and fa==fam})
    if cset!=lset:
        mismatches+=1
        no("%s [%s]: contracts=%s  ledger=%s" % (fn, fam, dict(cset), dict(lset)))
if mismatches==0:
    ncmp=len(keys)
    ok("per-(function,family) status multisets match across %d (fn,family) groups" % ncmp)

# 4. no ledger violation hidden by contracts (safety direction)
hidden=[fn for fn in led_cx if not any(f==fn and t=="counterexample" for (f,_,t) in con)]
if hidden: no("ledger counterexample NOT shown in contracts for: %s" % ", ".join(hidden))
else: ok("every ledger counterexample is surfaced by contracts (%d)" % len(led_cx))

print("\n".join("  ok   "+m for m in PASS))
print("\n".join("  FAIL "+m for m in FAIL))
print("\nCONTRACTS-LEDGER-PARITY: PASS=%d FAIL=%d" % (len(PASS), len(FAIL)))
sys.exit(1 if FAIL else 0)
PY
