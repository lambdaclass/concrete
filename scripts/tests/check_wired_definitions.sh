#!/usr/bin/env bash
# WIRED DEFINITIONS — a ratchet on evidence-layer code that nothing consumes.
#
# Three separate pieces of this arc shipped DESIGNED, LANDED AND NEVER WIRED, and all three
# were found by accident rather than by any gate:
#
#   * `SubjectCompletenessV2.of` — the dependency axis, derived and never called;
#   * `SubjectQualificationV2`   — the assumption axis, zero consumers, and vacuous even if
#                                  it had had one, because assume() emitted a gap;
#   * `DependencyRoot`           — DepNode, typed edges, fail-closed errors, trust
#                                  propagation: a complete design with no producer.
#
# Each looked finished in review. The type was right, the doc comment was careful, and
# nothing called it. That is a specific failure mode worth an instrument: unused code in a
# normal codebase is dead weight, but here it means a FACT NOBODY IS CHECKING — the axis
# exists, so the subject looks like it has one, and no verdict ever consults it.
#
# WHY A RATCHET AND NOT A PROHIBITION. Same-file-only use is often legitimate: members of a
# mutual recursion are consumed by their own file's entry point (`exprCallees` by
# `bodyCallees`, `patternBytes` by `bodyBytesV2`), and a field type is consumed by
# pattern-matching on the tree that holds it. Those are not defects, and a gate that
# flagged them would be argued with until it was disabled. What is actionable is the
# DIRECTION: this number must not grow. New evidence-layer code arrives with a consumer, or
# it does not arrive.
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
fatal() { local rc=$?; echo "FATAL: check_wired_definitions stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2; exit "$rc"; }
trap fatal ERR

# Measured 2026-08-07. Raise this ONLY with a note saying why the new definition has no
# consumer and when it will get one.
# 23 -> 24 (2026-10-26, bug 281): the identity-use walk is four accumulator-passing members of one
# mutual recursion (`exprFlatUsesInto` ... `armFlatUsesInto`), consumed by their own file's entry
# point (`stmtFlatUses`, then `flatUsesOf`), the same-file use this header calls legitimate; the
# three list-returning members they replace had no other consumer and are deleted.
BASELINE=24

rc=0
python3 - "$BASELINE" <<'PY' || rc=$?
import re, sys, glob
baseline = int(sys.argv[1])

# The R-0004 evidence and dependency layer. Deliberately not all of Concrete/Proof:
# Proof.lean is the Lean-facing proof library, consumed by generated theorems and by
# proofs/ rather than by compiler code, so scanning it produces ~128 false positives and
# an instrument nobody trusts.
TARGETS = ['Concrete/Proof/EvidenceTree.lean', 'Concrete/Proof/EvidenceBuild.lean',
           'Concrete/Proof/IdentityUseBytes.lean', 'Concrete/Proof/BodyScope.lean',
           'Concrete/Proof/DependencyRoot.lean', 'Concrete/Proof/DependencyEdge.lean',
           'Concrete/Proof/DependencyEdges.lean']

# The corpus must include proofs/, tests/ and the gate scripts: a definition consumed only
# by a Lean theorem or only by a gate probe IS wired, and counting it as unwired would make
# the ratchet fire on correct code.
corpus = {}
SELF = 'scripts/tests/check_wired_definitions.sh'
for pat in ['Concrete/**/*.lean', '*.lean', 'proofs/**/*.lean', 'tests/**/*.lean',
            'scripts/**/*.sh']:
    for f in glob.glob(pat, recursive=True):
        # EXCLUDE THIS GATE. Its header names the very definitions it measures
        # (SubjectQualificationV2, DependencyRoot, ...), so including it made the gate
        # report them as consumed BY ITS OWN PROSE — measuring itself and under-counting
        # by exactly the examples it was written to describe.
        if f.replace('\\', '/') == SELF: continue
        try: corpus[f] = open(f, encoding='utf-8', errors='replace').read()
        except OSError: pass

missing = [t for t in TARGETS if t not in corpus]
if missing:
    print(f"  FAIL target file(s) not found: {missing}")
    print("       If the evidence layer moved, re-point TARGETS — do not silently measure less.")
    sys.exit(1)

unwired = []
for f in TARGETS:
    for m in re.finditer(r"^(?:partial )?(?:def|structure|inductive|abbrev) ([A-Za-z_][A-Za-z0-9_']*)",
                         corpus[f], re.M):
        name = m.group(1)
        if any(name in c for g, c in corpus.items() if g != f):
            continue
        unwired.append((f.split('/')[-1], name))

n = len(unwired)
if n > baseline:
    print(f"  FAIL {n} evidence-layer definitions have no consumer outside their own file (baseline {baseline})")
    print()
    for f, name in unwired:
        print(f"       {f}: {name}")
    print()
    print("  A new definition here with no consumer is not dead weight, it is a FACT NOBODY")
    print("  CHECKS: the axis exists, the subject looks like it has one, and no verdict ever")
    print("  consults it. Three pieces shipped this way in R-0004 and every one was found by")
    print("  accident. Wire it, or raise BASELINE with a note saying when it gets a consumer.")
    sys.exit(1)

if n < baseline:
    print(f"  ok   {n} unwired (baseline {baseline}) — LOWER, so tighten BASELINE to {n}")
else:
    print(f"  ok   {n} unwired evidence-layer definitions, at the {baseline} baseline")
PY

if [ "$rc" -eq 0 ]; then
  echo "WIRED-DEFINITIONS: PASS=1 FAIL=0"
else
  echo "WIRED-DEFINITIONS: PASS=0 FAIL=1"
  exit 1
fi
