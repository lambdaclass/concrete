#!/usr/bin/env bash
# Docs-drift gate (ROADMAP Phase 4 #44).
#
# Both the Phase 3 and Phase 4 audits found the same meta-failure: docs that
# CLAIM something is true today drift behind the code. This gate makes the
# mechanically-checkable part of that a standing invariant so the audits don't
# have to re-derive it: in PRESENT-TENSE docs (claims about current reality), every
# referenced repo artifact must actually exist.
#
# Checks (referential integrity, on the present-tense doc set):
#   1. Every referenced `scripts/tests/*.sh`, `Concrete/*.lean`, `docs/*.md`, and
#      `std/src/*.con` path exists. (Catches a renamed/deleted gate or module, a
#      dead doc cross-link — e.g. citing `check_obligation_discharge_adapters.sh`
#      when the real gate is `check_discharge_adapters.sh`, the exact slip the
#      Phase 3 audit made.)
#   2. Every `--report <kind>` names a real report kind (extracted from the CLI
#      dispatch in Main.lean, so the check tracks the source of truth).
#   3. ROADMAP (with its backlog, docs/project/ROADMAP_BACKLOG.md) has one
#      stable-ID Task sequence, no dangling/duplicate IDs, and
#      no parallel queue/task headings.
#   4. Every ROADMAP task has a phase or a recorded disposition, the
#      reconciliation ledger and queue rows name real tasks in consistent phases,
#      and every archived task body has its roadmap stub
#      (check_roadmap_coverage.sh, with its own negative controls).
#
# DELIBERATELY NOT attempted (these are not mechanically robust and would make the
# gate a false-positive generator — verified during design):
#   - `concrete <subcommand>` existence: "concrete" is the language name AND an
#     English adjective ("a concrete structure", "concrete examples"), so this
#     collides constantly.
#   - ROADMAP command honesty: a roadmap's PURPOSE is to propose future commands
#     (`concrete fmt`, `doc`, `bench`, `lint`, ...); flagging them is wrong.
#   - `examples/...` path existence: docs legitimately reference unbuilt (probe)
#     or historical (renamed) examples.
#   - CHANGELOG `--report` kinds: historical milestone narrative, not a live claim.
#   - Semantic truth of [DONE]/[OPEN] prose: not mechanizable; that is what the
#     phase audits are for. This gate makes "named artifact exists" mechanical so
#     the audits can focus on semantics.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Present-tense docs: assert "what is true now". CHANGELOG is included for the
# path check (it cites gates/modules) but excluded from the --report check (it is
# historical narrative).
PRESENT_DOCS=(
  docs/verification/CLAIMS_TODAY.md
  docs/verification/KNOWN_HOLES.md
  docs/verification/PHASE3_OBLIGATION_CORE_AUDIT.md
  docs/verification/PHASE4_COMPILER_LEDGER_AUDIT.md
  CHANGELOG.md
  # Evidence-surface docs. They make present-tense claims about --report modes,
  # policy keys, gates and trust boundaries, so they drift exactly like the four
  # above. They were outside this gate while carrying that content, which is the
  # same failure this gate exists to catch, one level up.
  docs/verification/CONTRACTS_AND_VCS.md
  docs/verification/SMT_SOUNDNESS.md
  docs/verification/AXIOMS.md
  docs/verification/TRUSTED_COMPUTING_BASE.md
  # Normative architecture for the prover-neutral obligation layer.
  docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md
)
# docs/NOTES/ by GLOB, not by name. These are design notes that make present-tense
# claims about modules and report kinds, and the design in them was re-derived more
# than once precisely because nothing checked them. A glob rather than a list so the
# NEXT note is gated on arrival instead of when someone remembers to add it — the
# failure mode here has always been the unlisted file.
#
# research/ is deliberately NOT included: it holds dated investigation records whose
# value is that they say what was believed at the time. Gating them would force either
# rewriting history or freezing it. That convention is written down in
# research/README.md so the omission reads as a decision, not an oversight.
for f in docs/NOTES/*.md; do [ -e "$f" ] && PRESENT_DOCS+=("$f"); done
REPORT_DOCS=(  # --report claims here describe the current CLI
  docs/verification/CLAIMS_TODAY.md
  docs/verification/KNOWN_HOLES.md
  docs/verification/PHASE3_OBLIGATION_CORE_AUDIT.md
  docs/verification/PHASE4_COMPILER_LEDGER_AUDIT.md
)

REPORTS="$(grep -oE 'reportType == "[a-z-]+"' Main.lean | sed -E 's/.*"([a-z-]+)"/\1/' | sort -u)"

echo "=== 1. present-tense docs reference only real gates / modules / docs / stdlib files ==="
miss=0
for f in "${PRESENT_DOCS[@]}"; do
  [ -f "$f" ] || { no "present-tense doc itself is missing: $f"; miss=$((miss+1)); continue; }
  for p in $(grep -oE '(scripts/tests/[A-Za-z0-9_]+\.sh|Concrete/[A-Za-z0-9_]+\.lean|docs/[A-Za-z0-9_]+\.md|std/src/[A-Za-z0-9_]+\.con)' "$f" | sort -u); do
    [ -e "$p" ] || { no "$f references missing repo path: $p"; miss=$((miss+1)); }
  done
done
[ "$miss" -eq 0 ] && ok "every gate/module/doc/stdlib path in present-tense docs resolves"

echo "=== 2. present-tense docs reference only real --report kinds ==="
bad=0
for f in "${REPORT_DOCS[@]}"; do
  [ -f "$f" ] || continue
  for k in $(grep -oE -- '--report [a-z-]+' "$f" | awk '{print $2}' | sort -u); do
    printf '%s\n' "$REPORTS" | grep -qx "$k" || { no "$f references unknown --report kind: $k"; bad=$((bad+1)); }
  done
done
[ "$bad" -eq 0 ] && ok "every --report kind in present-tense docs is a real CLI report"

echo "=== 3. roadmap has one stable-ID task sequence ==="
if bash scripts/tests/check_roadmap_linear.sh; then
  ok "ROADMAP task IDs are stable, unique, reference-safe, and queue-free"
else
  no "ROADMAP linear-task structure"
fi

echo "=== 4. every roadmap task has a phase or disposition, and the queue agrees ==="
if bash scripts/tests/check_roadmap_coverage.sh; then
  ok "ROADMAP tasks are phased or dispositioned; ledger and queue rows name real tasks"
else
  no "ROADMAP task coverage (check_roadmap_coverage.sh)"
fi

echo ""
echo "DOCS-DRIFT: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
