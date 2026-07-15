#!/usr/bin/env bash
export CONCRETE_ECHO_RESULT=1  # MAIN_EXIT_MODEL stage 1: legacy echoed-result mode until fixtures migrate (stage 2 deletes this)
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
  docs/CLAIMS_TODAY.md
  docs/KNOWN_HOLES.md
  docs/PHASE3_OBLIGATION_CORE_AUDIT.md
  docs/PHASE4_COMPILER_LEDGER_AUDIT.md
  CHANGELOG.md
)
REPORT_DOCS=(  # --report claims here describe the current CLI
  docs/CLAIMS_TODAY.md
  docs/KNOWN_HOLES.md
  docs/PHASE3_OBLIGATION_CORE_AUDIT.md
  docs/PHASE4_COMPILER_LEDGER_AUDIT.md
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

echo ""
echo "DOCS-DRIFT: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
