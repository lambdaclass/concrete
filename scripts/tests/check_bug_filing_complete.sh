#!/usr/bin/env bash
# A filed bug requires THREE artifacts, and this gate is what makes that a rule rather
# than a remembered procedure:
#
#   1. docs/bugs/NNN_*.md          — the document
#   2. its coverage entry               — since the ledger migration, the `fixtures:` or
#                                        `coverage:` line of the ledger block in the record
#                                        (scripts/gen/bug_ledger.py corpus); before it, an
#                                        entry in audit_bug_corpus.sh's BUG_TEST_MAP (a
#                                        fixture) or SKIP_BUGS (a rationale naming the coverage)
#   3. coverage that actually exists  — the fixture file, or a named gate script
#
# Bug 066 shipped with 1 and 3 but not 2, and main went red on the CI-only trust gate.
# This runs in seconds so the FIRST local push catches it; the trust gate stays in CI
# as the broader backstop.
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
fatal() { local rc=$?; echo "FATAL: check_bug_filing_complete stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2; exit "$rc"; }
trap fatal ERR
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

AUDIT="scripts/tests/audit_bug_corpus.sh"
[ -f "$AUDIT" ] || { echo "error: $AUDIT missing" >&2; exit 2; }
# The records' coverage rows (empty before the ledger migration):
# NNN<TAB>fixtures|coverage<TAB>open|fixed<TAB>value.
if ! LEDGER_ROWS="$(python3 -I scripts/gen/bug_ledger.py corpus)"; then
  echo "error: scripts/gen/bug_ledger.py corpus failed; the bug records' ledger blocks cannot be read" >&2
  exit 2
fi
# Both sources as legacy-shaped lines, so every leg below reads one form: `[NNN]="value"`.
ledger_lines() { # fixtures|coverage
  [ -n "$LEDGER_ROWS" ] || return 0
  # An open bug's coverage line may name no gate yet (its status is the rationale, as an "OPEN" prefix
  # was before the migration); the status column says so, from the record's own Status line.
  awk -F'\t' -v k="$1" '$2 == k { printf "  [%s]=\"%s%s\"\n", $1, ($3 == "open" ? "OPEN (record status) " : ""), $4 }' <<<"$LEDGER_ROWS"
}

docs=0; missing=""
for f in docs/bugs/[0-9][0-9][0-9]_*.md; do
  [ -e "$f" ] || continue
  docs=$((docs+1))
  n="$(basename "$f" | cut -c1-3)"
  # Entry in either map. Both are `[NNN]=` keyed, so one grep covers them.
  if ! grep -qE "^\s*\[$n\]=" "$AUDIT" && ! grep -qE "^$n	" <<<"$LEDGER_ROWS"; then
    missing="$missing $n"
  fi
done

if [ "$docs" -eq 0 ]; then
  no "no bug documents found — the scan pattern is wrong, so every leg below is vacuous"
else
  ok "scanned $docs bug documents"
fi

if [ -z "$missing" ]; then
  ok "every bug document has a corpus entry"
else
  no "bug documents with NO coverage entry:$missing (give the record's ledger block a fixtures: line, or a coverage: line naming the gate that covers it; docs/bugs/README.md, Filing a bug)"
fi

# The reverse direction: an entry naming a fixture whose file does not exist is a map
# that claims coverage it does not have — the same defect pointing the other way.
absent=""
while IFS= read -r line; do
  n="$(sed -E 's/^[[:space:]]*\[([0-9]+)\]=.*/\1/' <<<"$line")"
  val="$(sed -E 's/^[[:space:]]*\[[0-9]+\]="?([^"]*)"?.*/\1/' <<<"$line")"
  case "$val" in *.con*) ;; *) continue ;; esac
  for cand in $val; do
    case "$cand" in *.con) ;; *) continue ;; esac
    [ -e "tests/programs/$cand" ] || [ -e "$cand" ] || absent="$absent $n:$cand"
  done
done <<< "$({ awk '/declare -A BUG_TEST_MAP/,/^\)/' "$AUDIT"; ledger_lines fixtures; } | grep -E '^\s*\[[0-9]+\]=')"
if [ -z "$absent" ]; then
  ok "every fixture named in BUG_TEST_MAP exists on disk"
else
  no "BUG_TEST_MAP names fixtures that do not exist:$absent"
fi

# A SKIP_BUGS rationale must NAME its coverage, or "skip" degrades into "untested".
vague=""
while IFS= read -r line; do
  n="$(sed -E 's/^[[:space:]]*\[([0-9]+)\]=.*/\1/' <<<"$line")"
  if ! grep -qE "\.con|\.sh|OPEN|HALF CLOSED" <<<"$line"; then
    vague="$vague $n"
  fi
done <<< "$({ awk '/declare -A SKIP_BUGS/,/^\)/' "$AUDIT"; ledger_lines coverage; } | grep -E '^\s*\[[0-9]+\]=')"
# RATCHET, not a bar. Six historical entries name a commit or a prose rationale rather
# than a fixture or gate; retrofitting them is separate work and blocking every push on
# it would be the wrong trade. What must not happen is a NEW one, so the count may fall
# and must never rise.
VAGUE_BASELINE=6
n_vague=$(printf '%s' "$vague" | wc -w | tr -d ' ')
if [ "$n_vague" -le "$VAGUE_BASELINE" ]; then
  if [ "$n_vague" -lt "$VAGUE_BASELINE" ]; then
    ok "SKIP_BUGS entries naming no concrete coverage: $n_vague (below baseline $VAGUE_BASELINE — lower it)"
  else
    ok "no NEW SKIP_BUGS entry without concrete coverage ($n_vague at baseline)"
  fi
else
  no "a SKIP_BUGS entry names no fixture, gate or OPEN status ($n_vague > baseline $VAGUE_BASELINE):$vague"
fi

# STRUCTURAL CAUSE AND OWNER (docs/bugs/README.md, "Filing a bug"). From bug 102 on, a record must
# say which design fact let the bug exist and which roadmap task removes it for the whole class:
# a "## Structural cause" and a "## Structural owner" heading, each followed by text. Earlier
# records are classified in docs/bugs/STRUCTURAL_CLASSES.md instead.
STRUCT_FROM=102
structural_missing() { # file -> prints the missing sections, empty when complete
  awk '
    /^## Structural cause[[:space:]]*$/ { sec="cause"; next }
    /^## Structural owner[[:space:]]*$/ { sec="owner"; next }
    /^#/ { sec=""; next }
    sec != "" && NF > 0 { seen[sec]=1 }
    END { if (!seen["cause"]) printf " cause"; if (!seen["owner"]) printf " owner" }
  ' "$1"
}
nostruct=""; nstruct=0
for f in docs/bugs/[0-9][0-9][0-9]_*.md; do
  [ -e "$f" ] || continue
  n="$(basename "$f" | cut -c1-3)"
  [ "$((10#$n))" -ge "$STRUCT_FROM" ] || continue
  nstruct=$((nstruct+1))
  m="$(structural_missing "$f")"
  [ -z "$m" ] || nostruct="$nostruct $n(missing:$m)"
done
if [ "$nstruct" -eq 0 ]; then
  no "no bug record numbered >= $STRUCT_FROM found — the structural-section leg is vacuous"
elif [ -z "$nostruct" ]; then
  ok "all $nstruct bug records numbered >= $STRUCT_FROM state a structural cause and owner"
else
  no "bug records without a non-empty '## Structural cause' / '## Structural owner' section:$nostruct (docs/bugs/README.md, Filing a bug)"
fi
# Controls: the section check must refuse a record missing either section, or with an empty one,
# and accept a complete one.
STMP="$(mktemp -d)"; trap 'rm -rf "$STMP"' EXIT
printf '# Bug 999\n\n## Cause\nx\n\n## Structural owner\nrow 4\n' > "$STMP/no_cause.md"
printf '# Bug 999\n\n## Structural cause\nnames\n\n## Fix\ny\n' > "$STMP/no_owner.md"
printf '# Bug 999\n\n## Structural cause\n\n## Structural owner\nrow 4\n' > "$STMP/empty_cause.md"
printf '# Bug 999\n\n## Structural cause\nnames\n\n## Structural owner\nrow 4\n' > "$STMP/complete.md"
[ "$(structural_missing "$STMP/no_cause.md")" = " cause" ] && ok "control: a record without a structural cause is refused" \
  || no "control: a record without a structural cause was accepted"
[ "$(structural_missing "$STMP/no_owner.md")" = " owner" ] && ok "control: a record without a structural owner is refused" \
  || no "control: a record without a structural owner was accepted"
[ "$(structural_missing "$STMP/empty_cause.md")" = " cause" ] && ok "control: an empty structural cause is refused" \
  || no "control: an empty structural cause was accepted"
[ -z "$(structural_missing "$STMP/complete.md")" ] && ok "control: a complete record is accepted" \
  || no "control: a complete record was refused"

# UNFILLED SCAFFOLD. scripts/gen/new_bug.sh marks every placeholder it writes with FILL-IN(new_bug.sh).
# A placeholder is not a statement: a '## Structural cause' holding only the token passes the section
# leg above, so the token itself is refused wherever the scaffold writes (record, reproducer,
# changelog fragment). The wip/* tier of the pre-push hook does not run this gate, so a draft can be
# backed up; it cannot reach a ci/* candidate or main.
unfilled="$(grep -lF 'FILL-IN(new_bug.sh)' docs/bugs/[0-9][0-9][0-9]_*.md changelog.d/*.md 2>/dev/null || true)"
unfilled="$unfilled$(find tests/regressions -name '*.con' -exec grep -lF 'FILL-IN(new_bug.sh)' {} + 2>/dev/null || true)"
if [ -z "$unfilled" ]; then
  ok "no bug record, reproducer or changelog fragment still holds a new_bug.sh placeholder"
else
  no "unfilled new_bug.sh placeholders (FILL-IN(new_bug.sh)) in: $(printf '%s\n' "$unfilled" | sed '/^$/d' | tr '\n' ' ')"
fi
# (Its negative control is in check_new_bug.sh: a freshly scaffolded bug must fail this gate here.)

echo "BUG-FILING-COMPLETE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
