#!/usr/bin/env bash
# A filed bug requires THREE artifacts, and this gate is what makes that a rule rather
# than a remembered procedure:
#
#   1. docs/bugs/NNN_*.md          — the document
#   2. an entry in audit_bug_corpus.sh — BUG_TEST_MAP (a fixture) or SKIP_BUGS (a
#                                        rationale naming the coverage)
#   3. coverage that actually exists  — the fixture file, or a named gate script
#
# Bug 066 shipped with 1 and 3 but not 2, and main went red on the CI-only trust gate.
# This runs in seconds so the FIRST local push catches it; the trust gate stays in CI
# as the broader backstop.
set -Eeuo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
fatal() { local rc=$?; echo "FATAL: check_bug_filing_complete stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2; exit "$rc"; }
trap fatal ERR
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }

AUDIT="scripts/tests/audit_bug_corpus.sh"
[ -f "$AUDIT" ] || { echo "error: $AUDIT missing" >&2; exit 2; }

docs=0; missing=""
for f in docs/bugs/[0-9][0-9][0-9]_*.md; do
  [ -e "$f" ] || continue
  docs=$((docs+1))
  n="$(basename "$f" | cut -c1-3)"
  # Entry in either map. Both are `[NNN]=` keyed, so one grep covers them.
  if ! grep -qE "^\s*\[$n\]=" "$AUDIT"; then
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
  no "bug documents with NO entry in $AUDIT:$missing (add to BUG_TEST_MAP with a fixture, or SKIP_BUGS with a rationale naming the coverage)"
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
done <<< "$(awk '/declare -A BUG_TEST_MAP/,/^\)/' "$AUDIT" | grep -E '^\s*\[[0-9]+\]=')"
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
done <<< "$(awk '/declare -A SKIP_BUGS/,/^\)/' "$AUDIT" | grep -E '^\s*\[[0-9]+\]=')"
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

echo "BUG-FILING-COMPLETE: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
