#!/usr/bin/env bash
# THE RETENTION MANIFEST GIVES EVERY TRACKED FILE EXACTLY ONE GOVERNING ROW (R-0562; P6, P21).
#
# RETENTION.tsv is written before the legacy/ move so that the move, and later deletions under
# legacy/, cannot drop a corpus, a ledger, the site or an archived service by omission. This gate
# holds it to that, through scripts/land/retention.py (the one owner of the rule): the header is
# exact; every row has six non-empty fields and one of the four dispositions; no path is listed
# twice (so a specific row inside a broad one governs unambiguously, by longest prefix); and every
# tracked file of the tree has a governing row. Each leg has a negative control against a mutated
# copy, so the gate cannot pass by checking nothing.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR" || exit 2
[ -f RETENTION.tsv ] || { echo "FATAL: RETENTION.tsv missing" >&2; exit 2; }
RP="scripts/land/retention.py"
TMP="$(mktemp -d)" || exit 2
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
files="$( { git ls-files; echo RETENTION.tsv; } | LC_ALL=C sort -u)"
problems(){ printf '%s\n' "$files" | python3 -I "$RP" check "$1"; }
govern(){ printf '%s\n' "$2" | python3 -I "$RP" govern "$1" | cut -f3; }

echo "=== RETENTION.tsv governs the tree ==="
out="$(problems RETENTION.tsv)"
[ -z "$out" ] && ok "every tracked file ($(printf '%s\n' "$files" | wc -l | tr -d ' ')) has exactly one governing row" \
  || { no "RETENTION.tsv has problems"; echo "$out" | head -20 | sed 's/^/      /'; }
# Precedence: specific rows inside broad ones govern their own paths.
[ "$(govern RETENTION.tsv docs/spec/decisions.md)" = retained ] && [ "$(govern RETENTION.tsv docs/spec/types.md)" = archived ] \
  && [ "$(govern RETENTION.tsv docs/README.md)" = archived ] && [ "$(govern RETENTION.tsv tests/fixtures/fuzz_farm/findings/x)" = migrate ] \
  && ok "the longest listed prefix governs (decisions.md retained inside archived docs/spec; fuzz findings migrate)" \
  || no "precedence: decisions.md=$(govern RETENTION.tsv docs/spec/decisions.md) types.md=$(govern RETENTION.tsv docs/spec/types.md)"

echo "=== negative controls ==="
out="$( { printf '%s\n' "$files"; echo new-top-level-thing; echo docs/new/deep/file.md; } | python3 -I "$RP" check RETENTION.tsv)"
case "$out" in *"no row governs new-top-level-thing"*) ok "an ungoverned path is refused" ;; *) no "an ungoverned path passed" ;; esac
awk 'BEGIN{FS=OFS="\t"} !/^#/ && $1!="path" && $3=="migrate" && !done {$3="keep-forever"; done=1} {print}' RETENTION.tsv > "$TMP/bad-disp.tsv"
out="$(problems "$TMP/bad-disp.tsv")"
case "$out" in *"keep-forever"*) ok "an unknown disposition is refused" ;; *) no "an unknown disposition passed" ;; esac
awk 'BEGIN{FS=OFS="\t"} !/^#/ && $1!="path" && !done {$6=""; done=1} {print}' RETENTION.tsv > "$TMP/bad-crit.tsv"
out="$(problems "$TMP/bad-crit.tsv")"
case "$out" in *"non-empty tab-separated"*) ok "an empty deletion criterion is refused" ;; *) no "an empty deletion criterion passed" ;; esac
awk -F'\t' '$1 != "std"' RETENTION.tsv > "$TMP/no-std.tsv"
if [ "$(wc -l < "$TMP/no-std.tsv")" -lt "$(wc -l < RETENTION.tsv)" ]; then
  out="$(problems "$TMP/no-std.tsv")"
  case "$out" in *"no row governs std/"*) ok "removing the std row leaves std's files ungoverned" ;; *) no "removing the std row went unnoticed" ;; esac
else
  no "the control did not remove the std row (the mutation itself failed)"
fi
{ cat RETENTION.tsv; printf 'docs/bugs\tdup\tretire\tx\tR-0\tB3\topen\n'; } > "$TMP/dup.tsv"
out="$(problems "$TMP/dup.tsv")"
case "$out" in *"docs/bugs is already listed"*) ok "a path listed twice is refused (no ambiguous deletion authority)" ;; *) no "a duplicate listing passed" ;; esac

awk 'BEGIN{FS=OFS="\t"} !/^#/ && $1!="path" && $3=="retained" && !done {$7="resolved"; done=1} {print}' RETENTION.tsv > "$TMP/ret-res.tsv"
out="$(problems "$TMP/ret-res.tsv")"
case "$out" in *"a retained row never resolves"*) ok "a resolved retained row is refused" ;; *) no "a resolved retained row passed" ;; esac
awk 'BEGIN{FS=OFS="\t"} !/^#/ && $1!="path" && !done {$7="maybe"; done=1} {print}' RETENTION.tsv > "$TMP/bad-status.tsv"
out="$(problems "$TMP/bad-status.tsv")"
case "$out" in *"status 'maybe'"*) ok "an unknown status is refused" ;; *) no "an unknown status passed" ;; esac
# After the move: legacy/P is governed as P, and the new root's own paths are governed too.
out="$(printf 'legacy/std/src/lib.con\nlegacy/docs/spec/types.md\nAGENTS.md\ndocs/decisions.md\nscripts/checks/run.sh\n' | python3 -I "$RP" govern RETENTION.tsv | cut -f3 | tr '\n' ' ')"
[ "$out" = "migrate archived retained retained retained " ] && ok "after the move, legacy/ paths and the new root's paths are governed" \
  || no "post-move governance: '$out'"

echo
echo "check_retention_manifest: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
