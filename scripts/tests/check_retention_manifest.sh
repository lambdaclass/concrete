#!/usr/bin/env bash
# THE RETENTION MANIFEST NAMES EVERY ASSET, WITH A DISPOSITION AND A DELETION CRITERION (R-0562).
#
# RETENTION.tsv is written before the legacy/ move so that the move, and later deletions under
# legacy/, cannot drop a corpus, a ledger, the site or an archived service by omission. This gate
# holds the manifest to that: the header is exact, every row has six non-empty fields and one of the
# four dispositions, and every top-level path of the tree, and every first-level path under docs/,
# scripts/ and tests/, is named by some row. A new top-level file therefore fails here until it is
# given a row. Each leg has a negative control run against a mutated copy, so the gate cannot pass
# by checking nothing.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR" || exit 2
[ -f RETENTION.tsv ] || { echo "FATAL: RETENTION.tsv missing" >&2; exit 2; }
TMP="$(mktemp -d)" || exit 2
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Prints one problem per line for manifest $1 against the path inventory on stdin.
problems() {
  python3 -I -c '
import sys
DISP = {"retained", "archived", "migrate", "retire"}
HDR = ["path", "kind", "disposition", "destination", "owner", "deletion_criterion"]
lines = [l.rstrip("\n") for l in open(sys.argv[1]) if not l.startswith("#") and l.strip()]
if not lines or lines[0].split("\t") != HDR:
    print("header is not: " + "\t".join(HDR)); sys.exit(0)
names = set()
for n, l in enumerate(lines[1:], 2):
    f = l.split("\t")
    if len(f) != 6 or any(not x.strip() for x in f):
        print(f"row {n}: needs six non-empty tab-separated fields: {l[:60]}"); continue
    if f[2] not in DISP:
        print(f"row {n}: disposition {f[2]!r} is not one of {sorted(DISP)}")
    names.update(f[0].split())
for p in (l.strip() for l in sys.stdin):
    if p and p not in names and not any(n.endswith("*") and p.startswith(n[:-1]) for n in names):
        print("no row names " + p)' "$1"
}
inventory() { # tree-ish
  local p s
  for p in $(git ls-tree --name-only "$1"); do
    case "$p" in
      docs|scripts|tests) for s in $(git ls-tree --name-only "$1:$p"); do echo "$p/$s"; done ;;
      *) echo "$p" ;;
    esac
  done
}
# The work tree's tracked paths plus RETENTION.tsv itself (it may be new and uncommitted).
inv="$( { git ls-files | awk -F/ '{ if ($1=="docs"||$1=="scripts"||$1=="tests") print $1"/"$2; else print $1 }'; echo RETENTION.tsv; } | sort -u)"

echo "=== RETENTION.tsv covers the tree ==="
out="$(printf '%s\n' "$inv" | problems RETENTION.tsv)"
[ -z "$out" ] && ok "every top-level path and docs/, scripts/, tests/ entry has a valid row" \
  || { no "RETENTION.tsv has problems"; echo "$out" | sed 's/^/      /'; }

echo "=== negative controls ==="
# An asset with no row is reported.
out="$(printf '%s\nnew-top-level-thing\n' "$inv" | problems RETENTION.tsv)"
case "$out" in *"no row names new-top-level-thing"*) ok "an unlisted path is refused" ;; *) no "an unlisted path passed" ;; esac
# A bad disposition is reported.
awk 'BEGIN{FS=OFS="\t"} !/^#/ && NR>1 && !done && $3=="migrate" {$3="keep-forever"; done=1} {print}' RETENTION.tsv > "$TMP/bad-disp.tsv"
out="$(printf '%s\n' "$inv" | problems "$TMP/bad-disp.tsv")"
case "$out" in *"keep-forever"*) ok "an unknown disposition is refused" ;; *) no "an unknown disposition passed" ;; esac
# An empty deletion criterion is reported.
awk 'BEGIN{FS=OFS="\t"} !/^#/ && $1!="path" && !done {$6=""; done=1} {print}' RETENTION.tsv > "$TMP/bad-crit.tsv"
out="$(printf '%s\n' "$inv" | problems "$TMP/bad-crit.tsv")"
case "$out" in *"six non-empty"*) ok "an empty deletion criterion is refused" ;; *) no "an empty deletion criterion passed" ;; esac
# A dropped row uncovers its path.
awk -F'\t' '$1 != "std"' RETENTION.tsv > "$TMP/no-std.tsv"
out="$(printf '%s\n' "$inv" | problems "$TMP/no-std.tsv")"
if [ "$(wc -l < "$TMP/no-std.tsv")" -lt "$(wc -l < RETENTION.tsv)" ]; then
  case "$out" in *"no row names std"*) ok "removing the std row uncovers std" ;; *) no "removing the std row went unnoticed" ;; esac
else
  no "the control did not remove the std row (the mutation itself failed)"
fi

echo
echo "check_retention_manifest: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
