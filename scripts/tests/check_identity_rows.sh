#!/usr/bin/env bash
# IDENTITY ROWS — R-0482's frozen canonical rows, and proof that freezing rows (not counts) matters.
#
# `check_identity_freeze.sh` pins populations. This gate pins ASSOCIATIONS: which subject is attested
# under which table, which model each consumer pairs with which reference, which callee each body edge
# names, which fingerprint each stored link carries (`scripts/tools/identity_rows.sh`). The R-0482
# migration must reproduce these rows or account for each one in its typed old-to-new map.
#
# Three controls run in a scratch copy, so the working tree is never touched:
#   A. swapping two consumers' models keeps every frozen COUNT and must still move the rows — the
#      reason rows exist;
#   B′. an unused std edit must rename NO generated symbol, move no scope and move no row (R-0482:
#      before the separation it renamed 42 symbols in 7 packages);
#   C′/G. declaring an origin in one manifestful fixture must move exactly that scope (sensitivity);
#      consumers left on the old symbols must be REFUSED — a stale selection is never absorbed — and
#      the renames paired by source fixture must return the rows to the freeze.
#
# The RAW baseline (`docs/verification/identity-raw-baseline-pre-r0482.tsv`) keeps the digest bytes
# the normalized rows drop, tied to its commit; this gate checks the two describe one population, and
# does not hold later trees to the raw bytes.
#
# A legitimate change (a proved body, theorem, table or fingerprint moves) regenerates the frozen file
# in the same commit and explains every changed row:
#   bash scripts/tools/identity_rows.sh > docs/verification/identity-rows-pre-r0482.tsv
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
FROZEN="docs/verification/identity-rows-pre-r0482.tsv"
BIN="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$BIN" ] || { echo "error: build first ($BIN missing)" >&2; exit 2; }
[ -f "$FROZEN" ] || { echo "FATAL: $FROZEN missing" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== the frozen rows ==="
if bash scripts/tools/identity_rows.sh > "$TMP/now.tsv" 2> "$TMP/now.err"; then
  ok "the row producer ran without refusal"
else
  no "the row producer refused: $(tail -3 "$TMP/now.err" | tr '\n' ' ')"
fi
for k in subject:80 dependency:40 reference:60 selection:40 link:60 scope:20; do
  kind="${k%%:*}"; floor="${k##*:}"
  n="$(grep -c "^$kind	" "$FROZEN" || true)"
  [ "$n" -ge "$floor" ] && ok "frozen file holds $n $kind rows" \
    || no "frozen file holds only $n $kind rows (floor $floor) — the freeze is vacuous"
done
if diff "$FROZEN" "$TMP/now.tsv" > "$TMP/drift.diff"; then
  ok "every row matches the freeze ($(grep -vc '^#' "$FROZEN") rows)"
else
  no "rows moved ($(grep -c '^[<>]' "$TMP/drift.diff") diff lines). A legitimate change regenerates the freeze in the same commit and explains each row; the R-0482 migration maps each one:"
  head -20 "$TMP/drift.diff" | sed 's/^/         /'
fi

echo ""
echo "=== the raw baseline (digest bytes the migration map is written against) ==="
RAW="docs/verification/identity-raw-baseline-pre-r0482.tsv"
if [ ! -f "$RAW" ]; then
  no "$RAW missing — the exact old digests would be lost"
else
  if grep -qE '^# baseline-commit: [0-9a-f]{40}$' "$RAW"; then
    ok "the raw baseline names its commit ($(awk '/^# baseline-commit:/ {print substr($3, 1, 8)}' "$RAW"))"
  else
    no "the raw baseline does not name a full baseline commit"
  fi
  for k in subject dependency reference selection link; do
    a="$(grep -c "^$k	" "$RAW" || true)"; b="$(grep -c "^$k	" "$FROZEN" || true)"
    # Rows ADDED after the baseline (explained `A` amendments) are in the frozen rows but not the raw.
    add="$(grep -c "^A	$k	" docs/verification/identity-rows-amendments.tsv 2>/dev/null || true)"
    a=$(( a + ${add:-0} ))
    [ "$a" = "$b" ] && ok "raw (plus $add explained additions) and normalized agree on $k rows ($a)" || no "raw has $a $k rows (with $add explained additions), normalized $b"
  done
  # The two files describe ONE population: the normalized rows must be exactly the raw rows' projection.
  # The bijection the projection needs is a property of this BASELINE only; it is not imposed on trees
  # after the migration, which are judged by the typed map against these raw bytes.
  # AMENDED, NOT REWRITTEN. A row legitimately re-frozen after the baseline (the header's rule) is listed
  # in AMEND as its baseline projection and its new value; the baseline's bytes stay tied to their
  # commit. The projection with exactly those replacements must equal the frozen rows — an unlisted
  # change, a listed one that did not happen, or a listed row the baseline never had all fail.
  AMEND="docs/verification/identity-rows-amendments.tsv"
  bash scripts/tools/identity_rows.sh --normalize "$RAW" 2> "$TMP/proj.err" > "$TMP/proj.tsv"
  if python3 - "$TMP/proj.tsv" "$AMEND" "$TMP/amended.tsv" > "$TMP/amend.err" 2>&1 <<'PY'
import sys
proj = open(sys.argv[1]).read().splitlines()
minus, plus = [], []
for line in open(sys.argv[2]).read().splitlines():
    if line.startswith("-\t"): minus.append(line[2:])
    elif line.startswith("+\t"): plus.append(line[2:])
added = [l[2:] for l in open(sys.argv[2]).read().splitlines() if l.startswith("A\t")]
if len(minus) != len(plus): sys.exit("amendments must pair each - row with a + row")
for old, new in zip(minus, plus):
    if proj.count(old) != 1: sys.exit(f"amended row is not exactly once in the baseline projection: {old}")
    proj[proj.index(old)] = new
# An ADDED row (`A`) is a subject the baseline never had — a new fixture, not a migrated subject. It
# must be absent from the baseline and listed once; the frozen rows are sorted (C order), so it is
# merged in sorted.
for row in added:
    if row in proj: sys.exit(f"added row is already in the baseline projection: {row}")
    if added.count(row) != 1: sys.exit(f"added row listed more than once: {row}")
if added:
    # The leading `#` header lines stay first and in order; only the rows are merged, sorted.
    head = [l for l in proj if l.startswith("#")]
    rows = [l for l in proj if not l.startswith("#")]
    proj = head + sorted(rows + added)
open(sys.argv[3], "w").write("\n".join(proj) + ("\n" if proj else ""))
PY
  then
    if cmp -s "$TMP/amended.tsv" "$FROZEN"; then
      ok "the raw baseline projects onto the frozen rows, with $(grep -c '^+' "$AMEND") explained amendment(s) and $(grep -c '^A' "$AMEND") explained addition(s)"
    else
      no "the raw baseline, amended, does not equal the frozen rows: $(diff "$TMP/amended.tsv" "$FROZEN" | grep -c '^[<>]') unexplained diff lines"
    fi
  else
    no "the amendments do not apply to the raw baseline: $(tail -1 "$TMP/amend.err")"
  fi
  if grep -qE '^(subject|reference)	.*	[0-9a-f]{32}	' "$RAW"; then
    ok "the raw baseline retains full 32-hex digests"
  else
    no "the raw baseline carries no full digests — it would not support an exact map"
  fi
fi

# --- scratch copy: tracked + untracked-unignored files, plus the executable ----------------------------
W="$TMP/repo"; mkdir -p "$W/.lake/build"
# ONE tar reads the whole NUL-separated list (-T -). Through xargs, a list longer than xargs'
# command buffer (131072 bytes, GNU) became several concatenated archives; the receiving tar stops
# at the first end-of-archive marker and the next writer died of SIGPIPE.
git ls-files -co --exclude-standard -z | (cd "$ROOT_DIR" && tar cf - --null -T -) | (cd "$W" && tar xf -) \
  || { no "could not copy the repository"; echo "IDENTITY-ROWS: PASS=$PASS FAIL=$FAIL"; exit 1; }
# A COPY of the executable, not a link: the compiler finds std relative to `IO.appPath`, so a linked
# binary would read the ORIGINAL tree's std and the std-edit controls would measure nothing.
# `cp -c` clones on APFS; elsewhere it is a plain copy.
mkdir -p "$W/.lake/build/bin"
cp -c "$BIN" "$W/.lake/build/bin/concrete" 2>/dev/null || cp "$BIN" "$W/.lake/build/bin/concrete"
snapshot(){ (cd "$W" && tar cf "$TMP/pristine.tar" Concrete/Proof/Proof.lean Concrete/Proof/GeneratedAttestations.lean std/src/base64.con proofs); }
restore(){ (cd "$W" && tar xf "$TMP/pristine.tar"); }
snapshot

echo ""
echo "=== control A: a model swap keeps every count and must move the rows ==="
perl -0pi -e 's/AttestedPFnDef\.of checkNonceFn(\s+GeneratedAttestations\.cryptoFns_([0-9a-f]{8})_check_nonce)(.*?)AttestedPFnDef\.of computeTagFn(\s+GeneratedAttestations\.cryptoFns_\2_compute_tag)/AttestedPFnDef.of computeTagFn$1$3AttestedPFnDef.of checkNonceFn$4/s' "$W/Concrete/Proof/Proof.lean"
if cmp -s "$W/Concrete/Proof/Proof.lean" "$ROOT_DIR/Concrete/Proof/Proof.lean"; then
  no "control A could not apply its swap — the anchor moved, so the control is vacuous"
else
  (cd "$W" && bash scripts/tests/check_identity_freeze.sh > "$TMP/a.freeze" 2>&1)
  FREEZE_A=$?
  (cd "$W" && bash scripts/tools/identity_rows.sh > "$TMP/a.tsv" 2>/dev/null)
  MOVED_A="$(diff "$FROZEN" "$TMP/a.tsv" | grep -c '^[<>]' || true)"
  if [ "$FREEZE_A" -eq 0 ] && [ "$MOVED_A" -gt 0 ]; then
    ok "the swap passes every count in IDENTITY-FREEZE yet moves $MOVED_A row lines — rows see what counts cannot"
  elif [ "$FREEZE_A" -ne 0 ]; then
    no "the swap moved a count, so it no longer demonstrates the count-blind case: $(tail -1 "$TMP/a.freeze")"
  else
    no "a model swap moved NO row — the selection rows do not bind the model/reference pairing"
  fi
fi
restore

echo ""
echo "=== control B′: an unused std edit renames nothing (R-0482) ==="
cp "$W/Concrete/Proof/GeneratedAttestations.lean" "$TMP/gen.before"
(cd "$W" && ATTESTATION_JOIN_ROWS_OUT="$TMP/raw.before" bash scripts/gen/attestation_manifest.sh > /dev/null 2>&1)
perl -0pi -e 's/if v == 62 \{ return 43; \}/if v == 62 { return 43; } if v == 63 { return 47; }/' "$W/std/src/base64.con"
if cmp -s "$W/std/src/base64.con" "$ROOT_DIR/std/src/base64.con"; then
  no "B′: the std edit did not apply — the anchor moved, so the control is vacuous"
else
  (cd "$W" && bash scripts/gen/attestation_refs.sh > /dev/null 2>&1)
  (cd "$W" && ATTESTATION_JOIN_ROWS_OUT="$TMP/raw.after" bash scripts/gen/attestation_manifest.sh > /dev/null 2>&1)
  ids(){ grep -oE '^def [A-Za-z0-9_]+' "$1" | awk '{print $2}' | sort; }
  RENAMED="$(comm -23 <(ids "$TMP/gen.before") <(ids "$W/Concrete/Proof/GeneratedAttestations.lean") | grep -c . || true)"
  [ "$RENAMED" -eq 0 ] && ok "B′: the std edit renamed no generated symbol (pre-R-0482 it renamed 42 in 7 packages)" \
    || no "B′: the std edit renamed $RENAMED generated symbols — std content has leaked back into scope"
  scopes(){ awk -F'\t' '$1=="subject" && $3!="-" {print $NF "\t" $3}' "$1" | sort -u; }
  if [ -s "$TMP/raw.before" ] && cmp -s <(scopes "$TMP/raw.before") <(scopes "$TMP/raw.after"); then
    ok "B′: no scope moved ($(scopes "$TMP/raw.before" | wc -l | tr -d ' ') scopes compared)"
  else
    no "B′: a std edit moved a scope digest"
  fi
  if (cd "$W" && bash scripts/tools/identity_rows.sh > "$TMP/b.tsv" 2> "$TMP/b.err") && cmp -s "$FROZEN" "$TMP/b.tsv"; then
    ok "B′: with no consumer edit at all, the rows still match the freeze"
  else
    no "B′: the rows moved or refused after an unused std edit: $(tail -2 "$TMP/b.err" | tr '\n' ' ')"
  fi
fi
restore
cp "$ROOT_DIR/std/src/base64.con" "$W/std/src/base64.con"

echo ""
echo "=== control C′/G: a declared origin moves its scope; stale consumers are refused ==="
# The rename C used to force with a std edit is now forced only by what scope binds: the manifest's
# name and origin. Declaring an origin in `crypto_verify` must rename exactly that package's symbols
# (sensitivity, control G); consumers left on the old symbols must be REFUSED, never absorbed; and the
# renames paired by source fixture must return the rows to the freeze.
TOML="$W/examples/crypto_verify/Concrete.toml"
cp "$TOML" "$TMP/toml.orig"
cp "$W/Concrete/Proof/GeneratedAttestations.lean" "$TMP/gen.before"
(cd "$W" && ATTESTATION_JOIN_ROWS_OUT="$TMP/raw.before" bash scripts/gen/attestation_manifest.sh > /dev/null 2>&1)
perl -0pi -e 's/(\[package\]\nname = "crypto_verify"\n)/$1origin = "example.org\/r0482-control"\n/' "$TOML"
if cmp -s "$TOML" "$TMP/toml.orig"; then
  no "C′: could not declare the origin — the manifest anchor moved, so the control is vacuous"
else
  (cd "$W" && bash scripts/gen/attestation_refs.sh > /dev/null 2>&1)
  (cd "$W" && ATTESTATION_JOIN_ROWS_OUT="$TMP/raw.after" bash scripts/gen/attestation_manifest.sh > /dev/null 2>&1)
  pkgs(){ awk -F'\t' '$1=="subject" && $3!="-" {print $NF "\t" substr($3, 1, 8)}' "$1" | sort -u; }
  join -t$'\t' <(pkgs "$TMP/raw.before") <(pkgs "$TMP/raw.after") \
    | awk -F'\t' '$2 != $3 {print $1 "\t_" $2 "_\t_" $3 "_"}' | sort -u > "$TMP/prefixes.tsv"
  MOVED_SCOPES="$(cut -f1 "$TMP/prefixes.tsv" | sort -u | tr '\n' ' ')"
  if [ "$MOVED_SCOPES" = "examples/crypto_verify/src/main.con " ]; then
    ok "G: declaring an origin moved exactly crypto_verify's scope"
  else
    no "G: declaring crypto_verify's origin moved the scopes of: ${MOVED_SCOPES:-nothing}"
  fi
  if (cd "$W" && bash scripts/tools/identity_rows.sh > /dev/null 2>&1); then
    no "C′: consumers still select the pre-edit symbols, yet the row producer accepted them"
  else
    ok "C′: with consumers left on stale symbols the row producer refuses (it never absorbs them)"
  fi
  while IFS=$'\t' read -r _ op np; do
    grep -rl -- "$op" "$W/Concrete/Proof/Proof.lean" "$W/proofs" 2>/dev/null | xargs perl -pi -e "s/$op/$np/g"
  done < "$TMP/prefixes.tsv"
  if (cd "$W" && bash scripts/tools/identity_rows.sh > "$TMP/c.tsv" 2> "$TMP/c.err") && cmp -s "$FROZEN" "$TMP/c.tsv"; then
    ok "C′: after the renames paired by source fixture, every row matches the freeze"
  else
    no "C′: after the renames the rows still differ or refuse: $(tail -2 "$TMP/c.err" | tr '\n' ' ')"
  fi
fi
cp "$TMP/toml.orig" "$TOML"
restore

echo ""
echo "IDENTITY-ROWS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
