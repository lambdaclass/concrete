#!/usr/bin/env bash
# THE GENERATED REFERENCE FILE MUST BE A FUNCTION OF MANIFEST CONTENT, NOT MANIFEST ORDER.
#
# `check_attestation_manifest.sh` compares `Concrete/Proof/GeneratedAttestations.lean`
# byte for byte against a fresh derivation. That comparison is only meaningful if the
# generator emits the same bytes for the same facts on every machine. It did not: the
# emitter inherited the manifest's enumeration order, one machine ordered
# `block_to_words_at` before `block_to_words`, and the freshness gate reported a
# CORRECT tree as STALE while advising a regeneration that would have committed that
# machine's order and broken CI. A gate that tells you to break the build is worse than
# no gate, because it is obeyed.
#
# `scripts/gen/attestation_refs.sh` now sorts under `LC_ALL=C` BEFORE the dedupe. Before
# matters: `awk` keeps the first row per key, so unsorted input decides WHICH row
# survives, not merely where it lands. Sorting after the dedupe would leave that
# selection environment-dependent while making the output look canonical.
#
# THIS GATE ASSERTS THE PROPERTY, NOT THE CODE. Grepping the generator for the word
# `sort` would pass on a sort in the wrong place and fail on a correct rewrite. So it
# runs the REAL generator over two permutations of one manifest and requires identical
# bytes — and carries a control proving the comparison can fail, because a test whose
# negative case never runs is a test that passes vacuously.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

GEN="scripts/gen/attestation_refs.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== one manifest, two orders, one output ==="

if ! bash scripts/gen/attestation_manifest.sh > "$TMP/man.original" 2>/dev/null; then
  echo "FATAL: the manifest refused; cannot test the emitter against it" >&2
  exit 2
fi

lines="$(wc -l < "$TMP/man.original")"
if [ "$lines" -lt 2 ]; then
  echo "FATAL: manifest has $lines line(s); a permutation test needs at least two" >&2
  exit 2
fi
ok "manifest produced $lines lines to permute"

# Two DETERMINISTIC permutations. Not `sort -R`: a gate that permutes differently on
# each run reports a different experiment each time, and a failure could not be replayed.
tac "$TMP/man.original" > "$TMP/man.reversed"
# Rotate by one: distinct from both the original and the reversal for any n > 2.
{ tail -n +2 "$TMP/man.original"; head -n 1 "$TMP/man.original"; } > "$TMP/man.rotated"

if cmp -s "$TMP/man.original" "$TMP/man.reversed"; then
  no "the reversal equals the original, so this permutation tests nothing"
else
  ok "the reversed manifest genuinely differs from the original"
fi

emit() { # emit <manifest-file> <output-file> ; runs the real generator
  ATTESTATION_MANIFEST_FILE="$1" ATTESTATION_REFS_OUT="$2" bash "$GEN" >/dev/null 2>&1
}

emit "$TMP/man.original" "$TMP/out.original" || no "generator failed on the original order"
emit "$TMP/man.reversed" "$TMP/out.reversed" || no "generator failed on the reversed order"
emit "$TMP/man.rotated"  "$TMP/out.rotated"  || no "generator failed on the rotated order"

if cmp -s "$TMP/out.original" "$TMP/out.reversed"; then
  ok "reversed manifest emits byte-identical references"
else
  no "reversed manifest emits DIFFERENT references — the emitter depends on input order"
  diff "$TMP/out.original" "$TMP/out.reversed" | head -8 | sed 's/^/       /'
fi

if cmp -s "$TMP/out.original" "$TMP/out.rotated"; then
  ok "rotated manifest emits byte-identical references"
else
  no "rotated manifest emits DIFFERENT references — the emitter depends on input order"
  diff "$TMP/out.original" "$TMP/out.rotated" | head -8 | sed 's/^/       /'
fi

# The emitter must still be doing its job, not emitting nothing identically.
refs="$(grep -c '^def ' "$TMP/out.original" 2>/dev/null || echo 0)"
if [ "$refs" -gt 0 ]; then
  ok "the compared output is non-empty ($refs references)"
else
  no "the compared output has no references; identical emptiness proves nothing"
fi

echo "=== CONTROL: the comparison can fail ==="

# Strip the canonical sort from a COPY and confirm the permutation is then detected.
# Without this, every assertion above would also pass on a generator that had silently
# stopped sorting but happened to receive pre-ordered input.
# Literal match, and asserted to hit exactly one line: a control built by a pattern
# that silently matched nothing (or matched a comment) would "remove the sort" while
# changing nothing, and then report that the gate discriminates when it does not.
sortlines="$(grep -cF '| LC_ALL=C sort' "$GEN")"
if [ "$sortlines" -ne 1 ]; then
  no "CONTROL: expected exactly one canonical-sort line in the generator, found $sortlines"
fi
grep -vF '| LC_ALL=C sort' "$GEN" > "$TMP/unsorted.sh"
if cmp -s "$GEN" "$TMP/unsorted.sh"; then
  no "CONTROL could not remove the sort line; the control is not exercising anything"
else
  ok "CONTROL: built an unsorted variant of the generator"
  cp "$TMP/unsorted.sh" "scripts/gen/.attestation_refs_control.sh"
  ATTESTATION_MANIFEST_FILE="$TMP/man.original" ATTESTATION_REFS_OUT="$TMP/ctl.original" \
    bash "scripts/gen/.attestation_refs_control.sh" >/dev/null 2>&1
  ATTESTATION_MANIFEST_FILE="$TMP/man.reversed" ATTESTATION_REFS_OUT="$TMP/ctl.reversed" \
    bash "scripts/gen/.attestation_refs_control.sh" >/dev/null 2>&1
  rm -f "scripts/gen/.attestation_refs_control.sh"
  if [ -s "$TMP/ctl.original" ] && [ -s "$TMP/ctl.reversed" ]; then
    if cmp -s "$TMP/ctl.original" "$TMP/ctl.reversed"; then
      no "CONTROL: the unsorted generator was order-INSENSITIVE, so this gate cannot detect the defect it exists for"
    else
      ok "CONTROL: the unsorted generator IS order-sensitive, so the checks above discriminate"
    fi
  else
    no "CONTROL: the unsorted variant produced no output; the control did not run"
  fi
fi

echo
echo "=== twins resolve by PACKAGE, not by name (identity migration 2026-10-06) ==="
# crypto_verify and proof_pressure both define main.check_nonce with the same implementation. A
# migration that keyed proof references by name collapsed both cryptoFns entries onto one package.
# Each proof reference must name the package the manifest attributes to its own source file.
tw="$(python3 - "$TMP/man.original" "$ROOT_DIR/Concrete/Proof/Proof.lean" <<'PY'
import re, sys
rows = [l for l in open(sys.argv[1]) if l.startswith("Concrete.Proof.cryptoFns <- ") and "/main.check_nonce " in l]
by_path = {}
for l in rows:
    m = re.search(r"<- ([0-9a-f]{32})/main\.check_nonce .*\(([^()]+\.con)\)\s*$", l.strip())
    if m: by_path.setdefault(m.group(2), set()).add(m.group(1)[:8])
want = set().union(*by_path.values()) if by_path else set()
got = set(re.findall(r"GeneratedAttestations\.cryptoFns_([0-9a-f]{8})_check_nonce\b", open(sys.argv[2]).read()))
ok = len(by_path) == 2 and all(len(v) == 1 for v in by_path.values()) and len(want) == 2 and got == want
print(("ok" if ok else "no") + f" sources={sorted(by_path)} manifest={sorted(want)} proof_refs={sorted(got)}")
PY
)"
case "$tw" in
  ok*) ok "the two main.check_nonce twins map to two packages, each its own source's (${tw#ok })" ;;
  *)   no "main.check_nonce twins do not map one-to-one to their sources' packages (${tw#no })" ;;
esac

echo "ATTESTATION-REFS-ORDER: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
