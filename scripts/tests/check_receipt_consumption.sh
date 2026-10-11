#!/usr/bin/env bash
# RECEIPT CONSUMPTION (R-0004 package 3): the other half of the vertical slice.
#
# Issuance proves a receipt can be minted. This proves one can be STORED, READ BACK, and CHECKED —
# and, more importantly, that reading one back grants nothing. A stored receipt is bytes; anything
# can write them. `StoredReceipt` is a deliberately different type from `ProofEvidenceReceipt`, it
# has no minting path, and the only question it is ever asked is whether it agrees with material
# computed fresh from the program right now.
#
# THE STORAGE KEY IS NOT TRUSTED. Records are filed under a subject name so a consumer can find
# them; the name never enters the verdict, because the comparison reads the subject DIGEST. Swapping
# two receipts under each other's names is therefore defeated by the comparison itself rather than
# by a check someone remembered to write.
#
# EVERY NEGATIVE HAS A LIVE POSITIVE CONTROL, and the controls are TARGETED: a tampered store must
# move exactly the receipts it tampered with, because a consumer that rejected everything would
# satisfy the negatives while being useless.
set -uo pipefail
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
TMP="$(mktemp -d)"
# Relocation probes live INSIDE the workspace: a program outside it cannot resolve the proof library
# from its own input and is refused for that separate reason (asserted in check_receipt_issuance.sh).
WORK="$ROOT_DIR/.receipt-consume-probe"
rm -rf "$WORK"; mkdir -p "$WORK"
trap 'rm -rf "$TMP" "$WORK"' EXIT
BIN="$ROOT_DIR/.lake/build/bin/concrete"
SRC="examples/elf_header/src/main.con"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# The receipts section only; the proof-status report above it is a different question.
consume() { "$BIN" "$2" --report proof-status --receipts "$1" 2>/dev/null | sed -n '/=== Replay Receipts/,$p' || true; }
tally()   { grep -oE '[0-9]+ current, [0-9]+ not current, [0-9]+ unreadable' <<<"$1" | head -1; }

"$BIN" "$SRC" --report receipts --out "$TMP/r.txt" >/dev/null 2>&1

echo "=== a freshly written store reads back as current ==="

BASE="$(consume "$TMP/r.txt" "$SRC")"
if [ "$(tally "$BASE")" = "5 current, 0 not current, 0 unreadable" ]; then
  ok "5 receipts written and read back current (the control every negative below is measured against)"
else
  no "round trip moved: $(tally "$BASE")"
fi
if grep -q 'kernel-replayed, receipt current' <<<"$BASE"; then
  ok "a current receipt is what makes a claim kernel-replayed"
else
  no "no claim reports as kernel-replayed — the consumer grants nothing, so the negatives are vacuous"
fi
# THE SEPARATION, asserted. Source-derived status and receipt-backed replay are different facts, and
# the report must not let one read as the other.
if grep -q 'does not itself mean the kernel ran' <<<"$BASE"; then
  ok "the report states that source-derived status is not a replay claim"
else
  no "the disclosure is gone — 'proved' now reads as 'kernel-replayed'"
fi

echo "=== substitution is defeated by the comparison, not by the filename ==="

python3 - "$TMP" <<'PY'
import sys, pathlib
T = sys.argv[1]
s = pathlib.Path(T + "/r.txt").read_text()
recs = []
for blk in s.split("== ")[1:]:
    name, _, body = blk.partition("\n")
    recs.append((name.strip(), body.rstrip("\n")))
d = dict(recs)
def write(p, rs): pathlib.Path(p).write_text("\n".join(f"== {k}\n{v}" for k, v in rs))
sw = dict(d); sw["main.check_magic"], sw["main.check_class"] = d["main.check_class"], d["main.check_magic"]
write(T + "/swapped.txt", list(sw.items()))
write(T + "/foreign.txt", recs + [("main.no_such_function", d["main.check_magic"])])
write(T + "/empty.txt", [])
# a record missing one required line
partial = dict(d)
partial["main.check_data"] = "\n".join(l for l in d["main.check_data"].split("\n") if not l.startswith("root"))
write(T + "/partial.txt", list(partial.items()))
# a record carrying a field this decoder does not know
smuggled = dict(d); smuggled["main.check_data"] = d["main.check_data"] + "\nblessed true"
write(T + "/smuggled.txt", list(smuggled.items()))
# a record written under another envelope version
older = dict(d); older["main.check_data"] = d["main.check_data"].replace("schema receiptV1", "schema receiptV0")
write(T + "/older.txt", list(older.items()))
PY

SWAP="$(consume "$TMP/swapped.txt" "$SRC")"
if [ "$(tally "$SWAP")" = "3 current, 2 not current, 0 unreadable" ]; then
  ok "swapping two receipts moves EXACTLY those two; the other three are untouched"
else
  no "swap detection is not targeted: $(tally "$SWAP")"
fi

# A genuine, unmodified receipt filed under a claim the program does not have. This is the shape a
# receipt from another package takes when it lands in this store, and dropping it silently would
# make an unrelated store look like a partial one.
FOR="$(consume "$TMP/foreign.txt" "$SRC")"
if grep -q 'no such claim in this program' <<<"$FOR" \
   && [ "$(tally "$FOR")" = "5 current, 1 not current, 0 unreadable" ]; then
  ok "a receipt for a claim this program does not have is NAMED, and the real five are unaffected"
else
  no "foreign-claim handling moved: $(tally "$FOR")"
fi

echo "=== tampering with what a receipt binds makes it non-current ==="

# One leg per bound field would be ideal; these two cover the two kinds — a digest the program
# derives (subject) and a digest the closure derives (root). Both must participate.
for field in root subject; do
  sed "s/^$field .*/$field TAMPERED/" "$TMP/r.txt" > "$TMP/t_$field.txt"
  T_OUT="$(consume "$TMP/t_$field.txt" "$SRC")"
  if [ "$(tally "$T_OUT")" = "0 current, 5 not current, 0 unreadable" ]; then
    ok "tampering with '$field' makes every receipt non-current"
  else
    no "'$field' does not participate in the comparison: $(tally "$T_OUT")"
  fi
done

echo "=== a store that will not decode is not a store with fewer receipts ==="

# Partial decoding is the attack: a decoder that defaults a missing field produces a receipt that
# compares equal to another defaulted one. Each of these must be REPORTED, not skipped.
for case in partial:missing_field smuggled:unknown_key older:schema_unreadable; do
  f="${case%%:*}"; want="${case##*:}"
  OUT="$(consume "$TMP/$f.txt" "$SRC")"
  if grep -q "$want" <<<"$OUT" && [ "$(tally "$OUT")" = "4 current, 0 not current, 1 unreadable" ]; then
    ok "a $f record is reported unreadable as [$want], and the other four still read"
  else
    no "$f handling moved (wanted $want): $(tally "$OUT")"
  fi
done

# AN EMPTY STORE CLAIMS NOTHING, and says so with zeros rather than silence.
EMPTY="$(consume "$TMP/empty.txt" "$SRC")"
if [ "$(tally "$EMPTY")" = "0 current, 0 not current, 0 unreadable" ]; then
  ok "an empty store reports zeros — it does not make anything kernel-replayed"
else
  no "an empty store reported something: $(tally "$EMPTY")"
fi
# A MISSING store is an error, not an absence. Silence would read as "no receipts", which is exactly
# what an attacker deleting the file wants it to read as.
MISSING="$("$BIN" "$SRC" --report proof-status --receipts "$TMP/does_not_exist.txt" 2>/dev/null || true)"
if grep -q 'cannot read receipt store' <<<"$MISSING"; then
  ok "a missing store is an error, not silence"
else
  no "a missing receipt store produced no complaint — deleting the file reads as 'no receipts'"
fi

echo "=== a changed program invalidates its receipts ==="

# The case the whole envelope exists for: receipts written, then the program edited. One byte, inside
# a body, so the change is semantic.
cp -r examples/elf_header "$WORK/edited"
EDITED="$WORK/edited/src/main.con"
"$BIN" "$EDITED" --report receipts --out "$TMP/edited.txt" >/dev/null 2>&1
BEFORE="$(tally "$(consume "$TMP/edited.txt" "$EDITED")")"
if grep -q 'b0 == 127' "$EDITED"; then
  sed -i 's/b0 == 127/b0 == 126/' "$EDITED"
  AFTER="$(consume "$TMP/edited.txt" "$EDITED")"
  if [ "$BEFORE" = "5 current, 0 not current, 0 unreadable" ] && grep -qE '[1-9] not current' <<<"$AFTER"; then
    ok "editing one byte of a body invalidates the receipts written before it ($BEFORE -> $(tally "$AFTER"))"
  else
    no "a body edit did not invalidate receipts: before=$BEFORE after=$(tally "$AFTER")"
  fi
else
  no "could not find the literal to edit — this control lost its target"
fi

echo "=== a claim that is no longer proved is refused before any comparison ==="

# THE STATUS CHECK, NOT THE DIGEST COMPARISON. Every leg above invalidates by changing what the
# receipt binds, so a consumer that compared material without first asking whether the claim is
# still proved passed them all (the 2026-10-11 campaign: freshfacts-requires-proved-status
# SURVIVED). Editing ONLY a proof fingerprint makes the claim `stale` while its body is unchanged;
# the consumer must refuse it by status, and naming the cause is the observable difference.
cp -r examples/elf_header "$WORK/stale"
STALE_SRC="$WORK/stale/src/main.con"
"$BIN" "$STALE_SRC" --report receipts --out "$TMP/stale.txt" >/dev/null 2>&1
STALE_BEFORE="$(tally "$(consume "$TMP/stale.txt" "$STALE_SRC")")"
if grep -q 'proof_fingerprint("v2:a669dd14614bc56a33be35aa10226679")' "$STALE_SRC"; then
  sed -i 's/v2:a669dd14614bc56a33be35aa10226679/v2:00000000000000000000000000000000/' "$STALE_SRC"
  STALE_OUT="$(consume "$TMP/stale.txt" "$STALE_SRC")"
  if [ "$STALE_BEFORE" != "5 current, 0 not current, 0 unreadable" ]; then
    no "the stale-claim control had no current receipts to start from — it would be vacuous"
  elif grep -qE "^\s+✗ main\.check_magic — cannot be checked \[claim_not_proved\] .*'stale'" <<<"$STALE_OUT"; then
    ok "a claim made stale by its fingerprint alone is refused as [claim_not_proved], naming staleness"
  else
    no "a stale claim was not refused by status — its receipt was compared as if the claim were proved"
  fi
else
  no "could not find check_magic's fingerprint to edit — this control lost its target"
fi

echo "=== a trusted closure's receipt reads current WITH its boundary ==="

# THE TRUST QUALIFICATION, consumed. Issuance names the boundary (check_receipt_issuance.sh); the
# consumer's fresh facts must carry it too, or every trust-qualified receipt is refused and the
# elf_header legs above — none of which crosses a trusted boundary — cannot tell
# (freshfacts-carries-trusted-boundaries SURVIVED the 2026-10-11 campaign).
TRUST_SRC="examples/proof_patterns/composition_trusted_helper/src/main.con"
"$BIN" "$TRUST_SRC" --report receipts --out "$TMP/trusted.txt" >/dev/null 2>&1
TRUST_OUT="$(consume "$TMP/trusted.txt" "$TRUST_SRC")"
if [ "$(tally "$TRUST_OUT")" = "2 current, 0 not current, 0 unreadable" ] \
   && grep -qE '^\s+✓ calls\.combine — kernel-replayed, receipt current .* ASSUMING calls\.dbl' <<<"$TRUST_OUT"; then
  ok "a trust-qualified receipt reads current and still names its boundary (combine ASSUMING calls.dbl)"
else
  no "a trust-qualified receipt did not read current with its boundary: $(tally "$TRUST_OUT")"
fi

echo "=== a changed PROOF LIBRARY invalidates its receipts ==="

# THE AUTHORITY GAP THIS CLOSES. Until 2026-08-16 `importsId` bound a digest of the compiler's own
# checked-in classification table — compiled into the binary — so replaying the same theorem NAMES
# against a DIFFERENT proof library produced a byte-identical receipt. Nothing in the evidence said
# whose theorems had been accepted. Path independence did not help: both runs were equally
# path-independent and equally anonymous about the library.
#
# The probe ADDS a file rather than editing one, so no existing source is ever left modified if this
# gate is interrupted. An orphan `.lean` is not imported and so is not built, but it IS part of the
# library's content, which is precisely what the receipt now binds.
PROBE="$ROOT_DIR/Concrete/Proof/ZZ_receipt_closure_probe.lean"
rm -f "$PROBE"
LIB_BEFORE="$(tally "$(consume "$TMP/r.txt" "$SRC")")"
printf -- '-- transient probe file for check_receipt_consumption.sh\n' > "$PROBE"
LIB_AFTER="$(tally "$(consume "$TMP/r.txt" "$SRC")")"
rm -f "$PROBE"
LIB_RESTORED="$(tally "$(consume "$TMP/r.txt" "$SRC")")"
if [ "$LIB_BEFORE" != "5 current, 0 not current, 0 unreadable" ]; then
  no "the proof-library control had no current receipts to start from — it would be vacuous"
elif ! grep -qE '^0 current, 5 not current' <<<"$LIB_AFTER"; then
  no "changing the PROOF LIBRARY left the receipts current ($LIB_AFTER) — nothing binds whose theorems were accepted"
elif [ "$LIB_RESTORED" != "5 current, 0 not current, 0 unreadable" ]; then
  no "receipts did not recover when the library was restored ($LIB_RESTORED) — the binding is not a function of content"
else
  ok "changing the proof library invalidates its receipts, and restoring it makes them current again"
fi

echo "=== receipt currency does not depend on the working tree ==="

# A receipt must bind the PROGRAM and its checker, not incidental repository state. Until 2026-08-16
# it bound `compilerIdentity`, which appends `-dirty` when `git status` is non-empty — computed at
# REPORT time. Creating any untracked file, anywhere, turned every current receipt non-current.
#
# The flag was wrong in BOTH directions, which is why it was removed rather than kept as approximate
# signal: a binary built from a clean commit stays that binary after the tree is dirtied, and a
# binary built from a dirty tree reports clean once the tree is cleaned.
NOISE="$ROOT_DIR/.receipt-worktree-noise"
rm -rf "$NOISE"
NOISE_BEFORE="$(tally "$(consume "$TMP/r.txt" "$SRC")")"
mkdir -p "$NOISE"; printf 'untracked file, unrelated to any program\n' > "$NOISE/noise.txt"
NOISE_AFTER="$(tally "$(consume "$TMP/r.txt" "$SRC")")"
rm -rf "$NOISE"
if [ "$NOISE_BEFORE" != "5 current, 0 not current, 0 unreadable" ]; then
  no "the worktree-noise control had no current receipts to start from — it would be vacuous"
elif [ "$NOISE_AFTER" = "$NOISE_BEFORE" ]; then
  ok "an unrelated untracked file leaves every receipt current"
else
  no "an untracked file moved receipt currency ($NOISE_BEFORE -> $NOISE_AFTER) — a receipt is binding repository state, not the program"
fi

GATE_DONE=1
echo "RECEIPT-CONSUMPTION: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
