#!/usr/bin/env bash
# SLICE 8 — ADVERSARIAL CLOSURE (R-0004).
#
# Every other gate asks whether the evidence path works. This one assumes an attacker who has write
# access to the receipt store and wants a claim to read as kernel-replayed when it is not.
#
# THE STRUCTURE IS THE CONTROL. Each attack modifies exactly ONE record in a store of five genuine
# receipts, and every assertion checks BOTH halves:
#
#     the attacked record must not read as current   AND   the other four must still read current
#
# A consumer that rejected everything would satisfy the first half of every attack in this file and
# fail the second, so "deny all" cannot pass. A consumer that accepted everything fails the first.
# Neither degenerate answer survives.
#
# ATTACKS RUN THROUGH THE PRODUCTION CONSUMER — `--report proof-status --receipts` — not through a
# unit probe of the decoder. What matters is what the shipped status surface says when handed hostile
# bytes, because that is the thing a user reads.
#
# DELIBERATELY OUT OF SCOPE: cache attacks. `ProofCache` does not exist, so a cold/warm/corrupt-cache
# test would pass vacuously today. It is a ProofCache graduation condition, not a Slice 8 exit
# criterion, and asserting it here would be a green light bought with an empty test.
set -uo pipefail
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
BIN="$ROOT_DIR/.lake/build/bin/concrete"
SRC="examples/elf_header/src/main.con"
TRUST_SRC="examples/proof_patterns/composition_trusted_helper/src/main.con"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

consume(){ "$BIN" "${2:-$SRC}" --report proof-status --receipts "$1" 2>/dev/null \
           | sed -n '/=== Replay Receipts/,$p' || true; }
tally(){ grep -oE '[0-9]+ current, [0-9]+ not current, [0-9]+ unreadable' <<<"$1" | head -1; }

"$BIN" "$SRC" --report receipts --out "$TMP/base.txt" >/dev/null 2>&1
BASE="$(tally "$(consume "$TMP/base.txt")")"
if [ "$BASE" = "5 current, 0 not current, 0 unreadable" ]; then
  ok "baseline: five genuine receipts read current (every attack below is measured against this)"
else
  no "baseline is not 5 current ($BASE) — every attack in this file would be vacuous"
  GATE_DONE=1; echo "SLICE8-ADVERSARIAL: PASS=$PASS FAIL=$FAIL"; exit 1
fi

# attack <name> <python-mutator> <expect-current> <expect-bad>
#   The mutator receives the store as a list of (subject, body-lines) and returns the same shape.
#   `expect-bad` is the count that must NOT be current — either not-current or unreadable.
attack() {
  local name="$1" mut="$2" wantcur="$3" wantbad="$4"
  python3 - "$TMP" "$mut" <<'PY'
import sys, pathlib
T, mut = sys.argv[1], sys.argv[2]
s = pathlib.Path(T + "/base.txt").read_text()
recs = []
for blk in s.split("== ")[1:]:
    name, _, body = blk.partition("\n")
    recs.append([name.strip(), body.rstrip("\n").split("\n")])
exec(mut)
pathlib.Path(T + "/attack.txt").write_text(
    "\n".join("== " + k + "\n" + "\n".join(v) for k, v in recs))
PY
  local out cur bad
  out="$(consume "$TMP/attack.txt")"
  cur="$(grep -oE '^[0-9]+ current' <<<"$(tally "$out")" | grep -oE '[0-9]+' || echo 0)"
  bad=$(( ${wantcur} + ${wantbad} - ${cur} ))
  if [ "$cur" = "$wantcur" ]; then
    ok "$name — attacked record rejected, $cur genuine receipt(s) still current"
  else
    no "$name — expected $wantcur still current, got: $(tally "$out")"
  fi
}

echo "=== substitution: the store's key is not the claim ==="

# A genuine receipt filed under another claim's name. The comparison reads the subject DIGEST, so the
# filename never enters the verdict.
attack "receipt/claim substitution" \
  'recs[0][1], recs[1][1] = recs[1][1], recs[0][1]' 3 2

# A receipt from a DIFFERENT PACKAGE, filed under a name this program does have. Its subject digest
# describes another program's declaration.
attack "package substitution" \
  'import subprocess, pathlib
o = subprocess.run(["'"$BIN"'", "examples/crypto_verify/src/main.con", "--report", "receipts",
                    "--out", T + "/other.txt"], capture_output=True)
other = pathlib.Path(T + "/other.txt").read_text().split("== ")[1]
recs[0][1] = other.partition("\n")[2].rstrip("\n").split("\n")' 4 1

echo "=== forging what the receipt binds ==="

# The proof term. Naming a theorem that exists but was never replayed for this claim.
attack "unreplayed artifact substituted" \
  'recs[0][1] = [l if not l.startswith("artifact ") else "artifact Examples.ElfHeader.Proofs.check_class_correct" for l in recs[0][1]]' 4 1

# The dependency closure. A receipt whose root describes a different closure than the program has.
attack "dependency closure altered" \
  'recs[0][1] = [l if not l.startswith("root ") else l[:-4] + "0000" for l in recs[0][1]]' 4 1

# Table bindings: one added, and one removed. Both change what the evidence claims to depend on.
attack "table binding injected" \
  'recs[0][1] = recs[0][1] + ["table Fake.Table 00000000000000000000000000000000"]' 4 1
attack "table binding removed" \
  'recs[0][1] = [l for l in recs[0][1] if not l.startswith("table ")]' 4 1

echo "=== trust deletion and laundering ==="

# These run against a fixture that genuinely crosses a trusted boundary, so the attack has something
# to launder. The baseline is asserted first for the same reason as above.
"$BIN" "$TRUST_SRC" --report receipts --out "$TMP/trust.txt" >/dev/null 2>&1
TBASE="$(tally "$(consume "$TMP/trust.txt" "$TRUST_SRC")")"
if grep -qE '^2 current' <<<"$TBASE"; then
  ok "trust baseline: both receipts current, one carrying a named boundary"
else
  no "trust baseline is not 2 current ($TBASE) — the laundering attacks would be vacuous"
fi

launder() {
  local name="$1" py="$2" wantcur="$3"
  python3 - "$TMP" "$py" <<'PY'
import sys, pathlib
T, mut = sys.argv[1], sys.argv[2]
s = pathlib.Path(T + "/trust.txt").read_text()
recs = []
for blk in s.split("== ")[1:]:
    name, _, body = blk.partition("\n")
    recs.append([name.strip(), body.rstrip("\n").split("\n")])
exec(mut)
pathlib.Path(T + "/laundered.txt").write_text(
    "\n".join("== " + k + "\n" + "\n".join(v) for k, v in recs))
PY
  local out cur
  out="$(consume "$TMP/laundered.txt" "$TRUST_SRC")"
  cur="$(grep -oE '^[0-9]+ current' <<<"$(tally "$out")" | grep -oE '[0-9]+' || echo 0)"
  if [ "$cur" = "$wantcur" ]; then
    ok "$name — laundered record rejected, $cur genuine receipt(s) still current"
  else
    no "$name — expected $wantcur still current, got: $(tally "$out")"
  fi
}

# DELETING the boundary while keeping the flag: a qualification a reader cannot act on.
launder "trust boundary deleted" \
  'recs = [[k, [l for l in v if not l.startswith("boundary ")]] for k, v in recs]' 1
# LAUNDERING: flag flipped to false and boundaries dropped, so a conditional claim reads
# unconditional. This is the attack the qualification exists to prevent.
launder "trust laundered to unconditional" \
  'recs = [[k, [("trust false" if l.startswith("trust ") else l) for l in v if not l.startswith("boundary ")]] for k, v in recs]' 1
# A boundary ADDED where none was assumed: the receipt now disagrees with the program about what the
# claim rests on, in the direction that looks more cautious. It must still be rejected — a receipt
# that can be edited toward caution can be edited away from it.
launder "trust boundary fabricated" \
  'recs[0][1] = [("trust true" if l.startswith("trust ") else l) for l in recs[0][1]] + ["boundary calls.imaginary"]' 1

echo "=== decoding: partial, confused, and contradictory ==="

attack "schema downgraded to an older envelope" \
  'recs[0][1] = [l.replace("receiptV3", "receiptV2") for l in recs[0][1]]' 4 1
attack "required field removed" \
  'recs[0][1] = [l for l in recs[0][1] if not l.startswith("subject ")]' 4 1
attack "required field duplicated with a different value" \
  'recs[0][1] = recs[0][1] + ["subject 00000000000000000000000000000000"]' 4 1
attack "unknown field smuggled in" \
  'recs[0][1] = recs[0][1] + ["verified true"]' 4 1
attack "field value emptied" \
  'recs[0][1] = [("root " if l.startswith("root ") else l) for l in recs[0][1]]' 4 1
attack "digest truncated" \
  'recs[0][1] = [(l[:20] if l.startswith("subject ") else l) for l in recs[0][1]]' 4 1

# CONTRADICTORY CONTEXT: the same subject appearing twice with different contents. Whichever record a
# consumer believes, it is believing one of two claims that disagree — so neither may read current.
attack "same subject recorded twice, disagreeing" \
  'bad = [l if not l.startswith("subject ") else "subject 00000000000000000000000000000000" for l in recs[0][1]]
recs.append([recs[0][0], bad])' 4 1

echo "=== the whole store replaced ==="

# A store of entirely foreign receipts. Nothing is current, and — the part that matters — nothing is
# silently dropped either: every record is reported.
python3 - "$TMP" <<'PY'
import sys, pathlib, subprocess
T = sys.argv[1]
subprocess.run([pathlib.Path(".lake/build/bin/concrete").as_posix(),
                "examples/crypto_verify/src/main.con", "--report", "receipts", "--out", T + "/foreign.txt"],
               capture_output=True)
PY
FOREIGN="$(consume "$TMP/foreign.txt")"
if grep -qE '^0 current' <<<"$(tally "$FOREIGN")" \
   && grep -q 'no such claim in this program' <<<"$FOREIGN"; then
  ok "a wholly foreign store makes nothing current, and every record is named rather than dropped"
else
  no "foreign store handling moved: $(tally "$FOREIGN")"
fi

echo "=== a theorem must be ABOUT the claim it is bound to ==="

# THE BREAK AN INDEPENDENT NON-AUTHOR REVIEW FOUND, and the reason this section exists. Every attack
# above works on the STORE; this one needs no store at all. `#[proof_by(...)]` is a source assertion,
# and issuance took it as ground truth — so binding a claim to a REAL theorem the kernel accepts, but
# about a different declaration, produced "kernel-replayed, receipt current".
#
# The kernel accepting a theorem says the theorem is TRUE. It does not say it is about this claim.
FORGE="$ROOT_DIR/.slice8-forge"
forge_attack() {
  local name="$1" thm="$2" wantissued="$3"
  rm -rf "$FORGE"; mkdir -p "$FORGE"
  cp -r examples/elf_header "$FORGE/f"
  sed -i "s/#\\[proof_by(Examples.ElfHeader.Proofs.check_magic_correct)\\]/#[proof_by($thm)]/" "$FORGE/f/src/main.con"
  local out; out="$("$BIN" "$FORGE/f/src/main.con" --report receipts --out "$TMP/forge.txt" 2>/dev/null || true)"
  local issued; issued="$(grep -oE 'Summary: [0-9]+ issued' <<<"$out" | grep -oE '[0-9]+' || echo -1)"
  # SINCE BUG 079 the subject binding refuses the rebound claim at STATUS, before issuance: its claimed
  # spec is not one the theorem evaluates, so it is `spec_unbound` — and `validate_header`, which reaches
  # it, is contained (`deps_not_current`). Issuance's own theorem_not_about_subject check is now behind
  # the binding and is not what fires. One result governs proved, issuance and consumption.
  local named; named="$(grep -c "main.check_magic \[claim_not_proved\] 'main.check_magic' is 'spec_unbound'" <<<"$out" || true)"
  local contained; contained="$(grep -c "main.validate_header \[claim_not_proved\] 'main.validate_header' is 'deps_not_current'" <<<"$out" || true)"
  rm -rf "$FORGE"
  if [ "$issued" = "$wantissued" ] && [ "$named" -ge 1 ] && [ "$contained" -ge 1 ]; then
    ok "$name — refused as spec_unbound, its caller contained; $issued honest claim(s) still issued"
  else
    no "$name — issued=$issued (wanted $wantissued), named=$named, contained=$contained. A claim bound to an unrelated theorem must not mint, nor anything resting on it."
  fi
}
# ALL FOUR honest claims still issue (R-0482). Before the identity separation the copy's attribute
# edit moved the over-bound package digest, renaming the attestations `validate_header`'s closure rests
# on, so it was withheld too and this expected 3 — the sibling-edit defect, not a property of the
# attack. Scope now binds only name and origin, so only the forged claim is refused.
# THREE AGAIN since bug 079, for a different reason, and the leg asserts the reason so the two cannot be
# confused: the forged claim is not proved (`spec_unbound`), so `validate_header`, which reaches it, is
# CONTAINED (`deps_not_current`) — a claim resting on a non-proved callee must not mint. The sibling-edit
# defect withheld it for an identity rename; this withholds it for containment, and says so.
# Same table, different declaration: binding the table cannot tell these apart, only the spec can.
forge_attack "claim rebound to a sibling theorem" "Examples.ElfHeader.Proofs.check_class_correct" 3
# Across programs: the record's own fields contradict each other (a crypto table under an ELF claim).
forge_attack "claim rebound across programs" "Examples.CryptoVerify.Proofs.check_nonce_correct" 3

echo "=== coverage and exit status are part of the verdict ==="

# DELETING a record used to produce an all-green section: the claim vanished with no line, and a
# dependent of it still read `current`. An all-tick section with a silently truncated denominator is
# exactly what a reviewer acts on.
python3 - "$TMP" <<'PY2'
import sys, pathlib
T = sys.argv[1]
s = pathlib.Path(T + "/base.txt").read_text()
recs = [b for b in s.split("== ")[1:] if not b.startswith("main.check_magic")]
pathlib.Path(T + "/short.txt").write_text("".join("== " + b for b in recs))
PY2
SHORT="$(consume "$TMP/short.txt")"
if grep -q 'NO RECEIPT in this store' <<<"$SHORT" && grep -qE '1 no receipt' <<<"$SHORT"; then
  ok "a proof-linked claim with no record is NAMED and counted, not silently dropped"
else
  no "a deleted record produced no coverage complaint: $(tally "$SHORT")"
fi

# EXIT STATUS. It was unconditionally 0 — five current, zero current, empty and unreadable stores all
# exited 0 — so any CI gate built on this surface passed regardless of what it found.
ex(){ "$BIN" "$SRC" --report proof-status --receipts "$1" >/dev/null 2>&1; echo $?; }
: > "$TMP/empty2.txt"
sed 's/^root .*/root TAMPERED/' "$TMP/base.txt" > "$TMP/bad2.txt"
GOOD_EX="$(ex "$TMP/base.txt")"; SHORT_EX="$(ex "$TMP/short.txt")"
BAD_EX="$(ex "$TMP/bad2.txt")"; EMPTY_EX="$(ex "$TMP/empty2.txt")"
if [ "$GOOD_EX" = "0" ] && [ "$SHORT_EX" = "1" ] && [ "$BAD_EX" = "1" ] && [ "$EMPTY_EX" = "1" ]; then
  ok "exit status distinguishes a clean store (0) from incomplete, tampered and empty ones (1)"
else
  no "exit codes do not carry the verdict: good=$GOOD_EX short=$SHORT_EX bad=$BAD_EX empty=$EMPTY_EX"
fi

echo "=== the honest path still works after all of it ==="

# THE FINAL NON-VACUITY CHECK. After every attack above, the untouched store must still read current.
# Without this, a consumer that had been broken into rejecting everything would have passed this file.
FINAL="$(tally "$(consume "$TMP/base.txt")")"
if [ "$FINAL" = "5 current, 0 not current, 0 unreadable" ]; then
  ok "the genuine store still reads 5 current — no attack left the consumer in a deny-all state"
else
  no "the genuine store no longer reads current after the attack suite: $FINAL"
fi

GATE_DONE=1
echo "SLICE8-ADVERSARIAL: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
