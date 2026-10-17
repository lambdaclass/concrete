#!/usr/bin/env bash
# SUBJECT BINDING (bug 079) — a claim is proved only when its extracted body and parameters ARE the
# specification its theorem evaluates.
#
# Before the fix, a body was compared with a specification only when the function's SOURCE NAME had
# an entry in a side table (`Concrete.Proof.specs`); every other link was skipped. On b87152ba,
# `fn always_seven(x: Int) -> Int { return 7; }` linked to `check_nonce_correct` was `proved`, minted
# a receipt, and read the receipt back as current. One result (`subjectBindingOf`) now governs the
# proved status, receipt issuance and receipt consumption.
#
#   P   positive control: a claim whose body IS its theorem's specification still mints, and its
#       receipt reads back current — so the legs below are not refusing everything;
#   A   always_seven: not proved, named as a different body, and no receipt issued;
#   R   the receipt b87152ba issued for always_seven is REJECTED on consumption, for the binding
#       (claim_not_proved), not for any incidental mismatch;
#   C   a drift-covered claim whose #[spec] and #[proof_by] are changed TOGETHER to another valid
#       proof, with its fingerprint re-pinned, is refused — the name table checked it against its OLD
#       spec, by its source name;
#   M   a subject ATTESTED in its theorem's table, linked to a theorem whose specification is a
#       different body: membership is not binding;
#   K   parameters: the specification's body with the parameters in another order is refused;
#   N   a theorem whose statement evaluates no specification binds nothing.
#
# Everything runs in a scratch copy: receipts are minted only for a workspace resolved from the input.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
export LC_ALL=C
BIN="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$BIN" ] || { echo "error: build first ($BIN missing)" >&2; exit 2; }
OLD_STORE="$ROOT_DIR/scripts/tests/fixtures/bug079/always_seven.b87152ba.store"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Scratch copy: tracked + untracked-unignored files, the built Lean libraries (receipts replay the
# theorem), and a COPY of the binary (the compiler finds std relative to `IO.appPath`).
W="$TMP/repo"; mkdir -p "$W/.lake/build/bin"
git ls-files -co --exclude-standard -z | xargs -0 tar cf - | (cd "$W" && tar xf -) \
  || { no "could not copy the repository"; echo "SUBJECT-BINDING: PASS=$PASS FAIL=$FAIL"; exit 1; }
cp -c "$BIN" "$W/.lake/build/bin/concrete" 2>/dev/null || cp "$BIN" "$W/.lake/build/bin/concrete"
cp -cR "$ROOT_DIR/.lake/build/lib" "$W/.lake/build/" 2>/dev/null || cp -R "$ROOT_DIR/.lake/build/lib" "$W/.lake/build/"
[ -d "$ROOT_DIR/.lake/packages" ] && { cp -cR "$ROOT_DIR/.lake/packages" "$W/.lake/" 2>/dev/null || cp -R "$ROOT_DIR/.lake/packages" "$W/.lake/"; }
C="$W/.lake/build/bin/concrete"
cd "$W"

status(){ "$C" "$1" --report proof-status 2>/dev/null; }
# block <status-text> <qualName> — the proof-status block for one function.
block(){ awk -v q="\`$2\`" '/^-- /{if (hit) exit; buf=$0; next} {buf=buf "\n" $0} index($0, q){hit=1} END{if (hit) print buf}' <<<"$1"; }
# pin <file> <decl> — record the subject's CURRENT v2 digest, as an author re-pinning without
# re-proving would. The legs below are about what the theorem is about, not about freshness.
pin(){
  local d
  d="$("$C" "$1" --report subject-facts 2>/dev/null | awk -v n="$2" '
    /^[^ ]/ {k = split($1, h, "."); cur = h[k]}
    cur == n && /subjectFreshness:/ {
      # The CURRENT digest: "current v2:…" when one is stored, "would be v2:…" when none is, the sole
      # value when it already matches. Taking the first v2 on the line took the STORED one, so a leg
      # that claimed to re-pin left the fingerprint stale and could not isolate the binding.
      if (match($0, /(current|would be) v2:[0-9a-f]+/)) { v = substr($0, RSTART, RLENGTH); sub(/^[a-z ]+ /, "", v) }
      else if (match($0, /v2:[0-9a-f]+/)) v = substr($0, RSTART, RLENGTH)
      print v; exit}')"
  [ -n "$d" ] || return 1
  perl -0pi -e "s/(#\[proof_fingerprint\(\")[^\"]*(\"\)\]\s*\n\s*(?:pub )?fn $2\()/\${1}$d\${2}/" "$1"
}
# fresh <file> <decl> — the stored digest matches the current subject, so a refusal is not freshness.
fresh(){ "$C" "$1" --report subject-facts 2>/dev/null | awk -v n="$2" '
    /^[^ ]/ {k = split($1, h, "."); cur = h[k]}
    cur == n && /subjectFreshness:/ {print; exit}' | grep -q "WOULD-MATCH"; }
mkpkg(){ mkdir -p "$W/examples/$1/src"; printf '[package]\nname = "%s"\n' "$1" > "$W/examples/$1/Concrete.toml"; cat > "$W/examples/$1/src/main.con"; }

echo "=== P: a claim whose body IS its theorem's specification still mints ==="
mkpkg b079_bound <<'CON'
mod main {
    #[spec(Concrete.Proof.computeTagExpr)]
    #[proof_by(Examples.CryptoVerify.Proofs.compute_tag_correct)]
    #[proof_coverage(iff)]
    #[proof_fingerprint("")]
    fn compute_tag(key: Int, message: Int, nonce: Int) -> Int {
        return key * message + nonce;
    }

    pub fn main() -> Int { return compute_tag(1, 2, 3); }
}
CON
PB=examples/b079_bound/src/main.con
pin "$PB" compute_tag || no "P: could not pin the positive control"
B="$(block "$(status "$PB")" main.compute_tag)"
grep -q "^-- proved" <<<"$B" && grep -q "spec: bound to Concrete.Proof.computeTagExpr" <<<"$B" \
  && ok "P: compute_tag is proved and bound to computeTagExpr" \
  || no "P: the positive control is not proved and bound: $(head -1 <<<"$B")"
"$C" "$PB" --report receipts --out "$TMP/p.store" > "$TMP/p.issue" 2>&1
grep -q "Summary: 1 issued, 0 withheld" "$TMP/p.issue" \
  && ok "P: its receipt is issued" || no "P: no receipt for a bound claim: $(grep -m1 -E 'Summary|withheld|-' "$TMP/p.issue")"
"$C" "$PB" --report proof-status --receipts "$TMP/p.store" > "$TMP/p.consume" 2>&1
grep -q "1 current, 0 not current" "$TMP/p.consume" \
  && ok "P: and reads back current" || no "P: the bound claim's receipt does not read back current"

echo ""
echo "=== A: always_seven is not what check_nonce_correct is about ==="
mkpkg b079_seven <<'CON'
mod main {
    #[spec(Concrete.Proof.checkNonceExpr)]
    #[proof_by(Examples.CryptoVerify.Proofs.check_nonce_correct)]
    #[proof_coverage(iff)]
    #[proof_fingerprint("v2:9c35413fd77272f9bc5933c510f2c5bf")]
    fn always_seven(x: Int) -> Int { return 7; }

    pub fn main() -> Int { return always_seven(1); }
}
CON
P7=examples/b079_seven/src/main.con
B="$(block "$(status "$P7")" main.always_seven)"
grep -q "^-- proof stale" <<<"$B" && grep -q "linked theorem is about a different body" <<<"$B" \
  && grep -q "checkNonceExpr' that theorem 'Examples.CryptoVerify.Proofs.check_nonce_correct' evaluates is a different body" <<<"$B" \
  && ok "A: always_seven is not proved — the specification check_nonce_correct evaluates is a different body" \
  || no "A: always_seven is not refused by the binding: $(head -1 <<<"$B")"
"$C" "$P7" --report receipts --out "$TMP/a.store" > "$TMP/a.issue" 2>&1
grep -q "Summary: 0 issued, 1 withheld" "$TMP/a.issue" \
  && grep -q "main.always_seven \[claim_not_proved\]" "$TMP/a.issue" \
  && ok "A: no receipt is issued, and the refusal is claim_not_proved" \
  || no "A: always_seven's receipt was not withheld by the binding: $(grep -m1 -E 'always_seven|Summary' "$TMP/a.issue")"

echo ""
echo "=== R: the receipt b87152ba issued for always_seven is rejected ==="
# The store was minted by the published compiler at b87152ba for exactly this program. A receipt is
# re-checked against facts computed now; the binding is one of those facts.
if [ ! -s "$OLD_STORE" ] || ! grep -q "^== main.always_seven" "$OLD_STORE"; then
  no "R: the b87152ba store is missing — the leg would be vacuous"
else
  "$C" "$P7" --report proof-status --receipts "$OLD_STORE" > "$TMP/r.consume" 2>&1
  grep -q "✗ main.always_seven — cannot be checked \[claim_not_proved\] 'main.always_seven' is 'stale'" "$TMP/r.consume" \
    && grep -q "0 current, 1 not current" "$TMP/r.consume" \
    && ok "R: the previously issued receipt is not current, rejected because the claim is not proved" \
    || no "R: the old receipt was not rejected by the binding: $(grep -m1 -E 'always_seven|current' "$TMP/r.consume")"
fi

echo ""
echo "=== C: #[spec] and #[proof_by] changed together on a drift-covered claim ==="
CV=examples/crypto_verify/src/main.con
B="$(block "$(status "$CV")" main.check_nonce)"
grep -q "^-- proved" <<<"$B" && grep -q "spec: bound to Concrete.Proof.checkNonceExpr" <<<"$B" \
  && ok "C: before — check_nonce is proved and bound to checkNonceExpr" \
  || no "C: before — check_nonce is not proved and bound, so the leg is vacuous: $(head -1 <<<"$B")"
perl -0pi -e 's/#\[spec\(Concrete\.Proof\.checkNonceExpr\)\]\n#\[proof_by\(Examples\.CryptoVerify\.Proofs\.check_nonce_correct\)\]/#[spec(Concrete.Proof.checkClassExpr)]\n#[proof_by(Examples.ElfHeader.Proofs.check_class_correct)]/' "$CV"
if ! grep -q "checkClassExpr" "$CV"; then
  no "C: the attribute edit did not apply — the anchor moved"
else
  pin "$CV" check_nonce || no "C: could not re-pin check_nonce"
  fresh "$CV" check_nonce && ok "C: re-pinned — the fingerprint is fresh, so only the binding can refuse" \
    || no "C: the re-pin did not make the fingerprint fresh, so the leg cannot isolate the binding"
  B="$(block "$(status "$CV")" main.check_nonce)"
  grep -q "^-- proof stale" <<<"$B" \
    && grep -q "checkClassExpr' that theorem 'Examples.ElfHeader.Proofs.check_class_correct' evaluates is a different body" <<<"$B" \
    && ok "C: after — refused: check_class_correct's specification is a different body (fingerprint re-pinned)" \
    || no "C: after — the re-linked claim is not refused by the binding: $(head -1 <<<"$B")"
fi

echo ""
echo "=== M: an attested subject linked to a theorem whose specification is a different body ==="
EH=examples/elf_header/src/main.con
grep -qE "^def elfFns_[0-9a-f]+_check_class " Concrete/Proof/GeneratedAttestations.lean \
  && ok "M: before — main.check_class is attested in elfFns" \
  || no "M: check_class is not attested in elfFns, so the leg does not test membership"
perl -0pi -e 's/#\[spec\(Concrete\.Proof\.checkClassExpr\)\]\n#\[proof_by\(Examples\.ElfHeader\.Proofs\.check_class_correct\)\]/#[spec(Concrete.Proof.checkDataExpr)]\n#[proof_by(Examples.ElfHeader.Proofs.check_data_correct)]/' "$EH"
# check_data's own link names check_data_correct once; the re-link makes it twice.
if [ "$(grep -c 'proof_by(Examples.ElfHeader.Proofs.check_data_correct)' "$EH")" -ne 2 ]; then
  no "M: the attribute edit did not apply — the anchor moved"
else
  pin "$EH" check_class || no "M: could not re-pin check_class"
  fresh "$EH" check_class && ok "M: re-pinned — the fingerprint is fresh, so only the binding can refuse" \
    || no "M: the re-pin did not make the fingerprint fresh, so the leg cannot isolate the binding"
  B="$(block "$(status "$EH")" main.check_class)"
  grep -q "^-- proof stale" <<<"$B" \
    && grep -q "checkDataExpr' that theorem 'Examples.ElfHeader.Proofs.check_data_correct' evaluates is a different body" <<<"$B" \
    && ok "M: after — refused although check_class is a member of the theorem's table: membership is not binding" \
    || no "M: after — the attested subject with the wrong model is not refused: $(head -1 <<<"$B")"
fi

echo ""
echo "=== K: the specification's body under other parameters ==="
mkpkg b079_params <<'CON'
mod main {
    #[spec(Concrete.Proof.computeTagExpr)]
    #[proof_by(Examples.CryptoVerify.Proofs.compute_tag_correct)]
    #[proof_coverage(iff)]
    #[proof_fingerprint("")]
    fn compute_tag(message: Int, key: Int, nonce: Int) -> Int {
        return key * message + nonce;
    }

    pub fn main() -> Int { return compute_tag(1, 2, 3); }
}
CON
PK=examples/b079_params/src/main.con
pin "$PK" compute_tag || no "K: could not pin"
B="$(block "$(status "$PK")" main.compute_tag)"
grep -q "^-- spec not bound to body" <<<"$B" \
  && grep -q "declares parameters \[key, message, nonce\], the function has \[message, key, nonce\]" <<<"$B" \
  && ok "K: the same body with permuted parameters is not bound, and the parameters are named" \
  || no "K: a parameter mismatch was not refused: $(head -1 <<<"$B")"

echo ""
echo "=== N: a theorem whose statement evaluates no specification ==="
B="$(block "$(status examples/loop_invariant/src/main.con)" loop_invariant.count_up)"
grep -q "^-- spec not bound to body" <<<"$B" \
  && grep -q "count_up_loop_preserves' evaluates no specification" <<<"$B" \
  && ok "N: count_up is not proved — its theorem is about a loop step, not a function body" \
  || no "N: count_up is not refused for an unbound specification: $(head -1 <<<"$B")"

echo ""
echo "SUBJECT-BINDING: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
