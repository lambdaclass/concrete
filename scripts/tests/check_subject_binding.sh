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
#   N   a theorem whose statement evaluates no specification binds nothing;
#   L   count_up's `coverage: invariant` label discharges its loop step in none of contracts, prove
#       and the VC report — there is no loop-step binding;
#   H   a specification mentioned only as an assumption — a top-level hypothesis, an implication's
#       domain beneath a conjunction, or a disjunct never established — binds nothing;
#   X   bug 081 — a call's KEY must denote the callee it reaches. tests/programs/bug081_alias_key
#       imports `check_version as check_class`; the body is validateHeaderExpr's, the subject is
#       fresh and bound, and the claim is refused (closure unjustified) and not minted, while the
#       unaliased examples/elf_header validate_header stays proved and minted. The same attack with
#       a colliding `check_class` definition is refused too, and the stale-fingerprint warning
#       prints the value migration-manifest reports as current.
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
# ONE tar reads the whole NUL-separated list (-T -). Through xargs, a list longer than xargs'
# command buffer (131072 bytes, GNU) became several concatenated archives; the receiving tar stops
# at the first end-of-archive marker and the next writer died of SIGPIPE.
git ls-files -co --exclude-standard -z | tar cf - --null -T - | (cd "$W" && tar xf -) \
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
echo "=== X: bug 081 — a call's key must denote the callee it reaches ==="
EHX=examples/elf_header/src/main.con
B="$(block "$(status "$EHX")" main.validate_header)"
grep -q "^-- proved" <<<"$B" && grep -q "spec: bound to Concrete.Proof.validateHeaderExpr" <<<"$B" \
  && ok "X: control — the unaliased elf_header validate_header is proved and bound" \
  || no "X: control — elf_header validate_header is not proved, so the legs below are vacuous: $(head -1 <<<"$B")"
"$C" "$EHX" --report receipts --out "$TMP/x0.store" > "$TMP/x0.issue" 2>&1
grep -q "^== main.validate_header" "$TMP/x0.store" \
  && ok "X: control — its receipt is issued" \
  || no "X: control — no receipt for the unaliased validate_header: $(grep -m1 -E 'validate_header|Summary' "$TMP/x0.issue")"
FX=tests/programs/bug081_alias_key/src/main.con
EV=main.evil.evil_validate_header
fresh "$FX" evil_validate_header && ok "X: the aliased claim's stored fingerprint is fresh, so only the key can refuse" \
  || no "X: the aliased claim's fingerprint is not fresh — the leg cannot isolate the key"
# `dependency closure unjustified` is a DOWNGRADE of `proved`: it is reached only by a claim whose
# subject is fresh and bound to its theorem's specification, so the key is the only thing refusing.
B="$(block "$(status "$FX")" "$EV")"
grep -q "^-- dependency closure unjustified" <<<"$B" \
  && grep -q "call \`check_class\` reaches 'main.check_version', but 'Concrete.Proof.elfFns' binds \`check_class\` to 'main.check_class'" <<<"$B" \
  && ok "X: refused — check_class reaches check_version, and elfFns binds the key check_class to check_class" \
  || no "X: the aliased claim is not refused by its key: $(head -1 <<<"$B")"
"$C" "$FX" --report receipts --out "$TMP/x.store" > "$TMP/x.issue" 2>&1
grep -q "$EV \[claim_not_proved\]" "$TMP/x.issue" && ! grep -q "^== $EV" "$TMP/x.store" \
  && ok "X: no receipt is minted for it, and the refusal is claim_not_proved" \
  || no "X: the aliased claim's receipt was not withheld: $(grep -m1 -E 'evil|Summary' "$TMP/x.issue")"
grep -q "^== main.check_magic" "$TMP/x.store" \
  && ok "X: the fixture's own unaliased claims still mint (the refusal is not the whole package)" \
  || no "X: the fixture minted nothing — the refusal leg may be refusing everything"
# concrete prove --check is a surface too (Codex, land3): it mapped every verdict but `stale` to
# "checked" once the theorem replayed, so the refused claim read discharged=true and exited 0.
# check_info <file> <qual> — "<status> <discharged> <exit>" of its one refinement check.
check_info(){
  local js rc=0
  js="$("$C" prove "$1" "$2" --check --json 2>/dev/null)" || rc=$?
  python3 -c "
import json,sys; d=json.loads(sys.argv[1]); r=[c for c in d['checks'] if c['obligation_kind']=='refinement']
print(r[0]['status'] if len(r)==1 else 'none', r[0]['discharged'] if len(r)==1 else 'none', d['all_checked'], sys.argv[2])" "$js" "$rc" 2>/dev/null || echo "unparsable"
}
CI="$(check_info "$FX" "$EV")"
[ "$CI" = "correspondence_unjustified False False 2" ] \
  && ok "X: prove --check reports the aliased claim correspondence_unjustified, not discharged, exit 2" \
  || no "X: prove --check on the aliased claim reads '$CI' (want correspondence_unjustified False False 2)"
CI="$(check_info "$FX" main.check_magic)"
[ "$CI" = "checked True True 0" ] \
  && ok "X: control — prove --check discharges the fixture's unaliased check_magic (exit 0)" \
  || no "X: control — prove --check on check_magic reads '$CI' (want checked True True 0)"
# Whole-file check-proofs (Codex, land3 round 3): the same rule. The aliased claim's theorem replays,
# so it used to read `checked` here (and in the claim inventory) and was listed as kernel-verified.
CPJ="$("$C" "$FX" --report check-proofs --json 2>/dev/null)"
CPV="$(python3 -c "
import json,sys; d=json.loads(sys.argv[1]); c={x['function']:x for x in d['checks'] if x['obligation_kind']=='refinement'}
e=c.get(sys.argv[2],{}); m=c.get('main.check_magic',{})
print(e.get('status'), e.get('discharged'), m.get('status'), m.get('discharged'), d['summary'].get('refused'), d['all_checked'])" "$CPJ" "$EV" 2>/dev/null)"
CPT="$("$C" "$FX" --report check-proofs 2>&1)"
# 2 refused: the aliased claim and the fixture's stale check_class_orig (a stale claim is not verified).
[ "$CPV" = "correspondence_unjustified False checked True 2 False" ] \
  && grep -qF "? $EV — Examples.ElfHeader.Proofs.validate_header_correct (claim verdict: correspondence_unjustified)" <<<"$CPT" \
  && ! grep -qF "✓ $EV" <<<"$CPT" \
  && ok "X: check-proofs reports the aliased claim refused (not checked, not kernel-verified); check_magic stays checked" \
  || no "X: check-proofs on the aliased claim reads '$CPV' (want correspondence_unjustified False checked True 2 False)"
# A STALE claim is not verified either (Codex, land3 round 4): stale_missing_partial links one theorem to
# a proved claim (states.partial) and a stale one (states.stale); only the proved one is kernel-verified.
SMP="$("$C" examples/proof_patterns/stale_missing_partial/src/main.con --report check-proofs 2>&1)"
grep -qF "✓ states.partial — Examples.ProofPatterns.Proofs.add_three_correct" <<<"$SMP" \
  && grep -qF "? states.stale — Examples.ProofPatterns.Proofs.add_three_correct (claim verdict: stale)" <<<"$SMP" \
  && ! grep -qF "✓ states.stale" <<<"$SMP" \
  && grep -qF "Summary: 1 verified, 0 failed; 1 refused (type-checked, NOT proved)" <<<"$SMP" \
  && [ "$("$C" examples/proof_patterns/stale_missing_partial/src/main.con --report check-proofs --json 2>/dev/null | python3 -c "import json,sys; print(json.load(sys.stdin)['all_checked'])" 2>/dev/null)" = False ] \
  && ok "check-proofs: the stale claim sharing a theorem with a proved one is refused, not verified" \
  || no "check-proofs counts the stale claim as verified: $(grep -m1 'Summary' <<<"$SMP")"
# Positive control for all_checked: a file whose every claim is proved and replays reads true.
ACK="$("$C" examples/elf_header/src/main.con --report check-proofs --json 2>/dev/null | python3 -c "import json,sys; d=json.load(sys.stdin); print(d['all_checked'], len(d['checks'])>0 and all(c['status']=='checked' for c in d['checks']))" 2>/dev/null)"
[ "$ACK" = "True True" ] \
  && ok "check-proofs: all_checked is true for elf_header, whose every claim is proved and checked" \
  || no "check-proofs all_checked positive control on elf_header reads '$ACK' (want True True)"
# THE TABLE SIDE OF THE KEY (Codex, land3). A specification's `.call key` evaluates whatever the
# table's `globals` dispatch returns, while scoped evidence was read from `attested`/`entries` alone: a
# table attesting checkClassFn under `check_class` while dispatching `check_class` to checkVersionFn
# (or to a model with the same body and other parameters) was still evidence. Each must be refused.
cat > "$TMP/dispatch.lean" <<'LEAN'
import Concrete
open Concrete Concrete.Proof
def swapped : FnTable :=
  { elfFns with globals := fun k => if k == "check_class" then elfFns.globals "check_version" else elfFns.globals k }
def bump (k : String) (m : PFnDef) : PFnDef :=
  if k == "check_class" then { m with params := m.params ++ ["extra"] } else m
def reparam : FnTable := { elfFns with globals := fun k => (elfFns.globals k).map (bump k) }
def verdict (t : FnTable) : String :=
  match scopedEntryEvidence t with
  | .error (.dispatchMismatch d k) => s!"refused:{d.localName}:{k}"
  | .error _ => "other"
  | .ok rows => s!"ok:{rows.length}"
#eval IO.println s!"CONTROL {verdict elfFns}"
#eval IO.println s!"SWAP {verdict swapped}"
#eval IO.println s!"PARAM {verdict reparam}"
-- The other evidence APIs (Codex, land3 round 4): a dispatch returning the SAME IDENTITY with another
-- body or parameter list. isEvidenceBearing stays identity-level (it must kernel-reduce in generated
-- `by decide` examples); root and tableEntryEvidence compare the whole model.
def tv (t : FnTable) : String :=
  let te := match tableEntryEvidence t with
    | .ok rows => s!"ok:{rows.length}"
    | .error (.dispatchMismatch c k) => s!"refused:{c.render}:{k}"
    | .error _ => "other"
  s!"root={t.root.isSome} entries={te}"
def subBody (k : String) (m : PFnDef) : PFnDef :=
  if k == "check_class" then { m with body := checkVersionFn.body } else m
#eval IO.println s!"TCONTROL {tv elfFns}"
#eval IO.println s!"TBODY {tv { elfFns with globals := fun k => (elfFns.globals k).map (subBody k) }}"
#eval IO.println s!"TPARAM {tv reparam}"
LEAN
DOUT="$(lake env lean "$TMP/dispatch.lean" 2>&1)"
grep -q "^CONTROL ok:5$" <<<"$DOUT" \
  && ok "X: control — elfFns yields its five scoped entries (its dispatch answers each key with its model)" \
  || no "X: control — elfFns scoped evidence: $(head -3 <<<"$DOUT" | tr '\n' ' ')"
grep -q "^SWAP refused:main.check_class:check_class$" <<<"$DOUT" \
  && ok "X: a table whose dispatch sends check_class to another model is refused (dispatchMismatch)" \
  || no "X: a mismatched dispatch was not refused: $(grep -m1 '^SWAP' <<<"$DOUT")"
grep -q "^PARAM refused:main.check_class:check_class$" <<<"$DOUT" \
  && ok "X: a dispatch to the same body under other parameters is refused (whole-model equality)" \
  || no "X: a same-body, other-parameters dispatch was not refused: $(grep -m1 '^PARAM' <<<"$DOUT")"
grep -q "^TCONTROL root=true entries=ok:5$" <<<"$DOUT" \
  && ok "X: control — elfFns has a root and five entry-evidence rows" \
  || no "X: control — elfFns root/entry evidence: $(grep -m1 '^TCONTROL' <<<"$DOUT")"
grep -q "^TBODY root=false entries=refused:v1:user:main.check_class:check_class$" <<<"$DOUT" \
  && grep -q "^TPARAM root=false entries=refused:v1:user:main.check_class:check_class$" <<<"$DOUT" \
  && ok "X: a same-identity dispatch with another body or parameters gets no root and refuses entry evidence" \
  || no "X: same-identity substitution: $(grep -m1 '^TBODY' <<<"$DOUT") / $(grep -m1 '^TPARAM' <<<"$DOUT")"
# The same attack where `check_class` IS defined. The call graph then resolves the spelling to the
# definition, not the import, so the edge set it reports is not what the elaborator resolved.
FC=tests/programs/bug081_alias_key_collide/src/main.con
mkdir -p "$(dirname "$FC")"; cp tests/programs/bug081_alias_key/Concrete.toml "$(dirname "$(dirname "$FC")")/"
perl -pe 's/fn check_class_orig\(/fn check_class(/' "$FX" > "$FC"
B="$(block "$(status "$FC")" main.check_class)"
grep -q "^-- proved" <<<"$B" && ok "X: collision — main.check_class is proved" \
  || no "X: collision — the rename did not produce a proved check_class: $(head -1 <<<"$B")"
B="$(block "$(status "$FC")" "$EV")"
grep -q "^-- dependency closure unjustified" <<<"$B" && grep -q "call \`check_class\` reaches 'main.check_version'" <<<"$B" \
  && ok "X: collision — the aliased claim is still refused by its key" \
  || no "X: collision — the aliased claim is not refused: $(head -1 <<<"$B")"
# The stale-fingerprint warning names the value to pin. `check_class_orig` is the fixture's stale link
# (the rename moved its subject); the printed value must be migration-manifest's `current`.
WARN="$("$C" "$FX" --report proof-status 2>&1 >/dev/null | grep "stale fingerprint for 'main.check_class_orig'" | grep -oE 'current "[^"]*"' | head -1 | sed -E 's/current "(.*)"/\1/')"
MAN="$("$C" "$FX" --report migration-manifest 2>/dev/null | awk -F' [|] ' '$1 == "main.check_class_orig" {sub(/^current=/, "", $6); print $6}')"
[ -n "$WARN" ] && [ "$WARN" = "$MAN" ] && [[ "$WARN" == v2:* ]] \
  && ok "X: the stale-fingerprint warning prints the value to pin ($WARN), as migration-manifest reports it" \
  || no "X: the stale-fingerprint warning prints '$WARN', migration-manifest reports '$MAN'"

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
CI="$(check_info "$PK" main.compute_tag)"
[ "$CI" = "spec_unbound False False 2" ] \
  && ok "K: prove --check reports it spec_unbound, not discharged, exit 2 (the theorem replays)" \
  || no "K: prove --check on the permuted-parameter claim reads '$CI' (want spec_unbound False False 2)"

echo ""
echo "=== N: a theorem whose statement evaluates no specification ==="
B="$(block "$(status examples/loop_invariant/src/main.con)" loop_invariant.count_up)"
grep -q "^-- spec not bound to body" <<<"$B" \
  && grep -q "count_up_loop_preserves' evaluates no specification" <<<"$B" \
  && ok "N: count_up is not proved — its theorem is about a loop step, not a function body" \
  || no "N: count_up is not refused for an unbound specification: $(head -1 <<<"$B")"

echo ""
echo "=== L: no coverage label discharges count_up's loop step (contracts, prove, VCs) ==="
# The link says `coverage: invariant` and names count_up_loop_preserves, a theorem about a loop step
# that no loop-step binding ties to this loop. Three views used to discharge O2 from the label alone.
LI=examples/loop_invariant/src/main.con
CT="$("$BIN" "$LI" --report contracts 2>&1)"
COUNTUP="$(awk '/^loop_invariant.count_up  \(loop/{f=1;next} /^loop_invariant\./{f=0} f' <<<"$CT")"
[ -n "$COUNTUP" ] && ! grep -q "proved_by_lean\|Proofs\.count_up_loop_preserves" <<<"$COUNTUP" \
  && grep -q "operational step:  planned — needs Lean" <<<"$COUNTUP" \
  && ok "L: contracts — count_up's operational step reads needs Lean, not proved_by_lean" \
  || no "L: contracts still discharge count_up's loop step: $(grep -m1 'operational step' <<<"$COUNTUP")"
PV="$("$BIN" prove "$LI" count_up 2>&1)"
grep -q "O2 invariant_preservation.*operational: needs Lean" <<<"$PV" && ! grep -q "Proofs\.count_up_loop_preserves\|proved_by_lean" <<<"$PV" \
  && ok "L: prove — O2's operational half needs Lean; the label's theorem is not cited" \
  || no "L: prove discharges count_up's O2: $(grep -m1 'O2' <<<"$PV")"
VC="$("$BIN" "$LI" --report vcs 2>&1)"
O2="$(awk '/count_up@[0-9]+#O2\]/{f=1;print;next} /^  \[/{f=0} f' <<<"$VC")"
[ -n "$O2" ] && ! grep -q "proved_by_lean\|Proofs\.count_up_loop_preserves" <<<"$O2" \
  && grep -q "Total: .* 0 proved_by_lean" <<<"$VC" \
  && ok "L: VCs — count_up's O2 is not proved_by_lean and no VC in the file is" \
  || no "L: the VC report discharges count_up's O2 by Lean: $(tr '\n' ' ' <<<"$O2" | head -c 200)"

echo ""
echo "=== H: a specification mentioned only in a HYPOTHESIS binds nothing ==="
# Examples.TestFixtures.Proofs.spec_in_hypothesis_only assumes add_commutativity evaluates to none and
# concludes True. The generator read spec terms from the whole type, so it bound — and the claim's body
# IS add_commutativity, so it read proved on a theorem that proves True. Bindings come from conclusions.
TBL=Concrete/Proof/ClassificationTable.lean
if grep -q '^  ("Examples.TestFixtures.Proofs.spec_in_hypothesis_only", "body"' "$TBL"; then
  ok "H: the theorem was processed (it has a classification row)"
else
  no "H: the hypothesis-only theorem has no classification row, so the leg below would be vacuous"
fi
if grep -qE '^  \("Examples.TestFixtures.Proofs.spec_in_hypothesis_only", "add_commutativity"' "$TBL"; then
  no "H: a binding row exists for a spec that appears only in a hypothesis"
else
  ok "H: no binding row — the spec appears only in a hypothesis"
fi
B="$(block "$(status tests/programs/spec_in_hypothesis_only/test_spec_hyp.con)" main.add)"
grep -q "^-- spec not bound to body" <<<"$B" && grep -q "spec_in_hypothesis_only' evaluates no specification" <<<"$B" \
  && ok "H: the claim is not proved — its theorem's conclusion evaluates no specification" \
  || no "H: the hypothesis-only claim is not refused: $(head -1 <<<"$B")"
# The same assumption beneath a connective (Codex land1 review): the generator removed only the OUTER
# hypotheses and still searched nested implication domains, so `True ∧ ((eval … = none) → True)`
# bound. Only recognized evaluation conclusions bind; a disjunct never established binds nothing.
for case in "spec_in_nested_implication:test_spec_nested.con:a nested implication's domain" \
            "spec_in_irrelevant_disjunction:test_spec_disj.con:an irrelevant disjunction" \
            "spec_in_proof_argument:test_spec_arg.con:an arbitrary function's proof argument" \
            "spec_inside_evaluated_term:test_spec_inside.con:a term the evaluated program ignores"; do
  thm="${case%%:*}"; rest="${case#*:}"; file="${rest%%:*}"; why="${rest#*:}"
  grep -q "^  (\"Examples.TestFixtures.Proofs.$thm\", \"body\"" "$TBL" \
    && ok "H: $thm was processed (it has a classification row)" \
    || no "H: $thm has no classification row, so the leg below would be vacuous"
  grep -qE "^  \(\"Examples.TestFixtures.Proofs.$thm\", \"add_commutativity\"" "$TBL" \
    && no "H: a binding row exists for a spec that appears only in $why" \
    || ok "H: no binding row for $thm — the spec appears only in $why"
  B="$(block "$(status "tests/programs/$thm/$file")" main.add)"
  grep -q "^-- spec not bound to body" <<<"$B" && grep -q "$thm' evaluates no specification" <<<"$B" \
    && ok "H: the $thm claim is not proved — its conclusion evaluates no specification" \
    || no "H: the $thm claim is not refused: $(head -1 <<<"$B")"
done

echo ""
echo "SUBJECT-BINDING: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
