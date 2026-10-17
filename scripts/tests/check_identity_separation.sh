#!/usr/bin/env bash
# IDENTITY SEPARATION — the R-0482 migration's own controls (IDENTITY_MIGRATION_PLAN.md section 6).
#
# The positive forms of the old over-binding legs live where those legs lived (O1/O2/O2s/O3 in
# check_identity_overbinding.sh; B′ and C′/G in check_identity_rows.sh). This gate holds the rest:
#
#   S   reachable-edit sensitivity: editing a definition a claim reaches moves exactly that claim's
#       dependency root (and the edited subject's own), never the roots of claims that do not reach it;
#   R   context vs root: an UNREACHED dependency edit moves the resolution context and no root, and a
#       stored receipt then reads "re-validate under policy", never "re-establish";
#   T   tagged-origin impersonation, through real manifests and the real resolver;
#   D   D1 refusals at the resolver (two undeclared same-named packages) and at the attestation
#       surface, with the declared-origin and distinct-name cases passing;
#   C5  D5 collisions: a forced 8-hex scope-prefix collision, and two keys rendering one symbol,
#       refuse generation;
#   C6  D6: the debug paths derive no identity of their own; every producer is the one producer;
#   M   the committed old-to-new map is total with 0 shared/refused, and the map tool refuses a
#       doctored tree (lost row, changed implementation, re-pointed selection);
#   V   old bytes are not comparable: a receiptV2 record and old encodings read as unreadable, with no
#       fallback.
#
# Everything that edits source does so in a scratch copy; the working tree is never touched.
# P1 (2026-10-13): a claim whose closure reaches std or another package stays fail-closed, so S is
# demonstrated in-package only; the cross-package leg lands with the slice that admits such nodes.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
export LC_ALL=C
BIN="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$BIN" ] || { echo "error: build first ($BIN missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# Scratch copy: tracked + untracked-unignored files, and a COPY of the binary (the compiler finds
# std relative to `IO.appPath`, so a linked binary would read the original tree's std).
W="$TMP/repo"; mkdir -p "$W/.lake/build/bin"
git ls-files -co --exclude-standard -z | xargs -0 tar cf - | (cd "$W" && tar xf -) \
  || { no "could not copy the repository"; echo "IDENTITY-SEPARATION: PASS=$PASS FAIL=$FAIL"; exit 1; }
WBIN="$W/.lake/build/bin/concrete"
cp -c "$BIN" "$WBIN" 2>/dev/null || cp "$BIN" "$WBIN"

# roots <file> — "<decl>\t<shadow depRoot>" for every subject of one program.
roots() {
  "$WBIN" "$1" --report subject-facts 2>/dev/null | awk '
    /^[^ ]/ { n = split($1, h, "."); cur = h[n] }
    /^  shadow depRoot: / { sub(/^  shadow depRoot: /, ""); print cur "\t" $0 }'
}
pid() { "$WBIN" "$1" --report package-identity 2>/dev/null | awk -F'\t' -v f="$2" '$1 == f {print $2; exit}'; }

echo "=== S: a reachable edit moves exactly the roots of the claims that reach it ==="
CV="$W/examples/crypto_verify/src/main.con"
roots "$CV" > "$TMP/s.before"
perl -0pi -e 's/if nonce <= max_nonce \{/if max_nonce >= nonce {/' "$CV"
if cmp -s "$CV" "$ROOT_DIR/examples/crypto_verify/src/main.con"; then
  no "S: the check_nonce edit did not apply — the anchor moved, so S is vacuous"
else
  roots "$CV" > "$TMP/s.after"
  N="$(grep -c . "$TMP/s.before" || true)"
  MOVED="$(join -t$'\t' <(sort "$TMP/s.before") <(sort "$TMP/s.after") | awk -F'\t' '$2 != $3 {print $1}' | sort | tr '\n' ' ')"
  if [ "$N" -lt 4 ] || grep -q 'REFUSED' "$TMP/s.before"; then
    no "S: crypto_verify's roots could not be read ($N subjects; $(grep -c REFUSED "$TMP/s.before") refused) — S is vacuous"
  elif [ "$MOVED" = "check_nonce verify_message " ]; then
    ok "S: editing check_nonce moved the roots of exactly check_nonce and verify_message (its caller), of $N subjects"
  else
    no "S: editing check_nonce moved the roots of: ${MOVED:-nothing} (expected check_nonce verify_message)"
  fi
fi
cp "$ROOT_DIR/examples/crypto_verify/src/main.con" "$CV"

echo ""
echo "=== R: an unreached dependency edit moves context, not roots — and says so ==="
# The receipts need a replay, and a replay needs the BUILT Lean workspace, so R runs on the real tree
# and edits std in the scratch copy, selected with CONCRETE_STD (the resolver's own override).
RCV="$ROOT_DIR/examples/crypto_verify/src/main.con"
rroots() { CONCRETE_STD="$1" "$BIN" "$RCV" --report subject-facts 2>/dev/null | awk '
    /^[^ ]/ { n = split($1, h, "."); cur = h[n] }
    /^  shadow depRoot: / { sub(/^  shadow depRoot: /, ""); print cur "\t" $0 }'; }
rctx() { CONCRETE_STD="$1" "$BIN" "$RCV" --report package-identity 2>/dev/null | awk -F'\t' '$1 == "context" {print $2; exit}'; }
rroots "$ROOT_DIR/std" > "$TMP/r.roots0"; CTX0="$(rctx "$ROOT_DIR/std")"
CONCRETE_STD="$ROOT_DIR/std" "$BIN" "$RCV" --report receipts --out "$TMP/r.store" > "$TMP/r.issue" 2>&1
perl -0pi -e 's/if v == 62 \{ return 43; \}/if v == 62 { return 43; } if v == 63 { return 47; }/' "$W/std/src/base64.con"
if cmp -s "$W/std/src/base64.con" "$ROOT_DIR/std/src/base64.con"; then
  no "R: the std edit did not apply — the anchor moved, so R is vacuous"
else
  rroots "$W/std" > "$TMP/r.roots1"; CTX1="$(rctx "$W/std")"
  [ -n "$CTX0" ] && [ "$CTX0" != "$CTX1" ] && ok "R: the unreached std edit moved crypto_verify's resolution context" \
    || no "R: the std edit did not move the context ('$CTX0' -> '$CTX1')"
  cmp -s "$TMP/r.roots0" "$TMP/r.roots1" && [ -s "$TMP/r.roots0" ] \
    && ok "R: ...and moved no claim root ($(grep -c . "$TMP/r.roots0") roots compared)" \
    || no "R: an unreached std edit moved a claim root"
  if grep -q '^== ' "$TMP/r.store" 2>/dev/null; then
    CONCRETE_STD="$W/std" "$BIN" "$RCV" --report proof-status --receipts "$TMP/r.store" 2>/dev/null \
      | sed -n '/=== Replay Receipts/,$p' > "$TMP/r.consume"
    NR="$(grep -c '^== ' "$TMP/r.store")"
    if [ "$(grep -c 'differs in: context; still matching' "$TMP/r.consume" || true)" -eq "$NR" ] \
       && [ "$(grep -c 'requires: resolution context changed — re-validate under policy' "$TMP/r.consume" || true)" -eq "$NR" ]; then
      ok "R: all $NR receipts issued before the edit differ only in context and read 're-validate under policy'"
    else
      no "R: the context-only receipts did not all report 'differs in: context' + 're-validate': $(grep -m2 'differs in\|requires' "$TMP/r.consume" | tr '\n' ' ')"
    fi
    if grep -q 're-establish' "$TMP/r.consume"; then
      no "R: a context-only change read as 're-establish' — a context change must never read as a broken proof"
    else
      ok "R: no context-only receipt reads 're-establish'"
    fi
  else
    no "R: no receipt store was issued before the edit, so the receipt legs are vacuous: $(tail -3 "$TMP/r.issue" | tr '\n' ' ')"
  fi
fi
cp "$ROOT_DIR/std/src/base64.con" "$W/std/src/base64.con"
# The pure rule, on every input shape, so the reporting cannot depend on a fixture happening to move
# one field: root-class moves say re-establish, context-only says re-validate, never the other.
cat > "$TMP/repair.lean" <<'LEAN'
import Concrete
open Concrete.Proof
#eval
  let est := fun (d : List String) => ((receiptRepairOf d).getD "").startsWith "the claim's dependency closure changed"
  let val := fun (d : List String) => ((receiptRepairOf d).getD "").startsWith "resolution context changed"
  IO.println s!"{est ["root"]} {est ["root", "context"]} {est ["scope"]} {val ["context"]} {val ["context", "toolchain"]} {!(val ["root", "context"])} {(receiptRepairOf []).isNone}"
LEAN
if out="$(lake env lean "$TMP/repair.lean" 2>&1)" && [ "$out" = "true true true true true true true" ]; then
  ok "R: the repair rule — root-class => re-establish, context-only => re-validate, nothing moved => none"
else
  no "R: the repair rule answered '$out'"
fi

echo ""
echo "=== T: tagged origins cannot be impersonated through a manifest ==="
mkproj() { # mkproj <dir> <name> [origin]
  mkdir -p "$1/src"
  { echo "[package]"; echo "name = \"$2\""; [ -n "${3:-}" ] && echo "origin = \"$3\""; } > "$1/Concrete.toml"
  printf 'mod main {\n    pub fn main() -> Int { return 0; }\n}\n' > "$1/src/main.con"
}
mkproj "$TMP/t_und" app; mkproj "$TMP/t_fake" app "«undeclared»"
mkproj "$TMP/t_tool" app "concrete-toolchain"; mkproj "$TMP/t_std" std
U="$(pid "$TMP/t_und/src/main.con" scope)"; F="$(pid "$TMP/t_fake/src/main.con" scope)"
TC="$(pid "$TMP/t_tool/src/main.con" scope)"; US="$(pid "$TMP/t_std/src/main.con" scope)"
STD="$("$WBIN" "$TMP/t_und/src/main.con" --report package-identity 2>/dev/null | awk -F'\t' '$1=="dependency" && $2=="std" {print $3; exit}')"
FD="$("$WBIN" "$TMP/t_fake/src/main.con" --report package-identity 2>/dev/null | awk -F'\t' '$1=="scope" {print $4; exit}')"
if [ -n "$U" ] && [ -n "$F" ] && [ -n "$TC" ] && [ -n "$US" ] && [ -n "$STD" ]; then
  [ "$U" != "$F" ] && ok "T: origin = \"«undeclared»\" is a declared namespace, distinct from the undeclared scope" \
    || no "T: a manifest origin impersonated the undeclared variant"
  case "$FD" in declared*) ok "T: ...and the report labels it a declared namespace claim" ;; *) no "T: the report labels it '$FD'" ;; esac
  [ "$TC" != "$STD" ] && [ "$US" != "$STD" ] \
    && ok "T: origin = \"concrete-toolchain\" and a user package named std both differ from the bundled std's toolchain scope" \
    || no "T: a user manifest obtained the toolchain std's scope"
else
  no "T: could not read the scopes (und='$U' fake='$F' tool='$TC' userstd='$US' std='$STD')"
fi

echo ""
echo "=== D: two undeclared same-named packages are refused (D1) ==="
mklib() { # mklib <dir> <name> <module> [origin]
  mkdir -p "$1/src"
  { echo "[package]"; echo "name = \"$2\""; [ -n "${4:-}" ] && echo "origin = \"$4\""; } > "$1/Concrete.toml"
  printf 'mod %s {\n    pub fn one() -> Int { return 1; }\n}\n' "$3" > "$1/src/lib.con"
}
mkapp() { # mkapp <dir> <dep1 path> <dep2 path>
  mkdir -p "$1/src"
  printf '[package]\nname = "app_r0482"\n\n[dependencies]\nu1 = { path = "%s" }\nu2 = { path = "%s" }\n' "$2" "$3" > "$1/Concrete.toml"
  printf 'mod main {\n    pub fn main() -> Int { return 0; }\n}\n' > "$1/src/main.con"
}
mklib "$TMP/d_a" util util_a; mklib "$TMP/d_b" util util_b; mklib "$TMP/d_c" util util_c "c.example"
mklib "$TMP/d_d" other util_d
mkapp "$TMP/d_amb" "$TMP/d_a" "$TMP/d_b"; mkapp "$TMP/d_decl" "$TMP/d_a" "$TMP/d_c"; mkapp "$TMP/d_dist" "$TMP/d_a" "$TMP/d_d"
"$WBIN" "$TMP/d_amb/src/main.con" --report package-identity > "$TMP/d_amb.out" 2> "$TMP/d_amb.err"; RC=$?
if [ "$RC" -ne 0 ] && grep -q "two packages named 'util' declare no origin" "$TMP/d_amb.err"; then
  ok "D: two different undeclared packages named 'util' in one resolution are refused, by name"
else
  no "D: the ambiguous resolution was not refused (rc=$RC): $(head -2 "$TMP/d_amb.err" | tr '\n' ' ')"
fi
"$WBIN" "$TMP/d_decl/src/main.con" --report package-identity > /dev/null 2>&1 \
  && ok "D: the same name with one declared origin resolves" || no "D: a declared-origin pair was refused"
"$WBIN" "$TMP/d_dist/src/main.con" --report package-identity > /dev/null 2>&1 \
  && ok "D: distinct undeclared names resolve" || no "D: distinct names were refused"
printf '[package]\nname = "app_dup"\n\n[dependencies]\nu1 = { path = "%s" }\nu1 = { path = "%s" }\n' "$TMP/d_a" "$TMP/d_d" > "$TMP/d_amb/Concrete.toml"
"$WBIN" "$TMP/d_amb/src/main.con" --report package-identity > /dev/null 2> "$TMP/d_dup.err" \
  && no "D: a dependency name bound twice was accepted" \
  || { grep -q "bound twice" "$TMP/d_dup.err" && ok "D: a dependency name bound twice is refused" \
         || no "D: the duplicate binding refused for another reason: $(head -1 "$TMP/d_dup.err")"; }
# THE ATTESTATION SURFACE: two corpus fixtures that are different undeclared packages of one name.
for x in r0482_dupe_a r0482_dupe_b; do
  mkdir -p "$W/examples/$x/src"
  printf '[package]\nname = "r0482_dupe"\n' > "$W/examples/$x/Concrete.toml"
  printf 'mod main {\n    #[proof_by(Examples.Nothing.%s)]\n    pub fn f() -> Int { return %s; }\n    pub fn main() -> Int { return 0; }\n}\n' "$x" "${#x}" > "$W/examples/$x/src/main.con"
done
printf 'mod main {\n    #[proof_by(Examples.Nothing.b)]\n    pub fn f() -> Int { return 7; }\n    pub fn main() -> Int { return 0; }\n}\n' > "$W/examples/r0482_dupe_b/src/main.con"
(cd "$W" && bash scripts/gen/attestation_manifest.sh > /dev/null 2> "$TMP/d_surf.err"); RC=$?
if [ "$RC" -ne 0 ] && grep -q "r0482_dupe" "$TMP/d_surf.err"; then
  ok "D: the attestation surface refuses two different undeclared packages named 'r0482_dupe'"
else
  no "D: the attestation surface accepted the undeclared same-name pair (rc=$RC): $(grep -m1 REFUSE "$TMP/d_surf.err")"
fi
rm -rf "$W/examples/r0482_dupe_a" "$W/examples/r0482_dupe_b"

echo ""
echo "=== OWN: each definition is scoped by its OWNER, never by the root ==="
# A project's ProofCore holds std's definitions beside its own. Before R-0482 every one of them was
# scoped by the ROOT package, so one std function had a different identity in every project using it.
PI="$("$WBIN" "$W/examples/crypto_verify/src/main.con" --report package-identity 2>/dev/null)"
ROOTSC="$(awk -F'\t' '$1=="scope" {print $2}' <<<"$PI")"
STDSC="$(awk -F'\t' '$1=="dependency" && $2=="std" {print $3}' <<<"$PI")"
NROOT="$(awk -F'\t' -v s="$ROOTSC" '$1=="definitions" && $2==s {print $3}' <<<"$PI")"
NSTD="$(awk -F'\t' -v s="$STDSC" '$1=="definitions" && $2==s {print $3}' <<<"$PI")"
NOTHER="$(awk -F'\t' -v a="$ROOTSC" -v b="$STDSC" '$1=="definitions" && $2!=a && $2!=b' <<<"$PI" | grep -c . || true)"
if [ -n "$ROOTSC" ] && [ -n "$STDSC" ] && [ "${NROOT:-0}" -gt 0 ] && [ "${NSTD:-0}" -gt 0 ] && [ "$NOTHER" -eq 0 ]; then
  ok "OWN: crypto_verify's ProofCore scopes $NROOT definitions under its own scope and $NSTD under std's, none elsewhere"
else
  no "OWN: definitions are not owner-scoped (root=$ROOTSC:${NROOT:-0} std=$STDSC:${NSTD:-0} other scopes=$NOTHER)"
fi
STDOWN="$(awk -F'\t' '$1=="owner" && $4=="std"' <<<"$PI" | grep -c . || true)"
[ "$STDOWN" -ge 1 ] && ok "OWN: the resolver records std as the owner of its top-level module(s) ($STDOWN)" \
  || no "OWN: the resolver records no module owned by std"

echo ""
echo "=== C5: generation refuses truncated-prefix and symbol collisions (D5) ==="
GOODLINE="$(bash scripts/gen/attestation_manifest.sh 2>/dev/null | grep ' <- ' | grep -v EXCLUDED | head -1)"
if [ -z "$GOODLINE" ]; then
  no "C5: no manifest row to build the collision from — C5 is vacuous"
else
  PKG="$(printf '%s' "$GOODLINE" | sed -E 's/.* <- ([0-9a-f]{32})\/.*/\1/')"
  OTHER="${PKG:0:8}$(printf '%024d' 0)"
  [ "$OTHER" = "$PKG" ] && OTHER="${PKG:0:8}$(printf '%023d1' 0)"
  { printf '%s\n' "$GOODLINE"; printf '%s\n' "${GOODLINE//$PKG/$OTHER}"; } > "$TMP/c5.man"
  ATTESTATION_MANIFEST_FILE="$TMP/c5.man" ATTESTATION_REFS_OUT="$TMP/c5.lean" \
    bash scripts/gen/attestation_refs.sh > /dev/null 2> "$TMP/c5.err"; RC=$?
  if [ "$RC" -ne 0 ] && grep -q 'share the 8-hex symbol prefix' "$TMP/c5.err" && [ ! -f "$TMP/c5.lean" ]; then
    ok "C5: two scopes sharing an 8-hex prefix refuse generation, and nothing is written"
  else
    no "C5: the prefix collision was not refused (rc=$RC): $(head -2 "$TMP/c5.err" | tr '\n' ' ')"
  fi
  { printf '%s\n' "$GOODLINE"; printf '%s\n' "$GOODLINE" | sed -E 's/impl=[0-9a-f]+/impl=00000000000000000000000000000001/'; } > "$TMP/c5b.man"
  ATTESTATION_MANIFEST_FILE="$TMP/c5b.man" ATTESTATION_REFS_OUT="$TMP/c5b.lean" \
    bash scripts/gen/attestation_refs.sh > /dev/null 2> "$TMP/c5b.err"; RC=$?
  # Two rows of one (table, scope, declaration) with different implementations are two full keys
  # rendering one symbol; the generator's dedupe must not pick one silently.
  if [ "$RC" -ne 0 ] && grep -q 'distinct full keys render one symbol' "$TMP/c5b.err"; then
    ok "C5: two full keys rendering one symbol refuse generation"
  else
    no "C5: two keys rendering one symbol were not refused (rc=$RC): $(head -2 "$TMP/c5b.err" | tr '\n' ' ')"
  fi
fi

echo ""
echo "=== C6: one producer per identity; the debug paths derive none (D6) ==="
PRODUCERS="$(git grep -nE 'PackageArtifactIdentity\.of\?|PackageScopeIdentity\.(ofManifest|standalone|toolchainStd)|ResolvedIdentity\.standalone|ResolutionContextIdentity\.of\?' -- 'Concrete/*.lean' Main.lean \
  | grep -v '^Concrete/Proof/DefinitionIdentity.lean' | cut -d: -f1 | sort -u | tr '\n' ' ')"
EXPECTED="Concrete/Report/DebugBundle.lean Concrete/Report/Reduce.lean Concrete/Resolve/Project.lean Main.lean "
[ "$PRODUCERS" = "$EXPECTED" ] && ok "C6: identity is produced only by the resolver and the one manifestless producer's callers ($PRODUCERS)" \
  || no "C6: identity producers are called from: $PRODUCERS (expected $EXPECTED)"
if git grep -nE 'PackageArtifactIdentity\.of\?|PackageScopeIdentity\.' -- Concrete/Report/DebugBundle.lean Concrete/Report/Reduce.lean | grep -q .; then
  no "C6: a debug path builds an artifact or scope itself instead of calling the one producer"
else
  ok "C6: DebugBundle and Reduce call only ResolvedIdentity.standalone over their complete source map"
fi
if git grep -nE 'syntheticForModules|packageIdentityOf|PackageIdentity\b' -- 'Concrete/*.lean' Main.lean | grep -q .; then
  no "C6: a deleted producer (PackageIdentity / syntheticForModules / packageIdentityOf) is still referenced"
else
  ok "C6: PackageIdentity and its three producers are deleted, not deprecated"
fi

echo ""
echo "=== M: the old-to-new map ==="
MAP="docs/verification/identity-migration-map-r0482.tsv"
RAW="docs/verification/identity-raw-baseline-pre-r0482.tsv"
if [ ! -f "$MAP" ]; then
  no "M: $MAP is missing"
else
  if grep -qE '^# totals: baseline-rows=([0-9]+) mapped=\1 shared=0 refused=0$' "$MAP"; then
    ok "M: the committed map is total — $(grep '^# totals' "$MAP" | sed 's/# totals: //')"
  else
    no "M: the committed map is not total: $(grep '^# totals' "$MAP")"
  fi
  for k in fixture subject dependency reference selection link; do
    a="$(grep -c "^$k	" "$RAW" || true)"; b="$(grep -c "^mapped	$k	" "$MAP" || true)"
    [ "$a" = "$b" ] && ok "M: every baseline $k row is mapped ($a)" || no "M: baseline has $a $k rows, the map maps $b"
  done
  if grep -E '^mapped	(subject|dependency|reference)	' "$MAP" | grep -q 'implementation-changed'; then
    no "M: the map records an implementation change"
  else
    ok "M: no implementation digest changed across the migration"
  fi
fi
# The tool refuses what it must, measured against the migration's own new rows.
NEWRAW="$TMP/new.raw"
(cd "$W" && bash scripts/tools/identity_rows.sh --raw > "$NEWRAW" 2> "$TMP/newraw.err") \
  || no "M: the current tree's raw rows refused: $(tail -2 "$TMP/newraw.err" | tr '\n' ' ')"
AMEND="docs/verification/identity-rows-amendments.tsv"
if [ -s "$NEWRAW" ]; then
  # THE CONTROL FOR THE LEGS BELOW. Each doctors the current tree and requires a specific refusal; if
  # the UNDOCTORED tree already refused, every one of them would pass on that refusal alone — measured:
  # bug 079's explained proof_pressure re-freeze did exactly this, and the map-totality mutation
  # survived. The current tree must map totally, with the explained amendments and nothing else.
  if bash scripts/tools/identity_migration_map.sh "$RAW" "$NEWRAW" "$AMEND" > "$TMP/current.map" 2> "$TMP/current.err"; then
    ok "M: the current tree maps totally onto the baseline — $(grep '^# totals' "$TMP/current.map" | sed 's/# totals: //')"
  else
    no "M: the current tree does not map totally, so the refusal legs below are vacuous: $(grep -E '^refused' "$TMP/current.map" | head -2 | tr '\n' ' ')$(tail -1 "$TMP/current.err")"
  fi
  doctor() { # doctor <label> <awk program> <expected refusal text>
    awk "$2" "$NEWRAW" > "$TMP/doctored.raw"
    if bash scripts/tools/identity_migration_map.sh "$RAW" "$TMP/doctored.raw" "$AMEND" > "$TMP/doctored.map" 2>/dev/null; then
      no "M: the map tool accepted a tree with $1"
    elif grep -E "^refused" "$TMP/doctored.map" | grep -q "$3"; then
      ok "M: the map tool refuses a tree with $1 ($3)"
    else
      no "M: the map tool refused a tree with $1, but not as $3"
    fi
  }
  doctor "a lost subject row" 'BEGIN{d=0} /^subject\t/ && !d {d=1; next} {print}' "no-counterpart"
  doctor "a changed implementation" 'BEGIN{d=0} /^subject\t/ && !d {d=1; OFS="\t"; $7="00000000000000000000000000000000"} {print}' "implementation-changed"
  # Re-pointing one selection at another's symbol leaves two selections on one symbol and none on the
  # other: the tool refuses it by MULTIPLICITY. The leg used to expect "selects" and passed only because
  # a MAPPED line elsewhere contained `ch_selects_high` — the match is now on refused lines only.
  doctor "a re-pointed selection" 'BEGIN{FS=OFS="\t"} /^selection\t/ {s[++n]=$0} !/^selection\t/ {print} END{split(s[1],a,"\t"); split(s[2],b,"\t"); for(i=1;i<=n;i++){ if(i==1){a[5]=b[5]; print a[1],a[2],a[3],a[4],a[5]} else print s[i]}}' "multiplicity"
fi

echo ""
echo "=== V: old bytes are not comparable — no fallback ==="
cat > "$TMP/v.lean" <<'LEAN'
import Concrete
open Concrete.Proof
#eval
  let v2 := "schema receiptV2\nsubject v2:s\nedge body\nroot R\nartifact T\ntrust false\ntoolchain t\ncompiler c\nchecker k\nworkspace w\nimports i"
  match StoredReceipt.decode v2 with
  | .error (.schemaUnreadable "receiptV2") => IO.println "V2-UNREADABLE"
  | .error e => IO.println s!"OTHER {e.canonical}"
  | .ok _ => IO.println "DECODED"
LEAN
OUT="$(lake env lean "$TMP/v.lean" 2>&1)"
[ "$OUT" = "V2-UNREADABLE" ] && ok "V: a receiptV2 record is schema_unreadable (needs re-verification), never compared field-wise" \
  || no "V: a receiptV2 record decoded as: $OUT"
if git grep -nE '"(defIdV1|pkgIdV1|pkgOriginV1|pkgRootV1|pkgSyntheticV1|workspaceV1|depRootPreimageV2):?' -- 'Concrete/*.lean' Main.lean | grep -q .; then
  no "V: a pre-R-0482 encoding tag is still produced: $(git grep -lE '"(defIdV1|pkgIdV1|pkgSyntheticV1|depRootPreimageV2)' -- 'Concrete/*.lean' Main.lean | tr '\n' ' ')"
else
  ok "V: no pre-R-0482 encoding tag (defIdV1, pkg*V1, workspaceV1, depRootPreimageV2) is produced anywhere"
fi
# A REAL record, rewritten into the old envelope: schema receiptV2 with the single `workspace` field.
if [ -s "$TMP/r.store" ]; then
  perl -0pe 's/^schema receiptV3$/schema receiptV2/mg; s/^scope (\S+)\ncontext \S+$/workspace $1/mg' "$TMP/r.store" > "$TMP/v.store"
  VOUT="$("$BIN" "$RCV" --report proof-status --receipts "$TMP/v.store" 2>/dev/null | sed -n '/=== Replay Receipts/,$p')"
  NREC="$(grep -c '^== ' "$TMP/v.store" || true)"
  if [ "$NREC" -gt 0 ] && printf '%s\n' "$VOUT" | grep -qE "^  0 current, 0 not current, $NREC unreadable" \
     && [ "$(printf '%s\n' "$VOUT" | grep -c 'schema_unreadable' || true)" -eq "$NREC" ]; then
    ok "V: $NREC real records rewritten into receiptV2 are all schema_unreadable and none is current"
  else
    no "V: an old-envelope store was not read as unreadable: $(printf '%s\n' "$VOUT" | grep -m1 'current,')"
  fi
else
  no "V: no issued store to rewrite into the old envelope (R did not issue one)"
fi

echo ""
echo "IDENTITY-SEPARATION: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
