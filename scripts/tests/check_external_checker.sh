#!/usr/bin/env bash
# EXTERNAL CHECKER — every theorem in Concrete's Lean sources re-checked outside the Lean kernel that
# produced it.
#
# The population is every theorem the `Concrete`, `Examples` and `DesignExamples` libraries declare, read from the
# elaborated environment by scripts/tools/proof_layer_theorems.lean (the #[proof_by] evidence
# theorems are a subset). It is exported with lean4export and re-checked by two checkers that share
# no code with the Lean kernel or with each other:
#
#   con-leche (leanprover/con-leche, Lean) — machine-checked consistency proof: an accepted stream has
#     a set-theoretic model, so it contains no proof of False (`model_exists`, `no_False_declaration`
#     in ConLeche/MainTheorem.lean). Run in --verified, the mode the theorem is about.
#   nanoda (ammkrn/nanoda_lib, Rust) — no consistency proof; implementation diversity.
#
#   P  the three tools are the pinned revisions (external_checker.pins);
#   I  population: every source module declaring a theorem contributes to it; no sorryAx, and every non-standard axiom is a native axiom minted by a lemma
#      declared in axiom_native_owners.txt — exactly those owners, each named in AXIOMS.md; the
#      evidence theorems among the native ones are exactly axiom_native_trust.txt;
#   A  the standard export is from this toolchain, holds every standard theorem, and declares only
#      the three standard axioms;
#   S  statement fidelity: every exported statement equals the statement in Lean's own environment,
#      byte for byte in the kernel's view (lean4export is unverified; con-leche's theorem is about
#      what it was handed, so this is what ties it to what Lean checked);
#   C  con-leche --verified accepts the standard export;      K  so does nanoda (standard axioms only);
#   X  hostile control: one theorem handed another's proof is rejected by both — by con-leche as a
#      type mismatch AT that theorem, by nanoda at its definitional-equality check (it panics without
#      naming the theorem; the doctored stream differs from the accepted one in that theorem alone).
#      A malformed stream also fails, so the reason is checked, never the exit code alone;
#   N  native trust: con-leche declines the native export for its ._native axiom (it accepts only the
#      standard three) and nanoda refuses it with only those permitted — and nanoda ACCEPTS it with
#      exactly the declared owners' native axioms permitted: every native-trust theorem re-checked
#      except the bit-blasting certificate check each such axiom stands for (check_bv_certificates.sh
#      re-checks those certificates with drat-trim).
#
# Tools are built from pinned SHAs into $CONCRETE_EXT_TOOLS (default ~/.cache/concrete-ext) and
# reused. lean4export is built with THIS repository's toolchain (it must read our .olean files);
# con-leche with its own (it reads exports from older toolchains: the Lean Kernel Arena runs it on
# Lean 4.34.1 exports); nanoda with cargo.
set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
# A pre-push hook in a linked worktree exports repository-local Git variables.
# Without clearing them, `git -C "$tool" rev-parse HEAD` reads Concrete's HEAD,
# falsely rejecting an already-built pinned tool (and misdirecting checkout).
# All Git operations below address separate external-tool repositories.
for git_local_var in $(git rev-parse --local-env-vars); do
  unset "$git_local_var"
done
export LC_ALL=C
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
finish(){ echo "EXTERNAL-CHECKER: PASS=$PASS FAIL=$FAIL"; [ "$FAIL" = 0 ] && exit 0 || exit 1; }

# shellcheck source=external_checker.pins
. scripts/tests/external_checker.pins
EXPECT_FORMAT=3.1.0
STANDARD_AXIOMS="Classical.choice Quot.sound propext"
NATIVE_FILE=scripts/tests/axiom_native_trust.txt
OWNERS_FILE=scripts/tests/axiom_native_owners.txt
NATIVE_AXIOM_RE='\._native\.(bv_decide|native_decide)\.ax_[0-9_]+$'
# A theorem pair for the swap control: distinct statements, both standard-axiom.
SWAP_A=Examples.ElfHeader.Proofs.check_magic_correct
SWAP_B=Examples.CryptoVerify.Proofs.check_nonce_correct
INV=scripts/tools/export_inventory.py

ELAN_BIN="${ELAN_HOME:-$HOME/.elan}/bin"
LAKE="$ELAN_BIN/lake"
[ -x "$LAKE" ] || { echo "error: elan's lake not found at $LAKE (the tools need elan toolchains)" >&2; exit 2; }
[ -x .lake/build/bin/concrete ] || { echo "error: build first (lake build)" >&2; exit 2; }
TOOLS="${CONCRETE_EXT_TOOLS:-$HOME/.cache/concrete-ext}"
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
TOOLCHAIN="$(cat lean-toolchain)"

# build_lake_tool NAME URL REV [TOOLCHAIN-OVERRIDE] TARGET → prints the source dir; builds once per SHA
build_lake_tool() {
  local name=$1 url=$2 rev=$3 tc=$4 target=$5
  local dir="$TOOLS/$name-$rev"
  if [ ! -f "$dir/.built" ]; then
    rm -rf "$dir"; mkdir -p "$TOOLS"
    git clone -q "$url" "$dir" >&2 && git -C "$dir" checkout -q "$rev" >&2 || return 1
    [ -n "$tc" ] && echo "$tc" > "$dir/lean-toolchain"
    (cd "$dir" && "$LAKE" build "$target" >&2) || return 1
    touch "$dir/.built"
  fi
  [ "$(git -C "$dir" rev-parse HEAD)" = "$rev" ] || return 1
  echo "$dir"
}

echo "=== P: pinned tools ==="
EXPORT_DIR=$(build_lake_tool lean4export "$LEAN4EXPORT_URL" "$LEAN4EXPORT_REV" "$TOOLCHAIN" lean4export) \
  && ok "lean4export at $LEAN4EXPORT_REV, built with $TOOLCHAIN" \
  || { no "could not build lean4export at $LEAN4EXPORT_REV"; finish; }
CHECK_DIR=$(build_lake_tool con-leche "$CONLECHE_URL" "$CONLECHE_REV" "" con-leche) \
  && ok "con-leche at $CONLECHE_REV" \
  || { no "could not build con-leche at $CONLECHE_REV"; finish; }
EXPORTER="$EXPORT_DIR/.lake/build/bin/lean4export"
CONLECHE="$CHECK_DIR/.lake/build/bin/con-leche"
NANODA_DIR="$TOOLS/nanoda-$NANODA_REV"
if [ ! -f "$NANODA_DIR/.built" ]; then
  rm -rf "$NANODA_DIR"
  { git clone -q "$NANODA_URL" "$NANODA_DIR" && git -C "$NANODA_DIR" checkout -q "$NANODA_REV" \
    && grep -q 'STACK_SIZE: usize = 16_777_216;' "$NANODA_DIR/src/lib.rs" \
    && sed -i.orig 's/STACK_SIZE: usize = 16_777_216;/STACK_SIZE: usize = 268_435_456;/' "$NANODA_DIR/src/lib.rs" \
    && (cd "$NANODA_DIR" && cargo build --release -q) && touch "$NANODA_DIR/.built"; } >&2
fi
NANODA="$NANODA_DIR/target/release/nanoda_bin"
[ -x "$NANODA" ] && [ "$(git -C "$NANODA_DIR" rev-parse HEAD)" = "$NANODA_REV" ] \
  && ok "nanoda at $NANODA_REV (thread stack patched as the arena does)" \
  || { no "could not build nanoda at $NANODA_REV"; finish; }
# nanoda FILE PERMITTED-AXIOM... → runs nanoda on FILE permitting exactly the axioms named
nanoda() {
  local f=$1; shift
  python3 -c 'import json,sys; print(json.dumps({"export_file_path": sys.argv[1],
    "permitted_axioms": sys.argv[2:], "unpermitted_axiom_hard_error": True,
    "nat_extension": True, "string_extension": True, "num_threads": 4,
    "print_axioms": False, "print_success_message": True}))' "$f" "$@" > "$TMP/nanoda.json"
  "$NANODA" "$TMP/nanoda.json"
}

echo "=== I: population ==="
"$LAKE" env lean scripts/tools/proof_layer_theorems.lean > "$TMP/layer.tsv" 2> "$TMP/layer.err" \
  || { no "proof_layer_theorems.lean failed: $(head -3 "$TMP/layer.err" | tr '\n' ' ')"; finish; }
awk -F'\t' '{n=split($2,a," "); for(i=1;i<=n;i++) print a[i]}' "$TMP/layer.tsv" | sort -u > "$TMP/all-axioms.txt"
: > "$TMP/standard.txt"; : > "$TMP/native.txt"
awk -F'\t' -v std=" $STANDARD_AXIOMS " -v out_s="$TMP/standard.txt" -v out_n="$TMP/native.txt" '{
  s = 1; n = split($2, a, " "); for (i = 1; i <= n; i++) if (index(std, " " a[i] " ") == 0) s = 0
  print $1 > (s ? out_s : out_n) }' "$TMP/layer.tsv"
NALL=$(grep -c . "$TMP/layer.tsv"); NSTD=$(grep -c . "$TMP/standard.txt"); NNAT=$(grep -c . "$TMP/native.txt")
grep -rho 'proof_by([A-Za-z0-9_.]*)' --include='*.con' examples/ tests/ std/ 2>/dev/null \
  | sed 's/proof_by(\(.*\))/\1/' | sort -u > "$TMP/linked.txt"
NLINKED=$(cut -f1 "$TMP/layer.tsv" | sort | comm -12 - "$TMP/linked.txt" | grep -c .)
[ "$NSTD" -gt 0 ] && [ "$NLINKED" -ge 30 ] \
  && ok "$NALL theorems in the proof layer ($NSTD standard, $NNAT native-trust), $NLINKED of them #[proof_by] evidence" \
  || no "population too small: $NALL theorems, $NLINKED #[proof_by] evidence (a vacuous export?)"
# The population is the enumerator's word; this ties it to the source. Every module whose file
# declares a theorem must contribute one, so dropping a library or a module from the enumeration
# fails here rather than silently shrinking what the checkers are handed.
PROOF_LAYER_MODE=modules "$LAKE" env lean scripts/tools/proof_layer_theorems.lean > "$TMP/env-modules.txt" 2>/dev/null
grep -rlE '^\s*(@\[[^]]*\]\s*)?(private |protected )*theorem ' Concrete proofs --include='*.lean' 2>/dev/null \
  | sed -E 's#^proofs/##; s#\.lean$##; s#/#.#g' | sort -u > "$TMP/src-modules.txt"
UNSEEN=$(sort -u "$TMP/env-modules.txt" | comm -13 - "$TMP/src-modules.txt")
[ -s "$TMP/src-modules.txt" ] && [ -z "$UNSEEN" ] \
  && ok "every one of the $(grep -c . "$TMP/src-modules.txt") source modules declaring a theorem contributes to the population" \
  || no "source modules declaring theorems but absent from the population: $(echo $UNSEEN | cut -c1-300)"
# Every Lean library the lakefile declares is in the population. The enumerator, its module filter
# and the exporter name the libraries explicitly; a library added to lakefile.toml without them would
# otherwise be built and never re-checked (DesignExamples reached CI that way at 03d2edec). The
# source-module leg above only scans Concrete/ and proofs/, so this leg is what catches a library
# whose srcDir is anywhere else.
POP_LIBS="Concrete Examples DesignExamples"
LAKE_LIBS=$(python3 -c '
import tomllib
with open("lakefile.toml", "rb") as f:
    libs = tomllib.load(f)["lean_lib"]
names = [lib["name"] for lib in libs]
assert names and all(isinstance(n, str) and n and not any(c.isspace() for c in n) for n in names)
print("\n".join(sorted(set(names))))
') || { no "cannot enumerate lakefile lean_libs"; finish; }
MISSING_LIBS=$(comm -23 <(echo "$LAKE_LIBS") <(printf '%s\n' $POP_LIBS | sort -u))
EXPORTED_LIBS=$(grep -c "\"\$EXPORTER\" $POP_LIBS --" "$0")
UNIMPORTED=""
for lib in $POP_LIBS; do
  grep -q "^import $lib\$" scripts/tools/proof_layer_theorems.lean \
    && grep -q "some \`$lib\b" scripts/tools/proof_layer_theorems.lean || UNIMPORTED="$UNIMPORTED $lib"
done
[ -n "$LAKE_LIBS" ] && [ -z "$MISSING_LIBS" ] && [ "$EXPORTED_LIBS" = 2 ] && [ -z "$UNIMPORTED" ] \
  && ok "every lakefile lean_lib ($(echo $LAKE_LIBS)) is in the exported population" \
  || no "lakefile lean_libs outside the population or exporter: missing [$(echo $MISSING_LIBS)], exporter calls naming all of them: $EXPORTED_LIBS of 2; not imported or filtered by the enumerator: [$(echo $UNIMPORTED)]"
comm -23 "$TMP/all-axioms.txt" <(printf '%s\n' $STANDARD_AXIOMS | sort) > "$TMP/nonstd-axioms.txt"
BAD_AX=$(grep -vE "$NATIVE_AXIOM_RE" "$TMP/nonstd-axioms.txt" || true)
[ -z "$BAD_AX" ] && ok "no sorryAx and no axiom beyond the standard three and generated native ones" \
  || no "axioms outside the standard and native-trust sets: $(echo $BAD_AX)"
grep -E "$NATIVE_AXIOM_RE" "$TMP/nonstd-axioms.txt" > "$TMP/native-axioms.txt" || true
sed -E "s/$NATIVE_AXIOM_RE//" "$TMP/native-axioms.txt" | sort -u > "$TMP/owners.txt"
grep -vE '^(#|$)' "$OWNERS_FILE" | sort -u > "$TMP/owners-declared.txt"
cmp -s "$TMP/owners.txt" "$TMP/owners-declared.txt" \
  && ok "native axioms ($(grep -c . "$TMP/native-axioms.txt")) are minted by exactly the $(grep -c . "$TMP/owners.txt") lemmas in $OWNERS_FILE" \
  || no "native-axiom owners differ from $OWNERS_FILE: $(diff "$TMP/owners.txt" "$TMP/owners-declared.txt" | grep '^[<>]' | tr '\n' ' ')"
UNDOC=""; while read -r o; do grep -qF "$o" docs/verification/AXIOMS.md || UNDOC="$UNDOC $o"; done < "$TMP/owners-declared.txt"
[ -z "$UNDOC" ] && ok "every native-axiom owner is named in docs/verification/AXIOMS.md" \
  || no "native-axiom owners missing from AXIOMS.md:$UNDOC"
grep -vE '^(#|$)' "$NATIVE_FILE" | sort -u > "$TMP/native-listed.txt"
sort "$TMP/native.txt" | comm -12 - "$TMP/linked.txt" > "$TMP/native-evidence.txt"
cmp -s "$TMP/native-evidence.txt" "$TMP/native-listed.txt" \
  && ok "the native-trust evidence theorems are exactly $NATIVE_FILE ($(grep -c . "$TMP/native-listed.txt"))" \
  || no "native-trust evidence differs from $NATIVE_FILE: $(diff "$TMP/native-evidence.txt" "$TMP/native-listed.txt" | grep '^[<>]' | tr '\n' ' ')"

echo "=== A: export ==="
EXPORT="$TMP/standard.ndjson"; NATIVE_EXPORT="$TMP/native.ndjson"
# shellcheck disable=SC2046
"$LAKE" env "$EXPORTER" Concrete Examples DesignExamples -- $(cat "$TMP/standard.txt") > "$EXPORT" 2> "$TMP/export.err" \
  || { no "lean4export failed on the standard set: $(head -3 "$TMP/export.err" | tr '\n' ' ')"; finish; }
read -r EXP_LEAN EXP_TOOL EXP_FORMAT < <(python3 "$INV" meta "$EXPORT")
[ "leanprover/lean4:v$EXP_LEAN" = "$TOOLCHAIN" ] && [ "$EXP_FORMAT" = "$EXPECT_FORMAT" ] \
  && ok "export from Lean $EXP_LEAN (lean4export $EXP_TOOL, format $EXP_FORMAT)" \
  || no "export metadata: Lean $EXP_LEAN format $EXP_FORMAT, expected $TOOLCHAIN format $EXPECT_FORMAT"
python3 "$INV" theorems "$EXPORT" > "$TMP/exported.txt"
MISSING=$(sort "$TMP/standard.txt" | comm -23 - "$TMP/exported.txt")
[ -z "$MISSING" ] && ok "every standard theorem is in the export ($(grep -c . "$TMP/exported.txt") theorems with dependencies)" \
  || no "standard theorems missing from the export: $(echo $MISSING | cut -c1-300)"
python3 "$INV" axioms "$EXPORT" > "$TMP/axioms.txt"
EXTRA=$(comm -23 "$TMP/axioms.txt" <(printf '%s\n' $STANDARD_AXIOMS))
[ -z "$EXTRA" ] && ok "the standard export declares only $(echo $(cat "$TMP/axioms.txt"))" \
  || no "the standard export declares non-standard axioms: $(echo $EXTRA)"
if [ "$NNAT" -gt 0 ]; then
  # shellcheck disable=SC2046
  "$LAKE" env "$EXPORTER" Concrete Examples DesignExamples -- $(cat "$TMP/native.txt") > "$NATIVE_EXPORT" 2> "$TMP/export.err" \
    || { no "lean4export failed on the native set: $(head -3 "$TMP/export.err" | tr '\n' ' ')"; finish; }
fi

echo "=== S: statement fidelity ==="
PROOF_LAYER_MODE=statements "$LAKE" env lean scripts/tools/proof_layer_theorems.lean > "$TMP/st-lean.tsv" 2> "$TMP/st.err" \
  || { no "statement rendering failed: $(head -3 "$TMP/st.err" | tr '\n' ' ')"; finish; }
{ python3 "$INV" statements "$EXPORT"; [ "$NNAT" -gt 0 ] && python3 "$INV" statements "$NATIVE_EXPORT"; } \
  | sort -u > "$TMP/st-export.tsv"
# Only the population is compared (the export also carries Lean core's theorems as dependencies).
join -t$'\t' <(sort "$TMP/st-lean.tsv") "$TMP/st-export.tsv" > "$TMP/st-joined.tsv"
awk -F'\t' '$2!=$4 || $3!=$5 {print $1}' "$TMP/st-joined.tsv" > "$TMP/st-diff.txt"
NCMP=$(grep -c . "$TMP/st-joined.tsv")
[ "$NCMP" = "$NALL" ] && [ ! -s "$TMP/st-diff.txt" ] \
  && ok "all $NCMP exported statements equal Lean's own, in the kernel's view" \
  || no "statement fidelity: $NCMP of $NALL compared, $(grep -c . "$TMP/st-diff.txt") differ: $(head -3 "$TMP/st-diff.txt" | tr '\n' ' ')"

echo "=== C/K: re-check ==="
"$CONLECHE" --verified "$EXPORT" > "$TMP/c.out" 2>&1; rc=$?
[ "$rc" = 0 ] && grep -qE "^con-leche: accepted [0-9]+ declarations \(--verified\)" "$TMP/c.out" \
  && ok "con-leche --verified accepts the standard export ($(grep -oE 'accepted [0-9]+ declarations' "$TMP/c.out"))" \
  || no "con-leche rc=$rc on the standard export: $(grep -v 'stats:' "$TMP/c.out" | tail -3 | tr '\n' ' ')"
# shellcheck disable=SC2086
nanoda "$EXPORT" $STANDARD_AXIOMS > "$TMP/k.out" 2>&1; rc=$?
[ "$rc" = 0 ] && grep -qE "^Checked [0-9]+ declarations with no errors$" "$TMP/k.out" \
  && ok "nanoda accepts the standard export ($(grep -oE '^Checked [0-9]+ declarations' "$TMP/k.out"))" \
  || no "nanoda rc=$rc on the standard export: $(tail -3 "$TMP/k.out" | tr '\n' ' ')"

echo "=== X: hostile control (swapped proof) ==="
read -r RECEIVER DONOR < <(python3 "$INV" swap "$EXPORT" "$SWAP_A" "$SWAP_B" "$TMP/swapped.ndjson")
"$CONLECHE" --verified "$TMP/swapped.ndjson" > "$TMP/cx.out" 2>&1; rc=$?
[ "$rc" = 1 ] && grep -qF "type mismatch in theorem $RECEIVER " "$TMP/cx.out" \
  && ok "con-leche rejects a proof of $DONOR handed in as $RECEIVER as a type mismatch there" \
  || no "con-leche on the swapped export: rc=$rc, expected 1 with a type mismatch at ${RECEIVER:-?}: $(grep -v 'stats:' "$TMP/cx.out" | tail -2 | tr '\n' ' ')"
# shellcheck disable=SC2086
nanoda "$TMP/swapped.ndjson" $STANDARD_AXIOMS > "$TMP/kx.out" 2>&1; rc=$?
[ "$rc" != 0 ] && grep -qF "assertion failed: self.def_eq" "$TMP/kx.out" \
  && ok "nanoda fails the swapped proof at a definitional-equality check (rc=$rc)" \
  || no "nanoda on the swapped export: rc=$rc, expected a def_eq failure: $(tail -3 "$TMP/kx.out" | tr '\n' ' ')"

echo "=== N: native trust ==="
if [ "$NNAT" -gt 0 ]; then
  "$CONLECHE" --verified "$NATIVE_EXPORT" > "$TMP/cn.out" 2>&1; rc=$?
  [ "$rc" = 2 ] && grep -qE "non-standard axiom \([^)]*\._native\.(bv_decide|native_decide)\.ax_" "$TMP/cn.out" \
    && ok "con-leche declines the native export for its ._native axiom (exit 2), never accepts it" \
    || no "con-leche on the native export: rc=$rc, expected 2 declining a ._native axiom: $(grep -v 'stats:' "$TMP/cn.out" | tail -2 | tr '\n' ' ')"
  # shellcheck disable=SC2086
  nanoda "$NATIVE_EXPORT" $STANDARD_AXIOMS > "$TMP/kn.out" 2>&1; rc=$?
  [ "$rc" != 0 ] && grep -qE "unpermitted axiom \"[^\"]*\._native\.(bv_decide|native_decide)\.ax_" "$TMP/kn.out" \
    && ok "nanoda refuses the native export with only the standard axioms permitted (rc=$rc)" \
    || no "nanoda on the native export, standard axioms only: rc=$rc: $(tail -2 "$TMP/kn.out" | tr '\n' ' ')"
  # The permitted set is the exported axioms, each of which must have passed the owner check above.
  python3 "$INV" axioms "$NATIVE_EXPORT" > "$TMP/native-export-axioms.txt"
  STRAY=$(comm -23 "$TMP/native-export-axioms.txt" <(sort -u "$TMP/native-axioms.txt" <(printf '%s\n' $STANDARD_AXIOMS)))
  # shellcheck disable=SC2046
  nanoda "$NATIVE_EXPORT" $(cat "$TMP/native-export-axioms.txt") > "$TMP/kp.out" 2>&1; rc=$?
  [ -z "$STRAY" ] && [ "$rc" = 0 ] && grep -qE "^Checked [0-9]+ declarations with no errors$" "$TMP/kp.out" \
    && ok "nanoda accepts all $NNAT native-trust theorems with only the declared owners' native axioms permitted ($(grep -oE '^Checked [0-9]+ declarations' "$TMP/kp.out"))" \
    || no "nanoda on the native export with its native axioms permitted: rc=$rc${STRAY:+ stray axioms: $(echo $STRAY)}: $(tail -2 "$TMP/kp.out" | tr '\n' ' ')"
else
  ok "no native-trust theorem exists (native legs not applicable)"
fi
finish
