#!/usr/bin/env bash
# INDEPENDENT RE-CHECK: every theorem of the specification exported with lean4export and re-checked
# by two checkers that share no code with the Lean kernel or with each other.
#
#   con-leche (leanprover/con-leche, Lean): machine-checked consistency proof: an accepted stream has
#     a set-theoretic model, so it contains no proof of False. Run in --verified, the mode the
#     theorem is about, single-threaded (--jobs=1: Lean 4.35.0's lean4#15288 fixed a thread-sharing
#     bug that named Lean-based checkers such as con-leche as possibly affected).
#   nanoda (ammkrn/nanoda_lib, Rust): no consistency proof; implementation diversity.
#
#   P  the three tools are the revisions pinned in scripts/proofs/external.pins, and every con-leche
#      call goes through one function that passes --verified --jobs=1;
#   I  population: every theorem scripts/proofs/theorems.lean lists, all of them on the standard
#      axioms (a theorem on any other is refused here, not left out of the export);
#   A  the export is from this toolchain, holds every theorem of the population, and declares only
#      the three standard axioms;
#   S  statement fidelity: every exported statement equals the statement in Lean's own environment,
#      in the kernel's view (lean4export is unverified, and con-leche's theorem is about what it was
#      handed, so this is what ties the re-check to what Lean checked);
#   C  con-leche --verified accepts the export;   K  so does nanoda, the standard axioms permitted.
#
# NEGATIVE CONTROLS, each on a corrupted copy of the real export:
#   X  one theorem handed another's proof: con-leche rejects it as a type mismatch AT that theorem,
#      nanoda at its definitional-equality check (the reason is checked, never the exit code alone);
#   T  the export cut inside a record, and D  the export missing one expression record: both refuse;
#   F  a statement rewritten in the comparison input: the fidelity comparison reports exactly it.
#
# Tools are built once per pinned SHA into $CONCRETE_EXT_TOOLS (default ~/.cache/concrete-ext):
# lean4export with THIS toolchain (it reads our .olean files), con-leche with its own (elan installs
# it), nanoda with cargo (CI runs this script in `nix develop .#extcheck`, which adds cargo and rustc
# to the gate userland).
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK_NAME=PROOF-EXTERNAL
source scripts/proofs/lib.sh
# A pre-push hook in a linked worktree exports repository-local Git variables; without clearing
# them, `git -C "$tool" rev-parse HEAD` reads this repository's HEAD instead of the tool's.
for git_local_var in $(git rev-parse --local-env-vars); do unset "$git_local_var"; done

# shellcheck source=external.pins
. scripts/proofs/external.pins
EXPECT_FORMAT=3.1.0
INV="python3 scripts/proofs/export_inventory.py"
TOOLS="${CONCRETE_EXT_TOOLS:-$HOME/.cache/concrete-ext}"
TOOLCHAIN="$(cat lean-toolchain)"

# build_lake_tool NAME URL REV TOOLCHAIN-OVERRIDE TARGET → prints the source dir; builds once per SHA
build_lake_tool() {
  local name=$1 url=$2 rev=$3 tc=$4 target=$5
  # The toolchain override is part of the build's identity (an exporter built with another toolchain
  # cannot read this build's .olean files), so it names the directory alongside the SHA.
  local dir="$TOOLS/$name-$rev${tc:+-$(printf '%s' "$tc" | tr -c 'A-Za-z0-9._-' '_')}"
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
CONLECHE_FLAGS=(--verified --jobs=1)
conleche() { "$CONLECHE" "${CONLECHE_FLAGS[@]}" "$1"; }
# Every con-leche run goes through conleche(): a direct call would run at the default worker count.
DIRECT=$(grep -nE '^[^#]*"\$CONLECHE"' "$0" | grep -v 'CONLECHE_FLAGS\[@\]')
[ "${CONLECHE_FLAGS[*]}" = "--verified --jobs=1" ] && [ -z "$DIRECT" ] \
  && ok "con-leche runs as --verified --jobs=1 at every call site" \
  || no "con-leche flags are [${CONLECHE_FLAGS[*]}]; direct calls: $(echo $DIRECT | cut -c1-200)"
NANODA_DIR="$TOOLS/nanoda-$NANODA_REV"
if [ ! -f "$NANODA_DIR/.built" ]; then
  command -v cargo >/dev/null || { echo "error: building nanoda needs cargo (nix develop .#extcheck)" >&2; exit 2; }
  rm -rf "$NANODA_DIR"
  # The thread stack is raised from 16 MiB to 256 MiB exactly as the Lean Kernel Arena patches it.
  { git clone -q "$NANODA_URL" "$NANODA_DIR" && git -C "$NANODA_DIR" checkout -q "$NANODA_REV" \
    && grep -q 'STACK_SIZE: usize = 16_777_216;' "$NANODA_DIR/src/lib.rs" \
    && sed -i.orig 's/STACK_SIZE: usize = 16_777_216;/STACK_SIZE: usize = 268_435_456;/' "$NANODA_DIR/src/lib.rs" \
    && (cd "$NANODA_DIR" && cargo build --release -q) && touch "$NANODA_DIR/.built"; } >&2
fi
NANODA="$NANODA_DIR/target/release/nanoda_bin"
[ -x "$NANODA" ] && [ "$(git -C "$NANODA_DIR" rev-parse HEAD)" = "$NANODA_REV" ] \
  && ok "nanoda at $NANODA_REV (thread stack patched as the Arena does)" \
  || { no "could not build nanoda at $NANODA_REV"; finish; }
# nanoda FILE → runs nanoda on FILE permitting exactly the standard axioms
nanoda() {
  python3 -c 'import json,sys; print(json.dumps({"export_file_path": sys.argv[1],
    "permitted_axioms": sys.argv[2:], "unpermitted_axiom_hard_error": True,
    "nat_extension": True, "string_extension": True, "num_threads": 4,
    "print_axioms": False, "print_success_message": True}))' "$1" $STANDARD_AXIOMS > "$TMP/nanoda.json"
  "$NANODA" "$TMP/nanoda.json"
}

echo "=== I: population ==="
population theorems > "$TMP/inv.tsv" 2> "$TMP/inv.err" \
  || { no "the enumerator failed: $(head -3 "$TMP/inv.err" | tr '\n' ' ')"; finish; }
NALL=$(grep -c . "$TMP/inv.tsv")
NONSTD=$(awk -F'\t' -v std=" $STANDARD_AXIOMS " '{ n = split($2, a, " ")
  for (i = 1; i <= n; i++) if (index(std, " " a[i] " ") == 0) { print $1; break } }' "$TMP/inv.tsv")
cut -f1 "$TMP/inv.tsv" > "$TMP/population.txt"
[ "$NALL" -ge 100 ] && [ -z "$NONSTD" ] && ok "$NALL theorems, all on the standard axioms" \
  || no "population: $NALL theorems; on a non-standard axiom: $(echo $NONSTD | cut -c1-300)"

echo "=== A: export ==="
EXPORT="$TMP/spec.ndjson"
# shellcheck disable=SC2046
"$LAKE" env "$EXPORTER" ConcreteSpec -- $(cat "$TMP/population.txt") > "$EXPORT" 2> "$TMP/export.err" \
  || { no "lean4export failed: $(head -3 "$TMP/export.err" | tr '\n' ' ')"; finish; }
read -r EXP_LEAN EXP_TOOL EXP_FORMAT < <($INV meta "$EXPORT")
[ "leanprover/lean4:v$EXP_LEAN" = "$TOOLCHAIN" ] && [ "$EXP_FORMAT" = "$EXPECT_FORMAT" ] \
  && ok "export from Lean $EXP_LEAN (lean4export $EXP_TOOL, format $EXP_FORMAT)" \
  || no "export metadata: Lean $EXP_LEAN format $EXP_FORMAT, expected $TOOLCHAIN format $EXPECT_FORMAT"
$INV theorems "$EXPORT" > "$TMP/exported.txt"
MISSING=$(sort "$TMP/population.txt" | comm -23 - "$TMP/exported.txt")
[ -z "$MISSING" ] && ok "every theorem of the population is in the export ($(grep -c . "$TMP/exported.txt") theorems with dependencies)" \
  || no "theorems missing from the export: $(echo $MISSING | cut -c1-300)"
$INV axioms "$EXPORT" > "$TMP/axioms.txt"
EXTRA=$(comm -23 "$TMP/axioms.txt" <(printf '%s\n' $STANDARD_AXIOMS))
[ -z "$EXTRA" ] && ok "the export declares only $(echo $(cat "$TMP/axioms.txt"))" \
  || no "the export declares non-standard axioms: $(echo $EXTRA)"

echo "=== S: statement fidelity ==="
population statements > "$TMP/st-lean.tsv" 2> "$TMP/st.err" \
  || { no "statement rendering failed: $(head -3 "$TMP/st.err" | tr '\n' ' ')"; finish; }
$INV statements "$EXPORT" | sort -u > "$TMP/st-export.tsv"
# fidelity LEAN EXPORT → `<compared>\t<differing names>`; only the population is compared (the
# export also carries Lean core's theorems as dependencies)
fidelity() {
  join -t$'\t' <(sort "$1") "$2" > "$TMP/st-joined.tsv"
  printf '%s\t%s\n' "$(grep -c . "$TMP/st-joined.tsv")" "$(awk -F'\t' '$2!=$4 || $3!=$5 {print $1}' "$TMP/st-joined.tsv" | tr '\n' ' ')"
}
IFS=$'\t' read -r NCMP SDIFF < <(fidelity "$TMP/st-lean.tsv" "$TMP/st-export.tsv")
[ "$NCMP" = "$NALL" ] && [ -z "${SDIFF// }" ] \
  && ok "all $NCMP exported statements equal Lean's own, in the kernel's view" \
  || no "statement fidelity: $NCMP of $NALL compared, differing: $(echo $SDIFF | cut -c1-300)"

echo "=== C/K: re-check ==="
t0=$SECONDS; conleche "$EXPORT" > "$TMP/c.out" 2>&1; rc=$?
[ "$rc" = 0 ] && has_match -E "^con-leche: accepted [0-9]+ declarations \(--verified\)" < "$TMP/c.out" \
  && ok "con-leche --verified --jobs=1 accepts the export ($(grep -oE 'accepted [0-9]+ declarations' "$TMP/c.out"), $((SECONDS-t0))s)" \
  || no "con-leche rc=$rc on the export: $(grep -v 'stats:' "$TMP/c.out" | tail -3 | tr '\n' ' ')"
nanoda "$EXPORT" > "$TMP/k.out" 2>&1; rc=$?
[ "$rc" = 0 ] && has_match -E "^Checked [0-9]+ declarations with no errors$" < "$TMP/k.out" \
  && ok "nanoda accepts the export ($(grep -oE '^Checked [0-9]+ declarations' "$TMP/k.out"))" \
  || no "nanoda rc=$rc on the export: $(tail -3 "$TMP/k.out" | tr '\n' ' ')"

echo "=== X: corrupted export, a theorem handed another's proof ==="
# The pair: the first two universe-monomorphic theorems of the population with different statements.
read -r SWAP_A SWAP_B < <(awk -F'\t' '$2 == "" { if (!a) { a = $1; s = $3 } else if ($3 != s) { print a, $1; exit } }' \
  <(sort "$TMP/st-lean.tsv"))
read -r RECEIVER DONOR < <($INV swap "$EXPORT" "$SWAP_A" "$SWAP_B" "$TMP/swapped.ndjson")
conleche "$TMP/swapped.ndjson" > "$TMP/cx.out" 2>&1; rc=$?
[ -n "${RECEIVER:-}" ] && [ "$rc" = 1 ] && has_match -F "type mismatch in theorem $RECEIVER " < "$TMP/cx.out" \
  && ok "con-leche rejects a proof of $DONOR handed in as $RECEIVER, as a type mismatch there" \
  || no "con-leche on the swapped export: rc=$rc, expected 1 with a type mismatch at ${RECEIVER:-?}: $(grep -v 'stats:' "$TMP/cx.out" | tail -2 | tr '\n' ' ')"
nanoda "$TMP/swapped.ndjson" > "$TMP/kx.out" 2>&1; rc=$?
[ "$rc" != 0 ] && has_match -F "assertion failed: self.def_eq" < "$TMP/kx.out" \
  && ok "nanoda fails the swapped proof at a definitional-equality check (rc=$rc)" \
  || no "nanoda on the swapped export: rc=$rc, expected a def_eq failure: $(tail -3 "$TMP/kx.out" | tr '\n' ' ')"

echo "=== T/D: corrupted export, cut mid-record and missing a record ==="
# A prefix that ends at a record boundary is itself a valid stream (lean4export writes in dependency
# order) and both checkers rightly accept it; that is why A requires every theorem to be exported. The
# corruptions here are a stream cut inside a record and a stream missing one expression record the
# rest refers to.
python3 - "$EXPORT" "$TMP/cut.ndjson" "$TMP/dropped.ndjson" <<'PY'
import json, sys
data = open(sys.argv[1], "rb").read()
cut = len(data) // 2
while data[cut - 1:cut] in (b"\n", b"}"): cut -= 1          # end inside a record, never at its end
open(sys.argv[2], "wb").write(data[:cut])
lines = data.decode("utf-8").splitlines(keepends=True)
exprs = [i for i, l in enumerate(lines) if '"ie"' in l and "ie" in json.loads(l)]
del lines[exprs[len(exprs) // 2]]
open(sys.argv[3], "w", encoding="utf-8").write("".join(lines))
PY
for c in cut dropped; do
  conleche "$TMP/$c.ndjson" > "$TMP/c-$c.out" 2>&1; rc=$?
  [ "$rc" != 0 ] && ! has_match -E "^con-leche: accepted" < "$TMP/c-$c.out" \
    && ok "con-leche refuses the $c export (rc=$rc)" \
    || no "con-leche on the $c export: rc=$rc: $(tail -2 "$TMP/c-$c.out" | tr '\n' ' ')"
  nanoda "$TMP/$c.ndjson" > "$TMP/k-$c.out" 2>&1; rc=$?
  [ "$rc" != 0 ] && ! has_match -E "^Checked [0-9]+ declarations with no errors$" < "$TMP/k-$c.out" \
    && ok "nanoda refuses the $c export (rc=$rc)" \
    || no "nanoda on the $c export: rc=$rc: $(tail -2 "$TMP/k-$c.out" | tr '\n' ' ')"
done

echo "=== F: statement fidelity control ==="
awk -F'\t' -v n="$SWAP_A" 'BEGIN{OFS="\t"} $1 == n { $3 = "(c 4:True [])" } { print }' "$TMP/st-export.tsv" > "$TMP/st-forged.tsv"
IFS=$'\t' read -r _ FDIFF < <(fidelity "$TMP/st-lean.tsv" "$TMP/st-forged.tsv")
[ "${FDIFF% }" = "$SWAP_A" ] \
  && ok "a rewritten exported statement ($SWAP_A) is reported, and only it" \
  || no "the fidelity comparison on a rewritten statement reported '$FDIFF'"
finish
