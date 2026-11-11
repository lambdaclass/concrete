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
#   S  declaration fidelity: every declaration of the stream (theorem statements and proofs, the
#      definitions and inductive types they name, Lean core's included) equals Lean's own, in the
#      kernel's view (lean4export is unverified, and con-leche's theorem is about what it was handed,
#      so this is what ties the re-check to what Lean checked);
#   C  con-leche --verified accepts the export;   K  so does nanoda, the standard axioms permitted.
#
# NEGATIVE CONTROLS, each on a corrupted copy of the real export:
#   X  one theorem handed another's proof: con-leche rejects it as a type mismatch AT that theorem,
#      nanoda at its definitional-equality check (the reason is checked, never the exit code alone);
#   T  the export cut inside a record, and D  the export missing one expression record: both refuse;
#   F  a definition's body replaced, or an (anonymous) universe parameter added to one, while every
#      statement stays the same: S reports exactly that definition;
#   N  names whose dotted spellings collide, and a non-ASCII one, are encoded apart and alike by both
#      renderers (every comparison is by that injective encoding).
#
# Tools are built once per pinned SHA into $CONCRETE_EXT_TOOLS (default ~/.cache/concrete-ext):
# lean4export and con-leche with their declared, pinned toolchains (explicit elan dispatch; the
# exporter must match this project to read its .olean files), nanoda with cargo. CI runs this
# script in `nix develop .#extcheck`, which adds cargo, rustc and elan to the gate userland.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/../checks/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CHECK_NAME=PROOF-EXTERNAL
source scripts/proofs/lib.sh
# A pre-push hook in a linked worktree exports repository-local Git variables; without clearing
# them, `git -C "$tool" rev-parse HEAD` reads this repository's HEAD instead of the tool's
# (the sandbox helper, ISS-0015).
source scripts/checks/lib/git_sandbox.sh || exit 2
git_sandbox || exit 2

# shellcheck source=external.pins
. scripts/proofs/external.pins
EXPECT_FORMAT=3.1.0
INV="python3 scripts/proofs/export_inventory.py"
TOOLS="${CONCRETE_EXT_TOOLS:-$HOME/.cache/concrete-ext}"
TOOLCHAIN="$(cat lean-toolchain)"

# The cache is shared by every run of this user on the machine (a self-hosted runner's HOME persists,
# and worktrees share it): building holds an exclusive lock on it, builds in a private directory and
# publishes the finished one with a rename, so no run sees, or removes, another's half-built tool.
mkdir -p "$TOOLS" && exec 9> "$TOOLS/.lock" && flock 9 \
  || { echo "error: cannot lock the tool cache $TOOLS" >&2; exit 2; }
# publish DIR BUILD-COMMAND… → runs the command in DIR.partial.$$ (already cloned) and renames it to DIR
publish() {
  local dir=$1 work="$1.partial.$$"; shift
  (cd "$work" && "$@" >&2) && touch "$work/.built" && rm -rf "$dir" && mv "$work" "$dir"
}
. scripts/proofs/lake_tool.sh

echo "=== P: pinned tools ==="
[ "$LEAN4EXPORT_TOOLCHAIN" = "$TOOLCHAIN" ] \
  || { no "exporter pin $LEAN4EXPORT_TOOLCHAIN differs from project $TOOLCHAIN"; finish; }
EXPORT_DIR=$(build_lake_tool lean4export "$LEAN4EXPORT_URL" "$LEAN4EXPORT_REV" "$LEAN4EXPORT_TOOLCHAIN" lean4export) \
  && ok "lean4export at $LEAN4EXPORT_REV, built with $TOOLCHAIN" \
  || { no "could not build lean4export at $LEAN4EXPORT_REV"; finish; }
CHECK_DIR=$(build_lake_tool con-leche "$CONLECHE_URL" "$CONLECHE_REV" "$CONLECHE_TOOLCHAIN" con-leche) \
  && ok "con-leche at $CONLECHE_REV, built with $CONLECHE_TOOLCHAIN" \
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
  W="$NANODA_DIR.partial.$$"; rm -rf "$W"
  # The thread stack is raised from 16 MiB to 256 MiB exactly as the Lean Kernel Arena patches it.
  { git clone -q "$NANODA_URL" "$W" && git -C "$W" checkout -q "$NANODA_REV" \
    && has_match -F 'STACK_SIZE: usize = 16_777_216;' < "$W/src/lib.rs" \
    && sed -i.orig 's/STACK_SIZE: usize = 16_777_216;/STACK_SIZE: usize = 268_435_456;/' "$W/src/lib.rs" \
    && publish "$NANODA_DIR" cargo build --release -q; } >&2 || rm -rf "$W"
fi
NANODA="$NANODA_DIR/target/release/nanoda_bin"
[ -x "$NANODA" ] && [ "$(git -C "$NANODA_DIR" rev-parse HEAD)" = "$NANODA_REV" ] \
  && ok "nanoda at $NANODA_REV (thread stack patched as the Arena does)" \
  || { no "could not build nanoda at $NANODA_REV"; finish; }
flock -u 9   # every tool is built and verified; the rest only reads them
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
# Names: lean4export reads them as Lean writes them (`«…»` where needed); everything is compared by
# the injective encoding both theorems.lean and export_inventory.py produce (PROOF_MODE=encoded).
population encoded > "$TMP/encoded.tsv" 2>> "$TMP/inv.err" || { no "PROOF_MODE=encoded failed"; finish; }
cut -f1 "$TMP/encoded.tsv" > "$TMP/population.txt"
cut -f2 "$TMP/encoded.tsv" | sort -u > "$TMP/population-enc.txt"
[ "$NALL" -ge 100 ] && [ -z "$NONSTD" ] && ok "$NALL theorems, all on the standard axioms" \
  || no "population: $NALL theorems; on a non-standard axiom: $(echo $NONSTD | cut -c1-300)"

echo "=== A: export ==="
EXPORT="$TMP/spec.ndjson"
# lean4export can panic on a name it cannot resolve and still exit 0, so its stderr must be empty.
# One argument per name, quoted: a name such as `«foo bar»` holds a space and `«*»` a glob.
mapfile -t POP < "$TMP/population.txt"
"$LAKE" env "$EXPORTER" ConcreteSpec -- "${POP[@]}" > "$EXPORT" 2> "$TMP/export.err"; rc=$?
[ "$rc" = 0 ] && [ ! -s "$TMP/export.err" ] && ok "lean4export exported the population (no diagnostics)" \
  || { no "lean4export rc=$rc: $(head -3 "$TMP/export.err" | tr '\n' ' ')"; finish; }
read -r EXP_LEAN EXP_TOOL EXP_FORMAT < <($INV meta "$EXPORT")
[ "leanprover/lean4:v$EXP_LEAN" = "$TOOLCHAIN" ] && [ "$EXP_FORMAT" = "$EXPECT_FORMAT" ] \
  && ok "export from Lean $EXP_LEAN (lean4export $EXP_TOOL, format $EXP_FORMAT)" \
  || no "export metadata: Lean $EXP_LEAN format $EXP_FORMAT, expected $TOOLCHAIN format $EXPECT_FORMAT"
$INV theorems "$EXPORT" > "$TMP/exported.txt" \
  || { no "the stream's theorems could not be listed"; finish; }
MISSING=$(comm -23 "$TMP/population-enc.txt" <(sort -u "$TMP/exported.txt"))
[ "$(grep -c . "$TMP/population-enc.txt")" = "$NALL" ] && [ -z "$MISSING" ] && ok "every theorem of the population is in the export ($(grep -c . "$TMP/exported.txt") theorems with dependencies)" \
  || no "theorems missing from the export: $(echo $MISSING | cut -c1-300)"
$INV axioms "$EXPORT" > "$TMP/axioms.txt" \
  || { no "the stream's axioms could not be listed"; finish; }
EXTRA=$(comm -23 "$TMP/axioms.txt" <(printf '%s\n' $STANDARD_AXIOMS))
[ -z "$EXTRA" ] && ok "the export declares only $(echo $(cat "$TMP/axioms.txt"))" \
  || no "the export declares non-standard axioms: $(echo $EXTRA)"

echo "=== S: declaration fidelity ==="
# Every declaration in the stream (the population and everything it depends on, Lean core included:
# types, definition bodies, proofs, inductive types, constructors, recursors) against Lean's own
# environment. A statement can only be trusted together with the definitions it names: a stream that
# kept a theorem's type but changed a definition it mentions would prove another proposition.
$INV names "$EXPORT" > "$TMP/names.txt" \
  || { no "the stream's declaration names could not be listed"; finish; }
PROOF_NAMES="$TMP/names.txt" PROOF_OUT="$TMP/decls-lean.txt" population decls > "$TMP/decls.err" 2>&1 \
  || { no "declaration rendering failed: $(head -3 "$TMP/decls.err" | tr '\n' ' ')"; finish; }
$INV decls "$EXPORT" > "$TMP/decls-export.txt" \
  || { no "the stream's declarations could not be rendered"; finish; }
# fidelity LEAN EXPORT → empty when equal, else the first declaration whose rendering differs
fidelity() {
  cmp -s "$1" "$2" && return 0
  local line; line=$(cmp "$1" "$2" 2>/dev/null | sed -nE 's/.* line ([0-9]+).*/\1/p')
  [ -n "$line" ] || line=$(( $(grep -c '' "$1") + 1 ))     # one is a prefix of the other
  tail -n +"$line" "$2" | grep -m1 '^D' | cut -f2
  [ -n "$(tail -n +"$line" "$2" | grep -m1 '^D')" ] || echo "(end of stream)"
}
NDECL=$(grep -c '^D' "$TMP/decls-export.txt")
FD=$(fidelity "$TMP/decls-lean.txt" "$TMP/decls-export.txt")
[ "$NDECL" = "$(grep -c . "$TMP/names.txt")" ] && [ -z "$FD" ] \
  && ok "all $NDECL exported declarations equal Lean's own, in the kernel's view ($(grep -vc '^D' "$TMP/decls-export.txt") shared nodes)" \
  || no "declaration fidelity: $NDECL rendered of $(grep -c . "$TMP/names.txt"); first differing: ${FD:-none}"

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
# The pair must have statements that are not definitionally equal, or a correct checker would
# accept the swap: different statements as written are not enough (`True` and `id True` are
# equal). So the pair is the first two universe-monomorphic theorems of the population whose
# statements are applications of DIFFERENT inductive types (PROOF_MODE=heads): such statements are
# in weak head normal form, and no reduction makes them equal.
population heads > "$TMP/heads.tsv" 2> "$TMP/heads.err" || no "PROOF_MODE=heads failed: $(head -2 "$TMP/heads.err" | tr '\n' ' ')"
read -r SWAP_A SWAP_B < <(sort "$TMP/heads.tsv" | awk -F'\t' '!a { a = $1; h = $2; next } $2 != h { print a, $1; exit }')
read -r RECEIVER DONOR _ _ < <($INV swap "$EXPORT" "$SWAP_A" "$SWAP_B" "$TMP/swapped.ndjson")
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

echo "=== F: declaration fidelity controls, a declaration changed under unchanged statements ==="
# forge MODE OUT → copies the export to OUT with the first definition of the population's own library
# changed, and prints its encoded name. body: its body becomes its type. arity: a definition with no
# universe parameter gets one, the anonymous name (encoded as the empty string, so a list that did
# not delimit its names would render [] and [anonymous] alike). No theorem's statement changes.
forge() {
  python3 - "$EXPORT" "$2" "$1" <<'PY'
import json, sys
sys.path.insert(0, "scripts/proofs")
from export_inventory import name_table, records
names = name_table(sys.argv[1], encoded=True); done = None; mode = sys.argv[3]
with open(sys.argv[2], "w", encoding="utf-8") as o:
    for r in records(sys.argv[1]):
        d = r.get("def")
        if done is None and d and names[d["name"]].startswith("s12:ConcreteSpec") \
                and (mode == "body" or d["levelParams"] == []):
            if mode == "body": d["value"] = d["type"]
            else: d["levelParams"] = [0]
            done = names[d["name"]]
        o.write(json.dumps(r, separators=(",", ":")) + "\n")
print(done or "")
PY
}
for mode in body arity; do
  FORGED=$(forge $mode "$TMP/forged-$mode.ndjson")
  $INV decls "$TMP/forged-$mode.ndjson" > "$TMP/decls-forged-$mode.txt"
  FF=$(fidelity "$TMP/decls-lean.txt" "$TMP/decls-forged-$mode.txt")
  [ -n "$FORGED" ] && [ "$FF" = "$FORGED" ] \
    && ok "a definition with a forged $mode ($FORGED) is reported, though no statement changed" \
    || no "the fidelity comparison on a forged $mode (${FORGED:-none}) reported '${FF:-nothing}'"
done
echo "=== N: name encoding control ==="
# Dotted names are not injective (`a.b` as one component and as two print alike), and stripping `«»`
# is not safe on UTF-8 under LC_ALL=C. Both encoders must give these three names three different
# encodings, and the same ones.
printf '%s\n' 'theorem ConcreteSpecControl.«a.b» : True := trivial' \
  'theorem ConcreteSpecControl.a.b : True := trivial' 'theorem ConcreteSpecControl.«λ» : True := trivial' > "$TMP/c-names.lean"
population encoded "$TMP/c-names.lean" 2>/dev/null | grep -F 'ConcreteSpecControl' | cut -f2 | sort > "$TMP/enc-lean.txt"
printf '%s\n' '{"in":1,"str":{"pre":0,"str":"ConcreteSpecControl"}}' '{"in":2,"str":{"pre":1,"str":"a.b"}}' \
  '{"in":3,"str":{"pre":1,"str":"a"}}' '{"in":4,"str":{"pre":3,"str":"b"}}' '{"in":5,"str":{"pre":1,"str":"λ"}}' > "$TMP/names.ndjson"
$INV encode "$TMP/names.ndjson" | sed -n '2p;4p;5p' | sort > "$TMP/enc-py.txt"
[ "$(sort -u "$TMP/enc-lean.txt" | grep -c .)" = 3 ] && cmp -s "$TMP/enc-lean.txt" "$TMP/enc-py.txt" \
  && ok "«a.b», a.b and «λ» get three distinct encodings, the same in Lean and in the stream reader" \
  || no "name encodings: Lean [$(echo $(cat "$TMP/enc-lean.txt"))] stream [$(echo $(cat "$TMP/enc-py.txt"))]"
# Every output of both readers is line-based (most tab-separated), so a name holding a tab or a line
# break could shift or split a row (an axiom list gaining an allowlisted line). Both refuse it.
printf 'theorem ConcreteSpecControl.«a\tb» : True := trivial\n' > "$TMP/c-tab.lean"
population heads "$TMP/c-tab.lean" > "$TMP/c-tab.out" 2>&1 && LRC=0 || LRC=$?
printf '%s\n' '{"in":1,"str":{"pre":0,"str":"ConcreteSpecControl"}}' \
  '{"in":2,"str":{"pre":1,"str":"Classical.choice\nQuot.sound"}}' > "$TMP/c-nl.ndjson"
$INV encode "$TMP/c-nl.ndjson" > "$TMP/c-nl.out" 2>&1 && PRC=0 || PRC=$?
[ "$LRC" != 0 ] && grep -q 'control character' "$TMP/c-tab.out" && [ "$PRC" != 0 ] && grep -q 'control character' "$TMP/c-nl.out" \
  && ok "a name holding a tab (Lean, heads mode) or a line break (stream reader) fails the run" \
  || no "control-character names: Lean rc=$LRC '$(head -c 200 "$TMP/c-tab.out")', stream rc=$PRC '$(head -c 200 "$TMP/c-nl.out")'"
finish
