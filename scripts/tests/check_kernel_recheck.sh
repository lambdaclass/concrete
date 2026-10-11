#!/usr/bin/env bash
# FRESH KERNEL RE-CHECK (R-0208 slice 1b, decision 1): a receipt is minted only for a theorem the
# kernel RE-CHECKED in the replay run, not one whose name was merely found in a compiled `.olean`.
#
# The hole, measured 2026-10-11: replay emitted `#check @thm` against object files an earlier
# `lake build` produced. An ILL-TYPED theorem added with the kernel check skipped (`bogus : 1 = 2`
# proved by `Eq.refl 1`) builds into an `.olean` and is "found" by that lookup. The replay process
# now replays every workspace module in each theorem's import closure through the kernel
# (`Lean.Environment.replay'`, the routine `leanchecker` uses), in the SAME process whose identity
# the receipt binds, and minting requires the verdict `rechecked`.
#
# Legs, each with a live positive control:
#   - the ill-typed theorem is still FOUND by the lookup (the hole is real) but fails the re-check,
#     and mints nothing; a genuine theorem in another module re-checks and mints, and so does one in
#     the SAME module (the re-check is per constant, so an unrelated bad declaration is not contagious);
#   - a theorem resting on native evaluation (`bv_decide`) is reported `native_trust`, never
#     re-checked, and mints nothing;
#   - a missing, duplicated or foreign-nonce re-check line is not a verdict.
#
# WHAT IT DOES NOT ESTABLISH: modules shipped with the toolchain are not replayed (they are part of
# the checker build `CheckerIdentity` names); and the verdict line is correlated with the run by
# nonce, not authenticated.
set -uo pipefail
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
BIN="$ROOT_DIR/.lake/build/bin/concrete"
SRC="examples/elf_header/src/main.con"
REAL_LAKE="$(command -v lake)"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

echo "=== an ill-typed theorem in a compiled .olean is found, then refused by the re-check ==="

# The fixture is a SEPARATE Lake workspace outside the repository, so nothing in it can reach the
# compiler or the proof library, and `replay` resolves it from the input path like any program.
KW="$TMP/kernel_recheck"
cp -R scripts/tests/fixtures/kernel_recheck "$KW"
cp lean-toolchain "$KW/lean-toolchain"
: > "$KW/main.con"
if ! (cd "$KW" && lake build Bad Good >"$TMP/kw_build.log" 2>&1); then
  no "the fixture workspace did not build — every leg below would be vacuous ($(tail -2 "$TMP/kw_build.log" | tr '\n' ' '))"
fi
cat > "$TMP/hostile.lean" <<LEAN
import Concrete
open Concrete.Proof
def tg (n : String) : ReplayTarget :=
  { subject := n, theoremName := n, kind := .refinement, origin := .hardcoded, binding := .bound }
#eval show IO Unit from do
  match ← replay { inputPath := "$KW/main.con", imports := ["Bad", "Good"], targets := [tg "bogus", tg "fine", tg "fine_beside"] } with
  | .error e => IO.println s!"REPLAY-REFUSED {e.canonical}"
  | .ok r =>
    for c in r.checks do IO.println s!"LOOKUP {c.target.theoremName} {c.verdict.canonical}"
    for (t, v) in r.recheck do
      IO.println s!"RECHECK {t} {v.canonical} {match v with | .failed w => w | _ => ""}"
    for t in ["bogus", "fine", "fine_beside"] do
      match SuccessfulReplay.of? r t with
      | .ok _ => IO.println s!"TOKEN {t}"
      | .error e => IO.println s!"TOKEN-REFUSED {t} {e.canonical}"
LEAN
H="$(lake env lean "$TMP/hostile.lean" 2>&1 || true)"
if grep -q '^LOOKUP bogus accepted' <<<"$H"; then
  ok "the lookup still FINDS the ill-typed theorem (the hole the re-check closes is real)"
else
  no "the lookup no longer accepts bogus — this control lost its target: $(grep -E '^(LOOKUP|REPLAY)' <<<"$H" | tr '\n' ' ')"
fi
if grep -qE '^RECHECK bogus recheck_failed .*type mismatch' <<<"$H" \
   && grep -q '^TOKEN-REFUSED bogus not_rechecked' <<<"$H"; then
  ok "the kernel re-check refuses it (declaration type mismatch) and no token is minted"
else
  no "the ill-typed theorem was not refused by the re-check: $(grep -E '^(RECHECK|TOKEN)' <<<"$H" | tr '\n' ' ')"
fi
if grep -q '^RECHECK fine rechecked' <<<"$H" && grep -q '^TOKEN fine$' <<<"$H"; then
  ok "a genuine theorem in its own module re-checks and mints (the positive control)"
else
  no "the genuine theorem did not re-check — the refusal above may be refuse-everything: $(grep -E '^(RECHECK|TOKEN)' <<<"$H" | tr '\n' ' ')"
fi
if grep -q '^RECHECK fine_beside rechecked' <<<"$H" && grep -q '^TOKEN fine_beside$' <<<"$H"; then
  ok "a genuine theorem in the SAME module as the bad one re-checks too (per constant, not per module)"
else
  no "a bad declaration took down its module-mate: $(grep -E '^(RECHECK|TOKEN) fine_beside' <<<"$H" | tr '\n' ' ')"
fi

echo "=== a theorem resting on native evaluation is never re-checked ==="

cat > "$TMP/native.lean" <<'LEAN'
import Concrete
open Concrete.Proof
#eval show IO Unit from do
  let t : ReplayTarget := { subject := "s", theoremName := "Examples.HmacSha256.Proofs.sha256_hash_refines_spec"
                          , kind := .refinement, origin := .hardcoded, binding := .bound }
  match ← replay { inputPath := "Main.lean", imports := ["Concrete", "Examples"], targets := [t] } with
  | .error e => IO.println s!"REPLAY-REFUSED {e.canonical}"
  | .ok r =>
    for c in r.checks do IO.println s!"LOOKUP {c.verdict.canonical}"
    for (_, v) in r.recheck do IO.println s!"RECHECK {v.canonical}"
    match SuccessfulReplay.of? r t.theoremName with
    | .ok _ => IO.println "TOKEN"
    | .error e => IO.println s!"TOKEN-REFUSED {e.canonical}"
LEAN
N="$(lake env lean "$TMP/native.lean" 2>&1 || true)"
if grep -q '^LOOKUP accepted' <<<"$N" && grep -q '^RECHECK native_trust' <<<"$N" \
   && grep -q '^TOKEN-REFUSED not_rechecked' <<<"$N"; then
  ok "a bv_decide theorem is found, reported native_trust, and mints nothing"
else
  no "native-trust handling moved: $(grep -E '^(LOOKUP|RECHECK|TOKEN|REPLAY)' <<<"$N" | tr '\n' ' ')"
fi

echo "=== the real corpus still mints, and only with a verdict per theorem ==="

ISSUE="$("$BIN" "$SRC" --report receipts --out "$TMP/r.txt" 2>&1 || true)"
if grep -q 'Summary: 5 issued, 0 withheld' <<<"$ISSUE"; then
  ok "elf_header's five claims re-check and mint"
else
  no "elf_header no longer mints after re-check: $(grep '^Summary' <<<"$ISSUE")"
fi

stub() {  # $1 = name, $2 = sed program applied to the checker's combined output
  mkdir -p "$TMP/stub_$1"
  cat > "$TMP/stub_$1/lake" <<EOF
#!/usr/bin/env bash
set -o pipefail
"$REAL_LAKE" "\$@" 2>&1 | sed -E '$2'
EOF
  chmod +x "$TMP/stub_$1/lake"
}
stub drop '/^CONCRETE-RECHECK /d'
stub dup  '/^CONCRETE-RECHECK /{p;}'
stub foreign 's/^(CONCRETE-RECHECK )([0-9a-f]+) (.*) (failed|native|unknown)(.*)$/\1\2 \3 \4\5\n\1deadbeef \3 rechecked/'
for c in "drop:no re-check verdict" "dup:two re-check verdicts per theorem"; do
  s="${c%%:*}"; why="${c#*:}"
  OUT="$(PATH="$TMP/stub_$s:$PATH" "$BIN" "$SRC" --report receipts --out "$TMP/none_$s.txt" 2>&1 || true)"
  if grep -q 'Summary: 0 issued, 5 withheld' <<<"$OUT" && grep -q 'not re-checked by the kernel' <<<"$OUT"; then
    ok "$why: all five withheld as not re-checked"
  else
    no "$why still minted: $(grep '^Summary' <<<"$OUT")"
  fi
done
# A verdict under some OTHER nonce is ignored: injecting "rechecked" for the bogus theorem under a
# foreign nonce must not rescue it.
FOR="$(PATH="$TMP/stub_foreign:$PATH" lake env lean "$TMP/hostile.lean" 2>&1 || true)"
if grep -q '^TOKEN-REFUSED bogus not_rechecked' <<<"$FOR" && grep -q '^TOKEN fine$' <<<"$FOR"; then
  ok "a 'rechecked' line under a foreign nonce is ignored; the run's own verdict decides"
else
  no "a foreign-nonce verdict moved the outcome: $(grep -E '^(RECHECK|TOKEN)' <<<"$FOR" | tr '\n' ' ')"
fi

GATE_DONE=1
echo "KERNEL-RECHECK: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
