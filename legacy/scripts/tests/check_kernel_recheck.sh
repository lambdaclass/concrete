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
# Legs, each with a live positive control (plus: a theorem resting on a DECLARED AXIOM replays
# fine — the kernel takes axioms on trust — so axioms are allow-listed to the kernel's standard three):
#   - the ill-typed theorem is still FOUND by the lookup (the hole is real) but fails the re-check,
#     and mints nothing; a genuine theorem in another module re-checks and mints, and so does one in
#     the SAME module (the re-check is per constant, so an unrelated bad declaration is not contagious);
#   - a theorem resting on native evaluation (`bv_decide`) is reported `native_trust`, never
#     re-checked, and mints nothing;
#   - a missing, duplicated or foreign-nonce re-check line is not a verdict;
#   - native trust is classified identically on Lean 4.34.1 and Lean 4.35 (which removes
#     Lean.ofReduceBool/ofReduceNat/trustCompiler): no Lean source resolves a removed name, and the
#     probe's classifier, compiled alone on each toolchain, reads real native proofs as native and
#     accounts for every axiom the toolchain declares.
#
# WHAT IT DOES NOT ESTABLISH: modules shipped with the toolchain are not replayed (they are part of
# the checker build `CheckerIdentity` names); and the verdict line is correlated with the run by
# nonce, not authenticated.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
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
  match ← replay { inputPath := "$KW/main.con", imports := ["Bad", "Good"], targets := [tg "bogus", tg "fine", tg "fine_beside", tg "uses_axiom"] } with
  | .error e => IO.println s!"REPLAY-REFUSED {e.canonical}"
  | .ok r =>
    for c in r.checks do IO.println s!"LOOKUP {c.target.theoremName} {c.verdict.canonical}"
    for (t, v) in r.recheck do
      IO.println s!"RECHECK {t} {v.canonical} {match v with | .failed w => w | _ => ""}"
    for t in ["bogus", "fine", "fine_beside", "uses_axiom"] do
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

# A DECLARED AXIOM IS NOT A PROOF. Replay succeeds (the kernel takes an axiom on trust), so a re-check
# that only replayed would call this theorem rechecked; the axiom allow-list must refuse it by name.
if grep -qE "^RECHECK uses_axiom recheck_failed .*rests on axioms beyond .*cheat" <<<"$H" \
   && grep -q '^TOKEN-REFUSED uses_axiom not_rechecked' <<<"$H"; then
  ok "a theorem resting on a declared axiom is refused, naming the axiom"
else
  no "a theorem resting on a declared axiom was not refused: $(grep -E '^(RECHECK|TOKEN) uses_axiom' <<<"$H" | tr '\n' ' ')"
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

echo "=== native trust is classified the same on Lean 4.34.1 and Lean 4.35 ==="
# Lean 4.35 removes Lean.ofReduceBool, Lean.ofReduceNat and Lean.trustCompiler (lean4#14953); native
# evaluation is then only the per-use `<decl>._native.<tactic>.ax_<n>` axioms, which 4.34 already
# mints. The replay probe is Lean source elaborated by whichever toolchain runs it, so a resolved
# (double-backtick) reference to a removed name would stop it compiling on 4.35 and every theorem
# would read notRun. The classifier the probe uses (Replay.nativeAxiomClassifierSrc) is compiled here
# on its own, under this repository's toolchain and under Lean 4.35 when that toolchain is installed,
# against real native_decide / decide +native / bv_decide proofs and the toolchain's whole axiom set.
SCAN=scripts/tools/removed_native_names.py
if python3 -I "$SCAN" --selftest >/dev/null; then
  HITS="$(git ls-files '*.lean' | xargs python3 -I "$SCAN" 2>&1 || true)"
  [ -z "$HITS" ] && ok "no Lean source (probes held in strings and comments included) names a Lean 4.35-removed constant except as a single-backtick literal" \
    || no "Lean 4.35-removed native names resolved (use a single-backtick literal): $(head -3 <<<"$HITS" | tr '\n' ' ')"
else
  no "the removed-name scanner fails its own self-test: $(python3 -I "$SCAN" --selftest 2>&1 | head -2 | tr '\n' ' ')"
fi
cat > "$TMP/classifier_dump.lean" <<'LEAN'
import Concrete.Proof.Replay
#eval IO.print Concrete.Proof.nativeAxiomClassifierSrc
LEAN
CLS="$(lake env lean "$TMP/classifier_dump.lean" 2>&1 || true)"
if grep -q '^def _concreteIsNativeAxiom' <<<"$CLS"; then
  { printf 'import Lean\nimport Std.Tactic.BVDecide\n'; printf '%s\n' "$CLS"; cat <<'LEAN'
theorem _t_nd : 2 + 2 = 4 := by native_decide
theorem _t_dn : 3 * 3 = 9 := by decide +native
theorem _t_bv (x : BitVec 8) : (x &&& 0x0F) + (x &&& 0xF0) = x := by bv_decide
theorem _t_std (p : Prop) : p ∨ ¬p := Classical.em p
open Lean Elab Command in
#eval show CommandElabM Unit from do
  let env ← getEnv
  IO.println s!"VERSION {Lean.versionString}"
  for t in [`_t_nd, `_t_dn, `_t_bv, `_t_std] do
    let axs ← liftCoreM <| Lean.collectAxioms t
    let nat := axs.filter _concreteIsNativeAxiom
    IO.println s!"CASE {t} native={nat.size} total={axs.size} {" ".intercalate (nat.toList.map toString)}"
  for (n, ci) in env.constants.toList do
    if let .axiomInfo _ := ci then
      unless n.toString.startsWith "_t_" do
        IO.println s!"AXIOM {n} {if _concreteIsNativeAxiom n then "native" else "other"}"
  for n in _concreteLegacyNativeAxioms do
    IO.println s!"LEGACY {n} {env.contains n}"
LEAN
  } > "$TMP/classifier_test.lean"
  # Axioms a toolchain declares that are NOT native evaluation: the kernel's three, sorryAx, and the
  # compiler's internal ones. The replay refuses a theorem resting on any but the three (fail closed).
  # A toolchain axiom outside this list and outside the classifier fails here: a new spelling of
  # native trust must be classified deliberately, never inherited as "some other axiom".
  OTHER_AXIOMS="Classical.choice Quot.sound propext sorryAx Quot.lcInv isScalarObj lcAny lcCast lcErased lcProof lcUnreachable lcVoid"
  # classify_on LABEL LEAN-COMMAND... → runs the test file and judges it; LEGACY_WANT is true|false
  classify_on() {
    local label=$1 want=$2; shift 2
    local out; out="$("$@" "$TMP/classifier_test.lean" 2>&1 || true)"
    local cases_ok=1 c
    # Each proof's native axiom is the one nativeEqTrue mints for ITS tactic (the measured spelling on
    # both toolchains), and is the only native axiom it rests on. A bv_decide goal its preprocessing
    # closes mints none (x &&& x = x does), so this one reaches the SAT solver.
    for c in _t_nd:native_decide _t_dn:decide _t_bv:bv_decide; do
      grep -qE "^CASE ${c%%:*} native=1 total=[1-9][0-9]* ${c%%:*}\._native\.${c#*:}\.ax_[0-9_]+$" <<<"$out" || cases_ok=0
    done
    grep -qE '^CASE _t_std native=0 total=3 ?$' <<<"$out" || cases_ok=0
    local native other legacy
    native="$(sed -n 's/^AXIOM \(.*\) native$/\1/p' <<<"$out" | sort)"
    other="$(sed -n 's/^AXIOM \(.*\) other$/\1/p' <<<"$out" | sort)"
    legacy="$(sed -n 's/^LEGACY \(.*\) \(true\|false\)$/\1 \2/p' <<<"$out")"
    local stray; stray="$(comm -23 <(echo "$other") <(printf '%s\n' $OTHER_AXIOMS | sort) | tr '\n' ' ')"
    local declared; declared="$(awk '$2=="true"{print $1}' <<<"$legacy" | sort)"
    local legacy_ok=1
    [ "$(grep -c . <<<"$legacy")" = 3 ] || legacy_ok=0
    if [ "$want" = true ]; then [ "$(grep -c . <<<"$declared")" = 3 ] || legacy_ok=0
    else [ -z "$declared" ] || legacy_ok=0; fi
    if grep -q '^VERSION ' <<<"$out" && [ "$cases_ok" = 1 ] && [ -z "$(echo $stray)" ] \
       && [ "$native" = "$declared" ] && [ "$legacy_ok" = 1 ]; then
      ok "$label ($(sed -n 's/^VERSION //p' <<<"$out")): native_decide, decide +native and bv_decide proofs read native, a classical one does not; the toolchain's axioms are exactly the classified set (native: $(echo ${native:-none}))"
    else
      no "$label: classification moved — cases=[$(grep '^CASE' <<<"$out" | tr '\n' ';')] cases_ok=$cases_ok stray=[${stray}] native=[$(echo $native)] legacy=[$(echo $legacy)] want_legacy=$want: $(grep -vE '^(AXIOM|LEGACY|CASE|VERSION)' <<<"$out" | head -3 | tr '\n' ' ')"
    fi
  }
  classify_on "Lean $(sed 's/^leanprover\/lean4://' lean-toolchain)" true lake env lean
  # Lean 4.35, by its elan toolchain. Never fetched here unless asked (CONCRETE_LEAN435_FETCH=1): an
  # offline gate must not depend on the network. Absent, it is an explicit SKIP, or a FAIL when
  # CONCRETE_REQUIRE_LEAN435=1 (CI's checker-gates job installs it and sets that).
  L435_TC="leanprover/lean4:v4.35.0"
  L435="${ELAN_HOME:-$HOME/.elan}/toolchains/leanprover--lean4---v4.35.0/bin/lean"
  if [ ! -x "$L435" ] && [ "${CONCRETE_LEAN435_FETCH:-0}" = 1 ] && command -v elan >/dev/null; then
    elan toolchain install "$L435_TC" >/dev/null 2>&1 || true
  fi
  if [ -x "$L435" ]; then
    # Its own library only: an inherited LEAN_PATH names this repository's 4.34 build.
    classify_on "Lean 4.35 ($L435_TC)" false env -u LEAN_PATH -u LEAN_SYSROOT "$L435"
  elif [ "${CONCRETE_REQUIRE_LEAN435:-0}" = 1 ]; then
    no "Lean 4.35 is required (CONCRETE_REQUIRE_LEAN435=1) but $L435 is not installed"
  else
    echo "  SKIP Lean 4.35 leg: $L435_TC is not installed (CONCRETE_LEAN435_FETCH=1 installs it)"
  fi
else
  no "could not read Replay.nativeAxiomClassifierSrc: $(head -3 <<<"$CLS" | tr '\n' ' ')"
fi

GATE_DONE=1
echo "KERNEL-RECHECK: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
