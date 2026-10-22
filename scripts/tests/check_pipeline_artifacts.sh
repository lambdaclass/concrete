#!/usr/bin/env bash
# PIPELINE ARTIFACTS — R-0497 slice 1 (verified-pipeline S5, artifact half).
#
# From `ValidatedCore` on, every pipeline artifact in Concrete/Pipeline/Pipeline.lean is SEALED
# (private constructor) and CERTIFIED (an erased Prop field stating what its producer established
# about its exact payload). This gate proves both by elaboration, not by grepping for `private`:
#
#   P   positive controls: the certificates are readable as the stated equations, the producer
#       chain type-checks, and a whole program and a filtered program are `CoreSubset`s;
#   F   forgery: `{ … }`, `⟨…⟩` and `.mk` on ValidatedCore / MonomorphizedProgram / SSAProgram /
#       ElaboratedProgram / CoreSubset do not elaborate outside Pipeline.lean;
#   U   mutation reuse: `{ a with payload := … }` keeping the certified type does not elaborate;
#   D   diagnostic bypass: `UnverifiedSSA` does not reach `emit`; a `CoreSubset` does not reach
#       Mono or the verify gates (it is not a certificate);
#   S   source: nothing opens the private constructors (`open private`), and Pipeline.lean holds
#       no `sorry`/axiom that could mint a certificate;
#   B   bug 180: in a project, `--report mono` and `--report verify` describe the program the build
#       compiles, not the invoked file's modules re-run without their dependencies.
#
# Each F/U/D control must fail for its stated reason (the expected message is pinned), so a
# control that fails for an unrelated reason (a renamed import, a typo) is reported, not counted.
set -uo pipefail
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
export LC_ALL=C
BIN="$ROOT_DIR/.lake/build/bin/concrete"
[ -x "$BIN" ] || { echo "error: build first ($BIN missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
P="Concrete/Pipeline/Pipeline.lean"

HDR='import Concrete
open Concrete'

echo "=== P: certificates and the producer chain ==="
cat > "$TMP/pos.lean" <<LEAN
$HDR
example (vc : ValidatedCore) : coreCheckProgram vc.coreModules = .ok () := vc.valid
example (m : MonomorphizedProgram) :
    monoProgram m.source.coreModules (preTypeCheck := verifyNoStoredRefs) = .ok m.coreModules :=
  m.produced
example (m : MonomorphizedProgram) : verifyPostMono m.coreModules = [] := m.postMono
example (s : SSAProgram) : ssaVerifyProgram s.ssaModules = .ok () := s.verified
example (s : SSAProgram) : ∃ raw, ssaVerifyProgram raw = .ok () ∧ s.ssaModules = ssaCleanupProgram raw :=
  let ⟨raw, _, hv, hc⟩ := s.lowered; ⟨raw, hv, hc⟩
example (e : ElaboratedProgram) : Except Diagnostics String := do
  let vc ← Pipeline.coreCheck e
  let m ← Pipeline.monomorphize vc
  let s ← Pipeline.lower m
  pure (Pipeline.emit s)
example (vc : ValidatedCore) : CoreSubset := vc
example (vc : ValidatedCore) : (vc.filterModules (fun _ => true)).coreModules.Sublist vc.coreModules :=
  (vc.filterModules _).drawn
example (vc : ValidatedCore) : (vc.whole).source = vc := rfl
#print axioms ValidatedCore.whole
#print axioms Pipeline.lower
LEAN
POS_OUT="$(lake env lean "$TMP/pos.lean" 2>&1)"; POS_RC=$?
if [ "$POS_RC" -eq 0 ] && ! grep -q 'error' <<<"$POS_OUT"; then
  ok "P: the four certificates read as their equations and the producer chain type-checks"
else
  no "P: the positive control did not elaborate (rc=$POS_RC): $(head -c 1500 <<<"$POS_OUT")"
fi
if grep -q 'sorryAx' <<<"$POS_OUT"; then
  no "P: a pipeline producer depends on sorryAx"
else
  ok "P: ValidatedCore.whole and Pipeline.lower depend on no sorryAx"
fi

# neg <id> <reason-regex> <lean body> — the body must NOT elaborate, and the error must match.
neg() {
  local id="$1" want="$2" body="$3" out rc
  printf '%s\n%s\n' "$HDR" "$body" > "$TMP/$id.lean"
  out="$(lake env lean "$TMP/$id.lean" 2>&1)"; rc=$?
  if [ "$rc" -eq 0 ]; then
    no "$id: elaborated — $(tr '\n' ' ' <<<"$body" | head -c 160)"
  elif grep -Eq "$want" <<<"$out"; then
    ok "$id: refused ($(grep -Eo "$want" <<<"$out" | head -1))"
  else
    no "$id: failed, but not for the expected reason /$want/: $(grep -m2 'error' <<<"$out" | head -c 400)"
  fi
}
PRIV='private|[Uu]nknown (constant|identifier) `Concrete\.[A-Za-z]+\.mk`'

echo ""
echo "=== F: forgery outside the producer ==="
neg F1 "$PRIV" 'def forged : ValidatedCore := { coreModules := [], valid := sorry }'
neg F2 "$PRIV" 'def forged : ValidatedCore := ⟨[], sorry⟩'
neg F3 "$PRIV" 'def forged : ValidatedCore := ValidatedCore.mk [] sorry'
neg F4 "$PRIV" 'def forged (vc : ValidatedCore) : MonomorphizedProgram :=
  { source := vc, coreModules := vc.coreModules, produced := sorry, postMono := sorry }'
neg F5 "$PRIV" 'def forged (m : MonomorphizedProgram) (xs : List SModule) : SSAProgram :=
  ⟨m, xs, sorry, sorry⟩'
neg F6 "$PRIV" 'def forged (xs : List CModule) : ElaboratedProgram := { coreModules := xs }'
neg F7 "$PRIV" 'def forged (vc : ValidatedCore) (xs : List CModule) : CoreSubset := ⟨vc, xs, sorry⟩'

echo ""
echo "=== U: mutation reuse keeps no certificate ==="
neg U1 "$PRIV" 'def reuse (vc : ValidatedCore) (xs : List CModule) : ValidatedCore := { vc with coreModules := xs }'
neg U2 "$PRIV" 'def reuse (s : SSAProgram) (xs : List SModule) : SSAProgram := { s with ssaModules := xs }'
neg U3 "$PRIV" 'def reuse (m : MonomorphizedProgram) (xs : List CModule) : MonomorphizedProgram := { m with coreModules := xs }'
neg U4 "$PRIV" 'def reuse (s : CoreSubset) (xs : List CModule) : CoreSubset := { s with coreModules := xs }'

echo ""
echo "=== D: diagnostic and subset artifacts are not certificates ==="
neg D1 'type mismatch|Application type mismatch|has type' 'def bypass (u : UnverifiedSSA) : String := Pipeline.emit u'
neg D2 'type mismatch|Application type mismatch|has type' 'def bypass (s : CoreSubset) := Pipeline.monomorphize s'
neg D3 'type mismatch|Application type mismatch|has type' 'def bypass (s : CoreSubset) := Pipeline.runVerifyGates s'
neg D4 'type mismatch|Application type mismatch|has type' 'def bypass (e : ElaboratedProgram) := Pipeline.monomorphize e'

echo ""
echo "=== S: no source opens the seals ==="
HITS="$(grep -rnE 'open private .*(ValidatedCore|MonomorphizedProgram|SSAProgram|ElaboratedProgram|CoreSubset)' \
  --include='*.lean' Concrete Main.lean tests 2>/dev/null || true)"
[ -z "$HITS" ] && ok "S: no 'open private' of a pipeline artifact constructor" || no "S: $HITS"
if grep -nE '\bsorry\b|^\s*axiom\b' "$P" | grep -v -- '--' >/dev/null; then
  no "S: $P holds a sorry or axiom: $(grep -nE '\bsorry\b|^\s*axiom\b' "$P" | head -3)"
else
  ok "S: $P holds no sorry and no axiom"
fi
# The record-update forgery this slice removed must not come back under another spelling.
HITS="$(grep -rnE '\{ *[A-Za-z_]+ with (coreModules|ssaModules) *:=' --include='*.lean' Concrete Main.lean 2>/dev/null | grep -v "^$P:" || true)"
[ -z "$HITS" ] && ok "S: no record update of an artifact payload outside $P" || no "S: $HITS"

echo ""
echo "=== B: project reports run on the compiled program (bug 180) ==="
WF="examples/wordfreq"
MONO="$("$BIN" "$WF/src/main.con" --report mono 2>&1)"
SPEC="$(grep -E '^Specializations: +[0-9]+' <<<"$MONO" | grep -oE '[0-9]+' | head -1)"
LL="$(cd "$WF" && "$BIN" build --emit-llvm 2>/dev/null)"
if [ -z "$SPEC" ] || [ -z "$LL" ]; then
  no "B: could not read wordfreq's mono report or IR (spec='$SPEC', ir bytes=${#LL})"
else
  # Every specialization the report lists is a function the build emits, and the build emits none
  # the report omits: the report and the compile describe one program.
  grep -E '^  [^ ].* -> ' <<<"$MONO" | sed -E 's/.* -> //' | sort -u > "$TMP/rep.names"
  # An emitted symbol is `_C.<module path>.<mono name>`; the report lists the mono name.
  grep -oE '^define [^@]*@[A-Za-z0-9_.$]+' <<<"$LL" | sed -E 's/.*@//; s/.*\.//' | grep '_for_' | sort -u > "$TMP/ll.names"
  NREP="$(grep -c . "$TMP/rep.names" || true)"; NLL="$(grep -c . "$TMP/ll.names" || true)"
  MISSING="$(comm -13 "$TMP/rep.names" "$TMP/ll.names" | head -5 | tr '\n' ' ')"
  if [ "$NLL" -gt 0 ] && [ "$SPEC" -gt 0 ] && [ -z "$MISSING" ]; then
    ok "B: wordfreq's project --report mono lists $SPEC specializations, including all $NLL the build emits"
  else
    no "B: wordfreq's --report mono lists $SPEC ($NREP names) but the build emits $NLL; unlisted: ${MISSING:-none}"
  fi
fi
VER="$("$BIN" "$WF/src/main.con" --report verify 2>&1)"
grep -q 'VERIFY-GATES: ok' <<<"$VER" && ok "B: wordfreq's project --report verify is clean on the compiled program" \
  || no "B: wordfreq's project --report verify: $(head -3 <<<"$VER" | tr '\n' ' ')"

echo ""
echo "PIPELINE-ARTIFACTS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
