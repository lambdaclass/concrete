#!/usr/bin/env bash
# VC-bridge register gate.
#
# docs/verification/VC_BRIDGE_REGISTER.md inventories the Core→obligation lowering rule by rule:
# what each emits, assumes, rejects, which example forced it, and the theorem that
# will discharge it. That bridge is the one every runtime-safety claim rests on, and
# the one multi-kernel agreement provably cannot check — all kernels check the SAME
# lowered proposition, so a mis-lowering yields unanimous agreement on the wrong
# formula.
#
# This gate keeps the register honest in both directions:
#   * every obligation-family generator in the code has a register row, so a new
#     family cannot ship undocumented;
#   * every row names a real generator, so the register cannot accumulate rows for
#     rules that no longer exist (the drift direction docs usually fail in).
#
# Cheap: pure grep, no build, no provers.

set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

REG="docs/verification/VC_BRIDGE_REGISTER.md"
SRC="Concrete/Report/ReportObligations.lean"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

[ -f "$REG" ] || { echo "error: $REG missing" >&2; exit 2; }
[ -f "$SRC" ] || { echo "error: $SRC missing" >&2; exit 2; }

echo "=== every obligation family generator has a register row ==="
# The families are the top-level `def <name>Obligations (modules : List Module)`
# generators. Derived from the source, not hardcoded, so adding a family is what
# trips this gate rather than someone remembering to update a list here.
FAMILIES="$(grep -oE '^def [a-zA-Z]+Obligations \(modules' "$SRC" \
            | sed -E 's/^def ([a-zA-Z]+Obligations) .*/\1/' | sort -u)"

if [ -z "$FAMILIES" ]; then
  no "found no obligation-family generators in $SRC (pattern changed?)"
else
  COUNT=$(printf '%s\n' "$FAMILIES" | grep -c '[^[:space:]]')
  ok "discovered $COUNT obligation-family generator(s) in the source"
  for f in $FAMILIES; do
    if grep -q "\`$f\`" "$REG"; then
      ok "$f has a register row"
    else
      no "$f has NO row in $REG — a new obligation family must be registered"
    fi
  done
fi

echo "=== every register row names a real generator (no stale rows) ==="
# Rows are `### \`name\`` headings. A row for a deleted generator is drift in the
# other direction and would quietly overstate coverage.
ROWS="$(grep -oE '^### `[a-zA-Z]+Obligations`' "$REG" \
        | sed -E 's/^### `([a-zA-Z]+Obligations)`/\1/' | sort -u)"
if [ -z "$ROWS" ]; then
  no "no register rows found in $REG (heading format changed?)"
else
  for r in $ROWS; do
    grep -qE "^def $r \(modules" "$SRC" \
      && ok "row $r corresponds to a real generator" \
      || no "row $r names no generator in $SRC — stale register row"
  done
fi

echo "=== each row states what it assumes, rejects, and what will discharge it ==="
# A row that omits these is a title, not a contract. Checked per row so an
# incomplete row cannot hide behind a complete one.
for r in $ROWS; do
  # slice from this row's heading to the next `###`/`##`
  SEC="$(awk -v pat="^### \`$r\`" '
    $0 ~ pat {f=1; next}
    f && /^#{2,3} / {exit}
    f {print}' "$REG")"
  # A row may declare itself a PROJECTION — it selects existing obligations rather
  # than lowering anything, so it has no contract to state and nothing to discharge.
  # Giving such a row invented Emits/Assumes fields would be worse than exempting it:
  # it would assert a contract that does not exist.
  if has_match '\*\*Kind\*\*: projection' <<<"$SEC"; then
    ok "$r is a declared projection (no lowering contract to state)"
    continue
  fi
  MISSING=""
  has_match '\*\*Emits\*\*' <<<"$SEC"    || MISSING="$MISSING Emits"
  has_match '\*\*Assumes\*\*' <<<"$SEC"  || MISSING="$MISSING Assumes"
  has_match '\*\*Rejects\*\*' <<<"$SEC"  || MISSING="$MISSING Rejects"
  has_match 'Discharging theorem' <<<"$SEC" || MISSING="$MISSING DischargingTheorem"
  if [ -z "$MISSING" ]; then
    ok "$r states Emits / Assumes / Rejects / discharging theorem"
  else
    no "$r is missing:$MISSING"
  fi
done

echo "=== the register does not claim rows are discharged when they are not ==="
# The status line is the one place a reader looks for "how much of this is real".
# If a row ever gets discharged, this must be updated deliberately, not drift.
grep -qE 'Rows discharged: \*\*[0-9]+ of [0-9]+\*\*' "$REG" \
  && ok "register states an explicit discharged-row count" \
  || no "register must state 'Rows discharged: **N of M**'"

echo "=== R-0460: the sufficiency theorem exists and is actually proved ==="
# A register row is discharged by a THEOREM, not by a row saying so. The build being green
# is the proof; this only checks the row was not quietly emptied.
IA="Concrete/Semantics/IntArith.lean"
grep -q "^theorem trapConditions_sufficient" "$IA" \
  && ok "trapConditions_sufficient is present" \
  || no "trapConditions_sufficient MISSING — the div/mod row is undischarged again"
for bad in sorry admit native_decide; do
  grep -qE "\b$bad\b" "$IA" \
    && no "$IA contains '$bad' — the sufficiency theorem is not actually proved" \
    || ok "no '$bad' in the trap semantics"
done
# The defect the theorem caught. `resultInRange` shipped as the constant `true`, which made
# the statement false for + - * while the row looked one step from discharged.
if grep -qE "^\s*\|\s*\.resultInRange => true" "$IA"; then
  no "resultInRange is the constant 'true' again — the sufficiency claim is vacuous for + - *"
else
  ok "resultInRange is op-specific (not the vacuous placeholder it shipped as)"
fi
# And the honest limit: shifts are covered only vacuously, so the row must NOT read discharged.
# A DECLARATION, not a mention: the docstring legitimately discusses the shift row in order
# to say the theorem does not cover it. Matching bare text flagged that explanation — the
# same "gate describes the defect it guards against" noise this suite has hit before.
if grep -qE "^(private )?theorem shift_obligation_sufficient" "$IA"; then
  no "a shift sufficiency theorem is DECLARED — but evalIntBinOp does not model shifts, so it would be vacuous"
else
  ok "no shift sufficiency theorem is declared (evalIntBinOp does not model shifts)"
fi
grep -q "VACUOUS" "$IA" \
  && ok "the vacuity of the shift case is stated where the theorem lives" \
  || no "nothing warns that the theorem's shift case is vacuous — it reads as covering shifts"

echo "=== the register's ROWS are checked, not just its row COUNT (2026-08-04) ==="
# Until now this gate checked that every generator HAS a row and that no row is stale about
# its generator. It never checked what the rows SAY. Two defects got through that gap in two
# days: the Sufficiency section declared every row's Assumes clause "currently **false** in
# the presence of loop invariants" a day after H23 was closed, and the shift row was one
# edit away from being recorded as discharged by a theorem that holds only because
# `evalIntBinOp` does not model shifts.
#
# This file is load-bearing for Register A — it is the list of what the compiler still owes —
# and it had no gate over its claims at all.
REG="docs/verification/VC_BRIDGE_REGISTER.md"

# Structure is ALREADY gated above (per-row Emits/Assumes/Rejects/Discharging-theorem, with
# a projection exemption). My first version of this block duplicated it and omitted the
# exemption, so it failed the one row legitimately without a lowering contract. What was
# genuinely ungated is what the rows CLAIM — the checks below.

# A cell marked DISCHARGED must name theorems that EXIST; a cell marked TODO must name
# theorems that do NOT (a TODO naming a proved theorem is the understating drift). Splitting
# on the marker is the whole point — the first version of this check demanded existence
# everywhere and flagged three legitimate TODOs, which is a gate calling correct
# documentation wrong.
PHANTOM=0
while IFS= read -r cell; do
  IDS="$(printf '%s' "$cell" | grep -oE '[A-Za-z][A-Za-z_]*_(sufficient|necessary)' | sort -u)"
  if has_match "DISCHARGED" <<<"$cell"; then
    for name in $IDS; do
      grep -rqE "^(private )?theorem $name" Concrete/ 2>/dev/null \
        || { no "a DISCHARGED cell names theorem '$name', which does not exist in Concrete/"; PHANTOM=1; }
    done
  elif has_match "TODO" <<<"$cell"; then
    for name in $IDS; do
      if grep -rqE "^(private )?theorem $name" Concrete/ 2>/dev/null; then
        no "a TODO cell names theorem '$name', which IS proved — the row understates the compiler"
        PHANTOM=1
      fi
    done
  fi
done < <(grep "Discharging theorem" "$REG")
[ "$PHANTOM" = "0" ] && ok "DISCHARGED cells name real theorems; TODO cells name unproved ones"

# 3. NO UNRECORDED DISCHARGE. A proved sufficiency/necessity theorem that no row mentions is
# drift in the understating direction — the compiler is stronger than its own register says,
# and the next reader plans around the register.
UNREC=0
for t in $(grep -rhoE "^(private )?theorem [A-Za-z_]+_(sufficient|necessary)" Concrete/ 2>/dev/null \
           | awk '{print $NF}' | sort -u); do
  grep -q "$t" "$REG" || { no "theorem '$t' is proved but no register row mentions it"; UNREC=1; }
done
[ "$UNREC" = "0" ] && ok "every proved sufficiency/necessity theorem is recorded in a row"

# 3b. THE HEADLINE COUNT. The Status line is what a reader takes away, and it said "0 of 4"
# for a day after the first row was half-discharged. Derive both numbers and compare.
LOWERING_ROWS="$(awk '/^### `/{r=$0} /\*\*Kind\*\*: projection/{next} /^### `/{n++} END{print n}' "$REG")"
HALF="$(grep -c "Discharging theorem (HALF DISCHARGED" "$REG" || true)"
STATED_HALF="$(grep -oE "Half discharged: \*\*[0-9]+ of [0-9]+\*\*" "$REG" | grep -oE "[0-9]+ of" | head -1 | tr -d ' of')"
if [ "$HALF" = "$STATED_HALF" ]; then
  ok "the Status line's half-discharged count ($STATED_HALF) matches the rows ($HALF)"
else
  no "Status says $STATED_HALF half-discharged; the rows say $HALF — the headline is stale"
fi

# 4. FIXTURES EXIST. "Forced by" names the program that makes the row non-hypothetical; a
# path that has been moved or deleted turns the strongest column into decoration.
MISSINGFX=0
for f in $(grep -oE '`(examples|tests|std)/[A-Za-z0-9_./-]+`' "$REG" | tr -d '`' | sort -u); do
  [ -e "$f" ] || { no "the register cites '$f', which does not exist"; MISSINGFX=1; }
done
[ "$MISSINGFX" = "0" ] && ok "every fixture path the register cites exists on disk"

# 5. NO UNQUALIFIED "currently false". The H23 instance: a row asserted its own Assumes
# clause was false, and stayed that way after the cause was fixed. Such a claim is allowed,
# but it must name the hole it refers to, so the hole-status gate can catch it going stale.
BADCLAIM="$(grep -nE "currently \*\*?false|is currently false" "$REG" | grep -vE "H[0-9]+" || true)"
if [ -n "$BADCLAIM" ]; then
  no "a row claims something is 'currently false' without naming a hole (unauditable):"
  printf '%s\n' "$BADCLAIM" | head -3 | sed 's/^/         /'
else
  ok "no unqualified 'currently false' claim (any such claim must name its hole)"
fi

echo "=== the reference evaluator's division convention (2026-08-04) ==="
# `evalIntEnv` is what EVERY lowering-agreement check measures a rendering against. If its
# arithmetic disagrees with the runtime's, "validated" means validated against the wrong
# thing, and no amount of kernel agreement would reveal it — every kernel is compared to the
# same wrong reference.
#
# This block used to be a GREP, and said so: `evalIntEnv` was a `partial def`, so the kernel
# could not reduce it and no `rfl` example could pin its behaviour. R-0455 replaced it with a
# structural wrapper over the term IR, so the behaviour is now pinned by COMPILE-TIME
# examples and this gate only has to prove those were not deleted.
# That evalIntEnv and evalBoolEnv stay total is pinned in docs/compiler/totality_pinned.tsv and held
# by scripts/tests/check_totality_ratchet.sh, which reads the elaborated environment.
RO="Concrete/Report/ReportObligations.lean"
# The locks that grep stood in for. Truncating vs flooring divergence at a negative dividend
# is where a silent re-pointing of the reference would hide.
grep -q 'example : evalIntEnv \[("a", -7), ("b", 2)\]' "$RO" \
  && ok "evalIntEnv's division is pinned by rfl at a negative dividend (-7 / 2 = -3)" \
  || no "the division-convention lock is gone — a swap to floored division becomes invisible"
grep -q '(.binOp spR .div (.ident spR "a") (.ident spR "b")) = none := rfl' "$RO" \
  && ok "division by zero yields no reference value (never guessed)" \
  || no "the divide-by-zero lock is gone"
grep -q 'example : evalIntEnv \[\] (.call spR "f" \[\] \[.intLit spR 1\]) = none := rfl' "$RO" \
  && ok "a spec call has no reference value — uninterpreted means uninterpreted" \
  || no "the uninterpreted-symbol lock is gone: the evaluator could invent a value"
# The convention examples in the IR itself must survive too.
grep -q 'example : (-7 : Int).fdiv 2 = -4 := rfl' Concrete/Report/ReportObligations.lean \
  || grep -q 'example : (-7 : Int).fdiv 2 = -4 := rfl' Concrete/Semantics/TermIR.lean \
  && ok "the divergent spellings remain distinguished at compile time" \
  || no "the tdiv/fdiv distinction is no longer pinned"

echo ""
echo "VC-BRIDGE-REGISTER: PASS=$PASS  FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
