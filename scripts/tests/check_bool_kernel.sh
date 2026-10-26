#!/usr/bin/env bash
# Non-arithmetic multi-kernel gate (branch spike/non-arithmetic-multi-kernel).
#
# Every other kernel-agreement gate in this repo is LINEAR INTEGER ARITHMETIC: omega, lia,
# presburger. Three mature decision procedures for one theory. This gate carries BOOLEAN
# postconditions to the same three kernels, where each must use a different procedure —
# `destruct; reflexivity` (Rocq), `auto` (Isabelle/HOL), `decide` (Lean) — so agreement is not
# three invocations of the same idea.
#
# Two things this tier has that the arithmetic tier does not:
#
#   1. Reference-evaluator agreement is EXHAUSTIVE (all 2^n assignments), not sampled. A kernel
#      cannot close a lowering that means something else without this gate noticing.
#   2. The Rocq proofs are CONSTRUCTIVE, verified by `Print Assumptions` reporting
#      "Closed under the global context". Lowering Concrete `bool` to Rocq `Prop` instead would
#      make De Morgan need classical logic and put an axiom in an attestation.
#
# WHY THIS GATE RUNS THE GENERATED SCRIPTS rather than trusting the in-Lean `rfl` locks: the
# locks passed while the emitted Rocq did not compile at all (`&&` needs `bool_scope`), and
# again while `xor` was a type error (`negb (a = b)` — `a = b` is a `Prop`). Both were found
# here and only here.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
BIN="./.lake/build/bin/concrete"
DEMO="examples/bool_kernel_demo/src/main.con"
PASS=0; FAIL=0; INCONC=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
# "I could not test it" and "it is wrong" are different facts; conflating them is the failure
# this repo's other prover gates are careful about, so the count is in the summary line.
inconc(){ echo "  INCONC $1"; INCONC=$((INCONC+1)); }

lake build >/dev/null 2>&1 || { echo "FAIL build failed"; exit 1; }
[ -x "$BIN" ] || { echo "FAIL $BIN missing"; exit 1; }
[ -f "$DEMO" ] || { echo "FAIL $DEMO missing"; exit 1; }

WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
# Each prover run is bounded (a hang is INCOMPLETE, not a refusal): bounded_gate_status below.
BOUNDED_PARENT="$WORK"; source "$ROOT_DIR/scripts/tests/lib/bounded.sh"
REPORT="$WORK/report.txt"
"$BIN" "$DEMO" --report bool-kernel > "$REPORT" 2>&1

echo "=== the obligations are non-arithmetic, and the tier says so ==="
grep -q "EXHAUSTIVE" "$REPORT" \
  && ok "the report states agreement is exhaustive, not sampled" \
  || no "the exhaustiveness claim is gone"
# No arithmetic tactic may appear: if lia/presburger/omega shows up, this is not a
# non-arithmetic tier any more.
if grep -qE "\blia\b|\bpresburger\b|\bomega\b|ZArith" "$REPORT"; then
  no "an ARITHMETIC tactic or ZArith appears — the non-arithmetic claim is false"
else
  ok "no arithmetic tactic and no ZArith in any lowering"
fi

echo "=== reference evaluator: exhaustive, and it discriminates ==="
NHOLD="$(grep -c "holds on every assignment" "$REPORT")"
[ "$NHOLD" = "4" ] \
  && ok "4 obligations hold on every assignment (got $NHOLD)" \
  || no "expected 4 exhaustively-true obligations, got $NHOLD"
grep -q "wrong_post#boolpost0\]  reference evaluator: FALSE" "$REPORT" \
  && ok "the WRONG postcondition is caught before any kernel is asked" \
  || no "a false postcondition is not being caught by the reference evaluator"
# Coverage must be stated: a function this tier cannot handle is NAMED, not silently absent.
grep -q "multi_stmt: body is not a single" "$REPORT" \
  && ok "a function outside the tier is named, not silently skipped" \
  || no "skipped functions are not reported — coverage can be over-read"

# ---- extract the generated per-kernel sources -------------------------------------------
python3 - "$REPORT" "$WORK" <<'PY'
import re, sys
report, work = sys.argv[1], sys.argv[2]
txt = open(report).read()
names = []
for b in re.split(r"\n  \[", txt)[1:]:
    name = b.split("]")[0].split(".")[1].split("#")[0]
    names.append(name)
    m = re.search(r"--- rocq:destruct ---\n(.*?)\n(?=    --- isabelle)", b, re.S)
    if m: open(f"{work}/{name}.v", "w").write(m.group(1).strip() + "\n")
    mi = re.search(r"--- isabelle:auto ---\n(.*?)\nend", b, re.S)
    if mi:
        thy = (mi.group(1) + "\nend").replace("theory BoolPost", f"theory Thy_{name}")
        open(f"{work}/Thy_{name}.thy", "w").write(thy.strip() + "\n")
    ml = re.search(r"lean:decide goal: (.*)", b)
    if ml: open(f"{work}/{name}.lean", "w").write(f"theorem bp : {ml.group(1).strip()} := by decide\n")
open(f"{work}/names.txt", "w").write("\n".join(names))
# TRUE goals only in the Isabelle session; the false one is built separately as a control.
true_names = [n for n in names if n != "wrong_post"]
open(f"{work}/ROOT", "w").write("session BK = HOL +\n  theories\n" +
                                "".join(f"    Thy_{n}\n" for n in true_names))
PY
TRUE_FNS="nand nor and3 xor"

echo "=== LEAN (decide): the four close, the false one is REFUSED ==="
LCLOSED=0
for t in $TRUE_FNS; do
  if bounded_run 300 -- lake env lean "$WORK/$t.lean" >/dev/null 2>&1; then LCLOSED=$((LCLOSED+1)); fi
done
[ "$LCLOSED" = "4" ] \
  && ok "lean:decide closed all 4 boolean goals" \
  || no "lean:decide closed only $LCLOSED of 4"
if bounded_run 300 -- lake env lean "$WORK/wrong_post.lean" >/dev/null 2>&1; then
  no "lean:decide CLOSED the false postcondition — the tier has no teeth"
else
  ok "lean:decide refuses the false postcondition"
fi

echo "=== ROCQ (destruct/reflexivity) + AXIOM-FREEDOM ==="
if command -v coqc >/dev/null 2>&1; then
  RCLOSED=0; RAXFREE=0
  for t in $TRUE_FNS; do
    O="$( cd "$WORK" && coqc "$t.v" 2>&1 )"
    echo "$O" | grep -q "Closed under the global context" && RAXFREE=$((RAXFREE+1))
    echo "$O" | grep -qiE "^error|error:" || RCLOSED=$((RCLOSED+1))
  done
  [ "$RCLOSED" = "4" ] \
    && ok "rocq closed all 4 (the GENERATED scripts, not hand-written ones)" \
    || no "rocq closed only $RCLOSED of 4 — the emitted script is wrong"
  [ "$RAXFREE" = "4" ] \
    && ok "all 4 are axiom-free (Print Assumptions: closed under the global context)" \
    || no "only $RAXFREE of 4 are axiom-free — a classical axiom leaked into an attestation"
  WO="$( cd "$WORK" && coqc wrong_post.v 2>&1 )"
  if echo "$WO" | grep -qi "unable to unify"; then
    ok "rocq refuses the false postcondition (Unable to unify)"
  else
    no "rocq did not refuse the false postcondition"
  fi
else
  inconc "coqc not on PATH — run under \`nix develop .#provers\` for the Rocq assertions"
fi

echo "=== ISABELLE/HOL (auto) ==="
if command -v isabelle >/dev/null 2>&1; then
  IO_OUT="$( cd "$WORK" && isabelle build -D . 2>&1 )"
  printf '%s' "$IO_OUT" | grep -q "^Finished" \
    && ok "isabelle closed all 4 boolean goals with \`auto\`" \
    || no "isabelle did not finish the session — the emitted theory is wrong"
  # The false control as its own session, so its failure cannot be confused with a real one.
  mkdir -p "$WORK/ctl"
  sed 's/theory Thy_wrong_post/theory Thy_ctl/' "$WORK/Thy_wrong_post.thy" > "$WORK/ctl/Thy_ctl.thy" 2>/dev/null
  printf 'session CTL = HOL +\n  theories\n    Thy_ctl\n' > "$WORK/ctl/ROOT"
  CO="$( cd "$WORK/ctl" && isabelle build -D . 2>&1 )"
  if printf '%s' "$CO" | grep -qE "Failed to finish proof|FAILED"; then
    ok "isabelle refuses the false postcondition"
  else
    no "isabelle did not refuse the false postcondition"
  fi
else
  inconc "isabelle not on PATH — run under \`nix develop .#provers\` for the HOL assertions"
fi

echo "=== the sort choice is load-bearing, not cosmetic ==="
# Concrete `bool` -> Rocq `bool`. Over `Prop`, De Morgan needs classical logic and
# `Print Assumptions` would report it. The generated scripts must therefore never open ZArith
# nor state the lemma over Prop.
grep -q "Open Scope bool_scope" "$REPORT" \
  && ok "the Rocq lowering opens bool_scope (required for && and ||)" \
  || no "bool_scope is gone — the emitted script will not compile"
grep -q "From Stdlib Require Import Bool" "$REPORT" \
  && ok "and imports Bool (required for nested boolean equality \`eqb\`)" \
  || no "the Bool import is gone — a nested equality will be a Prop/bool type error"

# ============================ RUNG 5: EUF ============================
echo "=== EUF (rung 5): uninterpreted spec functions in three kernels ==="
EUFDEMO="examples/euf_kernel_demo/src/main.con"
if [ ! -f "$EUFDEMO" ]; then
  no "$EUFDEMO missing"
else
EW="$WORK/euf"; mkdir -p "$EW/n"
"$BIN" "$EUFDEMO" --report bool-kernel > "$EW/report.txt" 2>&1

# The symbol must be QUANTIFIED, never declared: a `Parameter`/`axiom` would enter the trusted
# base and `Print Assumptions` would report it, turning an axiom-free attestation into an
# axiom-bearing one.
if grep -qE "^Parameter |^Axiom |^axiom " "$EW/report.txt"; then
  no "a spec symbol is DECLARED (Parameter/Axiom) — that puts it in the trusted base"
else
  ok "spec symbols are quantified function variables, never declared"
fi

# Propositional abstraction is sound in ONE direction. It must never claim to refute: an earlier
# version reported "FALSE — no kernel should close it" for CONGRUENCE, the defining property of
# an uninterpreted function.
if grep -q "no kernel should close it" "$EW/report.txt"; then
  no "the abstraction claims to REFUTE — it cannot; it forgets that f is a function"
else
  ok "the abstraction only ever confirms (tautology) or reports inconclusive"
fi
grep -q "congruence#euf0\]" "$EW/report.txt" \
  && ok "a congruence obligation is collected" \
  || no "the congruence obligation is missing"
# Symmetry through an opaque term IS a tautology once equality atoms are normalised: `f m = t`
# and `t = f m` are one atom, and treating them as two made this look falsifiable.
awk '/sym_through_opaque#euf0/{f=1} f&&/abstraction:/{print;exit}' "$EW/report.txt" \
  | grep -q "tautology" \
  && ok "symmetry through an opaque term is a tautology (equality atoms are normalised)" \
  || no "equality atoms are not normalised — `f m = t` and `t = f m` count as two atoms"
# And congruence must be inconclusive, not a tautology — that is the documented incompleteness.
awk '/eufdemo.congruence#euf0/{f=1} f&&/abstraction:/{print;exit}' "$EW/report.txt" \
  | grep -q "inconclusive" \
  && ok "congruence is inconclusive under abstraction (the incompleteness, shown)" \
  || no "congruence is being reported as decided — the abstraction cannot decide it"

python3 - "$EW/report.txt" "$EW" <<'PYEUF'
import re, sys
report, work = sys.argv[1], sys.argv[2]
txt = open(report).read()
for b in re.split(r"\n  \[", txt)[1:]:
    if "#euf" not in b.split("]")[0]: continue
    name = b.split("]")[0].split(".")[1].split("#")[0]
    def sect(marker):
        # bounded by the NEXT marker, not end-of-block: an unbounded `.*?$` swallowed the other
        # kernels' sections and produced files that could not parse.
        m = re.search(r"--- " + marker + r" ---\n(.*?)(?=\n    ---|\Z)", b, re.S)
        return m.group(1).rstrip() if m else None
    for mk, ext in [("lean:by_cases", ".lean"), ("rocq:congruence", ".v")]:
        v = sect(mk)
        if v: open(f"{work}/{name}{ext}", "w").write(v + "\n")
    iv = sect(r"isabelle:auto \(euf\)")
    if iv:
        d = work + "/n" if name == "bad_constant" else work
        open(f"{d}/Thy_{name}.thy", "w").write(iv.replace("theory EufGoal", f"theory Thy_{name}") + "\n")
good = ["sym_through_opaque", "congruence", "nested", "demorgan_opaque"]
open(f"{work}/ROOT", "w").write("session EUF = HOL +\n  theories\n" + "".join(f"    Thy_{n}\n" for n in good))
open(f"{work}/n/ROOT", "w").write("session EUFN = HOL +\n  theories\n    Thy_bad_constant\n")
PYEUF

EUF_TRUE="sym_through_opaque congruence nested demorgan_opaque"
ELC=0
for t in $EUF_TRUE; do
  bounded_run 300 -- lake env lean "$EW/$t.lean" >/dev/null 2>&1 && ELC=$((ELC+1))
done
[ "$ELC" = "4" ] \
  && ok "lean closed all 4 EUF goals (by_cases on DECIDABLE atoms, no classical axiom)" \
  || no "lean closed only $ELC of 4 EUF goals"
if bounded_run 300 -- lake env lean "$EW/bad_constant.lean" >/dev/null 2>&1; then
  no "lean CLOSED \`f m = f t\` — the symbol is not uninterpreted"
else
  ok "lean refuses \`f m = f t\` (nothing implies an opaque f is constant)"
fi

if command -v coqc >/dev/null 2>&1; then
  ERC=0; EAX=0
  for t in $EUF_TRUE; do
    O="$( cd "$EW" && coqc "$t.v" 2>&1 )"
    echo "$O" | grep -q "Closed under the global context" && EAX=$((EAX+1))
    echo "$O" | grep -qiE "^error|error:" || ERC=$((ERC+1))
  done
  [ "$ERC" = "4" ] && ok "rocq closed all 4 EUF goals" || no "rocq closed only $ERC of 4 EUF goals"
  # The important one. `~(A/\B) -> ~A \/ ~B` is classically valid; importing `Classical` would
  # close it and show up here as an axiom. Decidable case analysis keeps it clean.
  [ "$EAX" = "4" ] \
    && ok "all 4 are AXIOM-FREE — no Classical import leaked in" \
    || no "only $EAX of 4 axiom-free — a classical axiom is in the attestation"
  BO="$( cd "$EW" && coqc bad_constant.v 2>&1 )"
  if echo "$BO" | grep -qiE "congruence failed|Tactic failure|No applicable"; then
    ok "rocq refuses \`f m = f t\`"
  else
    no "rocq did not refuse \`f m = f t\`"
  fi
else
  inconc "coqc absent — EUF Rocq assertions (incl. axiom-freedom) not run"
fi

if command -v isabelle >/dev/null 2>&1; then
  EIO="$( cd "$EW" && isabelle build -D . 2>&1 )"
  printf '%s' "$EIO" | grep -q "^Finished" \
    && ok "isabelle closed all 4 EUF goals" \
    || no "isabelle did not finish the EUF session"
  EIN="$( cd "$EW/n" && isabelle build -D . 2>&1 )"
  if printf '%s' "$EIN" | grep -qE "Failed to finish proof|FAILED"; then
    ok "isabelle refuses \`f m = f t\`"
  else
    no "isabelle did not refuse \`f m = f t\`"
  fi
else
  inconc "isabelle absent — EUF HOL assertions not run"
fi
fi

# ====================== RUNGS 6+7: DATATYPES + ARRAYS ======================
echo "=== rungs 6+7: struct declarations and array reads in three kernels ==="
DTDEMO="examples/datatype_kernel_demo/src/main.con"
if [ ! -f "$DTDEMO" ]; then
  no "$DTDEMO missing"
else
DW="$WORK/dt"; mkdir -p "$DW/n"
"$BIN" "$DTDEMO" --report bool-kernel > "$DW/report.txt" 2>&1

# The DECLARATION must be emitted. Nothing told a prover a Concrete struct existed before this
# tier: CoreExtract emits Gallina Definitions and zero Inductive/Record.
grep -q "^Record Hdr" "$DW/report.txt" \
  && ok "Rocq Record declaration is emitted with the goal" \
  || no "no Record declaration — the lemma would reference an unknown type"
grep -q "^record Hdr" "$DW/report.txt" \
  && ok "Isabelle record declaration is emitted" || no "no Isabelle record declaration"
grep -q "^structure Hdr" "$DW/report.txt" \
  && ok "Lean structure declaration is emitted" || no "no Lean structure declaration"
# Field projection is the one genuine syntax difference: field-first vs field-last.
grep -q "(magic h)" "$DW/report.txt" && grep -q "h.magic" "$DW/report.txt" \
  && ok "field projection is rendered per kernel (magic h vs h.magic)" \
  || no "field projection is not being rendered per kernel"
# Arrays as total functions, sound because bounds are proved elsewhere.
grep -qE "\(a : Z -> Z\)" "$DW/report.txt" \
  && ok "an array is modelled as a total index -> value function" \
  || no "arrays are not modelled as functions"

python3 - "$DW/report.txt" "$DW" <<'PYDT'
import re, sys
report, work = sys.argv[1], sys.argv[2]
txt = open(report).read()
for b in re.split(r"\n  \[", txt)[1:]:
    if "#struct" not in b.split("]")[0]: continue
    name = b.split("]")[0].split(".")[1].split("#")[0]
    def sect(mk):
        m = re.search(r"--- " + mk + r" ---\n(.*?)(?=\n    ---|\Z)", b, re.S)
        return m.group(1).rstrip() if m else None
    for mk, ext in [(r"lean:by_cases \(struct\)", ".lean"), ("rocq:record", ".v")]:
        v = sect(mk)
        if v: open(f"{work}/{name}{ext}", "w").write(v + "\n")
    iv = sect("isabelle:record")
    if iv:
        d = work + "/n" if name == "bad_array" else work
        open(f"{d}/Thy_{name}.thy", "w").write(iv.replace("theory StructGoal", f"theory Thy_{name}") + "\n")
good = ["magic_kept", "both_fields", "array_read_stable", "mixed_demorgan"]
open(f"{work}/ROOT", "w").write("session DT = HOL +\n  theories\n" + "".join(f"    Thy_{n}\n" for n in good))
open(f"{work}/n/ROOT", "w").write("session DTN = HOL +\n  theories\n    Thy_bad_array\n")
PYDT

DT_TRUE="magic_kept both_fields array_read_stable mixed_demorgan"
DLC=0
for t in $DT_TRUE; do bounded_run 300 -- lake env lean "$DW/$t.lean" >/dev/null 2>&1 && DLC=$((DLC+1)); done
[ "$DLC" = "4" ] && ok "lean closed all 4 struct/array goals" || no "lean closed only $DLC of 4"
if bounded_run 300 -- lake env lean "$DW/bad_array.lean" >/dev/null 2>&1; then
  no "lean CLOSED \`a i = a j\` — two different array reads are being conflated"
else
  ok "lean refuses \`a i = a j\` (different indices need not agree)"
fi

if command -v coqc >/dev/null 2>&1; then
  DRC=0; DAX=0
  for t in $DT_TRUE; do
    O="$( cd "$DW" && coqc "$t.v" 2>&1 )"
    echo "$O" | grep -q "Closed under the global context" && DAX=$((DAX+1))
    echo "$O" | grep -qiE "^error|error:" || DRC=$((DRC+1))
  done
  [ "$DRC" = "4" ] && ok "rocq closed all 4 struct/array goals" || no "rocq closed only $DRC of 4"
  [ "$DAX" = "4" ] \
    && ok "all 4 axiom-free — Record declarations add nothing to the trusted base" \
    || no "only $DAX of 4 axiom-free"
  BAO="$( cd "$DW" && coqc bad_array.v 2>&1 )"
  if echo "$BAO" | grep -qiE "congruence failed|Tactic failure|No applicable"; then
    ok "rocq refuses \`a i = a j\`"
  else
    no "rocq did not refuse \`a i = a j\`"
  fi
else
  inconc "coqc absent — struct/array Rocq assertions not run"
fi

if command -v isabelle >/dev/null 2>&1; then
  DIO="$( cd "$DW" && isabelle build -D . 2>&1 )"
  printf '%s' "$DIO" | grep -q "^Finished" \
    && ok "isabelle closed all 4 struct/array goals" \
    || no "isabelle did not finish the struct session"
  DIN="$( cd "$DW/n" && isabelle build -D . 2>&1 )"
  if printf '%s' "$DIN" | grep -qE "Failed to finish proof|FAILED"; then
    ok "isabelle refuses \`a i = a j\`"
  else
    no "isabelle did not refuse \`a i = a j\`"
  fi
else
  inconc "isabelle absent — struct/array HOL assertions not run"
fi
fi

echo "=== coverage is REPORTED, so 'capability' cannot be read as 'covered' ==="
# These tiers are verified capability with no instances in the existing corpus. I overstated that
# in the roadmap twice before measuring it, so the number is now derived from the AST and printed.
HM="examples/hmac_sha256/src/main.con"
if [ -f "$HM" ]; then
  HC="$("$BIN" "$HM" --report bool-kernel 2>/dev/null | grep -oE 'COVERAGE:.*')"
  printf '%s' "$HC" | grep -q "0 reached a tier" \
    && ok "hmac_sha256's contracts reach NO tier, and the report says so" \
    || no "the coverage line no longer reports hmac_sha256 as unreached"
  # Its contracts are refinement obligations: unprovable with an opaque spec, which is why they
  # are excluded rather than attempted.
  printf '%s' "$HC" | grep -qE "[1-9][0-9]* are refinement obligations" \
    && ok "and identifies them as refinement obligations needing the spec's definition" \
    || no "refinement obligations are not being identified — the gap becomes invisible"
  # The label must not claim to be per-file: elf_header's own source has no contracts yet the
  # count is nonzero, because imports have them.
  printf '%s' "$HC" | grep -q "IMPORTS INCLUDED" \
    && ok "the count is labelled as program-wide, not per-file" \
    || no "the coverage count is mislabelled as per-file"
fi
# And a demo file DOES reach tiers, so the measurement discriminates.
DC="$("$BIN" "$DTDEMO" --report bool-kernel 2>/dev/null | grep -oE 'COVERAGE:.*')"
printf '%s' "$DC" | grep -qE "[1-9][0-9]* reached a tier" \
  && ok "a demo file reaches tiers (the coverage metric is not stuck at zero)" \
  || no "no file reaches a tier — the metric is measuring nothing"

# ================= REFINEMENT against a DEFINED spec =================
echo "=== refinement against a defined spec — the coverage fix ==="
# The measured problem: these tiers reached ZERO of the corpus's real contracts, because those are
# refinement obligations and a body-less `spec fn` is uninterpreted, so `result == spec(args)` is
# unprovable outside Lean (where the spec's meaning lives). A `spec fn` may now carry a
# definitional body in Concrete, making the obligation an equation between two expressions of THIS
# language -- visible to every kernel.
RFDEMO="examples/refine_kernel_demo/src/main.con"
if [ ! -f "$RFDEMO" ]; then
  no "$RFDEMO missing"
else
RW="$WORK/rf"; mkdir -p "$RW/n"
"$BIN" "$RFDEMO" --report bool-kernel > "$RW/report.txt" 2>&1

# The coverage number must MOVE. If a new tier does not change it, the metric is decorative.
grep -qE "COVERAGE: [0-9]+ contract clause\(s\).*[1-9][0-9]* reached a tier" "$RW/report.txt" \
  && ok "refinement obligations now reach a tier (the coverage number moved)" \
  || no "the coverage metric did not move — a new tier is not being counted"
# And an UNDEFINED spec must stay named as unreachable, not quietly counted as covered.
grep -q "has no definitional body" "$RW/report.txt" \
  && ok "a body-less spec is still named unreachable (uninterpreted, only Lean can discharge it)" \
  || no "an undefined spec is no longer reported — the remaining gap became invisible"

# A spec that RESTATES the implementation must be detected and NOT counted. Otherwise the
# coverage metric can be inflated by copying the body into the spec -- and that is not a
# hypothetical: hmac_sha256's registered Lean spec for `ch` is character-for-character its
# function body, so converting it to a definitional body would have manufactured exactly this.
grep -q "TRIVIAL: the spec RESTATES the implementation" "$RW/report.txt" \
  && ok "a spec that restates the implementation is flagged as trivial" \
  || no "a tautological refinement is not detected — coverage can be inflated by copying the body"
TRIVN="$(grep -c 'TRIVIAL: the spec RESTATES' "$RW/report.txt")"
REACHN="$(grep -oE '[0-9]+ reached a tier' "$RW/report.txt" | grep -oE '^[0-9]+')"
# 4 defined-spec refinements exist in the demo, one of them trivial; only 3 may be counted.
[ "$TRIVN" = "1" ] && [ "$REACHN" = "3" ] \
  && ok "the trivial one is EXCLUDED from the coverage count (3 reached, 1 trivial)" \
  || no "trivial refinements are being counted as coverage (trivial=$TRIVN reached=$REACHN)"

python3 - "$RW/report.txt" "$RW" <<'PYRF'
import re, sys
report, work = sys.argv[1], sys.argv[2]
txt = open(report).read()
for b in re.split(r"\n  \[", txt)[1:]:
    if "#refine" not in b.split("]")[0]: continue
    name = b.split("]")[0].split(".")[1].split("#")[0]
    def sect(mk):
        m = re.search(r"--- " + mk + r" ---\n(.*?)(?=\n    ---|\n\nREFINEMENT|\Z)", b, re.S)
        return m.group(1).rstrip() if m else None
    for mk, ext in [(r"lean:omega \(refine\)", ".lean"), (r"rocq:lia \(refine\)", ".v")]:
        v = sect(mk)
        if v: open(f"{work}/{name}{ext}", "w").write(v + "\n")
    iv = sect(r"isabelle:simp \(refine\)")
    if iv:
        d = work + "/n" if name == "bad_refine" else work
        open(f"{d}/Thy_{name}.thy", "w").write(iv.replace("theory Refines", f"theory Thy_{name}") + "\n")
open(f"{work}/ROOT", "w").write("session RF = HOL +\n  theories\n    Thy_double\n    Thy_triple\n")
open(f"{work}/n/ROOT", "w").write("session RFN = HOL +\n  theories\n    Thy_bad_refine\n")
PYRF

RLC=0
for t in double triple; do bounded_run 300 -- lake env lean "$RW/$t.lean" >/dev/null 2>&1 && RLC=$((RLC+1)); done
[ "$RLC" = "2" ] && ok "lean proved both refinements" || no "lean proved only $RLC of 2 refinements"
if bounded_run 300 -- lake env lean "$RW/bad_refine.lean" >/dev/null 2>&1; then
  no "lean CLOSED a body that does NOT refine its spec — portability made refinement lax"
else
  ok "lean refuses a body that does not refine its spec"
fi
if command -v coqc >/dev/null 2>&1; then
  RRC=0; RAX=0
  for t in double triple; do
    O="$( cd "$RW" && coqc "$t.v" 2>&1 )"
    echo "$O" | grep -q "Closed under the global context" && RAX=$((RAX+1))
    echo "$O" | grep -qiE "^error|error:" || RRC=$((RRC+1))
  done
  [ "$RRC" = "2" ] && ok "rocq proved both refinements" || no "rocq proved only $RRC of 2"
  [ "$RAX" = "2" ] && ok "and both are axiom-free" || no "only $RAX of 2 axiom-free"
  RBO="$( cd "$RW" && coqc bad_refine.v 2>&1 )"
  echo "$RBO" | grep -qiE "Tactic failure|cannot" \
    && ok "rocq refuses the non-refining body" || no "rocq did not refuse the non-refining body"
else
  inconc "coqc absent — refinement Rocq assertions not run"
fi
if command -v isabelle >/dev/null 2>&1; then
  printf '%s' "$( cd "$RW" && isabelle build -D . 2>&1 )" | grep -q "^Finished" \
    && ok "isabelle proved both refinements" || no "isabelle did not finish the refinement session"
  printf '%s' "$( cd "$RW/n" && isabelle build -D . 2>&1 )" | grep -qE "Failed to finish proof|FAILED" \
    && ok "isabelle refuses the non-refining body" || no "isabelle did not refuse it"
else
  inconc "isabelle absent — refinement HOL assertions not run"
fi
fi

echo "=== a spec BODY is checked, not just parsed ==="
# I added definitional bodies to `spec fn` and did not check them. A body referencing an
# undeclared variable and calling a nonexistent function was accepted SILENTLY -- the refinement
# tier would then render that nonsense into three kernels, where it fails as an unbound
# identifier: loud, but at the wrong layer and long after the mistake. Two rules, both from what
# a spec is (pure and total): only its own parameters, only calls to other spec fns.
SPW="$WORK/spec"; mkdir -p "$SPW"
cat > "$SPW/freevar.con" <<'CON'
mod sv { spec fn bogus(x: i32) -> i32 = undefined_var + x; }
CON
cat > "$SPW/execcall.con" <<'CON'
mod sv2 {
    fn runtime_fn(x: i32) -> i32 { return x; }
    spec fn calls_exec(x: i32) -> i32 = runtime_fn(x);
}
CON
cat > "$SPW/ok.con" <<'CON'
mod sv3 {
    spec fn inner(x: i32) -> i32 = x + 1;
    spec fn outer(x: i32) -> i32 = inner(x) + 1;
    spec fn pred(x: i32) -> bool = x > 0;
}
CON
# Output captured FIRST, then matched. `set -o pipefail` is on, so `failing_cmd | grep -q` reports
# failure even when grep MATCHES -- and a rejected program makes the compiler exit non-zero, so
# both of these assertions silently took the wrong branch and reported the fix as broken.
FV_OUT="$("$BIN" "$SPW/freevar.con" --report vcs 2>&1 || true)"
printf '%s' "$FV_OUT" | grep -q "references unknown name" \
  && ok "a spec body referencing an undeclared name is REJECTED" \
  || no "a spec body may reference undeclared names — nonsense reaches the kernels"
EC_OUT="$("$BIN" "$SPW/execcall.con" --report vcs 2>&1 || true)"
printf '%s' "$EC_OUT" | grep -q "calls non-spec function" \
  && ok "a spec body calling executable code is REJECTED (specs must not depend on runtime)" \
  || no "a spec body may call executable functions — the spec is no longer independent"
# And the rule must not be so strict it forbids legitimate specs: spec-to-spec calls are fine.
# SORT: a bool body for an int spec (and the reverse) must be rejected. This was accepted until
# 2026-08-06 -- the refinement tier then emitted `x = true` to three kernels, where it failed as a
# type error: right diagnosis, wrong layer.
cat > "$SPW/sort1.con" <<'CON'
mod s1 { spec fn wrong(x: i32) -> i32 = true; }
CON
cat > "$SPW/sort2.con" <<'CON'
mod s2 { spec fn wrong2(x: i32) -> bool = x + 1; }
CON
S1="$("$BIN" "$SPW/sort1.con" --report vcs 2>&1 || true)"
printf '%s' "$S1" | grep -q "wrong sort" \
  && ok "a boolean body for an integer spec is REJECTED" \
  || no "a bool body for an int spec is accepted — the kernels see the type error instead"
S2="$("$BIN" "$SPW/sort2.con" --report vcs 2>&1 || true)"
printf '%s' "$S2" | grep -q "wrong sort" \
  && ok "an integer body for a boolean spec is REJECTED (both directions)" \
  || no "an int body for a bool spec is accepted"

if "$BIN" "$SPW/ok.con" --report vcs >/dev/null 2>&1; then
  ok "a spec calling ANOTHER spec is still accepted (rule constrains, does not forbid)"
else
  no "spec-to-spec calls are rejected — the check is too strict to be usable"
fi

# --- CAPTURE CONTAINMENT ------------------------------------------------------
# `refineObligations` substitutes spec parameters by NAME (`substExpr`), with no capture
# avoidance. A spec body that BINDS a name can therefore have the binding substituted away:
#     spec fn s(x: i32) -> i32 = if x > 0 { let x: i32 = 100; x } else { x };
# instantiated at `s(y)` would yield `if y > 0 { let x = 100; y } else { y }` — a corrupted
# obligation. Today no wrong goal reaches a kernel only because `renderTerm` cannot render an
# if-expression, so the obligation reports "outside the fragment". That is safety by ACCIDENT:
# it holds on the renderer's narrowness, and adding `.ifExpr` to `renderTerm` makes it live.
#
# The check rejects binder-bearing bodies, and these two assertions are what keep it honest.
cat > "$SPW/capture.con" <<'EOF'
mod cap {
    spec fn s(x: i32) -> i32 = if x > 0 { let x: i32 = 100; x } else { x };

    #[ensures(result == s(y))]
    fn f(y: i32) -> i32 {
        return y;
    }
}
EOF
CAP="$("$BIN" "$SPW/capture.con" --report bool-kernel 2>&1 || true)"
printf '%s' "$CAP" | grep -q "binds name" \
  && ok "a spec body that BINDS a name is rejected (substitution is capture-unsafe)" \
  || no "a binder-bearing spec body is accepted — substExpr can corrupt the obligation"
printf '%s' "$CAP" | grep -qE "refine[0-9]" \
  && no "the corrupted obligation still reaches the refinement tier" \
  || ok "...and no refinement obligation is issued from it"

# A SHALLOW binder check is the likely regression: an if-expression's branches are `List Stmt`,
# so a `let` buried in a nested branch is exactly what a top-level-only check would miss.
cat > "$SPW/capture_deep.con" <<'EOF'
mod capd {
    spec fn s(x: i32) -> i32 = if x > 0 { if x > 1 { if x > 2 { let x: i32 = 9; x } else { x } } else { x } } else { x };

    #[ensures(result == s(y))]
    fn f(y: i32) -> i32 {
        return y;
    }
}
EOF
# Output captured BEFORE grep, deliberately: the binary exits nonzero when it reports an error,
# and under `set -o pipefail` a direct `"$BIN" ... | grep -q` is nonzero even when grep MATCHES.
# That reports a working check as broken.
DEEP="$("$BIN" "$SPW/capture_deep.con" --report bool-kernel 2>&1 || true)"
printf '%s' "$DEEP" | grep -q "binds name" \
  && ok "a binder nested three branches deep is caught (the check recurses into statements)" \
  || no "a deeply nested binder escapes — the binder check is shallow"

# NEGATIVE CONTROL, and the reason it is not optional: an earlier version of this check rejected
# every if-expression, binder or not. That contains the defect by banning the entire conditional
# fragment, which is over-containment -- by-name substitution is perfectly safe when nothing is
# bound. Without this assertion the check can silently regress to that state and still look green.
cat > "$SPW/nobind.con" <<'EOF'
mod nb {
    spec fn s(x: i32) -> i32 = if x > 0 { 1 } else { 2 };

    #[ensures(result == s(y))]
    fn f(y: i32) -> i32 {
        return y;
    }
}
EOF
NB="$("$BIN" "$SPW/nobind.con" --report bool-kernel 2>&1 || true)"
printf '%s' "$NB" | grep -q "error\[" \
  && no "a BINDER-FREE conditional is rejected — containment ate the useful fragment" \
  || ok "a binder-free conditional is still admitted (containment is targeted, not blanket)"

echo ""
echo "BOOL-KERNEL: PASS=$PASS  FAIL=$FAIL  INCONC=$INCONC"
bounded_gate_status && [ "$FAIL" -eq 0 ]
