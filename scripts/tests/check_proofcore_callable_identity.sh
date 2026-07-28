#!/usr/bin/env bash
# R-0442 / bug 061 — a locally bound callable is not a global name.
#
# `PExpr.call "f" args` used to mean BOTH "call the definition `f`" and "apply
# the fn-typed parameter `f`", and `eval` resolved both through the global
# `FnTable`. In the one place the project makes soundness claims, a parameter and
# a definition of the same spelling were the same thing.
#
# That is why `Option::map`, `Result::map` and `Result::map_err` hold only for
# their registered representative callback: the callback was bound in the GLOBAL
# table under the parameter's name. Their scope is `proof_coverage(representative)`
# and nothing may present them as universal callback theorems.
#
# The fix is two identities and two namespaces:
#   * `PExpr.call fn args`        — `fn` names a definition; answered by
#                                   `FnTable.globals` only.
#   * `PExpr.applyVar b args`     — `b` is a local binding; answered by
#                                   `FnTable.callables` only.
#
# So this gate checks that the DISTINCTION is real and observable, not merely
# that the code compiles: same spelling must reach different nodes, each form
# must have its own completeness predicate, the three representative proofs must
# still hold, and no surface may render them as universal.

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
COMPILER="${COMPILER:-.lake/build/bin/concrete}"
[ -x "$COMPILER" ] || { echo "error: build first ($COMPILER missing)" >&2; exit 2; }
TMP="$(mktemp -d)"; trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# The discriminating program: `f` is a DEFINITION and, in another function, the
# name of a PARAMETER. One spelling, two identities.
cat > "$TMP/same_spelling.con" <<'CON'
fn f(x: Int) -> Int { return x + 100; }

#[spec]
fn global_f(y: Int) -> Int { return f(y); }

#[spec]
fn param_f(f: fn(Int) -> Int, y: Int) -> Int { return f(y); }

fn main() -> Int { return global_f(1); }
CON

echo "=== the same spelling extracts to DIFFERENT nodes ==="
STUBS="$("$COMPILER" "$TMP/same_spelling.con" --report lean-stubs 2>&1)"
GLOBAL_NODE="$(awk '/Extracted from `main.global_f`/{f=1} f&&/PExpr :=/{getline; print; exit}' <<<"$STUBS")"
PARAM_NODE="$(awk '/Extracted from `main.param_f`/{f=1} f&&/PExpr :=/{getline; print; exit}' <<<"$STUBS")"

if grep -q '\.call "f"' <<<"$GLOBAL_NODE"; then
  ok "the definition call extracts to .call \"f\""
else
  no "the definition call did not extract to .call: $GLOBAL_NODE"
fi
if grep -q '\.applyVar "f"' <<<"$PARAM_NODE"; then
  ok "the parameter application extracts to .applyVar \"f\""
else
  no "the parameter application did not extract to .applyVar (bug 061): $PARAM_NODE"
fi
if [ -n "$GLOBAL_NODE" ] && [ "$GLOBAL_NODE" = "$PARAM_NODE" ]; then
  no "both forms produced the IDENTICAL node — the conflation is back"
else
  ok "the two nodes are not identical"
fi

echo "=== the fingerprint distinguishes them too ==="
FPS="$("$COMPILER" "$TMP/same_spelling.con" --report obligations 2>&1)"
if grep -qE 'fingerprint:.*\(call f ' <<<"$FPS" && grep -qE 'fingerprint:.*\(callptr f ' <<<"$FPS"; then
  ok "one 'call f' and one 'callptr f' fingerprint, same spelling"
else
  no "the fingerprints do not separate the two forms:
$(grep 'fingerprint:' <<<"$FPS")"
fi

echo "=== a parameter application takes NO dependency edge on the definition ==="
# It must not: the parameter is not that definition. A spurious edge would make a
# proof about the callback depend on, and stale with, an unrelated function.
if awk '/main\.param_f/{f=1} f&&/dependencies:/{print; exit}' <<<"$FPS" | grep -q "none"; then
  ok "param_f depends on nothing"
else
  no "param_f took a dependency edge: $(awk '/main\.param_f/{f=1} f&&/dependencies:/{print; exit}' <<<"$FPS")"
fi

echo "=== each form has its OWN completeness predicate ==="
# One predicate over one table is what let a parameter be 'resolved' by a
# definition. Two collectors, two predicates, two namespaces.
probe() {
  local label="$1" want="$2" body="$3"
  cat > "$TMP/probe.lean" <<LEAN
import Concrete.Proof.ProofSoundness
open Concrete.Proof Concrete.ProofSoundness
$body
LEAN
  local out; out="$(env LEAN_PATH=.lake/build/lib/lean lean "$TMP/probe.lean" 2>&1)"
  if grep -q "$want" <<<"$out"; then
    ok "$label"
  else
    no "$label — got: $(printf '%s' "$out" | tr '\n' ' ' | head -c 300)"
  fi
}

# `pexprCalls` must NOT report an applied parameter as a definition name;
# otherwise fnTableComplete would demand a global for every callback.
probe "pexprCalls ignores an applied local binding" "true" \
'#eval (pexprCalls (.applyVar "f" [.var "x"])).isEmpty'
# `pexprApplies` must report it.
probe "pexprApplies reports the applied local binding" '\["f"\]' \
'#eval pexprApplies (.applyVar "f" [.var "x"])'
# ...and the mirror: a definition call is not an application of a local.
probe "pexprApplies ignores a definition call" "true" \
'#eval (pexprApplies (.call "f" [.var "x"])).isEmpty'
probe "pexprCalls reports the definition call" '\["f"\]' \
'#eval pexprCalls (.call "f" [.var "x"])'

echo "=== a definition can NEVER answer an application of a local binding ==="
# The negative half of R-0442, and the direct refutation of bug 061. With `f`
# defined as a GLOBAL, `.applyVar "f"` must be stuck.
probe "a global f does not satisfy .applyVar f" "none" \
'def gf : String → Option PFnDef
  | "f" => some { name := "f", params := ["x"], body := .lit (.int 7) }
  | _ => none
#eval eval (FnTable.ofGlobals gf) Env.empty 5 (.applyVar "f" [.lit (.int 1)])'
# ...while the same table answers the definition call, so the check above is not
# just "everything is stuck".
probe "the same global f DOES satisfy .call f" "int 7" \
'def gf2 : String → Option PFnDef
  | "f" => some { name := "f", params := ["x"], body := .lit (.int 7) }
  | _ => none
#eval eval (FnTable.ofGlobals gf2) Env.empty 5 (.call "f" [.lit (.int 1)])'
# ...and a callable-bound f answers the application.
probe "a callable-bound f satisfies .applyVar f" "int 7" \
'def cf : String → Option PFnDef
  | "f" => some { name := "f", params := ["x"], body := .lit (.int 7) }
  | _ => none
#eval eval (FnTable.withCallables (fun _ => none) cf) Env.empty 5 (.applyVar "f" [.lit (.int 1)])'
# ...and symmetrically a callable does NOT answer a definition call.
probe "a callable-bound f does not satisfy .call f" "none" \
'def cf2 : String → Option PFnDef
  | "f" => some { name := "f", params := ["x"], body := .lit (.int 7) }
  | _ => none
#eval eval (FnTable.withCallables (fun _ => none) cf2) Env.empty 5 (.call "f" [.lit (.int 1)])'

echo "=== the three representative proofs still hold ==="
# They are the reason extraction may not simply refuse an indirect callee, and
# the reason the callback is bound at all. If they break, the model lost them.
for t in option_map_correct result_map_correct result_map_err_correct; do
  if grep -rq "theorem $t" proofs/Examples/PureCore/Proofs.lean; then
    ok "$t is still present"
  else
    no "$t disappeared from the PureCore proofs"
  fi
done
# ...and they must go through the CALLABLE namespace now, not the global one.
if grep -q "pureCoreCallables" Concrete/Proof/Proof.lean \
   && ! grep -q "pureCoreFnsGlobals" Concrete/Proof/Proof.lean; then
  ok "the representative callback is bound as a callable, not a global"
else
  no "the representative callback is still bound in the global namespace (bug 061)"
fi
if grep -q 'applyVar "f"' Concrete/Proof/Proof.lean; then
  ok "the HOF specs apply their callback as a local binding"
else
  no "the HOF specs still spell their callback application as a definition call"
fi

echo "=== the specs' global namespace is empty, and checkably so ==="
# These specs call no definitions. An empty `globals` makes that a fact rather
# than an accident, and means nothing can quietly start resolving there.
probe "pureCoreFns binds no globals" "none" \
'#eval pureCoreFns.globals "f"'
# ...while the callable namespace DOES bind it.
probe "pureCoreFns binds f as a callable" "some" \
'#eval pureCoreFns.callables "f"'

echo "=== no surface renders a representative proof as universal ==="
# A report that printed a parameter application identically to a definition call
# would let a reader upgrade `proof_coverage(representative)` by eye.
if grep -q 'applyVar binding args' Concrete/Report/Report.lean; then
  ok "the report renders .applyVar with its own case"
else
  no "the report has no distinct rendering for .applyVar"
fi
# Scoped to claims about the CALLBACK. `unwrap_or` is legitimately universal over
# its payload, so a bare grep for "universal" flags correct documentation — the
# hazard is specifically presenting the callback as arbitrary.
if grep -rniE "(universal|arbitrary|every|any)[^.]{0,40}(callback|callable|\bf\b)|\b(callback|callable)[^.]{0,40}(universal|arbitrary)" \
     proofs/Examples/PureCore/Proofs.lean | grep -viE "not |NOT |gap|limitation|representative|gap\)" | grep -q .; then
  no "the PureCore proofs present the callback as arbitrary:
$(grep -rniE "(universal|arbitrary|every|any)[^.]{0,40}(callback|callable|\bf\b)" proofs/Examples/PureCore/Proofs.lean | grep -viE "not |gap|limitation|representative")"
else
  ok "no PureCore proof presents the callback as arbitrary"
fi
# Each of the three must SAY it is representative-scoped.
for t in option_map result_map result_map_err; do
  if awk -v t="theorem ${t}_correct" '
      /\/--/{buf=""} {buf=buf" "$0} index($0,t){print buf; exit}' \
      proofs/Examples/PureCore/Proofs.lean | grep -qi "representative"; then
    ok "${t}_correct states its representative scope"
  else
    no "${t}_correct does not state that its callback is a representative"
  fi
done
if grep -q "representative" proofs/Examples/PureCore/Proofs.lean; then
  ok "the representative scope is stated in the proofs"
else
  no "the representative scope is no longer stated"
fi

echo "=== structurally, the two identities exist and cannot be coerced ==="
if grep -qE '\| applyVar \(binding : String\)' Concrete/Proof/Proof.lean; then
  ok "PExpr.applyVar exists"
else
  no "PExpr.applyVar is gone — identity moved back into a shared node"
fi
if grep -qE '^structure FnTable' Concrete/Proof/Proof.lean \
   && grep -qE '^  callables :' Concrete/Proof/Proof.lean; then
  ok "FnTable carries two namespaces"
else
  no "FnTable no longer separates globals from callables"
fi
if grep -qE 'instance.*CoeFun FnTable' Concrete/Proof/Proof.lean; then
  no "a CoeFun on FnTable exists — an implicit application would silently mean 'globals'"
else
  ok "no CoeFun on FnTable (every application names its namespace)"
fi

echo
echo "PROOFCORE-CALLABLE-IDENTITY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
