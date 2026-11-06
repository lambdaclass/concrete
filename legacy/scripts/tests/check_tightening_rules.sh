#!/usr/bin/env bash
# The 2026-10-23 tightening batch, as one fast gate (landing train 6).
#
# Each slice of the batch has a reproducer that failed before its fix: bugs 136/137 (literal
# evaluation order), 165 (generated helper calls bound by spelling), 166 (an `extern fn` taken as a
# value, D4 / FFI-1), 183 (dereference-assignment order), 185 (a monomorphized instance lost its
# authority), 190 (an array value aliased its source), 198 (T-RAWDUP / TRUST-7), 199 (a cast to an
# alias), the reference well-formedness gaps G2, G13, G15 and G16, and decisions D1 (REF-10), D2
# (REF-5 / REF-11), D3 (REF-9, the G17 Copy case, REF-6 struct destructuring) and D6 (OWN-D1@2 /
# TYPE-14). run_tests.sh keeps its rows for all of them; this gate exists so the mutation campaign
# (check_gate_mutation_coverage.sh) has one named gate whose rows kill each slice's mutation, at
# the cost of seconds rather than a full suite.
#
# A reject row requires the code; a run row requires the binary's exit status AND the interpreter's
# printed result (the two consumers compared with the stated value, never only with each other).
# Rows carrying a rule tag are checked against docs/spec by check_rules_2i.sh and check_spec.sh;
# they are not repeated here.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/fresh.sh"
require_fresh_binary || exit 1
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
CC=".lake/build/bin/concrete"
[ -x "$CC" ] || { echo "error: build first ($CC missing)" >&2; exit 2; }
T="tests/programs"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# rejects <fixture> <code>: the compile fails with that code.
rejects(){ local f="$T/$1.con" code="$2" out rc=0
  [ -f "$f" ] || { no "$1: fixture missing"; return; }
  out="$("$CC" "$f" -o "$TMP/bin" 2>&1)" || rc=$?
  if [ "$rc" -ne 0 ] && grep -qF "($code)" <<<"$out"; then ok "$1 rejected ($code)"
  else no "$1: want ($code), rc $rc: $(grep -m1 'error' <<<"$out")"; fi; }

# runs <fixture> <value> [stdout]: the binary exits <value>, and --interp prints <value> last
# (after the same stdout when given).
runs(){ local f="$T/$1.con" want="$2" stdout="${3-}" n=-1 bout="" iout
  [ -f "$f" ] || { no "$1: fixture missing"; return; }
  rm -f "$TMP/bin"
  if "$CC" "$f" -o "$TMP/bin" >"$TMP/build.log" 2>&1; then
    bout="$("$TMP/bin" </dev/null 2>/dev/null)" && n=0 || n=$?
  fi
  iout="$("$CC" "$f" --interp </dev/null 2>&1 | tail -1)"
  if [ "$n" = "$want" ] && [ "$iout" = "$want" ] && { [ -z "$stdout" ] || [ "$bout" = "$stdout" ]; }; then
    ok "$1: binary $n, --interp $iout"
  else
    no "$1: want $want${stdout:+ with stdout '$stdout'}; binary exit $n stdout '$bout', --interp '$iout' $(grep -m1 error "$TMP/build.log")"
  fi; }

echo "=== bugs 136/137, 183, 190: evaluation order and array values ==="
runs regress_136_137_literal_eval_order 42
runs regress_183_deref_assign_rhs_first 129
runs regress_190_array_value_copy 42

echo "=== bug 165: generated helper calls carry the helper's identity ==="
runs regress_165_generated_helper_call 42 "7 hi true x n=hi ok"

echo "=== bug 166 (D4, FFI-1): an extern fn is never a value ==="
for f in error_166_foreign_fn_value_repr_c error_166_foreign_fn_value_repr_c_let error_166_foreign_fn_value_scalar \
         error_166_imported_extern_value error_166_extern_value_in_const error_166_extern_value_after_scope; do
  rejects "$f" E0588
done
runs regress_166_wrapper_value 42

echo "=== bug 185: an instance keeps its definition's authority ==="
rejects error_185_generic_rawptr_eq_safe E0229
runs regress_185_generic_rawptr_eq_authority 42

echo "=== bug 198 (T-RAWDUP, TRUST-7) and bug 199 ==="
rejects error_rawdup_trusted_deref E0311
runs regress_198_read_owned 42
runs regress_199_cast_to_alias 42

echo "=== D1 (REF-10): one argument mode for &mut ==="
runs regress_d1_mut_ref_reborrow 42
runs regress_d1_defer_branches_and_loop_exit 2
runs regress_d1_append_ref_twice 4
rejects error_d1_defer_holds_loan E0206
rejects error_d1_defer_holds_loan_param E0206
rejects error_d1_overlapping_mut_args E0293

echo "=== D2 (REF-5, REF-11): a reference has a variable root; loans are keyed by it ==="
for f in error_d2_borrow_literal error_d2_borrow_field_of_temporary error_d2_temporary_receiver \
         error_d2_mut_borrow_string_literal error_d2_branch_receiver; do
  rejects "$f" E0238
done
for f in error_d2_call_disjoint_fields error_d2_call_field_read error_d2_receiver_projection_read \
         error_d2_call_field_read_in_branch error_d2_explicit_mut_receiver_read; do
  rejects "$f" E0293
done
runs regress_d2_roots_and_projections 42

echo "=== D3 (REF-9, REF-8 G17 Copy case, REF-6): a binder through a reference is a reference ==="
runs regress_181_borrowed_binders 42
for f in error_181_root_reassigned_match error_181_root_reassigned_while_let error_181_root_reassigned_guard \
         error_181_root_reassigned_nested error_181_root_reassigned_whole_binder error_181_root_reassigned_defer; do
  rejects "$f" E0232
done
rejects error_2i_ref6_struct_destructure_through_ref E0302
runs regress_2i_ref6_struct_fields_through_ref 42

echo "=== G2, G13, G15, G16 (REF-1, REF-2): one reference well-formedness judgment ==="
rejects error_2i_ref1_trait_decl_returns_ref E0236
runs regress_2i_ref1_trait_decl_value 42
rejects error_2i_ref1_extern_param_ptr_to_ref E0574
rejects error_2i_ref1_extern_ptr_to_ref E0575
runs regress_2i_ref1_extern_ptr_to_value 42
rejects error_2i_ref2_unused_alias_fn_type_returns_ref E0236
rejects error_2i_ref2_field_fn_type_returns_ref E0236
runs regress_2i_ref2_callback_field 42
rejects error_2i_ref1_transitive_generic E0236

echo "=== D6 (OWN-D1@2, TYPE-14): Copy of a generic is declared ==="
rejects error_d6_copy_generic_field_not_copy E0571
rejects error_d6_copy_enum_payload_not_copy E0571
rejects error_d6_phantom_arg_not_copy E0208
runs regress_d6_copy_generics 42

echo ""
echo "TIGHTENING-RULES: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
