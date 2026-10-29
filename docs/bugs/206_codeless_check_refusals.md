# Bug 206 — Check refusals built from a bare message carried no diagnostic code

<!-- bug-ledger
summary: twenty Check refusals built from a bare message (`throwCheckMsg`) and eleven literal diagnostics carried no code, so no rule or specification link could be attached to them. `Diagnostic.code` has no default and the helper is gone; each refusal names its rule's code (new: E0122 – E0124, E0312 – E0321). `check_diagnostic_codes_present.sh` and the `run_err` output check keep it so. Found while splitting E0420 (bug 203)
fixtures: error_206_newtype_field_other_than_zero.con error_206_newtype_constructor_arity.con error_206_receiverless_trait_method.con error_206_wrapping_on_non_integers.con error_206_print_without_arguments.con error_206_destructure_in_for_header.con error_206_test_with_parameter.con error_206_integer_requires.con error_206_spec_fn_calls_runtime_code.con
class: 11 — an empty diagnostic code was a valid value
silent: no
ruling: Outside the spec: tooling — Check refusals and literal diagnostics carried no diagnostic code
-->

**Status:** FIXED (branch `wip/bug206`, based on `main` `1b2dba60` with `wip/train6` `36bf7612` and `wip/bug203` merged)
**Found:** 2026-10-23, while splitting E0420 (bug 203): the `append()` refusal reached the user
from Check with no code at all, through the message-only helper `throwCheckMsg`, which eighteen
other Check refusals also used
**Severity:** a diagnostic-identity defect, not a soundness one. Every program involved was
refused, but the diagnostic named no code, so no rule tag or specification link could be attached
to it, and the specification's code mapping (check_spec S6) could not see the refusal at all.

Confirmed real by Codex review, 2026-10-25: Message-only diagnostics omitted required refusal codes, violating docs/spec/README.md’s coded-rejection contract; this is diagnostic failure, not unsound acceptance.

## Reproducer

```con pseudocode
newtype Meters = Int;
fn main() -> Int {
    let m: Meters = Meters(40, 2);
    return m.0;
}
```

Expected: a coded refusal (TYPE-9). Observed on `wip/train6` `36bf7612`:
`error[check]: newtype 'Meters' constructor takes exactly 1 argument`, with no code.

## Cause

`Concrete/Check/CheckHelpers.lean` defined `throwCheckMsg`, which built a `Diagnostic` from a
message alone, and `Diagnostic.code` defaulted to `""`, so a diagnostic without a code
typechecked anywhere. Twenty Check sites used the helper (builtin arities and argument types,
newtype constructors and fields, receiverless trait methods, a destructuring `let` in a `for`
header, `#[test]` shapes, internal binder-table failures), and eleven literal diagnostics in Check,
Verify, SSAVerify, Pipeline and FileSummary left the field out (spec functions and contract
clauses, reserved function names, a Copy struct after specialization, a function with no blocks,
unknown or private imports), and the module loader returned strings that `resolveFiles` wrapped in
one code-less diagnostic, so even a parse error inside a loaded file lost its E0001.

## Fix

`Diagnostic.code` has no default, so every construction site names a code, and `throwCheckMsg`
is gone: every Check refusal is a `CheckError` whose constructor fixes the code. The refusals
reuse the code of the rule they enforce where one existed (E0268 – E0270 builtin signatures,
E0201 unknown names, E0301 shadowed parameters, E0571 post-specialization Copy, E0578 reserved
names, E0701, E0110/E0111/E0284/E0285/E0418/E0419 imports) and take new codes otherwise: E0122 – E0124
(new MOD-4, loading module files; the loader now returns diagnostics, and a loaded file's parse
error keeps its own code), E0312 (internal), E0313 and E0317 (TYPE-9), E0314 and E0315 (TYPE-10), E0316 (TYPE-7),
E0318 (new PAT-8), E0319 (new STMT-10), E0320 and E0321 (CON-3, now partial). The gate
`check_diagnostic_codes_present.sh` keeps the field without a default, the helper absent, no
helper parameter defaulting a code, and every literal code well formed, with negative controls; `run_tests.sh` fails any `run_err` fixture whose
output holds an error without a code. Fixtures: nine `error_206_*.con`, one per new user-facing
code, taken from the specification's examples.

## Structural cause

Class 11, absence read as a fact: an empty code was a valid value, so a site that forgot its code
produced a well-formed diagnostic that no table could classify.

## Structural owner

The `Diagnostic` type (a code is a required field) and the specification's code mapping
(`docs/spec`, S6), with `check_diagnostic_codes_present.sh` for the parts types cannot hold.
