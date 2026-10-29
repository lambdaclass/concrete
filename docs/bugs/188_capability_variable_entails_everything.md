# Bug 188: a capability variable satisfied every capability requirement

<!-- bug-ledger
summary: a caller holding ANY capability variable was treated as covering every requirement, `Unsafe` included (`capsContain`: "resolved at instantiation", which never happens for a generic body), so `fn f<cap C>(..) with(C)` printed, called `with(Process)`, called an untrusted extern and dereferenced raw pointers under a `main` declaring nothing. One entailment rule now (`Capabilities.missingCaps`): a variable entails only itself, unknown variables fail closed, a callback's `Unsafe` is decided on the instantiated set, and a capability parameter may not be spelled like a capability (E0121). Pinned by nine `error_188_*.con` witnesses (E0520/E0521/E0240/E0121/E0001), the positive control `regress_188_capvar_polymorphism.con` (binary == `--interp`), `check_capability_judgment.sh` and five mutation families ((silent authority violation))
fixtures: error_188_capvar_console.con error_188_capvar_process.con error_188_capvar_unsafe_call.con error_188_capvar_unsafe_op.con error_188_capvar_unsafe_callback.con error_188_capparam_named_unsafe.con error_188_capvar_method_unsafe_callback.con error_188_capvar_impl_receiver.con error_188_bodyless_capparam.con regress_188_capvar_polymorphism.con
class: 6 — a capability variable entailed every requirement, deferred to an instantiation nothing checks
silent: yes
exit: 0b
ruling: Violates: TRUST-4@1
-->

**Status:** FIXED
**Found:** 2026-10-23, by a Codex review of `Concrete/Semantics/Capabilities.lean`, confirmed by a
second (Claude) review and reproduced on the build server at ee1fff8e.
**Severity:** silent authority violation. Accepted programs performed effects (Console, Process),
called untrusted foreign code and dereferenced raw pointers that no function in the program was
granted. The binary ran.

Confirmed real by Codex review, 2026-10-25: A capability variable granted undeclared Console/Unsafe authority, violating docs/spec/trusted.md TRUST-4 and HANDLE_CAPABILITIES.md’s caller-supplied authority contract.

## Observed

```
fn sneaky<cap C>(f: fn(i32) with(C) -> i32, x: i32) with(C) -> i32 {
    print_int(x as i64);
    return f(x);
}

fn pure_id(x: i32) -> i32 { return x; }

fn main() -> i32 {
    return sneaky(pure_id, 42);
}
```

At ee1fff8e this compiled, printed 42 and exited 42. `main` declares nothing and instantiates
`C := {}`; `sneaky` declares only `C`. Five more shapes behaved the same (all compiled, all exited
42 natively; `--interp` agreed where the interpreter models the call):

- a `with(C)` body calling a `trusted extern fn getpid() with(Process)`;
- a `with(C)` body calling an untrusted `extern fn abs(..) with()`, which requires `Unsafe`;
- a `with(C)` body dereferencing a raw pointer (the `Unsafe` operation gate);
- a pure `main` calling `apply<cap C>(magnitude, ..)` where `magnitude` is `with(Unsafe)`: the
  callback instantiates `C := Unsafe`, and nothing refused the call;
- `fn launder<cap Unsafe>(f: fn(i32) with(Unsafe) -> i32, p: *const i32) with(Unsafe)`: a
  capability parameter spelled like a capability.

## Expected

A requirement is satisfied by a concrete capability the caller declares, or by a capability
variable only when the variable's declared bound entails it. A capability parameter declares no
bound, so it entails only itself. Each witness above is refused.

## Structural cause

`Capabilities.capsContain`, the one capability-superset primitive, answered "covered" for every
requirement whenever the caller's set contained ANY capability variable
(`!callerVars.isEmpty || ...`), and for every required variable (`| .var _ => true`). Its comment
gave the reason: the variable "is resolved at instantiation". It is not. A generic body is checked
once, on the generic definition. Nothing re-checks it at the instantiation, and capability
parameters are not specialized by Mono, so `verifyCapsPostMono` skips any set that names a
variable. The assumption deferred the decision to a stage that never makes it.

Two smaller defects compounded it:

1. Check deliberately left `Unsafe` to CoreCheck at a call to a capability-polymorphic callee,
   because CoreCheck owns the trusted-body discharge. CoreCheck sees that call without the
   inferred binding, so a callback's `Unsafe` reaching the callee through `C` was decided by no
   one (the fifth witness).
2. Inside a fn-pointer type a capability variable is spelled as a plain name (`fn() with(C)`),
   so the rule matches a requirement against the caller's variables by name. Nothing kept
   capability-parameter names apart from capability names (the sixth witness).

The function had two readings of one fact: `missingCaps` matched variables by name and never
treated a caller variable as a blanket, while `capsContain` did. `decideCall` combined them, so
the per-capability view and the satisfaction view could already disagree.

## Structural owner

Class 6 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md): capability facts not carried across
dispatch, specialization or package boundaries. Owner R-0499 (S8 capability containment,
re-checked after Mono). This instance is the specialization boundary: a decision deferred to an
instantiation that is never checked.

## Fix

One entailment rule, `Capabilities.missingCaps`, with `capsContain` as its boolean view:

- a required capability is entailed by a concrete capability of the same name, or by a capability
  variable of the same name (a variable entails exactly itself); any other required name,
  variable or concrete, is missing (fail closed);
- direct calls, calls through a function pointer (`missingCapsThroughPtr` is gone, merged into
  the one rule), method calls, calls through a trait bound and the raw-operation gate
  (`capsAllowUnsafeOp`) all read it;
- CoreCheck checks a direct call against the callee's declared concrete set
  (`instantiatedRequirement`); the callee's own capability parameters are instantiated at the
  call site;
- Check decides a call to a capability-polymorphic callee (function or method) on the
  INSTANTIATED set, `Unsafe` included, with the same trusted-body discharge as CoreCheck
  (`trustedDischarges`). Uninferable variables were already refused (`cannotInferCapVariable`);
- Resolve refuses a capability parameter spelled like a built-in capability, `Std` or a module
  capability alias (E0121), so name matching cannot launder a real capability;
- the parser refuses capability parameters on a bodiless declaration (a foreign binding or an
  intrinsic). It has no call-site instantiation, so `trusted fn f<cap C>() with(C);` dropped
  its parameters and its `with(C)` was read as the callee's own, instantiated parameter:
  callable with no authority (found by the Codex review of this fix);
- Check compares a capability-polymorphic call's parameter types under the INFERRED bindings,
  as the method path already did. It rebuilt them from the whole resolved set minus names
  equal to a callee parameter, which refused two legitimate shapes with a false E0220:
  forwarding a variable to a callee whose parameter has the same name, and a callee declaring
  `with(C, Unsafe)` with a pure callback (Codex review).

The decision is made before specialization, on the generic definition. `verifyCapsPostMono` stays
as defense in depth.

`std.ordered_set`'s `fold` and `for_each` relied on the hole: they walk the inner map's key buffer
by raw pointer and declared only `with(C)`. They now declare `trusted` (the audit of a raw
operation is explicit), which is the only change the rule required outside the witnesses
(`scripts/tools/rule_impact.sh`, 2167 units: 6 newly refused, all of them this bug's fixtures).

## Containment

- `tests/programs/error_188_capvar_{console,process,unsafe_call}.con` (E0520),
  `error_188_capvar_unsafe_op.con` (E0521), `error_188_capvar_unsafe_callback.con` (E0240),
  `error_188_capparam_named_unsafe.con` (E0121), and from the Codex review
  `error_188_capvar_method_unsafe_callback.con` (E0240, the method path),
  `error_188_capvar_impl_receiver.con` (E0520, an `impl<cap C>` method) and
  `error_188_bodyless_capparam.con` (E0001), in `run_tests.sh`.
- `tests/programs/regress_188_capvar_polymorphism.con`: the positive control (a variable entails
  itself, forwarding, a concrete capability beside a variable, a trusted capability-polymorphic
  body, a callback instantiated with authority the caller holds). Binary exit 42, and the
  `--interp == binary` loop in `run_tests.sh` agrees.
- `check_capability_judgment.sh`, section "a capability VARIABLE entails only itself": every
  witness with its code, the positive control with its `--interp` leg, and a fn-pointer row.
- Mutation families `capvar-entails-only-itself`, `capvar-instantiated-unsafe`,
  `capvar-method-instantiated-unsafe`, `bodyless-capparams-refused` and `capparam-name-reserved`
  (`check_gate_mutation_coverage.sh`) restore each leg of the hole and must be killed by that
  gate.

## Left over

- Capability variables have no bound syntax. The rule is written so a declared bound would entail
  through it; none exists today, and none is needed by any workload.
- `Unsafe` stays a capability. Separating effects from safety obligations is the later trust-split
  slice.
