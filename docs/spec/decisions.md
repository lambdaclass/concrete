# Open Owner Decisions

This chapter lists the language questions that only the project owner decides: OD1 to OD22 of
[ROADMAP.md](../../ROADMAP.md#owner-decisions), exit 2n. Each entry states the question and encodes
no answer. The rules it bears on are named in its rationale, together with what current main does,
which stays provisional until the owner answers. The answer, when given, is recorded in ROADMAP and
turns the entry into the rules it decides; until then no agent picks a side
([AGENTS.md](../project/AGENTS.md#rules)).

<a id="OPEN-3"></a>
### OPEN-3. Pattern binders through a borrowed scrutinee

**OPEN-3@1.** The type and mode of a pattern binder that binds through a borrowed scrutinee, and whether the root may be reassigned while such a binder lives, are an open owner decision (OD3).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD3
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-9, PASS-2. Current main: on main as REF-9 (every such binder has reference type). The options and the recommendation are in ROADMAP's row OD3; the owner's answer is recorded there.

<a id="OPEN-8"></a>
### OPEN-8. Global state

**OPEN-8@1.** Whether a program may hold mutable global state, and in what form, is an open owner decision (OD8).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD8
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-6. Current main: pending as TRUST-6 (R-0509). The options and the recommendation are in ROADMAP's row OD8; the owner's answer is recorded there.

<a id="OPEN-15"></a>
### OPEN-15. Freezing under a shared borrow and `&mut` exclusivity

**OPEN-15@1.** Whether a shared borrow freezes its whole owner, and what passes may assume about `&mut` exclusivity, are an open owner decision (OD15).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD15
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-8, REF-11, REF-12. Current main: on main as REF-8, REF-11 and REF-12. The options and the recommendation are in ROADMAP's row OD15; the owner's answer is recorded there.

<a id="OPEN-16"></a>
### OPEN-16. A struct literal in a condition head

**OPEN-16@1.** How a struct literal written in an `if`, `while` or `match` head is parsed or refused is an open owner decision (OD16).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD16
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-1. Current main: on main provisionally as part of SYN-1 (bug 212). The options and the recommendation are in ROADMAP's row OD16; the owner's answer is recorded there.

<a id="OPEN-17"></a>
### OPEN-17. Field writes and `mut`

**OPEN-17@1.** Whether writing a field requires the binding to be declared `mut` is an open owner decision (OD17).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD17
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on OWN-10. Current main: open: today a field write on a binding not declared `mut` is accepted (bug 216). The options and the recommendation are in ROADMAP's row OD17; the owner's answer is recorded there.

<a id="OPEN-23"></a>
### OPEN-23. Inline submodules under one file per module

**OPEN-23@1.** Whether an inline submodule (`mod m { ... }` written inside its parent's file) is compatible with OD9's one file per module is an open owner decision (OD23).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD23
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-4 and MOD-7. Current main: inline submodules are accepted, and most
examples of this specification use them. Raised while recording OD9 (2026-10-29). The options are in
ROADMAP's row OD23; the owner's answer is recorded there.

## Decided

These entries were retired when the owner answered them; their IDs are never reused.

- **OPEN-1** (OD1, references): decided 2026-10-29. Now REF-5 (formation, `&s.f`, no temporary root), REF-1 and REF-3 (second-class), TRUST-3 (the raw-pointer validity premise, pending) and REF-8 (loans keyed by the root variable; the keying waits on OD15).
- **OPEN-4** (OD4, foreign function values): decided 2026-10-29. Now FFI-1.
- **OPEN-5** (OD5, generics and Copy): decided 2026-10-29. Now OWN-D1, TYPE-13 and TYPE-14.
- **OPEN-6** (OD6, trait coherence): decided 2026-10-29. Now TRAIT-1 to TRAIT-7.
- **OPEN-7** (OD7, an unsafe-module marker): decided 2026-10-29. Now TRUST-6@2 (pending, R-0509).
- **OPEN-10** (OD10, arithmetic): decided 2026-10-29. Now ARITH-3, ARITH-4 and STMT-2 (pending).
- **OPEN-11** (OD11, casts): decided 2026-10-29. Now ARITH-5 (partial) and TRUST-1 (pending).
- **OPEN-12** (OD12, contracts): decided 2026-10-29. Now CON-1 (pending) and CON-3 (partial).
- **OPEN-13** (OD13, raw-pointer ownership): decided 2026-10-29. Now TRUST-7.
- **OPEN-14** (OD14, type validity): decided 2026-10-29. Now REF-2 and REF-4 (the single judgment before specialization is R-0499's).
- **OPEN-18** (OD18, match arm order and unreachable arms): decided 2026-10-29. Now PAT-9 (pending, R-0500).
- **OPEN-19** (OD19, the provable subset): decided 2026-10-29. Now the scope of [PROVABLE_V1.md](../verification/PROVABLE_V1.md), not a language rule.
- **OPEN-2** (OD2, `&mut` arguments, deferred loans and the exit order): decided 2026-10-29. Now
  REF-10@2 (lending), REF-11@3 (exclusivity at a call by root plus path), REF-13 (a `&mut` refers to the
  place), OWN-16 (a deferred call's borrow lives until it runs) and STMT-11 (one exit sequence).
- **OPEN-9** (OD9, modules and interfaces): decided 2026-10-29. Now MOD-7 (pending, R-0556).

- **OPEN-20** (OD20, the visibility of std's `std.libc` bindings): decided 2026-10-29, private to std.
  Now MOD-6 of [modules.md](modules.md#MOD-6) (pending, bug 226).
- **OPEN-21** (OD21, a parent module's rights over its child): decided 2026-10-29, only the declaring
  module. Now TYPE-11 of [types.md](types.md#TYPE-11), enforced as written (the parent case is pending,
  R-0011).
- **OPEN-22** (OD22, trust on an `impl` block): decided 2026-10-29, per method only. Now TRUST-5@2 of
  [trusted.md](trusted.md#TRUST-5).

