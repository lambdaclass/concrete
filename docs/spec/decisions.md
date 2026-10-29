# Open Owner Decisions

This chapter lists the language questions that only the project owner decides: OD1 to OD22 of
[ROADMAP.md](../../ROADMAP.md#owner-decisions), exit 2n. Each entry states the question and encodes
no answer. The rules it bears on are named in its rationale, together with what current main does,
which stays provisional until the owner answers. The answer, when given, is recorded in ROADMAP and
turns the entry into the rules it decides; until then no agent picks a side
([AGENTS.md](../project/AGENTS.md#rules)).

<a id="OPEN-1"></a>
### OPEN-1. The reference model

**OPEN-1@1.** Which places a safe reference may be formed from, and how loans are keyed (by the root variable or otherwise), is an open owner decision (OD1).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD1
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-5, REF-11. Current main: implemented provisionally in landing train 6 as REF-5 and REF-11. The options and the recommendation are in ROADMAP's row OD1; the owner's answer is recorded there.

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

<a id="OPEN-4"></a>
### OPEN-4. Foreign function values

**OPEN-4@1.** Whether an `extern fn` may be used as a value, now or through a typed foreign function type later, is an open owner decision (OD4).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD4
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on FFI-1. Current main: on main as FFI-1 (every such use refused). The options and the recommendation are in ROADMAP's row OD4; the owner's answer is recorded there.

<a id="OPEN-5"></a>
### OPEN-5. Generic parameters and Copy

**OPEN-5@1.** What an unbounded type parameter admits and how Copy is decided for a generic struct or enum are an open owner decision (OD5).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD5
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on OWN-D1, TYPE-13, TYPE-14. Current main: on main as OWN-D1, TYPE-13 and TYPE-14. The options and the recommendation are in ROADMAP's row OD5; the owner's answer is recorded there.

<a id="OPEN-6"></a>
### OPEN-6. Trait coherence

**OPEN-6@1.** How trait impls are kept coherent (one impl per trait and type, where an impl may be declared, how an ambiguous method call is resolved) is an open owner decision (OD6).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD6
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRAIT-1 to TRAIT-7. Current main: on main as TRAIT-1 to TRAIT-7 (bug 186's repair). The options and the recommendation are in ROADMAP's row OD6; the owner's answer is recorded there.

<a id="OPEN-7"></a>
### OPEN-7. An unsafe-module marker

**OPEN-7@1.** Whether a source marker such as `trusted mod` delimits the modules that may hold unsafe code, and what it confers, is an open owner decision (OD7).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD7
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-6. Current main: pending as TRUST-6 (R-0509). The options and the recommendation are in ROADMAP's row OD7; the owner's answer is recorded there.

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

<a id="OPEN-10"></a>
### OPEN-10. Arithmetic

**OPEN-10@1.** Which integer operations check, wrap or saturate by default, and whether an abort runs deferred calls, are an open owner decision (OD10).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD10
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-3, ARITH-4, STMT-2. Current main: on main as ARITH-3 and ARITH-4. The options and the recommendation are in ROADMAP's row OD10; the owner's answer is recorded there.

<a id="OPEN-11"></a>
### OPEN-11. Casts

**OPEN-11@1.** Which conversions `as` may write and which need a named operation is an open owner decision (OD11).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD11
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on ARITH-5, TRUST-1. Current main: partial as ARITH-5 (R-0500). The options and the recommendation are in ROADMAP's row OD11; the owner's answer is recorded there.

<a id="OPEN-12"></a>
### OPEN-12. Contracts

**OPEN-12@1.** How a contract expression is judged (purity, consumption, the arithmetic it means) is an open owner decision (OD12).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD12
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-1, CON-3. Current main: partial as CON-3 (R-0473). The options and the recommendation are in ROADMAP's row OD12; the owner's answer is recorded there.

<a id="OPEN-13"></a>
### OPEN-13. Raw-pointer ownership

**OPEN-13@1.** How a non-Copy value is moved out of raw memory, and what obligation a raw address carries, are an open owner decision (OD13).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD13
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-7. Current main: on main provisionally as TRUST-7 (bug 198). The options and the recommendation are in ROADMAP's row OD13; the owner's answer is recorded there.

<a id="OPEN-14"></a>
### OPEN-14. Type validity

**OPEN-14@1.** Whether one structural well-formedness judgment runs before specialization, and where a function type returning a reference is refused, are an open owner decision (OD14).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD14
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-2, REF-4. Current main: on main as REF-2 and REF-4. The options and the recommendation are in ROADMAP's row OD14; the owner's answer is recorded there.

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

<a id="OPEN-18"></a>
### OPEN-18. Match arm order and unreachable arms

**OPEN-18@1.** Which arm of a `match` is selected when several match, and whether an unreachable arm is refused, are an open owner decision (OD18).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD18
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-2, PAT-3. Current main: on main provisionally (bug 213: an unguarded catch-all arm is moved after the other arms). The options and the recommendation are in ROADMAP's row OD18; the owner's answer is recorded there.

<a id="OPEN-19"></a>
### OPEN-19. The provable subset

**OPEN-19@1.** Which constructs the provable subset of the language admits (discarded statements, guards, ranges) is an open owner decision (OD19).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD19
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on CON-1. Current main: open (PROVABLE_V1). The options and the recommendation are in ROADMAP's row OD19; the owner's answer is recorded there.

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

