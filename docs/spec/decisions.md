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

<a id="OPEN-2"></a>
### OPEN-2. `&mut` arguments, deferred loans and exit order

**OPEN-2@1.** How a `&mut` argument is passed on (reborrowed or moved), whether overlapping exclusive arguments are refused, how long a deferred call keeps its loan and in what order cleanup, loan end and write-back happen are an open owner decision (OD2).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD2
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on REF-10, REF-11, OWN-12. Current main: the reborrow is on main as REF-10; the defer and exit-order rules are not implemented (bugs 305 and 306 wait on this item). The options and the recommendation are in ROADMAP's row OD2; the owner's answer is recorded there.

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

<a id="OPEN-9"></a>
### OPEN-9. Modules and interfaces

**OPEN-9@1.** How a module's interface is stated and produced (one file per module, `pub`, generated or handwritten interface artifacts) is an open owner decision (OD9).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD9
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-2, MOD-4. Current main: not implemented beyond MOD-2 and MOD-4. The options and the recommendation are in ROADMAP's row OD9; the owner's answer is recorded there.

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

<a id="OPEN-20"></a>
### OPEN-20. Visibility of std's internal bindings

**OPEN-20@1.** Whether std's internal `std.libc` bindings are visible to programs is an open owner decision (OD20).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD20
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-2. Current main: open (bug 226). The options and the recommendation are in ROADMAP's row OD20; the owner's answer is recorded there.

<a id="OPEN-21"></a>
### OPEN-21. A parent module's rights over its child

**OPEN-21@1.** Whether a parent module may construct its child's newtypes and read or write its child's private fields is an open owner decision (OD21).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD21
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-11. Current main: on main provisionally: a parent has these rights (bug 262). The options and the recommendation are in ROADMAP's row OD21; the owner's answer is recorded there.

<a id="OPEN-22"></a>
### OPEN-22. Trust on an impl block

**OPEN-22@1.** Whether trust is declared on an `impl` block or only on each `trusted fn` is an open owner decision (OD22).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD22
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-5. Current main: on main provisionally: `trusted impl` is refused and trust is per method (bug 189), while TRUST-5@1 still names `trusted impl`. The options and the recommendation are in ROADMAP's row OD22; the owner's answer is recorded there.
