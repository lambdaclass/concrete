# Open Owner Decisions

This chapter lists the language questions that only the project owner decides: OD1 to OD42 of
[ROADMAP.md](../../ROADMAP.md#owner-decisions), exit 2n. Each entry states the question and encodes
no answer. The rules it bears on are named in its rationale, together with what current main does,
which stays provisional until the owner answers. The answer, when given, is recorded in ROADMAP and
turns the entry into the rules it decides; until then no agent picks a side
([AGENTS.md](../project/AGENTS.md#rules)). OD1 to OD22 were answered on 2026-10-29 and are listed under
Decided; OD23 to OD32 were decided the same day; OPEN-33 to OPEN-42, raised while recording them, are open.

<a id="OPEN-33"></a>
### OPEN-33. Inline submodules under one file per module

**OPEN-33@1.** Whether an inline submodule (`mod m { ... }` written inside its parent's file) is compatible with OD9's one file per module is an open owner decision (OD33).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD33
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-4 and MOD-7. Current main: inline submodules are accepted, and most
examples of this specification use them. Raised while recording OD9 (2026-10-29). The options are in
ROADMAP's row OD33; the owner's answer is recorded there.

<a id="OPEN-34"></a>
### OPEN-34. A struct literal inside brackets in a condition head

**OPEN-34@1.** Whether a struct literal written inside a call's argument list, an index or an array literal within an `if`, `while` or `match` head counts as parenthesized under SYN-4 is an open owner decision (OD34).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD34
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on SYN-4. Current main: `if same(p, P { v: 1 }) { ... }` is accepted (an
uppercase struct literal is read anywhere in a head). The grammar distinguishes a grouped expression
`( expr )` from the parentheses of a call's arguments, and OD16's answer does not say which one it
means. Raised while recording OD16 (2026-10-29). The options are in ROADMAP's row OD34; the owner's
answer is recorded there.

<a id="OPEN-35"></a>
### OPEN-35. How an array satisfies a `Destroy` bound

**OPEN-35@1.** Whether arrays satisfy a `Destroy` bound through one impl written in std or through one language impl that is its own entry is an open owner decision (OD35).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD35
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRAIT-10. OD28's answer allows either form and does not choose; current main
counts an array of destroyable elements as `Destroy` with no impl (bug 324). The options and a
recommendation are in ROADMAP's row OD35; the owner's answer is recorded there.

<a id="OPEN-36"></a>
### OPEN-36. The explicit place form that opens a heap cell

**OPEN-36@1.** The spelling of the explicit non-consuming place form that opens a `Heap` or `HeapArray` for a field access is an open owner decision (OD36).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD36
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MEM-4. OD30's answer requires such a form and keeps `*` as the consuming
move-out, but names no spelling; current main opens one heap layer implicitly (bug 326). The options
and a recommendation are in ROADMAP's row OD36; the owner's answer is recorded there.

<a id="OPEN-37"></a>
### OPEN-37. The names of the pointer difference and ordering functions

**OPEN-37@1.** The names and signatures of the named functions that replace pointer difference and pointer ordering, beside `ptr::offset`, are an open owner decision (OD37).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD37
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-6 and TRUST-1. OD26 removes the operators and asks for named functions without naming them; TRUST-1 already names `ptr::offset`, `ptr::expose` and `ptr::from_exposed`. The options and a recommendation are in ROADMAP's row OD37;
the owner's answer is recorded there.

<a id="OPEN-38"></a>
### OPEN-38. The root resource API

**OPEN-38@1.** The type of the root resource value `main` receives, how it is split into tokens, and the signature `main` declares to receive it are an open owner decision (OD38).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD38
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TRUST-9 and STMT-7. OD8 decides that `main` receives a root value and splits it, in the spirit of Austral's `RootCapability`, without naming its type or the splitting functions. The options and a recommendation are in ROADMAP's row OD38;
the owner's answer is recorded there.

<a id="OPEN-39"></a>
### OPEN-39. The interface artifact format

**OPEN-39@1.** The format and location of the interface artifact the compiler generates for each module are an open owner decision (OD39).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD39
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on MOD-7. OD9 decides that the compiler generates the artifact and a gate diffs it, without fixing its format. The options and a recommendation are in ROADMAP's row OD39;
the owner's answer is recorded there.

<a id="OPEN-40"></a>
### OPEN-40. PAT-9 and the fallback arm of a desugared form

**OPEN-40@1.** Whether PAT-9's refusal of an unreachable arm applies to the fallback arm the parser adds when it desugars `if let`, `let ... else` and `while let` is an open owner decision (OD40).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD40
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-9. OD18 is stated for a `match`; over a one-variant enum the fallback arm of `if let` can never run, and refusing it would refuse `if let` there. Bug 317's fix, unlanded, does not judge that arm. The options and a recommendation are in ROADMAP's row OD40;
the owner's answer is recorded there.

<a id="OPEN-41"></a>
### OPEN-41. PAT-9 and integer ranges that span their type

**OPEN-41@1.** Whether a catch-all after integer ranges that together span the scrutinee's whole type is an unreachable arm under PAT-9 is an open owner decision (OD41).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD41
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on PAT-9 and PAT-2. PAT-2 requires a catch-all over an integer scrutinee, so refusing it there would make such a match impossible to write. Bug 317's fix, unlanded, does not count the type's range as covered. The options and a recommendation are in ROADMAP's row OD41;
the owner's answer is recorded there.

<a id="OPEN-42"></a>
### OPEN-42. Comparisons on `bool` and `char`

**OPEN-42@1.** Whether OD26's "comparisons take numbers only" removes equality and ordering on `bool` and `char` is an open owner decision (OD42).

- Kind: open
- Uses: none
- Codes: none
- Status: open OD42
- Enforced by: none
- Semantics: none
- Obligation: none
- Evidence: open

**Rationale.** Bears on TYPE-6. Current main compares `bool` and `char`, and TYPE-6@2 keeps that
while this is open. The options and a recommendation are in ROADMAP's row OD42; the owner's answer is
recorded there.

## Decided

These entries were retired when the owner answered them; their IDs are never reused.

- **OD23 – OD32** (the strictness pass, decided 2026-10-29, never open entries): REF-11@3 (OD23),
  STMT-7@3 (OD24), PAT-3@3 and PAT-5@3 (OD25), TYPE-6@2 (OD26), STMT-6@2 (OD27), TRAIT-10@2 (OD28),
  OWN-7@3 (OD29), MEM-4@3 (OD30), ARITH-2@2 (OD31) and OWN-15@3 (OD32), each pending its bug (319 –
  328); OD28 and OD30 left OPEN-35 and OPEN-36.
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
  REF-10@2 (lending), REF-11@3 (exclusivity at a call, keyed by the root as OD15 (a) tightened it), REF-13 (a `&mut` refers to the
  place), OWN-16 (a deferred call's borrow lives until it runs) and STMT-11 (one exit sequence).
- **OPEN-9** (OD9, modules and interfaces): decided 2026-10-29. Now MOD-7 (pending, R-0556).
- **OPEN-3** (OD3, pattern binders through a borrowed scrutinee): decided 2026-10-29, as enforced.
  Now REF-9 (the binder is always a reference), PASS-2 (its mode stored in Core) and REF-8 (the root
  is not reassigned while the binder's loan lives).
- **OPEN-15** (OD15, freezing and `&mut` exclusivity): decided 2026-10-29. Part (a), whole-owner
  freezing with loans keyed by the root binding, is REF-11@3 (enforced; it tightens OD2's part 2).
  Part (b), the strict option, is REF-14 (pending, R-0496): no pass exploits `&mut` exclusivity until
  it is proved, and an alias derived from a loan is valid only while the loan lives.
- **OPEN-16** (OD16, a struct literal in a condition head): decided 2026-10-29, refused unless
  parenthesized, with no exceptions. Now SYN-4 (pending, bug 318); what counts as parenthesized inside
  a call's arguments is OPEN-34.
- **OPEN-17** (OD17, field writes and `mut`): decided 2026-10-29. Now OWN-10@2 (partial, bug 216).
- **OPEN-8** (OD8, global state): decided 2026-10-29, constants only and global resources as linear
  tokens. Now TRUST-8, TRUST-9 and TRUST-10 of [trusted.md](trusted.md#TRUST-8) (pending, R-0557).

- **OPEN-20** (OD20, the visibility of std's `std.libc` bindings): decided 2026-10-29, private to std.
  Now MOD-6 of [modules.md](modules.md#MOD-6) (pending, bug 226).
- **OPEN-21** (OD21, a parent module's rights over its child): decided 2026-10-29, only the declaring
  module. Now TYPE-11 of [types.md](types.md#TYPE-11), enforced as written (the parent case is pending,
  R-0011).
- **OPEN-22** (OD22, trust on an `impl` block): decided 2026-10-29, per method only. Now TRUST-5@2 of
  [trusted.md](trusted.md#TRUST-5).

