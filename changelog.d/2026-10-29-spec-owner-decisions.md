### The owner's language decisions recorded in the specification (2026-10-29)

The project owner decided every open language question on 2026-10-29, and the specification records
each answer as rules, with each rule's status following main (`pending` with its bug or task where
main does not enforce it yet); the open entries were retired with pointers in `docs/spec/decisions.md`.

- OD1, references (the Austral model): REF-5, REF-1, REF-3, TRUST-3, REF-8.
- OD2, `&mut` arguments: lending (REF-10@2), a `&mut` refers to the place (REF-13), a deferred call's borrow lives until it runs (OWN-16), one exit sequence (STMT-11).
- OD3, binders through a borrowed scrutinee: REF-9, as enforced.
- OD4, foreign function values refused: FFI-1.
- OD5, generics and Copy: OWN-D1, TYPE-13, TYPE-14.
- OD6, trait coherence: TRAIT-1 to TRAIT-7.
- OD7, the `trusted mod` marker: TRUST-6@2.
- OD8, global state (constants only, global resources as linear tokens): TRUST-8 to TRUST-10, task R-0557.
- OD9, one file per module with generated interfaces: MOD-7, task R-0556.
- OD10, arithmetic and abort: ARITH-3, ARITH-4, STMT-2.
- OD11, casts: ARITH-5, TRUST-1.
- OD12, contracts: CON-1, CON-3.
- OD13, raw-pointer ownership: TRUST-7.
- OD14, type validity: REF-2, REF-4.
- OD15, whole-owner freezing (REF-11@3) and no pass exploiting `&mut` exclusivity until it is proved (REF-14).
- OD16, a struct literal in a condition head only in parentheses: SYN-4 (bug 318).
- OD17, a field write needs a `mut` binding: OWN-10@2 (bug 216).
- OD18, the first matching arm wins and an unreachable arm is refused: PAT-9.
- OD19, the provable subset: the scope of PROVABLE_V1.md.
- OD20, `std.libc` private to std: MOD-6 (bug 226).
- OD21, only the declaring module has representation rights: TYPE-11.
- OD22, trust per method only: TRUST-5@2.
- OD23, every call argument overlap-checked, re-lent `&mut` included: REF-11@3 (bug 319).
- OD24, `main` returns `u8` or `Unit`: STMT-7@3 (bug 320).
- OD25, ignoring a payload or field is written: PAT-3@3, PAT-5@3 (bug 321).
- OD26, operators take numbers only: TYPE-6@2 (bug 322).
- OD27, `break <expr>` refused at parse: STMT-6@2 (bug 323).
- OD28, `Destroy` only by an impl: TRAIT-10@2 (bug 324).
- OD29, a statement expression has type `Unit`: OWN-7@3 (bug 325).
- OD30, no implicit heap opening in `e.f`: MEM-4@3 (bug 326).
- OD31, no `Int` or `Float64` fallback for a literal: ARITH-2@2 (bug 327).
- OD32, no receiver auto-borrow: OWN-15@3 (bug 328).

Four questions raised while recording these are open for the owner: OD33 (inline submodules under
one file per module), OD34 (a struct literal inside a call's arguments in a condition head), OD35
(how an array satisfies `Destroy`) and OD36 (the spelling of the explicit heap place form).
