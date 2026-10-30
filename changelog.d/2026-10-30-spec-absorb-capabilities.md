### Language specification: handle and callable-value capabilities absorbed (2026-10-30)

`docs/language/CALLABLE_VALUES_AND_CAPABILITIES.md` and `docs/language/HANDLE_CAPABILITIES.md` are
absorbed into the specification and moved to `docs/archive/design-records/`; `trusted.md` no longer
defers to them. New entries: TRUST-D1 (what `Console`, `File`, `Network`, `Random` and `Unsafe`
mean), TYPE-D1 (capability sets and `<cap C>`), TYPE-17 (a function type's capability set is part of
the type, so no combinator erases it) and TYPE-18 (capabilities have no run-time representation),
with TYPE-4 examples for the refusal, and an "Absent by decision" section in `trusted.md` (no
capability bounds, subtraction, capability values, effect handlers or effect rows). Questions the
documents left open, or where main contradicts them, are open entries: the `with(f)` shortcut,
widening a function value's set, the meanings of `Time`, `Env`,
`Process` and `Alloc`, and signature agreement between two bindings of one C symbol. The
`for x in xs` loop the callable-values design described is not the language (STMT-6, OPEN-51), and
the handle design's capability inference is OPEN-45. An unknown name in `with(...)` is not an open
question: MOD-1 already refuses an unresolved name, and main's acceptance is bug 337.
