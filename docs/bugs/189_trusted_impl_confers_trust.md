# Bug 189: `trusted impl` conferred trust on every method of the block

<!-- bug-ledger
summary: `trusted impl` made every method of the block trusted whether or not it declared it (99 blocks, 326 methods; only 175 needed trust). A block never confers trust now: the parser refuses `trusted impl` (E0001) and each method declares `trusted fn` itself; std and the corpus were migrated by compiling and marking exactly the methods the compiler refused. Pinned by `error_189_trusted_impl_refused.con` and `check_trusted_method.sh` (both impl forms refused; an untrusted sibling method keeps its E0521) ((silent trust widening))
fixtures: error_189_trusted_impl_refused.con
class: 14 — `trusted impl` made every method trusted without declaring it
silent: yes
exit: 0b
ruling: Violates: unrecorded
-->

**Status:** FIXED
**Found:** 2026-10-23, in the same review as bug 188 (`Concrete/Frontend/AST.lean`: "trusted impl:
all methods inherit trusted boundary"), against the decided rule that a marker never confers trust.
**Severity:** silent trust widening. Every method of a `trusted impl` block was trusted whether or
not it needed or declared it: it could perform raw-pointer operations, discharge callees' `Unsafe`
obligations, destroy through a reference and compare raw pointers, and it was excluded from proof
eligibility and reported as a trusted boundary. A method added to such a block later was audited by
nobody. No program behaved wrongly at run time because of it; the defect is that the trusted base
was larger than anything declared.

Codex review 2026-10-25: NOT CONFIRMED — intended behaviour: docs/spec/trusted.md TRUST-5 explicitly authorizes trusted impl; block-wide trust was declared, and replacing it with per-method trust changes policy.

## Observed

```
struct Buf { p: *mut u8 }

trusted impl Buf {
    fn first(&self) -> u8 { return *self.p; }   // never declared trusted
}
```

compiled. At ee1fff8e, std and the corpus had 99 such blocks in 52 files, with 326 methods. Only 175 of them needed trust (measured below); the other 151 were trusted by the block
alone.

## Expected

`trusted` is a property a function declares. A block of methods confers nothing.

## Structural cause

`ImplBlock.isTrusted` and `ImplTraitBlock.isTrusted` were OR-ed into each method's `isTrusted`, in
two places (Elab and Check), so trust was a property of where a method was written rather than of
the method. Per-method `trusted fn` (2026-09-25) had already made the block marker unnecessary; it
stayed as a second, wider way to say the same thing.

## Structural owner

Class 14 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md): trusted `std` and FFI code with no
checked contract. Owner R-0509 (S16 trusted base). The rule this enforces: trust is declared per
function, so the trusted base is exactly the set of functions that say so.

## Fix

The block-level marker is gone. `ImplBlock.isTrusted` and `ImplTraitBlock.isTrusted` are removed,
the parser refuses `trusted impl` (E0001, "'trusted impl' is not allowed: trust is never conferred
by a block", with a hint to mark each method), and Elab and Check no longer OR a block flag into a
method. The formatter no longer prints the marker, and `stdlib_manifest.py` no longer classifies a
method as a trusted boundary from its enclosing block.

Migration, decided by the compiler rather than by reading. Every `trusted impl` became `impl`.
Then std and every unit that contained a block were compiled repeatedly, and a method got
`trusted fn` exactly when a diagnostic landed inside it (E0521, E0520, E0290, E0229, E0240): 175
methods. Thirteen fixtures whose methods do raw work but whose compile stops at an earlier,
intended Check error (the `error_091_*` destroy fixtures, two newtype-payload destroy fixtures
written on one line, `trusted_report_check.con`, `manifest_selftest.con`) were marked by hand so
each keeps its premise. The stdlib surface manifest was regenerated: 89 public rows lose a
`trusted-boundary` class they held only through their block.

Breakage, counted with `scripts/tools/rule_impact.sh` (the refusal compiler against bug 188's head
3e54c177, unmigrated corpus): 204 of 2167 units differ, 131 accepted to refused and 73 refused with
a changed code set, all E0001. Std is one unit, and every std importer follows it.

## Containment

- `tests/programs/error_189_trusted_impl_refused.con` (run_err on the refusal).
- `check_trusted_method.sh`, section "a block never confers trust": an inherent and a trait
  `trusted impl` are both refused, and the untrusted sibling of a `trusted fn` method in a plain
  impl has its raw deref refused (E0521) while the trusted method is accepted.

## Left over

- Proof eligibility and reports change for the 151 methods that lost inherited trust. A method that
  is pure and not generic can now be proof-eligible; that is the intended consequence.
- `Unsafe` stays a capability; separating effects from safety obligations is the later trust-split
  slice.
