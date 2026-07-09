import Concrete.Frontend.AST
import Concrete.Resolve.Intrinsic

/-!
# Capability / effect semantics — the single source of truth

Phase 6.5 #5. Capabilities are the second identity-defining semantic axis of
Concrete (after integer arithmetic, Phase 6.5 #1). The base `CapSet` primitives
(`normalize`, `concreteCaps`, `isEmpty`, `expandAliases`, `stdCaps`,
`validCaps`) already live once in `Frontend/AST.lean`. What was scattered is the
*derived capability facts*:

- the superset ("does the caller's authority cover the callee's?") check lived
  in `Resolve/Shared`;
- the "does an Unsafe op have authority here?" test was open-coded four times in
  CoreCheck as `!inTrusted && !capsContain cs (.concrete ["Unsafe"])`;
- the "does this cap set literally list Unsafe?" classification lived in
  `Report/ReportInterface` (`hasUnsafeCap`);
- the fact "an untrusted `extern fn` requires `Unsafe`" was recomputed
  independently in CoreCheck's signature table AND twice in ReportBase's
  cap-lookup builders — a duplicated fact that could let a report disagree with
  the checker.

This module is the one place those facts are defined. Check, CoreCheck, Report,
packages, and audit read from here, so the capability answer a diagnostic gives
and the one a report renders are the same fact by construction — never two
implementations kept in sync by luck.

Leaf module: imports only the AST (for `CapSet`) and `Intrinsic` (for
`unsafeCapName`), so every stage can depend on it without a cycle.
-/

namespace Concrete
namespace Capabilities

/-- Does `caller`'s authority cover everything `callee` requires? A caller that
    is a capability *variable* is assumed to satisfy any requirement (it is
    resolved at instantiation), matching the polymorphic-callback contract.
    (Moved here from `Resolve/Shared`; it is the capability superset primitive.) -/
def capsContain (caller callee : CapSet) : Bool :=
  match callee with
  | .empty => true
  | .concrete calleeCaps =>
    match caller with
    | .empty => calleeCaps.isEmpty
    | .concrete callerCaps => calleeCaps.all fun c => callerCaps.contains c
    | .var _ => true  -- capability variable assumed to satisfy
    | .union a b => capsContain a callee || capsContain b callee
  | .var _ => true  -- capability variable, can't check statically here
  | .union a b => capsContain caller a && capsContain caller b

/-- Does this cap set LITERALLY list `Unsafe`? This is membership of the
    declared concrete caps (a capability *variable* is not literal Unsafe) — the
    right question for report counting ("how many functions declare Unsafe"),
    NOT for authority. For authority use `capsAllowUnsafeOp`. -/
def capSetHasUnsafe (cs : CapSet) : Bool :=
  cs.concreteCaps.contains unsafeCapName

/-- Does the current context have AUTHORITY to perform an `Unsafe` operation
    (raw-pointer deref/assign, pointer arithmetic, unsafe cast)? True inside a
    `trusted` function, or when the capability set covers `Unsafe` (including a
    capability variable, via `capsContain`). This is the one predicate behind
    CoreCheck's raw-pointer/unsafe-cast gates. -/
def capsAllowUnsafeOp (inTrusted : Bool) (cs : CapSet) : Bool :=
  inTrusted || capsContain cs (.concrete [unsafeCapName])

/-- The capability set an `extern fn` requires: none if it is `trusted`
    (the trust boundary is the author's responsibility), otherwise `Unsafe`.
    One definition of the fact that CoreCheck's signature table and every
    report/audit cap-lookup builder must agree on. -/
def externFnRequiredCaps (isTrusted : Bool) : CapSet :=
  if isTrusted then .empty else .concrete [unsafeCapName]

end Capabilities
end Concrete
