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

/-- Does this cap set LITERALLY declare capability `cap`? Membership of the
    concrete caps (a capability *variable* is not a literal declaration). This is
    the fact behind report/audit provenance ("which callee contributes each
    declared cap"), single-sourced so a report cannot classify a cap set
    differently than the checker. For *authority* (coverage) use `capsContain`;
    this is declaration membership only. -/
def capSetHas (cs : CapSet) (cap : String) : Bool :=
  cs.concreteCaps.contains cap

/-- Does this cap set LITERALLY list `Unsafe`? The `Unsafe`-specialized
    `capSetHas` — the right question for report counting ("how many functions
    declare Unsafe"), NOT for authority. For authority use `capsAllowUnsafeOp`. -/
def capSetHasUnsafe (cs : CapSet) : Bool :=
  capSetHas cs unsafeCapName

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

/-- The concrete capabilities a `callee` requires that a `caller` does NOT hold
    (in its concrete caps or as a capability variable). This is the specific
    "what is missing" behind the per-capability E0240 diagnostic; it was
    open-coded identically at every direct-call site (Check + CheckHelpers). It
    considers only the required set's CONCRETE caps — a required capability
    variable is not a per-cap miss (its satisfaction is the polymorphic-callback
    contract, decided by `capsContain`). -/
def missingCaps (caller callee : CapSet) : List String :=
  let (callerCaps, callerVars) := caller.normalize
  let (calleeCaps, _calleeVars) := callee.normalize
  calleeCaps.filter fun c => !(callerCaps.contains c || callerVars.contains c)

/-- Like `missingCaps`, but a required capability VARIABLE also counts as a
    requirement the caller must hold. This is the anti-smuggling rule for calls
    THROUGH a function pointer (Phase 6.5 CapabilityJudgment, slice 3): a caller
    with no authority must not invoke `f : fn() with(C)` merely because `C` is a
    variable — that would smuggle authority past the caller's own header. A
    *direct* call treats a callee variable as satisfied (`missingCaps` / the
    polymorphic-callback contract); an *indirect* call through the pointer type
    does not. (ROADMAP Phase 5 #24a red-team.) -/
def missingCapsThroughPtr (caller callee : CapSet) : List String :=
  let (callerCaps, callerVars) := caller.normalize
  let (calleeCaps, calleeVars) := callee.normalize
  (calleeCaps ++ calleeVars).filter fun c => !(callerCaps.contains c || callerVars.contains c)

/-- The one direct-call capability DECISION (Phase 6.5 CapabilityJudgment, slice
    1): whether `caller`'s authority covers a call requiring `callee`, and — when
    it does not — which concrete caps are missing. `satisfied` is authoritative
    (`capsContain`, honoring unions and polymorphic variables); `missing` is the
    per-cap detail for diagnostics. Check (per-cap E0240) and CoreCheck (whole-set
    E0520) build their diagnostics from THIS record, and reports read it — so they
    cannot disagree on satisfaction or on which caps are missing. -/
structure CallCapDecision where
  required  : CapSet
  callerHas : CapSet
  satisfied : Bool
  missing   : List String
  deriving Repr

def decideCall (caller callee : CapSet) : CallCapDecision :=
  { required := callee, callerHas := caller
    satisfied := capsContain caller callee
    missing := missingCaps caller callee }

end Capabilities
end Concrete
