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
    -- Normalize the CALLER before testing membership. A union must be flattened
    -- first: `Alloc ∪ Unsafe` covers a callee requiring `Alloc, Unsafe`, but the
    -- earlier per-branch form asked whether EITHER side covered the whole
    -- requirement and answered no — an under-approximation of a set union.
    --
    -- That also made this disagree with `missingCaps`, which has always
    -- normalized, so `decideCall` could report `satisfied := false` alongside
    -- `missing := []` and render "requires Alloc, Unsafe but caller has
    -- Alloc + Unsafe". One cap set, two readings, which is precisely what this
    -- module exists to prevent.
    let (callerCaps, callerVars) := caller.normalize
    -- A capability VARIABLE in the caller is assumed to satisfy any requirement
    -- (it is resolved at instantiation), matching the polymorphic-callback
    -- contract — the same allowance the `.var` case made before.
    !callerVars.isEmpty || calleeCaps.all fun c => callerCaps.contains c
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

/-- WHAT `trusted` DOES WITH AN EXTERN CALL (R-0484, 2026-10-01 — reversed).

    Until 2026-10-01, `trusted` granted `Unsafe` for a raw OPERATION on memory the
    body already holds (`capsAllowUnsafeOp`) but NOT for a call to a plain `extern`,
    which still required `with(Unsafe)` on the header. That rule, pinned by
    `error_trusted_extern_needs_unsafe.con` and `error_trusted_no_extern.con`, pushed
    every effectful C function into `trusted extern` — callable with no capability at
    all — and that is how `console_write` performed I/O while declaring nothing.

    Now a trusted body absorbs an extern call's `Unsafe`, exactly as it absorbs any
    callee's `Unsafe` obligation (CoreCheck, `dropCrossPackageUnsafe`): trust vouches
    for memory safety. It absorbs NOTHING ELSE. The binding's declared effects
    (`extern fn write(..) with(Console)`) still bind the trusted caller, and every
    binding must declare them (Resolve, E0116), so trust can never absorb an
    unclassified effect. Fixtures: `trusted_absorbs_extern_unsafe.con` and
    `error_trusted_extern_effect_not_absorbed.con`. Design:
    `docs/language/HANDLE_CAPABILITIES.md` R2. -/
def trustedGrantsUnsafeOnCalls : Bool := true

/-- C symbols known to have external effects (R-0484 R3). Binding one of them with an
    EMPTY effect declaration is refused: the likeliest audit mistake, caught mechanically.
    It suppresses nothing — a binding that declares an effect is checked like any other.
    From the construction/caller audit (`docs/language/HANDLE_CAPABILITIES_AUDIT.md` §3).
    `abort` is deliberately absent: a language-defined failure carries no capability (D3).
    The list is incomplete by nature; its completeness is itself an audited assumption. -/
def knownEffectfulForeignSymbols : List String :=
  [ -- standard streams and files
    "write", "read", "open", "fopen", "fdopen", "fclose", "fflush", "ferror", "fread",
    "fwrite", "fseek", "ftell", "puts", "putchar", "printf", "fprintf", "dup", "dup2",
    "unlink", "mkdir", "rmdir", "rename",
    -- network
    "socket", "bind", "listen", "accept", "connect", "close", "send", "recv",
    "setsockopt", "getaddrinfo",
    -- environment and arguments
    "getenv", "setenv", "unsetenv", "uname", "__concrete_get_argc", "__concrete_get_argv",
    -- process
    "exit", "_exit", "fork", "execvp", "waitpid", "kill", "getpid", "raise", "signal",
    "sigaction",
    -- time and randomness
    "time", "clock_gettime", "nanosleep", "sleep", "rand", "srand", "getrandom" ]

/-- What calling a foreign binding requires (R-0484): the effects it DECLARES, plus
    `Unsafe` unless it is a `trusted extern` (safe for every argument its types
    permit). The declared effects are never dropped, by `trusted` or anything else.
    One definition of the fact that CoreCheck's signature table and every
    report/audit cap-lookup builder must agree on. -/
def externFnRequiredCaps (isTrusted : Bool) (declared : CapSet) : CapSet :=
  if isTrusted then declared
  else
    -- One normalized set, so it renders `Console, Unsafe`, never `(none) + Unsafe`.
    let (names, vars) := declared.normalize
    let base : CapSet := .concrete ((names ++ [unsafeCapName]).mergeSort (· < ·) |>.eraseDups)
    vars.foldl (fun acc v => .union acc (.var v)) base

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

/-- Resolve a cap-polymorphic signature's declared cap set against inferred
    capability-variable bindings (Phase 6.5 CapabilityJudgment, slice 3 — the
    "callback requires `C` ⇒ the caller requires `C`" propagation). A declared cap
    that is a cap PARAMETER is replaced by its bound caps; every other cap passes
    through. `Except.error` carries the capability variable that could NOT be
    inferred (the caller renders it as `cannotInferCapVariable`). This is the one
    resolution shared by the call path (Check) and the method-call path
    (CheckHelpers), which had verbatim copies. -/
def resolveCaps (capParams : List String) (capBindings : List (String × List String))
    (declared : CapSet) : Except String (List String) := do
  let (concreteCaps, capVars) := declared.normalize
  let mut resolved : List String := []
  for cap in concreteCaps do
    if capParams.contains cap then
      match capBindings.find? fun (n, _) => n == cap with
      | some (_, caps) => resolved := resolved ++ caps
      | none => throw cap
    else resolved := resolved ++ [cap]
  for cv in capVars do
    match capBindings.find? fun (n, _) => n == cv with
    | some (_, caps) => resolved := resolved ++ caps
    | none => throw cv
  return resolved

end Capabilities
end Concrete
