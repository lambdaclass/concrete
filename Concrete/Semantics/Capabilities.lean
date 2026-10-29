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

/-- CAPABILITY ENTAILMENT (bug 188) — the one rule for "does the caller's authority
    cover this requirement?". A required capability is entailed by:
      * a CONCRETE capability the caller declares (`with(Console)` entails `Console`), or
      * a capability VARIABLE the caller declares only when the variable's declared bound
        entails it. A capability parameter (`cap C`) declares no bound, so it is
        "some set chosen by the instantiator" and entails exactly ITSELF: `with(C)`
        entails a requirement `C`, and nothing else — not `Console`, not `Process`,
        and in particular not `Unsafe`.
    A required variable the caller does not itself declare fails closed. A callee's OWN
    capability parameters are not requirements of this kind: they are instantiated at the
    call site, where Check infers them (`resolveCaps`, fail-closed on an uninferable one)
    and decides the instantiated set with this same rule (see `instantiatedRequirement`).

    Until bug 188 a caller holding ANY capability variable was treated as covering every
    requirement ("resolved at instantiation"), so `fn f<cap C>(..) with(C)` could call a
    `with(Process)` function, an untrusted `extern`, or dereference a raw pointer, and a
    caller instantiating `C := {}` ran it with no authority at all. Nothing at the
    instantiation re-checked the body: a generic body is checked once, here.

    Direct calls, calls through a function pointer, method calls and the raw-operation
    gate all read this one rule (`missingCaps` is its per-capability view). A capability
    parameter can never be spelled like a real capability (Resolve, E0121), so matching a
    requirement against a caller's variable by name cannot launder `Unsafe`. -/
def missingCaps (caller callee : CapSet) : List String :=
  let (callerCaps, callerVars) := caller.normalize
  let (calleeCaps, calleeVars) := callee.normalize
  -- A required name is entailed by a concrete capability of the same name, or by a
  -- capability variable of the same name (a variable entails exactly itself). Inside a
  -- fn-pointer TYPE a variable is still spelled as a concrete name (`fn() with(C)`),
  -- so both sides are matched by name; E0121 keeps the two namespaces disjoint.
  (calleeCaps ++ calleeVars).eraseDups.filter fun c => !(callerCaps.contains c || callerVars.contains c)

/-- Does `caller`'s authority cover everything `callee` requires? The boolean view of
    the one entailment rule (`missingCaps`): satisfied iff nothing is missing. A union is
    flattened first (`normalize`), so `Alloc ∪ Unsafe` covers `Alloc, Unsafe`.
    (Moved here from `Resolve/Shared`; it is the capability superset primitive.) -/
def capsContain (caller callee : CapSet) : Bool :=
  (missingCaps caller callee).isEmpty

/-- What a DIRECT call to a callee declaring `declared` requires of its caller BEFORE the
    callee's own capability parameters are instantiated: the declared concrete
    capabilities. Every `.var` in a declared callee set is one of the callee's own
    capability parameters (the parser turns exactly the names in `capParams` into `.var`,
    and an impl's parameters into its methods' `.var`s), and those are instantiated per
    call site, where Check infers them and decides the instantiated set with
    `missingCaps` — failing closed when one cannot be inferred. CoreCheck, which sees
    the call without the inferred binding, checks this part; it never treats an
    uninstantiated variable as satisfied by the CALLER's variables (bug 188). -/
def instantiatedRequirement (declared : CapSet) : CapSet :=
  let (names, _ownParams) := declared.normalize
  if names.isEmpty then .empty else .concrete names

/-- A capability parameter may not be spelled like a capability: `fn f<cap Unsafe>(..)
    with(Unsafe)` would make a variable whose name-match entails the real `Unsafe`
    (bug 188). The reserved spellings are every built-in capability, the `Std` macro and
    the module's capability aliases. -/
def capParamNameReserved (aliases : List String) (name : String) : Bool :=
  validCaps.contains name || name == stdCapMacroName || aliases.contains name

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
    `trusted` function, or when the capability set covers `Unsafe` by the one entailment
    rule (`capsContain`) — a capability VARIABLE does not (bug 188). This is the one
    predicate behind CoreCheck's raw-pointer/unsafe-cast gates. -/
def capsAllowUnsafeOp (inTrusted : Bool) (cs : CapSet) : Bool :=
  inTrusted || capsContain cs (.concrete [unsafeCapName])

/-- Does a `trusted` body discharge the missing requirement `cap`? Only `Unsafe`, and
    only in a trusted body: vouching that it meets a callee's memory-safety precondition
    is what `trusted` means; every other capability still binds. The per-capability view
    of CoreCheck's call-site discharge, shared by Check's call paths (direct
    cap-polymorphic calls, method calls, calls through a trait bound) so the three
    decide `Unsafe` the same way CoreCheck does (bug 188: Check used to leave a
    cap-polymorphic call's instantiated `Unsafe` to CoreCheck, which cannot see the
    instantiation). -/
def trustedDischarges (inTrusted : Bool) (cap : String) : Bool :=
  inTrusted && cap == unsafeCapName

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
    From the construction/caller audit (`docs/stdlib/HANDLE_CAPABILITIES_AUDIT.md` §3).
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

/-- The one direct-call capability DECISION (Phase 6.5 CapabilityJudgment, slice
    1): whether `caller`'s authority covers a call requiring `callee`, and — when
    it does not — which caps are missing. `satisfied` and `missing` are two views of
    the one entailment rule (`missingCaps`), so they cannot disagree; `missing` is the
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
