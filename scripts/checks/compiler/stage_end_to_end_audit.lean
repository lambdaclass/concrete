import Concrete.Interfaces.Pipeline
import Lean.Elab.Command
import Lean.Meta

namespace EndToEndAudit
open Concrete.Interfaces ConcreteSpec.StageInterface ConcreteSpec.StageGrammar
open Lean Meta Elab Command

/-- Built-in data-only primitive types permitted by the allow-list. -/
private def isAllowedPrimitive (name : Name) : Bool :=
  [``Nat, ``Int, ``Bool, ``UInt8, ``UInt16, ``UInt32, ``UInt64,
    ``Char, ``String, ``ByteArray, ``Fin, ``BitVec,
    ``Unit, ``PUnit, ``Empty, ``PEmpty, ``Lean.Name].contains name

/-- Built-in data-only container types and their required type-argument arities. -/
private def allowedContainerArity? (name : Name) : Option Nat :=
  if name == ``List || name == ``Array || name == ``Option then some 1
  else if name == ``Prod || name == ``Sum || name == ``Except then some 2
  else none

/-- Check whether `name` was actually compiled in a module whose root matches `pred`
(never trusting a locally opened namespace string prefix). -/
private def isDefinedInModuleRoot (env : Environment) (name : Name) (pred : Name → Bool) : Bool :=
  match env.getModuleIdxFor? name with
  | some idx =>
    match env.header.moduleNames[idx.toNat]? with
    | some modName => pred modName.getRoot
    | none => false
  | none => false

/-- True iff `name` comes from Lean's core/standard library modules (`Init`, `Std`, `Lean`). -/
private def isCoreLibraryDecl (env : Environment) (name : Name) : Bool :=
  isDefinedInModuleRoot env name fun r => r == `Init || r == `Std || r == `Lean

/-- True iff `name` was actually compiled in the pinned external `LeanCompilerBackend` package. -/
private def isPinnedLCBDecl (env : Environment) (name : Name) : Bool :=
  isDefinedInModuleRoot env name (· == `LeanCompilerBackend)

/-- Namespaces whose inductive/structure definitions may be inspected recursively as data. -/
private def isRegisteredDataNamespace (name : Name) : Bool :=
  let root := privateToUserName name
  [`Concrete, `ConcreteSpec, `LeanCompilerBackend, `EndToEndAudit].any (·.isPrefixOf root)

/-- Namespaces whose proposition definitions and inductive predicates are audited recursively. -/
private def isAuditPropNamespace (env : Environment) (name : Name) : Bool :=
  let root := privateToUserName name
  -- Locally declared definitions (module idx = `none`) are ALWAYS audited regardless of namespace.
  (env.getModuleIdxFor? name).isNone ||
  [`Concrete, `ConcreteSpec, `EndToEndAudit].any (·.isPrefixOf root)

/-- Pipeline/stage combinators preserve their caller's attribution origin when unfolded. -/
private def isCombinatorOrigin (name : Name) : Bool :=
  [``Pipeline.Trusted, ``Pipeline.Validated, ``Pipeline.Ready, ``Pipeline.Corresponds,
    ``Pipeline.Sound, ``RoutePipeline.Trusted, ``RoutePipeline.Validated, ``RoutePipeline.Ready,
    ``RoutePipeline.Corresponds, ``RoutePipeline.Sound, ``Stage.Trusted, ``Stage.Validated,
    ``Stage.Checks, ``Stage.Compatible, ``Stage.trust, ``Stage.success, ``Stage.refusal,
    ``Stage.inputInvariant, ``Stage.outputInvariant].contains name

/-- Concrete, non-typeclass atomic predicates whose inductive definitions need not be
unfolded further (their type arguments are still checked by the data allow-list, their
predicate arguments are recursively audited, and their value arguments are scanned via
`scanTerm`). Typeclass projections (`LT.lt`, `LE.le`, `Membership.mem`, `Dvd.dvd`, etc.)
are deliberately EXCLUDED so every supplied instance is unfolded to its concrete relation,
and external `LeanCompilerBackend` predicates are trusted only by actual source module,
never by namespace string prefix. -/
private def isAllowedAtomicProp (env : Environment) (name : Name) : Bool :=
  [``Nat.le, ``Int.NonNeg, ``List.Mem, ``List.Pairwise].contains name ||
  isPinnedLCBDecl env name

/-- Core wrappers/eliminators that must still be unfolded or scanned in `scanTerm`. -/
private def isCoreUnfoldableWrapper (name : Name) : Bool :=
  [``id, ``Function.const, ``Function.comp, ``Prod.fst, ``Prod.snd,
    ``Option.getD, ``List.headD, ``cond, ``Bool.and, ``Bool.or, ``Bool.not].contains name

/-- Normalize an inductive argument for index-aware memoization without unfolding huge dense
certificate arrays (`earlyProposal`, `sliceParser`, `sliceGrammar`, `sliceLanguage`).
Closed ground indices (`Bool.true`, `Bool.false`, `0`, `[]`, etc.) and structural
type/pipeline/language parameters are preserved verbatim, while open (fvar-containing)
value indices (such as growing `tail ++ rest` or `steps + 1` expressions in mutual
inductives like `Predictive`/`Derives`) are abstracted to their outer constructor head or
type-tagged wildcard so recursive predicates terminate in bounded visits without aliasing
distinct concrete indices. -/
private def normalizeMemoArg (a : Expr) : MetaM Expr := do
  let a ← instantiateMVars a
  let aTy ← whnf (← inferType a)
  if aTy.isSort then
    whnf a
  else
    let fn := aTy.getAppFn
    let norm ←
      if fn.isConstOf ``Bool || fn.isConstOf ``Nat || fn.isConstOf ``Route ||
         fn.isConstOf ``IRPoint || fn.isConstOf ``BackendKind || fn.isConstOf ``Edge ||
         fn.isConstOf ``Pipeline then
        whnf a
      else if a.isProj then
        pure a
      else
        whnfCore a
    let keepExact :=
      aTy.isForall ||
      fn.isConstOf ``Pipeline || fn.isConstOf ``Stage || fn.isConstOf ``RoutePipeline ||
      fn.isConstOf ``Profile || fn.isConstOf ``CoreLanguage || fn.isConstOf ``LoweringLanguage ||
      fn.isConstOf ``SharedGrammar || fn.isConstOf ``CheckedTable || fn.isConstOf ``Slice ||
      fn.isConstOf ``PassFunctions
    if keepExact || !norm.hasFVar then
      return norm
    let env ← getEnv
    match norm.getAppFn with
    | .const ctorName _ =>
      if let some (.ctorInfo _) := env.find? ctorName then
        return mkApp (mkConst ctorName) aTy
      return mkApp (mkConst `_openArg) aTy
    | _ =>
      return mkApp (mkConst `_openArg) aTy

/-- Alpha-canonicalize non-let free variables for memo keys while expanding let-bound
free variables so distinct local bindings never alias in the cache. Returns `none`
when depth is exhausted so the caller safely skips caching. -/
private partial def canonicalizeExpr? (e : Expr) :
    StateT (Array (FVarId × Nat)) MetaM (Option Expr) := do
  let e ← instantiateMVars e
  go e 64
where
  go (e : Expr) (fuel : Nat) : StateT (Array (FVarId × Nat)) MetaM (Option Expr) := do
    if fuel == 0 then return none
    match e with
    | .fvar fid =>
      let lctx ← getLCtx
      match lctx.find? fid with
      | some (.ldecl (value := val) ..) => go val (fuel - 1)
      | some (.cdecl (type := ty) ..) =>
        let seen ← get
        match seen.find? (·.1 == fid) with
        | some (_, idx) => return some (mkConst (Name.mkNum `_canon idx))
        | none =>
          let idx := seen.size
          set (seen.push (fid, idx))
          let some tyC ← go ty (fuel - 1) | return none
          return some (mkApp (mkConst (Name.mkNum `_canon idx)) tyC)
      | none => return some e
    | .app f a =>
      let some f' ← go f (fuel - 1) | return none
      let some a' ← go a (fuel - 1) | return none
      return some (mkApp f' a')
    | .lam n d b bi =>
      let some d' ← go d (fuel - 1) | return none
      let some b' ← go b (fuel - 1) | return none
      return some (.lam n d' b' bi)
    | .forallE n d b bi =>
      let some d' ← go d (fuel - 1) | return none
      let some b' ← go b (fuel - 1) | return none
      return some (.forallE n d' b' bi)
    | .letE n t v b nd =>
      let some t' ← go t (fuel - 1) | return none
      let some v' ← go v (fuel - 1) | return none
      let some b' ← go b (fuel - 1) | return none
      return some (.letE n t' v' b' nd)
    | .proj s i b =>
      let some b' ← go b (fuel - 1) | return none
      return some (.proj s i b')
    | .mdata _ b => go b (fuel - 1)
    | _ => return some e

private def canonicalizeKey? (args : Array Expr) : MetaM (Option (Array Expr)) := do
  let (res, _) ← (args.mapM canonicalizeExpr?).run #[]
  return res.mapM id

private structure AuditState where
  /-- Verified data inductives keyed by `(declName, canonicalizedAllArgs)` (BOTH params and
  indices normalized in kernel normal form so dependent indices can never alias). -/
  verifiedData : Std.HashSet (Name × Array Expr) := {}
  /-- Verified `Prop`-valued inductives keyed by `(declName, pos, canonicalizedAllArgs)`. -/
  verifiedPropInducts : Std.HashSet (Name × Bool × Array Expr) := {}
  /-- Verified non-combinator `Prop` definitions keyed by `(declName, pos, canonicalizedArgs)`. -/
  verifiedPropDefs : Std.HashSet (Name × Bool × Array Expr) := {}
  /-- Verified non-core value definitions keyed by `(declName, canonicalizedTypeAndPropArgs)`
  whose instantiated bodies carry no hidden non-data propositions. -/
  verifiedTermDefs : Std.HashSet (Name × Array Expr) := {}
  /-- Iterative worklist of closed non-core value definitions queued by `scanTermStep`,
  drained at constant MetaM recursion depth by the outermost `scanTerm`. -/
  pendingTermDefs : Array (Name × (Name × Array Expr) × Expr) := #[]
  /-- Keys already queued during the current root `scanTerm` drain (prevents re-queuing
  recursive or diamond-shared helper definitions like `TableVerifier.Layout.*`). -/
  seenPendingTermDefs : Std.HashSet (Name × Array Expr) := {}
  /-- True while an enclosing `scanTerm` call is draining `pendingTermDefs`. While true,
  caches are recorded in `tempVerified*` and only committed to `verified*` once the full
  drain succeeds (discarded on any hit so failed drains never pollute the cache). -/
  drainingTermDefs : Bool := false
  tempVerifiedData : Std.HashSet (Name × Array Expr) := {}
  tempVerifiedPropInducts : Std.HashSet (Name × Bool × Array Expr) := {}
  tempVerifiedPropDefs : Std.HashSet (Name × Bool × Array Expr) := {}
  tempVerifiedTermDefs : Std.HashSet (Name × Array Expr) := {}

private abbrev AuditM := StateT AuditState MetaM

private def hasVerifiedTermDef (k : Name × Array Expr) : AuditM Bool := do
  let st ← get
  return st.verifiedTermDefs.contains k || st.tempVerifiedTermDefs.contains k

private def addVerifiedTermDef (k : Name × Array Expr) : AuditM Unit :=
  modify fun st =>
    if st.drainingTermDefs then { st with tempVerifiedTermDefs := st.tempVerifiedTermDefs.insert k }
    else { st with verifiedTermDefs := st.verifiedTermDefs.insert k }

private def hasVerifiedData (k : Name × Array Expr) : AuditM Bool := do
  let st ← get
  return st.verifiedData.contains k || st.tempVerifiedData.contains k

private def addVerifiedData (k : Name × Array Expr) : AuditM Unit :=
  modify fun st =>
    if st.drainingTermDefs then { st with tempVerifiedData := st.tempVerifiedData.insert k }
    else { st with verifiedData := st.verifiedData.insert k }

private def hasVerifiedPropDef (k : Name × Bool × Array Expr) : AuditM Bool := do
  let st ← get
  return st.verifiedPropDefs.contains k || st.tempVerifiedPropDefs.contains k

private def addVerifiedPropDef (k : Name × Bool × Array Expr) : AuditM Unit :=
  modify fun st =>
    if st.drainingTermDefs then { st with tempVerifiedPropDefs := st.tempVerifiedPropDefs.insert k }
    else { st with verifiedPropDefs := st.verifiedPropDefs.insert k }

private def hasVerifiedPropInduct (k : Name × Bool × Array Expr) : AuditM Bool := do
  let st ← get
  return st.verifiedPropInducts.contains k || st.tempVerifiedPropInducts.contains k

private def addVerifiedPropInduct (k : Name × Bool × Array Expr) : AuditM Unit :=
  modify fun st =>
    if st.drainingTermDefs then { st with tempVerifiedPropInducts := st.tempVerifiedPropInducts.insert k }
    else { st with verifiedPropInducts := st.verifiedPropInducts.insert k }

private def resultIsPropExpr (e : Expr) : MetaM Bool :=
  forallTelescope e fun _ res => do
    let resW ← whnf res
    return resW == .sort .zero

/-- Construct the constant `True` predicate of a `Prop`-returning telescope type
`d₁ → ... → dₖ → Prop`, used as the base inductive-hypothesis placeholder when
auditing the step function of a `Prop`-valued `List.foldr`. -/
private def mkTruePred (ty : Expr) : MetaM Expr :=
  forallTelescope ty fun xs _ => mkLambdaFVars xs (mkConst ``True)

/-- Count the exact number of leading `forallE` binders on the `minorIdx`-th binder of a
recursor's type (without unfolding any user motive, so higher-order motives preserve the
distinction between constructor-field/IH binders and trailing arguments). -/
private def countRecMinorBinders (recType : Expr) (minorIdx : Nat) : Nat :=
  go recType minorIdx
where
  countForalls : Expr → Nat
    | .forallE _ _ b _ => 1 + countForalls b
    | .mdata _ b => countForalls b
    | _ => 0
  go : Expr → Nat → Nat
    | .forallE _ dom _ _, 0 => countForalls dom
    | .forallE _ _ body _, k + 1 => go body k
    | .mdata _ body, k => go body k
    | _, _ => 0

/-- Constructor-head pattern matching between a constructor's result index `pat` (containing
fresh metavariables for the constructor's fields) and a concrete applied index `target`.
Only returns `.clash` when both sides reduce to distinct constructors of the same inductive,
guaranteeing we never skip a reachable constructor on an abstract/free-variable index. -/
private inductive IndexMatch where
  | matched
  | clash
  | unknown
  deriving BEq

private partial def matchIndexTerm (pat target : Expr) (fuel : Nat := 32) : MetaM IndexMatch := do
  if fuel == 0 then return .unknown
  let pat ← whnf (← instantiateMVars pat)
  let target ← whnf (← instantiateMVars target)
  if pat.isMVar then
    if ← isDefEq pat target then
      return .matched
    else
      return .unknown
  let env ← getEnv
  let pFn := pat.getAppFn
  let tFn := target.getAppFn
  match pFn, tFn with
  | .const pn _, .const tn _ =>
    match env.find? pn, env.find? tn with
    | some (.ctorInfo pinfo), some (.ctorInfo tinfo) =>
      if pinfo.induct == tinfo.induct then
        if pn != tn then
          return .clash
        let pArgs := pat.getAppArgs
        let tArgs := target.getAppArgs
        if pArgs.size != tArgs.size then
          return .unknown
        let mut res := IndexMatch.matched
        for i in [:pArgs.size] do
          match ← matchIndexTerm pArgs[i]! tArgs[i]! (fuel - 1) with
          | .clash => return .clash
          | .unknown => res := .unknown
          | .matched => pure ()
        return res
      else
        return .unknown
    | _, _ =>
      if ← isDefEq pat target then return .matched else return .unknown
  | _, _ =>
    if ← isDefEq pat target then return .matched else return .unknown

/-- Recognize `Option (Exec.Out O)` in `ConcreteSpec.Core.Exec.Step` (the CPS request
continuation `k` of `Exec.Out.request` is the fixed operational dispatcher boundary). -/
private def isFixedExecStepOutEq (origin : Name) (eqTy : Expr) : MetaM Bool := do
  unless origin == ``ConcreteSpec.Core.Exec.Step do return false
  let eqTyW ← whnf eqTy
  unless eqTyW.getAppFn.isConstOf ``Option && eqTyW.getAppArgs.size == 1 do return false
  let innerW ← whnf eqTyW.getAppArgs[0]!
  return innerW.getAppFn.isConstOf ``ConcreteSpec.Core.Exec.Out && innerW.getAppArgs.size == 1

/-- Recognize `sliceLanguage s₁ = sliceLanguage s₂`, the fixed repository slice-selection
witness carried by `AdmissionInput.pending` and `Payload.straightLine`. -/
private def isFixedSliceLanguageEq (eqTy lhs rhs : Expr) : MetaM Bool := do
  unless (← whnf eqTy).isConstOf ``CoreLanguage do return false
  let lhsW ← whnfCore lhs
  let rhsW ← whnfCore rhs
  return lhsW.getAppFn.isConstOf ``sliceLanguage && lhsW.getAppArgs.size == 1 &&
         rhsW.getAppFn.isConstOf ``sliceLanguage && rhsW.getAppArgs.size == 1

/-- Recognize `Except (Σ R : Type, R) β`, the exact heterogeneous refusal result type of
`Pipeline.run` / `RoutePipeline.run`, when one side of an equality is `Except.ok _`. -/
private def isPipelineRunOkEq (origin : Name) (eqTy lhs rhs : Expr) : MetaM (Option Expr) := do
  unless origin == ``Pipeline.endToEnd || origin == ``registeredLCBEndToEnd do
    return none
  let lhsW ← whnfCore lhs
  let rhsW ← whnfCore rhs
  unless lhsW.getAppFn.isConstOf ``Except.ok || rhsW.getAppFn.isConstOf ``Except.ok do
    return none
  let eqTyW ← whnf eqTy
  unless eqTyW.getAppFn.isConstOf ``Except && eqTyW.getAppArgs.size == 2 do
    return none
  let errTy ← whnf eqTyW.getAppArgs[0]!
  unless errTy.getAppFn.isConstOf ``Sigma && errTy.getAppArgs.size == 2 do
    return none
  let rDom ← whnf errTy.getAppArgs[0]!
  unless rDom == .sort (.succ .zero) do
    return none
  let rFam ← whnf errTy.getAppArgs[1]!
  match rFam with
  | .lam _ _ (.bvar 0) _ => return some eqTyW.getAppArgs[1]!
  | _ => return none

mutual
/-- Allow-list classifier for data-only types.
Returns `none` iff `type` (after `instantiateMVars` + `whnf` reducing abbreviations,
definitions, `let`s, beta-redexes, closed projections, and known matches) is built
solely from:
- explicit data primitives (`Nat`, `Int`, `Bool`, `UInt8`-`UInt64`, `Char`, `String`,
  `ByteArray`, `Fin n`, `BitVec n`, `Unit`/`PUnit`, `Empty`/`PEmpty`, `Lean.Name`)
  with every index argument audited via `scanTerm`,
- explicit data containers (`List`, `Array`, `Option`, `Prod`, `Sum`, `Except`, `Sigma`),
- `Subtype` over an allowed data carrier with an audited predicate,
- registered repository inductives/structures whose full application (`params` + `indices`),
  type parameters, type-family parameters, instantiated index domains, index values, and
  instantiated constructor fields are all recursively allowed (with index-aware memoization
  keyed on ALL normalized params and indices).

Anything else (arrows/pis, `Sort`-valued or `Prop`-carrying fields, under-applied recursors,
recursors/matchers with non-data trailing arguments, subtypes over non-data, opaque constants,
unregistered inductives, stuck non-data expressions) is rejected with a finding. -/
private partial def rejectNonDataType (type : Expr)
    (inProgress : List (Name × Array Expr) := [])
    (fuel : Nat := 256) (allowBehavior := false) : AuditM (Option String) := do
  let savedState ← get
  let res ← rejectNonDataTypeCore type inProgress fuel allowBehavior
  if res.isSome then
    set savedState
  return res

private partial def rejectNonDataTypeCore (type : Expr)
    (inProgress : List (Name × Array Expr)) (fuel : Nat) (allowBehavior : Bool) :
    AuditM (Option String) := do
  if fuel == 0 then
    return some s!"type classification depth exceeded on {← ppExpr type}"
  let type ← instantiateMVars type
  let type ← whnf type
  match type with
  | .mdata _ body => rejectNonDataTypeCore body inProgress (fuel - 1) allowBehavior
  | .letE name t v b _ =>
    if (← whnf t) == .sort .zero then
      if let some (_, bname) ← auditPropExprCore `let v false inProgress [] [] (fuel - 1) then
        return some s!"let-bound prop binder {bname}"
    else if let some (_, bname) ← scanTerm `let v inProgress [] [] (fuel - 1) then
      return some s!"let-bound term binder {bname}"
    withLetDecl name t v fun fv =>
      rejectNonDataTypeCore (b.instantiate1 fv) inProgress (fuel - 1) allowBehavior
  | .forallE .. =>
    return some s!"disallowed arrow/pi type {← ppExpr type}"
  | .sort .. =>
    return some s!"disallowed Sort-valued type {← ppExpr type}"
  | _ =>
    if let some m ← matchMatcherApp? type then
      for discr in m.discrs do
        let discrTy ← inferType discr
        let discrTyW ← whnf discrTy
        if discrTyW == .sort .zero then
          if let some (_, bname) ← auditPropExprCore `match discr false inProgress [] [] (fuel - 1) then
            return some s!"matcher prop discriminant binder {bname}"
        else if ← isProp discrTy then
          if let some (_, bname) ← auditPropExprCore `match discrTy false inProgress [] [] (fuel - 1) then
            return some s!"matcher proof discriminant binder {bname}"
        else
          if let some err ← rejectNonDataTypeCore discrTy inProgress (fuel - 1) allowBehavior then
            return some s!"matcher discriminant type: {err}"
          if let some (_, bname) ← scanTerm `match discr inProgress [] [] (fuel - 1) then
            return some s!"matcher discriminant term binder {bname}"
      for rem in m.remaining do
        if let some (_, bname) ← scanTerm `match rem inProgress [] [] (fuel - 1) then
          return some s!"matcher remaining arg binder {bname}"
      for i in [:m.alts.size] do
        let alt := m.alts[i]!
        let nParams := m.altNumParams[i]!
        let err ← lambdaBoundedTelescope alt nParams fun _ body =>
          rejectNonDataTypeCore (mkAppN body m.remaining) inProgress (fuel - 1) allowBehavior
        if let some err := err then
          return some s!"matcher branch {i}: {err}"
      return none
    let fn := type.getAppFn
    let args := type.getAppArgs
    if fn.isConstOf ``ite || fn.isConstOf ``dite then
      if args.size ≥ 5 then
        let c := args[1]!
        if let some (_, bname) ← auditPropExprCore `ite c false inProgress [] [] (fuel - 1) then
          return some s!"if condition binder {bname}"
        let extra := args[5:]
        if fn.isConstOf ``dite then
          let err1 ← withLocalDeclD `_h c fun h =>
            rejectNonDataTypeCore (mkAppN (mkApp args[3]! h) extra) inProgress (fuel - 1) allowBehavior
          if let some err := err1 then return some s!"dite then-branch: {err}"
          return ← withLocalDeclD `_h (mkApp (mkConst ``Not) c) fun h =>
            rejectNonDataTypeCore (mkAppN (mkApp args[4]! h) extra) inProgress (fuel - 1) allowBehavior
        else
          if let some err ← rejectNonDataTypeCore (mkAppN args[3]! extra) inProgress (fuel - 1) allowBehavior then
            return some s!"ite then-branch: {err}"
          return ← rejectNonDataTypeCore (mkAppN args[4]! extra) inProgress (fuel - 1) allowBehavior
    match fn with
    | .const name lvls =>
      if isAllowedPrimitive name then
        for arg in args do
          if let some (_, bname) ← scanTerm name arg inProgress [] [] (fuel - 1) then
            return some s!"embedded non-data prop binder {bname} in index of {name}"
        return none
      if let some expectedArity := allowedContainerArity? name then
        if args.size != expectedArity then
          return some s!"container {name} has unexpected arity {args.size}"
        for arg in args do
          if let some err ← rejectNonDataTypeCore arg inProgress (fuel - 1) allowBehavior then
            return some err
        return none
      if name == ``Sigma then
        if args.size != 2 then return some s!"unexpected Sigma arity {args.size}"
        let α := args[0]!
        let β := args[1]!
        if let some err ← rejectNonDataTypeCore α inProgress (fuel - 1) allowBehavior then
          return some err
        return ← withLocalDeclD `_x α fun x =>
          rejectNonDataTypeCore (mkApp β x) inProgress (fuel - 1) allowBehavior
      if name == ``Subtype then
        if args.size != 2 then return some s!"unexpected Subtype arity {args.size}"
        let α := args[0]!
        let p := args[1]!
        if let some err ← rejectNonDataTypeCore α inProgress (fuel - 1) allowBehavior then
          return some s!"subtype carrier: {err}"
        let propErr ← withLocalDeclD `_x α fun x =>
          auditPropExprCore `Subtype (mkApp p x) false inProgress [] [] (fuel - 1)
        return propErr.map fun (_, bname) => s!"subtype predicate binder {bname}"
      if name == ``Decidable then
        if args.size != 1 then return some s!"unexpected Decidable arity {args.size}"
        let propErr ← auditPropExprCore `Decidable args[0]! false inProgress [] [] (fuel - 1)
        return propErr.map fun (_, bname) => s!"Decidable prop binder {bname}"
      if allowBehavior && name == ``Behavior then
        if args.size != 1 then return some s!"unexpected Behavior arity {args.size}"
        return ← rejectNonDataTypeCore args[0]! inProgress (fuel - 1) false
      -- `ActionArgs N value rhs` computes a right-nested tuple of `Unit`,
      -- `SurfaceToken`, and `value n`, so it is data-only iff `N` and `value n` are.
      if name == ``ConcreteSpec.StageGrammar.ActionArgs then
        if args.size != 3 then return some s!"unexpected ActionArgs arity {args.size}"
        let nTy := args[0]!
        let valFam := args[1]!
        if let some err ← rejectNonDataTypeCore nTy inProgress (fuel - 1) false then
          return some s!"ActionArgs index type: {err}"
        if let some (_, bname) ← scanTerm name args[2]! inProgress [] [] (fuel - 1) then
          return some s!"ActionArgs rhs binder {bname}"
        return ← withLocalDeclD `_n nTy fun n =>
          rejectNonDataTypeCore (mkApp valFam n) inProgress (fuel - 1) false
      let env ← getEnv
      match env.find? name with
      | some (.opaqueInfo _) =>
        return some s!"disallowed opaque constant {name}"
      | some (.inductInfo info) =>
        unless isRegisteredDataNamespace name do
          return some s!"unregistered inductive {name}"
        if name == ``PassFunctions then
          return some "PassFunctions is allowed only as the top-level audited pass parameter"
        if ← resultIsPropExpr (info.type.instantiateLevelParams info.levelParams lvls) then
          return some s!"Prop-valued inductive {name} in data position"
        if args.size != info.numParams + info.numIndices then
          return some s!"inductive {name} has arity {args.size}, expected {info.numParams + info.numIndices}"
        -- Normalize ALL arguments (both parameters and indices) so the memo key is
        -- strictly index-aware and dependent indices can never alias across visit orders.
        let mut normArgs : Array Expr := #[]
        for i in [:args.size] do
          normArgs := normArgs.push (← normalizeMemoArg args[i]!)
        let key? ← match ← canonicalizeKey? normArgs with
          | some ca => pure (some (name, ca))
          | none => pure none
        if let some key := key? then
          if ← hasVerifiedData key then return none
        let indType := info.type.instantiateLevelParams info.levelParams lvls
        -- ALWAYS check all type-valued parameters, type-family parameters, instantiated
        -- index domains, and embedded props in index terms before short-circuiting on `inProgress`.
        let paramIdxErr ← forallTelescope indType fun xs _ => do
          let mut substMap : Array (Expr × Expr) := #[]
          for i in [:Nat.min xs.size args.size] do
            substMap := substMap.push (xs[i]!, args[i]!)
          let applySubst (e : Expr) : Expr :=
            substMap.foldl (fun acc (fv, val) => acc.replaceFVar fv val) e
          for i in [:Nat.min info.numParams (Nat.min xs.size args.size)] do
            let pTy ← whnf (applySubst (← inferType xs[i]!))
            if pTy.isSort then
              if let some err ← rejectNonDataTypeCore args[i]! inProgress (fuel - 1) false then
                return some s!"type param {i} of {name}: {err}"
            else
              let isTypeFamily ← forallTelescope pTy fun _ res => do
                pure (← whnf res).isSort
              if isTypeFamily then
                let famErr ← forallTelescope pTy fun fxs _ => do
                  for fx in fxs do
                    let fxTy ← inferType fx
                    if !(← isProp fxTy) then
                      if let some err ← rejectNonDataTypeCore fxTy inProgress (fuel - 1) false then
                        return some s!"type-family param {i} domain of {name}: {err}"
                  rejectNonDataTypeCore (mkAppN args[i]! fxs) inProgress (fuel - 1) false
                if let some err := famErr then return some err
              else if ← isProp pTy then
                if let some (_, bname) ← auditPropExprCore name pTy false inProgress [] [] (fuel - 1) then
                  return some s!"prop param {i} of {name} binder {bname}"
              else
                if let some (_, bname) ← scanTerm name args[i]! inProgress [] [] (fuel - 1) then
                  return some s!"value param {i} of {name} binder {bname}"
          for i in [info.numParams:xs.size] do
            let idxTy := applySubst (← inferType xs[i]!)
            if ← isProp idxTy then
              if let some (_, bname) ← auditPropExprCore name idxTy false inProgress [] [] (fuel - 1) then
                return some s!"prop index {i} of {name} binder {bname}"
            else
              if let some err ← rejectNonDataTypeCore idxTy inProgress (fuel - 1) false then
                return some s!"index domain {i} of {name}: {err}"
              if i < args.size then
                if let some (_, bname) ← scanTerm name args[i]! inProgress [] [] (fuel - 1) then
                  return some s!"index term {i} of {name} binder {bname}"
          return none
        if let some err := paramIdxErr then return some err
        if let some key := key? then
          if inProgress.contains key then return none
        let inProgress' := match key? with | some k => k :: inProgress | none => inProgress
        -- Check every constructor field with applied parameters instantiated.
        for ctor in info.ctors do
          let some (.ctorInfo cinfo) := env.find? ctor
            | return some s!"missing constructor {ctor}"
          let ctype := cinfo.type.instantiateLevelParams cinfo.levelParams lvls
          let ctorErr ← forallBoundedTelescope ctype cinfo.numParams fun pxs body => do
            let mut b := body
            for i in [:Nat.min pxs.size args.size] do
              b := b.replaceFVar pxs[i]! args[i]!
            forallTelescope b fun fields _ => do
              for f in fields do
                let fTy ← inferType f
                if ← isProp fTy then
                  if let some (_, bname) ← auditPropExprCore ctor fTy false inProgress' [] [] (fuel - 1) then
                    return some s!"proof field of {ctor} binder {bname}"
                else
                  let isFiniteRank ← do
                    if ctor == ``ConcreteSpec.StageGrammar.CheckedTable.mk &&
                        (← f.fvarId!.getUserName) == `rank then
                      match ← whnf fTy with
                      | .forallE _ d b _ =>
                        pure ((← whnf d).getAppFn.isConstOf ``Fin && !b.hasLooseBVars &&
                          (← whnf b).isConstOf ``Nat)
                      | _ => pure false
                    else pure false
                  if !isFiniteRank then
                    if let some err ← rejectNonDataTypeCore fTy inProgress' (fuel - 1) false then
                      return some s!"field of {ctor}: {err}"
              return none
          if let some err := ctorErr then return some err
        if let some key := key? then
          -- Safe because `rejectNonDataType` rolls back `AuditState` on any failure, and
          -- during a `scanTerm` drain entries go to `tempVerifiedData` until the drain succeeds.
          addVerifiedData key
        return none
      | some (.recInfo info) =>
        let minorStart := info.numParams + info.numMotives
        let minorEnd := minorStart + info.numMinors
        let majorIdx := minorEnd + info.numIndices
        -- Fail closed on any under-applied recursor (Counterexample 1).
        if args.size < majorIdx + 1 then
          return some s!"under-applied recursor {name} (got {args.size} args, need at least {majorIdx + 1})"
        let major := args[majorIdx]!
        let majorTy ← inferType major
        if ← isProp majorTy then
          if let some (_, bname) ← auditPropExprCore name majorTy false inProgress [] [] (fuel - 1) then
            return some s!"recursor {name} prop major binder {bname}"
        else
          if let some err ← rejectNonDataTypeCore majorTy inProgress (fuel - 1) false then
            return some s!"recursor {name} major type: {err}"
          if let some (_, bname) ← scanTerm name major inProgress [] [] (fuel - 1) then
            return some s!"recursor {name} major term binder {bname}"
        for idxPos in [minorEnd:majorIdx] do
          let idxArg := args[idxPos]!
          if let some (_, bname) ← scanTerm name idxArg inProgress [] [] (fuel - 1) then
            return some s!"recursor {name} index arg binder {bname}"
        let extra := args[majorIdx + 1:]
        for offset in [:info.numMinors] do
          let minor := args[minorStart + offset]!
          let nBinders := countRecMinorBinders info.type (minorStart + offset)
          let minorTy ← inferType minor
          let err ← forallBoundedTelescope minorTy (some nBinders) fun mxs _ =>
            rejectNonDataTypeCore (mkAppN (mkAppN minor mxs) extra) inProgress (fuel - 1) allowBehavior
          if let some err := err then
            return some s!"recursor {name} minor {offset}: {err}"
        return none
      | _ =>
        return some s!"disallowed constant {name}"
    | _ =>
      return some s!"disallowed non-constant type {← ppExpr type}"

/-- Kernel-normal-form term auditor for value-level expressions appearing in type indices
(e.g. `Fin (badWidth bs)`), equalities, matcher discriminants, and atomic predicate
arguments. Audits every embedded `Prop`, proof term type, decision/choice expression,
and unfolds/inspects every non-core `defnInfo` body (memoized in `verifiedTermDefs`) while
failing closed on non-core opaque constants and axioms (Counterexample 3).

Closed non-core `defnInfo` bodies are queued onto `AuditState.pendingTermDefs` and drained
iteratively by the outermost `scanTerm` call so deep helper call chains (such as
`TableVerifier.Layout.*`) audit at constant MetaM recursion depth. -/
private partial def scanTerm (origin : Name) (e : Expr)
    (inProgressData : List (Name × Array Expr))
    (inProgressProps : List (Name × Bool × Array Expr))
    (inProgressTerms : List Name) (fuel : Nat) :
    AuditM (Option (Name × Name)) := do
  let isRoot := !(← get).drainingTermDefs
  if isRoot then
    modify fun st => { st with
      drainingTermDefs := true
      pendingTermDefs := #[]
      seenPendingTermDefs := {}
      tempVerifiedData := {}
      tempVerifiedPropInducts := {}
      tempVerifiedPropDefs := {}
      tempVerifiedTermDefs := {} }
  let res ← scanTermStep origin e inProgressData inProgressProps inProgressTerms fuel
  if !isRoot then
    return res
  if res.isSome then
    modify fun st => { st with
      drainingTermDefs := false
      pendingTermDefs := #[]
      seenPendingTermDefs := {}
      tempVerifiedData := {}
      tempVerifiedPropInducts := {}
      tempVerifiedPropDefs := {}
      tempVerifiedTermDefs := {} }
    return res
  let mut verifiedInDrain : Array (Name × Array Expr) := #[]
  let mut hit? : Option (Name × Name) := none
  while hit?.isNone do
    let st ← get
    if h : st.pendingTermDefs.size > 0 then
      let (qOrigin, qKey, qExpr) := st.pendingTermDefs[st.pendingTermDefs.size - 1]
      modify fun s => { s with pendingTermDefs := s.pendingTermDefs.pop }
      if !(← hasVerifiedTermDef qKey) then
        match ← scanTermStep qOrigin qExpr [] [] [qKey.1] 512 with
        | some hit => hit? := some hit
        | none => verifiedInDrain := verifiedInDrain.push qKey
    else
      break
  match hit? with
  | some hit =>
    modify fun st => { st with
      drainingTermDefs := false
      pendingTermDefs := #[]
      seenPendingTermDefs := {}
      tempVerifiedData := {}
      tempVerifiedPropInducts := {}
      tempVerifiedPropDefs := {}
      tempVerifiedTermDefs := {} }
    return some hit
  | none =>
    modify fun st =>
      let vt0 := verifiedInDrain.foldl (fun s k => s.insert k) st.verifiedTermDefs
      let vt := st.tempVerifiedTermDefs.fold (fun s k => s.insert k) vt0
      let vd := st.tempVerifiedData.fold (fun s k => s.insert k) st.verifiedData
      let vpi := st.tempVerifiedPropInducts.fold (fun s k => s.insert k) st.verifiedPropInducts
      let vpd := st.tempVerifiedPropDefs.fold (fun s k => s.insert k) st.verifiedPropDefs
      { st with
        drainingTermDefs := false
        pendingTermDefs := #[]
        seenPendingTermDefs := {}
        tempVerifiedData := {}
        tempVerifiedPropInducts := {}
        tempVerifiedPropDefs := {}
        tempVerifiedTermDefs := {}
        verifiedTermDefs := vt
        verifiedData := vd
        verifiedPropInducts := vpi
        verifiedPropDefs := vpd }
    return none

private partial def scanTermStep (origin : Name) (e : Expr)
    (inProgressData : List (Name × Array Expr))
    (inProgressProps : List (Name × Bool × Array Expr))
    (inProgressTerms : List Name) (fuel : Nat) :
    AuditM (Option (Name × Name)) := do
  if fuel == 0 then return some (origin, `termDepthExceeded)
  let e ← instantiateMVars e
  let e ← match e with
    | .proj ``Stage _ _ | .proj ``Registry _ _ | .proj ``RoutePipeline _ _ | .proj ``PassFunctions _ _ => whnf e
    | .proj _ _ _ => pure e
    | _ =>
      let e ← whnfCore e
      if e.getAppFn.isConstOf ``Stage.run || e.getAppFn.isConstOf ``RoutePipeline.run then
        whnf e
      else pure e
  -- Audit `e` directly if `e` itself is a `Prop`, or audit its type if `e` is a proof of a `Prop`.
  let eTy ← inferType e
  if (← whnf eTy) == .sort .zero then
    return ← auditPropExprCore origin e false inProgressData inProgressProps inProgressTerms (fuel - 1)
  if ← isProp eTy then
    if let some hit ← auditPropExprCore origin eTy false inProgressData inProgressProps inProgressTerms (fuel - 1) then
      return some hit
    -- Proof terms carry no computational information beyond the proposition they prove.
    return none
  let fn := e.getAppFn
  let args := e.getAppArgs
  if fn.isConstOf ``decide && args.size ≥ 1 then
    if let some hit ← auditPropExprCore origin args[0]! false inProgressData inProgressProps inProgressTerms (fuel - 1) then
      return some hit
  if (fn.isConstOf ``ite || fn.isConstOf ``dite) && args.size ≥ 2 then
    if let some hit ← auditPropExprCore origin args[1]! false inProgressData inProgressProps inProgressTerms (fuel - 1) then
      return some hit
  if (fn.isConstOf ``Classical.propDecidable || fn.isConstOf ``Classical.em) && args.size ≥ 1 then
    if let some hit ← auditPropExprCore origin args[0]! false inProgressData inProgressProps inProgressTerms (fuel - 1) then
      return some hit
  if (fn.isConstOf ``Classical.choose || fn.isConstOf ``Classical.indefiniteDescription) && args.size ≥ 2 then
    let α := args[0]!
    let p := args[1]!
    if !(← isProp α) then
      if let some _ ← rejectNonDataTypeCore α inProgressData (fuel - 1) false then
        return some (origin, `Classical.choose)
    let predHit ← withLocalDeclD `_x α fun x =>
      auditPropExprCore origin (mkApp p x) false inProgressData inProgressProps inProgressTerms (fuel - 1)
    if let some hit := predHit then return some hit
  -- Audit matcher expressions directly via their discriminants, remaining args, and arms
  -- (avoiding unfolding internal equation-compiler `*.match_1` `Eq`/`HEq` cast plumbing).
  if let some m ← matchMatcherApp? e then
    for discr in m.discrs do
      let discrTy ← whnf (← inferType discr)
      if discrTy == .sort .zero then
        if let some hit ← auditPropExprCore origin discr false inProgressData inProgressProps inProgressTerms (fuel - 1) then
          return some hit
      else if !discrTy.isSort then
        if let some hit ← scanTermStep origin discr inProgressData inProgressProps inProgressTerms fuel then
          return some hit
    for rem in m.remaining do
      let remTy ← whnf (← inferType rem)
      if remTy == .sort .zero then
        if let some hit ← auditPropExprCore origin rem false inProgressData inProgressProps inProgressTerms (fuel - 1) then
          return some hit
      else if remTy.isSort then
        if let some _ ← rejectNonDataTypeCore rem inProgressData (fuel - 1) false then
          return some (origin, `matcherRem)
      else
        if let some hit ← scanTermStep origin rem inProgressData inProgressProps inProgressTerms fuel then
          return some hit
    for alt in m.alts do
      if let some hit ← scanTermStep origin alt inProgressData inProgressProps inProgressTerms fuel then
        return some hit
    return none
  if let .const name lvls := fn then
    let env ← getEnv
    match env.find? name with
    | some (.opaqueInfo _) =>
      unless isCoreLibraryDecl env name || isPinnedLCBDecl env name do
        return some (origin, privateToUserName name)
    | some (.axiomInfo _) =>
      unless name == ``propext || name == ``Quot.sound || name == ``Classical.choice do
        return some (origin, privateToUserName name)
    | some (.defnInfo info) =>
      if isCoreUnfoldableWrapper name then
        if let some unfolded ← unfoldDefinition? e then
          return ← scanTermStep origin unfolded inProgressData inProgressProps inProgressTerms (fuel - 1)
      else if !isCoreLibraryDecl env name && !isPinnedLCBDecl env name then
        -- First scan actual arguments (catching polymorphic Prop/value arguments at call site).
        for arg in args do
          let argTy ← whnf (← inferType arg)
          if argTy == .sort .zero then
            if let some hit ← auditPropExprCore origin arg false inProgressData inProgressProps inProgressTerms (fuel - 1) then
              return some hit
          else if !argTy.isSort then
            if let some hit ← scanTermStep origin arg inProgressData inProgressProps inProgressTerms fuel then
              return some hit
        -- Instantiate level params and substitute concrete type/prop/instance arguments into
        -- the definition body so polymorphic helpers (e.g. `AList.lookup`) are audited at
        -- their actual concrete carrier types without leaving abstract type variables.
        let val := info.value.instantiateLevelParams info.levelParams lvls
        let rec substParams (cur : Expr) (i : Nat) (keyArgs : Array Expr) :
            MetaM (Expr × Array Expr) := do
          match cur with
          | .lam n dom body bi =>
            if i < args.size then
              let domW ← whnf dom
              let isLang := domW.isConstOf ``CoreLanguage ||
                domW.getAppFn.isConstOf ``LoweringLanguage ||
                domW.getAppFn.isConstOf ``Concrete.Contracts.Semantics ||
                domW.getAppFn.isConstOf ``ConcreteSpec.StageGrammar.SharedGrammar ||
                domW.isConstOf ``IRPoint || domW.isConstOf ``Route || domW.isConstOf ``BackendKind
              let isPred ← resultIsPropExpr domW
              if domW.isSort || (← isClass? domW).isSome || isLang || isPred then
                let aNorm ←
                  if domW.isSort || domW.isConstOf ``IRPoint || domW.isConstOf ``Route || domW.isConstOf ``BackendKind then
                    whnf (← instantiateMVars args[i]!)
                  else
                    whnfCore (← instantiateMVars args[i]!)
                substParams (body.instantiate1 aNorm) (i + 1) (keyArgs.push aNorm)
              else
                withLocalDecl n bi dom fun fv => do
                  let (body', keyArgs') ← substParams (body.instantiate1 fv) (i + 1) keyArgs
                  return (← mkLambdaFVars #[fv] body', keyArgs')
            else
              return (cur, keyArgs)
          | _ => return (cur, keyArgs)
        let (closed, keyArgs) ← substParams val 0 #[]
        let key? ← match ← canonicalizeKey? keyArgs with
          | some ca => pure (some (name, ca))
          | none => pure none
        if inProgressTerms.contains name then
          return none
        if let some key := key? then
          let st ← get
          if (← hasVerifiedTermDef key) || st.seenPendingTermDefs.contains key then
            return none
          if !closed.hasFVar then
            modify fun st =>
              { st with
                pendingTermDefs := st.pendingTermDefs.push (origin, key, closed)
                seenPendingTermDefs := st.seenPendingTermDefs.insert key }
            return none
        if let some hit ← scanTermStep origin closed inProgressData inProgressProps (name :: inProgressTerms) 512 then
          return some hit
        if let some key := key? then
          addVerifiedTermDef key
        return none
    | _ => pure ()
  -- When `Stage.mk` appears inside a residual `Pipeline.run` value term, scan `run`/`kind`
  -- as terms and audit its five `Prop`-valued contract fields at their actual stage polarities
  -- (`inputInvariant` negative; `outputInvariant`/`success`/`refusal`/`trust` positive).
  if fn.isConstOf ``Concrete.Interfaces.Stage.mk && args.size == 10 then
    if let some hit ← scanTermStep origin args[3]! inProgressData inProgressProps inProgressTerms fuel then
      return some hit
    if let some hit ← scanTermStep origin args[4]! inProgressData inProgressProps inProgressTerms fuel then
      return some hit
    for (idx, fieldPos) in #[(5, false), (6, true), (7, true), (8, true), (9, true)] do
      let field := args[idx]!
      let fieldTy ← inferType field
      let hit ← forallTelescope fieldTy fun fxs _ =>
        auditPropExprCore origin (mkAppN field fxs) fieldPos inProgressData inProgressProps inProgressTerms (fuel - 1)
      if let some hit := hit then return some hit
    return none
  -- Iteratively traverse `List.cons` chains without consuming recursion depth.
  if fn.isConstOf ``List.cons && args.size == 3 then
    let mut cur := e
    while cur.getAppFn.isConstOf ``List.cons && cur.getAppArgs.size == 3 do
      let cargs := cur.getAppArgs
      if let some hit ← scanTermStep origin cargs[1]! inProgressData inProgressProps inProgressTerms fuel then
        return some hit
      cur ← whnfCore (← instantiateMVars cargs[2]!)
    return ← scanTermStep origin cur inProgressData inProgressProps inProgressTerms fuel
  match e with
  | .app .. =>
    -- Visit head (if non-constant) and all arguments in a flat loop instead of recursing down the app spine.
    unless fn.isConst || fn.isFVar do
      if let some hit ← scanTermStep origin fn inProgressData inProgressProps inProgressTerms fuel then
        return some hit
    for a in args do
      let aTy ← whnf (← inferType a)
      if aTy == .sort .zero then
        if let some hit ← auditPropExprCore origin a false inProgressData inProgressProps inProgressTerms (fuel - 1) then
          return some hit
      else if !aTy.isSort then
        if let some hit ← scanTermStep origin a inProgressData inProgressProps inProgressTerms fuel then
          return some hit
    return none
  | .lam name dom body bi =>
    let domW ← whnf dom
    if domW == .sort .zero then
      scanTermStep origin (body.instantiate1 (mkConst ``True)) inProgressData inProgressProps inProgressTerms fuel
    else if let .sort (.succ u) := domW then
      scanTermStep origin (body.instantiate1 (mkConst ``PUnit [Level.succ u])) inProgressData inProgressProps inProgressTerms fuel
    else if ← isProp dom then
      if let some hit ← auditPropExprCore origin dom false inProgressData inProgressProps inProgressTerms (fuel - 1) then
        return some hit
      withLocalDecl name bi dom fun fv =>
        scanTermStep origin (body.instantiate1 fv) inProgressData inProgressProps inProgressTerms fuel
    else
      withLocalDecl name bi dom fun fv =>
        scanTermStep origin (body.instantiate1 fv) inProgressData inProgressProps inProgressTerms fuel
  | .letE name t v b _ =>
    if (← whnf t) == .sort .zero then
      if let some hit ← auditPropExprCore origin v false inProgressData inProgressProps inProgressTerms (fuel - 1) then
        return some hit
    else if ← isProp t then
      if let some hit ← auditPropExprCore origin t false inProgressData inProgressProps inProgressTerms (fuel - 1) then
        return some hit
    else if !(← whnf t).isSort then
      if let some hit ← scanTermStep origin v inProgressData inProgressProps inProgressTerms fuel then
        return some hit
    withLetDecl name t v fun fv =>
      scanTermStep origin (b.instantiate1 fv) inProgressData inProgressProps inProgressTerms fuel
  | .mdata _ body | .proj _ _ body =>
    scanTermStep origin body inProgressData inProgressProps inProgressTerms fuel
  | _ => return none

/-- Recognize the exact five finite-ID structural renaming maps carried by the frozen
`SSAGraph.LowersFrom` correspondence (`BlockId → BlockId`, `Nat → Nat`, `SSAEnv Ty lang`,
`BlockId → SSAEnv Ty lang`, and `BlockId → List (SSABlock × LocatedOp SSAInst)`), verifying
that every codomain carrier is data-only. Never admits arbitrary arrows or decoders. -/
private partial def isSSACorrespondenceType (origin : Name) (ty : Expr)
    (inProgressData : List (Name × Array Expr)) (fuel : Nat) : AuditM Bool := do
  let ssaOrigins : List Name :=
    [``Concrete.Interfaces.SSAGraph.LowersFrom,
      ``Concrete.Interfaces.SSAGraph.InstsLower,
      ``Concrete.Interfaces.WriteDest,
      ``Concrete.Interfaces.WriteBindings,
      ``Concrete.Interfaces.EdgeLowers,
      ``Concrete.Interfaces.TermLowers,
      ``Concrete.Interfaces.EdgeTransfersEnv]
  unless ssaOrigins.contains origin do return false
  let tyW ← whnf ty
  match tyW with
  | .forallE _ d b _ =>
    if b.hasLooseBVars then return false
    let dW ← whnf d
    let bW ← whnf b
    if dW.isConstOf ``ConcreteSpec.Core.BlockId && bW.isConstOf ``ConcreteSpec.Core.BlockId then
      return true
    if dW.isConstOf ``Nat && bW.isConstOf ``Nat then
      return true
    if dW.isConstOf ``ConcreteSpec.Core.PlaceId then
      -- `SSAEnv Ty lang = PlaceId → Option (Sigma (SSARead Ty lang.literal))`
      if bW.getAppFn.isConstOf ``Option && bW.getAppArgs.size == 1 then
        return (← rejectNonDataTypeCore bW inProgressData (fuel - 1) false).isNone
      return false
    if dW.isConstOf ``ConcreteSpec.Core.BlockId then
      -- `BlockId → SSAEnv Ty lang`
      if bW.isForall then
        return ← isSSACorrespondenceType origin bW inProgressData fuel
      -- `BlockId → List (SSABlock lang × LocatedOp (SSAInst lang))`
      if bW.getAppFn.isConstOf ``List && bW.getAppArgs.size == 1 then
        return (← rejectNonDataTypeCore bW inProgressData (fuel - 1) false).isNone
      return false
    return false
  | _ => return false

/-- Verify a `Production value` type appearing inside fixed-grammar `Derives`/`Predictive`
constructors is backed by data-only nonterminal and semantic-value families. -/
private partial def isDataBackedProduction (ty : Expr) (inProgressData : List (Name × Array Expr))
    (fuel : Nat) : AuditM Bool := do
  let tyW ← whnf ty
  if tyW.getAppFn.isConstOf ``Production && tyW.getAppArgs.size == 2 then
    let nTy := tyW.getAppArgs[0]!
    let vFam := tyW.getAppArgs[1]!
    let okN := (← rejectNonDataTypeCore nTy inProgressData (fuel - 1) false).isNone
    let okV ← withLocalDeclD `_n nTy fun n => do
      pure (← rejectNonDataTypeCore (mkApp vFam n) inProgressData (fuel - 1) false).isNone
    return okN && okV
  else if tyW.getAppFn.isConstOf ``List && tyW.getAppArgs.size == 1 then
    let elemW ← whnf tyW.getAppArgs[0]!
    if elemW.getAppFn.isConstOf ``Production && elemW.getAppArgs.size == 2 then
      let nTy := elemW.getAppArgs[0]!
      let vFam := elemW.getAppArgs[1]!
      let okN := (← rejectNonDataTypeCore nTy inProgressData (fuel - 1) false).isNone
      let okV ← withLocalDeclD `_n nTy fun n => do
        pure (← rejectNonDataTypeCore (mkApp vFam n) inProgressData (fuel - 1) false).isNone
      return okN && okV
    else return false
  else return false

/-- Audit a proposition expression using the same allow-list classifier for every
quantified binder (`∀`, `∃`, `Nonempty`, inductive predicate constructor fields read from
actual declarations, matcher discriminants, equality types, and embedded `decide`/`ite`
conditions). Uses `whnfCore` + `withLetDecl` so let-bound propositions (ISS-0131),
projections, typeclass instances, and aliases are reduced before classification. -/
private partial def auditPropExpr (origin : Name) (expr : Expr) (pos : Bool := true)
    (inProgressData : List (Name × Array Expr) := [])
    (inProgressProps : List (Name × Bool × Array Expr) := [])
    (inProgressTerms : List Name := [])
    (fuel : Nat := 512) : AuditM (Option (Name × Name)) := do
  let savedState ← get
  let res ← auditPropExprCore origin expr pos inProgressData inProgressProps inProgressTerms fuel
  if res.isSome then
    set savedState
  return res

private partial def auditPropExprCore (origin : Name) (expr : Expr) (pos : Bool)
    (inProgressData : List (Name × Array Expr))
    (inProgressProps : List (Name × Bool × Array Expr))
    (inProgressTerms : List Name)
    (fuel : Nat) : AuditM (Option (Name × Name)) := do
  if fuel == 0 then return some (origin, `depthExceeded)
  let expr ← instantiateMVars expr
  let expr ← whnfCore expr
  let expr ← match expr.getAppFn with
    | .proj ``Concrete.Contracts.Semantics _ _
    | .proj ``Stage _ _ | .proj ``Registry _ _ | .proj ``RoutePipeline _ _ | .proj ``PassFunctions _ _ =>
      whnf expr
    | _ => pure expr
  match expr with
  | .mdata _ body =>
    auditPropExprCore origin body pos inProgressData inProgressProps inProgressTerms (fuel - 1)
  | .letE name t v b _ =>
    if (← whnf t) == .sort .zero then
      if let some hit ← auditPropExprCore origin v false inProgressData inProgressProps inProgressTerms (fuel - 1) then
        return some hit
    else if !(← isProp t) && !(← whnf t).isSort then
      if let some hit ← scanTerm origin v inProgressData inProgressProps inProgressTerms (fuel - 1) then
        return some hit
    withLetDecl name t v fun fv =>
      auditPropExprCore origin (b.instantiate1 fv) pos inProgressData inProgressProps inProgressTerms (fuel - 1)
  | .forallE name dom body bi =>
    if ← isProp dom then
      if let some hit ← auditPropExprCore origin dom (!pos) inProgressData inProgressProps inProgressTerms (fuel - 1) then
        return some hit
      withLocalDecl name bi dom fun fv =>
        auditPropExprCore origin (body.instantiate1 fv) pos inProgressData inProgressProps inProgressTerms (fuel - 1)
    else
      let allowBeh := origin == ``Concrete.Interfaces.P_print
      -- Infinite execution runs (`Nat → State`) in `BehaviorRefinement` only.
      let isTraceRun ← do
        if origin == ``Concrete.Interfaces.BehaviorRefinement && (name == `run || name == `matching) then
          let dw ← whnf dom
          match dw with
          | .forallE _ d b _ =>
            if (← whnf d).isConstOf ``Nat && !b.hasLooseBVars then
              pure (← rejectNonDataTypeCore b inProgressData (fuel - 1) false).isNone
            else pure false
          | _ => pure false
        else pure false
      -- Production membership quantification (`∀ p ∈ g.productions.toList, ...`) over a
      -- data-backed `SharedGrammar` (used by `CheckedTable.acyclic`).
      let isBoundedProduction ← do
        let dw ← whnf dom
        if dw.getAppFn.isConstOf ``Production then
          if ← isDataBackedProduction dom inProgressData fuel then
            match body with
            | .forallE _ prem _ _ =>
              let premW ← whnfCore prem
              let pfn := premW.getAppFn
              let pargs := premW.getAppArgs
              let isMem :=
                (pfn.isConstOf ``Membership.mem && pargs.size == 5 &&
                  pargs[4]! == .bvar 0 &&
                  pargs[3]!.getAppFn.isConstOf ``Array.toList &&
                  pargs[3]!.getAppArgs.size == 2 &&
                  pargs[3]!.getAppArgs[1]!.getAppFn.isConstOf ``SharedGrammar.productions) ||
                (pfn.isConstOf ``List.Mem && pargs.size == 3 &&
                  pargs[1]! == .bvar 0 &&
                  pargs[2]!.getAppFn.isConstOf ``Array.toList &&
                  pargs[2]!.getAppArgs.size == 2 &&
                  pargs[2]!.getAppArgs[1]!.getAppFn.isConstOf ``SharedGrammar.productions)
              pure isMem
            | _ => pure false
          else pure false
        else pure false
      if !isTraceRun && !isBoundedProduction then
        if let some _ ← rejectNonDataTypeCore dom inProgressData (fuel - 1) allowBeh then
          return some (origin, name)
      withLocalDecl name bi dom fun fv =>
        auditPropExprCore origin (body.instantiate1 fv) pos inProgressData inProgressProps inProgressTerms (fuel - 1)
  | _ =>
    if let some m ← matchMatcherApp? expr then
      -- Audit every matcher discriminant (type and embedded props/decisions; Counterexample 4b)
      -- and apply `m.remaining` to every alternative.
      for discr in m.discrs do
        let discrTy ← inferType discr
        let discrTyW ← whnf discrTy
        if discrTyW == .sort .zero then
          if let some hit ← auditPropExprCore origin discr false inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
        else if ← isProp discrTy then
          if let some hit ← auditPropExprCore origin discrTy false inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
        else
          let allowBeh := origin == ``Concrete.Interfaces.P_print
          if let some _ ← rejectNonDataTypeCore discrTy inProgressData (fuel - 1) allowBeh then
            return some (origin, `matcherDiscr)
          if let some hit ← scanTerm origin discr inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
      for rem in m.remaining do
        let remTy ← whnf (← inferType rem)
        if remTy == .sort .zero then
          if let some hit ← auditPropExprCore origin rem false inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
        else if !remTy.isSort then
          if let some hit ← scanTerm origin rem inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
      for i in [:m.alts.size] do
        let alt := m.alts[i]!
        let nParams := m.altNumParams[i]!
        let hit ← lambdaBoundedTelescope alt nParams fun _ body => do
          let rec applyRemaining (cur : Expr) (rems : List Expr) : AuditM (Option (Name × Name)) := do
            match rems with
            | [] => auditPropExprCore origin cur pos inProgressData inProgressProps inProgressTerms (fuel - 1)
            | rem :: rest =>
              let remTy ← inferType rem
              let remIsBelow := match remTy.getAppFn with
                | .const cname _ => cname.getString! == "below"
                | _ => false
              if remIsBelow then
                let curW ← whnfCore cur
                match curW with
                | .lam nm dom b bi =>
                  withLocalDecl nm bi dom fun fv =>
                    applyRemaining (b.instantiate1 fv) rest
                | _ =>
                  applyRemaining (mkApp cur rem) rest
              else
                applyRemaining (mkApp cur rem) rest
          applyRemaining body m.remaining.toList
        if let some hit := hit then return some hit
      return none
    let fn := expr.getAppFn
    let args := expr.getAppArgs
    if fn.isConstOf ``ite || fn.isConstOf ``dite then
      if args.size ≥ 5 then
        let c := args[1]!
        if let some hit ← auditPropExprCore origin c false inProgressData inProgressProps inProgressTerms (fuel - 1) then
          return some hit
        let extra := args[5:]
        if fn.isConstOf ``dite then
          let h1 ← withLocalDeclD `_h c fun h =>
            auditPropExprCore origin (mkAppN (mkApp args[3]! h) extra) pos inProgressData inProgressProps inProgressTerms (fuel - 1)
          if let some hit := h1 then return some hit
          return ← withLocalDeclD `_h (mkApp (mkConst ``Not) c) fun h =>
            auditPropExprCore origin (mkAppN (mkApp args[4]! h) extra) pos inProgressData inProgressProps inProgressTerms (fuel - 1)
        else
          if let some hit ← auditPropExprCore origin (mkAppN args[3]! extra) pos inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
          return ← auditPropExprCore origin (mkAppN args[4]! extra) pos inProgressData inProgressProps inProgressTerms (fuel - 1)
    if fn.isConstOf ``And || fn.isConstOf ``Or then
      for arg in args do
        if let some hit ← auditPropExprCore origin arg pos inProgressData inProgressProps inProgressTerms (fuel - 1) then
          return some hit
      return none
    if fn.isConstOf ``Iff then
      for arg in args do
        if let some hit ← auditPropExprCore origin arg false inProgressData inProgressProps inProgressTerms (fuel - 1) then
          return some hit
      return none
    if fn.isConstOf ``Bool.Reflects && args.size ≥ 2 then
      if let some hit ← scanTerm origin args[0]! inProgressData inProgressProps inProgressTerms (fuel - 1) then
        return some hit
      return ← auditPropExprCore origin args[1]! false inProgressData inProgressProps inProgressTerms (fuel - 1)
    if fn.isConstOf ``True || fn.isConstOf ``False then
      return none
    if fn.isConstOf ``Exists then
      if args.size ≥ 2 then
        let α := args[0]!
        let p := args[1]!
        let bname := match p with | .lam n .. => n | _ => `witness
        if ← isProp α then
          -- Prop-valued existential domain (Counterexample 4a: `¬ (∃ _ : W, True)`).
          if let some hit ← auditPropExprCore origin α false inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
        else
          let αw ← whnf α
          -- Infinite execution runs (`Nat → State`) in `BehaviorRefinement` and `P_print`.
          let isTraceRun ← do
            if (origin == ``Concrete.Interfaces.BehaviorRefinement || origin == ``Concrete.Interfaces.P_print) &&
                (bname == `run || bname == `matching) then
              match αw with
              | .forallE _ d b _ =>
                if (← whnf d).isConstOf ``Nat && !b.hasLooseBVars then
                  let bw ← whnf b
                  if origin == ``Concrete.Interfaces.P_print && !bw.getAppFn.isConstOf ``MemoryState then
                    pure false
                  else
                    pure (← rejectNonDataTypeCore b inProgressData (fuel - 1) false).isNone
                else pure false
              | _ => pure false
            else pure false
          -- Positive checkpoint-simulation witness in `LowersMemory` only.
          let isMemorySim ← do
            if pos && origin == ``Concrete.Interfaces.LowersMemory && bname == `sim &&
                αw.getAppFn.isConstOf ``CheckpointSimulation then
              let sargs := αw.getAppArgs
              if sargs.size ≥ 6 then
                let okA := (← rejectNonDataTypeCore sargs[0]! inProgressData (fuel - 1) false).isNone
                let okB := (← rejectNonDataTypeCore sargs[1]! inProgressData (fuel - 1) false).isNone
                let okE := (← rejectNonDataTypeCore sargs[2]! inProgressData (fuel - 1) false).isNone
                let okO := (← rejectNonDataTypeCore sargs[3]! inProgressData (fuel - 1) false).isNone
                let okS := (← rejectNonDataTypeCore (mkProj ``Concrete.Contracts.Semantics 0 sargs[4]!) inProgressData (fuel - 1) false).isNone
                let okT := (← rejectNonDataTypeCore (mkProj ``Concrete.Contracts.Semantics 0 sargs[5]!) inProgressData (fuel - 1) false).isNone
                pure (okA && okB && okE && okO && okS && okT)
              else pure false
            else pure false
          let isSSACorr ← isSSACorrespondenceType origin α inProgressData fuel
          if !isTraceRun && !isMemorySim && !isSSACorr then
            if let some _ ← rejectNonDataTypeCore α inProgressData (fuel - 1) false then
              return some (origin, bname)
        return ← withLocalDeclD bname α fun fv =>
          auditPropExprCore origin (mkApp p fv) pos inProgressData inProgressProps inProgressTerms (fuel - 1)
      return none
    if fn.isConstOf ``Nonempty then
      if args.size ≥ 1 then
        let α ← whnf args[0]!
        if ← isProp α then
          return ← auditPropExprCore origin α false inProgressData inProgressProps inProgressTerms (fuel - 1)
        if let .forallE name dom _ _ := α then
          if let some _ ← rejectNonDataTypeCore dom inProgressData (fuel - 1) false then
            return some (origin, name)
        if let some _ ← rejectNonDataTypeCore α inProgressData (fuel - 1) false then
          return some (origin, `witness)
      return none
    if fn.isConstOf ``Eq then
      if args.size ≥ 3 then
        let eqTy := args[0]!
        let lhs := args[1]!
        let rhs := args[2]!
        let eqTyW ← whnf eqTy
        if eqTyW == .sort .zero then
          if let some hit ← auditPropExprCore origin lhs false inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
          return ← auditPropExprCore origin rhs false inProgressData inProgressProps inProgressTerms (fuel - 1)
        if eqTyW.isSort then
          if let some _ ← rejectNonDataTypeCore lhs inProgressData (fuel - 1) false then
            return some (origin, `eqType)
          if let some _ ← rejectNonDataTypeCore rhs inProgressData (fuel - 1) false then
            return some (origin, `eqType)
          return none
        -- Check the equality's carrier type against the data allow-list (Counterexample 4c),
        -- admitting only `Pipeline.run ... = .ok output` at `Pipeline.endToEnd` / `registeredLCBEndToEnd`
        -- when `output`'s type is data-only, fixed `sliceLanguage s₁ = sliceLanguage s₂` witnesses,
        -- fixed `Exec.Out` in `Exec.Step`, and `Option (Production g.value)` in `Predictive`.
        let isPredictiveProdEq ← do
          if origin == ``ConcreteSpec.StageGrammar.Predictive &&
              eqTyW.getAppFn.isConstOf ``Option && eqTyW.getAppArgs.size == 1 then
            isDataBackedProduction eqTyW.getAppArgs[0]! inProgressData fuel
          else pure false
        if let some okTy ← isPipelineRunOkEq origin eqTy lhs rhs then
          if let some _ ← rejectNonDataTypeCore okTy inProgressData (fuel - 1) false then
            return some (origin, `eqType)
        else if ← isFixedSliceLanguageEq eqTy lhs rhs then
          pure ()
        else if ← isFixedExecStepOutEq origin eqTy then
          pure ()
        else if isPredictiveProdEq then
          pure ()
        else if ← isSSACorrespondenceType origin eqTy inProgressData fuel then
          pure ()
        else
          if let some _ ← rejectNonDataTypeCore eqTy inProgressData (fuel - 1) false then
            return some (origin, `eqType)
        if let some hit ← scanTerm origin lhs inProgressData inProgressProps inProgressTerms (fuel - 1) then
          return some hit
        return ← scanTerm origin rhs inProgressData inProgressProps inProgressTerms (fuel - 1)
      return none
    if fn.isConstOf ``HEq then
      if args.size ≥ 4 then
        for tyArg in #[args[0]!, args[2]!] do
          if (← whnf tyArg) != .sort .zero then
            if let some _ ← rejectNonDataTypeCore tyArg inProgressData (fuel - 1) false then
              return some (origin, `heqType)
        if let some hit ← scanTerm origin args[1]! inProgressData inProgressProps inProgressTerms (fuel - 1) then
          return some hit
        return ← scanTerm origin args[3]! inProgressData inProgressProps inProgressTerms (fuel - 1)
      return none
    if fn.isConstOf ``List.foldr && args.size ≥ 5 then
      let α := args[0]!
      let β := args[1]!
      let stepFn := args[2]!
      let initVal := args[3]!
      let xsVal := args[4]!
      let extra := args[5:]
      if let some _ ← rejectNonDataTypeCore α inProgressData (fuel - 1) false then
        return some (origin, `List.foldr)
      unless ← resultIsPropExpr β do
        return some (origin, `List.foldr)
      let domErr ← forallTelescope β fun bxs _ => do
        for bx in bxs do
          let bxTy ← inferType bx
          if !(← isProp bxTy) then
            if let some _ ← rejectNonDataTypeCore bxTy inProgressData (fuel - 1) false then
              return some (origin, ← bx.fvarId!.getUserName)
        return none
      if let some hit := domErr then return some hit
      if let some hit ← scanTerm origin xsVal inProgressData inProgressProps inProgressTerms (fuel - 1) then
        return some hit
      for ex in extra do
        if let some hit ← scanTerm origin ex inProgressData inProgressProps inProgressTerms (fuel - 1) then
          return some hit
      let initHit ← forallTelescope β fun bxs _ =>
        auditPropExprCore origin (mkAppN initVal bxs) pos inProgressData inProgressProps inProgressTerms (fuel - 1)
      if let some hit := initHit then return some hit
      let ihDummy ← mkTruePred β
      return ← withLocalDeclD `_elem α fun elem =>
        forallTelescope β fun bxs _ =>
          auditPropExprCore origin (mkAppN (mkApp2 stepFn elem ihDummy) bxs)
            pos inProgressData inProgressProps inProgressTerms (fuel - 1)
    if let .const name lvls := fn then
      let root := privateToUserName name
      let env ← getEnv
      match env.find? name with
      | some (.opaqueInfo _) =>
        -- The sole permitted opaque predicate is the external LLVM behavior boundary in `P_print`:
        -- `llvmBehaviors {E : Type} (bytes : LLVMPackageBytes) (behavior : Behavior E) : Prop`.
        if name == ``Concrete.Interfaces.llvmBehaviors && origin == ``Concrete.Interfaces.P_print &&
            args.size == 3 then
          if let some _ ← rejectNonDataTypeCore args[0]! inProgressData (fuel - 1) false then
            return some (origin, `llvmBehaviors)
          if let some hit ← scanTerm origin args[1]! inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
          if let some _ ← rejectNonDataTypeCore (← inferType args[1]!) inProgressData (fuel - 1) false then
            return some (origin, `llvmBehaviors)
          if let some hit ← scanTerm origin args[2]! inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
          if let some _ ← rejectNonDataTypeCore (← inferType args[2]!) inProgressData (fuel - 1) true then
            return some (origin, `llvmBehaviors)
          return none
        return some (origin, root)
      | some (.axiomInfo _) =>
        return some (origin, root)
      | some (.recInfo info) =>
        -- Audit recursor eliminations into `Prop` (e.g. `Bool.casesOn (decide W) ...`, Counterexample 4b).
        let minorStart := info.numParams + info.numMotives
        let minorEnd := minorStart + info.numMinors
        let majorIdx := minorEnd + info.numIndices
        if args.size < majorIdx + 1 then
          return some (origin, root)
        let major := args[majorIdx]!
        let majorTy ← inferType major
        let majorTyW ← whnf majorTy
        if majorTyW == .sort .zero then
          if let some hit ← auditPropExprCore origin major false inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
        else if ← isProp majorTy then
          if let some hit ← auditPropExprCore origin majorTy false inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
        else
          let allowBeh := origin == ``Concrete.Interfaces.P_print
          if let some _ ← rejectNonDataTypeCore majorTy inProgressData (fuel - 1) allowBeh then
            return some (origin, root)
          if let some hit ← scanTerm origin major inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
        for idxPos in [minorEnd:majorIdx] do
          if let some hit ← scanTerm origin args[idxPos]! inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
        let extra := args[majorIdx + 1:]
        for offset in [:info.numMinors] do
          let minor := args[minorStart + offset]!
          let nBinders := countRecMinorBinders info.type (minorStart + offset)
          let minorTy ← inferType minor
          let hit ← forallBoundedTelescope minorTy (some nBinders) fun mxs _ => do
            let mut appArgs : Array Expr := #[]
            for mx in mxs do
              let mxTy ← inferType mx
              if ← resultIsPropExpr mxTy then
                appArgs := appArgs.push (← mkTruePred mxTy)
              else
                appArgs := appArgs.push mx
            auditPropExprCore origin (mkAppN (mkAppN minor appArgs) extra) pos
              inProgressData inProgressProps inProgressTerms (fuel - 1)
          if let some hit := hit then return some hit
        return none
      | some (.defnInfo info) =>
        -- Handle Lean's `*.brecOn` structural recursion into `Prop` directly by scanning its
        -- target/index arguments and auditing the step function `F` under its `*.below` IH binder.
        if isCoreLibraryDecl env name && name.getString! == "brecOn" then
          let nParams :=
            let rec countPis : Expr → Nat
              | .forallE _ _ b _ => 1 + countPis b
              | .mdata _ b => countPis b
              | _ => 0
            countPis info.type
          if nParams ≥ 2 && args.size ≥ nParams then
            let stepFn := args[nParams - 1]!
            let extra := args[nParams:].toArray
            for i in [:nParams - 1] do
              let a := args[i]!
              let aTy ← whnf (← inferType a)
              if !aTy.isSort && !aTy.isForall && !(← isProp aTy) then
                if let some hit ← scanTerm origin a inProgressData inProgressProps inProgressTerms (fuel - 1) then
                  return some hit
            for ex in extra do
              let exTy ← whnf (← inferType ex)
              if exTy == .sort .zero then
                if let some hit ← auditPropExprCore origin ex false inProgressData inProgressProps inProgressTerms (fuel - 1) then
                  return some hit
              else if !exTy.isSort then
                if let some hit ← scanTerm origin ex inProgressData inProgressProps inProgressTerms (fuel - 1) then
                  return some hit
            let stepTy ← inferType stepFn
            return ← forallTelescope stepTy fun sxs _ => do
              let body := mkAppN stepFn sxs
              let body' := if extra.isEmpty then body else mkAppN body extra
              -- If `stepFn` returns `d₁ → ... → Prop`, `sxs` already includes `extra`'s binders.
              let applied :=
                if sxs.size > 0 && extra.size > 0 then
                  -- Apply `stepFn` only to the non-trailing binders of `stepTy`, then `extra`.
                  let nStepOnly := sxs.size - extra.size
                  mkAppN (mkAppN stepFn sxs[:nStepOnly]) extra
                else
                  body'
              auditPropExprCore origin applied pos inProgressData inProgressProps inProgressTerms (fuel - 1)
        if isAllowedAtomicProp env name then
          for arg in args do
            let argTy ← whnf (← inferType arg)
            if argTy.isSort then
              if argTy != .sort .zero then
                let okProd ←
                  if origin == ``ConcreteSpec.StageGrammar.Derives ||
                      (name == ``List.Mem && args.size == 3 &&
                        args[2]!.getAppFn.isConstOf ``Array.toList &&
                        args[2]!.getAppArgs.size == 2 &&
                        args[2]!.getAppArgs[1]!.getAppFn.isConstOf ``SharedGrammar.productions) then
                    isDataBackedProduction arg inProgressData fuel
                  else pure false
                if !okProd then
                  if let some _ ← rejectNonDataTypeCore arg inProgressData (fuel - 1) false then
                    return some (origin, root)
            else if ← resultIsPropExpr argTy then
              let predHit ← forallTelescope argTy fun fxs _ => do
                for fx in fxs do
                  let fxTy ← inferType fx
                  if !(← isProp fxTy) then
                    if let some _ ← rejectNonDataTypeCore fxTy inProgressData (fuel - 1) false then
                      return some (origin, ← fx.fvarId!.getUserName)
                auditPropExprCore origin (mkAppN arg fxs) false inProgressData inProgressProps inProgressTerms (fuel - 1)
              if let some hit := predHit then return some hit
            else
              if let some hit ← scanTerm origin arg inProgressData inProgressProps inProgressTerms (fuel - 1) then
                return some hit
          return none
        let canCache := isAuditPropNamespace env name && !isCombinatorOrigin root
        let cacheKey? ← if canCache then do
          let mut normArgs : Array Expr := #[]
          for i in [:args.size] do
            normArgs := normArgs.push (← normalizeMemoArg args[i]!)
          match ← canonicalizeKey? normArgs with
          | some cArgs => pure (some (name, pos, cArgs))
          | none => pure none
        else pure none
        if let some ck := cacheKey? then
          if inProgressProps.contains ck then return none
          if ← hasVerifiedPropDef ck then
            -- Before short-circuiting on a normalized cache hit, scan every non-fvar value
            -- argument so a call-site term like `Foo (badWidth bs)` can never ride a cached hit.
            for arg in args do
              if !arg.isFVar then
                let argTy ← whnf (← inferType arg)
                if !argTy.isSort && !argTy.isForall && !(← isProp argTy) then
                  if let some hit ← scanTerm origin arg inProgressData inProgressProps inProgressTerms (fuel - 1) then
                    return some hit
            return none
        let inProgressProps' := match cacheKey? with
          | some ck => ck :: inProgressProps
          | none => inProgressProps
        let nextOrigin :=
          if isAuditPropNamespace env name && !isCombinatorOrigin root then root else origin
        let hit? ← match ← unfoldDefinition? expr with
          | some unfolded =>
            pure (some (← auditPropExprCore nextOrigin unfolded pos inProgressData inProgressProps' inProgressTerms (fuel - 1)))
          | none =>
            let exprW ← whnf expr
            if exprW != expr then
              pure (some (← auditPropExprCore nextOrigin exprW pos inProgressData inProgressProps' inProgressTerms (fuel - 1)))
            else if isAuditPropNamespace env name then
              -- Structurally recursive `Prop` definitions (e.g. `AllLoads`, `OperandsImage`,
              -- `ParamStores`) do not reduce on an abstract list under `unfoldDefinition?`;
              -- instantiate their actual definition body and audit it via `recInfo`/`matchMatcherApp?`.
              let instVal := mkAppN (info.value.instantiateLevelParams info.levelParams lvls) args
              pure (some (← auditPropExprCore nextOrigin instVal pos inProgressData inProgressProps' inProgressTerms (fuel - 1)))
            else pure none
        match hit? with
        | some hit =>
          if hit.isNone then
            if let some ck := cacheKey? then
              addVerifiedPropDef ck
          return hit
        | none =>
          -- Fail closed on any stuck `Prop` definition outside the allow-list.
          return some (origin, root)
      | some (.inductInfo info) =>
        if isAllowedAtomicProp env name then
          for arg in args do
            let argTy ← whnf (← inferType arg)
            if argTy.isSort then
              if argTy != .sort .zero then
                let okProd ←
                  if origin == ``ConcreteSpec.StageGrammar.Derives ||
                      (name == ``List.Mem && args.size == 3 &&
                        args[2]!.getAppFn.isConstOf ``Array.toList &&
                        args[2]!.getAppArgs.size == 2 &&
                        args[2]!.getAppArgs[1]!.getAppFn.isConstOf ``SharedGrammar.productions) then
                    isDataBackedProduction arg inProgressData fuel
                  else pure false
                if !okProd then
                  if let some _ ← rejectNonDataTypeCore arg inProgressData (fuel - 1) false then
                    return some (origin, root)
            else if ← resultIsPropExpr argTy then
              let predHit ← forallTelescope argTy fun fxs _ => do
                for fx in fxs do
                  let fxTy ← inferType fx
                  if !(← isProp fxTy) then
                    if let some _ ← rejectNonDataTypeCore fxTy inProgressData (fuel - 1) false then
                      return some (origin, ← fx.fvarId!.getUserName)
                auditPropExprCore origin (mkAppN arg fxs) false inProgressData inProgressProps inProgressTerms (fuel - 1)
              if let some hit := predHit then return some hit
            else
              if let some hit ← scanTerm origin arg inProgressData inProgressProps inProgressTerms (fuel - 1) then
                return some hit
          return none
        if isAuditPropNamespace env name || isCoreLibraryDecl env name then
          -- Index-aware memoization over ALL normalized arguments (both params and indices).
          let mut normArgs : Array Expr := #[]
          for i in [:args.size] do
            normArgs := normArgs.push (← normalizeMemoArg args[i]!)
          let key? ← match ← canonicalizeKey? normArgs with
            | some ca => pure (some (name, pos, ca))
            | none => pure none
          if let some key := key? then
            if inProgressProps.contains key then return none
            if ← hasVerifiedPropInduct key then
              for arg in args do
                if !arg.isFVar then
                  let argTy ← whnf (← inferType arg)
                  if !argTy.isSort && !argTy.isForall && !(← isProp argTy) then
                    if let some hit ← scanTerm origin arg inProgressData inProgressProps inProgressTerms (fuel - 1) then
                      return some hit
              return none
          let inProgressProps' := match key? with
            | some k => k :: inProgressProps
            | none => inProgressProps
          let nextOrigin := if isCombinatorOrigin root then origin else root
          -- Read EVERY actual constructor from `env` (never hand-reconstructing premises; Counterexample 6).
          -- For indexed inductives, match constructor result indices against `args` using
          -- `matchIndexTerm`, skipping ONLY provably clashing constructors and auditing EVERY
          -- unassigned constructor field (including any added premise or decoder field).
          for ctor in info.ctors do
            let some (.ctorInfo cinfo) := env.find? ctor | return some (nextOrigin, ctor)
            let ctype := cinfo.type.instantiateLevelParams cinfo.levelParams lvls
            let hit ← forallBoundedTelescope ctype cinfo.numParams fun pxs body => do
              let mut b := body
              for i in [:Nat.min pxs.size args.size] do
                b := b.replaceFVar pxs[i]! args[i]!
              -- Try index matching when the inductive has indices.
              let matched? : Option (Option (Name × Name)) ←
                if info.numIndices > 0 && args.size == info.numParams + info.numIndices then
                  withNewMCtxDepth do
                    let (mvars, _, resTy) ← forallMetaTelescope b
                    let resArgs := resTy.getAppArgs
                    if resArgs.size != args.size then
                      pure none
                    else
                      let mut status := IndexMatch.matched
                      for i in [info.numParams:args.size] do
                        match ← matchIndexTerm resArgs[i]! args[i]! with
                        | .clash => status := .clash; break
                        | .unknown => status := .unknown
                        | .matched => pure ()
                      match status with
                      | .clash =>
                        -- Provably unreachable constructor for this concrete index.
                        pure (some none)
                      | .matched =>
                        -- Introduce fresh local variables for every unassigned constructor field
                        -- (instantiating dependent metavariables as we telescope so no raw `.mvar`
                        -- ever leaks into recursive calls or memo keys) and audit every field.
                        let rec auditMvars : List Expr → AuditM (Option (Name × Name))
                          | [] => return none
                          | mv :: rest => do
                            if ← mv.mvarId!.isAssigned then
                              auditMvars rest
                            else
                              let mdecl ← mv.mvarId!.getDecl
                              let fTy ← instantiateMVars mdecl.type
                              let fname := mdecl.userName
                              if ← isProp fTy then
                                if let some h ← auditPropExprCore nextOrigin fTy pos inProgressData inProgressProps' inProgressTerms (fuel - 1) then
                                  return some h
                              else
                                let dispatchCont := root == ``ConcreteSpec.Core.Exec.Step && fname == `k
                                let fixedProduction ← do
                                  if (ctor == ``ConcreteSpec.StageGrammar.Derives.apply && fname == `production) ||
                                     (ctor == ``ConcreteSpec.StageGrammar.Predictive.apply && fname == `p) then
                                    isDataBackedProduction fTy inProgressData fuel
                                  else pure false
                                let ssaCorr ← isSSACorrespondenceType nextOrigin fTy inProgressData fuel
                                if !dispatchCont && !fixedProduction && !ssaCorr then
                                  if let some _ ← rejectNonDataTypeCore fTy inProgressData (fuel - 1) false then
                                    return some (nextOrigin, fname)
                              withLocalDeclD fname fTy fun fv => do
                                mv.mvarId!.assign fv
                                auditMvars rest
                        let found ← auditMvars mvars.toList
                        pure (some found)
                      | .unknown =>
                        pure none
                else
                  pure none
              match matched? with
              | some res => return res
              | none =>
                forallTelescope b fun fields _ => do
                  for f in fields do
                    let fTy ← inferType f
                    let fname ← f.fvarId!.getUserName
                    if ← isProp fTy then
                      if let some hit ← auditPropExprCore nextOrigin fTy pos inProgressData inProgressProps' inProgressTerms (fuel - 1) then
                        return some hit
                    else
                      let dispatchCont := root == ``ConcreteSpec.Core.Exec.Step && fname == `k
                      let fixedProduction ← do
                        if (ctor == ``ConcreteSpec.StageGrammar.Derives.apply && fname == `production) ||
                           (ctor == ``ConcreteSpec.StageGrammar.Predictive.apply && fname == `p) then
                          isDataBackedProduction fTy inProgressData fuel
                        else pure false
                      let ssaCorr ← isSSACorrespondenceType nextOrigin fTy inProgressData fuel
                      if !dispatchCont && !fixedProduction && !ssaCorr then
                        if let some _ ← rejectNonDataTypeCore fTy inProgressData (fuel - 1) false then
                          return some (nextOrigin, fname)
                  return none
            if let some hit := hit then return some hit
          if let some key := key? then
            addVerifiedPropInduct key
          return none
        -- Fail closed on any unregistered `Prop`-valued inductive outside the allow-list.
        return some (origin, root)
      | _ => return some (origin, root)
    -- Atomic `sim.related p q` projection from the positive `CheckpointSimulation` witness in `LowersMemory`.
    if let .proj ``CheckpointSimulation 0 simExpr := fn then
      if (origin == ``Concrete.Interfaces.LowersMemory ||
          origin == ``Concrete.Interfaces.CheckpointSimulation.related) && simExpr.isFVar then
        for arg in args do
          if let some hit ← scanTerm origin arg inProgressData inProgressProps inProgressTerms (fuel - 1) then
            return some hit
        return none
    -- Handle Lean's `(I.brecOn.go ... t F).1` structural recursion into `Prop` directly
    -- (generated when `I.brecOn` unfolds in recursive `Prop` definitions like `AllLoads`).
    if let .proj ``PProd 0 inner := fn then
      let innerFn := inner.getAppFn
      let innerArgs := inner.getAppArgs
      let isBrecGo := match innerFn with
        | .const cname _ =>
          cname.getString! == "go" && cname.getPrefix.getString! == "brecOn"
        | _ => false
      if isBrecGo && innerArgs.size ≥ 2 then
        let stepFn := innerArgs[innerArgs.size - 1]!
        for i in [:innerArgs.size - 1] do
          let a := innerArgs[i]!
          let aTy ← whnf (← inferType a)
          if !aTy.isSort && !aTy.isForall && !(← isProp aTy) then
            if let some hit ← scanTerm origin a inProgressData inProgressProps inProgressTerms (fuel - 1) then
              return some hit
        for ex in args do
          let exTy ← whnf (← inferType ex)
          if exTy == .sort .zero then
            if let some hit ← auditPropExprCore origin ex false inProgressData inProgressProps inProgressTerms (fuel - 1) then
              return some hit
          else if !exTy.isSort then
            if let some hit ← scanTerm origin ex inProgressData inProgressProps inProgressTerms (fuel - 1) then
              return some hit
        let stepTy ← inferType stepFn
        return ← forallTelescope stepTy fun sxs _ => do
          let applied :=
            if sxs.size ≥ args.size && args.size > 0 then
              mkAppN (mkAppN stepFn sxs[:sxs.size - args.size]) args
            else
              mkAppN (mkAppN stepFn sxs) args
          auditPropExprCore origin applied pos inProgressData inProgressProps inProgressTerms (fuel - 1)
    -- Inductive-hypothesis projection `f.1` / `f.2.1` inside Lean's `*.brecOn` structural
    -- recursion on `Prop` definitions (`AllLoads`, `OperandsImage`, `ParamStores`, etc.),
    -- where `f` has type `*.below ...` (rejected in all user binders by `rejectNonDataTypeCore`).
    let rec stripPProdProj : Expr → Option Expr
      | .proj ``PProd _ inner =>
        match inner with
        | .fvar _ => some inner
        | _ => stripPProdProj inner
      | _ => none
    if let some baseFv := stripPProdProj fn then
      let baseTy ← inferType baseFv
      let isBelow := match baseTy.getAppFn with
        | .const cname _ => cname.getString! == "below"
        | _ => false
      if isBelow then
        for arg in args do
          let argTy ← whnf (← inferType arg)
          if argTy == .sort .zero then
            if let some hit ← auditPropExprCore origin arg false inProgressData inProgressProps inProgressTerms (fuel - 1) then
              return some hit
          else if !argTy.isSort then
            if let some hit ← scanTerm origin arg inProgressData inProgressProps inProgressTerms (fuel - 1) then
              return some hit
        return none
    -- Fail closed on any stuck non-constant `Prop` expression (e.g. abstract predicate fvar).
    return some (origin, `unknownProp)
end

/-- Recognize the exact registered top-level `Passes slice profile` instantiation:
`PassFunctions (sliceGrammarCount s) (sliceGrammar s) (sliceLanguage s) (sliceLoweringLanguage s) p`. -/
private def isRegisteredPassesType (dom : Expr) : AuditM Bool := do
  let domW ← whnf dom
  unless domW.getAppFn.isConstOf ``PassFunctions do return false
  let args := domW.getAppArgs
  unless args.size == 5 do return false
  let pTy ← inferType args[4]!
  unless (← whnf pTy).isConstOf ``Profile do return false
  let s1? := match args[1]!.getAppFn, args[1]!.getAppArgs with
    | .const ``sliceGrammar _, #[s] => some s
    | _, _ => none
  let s2? := match args[2]!.getAppFn, args[2]!.getAppArgs with
    | .const ``sliceLanguage _, #[s] => some s
    | _, _ => none
  let s3? := match args[3]!.getAppFn, args[3]!.getAppArgs with
    | .const ``sliceLoweringLanguage _, #[s] => some s
    | _, _ => none
  match s1?, s2?, s3? with
  | some s1, some s2, some s3 =>
    if (← isDefEq s1 s2) && (← isDefEq s1 s3) &&
        (← isDefEq args[0]! (mkApp (mkConst ``sliceGrammarCount) s1)) then
      return (← rejectNonDataType (← inferType s1)).isNone
    else return false
  | _, _, _ => return false

/-- Construct a `PassFunctions` value sharing the 9 stage fields of `basePasses` with a
concrete `passesAt` family so `foldPasses` and `pipelineAppend` reduce completely to
concrete `Pipeline.next`/`Pipeline.done` chains across all 4 `IRPoint`s. -/
private def mkConcretePasses (domW basePasses passesAtFn : Expr) : Expr :=
  let args := domW.getAppArgs
  mkAppN (mkConst ``PassFunctions.mk)
    (args ++ #[
      mkProj ``PassFunctions 0 basePasses,
      mkProj ``PassFunctions 1 basePasses,
      mkProj ``PassFunctions 2 basePasses,
      mkProj ``PassFunctions 3 basePasses,
      mkProj ``PassFunctions 4 basePasses,
      mkProj ``PassFunctions 5 basePasses,
      mkProj ``PassFunctions 6 basePasses,
      mkProj ``PassFunctions 7 basePasses,
      mkProj ``PassFunctions 8 basePasses,
      passesAtFn])

private def mkIRPassType (lang profile k : Expr) : MetaM Expr := do
  let langTy ← whnf (← inferType lang)
  let ty := langTy.getAppArgs[0]!
  return mkAppN (mkConst ``IRPass) #[ty, lang, profile, k]

private def mkPassesAtEmpty (lang profile : Expr) : MetaM Expr := do
  withLocalDeclD `k (mkConst ``IRPoint) fun k => do
    let passTy ← mkIRPassType lang profile k
    mkLambdaFVars #[k] (mkApp (mkConst ``List.nil [.zero]) passTy)

private def mkPassesAtList (lang profile : Expr) (mirList memList ssaList birList : Expr) : MetaM Expr := do
  let motive ← withLocalDeclD `k (mkConst ``IRPoint) fun k => do
    let passTy ← mkIRPassType lang profile k
    mkLambdaFVars #[k] (mkApp (mkConst ``List [.zero]) passTy)
  withLocalDeclD `k (mkConst ``IRPoint) fun k =>
    mkLambdaFVars #[k] (mkAppN (mkConst ``IRPoint.casesOn [.succ .zero])
      #[motive, k, mirList, memList, ssaList, birList])

/-- Audit a top-level theorem or predicate type/value. Top-level binders over `Bool`,
`Route`, `Edge`, `IRPoint`, `Profile`, and registered `PassFunctions` (expanding
`passesAt` across empty, singleton, and two-pass chains at all 4 `IRPoint`s) are
case-split so `Registry.pipeline`, `foldPasses`, `pipelineAppend`, and `Stage.Sound`
reduce completely without abstract stage parameters. -/
private partial def auditTopLevel (origin : Name) (expr : Expr) (pos : Bool := true)
    (allowTopPasses : Bool := true) (fuel : Nat := 128) : AuditM (Option (Name × Name)) := do
  if fuel == 0 then return some (origin, `depthExceeded)
  let expr ← instantiateMVars expr
  let expr ← whnfCore expr
  let expr ← match expr.getAppFn with
    | .proj ``Concrete.Contracts.Semantics _ _
    | .proj ``Stage _ _ | .proj ``Registry _ _ | .proj ``RoutePipeline _ _ | .proj ``PassFunctions _ _ =>
      whnf expr
    | _ => pure expr
  match expr with
  | .mdata _ body => auditTopLevel origin body pos allowTopPasses (fuel - 1)
  | .forallE name dom body bi =>
    if ← isProp dom then
      let domPos := if (← whnf body).isConstOf ``False then !pos else pos
      if let some hit ← auditPropExpr origin dom domPos then return some hit
      withLocalDecl name bi dom fun fv =>
        auditTopLevel origin (body.instantiate1 fv) pos allowTopPasses (fuel - 1)
    else
      let passesOk ← if allowTopPasses then isRegisteredPassesType dom else pure false
      if !passesOk then
        if let some _ ← rejectNonDataType dom then
          return some (origin, name)
      let domW ← whnf dom
      if passesOk then
        -- Instantiate `passesAt` with empty, 1-pass, and 2-pass lists at all 4 IR points
        -- so `Registry.pipeline` reduces completely to concrete `Pipeline` constructors.
        let lang := domW.getAppArgs[3]!
        let profile := domW.getAppArgs[4]!
        return ← withLocalDecl name bi dom fun basePasses => do
          let emptyFn ← mkPassesAtEmpty lang profile
          if let some hit ← auditTopLevel origin (body.instantiate1 (mkConcretePasses domW basePasses emptyFn))
              pos allowTopPasses (fuel - 1) then
            return some hit
          if origin == ``Pipeline.endToEnd || origin == ``registeredLCBEndToEnd then
            let mirTy ← mkIRPassType lang profile (mkConst ``IRPoint.mir)
            let memTy ← mkIRPassType lang profile (mkConst ``IRPoint.memory)
            let ssaTy ← mkIRPassType lang profile (mkConst ``IRPoint.ssa)
            let birTy ← mkIRPassType lang profile (mkConst ``IRPoint.bir)
            let oneHit ←
              withLocalDeclD `pMir1 mirTy fun pMir1 =>
              withLocalDeclD `pMem1 memTy fun pMem1 =>
              withLocalDeclD `pSsa1 ssaTy fun pSsa1 =>
              withLocalDeclD `pBir1 birTy fun pBir1 => do
                let mkSingle (ty p : Expr) :=
                  mkApp3 (mkConst ``List.cons [.zero]) ty p (mkApp (mkConst ``List.nil [.zero]) ty)
                let oneFn ← mkPassesAtList lang profile
                  (mkSingle mirTy pMir1) (mkSingle memTy pMem1)
                  (mkSingle ssaTy pSsa1) (mkSingle birTy pBir1)
                if let some hit ← auditTopLevel origin (body.instantiate1 (mkConcretePasses domW basePasses oneFn))
                    pos allowTopPasses (fuel - 1) then
                  return some hit
                withLocalDeclD `pMir2 mirTy fun pMir2 =>
                withLocalDeclD `pMem2 memTy fun pMem2 =>
                withLocalDeclD `pSsa2 ssaTy fun pSsa2 =>
                withLocalDeclD `pBir2 birTy fun pBir2 => do
                  let mkPair (ty p1 p2 : Expr) :=
                    mkApp3 (mkConst ``List.cons [.zero]) ty p1 (mkSingle ty p2)
                  let twoFn ← mkPassesAtList lang profile
                    (mkPair mirTy pMir1 pMir2) (mkPair memTy pMem1 pMem2)
                    (mkPair ssaTy pSsa1 pSsa2) (mkPair birTy pBir1 pBir2)
                  auditTopLevel origin (body.instantiate1 (mkConcretePasses domW basePasses twoFn))
                    pos allowTopPasses (fuel - 1)
            if let some hit := oneHit then return some hit
          return none
      if domW.isConstOf ``Bool then
        for b in [``Bool.false, ``Bool.true] do
          if let some hit ← auditTopLevel origin (body.instantiate1 (mkConst b)) pos allowTopPasses (fuel - 1) then
            return some hit
        return none
      if domW.isConstOf ``Route then
        for r in [``Route.checked, ``Route.unchecked] do
          if let some hit ← auditTopLevel origin (body.instantiate1 (mkConst r)) pos allowTopPasses (fuel - 1) then
            return some hit
        return none
      if domW.isConstOf ``IRPoint then
        for k in [``IRPoint.mir, ``IRPoint.memory, ``IRPoint.ssa, ``IRPoint.bir] do
          if let some hit ← auditTopLevel origin (body.instantiate1 (mkConst k)) pos allowTopPasses (fuel - 1) then
            return some hit
        return none
      if domW.isConstOf ``Edge then
        let env ← getEnv
        if let some (.inductInfo info) := env.find? ``Edge then
          for ctor in info.ctors do
            if let some hit ← auditTopLevel origin (body.instantiate1 (mkConst ctor)) pos allowTopPasses (fuel - 1) then
              return some hit
          return none
      if domW.isConstOf ``Profile then
        for bk in [``BackendKind.llvm, ``BackendKind.lcb] do
          let hit ← withLocalDeclD `target (mkConst ``Concrete.Backend.LCB.Target) fun target =>
            withLocalDeclD `slots (mkConst ``Nat) fun slots =>
            withLocalDeclD `bytes (mkConst ``Nat) fun bytes => do
              let prof := mkAppN (mkConst ``Profile.mk) #[mkConst bk, target, slots, bytes]
              auditTopLevel origin (body.instantiate1 prof) pos allowTopPasses (fuel - 1)
          if let some hit := hit then return some hit
        return none
      withLocalDecl name bi dom fun fv =>
        auditTopLevel origin (body.instantiate1 fv) pos allowTopPasses (fuel - 1)
  | .lam name dom body bi =>
    let passesOk ← if allowTopPasses then isRegisteredPassesType dom else pure false
    if !passesOk then
      if !(← isProp dom) then
        if let some _ ← rejectNonDataType dom then
          return some (origin, name)
    let domW ← whnf dom
    if passesOk then
      let lang := domW.getAppArgs[3]!
      let profile := domW.getAppArgs[4]!
      return ← withLocalDecl name bi dom fun basePasses => do
        let emptyFn ← mkPassesAtEmpty lang profile
        auditTopLevel origin (body.instantiate1 (mkConcretePasses domW basePasses emptyFn))
          pos allowTopPasses (fuel - 1)
    if domW.isConstOf ``Bool then
      for b in [``Bool.false, ``Bool.true] do
        if let some hit ← auditTopLevel origin (body.instantiate1 (mkConst b)) pos allowTopPasses (fuel - 1) then
          return some hit
      return none
    if domW.isConstOf ``Route then
      for r in [``Route.checked, ``Route.unchecked] do
        if let some hit ← auditTopLevel origin (body.instantiate1 (mkConst r)) pos allowTopPasses (fuel - 1) then
          return some hit
      return none
    if domW.isConstOf ``IRPoint then
      for k in [``IRPoint.mir, ``IRPoint.memory, ``IRPoint.ssa, ``IRPoint.bir] do
        if let some hit ← auditTopLevel origin (body.instantiate1 (mkConst k)) pos allowTopPasses (fuel - 1) then
          return some hit
      return none
    if domW.isConstOf ``Edge then
      let env ← getEnv
      if let some (.inductInfo info) := env.find? ``Edge then
        for ctor in info.ctors do
          if let some hit ← auditTopLevel origin (body.instantiate1 (mkConst ctor)) pos allowTopPasses (fuel - 1) then
            return some hit
        return none
    if domW.isConstOf ``Profile then
      for bk in [``BackendKind.llvm, ``BackendKind.lcb] do
        let hit ← withLocalDeclD `target (mkConst ``Concrete.Backend.LCB.Target) fun target =>
          withLocalDeclD `slots (mkConst ``Nat) fun slots =>
          withLocalDeclD `bytes (mkConst ``Nat) fun bytes => do
            let prof := mkAppN (mkConst ``Profile.mk) #[mkConst bk, target, slots, bytes]
            auditTopLevel origin (body.instantiate1 prof) pos allowTopPasses (fuel - 1)
        if let some hit := hit then return some hit
      return none
    withLocalDecl name bi dom fun fv =>
      auditTopLevel origin (body.instantiate1 fv) pos allowTopPasses (fuel - 1)
  | .sort .zero =>
    return none
  | _ =>
    auditPropExpr origin expr pos

/-- Audit a single executable pass field type: after full `whnf` unfolding of any
number of transparent abbreviations, it must be a first-order function `D₁ → ... → Dₖ → R`
(`k ≥ 1`) where every input `Dᵢ` and the return type `R` pass the data-only allow-list.
When `allowPassList` is set (solely for `PassFunctions.passesAt`), the return type may
be `List F` where `F` itself passes `checkPassFieldType` (first-order data-to-data). -/
private partial def checkPassFieldType (type : Expr) (arity : Nat := 0) (fuel : Nat := 64)
    (allowPassList : Bool := false) :
    AuditM (Option String) := do
  if fuel == 0 then return some "pass type unfolding depth exceeded"
  let type ← instantiateMVars type
  let type ← whnf type
  match type with
  | .mdata _ body => checkPassFieldType body arity (fuel - 1) allowPassList
  | .letE name t v b _ =>
    withLetDecl name t v fun fv =>
      checkPassFieldType (b.instantiate1 fv) arity (fuel - 1) allowPassList
  | .forallE name dom body bi =>
    if let some err ← rejectNonDataType dom then
      return some s!"pass parameter {name}: {err}"
    let domW ← whnf dom
    if allowPassList && domW.isConstOf ``IRPoint then
      for k in [``IRPoint.mir, ``IRPoint.memory, ``IRPoint.ssa, ``IRPoint.bir] do
        if let some err ← checkPassFieldType (body.instantiate1 (mkConst k)) (arity + 1) (fuel - 1) allowPassList then
          return some err
      return none
    withLocalDecl name bi dom fun fv =>
      checkPassFieldType (body.instantiate1 fv) (arity + 1) (fuel - 1) allowPassList
  | ret =>
    if arity == 0 then
      return some s!"pass field is not a function: {← ppExpr ret}"
    if allowPassList && ret.getAppFn.isConstOf ``List && ret.getAppArgs.size == 1 then
      return ← checkPassFieldType ret.getAppArgs[0]! 0 (fuel - 1) false
    rejectNonDataType ret

/-- Check every field of a pass-bundle record instantiated with `paramArgs`. -/
private def badPassFieldAtWithReason (record : Name) (paramArgs : Array Expr := #[]) :
    AuditM (Option (Name × String)) := do
  let env ← getEnv
  let some structureInfo := getStructureInfo? env record | return some (record, "not a structure")
  let some (.inductInfo recordInfo) := env.find? record | return some (record, "missing inductInfo")
  if paramArgs.size != recordInfo.numParams then return some (record, "parameter count mismatch")
  for field in structureInfo.fieldNames do
    let some (.defnInfo projection) := env.find? (record ++ field) | return some (field, "missing projection")
    let allowList := record == ``PassFunctions && field == `passesAt
    let bad ← forallBoundedTelescope projection.type (some (recordInfo.numParams + 1)) fun xs fieldTy => do
      let mut ty := fieldTy
      for i in [:paramArgs.size] do
        ty := ty.replaceFVar xs[i]! paramArgs[i]!
      checkPassFieldType ty 0 64 allowList
    if let some err := bad then return some (field, err)
  return none

private def badPassField (record : Name) : AuditM (Option Name) :=
  return (← badPassFieldAtWithReason record #[]).map Prod.fst

private def badRegisteredPasses : AuditM (Option (Name × String)) := do
  for bk in [``BackendKind.llvm, ``BackendKind.lcb] do
    let hit ←
      withLocalDeclD `slice (mkConst ``Slice) fun slice =>
      withLocalDeclD `target (mkConst ``Concrete.Backend.LCB.Target) fun target =>
      withLocalDeclD `slots (mkConst ``Nat) fun slots =>
      withLocalDeclD `bytes (mkConst ``Nat) fun bytes => do
        let profile := mkAppN (mkConst ``Profile.mk) #[mkConst bk, target, slots, bytes]
        badPassFieldAtWithReason ``PassFunctions
          #[mkApp (mkConst ``sliceGrammarCount) slice,
            mkApp (mkConst ``sliceGrammar) slice,
            mkApp (mkConst ``sliceLanguage) slice,
            mkApp (mkConst ``sliceLoweringLanguage) slice,
            profile]
    if let some f := hit then return some f
  return none

/-- Check a theorem, proposition definition, or `Prop`-valued inductive declaration. -/
private def checkDecl (name : Name) : AuditM (Option (Name × Name)) := do
  let env ← getEnv
  let root := privateToUserName name
  match env.find? name with
  | some (.thmInfo info) => auditTopLevel root info.type true true
  | some (.defnInfo info) =>
    let tyHit ←
      if ← resultIsPropExpr info.type then
        auditTopLevel root info.type true true
      else
        pure none
    if let some hit := tyHit then return some hit
    auditTopLevel root info.value true true
  | some (.inductInfo info) =>
    if ← resultIsPropExpr info.type then
      auditPropExpr root (.const name []) true
    else
      return some (root, `nonPropInductive)
  | _ => return some (root, `missingDeclaration)

-- Ties the audited field to the actual premise of the composition theorem.
example {A R B : Type} (edge : Stage A R B) :
    edge.Trusted = (∀ a b, edge.inputInvariant a → Concrete.Contracts.Graph edge.run a b →
      edge.trust a b) := rfl

example {A : Type} : (Pipeline.done (A := A)).Trusted = True := rfl

example {A R B C : Type} (edge : Stage A R B) (rest : Pipeline B C) :
    (Pipeline.next edge rest).Trusted = (edge.Trusted ∧ rest.Trusted) := rfl

/-- Every stage in a prefix pipeline carries direct/validator evidence and trivial trust. -/
private inductive DirectPrefix : {A B : Type} → Pipeline A B → Prop
  | done {A : Type} : DirectPrefix (.done : Pipeline A A)
  | next {A R B C : Type} {edge : Stage A R B} {rest : Pipeline B C} :
      (edge.kind = .directProof ∨ edge.kind = .provedValidator) →
      edge.trust = (fun _ _ => True) →
      DirectPrefix rest →
      DirectPrefix (.next edge rest)

/-- Read the trust field on the composed edges. The final constructor fixes
both the BIR input and the profile-selected backend output at this slice and profile. -/
private inductive FixedTrust (slice : Slice) (profile : Profile) :
    {A B : Type} → Pipeline A B → Prop
  | llvm {target maxSlots maxStack}
      {edge : Stage (BIR (sliceLoweringLanguage slice) ⟨.llvm, target, maxSlots, maxStack⟩)
        StageFailure (BackendOutput ⟨.llvm, target, maxSlots, maxStack⟩)} :
      profile = ⟨.llvm, target, maxSlots, maxStack⟩ →
      edge.kind = .namedPremise "P_print" →
      edge.trust = @P_print (sliceLanguage slice) (sliceLoweringLanguage slice)
        ⟨.llvm, target, maxSlots, maxStack⟩ →
      FixedTrust slice profile (.next edge .done)
  | lcb {target maxSlots maxStack}
      {edge : Stage (BIR (sliceLoweringLanguage slice) ⟨.lcb, target, maxSlots, maxStack⟩)
        StageFailure (BackendOutput ⟨.lcb, target, maxSlots, maxStack⟩)} :
      profile = ⟨.lcb, target, maxSlots, maxStack⟩ →
      edge.kind = .directProof →
      edge.trust = (fun _ _ => True) →
      FixedTrust slice profile (.next edge .done)
  | step {A R B C} {edge : Stage A R B} {rest : Pipeline B C} :
      (edge.kind = .directProof ∨ edge.kind = .provedValidator) →
      edge.trust = (fun _ _ => True) → FixedTrust slice profile rest →
      FixedTrust slice profile (.next edge rest)

private theorem fixedTrust_append {slice : Slice} {profile : Profile} {A B C : Type}
    {first : Pipeline A B} {second : Pipeline B C}
    (h1 : DirectPrefix first) (h2 : FixedTrust slice profile second) :
    FixedTrust slice profile (pipelineAppend first second) := by
  induction h1 with
  | done => exact h2
  | next hk ht _ ih => exact .step hk ht (ih h2)

private theorem directPrefix_foldPasses {slice : Slice} {profile : Profile} {k : IRPoint}
    (ps : List (IRPass (sliceLoweringLanguage slice) profile k)) :
    DirectPrefix (foldPasses (language := sliceLanguage slice) ps) := by
  induction ps with
  | nil => exact .done
  | cons _ _ ih => exact .next (Or.inr rfl) rfl ih

example (slice : Slice) (profile : Profile) (passes : Passes slice profile) (route : Route) :
    FixedTrust slice profile (Registry.pipeline passes route).pipeline := by
  obtain ⟨backend, target, slots, bytes⟩ := profile
  cases route <;> cases backend <;>
    repeat' first
      | exact FixedTrust.llvm rfl rfl rfl
      | exact FixedTrust.lcb rfl rfl rfl
      | apply FixedTrust.step (Or.inl rfl) rfl
      | apply FixedTrust.step (Or.inr rfl) rfl
      | apply fixedTrust_append (directPrefix_foldPasses _)

-- Existing negative controls (preserved across the freeze schema upgrade).
private abbrev Model := CoreLanguage

private def plantedStatement (profile : Profile) (passes : Passes slice0 profile) : Prop :=
  let lexer := {(registry passes).lexer with
    trust := fun _ _ => ∀ model : Model, model = model}
  let pipeline := Pipeline.next lexer .done
  pipeline.Trusted

private def plantedRelation (profile : Profile) (passes : Passes slice0 profile) : Prop :=
  let lexer := {(registry passes).lexer with
    success := fun source stream =>
      ∀ meaning : Source → TokenStream → Prop, meaning source stream}
  ∀ source stream, (Pipeline.next lexer .done).Corresponds source stream

private def plantedCodec (profile : Profile) (passes : Passes slice0 profile) : Prop :=
  let lexer := {(registry passes).lexer with
    trust := fun _ _ => ∀ decode : ByteArray → Option UInt8, decode ByteArray.empty = none}
  (Pipeline.next lexer .done).Trusted

private structure WrappedModel where
  decode : ByteArray → Option UInt8

private def plantedWrapped : Prop := ∀ wrapped : WrappedModel, wrapped = wrapped

private def plantedDataBinder : Prop :=
  Nonempty ((decode : ByteArray → Option UInt8) → PLift (decode ByteArray.empty = none))

private def plantedCodecIdentity : Prop :=
  Nonempty (id ((decode : ByteArray → Option UInt8) → PLift (decode ByteArray.empty = none)))

private def plantedCodecPair : Prop :=
  Nonempty (Prod.fst (((decode : ByteArray → Option UInt8) → PLift (decode ByteArray.empty = none)), Nat))

private def plantedCodecOption : Prop :=
  Nonempty ((some ((decode : ByteArray → Option UInt8) → PLift (decode ByteArray.empty = none))).getD PUnit)

private def plantedCodecList : Prop :=
  Nonempty ([((decode : ByteArray → Option UInt8) → PLift (decode ByteArray.empty = none))].headD PUnit)

private def plantedModelPair : Prop :=
  Nonempty (Prod.fst (((model : Model) → PLift (model = model)), Nat))

private def plantedModelOption : Prop :=
  Nonempty ((some ((model : Model) → PLift (model = model))).getD PUnit)

private def plantedModelList : Prop :=
  Nonempty ([((model : Model) → PLift (model = model))].headD PUnit)

private inductive PlantedInductive : Prop where
  | intro (evidence : ∀ decode : ByteArray → Option UInt8, decode ByteArray.empty = none)

private inductive PlantedConstructor : Prop where
  | intro (decode : ByteArray → Option UInt8)

private structure PlantedPassBundle where
  run : Source → Except LexRefusal WrappedModel

-- Indexed type-family controls (ISS-0114).
private inductive PlantedIndexedDecoder : Nat → Type where
  | intro (decode : ByteArray → Option UInt8) : PlantedIndexedDecoder 0

private def plantedIndexedDecoder : Prop :=
  ∀ carrier : PlantedIndexedDecoder 0, carrier = carrier

private abbrev PlantedIndexedAlias (n : Nat) := PlantedIndexedDecoder n

private def plantedIndexedAlias : Prop :=
  ∀ carrier : PlantedIndexedAlias 0, carrier = carrier

private inductive PlantedModelIndex : (ByteArray → Option UInt8) → Type where
  | unit : PlantedModelIndex (fun _ => none)

private def plantedModelIndex : Prop :=
  ∀ carrier : PlantedModelIndex (fun _ => none), carrier = carrier

private inductive PlantedCoreIndex : CoreLanguage → Type where
  | unit : PlantedCoreIndex (sliceLanguage slice0)

private def plantedCoreIndex : Prop :=
  ∀ carrier : PlantedCoreIndex (sliceLanguage slice0), carrier = carrier

private inductive DataIndexedFamily.{u} (α : Type u) : Nat → Type u where
  | zero : DataIndexedFamily α 0
  | cons {n : Nat} (head : α) (tail : DataIndexedFamily α n) : DataIndexedFamily α (n + 1)

private def plantedPolyIndexedModel : Prop :=
  ∀ carrier : DataIndexedFamily CoreLanguage 1, carrier = carrier

private def plantedPolyIndexedNested : Prop :=
  ∀ carrier : DataIndexedFamily (DataIndexedFamily (ByteArray → Option UInt8) 0) 1,
    carrier = carrier

private def plantedPolyIndexedAlias : Prop :=
  ∀ carrier : DataIndexedFamily Model 0, carrier = carrier

private def plantedMatchIndexed : Bool → Type
  | true => Nat
  | false => ByteArray → Option UInt8

private def plantedMatchIndexedProp (flag : Bool) : Prop :=
  ∀ carrier : plantedMatchIndexed flag, carrier = carrier

private structure PlantedIndexedPassBundle where
  run : Source → Except LexRefusal (Option (PlantedIndexedDecoder 0))

private structure PlantedPolyIndexedPassBundle where
  run : Source → Except LexRefusal (List (DataIndexedFamily WrappedModel 1))

private structure PlantedModelIndexPassBundle where
  run : Source → Except LexRefusal (PlantedModelIndex (fun _ => none))

private structure PlantedNestedFnPassBundle where
  run : Source → Except LexRefusal (Option (Source → TokenStream))

private abbrev PassResultAlias (α : Type) := Source → Except LexRefusal α

private structure PlantedAliasPassBundle where
  run : PassResultAlias (PlantedIndexedDecoder 0)

private def passingDataIndexed : Prop :=
  ∀ carrier : DataIndexedFamily Nat 2, carrier = carrier

private def passingRegisteredIndexed (ty : (sliceLanguage slice0).operands.Ty)
    (tys : List (sliceLanguage slice0).operands.Ty) : Prop :=
  ∀ (r : Read (sliceLanguage slice0).operands.Ty (sliceLoweringLanguage slice0).literal ty)
    (args : Args (Read (sliceLanguage slice0).operands.Ty (sliceLoweringLanguage slice0).literal) tys),
    r = r ∧ args = args

private structure PassingDataIndexedPassBundle where
  run : Source → Except LexRefusal (DataIndexedFamily UInt8 3)

private structure PassingAliasPassBundle where
  run : PassResultAlias (DataIndexedFamily UInt8 3)

-- Round-2 allow-list attack controls (Codex findings 1-3).
private inductive PlantedSubtypeDecoder : Nat → Type where
  | intro (decode : { f : ByteArray → Option UInt8 // f ByteArray.empty = none }) :
      PlantedSubtypeDecoder 0

private def plantedSubtypeDecoder : Prop :=
  ∀ carrier : PlantedSubtypeDecoder 0, carrier = carrier

private structure PlantedSubtypePassBundle where
  run : Source → Except LexRefusal (PlantedSubtypeDecoder 0)

private structure HiddenDecoderCarrier where
  carrier : Type

private def hiddenDecoderCarrier : HiddenDecoderCarrier :=
  ⟨ByteArray → Option UInt8⟩

private inductive PlantedProjDecoder : Nat → Type where
  | intro (decode : hiddenDecoderCarrier.carrier) : PlantedProjDecoder 0

private def plantedProjDecoder : Prop :=
  ∀ carrier : PlantedProjDecoder 0, carrier = carrier

private structure PlantedProjPassBundle where
  run : Source → Except LexRefusal (PlantedProjDecoder 0)

private abbrev NestedDataAlias (α : Type) := DataIndexedFamily α 1

private def plantedNestedAlias : Prop :=
  ∀ carrier : NestedDataAlias (NestedDataAlias (ByteArray → Option UInt8)), carrier = carrier

private structure PlantedNestedAliasPassBundle where
  run : Source → Except LexRefusal (NestedDataAlias (NestedDataAlias (ByteArray → Option UInt8)))

private inductive PlantedWordDecoder : Nat → Type where
  | intro (decode : ByteArray → UInt64) : PlantedWordDecoder 0

private def plantedWordDecoder : Prop :=
  ∀ carrier : PlantedWordDecoder 0, carrier = carrier

private structure PlantedWordDecoderPassBundle where
  run : Source → Except LexRefusal (PlantedWordDecoder 0)

private opaque PlantedOpaqueType : Type := Nat

private def plantedOpaque : Prop :=
  ∀ carrier : PlantedOpaqueType, carrier = carrier

private structure PlantedOpaquePassBundle where
  run : Source → Except LexRefusal PlantedOpaqueType

private structure PlantedSortField where
  carrier : Type

private def plantedSortField : Prop :=
  ∀ carrier : PlantedSortField, carrier = carrier

private structure PlantedPropField where
  predicate : Prop

private def plantedPropField : Prop :=
  ∀ carrier : PlantedPropField, carrier = carrier

private def plantedBadPredicateSubtype : Prop :=
  ∀ carrier : { _n : Nat // ¬ ∃ f : ByteArray → Option UInt8, f ByteArray.empty = none },
    carrier = carrier

private def passingMatchIndexedTrue : Prop :=
  ∀ carrier : plantedMatchIndexed true, carrier = carrier

private abbrev PassResultAliasOuter (α : Type) := PassResultAlias α

private structure PassingDoubleAliasPassBundle where
  run : PassResultAliasOuter (DataIndexedFamily UInt8 3)

private def passingDataSubtype : Prop :=
  ∀ x : { n : Nat // n < 10 }, x = x

-- ISS-0131 controls: let-bound decoder existence and its negated hypothesis in `P_print`.
private def plantedPPrintLetExists (profile : Profile)
    (bir : BIR (sliceLoweringLanguage slice0) profile) (output : LLVMInput profile) : Prop :=
  let witness := ∃ decode : ByteArray → Option UInt8, decode ByteArray.empty = none
  ¬ witness → P_print (language := sliceLanguage slice0) bir output

private def plantedLetExistsPipeline (target : Concrete.Backend.LCB.Target) (slots bytes : Nat)
    (passes : Passes slice0 ⟨.llvm, target, slots, bytes⟩) : Prop :=
  let emit := {(registry passes).emit with
    trust := plantedPPrintLetExists ⟨.llvm, target, slots, bytes⟩}
  (Pipeline.next emit .done).Trusted

private def plantedFunctionConstExists : Prop :=
  ¬ (Function.const Nat Exists 0) (fun (decode : ByteArray → Option UInt8) => decode ByteArray.empty = none)

private def passingUnmodifiedPPrint (profile : Profile)
    (bir : BIR (sliceLoweringLanguage slice0) profile) (output : LLVMInput profile) : Prop :=
  P_print (language := sliceLanguage slice0) bir output

/-! ### Codex round-2 counterexample controls (ISS-0114 / ISS-0131, all 6 counterexamples) -/

-- Counterexample (1): selecting between two partially applied Bool recursors applied to two
-- copies of `ByteArray → UInt64` and `true` hides an executable decoder field.
private def selectBoolRec (b : Bool) : Type → Type → Bool → Type :=
  Bool.casesOn (motive := fun _ => Type → Type → Bool → Type)
    b (@Bool.rec (fun _ => Type)) (@Bool.rec (fun _ => Type))

private structure PlantedRecursorDecoder (b : Bool) where
  hidden : selectBoolRec b (ByteArray → UInt64) (ByteArray → UInt64) true

private def plantedRecursorProp (b : Bool) : Prop :=
  ∀ carrier : PlantedRecursorDecoder b, carrier = carrier

private def plantedRecursorOptionProp (b : Bool) : Prop :=
  ∀ carrier : Option (PlantedRecursorDecoder b), carrier = carrier

private structure PlantedRecursorPassBundle where
  run : Bool → Except LexRefusal (PlantedRecursorDecoder true)

private structure PlantedRecursorStuckPassBundle where
  run : (b : Bool) → Except LexRefusal (Option (PlantedRecursorDecoder b))

-- Counterexample (2): dependent-index family whose second index is `Nat` at `true` and
-- `ByteArray → UInt64` at `false` must fail at `false` even AFTER visiting `true`,
-- including through a recursive constructor field (with NO outer decoder binder).
private def depIdxTy : Bool → Type
  | true => Nat
  | false => ByteArray → UInt64

private inductive PlantedDepIdx : (b : Bool) → depIdxTy b → Type where
  | empty : PlantedDepIdx true (0 : Nat)

private def plantedDepIdxFalseOnly : Prop :=
  ∀ carrier : PlantedDepIdx false (fun _ => (0 : UInt64)), carrier = carrier

private def plantedDepIdxAfterTrue : Prop :=
  ∀ (_ok : PlantedDepIdx true (0 : Nat))
    (carrier : PlantedDepIdx false (fun _ => (0 : UInt64))), carrier = carrier

set_option genSizeOf false in
private inductive PlantedDepIdxRec : (b : Bool) → depIdxTy b → Type where
  | base : PlantedDepIdxRec true (0 : Nat)
  | step (inner : PlantedDepIdxRec false (fun _ => (0 : UInt64))) : PlantedDepIdxRec true (0 : Nat)

private def plantedDepIdxRecProp : Prop :=
  ∀ carrier : PlantedDepIdxRec true (0 : Nat), carrier = carrier

private def passingDepIdxTrueOnly : Prop :=
  ∀ carrier : PlantedDepIdx true (0 : Nat), carrier = carrier

-- Counterexample (3): named `Fin` width returning 0 iff some byte decoder returns 0 on the bytes.
private noncomputable def badDecoderWidth (bs : ByteArray) : Nat :=
  open Classical in
  if ∃ decode : ByteArray → UInt64, decode bs = 0 then 0 else 1

private structure PlantedFinWidth (bs : ByteArray) where
  slot : Fin (badDecoderWidth bs)

private def plantedFinWidthProp (bs : ByteArray) : Prop :=
  ∀ carrier : PlantedFinWidth bs, carrier = carrier

private structure PlantedFinWidthPassBundle where
  run : ByteArray → Except LexRefusal (PlantedFinWidth ByteArray.empty)

private def goodDataWidth (bs : ByteArray) : Nat := bs.size + 1

private structure PassingFinWidth (bs : ByteArray) where
  slot : Fin (goodDataWidth bs)

private def passingFinWidthProp (bs : ByteArray) : Prop :=
  ∀ carrier : PassingFinWidth bs, carrier = carrier

-- Counterexample (4) (ISS-0131): three equivalent false printer guards using
-- `W := ∃ decode : ByteArray → UInt64, decode out.packageBytes.component = 0`.
-- 4a: `¬ (∃ _ : W, True)` (Prop-typed existential domain).
private def plantedPPrintNegExistsProp (profile : Profile)
    (bir : BIR (sliceLoweringLanguage slice0) profile) (output : LLVMInput profile) : Prop :=
  let W := ∃ decode : ByteArray → UInt64, decode output.packageBytes.component = 0
  ¬ (∃ _ : W, True) → P_print (language := sliceLanguage slice0) bir output

-- 4b: Bool match on `decide W` (matcher discriminant carrying a classical decision of `W`).
private noncomputable def plantedPPrintMatchDecide (profile : Profile)
    (bir : BIR (sliceLoweringLanguage slice0) profile) (output : LLVMInput profile) : Prop :=
  open Classical in
  let W := ∃ decode : ByteArray → UInt64, decode output.packageBytes.component = 0
  (match decide W with
   | true => False
   | false => True) → P_print (language := sliceLanguage slice0) bir output

-- 4c: Function-typed equality `(fun d => d bs) = (fun _ => 0)`.
private def plantedPPrintFnEq (profile : Profile)
    (bir : BIR (sliceLoweringLanguage slice0) profile) (output : LLVMInput profile) : Prop :=
  ((fun d : ByteArray → UInt64 => d output.packageBytes.component) =
    (fun _ : ByteArray → UInt64 => (0 : UInt64))) →
    P_print (language := sliceLanguage slice0) bir output

-- Counterexample (5): locally declared predicate under `namespace LeanCompilerBackend`
-- and a custom `LT` instance implementing the false printer guard.
end EndToEndAudit

namespace LeanCompilerBackend
private def PlantedLocalGuard (bs : ByteArray) : Prop :=
  ¬ ∃ decode : ByteArray → UInt64, decode bs = 0
end LeanCompilerBackend

namespace EndToEndAudit
open Concrete.Interfaces ConcreteSpec.StageInterface ConcreteSpec.StageGrammar
open Lean Meta Elab Command

private def plantedPPrintLocalLCBNamespace (profile : Profile)
    (bir : BIR (sliceLoweringLanguage slice0) profile) (output : LLVMInput profile) : Prop :=
  LeanCompilerBackend.PlantedLocalGuard output.packageBytes.component →
    P_print (language := sliceLanguage slice0) bir output

@[instance_reducible] private def evilLTInstance (bs : ByteArray) : LT Unit :=
  ⟨fun _ _ => ¬ ∃ decode : ByteArray → UInt64, decode bs = 0⟩

private def plantedPPrintCustomLT (profile : Profile)
    (bir : BIR (sliceLoweringLanguage slice0) profile) (output : LLVMInput profile) : Prop :=
  @LT.lt Unit (evilLTInstance output.packageBytes.component) () () →
    P_print (language := sliceLanguage slice0) bir output

-- Counterexample (6): inductive validation predicate with a decoder added to its `.last` constructor.
private inductive PlantedValidated : {A B : Type} → Pipeline A B → Prop where
  | done {A : Type} : PlantedValidated (.done : Pipeline A A)
  | last {A R B : Type} {edge : Stage A R B} (decode : ByteArray → UInt64) :
      edge.Validated → PlantedValidated (.next edge .done)
  | next {A R B S C D : Type} {edge : Stage A R B} {after : Stage B S C}
      {rest : Pipeline C D} : edge.Validated → edge.Compatible after →
      PlantedValidated (.next after rest) → PlantedValidated (.next edge (.next after rest))

private def plantedValidatedLastDecoder {slice : Slice} {profile : Profile}
    (passes : Passes slice profile) (route : Route) : Prop :=
  PlantedValidated (Registry.pipeline passes route).pipeline

-- Exact boundary theorems exercised against the frozen `RoutePipeline` API.
example {slice : Slice} {profile : Profile} (passes : Passes slice profile) (route : Route)
    {input : Source} {output : BackendOutput profile}
    (validated : (Registry.pipeline passes route).Validated)
    (trusted : (Registry.pipeline passes route).Trusted)
    (ready : (Registry.pipeline passes route).Ready input)
    (ran : (Registry.pipeline passes route).run input = .ok output) :
    (Registry.pipeline passes route).Corresponds input output :=
  @Pipeline.endToEnd profile slice passes route input output validated trusted ready ran

example {slice : Slice} {profile : Profile} (passes : Passes slice profile) (route : Route)
    (hLCB : profile.backend = .lcb)
    {input : Source} {output : BackendOutput profile}
    (validated : (Registry.pipeline passes route).Validated)
    (ready : (Registry.pipeline passes route).Ready input)
    (ran : (Registry.pipeline passes route).run input = .ok output) :
    (Registry.pipeline passes route).Corresponds input output :=
  @registeredLCBEndToEnd slice profile passes route hLCB input output validated ready ran

run_cmd liftTermElabM do
  let env ← getEnv
  -- 1. Frozen `PassFunctions` schema: exact 10 fields including single `emit` and `passesAt`.
  let fields := #[`lex, `elaborate, `checkCore, `lowerCore, `lowerUnchecked, `lowerMemory,
    `buildSSA, `buildBIR, `emit, `passesAt]
  unless (getStructureInfo? env ``PassFunctions).map (·.fieldNames) == some fields do
    throwError "The pass argument must contain only the registered executable functions"
  -- 2. `RoutePipeline` and `Registry` must keep private constructors.
  for privStruct in [``RoutePipeline, ``Registry] do
    let some (.inductInfo sinfo) := env.find? privStruct
      | throwError "Missing frozen structure {privStruct}"
    unless sinfo.ctors.length == 1 && isPrivateName sinfo.ctors[0]! do
      throwError "Frozen structure {privStruct} must have a single private constructor"
  -- 3. Import-closure hygiene on the 22 frozen files:
  -- zero transitive imports of provisional `Concrete.BIR` (or other provisional compiler passes),
  -- and zero `ConcreteSpec.Spec.*` imports of `ConcreteSpec.Proofs.*`.
  let forbiddenProvisional : List Name := [
    `Concrete.BIR, `Concrete.BIR.Progress, `Concrete.SSA, `Concrete.MIR,
    `Concrete.Elab, `Concrete.Core, `Concrete.LL1,
    `Concrete.Backend.ProvisionalRuntime, `Concrete.Backend.ProvisionalObservation,
    `Concrete.Backend.LLVM, `Concrete.Backend.LCB
  ]
  for modName in env.header.moduleNames do
    if forbiddenProvisional.contains modName then
      throwError "Frozen interface closure transitively imports provisional module {modName}"
  for i in [:env.header.moduleNames.size] do
    let modName := env.header.moduleNames[i]!
    if (`ConcreteSpec.Spec).isPrefixOf modName then
      if let some mdata := env.header.moduleData[i]? then
        for imp in mdata.imports do
          if (`ConcreteSpec.Proofs).isPrefixOf imp.module then
            throwError "Specification module {modName} imports proof module {imp.module}"
  -- 4. Semantic-parameter audit of registered passes, end-to-end theorems, `Stage.Sound`,
  -- and every planted + passing control (including all 6 Codex round-2 counterexamples).
  let (_, _) ← (do
    if let some (field, reason) ← badRegisteredPasses then
      throwError "The registered pass field {field} contains an open semantic model: {reason}"
    for name in [``Pipeline.endToEnd, ``registeredLCBEndToEnd] do
      if let some (origin, binder) ← checkDecl name then
        throwError "End-to-end statement has an open semantic binder {binder} in {origin}"
    if let some (origin, binder) ← checkDecl ``Stage.Sound then
      throwError "Stage soundness has an open semantic binder {binder} in {origin}"
    if let some (origin, binder) ← checkDecl ``PassPost then
      throwError "PassPost has an open semantic binder {binder} in {origin}"
    -- Existing wrapper controls.
    for (name, binder) in [(``plantedDataBinder, `decode),
        (``plantedCodecIdentity, `decode),
        (``plantedCodecPair, `decode),
        (``plantedCodecOption, `decode),
        (``plantedCodecList, `decode),
        (``plantedModelPair, `model),
        (``plantedModelOption, `model),
        (``plantedModelList, `model)] do
      unless (← checkDecl name) == some (privateToUserName name, binder) do
        throwError "End-to-end audit accepted a caller-selected binder in {name}"
    unless (← checkDecl ``plantedStatement) == some (`EndToEndAudit.plantedStatement, `model) do
      throwError "End-to-end audit accepted a planted semantic model parameter"
    unless (← checkDecl ``plantedRelation) == some (`EndToEndAudit.plantedRelation, `meaning) do
      throwError "End-to-end audit accepted a planted relation parameter"
    unless (← checkDecl ``plantedCodec) == some (`EndToEndAudit.plantedCodec, `decode) do
      throwError "End-to-end audit accepted a planted codec parameter"
    unless (← checkDecl ``plantedWrapped) == some (`EndToEndAudit.plantedWrapped, `wrapped) do
      throwError "End-to-end audit accepted a planted wrapped decoder"
    unless (← checkDecl ``PlantedInductive) == some (`EndToEndAudit.PlantedInductive, `decode) do
      throwError "End-to-end audit accepted a planted decoder under an inductive constructor"
    unless (← checkDecl ``PlantedConstructor) == some (`EndToEndAudit.PlantedConstructor, `decode) do
      throwError "End-to-end audit accepted a planted decoder as an inductive constructor field"
    unless (← badPassField ``PlantedPassBundle) == some `run do
      throwError "End-to-end audit accepted a planted decoder in the executable pass bundle"
    -- Indexed type-family and allow-list attack controls (ISS-0114).
    for (name, binder) in [(``plantedIndexedDecoder, `carrier),
        (``plantedIndexedAlias, `carrier),
        (``plantedModelIndex, `carrier),
        (``plantedCoreIndex, `carrier),
        (``plantedPolyIndexedModel, `carrier),
        (``plantedPolyIndexedNested, `carrier),
        (``plantedPolyIndexedAlias, `carrier),
        (``plantedMatchIndexedProp, `carrier),
        (``plantedSubtypeDecoder, `carrier),
        (``plantedProjDecoder, `carrier),
        (``plantedNestedAlias, `carrier),
        (``plantedWordDecoder, `carrier),
        (``plantedOpaque, `carrier),
        (``plantedSortField, `carrier),
        (``plantedPropField, `carrier),
        (``plantedBadPredicateSubtype, `carrier)] do
      unless (← checkDecl name) == some (privateToUserName name, binder) do
        throwError "End-to-end audit accepted a planted non-data carrier in {name}"
    for bundle in [``PlantedIndexedPassBundle, ``PlantedPolyIndexedPassBundle,
        ``PlantedModelIndexPassBundle, ``PlantedNestedFnPassBundle, ``PlantedAliasPassBundle,
        ``PlantedSubtypePassBundle, ``PlantedProjPassBundle, ``PlantedNestedAliasPassBundle,
        ``PlantedWordDecoderPassBundle, ``PlantedOpaquePassBundle] do
      unless (← badPassField bundle) == some `run do
        throwError "End-to-end audit accepted a planted non-data carrier in executable pass bundle {bundle}"
    -- ISS-0131 let-bound and Function.const controls.
    unless (← checkDecl ``plantedPPrintLetExists) == some (`EndToEndAudit.plantedPPrintLetExists, `decode) do
      throwError "End-to-end audit accepted a let-bound negated existential in plantedPPrintLetExists"
    unless (← checkDecl ``plantedLetExistsPipeline) == some (`EndToEndAudit.plantedPPrintLetExists, `decode) do
      throwError "End-to-end audit accepted a let-bound negated existential through Pipeline.Trusted"
    unless (← checkDecl ``plantedFunctionConstExists) == some (`EndToEndAudit.plantedFunctionConstExists, `decode) do
      throwError "End-to-end audit accepted Exists wrapped behind Function.const"
    -- Counterexample (1): partially applied Bool recursor hiding a decoder (bare, Option, pass bundles).
    unless (← checkDecl ``plantedRecursorProp) == some (`EndToEndAudit.plantedRecursorProp, `carrier) do
      throwError "Counterexample 1: audit accepted partially-applied recursor decoder carrier"
    unless (← checkDecl ``plantedRecursorOptionProp) == some (`EndToEndAudit.plantedRecursorOptionProp, `carrier) do
      throwError "Counterexample 1: audit accepted Option-wrapped recursor decoder carrier"
    for bundle in [``PlantedRecursorPassBundle, ``PlantedRecursorStuckPassBundle] do
      unless (← badPassField bundle) == some `run do
        throwError "Counterexample 1: audit accepted recursor decoder in pass bundle {bundle}"
    -- Counterexample (2): dependent-index memoization visit-order independence.
    if let some (origin, binder) ← checkDecl ``passingDepIdxTrueOnly then
      throwError "Counterexample 2: audit rejected passing true-indexed family ({binder} in {origin})"
    -- Even with `PlantedDepIdx true 0` ALREADY cached in `AuditState`, `false` MUST still fail!
    unless (← checkDecl ``plantedDepIdxFalseOnly) == some (`EndToEndAudit.plantedDepIdxFalseOnly, `carrier) do
      throwError "Counterexample 2: audit accepted false-indexed family after caching true index"
    unless (← checkDecl ``plantedDepIdxAfterTrue) == some (`EndToEndAudit.plantedDepIdxAfterTrue, `carrier) do
      throwError "Counterexample 2: audit accepted false-indexed family following true in same binder list"
    unless (← checkDecl ``plantedDepIdxRecProp) == some (`EndToEndAudit.plantedDepIdxRecProp, `carrier) do
      throwError "Counterexample 2: audit accepted recursive constructor referencing bad dependent index"
    -- Counterexample (3): named Fin width hiding a classical decoder premise.
    unless (← checkDecl ``plantedFinWidthProp) == some (`EndToEndAudit.plantedFinWidthProp, `carrier) do
      throwError "Counterexample 3: audit accepted Fin with named decoder-dependent width"
    unless (← badPassField ``PlantedFinWidthPassBundle) == some `run do
      throwError "Counterexample 3: audit accepted Fin with named decoder-dependent width in pass bundle"
    if let some (origin, binder) ← checkDecl ``passingFinWidthProp then
      throwError "Counterexample 3: audit rejected genuine data-only named Fin width ({binder} in {origin})"
    -- Counterexample (4) (ISS-0131): three equivalent false printer guards.
    unless (← checkDecl ``plantedPPrintNegExistsProp) == some (`EndToEndAudit.plantedPPrintNegExistsProp, `decode) do
      throwError "Counterexample 4a: audit accepted ¬ (∃ _ : W, True) printer guard"
    unless (← checkDecl ``plantedPPrintMatchDecide) == some (`EndToEndAudit.plantedPPrintMatchDecide, `decode) do
      throwError "Counterexample 4b: audit accepted match (decide W) printer guard"
    unless (← checkDecl ``plantedPPrintFnEq) == some (`EndToEndAudit.plantedPPrintFnEq, `eqType) do
      throwError "Counterexample 4c: audit accepted function-typed decoder equality printer guard"
    -- Counterexample (5): local predicate in `LeanCompilerBackend` namespace & custom `LT` instance.
    unless (← checkDecl ``plantedPPrintLocalLCBNamespace) == some (`LeanCompilerBackend.PlantedLocalGuard, `decode) do
      throwError "Counterexample 5a: audit trusted locally declared LeanCompilerBackend.* predicate"
    unless (← checkDecl ``plantedPPrintCustomLT) == some (`EndToEndAudit.plantedPPrintCustomLT, `decode) do
      throwError "Counterexample 5b: audit accepted custom LT instance hiding decoder guard"
    -- Counterexample (6): added decoder argument on `.last` constructor of validation inductive.
    unless (← checkDecl ``plantedValidatedLastDecoder) == some (`EndToEndAudit.PlantedValidated, `decode) do
      throwError "Counterexample 6: audit missed decoder argument added to validation `.last` constructor"
    -- Genuine passing controls.
    for passing in [``passingDataIndexed, ``passingRegisteredIndexed,
        ``passingMatchIndexedTrue, ``passingDataSubtype, ``passingUnmodifiedPPrint] do
      if let some (origin, binder) ← checkDecl passing then
        throwError "End-to-end audit rejected genuine data-only control {passing} ({binder} in {origin})"
    for bundle in [``PassingDataIndexedPassBundle, ``PassingAliasPassBundle,
        ``PassingDoubleAliasPassBundle] do
      if let some field ← badPassField bundle then
        throwError "End-to-end audit rejected genuine data-only pass bundle {bundle} at field {field}"
  ).run {}

end EndToEndAudit
