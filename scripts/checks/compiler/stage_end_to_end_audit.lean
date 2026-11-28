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

/-- Namespaces whose inductive/structure definitions may be inspected recursively as data. -/
private def isRegisteredDataNamespace (name : Name) : Bool :=
  let root := privateToUserName name
  [`Concrete, `ConcreteSpec, `LeanCompilerBackend, `EndToEndAudit].any (·.isPrefixOf root)

/-- Namespaces whose proposition definitions and inductive predicates are audited recursively. -/
private def isAuditPropNamespace (name : Name) : Bool :=
  let root := privateToUserName name
  [`Concrete, `ConcreteSpec, `EndToEndAudit].any (·.isPrefixOf root)

/-- Pipeline/stage combinators preserve their caller's attribution origin when unfolded. -/
private def isCombinatorOrigin (name : Name) : Bool :=
  [``Pipeline.Trusted, ``Pipeline.Validated, ``Pipeline.Ready, ``Pipeline.Corresponds,
    ``Pipeline.Sound, ``Stage.Trusted, ``Stage.Validated, ``Stage.Checks, ``Stage.Compatible,
    ``Stage.trust, ``Stage.success, ``Stage.refusal, ``Stage.inputInvariant,
    ``Stage.outputInvariant].contains name

/-- Explicit allow-list of atomic data-level predicates whose definitions need not be
unfolded further (their type arguments are still checked by the data allow-list, their
predicate arguments are recursively audited, and their value arguments are scanned for
embedded `Prop`s). Any other un-unfoldable `Prop` constant fails closed. -/
private def isAllowedAtomicProp (name : Name) : Bool :=
  [``LT.lt, ``LE.le, ``GT.gt, ``GE.ge, ``Nat.le, ``Nat.lt, ``Int.le, ``Int.lt,
    ``Dvd.dvd, ``Membership.mem, ``List.Mem, ``List.Nodup, ``List.Pairwise,
    ``HasSubset.Subset].contains name ||
  (`LeanCompilerBackend).isPrefixOf (privateToUserName name)

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
  /-- Verified data inductives keyed by `(declName, canonicalizedParams)`.
  Abbreviations and `def`s are never memoized here. -/
  verifiedData : Std.HashSet (Name × Array Expr) := {}
  /-- Verified `Prop`-valued inductives keyed by `(declName, pos, canonicalizedParams)`. -/
  verifiedPropInducts : Std.HashSet (Name × Bool × Array Expr) := {}
  /-- Verified non-combinator `Prop` definitions keyed by `(declName, pos, canonicalizedArgs)`. -/
  verifiedPropDefs : Std.HashSet (Name × Bool × Array Expr) := {}

private abbrev AuditM := StateT AuditState MetaM

private def resultIsPropExpr (e : Expr) : MetaM Bool :=
  forallTelescope e fun _ res => do
    let resW ← whnf res
    return resW == .sort .zero

/-- Construct the constant `True` predicate of a `Prop`-returning telescope type
`d₁ → ... → dₖ → Prop`, used as the base inductive-hypothesis placeholder when
auditing the step function of a `Prop`-valued `List.foldr`. -/
private def mkTruePred (ty : Expr) : MetaM Expr :=
  forallTelescope ty fun xs _ => mkLambdaFVars xs (mkConst ``True)

mutual
/-- Allow-list classifier for data-only types.
Returns `none` iff `type` (after `instantiateMVars` + `whnf` reducing abbreviations,
definitions, `let`s, beta-redexes, closed projections, and known matches) is built
solely from:
- explicit data primitives (`Nat`, `Int`, `Bool`, `UInt8`-`UInt64`, `Char`, `String`,
  `ByteArray`, `Fin n`, `BitVec n`, `Unit`/`PUnit`, `Empty`/`PEmpty`, `Lean.Name`),
- explicit data containers (`List`, `Array`, `Option`, `Prod`, `Sum`, `Except`, `Sigma`),
- `Subtype` over an allowed data carrier with an audited predicate,
- registered repository inductives/structures whose type parameters, index domains,
  and instantiated constructor fields are all recursively allowed (with recursion
  guarded by `(inductName, canonicalizedParams)` and never by abbreviation names).

Anything else (arrows/pis, `Sort`-valued or `Prop`-carrying fields, subtypes over
non-data, opaque constants, unregistered inductives, stuck non-data expressions) is
rejected with a finding. -/
private partial def rejectNonDataType (type : Expr)
    (inProgress : List (Name × Array Expr) := [])
    (fuel : Nat := 64) (allowBehavior := false) : AuditM (Option String) := do
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
  | .letE name t v b nondep =>
    withLetDecl name t v (nondep := nondep) fun fv =>
      rejectNonDataTypeCore (b.instantiate1 fv) inProgress (fuel - 1) allowBehavior
  | .forallE .. =>
    return some s!"disallowed arrow/pi type {← ppExpr type}"
  | .sort .. =>
    return some s!"disallowed Sort-valued type {← ppExpr type}"
  | _ =>
    if let some m ← matchMatcherApp? type then
      for discr in m.discrs do
        let discrTy ← inferType discr
        if !(← isProp discrTy) then
          if let some err ← rejectNonDataTypeCore discrTy inProgress (fuel - 1) allowBehavior then
            return some s!"matcher discriminant type: {err}"
      for i in [:m.alts.size] do
        let alt := m.alts[i]!
        let nParams := m.altNumParams[i]!
        let err ← lambdaBoundedTelescope alt nParams fun _ body =>
          rejectNonDataTypeCore body inProgress (fuel - 1) allowBehavior
        if let some err := err then
          return some s!"matcher branch {i}: {err}"
      return none
    let fn := type.getAppFn
    let args := type.getAppArgs
    if fn.isConstOf ``ite || fn.isConstOf ``dite then
      if args.size ≥ 5 then
        let c := args[1]!
        if let some (_, bname) ← auditPropExpr `ite c false inProgress [] (fuel - 1) then
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
          if let some (_, bname) ← scanEmbeddedProps name arg inProgress [] (fuel - 1) then
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
          auditPropExpr `Subtype (mkApp p x) false inProgress [] (fuel - 1)
        return propErr.map fun (_, bname) => s!"subtype predicate binder {bname}"
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
        let numP := Nat.min info.numParams args.size
        let mut normParams : Array Expr := #[]
        for i in [:numP] do
          let p ← instantiateMVars args[i]!
          let pTy ← inferType p
          let pNorm ← if (← whnf pTy).isSort then whnf p else pure p
          normParams := normParams.push pNorm
        let key? ← match ← canonicalizeKey? normParams with
          | some cp => pure (some (name, cp))
          | none => pure none
        if let some key := key? then
          if inProgress.contains key then return none
          if (← get).verifiedData.contains key then return none
        let indType := info.type.instantiateLevelParams info.levelParams lvls
        -- Check all type-valued parameters, type-family parameters, and index domains.
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
          for i in [info.numParams:xs.size] do
            let idxTy := applySubst (← inferType xs[i]!)
            if !(← isProp idxTy) then
              if let some err ← rejectNonDataTypeCore idxTy inProgress (fuel - 1) false then
                return some s!"index domain {i} of {name}: {err}"
          return none
        if let some err := paramIdxErr then return some err
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
                  if let some (_, bname) ← auditPropExpr ctor fTy false inProgress' [] (fuel - 1) then
                    return some s!"proof field of {ctor} binder {bname}"
                else
                  if let some err ← rejectNonDataTypeCore fTy inProgress' (fuel - 1) false then
                    return some s!"field of {ctor}: {err}"
              return none
          if let some err := ctorErr then return some err
        if let some key := key? then
          if inProgress.isEmpty then
            modify fun st => { st with verifiedData := st.verifiedData.insert key }
          else
            -- Safe because `rejectNonDataType` rolls back `AuditState` on any failure.
            modify fun st => { st with verifiedData := st.verifiedData.insert key }
        return none
      | some (.recInfo info) =>
        let minorStart := info.numParams + info.numMotives
        for offset in [:info.numMinors] do
          let idx := minorStart + offset
          if h : idx < args.size then
            let err ← lambdaTelescope args[idx] fun _ body =>
              rejectNonDataTypeCore body inProgress (fuel - 1) allowBehavior
            if let some err := err then
              return some s!"recursor {name} minor {offset}: {err}"
        return none
      | _ =>
        return some s!"disallowed constant {name}"
    | _ =>
      return some s!"disallowed non-constant type {← ppExpr type}"

/-- Scan term arguments of equalities and indices for embedded `Prop` expressions
(such as `@decide P inst` or `@ite _ P inst _ _`) so non-data quantifiers cannot hide
inside classical decision procedures. -/
private partial def scanEmbeddedProps (origin : Name) (e : Expr)
    (inProgressData : List (Name × Array Expr))
    (inProgressProps : List (Name × Bool × Array Expr)) (fuel : Nat) :
    AuditM (Option (Name × Name)) := do
  if fuel == 0 then return none
  let fn := e.getAppFn
  let args := e.getAppArgs
  if fn.isConstOf ``decide && args.size ≥ 1 then
    if let some hit ← auditPropExpr origin args[0]! false inProgressData inProgressProps (fuel - 1) then
      return some hit
  if (fn.isConstOf ``ite || fn.isConstOf ``dite) && args.size ≥ 2 then
    if let some hit ← auditPropExpr origin args[1]! false inProgressData inProgressProps (fuel - 1) then
      return some hit
  match e with
  | .app f a =>
    if let some hit ← scanEmbeddedProps origin f inProgressData inProgressProps (fuel - 1) then
      return some hit
    scanEmbeddedProps origin a inProgressData inProgressProps (fuel - 1)
  | .lam name dom body bi =>
    if let some hit ← scanEmbeddedProps origin dom inProgressData inProgressProps (fuel - 1) then
      return some hit
    withLocalDecl name bi dom fun fv =>
      scanEmbeddedProps origin (body.instantiate1 fv) inProgressData inProgressProps (fuel - 1)
  | .letE name t v b nondep =>
    if let some hit ← scanEmbeddedProps origin t inProgressData inProgressProps (fuel - 1) then
      return some hit
    if let some hit ← scanEmbeddedProps origin v inProgressData inProgressProps (fuel - 1) then
      return some hit
    withLetDecl name t v (nondep := nondep) fun fv =>
      scanEmbeddedProps origin (b.instantiate1 fv) inProgressData inProgressProps (fuel - 1)
  | .mdata _ body | .proj _ _ body =>
    scanEmbeddedProps origin body inProgressData inProgressProps (fuel - 1)
  | _ => return none

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
quantified binder (`∀`, `∃`, `Nonempty`, inductive predicate constructor fields, and
embedded `decide`/`ite` conditions). Uses `whnfCore` + `withLetDecl` so let-bound
propositions (ISS-0131), projections, and aliases are reduced before classification. -/
private partial def auditPropExpr (origin : Name) (expr : Expr) (pos : Bool := true)
    (inProgressData : List (Name × Array Expr) := [])
    (inProgressProps : List (Name × Bool × Array Expr) := [])
    (fuel : Nat := 256) : AuditM (Option (Name × Name)) := do
  let savedState ← get
  let res ← auditPropExprCore origin expr pos inProgressData inProgressProps fuel
  if res.isSome then
    set savedState
  return res

private partial def auditPropExprCore (origin : Name) (expr : Expr) (pos : Bool)
    (inProgressData : List (Name × Array Expr))
    (inProgressProps : List (Name × Bool × Array Expr))
    (fuel : Nat) : AuditM (Option (Name × Name)) := do
  if fuel == 0 then return some (origin, `depthExceeded)
  let expr ← instantiateMVars expr
  let expr ← whnfCore expr
  match expr with
  | .mdata _ body =>
    auditPropExprCore origin body pos inProgressData inProgressProps (fuel - 1)
  | .letE name t v b nondep =>
    -- If a local `let` binds a `Prop` value (ISS-0131), audit it directly as well as
    -- zeta-reducing through `withLetDecl` in the body.
    if (← whnf t) == .sort .zero then
      if let some hit ← auditPropExprCore origin v false inProgressData inProgressProps (fuel - 1) then
        return some hit
    withLetDecl name t v (nondep := nondep) fun fv =>
      auditPropExprCore origin (b.instantiate1 fv) pos inProgressData inProgressProps (fuel - 1)
  | .forallE name dom body bi =>
    if ← isProp dom then
      if let some hit ← auditPropExprCore origin dom (!pos) inProgressData inProgressProps (fuel - 1) then
        return some hit
      withLocalDecl name bi dom fun fv =>
        auditPropExprCore origin (body.instantiate1 fv) pos inProgressData inProgressProps (fuel - 1)
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
      if !isTraceRun then
        if let some _ ← rejectNonDataTypeCore dom inProgressData (fuel - 1) allowBeh then
          return some (origin, name)
      withLocalDecl name bi dom fun fv =>
        auditPropExprCore origin (body.instantiate1 fv) pos inProgressData inProgressProps (fuel - 1)
  | _ =>
    if let some m ← matchMatcherApp? expr then
      for i in [:m.alts.size] do
        let alt := m.alts[i]!
        let nParams := m.altNumParams[i]!
        let hit ← lambdaBoundedTelescope alt nParams fun _ body =>
          auditPropExprCore origin body pos inProgressData inProgressProps (fuel - 1)
        if let some hit := hit then return some hit
      return none
    let fn := expr.getAppFn
    let args := expr.getAppArgs
    if fn.isConstOf ``ite || fn.isConstOf ``dite then
      if args.size ≥ 5 then
        let c := args[1]!
        if let some hit ← auditPropExprCore origin c false inProgressData inProgressProps (fuel - 1) then
          return some hit
        let extra := args[5:]
        if fn.isConstOf ``dite then
          let h1 ← withLocalDeclD `_h c fun h =>
            auditPropExprCore origin (mkAppN (mkApp args[3]! h) extra) pos inProgressData inProgressProps (fuel - 1)
          if let some hit := h1 then return some hit
          return ← withLocalDeclD `_h (mkApp (mkConst ``Not) c) fun h =>
            auditPropExprCore origin (mkAppN (mkApp args[4]! h) extra) pos inProgressData inProgressProps (fuel - 1)
        else
          if let some hit ← auditPropExprCore origin (mkAppN args[3]! extra) pos inProgressData inProgressProps (fuel - 1) then
            return some hit
          return ← auditPropExprCore origin (mkAppN args[4]! extra) pos inProgressData inProgressProps (fuel - 1)
    if fn.isConstOf ``And || fn.isConstOf ``Or then
      for arg in args do
        if let some hit ← auditPropExprCore origin arg pos inProgressData inProgressProps (fuel - 1) then
          return some hit
      return none
    if fn.isConstOf ``Iff then
      for arg in args do
        if let some hit ← auditPropExprCore origin arg false inProgressData inProgressProps (fuel - 1) then
          return some hit
      return none
    if fn.isConstOf ``True || fn.isConstOf ``False then
      return none
    if fn.isConstOf ``Exists then
      if args.size ≥ 2 then
        let α := args[0]!
        let p := args[1]!
        let bname := match p with | .lam n .. => n | _ => `witness
        if !(← isProp α) then
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
          if !isTraceRun && !isMemorySim then
            if let some _ ← rejectNonDataTypeCore α inProgressData (fuel - 1) false then
              return some (origin, bname)
        return ← withLocalDeclD bname α fun fv =>
          auditPropExprCore origin (mkApp p fv) pos inProgressData inProgressProps (fuel - 1)
      return none
    if fn.isConstOf ``Nonempty then
      if args.size ≥ 1 then
        let α ← whnf args[0]!
        if let .forallE name dom _ _ := α then
          if let some _ ← rejectNonDataTypeCore dom inProgressData (fuel - 1) false then
            return some (origin, name)
        if let some _ ← rejectNonDataTypeCore α inProgressData (fuel - 1) false then
          return some (origin, `witness)
      return none
    if fn.isConstOf ``Eq then
      if args.size ≥ 3 then
        if (← whnf args[0]!).isProp then
          if let some hit ← auditPropExprCore origin args[1]! false inProgressData inProgressProps (fuel - 1) then
            return some hit
          return ← auditPropExprCore origin args[2]! false inProgressData inProgressProps (fuel - 1)
        if let some hit ← scanEmbeddedProps origin args[1]! inProgressData inProgressProps (fuel - 1) then
          return some hit
        return ← scanEmbeddedProps origin args[2]! inProgressData inProgressProps (fuel - 1)
      return none
    if fn.isConstOf ``List.foldr && args.size ≥ 5 then
      -- `List.foldr (α := α) (β := β) f init xs extra...` computing a `Prop`:
      -- check `α` and `β`'s argument domains are data, audit `init`, audit `f a ih`
      -- with `ih := fun .. => True`, and scan `xs` and `extra` for embedded `Prop`s.
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
      if let some hit ← scanEmbeddedProps origin xsVal inProgressData inProgressProps (fuel - 1) then
        return some hit
      for ex in extra do
        if let some hit ← scanEmbeddedProps origin ex inProgressData inProgressProps (fuel - 1) then
          return some hit
      let initHit ← forallTelescope β fun bxs _ =>
        auditPropExprCore origin (mkAppN initVal bxs) pos inProgressData inProgressProps (fuel - 1)
      if let some hit := initHit then return some hit
      let ihDummy ← mkTruePred β
      return ← withLocalDeclD `_elem α fun elem =>
        forallTelescope β fun bxs _ =>
          auditPropExprCore origin (mkAppN (mkApp2 stepFn elem ihDummy) bxs)
            pos inProgressData inProgressProps (fuel - 1)
    if fn.isConstOf ``Pipeline.Validated then
      if args.size ≥ 3 then
        let pipe ← whnf args[2]!
        let pfn := pipe.getAppFn
        let pargs := pipe.getAppArgs
        if pfn.isConstOf ``Pipeline.done then return none
        if pfn.isConstOf ``Pipeline.next && pargs.size ≥ 6 then
          let a := pargs[0]!; let r := pargs[1]!; let b := pargs[2]!; let c := pargs[3]!
          let edge := pargs[4]!; let rest := pargs[5]!
          let edgeVal := mkAppN (mkConst ``Stage.Validated) #[a, r, b, edge]
          if let some hit ← auditPropExprCore origin edgeVal pos inProgressData inProgressProps (fuel - 1) then
            return some hit
          let restW ← whnf rest
          let rfn := restW.getAppFn
          let rargs := restW.getAppArgs
          if rfn.isConstOf ``Pipeline.done then return none
          if rfn.isConstOf ``Pipeline.next && rargs.size ≥ 6 then
            let s := rargs[1]!; let c' := rargs[2]!; let after := rargs[4]!
            let compat := mkAppN (mkConst ``Stage.Compatible) #[a, r, b, s, c', edge, after]
            if let some hit ← auditPropExprCore origin compat pos inProgressData inProgressProps (fuel - 1) then
              return some hit
            let restVal := mkAppN (mkConst ``Pipeline.Validated) #[b, c, rest]
            return ← auditPropExprCore origin restVal pos inProgressData inProgressProps (fuel - 1)
          return some (origin, `pipeline)
        return some (origin, `pipeline)
    if let .const name lvls := fn then
      let root := privateToUserName name
      let env ← getEnv
      match env.find? name with
      | some (.opaqueInfo _) =>
        -- The sole permitted opaque predicate is the external LLVM behavior boundary in `P_print`.
        if name == ``Concrete.Interfaces.llvmBehaviors && origin == ``Concrete.Interfaces.P_print &&
            args.size ≥ 1 then
          if let some _ ← rejectNonDataTypeCore args[0]! inProgressData (fuel - 1) false then
            return some (origin, `llvmBehaviors)
          return none
        return some (origin, root)
      | some (.axiomInfo _) =>
        return some (origin, root)
      | some (.defnInfo _) =>
        if isAllowedAtomicProp name then
          for arg in args do
            let argTy ← whnf (← inferType arg)
            if argTy.isSort then
              if argTy != .sort .zero then
                let okProd ←
                  if origin == ``ConcreteSpec.StageGrammar.Derives then
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
                auditPropExprCore origin (mkAppN arg fxs) false inProgressData inProgressProps (fuel - 1)
              if let some hit := predHit then return some hit
            else
              if let some hit ← scanEmbeddedProps origin arg inProgressData inProgressProps (fuel - 1) then
                return some hit
          return none
        let canCache := isAuditPropNamespace name && !isCombinatorOrigin root
        let cacheKey? ← if canCache then do
          match ← canonicalizeKey? args with
          | some cArgs => pure (some (name, pos, cArgs))
          | none => pure none
        else pure none
        if let some ck := cacheKey? then
          if (← get).verifiedPropDefs.contains ck then return none
        let nextOrigin :=
          if isAuditPropNamespace name && !isCombinatorOrigin root then root else origin
        let hit? ← match ← unfoldDefinition? expr with
          | some unfolded =>
            pure (some (← auditPropExprCore nextOrigin unfolded pos inProgressData inProgressProps (fuel - 1)))
          | none =>
            let exprW ← whnf expr
            if exprW != expr then
              pure (some (← auditPropExprCore nextOrigin exprW pos inProgressData inProgressProps (fuel - 1)))
            else pure none
        match hit? with
        | some hit =>
          if hit.isNone then
            if let some ck := cacheKey? then
              modify fun st => { st with verifiedPropDefs := st.verifiedPropDefs.insert ck }
          return hit
        | none =>
          -- Fail closed on any stuck `Prop` definition outside the allow-list.
          return some (origin, root)
      | some (.inductInfo info) =>
        if isAllowedAtomicProp name then
          for arg in args do
            let argTy ← whnf (← inferType arg)
            if argTy.isSort then
              if argTy != .sort .zero then
                if let some _ ← rejectNonDataTypeCore arg inProgressData (fuel - 1) false then
                  return some (origin, root)
            else if ← resultIsPropExpr argTy then
              let predHit ← forallTelescope argTy fun fxs _ => do
                for fx in fxs do
                  let fxTy ← inferType fx
                  if !(← isProp fxTy) then
                    if let some _ ← rejectNonDataTypeCore fxTy inProgressData (fuel - 1) false then
                      return some (origin, ← fx.fvarId!.getUserName)
                auditPropExprCore origin (mkAppN arg fxs) false inProgressData inProgressProps (fuel - 1)
              if let some hit := predHit then return some hit
            else
              if let some hit ← scanEmbeddedProps origin arg inProgressData inProgressProps (fuel - 1) then
                return some hit
          return none
        if isAuditPropNamespace name then
          let key? ← match ← canonicalizeKey? (args[:Nat.min info.numParams args.size].toArray) with
            | some cp => pure (some (name, pos, cp))
            | none => pure none
          if let some key := key? then
            if inProgressProps.contains key then return none
            if (← get).verifiedPropInducts.contains key then return none
          let inProgressProps' := match key? with
            | some k => k :: inProgressProps
            | none => inProgressProps
          for ctor in info.ctors do
            let some (.ctorInfo cinfo) := env.find? ctor | continue
            let ctype := cinfo.type.instantiateLevelParams cinfo.levelParams lvls
            let hit ← forallBoundedTelescope ctype cinfo.numParams fun pxs body => do
              let mut b := body
              for i in [:Nat.min pxs.size args.size] do
                b := b.replaceFVar pxs[i]! args[i]!
              forallTelescope b fun fields _ => do
                for f in fields do
                  let fTy ← inferType f
                  let fname ← f.fvarId!.getUserName
                  if ← isProp fTy then
                    if let some hit ← auditPropExprCore root fTy pos inProgressData inProgressProps' (fuel - 1) then
                      return some hit
                  else
                    -- The primitive continuation is fixed by the dispatch equality in `Step.prim`.
                    let dispatchCont := root == ``ConcreteSpec.Core.Exec.Step && fname == `k
                    -- `Derives.apply` and `Predictive.apply` bind `production` from the fixed
                    -- repository grammar `g.productions`; still verify `g.value` is data-only.
                    let fixedProduction ← do
                      if (ctor == ``ConcreteSpec.StageGrammar.Derives.apply && fname == `production) ||
                         (ctor == ``ConcreteSpec.StageGrammar.Predictive.apply && fname == `p) then
                        isDataBackedProduction fTy inProgressData fuel
                      else pure false
                    if !dispatchCont && !fixedProduction then
                      if let some _ ← rejectNonDataTypeCore fTy inProgressData (fuel - 1) false then
                        return some (root, fname)
                return none
            if let some hit := hit then return some hit
          if let some key := key? then
            modify fun st => { st with verifiedPropInducts := st.verifiedPropInducts.insert key }
          return none
        -- Fail closed on any unregistered `Prop`-valued inductive outside the allow-list.
        return some (origin, root)
      | _ => return some (origin, root)
    -- Atomic `sim.related p q` projection from the positive `CheckpointSimulation` witness in `LowersMemory`.
    if let .proj ``CheckpointSimulation 0 simExpr := fn then
      if (origin == ``Concrete.Interfaces.LowersMemory ||
          origin == ``Concrete.Interfaces.CheckpointSimulation.related) && simExpr.isFVar then
        for arg in args do
          if let some hit ← scanEmbeddedProps origin arg inProgressData inProgressProps (fuel - 1) then
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
  -- Match `sliceGrammar s`, `sliceLanguage s`, `sliceLoweringLanguage s` on the same slice `s`.
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

/-- Audit a top-level theorem or predicate type/value. Top-level binders over `Bool`,
`Edge`, and `Profile` are case-split over their finite constructors so `Registry.pipeline`
and `Stage.Sound` reduce completely without abstract stage parameters. -/
private partial def auditTopLevel (origin : Name) (expr : Expr) (pos : Bool := true)
    (allowTopPasses : Bool := true) (fuel : Nat := 128) : AuditM (Option (Name × Name)) := do
  if fuel == 0 then return some (origin, `depthExceeded)
  let expr ← instantiateMVars expr
  let expr ← whnfCore expr
  match expr with
  | .mdata _ body => auditTopLevel origin body pos allowTopPasses (fuel - 1)
  | .forallE name dom body bi =>
    if ← isProp dom then
      -- Top-level theorem premises (e.g. `Validated`, `Trusted`, `Ready` in `Pipeline.endToEnd`)
      -- are audited in their own positive polarity unless this `forallE` is a negation (`dom → False`).
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
      if domW.isConstOf ``Bool then
        for b in [``Bool.false, ``Bool.true] do
          if let some hit ← auditTopLevel origin (body.instantiate1 (mkConst b)) pos allowTopPasses (fuel - 1) then
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
    if domW.isConstOf ``Bool then
      for b in [``Bool.false, ``Bool.true] do
        if let some hit ← auditTopLevel origin (body.instantiate1 (mkConst b)) pos allowTopPasses (fuel - 1) then
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
    -- Return sort `Prop` of a predicate signature (e.g., `Stage.Sound : ... → Prop`).
    return none
  | _ =>
    auditPropExpr origin expr pos

/-- Audit a single executable pass field type: after full `whnf` unfolding of any
number of transparent abbreviations, it must be a first-order function `D₁ → ... → Dₖ → R`
(`k ≥ 1`) where every input `Dᵢ` and the return type `R` pass the data-only allow-list. -/
private partial def checkPassFieldType (type : Expr) (arity : Nat := 0) (fuel : Nat := 64) :
    AuditM (Option String) := do
  if fuel == 0 then return some "pass type unfolding depth exceeded"
  let type ← instantiateMVars type
  let type ← whnf type
  match type with
  | .mdata _ body => checkPassFieldType body arity (fuel - 1)
  | .letE name t v b nondep =>
    withLetDecl name t v (nondep := nondep) fun fv =>
      checkPassFieldType (b.instantiate1 fv) arity (fuel - 1)
  | .forallE name dom body bi =>
    if let some err ← rejectNonDataType dom then
      return some s!"pass parameter {name}: {err}"
    withLocalDecl name bi dom fun fv =>
      checkPassFieldType (body.instantiate1 fv) (arity + 1) (fuel - 1)
  | ret =>
    if arity == 0 then
      return some s!"pass field is not a function: {← ppExpr ret}"
    rejectNonDataType ret

/-- Check every field of a pass-bundle record instantiated with `paramArgs`. -/
private def badPassFieldAt (record : Name) (paramArgs : Array Expr := #[]) :
    AuditM (Option Name) := do
  let env ← getEnv
  let some structureInfo := getStructureInfo? env record | return some record
  let some (.inductInfo recordInfo) := env.find? record | return some record
  if paramArgs.size != recordInfo.numParams then return some record
  for field in structureInfo.fieldNames do
    let some (.defnInfo projection) := env.find? (record ++ field) | return some field
    let bad ← forallBoundedTelescope projection.type (some (recordInfo.numParams + 1)) fun xs fieldTy => do
      let mut ty := fieldTy
      for i in [:paramArgs.size] do
        ty := ty.replaceFVar xs[i]! paramArgs[i]!
      checkPassFieldType ty
    if bad.isSome then return some field
  return none

private def badPassField (record : Name) : AuditM (Option Name) :=
  badPassFieldAt record #[]

private def badRegisteredPasses : AuditM (Option Name) :=
  withLocalDeclD `slice (mkConst ``Slice) fun slice =>
  withLocalDeclD `profile (mkConst ``Profile) fun profile =>
    badPassFieldAt ``PassFunctions
      #[mkApp (mkConst ``sliceGrammarCount) slice,
        mkApp (mkConst ``sliceGrammar) slice,
        mkApp (mkConst ``sliceLanguage) slice,
        mkApp (mkConst ``sliceLoweringLanguage) slice,
        profile]

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

/-- Read the trust field on the composed edges. The final constructor fixes
both the BIR input and the backend output at this slice and profile. -/
private inductive FixedTrust (slice : Slice) (profile : Profile) :
    {A B : Type} → Pipeline A B → Prop
  | llvm {edge : Stage (BIR (sliceLoweringLanguage slice) profile) StageFailure (LLVMInput profile)} :
      edge.kind = .namedPremise "P_print" →
      edge.trust = @P_print (sliceLanguage slice) (sliceLoweringLanguage slice) profile →
      FixedTrust slice profile (.next edge .done)
  | lcb {edge : Stage (BIR (sliceLoweringLanguage slice) profile) StageFailure (LCBInput profile)} :
      edge.kind = .directProof → edge.trust = (fun _ _ => True) →
      FixedTrust slice profile (.next edge .done)
  | step {A R B C} {edge : Stage A R B} {rest : Pipeline B C} :
      (edge.kind = .directProof ∨ edge.kind = .provedValidator) →
      edge.trust = (fun _ _ => True) → FixedTrust slice profile rest →
      FixedTrust slice profile (.next edge rest)

example (slice : Slice) (profile : Profile) (passes : Passes slice profile) (unchecked : Bool) :
    FixedTrust slice profile (Registry.pipeline passes unchecked) ∧
    FixedTrust slice profile ((registry passes).lcbPipeline unchecked) := by
  obtain ⟨backend, target, slots, bytes⟩ := profile
  cases backend <;> cases unchecked <;> constructor <;>
    repeat' first
      | exact FixedTrust.llvm rfl rfl
      | exact FixedTrust.lcb rfl rfl
      | apply FixedTrust.step (Or.inl rfl) rfl
      | apply FixedTrust.step (Or.inr rfl) rfl

-- This mutation hides a caller-selected model inside an unfolded relation.
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

-- A data-valued quantified function in a logical premise remains caller-selectable.
private def plantedDataBinder : Prop := Nonempty ((decode : ByteArray → Option UInt8) → PLift (decode ByteArray.empty = none))

-- Identity can also carry a quantified type as its value. That binder is open.
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

-- Indexed type-family controls (ISS-0114): hidden decoder, model-valued index,
-- polymorphic family instantiated with a model, alias, nested executable results,
-- and genuine data-only indexed families (custom and registered) that must pass.
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

-- Round-2 allow-list attack controls (Codex findings 1-3):
-- 1a. Subtype carrying a constrained decoder.
private inductive PlantedSubtypeDecoder : Nat → Type where
  | intro (decode : { f : ByteArray → Option UInt8 // f ByteArray.empty = none }) :
      PlantedSubtypeDecoder 0

private def plantedSubtypeDecoder : Prop :=
  ∀ carrier : PlantedSubtypeDecoder 0, carrier = carrier

private structure PlantedSubtypePassBundle where
  run : Source → Except LexRefusal (PlantedSubtypeDecoder 0)

-- 1b. Decoder behind a closed record projection.
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

-- 2. Nesting the same type abbreviation around a decoder.
private abbrev NestedDataAlias (α : Type) := DataIndexedFamily α 1

private def plantedNestedAlias : Prop :=
  ∀ carrier : NestedDataAlias (NestedDataAlias (ByteArray → Option UInt8)), carrier = carrier

private structure PlantedNestedAliasPassBundle where
  run : Source → Except LexRefusal (NestedDataAlias (NestedDataAlias (ByteArray → Option UInt8)))

-- 3. Word-valued decoder (`ByteArray → UInt64`).
private inductive PlantedWordDecoder : Nat → Type where
  | intro (decode : ByteArray → UInt64) : PlantedWordDecoder 0

private def plantedWordDecoder : Prop :=
  ∀ carrier : PlantedWordDecoder 0, carrier = carrier

private structure PlantedWordDecoderPassBundle where
  run : Source → Except LexRefusal (PlantedWordDecoder 0)

-- Additional allow-list rejections: opaque constant, Sort-valued field, Prop-carrying field,
-- and subtype over data with a non-data predicate.
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

-- Round-2 false-positive controls (must now PASS after type reduction):
-- 1. Boolean family at `true` reduces to `Nat`.
private def passingMatchIndexedTrue : Prop :=
  ∀ carrier : plantedMatchIndexed true, carrier = carrier

-- 2. Double transparent abbreviation around the passing executable schema.
private abbrev PassResultAliasOuter (α : Type) := PassResultAlias α

private structure PassingDoubleAliasPassBundle where
  run : PassResultAliasOuter (DataIndexedFamily UInt8 3)

-- Genuine data-only subtype passes.
private def passingDataSubtype : Prop :=
  ∀ x : { n : Nat // n < 10 }, x = x

-- ISS-0131 controls: let-bound decoder existence and its negated hypothesis in `P_print`
-- must be rejected, while the unmodified `P_print` premise must pass.
private def plantedPPrintLetExists (profile : Profile)
    (bir : BIR (sliceLoweringLanguage slice0) profile) (output : LLVMInput profile) : Prop :=
  let witness := ∃ decode : ByteArray → Option UInt8, decode ByteArray.empty = none
  ¬ witness → P_print (language := sliceLanguage slice0) bir output

private def plantedLetExistsPipeline (profile : Profile) (passes : Passes slice0 profile) : Prop :=
  let emit := {(registry passes).emitLLVM with
    trust := plantedPPrintLetExists profile}
  (Pipeline.next emit .done).Trusted

-- Standard-library wrapper indirection around Exists (must unfold and reject, never fail open).
private def plantedFunctionConstExists : Prop :=
  ¬ (Function.const Nat Exists 0) (fun (decode : ByteArray → Option UInt8) => decode ByteArray.empty = none)

private def passingUnmodifiedPPrint (profile : Profile)
    (bir : BIR (sliceLoweringLanguage slice0) profile) (output : LLVMInput profile) : Prop :=
  P_print (language := sliceLanguage slice0) bir output

-- The explicit schema also catches unstructured relation parameters at the
-- boundary, even if their types are never declared as semantic model records.
example {slice : Slice} {profile : Profile} (passes : Passes slice profile) (unchecked : Bool)
    {input : Source} {output : Registry.Output profile}
    (validated : (Registry.pipeline passes unchecked).Validated)
    (trusted : (Registry.pipeline passes unchecked).Trusted)
    (ready : (Registry.pipeline passes unchecked).Ready input)
    (ran : (Registry.pipeline passes unchecked).run input = .ok output) :
    (Registry.pipeline passes unchecked).Corresponds input output :=
  @Pipeline.endToEnd profile slice passes unchecked input output validated trusted ready ran

example {slice : Slice} {profile : Profile} (passes : Passes slice profile) (unchecked : Bool)
    {input : Source} {output : LCBInput profile}
    (validated : ((registry passes).lcbPipeline unchecked).Validated)
    (ready : ((registry passes).lcbPipeline unchecked).Ready input)
    (ran : ((registry passes).lcbPipeline unchecked).run input = .ok output) :
    ((registry passes).lcbPipeline unchecked).Corresponds input output :=
  @registeredLCBEndToEnd profile slice passes unchecked input output validated ready ran

run_cmd liftTermElabM do
  let env ← getEnv
  -- A model packaged inside the pass argument would still be caller-selected.
  let fields := #[`lex, `elaborate, `checkCore, `lowerCore, `lowerUnchecked, `lowerMemory,
    `buildSSA, `buildBIR, `emitLLVM, `emitLCB]
  unless (getStructureInfo? env ``PassFunctions).map (·.fieldNames) == some fields do
    throwError "The pass argument must contain only the registered executable functions"
  let (_, _) ← (do
    if let some field ← badRegisteredPasses then
      throwError "The registered pass field {field} contains an open semantic model"
    for name in [``Pipeline.endToEnd, ``registeredLCBEndToEnd] do
      if let some (origin, binder) ← checkDecl name then
        throwError "End-to-end statement has an open semantic binder {binder} in {origin}"
    if let some (origin, binder) ← checkDecl ``Stage.Sound then
      throwError "Stage soundness has an open semantic binder {binder} in {origin}"
    -- Each wrapper extracts the original quantified type inside Nonempty.
    -- Check the actual binder so unrelated findings cannot pass the controls.
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
    -- Indexed type-family and allow-list attack controls (ISS-0114): each planted family
    -- (including subtype decoder, closed projection, nested alias, word-valued decoder,
    -- opaque constant, Sort/Prop fields, and non-data subtype predicate) must fail with
    -- its exact open semantic binder.
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
    -- ISS-0131 controls: let-bound negated existential in P_print must be rejected both
    -- directly and when installed in a pipeline trust field.
    unless (← checkDecl ``plantedPPrintLetExists) == some (`EndToEndAudit.plantedPPrintLetExists, `decode) do
      throwError "End-to-end audit accepted a let-bound negated existential in plantedPPrintLetExists"
    unless (← checkDecl ``plantedLetExistsPipeline) == some (`EndToEndAudit.plantedPPrintLetExists, `decode) do
      throwError "End-to-end audit accepted a let-bound negated existential through Pipeline.Trusted"
    unless (← checkDecl ``plantedFunctionConstExists) == some (`EndToEndAudit.plantedFunctionConstExists, `decode) do
      throwError "End-to-end audit accepted Exists wrapped behind Function.const"
    -- Passing controls: genuine data-only indexed families, reduced match family at `true`,
    -- double-abbreviated executable pass bundle, data-only subtype, and unmodified `P_print`.
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
