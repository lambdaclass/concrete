import Concrete.Elab.Core
import Concrete.Report.Diagnostic
import Concrete.Check.Layout

namespace Concrete

/-! ## Verify — internal-consistency verifiers for compiler boundaries

Each verifier checks a structural invariant that must hold at a specific pipeline stage.
Violations produce `Diagnostics` with pass = "verify" so they are surfaced as
internal compiler errors, not user errors.

**Post-Elab verifier** (`verifyNoPlaceholders`):
  After elaboration, every type annotation in the Core IR must be resolved.
  `Ty.placeholder` is a sentinel used during type inference; if it survives into
  validated Core it means elaboration silently dropped a type.

  **Documented exception (warning-only):** `Ty.placeholder` legitimately survives
  elaboration in exactly two cases:
  1. `try_` (the `?` operator) — the error-branch expression type is not resolved
     until monomorphization instantiates the concrete Result type.
  2. `defer` — deferred cleanup expressions may carry placeholder types when the
     deferred value's type is inferred from context resolved later.
  Both are resolved during lowering. This is an intentional pipeline design choice,
  not a silent leak. The verifier reports these as warnings so they remain visible
  for audit. Promoting to a hard error requires fixing try/defer elaboration to
  resolve types eagerly.

**Post-Mono verifier** (`verifyNoTypeVars`):
  After monomorphization, every generic type variable must be substituted.
  `Ty.typeVar` surviving into lowering causes crashes in EmitSSA / LLVM emission.
-/

-- ============================================================
-- Helpers: collect offending types from the IR
-- ============================================================

/-- Check whether a Ty contains `placeholder` anywhere (including nested). -/
partial def Ty.containsPlaceholder : Ty → Bool
  | .placeholder    => true
  | .ref inner      => inner.containsPlaceholder
  | .refMut inner   => inner.containsPlaceholder
  | .ptrMut inner   => inner.containsPlaceholder
  | .ptrConst inner => inner.containsPlaceholder
  | .heap inner     => inner.containsPlaceholder
  | .heapArray inner => inner.containsPlaceholder
  | .array elem _   => elem.containsPlaceholder
  | .generic _ args => args.any Ty.containsPlaceholder
  | .fn_ params _ retTy => params.any Ty.containsPlaceholder || retTy.containsPlaceholder
  | _ => false

/-- Check whether a Ty contains `typeVar` anywhere (including nested). -/
partial def Ty.containsTypeVar : Ty → Bool
  | .typeVar _      => true
  | .ref inner      => inner.containsTypeVar
  | .refMut inner   => inner.containsTypeVar
  | .ptrMut inner   => inner.containsTypeVar
  | .ptrConst inner => inner.containsTypeVar
  | .heap inner     => inner.containsTypeVar
  | .heapArray inner => inner.containsTypeVar
  | .array elem _   => elem.containsTypeVar
  | .generic _ args => args.any Ty.containsTypeVar
  | .fn_ params _ retTy => params.any Ty.containsTypeVar || retTy.containsTypeVar
  | _ => false

-- ============================================================
-- Collecting violations from CExpr / CStmt trees
-- ============================================================

/-- A verification violation: function name + description of what was found. -/
structure VerifyViolation where
  fnName : String
  message : String

mutual
/-- Scan a CExpr for types satisfying `pred`, collecting violations. -/
partial def collectExprViolations (fnName : String) (pred : Ty → Bool) (label : String)
    (e : CExpr) : List VerifyViolation :=
  let checkTy (t : Ty) : List VerifyViolation :=
    if pred t then [{ fnName, message := s!"{label} in expression type: {tyToStr t}" }] else []
  let self := checkTy e.ty
  let children := match e with
    | .binOp _ lhs rhs _ =>
      collectExprViolations fnName pred label lhs ++
      collectExprViolations fnName pred label rhs
    | .unaryOp _ operand _ =>
      collectExprViolations fnName pred label operand
    | .call _ typeArgs args _ =>
      typeArgs.foldl (fun acc t => acc ++ if pred t then [{ fnName, message := s!"{label} in call type arg: {tyToStr t}" }] else []) [] ++
      args.foldl (fun acc a => acc ++ collectExprViolations fnName pred label a) []
    | .structLit _ typeArgs fields _ =>
      typeArgs.foldl (fun acc t => acc ++ if pred t then [{ fnName, message := s!"{label} in struct type arg: {tyToStr t}" }] else []) [] ++
      fields.foldl (fun acc (_, fe) => acc ++ collectExprViolations fnName pred label fe) []
    | .enumLit _ _ typeArgs fields _ =>
      typeArgs.foldl (fun acc t => acc ++ if pred t then [{ fnName, message := s!"{label} in enum type arg: {tyToStr t}" }] else []) [] ++
      fields.foldl (fun acc (_, fe) => acc ++ collectExprViolations fnName pred label fe) []
    | .fieldAccess obj _ _ => collectExprViolations fnName pred label obj
    | .borrow inner _ => collectExprViolations fnName pred label inner
    | .borrowMut inner _ => collectExprViolations fnName pred label inner
    | .deref inner _ => collectExprViolations fnName pred label inner
    | .cast inner _ => collectExprViolations fnName pred label inner
    | .try_ inner _ => collectExprViolations fnName pred label inner
    | .arrayLit elems _ =>
      elems.foldl (fun acc el => acc ++ collectExprViolations fnName pred label el) []
    | .arrayIndex arr idx _ =>
      collectExprViolations fnName pred label arr ++
      collectExprViolations fnName pred label idx
    | .allocCall inner allocExpr _ =>
      collectExprViolations fnName pred label inner ++
      collectExprViolations fnName pred label allocExpr
    | .match_ scrutinee arms _ =>
      collectExprViolations fnName pred label scrutinee ++
      arms.foldl (fun acc arm => acc ++ collectArmViolations fnName pred label arm) []
    | .whileExpr cond body elseBody _ =>
      collectExprViolations fnName pred label cond ++
      body.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) [] ++
      elseBody.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) []
    | .ifExpr cond then_ else_ _ =>
      collectExprViolations fnName pred label cond ++
      then_.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) [] ++
      else_.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) []
    | .fnRef _ _ => []
    | _ => []  -- literals and ident: no children
  self ++ children

/-- Scan a match arm for type violations. -/
partial def collectArmViolations (fnName : String) (pred : Ty → Bool) (label : String)
    (arm : CMatchArm) : List VerifyViolation :=
  let gv := fun (g : Option CExpr) => (g.map (collectExprViolations fnName pred label ·)).getD []
  match arm with
  | .enumArm _ _ bindings guard body =>
    bindings.foldl (fun acc (_, t) => acc ++ if pred t then [{ fnName, message := s!"{label} in match binding type: {tyToStr t}" }] else []) [] ++
    gv guard ++ body.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) []
  | .litArm value guard body =>
    collectExprViolations fnName pred label value ++ gv guard ++
    body.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) []
  | .varArm _ bindTy guard body =>
    (if pred bindTy then [{ fnName, message := s!"{label} in match var binding type: {tyToStr bindTy}" }] else []) ++
    gv guard ++ body.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) []
  | .rangeArm lo hi _ guard body =>
    collectExprViolations fnName pred label lo ++
    collectExprViolations fnName pred label hi ++ gv guard ++
    body.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) []

/-- Scan a CStmt for type violations. -/
partial def collectStmtViolations (fnName : String) (pred : Ty → Bool) (label : String)
    (stmt : CStmt) : List VerifyViolation :=
  match stmt with
  | .letDecl _ _ ty value =>
    (if pred ty then [{ fnName, message := s!"{label} in let binding type: {tyToStr ty}" }] else []) ++
    collectExprViolations fnName pred label value
  | .assign _ value => collectExprViolations fnName pred label value
  | .return_ (some value) retTy =>
    (if pred retTy then [{ fnName, message := s!"{label} in return type: {tyToStr retTy}" }] else []) ++
    collectExprViolations fnName pred label value
  | .return_ none retTy =>
    if pred retTy then [{ fnName, message := s!"{label} in return type: {tyToStr retTy}" }] else []
  | .expr e _ => collectExprViolations fnName pred label e
  | .ifElse cond then_ else_ =>
    collectExprViolations fnName pred label cond ++
    then_.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) [] ++
    match else_ with
    | some stmts => stmts.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) []
    | none => []
  | .while_ cond body _ step =>
    collectExprViolations fnName pred label cond ++
    body.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) [] ++
    step.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) []
  | .fieldAssign obj _ value =>
    collectExprViolations fnName pred label obj ++
    collectExprViolations fnName pred label value
  | .derefAssign target value =>
    collectExprViolations fnName pred label target ++
    collectExprViolations fnName pred label value
  | .arrayIndexAssign arr idx value =>
    collectExprViolations fnName pred label arr ++
    collectExprViolations fnName pred label idx ++
    collectExprViolations fnName pred label value
  | .break_ (some value) _ => collectExprViolations fnName pred label value
  | .break_ none _ => []
  | .continue_ _ => []
  | .defer body => collectExprViolations fnName pred label body
  | .borrowIn _ _ _ _ refTy body =>
    (if pred refTy then [{ fnName, message := s!"{label} in borrow ref type: {tyToStr refTy}" }] else []) ++
    body.foldl (fun acc s => acc ++ collectStmtViolations fnName pred label s) []
end

-- ============================================================
-- Function-level verification
-- ============================================================

/-- Verify a single function's IR against a type predicate.
    When `skipGenerics` is true, skips generic definitions (non-empty typeParams). -/
def verifyFnTypes (pred : Ty → Bool) (label : String) (fn : CFnDef) (skipGenerics : Bool := false) : List VerifyViolation :=
  if skipGenerics && !fn.typeParams.isEmpty then [] else
  -- Check parameter types
  let paramViolations := fn.params.foldl (fun acc (pname, t) =>
    acc ++ if pred t then [{ fnName := fn.name, message := s!"{label} in parameter '{pname}' type: {tyToStr t}" }] else []) []
  -- Check return type
  let retViolation := if pred fn.retTy
    then [{ fnName := fn.name, message := s!"{label} in return type: {tyToStr fn.retTy}" }]
    else []
  -- Check body
  let bodyViolations := fn.body.foldl (fun acc s =>
    acc ++ collectStmtViolations fn.name pred label s) []
  paramViolations ++ retViolation ++ bodyViolations

/-- Verify a module (and submodules) against a type predicate.
    When `skipGenerics` is true, skips generic definitions (functions/structs/enums with typeParams). -/
partial def verifyModuleTypes (pred : Ty → Bool) (label : String) (m : CModule) (skipGenerics : Bool := false) : List VerifyViolation :=
  let fnViolations := m.functions.foldl (fun acc fn =>
    acc ++ verifyFnTypes pred label fn skipGenerics) []
  -- Check struct field types
  let structViolations := m.structs.foldl (fun acc s =>
    if skipGenerics && !s.typeParams.isEmpty then acc else
    s.fields.foldl (fun acc2 (fname, t) =>
      acc2 ++ if pred t then [{ fnName := s.name, message := s!"{label} in struct field '{fname}' type: {tyToStr t}" }] else []) acc) []
  -- Check enum variant types
  let enumViolations := m.enums.foldl (fun acc e =>
    if skipGenerics && !e.typeParams.isEmpty then acc else
    e.variants.foldl (fun acc2 (vname, fields) =>
      fields.foldl (fun acc3 (fname, t) =>
        acc3 ++ if pred t then [{ fnName := s!"{e.name}::{vname}", message := s!"{label} in enum field '{fname}' type: {tyToStr t}" }] else []) acc2) acc) []
  -- Check extern fn types
  let externViolations := m.externFns.foldl (fun acc (name, params, retTy, _) =>
    let pv := params.foldl (fun acc2 (pname, t) =>
      acc2 ++ if pred t then [{ fnName := name, message := s!"{label} in extern param '{pname}' type: {tyToStr t}" }] else []) []
    let rv := if pred retTy then [{ fnName := name, message := s!"{label} in extern return type: {tyToStr retTy}" }] else []
    acc ++ pv ++ rv) []
  -- Check constant types
  let constViolations := m.constants.foldl (fun acc (name, ty, _) =>
    acc ++ if pred ty then [{ fnName := name, message := s!"{label} in constant type: {tyToStr ty}" }] else []) []
  -- Recurse into submodules
  let subViolations := m.submodules.foldl (fun acc sub =>
    acc ++ verifyModuleTypes pred label sub skipGenerics) []
  fnViolations ++ structViolations ++ enumViolations ++ externViolations ++ constViolations ++ subViolations

-- ============================================================
-- Public API: specific verifiers
-- ============================================================

/-- Convert violations to diagnostics. -/
def violationsToDiagnostics (pass : String) (violations : List VerifyViolation) : Diagnostics :=
  violations.map fun v => {
    severity := .error
    message := s!"[{pass}] {v.fnName}: {v.message}"
    pass := "verify"
    span := none
    hint := some "this is an internal compiler error — please report it"
    code := if pass == "post-elab" then "E0600" else "E0601"
  }

/-- **Post-Elab verifier**: no `Ty.placeholder` may survive elaboration.
    Run after `Pipeline.coreCheck` (on ValidatedCore). -/
def verifyNoPlaceholders (modules : List CModule) : Diagnostics :=
  let violations := modules.foldl (fun acc m =>
    acc ++ verifyModuleTypes Ty.containsPlaceholder "Ty.placeholder found" m) []
  violationsToDiagnostics "post-elab" violations

/-- **Post-Mono verifier**: no `Ty.typeVar` may survive monomorphization.
    Run after `Pipeline.monomorphize`. Skips generic definitions — only
    monomorphized copies are checked. -/
def verifyNoTypeVars (modules : List CModule) : Diagnostics :=
  let violations := modules.foldl (fun acc m =>
    acc ++ verifyModuleTypes Ty.containsTypeVar "Ty.typeVar found" m (skipGenerics := true)) []
  violationsToDiagnostics "post-mono" violations

/-- Check whether a type is Copy, given the full post-mono struct and enum lists.
    Delegates to the one shared definition (`Layout.isCopyTyCore`). Post-mono
    policy: any surviving `.typeVar` is treated as not-Copy (`typeVarIsCopy :=
    false`) — it should have been substituted away by monomorphization. -/
private def isCopyTyPostMono (allStructs : List CStructDef) (allEnums : List CEnumDef) (ty : Ty) : Bool :=
  Layout.isCopyTyCore allStructs allEnums (typeVarIsCopy := false) ty

/-- **Post-Mono Copy verifier**: monomorphized Copy structs must have all-Copy fields.
    Generic Copy structs skip field checks pre-mono (type params are assumed Copy).
    After monomorphization, concrete types are known and must be validated. -/
def verifyCopyFieldsPostMono (modules : List CModule) : Diagnostics :=
  let allStructs := modules.foldl (fun acc m => acc ++ m.structs) []
  let allEnums := modules.foldl (fun acc m => acc ++ m.enums) []
  let violations := allStructs.foldl (fun acc sd =>
    if !sd.isCopy || !sd.typeParams.isEmpty then acc
    else sd.fields.foldl (fun acc (fname, fty) =>
      if isCopyTyPostMono allStructs allEnums fty then acc
      else acc ++ [{ severity := .error
                     message := s!"Copy struct '{sd.name}' has non-Copy field '{fname}' after monomorphization"
                     pass := "post-mono"
                     span := none
                     hint := some "the generic type parameter was instantiated with a non-Copy type" }]
    ) acc
  ) ([] : Diagnostics)
  violations

/-- **Post-Mono verifier**: checks no `Ty.typeVar` survives monomorphization,
    and validates Copy struct fields after generic instantiation. Mixed-width
    binops (class 3) are already re-asserted at the CoreCheck boundary (E0502),
    so they need no separate post-mono check here — see docs/COMPILER_BOUNDARY.md. -/
def verifyPostMono (modules : List CModule) : Diagnostics :=
  verifyNoTypeVars modules ++ verifyCopyFieldsPostMono modules

-- ============================================================
-- Rendering
-- ============================================================

/-- Render verify diagnostics as a human-readable report. -/
def renderVerifyDiagnostics (ds : Diagnostics) : String :=
  if ds.isEmpty then "All verifier checks passed."
  else
    let errors := ds.filter (·.severity == .error)
    let warnings := ds.filter (·.severity == .warning)
    let header := if errors.isEmpty
      then s!"Verifier: {warnings.length} warning(s), 0 errors\n"
      else s!"VERIFIER FAILED: {errors.length} error(s), {warnings.length} warning(s)\n"
    let body := ds.foldl (fun acc d =>
      let sev := match d.severity with | .error => "error" | .warning => "warning" | .note => "note"
      acc ++ s!"  {sev}: {d.message}\n") ""
    header ++ body

/-- Render a per-gate verify report as a human-readable banner.
    A gate is marked `skipped` if any upstream gate has errors — that
    is, when its input artifact was not produced. Post-elab is
    warnings-only (never blocks downstream), so the skip cascade
    starts at post-mono. -/
def renderVerifyGates
    (postElab postMono postLower postCleanup : Diagnostics) : String :=
  let hasErrors (ds : Diagnostics) : Bool := ds.any (·.severity == .error)
  let renderGate (label : String) (ds : Diagnostics) (skipped : Bool) : String :=
    let errors := (ds.filter (·.severity == .error)).length
    let warnings := (ds.filter (·.severity == .warning)).length
    let status :=
      if skipped then "skipped (upstream gate failed)"
      else if errors > 0 then s!"FAIL ({errors} error(s), {warnings} warning(s))"
      else if warnings > 0 then s!"warn ({warnings} warning(s))"
      else "ok"
    let detail := ds.foldl (fun acc d =>
      let sev := match d.severity with
        | .error => "error" | .warning => "warning" | .note => "note"
      acc ++ s!"      {sev}: {d.message}\n") ""
    let pad := if label.length < 12 then "".pushn ' ' (12 - label.length) else ""
    s!"  {label}{pad} {status}\n{detail}"
  let monoSkipped     := false  -- post-elab never blocks
  let lowerSkipped    := hasErrors postMono
  let cleanupSkipped  := hasErrors postMono || hasErrors postLower
  let totalErrors :=
    (postElab.filter (·.severity == .error)).length +
    (postMono.filter (·.severity == .error)).length +
    (postLower.filter (·.severity == .error)).length +
    (postCleanup.filter (·.severity == .error)).length
  let totalWarnings :=
    (postElab.filter (·.severity == .warning)).length +
    (postMono.filter (·.severity == .warning)).length +
    (postLower.filter (·.severity == .warning)).length +
    (postCleanup.filter (·.severity == .warning)).length
  let banner :=
    if totalErrors > 0 then s!"VERIFY-GATES: FAIL ({totalErrors} error(s), {totalWarnings} warning(s))\n"
    else if totalWarnings > 0 then s!"VERIFY-GATES: warn ({totalWarnings} warning(s))\n"
    else "VERIFY-GATES: ok (all 4 gates clean)\n"
  banner ++
    renderGate "post-elab"    postElab    false ++
    renderGate "post-mono"    postMono    monoSkipped ++
    renderGate "post-lower"   postLower   lowerSkipped ++
    renderGate "post-cleanup" postCleanup cleanupSkipped

end Concrete
