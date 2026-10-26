import Std.Data.HashMap
import Std.Data.TreeMap
import Concrete.Elab.Core
import Concrete.Report.Diagnostic
import Concrete.Check.Layout
import Concrete.Semantics.IntArith
import Concrete.Proof.CallCollect
import Concrete.Semantics.Capabilities
import Concrete.Resolve.Shared
import Concrete.Semantics.RefWellFormed

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
  elaboration for `defer`: deferred cleanup expressions may carry placeholder types when the
     deferred value's type is inferred from context resolved later.
  It is resolved during lowering. This is an intentional pipeline design choice,
  not a silent leak. The verifier reports these as warnings so they remain visible
  for audit. Promoting to a hard error requires fixing defer elaboration to
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
  | .generic _ args _ => args.any Ty.containsPlaceholder
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
  | .generic _ args _ => args.any Ty.containsTypeVar
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
    (e : CExpr) (hook : CExpr → Option (List VerifyViolation) := fun _ => none) : List VerifyViolation :=
  match hook e with
  | some handled => handled
  | none =>
  let checkTy (t : Ty) : List VerifyViolation :=
    if pred t then [{ fnName, message := s!"{label} in expression type: {tyToStr t}" }] else []
  let self := checkTy e.ty
  let children := match e with
    | .binOp _ lhs rhs _ =>
      collectExprViolations (hook := hook) fnName pred label lhs ++
      collectExprViolations (hook := hook) fnName pred label rhs
    | .unaryOp _ operand _ =>
      collectExprViolations (hook := hook) fnName pred label operand
    | .call _ typeArgs args _ =>
      typeArgs.foldl (fun acc t => acc ++ if pred t then [{ fnName, message := s!"{label} in call type arg: {tyToStr t}" }] else []) [] ++
      args.foldl (fun acc a => acc ++ collectExprViolations (hook := hook) fnName pred label a) []
    | .structLit _ typeArgs fields _ =>
      typeArgs.foldl (fun acc t => acc ++ if pred t then [{ fnName, message := s!"{label} in struct type arg: {tyToStr t}" }] else []) [] ++
      fields.foldl (fun acc (_, fe) => acc ++ collectExprViolations (hook := hook) fnName pred label fe) []
    | .enumLit _ _ typeArgs fields _ =>
      typeArgs.foldl (fun acc t => acc ++ if pred t then [{ fnName, message := s!"{label} in enum type arg: {tyToStr t}" }] else []) [] ++
      fields.foldl (fun acc (_, fe) => acc ++ collectExprViolations (hook := hook) fnName pred label fe) []
    | .fieldAccess obj _ _ => collectExprViolations (hook := hook) fnName pred label obj
    | .borrow inner _ => collectExprViolations (hook := hook) fnName pred label inner
    | .borrowMut inner _ => collectExprViolations (hook := hook) fnName pred label inner
    | .deref inner _ => collectExprViolations (hook := hook) fnName pred label inner
    | .cast inner _ => collectExprViolations (hook := hook) fnName pred label inner
    | .arrayLit elems _ =>
      elems.foldl (fun acc el => acc ++ collectExprViolations (hook := hook) fnName pred label el) []
    | .arrayIndex arr idx _ =>
      collectExprViolations (hook := hook) fnName pred label arr ++
      collectExprViolations (hook := hook) fnName pred label idx
    | .allocCall inner allocExpr _ =>
      collectExprViolations (hook := hook) fnName pred label inner ++
      collectExprViolations (hook := hook) fnName pred label allocExpr
    | .match_ scrutinee arms _ =>
      collectExprViolations (hook := hook) fnName pred label scrutinee ++
      arms.foldl (fun acc arm => acc ++ collectArmViolations (hook := hook) fnName pred label arm) []
    | .ifExpr cond then_ else_ _ =>
      collectExprViolations (hook := hook) fnName pred label cond ++
      then_.foldl (fun acc s => acc ++ collectStmtViolations (hook := hook) fnName pred label s) [] ++
      else_.foldl (fun acc s => acc ++ collectStmtViolations (hook := hook) fnName pred label s) []
    | .fnRef _ _ => []
    | _ => []  -- literals and ident: no children
  self ++ children

/-- Scan a match arm for type violations. -/
partial def collectArmViolations (fnName : String) (pred : Ty → Bool) (label : String)
    (arm : CMatchArm) (hook : CExpr → Option (List VerifyViolation) := fun _ => none) : List VerifyViolation :=
  let gv := fun (g : Option CExpr) => (g.map (collectExprViolations (hook := hook) fnName pred label ·)).getD []
  match arm with
  | .enumArm _ _ bindings guard body =>
    bindings.foldl (fun acc (_, t) => acc ++ if pred t then [{ fnName, message := s!"{label} in match binding type: {tyToStr t}" }] else []) [] ++
    gv guard ++ body.foldl (fun acc s => acc ++ collectStmtViolations (hook := hook) fnName pred label s) []
  | .litArm value guard body =>
    collectExprViolations (hook := hook) fnName pred label value ++ gv guard ++
    body.foldl (fun acc s => acc ++ collectStmtViolations (hook := hook) fnName pred label s) []
  | .varArm _ bindTy guard body =>
    (if pred bindTy then [{ fnName, message := s!"{label} in match var binding type: {tyToStr bindTy}" }] else []) ++
    gv guard ++ body.foldl (fun acc s => acc ++ collectStmtViolations (hook := hook) fnName pred label s) []
  | .rangeArm lo hi _ guard body =>
    collectExprViolations (hook := hook) fnName pred label lo ++
    collectExprViolations (hook := hook) fnName pred label hi ++ gv guard ++
    body.foldl (fun acc s => acc ++ collectStmtViolations (hook := hook) fnName pred label s) []

/-- Scan a CStmt for type violations. -/
partial def collectStmtViolations (fnName : String) (pred : Ty → Bool) (label : String)
    (stmt : CStmt) (hook : CExpr → Option (List VerifyViolation) := fun _ => none) : List VerifyViolation :=
  match stmt with
  | .letDecl _ _ ty value =>
    (if pred ty then [{ fnName, message := s!"{label} in let binding type: {tyToStr ty}" }] else []) ++
    collectExprViolations (hook := hook) fnName pred label value
  | .assign _ value => collectExprViolations (hook := hook) fnName pred label value
  | .return_ (some value) retTy =>
    (if pred retTy then [{ fnName, message := s!"{label} in return type: {tyToStr retTy}" }] else []) ++
    collectExprViolations (hook := hook) fnName pred label value
  | .return_ none retTy =>
    if pred retTy then [{ fnName, message := s!"{label} in return type: {tyToStr retTy}" }] else []
  | .expr e _ => collectExprViolations (hook := hook) fnName pred label e
  | .ifElse cond then_ else_ =>
    collectExprViolations (hook := hook) fnName pred label cond ++
    then_.foldl (fun acc s => acc ++ collectStmtViolations (hook := hook) fnName pred label s) [] ++
    match else_ with
    | some stmts => stmts.foldl (fun acc s => acc ++ collectStmtViolations (hook := hook) fnName pred label s) []
    | none => []
  | .while_ cond body _ step =>
    collectExprViolations (hook := hook) fnName pred label cond ++
    body.foldl (fun acc s => acc ++ collectStmtViolations (hook := hook) fnName pred label s) [] ++
    step.foldl (fun acc s => acc ++ collectStmtViolations (hook := hook) fnName pred label s) []
  | .fieldAssign obj _ value =>
    collectExprViolations (hook := hook) fnName pred label obj ++
    collectExprViolations (hook := hook) fnName pred label value
  | .derefAssign target value =>
    collectExprViolations (hook := hook) fnName pred label target ++
    collectExprViolations (hook := hook) fnName pred label value
  | .arrayIndexAssign arr idx value =>
    collectExprViolations (hook := hook) fnName pred label arr ++
    collectExprViolations (hook := hook) fnName pred label idx ++
    collectExprViolations (hook := hook) fnName pred label value
  | .break_ (some value) _ => collectExprViolations (hook := hook) fnName pred label value
  | .break_ none _ => []
  | .continue_ _ => []
  | .defer body => collectExprViolations (hook := hook) fnName pred label body
  | .borrowIn _ _ _ _ refTy body =>
    (if pred refTy then [{ fnName, message := s!"{label} in borrow ref type: {tyToStr refTy}" }] else []) ++
    body.foldl (fun acc s => acc ++ collectStmtViolations (hook := hook) fnName pred label s) []
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
                     code := "E0571"
                     message := s!"Copy struct '{sd.name}' has non-Copy field '{fname}' after monomorphization"
                     pass := "post-mono"
                     span := none
                     hint := some "the generic type parameter was instantiated with a non-Copy type" }]
    ) acc
  ) ([] : Diagnostics)
  violations

/-- **Returned-reference verifier** (Phase 6.5 #4, class 6). References are
    second-class: NO function may return a reference type, directly or nested in
    an aggregate/generic instantiation (docs/language/VALUE_MODEL.md). `Check.lean` already
    rejects this at the front end for every function (the low-level escape is a
    raw pointer `*const`/`*mut`, not a returned reference); this is the post-mono
    re-assertion at the pipeline boundary — the one residue class that was
    otherwise caught only in Check (see docs/compiler/COMPILER_BOUNDARY.md). Structural, no
    lifetime/provenance analysis. Post-mono, so the return type is fully
    substituted (a generic instantiated to a reference is caught too). -/
-- spec: REF-1 (docs/spec/references.md; this declaration enforces the rule)
partial def verifyNoReturnedRefs : List CModule → Diagnostics
  | [] => []
  | m :: ms =>
    let here := m.functions.foldl (fun acc fn =>
      -- The same judgment as Check's (`tyContainsRef`, REF-1), as defense in depth after
      -- specialization: it looks through raw pointers and function results, which the old
      -- `tyExposesRef` did not, so `id::<*const &Int>` reached transitively is caught (G2).
      if tyContainsRef fn.retTy then
        acc ++ [({ severity := .error
                   message := s!"function '{fn.name}' returns a reference type ({tyToStr fn.retTy}); references are second-class and may not be returned — use a value, an owned view, a scoped accessor (with_value), or a raw pointer for low-level access"
                   pass := "post-mono"
                   span := fn.declSpan
                   hint := some "see docs/language/VALUE_MODEL.md — references are second-class"
                   code := "E0236"
                   evidence := [("rule", "REF-1@1"), ("spec", ruleSpecRef "REF-1@1")]
                   nextAction := some "return a value or an owned view, use a scoped callback (`with_value`), or return a raw pointer from trusted code" } : Diagnostic)]
      else acc) ([] : Diagnostics)
    here ++ verifyNoReturnedRefs m.submodules ++ verifyNoReturnedRefs ms

/-- The literal-range check for one expression node, as a walker hook. An
    integer literal must fit the integer type it is stamped with; a negated
    literal `-N` is checked as the value `-N` (so `-128 : i8`, whose inner `128`
    alone does not fit, is accepted) and its inner literal is not visited again.
    `none` = not a literal node, keep walking. -/
private def literalRangeHook (fnName : String) (e : CExpr) : Option (List VerifyViolation) :=
  let check (v : Int) (ty : Ty) : List VerifyViolation :=
    match IntArith.intRange ty with
    | some (lo, hi) =>
      if lo ≤ v && v ≤ hi then []
      else [{ fnName, message := s!"integer literal {v} is out of range for type '{tyToStr ty}' ({lo}..={hi}) after monomorphization" }]
    | none => []
  match e with
  | .intLit v ty => some (check v ty)
  | .unaryOp .neg (.intLit v _) ty => some (check (-v) ty)
  | _ => none

/-- **Post-Mono literal-range verifier** (bug 088). Check range-checks every
    literal whose type it knows, but a literal adopted at a type VARIABLE
    (`let y: T = 300`) gets its width only when Mono substitutes `T` — and Mono
    substitutes silently. Instantiated at `u8`, the binary emitted `i8 300` (44)
    while the interpreter kept 300. This re-asserts the literal-range rule on the
    monomorphized copies, where every literal's type is concrete, as a USER error
    with the same code as Check's (E0227). Generic definitions are skipped; only
    their instances are checked. -/
partial def verifyLiteralRanges (modules : List CModule) : Diagnostics :=
  let rec goModule (m : CModule) : List VerifyViolation :=
    let noTy : Ty → Bool := fun _ => false
    let fnVs := m.functions.foldl (fun acc fn =>
      if !fn.typeParams.isEmpty then acc
      else fn.body.foldl (fun acc2 st =>
        acc2 ++ collectStmtViolations fn.name noTy "" st (hook := literalRangeHook fn.name)) acc) []
    let constVs := m.constants.foldl (fun acc (name, _, value) =>
      acc ++ collectExprViolations name noTy "" value (hook := literalRangeHook name)) []
    fnVs ++ constVs ++ m.submodules.foldl (fun acc sub => acc ++ goModule sub) []
  let violations := modules.foldl (fun acc m => acc ++ goModule m) []
  violations.map fun v => {
    severity := .error
    message := s!"{v.fnName}: {v.message}"
    pass := "post-mono"
    span := none
    hint := some "a literal must fit the type it is instantiated at; use a value within that range or an explicit `as` cast"
    code := "E0227"
  }

/-- The comparison rule after monomorphization (bugs 134, 140): every comparison operator
    in an instance compares integers, floats, `bool`, `char` or raw pointers. Check accepts a
    comparison at a `Copy`-bounded type parameter (it cannot consume an operand); this
    re-asserts the scalar rule on each monomorphized copy, where the operand type is
    concrete, so `max<T: Copy>` at a struct is a user error (E0229) instead of an ill-typed
    `icmp` that only `llvm-as` rejected. `none` = not a refused comparison, keep walking. -/
private def comparisonTypeHook (fnName : String) (unsafeOk : Bool) (e : CExpr) : Option (List VerifyViolation) :=
  match e with
  | .binOp op lhs _ _ =>
    let isCmp := match op with
      | .eq | .neq | .lt | .gt | .leq | .geq => true
      | _ => false
    if !isCmp then none
    else
      let t := lhs.ty
      let ok := match t with
        | .bool | .char | .never | .placeholder => true
        -- A raw pointer compares only in trusted code or a function declaring `Unsafe`. An
        -- instance carries its definition's authority (`isTrusted` and `capSet`, copied by
        -- Mono; the same predicate Check applies at the source), so a safe `Copy`-bounded generic instantiated at a raw
        -- pointer is refused here as the same comparison written directly is refused by
        -- Check (bug 185).
        | .ptrMut _ | .ptrConst _ => unsafeOk
        | t => isNumeric t
      if ok then none
      else some [{ fnName, message := s!"comparison is not defined on '{tyToStr t}' after monomorphization: comparison operators are defined on integers, floats, `bool` and `char`, and on raw pointers only in trusted or `Unsafe` code" }]
  | _ => none

/-- **Post-Mono comparison verifier** (bugs 134, 140): see `comparisonTypeHook`. Generic
    definitions are skipped; only their instances are checked. -/
partial def verifyComparisonTypes (modules : List CModule) : Diagnostics :=
  let rec goModule (m : CModule) : List VerifyViolation :=
    let noTy : Ty → Bool := fun _ => false
    let fnVs := m.functions.foldl (fun acc fn =>
      if !fn.typeParams.isEmpty then acc
      else fn.body.foldl (fun acc2 st =>
        acc2 ++ collectStmtViolations fn.name noTy "" st
          (hook := comparisonTypeHook fn.name (Capabilities.capsAllowUnsafeOp fn.isTrusted fn.capSet))) acc) []
    fnVs ++ m.submodules.foldl (fun acc sub => acc ++ goModule sub) []
  let violations := modules.foldl (fun acc m => acc ++ goModule m) []
  violations.map fun v => {
    severity := .error
    message := s!"{v.fnName}: {v.message}"
    pass := "post-mono"
    span := none
    hint := some "a generic comparison instantiated at an aggregate, or at a raw pointer in a generic that is neither trusted nor declares `Unsafe`: compare through a named function, or instantiate at a scalar type"
    code := "E0229"
  }

/-- The operator rule after monomorphization (bug 211, TYPE-6@1): every arithmetic,
    wrapping/saturating and bitwise operator in an instance applies to operands of one numeric type
    (integers for wrapping/saturating and bitwise), or, for arithmetic in trusted or `Unsafe` code, to
    a raw pointer and an integer. CoreCheck defers an operand whose type is a type parameter to the
    instantiation (`isOpaqueOperandTy`); this is that check, on each monomorphized copy, so
    `add<T: Copy>` at a struct is a user error (E0501) instead of an ill-typed call that only
    `llvm-as` rejected. `none` = not a refused operator, keep walking. -/
private def operatorTypeHook (fnName : String) (unsafeOk : Bool) (e : CExpr) : Option (List VerifyViolation) :=
  match e with
  | .binOp op lhs rhs _ =>
    let free : Ty → Bool := fun t => match t with | .never | .placeholder => true | _ => false
    let isPtr : Ty → Bool := fun t => match t with | .ptrMut _ | .ptrConst _ => true | _ => false
    let lt := lhs.ty
    let rt := rhs.ty
    -- A numeric literal operand is flexible: in a generic body it keeps the type Elab gave it
    -- (`(v as T) - 48` in std's `digit_to_int` is `Uint - Int` at `T = Uint`) and takes the
    -- other operand's type when emitted; `verifyLiteralRanges` checks that it fits.
    let isLit : CExpr → Bool := fun e => match e with
      | .intLit .. | .floatLit .. | .unaryOp .neg (.intLit ..) _ | .unaryOp .neg (.floatLit ..) _ => true
      | _ => false
    let agree : Ty → Ty → Bool := fun a b => isLit lhs || isLit rhs || binOpOperandsAgree a b
    -- Both operands, and their agreement: CoreCheck defers the agreement too when EITHER side
    -- is a type parameter (`a: Int` plus `b: T` at a struct; Codex review).
    let ok? : Option Bool := match op with
      | .add | .sub | .mul | .div | .mod =>
        some (free lt || free rt
          || (isPtr lt && isInteger rt && unsafeOk)
          || (isNumeric lt && isNumeric rt && agree lt rt))
      | .wrappingAdd | .wrappingSub | .wrappingMul
      | .saturatingAdd | .saturatingSub | .saturatingMul
      | .bitand | .bitor | .bitxor | .shl | .shr =>
        some (free lt || free rt || (isInteger lt && isInteger rt && agree lt rt))
      | _ => none
    match ok? with
    | some false =>
      some [{ fnName, message := s!"operator is not defined on '{tyToStr lt}' and '{tyToStr rt}' after monomorphization: arithmetic takes two operands of one numeric type (bitwise, shift, wrapping and saturating operators two of one integer type), and pointer arithmetic needs trusted or `Unsafe` code" }]
    | _ => none
  | _ => none

/-- **Post-Mono operator verifier** (bug 211): see `operatorTypeHook`. Generic definitions are
    skipped; only their instances are checked. -/
partial def verifyOperatorTypes (modules : List CModule) : Diagnostics :=
  let rec goModule (m : CModule) : List VerifyViolation :=
    let noTy : Ty → Bool := fun _ => false
    let fnVs := m.functions.foldl (fun acc fn =>
      if !fn.typeParams.isEmpty then acc
      else fn.body.foldl (fun acc2 st =>
        acc2 ++ collectStmtViolations fn.name noTy "" st
          (hook := operatorTypeHook fn.name (Capabilities.capsAllowUnsafeOp fn.isTrusted fn.capSet))) acc) []
    fnVs ++ m.submodules.foldl (fun acc sub => acc ++ goModule sub) []
  let violations := modules.foldl (fun acc m => acc ++ goModule m) []
  violations.map fun v => {
    severity := .error
    message := s!"{v.fnName}: {v.message}"
    pass := "post-mono"
    span := none
    hint := some "a generic arithmetic or bitwise operator instantiated at a non-numeric type: instantiate at a numeric type, or compute through a named function of the type"
    code := "E0501"
  }

/-- Bug 092 (post-mono): does a value of this type store a reference inside an
    aggregate (array, `Heap`, generic argument)? A bare `&T` is a reference, not
    an aggregate; raw pointers and function types are not looked through. Same
    rule as Check's `tyStoresRef` / `tyHoldsRefInAggregate`. -/
private partial def tyStoresRefV : Ty → Bool
  | .ref _ | .refMut _ => true
  | .heap inner | .heapArray inner => tyStoresRefV inner
  | .array elem _ => tyStoresRefV elem
  | .generic _ args _ => args.any tyStoresRefV
  | _ => false

private partial def tyHoldsRefInAggregateV : Ty → Bool
  | .ref inner | .refMut inner => tyHoldsRefInAggregateV inner
  | .heap inner | .heapArray inner => tyStoresRefV inner
  | .array elem _ => tyStoresRefV elem
  | .generic _ args _ => args.any tyStoresRefV
  | _ => false

/-- **Stored-reference verifier** (bug 092, post-mono). Check rejects a
    reference stored in an aggregate wherever the type is written, but a generic
    body instantiated at `T = &U` (`Box<T>`, `[T; 2]`, `Heap<T>`) only shows the
    reference after monomorphization. Every instantiated struct/enum field, and
    every parameter, binding and expression type of an instantiated function, is
    checked here, as are module constants (their type and initializer). Extern
    (FFI) signatures are not: they are raw by contract. -/
-- spec: REF-3, spec: REF-4 (docs/spec/references.md; this declaration enforces the rule)
partial def verifyNoStoredRefs (modules : List CModule) : Diagnostics :=
  let rec goModule (m : CModule) : List VerifyViolation :=
    let label := "a reference stored in an aggregate after monomorphization"
    let fnVs := m.functions.foldl (fun acc fn =>
      acc ++ verifyFnTypes tyHoldsRefInAggregateV label fn (skipGenerics := true)) []
    let structVs := m.structs.foldl (fun acc sd =>
      if !sd.typeParams.isEmpty then acc else
      sd.fields.foldl (fun acc2 (fname, t) =>
        acc2 ++ if tyStoresRefV t then [{ fnName := sd.name, message := s!"{label}: struct field '{fname}' type {tyToStr t}" }] else []) acc) []
    let enumVs := m.enums.foldl (fun acc ed =>
      if !ed.typeParams.isEmpty then acc else
      ed.variants.foldl (fun acc2 (vname, fields) =>
        fields.foldl (fun acc3 (fname, t) =>
          acc3 ++ if tyStoresRefV t then [{ fnName := s!"{ed.name}::{vname}", message := s!"{label}: enum field '{fname}' type {tyToStr t}" }] else []) acc2) acc) []
    -- Module constants: the declared type and every initializer subexpression.
    let constVs := m.constants.foldl (fun acc (name, ty, value) =>
      -- a constant is storage: a bare reference type counts (bug 123)
      acc ++ (if tyStoresRefV ty
              then [{ fnName := name, message := s!"{label} in constant type: {tyToStr ty}" }] else [])
          ++ collectExprViolations name tyHoldsRefInAggregateV label value) []
    fnVs ++ structVs ++ enumVs ++ constVs ++ m.submodules.foldl (fun acc sub => acc ++ goModule sub) []
  let violations := modules.foldl (fun acc m => acc ++ goModule m) []
  violations.map fun v => {
    severity := .error
    message := s!"{v.fnName}: {v.message}"
    pass := "post-mono"
    span := none
    hint := some "references are second-class: a generic type parameter instantiated with a reference may not be stored in a struct, enum, array or generic container (docs/language/VALUE_MODEL.md)"
    code := "E0239"
    -- exit 2i §15.5: an instantiation storing a reference is REF-4@1 (b); a constant is REF-3@1
    evidence := let r := if v.message.startsWith "a reference stored in an aggregate after monomorphization in constant" then "REF-3@1" else "REF-4@1"
                [("rule", r), ("spec", ruleSpecRef r)]
    nextAction := some "instantiate the generic at a value type, or pass the reference directly as an argument"
  }

/-- **Post-mono capability verifier** (bug 091, defense in depth). Capability
    discipline is decided before monomorphization: Check decides a call through a trait
    bound against the TRAIT method's declared set, and CoreCheck bounds every impl
    method by that set (E0586). Mono then turns `T$method` into a direct call to the
    impl's body and nothing re-asked the question — which is how an impl's extra
    `with(Std)` once ran under a caller with none.
    This re-asks it on the specialized call graph: every direct call in a non-generic
    function must require only capabilities its caller declares. Deliberately
    conservative, so it can only ever report a real violation:
      * `Unsafe` is excluded — its trusted-body discharge is CoreCheck's decision and
        this pass does not see trust provenance;
      * a set naming a capability VARIABLE or a capability parameter (any name outside
        `validCaps`) is skipped, on either side — polymorphic authority is resolved by
        Check's inference, not by this structural re-check;
      * a callee is resolved only when every definition with that name, across all
        modules, declares the same set (no guessing between homonyms).
    A violation here means a front-end decision was wrong, so it is an internal error. -/
partial def verifyCapsPostMono (modules : List CModule) : Diagnostics :=
  let rec allFns (ms : List CModule) : List CFnDef :=
    ms.foldl (fun acc m => acc ++ m.functions ++ allFns m.submodules) []
  let fns := allFns modules
  -- `none` = not a plain concrete set this pass can decide on.
  let plainCaps : CapSet → Option (List String) := fun cs =>
    let (caps, vars) := cs.normalize
    if !vars.isEmpty || caps.any (fun c => !validCaps.contains c) then none else some caps
  -- Definitions by name, built once: a per-call scan of every function was quadratic in
  -- program size (self-review of bug 091).
  let byName : Std.HashMap String (List CFnDef) :=
    fns.foldl (fun m f => m.insert f.name (f :: (m.getD f.name []))) {}
  -- A callee spelled by an import alias names no definition directly (Codex round 2 of bug
  -- 091: Mono kept a nested destructor's `Type_destroy` alias spelling and this lookup
  -- found nothing). Resolve it through EVERY alias any module declares for that spelling
  -- (one more hop each, as Mono's `lookupFn` does), and decide only when all candidate
  -- definitions agree — never guess which module's alias the backend will pick.
  let rec allAliases (ms : List CModule) : List (String × String) :=
    ms.foldl (fun acc m => acc ++ m.linkerAliases ++ allAliases m.submodules) []
  let aliasTargets : Std.HashMap String (List String) :=
    (allAliases modules).foldl (fun m (l, t) => m.insert l (t :: (m.getD l []))) {}
  let definitionsOf : String → List CFnDef := fun callee =>
    match byName.getD callee [] with
    | [] =>
      let targets := aliasTargets.getD callee []
      let hops := targets ++ targets.flatMap (aliasTargets.getD · [])
      hops.eraseDups.flatMap (byName.getD · [])
    | ds => ds
  let required : String → CallSpace → Option (List String) := fun callee sp =>
    let defs := definitionsOf callee
    let inSpace := defs.filter fun d => d.isMethod == (sp == .method)
    match (if inSpace.isEmpty then defs else inSpace) with
    | [] => none
    | d :: ds =>
      if !d.typeParams.isEmpty || ds.any (fun d' => d'.capSet.normalize != d.capSet.normalize) then none
      else (plainCaps d.capSet).map (·.filter (· != unsafeCapName))
  let violations := fns.foldl (fun acc f =>
    if !f.typeParams.isEmpty then acc else
    match plainCaps f.capSet with
    | none => acc
    | some has =>
      -- Per namespace (bugs 238 – 239): a free call its function binds to a foreign declaration
      -- is that declaration (CoreCheck decides it), and otherwise each call is decided against
      -- the definitions of its namespace, a method call against methods only.
      let directs := ((collectCalleesStmts f.body).filterMap fun c => match c with
        | .direct n sp => some (n, sp)
        | _ => none).eraseDups
      let decided := directs.filter fun (n, sp) =>
        !(sp == .free && (f.foreignCalls.lookup n).any (fun d => !d.startsWith "_C$rt."))
      decided.foldl (fun acc (callee, sp) =>
        match required callee sp with
        | none => acc
        | some need =>
          let missing := need.filter (fun c => !has.contains c)
          if missing.isEmpty then acc
          else acc ++ [{ fnName := f.name
                         message := s!"call to '{callee}' requires capabilities ({String.intercalate ", " missing}) that '{f.name}' does not declare — a capability reached a caller through monomorphization without a front-end decision" }]) acc) []
  violationsToDiagnostics "post-mono" violations
/-! ### Post-mono reference-binding verifier (exit 2i, REF-6@1, MEMORY_SEMANTICS §15.3)

Check refuses every `let` of reference type (E0302) and every assignment to a reference binding
(E0303) it can see. A generic body is checked once at its opaque `T`, so `let y: T = x;` and
`x = y;` pass there and become a reference `let` / a reference re-pointing only when Mono
instantiates `T := &Int`. This pass re-asks both questions on every specialized (non-generic)
function after monomorphization, with the binding types Mono substituted. Its environment is keyed
by binding identity (`CLocal.key`, R-0492 step 4), so an assignment finds the type of the binding
it writes, not of whichever binding last used the spelling.

Bug 269: the environment was an association list searched per assignment, so a function of n
bindings and m assignments took time n·m to verify; it is a persistent map keyed by binding
identity, where a later binding of a key replaces the earlier one exactly as the list's
newest-first `lookup` found it. -/

private abbrev RefBindEnv := Std.TreeMap LocalKey Ty LocalKey.cmp

private def isRefTyPM : Ty → Bool
  | .ref _ | .refMut _ => true
  | _ => false

mutual
private partial def refBindExpr (env : RefBindEnv) : CExpr → List (String × String)
  | .binOp _ l r _ | .arrayIndex l r _ | .allocCall l r _ => refBindExpr env l ++ refBindExpr env r
  | .unaryOp _ x _ | .fieldAccess x _ _ | .borrow x _ | .borrowMut x _ | .deref x _ | .cast x _ =>
    refBindExpr env x
  | .call _ _ args _ | .arrayLit args _ => args.flatMap (refBindExpr env)
  | .structLit _ _ fs _ | .enumLit _ _ _ fs _ => fs.flatMap (fun (_, e) => refBindExpr env e)
  | .match_ s arms _ => refBindExpr env s ++ arms.flatMap (refBindArm env)
  | .ifExpr c t e _ => refBindExpr env c ++ refBindStmts env t ++ refBindStmts env e
  | _ => []

private partial def refBindArm (env : RefBindEnv) : CMatchArm → List (String × String)
  | .enumArm _ _ bs g body =>
    let env' := bs.foldl (fun env (b, t) => env.insert b.key t) env
    (g.map (refBindExpr env')).getD [] ++ refBindStmts env' body
  | .varArm b t g body =>
    let env' := env.insert b.key t
    (g.map (refBindExpr env')).getD [] ++ refBindStmts env' body
  | .litArm v g body => refBindExpr env v ++ (g.map (refBindExpr env)).getD [] ++ refBindStmts env body
  | .rangeArm lo hi _ g body =>
    refBindExpr env lo ++ refBindExpr env hi ++ (g.map (refBindExpr env)).getD [] ++ refBindStmts env body

private partial def refBindStmts (env : RefBindEnv) : List CStmt → List (String × String)
  | [] => []
  | s :: rest =>
    let (vs, env') := refBindStmt env s
    vs ++ refBindStmts env' rest

/-- One statement: its violations (code, message) and the bindings visible after it. -/
private partial def refBindStmt (env : RefBindEnv) : CStmt → List (String × String) × RefBindEnv
  | .letDecl n _ ty v =>
    -- `__destr_*` is Elab's struct-destructuring temporary (a `let S { .. } = s` through `&S`
    -- binds it to the reference); it is not a source `let`, whose binders Check already saw.
    (refBindExpr env v ++
      (if isRefTyPM ty && !n.name.startsWith "__destr_" then [("E0302", s!"a `let` binding may not hold a reference after monomorphization: '{n}' has type '{tyToStr ty}'")] else []),
     env.insert n.key ty)
  | .assign n v =>
    (refBindExpr env v ++
      (match env.get? n.key with
       | some t => if isRefTyPM t then [("E0303", s!"a reference binding is never assigned (after monomorphization): '{n}' has type '{tyToStr t}'")] else []
       | none => []), env)
  | .return_ v _ | .break_ v _ => ((v.map (refBindExpr env)).getD [], env)
  | .expr e _ | .defer e => (refBindExpr env e, env)
  | .ifElse c t e => (refBindExpr env c ++ refBindStmts env t ++ ((e.map (refBindStmts env)).getD []), env)
  | .while_ c body _ step => (refBindExpr env c ++ refBindStmts env body ++ refBindStmts env step, env)
  | .fieldAssign o _ v | .derefAssign o v => (refBindExpr env o ++ refBindExpr env v, env)
  | .arrayIndexAssign a i v => (refBindExpr env a ++ refBindExpr env i ++ refBindExpr env v, env)
  | .continue_ _ => ([], env)
  | .borrowIn _ ref _ _ refTy body => (refBindStmts (env.insert ref.key refTy) body, env)
end

/-- REF-6@1 after monomorphization: E0302 for a `let` and E0303 for an assignment whose binding
    type became a reference by instantiating a type parameter. Generic originals are skipped
    (their `T` is opaque; Check saw them). -/
-- spec: REF-6 (docs/spec/references.md; this declaration enforces the rule)
partial def verifyNoRefBindingsPostMono : List CModule → Diagnostics
  | [] => []
  | m :: ms =>
    let here := m.functions.foldl (fun acc fn =>
      if !fn.typeParams.isEmpty then acc else
      let params : RefBindEnv := (fn.paramKeys.zip (fn.params.map (·.2))).foldl (fun env (k, t) => env.insert k t) {}
      acc ++ (refBindStmts params fn.body).map fun (code, msg) =>
        ({ severity := .error
           message := s!"function '{fn.name}': {msg}"
           pass := "post-mono"
           span := fn.declSpan
           hint := some "references are second-class: a type parameter instantiated to a reference may not be bound by `let` or assigned (docs/spec/references.md#REF-6)"
           code := code
           evidence := [("rule", "REF-6@1"), ("spec", ruleSpecRef "REF-6@1")]
           nextAction := some "instantiate the generic at a value type, or restructure the generic so it passes its `T` argument on instead of binding or assigning it" } : Diagnostic)) ([] : Diagnostics)
    here ++ verifyNoRefBindingsPostMono m.submodules ++ verifyNoRefBindingsPostMono ms

/-- **Post-Mono verifier**: no `Ty.typeVar` survives monomorphization, Copy struct
    fields validate after instantiation, and no SAFE function exposes a
    reference-return type (references are second-class), and every integer
    literal fits its now-concrete type (bug 088). Mixed-width binops
    (class 3) are already re-asserted at the CoreCheck boundary (E0502) — see
    docs/compiler/COMPILER_BOUNDARY.md. -/
-- spec: PASS-7 (docs/spec/pipeline.md; this stage boundary)
def verifyPostMono (modules : List CModule) : Diagnostics :=
  verifyNoTypeVars modules ++ verifyCopyFieldsPostMono modules ++ verifyNoReturnedRefs modules
    ++ verifyLiteralRanges modules ++ verifyNoStoredRefs modules
    ++ verifyComparisonTypes modules
    ++ verifyOperatorTypes modules
    ++ verifyCapsPostMono modules ++ verifyNoRefBindingsPostMono modules

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
