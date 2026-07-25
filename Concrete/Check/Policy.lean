import Concrete.Elab.Core
import Concrete.Report.Diagnostic
import Concrete.Proof.ProofCore
import Concrete.Report.Report

namespace Concrete

/-! ## Policy — package-level compilation constraints

Parses `[policy]` from Concrete.toml and enforces constraints as compile errors.
Policies make the compiler's thesis properties (predictable execution, capability
discipline, proof requirements) enforceable at the package level rather than
only observable via `--report` and `--check`.
-/

-- ============================================================
-- Policy types
-- ============================================================

/-- Package-level policy parsed from `[policy]` in Concrete.toml. -/
structure ProjectPolicy where
  /-- Enforce predictable-execution profile (no recursion, alloc, FFI, blocking). -/
  predictable : Bool := false
  /-- Capabilities that are forbidden in this package. -/
  deny : List String := []
  /-- Require proofs for all eligible functions. -/
  requireProofs : Bool := false
  /-- Forbid `assume(...)` — the escape hatch is off-limits in this profile. -/
  forbidAssume : Bool := false
  /-- Release stance on external-solver (SMT) evidence. `""` = unset (no SMT
      enforcement); `"forbid"` = `solver_trusted` is not acceptable for release;
      `"allow"` = accepted; `"assumptions"` = accepted only with a named
      `solver-assumption`. An external solver is NOT Lean/kernel evidence unless
      replayed, so a release must opt into trusting it. -/
  solverEvidence : String := ""
  /-- Named provenance/assumption justifying `solver_trusted` under the
      `"assumptions"` stance (e.g. "z3-4.16-QF_NIA-trusted"). -/
  solverAssumption : String := ""

instance : Inhabited ProjectPolicy := ⟨{}⟩

/-- True when no policy constraints are set. -/
def ProjectPolicy.isEmpty (p : ProjectPolicy) : Bool :=
  !p.predictable && p.deny.isEmpty && !p.requireProofs && !p.forbidAssume
    && p.solverEvidence.isEmpty

-- ============================================================
-- TOML parsing
-- ============================================================

private def parseDenyList (line : String) : List String :=
  -- Extract strings between quotes in: deny = ["Unsafe", "Network"]
  let parts := line.splitOn "\""
  let rec extract (ps : List String) (inside : Bool) (acc : List String) : List String :=
    match ps with
    | [] => acc
    | p :: rest =>
      if inside then extract rest false (acc ++ [p])
      else extract rest true acc
  match parts with
  | _ :: rest => extract rest true []
  | [] => []

private def parseValue (line : String) : String :=
  match line.splitOn "=" with
  | _ :: rest => ("=".intercalate rest).trimAscii.toString
  | [] => ""

/-- Parse the `[policy]` section from Concrete.toml content.
    Returns (policy, warnings) — warnings are non-empty for unrecognized keys. -/
def parsePolicy (content : String) : ProjectPolicy × List String :=
  let lines := content.splitOn "\n"
  let rec go (ls : List String) (inPolicy : Bool) (p : ProjectPolicy)
      (warns : List String) : ProjectPolicy × List String :=
    match ls with
    | [] => (p, warns)
    | l :: rest =>
      let trimmed := l.trimAscii.toString
      if trimmed.startsWith "[policy]" then
        go rest true p warns
      else if trimmed.startsWith "[" then
        go rest false p warns
      else if !trimmed.isEmpty && inPolicy && !trimmed.startsWith "#" then
        if trimmed.startsWith "predictable" then
          go rest true { p with predictable := parseValue trimmed == "true" } warns
        else if trimmed.startsWith "require-proofs" then
          go rest true { p with requireProofs := parseValue trimmed == "true" } warns
        else if trimmed.startsWith "forbid-assume" then
          go rest true { p with forbidAssume := parseValue trimmed == "true" } warns
        else if trimmed.startsWith "solver-evidence" then
          go rest true { p with solverEvidence := (parseValue trimmed).replace "\"" "" } warns
        else if trimmed.startsWith "solver-assumption" then
          go rest true { p with solverAssumption := (parseValue trimmed).replace "\"" "" } warns
        else if trimmed.startsWith "deny" then
          go rest true { p with deny := parseDenyList trimmed } warns
        else
          let key := match trimmed.splitOn "=" with
            | k :: _ => k.trimAscii.toString
            | [] => trimmed
          go rest true p
            (warns ++ [s!"warning: Concrete.toml [policy]: unrecognized key '{key}'"])
      else
        go rest inPolicy p warns
  go lines false {} []

-- ============================================================
-- Enforcement
-- ============================================================

private partial def collectModuleFnDefs (m : CModule) : List CFnDef :=
  m.functions ++ m.submodules.foldl (fun acc s => acc ++ collectModuleFnDefs s) []

private def enforcePredictable (projectModules : List CModule) (pc : ProofCore)
    (locMap : Report.FnLocMap) : Diagnostics :=
  let recMap := pc.recMap
  let externNames := pc.externNames
  let violations := projectModules.foldl (fun acc m =>
    acc ++ Report.checkPredictableModule recMap externNames locMap m) []
  violations.map fun v =>
    let file := match v.loc with | some (f, _) => f | none => ""
    { severity := .error
      message := s!"policy violation: {v.reason}"
      pass := "policy"
      span := v.violationSpan
      hint := if v.hint.isEmpty then some "required by [policy] predictable = true" else some v.hint
      code := "E0610"
      file := file
      context := [s!"in function '{v.fnName}'"] }

private def enforceDeny (projectModules : List CModule) (denyCaps : List String) : Diagnostics :=
  projectModules.foldl (fun ds m =>
    ds ++ (collectModuleFnDefs m).foldl (fun ds2 f =>
      let (concreteCaps, _) := f.capSet.normalize
      ds2 ++ concreteCaps.filterMap fun cap =>
        if denyCaps.contains cap then
          some { severity := .error
                 message := s!"policy violation: capability '{cap}' is denied"
                 pass := "policy"
                 span := none
                 hint := some s!"remove with({cap}) or change [policy] deny list"
                 code := "E0611"
                 file := ""
                 context := [s!"in function '{f.name}'"] }
        else none
    ) []
  ) []

private def enforceRequireProofs (pc : ProofCore) : Diagnostics :=
  pc.obligations.filterMap fun o =>
    let mkDiag (msg : String) (hint : String) : Diagnostic :=
      let file := match o.loc with | some (f, _) => f | none => ""
      let span := match o.loc with
        | some (_, line) => some { line := line, col := 1, endLine := 0, endCol := 0 }
        | none => none
      { severity := .error
        message := s!"policy violation: {msg}"
        pass := "policy"
        span := span
        hint := some hint
        code := "E0612"
        file := file }
    match o.status with
    | .missing => some (mkDiag
        s!"'{o.functionId.qualName}' is proof-eligible but unproved"
        "add a Lean proof or change [policy] require-proofs")
    | .stale => some (mkDiag
        s!"'{o.functionId.qualName}' has a stale proof (fingerprint changed)"
        "update the proof to match the current function body")
    | .blocked => some (mkDiag
        s!"'{o.functionId.qualName}' is proof-eligible but extraction failed"
        "simplify the function body or change [policy] require-proofs")
    -- Fail closed: an unbound link is a claim nothing can check, which is a
    -- worse basis for a release than a stale one (a stale claim was at least
    -- pinned once). Listed explicitly rather than left to the catch-all below,
    -- which is where this state silently fell through when it was introduced.
    | .unbound => some (mkDiag
        s!"'{o.functionId.qualName}' has a proof link with no stored proof-subject digest"
        "re-verify the proof against the current body and record #[proof_fingerprint(\"...\")], or change [policy] require-proofs")
    | .proved | .ineligible | .trusted => none

/-- Reject vacuous contracts. A function whose precondition is unsatisfiable has
    a postcondition that holds only because no input ever reaches it — a
    misleading green, never a real proof. The vacuous function quals are computed
    by the caller (constant fold + omega over the source `#[requires]`), since
    contracts are AST metadata not present in Core. Rejected whenever a `[policy]`
    is configured (a vacuous contract is a release blocker by default). -/
def enforceNoVacuous (vacuousQuals : List String) : Diagnostics :=
  vacuousQuals.eraseDups.map fun q =>
    { severity := .error
      message := s!"policy violation: '{q}' has a vacuous (unsatisfiable) contract — its postcondition holds trivially and is not genuinely proved"
      pass := "policy"
      span := none
      hint := some "make the precondition satisfiable, or remove the contract"
      code := "E0613"
      file := ""
      context := [] }

/-- Reject `assume(...)` under a `forbid-assume` profile. An assume is a trust
    escape hatch — it lets a function proceed *as if* a fact held without proving
    it. A release profile can forbid the hatch entirely; every function that opens
    one is a blocker. The tainted function quals are computed by the caller (it
    walks the source AST for `assume` statements, absent from Core). -/
def enforceNoAssume (assumeQuals : List String) : Diagnostics :=
  assumeQuals.eraseDups.map fun q =>
    { severity := .error
      message := s!"policy violation: '{q}' uses assume(...) — the trust escape hatch is forbidden by [policy] forbid-assume = true"
      pass := "policy"
      span := none
      hint := some "discharge the assumption with a proof (turn it into an assert), or remove the assume"
      code := "E0614"
      file := ""
      context := [] }

/-- Release-policy gate on external-solver evidence. `solverTrustedQuals` are the
    VCs (by id) that an external solver discharged as `solver_trusted` during this
    build — solver evidence, NOT Lean/kernel evidence. The project's stance decides
    whether that is acceptable for release:
      - `forbid`      → every `solver_trusted` VC is a blocker (E0615);
      - `assumptions` → accepted only if a named `solver-assumption` is declared,
                        else a blocker;
      - `allow` / unset → accepted.
    `counterexample` / `unknown` / `timeout` / `solver_error` are non-proofs and are
    never in `solverTrustedQuals`, so they never pass this gate as evidence. -/
def enforceSolverEvidence (solverTrustedQuals : List String) (policy : ProjectPolicy) : Diagnostics :=
  let blocked (reason hint : String) : Diagnostics :=
    solverTrustedQuals.eraseDups.map fun q =>
      { severity := .error
        message := s!"policy violation: VC '{q}' is discharged only by an external solver (solver_trusted) — {reason}"
        pass := "policy"
        span := none
        hint := some hint
        code := "E0615"
        file := ""
        context := [] }
  if solverTrustedQuals.isEmpty then []
  else match policy.solverEvidence with
    | "forbid" => blocked "[policy] solver-evidence = \"forbid\" does not accept it for release"
        "prove it in Lean, or set [policy] solver-evidence = \"allow\"/\"assumptions\""
    | "assumptions" =>
      if policy.solverAssumption.isEmpty then
        blocked "[policy] solver-evidence = \"assumptions\" requires a named justification"
          "set [policy] solver-assumption = \"<provenance>\", or prove it in Lean"
      else []
    | _ => []  -- "allow" or unset

/-- Enforce policy constraints on compiled modules. Returns diagnostics for violations.
    Runs after CoreCheck (on ValidatedCore) so all type information is available. -/
def enforcePolicy (policy : ProjectPolicy) (modules : List CModule)
    (locMap : Report.FnLocMap := []) (pc : ProofCore)
    (depNames : List String := []) (vacuousQuals : List String := [])
    (assumeQuals : List String := []) (solverTrustedQuals : List String := []) : Diagnostics :=
  if policy.isEmpty then [] else
  let projectModules := modules.filter fun m => !depNames.contains m.name
  let ds1 := if policy.predictable then enforcePredictable projectModules pc locMap else []
  let ds2 := if !policy.deny.isEmpty then enforceDeny projectModules policy.deny else []
  let ds3 := if policy.requireProofs then enforceRequireProofs (pc.scopeToUser depNames) else []
  -- vacuous contracts are rejected whenever any policy is set (release default).
  let ds4 := enforceNoVacuous vacuousQuals
  let ds5 := if policy.forbidAssume then enforceNoAssume assumeQuals else []
  let ds6 := enforceSolverEvidence solverTrustedQuals policy
  ds1 ++ ds2 ++ ds3 ++ ds4 ++ ds5 ++ ds6

end Concrete
