import Concrete.Core
import Concrete.Diagnostic
import Concrete.ProofCore
import Concrete.Report

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

instance : Inhabited ProjectPolicy := ⟨{}⟩

/-- True when no policy constraints are set. -/
def ProjectPolicy.isEmpty (p : ProjectPolicy) : Bool :=
  !p.predictable && p.deny.isEmpty && !p.requireProofs

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
    | _ => none

/-- Enforce policy constraints on compiled modules. Returns diagnostics for violations.
    Runs after CoreCheck (on ValidatedCore) so all type information is available. -/
def enforcePolicy (policy : ProjectPolicy) (modules : List CModule)
    (locMap : Report.FnLocMap := []) (pc : ProofCore)
    (depNames : List String := []) : Diagnostics :=
  if policy.isEmpty then [] else
  let projectModules := modules.filter fun m => !depNames.contains m.name
  let ds1 := if policy.predictable then enforcePredictable projectModules pc locMap else []
  let ds2 := if !policy.deny.isEmpty then enforceDeny projectModules policy.deny else []
  let ds3 := if policy.requireProofs then enforceRequireProofs (pc.scopeToUser depNames) else []
  ds1 ++ ds2 ++ ds3

end Concrete
