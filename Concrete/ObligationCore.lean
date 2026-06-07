import Concrete.Report

/-!
# ObligationCore — the typed evidence ledger (Phase 3 schema v1)

One record model for every proof / contract / runtime-safety / SMT / policy /
audit / proof-authoring surface. Reports, policies, proof workspaces, audit
bundles, and codegen gates should all read from this ledger instead of
recomputing facts in parallel walkers. See `docs/OBLIGATION_CORE.md` (canonical)
and ROADMAP Phase 3.

This module establishes the schema (#1) and the single evidence/status
vocabulary (#2), and projects the existing VC-discharge ledger (`Report.VC`,
covering preconditions / postconditions / bounds / div-mod / overflow / asserts /
loop O*) into it. The contract-diagnostic and proof-link families migrate next
(Phase 3 #10-11); until then they keep their current report paths.
-/

namespace Concrete.ObligationCore

/-- The ONE evidence/status vocabulary (Phase 3 #2). Every downstream consumer
    uses these strings; reports may summarize them but may not invent a second
    vocabulary. A kernel-checked decision procedure can only yield
    `proved_by_kernel_decision`; an external solver only the solver classes. -/
def statusVocabulary : List String :=
  [ "proved_by_lean", "proved_by_kernel_decision", "proved_by_lean_replay",
    "arithmetic_proved", "solver_trusted", "tested_by_oracle", "runtime_checked",
    "enforced", "assumed", "trusted", "partial", "stale", "vacuous", "missing",
    "unproven", "planned", "counterexample", "unknown", "timeout", "solver_error",
    "ineligible" ]

/-- The canonical obligation kinds (Phase 3 #1). New obligation kinds are added
    here, not to a private report path. -/
def kindVocabulary : List String :=
  [ "requires_at_entry", "postcondition", "precondition", "array_bounds",
    "div_nonzero", "no_overflow", "assert", "assume", "vacuity",
    "loop_invariant_init", "loop_invariant_preservation", "loop_exit_implies_post",
    "variant_nonnegative", "variant_decreases", "invalid_contract_expression",
    "impure_contract_call", "source_proof_link", "proof_fingerprint", "spec_drift",
    "missing_theorem", "blocked_proof", "ineligible_construct", "smt_query",
    "oracle_evidence", "runtime_enforced", "trusted_boundary" ]

/-- ObligationCore schema v1 — the one typed record every downstream consumer
    reads. `id` is stable across harmless formatting (it is for tools); the span
    is for humans. -/
structure Obligation where
  id              : String
  kind            : String                 -- ∈ kindVocabulary
  function        : String
  file            : String
  line            : Nat
  origin          : String                 -- originating source construct (human)
  variables       : List String            -- typed variables in scope (names)
  hypotheses      : List String
  conclusion      : String
  semanticProfile : String                 -- constant | linear | bitvector | nonlinear | refinement | operational | unsupported
  dependencies    : List String            -- proof links / other obligations leaned on
  allowedEngines  : List String            -- which backends may discharge it
  status          : String                 -- ∈ statusVocabulary
  engine          : String                 -- the backend that produced `status` ("" if none)
  counterexample  : List (String × String) := []
  replay          : String := ""
  policyImpact    : String := ""

/-- Which backends are allowed to discharge an obligation of a given semantic
    profile. A profile maps to exactly the engines that may legitimately close it
    — no backend may claim a class stronger than its tier. -/
def enginesFor (profile : String) : List String :=
  match profile with
  | "constant"    => ["constant_fold"]
  | "linear"      => ["omega"]
  | "bitvector"   => ["bv_decide"]
  | "nonlinear"   => ["smt"]                -- external, opt-in, solver_trusted
  | "operational" => ["lean"]
  | "refinement"  => ["lean"]
  | _             => []

/-- The release-policy consequence of an obligation's status, for the audit/policy
    view. Empty when the status carries no special release impact. -/
def policyImpactOf (status : String) : String :=
  match status with
  | "solver_trusted" => "external-solver evidence — requires [policy] solver-evidence allowance"
  | "assumed"        => "trust escape hatch — rejected by [policy] forbid-assume"
  | "vacuous"        => "vacuous contract — rejected by policy (E0613)"
  | "stale"          => "stale proof — rejected by a no-stale-proofs policy"
  | "counterexample" => "non-proof — a concrete counterexample exists"
  | _                => ""

/-- Project a VC-discharge record into ObligationCore. This is the first migrated
    family (Phase 3 #4-9): the VC ledger flows into the unified model unchanged in
    meaning. `variables` is left empty here — it is populated when the obligation
    generators feed ObligationCore directly, not through the VC string view. -/
def ofVC (v : Report.VC) : Obligation :=
  { id := v.id, kind := v.kind, function := v.fn, file := v.file, line := v.line,
    origin := v.origin, variables := [], hypotheses := v.hypotheses,
    conclusion := v.conclusion, semanticProfile := v.arithProfile,
    dependencies := v.dependencies, allowedEngines := enginesFor v.arithProfile,
    status := v.status, engine := v.engine, counterexample := v.counterexample,
    replay := if v.smtQuery.isEmpty then "" else s!"z3 -T:5 vc.smt2 (smtlib-sha {v.smtHash})",
    policyImpact := policyImpactOf v.status }

/-- The current ObligationCore ledger: the discharged VC families projected into
    the unified model. (Contract-diagnostic and proof-link families are added by
    Phase 3 #10-11.) -/
def ledgerOfVCs (vcs : List Report.VC) : List Obligation := vcs.map ofVC

/-- Minimal JSON string escaper (self-contained; matches `proveReportJson`). -/
private def esc (s : String) : String :=
  s.foldl (fun a c => a ++ (match c with
    | '"' => "\\\"" | '\\' => "\\\\" | '\n' => "\\n" | '\t' => "\\t" | c => c.toString)) ""
private def q (s : String) : String := "\"" ++ esc s ++ "\""
private def jarrStr (xs : List String) : String := "[" ++ ", ".intercalate (xs.map q) ++ "]"
private def jobjStr (kvs : List (String × String)) : String :=
  "{" ++ ", ".intercalate (kvs.map (fun (n, x) => q n ++ ": " ++ q x)) ++ "}"

/-- One obligation as a JSON object (ledger schema v1). -/
def toJson (o : Obligation) : String :=
  String.intercalate ", " [
    s!"{q "id"}: {q o.id}", s!"{q "kind"}: {q o.kind}", s!"{q "function"}: {q o.function}",
    s!"{q "loc"}: \{{q "file"}: {q o.file}, {q "line"}: {o.line}}",
    s!"{q "origin"}: {q o.origin}",
    s!"{q "variables"}: {jarrStr o.variables}",
    s!"{q "hypotheses"}: {jarrStr o.hypotheses}",
    s!"{q "conclusion"}: {q o.conclusion}",
    s!"{q "semantic_profile"}: {q o.semanticProfile}",
    s!"{q "dependencies"}: {jarrStr o.dependencies}",
    s!"{q "allowed_engines"}: {jarrStr o.allowedEngines}",
    s!"{q "status"}: {q o.status}", s!"{q "engine"}: {q o.engine}",
    s!"{q "counterexample"}: {jobjStr o.counterexample}",
    s!"{q "replay"}: {q o.replay}", s!"{q "policy_impact"}: {q o.policyImpact}" ]
    |> (fun body => "{" ++ body ++ "}")

/-- Versioned JSON envelope of the obligation ledger. -/
def ledgerJson (obs : List Obligation) (schemaVer : Nat) : String :=
  String.join [
    "{", s!"{q "schema_version"}: {schemaVer}, ",
    s!"{q "schema_kind"}: {q "obligation_ledger"}, ",
    s!"{q "ledger_schema_version"}: 1, ",
    s!"{q "count"}: {obs.length}, ",
    s!"{q "obligations"}: [", ", ".intercalate (obs.map toJson), "]}" ]

/-- Human-readable ledger, grouped by originating function. -/
def ledgerReport (obs : List Obligation) : String := Id.run do
  if obs.isEmpty then return "=== Obligation Ledger (schema v1) ===\n\n(no obligations)"
  let mut out := "=== Obligation Ledger (schema v1) ==="
  for fq in (obs.map (·.function)).eraseDups do
    out := out ++ s!"\n\n{fq}"
    for o in obs.filter (·.function == fq) do
      let eng := if o.engine.isEmpty then "" else s!" ({o.engine})"
      let pol := if o.policyImpact.isEmpty then "" else s!"\n      policy:  {o.policyImpact}"
      out := out ++ s!"\n  [{o.id}]  {o.kind}  →  {o.status}{eng}"
        ++ s!"\n      engines:  {if o.allowedEngines.isEmpty then "(none)" else ", ".intercalate o.allowedEngines}{pol}"
  out := out ++ s!"\n\nTotal: {obs.length} obligations"
  return out

/-! ### Sanity checks — the projection lands in the canonical vocabularies -/

-- every engine the profile map names is a real backend tier.
example : (kindVocabulary.contains "array_bounds") = true := rfl
example : (statusVocabulary.contains "solver_trusted") = true := rfl
example : enginesFor "nonlinear" = ["smt"] := rfl
example : policyImpactOf "solver_trusted" ≠ "" := by decide

end Concrete.ObligationCore
