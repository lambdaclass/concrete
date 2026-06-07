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

/-- ObligationCore schema — the one typed obligation record (Phase 3 #18d). It is
    now an `abbrev` of `Report.Obligation`: there is a SINGLE record type, hosted
    in `Report` (where `collectVCs` lives, since `ObligationCore` imports
    `Report`), and both `Report.VC` and this name refer to it. The ledger-view
    fields (`variables`, `allowedEngines`, `replay`, `policyImpact`) live on that
    one record; reports read whichever fields they need. Field names follow the
    record: `fn` (not `function`), `arithProfile` (not `semanticProfile`). -/
abbrev Obligation := Report.Obligation

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

/-- Enrich an obligation with the ledger-view fields (Phase 3 #18d). Since the
    record is now unified, this is no longer a type conversion — it only fills the
    derived view fields (`allowedEngines`/`replay`/`policyImpact`) from the VC
    surface already present, leaving every VC field untouched. So a VC report
    rendering the result is byte-identical (it never reads the view fields), and
    the obligation-ledger sees the same derived values it did before. -/
def ofVC (v : Report.VC) : Obligation :=
  { v with
    allowedEngines := enginesFor v.arithProfile,
    replay := if v.smtQuery.isEmpty then "" else s!"z3 -T:5 vc.smt2 (smtlib-sha {v.smtHash})",
    policyImpact := policyImpactOf v.status }

/-- The current ObligationCore ledger: the discharged VC families enriched with
    the view fields. Contract-clause diagnostics ride in as VCs (Phase 3 #10);
    proof-link freshness is projected separately by `ofProofStatus` (#11). -/
def ledgerOfVCs (vcs : List Report.VC) : List Obligation := vcs.map ofVC

/-- `ofVC` only enriches the view fields; the VC surface is untouched, so a VC
    report rendering an enriched obligation is byte-identical to rendering the
    original (Phase 3 #18d). -/
example (v : Report.VC) : (ofVC v).fn = v.fn
    ∧ (ofVC v).arithProfile = v.arithProfile
    ∧ (ofVC v).status = v.status
    ∧ (ofVC v).counterexample = v.counterexample
    ∧ (ofVC v).smtQuery = v.smtQuery := ⟨rfl, rfl, rfl, rfl, rfl⟩

/-- Project a proof-link freshness entry into ObligationCore (Phase 3 #11). The
    proof-status model (proved / stale / missing / blocked / ineligible / trusted)
    becomes first-class ledger obligations instead of a separate report: a fresh
    link is `source_proof_link` (proved_by_lean), a fingerprint mismatch is
    `spec_drift` (stale), and so on — so a release gate can read proof staleness
    from the same ledger as the runtime/contract obligations. -/
def ofProofStatus (e : Report.ProofStatusEntry) : Obligation :=
  let (kind, status) := match e.state with
    | .proved      => ("source_proof_link", "proved_by_lean")
    | .stale       => ("spec_drift",        "stale")
    | .notProved   => ("missing_theorem",   "missing")
    | .blocked     => ("blocked_proof",     "unproven")
    | .notEligible => ("ineligible_construct", "ineligible")
    | .trusted     => ("trusted_boundary",  "trusted")
  let engine := match e.state with | .proved => "lean" | _ => ""
  let concl := match e.state with
    | .stale       => s!"proof fingerprint {e.expectedFp} ≠ current {e.currentFp}"
    | .proved      => if e.proofName.isEmpty then "in-source proof link is fresh" else s!"proved by {e.proofName}"
    | .notProved   => "no registered proof for an eligible function"
    | .blocked     => s!"extraction blocked: {", ".intercalate e.unsupported}"
    | .notEligible => s!"ineligible: {", ".intercalate e.profileGates}"
    | .trusted     => "trusted boundary (proof bypassed)"
  { id := s!"{e.qualName}#prooflink", kind := kind, fn := e.qualName,
    file := (e.loc.map (·.1)).getD "", line := (e.loc.map (·.2)).getD 0,
    origin := if e.origin.isEmpty then "proof link" else e.origin,
    variables := [], hypotheses := [], conclusion := concl,
    arithProfile := "operational", dischargeMode := "none",
    dependencies := if e.proofName.isEmpty then [] else [e.proofName],
    allowedEngines := if e.state matches .proved then ["lean"] else [],
    status := status, engine := engine, policyImpact := policyImpactOf status }

/-- Project the proof-link freshness entries into the ledger (Phase 3 #11). -/
def proofLinkLedger (entries : List Report.ProofStatusEntry) : List Obligation :=
  entries.map ofProofStatus

/-! ### Policy projections (Phase 3 #14)

Release policy reads its inputs from the one ledger instead of a parallel
`compute*Quals` side channel. Each projection filters the ledger for the
obligations a policy acts on; the policy logic (`enforceNoVacuous` /
`enforceNoAssume` / `enforceSolverEvidence`) is unchanged. -/

/-- Functions with a vacuous (unsatisfiable) precondition — a proved
    `#requires_vac` vacuity obligation, whether the constant folder or omega
    decided it. Feeds `forbid-vacuous` (E0613). -/
def vacuousFunctions (obs : List Obligation) : List String :=
  (obs.filter fun o =>
    o.kind == "vacuity" && o.status == "proved_by_kernel_decision"
      && o.id.endsWith "#requires_vac").map (·.fn) |>.eraseDups

/-- Functions that open an `assume(...)` escape hatch — an `assume` obligation in
    the ledger. Feeds `forbid-assume` (E0614). -/
def assumeFunctions (obs : List Obligation) : List String :=
  (obs.filter (·.kind == "assume")).map (·.fn) |>.eraseDups

/-- VC ids discharged ONLY by an external solver (`solver_trusted`) — solver
    evidence, never kernel/Lean (Lean-replayed VCs are `proved_by_lean_replay`,
    not `solver_trusted`, so they are excluded). Feeds `solver-evidence` (E0615). -/
def solverTrustedIds (obs : List Obligation) : List String :=
  (obs.filter (·.status == "solver_trusted")).map (·.id) |>.eraseDups

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
    s!"{q "id"}: {q o.id}", s!"{q "kind"}: {q o.kind}", s!"{q "function"}: {q o.fn}",
    s!"{q "loc"}: \{{q "file"}: {q o.file}, {q "line"}: {o.line}}",
    s!"{q "origin"}: {q o.origin}",
    s!"{q "variables"}: {jarrStr o.variables}",
    s!"{q "hypotheses"}: {jarrStr o.hypotheses}",
    s!"{q "conclusion"}: {q o.conclusion}",
    s!"{q "semantic_profile"}: {q o.arithProfile}",
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
  for fq in (obs.map (·.fn)).eraseDups do
    out := out ++ s!"\n\n{fq}"
    for o in obs.filter (·.fn == fq) do
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
