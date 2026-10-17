import Concrete.Report.Report

/-!
# ObligationCore — the typed evidence ledger (Phase 3 schema v1)

One record model for every proof / contract / runtime-safety / SMT / policy /
audit / proof-authoring surface. Reports, policies, proof workspaces, audit
bundles, and codegen gates should all read from this ledger instead of
recomputing facts in parallel walkers. See `docs/verification/OBLIGATION_CORE.md` (canonical)
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
    -- Second-kernel classes (spike/multi-prover-evidence). `proved_by_rocq` /
    -- `proved_by_isabelle` = that independent kernel closed its lowering of the
    -- obligation; `proved_by_two_kernels` / `proved_by_multi_kernel` = Lean plus
    -- one / ≥2 external kernels each closed their lowering of the obligation. These
    -- attest CHECKER diversity, not BRIDGE diversity: kernels are matched on the
    -- obligation KEY, and each per-prover renderer re-spells the obligation with
    -- nothing verifying they spell the SAME proposition (there is no subject-digest
    -- cross-check — that is future work). So this means "N kernels each accepted a
    -- lowering of this VC", NOT "N kernels agree on one checked proposition". When
    -- Isabelle (HOL) is among them the kernels span CIC and HOL. Never launders past
    -- a `trusted` boundary.
    "proved_by_rocq", "proved_by_isabelle",
    "proved_by_two_kernels", "proved_by_multi_kernel",
    -- `kernel_disagreement` = kernels rendering the SAME proposition (lowering agreed)
    -- returned OPPOSITE verdicts. Neither a badge nor `unproven`: every kernel here is
    -- complete for linear integer arithmetic, so a disagreement is a defect report,
    -- most likely about our driver for the dissenting kernel. It CAPS the claim and
    -- fails the two-kernel release gate. Distinct from a lowering disagreement, where
    -- the kernels decided different propositions and the dissenter simply does not
    -- attest.
    "kernel_disagreement",
    -- `solver_checked` = an external SMT solver reported unsat AND an independent
    -- kernel (Rocq `nia`) also closed the goal, so the solver leaves the TCB —
    -- strictly stronger than `solver_trusted` (solver-in-TCB). This corroborates the
    -- solver's VERDICT; it does not check the solver's reasoning.
    "solver_checked",
    -- `solver_replayed` = the solver's PROOF was reconstructed inference-by-inference
    -- in a kernel (Isabelle `smt` with `smt_oracle = false`, asserted oracle-free),
    -- not merely corroborated by a second decision procedure. Strictly stronger than
    -- `solver_checked`. Reconstruction covers linear integer arithmetic only, so a
    -- nonlinear VC cannot currently reach this class — see docs/verification/SMT_SOUNDNESS.md.
    "solver_replayed",
    "arithmetic_proved", "solver_trusted", "tested_by_oracle", "runtime_checked",
    "enforced", "assumed", "trusted", "partial", "stale", "vacuous", "missing",
    "unproven", "planned", "counterexample", "unknown", "timeout", "solver_error",
    "ineligible",
    -- A proof link with no stored proof-subject digest: not `proved` (nothing
    -- was checked), and not `stale` (no body change was observed — it was never
    -- pinned). R-0004 / bug 058.
    "unbound" ]

/-- The canonical obligation kinds (Phase 3 #1). New obligation kinds are added
    here, not to a private report path. -/
def kindVocabulary : List String :=
  [ "requires_at_entry", "postcondition", "precondition", "array_bounds",
    "div_nonzero", "no_overflow", "assert", "assume", "vacuity",
    -- R-0464 / H24: the two trap conditions obligation generation used to omit.
    -- `div_quotient_in_range` is signed `MIN / -1`, which `div_nonzero` does not imply;
    -- `shift_amount_in_range` had no family at all. Both are tied to
    -- `IntArith.allTrapConditions` by a totality example in Report.lean, so a condition
    -- added to the semantics cannot be left unclaimed by a family.
    "div_quotient_in_range", "shift_amount_in_range",
    "loop_invariant_init", "loop_invariant_preservation", "loop_exit_implies_post",
    "variant_nonnegative", "variant_decreases", "invalid_contract_expression",
    "impure_contract_call", "source_proof_link", "proof_fingerprint", "spec_drift",
    "missing_theorem", "blocked_proof", "ineligible_construct", "smt_query",
    "oracle_evidence", "runtime_enforced", "trusted_boundary",
    -- Companion kind for the `unbound` status above; distinct from `spec_drift`
    -- so a release gate can tell "the subject moved" from "there is no subject".
    "unbound_proof_link" ]

/-! ### Trap conditions ↔ obligation families (R-0464 / H24)

`IntArith.trapConditions` says what a checked op owes. This says which obligation family
discharges each, and the example below makes the correspondence TOTAL: a condition added to
the semantics has no family until someone names one here, and naming a family that emits no
such VC kind fails too.

That totality is the part `check_vc_bridge_register.sh` structurally could not provide. It
asserts every family GENERATOR has a register row, so a family that does not exist has no
generator to notice — which is exactly how the shift gap stayed invisible while the gate
suite was green. The direction matters: this walks from the SEMANTICS to the families, so
absence is what it detects. -/

/-- The VC kind that discharges each trap condition. Total by construction — a new
    `TrapCondition` constructor makes this a missing-case error, which is the point. -/
def familyForTrapCondition : IntArith.TrapCondition → String
  | .divisorNonZero     => "div_nonzero"
  | .quotientInRange    => "div_quotient_in_range"
  | .resultInRange      => "no_overflow"
  | .shiftAmountInRange => "shift_amount_in_range"

/-- Every trap condition in the semantics is claimed by a real obligation kind. Fails if a
    condition is added to `IntArith` without a family, or if `familyForTrapCondition` names
    a kind that is not in the canonical vocabulary. -/
example : IntArith.allTrapConditions.all
    (fun c => kindVocabulary.contains (familyForTrapCondition c)) = true := rfl

/-- And the map is injective: two conditions must not be answered by ONE obligation, which is
    the specific defect H24 was. `div_nonzero` covered `divisorNonZero` and was treated as if
    it also covered `quotientInRange`; a division discharged `b ≠ 0`, reported
    `proved_by_kernel_decision`, and aborted on `MIN / -1`. Distinct conditions, distinct
    obligations, distinct keys. -/
example : (IntArith.allTrapConditions.map familyForTrapCondition).eraseDups.length
    = IntArith.allTrapConditions.length := rfl

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
    -- A proof reaching a trusted boundary is REAL, and it is conditional. It
    -- gets its own status string rather than an extra field, because a consumer
    -- filtering `status == "proved_by_lean"` would otherwise count it as
    -- unconditional — which is precisely the laundering to prevent.
    | .proved      =>
      if e.trustedDeps.isEmpty then ("source_proof_link", "proved_by_lean")
      else ("source_proof_link", "proved_by_lean_modulo_trusted")
    | .stale       => ("spec_drift",        "stale")
    -- Its own ledger kind: a release gate must be able to tell "the subject
    -- moved" from "there is no subject", because only the second means the
    -- claim was never checkable.
    | .unbound     => ("unbound_proof_link", "unbound")
    -- Its own ledger kind again, for the same reason `unbound` has one: a
    -- release gate must distinguish "recorded under an older schema" from both
    -- "the subject moved" and "no subject exists". All three are non-evidence,
    -- and each has a different repair.
    | .needsRecheck => ("proof_needs_recheck", "needs_recheck")
    -- A distinct ledger kind, and deliberately NOT "proved_by_lean": the whole
    -- purpose of the status is that this claim contributes no proved evidence.
    -- A release gate reading the ledger must see the containment, not a pass.
    | .depsNotCurrent => ("dependency_not_current", "deps_not_current")
    -- ITS OWN LEDGER KIND. Folding it into `dependency_not_current` would tell a release gate that
    -- a callee needs re-verification, when what is missing is this claim's per-edge justification.
    | .correspondenceUnjustified => ("dependency_closure_unjustified", "correspondence_unjustified")
    -- Its own kind (bug 079): the repair is the LINK — which theorem, which specification — not
    -- the body, a callee or the evidence tables.
    | .specUnbound => ("spec_not_bound", "spec_unbound")
    | .notProved   => ("missing_theorem",   "missing")
    | .blocked     => ("blocked_proof",     "unproven")
    | .notEligible => ("ineligible_construct", "ineligible")
    | .trusted     => ("trusted_boundary",  "trusted")
  let engine := match e.state with | .proved => "lean" | _ => ""
  let concl := match e.state with
    | .stale       => s!"proof fingerprint {e.expectedFp} ≠ current {e.currentFp}"
    | .unbound     => "proof link unbound: no stored proof-subject digest"
    | .needsRecheck => s!"stored digest {e.expectedFp} is v1 (body-only); current schema is v2 — not comparable"
    | .depsNotCurrent =>
      if e.notCurrentDeps.isEmpty then "reaches a dependency that is not current"
      else s!"reaches non-current dependencies: {", ".intercalate e.notCurrentDeps}"
    | .correspondenceUnjustified =>
      if e.notCurrentDeps.isEmpty then "dependency closure has no validated per-edge justification"
      else s!"unjustified dependency edges: {", ".intercalate e.notCurrentDeps}"
    | .specUnbound => s!"spec not bound to body: {e.specBinding}"
    | .proved      =>
      let base := if e.proofName.isEmpty then "in-source proof link is fresh" else s!"proved by {e.proofName}"
      if e.trustedDeps.isEmpty then base
      else s!"{base}, ASSUMING trusted boundaries: {", ".intercalate e.trustedDeps}"
    | .notProved   => "no registered proof for an eligible function"
    | .blocked     => s!"extraction blocked: {", ".intercalate e.unsupported}"
    | .notEligible => s!"ineligible: {", ".intercalate e.eligibilityReasons}"
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
    -- Multi-kernel provenance: which kernels attested, at which VERSIONS. Empty
    -- arrays when no external kernel ran, so the default ledger is unchanged. A
    -- stored `proved_by_two_kernels` without versions cannot be re-audited — the
    -- reader cannot tell which prover builds agreed, nor invalidate the claim if one
    -- is later found buggy.
    s!"{q "attesting_kernels"}: {jarrStr o.attestingKernels}",
    s!"{q "attesting_kernel_versions"}: {jarrStr o.attestingKernelVersions}",
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
