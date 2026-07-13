-- Semantic diff / trust drift: compare two fact bundles (from --report
-- diagnostics-json) and report capability/allocation/evidence/proof-state
-- changes. A self-contained consumer of the Json leaf; used by `concrete diff`.
import Concrete.Report.Json

namespace Concrete.Report

-- ============================================================
-- Semantic diff / trust drift
-- ============================================================
-- Compare two fact bundles (from diagnostics-json) and report
-- changes in capabilities, allocation, evidence, proof state,
-- spec/proof attachment, obligation status, etc.

open Json in
/-- Extract a string from a Val, returning "" for non-strings. -/
private def valStr : Val → String
  | .str s => s
  | .num n => toString n
  | .bool b => if b then "true" else "false"
  | .null => "null"
  | .arr vs => s!"[{", ".intercalate (vs.map valStr)}]"
  | .obj _ => "{...}"

open Json in
/-- Get a field value from a JSON object as a Val. -/
private def jsonGetVal (v : Val) (key : String) : Option Val :=
  match v with
  | .obj kvs => (kvs.find? fun (k, _) => k == key).map (·.2)
  | _ => none

open Json in
/-- Render a Val to a short display string. -/
private def valDisplay : Val → String
  | .str s => s
  | .num n => toString n
  | .bool b => if b then "true" else "false"
  | .null => "null"
  | .arr vs => s!"[{", ".intercalate (vs.map valDisplay)}]"
  | .obj _ => "{…}"

/-- A single field change in a diff entry. -/
structure FieldChange where
  field : String
  oldVal : String
  newVal : String

/-- A single diff entry for one (kind, function) pair. -/
structure DiffEntry where
  kind : String
  function : String
  category : String       -- "added" | "removed" | "changed"
  changes : List FieldChange
  drift : String          -- "weakened" | "strengthened" | "neutral"

/-- Fields to compare for trust-relevant changes per fact kind. -/
private def trustFields (kind : String) : List String :=
  match kind with
  | "proof_status" => ["state", "spec", "proof", "source", "current_fingerprint"]
  | "obligation" => ["status", "spec", "proof", "source", "fingerprint"]
  | "extraction" => ["status", "eligible", "spec", "proof", "proof_core", "fingerprint"]
  | "effects" => ["capabilities", "is_pure", "allocates", "frees", "recursion",
                   "loops", "crosses_ffi", "is_trusted", "evidence"]
  | "capability" => ["capabilities", "is_pure"]
  | "unsafe" => ["has_unsafe_cap", "has_raw_pointers", "is_trusted"]
  | "alloc" => ["allocates", "frees", "defers", "potential_leak"]
  | "predictable_violation" => ["state", "reason"]
  | "traceability" => ["evidence", "extraction", "boundary", "spec", "proof", "fingerprint"]
  | "contract" => ["requires", "ensures", "invariants"]
  | _ => []

/-- Split a `∧`-joined clause field into a trimmed, non-empty clause set. -/
private def splitClauses (s : String) : List String :=
  (s.splitOn " ∧ ").map (·.trimAscii.toString) |>.filter (!·.isEmpty)

/-- Evidence level ordering for drift detection (higher = stronger). -/
private def evidenceRank (s : String) : Nat :=
  match s with
  | "proved" => 5
  | "stale" => 4
  | "enforced" => 3
  | "trusted-assumption" => 2
  | "reported" => 1
  | _ => 0

/-- Proof state ordering (higher = stronger). -/
private def proofStateRank (s : String) : Nat :=
  match s with
  | "proved" => 4
  | "stale" => 3
  | "missing" => 2
  | "ineligible" => 1
  | "trusted" => 2
  | "blocked" => 1
  | _ => 0

/-- Determine if a field change represents trust weakening. -/
private def isWeakening (kind : String) (field : String) (oldV newV : String) : Bool :=
  match kind, field with
  | "proof_status", "state" => proofStateRank newV < proofStateRank oldV
  | "obligation", "status" => proofStateRank newV < proofStateRank oldV
  | "effects", "evidence" => evidenceRank newV < evidenceRank oldV
  | "effects", "is_pure" => oldV == "true" && newV == "false"
  | "effects", "is_trusted" => oldV == "false" && newV == "true"
  | "effects", "crosses_ffi" => oldV == "false" && newV == "true"
  | "capability", "is_pure" => oldV == "true" && newV == "false"
  | "alloc", "potential_leak" => oldV == "false" && newV == "true"
  | "traceability", "evidence" => evidenceRank newV < evidenceRank oldV
  | "extraction", "status" =>
    let oldRank := match oldV with | "extracted" => 3 | "eligible_not_extractable" => 2 | "excluded" => 1 | _ => 0
    let newRank := match newV with | "extracted" => 3 | "eligible_not_extractable" => 2 | "excluded" => 1 | _ => 0
    newRank < oldRank
  -- Source-contract API stability, sound at the conjunctive clause-set level.
  | "contract", "requires" =>
    -- precondition = conjunction of clauses. A clause in NEW but not OLD
    -- STRENGTHENS the precondition → existing callers may now violate it →
    -- a breaking change (blocker).
    let oldS := splitClauses oldV
    !(splitClauses newV).all oldS.contains
  | "contract", "ensures" =>
    -- postcondition = conjunction. A clause in OLD but not NEW WEAKENS the
    -- guarantee → callers that relied on it break → a breaking change.
    let newS := splitClauses newV
    !(splitClauses oldV).all newS.contains
  | "contract", "invariants" =>
    -- a public invariant change alters the loop's guarantee surface; flag any.
    oldV != newV
  | _, _ => false

/-- Determine if a field change represents trust strengthening. -/
private def isStrengthening (kind : String) (field : String) (oldV newV : String) : Bool :=
  isWeakening kind field newV oldV

open Json in
/-- Compare two facts and return field changes. -/
private def compareFacts (kind : String) (oldFact newFact : Val) : List FieldChange :=
  let fields := trustFields kind
  fields.filterMap fun f =>
    let oldV := (jsonGetVal oldFact f).map valDisplay |>.getD "<missing>"
    let newV := (jsonGetVal newFact f).map valDisplay |>.getD "<missing>"
    if oldV == newV then none
    else some { field := f, oldVal := oldV, newVal := newV }

open Json in
/-- Build a keyed map: (kind, function) → Val for a list of facts.
    For fact kinds that allow multiple entries per function (e.g.,
    predictable_violation), the key includes a disambiguator. -/
private def keyFacts (facts : List Val) : List ((String × String) × Val) :=
  facts.filterMap fun v =>
    match jsonGetStr v "kind", jsonGetStr v "function" with
    | some k, some f =>
      -- Disambiguate multi-per-function fact kinds
      let suffix := match k with
        | "predictable_violation" => (jsonGetStr v "reason").getD ""
        | "proof_diagnostic" => (jsonGetStr v "diagnostic_kind").getD ""
        | _ => ""
      let key := if suffix.isEmpty then (k, f) else (k, f ++ ":" ++ suffix)
      some (key, v)
    | _, _ => none

open Json in
/-- Classify a newly-added fact as weakened or neutral based on its content.
    New functions with weak evidence, non-pure capabilities, FFI, or trust
    markers are real drift and should be flagged. -/
private def classifyNewFact (kind : String) (v : Val) : String :=
  match kind with
  | "predictable_violation" => "weakened"
  | "unsafe" => "weakened"
  | "effects" =>
    let ev := (jsonGetVal v "evidence").map valDisplay
    let pure := (jsonGetVal v "is_pure").map valDisplay
    let ffi := (jsonGetVal v "crosses_ffi").map valDisplay
    let trusted := (jsonGetVal v "is_trusted").map valDisplay
    let caps := (jsonGetVal v "capabilities").map valDisplay
    -- Missing fields in a new fact are suspicious — treat as weakened
    if ev.isNone || pure.isNone || ffi.isNone || trusted.isNone then "weakened"
    else if ev == some "reported" || ev == some "trusted-assumption" then "weakened"
    else if pure == some "false" then "weakened"
    else if ffi == some "true" then "weakened"
    else if trusted == some "true" then "weakened"
    else if caps != some "[]" && caps.isSome then "weakened"
    else "neutral"
  | "capability" =>
    let pure := (jsonGetVal v "is_pure").map valDisplay
    if pure.isNone then "weakened"  -- missing field is suspicious
    else if pure == some "false" then "weakened" else "neutral"
  | "alloc" =>
    let leak := (jsonGetVal v "potential_leak").map valDisplay
    if leak.isNone then "weakened"
    else if leak == some "true" then "weakened" else "neutral"
  | "proof_status" =>
    let state := (jsonGetVal v "state").map valDisplay
    if state.isNone then "weakened"
    else if state == some "missing" || state == some "ineligible" || state == some "stale" then "weakened"
    else "neutral"
  | "obligation" =>
    let status := (jsonGetVal v "status").map valDisplay
    if status.isNone then "weakened"
    else if status == some "missing" || status == some "stale" then "weakened"
    else "neutral"
  | "extraction" =>
    let status := (jsonGetVal v "status").map valDisplay
    if status.isNone then "weakened"
    else if status == some "excluded" then "weakened" else "neutral"
  | "traceability" =>
    let ev := (jsonGetVal v "evidence").map valDisplay
    if ev.isNone then "weakened"
    else if evidenceRank (ev.getD "") < 3 then "weakened" else "neutral"  -- below "enforced"
  | "contract" =>
    -- a function that gained a contract. A new precondition (`requires`) can
    -- break existing callers, so flag it; a function that only added/declared a
    -- postcondition is a compatible strengthening.
    let reqs := (jsonGetVal v "requires").map valDisplay
    if reqs.isSome && reqs != some "" then "weakened" else "neutral"
  | "eligibility" | "proof_diagnostic" => "neutral"  -- informational fact kinds
  | _ => "weakened"  -- unknown fact kind in new facts is suspicious

open Json in
/-- Find duplicate (kind, function) keys in a keyed fact list. -/
private def findDuplicateKeys (keyed : List ((String × String) × Val))
    : List (String × String) :=
  let keys := keyed.map (·.1)
  keys.foldl (fun (seen, dupes) key =>
    if seen.contains key then
      if dupes.contains key then (seen, dupes)
      else (seen, dupes ++ [key])
    else (seen ++ [key], dupes)
  ) ([], []) |>.2

open Json in
/-- Diff two fact bundles and produce a list of DiffEntries.
    Returns an error if either bundle contains duplicate (kind, function) keys. -/
def diffFacts (oldFacts newFacts : List Val) : Except String (List DiffEntry) :=
  let oldKeyed := keyFacts oldFacts
  let newKeyed := keyFacts newFacts
  -- Reject duplicate keys
  let oldDupes := findDuplicateKeys oldKeyed
  let newDupes := findDuplicateKeys newKeyed
  if !oldDupes.isEmpty then
    let desc := oldDupes.map fun (k, f) => s!"({k}, {f})"
    .error s!"duplicate keys in old bundle: {", ".intercalate desc}"
  else if !newDupes.isEmpty then
    let desc := newDupes.map fun (k, f) => s!"({k}, {f})"
    .error s!"duplicate keys in new bundle: {", ".intercalate desc}"
  else
  -- Removed: in old but not new
  let removed := oldKeyed.filterMap fun ((k, f), _) =>
    if newKeyed.find? (fun (key, _) => key == (k, f)) |>.isNone then
      some { kind := k, function := f, category := "removed"
           , changes := [], drift := "weakened" : DiffEntry }
    else none
  -- Added: in new but not old
  let added := newKeyed.filterMap fun ((k, f), v) =>
    if oldKeyed.find? (fun (key, _) => key == (k, f)) |>.isNone then
      let drift := classifyNewFact k v
      some { kind := k, function := f, category := "added"
           , changes := [], drift := drift : DiffEntry }
    else none
  -- Changed: in both, fields differ
  let changed := oldKeyed.filterMap fun ((k, f), oldV) =>
    match newKeyed.find? (fun (key, _) => key == (k, f)) with
    | none => none
    | some (_, newV) =>
      let fieldChanges := compareFacts k oldV newV
      if fieldChanges.isEmpty then none
      else
        let hasWeakening := fieldChanges.any fun fc => isWeakening k fc.field fc.oldVal fc.newVal
        let hasStrengthening := fieldChanges.any fun fc => isStrengthening k fc.field fc.oldVal fc.newVal
        let drift := if hasWeakening then "weakened"
          else if hasStrengthening then "strengthened"
          else "neutral"
        some { kind := k, function := f, category := "changed"
             , changes := fieldChanges, drift := drift : DiffEntry }
  .ok (removed ++ added ++ changed)

/-- Render a diff report as human-readable text. -/
def renderDiffReport (entries : List DiffEntry) : String :=
  if entries.isEmpty then "No trust-relevant changes detected.\n"
  else
    let header := "=== Semantic Diff / Trust Drift ==="
    -- Group by drift direction
    let weakened := entries.filter (·.drift == "weakened")
    let strengthened := entries.filter (·.drift == "strengthened")
    let neutral := entries.filter (·.drift == "neutral")
    let renderEntry (e : DiffEntry) : String :=
      let tag := match e.category with
        | "added" => "[+]"
        | "removed" => "[-]"
        | _ => "[~]"
      let changesStr := if e.changes.isEmpty then ""
        else "\n" ++ (e.changes.map fun fc =>
          s!"      {fc.field}: {fc.oldVal} → {fc.newVal}").foldl (· ++ "\n" ++ ·) ""
      s!"    {tag} {e.kind} / {e.function}{changesStr}"
    let sections := []
    let sections := if weakened.isEmpty then sections
      else sections ++ [s!"  TRUST WEAKENED ({weakened.length}):\n{"\n".intercalate (weakened.map renderEntry)}"]
    let sections := if strengthened.isEmpty then sections
      else sections ++ [s!"  TRUST STRENGTHENED ({strengthened.length}):\n{"\n".intercalate (strengthened.map renderEntry)}"]
    let sections := if neutral.isEmpty then sections
      else sections ++ [s!"  OTHER CHANGES ({neutral.length}):\n{"\n".intercalate (neutral.map renderEntry)}"]
    let summary := s!"Summary: {entries.length} changes — {weakened.length} weakened, {strengthened.length} strengthened, {neutral.length} neutral"
    s!"{header}\n\n{"\n\n".intercalate sections}\n\n{summary}\n"

open Json in
/-- Render a diff as JSON for machine consumption. -/
def renderDiffJson (entries : List DiffEntry) : String :=
  let vals := entries.map fun e =>
    Val.obj [
      ("kind", .str e.kind),
      ("function", .str e.function),
      ("category", .str e.category),
      ("drift", .str e.drift),
      ("changes", .arr (e.changes.map fun fc =>
        Val.obj [("field", .str fc.field), ("old", .str fc.oldVal), ("new", .str fc.newVal)]))
    ]
  (Val.arr vals).render

open Json in
/-- Parse a JSON string into a list of fact Vals.
    Accepts either a raw JSON array or a snapshot object with a "facts" field.
    Returns (facts, warnings) where warnings flag schema issues. -/
def parseFactsWarn (jsonStr : String) : Option (List Val) × List String :=
  match JsonParser.parse jsonStr with
  | some (.arr vs) =>
    let warnings := validateFactSchema vs
    (some vs, warnings)
  | some (.obj kvs) =>
    match kvs.find? (fun (k, _) => k == "facts") with
    | some (_, .arr vs) =>
      let warnings := validateFactSchema vs
      (some vs, warnings)
    | _ => (none, ["error: snapshot JSON object has no \"facts\" array field"])
  | some _ => (none, ["error: snapshot JSON is not an array or object"])
  | none => (none, ["error: snapshot JSON failed to parse"])
where
  /-- Validate fact entries have required fields. -/
  validateFactSchema (facts : List Val) : List String :=
    let issues := facts.foldl (fun (acc : List String × Nat) v =>
      let (ws, idx) := acc
      let kindOk := (jsonGetStr v "kind").isSome
      let funcOk := (jsonGetStr v "function").isSome
      let ws := if !kindOk then
        ws ++ [s!"warning: fact at index {idx} missing required \"kind\" field"]
      else ws
      let ws := if !funcOk then
        ws ++ [s!"warning: fact at index {idx} missing required \"function\" field"]
      else ws
      (ws, idx + 1)
    ) ([], 0)
    issues.1

open Json in
/-- Backward-compatible wrapper: parse facts without warnings. -/
def parseFacts (jsonStr : String) : Option (List Val) :=
  (parseFactsWarn jsonStr).1

end Concrete.Report
