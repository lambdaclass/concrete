/-
Build profiles — ROADMAP #10, Stage 1 (mechanism only).

A build profile is a POLICY BUNDLE: it selects check *enforcement*, diagnostics,
optimization assumptions, evidence floors, and reporting. It NEVER changes what a
source program means — a reviewer must not need to know the profile to know
whether `a + b` wraps. Arithmetic semantics are fixed and explicit (checked
`+ - *`, explicit `wrapping_*` / `saturating_*`); see docs/ARITHMETIC_POLICY.md
§14 and docs/PROFILES.md.

Stage 1 makes the active profile and its bundle VISIBLE (`--profile`,
`[profile]` in Concrete.toml, `--report profile`). It has NO codegen or semantic
effect: in particular the report states honestly that ordinary arithmetic still
lowers to silent wrapping until ROADMAP #10 Stage 2.
-/
namespace Concrete

inductive BuildProfile where
  | debug | release | predictable | proof | highIntegrity
  deriving DecidableEq, Repr, Inhabited

namespace BuildProfile

def name : BuildProfile → String
  | debug         => "debug"
  | release       => "release"
  | predictable   => "predictable"
  | proof         => "proof"
  | highIntegrity => "high-integrity"

def all : List BuildProfile := [debug, release, predictable, proof, highIntegrity]

def ofString? (s : String) : Option BuildProfile :=
  all.find? (fun p => p.name == s)

/-- Comma-separated list of valid profile names, for error messages. -/
def known : String := ", ".intercalate (all.map name)

/-- One-line gloss of what the profile's bundle emphasizes. -/
def summary : BuildProfile → String
  | debug         => "runtime checks on, rich diagnostics, no aggressive optimization assumptions"
  | release       => "optimized, runtime checks preserved (no hidden semantic change)"
  | predictable   => "predictable-execution admission (no recursion/alloc/FFI/unbounded loops) + stricter reporting"
  | proof          => "proof admission floor: eligible functions are expected to carry proofs"
  | highIntegrity => "strictest admission and reporting"

/-- Runtime-check enforcement strategy (Stage 1 reports intent; no codegen yet). -/
def runtimeChecks : BuildProfile → String
  | release => "enabled; a proof may elide a redundant check, and any further elision must be explicit + reported"
  | _       => "enabled (bounds via checked APIs; div/shift/overflow trapping arrives in Stage 2/3)"

def diagnostics : BuildProfile → String
  | release => "terse (machine-oriented)"
  | _       => "rich (full spans, hints, expected/actual)"

def optimization : BuildProfile → String
  | release       => "enabled — but never `nsw`/`nuw`; checked arithmetic semantics are preserved"
  | highIntegrity => "conservative; no assumptions a reviewer can't see"
  | _             => "none aggressive; predictable codegen"

def evidence : BuildProfile → String
  | proof          => "proofs expected for proof-eligible functions"
  | predictable   => "predictable-profile facts enforced and reported"
  | highIntegrity => "maximal evidence floor (proofs + predictable + reporting)"
  | _             => "proofs optional; no admission floor"

end BuildProfile

def defaultProfile : BuildProfile := .debug

/-- Where the active profile selection came from (reported for auditability). -/
inductive ProfileSource where
  | default | cli | manifest
  deriving DecidableEq

def ProfileSource.describe : ProfileSource → String
  | .default  => "default (no --profile flag, no [profile] in Concrete.toml)"
  | .cli      => "--profile (command line)"
  | .manifest => "[profile] in Concrete.toml"

/-- Strip surrounding double quotes and whitespace from a TOML value. -/
private def stripTomlValue (s : String) : String :=
  let cs := s.trimAscii.toString.toList
  if cs.length ≥ 2 && cs.head? == some '"' && cs.getLast? == some '"' then
    String.ofList (cs.drop 1).dropLast
  else
    String.ofList cs

/-- Read `name = "..."` from the `[profile]` section of Concrete.toml content.
    Returns the raw string (not yet validated against the known profiles). -/
def parseProfileName (content : String) : Option String := Id.run do
  let mut inProfile := false
  for raw in content.splitOn "\n" do
    let t := raw.trimAscii.toString
    if t.startsWith "[profile]" then
      inProfile := true
    else if t.startsWith "[" then
      inProfile := false
    else if inProfile && t.startsWith "name" then
      match (t.splitOn "=") with
      | _ :: rest => return some (stripTomlValue ("=".intercalate rest))
      | _ => pure ()
  return none

/-- Render `--report profile`: the active profile and its policy bundle.

    The `arithmetic` line is identical for every profile — arithmetic meaning is
    profile-invariant — and states the current lowering gap explicitly so the
    report never implies a guarantee the backend does not yet provide. -/
def renderProfileReport (p : BuildProfile) (src : ProfileSource) : String :=
  let arith :=
    "checked semantics for `+ - *` (trap on overflow); explicit `wrapping_*` / " ++
    "`saturating_*` for intentional modular/clamping arithmetic.\n" ++
    "                  CURRENT LOWERING GAP: ordinary arithmetic still lowers to " ++
    "silent two's-complement wrap; checked/trapping codegen is ROADMAP #10 Stage 2 " ++
    "(not yet implemented). Profiles do NOT change this — the gap is uniform."
  "=== Build Profile ===\n" ++
  s!"  profile:        {p.name}\n" ++
  s!"  source:         {src.describe}\n" ++
  s!"  bundle:         {p.summary}\n" ++
  s!"  arithmetic:     {arith}\n" ++
  s!"  runtime checks: {p.runtimeChecks}\n" ++
  s!"  diagnostics:    {p.diagnostics}\n" ++
  s!"  optimization:   {p.optimization}\n" ++
  s!"  evidence/proof: {p.evidence}\n" ++
  s!"  note:           a build profile is a policy bundle, never an arithmetic mode " ++
  "(docs/PROFILES.md, docs/ARITHMETIC_POLICY.md §14).\n"

end Concrete
