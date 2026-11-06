import Concrete.Elab.Core

/-! ## Backend — the backend contract (ROADMAP Phase 4 #17)

The single source of truth for the guarantees the code generator commits to:
target triple / data layout, integer overflow + division semantics, layout/ABI,
panic/assert behavior, optimization assumptions, and the libc/runtime surface.

`EmitSSA` emits its module header from `targetTriple` / `dataLayout` / the
runtime declarations here, and `--report backend-contracts` renders the same
facts — so the documented contract cannot drift from what the backend actually
emits. 17a makes the contract VISIBLE; tightening the backend checks it describes
(overflow/division/layout) comes in later slices. -/

namespace Concrete.Backend

/-- The LLVM target triple the backend emits for. -/
def targetTriple : String := "arm64-apple-macosx14.0.0"

/-- The LLVM data layout string emitted in the module header. -/
def dataLayout : String := "e-m:o-i64:64-i128:128-n32:64-S128-Fn32"

/-- libc / runtime functions the backend declares and links against. -/
def runtimeFunctions : List String :=
  ["malloc", "free", "realloc", "write", "abort", "printf", "strlen",
   "memset", "memcmp", "snprintf", "strtol"]

/-- One backend-contract clause: a topic and the guarantee the backend makes.
    `proofLinked` marks guarantees whose soundness rests on a proof obligation
    rather than a runtime check (so an undischarged obligation = UB at runtime). -/
structure Clause where
  topic       : String
  guarantee   : String
  proofLinked : Bool := false

/-- The static backend contract — independent of any particular program.
    Each clause states what is actually emitted today (honest, not aspirational). -/
def staticClauses : List Clause :=
  [ { topic := "integer_overflow",
      guarantee := "fixed-width integer +,-,* wrap (two's complement) by default; "
        ++ "#[overflow_checked] instead emits no-overflow proof obligations for that function",
      proofLinked := true },
    { topic := "division",
      guarantee := "signed types lower to sdiv/srem, unsigned to udiv/urem (round toward zero); "
        ++ "the backend inserts NO runtime zero-check — division/remainder by zero is a proof "
        ++ "obligation, and an undischarged one is undefined behavior at runtime (LLVM sdiv/udiv by zero)",
      proofLinked := true },
    { topic := "layout_abi",
      guarantee := "struct/enum layout follows the emitted data layout; #[repr(C)] aggregates use the "
        ++ "platform C ABI (register/byte passing via ABI flattening); the default repr is "
        ++ "compiler-defined and NOT ABI-stable across versions" },
    { topic := "panic_assert",
      guarantee := "OOM and other unrecoverable runtime conditions call libc abort() (SIGABRT); "
        ++ "there is no unwinding and no panic handler. Contract assert/precondition failures are "
        ++ "proof obligations, not runtime traps",
      proofLinked := true },
    { topic := "optimization",
      guarantee := "the backend emits textual LLVM IR and hands it to clang at clang's default "
        ++ "optimization; Concrete introduces no fast-math and no UB-based reassociation — runtime "
        ++ "semantics are exactly those established over the typed IR" },
    { topic := "target",
      guarantee := s!"emits for target triple {targetTriple} with data layout {dataLayout}" } ]

private def jesc (s : String) : String :=
  s.foldl (fun a c => a ++ (match c with
    | '"' => "\\\"" | '\\' => "\\\\" | '\n' => "\\n" | c => c.toString)) ""
private def jq (s : String) : String := "\"" ++ jesc s ++ "\""

private partial def allFns (m : CModule) : List CFnDef :=
  m.functions ++ m.submodules.foldl (fun acc s => acc ++ allFns s) []
private partial def allExterns (m : CModule) : List (String × List (String × Ty) × Ty × Bool) :=
  m.externFns ++ m.submodules.foldl (fun acc s => acc ++ allExterns s) []

/-- Trusted boundaries in THIS program: trusted functions and extern declarations
    whose correctness the backend takes on faith (not proven). -/
def trustedBoundaries (modules : List CModule) : List String × List String :=
  let fns := (modules.foldl (fun acc m => acc ++ allFns m) []).filterMap (fun f =>
    if f.isTrusted || f.trustedImplOrigin.isSome then some f.name else none)
  let externs := (modules.foldl (fun acc m => acc ++ allExterns m) []).map (·.1)
  (fns.eraseDups, externs.eraseDups)

-- ============================================================
-- Rendering: human + JSON from the same clauses (cannot drift)
-- ============================================================

def report (modules : List CModule) : String :=
  let (trustedFns, externs) := trustedBoundaries modules
  let clauseLines := staticClauses.map fun c =>
    let tag := if c.proofLinked then "  [proof-linked]" else ""
    s!"  {c.topic}{tag}\n    {c.guarantee}"
  let trustedStr := if trustedFns.isEmpty then "    (none)"
    else String.intercalate "\n" (trustedFns.map (s!"    {·}"))
  let externStr := if externs.isEmpty then "    (none)"
    else String.intercalate "\n" (externs.map (s!"    {·}"))
  String.join [
    "=== Backend Contract ===\n",
    "The guarantees the code generator commits to. [proof-linked] clauses rest on a\n",
    "proof obligation rather than a runtime check.\n\n",
    String.intercalate "\n" clauseLines,
    s!"\n  runtime\n    libc/runtime functions linked: {String.intercalate ", " runtimeFunctions}",
    s!"\n  trusted_boundaries\n   trusted functions (correctness assumed, not proven):\n{trustedStr}",
    s!"\n   extern declarations (foreign, outside the proof boundary):\n{externStr}" ]

def toJson (modules : List CModule) (schemaVer : Nat := 1) : String :=
  let (trustedFns, externs) := trustedBoundaries modules
  let clauseObjs := staticClauses.map fun c =>
    String.join ["{", jq "topic", ": ", jq c.topic, ", ", jq "guarantee", ": ", jq c.guarantee,
      ", ", jq "proof_linked", ": ", (if c.proofLinked then "true" else "false"), "}"]
  let strArr := fun (xs : List String) => "[" ++ String.intercalate ", " (xs.map jq) ++ "]"
  String.join [
    "{", jq "schema_version", ": ", toString schemaVer, ", ",
    jq "schema_kind", ": ", jq "backend_contract", ", ",
    jq "target_triple", ": ", jq targetTriple, ", ",
    jq "data_layout", ": ", jq dataLayout, ", ",
    jq "runtime_functions", ": ", strArr runtimeFunctions, ", ",
    jq "clauses", ": [", String.intercalate ", " clauseObjs, "], ",
    jq "trusted_functions", ": ", strArr trustedFns, ", ",
    jq "extern_declarations", ": ", strArr externs, "}" ]

end Concrete.Backend
