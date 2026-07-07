# LLM-Guided Proof Synthesis

Status: design target / Phase 9 product bet

This document defines the target shape for LLM-guided proof synthesis in
Concrete. The core rule is simple:

**LLM output is search, not evidence. Lean replay is evidence.**

The goal is to reduce proof-authoring cost without weakening Concrete's claim
discipline. A model may propose invariants, lemmas, contracts, or Lean proof
scripts, but no result is accepted unless the normal proof checker can replay it
without the model.

For the manual proof workflow, see [PROOF_WORKFLOW.md](PROOF_WORKFLOW.md).
For evidence classes, see [PROOF_STORY_MATRIX.md](PROOF_STORY_MATRIX.md).
For agent assurance guidance, see
[SPARK_CLASS_ASSURANCE.md](SPARK_CLASS_ASSURANCE.md).

## Product Claim

Manual proof authoring is the biggest adoption risk for a formally verifiable
systems language. The synthesis loop changes the cost curve:

- Concrete emits exact obligations.
- An LLM proposes invariants, helper lemmas, specs, or proof scripts.
- Lean's kernel and kernel-backed decision procedures check the result.
- Failures are minimized and fed back into the next attempt.
- The human reviews the spec, assumptions, trusted boundaries, and replayed
  evidence rather than hand-authoring every proof step.

This is not "AI proves the program." The accurate claim is:

> Concrete lets AI search for evidence, while the compiler, reports, and Lean
> kernel decide what is true.

## Command Shape

Target command:

```sh
concrete prove --synthesize examples/packet/src/main.con parse_header
```

Equivalent spellings are allowed if they produce the same artifacts:

```sh
concrete prove examples/packet/src/main.con parse_header --synthesize
concrete prove parse_header --synthesize --project examples/packet
```

The command should produce a replayable bundle:

```text
.build/prove/parse_header/
  obligations.json
  synthesis.json
  attempts/
    0001/
      prompt.json
      model_output.lean
      check.json
      failure.txt
      minimized/
    0002/
      prompt.json
      model_output.lean
      check.json
  final/
    Proofs.lean
    replay.sh
    replay.json
```

Only `final/replay.sh` and `final/replay.json` determine the evidence class.
The attempt history is diagnostic metadata.

## Example: Loop Invariant Synthesis

Source sketch:

```concrete
#[ensures(result >= 0)]
fn sum_nonnegative(xs: &[i64; 4]) -> i64 {
    let mut i: u64 = 0;
    let mut sum: i64 = 0;
    while i < 4 {
        let x: i64 = xs[i];
        if x > 0 {
            sum = sum + x;
        }
        i = i + 1;
    }
    return sum;
}
```

The compiler emits obligations such as:

```text
O1: loop invariant preserves i <= 4
O2: loop invariant preserves sum >= 0
O3: postcondition follows from exit facts
```

The model may propose:

```concrete
#[invariant(i <= 4)]
#[invariant(sum >= 0)]
while i < 4 { ... }
```

Concrete then generates the Lean obligations for those invariants. If Lean
checks them, the invariant becomes ordinary replayable evidence. If the proof
fails, the minimized failing obligation is fed back into the next attempt.

## Example: Missing Helper Lemma

If a proof fails because a collection fact is missing:

```text
needed:
  get(insert(m, k, v), k) == Some(v)
```

the model may propose a helper lemma over `std.formal_map`:

```lean
theorem formal_map_get_insert_same ... := by
  ...
```

The lemma is accepted only if it checks and is included in the replay bundle.
If it weakens the theorem, introduces an assumption, or depends on a trusted
axiom not allowed by policy, the synthesis attempt is rejected.

## Example: Stale Proof Repair

When a source change makes a proof stale, synthesis may be asked to repair it:

```sh
concrete prove --synthesize examples/hmac/src/main.con hmac_sha256 \
  --repair-stale
```

The tool should:

1. identify the stale proof link;
2. produce the current obligation;
3. show the old proof and old fingerprint as context;
4. ask the model for a patch or replacement;
5. replay the result without the model.

A stale proof is not evidence until replay passes.

## What Reports Must Say

Every synthesis report must include:

- model/tool identity;
- model/tool version when available;
- attempt count;
- prompt policy hash;
- generated assumptions;
- rejected assumptions;
- introduced trusted boundaries;
- introduced extern/Unsafe dependencies;
- solver-trusted facts, if any;
- final evidence class;
- replay command;
- whether replay works without LLM/network access.

Example summary:

```text
synthesis: attempted
model: claude-example-2026-07
attempts: 4
new assumptions: 0
new trusted boundaries: 0
new Unsafe uses: 0
final evidence: proved_by_lean
replay: .build/prove/parse_header/final/replay.sh
llm_needed_for_replay: false
```

## Agent Discoverability

LLMs must not need to scrape repository prose to discover the proof workflow.
The installed binary should expose a machine-readable feature catalog, for
example:

```sh
concrete agent features --json
```

or:

```sh
concrete help --json
```

The catalog should include:

- available proof commands;
- required inputs;
- output artifact paths;
- evidence classes and their meanings;
- replay command for each result;
- policy gates;
- forbidden actions without approval;
- links to local docs bundled with the tool.

Example shape:

```json
{
  "features": [
    {
      "name": "proof_synthesis",
      "command": "concrete prove --synthesize <target>",
      "evidence_rule": "llm_output_is_not_evidence",
      "replay_required": true,
      "replay_command_field": "final.replay_command",
      "forbidden_without_policy": [
        "new_assumption",
        "new_trusted_boundary",
        "new_unsafe_dependency",
        "claim_weakening"
      ]
    }
  ]
}
```

Any future MCP server should expose the same facts, but the CLI/JSON catalog is
the source of truth.

## Red-Team Requirements

The synthesis gate must include negative cases:

1. **Wrong theorem.** The model proves a weaker or unrelated property.
2. **Vacuous spec.** The model makes a precondition impossible or proves a
   trivial postcondition.
3. **Hidden assumption.** The model imports or states an unapproved axiom.
4. **Trust smuggling.** The model moves a difficult fact behind `trusted`,
   `extern`, `Unsafe`, or `solver_trusted`.
5. **Network-only replay.** The final artifact only checks if the model or a
   remote service is available.
6. **Stale replay.** The proof replays against an old fingerprint.

Each negative must fail as `rejected`, `needs_policy`, `stale`, or
`not_evidence`; none may appear as `proved`.

## Relationship To Auto-Discharge

The synthesis loop does not replace deterministic automation:

1. Structural/kernel auto-discharge handles trivial shape facts.
2. Decision procedures handle arithmetic and bitvector fragments.
3. LLM synthesis searches for invariants, lemma shapes, and proof scripts.
4. Lean replay is the only proof authority.

Prefer deterministic auto-discharge whenever it applies. Use synthesis where
human proof glue would otherwise dominate.

## External-Validation Gate

If a non-author rejects Concrete because manual proof authoring costs too much,
rerun the same workload through synthesis and measure review cost:

- Did the model find useful invariants or lemmas?
- Did replay catch wrong attempts?
- Did the human only review specs and evidence, or still write most proof code?
- Were failures actionable?
- Did the final bundle replay without the model?

The pass/fail distinction should be precise:

- **Manual proof failed:** authoring cost is too high.
- **Synthesis-assisted proof passed:** review cost is acceptable.
- **Synthesis-assisted proof failed:** the verification product bet is still
  too expensive or too unreliable.

## Non-Goals

- No evidence upgrade from model output alone.
- No hidden network dependency in replay.
- No policy bypass for assumptions, trusted facts, externs, or Unsafe.
- No source edits by default from `--synthesize`.
- No "AI verified" claim. The claim is kernel replay, not model confidence.
