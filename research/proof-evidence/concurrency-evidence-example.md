# Concurrency Evidence: A Worked Example

**Status:** Open research direction
**Affects:** evidence reports, audit surface, capability model, structured concurrency
**Date:** 2026-05-01

## Purpose

This note grounds the evidence-bearing concurrency direction in a concrete worked example. The umbrella direction is in [../stdlib-runtime/async-concurrency-evidence.md](../stdlib-runtime/async-concurrency-evidence.md); that note lists candidate evidence properties as a table. This note shows what an actual `--report concurrency` output should look like for a small program, and what each line of the report ties back to in the source.

The point is to force the design to answer: who reads this report, what question are they trying to answer, and what does the source have to look like for the answer to be machine-checked rather than narrated.

## The Example Program

A small parallel file processor. It reads two files concurrently, hashes their contents, and writes a combined result. There is one bounded channel for diagnostic events.

```con
fn process_pair(a_path: Str, b_path: Str, out_path: Str)
    with(File, Async, Tasks(2), Heap(64K))
    -> Result<Unit, IoError>
{
    scope s with(Async) {
        let (tx, rx): (Sender<Event, 8>, Receiver<Event, 8>) = channel();

        let h_a = s.spawn(hash_file, a_path, tx.clone());
        let h_b = s.spawn(hash_file, b_path, tx);
        let h_log = s.spawn(drain_log, rx);

        let hash_a = s.join(h_a)?;
        let hash_b = s.join(h_b)?;
        let _ = s.join(h_log)?;

        write_combined(out_path, hash_a, hash_b)
    }
}

fn hash_file(path: Str, tx: Sender<Event, 8>)
    with(File, Cancellable)
    -> Result<Hash, IoError>
{
    let content = read_file(path)?;
    let _ = tx.send(Event::Read(path));
    Ok(hash(content))
}

fn drain_log(rx: Receiver<Event, 8>) with() -> Result<Unit, Never> {
    loop {
        match rx.recv() {
            Ok(event) => log(event),
            Err(_) => return Ok(())
        }
    }
}
```

The shape is small enough to reason about, real enough that the evidence claims are non-trivial.

## What The Report Should Say

Running `concrete --report concurrency process_pair` produces something like:

```
process_pair (with(File, Async, Tasks(2), Heap(64K))):
  scope s @ line 5
    capability:                Async                       enforced
    children spawned:          3                           enforced
    children spawned: bound:   2                           reported  *MISMATCH*
    children spawned: source:  spawn at line 8, 9, 10
    cancellation propagation:  parent-error cancels        enforced
    cleanup on scope exit:     all handles consumed        enforced (linear)
    deadlocks under any sched: none                        enforced
       evidence: only with(Async); no with(Concurrent)
    handle leaks:              none                        enforced (linear)

    channel ch @ line 6
      capacity:                8 (bounded)                 enforced
      ownership:               linear sender, linear recv  enforced
      close behavior:          on last sender drop         enforced
      backpressure:            blocking send when full     reported (runtime backend)
      pending message bound:   8                           enforced

    handles:
      h_a, h_b, h_log
      consumed:                via join                    enforced (linear)
      transitive cancellation: yes if any join errors      enforced

  resource bounds:
    Tasks(2):                  3 spawned                   *VIOLATION*
    Heap(64K):                 channel(8 * Event) + ...    reported
       channel:                ~3.2K bound from capacity
       not yet computed:       per-task stack
       evidence level:         partial

  total evidence:
    proved:                    0 properties
    enforced:                  9 properties
    reported:                  4 properties
    trusted assumption:        0 properties
    *violations:               1 (Tasks bound vs spawn count)
    *mismatches:               1 (declared bound vs actual count)
```

The output above intentionally contains a violation. The `Tasks(2)` capability bound is exceeded by spawning three children. The compiler reports this and refuses to certify the function. The fix is either to declare `Tasks(3)` or to merge the `drain_log` task elsewhere.

This is the surface auditors should be reading. Every line is either compiler-checked, clearly reported as a runtime backend assumption, or marked as trusted. There is no narrative.

## How Each Line Is Justified

For the report to be honest, each line must trace to a specific source-level fact. The claims and their sources:

**capability: Async — enforced**
The scope declaration `scope s with(Async)` is a static fact in the AST. The compiler verifies the surrounding function holds at least `Async`. No runtime check is needed.

**children spawned: 3 — enforced**
The compiler counts `s.spawn` calls inside the scope's lexical extent. This is a structural property of the AST.

**cancellation propagation: parent-error cancels — enforced**
The structured scope's exit semantics (defined in the formal model) require child cancellation when the scope exits with an error. This is a property of the scope construct itself, not of user code. Marked enforced because it follows from the typing rules.

**cleanup on scope exit: all handles consumed — enforced (linear)**
Each `Handle<T, E>` is a linear value. The compiler refuses to compile the scope if any handle reaches scope exit unconsumed. Same reasoning for channel sender and receiver.

**deadlocks under any sched: none — enforced**
The function uses only `with(Async)`. The lattice rule says `Async` allows sequential execution; no missing-concurrency deadlock is possible. If the function had used `with(Concurrent)`, this line would change to *reported* with a different justification.

**handle leaks: none — enforced (linear)**
Same source as cleanup-on-scope-exit. Linear types catch this statically.

**channel capacity: 8 (bounded) — enforced**
The channel's type `Channel<Event, 8>` carries the capacity in the type. No runtime check is needed.

**channel close behavior: on last sender drop — enforced**
The channel's API (send, recv, close on last sender drop) is defined in the channel model. The behavior is a property of the channel type, enforced by its implementation, not user code. Marked enforced because the user did not opt out.

**backpressure: blocking send when full — reported (runtime backend)**
This is a backend property. `Io.Threaded` blocks; `Io.Evented` yields; `Io.Sim` schedules. Marked reported because the property depends on which backend the application chooses.

**Heap(64K) bound: partial — reported**
The compiler can compute bounds for some sources (channel buffer is known from capacity) but not all (per-task stack analysis is incomplete). The honest level is reported, with the partial breakdown shown.

**Tasks(2) bound: 3 spawned — *VIOLATION***
The capability declares an upper bound on transitive task count. The compiler counts and finds a mismatch. This is a hard error, not a warning. The function does not compile until reconciled.

## What This Demands From The Compiler

The report above is not free. To produce it, the compiler must:

1. Identify scopes by lexical extent and capability declaration.
2. Count `spawn` calls within scopes, transitively where possible.
3. Track linear handle consumption through control flow (already required for linearity checking).
4. Apply lattice rules to determine cancellation and capability subsumption.
5. Accumulate resource bounds across nested scopes and function calls.
6. Distinguish enforced facts from runtime-backend-dependent facts and clearly label each.
7. Produce a stable artifact (either a textual report or a structured data file) that can be diffed across versions.

Steps 1-4 are local analyses tied to the type checker. Steps 5-6 require capability-aware abstract interpretation across the call graph. Step 7 requires a stable report schema.

The schema is the most important deliverable. Reports must be diffable across compiler versions and across source revisions. A reviewer should be able to ask "what changed in the concurrency evidence between v1.4 and v1.5 of this binary" and get an answer in machine-readable form. This implies a structured format (JSON, TOML, or a Concrete-native s-expression form) backing the human-readable rendering.

## Evidence Levels And What They Mean

The vocabulary needs to be tight. Recommended definitions:

- **proved**: A Lean 4 proof exists that the property holds. The proof is part of the artifact.
- **enforced**: The property follows mechanically from the type system or capability check. No proof object exists, but the compiler refuses to compile code that violates it.
- **reported**: The compiler reports the fact but does not enforce it. The fact may be true under specific runtime backends or build configurations and false under others.
- **simulated(N)**: The property held across N seeds in the deterministic simulation backend. Empirical, not deductive, but machine-checkable.
- **trusted assumption**: The user marked a region as trusted, and the property depends on that region behaving as documented. The compiler propagates the assumption upward.
- **backend assumption**: The property depends on the runtime backend's behavior. Different backends produce different evidence levels for the same source.

A property may have different levels in different parts of the report. "No data races" might be enforced in safe code and trusted-assumption inside an FFI region.

## What This Replaces

Today, audit of concurrent code typically means:

1. Reading the code carefully and hoping to spot races, deadlocks, leaks.
2. Running stress tests that may or may not exercise the relevant interleavings.
3. Trusting library documentation about concurrency behavior.
4. Manual proofs or model-checking on critical sections only.

Concrete's evidence-bearing direction proposes that auditors should be able to ask the *compiler* the same questions and get answers backed by source-level facts. The report above is the artifact that answers those questions.

This does not replace careful review or testing. It establishes a baseline of mechanical claims so that human review can focus on the parts the compiler cannot check.

## What To Avoid

- **Inflating evidence levels.** A reported fact must not be presented as enforced. The audit story collapses if levels become marketing.
- **Burying violations.** A `*VIOLATION*` line must stop the build. Warnings that can be silenced are not evidence.
- **Per-backend reports that disagree silently.** If a property holds on `Io.Threaded` and not on `Io.Evented`, both reports must say so, with the same property name and different levels.
- **Reports that are not diffable.** The schema must be stable enough that a CI step can compare two report artifacts and flag regressions.

## Implementation Sequence

1. Define the report schema as a structured data format with a stable text rendering.
2. Implement enforced-level claims first (capability, scope structure, linearity, bounded channels).
3. Add reported-level claims for resource bounds where partial analysis is possible.
4. Add backend-assumption labels for runtime-dependent properties.
5. Add simulated(N) once the Sim backend exists.
6. Add proved-level claims last, as Lean integrations land.

## One-Line Test

A concurrency evidence report is good if a reviewer reading only the report (not the source) can answer: what is enforced, what is reported, what is trusted, and where the violations are — without ambiguity.

## Relationship To Other Notes

- [../stdlib-runtime/async-concurrency-evidence.md](../stdlib-runtime/async-concurrency-evidence.md) — umbrella direction; this note is the worked example
- [../stdlib-runtime/channel-model.md](../stdlib-runtime/channel-model.md) — channel evidence properties
- [evidence-review-workflows.md](evidence-review-workflows.md) — how reports are consumed in audit workflows
- [proof-evidence-artifacts.md](proof-evidence-artifacts.md) — artifact format and stability
- [provable-properties.md](provable-properties.md) — what can be proved vs reported vs trusted
- [concurrency-formal-model.md](concurrency-formal-model.md) — formal grounding for the enforced claims
