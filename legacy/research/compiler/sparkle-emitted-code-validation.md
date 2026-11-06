# Sparkle Lessons And Validation Of Emitted Code

Status: Open — dated analysis; implementation remains planned

Date: 2026-10-18. Concrete source baseline: `f5b201af10a3ff641f90fac1313befb8fd1cc343`.
Sparkle source baseline: `1614cee83ac586ca9a0626cd4af13bfef7347c3a`.
Method: source and documentation review; Sparkle was not built or benchmarked.

The [roadmap](../../ROADMAP.md) owns scheduling and acceptance. R-0299/R-0300
own translation validation; R-0288 owns BackendIR; R-0297 owns a possible
target-specific native backend; R-0413 owns compiler components implemented in
Concrete. This note records the reasoning and alternatives, not a second queue
or a claim that these validators or backends have shipped.

## The Practical Goal

A theorem about a function's model and a valid LLVM file answer different
questions. Concrete should also be able to answer: did this particular compiler
run emit code that preserves the checked function's behavior?

For a function that adds one to a `u8`, the required behavior includes both:

- input `10` returns `11`;
- input `255` triggers the required overflow failure.

An emitter that silently wraps `255 + 1` to `0` must fail validation. Checking
only ordinary return values, LLVM syntax, or a proof about the original model
does not establish that property of the emitted artifact.

The useful first experiment is a small, complete function and its required
arithmetic helpers. It need not wait for a proof of the whole compiler or a
replacement native backend. Production authority and evidence formats still
follow their existing prerequisites.

## What Sparkle Demonstrates

Sparkle is a Lean 4 hardware compiler. Its narrow, explicit checks offer useful
patterns for Concrete's systems compiler; its Signal DSL and hardware runtime
are not proposed Concrete language features.

| Pattern inspected at the pinned revision | Lesson for Concrete | Existing owner |
|---|---|---|
| [`#verify_emit`](https://github.com/Verilean/sparkle/blob/1614cee83ac586ca9a0626cd4af13bfef7347c3a/Tools/SVParser/VerifyEmit.lean) emits Verilog, parses it back, checks structural facts and generates bitvector equality obligations for register/output expressions | Validate the actual output artifact under a declared semantics, beyond checking the emitter's internal representation | R-0299/R-0300 |
| [`#verify_eq` and `#verify_eq_git`](https://github.com/Verilean/sparkle/blob/1614cee83ac586ca9a0626cd4af13bfef7347c3a/Sparkle/Verification/Equivalence.lean) generate function-equivalence goals | Give selected refactors a behavior-preservation check; retain both revisions' meaning contexts | R-0182/R-0183, R-0482 |
| [`OracleReduction`](https://github.com/Verilean/sparkle/blob/1614cee83ac586ca9a0626cd4af13bfef7347c3a/Sparkle/Core/OracleSpec.lean) requires an equality proof between iterated model steps and direct computation | An optimized replacement should carry an exact relation and its prerequisites; separately validate its attachment to executable code | R-0299/R-0300, R-0482 |
| [`proofs/`](https://github.com/Verilean/sparkle/blob/1614cee83ac586ca9a0626cd4af13bfef7347c3a/proofs/README.md) isolates Mathlib-heavy proof authoring from ordinary builds | Distinguish proof-authoring dependencies from runtime/build dependencies while preserving evidence requirements | R-0364/R-0365 |
| [Fresh downstream CI](https://github.com/Verilean/sparkle/blob/1614cee83ac586ca9a0626cd4af13bfef7347c3a/.github/workflows/downstream-smoke.yml) creates and builds a consumer pinned to the candidate revision and records setup timings | Test the installed package experience outside the compiler checkout on each claimed platform | R-0285, R-0352/R-0354 |
| [Counterexample search](https://github.com/Verilean/sparkle/blob/1614cee83ac586ca9a0626cd4af13bfef7347c3a/proofs/SparkleProofs/Retype/Falsify.lean) derives executable approximations from the mathematical definitions before expensive proving | Search early, retain witnesses and negative controls, and keep heuristic search separate from proof | R-0190/R-0191 |
| [Named cost coefficients](https://github.com/Verilean/sparkle/blob/1614cee83ac586ca9a0626cd4af13bfef7347c3a/Sparkle/Verification/Cost.lean) express circuit budgets under a cost model | Name the cost model and scope of a resource bound; source costs do not establish machine timing | R-0445 |

### Limits To Preserve When Borrowing

Sparkle's emission checker targets single-module, memory-free designs and
generates obligations over reflected IR expressions. Its parser, reflection,
structural checks and correspondence to source Signal semantics remain distinct
assurance questions. A round trip through related tools can share a mistake.

The divider has an explicit [circuit-to-state-machine bridge
proof](https://github.com/Verilean/sparkle/blob/1614cee83ac586ca9a0626cd4af13bfef7347c3a/Sparkle/Verification/Divider/Bridge.lean).
The control sidecar explicitly records a missing Signal-to-model bridge in its
[scope statement](https://github.com/Verilean/sparkle/blob/1614cee83ac586ca9a0626cd4af13bfef7347c3a/proofs/README.md#honest-scope).
Those are useful examples of the difference between proved mathematics and
proved implementation correspondence.

An oracle's equality about modeled final results does not by itself establish
that register-name matching, input/output encoding, trigger conditions or skipped
observable events preserve the running system. Concrete must bind and validate
those seams, including effects and failure behavior.

Audit the checks themselves. For example, Sparkle's inspected
[`verifyBitAccuracy`](https://github.com/Verilean/sparkle/blob/1614cee83ac586ca9a0626cd4af13bfef7347c3a/Sparkle/Verification/CoSim.lean)
compares zipped lists without requiring equal lengths, so matching prefixes can
produce its success value. The neighboring `compareOutputs` does check lengths.
Concrete's controls should reject missing outputs, empty measurements and
truncated traces. This is a source-level observation, not a reproduced Sparkle
build result or a claim that every caller uses that helper.

## Concrete Foundations At The Recorded Baseline

- [SSA](../../Concrete/IR/SSA.lean) represents instructions, blocks and explicit
  control flow. [Pipeline.lower](../../Concrete/Pipeline/Pipeline.lean) runs
  structural SSA verification before and after cleanup. Structural validity does
  not establish preservation of meaning across either transformation.
- [LLVM.lean](../../Concrete/Backend/LLVM.lean) provides a structured LLVM AST,
  including a raw-text escape. [EmitSSA](../../Concrete/Backend/EmitSSA.lean)
  builds code that [EmitLLVM](../../Concrete/Backend/EmitLLVM.lean) renders.
  Some instructions and checked-helper definitions still enter as raw text.
- [Main.lean](../../Main.lean) optionally runs `llvm-as` on the emitted file.
  That checks LLVM well-formedness; its absence skips this check. Neither outcome
  is semantic equivalence evidence.
- [IntArith](../../Concrete/Semantics/IntArith.lean) defines source integer widths,
  checked/wrapping/saturating operations and failure behavior. It supplies the
  source meaning to preserve, not a proof that the emitter implements it.

R-0299/R-0300's existing V1 relation ends at `ValidatedBackendIR`. Neither a
structured LLVM AST nor the existing syntax check extends that relation to the
actual LLVM text. The emitted-text relation needs an explicit additional increment.

## The Boundaries And Their Evidence

| Edge | Intended evidence | What remains outside that edge |
|---|---|---|
| Source/Core to checked SSA | Existing checking/testing; later pass-preservation proofs | Later lowering, LLVM and native execution |
| Checked SSA to `ValidatedBackendIR` | Existing planned R-0299/R-0300 V1 relation | LLVM rendering and toolchain |
| Checked SSA or validated BackendIR to actual emitted LLVM | New bounded semantic relation over reparsed output | LLVM optimization and native code generation |
| Emitted LLVM to optimized LLVM | Separate supported LLVM transformation checks | Machine-code emission and linking |
| LLVM or target IR to object bytes | Separate ISA/encoding/relocation validation or preservation proofs | Uncovered linker, loader, runtime and hardware behavior |
| Object files to executable behavior | Separate link/runtime/ABI assurance | Any declared environmental assumptions |

[Alive2](https://github.com/AliveToolkit/alive2) provides LLVM refinement checks
and translation-validation tooling. It does not directly prove Concrete SSA
correct, and its supported transformations and solver limits must be retained.
An Alive2 result remains in its actual assurance/trust class; it is not a Lean
kernel proof by renaming.

Artifact hashes identify the checked inputs and outputs. They do not establish
any semantic edge. Several backends agreeing on sampled executions remains
differential testing, especially when they share frontend/lowering code.

## Bounded Emitted-LLVM Experiment

### Inputs And Admission

Retain the exact checked SSA function, its signature and source identity, target
and arithmetic profile, emitted `.ll` bytes, required helper/callee closure,
checker/semantics version and explicit assumptions. Compare the parsed output
with the retained source artifact, not a producer-supplied `preserved` flag.

Start with `u8` values, booleans, return values and acyclic control flow, including
checked addition and its actual overflow helper. Admit only the LLVM operations
needed by those bodies: integer operations/comparisons, branches/phi as needed,
overflow intrinsics and result extraction, and the declared terminal-failure
boundary. Wider integers and other arithmetic operators follow this working case.

Ordinary user externs, heap/aliasing, floating point, recursion, loops and
unmodeled instructions stay outside the first fragment. An unsupported operation
that can affect a selected function makes that whole function unvalidated.
Unrelated functions may remain outside the selected closure, with that scope
reported explicitly. Required helper definitions cannot be omitted from it.

### Separate Semantics And Checking

1. Model admitted SSA executions using the canonical source arithmetic meaning.
2. Parse the actual LLVM artifact with an independently implemented reader or an
   explicitly trusted, pinned LLVM reader. Reject malformed, duplicate, missing
   or ambiguous definitions and resource-limit violations.
3. Give the accepted LLVM subset an independent denotation using the
   [LLVM language reference](https://llvm.org/docs/LangRef.html). Reusing the
   emitter's operator-selection/helper-selection decisions would hide its bugs.
4. Compare observable outcomes over the entire declared input domain: values,
   required terminal failures and any admitted effects. LLVM undefined behavior
   or poison cannot stand in for a defined Concrete result or required trap.
5. Retain the relation, artifact identities, domain, proof/checker result,
   assumptions and remaining trust for replay. The parser-to-bytes and
   semantics/reflection bridges need their own assurance; acknowledge them until
   proved or independently checked.

The initial one-argument `u8` domain has 256 values, making exhaustive probing a
cheap way to test the design. Finite executable comparison earns only its named
checked disposition and domain. A generated Lean theorem can establish the
model-level universal relation only after its proposition, proof and axiom base
are checked. Neither method silently establishes native-binary correctness.

A checker may model a designated `abort`/trap boundary, but must retain the
runtime assumption that gives that boundary its meaning. Calling a helper with
the right name does not establish that helper's body, attributes or signature
are correct. Unchecked `unreachable`, missing checks or newly introduced undefined
behavior cannot satisfy the required Concrete failure semantics.

### Discriminating Controls

The acceptance record should retain:

- `10 -> 11`, `254 -> 255`, and `255 -> required overflow failure` for checked
  `u8` increment, together with full declared-domain comparison;
- a valid explicitly wrapping version under its different source contract;
- the checked function mutated to wrap, and a correct caller paired with a
  corrupted checked-add helper;
- changed width/signedness, wrong return value and wrong branch target;
- an extra LLVM poison/undefined-behavior precondition, removed failure edge or
  incorrect helper signature;
- malformed output, a missing required helper and an unsupported instruction;
- a stale or substituted `.ll`, wrong target/semantics profile and a consistently
  rehashed bundle whose contents still fail the semantic relation;
- parser/solver timeout and resource exhaustion, each producing an explicit
  incomplete result; and
- a positive control showing that unrelated output formatting does not change
  the checked meaning while exact artifact identity still records the bytes.

Keep match, counterexample, unsupported, timeout, malformed and checker-error
outcomes distinct in the existing evidence architecture. Only an accepted
complete relation may support the corresponding claim. Policy-required validation
fails closed on an incomplete result. A development probe has no authority to
mint production proof receipts.

## Additional Workflow Lessons

**Refactors and optimized replacements.** Begin with a selected pure function and
an exact old/new relation. Reconstruct both revisions with their own dependency,
model, contract and target contexts; elaborating old source against current
imports is not historical-program equivalence. Keep preconditions, trap behavior,
effects and claim scope explicit. A model-level replacement proof still needs
correspondence to the code actually selected.

**Proof dependencies.** The integrated package tool should distinguish runtime,
host build-tool, test and proof-authoring edges in its one graph/lock model.
Ordinary builds need not fetch or compile heavy proof-search packages they do not
use. Required evidence still needs an independently checkable retained bundle or
the required replay; an omitted proof dependency cannot make stale evidence
acceptable. Proof dependencies and their toolchains remain in the appropriate
evidence roots even when absent from the executable link graph.

**Fresh consumers.** Bootstrap a project outside the checkout, pin the exact
candidate origin/revision and toolchain, build and run a real dependency use, and
compare actual output and failure behavior. Run on each claimed platform and
record setup/build/run timings. For fork PRs, fetch the candidate from its actual
origin. A trace marker or successful build alone is insufficient. Reuse R-0285's
clean/warm/offline and error/resource scenarios.

**Search before proof.** Derive the executable search model from the selected
specification where possible. Keep seeds, domains and a deliberately false
candidate proving that the search can detect violations. Recheck approximate
counterexamples against exact semantics before accepting them as authoritative.
No counterexample found is not a proof. Reuse R-0190/R-0191's persistence path.

**Resource budgets.** Reuse R-0445's named source-cost model and explicit unknowns.
Missing widths, opaque calls or incomplete bounds must not become zero cost.
Hardware-cycle numbers and source-step counts remain different claims.

## Could Concrete Own A QBE-Sized Native Backend?

Yes. Three projects have different deliverables:

| Project | Output path | What it adds |
|---|---|---|
| Existing QBE integration plan | Concrete SSA to QBE IL to external QBE | A separate code generator and differential oracle |
| Small native backend in Lean 4 | Validated BackendIR to target instructions to assembly/object | A backend owned by Concrete with a direct place to state and prove pass relations |
| Backend implemented in Concrete | A Concrete program consumes backend artifacts and emits target code | A systems workload and possible bootstrap path, with Concrete's own source/codegen correspondence obligations |

[QBE](https://c9x.me/compile/) is useful inspiration for a compact SSA backend,
instruction selection, spilling, allocation and ABI support. Reimplementing its
entire IL, optimizer and target matrix is not necessary to investigate a small
Concrete-native backend. Its [IL documentation](https://c9x.me/compile/doc/il.html)
also makes narrow-integer legalization explicit: ordinary temporaries use word/
long widths, so Concrete's checked sub-word semantics cannot be assumed automatic.

For the assurance goal, prefer investigating a Lean 4 implementation first. It
allows executable semantics and preservation theorems in the same environment as
the current compiler. Implementation in Lean alone proves nothing: the pass
relations still need proofs, and Lean's executable compiler/runtime remain trust
boundaries until the checked artifacts supply an independent route. A Concrete
implementation supplies useful independence and dogfooding but does not inherit
correctness merely because Concrete compiled it.

The compiler and its emitted programs also have different memory-management
requirements. Lean's [reference-counting runtime](https://lean-lang.org/doc/reference/latest/Run-Time-Code/Reference-Counting/)
runs inside the Lean-built backend executable. Emitting assembly/object code
does not automatically add that runtime or reference counting to the generated
program. Measure compiler overhead separately; preserve Concrete's no-GC target
semantics and record any runtime calls the generated program actually needs.

The requested standalone project is named **lean-compiler-backend**, because QBE
and Zig are references rather than limits on its design. The first executable
slice should use a documented QBE integer-IR subset and one host-compatible
assembly target; Concrete integration follows separately. Credit upstream
algorithms, copied/adapted code and test vectors with pinned sources and retained
licenses in its README, design/provenance records and affected modules. Its
initial implementation does not graduate a production Concrete backend.

Keep the first possible native experiment to one target and a tiny integer/
boolean/function subset. Compare target choice using an actual host/workload;
record its ISA, ABI, object format and execution oracle before implementation.
Begin with explicit stack locations and simple instruction selection, then
evaluate register allocation and optimizations separately. Even a stack-based
backend must account for frame layout, alignment, calls/clobbers and failures.

To claim more than differential agreement, prove the admitted lowering against
target instruction semantics or validate each output artifact. Assembly output
leaves the assembler trusted. Checking actual object bytes additionally needs
decoding, relocation and instruction-encoding relations; linking, loading and
runtime assumptions still require their own accounting. CompCert's
[documented correctness scope](https://compcert.org/man/manual001.html) is useful
prior art for defining those boundaries.

Retain wrong-instruction, wrong-register, clobber, width, branch, stack-offset,
trap and ABI mutations when those features enter the fragment. Check decoded
output rather than trusting the same emitter's annotations. Any imported ISA
model, decoder or shared layout logic remains a named trust/correlation boundary.

The research home is [qbe-in-concrete.md](qbe-in-concrete.md), extended to compare
Lean 4 and Concrete implementations, with R-0297/R-0413 as existing owners.
This remains workload-gated: a third backend must demonstrate reduced trust or
new independent bug detection sufficient to justify its maintenance cost.

## Scheduling And Decision

Adopt the bounded emitted-output experiment as an explicit R-0299/R-0300
increment, retaining the existing BackendIR V1 as a separate relation. Prototype
directly against the existing SSA emitter if that is the smallest useful probe;
do not require a wholesale BackendIR migration first. Authority follows the
existing task, identity, checker and schema owners. R-0302 owns toolchain pins.

The current soundness/identity work and executable C/Rust/Zig dependency slices
keep their recorded order. Fresh-consumer CI belongs with interop implementation;
the semantic validator and possible native backend do not become prerequisites
for the first working RocksDB consumer. New reproduced correctness failures
still enter the existing repair process. No full QBE rewrite, self-hosting,
whole-LLVM proof or native-backend migration is approved by this analysis.
