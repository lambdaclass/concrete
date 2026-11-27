# The Concrete compiler: slice 0 design (provisional)

PROVISIONAL. The Slice 0 prototype compiles standalone `.con` files through both native backends. This guide also retains the original Slice 0 design sketch (revision 8, 2026-11-06). Slice 0 is built before the owner freezes Core (`A2c`) and makes no normative claim before that freeze. [Decision](../docs/decisions.md#decided-project-proof-infrastructure).

## Running the current slice

The driver accepts a canonical project-relative `.con` path in a project without `Concrete.toml`. From the repository root, enter `nix develop` and run:

```sh
LEAN_NUM_THREADS=2 lake build concrete
.lake/build/bin/concrete build tests/slice0/answer.con -o build/answer
```

On success, `build/answer` contains `llvm.exe` and `lcb.exe`. Both return 42 for the checked-in file, whose last byte is a newline. The [Lean compilation witness](#supporting-theorems) uses an embedded string ending at the closing brace.
Use `--backend llvm` or `--backend lcb` for a single executable. The driver uses the pinned
native build helper and the LCB executable built by Lake. It compiles and links in a private
directory beside the destination, checks that the retained link inputs match the generated inputs,
and replays the retained native build with the pinned reference clang before publishing by
atomically installing or exchanging the directory. The Lake-built CLI locates its build
helpers and LCB relative to its own installation. After verification, the CLI writes and
flushes the selected standalone language-rules version before attempting publication.
Use exit status 0 to confirm publication.
Each backend retains a SHA-256 inventory of its inputs, objects and image. The build receipt
`runtime-premises.txt` identifies the runtime loader and system libraries as unbound environment
premises.

### Slice 1 driver follow-ups

- R-0576, R-0589 and
  R-0603 own the project decoder, its approved key spellings and revisions,
  project and dependency identity resolution, and the CI requirement for a recorded assumption
  diff. The current recognizer bounds bytes and decoded nesting in the TOML syntax used by the
  fixtures. R-0589 owns full TOML conformance, including arrays of tables, indexed key and table
  lookups, the admission judgment, and correspondence theorems for the driver checks.
  R-0589 also tracks distinguishing valid TOML outside the supported subset from malformed input
  in CLI diagnostics. Until then, a present manifest produces a structured driver refusal. The Lean policy checker
  can exercise typed requirements against slice 0 MIR and returns per-function coverage. It reports
  missing stack evidence explicitly. Inherited policy conjunction has a regression control.
  Independent compiler fact derivations and their soundness belong to R-0575.
  The general inherited-scope proof and registry examples for the remaining clause variants
  stay under R-0576. Typed assumption identities and set diffs are available for the future
  revision-aware CI reader.
- R-0586 owns grant decoding, the authority-backed Core oracle and trace serialization.
  The authority evaluator can now replay typed launch grants and return their event trace.
  The Core replay boundary accepts a supplied startup configuration and oracle and returns
  the evaluator's events, ending and residual state. Compare the same checked
  slice 1 source through the reference and native paths under R-0698 once the native pipeline
  accepts the slice 1 constructs and explicit abort. R-0586 also owns validation of grant identity
  uniqueness before issuing grants.
- [Q8.6](../docs/decisions.md#decided-2026-11-05-core) requires the full retained dependency
  closure. R-0505 ([Verified growth](../ROADMAP.md#e-verified-growth)) owns binding and validating the runtime loader and system
  libraries as part of final-image validation. R-0509 owns the trusted toolchain inventory.
  The current replay covers the emitted inputs, objects and image. R-0578
  owns sharing the typed launcher input boundary with LCB. R-0509 also owns coverage of the
  CLI root in the environment-level axiom and example scans.

Use a dedicated output directory. The CLI resolves ancestor symlinks before processing subsequent
`..` components and rejects a symlink at the final component, including one spelled with a
trailing slash or `/.`. The output directory cannot contain the working project. Its closest
existing parent must be writable so the CLI can create a private staging directory there.

A build writes and validates all selected artifacts in private staging. The publisher carries
forward unrelated entries and artifacts of an unselected backend, then commits the complete
directory with one atomic exchange (Linux `renameat2(RENAME_EXCHANGE)` or macOS
`renameatx_np(RENAME_SWAP)`). A first publication installs the output and any missing ancestors
with one exclusive rename. Before staging, the publisher checks the name-lookup rules of the closest
existing parent and any existing output tree. Before publication it checks that staging and the
destination use the same rules. On Linux it checks the mount and the directory's casefold flag on ext2, ext3, ext4,
F2FS and tmpfs. On macOS it checks the volume and its reported case sensitivity. A
mismatch or unavailable lookup information stops publication. A publication helper failure before the exchange
returns `TOOL-FAILURE` with exit 2 and leaves existing outputs in place. Missing output directories
are created only when publication commits. The parent-directory lock serializes commits by
cooperating CLI builds sharing that parent, including builds selecting different backends.
If a cooperating build installs a missing ancestor while another waits for the lock, the waiting
build locks the newly installed parent and rechecks the destination before taking its snapshot.

The exchange is atomic for the directory name. A reader opening several files across the
exchange may see different generations. The output directory inode changes, and carrying forward
regular files requires hard-link support. Use an output tree without mount points or concurrent
edits by other tools. This is a visibility guarantee during normal execution. It provides no
crash durability, and interruption or cleanup failure can leave private staging behind.

`scripts/checks/compiler/tests.sh` also compiles the `con:` manifest rows. For trap rows it
first checks that admission refuses, then runs the same lowering stages without discharged
evidence and compares both native observations.

## Scope

This document describes **the Slice-0 compiler prototype**: its stages, IRs, proof strategy, premises and gates. For the frozen slice-indexed stage-interface contracts replacing these provisional Slice-0 carriers at cutover, see [docs/compiler/STAGE_INTERFACES.md](../docs/compiler/STAGE_INTERFACES.md). For the frontier compiler architecture research series (`R-0730`–`R-0742`—including the cutover's closed per-IR pass slots and split optimizing-pass provenance rule, multi-obligation BIR/LCB validators, private proved-convertible `LayoutPlan` synthesis, non-vacuous cost preservation, safe open modules, `QueryDB`, and Stage-0 day-one foundations), see [docs/proposals/compiler-architecture/README.md](../docs/proposals/compiler-architecture/README.md). Language rules are not restated here. They live in `ConcreteSpec`, and this document links to them:
- the scalar Core fragment and its embedding in [`ConcreteSpec/Spec/Slice0.lean`](../ConcreteSpec/Spec/Slice0.lean);
- its behaviour theorems: [`ConcreteSpec/Proofs/Slice0.lean`](../ConcreteSpec/Proofs/Slice0.lean);
- what the parent process observes, including the trap status and the trap line: [`ConcreteSpec/Spec/Observation.lean`](../ConcreteSpec/Spec/Observation.lean);
- the Slice 0 lexical and grammatical judgments in [`ConcreteSpec/Spec/Surface0.lean`](../ConcreteSpec/Spec/Surface0.lean).

The conformance manifest includes hand-written BIR programs, Concrete source files and CLI cases. The `bir:`, `con:` and `cli:` rows in `tests/manifest.tsv` select the corresponding runner.

## The pipeline

The driver lexes and parses source, prepares the Slice 0 Core fragment, checks admission, lowers to MIR and Memory, and runs SSA construction to produce BIR. The shared output contains the project, file ID, tokens, AST, elaborated Core, admission evidence, MIR, Memory, BIR and a provenance proof. The driver prepares LLVM or LCB inputs after shared compilation.

**CheckedCore and lowering.** The scalar prototype embeds the spec’s Slice 0 fragment into Core. Admission records a binder and known value for each discharged addition. MIR projects the matching token to an optional integer. Scalar Memory evaluates ordered stores over a table of local values. The indexed interfaces specify the richer memory, occurrence and provenance contracts for cutover.

The compiler imports `Slice0.Program` and `Slice0.toCore` through `Concrete/Core.lean`. `Admission.prepareParsed` uses `SurfacePreparation.prepareSurface0` for the surface AST. A residual addition in the conformance route retains its operands and operator span. SSA emits an overflow check and a branch to a trap block for that addition. The unchecked route still checks eligibility and static rules.

**Proof strategy per pass**

| Pass | Current evidence |
|---|---|
| Lexer | Successful lexing implies the lexical judgment (`Lex.lex_correct`). |
| Parser and elaboration | The scalar frontend premise record still assumes parser soundness and preservation of evaluation by elaboration. |
| Admission, MIR and Memory | `MIR.lower_correct` uses `Admission.admit_never_traps`. Successful compilation preserves the Core outcome through Memory (`Stages.compile_core_correct`). |
| SSA | The SSA validator proves BIR well-formedness and the return value or trap kind on the recorded Memory/BIR pair. `SSA.lower_wf` and `Stages.compile_ssa_correct` give the two results. Exact source provenance is a separate obligation. |
| BIR → LCB adapter | The LCB adapter theorem applies to well-formed BIR that the adapter accepts. |
| Native | `Sanity.answerCompiles` witnesses shared compilation of the embedded answer example. The native conformance test builds the checked-in file. Production native correctness requires the registered reference and named native premises. |

**The scalar BIR contract.** BIR defines integer and flag values, the site table used to decode trap results, and a frame-slot count. Final-image stack bytes require separate evidence about the native artifact and launcher.
BIR is the one boundary that both backends read. It is designed for both from the start:
- block-argument SSA;
- `add.ovf` returns `(value, flag)`;
- `br`/`condbr`;
- a trap terminator that indexes the site table.

The LLVM emitter uses overflow intrinsics. For supported widths the LCB adapter expands overflow checks using word operations and comparisons.

**Both backends run in slice 0.**

The **LLVM printer** is unverified and stays a premise (`P_print`) until an LLVM-IR semantics exists.

The **BIR → LCB-IR adapter** is the first backend proof target. `adapter_correct` states that the adapter's output, under **LCB's own semantics** (`LeanCompilerBackend.Verified.Source`, the `ssa-int-v2` profile, imported and never restated), has the BIR behaviour. It is proved (`Proof.adapter_correct`) for every WF program the adapter accepts (`adapt p = .ok f`). The backend contract is complete (owner, 2026-11-08): `LCB.emit : Program → Except Refusal (Source.Function × SiteTable)` represents adaptation failure, `LCB.artifact` builds only from a successful `emit`, and `Proof.emit_correct` (success implies correctness), `Proof.artifact_of_emit_error` (a refusal produces no artifact, for every target) and `Proof.artifact_correct` (every artifact comes from a correct `emit`) state it. `P_adapter` is retired; the trusted-base golden (gate 9) records it.

The adapter's output is handed to LCB **in process**, as the value: on `aarch64-macos` through LCB's verified first path (`Verified.compileFunction`), on the Linux targets through LCB's compatibility backends (`Verified.reify`, then `AMD64`/`AArch64.emitModuleValidated`). No printer or importer sits between the adapter and LCB. The QBE text of the body is a debug dump only. The conformance gate checks the LCB job profile for paired executable fixtures on the two CI hosts.

**The `adapter_correct` proof, in layers** (in `Backend/LCB.lean`, namespace `Proof`):
1. *Values* (proved): `enc` represents an integer as its 32-bit word and a flag as 0/1; `binaryBits_exact`, `wrap_word` (the mask-and-rebias expansion computes `wrap t`) and `flag_word` (the unsigned compare is exactly `inRange`), over the profile's types.
2. *Instructions*: the invariant is: `env.size = f.valueWidths.size = valueCount p`, every width is `word`, and for every register `r` with BIR `lookup benv r = some v` (the newest binding: BIR's list shadows on loops where LCB overwrites a slot), `env[id r] = some (enc v)`, and every current integer binding `.int t x` is in range (`t.inRange x`: `Agrees` alone would let a `.u8` register hold `2^32`, whose word is 0, and then the overflow flags differ; immediates are in range by WF, results by `wrap_inRange`, and block arguments carry it). From the instruction's first position, the expansion's steps reach the next position with the invariant extended by the result (and flag); every operand read succeeds at width `word`, and BIR's operands stay in range (for `flag_word`). Needs: register ids injective, intermediate ids disjoint from register ids, every id below `valueCount`. Proved so far: the environment lemmas (`Agrees`, `read_operand`, `write_ok`, `agrees_set_other`, `agrees_define`) and the step lemmas for each expansion, from its first position with word-width slots: `bin_steps` (4 steps, `word (wrap t (x op y))` at `n + 3`), `binOvf_steps` (6 steps, also `enc (.bool (!t.inRange (x op y)))` at `n + 5`, for in-range operands) and `trunc_steps` (3 steps, `word (wrap to x)` at `n + 2`). With the numbering facts (`numberingOk`: intermediates are no register's id, every id is below `valueCount`), `inst_sim` carries the invariant (agreement, range, size) through any one instruction, with the result (and flag) bound, in `Inst.width ins` LCB steps (the adapter's `Inst.width`). Layer 3 so far: the adapted body's shape (`adapt_block`, `adapt_widths`, `block_insts`: instruction `i`'s expansion starts at `instPos b i`; `block_size`: the terminator follows the last one) and `body_sim`: the LCB steps of a block's instructions keep the invariant through BIR's `execInsts`. Terminators: `ret_sim` and `trap_sim` (LCB returns `enc v`, and the word `256 + s`), and `transfer_ok`: along an edge whose every phi has one arm from the predecessor (WF's `t ≠ e`), LCB reads every phi from the old environment and then writes them. `edge_sim`: along the only edge `j → t` (WF's `t ≠ e` makes a taken `condbr` arm the only one), LCB's `transfer` lands at `t`'s first position with the invariant for BIR's `enter`: each parameter's slot holds its new value, every other register keeps its slot. `runBlock_sim` composes them over BIR's `runBlock` (by fuel): an exit or a trap is matched by LCB returning its encoding, running out of fuel by at least `fuel` LCB steps (each block transition is at least one LCB step: the terminator's `transfer`). Layer 4: `initial_ok` (the entry block, no phis since WF's entry has no parameters, an empty environment of size `valueCount p`) and **`adapter_correct_run`**: for `WF p` and `adapt p = .ok f`, `run p fuel = exited c` gives `HasBehavior f #[] (.returned (word c))`, `trapped s` gives `.returned (word (256 + s))`, and running out of fuel at every fuel gives `.diverges` (`steps_prefix`). All under LCB's own `Source` semantics, with only `propext`, `Classical.choice` and `Quot.sound`. Then, closing the statement:
   - *BIR progress* (`Concrete/BIR/Progress.lean`): `run_progress`: for `WF p`, every `run p fuel` is `outOfFuel` or `Outcome.Encodable` (an exit below 256, a trap at a site of a table of at most `maxSites`), so never `stuck`; the invariant `Typed` (every register in the block's scope is bound to a value of its type, in range) is block-local, extended by each instruction as `Block.wf` extends the scope and established at entry from the arguments (registers distinct, from WF's `eraseDups` check). `behaviour_diverges_iff` (any program): `behaviour p = .diverges` iff `run p fuel = .outOfFuel` at every fuel.
   - *Uniqueness*: `hasBehavior_unique` (from LCB's `step_deterministic`): a function has at most one `Source.HasBehavior` on given arguments (two returns return the same value; a return excludes divergence).
   - *Bridge*: `adapter_correct_behaviour`: the adapted body returns the word `w` with `behaviour p = decode p.sites w` (by `decode_encode`, which keeps the site lookup and the trap kind), or diverges exactly when BIR diverges.
   - **`adapter_correct`**: `WF p → adapt p = .ok f → bodyResult f r → r.behaviour p.sites = behaviour p`, with `bodyResult` defined in `Backend/LCB.lean` as below. Axioms: `propext`, `Classical.choice`, `Quot.sound`.
   - **`emit_correct`** lifts the adapter proof to successful emission of a function and site table. Well-formed BIR may use integer widths outside the scalar adapter’s domain.
3. *Blocks*: `execInsts` against those steps; `ret` and `trap` against LCB's `returned`; `br`/`condbr` against `transfer`: the target exists, each phi has exactly one arm from this predecessor (WF's `t ≠ e`), destinations are distinct (distinct registers), and all arms are read from the OLD environment before any destination is written, which is BIR's `enter` (all arguments evaluated before binding), including loop-carried swaps; the new state is at position 0.
4. *Runs*: under `WF p` and `adapt p = .ok f`, `Source.initial f #[] = .ok s₀` (entry block exists with no phis, no parameters, an empty environment of size `valueCount p`); then `run p fuel = exited c` or `trapped s` gives `HasBehavior f #[] (.returned (word (encode o)))`. Divergence: each BIR block transition is at least one LCB `Step` (BIR fuel counts blocks, LCB counts instructions and terminators), so BIR running out of fuel at every fuel gives arbitrarily long LCB prefixes, i.e. `HasBehavior f #[] .diverges`. BIR progress (a WF program never gets stuck) is needed only to make exit, trap and divergence exhaustive.

Two refactors make layers 2–4 tractable, and both are done: `runBlock` names blocks by index (as LCB's state does), and the numbering is a closed function of position (`blockBase`, `instBase`, `regId`), which LCB's canonical-form check accepts for every example.

LCB's own lowering, assembler and linker are LCB's named trust boundaries, carried as `P_toolchain[lcb]` until LCB's preservation theorem covers the slice-0 domain.

LCB is a pinned lake dependency (`require` with an exact rev, LCB's upstream lock recorded). Concrete builds it as a Lake dependency with Lean v4.35.0 and no Mathlib. The pinned upstream revision declares v4.34.1 for standalone builds; its sources also build under Concrete’s v4.35.0 toolchain.

**The encoded result, with one decoding runtime.** The compiled body returns an encoded result. The separately emitted launcher calls the body and performs runtime I/O.

Each backend emits `main`'s body as a function `concrete_body() -> w`, a 32-bit word, whose return value **encodes** the behaviour:
- `exited c` returns `c` (0–255);
- a trap at site `s` returns `256 + s.idx`. The runtime's site table maps the index to the trap's kind, typed `SiteId` and span.

The adapter uses the integer, comparison, branch, phi and return instructions of LCB's imported source profile.

This separates a normal return of 134 from a trap, whose encoding starts at 256.

Both launchers call `concrete_body` and decode its word using the runtime emitted by [Backend/Runtime.lean](Backend/Runtime.lean).

| Body word | Runtime action |
|---|---|
| Below 256 | Exit with that status. |
| `256 + i`, with an entry at index `i` | Attempt to write the entry's trap line and a newline to fd 2, then exit 134 even if the write returns zero or an error. |
| `256 + i`, with no entry at index `i` | Attempt to write `concrete: runtime: invalid result word` and a newline to fd 2, then call `abort()`. The Lean decoder gives `.stuck`. |

The scalar native premise `P_rt` relates the observed behaviour of a non-killed launch to a result of the body on that launch. For an admitted, observed launch whose body returns a trap word with a matching table entry, `P_trap_site` supplies the exact bytes on fd 2. [Native premise fields](Premises.lean) · [BIR decoder](BIR.lean).


## Layout (lean root, few cohesive folders)

Main.lean at the repository root implements the CLI. Concrete/Stages.lean implements scalar compilation, Concrete/Premises.lean lists its premises, Concrete/Theorem.lean states the image claim, and Concrete/Certificate.lean defines certificate validation propositions. The general facts reporter and per-build Lean certificate emitter are planned.

| Files under `Concrete/` | Purpose |
|---|---|
| `Lex.lean`, `Parse.lean`, `LL1.lean` | Lexer, table parser, and compatibility re-export of the spec's LL(1) definitions. |
| `Core.lean`, `Elab.lean`, `Admission.lean`, `Refusal.lean` | Core embedding, surface sites and reference AST evaluator, preparation and admission, and diagnostics. |
| `MIR.lean`, `Memory.lean`, `SSA.lean` | Scalar lowering and validation. Memory proves `lower_correct`, `lower_admitted_correct` and `lower_unchecked_correct`. |
| `BIR.lean`, `BIR/Progress.lean` | Backend IR, `binOvf_spec`, and progress for well-formed BIR. |
| `CFG.lean`, `Origins.lean`, `Memory/Lowering.lean`, `Memory/Placement.lean`, `Memory/Refinement.lean`, `IntegratedCore.lean`, `IntegratedCore/` | CFGs, provenance, indexed Memory lowering, placement and exhaustion proofs, and the integrated Core bridge. |
| `Interfaces/` — `Behavior.lean`, `Carriers.lean`, `Contracts.lean`, `Pipeline.lean`, `SurfaceElaboration.lean` | Slice-indexed refinement, IR types, stage contracts, registry and structural elaboration. |
| `Backend/` | LLVM IR and printer (`LLVMIR.lean`, `LLVM.lean`), LCB adapter, runtime and observation bridge. |
| `Driver/` | Project, reference and launcher boundaries in `Project.lean`, `Reference.lean` and `Launcher.lean`. |
| `Artifact/` | Canonical framing and digests in `Encoding.lean` and `Sha256.lean`. |

`tests/slice0/` holds active Concrete source fixtures. Compiler gates are in `scripts/checks/compiler/` and shared helpers in `scripts/checks/lib/`.

The root inventory (`scripts/checks/root_inventory.txt`) lists each new root entry in the same change that creates it. An unlisted entry fails the check, and so does a listed entry that is missing.

## Frontend specifications

- **Lexing.** `ConcreteSpec.Surface0.Lexes` is an independent byte-level judgment. `Lex.lex_correct` proves soundness against it. Individual token classes have scanner soundness, completeness and uniqueness lemmas listed in `Concrete/Premises.lean`.
- **The grammar as data.** Every grammar the parser uses is a `CheckedGrammar` with a proof
  that `ll1` returns `true`. The check in `ConcreteSpec/Spec/LL1.lean` verifies closure of the
  computed nullable, FIRST and FOLLOW sets and disjointness of the prediction sets.
  `ll1_sound` shows that at most one production of a nonterminal is predicted for any lookahead.
  Both the slice-0 grammar and the refusal grammar discharge the check by `decide`.
  The parser starts at the grammar's start symbol and accepts only a `CheckedTable`, whose
  entries must equal `makeTable` of that grammar. The parser uses the precomputed Slice 0 entries. `table0_checked` proves that they equal the generated table.
  Later slices extend the shared surface grammar and must pass the same LL(1) check.
- **`Derives : List Token → Surface0.Program → Prop`** binds token payloads as well as token kinds.
- **Elaboration.** `ConcreteSpec/Spec/Surface0.lean` defines `Lexes`, `Derives` and `checkBody`. The declarative `Elaborates` relation lives in `Concrete/Theorem.lean`. Preservation of evaluation by elaboration is the open premise `P_elab`.
- **Refusals.** The driver distinguishes syntax errors, decided forms outside the supported grammar, and checker refusals. The manifest exercises each class.

## Behaviours: the fuel-free semantic interface

Scalar Core, MIR and Memory are compared on outcomes. BIR's `behaviour` distinguishes exits, traps, divergence and stuck executions. The image proposition also compares observable traces and the diagnostic bytes reported by a native process. Its launch and liveness premises are listed below.

## Premises and target profile

The live definitions in `Concrete/Premises.lean` are `FrontendPremises` (parse and elaborate) and, under `Concrete.Native`, `BackendModel`, `LLVMPremises`, `World` and `Premises`. The original Slice 0 design sketch below uses earlier names. The current theorems are `Native.lcb_emit_correct` and `Native.observed_behaviour`.

```lean
/-- Claims about fixed frontend implementations, used only by the universal theorem.
    The certificate checks parsing and elaboration for each compilation (below). -/
structure FrontendPremises : Prop where
  /-- P_parse (implementation), until the generic table-parser theorem lands. -/
  parse : ∀ eof toks ast, Parse.parse eof toks = .ok ast → Surface0.Derives toks ast

/-- The target profile: the pinned triple, the toolchain store paths, the stack budget, and
    `maxSlots`, the largest BIR frame the profile vouches for under that budget. `compile` refuses
    a program whose frame exceeds `maxSlots` ("unsupported: exceeds the profile's frame bound").
    An LCB artifact also requires `Stages.prepareLCB` to pass the LCB admission profile. -/
structure TargetProfile where
  triple : String; toolchain : String; stackBytes : Nat; maxSlots : Nat

/-- What the body function produces: a 32-bit result word, or divergence. -/
inductive BodyResult | returns (w : BitVec 32) | diverges | stuck

/-- The encoding, parameterized by the program's trap-site table `tbl` (one definition, shared by
    both backends and the runtime). `exited c` (c < 256) ↦ c; a trap at site index i ↦ 256 + i.
    `tbl` is emitted by Runtime.lean and maps i to (kind, `SiteId`, span); i < tbl.size < 2^31 − 256.
    `decode tbl` maps a word below 256 to `exited`, a word 256 + i with i < tbl.size to
    `trapped (tbl.kind i)`, and every other word to `stuck` (invalid). Proved: `decode_encode` for
    every encodable behaviour, and `emit_encodes`: the body of a WF BIR returns only valid words. -/
def encode (tbl : SiteTable) : BIR.Outcome → Option (BitVec 32) := ...
def decode (tbl : SiteTable) : BitVec 32 → Behaviour := ...
def BodyResult.behaviour (tbl : SiteTable) : BodyResult → Behaviour
  | .returns w => decode tbl w | .diverges => .diverges | .stuck => .stuck
-- Everywhere below, `r.behaviour` abbreviates `r.behaviour b.sites` for the BIR `b` concerned.

/-- A backend: the Lean function from BIR to the body text a native toolchain consumes, or its
    refusal (then no artifact exists), and what that text means as a `BodyResult`. For LCB,
    `Means` is DEFINED from LCB's imported semantics, so the emit obligation is a theorem
    (`emit_correct`). For LLVM it is a parameter (a premise). -/
structure Backend where
  Text : Type
  emit : BIR.Program → Except Refusal Text
  Means : Text → BodyResult → Prop

/-- `LLVM.module` emits the complete module, including runtime and site table.
    Signed and unsigned types of equal width share a type spelling.
    `P_print` is the premise about the meaning of the exact emitted text. -/
def llvm (M : String → BodyResult → Prop) : Backend := ⟨String, fun b => .ok (LLVM.module b), M⟩
/-- LCB emits the adapted body paired with its site table for decoding the result. -/
def lcb : Backend := ⟨LCB.Source.Function × SiteTable, LCB.emit,
  fun (f, _) r => LCB.bodyResult f r⟩
/-- From LCB's own source semantics (`LeanCompilerBackend.Verified.Source`, imported, never
    restated), with no arguments: `returns w` iff `Source.HasBehavior f #[] (.returned (.word w))`,
    `diverges` iff `Source.HasBehavior f #[] .diverges`, and `stuck` otherwise. "Otherwise" covers
    an `initial` fault, a `.long` return and a stuck fault. It is covered by the digest gate. -/
def LCB.bodyResult (f : LCB.Source.Function) (r : BodyResult) : Prop := ...

/-- The native world for one backend, a parameter so its consistency is witnessable. -/
structure Native (B : Backend) where
  Image : Type
  Built : TargetProfile → B.Text → Image → Prop        -- image: body + runtime entry, pinned toolchain
  BodyRuns : TargetProfile → Image → BodyResult → Prop -- what the linked body returns in a launch
  Runs : TargetProfile → Image → Behaviour → Prop      -- the whole launch's behaviour

/-- `Behaviour` gains the native-only `refused` (startup refusal, status 125) and `killed`.
    No model program has either: slice 0 has no grants. An ADMITTED launch (`T`) excludes
    signals, ptrace and rlimits below `stackBytes`, and requires fd 2 to be open, writable and
    not to block forever. -/
structure NativePremises (B : Backend) (N : Native B) (T : TargetProfile) : Prop where
  /-- emit (implementation): an emitted body text means at most the BIR behaviour, encoded.
      LLVM: premise P_print. LCB: filled by `emit_correct` (no premise). -/
  emit : ∀ b, b.WF → ∀ t r, B.emit b = .ok t → B.Means t r → r.behaviour = BIR.behaviour b
  /-- P_toolchain[B] (toolchain): every result the linked body produces is one its text means.
      LCB: LCB's own importer, lowering, assembler and linker, i.e. its named trust boundaries
      (`docs/SEMANTICS.md`, "Target and environment assumptions"). -/
  toolchain : ∀ b t img r, b.WF → B.emit b = .ok t → N.Built T t img → N.BodyRuns T img r → B.Means t r
  /-- P_rt (implementation): relate the observed behaviour to the decoded body result. -/
  rt : ∀ b t img β, b.WF → B.emit b = .ok t → N.Built T t img → N.Runs T img β → β ≠ .killed →
    ∃ r, N.BodyRuns T img r ∧ β = r.behaviour
  /-- P_launch (environment, resources): exclude a killed launch within the frame bound. -/
  launch : ∀ b t img β, b.WF → b.frameSlots ≤ T.maxSlots →
    B.emit b = .ok t → N.Built T t img → N.Runs T img β → β ≠ .killed
  /-- P_live (environment): an admitted launch happens and has a behaviour (maximal: a run that
      never ends has behaviour `diverges`). -/
  live : ∀ b t img, b.WF → B.emit b = .ok t → N.Built T t img → ∃ β, N.Runs T img β
```

For a well-formed BIR within the frame bound, `Native.observed_behaviour` requires successful emission, a built image, emission correctness and the native premises. Liveness supplies an admitted launch, and the launch, runtime and toolchain premises relate its observed behaviour to BIR.

The native helper recognizes x86-64 Linux, AArch64 Linux and AArch64 macOS. The CI matrix exercises x86-64 Linux and AArch64 macOS. Production LCB toolchain correctness remains a named premise.

**The open premises for slice 0** (a generated view of the registry `Concrete/Premises.lean`, `Premises.all`, which also states each premise; gate 9 fails when this table drifts from it):

| Premise | Trust class | Retired by |
|---|---|---|
| P_parse | implementation | the generic table-parser theorem |
| P_elab | implementation | ANF and resolution preservation |
| P_print | implementation | an LLVM-IR semantics; not planned for slice 0 |
| P_toolchain[llvm] | toolchain | not planned |
| P_toolchain[lcb] | toolchain | LCB's own proofs (C01–C03 and beyond) |
| P_rt | implementation | a proof of the decoding entry against a libc model; not planned for slice 0 |
| P_trap_site | implementation | the same runtime-entry proof (owner, 2026-11-06: a premise now) |
| P_launch | environment | not planned |
| P_live | environment | not planned |

**Launch and trap reporting.** The registry prose mentions fd 2 under `P_launch`. The Lean field `Native.Premises.launch` concludes only `output ≠ .killed`. The exact bytes on fd 2 come from `Native.Premises.trapSite`, when that same admitted, observed launch returns the trap word and the table lookup succeeds. Native conformance compares the observed stderr bytes. The two field types are quoted from [Premises.lean](Premises.lean).

```lean
  launch : ∀ p artifact image launch output stderr, BIR.WF p → p.frameSlots ≤ maxSlots →
    backend.emit p = .ok artifact → world.built artifact image → world.launches image launch →
    world.admitted launch → world.observes launch output stderr → output ≠ .killed
  trapSite : ∀ p artifact image launch output stderr i entry, BIR.WF p →
    backend.emit p = .ok artifact → world.built artifact image → world.launches image launch →
    world.admitted launch → world.observes launch output stderr →
    world.bodyRuns launch (.returns (BitVec.ofNat 32 (256 + i))) → p.sites[i]? = some entry →
    stderr = Runtime.trapLine entry ++ "\n"
```

The certificate proposal would check parsing per compilation. The CLI currently checks the emitted LLVM link inputs (`component.ll` and `launcher.ll`) and replays the native build before publication. LCB receives its adapted function in process.

## The end-to-end theorem (`Concrete/Theorem.lean`)

[`Concrete/Theorem.lean`](Theorem.lean) states `ImageTheorem.compile_correct` as a proposition.
It binds the exact input bytes to declarative lexing, parsing and name-parametric elaboration.
Both binding rules and the compiler use the spec’s named-binder judgment. It excludes the
wildcard, the entry point, intrinsic and builtin type names, and earlier locals.
The conclusion includes the shared stage obligations and the directly proved LCB adapter obligation.
The scalar image proposition models `LLVM.module` as one complete module. The CLI instead compiles
`component.ll` and `launcher.ll` and links their objects. The cutover must bind the theorem to that
exact ordered pair and the resulting image. LCB adaptation has a proof for accepted bodies.
Toolchain, runtime and launch premises remain open.

[`Concrete/Contracts.lean`](Contracts.lean) derives reachable prefixes, terminal and stuck
states, and infinite runs from initialized transition systems. The lowering simulations
use those same transitions and account for steps that make no source progress. Maximal
refinement compares divergent traces as well as terminal observations. The path-indexed Core
model uses the spec evaluator. The BIR evaluator and imported LCB steps are interpreted in
[`Backend/Observation.lean`](Backend/Observation.lean), which contains the result-word
projection. Process observations include exit status, trap kind, diagnostic site and exact
stderr bytes. The separate origin relation binds the table entries to source byte ranges.

The BIR slot limit applies before emission. Final-image stack adequacy and launch admission
govern finite-prefix safety. Maximal behavior also needs OS liveness and progress through
short stderr writes and exit. The verifier selects the decoder, decoded-machine semantics and
raw-byte process semantics from its native target registry. The registration for the
profile's target is a hypothesis of `compile_correct`. Its conclusion requires both
backend claims, the unchecked claim and joint witnesses under the same reference, profile,
execution policy, models and tools. B0a and LP0 use the composition obligations stated in
`CompileCorrectUnderReference` with their specified platform reference and named residual
premises. The production registry is empty and the trusted-base inventory records no
discharged production profiles. LP0 / R-0505 owns production registration and the joint
non-vacuity witnesses for those profiles. The registered synthetic profile already has
joint witnesses for both backends, including their unchecked trap claims.
The per-path premise list records dispositions for the remaining obligations. The unchecked path requires static typing and
scope, and uses empty discharge evidence. Its conclusion requires the exact trap observation
under the run premises. Each backend has joint witness obligations for both production and
trapping conformance builds.

[`Concrete/Certificate.lean`](Certificate.lean) defines propositions for pre-link certificates, optional
notes, external receipts and refusal certificates. The validation propositions bind exact payload digests, resolved pins and evidence to fixed obligations.
The certificate design uses a single acyclic evidence graph to identify dependencies for revocation. Note or image validation
requires a registered reference and the verifier's policy. A driver-attested receipt without a
note requires equality to the linked bytes. An image-validation witness applies to the final bytes.
A conditional stack claim retains the stack premise. A proved-stack claim discharges it for admitted
launch environments. Launch, OS liveness and stderr progress remain premises. Canonical framing and
hashing have implementations. General certificate emission and bounded offline replay remain roadmap work.

The structural SSA validator checks operands, fresh definitions, ordered continuation
arguments, the complete trap-site entry, and the return. `SSA.lower_wf` establishes BIR
well-formedness, and `Stages.compile_ssa_correct` applies `SSA.lower_correct` to the recorded
Memory and BIR programs. Forward edges give the validator's block-count fuel bound.

### Supporting theorems

- `BIR.binOvf_spec` gives the wrapped result and the exact out-of-range flag when the operands resolve to the two integer arguments.
- `LCB.Proof.adapter_correct` and `emit_correct` apply to accepted well-formed BIR against LCB's imported `Source.Function` semantics. `Native.lcb_emit_correct` lifts the result to `Native.EmitCorrect`.
- `Stages.prepareLCB_of_edges` composes successful emission of the function and its site table with job validation.
- The full statement is `theorem answerCompiles : Stages.compile answerInput = .ok answerOutput`. It has no hypotheses. `ImageTheorem.Sanity.answerInput` loads the literal `r#"fn main() -> u8 { return 20 + 22; }"#.toUTF8` with the fixture's path as its name. The native test reads the file from disk, including its trailing newline. Editing the file does not change the theorem's input. [Theorem and embedded input](Theorem.lean) · [Native fixture](../tests/slice0/answer.con).

## Per-compilation certificate emission (proposed)

**Proposed certificate emission under R-0552.** The sketch would emit a Lean module per compilation and have the kernel check it:

```lean
def srcBytes : ByteArray := ...      -- the exact input
def expected : Stages.Output := ...  -- the exact tokens, AST, Core, evidence, MIR, Memory and BIR
-- one entry per edge, each checked by `decide` on that ONE stage function:
theorem e_lex   : Lex.lex srcBytes = .ok expected.toks := by decide
theorem e_parse : Parse.parse input.eof expected.toks = .ok expected.ast := by decide
-- … e_elab, e_admit, e_lower, e_memory, e_ssa
theorem cert_compile : Stages.compile input = .ok expected :=
  Stages.compile_of_edges input … e_lex e_parse e_elab e_admit e_lower e_memory e_ssa
-- An LCB artifact requires successful shared compilation and successful LCB preparation.
-- Compose the adapter and LCB profile checks to certify preparation.
theorem cert_lcb : Stages.prepareLCB expected.bir = .ok expectedLCB :=
  Stages.prepareLCB_of_edges expected.bir expectedLCB e_adapter e_profile
theorem cert_lexes : Surface0.Lexes srcBytes expected.toks := Lex.lex_correct e_lex
theorem cert_parse_elab : Surface0.derivesB expected.toks expected.ast &&
    Surface0.elabB expected.ast expected.p = true := by decide
```

The certificate keeps `e_lex` to establish the exact lexer output and derives `cert_lexes` from the universal theorem. The certificate design uses Boolean checkers with soundness lemmas for `Derives` and `Elab`. Together with `cert_lexes`, their judgments give `Denotes src p` for this compilation, so `compile_correct_cert` needs only `NativePremises`.

A **refusal certificate** proves the relevant stage returned `.error r` for each refused example.

**What is proved and what is attested:**
- The kernel would prove that the exact source maps to the shared output and check the requested backend inputs.
- The driver would compare the ordered LLVM link inputs (`component.ll`, `launcher.ll`) with their expected renderings, or hand LCB the checked value, and record the toolchain store paths and image digest.
- The association between the source, the text, the toolchain and the image, and therefore `Built`, is **attested** by the driver plus the nix store, and listed in the trust inventory. A digest is identity, not correspondence.

**Budget, and the per-pass design.** The time and size of each certificate are measured and gated. The LL(1) table is a precomputed constant.

The certificate format is a **list of per-edge entries**, composed by the proved `compile_of_edges` (above). In slice 0, each edge is still checked by `decide` running that stage in the kernel, so in total the kernel does run the whole compiler. That is accepted for **slice 0 only**. Later, each edge carries a witness that is small and linear in program size: a name-binding map, an ANF map, an SSA dominance witness, or a register assignment. A proved validator checks the witness, and the universal pass theorem composes with it. From then on, the kernel never re-runs the whole compiler (R-0552/R-0521).

## Stage registry (historical scalar design)

**Historical scalar registry design.** The indexed registry and its conditional composition theorem are in `Concrete/Interfaces/Pipeline.lean`. The scalar `Stages.compile` composes its stage functions directly. The indexed contracts and their composition are audited by `scripts/checks/compiler/stage_end_to_end_audit.lean`. The following snippet and gate describe the earlier proposal.

```lean
/-- The contracts are fixed per stage in ONE table and stated about the stage function itself. -/
def lowerContract (f : Core.Program × Tokens → Except Refusal MIR.Program) : Prop :=
  ∀ p t m, WellScoped p → TokensValid p t → f (p, t) = .ok m →
    m.WF ∧ MIR.behaviour m = Core.behaviour p
-- … one per stage: lex, parse, elab, admit (check + tokens), lower, memory, ssa,
--   and the two backend emits

inductive Evidence (C : Prop) where
  | proof (h : C)
  | frontend (h : FrontendPremises → C)                      -- P_parse, P_elab
  | native {ι} (B : ι → Backend) (C' : ι → Prop) (hC : C = ∀ i, C' i)
      (h : ∀ i N T, NativePremises (B i) N T → C' i)  -- the stage's OWN backend family

structure Stage (A B : Type) (Contract : (A → Except Refusal B) → Prop) where
  run : A → Except Refusal B
  evidence : Evidence (Contract run)
```

The contract is an index, not a field, and it is applied to `run`. So `Contract := True` is impossible unless the golden table says so, and an unrelated theorem doesn't typecheck. The contract table's definitions are covered by the digest gate. The backend emits are stages too. Each has `native` evidence for its own backend family, as the golden table fixes. For the adapter it is `ι := Unit`, `B _ := lcb`. For the printer it is `ι := (String → BodyResult → Prop)`, `B M := llvm M`, so the contract is `∀ M, …`, with `M` in scope. `emit_correct` turns LCB's evidence into `proof`. The glue between stages is stated as theorems:
- `pre₀`: any bytes are an admissible input;
- `postᵢ → preᵢ₊₁`;
- `compile_spec`.

The gate:
- enumerates the constants `compile` calls, independently, from its elaborated term;
- requires each one to be a registered stage, and each stage to be called;
- compares the premise fields with a golden list;
- compares the digests of the definitions those premises mention (`Behaviour`, the `behaviour` functions, `WF`, `frameSlots`, `TargetProfile`, `Native`) with a golden file.

Changing what a premise means therefore fails CI, just like adding one: holes only shrink.

## Facts query (planned)

**Proposed facts query under R-0575.** The following table specifies intended report contents. The public driver supports build and help. `Concrete/Driver/Project.lean` has an initial `mirFacts` helper.

**One derivation.** `derive : Accepted → FnId → Report` runs over the whole lowered bundle, so BIR slot counts are visible to it. Here `Accepted := { out // ∃ T src, compile T src = .ok out }`: facts are derived only for compiler-accepted outputs, never for arbitrary data. Each fact is a typed judgment `Fact.Holds`. `derive_sound` covers **every** status:
- `established` → `Holds`;
- `conditional(as)` → `Premises`-fields `as` imply `Holds`;
- `refuted(w)` → `w` is a checked counter-witness;
- `notEstablished(blocker, site)` → the site exists and `blocker` names a missing evidence kind;
- `notApplicable` → the fact's subject is absent;
- coverage counts → examined sites = enumerated sites.

Human output, JSON and future policies all render the same `Report`.

**The scope is explicit.** Each fact is a **source/Core fact** or an **artifact fact**. Artifact facts are about the executable, which includes the C runtime, startup and the linker. They are never unconditional in slice 0: at best they are `conditional` on the native premises, and otherwise `notEstablished`.

| Fact (scope) | Intended Slice 0 report |
|---|---|
| Authority: capabilities and grants (source) | `established`: requires no capabilities. This is about authority only, and never printed as "touches nothing". |
| Allocation (source) | `established`: none. |
| Allocation (artifact) | `notEstablished`: crt/startup are not modeled. |
| Memory reads/writes (source) | `established`: own locals only. |
| Implicit trap sites (source) | One per `+`, with a typed `SiteId` and a span. Each is `established` as "cannot trap", with the operator span, binder and known value, linked to the program’s admission evidence. |
| Trapping (artifact, behavioural) | `conditional` on the backend premises: no run traps. |
| Trap code present (artifact, structural) | `notEstablished`: no artifact inspection yet. Behavioural premises allow unreachable trap code in an observationally equivalent image. |
| `Result` errors, abort, allocation failure, foreign failure | `notApplicable`. |
| Trusted / FFI / opaque (source) | `established`: none. |
| Trusted / FFI / opaque (artifact) | `notEstablished`: no link-map or symbol-inventory evidence yet. Behavioural premises can't establish this, because an observationally equal image may still contain foreign code. |
| Termination (source) | `established` (`slice0_exits`). |
| Termination (artifact) | `conditional`, on the backend's own premises: LLVM `P_print, P_toolchain[llvm], P_rt, P_launch, P_live`; LCB `P_toolchain[lcb], P_rt, P_launch, P_live` (`emit` proved: `emit_correct`). |
| Recursion depth bound | `established`: 0, since there are no calls. |
| May block | `established`: no. |
| BIR frame slots (IR) | `established`: *n*. Reported as slots, never as bytes. |
| Stack bytes, stack exhaustion (artifact) | `notEstablished`: no final-artifact/target stack evidence. |
| Bounded (derived: terminates ∧ stack bound ∧ no blocking) | `notEstablished`, blocked on the stack bytes. |

**Identity and replay.** Every report carries:
- typed function and site identities;
- the schema version `concrete-facts/0` and the semantics version;
- the digests of the compiler, spec, target, backend (LLVM or LCB, with the pinned LCB rev) and artifact. Artifact facts are per backend artifact, and their premise identities are per backend;
- the replay command.

R-0575 would expose these reports per file or function through a facts subcommand.

## Gates

1. **Compiler build:** `lake build Concrete`, with warnings as errors and `autoImplicit` off.
2. **Axioms and trust.** The indexed LLVM boundary includes the opaque `llvmBehaviors` in `Concrete/Interfaces/Pipeline.lean`.
   The trust ban (`scripts/proofs/trust_ban.sh`) checks both `ConcreteSpec` and `Concrete`:
   sources, every constant, import-closure coverage and examples retained as named declarations.
   The compiler import allow-list enumerates existing modules under `ConcreteSpec`, `Concrete`, `Lean` and the pinned `LeanCompilerBackend`. It also permits the single pinned module `Std.Data.HashMap.Lemmas`, used by SSA. [Import check](../scripts/proofs/lean_lex.py).
   The trusted-base golden (gate 9) also fixes each headline theorem's axioms (a `sorryAx` fails it).
   R-0553 owns the compiler totality audit. R-0702 owns external re-checking of the spec and compiler proof closures, in coordination with R-0553. `scripts/proofs/external.sh` currently replays `ConcreteSpec` and `ConcreteSpecEvidence`.
3. **Premise registry and stage coverage.** The trusted-base gate checks the premise table. The indexed pipeline has a separate stage and end-to-end contract audit.
4. **Conformance, both backends:** each `bir:` row in `tests/manifest.tsv` runs through:
   - the Lean `run`;
   - the LLVM-native executable;
   - the LCB-native executable.

   All three must give the same observation, and each must match the row's `expect`. The conformance manifest includes hand-written BIR programs, Concrete source files and CLI cases.

   The [LLVM rendering fixtures](../scripts/checks/compiler/llvm_rendering.lean) compare
   full modules byte for byte against the [golden](../tests/golden/slice0_llvm.txt).
   The corpus includes every signed and unsigned integer tag, 64-bit narrowing
   with high bits set, raw BIR width boundaries and runtime byte escapes.
   The trusted-base audit records the theorem that LLVM's width mapping agrees
   with Core for every integer tag. The well-formed width fixture also runs
   natively and its observation must match the Lean BIR oracle.
   The mutation controls rebuild the modified printer and check the same golden.
   Native controls must disagree with the Lean BIR oracle on the changed exit
   value and on the exact trap line when the runtime routes to the wrong site.
   To compare the generated LLVM with the golden, run these commands inside `nix develop`.

   ```sh
   lake build Concrete
   llvm_fixture=$(mktemp)
   lake env lean --run scripts/checks/compiler/llvm_rendering.lean > "$llvm_fixture"
   diff -u tests/golden/slice0_llvm.txt "$llvm_fixture"
   ```

   An intended printer change needs a reviewed golden update.

   **Native observations.** The gate compares each run with its fixture's expected status and exact stderr bytes, and requires empty stdout. Three cases exercise shell status 134. [Manifest](../tests/manifest.tsv) · [Invalid-word control](../scripts/checks/compiler/slice0_bir.lean).

   | Case | Expected stderr | Meaning |
   |---|---|---|
   | Hand-written `bir:return134` | Empty | Normal exit 134 |
   | Trap fixture | The expected site's exact trap line plus one newline | Language trap |
   | Control returning word 257 with a one-entry table | `concrete: runtime: invalid result word` plus one newline | Runtime abort on an invalid word |

   A different byte sequence fails the fixture comparison. The comparator self-tests use forged status, stderr and stdout files for a status-134 trap. They exercise an exact match, newline, status and stdout mismatches, an unexpected timeout, runner failure and an unsupported profile. [Comparator and controls](../scripts/checks/compiler/tests.sh).

   Slice 0 has no grants and cannot produce a startup refusal. The native manifest has no normal exit-125 fixture, and the comparator has no startup-refusal control. The Slice 1 Lean reference-driver check exercises refusal before evaluation and checks status 125 for a missing or mismatched grant. [Reference check](../scripts/checks/compiler/slice1_driver.lean).

   The `llvm exit STATUS lcb resource RESOURCE BOUND MEASURED` row runs the public build command in each backend mode and checks the native LLVM exit and the LCB resource diagnostic. The CLI
   diagnostic rows check the diagnostic and exit status for an invalid backend, a non-canonical
   source path, an unknown target reported by the native helper, a failed native helper, and a failed build-input write.

   The compiler CLI exits 0 for a successful build or help, 1 for a source refusal
   (`SYNTAX`, `SLICE-0`, `CORE-CHECK`, or a `MANIFEST-*` refusal), 2 for an incomplete build
   (including lexer `RESOURCE` limits), 3 for `USAGE`, and 4 for `INTERNAL`.
   Native build failures use `incomplete (reason)`. This includes
   operational failures (`TOOL-FAILURE`) such as an unavailable native tool, a failed link,
   or a failed CLI output write. If stderr cannot carry a diagnostic, the CLI still exits 2.
   Compiler invariant failures use `INTERNAL`.

   The current gate checks manifest observations and refusal controls. A `pending-refusal` row checks the exit status and refusal class. A structured `refuse` row also checks the rule and source origins. Source trap fixtures must refuse admission before the conformance route exercises the residual check through both native backends.
5. **Source differential.** Seeded source fuzzing is planned.
6. **Unchecked differential.** The conformance gate compares the manifest trap observations, including stderr, with both backends on x86-64 Linux and AArch64 macOS.
7. **Certificates.** General acceptance and refusal certificate emission is a roadmap obligation.
8. **Reproducibility.** CI compares the emitted fixture artifacts from independent builds and compares the manifests from two Linux runners. See [Reproducible builds](#reproducible-builds-owner-2026-11-07).
9. **Trusted base.** `FrontendPremises` and `Native.Premises` are Lean structures in `Concrete/Premises.lean`. The gate prints `Premises.report`, including the prose premise registry, theorem axioms and their types. It compares the report and toolchain pins with `tests/golden/slice0_trusted_base.txt` and the README's open-premise table with `Premises.tableRow`.
10. **Sanitizers** (owner, 2026-11-07; UBSan dropped by the owner, 2026-11-08; implemented in `scripts/checks/compiler/slice0_artifacts.sh`): each paired executable fixture also runs built with LLVM ASan in CI, and must give the same observation with no sanitizer report. ASan instruments only functions marked `sanitize_address`, which clang's C front end adds and the printed IR lacks, so the sanitizer build adds it to every definition (the native helper's sanitizer mode); a control body with an out-of-bounds load must be reported. There is no UBSan leg: UBSan's checks are inserted by clang's C front end, so on LLVM IR input it checks nothing. No C is compiled in slice 0, and the LLVM-level undefined behaviour of the printed IR (poison, `unreachable`) stays P_print's.
11. **Binary size** (owner, 2026-11-07; implemented, same script): the size of each paired fixture's executable (bytes and text size, pinned `llvm-size`), per backend and target, is recorded per run in `sizes.tsv`, uploaded with the job. Report-only: it never fails a run. **Compile time** (owner, 2026-11-08; same script, report-only): each example's emit step (the Lean emitter, Lean start-up included) and native step (clang and LCB's command line) in wall-clock milliseconds, next to its budget (the step's time bound), in `compile_times.tsv`.
12. **Stack bound** (owner, 2026-11-07; implemented in the same script and `lib/stack_bound.py`; the `aarch64-macos` run check with LLDB, 2026-11-08): a stack bound for the analyzed Concrete code in each paired fixture's linked executables, checked against a run on the two CI targets. The analysis follows every path of the Concrete-owned code, instruction by instruction from an explicit set of modelled instructions, (`main`, `concrete_rt_write_all`, `concrete_body`) over the disassembly's control-flow graph, tracking the stack and frame pointers' depth below `main`'s entry and every byte addressed relative to them (so the red zone), refusing an access deeper than the target's red zone below the stack pointer (128 bytes on x86-64 System V and on Apple arm64, none under AAPCS64 on Linux: deeper bytes may be overwritten by a signal handler), and fails closed on anything it does not model (an instruction outside the set, another stack-pointer write, frame- or link-register writeback, an indirect branch or call, a stack pointer copied out, an indexed stack address, a stack read of bytes the function has not itself written on every path since its last call (so no stale saved frame pointer is ever loaded), a saved slot read other than by the modelled restore, two depths meeting, recursion, a call to a function neither owned nor a PLT/stub). The external calls (`write`, `_exit`, `abort`) are listed with the depth at their entry; their own use, and that `_exit` and `abort` do not return, are the libc premise. On x86-64 Linux the run check single-steps the owned code under the pinned gdb and requires the measured depth to be within the bound, for a complete run only (stopped first at `main`, back at each external call's return address, no signal, the expected exit status). The measured depth is the lowest of the stack pointer and every stack byte a stepped instruction addresses (`lib/stack_access.py` evaluates each memory operand), so a red-zone access is counted, and one deeper than the red zone is an error. A control links a `main` that stores at the red zone's edge without moving the stack pointer and requires bound 128 and measured 128 (on AArch64 it first allocates and frees a 144-byte frame with a pre-indexed store, judged against the new stack pointer, so 144); with a 120-byte red zone the same run gives no measurement. On Mach-O each stub entry's imported symbol comes from the indirect symbol table (the Mach-O indirect-symbol view of `llvm-objdump`), since the disassembly names only the `__stubs` section. On `aarch64-macos` the run check is the same, under Apple's LLDB (`/usr/bin/lldb`, `lib/stack_measure_lldb.py`; an external call's return address is the link register); LLDB is not pinned (Apple's signed debugserver is the only one allowed to attach), so its version is printed in the gate log. `stack.tsv` records bound, measurement and externals.

Facts-schema goldens are a roadmap obligation.

**Negative controls in the original gate design** (planned coverage includes):
- a deleted pass theorem;
- an unrelated theorem substituted as a stage's evidence;
- a silently omitted stage;
- an extra or strengthened premise;
- a changed premise-definition digest;
- a certificate for the wrong source, BIR, `.ll` or LCB-IR;
- a swapped executable or toolchain;
- a reject-all compiler;
- a printer that drops the overflow check (gate 6);
- an adapter that drops the overflow check (gate 6, LCB leg);
- an LCB output whose exit status disagrees with LLVM's (gate 4);
- a grammar edit that creates an LL(1) conflict;
- a lexer that splits a token;
- a facts derivation that reports a false status;
- `partial` added to a stage.

Heavy runs (native builds, differentials, certificates) run on concrete-ci-2 or GitHub runners, never as full local runs.

## Owner decisions (2026-11-06)

1. The November 6 CLI design included build and a planned per-function facts query.
2. A decided form outside Slice 0 (`0x2a`, `let mut`, `if`) gets an unsupported-form refusal. Uppercase `0x2A` gives a lexical `SYNTAX` error.
3. Leading-zero literals such as `007` are refused: one form per number. This is recorded in `ConcreteSpec/Spec/Surface0.lean`.
4. The trap site that the executable reports is a named premise now, `P_trap_site`, which a proof retires later.
5. Both backends are in slice 0. The default build requests both. A build checks the LCB profile
   only when it requests LCB (decided-2026-11-23-slice0-and-roadmap-answers, answer 6).
6. The [November 21 decision](../docs/decisions.md#decided-2026-11-21-secrets-after-1-0) defers the November 6 information-flow and constant-time design, including its proposed slice 0 security fact fields, until after 1.0.
7. OS sandboxing of the launcher is out of scope for the compiler.

8. The trap line is `concrete: trap: <kind> at <path>:<line>:<col> (site <id>)`: the path relative to the project root with forward slashes, line and column 1-based (owner, 2026-11-06). The compiler implements it in `Backend/Runtime.lean` (`trapLine`), from the site table's `Loc`; `P_trap_site` covers the executable writing it.
9. STMT-14 refuses the first statement after an unconditional return. The parser preserves the
   statement list and spans. The surface body check runs in elaboration before conversion to Core
   and gives `CORE-CHECK` with the span from the statement keyword through its semicolon.
   Legacy builtin names whose spelling has yet to be decided, such as `alloc_array`,
   `free_array`, `vec_pop` and `string_length`, remain ordinary identifiers
   (decided-2026-11-23-slice0-and-roadmap-answers, answers 1a and 2a).
   `byte_at` remains reserved under [Q4.6](../docs/decisions.md#decided-2026-11-05-core) and the [November 24 clarification](../docs/decisions.md#decided-2026-11-24-builtin-names).
10. The rule identity design pairs each Lean rule name with a revision. Its use throughout report headers and the proposed facts query is a follow-up.

## Follow-ups (recorded after the final review round, round 8)

These follow-ups record the earlier design. The roadmap governs staging.

- **Retire premises:**
  - parser soundness (P_parse), using the enforced `CheckedGrammar` and `CheckedTable`;
  - ANF and resolution preservation (P_elab);
  - a proof of the decoding entry against a libc model (P_rt).
- **P_toolchain[lcb].** Production LCB toolchain correctness remains a named premise on all three hosts recognized by the native helper.
- **Encoded result.** It works while LCB's verified profile has no calls, memory or traps. When it gains them, revisit the encoding: an external trap call, or a multi-function encoding.
- **SSA:** extend the slot-to-register relation and edge witnesses to joins, back edges, and
  call/return states as those constructs arrive. The slice-0 proof uses forward edges to bound
  termination. Later proofs will need to account for potentially unbounded execution.
- **Certificates:** move from per-edge `decide` to per-edge witnesses checked by proved validators (R-0552/R-0521), with con-leche/nanoda replay.
- **Artifact facts:**
  - a link-map and symbol inventory, for trusted/FFI/opaque code;
  - structural trap-code inspection;
  - stack bytes from the final artifact plus the launcher's reservation (R-0505 final-image checks);
  - final-image validation against LCB's ISA model.
- **Trusted base:** a minimal direct-syscall launcher written in Concrete and compiled through the verified path. It does no OS sandboxing (out of scope).
- **Mono:** when it lands, admission re-runs after it (Q6.3), and the tokens are re-validated for each instance.
- **Integrated Core:** when A2i/R-0571 lands, point `Concrete/Core.lean` at it (or delete it and import the spec directly).
- **The spec's pre-decision admission rule** (`knownViolates`-only refusal, `RangeOk`, R-0574). It does not bite slice 0, where every value is known. It must be closed before the language grows unknown values.
- **Generated views:** the stage, premise and fact tables here become views of the Lean registries, gated against drift.
- **Slice order after Slice 0.** The current slice sequence is the [Vertical slices table in ROADMAP.md](../ROADMAP.md#vertical-slices).
- **BIR well-formedness** makes dominance hold by construction: values cross blocks only as block arguments, every operand is defined earlier in its own block, and every non-entry block has a predecessor. That a well-formed program never reads an undefined register is proved (`run_progress`, `Concrete/BIR/Progress.lean`).

## What LCB must provide

LCB is our own project. The items below track work for LCB. Concrete pins LCB at
`1d2584c0f6cfa9ba10bd918bc62234c5878dd38f`. LCB's scaling gate covers numbered
validation and the x86-64 Linux and AArch64 macOS emitters. Separate compatibility
emitter cases scale the block count, phi count, and phi fan-in. The gate also
counts byte comparisons for long names that differ at the first byte as suffix
length and lookup count vary independently. LCB's `compiler-build.json` records
build identity `581c7ceb46686862f1d1c8f0b37db8a1de1658da681a60d7026be00673b50364`.
The pin also adds typed high-multiply proofs and an execution-closure audit. The audit rejects
reachable partial definitions and unaudited externs, requires equality proofs
using only standard axioms for reachable `csimp` substitutions, checks
that the shipped executables import only `Init`, `Std`, and backend modules,
and compares the stock Lean build's generated C and Lean module artifacts byte
for byte with the audit build. Wire packets use header v2 with semantics
`ssa-int-v2` / `a64-leaf-v2`. LCB refuses packets with the old header or
semantics identifiers. `CodeInput.version` stays 1.

**LCB follow-ups for the trusted base:**
1. A public entry `compileSource : Target → Symbol → Source.Function → Except Error Artifact`. Today the compiler builds LCB's `FunctionJob` itself (`Concrete/Backend/LCB.lean`, `assemble`).
2. The verified first path for `x86_64-linux`. It exists only for `aarch64-apple-darwin` leaf functions; x86-64 goes through the compatibility backend, a larger unverified base.
3. Larger resource limits on the first path: 64 blocks, 488 values and 480 definitions per function (`Verified/Canonical.lean`). Slice programs can exceed them. `BACKEND-LIMIT` reports
   `incomplete (LCB backend: RESOURCE: MEASURED exceeds limit LIMIT)` and exits 2. LLVM is
   available independently through `--backend llvm`. Unsupported adapter widths use
   `BACKEND-PROFILE` and also exit 2.
4. Object emission (LCB V04), so assembly text and the system assembler leave the path.

**Slice 2 prerequisites provided by LCB's first path (Apple AArch64).** LCB's verified profile now has `word` (32-bit) and `long` (64-bit) values, `add sub mul and or xor`, the shifts `shiftLeft logicalRight arithmeticRight` (count modulo the width), `mulHighUnsigned mulHighSigned` (accepted only at `long`), division and remainder (`unsignedDiv unsignedRem signedDiv signedRem`; a zero divisor or signed minimum by −1 is stuck, so the adapter must branch to a trap before them), and the comparisons `eq unsignedLT unsignedLE signedLT signedLE` (word result). The proposed Slice 2 adapter would carry every integer as a `long`: exact for types up to 32 bits; 64-bit overflow uses the high multiply and, for add/sub, `xor`/`and`/compare. Still absent and optional while every integer is a `long`: width conversion (`word`↔`long` extension and truncation).

**Still needed for the Linux targets:** compatibility reification (`Verified.reify`
in LCB's `Verified/Canonical.lean`) now returns `unsupportedBinary` for both
high multiplies. The compatibility IR still needs these operations, or Linux
needs a verified path that supports them, before the adapter can lower slice 2's
64-bit multiply overflow checks there. Slice 0 emits neither operation.

## Not in slice 0

- Mono Core.
- Grants, the launcher's startup refusal (125) and events.
- The LP0 proven path: LCB's own theorems composed with `adapter_correct`, replacing `P_toolchain[lcb]`. It comes right after the functions-and-calls slice (slice 4 in the earlier numbering, slice 5 now). In slice 0 LCB already runs, unverified, with the adapter proof first.
- An LLVM-IR semantics. The LLVM meaning `M` stays a premise parameter until one exists, and then `P_print` becomes a theorem.
- Information flow and constant time are deferred to [after 1.0](../ROADMAP.md#after-10).

## Reproducible builds (owner, 2026-11-07)

The same source, compiler revision, toolchain pins and target must produce byte-identical BIR,
LLVM text, LCB input and output, objects and executables. `nix develop --command bash
scripts/checks/compiler/build_identity.sh` prints the canonical identity: the compiler commit, a digest of
the working-tree content of every tracked input with `source_tree=clean|dirty` (so a local edit never
reports a committed identity), Lean, clang/LLVM, the Nix inputs, the LCB revision and the target. It
fails unless every dependency checkout is at the revision `lake-manifest.json` pins with no
modified or untracked (not ignored) file. The
planned `concrete facts` command will print this identity.

The reproducibility gate (`scripts/checks/compiler/slice0_repro.sh`) makes two independent builds from different source paths and TMPDIRs. Each compiles its dependencies and the LCB executable with the Lake artifact cache disabled. Each side checks the hand-written BIR inventory and emits both `bir:` and `con:` rows, including source refusals. It builds the executable rows natively, skipping CLI and backend-limit cases. The comparator checks the BIR and LCB dumps, emitted text, assembly, objects and executables byte for byte. Negative controls cover a flipped byte, a missing artifact and a differing inventory. The separate conformance gate also exercises the source front end.

Both native gates use `scripts/checks/lib/slice0_native.sh` (a build helper, not a gate): the Nix-pinned clang 21 (`CONCRETE_SLICE0_CLANG`; the
shell's general `clang` is unchanged), an environment reduced to `PATH`, `HOME` and `TMPDIR`
(reduced, not hermetic), fixed relative object names and link order, epoch 1, UTC/C locale, no
debug information, path prefix maps, and no ELF build ID or Mach-O UUID (`ZERO_AR_DATE=1` on
both paths). The printers traverse ordered lists/arrays (including stable first-occurrence
intrinsic deduplication); site paths are project-relative source data. They emit no clock,
hostname or temporary path. CI repeats this on x86-64 Linux and AArch64 macOS and compares the
complete Linux manifests from two independent runners. Different targets are never compared.
Changes to these pins change the build identity; no reproducibility claim spans different
toolchains or targets.

The native gates use `scripts/checks/lib/limits.sh` and `bounded_run`. A second timeout makes the gate incomplete. The Linux conformance and reproducibility gates cap per-process address space at 64 GiB. The artifact gate makes a separate allowance for ASan's shadow address space. On Darwin the CI host supplies the memory bound.
