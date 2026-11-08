# The Concrete compiler: slice 0 design (provisional)

PROVISIONAL. This is a design sketch, revision 8, written after eight Codex and agy review rounds. It folds in the Codex/agy/Kimi pipeline review and the owner's answers (2026-11-06). Build step 1 (BIR as the shared backend contract) is implemented: `Concrete/BIR.lean` and `Concrete/Backend/`, gate `scripts/checks/compiler/tests.sh`. Slice 0 is the first compiler slice. It is built before the owner freezes Core (A2c) and makes no normative claim before that freeze ([decision](../docs/decisions.md#decided-project-proof-infrastructure)). Branch `wip/slice0` depends on `wip/spec-root`, which puts `ConcreteSpec` on the new root, and is rebased onto main (under a new branch name) once that lands.

## Scope

This document describes **the compiler**: its stages, IRs, proof strategy, premises and gates. Language rules are not restated here. They live in `ConcreteSpec`, and this document links to them:
- the one Core (syntax and semantics) and the slice fragments, each a pair of decidable predicates `Fragment.surface` and `Fragment.core` (planned `ConcreteSpec/Spec/Fragments.lean`, A2i item I3); until those land, the current slice-0 file [`ConcreteSpec/Spec/Slice0.lean`](../ConcreteSpec/Spec/Slice0.lean);
- its behaviour theorems: [`ConcreteSpec/Proofs/Slice0.lean`](../ConcreteSpec/Proofs/Slice0.lean);
- what the parent process observes, including the trap status and the trap line: [`ConcreteSpec/Spec/Observation.lean`](../ConcreteSpec/Spec/Observation.lean);
- the slice-0 surface (token rules, grammar, elaboration): the new `ConcreteSpec/Spec/Surface0.lean` described below.

The test corpus is `tests/manifest.tsv`. Today's rows name hand-written BIR programs. Future `con:` rows name source files under `tests/`.

## The pipeline

Each arrow is a total Lean function of the compiler. **Proved** means a kernel-checked theorem. **Premise** means a field of one of the two premise structures (frontend, native) in the one file `Concrete/Premises.lean`.

```
source bytes (ByteArray)
  │ decode + lex        Lex.lean      proved against Surface0.Lexes (independent byte-level spec);
  ▼                                   until done: premise P_lex (implementation)
tokens (kind + payload + span)
  │ parse (LL(1))       Parse.lean    generic table-driven parser over Surface0.grammar;
  ▼                                   premise P_parse (implementation) until the generic theorem lands
surface AST
  │ resolve + elab      Elab.lean     resolve + elab: one stage edge, straight to the SPEC's Core
  ▼                                   (no slice-0 Core syntax); proved to satisfy Surface0.Elab,
                                      with freshness and naming-independence; a form outside
                                      `Fragment.surface s0` is "not supported in slice 0"
Core program + site map  (the spec's Core value, `out.p`, `out.sites`)
  │ check = ADMIT       `Fragment.core s0 p` and the spec's own `check`; `check_iff` gives
  ▼                     `Checked p`; the ONE admission point; emits discharged-evidence tokens
CheckedCore
  │ (mono: absent in slice 0; when it exists, admission re-runs after it, row Q6.3)
  │ lower               MIR.lean      Place MIR: expands RESIDUAL checks only, never admits;
  ▼                                   proved over ALL well-scoped programs with valid tokens
Place MIR  (`out.mir`)
  │ memory              Memory.lean   Memory CFG: the one Layout authority (sizes, alignment,
  ▼                                   offsets, loads/stores over block-offset memory); identity-shaped
                                      in slice 0 (u8 slots), proved: memory_correct
Memory CFG  (`out.mem`)
  │ ssa                 BIR.lean      BIR; slice 0: direct proof (constraint below)
  ▼
BIR  (`out.bir`)  ── designed for both backends from the start
  ├───────────────────────────────────────────┐
  │ print   Backend/LLVM.lean                  │ adapt   Backend/LCB.lean
  ▼ (unverified: premise P_print)              ▼ (proved: emit_correct against LCB's imported
.ll text  (`out.ll`)                           │  semantics; a refused adaptation emits nothing)
  │ clang/llc, link                          LCB `Source.Function` value (`out.lcb`), handed
  │ (premise P_toolchain[llvm])                │ to LCB IN PROCESS (no text on this path);
  │                                            │ LCB lowering + assembler + link
  ▼                                            ▼ (premise P_toolchain[lcb]: LCB's named trust boundaries)
executable (x86-64)                          executable (x86-64, AArch64 macOS)
  │ run: premises P_launch, P_live (environment), under the target profile
  ▼
observed behaviour  →  observation (exit status 0–255 / 125 startup refusal / 134 trap + fd-2 line)
```

**`compile` is defined as the composition of the stage functions listed in `Stages.lean`**, so a pass that isn't registered can't be in the pipeline. `Output` is pure data with `DecidableEq`: `{ toks, ast, p, sites, tokens, mir, mem, bir, ll, lcb }`. There are no proofs inside it, and every theorem is stated *about* `compile`.

There's no Mono Core, since slice 0 has no generics. When Mono exists, the admission check runs again on its output: substitution can expose new constants and cycles (row Q6.3).

**CheckedCore is the single admission point.** Trap admission, termination and fact derivation run there, and nowhere else. The checker also emits one **discharged-evidence token** per operation it proved safe; in slice 0 that is `known (v)` for a `+` whose value is statically known. Tokens are carried through MIR, Memory CFG and BIR. A token is validated (`token_valid`). Its soundness connects the checker's known value to the run-time operands through `knownExpr_sound`, because `knownExpr_u8` alone establishes only static knowledge. Only a validated token lets a pass omit a check. No pass removes a check without evidence (PASS-9).

**Place MIR** is where later slices put evaluation order, cleanup edges and the expansion of residual checks. In slice 0, an add with no valid token becomes an explicit `add.ovf` plus a branch to a trap site, and an add with a valid token becomes a plain add. Every accepted slice-0 program has tokens on all its adds, so its generated IR (MIR through BIR and both backend inputs) contains no trap path. Whether the executable contains trap code is an artifact fact, not established yet. Unchecked well-scoped programs, such as `200 + 56` in gate 6, have no tokens, so the check-and-trap path is still proved and tested.

**Memory CFG** is the one layout authority. It consumes the spec's byte encoding (`ConcreteSpec/Spec/Rep.lean`) and turns places into loads and stores over block-offset memory with provenance. In slice 0 every local is a `u8` slot (size 1, alignment 1), so the pass is identity-shaped. It exists from day one so that layout never leaks into SSA construction or the printers.

**Typed source identity, end to end.** The lexer and parser mint typed `SiteId`s and spans. Elaboration records an origin edge for every generated temporary in `out.sites`. Every MIR, Memory CFG and BIR instruction carries its origin, and so does every trap site. Diagnostics, facts, JSON and certificates all read this one evidence graph. Core itself has no positions yet (spec open items O1/O4), so the site map sits beside `out.p` without changing the spec's Core.

**Proof strategy per pass** (pipeline review, item 7):

| Pass | Strategy |
|---|---|
| Lexer | direct correspondence against `Surface0.Lexes`, not merely "total and re-prints" |
| Parser | one direct theorem for the generic table-driven LL(1) parser |
| Resolve | direct proof; HIR is only the resolved surface tree, no second checker |
| Elab | direct proof |
| Lower, Memory CFG | direct proofs |
| SSA | long-term, a proved validator. Slice 0 proves `ssa_correct` directly, on one condition: BIR's shape may not depend on that proof, so the validator can replace it without changing the IR. |
| BIR → LCB-IR adapter | direct proof |
| Mono, optimizations, register allocation, linking (later) | proved validators |

**One Core, no slice-0 syntax.** The spec has one Core syntax; a slice is two decidable predicates, `Fragment.surface` (the grammar productions it supports) and `Fragment.core` (the Core constructors it supports) (A2i plan, item I3). Elaboration produces the spec's Core directly. Admission checks `Fragment.core s0 p` together with the spec's `check`, and the slice theorems (`slice0_exits`, `slice0_never_traps`) are stated over `Fragment.core s0 p ∧ Checked p`. The "not supported in slice 0" refusal comes from `Fragment.surface s0`. The compiler keeps no Core syntax of its own and no `toCore`.

**Dependency.** If the integrated Core (A2i, R-0571) is not on main when elaboration is built, the compiler targets the current spec Core behind one thin interface file, `Concrete/Core.lean`: it re-exports the spec's Core type, `check`, `check_iff`, `run`, and the slice-0 fragment predicates, and defines nothing of its own. It is the only file that changes when the integrated Core lands.

**BIR is a complete backend contract.** Each item below is fixed in `BIR.lean`. Its slice-0 instance is in parentheses.
- **integer semantics** (`u8`/`w32`, wrapping arithmetic plus `add.ovf`);
- **layout, endianness, alignment and zero-sized values**, all taken from Memory CFG's `Layout` (little-endian; no aggregates and no ZSTs in slice 0);
- **internal and foreign calling conventions** (one parameterless body function returning a word; no foreign calls);
- **external events** (none);
- **trap realization**: the kind plus the typed site (an encoded result word; see the runtime below);
- **frame-accounting handoff**: BIR reports its frame slots. **Stack bytes come only from the final artifact plus the launcher's reservation**, never from BIR.

BIR is the one boundary that both backends read. It is designed for both from the start:
- block-argument SSA;
- `add.ovf` returns `(value, flag)`;
- `br`/`condbr`;
- a `trap k` terminator.

Every BIR construct has a direct LLVM spelling (`@llvm.uadd.with.overflow.i8`) and a direct LCB-IR expansion: a widened add, a compare, and a branch to the trap block. No construct exists only for one backend.

**Both backends run in slice 0.**

The **LLVM printer** is unverified and stays a premise (`P_print`) until an LLVM-IR semantics exists.

The **BIR → LCB-IR adapter** is the first backend proof target. `adapter_correct` states that the adapter's output, under **LCB's own semantics** (`LeanCompilerBackend.Verified.Source`, the `ssa-int-v2` profile, imported and never restated), has the BIR behaviour. It is proved (`Proof.adapter_correct`) for every WF program the adapter accepts (`adapt p = .ok f`). The backend contract is complete (owner, 2026-11-08): `LCB.emit : Program → Except String (Source.Function × SiteTable)` represents adaptation failure, `LCB.artifact` builds only from a successful `emit`, and `Proof.emit_correct` (success implies correctness), `Proof.artifact_of_emit_error` (a refusal produces no artifact, for every target) and `Proof.artifact_correct` (every artifact comes from a correct `emit`) state it. `P_adapter` is retired; the trusted-base golden (gate 9) records it.

The adapter's output is handed to LCB **in process**, as the value: on `aarch64-macos` through LCB's verified first path (`Verified.compileFunction`), on the Linux targets through LCB's compatibility backends (`Verified.reify`, then `AMD64`/`AArch64.emitModuleValidated`). No printer or importer sits between the adapter and LCB. The QBE text of the body is a debug dump only. The gate also runs the first path's admission on every example on every host.

**The `adapter_correct` proof, in layers** (in `Backend/LCB.lean`, namespace `Proof`):
1. *Values* (proved): `enc` represents an integer as its 32-bit word and a flag as 0/1; `binaryBits_exact`, `wrap_word` (the mask-and-rebias expansion computes `wrap t`) and `flag_word` (the unsigned compare is exactly `inRange`), over the profile's types.
2. *Instructions*: the invariant is: `env.size = f.valueWidths.size = valueCount p`, every width is `word`, and for every register `r` with BIR `lookup benv r = some v` (the newest binding: BIR's list shadows on loops where LCB overwrites a slot), `env[id r] = some (enc v)`, and every current integer binding `.int t x` is in range (`t.inRange x`: `Agrees` alone would let a `.u8` register hold `2^32`, whose word is 0, and then the overflow flags differ; immediates are in range by WF, results by `wrap_inRange`, and block arguments carry it). From the instruction's first position, the expansion's steps reach the next position with the invariant extended by the result (and flag); every operand read succeeds at width `word`, and BIR's operands stay in range (for `flag_word`). Needs: register ids injective, intermediate ids disjoint from register ids, every id below `valueCount`. Proved so far: the environment lemmas (`Agrees`, `read_operand`, `write_ok`, `agrees_set_other`, `agrees_define`) and the step lemmas for each expansion, from its first position with word-width slots: `bin_steps` (4 steps, `word (wrap t (x op y))` at `n + 3`), `binOvf_steps` (6 steps, also `enc (.bool (!t.inRange (x op y)))` at `n + 5`, for in-range operands) and `trunc_steps` (3 steps, `word (wrap to x)` at `n + 2`). With the numbering facts (`numberingOk`: intermediates are no register's id, every id is below `valueCount`), `inst_sim` carries the invariant (agreement, range, size) through any one instruction, with the result (and flag) bound, in `Inst.width ins` LCB steps (the adapter's `Inst.width`). Layer 3 so far: the adapted body's shape (`adapt_block`, `adapt_widths`, `block_insts`: instruction `i`'s expansion starts at `instPos b i`; `block_size`: the terminator follows the last one) and `body_sim`: the LCB steps of a block's instructions keep the invariant through BIR's `execInsts`. Terminators: `ret_sim` and `trap_sim` (LCB returns `enc v`, and the word `256 + s`), and `transfer_ok`: along an edge whose every phi has one arm from the predecessor (WF's `t ≠ e`), LCB reads every phi from the old environment and then writes them. `edge_sim`: along the only edge `j → t` (WF's `t ≠ e` makes a taken `condbr` arm the only one), LCB's `transfer` lands at `t`'s first position with the invariant for BIR's `enter`: each parameter's slot holds its new value, every other register keeps its slot. `runBlock_sim` composes them over BIR's `runBlock` (by fuel): an exit or a trap is matched by LCB returning its encoding, running out of fuel by at least `fuel` LCB steps (each block transition is at least one LCB step: the terminator's `transfer`). Layer 4: `initial_ok` (the entry block, no phis since WF's entry has no parameters, an empty environment of size `valueCount p`) and **`adapter_correct_run`**: for `WF p` and `adapt p = .ok f`, `run p fuel = exited c` gives `HasBehavior f #[] (.returned (word c))`, `trapped s` gives `.returned (word (256 + s))`, and running out of fuel at every fuel gives `.diverges` (`steps_prefix`). All under LCB's own `Source` semantics, with only `propext`, `Classical.choice` and `Quot.sound`. Then, closing the statement:
   - *BIR progress* (`Concrete/BIR/Progress.lean`): `run_progress`: for `WF p`, every `run p fuel` is `outOfFuel` or `Outcome.Encodable` (an exit below 256, a trap at a site of a table of at most `maxSites`), so never `stuck`; the invariant `Typed` (every register in the block's scope is bound to a value of its type, in range) is block-local, extended by each instruction as `Block.wf` extends the scope and established at entry from the arguments (registers distinct, from WF's `eraseDups` check). `behaviour_diverges_iff` (any program): `behaviour p = .diverges` iff `run p fuel = .outOfFuel` at every fuel.
   - *Uniqueness*: `hasBehavior_unique` (from LCB's `step_deterministic`): a function has at most one `Source.HasBehavior` on given arguments (two returns return the same value; a return excludes divergence).
   - *Bridge*: `adapter_correct_behaviour`: the adapted body returns the word `w` with `behaviour p = decode p.sites w` (by `decode_encode`, which keeps the site lookup and the trap kind), or diverges exactly when BIR diverges.
   - **`adapter_correct`**: `WF p → adapt p = .ok f → bodyResult f r → r.behaviour p.sites = behaviour p`, with `bodyResult` defined in `Backend/LCB.lean` as below. Axioms: `propext`, `Classical.choice`, `Quot.sound`.
   - **`emit_correct`**: `WF p → emit p = .ok (f, tbl) → bodyResult f r → r.behaviour tbl = behaviour p`, with `artifact_of_emit_error` and `artifact_correct` for the artifact (no premise).
   - *Adapter domain*: WF alone does not give `adapt p = .ok f` (WF admits `i32`/`u32`/64-bit instructions, which slice 0's adapter rejects); the statement takes the adapter's acceptance as a hypothesis, and widening the domain is LCB's 32↔64 work (wire version 3).
3. *Blocks*: `execInsts` against those steps; `ret` and `trap` against LCB's `returned`; `br`/`condbr` against `transfer`: the target exists, each phi has exactly one arm from this predecessor (WF's `t ≠ e`), destinations are distinct (distinct registers), and all arms are read from the OLD environment before any destination is written, which is BIR's `enter` (all arguments evaluated before binding), including loop-carried swaps; the new state is at position 0.
4. *Runs*: under `WF p` and `adapt p = .ok f`, `Source.initial f #[] = .ok s₀` (entry block exists with no phis, no parameters, an empty environment of size `valueCount p`); then `run p fuel = exited c` or `trapped s` gives `HasBehavior f #[] (.returned (word (encode o)))`. Divergence: each BIR block transition is at least one LCB `Step` (BIR fuel counts blocks, LCB counts instructions and terminators), so BIR running out of fuel at every fuel gives arbitrarily long LCB prefixes, i.e. `HasBehavior f #[] .diverges`. BIR progress (a WF program never gets stuck) is needed only to make exit, trap and divergence exhaustive.

Two refactors make layers 2–4 tractable, and both are done: `runBlock` names blocks by index (as LCB's state does), and the numbering is a closed function of position (`blockBase`, `instBase`, `regId`), which LCB's canonical-form check accepts for every example.

LCB's own lowering, assembler and linker are LCB's named trust boundaries, carried as `P_toolchain[lcb]` until LCB's preservation theorem covers the slice-0 domain.

LCB is a pinned lake dependency (`require` with an exact rev, LCB's upstream lock recorded). It uses the same Lean toolchain (v4.34.1) and no Mathlib.

**The encoded result, with one decoding runtime.** Neither backend's input contains a call or a trap.

Each backend emits `main`'s body as a function `concrete_body() -> w`, a 32-bit word, whose return value **encodes** the behaviour:
- `exited c` returns `c` (0–255);
- a trap at site `s` returns `256 + s.idx`. The runtime's site table maps the index to the trap's kind, typed `SiteId` and span.

This fits LCB's verified source profile (`ssa-int-v2`, `LeanCompilerBackend/Verified/Source.lean`), which has integer operations, compares, branches, phis, a return or divergence, and no calls, traps or memory. It is also what lets the adapter be proved against LCB's semantics as it stands.

It also separates `return 134` from a trap: they return the words 134 and 256.

A fixed runtime entry, `Backend/Runtime.lean`, is emitted per backend as LLVM text and as QBE text for LCB's command line (the runtime is not on the adapter's proof path; it is premise `P_rt`). It calls `concrete_body`, and then:
- for a word below 256, it exits with that status;
- otherwise, it writes the trap line for `k` to fd 2 and exits 134.

The decoding lives in this one place for both backends. Its correctness is premise `P_rt` (implementation, over libc `write`/`_exit`): an image whose `concrete_body` returns word `w` has the behaviour `decode w`, and a body that diverges gives a run that diverges.

Each IR has a total, fuel-indexed evaluator returning the spec's `Outcome` and a decidable well-formedness predicate.

## Layout (lean root, few cohesive folders)

**New root entries.** There are only three, each with a stated reason:

| Entry | Purpose |
|---|---|
| `Concrete.lean` | The library root that lake requires; it imports every stage. It is the same pattern as `ConcreteSpec.lean`. |
| `Concrete/` | The compiler: one flat folder, one file per pipeline stage or concern. |
| `tests/` | The test corpus. `manifest.tsv` declares each program, its expected outcome and its owning slice. Source programs live under `tests/`. |

**Changes to existing entries.** No new entry is created for any of these:
- `lakefile.toml` gains the `Concrete` lib, the `concrete` exe, and a pinned `require` of `lean-compiler-backend`; `lake-manifest.json` records it.
- `ConcreteSpec/Spec/Surface0.lean` and `ConcreteSpec/Proofs/Surface0.lean` go into the spec's existing folders.
- The gates and runners (conformance, the two differentials, certificates, the premise registry, negative controls) are files in the existing `scripts/checks/`, in its subfolder `scripts/checks/compiler/`: they are CI jobs of their own (they need the compiler's devshell and a macOS runner), not root checks, which are exactly the `scripts/checks/*.sh` that `run.sh` runs. Their shared helpers are in `scripts/checks/lib/`. No `scripts/compiler/` is added.
- `required-checks.txt` and the CI workflow gain entries.

Generated outputs are never committed and get no new root entry. That covers certificates, `.ll` files, images and facts JSON, which all go under the git-ignored `.lake/concrete/`.

**The folder tree:**

```
Concrete.lean          library root (imports only)
Concrete/
  README.md            this design
  LL1.lean             generic LL(1) check `ll1` and `ll1_sound` (built; moves beside the spec's
                       grammar when A2i places it)
  Lex.lean             UTF-8 decode + lexer, and its theorem against Surface0.Lexes
  Parse.lean           generic LL(1) table parser over Surface0.grammar
  Core.lean            thin interface to the spec's Core and the slice fragment predicates (until
                       the integrated Core lands; defines no Core of its own)
  Elab.lean            resolve + elaborate to the spec's Core (one stage edge), and its theorems
  MIR.lean             Place MIR: syntax, semantics, WF, lowering (residual checks only), lower_correct
  Memory.lean          Memory CFG: the Layout authority, block-offset memory, memory_correct
  BIR.lean             BIR: syntax, semantics, behaviour, WF, ssa, ssa_correct, addOvf_correct
  Backend/             the backend stage: everything after BIR, one file per backend
    LLVM.lean          BIR → LLVM text printer (unverified; P_print)
    LCB.lean           BIR → LCB `Source.Function` adapter + adapter_correct; in-process assembly
    Runtime.lean       the fixed entry that decodes the result word into exit status / trap line, per backend
  Stages.lean          typed stage contracts; `compile` is the fold of the stage list
  Premises.lean        THE one file of premises, plus the execution-trust inventory
  Theorem.lean         compile_correct, the non-vacuity witnesses, answer_correct
  Facts.lean           per-function facts: judgments, derive, derive_sound, rendering (text/JSON)
  Cert.lean            per-compilation certificate emitter
  Main.lean            CLI driver (IO, toolchain invocation); placeholder spellings
tests/
  manifest.tsv         one row per test: program, expected outcome and owning slice
  slice0/              future *.con sources; their outcomes stay in manifest.tsv
  golden/              golden outputs, not test programs
```

**Rules:**
- `Concrete/` has one subfolder, `Backend/`, because the backend stage has three files. No other subfolder exists until another stage outgrows one file, and a split never creates a one-file folder.
- Each stage's definitions and theorems live in the same file, so each fact has one home.
- `Resolve` merges into `Elab`, because it is one stage edge with one contract.
- Every entry has exactly one home: tests are in `tests/`, gates in `scripts/checks/` (the compiler's in `scripts/checks/compiler/`), the spec in `ConcreteSpec/`. Conformance has one home, `tests/`.
- The stage table, the premise table and the fact-kind table in this README become **generated views** of the Lean registries in `Stages.lean`, `Premises.lean` and `Facts.lean` once the code exists. A gate fails when a view drifts from its registry.

The root inventory (`scripts/checks/root_inventory.txt`) lists each new root entry in the same change that creates it. An unlisted entry fails the check, and so does a listed entry that is missing.

## Frontend specifications (new spec file `ConcreteSpec/Spec/Surface0.lean`)

This file is spec work (open item O1, slice 0 only). It is a new file, and no existing spec file changes. The language content lives in the Lean file, not here: the token rules (SYN-5/SYN-6, plus the owner's decision that leading-zero literals are refused), the grammar and the elaboration relation. What the compiler needs from it:

- **`Lexes : Bytes → List Token → Prop`**, an independent byte-level judgment. The lexer is proved sound and complete against it, which rules out splitting a token.
- **The grammar as data**, which must be LL(1), plus **`ll1 : Grammar → Bool`**, which computes nullable, FIRST and FOLLOW sets. It comes with `ll1_sound : ll1 g = true → ∀ n t, at most one production of n is predicted on t`, where "predicted" is defined declaratively (inductive nullable, FIRST and FOLLOW relations). Both are built, generically, in `Concrete/LL1.lean`: the check verifies that the computed sets are closed under the rules (so they contain the declarative relations) and that each nonterminal's prediction sets are disjoint; the fixed-point iteration needs no proof. The fact `ll1 grammar0 = true` is proved by `decide`. The generic table parser is proved against `Derives`.
- **`Derives : List Token → Surface.Program → Prop`**, which binds token payloads, not only token kinds.
- **`Elab : Surface.Program → Core.Program → Prop`**, with the compiler's theorems `elab_fresh` (temporaries never collide with user names) and `elab_rename_invariant` (renaming temporaries changes neither acceptance nor outcome).
- **Refusal classes**, as the spec defines them:
  - a syntax error;
  - "not supported in slice 0", for a decided form outside `Fragment.surface s0` (owner, 2026-11-06);
  - a checker refusal.

  The compiler reports exactly the class that the spec judgment gives.
- **`Denotes src p := ∃ toks ast, Lexes src toks ∧ Derives toks ast ∧ Elab ast p`.** Its theorem is `denotes_behaviour_det` (see the theorem section).

## Behaviours: the fuel-free semantic interface

Every level (Core, MIR, Memory CFG (`Mem`), BIR) exposes a **maximal behaviour** derived from its fuel-indexed evaluator:

```lean
inductive Behaviour | exited (c : Nat) | trapped (k : Trap) | diverges | stuck | refused | killed
-- `Behaviour` carries the trap KIND only, matching Core. The trap SITE is tracked separately, as
-- the `SiteId` of the trapping instruction, and `trap_site_correct` relates it to the
-- origin of the add in `out.sites`.
def X.behaviour (p) : Behaviour   -- exited/trapped/stuck if some fuel reaches it, else diverges
```

These definitions are noncomputable and used only in statements. Fuel monotonicity, which the spec already proves for Core as `runBody_mono`, makes `behaviour` well-defined. It also gives `run_exit_behaviour : run p n = .exit c → Core.behaviour p = .exited c`.

Preservation is stated over behaviours, so a lowering may take a different number of steps from its source. `refused` (a startup refusal, status 125) and `killed` are never the behaviour of a slice-0 model program; they exist only for native runs. `Behaviour.observe` maps behaviours to the spec's `Observation`, sharing `Observation.status`.

## Premises and target profile (`Concrete/Premises.lean`: the one file)

The premises fall into two separate structures, because they are different kinds of claims.

```lean
/-- Claims about fixed frontend implementations, used only by the universal theorem.
    Per compilation, the certificate discharges both by kernel `decide` (below). -/
structure FrontendPremises : Prop where
  /-- P_lex (implementation), until `lex_correct` lands. -/
  lex   : ∀ src toks, Lex.lex src = .ok toks → Surface0.Lexes src toks
  /-- P_parse (implementation), until the generic table-parser theorem lands. -/
  parse : ∀ toks ast, Parse.parse toks = .ok ast → Surface0.Derives toks ast

/-- The target profile: the pinned triple, the toolchain store paths, the stack budget, and
    `maxSlots`, the largest BIR frame the profile vouches for under that budget. `compile` refuses
    a program whose frame exceeds `maxSlots` ("unsupported: exceeds the profile's frame bound").
    `compile` also refuses a body that LCB's admission profile would reject (at most 64 blocks, 480
    definitions and a 4080-byte frame), so every accepted program has an admitted LCB form. -/
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
  emit : BIR.Program → Except String Text
  Means : Text → BodyResult → Prop

def llvm (M : String → BodyResult → Prop) : Backend := ⟨String, fun b => .ok (LLVM.print b), M⟩
/-- The emitted LCB text is the adapted body PAIRED with its site table, so the table that
    decodes the result is part of the emitted artifact. (The LLVM text already contains its
    table, and printer injectivity covers it.) -/
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
  /-- P_rt (implementation): the runtime entry decodes. A launch whose body returns `w` has the
      behaviour `decode w`: it exits with w below 256; otherwise it writes the whole trap line to
      fd 2 (looping on partial writes, ignoring write errors) and then exits 134 unconditionally.
      A body that diverges gives a launch that diverges. The trap line's presence also relies on
      the admitted-launch fd-2 condition, which belongs to P_launch, not here. -/
  rt : ∀ b t img β, b.WF → B.emit b = .ok t → N.Built T t img → N.Runs T img β → β ≠ .killed →
    ∃ r, N.BodyRuns T img r ∧ β = r.behaviour
  /-- P_launch (environment, resources): an admitted launch of an image built from a WF BIR
      within the profile's frame bound is never killed (stack, signals), and fd 2 accepts the
      trap line. -/
  launch : ∀ b t img β, b.WF → b.frameSlots ≤ T.maxSlots →
    B.emit b = .ok t → N.Built T t img → N.Runs T img β → β ≠ .killed
  /-- P_live (environment): an admitted launch happens and has a behaviour (maximal: a run that
      never ends has behaviour `diverges`). -/
  live : ∀ b t img, b.WF → B.emit b = .ok t → N.Built T t img → ∃ β, N.Runs T img β
```

Every premise is used in the composition:
- `launch` excludes `killed`;
- `rt` reduces the launch to the body's result;
- `toolchain` takes that result to the text's meaning;
- `emit` takes the meaning to the BIR behaviour;
- `live` makes all of this non-vacuous for the image.

There are two target profiles: `x86_64-linux` and `aarch64-macos` (LCB's first native proof profile). The LLVM backend runs on `x86_64-linux` (on concrete-ci-2) and as a cross-check on `aarch64-macos`. The LCB backend runs on both.

**The open premises for slice 0** (a generated view of the registry `Concrete/Premises.lean`, `Premises.all`, which also states each premise; gate 9 fails when this table drifts from it):

| Premise | Trust class | Retired by |
|---|---|---|
| P_lex | implementation | `lex_correct` |
| P_parse | implementation | the generic table-parser theorem |
| P_print | implementation | an LLVM-IR semantics; not planned for slice 0 |
| P_toolchain[llvm] | toolchain | not planned |
| P_toolchain[lcb] | toolchain | LCB's own proofs (C01–C03 and beyond) |
| P_rt | implementation | a proof of the decoding entry against a libc model; not planned for slice 0 |
| P_trap_site | implementation | the same runtime-entry proof (owner, 2026-11-06: a premise now) |
| P_launch | environment | not planned |
| P_live | environment | not planned |

The fd-2 condition is part of the admitted launch, so it is inside P_launch (environment).

**The trap site the executable reports is covered by premise `P_trap_site`** (owner, 2026-11-06). `trap_site_correct` proves the site at IR level. The executable's report is assumed: `Native` gains `ReportsSite : TargetProfile → Image → SiteId → Prop`, and a field states that a launch whose body returns `256 + i` writes exactly `Runtime.trapLine tbl[i]` and a newline on fd 2 (the kind, the `Loc` and the site id of entry `i`). Gates 4 and 6 test it. A proof of the runtime entry retires it, together with `P_rt`.

P_lex and P_parse are also discharged for each compilation by the certificate.

The **execution-trust inventory** is in the same file, as documented non-Prop entries:
- the Lean compiler and runtime executing `compile`. The certificate closes this for each output.
- the driver's IO: the byte-for-byte comparison of the emitted `.ll` file, the in-process hand-off of the LCB `Source.Function` value, and the toolchain invocation and its provenance record. These are attested, not proved (see the certificate section).
- LCB's own native execution: it lowers the `Source.Function` value to assembly in the compiler's process, as Lean-compiled code. This is inside `P_toolchain[lcb]`.

## The end-to-end theorem (`Concrete/Theorem.lean`)

```lean
/-- What `compile` returns: pure data, every intermediate recorded. -/
structure Output where
  toks : List Token; ast : Surface.Program
  p : Core.Program; sites : SiteMap; tokens : Tokens; mir : MIR.Program; mem : Mem.Program
  bir : BIR.Program; ll : String; lcb : LCB.Source.Function

/-- One theorem for both backends: `B` is `llvm _` or `lcb`. -/
theorem compile_correct (F : FrontendPremises) {B N T} (P : NativePremises B N T)
    {src : ByteArray} {out : Output} {img : N.Image}
    (hc : compile T src = .ok out) (he : B.emit out.bir = .ok t) (hb : N.Built T t img) :
    Surface0.Denotes src out.p ∧ Fragment.core s0 out.p ∧ Checked out.p ∧
    ∃ c, c < 256 ∧ run out.p (Core.fuel out.p) = .exit c ∧
      (∃ β, N.Runs T img β) ∧ ∀ β, N.Runs T img β → β = .exited c
```

**Composition, each step a stated theorem:**
1. `compile_spec`: `compile T src = .ok out` implies:
   - `Lex.lex`, `Parse.parse` and `Elab.elab` succeed and produce `out.p`;
   - `Fragment.core s0 out.p` and `check out.p = true`;
   - `out.tokens = admit out.p`, `out.mir = lower out.p out.tokens`, `out.mem = memory out.mir`, `out.bir = ssa out.mem`, `out.ll = LLVM.print out.bir` and `LCB.emit out.bir = .ok (out.lcb, out.bir.sites)`;
   - `out.bir.frameSlots ≤ T.maxSlots`.
2. `F.lex`, `F.parse` and `elab_sound` give `Denotes src out.p`. `check_iff` gives `Checked`.
3. `slice0_exits` gives `run … = .exit c` with `c < 256`; `run_exit_behaviour` then gives `Core.behaviour = .exited c`.
4. `checked_wellScoped : Checked p → WellScoped p`. `WellScoped` includes in-range literals, because Core's `run` does not range-check literals.
5. `admit_tokens_valid : Checked p → TokensValid p (admit p)`. The empty token set is valid for any well-scoped program.
6. `lower_wf`, and `lower_correct : WellScoped p → TokensValid p t → MIR.behaviour (lower p t) = Core.behaviour p`. This holds for every well-scoped program with valid tokens, checked or not, so `200 + 56` with no tokens preserves `trapped .arithmeticOverflow`. The trap path is real.
7. `memory_wf` and `memory_correct : m.WF → Mem.behaviour (memory m) = MIR.behaviour m`. Then `ssa_wf` and `ssa_correct : g.WF → BIR.behaviour (ssa g) = Mem.behaviour g`.
8. `P.launch` excludes `killed`. `P.rt` gives a body result `r` with `β = r.behaviour`. `P.toolchain` gives `B.Means t r`, and `P.emit` gives `r.behaviour = BIR.behaviour out.bir = .exited c`. `P.live` gives existence.

**Supporting theorems:**
- `addOvf_correct`: for arbitrary `u8` operands, the result is `(a+b) % 256` and the flag is `a+b ≥ 256`.
- `adapter_correct : b.WF → LCB.adapt b = .ok f → LCB.bodyResult f r → r.behaviour b.sites = BIR.behaviour b` (proved, `Backend/LCB.lean`), over all WF BIR the adapter accepts, overflow included, against LCB's **raw** `Source.Function` semantics (`LeanCompilerBackend/Verified/Source.lean`, imported, never restated). `emit_correct` lifts it to `LCB.emit` and fills `NativePremises lcb`'s `emit` field.
- `adapt_admitted : compile T src = .ok out → LCB.admit (LCB.adapt out.bir) = .ok _`. It is separate from semantic preservation and uses LCB's own admission function, so it covers every profile check: SSA and dominance validation, reachability of all blocks, exact widths, supported operations only, at most 64 blocks, 480 definitions and the frame bound. It is proved, or, until then, checked for each compilation by `decide` in the certificate.
- `lower_emits_check`: every MIR add without a valid discharged-evidence token is followed by a branch on its flag to a trap site of kind `arithmeticOverflow`, with that add's origin.
- `denotes_behaviour_det`: two denotations of the same `src` have the same `Core.behaviour`. This is stated over behaviour, not at a fixed fuel.

**Program correctness transports**, through the acceptance witness `answer_accepts : compile T₀ answerSrc = .ok answerOut`:

```lean
theorem answer_correct (F : FrontendPremises) (P : NativePremises B N T₀)
    (he : B.emit answerOut.bir = .ok t) (hb : N.Built T₀ t img) : (∃ β, N.Runs T₀ img β) ∧ ∀ β, N.Runs T₀ img β → β = .exited 42
```

**Non-vacuity, one witness per layer:**
1. **Acceptance:** `answer_accepts`, by kernel `decide`.
2. **Native model consistency, per backend.** For LLVM, `NativePremises (llvm refMeans) refNative T₀` is proved for a reference model:
   - `Image := BIR.Program`;
   - `Built T ll b := b.WF ∧ b.frameSlots ≤ T.maxSlots ∧ LLVM.print b = ll`;
   - `BodyRuns T b r := r = BIR.bodyResult b`, where `bodyResult` returns `encode (behaviour b)` or diverges;
   - `Runs T b β := β = BIR.behaviour b`;
   - `refMeans ll r := ∃ b, b.WF ∧ LLVM.print b = ll ∧ r = BIR.bodyResult b`.

   The `rt` field follows from `decode_encode`. The `emit` field needs **printer injectivity**, which is proved: `LLVM.unprint (LLVM.print b) = some b` for every WF `b`. That also makes the `.ll` text determine its BIR, which the certificate uses.

   For LCB, `Means` is fixed by LCB's semantics. The witness `NativePremises lcb refNativeLCB T₀` uses:
   - `Image := LCB.Source.Function × SiteTable`, which is the emitted text itself;
   - `Built T t img := img = t ∧ frame bound`;
   - `BodyRuns T (g, _) := LCB.bodyResult g`;
   - `Runs T (g, tbl) β := ∃ r, LCB.bodyResult g r ∧ β = r.behaviour tbl`.

   Because a successful `emit b` is `(f, b.sites)` (`Proof.emit_ok`), a built image carries exactly `b.sites`, so `rt` holds by definition and no adapter injectivity is needed. `emit` is `emit_correct`.

   `live` holds because LCB's step relation is deterministic, so a run returns, faults or diverges. The witness follows from `emit_correct`.
3. **Frontend premises:** these are claims about fixed code, not something a model can satisfy, so no model witness applies. They are discharged per compilation (next section), and universally once `lex_correct` and `parse_correct` land.
4. **Native-build existence:** this is attested, not proved. The conformance job records toolchain provenance and the image digest for each example.

## Per-compilation certificate (`Concrete/Cert.lean`), R-0552 first increment

The driver emits `Cert_<digest>.lean`, which the kernel checks (`lake env lean`, no `native_decide`):

```lean
def srcBytes : ByteArray := ...      -- the exact input
def expected : Output := ...         -- the exact toks, ast, q, sites, tokens, mir, mem, bir, ll, lcb
-- one entry per edge, each checked by `decide` on that ONE stage function:
theorem e_lex   : Lex.lex srcBytes = .ok expected.toks := by decide
theorem e_parse : Parse.parse expected.toks = .ok expected.ast := by decide
-- … e_elab, e_admit, e_lower, e_memory, e_ssa, e_llvm, e_lcb
theorem cert_compile : compile T₀ srcBytes = .ok expected :=
  compile_of_edges e_lex e_parse e_elab e_admit e_lower e_memory e_ssa e_llvm e_lcb
    g_frame g_lcbAdmit
-- the guards compile also checks, as explicit witnesses:
--   g_frame    : expected.bir.frameSlots ≤ T₀.maxSlots := by decide
--   g_lcbAdmit : LCB.admit expected.lcb = .ok _ := by decide
theorem cert_denotes : Surface0.lexesB srcBytes expected.toks && Surface0.derivesB expected.toks expected.ast &&
    Surface0.elabB expected.ast expected.p = true := by decide
```

`Lexes`, `Derives` and `Elab` each have a Boolean checker over explicit witnesses, with a one-way soundness lemma (`lexesB_sound` and so on, giving `Denotes src p`). So `cert_denotes` discharges the frontend premises **for this compilation**, and `compile_correct_cert` needs only `NativePremises`.

A **refusal certificate** proves `compile T₀ src = .error r` for each refused example.

**What is proved and what is attested:**
- The kernel proves the proposition: the exact source maps to the exact output, including both backend inputs (the `.ll` text and the LCB `Source.Function` value).
- The driver **compares the bytes** of the `.ll` file it handed to clang with the rendering of `expected`, and hands LCB the value `expected.lcb` itself (compared by `DecidableEq`, no text). This is a literal comparison, not a digest comparison. The driver also records the toolchain store paths that were used, and the image digest.
- The association between the source, the text, the toolchain and the image, and therefore `Built`, is **attested** by the driver plus the nix store, and listed in the trust inventory. A digest is identity, not correspondence.

**Budget, and the per-pass design.** The time and size of each certificate are measured and gated. The LL(1) table is a precomputed constant.

The certificate format is a **list of per-edge entries**, composed by the proved `compile_of_edges` (above). In slice 0, each edge is still checked by `decide` running that stage in the kernel, so in total the kernel does run the whole compiler. That is accepted for **slice 0 only**. Later, each edge carries a witness that is small and linear in program size: a name-binding map, an ANF map, an SSA dominance witness, or a register assignment. A proved validator checks the witness, and the universal pass theorem composes with it. From then on, the kernel never re-runs the whole compiler (R-0552/R-0521).

## Stage registry (`Concrete/Stages.lean`)

`compile` is written as an explicit sequence of named stage calls that records every intermediate in `Output`. Each stage is declared with a **fixed contract proposition** and evidence for **exactly that contract**:

```lean
/-- The contracts are fixed per stage in ONE table and stated about the stage function itself. -/
def lowerContract (f : Core.Program × Tokens → Except Refusal MIR.Program) : Prop :=
  ∀ p t m, WellScoped p → TokensValid p t → f (p, t) = .ok m →
    m.WF ∧ MIR.behaviour m = Core.behaviour p
-- … one per stage: lex, parse, elab, admit (check + tokens), lower, memory, ssa,
--   and the two backend emits

inductive Evidence (C : Prop) where
  | proof (h : C)
  | frontend (h : FrontendPremises → C)                      -- P_lex, P_parse
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

## Facts query (`Concrete/Facts.lean`)

The query follows the reviewed facts-query design (Codex, agy and Kimi, 2026-11-06). Slice 0's facts are simple, but they use the full shape.

**One derivation.** `derive : Accepted → FnId → Report` runs over the whole lowered bundle, so BIR slot counts are visible to it. Here `Accepted := { out // ∃ T src, compile T src = .ok out }`: facts are derived only for compiler-accepted outputs, never for arbitrary data. Each fact is a typed judgment `Fact.Holds`. `derive_sound` covers **every** status:
- `established` → `Holds`;
- `conditional(as)` → `Premises`-fields `as` imply `Holds`;
- `refuted(w)` → `w` is a checked counter-witness;
- `notEstablished(blocker, site)` → the site exists and `blocker` names a missing evidence kind;
- `notApplicable` → the fact's subject is absent;
- coverage counts → examined sites = enumerated sites.

Human output, JSON and future policies all render the same `Report`.

**The scope is explicit.** Each fact is a **source/Core fact** or an **artifact fact**. Artifact facts are about the executable, which includes the C runtime, startup and the linker. They are never unconditional in slice 0: at best they are `conditional` on the native premises, and otherwise `notEstablished`.

| Fact (scope) | Slice-0 status |
|---|---|
| Authority: capabilities and grants (source) | `established`: requires no capabilities. This is about authority only, and never printed as "touches nothing". |
| Allocation (source) | `established`: none. |
| Allocation (artifact) | `notEstablished`: crt/startup are not modeled. |
| Memory reads/writes (source) | `established`: own locals only. |
| Implicit trap sites (source) | One per `+`, with a typed `SiteId` and a span. Each is `established` as "cannot trap", by its validated `known(v)` discharge token (`token_valid`, `slice0_never_traps`), so its check is omitted. |
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

The CLI is `concrete facts FILE [FN]` and `concrete build FILE -o OUT` (owner, 2026-11-06).

## Gates (CI jobs, added to `required-checks.txt`)

1. **Compiler build:** `lake build Concrete`, with warnings as errors and `autoImplicit` off.
2. **Axioms and trust:** extended to `Concrete/`:
   - only the three axioms;
   - no `sorry` and no `native_decide`/`ofReduceBool`;
   - a **totality ban**: no `partial`, `unsafe`, `@[implemented_by]`, `@[extern]` or `opaque` in `Concrete/` outside `Premises.lean`;
   - an external-checker replay (con-leche/nanoda, R-0553) of the compiler's theorems.

   Not yet a job. Until it is, the trusted-base golden (gate 9) fixes each headline theorem's axioms (a `sorryAx` among them fails it), and the trust ban of `scripts/proofs/trust_ban.sh` covers the specification only: it reads the compiler's library entry and its pinned dependency in `lakefile.toml`, not its sources or constants.
3. **Premise registry and stage coverage:** the checks described in the stage registry section above.
4. **Conformance, both backends:** each `bir:` row in `tests/manifest.tsv` runs through:
   - the Lean `run`;
   - the LLVM-native executable;
   - the LCB-native executable.

   All three must give the same observation, and each must match the row's `expect`. Future `con:` rows use the same manifest and runner.

   **Classification uses the status and the runtime evidence, never the status alone.** `Observation.status` is many-to-one.
   - Status 134 **with** the runtime's trap line on fd 2 is `trapped k`.
   - Status 134 **without** it is `exited 134`.
   - Status 125 **with** the launcher's refusal line is `startupRefused`.
   - Status 125 **without** it is `exited 125`.

   Accepted examples `return 125;` and `return 134;` are in the corpus and must classify as exits. Slice 0 can't produce a real startup refusal, because it has no grants. The refusal and trap classifications are also exercised by comparator self-tests: stub images, and the trap via gate 6.

   **Refused examples have their own leg.** The Lean `check` result, the compiler's refusal (with its rule) and the refusal certificate must agree with the row's `refuse RULE-ID` expectation. No executable exists for a refused example. A separate supported-subset coverage report lists every grammar production and Core form that the corpus exercises. A reject-all compiler fails, because every accepted example must exit with its status.
5. **Differential at the source** (implemented in `scripts/checks/compiler/spec_diff.sh` and `spec_diff.lean`): a pure splitmix64 generator makes slice-0 `Slice0.Program` candidates, filtered by the spec's `check`; every accepted program is in the fragment by construction and its oracle is `run (toCore q)`. The seed comes from `SPEC_DIFF_SEED` or the commit SHA, and seed plus candidate index determines the program. `SPEC_DIFF_REPLAY=bir:seed:index` and `spec:seed:index` select the leg (bare `seed:index` means BIR); the driver also exposes `gen-spec`, `gen-bir`, `replay` and `replay-spec`. The PRNG and both generators live only in the one test driver (Lean 4.34.1 does not import source-only modules through `LEAN_SRC_PATH` without separately built oleans); nothing in `ConcreteSpec/` or `Concrete/` imports them, and `scripts/checks/import_layering.sh` (a required root check with planted N11 controls) enforces that `ConcreteSpec/` imports only `ConcreteSpec.*` and `Concrete/` imports only `ConcreteSpec.*`, `Concrete.*` and `LeanCompilerBackend.*` while forbidding test/generator modules. Each generator has one weighted production table that every slice brief must extend. The spec table drives statement and return selection; unread bindings and known values are tracked (with unread bindings prioritized on non-defect candidates and folded at body end so accepted yield stays above 50%), coverage is derived from emitted syntax (`let_literal`, `let_copy`, `add_literal`, `add_variable`, `return_literal`, `return_variable`), literals span 0–255, and seeded deliberate out-of-range, known-overflow and unused-binding candidates make all three checker rejection categories (`out_of_range_literal`, `known_overflow`, `unused_binding`) load-bearing on every run.
   `compileCore : ConcreteSpec.Program → Except String Concrete.BIR.Program` and `loweringAvailable : Bool`, in `spec_diff.lean`, form **the** capability contract for Core→BIR lowering. While `loweringAvailable = false`, `compileCore` returns `no Core→BIR lowering yet (slice 1)`; the gate checks generation, acceptance (`>= 50%`), oracle exits, all six source constructs and all three rejection reasons, and loudly reports `spec leg: 0 programs compiled (no Core→BIR lowering; becomes required in the lowering branch)` without claiming a spec/native comparison passed. Once a branch flips `loweringAvailable = true` (or implements `compileCore`), every supported accepted candidate is required to compile and be judged (`compiled = supportedAccepted = judged`, with `SPEC_DIFF_MIN_SPEC_JUDGED` defaulting to `ceil(N / 2)`); partial compilation fails closed, and a planted `control-partial-lowering` negative control verifies this contract now. The driver then emits the same native artifacts, with status, empty stdout and stderr derived from the SPEC outcome. Until a spec trap carries its site, more than one lowered site of its kind is a setup error, never an arbitrary first-site choice.
6. **Differential through the token-free lowering, both backends.** The interim implemented leg generates BIR directly in the test driver, over checked operations and trap branches, wrapped results observed through `trunc`, widening, multiple sites and multi-block chains at 8/16-bit types. Every candidate is filtered by `wf` and `LCB.emit`; rejects are counted and accepted yield must be at least 50%. BIR `run` supplies the expectation, and LLVM-native, LCB-native and LCB's in-process `evalBody` observation must match it exactly through `slice0_native.sh` and the one `observe`/`judge` machinery in `slice0_common.sh`. Exit/trap vacuity and coverage over all five productions (`checked`, `wrapped`, `widening`, `two_sites`, `chain`), all three widening pairs (`widen_u8_u16`, `widen_i8_i16`, `widen_u8_i16`), all three operations (`add`, `sub`, `mul`) and all four integer types (`u8`, `i8`, `u16`, `i16`) are computed over programs actually judged on all three legs (with `SPEC_DIFF_MIN_JUDGED` defaulting to `ceil(N / 2)` and requiring `judged = accepted` when not stopped by the deadline), not the generated corpus; the compiled spec leg has its own judged minimum (`SPEC_DIFF_MIN_SPEC_JUDGED`, defaulting to `ceil(N / 2)`) and coverage. When lowering lands, generated **well-scoped, unchecked** slice-0 programs over **arbitrary** `u8` operands, including `200 + 56`, additionally take `lower q ∅ → memory → ssa`, so they have no tokens and no checker. Their Core `run` is compared with both native paths, including exit 134 and the right site's trap line on overflow.

   **Relationship to ROADMAP A5 (verification vs executable differential).** Per-pass preservation proofs (`B0.8`, `B.5`) are the primary soundness goal for the compiler's internal transformations; once proved, a pass does not depend on random testing for its semantic correctness. Gates 5 and 6 remain permanently alongside the proofs as defense-in-depth over the unverified boundaries that sit outside the Lean theorem statement: the LLVM IR printer (`P_print`), the external native toolchains (`P_toolchain`: `clang` and LCB's assembler/linker driver), and the runtime entry and trap-reporting machinery (`P_rt` and `P_trap_site`).

   The gate builds its full import closure (`Concrete`, `ConcreteSpec.Spec.Slice0` and any module `spec_diff.lean` imports, plus `lean-compiler-backend/lcb`) under `SLICE0_BUILD_SECS`. `SPEC_DIFF_SECS` (default 600) then supplies one monotonic deadline propagated into the watchdog (`BOUNDED_DEADLINE_MONO`) covering generation, controls, builds, runs, bounded retries and `SIGTERM → SIGKILL` termination grace, with per-step caps on each driver/build (`120s`) and observation (`10s`) step; `SPEC_DIFF_N` (default 300) caps candidates per leg. A deadline is reported as `INCOMPLETE` (exit 3) only when no real failure (`FAIL`) occurred; `FAIL` always takes precedence over `INCOMPLETE`. Generated and judged outcome/construct counts are printed separately. A mismatch writes a persistent bundle under `SPEC_DIFF_REGRESSIONS/<leg>/<safe-seed>-<index>/` (default `.lake/concrete/spec_diff/regressions/<leg>/<safe-seed>-<index>/`, where `<safe-seed>` combines a lowercase sanitized prefix with the 64-bit FNV-1a hex hash of the raw seed) with the generated term, expected observation, per-backend `observed-<backend>/` directories, `backends` list, `Concrete.BIR.Examples` or `ConcreteSpec.Slice0.Program` entry, `.con` source file (`spec` leg), `test_manifest.py`-valid `bir:` or `con:` manifest row, and shell-quoted leg-qualified replay command (`SPEC_DIFF_REPLAY=<quoted-leg:seed:index>`); CI uploads it on failure. Negative controls run under active `set -euo pipefail`, require a dedicated planted-failure exit code (`99`) and anchored diagnostic regex, first prove each unmutated candidate, and verify: exact computed non-trap LLVM result (`! judge` against trap) while LCB retains the trap (`drop-overflow-llvm`); exact exit+1 on native LCB and its evaluator (`! judge` against original exit) while LLVM retains the original exit (`lcb-off-by-one`); rejection of a planted spec exit+1 expectation across all three legs (`spec-oracle`); refusal of partial `compileCore` compilation when `loweringAvailable = true` (`partial-lowering`); end-to-end `report_mismatch` bundle emission and `test_manifest.py` validation on both `bir` and `spec` legs (`report-mismatch`); and same-seed byte identity plus different-seed variation (`determinism`). Mutation flags require the explicit `--control SPEC_DIFF_PLANTED_CONTROL` guard.
   Failure and completion are independent: `native_mismatch` remains set after any observed mismatch, and `native_complete` becomes true only after all three comparisons. A later observation timeout therefore stops coverage accounting but still exits FAIL in both generation and replay. The overflow control also runs its proven LLVM mismatch followed by a real LCB timeout, checking the exact mismatch and timeout diagnostics and both result fields. The partial-lowering control uses a fixed corpus: two accepted literal returns must emit native-ready artifacts before candidate 2 refuses with the planted capability diagnostic; the synthetic partial summary separately checks the shell contract.

   Replay splits the numeric index from the final colon, preserving colons within the seed. Saved commands use Bash `%q` quoting; controls execute those saved commands through the production parser and check seed, leg and index, including a seed containing both a colon and a single quote. The shared watchdog's opt-in deadline never increases an attempt's requested execution budget; cleanup is independently bounded by the absolute deadline. `scripts/checks/lib/test_bounded_exec.py` runs cheap process controls for short execution budgets with longer grace, stubborn-child deadline cleanup, and unchanged behavior without deadline opt-in; the differential runs these controls before generation.

6a. **AArch64:** a GitHub-hosted AArch64 macOS job (`macos-15`) builds the LCB output for `aarch64-macos`, runs gates 4 and 6 there natively, and compares against Lean. LLVM-native also runs there, as a third cross-check. Gates 4 and 6 for x86-64 run on concrete-ci-2.
7. **Certificates:** the kernel checks every acceptance and refusal certificate, with measured budgets.
8. **Reproducibility** (implemented, `scripts/checks/compiler/slice0_repro.sh`): two independent builds give byte-identical outputs at every stage, on each target, and two independent Linux runners give identical manifests. See [Reproducible builds](#reproducible-builds-owner-2026-11-07).
9. **Trusted base** (owner, 2026-11-07): a golden test that `concrete facts` prints exactly the spec's premise list, so a premise added, removed or reworded anywhere fails until the golden is updated with it. Implemented provisionally in `scripts/checks/compiler/slice0_trusted_base.sh`: until `concrete facts` exists, `scripts/checks/compiler/slice0_trusted_base.lean` prints `Premises.report`: the premise registry `Concrete/Premises.lean`, the sorted axioms of each headline theorem (`Premises.theorems`) and each axiom's type; the gate appends the Lean toolchain and the Lake pins, and the result must equal `tests/golden/slice0_trusted_base.txt` byte for byte. The README's open-premise table must equal the registry's rendering (`Premises.tableRow`). Controls cover the comparison, a failing printer, the axiom collection and a drifted table. The statements are text until `FrontendPremises` and `NativePremises` land as Lean structures; each entry then names its field.
10. **Sanitizers** (owner, 2026-11-07; UBSan dropped by the owner, 2026-11-08; implemented in `scripts/checks/compiler/slice0_artifacts.sh`): every LLVM example also runs built with ASan in CI, and must give the same observation with no sanitizer report. ASan instruments only functions marked `sanitize_address`, which clang's C front end adds and the printed IR lacks, so the sanitizer build adds it to every definition (`slice0_native.sh sanitize`); a control body with an out-of-bounds load must be reported. There is no UBSan leg: UBSan's checks are inserted by clang's C front end, so on LLVM IR input it checks nothing. No C is compiled in slice 0, and the LLVM-level undefined behaviour of the printed IR (poison, `unreachable`) stays P_print's.
11. **Binary size** (owner, 2026-11-07; implemented, same script): the size of each example's executable (bytes and text size, pinned `llvm-size`), per backend and target, is recorded per run in `sizes.tsv`, uploaded with the job. Report-only: it never fails a run. **Compile time** (owner, 2026-11-08; same script, report-only): each example's emit step (the Lean emitter, Lean start-up included) and native step (clang and LCB's command line) in wall-clock milliseconds, next to its budget (the step's time bound), in `compile_times.tsv`.
12. **Stack bound** (owner, 2026-11-07; implemented in the same script and `lib/stack_bound.py`; the `aarch64-macos` run check with LLDB, 2026-11-08): a stack-bound fact for each example, derived from the final artifact (the linked executable, not the IR) for both backends, and checked against a run. The analysis follows every path of the Concrete-owned code, instruction by instruction from an explicit set of modelled instructions, (`main`, `concrete_rt_write_all`, `concrete_body`) over the disassembly's control-flow graph, tracking the stack and frame pointers' depth below `main`'s entry and every byte addressed relative to them (so the red zone), refusing an access deeper than the target's red zone below the stack pointer (128 bytes on x86-64 System V and on Apple arm64, none under AAPCS64 on Linux: deeper bytes may be overwritten by a signal handler), and fails closed on anything it does not model (an instruction outside the set, another stack-pointer write, frame- or link-register writeback, an indirect branch or call, a stack pointer copied out, an indexed stack address, a stack read of bytes the function has not itself written on every path since its last call (so no stale saved frame pointer is ever loaded), a saved slot read other than by the modelled restore, two depths meeting, recursion, a call to a function neither owned nor a PLT/stub). The external calls (`write`, `_exit`, `abort`) are listed with the depth at their entry; their own use, and that `_exit` and `abort` do not return, are the libc premise. On x86-64 Linux the run check single-steps the owned code under the pinned gdb and requires the measured depth to be within the bound, for a complete run only (stopped first at `main`, back at each external call's return address, no signal, the expected exit status). The measured depth is the lowest of the stack pointer and every stack byte a stepped instruction addresses (`lib/stack_access.py` evaluates each memory operand), so a red-zone access is counted, and one deeper than the red zone is an error. A control links a `main` that stores at the red zone's edge without moving the stack pointer and requires bound 128 and measured 128 (on AArch64 it first allocates and frees a 144-byte frame with a pre-indexed store, judged against the new stack pointer, so 144); with a 120-byte red zone the same run gives no measurement. On Mach-O each stub entry's imported symbol comes from the indirect symbol table (`llvm-objdump --macho --indirect-symbols`), since the disassembly names only the `__stubs` section. On `aarch64-macos` the run check is the same, under Apple's LLDB (`/usr/bin/lldb`, `lib/stack_measure_lldb.py`; an external call's return address is the link register); LLDB is not pinned (Apple's signed debugserver is the only one allowed to attach), so its version is printed in the gate log. `stack.tsv` records bound, measurement and externals.

With the front end (owner, 2026-11-07), gate 5's `compileCore` hook makes native comparison against the Lean evaluator non-vacuous and required. A JSON schema generated from the Lean types of the `facts` output has goldens. Each remaining gate joins `required-checks.txt` when its compiler stage lands.

**Negative controls**, each a mutation the matching gate must refuse:
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

1. The CLI is `concrete build FILE -o OUT` and `concrete facts FILE [FN]`.
2. A decided form outside slice 0 (`0x2A`, `let mut`, `if`) gets a distinct "not supported in slice 0" refusal, separate from a syntax error.
3. Leading-zero literals such as `007` are refused: one form per number. This is recorded in `Surface0.lean`.
4. The trap site that the executable reports is a named premise now, `P_trap_site`, which a proof retires later.
5. Both backends are in slice 0. This is being recorded on `wip/decisions-followup`.
6. Information flow and constant time are decided: `Secret<T>`, `classify`/`declassify(x, "reason")`, `ct_select`/`ct_eq`, and the baseline observer (branches, addresses and variable-time operations; speculation not covered). Slice 0 carries only the fact fields.
7. OS sandboxing of the launcher is out of scope for the compiler.

8. The trap line is `concrete: trap: <kind> at <path>:<line>:<col> (site <id>)`: the path relative to the project root with forward slashes, line and column 1-based (owner, 2026-11-06). The compiler implements it in `Backend/Runtime.lean` (`trapLine`), from the site table's `Loc`; `P_trap_site` covers the executable writing it.
9. Rule IDs are the Lean rule name plus a per-rule revision; each output header carries one `spec_version` (owner, 2026-11-06). The facts query and refusal output use them.

## Follow-ups (recorded after the final review round, round 8)

These are internal. None blocks slice 0's correctness.

- **Retire premises:**
  - `lex_correct` (P_lex);
  - the generic table-parser theorem (P_parse);
  - `adapter_correct` and `emit_correct` (P_adapter, retired 2026-11-08);
  - a proof of the decoding entry against a libc model (P_rt).
- **P_toolchain[lcb]** is retired by LCB's own C01–C03 work. LCB's verified profile names Apple AArch64 as its intended target. The `x86_64-linux` executable goes through LCB's compatibility pipeline, inside P_toolchain[lcb], until LCB certifies that target.
- **Encoded result.** It works while LCB's verified profile has no calls, memory or traps. When it gains them, revisit the encoding: an external trap call, or a multi-function encoding.
- **SSA:** replace the direct `ssa_correct` with a proved validator. BIR's shape must not depend on the direct proof.
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
- **Rebase:** rebase onto main, under a new branch name, when `wip/spec-root` lands.
- **Slice order after slice 0** (owner, 2026-11-06, updated with the approved decision set). Each slice is a thin vertical slice, proven end to end:
  1. Q8.2's linear empty struct, explicit `abort()`, and the refused `200 + 56` control;
  2. integers complete: all widths, wrapping/saturating, comparisons, bitwise, shifts;
  3. capabilities: `main` grants, `with(Console)`, writing an integer, with output compared across both backends and the Lean interpreter;
  4. `bool`/`if`/`while` and `#[variant]`;
  5. functions, calls, recursion, capabilities through calls, contracts;
  6. the trap checker;
  7. `Secret<T>` on scalars;
  8. enums, `match`, `Result` and `checked_*`;
  9. structs and ownership;
  10. borrows;
  11. allocation, arrays and memory bounds.
- **Extension without reshaping.** BIR's integer types are the spec's `IntTy`: all eight widths and both signednesses, with values as in-range mathematical integers. `BinOp` grows the bitwise operations, shifts and comparisons. Saturating and checked operations lower to `binOvf` plus branches or selects.
  - **The LCB adapter** covers types up to 16 bits today, where exact results fit a word. Slice 2 (integers complete) needs from LCB the items in "What LCB must provide" below.
  - **Slice 3** turns console output into an observation beyond the exit code. `Behaviour` gains an event trace, and BIR gains an effect instruction naming its capability. The runtime entry gains the grant table and the startup refusal (status 125).
  - **Explicit `abort()`** (slice 1) ends the run with whatever observation the spec gives it. If the spec makes it a trap kind, it is BIR's `trap` terminator at a site of that kind, and the encoding, runtime and trap line are unchanged; if it is a distinct observation, the result word gains one more range above the trap sites.
  - **The result word** keeps encoding only how the run ends.
  - **LCB's verified profile has no calls**, so slice 3 needs LCB calls in that profile, or an event protocol through the result. That needs agreeing with LCB.
- **BIR well-formedness** makes dominance hold by construction: values cross blocks only as block arguments, every operand is defined earlier in its own block, and every non-entry block has a predecessor. That a well-formed program never reads an undefined register is proved (`run_progress`, `Concrete/BIR/Progress.lean`).

## What LCB must provide

LCB is our own project; these are tasks for it, not premises to keep. Checked against the pinned rev `4bea4b3` (LCB main after PR #1: wire packets at header v2 with semantics `ssa-int-v2` / `a64-leaf-v2`; packets with the old header or old semantics identifiers are refused; `CodeInput.version` stays 1).

**For the trusted base (now):**
1. A public entry `compileSource : Target → Symbol → Source.Function → Except Error Artifact`. Today the compiler builds LCB's `FunctionJob` itself (`Concrete/Backend/LCB.lean`, `assemble`).
2. The verified first path for `x86_64-linux`. It exists only for `aarch64-apple-darwin` leaf functions; x86-64 goes through the compatibility backend, a larger unverified base.
3. Larger resource limits on the first path: 64 blocks, 488 values and 480 definitions per function (`Verified/Canonical.lean`). Slice programs will exceed them.
4. Object emission (LCB V04), so assembly text and the system assembler leave the path.

**For slice 2, integers complete (all widths u8–u64, i8–i64): provided on LCB's verified path (Apple AArch64).** LCB's verified profile now has `word` (32-bit) and `long` (64-bit) values, `add sub mul and or xor`, the shifts `shiftLeft logicalRight arithmeticRight` (count modulo the width), `mulHighUnsigned mulHighSigned` (accepted only at `long`), division and remainder (`unsignedDiv unsignedRem signedDiv signedRem`; a zero divisor or signed minimum by −1 is stuck, so the adapter must branch to a trap before them), and the comparisons `eq unsignedLT unsignedLE signedLT signedLE` (word result). The adapter will carry every integer as a `long`: exact for types up to 32 bits; 64-bit overflow uses the high multiply and, for add/sub, `xor`/`and`/compare. Still absent and optional while every integer is a `long`: width conversion (`word`↔`long` extension and truncation).

**Still needed for the Linux targets:** the compatibility path is reached with `Verified.reify`, which maps both high multiplies to an ordinary `mul` (LCB `Verified/Canonical.lean`), because the compatibility IR has no high multiply. Slice 2's 64-bit multiply overflow would compute the wrong flag there. LCB must add high multiply to its compatibility IR and backends (or the x86-64 verified path) before slice 2; until then the adapter must refuse high multiply on Linux targets. Slice 0 emits none.

## Not in slice 0

- Mono Core.
- Grants, the launcher's startup refusal (125) and events.
- The LP0 proven path: LCB's own theorems composed with `adapter_correct`, replacing `P_toolchain[lcb]`. It comes right after the functions-and-calls slice (slice 4 in the earlier numbering, slice 5 now). In slice 0 LCB already runs, unverified, with the adapter proof first.
- An LLVM-IR semantics. The LLVM meaning `M` stays a premise parameter until one exists, and then `P_print` becomes a theorem.
- The information-flow and constant-time analysis. The surface and the observer are decided; slice 0 has only the fact fields.

## Reproducible builds (owner, 2026-11-07)

The same source, compiler revision, toolchain pins and target must produce byte-identical BIR,
LLVM text, LCB input and output, objects and executables. `nix develop --command bash
scripts/checks/compiler/build_identity.sh` prints the canonical identity: the compiler commit, a digest of
the working-tree content of every tracked input with `source_tree=clean|dirty` (so a local edit never
reports a committed identity), Lean, clang/LLVM, the Nix inputs, the LCB revision and the target. It
fails unless every dependency checkout is at the revision `lake-manifest.json` pins with no
modified or untracked (not ignored) file. The
planned `concrete facts` command will print this identity.

`scripts/checks/compiler/slice0_repro.sh [MANIFEST]` makes two independent builds: two source copies at
different paths with different TMPDIRs, each compiling the compiler, its dependencies and the `lcb`
executable itself, with no Lake artifact cache. They share only the pinned Lean toolchain and the
dependency sources (tracked files only). Each side
emits every `bir:` row in `tests/manifest.tsv` and builds it natively, and every stage is compared
without normalization: the BIR and adapted LCB value dumps, both text formats, both LCB assembly
outputs, all objects and both executables. Negative controls check that the comparator refuses a
flipped byte, a missing artifact and a differing inventory. The slice-0 front end is not implemented
yet, so this gate starts at the hand-written BIR contract.

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

Every slice-0 gate runs under `scripts/checks/lib/limits.sh` (per-process address space 64 GiB on
Linux, since Lake reserves far more than it uses; at most 40000 processes for the user), with
every in-gate time bound (builds, emitter calls, analyses, executables) through `bounded_run`
(time, its whole process group killed, one re-run with a larger budget; a run that times out twice
is INCOMPLETE, listed when the gate exits, and makes the gate fail rather than pass; the CI job's
outer `timeout` is only a runaway guard). Every executable under test also runs under `lib/limited_exec.sh` (1 GiB address space, no core
file). Darwin does not enforce an address-space limit, so there the CI job's memory is the bound.
