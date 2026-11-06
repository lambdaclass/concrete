# The Concrete compiler: slice 0 design (provisional)

PROVISIONAL. This is a design sketch, revision 8, written after eight Codex and agy review rounds. It folds in the Codex/agy/Kimi pipeline review and the owner's answers (2026-11-06). Build step 1 (BIR as the shared backend contract) is implemented: `Concrete/BIR.lean` and `Concrete/Backend/`, gate `scripts/checks/slice0_backends.sh`. Slice 0 is the first compiler slice. It is built before the owner freezes Core (A2c) and makes no normative claim before that freeze ([decision](../docs/decisions.md#decided-project-proof-infrastructure)). Branch `wip/slice0` depends on `wip/spec-root`, which puts `ConcreteSpec` on the new root, and is rebased onto main (under a new branch name) once that lands.

## Scope

This document describes **the compiler**: its stages, IRs, proof strategy, premises and gates. Language rules are not restated here. They live in `ConcreteSpec`, and this document links to them:
- the slice-0 fragment and its examples: [`ConcreteSpec/Spec/Slice0.lean`](../ConcreteSpec/Spec/Slice0.lean);
- its behaviour theorems: [`ConcreteSpec/Proofs/Slice0.lean`](../ConcreteSpec/Proofs/Slice0.lean);
- what the parent process observes, including the trap status and the trap line: [`ConcreteSpec/Spec/Observation.lean`](../ConcreteSpec/Spec/Observation.lean);
- the slice-0 surface (token rules, grammar, elaboration): the new `ConcreteSpec/Spec/Surface0.lean` described below.

The conformance corpus (`tests/slice0/`) instantiates those spec examples as source files.

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
  │ resolve + elab      Elab.lean     resolve + elab: one stage edge: proved to satisfy
  ▼                                   Surface0.Elab, with freshness and naming-independence
Slice0.Program + site map  (data, `out.q`, `out.sites`)
  │ check = ADMIT       the spec's own `ConcreteSpec.check`; `check_iff` gives `Checked (toCore q)`;
  ▼                     the ONE admission point; emits discharged-evidence tokens (`out.tokens`)
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
  ▼ (unverified: premise P_print)              ▼ (first backend proof target: adapter_correct
.ll text  (`out.ll`)                           │  against LCB's imported semantics; P_adapter until proved)
  │ clang/llc, link                          LCB-IR  (`out.lcb`)
  │ (premise P_toolchain[llvm])                │ LCB lowering + assembler + link
  ▼                                            ▼ (premise P_toolchain[lcb]: LCB's named trust boundaries)
executable (x86-64)                          executable (x86-64 and AArch64)
  │ run: premises P_launch, P_live (environment), under the target profile
  ▼
observed behaviour  →  observation (exit status 0–255 / 125 startup refusal / 134 trap + fd-2 line)
```

**`compile` is defined as the composition of the stage functions listed in `Stages.lean`**, so a pass that isn't registered can't be in the pipeline. `Output` is pure data with `DecidableEq`: `{ toks, ast, q, sites, tokens, mir, mem, bir, ll, lcb }`. There are no proofs inside it, and every theorem is stated *about* `compile`.

There's no Mono Core, since slice 0 has no generics. When Mono exists, the admission check runs again on its output: substitution can expose new constants and cycles (row Q6.3).

**CheckedCore is the single admission point.** Trap admission, termination and fact derivation run there, and nowhere else. The checker also emits one **discharged-evidence token** per operation it proved safe; in slice 0 that is `known (v)` for a `+` whose value is statically known. Tokens are carried through MIR, Memory CFG and BIR. A token is validated (`token_valid`). Its soundness connects the checker's known value to the run-time operands through `knownExpr_sound`, because `knownExpr_u8` alone establishes only static knowledge. Only a validated token lets a pass omit a check. No pass removes a check without evidence (PASS-9).

**Place MIR** is where later slices put evaluation order, cleanup edges and the expansion of residual checks. In slice 0, an add with no valid token becomes an explicit `add.ovf` plus a branch to a trap site, and an add with a valid token becomes a plain add. Every accepted slice-0 program has tokens on all its adds, so its generated IR (MIR through BIR and both backend inputs) contains no trap path. Whether the executable contains trap code is an artifact fact, not established yet. Unchecked well-scoped programs, such as `200 + 56` in gate 6, have no tokens, so the check-and-trap path is still proved and tested.

**Memory CFG** is the one layout authority. It consumes the spec's byte encoding (`ConcreteSpec/Spec/Rep.lean`) and turns places into loads and stores over block-offset memory with provenance. In slice 0 every local is a `u8` slot (size 1, alignment 1), so the pass is identity-shaped. It exists from day one so that layout never leaks into SSA construction or the printers.

**Typed source identity, end to end.** The lexer and parser mint typed `SiteId`s and spans. Elaboration records an origin edge for every generated temporary in `out.sites`. Every MIR, Memory CFG and BIR instruction carries its origin, and so does every trap site. Diagnostics, facts, JSON and certificates all read this one evidence graph. Core itself has no positions yet (spec open items O1/O4), so the site map sits beside `out.q` without changing the spec's Core.

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

**The authoritative target.** `Slice0.toCore` currently targets the slice-1 Core fragment. Slice 0's authoritative claim targets the **integrated Core** once A2i/R-0560 lands. `toCore` is the only coupling point, so retargeting changes one function and its theorems.

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

The **BIR → LCB-IR adapter** is the first backend proof target. `adapter_correct` states that the adapter's output, under **LCB's own semantics** (`LeanCompilerBackend.Verified.Source`, the `ssa-int-v1` profile, imported and never restated), has the BIR behaviour. Until it is proved, it is premise `P_adapter`, and closing it is a slice-0 milestone.

LCB's own lowering, assembler and linker are LCB's named trust boundaries, carried as `P_toolchain[lcb]` until LCB's preservation theorem covers the slice-0 domain.

LCB is a pinned lake dependency (`require` with an exact rev, LCB's upstream lock recorded). It uses the same Lean toolchain (v4.34.1) and no Mathlib.

**The encoded result, with one decoding runtime.** Neither backend's input contains a call or a trap.

Each backend emits `main`'s body as a function `concrete_body() -> w`, a 32-bit word, whose return value **encodes** the behaviour:
- `exited c` returns `c` (0–255);
- a trap at site `s` returns `256 + s.idx`. The runtime's site table maps the index to the trap's kind, typed `SiteId` and span.

This fits LCB's verified source profile (`ssa-int-v1`, `LeanCompilerBackend/Verified/Source.lean`), which has integer operations, compares, branches, phis, a return or divergence, and no calls, traps or memory. It is also what lets the adapter be proved against LCB's semantics as it stands.

It also separates `return 134` from a trap: they return the words 134 and 256.

A fixed runtime entry, `Backend/Runtime.lean`, is emitted per backend as LLVM text and as LCB-IR text. It calls `concrete_body`, and then:
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
| `tests/` | The executable test corpus. Each slice gets one subfolder, which holds `.con` sources and their `.expect` outcomes. |

**Changes to existing entries.** No new entry is created for any of these:
- `lakefile.toml` gains the `Concrete` lib, the `concrete` exe, and a pinned `require` of `lean-compiler-backend`; `lake-manifest.json` records it.
- `ConcreteSpec/Spec/Surface0.lean` and `ConcreteSpec/Proofs/Surface0.lean` go into the spec's existing folders.
- The gates and runners (conformance, the two differentials, certificates, the premise registry, negative controls) are files in the existing `scripts/checks/`. No `scripts/compiler/` is added.
- `required-checks.txt` and the CI workflow gain entries.

Generated outputs are never committed and get no new root entry. That covers certificates, `.ll` files, images and facts JSON, which all go under the git-ignored `.lake/concrete/`.

**The folder tree:**

```
Concrete.lean          library root (imports only)
Concrete/
  README.md            this design
  Lex.lean             UTF-8 decode + lexer, and its theorem against Surface0.Lexes
  Parse.lean           generic LL(1) table parser over Surface0.grammar
  Elab.lean            resolve + elaborate to Slice0.Program (one stage edge), and its theorems
  MIR.lean             Place MIR: syntax, semantics, WF, lowering (residual checks only), lower_correct
  Memory.lean          Memory CFG: the Layout authority, block-offset memory, memory_correct
  BIR.lean             BIR: syntax, semantics, behaviour, WF, ssa, ssa_correct, addOvf_correct
  Backend/             the backend stage: everything after BIR, one file per backend
    LLVM.lean          BIR → LLVM text printer (unverified; P_print)
    LCB.lean           BIR → LCB-IR adapter + adapter_correct against LCB's semantics
    Runtime.lean       the fixed entry that decodes the result word into exit status / trap line, per backend
  Stages.lean          typed stage contracts; `compile` is the fold of the stage list
  Premises.lean        THE one file of premises, plus the execution-trust inventory
  Theorem.lean         compile_correct, the non-vacuity witnesses, answer_correct
  Facts.lean           per-function facts: judgments, derive, derive_sound, rendering (text/JSON)
  Cert.lean            per-compilation certificate emitter
  Main.lean            CLI driver (IO, toolchain invocation); placeholder spellings
tests/
  slice0/              *.con sources and *.expect outcomes (exit status, or refusal + rule)
```

**Rules:**
- `Concrete/` has one subfolder, `Backend/`, because the backend stage has three files. No other subfolder exists until another stage outgrows one file, and a split never creates a one-file folder.
- Each stage's definitions and theorems live in the same file, so each fact has one home.
- `Resolve` merges into `Elab`, because it is one stage edge with one contract.
- Every entry has exactly one home: tests are in `tests/`, gates in `scripts/checks/`, the spec in `ConcreteSpec/`. Conformance has one home, `tests/`.
- The stage table, the premise table and the fact-kind table in this README become **generated views** of the Lean registries in `Stages.lean`, `Premises.lean` and `Facts.lean` once the code exists. A gate fails when a view drifts from its registry.

The root inventory (`scripts/checks/root_inventory.txt`) lists each new root entry in the same change that creates it. An unlisted entry fails the check, and so does a listed entry that is missing.

## Frontend specifications (new spec file `ConcreteSpec/Spec/Surface0.lean`)

This file is spec work (open item O1, slice 0 only). It is a new file, and no existing spec file changes. The language content lives in the Lean file, not here: the token rules (SYN-5/SYN-6, plus the owner's decision that leading-zero literals are refused), the grammar and the elaboration relation. What the compiler needs from it:

- **`Lexes : Bytes → List Token → Prop`**, an independent byte-level judgment. The lexer is proved sound and complete against it, which rules out splitting a token.
- **The grammar as data**, which must be LL(1), plus **`ll1 : Grammar → Bool`**, which computes nullable, FIRST and FOLLOW sets. It comes with `ll1_sound : ll1 g = true → ∀ n t, at most one production of n is predicted on t`. The fact `ll1 grammar0 = true` is proved by `decide`. The generic table parser is proved against `Derives`.
- **`Derives : List Token → Surface.Program → Prop`**, which binds token payloads, not only token kinds.
- **`Elab : Surface.Program → Slice0.Program → Prop`**, with the compiler's theorems `elab_fresh` (temporaries never collide with user names) and `elab_rename_invariant` (renaming temporaries changes neither acceptance nor outcome).
- **Refusal classes**, as the spec defines them:
  - a syntax error;
  - "not supported in slice 0", for a decided form outside the fragment (owner, 2026-11-06);
  - a checker refusal.

  The compiler reports exactly the class that the spec judgment gives.
- **`Denotes src q := ∃ toks ast, Lexes src toks ∧ Derives toks ast ∧ Elab ast q`.** Its theorem is `denotes_behaviour_det` (see the theorem section).

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

/-- A backend: the total Lean function from BIR to the body text a native toolchain consumes, and
    what that text means as a `BodyResult`. For LCB, `Means` is DEFINED from LCB's imported
    semantics, so the emit obligation is a theorem target. For LLVM it is a parameter (a premise). -/
structure Backend where
  Text : Type
  emit : BIR.Program → Text
  Means : Text → BodyResult → Prop

def llvm (M : String → BodyResult → Prop) : Backend := ⟨String, LLVM.print, M⟩
/-- The emitted LCB text is the adapted body PAIRED with its site table, so the table that
    decodes the result is part of the emitted artifact. (The LLVM text already contains its
    table, and printer injectivity covers it.) -/
def lcb : Backend := ⟨LCB.Source.Function × SiteTable, fun b => (LCB.adapt b, b.sites),
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
  /-- emit (implementation): the body text means at most the BIR behaviour, encoded.
      LLVM: premise P_print. LCB: premise P_adapter until `adapter_correct` is proved, after
      which this field is filled by the theorem and the premise is deleted. -/
  emit : ∀ b, b.WF → ∀ r, B.Means (B.emit b) r → r.behaviour = BIR.behaviour b
  /-- P_toolchain[B] (toolchain): every result the linked body produces is one its text means.
      LCB: LCB's own importer, lowering, assembler and linker, i.e. its named trust boundaries
      (`docs/SEMANTICS.md`, "Target and environment assumptions"). -/
  toolchain : ∀ b img r, b.WF → N.Built T (B.emit b) img → N.BodyRuns T img r → B.Means (B.emit b) r
  /-- P_rt (implementation): the runtime entry decodes. A launch whose body returns `w` has the
      behaviour `decode w`: it exits with w below 256; otherwise it writes the whole trap line to
      fd 2 (looping on partial writes, ignoring write errors) and then exits 134 unconditionally.
      A body that diverges gives a launch that diverges. The trap line's presence also relies on
      the admitted-launch fd-2 condition, which belongs to P_launch, not here. -/
  rt : ∀ b img β, b.WF → N.Built T (B.emit b) img → N.Runs T img β → β ≠ .killed →
    ∃ r, N.BodyRuns T img r ∧ β = r.behaviour
  /-- P_launch (environment, resources): an admitted launch of an image built from a WF BIR
      within the profile's frame bound is never killed (stack, signals), and fd 2 accepts the
      trap line. -/
  launch : ∀ b img β, b.WF → b.frameSlots ≤ T.maxSlots →
    N.Built T (B.emit b) img → N.Runs T img β → β ≠ .killed
  /-- P_live (environment): an admitted launch happens and has a behaviour (maximal: a run that
      never ends has behaviour `diverges`). -/
  live : ∀ b img, b.WF → N.Built T (B.emit b) img → ∃ β, N.Runs T img β
```

Every premise is used in the composition:
- `launch` excludes `killed`;
- `rt` reduces the launch to the body's result;
- `toolchain` takes that result to the text's meaning;
- `emit` takes the meaning to the BIR behaviour;
- `live` makes all of this non-vacuous for the image.

There are two target profiles: `x86_64-linux` and `aarch64-linux`. The LLVM backend runs on `x86_64-linux` (on concrete-ci-2) and as a cross-check on `aarch64-linux`. The LCB backend runs on both.

**The open premises for slice 0:**

| Premise | Trust class | Retired by |
|---|---|---|
| P_lex | implementation | `lex_correct` |
| P_parse | implementation | the generic table-parser theorem |
| P_print | implementation | an LLVM-IR semantics; not planned for slice 0 |
| P_adapter | implementation | `adapter_correct`, the first backend proof target |
| P_toolchain[llvm] | toolchain | not planned |
| P_toolchain[lcb] | toolchain | LCB's own proofs (C01–C03 and beyond) |
| P_rt | implementation | a proof of the decoding entry against a libc model; not planned for slice 0 |
| P_trap_site | implementation | the same runtime-entry proof (owner, 2026-11-06: a premise now) |
| P_launch | environment | not planned |
| P_live | environment | not planned |

The fd-2 condition is part of the admitted launch, so it is inside P_launch (environment).

**The trap site the executable reports is covered by premise `P_trap_site`** (owner, 2026-11-06). `trap_site_correct` proves the site at IR level. The executable's report is assumed: `Native` gains `ReportsSite : TargetProfile → Image → SiteId → Prop`, and a field states that a launch whose body returns `256 + i` reports `tbl[i].site` on fd 2. Gates 4 and 6 test it. A proof of the runtime entry retires it, together with `P_rt`.

P_lex and P_parse are also discharged for each compilation by the certificate.

The **execution-trust inventory** is in the same file, as documented non-Prop entries:
- the Lean compiler and runtime executing `compile`. The certificate closes this for each output.
- the driver's IO: the byte-for-byte comparison of the emitted `.ll` or LCB-IR file, and the toolchain invocation and its provenance record. These are attested, not proved (see the certificate section).
- LCB's own native execution: it lowers LCB-IR to assembly as a Lean-compiled program. This is inside `P_toolchain[lcb]`.

## The end-to-end theorem (`Concrete/Theorem.lean`)

```lean
/-- What `compile` returns: pure data, every intermediate recorded. -/
structure Output where
  toks : List Token; ast : Surface.Program
  q : Slice0.Program; sites : SiteMap; tokens : Tokens; mir : MIR.Program; mem : Mem.Program
  bir : BIR.Program; ll : String; lcb : LCB.Source.Function

/-- One theorem for both backends: `B` is `llvm _` or `lcb`. -/
theorem compile_correct (F : FrontendPremises) {B N T} (P : NativePremises B N T)
    {src : ByteArray} {out : Output} {img : N.Image}
    (hc : compile T src = .ok out) (hb : N.Built T (B.emit out.bir) img) :
    Surface0.Denotes src out.q ∧ Checked (Slice0.toCore out.q) ∧
    ∃ c, c < 256 ∧ run (Slice0.toCore out.q) (out.q.body.length + 1) = .exit c ∧
      (∃ β, N.Runs T img β) ∧ ∀ β, N.Runs T img β → β = .exited c
```

**Composition, each step a stated theorem:**
1. `compile_spec`: `compile T src = .ok out` implies:
   - `Lex.lex`, `Parse.parse` and `Elab.elab` succeed and produce `out.q`;
   - `check (toCore out.q) = true`;
   - `out.tokens = admit out.q`, `out.mir = lower out.q out.tokens`, `out.mem = memory out.mir`, `out.bir = ssa out.mem`, `out.ll = LLVM.print out.bir` and `out.lcb = LCB.adapt out.bir`;
   - `out.bir.frameSlots ≤ T.maxSlots`.
2. `F.lex`, `F.parse` and `elab_sound` give `Denotes src out.q`. `check_iff` gives `Checked`.
3. `slice0_exits` gives `run … = .exit c` with `c < 256`; `run_exit_behaviour` then gives `Core.behaviour = .exited c`.
4. `checked_wellScoped : Checked (toCore q) → WellScoped q`. `WellScoped` includes in-range literals, because Core's `run` does not range-check literals.
5. `admit_tokens_valid : Checked (toCore q) → TokensValid q (admit q)`. The empty token set is valid for any well-scoped program.
6. `lower_wf`, and `lower_correct : WellScoped q → TokensValid q t → MIR.behaviour (lower q t) = Core.behaviour (toCore q)`. This holds for every well-scoped program with valid tokens, checked or not, so `200 + 56` with no tokens preserves `trapped .arithmeticOverflow`. The trap path is real.
7. `memory_wf` and `memory_correct : m.WF → Mem.behaviour (memory m) = MIR.behaviour m`. Then `ssa_wf` and `ssa_correct : g.WF → BIR.behaviour (ssa g) = Mem.behaviour g`.
8. `P.launch` excludes `killed`. `P.rt` gives a body result `r` with `β = r.behaviour`. `P.toolchain` gives `B.Means (B.emit out.bir) r`, and `P.emit` gives `r.behaviour = BIR.behaviour out.bir = .exited c`. `P.live` gives existence.

**Supporting theorems:**
- `addOvf_correct`: for arbitrary `u8` operands, the result is `(a+b) % 256` and the flag is `a+b ≥ 256`.
- `adapter_correct : b.WF → LCB.bodyResult (LCB.adapt b) r → r.behaviour = BIR.behaviour b`. It is proved over all WF BIR, overflow included, against LCB's **raw** `Source.Function` semantics (`LeanCompilerBackend/Verified/Source.lean`, imported, never restated). It fills `NativePremises lcb`'s `emit` field.
- `adapt_admitted : compile T src = .ok out → LCB.admit (LCB.adapt out.bir) = .ok _`. It is separate from semantic preservation and uses LCB's own admission function, so it covers every profile check: SSA and dominance validation, reachability of all blocks, exact widths, supported operations only, at most 64 blocks, 480 definitions and the frame bound. It is proved, or, until then, checked for each compilation by `decide` in the certificate.
- `lower_emits_check`: every MIR add without a valid discharged-evidence token is followed by a branch on its flag to a trap site of kind `arithmeticOverflow`, with that add's origin.
- `denotes_behaviour_det`: two denotations of the same `src` have the same `Core.behaviour`. This is stated over behaviour, not at a fixed fuel.

**Program correctness transports**, through the acceptance witness `answer_accepts : compile T₀ answerSrc = .ok answerOut`:

```lean
theorem answer_correct (F : FrontendPremises) (P : NativePremises B N T₀)
    (hb : N.Built T₀ (B.emit answerOut.bir) img) : (∃ β, N.Runs T₀ img β) ∧ ∀ β, N.Runs T₀ img β → β = .exited 42
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

   Because `emit b = (adapt b, b.sites)`, a built image carries exactly `b.sites`, so `rt` holds by definition and no adapter injectivity is needed. `emit` is `adapter_correct`.

   `live` holds because LCB's step relation is deterministic, so a run returns, faults or diverges. The witness follows from `adapter_correct` and lands with it.
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
    Surface0.elabB expected.ast expected.q = true := by decide
```

`Lexes`, `Derives` and `Elab` each have a Boolean checker over explicit witnesses, with a one-way soundness lemma (`lexesB_sound` and so on, giving `Denotes src q`). So `cert_denotes` discharges the frontend premises **for this compilation**, and `compile_correct_cert` needs only `NativePremises`.

A **refusal certificate** proves `compile T₀ src = .error r` for each refused example.

**What is proved and what is attested:**
- The kernel proves the proposition: the exact source maps to the exact output, including both backend inputs (the `.ll` text and the LCB-IR module).
- The driver **compares the bytes** of the backend input it handed on (the `.ll` file to clang, the LCB-IR text to LCB, rendered with LCB's own printer) with the rendering of `expected`. This is a literal comparison, not a digest comparison. The driver also records the toolchain store paths that were used, and the image digest.
- The association between the source, the text, the toolchain and the image, and therefore `Built`, is **attested** by the driver plus the nix store, and listed in the trust inventory. A digest is identity, not correspondence.

**Budget, and the per-pass design.** The time and size of each certificate are measured and gated. The LL(1) table is a precomputed constant.

The certificate format is a **list of per-edge entries**, composed by the proved `compile_of_edges` (above). In slice 0, each edge is still checked by `decide` running that stage in the kernel, so in total the kernel does run the whole compiler. That is accepted for **slice 0 only**. Later, each edge carries a witness that is small and linear in program size: a name-binding map, an ANF map, an SSA dominance witness, or a register assignment. A proved validator checks the witness, and the universal pass theorem composes with it. From then on, the kernel never re-runs the whole compiler (R-0552/R-0521).

## Stage registry (`Concrete/Stages.lean`)

`compile` is written as an explicit sequence of named stage calls that records every intermediate in `Output`. Each stage is declared with a **fixed contract proposition** and evidence for **exactly that contract**:

```lean
/-- The contracts are fixed per stage in ONE table and stated about the stage function itself. -/
def lowerContract (f : Slice0.Program × Tokens → Except Refusal MIR.Program) : Prop :=
  ∀ q t m, WellScoped q → TokensValid q t → f (q, t) = .ok m →
    m.WF ∧ MIR.behaviour m = Core.behaviour (toCore q)
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

The contract is an index, not a field, and it is applied to `run`. So `Contract := True` is impossible unless the golden table says so, and an unrelated theorem doesn't typecheck. The contract table's definitions are covered by the digest gate. The backend emits are stages too. Each has `native` evidence for its own backend family, as the golden table fixes. For the adapter it is `ι := Unit`, `B _ := lcb`. For the printer it is `ι := (String → BodyResult → Prop)`, `B M := llvm M`, so the contract is `∀ M, …`, with `M` in scope. `adapter_correct` turns LCB's evidence into `proof`. The glue between stages is stated as theorems:
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
| Termination (artifact) | `conditional`, on the backend's own premises: LLVM `P_print, P_toolchain[llvm], P_rt, P_launch, P_live`; LCB `P_adapter` (until `adapter_correct`), `P_toolchain[lcb], P_rt, P_launch, P_live`. |
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
3. **Premise registry and stage coverage:** the checks described in the stage registry section above.
4. **Conformance, both backends:** each `tests/slice0/*.con` example runs through:
   - the Lean `run`;
   - the LLVM-native executable;
   - the LCB-native executable.

   All three must give the same observation, and each must match the `.expect`.

   **Classification uses the status and the runtime evidence, never the status alone.** `Observation.status` is many-to-one.
   - Status 134 **with** the runtime's trap line on fd 2 is `trapped k`.
   - Status 134 **without** it is `exited 134`.
   - Status 125 **with** the launcher's refusal line is `startupRefused`.
   - Status 125 **without** it is `exited 125`.

   Accepted examples `return 125;` and `return 134;` are in the corpus and must classify as exits. Slice 0 can't produce a real startup refusal, because it has no grants. The refusal and trap classifications are also exercised by comparator self-tests: stub images, and the trap via gate 6.

   **Refused examples have their own leg.** The Lean `check` result, the compiler's refusal (with its rule) and the refusal certificate must agree with `.expect`. No executable exists for a refused example. A separate supported-subset coverage report lists every grammar production and Core form that the corpus exercises. A reject-all compiler fails, because every accepted example must exit with its status.
5. **Differential at the source:** generated accepted programs, with replayable seeds; native exit = Lean `run`.
6. **Differential through the token-free lowering, both backends.** This covers overflow. Generated **well-scoped, unchecked** slice-0 programs over **arbitrary** `u8` operands, including `200 + 56`, take `lower q ∅ → memory → ssa`, so they have no tokens and no checker. Their Core `run` is compared with the LLVM path and the LCB path. Overflow must give exit 134 and the right site's trap line on fd 2, for both backends. This exercises token-aware lowering, Memory CFG and SSA, not only the backends.
   A second leg compares generated BIR directly with both backends, to cover BIR shapes that lowering does not produce.

   An in-process leg also compares `BIR.behaviour b` with LCB's own evaluator on `adapt b`. That is the executable shadow of `adapter_correct` until it is proved.
6a. **AArch64:** a GitHub-hosted `ubuntu-24.04-arm` job builds the LCB output for `aarch64-linux`, runs gates 4 and 6 there natively, and compares against Lean. LLVM-native also runs there, as a third cross-check. Gates 4 and 6 for x86-64 run on concrete-ci-2.
7. **Certificates:** the kernel checks every acceptance and refusal certificate, with measured budgets.

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

Open: the exact bytes of the trap line. Its content is decided by OD101 (one line naming the trap and its location), but its form is not; the spec does not model the bytes. Until the owner fixes them, the runtime prints the provisional placeholder `concrete: trap: <kind> at <site>`.

## Follow-ups (recorded after the final review round, round 8)

These are internal. None blocks slice 0's correctness.

- **Retire premises:**
  - `lex_correct` (P_lex);
  - the generic table-parser theorem (P_parse);
  - `adapter_correct` (P_adapter, a slice-0 milestone);
  - a proof of the decoding entry against a libc model (P_rt).
- **P_toolchain[lcb]** is retired by LCB's own C01–C03 work. LCB's verified profile names Apple AArch64 as its intended target. The Linux `x86_64` and `aarch64` executables go through LCB's compatibility pipeline, inside P_toolchain[lcb], until LCB certifies those targets.
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
- **Integrated Core:** retarget `toCore` to the integrated Core when A2i/R-0560 lands.
- **The spec's pre-decision admission rule** (`knownViolates`-only refusal, `RangeOk`, R-0574). It does not bite slice 0, where every value is known. It must be closed before the language grows unknown values.
- **Generated views:** the stage, premise and fact tables here become views of the Lean registries, gated against drift.
- **Rebase:** rebase onto main, under a new branch name, when `wip/spec-root` lands.
- **Slice order after slice 0** (owner, 2026-11-06). Each slice is a thin vertical slice, proven end to end:
  1. integers complete: all widths, wrapping/saturating, comparisons, bitwise, shifts;
  2. capabilities: `main` grants, `with(Console)`, writing an integer, with output compared across both backends and the Lean interpreter;
  3. `bool`/`if`/`while` and `#[variant]`;
  4. functions, calls, recursion, capabilities through calls, contracts;
  5. the trap checker;
  6. `Secret<T>` on scalars;
  7. enums, `match`, `Result` and `checked_*`;
  8. structs and ownership;
  9. borrows;
  10. allocation, arrays and memory bounds.
- **Extension without reshaping.** BIR's integer types are the spec's `IntTy`: all eight widths and both signednesses, with values as in-range mathematical integers. `BinOp` grows the bitwise operations, shifts and comparisons. Saturating and checked operations lower to `binOvf` plus branches or selects.
  - **The LCB adapter** covers types up to 16 bits today, where exact results fit a word. For 32 and 64 bits it needs `long` intermediates and a different overflow test, because LCB's verified profile has no extension operations.
  - **Slice 2** turns console output into an observation beyond the exit code. `Behaviour` gains an event trace, and BIR gains an effect instruction naming its capability. The runtime entry gains the grant table and the startup refusal (status 125).
  - **The result word** keeps encoding only how the run ends.
  - **LCB's verified profile has no calls**, so slice 2 needs LCB calls in that profile, or an event protocol through the result. That needs agreeing with LCB.
- **BIR well-formedness** makes dominance hold by construction: values cross blocks only as block arguments, every operand is defined earlier in its own block, and every non-entry block has a predecessor. Prove that a well-formed program never reads an undefined register (progress) before `ssa_correct` relies on it.

## Not in slice 0

- Mono Core.
- Grants, the launcher's startup refusal (125) and events.
- LP0 beyond slice 0: the slice-1 subset through LCB, and LCB's preservation theorem replacing `P_toolchain[lcb]`.
- An LLVM-IR semantics. The LLVM meaning `M` stays a premise parameter until one exists, and then `P_print` becomes a theorem.
- The information-flow and constant-time analysis. The surface and the observer are decided; slice 0 has only the fact fields.
