# The Concrete compiler: slice 0 design (provisional)

PROVISIONAL. This is a design sketch, revision 4, written after three Codex and agy review rounds, and there is no code yet. Slice 0 is the first compiler slice. It is built before the owner freezes Core (A2c) and makes no normative claim before that freeze ([decision](../docs/decisions.md#decided-project-proof-infrastructure)). Branch `wip/slice0` depends on `wip/spec-root`, which puts `ConcreteSpec` on the new root, and is rebased onto main (under a new branch name) once that lands.

## What slice 0 accepts

Slice 0 has one source file and no imports or grants. Its forms:
- a parameterless `fn main() -> u8`;
- immutable `let` bindings of explicitly typed `u8` values;
- decimal `u8` literals;
- reads of locals;
- grouping with parentheses;
- checked `+`;
- a final `return`.

Every other form is refused as unsupported.

The required examples:
- exit 42 (`20 + 22`);
- exit 255, the boundary;
- refusals:
  - a known overflow;
  - an out-of-range literal;
  - an unused binding;
  - an undeclared local;
  - malformed input.

## The pipeline

Each arrow is a total Lean function of the compiler. **Proved** means a kernel-checked theorem. **Premise** means a field of the one premise structure in `Concrete/Premises.lean`.

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
Slice0.Program  (data, `out.q`)
  │ check               the spec's own `ConcreteSpec.check`; `check_iff` gives `Checked (toCore q)`
  ▼
  │ lower               MIR.lean      Place MIR; proved over ALL well-scoped programs, not only checked ones
  ▼
Place MIR  (`out.mir`)
  │ ssa                 BIR.lean      BIR; proved over all well-formed MIR
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

**`compile` is defined as the composition of the stage functions listed in `Stages.lean`**, so a pass that isn't registered can't be in the pipeline. `Output` is pure data with `DecidableEq`: `{ q, mir, bir, ll, lcb }`. There are no proofs inside it, and every theorem is stated *about* `compile`.

There's no Mono Core, since slice 0 has no generics.

**Place MIR** is where later slices put evaluation order, cleanup edges and explicit checks. In slice 0 it makes the overflow check explicit.

**BIR** is the one boundary that both backends read, and it is designed for both from the start:
- block-argument SSA;
- `add.ovf` returns `(value, flag)`;
- `br`/`condbr`;
- a `trap k` terminator.

Every BIR construct has a direct LLVM spelling (`@llvm.uadd.with.overflow.i8`) and a direct LCB-IR expansion: a widened add, a compare, and a branch to the trap block. No construct exists only for one backend.

**Both backends run in slice 0.**

The **LLVM printer** is unverified and stays a premise (`P_print`) until an LLVM-IR semantics exists.

The **BIR → LCB-IR adapter** is the first backend proof target. `adapter_correct` states that the adapter's output, under **LCB's own semantics** (`LeanCompilerBackend.Semantics`, imported and never restated), has the BIR behaviour. Until it is proved, it is premise `P_adapter`, and closing it is a slice-0 milestone.

LCB's own lowering, assembler and linker are LCB's named trust boundaries, carried as `P_toolchain[lcb]` until LCB's preservation theorem covers the slice-0 domain.

LCB is a pinned lake dependency (`require` with an exact rev, LCB's upstream lock recorded). It uses the same Lean toolchain (v4.34.1) and no Mathlib.

**The encoded result, with one decoding runtime.** Neither backend's input contains a call or a trap.

Each backend emits `main`'s body as a function `concrete_body() -> w`, a 32-bit word, whose return value **encodes** the behaviour:
- `exited c` returns `c` (0–255);
- `trapped k` returns `256 + idx k`.

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
  MIR.lean             Place MIR: syntax, semantics, WF, lowering, lower_correct
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
- Every entry has exactly one home: tests are in `tests/`, gates in `scripts/checks/`, the spec in `ConcreteSpec/`.

The root inventory (`scripts/checks/root_inventory.txt`) lists each new root entry in the same change that creates it. An unlisted entry fails the check, and so does a listed entry that is missing.

## Frontend specifications (new spec file `ConcreteSpec/Spec/Surface0.lean`)

These are new files only. No existing spec file changes, so there is no conflict with the spec worker. This is spec open item O1, for slice 0 only.

- **Bytes.** Non-UTF-8 input is refused before lexing (SYN-5).
- **Lexical spec `Lexes : Bytes → List Token → Prop`.** It is defined independently of the lexer:
  - maximal munch;
  - trivia: spaces, newlines and `//` comments, all consumed;
  - EOF must be consumed;
  - every byte is covered exactly once by a token or by trivia;
  - payloads: identifier strings and decimal values.

  The lexer's theorem is soundness plus completeness against `Lexes`, which rules out splitting `ab` into `a b` or `256` into `25 6`. Literals with a leading zero are open owner question 3.
- **The grammar as data:**
  ```
  program  = 'fn' IDENT '(' ')' '->' IDENT block ;
  block    = '{' let_list 'return' expr ';' '}' ;
  let_list = 'let' IDENT ':' IDENT '=' expr ';' let_list | ε ;
  expr     = term expr_tail ;
  expr_tail= '+' term expr_tail | ε ;
  term     = INT | IDENT | '(' expr ')' ;
  ```
  It is right-factored, so it is LL(1). The nullable nonterminals are `let_list` and `expr_tail`.
- **The LL(1) checker `ll1 : Grammar → Bool`.** It computes nullable, FIRST and FOLLOW sets and checks for conflicts. It comes with `ll1_sound : ll1 g = true → ∀ n t, at most one production of n is predicted on t`. The fact `ll1 grammar0 = true` is proved by `decide`.
- **`Derives : List Token → Surface.Program → Prop`.** It binds token payloads, not only token kinds.
- **`Elab : Surface.Program → Slice0.Program → Prop`.** It:
  - left-associates `+` chains, preserving the decided left-to-right order (STMT-4);
  - introduces ANF temporaries;
  - refuses duplicate names (no shadowing) and undeclared names;
  - refuses a function name other than `main`, or a type other than `u8`, as "unsupported in slice 0".

  Two theorems come with it: `elab_fresh` (temporaries never collide with user names) and `elab_rename_invariant` (renaming temporaries changes neither acceptance nor outcome).
- **`Denotes src q := ∃ toks ast, Lexes src toks ∧ Derives toks ast ∧ Elab ast q`.** Its theorem is `denotes_behaviour_det` (see the theorem section).

## Behaviours: the fuel-free semantic interface

Every level (Core, MIR, BIR) exposes a **maximal behaviour** derived from its fuel-indexed evaluator:

```lean
inductive Behaviour | exited (c : Nat) | trapped (k : Trap) | diverges | stuck | refused | killed
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

/-- A backend: the total Lean function from BIR to the text a native toolchain consumes, and
    what that text means. For LCB, `Means` is DEFINED from LCB's imported semantics, so the
    emit obligation is a theorem target. For LLVM it is a parameter, so it stays a premise. -/
structure Backend where
  Text : Type
  emit : BIR.Program → Text
  Means : Text → Behaviour → Prop

def llvm (LLMeans : String → Behaviour → Prop) : Backend := ⟨String, LLVM.print, LLMeans⟩
def lcb : Backend := ⟨LCB.Source.Function, LCB.adapt, fun f β => LCB.behaviour f = β⟩
/-- From LCB's own source semantics (imported, never restated):
    `decode w` if `LCB.Source.run` returns `w`, `diverges` if `LCB.Source.Diverges`, otherwise `stuck`.
    It is noncomputable (classical), and used only in statements. Its definition is in the digest gate. -/
noncomputable def LCB.behaviour (f : LCB.Source.Function) : Behaviour := ...

/-- The native world for one backend, a parameter so its consistency is witnessable. -/
structure Native (B : Backend) where
  Image : Type
  Built : TargetProfile → B.Text → Image → Prop      -- image from text by the pinned toolchain
  Runs : TargetProfile → Image → Behaviour → Prop    -- a behaviour of a launch the profile admits

/-- `Behaviour` gains the native-only `refused` (startup refusal, status 125) and `killed`.
    No model program has either: slice 0 has no grants. -/
structure NativePremises (B : Backend) (N : Native B) (T : TargetProfile) : Prop where
  /-- emit (implementation): the emitted text means at most the BIR behaviour.
      LLVM: premise P_print. LCB: premise P_adapter until `adapter_correct` is proved, after
      which this field is filled by the theorem and the premise is deleted. -/
  emit : ∀ b, b.WF → ∀ β, B.Means (B.emit b) β → β = BIR.behaviour b
  /-- P_toolchain[B] (toolchain): every non-killed behaviour of the image is one the text means.
      Both backends include `P_rt`, the decoding entry linked with the body (see Runtime).
      LCB also includes LCB's own importer, lowering, assembler and linker: LCB's named trust
      boundaries (`docs/SEMANTICS.md`, "Target and environment assumptions"). -/
  toolchain : ∀ b img β, b.WF → N.Built T (B.emit b) img → N.Runs T img β → β ≠ .killed →
    B.Means (B.emit b) β
  /-- P_launch (environment, resources): an admitted launch of an image built from a WF BIR
      within the profile's frame bound is never killed. Here "admitted" excludes signals, ptrace
      and rlimits below `stackBytes`. -/
  launch : ∀ b img β, b.WF → b.frameSlots ≤ T.maxSlots →
    N.Built T (B.emit b) img → N.Runs T img β → β ≠ .killed
  /-- P_live (environment): an admitted launch happens and has a behaviour (maximal: a run that
      never ends has behaviour `diverges`). -/
  live : ∀ b img, b.WF → N.Built T (B.emit b) img → ∃ β, N.Runs T img β
```

Every premise is used in the composition. `emit`, `toolchain` and `launch` give "every run is the BIR behaviour", and `live` makes that non-vacuous for the image.

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
| P_launch | environment | not planned |
| P_live | environment | not planned |

P_lex and P_parse are also discharged for each compilation by the certificate.

The **execution-trust inventory** is in the same file, as documented non-Prop entries:
- the Lean compiler and runtime executing `compile`. The certificate closes this for each output.
- the driver's IO: the byte-for-byte comparison of the emitted `.ll` or LCB-IR file, and the toolchain invocation and its provenance record. These are attested, not proved (see the certificate section).
- LCB's own native execution: it lowers LCB-IR to assembly as a Lean-compiled program. This is inside `P_toolchain[lcb]`.

## The end-to-end theorem (`Concrete/Theorem.lean`)

```lean
/-- What `compile` returns: pure data, every intermediate recorded. -/
structure Output where
  q : Slice0.Program; mir : MIR.Program; bir : BIR.Program; ll : String; lcb : LCB.Source.Function

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
   - `out.mir = lower out.q`, `out.bir = ssa out.mir`, `out.ll = LLVM.print out.bir` and `out.lcb = LCB.adapt out.bir`;
   - `out.bir.frameSlots ≤ T.maxSlots`.
2. `F.lex`, `F.parse` and `elab_sound` give `Denotes src out.q`. `check_iff` gives `Checked`.
3. `slice0_exits` gives `run … = .exit c` with `c < 256`; `run_exit_behaviour` then gives `Core.behaviour = .exited c`.
4. `checked_wellScoped : Checked (toCore q) → WellScoped q`. `WellScoped` includes in-range literals, because Core's `run` does not range-check literals.
5. `lower_wf : WellScoped q → (lower q).WF`.
6. `lower_correct : WellScoped q → MIR.behaviour (lower q) = Core.behaviour (toCore q)`. This holds for every well-scoped program, checked or not, so `200 + 56` preserves `trapped .arithmeticOverflow`. The trap path is real.
7. `ssa_wf : m.WF → (ssa m).WF`, and `ssa_correct : m.WF → BIR.behaviour (ssa m) = MIR.behaviour m`.
8. `P.launch` excludes `killed`. `P.toolchain` then gives `B.Means (B.emit out.bir) β`, and `P.emit` gives `β = BIR.behaviour out.bir = .exited c`. `P.live` gives existence.

**Supporting theorems:**
- `addOvf_correct`: for arbitrary `u8` operands, the result is `(a+b) % 256` and the flag is `a+b ≥ 256`.
- `adapter_correct : b.WF → LCB.behaviour (LCB.adapt b) = BIR.behaviour b`. It is proved over all WF BIR, overflow included. LCB's semantics is imported from `LeanCompilerBackend.Semantics` and never restated. It fills `NativePremises lcb`'s `emit` field.
- `lower_emits_check`: every MIR add is followed by a branch on its flag to `trap .arithmeticOverflow`.
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
   - `Runs T b β := β = BIR.behaviour b`;
   - `refMeans ll β := ∃ b, b.WF ∧ LLVM.print b = ll ∧ β = BIR.behaviour b`.

   The `print` field needs **printer injectivity**, which is proved: `LLVM.unprint (LLVM.print b) = some b` for every WF `b`, so `print_injective` follows. This also makes the `.ll` text determine its BIR, which the certificate uses.
   For LCB, `Means` is fixed by LCB's semantics. The witness `NativePremises lcb refNativeLCB T₀`, where `Image := LCB.Source.Function` and `Runs := LCB.behaviour`, follows from `adapter_correct`, so it lands with that proof.
3. **Frontend premises:** these are claims about fixed code, not something a model can satisfy, so no model witness applies. They are discharged per compilation (next section), and universally once `lex_correct` and `parse_correct` land.
4. **Native-build existence:** this is attested, not proved. The conformance job records toolchain provenance and the image digest for each example.

## Per-compilation certificate (`Concrete/Cert.lean`), R-0552 first increment

The driver emits `Cert_<digest>.lean`, which the kernel checks (`lake env lean`, no `native_decide`):

```lean
def srcBytes : ByteArray := ...      -- the exact input
def expected : Output := ...         -- the exact q, mir, bir, ll, lcb
theorem cert_compile : compile T₀ srcBytes = .ok expected := by decide
def toks : List Token := ...         -- witnesses emitted by the driver
def ast : Surface.Program := ...
theorem cert_denotes : Surface0.lexesB srcBytes toks && Surface0.derivesB toks ast &&
    Surface0.elabB ast expected.q = true := by decide
```

`Lexes`, `Derives` and `Elab` each have a Boolean checker over explicit witnesses, with a one-way soundness lemma (`lexesB_sound` and so on, giving `Denotes src q`). So `cert_denotes` discharges the frontend premises **for this compilation**, and `compile_correct_cert` needs only `NativePremises`.

A **refusal certificate** proves `compile T₀ src = .error r` for each refused example.

**What is proved and what is attested:**
- The kernel proves the proposition: the exact source maps to the exact output, including both backend inputs (the `.ll` text and the LCB-IR module).
- The driver **compares the bytes** of the backend input it handed on (the `.ll` file to clang, the LCB-IR text to LCB, rendered with LCB's own printer) with the rendering of `expected`. This is a literal comparison, not a digest comparison. The driver also records the toolchain store paths that were used, and the image digest.
- The association between the source, the text, the toolchain and the image, and therefore `Built`, is **attested** by the driver plus the nix store, and listed in the trust inventory. A digest is identity, not correspondence.

**Budget.** The time and size of each certificate are measured and gated. The LL(1) table is a precomputed constant. Later slices move to per-pass translation validators with kernel-checked soundness, not whole-compiler normalization (R-0552).

## Stage registry (`Concrete/Stages.lean`)

`compile` is written as an explicit sequence of named stage calls that records every intermediate in `Output`. Each stage is declared with a **fixed contract proposition** and evidence for **exactly that contract**:

```lean
/-- The contracts are fixed per stage in ONE table and stated about the stage function itself. -/
def lowerContract (f : Slice0.Program → Except Refusal MIR.Program) : Prop :=
  ∀ q m, WellScoped q → f q = .ok m → m.WF ∧ MIR.behaviour m = Core.behaviour (toCore q)
-- … one per stage: lex, parse, elab, check, lower, ssa, and the two backend emits

inductive Evidence (C : Prop) where
  | proof (h : C)
  | frontend (h : FrontendPremises → C)                      -- P_lex, P_parse
  | native (h : ∀ B N T, NativePremises B N T → C)          -- emit fields: P_print, P_adapter

structure Stage (A B : Type) (Contract : (A → Except Refusal B) → Prop) where
  run : A → Except Refusal B
  evidence : Evidence (Contract run)
```

The contract is an index, not a field, and it is applied to `run`. So `Contract := True` is impossible unless the golden table says so, and an unrelated theorem doesn't typecheck. The contract table's definitions are covered by the digest gate. The backend emits are stages too, with `native` evidence until `adapter_correct` turns LCB's into `proof`. The glue between stages is stated as theorems:
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
| Implicit trap sites (source) | One per `+`, with a typed site id and a span; each is `established` as "cannot trap" (`slice0_never_traps`). |
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

The **CLI spellings are placeholders** (`concrete facts FILE [FN]`, `concrete build FILE -o OUT`), pending the owner.

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
6. **Differential at BIR → native, both backends:** this covers overflow. Generated BIR programs over **arbitrary** `u8` operands, including `200 + 56`, go through `BIR.behaviour`, through the LLVM path and through the LCB path. Overflow must give exit 134 and the trap line on fd 2 for both.

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

## Open questions for the owner (none blocks the skeleton)

1. The CLI spellings: the compiler command and the facts query.
2. Whether a decided form that slice 0 doesn't support (`0x2A`, `let mut`, `if`) gets a dedicated "unsupported in slice 0" refusal, distinct from a syntax error. That is proposed.
3. Decimal literals with leading zeros, such as `007`: SYN-6 is silent. Slice 0 will refuse them as unsupported until the owner decides.

## Not in slice 0

- Mono Core.
- Grants, the launcher's startup refusal (125) and events.
- LP0 beyond slice 0: the slice-1 subset through LCB, and LCB's preservation theorem replacing `P_toolchain[lcb]`.
- An LLVM-IR semantics. `LLMeans` stays a premise parameter until one exists, and then `P_print` becomes a theorem.
