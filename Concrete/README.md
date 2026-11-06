# The Concrete compiler: slice 0 design (provisional)

PROVISIONAL. This is a design sketch, revision 2, written after the first Codex and agy review round, and there is no code yet. Slice 0 is the first compiler slice. It is built before the owner freezes Core (A2c) and makes no normative claim before that freeze ([decision](../docs/decisions.md#decided-project-proof-infrastructure)). Branch `wip/slice0` depends on `wip/spec-root`, which puts `ConcreteSpec` on the new root, and is rebased onto main (under a new branch name) once that lands.

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
BIR  (`out.bir`)
  │ print               LLVM.lean     LLVM text (`out.ll`); premise P_print (implementation)
  ▼
.ll text
  │ clang/llc, link     premise P_toolchain (toolchain); pinned by the flake
  ▼
executable
  │ run                 premise P_launch (environment), under the target profile
  ▼
observed behaviour
```

**`compile` is defined as the composition of the stage functions listed in `Stages.lean`**, so a pass that isn't registered can't be in the pipeline. `Output` is pure data with `DecidableEq`: `{ q, mir, bir, ll }`. There are no proofs inside it, and every theorem is stated *about* `compile`.

There's no Mono Core, since slice 0 has no generics.

**Place MIR** is where later slices put evaluation order, cleanup edges and explicit checks. In slice 0 it makes the overflow check explicit.

**BIR** is the boundary that the printer, and later the LCB adapter, read. It uses block-argument SSA, and `add.ovf` returns `(value, flag)`. It has `br`/`condbr` and a `trap k` terminator.

Each IR has a total, fuel-indexed evaluator returning the spec's `Outcome` and a decidable well-formedness predicate.

## Layout (lean root, few cohesive folders)

**New root entries.** There are only three, each with a stated reason:

| Entry | Purpose |
|---|---|
| `Concrete.lean` | The library root that lake requires; it imports every stage. It is the same pattern as `ConcreteSpec.lean`. |
| `Concrete/` | The compiler: one flat folder, one file per pipeline stage or concern. |
| `tests/` | The executable test corpus. Each slice gets one subfolder, which holds `.con` sources and their `.expect` outcomes. |

**Changes to existing entries.** No new entry is created for any of these:
- `lakefile.toml` gains the `Concrete` lib and the `concrete` exe.
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
  LLVM.lean            BIR → LLVM text printer (unverified; covered by P_print)
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
- There are no subfolders inside `Concrete/` until a stage outgrows one file. A split then creates one folder for that stage, never a one-file folder.
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
- **`Denotes src q := ∃ toks ast, Lexes src toks ∧ Derives toks ast ∧ Elab ast q`.** Its theorem is `denotes_outcome_det`: two denotations of the same `src` have the same `run` outcome.

## Premises and target profile (`Concrete/Premises.lean`: the one file)

```lean
/-- Behaviour of a whole run, maximal: a run that does not terminate is `diverges`, not absent. -/
inductive Behaviour | exited (c : Nat) | trapped (k : Trap) | diverges | killed

/-- The target profile: pinned triple, toolchain store paths, frame stack budget, launch rules. -/
structure TargetProfile where triple : String; toolchain : String; stackBytes : Nat

/-- The native world, a parameter, not an opaque constant, so consistency is witnessable. -/
structure Native where
  Image : Type
  LLMeans : String → Behaviour → Prop          -- what the pinned LLVM takes `.ll` text to mean
  Built : TargetProfile → String → Image → Prop -- image produced from `.ll` by the pinned toolchain
  Runs : TargetProfile → Image → Behaviour → Prop -- a behaviour of a launch admitted by the profile

structure Premises (N : Native) (T : TargetProfile) : Prop where
  /-- P_lex (implementation), until `lex_correct` lands. -/
  lex   : ∀ src toks, Lex.lex src = .ok toks → Surface0.Lexes src toks
  /-- P_parse (implementation), until the generic table-parser theorem lands. -/
  parse : ∀ toks ast, Parse.parse toks = .ok ast → Surface0.Derives toks ast
  /-- P_print (implementation): the printer's text means exactly the BIR behaviour. -/
  print : ∀ b, b.WF → ∀ β, N.LLMeans (LLVM.print b) β ↔ β = BIR.behaviour b
  /-- P_toolchain (toolchain): every behaviour of the built image is one the text means. -/
  toolchain : ∀ ll img β, N.Built T ll img → N.Runs T img β → N.LLMeans ll β
  /-- P_launch (environment): an admitted launch with stack ≥ the frame bound is never `killed`
      for resource reasons; the profile names what "admitted" excludes (signals, ptrace, rlimits). -/
  launch : ∀ ll img, N.Built T ll img → ∀ β, N.Runs T img β → β ≠ .killed
```

`BIR.behaviour` is maximal: `exited`, `trapped` or `diverges`, defined from the fuel-indexed evaluator by "some fuel exits or traps, otherwise diverges". It is noncomputable, which is fine because it is used only in statements.

The **execution-trust inventory** is listed in the same file as documented non-Prop entries:
- the Lean compiler and runtime executing `compile`. The per-compilation certificate closes this for each output.
- the IO driver, which writes `out.ll` byte-for-byte; the certificate's digest check covers it.

## The end-to-end theorem (`Concrete/Theorem.lean`)

```lean
theorem compile_correct {N : Native} {T : TargetProfile} (P : Premises N T)
    {src : ByteArray} {out : Output} {img : N.Image}
    (hc : compile src = .ok out) (hb : N.Built T out.ll img) :
    Surface0.Denotes src out.q ∧ Checked (Slice0.toCore out.q) ∧
    ∃ c, c < 256 ∧ run (Slice0.toCore out.q) (out.q.body.length + 1) = .exit c ∧
      ∀ β, N.Runs T img β → β = .exited c
```

The theorem is stated about **`out.q`** itself, so program theorems transport directly:

```lean
theorem answer_correct (P : Premises N T) (hb : N.Built T (compile answerSrc).ll img) :
  ∀ β, N.Runs T img β → β = .exited 42
```

**The composition:**
- `lex`/`parse` (as premises) and `elab_sound` give `Denotes`;
- `check_iff` gives `Checked`;
- `slice0_exits` gives the exit;
- `lower_correct` and `ssa_correct` (below) take the BIR behaviour to the Core outcome;
- `P.print`, `P.toolchain` and `P.launch` take that behaviour to the image.

Termination is proved at the source level. Native termination comes only through the three named native premises, and `P.launch` carries the resource side (the stack bound).

**Preservation over all programs, so the trap path isn't dead code:**
- `lower_correct : ∀ q, WellScoped q → MIR.run (lower q) = run (toCore q)`. This holds for *every* well-scoped `u8` program, checked or not, so `200 + 56` preserves `.trap .arithmeticOverflow`.
- `ssa_correct : ∀ m, m.WF → BIR.run (ssa m) = MIR.run m`, plus `(ssa m).WF`.
- `addOvf_correct`: for arbitrary `u8` operands, the result is `(a+b) % 256` and the flag is `a+b ≥ 256`.
- `lower_emits_check`, a syntactic output invariant: every MIR add is followed by a conditional branch on its flag to `trap .arithmeticOverflow`.

**Non-vacuity, one witness per layer:**
1. **Acceptance:** `compile answerSrc = .ok answerOut`, by kernel `decide`.
2. **Model consistency:** `Premises refNative refProfile` is proved for a reference model, where `Image := BIR.Program`, `Built` means "image is the BIR the text prints from" and `Runs := BIR.behaviour`. This shows that the premises together are satisfiable, not contradictory.
3. **Native-build existence:** this is evidence, not a theorem. The conformance job records a real `Built` instance per example: toolchain provenance and image digest. It is reported as attested.

## Per-compilation certificate (`Concrete/Cert.lean`), R-0552 first increment

The driver emits `Cert_<digest>.lean`, and the kernel checks it (`lake env lean`, no `native_decide`):

```lean
def srcBytes : ByteArray := ...      -- the exact input
def expected : Output := ...         -- the exact q, mir, bir, ll
theorem cert : compile srcBytes = .ok expected := by decide
```

`ll` is a field of `Output`, so the certificate covers the printer's output too. The driver then checks:
- that the `.ll` file on disk is byte-equal to `expected.ll`, by digest;
- that the build used the profile's toolchain store paths;
- the SHA-256 digests of the image, compiler, spec and target.

A **refusal certificate** proves `compile src = .error r` for each refused example, so the refusal and its rule are kernel-checked too.

**Budget.** Kernel `decide` over the whole slice-0 compiler is feasible for small inputs. The LL(1) table is a precomputed constant. The time and size per certificate are measured and gated. For later slices, the long-term plan is per-pass translation validators with kernel-checked validator soundness, not whole-compiler normalization; it's recorded under R-0552.

## Stage registry (`Concrete/Stages.lean`)

Each stage is a value of a **typed** contract:

```lean
structure Stage (A B : Type) where
  run : A → Except Refusal B
  pre : A → Prop
  post : A → B → Prop
  evidence : (∀ a b, pre a → run a = .ok b → post a b) ⊕ PremiseField
```

So an unrelated theorem doesn't typecheck as a stage's evidence. `compile` is the fold of the stage list. `pipeline_correct` is proved by composing the `post` contracts, and `compile_correct` is derived from it.

The gate:
- enumerates the stage list from the elaborated `compile` term, independently;
- compares the `Premises` fields with a golden list;
- compares digests of the semantic definitions those premises mention (`BIR.behaviour`, `WF`, `observe`, `TargetProfile`) with a golden file.

Changing a premise's meaning therefore fails CI as surely as adding a field does, and needs owner approval: holes only shrink.

## Facts query (`Concrete/Facts.lean`)

The query follows the reviewed facts-query design (Codex, agy and Kimi, 2026-11-06). Slice 0's facts are simple, but they use the full shape.

**One derivation.** `derive : Output → FnId → Report` runs over the whole lowered bundle, so BIR slot counts are visible to it. Each fact is a typed judgment `Fact.Holds`. `derive_sound` covers **every** status:
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
| Trusted / FFI / opaque (artifact) | `conditional(P_toolchain, P_launch)`. |
| Termination (source) | `established` (`slice0_exits`). |
| Termination (artifact) | `conditional(P_print, P_toolchain, P_launch)`. |
| Recursion depth bound | `established`: 0, since there are no calls. |
| May block | `established`: no. |
| BIR frame slots (IR) | `established`: *n*. Reported as slots, never as bytes. |
| Stack bytes, stack exhaustion (artifact) | `notEstablished`: no final-artifact/target stack evidence. |
| Bounded (derived: terminates ∧ stack bound ∧ no blocking) | `notEstablished`, blocked on the stack bytes. |

**Identity and replay.** Every report carries:
- typed function and site identities;
- the schema version `concrete-facts/0` and the semantics version;
- the digests of the compiler, spec, target and artifact;
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
4. **Conformance:** each `tests/slice0/*.con` example runs through the Lean `run` and through the native runner, and both must match its `.expect` (an exit status, or a refusal with its rule). A separate supported-subset coverage report lists every grammar production and Core form that the corpus exercises. A reject-all compiler fails, because every accepted example must exit with its status.
5. **Differential at the source:** generated accepted programs, with replayable seeds; native exit = Lean `run`.
6. **Differential at BIR → native:** this covers overflow. Generated BIR programs over **arbitrary** `u8` operands, including `200 + 56`, go through `BIR.behaviour` and through print plus clang plus execution. Overflow must give exit 134 and the trap line on fd 2.
7. **Certificates:** the kernel checks every acceptance and refusal certificate, with measured budgets.

**Negative controls**, each a mutation the matching gate must refuse:
- a deleted pass theorem;
- an unrelated theorem substituted as a stage's evidence;
- a silently omitted stage;
- an extra or strengthened premise;
- a changed premise-definition digest;
- a certificate for the wrong source, BIR or `.ll`;
- a swapped executable or toolchain;
- a reject-all compiler;
- a printer that drops the overflow check (gate 6);
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
- The LCB path (LP0).
- An LLVM-IR semantics. `LLMeans` stays a premise parameter until one exists, and then `P_print` becomes a theorem.
