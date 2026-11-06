# The Concrete compiler: slice 0 design (provisional)

PROVISIONAL (status: design sketch, under review; no code yet). Slice 0 is the first compiler slice. It is built before the owner freezes Core (A2c)
and makes no authoritative or normative claim before that freeze ([decision](../docs/decisions.md#decided-project-proof-infrastructure)).
Branch `wip/slice0` depends on `wip/spec-root`, which puts `ConcreteSpec` on the new root, and is
rebased onto main once that lands.

Slice 0 has one source file and no imports or grants: a parameterless `fn main() -> u8`,
immutable `let` bindings of explicitly typed `u8` values, decimal `u8` literals, reads of locals,
grouping with parentheses, checked `+`, and a final `return`. Every other form is refused as
unsupported. The required examples exit 42 (`20 + 22`) and 255 (the boundary), and refuse a known
overflow, an out-of-range literal, an unused binding, an undeclared local and malformed input.

## The pipeline

Each arrow is a function of the compiler, written in Lean and run as Lean. **Proved** means a
kernel-checked theorem; **premise** means a named, registered assumption in `Concrete/Premises.lean`.

```
source bytes (List UInt8)
  │ utf8 + lex          Concrete/Lex.lean       proved: lex is total; tokens re-print to the bytes they cover
  ▼
tokens (with byte spans)
  │ parse (LL(1))       Concrete/Parse.lean     table-driven from the grammar data in ConcreteSpec;
  ▼                                             premise P_parse (parse agrees with the grammar's derivations), discharged later
surface AST
  │ resolve             Concrete/Resolve.lean   names → fresh `Var` identities; undeclared and duplicate names refused
  ▼
HIR
  │ elaborate           Concrete/Elab.lean      ANF with fresh temporaries, left-to-right; proved sound against
  ▼                                             the elaboration relation `Surface0.Elab` (ConcreteSpec, new)
Slice0.Program  ──toCore──►  Core (ConcreteSpec, unchanged)
  │ check               the spec's own `ConcreteSpec.check`; `check_iff` gives `Checked`
  ▼
CheckedCore             { src, q, h : Checked (toCore q), hfront : FrontOk src q }
  │ lower               Concrete/MIR.lean       Place MIR: numbered slots, explicit `add.ovf` + check → trap;
  ▼                                             proved: preservation + output well-formedness
Place MIR
  │ ssa                 Concrete/BIR.lean       BIR: block-argument SSA, `add.ovf` returns (value, flag), `trap`
  ▼                                             terminator; proved: preservation + SSA well-formedness
BIR
  │ print (UNVERIFIED)  Concrete/LLVM.lean      LLVM IR text: `@llvm.uadd.with.overflow.i8`, a trap block
  ▼                                             that writes the trap line to fd 2 and exits 134
.ll text
  │ clang (pinned by the flake), link
  ▼
executable              premises P_print, P_llvm, P_link, P_load (see below)
```

There's no Mono Core in slice 0, because there are no generics; the pass gets added when a slice needs it.
**Place MIR** is where later slices put evaluation order, cleanup edges and explicit checks. In
slice 0 it already makes the overflow check explicit. **BIR** is the boundary the LLVM printer reads
and the future LCB adapter will read. Each IR has a total, fuel-indexed evaluator that returns the
spec's `Outcome`, plus a decidable well-formedness predicate.

## Files (new root entries)

| Path | Content |
|---|---|
| `ConcreteSpec/Spec/Surface0.lean` | **Spec, new file:** the slice-0 surface grammar **as data** (productions over token kinds), its mechanical LL(1) check (`decide`), the surface AST, and the elaboration relation `Elab : Surface.Program → Slice0.Program → Prop`. It changes no existing spec file. |
| `Concrete.lean`, `Concrete/` | The compiler library and the `concrete` executable. |
| `Concrete/Lex.lean`, `Parse.lean`, `Resolve.lean`, `Elab.lean` | The frontend. |
| `Concrete/MIR.lean`, `BIR.lean` | The IRs, with their semantics, well-formedness, lowering and preservation proofs. |
| `Concrete/LLVM.lean` | The BIR → LLVM text printer, labelled unverified. |
| `Concrete/Premises.lean` | **The one file of named premises** (the holes), each with its trust class: implementation, toolchain, environment. |
| `Concrete/Stages.lean` | The stage registry: every pipeline edge maps to a theorem or a premise. |
| `Concrete/Theorem.lean` | The composed end-to-end theorem and its non-vacuity witness. |
| `Concrete/Facts.lean` | The per-function facts, and a theorem for each fact kind that it is sound. |
| `Concrete/Cert.lean` | The per-compilation evidence emitter. |
| `Concrete/Main.lean` | The CLI (placeholder spellings, see below). |
| `conformance/slice0/*.con`, `*.expect` | The examples, each naming its rule, source and expected outcome. |
| `scripts/compiler/*.sh` | The runners and gates: conformance (Lean runner + native runner), differential, premise registry, certificates, negative controls. |

The root inventory (`scripts/checks/root_inventory.txt`) gains `Concrete`, `Concrete.lean`,
`conformance` and `scripts/compiler`. `lakefile.toml` gains a `Concrete` library and a `concrete`
executable.

## The end-to-end theorem (sketch)

```lean
-- Premises.lean: every unproved step, named; nothing else may appear as a hypothesis.
/-- What the parent process observes when the image runs (the target's observation, Q7.10). -/
opaque Image : Type
/-- `img` was produced by the pinned toolchain (clang, LLVM, linker, crt) from the LLVM text `ll`. -/
opaque Built : String → Image → Prop
/-- What the parent process observes when it runs `img` with no arguments and no grants. -/
opaque ExeObs : Image → Observe.Observation → Prop

structure Premises : Prop where
  /-- P_parse (frontend, implementation): the table-driven parser agrees with the grammar's derivations. -/
  parse : ∀ toks ast, Parse.parse toks = .ok ast → Surface0.Derives toks ast
  /-- P_native (toolchain + environment): printing, LLVM, linking, loading and exit. It is one
      premise for now, split into P_print / P_llvm / P_link / P_load as soon as an intermediate
      model (an LLVM IR semantics) exists. A split never strengthens it. -/
  native : ∀ (b : BIR.Program) (img : Image), b.WF → Built (LLVM.print b) img →
    ∀ o, ExeObs img o ↔ BIR.observe b = some o

-- Theorem.lean
/-- Slice-0 compiler correctness. If the compiler accepts `src`, emitting BIR `b`, and `img` is
built from `b`'s LLVM text, then the only thing the parent process can observe is the
observation the source program's Core semantics gives: an exit with the source's status
(slice 0 never traps, `slice0_never_traps`). -/
theorem compile_correct (P : Premises) {src : List UInt8} {out : Output} {img : Image}
    (hc : compile src = .ok out) (hb : Built (LLVM.print out.bir) img) :
    ∃ q, Surface0.Denotes src q ∧ Checked (Slice0.toCore q) ∧
      ∃ c, c < 256 ∧ run (Slice0.toCore q) (q.body.length + 1) = .exit c ∧
      ∀ o, ExeObs img o ↔ o = .exited c
```

This is the target-theorem shape (ROADMAP) reduced to slice 0:
- **Observable events:** slice 0 has none except the final observation.
- **Safety:** every observation of the image is the source's.
- **Termination:** proved for slice 0. Every accepted program exits (`slice0_exits`), so no fuel premise is needed.
- **Exact artifacts:** `src` is the exact byte list. `out.bir` is the exact BIR. `img` is tied to it only through `Built`, which the per-compilation certificate records by digest. A digest is identity, not correspondence; the correspondence is premise `native`.
- **Trusted base:** `Premises` plus the Lean kernel, plus Lean's compiler and runtime executing `compile`. The last is an execution assumption, listed in `Premises.lean` as a documented non-Prop entry. It's closed for each output by the certificate (below).
- **Authority:** vacuous. Slice 0 has no grants and no events.
- **Non-vacuity:** `compile_correct_witness` shows that `compile answerSrc = .ok _` holds by kernel `decide`, so the premises of the theorem are satisfiable for the 42 example.

**Per-pass theorems composed:**
- `lex_correct`;
- `resolve_correct` and `elab_sound` (→ `Surface0.Elab`);
- `check_iff` (spec);
- `lowerMIR_correct` (∀ q, Checked → MIR outcome = Core outcome, any fuel ≥ bound);
- `ssa_correct` (MIR → BIR, outcome-preserving, output `WF`);
- `addOvf_correct`: BIR's `add.ovf` on **arbitrary** `u8` operands gives `(a+b) mod 256` and the flag `a+b ≥ 256`, and the check branch traps exactly on overflow. This is the decision's "checked addition proved over arbitrary representable operands at the IR boundary, overflow included".

**Program correctness, first instance.** `answer_correct : ∀ img, Premises → Built (print (compile answerSrc).bir) img → ∀ o, ExeObs img o ↔ o = .exited 42`. It's derived from `compile_correct` plus the Lean fact `run (toCore answer) _ = .exit 42`.

**Per-compilation evidence (R-0552, first increment).** For each compiled file, the driver writes
a Lean file:

```lean
theorem cert : compile ⟨bytes⟩ = .ok ⟨bir⟩ := by decide
```

The kernel checks it (`lake env lean`; no `native_decide`). That connects the exact input to the exact BIR without trusting the natively executed compiler. The certificate also records the SHA-256 of the source, the `.ll` text, the executable and the toolchain identity.

## Gates (CI jobs, added to `required-checks.txt`)

1. **Compiler build:** `lake build Concrete`, with warnings as errors and `autoImplicit` off.
2. **Proof axioms and trust ban, extended to `Concrete`:** the same scripts as spec-root. Every
   theorem's axioms must be within `propext`, `Quot.sound` and `Classical.choice`; no `sorryAx` and no
   `ofReduceBool`. `opaque` constants are allowed only in `Premises.lean`, and are listed.
3. **Premise registry:** the type of `compile_correct` mentions no hypothesis except `Premises`
   and the stated arguments, compared against a golden statement. Every edge in `Stages.lean`
   names an existing theorem or a field of `Premises`. A field with no edge, or an edge with neither,
   fails. The premise list is golden, so adding or strengthening a premise fails CI until the owner
   approves (the rule that holes only shrink).
4. **Conformance (slice 0):** each example runs through the Lean semantics runner and the native
   runner (compile, then clang, then execute), and both must match `.expect`. That's an exit status, or a
   refusal with its rule. The suite's coverage is listed separately from compiler success: a
   compiler that rejects everything fails, because every accepted example must exit with its status.
5. **Differential:** generated slice-0 programs (random literals and sums) go through the Lean `run`
   and through the native executable, which must agree, with replayable seeds.
6. **Certificates:** the kernel checks the certificate of every conformance example.

**Negative controls**, each a mutation the matching gate must refuse:
- a deleted pass theorem;
- an extra unregistered hypothesis;
- a certificate for the wrong source or the wrong BIR;
- a compiler that rejects everything;
- a printer that emits `add` without the overflow check, which the differential must catch on 200 + 56 at the BIR boundary.

Heavy runs (native builds, differential) run on concrete-ci-2 or on GitHub-hosted runners, never as full local runs.

## The facts query (from day one)

The query follows the facts-query design that Codex, agy and Kimi reviewed (2026-11-06). Slice 0's facts are simple, but they use the full shape from the start, so later slices add fact kinds without changing the format.

**One derivation.** Each fact is a typed Lean judgment, `Fact.Holds : CheckedCore → FnId → Fact → Prop`, computed by `Facts.derive : CheckedCore → FnId → Report`. A derivation-soundness theorem `derive_sound : derive c f = r → ∀ fact ∈ r.established, Fact.Holds c f fact` holds for each fact kind. Human output, JSON and any future policy check all render the same `Report`; none of them re-derives anything.

**A status for each fact, and coverage for each fact:**
- `established`;
- `conditional(assumptions)`, naming the premises from `Premises.lean`;
- `refuted(witness)`;
- `notEstablished(blocker, site)`;
- `notApplicable`.

Coverage is reported for each fact: the sites examined out of the sites that exist.

**The vocabulary is split, and nothing is printed as a vacuous summary.** Empty capabilities are never printed as "touches nothing". Each axis is reported separately:
- **authority:** capabilities, and the grants they reach;
- **resources:** allocation;
- **memory:** reads, and separately writes;
- **failure:** each kind is its own fact:
  - implicit trap sites;
  - `Result` errors;
  - abort;
  - allocation failure;
  - stack exhaustion;
  - foreign failure.
- **trusted / FFI / opaque code;**
- **termination;**
- **stack;**
- **proofs:** the theorems and premises each claim depends on.

**Slice 0's values:**

| Fact | Slice-0 status |
|---|---|
| Authority | `established`: no capabilities and no grants. It is printed as "requires no capabilities", which is a statement about authority only. |
| Allocation | `established`: none. |
| Memory | `established`: reads and writes only its own frame slots. |
| Implicit trap sites | Each checked `+`, with its typed site identity and source span. Each is `established` as "cannot trap": its operands are known, so `slice0_never_traps` applies. |
| `Result` errors, abort, allocation failure, foreign failure | `notApplicable`. |
| Stack exhaustion, and stack bytes | `notEstablished`, with blocker "no final-artifact/target stack evidence" at the function. Stack bytes come only from final-artifact or target evidence. The BIR slot count is reported as an IR fact, never as bytes. |
| Termination | `established` (`slice0_exits`). |
| Trusted / FFI | `established`: none. |

**Identity and replay.** Every report carries:
- typed identities: function and site identities, never spellings;
- the schema version `concrete-facts/0` and the semantics version;
- the digests of the compiler, the spec, the target and the artifact;
- the replay command that regenerates the report.

The **CLI spelling is a placeholder** (`concrete facts FILE [FN]`, `concrete build FILE -o OUT`), pending the owner.

## Open questions for the owner (none blocks the skeleton)

1. The CLI spellings: the compiler command and the facts query.
2. How a decided but slice-0-unsupported form is reported: a dedicated "unsupported in slice 0" refusal is proposed, distinct from a syntax error, for `0x2A`, `let mut`, `if` and the like.

## Not in slice 0

- Mono Core.
- Grants, the launcher's startup refusal (exit 125) and the trap bytes. Slice 0 has no reachable trap: the BIR trap path exists and is proved, but no accepted program reaches it.
- The LCB path (LP0).
- Parser correspondence as a proof. It's a premise until discharged; the table-driven design makes it one generic theorem.
