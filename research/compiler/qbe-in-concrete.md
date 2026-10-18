# Implementing QBE In Concrete

Status: research

The original Concrete implementation proposal below is complemented by the
[2026-10-18 Lean 4 comparison](#lean-4-alternative-and-generated-runtime-2026-10-18).

This note covers the larger and much more speculative of the two QBE-related ideas:

- implement QBE itself in Concrete

That is a very different project from merely adding a QBE backend target.

## What This Would Mean

This would mean writing a compiler in Concrete that:

- consumes QBE-style IR
- performs QBE-style lowering/code generation work
- emits assembly or object code

In other words, this is not just "another backend file."
It is a serious compiler project and a partial self-hosting milestone.

## Why It Is Interesting

This idea is attractive because it lines up with several long-term Concrete ambitions:

- self-hosting pressure
- small auditable compiler components
- backend simplicity over giant backend frameworks
- a more proof-friendly backend target than LLVM

If it worked well, it could become:

- a strong showcase program
- a serious test of runtime and compiler-implementation maturity
- a possible long-term bootstrap/backend story

## Why It Is Much Bigger Than A QBE Backend

The smaller project is:

- `Concrete SSA -> QBE IL`

This bigger project is:

- "build a native-code backend compiler in Concrete"

That adds:

- parser/IR loading work
- instruction selection
- register allocation
- target code emission
- assembler/object integration strategy
- serious testing burden

So this should be thought of as:

- later
- optional
- earned only after the backend experiment proves QBE is strategically useful

## Why It Could Be Valuable

If Concrete ever wants a smaller backend story than LLVM, QBE-in-Concrete has several appealing properties:

- the implementation could stay much smaller than an LLVM-scale backend stack
- it would be a better showcase of Concrete's ability to implement real systems software
- it could improve auditability of the code generation path
- it might become a cleaner long-term bootstrap story than "Lean compiler + LLVM forever"

## Why It Is Still High Risk

This idea has major risks:

- it is a large project that can absorb a lot of time
- it may arrive before Concrete's package/tooling/product story is mature enough
- codegen quality may still lag LLVM significantly
- the project could drift into "compiler backend research" at the expense of Concrete's more distinctive evidence/audit identity

That means this idea must be gated by strong evidence.

## What Must Happen First

Before this deserves implementation, Concrete should already have:

- a working QBE backend experiment
- evidence that QBE is strategically valuable for Concrete
- stronger package/workspace/tooling maturity
- stronger compiler artifact and driver discipline
- enough language/runtime maturity that a compiler-sized program in Concrete is realistic

Without those, this is too early.

## Best Role For This Idea

The best role for QBE-in-Concrete is:

- long-horizon showcase
- self-hosting pressure program
- possible bootstrap/backend research track

not:

- immediate roadmap priority
- substitute for current flagship audit/evidence work

## Philosophy Check

This idea is philosophy-compatible only if it is pursued for:

- smaller trusted surfaces
- auditability
- explicit backend structure
- evidence-backed backend plurality

It becomes philosophy drift if it turns into:

- backend work as prestige engineering
- self-hosting for its own sake
- compiler complexity growth without clear user value

## Relationship To The Smaller Idea

The correct order is:

1. research QBE as an alternate backend target
2. implement `SSA -> QBE IL` if it earns the cost
3. evaluate code quality, determinism, maintenance, and usefulness
4. only then ask whether implementing QBE in Concrete is justified

Skipping directly to step 4 is not disciplined.

## Roadmap Fit

This does not belong in the near roadmap.

At most, it belongs as:

- long-horizon research
- possible later showcase or backend-plurality work

It should remain out of the active roadmap until the smaller QBE backend path proves itself.

## Bottom Line

Implementing QBE in Concrete is interesting because it could become a compact, auditable, self-hosting-adjacent backend story.

But it is a much larger bet than "add a QBE backend," and it should stay clearly separated from that smaller, more realistic experiment.

## Lean 4 Alternative And Generated Runtime (2026-10-18)

The [Sparkle/emitted-code analysis](sparkle-emitted-code-validation.md) adds a
second host-language option: implement a small QBE-inspired native backend in
Lean 4, then evaluate whether a later Concrete implementation buys useful
independence or dogfooding. R-0297 owns target-backend scope and R-0413 owns the
Concrete implementation comparison. The ordinary external-QBE integration
remains a separate project.

Lean 4 is the recommended first host for the correctness experiment because the
compiler already uses it, and instruction semantics, executable passes and
preservation theorems can share an environment. The required proofs still need
to be written. A Concrete port would additionally depend on establishing the
meaning of the Concrete program implementing that port.

### Compiler Memory Management Versus Generated Programs

[Lean uses reference counting](https://lean-lang.org/doc/reference/latest/Run-Time-Code/Reference-Counting/).
Its [runtime](https://lean-lang.org/doc/reference/latest/Run-Time-Code/) supports
the executable built from Lean code. Building a backend with Lean's C compilation
path therefore produces a compiler executable that uses Lean's runtime. That does
not put Lean runtime calls into assembly or machine code that this compiler emits.

```text
Build the compiler:
  Lean implementation -> generated C + Lean runtime -> backend executable

Compile a program:
  QBE-style/Concrete backend input -> backend executable -> target assembly/object
                                  -> assembler/linker -> native program
```

The native program needs only the runtime required by its own language and
explicit calls. Concrete's no-GC target model can remain unchanged. Compiler
allocation/reference-count overhead affects compilation performance and memory
use and should be measured independently. Lean compiler/runtime correctness
remains toolchain trust until the applicable artifact checks/proofs cover it.
Using this backend to compile Lean programs themselves would be a separate
problem with Lean's own runtime requirements.

### Small First Scope

Choose one target, pin its ISA/ABI/object format and begin with integer/boolean
functions, branches and simple stack-based allocation. Keep unsupported operations
explicit. Prove the lowering relative to target instruction semantics or validate
each emitted artifact; preserve required traps, values and calling conventions.
Add register allocation and optimization only after the simple path has useful
evidence. A full QBE parser/optimizer/target matrix is not required to establish
the first relation.

Assembly emission leaves the assembler trusted. Actual object-code assurance
needs encoding/decoding and relocation checks as well, followed separately by
linker/loader/runtime accounting. Compare against LLVM, external QBE and an
independent oracle; sampled agreement remains testing. This larger project must
not become a prerequisite for checking today's LLVM output or for Concrete's
first executable dependency integration.
