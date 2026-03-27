# Implementing QBE In Concrete

Status: research

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
