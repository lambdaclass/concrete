# Predictable Execution

**Status:** Open

This note defines the research direction for Concrete as a language that can support a restricted, analyzable execution profile.

The goal is not to promise full cycle-accurate WCET analysis across arbitrary targets. The goal is to make execution uncertainty visible, shrink the set of dynamic behaviors that matter, and make stronger boundedness claims possible where the language and compiler structure justify them.

## Why This Fits Concrete

Concrete already has several properties that make predictable execution more realistic than in mainstream general-purpose systems languages:

- explicit capabilities instead of ambient authority
- explicit ownership and cleanup
- no hidden control flow
- no hidden allocation
- no closures or trait-object-style dynamic dispatch
- a compiler pipeline with explicit IR and reporting phases

That does not automatically give Concrete WCET guarantees. It does make boundedness, execution-shape reports, and restricted analyzable profiles much more plausible.

## What This Direction Means

Predictable execution in Concrete should mean:

1. the language can define a restricted analyzable profile
2. the compiler can report the main sources of execution uncertainty
3. some profile rules can be enforced rather than left as convention
4. concurrency and FFI are treated as explicit analysis boundaries
5. claims are staged: structural boundedness first, stronger timing claims only where justified

It should not mean:

1. promising exact runtime on arbitrary hardware
2. pretending hosted libc calls are timing-transparent
3. hiding difficult cases behind optimistic annotations
4. turning Concrete into a proof-only or real-time-only language

## The Main Research Questions

### 1. What is the analyzable profile?

Concrete needs a documented restricted profile that says what is allowed if a program wants predictable execution properties.

Likely ingredients:

1. no unrestricted heap allocation
2. no recursion
3. no loops without a known bound or accepted analysis boundary
4. no unrestricted FFI
5. no blocking operations in the first profile
6. no unstructured concurrency
7. no hidden runtime services outside the selected execution model

This should become a concrete profile, not a vague aspiration.

The first profile should be strict and compiler-enforced:

1. recursion is a compile error
2. call-graph cycles are a compile error
3. unknown-bound loops are a compile error unless the language later grows an accepted bound mechanism
4. unrestricted allocation is a compile error
5. unrestricted FFI is a compile error
6. blocking operations are a compile error
7. disallowed concurrency constructs are a compile error

### 2. What should the compiler report?

The compiler should expose execution-relevant uncertainty before it tries to prove deep timing facts.

Core report categories:

1. allocation present / absent / structurally bounded
2. recursion present / absent
3. loop bounds known / unknown
4. blocking operations present / absent
5. FFI boundary present / absent
6. concurrency boundary present / absent
7. stack-growth risks where structurally visible

The first win is visibility. Enforcement can follow.

### 3. What is enforceable?

Some predictable-execution properties are structurally enforceable:

1. `NoAlloc`
2. no recursion
3. no FFI
4. no blocking calls in selected profiles
5. restricted stdlib surface

Some are harder and may begin as report-only:

1. bounded loop iteration counts
2. bounded stack growth in the general case
3. precise transitive execution budgets
4. target-specific timing bounds

Concrete should separate these clearly.

There should likely be two stages:

1. a first profile with a simple `NoAlloc` rule
2. a later tighter bounded-allocation subprofile for code that can allocate, but only under structurally explainable limits

### 4. What is the concurrency story?

Predictable execution and unrestricted concurrency do not mix well.

Concrete needs a clear answer to:

1. whether the first analyzable profile is single-threaded only
2. whether later support should resemble a Ravenscar-style restricted concurrency model
3. which synchronization primitives are analyzable enough to allow
4. whether message passing should be fixed-capacity in the analyzable profile

This should be decided alongside the concurrency design, not afterward.

### 5. What is the FFI story?

FFI is a major analysis boundary.

For predictable execution, foreign calls should be treated as one of:

1. forbidden in the restricted profile
2. allowed only through audited wrappers with explicit classification
3. treated as unknown timing / unknown boundedness unless proven otherwise

Concrete should not pretend foreign calls are analyzable just because the language surface is explicit.

### 6. What can be claimed without hardware models?

Concrete can likely support:

1. structural boundedness reports
2. abstract instruction-cost estimates
3. enforcement of selected restricted-profile rules

Concrete should not claim cycle-accurate WCET without:

1. target-specific backend assumptions
2. hardware timing models
3. validation against actual emitted code and target behavior

That separation is essential for credibility.

## Recommended Research Sequence

1. define the restricted analyzable profile in source-language terms
2. extend reports to surface execution uncertainty directly
3. implement enforceable subsets such as `NoAlloc`, no recursion, and restricted FFI
4. decide whether analyzable concurrency starts as "none" or as a restricted structured subset
5. add bounded examples that validate the profile against real code
6. only then investigate stronger timing/cost estimation

## Candidate Validation Examples

The profile should be tested on small but meaningful examples. Each example should exercise a different subset of profile rules so the full surface is covered.

| Example | NoAlloc | No recursion | Bounded loops | No FFI | No blocking | Single-threaded | Fixed-capacity types |
|---|---|---|---|---|---|---|---|
| Fixed-buffer parser | yes | yes | yes (input length) | yes | yes | yes | yes |
| Bounded-state controller | yes | yes | yes (state transitions) | yes | yes | yes | no |
| Ring buffer | yes | yes | yes (capacity) | yes | yes | yes | yes |
| Packet decoder | yes | yes | yes (packet length) | yes | yes | yes | yes |

Later, the bounded-allocation subprofile should be tested on examples that allocate under structurally explainable limits:

| Example | Bounded alloc | Key constraint |
|---|---|---|
| Fixed-capacity map builder | yes | capacity from construction |
| Bounded message processor | yes | allocation proportional to fixed channel depth |

These should validate whether the profile is realistic, not just elegant on paper.

## Relationship To Other Notes

This note is the umbrella for the predictable-execution direction.

Related work:

1. [../stdlib-runtime/execution-cost.md](../stdlib-runtime/execution-cost.md)
2. [../stdlib-runtime/allocation-budgets.md](../stdlib-runtime/allocation-budgets.md)
3. [../stdlib-runtime/concurrency.md](../stdlib-runtime/concurrency.md)
4. [../stdlib-runtime/long-term-concurrency.md](../stdlib-runtime/long-term-concurrency.md)

Supporting notes:

1. [bounded-loops.md](bounded-loops.md)
2. [analyzable-concurrency.md](analyzable-concurrency.md)
3. [ffi-boundaries.md](ffi-boundaries.md)
4. [blocking-effects.md](blocking-effects.md)
5. [fixed-capacity-stdlib.md](fixed-capacity-stdlib.md)
6. [stack-bounds.md](stack-bounds.md)

## Bottom Line

Concrete is closer in spirit to analyzable high-integrity languages than to general-purpose systems languages, but that direction needs to be made explicit and disciplined.

The next step is not "promise WCET." The next step is:

1. define the analyzable profile
2. report the reasons a program falls outside it
3. enforce the structurally checkable parts
4. validate the model on real bounded examples
