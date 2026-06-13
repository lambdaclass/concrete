# Capability Hardening And Sandboxing

**Status:** Open

This note collects ideas for making Concrete's capability system better at sandboxing code, especially low-level code that should be auditable, reviewable, and mechanically restricted.

The current best direction now assumes a related three-way split:

- semantic effects are expressed as capabilities
- implementation trust is expressed by `trusted fn` / `trusted impl`
- foreign semantic danger remains under `with(Unsafe)`

The important consequence is that sandboxing is not only about user-written code. It also has to apply coherently across:

- builtins
- stdlib internals
- ordinary user code

Concrete already has an unusually strong base:

- explicit `with(...)` capability declarations
- no user-defined ambient effects
- explicit `Unsafe`
- explicit allocation
- audit-friendly function signatures

The question here is not whether Concrete needs hidden or more abstract effects. It is whether the existing capability model can become better at expressing and enforcing *restricted authority*.

## Goals

Any improvement in this area should preserve:

- explicit source syntax
- grep-ability
- auditability
- proof-friendliness
- low cognitive load for ordinary systems code

The target is not “a more powerful effect system” in the abstract.
The target is:

- easier sandboxing
- clearer authority boundaries
- better auditing
- stronger machine-checkable restrictions

## What "Better Sandboxing" Means

Concrete should eventually make it easier to say things like:

- this parser can allocate but cannot touch the network
- this subsystem can read files but cannot spawn processes
- this module can connect outbound but cannot listen on sockets
- this utility can format and hash data but cannot block on host I/O
- this code can use `Unsafe` only through a narrowly scoped wrapper

Today some of this is already expressible through broad capabilities. The research question is how much further to push that without making signatures noisy or theory-heavy.

## High-Leverage Ideas

### 1. Better compiler capability reports

Before changing source syntax, make the compiler better at exposing the capability graph.

Useful reports:

- direct capabilities used by a function
- transitive capabilities required through callees
- per-module capability summaries
- call-chain explanations for *why* a capability is required

Example:

```text
fn serve:
  direct: Network
  transitive: Network, Alloc, File

why File?
  serve -> log_request -> append_file
```

Why this is high leverage:

- strengthens sandboxing without changing the language
- improves code review and security auditing immediately
- makes coarse capability sets more understandable before splitting them further

This is one of the highest-value security features Concrete could have. A reviewer should be able to ask not just “does this function require `Network`?”, but “why does it require `Network`, through which path, and what narrower authority wrapper could remove that requirement?”

This is the safest and highest-value first step.

### 2. Capability aliases

Allow named bundles of existing capabilities.

Example:

```con
cap IO = File + Console;
cap Host = File + Network + Env + Process;
```

Then:

```con
fn run() with(IO) -> Int { ... }
```

Why this is good:

- reduces repetitive signatures
- keeps capabilities explicit
- preserves auditability
- improves readability without changing semantics

This is one of the safest source-language upgrades.

### 3. Authority budgets for modules, packages, and binaries

One of the strongest future extensions would be to let larger program units declare not just what they *currently require*, but what they are *allowed to require at all*.

Example shape:

- package `logger` may use only `{File, Alloc}`
- package `config` may use only `{FileRead, Alloc}`
- binary `worker` may not require `Network` or `Process`

Then if a dependency or subsystem grows new authority:

- the compiler or package tooling reports it
- the build can fail if the declared budget is exceeded

Why this is powerful:

- it turns capability checking into a project-level control mechanism
- it helps prevent authority creep in dependencies
- it gives reviewers a simple least-authority story at the subsystem or package level
- it fits naturally with Concrete's audit/report identity

This becomes especially compelling once Concrete has a real package/dependency model. Capabilities then stop being only function-level facts and become enforceable package-level boundaries too.

### 4. Finer-grained capabilities

Some current capabilities may be too broad to express useful sandbox boundaries.

Promising later splits:

- `FileRead` / `FileWrite`
- `NetConnect` / `NetListen`
- `ConsoleIn` / `ConsoleOut`
- `ProcessSpawn` / `ProcessKill` / `ProcessExit`
- perhaps `Alloc` / `HeapAlloc` if stack-only distinctions become important

Benefits:

- tighter review boundaries
- clearer least-authority APIs
- more useful audit output

Risk:

- signatures become noisy quickly
- programmers stop reading capability sets if they become too fine-grained

So this should be introduced only where the split buys real operational value.

### 4a. Capability vocabulary additions (decided 2026-06-13)

Forward-looking decisions on the capability *vocabulary* (distinct from the
granularity splits above). The guiding lesson — reinforced by the H1 closure
(`docs/KNOWN_HOLES.md`: smaller, sharper primitives win) — is to add capabilities
only when they name a *distinct authority* a real workload needs, and to keep
each one sharp.

- **`Device` — planned, deferred to the freestanding path (NOT added yet).**
  `with(Device)` (with `with(Mmio)` as a possible companion) for memory-mapped
  I/O / volatile hardware access. When added it will be a registered capability
  **not** in `Std` — opt-in and audit-visible like `Unsafe`, so hardware access
  is named distinctly rather than hiding under generic `Unsafe`. Deferred until
  there is a real consumer: it lands with the freestanding/embedded work (ROADMAP
  Phase 15), not speculatively — adding a capability with no consumer is exactly
  the premature vocabulary growth this note warns against.
- **`Thread` — future concurrency vocabulary.** Reserve `with(Thread)` for the
  structured-concurrency direction (alongside the existing `Async`/`Concurrent`
  lattice nodes). Not added now — it lands with the concurrency model, not
  before, so it is designed against real `spawn`/scope APIs
  (`capability-polymorphism.md`, `async-concurrency-evidence.md`).
- **`Random` — clarified, not changed.** `Random` is authority over **external
  entropy** (the OS/hardware RNG — a genuine nondeterminism source), not
  permission to run a pure PRNG. A deterministic PRNG seeded from a value is pure
  and needs no capability; only drawing real entropy requires `with(Random)`.
  This keeps the predictable/`ProvableV1` boundary honest. (Clarify in
  `docs/SAFETY.md`.)
- **`Signal` — deferred.** OS signal handling (`with(Signal)`) is deferred: its
  control-flow/async-safety semantics are subtle and no current workload needs
  it. Revisit only with concrete evidence.

### 5. Capability-aware stdlib design

A better sandboxing story is not only syntax. It is also library discipline.

The stdlib should make authority boundaries obvious:

- `fs` APIs require file authority
- `net` APIs require network authority
- `process` APIs require process authority
- allocation remains visible
- helper APIs do not accidentally widen authority

This may matter more than adding any new capability syntax.

### 6. Blocking vs non-blocking as explicit authority

This is one of the most promising future directions.

Instead of treating all I/O-ish code as one category, Concrete could eventually track distinctions such as:

- may block
- may spawn
- may wait
- may sleep
- may perform host I/O

Why it matters:

- much better concurrency/runtime auditing
- easier reasoning about scheduler-sensitive code
- useful for embedded, realtime, and service isolation work

This should be developed together with the concurrency design, not bolted onto the stdlib ad hoc.

### 6. Capability-based runtime access

Runtime services should not become ambient.

Examples of authority that should remain explicit:

- spawning tasks
- sleeping/timers
- networking
- file I/O
- synchronization

The key idea:

- libraries depend on explicit capabilities, not on an assumed global runtime

This avoids Rust-style runtime fragmentation and hidden runtime coupling.

### 7. Local capability narrowing

Potentially useful later:

```con
with(File) {
  ...
}
```

or some equivalent that narrows authority inside a scope.

Possible value:

- smaller review regions
- local sandboxing within a larger effectful function

Risk:

- adds syntax with little gain if reports already solve most audit needs
- can complicate proofs if not designed carefully

This should be treated as research, not as an obvious feature.

### 8. Explicit authority wrappers

Some boundaries may be better modeled as ordinary values than as global function capabilities.

Examples:

- an owned file-system handle object that only exposes read operations
- a network client wrapper that can connect but not listen
- a formatter/buffer object with no host effects at all

This works well with Concrete's explicit ownership style and may be a better sandboxing tool than continually refining the effect syntax.

This is especially promising for:

- read-only vs write-capable file handles
- connect-only vs listen-capable networking wrappers
- append-only logging handles
- authority-restricted host interfaces passed into subsystems

### 9. Better `Unsafe` inspection and narrowing

`Unsafe` is currently broad by design. Over time, Concrete may want better ways to narrow *how* unsafe authority is used.

Promising directions:

- compiler reports of where `Unsafe` is required and why
- wrapper patterns that contain raw-pointer or FFI authority
- the `trusted fn` / `trusted impl` boundary for containing pointer-level implementation unsafety behind safe APIs (see [trusted-boundary.md](trusted-boundary.md))
- maybe later splitting `Unsafe` conceptually into subcategories for reports, even if the source language still uses one `Unsafe`

The three-way split described in [trusted-boundary.md](trusted-boundary.md) is directly relevant here:

- **`with(Alloc)` and other capabilities** = semantic effects visible to callers
- **`trusted`** = containment of internal pointer-level implementation techniques (raw ptr deref, assignment, arithmetic, casts) behind a safe API
- **`with(Unsafe)`** = authority to cross foreign boundaries (FFI, transmute) — required even inside `trusted` code

This should apply coherently across builtins, stdlib code, and user code. The sandboxing model gets weaker if any one of those layers is silently exempt.

The important rule:

- keep the user-facing story simple before making the type/effect story more detailed

### 10. Hosted vs freestanding support as sandboxing leverage

A later hosted vs freestanding / `no_std` split would strengthen sandboxing at the platform boundary too.

Why it matters:

- code can be compiled without hosted modules (`fs`, `env`, `process`, `net`)
- host access stops being assumed by the environment
- restricted targets get a smaller default authority surface

This should be treated as a later runtime/library-boundary milestone, not an immediate language-syntax feature.

See [no-std-freestanding.md](no-std-freestanding.md).

## Things To Avoid

Concrete should avoid “improving” sandboxing in ways that make the language harder to audit.

Specifically avoid:

- implicit effects
- inference-heavy effect syntax
- giant effect rows or unreadable algebraic notation in ordinary code
- too-fine-grained capabilities too early
- runtime assumptions hidden in library APIs
- effect systems that become easier for researchers than for working programmers

## Recommended Order

If Concrete wants better sandboxing, the best order is:

1. better capability reports
2. capability aliases
3. keep stdlib authority boundaries explicit and consistent
4. evaluate whether current capabilities are too coarse
5. develop blocking/non-blocking/runtime authority together with the concurrency design
6. add explicit authority-wrapper patterns where they help
7. only then consider local narrowing or finer-grained authority syntax
8. later, use hosted vs freestanding support to strengthen the sandbox boundary further

This keeps the model practical and readable.

## Device Test Plan

When `Device` is wired into the compiler vocabulary, it should come with a
small permanent gate rather than only a documentation change.

Future gate: `scripts/tests/check_device_capability.sh`.

It should prove:

- `with(Device)` parses, typechecks, appears in `--report caps`, and appears in
  JSON capability facts.
- `Device` is **not** part of `Std`; `with(Std)` must not authorize MMIO/device
  helpers.
- `Unsafe` does not imply `Device`, and `Device` does not imply `Unsafe`.
  Hardware-facing raw-pointer helpers should normally require both:
  `with(Device, Unsafe)`.
- a pure or hosted-safe module can forbid device access with an import/package
  constraint such as `requires(no Device)` or `forbidden = ["Device"]`.
- a dependency that widens from no-device to `Device` breaks the importing
  package until explicitly accepted.
- freestanding/MMIO examples report `Device` separately from generic pointer
  unsafety, so audit output can answer "which functions touch hardware?" without
  treating every `Unsafe` wrapper as hardware authority.

The first implementation should prefer one capability, `Device`, over a
premature split into `Mmio`, `Gpio`, `Serial`, and similar names. If real
embedded workloads later need finer review boundaries, those should be split
from `Device` with evidence and their own gates.

## Relationship To Other Notes

- See [concurrency.md](concurrency.md) for the runtime/concurrency side of blocking, spawning, cancellation, and structured concurrency.
- See [external-ideas.md](external-ideas.md) for broader language ideas that may influence capability design indirectly.
- If these ideas become stable language or compiler rules, they should move into `docs/`, not stay in research.
