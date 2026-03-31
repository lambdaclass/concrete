<div align="center">
<img src="./logo.png" height="150" style="border-radius:20%">

# The Concrete Programming Language
[![CI](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml/badge.svg)](https://github.com/unbalancedparentheses/concrete2/actions/workflows/lean_action_ci.yml)
[![Telegram Chat][tg-badge]][tg-url]
[![license](https://img.shields.io/github/license/lambdaclass/concrete)](/LICENSE)

[tg-badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Ftg.sumanjay.workers.dev%2Fconcrete_proglang%2F&logo=telegram&label=chat&color=neon
[tg-url]: https://t.me/concrete_proglang

</div>

>Most ideas come from previous ideas - Alan C. Kay, The Early History Of Smalltalk

Concrete is a systems language where capability requirements, trust boundaries, and ownership are visible in every function signature — written in Lean 4 so the compiler itself can be a proof target.

## The Core Idea

In mainstream systems languages, function signatures usually do not tell you whether code may read files, use the network, or allocate memory. In Concrete, those effects are declared:

```con
fn parse_json(input: &String) with(Alloc) -> JsonValue { ... }
fn serve(port: u16) with(Network, Alloc, Console) -> Int { ... }
fn sha256(data: &Bytes) with(Alloc) -> String { ... }
```

`parse_json` can allocate but can't touch the filesystem or network. `sha256` can allocate but provably never reads a file. This isn't a convention — the compiler enforces it. A function can only call functions whose capabilities are a subset of its own.

The real payoff shows up in something like an artifact verifier: `sha256() with(Alloc)` cannot touch the filesystem. `read_file() with(File, Alloc)` cannot leak to the console. `report() with(Console)` cannot read files. An auditor can verify the authority boundaries from the signatures alone, without first reading the function bodies.

### The three-way trust split

Rust has one keyword: `unsafe`. Concrete splits this into three distinct things:

1. **Capabilities** (`with(File, Network)`) — semantic effects visible to callers. "This function does I/O."
2. **`trusted`** — containment of pointer-level tricks behind a safe API. Only permits pointer arithmetic, raw deref, raw assignment, pointer casts. Does NOT permit FFI calls, does NOT suppress capabilities, does NOT relax ownership.
3. **`with(Unsafe)`** — authority to cross foreign boundaries (FFI, transmute). Required even inside `trusted` code.

In Rust, when you see `unsafe`, you don't know if it's "pointer arithmetic I've proven correct" or "calling into C code that might do anything." In Concrete, those are syntactically and semantically different, and the compiler reports on them separately.

### The compiler as an audit machine

Concrete has 8 built-in report modes:

- `--report caps` — per-function capabilities
- `--report authority` — for each capability, which functions require it and through which call chain
- `--report unsafe` — where are the trust boundaries, extern functions, and unsafe crossings?
- `--report alloc` — where does allocation happen, and where is cleanup?
- `--report proof` — which functions are pure enough to be formally proved?
- `--report layout` — struct sizes, alignment, and field offsets
- `--report interface` — public API surface
- `--report mono` — which monomorphized generic code actually exists

These are structured compiler outputs derived from the same semantic analysis that type-checks the code. Not linting — real facts about the program.

## What Concrete Looks Like

Pure decision core — no capabilities needed, testable on its own:

```con
fn allow_command(cmd: Command, policy: Policy) -> Decision {
    if cmd.target > policy.max_target {
        return Decision#Deny;
    }
    if cmd.remote && policy.allow_remote == false {
        return Decision#Deny;
    }
    return Decision#Allow;
}
```

Trusted wrapper — pointer unsafety contained behind a safe API:

```con
trusted fn load_policy(path: &String) with(File, Alloc) -> Policy {
    let result: Result<String, FsError> = read_to_string(path);
    // ... parse the file contents into a Policy ...
}
```

Callers see `load_policy(path) with(File, Alloc)` — they know it reads files and allocates, but the raw pointer work is invisible to them. The `trusted` boundary is one declaration, not ambient privilege.

Explicit resource cleanup:

```con
fn process_request(stream: TcpStream, root: &String) with(Std) {
    let buf: [u8; 4096] = [0; 4096];
    let n: i64 = stream.read(&buf as *mut u8, 4096);
    // ... handle request ...
    stream.close();
}
```

Cleanup is written in the function body. No hidden destructors, no runtime finalization. Ownership is explicit and the compiler enforces linearity — forgetting to consume a resource is a compile error.

## The Proof Story

The compiler is written in Lean 4. This enables two layers of proof:

1. **Prove the compiler** — type soundness, ownership coherence, capability preservation, Core→SSA lowering correctness. This is proving the language rules are right.
2. **Prove user programs** — through formalized Core semantics, a Concrete function's behavior can be stated and proved as a Lean theorem. This means you could write a hash function in Concrete (real systems language, real FFI, real memory layout) and prove it correct in Lean (real theorem prover).

The architecture keeps proof tooling separate from compilation. The compiler produces stable artifacts (`ValidatedCore`, `ProofCore`); proof tools consume them. This avoids pushing proof search into the normal compile path.

Currently: 17 proven theorems over a pure Core fragment. The formalization is narrow but the architecture is designed to grow.

## Research Directions

The most developed ideas in the [research/](research/) directory are roadmap and research directions, not already-landed product surfaces:

**Authority budgets** — capabilities already tell you what each function requires. Budgets would make this enforceable at module and package scope: "this module may only use `Alloc`." If any function inside transitively reaches `Network`, the build would fail. The transitive capability set is already computed; budget checking is set containment. ([research/packages-tooling/authority-budgets.md](research/packages-tooling/authority-budgets.md))

**Allocation budgets** — `with(Alloc)` is currently binary. The proposal classifies functions as NoAlloc / Bounded / Unbounded by walking the call graph. This would push allocation reporting into a stricter audit surface. ([research/stdlib-runtime/allocation-budgets.md](research/stdlib-runtime/allocation-budgets.md))

**Execution cost tracking** — structural classification of functions as bounded or unbounded (loops, recursion, call depth). For bounded functions, compute abstract operation counts via IPET. Concrete is unusually tractable here: no dynamic dispatch, no closures, no hidden allocation. ([research/stdlib-runtime/execution-cost.md](research/stdlib-runtime/execution-cost.md))

**Semantic diff and trust drift** — diff trust-relevant properties across two versions: authority changes, new trusted boundaries, allocation shifts. Not source text diffs — semantic fact diffs. ([research/compiler/semantic-diff-and-trust-drift.md](research/compiler/semantic-diff-and-trust-drift.md))

**Proof addon architecture** — the compiler produces stable proof-facing artifacts; proof tooling is a separate consumer (SMT, symbolic execution, Lean export). This avoids fusing proof search into compilation. ([research/proof-evidence/proof-addon-architecture.md](research/proof-evidence/proof-addon-architecture.md))

**AI-assisted optimization** — the report system produces structured semantic facts (authority, allocation, purity, trust boundaries), not just pass/fail. This creates a tight feedback loop for automated agents: try a refactoring, check whether the compiler's structured report changed in the intended direction ("did this function become NoAlloc?", "did the authority set shrink?", "did more functions become proof-eligible?"). Semantic guardrails mean an agent can verify that a faster version didn't silently grow its trust surface.

## Where Concrete Fits

Concrete is not trying to replace Rust, Zig, or C for general-purpose systems programming. Its case is narrower: software that must be small, explicit, reviewable, and honest about power.

The most compelling targets are high-consequence components with narrow authority:

- boot, update, and artifact verification tools
- key-handling and cryptographic policy helpers
- safety/security guard processes with tightly bounded behavior
- industrial control safety interlocks, medical-device policy kernels
- audited wrappers around critical C libraries or hardware interfaces

**Compared to Rust:** Smaller surface, less abstraction, more explicit authority. Rust's `unsafe` covers everything Concrete splits into three checkable surfaces. Rust has no way to declare that a crate may not do network I/O.

**Compared to Zig:** Shares the low-level explicitness, but pushes harder on ownership, capability tracking, and proof-oriented structure.

**Compared to verification languages (F\*, SPARK):** Keeps low-level runtime, FFI, layout, and ownership first-class instead of treating them as escape hatches.

## Current State

The compiler implements the full pipeline: `Parse → Resolve → Check → Elab → CoreCheck → Mono → Lower → EmitSSA → LLVM IR`.

What exists: centralized ABI/layout, 8 report modes with 59 assertions, `trusted`/capability boundaries, a real stdlib (vec, string, io, bytes, fs, net, hash, time, parse, test, and 8 collection types), `#[test]` execution, `concrete build`/`test`/`run`, and 12 example programs that have pressure-tested parsers, interpreters, storage, networking, and integrity workloads.

What doesn't exist yet: incremental compilation, third-party dependencies, backend plurality, broad formalization, a runtime.

For priorities, see [ROADMAP.md](ROADMAP.md). For landed milestones, see [CHANGELOG.md](CHANGELOG.md).

## Try It

```bash
make build
.lake/build/bin/concrete examples/snippets/hello_world.con -o /tmp/hello && /tmp/hello
```

The compiler enforces linearity — forgetting or reusing a resource is a compile error:

```
struct Resource { value: Int }

fn consume(r: Resource) -> Int {
    return r.value;
}

fn main() -> Int {
    let r: Resource = Resource { value: 42 };
    let v: Int = consume(r);  // r is consumed here
    // Using r again: "linear variable 'r' used after move"
    // Forgetting r: "linear variable 'r' was never consumed"
    return v;
}
```

## Building

Requires [Lean 4](https://leanprover.github.io/lean4/doc/setup.html) (v4.28.0+) and clang.

```bash
make build    # or: lake build
make test     # runs the full test suite
make clean    # or: lake clean
```

## Doc Map

- [docs/IDENTITY.md](docs/IDENTITY.md) — project identity and vision
- [docs/SAFETY.md](docs/SAFETY.md) — the three-way trust model
- [ROADMAP.md](ROADMAP.md) — what's next
- [CHANGELOG.md](CHANGELOG.md) — what's landed
- [research/](research/) — design research and future directions
- [docs/](docs/README.md) — full documentation index

## License

Concrete was originally specified and created by Federico Carrone at LambdaClass.

[Apache 2.0](/LICENSE)
