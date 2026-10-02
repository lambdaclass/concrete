# Predictable/Proved Code Boundaries

Status: reference

This document classifies the runtime boundaries of Concrete code that passes the predictable profile and/or has Lean-backed proofs. It answers: what can happen at runtime, what is ruled out, and what is assumed.

For the general execution model, see [EXECUTION_MODEL.md](../language/EXECUTION_MODEL.md).
For profile definitions and gates, see [PROFILES.md](PROFILES.md).
For the trusted computing base, see [TRUSTED_COMPUTING_BASE.md](../verification/TRUSTED_COMPUTING_BASE.md).
For memory safety guarantees, see [MEMORY_GUARANTEES.md](../language/MEMORY_GUARANTEES.md).

---

## Scope

This classification applies to functions that pass `--check predictable` (all five gates: no recursion, bounded loops, no allocation, no FFI, no blocking I/O). Functions that additionally have Lean-backed proofs (`evidence: proved`) have a further layer described under "Proved functions" below.

The classification is **source-level**. It describes what the compiler enforces and reports about the source program. It does **not** make claims about binary-level timing, LLVM optimization effects, or hardware behavior.

---

## 1. Host Calls

### Reachable from predictable code

Predictable functions cannot allocate, call FFI, or use blocking I/O capabilities. The host calls reachable from predictable code are:

| Host call | Reachable? | Condition | Notes |
|-----------|-----------|-----------|-------|
| `malloc` | No | Blocked by no-allocation gate | Alloc capability or alloc/vec_new intrinsics |
| `free` | No | Blocked by no-allocation gate | |
| `realloc` | No | Blocked by no-allocation gate | |
| `abort` | No | `abort` requires Process capability | Blocked by no-blocking gate |
| `printf` | Only from `main` wrapper | Compiler-emitted, not user code | `main` wrapper prints return value |
| `write` | Via Console capability | Console is allowed in predictable | `print`/`println` use write(2) |
| `strlen` | No | String operations imply Alloc | |
| `memcpy` | No | Vec/String operations imply Alloc | |
| `memset` | No | Vec pop implies Alloc | |
| `memcmp` | No | String equality implies Alloc | |
| `snprintf` | No | int_to_string implies Alloc | |
| `strtol` | No | string_to_int implies Alloc | |
| File/Network/Process syscalls | No | Blocked by no-blocking gate | |

**Summary**: Predictable functions may call `write` (Console print) and pure arithmetic/logic. All heap, string, Vec, FFI, and blocking operations are excluded by the five gates.

### Compiler-emitted code in predictable functions

| Emitted construct | Present? | Notes |
|-------------------|----------|-------|
| `alloca` (stack allocation) | Yes | Local variables, temporaries, aggregates |
| `getelementptr` (field/array access) | Yes | Struct field access, array indexing |
| `call` to user functions | Yes | Bounded call depth (acyclic, no recursion) |
| `call` to `__concrete_check_oom` | No | Only emitted for malloc results |
| Loop constructs | Yes | All loops bounded by predictable gate |
| `br` / `switch` (branching) | Yes | if/else, match |

---

## 2. Cleanup Paths

### Defer behavior

Predictable functions may use `defer`. Deferred expressions execute:

- **On normal return**: LIFO order, innermost scope first
- **On scope exit**: before control leaves the scope block
- **On break/continue**: deferred calls for exited scopes, stopping at loop/function boundary

Deferred expressions do **not** execute on:

- **abort()**: not reachable from predictable code (requires Process capability)
- **Hardware signals (SIGSEGV)**: process terminates immediately; defer is not a signal handler
- **OOM**: not reachable from predictable code (no allocation)

### Resource cleanup

Predictable functions do not allocate heap memory, so there is no heap resource to leak. All data is on the stack. When the function returns, the stack frame is reclaimed by the caller.

The only resources predictable code may hold are file descriptors from Console (stdout/stderr), which are process-global and not closed by user code.

---

## 3. Determinism Sources

### What is deterministic (compiler-enforced)

| Property | Guarantee level | Mechanism |
|----------|----------------|-----------|
| No recursion | Source-level | Call graph SCC analysis; an **indirect call is refused**, see below |
| Bounded iteration | Source-level | Loop bound classification (see **What "bounded" means** below) |
| No allocation | Source-level | Alloc capability + intrinsic detection |
| No FFI | Source-level | Extern function detection |
| No blocking I/O | Source-level | Capability gate (File, Network, Process) |
| Acyclic call graph | Source-level | SCC analysis + recursion gate |
| Bounded stack depth | Source-level | `--report stack-depth` (item 25), not gated; a bound is claimed **only where establishable**, see below |
| Report/IR determinism | Compiler | No HashMap in output paths, monotonic counters |

### What "bounded" means (and what it does not)

A loop is classified `bounded` only when a variable appearing in its condition is stepped
**toward** the bound by a constant:

```
for (let mut i = 0; i < n; i = i + 1)     bounded    -- i increases toward n
for (let mut i = n; i > 0; i = i - 1)     bounded    -- i decreases toward 0
```

Everything else is `unbounded`, and the `predictable` profile rejects it. That includes cases
this analysis cannot certify rather than ones it knows to diverge:

```
for (let mut i = 0; i < n; z = z + 1)     unbounded  -- step touches an unrelated variable
for (let mut i = 0; i < n; i = i - 1)     unbounded  -- step moves AWAY from the bound
for (let mut i = 0; i < n; i = i + k)     unbounded  -- k is a variable; could be 0 or negative
while (i != n)                            unbounded  -- terminates only for a start value
                                                        this analysis cannot see
```

**This was previously much weaker, and wrong.** The rule asked only "is the condition a
comparison?" and "is the step list non-empty?", with nothing connecting them. The first two
examples above were classified `bounded`, `--check predictable` **passed** a module containing
them, and the effects report said `0 unbounded loops` — for code that runs forever.

Two things about that are worth keeping in mind for the rest of this document:

- **It was a liveness claim, so no safety obligation could have caught it.** Every proof
  obligation in the pipeline rules out a *bad event* (overflow, out-of-bounds). A program that
  hangs performs no bad event; it just never finishes. Nothing downstream of the classifier was
  capable of noticing.
- **The tightening changed nothing in the corpus.** Per-function classification across every
  example with a loop is byte-identical before and after — all 31 previously-`bounded` loops use
  the `i = i + 1` idiom the new rule accepts. The defect survived because no fixture had a
  fake-bounded loop, not because the rule was load-bearing.

What is still NOT established: that a `bounded` loop terminates within any particular *number*
of iterations, or that its bound relates to anything else in the program. `i < n` with
`i = i + 1` terminates for every fixed `n`, and that is the whole of the claim. Bounding
iteration COUNTS would need the measure supplied and proved, which is termination proof proper
and is not attempted here.

### Indirect calls cannot be certified acyclic

The call graph records only DIRECT callees. That is deliberate and right for extraction — an
indirect callee is a fn-typed binding whose statically-known target set is empty. But two
guarantees were built on the same graph without revisiting that choice:

```
fn apply(f: fn(i32) -> i32, x: i32) -> i32 { return f(x); }
fn ping(x: i32) -> i32 {
    if x <= 0 { return 0; }
    return apply(ping, x - 1);      // recursion, through a function pointer
}
```

There is no edge for `f(x)`, so SCC found no cycle. `ping` reported `recursion: none`,
`--check predictable` **passed** the module, and `--report stack-depth` stated
`depth: 1, stack: 32 bytes` with `Max stack bound: 32 bytes` — a byte-exact figure for a
function that recurses to an arbitrary depth. **A false number is worse than a missing one,
because it is quotable.**

Now: a body containing an indirect call is refused by the profile, and gets no stack bound.
Resolving the target set — proving every call site of a combinator passes a known function — is
a whole-program analysis and a real project. Assuming the set is empty is not a conservative
approximation of that; it is the opposite.

Unboundedness also **propagates to callers** now. `computeCallDepths` filtered recursive callees
out of each chain, so a function calling an unbounded one still received a finite bound; the
edge was dropped rather than the answer declined.

### Transitivity, gate by gate

The five gates did not agree on this, and the disagreement was not deliberate.

| Gate | Transitive? | Mechanism |
|------|-------------|-----------|
| No allocation | **Yes** | `E0520: requires Alloc but caller has (none)` — a caller must declare the capability, so the effects report sees it on the signature |
| No blocking I/O | **Yes** | same, for `File` / `Network` / `Process` |
| No FFI | **Yes, as of 2026-08-05** | closed over the call graph; previously a direct-callee check only |
| No recursion | Per-function | see the gap below |
| Bounded iteration | Per-function by nature | a loop is in one body |

Alloc and blocking get transitivity for free from the capability system, and it cannot be
laundered through a combinator either: a function-pointer type carries its capset, so passing an
`with(Alloc)` function where a pure `fn(i32) -> i32` is expected is an `E0220` type error.

FFI had no capability behind it. `f` calling `g` calling an extern reported `ffi: no` and
`evidence: enforced` while transitively crossing FFI. Closing it over the call graph reclassified
three real examples (`http`, `integrity`, `verify`, 1 → 3 FFI-crossing functions each) and all
were true positives — in `http` the chain is `send_string` → `handle_client` → `main`.

One imprecision worth stating: the call graph resolves callees to qualified names while the
effects report compares raw ones, so the closed set carries both the qualified name and its final
component. A same-named function in another module can therefore be over-flagged. That direction
refuses a function rather than admitting one, which is the side to err on for an admission gate.

### Admission is transitive (all five gates, as of 2026-08-05)

"Predictable execution" is a property of RUNNING a function. If `f` calls `g`, running `f` runs
`g`, so `f` can only be predictable if `g` is. The label used to be computed from each body in
isolation, so this was `enforced`:

```
fn fib(n: Int)  -> Int { if n <= 1 { return n; } return fib(n - 1) + fib(n - 2); }
fn main() -> Int { return fib(10); }          // enforced — contained no recursion textually
```

`main` contains no recursion in its own body and unbounded recursion in practice.

| Gate | Mechanism |
|------|-----------|
| No allocation | capability: `E0520: requires Alloc but caller has (none)` |
| No blocking I/O | capability, same |
| No FFI | call-graph closure |
| **No recursion** | **call-graph closure** — direct, mutual, and *reaching* either |
| Bounded iteration | per body; a loop lives in one function |

Recursion was the last gate asking "is it written in your body?" rather than "can you reach
it?", and the difference was never designed — it is what you get when one property is enforced
by types and another by a graph walk. A body containing an **indirect** call seeds the closure
too: with no edge for `f(x)` the callee set is unknown, so acyclicity is not established there
either, and callers inherit that.

**Measured impact:** 14 functions across 10 examples moved from `enforced` to `reported`, and
they are true positives — mostly a `main` calling a recursive `fib`/`factorial`/`gcd`, plus
`lox` and `toml` (3 each). Four test goldens were counting the old behaviour and now assert the
new one. Nothing stops compiling: this is an admission and label change, not a type error, and
module-level `--check predictable` already failed these programs because the recursive function
itself failed. What was fooled was anyone reading the per-function evidence level.

**One consumer was silently left behind, twice over.** There are seven call sites of the
predictable check, including `predictableQuery`, `evidenceQuery`, `auditQuery`, and the policy
path in `Check/Policy.lean` that REJECTS builds. When FFI was closed over the call graph, three
of them kept passing the unclosed set — including the policy path — so the gate that actually
rejects a build was the one still on the direct-callee check. `Report.profileClosures` now
returns both closed sets together, so a consumer cannot obtain one without the other.

### Two properties, deliberately different — and they must not share a phrase

Making admission transitive changed what "passes the predictable profile" means. `ProofCore`
computes its own version of that same-named property to decide **proof eligibility**, and was
left alone on purpose. The result was two commands printing opposite claims about one function:

```
--check predictable    caller — reaches a function whose recursion cannot be ruled out
--report proof-status   `caller` passes the predictable profile
```

The logic was right in both places. The **phrase** was doing two jobs.

| | asks | shape |
|---|---|---|
| **Admission** (`--check predictable`) | is this function's execution predictable? | transitive — running `f` runs `g` |
| **Proof eligibility** (`--report proof-status`) | can this body's obligations be proved? | per-body |

**Eligibility must not become transitive.** `caller`'s own overflow obligation is provable
whether or not `recurses` terminates: overflow is a safety property (a bad event that never
happens), non-termination is a liveness one. Making eligibility transitive would refuse a
provable obligation for no soundness gain — it would destroy proofs to tidy a name.

So the fix was vocabulary, not logic: the proof surface now says "eligible for proof" /
"fails the proof-eligibility gates". The reasons field it builds from was called `profileGates`
while holding things like `is entry point (main)` and `has capabilities: Console` — which are not
profile gates at all — and is now `eligibilityReasons`. A gate asserts the two surfaces may
disagree about a function and that neither speaks in the other's vocabulary.

**Still not transitive, deliberately:** `proofReport`'s extraction eligibility
(`proofExclusionReasons`). That answers a different question — can this body be extracted to
Gallina — and changing it would affect which proofs are attempted, so it is a separate decision
rather than part of this one.

### Planned unified resource certificate

Task R-0445 will pull a composite source-resource certificate forward before
broad stdlib expansion. It will report a constant or symbolic upper bound—or a
specific `unknown` reason—for source-model steps, call depth, source-model stack,
allocation count/bytes, and blocking/opaque boundaries. The parser-combinator
workloads are its first forcing corpus.

This is planned work, not a current command or guarantee. Its facts will carry
`semantic_scope = source`, assumptions, completeness, and provenance. They will
also name the versioned source cost/layout model. They will not claim elapsed
time, cycles, backend frame size, or hardware WCET; later tasks own those
bridges and measurements.

### What is NOT deterministic (outside compiler scope)

| Property | Why not | Mitigation |
|----------|---------|-----------|
| Execution timing | LLVM optimization, CPU microarchitecture, OS scheduling | Not claimed |
| Binary layout | LLVM register allocation, instruction scheduling | Not claimed |
| Stack addresses | ASLR, OS stack allocation | Not claimed |
| Cache behavior | Hardware-dependent | Not claimed |
| Console output timing | OS write(2) buffering | Not claimed |

**Key point**: "Predictable" means source-level execution shape is bounded and reviewable. It does **not** mean constant-time, timing-deterministic, or binary-reproducible.

---

## 4. Failure Paths

### Failures reachable from predictable code

| Failure mode | Reachable? | Condition | Consequence |
|-------------|-----------|-----------|-------------|
| Explicit error return | Yes | `Result<T, E>` propagation | Normal control flow, caller handles |
| OOM (malloc null) | No | No allocation in predictable code | |
| Null pointer deref | Only in trusted | Safe code has no raw pointers | SIGSEGV if triggered |
| Integer overflow | Trap (abort) | Ordinary `+ - *` are checked (ROADMAP #10 Stage 2.3) | Aborts on overflow; `wrapping_*` for intentional modular |
| Array out-of-bounds | Trap (abort) | Safe `arr[i]` is runtime-checked | See below |
| Stack overflow | Possible | Deep (but bounded) call chains | OS guard page → SIGSEGV |
| abort() | No | Requires Process capability | |

### Array bounds checking

Safe array indexing (`arr[i]`) inserts a runtime check and aborts when the index
is negative or outside the declared length. Constant OOB is rejected earlier,
and a statically discharged in-range obligation may remove the redundant check.
Trusted/explicitly unchecked pointer access remains outside this guarantee.

### Integer overflow

Ordinary `+ - *` on fixed-width integers are **checked** (ROADMAP #10 Stage 2.3): on
signed or unsigned overflow they **trap (abort)**, in every profile. Intentional
modular arithmetic is the explicit `wrapping_*`; clamping is `saturating_*`. This
narrows the proof-vs-runtime integer gap: a proof over unbounded integers and the
checked runtime can only disagree by the runtime *aborting* — never by silently
producing a wrong (wrapped) value. Division/modulo zero and signed `MIN / -1`,
invalid shifts, and checked negation follow the same defined trap policy. See
ARITHMETIC_POLICY.md and PROOF_SEMANTICS_BOUNDARY.md.

---

## 5. Memory and UB Boundaries

### Safe code guarantees (enforced by checker)

| Guarantee | Mechanism | Applies to predictable? |
|-----------|-----------|------------------------|
| No use-after-move | VarState tracking in Check.lean | Yes |
| No borrow conflict | Exclusive mutable borrow enforcement | Yes |
| No borrow escape | Escape analysis | Yes |
| No linear value leak | Scope-exit consumption check | Yes |
| No reassignment of consumed linear | State machine | Yes |
| Branch agreement on ownership | Both arms must agree | Yes |

### UB still possible in predictable code

| Source of UB | How | Why not prevented |
|-------------|-----|-------------------|
| Trusted unchecked pointer/index access | invalid raw address or index | trusted/Unsafe code is outside the safe guarantee |
| Dishonest extern contract | ABI/type/lifetime mismatch | foreign implementation is an explicit assumption |

Ordinary integer overflow, division/modulo by zero, invalid shifts, checked
negation, and safe array OOB are defined aborts, not UB.

### UB NOT possible in safe predictable code

| Excluded UB | Why |
|-------------|-----|
| Use-after-free | No allocation, no free |
| Double free | No allocation |
| Null pointer deref | No raw pointers in safe code |
| Dangling reference | Borrow checker prevents escape |
| Data race | Single-threaded execution model |
| Uninitialized memory | All variables initialized at declaration |
| Type confusion | Static type system, no casts in safe code (except numeric) |

### Trusted functions in predictable code

Functions marked `trusted fn` bypass the checker. A predictable program may call trusted functions — they pass the five gates (no recursion, bounded loops, no allocation, no FFI, no blocking) but their bodies are not ownership-checked.

`--report effects` classifies trusted functions as `evidence: trusted-assumption`. This means the compiler verifies the function's signature and gate properties but trusts the implementation.

---

## 6. Proved Functions — Additional Boundaries

Functions with `evidence: proved` have all predictable properties plus a Lean-verified theorem about their PExpr representation.

### What "proved" adds

| Property | Mechanism |
|----------|-----------|
| Stated theorem holds over PExpr | Lean 4 kernel checking |
| Fingerprint matches current body | Stale detection via structural fingerprint |
| Function has no external authority (empty `with(...)`, no effects reachable through a handle or foreign binding) | Proof eligibility gate |
| Only admitted state/loop forms | ProofCore extraction and boundedness gates |

### What "proved" does NOT add

| Gap | Reason |
|-----|--------|
| Binary correctness | Proof is over PExpr, not compiled LLVM IR |
| Integer semantics match | PExpr uses unbounded integers; binary uses fixed-width |
| Composition | Per-function proofs, no cross-function theorem |
| All properties covered | Only the stated theorem; other properties unchecked |
| Backend faithfulness | Extraction pipeline not formally verified |
| Full proof-subject freshness | Current body fingerprint omits some signature/type, contract, and dependency facts (R-0004) |

A source proof link with no stored fingerprint is `unbound`, not proved. Kernel
replay and attachment freshness are separately visible evidence.

### Proof ≠ predictable

Proof eligibility is stricter than predictable because it is authority-free and
limited to the modeled ProofCore surface. Selected bounded loops and functional
state updates are admitted; many predictable constructs are still not
extractable.

---

## 7. Profile Interaction Summary

| Property | Safe | Predictable | Proved |
|----------|------|-------------|--------|
| Ownership enforcement | Yes | Yes (inherited) | Yes (inherited) |
| No recursion | No | Yes | Yes (inherited) |
| Bounded loops | No | Yes | Selected modeled forms |
| No allocation | No | Yes | Yes (no capabilities) |
| No FFI | No | Yes | Yes (inherited) |
| No blocking I/O | No | Yes | Yes (no capabilities) |
| Stack bound computable | No (may recurse) | Yes (`--report stack-depth`) | Yes (inherited) |
| Lean theorem verified | No | No | Yes |
| Checked arithmetic trap possible | Yes | Yes | Yes unless discharged/modeled |
| Array OOB trap possible | Yes | Yes | Yes for admitted array forms unless discharged |

---

## 8. Verification Commands

| Question | Command |
|----------|---------|
| Does this function pass predictable? | `concrete file.con --check predictable` |
| What are the effects of each function? | `concrete file.con --report effects` |
| What is the stack depth? | `concrete file.con --report stack-depth` |
| What is the composite source-resource bound? | Planned: `concrete file.con --report resources` (R-0445) |
| Is this function proved? | `concrete file.con --report proof-status` |
| What host calls does this function make? | `concrete file.con --report alloc` (allocation), `--report caps` (capabilities) |
| Is there recursion? | `concrete file.con --report recursion` |
| What is trusted? | `concrete file.con --report effects` (trusted: yes/no column) |
| What changed since last snapshot? | `concrete diff old.json new.json` |
