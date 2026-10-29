# Candidate Ports: A Measured Inventory

Status: Open

This note records the ~10k-line C/C++ programs evaluated as Concrete port
workloads, with sizes measured rather than quoted from READMEs, plus the fit
analysis that decides which roadmap item owns each. It exists so the port ladder
(`Task R-0134`) can admit one port at a time against a named forcing claim
instead of re-litigating candidate choice each time a tier opens.

The roadmap items that own this inventory are `Task R-0449` (old Phase 8, the port
set and the admission rule), `Task R-0450` (Phase 8, the X.509 security
flagship), `Task R-0451` (Phase 11, the SAT/DRAT trust item), `Task R-0452`
(Phase 13, the obligation-density port), and `Task R-0453` (Phase 16, the
block-device freestanding workload). `Task R-0448` (MM0 verifier) predates this
note; its analysis remains owned by that roadmap task rather than by a separate research file.

Measurements were taken 2026-07-31 from shallow clones at the commits recorded
below. "Core LOC" counts non-test `.c`/`.h` (`.cc` for MiniSat) in the subset a
port would actually reproduce — not the whole repository, and not counting the
upstream test corpora, which are the assets a port wants most.

## How Candidates Were Filtered

Four filters did the real work, and they are worth keeping for future candidates:

1. **Size band.** 5k–12k core lines. Below ~3k the port does not pressure the
   language; above ~15k it stops being a workload and becomes a program of
   record with its own maintenance cost.
2. **The oracle must be external and machine-checkable.** A port whose
   correctness argument is "it looks right" produces no evidence. Preferred:
   an upstream conformance suite (CommonMark), a bit-exact reference codec
   (zlib), a certificate/proof corpus, or the upstream implementation itself as
   a differential oracle.
3. **No GC, no `longjmp`/unwinding.** Concrete aborts rather than unwinding and
   has no GC, so Lua, wren, and QuickJS are ports of a runtime the language
   deliberately refuses. This excluded every candidate small-language runtime
   except the ones already covered by `examples/lox` and `examples/mal`.
4. **Global mutable state is an admission cost.**
   `docs/spec/overview.md` §10 forbids it, so a program built around
   file-scope mutable arrays must have every one threaded through an explicit
   context struct before the port starts. Candidates already organized around a
   context handle (`MD_CTX`, `z_stream`, `lfs_t`) port close to 1:1; QBE,
   chibicc, and uIP do not.

Two structural notes that are *not* filters, because Concrete has an answer:

- **Callback/renderer architectures are fine.** A C struct of function pointers
  is exactly the shape `docs/project/ANTI_FEATURES.md` prescribes in place of trait
  objects, so md4c's renderer interface validates that decision on real code.
  The friction is the untyped `void *userdata` beside it, which wants to become
  a typed generic parameter.
- **Aliased mutable graphs cost per site, not per concept.** Index/arena
  designs (QBE's packed `Ref`, littlefs's block numbers) already match what
  linear ownership pushes toward; designs built on pervasive aliasing must be
  re-architected, and the cost scales with the number of touch sites, not the
  difficulty of the idea.

## Measured Inventory

| Candidate | Core LOC | Upstream commit | Context model | External oracle | Owner |
|---|---|---|---|---|---|
| md4c (CommonMark) | 10,660 (7,240 engine + 2,185 entity tables) | `10c0158a` | `MD_CTX` | CommonMark spec suite (650+ cases), pathological suite, fuzzers | R-0449 |
| mbedtls X.509 read path | 5,126 (`x509.c` + `x509_crt.c`; ASN.1 primitives now in the `tf-psa-crypto` submodule, uncounted) | `3bb37386` | ctx-based | certificate corpora, fuzzers, differential vs mbedtls | R-0450 |
| MiniSat | 5,296 | `37dc6c67` | solver object | SAT competition benchmarks; DRAT certificates checkable by `drat-trim` | R-0451 |
| microsat | 251 | `26985d9b` | single struct | same, at probe scale | R-0451 (probe) |
| zlib inflate | 2,365 | `e3dc0a85` | `z_stream` | bit-exact vs zlib on any corpus; RFC 1950/1951 | R-0452 |
| zlib deflate | 3,687 | `e3dc0a85` | `z_stream` | round-trip through zlib's inflate | R-0452 (second stage) |
| littlefs | 7,633 (`lfs.c` 6,558) | `6cb4e865` | `lfs_t`, zero malloc | upstream test runner + `lfs_emubd` power-loss simulation | R-0453 |
| QBE (amd64 only) | 11,281 (17,153 with arm64/rv64/minic) | `cf06ce15` | **file-scope globals** | Concrete's own LLVM backend as differential oracle | R-0449 (deferred tier) |
| cproc | 8,923 | `d1c53ddf` | globals | C conformance suites; emits QBE IR, so it chains with QBE | R-0449 (deferred tier) |
| chibicc | 9,154 | `90d1f7f1` | globals | its own test suite; self-hosting | R-0449 (deferred tier) |
| uIP | 8,709 | `a49def74` | global packet buffers | interop against a real TCP peer | R-0449 (deferred tier) |
| Monocypher | 3,956 | `1830c06d` | pure functions | RFC/reference vectors, TweetNaCl differential | R-0450 (pulled only if signature verification is in scope) |
| tinf | 1,720 | `57ffa1f1` | ctx-based | zlib | none; a probe for R-0452, not a workload |

## Per-Candidate Notes

### md4c — the oracle candidate

A CommonMark 0.31 parser with a SAX-style renderer interface. It is the best
*oracle* on the list, which is what Phase 8's external-credibility probe
actually needs: the upstream `test/` tree carries the full CommonMark spec suite
(650+ machine-checkable cases), a pathological-input suite, and fuzz harnesses.
Pure text→text with no I/O, so the capability surface is `with(Alloc)` only, and
2,185 of its lines are generated HTML-entity tables rather than logic.

Where the fit is bad: CommonMark is a spec of *behavior*, not of properties, so
this port produces `tested_by_oracle` at high coverage and very little proof.
Inline-parsing state (delimiter stacks, link-reference definitions) is the part
that will pressure linear ownership hardest.

### mbedtls X.509 read path — the security flagship

Certificate parsing is where authority, adversarial input, and bounds
obligations all meet, which is exactly Phase 8's stated Done criterion ("a
serious security/crypto or protocol example"). The read-only subset is the port:
parse, validate structure, expose fields. Corpora and fuzzers exist in
abundance, and mbedtls itself is the differential oracle.

Where the fit is bad: signature *verification* pulls in bignum and
curve/RSA primitives (the Monocypher-class dependency), which is a second
workload, not a stretch goal of this one. A port that validates structure but
cannot check a signature must say so in its claim, loudly.

### MiniSat / microsat — the reflexive candidate

The only candidate whose value is a change to Concrete's own trust story rather
than a workload lesson: a Concrete-native CDCL solver that emits DRAT proof
certificates lets `solver_trusted` results be *replayed* by code the project
controls. microsat (251 lines) is a cheap probe of the port shape; MiniSat
(5,296) is the real thing.

Where the fit is bad: MiniSat is C++ with templates (fine — monomorphization
maps directly) but also with its own allocator, aggressive in-place clause
mutation, and `OutOfMemoryException`. The clause database is the linear-ownership
stress point. Critically, a Concrete SAT solver does **not** produce kernel
evidence; see the claim discipline in R-0451.

### zlib inflate/deflate — the proof candidate

The best *proof* target and the smallest serious one. A bit reader plus a 32 KB
sliding window is nothing but index and shift obligations — exactly the shape
`omega`/`bv_decide` discharge in-kernel — over the historically most CVE-dense
code shape in computing, with a bit-exact oracle for free. Inflate first (2,365
lines); deflate (3,687) only if inflate's obligation story lands.

Where the fit is bad: zlib's C is written for speed, with pointer-arithmetic
fast paths (`inffast.c`) that a faithful port must either re-express as index
arithmetic or admit as a trusted island. Choosing the former is the point of the
exercise; choosing the latter cheaply defeats it.

### littlefs — the freestanding candidate

A power-loss-resilient flash filesystem: no dynamic allocation, fixed buffers,
a block-device interface, and an explicit `lfs_t` context. Upstream ships a test
runner plus `lfs_emubd`, an emulated block device that can inject power loss at
arbitrary points — a ready-made red-team harness for exactly the property that
matters.

Where the fit is bad: crash consistency is a *relational* property across
program runs (any interrupted prefix leaves a mountable filesystem). That is
beyond what `#[requires]`/`#[ensures]` can state today, so the honest evidence
class is `tested_by_oracle` under adversarial power-loss injection, not a proof.

### QBE — the strategic candidate, deliberately not scheduled

Port value is high: Phase 7.5's backend *emits* QBE IR, so a Concrete
implementation of QBE would let the emitted-IR path and the ported tool
cross-validate, and the LLVM backend is a ready differential oracle. QBE's
packed `Ref` handles are already the arena style linear ownership wants, and its
passes (`parse.c` 1,433; the rest 400–700 each) decompose into ~15 independently
green slices.

Why it is not scheduled: it is the largest candidate (11,281 lines for amd64
alone), and it needs every file-scope mutable global threaded through a context
struct first. Scheduling an 11k-line port before any 10k-line port has landed
would violate R-0134's own sequencing rule. **This is not a substitute for
Phase 7.5's QBE backend, and must never be conflated with it:** one is Concrete
emitting QBE IR, the other is QBE written in Concrete.

### cproc / chibicc, uIP, Monocypher, tinf — recorded, not scheduled

- **cproc (8,923) / chibicc (9,154):** a C11 frontend in Concrete. cproc emits
  QBE IR, so cproc + the QBE port is a complete C toolchain in Concrete — a
  strong story, gated on the QBE port, and on accepting C's enormous
  conformance surface. Pull trigger: the QBE port lands.
- **uIP (8,709):** the `with(Network)` flagship on paper, but its global packet
  buffers and macro-heavy 2000s-era C make the port a rewrite. Pull trigger:
  a network workload that `std.net` cannot already carry.
- **Monocypher (3,956):** clean fit (pure functions over byte arrays) but
  largely redundant with `examples/hmac_sha256` and `std.sha256`. Pull trigger:
  R-0450 needs signature verification.
- **tinf (1,720):** too small to be a workload; useful only as a two-day probe
  of the inflate port shape.

## What Was Rejected, With Numbers

- **rv32emu (40,112 lines)** and **wasm3 (26,110)** — outside the band. A
  RISC-V or Wasm interpreter remains attractive (both have official conformance
  suites); the well-engineered implementations are simply too large, and a
  hand-rolled subset would lose the oracle that made them attractive.
- **Lua, wren, QuickJS** — GC plus `longjmp`. Excluded by filter 3.
- **kuna (443,953 lines of Rust across 657 files)** — the original question that
  produced this inventory. Beyond size, it is the anti-example for the
  structural filters: 2,473 `Rc<...>` sites (1,155 `Rc<Datatype>`, 775
  `Rc<AddrSpace>`) plus 265 `RefCell`, and 1,544 `dyn` sites over an open rule
  hierarchy (`dyn Action` 216, `dyn Rule` 182). A port is a re-architecture, and
  at an estimated 700k–1M lines of Concrete it would exceed the entire existing
  `.con` corpus (56,175 lines) by more than an order of magnitude under
  whole-program monomorphization with no separate compilation. A vertical slice
  (ELF loader + `.sla` load + one architecture's lifter, ~30–50k lines, with
  kuna itself as the oracle) is the only tractable framing, and it is not
  scheduled.

## Sequencing

One port at a time, in the order the owning roadmap items appear by file
position. Each port carries `PORT.toml` with the fields R-0134 already defines,
and each leaves the `WORKLOAD_REPORT.md` gap report R-0134 requires — a port
that only builds has not paid for itself. Candidates keep their measured numbers
here; adding a candidate means measuring it the same way, not quoting a README.
