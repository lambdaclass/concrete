# Comparative Program Suite

Status: exploratory

This note defines the large-program comparison suite for Concrete after the language surface has been disciplined enough to make sustained implementation pressure meaningful.

The goal is not only to produce benchmark numbers. The goal is to compare Concrete against neighboring systems languages through the same real programs and the same inputs, and to ask:

- are the programs correct?
- are they fast enough?
- how much memory do they use?
- how large are the binaries?
- how long do they take to compile?
- how much code do they require?
- how much unsafe/trusted/manual-boundary code do they need?
- how easy are they to audit and explain?

The suite should therefore compare:

- Concrete
- Rust
- Zig
- C

where that comparison is reasonable.

## Comparison Dimensions

Every program in the suite should be evaluated across the same broad dimensions:

| Dimension | Questions |
|-----------|-----------|
| Correctness | Do all implementations pass the same tests and edge cases? |
| Runtime | Throughput, latency, steady-state speed |
| Memory | Peak memory, allocation behavior, retained memory |
| Build cost | Compile time, rebuild time, incremental behavior |
| Binary size | Output size under comparable build modes |
| Code size | Approximate LOC and module/package structure |
| Trust surface | How much `unsafe`, FFI, or manual boundary code is needed? |
| Auditability | How easy is it to identify authority, allocation, trust, and cleanup boundaries? |

The point is to understand where Concrete is stronger, weaker, or simply different, not to reduce the comparison to one speed number.

## Portfolio Shape

Do not treat the suite as 20 equal examples. Treat it as a ladder:

1. a first wave that should reveal the highest-leverage language and stdlib pressure quickly
2. a second wave that expands coverage once the first findings are closed
3. a broader 20-program portfolio for continued comparison breadth

The portfolio should optimize for:

- different pressure shapes
- external comparability
- a clear “why Concrete?” signal
- enough overlap for findings to compound without filling the suite with duplicates

The full suite still spans four buckets:

1. standard comparison-friendly workloads
2. systems / infrastructure workloads
3. data-structure / algorithm workloads
4. Concrete-identity / mission-critical workloads

This mix gives both:

- externally credible comparison points
- workloads that actually test Concrete's intended niche

## Recommended 20-Program Suite

This is the broader “cool + comparable + finishable” portfolio. The target size for most of these should stay under roughly `10k-20k` LOC.

| # | Program | Bucket | Why It Matters | Est. LOC | Compare Against |
|---|---------|--------|----------------|----------|-----------------|
| 1 | MAL-style Lisp interpreter | Standard | staged interpreter workload with tests; reader/evaluator/env pressure | 15k-20k | Rust, Zig, C |
| 2 | Lox interpreter | Standard | known parser + resolver + runtime target from Crafting Interpreters | 15k-20k | Rust, Zig, C |
| 3 | JSON parser + validator | Standard | parsing, trees, errors, allocation | 10k-15k | Rust, Zig, C |
| 4 | TOML parser | Standard | richer structured parsing with shared test corpora | 10k-20k | Rust, Zig, C |
| 5 | grep-like text search tool | Standard | strings, files, streaming, performance | 10k-20k | Rust, Zig, C |
| 6 | regex engine | Standard | parser + automata + performance pressure | 10k-20k | Rust, Zig, C |
| 7 | bytecode VM / interpreter | Standard | dispatch, control flow, runtime values | 15k-20k | Rust, Zig, C |
| 8 | artifact/update verifier | Identity | hashes, signatures, policy, critical path | 10k-20k | Rust, Zig, C |
| 9 | policy/rule engine | Identity | explicit authority, auditability, decision logic | 10k-20k | Rust, Zig, C |
| 10 | protocol/message validator | Identity | bounded parsing, correctness, high-integrity fit | 10k-20k | Rust, Zig, C |
| 11 | small TCP/HTTP service | Systems | networking, parsing, module structure | 10k-20k | Rust, Zig, C |
| 12 | file tree scanner + policy checker | Systems | paths, traversal, explicit authority | 10k-20k | Rust, Zig, C |
| 13 | package/archive indexer | Systems | file formats, hashing, metadata | 10k-20k | Rust, Zig, C |
| 14 | text/template transformer | Systems | strings, parsing, output correctness | 10k-20k | Rust, Zig, C |
| 15 | log processing/query pipeline | Systems | parsing, transforms, aggregation | 10k-20k | Rust, Zig, C |
| 16 | diff / matcher engine | Data structure | strings, algorithms, memory behavior | 10k-20k | Rust, Zig, C |
| 17 | inverted index / search core | Data structure | `HashMap`/`Vec` pressure, indexing | 15k-20k | Rust, Zig, C |
| 18 | scheduler / event engine | Data structure | heaps, ordering, event flow | 10k-20k | Rust, Zig, C |
| 19 | SQLite-style miniature database | Systems | serious storage/runtime comparison target | 15k-20k | Rust, Zig, C |
| 20 | WASM validator/interpreter subset | Standard | semantics/runtime validation with strong external comparability | 10k-20k | Rust, Zig, C |

## How To Use Existing Work

Where possible, these programs should reuse:

- existing benchmark problem shapes
- public datasets and benchmark inputs
- well-understood reference implementations

This keeps the suite comparable and avoids inventing a benchmark world that only Concrete knows how to play in.

Good inputs to reuse:

- JSON/config corpora
- real log files
- package/manifest corpora
- graph datasets
- protocol/message traces
- real file trees or archive collections

## Existing-Comparison-Friendly Subset

These are the best first programs for direct Rust/Zig/C credibility:

1. MAL-style Lisp interpreter
2. Lox interpreter
3. JSON parser + validator
4. TOML parser
5. grep-like text search tool
6. regex engine
7. bytecode VM / interpreter
8. small TCP/HTTP service

These workloads already have obvious equivalents in neighboring languages.

## Concrete-Identity Subset

These are the strongest programs for showing why Concrete should exist at all:

1. policy/rule engine
2. artifact/update verifier
3. protocol/message validator
4. HSM/key-use policy engine
5. MAL-style Lisp interpreter

These are especially important because they stress:

- explicit authority
- visible trust boundaries
- high-integrity profile direction
- report usefulness
- reviewability under real code size
- interpreter/runtime pressure against a known external target rather than only internal examples

## First-Wave Ladder

The best early sequence is:

1. policy/rule engine
2. MAL-style Lisp interpreter
3. JSON parser + validator
4. grep-like text search tool
5. bytecode VM / interpreter
6. artifact/update verifier

Why this specific ladder:

- policy/rule engine proves the authority/auditability niche
- MAL proves parser/runtime/interpreter pressure
- JSON and grep prove text/parser/streaming reality
- bytecode VM proves control-flow/runtime/codegen pressure
- artifact verifier returns to Concrete’s intended critical-software niche

This is a reordering and prioritization, not a replacement. The early comparison-heavy set is still present; it is just arranged to maximize insight sooner.

## Second-Wave Ladder

Once the first-wave findings are closed, the next best sequence is:

1. regex engine
2. Lox interpreter
3. small TCP/HTTP service
4. file tree scanner + policy checker
5. package/archive indexer
6. HSM/key-use policy engine

Why these next:

- regex and Lox deepen text/parser/runtime pressure with strong external comparability
- TCP/HTTP, file tree scanning, and archive indexing expose systems/module/stdlib reality
- HSM/key-use policy returns to a high-consequence Concrete-identity workload once the surrounding ergonomics are stronger

## Highest-Value 12

If the suite needs to be cut down to the most important 12 programs, keep:

1. policy/rule engine
2. MAL-style Lisp interpreter
3. JSON parser + validator
4. grep-like text search tool
5. bytecode VM / interpreter
6. artifact/update verifier
7. regex engine
8. small TCP/HTTP service
9. Lox interpreter
10. file tree scanner + policy checker
11. package/archive indexer
12. HSM/key-use policy engine

This set is strong because:

- 1, 6, 12 show Concrete’s niche
- 2, 5, 9 stress runtime/interpreter shape
- 3, 4, 7 stress text/parser workloads
- 8, 10, 11 stress systems/module/stdlib reality

## Interpreter Target

The recommended interpreter/runtime workload is **MAL-style Lisp**, not an ad hoc Scheme or Common Lisp subset.

For implementation order, MAL should be the **second Phase H program**, immediately after the first policy/rule-engine workload.

Why MAL is the right target:

- it is a known staged implementation target rather than a vague "tiny Lisp"
- it comes with a strong external comparison story and existing test material
- it stresses reader/parser, AST/value representation, environments, evaluation, errors, and REPL/runtime structure
- it is large enough to reveal real language/runtime ergonomics without exploding into full-language implementation sprawl

The intent is not to turn Phase H into "build a scripting language ecosystem." The intent is to include one recognizable interpreter workload that:

- is interesting outside the Concrete project
- has clear Rust/Zig/C comparison value
- reveals how Concrete handles dynamic-language runtime structure, allocation pressure, and sustained module growth

before filling out the rest of the portfolio.

## External-Tested Workload Track

Phase H should not rely only on self-chosen programs. It should also maintain an explicit track of workloads that come with known specs, known tests, or widely recognized behavior targets.

Best options:

- **MAL (Make a Lisp)**  
  Probably the best fit for an interpreter workload. Good because it has staged tasks and tests.
- **SQLite-style miniature database projects**  
  For example the "build your own sqlite" class of projects. Good because they usually have known behavior and lots of comparison value. Harder, but very revealing.
- **Crafting Interpreters / Lox**  
  Lox has a well-known spec and test corpus. Very strong if you want a parser + resolver + runtime workload.
- **Wren / Lua-style small VM/interpreter clones**  
  Good if you want bytecode/runtime pressure with recognizable semantics.
- **Scheme/Lisp educational interpreters**  
  SICP-style or Norvig-style projects are good, but MAL is usually more comparable.
- **Regex engine projects**  
  Small regex engines often have test suites and clear behavior targets. Good for parser + automata + performance.
- **JSON test suites**  
  There are standard JSON conformance suites. Great for parser/validator comparison.
- **TOML / YAML / CSV parser suites**  
  TOML especially has strong shared test corpora.
- **WASM interpreter / validator subsets**  
  There are small WASM runtimes and validator projects with test material. Very good long-term, but probably not first-wave.
- **Brainfuck interpreters**  
  Many exist with tests, but they are too toy-like to be a flagship.

Recommended use:

- **First-wave:** MAL, JSON, grep, bytecode VM, artifact verifier
- **Second-wave:** regex, Lox, small TCP/HTTP service, file tree scanner, package/archive indexer, HSM/key-use policy engine
- **Later but important:** TOML parser, SQLite-style miniature database, WASM subset
- **Secondary comparison baselines:** Wren/Lua-style small VM, Scheme/Lisp educational interpreters, Brainfuck

## What To De-Prioritize

Avoid spending too much early effort on:

- Brainfuck as a major deliverable
- multiple near-duplicate Lisp/Scheme interpreters
- too many pure algorithm benchmarks
- workloads that mostly test LLVM rather than Concrete
- examples that need lots of ecosystem glue before they reveal anything meaningful

## Per-Program Output

Every serious program in the suite should leave behind:

- correctness status
- performance snapshot
- memory, binary-size, and compile-time notes
- language gaps found
- stdlib gaps found
- whether Concrete looked stronger, weaker, or just different from Rust/Zig/C

## Success Conditions

This suite is successful if it reveals:

- language design weaknesses that only appear at 10k-30k LOC
- stdlib gaps that integration tests do not expose
- diagnostics/tooling failures that only appear under sustained use
- package/build workflow friction
- codegen and allocation cliffs
- places where Concrete is genuinely more auditable or easier to constrain than Rust/Zig/C

It is not successful if it becomes:

- only a speed leaderboard
- only toy programs
- only workloads that favor LLVM micro-optimizations
- only internal demos with no comparison value
