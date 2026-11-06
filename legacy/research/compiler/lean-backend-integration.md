research record, from archived branch feat/lean-backend-integration; schedules nothing

# Experimental Lean Backend Consumer

Status: experimental integration, restricted supported domain
Date: 2026-10-18
Owner: R-0297 / Phase 7.5; this experiment does not close either task

Concrete now has an isolated consumer experiment for the general-purpose
[Lean Compiler Backend](https://github.com/unbalancedparentheses/lean-compiler-backend).
That separate project is porting QBE 1.3 to Lean 4; it is not a Concrete-specific
backend and is not yet the full port. The consumer code is developed in the
`feat/lean-backend-integration` worktree/branch, separately from Concrete main.

```text
Concrete source → normal frontend/Core checks → monomorphization
→ verified/cleaned SSA → restricted QBE text → lcb → ARM64 macOS assembly
→ Nix C compiler/linker + small exit wrapper → native executable
```

## Implemented consumer

- `Concrete/Backend/EmitQBE.lean` emits standard QBE-style IL; no backend library
  dependency is added to Concrete's type checker or language semantics.
- `concrete FILE --emit-qbe` runs the normal frontend, proven-violation check,
  monomorphization and both SSA verifier passes before emission.
- `bash scripts/compile_with_lcb.sh FILE -o OUTPUT` invokes the separately built
  backend and Nix's SDK-aware C compiler. Its wrapper uses the same `ret & 255`
  exit-status rule as the existing LLVM entry wrapper.
- Names are encoded rather than interpolated into assembler/IR identifiers.
  Unsupported instructions produce an error before a native output is created.

The supported domain is Int/Uint, i32/u32 and Bool, explicit wrapping arithmetic,
bitwise/Boolean operations, integer comparisons, selected integer casts, branches,
loops and phi nodes. Executables currently require a no-argument Int/Uint main.
Backend resource limits and the initial eight-register-argument ABI still apply.

Concrete's actual LLVM emitter implements runtime checks for ordinary arithmetic,
negation, division and shifts. Those remaining SSA operations are **rejected** by
this adapter; translating them to modular QBE operations would change behavior.
Safe constant folding can remove an operation before emission. Tests use unknown
parameters when checking that unsupported checked operations are rejected.

Calls, runtime helpers, global data, memory/address operations, aggregates in use,
floating point, narrow integers, Unit returns and Boolean casts remain outside
the consumer's supported domain. `unreachable` emits QBE `hlt`; this is not a
replacement for Concrete's specified `abort()` behavior on checked failures.

## Reproduce with Nix

From this worktree, with the backend checkout beside Concrete under `projects/`:

```sh
nix build path:/Users/unbalancedparen/projects/lean-compiler-backend -o /private/tmp/concrete-lcb-result
nix develop -c lake build concrete
nix develop -c env LCB_BIN=/private/tmp/concrete-lcb-result/bin/lcb bash scripts/compile_with_lcb.sh examples/lean_backend/src/main.con -o /private/tmp/concrete-lcb-example
nix develop -c env LCB_BIN=/private/tmp/concrete-lcb-result/bin/lcb python3 scripts/tests/check_lcb_integration.py
```

The example returns exit status 42. The integration test compares seven actual
native executables against Concrete's LLVM emission linked with the same Nix
compiler, including loops/phi nodes, signed/unsigned comparison, wrapping overflow
and exit masking. Six controls exercise rejected checked/narrow/call operations.
Native execution requires an ARM64 Mac; unsupported hosts do not count as passes.

## Factorial runtime baseline

The benchmark (`tests/perf/lean_backend/README.md` in `refs/archive/branches/feat/lean-backend-integration`) compiles one Concrete
factorial source through LLVM, LCB and original QBE 1.3 using Nix tools. All three
check 0! through 20! and produce identical timed-loop checksums. The inputs vary
at runtime, calls cross a compilation boundary and LTO is disabled.

Local ARM64 macOS results on 2026-10-18, median of nine interleaved trials:

| Calls per trial | LLVM | QBE 1.3 | LCB | LCB / LLVM elapsed time |
| --- | ---: | ---: | ---: | ---: |
| 1,000,000 | 9.740 ms | 11.377 ms | 36.041 ms | 3.70× |
| 10,000,000 | 125.120 ms | 126.912 ms | 905.519 ms | 7.24× |

The host was heavily loaded: the one-minute load averages were approximately
43 and 49 on an 18-logical-CPU machine. In the longer run, LCB samples ranged
from 608 to 1,134 ms, LLVM from 97 to 178 ms and QBE from 87 to 159 ms. These
measurements establish a working, slower initial backend on this workload;
the different ratios do not establish a stable slowdown factor. Repeat on an
idle host before setting a performance target. LCB currently uses stack slots
for intermediate values and has no register allocator or optimization pipeline.
That implementation is a clear optimization opportunity; its contribution to
the measured gap has not been isolated. Lean's compiler runtime is not linked
into these generated factorial executables.

Raw measurements, including all samples and source/tool hashes:

- [One million calls](measurements/2026-10-18-factorial-1m.json)
- [Ten million calls](measurements/2026-10-18-factorial-10m.json)

This is testing evidence for the stated domain. It is not a proof of the adapter,
the backend, the assembler/linker, or source-to-binary correctness. The backend's
small primitive-selection theorem does not cover this integration. Full QBE
compatibility, broader Concrete consumers and the larger proof effort remain
separate milestones.
