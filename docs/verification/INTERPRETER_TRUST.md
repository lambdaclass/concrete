# Interpreter Trust Boundary

Status: current reference, pinned to `Concrete/Interp/Interp.lean`.

The source-level interpreter is invoked by
`concrete <file.con> --interp` and participates in the differential corpus under
`tests/oracle/` and `scripts/tests/check_differential_positions.sh`.

It is not a privileged definition of the whole language and it is no longer
justified by being a tiny, width-blind tree walker. It is a second executable
semantics over validated Core. Its value comes from taking a different path from
Lower/SSA/LLVM and exposing disagreements; trust is earned through explicit
coverage, differential testing, and independent expectations where both paths
could share a bug.

## 1. Boundary and shared dependencies

The interpreter consumes the `CModule` / `CExpr` / `CStmt` program produced
after parsing, resolution, checking, elaboration, and `CoreCheck`. It does not
run monomorphization, Lower, SSA cleanup, LLVM emission, clang, or the linker.

That split provides useful independence for middle-end and backend defects, but
it is not full independence:

- both interpreted and compiled paths trust the same frontend and Core;
- checked integer rules are shared through
  `Concrete.Semantics.IntArith`;
- builtin contracts and some runtime conventions are represented on both paths;
- neither agreement nor a cache/artifact match proves the shared input correct.

Accordingly:

> Interpreter/compiled agreement is strong differential evidence over the
> exercised observation, not proof and not an independent source oracle.

When both paths could share a lowering-independent mistake, use a hand-authored
expectation, standard test vector, external reference, or metamorphic property.

## 2. Current executable subset

The interpreter currently models:

- fixed-width integer, boolean, character, and string values;
- checked arithmetic, div/mod, shifts, negation, bitwise operations, and casts;
- structs, enums, arrays, matches, and nested field/index paths;
- local assignment, field/index/deref assignment, and outer-variable mutation;
- `if`, bounded `while`, `break`, `continue`, and labeled loop flow;
- direct calls, function-pointer values, and indirect calls;
- immutable/mutable path borrows across calls and named borrow regions;
- explicit `Result`/`Option` matching and returning error arms;
- `defer` on fall-through, return, break, and continue, in LIFO scope order;
- the runtime's String/print builtins, each modelled on its `EmitBuiltins`
  implementation over a `String` that is the `{ptr, len, cap}` struct over a heap
  buffer, and the builtin Vec intrinsics;
- IEEE `f64`/`f32` values: literals, arithmetic (`%` is C `fmod`), ordered and
  unordered comparisons, integer → float rounding to nearest-even, and the
  checked float → integer conversion;
- module constants, evaluated at the use in their own frame;
- a typed heap: `alloc`/`free`/`Heap<T>`, `malloc`/`calloc`/`realloc`/`free`,
  raw pointers with element-sized arithmetic, `memcpy`/`memmove`/`memset`/
  `memcmp`/`strlen`, `sizeof`/`alignof` through `Layout` with the running generic
  function's type arguments;
- a fixed table of C functions (`evalExtern`): the memory functions above,
  `puts`/`putchar`/`write` to stdout and stderr, `read` from stdin, `exit`,
  `abort`, `getenv`, the `FILE *` functions over an in-run file overlay
  (`fopen`/`fread`/`fwrite`/`fseek`/`ftell`/`ferror`/`fflush`/`fclose`; reads see
  the host's files, writes never reach the host; a write open needs the host's
  write permission, asked through access(2)), argv, and a few libm functions;
- the C library's stdout buffer: the print builtins, `puts` and `putchar` go
  through stdio (line-buffered on a terminal, else fully buffered), `write(1, ..)`
  does not, and stdout and stderr are replayed in the order they were written;
  `abort` flushes stdio on Darwin and not on glibc, as the binary's runtime does;
- project mode: `concrete src/main.con --interp -- args` on a project's entry
  point interprets the whole module graph `concrete build` compiles, std
  included, with the program's arguments and standard input;
- `return` reached from value position (a call argument, a literal field), and
  output printed before a trap, which is kept and printed.

`IVal` stores integer values as Lean `Int` together with their Concrete `Ty`.
References are logical paths (base binding, creation-frame depth, field/index
steps, mutability, or a heap cell), not machine addresses. Heap memory is typed:
a block holds the values stored into it at their byte offsets. A read finds the
value stored there, or assembles it from smaller values the layout places there
(integers byte-wise, little-endian; structs and arrays field-wise), or refuses;
reading uninitialized memory, reinterpreting a value as an unrelated type, a use
after free, a double free and an out-of-bounds access are refused with a reason,
never given a made-up value. A pointer converted to an integer stays a symbolic
address (offset arithmetic, differences and comparisons within one allocation);
any other use of the number is refused. Function pointers are explicit
`fnPtr` values. This lets the interpreter model observable value and mutation
semantics without pretending to reproduce LLVM memory layout or ABI behavior.

The interpreter does not enforce the borrow checker or ownership discipline
again. It consumes already-validated Core and assumes those checks were correct.
Its path-reference model is therefore an execution model, not a second safety
checker.

### Entry observation

The original oracle-vector contract is `fn main() -> Int`, compared through the
program's observable output/exit projection. Additional differential gates
compare stdout and trap behavior for selected constructs. A new observation
shape must be added deliberately; it must not be normalized away merely because
one path prints it differently.

## 3. Explicit exclusions

Unsupported constructs fail with an `interp: ...` diagnostic rather than being
silently approximated. Current important exclusions include:

| Construct | Current result |
|---|---|
| A foreign function outside the `evalExtern` table | `interp: foreign function '...' is not modelled` |
| The clock (`clock_monotonic_ns`), and any value only the host knows | explicit refusal |
| Uninitialized or reinterpreted memory, use after free, out-of-bounds access | explicit refusal naming the allocation |
| A pointer's numeric address used as a number | explicit refusal (`addr` supports only offset arithmetic) |
| Reading a union through a field other than the one written | explicit refusal |
| `alloc` with an explicit allocator (`allocCall`) | explicit unsupported diagnostic |
| The order of `write(1, ..)` or `write(2, ..)` output after more than 512 bytes of still-buffered stdio output, and a glibc trap with that much buffered | explicit refusal (depends on the C library's buffer size) |
| Reading stdin from a terminal; `argv[0]`; argv or environment strings that are not UTF-8 | explicit refusal |
| `fopen` of a device, FIFO or socket, a second handle on a file open for writing, writing a file with several hard links, update modes (`r+`...) | explicit refusal |
| Allocations over 4 GiB (success depends on the machine); past `PTRDIFF_MAX` they are NULL as in C | explicit refusal |
| Pointer comparisons that need a value's layout, the stack layout (one past the end, zero-sized values) or an allocator's reuse of freed memory | explicit refusal |
| A C function declared with an argument narrower than its C parameter, or a result wider than the C result | explicit refusal |
| Unsupported borrow/place shape | explicit shape diagnostic |
| Unsupported print/IO intrinsic reaching Core without required desugaring | explicit desugar/support diagnostic |
| Loop runaway beyond interpreter fuel | explicit fuel diagnostic |

The implementation's exhaustive pattern matches and the differential-position
gate are the operational source of truth for this list. Adding a new supported
constructor requires a discriminating interpreter/native case; adding an
unsupported branch requires an explicit diagnostic. R-0438 will generate
constructor coverage so this table cannot silently lag the evaluator.

## 4. Arithmetic, bounds, and failure

The old arbitrary-precision oracle argument no longer describes the
implementation. `Concrete.Semantics.IntArith` gives integer values fixed-width
meaning:

- ordinary addition, subtraction, multiplication, and negation trap when the
  result is outside the type's range;
- division/modulo trap on zero and on signed `MIN / -1`;
- shift amounts are checked against the operand width;
- bitwise operations are masked to the declared width;
- casts follow the language's target-width cast policy.

Raw array indexing is checked on both interpreted and compiled paths and traps
when the index is negative or outside the declared length. These paths are
expected to agree on value versus trap and, as the differential infrastructure
matures, on trap identity. Stack exhaustion remains host-dependent: the
interpreter uses Lean's stack while the compiled program uses the target stack,
so stack-bound claims belong to the compiled/profile evidence.

Sharing `IntArith` is deliberate single-sourcing of the language rule, but it
reduces oracle independence for arithmetic. Optimizer/DCE and emitted LLVM trap
semantics still need separate structural and mutation gates under R-0005; an
interpreter/compiled match alone cannot prove a shared rule was encoded
correctly.

## 5. Why the interpreter is useful

`Interp.lean` is currently about 2,700 lines and implements substantial language
behavior, including width-aware traps, mutable path references, function
pointers, loops, and `defer`. Its trust case is therefore not “small enough to
assume correct.”

The useful properties are:

- **Different execution path after Core.** It bypasses Mono, Lower, SSA cleanup,
  LLVM text generation, optimization, linking, and native ABI behavior.
- **Explicit unsupported boundary.** Unsupported forms fail loudly and remain
  inventory items instead of receiving guessed semantics.
- **Deterministic replay.** Supported pure/core observations do not depend on an
  LLVM version, linker, or target ABI.
- **Divergence hunting.** Each supported semantic family has paired execution,
  and failures retain the source and observations needed to determine which side
  is wrong.
- **No automatic authority upgrade.** Agreement is classified as tested
  evidence and cannot become `proved`, `enforced`, or a broader semantic claim.

On mismatch, neither side wins by status. Triage against the language policy,
an independent expectation, or a reduced witness. A mismatch proves
disagreement; it does not by itself identify the faulty implementation.

The interpreter is not intended to become a fully verified reference
implementation by accretion. ProofCore and the compiler-soundness bridge own
formal semantics/preservation work. The source interpreter remains an
engineering oracle whose coverage and shared dependencies must stay visible.

## See also

- `tests/oracle/README.md` — vector and PENDING/FAIL contract
- [ARITHMETIC_POLICY.md](../language/ARITHMETIC_POLICY.md) — integer and bounds semantics
- [PREDICTABLE_BOUNDARIES.md](../platform/PREDICTABLE_BOUNDARIES.md) — compiled/profile
  execution boundary
- [PROOF_SEMANTICS_BOUNDARY.md](PROOF_SEMANTICS_BOUNDARY.md) — separate
  ProofCore semantics
- [TRUSTED_COMPUTING_BASE.md](TRUSTED_COMPUTING_BASE.md) — project-wide trust
  accounting
