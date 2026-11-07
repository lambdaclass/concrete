# Examples per slice (proposal, 2026-11-06; showcases adopted 2026-11-07)

Status: the per-slice draft is a proposal; the showcases are adopted
([decision](../decisions.md#decided-2026-11-07-showcases)). Codex, agy and Kimi reviewed the
per-slice draft below, and all three said "agree with changes". Their changes still need a second
review round. Once accepted, the examples live as files next to each slice, and this file is retired.

We propose that every example runs on the Lean evaluator, LLVM and LCB, and that each claim is a
Lean theorem or a `concrete facts` line.

## Showcases

Status: adopted by the owner on 2026-11-07
([decision](../decisions.md#decided-2026-11-07-showcases)). The owner asked for bigger examples that are
more show-off and useful (2026-11-06: "this is great but i think we should build better and bigger
examples" and "amzing, but i would laso want something that is more show of and useful"). Each pairs a program people already use with what Concrete proves
about it, and states its functional, resource, authority and backend claims separately.

| Showcase | First version | Full version | What Concrete proves |
|---|---|---|---|
| `grep` with a linear-time regex | 16: stdin, a pattern given in the source, fixed buffers | 18: `grep PATTERN FILE`; 22: dynamic patterns | matching terminates and agrees with the regex semantics; linear time through the step model at 4–5 and step-bound proofs at 8 ([decision](../decisions.md#decided-2026-11-07-step-bounds)) |
| Image decoder | 19 → 21: QOI | 21–22: PNG with DEFLATE | any input decodes or is refused with a typed error; memory bounded from the header; no hidden traps |
| EVM interpreter | 15: u256 arithmetic and the opcode set | 21 → 23: growable memory, gas, Keccak | each opcode against the EVM spec; gas-bounded; no trap on any bytecode |
| Embedded key-value store | 23: in-memory index plus an append-only log | 23, with RocksDB at 25 for comparison | bounded memory and correct contents; durability "not established" (crash model not planned) |
| WebAssembly sandbox | 21: interpreter with fuel and linear memory | 23 → 26: host imports, capability-limited host calls | memory safety, fuel-bounded termination, host calls limited by capabilities |
| Encrypted tunnel (Noise + ChaCha20-Poly1305) | 7 → 15: ChaCha20, Poly1305, X25519 as the `Secret<T>` flagship; 18: file encryptor | after 28: the tunnel, after the randomness/nonce audit | crypto correct and constant time; secrets never reach output; one network grant |
| Static web server serving the Concrete website | 27: one connection at a time, reading `./site` | after 28: concurrent version | reads only `./site`; per-request memory bound; no crash on any input |

Growing flagships: sha256sum (2 to 19), the ChaCha20 constant-time encryptor (7 to 18), and a calc
tokenizer, parser and stack VM (3 to 19).

## Per-slice draft (Claude, reviewed once)

We propose that every slice has these kinds of examples:
- **F, flagship:** a small real program with a Lean theorem about what it computes.
- **R, refusals:** what Concrete catches, each paired with the accepted version.
- **X, adversarial interaction:** R-0573, plus a facts snapshot (`concrete facts`).

Each proposed example runs on the Lean evaluator, LLVM and LCB.

| Slice | Examples |
|---|---|
| 0 u8 + checked add | **F:** `return 20 + 22` = 42 and a `let` chain. **R:** `200 + 56` refused (known overflow). **Facts:** authority none, terminates, heap 0, may-abort no. |
| 1 linear empty struct + `abort()` | **F:** a token is created, moved and consumed. `abort()` gives exit 134 and the trap line. **R:** use-after-move; a linear value dropped without being consumed; `200 + 56`. |
| 2 integers complete | **F:** bswap32, popcount via bit ops, saturating clamp, sign-extension conversions, each proven equal to a spec function. **R:** a shift by ≥ the width, known division by zero, i8 overflow, a lossy `as`. |
| 3 bool/if/let mut + trap checker | **F:** `abs`, `clamp`, `max3`, proven. The guarded add `if x < 200 { x + 55 }` is accepted with no runtime check. **R:** an unguarded add of an unknown value; a mutated value used across a branch without evidence. |
| 4 loops + termination | **F:** sum 1..n with a bound, iterative gcd with `#[variant]`, integer square root, iterative Fibonacci with an overflow guard. **R:** a loop with no variant (refused, or marked `#[may_not_terminate("...")]`); a Collatz loop as the marked example. |
| 5 functions + recursion | **F:** recursive factorial with a variant and an overflow `#[requires]`; recursive gcd; mutual recursion (even/odd). **R:** Ackermann without a lexicographic measure (refused or marked); a call violating `#[requires]`. **Facts:** stack bound from the artifact. |
| 6 capabilities | **F:** `main` receives a grant and passes it to a helper with `with(...)`; `give_up` ends it. **R:** calling a `with(X)` function without X; leaking a grant. **Facts:** transitive authority. |
| 7 `Secret<T>` scalars | **F:** constant-time select and min/max of secrets; equality returning `Secret<bool>`; a traced `declassify`. **R:** branching on a secret, indexing with a secret, plain `+` on a secret, an untraced release. |
| 8 proof surface | **F:** a ghost `spec fn` for n(n+1)/2 with `#[proof(path::to::theorem)]` naming the theorem that links the loop sum to it; `assert(e) by thm(...)`. **R:** a vacuous proof (an impossible precondition); an unmatched `#[proof(path::to::theorem)]`. |
| 9 structs | **F:** Point/Rect area with a proven formula; a struct owning a linear field. **R:** a field moved twice; a partial move used afterwards. |
| 10 enums/match | **F:** a traffic-light state machine with a proven invariant; a byte classifier. **R:** a non-exhaustive match. |
| 11 generics | **F:** generic `max`, `swap`, `Pair<T>`. **R:** a Copy-only operation on a linear `T`. |
| 12 Option/Result/checked_* | **F:** parse a decimal digit giving `Result`; a `checked_*` chain with typed `ArithmeticError`; `try_convert`. **R:** an ignored `Result`. |
| 13 borrows | **F:** increment through `&mut`; read-only borrows. **R:** two `&mut`; using the root while it is frozen; returning a reference. |
| 14 traits/Destroy/defer | **F:** a resource with `Destroy`; `defer` cleanup on every returning path, including an early return. **Control:** `abort()` runs no cleanup. **R:** a missing cleanup; using a value after `defer` consumed it. |
| 15 arrays | **F:** array sum, binary search, insertion sort proven sorted and a permutation; `ct_eq` on keys. **R:** a possibly-out-of-bounds index; a secret index. |
| 16 text/console | **F:** hello world, echo, ASCII uppercase, UTF-8 validation proven. **R:** writing without the Console grant. |
| 17 modules | **F:** base64 encode/decode split into modules, roundtrip proven (legacy workload 1). **R:** a private item used outside its module. |
| 18 files → file-copy, 0.1 | **F:** file-copy, cat, wc; every error path closes its handles (proven). **R:** opening outside the granted directory; a leaked handle. |
| 19 allocation | **F:** an arena tokenizer with a proven peak-memory bound; png_chunks (legacy workload 2). **R:** use after reset; any allocation, bounded or not, under the predictable policy, which admits none ([row](../decisions.md#decided-project-predictable-policy)). |
| 20 function values | **F:** callback-based iteration with explicit context; effects shown through the callback. **R:** a function with a safety `#[requires]` used as a value without a matching function-type contract (R-0601). |
| 21 HeapArray | **F:** a fixed-length heap buffer filled and read back. **R:** an index past the length. |
| 22 Vec and String | **F:** a line-splitting tokenizer. **R:** an element lost from `drain`. |
| 23 OrderedMap and HashMap | **F:** wordfreq (legacy workload 6), envcfg (workload 5). **R:** a lookup result ignored. |
| 24 raw/unsafe | **F:** a ring buffer and memcpy in a `trusted fn` behind a proven safe API. **R:** raw pointer use outside `with(Unsafe)`. |
| 25 FFI → RocksDB | **F:** libc `write` via extern; the RocksDB slice-1 acceptance. **R:** a foreign call without its declared premise. |
| 26 floats | **F:** mean/variance with IEEE semantics. **R:** a NaN or out-of-range float converted to an integer without `try_convert`. |
| 27 network | **F:** httpget, tcpserve (legacy workloads 7–8), and the sequential static web server. **R:** a network call without its grant. |
| 28 concurrency → ethlambda and the encrypted tunnel | **F:** concurrency examples once its model is decided, then ethlambda and the encrypted tunnel after the randomness/nonce audit. |

## Reviewer changes (round 1: all three agree with changes)

- Split slices 21–23 by container: HeapArray (21), Vec and String (22), OrderedMap and HashMap (23).
  wordfreq and envcfg move to 23. (Applied in the draft above.)
- Keep `cat`, `wc` and file-copy on fixed stack buffers with zero heap. png_chunks waits for its
  containers.
- Grow a few recurring programs across slices instead of replacing each slice's example. Codex
  proposes arithmetic, token and byte-codec families; Kimi proposes a triangle sum and a byte codec.
- Floats (26): claim bit-exact IEEE behaviour, including NaN, infinities and signed zero, never
  real-number equality. Replace the invented "float where integers required" refusal with real
  conversion errors. (Applied in the draft above.)
- We propose that every slice adds an adversarial X case and states its facts claim separately from its theorem.
- RocksDB and ethlambda need their acceptance-program audit against the decided language (R-0604)
  before they are accepted.
