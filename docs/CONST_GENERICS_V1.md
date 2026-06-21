# Narrow Const Generics V1 — decision & scoping (build deferred)

Status: DESIGN DECIDED, BUILD DEFERRED (2026-06-21) — ROADMAP Phase 5 #6a.
This doc fixes the V1 boundary so the feature is unambiguous when a workload
pulls it, and records the forcing probe that says *no current workload pulls it
yet*. Per the project's workload-driven (not symmetry-driven) discipline, no
compiler change is made until the probe's verdict flips.

## The question

Const generics let one type be parameterized by a compile-time *value*, not just
a type: `Buf<T, const N: u64>` with a `[T; N]` field, instantiated at
`Buf<u8, 256>` and `Buf<u8, 512>` as distinct, capacity-specific monomorphic
types. The roadmap calls them load-bearing for the no-allocation story —
`BoundedVec<T, N>`, `RingBuffer<T, N>`, `PacketBuf<N>`, fixed hash tables, parser
scratch buffers, embedded queues — so that fixed-capacity APIs don't need one
hand-written type per capacity.

The risk is the opposite: adding a large type-system feature because it is
*conventional*, not because a Concrete workload needs it.

## V1 boundary (what would be admitted, if built)

- **Syntax**: a const generic parameter in the existing generic list:
  ```
  struct Copy RingBuffer<T, const N: u64> { data: [T; N], head: u64, count: u64 }
  fn zeroed<const N: u64>() -> [u8; N] { return [0; N]; }
  ```
  `const N: u64` sits alongside type params (`<T, const N: u64>`). `N` is then a
  `u64`-typed value usable in array sizes (`[T; N]`) and in expressions/bounds
  inside the body.
- **Param types**: integers only in V1 (`u64`; `usize`-style). No `bool`, no
  enum, no struct, no string const params.
- **Arguments**: integer **literals** and **constant expressions the compiler can
  already fold** without general comptime execution — e.g. `RingBuffer<i32, 16>`,
  `PacketBuf<{ 64 * 2 }>` only if `64 * 2` is const-foldable by the existing
  evaluator. A const arg may also be a named top-level `const`. No call into
  arbitrary functions, no recursion, no comptime programs.
- **Monomorphization**: each distinct concrete `N` produces a **separate
  specialization**, exactly as a distinct type argument does today. `RingBuffer<
  i32, 16>` and `RingBuffer<i32, 64>` are different monomorphic types with
  capacity-specific layout. The mangled/monomorphic name **includes N**
  (e.g. `RingBuffer$i32$16`), so two capacities never collide in the symbol
  table, the layout report, or proof/evidence artifacts.
- **Ledger / layout / obligations name N**: the concrete `N` is recorded in the
  compiler ledger, the layout report (the array field's byte size is `N *
  sizeof(T)`, capacity-specific), every backend contract, and — critically —
  runtime-safety obligation ids and messages: an index obligation reads
  `index < 16` for the `N=16` instance, `index < 64` for `N=64`, never a symbolic
  `index < N`. Obligations are per-instantiation, like all monomorphic
  obligations.

## Explicitly rejected in V1 (and likely forever, absent a forcing case)

- **Type-level computation / dependent types**: no `[T; N+1]` arithmetic that
  must typecheck symbolically, no `where N > 0` dependent constraints, no proofs
  *about* N at the type level. N is a monomorphization knob, not a theorem
  variable.
- **Reflection**: no querying N's value as type metadata, no generated
  per-N methods, no `typeof`/size introspection beyond the existing layout report.
- **Comptime functions**: no arbitrary compile-time evaluation to produce N
  beyond the existing constant folder. If the folder can't already evaluate it,
  it is not a valid const arg.
- **Runtime-bound const params**: N is fixed at instantiation; a const param can
  never be bound to a runtime value (that is what `Vec<T>` + `Alloc` is for).

This keeps V1 to "the array-size literal becomes a generic knob," nothing more.

## Forcing probe (2026-06-21) — does any current workload need it?

Two empirical checks against the live tree:

1. **Can the workaround express it without const generics?** The fixed-capacity
   idiom (one struct, one literal-sized array, a runtime `len`) is what
   `examples/fixed_capacity` already uses: `MsgBuf { data: [u8; 256], len }`,
   `RingBuf { data: [i32; 16], head, count }`. It builds, runs, and proves. The
   alternative workaround — a type param standing in for the array
   (`Ring<A> { data: A }`, instantiated `Ring<[i32; 16]>`) — does **not** work
   today (array-literal element-type defaults don't unify with the param, and a
   generic `A` carries no size for indexing/length), so the *only* real
   workarounds are (a) one struct per capacity or (b) one struct at a single
   fixed capacity.

2. **Does any workload actually need one logical container at multiple
   capacities?** Surveying every fixed-size array in `examples/`: the sizes are
   **domain constants used once each** — SHA-256 `[u32; 8]` state / `[u32; 64]`
   schedule / `[u8; 64]` block, constant-time tag `[u8; 16]`, HTTP `[u8; 4096]`,
   `fixed_capacity` `[u8; 256]` + `[i32; 16]`. **No example instantiates the same
   container type at two different capacities.** Each fixed array is dictated by a
   protocol or algorithm and a literal `[T; N]` is exactly right; a const-generic
   parameter would add a knob nothing turns.

**Verdict: deferred.** The single-fixed-capacity workaround is *fine* for every
current workload because each needs exactly one capacity per type. The
duplication cost const generics removes (the same type at many capacities) is not
paid anywhere in the tree. Building it now would be symmetry-driven. The decision
above is fixed so that the day a workload genuinely needs `RingBuffer<T, N>` at
several N in one program — a parser scratch pool sized per grammar, an embedded
queue family, a fixed hash table at multiple load factors — the build is
unambiguous.

## Forcing conditions (when to build)

Marker: **DESIGNED / DEFERRED — workload-gated.** Build V1 when any ONE of these
appears in a real workload (not a symmetry argument):

- A program needs the **same** fixed-capacity container type at **two or more**
  capacities, and over-provisioning to a single max wastes meaningful stack/space
  or muddies the proof obligations.
- A stdlib fixed-capacity API (`BoundedVec`, `RingBuffer`, `PacketBuf`) is about
  to be written and would otherwise ship as N hand-duplicated types or a single
  hard-coded capacity that callers cannot choose.
- A no-alloc parser/buffer workload **duplicates types by size** (the same logic
  copy-pasted for `[u8; 64]` and `[u8; 384]`, etc.).
- Freestanding/embedded work needs reusable stack/static buffers parameterized by
  a caller-chosen compile-time capacity.
- Proof/runtime obligations need reusable **capacity-indexed** APIs (one proved
  container reused at several N, rather than re-proving per hand-specialized type).

## Implementation difficulty (for when the verdict flips)

Narrow V1 is **moderate-to-hard (~7/10)** — not a research feature, but it is
cross-cutting: it touches parser, type representation (types must carry *value*
parameters, not just type parameters), resolver/checker (N is a compile-time
integer, substituted into `[T; N]`), monomorphization (distinct specialization +
mangled name per concrete N), layout/backend (N resolved before size/offset; no
symbolic-N array reaches lowering), and obligation/report identity (bounds
obligations, layout reports, proof artifacts must embed the concrete N). The
**riskiest** parts are monomorphization + layout + obligation identity — that is
where a missing-N path causes silent symbol/evidence collisions or wrong layouts.
If V1 stays narrow it is feasible; if it drifts into general comptime or
type-level arithmetic it becomes a major language subsystem (hence the hard
exclusions above). The staged build is in Deliverables below.

## Deliverables — staged build (when the verdict flips; NOT built now)

Each stage lands behind tests before the next, mirroring how every Phase 5 item
shipped (build the gap, gate it, then proceed):

1. **Design** — this doc (done).
2. **Parse + type model** — `const N: u64` in the generic param list and const
   args in type application; types carry value parameters. Rejected *before use*
   (parse/resolve), with the V1-excluded forms (`Buf<n + 1>` non-foldable,
   `Buf<runtime_len>`, non-integer params, missing const args) producing clear
   diagnostics.
3. **`[T; N]` inside generic structs/functions** — substitute `N` into array
   sizes and body bounds expressions.
4. **Monomorphize by concrete literal N** — distinct specialization + mangled
   name per N (`Buf$u8$256` vs `Buf$u8$4096`); never collide.
5. **Layout/backend** — resolve `N` before size/offset; no symbolic-N array
   reaches lowering unspecialized.
6. **Obligation/report identity** — bounds obligations, layout reports, ledger,
   proof/evidence artifacts, and backend contracts all embed the concrete `N`
   (`index < 256`, not `index < N`) so evidence never collides or stales.
7. **Gate** — `examples/const_generics/{bounded_vec,ring_buffer,packet_buf}/` and
   `scripts/tests/check_const_generics_v1.sh` proving `PacketBuf<16>` vs
   `PacketBuf<32>` specialize to distinct layouts AND distinct obligations, and
   that the excluded non-integer/comptime/reflection forms are rejected.

The riskiest stages (4–6) are exactly where N must be threaded *everywhere* or
silent collisions/wrong layouts result — see Implementation difficulty above.

Until then this remains a recorded decision, not code — see KNOWN_HOLES.md
("unmade decisions / designed-deferred") and ROADMAP Phase 5 #6a.
