# Allocator As Value (13s — decision note, pre-Phase-7)

Status: DECISION — **GO, two-tier**. Written 2026-07-14, before Phase 7
collection APIs harden (retrofitting an allocator parameter later is an
H12/H17-scale burn-down; deciding now is the point).

Related packets: `arena-allocation.md` (the Arena value + `&mut` threading is
already allocator-identity-as-value), `allocation-budgets.md`,
`no-std-freestanding.md`, `languages/zig.md` (allocator-first precedent),
`docs/CALLABLE_VALUES_AND_CAPABILITIES.md` (fn ptr + explicit context = the
dispatch mechanism a polymorphic allocator needs).

## The split this decides

- `with(Alloc)` stays the AUTHORITY: "this function may allocate." Capability,
  checked at every call site, reported by `--report caps`. Unchanged.
- An `Allocator`-typed VALUE is the IDENTITY: "this allocation is owned by
  THAT allocator." Explicit parameter, linear/borrow rules apply, never a
  hidden global.

Authority without identity (today) cannot express: arenas feeding a parser,
test allocators that fail on leak, embedded fixed regions, hot-reload memory
that must outlive a reloaded library. Identity without authority would hide
effects. Both, orthogonal, is the model.

## Decision: GO — two tiers, identity is opt-in per signature

**Tier 1 (default, unchanged ergonomics):** collections and APIs that
allocate from the GLOBAL allocator keep their current signatures —
`Vec::new() with(Alloc)`. The global allocator is named (`std.alloc.Global`),
not hidden: reports attribute tier-1 allocations to it. We deliberately do
NOT adopt Zig's everything-takes-Allocator convention — that is the
"everything-bubbles" noise 13t warns about, transplanted to allocation.

**Tier 2 (explicit identity):** any API that should work against a
caller-chosen allocator takes it as the FIRST parameter, by `&mut` borrow:

```con
// the identity-carrying forms (sketches; H17/linearity rules as shipped)
fn vec_new_in<T>(a: &mut Arena) with(Alloc) -> Vec<T>
fn string_from_in(a: &mut Arena, s: &Text) with(Alloc) -> String
fn parse(a: &mut Arena, input: &Text) with(Alloc) -> Result<Ast, ParseErr>

// arena: already designed (arena-allocation.md) — linear value, bulk free
let mut arena: Arena = arena_new(4096);
let ast: Result<Ast, ParseErr> = parse(&mut arena, &input);
arena_free(arena);                      // linear: exactly once

// test allocator: same shape, different concrete type
let mut ta: TestAlloc = test_alloc_new();      // counts, fails-on-leak
let v: Vec<i32> = vec_new_in(&mut ta);
test_alloc_assert_balanced(ta);                // consumed: leak check IS the drop

// hot-reload / plugin-owned memory: the HOST passes the allocator whose
// lifetime it controls; the plugin cannot allocate anywhere else.
fn plugin_init(host_mem: &mut Arena, cfg: &Text) with(Alloc) -> PluginState
```

**Polymorphism strategy: monomorphic-first.** Tier-2 APIs are generic over the
allocator TYPE (`fn vec_new_in<A: Alloc>(a: &mut A)`) and monomorphize — no
dynamic dispatch, no vtable object in v1. If a workload later needs a runtime-
chosen allocator, the shipped callable-values model (context ptr + fn table,
caps at use site) is exactly the escape hatch — deferred until pulled.

Rules (all existing machinery, no new checker features):
- An allocator value is a linear resource (`Arena`) or a borrow of one
  (`&mut Arena`); Copy handles are out — identity must not silently duplicate.
- Never a hidden global: tier-2 signatures NAME the parameter; tier-1 names
  `Global` in reports. No ambient thread-local/context lookup.
- Values allocated from an arena borrow from it under the shipped borrow rules
  (references never returned; region tie via `borrow … in` where needed).

## Report fields (authority + identity together)

- `--report caps` (exists): `with(Alloc)` authority per function. Unchanged.
- `--report alloc` (exists): extend rows with an `allocator` column —
  `global` | `param:<name>` (tier-2) | `arena-local`. This is the identity
  surface an auditor reads next to the authority surface.
- telemetry: `alloc` counts already exist; no change needed for v1.

## Consequences for Phase 7 sequencing

1. Phase 7 collection APIs ship tier-1 signatures NOW without blocking on
   allocator work — the tier-2 `*_in` forms are additive, not retrofits
   (this is what makes the two-tier split retrofit-safe, the 13s worry).
2. Arena lands per `arena-allocation.md` when its workload pulls it; `*_in`
   variants are added alongside, generic over `A: Alloc`.
3. The `Alloc` trait-bound shape (`A: Alloc` with alloc/grow/free) is the one
   NEW design piece Phase 7 must pin before the first `*_in` API ships.

## Review fixture (design gate, not implementation)

The signatures above are the fixture: explicit allocator value, first-param
convention, `&mut` borrow (linear owner), no hidden global, monomorphic
dispatch, reports carrying both authority (`caps`) and identity (`alloc`
column). NO-GO was rejected because arenas/tests/hot-reload all demand
identity, and the only alternative (allocator baked into each collection type
statefully) makes identity invisible to reports and retrofits worse.
