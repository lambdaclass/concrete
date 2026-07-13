# Alignment Facts

Status: design analysis / not implemented. Tracked by ROADMAP Phase 15 #6a.

Question: should Concrete make alignment first-class, and if so should it be
part of the type system?

Short answer: yes, alignment should become a first-class *fact* for backend,
ABI, allocator, SIMD, and embedded work. It should not become a separate
ordinary value type for every `T`; it should be modeled as layout facts on
types/objects and as refinement facts on pointers, references, slices, and
places.

## Why this belongs in Concrete

Concrete already treats layout, ABI, target assumptions, allocator behavior, and
audit reports as language-facing facts. Alignment fits that model:

- Some targets make misalignment a correctness issue.
- SIMD/SIMT and DMA/MMIO code often need stronger-than-natural alignment for
  performance or instruction legality.
- FFI and generated glue need to report size, alignment, offsets, and calling
  convention assumptions together.
- An optimizer cannot safely use an aligned operation unless the source,
  allocator, or a checked assertion establishes the fact.

The important distinction is that there are three different questions:

- **Type/layout alignment:** the natural ABI alignment of `T`, or an explicitly
  over-aligned aggregate type.
- **Object/place alignment:** the actual alignment guaranteed for this storage
  location.
- **Pointer/reference/slice alignment:** the alignment known for this address
  value after allocation, field projection, indexing, slicing, pointer
  arithmetic, or a runtime check.

Only the first is naturally a property of the type. The other two are facts about
storage and addresses.

## Comparison

### Zig

Zig is the strongest mainstream reference for the feature shape. Zig documents
that every type has an alignment, but pointer types may explicitly specify an
alignment; stronger-aligned pointers coerce to weaker-aligned pointers, not the
other way around. Variables, functions, pointers, and slices can carry alignment,
and `@alignCast` refines a pointer/slice with a safety check.

This is close to the model Concrete should study, especially:

- `*align(N) T` / `[]align(N) T` as pointer/slice facts.
- Larger alignment is a subtype of smaller alignment.
- Runtime checked refinement exists and is explicit.

Concrete should not copy Zig syntax mechanically, but Zig proves the feature is
tractable in a systems language.

Source: <https://ziglang.org/documentation/master/#Alignment>

### Rust

Rust treats layout alignment seriously but mostly at the type/representation
level. The Rust Reference defines layout as size, alignment, and field offsets;
`repr(align(N))` raises alignment and `repr(packed(N))` lowers it for composite
types. Rust references/pointers do not generally carry a source-level
`aligned-to-N` type parameter in ordinary APIs.

Rust's lesson for Concrete: layout alignment belongs in reports and ABI rules,
but wrapper types such as `#[repr(align(64))] struct Align64<T>(T);` are a coarse
way to express "this pointed-to value is aligned." They are useful, but they do
not solve alignment facts for slices, offsets, field projections, or runtime
refinements.

Sources:
- <https://doc.rust-lang.org/reference/type-layout.html>
- <https://doc.rust-lang.org/reference/type-layout.html#the-alignment-modifiers>

### Odin

Odin exposes custom alignment as a declaration tag for structs and unions
(`#align(N)`) and provides `align_of(T)` for allocation/layout code. This is a
practical low-ceremony layout feature, not a general pointer-alignment type
system.

Odin's lesson for Concrete: declaration-level alignment should stay simple and
visible, but it is not enough for passing "this slice is 32-byte aligned" across
function boundaries.

Source: <https://odin-lang.org/docs/overview/#align>

### C

C has object/type alignment concepts and C11/C23 alignment specifiers. Alignment
can be queried with `_Alignof`/`alignof`; `_Alignas`/`alignas` can strengthen an
object declaration, subject to restrictions. C does not let function parameter
types express "this pointer value is aligned to N" as a checked type contract;
that usually falls to attributes, builtins, convention, or optimizer-specific
assumptions.

C's lesson for Concrete: object alignment is necessary for ABI compatibility,
but an ad hoc attribute-only model is too easy to make semantically dark.

Sources:
- <https://en.cppreference.com/w/c/language/object>
- <https://en.cppreference.com/w/c/language/_Alignas>

### C++

C++ `alignas` can apply to class declarations, non-bitfield data members, and
variables, but not function parameters. Like C, this gives strong type/object
layout control but does not make pointer alignment a normal function-contract
fact.

C++'s lesson for Concrete: over-aligned types are useful for storage ownership
and ABI shape, but they do not replace aligned references/slices for hot loops or
backend metadata.

Source: <https://en.cppreference.com/w/cpp/language/alignas>

### SPARK/Ada

Ada has representation attributes including `Alignment`, and alignment may be
specified for first subtypes and stand-alone objects. The Ada Reference Manual is
explicit that `S'Alignment` is subtype-specific and that conflicting address and
alignment clauses can make execution erroneous. This is the most relevant
comparison for Concrete's assurance/audit culture: representation choices are
visible, specifiable, and tied to implementation support.

SPARK inherits Ada's representation vocabulary but adds verification discipline.
The useful lesson is not "make alignment a proof theorem immediately"; it is
"make representation facts explicit enough that tools can either verify,
runtime-check, or name the remaining assumption."

Source: <https://www.adaic.org/resources/add_content/standards/05rm/html/RM-13-3.html>

## Proposed Concrete model

### 1. Keep ordinary values simple

Do not make `T` and `align(32) T` two unrelated ordinary value types. That would
cause API and generic duplication while solving the wrong problem: a scalar value
does not become semantically different because one storage location has stronger
alignment.

### 2. Add layout alignment for declarations

Concrete should eventually support declaration-level alignment for ABI/layout:

```concrete
#[repr(align(64))]
struct CacheLine {
    bytes: [u8; 64],
}
```

Rules to decide later:

- power-of-two only;
- target maximum from the target profile;
- cannot weaken natural alignment except via an explicit packed/layout feature;
- appears in `--report layout`;
- included in C/ABI glue and layout round-trip tests.

### 3. Add pointer/reference/slice alignment facts

The useful function-boundary feature is an alignment fact on address-carrying
values:

```concrete
fn sum(xs: &[f32] where aligned(xs, 32)) -> f32
```

or, if syntax pressure later favors a type-like spelling:

```concrete
fn sum(xs: &align(32) [f32]) -> f32
```

The semantic model should be the same either way: `aligned(place, N)` is a fact
about the address value, not a distinct numeric element type.

### 4. Stronger alignment flows to weaker requirements

For power-of-two alignments, `aligned(p, 64)` satisfies `aligned(p, 32)`,
`aligned(p, 16)`, and so on. The reverse requires proof or a checked cast.

This should be explicit in type checking / fact checking, not left as optimizer
metadata.

### 5. Checked refinement

Concrete should provide a checked refinement operation before any unchecked
alignment assertion:

```concrete
let ys = align_check(xs, 32)?;
```

The result carries `aligned(ys, 32)`. If Concrete later allows an unchecked form,
it must be visibly `unsafe`/trusted and reported in audit.

### 6. Fact propagation rules

The design must define these before implementation:

- `alloc(layout(size, align))` returns a pointer with that alignment if the
  allocator contract says so.
- `&x` carries at least `align_of(type_of(x))`, or the declared object alignment
  if stronger.
- Field projection derives alignment from base address plus field offset.
- Array indexing/slicing weakens alignment according to element stride and byte
  offset.
- Pointer arithmetic weakens alignment by the offset; for byte offsets, the
  conservative rule is based on the greatest common divisor of the old alignment
  and the offset.
- Casts preserve, weaken, check, or drop alignment facts explicitly.
- FFI imports either declare alignment facts or are target/ABI-trusted.

### 7. Reports and backend lowering

Alignment facts must be visible in:

- `--report layout`: type/object alignment, field offsets, packed/over-aligned
  decisions;
- `--report abi`: parameter/return alignment assumptions crossing FFI;
- `--report audit`: checked versus trusted alignment refinements;
- backend IR emission: alignment metadata only when backed by a Concrete fact.

## Decision criteria

Implement alignment facts only when at least one forcing workload needs them:

- SIMD/autovectorized byte/float slice processing;
- C ABI glue requiring over-aligned structs or buffers;
- freestanding/embedded DMA/MMIO buffers;
- allocator APIs that must state and preserve layout contracts;
- a compiled-oracle differential test where missing alignment facts block a
  legitimate optimization or hide a target assumption.

Do not implement them merely because the feature is elegant.

## Recommendation

Add this to Phase 15 as a backend/target/layout contract item:

1. First implement reporting and declaration-level layout alignment.
2. Then add pointer/reference/slice alignment facts if a real workload requires
   function-boundary guarantees.
3. Only then consider syntax. Prefer a fact/contract spelling unless repeated
   programs show that a type-like spelling is substantially clearer.

This keeps Concrete aligned with its core rule: if source code relies on a target
or layout fact, that fact must be explicit, checked where possible, and reported
where trust remains.
