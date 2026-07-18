# Construction Rights & Representation Privacy

**Status:** authoritative design (defect-queue 0b). Slice 1 (newtype
construction rights) implemented; slice 2 (struct field privacy) staged.

## Why

Smart constructors are only as strong as the language's construction
rules. Verified 2026-07-18: safe cross-module code could write
`NonZeroU32(0)` — bypassing `try_new`'s zero-rejection — and could read
and write another module's struct fields (`b.len`, `b.len = 999`).
Pointer-bearing fields were only *accidentally* protected (the
int-to-pointer cast needs `Unsafe`); the language enforced nothing. So
every invariant a stdlib type maintains — including the kernel-proved
`NonZeroU32` non-zero property — was advisory. Representation privacy
makes those invariants load-bearing.

**Privacy protects invariants. It is NOT cryptographic secrecy and NOT
proof evidence** — a `trusted` caller can still do unsafe things; it just
cannot silently claim representation authority over another module's type.

## Model: private-by-default representation, explicit public exposure

`pub` on a type exports the **type name**, not its representation. A
module keeps construction/field authority over its own types unless it
explicitly opts fields (or, later, whole newtypes) into the public
contract.

## Visibility matrix

| Operation                                   | Same module | Other module |
|---------------------------------------------|-------------|--------------|
| Name the type (`let x: T`, generic arg)     | ✓           | ✓ (if `pub`) |
| Call a `pub fn` / method on it              | ✓           | ✓            |
| **Struct literal** `T { f: … }`             | ✓           | only if every field is `pub` and T has no inaccessible required field |
| **Field read** `x.f`                        | ✓           | only if `f` is `pub` |
| **Field write** `x.f = …`                   | ✓           | only if `f` is `pub` |
| **Record update** `T { ..x, f: … }`         | ✓           | only if every named/required field is `pub` |
| **Pattern / destructure** `let T { f } = x` | ✓           | only if every bound field is `pub` |
| **Newtype construct** `N(v)`                | ✓           | ✗ (private; use the module's constructor fn) |
| **Newtype unwrap to inner**                 | ✓           | ✗ (use an accessor fn) |
| Enum: match variant NAME                    | ✓           | ✓ |
| Enum: bind a variant PAYLOAD field          | ✓           | only if that payload field is `pub`/representation-public |

Rules that hold across the matrix:

- **Fields are private to the defining module by default.** `pub` before
  a field name adds it to the representation contract.
- **No friend-module or stdlib exemption.** std migrates its own
  cross-module field pokes to constructors / accessors / scoped mutation /
  audited raw-parts APIs — exactly like any other consumer.
- **Privacy applies in `trusted` callers too.** `trusted` grants unsafe
  *operations*, never representation authority over another module's type.
- Enforcement is identical in Resolve/Check/Elab, project mode, the
  interpreter, and the compiled path (fail-closed at Check; the later
  stages inherit).

## Newtypes (slice 1, implemented)

```
pub newtype NonZeroU32 = u32     // exports the nominal type
pub fn try_new(v: u32) -> Option<NonZeroU32>   // the public constructor
pub fn get(&self) -> u32                        // the public accessor
```

`NonZeroU32(v)` and unwrapping to `u32` are private to `numeric.con`.
External code goes through `try_new`/`get`. An intentionally transparent
wrapper would need a future explicit `pub transparent newtype` — NOT the
default (public newtypes are opaque in representation by default).

## Enums (slice 2)

Public enum and variant *names* stay matchable across modules (they are
the type's interface). A variant's payload *fields* follow the same field
visibility rules: a variant carrying private representation data cannot be
fabricated or destructured around that data from another module.

## Non-goals / recorded limits

- Not secrecy, not a capability, not proof evidence (restated above).
- Visibility is MODULE-granular (Concrete's unit of encapsulation), not
  type- or function-granular.
- `pub transparent newtype` is deferred until a workload needs a
  representation-transparent public wrapper.

## Implementation order (user-directed)

1. This doc + matrix. ✓
2. Negative fixtures (external construct/read/write/update/destructure). — slice-scoped
3. Positive fixtures (pub-field records, module-owned constructors).
4. Pin `NonZeroU32(0)` rejection specifically. ✓ (slice 1)
5. Enforce in Resolve/Check/Elab, project mode, interp, compiled.
6. Migrate std cross-module field pokes to APIs (no blanket exemption). — slice 2 (25 sites)
7. Manifest fields: type visibility / construction visibility / field
   visibility / representation class.
8. Mutation-test every enforcement point.
9. This doc's privacy-≠-secrecy note. ✓
