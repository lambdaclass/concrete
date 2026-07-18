# Construction Rights & Representation Privacy

**Status:** authoritative design (defect-queue 0b). Slice 1 (newtype
construction rights) AND slice 2 (struct field privacy: cross-module
literal/read/write rejected — E0297/E0298; `pub` fields opt in; std migrated
to accessors/from_raw_unchecked) implemented. Enum-variant visibility +
newtype `.0` unwrap privacy remain the scheduled follow-on slice.

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
module keeps construction/field/variant authority over its own types unless it
marks that exact surface `pub`. `pub` is the only visibility word: there is no
second `private`, `sealed`, `opaque`, `transparent`, or friend-module tier.

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
| **Newtype unwrap to inner**                 | ✓           | target: ✗ (scheduled follow-on; use an accessor fn) |
| Enum: construct/match a variant             | ✓           | only when the variant is `pub` (scheduled follow-on; currently names remain temporarily matchable) |
| Enum: bind a variant payload                | ✓           | follows the variant's visibility as one unit in v1 |

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

`NonZeroU32(v)` construction is private to `numeric.con`; raw payload
projection/unwrap privacy is the scheduled follow-on. Ordinary external code
goes through `try_new`/`get`. If a wrapper is intentionally transparent, its
defining module exposes explicit public construction/access operations;
Concrete does not add a second `transparent` visibility word.

## Enums (scheduled follow-on)

`pub enum T` exports the type name. A variant becomes externally constructible
and matchable only when the variant itself is marked `pub`, for example
`pub Ok { value: T }`. The marker governs the whole payload in v1; there is no
second payload-field visibility dimension. Until this migration lands, variant
names retain the documented temporary always-matchable behavior. External
matches that cannot see every variant will require a wildcard, and imports,
re-exports, exhaustiveness, diagnostics, and interface hashes must preserve the
defining module's visibility decision.

## Non-goals / recorded limits

- Not secrecy, not a capability, not proof evidence (restated above).
- Visibility is MODULE-granular (Concrete's unit of encapsulation), not
  type- or function-granular.
- Representation-transparent wrappers use explicit public operations; another
  visibility keyword is not reserved or deferred.

## Implementation order (user-directed)

1. Direct newtype construction privacy and the `NonZeroU32(0)` regression: done.
2. Cross-module struct literal/read/write enforcement (E0297/E0298), public
   positive fixture, and stdlib accessor/raw-adapter migration: done.
3. Record update/destructure, external-impl non-privilege, transitive
   private-type leakage, re-export preservation, and generated-operation/API
   snapshot closure gates: active roadmap work.
4. Private-by-default enum variants, raw newtype payload projection,
   exhaustiveness/import migration, and interpreter/backend parity: active
   roadmap work.
5. Manifest/compiler-interface facts and mutation tests for every remaining
   enforcement path: land with the corresponding active slices.
