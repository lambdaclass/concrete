# F11 memory extraction disposition

Source: `origin/wip/spec-integration-head` at `aa44140a3`, complete
`ConcreteSpec/Spec/Memory.lean` and `ConcreteSpec/Proofs/Memory.lean`.
Historical approval annotations are discarded. This document records extraction scope,
not an independent review or approval. Normative meanings and examples are in Lean.

The following partition accounts for the complete source command population, including
anonymous examples, private helpers and audit definitions. No text-sliced theorem body
or dependent Boolean command is retained.

| Source group | Disposition |
|---|---|
| Spec imports and module documentation | Repair: explicit Ident dependency, current authority map and held scope. |
| Spec `MemBlockId` | Replace all occurrences with existing `ConcreteSpec.Core.BlockId` from Ident; retire the duplicate structure. |
| Spec blocks, memory invariants, allocation, release, access, load/store, exposure, reconstruction and address equality | Retain complete definitions using Basics association lookup/update and Target. |
| Spec integer octets, byte ordering and integer encoding/decoding | Retain using Scalar bit semantics; prove compatibility with current Rep. |
| Spec `encodeBool`, `decodeBool` | Owner-held, omitted with dependent documentation/examples; ISS-0069. |
| Spec pointer encoding/decoding | Retain whole-pointer bookkeeping and strict decoder, under recovered item 20; no physical layout extension. |
| Spec anonymous examples | Retain all non-Boolean cases; split the bundled target-failure example into independent address, space and allocation cases. Retire its redundant invalid-pointer test, which also failed numeric consistency; replace with an isolated validity control in Proofs. |
| Proof integer, pointer, memory preservation, allocation, access, exposure, framing and reconstruction commands | Retain complete commands and private support, replacing block identity; remove all inherited APPR annotations. Preserve law/helper/representation classifications without claiming they have received approval. |
| Proof `decodeBool_encodeBool`, `decodeBool_valid`, `badDecodeBool`, `badEncodeBool`, audit groups 3 and 4 | Owner-held with their complete dependent examples, omitted; ISS-0069. |
| Remaining proof Audit definitions and anonymous commands | Retain; repair constant-result integer/pointer/load/store/access/exposure mutants with arithmetic, guard, lookup or state-update faults. Keep premises with witnesses. |
| New compatibility evidence | `sliceIntTy`, `sliceByte`, `leBytes_rep`, `encodeInt_rep`: a checked representation view, including all eight integer types on the existing profile. |
| New isolated controls | Pointer width, fragment identity, numeric octets and target validity; access liveness, space, lower/upper bounds and alignment; adjacent-block reconstruction priority, exposure and zero-size exclusion. |

There are no local-variable fixtures or local-variable definitions in either source module.
All identities used by this piece are block identities; numeric local identity stays in Basics.
The memory model is independent of the capability base. Write permission and the prohibition
on borrow-derived exposure remain caller obligations, not newly implemented authority rules.

Carried authorities, all in [the current decisions](../decisions.md):

| Rule family | Authority |
|---|---|
| Allocation failure, conserved surviving state and separate availability contract | Q1.8–Q1.11 under `decided-2026-11-05-core` |
| Provenance, bounds, lifetime, alignment, initialization, write permission, transfer and frame premises; aligned non-null zero-byte exception | Q3.7, including its linked full row recommendation and note |
| Zero-sized allocations occupy no bytes | Q3.8 |
| Exposure; only live, exposed, nonempty candidates; containing before one-past; stale identities stay stale | Q3.9, including its full row note |
| Occupied-byte collisions, representable one-past address, ordinary address exhaustion, address equality grants no authority | Q3.10, including its full row note |
| Structured/raw representation boundary | Q3.11 and `foundation-q2` |
| Read-only address widths, pointer widths, byte order and spaces; existing profile unchanged | `foundation-q3`, OD126; reuse Target and Scalar |
| No pointer meaning for non-whole stored-pointer bytes | `recovered-29-20` |

Held scope and tracking:

| Held item | Reason and structural owner |
|---|---|
| Boolean byte encoding and Boolean-to-integer conversion | ISS-0069: size authority does not select a bit mapping. Existing Rep behavior remains unchanged. The owner and F33 checked-view migration own reconciliation. |
| Struct layout, padding and further whole-pointer physical representation extensions | ISS-XXXX (coordinator allocation pending): F12/F33 must obtain explicit owner answers before introducing these rules. The fragment tags here are provenance bookkeeping for stored whole pointers, not physical pointer-layout fields or a bytes-to-address API. Closure requires source-backed decisions and complete dependent laws/controls. |
| U+FEFF scope and bare CR in comments | ISS-0064: unresolved authority; no memory rule depends on them. |
| Five partial recovered items and the remaining recovery queue | ISS-0065: do not promote unshown or superseded proposals. |
| Path-character/normalization policy | ISS-0068: independent authority missing; no path/Span dependency. |
| Scalar-only constant restrictions and SPARK-style character preconditions | ISS-XXXX (coordinator allocation pending): owner authority is absent; excluded from F11, whose integer representation makes no constant-admission or character-conversion choice. Owners are the scalar/constant foundation pieces and decision recovery. |

Qualification must be run on the final unchanged tree. The harness owns commits and pushes;
this writer does not invoke review tools or create review notes. Root gate sandbox controls
exercise the repository's review/landing scripts only in their private test repositories.
