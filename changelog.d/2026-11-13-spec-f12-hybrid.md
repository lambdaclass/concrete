Add the F12 integer hybrid store and its complete adapted preservation/frame proof families.
Reuse Memory, Scalar, Exec and the existing Rep interface through checked integer adapters.
First materialization preserves typed reads; raw writes determine subsequent decoding;
active typed writes preserve trailing bytes and fragments. Laws carry positive instances
and premise-preserving planted controls, with historical approval claims removed.

Authority: docs/decisions.md#foundation-q2 (C4 hybrid contract), Q3.7/Q3.9/Q3.11,
#foundation-q3 and #od126 (integer representation); theorem tags follow
#decided-2026-11-13-proof-labels. Source: integration-head aa44140a3 Spec/Hybrid
and Proofs/Hybrid, adapted rather than importing the historical foundation.

Held: Boolean storage encoding (ISS-0069); struct layout, whole-pointer representation,
pointer-byte mapping and layout/padding extensions (ISS-0070). Explicit uninterpreted
trailing bytes do not select struct padding or pointer encoding. Character/unit layouts
are held under ISS-XXXX pending explicit storage-layout authority; this does not restrict scalar values or constant admission. U+FEFF and
bare CR (ISS-0064), queued partial/constant items (ISS-0065), paths (ISS-0068), character
precondition alternatives (ISS-0071), wiping (ISS-0072), startup/ABI (ISS-0073) remain
outside this piece. ISS-XXXX is pending coordinator allocation; no owner decision is chosen.

Round 2: replace the hybrid layout table with a partial adapter to Rep.scalarSize;
Boolean size remains one byte independently of the encoding hold. Reads and projection
invariants require a successful size query. Replace fixture-specific planted controls with
faulty storage dispatch, repeated allocation, unpublished projections, padding clearing,
projection confusion and stale structured-cache lookup implementations. Track character
and unit layout exclusions, authority gaps and restoration conditions in ISS-XXXX.
