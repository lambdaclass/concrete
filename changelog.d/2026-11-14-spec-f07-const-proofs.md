### F07: constant proofs

Import `Proofs/Const` into the specification root. Carry the complete C2 evaluation,
typing, failure, Copy, dependency sorting, cycle exclusion and order-independence
proof families against main's shared definitions. Thread closed integer/bool
admission through the declaration and fixed-point proofs; remove unit expressions,
Boolean conversion and inherited approval labels. Add focused kernel-checked
Instance/Planted controls, including unused overflow, exhaustion versus refusal,
closed types, forward references and competing valid dependency orders. Canonical
source tie-breaking is a Helper and implementation control, not a
new language decision. Only Law declarations count as evidence.

Authorities in docs/decisions.md: #recovered-29-18, #recovered-29-19,
#recovered-59-26, #decided-2026-11-08-constants,
#decided-2026-11-14-constant-types, #decided-2026-11-12-char and #od106;
evidence tags and controls: #decided-2026-11-13-proof-labels and
#decided-2026-11-14-trap-sites-and-law-tests.

Held: char/unit constants, calls and richer R2 preconditions (ISS-0080), SPARK
character preconditions (ISS-0071), Boolean encoding/conversion (ISS-0069), path
characters (ISS-0068), pointer bytes (ISS-0070), wiping (ISS-0072), startup/ABI
(ISS-0073), and layout (ISS-0076). U+FEFF scope, bare CR and queued partial items
are outside this constant grammar; no rule or approval is inferred for them.
