Restore the scalar foundation from the integration snapshot, with fixed-width arithmetic,
range proofs, machine/exact contract arithmetic and numeric contract variables from Basics.
Discard inherited proof approval claims; retain explicit law/helper/representation classifications
and premise-preserving controls. Replace constant-result arithmetic mutants with arithmetic faults.

Add the November 12 decided character API: code-point projection and Option-returning code-point
and ASCII construction, including scalar-boundary and surrogate controls. Shared scalar types and
values include characters without defining a constant-expression admission policy.

Hold the inherited Boolean bit encoding and Boolean-to-integer mapping: OD126 specifies size,
not the encoding, and OD11 does not choose a numeric map. Omit their dependent evidence rather
than treating source implementation as approval. Keep conversion/exact surface spellings,
SPARK-style char preconditions, floats, constant restrictions and unresolved source/path policy
out of this piece. Scalar.lean records the carried decision anchors and exact held scope.

ISS-0069 tracks the unresolved Boolean encoding and conversion authority, existing Rep
bytes (false=0, true=1, all other/uninitialized bytes invalid), structural owner and
owner-disposition closure condition. Scalar defers storage encoding to Rep and does
not infer a numeric conversion. No owner choice is made.

Add paired diagnostics fixtures: renaming presentations preserves scalar contract
results, while distinct numbers sharing a spelling retain distinct results. Each
fixture rejects a presentation-keyed valuation that respectively loses or aliases
a binding.
