### Language specification: docs/verification cites the spec (2026-10-30)

The language-bearing documents of `docs/verification/` (SAFETY, MEMORY_GUARANTEES, CONTRACTS_GUIDE,
CONTRACTS_AND_VCS, GUARANTEE_STATEMENT, CLAIMS_TODAY, TRUSTED_BOUNDARY_GUIDE, THREAT_MODEL,
PROVABLE_SUBSET, KNOWN_HOLES, MEMORY_REGRESSION_CHECKLIST, EFFECT_PROOF_BOUNDARIES,
PROOF_SEMANTICS_BOUNDARY, INTERPRETER_TRUST, TRUSTED_COMPUTING_BASE) now cite rule IDs instead of
restating them, and every KNOWN_HOLES entry names the rule it is against. Stale statements are
corrected: `&mut` is lent per call (REF-10), not consumed; shadowing is refused (SCOPE-1); a
trusted body calls a plain `extern fn` without `Unsafe` (TRUST-5); defer takes one call, not a body;
the TRUSTED_BOUNDARY_GUIDE FFI example follows FFI-3, FFI-9, TYPE-4 and OWN-7, and forms still
pending (`ptr::offset`, `trusted mod`, the counted loop, `u8` main, `field: _`) are marked pending.
New entries: CON-5 (a `spec fn` is erased), CON-6 (`assert` and `assume`), CON-7 (`result` only in
an `ensures`), CON-8 (stacked clauses), MEM-7 (the heap builtins need `Alloc`), MEM-8 (indexing
through a reference), FFI-9 (what `trusted extern fn` asserts), TRUST-13 (a reference converts to a
raw pointer without trust), TYPE-22 (a constant is evaluated at each use), ARITH-12 (float `%` is
`fmod`) and OWN-17 (a deferred call's result is dropped), with an OWN-5 example for a deferred
linear result. Open placeholders OPEN-115 to OPEN-121: a `spec fn`'s body and totality, a
`total fn` category, `old`/quantifiers/implication, proven violations in trusted code, output on
abort, vacuous contracts, and projections from an untraceable base.
