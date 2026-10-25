# Bug 064: `--query KIND` rejects a kind and then lists that same kind as known, so the diagnostic contains no derivable next action

<!-- bug-ledger
summary: every bare semantic `--query KIND` exits 1 with "unknown query kind 'predictable'" and then lists `predictable` first among known kinds; the arity is wrong, not the kind, so no repair is derivable from the message (R-0471, surface audit 2026-07-31)
coverage: a bare semantic --query KIND is rejected as unknown while the message lists that same kind as known (arity is wrong, not spelling). CLI/report diagnostic, not a .con defect: the regression vehicle is the query leg of a gate script (every knownQueryKinds/knownFactKinds member in both bare and argument form, asserting the message never lists the rejected token), not a program fixture. No gate yet
class: 12
-->

**Status:** Open
**Discovered:** 2026-07-31, while auditing the user-exposed surface for
under-gated features. Reproduced on all five bare semantic kinds.

## Symptom

Every bare semantic query kind exits 1 with a self-contradictory message. The
rejected token is the *first* entry in the list of known kinds:

```
$ concrete examples/elf_header/src/main.con --query predictable
error: unknown query kind 'predictable'. Known kinds: predictable, proof, evidence,
audit, fn, why-capability, traceability (semantic), proof_diagnostic,
predictable_violation, proof_status, eligibility, obligation, extraction,
traceability, effects, capability, unsafe, alloc, contract (fact filter)
```

Reproduced identically for `proof`, `evidence`, `audit`, and `fn` — the five
kinds in `knownQueryKinds` that are not also in `knownFactKinds`. All five are
reachable from the help page, which advertises
`<file> --query <kind>    semantic queries (why-capability, evidence, …)`
(`Main.lean:69`) — so the documented spelling is the one that fails.

`traceability` does not reproduce, for the wrong reason: it happens to appear in
*both* kind lists, so the bare form is served by the fact filter.

## Root cause

Two lists with overlapping membership, and a validity check that consults one
while the error message renders both.

- `knownQueryKinds` (`Concrete/Report/Report.lean:3431`) — semantic kinds:
  `predictable`, `proof`, `evidence`, `audit`, `fn`, `why-capability`,
  `traceability`. Every one of these *requires* an argument
  (`predictable:FN`, `why-capability:FN:CAP`).
- `knownFactKinds` (`:3425`) — fact-filter kinds, valid bare.

The single-word branch validates against `knownFactKinds` only:

```lean
if knownFactKinds.contains query then ... else
  .error s!"unknown query kind '{query}'. Known kinds: {allKindsStr}"
```
(`Concrete/Report/Report.lean:4430`)

while `allKindsStr` is built from **both** lists (`:4383`):

```lean
let allKindsStr := s!"{", ".intercalate knownQueryKinds} (semantic), {", ".intercalate knownFactKinds} (fact filter)"
```

So the rejection is correct — a bare `predictable` really is not a valid query —
but the stated *reason* is false. The kind is known; its arity is wrong. The
message asserts both, and offers nothing the reader can act on.

## Severity

This is a diagnostic-quality defect with no wrong-code component: the exit code
is 1, no report is produced, and nothing enters the evidence ledger. It is filed
as a numbered bug rather than a polish item because of who reads it.

Concrete's authoring consumer is a language model, which cannot fall back on
guessing the way a human reader can. A message that names a token as unknown and
simultaneously displays that token among the known set leaves no repair to
derive; the repair loop cannot close from the diagnostic alone. That is the
failure mode R-0464's repair gate is built to measure, and this is its first
known instance.

Two further explicitness defects sit in the same code, and a fix should not
leave them:

1. **`traceability` names two different features.** `--query traceability` is a
   fact filter served by `queryFacts`; `--query traceability:FN` is a separate
   backend-requiring path in `Main.lean:456` that monomorphizes and lowers
   before answering. One name, two behaviours, no signal at the surface which
   one ran.
2. **The error cannot distinguish arity from spelling.** `--query predictabl`
   (a typo) and `--query predictable` (right kind, missing argument) produce the
   same sentence, though they need opposite repairs.

## Candidate fix

Split the rejection into the two cases the code already has the facts to
separate, before the `knownFactKinds` check:

- `query ∈ knownQueryKinds` and bare → *"'predictable' is a semantic query kind
  and requires a function: `--query predictable:FUNCTION`"*. For
  `why-capability`, name both arguments.
- otherwise → keep "unknown query kind", but list only the kinds valid in the
  position the user actually used, i.e. `knownFactKinds` for the bare form.

Then give `traceability` one meaning at the surface, or two names.

Gate: extend the query leg so every kind in `knownQueryKinds` and every kind in
`knownFactKinds` is invoked in both bare and argument forms, asserting the exit
code and that the message never lists the rejected token as known. The
class-level guard is the second half of that sentence — it catches any future
kind added to one list and not the other, which is the mechanism that produced
this bug. A mutation restoring `allKindsStr` in the bare-form branch must make
the gate fail.
