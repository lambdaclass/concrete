# Bug 081 — a call's name in the proof model was not bound to the function it calls, so an import alias could borrow a proof

<!-- bug-ledger
summary: the proof model's `.call "k"` means the theorem table's entry under `k`, but edges were justified by table CONTAINMENT, so `import main.{ check_version as check_class }` borrowed `validate_header_correct`; every call is now bound by key to the callee the elaborator resolved, and the stale-fingerprint warning prints the v2 value to pin
coverage: the proof model calls by table KEY but edges were justified by table containment, so an import alias (check_version as check_class) borrowed validate_header_correct and minted. Regression vehicle is a gate: check_subject_binding.sh legs X refuse and do not mint tests/programs/bug081_alias_key (and its colliding-definition variant) while the unaliased elf_header claims stay proved and minted; see docs/bugs/081 (2026-10-18)
class: 1
class: 9
silent: yes
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `bug081-callee-key`, on the unpublished bug-080 head `00efbaf4`)
**Found:** 2026-10-18 by a name-resolution hunt, confirmed by running it.
**Severity:** a false proof that mints a receipt. Copy `examples/elf_header`, and in a module
`evil` write `import main.{ check_magic, check_version as check_class, check_data, check_version };`
followed by `validate_header` with its original body, `#[spec(Concrete.Proof.validateHeaderExpr)]`
and `#[proof_by(Examples.ElfHeader.Proofs.validate_header_correct)]`. `--report proof-status` said
`proved`, although the function calls `check_version` where the theorem's model calls `check_class`:
for a valid class 2 header it returns 0 where `validateHeaderExpr` says 1.

## Mechanism

The extracted body keeps a call's LOCAL spelling — an import alias included — so `check_class(cls)`
extracts as `.call "check_class"`. A specification calls by name as well: `.call "check_class"`
means whatever the theorem's `FnTable` stores under the key `check_class`. The subject binding
(bug 079) compares the two bodies' digests, and they are equal — spelling for spelling.

The only thing that relates a call to a definition is the dependency-edge check
(`correspondenceInputOf`), and it asked a different question: is the resolved callee CONTAINED in
some table the theorem names (`scopedEvidenceContains`, `rows.any sameDefinition`)? The edge to
`check_version` was justified by `elfFns`'s own `check_version` entry. Nothing asked which KEY holds
it, so nothing noticed that the call spelled `check_class` reaches `check_version` while the model's
`check_class` is `main.check_class`.

A second resolver made it worse: when a `check_class` is also defined, the call graph resolves the
spelling to that definition (first global bare-name match) instead of to the import, so the edge set
it reports is not what the elaborator resolved.

## Fixed

- **Elab records what each call spelling reaches** (`CModule.callTargets`): every direct-call
  spelling in a module's bodies, resolved by the same resolver that gives evidence its callee
  identities, threaded to `ProofCoreEntry.callTargets`.
- **Scoped table evidence carries the key** (`ScopedEntryEvidence.operationalKey`, the attested
  model's `operationalKey`), for in-build tables and for the generated external rows
  (`externalScopedEntries` is now a 6-tuple; `refresh_classifications.sh` emits it).
- **Every call is bound by key** in `correspondenceInputOf`: for each `.call k` of a claim's extracted
  body, the elaborator's callee for `k` must be an edge the call graph records, and in every table the
  theorem names that holds `k`, the key must select one model attested to that exact definition; some
  named table must hold `k`. Otherwise a named `CallKeyRefusal` (`unresolved`, `unscoped`,
  `notAnEdge`, `keyUnbound`, `keyBindsOther`, `keyAmbiguous`) enters the join's `malformed` set, and
  the claim is `correspondence_unjustified` (reported as `dependency closure unjustified`, naming the
  call). Issuance refuses through the same status (`claim_not_proved`), and consumption requires
  `proved`.
- **Companion diagnostic fix:** the stale-fingerprint warning printed `shortHash currentFp`, a hash
  of the V1 body fingerprint, as the "current" value — not a value any `#[proof_fingerprint]` can hold.
  It now prints `v2:` + the short subject digest, the value `--report migration-manifest` reports as
  `current`.

A trusted callee is not bound by key: its edge is justified by the declared boundary, and the claim
discloses `assumes <callee>` naming the definition the call REACHES. `composition_trusted_helper`'s
trusted `dbl` is not the definition `combineFns` attests under `dbl`; that difference is what it
trusts. One model attested to several definitions (`cryptoFns` attests `check_nonce` once per
consuming package) binds whichever of them the call reaches.

## Prevention

`check_subject_binding.sh` legs X: the fixture `tests/programs/bug081_alias_key` (the attack, made
self-contained under the `elf_header` package name so its checks are attested) is fresh and bound,
refused as unjustified with the call named, and not minted, while its unaliased claims and the
unaliased `examples/elf_header` `validate_header` stay proved and minted; the colliding-definition
variant is refused too; and the stale warning's value equals migration-manifest's `current`.

## Found alongside, not fixed here

With a function named `check_class` defined in another module, `import a.{ check_version as
check_class }` is IGNORED by codegen and the interpreter: the call runs `a.check_class`
(`EmitSSA` prefers any defined name over the module's local alias). The elaborator, the evidence body
and now the key check resolve it to `check_version`, so the proof side refuses rather than agreeing
with codegen; the miscompile itself is a separate wrong-code bug in the bug-039 family.
