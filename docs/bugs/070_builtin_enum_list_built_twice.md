# Bug 070 — `builtinEnumList` is constructed in two places

<!-- bug-ledger
summary: `builtinEnumList` built twice and `hasUserResult` computed from different inputs, so Elab and Check held different beliefs about a user `Result` (fixed 2026-08-02; **renumbered from 065**, which a second, unrelated bug already held)
coverage: builtinEnumList built twice AND hasUserResult computed from different inputs (Elab consults imports, Check does not), so the passes hold different beliefs about a user Result; code divergence measured, no wrong-behaviour witness yet; gated by check_builtin_enum_owner.sh (four Result arrival paths: absent, local, imported-unaliased, imported-aliased) (2026-08-02 (renumbered from 065))
class: 12
area: Resolve/BuiltinEnums — the record's Fixed section; renumbered from 065, so commits naming 065 are another bug's
-->

> **Renumbered 065 -> 070 on 2026-09-20.** A second, unrelated bug was filed under
> 065 and the bug ledger keys by number, so one of the pair was silently discarded:
> the corpus is a bash associative array, where a repeated key overwrites rather than
> collides, and this entry lost. It was absent from the README index as well. Found by
> the status-agreement check added to `scripts/tests/audit_bug_corpus.sh`.

**Status:** FIXED 2026-08-02
**Found:** 2026-08-02, while inventorying type-ingress paths for R-0004.

## Symptom

The list of compiler-provided enums is built independently in two passes, with
the same conditional duplicated:

* `Concrete/Elab/Elab.lean:1632`
* `Concrete/Check/Check.lean:2121`

both as `[builtinOptionEnum] ++ (if hasUserResult then [] else [builtinResultEnum])`.

## Why it matters

One fact in two places, free to drift — the defect class R-0004 exists to close
(bugs 050, 054, 055, 056, 057, 061 are all instances). If the two ever disagree,
Check and Elab hold different beliefs about what `Result` is: a program could
type-check against one definition and elaborate against another, and nothing in
the compiler would report the disagreement, because neither pass can see the
other's list.

The `hasUserResult` shadowing rule makes divergence more likely than a plain
constant would: it is a CONDITIONAL, so a change to when a user `Result` shadows
the builtin has to be made twice.

## Fixed

One owner: `builtinOptionEnum`, `builtinResultEnum`, `hasUserResultEnum` and
`builtinEnums` now live in `Concrete/Resolve/BuiltinEnums.lean`, consumed by both passes.

Not in `Resolve/Intrinsic` (the obvious home, where `resultEnumName` lives): these
are `EnumDef` VALUES and AST imports Intrinsic, so it would cycle. Not in
`Frontend/AST` either, which was the first attempt — AST declares what a program
CAN say, whereas which builtins are in scope and when a user declaration shadows
one is construction POLICY. A dedicated module imports AST without cycling and
keeps the two kinds of fact apart.

`hasUserResultEnum` takes BOTH the module's enums and its imported ones, so the
input asymmetry is gone by construction rather than by both sites remembering.

## The inputs ALREADY differ — but no symptom is demonstrated

The two sites do not merely duplicate the list; they compute `hasUserResult` from
DIFFERENT inputs:

```
Elab.lean:1630   m.enums.any (== Result) || imports.enums.any (== Result)
Check.lean:2120  m.enums.any (== Result)                    -- imports NOT consulted
```

So an IMPORTED user `Result` suppresses the builtin in Elab and does not in
Check. That is a genuine disagreement in the code.

**It is not yet a demonstrated defect.** A program importing a user `Result`
compiles cleanly, so the differing belief does not surface as wrong behaviour in
the obvious case — probably because a duplicate name in `allEnums` resolves to
the first entry and the two orders happen to agree here. A witness would need a
case where resolution order or exhaustiveness actually depends on which `Result`
is present.

Recorded this way deliberately: "the inputs differ" is measured, "it produces a
wrong answer" is not, and stating the second would be the overreach this bug is
an instance of.
