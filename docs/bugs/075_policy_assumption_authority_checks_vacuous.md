# Bug 075 — the policy and assumption-file authority checks never saw a capability

**Status:** FIXED 2026-10-06 (branch `r0484-integrated`)
**Found:** 2026-10-06, when the R-0484 R10 report wording changed and both gates started
failing: the failures were caused by new wording, but reading them showed the checks had never
been able to see a declared capability at all.
**Severity:** a silent gate. `check_policy.sh` and `check_assumptions.sh` passed on every
example whatever the example declared, so a policy forbidding `Console` could not fail.

## Mechanism

Both gates scraped `--report caps` text, extracting only PARENTHESISED tokens after a colon
(`fn : (pure)`). A capability headline prints bare — `fn : Console` — so the extracted set never
contained a capability: every "forbidden ∩ used" and "used ⊆ required/allowed" check ran on an
empty set. A second defect hid behind it: ten policy/assumption files (and two docs) forbade
`Net`, which is not a capability (`Network` is), so those entries could never match even once
the parser worked. And `check_assumptions` printed `ok   authority` unconditionally
(`[ "$errs" -eq 0 ] || true`), next to its own failures.

## Fixed

- `scripts/tests/lib/authority_facts.py` reads declared capabilities from
  `--report diagnostics-json` (schema v2), and REFUSES (exit 2) malformed JSON, a missing or
  wrong schema version, no capability facts, or facts whose assumptions were not computed — never
  an empty capability set. Declared capabilities are the compiler-enforced, complete list
  (R-0484); incomplete call-graph coverage is reported beside each verdict.
- Policy names are validated against the compiler's own `validCaps`, and `Std` is expanded from
  `stdCaps` (both read from `Concrete/Frontend/AST.lean`); an unknown name is an error.
- `Net` corrected to `Network` in the ten files and both docs.
- When facts are unavailable the policy FAILS and is not evaluated further, so an empty set never
  prints `ok`.

## Regression

Controls in both gates, run through the real per-project checks: a declared `Console` that is
forbidden fails; an allowed/required `Console` passes; `Std` covers `Console`; a declared
`Console` missing from `required` fails; empty declarations pass and say so; an indirect call is
reported as incomplete coverage; an unknown name (`Net`) fails; a program whose facts cannot be
produced fails as "facts unavailable". `check_policy.sh` 21/0, `check_assumptions.sh` 11/0.
