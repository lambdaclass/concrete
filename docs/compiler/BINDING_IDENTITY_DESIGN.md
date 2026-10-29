# Binding identities — scope, decided before editing

Status: compiler design record (moved from docs/language, which holds no language rules now: the
scope rules it serves are SCOPE-1 to SCOPE-3 of [docs/spec/scope.md](../spec/scope.md)).

Stable binding identities are the prerequisite for retiring `substExpr` as proof infrastructure
(H25) and for resolving the shadowing ambiguity (H27). They are also the kind of refactor that
becomes invasive quietly, so this note fixes the scope first. It answers seven questions that were
put to the design before any code was written; where the answer is "not yet", that is recorded as
an answer rather than deferred silently.

## The central decision: two notions, kept apart

The single most important choice is **not to build one identifier that serves both purposes.**

| | **Lexical binding ID** | **Stable evidence identity** |
|---|---|---|
| Purpose | capture-avoiding substitution | naming obligations in artifacts and fingerprints |
| Scope | unique within one elaborated function | canonical across a whole program and across time |
| Lifetime | exists during elaboration and VC generation | outlives the compiler run; appears in stored evidence |
| Must survive | nothing — it is regenerated every build | harmless edits: reformatting, comments, unrelated code moving |
| May contain | a counter, an allocation order, anything unique | nothing positional that a harmless edit perturbs |
| Alpha-renaming | irrelevant; IDs are *how* renaming is avoided | must be stable, or renaming a local invalidates stored evidence |

Trying to make one identifier do both forces a contradiction: substitution wants an identity that
is cheap, dense, and freely regenerated, while evidence wants one that is expensive to change and
survives edits. **Only the lexical binding ID is in scope for the substitution work.** Evidence
identity is a separate concern with a separate lifetime, and conflating them is the failure this
table exists to prevent.

## The seven questions

**1. Which nodes receive IDs?**
Binding *occurrences* and the references that resolve to them: parameters, `let`/`ghost let`
bindings, match-arm pattern bindings, `borrowIn` references, and loop-init bindings — plus every
`Expr.ident` that resolves to one. Types, fields, functions and constants do not: they are not
bound by an enclosing scope, so substitution cannot capture them.

**2. Where are IDs assigned?**
At the point that already computes scope — the checker's per-function environment. Assigning them
in the parser would be wrong: the parser does not know what a name resolves to, and an identity
that is not a resolution result is just a decorated string.

**3. Are they stable under formatting and alpha-renaming?**
The lexical ID is *deliberately not* required to be. It is regenerated each build and never
serialized, so formatting-stability is a property it does not need and should not pay for. Stable
evidence identity has the opposite requirement, which is precisely why they are separate.

**4. Compiler-internal, or evidence-artifact identities?**
Internal only. Nothing in this slice appears in an artifact, a fingerprint, or a stored evidence
record. If a lexical ID ever leaks into stored evidence, that is a defect, because it would make a
rebuild look like a different program.

**5. How do monomorphization and inlining transform them?**
Unanswered, and deliberately out of scope. Contract expressions are checked and substituted before
monomorphization, so the substitution work does not need the answer. It must be answered before
binding identities reach Core in a form later passes consume — recorded here so that dependency is
visible rather than discovered.

**6. Do source contracts and runtime expressions share identities?**
They must, for the cases where a contract names something a runtime expression also names — a
parameter, a ghost binding. That shared resolution is the point: it is what makes "which `x`" a
question with an answer. Contract-only names (`result`, and later `old(...)`) get identities from
the contract's own scope model, which is what H27's `result`-is-`ensures`-only rule already
approximates by name.

**7. Serialized?**
No. See 3 and 4. Not serializing is a design commitment, not an omission: it is what keeps the
lexical ID free to be regenerated.

## Sequence

Each step is independently committable and none depends on prover-specific code, so the series
stays extractable even though it lands on `spike/multi-kernel-theories`.

1. **Containment first** (done): reject contracts naming a shadowed parameter, and binder-bearing
   spec bodies. Both are temporary and say so in their diagnostics.
2. Resolve contract identifiers to lexical binding IDs in the checker; keep names alongside for
   diagnostics.
3. Carry resolved contract expressions into the typed contract record rather than re-reading AST
   metadata in each of the seven consumers.
4. Implement substitution over identities; prove or exhaustively gate the evaluation law
   `eval(subst(Q, x, e), rho) = eval(Q, rho[x |-> eval(e, rho)])` over the supported fragment.
5. Lift the H25 binder restriction and the H27 shadowing restriction — in that order, each with
   the negative controls that currently pin them flipped to positive assertions.

## What this does not do

It does not make contracts type-checked (H27 lists what is unchecked and each gap is pinned by a
gate). It does not touch monomorphization or inlining. It does not define evidence identity. Those
are named here so that "binding identities landed" cannot be read as more than it is.
