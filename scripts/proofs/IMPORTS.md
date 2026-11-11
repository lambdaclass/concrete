# Direct proof imports

Direct proof imports are checked by `imports.py` inside the required trust-ban
job. It elaborates complete proof commands (naming anonymous examples) and
compares imports with the modules defining their directly used constants.
Controls verify rejection of unused imports and of direct imports omitted in
favor of transitive visibility. Both import gates share the lexer header parser;
`imports.py --write` preserves comments while updating import commands. Each
build and elaboration uses the bounded runner and forwards failure diagnostics.

Proof modules are enumerated recursively, with an empty population rejected.
The inspector runs separately from source namespaces. Write mode keeps
surviving import commands and their comments in place, preserves line endings,
and is idempotent. The controls exercise these properties.

The source probe is compiled without importing Lean or changing any source
options. A separate inspector imports the resulting `.olean`, with Lean's
extensions loaded, and reads direct constant references. Reserved names are
resolved to their enclosing declaration using `isReservedName` on the user name;
other compiler helpers without declaration ranges walk to their enclosing
source declaration. Private realization prefixes are stripped with
`privateToUserName` when their enclosing declaration is not under that prefix.
Thus a helper belongs to its definition's module, irrespective of where Lean
first realized it. The equation-lemma control compares the same body with and
without the realizing proof import and rejects the superfluous import.

Anonymous examples must be named to retain their terms in an artifact. They
become noncomputable abbreviations, which accept both proposition and data
values without changing any linter option. These fresh names are never referenced
by the original source. Every current proof file also compares the probe's
named-declaration dependency set with the real build's `.olean` dependency set,
excluding synthetic examples and their helpers. A mismatch fails the gate. This
checks the retained source context against the actual build, while the example
dependencies additionally participate in exact-import checking.
