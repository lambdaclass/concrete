# Direct proof imports

Direct proof imports are checked by `imports.py` inside the required trust-ban
job. It elaborates complete proof commands (naming anonymous examples) and
compares imports with the modules defining their directly used constants.
Controls verify rejection of unused imports and of direct imports omitted in
favor of transitive visibility. Both import gates share the lexer header parser;
`imports.py --write` preserves comments while updating import commands. Each
build and elaboration uses the bounded runner and forwards failure diagnostics.

Proof modules are enumerated recursively, with an empty population rejected.
The probe uses root-qualified names even inside open namespaces. Write mode keeps
surviving import commands and their comments in place, preserves line endings,
and is idempotent. The controls exercise these properties.
