### Resolve Checks Every Nested Submodule (bug 217, 2026-10-24)

Resolve walked only the modules of the top-level list, so a module nested two deep, a module written
after `main`, and every `mod x;` file of a project (std included) produced no Resolve diagnostics: a
cast to an unknown type there reached the backend (a panic natively; `--interp` ran it), and
undeclared names or unknown annotation types met Check's codes (E0201, E0220) instead of Resolve's
(E0100, E0108). Resolve now walks the whole module tree, each submodule resolved against its own
imports and declarations ahead of its enclosing module's scope. Found by the Codex review of bug 215.
Pinned by the `error_217_*.con` fixtures, `regress_217_nested_submodule_scope.con` and a project leg.
