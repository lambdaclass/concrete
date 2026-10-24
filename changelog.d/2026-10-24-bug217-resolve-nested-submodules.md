### Resolve Checks Every Nested Submodule (bug 217, 2026-10-24)

Resolve walked only the modules of the top-level list, so a module nested two deep, a module written
after `main`, and every `mod x;` file of a project (std included) produced no Resolve diagnostics: a
cast to an unknown type there reached the backend (a panic natively; `--interp` ran it), and
undeclared names or unknown annotation types met Check's codes (E0201, E0220) instead of Resolve's
(E0100, E0108). Resolve now walks the whole module tree, each submodule resolved against the
environment Check gives it: its imports, its own declarations and its sibling modules' types. Found by the Codex review of bug 215.
Pinned by the `error_217_*.con` and `regress_217_*.con` fixtures and a project leg.
