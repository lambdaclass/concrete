Trust checks now preserve Lean interpolation boundaries across whitespace and nested/line comments,
validate character/string escapes and string gaps, and reject ambiguous ordinary strings containing
`{`. Literal backend output uses escaped interpolated braces without changing the emitted text.
The lexer follows the pinned Lean parser's literal and identifier rules; unknown syntax quotations,
ambiguous custom interpolation and malformed literals fail closed. Regression witnesses compile
with Lean before checking that both scanning and example rewriting retain the hidden declarations.

Compiler import permissions are the explicit module inventory under the approved ConcreteSpec,
Concrete, Lean and pinned LeanCompilerBackend roots, not a permissive module-name pattern.
The shared environment enumerator accepts PROOF_PKG=Concrete (default ConcreteSpec), with explicit
imports instead of sed-generated variants. Orphan cleanup checks both libraries' source inventories
and removes orphan library/IR artifacts before building both targets. Controls exercise the compiler
CLI's import and trust bans, literal spoofing, and orphan deletion/preservation in private scratch.
The README now distinguishes this coverage from the remaining compiler totality and external replay
work (R-0553). ISS-0032 records the structural cause and owner.

No Concrete or ConcreteSpec declaration name or signature changes in this round. The APIs used by
wip/slice0-lexer-ws and wip/lexer-literals remain stable. Existing lean_lex.py public entry points
remain; scan-compiler now requires the approved source inventories and the pinned Lean installation.
