### The Language Specification Is One Checked Document (2026-10-23)

[docs/spec/](docs/spec/README.md) is now the one written statement of the Concrete language, in
the Austral tradition of a language that fits in one auditable document. Each area has a chapter,
and every rule and definition has a stable, versioned ID (`REF-3@1`) that other documents, the
website and the compiler cite instead of restating it. The normative definition is Core's
semantics in Lean; an entry links the Lean definition it describes where one exists, and until then
its sentence is provisional and the missing link is tracked debt. Authority moves one chapter at a
time: a migrated chapter replaces the area's old prose home, which becomes a design record.

Fifteen chapters exist. `references.md` and `scope.md` are the pilots and carry the exit 2i rules;
`patterns`, `ffi`, `ownership`, `pipeline`, `types`, `memory`, `trusted`, `statements`,
`arithmetic`, `contracts`, `modules`, `syntax` and `traits` are migrated from their earlier
documents (`PIPELINE_CONTRACTS.md`'s fixtures became spec examples and its gate was retired). `ids.tsv` registers every ID ever allocated,
append-only, and `codes.tsv` classifies every diagnostic code.

`scripts/spec/spec.py` is the one parser, and `check_spec.sh` (CI) is the gate, with negative
controls. It checks the format and the ID history (no ID removed, no sentence hash changed, states
move only forward), runs every example on both consumers, runs each accepted example's functions
under the Lean model as well (`--report model-diff` against the interpreter), requires reject
examples to pair with the rewrite they accept, classifies every code, resolves each `Enforced by:`
and `Semantics:` site to a declaration carrying a `spec:` comment, regenerates the manifests
(`tests/rules/exit_2i.tsv` comes from the spec), and refuses a normative sentence restated outside
`docs/spec/`. The website's reference pages and the spec's pages with a rule index are generated
at build time (`scripts/gen/site_reference.py`; the tracked copies had all drifted), and
`check_site_generated.sh` keeps them generated.
