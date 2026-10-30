### Language specification: one document that opens with the language (2026-10-30)

`docs/spec/README.md` is now a short landing page. The overview chapter opens with what Concrete is,
how to read a rule, and the chapter map in reading order, from which the tooling reads the chapters.
The process material (format, registry, gates, migration) moved unchanged to
`docs/spec/MAINTAINERS.md`, section numbers kept. `docs/spec/SPEC.md` is the whole specification as
one generated page, with one table of contents; `check_docs_drift.sh` refuses it when it differs from
the chapters, with a negative control, and `refresh_generated.sh` regenerates it.
