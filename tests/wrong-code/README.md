# Wrong-code regression corpus

The named, durable home for compiler bugs that ever silently shipped
wrong behavior. Contract: `docs/WRONG_CODE_CORPUS.md`.

This directory is a **registry**, not a relocation. Existing repros
under `tests/programs/bug_*.con` and `tests/programs/adversarial/`
stay where they are; the manifest below references them by path so
existing test wiring keeps working.

## Files

- `manifest.toml` — the registry. Every wrong-code case has an entry.
- `cases/WC-NNNN.md` — per-case notes (root cause, fix, minimization).
- `cases/<id>/` — full artifact bundle for cases that need more than a
  single notes file (IR dumps, original un-reduced source, etc.).

## Running

```sh
make test-wrong-code
bash scripts/tests/test_wrong_code.sh --include-open
```

The default run asserts that every `status = "fixed"` case still
behaves as expected. `--include-open` additionally probes
`status = "open"` cases, reporting their pass/fail without failing
the run; this is how we detect when an open bug becomes fixed.

## Adding a case

1. Pick the next free `WC-NNNN` id.
2. Append an entry to `manifest.toml`. See `docs/WRONG_CODE_CORPUS.md`
   for field semantics.
3. Write `cases/WC-NNNN.md` with the conventional skeleton (Symptom,
   Root cause, Fix, Minimization, How to verify it stays fixed).
4. If the repro doesn't already live somewhere with stable wiring, put
   it under `tests/programs/bug_*.con` or
   `tests/programs/adversarial/<area>/<name>.con`. The manifest
   `repro` field points at it.
5. Run `make test-wrong-code` and confirm the new case passes (for
   `status = "fixed"`) or fails as expected (for `status = "open"`).

Cases are **never deleted**. A case that becomes irreproducible after
a deliberate language change keeps its manifest entry; the notes file
gains a "superseded" paragraph explaining what changed.
