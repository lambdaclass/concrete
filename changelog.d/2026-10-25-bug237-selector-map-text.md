### Gate Selection Reads Map Notes as Text (bug 237, 2026-10-25)

The impact selector counted a script name written in a keyed array's value (a bug-corpus note, an
exemption reason) as a call, so a corpus note naming the docs-drift gate made the corpus runners
look as if they read `docs/`, and a docs-only diff selected 19 gate commands in CI. A keyed
element's value is now text and its key a read, unless the script executes the map. Pinned by the
gate-selection gate's "text is not a call" controls.
