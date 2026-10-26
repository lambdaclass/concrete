### The Binder-Reference Gate Sees Eagerly Opened Frames (bug 268, 2026-10-25)

`check_binder_refs.sh`'s empty-scope leg read only an `if`, which never enters a frame scope, so
binder frames opened when a scope is entered (rather than at its first binder) passed it, and
mutation 56 modeled a different defect that other legs caught. A new leg reads a match arm that
binds nothing; mutation 56 is now the faithful defect and that leg kills it
([bug 268](docs/bugs/268_binder_refs_empty_scope_leg_missed_eager_frames.md)).
