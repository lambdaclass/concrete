### The kernel replay checks the modules the sources declare (ISS-0016) (2026-11-07)

`scripts/proofs/kernel_replay.sh` no longer lets leanchecker pick its modules from the build
directory. Given a prefix, leanchecker replays every `.olean` under it on the search path, so a lake
cache restored from a build older than a module rename replayed modules that no longer exist and
failed CI (run 42424028055, `ConcreteSpec.Spec.Ownership` after its rename to `Slice1Ownership`;
the cache came from train commit 5b894432ad, whose sources predate the rename).
Both replay legs now run on a staged library holding exactly the modules `ConcreteSpec.lean` and
`ConcreteSpec/**/*.lean` declare, and the per-module leg checks that the replayed set equals that list. As defense in depth,
`scripts/proofs/lib.sh` deletes build files of modules with no source before every proof check
builds. New controls plant an orphan that fails an unfiltered replay and is ignored by the staged
one, check that pruning removes orphans and keeps every source module, and refuse a missing or
skipped source module.
