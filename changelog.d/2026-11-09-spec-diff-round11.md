Separate deterministic differential candidate identity from execution identity. Replaying a
saved command into its original populated regression root now allocates a new execution,
retaining immutable evidence snapshots from both runs. Candidate identity/content collisions
still fail with an exact diagnostic. Saved-command controls use the original destination and
fingerprint the original evidence before and after replay; copied plants and baselines get
fresh execution identities. No language rule changes.
