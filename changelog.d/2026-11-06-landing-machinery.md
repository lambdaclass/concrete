- The landing and review machinery fails closed (ISS-0008 to ISS-0011): `train.sh check` fails when
  no reviewer exists instead of dropping the review leg, and `train.sh land` requires review
  provenance (a clean review note at the train head, or on every member at its exact SHA, judged by
  `scripts/review/verify.sh`). The legacy/ move gate triggers only on changes under `legacy/` and to
  the root inventory's top-level entries, so a new folder under `docs/` needs no rehearsal.
  `scripts/land/acceptance.sh` keeps the review leg and builds with `nix develop --command lake
  build`. The review prompts permit recording an owner-approved decision and check its provenance
  ([docs/REVIEW.md](docs/REVIEW.md#owner-decisions-choosing-versus-recording)).
