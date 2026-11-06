### Every Match Arm Reaches the Proof Subject (bug 284, 2026-10-26)

The arms of a `match` on an integer, `Bool`, `Char` or `String`, and every range arm, were missing
from the structural evidence body, so a function's V2 subject digest and implementation identity
did not change when an arm body did, and a stored v2 proof fingerprint read current after the
edit. Each arm's Core and evidence are now produced as one entry; subject digests of functions
with such matches move once. `check_binder_refs.sh` pins it
([bug 284](docs/bugs/284_match_arm_evidence_dropped.md)).
