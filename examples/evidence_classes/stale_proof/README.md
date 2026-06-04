# stale_proof  (negative case)

**Class:** a proof link whose body has DRIFTED from the linked spec — reported
`stale`, not proved.

Same in-source link as a real `ct_compare`, but `diff` starts at 1 (the real one
starts at 0), so the extracted PExpr no longer matches `#[spec]
Concrete.Proof.ctCompareExpr`. `concrete <src> --report proof-status` reports
`stale` (spec drift). This is the negative case for every `proved_*` subexample:
an in-source link is subject to the same staleness machinery as a JSON entry.
