### The Mutation Campaign Declares Expected-Red Gates: The PENDING Disposition (2026-10-22)

`check_atomic_flip_entrance.sh` was red on the clean tree, so its two mutation families scored
INVALID (red on clean) on shards 11 and 24 of every campaign. One cause was a pin that had drifted
as fixtures entered its scan (updated); the other is genuine pending R-0004 work (bug 083 linked
`thesis_demo` to `proofFns` with no attestation), and the gate's FAIL line now names the pending
tables. The campaign driver gains `expect_red <gate> <reason> <exact FAIL legs>`. A declared gate
whose clean baseline fails with exactly the declared legs makes its families PENDING: a fifth
disposition, never a kill, published and named on every summary, and not blocking qualification.
Any other red is still red on clean, and a declared gate that turns green is refused until its
declaration is removed. The shard aggregator and the campaign supervisor each check pending results
against the plan and the shard's baseline transcript instead of trusting them.
