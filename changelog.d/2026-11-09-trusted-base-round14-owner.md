Gate D implements owner decision X4 with SSH signature verification over the
canonical trust digest, namespace concrete-trusted-base-v1 and principal
concrete-owner. Concrete/owner.allowed_signers is the single registry-owned
trust root, empty pending the coordinator obtaining the owner key. A consistent
candidate fails exactly with "FAIL approval: awaiting owner key". Only unsigned
candidate metadata was regenerated; no approval signature or production key was
created, and the gated trusted-base golden update remains blocked. Controls use
fresh keys solely within their private temp directory and check positive,
wrong-signer, digest-tamper, wrong-namespace and non-owner-entry cases exactly.
OpenSSH is pinned and listed in the gate-only component; no pipeline role grows.
