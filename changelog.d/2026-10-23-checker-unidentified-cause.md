### An Unidentified Checker Names Its Cause (2026-10-23)

When the Lean checker's identity cannot be established, every report now says why. Before, the
receipts and proof-status reports printed a bare `Checker: UNIDENTIFIED` and withheld every receipt.
On 2026-10-23 that hid a gate host whose non-login PATH lacked `shasum`, which the build digest is
computed with, and 17 receipt, replay and checker-identity gates failed with nothing pointing at the
missing tool.

`ReplayEnvironment.checker` is now a `CheckerResolution` (`identified` or `unidentified why`) rather
than an `Option CheckerIdentity`. The reason comes from where identification failed: `shasum` that
cannot run, exits non-zero or prints the wrong digests; no identity line or more than one; the
loaded-image refusals; a checker process that could not start or exited non-zero. Both refusals
(`ReplayEvidenceRefusal.checkerUnidentified`, `IssueRefusal.checkerUnidentified`) carry it into
`Checker:`, `Checker now:`, every withheld or refused receipt, the policy line of the consumer's
fresh re-check, and the JSON report (`checker_unidentified_reason`; `"checker": "UNIDENTIFIED"` is
unchanged). An unidentified checker still mints nothing and is current against nothing.

`check_checker_identity.sh` asserts the named cause for each refusal stub and adds a leg with
`shasum` stripped from PATH (17 legs). The two identity mutation families re-anchor on the new
refusal lines and are still killed by that gate.
