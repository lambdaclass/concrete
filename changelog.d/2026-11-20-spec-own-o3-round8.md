### Require progress at ownership dispatch boundaries

Independent admission rules cover lifecycle, loan, and mixed issuance
events. Admitted traces compose the ledger rules for each transition,
including the grants made earlier in a standalone downward entry.
The dispatcher Laws require a result for every admitted event and
preserve the applicable ownership, pending-buffer, or retirement
invariant for every returned result.

Lean checks exhaustive admission coverage and rejects targeted refusal
and unsafe-success controls, including a refusal at an actual nested
zero-argument call. The two round-7 retirement fixture lemmas now carry
Instance tags.
