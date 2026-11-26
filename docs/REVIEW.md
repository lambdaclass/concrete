# Reviews

The coordinator runs reviews outside this repository and records the result
for the revision sent for review. See [AGENTS.md](../AGENTS.md#rules)
for review procedure and [LANDING.md](LANDING.md) for the merge path.

<a id="the-cap"></a>
The review caps are two rounds for docs and roadmap, three for scripts and
five for compiler and spec.

## What blocks a review

Apply the [review proportion rule](../AGENTS.md#review-proportion).

## Owner decisions: choosing versus recording

The coordinator checks whether approval covers any change that chooses, revises, encodes, implements,
extends or reverts a language decision OD1 – OD157 ([decisions.md](decisions.md#owner-decisions))
or an important project question. The reviewer checks the decision record for one of these
sources of approval:

- The owner's quoted, dated answer or the quoted owner rule that decides the question under the
  [recording rule](decisions.md#decided-project-recording).
- For a matter within Slices 0 and 1, a decision recorded under the
  [November 25 delegation](decisions.md#decided-2026-11-25-freeze-and-grammar), with verdicts
  showing that Claude, Codex and agy all agree on that decision. Report the decision to the owner
  as the delegation requires.

Outside Slices 0 and 1, or when the record lacks any of the three agreeing verdicts, the
delegation supplies no approval and the owner must decide. For either source, the reviewer
checks that the record faithfully states the approved decision, every change stays within its
scope, and every matter marked decided has approval. Missing provenance or any failure of
these checks is a finding. A patch that needs an unapproved decision must stop and be reported
to the owner before landing.

Address a provenance or scope finding in the next review round with the applicable evidence
or a narrower patch.
