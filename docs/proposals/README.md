# Proposals and work plans

These documents support decisions and implementation; they do not override the
[owner decision record](../decisions.md) or the [Lean specification](../../ConcreteSpec/README.md).
A reviewer's acceptance is not owner approval, and owner approval is not completed implementation.

| Document | Status and scope | Owner record / implementation home | Retirement condition |
| --- | --- | --- | --- |
| [Foundation](FOUNDATION.md) | Foundation contracts adopted; implementation and proofs remain work. The document distinguishes proposed completions. | [Foundation decision](../decisions.md#decided-2026-11-08-foundation); A2i in the [roadmap](../../ROADMAP.md#the-queue). | Archive the plan after its decided contracts and laws land in Lean, with remaining questions dispositioned. |
| [Spec unification](SPEC_UNIFICATION.md) | A2i implementation plan with recorded reviews; not a normative specification. | [Integration decision](../decisions.md#decided-project-long-term-2026-11-06); R-0571. | The document calls for retirement when A2i is done. Preserve its review evidence and disposition first. |
| [Examples](EXAMPLES.md) | Showcases adopted; per-slice examples remain draft. | Owner adoption recorded in the document; slice implementations own accepted examples. | Retire after accepted examples live next to their slices, as the plan requires. |
| [SPARK comparison](SPARK.md) | Mixed: decided items, scheduled internal work and owner questions. | [SPARK decision](../decisions.md#decided-project-spark-2026-11-06); item-level status in the comparison. | Archive after every recommendation has a decision, task or explicit deferral. |
| [Legacy ideas sweep](IDEAS.md) | Historical review snapshot with proposed dispositions; check each item against current decisions and tasks. | The snapshot names its sources; [retention](../../RETENTION.tsv) and the [roadmap](../../ROADMAP.md) govern current migration. | Archive after every finding has a recorded current disposition. |
| [Merged pipeline review](MERGED.md) | Historical findings from pinned 2026-11-06 snapshots; its proposed label does not describe every item's current status. | [Compiler architecture](../../Concrete/README.md) and [owner decisions](../decisions.md) hold current answers. | Archive after every finding maps to a decision, implementation, task or deferral. |

The historical reviews mention temporary branch copies, raw output and local paths that are not
retained in this tree. Those references are context, not replayable evidence. Keep the pinned
snapshot identities; do not imply missing files are available. No review report is moved or deleted
solely because a newer plan exists: its findings must first have an explicit disposition.
