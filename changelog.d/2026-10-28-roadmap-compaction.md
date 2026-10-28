### Roadmap: ROADMAP.md Cut Down To Its Control Plane; Compiler Totality And Repository Cleanup Queued (2026-10-28)

[ROADMAP.md](ROADMAP.md) now holds only the control plane: the active and next phases, the execution
queue with its Phase 1 exits, the owner decisions OD1 – OD22 (unchanged, all unanswered), the feature
prerequisites, the capacity allocation and the release map. Everything else moved verbatim: the task
bodies to the [task register](docs/project/ROADMAP_TASKS.md), the dated decisions to the
[decision log](docs/project/ROADMAP_DECISIONS.md), direction and standing rules to
[ROADMAP_DIRECTION.md](docs/project/ROADMAP_DIRECTION.md), the promotion pipeline to
[PROMOTION_PIPELINE.md](docs/project/PROMOTION_PIPELINE.md), the language rules to the
[archived language scope](docs/archive/roadmap-language-scope.md) (pending absorption into the
language specification) and completed records to the
[roadmap history](docs/archive/ROADMAP_HISTORY.md). The roadmap gates read the control plane and
the task register separately and refuse either taking over the other's part. The eight `tasks/`
in-flight records now read `done`: every branch they named had landed, and the queue builder needs
the record to keep from dispatching the task again. This is sub-item 3 of R-0554.

New tasks in queue row 2: R-0553, compiler totality (a ratchet gate over every non-total `Concrete.*`
declaration, exit 2p, then conversion stage by stage ahead of the pass proofs), and R-0554, the
repository cleanup programme.

### Repository Cleanup: Merged Remote Branches Deleted (2026-10-28)

The 192 remote branches already merged into main were deleted (R-0554, sub-item 14). Merged and dead
worktrees are pruned periodically under the same sub-item.
