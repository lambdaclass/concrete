### An Agent Work Queue And A Status Page (2026-10-22)

`scripts/queue/` lets agents run without a human orchestrator. Each dispatcher tick rebuilds
`queue.json` from its sources (open bug records, the roadmap's development-throughput rows and
execution queue, fuzz-farm findings, and explicit `tasks/*.md` files), watches the running agents,
and starts headless agents on the next ready tasks, with priority tiers, area conflicts, budgets, a
heartbeat and a kill switch. A finished agent leaves a branch with review and gate notes for the
landing bot. The dispatcher never pushes to main or `ci/*`: its clone's pre-push hook
(`push_guard.sh`) admits only `wip/*` branches and the readiness notes. [WORK_QUEUE.md](docs/project/WORK_QUEUE.md) describes it.

`scripts/status/status.py` generates `status.json` and a self-contained, phone-width `status.html`:
whether both mains agree and their CI is green, trains in flight against the 40-minute target, the
landing medians, branches, open bugs, and runner health. A sanitized public variant withholds
infrastructure details, and `scripts/land/notify.sh` sends deduplicated notifications to ntfy,
Slack or stdout. [STATUS_PAGE.md](docs/project/STATUS_PAGE.md) records the publishing decision.

Both installers write systemd units and enable nothing by default (`scripts/status/install.sh
--enable` starts the status services on request). `check_work_queue.sh` and
`check_status_page.sh` (CI) test them with stub tools.
