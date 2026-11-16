### The bot's launcher rebuilds a cached snapshot missing any file (2026-11-08)

The launcher `scripts/land/install-bot.sh` writes judged a cached snapshot complete when each listed
`SNAPSHOT` path existed, so a cache that had `scripts/land/` but lacked `scripts/land/bot.sh` was
kept and failed on every run (Kimi, review of the slice-0 branch). It now requires every file the
commit has under those paths (`git ls-tree`), and rebuilds otherwise. `scripts/checks/landing_bot.sh`
runs the launcher's own cache step on a missing, an incomplete and a complete cache.
