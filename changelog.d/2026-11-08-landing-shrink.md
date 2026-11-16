### The landing tools shrink to the train, the review leg and the gates (2026-11-08)

The owner approved the redesign of the landing tools ("yes redesign", 2026-11-08): shrink them
first, then switch to the one repository and protect main on the server. The landing bot is gone:
`scripts/land/bot.sh`, its notifier `notify.sh` and `notify.env.example`, its installer
`install-bot.sh`, its systemd service and timer, and its fixture `scripts/checks/landing_bot.sh`.
What only the bot used went with it: the bisection kill switch (`LAND_PAUSE_FILE`) in `train.sh`,
and the review-note keys `carried-from` and `order`, which `scripts/review/verify.sh` now refuses
as unknown keys. A train still lands only through `scripts/land/train.sh`, with its review leg,
its gates and the `legacy/` protection unchanged. The fixture's legs that tested `ready.sh` and
`train.sh` themselves (the remote leg never adopts another run's ref, a failed `ready.sh` leg
revokes readiness, nothing force-pushes main) moved to `scripts/checks/landing_ready.sh`.
