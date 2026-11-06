# Status page and notifications

Status: operational procedure (landing throughput; complements docs/project/LANDING.md)

A generated page replaces the orchestrator's hand-written status reports, and push
notifications replace polling it. Both are readable from a phone.

## What the page shows

`scripts/status/status.py` writes `status.json` and one self-contained `status.html`. The page
has no external assets, works at phone width, follows the system dark mode and reloads itself
with a meta refresh every 120 s. If the generator stops running, a banner marks the page stale.

| section | source | flags |
|---|---|---|
| Now | everything below | a red headline for a main mismatch, red main CI, a failing job on a running `ci/*` run, an offline runner |
| Main | `git ls-remote` on origin and lambdaclass; the latest `CI` run on main of each repo | the two mains must be the same SHA; a CI run that is not at main's head is called out |
| In flight | `gh run list` (workflow `CI`, branches `ci/*`), `gh run view --json jobs` | elapsed time over the 40 min target; failing jobs by name |
| Landing (R-0547) | the per-train TSVs `scripts/land/train.sh` writes under `<git common dir>/concrete-landing/log` | median time to land against under 2 h, median landing CI against at most 40 min, the last 10 trains with their stage durations |
| Branches | `wip/*`, `ready/*`, `train/*` on origin; ahead/behind against `origin/main`; the branch's latest workflow run; git notes `refs/notes/concrete-review` and `refs/notes/concrete-gates` on the tip | far behind main, no commit for 7 days |
| Open bugs | `scripts/gen/bug_ledger.py corpus` when it exists and the records are migrated; otherwise each `docs/bugs/NNN_*.md` record's first `Status:` line (OPEN, CONTAINED, Half, Partially = open) | |
| Server | load average (this host, or `STATUS_LOAD_CMD`); `gh api repos/.../actions/runners` | any runner offline |

"Last push" is the tip's committer date: git does not record push times. Every collector fails
soft. A section it cannot read says "unavailable" and why, and the rest of the page renders.
The generator only reads from GitHub. `--fetch` writes only remote-tracking refs and a private
notes mirror (`refs/notes/status-mirror/*`), so it never overwrites a local note.

## Publishing: decision

**The full page is served privately from the CI server (option a).** The repository is public,
and the full page names infrastructure: runner host names, the generating host's load, and
whatever agents write into review and gate notes. Nothing on it is needed by anyone who cannot
already SSH to the server.

`scripts/status/install.sh`, run as `dev` on the server, writes three systemd user units:

- `concrete-status.service` is a oneshot. It fetches a dedicated clone
  (`~/concrete-status/repo`, never a working clone), detaches it at `origin/main` and runs a
  copy of `status.py` from `~/concrete-status/bin`, so switching branches in any checkout cannot
  break it. Re-run the installer to update that copy.
- `concrete-status.timer` runs the service every 2 minutes.
- `concrete-status-http.service` runs `python3 -m http.server`, bound to `127.0.0.1` only,
  serving only `~/concrete-status/www`.

The installer enables nothing unless it is given `--enable`. To enable:

```sh
sudo loginctl enable-linger dev                 # once, as ubuntu: user units run without a login
scripts/status/install.sh                       # as dev: writes the units, prints the commands
systemctl --user daemon-reload && systemctl --user enable --now concrete-status.timer concrete-status-http.service
```

`dev` needs a `gh` login with a read-only token: Actions read and Metadata read. Listing runners
also needs Administration read, and without it the runner row says "unavailable". The landing log
lives in the git common dir of the clone that runs trains. When that is another account, pass
`--log-dir` with a directory `dev` can read. `report.sh --export <dir>` writes one.

**Viewing.** From a laptop, run `ssh -N -L 8787:127.0.0.1:8787 dev@<server>` and open
`http://localhost:8787/status.html`. From a phone, an SSH client with port forwarding (Termius,
Blink) does the same. Tailscale on the server and the phone would make it a plain URL without
opening a port. That is a later step, and it changes nothing here, because the server stays
bound to localhost and Tailscale serves it.

**Optional public variant (option b).** `install.sh --public` also writes a sanitized page to
`~/concrete-status/public`. `status.py --public-out DIR` produces the same page directly.
Free-form text is **withheld**, not scrubbed: review and gate notes, landing notes and error
messages. Only the fact that a note exists survives, because no pattern list can vouch for
arbitrary prose. The scrub then runs over every remaining string and removes the following:

- IPv4 and IPv6 addresses;
- GitHub, Slack, OpenAI and AWS tokens and Bearer credentials;
- credential assignments: `key=value`, `key: value` and `"key": "value"`, where the key
  contains token, secret, password, pw, api key, credential, topic or webhook url
  (`NTFY_TOKEN`, `AWS_SECRET_ACCESS_KEY`), with a quoted value removed whole;
- Slack webhook URLs and ntfy topics;
- private keys and SSH public keys;
- e-mail addresses;
- home-directory paths;
- the server's host name, plus any name in `STATUS_PRIVATE_HOSTS`.

Runner names become "runner N". Only this variant may ever be pushed to a `status` branch or to
gh-pages. Nothing pushes it today.

## Notifications

```sh
scripts/land/notify.sh [--dry-run] <info|success|warn|error> <title> <body>
```

The landing bot calls this script. Channels come from `~/.config/concrete/notify.env` (mode 600,
never committed). `scripts/land/notify.env.example` is the template. The file is parsed as
`KEY=VALUE` lines and never sourced.

- **ntfy** (`NTFY_TOPIC`). Install the ntfy app (iOS or Android), tap "+", and subscribe to the
  topic on `ntfy.sh`. No account is needed. **The topic must be a long random string**: an ntfy
  topic is public, so anyone who knows or guesses it can read every notification and post fake
  ones. Generate it with
  `python3 -c 'import secrets; print("concrete-" + secrets.token_urlsafe(24))'`. notify.sh
  refuses a topic shorter than 24 characters. The level sets the priority: `error` is urgent and
  breaks through Do Not Disturb if the app allows it, and `warn` is high.
- **Slack** (`SLACK_WEBHOOK_URL`). An incoming webhook. The URL is itself the credential.
- **stdout**. The fallback when no channel is configured, or when every configured channel fails.

The topic, the token and the webhook reach curl through a mode-600 config file, never on a
command line that `ps` would show. The body is read from stdin, so a body that starts with `@`
is sent as text and never makes curl upload a file. A notification never fails a landing. Delivery failures go to stderr, and the exit status is 0.
Exit 2 is reserved for usage errors. An identical (level, title, body) within
`NOTIFY_DEDUP_SECONDS` (default 900) is sent once. A delivery that failed on every channel is
not recorded, so its retry is not swallowed as a duplicate. `--dry-run` prints what each channel
would receive, with the topic and the webhook redacted, and sends and records nothing.

## Tests

`scripts/tests/check_status_page.sh` runs in CI's build-free grammar job. It covers:

- generation from a throwaway repository: bare-repo origin and mirror, notes, bug records and a
  landing log, with a fake `gh`;
- fail-soft rendering without `gh`;
- the sanitizer, against secrets planted in the full page;
- basic HTML validity;
- notify's usage errors, fallback, dry-run, short-topic refusal, dedup window and failed-delivery
  retry;
- the installer, which enables nothing and binds to localhost only.
