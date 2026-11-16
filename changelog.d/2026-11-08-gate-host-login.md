### The gate host checks its login before it fetches (2026-11-08)

A landing check run from an account whose `concrete-ci-2` ssh alias is the root admin login
(by design: the provisioned gate identity is `agent` on fede-256gb, which logs into `dev` with the
`ci_gates` key, dotfiles `servers/agent-home.sh`) looked for `/root/concrete2` and reported only that
the fetch failed. `scripts/land/gates_transport_ssh.sh` now checks the login on the host before
anything is fetched: a root login is refused (gates never run as root), a login `id` cannot read is
refused rather than taken as non-root, and a login without its gate clone names the account and
path. Each is INCOMPLETE, never a pass. `scripts/checks/landing_script.sh` covers all three through
the real transport, and none fetches.
