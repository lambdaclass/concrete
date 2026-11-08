### The gate host refuses a root login and names a missing gate clone (2026-11-08)

A landing check run from an account whose `concrete-ci-2` ssh alias is the root admin login
(by design: the provisioned gate identity is `agent` on fede-256gb, which logs into `dev` with the
`ci_gates` key) looked for `/root/concrete2` and reported only that the fetch failed, as
INCOMPLETE. `scripts/land/gates_transport_ssh.sh` now checks the login on the host before
anything else: a root login is refused (gates never run as root) and a login without its gate
clone names the account and the path, both INCOMPLETE. `scripts/checks/landing_script.sh` covers
both through the real transport, and neither fetches.
