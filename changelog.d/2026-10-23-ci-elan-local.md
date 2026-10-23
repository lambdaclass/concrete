### CI: Self-Hosted Runners Keep Their Lean Toolchain Instead Of Re-Downloading It (2026-10-23)

Every CI job restored `~/.elan` from the GitHub Actions cache: a 2.3 GB archive downloaded at
13-16 MB/s, 2-4 minutes per job (median 176 s, 48 jobs over trains 9-11, 151 runner-minutes, about a
third of the Hetzner runner time in a train). On the self-hosted runners the runner user's home
persists and already held those toolchains. The `Cache the Lean toolchain (~/.elan)` steps now run
on GitHub-hosted runners only; on self-hosted runners lean-action and the 4.35 leg's
`elan toolchain install` add a missing toolchain once. The `.lake/build` cache, which hands the Build
job's output to every gate job, is unchanged.
