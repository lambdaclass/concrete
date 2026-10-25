### Bug 244: A Killed Gate Run No Longer Wedges The Repository (2026-10-25)

Killing a gate run killed only the runner. Its gates, the compilers they started and the harness
copies they worked in kept running under init, and the repository lock refused every later run
until someone removed it by hand. A run now executes as one process group of its own session
(`scripts/tests/lib/gate_session.py`). A signal to the runner terminates the whole group: SIGTERM,
then SIGKILL after a grace period. The lock records the group, and a later run reclaims it
automatically once the kernel reports the group empty. While any member, or a campaign group
registered in the lock, is still alive, the lock still refuses. The pre-push hook forwards its
signals to the run. Structural class 16 (artifacts at paths no run owns).
