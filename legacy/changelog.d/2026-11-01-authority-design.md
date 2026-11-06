### Permission is `with`, resources are linear values; `Std` is removed (2026-11-01)

The owner decided the authority design:
- `with(...)` is the only permission mechanism;
- linear values are the only resources, and an operation needs both;
- the permission-token concept is dropped;
- `Std` stops being a capability, so signatures list their real capabilities;
- `main`'s parameters are the target's singletons, and there is no `Root` type.

The roadmap also now says that once the spec is rewritten, all other documentation is updated to follow it.
