# assumed_boundary

**Class:** a precondition the function ASSUMES about its inputs — `assumed_at_entry`
inside the body, with the obligation pushed to callers.

`at`'s `#[requires(0 <= i && i < 16)]` is `assumed_at_entry`; `good_caller`'s
call `at(3)` discharges it `proved_at_callsite`. `concrete <src> --report
contracts` shows both. The other assumed flavor — a tainted, audit-loud
`assumptions.toml` entry that is never `proved` — is on the constant_time_tag
flagship (machine-level timing).
