# Module Qualification

Status: closed

Qualified module access landed during Phase H. Real programs no longer need rename-heavy workarounds for ordinary multi-module code.

## What Landed

Concrete now supports qualified access for the practical cases Phase H workloads needed:

```con
math::add(...)
net::parse(...)
```

- file-based `mod::fn` access
- mixed imported + qualified access
- two-submodule qualification
- top-level + qualified coexistence
- qualified submodule `extern fn`
- qualified submodule struct/import interaction
- same-name collision handling
- inline sibling `::` access

The implementation settled on explicit qualified names rather than import magic or alias-heavy recovery.

## What Changed

- Elab-time definition prefixing and cross-module renaming resolved same-name collisions without adding a second naming model.
- Resolve / Check / Elab now consistently support sibling module qualification.
- Diagnostics and roadmap language now treat qualified module access as complete for the practical cases exercised by real programs.

## Why This Closed Cleanly

- lookup rules stayed simple
- explicit module boundaries were preserved
- no hidden import magic was added
- the feature solved real Phase H rename pressure without pushing Concrete toward a heavier namespace system

## Historical Note

This note started as an open design question when real programs first hit namespace pressure. It is kept as a short historical record because the problem was real, but the language/design question is now closed.
