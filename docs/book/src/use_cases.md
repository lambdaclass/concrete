# Use Cases

Concrete is not trying to be the best language for every kind of software.

It is most compelling where low-level control, auditability, and trust boundaries matter at the same time.

## 1. Audit-Sensitive Systems Code

Good fit:

- code that touches files, processes, networks, or foreign interfaces
- code that must make authority and trust boundaries visible
- code that benefits from reports showing why authority is required

Why Concrete fits:

- capabilities are explicit
- `Unsafe` and `trusted` are explicit
- audit reports can explain authority, trust, allocation, and cleanup

## 2. High-Integrity Or Critical Components

Good fit:

- code that needs tighter restrictions around allocation, authority, or FFI
- systems where analyzability matters as much as raw flexibility

Why Concrete fits:

- the roadmap includes a future high-integrity profile/subset
- the language already emphasizes explicit runtime and trust boundaries

## 3. Proof-Oriented Low-Level Libraries

Good fit:

- parsers and formatters
- container operations with invariants
- low-level routines that need stronger trust arguments

Why Concrete fits:

- validated Core is being shaped as a proof boundary
- the project aims to let selected Concrete functions be proved in Lean 4

## 4. Authority-Constrained Tools

Good fit:

- tools that should read files but never open sockets
- components that should format or parse but never spawn processes
- packages that should stay within a fixed authority budget

Why Concrete fits:

- the capability model already exposes authority at function scope
- later phases aim to extend that into package/subsystem-level authority budgets

## 5. Compiler And Language Research With Real Systems Pressure

Good fit:

- people who care about compiler trust, audit outputs, and proof boundaries
- people who want low-level language work without hiding semantics behind giant feature surfaces

Why Concrete fits:

- the compiler is in Lean 4
- the pass structure is explicit
- the project is intentionally shaping Core and SSA as real semantic/backend boundaries
