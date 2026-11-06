### Deeply Nested Blocks Resolve in Linear Time (bug 282, 2026-10-26)

Resolve looked a name up by walking its stack of block scopes from the innermost, so a body
nested n blocks deep reading a binding of its outermost block cost n per use and n² for the nest
(10000 nested `if`s: 147 ms to resolve, now 5 ms). One map counts, per spelling, the open scopes
that bind it. Diagnostics and output are unchanged
([bug 282](docs/bugs/282_resolve_lookup_scope_per_nesting_level.md)).
