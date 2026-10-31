### Bug 343: A qualified call m::f() runs a parent function named m_f, and an undeclared m_f() reaches the submodule function m.f (2026-10-31)

Filed, not fixed: a qualified call `m::f()` is lowered to the flat name `m_f`, so a top-level function of that spelling captures it (silent wrong code, exit 7 instead of 42) and an undeclared `m_f()` reaches `m.f`. Pinned by `check_example_outputs.sh` (open-bug reproducers leg); the structural owner is R-0492 (`DefId`) through R-0514
([bug 343](docs/bugs/343_a_qualified_call_m_f_runs_a_parent_function_name.md)).
