### The Scaling Gate Separates the Compiler's Work From the CI Host's Load (bug 294, 2026-10-26)

The compile-time scaling gate compared stage times with fixed budgets measured on a quiet host, so
a busy CI host failed stages whose work had not changed. Each run's retired instructions and each
stage's allocations are now held to exact budgets, and stage times are compared with their
budgets at the host speed the budgets were measured at, up to a factor of two
([bug 294](docs/bugs/294_scaling_gate_wall_budgets_under_load.md)).
