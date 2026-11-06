### Bug 349: append is accepted in a function that does not declare Alloc, because Elab desugars it into string_append calls that carry no capability (2026-10-31)

Filed, not fixed: `append(&mut s, ...)` is accepted in a function that does not declare `Alloc`, because Elab desugars it into `string_append*` calls that carry no capability (MEM-7@1). The reproducer is pinned by `check_example_outputs.sh` (open-bug reproducers leg); the structural owner is R-0499
([bug 349](docs/bugs/349_append_is_accepted_in_a_function_that_does_not_d.md)).
