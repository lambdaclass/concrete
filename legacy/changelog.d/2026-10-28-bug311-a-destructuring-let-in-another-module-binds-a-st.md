### Bug 311: a destructuring let in another module binds a struct's private fields (TYPE-11 enforced only for literals, reads and writes) (2026-10-28)

A destructuring `let` outside a struct's module may bind only its `pub` fields (E0298, TYPE-11), as a field read already had to; and, as the owner decided OD21 on 2026-10-29, a parent module no longer constructs its child's newtype or names its child's private fields (E0296, E0298). Pinned by run_tests.sh and the spec examples `TYPE-11/destructure-private-field` and `TYPE-11/parent-*`; structural owner R-0499
([bug 311](docs/bugs/311_a_destructuring_let_in_another_module_binds_a_st.md)).
