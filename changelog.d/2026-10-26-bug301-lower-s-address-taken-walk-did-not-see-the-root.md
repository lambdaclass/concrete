### Bug 301: Lower's address-taken walk did not see the root local of a field or element borrow (2026-10-26)

A borrow of a field or element of an immutable struct local or a struct parameter (`&s.x`,
`&q.p.y`, `&w.a[1]`, a method call on a field) inside one branch no longer makes the native binary
read an uninitialized slot on the other path: Lower's address-taken walk now names the root local
`placeAddr` promotes, decided by the same predicate. Regression leg:
`regress_301_place_borrow_root.con` in `run_tests.sh` (value and SSA). Structural owner: R-0501
([bug 301](docs/bugs/301_lower_s_address_taken_walk_did_not_see_the_root.md)).
