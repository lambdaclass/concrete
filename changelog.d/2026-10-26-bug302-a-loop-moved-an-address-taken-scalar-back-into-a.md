### Bug 302: a loop moved an address-taken scalar back into a register at its exit, splitting it from a raw pointer to its slot (2026-10-26)

A raw pointer to a scalar local taken inside a loop (`&mut x as *mut i64`) now stays an alias of
the local after the loop: Lower keeps a scalar the loop made slot-backed in its slot instead of
moving it back into a register at the exit. Regression leg:
`regress_302_loop_keeps_addr_taken_slot.con` in `run_tests.sh`. Structural owner: R-0501
([bug 302](docs/bugs/302_a_loop_moved_an_address_taken_scalar_back_into_a.md)).
