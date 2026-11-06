### A Deferred Call's Borrowed Local Gets Its Slot Where the `defer` Registers (bug 287, 2026-10-26)

A `defer` that borrows a local, such as `defer copy(sink, &x);`, runs at every exit of its scope.
Lower gave the local its stack slot at the first of those exits it compiled, so on every other
exit the deferred call read a slot that was never written (wrong values natively). The slot is now
made and filled where the `defer` registers, which comes before every exit
([bug 287](docs/bugs/287_defer_borrow_promoted_at_first_emission.md)).
