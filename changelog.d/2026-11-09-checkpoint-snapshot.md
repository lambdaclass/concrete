- Add slice B checkpoint infrastructure: immutable complete ref snapshots accepted by
  verification-only transactions, and complete-set compare-and-swap installation.
  Prepared transactions inspect every participant under lock and reject symbolic
  refs; disposable controls cover files, packed refs and reftable with concurrent
  writers. Existing callers and writer-history authority predicates are unchanged.
- Round 22: bound capture to three race attempts; lock before inspecting symbolic
  status, exact names, expected values and object types; validate update objects in
  an aborted prepared transaction before preparing installation and rechecking the
  complete snapshot. Bound protocol I/O and teardown, reject empty names, enforce
  backend support in the transaction owner, and plant removal-sensitive controls.
