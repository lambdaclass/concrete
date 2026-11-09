- Add slice B checkpoint infrastructure: immutable complete ref snapshots accepted by
  verification-only transactions, and complete-set compare-and-swap installation.
  Prepared transactions inspect every participant under lock and reject symbolic
  refs; disposable controls cover files, packed refs and reftable with concurrent
  writers. Existing callers and writer-history authority predicates are unchanged.
