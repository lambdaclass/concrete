### Require LL(1) proofs at the parser boundary (2026-11-25)

The slice-0 parser and its refusal recognizer use checked grammars and tables. Lean requires
a proof that each grammar passes the LL(1) check and that the parser table equals the table
constructed from that grammar. The slice-0 table remains precomputed.
