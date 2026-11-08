- Gate 9 separates writer-produced candidate trust digests from owner authentication.
  T1 reports authorization not established until the owner chooses an authenticator
  and approves the initial canonical digest (R-0678 / R-0509). Historical citations
  are context; regenerating the report and JSON cannot establish authorization.
- Traced tools are checked under native compile/link, Lean build, or Lean emission
  roles. Native compilation rejects Lean's bundled clang and the other compiler
  package; linker roots follow the selected compiler's bintools. Focused controls
  cover substitutions and cross-component resolution entirely inside the store.
