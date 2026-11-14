Constant registry examples now use monomorphic scalar declaration data, with no runtime
universe dictionary hidden in constructor parameters. The registered `ConstsPermitted`
judgment embeds those declarations field for field and delegates to the existing
`ConstsAdmitted` judgment; constant typing and evaluation semantics are unchanged.
The accepted dependency example and unused-overflow refusal remain Instance checks.
