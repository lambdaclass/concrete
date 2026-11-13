/-!
# Capability base

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.

The capability set of authority rules 5 and 13 and row Q7.11, shared without importing the
higher authority model. The declarations retain their `ConcreteSpec.Authority` names so existing
consumers use the same definitions. This module has no explicit imports.
-/

namespace ConcreteSpec.Authority

/-- The reserved capability names (authority rule 13). `Console` is the permission for operations
on the standard-stream grants `StdIn`, `StdOut` and `StdErr` (row Q7.11); `Device` is for device
grants; `Unsafe` is not an operational capability (rule 5). -/
inductive Cap where
  | fs | network | env | time | alloc | device | console | process | «unsafe»
  deriving DecidableEq, Repr

/-- Every capability, once. -/
def Cap.all : List Cap := [.fs, .network, .env, .time, .alloc, .device, .console, .process, .«unsafe»]

end ConcreteSpec.Authority
