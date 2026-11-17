### Require admitted ownership transitions to complete

Scope cleanup now requires a successful lifecycle result when the pending
buffer is empty and the current frame's departing locals are discharged
and free of loans. It removes the selected slots and bindings and preserves
the loan table. A moved-local scope-exit fixture checks the actual dispatcher.

The audit also adds progress to the loan action, binding, primitive drop,
direct owner argument and two return restoration Laws. The proofs reuse
the existing primitive and return progress results. Each strengthened Law
rejects a targeted refusal control. History and certified prefix Laws
continue to state preservation across produced transitions.
