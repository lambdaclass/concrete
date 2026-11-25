### Slice 0 proves SSA outcome preservation

SSA validates the Memory observation against the actual emitted BIR before either backend
uses it. The proof covers return values and the recorded kind of the taken trap. A mismatch
is an internal compiler error. The accepted pipeline gets preservation from this proof, retiring
the SSA premise.
