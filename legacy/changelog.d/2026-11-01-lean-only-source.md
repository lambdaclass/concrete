### Lean is the only source of the language (2026-11-01)

The owner decided that the language is defined only in Lean: Core, its rules, its execution semantics and
the desugaring of every surface feature. The spec, the rule registry and the conformance oracle are to be generated
from that definition, the reference interpreter is to be the Lean semantics itself (replacing `--interp`
once it passes the existing differential corpus), and the compiler is to be proved against it. The roadmap also records the order of the compiler proof, from a research review: semantic
contract, executable Core semantics, one end-to-end path, functional models replacing ProofCore,
open-module correctness, ghost permissions for trusted code, and SMT-checked lowering. The plan is at the
top of `ROADMAP.md`; the decision is in `docs/project/ROADMAP_DECISIONS.md`.
