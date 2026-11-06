# proved_by_kernel_decision (omega)

**Class:** a linear-integer obligation the compiler discharges itself with
`omega` — a kernel decision procedure, no external SMT, no hand proof.

The counted loop's init (O1) and variant (O4/O5) obligations close by omega.
`concrete <src> --report contracts` shows them `proved_by_kernel_decision
(omega)`. (O2's operational step still needs Lean — shown honestly as such.)
