# proved_by_kernel_decision (bv_decide)

**Class:** a closed bitvector obligation discharged with `bv_decide`
(bit-blasted, LRAT-checked) — a kernel decision procedure, no external SMT.

`rotr7` calls `rotr`, whose `#[requires(0 <= n && n < 32)]` is closed after
let-substitution (`n = 7`). `concrete <src> --report contracts` shows the call
`proved_by_kernel_decision`, engine `bv_decide`.
