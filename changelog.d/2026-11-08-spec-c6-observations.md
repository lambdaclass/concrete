### Foundation C6: observations and the environment (2026-11-08)

`ConcreteSpec.Spec.Observe` defines extensible typed request/response/output events, contract-checked and explicitly blocking environments, finite and infinite observations, startup refusal, exit statuses, and exact trap lines with byte columns. Its C1 adapter preserves event histories through terminal and blocked observations without changing `Exec.lean`. `ConcreteSpec.Proofs.Observe2` proves preservation across silent internal steps, trap-line injectivity, byte-column rendering, and output-before-trap preservation.
