# Contracts

This chapter is a stub. It holds the foundation decision about what a contract claim means. The
rest of the area (`requires`, `ensures`, invariants, ghost code, the specification judgment)
migrates here from CONTRACTS_GUIDE in README §8, step 7.

## Rules

<a id="CON-1"></a>
### CON-1. Claims are partial correctness

**CON-1@1.** A contract claim states partial correctness, holding for every execution that terminates, and a function is claimed to terminate only when `decreasing` evidence for its loops and recursion is checked.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0473
- Semantics: none
- Obligation: claims are generated against Core semantics as partial-correctness statements, with total correctness only from checked `decreasing` measures (R-0473, R-0495)
- Evidence: tested

**Rationale.** Partial correctness is what a proof about a terminating execution can establish
without a termination argument. Termination is a separate, explicit claim, so a proof never
implies termination by accident. Divergence remains one of the three outcomes (STMT-1).

<a id="CON-2"></a>
### CON-2. Ghost values stay in ghost code

**CON-2@1.** A ghost binding is read only by contracts, assertions and other ghost code, never by code that runs.

- Kind: refusal
- Uses: none
- Codes: E0420 (elab)
- Status: enforced
- Enforced by: Concrete.elabExprEv
- Semantics: none
- Obligation: ghost code is erased before Core without changing any runtime value (R-0473)
- Evidence: tested

**Rationale.** Ghost bindings are proof-only and are erased before code generation, so a runtime
read of one would read nothing. The code was in the compiler before this chapter; it entered the
ledger when the codes-completeness gate began to scan Elab (2026-10-23).

```con reject name=ghost-read-at-runtime code=E0420 stage=elab pair=ghost-in-assert
fn main() -> Int {
    ghost let g: Int = 42;
    return g;
}
```

```con accept name=ghost-in-assert exit=42
fn main() -> Int {
    ghost let g: Int = 42;
    assert(g == 42);
    return 42;
}
```

