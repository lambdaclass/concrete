# Thesis Threat And Accident Model

**Status:** Open

This note names the changes Concrete's thesis workflow should catch early.

It is not a complete security model. It is the near-term adversary / accident model for reports, predictable checks, proof evidence, semantic diff, policy gates, and attacker-style demos.

## Concrete Should Catch

1. a dependency adds `Network`, `File`, `Process`, `Random`, `Alloc`, FFI, blocking, or trusted code
2. a pure/parser-core function starts allocating
3. a predictable-profile function gains recursion or an unbounded loop
4. a bounded parser moves an I/O operation into the parser core
5. a proved function changes body and keeps the same name
6. a proved function moves modules and accidentally matches the wrong proof
7. a proof is missing, stale, body-mismatched, identity-mismatched, or targets an unsupported fragment
8. a trusted wrapper expands from small pointer manipulation into broader authority
9. a host-backed stdlib call is treated as transparent even though it can block or has timing opacity
10. a performance change makes runtime faster but expands authority, allocation, trust, FFI, blocking, proof drift, recursion, or unbounded loops

## Demo Shape

The attacker-style thesis demo should show at least one realistic before/after:

1. start with a predictable, capability-free, proof-backed parser core
2. add a malicious or accidental change, such as network exfiltration, allocation, unbounded loop, broader trusted wrapper, or body change
3. run the normal check/report workflow
4. show the compiler/report refusing or downgrading the claim

## Why This Matters

Concrete's unusual promise is not "bugs are impossible."

The promise is that authority, execution risk, trust boundaries, and proof/evidence status should become visible enough that dangerous drift is hard to hide.
