**DECIDED (owner, 2026-11-06: option (a) on every question, [decision](../../decisions.md#decided-project-infoflow-2026-11-06)); archived. Reviewed by Codex, agy and Kimi.**

Non-normative historical record: the spec is [ConcreteSpec/](../../../ConcreteSpec/).

# Information flow, secrets and constant time (proposal v2, 2026-11-06)

## Recommendation

There are two claims. Authority rule 14 keeps them separate, each with its own assumptions.

- **Noninterference (NI).** Take two runs whose public inputs are equal and whose ordered release
  traces are equal (each release is a pair of site and value). They give equal public observations: console, files, network, exit status,
  trap and abort messages, and foreign calls.
- **Constant time (CT).** Under the same hypothesis, the two runs give equal *leakage traces*:
  - branch decisions;
  - memory addresses;
  - allocation sizes;
  - loop counts;
  - operands of variable-time operations.

This is release-sensitive security (delimited release). After `declassify`, the released value
may steer control flow, because both runs agree on it by hypothesis. Each claim is a theorem
about the source semantics of every accepted program. The theorem is proved once for the type
system. At the binary, each claim holds only under named backend premises (see 7).

1. **Surface: `Secret<T>`, with two labels (public and secret).**
   - There is no implicit conversion, and `classify(x)` raises a value to secret.
   - Every output primitive, foreign parameter and raw-pointer conversion takes public types
     only.
   - The single exception: a trusted-base entry may take a `Secret` parameter to native code,
     with a named CT/NI premise. No other path exists.
   - Function values carry `Secret` in their parameter types, so callbacks are typed like any
     other call.
2. **No secret-dependent control flow, addresses, sizes or counts.**
   - A `Secret` cannot be any of these:
     - a condition;
     - a match scrutinee;
     - an index;
     - a loop bound;
     - an allocation size or length;
     - a shift count;
     - a divisor.
   - The operations on secrets form a closed per-target table of trap-free constant-time
     primitives:
     - `wrapping_add`, `wrapping_sub` and `wrapping_mul`;
     - `^ & | !`;
     - shifts and rotations by public counts;
     - comparisons returning `Secret<bool>`;
     - `ct_select`;
     - `ct_eq` on arrays and views.
   - Plain `+ - * / %` are refused on secrets, because a trap would be a secret-dependent
     observable.
   - `Secret` values are opaque to the no-hidden-traps checker, which gets no octagon facts from
     them.
   - Termination and trap-freedom therefore never depend on secrets.
   - Lengths of secret containers are public metadata. This is owner question 4.
3. **Declassification** is the builtin `declassify(x, "reason")`.
   - It is traced like the termination permission:
     - each function reports its reachable release sites and their paths;
     - callers inherit them;
     - a policy can forbid them, or allow them only in listed functions.
   - It is not a capability. Capabilities guard resource operations (authority rule 2), and
     declassification is pure computation.
4. **Wiping.**
   - `Secret<T>` is Copy only when `T` is a scalar.
   - `Secret<[T; N]>` and every heap secret are linear, and their destruction wipes their source
     storage. This is a semantic event that no pass may remove.
   - Linearity makes the wipe happen exactly once on every returning path, error paths included.
     `abort` is the stated exception.
   - **Scope of the claim:** the value's storage in the source semantics. Backend copies
     (registers, spills, moved temporaries) are covered only by a backend obligation (7).
   - This adds `Destroy` entries beyond Q2.2's closed list of sixteen, which needs your revision.
5. **Facts:**
   - `reads_secret`;
   - `declassifies` (the release sites);
   - `ct_source`, established at source level;
   - `ct_binary`, conditional on the backend premises;
   - `wipes`, with its scope stated.

   Policies in `Concrete.toml` (the owner's decision; the entry spellings are still open) can
   require them per module.
6. **Proofs in Lean.**
   - NI and CT are 2-safety properties of the integrated semantics, each proved once for the type
     discipline.
   - The leakage trace is *erased instrumentation*, a separate judgment over the evaluator:
     removing it changes no run.
   - It is not part of `Observation`, which stays what the parent process sees.
   - Per-program theorems are needed only for what a release reveals, for example "only the
     MAC-check bit".
7. **Backends: a separate obligation.**
   - Ordinary refinement preserves behaviour, not leakage. A lowering that is correct may still
     turn a select into a branch or drop a wipe. NI/CT therefore gets its own obligation in each
     lowering: a leakage-preservation (simulation) proof down to a *machine-level leakage model*
     per target. That model lists branches, addresses, and the target's variable-time
     instructions (for example AArch64 for the LCB).
   - **Does BIR carry secret tags and `ct_select`?** Yes, as data. Reviewers split on this: agy
     said BIR should carry them; Codex and Kimi said BIR tags cannot establish CT. Both are
     right, and they answer different questions:
     - Without tags, every later stage (the LLVM printer, the LCB adapter, a binary checker)
       would have to re-infer which values are secret. That is the identity-recovery pattern the
       project refuses.
     - But tags establish nothing. The claim comes from the source theorem composed with the
       per-lowering leakage proofs, or else from a named premise.
   - On the LLVM path, `ct_binary` and the binary wipe are named premises, checked where a binary
     CT checker exists (ct-verif, Binsec/Rel). LLVM lowers a BIR `ct_select` to a
     constant-time select, best effort.
   - LP0 comes before `Secret` exists. CT and wipe preservation is therefore a later LCB
     extension, as in Jasmin's proved CT-preserving compiler.
   - The legacy machine-CT gap (`legacy/examples/constant_time_tag/assumptions.toml`) stays a
     disclosed premise until then.

## Not waiting for a crypto port

The old plan deferred all of this until a crypto port pulled it. Because the owner marked it
crucial, the following land early:

- **Slice 0:** the fact fields exist in the facts-query schema, proved "not applicable" while no
  `Secret` exists.
- **A2i:** the instrumentation *interface*, a leakage-trace judgment beside the semantics. It
  places no obligation on slice or LP0 lowering proofs until CT preservation is claimed.
- **The slice with integers and `if` plus one std source of secrets:** `Secret<T>`, its rules and
  the NI and CT theorems ship together. A secret surface is never admitted without its proofs.
  This is phase C at the latest, before any HMAC/SHA port.

## What changes

- **Decisions:** one row, "secrets, noninterference and constant time". It pulls this part of
  phase E's dependency-flow layer forward. SPARK-style `Depends` contracts stay in phase E.
- **Spec:** A2i adds the instrumentation interface; the `Secret` slice adds the typing and the
  theorems.
- **Roadmap:** an R-task with acceptance examples:
  - a refused secret branch, index or length;
  - the legacy `constant_time_tag` rewritten with `ct_eq`;
  - a release trace in the facts;
  - a wipe on every path.

  The LCB plan gains CT and wipe preservation, and CI gains the binary CT check as a premise
  check.

## Cost for people writing code

Crypto code writes `ct_select` and fixed-trip loops. That is the accepted cost in Jasmin and
HACL\*. Other code pays nothing.

## Prior art

- **Jasmin/libjade:** the model; a CT type system with a proved CT-preserving compiler.
- **FaCT:** rewrites secret `if` automatically. Refused here as hidden control flow.
- **Low\*/HACL\*:** secret integer types.
- **CT-Wasm:** a two-label typing proof.
- **Jif/FlowCaml:** lattices and release policies.
- **SPARK:** `Global`/`Depends` contracts.

## Owner questions (syntax needs your decision)

1. **Surface.**
   - (a) **Recommended:** the type `Secret<T>`.
   - (b) A `#[secret]` parameter attribute, which generics cannot carry.
   - (c) A label lattice.
2. **Secret control flow.**
   - (a) **Recommended:** refused; write `ct_select`.
   - (b) Rewritten automatically, which hides control flow.
3. **Release.**
   - (a) **Recommended:** `declassify(x, "reason")`, traced, with policies saying who may release.
   - (b) An attribute on the enclosing function.
4. **Public metadata.**
   - (a) **Recommended:** container lengths are public, and a secret length is refused.
   - (b) Secret lengths are allowed, with padded and fixed-size operations only.
5. **Copy and wiping.**
   - (a) **Recommended:** Copy only for scalars. Arrays and heap secrets are linear and wiped,
     which revises Q2.2.
   - (b) Copy follows `T`, with no wipe guarantee for arrays.
6. **Names.**
   - (a) **Recommended:** `Secret`, `classify`, `declassify`, `ct_select`, `ct_eq`.
   - (b) Other names you prefer.
7. **Observer model (what counts as leakage).**
   - (a) **Recommended:** the baseline CT model of branches, addresses and variable-time
     operations, with exact addresses (stricter than cache-line granularity).
   - (b) Also speculative leakage (Spectre-style). This needs a speculative semantics and is much
     more work. It could come later as its own claim.

Owner, 2026-11-06: OS-level sandboxing (seccomp/landlock, Capsicum, pledge/unveil) is outside the compiler's scope; users may apply it themselves.
