# F31 scalar migration

F31 makes [Spec/Scalar](../../ConcreteSpec/Spec/Scalar.lean) the authority for
integer tags, range, arithmetic and bit encoding. The [F30 bridge](../../Concrete/BIR.lean)
relates the legacy tags and payloads to Core without changing their meaning.
Slice 1, Slice 0 and BIR each keep their existing admitted domains and
failure behavior during the migration.

## Census

Run the census from the development shell with a new destination whose parent exists.

```sh
nix develop -c python3 -B scripts/spec/f31_census.py --output /path/to/new-inventory
nix develop -c bash scripts/checks/compiler/f31_census.sh
```

The census snapshots the working contents of Git-tracked paths, builds both
Lean roots in private, and elaborates tracked Lean modules and script probes.
The frontend emits declarations with kernel dependencies and commands with
elaborated term dependencies. The latter include anonymous examples and
references in probes. Each command is parsed a second time in the original
frontend state to observe command-category selections. The observation is
accepted only if erasing its markers recovers the original syntax and parser
position. Quotation depth excludes quoted commands.

The report gives direct references to legacy declarations and references
through dependent Lean declarations. Generated declarations are included,
including helpers Lean realizes while elaborating a consumer. Dependency
propagation follows each file's import closure. Standalone probes can define
the same name independently.
The legacy seeds are the slice scalar vocabulary, BIR scalar types, values,
arithmetic and bridge, the old Rep integer decoder and the LLVM encoding entry
point. The surface manifest inventories tools, tests and goldens for review
alongside the Lean dependency graph. Opaque strings and external native code
have no Lean dependency edges. Surface rows record their tracked path and
content digest for manual reconciliation. The destination is published once,
with both reports and their digests.

The [compiler fixture manifest](../../tests/manifest.tsv) drives literal and
arithmetic success, overflow and trap-site observations on both backends via
the [BIR gate](../../scripts/checks/compiler/tests.sh). The
[trusted-base golden](../../tests/golden/slice0_trusted_base.txt) names the
F30 bridge through the [premise registry](../../Concrete/Premises.lean).
The [spec](../../scripts/proofs/axioms.golden) and
[compiler](../../scripts/proofs/compiler_axioms.golden) axiom goldens cover
both theorem populations. Reconcile those files with the corresponding migration step and
the resulting theorem names.

### Round 1 result

The census on the F30 base at `82d3793ce` with the round 1 LLVM change
took 4m 8s. It inventoried 188 tracked paths, including 67 Lean modules and
9 Lean probes. Lean emitted 17,052 declaration records and 5,553 command
records. Direct references appeared in 1,463 declaration records and 352
command records across 27 files. The closure, including legacy declarations
and transitive consumers, covered 2,340 declaration records and 714 command
records across 29 files. Those are observations, including generated
helpers, and are useful for sizing the slices.

The direct consumers include the compiler lexer, Surface0, the diagnostics
representation printer, the BIR emitter probe, and Hybrid and Memory proof
modules. Both Own modules enter through Hybrid. The premise registry's
quoted theorem references appear as elaborated command dependencies.

## Independently landable sequence

1. **LLVM scalar queries.** Keep the BIR boundary and switch the printer's
   width, signedness and immediate encoding to Core through the F30 bridge.
   BIR programs, result words and LLVM text continue to agree with the prior
   interface. The bridge gives equality for every tag and payload, including
   ill-typed payloads. Qualify with Laws for the immediate width, decimal-token
   parsing and Core round trip, the trusted-base golden and the existing
   two-backend slice-0 controls. The producer inventory theorem covers all
   BIR programs and the runtime items, before text rendering. Call roles carry
   their zero-based argument slot. Phi roles carry their incoming slot and
   predecessor label, including when two edges share a predecessor. Switch keys
   carry their case position and destination label. The inventory keeps a row
   for every assembly item, including empty rows, so moving an immediate
   between sibling instructions changes the inventory. The assembly reader
   recovers these rows from every emitted module, and the native fixtures
   exercise the complete writer path. LLVM execution semantics still use
   `P_print`.
2. **BIR and the runtime/backend boundary.** Switch the BIR scalar tag and
   payload, its typing and arithmetic, and every consumer in Progress, LCB,
   LLVM, Runtime and the emitter probes in one change. BIR has its own operand
   and instruction graph. It uses slice-1's integer tags and trap vocabulary,
   and its entry points consume BIR programs directly. The existing F30 tag
   bijection can still relate slice-1 tags to the new BIR tags. Preserve the
   F30 range and operation comparisons as witnesses across the two models
   while both exist. Qualify the typing invariant, overflow flag, result word
   and both backends together with positive and negative controls. Reconcile
   premise and headline references in the same change as their subjects.
3. **Slice syntax, statics, execution and storage, then retirement.** Switch
   Slice1Syntax, Statics, Slice1Semantics, Slice1Ownership, Slice1Checker,
   Slice0, Rep, Examples and their proof modules as one connected change.
   Their judgments index the same syntax and scalar values. For example,
   `SVTy` constrains the fields stored in `Val`, and the checker and evaluator
   transport those same fields through ownership and destructuring. A split
   through that boundary would require duplicate judgments or unqualified
   conversions between them. Switch Surface0's integer token and suffix
   judgments together with the compiler lexer and Surface0 proofs. Replace
   Hybrid's tag conversion and reconcile the Rep comparisons in Hybrid,
   Memory and Observe, plus dependent diagnostics and ownership proofs.
   Preserve the integer/Boolean domain in both BIR
   and the slice interfaces, using a restricted view of Core scalars where
   their wider character and unit vocabulary would admit new cases.
   Eliminate the legacy tag, scalar types and values, duplicate range and
   arithmetic definitions, and the temporary F30 bridge and its headline
   entries when the last users move. Reconcile the remaining probes, root
   exports and goldens with the same change. Run a fresh census before
   retirement and account for every remaining direct edge and surface row.

### Slice-1 text proof and controls

The assembly reader consumes each rendered item and preserves an arbitrary
suffix. The list proofs cover declaration types, definition parameters, call
arguments, phi arms and switch cases. The byte reader recovers the initializer
and checks its declared length. Dispatch checks earlier parser alternatives
before consuming each instruction. Nonempty item spellings give the parser
enough fuel for the whole list.

`Assembly.parse_render` recovers the assembly tree. `Assembly.inventory_render`
projects its integer inventory, which includes explicit widths even when all
operands are locals. `LLVM.module_inventory` composes with the BIR inventory
law for `LLVM.module`, the exact string passed to `IO.FS.writeFile` in
`writeBackends`. `Runtime.llvm_inventory` covers the standalone runtime text.
The invalid-word fixture appends that runtime text to a hand-authored body
returning an invalid result word. It exercises the runtime's rejection path
through the native tools. The hand-authored body and concatenated fixture
are outside the normal BIR lowering and its module inventory law.

Each renderer or producer mutation control must prove a concrete inequality
for the target law against the actual mutated definition. The witness uses
the real definitions and the independent expectation, with a kernel-checked
proof that does not depend on the affected law or its proof helpers.
Elaboration errors, recovered proofs and tactic failures do not count as
kills. A double reversal of the BIR instruction list must survive the kill
check and have a kernel-checked equality on its witness.

Slice 1 and Slice 2 leave the F30 comparison available at the boundaries
that still carry the old tags. Slice 3 is the final type substitution at
those boundaries. The sequence changes no decision about Boolean storage,
additional scalar cases, or the execution and trap vocabulary scheduled for
F35. The existing Boolean storage authority conflict remains under ISS-0069.
