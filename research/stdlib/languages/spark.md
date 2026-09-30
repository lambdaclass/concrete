# SPARK / Ada Stdlib Packet

Status: research (verified 2026-07-12 against SPARK UG/RM 27.0w + AdaCore/SPARKlib source)

Source pointers: SPARK User's Guide + Reference Manual
(docs.adacore.com/spark2014-docs), the `SPARK.Containers.Formal.*` source
(github.com/AdaCore/SPARKlib), learn.adacore.com.

Why this packet matters: SPARK is the mature high-assurance stdlib and the model
for Concrete's **proof-friendly pure core** (`option`/`result`/`bytes`/`numeric`).
It answers the one question the other packets don't: what makes a stdlib module
mechanically provable.

## What SPARK Has

- 12 **formal containers** (`SPARK.Containers.Formal.Vectors` / `Ordered_Maps` /
  ...) provable where the normal Ada containers are not, because: bounded storage
  (index into a fixed array, no hidden allocation), cursors carry no container
  reference (no aliasing), a narrowed API (no callback-taking `Update_Element`),
  and — the key — a **ghost mathematical model** (`Formal_Model`: sequence/map)
  that contracts are written against.
- Contracts as aspects: `Pre` / `Post` / `Contract_Cases` (proven disjoint +
  complete = totality) / `Global` / `Depends`. Real: `Append` has
  `Pre => Length < Capacity` (overflow is not even expressible), `Post` frames the
  new length and preservation of prior elements.
- Effect discipline: a side-effect-free function has `Global => null`, no
  out-parameters, no exceptions, and always terminates — a hard checked rule.
- `SPARK_Mode On/Off` partitions provable from trusted: a contracted (`On`) spec
  may have an unchecked (`Off`) body; the prover *assumes* the body honors the
  spec; the boundary is one-way.

## What Concrete Should Copy

1. **Model-not-representation contracts (the keystone).** Give each pure-core
   type an abstract mathematical model and write every contract against the
   model, never the layout — proofs become representation-independent and stable.
   For Concrete: `Option` = 0-or-1 model, `Result` = tagged-union model,
   `bytes` = finite `u8` sequence + length, `numeric` = mathematical integers +
   explicit bound predicates.
2. **Bound everything so allocation/overflow isn't expressible.**
   `Pre => Length < Capacity` beats a runtime check — the failure mode is proven
   absent. Feeds the fixed-capacity core.
3. **`Global => null` as the pure/no-effect marker** — the analogue of Concrete's
   "no capability ⇒ no IO." Any effect must surface as a declared global/capability.
4. **The trusted-boundary seam.** A contracted spec over an unchecked body
   (`SPARK_Mode Off`) = Concrete's `trusted`/`Unsafe`, where the proof-class
   degrades to `trusted-boundary` and the obligation becomes manual review. Feeds
   the surface-manifest proof-class column (Phase 7 item 2a) and the
   certificate-chain contract (Phase 6B item 14a).
5. **`Contract_Cases` (disjoint + complete)** as a totality-for-free pattern for
   pure-core partial functions.

## What Concrete Should Not Copy

- Ada surface syntax and aspect-heavy annotation density on every subprogram —
  Concrete's proof facts should derive from types + capabilities, not per-function
  aspect blocks.
- The full 12-container matrix now — take the *shape* (bounded, model-annotated)
  for the pure core, not the whole set.
- Cursors at all — Concrete already decided against them (see `iterators.md`).

## Missing Concrete Items This Pressures

- A mathematical-model convention for pure-core types (feeds Phase 7 #10
  `formal_vec`/`formal_map` and the manifest proof-class column).
- A bound-as-precondition idiom for fixed-capacity collections.
- Explicit degradation of proof-class at the trusted/Unsafe seam.

## Concrete Classification

- Copy now: model-not-representation contracts, `Global => null`-style purity
  marker, trusted-boundary seam, bound-as-precondition.
- Feeds later: `formal_vec`/`formal_map` (Phase 7 #10), certificate-chain (Phase 6B #14a).
- Reject: Ada surface, aspect density, cursors, full container matrix.

## Flow Analysis, External State and Foreign Code (added 2026-09-30, for R-0484)

This section answers a different question from the packet above: not "what makes a stdlib
provable" but "how does SPARK keep effect declarations honest across pointers, hidden state
and foreign code". It is split into verified facts, our interpretation, and the Concrete
decisions they informed, which are recorded in
[docs/language/HANDLE_CAPABILITIES.md](../../../docs/language/HANDLE_CAPABILITIES.md).

**Sources and versions.** User's Guide and Reference Manual pages as served on 2026-09-30,
labelled 27.0w; release notes for SPARK 24 and 25 (their own labels); one claim from GNAT's
library source, marked. The live documentation mixes release labels across pages, so each
claim should be re-checked against a pinned release before it is cited normatively. Sphinx
anchors were not individually checked.

### Verified facts

- **`Global` contracts** (modes `Input`, `Output`, `In_Out`, `Proof_In`; `Global => null`
  for none). GNATprove checks the body reads/writes exactly the listed globals — an "if
  and only if" rule (RM 6.1.2), and a contract must be "complete or not present at all".
  ([subprogram contracts](https://docs.adacore.com/spark2014-docs/html/ug/en/source/subprogram_contracts.html),
  [RM subprograms](https://docs.adacore.com/spark2014-docs/html/lrm/subprograms.html))
- **Absent contracts are generated**, graded *exact* / *precise* (from a SPARK body) /
  *coarse* (from a non-SPARK body: everything written is In_Out, every output depends on
  every input). The guide says coarse generation "can also result in incorrect
  contracts" (missed writes through access values passed as `in`; one lumped `__HEAP`).
  ([generation of dependency contracts](https://docs.adacore.com/spark2014-docs/html/ug/en/source/how_to_write_subprogram_contracts.html#generation-of-dependency-contracts))
- **`Depends`** states which outputs depend on which inputs; the default is all-on-all.
  The guide presents it as optional ("you don't need in general to add such contracts")
  and names certification data-coupling objectives (DO-178) as a main reason to write
  one. ([usage scenarios](https://docs.adacore.com/spark2014-docs/html/ug/en/usage_scenarios.html))
- **Hidden state:** `Abstract_State` / `Refined_State` / `Part_Of` / `Refined_Global` /
  `Initializes` group implementation state under abstract names.
  ([package contracts](https://docs.adacore.com/spark2014-docs/html/ug/en/source/package_contracts.html))
- **External state** models devices and I/O: properties `Async_Readers`,
  `Async_Writers`, `Effective_Reads`, `Effective_Writes`, all True when unspecified. A
  non-External abstraction may not contain External constituents, and an External
  abstraction carries every property of its parts (RM 7.1.2, 7.2.8).
  ([RM packages](https://docs.adacore.com/spark2014-docs/html/lrm/packages.html))
- **Text_IO** carries one abstract state, `File_System` ("the memory on the system and
  the file handles"); `Put` is `In_Out => File_System`. Per GNAT's source spec (not the
  manual) it is declared without `External`. `Set_Output` and friends are excluded from
  SPARK because they create aliasing.
  ([SPARK libraries §5.11.9](https://docs.adacore.com/spark2014-docs/html/ug/en/source/spark_libraries.html),
  [a-textio.ads](https://github.com/gcc-mirror/gcc/blob/master/gcc/ada/libgnat/a-textio.ads))
- **Printing outside the model:** the I/O library documentation describes wrapping output
  in a procedure with `Global => null` and an unanalyzed (`SPARK_Mode => Off`) body, when
  output is not relevant to the analysis. This is an explicit modelling choice: output is
  deliberately excluded from what the analysis reasons about.
- **Imported subprograms without a `Global`:** specifying one is "compulsory to specify…
  otherwise GNATprove assumes `null` data dependencies" — no global reads or writes. A
  body under `SPARK_Mode => Off` instead gets a coarse generated contract. A guaranteed
  "assumed Global null" warning is documented for partial analysis across units; that a
  plain `Import` always warns is unverified.
  ([contracts on imported subprograms, §7.4.7](https://docs.adacore.com/spark2014-docs/html/ug/en/source/how_to_write_subprogram_contracts.html#writing-contracts-on-imported-subprograms))
- **Assumptions are reported:** `--assumptions` writes remaining assumptions to
  `gnatprove.out` (currently only those on called subprograms), and the team guide names
  a review checklist ([ADA_SUBPROGRAMS], [ADA_STATE_ABSTRACTION], [SPARK_JUSTIFICATION]).
  ([managing assumptions](https://docs.adacore.com/spark2014-docs/html/ug/en/source/how_to_use_gnatprove_in_a_team.html#managing-assumptions))
- **Access to subprograms:** `'Access` requires the subprogram to have no global inputs
  or outputs; `Side_Effects`/`Volatile_Function` subprograms cannot be taken by access.
  The stated reason: data-dependency contracts "are not currently allowed on
  access-to-subprogram types". Effects on explicit parameters are not restricted by this.
  ([access types](https://docs.adacore.com/spark2014-docs/html/ug/en/source/access.html))
- **Functions are pure by default**; SPARK 25's `Side_Effects` relaxes this explicitly
  ("facilitates writing bindings to C libraries"), allowed only in statement positions,
  never in assertions (RM 6.1.13).
  ([SPARK 25 release notes](https://docs.adacore.com/live/wave/spark2014-release-notes/html/spark2014_release_note/release_notes_25.html))
- **Pain points on record:** generated globals "may not meet expectations… flow analysis
  and AoRTE proof could pass, but your code would not meet its requirements"
  ([AdaCore blog](https://www.adacore.com/blog/showing-global-contracts-with-gnat-studio));
  Text_IO's single state makes two-file preconditions unprovable.

### Our interpretation

- SPARK's conclusions about foreign code are only as good as the foreign contracts users
  supply; a missing contract on an import is read as "no global reads or writes". That is
  a documented design choice, not a flaw in the analysis — but it means an omission and a
  deliberate `null` look the same to the tool.
- SPARK's printing workaround and Concrete's `console_write` hole have the same shape
  (output invisible to the analysis) but different status: SPARK's is an explicit,
  documented modelling choice; Concrete's was accidental effect loss.
- SPARK restricts pointers to subprograms with global effects because its pointer types
  cannot carry data-dependency contracts. A pointer type that carries its capability is a
  way to lift that restriction rather than impose it.
- The RM 7.2.8 rules show that a classification can be preserved through grouping; they do
  not by themselves make a descriptor safe. Preservation and auditable reclassification
  are separate requirements.
- Text_IO's coarse `File_System`, with redirection excluded rather than modelled, is
  consistent with capabilities meaning logical authority — but excluding redirection is
  an aliasing decision, and typed classification alone does not settle aliasing.
- `Depends` supports modular reasoning about data dependencies as well as certification.
  It is broader than information-flow security.

### Concrete decisions this informed (R-0484)

- **Refuse an undeclared foreign binding** rather than read it as "no effects"; an
  explicit empty declaration is the audited claim.
- **Keep effects out of `trusted` bodies:** Concrete does not adopt the
  "declare-nothing wrapper over an unanalyzed body" pattern for output.
- **Carry the capability in the handle type** (`Writer<C>`) rather than forbid effectful
  function pointers.
- **Report assumptions by category,** each recording its declaration or conversion site,
  responsible boundary, and the claims that depend on it.
- **Preserve classification** through ordinary wrapping and duplication; require an
  explicit, reported justification for any reclassification.
- **Defer `Depends`**, recorded as a later candidate rather than rejected.
- **Do not generate** effect contracts from bodies Concrete cannot analyze.
