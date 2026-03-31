# Binary Format DSL And Derived Parser/Serializer Pairs

Status: research

## Problem

Concrete is explicitly interested in protocol validators, manifest/config checkers, and other audit-heavy low-level components.

That makes binary formats an attractive area:

- packet parsing
- firmware/update metadata
- compact on-disk records
- wire-format validation

The tempting idea is a dedicated format DSL that can derive:

- a parser
- a serializer
- round-trip properties
- layout/constraint reports

## Why This Is Attractive

If it worked well, it could give Concrete a strong story for:

- explicit parsing logic
- fewer hand-written binary parser bugs
- auditable format constraints
- proof-friendly round-trip claims

This is especially appealing because Concrete already values:

- explicit low-level structure
- reportable facts
- proof-oriented compiler artifacts

## Why This Is Dangerous

A binary format DSL is not a small convenience feature.

It risks adding:

- new grammar
- new elaboration rules
- new proof obligations
- new diagnostics surface
- pressure toward a second semantic sub-language

That is exactly the kind of feature Concrete should admit only with strong evidence.

## Best First Direction

The first move should not be a new DSL.

Prefer:

1. ordinary library patterns for parsers/serializers
2. better layout and boundedness reports
3. proof experiments over selected parser/formatter pairs
4. serious Phase H workloads that expose whether the manual approach is actually too costly or error-prone

If those paths prove insufficient, then a small format surface may become justified.

## What Would Justify A DSL Later

The idea earns another look only if multiple real workloads show repeated need for:

- declarative field layout
- length-dependent payload structure
- checksum/integrity relations
- mechanically derived parser/serializer pairs
- round-trip or constraint evidence that is hard to recover from ordinary libraries

The bar should be practical, not aesthetic.

Concrete should not add this because format DSLs are elegant.
It should add it only if real audited code becomes materially simpler, more inspectable, and easier to prove.

## Design Constraints If Revisited

If Concrete ever adopts such a feature, it should preserve:

- explicit generated authority requirements
- obvious lowering to ordinary Core
- no hidden allocation
- no hidden control flow
- reportability of layout and parse constraints
- proof/tool consumers built on the same semantic artifacts

The wrong version would look like a mini language with opaque code generation.

The right version would elaborate into ordinary explicit parsing code that the compiler can still explain.

## Roadmap Placement

This belongs in:

- **Phase O** first, as an evidence-gated idea
- later **Phase H** only if real workloads prove it
- later **Phase I/L** only if proof/report/tool consumers need to attach to it

## Current Recommendation

Do not roadmap a binary format DSL as a committed feature.

Keep it visible as a serious but evidence-gated idea, and let:

- real protocol/config workloads
- report needs
- parser proof experiments

decide whether it deserves to exist.
