# Unverified historical transcription

This is archived evidence, **not a decision or proposal approved here**. The original
59-item recommendation and grammar sheet named in OWNER_QUOTES.md were not present
in the archived tag. Their absence prevents checking the blanket approval against
the wording below. No rule, spelling, or revision in this extract gains authority
from this archive. Existing owner decisions continue to govern.

Source: `archive/2026-11-10/record-language`, docs/decisions.md lines 1166–1347.

```text
- <a id="decided-2026-11-08-language"></a>**Language and process decisions of 2026-11-08** (owner,
  2026-11-08: "record, yes to all recommended", approving the list as recommended; "i prefer fully
  gone, self and as are irrelevant from what i see", on `self` and `as`; and "i like 3", on how
  imported items are used). Codex, Kimi and agy reviewed the list in six rounds and agreed on every
  item except 11(b), which the owner then decided by choosing option 3. The spellings of items 6–29
  and 40–53 are illustrative; their exact spellings come in the [spelling round](#decided-2026-11-08-spelling-round).
  The trap names (item 4) and the grammar items 30–39 are decided as written.
  The numbers are the list's.
  - <a id="decided-2026-11-08-process"></a>*Process.*
    1. *Sign-off when the review cap is reached.* The owner may approve one exact SHA after Codex,
       Kimi and agy have all reviewed it. The approval and every outstanding finding and verdict
       are recorded. It never replaces a gate or a proof obligation.
    2. *A diff that touches several classes* gets the highest of their caps (docs 2, scripts 3,
       compiler and spec 5), classified by the files the diff actually changes. This settles the
       question [review caps](#decided-2026-11-08-review-caps) left to the owner.
    3. *One review round on where each std item lives* ([std per slice](#decided-2026-11-08-std-table)).
    4. *Trap names:* `overflow`, `division_by_zero`, `invalid_shift`, `out_of_bounds`,
       `invalid_range` and `abort`. They are diagnostic categories, distinct from the error types
       of the checked operations. This answers [foundation question 9](#foundation-q9).
    5. *Slice 0 lands at `0e2950e7`*, which three reviewers accepted with CI 9/9 green.
  - *Language simplifications.*
    6. *No dot method calls:* a call is `Type::f(x)` or `Trait::f(x)`.
    7. *No receiver forms:* `self`, `&self` and `&mut self` are replaced by an ordinary typed first
       parameter. One consuming `Destroy` protocol is kept.
    8. *Constructors* always write `field: value`.
    9. *Patterns* always write `field: binder`. The rules for ignored fields and linear
       consumption are kept.
    10. *Contracts mark exact mathematical arithmetic explicitly.* Its spelling and scope come
        later; [OD12](#od12) and [OD145](#od145) are kept.
    11. *`as` is removed from the language entirely*, and is not reserved.
        - (a) Expression casts are replaced by one canonical named lossless conversion. The
          value-domain rule of [OD11](#od11) and its provenance restrictions are kept.
        - (b) <a id="decided-2026-11-08-imports-qualified"></a>Import renaming is removed. Every
          imported item is always qualified (option 3; round-6 reviewers: Codex 3, Kimi 3, agy 1).
          - Imports stay item lists (`import std::json::{parse};`); the MOD-2 manifest and
            [OD139](#od139) are kept.
          - Every imported item is used as `<last segment of its module>::name` everywhere: in
            values, types, patterns and theorem paths (`json::parse(...)`, `vec::Vec<u8>`). It is
            never used bare.
          - Items declared in the same module, and builtin types, stay bare.
          - Two items with the same name from different modules may both be imported.
          - Two imported modules with the same last segment are refused.
          - A local named like an imported segment is refused; there is no shadowing.
        - (c) Borrowing is written `borrow [mut] view = source [from a until b] { ... } [else { ... }]`.
        - (d) Unpacking is written `unpack <brand A> x = p { ... }`.
    12. *Heap move-out:* the consuming `*h` on a heap box is replaced by one named move-out
        operation. It keeps allocator identity and conservation, and stays distinct from
        destruction.
    13. *`assert(e);`* is always parenthesized, with an optional `by` suffix.
    14. *Symbol attributes* are written `#[link_name("sym")]` and `#[export_name("sym")]`.
    15. *Import paths* use `::`.
    16. *No `char`:* text is `u8` plus explicit UTF-8 operations. Quoted ASCII literals, patterns
        and comparisons migrate explicitly.
    17. *No `print`, `println` or `append` builtins.* Output is an ordinary call that needs
        `Console` and its grant, and its linear errors are kept. String append is an ordinary
        allocating operation under `with(Alloc)`, not `Console`.
    18. *Written types:* every local has a written type, and every function a result arrow,
        `-> Unit` included. Generic arguments are still inferred from the call's arguments. The
        typing rules for destructuring and ghost bindings are defined later (item 55).
  - *Removals and keeps.*
    19. *`OrderedMap` only, for now.* `HashMap` comes when the key-value store's measurements
        (slice 23) need it. Map names get no compiler privilege.
    20. *`Process`:* every foreign declaration is audited first; the capability is removed only
        if none needs it ([authority rules](#decided-2026-11-02-authority)).
    21. *`defer` is kept with late reads* (Q9.8). Reserved by-value arguments, held loans
        and cleanup order are kept.
    22. *Handwritten bodyless ordinary functions are refused.* Compiler-generated interfaces
        ([OD9](#od9), [OD39](#od39)) keep bodyless declarations, as do trait signatures and
        `extern`.
    23. *No packed structs* until a real packed foreign ABI needs them. A demonstrated ABI
        reopens the question.
    24. *Local `assume`* is removed only after its replacement, named trusted contracts with
        evidence identity, has been shown on real examples, point-local conditional facts
        included. Until then it stays explicit and tracked, and it never admits a safety
        obligation.
    25. *Function values* keep the decided design: a function pointer plus explicit context, with
        contracts.
    26. *Constants are computed at compile time*, and a constant's value must be `Copy`. Before
        Q6.4 and TYPE-22 are revised, the closed total subset of initializers is defined and the
        decided conditions for reopening compile-time evaluation are checked.
    27. *Kept explicitly:* value-producing `if` and `match`, both proof scopes, and existential
        brands.
    28. *SPARK 4(b):* an optional `#[depends(result <- a, b)]`, checked against the derived fact and
        never trusted.
    29. *SPARK 4(c):* a dead store to a `mut` local is refused, extending [OD62](#od62).
  - *Grammar*, under the grammar review's own numbering (`OPEN-n` there, not the open items of this
    file).
    30. *Source text (OPEN-1):* UTF-8 source. Whitespace is space, tab, LF and CR. A `//` comment
        ends at LF, CR or end of file. There are no block comments, BOM, shebang or NUL.
    31. *Identifiers (OPEN-2)* are ASCII and case-sensitive.
    32. *Builtin names (OPEN-4):* one closed table of builtin names, with no aliases.
    33. *Escapes (OPEN-6):* exactly two escape lists, and no raw line breaks in quotes.
    34. *Tokens (OPEN-7):* longest match. `>>` is split only in generic closers.
    35. *Aliases and constants (OPEN-10):* `type N = T;` and `const N: T = e;`.
    36. *Attributes (OPEN-15):* a closed attribute set.
    37. *`borrow` and `defer` (OPEN-19):* the restrictions as proposed.
    38. *Views and constant-time APIs (OPEN-21)* are ordinary calls.
    39. *Concurrency (OPEN-24):* no concurrency syntax until the model is reviewed.
    40. *Reserved words (OPEN-3):* `spec`, `by`, `kept`, `Self`, `while` and `Entropy`. `self`
        and `as` are not reserved: they are ordinary identifiers with no special meaning, and
        Rust-style `&self` or `x as u32` gets an ordinary parse or resolution error, not a
        dedicated diagnostic. Obsolete reservations (`in`, `newtype`, `union`, `total`) are dropped
        unless a decided row still needs them.
    41. *Number suffixes (OPEN-5):* typed suffixes like `200u8` are kept; the exact suffix
        grammar comes later.
    42. *Precedence (OPEN-8):* the legacy precedence table is kept, so chained comparisons parse.
        agy preferred refusing chains.
    43. *Item syntax (OPEN-9):* attributes, then `pub`, then the item. Commas are mandatory, and a
        trailing comma is optional.
    44. *Imports (OPEN-11):* `import a::b::{x, y};` with a nonempty item list. There is no renaming
        and no module import, which all three reviewers rejected as a second form against the MOD-2
        manifest. Uses follow 11(b).
    45. *Spec functions (OPEN-13):* `spec fn f(...) -> T = e;` and `assert(e) by path(args);`. Spec
        functions use a pure subset, and the name restriction on `ghost let` is fixed.
    46. *Generic parameters (OPEN-14):* type parameters come before capability parameters (`cap C`).
        Unions use `+`, and `{}` is empty. Capability names are never ordinary types.
    47. *Blocks (OPEN-16):* the decided statement and value block behaviour is kept, with the
        common-prefix LL(1) grammar proved.
    48. *Match (OPEN-17):* enum patterns are qualified, and every arm ends with a comma.
    49. *Labels (OPEN-18):* both loop forms take labels (`'label: while`, `'label: for`), with
        `break 'label;` and `continue 'label;`. There are no empty statements.
    50. *Arrays (OPEN-20):* no empty array literals; empty structs are kept.
    51. *Brands (OPEN-22):* an explicit `exists`/`pack`/`unpack` brand surface, and named binder
        contracts on function types.
    52. *Kept arguments (OPEN-23):* `kept buf: H`, with its paired `Kept<H>` completion, and no
        borrow-derived kept arguments (Q3.12 in full). Only pointer qualification is left.
    53. *Ratified (OPEN-25):* value `while ... else` and postfix member `->` stay refused.
  - *Later, with no decision now.*
    54. The Core freeze (A2c), after the spec-core proofs and A2i integration.
    55. <a id="decided-2026-11-08-spelling-round"></a>*The spelling round:* the exact names and
        signatures of the lossless conversion, the move-out, the mathematical-arithmetic boundary,
        output and append, and the typing rules for destructuring and ghost bindings, together
        with every spelling from items 6–29 and 40–53. They are shown to the owner as before/after code.
    56. The entropy grant's shape, at slice 6.
    57. The concurrency model, at slice 28.
    58. Niches and smaller enum tags, only if a workload needs them.
    59. The remaining open grammar sub-rules come back to the owner in the spelling round with
        item 55, as before/after code:
        - the numeric token policy (OPEN-5);
        - attribute duplication, ordering, attachment and scope, the struct-invariant scope
          included (OPEN-15);
        - zero-count array repetition (OPEN-20);
        - the closed builtin inventory, and exhaustive signatures for the view, constant-time,
          text, container and branded APIs (OPEN-4/21);
        - trait and inherent impl shells (OPEN-12);
        - brand binder scope, escape and loop-carried existential rules (OPEN-22).
  - <a id="decided-2026-11-08-language-revisions"></a>*Rows this revises*, each an explicit owner
    revision of the row named:
    - [OD6](#od6), [OD32](#od32), [OD60](#od60) (items 6, 7): there are no method calls and no
      receivers. A trait call is `Trait::f(x)`; an ambiguous call is still qualified, and
      [OD6](#od6)'s coherence stands. OD32's `(&s).m()` form is gone. OD60's single consuming
      `Destroy` form stays, written with a typed first parameter.
    - [OD130](#od130) (item 9): every pattern writes `field: binder`.
    - [OD23](#od23) (items 6, 7): the overlap rule applies to ordinary arguments; there is no receiver.
    - [OD11](#od11), [OD91](#od91), Q6.2 (item 11a): `as` is gone. OD11's value-domain rule moves to the
      named lossless conversion, and OD91's cast-precedence question disappears.
    - [OD30](#od30), the [allocation rules](#decided-2026-11-02-allocation) (item 12): the consuming
      move-out is the named operation.
    - [OD59](#od59) (item 12): the move-out is a named operation, not `*h`. `destroy(h)` stays, and
      there is still no `free` or `take`.
    - TYPE-20, [OD137](#od137) and [OD42](#od42) (item 16): there is no `char` type and no character
      literal; only `bool` keeps `==` and `!=` from OD42.
    - Q9.8 (item 21): kept as decided, confirmed with no change.
    - [OD39](#od39), [OD77](#od77) (item 22): a bodyless declaration appears only in generated
      interfaces, trait signatures and `extern`, and the grammar resync refuses it elsewhere.
    - [OD111](#od111), [OD147](#od147) (item 23): no `packed` and no `repr(C, packed)` until a
      demonstrated packed foreign ABI reopens them.
    - [OD80](#od80) (item 24): `assume` stays explicit and tracked until its replacement is shown,
      and is then removed.
    - Q6.4, TYPE-22 (item 26): compile-time `Copy` constants. The rows are revised once the
      closed initializer subset is defined and the reopening conditions are checked.
    - [OD62](#od62) (item 29): a dead store to a `mut` local is refused.
    - Q4.6, Q4.7, Q9.5 and the text-only slice of R4 F4.3 (item 11c): the borrow form is `borrow [mut] view = source ...`.
    - MOD-2, MOD-3, [OD151](#od151) (items 11b, 44): OD151's multi-level paths are written in
      imports, and uses are qualified by the last segment. MOD-2's item-list manifest stands. MOD-3 is revised: two
      same-named items from different modules may both be imported, each used qualified; two
      modules with the same last segment are refused.
    - [OD90](#od90) (item 40): `self` is an ordinary identifier with no special meaning. `Copy`
      stays reserved.
    - [Foundation question 9](#foundation-q9) (item 4): the trap names above.
    - [Review caps](#decided-2026-11-08-review-caps) (items 1, 2): the sign-off path and the cap
      for a diff that mixes classes.
```
