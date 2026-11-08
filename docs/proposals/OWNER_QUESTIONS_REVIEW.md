# Owner questions: three-model review (2026-11-08)

One independent, read-only round by Codex, Kimi and agy (Gemini) on ten open owner questions.
This page decides nothing. It gives the owner each question, its options, the reviewers'
recommendation and their verdicts.

- Context: origin/main at b6a647bea; `docs/proposals/SLICE1_BRIEF.md` from origin/wip/slice1-brief
  (28b42ade9); `docs/proposals/STD_COVERAGE.md` from origin/wip/std-placement (abc648e4d); the
  grammar sheet v2 (`GRAMMAR_REVIEW.md`); the owner record `LANGUAGE_DECISIONS_2026-11-08.md`
  (items 1-59).
- Codex and Kimi read the files in a git-archive copy. agy answered from the same question text with
  the context inlined in its prompt (the 2026-11-08 record, grammar section 2 and OPEN-5/6/7, the
  slice-1 brief, ROADMAP rows B1/slice 0-1/R-0589/R-0600/R-0576, `docs/decisions.md` excerpts and
  the full `PHILOSOPHY.md`). No reviewer modified a file (the three copies were compared afterwards
  and are identical).
- The prompt and raw outputs are in `owner-questions-review/` (`prompt.txt`, `codex.out`,
  `kimi.out`, `agy.out`). Kimi's output includes its reasoning trace before the answer; Codex's
  includes its tool trace, with the answer at the end.
- F-NaN option (d) was added at the coordinator's request before launch; the owner leans toward
  (c) or (d).

## Table

| Question | Codex | Kimi | agy | Agreement |
|---|---|---|---|---|
| L1 item 34 and the token set | a | a | a | unanimous (a) |
| L2 second escape list after char | b | a | a | 2-1 (a) |
| L3 raw controls in strings | a | a | a | unanimous (a) |
| L4 suffixes and base prefixes | a | a | a | unanimous (a) |
| S1-scope | a | a | a | unanimous (a) |
| S1-move | a | a | a | unanimous (a) |
| S1-abort | a | a | a | unanimous (a) |
| S1-toml | b | b | a | 2-1 (b) |
| R0600 | c | b | b | 2-1 (b) |
| F-NaN | e (scoped d) | d (scoped, "d'") | c | no letter majority; Codex and Kimi same substance |
| Float `==` | IEEE | IEEE + named bit_eq | reflexive (no NaN by invariant) + named bits_eq | 2-1 IEEE |

## Per question

### L1. Did item 34 approve the section 2 token set?
Options: (a) yes, the set is closed; (b) only the rule, the inventory goes to the spelling round;
(c) approved except tokens touched by items 6/11/15/49.
- Codex: (a), runner-up (c). OPEN-7's recommendation was "exactly section 2's token set"; removing
  method calls or casts changes productions, not punctuation.
- Kimi: (a), runner-up (c). Items 55/59 do not reopen OPEN-7; no later item removes a listed token.
- agy: (a), runner-up (c). Same reading; item 59 omits OPEN-7.
- Agreement: unanimous (a). Missing option: none. Codex adds that any later token change must be an
  explicit revision.

### L2. Which literal owns a second escape list now?
Options: (a) single-quoted ASCII `u8` literal keeps `\n \t \r \0 \\ \'`, closing-quote priority
over `'label`; (b) no single-quoted literal, one escape list, `'` only starts a label; (c) a
byte-string literal with `\xHH`.
- Codex: (b), runner-up (a). Item 16 does not expressly introduce a `u8` quote literal; bytes are
  integers, text is strings, apostrophes are labels. Revise item 33 to one list explicitly.
- Kimi: (a), runner-up (b). Item 33 still requires two lists; (a) keeps the second list unchanged and
  implements item 16's "quoted ASCII literals ... migrate explicitly"; malformed extra quotes refused.
- agy: (a), runner-up (b). Same; disambiguation by closing-quote lookahead.
- Agreement: 2-1 for (a); Codex dissents. Missing options: Codex, labels are `'` plus identifier with
  no closing quote, and `'a'` and a lone `'` are refused; agy, one shared seven-escape list for both
  literals (revising item 33 while keeping `'a'`).

### L3. Raw control characters other than LF/CR in strings?
Options: (a) all refused, escapes only; (b) raw tab allowed; (c) all allowed.
- Codex: (a), runner-up (b). OPEN-6 recommended refusing raw controls, not only line breaks.
- Kimi: (a), runner-up (b). Invisible bytes hurt audit.
- agy: (a), runner-up (b).
- Agreement: unanimous (a). Missing options: with the current list, controls without an escape (for
  example 0x01) become unrepresentable in literals; Kimi offers (a') add `\xHH` if they must be
  representable (an explicit owner addition). Codex: the rule concerns decoded characters, not UTF-8
  continuation bytes.

### L4. Integer suffixes and base prefixes
Options: (a) eight type-name suffixes directly after digits, lowercase `0x/0b/0o`, suffix after any
base, no `_`, no float suffix; (b) as (a) without octal; (c) suffix on decimal only; (d) no prefixes.
- Codex: (a), runner-up (b). One suffix rule across bases; keep octal absent evidence for removing it.
- Kimi: (a), runner-up (b). `0o` avoids leading-zero octal ambiguity; floats keep OD63 expected typing.
- agy: (a), runner-up (b).
- Agreement: unanimous (a). Missing option (Codex): fix malformed-token boundaries, refusing `123u9`,
  `0b102` and `200u8foo` as malformed numerals rather than splitting them into tokens.

### S1-scope
Options: (a) the compiler admits a subset of the slice-1 spec, given by a predicate in the spec, with
preservation proofs; (b) narrow the spec to the slice.
- Codex: (a), runner-up (b). Avoids deleting formalized rules; across IRs use corresponding subset
  predicates or indexed invariants, not one source predicate.
- Kimi: (a), runner-up (b). Narrowing deletes rules needed at slices 2, 3 and 9 and re-adds them later.
- agy: (a), runner-up (b). Matches slice 0's pattern; one authoritative spec.
- Agreement: unanimous (a). Missing option: none.

### S1-move
Options: (a) source writes `move t`; (b) plain `t`, elaborated to Core `move t`.
- Codex: (a), runner-up (b). Losing access to a resource is behaviour worth showing at the use.
  Copy reads stay plain names; no optional `move`, no source `copy`. Apply the same rule at arguments,
  returns, constructors and destructuring inputs.
- Kimi: (a), runner-up (b). Q9.1 settles Core only, but its distinction argues for visible consumption.
  `move`'s reserved or contextual status needs the spelling round (it is not in item 40's list).
- agy: (a), runner-up (b). Transparency and explicitness beat brevity.
- Agreement: unanimous (a). Note: grammar sheet v2 proposed moves by ordinary reads (b), so (a) also
  changes the sheet and adds a reserved or contextual word.

### S1-abort
Options: (a) only `return abort();`, abort() bottom-typed; (b) let initializer or return value;
(c) bring `abort();` statements forward; (d) other.
- Codex: (a), runner-up (b). Divergence typing, not a fabricated Unit; abort runs no cleanup, and
  ownership soundness must cover that outcome. Missing option: a primitive divergence judgment
  without a source-spellable `never` type in slice 1.
- Kimi: (a), runner-up (b). Smallest sufficient form; does not pull OD29's Unit statements forward.
- agy: (a), runner-up (b). Avoids dead `let x: u8 = abort();` bindings.
- Agreement: unanimous (a).

### S1-toml
Options: (a) top-level `version = 1`, `[policy] predictable = true`, `[assumptions]` keyed by the
five decided kinds; (b) per-table versions and `[policy] profile = "predictable"`; (c) other.
- Codex: (b), runner-up (a). Version the policy and assumption schemas independently; a named bundle
  matches the decided "named, versioned bundle". Missing: separate schema version from bundle
  identity; version 1 pins the exact predictable bundle (termination, stack, trusted-assumption
  treatment), so a later compiler cannot silently change it.
- Kimi: (b), runner-up (a). Keep (a)'s five closed assumption kinds and unknown-key refusal. Missing:
  an explicit `trusted_assumptions` setting with its default, which Q13 requires and neither option
  states.
- agy: (a), runner-up (b). One top-level version avoids skew; booleans compose as modules tighten.
  Missing: top-level version plus a structured `[policy.predictable]` table with `enabled = true` and
  `allow_trusted_assumptions = false`.
- Agreement: 2-1 for (b). All three independently ask for an explicit trusted-assumptions setting.

### R0600
Options: (a) slice 1; (b) slice 5, remove from B1; (c) trivial `main` fact at 1, derived call-pruning
fact at 5.
- Codex: (c), runner-up (b). Slice 1 can report that `main` always aborts; propagation and pruning at
  5. Keep "always aborts", "may abort" and "never returns normally" distinct.
- Kimi: (b), runner-up (c). R-0600's own row is about abort wrappers and pruning after calls; direct
  aborts at slice 1 are already reported through R-0575; a slice-1 duplicate has no consumer.
- agy: (b), runner-up (c). Same reasoning.
- Agreement: 2-1 for (b); each side's runner-up is the other's pick.

### F-NaN
Options: (a) canonical NaN; (b) hardware/IEEE payload propagation; (c) no NaN-producing operation
without a proof or a checked form; (d) (c) as the surface rule plus (a) for NaN that still enters.
- Codex: missing option (e), a scoped (d); runner-up (d). NaN-creating operations need evidence or a
  checked form; NaN results of numerical operations are one canonical quiet NaN per width; bits are
  preserved through reinterpretation, copies and memory; sign operations are exact; foreign and guest
  boundaries carry explicit representation contracts. Literal (d) overreaches: canonicalizing every
  operation on NaN conflicts with reinterpretation and sign operations, whose exact bits Wasm exposes.
- Kimi: (d) scoped the same way (its "d'"), runner-up (c). (c) alone leaves NaN that enters by bits,
  foreign code or Wasm without deterministic semantics. Canonicalize arithmetic results only; keep
  reinterpretation, memory, `abs`, `neg` and `copysign` bit-exact. A checked-float error type needs
  its own approval, not a silent reuse of `OutOfRange`.
- agy: (c), runner-up (d). f32/f64 are NaN-free and finite by invariant: overflow to infinity is
  treated like integer overflow (evidence or checked/saturating forms); bit casts, FFI and Wasm enter
  through checked conversions (`from_bits -> Result`), so no backend canonicalization is needed.
  Missing: separate raw IEEE types (`RawFloat32/64`) for Wasm and FFI payloads.
- Proof cost of finiteness (all three agree): proving "not NaN" is not enough, since `inf - inf` and
  `0 * inf` produce NaN and finite operands can overflow to infinity and feed a later invalid
  operation. The proofs are the same under (c) and (d); canonicalization adds a backend obligation but
  removes no proof. Codex and Kimi: a separate finite-float type is not required (finite floats are
  not closed under arithmetic); start with predicates and checked forms. agy: no separate type either,
  because f32/f64 themselves become finite, which moves overflow to infinity under the no-hidden-traps
  evidence rule.
- Agreement: none on a letter. Codex and Kimi converge on the same scoped (d): proved or checked NaN
  creation, canonical NaN for arithmetic results only, bit-exact representation and sign operations.
  agy picks (c) with a stronger finite-by-invariant reading.

### Float `==`
- Codex: keep IEEE `==` (NaN unequal to itself, signed zeros equal), which legacy ARITH-6@1 already
  states; proofs distinguish it from Lean equality and need non-NaN evidence for reflexivity; a
  separately named bit equality for representation.
- Kimi: same; source `==` stays IEEE; proofs use Lean's total value equality; a named total `bit_eq`.
- agy: with NaN excluded by invariant, IEEE `==` is reflexive; a named `bits_eq` for bits; if NaN can
  be held, refuse `==` on floats not proved non-NaN.
- Agreement: all three keep a separately named bit-equality operation, and none redefines `==` as
  bitwise. ARITH-6@1 (`legacy/docs/spec/arithmetic.md`) does say "`x == x` is false for a NaN";
  agy's answer is compatible with it only because its invariant removes NaN.

## Missing options worth the owner's attention
1. F-NaN: scoped (d) (Codex (e), Kimi (d')): canonicalize arithmetic NaN results, keep representation
   and sign operations bit-exact. Literal (d) as written would conflict with Wasm.
2. F-NaN: agy's finite-by-invariant floats with raw IEEE boundary types.
3. S1-toml: an explicit trusted-assumptions setting (all three) and a pinned bundle identity per
   version (Codex).
4. L2: Codex's one-escape-list reading (refuse `'a'`) against agy's one shared seven-escape list.
5. L3: `\xHH`, if controls without an escape must be representable (Kimi).
6. L4: malformed numerals (`123u9`, `0b102`, `200u8foo`) refused as one malformed token (Codex).
7. S1-move: `move` must be reserved or contextual (spelling round), and the rule applies at every
   consuming position (Kimi, Codex).
