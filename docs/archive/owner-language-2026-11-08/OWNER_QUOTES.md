# Primary owner record excerpts

Selected excerpts from `docs/proposals/owner-2026-11-08/PENDING_RECORDS.md`
at source commit `054c5c4a5ae083c1ee72ff17d1fc753e0b5a251c`.
The quoted owner words are primary evidence; the surrounding explanations are the recorder’s context.
Excerpts retain source wording; headings introduced for navigation are editorial. Intervening
entries are omitted. Dispositions and rules are recorded only in [decisions.md](../../decisions.md#decided-2026-11-08-language).

## Language approval and qualification

- 2026-11-08, verbatim: "i prefer fully gone, self and as are irrelevant from what i see". This answers the reserve-or-remove question for `self` and `as` (after items 7 and 11-full remove receivers and every use of `as`): neither is reserved; both are ordinary identifiers with no special meaning. Rust-style `&self` or `x as u32` gets an ordinary parse/resolution error, not a dedicated diagnostic. Item 40's reserved list becomes spec, by, kept, Self, while, Entropy (obsolete reservations dropped). The 59-item list itself (scratchpad/verdict2/LIST.md + verdict3/DELTA.md + verdict4/DELTA.md, reviewed unanimous by Codex/Kimi/agy except 11(b) imports, in round 5) is NOT yet approved.

- 2026-11-08, verbatim: "record, yes to all recommended". This approves the full language/process list as recommended: scratchpad/LANGUAGE_DECISIONS_2026-11-08.md (items 1-59, with item 11 = remove `as` entirely and item 40 per the "fully gone" answer). Includes item 5 (land slice 0 at 0e2950e7). Item 11(b), how two same-named imported items are used, is still under review (round 6, options 1-3, Claude recommends 2) and is NOT covered by this approval; it goes back to the owner.

- 2026-11-08, verbatim: "i like 3". This answers item 11(b): option 3, always qualified. Imports stay item lists (`import std::json::{parse};`, MOD-2 manifest and OD139 kept); every imported item is used as `<module last segment>::name` everywhere (values, types, patterns, theorem paths), never bare; items declared in the same module stay bare; builtin types (u8, bool, ...) stay bare. Two imports of the same item name from different modules are allowed (revises MOD-3). Two imported modules with the same last segment are refused. A local named like an imported module segment is refused (no shadowing). Reviewers round 6: Codex 3, Kimi 3, agy 1; Claude revised to 3. Exact spelling in the spelling round.

## 2026-11-08 15:08 UTC owner (verbatim), answering the 29 questions
"ok but i said already all yes based on the best long term reviews of each agent and what we said before right?"
Taken as: all 29 decided as Claude's final picks after the three-model rounds, including the revised 3, 14 and 18:
- 3: `'a'` is a u8 ASCII literal, with tests;
- 14: `help` now, no `run`, which revises the slice-0 surface;
- 18: fallible primitive operations in a closed initializer grammar, with no calls, loops or effects; success is kernel-checked, every initializer is evaluated, running out of fuel refuses, and TYPE-22 is revised.
The final list as shown to the owner is in the session transcript at 2026-11-08T15:08Z.
Item 28: the owner applies d6159bf6 personally.
Item 27: the sweep 35b489e4 is signed off.
B e997548d was asked separately (item 30) and is not covered by this reply.


## 2026-11-08T15:31:59Z owner (verbatim)
"okay with your decisions"
This answers three pending asks, with Claude's picks as shown:

### Lexer spellings
Block (a)-(d) unanimous:
- (a) Byte-literal escapes are exactly \n \t \r \0 \\ \'. \" and \xHH are refused. Raw content is U+0020-U+007E excluding ' and \.
- (b) Strings refuse raw: all of Cc (U+0000-U+001F, U+007F-U+009F), U+2028-U+2029, and the bidi controls (U+061C, U+200E-U+200F, U+202A-U+202E, U+2066-U+2069). This is explicitly approved as a new restriction. Byte literals allow raw U+0020-U+007E only.
- (c) Prefixes are lowercase only (0x/0b/0o), and hex digits are lowercase only.
- (d) The suffix set is exactly i8 i16 i32 i64 u8 u16 u32 u64, on every base, with no float suffixes. Malformed adjacent text is one refused token.

The two split questions:
1. A raw TAB inside a // comment is PERMITTED. Other Cc, U+2028/9 and bidi controls are refused in comments, and LF/CR end a comment.
2. Suffixes are COMPLEMENTARY. A suffix is required where no expected-type position (OD138) exists, and refused where one does. This revises OD31/OD138, and the Q6.2 example becomes S { a: 200 }.

### C9 identity
1. FunctionRelativeKey is a structured path of (construct kind, name) segments. Unnamed items are keyed by an ordinal among same-kind siblings under the nearest named ancestor, with an optional user label to pin one. There are no raw child indices.
2. PackageId has structured name and version fields, with no content hash; a hash belongs to artifact identity. The same name and version from different origins is refused at load, consistent with OD44.
3. A new premise category is added only when a decided row names it. The schema is versioned, unknown categories are refused, and there is no open-string escape hatch.

### Codex login
Codex device-auth re-login for agent on concrete-ci-2 (option A: main's review.sh on concrete-ci-2 for review-access). Approved.

## Later slice-0 sign-off

- 2026-11-08, verbatim: "sign off ea5181b5, 1547734f8, a27fbb842, yes redesign". Owner sign-offs under process item 1 (exact SHAs reviewed by Codex, Kimi and agy):
  - wip/slice0 at ea5181b535fd2983abfcafe31b3ff7bf59ec6439 (Codex verification ACCEPT, Kimi ACCEPT, agy ACCEPT after withdrawing; branch CI 42451061873 18/18);
  - wip/record-language at 1547734f86e83d381f6cac2f25d2810b083bc9ed (Codex, Kimi verification ACCEPT, agy ACCEPT);
  - wip/one-repository (B) at a27fbb842 (all three ACCEPT at cap 3).
  - "yes redesign": guard A and the acceptance shim are redesigned: shrink the landing scripts first (process decision 3: drop bot, notify, install-bot, systemd units), then B (one repository, lambdaclass/concrete only) lands BEFORE the foundation freeze, then server-side protection of main (GitHub rulesets/branch protection: only a train merge with required checks green updates main) as the real boundary, then a much smaller local guard as a convenience. wip/remote-guard (b60670804) and wip/acceptance-shim (eca268078) are superseded.
