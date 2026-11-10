### Lexer round 4: canonical spellings and recognition boundaries

Repairs agy's four findings on 5b0e63189 (ISS-0035 through ISS-0038).
The intended owner-decided spellings stay unchanged: `i32` and `u32` replace their erroneous
codepoint transcriptions. Suffix decoding derives from `intTySpelling`. Kernel-checked
readable-literal comparisons cover every suffix and radix prefix and both complete escape
relations, including their decoded values. Negative cases refuse `i23` and `u23`.

Escape-led byte inputs now either succeed with the exact closing quote or return an error,
including `unterminated`; they cannot fall through to labels. The registered uniqueness and
declarative absence proofs cover byte recognition. Comment absence now means no double-slash
opener, comment errors mean an opener with no valid body suffix, and trivia soundness and
completeness use the absence theorem.

Lean workers default to four in the shared limits library, inherited by every guarded gate.
Smaller explicit requests are preserved; larger requests are capped and invalid requests fail.
The root check `lean_threads.sh` exercises that policy independently of host core count.

Codepoint audit: reviewed every decimal list and list pattern in
`ConcreteSpec/Spec/Surface0.lean` and `Concrete/Lex.lean`. The spelling tables are exactly
`intTySpelling`, `Radix.basePrefix`, `ByteEscape` and `StringEscape`; no keyword or punctuation
table is encoded here (longest matching accepts a caller's table). Also checked the scanner
patterns/decoders against those definitions: slash 47, quotes 34/39, backslash 92, escapes
n/t/r/0 and decoded values, prefixes 0b/0o/0x, whitespace 32/9/10/13, identifier/digit
bounds and positional digit values. Other lists are conformance inputs/results (UTF-8,
source refusals, comments, trivia, identifiers, strings, bytes, labels, generic longest-match
tables and integers); the i32/u32 positive cases shared the transposition and were corrected.
No other transcription error was found. Non-ASCII Unicode constraints use hexadecimal values.

Independent controls were run against a private source copy of 5b0e63189 and the repaired
branch: suffix spellings/refusals, all four malformed byte inputs, declarative comment absence,
and default worker count. Each control exits 1 on baseline and 0 after repair; the baseline
build exits 0. Checked-in Lean conformance cases and theorem statements preserve the semantic
regressions; lean_threads.sh preserves the shared-environment regression.
