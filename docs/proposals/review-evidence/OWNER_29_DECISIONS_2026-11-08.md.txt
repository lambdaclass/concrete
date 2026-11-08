# The 29 owner decisions, 2026-11-08

Owner, 15:08 UTC, verbatim: "ok but i said already all yes based on the best long term reviews of each agent and what we said before right?"

Each item is decided with the final pick shown below. Every item went through a Codex/Kimi/agy round. Items 3, 14 and 18 were revised and got a second round. "REVISES" marks the decided row an item changes; a revision gets a dated revision note.

## A. Process
1. When Codex wrote a branch, the review leg uses Kimi or agy as the reviewer, never Codex. The reviewer model is recorded in the note. REVISES: the AGENTS.md writer/reviewer text and the review.sh Codex-only leg.

## B. Lexer and syntax
2. The operator and punctuation token list from the grammar sheet is closed. A token added later needs an explicit revision.
3. `'a'` is an ASCII byte literal of type u8 (equal to 97u8), with its own small escape list. Non-ASCII content is refused. The lexer tells it apart from `'label` by looking for the closing quote. Tests pin down both the literal and the label distinction. REVISES: language item 16, which removed char and migrated quoted ASCII literals explicitly. This item fixes the spelling.
4. No raw tab or other control character inside a string. Write `\t`.
5. Numbers allow:
   - suffixes such as 200u8;
   - the prefixes 0x, 0b and 0o.

   They refuse:
   - `_` separators;
   - malformed suffixes such as 123u9, instead of splitting them into tokens.

## C. Slice 1
6. Slice 1 admits a stated part of the existing spec, with proofs. The spec is not cut down.
7. Consuming a linear value is written `move t`.
8. `abort()` appears only as `return abort();`. It never produces a value and runs no cleanup.
9. Concrete.toml:
   - each table has its own version;
   - the policy is chosen by name, for example `profile = "predictable"`;
   - trusted assumptions have an explicit setting;
   - version 1 fixes exactly what each named policy means.
10. R-0600, the "always aborts" fact, belongs to slice 5, not slice 1. REVISES the R-0600 placement, if it currently says slice 1.

## D. Floats
11. NaN, following the Codex/Kimi version:
   - operations that can create NaN need a proof, or a checked form;
   - arithmetic that does produce NaN gives the one canonical bit pattern;
   - bit conversions, abs, neg and copysign preserve the exact bits;
   - `==` follows IEEE, and a separate `bit_eq` compares bits.

   This also answers STD_COVERAGE S5 as option (a).

## E. CLI
12. Each file is checked separately, and checking within a file stops at its first error.
13. The exit codes are the same for every command:
   - 0: OK;
   - 1: refused;
   - 2: incomplete;
   - 3: usage error;
   - 4: internal error.

   `concrete facts` doubles as the check mode.
14. `help` exists now. `emit`, `trace` and `interp` each arrive once their data exists. There is no `run` command: build, then execute the binary, is the one way to run a program. `interp` runs the spec's evaluator. REVISES the slice-0 CLI surface decision.
15. Formatter modes are `--check`, `--write` and `--stdin`, and combining them is refused.
16. `concrete schema` and `concrete capabilities` exist, for agent discovery.
17. A `[policy]` key sets the minimum proof strength for the user's own contracts and rejects vacuous proofs.

## F. Foundation
18. Q1, constant initializers:
   - an initializer may contain fallible primitive operations, inside a closed initializer grammar with no calls, loops or effects;
   - every initializer is evaluated at compile time, and the kernel checks that it succeeded;
   - a failure, or running out of fuel, refuses the constant;
   - `1 + 2` is admitted;
   - there is one operation set, shared with runtime code.

   REVISES TYPE-22 / Q6.4, and language item 26's "closed total initializer subset".
19. Q2: a constant may refer to one declared later. Cycles are refused, and the error shows the chain.
20. Q3: bytes that are not one whole stored pointer have no pointer meaning. A named bytes-to-address conversion is added only when a workload needs it.
21. The C1 change is confirmed: a cleanup step can be refused, and a refused step ends the run stuck. REVISES the C1 cleanup semantics.

## G. Std
22. The std placement plan (STD_PLACEMENT.md) and O1-O12, as recommended.
23. S1 (a): a safe place-level `replace(p: &mut T, x: T) -> T` at slice 13, and a two-index view `swap` at slice 15.
24. S2: a view may lend several of its elements at once as shared, read-only borrows. Two mutable borrows at once: never.
25. S3, the stream interface, staged as recommended in STD_COVERAGE §6 S3:
   - slice 18: per-family functions for StdOut and File, with one partial-write law stated once in Lean over an abstract writer;
   - slice 20: one generic function with an explicit capability parameter, taking the write operation as a function value;
   - the two are compared in probe D4, and the Socket case comes at slice 27;
   - an associated-capability trait only if both fail.
26. S4: a borrowing traversal uses the decided function pointer plus explicit context. Probe D3 (slices 20-21) compares it with a coordinate cursor, and that probe has the final say.

## H. Sign-offs and settings
27. Sign-off of the LEGACY sweep 35b489e4 (wip/legacy-sweep-2), "the legacy sweep" in the original question. It is not the roadmap sweep.
28. The main protection proposal d6159bf6 is approved. The owner applies it on GitHub personally, and agents never apply GitHub settings.

## I. Planning
29. The showcase programs are sketched before the C1-C10 freeze, to test the design.

Not covered by this answer:
- item 30, B's sign-off of e997548d;
- the three C5 questions (activation targets, direct-owner call loans, the C1 erasure contract).
