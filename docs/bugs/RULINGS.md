# Bug Rulings Against the Spec

Status: generated view

The [language specification](../spec/README.md) decides what is a bug. Every bug record states its
**ruling** in its ledger block (`ruling:`, [Filing a bug](README.md#filing-a-bug)): the versioned
rule the implementation contradicts (`Violates: OWN-1@2`), a **spec gap** (no rule decides the
behaviour, so the fix starts by writing the rule, which is spec work), an undecided owner question
(`Open: OPEN-17`), a rule that permits the behaviour (`Permitted by: ...`, not a bug), or **outside
the spec** (tooling, performance or library, which no language rule governs). Bugs filed before
bug 300 and not ruled yet say `Violates: unrecorded`; they must be exactly the list
`UNRULED` in `scripts/gen/bug_rulings.py`, which only shrinks (`check_bug_filing_complete.sh`).

Everything below the marker is generated from the records by `python3 scripts/gen/bug_ledger.py
--write` (`scripts/gen/bug_rulings.py` holds the format); never edit it. A cited rule must be a key
of [docs/spec/ids.tsv](../spec/ids.tsv), or the generator refuses the record.

<!-- BEGIN GENERATED: spec rulings (scripts/gen/bug_ledger.py; edit the records' ruling lines) -->

279 bug records, 276 of them ruled: violates a rule 140, spec gap 32, open owner question 3, permitted by a rule 0, outside the spec 101; not ruled yet (fixed bugs filed before bug 300) 3. Open bugs are in bold.

### By kind

| ruling | bugs | open | silent |
|---|--:|--:|--:|
| violates a rule | 140 | 3 | 68 |
| spec gap | 32 | 2 | 5 |
| open owner question | 3 | 1 | 1 |
| permitted by a rule (not a bug) | 0 | 0 | 0 |
| outside the spec | 101 | 4 | 16 |
| not ruled yet | 3 | 0 | 0 |

### By chapter

Every bug counts once under each chapter its ruling names: the rules it violates, the owner questions it waits on, the chapter a spec gap belongs to, or the rule that permits it.

| chapter | prefix | bugs | open | silent | violates | spec gap | open question | permitted |
|---|---|--:|--:|--:|--:|--:|--:|--:|
| pipeline.md | PASS | 42 | 2 | 6 | 22 | 20 | 0 | 0 |
| modules.md | MOD | 32 | 1 | 11 | 27 | 5 | 0 | 0 |
| types.md | TYPE | 19 | 0 | 10 | 17 | 2 | 0 | 0 |
| ownership.md | OWN | 16 | 0 | 6 | 15 | 1 | 0 | 0 |
| scope.md | SCOPE | 13 | 0 | 5 | 13 | 0 | 0 | 0 |
| memory.md | MEM | 12 | 0 | 7 | 11 | 1 | 0 | 0 |
| ffi.md | FFI | 11 | 2 | 6 | 10 | 1 | 0 | 0 |
| arithmetic.md | ARITH | 8 | 0 | 5 | 7 | 1 | 0 | 0 |
| references.md | REF | 7 | 0 | 7 | 7 | 0 | 0 | 0 |
| statements.md | STMT | 7 | 0 | 6 | 7 | 0 | 0 | 0 |
| patterns.md | PAT | 6 | 0 | 4 | 6 | 0 | 0 | 0 |
| trusted.md | TRUST | 6 | 0 | 4 | 5 | 1 | 0 | 0 |
| traits.md | TRAIT | 5 | 0 | 3 | 5 | 0 | 0 | 0 |
| decisions.md | OPEN | 3 | 1 | 1 | 0 | 0 | 3 | 0 |
| syntax.md | SYN | 1 | 0 | 1 | 1 | 0 | 0 | 0 |

### By violated rule

| rule | chapter | bugs | which |
|---|---|--:|---|
| MOD-1@1 | modules.md | 24 | 022, 039, 042, 069, 082, 096, 097, 098, 117, 118, 132, 151, 215, 217, 227, 230, 231, 238, 239, 241, 242, 243, 245, 262 |
| PASS-9@1 | pipeline.md | 10 | 023, 033, 056, 078, 110, 175, 213, 271, 303, 309 |
| SCOPE-3@1 | scope.md | 9 | 040, 041, 045, 050, 061, 087, 101, 176, 208 |
| SCOPE-1@1 | scope.md | 6 | 045, 087, 093, 094, 107, 124 |
| FFI-2@1 | ffi.md | 5 | 037, 051, 057, 121, **122** |
| MEM-3@1 | memory.md | 5 | 031, 034, 285, 287, 301 |
| PASS-6@1 | pipeline.md | 5 | 002, 099, 110, 177, 304 |
| TYPE-6@1 | types.md | 5 | 134, 140, 185, 210, 211 |
| MEM-4@2 | memory.md | 4 | 001, 120, 126, 133 |
| OWN-2@2 | ownership.md | 4 | 020, 093, 094, 149 |
| PASS-1@1 | pipeline.md | 4 | 052, 105, 119, 251 |
| STMT-4@1 | statements.md | 4 | 127, 136, 137, 183 |
| TRAIT-10@1 | traits.md | 4 | 052, 186, 204, 222 |
| TRUST-4@1 | trusted.md | 4 | 071, 091, 185, 188 |
| FFI-5@1 | ffi.md | 3 | 103, **114**, 129 |
| MOD-3@2 | modules.md | 3 | 028, 082, 218 |
| OWN-15@2 | ownership.md | 3 | 003, 018, 190 |
| REF-6@1 | references.md | 3 | 104, 107, 131 |
| ARITH-2@1 | arithmetic.md | 2 | 002, 088 |
| ARITH-5@1 | arithmetic.md | 2 | 128, 130 |
| ARITH-6@1 | arithmetic.md | 2 | 140, 150 |
| MEM-5@1 | memory.md | 2 | 144, 146 |
| MOD-2@1 | modules.md | 2 | **226**, 230 |
| OWN-11@1 | ownership.md | 2 | 147, 148 |
| OWN-5@2 | ownership.md | 2 | 145, 197 |
| OWN-8@2 | ownership.md | 2 | 115, 144 |
| OWN-9@2 | ownership.md | 2 | 115, 145 |
| PASS-7@1 | pipeline.md | 2 | 051, 100 |
| PAT-3@2 | patterns.md | 2 | 196, 197 |
| PAT-4@2 | patterns.md | 2 | 066, 090 |
| PAT-5@2 | patterns.md | 2 | 195, 202 |
| REF-3@1 | references.md | 2 | 092, 123 |
| REF-5@1 | references.md | 2 | 123, 182 |
| TYPE-12@1 | types.md | 2 | 199, 246 |
| TYPE-15@1 | types.md | 2 | 073, 200 |
| TYPE-16@1 | types.md | 2 | 024, 205 |
| ARITH-3@1 | arithmetic.md | 1 | 053 |
| FFI-1@1 | ffi.md | 1 | 166 |
| FFI-3@1 | ffi.md | 1 | 113 |
| FFI-4@1 | ffi.md | 1 | 037 |
| OWN-12@1 | ownership.md | 1 | 143 |
| OWN-3@2 | ownership.md | 1 | 021 |
| OWN-4@2 | ownership.md | 1 | 149 |
| PASS-10@1 | pipeline.md | 1 | 025 |
| PASS-2@2 | pipeline.md | 1 | 181 |
| REF-9@2 | references.md | 1 | 181 |
| STMT-5@1 | statements.md | 1 | 142 |
| STMT-7@2 | statements.md | 1 | 111 |
| STMT-9@1 | statements.md | 1 | 194 |
| SYN-1@1 | syntax.md | 1 | 220 |
| TRAIT-2@1 | traits.md | 1 | 186 |
| TRAIT-9@1 | traits.md | 1 | 091 |
| TRUST-7@1 | trusted.md | 1 | 198 |
| TYPE-11@1 | types.md | 1 | 253 |
| TYPE-14@2 | types.md | 1 | 036 |
| TYPE-2@1 | types.md | 1 | 240 |
| TYPE-5@1 | types.md | 1 | 192 |
| TYPE-7@1 | types.md | 1 | 193 |
| TYPE-9@1 | types.md | 1 | 239 |

### Spec gaps: rules to write

No rule decides these bugs' behaviour. Each fix starts by writing the rule (spec work, owned by docs/spec/); bugs that need the same rule share one row.

| chapter | the missing rule | bugs |
|---|---|---|
| PASS | a program every rule accepts is compiled: no stage after Check refuses it, crashes on it or emits invalid LLVM IR for it | 005, 009, 014, 016, 029, 032, 035, 044, **054**, 055, **077**, 108, 116 |
| PASS | every consumer gives an accepted program its checked Core behaviour: lowering, emission and the interpreter preserve meaning on every profile (PASS-3 states it for the native profile only) | 004, 006, 019, 038, 102, 184, 254 |
| ARITH | a floating literal denotes its correctly rounded value at the floating type it takes, and emission preserves that value | 109 |
| FFI | an `#[intrinsic]` declaration the compiler implements is not a foreign binding and need not declare effects, and a body-less declaration keeps its written `with(...)` | 112 |
| MEM | a raw pointer to a live local keeps denoting that local across control flow, and a write through either the local or the pointer is seen through the other | 302 |
| MOD | a call the compiler generates when it desugars print/println/append is bound to its runtime helper by identity, never to a same-named declaration of the program | 165 |
| MOD | a pub constant can be imported and used in another module | 233 |
| MOD | an import alias of a type denotes the imported declaration itself: the same type, with its methods and impls, in every pass | 223 |
| MOD | how a type position resolves a name: type-namespace lookup, a user type against a builtin of the same name, and a child module's type declarations | 229 |
| MOD | two packages of one program do not define the same top-level module name (or module paths are qualified by their package) | 074 |
| OWN | assigning an element of a directly named array requires its binding to be declared mut | 030 |
| TRUST | a capability variable is bound from the capability set of the argument's function type whatever expression supplies that argument, and a variable no argument determines is refused as uninferable | 063 |
| TYPE | a call of a generic function without explicit type arguments binds each type parameter from the types of the arguments given for it, whatever expression form supplies them | 308 |
| TYPE | a call's explicit type-argument list has exactly as many types as the callee has type parameters | 221 |

### Open owner questions

| question | bugs |
|---|---|
| OPEN-16 | 212 |
| OPEN-17 | **216** |
| OPEN-22 | 189 |

### Permitted by a rule (not a bug)

| rule | bugs |
|---|---|
| — | — |

### Outside the spec

| reason | bugs |
|---|---|
| tooling | **049**, **058**, 059, 060, 062, **064**, 065, 067, 068, 075, 076, 079, 080, 081, 083, 084, 085, 086, 089, 106, 135, 179, 180, 191, 203, 206, 207, 209, 214, 219, 224, 225, 232, 237, 244, 247, 252, 263, 268, 272, 283, 284, 294, 299, 300, 315 |
| performance | 015, 026, 027, 178, 228, 234, 235, 236, 248, 249, 250, 255, 256, 257, 258, 259, 260, 261, 265, 266, 267, 269, **270**, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 286, 288, 289, 290, 291, 292, 293, 295, 296, 297, 298 |
| library | 007, 010, 011, 012, 017, 043, 046, 047, 048, 072, 152 |

<!-- END GENERATED: spec rulings -->
