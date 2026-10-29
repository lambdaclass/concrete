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

279 bug records, 18 of them ruled: violates a rule 7, spec gap 4, open owner question 1, permitted by a rule 0, outside the spec 6; not ruled yet (fixed bugs filed before bug 300) 261. Open bugs are in bold.

### By kind

| ruling | bugs | open | silent |
|---|--:|--:|--:|
| violates a rule | 7 | 3 | 2 |
| spec gap | 4 | 2 | 1 |
| open owner question | 1 | 1 | 0 |
| permitted by a rule (not a bug) | 0 | 0 | 0 |
| outside the spec | 6 | 4 | 0 |
| not ruled yet | 261 | 0 | 87 |

### By chapter

Every bug counts once under each chapter its ruling names: the rules it violates, the owner questions it waits on, the chapter a spec gap belongs to, or the rule that permits it.

| chapter | prefix | bugs | open | silent | violates | spec gap | open question | permitted |
|---|---|--:|--:|--:|--:|--:|--:|--:|
| pipeline.md | PASS | 5 | 2 | 0 | 3 | 2 | 0 | 0 |
| ffi.md | FFI | 2 | 2 | 1 | 2 | 0 | 0 | 0 |
| memory.md | MEM | 2 | 0 | 2 | 1 | 1 | 0 | 0 |
| types.md | TYPE | 1 | 0 | 0 | 0 | 1 | 0 | 0 |
| modules.md | MOD | 1 | 1 | 0 | 1 | 0 | 0 | 0 |
| decisions.md | OPEN | 1 | 1 | 0 | 0 | 0 | 1 | 0 |

### By violated rule

| rule | chapter | bugs | which |
|---|---|--:|---|
| PASS-9@1 | pipeline.md | 2 | 303, 309 |
| FFI-2@1 | ffi.md | 1 | **122** |
| FFI-5@1 | ffi.md | 1 | **114** |
| MEM-3@1 | memory.md | 1 | 301 |
| MOD-2@1 | modules.md | 1 | **226** |
| PASS-6@1 | pipeline.md | 1 | 304 |

### Spec gaps: rules to write

No rule decides these bugs' behaviour. Each fix starts by writing the rule (spec work, owned by docs/spec/); bugs that need the same rule share one row.

| chapter | the missing rule | bugs |
|---|---|---|
| PASS | a program every rule accepts is compiled: no stage after Check refuses it, crashes on it or emits invalid LLVM IR for it | **054**, **077** |
| MEM | a raw pointer to a live local keeps denoting that local across control flow, and a write through either the local or the pointer is seen through the other | 302 |
| TYPE | a call of a generic function without explicit type arguments binds each type parameter from the types of the arguments given for it, whatever expression form supplies them | 308 |

### Open owner questions

| question | bugs |
|---|---|
| OPEN-17 | **216** |

### Permitted by a rule (not a bug)

| rule | bugs |
|---|---|
| — | — |

### Outside the spec

| reason | bugs |
|---|---|
| tooling | **049**, **058**, **064**, 300, 315 |
| performance | **270** |
| library | — |

<!-- END GENERATED: spec rulings -->
