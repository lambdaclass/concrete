# Structural Classes of Bugs 039 – 107

Status: reference (2026-10-20). Bugs 108 – 122 (the examples-health pass and bug 121's layout probe) 123 – 124 (the exit 2i
specification, 2026-10-21) and 127 – 131 (the semantics probes, 2026-10-21) and 132 (batch 2's review of 123, 2026-10-21) and 126 (2026-10-21) and 135 – 137 (135 recorded by 109's fix, 136 – 137 found fixing 127; 2026-10-21) are placed in the summary table (102, 105, 115 and 126 were fixed on 2026-10-21; their classes stand, and 105 moved from class 8 to classes 12 and 6 once its cause was reduced) from their records' structural sections; the per-class prose below predates them. Maintained under R-0491: every new bug record states its
structural cause and owner ([filing a bug](README.md#filing-a-bug)) and either joins a class below
or adds one. Since 2026-10-21 the roadmap's defect disposition rule
([execution queue](../../ROADMAP.md#the-current-execution-queue)) uses these owners: silent defects
(memory safety, wrong code, false proofs) are stopped at once by repair or fail-closed containment;
loud defects are filed with the class owner below, pinned, and removed when that owner's structural fix
lands; and repair effort is charged to the capacity track of the defect's exit under that rule.

A bug fix repairs one spelling of a defect. This file asks the other question: which design fact let
the defect exist, which other bugs share it, and which roadmap task removes it for the whole class.
Most bugs since 039 fall into a dozen classes, and most classes are owned by a stage of the
[verified-compiler plan](../compiler/VERIFIED_PIPELINE.md) whose exit already names some of them.
A bug may belong to more than one class; the first listed is its primary class.

**Sources.** Bug records on main for 039 – 080, 084 – 086, 088, 090, 092 – 094, 101 – 104 and 107;
on branches for 081, 083 and 089 (`land3`), 082, 087 and 091 (`land2-fix`). Bugs 096 – 100 have records on main (fixed on `land5`). **No checked-in
record** exists yet for 095 (exit 2b requires it): 095 is classified from the
verified-pipeline plan ("requires correct storage aliasing during lowering"; the historical 092/095
aggregate-reference programs). Bug 105's record is on main
(`105_reference_valued_if_miscompiles.md`): a reference-valued `if` expression miscompiled (the
binary segfaulted while `--interp` printed 2); the checker contains it in source (E0238 refuses
every reference-valued `if`/`match`), a generic instantiated at a reference type got past that
containment, and the defect itself, fixed on 2026-10-21, was EmitSSA storing a reference register
as the aggregate it points at (classes 12 and 6 below, no longer class 8). Bug 106's record is on `land2-fix` too (`106_model_diff_report_hangs.md`): `--report
model-diff` on `thesis_demo` never terminated and `check_model_diff.sh` composed reports with
`|| true`, so the gate stayed green; the branch budgets the report and makes the gate fail closed
(`f950969f`), while on main it is still open, and the residual (recursion and calls are not budgeted;
the gate's watchdog turns a long run into a FAIL) is owned by R-0460.

## Summary

<!-- BEGIN GENERATED: class table bugs column (scripts/gen/bug_ledger.py; a bug joins a class in its record's ledger block; the class name and owner cells are edited here) -->

| class | bugs | structural owner |
|---|---|---|
| 1. Declaration identity by spelled name, not a resolved `DefId` | 039, 044, 050, 051, 054, 055, 056, 061, 069, 074, 077, 081, 082, 096, 097, 098, 114, 118, 122 (the builtin/user `Option`/`Result` name join), 129 (a user function's link symbol was its spelling; fixed 2026-10-21 by mangling every Concrete-defined symbol apart from C), 132 (two modules' constants of one name), 151 (two nested modules' functions flattened to one name), 165 (a compiler-generated `print_int` call binds to a same-named user function), 186 (an impl keyed by its type's spelling with its arguments dropped, and a method by `Type_method`: impl selection was a first-match search), 208 (a fn-typed local renamed to a same-spelled imported function), 218 (two declarations of one spelling in a module: nothing compared declarations with each other), 223 (an import alias of a type is a second spelling and so a second type: the alias has none of the type's methods, and the type's values do not match it; fixed 2026-10-25, R-0492 step 3: types compared and methods looked up by `TypeId`), 227 (a user `Result` in one module answers the builtin in another: Lower's and Resolve's program-wide spelled-name lookups; fixed 2026-10-25, R-0492 slice 6: one type environment per module, and Core spells a user `Result` apart from the builtin), 238 (a method call's caller-side spelling met a free function (or extern, constant, alias) defined under it; fixed 2026-10-26: Core references carry their namespace (`CallSpace`) and method references resolve among methods only), 239 (free functions and impl methods were one spelled namespace in Check, Elab and Core: a free `P_get` answered `p.get()`; fixed 2026-10-26: method tables, `CallSpace` on every Core/SSA reference, namespace-aware consumers), 241 (a method call renamed by two passes that each recompute its definition's spelling), 242 (a type in an imported signature is a spelling the importer re-reads, so the importer's own type of that spelling is captured; fixed 2026-10-25: a transported type whose spelling denotes another declaration in the importer is respelled to its identity), 243 (an aliased newtype import binds no name: an import copies the declaration under the alias, and the newtype kind has no such copy; fixed 2026-10-25: a newtype carries its definition name, and the import binds the alias to a copy with the declaration's identity), 245 (a constant reference is its spelling, scoped program-wide by the interpreter and per tree by Lower; fixed 2026-10-25: both scope it by one definition, `CModule.treeConstants`), 246 (an imported alias's target crossed as the exporter's spelling; fixed 2026-10-25 by expanding it at the import), 251 (a generic's specialization moved to the instantiating tree, whose constants and function names then answered its spellings; fixed 2026-10-25: a specialization keeps its defining tree), 253 (construction and field rights asked whether a type of the spelling is defined here, not whether the declaration the spelling resolved to is; fixed 2026-10-25: judged by identity) | R-0492 (S5 `DefId`), applied to the shared front end by R-0514 (row 4); R-0510 (S8a) for specialization names |
| 2. Binder identity by name, not a `LocalId` | 040, 041, 045, 087, 093, 094, 107, 124, 176 (a loop body's shadowing `let` outlived the loop in the checker) (and 068's binder half) | R-0492 (S5 `LocalId`); R-0499/R-0514 (row 4) for the checker's environments. Since exit 2i the source rule SCOPE-1@1 (E0301) keeps every shadowing shape of this class out of the compiler (124 closed by it); the identity repair stays R-0492's |
| 3. Destruction dispatched by synthesized `<Type>_destroy` names | 052, 091 (nested-module `Destroy`), 099, 100, 103, 116 | R-0510 (S8a checked specialization) with R-0492; cleanup edges R-0501 (S10) |
| 4. No reference-provenance model in the source checker | 091 (receiver cases), 092, 104, 105 (checker containment), 131 | R-0499 (S8, row 4); rules in exit 2i (R-0500) |
| 5. Borrow tracking by syntax instead of normalized places | 091 (receivers, arguments, explicit borrows), 092, 104, 120, 131, 143 (a deferred call's borrow reserved nothing), 216 (OWN-10's mutability half applied per statement form, not at the normalized place: field writes are not checked) | R-0499 (S8, row 4) over Core places; interim: batch 2's normalized place rule |
| 6. Capability facts not carried across dispatch, specialization or package boundaries | 063, 071, 091 (impl capabilities), 105 (a source-level reference rule satisfied later by Mono's substitution), 185 (a monomorphized instance dropped its definition's `trusted` flag), 188 (a capability variable entailed every requirement, deferred to an instantiation nothing checks), 211 (an operator rule deferred to the instance had no instance check: arithmetic, unlike comparisons) | R-0499 (S8 capability containment, re-checked after Mono); R-0510 (S8a) |
| 7. Semantics implemented several times (interpreter, proof model, folder, backend disagree) | 053, 066, 080, 084, 085, 086, 088, 101, 102, 105, 108, 109, 117, 118, 119, 120, 123, 127, 128, 130, 133, 134, 136 (until 127's fix the consumers disagreed), 137 (the payload list is both evaluation order and positional identity), 140, 150 (the folder's float self-comparison), 152 (a runtime string builtin's empty-input edge), 183 (a dereference assignment's operand order), 184 (the interpreter read all of stdin before `main`; the binary reads when the program does), 193 (which occurrence of a repeated literal field counts), 254 (the interpreter had no case for a newtype's construction or unwrap over a struct or enum, which Core writes as a cast; fixed 2026-10-25) | R-0493 (S6 `IntArith`), R-0496 (S7 Core semantics), R-0495 (S9 claims on Core); harness R-0491 (exit 2c) |
| 8. Storage, places and control flow decided ad hoc in `Lower` | 078, 102, 107, 108, 110, 116, 126, 127, 130, 133, 134, 146 (`*h` lowered as a load only), 175 (a `match` whose arms all diverge left its merge open: 078's shape, missed in `match`), 182 (a borrowed temporary has no slot), 190 (an array bound by value aliased its source), 213 (an arm after an unguarded catch-all lowered into a block no edge reaches) | R-0501 (S10 Place MIR); R-0502/R-0503 (S11); R-0504 (S12 validators) |
| 9. Evidence verdicts computed per surface and joined to their subject by name | 067, 079, 081, 083, 089 | exit 0a (R-0482) now; R-0494 (S2 one `Verdict`), R-0495 (S9), R-0521 (row 9) |
| 10. Evidence identity computed from a partial subject | 059, 060, 062, 068 | R-0521 (row 9 canonical `VerificationTask`); R-0497 (S5 payload-bound artifacts) |
| 11. Absence read as a fact (fail-open defaults) | 049, 052, 058, 063, 065, 067, 075, 076, 103, 106, 112, 123, 128, 206 (an empty diagnostic code was a valid value), 207 (a gate that died on its first failed check gave the mutation harness no verdict), 217 (a nested submodule Resolve never visits yields no diagnostics), 252 (a link check that skipped every anchor and that no workflow step ran passed as coverage) | the absence rule (`check_absence_not_fact.sh`); R-0494 (S2) for verdicts and R-0494/R-0495 for proof admission (076); R-0513 (exits 2d, 2e) for gates; R-0510 for synthesized definitions |
| 12. One fact kept in two places | 042, 057, 064, 070, 105 (`isPassByPtr` read both as "passed as a pointer" and "a register holding an aggregate by pointer"), 108, 109 (float rendering kept in several places), 110 (the element-type hint: Check passes it, Elab did not), 112, 121, 122, 129, 135 (the formatter's copy of that rendering), 177 (secondary: CoreCheck kept a width-blind copy of type equality), 181 (an arm binder through a borrowed scrutinee: `&T` in Check, `T` in Elab), 199 (a cast target was not resolved through type aliases), 202 (the front-end stage sequence restated by each driver; four skipped desugaring), 203 (one diagnostic code, E0420, for two unrelated errors), 210 (which types an operator rule defers to the instantiation: a local copy per rule read every named type as a parameter), 212 (the struct-literal decision: a case test in the parser, an ambiguous production in the grammar), 215 (a type written inside an expression was not checked to name a declared type), 219 (the bug-061 extraction arm in an executed and a proof-only copy; the mutation hit the unexecuted one), 222 (what travels with an imported type, selected once per type kind; the enum paths did not select it), 224 (a recovered `let` restated by hand with its mutability fixed to false), 225 (the type a refused bound names, rendered per branch; one copy printed `<type>`), 226 (which declarations an import may name: Resolve's rule checks `pub`, the submodule rule Check and Elab use does not), 229 (Resolve's module scope: one first-wins map for types and values, its own builtin-`Option` rule, child newtypes missing, against Check's environment), 230 (which type names a top-level module may write: program-wide in Resolve, per module in Check; fixed 2026-10-25 by one `moduleTypeEnvironment` for every module, R-0492 slice 6), 232 (the discovery signal's streak: computed by the dashboard generator, restated as a literal by the docs-drift controls), 233 (which declarations an import may name: the public-name list holds traits and constants, the import rules have no case for them; fixed 2026-10-25, R-0492 step 4: a constant is imported by its `ConstId`, a trait refused with the reason), 247 (a function value has two Core forms, `fnRef` and an identifier; the assumption summary's value walk read one), 262 (which newtype constructors a module binds: Check's environment includes its children's newtypes, Elab's does not; fixed 2026-10-25: one definition, the module's own and its imports') | R-0492/R-0514 (one resolver and builtin table); R-0502 (S11 one `Layout`); R-0494 (S2 report split) |
| 13. Documented rules with no enforcing site | 073, 075, 091 (impl conformance), 092, 108 (`<int> as bool` admitted with no meaning), 111, 113, 115, 123, 124 (closed by SCOPE-1@1, exit 2i), 130, 134 (no type had a defined comparison), 136 (evaluation-order rule 6: Core cannot bind the base once), 140 (nor was a comparison non-consuming), 144 (heap interiors exempted from use-once by a disclosed exclusion), 145 (raw collection intrinsics outside rule (c)), 177 (CoreCheck documented "arguments match parameters" and never compared any binder's type with its value's), 182 (REF-5: a borrowed call result is accepted), 192 (an `if` / `while` / `for` statement condition: Check discarded its type, CoreCheck admitted integers), 194 (a function returns a value on every path: no site; the backend verifier noticed), 195 (`let ... else` divergence: desugared away before any checker), 196 (pattern shape left to CoreCheck, after Elab crashed or lowered the arm), 198 (`trusted` does not relax linearity, unenforced at `*p`), 200 (a generic type named without its argument list: arity checked only where a list was written), 204 (a bound on a primitive type argument: `checkTraitBounds` skipped every unnamed shape), 205 (finite size judged on declarations and assumed for generic instantiations), 220 (the grammar's radix literal needs a digit; the lexer read none as 0), 221 (a call's type-argument count: a `zip` stood in for the check), 231 (MOD-1 for a bound's trait names: no pass resolved them, later reads matched spellings), 240 (TYPE-2 at the source level: a trait in a type position was taken for a type) | R-0498 (S3, exit 2l); R-0499 (S8: every rule has an enforcing site) |
| 14. Trusted `std` and FFI code with no checked contract | 043, 046, 047, 048, 072, 166 (a foreign fn value called with the Concrete ABI), 189 (`trusted impl` made every method trusted without declaring it) | R-0509 (S16 trusted base); R-0283/R-0287 (ABI checks) for the C boundary |
| 15. Ownership state not modeled along control-flow paths | 090, 115, 142 (a value moved into a construct an exit left incomplete), 143 (a pending deferred call was not part of the state), 147 (an erased statement counted as a move), 148 (a short-circuit operand treated as sequential), 149 (loop conditions, nested returns and labelled exits), 197 (a variant pattern with no field list ignored a non-Copy payload), 214 (a per-exit refusal named the binding but not the exit) | R-0499 (S8 use-once and conservation at reachable exits, row 4) |
| 16. Artifacts at paths no run owns | 179 (the intermediate `.ll` at `<input>.ll`, shared by concurrent compiles of one source), 191 (test scratch files at fixed `/tmp` names, owned by the first user on a shared host), 209 (a read-only review's git left the caller's `index.lock` behind), 244 (a killed gate run's children and its lock, owned by no process group a signal or a later run could address) | the compile driver's private-intermediate rule (`privateIntermediate` in `Main.lean`) and the test runner's scratch directory (`CONCRETE_TEST_TMPDIR`, `Main.runTestBinary`); no roadmap stage yet |
| 17. Pass state in association lists and per-query recomputation (superlinear compile time) | 026, 027 (EmitSSA searched a list of pointer registers per operand; one store per literal element), 178 (a wide `match` was cubic in Lower + SSAVerify + SSACleanup; deep `if` nesting cubic in the obligation walk), 228 (the interpreter's environment, an association list, kept every loop iteration's bindings), 234 (the same environment's length, a scope's restore point and a reference's frame, recomputed by a walk at every call, branch, arm and borrow), 235 (an interpreter array read converted the array to a list per read), 236 (Elab appended an array literal's elements one at a time), 248 (an interpreter element write copied the array the environment still held), 249 (the parser appended an explicit array literal's elements one at a time), 250 (a native place write rebuilt its whole aggregate base) | R-0422 (compiler throughput queue, complexity envelopes); ROADMAP #5 (Lower builder) and R-0504 (S12 validators) for the lowering and verification halves |
| 18. Pipeline artifacts not bound to their payload | 180 (a project report replaced a `ValidatedCore`'s modules by record update and ran Mono and lowering on a forest CoreCheck never certified) | R-0497 (S5 sealed, payload-bound artifacts; slice 1 landed with 180's fix) |
| 19. A mention read as a use (tool facts inferred from where a name is spelled) | 237 (the impact selector counted a script name in a keyed array's note text as a call) | R-0513 (impact gate selection: a name judged by its shell context, each text context pinned by a control) |

<!-- END GENERATED: class table -->

## 1. Declaration identity by spelled name, not a resolved `DefId`

**Bugs.** 039 (a bare imported name rebound through a flat, first-match linker-alias list
accumulated across modules), 044 (a renamed generic import's alias entries, first match wins, so it
was never monomorphized), 050 (Mono resolved an indirect call through a local by the global function
name and rewrote it to a direct call), 051 (generic enums kept one LLVM type per enum name), 054
(generated specialization names and user names share one namespace), 055 (a sibling-module alias
names neither orientation of the prefixed definition), 056 (a function reference encoded as a
sentinel register name), 061 (`PExpr` spelled a parameter application and a global call the same
way), 069 (an aliased imported type registered under the definition-site spelling), 074 (two
packages' modules with one name became one LLVM module), 077 (a relative call into a nested
submodule lowered to an unprefixed symbol), 081 (a call's key in the proof model was the call-site
spelling, so an import alias could borrow another function's proof), 082 (an import alias lost to
any same-named definition), 096 – 098 (same-named types, private functions and generic functions in
two modules resolved to one definition), 186 (an impl was keyed by its type's spelling with the
target's arguments dropped, and a method by `Type_method`, so "which impl?" was answered by the
first match and nothing established that there was only one).

**Design cause.** Names stay `String` after resolution: Core binders and callees (with a
`Coe String Callee`), SSA registers, the alias pools and `PExpr` are joined by spelling, and module
prefixing (`prefixModuleFnNames`) and mangling produce names that collide. Each pass resolves a
spelling again with its own lookup order.

**Structural fix.** A `DefId` allocated at Resolve for every declaration, preserving callable
namespaces, specialization arguments and the extern/intrinsic/generated distinction; no
`String → identity` coercion after Resolve; `check_ir_no_string_identity.sh` rejects string joins.

**Owner.** R-0492 (verified-pipeline S5, whose exit names 081 and 082), applied to the shared front
end for every user by R-0514 (queue row 4); R-0510 (S8a) owns specialization identity (051, 054,
098). R-0002, R-0436 and R-0442 already retired the string encodings behind 050, 056 and 061. Bug
186's coherence judgment is typed (`TraitId`, `TypeId`, `MethodKey`); R-0548 moves the method
tables themselves off the `Type_method` spelling.

## 2. Binder identity by name, not a `LocalId`

**Bugs.** 040 (CoreCheck's first-match variable table), 041 (a match merge patched states by name
from the last arm's environment), 045 (nested same-named match binders shared one slot), 087 (a
block `let` shadowing an outer name was not lexically scoped in the interpreter, the binary or the
model), 093 (a shadowing block local was skipped by the never-consumed check because its name
existed before the block), 094 (per-binding state updates marked every entry with the name), 107
(a same-scope `let` shadowing a borrowed binding reused its Core name and so its stack slot: a
reference to the old binding read the new value in the binary, not in the interpreter), 176 (a
loop body's shadowing `let` stayed visible to the checker after the loop, because Check's
`while`/`for` restored only loop depth and labels: the outer name took the inner type and
mutability, so a `u8` held 300), and 068's binder half (a ghost `let` opened no scope slot).

**Design cause.** The checker's environment is a list of `(name, VarInfo)`; scope exit, merges and
state updates find bindings by name, and Elab alpha-renames only some binders. Scope exit was
written per construct (176: three mechanisms, and the loops had none); since 176 every block
closes through `closeBlockScope`.

**Structural fix.** A `LocalId` for every binder from Resolve on, consumed by the source checker
and Elab alike; the language rule "no block `let` shadowing a visible name" (exit 2i).

**Owner.** R-0492 (S5, whose exit names 087, 093 and 094, and must name 107); R-0499/R-0514 (row 4) move the
checker's environments onto it.

## 3. Destruction dispatched by synthesized `<Type>_destroy` names

**Bugs.** 052 (`T_destroy` for an array element type resolved to a no-op), 091's nested-module
case (the H18 fence looked up `Type_destroy` but the destructor had been renamed
103 (any unresolved callee ending in `_destroy` replaced by an empty body, swallowing externs;
fixed by `0c507fb9`: dispatch keys on the reserved `$destroy` callee and the argument type, glue is
named `$destroy$<type>`, and since 2026-10-21 every Concrete-defined symbol is mangled apart from C).
`sub_Type_destroy`), 099 (Check and Elab inferred a generic call's type arguments from two
argument-type peeks, so Core instantiated `Placeholder` where Check had typed the argument), 100 (a
generic impl's `&self` destructor reached by its unspecialized name),

**Design cause.** Drop glue spells a destructor as `<Type>_destroy` and resolves the spelling late;
an unresolved spelling is filled in rather than refused (class 11), and an extern is
indistinguishable from a generated destructor by name (class 1).

**Structural fix.** A destroy call resolves to the identity of the exact `Destroy` impl instance;
checked specialization rejects a missing or substituted instance; no definition is synthesized from
a name.

**Owner.** R-0510 (S8a: "missing or substituted instances, wrong impls ... are rejected"), with
R-0492's `DefId`; cleanup edges move to Place MIR under R-0501 (S10).

## 4. No reference-provenance model in the source checker

**Bugs.** 091's receiver cases (a by-value or `&mut self` receiver reached through a shared
reference), 092 (a reference stored in an aggregate did not freeze its source), 104 (a reference
assigned from an inner block outlives its source), and 105's checker side (a reference chosen at run
time between branches cannot be traced, so it is refused). 087's borrow-block use after free is
not in this class: the checker had assumed the right binding and the compiled program read a
different one, a binder-identity defect (class 2).

**Design cause.** Borrow facts are attached to the syntactic form that created them (`let x = &y`
records `borrowedFrom`), keyed by a source name; a reference that flows by assignment, through a
merge, into an aggregate or through a receiver carries no provenance, and the rules run on the
source tree while elaboration rebuilds the program, so the program checked is not the program
compiled.

**Structural fix.** Loans on normalized places, carried through every assignment, merge and call,
checked by declarative rules (borrow validity, no escaping or stored reference, use-once) in one
checker over Core whose output is the only input to lowering.

**Owner.** R-0499 (S8, queue row 4; its exit names 090 – 094 and 087's borrow-block case), with the
reference rules written and enforced by exit 2i (R-0500). The immediate repairs stay under exit 0b.

## 5. Borrow tracking by syntax instead of normalized places

**Bugs.** 091 (receivers, call arguments, explicit borrows, overlap and destruction each walked
expressions their own way; ten review passes each found another spelling of the same alias), 092
(only the bare `let x = &y` form was tracked), 104 (the assignment form was not).

**Design cause.** Every check that needs "which place does this expression denote" computed it
itself, so the checks disagreed on parentheses, dereferences, projections and branch values. Bug
091 converged only after one normalized-place rule (`normPlace` / `normRef` in `CheckHelpers.lean`
with batch 2) replaced them, with E0238 refusing what it cannot normalize, including re-pointing a
reference by reassignment (which fixes 104) and shadowing a name a live reference borrows (which
fixes 107's reference reproducers); the deliberate restrictions are listed in KNOWN_HOLES H28.

**Structural fix.** Places are a representation, not a per-check walk: Core places with projections
and dereferences, consumed by every ownership and borrow rule.

**Owner.** R-0499 (row 4), which replaces the interim syntactic rule and lifts its deliberate E0238
restrictions; Place MIR (R-0501, S10) carries the same places into lowering.

## 6. Capability facts not carried across dispatch, specialization or package boundaries

**Bugs.** 063 (cap-variable inference read an unknown argument type as no capabilities), 071 (a
capability declared in a dependency did not bind on its caller), 091 (a trait impl's extra
capabilities were hidden by generic dispatch, and nothing re-checked after Mono). 076 is not in this
class: its proof admission ignored dependency effects, unresolved generic calls and foreign
assumptions, an evidence decision (class 11).

**Design cause.** Capability requirements are checked once, before specialization, against the
signatures a pass can see; dispatch through a trait bound, a function pointer or a package boundary
reaches code whose requirements were never compared with the caller's.

**Structural fix.** Capability containment as a checker rule over resolved callees, re-checked on
specialized code (`CheckedMonoCore`), through pointers, generics and FFI.

**Owner.** R-0499 (S8 capability containment, "capabilities are re-checked after Mono") and R-0510
(S8a, whose exit names 091's impl-capability case).

## 7. Semantics implemented several times

**Bugs.** 053 (DCE's trap inventory omitted negation), 066 (the interpreter discarded a guard's
effects), 080 (a narrowing cast was the identity in the model), 084 (the model bound match binders
by name, the program by position), 085 (a loop step read the pre-iteration environment), 086 (a
discarded statement modeled as the block's value), 088 (an unhinted literal unchecked, and the
folder compared it unwrapped), 101 (a branch-local `let` captured the continuation in the model),
102 (the interpreter refuses a raw-pointer cast the backend miscompiles, so the differential gates
are blind to it), 105 (the binary and the interpreter disagree on a reference-valued `if`), 184
(the interpreter modelled standard input as a snapshot the driver read before `main`, so it
consumed input when and as far as the binary never does; every differential gate feeds a closed
stdin, so none could see it).
104's heap reproducer also cannot run under `--interp` (`alloc` is undefined there).

**Design cause.** The language's meaning lives in `Proof.eval`'s `PExpr`, the interpreter, the SSA
folder and the emitter, each with its own arithmetic, scoping and statement rules; where one of
them does not implement a construct, differential testing against it is silently blind.

**Structural fix.** One arithmetic (`IntArith`), one Core semantics that the interpreter executes
and every other side is checked against, and claims stated against Core rather than a separate
model; an unsupported construct is a counted coverage gap, never a skipped comparison.

**Owner.** R-0493 (S6, exit 2h: 053, 080, 088), R-0496 (S7: the interpreter becomes Core's
semantics, covering every construct the checker accepts: 066, 102, 105), R-0495 (S9 claims on
Core: 084 – 086, 101); the harness R-0491 (S1, exit 2c) detects the class meanwhile.

## 8. Storage, places and control flow decided ad hoc in `Lower`

**Bugs.** 078 (an `if`/`else` whose arms both diverge left its merge block open), 095 (no record:
stale storage of aggregate references, "requires correct storage aliasing during lowering"), 102 (a
pointer-typed local whose address is taken got no stack slot), 105 (listed here while it was
open; its cause turned out to be in EmitSSA, class 12), 107 (a shadowing `let` was given the shadowed binding's slot because slots
are keyed by Core name, while a reference to the old binding was still live), 175 (a `match`
whose arms all diverge left its merge block open: the 078 shape again, because each branch
construct decides for itself whether its merge is reachable).

**Design cause.** `Lower` performs sequencing, `defer`, match compilation, promotion, layout and
bounds checks in one pass with no semantics of its own; storage is decided by recognizing
address-taking shapes per type (the 029, 031, 034 and 038 lineage), and nothing validates SSA
against a semantics.

**Structural fix.** Place MIR: explicit places, storage and cleanup edges, one pass per job, each
differentially tested against Core; one proved `Layout`; BIR with a semantics and validators.

**Owner.** R-0501 (S10 Place MIR), R-0502/R-0503 (S11), R-0504 (S12 validators).

## 9. Evidence verdicts computed per surface and joined to their subject by name

**Bugs.** 079 (a receipt minted for a body the theorem says nothing about), 081 (a call's proof key
not bound to the callee), 083 (a function's name was a proof claim), 089 (an `#[ensures]` discharged
by a link, not a checked theorem), 067 (a capped obligation's status recomputed from the attestation
count instead of its hypothesis debt).

**Design cause.** Evidence status is computed separately by six surfaces, and callees, theorems
and specifications are joined by name.

**Structural fix.** One `Verdict` with a private discharging constructor, consuming checked evidence
bound to subject, proposition, clause, coverage and dependency closure; every surface renders it.

**Owner.** Exit 0a (R-0482) for the repairs; R-0494 (S2, exit 2a), R-0495 (S9) and R-0521 (row 9,
one canonical `VerificationTask`).

## 10. Evidence identity computed from a partial subject

**Bugs.** 059 (the body fingerprint omitted types and the signature), 060 (contracts outside the
fingerprint), 062 (staleness did not propagate to dependents), 068 (a ghost `let` and a runtime
`let` produced the same evidence).

**Design cause.** What a proof is about was hashed from a projection of the subject chosen by the
hasher, not from the exact checked artifact and its dependency closure.

**Structural fix.** Evidence binds the exact payload-bound artifact (subject, contracts, dependency
closure, environment) through one constructor.

**Owner.** R-0521 (row 9) and R-0497 (S5 payload-bound artifacts); R-0004 repaired 059 and 060 (V2
activation) and 062 (slice 3).

## 11. Absence read as a fact (fail-open defaults)

**Bugs.** 049 (an unevaluable crash predicate stubbed to parse-only), 052 (`destroyable` accepted any
unmatched type), 058 (a missing fingerprint compared with itself), 063 (unknown capabilities read as
none), 065 (recursive callees dropped, so unbounded stack read as bounded), 067 (a cap lost to
attestation), 075 (policy gates that never saw a capability passed), 076 (proof admission treated
unseen dependency effects, unresolved generic calls and foreign assumptions as effect-free), 103
(an unresolved `_destroy` filled with an empty body), 106 (a non-terminating model-diff report
swallowed by the gate's `|| true`), 112 (a body-less declaration's missing effect clause read as an
undeclared foreign binding, an `#[intrinsic]` included; the intrinsic case fixed with 113 on
2026-10-21, since an intrinsic is not a foreign binding), 252 (the Markdown-link check dropped
anchors and was run by nothing but the Makefile, so unchecked anchors and an unrun check both read
as passing).

**Design cause.** An unknown, uncomputed or unresolved value defaults to empty, pure, fresh, bounded
or successful, and nothing downstream can tell it from a measurement
([ABSENCE_IS_NOT_A_FACT](../project/ABSENCE_IS_NOT_A_FACT.md)).

**Structural fix.** Unknown is carried as `Option`, `Except` or an explicit variant through to the
verdict; gates count completed work and fail below a minimum.

**Owner.** The absence rule and `check_absence_not_fact.sh`; R-0494 (S2) for verdicts; R-0513
(exits 2d and 2e) for gates (075, 106, 252); R-0460 for 106's unbudgeted evaluation residual; R-0510 (S8a) for synthesized definitions (052, 103); proof
admission over complete dependency and foreign-assumption summaries (076) belongs to the evidence
owners R-0494 (S2) and R-0495 (S9).

## 12. One fact kept in two places

**Bugs.** 042 (Resolve and Elab classified imports separately and disagreed), 057 (a hardcoded
`HashMap` size disagreed with its declaration), 064 (two query-kind lists, one validating and both
rendered), 070 (`builtinEnumList` built twice), 121 and 122 (a type's layout computed by `Layout`
for field offsets and restated as an LLVM type whose layout LLVM computes again: payload-less and
align-8 enums in 121, the canonical builtin `Option`/`Result` union in 122).

**Design cause.** A fact is recomputed or restated where it is needed instead of read from one
owner.

**Structural fix.** One resolver and builtin table shared by every pass; one `Layout` authority;
report surfaces split by owner.

**Owner.** R-0492/R-0514 (042, 070), R-0502 (S11, 057, 121, 122), R-0494 (S2 splits `Report.lean` by
surface, 064). Until R-0502, `check_enum_union_layout.sh` leg 5 compares LLVM's layout of every
emitted struct and enum type with the compiler's (121, 122).

## 13. Documented rules with no enforcing site

**Bugs.** 073 (generic type-argument arity unchecked), 075 (authority checks that could not see a
capability), 091 (trait impl conformance compared only return types), 092 (references are
second-class and "never stored in an aggregate", but nothing enforced it), 113 (R-0484 R3 was
checked over the module Resolve was handed and not its nested submodules; fixed 2026-10-21: the
check walks every submodule), 177 (CoreCheck, the independent re-check of Core, documented
"arguments match parameters" but compared only argument counts, and never compared a `let`,
assignment, field or store's declared type with its value's; its returns were compared
width-blind. A front-end defect, bug 176, therefore reached a binary instead of a refusal; fixed
2026-10-22 by one binder-agreement relation, `bindTyAgrees`).

**Design cause.** Language and verification documents state rules that no check performs, and
nothing ties a stated rule to a gate leg that would fail without it.

**Structural fix.** A claim → gate-leg → targeted-mutation manifest, and declarative checker rules
each with an enforcing site and a mutation class.

**Owner.** R-0498 (S3, exit 2l) and R-0499 (S8).

## 14. Trusted `std` and FFI code with no checked contract

**Bugs.** 043 (non-NUL-terminated buffers passed to C string APIs), 046 (`keys`/`values` duplicated
linear payloads in an unconstrained trusted impl), 047 and 048 (`HashMap` probing defects), 072 (a
child that failed to exec flushed the parent's buffers).

**Design cause.** `trusted` code and foreign calls are outside the ownership checker and have no
functional contract, so their correctness rests on review.

**Structural fix.** `std`'s trusted implementations verified against contracts or listed with an
audit (the list only shrinks); C boundary facts (string termination, ownership transfer) stated in
binding descriptions and checked.

**Owner.** R-0509 (S16 trusted base); R-0283/R-0287 (ABI checks) and R-0285's binding descriptions
for the C boundary.

## 15. Ownership state not modeled along control-flow paths

**Bugs.** 090 (a match guard's move was recorded as its arm's move; when the guard was false a later
arm moved the value again).

**Design cause.** The checker computes each arm from the pre-match environment and folds effects
into the construct it is checking, instead of following ownership state along the program's actual
control-flow paths, so an effect on a path that does not commit (a failed guard) is attributed to a
path that does.

**Structural fix.** Use-once and conservation as rules over Core's control flow, with the state
threaded along every reachable path, including guard failure.

**Owner.** R-0499 (S8: use-once and conservation at reachable exits; its exit names 090), queue
row 4.

## 16. Artifacts at paths no run owns

**Bugs.** 179 (the intermediate LLVM file was `<input>.ll` or `<main>.ll`, so two concurrent
compiles of one source removed or rewrote it under each other; test mode, `concrete run` and the
reducer named their temporaries from the source path or a millisecond clock), 191 (test programs
and std tests wrote fixed `/tmp` names; on a shared host the first user to run the suite owned
them and `fopen` failed for every other user), 209 (a read-only Codex review's `git status` took the
caller's `index.lock` and, stopped mid-refresh, left it behind), 244 (a killed gate run left its
gates working the tree under init and the repository lock held, because the lock recorded only the
creator's pid and nothing named the processes of the run).

**Design cause.** The driver derived the location of an intermediate from a shared input, the
source path or the machine's clock, instead of from the compile that writes and reads it, so it
implicitly assumed that it was the only process touching those paths. Test code made the same
assumption about a fixed name in a machine-wide directory, and also about being the only user.

**Structural fix.** Every intermediate that a compile writes and later reads lives in a directory
that compile creates for itself (`IO.FS.createTempDir` / `IO.FS.withTempDir`), named by
`privateIntermediate`, and the directory is removed on every path unless it is kept on purpose for
debugging. Every file a test writes lives in the scratch directory its runner provides
(`CONCRETE_TEST_TMPDIR`: `concrete test` / `--test` set it inside the run's private directory, the
suite harnesses per run or per job; std tests use `std.test.scratch_path`), with no fallback.

**Owner.** The compile driver (`Main.lean`, `privateIntermediate`); the concurrent-compile legs in
`run_tests.sh` (file and project mode) enforce it. For test scratch, the test runner
(`Main.runTestBinary`, `run_tests.sh`, `test_ssa.sh`); `check_gate_hygiene.sh` refuses fixed `/tmp`
paths in gate scripts (R-0546) and in every `.con` source (bug 191). For review runs, the review
entry point (`scripts/review/review.sh`, `GIT_OPTIONAL_LOCKS=0`; enforced by
`check_review_tool.sh`, bug 209). For gate runs, the gate-session launcher
(`scripts/tests/lib/gate_session.py`: a run is one process group, swept on any signal) and the
repository lock (`scripts/tests/lib/fresh.sh`: its lease is that group; bug 244). No roadmap stage owns
the driver yet.

## 17. Pass state in association lists and per-query recomputation

**Bugs.** 026 (the parser built a `[v; N]` literal with `elems ++ [x]`), 027 (instruction lists in
Lower and SSAVerify built by `acc ++ x`), 178 (a 1000-arm `match` spent 8.45 s in the `lower` stage:
SSAVerify kept dominator sets as `List String` intersected by `filter`/`contains` and rebuilt every
strict dominator's definitions per block, cubic in arms; SSACleanup and Lower added quadratic
`find?`, list-membership and append terms; and on the same compile path the obligation walk's
`assignedScalarsS` copied each nesting level's list into its parent's, cubic in `if` depth). The
2026-07 O(n²) sweep (lexer `acc ++ [tok]`, dominator Jacobi passes, `collectAllUses`,
`Mono.lookupFn`, reassignment chains) fixed earlier members before the class had a number.

**Design cause.** Passes keep per-function state in association lists and sets as lists, build
results by appending, and recompute derived relations (dominators, predecessors, definitions, phi
sources) per block or per use rather than once per function. Each choice is linear per step, so a
pass is quadratic, and two nested ones are cubic. No pass states its complexity envelope, and the
older guard (`check_compiler_complexity.sh`) times wall clock at sizes up to 200, where these terms
are still small.

**Structural fix.** Index-based state (`HashMap`/`HashSet`/`Array`) for anything looked up by key,
accumulate-then-reverse for anything built element by element, derived relations computed once
per function with O(1) queries, and a per-stage envelope gate measured by a load-independent count
(`check_ssa_lowering_scaling.sh`: lower-stage heartbeats and `instructions:u`, slope ≤ 1.3). The
append ratchet (`check_no_quadratic_append.sh`) keeps the commonest spelling from returning.

**Owner.** R-0422 (one owned queue for measured compiler performance defects, with explicit
complexity envelopes gated per class). ROADMAP #5's structured Lower/SSA builder should own block
and merge accumulation; R-0504's SSA validator should keep the O(1) dominance query.

## 18. Pipeline artifacts not bound to their payload

**Bugs.** 180 (in a project, `--report mono`, `verify` and `traceability` built a `ValidatedCore`
by `{ validCore with coreModules := … }` from the invoked file's modules, wrapped in synthetic empty
parents and stripped of `std` and every other dependency, then ran Mono and lowering on it; the
reports described a program nothing compiles, with no dependency specialization at all).

**Design cause.** A pipeline artifact was a plain record. Its type said which pass had accepted it,
but nothing tied that acceptance to the payload it carried, so any code could replace the payload
and keep the type, and every consumer downstream read the new payload as certified.

**Structural fix.** Sealed, certified artifacts (R-0497): each artifact from `ValidatedCore` on has
a private constructor and carries, as an erased `Prop` field, the equation its producer established
about its exact payload, and names the artifact it was produced from. A selection of a validated
program is a separate type (`CoreSubset`) that no compiling pass accepts, and the debugging SSA view
is `UnverifiedSSA`, which `emit` does not accept.

**Owner.** R-0497 (S5, artifact half). `check_pipeline_artifacts.sh` refuses forgery, payload
replacement and diagnostic bypass by elaboration.

## 19. A mention read as a use

**Bugs.** 237 (the impact gate selector counted a script name in a keyed array's value, a
bug-corpus note, as a call, so the corpus runners appeared to run some forty gates and read
`docs/`, and a docs-only diff selected 19 gate commands).

**Design cause.** Repository tools derive facts about scripts (which gates a script runs, which
files it reads, whether a gate is reachable from the workflow) from where a name is spelled,
judged line by line with regular expressions. A name in text (a note, a reason, a message) and a
name a command uses look alike to such a scan. Each context was taught separately as it bit:
grep and echo lines, heredoc bodies, the whole-suite drivers, the gate-registration walk ("a
mention is not an invocation"), and now keyed array values.

**Structural fix.** Judge a name by its shell context: a word in command position or under an
interpreter is a run, an argument of a file reader is a read, a data value the script only
prints or compares is nothing, and every context the tool does not know is a run. The contexts
treated as text are a whitelist, each pinned by a control beside a counterpart that stays a use;
a context the reader does not model keeps the dependency.

**Owner.** R-0513 (impact gate selection); the controls are the "text is not a call" legs of
`check_gate_selection.sh`.
