# Bug 262 — a child's newtype constructor is bound in its parent by Check but not by Elab

<!-- bug-ledger
summary: a parent calling its child's newtype constructor without importing it (`K(30)` beside `mod kids { pub newtype K = i64; }`) passed Check, whose environment holds the children's newtypes as types and took them for bound constructors, and was refused by Elab as an undeclared function (E0402, an internal code), natively and under `--interp`. Check and Elab now bind newtype constructors from one definition (the module's own and its imports'), so Check refuses it (E0263, MOD-2), and a call of an imported function sharing a child newtype's spelling, which Check had refused as that newtype's constructor (E0220), is accepted. Found by the identity follow-up probes, 2026-10-25. Fixtures `error_262_*`, `regress_262_own_rights_beside_child.con`. Class 12.
fixtures: error_262_parent_constructs_child_newtype.con error_262_sibling_constructs_newtype.con error_262_child_constructs_parent_newtype.con regress_262_own_rights_beside_child.con regress_262_imported_fn_beside_child_newtype.con
class: 12 — which newtype constructors a module binds: Check's environment includes its children's newtypes, Elab's does not; fixed 2026-10-25: one definition, the module's own and its imports'
silent: no
ruling: Violates: MOD-1@1
-->

**Status:** FIXED (2026-10-25, branch `wip/newtype-ctor-stage`)
**Found:** 2026-10-25, by the agent fixing the identity follow-ups of bugs 242 to 254, probing
newtype construction across module boundaries.
**Severity:** loud. An invalid program is refused by the wrong stage: Check admits it and Elab
refuses it with E0402, an internal code (a refusal Check should have made first). A valid program
is refused: a call of an imported function that shares a child newtype's spelling.

Confirmed real by Codex review, 2026-10-25: MOD-1 (docs/spec/modules.md): Check mistook an explicitly imported function for a child’s constructor and rejected its call; broader parent/child rights remain pending OD21.

## Reproducer

`tests/programs/error_262_parent_constructs_child_newtype.con`: `mod kids { pub newtype K = i64; }`
and, in the parent, `let k: K = K(30);` with no import of `K`. Observed on `wip/train24`
(`29173ffb`, main `525ea19e` alike): Check admits the program, then
`error[elab]: (E0402) call to undeclared function 'K'`, natively and under `--interp`. Expected: a
refusal at Check, `(E0263) call to undeclared function 'K'`.

`tests/programs/regress_262_imported_fn_beside_child_newtype.con`: the same child newtype `K`
beside `import lib.{K}`, a function. Check took `K(30)` for the child newtype's constructor and
refused the program (`(E0220) type mismatch in return value: expected i64, got K`), while Elab
binds `K` to the imported function. Expected: 42.

## Rule

MOD-2@1 ([docs/spec/modules.md](../spec/modules.md#MOD-2)): a declaration is private to its module
unless it is `pub`, and another module uses it only by importing it. A child module is another
module (TYPE-11 and [CONSTRUCTION_RIGHTS.md](../spec/types.md#TYPE-11): visibility is
module-granular). So the parent calls `K`'s constructor only after importing it. Elab already binds
exactly that: a newtype constructor is the module's own newtype or an imported one
(`m.newtypes ++ imports.newtypes`). The rule is unchanged; Check now enforces it.

## Cause

Check's environment for a module holds its children's newtypes too (`allNewtypes`, bug 229: they
are types the parent may write, as in `let k: K = kids::mk();`), and its call rule took every newtype
of that environment for a constructor bound here. Elab's environment holds only the module's own
newtypes and its imports. Which newtype constructors a module binds by spelling was decided twice,
from two lists, and the lists differ by the children's newtypes. Construction rights (TYPE-11) were
not the cause: Check counts a child's newtypes among the module's own for them, so it never asked.

## Fix

One definition, `newtypeConstructorsInScope` (`Concrete/Resolve/FileSummary.lean`): the module's own
newtypes and its imports'. Elab binds its newtypes from it; Check carries it as
`TypeEnv.newtypeConstructors`, and a call of a newtype constructor not in it is refused with
E0263, the message Elab gave: Check looks a constructor up in that list, as Elab does, and a
spelling not in it is checked as a function call. The children's newtypes stay in Check's
environment as types.

Regression legs in `run_tests.sh`, native (`run_err`) and `--interp`: the reproducer (E0263), and
two controls of the same rule refused before the fix too: a sibling's imported newtype
(`error_262_sibling_constructs_newtype.con`) and a parent's imported newtype in its child
(`error_262_child_constructs_parent_newtype.con`), both E0296. The positive control
`regress_262_own_rights_beside_child.con` (a module with a child constructs its own newtype and
struct, writes the child's newtype as a type and obtains its value from the child's constructor
function) and `regress_262_imported_fn_beside_child_newtype.con` return 42 natively and under
`--interp`.

## Not changed

- Resolve still binds a child's newtype in its parent as a symbol (it mirrors Check's environment,
  bug 229), so this program passes Resolve and is refused at Check; a sibling's or a parent's
  newtype named without an import is refused at Resolve (E0101).
- When the parent imports the child's newtype (`import kids.{K}`), every stage admits `K(30)`, and
  a parent likewise builds and reads the private fields of a struct it imports from its child:
  Check's construction and field rights count the children's declarations among the module's own
  (since construction rights slice 1, 2026-07-18; `tests/programs/module_qualified_impl` builds a
  child module's struct with private fields). TYPE-11@1 says such a construction is refused
  outside the declaring module. Whether a parent holds its children's representation rights is a
  language decision for the rule's owner (R-0011); this fix does not change it.

## Structural cause

Class 12 ("one fact kept in two places"): which newtype constructors a module binds by spelling
was computed by Check and by Elab from different lists.

## Structural owner

R-0492 / R-0514: one module environment, built once, from which every front-end stage reads what a
spelling denotes. Until then the bound constructors have one definition both stages call.
