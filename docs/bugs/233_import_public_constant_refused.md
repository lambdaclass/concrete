# Bug 233 — a public constant cannot be imported, and the refusal says it is not public

**Status:** OPEN (contained 2026-10-24 on `wip/diffhunt-1025`: the import is refused with its
reason; importing constants waits for constant identity, R-0492)
**Found:** 2026-10-24, by the differential hunt on main `9622f7e0` (module constants across
modules): `mod cfg { pub const BASE: i32 = 7; } import cfg.{BASE};` is refused by both the binary
and `--interp` with `(E0285) 'BASE' is not public in module 'cfg'`.
**Severity:** a valid program by MOD-2 is refused, with a false diagnostic; no wrong code.

## Reproducer

- `tests/programs/error_233_import_constant.con`: the program above. On main: Check's E0285 "not
  public". Now: `(E0126) 'BASE' is a constant of module 'cfg', and a constant cannot be imported
  yet: …`.
- `tests/programs/error_233_import_trait.con`: `import a.{Show}` of a `pub trait`. On main: E0285
  "not public"; traits are module-local and cannot be imported (TRAIT spec), but they are public.
  Now: E0126, "'Show' is a trait of module 'a', and a trait cannot be imported: traits are
  module-local".
- `tests/programs/error_233_nested_import_constant.con`: the constant import in a nested
  submodule, whose imports Check validates. Now E0323 with the same reason.

Expected by MOD-2@1 ("another module uses [a `pub` declaration] only by importing it"): the
constant import is accepted and `main` returns 7. A qualified use, `cfg::BASE`, is refused too
(E0100, "undeclared variable 'cfg_BASE'"); MOD-2 names import as the way in, so that is not this
bug.

## Cause

`FileSummary.publicNames`, the list Resolve's import check reads, includes public traits and
constants, so `Resolve.summaryImports` admitted the import; it then found no struct, enum,
function, newtype or alias of that name and bound nothing. `FileSummary.resolveImports`, the rule
Check and Elab apply, has no constant or trait case at all and fell through to its last arm, "not
public". Nothing below the import can carry an imported constant either: Elab elaborates only a
module's own constants (`ConstSummary` has no initializer), and Lower inlines a constant by its bare
spelling from the module's own tree (`collectAllConstants`; bug 132 refuses two modules' constants
of one name for that reason).

## Containment

`FileSummary.unimportableKind` names a public trait or constant, and both import rules refuse it
with the reason: Resolve (E0126, top-level modules), Check (E0323) and Elab (E0423) through
`resolveImports`. Traits stay unimportable by design, so that half is fixed; the constant half is
open. Regression legs in `run_tests.sh`: the three `error_233_*.con` fixtures (`run_err` on the code).

## Structural cause

Class 12 ("one fact kept in two places"): which declarations an import may name is stated twice,
as the module's public-name list (every `pub` declaration) and as the import rules' per-kind cases,
and the two disagree for traits and constants; and class 1 for the open half: a constant's identity
in Core is its spelling, so there is nothing an import could bind.

## Structural owner

R-0492/R-0514 (one resolver with `DefId`s: an import binds a declaration identity, which a constant
reference then carries through Elab and Lower). Importing a constant is accepted when that lands;
until then the refusal states the reason.
