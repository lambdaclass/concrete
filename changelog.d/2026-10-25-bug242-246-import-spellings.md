### Four Spelling Defects Around Imports (bugs 242, 243, 245 and 246, 2026-10-25)

The probes of bug 223's fix found two more cases of an import joined by spelling, both refusals of
valid programs. A type in an imported signature is read in the importer's environment, so the
import closure's copy of it hides the importer's own type of that spelling (bug 242, E0252); and an
aliased newtype import binds no name (bug 243, E0106). Both are owned by R-0492/R-0514 and pinned as
open-bug reproducers. The review of bug 233's fix
found a third: the interpreter scopes a module constant's bare spelling program-wide, so it refuses
two modules' private constants of one name that the binary keeps apart (bug 245). A fixture for that
review also found an imported alias whose target is another alias of its module arriving as a
type the importer cannot name (bug 246, fixed: the target is expanded at the import).
