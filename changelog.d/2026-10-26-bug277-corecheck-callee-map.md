### CoreCheck Finds a Callee's Signature by Name, Not by a Search of Every Signature (bug 277, 2026-10-26)

CoreCheck looked up every direct call's capabilities and signature in a list of all the program's
function signatures, so a program of n functions with a call each took time n² to validate (a
chain of 20000 functions: 2.5 s, now 26 ms). The signatures are a map, first entry of a name
winning as before. Output, diagnostics and reports are unchanged
([bug 277](docs/bugs/277_corecheck_callee_signature_list.md)).
