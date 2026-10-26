### Checking Many Declarations and Calls Takes Linear Time (bug 275, 2026-10-26)

Check's duplicate-declaration rule searched an association list of the declarations seen so far
for every declaration, and a call found its callee by searching a list of every function's name
and walking the signature list, so a module of n declarations took time n² to check (a chain of
20000 functions each calling the next: 1.5 s, now 77 ms). The declarations seen and the callee
tables are maps, the signatures an array. Diagnostics and output are unchanged
([bug 275](docs/bugs/275_check_declaration_collisions_assoc_list.md)).
