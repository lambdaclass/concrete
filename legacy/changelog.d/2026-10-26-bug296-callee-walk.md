### Functions With Many Calls Are Elaborated and Verified in Linear Time (bug 296, 2026-10-26)

The walk that lists the functions a body calls appended each statement's calls to the list built
so far, and the verifier after monomorphization removed duplicates by comparing every pair, so a
function of n calls took time n² in two stages (20000 calls: 2.7 s and 2.4 s). The walk now
collects in one pass and duplicates are removed with a set; the same program takes 0.3 s and
0.2 s, and every call list is unchanged ([bug 296](docs/bugs/296_callee_walk_list_appends.md)).
