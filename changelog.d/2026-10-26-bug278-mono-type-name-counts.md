### Monomorphizing Many Structs or Enums No Longer Filters Every Type Name per Name (bug 278, 2026-10-26)

Mono found the program's ambiguous type names by filtering the whole list of type names once per
name, so a program of n structs or enums took time n² to monomorphize (20000 structs: 2.4 s, now
4 ms). Each name is counted once in a map. The many-functions shape's monomorphize stage, recorded
as growing faster than linearly, does linear work (instruction counts double with the size); its
elapsed-time excess is not an algorithm. Output and reports are unchanged
([bug 278](docs/bugs/278_mono_type_name_collision_filter.md)).
