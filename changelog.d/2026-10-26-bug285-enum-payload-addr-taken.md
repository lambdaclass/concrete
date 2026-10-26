### An Address Taken in an Enum Payload Is Seen by Branch and Loop Lowering (bug 285, 2026-10-26)

Lower decides which locals need a stack slot before a branch or loop by walking the code for
`&x`, and that walk skipped the payload of an enum literal. A local whose address was taken only
there was given its slot inside the branch, so the other path read a never-written slot (wrong
results natively), and a loop counter split between a slot and a register (the loop never ended).
The walk now covers enum payloads and names every expression form, so a new one cannot be
skipped ([bug 285](docs/bugs/285_enum_payload_addr_taken_unseen.md)).
