### Calls With Many Arguments Compile in Linear Time, and Wide Matches Merge Without Copying (bug 297, 2026-10-26)

The elaborator and the lowering pass built a call's argument list, and the lowering pass each
merged value of a `match`, by appending one element at a time to a list that was copied each
time, so a call of 12000 arguments took a third of a second in each pass. The lists are now built
in one pass and the generated code is unchanged (a wide `match` still has a superlinear
dominator computation, fixed separately as a follow-up of bug 178)
([bug 297](docs/bugs/297_call_arguments_and_merge_inputs_appended.md)).
