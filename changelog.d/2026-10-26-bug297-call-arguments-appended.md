### Calls With Many Arguments and Matches With Many Arms Compile in Linear Time (bug 297, 2026-10-26)

The elaborator and the lowering pass built a call's argument list, and the lowering pass each
merged value of a `match`, by appending one element at a time to a list that was copied each
time, so a call of 12000 arguments took a third of a second in each pass. The lists are now built
in one pass and the generated code is unchanged
([bug 297](docs/bugs/297_call_arguments_and_merge_inputs_appended.md)).
