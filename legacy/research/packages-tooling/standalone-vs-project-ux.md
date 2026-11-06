# Standalone Vs Project UX

Status: open

Phase H improved standalone usability with builtins for printing and timing, but the split between:

- `concrete file.con`
- project-based code with stdlib dependencies

is still too visible.

## Problem

Single-file examples and benchmarks should be able to do ordinary things without awkward scaffolding, but the full language system still needs a real package/project model.

## Questions

1. What should “just work” in standalone mode?
2. Which stdlib facilities should remain project-only?
3. Should standalone mode expose a tiny hosted prelude, or should it stay thin and rely on builtins?
4. How should examples and benchmarks be written so they are honest about the project model?

## Likely Direction

- keep standalone mode small and explicit
- expose a narrow always-available surface for common examples and benchmarks
- let richer functionality remain part of the package/project model

## Why It Matters

If this split stays awkward, Phase H examples will keep teaching workarounds instead of the intended language experience.

Concrete example already observed in Phase H:

- standalone benchmarks cannot conveniently use `std.fs.read_to_string`
- as a result, serious parser benchmarks get pushed toward ad hoc `trusted extern fn` wrappers even though the better stdlib path already exists

That makes this a real package/dependency resolution gap, not just a hypothetical UX note.
