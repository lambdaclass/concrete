# Creating a project

Concrete includes a `new` subcommand for creating a project skeleton.

To create a project:

```bash
concrete new <project_name> [--lib]
```

Pass `--lib` to create a library project instead of a binary-oriented one.

## What It Creates

The intent is to give you a starting project with:

- a project directory
- package/project metadata
- a main binary or library entry point

The exact layout may still evolve as Concrete's package/project model matures.

## Building A Project

To build the created project:

```bash
cd <project_name>
concrete build
```

## Current State

The project/package workflow is still evolving. Today:

- it is useful as a starting point
- it is not yet the final long-term package/dependency model
- the roadmap treats package/dependency ecosystem work as a later explicit phase

If you are working directly on the language or compiler, you will still often use the repository-level workflow rather than only the `concrete new` path.
