+++
title = "Project"
+++

# Creating a project

Concrete projects are plain directories — there is no generator command (a
`concrete new` scaffolding command is roadmap Phase 18 work, not implemented
today). Create the two files by hand:

```bash
mkdir my_project
cd my_project
```

`Concrete.toml`:

```toml
[package]
name = "my_project"
version = "0.1.0"
license = "MIT"
```

`src/main.con`:

```con
mod my_project {
    fn main() with(Std) -> Int {
        println("hello");
        return 0;
    }
}
```

## Building A Project

```bash
concrete build     # compile the package
concrete run       # compile and run src/main.con
concrete test      # run #[test] functions
```

The manifest drives the project model: `[package]` metadata, the `src/`
layout, and (for examples in this repository) dependency on the `std`
package. See any `examples/*/Concrete.toml` for a working manifest.

## Current State

The project/package workflow is still evolving. Today:

- the manifest + `src/` layout is the supported project shape
- it is not yet the final long-term package/dependency model
- the roadmap treats package/dependency ecosystem work as a later explicit phase

If you are working directly on the language or compiler, you will still often
use the repository-level workflow rather than the project flow.
