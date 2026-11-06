+++
title = "Modules"
+++

# Modules

Modules are the basic unit of organization in Concrete.

They group:

- functions
- types
- impl blocks
- submodules
- imports

## A Small Module

```rust
mod math {
    pub fn double(x: Int) -> Int {
        return x * 2;
    }

    pub fn triple(x: Int) -> Int {
        return x * 3;
    }
}

fn main() with(Std) -> Int {
    return math::double(21);
}
```

The syntax is familiar, but modules matter for more than namespacing. They are part of how Concrete keeps visibility, resolution, and project structure explicit.

## Why Modules Matter

Modules are important to:

- visibility boundaries
- name resolution
- multi-file structure
- future package/dependency behavior

Concrete's package/dependency model is still a later roadmap phase, but the compiler already treats modules as a serious boundary rather than just syntax decoration.

## Nested Modules

Concrete also supports nested modules:

```rust
mod net {
    mod tcp {
        pub fn default_port() -> Int {
            return 8080;
        }
    }
}
```

This keeps related functionality together without flattening everything into one namespace.

## The Bigger Point

In many languages, modules are easy to treat as boring plumbing.

In Concrete they matter because the compiler is intentionally explicit about:

- where names come from
- what gets imported
- what belongs to a public boundary
- what later becomes part of package and interface artifacts

So modules are part of the language's semantic clarity, not just part of its file layout.
