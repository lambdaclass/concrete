# Getting Started

This section covers the current practical path for trying Concrete:

- what you need installed
- how to build the compiler from source
- how to compile and run a small program
- how to create a project with the `concrete` CLI

Concrete is still source-first. The language and compiler are moving quickly, so treat this book as guidance for the current tree, not as a stable packaged release manual.

## Current Fast Path

If you only want to try the compiler from the repository:

1. install the required dependencies from [Installation](./installation.md)
2. build the compiler
3. compile a small `.con` file
4. run the output binary

Typical workflow:

```bash
git clone https://github.com/unbalancedparentheses/concrete2.git
cd concrete2
make build
.lake/build/bin/concrete examples/snippets/hello_world.con -o /tmp/hello
/tmp/hello
```

## What To Read Next

- [Installation](./installation.md) for dependencies and build setup
- [Creating a project](./project.md) for the `concrete new` workflow
- [The Language](./language/intro.md) for the current language shape
- [Internal Details](./internal/index.md) if you want to understand the compiler pipeline
