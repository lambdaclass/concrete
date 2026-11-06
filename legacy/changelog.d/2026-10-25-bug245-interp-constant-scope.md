### The Interpreter Scopes a Module Constant as Lower Does (bug 245, 2026-10-25)

`--interp` refused two top-level modules' private constants of one spelling ("defined by more than
one module") that the native binary keeps apart: the interpreter looked a bare constant spelling up
in every module's constants, Lower in the constants of the using function's top-level tree. Both
now take that scope from one definition, `CModule.treeConstants`. Pinned by
`regress_245_toplevel_private_constants.con` and `regress_245_nested_private_constants.con`,
native and `--interp`.
