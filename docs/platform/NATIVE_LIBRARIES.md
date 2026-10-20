# Native Libraries

Status: implemented 2026-10-21 (closes gap G1 of [ROCKSDB_SLICE1.md](ROCKSDB_SLICE1.md)); the
smallest mechanism, not the R-0285 build recipe.

A package whose `extern fn`s resolve against a C library declares that library in its
`Concrete.toml`. `concrete build`, `concrete run` and `concrete test` link every declared library
into the final binary, for the root package and for each dependency package, so an application
that depends on a binding package needs no linker flags of its own. Gate:
`scripts/tests/check_native_libs.sh`.

## The section

```toml
[native]
pkg-config = ["rocksdb"]     # flags from `pkg-config --libs rocksdb`, resolved at link time
libs       = ["m"]           # -l<name>
lib-dirs   = ["native/lib"]  # relative to the declaring package's root: -L<dir> and an rpath
```

All three keys are optional, each takes a one-line array of plain strings, and a package may use
any combination. pkg-config is the preferred form: it carries the library's own search path and
its transitive libraries. `libs` and `lib-dirs` cover a library with no `.pc` file, including one
shipped inside the package.

## Semantics

- **Scope.** The declarations of the root package and of every package it loads under
  `[dependencies]` are collected by `loadProject` (`ProjectContext.nativeLibs`, in
  `Concrete/Resolve/Project.lean`). The final link receives them in order: the root package's
  first, then each dependency's in declaration order, so a library precedes the libraries it uses
  when they are static archives.
- **Link only.** Resolution happens at the link step. `check`, the reports and `build --emit-llvm`
  read the declarations but do not run pkg-config, so they work without the library installed.
- **Directories.** Each `lib-dirs` entry and each `-L` directory pkg-config emits is canonicalised
  and must exist. It is passed as `-L<dir>` and `-Wl,-rpath,<dir>`, so the binary finds a shared
  library without `LD_LIBRARY_PATH` or `DYLD_LIBRARY_PATH`. Duplicate directories are passed once.

## Fail closed

Every one of these stops the command with a diagnostic naming the declaring package. Nothing is
dropped, because a dropped library reappears as an unresolved symbol far from its cause, which is
what G1 recorded.

| condition | refused at |
|---|---|
| unknown key, malformed or multi-line array, escape sequence, `[native.x]` or `[[native]]` table | load (every command) |
| a name that is not `[A-Za-z0-9_][A-Za-z0-9_.+-]*` (at most 128 characters) | load |
| a `lib-dirs` entry that is empty, starts with `-`, or contains `,` or a control character | load |
| pkg-config not on `PATH` | link |
| a pkg-config package pkg-config cannot find (its own `--print-errors` text is included) | link |
| a pkg-config `--libs` token other than `-L<dir>`, `-l<name>`, `-pthread`, `-framework <name>` | link |
| a library directory that does not exist or is not a directory | link |

A link that still fails prints the declared libraries, or says none is declared and points here.

## No flag injection

Every value reaches clang as one argument vector element; no shell is involved. A name cannot
begin with `-`, so it cannot become a flag, and it cannot contain a space or `,`. A directory
cannot contain `,`, which `-Wl,-rpath,<dir>` would otherwise split into further linker arguments.
pkg-config output comes from a `.pc` file the manifest does not control, so it is checked against
the allow-list above instead of being forwarded: a `.pc` that emits `-Wl,...` or any other flag is
refused.

## Trust

A native library is code linked into the binary that Concrete does not check, so it is a trusted
dependency of every program that links it. It is listed:

- by `--report unsafe`, in a "Native libraries (trusted foreign dependencies …)" section after the
  foreign effect declarations, one line per declaration and package;
- in the compiler ledger (`concrete --report compiler-ledger`) as a `native-library` fact.

The foreign functions themselves remain the audited assumptions [FFI.md](FFI.md) describes; the
declaration says which library supplies them, not that they behave as declared.

## Not covered

- Building a native library from source, fetching it, or pinning its version and hash. That is
  the R-0285 build recipe and lock entry ([FFI_GLUE.md](FFI_GLUE.md)); the resolved
  pkg-config version is not yet recorded in any evidence.
- Static-versus-shared selection, `pkg-config --static`, and per-target declarations.
- Transitive package dependencies: the loader reads the root's `[dependencies]` only, so the
  declarations collected are those of the packages it loads.
