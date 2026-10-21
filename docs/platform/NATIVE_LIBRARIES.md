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
- **Single-file mode.** `concrete <file> -o out` and `concrete <file> --test` inside a package
  link that package's own declarations, refused on the same terms. File mode loads no dependency
  package, so no dependency's library is linked or needed.
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
| a key not spelled exactly `pkg-config`, `libs` or `lib-dirs` (quoted keys are not read), unknown or repeated key, malformed or multi-line array, escape sequence, a second `[native]` | load (every command) |
| any other spelling of the table: `["native"]`, `[ native ]`, `[native.x]`, `[[native]]`, a top-level `native = {...}` or `native.libs = [...]` (the line reader reads only the plain form, so it refuses the rest instead of skipping them) | load |
| a table header or top-level key written with a backslash escape (escapes are not decoded, so it could spell `native`) | load |
| a dependency manifest that exists but cannot be read (an absent one is a manifestless package) | load |
| an enclosing or dependency manifest that exists but cannot be read is never treated as absent (project discovery decides by `lstat`, so only a genuine missing entry is absence; it no longer walks past an unreadable manifest to an ancestor's) | discovery / load |
| a bracket or brace closed by the other delimiter | load |
| an assignment at array level on a line continuing a multi-line array; any text but `,` `]` `}` after a multi-line string closes on its line | load |
| any line of the manifest that is not exactly one header, one `key = value` (one string, one bracket group, or one scalar) or a comment: text trailing a header or a complete value (`[package] [native]`, `name = "x" [native]`) is refused, since skipping it would hide what it spells | load |
| the bare word `native` (outside a string) on a line continuing a multi-line array or inline table: no TOML value is that word, so it is refused rather than skipped | load |
| a name that is not `[A-Za-z0-9_][A-Za-z0-9_.+-]*` (at most 128 characters) | load |
| a `libs` name whose `-l<name>` spelling is a linker or driver option (`to_…`, `d_…`, `d6…`, `azy…`, `oad_…`, `ibrary…`: `-lto_library`, `-ld_classic`, `-ld64`, `-lazy_framework`, `-load_hidden`, GNU ld's single-dash `-library…`); the same rule applies to pkg-config's `-l` tokens | load (pkg-config: link) |
| a `lib-dirs` entry (or canonical, or pkg-config `-L`, directory) that is empty, starts with `-`, or contains `,` (splits `-Wl,`), `:` (an ELF rpath separator), `$` (an rpath token such as `$ORIGIN`) or a control character | load (pkg-config: link) |
| pkg-config not on `PATH` | link |
| a pkg-config package pkg-config cannot find (its own `--print-errors` text is included) | link |
| a pkg-config `--libs` token other than `-L<dir>`, `-l<name>`, `-pthread`, `-framework <name>` | link |
| a library directory that does not exist or is not a directory | link |

The manifest is read as STATEMENTS, not lines: each is one header or one `key = value`, however
many lines its strings, arrays and inline tables span, with strings, escapes, comments and
three-to-five-quote closers resolved. Every statement must have exactly one such shape with nothing
after it, so text quoted inside a string declares and refuses nothing, and a header or key hidden
after a closing delimiter, inside an array, or after another header is refused. The text before `=` must be a key (dotted components,
each bare or quoted). Values are validated
recursively: array elements are values, inline-table entries are `key = value` on one line (TOML
1.0), a scalar holds no brackets, strings or `=`. A header is one line with no comment inside its
brackets. Unterminated strings and values and mismatched brackets are refused.

A link that still fails prints the declared libraries, or says none is declared and points here.

## No flag injection

Every value reaches clang as one argument vector element; no shell is involved. A name cannot
begin with `-`, so it cannot become a flag, and it cannot contain a space or `,`. Prefixing `-l`
is not by itself enough, because some linker options begin with `-l`; names that would spell one
are refused (table above). A directory
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
