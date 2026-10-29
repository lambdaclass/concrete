# Modules and Names

A program is a tree of modules (`mod name { ... }`, or one file per module in a package). Every
declaration is private to its module unless it is declared `pub`, and another module reaches it
only by importing it (`import a.{f, T as U};`). Names resolve once, before checking, to exactly
one declaration. This chapter covers name resolution (MOD-1), visibility (MOD-2), one binding
per name in a module (MOD-3) and loading module files (MOD-4). Representation privacy (which fields and constructors another module
may use) is TYPE-11, and trait coherence is the traits chapter.

Two decisions are recorded without an entry yet. SCOPE-4, whether a local may reuse the name of a
top-level function, an import alias or an intrinsic, is still open: today it may. TIGHTENING_DECISIONS
D9 (one source file per module with `pub` as its interface, and interface artifacts the compiler
generates and checks) is decided and scheduled late; it enters as an entry with its owner when that
work starts. The design record is
[VISIBILITY_AND_MODULE_HYGIENE.md](../archive/design-records/VISIBILITY_AND_MODULE_HYGIENE.md).

## Rules

<a id="MOD-1"></a>
### MOD-1. Every name resolves to one declaration

**MOD-1@1.** Every name a program uses resolves to exactly one declaration visible where it is used (a local binding, a declaration of the module, an import, a type parameter or a builtin), and a name that resolves to none, a type alias that names itself, and `Self` outside an `impl` are refused.

- Kind: refusal
- Uses: none
- Codes: E0100 (resolve), E0101 (resolve), E0102 (resolve), E0103 (resolve), E0104 (resolve), E0105 (resolve), E0106 (resolve), E0107 (resolve), E0108 (resolve), E0109 (resolve), E0112 (resolve), E0125 (resolve), E0200 (check), E0201 (check), E0202 (check), E0263 (check), E0267 (check)
- Status: enforced
- Enforced by: Concrete.Pipeline.resolve
- Semantics: none
- Obligation: resolution by identity (`DefId`, `LocalId`), never by spelling (R-0492)
- Evidence: tested

**Rationale.** Resolution validates every name once, before checking. Later passes do not yet
work only with what a name denotes: Elab and CoreCheck still look some names up by spelling, and
R-0492 moves them to identities. Bugs 039 to 045 were names resolved more than once, differently; binders
are now keyed by identity (SCOPE-1, SCOPE-3), and declarations get the same treatment under R-0492.
The checker repeats some lookups (E0200 to E0202, E0263, E0267) as defense in depth.

```con reject name=undeclared-name code=E0100 stage=resolve pair=declared-name
fn main() -> Int {
    return missing + 1;
}
```

```con accept name=declared-name exit=42
fn main() -> Int {
    let present: Int = 41;
    return present + 1;
}
```

<a id="MOD-2"></a>
### MOD-2. Only `pub` declarations cross a module boundary

**MOD-2@1.** A declaration is private to its module unless it is declared `pub`, and another module uses it only by importing it from a module that exists, where it is `pub`.

- Kind: refusal
- Uses: none
- Codes: E0110 (resolve), E0111 (resolve), E0126 (resolve), E0284 (check), E0285 (check), E0323 (check), E0418 (elab), E0419 (elab), E0423 (elab)
- Status: partial bug-226
- Enforced by: Concrete.Pipeline.resolve
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Private by default makes every public API intentional: a missing `pub` cannot
export a helper by accident. `pub` on a type exports its name, not its representation (TYPE-11).
There is one public level today; a package-internal `pub(pkg)` is designed for when packages
depend on packages. Resolve, Check and Elab each verify imports (E0111, E0285, E0419).

Visibility has two levels, `pub` and private, and private is the default, so a module's public
surface is explicit and nothing is exported by accident; a `pub(pkg)` level is deferred until
packages need it, and `pub(super)` or `pub(in path)` will not be added (restructure the modules
instead). There are no glob imports, permanently: every imported name is listed, so each module
begins with a manifest of what it uses and a name's origin is always visible (STDLIB design
principle 8). A module's non-public items are not in its `FileSummary.publicNames` and cannot be
named in an import list (E0111). Capabilities are not hidden by a module boundary: calling a
function needs the capabilities in its signature (TRUST-4), so a module cannot smuggle a `File`
call behind an `Alloc` signature, and each public function declares only what it needs; std contains
`Unsafe` behind `trusted` functions that expose domain capabilities (`println` needs `Console`).
`--report caps`, `--report authority` and `--report interface` show these surfaces, and a package
policy (`[policy] deny = ["Unsafe"]`) can refuse capabilities. The visibility design and its
pre-package hygiene checklist are the archived
[VISIBILITY_AND_MODULE_HYGIENE.md](../archive/design-records/VISIBILITY_AND_MODULE_HYGIENE.md)
(several of its first-release choices, such as all fields and variants visible, were since replaced
by TYPE-11 and R-0011).

```con reject name=import-private code=E0111 stage=resolve pair=import-public
mod a {
    fn hidden() -> Int {
        return 1;
    }
}
mod m {
    import a.{hidden};
    pub fn main() -> Int {
        return hidden();
    }
}
```

```con reject name=submodule-imports-private code=E0111 stage=resolve pair=import-public pending=bug-226 now=accept:42
fn main() -> Int {
    return b::g();
}

mod a {
    fn secret() -> Int {
        return 42;
    }
}

mod b {
    import a.{secret};
    pub fn g() -> Int {
        return secret();
    }
}
```

```con accept name=import-public exit=42
mod a {
    pub fn shown() -> Int {
        return 42;
    }
}
mod m {
    import a.{shown};
    pub fn main() -> Int {
        return shown();
    }
}
```

<a id="MOD-3"></a>
### MOD-3. A module binds each name once

**MOD-3@2.** A module binds each name to at most one declaration, whether by a declaration or an import, an import alias does not name a compiler intrinsic, and no function definition takes a name the compiler reserves.

- Kind: refusal
- Uses: none
- Codes: E0299 (check), E0120 (resolve), E0578 (check, core-check)
- Status: enforced
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A name bound twice in a module made the meaning of a call depend on which binding a
lookup met first (bug 082). Renaming on import (`import a.{f as g}`) resolves a clash. Importing
the same declaration twice binds the name to one declaration and is accepted. The reserved names
are the `__` prefix, `user_main` and the builtins a program may call but not redefine; they are
reserved for functions only, and a `let` spelling the compiler reserves is SYN-3.

```con reject name=import-clashes-with-local code=E0299 stage=check pair=renamed-import
mod a {
    pub fn f() -> Int {
        return 40;
    }
}
mod m {
    import a.{f};
    fn f() -> Int {
        return 2;
    }
    pub fn main() -> Int {
        return f();
    }
}
```

```con accept name=renamed-import exit=42
mod a {
    pub fn f() -> Int {
        return 40;
    }
}
mod m {
    import a.{f as g};
    fn f() -> Int {
        return 2;
    }
    pub fn main() -> Int {
        return g() + f();
    }
}
```

<a id="MOD-4"></a>
### MOD-4. A `mod` declaration loads one module file

**MOD-4@1.** A `mod X;` declaration loads exactly one module from `X.con` or `X/mod.con` beside the declaring file, and a file that does not exist, holds other than one module, or was already loaded by another `mod` declaration of the program is refused.

- Kind: refusal
- Uses: none
- Codes: E0122 (resolve), E0123 (resolve), E0124 (resolve)
- Status: enforced
- Enforced by: Concrete.Pipeline.resolveFiles
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A module file is a module body, so a program's module tree is the tree of its
`mod` declarations, each file read once: a file reached twice, by a cycle or by two declarations,
is refused (E0123). A parse error inside a loaded file is that file's own
SYN-1 diagnostic (E0001), reported with the file's path, not a loading failure (bug 206).

```con reject name=missing-module-file code=E0122 stage=resolve pair=inline-module
mod missing;

fn main() -> Int {
    return 42;
}
```

```con accept name=inline-module exit=42
mod present {
    pub fn answer() -> Int {
        return 42;
    }
}

fn main() -> Int {
    return present::answer();
}
```

<a id="MOD-6"></a>
### MOD-6. The bindings of `std.libc` are private to std

**MOD-6@1.** The `extern` bindings of `std.libc` are imported only by modules of std, so no module of a program names them.

- Kind: refusal
- Uses: none
- Codes: new (resolve)
- Status: pending bug-226
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD20). std reaches the C library through
`std.libc`, whose `extern` declarations are not `pub`; std's own modules import them, and a program
uses std's wrappers instead. Today the import rule of a submodule admits a private declaration (bug
226), which is how std's modules reach `std.libc` and also how a program's submodule could; the
repair gives MOD-2 back to every program module and keeps `std.libc` visible to std alone.

```con reject name=program-imports-libc code=new stage=resolve pair=program-uses-std pending=bug-226 now=accept:42 project=libcuser interp-exempt=project-needs-std
fn main() -> Int {
    return b::g();
}

mod b {
    import std.libc.{strlen};
    pub fn g() -> Int {
        return 42;
    }
}
```

```con accept name=program-uses-std exit=42 project=stduser interp-exempt=project-needs-std
import std.string.{String};

fn main() -> Int {
    return 42;
}
```

<a id="MOD-7"></a>
### MOD-7. A module's interface is its `pub` declarations, generated

**MOD-7@1.** A module is written in one source file with no handwritten interface file, its `pub` declarations are its interface, and the compiler generates the module's interface artifact, which is compared between commits so that every change to a module's public API is reviewed as an interface change.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0556
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD9). The generator of interface artifacts and
the gate that diffs them between commits are R-0556, built on the interface data of R-0492 and
R-0497; separate compilation (V6) reads the same artifacts. `mod X;` loads one module per file
(MOD-4). Today the language also accepts an inline submodule (`mod m { ... }` written inside its
parent's file, as most examples in this specification are); whether that form is compatible with
"one file per module" is a question for the owner, OPEN-33, and this entry does not settle it.
<a id="MOD-8"></a>
### MOD-8. A generated call is bound by identity

**MOD-8@1.** A call the compiler generates, such as the runtime helpers `print`, `println` and `append` desugar into, is bound to its helper by identity and never to a declaration of the program that has the same name.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Symbols.runtimeFn
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** MOD-1 resolves the names a program writes; a generated call has no written name to resolve, so
its target is fixed when it is generated. A module that defined its own `print_int` once had every
`println` in it call that function (bug 165).

```con accept name=generated-call-ignores-same-name exit=42
fn print_int(x: Int) -> Int {
    return x + 35;
}

fn main() with(Console) -> Int {
    println(7);
    return print_int(7);
}
```

<a id="MOD-9"></a>
### MOD-9. An import binds the declaration it names

**MOD-9@1.** Importing a `pub` declaration binds that declaration itself, a constant included, and an import alias of a type names the same type, with its methods and impls, in every pass.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.resolveImports
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** MOD-2 says which declarations an import may name; this entry says what the import gives. A
`pub const` could not be imported (bug 233), and a type imported under an alias was a different
type, without its methods (bug 223).

```con accept name=import-constant-and-alias exit=42
mod m {
    mod shapes {
        pub const BASE: i64 = 38;
        pub struct Copy P { pub v: i64 }
        impl P {
            pub fn mk(n: i64) -> P { return P { v: n }; }
        }
    }
    mod user {
        import m.shapes.{P as Q, BASE};
        pub fn run() -> i64 {
            let a: Q = Q::mk(4);
            return a.v + BASE;
        }
    }
    import user.{run};
    pub fn main() -> i64 { return run(); }
}
```

