# Modules and Names

A program is a tree of modules, one source file per module, loaded by `mod name;`. Every
declaration is private to its module unless it is declared `pub`, and another module reaches it
only by importing it (`import a.{f, T as U};`). Names resolve once, before checking, to exactly
one declaration. This chapter covers name resolution (MOD-1), visibility (MOD-2), one binding
per name in a module (MOD-3), loading module files (MOD-4), the privacy of `std.libc` (MOD-6),
generated interfaces (MOD-7, pending), calls the compiler generates (MOD-8) and what an import binds
(MOD-9). There is no MOD-5: the number was never allocated, and IDs are not renumbered
([MAINTAINERS.md](MAINTAINERS.md) §2.1). Representation privacy (which fields and constructors another module
may use) is TYPE-11, and trait coherence is the traits chapter.

One question is recorded without an entry yet: SCOPE-4, whether a local may reuse the name of a
top-level function, an import alias or an intrinsic, is still open; today it may. TIGHTENING_DECISIONS
D9 (one source file per module with `pub` as its interface, and interface artifacts the compiler
generates and checks) is decided and scheduled late; it is MOD-7, pending R-0556. The design record is
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
The builtins include the enums `Option` and `Result`, which a module uses without an import; how
a user type of the same name resolves is OPEN-43. The types chapter's
[builtin catalog](types.md#builtin-functions) records the current compiler's builtin functions,
their signatures, the capabilities it checks and how it handles their names.

```con reject name=undeclared-name code=E0100 stage=resolve pair=declared-name
fn example_value() -> i64 {
    return missing + 1;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=declared-name exit=42
fn example_value() -> i64 {
    let present: i64 = 41;
    return present + 1;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```



<a id="MOD-2"></a>
### MOD-2. Only `pub` declarations cross a module boundary

**MOD-2@1.** A declaration is private to its module unless it is declared `pub`, and another module uses it only by importing it from a module that exists, where it is `pub`.

- Kind: refusal
- Uses: none
- Codes: E0110 (resolve), E0111 (resolve), E0126 (resolve), E0284 (check), E0285 (check), E0323 (check), E0418 (elab), E0419 (elab), E0423 (elab)
- Status: enforced
- Enforced by: Concrete.Pipeline.resolve, Concrete.importRefusal
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Private by default makes every public API intentional: a missing `pub` cannot
export a helper by accident. `pub` on a type exports its name, not its representation (TYPE-11).
There is one public level today; a package-internal `pub(pkg)` is designed for when packages
depend on packages. Resolve, Check and Elab each verify imports (E0111, E0285, E0419).

**Codes by case.** Each of the three refusals has one code per pass that can report it, Resolve
first and Check or Elab when they meet the import again (`importCode` and `unimportableCode` in
`Concrete/Resolve/FileSummary.lean`): an unknown module is E0110 (resolve), E0284 (check) or E0418
(elab); a declaration that is not `pub` is E0111, E0285 or E0419; and an import naming a public
trait, which no import binds, is E0126, E0323 or E0423. On a single file the import refusals are
reported at resolve (E0110, E0111, E0126); a qualified call to a private function there is E0101
(unknown function).

Visibility has two levels, `pub` and private, and private is the default, so a module's public
surface is explicit and nothing is exported by accident; a `pub(pkg)` level is deferred until
packages need it, and `pub(super)` or `pub(in path)` will not be added (restructure the modules
instead). There are no glob imports, permanently: every imported name is listed, so each module
begins with a manifest of what it uses and a name's origin is always visible (STDLIB design
principle 8). A module's non-public items are not in its `FileSummary.publicNames` and cannot be
named in an import list (E0111). Capabilities are not hidden by a module boundary: calling a
function needs the capabilities in its signature (TRUST-4), so a module cannot smuggle a `File`
call behind an `Alloc` signature, and each public function declares only what it needs; std contains
`Unsafe` behind `trusted` functions that expose domain capabilities (operations need their declared resource and permission).
`--report caps`, `--report authority` and `--report interface` show these surfaces, and a package
policy (`[policy] deny = ["Unsafe"]`) can refuse capabilities. The visibility design and its
pre-package hygiene checklist are the archived
[VISIBILITY_AND_MODULE_HYGIENE.md](../archive/design-records/VISIBILITY_AND_MODULE_HYGIENE.md)
(several of its first-release choices, such as all fields and variants visible, were since replaced
by TYPE-11 and R-0011).

An import binds a name in the importing module only and does not make it `pub` there, so a module
cannot re-export what it imports, and `pub import` is a parse error (SYN-1).

```con reject name=import-private code=E0111 stage=resolve pair=import-public
// spec-file: main.con
mod a;
import a.{hidden};
fn example_value() -> i64 {
    return hidden();
}
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: a.con
fn hidden() -> i64 {
    return 1;
}

```

```con reject name=submodule-imports-private code=E0285 stage=check pair=import-public
// spec-file: main.con
mod b;
mod a;
fn example_value() -> i64 {
    return b::g();
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: a.con
fn secret() -> i64 {
    return 42;
}

// spec-file: b.con
import a.{secret};
pub fn g() -> i64 {
    return secret();
}

```

```con reject name=reexport code=E0111 stage=resolve pair=import-public
// spec-file: main.con
mod c;
mod b;
mod a;
import c.{g};
fn example_value() -> i64 {
    return g();
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: a.con
pub fn f() -> i64 {
    return 42;
}

// spec-file: b.con
import a.{f};

// spec-file: c.con
import b.{f};
pub fn g() -> i64 {
    return f();
}

```

```con accept name=import-public exit=42
// spec-file: main.con
mod a;
import a.{shown};
fn example_value() -> i64 {
    return shown();
}
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}

// spec-file: a.con
pub fn shown() -> i64 {
    return 42;
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
reserved for functions only, and a `let` spelling the compiler reserves is SYN-3. The
[builtin catalog](types.md#builtin-functions) records which builtin names the current compiler
reserves. A probe of a program function with a builtin's name is recorded there only for
`string_length`. Whether a program may declare a function named `print`, `println` or `append` is
OPEN-133; whether it may declare one whose name is another builtin's, other than the names MOD-3
reserves, and which function the program's calls of that name reach, is OPEN-153.
The current compiler also counts an impl method under its definition name `Type_method`, so a free function
`P_get` beside `impl P { fn get(&self) ... }` in one module is refused with E0299; whether a method
should occupy that name is OPEN-135.

```con reject name=method-mangled-name code=E0299 stage=check pair=method-beside-free-function
struct Copy P { x: i64 }
impl P {
    fn get(&self) -> i64 {
        return self.x;
    }
}
fn P_get(a: i64) -> i64 {
    return a;
}
fn example_value() -> i64 {
    let p: P = P { x: 3 };
    return (&p).get() + P_get(4);
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=method-beside-free-function exit=7
struct Copy P { x: i64 }
impl P {
    fn get(&self) -> i64 {
        return self.x;
    }
}
fn get_p(a: i64) -> i64 {
    return a;
}
fn example_value() -> i64 {
    let p: P = P { x: 3 };
    return (&p).get() + get_p(4);
}
fn main() -> u8 {
    if example_value() == 7 {
        return 7;
    }
    return 1;
}
```

```con reject name=import-clashes-with-local code=E0299 stage=check pair=renamed-import
// spec-file: main.con
mod a;
import a.{f};
fn f() -> i64 {
    return 2;
}
fn example_value() -> i64 {
    return f();
}
pub fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: a.con
pub fn f() -> i64 {
    return 40;
}

```

```con accept name=renamed-import exit=42
// spec-file: main.con
mod a;
import a.{f as g};
fn f() -> i64 {
    return 2;
}
fn example_value() -> i64 {
    return g() + f();
}
pub fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}

// spec-file: a.con
pub fn f() -> i64 {
    return 40;
}

```

<a id="MOD-4"></a>
### MOD-4. A `mod` declaration loads one module file

**MOD-4@2.** A `mod X;` declaration loads exactly one file `X.con` beside the declaring file, and inline modules, the `X/mod.con` fallback, a missing file and a file already loaded by another declaration are refused.

- Kind: refusal
- Uses: none
- Codes: new (parse), E0122 (resolve), E0123 (resolve), E0124 (resolve)
- Status: pending R-0555
- Enforced by: Concrete.Pipeline.resolveFiles
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD9, OD33, OD61 and OD142 give a single source file per module, only `X.con`, without inline modules or a directory fallback.

```con reject name=missing-module-file-decided code=E0122 stage=resolve pair=module-file pending=R-0555 now=reject:E0122:resolve
mod missing;

fn main() -> u8 {
    return 42;
}
```

```con reject name=inline-module-removed code=new stage=parse pair=module-file pending=R-0555 now=accept:42
mod present { pub fn answer() -> u8 { return 42; } }
fn main() -> u8 { return present::answer(); }
```

```con accept name=module-file exit=42
// spec-file: main.con
mod present;
import present.{answer};
fn example_value() -> i64 {
    return present::answer();
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}

// spec-file: present.con
pub fn answer() -> i64 {
    return 42;
}

```

<a id="MOD-6"></a>
### MOD-6. The bindings of `std.libc` are private to std

**MOD-6@1.** The `extern` bindings of `std.libc` are imported only by modules of std, so no module of a program names them.

- Kind: refusal
- Uses: none
- Codes: E0326 (resolve, check)
- Status: enforced
- Enforced by: Concrete.importRefusal
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD20). std reaches the C library through
`std.libc`, whose `extern` declarations are not `pub`; std's own modules import them, and a program
uses std's wrappers instead. Until bug 226's fix the import rule of a submodule admitted a private
declaration, which is how std's modules reached `std.libc` and also how a program's submodule
could; one import rule (`importRefusal`) now gives MOD-2 to every module and keeps `std.libc`
visible to std alone, `pub` or not. A module belongs to std by its definition path (`std` and the
modules under it).

```con reject name=program-imports-libc code=E0326 stage=check pair=program-uses-std project=libcuser interp-exempt=project-needs-std
// spec-file: main.con
mod b;
fn example_value() -> i64 {
    return b::g();
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}

// spec-file: b.con
import std.libc.{strlen};
pub fn g() -> i64 {
    return 42;
}

```

```con reject name=program-root-imports-libc code=E0326 stage=resolve pair=program-uses-std project=libcroot interp-exempt=project-needs-std
import std.libc.{strlen};

fn main() -> u8 {
    return 42;
}
```

```con accept name=program-uses-std exit=42 project=stduser interp-exempt=project-needs-std
import std.string.{String};

fn main() -> u8 {
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
(MOD-4). OD33 and OD61 refuse inline modules; one source file declares one module.

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
`println` in it call that function (bug 165); in the example below that function divides by zero, so
binding `println` to it would abort.

```con accept name=generated-call-ignores-same-name exit=42
fn print_int(x: i64) -> i64 {
    let z: i64 = x - x;
    return 42 / z;
}

fn main() with(Fs) -> u8 {
    println(7);
    return 42;
}
```

<a id="MOD-9"></a>
### MOD-9. An import binds the declaration it names

**MOD-9@2.** Importing a `pub` function, foreign binding, struct, enum, trait, type alias or constant binds that declaration itself, and an import alias of a struct or enum names the same type, with its methods and impls, in every pass.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.resolveImports
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A public trait is explicitly exported and imported like other declarations (owner, 2026-11-02); aliases preserve declaration identity, and removed newtypes introduce none.

```con accept name=import-constant-and-alias exit=42
// spec-file: main.con
mod user;
mod shapes;
import user.{run};
pub fn main() -> u8 { if run() == 42 { return 42; } return 1; }

// spec-file: shapes.con
pub const BASE: i64 = 38;
pub struct Copy P { pub v: i64 }
impl P {
    pub fn mk(n: i64) -> P { return P { v: n }; }
}

// spec-file: user.con
import shapes.{P as Q, BASE};
pub fn run() -> i64 {
    let a: Q = Q::mk(4);
    return a.v + BASE;
}

```


## Version history

This appendix records superseded versions and is not normative.

- MOD-4@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; MOD-4@1 is superseded by version.
- MOD-9@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; MOD-9@1 is superseded by version.
