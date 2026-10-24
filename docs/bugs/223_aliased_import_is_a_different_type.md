# Bug 223 — an imported type under an alias is a different type

**Status:** OPEN
**Found:** 2026-10-24, while checking bug 222's fix on the import paths (probe programs on the
alias form of each import, Phase 7 workload 10).
**Severity:** loud. A valid program is refused; nothing miscompiles. `import m.{P as Q}` gives
the importer a type `Q` that has none of `P`'s methods and is not `P`: a value of type `P`, from
any function of `m`, does not have type `Q`.

## Reproducer

`tests/regressions/bug223_aliased_import_distinct_type/repro.con`:

```con
mod m {
    mod shapes {
        pub struct Copy P { pub v: i64 }
        impl P {
            pub fn mk(n: i64) -> P { return P { v: n }; }
            pub fn get(&self) -> i64 { return self.v; }
        }
        pub fn make(n: i64) -> P { return P { v: n }; }
    }
    mod user {
        import m.shapes.{P as Q, make};
        pub fn run() -> i64 {
            let a: Q = Q::mk(40);      // E0264 no method 'mk' on type 'Q'
            let b: Q = make(2);        // E0220 expected Q, got P
            return a.get() + b.v;      // E0264 no method 'get' on type 'Q'
        }
    }
    import user.{run};
    pub fn main() -> i64 { return run(); }
}
```

Observed on main `408880cb` and on this branch: E0264 (twice) and E0220. The same program without
`as Q` returns 42. The same holds for an aliased enum. Expected: 42.

## Cause

Bug 069 made an aliased import register the type's declaration under the importer's spelling
(`{ sd with name := localName }`), so `Q` resolves to a struct. Everything else about the type is
still keyed by its definition spelling: impl blocks and method signatures by `P` (`P_mk`), and the
types inside imported signatures as `.named "P"`. The checker compares named types and looks up
methods by spelling, so `Q` and `P` are two types, and `Q`'s methods are looked for as `Q_mk`.
Elab's imported method table is already keyed by the local spelling and identified by the
definition (its comment notes that Check refuses the call first), so the remaining gap is the
type identity the checker uses.

## Fix

Not fixed. Rebasing the impl lists onto the alias at the import would make `Q::mk` resolve but
leave `make(2): P` unequal to `Q`; renaming every type inside imported signatures would be a third
copy of "which spelling is this type". The fix is to compare and look up types by their declaration
identity, which is the identity migration below.

## Structural cause

Class 1 ("declaration identity by spelled name"): a type is identified by the string it is written
with, so a second spelling of one declaration is a second type.

## Structural owner

R-0492 (S5 `DefId`), applied to the front end by R-0514: the inherited obligation "Aliased
imported method (E0264)" (merged through R-0008) already requires that the identity migration
resolve this and add an exact alias-method fixture. The reproducer is pinned as an open bug by
`check_example_outputs.sh` (open-bug reproducers, compile-fails, E0264), which fails when it starts
compiling, so the fix updates this record.
