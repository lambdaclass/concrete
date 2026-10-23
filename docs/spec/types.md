# Types

This chapter is a stub. It holds the foundation decisions about callable values, dispatch and
text. The rest of the area (type formation, aliases, generics and const generics, newtypes,
construction rights, Copy) migrates here in README §8, step 2.

## Rules

<a id="TYPE-1"></a>
### TYPE-1. A callable value is a function pointer with explicit context

**TYPE-1@1.** A value of function type is a pointer to a named function, and any context the callee needs is passed to it as an explicit argument, so the language has no closures.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Ty
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A closure captures part of its environment. Capturing a reference would store it,
which REF-3 forbids, and capturing an owned value would need its own ownership and conversion
rules. A function pointer plus an explicit context argument keeps both in view: the context is an
ordinary parameter, borrowed or owned under the ordinary rules, and its capabilities are visible
at the call site ([CALLABLE_VALUES_AND_CAPABILITIES.md](../language/CALLABLE_VALUES_AND_CAPABILITIES.md)).
VERIFIED_PIPELINE's first verified profile excludes closures for the same reason. The type
grammar (`Ty`) has function-pointer types and no closure type.

```con accept name=explicit-context exit=42
struct Ctx { add: Int }

fn apply(ctx: &Ctx, x: Int, f: fn(&Ctx, Int) -> Int) -> Int {
    return f(ctx, x);
}

fn plus(c: &Ctx, x: Int) -> Int {
    return c.add + x;
}

fn main() -> Int {
    let c: Ctx = Ctx { add: 40 };
    let r: Int = apply(&c, 2, plus);
    let Ctx { add } = c;
    return r + add * 0;
}
```

<a id="TYPE-2"></a>
### TYPE-2. No dynamic dispatch

**TYPE-2@1.** There are no trait-object types, so every method call through a trait bound resolves to one implementation at compile time, by monomorphization.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Ty, Concrete.monoProgram
- Semantics: none
- Obligation: the substitution and dispatch theorem of S8a (R-0510)
- Evidence: tested

**Rationale.** A call through a trait bound is specialized at each instantiation, so the code that
runs is known statically. Dynamic dispatch would add a vtable layout, an indirect-call rule and a
second dispatch semantics to the formal model. Indirect calls exist only through function-pointer
values (TYPE-1). The coherence rules for impls (TIGHTENING_DECISIONS D7) belong to the traits
chapter.

```con accept name=static-dispatch exit=42
trait Area {
    fn area(&self) -> Int;
}

struct Copy Sq { s: Int }

impl Area for Sq {
    fn area(&self) -> Int { return self.s * self.s; }
}

fn total<T: Area>(x: &T) -> Int {
    return x.area();
}

fn main() -> Int {
    let q: Sq = Sq { s: 6 };
    return total::<Sq>(&q) + 6;
}
```

<a id="TYPE-3"></a>
### TYPE-3. `Text` is owned

**TYPE-3@1.** A `Text` owns an immutable copy of validated UTF-8 bytes, never aliases another value's buffer, and is linear.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: std/src/text.con#Text
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** R-0483 (closed 2026-10-09) replaced the borrowed `Text` view with an owned value.
The old `Copy` pointer-and-length pair could outlive the buffer it pointed into, and its UTF-8
validation went stale when the source was mutated. Every constructor now copies, the fields are
private, and there is no `&mut` access, so the property established at construction holds for the
value's whole life. The borrowed form of a string is `&String`.
[STRING_TEXT_CONTRACT.md](../language/STRING_TEXT_CONTRACT.md) is the library contract.

```con accept name=outlives-its-source exit=42 project=text_owned interp-exempt=project-needs-std
mod text_owned {
    import std.string.{String};
    import std.text.{Text};
    fn main() with(Alloc) -> Int {
        let mut s: String = String::new();
        s.push_char(42);
        let t: Text = Text::from_string(&s);
        s.drop();
        let mut out: Int = 0;
        if let Option::Some { value } = t.byte(0) {
            out = value as Int;
        }
        t.drop();
        return out;
    }
}
```
