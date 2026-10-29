# Ownership

Concrete is **linear**: a non-Copy value is used exactly once and never silently disappears.
"Used" means moved, consumed, returned or handed off. There is no implicit destructor and no
catch-all discard. If a value is not Copy, the checker must see where it goes. Three concepts are
kept apart: **Copy** (may it be duplicated?), **linear**, meaning every non-Copy value (must it be
accounted for exactly once?), and **Destroy** (is there an explicit consuming cleanup verb?). A
permit or proof token owns nothing and has no `Destroy`, yet it is linear and must be handed off.
`&mut T` owns nothing either: it is exclusive, but letting it lapse discharges nothing and leaks
nothing (REF-10).

The design record, including why the language is linear rather than affine, is
[OWNERSHIP_MODEL.md](../language/OWNERSHIP_MODEL.md). The decided `defer` and exit-order
refinements (TIGHTENING_DECISIONS §4.5) and Copy being explicit (D6) are not yet implemented.
Every claim of "exactly once" here holds on every path that does not abort (STMT-2).

## Definitions

<a id="OWN-D1"></a>
### OWN-D1. Copy

**OWN-D1@2.** A type is Copy when it is a primitive, a shared reference, a raw pointer, a function pointer, an array of a Copy type, a newtype over a Copy type, a type parameter bounded by `Copy`, or an instantiation of a struct or enum declared `Copy` whose type arguments are all Copy, and every other type is linear.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.isCopyType
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Copy answers "may this be duplicated", not "may this be dropped". A generic
declared `Copy` is Copy exactly at the instantiations whose type arguments are Copy:
`Option<i32>` is, `Option<String>` is not. An instantiation that does not qualify is linear, not
an error. Version 2 (TIGHTENING_DECISIONS D6, option (a), 2026-10-23) decides this from the
declaration alone, as a derived `Copy` is: version 1 looked at the fields after substitution, so
a parameter no field mentions (`struct Copy P<T> { n: Int }`) made every instantiation Copy, and
Mono needed a fixpoint over the type graph. The declaration itself is checked once (OWN-14), and
TYPE-14 states the consequence.

## Rules

<a id="OWN-1"></a>
### OWN-1. A moved value is not used again

**OWN-1@2.** After a linear value is moved or consumed, the binding that held it is not used or borrowed again.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0205 (check), E0230 (check)
- Status: enforced
- Enforced by: Concrete.consumeVar, Concrete.checkSharedAccess, Concrete.checkStmt
- Semantics: none
- Obligation: no use after move in safe Core (R-0507)
- Evidence: tested

**Rationale.** Every site a value flows into moves it: `let g = f;`, a by-value argument,
`return f;`, a match scrutinee, an assignment `a = f;`, `break f;`, a struct, enum or array literal,
and a store `*p = v` or `a[i] = v`. Using the source afterwards would use a value that now has
another owner.

```con reject name=use-after-move code=E0205 stage=check pair=move-once
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn main() -> Int {
    let r: R = R { v: 21 };
    let a: Int = take(r);
    return a + take(r);
}
```

```con reject name=borrow-after-move code=E0230 stage=check pair=move-once
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn main() -> Int {
    let r: R = R { v: 21 };
    let a: Int = take(r);
    let mut out: Int = 0;
    borrow r as rr in Q {
        out = rr.v;
    }
    return a + out;
}
```

```con accept name=move-once exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn peek(r: &R) -> Int {
    return r.v;
}

fn main() -> Int {
    let r: R = R { v: 21 };
    let a: Int = peek(&r);
    return a + take(r);
}
```

<a id="OWN-2"></a>
### OWN-2. Every owned linear binding is consumed before every exit

**OWN-2@2.** Every owned linear binding, parameters and by-value `self` included, is consumed before every exit of its scope (falling off the end, `return`, `break` and `continue`), except where the end of the scope is unreachable.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0208 (check), E0210 (check), E0211 (check)
- Status: enforced
- Enforced by: Concrete.checkScopeExit, Concrete.checkBlockLocalsConsumed, Concrete.checkReturnPathConsumed, Concrete.checkStmt
- Semantics: none
- Obligation: linear conservation at reachable exits (R-0507; VERIFIED_PIPELINE, "Resources and failure")
- Evidence: tested

**Rationale.** Parameters are owned locals: the caller moved the value in, so the callee holds
the obligation. There is no terminal parameter sink, so `fn drop_it(f: File) {}` is refused. A
scope whose end is unreachable, such as an infinite `while true` with no `break` or a call to
`abort()`, need not consume its locals, because no exit follows. A pending `defer` counts as
consumption (OWN-12).

```con reject name=never-consumed code=E0208 stage=check pair=consumed-on-every-exit
struct R { v: Int }

fn main() -> Int {
    let r: R = R { v: 42 };
    return 0;
}
```

```con reject name=early-return-leaks code=E0208 stage=check pair=consumed-on-every-exit
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn f(c: bool) -> Int {
    let r: R = R { v: 42 };
    if c {
        return 0;
    }
    return take(r);
}

fn main() -> Int {
    return f(false);
}
```

```con accept name=consumed-on-every-exit exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn f(c: bool) -> Int {
    let r: R = R { v: 42 };
    if c {
        return take(r) * 0;
    }
    return take(r);
}

fn main() -> Int {
    return f(false);
}
```

<a id="OWN-3"></a>
### OWN-3. Branches agree on what they consume

**OWN-3@2.** Every path of an `if` or `match` that reaches the point after it has consumed the same linear values, while a path that diverges or leaves the function is exempt from that agreement.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0209 (check), E0212 (check), E0213 (check)
- Status: enforced
- Enforced by: Concrete.mergeVarStates, Concrete.checkNoBranchConsumption, Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation at reachable exits (R-0507)
- Evidence: tested

**Rationale.** After a merge a value cannot be "maybe consumed". A branch that returns, breaks
out of the function or aborts never reaches the merge, so
`Some { v } => use(v), None => { drop(x); return 1; }` is sound. If every arm diverges, the code
after the match is unreachable.

```con reject name=one-branch-consumes code=E0212 stage=check pair=both-branches-consume
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn main() -> Int {
    let r: R = R { v: 42 };
    let mut out: Int = 0;
    if out == 0 {
        out = take(r);
    } else {
        out = 1;
    }
    return out;
}
```

```con accept name=both-branches-consume exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn main() -> Int {
    let r: R = R { v: 42 };
    let mut out: Int = 0;
    if out == 0 {
        out = take(r);
    } else {
        out = take(r) * 0 + 1;
    }
    return out;
}
```

<a id="OWN-4"></a>
### OWN-4. A loop body does not consume an outer value

**OWN-4@2.** A loop body does not consume a linear value declared outside the loop, except on a path that leaves the function or the loop before another iteration can run, or in an assignment that consumes a `mut` binding and gives it a new value in the same statement (`acc = f(acc)`).

- Kind: refusal
- Uses: OWN-D1
- Codes: E0207 (check)
- Status: enforced
- Enforced by: Concrete.consumeVar
- Semantics: none
- Obligation: linear conservation at reachable exits (R-0507)
- Evidence: tested

**Rationale.** The next iteration would see a moved value.
`while c { if bad { s.drop(); return 1; } }` is sound, because no iteration follows the return.
A loop nested inside such a branch resets the exemption, because it iterates before the return.
A consequence for APIs: a generic function that returns one of two linear arguments always leaks
the other, so such helpers require `T: Copy`.

```con reject name=consume-outer-in-loop code=E0207 stage=check pair=consume-after-loop
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn main() -> Int {
    let r: R = R { v: 42 };
    let mut out: Int = 0;
    let mut i: Int = 0;
    while i < 2 {
        out = take(r);
        i = i + 1;
    }
    return out;
}
```

```con accept name=consume-after-loop exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn main() -> Int {
    let r: R = R { v: 40 };
    let mut out: Int = 0;
    let mut i: Int = 0;
    while i < 2 {
        out = out + 1;
        i = i + 1;
    }
    return out + take(r);
}
```

<a id="OWN-5"></a>
### OWN-5. A linear value is never discarded

**OWN-5@2.** A linear value is never discarded: `_` ignores only Copy values, `let _` does not exist, a statement expression does not drop a linear value, and `discard(e)` accepts only a Copy value.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0287 (check), E0288 (check), E0289 (check), E0295 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** All the "may this disappear?" checks are keyed on Copy, never on whether the value
owns a resource. A linear value is accounted for: it is consumed, passed on, returned, destroyed
with `destroy(x)`, or destructured exhaustively with its parts handed off. `destroy(x)` runs a
linear value's destructor, and `discard(x)` acknowledges that a trivial Copy value is dropped.
Over a non-Copy enum, a `let ... else`, `if let` or `while let` that has no arm for the other
variants is a hidden `_` and is refused the same way. Write an exhaustive `match` instead. A
variant pattern written without a field list ignores every field, so over an owned scrutinee it
is refused when a field is not Copy (bug 197).

`_` can never silence a resource: a resource-bearing payload is non-Copy, so `Ok { _ }` over it is
E0288, and the payload is bound and released (`Result::Ok { f } => { f.destroy(); }`). For data
(`Result` or `Option` over Copy payloads) an exhaustive match with `_` payloads is a genuine,
auditable drop. `discard` with a non-Copy argument is E0295 (`CheckError.discardNonCopy`): a
resource is consumed or `destroy`ed, never discarded, since dropping it would leak it.

```con reject name=wildcard-drops-linear code=E0288 stage=check pair=account-for-it
struct R { v: Int }

fn main() -> Int {
    let o: Option<R> = Option::Some { value: R { v: 42 } };
    match o {
        _ => {}
    }
    return 0;
}
```

```con reject name=statement-drops-linear code=E0287 stage=check pair=account-for-it
struct R { v: Int }

fn mk() -> R {
    return R { v: 42 };
}

fn main() -> Int {
    mk();
    return 0;
}
```

```con reject name=bare-variant-drops-payload code=E0288 stage=check pair=account-for-it
struct R { v: Int }
enum E { A { r: R }, B }

fn main() -> Int {
    let e: E = E::A { r: R { v: 42 } };
    match e {
        E::A => { return 1; },
        E::B => { return 0; }
    }
}
```

```con reject name=discard-linear code=E0295 stage=check pair=account-for-it
struct R { v: Int }

fn main() -> Int {
    let r: R = R { v: 42 };
    discard(r);
    return 0;
}
```

```con accept name=account-for-it exit=42
struct R { v: Int }

fn main() -> Int {
    let o: Option<R> = Option::Some { value: R { v: 42 } };
    let k: Int = 3;
    discard(k);
    let mut out: Int = 0;
    match o {
        Option::Some { value } => {
            let R { v } = value;
            out = v;
        },
        Option::None => {}
    }
    return out;
}
```

<a id="OWN-6"></a>
### OWN-6. A fallible result is used

**OWN-6@1.** A statement expression does not discard a value of type `Result` or `Option`.

- Kind: refusal
- Uses: none
- Codes: E0286 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** `Result` and `Option` exist to make the caller consider failure or absence.
Dropping one on the floor throws that away, even when the payload is Copy. The value is used by
matching it, propagating it or binding it (OWN-2 then requires a linear binding to be consumed). A
trailing value expression with no `;` is the block's value, not a discard. User enums are not
must-use.

A statement expression ending in `;` discards its value, and discarding a `Result` or `Option`
throws away a possible failure or absence. The linear checker already catches a named value that
is never consumed (E0208); this rule closes the gap for an unnamed temporary a call produces and
drops. Only these two types are must-use, however spelled (bare or with type arguments); an
`i32`, a `bool` or a `()` is not flagged, and user enums are not must-use (a workload that needs
one would widen `mustUseEnumName?` in `Concrete/Check/Check.lean`). Handling the value (`match
risky() { ... }`), propagating it by matching and returning the error arm, binding it (`let r =
risky();`, after which OWN-2 requires `r` be used) and a trailing value expression (the block's
value, not a discard) never trip E0286. The deliberate acknowledgement is an exhaustive match that
ignores only Copy payloads (`Option::Some { _ } => {}`): a catch-all `_` arm over the non-Copy value
is E0288 and `let _ = e;` is E0289 (OWN-5). The future error-set report (`--report error-sets`,
ROADMAP old Phase 6 #14) must keep these diagnostics firing. Site: `mustUseEnumName?` and the
`Stmt.expr` discard check of `checkStmt` (`CheckError.discardedMustUse`); fixtures in
`tests/programs/ignored_result/`, gate `scripts/tests/check_ignored_result.sh`.

```con reject name=ignored-option code=E0286 stage=check pair=handled-option
fn find(k: Int) -> Option<Int> {
    if k > 0 {
        return Option::Some { value: k };
    }
    return Option::None;
}

fn main() -> Int {
    find(42);
    return 0;
}
```

```con accept name=handled-option exit=42
fn find(k: Int) -> Option<Int> {
    if k > 0 {
        return Option::Some { value: k };
    }
    return Option::None;
}

fn main() -> Int {
    let mut out: Int = 0;
    match find(42) {
        Option::Some { value } => { out = value; },
        Option::None => {}
    }
    return out;
}
```

<a id="OWN-7"></a>
### OWN-7. A pure value is not computed and thrown away

**OWN-7@2.** A statement expression does not discard a pure, trap-free, non-Unit Copy value, that is, one built only from literals, variable and field reads, and arithmetic, comparison, logical, bitwise and shift operators over them.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0294 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** `2 + 3;` or `x;` is a dead computation, usually a mistake. Calls are not covered:
an empty capability set does not make a call pure, because mutation through `&mut` parameters
and foreign effects are not capabilities. Division and remainder are not covered either, because
their zero check is a real runtime trap. An intentional discard is written `discard(e)`.

This rule keys on effect-freedom as OWN-6 keys on fallibility (CapabilityJudgment slice 5). Calls
are never flagged: the capability model tracks no mutation through `&mut` parameters, `trusted` or
`extern` code, so an empty capability set does not prove a call pure (`env_assign(&mut e, ...)`,
`fclose(fp)`), and a discarded call result may be deliberate. Operations that can trap (`/`, `%`)
are not flagged either. `discard(e)` acknowledges an intentional discard of a Copy value. Site:
`exprPureDiscardable`, a local structural purity predicate, in the same `Stmt.expr` check
(`CheckError.discardedPureValue`).

```con reject name=dead-computation code=E0294 stage=check pair=used-computation
fn main() -> Int {
    let a: Int = 40;
    a + 2;
    return a;
}
```

```con accept name=used-computation exit=42
fn main() -> Int {
    let a: Int = 40;
    discard(a + 1);
    return a + 2;
}
```

<a id="OWN-8"></a>
### OWN-8. A non-Copy part is not read out of a place

**OWN-8@2.** A non-Copy field, array element or heap interior is never read out of its place by value, so it is borrowed in place or its whole owner is destructured.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0290 (check)
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** The place would still own the value it was read from, so the read would duplicate
it. Projection bases, borrow targets, assignment targets and auto-borrowed receivers are
places, not reads, so `w.f.g`, `a[i] = v` and `&self` method calls stay legal. A heap node is
taken apart by moving it out of its cell (`let n = *h;`, which frees the cell) and destructuring
it.

```con reject name=field-read-by-value code=E0290 stage=check pair=destructure-owner
struct R { v: Int }
struct W { r: R }

fn main() -> Int {
    let w: W = W { r: R { v: 42 } };
    let g: R = w.r;
    let R { v } = g;
    return v;
}
```

```con accept name=destructure-owner exit=42
struct R { v: Int }
struct W { r: R }

fn main() -> Int {
    let w: W = W { r: R { v: 42 } };
    let W { r } = w;
    let R { v } = r;
    return v;
}
```

<a id="OWN-9"></a>
### OWN-9. An assignment never overwrites a live linear value

**OWN-9@2.** An assignment never overwrites a live linear value, whether it is in a binding, a field, an array element or a place behind `&mut`, and a `mut` binding of linear type may be assigned again once its old value has been consumed.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0219 (check), E0291 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** Overwriting would leak the old value. The accumulate pattern `acc = f(acc, x)` is
legal, because the right-hand side consumed the old value first. Stores through a `*mut T` raw
pointer are exempt: trusted collection code writes uninitialized slots. The order in which an
assignment evaluates its parts, and the operations that return the displaced value, belong to
the statements chapter.

```con reject name=overwrite-live code=E0219 stage=check pair=rebind-after-consume
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn main() -> Int {
    let mut r: R = R { v: 1 };
    r = R { v: 42 };
    return take(r);
}
```

```con reject name=overwrite-through-mut-ref code=E0291 stage=check pair=rebind-after-consume
struct R { v: Int }

fn replace(slot: &mut R, v: R) {
    *slot = v;
}

fn main() -> Int {
    return 0;
}
```

```con accept name=rebind-after-consume exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn bump(r: R) -> R {
    let R { v } = r;
    return R { v: v + 41 };
}

fn main() -> Int {
    let mut r: R = R { v: 1 };
    r = bump(r);
    return take(r);
}
```

<a id="OWN-10"></a>
### OWN-10. Only a `mut` binding is assigned or mutably borrowed

**OWN-10@1.** A binding is assigned, or borrowed with `&mut`, only when it is declared `mut`.

- Kind: refusal
- Uses: none
- Codes: E0217 (check), E0233 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.checkMutAccess
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Mutation is visible at the declaration. A `&mut` parameter is already a mutable
place, so writing through it needs no `mut` on the parameter.

```con reject name=assign-immutable code=E0217 stage=check pair=assign-mut
fn main() -> Int {
    let x: Int = 1;
    x = 42;
    return x;
}
```

```con reject name=mut-borrow-immutable code=E0233 stage=check pair=assign-mut
fn set(r: &mut Int) {
    *r = 42;
}

fn main() -> Int {
    let x: Int = 1;
    set(&mut x);
    return x;
}
```

```con accept name=assign-mut exit=42
fn set(r: &mut Int) {
    *r = 41;
}

fn main() -> Int {
    let mut x: Int = 1;
    set(&mut x);
    x = x + 1;
    return x;
}
```

<a id="OWN-11"></a>
### OWN-11. A move happens only where its code always runs

**OWN-11@1.** Nothing is moved by code that may not run when its effect is relied on: a proof-only statement (`assert`, `assume` or `ghost let`) changes the ownership state of no runtime binding, and the right operand of `&&` or `||` moves no value.

- Kind: refusal
- Uses: none
- Codes: E0243 (check), E0244 (check)
- Status: enforced
- Enforced by: Concrete.refuseErasedConsumption, Concrete.refuseConditionalConsumption
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** Proof-only code is erased before Core, so a move there would happen in the
checker's model and not in the binary (bug 147). The right operand of `&&` or `||` runs only
sometimes, so a move there would happen on only some paths (bug 148). A guard of a match arm is
read-only for the same reason; that rule (E0300) belongs to the patterns chapter. Ghost bindings exist only
in proof code, so proof code may consume them.

```con reject name=move-in-short-circuit code=E0244 stage=check pair=move-before-condition
struct R { v: Int }

fn take(r: R) -> bool {
    let R { v } = r;
    return v > 0;
}

fn main() -> Int {
    let r: R = R { v: 42 };
    let c: bool = true;
    if c && take(r) {
        return 42;
    }
    return 0;
}
```

```con accept name=move-before-condition exit=42
struct R { v: Int }

fn take(r: R) -> bool {
    let R { v } = r;
    return v > 0;
}

fn main() -> Int {
    let r: R = R { v: 42 };
    let t: bool = take(r);
    let c: bool = true;
    if c && t {
        return 42;
    }
    return 0;
}
```

<a id="OWN-12"></a>
### OWN-12. `defer` reserves its arguments until the scope exits

**OWN-12@1.** A `defer` registers a function call whose arguments are local names, borrows of local names or constants, the call runs when its block exits on any path that does not abort, the block's deferred calls run in reverse order of registration, and until then a local passed by value is reserved and a local passed by borrow is pinned, so neither may be moved or consumed.

- Kind: refusal
- Uses: none
- Codes: E0206 (check), E0279 (check), E0282 (check)
- Status: enforced
- Enforced by: Concrete.consumeVar, Concrete.checkStmt
- Semantics: none
- Obligation: deferred calls run exactly once on every exit that does not abort (R-0496, R-0501)
- Evidence: tested

**Rationale.** A deferred call reads its argument places at the exit, not at registration: a
Copy argument sees the value the local holds then. Arguments are bound by binding identity, so a
later `let` of the same name is a different binding. A reserved local may still be read,
borrowed and mutated in place. The deferred call discharges its obligation, and a later use of it
is a use after move (OWN-1). A pinned local must still be consumed, so a deferred observer goes
after the deferred consumer in source (`defer kill(t); defer peek(&t);` runs `peek` first). The
body is a function call. A method call, a block, a projection or a nested call is refused,
because it would be evaluated at the exit. The decided refinements are pending: loans retained by
a deferred call stay live until it runs, and the exit order (the exit value first, then each
scope's deferred calls innermost first, then its loans end) is TIGHTENING_DECISIONS §4.5.

`defer <call>;` schedules the call for the exit of the enclosing block: within a block the calls
run last registered first (`defer a(); defer b();` runs `b`, then `a`), on fall-through, on an early
`return` (an explicit `Result::Err` return included) and on a `break` or `continue` out of the block,
and each block runs its own list, inner blocks before outer ones (`Concrete/IR/Lower.lean`:
`emitFrameDeferredCalls` at a scope's end, `emitAllDeferredCalls` on `return`,
`emitDeferredUntilLoop` on `break` and `continue`). An argument is a local name, a shared or
exclusive borrow of a local name, or a constant (a literal, a module constant, a function name); a
projection (`defer f(t.id)`, `defer f(&t.field)`), a nested call (`defer f(g(x))`) or a string literal
is E0279, so write a named cleanup function that takes the local or a borrow of it and computes
inside (decided 2026-10-21, bug 143). A by-value local is reserved, not taken: E0206 ("reserved by
defer") refuses a later move or consumption (`error_defer_move.con`,
`error_memory_edge_defer_then_move.con`, `pressure_err_defer_then_move.con`), and when the block ends
the reserved locals are consumed (a later use is E0205) and the pins end. A borrowed local is
pinned (E0206, "borrowed by a pending defer"), so `defer peek(&t); defer kill(t);` is refused and
`defer peek(&t);` alone leaves `t` unconsumed (E0208). A registration-time value is spelled
`let x0 = x; defer show(x0);`; `let x = 1; defer show(x); let x = 5;` shows 1, natively and under
`--interp`, because Elab gives a shadowing binder a fresh Core name (bug 087). A method call
`defer obj.method();` is E0282 (`deferBodyNotCall`; an earlier design note wrongly allowed it,
EVALUATION2 D7): defer the function the method desugars to, or a named cleanup function; `defer
{ ... }` is a parse error. A deferred call's arguments are typed by its parameters as in an ordinary
call (`defer f(1);` passes an `i32` to an `i32` parameter), but a string literal is not a deferred
argument (E0279), so `defer println("x")` is refused where `println("x")` compiles: defer a cleanup
function instead (an earlier design note blamed a missing argument coercion). What a deferred call that fails or diverges does on a return or error path
is open; whether an abort runs deferred calls is owner decision OD10 (STMT-2 is pending), and today
a deferred call is an ordinary call with no recovery rule. There is no implicit scope-end drop to order
against `defer` (OWN-13): `destroy(x)`, `x.drop()` or a deferred explicit call consumes a value.
The surface is pinned by `scripts/tests/check_defer.sh` and `examples/defer/cleanup_order/`
(ROADMAP old Phase 6 #7, 2026-06-23).

```con reject name=move-reserved code=E0206 stage=check pair=defer-consumes
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn release(r: R) {
    let R { v } = r;
}

fn main() -> Int {
    let r: R = R { v: 42 };
    defer release(r);
    return take(r);
}
```

```con reject name=defer-projection code=E0279 stage=check pair=defer-consumes
struct Copy P { n: Int }

fn show(n: Int) {
}

fn main() -> Int {
    let p: P = P { n: 42 };
    defer show(p.n);
    return 0;
}
```

```con accept name=defer-consumes exit=42
struct R { v: Int }

fn release(r: R) {
    let R { v } = r;
}

fn peek(r: &R) -> Int {
    return r.v;
}

fn main() -> Int {
    let r: R = R { v: 42 };
    defer release(r);
    return peek(&r);
}
```

<a id="OWN-13"></a>
### OWN-13. Nothing is destroyed implicitly

**OWN-13@2.** No value is destroyed implicitly: a linear value is destroyed only by an explicit consuming call (`destroy(x)`, a consuming method or a deferred one), no type is both Copy and `Destroy`, and a `Destroy` implementation requires no capability beyond `Alloc`.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0570 (core-check), E0584 (core-check), E0272 (check), E0273 (check)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls, Concrete.checkExprCore
- Semantics: none
- Obligation: ownership instrumentation with a proved erasure theorem (R-0496, R-0507)
- Evidence: tested

**Rationale.** Rust is affine with an implicit `Drop`. Concrete makes cleanup explicit, so it
refuses silent disappearance (OWN-5) rather than inserting a destructor. Drop glue does not
change this: when source code explicitly consumes an owner, that operation may destroy the owner's
fields or elements recursively, but the decision stays visible in the source. A Copy value can be
duplicated, so it cannot also have a destructor that must run exactly once.

```con reject name=copy-and-destroy code=E0570 stage=core-check pair=explicit-destroy
struct Copy C { v: Int }

impl Destroy for C {
    fn destroy(&self) {
    }
}

fn main() -> Int {
    return 0;
}
```

```con accept name=explicit-destroy exit=42
struct R { v: Int }

impl Destroy for R {
    fn destroy(&self) {
    }
}

fn main() -> Int {
    let r: R = R { v: 42 };
    let out: Int = r.v;
    destroy(r);
    return out;
}
```

<a id="OWN-14"></a>
### OWN-14. A `Copy` declaration has only Copy fields

**OWN-14@2.** A struct or enum declared `Copy` has only fields and payloads that are Copy when its type parameters are taken as Copy, so no Copy value ever contains a linear one.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0571 (core-check, post-mono)
- Status: enforced
- Enforced by: Concrete.ccCheckModuleDecls
- Semantics: none
- Obligation: Copy is a fact of the declaration (TIGHTENING_DECISIONS D6, pending)
- Evidence: tested

**Rationale.** A Copy value is duplicated freely, so a linear field inside it would be duplicated
with it. A generic declaration is checked once, with its type parameters taken as Copy, so a
field that is linear whatever the arguments (`String`) is refused at the declaration (version 2;
version 1 checked each instantiation and made a non-qualifying one linear).

```con reject name=copy-with-linear-field code=E0571 stage=core-check pair=copy-with-copy-fields
struct S { x: Int }
struct Copy T { s: S }

fn main() -> Int {
    return 0;
}
```

```con accept name=copy-with-copy-fields exit=42
struct Copy S { x: Int }
struct Copy T { s: S }

fn main() -> Int {
    let t: T = T { s: S { x: 21 } };
    let u: T = t;
    return t.s.x + u.s.x;
}
```

```con reject name=generic-with-linear-field code=E0571 stage=core-check pair=copy-with-copy-fields
struct Copy S<T> { a: T, s: String }

fn main() -> Int {
    return 0;
}
```

<a id="OWN-15"></a>
### OWN-15. Every surface form has one value-flow behaviour

**OWN-15@2.** Every surface expression, match-arm and statement constructor creates, moves, borrows, copies, overwrites or refuses its operands as its row in this entry's value-flow tables states.

- Kind: guarantee
- Uses: OWN-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkExprCore, Concrete.checkStmt
- Semantics: none
- Obligation: one value-flow rule per Core constructor in the Core checker (R-0499)
- Evidence: tested

**Rationale.** Value flow used to be spread over many syntax handlers, and new forms kept
landing without a conservation story (`break (value)` had no row for weeks). Now
`scripts/tests/check_value_flow_spec.sh` reads the constructors from `Concrete/Frontend/AST.lean`
and fails until each one has a row here, and its gate name, where there is one, must have a
fixture. Since the 2026-07-06 refactor, `checkExpr` decides consumption centrally by mode.
`callArg` is confined to argument checking, and the gate pins its use-site count.

The tables were introduced under ROADMAP 13c, the prevention program after the H13 – H17 sweep,
which named three failure classes, each with its gate: **discard** (a value vanishes,
`check_linear_discard.sh`), **conservation** (a value duplicates or aliases,
`check_linear_conservation.sh`) and **scope** (a value falls out of nested control flow,
`check_linear_nested_scope.sh`). The cross-cutting ruling H17 (2026-07-05) made parameters owned
locals, by-value `self` and `Destroy` bodies included.

```con accept name=flows-move-once exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

struct Two { a: R, b: R }

fn main() -> Int {
    let a: R = R { v: 20 };
    let b: R = R { v: 22 };
    let two: Two = Two { a: a, b: b };
    let Two { a: x, b: y } = two;
    return take(x) + take(y);
}
```

#### Value flow: Expr

| constructor | value flow | gate / errors |
|---|---|---|
| `intLit` `floatLit` `boolLit` `charLit` | creates a Copy value | — |
| `strLit` | **creates an owned String** (linear), which must be consumed | discard (E0208 via binding rules) |
| `ident` | in value position **moves** a non-Copy binding (consumed at the use site); a Copy one is copied | conservation (E0205 reuse after move) |
| `binOp` | reads Copy operands (non-Copy operands are type errors); `&&` and `\|\|` short-circuit control flow only (OWN-11) | — |
| `unaryOp` | reads a Copy operand | — |
| `call` | each ident argument **moves**; the result is an owned value the caller must account for | conservation (E0205); discard (E0286 / E0287 on a `;` discard) |
| `paren` | transparent: propagates the position (including place-ness) to the inner expression | — |
| `structLit` | each field expression **moves** in; `..base` may copy only Copy fields (otherwise **E0220**); creates an owned value | conservation |
| `fieldAccess` | place projection: a Copy field is copied, a non-Copy field read by value is **refused** (**E0290**, OWN-8); as a projection base, borrow target or assignment target it is a place (no flow); a newtype `.0` on an ident is a whole-owner **move** | conservation (H11 rows) |
| `enumLit` | payload expressions **move** in; creates an owned value | conservation |
| `match_` | an ident scrutinee **moves**; matching through `&` / `&mut` borrows payloads (REF-9); arms must agree on consumption (E0209, OWN-3) | conservation + discard (`_` rules E0288) |
| `borrow` | **borrows** the place (no move); the result `&T` is Copy | conservation (accept rows) |
| `borrowMut` | **exclusive borrow**; the result `&mut T` is not Copy but carries no consume obligation | conservation |
| `deref` | `*ref` reads (a Copy pointee is copied); `*heap_ident` **consumes** the `Heap` (load and free); `*rawptr` is the trusted idiom | conservation |
| `arrayLit` | **moves** each element in (H10) | conservation (E0205 rows) |
| `arrayIndex` | place projection: a Copy element is copied, a non-Copy element read by value is **refused** (**E0290**, OWN-8) | conservation (H11 rows) |
| `cast` | reads a Copy numeric | — |
| `methodCall` | the receiver is a place, auto-borrowed for `&self` / `&mut self`; by-value `self` **moves** an ident receiver and **refuses** a projection receiver (**E0290**); ident arguments **move** | conservation (H11 rows) |
| `staticMethodCall` | ident arguments **move**; the result is owned | conservation |
| `fnRef` | creates a Copy function-pointer value | — |
| heap-shell `.fieldAccess` (the removed `->` form) | heap-interior read: OWN-8 applies (bug 144), so a non-Copy field read by value is **E0290**; a heap store through `fieldAssign` refuses a non-Copy overwrite (**E0219**); a node is taken apart with `let n = *h;` and a pattern | projection |
| `allocCall` | the inner call or allocator is evaluated; creates an owned `Heap<T>` | discard |
| `whileExpr` | a loop as an expression; `break value;` **moves** the value out as the loop's result (H14) | conservation (H14 rows) |
| `ifExpr` | branch values **move**; branches must agree on consumption (OWN-3) | conservation |

#### Value flow: MatchArm

| constructor | value flow | gate / errors |
|---|---|---|
| `mk` (enum arm) | bindings **own** the payloads of an owned scrutinee (they must be consumed, E0208) and **borrow** them through a reference scrutinee; a `_` payload may ignore only a Copy value (**E0288**); guards run before the arm commits and are read-only: a guard that moves a non-Copy value is **refused** (**E0300**, bug 090) | discard + scope |
| `litArm` | no bindings; a literal compare on a Copy scrutinee | — |
| `varArm` | the binding **owns** the whole scrutinee value (**moves** it; it must be consumed) | discard (E0205 / E0208 rows) |
| `rangeArm` | a Copy numeric compare; no bindings | — |

#### Value flow: Stmt

| constructor | value flow | gate / errors |
|---|---|---|
| `letDecl` | **moves** an ident right-hand side (H9); reusing the name of any visible binding is **refused** (**E0301**, SCOPE-1); `let _` is removed (**E0289**) | conservation + discard |
| `assign` | the old value must already be consumed (otherwise **E0219**, OWN-9); an ident right-hand side **moves** (H13) | conservation (H13 rows) |
| `return_` | an ident value **moves**; every return path must leave no unconsumed linear value (OWN-2) | conservation + scope |
| `expr` | `e;` (not a value) **discards**: a fallible result is **refused** (**E0286**, whether or not it is Copy), a non-Copy value is **refused** (**E0287**); a trailing `e` (a value) is the block's value, never a discard; `free(...)` is itself the consumption when its pointee is Copy (a non-Copy pointee would be dropped: **E0287**, bug 144) | discard |
| `ifElse` | branches must agree on consumption of outer linear values (E0209 / E0212, OWN-3); branch locals are checked at branch exit (H9) | scope |
| `while_` | the body may not consume an outer linear value (**E0207**, OWN-4) except in a branch that exits the function, a rebind, or a move next to a `break` (the H14 one-level exemption) | scope |
| `forLoop` | the same rules as `while_` | scope |
| `fieldAssign` | an overwrite of a non-Copy field is **refused** (**E0219**); an ident right-hand side **moves**; the object is a place | conservation |
| `derefAssign` | through `&mut T`: an overwrite of a non-Copy pointee is **refused** (**E0291**, H15); through `*mut T`: a trusted uninitialized-slot store (exempt); an ident right-hand side **moves** | conservation (H15 rows) |
| `arrayIndexAssign` | an overwrite of a non-Copy element is **refused** (**E0291**, H15); an ident right-hand side **moves**; the array is a place | conservation (H15 rows) |
| `break_` | may not skip unconsumed loop-local linear values (E0210); `break f;` **moves** an ident value out (H14), with a one-level loop-depth exemption (a break fires at most once per loop entry) | conservation (H14 rows) + scope |
| `continue_` | may not skip unconsumed loop-local linear values (E0211) | scope |
| `defer` | the deferred call **reserves** its operands, consumed at scope exit (OWN-12); a discarded non-Copy or fallible result is refused (E0287 / E0286) | discard |
| `assert_` | proof-only, erased before Core; must not change the ownership state of a runtime binding (OWN-11) | — |
| `assume_` | proof-only, erased before Core; must not change the ownership state of a runtime binding (OWN-11) | — |
| `borrowIn` | a borrow block: the source is frozen for the block; the reference is scoped and may not escape (REF-6, REF-8) | conservation (escape rows) |
| `letDestructure` | enum destructure or let-else: **consumes** the source; bindings **own** payloads; a let-else over a non-Copy enum is **refused** (**E0288**: the no-match path would discard it) | conservation + discard |
| `letStructDestructure` | a whole-owner **move**: the source is consumed, all fields must be bound (a partial pattern is **E0252**), and each binding is owned (it must be consumed) | conservation |

#### Expression modes

The checker decides consumption by the position an expression is checked in (`UseMode` of
`checkExpr`). Every `Expr` constructor has a row.

| constructor | value | callArg | place |
|---|---|---|---|
| `intLit` `floatLit` `boolLit` `strLit` `charLit` | creates | same as value | n/a (not addressable) |
| `ident` | **moves** non-Copy (auto-consume; frozen check for all) | reads; the call site consumes according to the parameter type | reads; no consume |
| `binOp` `unaryOp` `cast` | reads Copy operands (a cast operand is checked as a place, so an illegal cast reports "cannot cast") | same | n/a |
| `call` `methodCall` `staticMethodCall` | the result is an owned value; arguments are checked in callArg | n/a (a call result is a value) | n/a |
| `paren` | transparent: propagates the surrounding mode | transparent | transparent |
| `structLit` `enumLit` `arrayLit` | fields and elements are checked in value mode (moved in); `..base` is checked as a place | same | n/a |
| `fieldAccess` `arrayIndex` | a Copy read copies; a non-Copy read is refused (E0290); the base is checked as a place | same as value | sub-place; no flow |
| `match_` `ifExpr` `whileExpr` | scrutinee and arm values in value mode; branch environments merged | n/a | n/a |
| `borrow` `borrowMut` | the inner expression is checked as a place; the result `&T` is Copy and `&mut T` exclusive | same | n/a |
| `deref` | the inner expression is checked as a place (reading through a reference or pointer never consumes the binding); `*heap_ident` explicitly consumes | same | inner as place |
| `fnRef` | creates a Copy function pointer | same | n/a |
| `allocCall` | the inner call or allocator is checked; an owned `Heap` result | n/a | n/a |
