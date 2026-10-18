# Bug 087 — a block-scoped `let` that shadowed an outer name was not lexically scoped

**Status:** FIXED (branch `bug087-let-shadowing`)
**Found:** 2026-10-18 by the model/scope hunt (`hunt-model/scope`); the memory-safety
flavour by the checker hunt (`hunt-checker/p17`).
**Severity:** silent wrong code, a heap use-after-free in safe code, and a proof model that
differed from the program. The interpreter, the compiled binary and the extracted proof model
each resolved a shadowed name differently, and none of them was always lexical.

## Repro

```text
fn shadow_if(a: i32) -> i32 {             // a = 5
    let x: i32 = 1;
    if a > 0 { let x: i32 = 100; let y: i32 = x; }
    return x;                             // lexical: 1
}
fn shadow_param(a: i32) -> i32 {          // a = 3
    if a > 0 { let a: i32 = 7; }
    return a;                             // lexical: 3
}
fn shadow_loop(a: i32) -> i32 {
    let mut s: i32 = 0;
    let k: i32 = 5;
    for (let mut i: i32 = 0; i < 2; i = i + 1) { let k: i32 = 1000; s = s + k; }
    return s + k;                         // lexical: 2005
}
```

| function       | interp | binary | model (`--report extraction`)          |
|----------------|--------|--------|----------------------------------------|
| `shadow_if`    | 1      | 100    | `then let x = 100; x else x` → 100     |
| `shadow_param` | 3      | 7      | `then let a = 7; a else a` → 7         |
| `shadow_loop`  | 3000   | 3000   | 2005                                   |

The memory-safety shape (p17):

```text
borrow base as r in R {
    if true { let r: &String = &s; ... }
    s.drop();
    <allocate a 40-byte 'Z' string>
    show(r);          // checker: the borrow of `base`; binary: the dangling inner `r`
}
```

The compiled program printed the reused buffer (`ZZZZ…` or garbage) — a use-after-free that
the source checker had accepted as safe, because the checker's view of `r` (the borrow of
`base`) was the lexical one and the code's was not.

## Mechanism

Bug 045 alpha-renamed match payload binders in Elab (`value` → `value.bN`) so Core names are
unique. Every other block binder — `let` in an `if`/`else` branch, a value-`if` branch, a
`while`/`for` body, a `for` header, a borrow block's ref — kept its surface spelling, and Elab
never closed those blocks: an inner binding stayed in `vars` (and in the evidence binder frame)
after its block ended. Below Elab every table is function-flat and keyed by name:

- **Lower** gave both `x`s one slot, so the read after the `if` saw whichever store ran last
  (100). In p17 that was the dangling `&s`.
- **Interp** scopes branch environments but not loop bodies, so it was right for `if` and
  wrong for the loop (the body's `k = 1000` overwrote the outer `k`).
- **Extraction** turns a statement list into a continuation-passing `PExpr`, duplicating the
  rest of the function into each branch; the branch's `let x` then scoped over the outer
  `return x`.
- **Evidence** resolved the outer `x` after the block to the inner binder (within one frame
  the last same-named binder wins).

## Fixed

- **Elab (`bindLocal`, `enterBlock`/`exitBlock`).** A binding that shadows a visible name (a
  parameter, an outer `let`, an earlier `let` in the same block, a match binder) gets a fresh
  Core name `x.bN`, the same scheme as bug 045. Bindings that do not shadow keep their spelling,
  so a program that never shadows produces the same Core as before. Every block (`if`/`else`,
  value-`if`, `while` body, `for` header and body, borrow block) restores the visible
  variables, the surface→Core renames and the ghost names when it closes, so after the block
  the outer binder is the visible one again. Borrow blocks emit the Core name of the borrowed
  variable, and indirect calls emit the callee local's Core name.
- **Evidence frames.** A block's binders keep their positions in the enclosing frame (the
  encoding every shipped `v2:` fingerprint was taken over) but become unresolvable when the
  block closes, replaced by a tombstone no identifier can equal. Opening a new frame per block
  would have moved the digest of every body with a branch-local `let` without changing what
  any reference resolves to. Measured: no shipped fingerprint changed (`elf_header`'s
  `validate_header` went stale under the new-frame variant and is current under this one).
- **Normalizer (`alphaCanon`).** With unique names, the extracted model is lexical. But the
  drift and digest normalizer stripped every `.bN` suffix blindly, and stripping is a renaming
  that is sound only when it is injective. On the continuation-duplicated form it was not:
  `then let x.b0 = 100; x` became `let x = 100; x` again, and in the loop the stripped body
  read `k` while the binder was still `k.b1`, so dead-let elimination deleted
  `let k.b1 = 1000` and the model read the outer `k` (`s = 10`, result 15). `alphaCanon` now
  strips a suffix only when no other local in the body shares its base name, and renames every
  local position uniformly (vars, `let` binders, patterns, applied bindings, loop-carried names,
  assigned names, `LoopStep` field labels). The same blind strip applied to bug 045's match
  binders, so nested same-named match binders were capture-prone in the model too; the same rule
  covers them.

After the fix: interp = binary = model = lexical (1, 3, 2005), and p17 reads `outer`.

## Does Elab renaming alone close p17?

Yes, for the runtime defect: with the inner `r` renamed and dropped at the end of the `if`,
the later `show(r)` compiles to a read of the borrow of `base`, which is what the checker had
already assumed, so the program the checker approved is now the program that runs. The checker
keeps its own name-keyed environment (`consumeVar` marks every entry with a name; the
after-block comparison matches entries by name). Those are separate defects (093/094) in what
the checker accepts or rejects, and they are not fixed here.

## Regression tests

- `tests/programs/regress_087_let_block_shadow.con` (`run_ok 42`): the three hunt shapes plus
  else-branch, value-`if`, `while` and different-type shadows. A `run_tests.sh` leg also
  requires `--interp` to print 42 and the extracted model of `shadow_if`, `shadow_param` and
  `shadow_loop` to be lexical. Previous compiler: E0710 (SSA verify) compiled, 2 interpreted.
- `tests/programs/regress_087_borrow_shadow_uaf/` (project fixture, exit 0): p17 made
  self-checking. Previous compiler: exit 2 (the outer `r` was not `"outer"`).
- `scripts/tests/fuzz_differential.py`: new statement shape (`k == 11`) that shadows `v0`/`v1`/`v2`
  inside an `if`/`else` branch or a `while` body, reads the shadow, and leaves the outer value
  to the digest. Run against the previous compiler with `--seed 1`, it reports a mismatch on
  this shape within 60 programs. With the fix, seeds 1 and 2 (100 programs each) report 0
  mismatches.

## Limits

Shadowing is still accepted. The decided rule is to ban it (LANGUAGE_INVARIANTS §4, R-0435),
and that ban is not enforced yet; this fix makes the semantics that are accepted today lexical
on every consumer. Function-level `freshBinder` numbering is shared across a module, so adding
a shadow renumbers later `.bN` names in the same module. The normalizer is unaffected because
it compares suffix-free names where that is injective.
