# Bug 126 — a field access through a `&mut Heap<T>` argument treats the slot holding the heap pointer as the struct

**Status:** FIXED (branch `lowering-bugs`, with bug 102)
**Found:** 2026-10-21, while drafting reproducers for the exit 2i specification (bug 123's session).
**Severity:** memory corruption in safe code. A field write through a `&mut Heap<B>` parameter
overwrites the caller's heap-pointer slot (and stores 16 bytes into that 8-byte slot), and the
later `free` receives a corrupted pointer; a field read returns the heap pointer's bits.

## Reproducer

`tests/regressions/bug126_field_write_through_mut_heap_ref/repro.con`:

```con
struct Copy B { v: Int, w: Int }

fn bump(h: &mut Heap<B>) {
    h.v = h.v + 1;
}

fn main() with(Alloc) -> Int {
    let mut h: Heap<B> = alloc(B { v: 42, w: 5 });
    bump(&mut h);
    let out: Int = h.v;
    free(h);
    // Every normal exit is below 128, so the signals pin can read a status
    // above 128 as a signal: 43 when correct, 1 for any other value.
    if out == 43 {
        return 43;
    }
    return 1;
}
```

On main (`b9b3cca9`, and unchanged at `9d17ccc5` after batch 2 landed; arm64 macOS) the program compiles and the binary is killed by `SIGTRAP`
(`Trace/BPT trap: 5`, exit 133), on every run. Expected: exit 43. `--interp` fails
`interp: undefined function 'alloc'` (the interpreter has no heap allocation; the same gap keeps
bug 104's heap reproducers out of the interpreter), so no interpreter/native comparison sees it.

The read half, `tests/regressions/bug126_field_write_through_mut_heap_ref/repro_read.con`
(`fn get(h: &mut Heap<B>) -> Int { return h.v; }`, called as `get(&mut h)`), compiles, and the
binary returns the heap pointer in place of field `v`: without the `% 16` the reproducer applies it
exits 32 on arm64 macOS (the pointer's low byte) where 42 is correct. The reproducer returns
`get(&mut h) % 16`, which is 10 when correct. The wrong value is a heap address, at least 8-byte
aligned, so `% 16` gives 0 or 8 and never 10 (0 on arm64 macOS, where malloc aligns to 16).

Controls that work on the same compiler: the same read through `&Heap<B>` (`get(&h)`) exits 42;
the field write on the `Heap<B>` local itself (`h.v = h.v + 1;` in `main`) exits 43; the same
write through `&mut B` to a stack struct exits 43 (and `--interp` prints 43).

The emitted SSA for `bump` and the caller:

```
define void @bump(ptr(mut ptr(%B)) %h) {
  %t5 = alloca %B
  %t0 = load %B, %h          ; loads a B from the slot that holds the heap pointer
  %t2 = load i64, %t0        ; field v = the heap pointer's bits
  %t3 = add i64 %t2, 1
  store %B %t0, %t5
  store i64 %t3, %t5
  %t7 = load %B, %t5
  store %B %t7, %h           ; 16-byte store into the caller's 8-byte slot
  ret void
}
  ; main
  %mutref.5 = alloca ptr(%B)
  store ptr %t4, %mutref.5   ; %t4 = the malloc result
  call void @bump(ptr(mut ptr(%B)) %mutref.5)
  %wb.6 = load ptr(%B), %mutref.5   ; now heap pointer + 1
  call void @free(ptr(%B) %wb.6)
```

The parameter type says two layers (`ptr(mut ptr(%B))`); the body dereferences one. The callee
reads 16 bytes from the caller's 8-byte `%mutref` slot, takes the heap pointer as `v`, and writes
back `{pointer + 1, the 8 bytes past the slot}`, so `free` receives the heap pointer plus one. A
write to `w` instead of `v` would store the new value into the caller's stack past the slot.

## On `land2-fix` (`74f7284a`)

Same behaviour. The `land2fix` worktree's compiler binary postdates that branch's last
compiler-source commit, and `Concrete/` is identical between `74f7284a` and the branch head
`f339ce64`: the reproducer compiles and the binary exits 133 (`SIGTRAP`), the read half exits 32,
and the three controls exit 42, 43 and 43. `land2-fix` neither rejects nor fixes it.

## Cause

Two lowering decisions about how many pointer layers a reference to a heap shell has disagree.

1. **Elab, the callee.** The heap-shell auto-deref (6D#3) in `Concrete/Elab/Elab.lean`
   (`.fieldAccess`, around line 704, and `.fieldAssign`, around line 1760) elaborates `h.v` for
   `h : &Heap<T>` or `&mut Heap<T>` as `(deref h : T).v`: a single dereference that skips both the
   reference and the heap shell. The checker (`Concrete/Check/Check.lean`, `.fieldAccess`, around
   line 845) states the same one-layer rule. That is correct only if a reference to a heap shell
   is represented as the heap pointer itself.
2. **Lower, the callers.** A shared borrow of a heap local follows that representation: in
   `lowerExpr`'s `.borrow` case (`Concrete/IR/Lower.lean`, around line 1345) an inner value of
   type `.heap _` is cast to the reference type, so `&h` is the heap pointer (one layer), which
   is why `get(&h)` reads 42. But a `&mut` borrow of a local passed directly as a call argument
   takes the write-back path in the `.call` case (around line 895, `.borrowMut (.ident ..)`):
   it allocates a `mutref.` slot of the local's type `Heap<B>`, stores the heap pointer into it,
   passes the slot's address and reloads the slot after the call (two layers). A promoted local
   on the same path passes its alloca, also two layers. Nothing reconciles the two.

`storeToPlace` then writes the updated `B` through the single-deref place, which is the slot.

## Fix

The decided one-hop rule, implemented on both sides so caller and callee agree:

- **Callers (`Lower`).** `&h` and `&mut h` on a `Heap<S>` local denote the slot `h`, like a borrow
  of any other non-array local (bug 102's place rule): `addrOfLocal` promotes the local to a stack
  slot when the borrow's type points at `Heap<S>`, `prePromoteAddrTaken` does it before a branch,
  and the `&mut h` call-argument write-back path already passed the slot. The shared borrow no
  longer casts the heap pointer to the reference type, so `get(&h)` and `get(&mut h)` now pass
  the same thing. No cheaper machine form is kept: passing the heap pointer itself is equivalent
  only for a callee that never writes the slot, and nothing records that.
- **Callees (`Elab`).** A field access or field assignment through `&Heap<S>` / `&mut Heap<S>`
  elaborates to two dereferences, `(*(*r)).f`: the reference to the slot, then the heap pointer
  it holds (an owned `Heap<S>` keeps one). The assignment's place evidence records both.
- **`CoreCheck`.** The inner dereference of a heap shell reached through a reference does not
  require `Alloc`, which keeps the source checker's rule (reading or writing a field through a
  borrowed heap shell needs no capability; dereferencing an owned one does).

A second defect on the same path is gone with it: a `&mut h` taken in one branch and in a loop
produced an SSA phi mixing `Heap<B>` and `ptr<B>` (E0710 at `ssa-verify`), because the heap local
was never promoted; it is now memory-backed.

Permanent legs: `tests/programs/regress_126_heap_ref_one_hop.con` in `run_tests.sh` (run_ok 255:
field write, field read through `&mut` and `&`, a `&mut Heap<B>` parameter passed on (reborrow),
through generics (`&mut Heap<Cell<T>>`), nested (a heap field of a struct reached through
`&mut Outer`, then `&mut o.h` passed on), a `&h` held in a binding, and the borrow taken in one
branch and in a loop; on the pre-fix compiler it does not compile, E0710), and the existing
`regress_090_guard_reads.con` and `regress_092_refs_flow_down.con` reads through `&Heap<B>`. Both
reproducers now exit correctly (43 and 10); their open-bug rows are dropped. The interpreter
still has no `alloc` (the gap that also keeps bug 104's heap reproducers out of it), so these
legs are native only.

## Structural cause

Storage and place representation decided ad hoc in `Lower` ([structural class](STRUCTURAL_CLASSES.md)
8, with 078, 095, 102, 105, 107, 110 and 116): whether `&mut` of a heap-shell local is the heap
pointer or the address of the local's slot is decided separately by the `.borrow` / `.borrowMut`
cases, by the call-argument write-back path and, upstream, by Elab's heap-shell desugar, with no
single place type saying how many layers a reference to `Heap<T>` has. The SSA verifier accepts a
`load %B` from a `ptr(mut ptr(%B))` operand, so the type disagreement reaches LLVM unchecked (a
validator gap of the kind R-0504 owns). The interpreter's missing `alloc` (bug 104) means the
interpreter/native differential cannot see any heap-shell reproducer.

## Structural owner

Exit 0b (memory corruption in safe code). The class is owned by R-0501 (S10 Place MIR: a borrow of
a heap-shell local and a field projection through it are one place with one representation) and
R-0504 (S12 validators: a load whose type disagrees with its pointer operand's pointee is
refused). The meaning the repair must implement was decided on 2026-10-21 as the one-hop rule
([MEMORY_SEMANTICS §5](../language/MEMORY_SEMANTICS.md), "Field access through a reference or a
heap shell"): `&h` and `&mut h` denote the slot `h`, and `e.f` with `e : &mut Heap<S>`
dereferences the reference to the slot and then the heap pointer the slot holds. The interim
repair (the Fix section) makes Elab's heap-shell desugar follow that rule (two dereferences for
`&Heap<S>` and `&mut Heap<S>`) and gives every `&h` / `&mut h` the slot: it keeps no cheaper
heap-pointer representation of `&h`, since none is shown equivalent to passing the slot.

## Containment while open

Until the fix, `scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") pinned both reproducers:
`repro.con` in mode `signals` (the binary builds and is killed by a signal; the signal is not
pinned, because the C allocator's response to the corrupted pointer differs by platform) and
`repro_read.con` in mode `wrong-exit:10`. When the bug is fixed, by rejection or correct code,
that leg failed until this document, the corpus entry and the leg were updated together; the fix
dropped both rows.
