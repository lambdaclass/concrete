# Bug 029: early-return `if` + `while` + post-loop array address-of miscompiles

**Status:** Open (workaround in std.fmt; K-shape below)
**Discovered:** 2026-07-14
**Discovered in:** Phase 7 14b — `std.fmt.write_int` stack-buffer rendering
**Minimal repro:** below (front-end accepts; `llvm-as` rejects the emitted IR)

## Symptom

A function containing (1) an `if` whose body RETURNS early, (2) any `while`
loop after it, and (3) `&arr as *const u8` on a local ARRAY after the loop,
emits IR where the array local is reloaded as a VALUE (`[N x i8]`) in a
position expecting its ADDRESS (`ptr`):

```text
llvm-as: error: '%if.load.53' defined with type '[20 x i8]' but expected 'ptr'
```

All three ingredients are required: drop the early-return `if` (probe G), drop
the `while` (H), or use an `if/else` with no early return (K) and the program
compiles and runs correctly. This is a new member of the divergent-branch
wrong-type-slot family (cf. the H7-era through-ref/void-slot cluster): the
post-`if` merge path re-materializes the array local via a load where the
address should flow.

## Minimal repro (front-end green, llvm-as red)

```concrete pseudocode
mod m {
  trusted fn take(p: *const u8, n: u64) -> u64 { return n; }
  trusted fn f(value: i64) -> u64 {
    let buf: [u8; 4] = [0; 4];
    if value == 0 { let z: u8 = 48; return take(&z as *const u8, 1); }
    let mut v: i64 = value;
    while v != 0 { v = v / 10; }
    return take(&buf as *const u8, 4);
  }
  fn main() -> Int { return f(42) as Int; }
}
```

## Workaround (K-shape)

Replace the early-return `if` with an `if/else` that assigns and falls
through; the same loop + array address-of then compiles and runs correctly.
`std.fmt.write_int`/`write_uint` use this shape.

## Secondary finding

The front-end accepted `buf[i] = …` on a NON-`mut` array binding inside a
`trusted fn` during the same probing (probe D) — mutability enforcement gap,
tracked here until split out.

## Fix sketch

Lower/EmitSSA: in the divergent-branch merge path, array locals must keep
their SLOT (address) identity — the merge should not introduce a by-value
reload of an aggregate that is later used via address-of. Likely the same
freshResultSlot/slot-identity machinery as the H7-era fixes; add the repro to
`tests/codegen/` as the regression once fixed and map it in
`audit_bug_corpus`.
