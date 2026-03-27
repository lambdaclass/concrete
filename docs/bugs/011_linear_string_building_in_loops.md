# Bug 011: Linear String Building Is Awkward Inside Loops

**Status:** Fixed
**Discovered:** 2026-03-15
**Discovered in:** `examples/mal/main.con`

## Symptom

Incremental string building inside loops is awkward because `String` is linear and the obvious concat/update pattern does not compose well with loop-carried variables:

```con
let mut acc: String = "";
while cond {
    let next: String = string_concat(acc, piece);
    drop_string(acc);
    acc = next;
}
```

This became painful while implementing MAL reader/parser code. Combined with the lack of substring extraction, it pushed the implementation away from ordinary string construction altogether.

## Current Workarounds

- avoid building intermediate strings where possible
- compute hashes or lengths directly from source positions
- use more manual control-flow than the domain logic naturally wants

## Impact

- parser and pretty-printing code are harder to write than they should be
- string-heavy real programs pay an unnecessary ergonomics tax
- encourages avoidance patterns instead of direct, readable code

## Fix

Added two in-place string mutation builtins (analogous to `vec_push`):

- `string_push_char(s: &mut String, ch: Int)` — appends a single character. Grows buffer on demand (2x or min 8).
- `string_append(s: &mut String, other: &String)` — appends another string. Grows buffer to fit.

These take `&mut String`, so they work naturally with loop-carried mutable variables without consuming the string:

```con
let mut s: String = "";
while i < n {
    string_push_char(&mut s, 65 + i);
    i = i + 1;
}
string_append(&mut s, &"!");
```
