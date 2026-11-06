### Language specification: docs/platform and docs/stdlib cite the spec (2026-10-30)

`docs/platform/LAYOUT_CONTRACT.md` is now a design record in `docs/archive/design-records/`. New
entries: FFI-10 (the four layout forms; `transparent` and any `repr` on an enum are refused),
FFI-11 (a `repr(C)` struct argument of an `extern fn` passes by value, pending R-0502), FFI-12
(the stride of an array, `HeapArray` or raw buffer is `sizeof::<T>()`), TYPE-23 (the builtin
`Option` and `Result`), TYPE-24 (the conversions of a newtype), SYN-5 (UTF-8 source and literals,
the six escapes), STMT-13 (`abort()` needs `Process`) and TRAIT-11 (destruction cannot fail). The
platform and stdlib documents now cite these and the existing entries instead of restating them,
and their stale statements are corrected: an `Int` `main` masked to 8 bits, `exit()` as an
operation, implicit destruction at scope exit, `trusted extern fn` needing no capability, `&mut`
consumed on call, C-style `for` examples, `Std` including `Unsafe`, `as` narrowing, guards and `_`
as deferred, `fn(T) -> U` as C-compatible, `Text` as a borrowed view, contracts as deferred and
overflow trapping as "not a guarantee". Placeholders OPEN-122 to OPEN-127 for returned `repr(C)`
structs, `as` on a newtype outside its module, profiles in the spec, contracts at run time,
target assumptions and UTF-8 in every `String`.
