# conlog — #35 validation-project findings

Running the log-analyzer workload against the real language/tooling surface.
Each finding is what the workload *forced*, not a theoretical gap.

## F1 (BLOCKER, checker bug) — FIXED 2026-07-07 (bug ledger 022)

## F1 (BLOCKER, checker bug): a submodule declaration breaks linear consumption in the PARENT module

Declaring `mod sub;` in a module makes the parent's linear-consumption
analysis stop recognizing `.drop()` (and, presumably, other consuming moves)
as consuming. A straight-line `s.drop()` on an owned `String` in the parent's
`main` is then falsely rejected with **E0208 "never consumed"**.

Minimal reproducer (project mode, `src/main.con`):

```con
mod conlog {
    mod parser;                    // <-- remove this line and it compiles
    fn process_text(text: &String) with(Std) -> Int {
        let n: Int = string_length(text);
        let mut i: Int = 0;
        while i <= n {
            if i == n {
                let line: String = string_slice(text, 0, i);
                line.drop();       // E0208: 'line' never consumed
            }
            i = i + 1;
        }
        return 0;
    }
    pub fn main() with(Std) -> Int {
        let s: String = "abc";
        let r: Int = process_text(&s);
        s.drop();                  // E0208: 's' never consumed (!)
        return r;
    }
}
```

- Without `mod parser;`: builds clean.
- With `mod parser;` (even with NO `import parser.{...}`): both `.drop()`s
  are rejected as non-consuming.

This is over-rejection (fail-closed, not unsound) but it blocks any
multi-module project that owns linear values — i.e. essentially every real
program. Almost certainly in the H12 submodule-checking path (the parent's
checker env / prelude String impl / consume state is disturbed when the
submodule is checked). Status: UNDER INVESTIGATION → fix required for #35.

## F1b (BLOCKER, codegen bug) — FIXED 2026-07-07 (bug ledger 023)

Short-circuit `&&`/`||` with a promoted aggregate (String) live in scope
emitted a phi of aggregate type (E0714, invalid IR). Hit by
`if got == 1 && tag() == 7`. Fixed in `Lower.lean` (never phi promoted vars
or aggregates at the scand/scor merge). Regression:
`tests/programs/scand_aggregate_in_scope/`.

## F2 (ergonomics): the cross-module String surface is the builtin free-functions, not methods

`string_length` / `string_char_at` / `string_slice` / `string_eq` /
`string_concat` (builtin intrinsics, no import) are the working String API in
project code, plus the `.append` / `.append_int` / `.drop` methods. The other
`String` methods defined in `std/src/string.con` (`.len()`, `.get()`,
`.push_char()`, `String::new()`) do NOT resolve from a project — method
dispatch misfires (wrong arg counts / wrong overload). Empty string is `""`,
not `String::new()`. Feeds the Phase 7 std-ergonomics + doc work: the method
surface and the free-function surface should be one consistent story.

## F3 (ergonomics): no `String::split` / `lines()`

Splitting a buffer into lines is hand-rolled with `string_char_at` +
`string_slice` scanning for byte 10. A `lines()` iterator or `split(sep)` is
the obvious missing std helper the moment you process text. (Deferral
discipline: this is the workload pulling the API.)
