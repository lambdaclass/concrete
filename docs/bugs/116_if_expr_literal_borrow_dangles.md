# Bug 116 — an if-expression whose arms borrow string literals yields a dangling reference

**Status:** OPEN
**Found:** 2026-10-20, by the examples-health pass (porting and output-testing every example).
**Severity:** memory safety: an accepted program reads freed storage.

## Reproducer

`tests/regressions/bug116_if_expr_literal_borrow_dangles/repro.con`:

```concrete
// Bug 116: an if-expression whose arms borrow string literals yields a
// reference that outlives the arm's temporary. The binary crashes (bus
// error, exit 138) and the interpreter fails "ref path base
// '__interp_str_lit_N' not in env". Expected: prints ALLOW, exit 0 (or a
// diagnostic). Found by examples/policy_engine.
fn main() with(Std) -> Int {
    let v: Int = 1;
    let label: &String = if v == 1 { &"ALLOW" } else { &"DENY" };
    print_string(label);
    print_char(10);
    return 0;
}
```

## Observed

`let label: &String = if v == 1 { &"ALLOW" } else { &"DENY" };` then `print_string(label)`: the binary crashes (bus error, exit 138) and the interpreter fails `ref path base '__interp_str_lit_N' not in env`. The literal temporary lives in the arm, and the borrow outlives it.

## Expected

Either reject the borrow (it escapes the arm) or extend the temporary through the binding's last use (for example to the end of its enclosing scope); extending it only to the end of the `let` statement would still leave `label` dangling at `print_string`.

## Where it showed up

examples/policy_engine (`gate`), which now appends a label in each arm.

## Containment while open

`scripts/tests/check_example_outputs.sh` (leg "open-bug reproducers") runs the reproducer and requires the recorded failure; when the bug is fixed that leg fails until this document, the corpus entry and the leg are updated together. Fixing the compiler is out of scope for the pass that filed it.
