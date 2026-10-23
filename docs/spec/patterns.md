# Patterns

This chapter is a stub. It holds the one pattern rule that SCOPE-1 depends on. The rest of the
area (exhaustiveness, nested and destructuring patterns, guards, `if let` and `while let`)
migrates here later from PATTERN_DESTRUCTURING, PATTERN_ERGONOMICS and NESTED_PATTERNS
(README §8, step 3).

## Rules

<a id="PAT-1"></a>
### PAT-1. A pattern field may be bound under another name

**PAT-1@1.** A struct or enum pattern in `match`, `if let`, `while let` or a destructuring `let` may bind a field under another name with `field: binder`, and the formatter keeps the label.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseBindingList
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** SCOPE-1 forbids a binder that reuses a visible name. Without this rule, a nested
`Option` or `Result` match could not be written, because both payload fields are named
`value`. If any entry of a pattern is labeled, every entry names its field, each field once,
and every field is listed (`field: _` ignores a Copy field). Breaking that is E0304, which
migrates here with the rest of the patterns chapter. This was gap G4 of MEMORY_SEMANTICS §15.

```con accept name=nested-option exit=42
fn nested(o: Option<Option<Int>>) -> Int {
    match o {
        Option::Some { value: outer } => {
            match outer {
                Option::Some { value: inner } => { return inner; },
                Option::None => { return 0; }
            }
        },
        Option::None => { return 0; }
    }
}

fn main() -> Int {
    return nested(Option::Some { value: Option::Some { value: 42 } });
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_renamed_pattern_binder.con` | `run:42` | `-` | `binary,interp,fmt` |
