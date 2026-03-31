# Bug 006: Cross-Module String Literal Name Collisions

**Status:** Fixed
**Discovered:** 2026-03-15
**Fix:** `Concrete/Lower.lean` — prefix string constant names with module name

## Symptom

Programs with string literals in multiple modules produce corrupt string data at runtime. One string silently aliases another, causing wrong output, crashes in `string_concat`, or `free()` of invalid pointers.

## Reproduction

```con
mod Names {
    pub fn get_name() -> String {
        return "hello";
    }
}

mod Main {
    import Names.{ get_name };

    fn main() with(Std) -> Int {
        let s: String = get_name();
        let r: String = string_concat(s, " world");
        drop_string(r);
        return 0;
    }
}
```

This crashes at runtime. The generated LLVM IR contains two conflicting globals:

```llvm
@str.0 = private constant [6 x i8] c"hello\00"
@str.0 = private constant [7 x i8] c" world\00"
```

LLVM silently picks one definition and discards the other, so both modules reference the same bytes.

## Root Cause

`Lower.lean:lowerModule` deduplicates string literals per-module and assigns names `str.0`, `str.1`, etc. using `s!"str.{deduped.length}"`. Each module starts its counter at 0. When `emitSSAProgram` processes multiple modules sequentially, their string globals collide.

The within-module deduplication (lines 1633-1637) is correct — same-value strings in the same module share a name. The bug is that different-value strings across modules can get the same name.

## Fix

Prefix string constant names with the module name:

```lean
-- Before:
else deduped ++ [(s!"str.{deduped.length}", strVal)]

-- After:
else deduped ++ [(s!"{m.name}.str.{deduped.length}", strVal)]
```

This produces `@Names.str.0` and `@Main.str.0` — unique across all modules.

## Impact Before Fix

- Any multi-module program with string literals in more than one module could silently corrupt data
- `string_concat` with cross-module strings crashed (double-free or wrong memcpy length)
- The policy engine example was completely blocked by this bug
- Single-module programs and programs where all modules happen to share identical string sets were unaffected

## Regression Test

The policy engine example (`examples/policy_engine/main.con`) exercises this path: the `Gate` module contains string literals (`"["`, `"] "`, `" "`) while `Main` contains different ones (`"DENY"`, `"ALLOW"`, principal/resource names). Before the fix, it crashed; after, it prints correct output.
