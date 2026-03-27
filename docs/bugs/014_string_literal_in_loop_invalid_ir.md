# Bug 014: String Literal In Loop Produced Invalid LLVM IR

Status: fixed

## Symptom

String literals used in looped value-to-pointer paths could generate invalid LLVM IR, effectively trying to store a global symbol as a `%struct.String` value.

Representative shape:

```con
let s: String = "hello";
vec_push::<String>(&mut xs, s);
```

inside a loop or another path that required value-as-pointer materialization.

## Root Cause

`ensureValAsPtr` in `EmitSSA.lean` had no `.strConst` case.

That meant string constants bypassed proper materialization and ended up in invalid store paths.

## Fix

- added a `.strConst` case to `ensureValAsPtr`
- routed it through `materializeStrConst`
- ensured string constants are materialized as proper `{ ptr, len, cap }` values before pointer-based use

## Regression Coverage

- dedicated string-literal-in-loop regression added in `lean_tests/`
- real-world confirmation came from the JSON benchmark path and loop-heavy string cases
