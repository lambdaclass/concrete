# Bug 001: Cross-Module Struct Field Offset

**Status:** Fixed
**Discovered:** 2026-03-14
**Fixed:** 2026-03-14
**Regression test:** `lean_tests/bug_cross_module_struct_field.con`

## Symptom

All fields of a struct defined in another module read as offset 0 (the first field's value).

```
mod Types {
    pub struct Copy Point { x: i32, y: i32 }
}
mod Main {
    import Types.{ Point };
    fn main() with(Std) -> Int {
        let p: Point = ...;
        // p.y returns 10 (same as p.x), not 20
    }
}
```

## Root Cause

**File:** `Concrete/Elab.lean`, `elabModule` function (line ~1066–1121)

During elaboration, the `ElabEnv` correctly includes all struct definitions (`imports.structs ++ m.structs`) for type checking. However, the resulting `CModule` only included **local** struct definitions:

```lean
let cStructs := m.structs.map fun sd => ...
-- ...
.ok {
    structs := cStructs  -- BUG: only local structs, not imported ones
}
```

When `Lower.lowerModule` later calls `collectAllStructs`, it only finds structs in the module's `CModule.structs`. Since imported structs weren't there, `Layout.fieldOffset` couldn't find the struct definition and silently returned offset 0:

```lean
def fieldOffset (ctx : Ctx) (structName fieldName : String) : Nat :=
  match lookupStruct ctx structName with
  | some sd => ... -- compute real offset
  | none => 0      -- silent fallback to offset 0!
```

This meant every field access generated `gep ptr, 0` regardless of which field was being accessed.

## Fix

**File:** `Concrete/Elab.lean`

Include imported structs (deduplicated against local names) in the `CModule` output:

```lean
let localStructNames := m.structs.map (·.name)
let cImportedStructs := (imports.structs.filter fun sd =>
    !(localStructNames.contains sd.name)).map fun sd =>
  { ... : CStructDef }
-- ...
.ok {
    structs := cStructs ++ cImportedStructs
}
```

## Impact

Any program accessing fields (other than the first) of a struct defined in a different module would silently read the wrong value. Workaround before fix: define structs in the same module where fields are accessed, or pass individual scalar arguments instead of structs.
