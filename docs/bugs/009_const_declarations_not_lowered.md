# Bug 009: Const Declarations Parsed But Not Lowered

**Status:** Fixed
**Discovered:** 2026-03-15

## Fix

Constants now inline correctly during lowering. Added `constants` field to `LowerState` and a constant lookup path in `lowerExpr` `.ident` handler.

### Changes made:
- Added `constants : List (String × Ty × CExpr)` to `LowerState` in `Lower.lean`
- Added `collectAllConstants` helper to gather all `const` declarations from a module
- Updated `lowerExpr` `.ident` case: when a variable is not found in scope, checks `constants` and inlines the constant expression
- Updated `lowerFn` and `lowerModule` to thread constants through

`examples/snippets/constants.con` now compiles and runs correctly.
