# Bug 008: If-Else Expression Does Not Parse for Aggregate Types

**Status:** Fixed
**Discovered:** 2026-03-15

## Fix

If-else now works as an expression for both scalar and aggregate types:

```con
let x: i32 = if cond { 10 } else { 20 };
let v_label: String = if v == 1 { "ALLOW" } else { "DENY" };
```

### Changes made:
- Added `ifExpr` variant to `AST.Expr` and `Core.CExpr`
- Added `parseExprBlock` in `Parser.lean` — variant of `parseBlock` allowing trailing expression without semicolon
- If-expression parsing in `parsePrimary` when token is `.if_`
- Elaboration propagates type hints to branch bodies
- Lowering uses alloca+condBr+store+load pattern with type casts for mismatched branch types
- Pattern match additions in Format, Resolve, CoreCanonicalize, Mono, CoreCheck, Elab, Check
