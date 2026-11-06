# Scope and Binding

A name inside a function body denotes at most one visible local value binding at every program
point (INV-SCOPE). A rule keyed by a name is therefore keyed by a binding. The reference rules
([references.md](references.md)) rely on this when they say that a reference's root "stays the
same binding". The design record is [MEMORY_SEMANTICS §15.4](../archive/design-records/MEMORY_SEMANTICS.md).

OD67 refuses a local that reuses a top-level declaration or import name; field labels remain distinct.

## Definitions

<a id="SCOPE-D1"></a>
### SCOPE-D1. Visible

**SCOPE-D1@3.** A local binding is visible from its declaration to the end of its declaring block, nested blocks included regardless of ownership state, with parameters visible throughout the body, pattern binders in their arm, a counted loop index only in its body and a borrow-block binder only in its block, without a named region.

- Kind: definition
- Uses: REF-D3
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.scope1Diagnostics
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Visibility is lexical and independent of ownership state; OD47 scopes a counted index to the body and OD57 removes named regions.

## Rules

<a id="SCOPE-1"></a>
### SCOPE-1. One lexical name, one binding

**SCOPE-1@3.** No binder reuses the spelling of a visible local binding, top-level declaration or import, and a contract expression binds no name at all.

- Kind: refusal
- Uses: SCOPE-D1
- Codes: E0301 (check), E0305 (check), E0292 (check), E0214 (check), E0215 (check)
- Status: pending R-0555
- Enforced by: Concrete.scope1Diagnostics, Concrete.constInitDiagnostics
- Semantics: none
- Obligation: INV-SCOPE (R-0500)
- Evidence: tested

**Rationale.** Visible names are not rebound, including top-level declarations and imports (OD67); siblings may reuse names, and field labels remain separate (SCOPE-2). Contracts introduce no runtime binders.

```con reject name=block-let-decided code=E0301 stage=check pair=block-let-renamed pending=R-0555 now=reject:E0301:check
fn example_value() -> i64 {
    let x: i64 = 1;
    if true {
        let x: i64 = 100;
        if x != 100 { return 0; }
    }
    return x + 41;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

```con accept name=block-let-renamed exit=42
fn example_value() -> i64 {
    let x: i64 = 1;
    if true {
        let inner_x: i64 = 100;
        if inner_x != 100 { return 0; }
    }
    return x + 41;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_scope1_const_initializer.con` | `reject:E0301:SCOPE-1@3` | `const-init` | `-` |
| `tests/programs/regress_2i_const_initializer_block.con` | `run:42` | `const-init` | `binary,interp-exempt:bug-117-the-interpreter-has-no-module-constants` |
| `tests/programs/error_2i_ref6_contract_ref_binding.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_scope1_contract_let.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_invariant_binds.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_variant_binds_ref.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_inferred_ref_alias.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_wrapped_binder.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_trait_impl_invariant.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_place_assignment.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_borrow_to_generic.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_generic_call.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_imported_generic.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_scope1_extern_duplicate_params.con` | `reject:E0301:SCOPE-1@3` | `scope1-duplicate_params` | `-` |
| `tests/programs/error_2i_scope1_trait_decl_duplicate_params.con` | `reject:E0301:SCOPE-1@3` | `scope1-duplicate_params` | `-` |
| `tests/programs/regress_2i_contract_aliased_ref_param.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/regress_2i_contract_ref_receiver.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/regress_2i_contract_static_nongeneric_method.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_spec_body_hidden_binder.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_spec_body_generic_call.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/regress_2i_contract_nongeneric_call_beside_generic_method.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_contract_generic_method_mangled.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/error_2i_contract_generic_method_static.con` | `reject:E0305:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/regress_2i_contract_paren_ref_arg.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_scope1_contracted_fn_shadow.con` | `reject:E0301:SCOPE-1@3` | `scope1-param_in_block` | `-` |
| `tests/programs/error_2i_scope1_spec_fn_duplicate_params.con` | `reject:E0301:SCOPE-1@3` | `contract` | `-` |
| `tests/programs/regress_2i_contract_and_spec_params.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_scope1_same_block_let.con` | `reject:E0301:SCOPE-1@3` | `scope1-same_block_let` | `-` |
| `tests/programs/regress_2i_scope1_same_block_let_renamed.con` | `run:42` | `scope1-same_block_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_block_let.con` | `reject:E0301:SCOPE-1@3` | `scope1-block_let` | `-` |
| `tests/programs/regress_2i_scope1_block_let_renamed.con` | `run:42` | `scope1-block_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_param_in_block.con` | `reject:E0301:SCOPE-1@3` | `scope1-param_in_block` | `-` |
| `tests/programs/regress_2i_scope1_param_in_block_renamed.con` | `run:42` | `scope1-param_in_block` | `binary,interp` |
| `tests/programs/error_2i_scope1_consuming_rebind.con` | `reject:E0301:SCOPE-1@3` | `scope1-consuming_rebind` | `-` |
| `tests/programs/regress_2i_scope1_consuming_rebind_renamed.con` | `run:42` | `scope1-consuming_rebind` | `binary,interp` |
| `tests/programs/error_2i_scope1_for_header.con` | `reject:E0301:SCOPE-1@3` | `scope1-for_header` | `-` |
| `tests/programs/regress_2i_scope1_for_header_renamed.con` | `run:42` | `scope1-for_header` | `binary,interp` |
| `tests/programs/error_2i_scope1_arm_binder.con` | `reject:E0301:SCOPE-1@3` | `scope1-arm_binder` | `-` |
| `tests/programs/regress_2i_scope1_arm_binder_renamed.con` | `run:42` | `scope1-arm_binder` | `binary,interp` |
| `tests/programs/error_2i_scope1_if_let.con` | `reject:E0301:SCOPE-1@3` | `scope1-if_let` | `-` |
| `tests/programs/regress_2i_scope1_if_let_renamed.con` | `run:42` | `scope1-if_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_while_let.con` | `reject:E0301:SCOPE-1@3` | `scope1-while_let` | `-` |
| `tests/programs/regress_2i_scope1_while_let_renamed.con` | `run:42` | `scope1-while_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_destructure.con` | `reject:E0301:SCOPE-1@3` | `scope1-destructure` | `-` |
| `tests/programs/regress_2i_scope1_destructure_renamed.con` | `run:42` | `scope1-destructure` | `binary,interp` |
| `tests/programs/error_2i_scope1_duplicate_params.con` | `reject:E0301:SCOPE-1@3` | `scope1-duplicate_params` | `-` |
| `tests/programs/regress_2i_scope1_duplicate_params_renamed.con` | `run:42` | `scope1-duplicate_params` | `binary,interp` |
| `tests/programs/error_2i_scope1_borrow_ref.con` | `reject:E0301:SCOPE-1@3` | `scope1-borrow_ref` | `-` |
| `tests/programs/regress_2i_scope1_borrow_ref_renamed.con` | `run:42` | `scope1-borrow_ref` | `binary,interp` |
| `tests/programs/error_2i_scope1_region_name.con` | `reject:E0301:SCOPE-1@3` | `scope1-region_name` | `-` |
| `tests/programs/regress_2i_scope1_region_name_renamed.con` | `run:42` | `scope1-region_name` | `binary,interp` |
| `tests/programs/error_borrow_shadow.con` | `reject:E0301:SCOPE-1@3` | `scope1-borrow_ref` | `-` |
| `tests/programs/error_093_shadowing_block_local_leak.con` | `reject:E0301:SCOPE-1@3` | `scope1-block_let` | `-` |
| `tests/programs/error_093_shadowing_match_binder_leak.con` | `reject:E0301:SCOPE-1@3` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_arm_binder_shadows_live.con` | `reject:E0301:SCOPE-1@3` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_destructure_shadows_live.con` | `reject:E0301:SCOPE-1@3` | `scope1-destructure` | `-` |
| `tests/programs/error_094_value_binder_shadows_live.con` | `reject:E0301:SCOPE-1@3` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_borrow_block_shadow_leak.con` | `reject:E0301:SCOPE-1@3` | `scope1-block_let` | `-` |
| `tests/programs/error_094_mut_ref_param_shadow_leak.con` | `reject:E0301:SCOPE-1@3` | `scope1-param_in_block` | `-` |
| `tests/programs/error_107_shadow_under_live_ref.con` | `reject:E0301:SCOPE-1@3` | `scope1-same_block_let` | `-` |
| `tests/programs/error_107_shadow_in_block_under_live_ref.con` | `reject:E0301:SCOPE-1@3` | `scope1-block_let` | `-` |
| `tests/programs/regress_093_shadowing_locals_consumed.con` | `reject:E0301:SCOPE-1@3` | `scope1-block_let` | `-` |
| `tests/programs/regress_094_shadowing_binders.con` | `reject:E0301:SCOPE-1@3` | `scope1-arm_binder` | `-` |
| `tests/programs/regress_094_shadow_state_by_identity.con` | `reject:E0301:SCOPE-1@3` | `scope1-same_block_let` | `-` |
| `tests/programs/regress_045_match_binder_shadow.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_087_let_block_shadow.con` | `reject:E0301:SCOPE-1@3` | `scope3-087` | `-` |
| `tests/programs/regress_087_borrow_shadow_uaf/` | `reject:E0301:SCOPE-1@3` | `scope3-087-borrow` | `-` |

<a id="SCOPE-2"></a>
### SCOPE-2. What is not a collision

**SCOPE-2@3.** Bindings in sibling scopes, one declaration in a loop body executed many times, an assignment and a field label are not collisions under SCOPE-1, while a local may not reuse a visible top-level declaration or import name and types and values share one module namespace.

- Kind: guarantee
- Uses: SCOPE-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.scope1Diagnostics
- Semantics: none
- Obligation: INV-SCOPE (R-0500)
- Evidence: tested

**Rationale.** OD43 completes one module namespace; OD67 refuses a local that reuses a top-level name. Sibling lexical scopes and field labels do not introduce two visible bindings.

```con accept name=sibling-branches exit=42
fn pick(c: bool) -> i64 {
    let mut out: i64 = 0;
    if c {
        let x: i64 = 40;
        out = x;
    } else {
        let x: i64 = 2;
        out = x;
    }
    return out;
}

fn example_value() -> i64 {
    return pick(true) + pick(false);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=loop-body-let exit=42
fn example_value() -> i64 {
    let mut total: i64 = 0;
    let mut k: i64 = 0;
    while k < 6 {
        let step: i64 = 7;
        total = total + step;
        k = k + 1;
    }
    return total;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_scope2_sibling_branches.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_sibling_arms.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_consecutive_loops.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_loop_body_let.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_assign_rebind.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_2i_scope2_borrow_after_loop.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_087_sibling_promoted.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_087_unpromoted_sibling.con` | `run:42` | `-` | `binary,interp` |

<a id="SCOPE-3"></a>
### SCOPE-3. Blocks are lexical on every consumer

**SCOPE-3@3.** A binding declared in a block is not visible after the block, and every consumer (the checker, elaboration, the interpreter, lowering, extraction and evidence frames) resolves a name to the innermost visible binding.

- Kind: guarantee
- Uses: SCOPE-D1
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.scope1Diagnostics, Concrete.bindLocal, Concrete.enterBlock
- Semantics: none
- Obligation: INV-SCOPE (R-0500); every consumer agrees (PASS-1)
- Evidence: tested

**Rationale.** SCOPE-1 lets a name be reused only in a sibling scope. Every consumer must still
mean the lexical binding when that happens. Before exit 2i, shadowing inside a block, loop body
or borrow block miscompiled (bug 087): `let x: i64 = 1; if true { let x: i64 = 100; } return x
+ 41;` exited 141. Elaboration's alpha-renaming of block locals (`bindLocal`, `enterBlock`)
stays as defense in depth. One conformance row also checks the extracted model.

```con accept name=sibling-reuse-after-block exit=42
fn example_value() -> i64 {
    let mut out: i64 = 0;
    if true {
        let x: i64 = 100;
        out = out + x * 0;
    }
    if true {
        let x: i64 = 42;
        out = out + x;
    }
    return out;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_scope1_087_renamed.con` | `run:42` | `scope3-087` | `binary,interp,model:087-renamed` |
| `tests/programs/regress_2i_scope1_087_borrow_renamed/` | `run:0` | `scope3-087-borrow` | `binary,interp-exempt:project-fixture-(the-interpreter-runs-single-files)` |

## Version history

This appendix records superseded versions and is not normative.

- SCOPE-D1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SCOPE-D1@1 is superseded by version.
- SCOPE-1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SCOPE-1@1 is superseded by version.
- SCOPE-2@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SCOPE-2@1 is superseded by version.
- SCOPE-3@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; SCOPE-3@1 is superseded by version.
