# Scope and Binding

A name inside a function body denotes at most one visible local value binding at every program
point (INV-SCOPE). A rule keyed by a name is therefore keyed by a binding. The reference rules
([references.md](references.md)) rely on this when they say that a reference's root "stays the
same binding". The design record is [MEMORY_SEMANTICS §15.4](../language/MEMORY_SEMANTICS.md).

SCOPE-4, the namespace boundary, is still an open question and is not a rule. It asks whether
a local may reuse the name of a top-level function, an import alias or an intrinsic. Today it
may. That decision belongs to the modules chapter (README §8).

## Definitions

<a id="SCOPE-D1"></a>
### SCOPE-D1. Visible

**SCOPE-D1@1.** A local binding is visible from its declaration to the end of the block that declares it, nested blocks included and whatever its state, where a parameter's block is the whole body, a pattern binder's is its arm or body, a `for` header binder's is the header and body, and a borrow-block binder's and region name's is the borrow block.

- Kind: definition
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.scope1Diagnostics
- Semantics: none
- Obligation: none
- Evidence: by-users

**Rationale.** Visibility is lexical and ignores ownership state. A binding that has been
consumed, frozen or moved is still visible, so a later binder cannot reuse its name to make
the old name mean something new. Under STMT-6@3 (pending, bug 331) a counted loop's index is visible
in the body only, not in its bounds; this definition gains a version when the counted loop lands,
since a version now would retag SCOPE-1 to SCOPE-3 while main has only the C-style header.

## Rules

<a id="SCOPE-1"></a>
### SCOPE-1. One lexical name, one binding

**SCOPE-1@1.** No binder reuses the spelling of a local value binding that is visible at its declaration, and a contract expression binds no name at all.

- Kind: refusal
- Uses: SCOPE-D1
- Codes: E0301 (check), E0305 (check), E0292 (check), E0214 (check), E0215 (check)
- Status: enforced
- Enforced by: Concrete.scope1Diagnostics, Concrete.constInitDiagnostics
- Semantics: none
- Obligation: INV-SCOPE (R-0500)
- Evidence: tested

**Rationale.** Binders are function parameters, `let` binders (destructuring ones included),
match-arm, `if let` and `while let` binders, `for` header binders (a counted loop's index, once OD47's loop lands), and the reference and region
names of a borrow block. The visible binding's state and type do not matter. So
`let s = transform(s)` is refused even though the right-hand side consumed `s`, and a Copy
binding is protected like a linear one. Contract expressions are binder-free propositions, so a
binder in one is refused as well (E0305). The rule prevents the shadowing bugs 087, 093, 094,
107 and 124 at their source. E0214, E0215 and E0292 are no longer emitted, and E0301 covers
them; E0238's shadowed-source guard is gone too. Binding identity is minted by the same walk
(R-0492), so a spelling that is not visible names a different binding.

```con reject name=block-let code=E0301 stage=check pair=block-let-renamed
fn main() -> Int {
    let x: Int = 1;
    if true {
        let x: Int = 100;
        if x != 100 { return 0; }
    }
    return x + 41;
}
```

```con accept name=block-let-renamed exit=42
fn main() -> Int {
    let x: Int = 1;
    if true {
        let inner_x: Int = 100;
        if inner_x != 100 { return 0; }
    }
    return x + 41;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/error_2i_scope1_const_initializer.con` | `reject:E0301:SCOPE-1@1` | `const-init` | `-` |
| `tests/programs/regress_2i_const_initializer_block.con` | `run:42` | `const-init` | `binary,interp-exempt:bug-117-the-interpreter-has-no-module-constants` |
| `tests/programs/error_2i_ref6_contract_ref_binding.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_scope1_contract_let.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_invariant_binds.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_variant_binds_ref.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_inferred_ref_alias.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_wrapped_binder.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_trait_impl_invariant.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_place_assignment.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_borrow_to_generic.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_generic_call.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_imported_generic.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_scope1_extern_duplicate_params.con` | `reject:E0301:SCOPE-1@1` | `scope1-duplicate_params` | `-` |
| `tests/programs/error_2i_scope1_trait_decl_duplicate_params.con` | `reject:E0301:SCOPE-1@1` | `scope1-duplicate_params` | `-` |
| `tests/programs/regress_2i_contract_aliased_ref_param.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/regress_2i_contract_ref_receiver.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/regress_2i_contract_static_nongeneric_method.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_spec_body_hidden_binder.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_spec_body_generic_call.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/regress_2i_contract_nongeneric_call_beside_generic_method.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_contract_generic_method_mangled.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/error_2i_contract_generic_method_static.con` | `reject:E0305:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/regress_2i_contract_paren_ref_arg.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_scope1_contracted_fn_shadow.con` | `reject:E0301:SCOPE-1@1` | `scope1-param_in_block` | `-` |
| `tests/programs/error_2i_scope1_spec_fn_duplicate_params.con` | `reject:E0301:SCOPE-1@1` | `contract` | `-` |
| `tests/programs/regress_2i_contract_and_spec_params.con` | `run:42` | `contract` | `binary,interp` |
| `tests/programs/error_2i_scope1_same_block_let.con` | `reject:E0301:SCOPE-1@1` | `scope1-same_block_let` | `-` |
| `tests/programs/regress_2i_scope1_same_block_let_renamed.con` | `run:42` | `scope1-same_block_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_block_let.con` | `reject:E0301:SCOPE-1@1` | `scope1-block_let` | `-` |
| `tests/programs/regress_2i_scope1_block_let_renamed.con` | `run:42` | `scope1-block_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_param_in_block.con` | `reject:E0301:SCOPE-1@1` | `scope1-param_in_block` | `-` |
| `tests/programs/regress_2i_scope1_param_in_block_renamed.con` | `run:42` | `scope1-param_in_block` | `binary,interp` |
| `tests/programs/error_2i_scope1_consuming_rebind.con` | `reject:E0301:SCOPE-1@1` | `scope1-consuming_rebind` | `-` |
| `tests/programs/regress_2i_scope1_consuming_rebind_renamed.con` | `run:42` | `scope1-consuming_rebind` | `binary,interp` |
| `tests/programs/error_2i_scope1_for_header.con` | `reject:E0301:SCOPE-1@1` | `scope1-for_header` | `-` |
| `tests/programs/regress_2i_scope1_for_header_renamed.con` | `run:42` | `scope1-for_header` | `binary,interp` |
| `tests/programs/error_2i_scope1_arm_binder.con` | `reject:E0301:SCOPE-1@1` | `scope1-arm_binder` | `-` |
| `tests/programs/regress_2i_scope1_arm_binder_renamed.con` | `run:42` | `scope1-arm_binder` | `binary,interp` |
| `tests/programs/error_2i_scope1_if_let.con` | `reject:E0301:SCOPE-1@1` | `scope1-if_let` | `-` |
| `tests/programs/regress_2i_scope1_if_let_renamed.con` | `run:42` | `scope1-if_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_while_let.con` | `reject:E0301:SCOPE-1@1` | `scope1-while_let` | `-` |
| `tests/programs/regress_2i_scope1_while_let_renamed.con` | `run:42` | `scope1-while_let` | `binary,interp` |
| `tests/programs/error_2i_scope1_destructure.con` | `reject:E0301:SCOPE-1@1` | `scope1-destructure` | `-` |
| `tests/programs/regress_2i_scope1_destructure_renamed.con` | `run:42` | `scope1-destructure` | `binary,interp` |
| `tests/programs/error_2i_scope1_duplicate_params.con` | `reject:E0301:SCOPE-1@1` | `scope1-duplicate_params` | `-` |
| `tests/programs/regress_2i_scope1_duplicate_params_renamed.con` | `run:42` | `scope1-duplicate_params` | `binary,interp` |
| `tests/programs/error_2i_scope1_borrow_ref.con` | `reject:E0301:SCOPE-1@1` | `scope1-borrow_ref` | `-` |
| `tests/programs/regress_2i_scope1_borrow_ref_renamed.con` | `run:42` | `scope1-borrow_ref` | `binary,interp` |
| `tests/programs/error_2i_scope1_region_name.con` | `reject:E0301:SCOPE-1@1` | `scope1-region_name` | `-` |
| `tests/programs/regress_2i_scope1_region_name_renamed.con` | `run:42` | `scope1-region_name` | `binary,interp` |
| `tests/programs/error_borrow_shadow.con` | `reject:E0301:SCOPE-1@1` | `scope1-borrow_ref` | `-` |
| `tests/programs/error_093_shadowing_block_local_leak.con` | `reject:E0301:SCOPE-1@1` | `scope1-block_let` | `-` |
| `tests/programs/error_093_shadowing_match_binder_leak.con` | `reject:E0301:SCOPE-1@1` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_arm_binder_shadows_live.con` | `reject:E0301:SCOPE-1@1` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_destructure_shadows_live.con` | `reject:E0301:SCOPE-1@1` | `scope1-destructure` | `-` |
| `tests/programs/error_094_value_binder_shadows_live.con` | `reject:E0301:SCOPE-1@1` | `scope1-arm_binder` | `-` |
| `tests/programs/error_094_borrow_block_shadow_leak.con` | `reject:E0301:SCOPE-1@1` | `scope1-block_let` | `-` |
| `tests/programs/error_094_mut_ref_param_shadow_leak.con` | `reject:E0301:SCOPE-1@1` | `scope1-param_in_block` | `-` |
| `tests/programs/error_107_shadow_under_live_ref.con` | `reject:E0301:SCOPE-1@1` | `scope1-same_block_let` | `-` |
| `tests/programs/error_107_shadow_in_block_under_live_ref.con` | `reject:E0301:SCOPE-1@1` | `scope1-block_let` | `-` |
| `tests/programs/regress_093_shadowing_locals_consumed.con` | `reject:E0301:SCOPE-1@1` | `scope1-block_let` | `-` |
| `tests/programs/regress_094_shadowing_binders.con` | `reject:E0301:SCOPE-1@1` | `scope1-arm_binder` | `-` |
| `tests/programs/regress_094_shadow_state_by_identity.con` | `reject:E0301:SCOPE-1@1` | `scope1-same_block_let` | `-` |
| `tests/programs/regress_045_match_binder_shadow.con` | `run:42` | `-` | `binary,interp` |
| `tests/programs/regress_087_let_block_shadow.con` | `reject:E0301:SCOPE-1@1` | `scope3-087` | `-` |
| `tests/programs/regress_087_borrow_shadow_uaf/` | `reject:E0301:SCOPE-1@1` | `scope3-087-borrow` | `-` |

<a id="SCOPE-2"></a>
### SCOPE-2. What is not a collision

**SCOPE-2@1.** Bindings in sibling scopes, one declaration in a loop body executed many times, an assignment and a name in another namespace (a field label, type, trait or module) are not collisions under SCOPE-1.

- Kind: guarantee
- Uses: SCOPE-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.scope1Diagnostics
- Semantics: none
- Obligation: INV-SCOPE (R-0500)
- Evidence: tested

**Rationale.** None of these makes one name denote two visible bindings. Sibling scopes are the
two branches of an `if`, sibling match arms, consecutive blocks and consecutive loops. An
assignment `x = e` introduces no binding. A `mut` linear place may be assigned after its value
is consumed (`acc = f(acc, x)`).

```con accept name=sibling-branches exit=42
fn pick(c: bool) -> Int {
    let mut out: Int = 0;
    if c {
        let x: Int = 40;
        out = x;
    } else {
        let x: Int = 2;
        out = x;
    }
    return out;
}

fn main() -> Int {
    return pick(true) + pick(false);
}
```

```con accept name=loop-body-let exit=42
fn main() -> Int {
    let mut total: Int = 0;
    let mut k: Int = 0;
    while k < 6 {
        let step: Int = 7;
        total = total + step;
        k = k + 1;
    }
    return total;
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

**SCOPE-3@1.** A binding declared in a block is not visible after the block, and every consumer (the checker, elaboration, the interpreter, lowering, extraction and evidence frames) resolves a name to the innermost visible binding.

- Kind: guarantee
- Uses: SCOPE-D1
- Codes: none
- Status: enforced
- Enforced by: Concrete.scope1Diagnostics, Concrete.bindLocal, Concrete.enterBlock
- Semantics: none
- Obligation: INV-SCOPE (R-0500); every consumer agrees (PASS-1)
- Evidence: tested

**Rationale.** SCOPE-1 lets a name be reused only in a sibling scope. Every consumer must still
mean the lexical binding when that happens. Before exit 2i, shadowing inside a block, loop body
or borrow block miscompiled (bug 087): `let x: Int = 1; if true { let x: Int = 100; } return x
+ 41;` exited 141. Elaboration's alpha-renaming of block locals (`bindLocal`, `enterBlock`)
stays as defense in depth. One conformance row also checks the extracted model.

```con accept name=sibling-reuse-after-block exit=42
fn main() -> Int {
    let mut out: Int = 0;
    if true {
        let x: Int = 100;
        out = out + x * 0;
    }
    if true {
        let x: Int = 42;
        out = out + x;
    }
    return out;
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_scope1_087_renamed.con` | `run:42` | `scope3-087` | `binary,interp,model:087-renamed` |
| `tests/programs/regress_2i_scope1_087_borrow_renamed/` | `run:0` | `scope3-087-borrow` | `binary,interp-exempt:project-fixture-(the-interpreter-runs-single-files)` |
