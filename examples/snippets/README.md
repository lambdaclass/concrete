# Language snippets

Small single-feature programs — the language by example. (For the proof/contract
features, see the gallery in [`../README.md`](../README.md).) Compile and run any
of them:

```sh
concrete examples/snippets/<name>.con -o /tmp/out && /tmp/out
```

## Control flow
`if_else` · `if_with_literals` · `nested_if` · `variable_inside_if` ·
`while` · `while_loop` · `while_if_false` · `for` · `for_loop` · `for_loop_var` ·
`for_while`

## Numbers & small algorithms
`arithmetic` · `casts` · `constants` · `chars` · `floats` · `sizeof_intrinsic` ·
`gcd` · `collatz` · `is_prime` · `fact` · `factorial` · `factorial_if` ·
`fib` · `fib_if`

## Data types
`structs` · `enum` · `union` · `type_alias` · `generic_struct`

## Pattern matching & error flow
`enum_match` — match on enum variants ·
`error_flow` — **user-defined Result enum + match propagation** (Concrete has no
built-in `Result`; this is the idiomatic error-flow pattern) ·
`generic_enum_impl`

## Traits & generics
`simple_trait` · `trait_dispatch` · `assoc_method` · `impl_block` ·
`impl_block_mut` · `generics_basic` · `fib_generic`

## References, borrows, linearity
`refs` · `borrow` · `linearExample01` · `linearExample02` · `linearExample03if`

## Collections & memory
`arrays` · `vec` · `opaque_vec` · `linked_list` · `malloc`

## Modules & imports
`import` · `import_struct` · `import_struct_impl` · `direct_module_use` ·
`module_test` · `nested_calls`

## Strings & I/O
`hello_world` · `hello_world_array` · `hello_world_hacky` · `string_processing`

## Smallest starting point
`simple` — a minimal program.

> Note: these target the full language (the native codegen path). The
> proof-oriented frontend handles the integer/array/loop subset; for what is in
> the *provable* subset, see [`../evidence_classes/`](../evidence_classes/) and
> [`../../docs/EVIDENCE_CLASSES.md`](../../docs/EVIDENCE_CLASSES.md).
