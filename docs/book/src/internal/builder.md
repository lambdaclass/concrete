# The IR builder

This struct is used during the building of the IR, it holds structures needed only temporarely while building.

Some of these structures are:

- A SymbolTable: which holds mappings from names to indexes.
- The AST bodies of the major structures like functions, structs, etc. They are `Arc` thus cheaply clonable.
- A mapping of top level module names to their module indexes.
- The current `self_ty` self type to use during lowering when inside a method with a self argument.
- The current `local_module` being built.
- A struct index to type index mapping, to avoid duplicating and making equality easier.
- A type index to module index mapping, to aid in method resolution.

# The function IR  builder

When lowering a function the IR builder is wrapper around a function builder, holding extra information to lower the function:

- The function IR being build.
- A mapping from name to local index (for named variables).
- A list of statements
- The return local index.
- A hashset to know whether a local has been initialized (aids error checking).

# Passes

Lowering is done roughly with the following passes:

## Symbol resolution

A key concept is that the arenas holding the structures are `Option<T>`, this is needed to solve cyclic references.

The first pass loops through all the modules and all their structures, inserting them into the arenas with `None` (for everything expect modules) and storing the returned index to the symbol table.

Actually, within the pass, 2 loops are done, this is because for `impl blocks` we first need to have the `struct` and `types` ids assigned, so we can relate the methods to their types. So impl blocks are handled as a separate step.

## Import resolution

The next pass handles imports between modules.

## Module lowering

Here the whole lowering happens for all functions, structs, etc.
