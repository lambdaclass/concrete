# The Concrete IR

Currently in Concrete, the AST is lowered first to a IR to help support multiple targets and ease the generation of code on the end of the compilation process.

This IR is based on the concept of basic blocks, where within each block there is a linear flow (i.e) there is no branching.

Each block has a terminator, which for example can return or jump to another block, this is what defines the control flow of the program.


## The IR struct

The `IR` struct holds the whole compile unit, with all the modules, structs, functions, types, etc defined within.

An arena is used to store the data types, so they can be cheaply referenced by the given typed index.

The `IR` stores all of these in a flat structure, i.e, functions defined in a submodule are available for lookup directly by their index through the arena.

## The Module struct

Defines a module in concrete.

This structure holds the indexes of the functions, structs, modules, etc defined within this module.

## The Function struct

Defines a function in concrete.

It holds the basic blocks and the locals used within.

## The BasicBlock

It holds a array of statements and a terminator.

The statements have no branching.

The terminator defines where to branch, return from a function, a switch, etc.

## The Statement

Currently there are 3 kinds of statements: assign, storage live, storage dead.

Only assign is used currently: it contains a place and a rvalue.

## Place

This defines a place in memory, where you can load or store.

## RValue

A value found in the right hand side of an assignment, for example the use of an operand or a binary operation with 2 operands, a reference to a place, etc.

## Operand

A operand is a value, either from a place in memory or constant data.

## Local
A local is a local variable within a function body, it is defined by a place and the type of local, such as temporary, argument or a return pointer.
