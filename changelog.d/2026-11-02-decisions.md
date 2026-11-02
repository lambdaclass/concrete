### The remaining owner questions are answered (2026-11-02)

`docs/spec/decisions.md` records the answers of 2026-11-02: the type-name aliases `Int`, `Uint`,
`Float64` and `Bool` are removed; `main` and test functions return `u8` only; a module has one
namespace for types and values, and a child module's declarations are reached only through
`child::T` or an import. The authority rules are completed: every operation needs a resource value and
a permission, the target's services (allocator, directories, environment, clock, network endpoints,
devices) are grants handed to `main`, which consumes them; allocation takes an `Allocator<A>` value on
every target, is fallible and conserves its input, and allocations carry their allocator's identity;
filesystem access goes through `Directory` values confined at lookup. The new compiler's architecture
is approved with conditions, 0.1 comes from the new compiler, and RocksDB, ethlambda and usability work
is paused until the new compiler can run it. The questions the spec audit raised are
answered too: allocator reset is allowed when nothing allocated from it is alive; any library may
declare externs in trusted code, only the compiler introduces intrinsics and only the target runtime
mints grants; `abort()` needs no capability; `main` declares the grants it wants; ambiguous trait calls
are written `Trait::method(&x)`; `defer` takes a call returning `Unit`; user capability aliases are
removed.
