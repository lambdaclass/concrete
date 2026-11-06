### An Aliased Newtype Import Names the Newtype (bug 243, 2026-10-25)

`import m.{N as M}` of a newtype bound no `M`, so `M::mk` was refused (E0106): the import appended
the newtype under its own name, and a newtype had no definition-name field to keep its identity
apart from an alias. A newtype now carries `definitionName` as a struct or enum does, and the
import binds the alias to the declaration's identity: `M` is `N`, with its methods, in Check, in
Core and in both backends. Pinned by `regress_243_aliased_newtype_import.con`,
`regress_243_aliased_newtype_with_original.con`, `regress_243_aliased_generic_newtype.con` and
`regress_243_nested_aliased_newtype.con`, native and `--interp`.
