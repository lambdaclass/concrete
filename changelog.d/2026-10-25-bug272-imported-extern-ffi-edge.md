### Calls of Imported Externs Have a calls-ffi Edge (bug 272, 2026-10-25)

`--report trust-edges` recorded `calls-ffi` only for calls of externs declared in the calling
module's own tree, so a call of an extern imported from another module (plainly, as a `trusted
extern`, or under an import alias) had none. The edge now comes from the call's binding to a
foreign declaration, the same binding its trust and capabilities were already decided by. Reporting
only: nothing is admitted or refused differently. Pinned by `regress_272_imported_extern_ffi_edge.con`,
natively and under `--interp`.
