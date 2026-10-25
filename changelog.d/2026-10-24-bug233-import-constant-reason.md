### Importing a Public Constant or Trait Is Refused With Its Reason (bug 233, 2026-10-24)

`import cfg.{BASE}` of a `pub const`, or of a `pub trait`, was refused as "'BASE' is not public in
module 'cfg'", which it is. Traits are module-local and cannot be imported; a constant cannot be
imported yet, because Core names a constant by its spelling (bug 132). Both are now refused with
that reason (E0126 at Resolve, E0323 / E0423 in Check and Elab). Importing constants stayed open
(bug 233) until constants carried their declaration's identity (R-0492 slice 6, fixed 2026-10-25).
