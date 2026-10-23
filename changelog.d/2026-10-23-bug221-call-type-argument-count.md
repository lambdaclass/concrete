### A Call's Type-Argument List Matches The Callee (bug 221, 2026-10-23)

An explicit type-argument list at a function or method call was zipped with the callee's type
parameters, so extra arguments were dropped silently and a short list left parameters unbound. A
list of the wrong length is now refused with E0322 (TYPE-9). Pinned by four `error_221_*.con`
fixtures and `regress_221_exact_type_arguments.con`.
