### A Bound Must Name a Declared Trait (bug 231, 2026-10-24)

A generic parameter's bound naming an undeclared trait (`fn id<A: NoSuchTrait>`) was accepted: no
pass resolved a bound's trait names, so an unused generic compiled and a call met only E0241. In a
nested module a bound on the enclosing module's trait was satisfied by spelling at the call while
its methods were unknown in the body. Resolve now refuses a bound that is not `Copy`, `Destroy` or
a trait of its module (E0125, MOD-1). Found by the differential hunt; pinned by
`error_231_unknown_bound_trait.con`, `error_231_enclosing_trait_bound.con` and
`regress_231_bounds_name_module_traits.con`.
