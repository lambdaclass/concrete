
Group 3 — test types the owner asked about ("any more type of tests we should add early on?"). For EACH: AGREE | AGREE-WITH-CHANGES | DISAGREE; tier (PR / daily / weekly / split); slice (now / slice 0 / slice 1 / later, with the dependency); size (SMALL / LARGE); exists already? (cite file:line or "no"); and where it lives (which gate/branch: A spec-diff, B manifest/tests.sh, C scaling/determinism, D trusted base, gate 8, gate 9, root check, a new gate).
T-a Metamorphic tests: semantics-preserving rewrites give identical observable results (alpha-rename every binder; reorder independent items; add dead code/unused items; split/join lets; reformat). Targets the old compiler's shadowing/name class (bugs 039-045).
T-b Exhaustive small-width arithmetic: every operator x every u8/i8 operand pair (65,536) through the spec and both backends; results, traps and trap names match. 16/32/64-bit: boundary values (0, 1, max, min, max-1, powers of two).
T-c Validator after every pass: each IR (HIR, Core, MIR, CFG, SSA, BIR) has a well-formedness checker run on every test after every pass; a failure names the pass that broke it.
T-d Memory checking of emitted binaries: every test binary under valgrind memcheck (or equivalent); safe Concrete code shows zero invalid accesses or leaks (catches backend miscompiles that happen to give the right output).
T-e C layout/ABI comparison: for every repr(C)/extern type and call, compare size, alignment, field offsets and calling convention against clang, in both call directions.
T-f Golden diagnostics: snapshot exact text, span, rule ID, ordering and exit code of each refusal; a change is a reviewed diff.
T-g Theorem-statement pinning: fingerprint the statement of every key theorem so it cannot be silently weakened while the proof still passes (complements the axioms property and the vacuity gate).
T-h Limit tests: max nesting depth, huge literals, long identifiers, 10k functions, empty files: work or refuse with a rule ID, never crash or hang.
T-i Termination/bounded-execution refusals: owner rule "termination known per function, no hidden traps"; one fixture per way a program can fail to terminate or exceed a bound, each refused with its rule.
T-j A regression fixture for every bug: every ISS/bug fix adds a manifest fixture that fails before the fix; a meta-check verifies each closed bug row cites its fixture.
Fold your answers for Group 3 into your TIERS section and build order too.
