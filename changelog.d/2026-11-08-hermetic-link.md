Native slice-0 links execute exact flake-owned real-linker templates (ISS-0031, R-0577).
No compiler/linker wrapper can rewrite arguments or synthesize RPATH after validation.
GNU and Darwin implicit library directories are disabled; searches, startup objects, runtimes
and local objects are checked before linking, then traces and recursive runtime metadata are
validated. Raw dot-component traversal, object symlinks and store objects passed as local inputs
are refused. Every constructor creates private scratch rather than inheriting ambient TMPDIR.

The compiler and real-linker grammars have direct negative controls. Shared invocation and host
alias controls run on Darwin as well as Linux; synthetic Mach-O headers exercise the production
parser on every host. A digest-checked historical helper keeps the four product and eight
invocation differential obligations available in shallow CI clones. Darwin metadata derivations
no longer introduce the uncached stdenvNoCC -> locale -> bmake build dependency.

Round-3 Linux reproducibility compared all 32 examples across 13 stages without normalization.
ISS-0031 remains open pending Darwin execution and cross-machine CI; Darwin runtime ABI libraries
remain supplied by the host OS. See Concrete/README.md for the policy and validation boundary.

Round 4 shares the production real-linker template with every metadata perturbation control,
restores Linux full RELRO and Darwin compiler-rt builtins, and validates OS install names
against strictly parsed, target-selected TAPI stubs plus explicit per-mode policy declarations.
The sysroot uses the complete pinned Apple SDK source; nixpkgs' processed SDK removes libc++
stubs needed to attest the ASan runtime's OS ABI. Fixture controls run the production Mach-O
validator on Linux, including malformed stubs, re-exports, missing names and mode restrictions.
