Native slice-0 links consume one structured flake-owned compiler, environment, flag and closure
policy (ISS-0031, R-0577). They reject unapproved caller arguments and undeclared effective
linker searches before linking, then validate exact trace grammar and recursive runtime metadata.
Object symlink escapes, empty ELF runtime tags and Mach-O environment injection are refused.
Linux uses compiler-rt and static LLVM libunwind rather than exempting GCC's empty runtime tag.
Darwin keeps the native wrapper target, fixed deployment version and pinned SDK, with a directly
named compiler-rt ASan runtime. Unexpected wrapper output produces a bounded diagnostic.

Working-directory, TMPDIR and ambient-flag controls now assert executable purity as well as equality;
all four product-purity assertions and eight invocation refusal controls detect the pre-fix
688e10a86 behavior. The dead ambient libc plant and surrogate-sysroot positive claim were removed;
sysroot overrides are negative controls. Build identity includes policy and closure digests.
Round-1 Linux CI manifests matched across runners, but macOS failed. ISS-0031 remains open pending
round-2 macOS and cross-machine CI; the host OS supplies Darwin runtime ABI dependencies.
