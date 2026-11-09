Gate D rejects unregistered native library mappings as well as unregistered
executables. All audited launches and scheduled trusted-base entries now inherit
one explicit environment allowlist, shared with the native helper, with a
lock-derived PATH and private configuration/cache roots. Real constructor-library
injection and clean-launch controls cover the loader bypass and inherited
configuration authority. T1 owner authentication remains fail-closed.
