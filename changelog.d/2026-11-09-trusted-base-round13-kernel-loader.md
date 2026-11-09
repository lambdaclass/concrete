Audit kernel-loaded ELF/shebang interpreters against the relevant closure;
reject writable executable ELF segments and executable/unspecified stacks. A real
custom PT_INTERP control demonstrates the loader is absent from strace yet must be
rejected for its exact identity. Refuse successful io_uring setup to close the
untraced async process-memory write route. Preserve execveat kind across resumption.
