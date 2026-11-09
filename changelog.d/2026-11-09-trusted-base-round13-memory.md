Trusted-base D13: parse executable-memory arguments structurally, reject anonymous,
writable and shared executable mappings, bind file mappings to the pipeline role,
and share production/test strace flags and byte decoding. Trace architecture memory
calls and refuse unhandled events, permission transitions, memory mutation APIs and
process-memory files. Support annotated absolute execveat; reject unaudited empty-path
execution. Real-syscall controls assert the precise planted diagnostic. T1 is unchanged.
