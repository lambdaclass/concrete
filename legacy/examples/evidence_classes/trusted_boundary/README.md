# trusted_boundary

**Class:** a declared trust boundary — proof bypassed, but the trust is made
VISIBLE, never a silent gap.

`read_device_word` is `trusted` (the escape hatch for code Concrete cannot
prove, e.g. a thin FFI/raw-op wrapper). `concrete <src> --report proof-status`
shows it `trusted` (proof bypassed); `--report effects` shows `evidence:
trusted`.
