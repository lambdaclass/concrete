# Golden-output oracles

An expected output (a golden) counts as evidence only when something other than the compiler
under test agrees with it. A golden recorded from the binary's own output proves only that the
output does not change: bug 121's `conlog` golden (`9 entries, 2 errors, 0 unparseable`) blessed a
miscompiled answer, because the interpreter could not run `conlog` and nothing else checked the
number ([bug 121](../bugs/121_enum_llvm_layout_disagrees.md)).

Every `@run` rule of [`scripts/tests/example_manifest.txt`](../../scripts/tests/example_manifest.txt)
therefore declares its oracle, and
[`scripts/tests/check_example_outputs.sh`](../../scripts/tests/check_example_outputs.sh) enforces it:

| oracle | meaning | enforced by the gate |
|---|---|---|
| `interp` | the interpreter runs the case and prints the same stdout and main's value | requires `interp=yes`; the agreement is checked on every run |
| `spec:<path>[#anchor]` | derived without the compiler: by hand from the program's specification, or by an independent reference tool, with the derivation recorded at the pointer | the file exists and the anchor names one of its headings |
| `unverified:<reason>` | no independent check yet | allowed only when the interpreter does not run the case; the gate lists every one and prints the count |

A rule with no oracle, an unknown oracle, `oracle=interp` without `interp=yes`, an unverified oracle
on a case the interpreter runs, or a spec pointer that does not resolve fails the gate (each has a
negative control). The unverified count is the size of the gap: it shrinks when the interpreter
learns to run a case (exit 2c's interpreter-gap repairs: projects importing `std` modules, E0110;
argv; the missing builtins) or when a derivation is recorded here. A new golden that is neither
interpreter-checked nor derived must say why in its `unverified:` reason.

A derivation records how the expected output was obtained independently: the reference command
and its output, or the hand computation from the program's stated rule. Re-derive it whenever the
expected output or the input fixture changes.

## Derivations

### base64_cli

`encode hello` and `decode aGVsbG8gd29ybGQ=` (RFC 4648 standard alphabet, padded). Reference: the
coreutils/BSD `base64` tool.

- `printf hello | base64` prints `aGVsbG8=`; the golden is `aGVsbG8=` with no trailing newline.
- `echo aGVsbG8gd29ybGQ= | base64 -d` prints `hello world` with no trailing newline; so does the golden.

### verify

`testdata/hello.txt`, SHA-256. Reference: `shasum -a 256 testdata/hello.txt` (or `sha256sum`)
prints `a948904f2f0f479b8f8197694b30184b0d2ed1c1cd2a1ec0fb85d299a192a447  testdata/hello.txt`, which
is the `hash` golden byte for byte (the same two-space format). `match` passes that digest and must
report `OK`, exit 0; `mismatch` passes 64 zeros and must report `FAILED` with the expected and the
actual digest, exit 1. The digest and the verdicts are independent of the compiler; the line layout
of `match` and `mismatch` follows the program's usage comment (`conhash <file> [expected_hex]`).

### grep

`testdata/poem.txt`. Reference: POSIX `grep`.

- `grep -n quick testdata/poem.txt` prints `1:The quick brown fox` (`line_numbers`).
- `grep -c -i quick testdata/poem.txt` prints `2` (`count_ignore_case`).
- `grep -v the testdata/poem.txt` prints the three lines of the `invert` golden.

### hexdump

`testdata/sample.bin`. Reference: `xxd` (the example prints xxd's layout).

- `xxd testdata/sample.bin` is byte-identical to the `full` golden.
- `xxd -s 32 -l 12 testdata/sample.bin` is byte-identical to the `skip_len` golden.

### wordfreq

`testdata/words.txt`: whitespace-separated words, punctuation kept, counted and sorted bytewise.
Reference:
`tr -s ' \n\t' '\n' < testdata/words.txt | grep -v '^$' | LC_ALL=C sort | uniq -c | awk '{print $2, $1}'`
is byte-identical to the `words` golden.

### tar_list

Reference: `tar tf`. `tar tf testdata/sample.tar` lists `docs/`, `docs/readme.txt`, `data.bin`,
`empty.txt`, the `sample` golden. `tar tf testdata/truncated.tar` lists `docs/` and then reports
the truncation; the example prints the members read before the truncation (`docs/`, the
`truncated` golden) and exits 1.

### conlog

`logs/sample.log`, by hand from the program's rule: an error is an `ERROR`-level line or any 4xx/5xx
status. The file has 9 parseable lines; the `ERROR 500` and `ERROR 503` lines and the two
`WARN 404` lines are errors; nothing is unparseable. So the `sample` golden is
`9 entries, 4 errors, 0 unparseable`. This is the derivation that corrected bug 121's golden.
