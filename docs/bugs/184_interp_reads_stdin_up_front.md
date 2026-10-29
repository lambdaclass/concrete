# Bug 184: `--interp` read all of standard input before `main` ran

<!-- bug-ledger
summary: `--interp` read ALL of standard input before `main` ran (`readBinToEnd` in `Main.interpProgram`, since 728a2a98), so a program that never reads stdin, fed by a pipe that stays open, waited for the writer to close it while the binary returns at once. Stdin is now an `Interp.StdinSource` the `read` builtin pulls from when the program reads. Pinned by two `run_tests.sh` legs with stdin held open under `timeout 10`: `regress_184_interp_stdin_unread.con` (21) and the reading control `regress_184_interp_stdin_read.con` (131)
fixtures: regress_184_interp_stdin_unread.con regress_184_interp_stdin_read.con
class: 7 — the interpreter read all of stdin before `main`; the binary reads when the program does
silent: no
ruling: Violates: PASS-13@1
-->

**Status:** FIXED (branch `train3-interp-stdin`, for landing train 3)
**Found:** 2026-10-23, on landing train 3 (`wip/train3` at 949d79e9): a `--interp` run fed by a
pipe that stayed open did not return until the pipe closed.
**Introduced:** 728a2a98 (the interpreter-coverage pass, train 1 / train 1b: "project-mode
`--interp` with args and stdin"). Main never had it.
**Severity:** a hang, not wrong output. A program that never reads stdin waits for its writer to
close the pipe under `--interp`, and never returns if the writer never does; the binary returns at
once. A program reading from an endless producer (`yes | concrete p.con --interp`) never ran at
all. Output, exit status and the bytes a reading program sees were unaffected once input ended.

## Reproducer

```
fn main() -> Int { return 3; }
```

```
sleep 8 | { time concrete p.con --interp; }
```

(The `time` is inside the pipeline: timing the whole pipeline measures `sleep`, which the shell
waits for whatever `concrete` does.)

Train 3 printed `3` after 8 seconds; main (and the compiled binary) print it at once.

## Bisection

On the OVH server, `sleep 8 | { time concrete p.con --interp; }` with the program above (the
time of the `concrete` process alone, not of the pipeline):

| ref | sha | `--interp` elapsed |
|---|---|---|
| main | 7db43a33 | 22 ms |
| `wip/train2` | 2ee46f65 | 28 ms |
| parent of 728a2a98 | e24e7287 | 24 ms |
| 728a2a98 | 728a2a98 | 8003 ms |
| `wip/train1` | f92b8f32 | 8003 ms |
| `wip/train3` | 949d79e9 | 8004 ms |
| this fix | e6599672 | 26 ms |

`git log -S readBinToEnd` names the same commit: 728a2a98 is the only one that adds a read of
standard input to `interpProgram`.

## Structural cause

The interpreter is a pure function of the program and its inputs (`Interp.interpret`, running in
`ExceptT IErr (StateM IState)`), and it modelled standard input as one of those inputs: a
`ByteArray` the driver produced before the run, with `IO.getStdin >>= readBinToEnd` in
`Main.interpProgram`, and the `read` builtin consumed from that snapshot. That conflates the
descriptor with its contents. For the binary, reading stdin is an effect the program performs, at
the moment and to the extent it chooses (`read(0, buf, n)` returns what is available, up to `n`).
The snapshot performed that effect for every program, in full and before `main`, so the
interpreter's I/O semantics disagreed with the binary's on WHEN input is consumed and HOW MUCH of
it, even though they agreed on the bytes.

No differential gate could see it. Every gate that runs `--interp` with input feeds it a file or
`/dev/null` (`check_example_outputs.sh`), which is at end of input from the start, so the
snapshot and the lazy read are indistinguishable there.

## Structural owner

Class 7 in [STRUCTURAL_CLASSES.md](STRUCTURAL_CLASSES.md) (semantics implemented several times:
the interpreter disagreed with the binary), owner R-0496 (S7: the interpreter becomes Core's
semantics, which includes its host effects). The site that owns the rule now is
`Interp.StdinSource` (`Concrete/Interp/Interp.lean`): standard input is a source the run pulls
from, never a value supplied up front.

## Fix

- `Interp.StdinSource` replaces the `stdin : ByteArray` / `stdinPos` pair in `IState`. It is
  `bytes data pos` (input given up front, for the pure callers: report harnesses, tests) or
  `lazy pull`, where `pull n` reads at most `n` more bytes and returns the source to continue
  from. The run threads the source linearly and pulls each one at most once.
- The `read` builtin pulls `n` bytes from the source when the program calls `read(0, buf, n)`
  (`n = 0` pulls nothing), and returns the count it got. A failed host read is an interpreter
  failure with the host's message, never made-up data.
- `Main.interpProgram` no longer reads stdin. It passes `.lazy (hostStdinPull stdin 0)`, where
  `hostStdinPull` reads at most `min n 65536` bytes from the host stream when it is forced
  (`unsafeIO` behind an `implemented_by` opaque). That hides an effect behind a pure type, which
  the Lean compiler may in principle reorder, duplicate, delete or hoist. The argument that it
  does not, recorded at `hostStdinPull`: the result is always consumed; the call goes through a
  closure taken from the run's state, never one static term; each node carries the number of pulls
  before it (a read at end of input counts), so no two pulls have equal arguments and none is a
  closed term;
  `@[never_extract, noinline]` forbids extraction and inlining; and the run threads its state
  linearly, so each node is pulled at most once. An effectful interpreter monad would remove the
  argument, at the price of making the pure model-diff harness (`Interp.evalClosed`) effectful;
  that is left to R-0496, which owns the interpreter's host effects.
- Terminal input is still refused at the read, as before.

## Residual

A pull of `n` bytes is one `IO.FS.Stream.read`, which is `fread`: it returns when it has `n`
bytes or the input ends, where `read(2)` returns what is available. A program that asks for more
bytes than a still-open pipe has written waits for more under `--interp` and not natively.
`read_line` in `std.io` reads one byte at a time, so line-oriented programs are unaffected.
Before the fix that program also waited (for the pipe to close), so this is not a regression; a
host read with `read(2)` semantics needs a primitive the Lean runtime does not expose.

## Containment

Two `run_tests.sh` legs (`r184_leg`), each feeding the program a fifo whose writer holds it open
for 30 seconds under `timeout 10` (the writer is killed and reaped after the run), comparing
`--interp` with the binary:

- `tests/programs/regress_184_interp_stdin_unread.con` never reads stdin: both give 21 at once.
- `tests/programs/regress_184_interp_stdin_read.con` is the control that reads: the pipe carries
  `AB` and stays open; one `read(0, buf, 2)` returns both bytes at once, both give 131.

On the previous compiler both `--interp` legs time out (124); the binary legs pass.
