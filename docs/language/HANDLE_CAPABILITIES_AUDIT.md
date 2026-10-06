# Handle Capabilities: Construction and Caller Audit

Status: audited design inventory (ROADMAP R-0484, slice 1), taken 2026-09-30 against
`bbba9ebf`, revised 2026-10-01 after review, and brought current 2026-10-06 (F6 carried out;
`_exit`, added by the bug 072 fix, audited in §3.2; every std binding now appears in exactly
one of §3.1–§3.3, checked by `check_descriptor_coverage.sh`). It records
std as it is today, measured against the rules in
[HANDLE_CAPABILITIES.md](HANDLE_CAPABILITIES.md). It is evidence for the design and a
work list for slice 3, not a claim that anything is fixed. Line numbers refer to that
commit.

Method: every std file was searched for foreign bindings, for construction of each
handle type (struct literals and constructors), and for calls into each binding
(following import renames such as `write as libc_write`). The handle modules
(`io.con`, `fs.con`, `net.con`, `process.con`) were read in full; the rest were read at
the call sites. Unique ownership is **not** inferred from linearity: each owning handle's
entry states the evidence for it.

## 1. Summary

- **Six handle types** carry external authority: `io.TextFile`, `fs.File`, `io.Writer`,
  `io.Reader`, `net.TcpListener`, `net.TcpStream`, plus `process.Child`, which holds a
  process id. All seven are non-`Copy` with private fields.
- **Owning handles with unique ownership established:** `TextFile`, `fs.File`,
  `TcpListener`, `TcpStream`, and file-backed `Writer`/`Reader`. Each is built only from a
  fresh OS result inside its own module, or by consuming another owning handle.
- **Owning handle whose uniqueness is NOT established:** `Child`. `Child::new(pid: i32)` is
  public and needs no capability, so any code can wrap any integer (F1).
- **Non-owning handles:** console `Writer`s (close is `console_noop`) and fixed-buffer
  `Writer`/`Reader` (caller-owned memory, no external authority).
- **Use requires the capability today** for `TextFile`, `fs.File`, `TcpListener`,
  `TcpStream` and `Child::wait`. It does **not** for `Writer` and `Reader`: that is the
  hole (F2).
- **65 foreign bindings** in std at the 2026-09-30 snapshot: `libc.con` 50, `math.con` 9,
  `alloc.con` 4, `args.con` 2, all `trusted extern`, 8 never called (F6). **Today
  (2026-10-06): 58** — `libc.con` 43 after F6 removed the eight and the bug 072 fix added
  `_exit`; since slice 3 they are no longer all `trusted extern` (R4).
- **Two wrappers exercise authority they do not declare:** `net.is_darwin` (`uname`) and
  `args::count` (`__concrete_get_argc`) (F4, F5).
- **`fork` duplicates owning handles and buffered output** across processes (F9), and
  `spawn`'s exec-failure path flushes copied buffers (F10).
- **Remaining assumptions** (reported, not proved): foreign effect declarations; which
  descriptor each binding receives; excluded `errno`/floating-point effects (D4).
- **Conditions the supported runtime profile requires** (§7): no signal handlers that
  run before an abort completes, and a single-threaded process at `fork`. Listing them
  does not establish that a deployment satisfies them.

## 2. Handles

Legend: **origin** is where the descriptor comes from; **cap** is its logical capability
(design §2); **use today** is what using it requires now; **required** is what the design
requires.

### 2.1 `io.TextFile` — owning, unique

- **Origin:** `fopen` through `fopen_cstr_io` (`io.con:6`), in `TextFile::create`
  (`io.con:30`, mode `"w"`) and `TextFile::open` (`io.con:41`). Both require
  `with(File, Alloc)`. **Cap:** `File`.
- **Construction sites:** `io.con:37`, `io.con:46` only, each from a non-null `fopen`
  result. No other literal; the field `ptr` is private.
- **Unique ownership, evidence:** each `fopen` returns a new `FILE*`; the value is placed
  in exactly one `TextFile`; the type is non-`Copy`; no function returns or accepts the
  raw pointer outside the module.
- **Ownership transfer:** consumed by `writer_from_file` (`io.con:148`) or
  `reader_from_file` (`io.con:253`), which destructure it and move the pointer into the
  new handle.
- **Use today:** `write`, `read_byte`, `flush`, `close` all require `File`. Matches the
  design.
- **Close:** `close(self)` calls `fclose` (`io.con:67`).
- **Failure paths:** a null `fopen` returns `Err(OpenFailed)` with no handle created, and
  `create` drops its `mode` string first. `write` ignores `fwrite`'s count and `close`
  ignores `fclose`'s result (F7).

### 2.2 `fs.File` — owning, unique

- **Origin:** `fopen` through `fopen_cstr` (`fs.con:16`), in `File::open` (`fs.con:26`) and
  `File::create` (`fs.con:35`), both `with(File, Alloc)`. **Cap:** `File`.
- **Construction sites:** `fs.con:31`, `fs.con:42` only. Field `handle` private.
- **Unique ownership, evidence:** as for `TextFile`. `fs.File` and `io.TextFile` are two
  separate owning types over `FILE*`; neither can be converted into the other.
- **Use today:** `seek`, `tell`, `close` require `File`; `write_bytes`, `read_bytes`
  require `File, Unsafe` (raw buffer pointers). Matches the design.
- **Close:** `close(self)` calls `fclose` (`fs.con:62`), ignoring its result (F7).
- **Whole-file helpers without a handle:** `read_file`, `write_file`, `append_file`,
  `file_exists`, `read_to_string` (`fs.con:68-160`) open, use and close a local `FILE*`
  inside one call, all `with(File, Alloc)`. Every success path calls `fclose`; the
  open-failure path creates nothing to close. `read_file` and `read_to_string` do not
  check `ftell` for `-1` (F8).

### 2.3 `io.Writer`

One type, four sinks. Fields (`ctx`, `write_fn`, `flush_fn`, `close_fn`) are private;
construction happens only in `io.con`.

| sink | constructor | acquisition cap | owning? | close | cap required under the design |
|---|---|---|---|---|---|
| file | `writer_from_file(f: TextFile)` (`io.con:148`) | `File` | yes, unique: consumes the `TextFile` | `file_close`: `fflush` + `fclose` | `Writer<File>` |
| stdout | `console_writer()` (`io.con:169`) | `Console` | no | `console_noop` | `Writer<Console>` |
| stderr | `console_error_writer()` (`io.con:178`) | `Console` | no | `console_noop` | `Writer<Console>` |
| fixed buffer | `fixed_writer(state: *mut FixedSinkState)` (`io.con:207`) | `Unsafe` | no: caller owns the memory | `console_noop` | `Writer<{}>` |

- **Use today:** `write`, `write_str`, `flush` and `close` require **nothing**;
  `write_raw` requires only `Unsafe`. This is the hole (F2).
- **Aliasing:** two console writers alias the standard streams; allowed, since neither
  owns them (design §10). A file writer cannot be aliased: it is built by consuming the
  only `TextFile`.
- **Descriptors used by the sinks:** `console_write` passes the literal `1` and
  `console_err_write` the literal `2` to `write`. `file_write` passes the `FILE*` from the
  consumed `TextFile`. These are the only descriptors any `Writer` touches.
- **Failure paths:** `file_close` flushes, then closes, and reports either failure as
  `CloseFailed`. `console_write` reports short writes; `console_err_write` ignores its
  result entirely (F7). `fixed_write` returns `BufferFull` on overflow.
- **Fixed-buffer note:** `FixedSinkState` is `Copy` with public fields, so the caller can
  keep copies while the writer mutates through the pointer. That is part of the
  memory-safety obligation taken on by calling `fixed_writer` with `Unsafe`, not an
  effect question.

### 2.4 `io.Reader`

| source | constructor | acquisition cap | owning? | close | cap required under the design |
|---|---|---|---|---|---|
| file | `reader_from_file(f: TextFile)` (`io.con:253`) | `File` | yes, unique: consumes the `TextFile` | `file_close` | `Reader<File>` |
| fixed buffer | `fixed_reader(state: *mut FixedSrcState)` (`io.con:311`) | `Unsafe` | no | `console_noop` | `Reader<{}>` |

- **Use today:** `read` requires only `Unsafe` (raw buffer pointer); `close` requires
  nothing. `read_all(r: &Reader)` (`io.con:263`) requires only `Alloc`, and it reads a
  file without declaring `File` (F2).
- **Failure paths:** `file_read` distinguishes EOF from a read error with `ferror`.
  `read_all` treats an error as end of input and returns partial data, by design.
- There is no console `Reader`; stdin is read only by `read_line` (§2.8).

### 2.5 `net.TcpListener` — owning, unique

- **Origin:** `socket(2, 1, 0)` in `TcpListener::bind` (`net.con:87`),
  `with(Alloc, Network)`. **Cap:** `Network`.
- **Construction site:** `net.con:137` only, after `setsockopt`, `bind` and `listen`
  succeed. Field `fd` private.
- **Unique ownership, evidence:** `socket` returns a new descriptor; it is placed in one
  listener; non-`Copy`; no API exposes or accepts the raw `fd`.
- **Use today:** `accept` and `close` require `Network`. Matches the design.
- **Failure paths:** every failure after `socket` succeeds calls `close(fd)`: `setsockopt`
  (`net.con:100`), address (`net.con:117`), `bind` (`net.con:124`), `listen`
  (`net.con:131`). The 16-byte `sockaddr` buffer is freed on every path that allocated
  it. `close(self)` ignores `close`'s result (F7).

### 2.6 `net.TcpStream` — owning, unique

- **Origin:** `socket` in `TcpStream::connect` (`net.con:161`), or `accept` in
  `TcpListener::accept` (`net.con:141`). Both require `Network`. **Cap:** `Network`.
- **Construction sites:** `net.con:146` (accepted), `net.con:192` (connected). Field `fd`
  private.
- **Unique ownership, evidence:** each `socket`/`accept` returns a new descriptor held by
  exactly one stream; non-`Copy`; no raw-`fd` API.
- **Use today:** `write`, `read`, `write_all` require `Unsafe, Network` (raw pointers);
  `read_all` and `close` require `Network`. Matches the design.
- **Failure paths:** `connect` closes the descriptor on address and connect failure and
  frees the `sockaddr` buffer on both. `close(self)` ignores `close`'s result (F7).

### 2.7 `process.Child` — owning, uniqueness NOT established

- **Origin:** `fork` in `spawn` (`process.con:91`), `with(Unsafe, Process, Alloc)`.
  **Cap:** `Process`.
- **Construction sites:** `process.con:109` (in `spawn`) **and `Child::new(pid: i32)`
  (`process.con:60`), which is public and requires no capability.** std's own tests use
  it to forge handles: `Child::new(99999)` (`process.con:116`), `Child::new(-1)`
  (`process.con:267`), and `Child::new(child_pid)` over a raw pid from `process_fork`
  (`process.con:149`).
- **Unique ownership: not established.** `Child::new` can wrap any integer, including one
  already held by another `Child` (F1).
- **Use today:** `wait(self)` requires `Process`; `pid(&self)` requires nothing (it reads
  a number).
- **Raw-pid APIs:** `process_fork()` returns `ForkResult::Parent { child_pid: i32 }`, a raw
  pid rather than a `Child` (F1). `process_kill(pid: i32, sig: i32)` takes any pid
  `with(Process)`: ambient `Process` authority over any signalable process, not a handle
  operation.
- **Reuse:** a process id can be reused by the OS once the child is waited on. `wait`
  consumes the `Child`, so no handle can reach the pid afterwards; a forged `Child` can.

### 2.8 Standard streams without a handle

`print`, `println`, `eprint`, `eprintln` (`io.con:316-347`) call `write` on 1 or 2 and
declare `Console`. `read_line` (`io.con:349`) calls `read` on 0 and declares
`Console, Alloc`. Correct under the design; stdin is `Console` (logical authority over
the standard streams).

## 3. Foreign bindings

Proposed effect declaration under R3, and classification under R4 ("`trusted extern`
only if safe for every argument its types permit, with no undeclared effects"). Callers
are std functions; test-only callers are omitted.

Each operation is classified by its **actual safety obligations**, not by the shape of
its arguments:

- Taking a scalar descriptor or process id is **not** by itself grounds for `Unsafe`.
  Naming or querying a resource is a different act from invalidating one.
- An operation that can **invalidate an owned resource** — release a descriptor another
  handle owns (`close`), or reap a child another handle owns (`waitpid`) — needs
  protection: it is plain `extern`, called only from the owning handle's trusted code.
- An operation whose safety depends on **memory validity** (pointer arguments or pointer
  results) is plain `extern`.
- Having no arguments does not make an operation safe. `fork` is audited separately
  (§3.4).
- Misdirected I/O on a descriptor named by an arbitrary integer is not a memory-safety
  question. It is the audited caller restriction of design §5: the capability is still
  declared, and which descriptor is passed is an assumption.

### 3.1 Descriptor and I/O bindings

| binding | proposed effect | R4 | std callers |
|---|---|---|---|
| `fopen` | `File` | plain `extern` (pointer args) | `fs.fopen_cstr`, `io.fopen_cstr_io` |
| `fclose` | `File` | plain | `fs.close`, whole-file helpers; `io.TextFile::close`, `io.file_close` |
| `fflush` | `File` | plain | `io.file_flush`, `io.file_close`, `io.TextFile::flush` |
| `ferror` | `File` | plain | `io.file_read` |
| `fread` | `File` | plain | `fs.read_bytes`, `fs.read_file`, `fs.read_to_string`; `io.file_read`, `io.read_byte` |
| `fwrite` | `File` | plain | `fs.write_bytes`, `fs.write_file`, `fs.append_file`; `io.file_write`, `io.TextFile::write` |
| `fseek`, `ftell` | `File` | plain | `fs.seek`, `fs.tell`, `fs.read_file`, `fs.read_to_string` |
| `write` | per effect under A: `write_console` (`Console`) | plain (buffer pointer) | `io.console_write`, `console_err_write`, `print`, `println`, `eprint`, `eprintln` — all on fds 1 and 2 |
| `read` | per effect under A: `read_console` (`Console`) | plain (buffer pointer) | `io.read_line` — fd 0 |
| `socket` | `Network` | `trusted extern` (scalar, creates a descriptor) | `net.TcpListener::bind`, `TcpStream::connect` |
| `listen` | `Network` | `trusted extern` (scalar; changes socket state, invalidates nothing) | `net.TcpListener::bind` |
| `bind`, `accept`, `connect`, `setsockopt` | `Network` | plain (pointer arguments) | `net.TcpListener::bind`, `TcpListener::accept`, `TcpStream::connect` |
| `send`, `recv` | `Network` | plain (buffer pointers) | `net.TcpStream::write`, `write_all`, `read`, `read_all` |
| `close` | `Network` today (every std caller passes a socket) | plain (invalidates a descriptor) | `net.bind`/`connect` failure paths, `TcpListener::close`, `TcpStream::close` |

Under encoding A, `write` and `read` are only ever called with the standard streams, so
the first cut needs just the console bindings. `close` is only ever called on sockets;
if a file descriptor is ever closed with it, it needs a second binding.

### 3.2 Other effectful bindings

| binding | proposed effect | R4 | std callers |
|---|---|---|---|
| `getenv`, `setenv`, `unsetenv` | `Env` | plain | `env.get`, `env.set`, `env.unset` |
| `__concrete_get_argv` | `Env` | plain (returns a pointer whose validity depends on the index) | `args::get` |
| `__concrete_get_argc` | `Env` (see F5) | `trusted extern` (no arguments, effect declared) | `args::count` |
| `uname` | `Env` (see F4) | plain (pointer argument) | `net.is_darwin` |
| `time`, `clock_gettime`, `nanosleep` | `Time` | plain (pointer arguments) | `time.unix_timestamp`, `time.now`, `time.sleep` |
| `rand`, `srand` | `Random` | `trusted extern` (scalar, effect declared) | `rand.random_int`, `random_range`, `seed` |
| `getpid` | `Process` | `trusted extern` (no arguments, queries only, effect declared) | `process.process_getpid` |
| `exit` | `Process` | `trusted extern` (scalar, effect declared); see F10 for the child path | `process.process_exit`, `spawn` |
| `_exit` (added 2026-10-04, bug 072) | `Process` | `trusted extern` (scalar, effect declared); takes no descriptor; terminates without flushing stdio or running atexit handlers | `process.spawn`, only in the forked child when `execvp` fails |
| `kill` | `Process` | `trusted extern` (scalar, effect declared): it signals but does not invalidate a handle's memory; targeting any pid is ambient `Process` authority, see special values in F1 | `process.process_kill` |
| `waitpid` | `Process` | plain (reaps a child another handle may own; status pointer) | `process.Child::wait` |
| `fork` | `Process` | **plain**, audited separately (§3.4) | `process.process_fork`, `spawn` |
| `execvp` | `Process` | plain (pointer arguments) | `process.spawn` |
| `malloc`, `realloc`, `free` | `Alloc` | plain (pointer args or validity-dependent) | `alloc.heap_new`, `grow`, `dealloc` (`alloc.con`'s own copies); `sha256.hash_raw` (`libc.con`'s copies) |
| `abort` | language-defined failure only, no added capability (see D3) | plain; audited restriction: called only on allocation failure | `alloc.heap_new`, `alloc.grow` (out-of-memory) |

### 3.3 Memory and pure bindings

| binding | proposed effect | R4 | std callers |
|---|---|---|---|
| `memcpy`, `memset`, `memcmp`, `strlen` | none | **plain `extern`**: safety depends on pointer validity the types cannot express | `string.*`, `bitset.*`, `map.*`, `args::get`, `env::get` |
| `htons` | none | `trusted extern` (scalar, total) | `net.fill_sockaddr_in` |
| `inet_pton` | none | plain (pointer args) | `net.fill_sockaddr_in` |
| `sqrt`, `sin`, `cos`, `tan`, `pow`, `log`, `exp`, `floor`, `ceil` | no external-authority capability; NOT effect-free (see D4) | `trusted extern` (scalar, total) | public, no std callers |

### 3.4 `fork`, audited separately

`fork` takes no arguments, but it is not a safe primitive:

- **Multithreaded parents.** After `fork` in a multithreaded process, the child may call
  only async-signal-safe functions until it calls `exec` (POSIX). std has no threads, so
  this holds today only because nothing in a Concrete program creates them; foreign code
  could. Recorded as an assumption.
- **Owning handles are duplicated.** Every descriptor is copied into the child, so after
  `process_fork` both processes hold every `TcpStream`, `TcpListener` and `FILE*`. The
  unique-ownership evidence of §2 holds *within one process*; `fork` breaks it across
  two (F9).
- **Buffered output is duplicated.** Unflushed `FILE*` buffers (`TextFile`, `fs.File`,
  file writers) are copied into the child and can be written twice (F9, F10).
- `spawn` (`process.con:91`) is the safe pattern: the child calls only `execvp` and then
  terminates. `process_fork` is not: the child returns into arbitrary Concrete code.

Classification: plain `extern`, `Process`, with these as named assumptions in reports.

### 3.5 Never called — removed (F6, carried out 2026-10-06)

`realloc` (libc copy; `alloc.con` has its own), `puts`, `fdopen`, `raise`, `putchar`,
`snprintf`, `strtol`, `htonl`. Removing `fdopen` also removes the only bound path from a
raw descriptor to a `FILE*`.

## 4. Findings

Defects are fixed in slice 3 unless noted. None is fixed by this audit.

- **F1. `Child` can be forged — FIXED 2026-10-04.** `Child::new` is removed, the field stays
  private, and only `spawn` constructs a `Child` (pid > 0, in its parent branch). Refusals of
  a struct literal (E0297) and of `Child::new` (E0106), with a `spawn`-then-`wait` positive
  control, are pinned in `check_construction_rights.sh` and fail with the constructor
  restored. The two std tests that forged handles were removed with it. Original finding: `pub fn Child::new(pid: i32) -> Child` needs no
  capability and accepts any integer, so uniqueness and origin are not established.
  `process_fork` returns a raw pid instead of a `Child`. Fix: make `Child::new` private
  (or `Unsafe`-gated and reported as a conversion, design R5), and have `process_fork`
  return a `Child` in its parent branch. The tests that forge handles become tests of
  the refusal. A capability requirement alone is not the repair: the repair must
  establish that the pid **denotes a child of this process that no other `Child` owns**,
  which only `spawn`/`process_fork` can know. It must also exclude the special values a
  raw pid can carry: `0` and negative values name process groups for `kill`, `-1` means
  "any child" for `waitpid` and "every process you may signal" for `kill`. A `Child`
  holds a pid greater than zero, and `wait` on it waits for exactly that child.
- **F2. `Writer`/`Reader` operations need no capability.** The hole R-0484 closes:
  `Writer::write`/`write_str`/`flush`/`close`, `Reader::close` and `read_all` require
  nothing; `write_raw`/`read` require only `Unsafe`. Fixed by `Writer<C>`/`Reader<C>`
  (slice 4).
- **F3. The sinks declare nothing.** `console_write`, `console_err_write`, `file_write`,
  `file_flush`, `file_close`, `file_read` perform I/O with empty headers. Under R2 each
  declares its capability, which is what lets `Writer<C>` type-check (slice 3).
- **F4. `net.is_darwin` calls `uname` declaring only `Alloc`.** Its callers hold
  `Network`, so no public function under-declares today, but the private helper does.
  Classification decision D1.
- **F5. `args::count` reads `argc` declaring nothing, while `args::get` requires `Env`.**
  Either `count` requires `Env`, or the argument count is declared not to be authority.
  Classification decision D2.
- **F6. Eight bindings are never called** (§3.5). Remove them. **Done 2026-10-06**: removed
  from `std/src/libc.con`; until then the runtime-profile statement in §7 that "the unused
  `raise` is removed" was not true of the code.
- **F7. Error results ignored (outside R-0484; recorded for the error-honesty work).**
  `console_err_write` ignores `write`'s result, while `console_write` checks it.
  `TextFile::write` ignores `fwrite`'s count. `TextFile::close`, `fs.File::close`,
  `TcpListener::close` and `TcpStream::close` ignore their close results.
- **F8. `ftell` failure is unchecked (outside R-0484).** In `fs.read_file` and
  `fs.read_to_string`, `ftell` returns `-1` for a stream that cannot seek (a pipe, a
  directory), and `size as u64` becomes an enormous capacity.
- **F9. `process_fork` duplicates owning handles and buffered output — FIXED 2026-10-04 by
  removal** (the first option below): `process_fork` and `ForkResult` are no longer public
  surface, nothing in std, examples or tests called them, and `spawn` is the supported
  fork-then-exec path. `check_construction_rights.sh` pins that the import is refused (E0111)
  and fails with the function restored. A future `fork` needs a stated ownership and
  runtime-profile contract first (R-0484 second step). **What removal does NOT do:** it closes
  the public ownership-duplication path, not forking's runtime conditions, which `spawn` still
  rests on and which are stated at `spawn` in `std/src/process.con`: the supported profile is a
  process that is single-threaded at the fork (Concrete starts no threads; a program in which
  foreign code has started threads is outside the supported profile), and between fork and
  exec the child branch runs no arbitrary user callbacks, allocation or stdio — only `execvp`,
  then `_exit`. Descriptors are inherited across exec because std sets no close-on-exec; a
  close-on-exec policy is a separate follow-up. These are stated conditions, not checked
  properties. Runtime regression for the supported path:
  `tests/regressions/spawn_exit/spawn_wait_status` (positive pid, `wait` decodes 0 and 1),
  run by `check_spawn_exit.sh`. Original finding: After it returns,
  parent and child both hold every owning handle, and unflushed `FILE*` buffers exist
  twice. Unique ownership (§2) is a per-process property. Options for slice 3: restrict
  `fork` to `spawn`'s fork-then-exec pattern and remove `process_fork` from the public
  surface, or document the duplication and flush before forking.
- **F10. `spawn`'s exec-failure path called `exit(127)` in the child — bug 072, FIXED
  2026-10-01.** `exit` runs `atexit` handlers and flushes stdio buffers copied from the
  parent, so buffered output was written twice (measured: `XX` for one byte). The child
  now calls `_exit`. Regression: `tests/regressions/spawn_exit/exec_failure_no_double_flush`,
  run by `check_spawn_exit.sh`, shown to fail with the fix reverted. See
  [docs/bugs/072](../bugs/072_spawn_exec_failure_flushes_parent_buffers.md).

## 5. Classification decisions

- **D1. `uname` → `Env`: accepted 2026-10-01.** Runtime system identification is
  logical environment authority. Removing the runtime query through target-specific
  compilation is a possible later improvement.
- **D2. `argc`/`argv` → `Env`: accepted 2026-10-01.** Counting arguments observes the
  same environment as reading them, so `args::count` requires `Env` like `args::get`.
- **D3. `abort`: keep the existing distinction (revised 2026-10-01).**
  `FAILURE_STRATEGY.md` separates *user-requested* abort — the `abort()` intrinsic, which
  requires `Process` — from *language-defined* failure (out-of-memory, checked
  arithmetic, bounds, std allocator preconditions), which aborts implicitly. This audit
  keeps that rule and does not propose changing it. std's `abort` binding
  (`alloc.con:7`) is used only on allocation failure, a language-defined failure, so it
  adds no capability beyond its callers' `Alloc`; that it is called *only* on failure
  paths is an audited restriction. libc `abort` raises `SIGABRT`, and a signal handler
  can run code before termination: Concrete installs no handlers (std binds no
  `sigaction` or `signal`, and the unused `raise` is removed, F6), but foreign code
  could. Recorded as an assumption, not treated as an inert primitive.
- **D4. Math bindings: no external-authority capability, but not effect-free (revised
  2026-10-01).** `sqrt`, `log`, `pow` and the others may write `errno` and read or write
  the floating-point environment (rounding mode, exception flags). Foreign code can
  observe both, so "Concrete never reads them" is not enough. These effects are
  **excluded from the capability classification** and recorded as such in the
  declaration. This decision grants no mathematical purity and no proof eligibility:
  whether these functions may appear in contracts or proofs is decided by the contract
  and proof rules, not by their empty capability set.

## 6. Acceptance cases this audit adds

Numbered after the design doc's cases (§8 there).

| # | case | expected |
|---|---|---|
| 17 | calling `Child::new` (or any raw-pid → `Child` conversion) outside `process` without `Unsafe` | refused (F1) |
| 18 | `process_fork` parent branch | yields a `Child`, not a raw pid |
| 19 | `Reader::read_all` over a file reader | requires `File` (F2) |
| 20 | a trusted sink performing I/O with an empty header | refused (F3, R2) |
| 21 | `args::count` without `Env` | refused (D2) |
| 23 | a `Child` from a pid of `0`, a negative pid, or a pid that is not this process's unowned child | not constructible |
| 24 | user-requested `abort()` without `Process` | refused (unchanged rule, D3) |
| 25 | `spawn` exec failure with unflushed buffered output | output written once (bug 072; `check_spawn_exit.sh`, in place) |
| 22 | binding `fdopen` or `dup`/`dup2` | refused until the second step (design §10) |

## 7. Conditions the supported runtime profile requires

These are not facts about std; they are conditions a deployment must meet for the
audit's conclusions to hold. Each states its scope and how it is enforced, or that it
is not.

| condition | scope | why it matters | enforcement today |
|---|---|---|---|
| No signal handler runs code before an abort completes | the hosted profile; every abort, language-defined or user-requested | libc `abort` raises `SIGABRT`, and an installed handler runs before termination (D3) | std installs none and binds no `sigaction`/`signal`, and the unused `raise` is removed (F6). **Not enforced** against foreign code linked into the program, which can install handlers. |
| The process is single-threaded at `fork` | every `fork`: `spawn` and `process_fork` | after `fork` in a multithreaded process, the child may call only async-signal-safe functions until `exec` (§3.4) | Concrete creates no threads and std binds no thread API. **Not enforced** against foreign code, which can create threads. `process_fork` additionally runs arbitrary code in the child (F9). |

Reports should name both conditions next to any conclusion that depends on them, and a
profile that cannot meet them (for example one that links threaded foreign libraries)
must not claim those conclusions.
