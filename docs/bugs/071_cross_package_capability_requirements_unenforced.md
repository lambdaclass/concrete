# Bug 071 — a capability requirement declared in a dependency does not bind on its caller

<!-- bug-ledger
summary: a capability declared in a dependency did not bind on its caller: a function declaring nothing read `$HOME` through `std.env.get`. Only hardcoded intrinsics were enforced across a package boundary
coverage: a capability declared in a DEPENDENCY did not bind on its caller: a function declaring nothing called std.env.get and the program printed $HOME. ALL FOUR CALL FORMS now bind, and they are enumerated because a headline that did not name them is exactly how the last one stayed open: imported free function, explicitly imported receiver method (RawCursor::read_u8), associated call (TcpStream::connect), and PRELUDE receiver method (String::drop). The prelude case survived two rounds because importedFnCaps was built by walking m.imports and String needs no import; check_cap_sibling_module.sh asserted the acceptance as a KNOWN HOLE (a passing check) while check_cross_package_caps.sh passed claiming methods bind -- two green gates, incompatible headlines. Public std Unsafe fell 154->21 via per-method trusted. Gated by check_cross_package_caps.sh (18/0) and check_cap_sibling_module.sh (17/0, now a rejection test with a positive control); see docs/bugs/071 (2026-09-21 (sinks+Alloc) / 2026-09-25 (Unsafe) / 2026-09-25 (prelude receivers))
class: 6
ruling: Violates: TRUST-4@1
-->

**Status:** Fixed — stage 1 2026-09-21 (sinks + `Alloc`), stage 2 2026-09-25 (`Unsafe`
uniformly), stage 3 2026-09-25 (prelude receiver methods).

**ENUMERATED, because a headline that does not name its call forms is how this stayed
open.** All four bind: imported free function · explicitly imported receiver method
(`RawCursor::read_u8`) · associated call (`TcpStream::connect`) · **prelude receiver
method** (`String::drop`). Gated by `check_cross_package_caps.sh` (18/0) for the first
three and `check_cap_sibling_module.sh` (17/0) for the fourth.

**Discovered:** 2026-09-20, while sizing the cross-package METHOD hole left open by
the sibling-submodule repair (R-0484, `c3fabe25`). The method case turned out to be
a subset: the boundary is the PACKAGE, and free functions cross it too.
**Severity:** authority escape, reachable from ordinary user code against the
shipping standard library. Not a rejected-valid-program; a wrongly-accepted one.

## Symptom

A function that declares no capabilities at all, and is not `trusted`, may call a
dependency function that declares `with(...)`. It reads the environment:

```con
mod sinkprobe {
    import std.env.{get};
    import std.io.{println};

    // Declares NOTHING. Not trusted. `std.env.get` is with(Alloc, Env, Unsafe).
    fn steal_env(k: &String) -> String {
        let r: Option<String> = get(k);
        match r {
            Option::Some { value } => { return value; },
            Option::None {} => { let e: String = "(unset)"; return e; }
        }
    }

    fn main() with(Console, Alloc) -> i64 {
        let k: String = "HOME";
        let v: String = steal_env(&k);
        println(&v);
        k.drop(); v.drop();
        return 0;
    }
}
```

Checks clean, builds clean, and prints the value of `$HOME`.

## Measured extent

Each row is a capability-free, non-`trusted` caller invoking the named dependency
function:

| callee | kind | declares | verdict |
|---|---|---|---|
| `std.io.println` | free fn | `Console, Alloc` | refused (E0520) |
| `std.env.get` | free fn | `Alloc, Env, Unsafe` | **accepted** |
| `std.fs.file_exists` | free fn | `Alloc, File, Unsafe` | **accepted** |
| `std.time.sleep` | free fn | `Time, Unsafe, Alloc` | **accepted** |
| `std.time.unix_timestamp` | free fn | `Time, Unsafe` | **accepted** |
| `String::clone` / `String::drop` | method | `Alloc, Unsafe` | **accepted** |

## Why `println` is the outlier, and why that is the bad news

`println` is not enforced because of its `std` declaration. It is enforced because
it is an INTRINSIC carrying a hardcoded capability
(`Concrete/Resolve/Intrinsic.lean:210` maps `.print | .println => some "Console"`),
reached through `lookupBuiltinCap`'s fallback in `CoreCheck.lookupFnCaps`.

So the enforced set across a package boundary is exactly the hardcoded intrinsic
table. **Every capability a dependency actually declares is decorative to its
consumers.** `with(File)`, `with(Env)`, `with(Time)`, `with(Network)`,
`with(Process)` and `with(Random)` do not bind on an importing caller.

That makes the checker's own E0520 hint false for every dependency:

> capabilities are part of a function's contract: a caller may only invoke effects
> it has itself declared, so effects stay visible at every call site

## Mechanism

The same shape as the sibling-submodule defect one boundary out, and the same
entry in [ABSENCE_IS_NOT_A_FACT.md](../project/ABSENCE_IS_NOT_A_FACT.md).
`ccCheckModule` builds `fnSigs` from the compilation unit's own module tree.
A dependency package is not in that tree, so `lookupFnCaps` finds nothing and

```lean
| none => pure ()  -- builtin/extern: no recorded capability set
```

reads "I have no signature for this name" as "this call requires nothing".

`FileSummary` already computes a `capSet` for every imported function
(`Concrete/Resolve/FileSummary.lean`), so the fact exists and is simply not
reaching the checker. This is a plumbing repair, not a new analysis.

## Controls that rule out the easy misreadings

- **Not a regression from the sibling-submodule fix.** `println` is still refused
  after it, and the accepted calls above were accepted before it too.
- **Not "the module is absent from the unit".** A program containing BOTH a
  legitimate `with(Alloc, File, Unsafe)` caller of `fs::file_exists` and a
  capability-free one still accepts the capability-free call.
- **Not a method-dispatch quirk.** Four of the six accepted callees are free
  functions.

## Fix

Merge dependency signatures into the checking environment the way
`collectAllFnSigs` merged the local module tree, covering every imported callable —
free functions, methods, aliases, generics and trusted declarations alike. Where a
requirement cannot be resolved, the result must be *unknown/refuse*, never
*requires nothing*.


## Repair, stage 1 (2026-09-21) — deliberately incomplete

`CModule.importedFnCaps` carries each imported callable's requirement, keyed by the local
spelling the call site uses; Elab joins the import list to the dependency `FileSummary`;
CoreCheck consults it on the same `decideCall` path as local and sibling calls. Six
escapes now refuse, including the cross-package METHOD case (`TcpStream::connect`), and
`check_cross_package_caps.sh` (16/0) carries the refusals, the accept-and-RUN control,
the capability-free and capability-polymorphic controls, and two compiler mutations —
severing the transport, and emptying the payload while leaving it wired.

Corpus cost: **12 declarations, every one honest** — including a function that read a
file without declaring `File`, and helpers that read argv without declaring `Env`.

### Why `Unsafe` is held back, and what has to happen before it is not

Enforcing it alongside the sinks produced **91 diagnostics across 32 of 95 packages, 82
of them missing only `Unsafe`** while the caller already held the full `Std` set —
which is DEFINED as every capability except `Unsafe`. So `Std` could not open a file,
compare two strings, or push to a `Vec`, because every allocating path reaches
`alloc::heap_new` (`with(Alloc, Unsafe)`).

The agreed end state is not this exception — identical declarations must not mean
different things depending on which package the caller is in. It is:

- every declared capability, `Unsafe` included, binds uniformly across packages;
- a safe public abstraction does NOT declare `Unsafe` merely because its implementation
  uses raw operations — it terminates that requirement at a small explicit trusted leaf;
- a genuinely unsafe public API (raw pointers, unchecked construction, unchecked memory
  access) keeps `Unsafe`;
- evidence still records the dependency on the trusted implementation boundary even
  though callers do not require `Unsafe`.

Remaining work: classify the 82 as safe-abstraction / genuinely-unsafe / accidental,
apply the three treatments, remove the exception, and re-run the whole corpus requiring
every declared capability to bind.


## Stage 2 (2026-09-25) — `Unsafe` binds too, and the exception is gone

The exception was never the fix; it bought one release while the real question was
measured. Enforcing `Unsafe` alongside the sinks had produced 91 diagnostics over 32 of
95 packages — **82 missing only `Unsafe`**, with the caller already holding the full
`Std` set, which is DEFINED as every capability except `Unsafe`. That is not authority
being enforced, it is one artifact reproduced everywhere.

What removed it was the std migration, not an exemption: **public `Unsafe` declarations
154 → 21**. A safe wrapper marked `trusted` no longer re-exports a requirement its
callers never owed, so the 82 diagnostics went with them.

Three things made that expressible, each measured rather than assumed:

- **per-method `trusted`** — `trusted` was a whole-impl modifier, so *safe interface,
  audited implementation* (`Vec::push`) could not be said of a method at all. 67 of the
  154 public declarations do raw work in their own body, so this was most of the library;
- **`alloc::heap_new`/`grow`/`dealloc` dropped `Unsafe`, kept `Alloc`** — the obligation
  is enforced by the argument type, since they traffic `*mut T` and E0521 gates every
  dereference independently;
- **a `trusted` body discharges an `Unsafe` OBLIGATION at a call** — vouching that it
  satisfies a callee's precondition is what audited means. Scoped to `Unsafe` only,
  non-`extern` callees only, call sites only; both `error_trusted_*` fixtures still
  refuse, and E0521 is untouched.

Verified against a baseline sweep on the same tip: success/expected-refusal/unexpected
identical to pre-migration, with an injected parse error required to surface as
*unexpected* (it reports 118, not 118 passes). A `with(Std)` program using `Vec::push`
and `println` builds and runs with no `Unsafe` anywhere; `RawCursor::read_u8` is refused
to a caller declaring nothing. Both are pinned.

Full record: [TWO_AXIS_SAFETY.md](../language/TWO_AXIS_SAFETY.md).


## Stage 3 (2026-09-25) — the prelude receiver, and why two green gates hid it

`String::clone` and `String::drop` declare `with(Alloc)`, and a function declaring
nothing could call them: the program built and exited 0. It was committed as a live
reproducer and `check_cap_sibling_module.sh` asserted that acceptance as a KNOWN HOLE —
a PASSING check confirming the hole was open — while `check_cross_package_caps.sh`
passed with the headline "free functions and methods alike". Two green gates, two
incompatible claims, and CI could not distinguish them.

`CModule.importedFnCaps` was built by walking `m.imports`. `RawCursor::read_u8` is
explicitly imported and `TcpStream::connect` is an associated call — both have an import
to walk, so both bound, and a gate built on them looked like it proved the general case.
`String` needs no import statement, so its methods never entered the table.

Two details had to be read off the compiler rather than guessed: the call emits
`String_drop`, **not** `string_String_drop` — no module component — and `summaryTable` is
keyed at the PACKAGE level, so `String`'s methods live in the `string` SUBMODULE summary
and must be collected recursively. The first wrong key produced a clean build and a still-
open hole; giving the reproducer an explicit `import std.string.{String}` and reading the
name off the resulting diagnostic settled it.

Corpus cost: 5 declarations, each a function that consumes a `String` and therefore
allocates. Baseline sweep unchanged at 40 pre-existing failures, parse-error control
firing at 120.

The `known_hole_cross_package_method` fixture keeps its name and is now a REJECTION test
with a paired positive control (`prelude_method_ok`, which must build and run).


## Follow-on (2026-09-25): the inverse defect, repaired

Reviewing the surviving obligations one by one turned up the mirror-image problem —
APIs that SHOULD declare an obligation and did not — and it was pre-existing, not
caused by the migration. Ten now declare `Unsafe`:

- **nine consume a caller-supplied pointer** they will dereference: `alloc::grow`,
  `alloc::dealloc`, `io::write_raw`, `io::read`, `ByteWriter::from_raw`,
  `ptr::offset`, `String::from_raw_unchecked`, `test::sink_matches`,
  `text::validate_utf8`;
- **`Vec::get_mut`**, which a consume-vs-return heuristic misses: it RETURNS a
  pointer, but computes `self.ptr + at` with no bounds check, so the obligation is on
  the INDEX. That one is the argument for reading each API rather than classifying by
  shape.

Six correctly need nothing — `heap_new` and the `raw_ptr`/`keys_ptr` accessors return
a base pointer, and the obligation arises at USE, which E0521 gates independently.

**Declaring these cost nothing, and that is the trusted-discharge rule earning its
keep.** `alloc::dealloc` carrying `Unsafe` used to cascade through the entire library;
std's trusted containers now vouch that they satisfy its precondition, so the
obligation stops at the audited boundary. Corpus unchanged at 40 pre-existing
failures, parse-control firing at 121.

`String::from_raw_unchecked`'s new `Unsafe` is also only EFFECTIVE because the
prelude-receiver path was closed first — before that, a declaration on a `String`
method bound nothing.

Three gates asserted "capability-free" where they meant "operationally
capability-free" (`check_io_writer.sh` for Writer methods and `Reader::read`,
`check_std_test.sh` for expectation helpers). Before the two-axis work there was no
reason to distinguish. Each now rejects operational authority BY NAME and requires the
pointer-taking members to declare exactly `Unsafe` — stricter than the `none` they
replaced, and following the precedent `check_io_writer.sh` already set for
`fixed_writer requires Unsafe (caller-owned raw region)`.
