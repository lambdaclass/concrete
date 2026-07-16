# std.cli — v1 design (Phase 7 item 2, decision note)

Status: DESIGN RESOLVED — v1 scoped, pulled by measured workload evidence.
Written 2026-07-16, after workload 3 (`examples/hexdump`) put a number on
the friction: ~45 lines of hand loop for two value flags and one
positional (indexed `get(i)` + String-literal `eq` + manual value
lookahead + duplicate-positional rejection). base64_cli and png_chunks
each hand-rolled smaller variants of the same loop. Three workloads, one
shape — this is the pull ROADMAP item 2 asked for.

Related: `docs/ERROR_CONVENTIONS.md` (13t buckets — usage errors exit 2),
`docs/MAIN_EXIT_MODEL.md` (u8 main), `std.args` (raw access layer, stays),
`docs/CALLABLE_VALUES_AND_CAPABILITIES.md` (no callback registration in
v1, so none of its machinery is needed here).

## What v1 is

A small, allocating, DECLARE-THEN-PARSE helper over `std.args` — not a
framework. The program declares its flags, calls one parse function, and
gets either a filled-out result or a usage failure it maps to exit 2.

```con pseudocode
import std.cli.{Cli, parse};

let mut cli: Cli = Cli::new();
let skip:  u64  = cli.flag_u64("-s", 0);        // value flag with default
let limit: u64  = cli.flag_u64("-n", 0);
let want_ascii: bool = cli.flag_bool("-a");      // presence flag
match cli.parse() {                              // walks std.args once
    CliResult::Ok { positionals } => { ... }     // Vec<String>, caller owns
    CliResult::Err { .. } => { return usage(); } // malformed value, unknown
                                                 // flag, missing value
}
```

(Exact surface below; the sketch shows the shape: declaration is data,
parsing is one pass, errors are one recoverable bucket.)

## Decisions (each with the reason)

1. **Two flag kinds only: presence (`bool`) and value (`u64` / `String`).**
   Workloads used exactly these. No counts, no repeated flags, no
   subcommands in v1 — each waits for a workload that needs it.
2. **Space-separated values only (`-n 16`), no `-n=16`, no `-n16`.**
   One syntax, one code path; xxd/POSIX-utility style. `=`-forms are an
   append-only extension if a real tool needs them.
3. **Unknown flags are ERRORS, not positionals.** Anything starting with
   `-` (except bare `-`) that was not declared fails the parse. Silent
   pass-through is how tools eat typos (`-nn 16`). Bare `-` stays a
   positional (conventional stdin name) — the CLI layer does not interpret
   it.
4. **Errors are ONE recoverable bucket (13t), not a taxonomy.** The result
   carries which argument failed and why (unknown flag / missing value /
   malformed value / duplicate positional beyond arity) as data for the
   usage message; every case maps to exit 2. No error hierarchy.
5. **The parse result OWNS its Strings** (positionals and String flag
   values are fetched once from `std.args` and moved out). Linear
   discipline: caller destroys what it takes. Presence/`u64` flags carry
   no ownership.
6. **No callbacks, no derive, no help generation in v1.** Usage text stays
   the program's one hand-written string (every workload already has one).
   Auto-help is append-only later; it must not gate the parser.
7. **`std.args` stays as-is** (raw indexed access, `Env`-capability
   trusted boundary). `std.cli` is a pure-shape layer OVER it:
   capability-wise it inherits args's `Env` read at parse() time plus
   `Alloc` for the owned Strings. Nothing hosted beyond that.
8. **Declaration order is usage order, and arity is explicit.** v1 supports
   `positionals: exactly N | at least N | any` as a parse argument —
   hexdump wants exactly 1, base64_cli wants 1-2. Extra positionals are
   the duplicate-positional error the hand loops all re-implemented.

## Measured shape it must collapse

From `examples/hexdump/src/main.con` (pre-pull): the while loop over
`count()`, per-iteration literal binds + `eq` + drops, `flag_value(i+1)`
lookahead with `Option`, `bad` flag threading, `have_path`/`path_idx`
split (a consume-then-rebind E0207 dodge). Target: those ~45 lines become
~8 declaration+match lines. The E0207 dodge disappears because the result
owns the positional Strings.

## Deliberately NOT in v1 (append-only extensions, each workload-gated)

- Subcommands (`tool verb --flag`) — no workload yet.
- Repeated flags / flag arrays — none needed.
- `--long`-vs-`-short` aliasing — workloads used single-dash short flags;
  aliases are a table extension, not a redesign.
- Auto-generated `--help` — see decision 6.
- Negative-number positional disambiguation (`tool -5`): v1 treats
  leading-`-` as flag; a tool needing negative positionals uses `--`…
  which is ALSO deferred until asked for. Documented sharp edge.
- Env-var fallback / config layering — different feature, different note.

## Implementation notes (for the v1 slice, when scheduled)

- One pass over `args.get(i)`; flags stored in a fixed small Vec of
  declarations (name, kind, default, seen). No map needed at CLI scale.
- `flag_u64` parses via `std.parse.parse_uint` — malformed = the same
  error bucket (hexdump already proved the `-n zzz` → exit 2 path).
- String comparison against declared names is the `String-literal eq`
  friction (hexdump FRICTION #3, second ask counting png_chunks's IEND
  check) — implementing std.cli may pull `String::eq_str(&self, lit)`
  first; if so, ship that as its own small pull with this note updated.
- Gate: `check_cli_helpers.sh` — a fixture tool declaring both flag kinds
  + arity, exercised over good/malformed/unknown/missing/duplicate inputs
  with exit codes asserted; hexdump/base64_cli switch over as
  proof-of-pull.

## Exit criterion for this note

v1 ships when the next CLI-shaped workload (or a switch-over of hexdump /
base64_cli) uses it and the hand loops are deleted. If that never
happens, the note stays a design record and the hand loop stays the
documented pattern — the helper must earn its place by deletion count.
