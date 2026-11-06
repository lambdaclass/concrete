# Elixir Stdlib Packet

Status: research

Source pointers: Elixir standard library docs: `Enum`, `Stream`, `Map`, `MapSet`,
`String`, `Regex`, `File`, `Path`, `IO`, `System`, `Date`, `Time`, `NaiveDateTime`,
`Calendar`, `URI`, `OptionParser`, `Agent`, `GenServer`, `Registry`,
`Supervisor`, `Task`, and ExUnit.

## What Elixir Has

- Rich enumerable APIs (`Enum`) and lazy stream APIs (`Stream`).
- Map/MapSet/list helpers.
- String/Unicode-heavy APIs.
- Regex.
- File/path/IO/system APIs.
- URI and option parser helpers.
- Date/time/calendar.
- Testing via ExUnit.
- Actor/OTP runtime: processes, supervisors, GenServer, Registry, Task, Agent.

## What Concrete Should Copy

1. **Clear module boundaries.**
   Elixir APIs are easy to find by module: `File`, `Path`, `IO`, `URI`,
   `OptionParser`. Concrete should keep similarly boring names.

2. **CLI-oriented helpers.**
   Option parsing, path handling, IO, logging, and test helpers are essential for
   useful tools.

3. **Testing ergonomics.**
   ExUnit-style clarity supports Concrete's `std.test` direction, but Concrete
   should attach evidence/oracle classifications.

4. **URI and date/time clarity.**
   These are practical and should be explicit about parsing vs authority.

## What Concrete Should Not Copy

- Actor/OTP runtime in core.
- Dynamic process registry/supervisor model.
- Regex in core.
- Unicode-heavy string defaults before policy is settled.
- Macro-heavy DSL style.

## Missing Concrete Items This Pressures

- `std.cli` option parser.
- `std.uri` and MIME/media-type parser.
- `std.terminal` / IO conventions.
- richer `std.test` UX.
- calendar/date/time scope decision beyond minimal timestamps.

## Concrete Classification

- Copy now: module clarity, CLI/test/URI inspiration.
- Stdlib later: option parser, terminal helpers, richer date/time.
- Package later: regex and heavy Unicode.
- Non-goal/research: actor runtime and OTP-style supervision.

