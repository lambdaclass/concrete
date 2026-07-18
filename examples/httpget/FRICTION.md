# httpget — friction log (Phase 7, workload 7)

`httpget <host> <port> <path>`: HTTP/1.0 GET, body bytes verbatim to
stdout. First compiled consumer of the Network boundary at workload scale
— `TcpStream::connect/write_all/read_all/close` against a real loopback
peer (python3 http.server), byte-diffed on text and an 8KB all-256-values
binary (multiple recv fills), cross-checked against curl.
Gate: `scripts/tests/check_httpget_differential.sh`.

## Compiler bug found (the discovery loop keeps paying)

- **Bug 045 (fixed in this stack):** nested same-named match binders
  shared one runtime slot. The httpget shape — inner `value: u64` binder
  inside an outer `value: TcpStream` arm — hit SSA-verify E0715; the
  reduced same-type probe showed the SILENT flavor: reading the outer
  `value` after an inner match returned the inner's value on BOTH
  backends (differential-blind). Root fix: Elab alpha-renames match
  binders to unique Core names, which retires the whole 040/041/045
  name-scoping class below Elab. This is the fifth bug found by "first
  compiled consumer of a boundary" in three days.

## Pull candidates

1. **Bytes sequence-find** — `header_end` hand-rolls a resume loop over
   single-byte `index_of` to find `\r\n\r\n`. CORRECTED (workload 8): this
   is the FIRST true ask for a buffered subsequence search — tar_list's
   magic check is a fixed-offset EQUALITY (`eq_at`), a different API
   counted separately. tcpserve's cross-buffer terminator scan is the
   second ask; one more pulls `Bytes::find(&needle, from)`.
2. **net read/write take raw pointers** — `write_all(ptr, len)` forces
   `raw.ptr as *const u8` + a trusted caller. A `&Bytes`-taking overload
   (or Writer-style sink) would keep user code out of Unsafe. First ask.
3. **No pattern rename** (`{ value: x }`) — third brush with this; the
   rebind-immediately idiom (`let path = match ... { Some { value } =>
   { value } ... }`) is fine but was FORCED here for three positionals.
   Now that bug 045 gives binders real scoping, the pressure is lower.

## Design notes

- HTTP/1.0 + `Connection: close` chosen precisely so `read_all`-to-EOF is
  the correct read loop — no Content-Length parsing needed for a v1.
- `print_char(byte as Int)` is byte-transparent, so binary bodies survive
  stdout; the gate pins this with the all-byte-values blob.
