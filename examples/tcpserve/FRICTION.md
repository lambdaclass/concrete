# tcpserve — friction log (Phase 7, workload 8)

`tcpserve <port>`: bind loopback, accept EXACTLY ONE connection, serve a
fixed 256-byte binary body over minimal HTTP/1.0, exit deterministically.
Closes the last never-compiled std.net path: `TcpListener::accept` and the
accepted-stream lifecycle. Gate: `scripts/tests/check_tcpserve.sh` (16
legs; exit status, stdout, and stderr asserted separately per leg; curl +
a bytewise raw-socket reference client; injected bind/early-EOF failures;
a dribbling client that splits the terminator across writes against an
8-byte server read buffer; watchdog reports infra-timeout as a distinct
class; EXIT-trap cleanup).

## Scope honesty (recorded, not claimed)

- **Partial reads: covered by construction** (8-byte buffer, terminator
  split across client writes). **Partial writes: NOT claimed** — write_all
  executes end-to-end but loopback does not force the OS to split writes;
  deterministic short-write coverage belongs to the capability-fault
  simulation gate (docs/DETERMINISTIC_SIMULATION.md), where TcpStream
  joins as an injectable backend.
- **IPv4 only**: std.net's sockaddr is AF_INET. IPv6 is out of scope by
  classification, not by accident.
- No compiler bugs found — the first boundary-opening workload since
  base64_cli to come up clean (bug 045 was found by httpget one workload
  earlier and already fixed by the time this compiled).

## Pull-ask taxonomy (corrected across workloads)

1. **`Bytes::find(&needle, from)` — now at its SECOND independent ask**:
   httpget's buffered header_end + this server's cross-buffer terminator
   scan. Both are buffered subsequence searches over an accumulating
   Bytes — the reusable shape — NOT streaming state machines (which would
   be workload-specific and would not count). One more ask pulls it.
   (Correction: httpget's FRICTION called itself the 2nd ask after
   tar_list; that was wrong — see next item.)
2. **`Bytes::eq_at(offset, &needle)` / fixed-offset compare — ONE ask**
   (tar_list's ustar magic). A different API from find; counted
   separately.
3. **Resume-offset idiom**: the bounded rescan (`scan_from = len - 3`)
   is the caller-side pattern a future `Bytes::find` should support via
   its `from` parameter — evidence for the signature, recorded here.
4. **&Bytes-taking net read/write** — SECOND ask (httpget was first):
   `stream.read(&tmp as *mut u8, 8)` and `write_all(out.ptr, len)` force
   trusted callers for what is conceptually safe buffer I/O.

## Notes

- READY is printed strictly after `TcpListener::bind` returns Ok, which
  in std.net means socket+bind+listen all succeeded — the gate leans on
  that ordering for its readiness signal and asserts READY's absence on
  the bind-failure leg.
- Close-exactly-once is enforced by linearity (double-close does not
  compile; a missed close is E0208); the gate corroborates with process
  exit on every leg.
