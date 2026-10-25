### Packages Declare Their Native Libraries; RocksDB Slice 1 Groundwork (2026-10-20)

A package whose `extern fn`s resolve against a C library now declares that library in its
`Concrete.toml`, in a `[native]` section with `pkg-config`, `libs` and `lib-dirs` keys.
`concrete build`, `run` and `test` link the declared libraries of the root package and of every
dependency package its manifest loads (transitive dependencies are not loaded), root first, and pass each library directory as `-L` and an rpath, so an
application that depends on a binding package needs no linker flags of its own. Resolution happens
only at the link step, so `check`, the reports and `--emit-llvm` work without the library
installed. The manifest reader fails closed: any spelling of the section it does not read, a
malformed or escaped value, an unreadable manifest, or a library name that spells a linker option
stops the command with a diagnostic naming the declaring package, because a dropped library
reappears later as an unresolved symbol. The rules are in
[NATIVE_LIBRARIES.md](docs/platform/NATIVE_LIBRARIES.md), and `check_native_libs.sh` (CI) is the
gate. This closes gap G1 of the RocksDB slice.

`examples/rocksdb_kv` is RocksDB slice 1's groundwork: an application with a hand-written binding
package (`[native] pkg-config = ["rocksdb"]`) whose resources are linear structs with exactly one
consuming release each, two compile-fail fixtures (a handle never destroyed, a handle used after
close), a release observer and the acceptance gate `check_rocksdb_slice1.sh`. Each fallible
operation's failure and resource contract is recorded in
[ROCKSDB_SLICE1.md](docs/platform/ROCKSDB_SLICE1.md), together with the gaps G1 – G7 found on the
way. The slice is not accepted: R-0285's acceptance waits on exits 0b and 2i. The binding's
declarations and ownership facts are reviewed assumptions taken from RocksDB's `c.h`, not checked
facts.
