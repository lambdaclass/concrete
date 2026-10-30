# Platform Documentation

Status: stable reference index

ABI, FFI, data layout, targets, runtime boundaries, profiles, freestanding operation, and
predictable-platform constraints.

Start with [ABI.md](ABI.md), [FFI.md](FFI.md), [LAYOUT_CONTRACT.md](../archive/design-records/LAYOUT_CONTRACT.md) (design record; the rules are in the spec's FFI chapter), and
[PROFILES.md](PROFILES.md).

RocksDB slice 1 (R-0285) groundwork, its acceptance gate and the gaps blocking it:
[ROCKSDB_SLICE1.md](ROCKSDB_SLICE1.md).

Declaring the native (C) libraries a package's `extern fn`s link against, `[native]` in
`Concrete.toml`: [NATIVE_LIBRARIES.md](NATIVE_LIBRARIES.md).
