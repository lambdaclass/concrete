#!/usr/bin/env bash
# The tracked inputs of a slice-0 build: what slice0_repro.sh copies and build_identity.sh digests.
# shellcheck disable=SC2034
slice0_sources=(Concrete Concrete.lean ConcreteSpec ConcreteSpec.lean Main.lean scripts/checks tests lakefile.toml lean-toolchain
  lake-manifest.json)

# Hash the tracked path list on stdin relative to the current checkout.
slice0_source_digest() {
  python3 -c '
import hashlib,sys
paths=sorted(p for p in sys.stdin.buffer.read().split(b"\0") if p)
h=hashlib.sha256()
for p in paths:
    h.update(hashlib.sha256(open(p,"rb").read()).hexdigest().encode()+b"  "+p+b"\n")
print(h.hexdigest())'
}

# emit_slice0_row MANIFEST PROGRAM DIR TARGET EXPECTATION
# Each caller supplies its bounded emitter command in the emitter array.
emit_slice0_row() {
  local manifest_path=$1
  local program=$2
  local dir=$3
  local target=$4
  local path
  local -a expectation
  read -r -a expectation <<<"$5"
  case "$program" in
    bir:*) "${emitter[@]}" emit "${program#bir:}" "$dir" "$target" "${expectation[@]}" ;;
    con:*)
      path=${program#con:}
      "${emitter[@]}" source "$path" "$(dirname "$manifest_path")/${path#tests/}" "$dir" "$target" "${expectation[@]}"
      ;;
    *) echo "slice0: unsupported program $program" >&2; return 1 ;;
  esac
}
