#!/usr/bin/env bash
# Phase 7 item 14a gate: the Writer IO spine discipline (option A: fn-pointer handle),
# as amended by R-0484 (handles carry their capability in their type).
#
#  1. the fixed-buffer writer writes without Alloc
#  2. file/console writers require the right acquisition capability
#  3. nothing returning a Writer hides allocation (caps visible at acquisition)
#  4. (next slice) std.fmt writes through Writer — placeholder assert: no second
#     sink interface exists for it to bypass
#  5. no closed sink enum, no dyn/trait-object writer, exactly one handle shape

set -uo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
M="docs/stdlib/STDLIB_SURFACE_MANIFEST.tsv"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

# 1. fixed_writer: allocates=no, and the write/flush/close METHODS are cap-free
row=$(grep -P "^io\tfixed_writer\t" "$M")
echo "$row" | awk -F'\t' '$3=="no"' | grep -q . && ok "fixed_writer allocates: no" || no "fixed_writer must not allocate ($row)"
# R-0484 REVERSED THIS SECTION'S RULE (2026-10-01). It asserted that `Writer` methods are
# capability-free because authority was settled at ACQUISITION and travels with the handle —
# the ocap reading. That is exactly what R-0484 removed: a function holding a `Writer` could
# then print while declaring nothing, so `with(...)` was not the complete list of what a
# function can do. Now the handle carries its capability in its TYPE (`Writer<C>`) and using
# it requires `with(C)`: every method declares the handle's capability VARIABLE `C`, which a
# caller instantiates (`Writer<Console>` makes `write` cost Console; `Writer<{}>` costs
# nothing). Design: docs/language/HANDLE_CAPABILITIES.md.
#
# SELECTED BY RECEIVER AND EXACT SIGNATURE, not by name. A name matches several rows (`write`
# is also `TextFile`'s method, `with(File)`), and this section once passed on whichever row of
# the name happened to say `none`. The manifest's receiver column (R-0484) names the impl.
#
# A method must declare exactly `C`, or `C` plus `Unsafe` where it dereferences a
# caller-supplied raw pointer — never a CONCRETE operational capability (that would hard-wire
# one sink's authority into every handle) and never nothing (the R-0484 hole).
method(){ # receiver name expected-caps expected-signature
  local r; r=$(awk -F'\t' -v rc="$1" -v n="$2" '$1=="io" && $10==rc && $2==n' "$M")
  if [ "$(printf '%s\n' "$r" | grep -c .)" -ne 1 ]; then
    no "$1.$2: expected exactly one manifest row, found: ${r:-none}"; return
  fi
  local caps sig; caps=$(printf '%s' "$r" | cut -f6); sig=$(printf '%s' "$r" | cut -f9)
  if [ "$caps" = "$3" ] && [ "$sig" = "$4" ]; then ok "$1.$2 declares exactly $3: $4"
  else no "$1.$2 should be [$3] $4 — is [$caps] $sig"; fi
}
method 'Writer<C>' write     'C'        '(&self, data: &Bytes) with(C) -> Result<u64, IoError>'
method 'Writer<C>' write_str 'C'        '(&self, data: &String) with(C) -> Result<u64, IoError>'
method 'Writer<C>' flush     'C'        '(&self) with(C) -> Result<u64, IoError>'
method 'Writer<C>' close     'C'        '(self) with(C) -> Result<u64, IoError>'
method 'Writer<C>' write_raw 'C,Unsafe' '(&self, data: *const u8, len: u64) with(Unsafe,C) -> Result<u64, IoError>'
method 'Reader<C>' read      'C,Unsafe' '(&self, buf: *mut u8, len: u64) with(Unsafe,C) -> Result<u64, IoError>'
method 'Reader<C>' close     'C'        '(self) with(C) -> Result<u64, IoError>'
# And no handle method anywhere escapes that shape: every Writer<C>/Reader<C> method carries C.
free=$(awk -F'\t' '$1=="io" && ($10=="Writer<C>" || $10=="Reader<C>") && $6 !~ /(^|,)C(,|$)/ {print $10"."$2" ["$6"]"}' "$M")
[ -z "$free" ] && ok "every Writer<C>/Reader<C> method requires the handle's capability" \
  || no "handle methods usable without the handle's capability: $free"

echo "=== instantiation and transfer (compiled controls) ==="
# The rule must bite at USE of a concretely-authorised handle and NOWHERE ELSE: a `Writer<{}>`
# carries no authority, and moving a handle is not using it. Each control is compiled, not
# grepped — a rule that charged `{}` or charged a move would pass every manifest check above.
CC="$ROOT_DIR/.lake/build/bin/concrete"; HC="$ROOT_DIR/tests/regressions/handle_caps"
TMPB="$(mktemp -d)"; trap 'rm -rf "$TMPB"' EXIT
for ctl in empty_writer_free transfer_free; do
  if (cd "$HC/$ctl" && "$CC" build . -o "$TMPB/$ctl" >/dev/null 2>&1) && "$TMPB/$ctl" >/dev/null 2>&1; then
    ok "CONTROL $ctl builds with no capability on the holder, and runs (exit 0)"
  else
    no "CONTROL $ctl is refused or fails — the rule charges more than USE of a concrete handle"
  fi
done
cout="$(cd "$HC/console_use_refused" && "$CC" check . 2>&1)"
if printf '%s' "$cout" | grep -q "function 'flush' requires capability 'Console' but 'use_unpaid' does not declare it"; then
  ok "using a Writer<Console> without declaring Console is refused, naming Console"
else
  no "a Writer<Console> is usable without Console: $(printf '%s' "$cout" | grep error | head -1 | cut -c1-160)"
fi

# 2. acquisition capabilities
grep -P "^io\twriter_from_file\t" "$M" | awk -F'\t' '$6 ~ /File/' | grep -q . \
  && ok "writer_from_file requires File" || no "writer_from_file missing File cap"
grep -P "^io\tconsole_writer\t" "$M" | awk -F'\t' '$6 ~ /Console/' | grep -q . \
  && ok "console_writer requires Console" || no "console_writer missing Console cap"
grep -P "^io\tfixed_writer\t" "$M" | awk -F'\t' '$6 ~ /Unsafe/' | grep -q . \
  && ok "fixed_writer requires Unsafe (caller-owned raw region)" || no "fixed_writer missing Unsafe cap"

# 3. every Writer-returning pub fn declares SOME acquisition authority (no silent sinks)
bad=$(grep -nE 'pub (trusted )?fn [a-z_]+.*-> *Writer' std/src/io.con | grep -v "with(" || true)
[ -z "$bad" ] && ok "every Writer constructor declares acquisition authority" || no "silent Writer constructor: $bad"

# 5. one handle shape; no closed sink enum; no dyn
[ "$(grep -c 'struct Writer' std/src/io.con)" -eq 1 ] && ok "exactly one Writer handle struct" || no "multiple Writer shapes"
grep -qE 'enum +Writer|dyn +Writer' std/src/*.con && no "closed sink enum / dyn Writer found" || ok "no closed sink enum, no dyn Writer"
n=$(grep -l "write_fn" std/src/*.con | wc -l | tr -d ' ')
[ "$n" -eq 1 ] && ok "the fn-pointer sink shape lives only in std.io" || no "parallel sink interfaces in $n files"

echo "=== Reader (symmetric contract) ==="
grep -P "^io\tfixed_reader\t" "$M" | awk -F'\t' '$3=="no" && $6 ~ /Unsafe/' | grep -q . \
  && ok "fixed_reader: no Alloc, Unsafe at acquisition" || no "fixed_reader facts wrong"
grep -P "^io\treader_from_file\t" "$M" | awk -F'\t' '$6 ~ /File/' | grep -q . \
  && ok "reader_from_file requires File" || no "reader_from_file missing File"
# The RESULT shape is the contract here: a read can fail at runtime and the caller must be
# able to recover. Its capability set is asserted with the Writer methods above.
r=$(awk -F'\t' '$1=="io" && $10=="Reader<C>" && $2=="read"' "$M")
echo "$r" | awk -F'\t' '$5=="result"' | grep -q . \
  && ok "Reader.read returns a recoverable Result" || no "Reader.read must return a recoverable Result ($r)"
[ "$(grep -c 'struct Reader' std/src/io.con)" -eq 1 ] && ok "exactly one Reader handle" || no "multiple Readers"

echo
echo "IO-WRITER: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
