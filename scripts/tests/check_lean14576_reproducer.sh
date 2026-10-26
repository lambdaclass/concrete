#!/usr/bin/env bash
# LEAN #14576 FIRE DRILL (R-0208): the RETAINED upstream reproducer, run in ISOLATION against the checker
# this repository pins. Upstream: the kernel accepted wrong-structure projections, so an adversarial
# metaprogram could add checked declarations proving `False` with `#print axioms` reporting none.
#
# The fixture is the issue's reproducer VERBATIM (pinned by sha256, so it cannot drift into something
# easier). It is never run through the ambient developer environment: a fresh directory, an emptied
# environment, and the exact checker executable the replay process reports — not whatever `lean` is
# first on PATH.
#
# Recorded once, outside CI (no 4.28 toolchain there), 2026-10-12: lean 4.28.0 (the previously pinned
# checker) ACCEPTS it — 'bad' and 'boom' "do not depend on any axioms"; lean 4.34.1 rejects it in the
# kernel ("(kernel) invalid projection"). See docs/verification/R-0208-closure.md.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
FIX="scripts/tests/fixtures/lean14576/Bug.lean"
PIN="f46b478f0838cd4b5a78a0e0beaa5947ef17851e24e5aa015e7e384bcdf7186a"
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

if [ "$(shasum -a 256 "$FIX" | cut -c1-64)" = "$PIN" ]; then
  ok "the retained reproducer is the pinned upstream text"
else
  no "the reproducer no longer matches its pinned sha256 — the drill would test something else"
fi

# The checker that actually runs, located by asking it (the same process identity receipts bind).
TMPW="$(mktemp -d)"; trap 'rm -rf "$TMPW"' EXIT
printf '#eval show IO Unit from do IO.println s!"APP {← IO.appPath}"\n' > "$TMPW/where.lean"
APP="$(lake env lean "$TMPW/where.lean" 2>/dev/null | sed -n 's/^APP //p' | head -1)"
if [ -z "$APP" ] || [ ! -x "$APP" ]; then
  no "could not locate the pinned checker executable — the drill cannot run"
else
  ISO="$TMPW/iso"; mkdir -p "$ISO"; cp "$FIX" "$ISO/Bug.lean"
  OUT="$(cd "$ISO" && env -i HOME="$ISO" PATH=/usr/bin:/bin "$APP" --trust=0 Bug.lean 2>&1 || true)"
  echo "  info checker: $("$APP" --version 2>/dev/null)"
  if grep -q "'boom' does not depend on any axioms" <<<"$OUT"; then
    no "the pinned checker ACCEPTS an axiom-free proof of False — Lean #14576 is not fixed here"
  elif grep -q "(kernel) invalid projection" <<<"$OUT"; then
    ok "the pinned checker's kernel rejects the reproducer (invalid projection), so no axiom-free False exists"
  else
    no "the reproducer neither succeeded nor hit the expected kernel rejection — inspect: $(head -3 <<<"$OUT" | tr '\n' ' ')"
  fi
fi

GATE_DONE=1
echo "LEAN-14576: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
