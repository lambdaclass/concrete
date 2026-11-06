#!/usr/bin/env bash
# CHECKER IDENTITY (R-0208 slice 1a): a receipt binds the checker that ACTUALLY RAN.
#
# Until R-0208 the checker was named by the text of the workspace's `lean-toolchain` file. The nix
# devshell ignores that file and CI's elan honours it, so the name could describe a process that
# never ran. The identity is now reported by the checking process itself — version, githash,
# platform — plus a digest of the native images it had LOADED (executable, libleanshared, GMP, ...),
# read from the process's own memory map, never from files that merely sit nearby.
#
# Every leg here is a hostile control with a live positive control:
#   - changing ONLY lean-toolchain must not change the identity;
#   - changing loaded executable/library BYTES must, and must make receipts non-current;
#   - a missing, duplicated, incomplete or edited identity must refuse acceptance — never match.
#
# WHAT A CURRENT RECEIPT MEANS AFTER THIS SLICE is asserted too: identity agreement, not a fresh
# kernel re-check and not advisory acceptance. Those are later R-0208 slices.
#
# The stub `lake` legs alter only what the checker PRINTS about itself, which is exactly the channel
# an identity has to survive: the nonce correlates a line with a run, and that is all it does.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
trap 'rc=$?; if [ "$rc" -ne 0 ] && [ "${GATE_DONE:-0}" -ne 1 ]; then
  echo "FATAL: unexpected shell failure (exit $rc) — the verdict below is not trustworthy" >&2; exit "$rc"; fi' ERR
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
source "scripts/tests/lib/fresh.sh"
require_fresh_binary || exit 1
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
BIN="$ROOT_DIR/.lake/build/bin/concrete"
SRC="examples/elf_header/src/main.con"
REAL_LAKE="$(command -v lake)"

PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }

consume() { "$BIN" "$SRC" --report proof-status --receipts "$TMP/r.txt" 2>/dev/null | sed -n '/=== Replay Receipts/,$p' || true; }
tally()   { grep -oE '[0-9]+ current, [0-9]+ not current, [0-9]+ unreadable' <<<"$1" | head -1; }
checker() { sed -n 's/^  Checker now: //p' <<<"$1" | head -1; }
ID_RE='^lean [^ ]+ commit [^ ]+ platform [^ ]+ build [0-9a-f]{64}$'

echo "=== the checker names itself, and receipts bind it ==="

ISSUE="$("$BIN" "$SRC" --report receipts --out "$TMP/r.txt" 2>&1 || true)"
ISSUED_ID="$(sed -n 's/^Checker: //p' <<<"$ISSUE" | head -1)"
if grep -qE "$ID_RE" <<<"$ISSUED_ID" && grep -q 'Summary: 5 issued, 0 withheld' <<<"$ISSUE"; then
  ok "issuance names the checker that ran (version, commit, platform, build digest) and mints 5"
else
  no "issuance did not name a well-formed checker or did not mint: '$ISSUED_ID'"
fi
if grep -q '^Toolchain: .*(declared pin; the checker below is what ran)' <<<"$ISSUE"; then
  ok "the lean-toolchain text is shown as a declared pin, not as the checker"
else
  no "the report no longer separates the declared pin from the checker that ran"
fi
BASE="$(consume)"
BASE_ID="$(checker "$BASE")"
if [ "$(tally "$BASE")" = "5 current, 0 not current, 0 unreadable" ] && [ "$BASE_ID" = "$ISSUED_ID" ]; then
  ok "the consumer resolves the SAME identity without a replay, and all 5 read current (the control)"
else
  no "consumer identity/currency moved: $(tally "$BASE") now='$BASE_ID' issued='$ISSUED_ID'"
fi

echo "=== 'current' is identity agreement; acceptance needs this run's re-check, and the report says so ==="

if grep -q "re-checked by the kernel in this run (fresh consumer replay)" <<<"$BASE"; then
  ok "the report states that acceptance comes from this run's own kernel re-check, not from currency"
else
  no "the disclosure is gone — 'current' now reads as re-checked or advisory-accepted"
fi

echo "=== changing ONLY lean-toolchain does not change the identity ==="

# The checker that ran, located by asking it. Putting ITS bin first in PATH bypasses any toolchain
# manager, so the bogus pin below is read by nobody but the report — exactly the separation claimed.
cat > "$TMP/where.lean" <<'LEAN'
#eval show IO Unit from do IO.println s!"APP {← IO.appPath}"
LEAN
APP="$(lake env lean "$TMP/where.lean" 2>/dev/null | sed -n 's/^APP //p' | head -1)"
REALBIN="$(dirname "$APP")"
PIN_WS="$TMP/pin_ws"
mkdir -p "$PIN_WS"
printf 'name = "checker_identity_pin_probe"\n' > "$PIN_WS/lakefile.toml"
printf 'leanprover/lean4:v0.0.0-not-a-real-toolchain\n' > "$PIN_WS/lean-toolchain"
cp "$SRC" "$PIN_WS/main.con"
PIN_OUT="$(PATH="$REALBIN:$PATH" "$BIN" "$PIN_WS/main.con" --report proof-status --receipts "$TMP/r.txt" 2>/dev/null | sed -n '/=== Replay Receipts/,$p' || true)"
PIN_ID="$(checker "$PIN_OUT")"
if [ -z "$APP" ] || [ ! -x "$APP" ]; then
  no "could not locate the running checker executable — this control lost its target"
elif [ "$PIN_ID" = "$BASE_ID" ] && [ -n "$PIN_ID" ]; then
  ok "a workspace pinning a bogus toolchain reports the same checker identity"
else
  no "the lean-toolchain text moved the identity: '$PIN_ID' vs '$BASE_ID'"
fi

echo "=== changing loaded executable/library bytes changes the identity ==="

# A copy of the toolchain: executables and native libraries COPIED, everything else (the 2GB of
# .olean data) symlinked. The copy runs its own libleanshared through its rpath.
PFX="$(dirname "$REALBIN")"
TC="$TMP/toolchain"
mkdir -p "$TC/lib/lean"
cp -R "$PFX/bin" "$TC/bin"; chmod -R u+w "$TC/bin"
for e in "$PFX/lib/lean/"*; do
  n="$(basename "$e")"
  case "$n" in
    *.dylib|*.so|*.so.*|*.dll) cp "$e" "$TC/lib/lean/$n"; chmod u+w "$TC/lib/lean/$n" ;;
    *) ln -s "$e" "$TC/lib/lean/$n" ;;
  esac
done
for e in "$PFX/lib/"*; do n="$(basename "$e")"; [ "$n" = lean ] || ln -s "$e" "$TC/lib/$n"; done
COPY_OUT="$(PATH="$TC/bin:$PATH" consume)"
COPY_ID="$(checker "$COPY_OUT")"
if [ "$COPY_ID" = "$BASE_ID" ] && [ "$(tally "$COPY_OUT")" = "5 current, 0 not current, 0 unreadable" ]; then
  ok "an identical build at another path is the same checker (images keyed by name, not location)"
else
  no "an identical copy of the checker changed identity or currency: $(tally "$COPY_OUT") '$COPY_ID'"
fi
LIB="$(ls "$TC/lib/lean/" | grep -E '^libleanshared\.(dylib|so)$' | head -1)"
if [ -z "$LIB" ]; then
  no "the copied toolchain has no libleanshared — this control lost its target"
else
  # One byte APPENDED: beyond every mapped segment, so the library still loads — what changes is
  # only the bytes the identity must bind.
  printf 'X' >> "$TC/lib/lean/$LIB"
  MOD_OUT="$(PATH="$TC/bin:$PATH" consume)"
  MOD_ID="$(checker "$MOD_OUT")"
  if [ -n "$MOD_ID" ] && grep -qE "$ID_RE" <<<"$MOD_ID" && [ "$MOD_ID" != "$BASE_ID" ] \
     && [ "$(tally "$MOD_OUT")" = "0 current, 5 not current, 0 unreadable" ]; then
    ok "one byte appended to the LOADED libleanshared changes the identity and every receipt goes non-current"
  else
    no "a changed kernel library left identity/currency unchanged: $(tally "$MOD_OUT") '$MOD_ID'"
  fi
fi

echo "=== a missing, ambiguous, incomplete or edited identity refuses ==="

# A stub `lake` that runs the real one and rewrites only what the checker says about itself.
stub() {  # $1 = name, $2 = sed program applied to the checker's combined output
  mkdir -p "$TMP/stub_$1"
  cat > "$TMP/stub_$1/lake" <<EOF
#!/usr/bin/env bash
set -o pipefail
"$REAL_LAKE" "\$@" 2>&1 | sed -E '$2'
EOF
  chmod +x "$TMP/stub_$1/lake"
}
stub drop   '/^CONCRETE-CHECKER-ID /d'
stub dup    '/^CONCRETE-CHECKER-ID /{p;}'
stub nokern '/^CONCRETE-CHECKER-IMAGE .*libleanshared/d'
stub noimg  '/^CONCRETE-CHECKER-IMAGE /d'
# A SECOND loaded image with the same file name at another path. Images are keyed by file name so
# the digest is location-independent; an ambiguous key would let one library stand in for another.
# The second path is a REAL file (a link to the same library), so hashing succeeds and only the
# duplicate-key rule can refuse — a missing file would refuse for an unrelated reason.
mkdir -p "$TMP/elsewhere"
for e in "$PFX/lib/lean/"libleanshared.*; do ln -s "$e" "$TMP/elsewhere/$(basename "$e")"; done
stub dupname "s#^(CONCRETE-CHECKER-IMAGE [0-9a-f]+ )(.*/)(libleanshared\\.(dylib|so))\$#\\1\\2\\3\\n\\1$TMP/elsewhere/\\3#"
stub edit   '/^CONCRETE-CHECKER-ID /s/\|[^|]*\|/|forged-commit|/'
stub foreign 's/^(CONCRETE-CHECKER-ID )([0-9a-f]+)(.*)$/\1\2\3\n\1deadbeef 9.99.0|forged|forged|\/forged/'

# Each refusal NAMES ITS CAUSE, in the checker line and in every receipt it refuses: "UNIDENTIFIED"
# alone once hid a missing `shasum` for hours (2026-10-23). The cause is matched as a fixed string.
for c in "drop:no identity line:the checker printed no identity line for this run" \
         "dup:two identity lines:the checker printed 2 identity lines for this run" \
         "nokern:no libleanshared loaded:no libleanshared was loaded" \
         "noimg:no loaded-image list:the checker listed no loaded images" \
         "dupname:two loaded images sharing a file name:two loaded images share a file name"; do
  s="${c%%:*}"; rest="${c#*:}"; why="${rest%%:*}"; cause="${rest#*:}"
  OUT="$(PATH="$TMP/stub_$s:$PATH" consume)"
  if grep -qF "Checker now: UNIDENTIFIED ($cause" <<<"$OUT" \
     && [ "$(tally "$OUT")" = "0 current, 5 not current, 0 unreadable" ] \
     && [ "$(grep -c 'cannot be checked \[checker_unidentified\]' <<<"$OUT")" = "5" ] \
     && [ "$(grep 'cannot be checked \[checker_unidentified\]' <<<"$OUT" | grep -cF "$cause")" = "5" ]; then
    ok "$why: identity refused with its cause named, in the checker line and in all 5 refusals"
  else
    no "$why did not refuse with its cause ('$cause'): $(tally "$OUT") now='$(checker "$OUT")'"
  fi
done

# ...and the same at issuance: nothing is minted when the run cannot say who checked it.
ISS_DROP="$(PATH="$TMP/stub_drop:$PATH" "$BIN" "$SRC" --report receipts --out "$TMP/none.txt" 2>&1 || true)"
DROP_CAUSE="the checker printed no identity line for this run"
if grep -qF "Checker: UNIDENTIFIED ($DROP_CAUSE" <<<"$ISS_DROP" && grep -q 'Summary: 0 issued, 5 withheld' <<<"$ISS_DROP" \
   && [ "$(grep 'checker executable and build' <<<"$ISS_DROP" | grep -cF "$DROP_CAUSE")" = "5" ]; then
  ok "issuance under an unidentified checker withholds all 5 and names the cause in each"
else
  no "an unidentified checker still minted: $(grep -E '^Summary|^Checker' <<<"$ISS_DROP" | tr '\n' ' ')"
fi

# THE HOST TOOL THE IDENTITY NEEDS, MISSING. The build digest is computed with the host's `shasum`
# (Replay.lean, sha256Files). A PATH holding every executable of the real PATH EXCEPT shasum (first
# hit wins, as PATH does) must refuse with that tool named — at issuance, in every withheld receipt,
# and at consumption. The control: the same PATH still runs the checker (it printed an identity),
# so what refused is the hash, not a missing lake.
NOSHA="$TMP/noshasum"; mkdir -p "$NOSHA"
IFS=: read -r -a _pdirs <<<"$PATH"
for d in "${_pdirs[@]}"; do
  [ -d "$d" ] || continue
  for f in "$d"/*; do
    n="${f##*/}"
    [ "$n" = shasum ] && continue
    [ -x "$f" ] && [ ! -e "$NOSHA/$n" ] && [ ! -L "$NOSHA/$n" ] && ln -s "$f" "$NOSHA/$n"
  done
done
if PATH="$NOSHA" command -v shasum >/dev/null 2>&1 || ! PATH="$NOSHA" command -v lake >/dev/null 2>&1; then
  no "CONTROL: the stripped PATH still has shasum or lost lake — this leg lost its target"
else
  SHA_CAUSE="shasum unavailable"
  ISS_NOSHA="$(PATH="$NOSHA" "$BIN" "$SRC" --report receipts --out "$TMP/nosha.txt" 2>&1 || true)"
  if grep -qF "Checker: UNIDENTIFIED ($SHA_CAUSE" <<<"$ISS_NOSHA" && grep -q 'Summary: 0 issued, 5 withheld' <<<"$ISS_NOSHA" \
     && [ "$(grep 'checker executable and build' <<<"$ISS_NOSHA" | grep -cF "$SHA_CAUSE")" = "5" ]; then
    ok "without shasum on PATH, issuance names it ('$SHA_CAUSE') in the checker line and all 5 withheld receipts"
  else
    no "without shasum, issuance did not name the cause: $(grep -E '^Summary|^Checker' <<<"$ISS_NOSHA" | tr '\n' ' ')"
  fi
  CON_NOSHA="$(PATH="$NOSHA" consume)"
  if grep -qF "Checker now: UNIDENTIFIED ($SHA_CAUSE" <<<"$CON_NOSHA" \
     && [ "$(tally "$CON_NOSHA")" = "0 current, 5 not current, 0 unreadable" ] \
     && [ "$(grep 'cannot be checked \[checker_unidentified\]' <<<"$CON_NOSHA" | grep -cF "$SHA_CAUSE")" = "5" ]; then
    ok "without shasum on PATH, the consumer names it and refuses all 5 stored receipts with it"
  else
    no "without shasum, the consumer did not name the cause: $(tally "$CON_NOSHA") now='$(checker "$CON_NOSHA")'"
  fi
fi

# An edited self-report is a DIFFERENT identity, never a match for the old one.
EDIT_OUT="$(PATH="$TMP/stub_edit:$PATH" consume)"
if grep -q 'forged-commit' <<<"$(checker "$EDIT_OUT")" \
   && [ "$(tally "$EDIT_OUT")" = "0 current, 5 not current, 0 unreadable" ]; then
  ok "an edited commit in the self-report is a different checker: every receipt non-current"
else
  no "an edited identity still matched: $(tally "$EDIT_OUT") '$(checker "$EDIT_OUT")'"
fi

# A line carrying some OTHER nonce is ignored, not adopted: the real identity still stands.
FOR_OUT="$(PATH="$TMP/stub_foreign:$PATH" consume)"
if [ "$(checker "$FOR_OUT")" = "$BASE_ID" ] && [ "$(tally "$FOR_OUT")" = "5 current, 0 not current, 0 unreadable" ]; then
  ok "an identity line under a foreign nonce is ignored; the run's own line decides"
else
  no "a foreign-nonce line moved the identity: '$(checker "$FOR_OUT")'"
fi

GATE_DONE=1
echo "CHECKER-IDENTITY: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
