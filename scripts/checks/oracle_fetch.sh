#!/usr/bin/env bash
# A RUNNER WITHOUT A VERIFIED ORACLE NEVER PASSES THE ORACLE CHECK (ISS-0005, P13; ROADMAP R-0564).
#
# scripts/oracle/ci_check.sh is the required oracle job on GitHub-hosted runners: fetch.sh downloads
# the published artifact and verifies it, then run.sh runs smoke.con through it. This gate runs the
# real fetch.sh, run.sh and ci_check.sh on a scratch copy of scripts/oracle with a test lock, a test
# artifact built with the real packer (lib.sh oracle_pack) and a fake `curl` (no network), and holds
# them to: a good artifact installs and verifies (positive control); a lock with no published
# artifact, a failed download, an artifact with other bytes, and an artifact whose install fails
# verification (manifest not the locked one) each leave fetch.sh and ci_check.sh INCOMPLETE (exit 3),
# never passed; an installed oracle whose files were altered is refused by run.sh (125) before
# anything runs, and ci_check.sh reports that as INCOMPLETE (an install altered after fetch.sh); a verified oracle whose smoke run exits
# other than 42, a compile failure (whatever its status, 42 included) and a compile that writes no
# program are FAILED (1), through the real run.sh with a fake `nix`. Each refusal is a negative
# control with its expected reason.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR" || exit 2
for f in fetch.sh run.sh ci_check.sh lib.sh smoke.con; do
  [ -f "scripts/oracle/$f" ] || { echo "FATAL: scripts/oracle/$f missing" >&2; exit 2; }
done
TMP="$(mktemp -d)" || exit 2
trap 'rm -rf "$TMP"' EXIT
PASS=0; FAIL=0
ok(){ echo "  ok   $1"; PASS=$((PASS+1)); }
no(){ echo "  FAIL $1"; FAIL=$((FAIL+1)); }
has(){ case "$1" in *"$2"*) return 0;; esac; return 1; }

COMMIT=1111111111111111111111111111111111111111
mkdir -p "$TMP/oracle" "$TMP/bin" "$TMP/art"
cp scripts/oracle/fetch.sh scripts/oracle/run.sh scripts/oracle/ci_check.sh scripts/oracle/lib.sh \
   scripts/oracle/smoke.con "$TMP/oracle/"
# shellcheck source=scripts/oracle/lib.sh
. "$TMP/oracle/lib.sh"

# A test artifact in the real format: bin/concrete, std/, shell/, MANIFEST.sha256 (oracle_pack).
stage() {  # stage DIR BINARY_TEXT
  mkdir -p "$1/bin" "$1/std" "$1/shell"
  printf '#!/bin/sh\n# %s\nexit 0\n' "$2" > "$1/bin/concrete"; chmod +x "$1/bin/concrete"
  printf 'mod std;\n' > "$1/std/lib.con"
  printf '{ }\n' > "$1/shell/flake.nix"; printf '{}\n' > "$1/shell/flake.lock"
  printf 'leanprover/lean4:v4.34.1\n' > "$1/shell/lean-toolchain"
}
stage "$TMP/good" "the test oracle"
oracle_pack "$TMP/good" "$TMP/art/good.tar.gz" || { echo "FATAL: oracle_pack failed" >&2; exit 2; }
GOOD_SHA="$(sha256sum "$TMP/art/good.tar.gz" | cut -d' ' -f1)"
MAN_SHA="$(sha256sum "$TMP/good/MANIFEST.sha256" | cut -d' ' -f1)"
BIN_SHA="$(sha256sum "$TMP/good/bin/concrete" | cut -d' ' -f1)"
# An artifact that unpacks but whose manifest is not the locked one (another std file).
stage "$TMP/bad" "the test oracle"; printf 'mod other;\n' > "$TMP/bad/std/lib.con"
oracle_pack "$TMP/bad" "$TMP/art/badman.tar.gz" || { echo "FATAL: oracle_pack failed" >&2; exit 2; }
BADMAN_SHA="$(sha256sum "$TMP/art/badman.tar.gz" | cut -d' ' -f1)"
printf 'not the artifact\n' > "$TMP/art/other.bin"

write_lock() {  # write_lock URL ARTIFACT_SHA
  cat > "$TMP/oracle/ORACLE.lock" <<EOF
tag=test-tag
commit=$COMMIT
platform=x86_64-linux
binary_sha256=$BIN_SHA
manifest_sha256=$MAN_SHA
artifact_sha256=$2
artifact_url=$1
EOF
}
URL=https://example.invalid/oracle-test-tag-x86_64-linux.tar.gz

# The fake curl serves $FAKE_SERVE to -o, or fails when FAKE_SERVE is empty.
cat > "$TMP/bin/curl" <<'EOF'
#!/usr/bin/env bash
out=""; while [ $# -gt 0 ]; do [ "$1" = -o ] && { out="$2"; shift; }; shift; done
[ -n "${FAKE_SERVE:-}" ] || { echo "curl: (6) Could not resolve host" >&2; exit 6; }
cp "$FAKE_SERVE" "$out"
EOF
chmod +x "$TMP/bin/curl"

H="$TMP/home"
fetch() { rm -rf "$H"; PATH="$TMP/bin:$PATH" ORACLE_HOME="$H" FAKE_SERVE="${1:-}" bash "$TMP/oracle/fetch.sh" 2>&1; }

echo "=== a good published artifact installs and verifies (positive control) ==="
write_lock "$URL" "$GOOD_SHA"
out="$(fetch "$TMP/art/good.tar.gz")"; rc=$?
[ "$rc" = 0 ] && has "$out" "verified" && ok "fetch.sh installs the good artifact (exit 0)" || no "good artifact: rc=$rc '$out'"
v="$(ORACLE_HOME="$H" bash -c '. "$1"; oracle_verify_install "$2"' _ "$TMP/oracle/lib.sh" "$H/$COMMIT")" \
  && ok "the installed oracle verifies against the lock" || no "install does not verify: $v"

echo "=== no published artifact, a failed download, other bytes, a failed verification: INCOMPLETE ==="
write_lock "" "$GOOD_SHA"
out="$(fetch "$TMP/art/good.tar.gz")"; rc=$?
[ "$rc" = 3 ] && has "$out" "INCOMPLETE: no published artifact" && ok "an empty artifact_url is INCOMPLETE (3)" || no "missing artifact: rc=$rc '$out'"
write_lock "$URL" "$GOOD_SHA"
out="$(fetch "")"; rc=$?
[ "$rc" = 3 ] && has "$out" "INCOMPLETE: download of" && ok "a failed download is INCOMPLETE (3)" || no "failed download: rc=$rc '$out'"
out="$(fetch "$TMP/art/other.bin")"; rc=$?
[ "$rc" = 3 ] && has "$out" "the lock says $GOOD_SHA" && ok "an artifact with other bytes (corrupted) is INCOMPLETE (3)" || no "corrupt artifact: rc=$rc '$out'"
[ ! -e "$H/$COMMIT" ] && ok "the corrupted artifact left no install behind" || no "corrupted artifact was installed"
write_lock "$URL" "$BADMAN_SHA"
out="$(fetch "$TMP/art/badman.tar.gz")"; rc=$?
[ "$rc" = 3 ] && has "$out" "MANIFEST.sha256 is" && ok "an install that fails verification (manifest not locked) is INCOMPLETE (3)" || no "failed verification: rc=$rc '$out'"
[ ! -e "$H/$COMMIT" ] && ok "the unverifiable artifact left no install behind" || no "unverifiable artifact was installed"

echo "=== ci_check.sh: INCOMPLETE without a verified oracle, FAILED on a wrong smoke result ==="
cc() { PATH="$TMP/bin:$PATH" ORACLE_HOME="$H" FAKE_SERVE="${1:-}" bash "$TMP/oracle/ci_check.sh" 2>&1; }
write_lock "" "$GOOD_SHA"; rm -rf "$H"
out="$(cc "$TMP/art/good.tar.gz")"; rc=$?
[ "$rc" = 3 ] && has "$out" "oracle-check: INCOMPLETE: fetch.sh exited 3" && ! has "$out" "passed" \
  && ok "no published artifact: the oracle check is INCOMPLETE (3), never passed" || no "ci_check missing: rc=$rc '$out'"
write_lock "$URL" "$GOOD_SHA"; rm -rf "$H"
out="$(cc "$TMP/art/other.bin")"; rc=$?
[ "$rc" = 3 ] && ! has "$out" "passed" && ok "a corrupted artifact: the oracle check is INCOMPLETE (3)" || no "ci_check corrupt: rc=$rc '$out'"
write_lock "$URL" "$BADMAN_SHA"; rm -rf "$H"
out="$(cc "$TMP/art/badman.tar.gz")"; rc=$?
[ "$rc" = 3 ] && ! has "$out" "passed" && ok "a failed verification: the oracle check is INCOMPLETE (3)" || no "ci_check verify: rc=$rc '$out'"

# A verified install, then the real run.sh refuses it once a file is altered (before any nix call).
write_lock "$URL" "$GOOD_SHA"
fetch "$TMP/art/good.tar.gz" >/dev/null || no "re-install for the run.sh controls failed"
printf 'mod tampered;\n' > "$H/$COMMIT/std/lib.con"
out="$(PATH="$TMP/bin:$PATH" ORACLE_HOME="$H" bash "$TMP/oracle/run.sh" "$TMP/oracle/smoke.con" 2>&1)"; rc=$?
[ "$rc" = 125 ] && has "$out" "differs from its manifest" && ok "run.sh refuses an altered install (125) before running anything" || no "tampered install: rc=$rc '$out'"

# The real run.sh end to end, with a fake `nix` standing in for the artifact's dev shell. FAKE_NIX
# picks what the compile step does: ok writes a program that exits 42; wrong writes one that exits 7;
# compile42 fails the compile with status 42 and writes nothing (a compiler status must never pass as
# the program's); compile1 is an ordinary compile error; noout "succeeds" without writing the program.
cat > "$TMP/bin/nix" <<'NIX'
#!/usr/bin/env bash
out=""; while [ $# -gt 0 ]; do [ "$1" = -o ] && { out="$2"; shift; }; shift; done
[ -n "$out" ] || { echo "fake nix: no -o" >&2; exit 99; }
case "${FAKE_NIX:-}" in
  ok)        printf '#!/bin/sh\nexit 42\n' > "$out"; chmod +x "$out" ;;
  wrong)     printf '#!/bin/sh\nexit 7\n' > "$out"; chmod +x "$out" ;;
  compile42) echo "error: the compiler failed" >&2; exit 42 ;;
  compile1)  echo "error: the compiler failed" >&2; exit 1 ;;
  noout)     : ;;
  *)         echo "fake nix: FAKE_NIX unset" >&2; exit 99 ;;
esac
NIX
chmod +x "$TMP/bin/nix"
ce() { rm -rf "$H"; PATH="$TMP/bin:$PATH" ORACLE_HOME="$H" FAKE_SERVE="$TMP/art/good.tar.gz" FAKE_NIX="$1" \
         bash "$TMP/oracle/ci_check.sh" 2>&1; }
write_lock "$URL" "$GOOD_SHA"
out="$(ce ok)"; rc=$?
[ "$rc" = 0 ] && has "$out" "oracle-check: passed" && ok "end to end: fetch, verify, compile, run to 42 passes (positive control)" || no "end to end ok: rc=$rc '$out'"
for case in "wrong:FAILED: smoke.con exited 7" \
            "compile42:FAILED: the verified oracle did not compile smoke.con (status 42)" \
            "compile1:FAILED: the verified oracle did not compile smoke.con (status 1)" \
            "noout:FAILED: smoke.con exited 127"; do
  mode="${case%%:*}"; msg="${case#*:}"
  out="$(ce "$mode")"; rc=$?
  [ "$rc" = 1 ] && has "$out" "$msg" && ! has "$out" "passed" && ok "end to end, $mode: FAILED (1), never passed" || no "end to end $mode: rc=$rc '$out'"
done
rm -rf "$H"
out="$(PATH="$TMP/bin:$PATH" ORACLE_HOME="$H" FAKE_SERVE="" FAKE_NIX=ok bash "$TMP/oracle/ci_check.sh" 2>&1)"; rc=$?
[ "$rc" = 3 ] && has "$out" "oracle-check: INCOMPLETE: fetch.sh exited 3" && ! has "$out" "passed" \
  && ok "a failed download: the oracle check is INCOMPLETE (3) even with a working toolchain" || no "ci_check download: rc=$rc '$out'"

# An install altered between fetch.sh and run.sh: ci_check.sh gets run.sh's refusal (125) and reports
# INCOMPLETE, not a program result. fetch.sh is replaced by a stub that alters the verified install
# and succeeds, so the refusal can only come from run.sh's own verification.
fetch "$TMP/art/good.tar.gz" >/dev/null || no "re-install for the tamper control failed"
cp "$TMP/oracle/fetch.sh" "$TMP/fetch.sh.real"
printf '#!/usr/bin/env bash\nprintf "mod tampered;\\n" > "$ORACLE_HOME/%s/std/lib.con"\n' "$COMMIT" > "$TMP/oracle/fetch.sh"
out="$(PATH="$TMP/bin:$PATH" ORACLE_HOME="$H" FAKE_NIX=ok bash "$TMP/oracle/ci_check.sh" 2>&1)"; rc=$?
cp "$TMP/fetch.sh.real" "$TMP/oracle/fetch.sh"
[ "$rc" = 3 ] && has "$out" "oracle-check: INCOMPLETE: run.sh refused the install" && has "$out" "differs from its manifest" \
  && ! has "$out" "passed" && ok "an install altered after fetch: the oracle check is INCOMPLETE (3) via run.sh's refusal" \
  || no "ci_check tampered install: rc=$rc '$out'"

echo
echo "check_oracle_fetch: $PASS passed, $FAIL failed"
[ "$FAIL" = 0 ] && [ "$PASS" -ge 19 ] || exit 1
