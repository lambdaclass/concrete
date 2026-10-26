#!/usr/bin/env bash
# A stand-in for the compiler in check_leanprof_profile.sh: records "cwd|args" and checks that the
# workload it was given exists (and the .marker of the source tree it was copied from, if any) (a package for `build`, a source file otherwise).
[ "${1:-}" = --version ] && { echo "concrete fake"; exit 0; }
[ -z "${FAKE_CC_LOG:-}" ] || echo "$PWD|$*|$(cat .marker 2>/dev/null)" >> "$FAKE_CC_LOG"
if [ "${1:-}" = build ]; then [ -f Concrete.toml ] || { echo "no Concrete.toml in $PWD" >&2; exit 1; }
else [ -f "${1:-}" ] || { echo "no source ${1:-}" >&2; exit 1; }; fi
echo "; fake LLVM IR"
