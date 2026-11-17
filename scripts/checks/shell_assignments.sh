#!/usr/bin/env bash
# Bash expands the right-hand sides of a local declaration before assigning any of them.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
cd "$(dirname "${BASH_SOURCE[0]}")/../.."
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

lint() {
  shellcheck --norc --shell=bash --include=SC2318 --format=gcc "$@"
}

# The bad declaration lives in a sourced library without a shebang.
cat > "$tmp/bad.sh" <<'EOF'
fixture() {
  local name="$1" \
    path="${name}.con"
  printf '%s\n' "$path"
}
EOF
cat > "$tmp/good.sh" <<'EOF'
#!/usr/bin/env bash
fixture() {
  local name="$1"
  local path="${name}.con" suffix="$2"
  printf '%s%s\n' "$path" "$suffix"
}
EOF

lint "$tmp/good.sh"
rc=0
out="$(lint "$tmp/bad.sh")" || rc=$?
if [ "$rc" -ne 1 ] || [[ "$out" != *'[SC2318]'* ]]; then
  printf 'shell_assignments: expected SC2318 on the planted declaration (exit %s)\n%s\n' "$rc" "$out" >&2
  exit 1
fi
echo "shell_assignments: dependent declaration refused and split declaration accepted"

# Include extensionless bash/sh entry points and shell libraries without a shebang.
find scripts .githooks -type f -print0 > "$tmp/files"
shell_files=()
while IFS= read -r -d '' path; do
  case "$path" in
    *.sh) shell_files+=("$path") ;;
    *)
      first=
      IFS= read -r first < "$path" || :
      if [[ "$first" =~ ^\#!.*[/[:blank:]](bash|sh)([[:blank:]]|$) ]]; then
        shell_files+=("$path")
      fi ;;
  esac
done < "$tmp/files"
lint "${shell_files[@]}"
