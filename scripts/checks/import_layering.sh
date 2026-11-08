#!/usr/bin/env bash
# IMPORT LAYERING (N11): one-way dependency boundaries between ConcreteSpec, Concrete, and test
# drivers/generators under scripts/checks/.
#
#   1. ConcreteSpec (ConcreteSpec.lean, ConcreteSpec/**/*.lean) may import only existing
#      ConcreteSpec.* modules. It may never import Concrete.*, LeanCompilerBackend.*, Lean.*, or any
#      test/generator module (ConcreteSpecDiff.*, *SpecDiff*, scripts.*, Tests.*).
#   2. Concrete (Concrete.lean, Concrete/**/*.lean) may import only existing Concrete.* and
#      ConcreteSpec.* modules, LeanCompilerBackend.*, and core Lean/Std/Init modules. It may never
#      import or house any test driver or generator (ConcreteSpecDiff.*, *SpecDiff*, scripts.*,
#      Tests.*).
#   3. Every import must appear in the module header (one module per line, before the first
#      declaration/command), checked over comment- and literal-stripped tokens so comments and string
#      literals cannot trigger false positives and comments/interpolations cannot hide imports.
#
# Negative controls (N11) plant forbidden imports into private copies, verify that the copy changed
# (! cmp -s), and require the check to fail naming the exact file and forbidden import.
set -uo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR" || exit 2

TMP="$(mktemp -d)" || { echo "FATAL: mktemp failed" >&2; exit 2; }
trap 'rm -rf "$TMP"' EXIT
PASS=0
FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS + 1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL + 1)); }

check_tree() {
  local tree="$1"
  python3 -I - "$tree" <<'PY'
import re
import sys
from pathlib import Path

root = Path(sys.argv[1]).resolve()


class LexError(Exception):
    pass


def strip_comments_and_literals(src: str) -> str:
    """Blank comments and string/char/quoted-name literals to spaces, keeping newlines and
    recursively lexing s!/m!/f! interpolation blocks `{ ... }` as code."""
    n, out = len(src), []

    def emit_blank(text: str):
        out.append("".join("\n" if ch == "\n" else " " for ch in text))

    def is_ident(i: int) -> bool:
        return i >= 0 and (src[i].isalnum() or src[i] in "_'!?." or ord(src[i]) > 127)

    def comment(i: int) -> int:
        depth = 0
        while i < n:
            if src.startswith("/-", i):
                depth += 1
                out.append("  ")
                i += 2
            elif src.startswith("-/", i):
                depth -= 1
                out.append("  ")
                i += 2
                if depth == 0:
                    return i
            else:
                out.append("\n" if src[i] == "\n" else " ")
                i += 1
        raise LexError("unterminated block comment")

    def string(i: int) -> int:
        interp = src[i - 2:i] in ("s!", "m!", "f!") and not is_ident(i - 3)
        out.append(" ")
        i += 1
        while i < n:
            c = src[i]
            if c == "\\":
                emit_blank(src[i:i + 2])
                i += 2
            elif c == '"':
                out.append(" ")
                return i + 1
            elif c == "{" and interp:
                out.append(" ")
                i = code(i + 1, True)
            else:
                out.append("\n" if c == "\n" else " ")
                i += 1
        raise LexError("unterminated string literal")

    def raw_string(i: int) -> int:
        j = i + 1
        while j < n and src[j] == "#":
            j += 1
        close = '"' + "#" * (j - i - 1)
        k = src.find(close, j + 1)
        if k < 0:
            raise LexError("unterminated raw string literal")
        emit_blank(src[i:k + len(close)])
        return k + len(close)

    def code(i: int, inner: bool) -> int:
        depth = 0
        while i < n:
            c = src[i]
            if src.startswith("/-", i):
                i = comment(i)
            elif src.startswith("--", i):
                j = src.find("\n", i)
                j = n if j < 0 else j
                out.append(" " * (j - i))
                i = j
            elif c == '"':
                i = string(i)
            elif c == "«":
                k = src.find("»", i + 1)
                if k < 0:
                    raise LexError("unterminated quoted identifier")
                emit_blank(src[i:k + 1])
                i = k + 1
            elif c == "r" and not is_ident(i - 1) and re.match(r'r#*"', src[i:]):
                i = raw_string(i)
            elif c == "'" and not is_ident(i - 1):
                m = re.match(r"'(\\(x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|.)|[^\\'\n])'", src[i:])
                if m:
                    emit_blank(m.group(0))
                    i += len(m.group(0))
                else:
                    out.append(c)
                    i += 1
            elif inner and c == "{":
                depth += 1
                out.append(c)
                i += 1
            elif inner and c == "}":
                out.append(" " if depth == 0 else c)
                i += 1
                if depth == 0:
                    return i
                depth -= 1
            else:
                out.append(c)
                i += 1
        if inner:
            raise LexError("unterminated string interpolation")
        return i

    code(0, False)
    return "".join(out)


IMPORT_LINE = re.compile(r"^import\s+([A-Za-z0-9_.]+)\s*$")
SPEC_MOD = re.compile(r"^ConcreteSpec(\.[A-Za-z0-9_]+)*$")
COMP_MOD = re.compile(r"^Concrete(\.[A-Za-z0-9_]+)*$")
ALLOWED_EXT_MOD = re.compile(r"^(LeanCompilerBackend|Lean|Std|Init)(\.[A-Za-z0-9_]+)*$")
SPECDIFF_TOKEN = re.compile(r"SpecDiff|ConcreteSpecDiff|^Tests(\.|$)|^scripts(\.|$)")


def module_source_path(mod_name: str) -> Path:
    return root / (mod_name.replace(".", "/") + ".lean")


def collect_files(layer_root: str):
    top = root / f"{layer_root}.lean"
    folder = root / layer_root
    files = []
    if top.is_file():
        files.append(top)
    if folder.is_dir():
        files.extend(sorted(folder.rglob("*.lean")))
    return files


errors = []

spec_files = collect_files("ConcreteSpec")
comp_files = collect_files("Concrete")
if not spec_files:
    errors.append("ConcreteSpec: no source files found")
if not comp_files:
    errors.append("Concrete: no source files found")

for layer, files in (("ConcreteSpec", spec_files), ("Concrete", comp_files)):
    for path in files:
        rel = path.relative_to(root).as_posix()
        if SPECDIFF_TOKEN.search(rel):
            errors.append(f"{rel}:1: forbidden test/generator file {rel} in {layer} layer")
        try:
            raw = path.read_text(encoding="utf-8")
            lexed = strip_comments_and_literals(raw)
        except Exception as exc:
            errors.append(f"{rel}:1: cannot lex source: {exc}")
            continue

        lines = lexed.split("\n")
        in_header = True
        for lineno, line in enumerate(lines, 1):
            stripped = line.strip()
            if not stripped:
                continue
            if in_header and stripped.startswith("import"):
                m = IMPORT_LINE.match(stripped)
                if not m:
                    errors.append(f"{rel}:{lineno}: malformed import header '{stripped}'")
                    continue
                mod = m.group(1)
                if SPECDIFF_TOKEN.search(mod):
                    errors.append(
                        f"{rel}:{lineno}: forbidden test/generator import {mod} in {layer} layer"
                    )
                    continue
                if layer == "ConcreteSpec":
                    if not SPEC_MOD.match(mod):
                        errors.append(f"{rel}:{lineno}: forbidden import {mod} in ConcreteSpec layer")
                    elif not module_source_path(mod).is_file():
                        errors.append(
                            f"{rel}:{lineno}: ConcreteSpec import {mod} has no source file"
                        )
                else:
                    if COMP_MOD.match(mod):
                        if not module_source_path(mod).is_file():
                            errors.append(
                                f"{rel}:{lineno}: Concrete import {mod} has no source file"
                            )
                    elif SPEC_MOD.match(mod):
                        if not module_source_path(mod).is_file():
                            errors.append(
                                f"{rel}:{lineno}: ConcreteSpec import {mod} has no source file"
                            )
                    elif not ALLOWED_EXT_MOD.match(mod):
                        errors.append(f"{rel}:{lineno}: forbidden import {mod} in Concrete layer")
            else:
                in_header = False
                if re.search(r"(?<![\w.'!?])import(?![\w'!?])", stripped):
                    errors.append(f"{rel}:{lineno}: import outside module header: '{stripped}'")
                if re.search(r"\b(namespace|open)\s+[A-Za-z0-9_.]*SpecDiff", stripped):
                    errors.append(
                        f"{rel}:{lineno}: forbidden SpecDiff namespace reference in {layer} layer"
                    )

if errors:
    for err in errors:
        print(err)
    sys.exit(1)
print(
    f"import_layering: {len(spec_files)} ConcreteSpec files and {len(comp_files)} Concrete files respect layer boundaries"
)
PY
}

copy_tree() {
  local dst="$1"
  mkdir -p "$dst"
  cp "$ROOT_DIR/ConcreteSpec.lean" "$ROOT_DIR/Concrete.lean" "$dst/"
  cp -R "$ROOT_DIR/ConcreteSpec" "$dst/ConcreteSpec"
  cp -R "$ROOT_DIR/Concrete" "$dst/Concrete"
}

run_control() { # run_control LABEL REL_FILE EXPECTED_DIAG MUTATION_FN
  local label="$1" rel="$2" expected="$3" fn="$4" dir="$TMP/ctl-$1" out rc=0
  copy_tree "$dir" || { no "control $label: failed to copy tree"; return; }
  "$fn" "$dir" || { no "control $label: mutation function failed"; return; }
  if cmp -s "$ROOT_DIR/$rel" "$dir/$rel" 2>/dev/null; then
    no "control $label: mutation did not change $rel"
    return
  fi
  out="$(check_tree "$dir" 2>&1)" || rc=$?
  if [ "$rc" -ne 0 ] && has_match -F "$expected" <<<"$out"; then
    ok "control $label: refused for planted reason ($expected)"
  else
    no "control $label: not refused with '$expected' (rc=$rc): $out"
  fi
}

plant_compiler_specdiff() {
  { printf 'import ConcreteSpecDiff.SpecGen\n'; cat "$1/Concrete.lean"; } >"$1/Concrete.lean.new"
  mv "$1/Concrete.lean.new" "$1/Concrete.lean"
}

plant_compiler_internal_specdiff() {
  printf 'namespace Concrete.SpecDiffRandom\nend Concrete.SpecDiffRandom\n' >"$1/Concrete/SpecDiffRandom.lean"
  { printf 'import Concrete.SpecDiffRandom\n'; cat "$1/Concrete.lean"; } >"$1/Concrete.lean.new"
  mv "$1/Concrete.lean.new" "$1/Concrete.lean"
}

plant_spec_imports_compiler() {
  { printf 'import Concrete.BIR\n'; cat "$1/ConcreteSpec.lean"; } >"$1/ConcreteSpec.lean.new"
  mv "$1/ConcreteSpec.lean.new" "$1/ConcreteSpec.lean"
}

plant_spec_imports_specdiff() {
  { printf 'import ConcreteSpecDiff.Random\n'; cat "$1/ConcreteSpec/Spec/Slice0.lean"; } >"$1/ConcreteSpec/Spec/Slice0.lean.new"
  mv "$1/ConcreteSpec/Spec/Slice0.lean.new" "$1/ConcreteSpec/Spec/Slice0.lean"
}

plant_compiler_unknown_import() {
  { printf 'import UnknownPkg.Helper\n'; cat "$1/Concrete/BIR.lean"; } >"$1/Concrete/BIR.lean.new"
  mv "$1/Concrete/BIR.lean.new" "$1/Concrete/BIR.lean"
}

plant_late_import() {
  printf '\nimport Concrete.BIR\n' >>"$1/Concrete/LL1.lean"
}

echo "=== production import layering ==="
if out="$(check_tree "$ROOT_DIR" 2>&1)"; then
  ok "$out"
else
  no "repository violates import layering: $out"
fi

echo "=== negative controls (N11) ==="
run_control n11-compiler-imports-specdiff Concrete.lean \
  "Concrete.lean:1: forbidden test/generator import ConcreteSpecDiff.SpecGen in Concrete layer" \
  plant_compiler_specdiff
run_control n11-compiler-imports-internal-specdiff Concrete.lean \
  "Concrete.lean:1: forbidden test/generator import Concrete.SpecDiffRandom in Concrete layer" \
  plant_compiler_internal_specdiff
run_control n11-spec-imports-compiler ConcreteSpec.lean \
  "ConcreteSpec.lean:1: forbidden import Concrete.BIR in ConcreteSpec layer" \
  plant_spec_imports_compiler
run_control n11-spec-imports-specdiff ConcreteSpec/Spec/Slice0.lean \
  "ConcreteSpec/Spec/Slice0.lean:1: forbidden test/generator import ConcreteSpecDiff.Random in ConcreteSpec layer" \
  plant_spec_imports_specdiff
run_control n11-compiler-unknown-import Concrete/BIR.lean \
  "Concrete/BIR.lean:1: forbidden import UnknownPkg.Helper in Concrete layer" \
  plant_compiler_unknown_import
run_control n11-late-import Concrete/LL1.lean \
  "import outside module header: 'import Concrete.BIR'" \
  plant_late_import

copy_tree "$TMP/ctl-comment"
printf '\n-- import ConcreteSpecDiff.SpecGen\n' >>"$TMP/ctl-comment/Concrete.lean"
if ! cmp -s "$ROOT_DIR/Concrete.lean" "$TMP/ctl-comment/Concrete.lean" &&
   check_tree "$TMP/ctl-comment" >/dev/null 2>&1; then
  ok "control n11-comment-ignored: commented-out import in Concrete.lean is ignored"
else
  no "control n11-comment-ignored: commented-out import was rejected"
fi

echo "import_layering: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
