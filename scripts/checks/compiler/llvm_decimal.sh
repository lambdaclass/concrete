# Run from tests.sh after slice0_common.sh has built the emitter and set its bounds.
# The controls mutate copies of the actual emitter, then run the same elaborated audit.
llvm_decimal_audit() {
  local audit=scripts/checks/compiler/llvm_decimal.lean
  local emitter_source=Concrete/Backend/LLVM.lean
  local mutation
  local diagnostic
  local rc
  {
    printf '%s\n' 'import Lean' 'import Concrete.Backend.LLVM'
    cat "$audit"
  } > "$work/decimal.lean"
  build llvm-decimal "$work/decimal.log" bounded 120 lake env lean "$work/decimal.lean"
  echo "ok   LLVM emitter uses the proved decimal formatter"

  for mutation in literal interpolation; do
    {
      printf '%s\n' 'import Lean'
      python3 -I - "$emitter_source" "$mutation" <<'PY'
from pathlib import Path
import sys

source = Path(sys.argv[1]).read_text()
original = r'  | .trap s => s!"  ret {integerType 32} {decimal (256 + s)}\n"'
mutants = {
    'literal': r'  | .trap _ => "  ret i32 1_7\n"',
    'interpolation': r'  | .trap s => s!"  ret {integerType 32} {256 + s}\n"',
}
if source.count(original) != 1:
    raise SystemExit('LLVM decimal control must replace exactly one trap return')
print(source.replace(original, mutants[sys.argv[2]]), end='')
PY
      cat "$audit"
    } > "$work/decimal-control.lean"
    case "$mutation" in
      literal) diagnostic="digit in literal" ;;
      interpolation) diagnostic="non-String interpolation" ;;
    esac
    rc=0
    bounded 120 lake env lean "$work/decimal-control.lean" > "$work/decimal-control.log" 2>&1 || rc=$?
    if [ "$rc" != 1 ] ||
      ! has_match -F "LLVM decimal bypass in Concrete.Backend.LLVM.term: $diagnostic" < "$work/decimal-control.log"; then
      echo "FAIL LLVM decimal audit did not refuse the trap-return $mutation" >&2
      cat "$work/decimal-control.log" >&2
      return 1
    fi
    echo "ok   LLVM decimal audit refused the trap-return $mutation"
  done
}
llvm_decimal_audit
