#!/usr/bin/env bash
# Converted gates are knob-PROOF: neutralize an inherited legacy-echo env
# (run_tests.sh still exports it for the unconverted corpus) so fixtures
# compiled here never double-print. Dies with the knob itself.
unset CONCRETE_ECHO_RESULT

# gate_selfprint_wrap <src.con> <dst.con>: MAIN_EXIT_MODEL stage 2 — the
# compiled binary no longer echoes main's result, so wide/negative-value
# fixtures print their own: rename `fn main` and add a printing main
# (print_int at full i64 width + newline, exit 0 — byte-identical stdout to
# the legacy echo, so gate expectations survive unchanged).
# Handles module-wrapped fixtures (insert before the FINAL brace) and bare
# top-level-fn fixtures (append); any integer return width (`as Int` matches
# the legacy echo's full-i64 rendering).
gate_selfprint_wrap() {
  local src="$1" dst="$2"
  local mainline='fn main() with(Console) -> Int { print_int((gate_wrapped_main()) as Int); print_char(10); return 0; }'
  if grep -q '^\s*mod ' "$src"; then
    sed 's/fn main(/fn gate_wrapped_main(/' "$src" \
      | perl -0pe "s/\\}\\s*\\z/\n    $mainline\n}\n/" > "$dst"
  else
    sed 's/fn main(/fn gate_wrapped_main(/' "$src" > "$dst"
    printf '\n%s\n' "$mainline" >> "$dst"
  fi
}
