#!/usr/bin/env bash
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
  # A main with NO return type is already self-reporting (prints its own
  # output, exits 0/traps) — wrapping it in print_int would be a type error.
  # Pass it through unchanged.
  if { grep -E 'fn main\([^)]*\)[[:space:]]*(with\([^)]*\))?[[:space:]]*\{' "$src" || true; } | grep -qv -- '->'; then
    cp "$src" "$dst"
    return 0
  fi
  # The printing main needs Console PLUS whatever the wrapped main declares
  # (a `with(Alloc)` fixture main is uncallable from a Console-only wrapper);
  # Console deduped if already present.
  local caps orig_caps
  orig_caps=$(grep -o 'fn main([^)]*)[[:space:]]*with(\([^)]*\))' "$src" 2>/dev/null | head -1 | sed 's/.*with(\(.*\))/\1/' || true)
  orig_caps=$(printf '%s' "$orig_caps" | sed -e 's/\bConsole\b//g' -e 's/,[[:space:]]*,/,/g' -e 's/^[[:space:]]*,//' -e 's/,[[:space:]]*$//')
  if [ -n "$orig_caps" ]; then caps="Console, $orig_caps"; else caps="Console"; fi
  local mainline="fn main() with($caps) -> Int { print_int((gate_wrapped_main()) as Int); print_char(10); return 0; }"
  if grep -q '^\s*mod ' "$src"; then
    sed 's/fn main(/fn gate_wrapped_main(/' "$src" \
      | perl -0pe "s/\\}\\s*\\z/\n    $mainline\n}\n/" > "$dst"
  else
    sed 's/fn main(/fn gate_wrapped_main(/' "$src" > "$dst"
    printf '\n%s\n' "$mainline" >> "$dst"
  fi
}
