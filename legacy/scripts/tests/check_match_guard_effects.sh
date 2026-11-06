#!/usr/bin/env bash
# Bug 066: a match guard's effects must be observable, and identical under the
# interpreter and the compiled binary.
#
# Return values agree even when the bug is present — only the EFFECTS diverge — so
# every leg compares interpreter output against compiled output, never exit codes.
set -Eeuo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"
fatal() { local rc=$?; echo "FATAL: check_match_guard_effects stopped at line ${BASH_LINENO[0]} (exit $rc)" >&2; exit "$rc"; }
trap fatal ERR
CC=".lake/build/bin/concrete"
[ -x "$CC" ] || { echo "error: build first" >&2; exit 2; }
PASS=0; FAIL=0
ok() { echo "  ok   $1"; PASS=$((PASS+1)); }
no() { echo "  FAIL $1"; FAIL=$((FAIL+1)); }
T="$(mktemp -d)"; trap 'rm -rf "$T"' EXIT

# Compare interpreter and compiled OUTPUT for one program.
differential() {
  local name="$1" label="$2"
  local i c
  # The interpreter appends the return value as a final line; the compiled binary does
  # not. Drop exactly that LAST line rather than filtering numeric lines — a numeric
  # filter also ate legitimate print_int output and made two real legs read as empty.
  # `|| true` guards pipefail when a program prints nothing at all.
  i="$( { "$CC" "$T/$name.con" --interp 2>&1 || true; } | sed '$d' | tr '\n' ',')"
  if ! "$CC" "$T/$name.con" -o "$T/$name.bin" >/dev/null 2>&1; then
    no "$label — compile failed"; return
  fi
  c="$( { "$T/$name.bin" 2>&1 || true; } | tr '\n' ',')"
  if [ -z "$c" ]; then
    no "$label — compiled produced no output; inconclusive, not agreement"
  elif [ "$i" = "$c" ]; then
    ok "$label (both [$c])"
  else
    no "$label — interp=[$i] compiled=[$c]"
  fi
}

hdr='mod g {
    enum Copy E { A { v: Int }, B { v: Int } }
    fn t1() with(Console) -> Bool { print("T1\n"); return true; }
    fn f1() with(Console) -> Bool { print("F1\n"); return false; }
    fn f2() with(Console) -> Bool { print("F2\n"); return false; }
    pub fn main() with(Console) -> Int {'
ftr='    }
}'

# 1. A guard that runs and whose arm is taken.
cat > "$T/taken.con" <<EOF
$hdr
        let e: E = E::A { v: 7 };
        match e { E::A { v } if t1() => { print("took\n"); return 1; },
                  E::A { v } => { return 2; }, E::B { v } => { return 0; } }
$ftr
EOF
differential taken "a succeeding guard's effect is observable"

# 2. A FAILING guard whose effect must still run before falling through. This is the
#    case a naive fix (threading the env only on success) would still get wrong.
cat > "$T/fell.con" <<EOF
$hdr
        let e: E = E::A { v: 7 };
        match e { E::A { v } if f1() => { return 1; },
                  E::A { v } => { print("fallback\n"); return 2; },
                  E::B { v } => { return 0; } }
$ftr
EOF
differential fell "a FAILING guard's effect survives the fall-through"

# 3. Two failing guards, so the ORDER of accumulated effects is checked.
cat > "$T/two.con" <<EOF
$hdr
        let e: E = E::A { v: 7 };
        match e { E::A { v } if f1() => { return 1; },
                  E::A { v } if f2() => { return 2; },
                  E::A { v } => { print("fallback\n"); return 3; },
                  E::B { v } => { return 0; } }
$ftr
EOF
differential two "two failing guards accumulate their effects in order"

# 4. A guard on a NON-MATCHING pattern must not run at all. The unguarded A arm is
#    required for exhaustivity: a GUARDED arm does not satisfy it, since the guard may
#    fail — which is correct, and worth noting so the extra arm does not look stray. — guards are evaluated
#    only after their pattern matches.
cat > "$T/nomatch.con" <<EOF
$hdr
        let e: E = E::B { v: 7 };
        match e { E::A { v } if t1() => { return 1; },
                  E::A { v } => { return 3; },
                  E::B { v } => { print("tookB\n"); return 2; } }
$ftr
EOF
differential nomatch "a guard whose pattern does not match never runs"
if { "$CC" "$T/nomatch.con" --interp 2>&1 || true; } | has_match "T1"; then
  no "a guard ran for a pattern that did not match"
else
  ok "no guard effect leaked from a non-matching pattern"
fi

# 5. SCOPE DISCIPLINE across a failed guard. Two halves that must BOTH hold, and a
#    fix can easily get one right and the other wrong:
#      - a mutation the guard makes to an OUTER variable must persist;
#      - a mutation to an ARM-LOCAL pattern binding must vanish with the arm.
#    Restoring by dropping to the outer length gives both, because interpreter stdout
#    and outer bindings live outside the dropped region while arm bindings live inside
#    it. Leaking the whole arm environment would pass the first and fail the second.
cat > "$T/outer.con" <<'EOF'
mod g {
    enum Copy E { A { v: Int }, B { v: Int } }
    fn bump(o: &mut Int) -> Bool { *o = *o + 5; return false; }
    pub fn main() with(Console) -> Int {
        let mut outer: Int = 0;
        let e: E = E::A { v: 7 };
        match e { E::A { v } if bump(&mut outer) => { return 1; },
                  E::A { v } => { print_int(outer); print("\n"); return 2; },
                  E::B { v } => { return 0; } }
    }
}
EOF
differential outer "a failed guard's mutation of an OUTER variable persists"

cat > "$T/local.con" <<'EOF'
mod g {
    enum Copy E { A { v: Int }, B { v: Int } }
    fn bumpLocal(o: &mut Int) -> Bool { *o = *o + 100; return false; }
    pub fn main() with(Console) -> Int {
        let e: E = E::A { v: 7 };
        match e { E::A { v } if bumpLocal(&mut v) => { return 1; },
                  E::A { v } => { print_int(v); print("\n"); return 2; },
                  E::B { v } => { return 0; } }
    }
}
EOF
differential local "an ARM-LOCAL mutation does NOT leak into the next arm"
if { "$CC" "$T/local.con" --interp 2>&1 || true; } | has_match "^107$"; then
  no "arm-local binding leaked across the failed guard (saw 107, expected 7)"
else
  ok "the arm's pattern bindings did not leak past its failed guard"
fi

# 6. BINDER-FRAME RESTORATION.
#
#    The arm-local leg above cannot see a leak: the next arm rebinds the same name, so
#    a leaked binding is shadowed and invisible. Leaking the whole arm environment
#    survived that leg. The observable case was a fallback that READS the name WITHOUT
#    binding it, while an OUTER binding of that name is visible — then a leaked payload
#    wins over the outer variable.
#
#    Exit 2i SCOPE-1@1 (E0301) makes that shape illegal: a pattern binder may not reuse a
#    visible name. Without the outer binding the fallback cannot read the name at all
#    (unbound), and with a rebinding the newest binding wins regardless of a leak, so the
#    leak is no longer observable in any legal program. The leg is therefore PINNED as a
#    refusal (the exact code, on both the compile and the interpreter path), plus the
#    closest legal controls below.
cat > "$T/restore.con" <<'EOF'
mod g {
    enum Copy E { A { v: Int }, B { v: Int } }
    fn eff(o: &mut Int) -> Bool { *o = *o + 5; return false; }
    pub fn main() with(Console) -> Int {
        let x: Int = 1;
        let mut outer: Int = 0;
        let e: E = E::A { v: 99 };
        match e {
            E::A { x } if eff(&mut outer) => { return 0; },
            E::A { w } => { print_int(x); print("\n"); print_int(outer); print("\n"); return 1; },
            E::B { w } => { return 2; }
        }
    }
}
EOF
rc_out="$( { "$CC" "$T/restore.con" -o "$T/restore.bin" 2>&1 && echo "__ACCEPTED__"; } || true)"
ri_out="$( { "$CC" "$T/restore.con" --interp 2>&1 && echo "__ACCEPTED__"; } || true)"
if grep -qF "(E0301)" <<<"$rc_out" && ! grep -qF "__ACCEPTED__" <<<"$rc_out"; then
  ok "PINNED: a pattern binder shadowing an outer local is refused with E0301 (compile)"
else
  no "the shadowing restoration probe is no longer refused with E0301 at compile — re-enable the leak leg [$(head -3 <<<"$rc_out" | tr '\n' ' ')]"
fi
if grep -qF "(E0301)" <<<"$ri_out" && ! grep -qF "__ACCEPTED__" <<<"$ri_out"; then
  ok "PINNED: the interpreter refuses the same program with E0301 before running any arm"
else
  no "the interpreter did not refuse the shadowing probe with E0301 [$(head -3 <<<"$ri_out" | tr '\n' ' ')]"
fi

# CLOSEST LEGAL CONTROL for the outer half: the failed arm binds a DIFFERENT name, its
# guard mutates an outer variable, and the fallback reads the outer variable without
# binding anything of that name. Outer x must read 1 and the guard's mutation must persist.
cat > "$T/restore_legal.con" <<'EOF'
mod g {
    enum Copy E { A { v: Int }, B { v: Int } }
    fn eff(o: &mut Int) -> Bool { *o = *o + 5; return false; }
    pub fn main() with(Console) -> Int {
        let x: Int = 1;
        let mut outer: Int = 0;
        let e: E = E::A { v: 99 };
        match e {
            E::A { y } if eff(&mut outer) => { return 0; },
            E::A { w } => { print_int(x); print("\n"); print_int(outer); print("\n"); return 1; },
            E::B { w } => { return 2; }
        }
    }
}
EOF
differential restore_legal "a failed arm leaves the outer variable and its guard's outer mutation intact (expect 1 then 5)"
rout="$( { "$CC" "$T/restore_legal.con" --interp 2>&1 || true; } | sed '$d' | tr '\n' ',')"
if [ "$rout" = "1,5," ]; then
  ok "outer x=1 survives the failed arm, and the failed guard's outer mutation=5 persists"
else
  no "restoration control gave [$rout], expected [1,5,]"
fi

# CONTROL for the inner half: sibling arms binding the SAME spelling (legal: sibling scopes,
# no outer binding of that name). After the first arm's guard fails, the fallback must bind
# and read its own payload. (Both arms bind the same payload, so this cannot tell a fresh 99
# from a stale one; it pins that sibling same-name binders are legal and bind correctly.)
cat > "$T/rebind.con" <<'EOF'
mod g {
    enum Copy E { A { v: Int }, B { v: Int } }
    fn no1() -> Bool { return false; }
    pub fn main() with(Console) -> Int {
        let e: E = E::A { v: 99 };
        match e {
            E::A { x } if no1() => { return 0; },
            E::A { x } => { print_int(x); print("\n"); return 1; },
            E::B { w } => { return 2; }
        }
    }
}
EOF
differential rebind "CONTROL: a rebinding fallback in a sibling arm sees its OWN payload"
bout="$( { "$CC" "$T/rebind.con" --interp 2>&1 || true; } | sed '$d' | tr -d '\n')"
if [ "$bout" = "99" ]; then
  ok "CONTROL: the rebinding arm reads its own payload 99"
else
  no "CONTROL failed: rebinding arm read [$bout], expected 99"
fi

echo "MATCH-GUARD-EFFECTS: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ] || exit 1
