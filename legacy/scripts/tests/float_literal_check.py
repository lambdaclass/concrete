#!/usr/bin/env python3
"""Float-literal rounding gate driver: runs float_literal_corpus.lean and
compares every emitted bit pattern against CPython float() (correctly rounded).
Reconstructs the literal from (m, k) exactly as the lexer sees it."""
import struct, subprocess, sys, os

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

r = subprocess.run(['lake', 'env', 'lean', '--run',
                    'scripts/tests/float_literal_corpus.lean'],
                   capture_output=True, text=True, cwd=ROOT)
if r.returncode != 0:
    print("corpus runner failed:", r.stderr[-2000:], file=sys.stderr)
    sys.exit(2)

def reconstruct(m, k):
    digits = str(m)
    if len(digits) <= k:
        return "0." + "0" * (k - len(digits)) + digits
    return digits[:-k] + "." + digits[-k:]

bad = lex_bad = 0
lines = r.stdout.strip().split('\n')
for line in lines:
    m_s, k_s, fn_s, lex_s = line.split()
    m, k, fn, lex = int(m_s), int(k_s), int(fn_s), int(lex_s)
    expected = struct.unpack('<Q', struct.pack('<d', float(reconstruct(m, k))))[0]
    if fn != expected:
        bad += 1
        if bad <= 10:
            print(f"  FAIL fn m={m} k={k} got={fn} want={expected} lit={reconstruct(m, k)}")
    if lex != expected:
        lex_bad += 1
        if lex_bad <= 10:
            print(f"  FAIL lexer lit={reconstruct(m, k)} got={lex} want={expected}")
print(f"float literals: checked {len(lines)} cases "
      f"(fn mismatches={bad}, lexer mismatches={lex_bad})")
sys.exit(1 if bad or lex_bad else 0)
