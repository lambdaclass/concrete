#!/usr/bin/env python3
"""Shared review-output contract: select the last declaration, then validate.

Declaration detection is deliberately broader than the accepted syntax. A malformed
later declaration must never reveal an earlier accepting verdict. The two output
contracts retain their existing canonical spelling and whitespace allowances.
"""
import argparse
import re
import sys


def read_verdict(text, contract='signoff'):
    if contract not in ('signoff', 'review'):
        raise ValueError('unknown verdict contract: ' + contract)
    if contract == 'review':
        # review.sh historically permits Markdown decoration and trailing space,
        # but requires the verdict to be the final nonempty output line.
        text = text.replace('`', '').replace('*', '')
    lines = text.split('\n')
    declarations = [line for line in lines
                    if line.lstrip().lower().startswith('verdict')]
    if not declarations:
        return None
    last = declarations[-1]
    if contract == 'review':
        nonempty = [line.rstrip() for line in lines if line.strip()]
        if not nonempty or nonempty[-1] != last.rstrip():
            return None
        match = re.fullmatch(r'VERDICT: (clean|findings)\s*', last)
    else:
        match = re.fullmatch(r'\s*VERDICT: (ACCEPT|REJECT)\s*', last)
    return match.group(1) if match else None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('contract', choices=('signoff', 'review'))
    args = parser.parse_args()
    verdict = read_verdict(sys.stdin.buffer.read().decode('utf-8', errors='replace'), args.contract)
    if verdict is None:
        return 1
    print(verdict)
    return 0


if __name__ == '__main__':
    sys.exit(main())
