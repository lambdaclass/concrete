#!/usr/bin/env python3
"""Print basenames of successful execve calls in a strace -f execve log."""

import json
import unicodedata
import os
import re
import sys

PID_PREFIX = re.compile(r'^\s*(?:(?:\[pid\s+([0-9]+)\]|([0-9]+))\s+)?(.*)$')
DIRFD_PREFIX = re.compile(r'^(AT_FDCWD|-?[0-9]+)\s*,\s*')
RESUMED_PREFIX = re.compile(r'^<\.\.\.\s+(execve(?:at)?)\s+resumed>')
UNFINISHED_TAIL = re.compile(r'^<unfinished \.\.\.>\s*$')
RESULT_TAIL = re.compile(
    r'^\s*=\s*(0|-[0-9]+(?:\s+E[A-Z0-9_]+(?:\s+\([^()\r\n]*\))?)?)\s*$'
)
SIGNAL_LINE = re.compile(r'^---\s+SIG[A-Z0-9_]+\s+\{[^"\r\n]*\}\s+---\s*$')

_SIMPLE_ESCAPES = {
    '"': ord('"'),
    '\\': ord('\\'),
    'a': 7,
    'b': 8,
    'f': 12,
    'n': 10,
    'r': 13,
    't': 9,
    'v': 11,
}
_ARG_TOKEN_CHARS = frozenset(
    ',0123456789abcdefABCDEFxX_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.|+-'
)


def checked_path(path, context="executable path"):
    if any(unicodedata.category(c) == "Cc" for c in path):
        raise ValueError(f"control character in {context}: {json.dumps(path, ensure_ascii=True)}")
    return path


def _skip_ws(text, idx):
    while idx < len(text) and text[idx].isspace():
        idx += 1
    return idx


def _scan_quoted_string(text, idx, line_number):
    """Scan a C/strace double-quoted string literal starting at text[idx] == '"'."""
    if idx >= len(text) or text[idx] != '"':
        raise ValueError(f"line {line_number}: expected quoted string literal")
    idx += 1
    raw = bytearray()
    while idx < len(text):
        ch = text[idx]
        if ch == '"':
            idx += 1
            truncated = text.startswith("...", idx)
            if truncated:
                idx += 3
            if 0 in raw:
                raise ValueError(f"line {line_number}: NUL byte in quoted string")
            return os.fsdecode(bytes(raw)), idx, truncated
        if ch == '\\':
            idx += 1
            if idx >= len(text):
                raise ValueError(f"line {line_number}: unterminated escape in string literal")
            esc = text[idx]
            if esc in _SIMPLE_ESCAPES:
                raw.append(_SIMPLE_ESCAPES[esc])
                idx += 1
            elif esc in '01234567':
                digits = esc
                idx += 1
                for _ in range(2):
                    if idx < len(text) and text[idx] in '01234567':
                        digits += text[idx]
                        idx += 1
                    else:
                        break
                val = int(digits, 8)
                if val > 0xFF:
                    raise ValueError(f"line {line_number}: octal escape out of range: \\{digits}")
                raw.append(val)
            elif esc == 'x':
                idx += 1
                digits = ""
                for _ in range(2):
                    if idx < len(text) and text[idx] in '0123456789abcdefABCDEF':
                        digits += text[idx]
                        idx += 1
                    else:
                        break
                if not digits:
                    raise ValueError(f"line {line_number}: invalid hex escape in string literal")
                raw.append(int(digits, 16))
            else:
                raise ValueError(f"line {line_number}: unknown escape \\{esc} in string literal")
        else:
            raw.extend(ch.encode("utf-8"))
            idx += 1
    raise ValueError(f"line {line_number}: unterminated string literal")


def _scan_args_to_end(text, idx, line_number, allow_unfinished, require_arg=False):
    """Scan syscall arguments outside the executable path to ')' or '<unfinished ...>'."""
    bracket_depth = 0
    saw_token = False
    while idx < len(text):
        ch = text[idx]
        if ch.isspace():
            idx += 1
            continue
        if ch == '<':
            if allow_unfinished and bracket_depth == 0 and UNFINISHED_TAIL.match(text[idx:]):
                return "unfinished", len(text)
            raise ValueError(f"line {line_number}: unexpected '<' in exec arguments")
        if text.startswith("/*", idx):
            close_idx = text.find("*/", idx + 2)
            if close_idx < 0:
                raise ValueError(f"line {line_number}: unterminated comment in exec arguments")
            idx = close_idx + 2
            continue
        if ch == '"':
            _, idx, _ = _scan_quoted_string(text, idx, line_number)
            saw_token = True
            continue
        if ch == '[':
            bracket_depth += 1
            saw_token = True
            idx += 1
            continue
        if ch == ']':
            if bracket_depth <= 0:
                raise ValueError(f"line {line_number}: unmatched ']' in exec arguments")
            bracket_depth -= 1
            idx += 1
            continue
        if ch == ')':
            if bracket_depth != 0:
                raise ValueError(f"line {line_number}: unclosed '[' before ')' in exec arguments")
            if require_arg and not saw_token:
                raise ValueError(f"line {line_number}: missing exec argument before ')'")
            return "completed", idx + 1
        if ch == '(' or text.startswith("*/", idx):
            raise ValueError(f"line {line_number}: unexpected token in exec arguments")
        if ch in _ARG_TOKEN_CHARS:
            if ch != ',':
                saw_token = True
            idx += 1
            continue
        raise ValueError(f"line {line_number}: unexpected character {ch!r} in exec arguments")
    raise ValueError(f"line {line_number}: missing exec result")


def parse(lines, paths=False):
    """Keep syscall state per trace; admit only completed successful executions.

    Relative execveat targets cannot be resolved without fd/cwd tracking. Reject
    them explicitly rather than guessing an identity. Absolute execveat is supported.
    """
    found, pending = set(), {}
    for line_number, raw_line in enumerate(lines, 1):
        line = raw_line.rstrip("\r\n")
        if not line.strip():
            continue
        pid_match = PID_PREFIX.match(line)
        pid = pid_match.group(1) or pid_match.group(2) or "main"
        body = pid_match.group(3)
        if not body:
            continue
        if SIGNAL_LINE.match(body) and "execve" not in body:
            continue
        if body.startswith(("execve(", "execveat(")):
            if pid in pending:
                raise ValueError(f"line {line_number}: overlapping exec calls")
            if body.startswith("execveat("):
                kind = "execveat"
                idx = _skip_ws(body, len("execveat("))
                dirfd_match = DIRFD_PREFIX.match(body[idx:])
                if not dirfd_match:
                    raise ValueError(f"line {line_number}: malformed execveat dirfd")
                idx = _skip_ws(body, idx + dirfd_match.end())
            else:
                kind = "execve"
                idx = _skip_ws(body, len("execve("))
            if idx >= len(body) or body[idx] != '"':
                raise ValueError(f"line {line_number}: missing quoted executable path")
            path, idx, truncated = _scan_quoted_string(body, idx, line_number)
            try:
                checked_path(path)
            except ValueError as error:
                raise ValueError(f"line {line_number}: {error}") from error
            if truncated:
                raise ValueError(f"line {line_number}: truncated executable path")
            idx = _skip_ws(body, idx)
            if idx >= len(body) or body[idx] not in (',', '<'):
                raise ValueError(f"line {line_number}: malformed exec arguments")
            has_comma = body[idx] == ','
            if has_comma:
                idx += 1
            state, end_idx = _scan_args_to_end(
                body, idx, line_number, allow_unfinished=True, require_arg=has_comma
            )
            if state == "unfinished":
                pending[pid] = (kind, path)
                continue
        elif resumed_match := RESUMED_PREFIX.match(body):
            resumed_kind = resumed_match.group(1)
            if pid not in pending:
                raise ValueError(f"line {line_number}: resumed exec has no unfinished call")
            expected_kind, path = pending.pop(pid)
            if resumed_kind != expected_kind:
                raise ValueError(
                    f"line {line_number}: resumed {resumed_kind} does not match unfinished {expected_kind}"
                )
            state, end_idx = _scan_args_to_end(
                body, resumed_match.end(), line_number, allow_unfinished=False, require_arg=False
            )
        else:
            raise ValueError(f"line {line_number}: unrecognised exec syscall")
        result = RESULT_TAIL.match(body[end_idx:])
        if not result:
            raise ValueError(f"line {line_number}: missing exec result")
        if result.group(1) == "0":
            if not os.path.isabs(path):
                raise ValueError(f"line {line_number}: unresolved relative executable {path!r}")
            norm_path = os.path.normpath(path)
            if path.endswith("/") or not os.path.basename(norm_path):
                raise ValueError(f"line {line_number}: invalid executable path {path!r}")
            found.add(path if paths else os.path.basename(norm_path))
    if pending:
        raise ValueError(f"unfinished exec calls have no result: {', '.join(sorted(pending))}")
    return sorted(found)


def parse_many(log_paths, paths=False):
    """Parse each trace session independently so unfinished PID state cannot cross sessions."""
    found = set()
    for log_path in log_paths:
        with open(log_path, encoding="utf-8", errors="replace") as log:
            found.update(parse(log, paths=paths))
    return sorted(found)


def main():
    if len(sys.argv) == 4 and sys.argv[1] == "--check-names":
        with open(sys.argv[2]) as f:
            allowed = set(f.read().splitlines())
        with open(sys.argv[3]) as f:
            names = json.load(f)
        if not names:
            raise ValueError("no successful exec records")
        unlisted = sorted(set(names) - allowed)
        for name in unlisted:
            print(f"FAIL tools: unlisted program '{checked_path(name)}'")
        if unlisted:
            sys.exit(1)
        return
    paths = "--paths" in sys.argv
    if paths: sys.argv.remove("--paths")
    if len(sys.argv) < 2:
        raise ValueError("usage: execve_names.py [--paths] LOG [LOG ...]")
    print(json.dumps(parse_many(sys.argv[1:], paths=paths), ensure_ascii=True))


if __name__ == "__main__":
    try:
        main()
    except (OSError, UnicodeError, ValueError) as error:
        print(f"execve_names: {error}", file=sys.stderr)
        sys.exit(1)
