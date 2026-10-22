#!/usr/bin/env python3
"""Find identifier tokens in Lean sources, outside comments and string literals.

  lean_idents.py NAME[,NAME...] FILE...

Prints `FILE:LINE: <source line>` for every occurrence of one of the NAMEs as a whole identifier
token or as a component of a dotted name (`Proof.PackageIdentity`), in code. Code includes the
`{...}` expressions of interpolated strings (`s!"..."`, `m!"..."`, `f!"..."`). Skipped: line
comments (`--` to end of line), block comments and docstrings (`/- ... -/`, nested, including `/--`
and `/-!`), string literals (with escapes), raw strings (`r"..."`, `r#"..."#`, no escapes) and
character literals. Identifier characters are Lean 4's `isIdFirst`/`isIdRest` (ASCII letters and
digits, `_`, `'`, `!`, `?`, Lean's letter-like ranges and subscripts), so `noPackageIdentity`,
`αPackageIdentity` and `PackageIdentity₁` are different identifiers, and `λx` is `λ` then `x`. A
`«...»` component is one token compared whole: `«PackageIdentity»` is that name,
`«some PackageIdentity name»` is not. Interpolation is recognized after `s!`, `m!` or `f!` (with
optional whitespace before the quote), not after other `!` tokens such as `#eval!`.

Exit status: 0 when the files were read and tokenized to the end (the output is the verdict); 2 on
a usage or read error or an unterminated comment, string or `«` name, so a caller can tell "found
nothing" from "could not look".
"""
import sys


def _letter_like(v):
    return ((0x3B1 <= v <= 0x3C9 and v != 0x3BB) or (0x391 <= v <= 0x3A9 and v not in (0x3A0, 0x3A3))
            or 0x3CA <= v <= 0x3FB or 0x1F00 <= v <= 0x1FFE or 0x2100 <= v <= 0x214F
            or 0x1D49C <= v <= 0x1D59F)


def _subscript_alnum(v):
    return 0x2080 <= v <= 0x2089 or 0x2090 <= v <= 0x209C or 0x1D62 <= v <= 0x1D6A


def _ascii_alpha(c):
    return ("a" <= c <= "z") or ("A" <= c <= "Z")


def ident_start(c):  # Lean 4 isIdFirst
    return _ascii_alpha(c) or c == "_" or _letter_like(ord(c))


def ident_rest(c):  # Lean 4 isIdRest
    return (_ascii_alpha(c) or ("0" <= c <= "9") or c in "_'!?" or _letter_like(ord(c))
            or _subscript_alnum(ord(c)))


INTERPOLATORS = {"s", "m", "f"}


class Unterminated(Exception):
    pass


class Scanner:
    def __init__(self, text, names):
        self.t, self.n, self.names = text, len(text), names
        self.i, self.line, self.hits = 0, 1, []

    def peek(self, s):
        return self.t.startswith(s, self.i)

    def adv(self, k=1):
        for _ in range(k):
            if self.i < self.n and self.t[self.i] == "\n":
                self.line += 1
            self.i += 1

    def prev_is_ident(self):
        return self.i > 0 and ident_rest(self.t[self.i - 1])

    def block_comment(self):
        depth = 0
        start = self.line
        while True:
            if self.i >= self.n:
                raise Unterminated(f"block comment opened on line {start}")
            if self.peek("/-"):
                depth += 1
                self.adv(2)
            elif self.peek("-/"):
                depth -= 1
                self.adv(2)
                if depth == 0:
                    return
            else:
                self.adv()

    def string(self, interpolated):
        start = self.line
        self.adv()  # opening quote
        while self.i < self.n and self.t[self.i] != '"':
            c = self.t[self.i]
            if c == "\\":
                self.adv(2)
            elif interpolated and c == "{":
                self.adv()
                self.code(stop_at_brace=True)
                self.adv()  # closing brace
            else:
                self.adv()
        if self.i >= self.n:
            raise Unterminated(f"string opened on line {start}")
        self.adv()  # closing quote

    def raw_string(self):
        start = self.line
        self.adv()  # r
        hashes = 0
        while self.i < self.n and self.t[self.i] == "#":
            hashes += 1
            self.adv()
        self.adv()  # opening quote
        close = '"' + "#" * hashes
        while self.i < self.n and not self.peek(close):
            self.adv()
        if self.i >= self.n:
            raise Unterminated(f"raw string opened on line {start}")
        self.adv(len(close))

    def interpolates(self):
        # the quote at self.i opens an interpolated string iff it follows s!, m! or f!
        j = self.i - 1
        while j >= 0 and self.t[j] in " \t":
            j -= 1
        if j < 0 or self.t[j] != "!":
            return False
        k = j
        while k > 0 and ident_rest(self.t[k - 1]):
            k -= 1
        word = self.t[k:j]
        return word in INTERPOLATORS and (k == 0 or self.t[k - 1] not in "#.")

    def code(self, stop_at_brace=False):
        depth = 0
        while self.i < self.n:
            c = self.t[self.i]
            if stop_at_brace:
                if c == "{":
                    depth += 1
                elif c == "}":
                    if depth == 0:
                        return
                    depth -= 1
            if self.peek("/-"):
                self.block_comment()
            elif self.peek("--"):
                while self.i < self.n and self.t[self.i] != "\n":
                    self.adv()
            elif c == '"':
                self.string(self.interpolates())
            elif c == "r" and not self.prev_is_ident() and self.i + 1 < self.n and self.t[self.i + 1] in '"#' \
                    and self.t[self.i + 1:].lstrip("#").startswith('"'):
                self.raw_string()
            elif c == "'" and not self.prev_is_ident():
                j = self.i + 1
                if j < self.n and self.t[j] == "\\":
                    j += 1
                    while j < self.n and self.t[j] not in "'\n":
                        j += 1
                else:
                    j += 1
                if j < self.n and self.t[j] == "'":
                    self.adv(j + 1 - self.i)
                else:
                    self.adv()
            elif c == "«":
                j = self.t.find("»", self.i + 1)
                if j < 0:
                    raise Unterminated(f"« name opened on line {self.line}")
                if self.t[self.i + 1:j] in self.names:
                    self.hits.append(self.line)
                self.adv(j + 1 - self.i)
            elif ident_start(c) and not self.prev_is_ident():
                j = self.i
                while j < self.n and ident_rest(self.t[j]):
                    j += 1
                if self.t[self.i:j] in self.names:
                    self.hits.append(self.line)
                self.adv(j - self.i)
            else:
                self.adv()


def scan(text, names):
    s = Scanner(text, names)
    s.code()
    return s.hits


def main(argv):
    if len(argv) < 2:
        print(__doc__, file=sys.stderr)
        return 2
    names = set(argv[0].split(","))
    for path in argv[1:]:
        try:
            text = open(path, encoding="utf-8").read()
        except (OSError, UnicodeDecodeError) as e:
            print(f"lean_idents: cannot read {path}: {e}", file=sys.stderr)
            return 2
        lines = text.split("\n")
        try:
            hits = scan(text, names)
        except Unterminated as e:
            print(f"lean_idents: {path}: unterminated {e}", file=sys.stderr)
            return 2
        for ln in sorted(set(hits)):
            print(f"{path}:{ln}: {lines[ln - 1].strip()}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
