#!/usr/bin/env python3
"""Find identifier tokens in Lean sources, outside comments and string literals.

  lean_idents.py NAME[,NAME...] FILE...

Prints `FILE:LINE: <source line>` for every occurrence of one of the NAMEs as a whole identifier
token or as a component of a dotted name (`Proof.PackageIdentity`), in code. Code includes the
`{...}` expressions of interpolated strings (`s!"..."`, `m!"..."`, `f!"..."`). Skipped: line
comments (`--` to end of line), block comments and docstrings (`/- ... -/`, nested, including `/--`
and `/-!`), string literals (with escapes), raw strings (`r"..."`, `r#"..."#`, no escapes) and
character literals. Identifier characters follow Lean: letters (any script), digits, `_`, `'`, `!`,
`?` and subscripts, so `noPackageIdentity`, `αPackageIdentity` and `PackageIdentity₁` are different
identifiers. A `«...»` component is one token compared whole: `«PackageIdentity»` is that name,
`«some PackageIdentity name»` is not.

Exit status: 0 when the files were read (the output is the verdict); 2 on a usage or read error,
so a caller can tell "found nothing" from "could not look".
"""
import sys
import unicodedata


def ident_start(c):
    return c == "_" or c.isalpha()


def ident_rest(c):
    if c.isalnum() or c in "_'!?":
        return True
    return unicodedata.category(c) in ("No", "Lm", "Mn", "Nl")  # subscripts, modifier letters


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
        while self.i < self.n:
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
        self.adv()  # closing quote

    def raw_string(self):
        self.adv()  # r
        hashes = 0
        while self.i < self.n and self.t[self.i] == "#":
            hashes += 1
            self.adv()
        self.adv()  # opening quote
        close = '"' + "#" * hashes
        while self.i < self.n and not self.peek(close):
            self.adv()
        self.adv(len(close))

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
                interp = self.i > 0 and self.t[self.i - 1] == "!"
                self.string(interp)
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
                    self.adv()
                    continue
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
        for ln in sorted(set(scan(text, names))):
            print(f"{path}:{ln}: {lines[ln - 1].strip()}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
