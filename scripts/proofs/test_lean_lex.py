#!/usr/bin/env python3
"""Trust-boundary regressions; LEAN_LEX_PATH can select a pre-fix lexer."""
import contextlib
import importlib.util
import io
import os
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location(
    "lean_lex", os.environ.get("LEAN_LEX_PATH", str(Path(__file__).with_name("lean_lex.py"))))
lexer = importlib.util.module_from_spec(spec)
spec.loader.exec_module(lexer)


class TrustLexer(unittest.TestCase):
    def test_interpolation_comment_spoof(self):
        for gap in (" ", "\n", "/- nested /- comment -/ -/", "-- comment\n"):
            for prefix in ("s!", "m!", "f!"):
                with self.subTest(prefix=prefix, gap=gap), tempfile.TemporaryDirectory() as tmp:
                    source = (f'def a := {prefix}{gap}"{{("/-")}}"\n'
                              'set_option maxRecDepth 100000 in\n'
                              'theorem bad : True := by decide +kernel\n'
                              'example : False := by sorry\n'
                              f'def b := {prefix}{gap}"{{("-/")}}"\n')
                    path = Path(tmp) / "Spoof.lean"
                    path.write_text(source)
                    output = io.StringIO()
                    with contextlib.redirect_stdout(output):
                        rc = lexer.scan(r"\b(set_option|sorry)\b", [path])
                    self.assertEqual(rc, 0)
                    self.assertIn("example : False := by sorry", output.getvalue())
                    self.assertIn("set_option", output.getvalue())
                    self.assertEqual(lexer.count_examples(source), 1)
                    with contextlib.redirect_stdout(io.StringIO()):
                        lexer.examples(tmp, str(Path(tmp) / "out"), ["Spoof.lean"])
                    self.assertIn("noncomputable def", (Path(tmp) / "out/Spoof.lean").read_text())

    def test_ambiguous_or_invalid_literals_fail_closed(self):
        for source in ('def x := "{--"', 'def x := custom "{("/-")}"',
                       'def x := "\\q"', "def x := '\\q'", "def x := 'unterminated",
                       'def x := r##"unterminated"#', 'def x := s!"{1',
                       '/- unterminated', 'def x := "\\\n\ntext"'):
            with self.subTest(source=source), self.assertRaises(lexer.LexError):
                lexer.lex(source)

    def test_literal_boundaries_and_offsets(self):
        source = ('def α\' := 1\n'
                  'def c := [\'{\', \'}\', \'\\\'\', \'\\\\\', \'"\', \'\\x41\', \'\\u0041\']\n'
                  'def d := ⟨\'"\', \'/\'⟩\n'
                  'def r := r##"{ "# /- -- }"##\n'
                  'def s := "gap\\\n  text"\n'
                  'def v := xs[i]\'(by omega)\n'
                  '/- outer /- inner -/ end -/\n'
                  'example : True := by trivial -- comment\n')
        result = lexer.lex(source)
        self.assertEqual(len(result), len(source))
        self.assertEqual([i for i, c in enumerate(source) if c == '\n'],
                         [i for i, c in enumerate(result) if c == '\n'])
        self.assertEqual(lexer.count_examples(source), 1)

    def test_compiler_import_graph_and_banned_words(self):
        header = lexer.compiler_header()
        for module, allowed in (("Concrete.Lex", True), ("Lean.Meta.Basic", True),
                                ("LeanCompilerBackend.AMD64", True), ("Std", False),
                                ("Init", False), ("Lean.DoesNotExist", False),
                                ("Concrete.DoesNotExist", False)):
            self.assertEqual(bool(header.match("import " + module)), allowed, module)
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "C.lean"
            for command in ("import Std", "set_option maxRecDepth 100000", "def bad := sorry"):
                path.write_text('import Concrete.Lex\n' + command + '\n')
                output = io.StringIO()
                with contextlib.redirect_stdout(output):
                    rc = lexer.scan(r"\b(set_option|sorry)\b", [path], header)
                self.assertEqual(rc, 0)
                self.assertIn(command, output.getvalue())


if __name__ == "__main__":
    unittest.main()
