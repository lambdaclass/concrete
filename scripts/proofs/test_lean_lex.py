#!/usr/bin/env python3
"""Trust-boundary regressions; LEAN_LEX_PATH can select a pre-fix lexer."""
import contextlib
import importlib.util
import io
import os
import subprocess
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
                    self.assertIn("noncomputable abbrev", (Path(tmp) / "out/Spoof.lean").read_text())

    def test_lean_accepts_interpolation_boundaries(self):
        # Differential witness: these are Lean terms, not merely invented lexer inputs.
        fixtures = ["import Lean\n"]
        for index, (prefix, gap) in enumerate(
                (p, g) for p in ("s!", "m!", "f!")
                for g in (" ", "\n", "/- nested /- comment -/ -/", "-- comment\n")):
            fixtures.append(f'''namespace Witness{index}
 def a := {prefix}{gap}"{{("/-")}}"
 set_option maxRecDepth 100000 in
 theorem bad : True := by decide +kernel
 example : False := by sorry
 def b := {prefix}{gap}"{{("-/")}}"
end Witness{index}
'''.replace("\n ", "\n"))
        source = "".join(fixtures)
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "Witness.lean"
            path.write_text(source)
            result = subprocess.run(["lake", "env", "lean", str(path)],
                                    text=True, capture_output=True, check=False)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertEqual(lexer.count_examples(source), 12)

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

    def test_example_renaming_line_start(self):
        source = '-- example\nexample : True := by trivial\n'
        self.assertEqual(lexer.rename_examples(source, lambda line: f'probe{line}'),
                         '-- example\nnoncomputable abbrev «probe2» : True := by trivial\n')
        for source in (' example : True := by trivial\n',
                       'set_option linter.unusedVariables false in example : True := by trivial\n'):
            with self.subTest(source=source), self.assertRaises(lexer.LexError):
                lexer.rename_examples(source, lambda line: f'probe{line}')

    def test_examples_command_error_names_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / 'Bad.lean'
            path.write_text('-- header\n example : True := by trivial\n')
            result = subprocess.run(
                ['python3', lexer.__file__, 'examples', tmp, str(Path(tmp) / 'out'), 'Bad.lean'],
                text=True, capture_output=True)
            self.assertEqual(result.returncode, 3)
            self.assertIn('Bad.lean:2: an `example`', result.stderr)
            self.assertNotIn('Traceback', result.stderr)

    def test_imports_command_lexer_error_names_proof(self):
        # Stub only build/control work; the real per-file checker must catch both
        # malformed lexemes and the shared renamer's line-start rejection.
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / 'ConcreteSpec/Proofs/Bad.lean'
            path.parent.mkdir(parents=True)
            evidence = Path(tmp) / 'ConcreteSpecEvidence/Fixture.lean'
            evidence.parent.mkdir()
            evidence.write_text('import ConcreteSpec.Spec.Basics\n')
            for body, message in (
                    ('open ConcreteSpec\n example : True := by trivial\n', '3: an `example`'),
                    ('/- unterminated', 'unterminated comment')):
                with self.subTest(body=body):
                    path.write_text('import ConcreteSpec.Spec.Basics\n' + body)
                    code = (
                        'import sys; from pathlib import Path; '
                        f'sys.path.insert(0, {str(Path(__file__).parent)!r}); '
                        'import imports; '
                        f'imports.ROOT = Path({tmp!r}); '
                        'imports.run = lambda command: ""; '
                        'imports.controls = lambda temp: None; '
                        'sys.exit(imports.main())')
                    result = subprocess.run(['python3', '-c', code], text=True, capture_output=True)
                    self.assertNotEqual(result.returncode, 0)
                    self.assertIn('ConcreteSpec/Proofs/Bad.lean:' + message, result.stderr)
                    self.assertNotIn('Traceback', result.stderr)

    def test_module_import_word_boundaries(self):
        modules = {'ConcreteSpec.Spec.Basics'}
        source = ('import ConcreteSpec.Spec.Basics\n'
                  'def importedX := 1\ndef value :=\n  importedX\n')
        self.assertEqual(lexer.module_imports(source, modules)[0], list(modules))
        with self.assertRaises(lexer.LexError):
            lexer.module_imports(source + 'def bad := 0; import ConcreteSpec.Spec.Basics\n', modules)

    def test_trust_scan_distinguishes_import_commands_from_literals(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / 'Imports.lean'
            path.write_text('import ConcreteSpec.Spec.Basics\n'
                            'def keyword := "import"\n'
                            'def forbidden := "sorry"\n'
                            'import ConcreteSpec.Spec.Scalar\n')
            output = io.StringIO()
            with contextlib.redirect_stdout(output):
                self.assertEqual(lexer.scan(r'\bsorry\b', [path]), 0)
            self.assertEqual(output.getvalue().splitlines(), [
                f'{path}:3: def forbidden := "sorry"',
                f'{path}:4: import ConcreteSpec.Spec.Scalar'])

    def test_compiler_import_graph_and_banned_words(self):
        header = lexer.compiler_header()
        for module, allowed in (("Concrete.Lex", True), ("Lean.Meta.Basic", True),
                                ("LeanCompilerBackend.AMD64", True),
                                ("Std.Data.HashMap.Lemmas", True),
                                ("Std.Data.HashMap", False),
                                ("Std.Data.String.ToNat", False),
                                ("Std.Data.String.ToInt", False), ("Std", False),
                                ("Init", False), ("Lean.DoesNotExist", False),
                                ("ConcreteSpecEvidence", False), ("ConcreteSpecEvidence.Own", False),
                                ("Concrete.DoesNotExist", False)):
            self.assertEqual(bool(header.match("import " + module)), allowed, module)
        self.assertIsNone(lexer.HEADER.match("import Std.Data.HashMap.Lemmas"))
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "C.lean"
            for command in ("import Std", "import ConcreteSpecEvidence", "set_option maxRecDepth 100000", "def bad := sorry"):
                path.write_text('import Concrete.Lex\n' + command + '\n')
                output = io.StringIO()
                with contextlib.redirect_stdout(output):
                    rc = lexer.scan(r"\b(set_option|sorry)\b", [path], header)
                self.assertEqual(rc, 0)
                self.assertIn(command, output.getvalue())

    def test_tactics_quotations_and_code_pattern(self):
        good_macro = 'namespace ConcreteSpec.Tactics\nmacro "ok_rfl" : tactic => `(tactic| rfl)\nend ConcreteSpec.Tactics\n'
        with self.assertRaises(lexer.LexError):
            lexer.lex(good_macro)
        self.assertIn("rfl", lexer.lex(good_macro, allow_quotations=True))
        for bad_q in (
            'macro "c" : tactic => `(tactic| /- comment -/ rfl)\n',
            'macro "c" : tactic => `(tactic| -- comment\n rfl)\n',
            'macro "u" : tactic => `(tactic| rfl\n',
        ):
            with self.subTest(bad_q=bad_q), self.assertRaises(lexer.LexError):
                lexer.lex(bad_q, allow_quotations=True)
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "T.lean"
            p.write_text(
                'namespace ConcreteSpec.Spec\n'
                'def bad1 : Nat := sorry\n'
                'unsafe def bad2 : Nat := 0\n'
                'def okQuoted : String := "extern"\n'
                'def «unsafe» : Nat := 1\n'
                'end ConcreteSpec.Spec\n'
            )
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                rc = lexer.scan(
                    r'\bsorry\b', [p],
                    header=lexer.HEADER,
                    code_pattern=r'\b(axiom|implemented_by|extern|unsafe)\b',
                )
            self.assertEqual(rc, 0)
            self.assertEqual(out.getvalue().splitlines(), [
                f'{p}:2: def bad1 : Nat := sorry',
                f'{p}:3: unsafe def bad2 : Nat := 0',
            ])

    def test_scan_tactics_whitelist_accepts_pure_templates(self):
        good = (
            'import ConcreteSpec.Tactics.Option\n'
            '/-! doc comment with do and Lean.Syntax.node ignored -/\n'
            'namespace ConcreteSpec.Tactics\n'
            'syntax (name := my_named) "my_named" ("[" ident,* "]")? ("at " (ident <|> "*"))? : tactic\n'
            'syntax "my_plain" ident : tactic\n'
            'macro "my_rfl" : tactic => `(tactic| rfl)\n'
            'macro "my_exact " h:ident : tactic => `(tactic| exact $h:term)\n'
            'macro_rules\n'
            '  | `(tactic| my_named) => `(tactic| rfl)\n'
            '  | `(tactic| my_named [$fs:ident,*] at $h:ident) =>\n'
            '    `(tactic| (try unfold $fs:ident* at $h:term; try simp only [$[$fs:term],*] at $h:term))\n'
            'end ConcreteSpec.Tactics\n'
        )
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "Good.lean"
            p.write_text(good)
            out = io.StringIO()
            with contextlib.redirect_stdout(out):
                rc = lexer.scan_tactics(r'\b(sorry|set_option|do)\b', [p],
                                        code_pattern=r'\b(axiom|implemented_by|extern|unsafe)\b')
            self.assertEqual(rc, 0)
            self.assertEqual(out.getvalue(), "")

    def test_scan_tactics_whitelist_refuses_meta_and_non_tactic_forms(self):
        cases = [
            ("command_macro", 'macro "c" : command => `(command| #check 0)'),
            ("command_macro_tac_rhs", 'macro "c" : command => `(tactic| rfl)'),
            ("term_macro", 'macro "t" : term => `(term| 0)'),
            ("attr_macro", 'macro "a" : attr => `(attr| simp)'),
            ("syntax_command", 'syntax "c" : command'),
            ("syntax_qualified_name", 'syntax (name := Lean.Parser.Command.eval) "c" : tactic'),
            ("elab", 'elab "e" : tactic => pure ()'),
            ("elab_rules", 'elab_rules | `(tactic| rfl) => pure ()'),
            ("do_rhs", 'macro "d" : tactic => do `(tactic| rfl)'),
            ("hand_syntax_node", 'macro "n" : tactic => pure ⟨Lean.Syntax.node .none .anonymous #[]⟩'),
            ("dot_antiquot_escape", 'macro "a" : tactic => `(tactic| $(⟨.node .none (.str .anonymous ("set_" ++ "option")) #[]⟩))'),
            ("token_antiquot", 'macro "ta" : tactic => `(tactic| rfl%$(x))'),
            ("double_backtick", 'macro "db" : tactic => ``(tactic| rfl)'),
            ("nested_quotation", 'macro "nq" : tactic => `(tactic| exact `(command| #check 0))'),
            ("unbound_rhs_var", 'macro "ub" : tactic => `(tactic| exact $x)'),
            ("trailing_rhs_combinator", 'macro "tr" : tactic => `(tactic| rfl) <|> `(tactic| trivial)'),
            ("macro_rules_cmd_lhs", 'macro_rules | `(command| c) => `(tactic| rfl)'),
            ("macro_rules_cmd_rhs", 'macro_rules | `(tactic| c) => `(command| #check 0)'),
            ("macro_rules_do_rhs", 'macro_rules | `(tactic| c) => do `(tactic| rfl)'),
            ("quot_set_option", 'macro "qo" : tactic => `(tactic| set_option maxRecDepth 10 in rfl)'),
            ("quot_eval", 'macro "qe" : tactic => `(tactic| #eval 1)'),
            ("quot_run_tac", 'macro "qt" : tactic => `(tactic| run_tac pure ())'),
            ("quot_run_cmd", 'macro "qc" : tactic => `(tactic| run_cmd pure ())'),
            ("quot_native_decide", 'macro "qn" : tactic => `(tactic| native_decide)'),
            ("quot_interp_string", 'macro "qi" : tactic => `(tactic| exact s!"{(")")}")'),
            ("unicode_prime_trailing_rhs", 'macro "up" : tactic => `(tactic| intro α\') <|> `(tactic| rfl)'),
            ("def_in_tactics", 'def helper : Nat := 0'),
            ("example_in_tactics", 'example : 1 = 1 := rfl'),
        ]
        with tempfile.TemporaryDirectory() as tmp:
            for name, body in cases:
                with self.subTest(case=name):
                    p = Path(tmp) / f"{name}.lean"
                    p.write_text(f'namespace ConcreteSpec.Tactics\n{body}\nend ConcreteSpec.Tactics\n')
                    out = io.StringIO()
                    with contextlib.redirect_stdout(out):
                        rc = lexer.scan_tactics(r'\b(sorry|native_decide|set_option|run_tac|run_cmd|do)\b|#eval\b', [p])
                    self.assertEqual(rc, 0)
                    self.assertIn("tactics whitelist:", out.getvalue(), f"{name} was not rejected by whitelist")

        for bad_imp in ('import Lean\n', 'import Lean.Elab\n', 'import Lean.Meta\n',
                        'import ConcreteSpec.Tactics\n', 'import ConcreteSpec.Spec.Basics\n'):
            with self.subTest(bad_imp=bad_imp.strip()), tempfile.TemporaryDirectory() as tmp:
                p = Path(tmp) / "BadImp.lean"
                p.write_text(bad_imp + 'namespace ConcreteSpec.Tactics\nend ConcreteSpec.Tactics\n')
                out = io.StringIO()
                with contextlib.redirect_stdout(out):
                    rc = lexer.scan_tactics(r'\bsorry\b', [p])
                self.assertEqual(rc, 0)
                self.assertTrue(out.getvalue().strip(), f"{bad_imp.strip()} was not rejected")


if __name__ == "__main__":
    unittest.main()
