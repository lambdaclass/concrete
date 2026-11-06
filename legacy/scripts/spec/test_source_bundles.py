"""Source-bundle isolation and rejection tests; requires no compiler or Lean build."""
import pathlib
import tempfile
import unittest

import spec


class SourceBundles(unittest.TestCase):
    def test_single_file_is_unchanged(self):
        body = 'fn main() -> u8 { return 42; }\n'
        self.assertEqual(spec.example_files(body), {'main.con': body})

    def test_bundle_writes_separate_modules_and_isolates_examples(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = pathlib.Path(tmp)
            for index, answer in enumerate((41, 42)):
                body = ('// spec-file: main.con\nmod answer;\n'
                        '// spec-file: answer.con\n'
                        f'pub fn value() -> u8 {{ return {answer}; }}\n')
                src = spec.write_example_source(body, str(root / str(index)))
                self.assertEqual(pathlib.Path(src).read_text(), 'mod answer;\n')
                self.assertIn(f'return {answer};', (root / str(index) / 'answer.con').read_text())
            self.assertIn('return 41;', (root / '0' / 'answer.con').read_text())

    def test_invalid_bundles_fail_before_writing(self):
        for body in ('// spec-file: ../escape.con\nx',
                     '// spec-file: /escape.con\nx',
                     '// spec-file: child.con\nx',
                     'source\n// spec-file: main.con\nx',
                     '// spec-file: main.con\nx\n// spec-file: main.con\ny'):
            with self.subTest(body=body), tempfile.TemporaryDirectory() as tmp:
                directory = pathlib.Path(tmp) / 'example'
                with self.assertRaises(ValueError):
                    spec.write_example_source(body, str(directory))
                self.assertEqual(list(directory.glob('*.con')), [])


if __name__ == '__main__':
    unittest.main()
