import runpy
from pathlib import Path
runpy.run_path(str(Path(__file__).with_name('suite_bootstrap.py')))

"""Exact isolated-copy controls for direct execution and complete inventories."""
from pathlib import Path
import sys
import tempfile
import unittest
from process_adapter import process_run
from runtime_support import runtime_environment

ROOT = Path(__file__).resolve().parent


class Inventory(unittest.TestCase):
    def check_copy(self, name, transform, diagnostic):
        with tempfile.TemporaryDirectory() as scratch:
            directory = Path(scratch) / 'scripts/checks/scheduled'
            directory.mkdir(parents=True)
            for dependency in ROOT.glob('*.py'):
                if dependency.stem != name:
                    (directory / dependency.name).symlink_to(dependency)
            (directory / (name + '.py')).write_text(
                "import runpy\nfrom pathlib import Path\nrunpy.run_path(str(Path(__file__).with_name('suite_bootstrap.py')))\n" +
                transform((ROOT / (name + '.py')).read_text()))
            result = process_run([sys.executable, '-I', str(ROOT / 'suite_adapter.py'),
                                  str(directory), name], env=runtime_environment(),
                                 cwd=ROOT.parents[2], timeout=15, text=True, capture_output=True)
            self.assertEqual(result.returncode, 1, result.stderr)
            self.assertEqual(result.stderr.splitlines()[-1], diagnostic, result.stderr)
            self.assertEqual(result.stdout, '')

    def test_premature_main_controls(self):
        for name, anchor in (('test_observe', 'class MergeHistory'),
                             ('test_protocol', 'class ExplicitIdentity')):
            with self.subTest(module=name):
                def plant(source):
                    offset = source.rindex('\nif __name__ == "__main__":')
                    dispatch, source = source[offset:], source[:offset]
                    self.assertIn(anchor, source)
                    return source.replace(anchor, dispatch + '\n\n' + anchor, 1)
                self.check_copy(name, plant,
                                'FAIL scheduled inventory: direct/discovery mismatch ' + name)

    def test_failed_module_control(self):
        self.check_copy('test_hermetic', lambda s: s.replace(
            'class Hermeticity(unittest.TestCase):',
            'class Hermeticity(unittest.TestCase):\n    def test_planted_failure(self): self.fail("planted module failure")'),
            'FAIL scheduled inventory: module failed test_hermetic')

    def test_zero_inventory_control(self):
        self.check_copy('test_hermetic', lambda s:
                        'from suite_adapter import direct_main\nif __name__ == "__main__": direct_main()\n',
                        'FAIL scheduled inventory: zero tests test_hermetic')

    def test_matching_truncated_inventories_control(self):
        self.check_copy('test_hermetic', lambda s:
                        'import suite_adapter\n_original = suite_adapter.execute\n'
                        'def truncated(module):\n'
                        '    report = _original(module)\n'
                        '    for key in ("inventory", "started", "outcomes"): report[key] = report[key][:1]\n'
                        '    return report\n'
                        'suite_adapter.execute = truncated\n' + s,
                        'FAIL scheduled inventory: direct/discovery mismatch test_hermetic')

    def test_isolated_direct_validation(self):
        controls = (
            ('', [], 'zero tests'),
            ('class First(unittest.TestCase):\n    def test_first(self): pass\n'
             'if __name__ == "__main__": direct_main()\n'
             'class Later(unittest.TestCase):\n    def test_later(self): pass\n', [], 'direct/discovery mismatch'),
            ('', ['--unknown'], 'unexpected arguments'),
            ('', ['--scheduled-report'], 'unexpected arguments'),
        )
        for source, args, diagnostic in controls:
            with self.subTest(diagnostic=diagnostic), tempfile.TemporaryDirectory() as scratch:
                module = Path(scratch) / 'test_direct_control.py'
                prefix = 'import runpy\nrunpy.run_path(' + repr(str(ROOT / 'suite_bootstrap.py')) + ')\n'
                module.write_text(prefix + 'import unittest\nfrom suite_adapter import direct_main\n' +
                                  source + '\nif __name__ == "__main__": direct_main()\n')
                result = process_run([sys.executable, '-I', str(module), *args],
                                     env=runtime_environment(), cwd=ROOT.parents[2],
                                     timeout=4, capture_output=True, text=True)
                self.assertEqual(result.returncode, 1, result.stderr)
                self.assertEqual(result.stdout, '')
                self.assertEqual(result.stderr.splitlines()[-1],
                                 'FAIL scheduled inventory: ' + diagnostic + ' test_direct_control')

    def test_skips_are_complete_ids(self):
        from suite_adapter import execute
        import types
        module = types.ModuleType('test_skip_fixture')
        module.__file__ = 'test_skip_fixture.py'
        exec('import unittest\nclass Example(unittest.TestCase):\n'
             '    @unittest.skip("fixture skip")\n    def test_skip(self): pass\n', module.__dict__)
        report = execute(module)
        self.assertEqual(report['inventory'], ['test_skip_fixture.Example.test_skip'])
        self.assertEqual(report['started'], report['inventory'])
        self.assertEqual(report['outcomes'], [['test_skip_fixture.Example.test_skip', 'SKIP', 'fixture skip']])


if __name__ == "__main__":
    from suite_adapter import direct_main
    direct_main()
