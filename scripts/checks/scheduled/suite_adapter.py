"""Single unittest inventory and execution report for imports and direct modules.

Reports distinguish the full loaded inventory from tests actually started. Class
fixtures/import failures remain failures even when every started test succeeded.
"""
import inspect
import json
from pathlib import Path
import sys
import unittest
sys.path.insert(0, str(Path(__file__).resolve().parent))
from process_adapter import process_run
import os
import tempfile


def inventory(module):
    loader = unittest.TestLoader()
    tests = []
    for name, value in inspect.getmembers(module, inspect.isclass):
        if issubclass(value, unittest.TestCase) and value.__module__ == module.__name__:
            tests.extend(value(method) for method in loader.getTestCaseNames(value))
    return tests


def identifier(test, module):
    value = test.id()
    prefix = module.__name__ + "."
    return module.__file__ and Path(module.__file__).stem + "." + value.removeprefix(prefix)


class Results(unittest.TextTestResult):
    def __init__(self, *args):
        super().__init__(*args)
        self.started = []
        self.outcomes = []

    def startTest(self, test):
        super().startTest(test)
        self.started.append(test)

    def addSuccess(self, test):
        super().addSuccess(test); self.outcomes.append((test, "PASS"))

    def addSkip(self, test, reason):
        super().addSkip(test, reason); self.outcomes.append((test, "SKIP", reason))

    def addFailure(self, test, error):
        super().addFailure(test, error); self.outcomes.append((test, "FAIL"))

    def addError(self, test, error):
        super().addError(test, error); self.outcomes.append((test, "ERROR"))

    def addExpectedFailure(self, test, error):
        super().addExpectedFailure(test, error); self.outcomes.append((test, "XFAIL"))

    def addUnexpectedSuccess(self, test):
        super().addUnexpectedSuccess(test); self.outcomes.append((test, "XPASS"))


def execute(module):
    tests = inventory(module)
    ids = sorted(identifier(test, module) for test in tests)
    result = unittest.TextTestRunner(resultclass=Results).run(unittest.TestSuite(tests))
    return dict(inventory=ids, started=sorted(identifier(t, module) for t in result.started),
                outcomes=sorted([identifier(row[0], module), *row[1:]] for row in result.outcomes),
                successful=result.wasSuccessful())


def validate(module, expected, discovery, direct):
    for report in (discovery, direct):
        if not report["successful"]:
            raise ValueError("FAIL scheduled inventory: module failed " + module)
        if not expected or not report["inventory"] or not report["started"]:
            raise ValueError("FAIL scheduled inventory: zero tests " + module)
    if any(report["inventory"] != expected or report["started"] != expected
           for report in (discovery, direct)):
        raise ValueError("FAIL scheduled inventory: direct/discovery mismatch " + module)
    if discovery["outcomes"] != direct["outcomes"]:
        raise ValueError("FAIL scheduled inventory: direct/discovery results differ " + module)


def direct_main():
    report = execute(sys.modules["__main__"])
    if len(sys.argv) == 3 and sys.argv[1] == "--scheduled-report":
        Path(sys.argv[2]).write_text(json.dumps(report))
    sys.exit(int(not report["successful"]))


def check_module(module, directory):
    expected = sorted(identifier(t, module) for t in inventory(module))
    discovery = execute(module)
    with tempfile.TemporaryDirectory(prefix='scheduled-direct-') as scratch:
        target = Path(scratch) / 'report.json'
        direct = process_run([sys.executable, '-I', '-c',
                             'import runpy,sys; sys.path.insert(0,sys.argv[1]); '
                             'sys.path.insert(0,sys.argv[2]); sys.argv=sys.argv[3:]; runpy.run_path(sys.argv[0],run_name="__main__")',
                             str(Path(__file__).resolve().parent), str(directory), str(Path(module.__file__).resolve()),
                             '--scheduled-report', str(target)],
                             env=dict(os.environ), cwd=Path(__file__).resolve().parents[3],
                             timeout=45, text=True, capture_output=True)
        if direct.returncode or not target.is_file():
            print(direct.stderr, end='', file=sys.stderr)
            raise ValueError('FAIL scheduled inventory: module failed ' + module.__name__)
        validate(module.__name__, expected, discovery, json.loads(target.read_text()))
    return discovery


if __name__ == '__main__':
    import importlib
    sys.modules['suite_adapter'] = sys.modules[__name__]
    try:
        directory = Path(sys.argv[1])
        sys.path.insert(0, str(directory))
        check_module(importlib.import_module(sys.argv[2]), directory)
    except ValueError as error:
        sys.exit(str(error))
