"""Regression checks for real strace records."""
import importlib.util
from pathlib import Path
import unittest
spec = importlib.util.spec_from_file_location("execve_names", Path(__file__).parents[1] / "lib/execve_names.py")
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)
class TraceTests(unittest.TestCase):
    def test_split_success_and_errno(self):
        lines = [
            '20402 execve("/nix/store/tool/bin/env", ["env"], 0x0 /* 144 vars */ <unfinished ...>',
            '20403 execve("/missing/clang", ["clang"], 0x0 <unfinished ...>',
            '20403 <... execve resumed>) = -1 ENOENT (No such file or directory)',
            '20402 <... execve resumed>) = 0',
            '[pid 20403] execveat(AT_FDCWD, "/nix/store/tool/bin/clang", [], 0x0, 0) = 0',
        ]
        self.assertEqual(m.parse(lines), ["clang", "env"])
        self.assertEqual(m.parse(lines, paths=True), ["/nix/store/tool/bin/clang", "/nix/store/tool/bin/env"])
    def test_fail_closed(self):
        for line, diagnostic in [
            ('1 execve("/bin/clang", [], 0x0 <unfinished ...>', "unfinished exec calls have no result"),
            ('1 execveat(4, "clang", [], 0x0, 0) = 0', "unresolved relative executable"),
            ('1 <... execve resumed>) = 0', "resumed exec has no unfinished call"),
        ]:
            with self.assertRaisesRegex(ValueError, diagnostic): m.parse([line])
if __name__ == "__main__": unittest.main()
