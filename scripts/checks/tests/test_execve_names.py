"""Regression checks for real strace records."""
import importlib.util
import os
from pathlib import Path
import platform
import shutil
import subprocess
import tempfile
import unittest

spec = importlib.util.spec_from_file_location("execve_names", Path(__file__).parents[1] / "lib/execve_names.py")
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)


class TraceTests(unittest.TestCase):
    def test_control_paths_real_trace(self):
        if not shutil.which("strace"):
            self.fail("strace required for executable framing controls")
        identity_spec = importlib.util.spec_from_file_location("identity", Path(__file__).parents[1] / "lib/trusted_identity.py")
        identity = importlib.util.module_from_spec(identity_spec)
        identity_spec.loader.exec_module(identity)
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            target = root / "clang\nsh"
            shutil.copyfile(shutil.which("bash"), target)
            target.chmod(0o755)
            log = root / "trace"
            subprocess.run(["strace", "-f", "-qq", "-s", "4096", "-e", "trace=execve,execveat",
                            "-o", str(log), str(target), "-c", "exit 0"], check=True, timeout=15)
            import json
            expected = "line 1: control character in executable path: " + json.dumps(str(target))
            for paths in (False, True):
                with self.assertRaises(ValueError) as error:
                    m.parse_many([log], paths=paths)
                self.assertEqual(str(error.exception), expected)
            # Both production audit entry points reject the actual trace for the same reason.
            allowed = root / "allowed"
            allowed.write_text("clang\nsh\n")
            evidence = root / "evidence"
            evidence.write_text(json.dumps({"inputs": {}}))
            with self.assertRaises(ValueError) as error:
                identity.check([log], allowed, evidence, "/unused/lcb", "native")
            self.assertEqual(str(error.exception), expected)
            result = subprocess.run(["python3", "-I", str(Path(m.__file__)), str(log)],
                                    capture_output=True, text=True, timeout=15)
            self.assertEqual(result.returncode, 1)
            self.assertEqual(result.stderr.strip(), "execve_names: " + expected)
            # A clean executed path can resolve through a symlink to a hostile target.
            link = root / "clang"
            link.symlink_to(target)
            subprocess.run(["strace", "-f", "-qq", "-e", "trace=execve,execveat",
                            "-o", str(log), str(link), "-c", "exit 0"], check=True, timeout=15)
            self.assertEqual(m.parse_many([log]), ["clang"])
            evidence.write_text(json.dumps({"inputs": {}, "role_closures": {"native": {}}}))
            with self.assertRaises(ValueError) as error:
                identity.check([log], allowed, evidence, "/unused/lcb", "native")
            self.assertEqual(str(error.exception), "control character in resolved executable path: " + json.dumps(str(target)))

    def test_split_success_and_errno(self):
        lines = [
            '20402 execve("/nix/store/tool/bin/env", ["env", ") = -1 ", "<unfinished ...>", "<... execve resumed>) = -1 "], 0x0 /* 144 vars */ <unfinished ...>',
            '20403 execve("/missing/clang", ["clang", ") = 0", "<unfinished ...>"], 0x0 <unfinished ...>',
            '20403 <... execve resumed>) = -1 ENOENT (No such file or directory)',
            '20402 <... execve resumed>) = 0',
            '[pid 20403] execveat(AT_FDCWD, "/nix/store/tool/bin/clang", ["clang", "\\") = -1 "...], 0x0, 0 <unfinished ...>',
            '[pid 20403] <... execveat resumed>, [") = -1 ", "<unfinished ...>"]) = 0',
            '20402 --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=20403} ---',
        ]
        self.assertEqual(m.parse(lines), ["clang", "env"])
        self.assertEqual(m.parse(lines, paths=True), ["/nix/store/tool/bin/clang", "/nix/store/tool/bin/env"])

    def test_fail_closed(self):
        for lines, diagnostic in [
            (['1 execve("/bin/clang", ["clang"], 0x0 <unfinished ...>'], "unfinished exec calls have no result"),
            (['1 execveat(4, "clang", ["clang"], 0x0, 0) = 0'], "unresolved relative executable"),
            (['1 <... execve resumed>) = 0'], "resumed exec has no unfinished call"),
            (
                [
                    '1 execve("/bin/clang", ["clang"], 0x0 <unfinished ...>',
                    '1 execve("/bin/lean", ["lean"], 0x0) = 0',
                ],
                "line 2: overlapping exec calls",
            ),
            (
                [
                    '1 execve("/bin/clang", ["clang"], 0x0 <unfinished ...>',
                    '1 <... execveat resumed>) = 0',
                ],
                "line 2: resumed execveat does not match unfinished execve",
            ),
            (['1 execve("/bin/clang"..., ["clang"], 0x0) = 0'], "line 1: truncated executable path"),
            (['1 execve("/bin/clang", ["clang"], 0x0) = 1'], "line 1: missing exec result"),
            (['1 execve("/bin/clang", ["clang"], 0x0) = 0 trailing'], "line 1: missing exec result"),
            (['1 execve("/bin/clang", ["clang"], 0x0 <unfinished ...>) = 0'], "line 1: unexpected '<' in exec arguments"),
            (['1 execve("/bin/clang", ["clang"'], "line 1: missing exec result"),
            (['1 execve("/bin/clang", ["unterminated, 0x0) = 0'], "line 1: unterminated string literal"),
            (['1 execve("/bin/clang", ["clang"], /* unclosed) = 0'], "line 1: unterminated comment in exec arguments"),
            (['1 execve("/bin/clang", ) = 0'], "line 1: missing exec argument before '\\)'"),
            (['1 random non-strace line'], "line 1: unrecognised exec syscall"),
        ]:
            with self.assertRaisesRegex(ValueError, diagnostic):
                m.parse(lines)

    def test_separate_sessions(self):
        with tempfile.TemporaryDirectory() as tmp:
            a, b = Path(tmp) / "a.execve", Path(tmp) / "b.execve"
            a.write_text('100 execve("/nix/store/tool/bin/lean", ["lean"], 0x0) = 0\n')
            b.write_text('100 execve("/nix/store/tool/bin/clang", ["clang"], 0x0) = 0\n')
            self.assertEqual(m.parse_many([a, b]), ["clang", "lean"])
            a.write_text('100 execve("/nix/store/tool/bin/lean", ["lean"], 0x0 <unfinished ...>\n')
            b.write_text('100 <... execve resumed>) = 0\n')
            with self.assertRaisesRegex(ValueError, "unfinished exec calls have no result"):
                m.parse_many([a, b])

    @unittest.skipUnless(
        platform.system() == "Linux" and shutil.which("strace"),
        "Linux strace required for real-trace controls",
    )
    def test_real_strace_controls(self):
        true_bin = "/usr/bin/true" if os.path.isfile("/usr/bin/true") else os.path.realpath(shutil.which("true"))
        bash_bin = shutil.which("bash")
        with tempfile.TemporaryDirectory() as tmp:
            tmp_dir = Path(tmp)

            def trace_cmd(name, cmd, *, strsize=4096, check=True):
                trace_path = tmp_dir / f"{name}.execve"
                proc = subprocess.run(
                    [
                        "strace",
                        "-f",
                        "-qq",
                        "-s",
                        str(strsize),
                        "-e",
                        "trace=execve,execveat",
                        "-o",
                        str(trace_path),
                        *cmd,
                    ],
                    capture_output=True,
                    text=True,
                    timeout=30,
                )
                if check:
                    self.assertEqual(proc.returncode, 0, proc.stderr)
                return trace_path

            # 1. Result-like text inside arguments (Codex P1 reproducer and errno variant).
            t_result = trace_cmd(
                "result_like",
                [true_bin, ") = -1 ", ") = -1 ENOENT (No such file or directory)"],
            )
            self.assertIn('") = -1 "', t_result.read_text())
            self.assertEqual(m.parse_many([t_result]), ["true"])
            self.assertEqual(m.parse_many([t_result], paths=True), [true_bin])

            # 2. Unfinished-like text inside arguments.
            t_unfinished = trace_cmd(
                "unfinished_like",
                [true_bin, "<unfinished ...>", "0x0 /* 145 vars */ <unfinished ...>"],
            )
            self.assertIn('"<unfinished ...>"', t_unfinished.read_text())
            self.assertEqual(m.parse_many([t_unfinished]), ["true"])
            self.assertEqual(m.parse_many([t_unfinished], paths=True), [true_bin])

            # 3. Resumed-like text inside arguments.
            t_resumed = trace_cmd(
                "resumed_like",
                [
                    true_bin,
                    "<... execve resumed>) = -1 ENOENT (No such file or directory)",
                    "<... execveat resumed>) = -1 ",
                ],
            )
            self.assertIn("<... execve resumed>) = -1", t_resumed.read_text())
            self.assertEqual(m.parse_many([t_resumed]), ["true"])
            self.assertEqual(m.parse_many([t_resumed], paths=True), [true_bin])

            # 4. Escaped quotes, backslashes, newlines, tabs inside arguments.
            t_quotes = trace_cmd(
                "escaped_quotes",
                [
                    true_bin,
                    'a"b\\"c\\\\") = -1 ',
                    '"<unfinished ...>"',
                    '"\n1 <... execve resumed>) = -1 \t"',
                ],
            )
            self.assertIn('\\"c\\\\\\\\\\") = -1 ', t_quotes.read_text())
            self.assertEqual(m.parse_many([t_quotes]), ["true"])
            self.assertEqual(m.parse_many([t_quotes], paths=True), [true_bin])

            # 5. Very long arguments truncated by -s (both -s 4096 and -s 24).
            long_arg_a = ") = -1 <unfinished ...> <... execve resumed>) = -1 " + ("A" * 5000)
            long_arg_b = ("B" * 4085) + '") = -1 <unfinished ...>' + ("C" * 1000)
            t_trunc_4096 = trace_cmd("trunc_4096", [true_bin, long_arg_a, long_arg_b], strsize=4096)
            self.assertIn('"...', t_trunc_4096.read_text())
            self.assertEqual(m.parse_many([t_trunc_4096]), ["true"])
            self.assertEqual(m.parse_many([t_trunc_4096], paths=True), [true_bin])

            t_trunc_24 = trace_cmd(
                "trunc_24",
                [
                    true_bin,
                    "0123456789abcdef) = -1 trailing",
                    "0123456789abcdef<unfinished ...>",
                    "0123456789abcdef<... execve resumed>) = -1 ",
                    '0123456789abcdef\\"quote) = -1 ',
                ],
                strsize=24,
            )
            self.assertIn('"...', t_trunc_24.read_text())
            self.assertEqual(m.parse_many([t_trunc_24]), ["true"])
            self.assertEqual(m.parse_many([t_trunc_24], paths=True), [true_bin])

            # Even with -s 8 (which truncates argv[0] itself), printpath keeps pathname intact.
            t_trunc_8 = trace_cmd(
                "trunc_8",
                [true_bin, ") = -1 <unfinished ...> <... execve resumed>) = -1 "],
                strsize=8,
            )
            self.assertIn('"...', t_trunc_8.read_text())
            self.assertEqual(m.parse_many([t_trunc_8]), ["true"])
            self.assertEqual(m.parse_many([t_trunc_8], paths=True), [true_bin])

            # 6. Genuinely failed exec with ') = 0', '<unfinished ...>', '<... execve resumed>) = 0' in args.
            env_bin = shutil.which("env")
            missing_bin = str(tmp_dir / "nonexistent-tool")
            t_failed_exec = trace_cmd(
                "failed_exec",
                [
                    env_bin,
                    missing_bin,
                    ") = 0",
                    "<unfinished ...>",
                    "<... execve resumed>) = 0",
                    'a"b\\") = 0',
                ],
                check=False,
            )
            self.assertIn(") = -1 ENOENT", t_failed_exec.read_text())
            self.assertEqual(m.parse_many([t_failed_exec]), ["env"])
            self.assertEqual(m.parse_many([t_failed_exec], paths=True), [env_bin])

            # Combined trace: genuinely failed exec followed by successful exec with ') = -1 ' etc.
            t_combined = trace_cmd(
                "combined",
                [
                    bash_bin,
                    "-c",
                    '"{0}" "{1}" ") = 0" "<unfinished ...>" "<... execve resumed>) = 0" 2>/dev/null; '
                    'exec "{2}" ") = -1 " "<unfinished ...>" "<... execve resumed>) = -1 " "\\") = -1 "'.format(
                        env_bin, missing_bin, true_bin
                    ),
                ],
            )
            self.assertIn(") = -1 ENOENT", t_combined.read_text())
            self.assertEqual(m.parse_many([t_combined]), ["bash", "env", "true"])
            self.assertEqual(m.parse_many([t_combined], paths=True), sorted([bash_bin, env_bin, true_bin]))


if __name__ == "__main__":
    unittest.main()
