from test_protocol import RegistryFixture
"""History controls: main advancing cannot hide defects or manufacture absence."""
import unittest
import os
from unittest.mock import patch
import subprocess
import observe
from test_protocol import SHA, NOW, RUNNERS, record

TIP = "b" * 40

class History(RegistryFixture, unittest.TestCase):
    def setUp(self):
        super().setUp()
        env = patch.dict(os.environ, {"GITHUB_REPOSITORY": "example/repo"}, clear=True)
        env.start(); self.addCleanup(env.stop)

    def test_tip_exact_pass(self):
        r = record(); r.update(sha=TIP, gate_revision=TIP)
        self.assertEqual(observe.observe_main([TIP, SHA], {TIP: [r]}, "daily", NOW, RUNNERS, "example/repo"), [f"PASS daily {TIP}"])

    def test_new_tip_recovers_operational_ancestor(self):
        for status, conclusion in (("MISSING", "cancelled"), ("INCOMPLETE", "incomplete")):
            old = record(); old.update(status=status, conclusion=conclusion)
            if status == "MISSING": old.update(gates=[], runner_name=None)
            else: old["gates"][0]["status"] = status
            tip = record(); tip.update(sha=TIP, gate_revision=TIP)
            self.assertEqual(observe.observe_main([TIP, SHA], {TIP: [tip], SHA: [old]}, "daily", NOW, RUNNERS, "example/repo"), [f"PASS daily {TIP}"])

    def test_ancestor_coverage(self):
        self.assertEqual(observe.observe_main([TIP, SHA], {SHA: [record()]}, "daily", NOW, RUNNERS, "example/repo"),
                         [f"PASS daily {SHA}: ancestor coverage; tip {TIP} requires its own evidence"])

    def test_ancestor_fail_never_ages_out(self):
        r = record(); r.update(status="FAIL", conclusion="failure"); r["gates"][0]["status"] = "FAIL"
        self.assertEqual(observe.observe_main([TIP, SHA], {SHA: [r]}, "daily", NOW + 999999, RUNNERS, "example/repo"),
                         [f"FAIL daily {SHA}: recorded attempt"])

    def test_stale_ancestor(self):
        self.assertEqual(observe.observe_main([TIP, SHA], {SHA: [record()]}, "daily", NOW + 999999, RUNNERS, "example/repo"),
                         [f"STALE daily {SHA}: no fresh PASS"])

    def test_missing_history(self):
        self.assertEqual(observe.observe_main([TIP, SHA], {}, "daily", NOW, {}, "example/repo"),
                         [f"MISSING daily {TIP}: absent run"])

    def test_corrupt_ancestor_does_not_hide_failure(self):
        r = record(); r.update(status="FAIL", conclusion="failure"); r["gates"][0]["status"] = "FAIL"
        corrupt = record(); corrupt.pop("completed"); corrupt["sha"] = TIP
        messages = observe.observe_main([TIP, SHA], {SHA: [r], TIP: [corrupt]}, "daily", NOW, RUNNERS, "example/repo")
        self.assertEqual(messages, [f"FAIL daily {SHA}: recorded attempt", f"INCOMPLETE daily {TIP}: 'completed'"])

    def test_malformed_note_does_not_hide_fail(self):
        r = record(); r.update(status="FAIL", conclusion="failure"); r["gates"][0]["status"] = "FAIL"
        for malformed in (None, [], {"gates": [None]}):
            messages = observe.observe_main([TIP, SHA], {TIP: [malformed], SHA: [r]}, "daily", NOW, RUNNERS, "example/repo")
            diagnostic = "invalid record shape" if not isinstance(malformed, dict) else "wrong SHA"
            self.assertEqual(messages, [f"FAIL daily {SHA}: recorded attempt", f"INCOMPLETE daily {TIP}: {diagnostic}"])

    def test_empty_note(self):
        from protocol import read_note, EvidenceError
        with patch("protocol.git", return_value=""):
            with self.assertRaisesRegex(EvidenceError, "^empty annotated note$"):
                read_note(SHA, "daily", listed=True)

    def test_all_malformed_no_false_absence(self):
        def fake_git(*args, **kwargs):
            if args[0] == "rev-list": return SHA
            if args[0] == "for-each-ref": return args[-1]
            return "blob " + SHA
        with patch.object(observe, "git", side_effect=fake_git), patch.object(observe, "read_note", side_effect=ValueError("planted malformed note")), patch("builtins.print") as output:
            self.assertEqual(observe.main(), 1)
            self.assertEqual([c.args[0] for c in output.call_args_list], [f"INCOMPLETE {tier} {SHA}: planted malformed note" for tier in ("daily", "weekly")])

    def test_git_timeout_explicit(self):
        with patch.object(observe, "git", side_effect=subprocess.TimeoutExpired("git", 60)), patch("builtins.print") as output:
            self.assertEqual(observe.main(), 1)
            output.assert_called_once_with("INCOMPLETE observer: Command 'git' timed out after 60 seconds")

if __name__ == "__main__":
    unittest.main()

class RecoveryControls(RegistryFixture, unittest.TestCase):
    def test_realistic_operational_ancestors_recover(self):
        import copy
        good = record(); good.update(sha=TIP, gate_revision=TIP)
        crashed = record(); crashed.update(status='INCOMPLETE', conclusion='incomplete',
                                          execution_error='worker exited without index', gates=[])
        dependency = copy.deepcopy(crashed); dependency['gate_revision'] = None
        partial = record(); partial.update(status='INCOMPLETE', conclusion='incomplete')
        partial['gates'][0].update(status='INCOMPLETE', controls=[])
        for old in (crashed, dependency, partial):
            with self.subTest(old=old):
                self.assertEqual(observe.observe_main([TIP, SHA], {TIP:[good], SHA:[old]},
                                 'daily', NOW, RUNNERS, 'example/repo'), [f'PASS daily {TIP}'])
                messages = observe.observe_main([SHA], {SHA:[old]}, 'daily', NOW, RUNNERS, 'example/repo')
                self.assertEqual(messages, [f'INCOMPLETE daily {SHA}: INCOMPLETE registry: ' +
                                            ('missing gate sample' if not old['gates'] else 'missing control sample/refusal')])

    def test_ancestor_substitution_never_recovers(self):
        good = record(); good.update(sha=TIP, gate_revision=TIP)
        old = record(); old['gates'][0]['command'] = ['false']
        self.assertEqual(observe.observe_main([TIP, SHA], {TIP:[good], SHA:[old]},
                         'daily', NOW, RUNNERS, 'example/repo'),
                         [f'FAIL daily {SHA}: FAIL registry: wrong command sample'])


class MergeHistory(RegistryFixture, unittest.TestCase):
    def test_replayed_second_parent_failure(self):
        import contextlib
        import datetime as dt
        import io
        import json
        import tempfile
        from protocol import git
        with tempfile.TemporaryDirectory() as scratch:
            def g(*args):
                return git('-C', scratch, *args)
            g('init', '-q')
            g('config', 'user.name', 'History control')
            g('config', 'user.email', 'history@example.invalid')
            def commit(*parents):
                tree = g('mktree')
                args = ['commit-tree', tree, '-m', 'history control']
                for parent in parents: args.extend(['-p', parent])
                return g(*args)
            root = commit()
            first = commit(root)
            replay = commit(root)
            # Distinct commits even with identical trees/timestamps.
            replay = commit(replay)
            tip = commit(first, replay)
            g('update-ref', 'refs/remotes/origin/main', tip)
            g('merge-base', '--is-ancestor', replay, tip)
            self.assertNotIn(replay, g('rev-list', '--first-parent', tip).splitlines())
            stamp = dt.datetime.now(dt.timezone.utc).isoformat()
            for tier in ('daily', 'weekly'):
                for sha, status in ((replay, 'FAIL'), (tip, 'PASS')):
                    r = record()
                    r.update(sha=sha, gate_revision=sha, tier=tier, started=stamp,
                             completed=stamp, status=status,
                             conclusion='failure' if status == 'FAIL' else 'success')
                    r['gates'][0]['status'] = status
                    g('notes', '--ref=refs/notes/concrete-gates-' + tier,
                      'add', '-m', json.dumps(r), sha)
            import protocol
            from pathlib import Path
            output = io.StringIO()
            with patch.dict(os.environ, {'GITHUB_REPOSITORY': 'example/repo'}), \
                 patch.object(observe, 'inventory', return_value=RUNNERS), \
                 patch.object(protocol, 'REGISTRY_ROOT', Path(scratch)), contextlib.redirect_stdout(output):
                from history import authorize
                self.assertEqual(authorize(replay, dict(os.environ)), replay)
                from protocol import EvidenceError
                with self.assertRaises(EvidenceError) as rejected:
                    authorize('f' * 40, dict(os.environ))
                self.assertEqual(str(rejected.exception), 'replay SHA is not eligible main history')
                self.assertEqual(observe.main(), 1)
            self.assertEqual(output.getvalue().splitlines(),
                             [f'FAIL {tier} {replay}: recorded attempt' for tier in ('daily', 'weekly')])


class FreshnessScope(RegistryFixture, unittest.TestCase):
    def test_side_branch_pass_is_not_operational_coverage(self):
        self.assertEqual(observe.observe_main([TIP, SHA], {SHA: [record()]},
                         'daily', NOW, RUNNERS, 'example/repo', freshness=[TIP]),
                         [f'STALE daily {SHA}: no fresh PASS'])
