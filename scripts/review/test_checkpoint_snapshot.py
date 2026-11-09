"""Slice B planted controls, using real independent Git writers in private repos."""
import importlib.util
import os
from pathlib import Path
import subprocess
import tempfile
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('writer_provenance', Path(__file__).with_name('writer_provenance.py'))
w = importlib.util.module_from_spec(spec)
spec.loader.exec_module(w)


def git(repo, *args, input=None):
    return subprocess.run(['git', '-C', repo, *args], input=input, text=True,
                          capture_output=True, env=w.canonical_git_env())


def require_error(call, expected):
    try:
        call()
    except w.ProvenanceError as exc:
        assert str(exc) == expected, (str(exc), expected)
    else:
        raise AssertionError('protection removed: ' + expected)


class Stream:
    def __init__(self, stream, callback, reading):
        self.stream, self.callback, self.reading = stream, callback, reading
    def __getattr__(self, key):
        return getattr(self.stream, key)
    def write(self, text):
        if not self.reading and '\nprepare\n' in text:
            self.callback()
        return self.stream.write(text)
    def readline(self):
        line = self.stream.readline()
        if self.reading and line == 'prepare: ok\n':
            self.callback()
        return line


def at_prepare(callback, *, after=False):
    original = w.canonical_git_run
    def run(*args, **kwargs):
        proc = original(*args, **kwargs)
        if kwargs.get('_interactive'):
            name = 'stdout' if after else 'stdin'
            setattr(proc, name, Stream(getattr(proc, name), callback, after))
        return proc
    return patch.object(w, 'canonical_git_run', run)


with tempfile.TemporaryDirectory() as root:
    for backend in ('files', 'reftable'):
        repo = os.path.join(root, backend)
        init = git(root, 'init', '-q', '--ref-format=' + backend, repo)
        assert init.returncode == 0, f'unsupported checkpoint ref backend: {backend}: {init.stderr}'
        def g(*args, input=None):
            result = git(repo, *args, input=input)
            assert result.returncode == 0, result.stderr
            return result.stdout.strip()
        g('config', 'user.name', 'Checkpoint Control')
        g('config', 'user.email', 'checkpoint@example.invalid')
        tree = g('mktree', input='')
        a = g('commit-tree', tree, input='a')
        b = g('commit-tree', tree, '-p', a, input='b')
        active, companion = 'refs/notes/concrete-writer', 'refs/notes/remote/concrete-writer'
        alias, absent = 'refs/notes/alias', 'refs/notes/absent'
        for packed in (False, True):
            def reset():
                for ref in (active, companion, alias, absent):
                    git(repo, 'update-ref', '--no-deref', '-d', ref)
                for ref in (active, companion, alias):
                    g('update-ref', ref, a)
                if packed:
                    g('pack-refs', '--all')
            def capture():
                return w.capture_checkpoint_snapshot(repo, target_ref=alias, check_refs=[absent], old_refs=[alias])
            reset()
            snap = capture()
            assert dict(snap.refs) == {active: a, companion: a, alias: a, absent: None}
            assert snap.repo is not w._VerifiedRepo(repo)
            # Distinct alias names with equal SHAs must remain participants.
            for raced in (companion, alias, absent):
                reset()
                snap = capture()
                g('update-ref', raced, b)
                actual_old = w.ZERO40 if raced == absent else a
                # Git backend messages differ, but the complete diagnostic must
                # preserve Git's exact prepare failure, including the ref name.
                direct = git(repo, 'update-ref', '--no-deref', '--stdin',
                             input=f'start\nverify {raced} {actual_old}\nprepare\n')
                assert direct.returncode != 0 and raced in direct.stderr
                expected = 'concurrent writer-notes ref update detected during control: ' + direct.stderr.strip()
                require_error(lambda: w.cas_update_refs(snap, {active: b, alias: b}, op='control'), expected)
                assert g('rev-parse', active) == a, 'partial update escaped failed transaction'
                assert g('rev-parse', raced) == b, 'concurrent writer overwritten'
            # The empty write set must provide the same complete-set CAS.
            require_error(lambda: w.cas_update_refs(snap, {}, op='control'), expected)
            # A symbolic participant introduced before prepare must abort all
            # writes even though Git accepts its dereferenced old OID.
            reset()
            snap = capture()
            for updates in ({active: b}, {active: b, alias: b}):
                reset()
                snap = capture()
                with at_prepare(lambda: g('symbolic-ref', alias, companion)):
                    require_error(lambda: w.cas_update_refs(snap, updates),
                                  'symbolic checkpoint ref is forbidden: ' + alias)
                assert g('rev-parse', active) == a
                assert g('symbolic-ref', alias) == companion
            # Under-lock inspection must actually happen with every name locked,
            # including unchanged aliases and continued absence.
            reset()
            snap = capture()
            seen = []
            def competitors():
                for ref in snap.refs:
                    for args in [('symbolic-ref', ref, companion),
                                 ('update-ref', '--no-deref', ref, b)]:
                        result = git(repo, *args)
                        assert result.returncode != 0, f'prepared participant was not locked: {backend}: {ref}: {args}'
                        diagnostic = (f"cannot lock ref '{ref}'" if backend == 'files' else 'cannot lock references')
                        assert diagnostic in result.stderr, result.stderr
                        seen.append((ref, args[0]))
            with at_prepare(competitors, after=True):
                w.cas_update_refs(snap, {active: b})
            assert len(seen) == 8
            assert g('rev-parse', active) == b
            assert g('rev-parse', alias) == a
            assert git(repo, 'show-ref', '--verify', absent).returncode != 0
            # Creation and deletion use the same complete transaction contract.
            reset()
            snap = capture()
            w.cas_update_refs(snap, {absent: b, alias: None})
            assert g('rev-parse', absent) == b
            assert git(repo, 'show-ref', '--verify', alias).returncode != 0
            # An active override adds a name; it cannot replace the standard ref.
            override = 'refs/notes/override'
            with patch.object(w, 'WRITER_NOTES_REF', override):
                overridden = capture()
            assert set(overridden.refs) == {override, active, companion, alias, absent}
            assert overridden.refs[override] is None
            # Verification-only acceptance must reject a raced enumeration and
            # recapture the complete set, with a fresh canonical capability.
            reset()
            original = w.canonical_git_run
            captures = []
            def enumerate_race(repo_arg, args, **kwargs):
                proc = original(repo_arg, args, **kwargs)
                if args[0] == 'for-each-ref' and '%(objectname)' in ' '.join(args):
                    captures.append(repo_arg)
                    if len(captures) == 1:
                        g('update-ref', companion, b)
                        g('update-ref', absent, b)
                return proc
            with patch.object(w, 'canonical_git_run', enumerate_race):
                accepted = capture()
            assert len(captures) == 2 and captures[0] is not captures[1]
            assert accepted.refs[companion] == accepted.refs[absent] == b
            # Snapshot acceptance itself enforces the never-symbolic rule.
            reset()
            g('symbolic-ref', alias, companion)
            require_error(capture, 'symbolic checkpoint ref is forbidden: ' + alias)
            print(f'checkpoint controls: {backend}, packed={packed}: PASS')
