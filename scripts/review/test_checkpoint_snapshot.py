"""Slice B planted controls, using real independent Git writers in private repos."""
import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import tempfile
from unittest.mock import patch

spec = importlib.util.spec_from_file_location('writer_provenance', Path(__file__).with_name('writer_provenance.py'))
w = importlib.util.module_from_spec(spec)
spec.loader.exec_module(w)


def git(repo, *args, input=None):
    return subprocess.run(['git', '-c', 'core.hooksPath=/dev/null', '-C', repo, *args], input=input, text=True,
                          capture_output=True, env=w.canonical_git_env())


# Optional differential proof uses the former per-ref queries as an independent
# oracle in the same prepared lock. Normal gate timing excludes oracle overhead.
if '--compare-inspection' in sys.argv:
    checkpoint_git = w._checkpoint_git
    comparisons = [0]
    def compare_inspection(repo, args, **kwargs):
        result = checkpoint_git(repo, args, **kwargs)
        if args[0] == 'for-each-ref' and '%(symref)' in args[1]:
            names = args[args.index('--') + 1:]
            batch = {f[0]: f for row in result.stdout.splitlines()
                     if (f := row.split()) and f[0] in names}
            for ref in names:
                old = checkpoint_git(repo, ['for-each-ref',
                    '--format=%(refname) %(objectname) %(objecttype)', '--', ref])
                exact = [row.split() for row in old.stdout.splitlines() if row.split()[0] == ref]
                fields = batch.get(ref)
                assert (fields[:3] if fields else None) == (exact[0] if exact else None)
                symbolic = checkpoint_git(repo, ['symbolic-ref', '--no-recurse', '-q', ref], check=False)
                if fields:
                    assert (len(fields) > 3) == (symbolic.returncode == 0)
                else:
                    # Missing rows still require the production fallback; both
                    # true absence and omitted symbolic refs occur in controls.
                    assert symbolic.returncode in (0, 1)
                comparisons[0] += 1
        return result
    w._checkpoint_git = compare_inspection


def require_error(call, expected):
    try:
        call()
    except w.ProvenanceError as exc:
        assert str(exc) == expected, (str(exc), expected)
    else:
        raise AssertionError('protection removed: ' + expected)


def at_prepare(callback, *, after=False):
    original = w._CheckpointProtocol.exchange
    def exchange(self, command, expected):
        preparing = expected == 'prepare: ok'
        if preparing and not after:
            callback()
        result = original(self, command, expected)
        if preparing and after:
            callback()
        return result
    return patch.object(w._CheckpointProtocol, 'exchange', exchange)


with tempfile.TemporaryDirectory() as root:
    for backend in ('files', 'reftable'):
        repo = os.path.join(root, backend)
        init = git(root, 'init', '-q', '--ref-format=' + backend, repo)
        assert init.returncode == 0, f'unsupported checkpoint ref backend: {backend}: {init.stderr}'
        def g(*args, input=None):
            result = git(repo, *args, input=input)
            assert result.returncode == 0, result.stderr
            return result.stdout.strip()
        g('config', 'core.hooksPath', '/dev/null')
        # Disposable competing writers should fail immediately on held locks.
        g('config', 'core.filesRefLockTimeout', '0')
        g('config', 'reftable.lockTimeout', '0')
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
                expected = 'checkpoint ref race during control'
                for updates in ({active: b}, {active: b, alias: b}, {}):
                    require_error(lambda: w.cas_update_refs(snap, updates, op='control'), expected)
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
            inspection_calls = []
            runner = w.canonical_git_run
            def count_inspection(repo_arg, args, **kwargs):
                if args[0] in ('for-each-ref', 'symbolic-ref', 'cat-file', 'check-ref-format'):
                    inspection_calls.append(args)
                return runner(repo_arg, args, **kwargs)
            with at_prepare(competitors, after=True), patch.object(w, 'canonical_git_run', count_inspection):
                w.cas_update_refs(snap, {active: b})
            if '--compare-inspection' not in sys.argv:
                assert sum(args[0] == 'for-each-ref' for args in inspection_calls) == 2
                assert sum(args[0] == 'cat-file' for args in inspection_calls) == 2
                assert [args[-1] for args in inspection_calls if args[0] == 'symbolic-ref'] == [absent, absent]
                assert not any(args[0] == 'check-ref-format' for args in inspection_calls)

            assert len(seen) == 16
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
                if args[0] == 'for-each-ref' and '%(objectname)' in ' '.join(args) and '--' not in args:
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
            # Deletion and lock contention are the same bounded race class.
            reset()
            snap = capture()
            g('update-ref', '-d', companion)
            require_error(lambda: w.cas_update_refs(snap, {active: b}),
                          'checkpoint ref race during checkpoint installation')
            reset()
            races = []
            def toggle():
                races.append(1)
                if git(repo, 'show-ref', '--verify', companion).returncode == 0:
                    g('update-ref', '-d', companion)
                else:
                    g('update-ref', companion, a)
            with at_prepare(toggle):
                require_error(capture, 'checkpoint ref race during checkpoint capture')
            assert len(races) == 3
            # The validation/install gap cannot authorize changed values or a
            # symbolic replacement, even for an unchanged alias.
            for symbolic_gap in (False, True):
                reset()
                snap = capture()
                exchange_original = w._CheckpointProtocol.exchange
                def in_gap(self, command, expected):
                    result = exchange_original(self, command, expected)
                    if expected == 'abort: ok':
                        if symbolic_gap:
                            g('symbolic-ref', alias, companion)
                        else:
                            g('update-ref', alias, b)
                    return result
                with patch.object(w._CheckpointProtocol, 'exchange', in_gap):
                    require_error(lambda: w.cas_update_refs(snap, {active: b}),
                                  'symbolic checkpoint ref is forbidden: ' + alias if symbolic_gap else
                                  'checkpoint ref race during checkpoint installation')
                assert g('rev-parse', active) == a
            # Exact full-ref lookup must ignore DWIM lookalikes.
            reset()
            g('update-ref', 'refs/heads/' + absent, b)
            assert capture().refs[absent] is None
            g('update-ref', '-d', 'refs/heads/' + absent)
            # Prefix matches are not exact refs, in loose, packed and reftable
            # stores. A live x/y must never stand in for the absent participant x.
            reset()
            child = absent + '/child'
            g('update-ref', child, b)
            if packed:
                g('pack-refs', '--all')
            # Git rejects parent/descendant namespace conflicts while preparing
            # on both backends. Preserve that fail-closed boundary and diagnostic.
            require_error(capture, 'checkpoint ref race during checkpoint capture'
                          if backend == 'files' and not packed else
                          'checkpoint transaction protocol failed')
            g('update-ref', '-d', child)
            # Every symbolic target has the same diagnostic, including absence.
            blob = g('hash-object', '-w', '--stdin', input='blob')
            target = 'refs/notes/symbolic-target'
            for value in (None, a, b, blob):
                for participant in (alias, absent):
                    reset()
                    git(repo, 'update-ref', '-d', target)
                    if value is not None:
                        g('update-ref', target, value)
                    snap = capture()
                    with at_prepare(lambda: g('symbolic-ref', participant, target)):
                        require_error(lambda: w.cas_update_refs(snap, {active: b}),
                                      'symbolic checkpoint ref is forbidden: ' + participant)
                    require_error(capture, 'symbolic checkpoint ref is forbidden: ' + participant)
            reset()
            g('symbolic-ref', target, alias)
            g('symbolic-ref', alias, target)
            require_error(capture, 'symbolic checkpoint ref is forbidden: ' + alias)
            git(repo, 'update-ref', '--no-deref', '-d', target)
            reset()
            head_checkpoint = 'refs/heads/symbolic-checkpoint'
            g('update-ref', target, blob)
            g('symbolic-ref', head_checkpoint, target)
            require_error(lambda: w.capture_checkpoint_snapshot(repo, target_ref=head_checkpoint),
                          'symbolic checkpoint ref is forbidden: ' + head_checkpoint)
            g('update-ref', '--no-deref', '-d', head_checkpoint)
            reset()
            g('update-ref', alias, blob)
            require_error(capture, 'checkpoint ref does not name a 40-hex commit: ' + alias)
            reset()
            snap = capture()
            for invalid in ('', 'bad name', 'refs/notes/bad..name'):
                require_error(lambda: w.cas_update_refs(w.CheckpointSnapshot(snap.repo, {invalid: None}), {}),
                              'invalid checkpoint ref name: ' + invalid)
                require_error(lambda: w.capture_checkpoint_snapshot(repo, target_ref=invalid),
                              'invalid checkpoint ref name: ' + invalid)
            for invalid in ('', 'z' * 40, w.ZERO40, 7):
                require_error(lambda: w.cas_update_refs(snap, {alias: invalid}),
                              f'invalid checkpoint update SHA for {alias}: {invalid}')
            require_error(lambda: w.cas_update_refs(snap, {alias: '1' * 40}),
                          f'checkpoint update does not name an existing commit: {alias}: ' + '1' * 40)
            # A ref named like a missing OID must not stand in for the object.
            g('update-ref', 'refs/heads/' + '1' * 40, a)
            require_error(lambda: w.cas_update_refs(snap, {alias: '1' * 40}),
                          f'checkpoint update does not name an existing commit: {alias}: ' + '1' * 40)
            g('update-ref', '-d', 'refs/heads/' + '1' * 40)
            tag = g('mktag', input=f'object {a}\ntype commit\ntag control\ntagger Control <control@example.invalid> 1 +0000\n\ncontrol\n')
            for invalid in (blob, tree, tag):
                require_error(lambda: w.cas_update_refs(snap, {alias: invalid}),
                              f'checkpoint update does not name an existing commit: {alias}: {invalid}')
            # Hold an actual competing prepared transaction throughout capture.
            holder = w.canonical_git_run(snap.repo, ['update-ref', '--no-deref', '--stdin'], _interactive=True)
            holder_protocol = w._CheckpointProtocol(holder)
            assert holder_protocol.deadline_seconds == 10
            try:
                holder_protocol.exchange('start', 'start: ok')
                holder_protocol.exchange(f'update {active} {a}\nprepare', 'prepare: ok')
                require_error(capture, 'checkpoint ref race during checkpoint capture')
            finally:
                holder_protocol.close()
            original_run = w.canonical_git_run
            def unsupported(repo_arg, args, **kwargs):
                if args == ['rev-parse', '--show-ref-format']:
                    return subprocess.CompletedProcess(args, 0, 'unsupported\n', '')
                return original_run(repo_arg, args, **kwargs)
            def timed_out_inspection(repo_arg, args, **kwargs):
                if args[0] == 'cat-file':
                    assert kwargs.get('_timeout') == 10
                    raise subprocess.TimeoutExpired(args, 10)
                return original_run(repo_arg, args, **kwargs)
            with patch.object(w, 'canonical_git_run', timed_out_inspection):
                require_error(lambda: w.cas_update_refs(snap, {alias: b}),
                              'checkpoint transaction timeout (INCOMPLETE)')
            with patch.object(w, 'canonical_git_run', unsupported):
                require_error(lambda: w.cas_update_refs(snap, {}),
                              'unsupported checkpoint ref backend: unsupported')
            # Protocol failure after prepare must release every lock and reap
            # the exact child, even when output exceeds pipe capacity.
            children = []
            def noisy(repo_arg, args, **kwargs):
                if kwargs.get('_interactive'):
                    p = subprocess.Popen(['python3', '-c',
                        "import sys; sys.stderr.write('x'*200000); sys.stderr.flush(); sys.exit(1)"],
                        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, start_new_session=True)
                    children.append(p)
                    return p
                return original_run(repo_arg, args, **kwargs)
            with patch.object(w, 'canonical_git_run', noisy):
                require_error(lambda: w.cas_update_refs(snap, {}),
                              'checkpoint transaction protocol failed')
            assert all(p.poll() is not None for p in children)
            with patch.object(w.os, 'write', side_effect=BrokenPipeError('planted pipe break')):
                require_error(lambda: w.cas_update_refs(snap, {}),
                              'checkpoint transaction protocol failed')
            if backend == 'files' and not packed:
                def stalled(repo_arg, args, **kwargs):
                    if kwargs.get('_interactive'):
                        p = subprocess.Popen(['python3', '-c', 'import time; time.sleep(60)'],
                            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, start_new_session=True)
                        children.append(p)
                        return p
                    return original_run(repo_arg, args, **kwargs)
                assert w.CHECKPOINT_PROTOCOL_DEADLINE == 10
                with patch.object(w, 'CHECKPOINT_PROTOCOL_DEADLINE', 0.2), patch.object(w, 'canonical_git_run', stalled):
                    require_error(lambda: w.cas_update_refs(snap, {}),
                                  'checkpoint transaction timeout (INCOMPLETE)')
                assert all(p.poll() is not None for p in children)
            def excessive_stdout(repo_arg, args, **kwargs):
                if kwargs.get('_interactive'):
                    p = subprocess.Popen(['python3', '-c',
                        "import sys,time; sys.stdout.write('x'*200000); sys.stdout.flush(); time.sleep(60)"],
                        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                        text=True, start_new_session=True)
                    children.append(p)
                    return p
                return original_run(repo_arg, args, **kwargs)
            with patch.object(w, 'canonical_git_run', excessive_stdout):
                require_error(lambda: w.cas_update_refs(snap, {}),
                              'checkpoint transaction protocol failed')
            original_exchange = w._CheckpointProtocol.exchange
            def fail_prepared(self, command, expected):
                result = original_exchange(self, command, expected)
                if expected == 'prepare: ok':
                    children.append(self.proc)
                    raise w.ProvenanceError('planted failure after prepare')
                return result
            with patch.object(w._CheckpointProtocol, 'exchange', fail_prepared):
                require_error(lambda: w.cas_update_refs(snap, {}), 'planted failure after prepare')
            assert all(p.poll() is not None for p in children)
            w.cas_update_refs(snap, {})
            print(f'checkpoint controls: {backend}, packed={packed}: PASS')

if '--compare-inspection' in sys.argv:
    print(f'under-lock per-ref/batch comparisons: {comparisons[0]}: PASS')
