#!/usr/bin/env python3
"""Full source progress controls through the production Lean census on committed HEADs."""
import contextlib
import io
import os
from pathlib import Path
import subprocess
import tempfile
import landing_manifest as inventory


def main():
    original_root = inventory.ROOT
    original_env = dict(os.environ)
    with tempfile.TemporaryDirectory(prefix='landing-progress-control-') as tmp:
        root = Path(tmp)
        (root/'ConcreteSpec').mkdir()
        (root/'ConcreteSpec.lean').write_text('import ConcreteSpec.Control\n')
        probe = root/'ConcreteSpec/Control.lean'
        original = '/-- Approved law. -/\ntheorem approved : True := by trivial\n'
        probe.write_text(original)
        config = root/'gitconfig'
        config.write_text('')
        raw = subprocess.check_output(['bash','-c',
            'source "$1" || exit 2; git_sandbox "$2" || exit 2; env -0',
            '_',str(original_root/'scripts/checks/lib/git_sandbox.sh'),str(config)])
        isolated = dict(os.fsdecode(e).split('=',1) for e in raw.split(b'\0') if e)
        templates = root/'empty-template'
        templates.mkdir()
        isolated['GIT_TEMPLATE_DIR'] = str(templates)
        def git(*args):
            return subprocess.check_output(['git',*args],cwd=root,env=isolated,text=True,stderr=subprocess.STDOUT).strip()
        def commit():
            git('add','ConcreteSpec','ConcreteSpec.lean')
            git('-c','user.name=Control','-c','user.email=c@example.invalid',
                '-c','commit.gpgsign=false','commit','--quiet','-m','control')
        git('init','--quiet')
        commit()
        caller = root/'caller'
        caller.mkdir()
        git('-C',str(caller),'init','--quiet')
        caller_refs = git('-C',str(caller),'for-each-ref')
        os.environ.update(GIT_DIR=str(caller/'.git'),GIT_WORK_TREE=str(caller),GIT_INDEX_FILE=str(caller/'.git/index'))
        # head_population uses the original exporter path; supply it in the isolated fixture.
        exporter = root/'scripts/spec/landing_inventory.lean'
        exporter.parent.mkdir(parents=True)
        exporter.write_bytes((original_root/'scripts/spec/landing_inventory.lean').read_bytes())
        inventory.ROOT = root
        try:
            snapshot = inventory.head_population()
            def assert_reason(expected):
                observed = inventory.progress_reasons(snapshot,inventory.head_population())
                if observed != {'approved':expected}:
                    raise ValueError(f'progress reason: expected {expected!r}, got {observed!r}')
            assert_reason('identical full digest')
            print('control PASS: identical theorem counts by identical full digest')
            for text,label in [(original.replace('Approved law.', 'Older documentation.'),'doc comment'),
                               (original.replace('by trivial','True.intro'),'proof source'),
                               (original+'attribute [simp] approved\n','separate attribute')]:
                probe.write_text(text)
                commit()
                assert_reason('full digest mismatch')
                print('control PASS: '+label+' change refused by full digest mismatch')
            probe.write_text('-- deleted\n')
            commit()
            assert_reason('missing declaration')
            print('control PASS: deleted theorem stops counting because declaration is missing')
            definition = 'def recursive : Nat → Nat\n  | 0 => 0\n  | n+1 => recursive n\n'
            probe.write_text(definition)
            consumer = root/'ConcreteSpec/Consumer.lean'
            consumer.write_text('import ConcreteSpec.Control\ntheorem realized : recursive 0 = 0 := by rw [recursive.eq_1]\nexample : True := by trivial\n')
            commit()
            realized = inventory.census(root, exporter, examples=False)
            generated = {d['name'] for d in realized if d['record']=='declaration'
                         and d['name'].startswith('recursive.')}
            if 'recursive.eq_1' not in generated:
                raise ValueError('control failed to realize recursive equation lemma')
            with_consumer = inventory.head_population()
            if 'recursive' not in with_consumer or any(n.startswith('recursive.') for n in with_consumer):
                raise ValueError('generated equation became a progress unit')
            example = next(n for n in with_consumer if n.startswith('command:ConcreteSpec/Consumer.lean:'))
            population = [inventory.row(record='piece',name='F03'),
                          inventory.row(record='declaration',name='recursive',piece='F03'),
                          inventory.row(record='declaration',name='recursive.eq_1',piece='F03'),
                          inventory.row(record='command',name=example.removeprefix('command:'),kind='example',piece='F03')]
            def assert_report(checked, expected):
                output = io.StringIO()
                with contextlib.redirect_stdout(output):
                    inventory.report(population,reasons=inventory.progress_reasons(with_consumer,checked))
                if expected not in output.getvalue():
                    raise ValueError('anonymous example excluded from landed total: '+output.getvalue())
            assert_report(with_consumer,'F03\tlanded=2/2\tremaining=0\tremaining-examples=0')
            consumer.unlink()
            commit()
            without_consumer = inventory.head_population()
            if without_consumer != {'recursive':with_consumer['recursive']}:
                raise ValueError('generated lemma realization affected definition progress')
            assert_report(without_consumer,'F03\tlanded=1/2\tremaining=1\tremaining-examples=1')
            reasons = inventory.progress_reasons(with_consumer,without_consumer)
            if reasons != {'recursive':'identical full digest','realized':'missing declaration',example:'missing declaration'}:
                raise ValueError('definition/example progress mismatch: '+str(reasons))
            probe.write_text(definition.replace('| 0 => 0','| 0 => 1'))
            commit()
            if inventory.progress_reasons(without_consumer,inventory.head_population()) != {'recursive':'full digest mismatch'}:
                raise ValueError('changed definition counted as landed')
            print('control PASS: generated equations never count; defining source and anonymous examples govern progress')
        finally:
            inventory.ROOT = original_root
            inventory.source.cache_clear()
            os.environ.clear()
            os.environ.update(original_env)
        if git('-C',str(caller),'for-each-ref')!=caller_refs:
            raise ValueError('progress fixture changed inherited caller refs')
        print('control PASS: HEAD progress Git isolation preserves caller refs')


if __name__=='__main__':
    main()
