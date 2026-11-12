#!/usr/bin/env python3
"""Full source progress controls through the production Lean census on committed HEADs."""
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
