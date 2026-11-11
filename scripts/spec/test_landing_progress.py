#!/usr/bin/env python3
"""Exercise progress against a real repaired/relocated declaration in an isolated git snapshot."""
import contextlib
import io
import os
from pathlib import Path
import subprocess
import tempfile
import landing_manifest as inventory


def main():
    original_root = inventory.ROOT
    with tempfile.TemporaryDirectory(prefix='landing-progress-control-') as tmp:
        root = Path(tmp)
        config = root/'gitconfig'
        config.write_text('')
        raw = subprocess.check_output(['bash','-c',
            'source "$1" || exit 2; git_sandbox "$2" || exit 2; env -0',
            '_',str(original_root/'scripts/checks/lib/git_sandbox.sh'),str(config)])
        isolated = dict(os.fsdecode(entry).split('=',1) for entry in raw.split(b'\0') if entry)
        templates = root/'empty-template'
        templates.mkdir()
        isolated['GIT_TEMPLATE_DIR'] = str(templates)
        (root/'ConcreteSpec').mkdir()
        exporter = root/'scripts/spec/landing_inventory.lean'
        exporter.parent.mkdir(parents=True)
        exporter.write_bytes((original_root/'scripts/spec/landing_inventory.lean').read_bytes())
        (root/'ConcreteSpec.lean').write_text('import ConcreteSpec.Repaired\n')
        (root/'ConcreteSpec/Repaired.lean').write_text('namespace Repaired\ndef answer : Nat := 9\nend Repaired\n')
        def git(*args):
            return subprocess.check_output(['git',*args],cwd=root,env=isolated,text=True,stderr=subprocess.STDOUT).strip()
        caller = root/'caller'
        caller.mkdir()
        git('-C',str(caller),'init','--quiet')
        git('-C',str(caller),'-c','user.name=Inventory caller','-c','user.email=caller@example.invalid',
            '-c','commit.gpgsign=false','commit','--quiet','--allow-empty','-m','caller fixture')
        caller_refs = git('-C',str(caller),'for-each-ref')
        original_env = dict(os.environ)
        os.environ.update(GIT_DIR=str(caller/'.git'),GIT_WORK_TREE=str(caller),GIT_INDEX_FILE=str(caller/'.git/index'))
        git('init','--quiet')
        git('add','ConcreteSpec','ConcreteSpec.lean','scripts/spec/landing_inventory.lean')
        git('-c','user.name=Inventory control','-c','user.email=inventory@example.invalid',
            '-c','commit.gpgsign=false','commit','--quiet','-m','repaired declaration fixture')
        ref = git('rev-parse','HEAD')
        if git('status','--porcelain','--untracked-files=no'):
            raise ValueError('progress fixture is not a committed snapshot')
        # A fresh depth-one CI checkout must fetch an older receipt's exact object.
        (root/'ConcreteSpec/Repaired.lean').write_text('namespace Repaired\ndef answer : Nat := 10\nend Repaired\n')
        git('add','ConcreteSpec/Repaired.lean')
        git('-c','user.name=Inventory control','-c','user.email=inventory@example.invalid',
            '-c','commit.gpgsign=false','commit','--quiet','-m','later fixture commit')
        fresh = root/'fresh'
        git('clone','--quiet','--depth=1',root.as_uri(),str(fresh))
        probe = subprocess.run(['git','cat-file','-e',ref+'^{commit}'],cwd=fresh,env=isolated,capture_output=True)
        if probe.returncode==0:
            raise ValueError('historical receipt object was not excluded by the shallow fixture')
        refs = git('-C',str(fresh),'for-each-ref')
        shallow = (fresh/'.git/shallow').read_bytes()
        inventory.ROOT = fresh
        try:
            # A valid object on an unmerged branch is never landed progress.
            git('-C',str(fresh),'checkout','--quiet','-b','unmerged')
            git('-C',str(fresh),'-c','user.name=Control','-c','user.email=c@example.invalid',
                '-c','commit.gpgsign=false','commit','--quiet','--allow-empty','-m','unmerged')
            unmerged = git('-C',str(fresh),'rev-parse','HEAD')
            git('-C',str(fresh),'checkout','--quiet','-')
            for rejected,prefix in [(unmerged,'landed source is not an ancestor'),
                                    (inventory.SOURCE,'snapshot pin cannot be landed progress')]:
                try:
                    inventory.validate_landed_ref(rejected)
                except ValueError as exc:
                    if not str(exc).startswith(prefix):
                        raise
                else:
                    raise ValueError('invalid landed source accepted: '+rejected)
            print('control PASS: unmerged branch and source snapshot progress refused')
            refs = git('-C',str(fresh),'for-each-ref')
            # Original source digest remains distinct from the repaired command digest.
            old = inventory.row(record='declaration',name='Original.answer',piece='F05',digest='original-source-digest')
            before = dict(old)
            landed = inventory.landed_population(ref)
            if git('-C',str(fresh),'for-each-ref')!=refs or (fresh/'.git/shallow').read_bytes()!=shallow or (fresh/'.git/FETCH_HEAD').exists():
                raise ValueError('receipt fetch moved refs, shallow boundary or FETCH_HEAD')
            receipt = inventory.row(record='landed',name=old['name'],kind='landed-declaration',
                source=ref,piece='F05',note='Repaired.answer',digest=landed['Repaired.answer'],disposition='present')
            inventory.check_landed([old,receipt],{old['name']:old})
            if old != before or receipt['digest']==old['digest']:
                raise ValueError('progress control overwrote original evidence')
            with contextlib.redirect_stdout(io.StringIO()) as out:
                inventory.report([inventory.row(record='piece',name='F05'),old,receipt],'F05')
            if 'F05\tremaining=0' not in out.getvalue() or 'remaining\tF05\tOriginal.answer' in out.getvalue():
                raise ValueError('verified renamed declaration remains in progress count')
            bad = dict(receipt,digest='wrong')
            try:
                inventory.check_landed([old,bad],{old['name']:old})
            except ValueError as exc:
                if not str(exc).startswith('landed declaration mismatch:'):
                    raise
            else:
                raise ValueError('invalid landed digest accepted')
            # Automatic recording also accepts a repaired command under a surviving name.
            surviving = dict(old,name='Repaired.answer')
            recorded = inventory.record_landed([surviving],'F05',ref)
            inventory.check_landed(recorded,{surviving['name']:surviving})
            print('control PASS: repaired/relocated progress preserves source evidence and verifies commit/digest')
        finally:
            inventory.ROOT = original_root
            inventory.landed_population.cache_clear()
            os.environ.clear()
            os.environ.update(original_env)
            if git('-C',str(caller),'for-each-ref')!=caller_refs:
                raise ValueError('progress fixture wrote through inherited Git context')
        print('control PASS: progress Git isolation preserves caller refs')


if __name__=='__main__':
    main()
