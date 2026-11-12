#!/usr/bin/env python3
"""Lightweight and annotated pin protocols, without network mutations."""
import os
from pathlib import Path
import subprocess
import tempfile
import landing_manifest as inventory


def main():
    original = inventory.git
    try:
        for annotated in (False,True):
            calls = []
            fetched = set()
            def fixture(*args):
                calls.append(args)
                if args[0]=='config':
                    if args[1:]!=('--get','landing.inventoryRemote'): raise ValueError('unexpected remote config lookup')
                    return b'fixture-remote\n'
                if args[0]=='rev-parse':
                    tag = args[-1].removesuffix('^{commit}')
                    if tag not in fetched: raise inventory.subprocess.CalledProcessError(1,args)
                    if annotated and not args[-1].endswith('^{commit}'):
                        return ('f'*40).encode()
                    return inventory.PIN_TAGS[tag.rsplit('/',1)[-1]].encode()
                if args[0]=='ls-remote':
                    if args[1]!='fixture-remote': raise ValueError('wrong configured remote')
                    tag = args[-2]
                    ref = inventory.PIN_TAGS[tag.rsplit('/',1)[-1]]
                    return ((('f'*40 if annotated else ref)+'\t'+tag+'\n')+
                            (ref+'\t'+tag+'^{}\n' if annotated else '')).encode()
                if args[0]=='fetch': fetched.add(args[-1].split(':')[1])
                return b''
            inventory.git = fixture
            inventory.ensure_objects()
            if fetched != {'refs/tags/spec-inventory/'+name for name in inventory.PIN_TAGS}:
                raise ValueError('gate fetched unexpected refs')
            if any('--depth=1' not in c for c in calls if c[0]=='fetch'):
                raise ValueError('unbounded pin fetch')
            for name,expected in sorted(inventory.PIN_TAGS.items()):
                tag = 'refs/tags/spec-inventory/'+name
                for advertised in (b'',('0'*40+'\t'+tag+'\n').encode()):
                    fetched.clear()
                    def missing(*args):
                        if args[0]=='ls-remote' and args[-2]==tag: return advertised
                        return fixture(*args)
                    inventory.git = missing
                    try: inventory.ensure_objects(remote='fixture-remote')
                    except ValueError as exc:
                        if str(exc)!='missing or changed protected inventory pin: '+tag+' expected '+expected: raise
                    else: raise ValueError('missing/changed pin accepted')
        def all_missing(*args):
            if args[0]=='rev-parse': raise inventory.subprocess.CalledProcessError(1,args)
            return b''
        inventory.git = all_missing
        try: inventory.ensure_objects(remote='fixture-remote')
        except ValueError as exc:
            expected = '\n'.join('missing or changed protected inventory pin: refs/tags/spec-inventory/'+n+' expected '+r
                                 for n,r in sorted(inventory.PIN_TAGS.items()))
            if str(exc)!=expected: raise
        else: raise ValueError('all missing pins accepted')
        def unavailable(*args):
            if args[0] in ('rev-parse','ls-remote'):
                raise inventory.subprocess.CalledProcessError(1,args)
            return b''
        inventory.git = unavailable
        expected = '\n'.join('missing or changed protected inventory pin: refs/tags/spec-inventory/'+n+' expected '+r
                             for n,r in sorted(inventory.PIN_TAGS.items()))
        try: inventory.ensure_objects(remote='fixture-remote')
        except ValueError as exc:
            if str(exc)!=expected: raise
        else: raise ValueError('unavailable remote accepted')
        with tempfile.TemporaryDirectory(prefix='landing-local-pins-') as tmp:
            root = Path(tmp)
            config = root/'gitconfig'
            config.write_text('')
            raw = subprocess.check_output(['bash','-c',
                'source "$1" || exit 2; git_sandbox "$2" || exit 2; env -0',
                '_',str(inventory.ROOT/'scripts/checks/lib/git_sandbox.sh'),str(config)])
            isolated = dict(os.fsdecode(e).split('=',1) for e in raw.split(b'\0') if e)
            templates = root/'empty-template'
            templates.mkdir()
            isolated['GIT_TEMPLATE_DIR'] = str(templates)
            def real_git(*args):
                return subprocess.check_output(['git', '-c','user.name=Control',
                    '-c','user.email=c@example.invalid','-c','commit.gpgsign=false',
                    '-c','tag.gpgsign=false',*args],cwd=root,env=isolated,stderr=subprocess.STDOUT)
            real_git('init','--quiet')
            real_git('commit','--allow-empty','--quiet','-m','first')
            expected = real_git('rev-parse','HEAD').decode().strip()
            real_git('commit','--allow-empty','--quiet','-m','second')
            saved_pins = inventory.PIN_TAGS
            inventory.PIN_TAGS = {'source':expected}
            inventory.git = real_git
            try:
                tag = 'refs/tags/spec-inventory/source'
                for annotated in (False, True):
                    def set_tag(target):
                        real_git('tag','--force',*(['-a','-m','annotated control'] if annotated else []),
                                 'spec-inventory/source',target)
                    set_tag(expected)
                    inventory.ensure_objects(remote='fixture-remote')
                    set_tag('HEAD')
                    try: inventory.ensure_objects(remote='fixture-remote')
                    except ValueError as exc:
                        if str(exc)!='changed protected inventory pin: '+tag+' expected '+expected: raise
                    else: raise ValueError('retargeted local pin accepted')
            finally:
                inventory.PIN_TAGS = saved_pins
        print('control PASS: shallow tag-only fetch; annotated pins peeled; missing/changed remote and real retargeted local pins refused')
    finally:
        inventory.git = original


if __name__=='__main__':
    main()
