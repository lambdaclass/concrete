#!/usr/bin/env python3
"""Lightweight and annotated pin protocols, without network mutations."""
import landing_manifest as inventory


def main():
    original = inventory.git
    try:
        for annotated in (False,True):
            calls = []
            fetched = set()
            def fixture(*args):
                calls.append(args)
                if args[0]=='rev-parse':
                    tag = args[-1].removesuffix('^{commit}')
                    if tag not in fetched: raise inventory.subprocess.CalledProcessError(1,args)
                    if annotated and not args[-1].endswith('^{commit}'):
                        return ('f'*40).encode()
                    return inventory.PIN_TAGS[tag.rsplit('/',1)[-1]].encode()
                if args[0]=='ls-remote':
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
            for name,expected in inventory.PIN_TAGS.items():
                tag = 'refs/tags/spec-inventory/'+name
                for advertised in (b'',('0'*40+'\t'+tag+'\n').encode()):
                    fetched.clear()
                    def missing(*args):
                        if args[0]=='ls-remote' and args[-2]==tag: return advertised
                        return fixture(*args)
                    inventory.git = missing
                    try: inventory.ensure_objects()
                    except ValueError as exc:
                        if str(exc)!='missing or changed protected inventory pin: '+tag+' expected '+expected: raise
                    else: raise ValueError('missing/changed pin accepted')
        print('control PASS: shallow tag-only fetch; annotated pins peeled; missing/changed pins refused')
    finally:
        inventory.git = original


if __name__=='__main__':
    main()
