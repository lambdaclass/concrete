#!/usr/bin/env python3
"""Pin-ref protocol controls; no network or repository mutations."""
import landing_manifest as inventory


def main():
    original = inventory.git
    calls = []
    def fixture(*args):
        calls.append(args)
        if args[0]=='rev-parse':
            raise inventory.subprocess.CalledProcessError(1,args)
        if args[0]=='ls-remote':
            name = args[-1].rsplit('/',1)[-1]
            return (inventory.PIN_TAGS[name]+'\t'+args[-1]+'\n').encode()
        return b''
    try:
        inventory.git = fixture
        inventory.ensure_objects()
        fetched = [args[-1] for args in calls if args[0]=='fetch']
        if fetched != ['refs/tags/spec-inventory/'+name for name in inventory.PIN_TAGS]:
            raise ValueError('gate fetched something other than declared tag refs')
        for missing_name, expected in inventory.PIN_TAGS.items():
            tag = 'refs/tags/spec-inventory/'+missing_name
            for advertised in (b'', b'0'*40+b'\t'+tag.encode()+b'\n'):
                def missing(*args):
                    if args[0]=='rev-parse': raise inventory.subprocess.CalledProcessError(1,args)
                    if args[0]=='ls-remote' and args[-1]==tag: return advertised
                    return fixture(*args)
                inventory.git = missing
                try:
                    inventory.ensure_objects()
                except ValueError as exc:
                    if str(exc)!='missing or changed protected inventory pin: '+tag+' expected '+expected:
                        raise
                else:
                    raise ValueError('unavailable or changed pin accepted')
        print('control PASS: gate fetches declared tag refs; missing/changed anchors refused')
    finally:
        inventory.git = original


if __name__=='__main__':
    main()
