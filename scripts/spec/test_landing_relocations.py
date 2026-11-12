#!/usr/bin/env python3
"""Read-mode provenance and completeness controls, using private artifacts only."""
import json
from pathlib import Path
import tempfile
import landing_manifest as inventory


def main():
    original = inventory.ROOT
    with tempfile.TemporaryDirectory(prefix='landing-relocations-') as tmp:
        root = Path(tmp)
        landing = root/'ConcreteSpec/landing'
        landing.mkdir(parents=True)
        validation = landing/'VALIDATION.md'
        validation.write_text('Control validation\n')
        file = 'ConcreteSpec/Proofs/ObserveEnvironment.lean'
        command = inventory.row(record='command',name=file+':1:1',source=file,line=1,
                                piece='F23',note='law fixture.law with control fixture.control')
        declaration = inventory.row(record='declaration',name='fixture.law',source=file,line=1,
                                    piece='F23',note=command['name']+'; '+command['note'])
        rows = [command,declaration]
        inventory.ROOT = root
        try:
            expected = inventory.relocation_evidence(rows,write=True)
            if {r['record'] for r in expected.values()}!={'command','declaration'}:
                raise ValueError('relocation evidence omitted commands or declarations')
            inventory.relocation_evidence(rows)
            path = landing/'moved-laws.json'
            pristine = path.read_text()
            def refused(population,message):
                try: inventory.relocation_evidence(population)
                except ValueError as exc:
                    if str(exc)!=message: raise
                else: raise ValueError('relocation control accepted: '+message)
            damaged = dict(expected)
            del damaged['command:'+command['name']]
            path.write_text(json.dumps(damaged))
            refused(rows,'relocation evidence is stale')
            path.write_text(pristine)
            refused([dict(command,note=''),declaration],'relocation lacks cause: '+command['name'])
            refused([dict(command,note='shared fixture f forces consumer'),declaration],
                    'law relocation lacks named law/control: '+command['name'])
            validation.write_text('Missing relocation table\n')
            refused(rows,'VALIDATION relocation table is stale')
            print('control PASS: read mode refuses omitted commands, missing/nonnamed law causes and incomplete VALIDATION')
        finally:
            inventory.ROOT = original


if __name__=='__main__':
    main()
