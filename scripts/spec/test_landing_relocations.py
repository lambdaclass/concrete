#!/usr/bin/env python3
"""Read-mode provenance and completeness controls, using private artifacts only."""
import json
from pathlib import Path
import tempfile
import landing_manifest as inventory


def main():
    original = inventory.ROOT
    original_metadata = inventory.LAW_METADATA
    with tempfile.TemporaryDirectory(prefix='landing-relocations-') as tmp:
        root = Path(tmp)
        landing = root/'ConcreteSpec/landing'
        landing.mkdir(parents=True)
        validation = landing/'VALIDATION.md'
        validation.write_text('Control validation\n')
        (landing/'shared-fixtures.json').write_text('[]\n')
        file = 'ConcreteSpec/Proofs/ObserveEnvironment.lean'
        command = inventory.row(record='command',name=file+':1:1',source=file,line=1,
                                piece='F23',note='law fixture.law with control fixture.control')
        declaration = inventory.row(record='declaration',name='fixture.law',source=file,line=1,
                                    piece='F23',note=command['name']+'; '+command['note'])
        control = inventory.row(record='declaration',name='fixture.control',source=file,line=1,
                                piece='F23',note=command['name']+'; '+command['note'])
        rows = [command,declaration,control]
        inventory.LAW_METADATA = [dict(laws=['fixture.law'], mutants=['fixture.control'],file=file,line=1)]
        inventory.ROOT = root
        try:
            expected = inventory.relocation_evidence(rows,write=True)
            if {r['record'] for r in expected.values()}!={'command','declaration'}:
                raise ValueError('relocation evidence omitted commands or declarations')
            inventory.relocation_evidence(rows)
            path = landing/'relocations.json'
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
            refused([dict(command,note=''),declaration,control],'relocation lacks cause: '+command['name'])
            refused([dict(command,note='shared fixture f forces consumer'),declaration,control],
                    'unverified shared fixture: '+command['name'])
            refused([dict(command,note='law invented with control fixture.control'),declaration,control],
                    'unresolved relocation law/control: invented with control fixture.control')
            refused([dict(command,note='law fixture.law with control invented'),declaration,control],
                    'unresolved relocation law/control: fixture.law with control invented')
            # Each structured cause kind must be justified by the row graph/census.
            def causes(population):
                return inventory.relocation_causes(population)
            unrelated = dict(command,name=file+':50:50',line='50',note=command['note'])
            try: causes(rows+[unrelated])
            except ValueError as exc:
                if not str(exc).startswith('unrelated relocation law/control:'): raise
            else: raise ValueError('unrelated row borrowed a real law/control pair')
            normalized = [dict(r) for r in rows]+[dict(unrelated)]
            inventory.relocation_causes(normalized,normalize=True)
            if normalized[-1]['note'].strip(): raise ValueError('unrelated inherited annotation survived generation')
            inventory.relocation_causes(normalized)
            (landing/'shared-fixtures.json').write_text(json.dumps([command['name']]))
            shared = dict(command,note='shared fixture/support moved to earliest consumer')
            if causes([shared,declaration,control])[('command',shared['name'])][0]['kind']!='shared-fixture':
                raise ValueError('real shared fixture refused')
            (landing/'shared-fixtures.json').write_text('[]')
            try: causes([shared,declaration,control])
            except ValueError as exc:
                if not str(exc).startswith('unverified shared fixture:'): raise
            else: raise ValueError('fabricated shared fixture accepted')
            auxiliary = inventory.row(record='declaration',name='fixture.auxiliary',source=file,line=0,
                                      piece='F23',regeneration=command['name'],note=command['name']+'; regenerated at observed kernel-identical emission')
            aux_rows = rows+[auxiliary]
            if causes(aux_rows)[('declaration',auxiliary['name'])][0]['kind']!='regenerated':
                raise ValueError('observed regeneration refused')
            try: causes(rows+[dict(auxiliary,regeneration='invented')])
            except ValueError as exc:
                if not str(exc).startswith('unverified regenerated relocation:'): raise
            else: raise ValueError('unobserved regeneration accepted')
            dependent = dict(unrelated,note='dependent on observed auxiliary fixture.auxiliary',
                             dependencies=json.dumps(['fixture.auxiliary']))
            if causes(aux_rows+[dependent])[('command',dependent['name'])][0]['kind']!='auxiliary-dependent':
                raise ValueError('observed auxiliary dependency refused')
            try: causes(aux_rows+[dict(dependent,dependencies='[]')])
            except ValueError as exc:
                if not str(exc).startswith('unverified auxiliary relocation:'): raise
            else: raise ValueError('fabricated auxiliary dependency accepted')
            law_dependent = dict(unrelated,dependencies=json.dumps(['fixture.law']))
            if causes(rows+[law_dependent])[('command',unrelated['name'])][0]['kind']!='law-dependent':
                raise ValueError('law-dependent cause refused')
            support_command = dict(unrelated,note=command['note'])
            support = inventory.row(record='declaration',name='fixture.support',source=file,line=50,
                                    piece='F23',note=support_command['name']+'; '+command['note'])
            supported_rows = [command,dict(declaration,dependencies=json.dumps(['fixture.support'])),control,
                              support_command,support]
            if causes(supported_rows)[('command',support_command['name'])][0]['kind']!='law-support':
                raise ValueError('actual law support refused')
            fixture_consumer = dict(dependent,note='shared fixture '+command['name']+' forces consumer')
            (landing/'shared-fixtures.json').write_text(json.dumps([command['name']]))
            if causes(aux_rows+[fixture_consumer])[('command',fixture_consumer['name'])][0]['kind']!='shared-fixture-consumer':
                raise ValueError('real fixture consumer refused')
            try: causes(aux_rows+[dict(fixture_consumer,dependencies='[]')])
            except ValueError as exc:
                if not str(exc).startswith('unverified shared fixture:'): raise
            else: raise ValueError('unrelated fixture consumer accepted')
            (landing/'shared-fixtures.json').write_text('[]')
            validation.write_text('Missing relocation table\n')
            refused(rows,'VALIDATION relocation table is stale')
            print('control PASS: relocation kinds require actual pair membership, dependencies, shared fixtures and observed regeneration; stale evidence refused')
        finally:
            inventory.ROOT = original
            inventory.LAW_METADATA = original_metadata


if __name__=='__main__':
    main()
