#!/usr/bin/env python3
"""Full source progress controls through the production Lean census on committed HEADs."""
import contextlib
import io
import json
import os
from pathlib import Path
import subprocess
import tempfile
import landing_manifest as inventory


def main():
    def sequence(offset):
        return [inventory.row(record='command',name=f'fixture:{offset+i}:{offset+i}',source='fixture',
                              line=offset+i,end_line=offset+i,kind=kind,digest=digest)
                for i,(kind,digest) in enumerate([
                    ('Lean.Parser.Command.namespace','X'),('example','same'),('example','same'),
                    ('Lean.Parser.Command.end','end'),('Lean.Parser.Command.namespace','Y'),
                    ('example','same'),('Lean.Parser.Command.end','end')])]
    before, shifted = sequence(1), sequence(20)
    old, new = inventory.command_match_keys(before), inventory.command_match_keys(shifted)
    if list(old.values())!=list(new.values()) or len(set(old.values()))!=len(old):
        raise ValueError('sequence matching collapsed duplicates/scopes or retained lines')
    print('control PASS: identical commands retain multiplicity and enclosing scopes independent of lines')
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
            consumer_source = consumer.read_text()
            for shifted in [consumer_source.replace('import ConcreteSpec.Control', 'import Init\nimport ConcreteSpec.Control'),
                            consumer_source.replace('theorem realized : recursive 0 = 0 := by rw [recursive.eq_1]\n','')]:
                consumer.write_text(shifted)
                commit()
                if inventory.progress_reasons(with_consumer,inventory.head_population())[example]!='identical full digest':
                    raise ValueError('line-shifted unchanged example did not land')
            consumer.write_text(consumer_source.replace('by trivial','True.intro'))
            commit()
            if inventory.progress_reasons(with_consumer,inventory.head_population())[example]=='identical full digest':
                raise ValueError('changed anonymous example counted as landed')
            consumer.write_text(consumer_source)
            commit()
            print('control PASS: header and declaration line shifts preserve examples; changed example does not land')
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
            # Use actual production full digests for a repaired, renamed declaration.
            old_rows = [inventory.row(record='declaration',name='recursive',piece='F03',disposition='repair')]
            new_name = 'renamed'
            probe.write_text('def renamed : Nat := 1\n')
            commit()
            repaired = inventory.head_population()
            spec = dict(supersedes=['recursive'], declarations={new_name:repaired[new_name]},
                        reason='control repair',review=dict(reference='control accepting review',verdict='ACCEPT'))
            replacement = inventory.row(record='replacement',name='control-repair',piece='F03',
                                        disposition='replace',note=json.dumps(spec))
            population = old_rows+[replacement]
            before = inventory.progress_reasons(without_consumer,repaired)
            if before['recursive']=='identical full digest': raise ValueError('silent replacement landed')
            if inventory.replacement_progress(population,before,repaired)['recursive']!='identical full digest':
                raise ValueError('reviewed replacement did not land')
            for checked in ({}, {new_name:'0'*64}):
                if inventory.replacement_progress(population,before,checked)['recursive']=='identical full digest':
                    raise ValueError('missing or changed replacement landed')
            for changed in [dict(spec,supersedes=['unknown']), dict(spec,review=''), dict(spec,review=dict(reference='rejected',verdict='REJECT')),
                            dict(spec,declarations={new_name:'bad'}), dict(spec,extra='unknown'),
                            dict(spec,supersedes=[]), dict(spec,supersedes=['recursive','recursive']),
                            dict(spec,declarations={'header:fake':'0'*64}), dict(spec,reason='')]:
                try: inventory.replacement_rows(old_rows+[dict(replacement,note=json.dumps(changed))])
                except ValueError: pass
                else: raise ValueError('invalid replacement schema accepted')
            duplicate_note = replacement['note'][:-1]+', "reason": "duplicate"}'
            try: inventory.replacement_rows(old_rows+[dict(replacement,note=duplicate_note)])
            except ValueError: pass
            else: raise ValueError('duplicate replacement JSON field accepted')
            for altered in [dict(old_rows[0],piece='F02'), dict(old_rows[0],piece='BASE'),
                            dict(old_rows[0],disposition='owner-held')]:
                try: inventory.replacement_rows([altered,replacement])
                except ValueError: pass
                else: raise ValueError('replacement escaped piece or owner hold')
            try: inventory.replacement_rows(population,authored=set())
            except ValueError: pass
            else: raise ValueError('generated constant became replacement progress')
            try: inventory.replacement_rows(population+[dict(replacement,name='second')])
            except ValueError as exc:
                if str(exc)!='already-replaced row: recursive': raise
            else: raise ValueError('duplicate supersession accepted')
            multi = dict(spec,declarations={new_name:repaired[new_name], 'second': '1'*64})
            multi_rows = old_rows+[dict(replacement,note=json.dumps(multi))]
            if inventory.replacement_progress(multi_rows,before,repaired)['recursive']=='identical full digest':
                raise ValueError('partial multi-declaration replacement landed')
            if inventory.replacement_progress(multi_rows,before,dict(repaired,second='1'*64))['recursive']!='identical full digest':
                raise ValueError('complete multi-declaration replacement refused')
            print('control PASS: reviewed replacement closes only at exact replacement digests; invalid, unknown, duplicate and partial rows refused')
            proof = root/'ConcreteSpec/Proofs/Header.lean'
            proof.parent.mkdir()
            proof.write_text('import ConcreteSpec.Control\ntheorem headerControl : True := by trivial\n')
            commit()
            header_snapshot = inventory.head_population()
            header = 'header:ConcreteSpec/Proofs/Header.lean'
            if header not in header_snapshot: raise ValueError('proof import header is not measured')
            targets = root/'ConcreteSpec/landing/headers.json'
            targets.parent.mkdir()
            length = len('import ConcreteSpec.Control'.encode())
            target_spec = dict(revision='f'*40, headers={str(proof.relative_to(root)):
                               dict(bytes=length,digest=header_snapshot[header],text='import ConcreteSpec.Control')})
            targets.write_text(json.dumps(target_spec))
            if inventory.header_targets({str(proof.relative_to(root))})!={header:header_snapshot[header]}:
                raise ValueError('exact parsed header target refused')
            entry = target_spec['headers'][str(proof.relative_to(root))]
            extra = entry['text']+'\n\n'
            entry.update(bytes=len(extra.encode()),digest=inventory.sha(extra.encode()),text=extra)
            targets.write_text(json.dumps(target_spec))
            try: inventory.header_targets({str(proof.relative_to(root))})
            except ValueError as exc:
                if str(exc)!='header target is not the exact parsed import header': raise
            else: raise ValueError('arbitrary hash-consistent header prefix accepted')
            proof.write_text('import Init\nimport ConcreteSpec.Control\ntheorem headerControl : True := by trivial\n')
            commit()
            if inventory.progress_reasons(header_snapshot,inventory.head_population())[header]!='full digest mismatch':
                raise ValueError('changed import header counted as landed')
            output = io.StringIO()
            with contextlib.redirect_stdout(output):
                inventory.report([inventory.row(record='piece',name='F01')],reasons={header:'identical full digest'})
            if 'F01\tlanded=1/1' not in output.getvalue(): raise ValueError('F01 header excluded from totals')
            print('control PASS: exact import headers measure F01; a changed header stops counting')
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
