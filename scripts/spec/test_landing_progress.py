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
    def refused(call, expected):
        try: call()
        except ValueError as exc:
            if str(exc)!=expected: raise ValueError(f'expected {expected!r}, got {str(exc)!r}') from exc
        else: raise ValueError('expected refusal: '+expected)

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
            old_rows = [inventory.row(record='declaration',name='recursive',piece='F03',disposition='repair',digest=without_consumer['recursive'])]
            new_name = 'renamed'
            probe.write_text('def renamed : Nat := 1\n')
            commit()
            repaired = inventory.head_population()
            review_path = root/'ConcreteSpec/landing/reviews/control.txt'
            review_path.parent.mkdir(parents=True)
            review_path.write_text('COMMIT: '+git('rev-parse','HEAD')+'\nUNIT: recursive\nUNIT: command:old-example\nVERDICT: ACCEPT\n')
            commit()
            spec = dict(supersedes=['recursive'], declarations={new_name:repaired[new_name]},commands=[],action='replace',
                        reason='control repair',review=dict(reference={'path':'ConcreteSpec/landing/reviews/control.txt','commit':git('rev-parse','HEAD')},verdict='ACCEPT'))
            replacement = inventory.row(record='replacement',name='control-repair',piece='F03',
                                        disposition='replace',note=json.dumps(spec))
            population = old_rows+[replacement]
            before = inventory.progress_reasons(without_consumer,repaired)
            if before['recursive']=='identical full digest': raise ValueError('silent replacement landed')
            if inventory.replacement_progress(population,before,repaired)['recursive']!='reconciled by replacement':
                raise ValueError('reviewed replacement did not land')
            for checked,expected in (({},'missing replacement declaration: renamed'), ({new_name:'0'*64},'replacement digest mismatch: renamed')):
                if inventory.replacement_progress(population,before,checked)['recursive']!=expected:
                    raise ValueError('missing or changed replacement landed')
            cases = [(dict(spec,supersedes=['unknown']), 'unknown replacement row: unknown'),
                (dict(spec,review=''), 'missing accepting review: control-repair'),
                (dict(spec,review=dict(reference='rejected',verdict='REJECT')), 'review verdict is not ACCEPT: control-repair'),
                (dict(spec,declarations={new_name:'bad'}), 'invalid replacement field declarations: control-repair'),
                (dict(spec,extra='unknown'), 'invalid replacement schema fields: control-repair missing= extra=extra'),
                (dict(spec,supersedes=[]), 'invalid replacement field supersedes: control-repair'),
                (dict(spec,supersedes=['recursive','recursive']), 'invalid replacement field supersedes: control-repair'),
                (dict(spec,declarations={'header:fake':'0'*64}), 'invalid replacement field declarations: control-repair'),
                (dict(spec,reason=''), 'invalid replacement field reason: control-repair')]
            for changed,expected in cases:
                refused(lambda: inventory.replacement_rows(old_rows+[dict(replacement,note=json.dumps(changed))]),expected)
            duplicate_note = replacement['note'][:-1]+', "reason": "duplicate"}'
            refused(lambda: inventory.replacement_rows(old_rows+[dict(replacement,note=duplicate_note)]),
                    'duplicate JSON field: reason')
            for altered,expected in [(dict(old_rows[0],piece='F02'),'replacement supersedes foreign piece unit: recursive'),
                                     (dict(old_rows[0],piece='BASE'),'replacement supersedes BASE unit: recursive'),
                                     (dict(old_rows[0],disposition='owner-held'),'replacement supersedes owner-held unit: recursive')]:
                refused(lambda: inventory.replacement_rows([altered,replacement]),expected)
            refused(lambda: inventory.replacement_rows(population,authored=set()),
                    'replacement is not an authored progress unit: recursive')
            refused(lambda: inventory.replacement_rows(population+[dict(replacement,name='second')]),
                    'already-replaced row: recursive')
            present = dict(repaired,recursive=without_consumer['recursive'])
            if inventory.replacement_progress(population,{'recursive':'identical full digest'},present)['recursive']!='superseded unit still present: recursive':
                raise ValueError('unchanged superseded declaration closed')
            in_place = dict(spec,declarations={'recursive':'1'*64})
            in_place_rows = old_rows+[dict(replacement,note=json.dumps(in_place))]
            if inventory.replacement_progress(in_place_rows,{'recursive':'full digest mismatch'},{'recursive':'1'*64})['recursive']!='reconciled by replacement':
                raise ValueError('in-place digest repair refused')
            two_old = old_rows+[inventory.row(record='declaration',name='second-old',piece='F03')]
            refused(lambda: inventory.replacement_rows(two_old+[dict(replacement,note=json.dumps(dict(spec,supersedes=['recursive','second-old'])))]),
                    'invalid replacement field one replacement obligation: control-repair')
            accepting_text = review_path.read_text()
            for text,expected in [
                (accepting_text+'VERDICT: REJECT\n','review artifact last verdict is not ACCEPT: ConcreteSpec/landing/reviews/control.txt'),
                (accepting_text.replace('UNIT: recursive\n',''),'review artifact does not cover replacement: control-repair'),
                (accepting_text.replace('COMMIT:', 'REVISION:'),'review artifact requires one COMMIT: ConcreteSpec/landing/reviews/control.txt')]:
                review_path.write_text(text)
                commit()
                bad_review = dict(spec,review=dict(verdict='ACCEPT',reference=dict(path='ConcreteSpec/landing/reviews/control.txt',commit=git('rev-parse','HEAD'))))
                refused(lambda: inventory.replacement_rows(old_rows+[dict(replacement,note=json.dumps(bad_review))]),expected)
            review_path.write_text(accepting_text)
            commit()
            multi = dict(spec,declarations={new_name:repaired[new_name], 'second': '1'*64})
            multi_rows = old_rows+[dict(replacement,note=json.dumps(multi))]
            if inventory.replacement_progress(multi_rows,before,repaired)['recursive']!='missing replacement declaration: second':
                raise ValueError('partial multi-declaration replacement landed')
            if inventory.replacement_progress(multi_rows,before,dict(repaired,second='1'*64))['recursive']!='reconciled by replacement':
                raise ValueError('complete multi-declaration replacement refused')
            print('control PASS: reviewed replacement closes only at exact replacement digests; invalid, unknown, duplicate and partial rows refused')
            # Command targets use the production scope/digest/ordinal identity.
            command_rows = [inventory.row(record='command',name='old-example',piece='F02',
                disposition='replace',source='ConcreteSpec/Control.lean',line=1,end_line=1,
                kind='example',digest='0'*64)]
            probe.write_text('example : True := by trivial\n')
            commit()
            command_head = inventory.head_population()
            key = next(iter(command_head.commands.values()))
            target = dict(module='ConcreteSpec.Control',scope=[list(x) for x in key[1]],
                          kind=key[2],digest=key[3],ordinal=key[4])
            exported = json.loads(json.dumps(inventory.head_digest_export(command_head)))
            if exported['headCommandKeys']!=[target] or exported['declarations']!={}:
                raise ValueError('HEAD command export cannot write replacement record')
            target = exported['headCommandKeys'][0]
            cspec = dict(spec,supersedes=['command:old-example'],declarations={},commands=[target])
            crec = dict(replacement,piece='F02',note=json.dumps(cspec))
            cr = {'command:old-example':'full digest mismatch'}
            if inventory.replacement_progress(command_rows+[crec],cr,command_head)['command:old-example']!='reconciled by replacement':
                raise ValueError('reviewed F02 example replacement did not close')
            if inventory.replacement_progress(command_rows+[crec],cr,inventory.Digests())['command:old-example']!='missing replacement command: '+json.dumps(key):
                raise ValueError('missing command replacement closed')
            changed_key = (*key[:3],'f'*64,key[4])
            changed_head = inventory.Digests(commands={'changed':changed_key})
            observed = inventory.replacement_progress(command_rows+[crec],cr,changed_head)['command:old-example']
            if observed!='replacement command digest mismatch: '+json.dumps(key): raise ValueError(observed)
            retired = dict(cspec,action='retire',commands=[])
            if inventory.replacement_progress(command_rows+[dict(crec,note=json.dumps(retired))],cr,{})['command:old-example']!='reconciled by retirement':
                raise ValueError('reviewed retirement refused')
            still_present = inventory.Digests(commands={'old':next(iter(inventory.command_match_keys(command_rows).values()))})
            if inventory.replacement_progress(command_rows+[dict(crec,note=json.dumps(retired))],cr,still_present)['command:old-example']!='superseded unit still present: command:old-example':
                raise ValueError('retirement closed with the old command still present')
            d_retired = dict(spec,action='retire',declarations={},commands=[])
            d_record = dict(replacement,note=json.dumps(d_retired))
            if inventory.replacement_progress(old_rows+[d_record],before,{})['recursive']!='reconciled by retirement':
                raise ValueError('reviewed declaration retirement refused')
            if inventory.replacement_progress(old_rows+[d_record],{'recursive':'identical full digest'},{'recursive':without_consumer['recursive']})['recursive']!='superseded unit still present: recursive':
                raise ValueError('retirement closed with the old declaration still present')
            if inventory.replacement_progress(old_rows+[d_record],before,{'recursive':'1'*64})['recursive']!='superseded unit still present: recursive':
                raise ValueError('retirement closed with a changed declaration still present')
            for bad,expected in ((dict(retired,reason=''),'invalid replacement field reason: control-repair'),
                                 (dict(retired,review={}), 'missing accepting review: control-repair'),
                                 (dict(cspec,action='retire'), 'invalid replacement field action targets: control-repair')):
                refused(lambda: inventory.replacement_rows(command_rows+[dict(crec,note=json.dumps(bad))]),expected)
            foreign = inventory.row(record='declaration',name=new_name,piece='F04')
            refused(lambda: inventory.replacement_rows(population+[foreign]),
                    'replacement target belongs to another piece: '+new_name)
            independent = dict(foreign,piece='F03')
            refused(lambda: inventory.replacement_rows(population+[independent]),
                    'replacement target is independent snapshot unit: '+new_name)
            sibling = inventory.row(record='declaration',name='other-old',piece='F03')
            # Cover both obligations in the fixture review.
            review_path.write_text(review_path.read_text().replace('VERDICT:', 'UNIT: other-old\nUNIT: command:sibling-command\nVERDICT:'))
            commit()
            spec['review']['reference']['commit']=git('rev-parse','HEAD')
            replacement['note']=json.dumps(spec)
            population=old_rows+[replacement]
            cspec['review']=spec['review']
            crec['note']=json.dumps(cspec)
            shared = dict(replacement,name='shared',note=json.dumps(dict(spec,supersedes=['other-old'])))
            refused(lambda: inventory.replacement_rows(population+[sibling,shared]),'shared replacement target: shared')
            unrelated = git('-c','user.name=Control','-c','user.email=c@example.invalid',
                            'commit-tree',git('rev-parse','HEAD^{tree}'),'-m','unrelated provenance')
            for reference,expected in [
                ({'path':'ConcreteSpec/landing/reviews/missing.txt','commit':git('rev-parse','HEAD')},
                 'missing review artifact: ConcreteSpec/landing/reviews/missing.txt'),
                ({'path':'ConcreteSpec/landing/reviews/control.txt','commit':unrelated},
                 'review commit is not an ancestor of HEAD: control-repair'),
                ({'note':'refs/notes/missing-review','commit':git('rev-parse','HEAD')},
                 'invalid review reference fields: control-repair'),
                ({'path':'flake.nix','commit':git('rev-parse','HEAD')},
                 'review path outside ConcreteSpec/landing/reviews/*.txt: control-repair')]:
                bad = dict(spec,review=dict(reference=reference,verdict='ACCEPT'))
                refused(lambda: inventory.replacement_rows(old_rows+[dict(replacement,note=json.dumps(bad))]),expected)
            command_foreign = dict(command_rows[0],name='foreign-command',piece='F04',
                                   source=key[0],kind=key[2],digest=key[3])
            refused(lambda: inventory.replacement_rows(command_rows+[command_foreign,crec]),
                    'replacement command belongs to another piece: foreign-command')
            forbidden = root/'ConcreteSpec/Forbidden.lean'
            forbidden.write_text('structure Var where\n  hint : String\n  id : Nat\n  deriving DecidableEq\n'
                                 'example : Var.mk "x" 0 ≠ Var.mk "y" 0 := by decide\n')
            commit()
            combined = inventory.head_population()
            forbidden_key = next(k for k in combined.commands.values()
                                 if k[0]=='ConcreteSpec/Forbidden.lean' and k[2]=='example')
            actual_old = dict(command_rows[0],source=forbidden_key[0],kind=forbidden_key[2],digest=forbidden_key[3])
            observed = inventory.replacement_progress([actual_old,crec],cr,combined)['command:old-example']
            if observed!='superseded unit still present: command:old-example': raise ValueError(observed)
            forbidden.unlink()
            commit()
            print('control PASS: F02 forbidden raw-record inequality left beside its replacement keeps obligation open')
            # A second record cannot reuse a replacing command either.
            sibling_command = dict(command_rows[0],name='sibling-command')
            shared_command = dict(crec,name='shared-command',note=json.dumps(
                dict(cspec,supersedes=['command:sibling-command'])))
            refused(lambda: inventory.replacement_rows(command_rows+[sibling_command,crec,shared_command]),
                    'shared replacement target: shared-command')
            print('control PASS: F02 command replacement, retirement, ownership and exclusive targets')
            proof = root/'ConcreteSpec/Proofs/Header.lean'
            proof.parent.mkdir()
            # Build the complete root and minimal direct imports with production checker.
            probe.write_text('def approved : Nat := 1\n')
            proof.write_text('import ConcreteSpec.Control\nopen Nat\nexample : approved = 1 := rfl\n')
            (root/'ConcreteSpec.lean').write_text('import ConcreteSpec.Proofs.Header\n')
            commit()
            targets = root/'ConcreteSpec/landing/headers.json'
            targets.parent.mkdir(exist_ok=True)
            target_spec = dict(revision=git('rev-parse','HEAD'),property='exact-imports-and-complete-root')
            targets.write_text(json.dumps(target_spec))
            header = 'header:F01'
            snapshot_property = inventory.header_targets()
            def property_reason():
                return inventory.progress_reasons(snapshot_property,inventory.head_population())[header]
            if property_reason()!='identical full digest': raise ValueError('canonical F01 property refused')
            probe2 = root/'ConcreteSpec/Later.lean'
            probe2.write_text('def later : Nat := 2\n')
            proof.write_text('import ConcreteSpec.Later\n'+proof.read_text())
            commit()
            expected = "F01 property failure: ConcreteSpec/Proofs/Header.lean expected imports ['ConcreteSpec.Control'], actual ['ConcreteSpec.Control', 'ConcreteSpec.Later']"
            observed = property_reason()
            if observed!=expected: raise ValueError(f'expected {expected!r}, got {observed!r}')
            probe2 = root/'ConcreteSpec/Later.lean'
            probe2.write_text('def later : Nat := 2\n')
            proof.write_text('import ConcreteSpec.Control\nimport ConcreteSpec.Later\nopen Nat\nexample : approved + later = 3 := rfl\n')
            commit()
            if property_reason()!='identical full digest': raise ValueError('legitimate later import unlanded F01')
            (root/'ConcreteSpec.lean').write_text('import ConcreteSpec.Control\n')
            commit()
            expected = 'F01 property failure: ConcreteSpec.lean unreachable modules: ConcreteSpec.Later, ConcreteSpec.Proofs.Header'
            observed = property_reason()
            if observed!=expected: raise ValueError(f'expected {expected!r}, got {observed!r}')
            target_spec['revision']=unrelated
            targets.write_text(json.dumps(target_spec))
            try: inventory.header_targets()
            except ValueError as exc:
                if str(exc)!='header revision is not an ancestor of HEAD': raise
            else: raise ValueError('unrelated header revision accepted')
            output = io.StringIO()
            with contextlib.redirect_stdout(output):
                inventory.report([inventory.row(record='piece',name='F01')],reasons={header:'identical full digest'})
            if 'F01\tlanded=1/1' not in output.getvalue(): raise ValueError('F01 property excluded from totals')
            output = io.StringIO()
            with contextlib.redirect_stdout(output):
                inventory.report([inventory.row(record='piece',name='F01')],reasons={header:expected})
            if 'progress refusal\t'+header+'\t'+expected not in output.getvalue():
                raise ValueError('F01 failing file omitted from report')
            print('control PASS: F01 exact-import property survives legitimate additions; extra imports, incomplete root and nonancestor revision fail')
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
