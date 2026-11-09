import runpy
from pathlib import Path
runpy.run_path(str(Path(__file__).with_name('suite_bootstrap.py')))

"""Real private clone, registry, slot wrapper, watchdog and multi-file report controls.

Only Nix environment entry is replaced: no builds or production campaigns run here.
"""
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch
import run
import protocol
import test_execution
from test_protocol import RUNNERS

ROOT = run.ROOT

class Integrated(unittest.TestCase):
    def invoke(self, programs, *, environment=None, occupied=False, control_command=None):
        with tempfile.TemporaryDirectory() as work:
            scratch=Path(work); controller=scratch/'controller'; controller.mkdir()
            lib=controller/'scripts/checks/lib'; lib.mkdir(parents=True)
            scheduled=controller/'scripts/checks/scheduled'; scheduled.mkdir()
            for name in ('bounded.sh','bounded_exec.py','scheduled_process.py','scheduled_process.sh','heavy_slots.sh','gate_outcome.py','scheduled_outcome.py','gate_env.sh','has_match.sh'):
                shutil.copy(ROOT/'scripts/checks/lib'/name,lib/name)
            shutil.copy(ROOT/'scripts/checks/scheduled/with_slot.sh',scheduled)
            from runtime_support import copy_policy_runtime
            copy_policy_runtime(scheduled.parent.parent.parent)
            pool=scratch/'pool'; pool.mkdir()
            handles=[]
            for n in (1,2):
                (pool/f'slot-{n}').touch()
                if occupied:
                    import fcntl
                    handle=(pool/f'slot-{n}').open('r+')
                    fcntl.flock(handle,fcntl.LOCK_EX|fcntl.LOCK_NB); handles.append(handle)
            # The lightweight environment shim still executes the complete real wrapper path.
            bin_dir=scratch/'bin'; bin_dir.mkdir()
            nix=bin_dir/'nix'; nix.write_text('#!/bin/sh\n[ "$1" = develop ] && [ "$2" = --command ] || exit 88\nshift 2\nexec "$@"\n'); nix.chmod(0o755)
            rows=[]
            for n,program in enumerate(programs):
                name=f'campaign-{n}.sh'
                (controller/name).write_text('#!/usr/bin/env bash\nset -uo pipefail\n'+program)
                rows.append(dict(id=f'campaign-{n}',tiers=['daily'],command=['bash',name],budget_seconds=2,
                                 runner_class='concrete-ci-2',artifacts=[dict(path=f'report-{n}',sha_file=f'report-{n}/selected-sha.txt')],
                                 controls=[dict(id='refusal',command=['bash','-c',
                                  'test "$SELECTED_TIER" = daily && test "$SCHEDULED_ATTEMPT_ID" = 1/1 && test "$RUNNER_ENVIRONMENT" = self-hosted && printf "planted refusal"; exit 7'],
                                  exit_code=7,diagnostic='planted refusal')],tools={},seed=0,workflow=None))
            if control_command is not None:
                for row in rows:row['controls'][0]['command']=control_command
            (controller/'scripts/checks/gates.json').write_text(json.dumps(dict(schema=1,windows=dict(daily=129600,weekly=691200),gates=rows)))
            def git(*args):
                return protocol.git('-C', str(controller), *args)
            git('init','-q');git('add','.')
            git('-c','user.name=Scheduled control','-c','user.email=scheduled@example.invalid','-c','core.hooksPath=/dev/null','commit','-qm','fixture')
            sha=git('rev-parse','HEAD')
            env={**test_execution.Execution().environment(), 'CONCRETE_GATES_SHELL':'1', 'CONCRETE_HEAVY_SLOT_DIR':str(pool),
                 'PATH':str(bin_dir)+os.pathsep+os.environ['PATH']}
            if environment:env.update(environment)
            output=scratch/'result'
            with patch.dict(os.environ,env,clear=True), patch.object(run,'ROOT',controller), patch.object(run,'inventory',return_value=RUNNERS):
                code=run.run(sha,'daily',output, env)
            index=json.loads((output/'index.json').read_text())
            if index['status'] == 'PASS':
                with patch.object(protocol, 'REGISTRY_ROOT', controller):
                    validated = protocol.validate(index, sha, 'daily', RUNNERS, 'example/repo', '1/1')
                self.assertEqual(validated['qualification_status'], 'PASS')
                self.assertEqual(validated['qualification'], [])
            files={str(p.relative_to(output)):p.read_bytes() for p in output.rglob('*') if p.is_file()}
            for handle in handles:handle.close()
            return code,index,files,sha

    def report(self,n=0):
        return f'''mkdir -p report-{n}/families
printf '%s\\n' "$SELECTED_SHA" > report-{n}/selected-sha.txt
printf 'size\\tsample\\n1\\t1\\n2\\t2\\n' > report-{n}/families/raw.samples.tsv
printf 'depth=128\\n' > report-{n}/depth.txt
'''

    def test_real_campaign_report_survives_checkout_cleanup(self):
        code,index,files,sha=self.invoke([self.report()])
        self.assertEqual((code,index['status']),(0,'PASS'))
        self.assertEqual(index['gate_revision'],sha)
        artifact=index['gates'][0]['artifacts'][0]
        self.assertEqual(artifact['sha'],sha)
        self.assertEqual(len(artifact['files']),3)
        for entry in artifact['files']:
            self.assertEqual(entry['sha256'],hashlib.sha256(files[entry['path']]).hexdigest())
        self.assertIn(b'size\tsample\n1\t1\n2\t2\n',files.values())
        self.assertEqual(index['gates'][0]['controls'][0]['diagnostic'],'planted refusal')

    def test_report_sha_mismatch_is_fail(self):
        code,index,files,_=self.invoke([self.report()+"printf '%040d\\n' 0 > report-0/selected-sha.txt\n"])
        self.assertEqual((code,index['status']),(1,'FAIL'))
        self.assertEqual(index['gates'][0]['error'],'FAIL scheduled artifact: selected SHA mismatch report-0')
        self.assertIn('gate-0-artifacts/report-0/depth.txt',files)

    def test_structured_sha_mismatch_is_fail(self):
        program=self.report()+'''SELECTED_SHA=0000000000000000000000000000000000000000 python3 "$SCHEDULED_OUTCOME_HELPER" PASS 'forged report'
'''
        code,index,files,_=self.invoke([program])
        self.assertEqual((code,index['status']),(1,'FAIL'))
        self.assertEqual(files['gate-0.log'],b'FAIL scheduled outcome: selected SHA mismatch\n')

    def test_report_symlink_is_fail(self):
        program=self.report()+"ln -s /etc/hosts report-0/foreign\n"
        code,index,_,_=self.invoke([program])
        self.assertEqual((code,index['status']),(1,'FAIL'))
        self.assertEqual(index['gates'][0]['error'],'FAIL scheduled artifact: unsafe path report-0/foreign')

    def test_missing_report_is_fail(self):
        code,index,_,_=self.invoke(['true'])
        self.assertEqual((code,index['status']),(1,'FAIL'))
        self.assertEqual(index['gates'][0]['error'],'FAIL scheduled artifact: missing report-0')

    def test_partial_report_without_binding_stays_incomplete(self):
        program = "mkdir report-0; echo partial > report-0/raw.txt; python3 \"$SCHEDULED_OUTCOME_HELPER\" INCOMPLETE 'measurement unavailable'; exit 1"
        code,index,files,_=self.invoke([program])
        self.assertEqual((code,index['status']),(1,'INCOMPLETE'))
        artifact=index['gates'][0]['artifacts'][0]
        self.assertEqual(artifact['binding_status'], 'INCOMPLETE')
        self.assertEqual(artifact['diagnostic'], 'INCOMPLETE scheduled artifact: missing SHA binding report-0/selected-sha.txt')
        self.assertIsNone(artifact['sha'])
        self.assertEqual(files['gate-0-artifacts/report-0/raw.txt'], b'partial\n')

    def test_real_unprovisioned_or_busy_pool_cannot_pass(self):
        code,index,files,_=self.invoke([self.report()],occupied=True)
        self.assertEqual((code,index['status']),(1,'INCOMPLETE'))
        self.assertEqual(files['gate-0.log'],b'INCOMPLETE heavy slots: both host slots occupied\n')

    def test_runner_environment_verified_before_execution(self):
        for environment in ('','github-hosted'):
            code,index,files,_=self.invoke([self.report()],environment={'RUNNER_ENVIRONMENT':environment})
            self.assertEqual((code,index['status']),(1,'INCOMPLETE'))
            self.assertEqual(files['incomplete.log'],b'forged runner: expected RUNNER_ENVIRONMENT=self-hosted\n')
            self.assertNotIn('gate-0.log',files)

    def test_internal_timeouts_and_failure_precedence(self):
        for stage,command in (('measurement', "python3 -c 'import time; time.sleep(1)'"),
                              ('build', "bash -c 'sleep 1'")):
            program=self.report()+f'''source scripts/checks/lib/scheduled_process.sh
bounded_run 0.02 -- {command}; rc=$?
if bounded_is_incomplete "$rc"; then python3 "$SCHEDULED_OUTCOME_HELPER" INCOMPLETE "measurement unavailable"; exit 1; fi
[ "$rc" = 0 ] || exit 1
'''
            code,index,files,_=self.invoke([program])
            self.assertEqual((code,index['status']),(1,'INCOMPLETE'),stage)
            self.assertEqual(files['gate-0.log'],b'INCOMPLETE scheduled gate: nested watchdog or gate incomplete\n',stage)
        control=['bash','-c', "source scripts/checks/lib/scheduled_process.sh; bounded_run 0.02 -- bash -c 'sleep 1'; rc=$?; if bounded_is_incomplete \"$rc\"; then python3 \"$SCHEDULED_OUTCOME_HELPER\" INCOMPLETE 'control unavailable'; fi; exit 1"]
        code,index,_,_=self.invoke([self.report()],control_command=control)
        self.assertEqual((code,index['status']),(1,'INCOMPLETE'))
        self.assertEqual(index['gates'][0]['controls'][0],dict(id='refusal',status='INCOMPLETE',
                         diagnostic='INCOMPLETE scheduled gate: nested watchdog or gate incomplete'))
        failure=self.report()+'''python3 "$SCHEDULED_OUTCOME_HELPER" FAIL 'real measurement mismatch'
source scripts/checks/lib/scheduled_process.sh
bounded_run 0.02 -- python3 -c 'import time; time.sleep(1)'
exit 1
'''
        code,index,files,_=self.invoke([failure,self.report(1)])
        self.assertEqual((code,index['status']),(1,'FAIL'))
        self.assertEqual([g['status'] for g in index['gates']],['FAIL','PASS'])
        self.assertEqual(files['gate-0.log'],b'FAIL scheduled gate: explicit failure outcome\n')
        code,index,_,_=self.invoke([failure],control_command=control)
        self.assertEqual((code,index['status']),(1,'FAIL'))
        self.assertEqual(index['gates'][0]['controls'][0]['status'],'INCOMPLETE')



if __name__ == "__main__":
    from suite_adapter import direct_main
    direct_main()
