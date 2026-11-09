#!/usr/bin/env python3
"""Exercise production rejection paths without reviewers, notes writes or remotes."""
import os
from pathlib import Path
import re
import shlex
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]
def source(path):
    return (ROOT / path).read_text()
def function(path, name):
    match = re.search(r'^' + name + r'\(\).*?^}', source(path), re.M | re.S)
    assert match, (path, name)
    return match.group()
def shell(code, expected=0, diagnostic=None, env=None, exact=False):
    proc = subprocess.run(['bash', '-c', 'set -euo pipefail\n' + code], text=True, capture_output=True, env=env, timeout=15)
    assert proc.returncode == expected, (code, proc.returncode, proc.stdout, proc.stderr)
    if diagnostic:
        assert diagnostic in proc.stdout + proc.stderr, (diagnostic, proc.stdout, proc.stderr)
    if exact:
        assert (proc.stdout + proc.stderr).strip() == diagnostic, (proc.stdout, proc.stderr, diagnostic)
    return proc.stdout

review = source('scripts/review/review.sh')
a = review.index('  case "$result" in', review.index('  stripped='))
b = review.index('  esac', a) + len('  esac')
aggregation = review[a:b]
for status in ('invalid', 'future-status'):
    shell('result=' + status + '\nN_BLOCKED=0; N_FINDINGS=0; FINDINGS_TEXT=; label=control; group=control; paths=control; stripped=control\ndie() { echo "$*"; exit 19; }\n' + aggregation + '\n[ "$N_BLOCKED" = 1 ]', 19 if status == 'future-status' else 0, 'unknown reviewer-output status: future-status' if status == 'future-status' else None)

signoff = source('scripts/land/signoff.sh')
a = signoff.index('    case "$c_status" in')
b = signoff.index('    esac', a) + len('    esac')
for status, diagnostic in [('invalid', 'the codex output is invalid (planted invalid)'), ('future-status', 'the codex output fails the reviewer-output contract: planted invalid')]:
    shell('c_status=' + status + '\nc_reason="planted invalid"; r=codex; verdicts=\nbad() { echo "$*"; exit 17; }\n' + signoff[a:b], 17, diagnostic)

with tempfile.TemporaryDirectory() as directory:
    tmp = Path(directory)
    # git is a read-only transport double. Every mutating command is refused.
    git = tmp / 'git'
    git.write_text('''#!/usr/bin/env python3
import os, sys
args=sys.argv[1:]
if args[0] in ('rev-parse','merge-base'): print('1'*40)
elif args[0]=='notes' and 'show' in args: print('verdict: '+os.environ['CONTROL_STATUS'])
elif args[0]=='fetch': pass
else: sys.exit('unexpected git command: '+repr(args))
''')
    git.chmod(0o755)
    env = dict(os.environ, PATH=directory + os.pathsep + os.environ['PATH'])
    verify = shlex.quote(str(ROOT / 'scripts/review/verify.sh'))
    for status, reason in [('invalid', 'invalid reviewer evidence is not a verdict'), ('future-status', 'bad verdict')]:
        env['CONTROL_STATUS'] = status
        shell('bash ' + verify + ' HEAD', 2, reason, env)
        shell('bash ' + verify + ' --fields HEAD', 2, reason, env)
        # notes readiness uses the verifier, and cannot bless invalid evidence.
        shell('bash ' + shlex.quote(str(ROOT / 'scripts/land/notes.sh')) + ' ready HEAD', 2, env=env)
        # Train's real provenance function refuses a present invalid head note.
        code = 'VERIFY=' + verify + '\nREMOTE=origin\nstate_get_soft() { echo ' + '1'*40 + '; }\n'
        code += function('scripts/land/train.sh', 'review_provenance') + '\nreview_provenance control ' + '1'*40
        shell(code, 1, 'the head\'s review note does not qualify:', env)
    env['CONTROL_STATUS'] = 'invalid'
    # Collector must preserve invalid explicitly, and turn unknown statuses into errors.
    collector = source('scripts/review/collect_output.sh')
    for status in ('invalid', 'future-status'):
        fake = tmp / 'output_contract.py'
        fake.write_text('print("status: ' + status + '\\nreason: planted invalid")\n')
        (tmp / 'out').write_text('planted output')
        (tmp / 'log').write_text('planted log')
        code = 'source ' + shlex.quote(str(ROOT / 'scripts/review/collect_output.sh')) + '\n'
        code += 'verify_review_archive() { return 0; }\nbounded_is_incomplete() { return 1; }\nCOLLECT_OUTPUT_DIR=' + shlex.quote(directory) + '\n'
        code += 'validate_reviewer_output . HEAD tree manifest ' + shlex.quote(str(tmp / 'out')) + ' ' + shlex.quote(str(tmp / 'log')) + ' clean,findings 0'
        expected = 'status: invalid\nreason: planted invalid' if status == 'invalid' else 'status: error\nreason: unknown reviewer-output status: future-status'
        shell(code, diagnostic=expected, env=env)
# Exercise the production collection tail: every rejected status removes --out.
with tempfile.TemporaryDirectory() as directory:
    out = Path(directory) / 'rejected.out'
    a = signoff.index('  if [ "$rc" -ne 0 ]', signoff.index('cmd_collect()'))
    b = signoff.index('  echo "signoff: collected', a)
    for status in ('invalid', 'blocked', 'future-status'):
        out.write_text('planted rejected evidence')
        diagnostic = ('signoff: collect invalid reviewer evidence (planted invalid)' if status == 'invalid'
                      else 'signoff: collect (codex on 1111111111) failed: status=' + status + ' (planted invalid, exit 0)')
        shell('out=' + shlex.quote(str(out)) + '; rc=0; c_status=' + status + '; c_reason="planted invalid"; reviewer=codex; sha=1111111111111111111111111111111111111111\n'
              + 'collect_tail() {\n' + signoff[a:b] + '\n}\ncollect_tail', 1, diagnostic, exact=True)
        assert not out.exists(), status
# Readiness's reporting tail must retain failure after notes readiness rejects invalid.
ready = source('scripts/land/ready.sh')
tail = ready[ready.index('verdict="$(bash'):]
shell('SELF_DIR=control; head=1111111111111111111111111111111111111111; base=$head; gates=pass\n'
      'bash() { case "$*" in *" get review "*) echo invalid ;; *" ready "*) return 1 ;; *) exit 23 ;; esac; }\n' + tail,
      1, 'review=invalid (not a verdict)')
print('citation caller controls: PASS (review, collector, signoff evidence, verifier, notes, readiness, train)')
