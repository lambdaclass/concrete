"""Real approval and row-audit failures must survive result aggregation."""
from pathlib import Path
import subprocess
import tempfile
import runpy
import json
import hashlib

with tempfile.TemporaryDirectory() as temporary:
    rows = Path(temporary) / 'empty.rows'
    rows.write_text('')
    approval = runpy.run_path('scripts/checks/lib/trusted_approval.py')
    report = Path('tests/golden/slice0_trusted_base.txt').read_text()
    record = json.loads(Path('tests/golden/slice0_trusted_base_approval.json').read_text())
    record['registry_sha256'] = hashlib.sha256(approval['registry_rows'](report)).hexdigest()
    record['theorems_sha256'] = hashlib.sha256(approval['theorem_rows'](report)).hexdigest()
    record['inputs'] = approval['input_digests']()
    candidate = Path(temporary) / 'candidate.json'
    candidate.write_text(json.dumps(record))
    signers = Path(temporary) / 'allowed_signers'
    signers.write_text('# deliberately awaiting owner key\n')
    entry = Path(temporary) / 'approval.py'
    entry.write_text("""import runpy,sys
approval = runpy.run_path('scripts/checks/lib/trusted_approval.py')
approval['authenticate_owner'].__globals__['OWNER_SIGNERS'] = sys.argv[2]
try:
    approval['verify']('tests/golden/slice0_trusted_base.txt', sys.argv[1])
except ValueError as error:
    sys.exit('FAIL approval: ' + str(error))
""")
    script = '''set -euo pipefail
source scripts/checks/lib/trusted_legs.sh
trusted_leg approval python3 -I "$3" "$2" "$4"
trusted_leg examples python3 -I scripts/checks/lib/example_rows.py "$1"
fail_fast() { false; echo 'escaped errexit'; }
trusted_leg fail-fast fail_fast
trusted_leg final true
exit "$trusted_legs_status"
'''
    result = subprocess.run(['bash', '-c', script, 'control', str(rows), str(candidate), str(entry), str(signers)],
                            text=True, capture_output=True, timeout=10)
    assert result.returncode == 1, result
    lines = (result.stdout + result.stderr).splitlines()
    for diagnostic in ['FAIL approval: awaiting owner key',
                       'FAIL examples: no example-json rows observed',
                       'FAIL trusted-base leg: approval (exit 1)',
                       'FAIL trusted-base leg: examples (exit 1)',
                       'FAIL trusted-base leg: fail-fast (exit 1)',
                       'PASS trusted-base leg: final']:
        assert lines.count(diagnostic) == 1, (diagnostic, result)
    assert 'escaped errexit' not in lines, result
print('PASS independent legs retain approval and planted examples failures')
