#!/usr/bin/env bash
# Inventory structure is permanent; authority eligibility is checked separately by SPEC-RULES.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh"
cd "$(dirname "$0")/../.."
python3 -I - <<'PY'
from pathlib import Path
import sys
sys.path.insert(0, str(Path('scripts/spec').resolve()))
from decision_inventory import check_structure
text = Path('docs/decisions.md').read_text()
errors = check_structure(text)
if errors:
    raise SystemExit('\n'.join(errors))
for name, bad in [
    ('standalone anchor', '<a id="control"></a>**Title**\n'),
    ('second paragraph', '- Parent\n\n  <a id="control"></a>**Title**\n'),
    ('duplicate anchor', '- <a id="control"></a>**A**\n- <a id="control"></a>**B**\n'),
    ('outdented reply', '- <a id="a"></a>**A**\n\nOwner reply: yes.\n'),
    ('outdented fence', '- <a id="a"></a>**A**\n\n```text\nyes\n```\n'),
    ('two anchors', '- <a id="a"></a>**A** <a id="b"></a>**B**\n'),
]:
    if not check_structure(bad):
        raise SystemExit('inventory control unexpectedly accepted: ' + name)
print('decision inventory: all anchors open unique list items; six negative controls pass')
PY
