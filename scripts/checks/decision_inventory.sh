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
for name, bad, diagnostic in [
    ('standalone anchor', '<a id="control"></a>**Title**\n', 'anchor does not open one list item'),
    ('second paragraph', '- Parent\n\n  <a id="control"></a>**Title**\n', 'anchor does not open one list item'),
    ('duplicate anchor', '- <a id="control"></a>**A**\n- <a id="control"></a>**B**\n', 'duplicate explicit anchor'),
    ('outdented reply', '- <a id="a"></a>**A**\n\nOwner reply: yes.\n', 'record field outside a list item'),
    ('outdented fence', '- <a id="a"></a>**A**\n\n```text\nyes\n```\n', 'record fence outside a list item'),
    ('two anchors', '- <a id="a"></a>**A** <a id="b"></a>**B**\n', 'multiple anchors in one item'),
    ('duplicate navigation', '<a id="nav"></a>\n\n## Heading\n\n- <a id="nav"></a>**Title**\n', 'duplicate explicit anchor'),
    ('duplicate table', '| Entry |\n|---|\n| <a id="a"></a>Text |\n\n- <a id="a"></a>**Title**\n', 'duplicate explicit anchor'),
    ('multiline outdent', '- <a id="a"></a>**A**\n\nOutside\nProvenance: transcript\n', 'record field outside a list item'),
    ('indented outdent', '- <a id="a"></a>**A**\n\nOutside\n\n    quoted code\n', 'record fence outside a list item'),
    ('concealed entry', text.replace('- <a id="outside-readers-ordering"', '\n      <a id="outside-readers-ordering"'), 'unaccounted explicit anchor: outside-readers-ordering'),
]:
    if diagnostic not in '\n'.join(check_structure(bad)):
        raise SystemExit('inventory control unexpectedly accepted: ' + name)
print('decision inventory: raw anchors accounted exactly once; diagnostic controls pass')
PY
