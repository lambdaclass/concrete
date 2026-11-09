"""One real child boundary probe and exact field detector for every adapter."""
import json
import os
from pathlib import Path
import stat
import sys
from process_adapter import process_run

PROBE = r"""
import json, os, stat, tempfile
from pathlib import Path
mask = os.umask(0o077); os.umask(mask)
try:
    os.fstat(9); fd9 = True
except OSError:
    fd9 = False
slot = os.environ.get('CONCRETE_HEAVY_SLOT_FD')
slot_valid = None
if slot is not None:
    try:
        identity = os.fstat(int(slot))
        pool = Path(os.environ.get('CONCRETE_HEAVY_SLOT_DIR', '/var/lib/concrete/heavy-slots'))
        slot_valid = any((p.stat().st_dev, p.stat().st_ino) == (identity.st_dev, identity.st_ino)
                         for p in (pool / 'slot-1', pool / 'slot-2'))
    except (OSError, ValueError):
        slot_valid = False
home = Path.home()
temporary = Path(tempfile.gettempdir())
print(json.dumps(dict(environment=dict(os.environ), cwd=os.getcwd(), umask=mask,
    home_private=home.is_dir() and stat.S_IMODE(home.stat().st_mode) == 0o700,
    config_absent=not (home / '.gitconfig').exists(),
    temp_private=temporary.is_dir() and stat.S_IMODE(temporary.stat().st_mode) == 0o700,
    fd9=fd9, slot_valid=slot_valid)))
"""


def detect(observed, cwd):
    env = observed['environment']
    if observed['slot_valid'] is False:
        raise ValueError('INCOMPLETE heavy slots: invalid inherited slot descriptor')
    checks = [
        ('env.SCHEDULER_FOREIGN', 'SCHEDULER_FOREIGN' not in env),
        ('cwd', observed['cwd'] == str(Path(cwd).resolve())),
        ('HOME', observed['home_private'] and observed['config_absent'] and
         Path(env.get('HOME', '')).name == 'home' and
         Path(env.get('HOME', '')).parent.name.startswith('scheduled-process-') and
         env.get('XDG_CONFIG_HOME') == str(Path(env['HOME']) / '.config') and
         env.get('GIT_CONFIG_GLOBAL') == os.devnull and env.get('GIT_CONFIG_NOSYSTEM') == '1' and
         env.get('GIT_CONFIG_KEY_0') == 'core.hooksPath' and env.get('GIT_CONFIG_VALUE_0') == os.devnull),
        ('umask', observed['umask'] == 0o077),
        ('locale', env.get('LC_ALL') == 'C' and env.get('LANG') == 'C'),
        ('timezone', env.get('TZ') == 'UTC'),
        ('temp', observed['temp_private'] and env.get('TMPDIR') == str(Path(env['HOME']).parent / 'tmp') and
         env.get('TMP') == env.get('TMPDIR') == env.get('TEMP')),
        ('fd.9', not observed['fd9'] or env.get('CONCRETE_HEAVY_SLOT_FD') == '9'),
        ('credential scope', not ({'GH_TOKEN', 'GITHUB_TOKEN'} & env.keys())),
    ]
    for field, valid in checks:
        if not valid:
            raise ValueError('FAIL scheduled process context: ' + field)


def main(helper, cwd):
    result = process_run([sys.executable, '-I', helper, '--cwd', cwd, '--timeout', '3',
                          '--', sys.executable, '-I', '-c', PROBE],
                         cwd=cwd, capture_output=True, text=True, timeout=4)
    if result.returncode or result.stderr:
        raise ValueError('INCOMPLETE scheduled process context: probe did not complete')
    detect(json.loads(result.stdout), cwd)
    return 0


if __name__ == '__main__':
    try: sys.exit(main(*sys.argv[1:]))
    except ValueError as error: sys.exit(str(error))
