"""Scheduled invocation/process specification; lifecycle belongs to bounded_exec.

All adapters materialize the same private context; lifecycle stays in bounded_exec.
"""
import argparse
import datetime as dt
from dataclasses import dataclass
import os
import math
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time
import fcntl
from contextlib import contextmanager

sys.path.insert(0, str(Path(__file__).resolve().parent))
from bounded_exec import supervise, Cancelled


CLOCK_DIAGNOSTIC = 'FAIL scheduled clock: undeclared read'


def _utc_now():
    """The only production wall-clock dependency; callers inject its observation."""
    return dt.datetime.now(dt.timezone.utc)


class WallObservation:
    def __init__(self, provider):
        self.provider = provider
        self.consumed = False

    def read(self):
        if self.consumed:
            raise ValueError(CLOCK_DIAGNOSTIC)
        self.consumed = True
        return self.provider()


@dataclass(frozen=True)
class OperationClock:
    observation: dt.datetime
    origin: float
    monotonic: object

    def instant(self):
        # Completion times advance from the declared observation by elapsed time;
        # wall-clock adjustments cannot change a duration or resample freshness.
        return self.observation + dt.timedelta(seconds=self.monotonic() - self.origin)

    def deadline(self, seconds):
        return self.monotonic() + seconds


def operation_clock(*, observation=None, wall=None, monotonic=None):
    monotonic = monotonic or time.monotonic
    source = WallObservation((lambda: observation) if observation is not None else (wall or _utc_now))
    observed = source.read()
    if not isinstance(observed, dt.datetime) or observed.utcoffset() != dt.timedelta(0):
        raise ValueError('FAIL scheduled clock: invalid observation')
    return OperationClock(observed, monotonic(), monotonic)


# Runtime tool identity is an explicit stage input (including Nix's resolved PATH).
# No wildcard namespaces: adding a controller input requires declaring its name.
ENVIRONMENT_KEYS = frozenset({
    'PATH', 'NIX_PATH', 'NIX_PROFILES', 'NIX_SSL_CERT_FILE', 'SSL_CERT_FILE',
    'CONCRETE_GATES_SHELL', 'CONCRETE_HEAVY_SLOT_DIR', 'CONCRETE_HEAVY_SLOT_FD',
    'GITHUB_REPOSITORY', 'GITHUB_SERVER_URL', 'GITHUB_RUN_ID', 'GITHUB_RUN_ATTEMPT',
    'GITHUB_EVENT_PATH', 'GITHUB_EVENT_NAME', 'GITHUB_OUTPUT', 'GITHUB_STEP_SUMMARY',
    'RUNNER_NAME', 'RUNNER_ENVIRONMENT', 'SELECTED_SHA', 'SELECTED_TIER',
    'SCHEDULED_ATTEMPT_ID', 'SCHEDULED_OUTCOME_FILE', 'SCHEDULED_OUTCOME_HELPER',
    'OCCURRENCE',
})
CREDENTIAL_KEYS = frozenset({'GH_TOKEN', 'GITHUB_TOKEN'})
INVALID_SLOT = 'INCOMPLETE heavy slots: invalid inherited slot descriptor'


def role_environment(environment, role):
    if role not in {'gate', 'publisher'}:
        raise ValueError('FAIL scheduled process specification: invalid role')
    keys = ENVIRONMENT_KEYS | (CREDENTIAL_KEYS if role == 'publisher' else set())
    return {k: v for k, v in environment.items() if k in keys}


def slot_descriptors(environment):
    value = environment.get('CONCRETE_HEAVY_SLOT_FD')
    if value is None:
        return ()
    try:
        if not value.isdecimal() or int(value) < 3:
            raise ValueError(INVALID_SLOT)
        fd = int(value)
        identity = os.fstat(fd)
        pool = Path(environment.get('CONCRETE_HEAVY_SLOT_DIR', '/var/lib/concrete/heavy-slots'))
        if pool.is_symlink() or not pool.is_dir():
            raise ValueError(INVALID_SLOT)
        slots = [pool / ('slot-' + str(n)) for n in (1, 2)]
        if any(p.is_symlink() or not p.is_file() for p in slots):
            raise ValueError(INVALID_SLOT)
        if not any((p.stat().st_dev, p.stat().st_ino) == (identity.st_dev, identity.st_ino) for p in slots):
            raise ValueError(INVALID_SLOT)
        fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        return (fd,)
    except (OSError, ValueError):
        raise ValueError(INVALID_SLOT) from None


@dataclass(frozen=True)
class Invocation:
    cwd: Path
    environment: dict
    role: str = 'gate'

    @classmethod
    def capture(cls, environment, cwd, role='gate'):
        return cls(Path(cwd).resolve(), role_environment(environment, role), role)

    def specification(self, argv, *, timeout=120, deadline=None, **streams):
        return dict(argv=list(argv), cwd=str(self.cwd), env=dict(self.environment),
                    role=self.role, timeout=timeout,
                    deadline=time.monotonic() + timeout if deadline is None else deadline, **streams)


@contextmanager
def private_context(spec):
    # Ignore ambient TMPDIR, HOME and Git configuration, including config includes.
    with tempfile.TemporaryDirectory(prefix='scheduled-process-', dir='/tmp') as root:
        home = Path(root) / 'home'; home.mkdir(mode=0o700)
        temporary = Path(root) / 'tmp'; temporary.mkdir(mode=0o700)
        environment = role_environment(spec['env'], spec.get('role', 'gate'))
        environment.update(HOME=str(home), XDG_CONFIG_HOME=str(home / '.config'),
                           TMPDIR=str(temporary), TMP=str(temporary), TEMP=str(temporary),
                           LC_ALL='C', LANG='C', TZ='UTC',
                           GIT_CONFIG_NOSYSTEM='1', GIT_CONFIG_GLOBAL=os.devnull,
                           GIT_CONFIG_COUNT='1', GIT_CONFIG_KEY_0='core.hooksPath',
                           GIT_CONFIG_VALUE_0=os.devnull)
        # Repository-local transport (checkout's scoped extraheader/remote URL) is
        # retained; global/system config and local hooks cannot influence execution.
        yield environment


def validate(spec):
    for field in ('cwd', 'argv', 'env', 'timeout', 'deadline'):
        if field not in spec or spec[field] is None:
            raise ValueError('FAIL scheduled process specification: missing ' + field)
    if not Path(spec['cwd']).is_absolute() or not Path(spec['cwd']).is_dir():
        raise ValueError('FAIL scheduled process specification: invalid cwd')
    if not isinstance(spec['env'], dict) or not all(isinstance(k, str) and isinstance(v, str) for k, v in spec['env'].items()):
        raise ValueError('FAIL scheduled process specification: invalid env')
    if not isinstance(spec['timeout'], (int, float)) or not math.isfinite(spec['timeout']) or spec['timeout'] <= 0:
        raise ValueError('FAIL scheduled process specification: invalid timeout')
    if not isinstance(spec['deadline'], (int, float)) or not math.isfinite(spec['deadline']):
        raise ValueError('FAIL scheduled process specification: invalid deadline')
    if not spec['argv'] or not all(isinstance(x, str) for x in spec['argv']):
        raise ValueError('FAIL scheduled process specification: invalid argv')


def execute(spec):
    validate(spec)
    argv = list(spec['argv'])
    tool = shutil.which(argv[0], path=spec['env'].get('PATH', os.defpath))
    if tool is None:
        raise FileNotFoundError(argv[0])
    argv[0] = str(Path(tool).resolve())
    text = spec.get('text', False)
    with private_context(spec) as environment, tempfile.TemporaryFile() as source, tempfile.TemporaryFile() as out, tempfile.TemporaryFile() as err:
        payload = spec.get('input')
        if payload is not None:
            source.write(payload.encode() if isinstance(payload, str) else payload)
            source.seek(0)
        capture = spec.get('capture_output', False)
        stdout = spec.get('stdout', subprocess.PIPE if capture else None)
        stderr = spec.get('stderr', subprocess.PIPE if capture else None)
        remaining = min(spec['timeout'], spec['deadline'] - time.monotonic())
        if remaining <= 0:
            raise subprocess.TimeoutExpired(spec['argv'], spec['timeout'])
        outcome = supervise(argv, remaining, spec.get('grace', 10),
                            cwd=spec['cwd'], env=environment, stdin=source,
                            pass_fds=slot_descriptors(environment), umask=0o077,
                            stdout=out if stdout == subprocess.PIPE else stdout,
                            stderr=err if stderr == subprocess.PIPE else stderr)
        if outcome == 'timeout':
            raise subprocess.TimeoutExpired(spec['argv'], spec['timeout'])
        out.seek(0); err.seek(0)
        output = out.read() if stdout == subprocess.PIPE else None
        errors = err.read() if stderr == subprocess.PIPE else None
        if text:
            output = output.decode() if output is not None else None
            errors = errors.decode() if errors is not None else None
        result = subprocess.CompletedProcess(spec['argv'], int(outcome.split()[1]), output, errors)
        if spec.get('check', False):
            result.check_returncode()
        return result


def run(argv, *, cwd, env, timeout=120, role='gate', **options):
    return execute(Invocation.capture(env, cwd, role).specification(argv, timeout=timeout, **options))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--cwd', required=True)
    parser.add_argument('--timeout', type=float, required=True)
    parser.add_argument('--retry', type=float)
    parser.add_argument('--role', choices=('gate', 'publisher'), default='gate')
    parser.add_argument('--status')
    parser.add_argument('argv', nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.argv[1:] if args.argv[:1] == ['--'] else args.argv
    for budget in ([args.timeout, args.retry] if args.retry is not None else [args.timeout]):
        try:
            result = run(command, cwd=args.cwd, env=dict(os.environ), role=args.role, timeout=budget, capture_output=args.retry is not None)
        except subprocess.TimeoutExpired:
            continue
        if args.status: Path(args.status).write_text('done\n')
        if result.stdout is not None: sys.stdout.buffer.write(result.stdout)
        if result.stderr is not None: sys.stderr.buffer.write(result.stderr)
        return result.returncode
    if args.status: Path(args.status).write_text('incomplete\n')
    return 124


if __name__ == '__main__':
    try:
        sys.exit(main())
    except Cancelled as error:
        sys.exit(128 + error.sig)
    except (ValueError, OSError) as error:
        sys.exit(str(error))
