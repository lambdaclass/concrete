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
    'BOUNDED_TREE_SOCKET', 'BOUNDED_TREE_KEY', 'BOUNDED_TREE_REQUEST',
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


def clean_env(environment):
    """Shared Git input sanitizer; protocol.clean_env re-exports this owner."""
    return {k: v for k, v in environment.items() if not k.startswith('GIT_')}


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
    def capture(cls, environment=None, cwd=None, role='gate'):
        validate(dict(cwd=cwd, env=environment, argv=['capture'], timeout=1, deadline=1))
        return cls(Path(cwd), role_environment(environment, role), role)

    def specification(self, argv, *, timeout=None, deadline=None, **streams):
        validate(dict(argv=argv, cwd=self.cwd, env=self.environment, timeout=timeout, deadline=deadline if deadline is not None else 1))
        return dict(argv=list(argv), cwd=str(self.cwd), env=dict(self.environment),
                    role=self.role, timeout=timeout,
                    deadline=time.monotonic() + timeout if deadline is None else deadline, **streams)


@contextmanager
def private_context(spec):
    # Ignore ambient TMPDIR, HOME and Git configuration, including config includes.
    with tempfile.TemporaryDirectory(prefix='scheduled-process-', dir='/tmp') as root:
        home = Path(root) / 'home'; home.mkdir(mode=0o700)
        temporary = Path(root) / 'tmp'; temporary.mkdir(mode=0o700)
        environment = role_environment(clean_env(spec['env']), spec.get('role', 'gate'))
        environment.update(HOME=str(home), XDG_CONFIG_HOME=str(home / '.config'),
                           TMPDIR=str(temporary), TMP=str(temporary), TEMP=str(temporary),
                           LC_ALL='C', LANG='C', TZ='UTC',
                           GIT_CONFIG_NOSYSTEM='1', GIT_CONFIG_GLOBAL=os.devnull,
                           GIT_CONFIG_COUNT='1', GIT_CONFIG_KEY_0='core.hooksPath',
                           GIT_CONFIG_VALUE_0=os.devnull)
        if spec.get('gate_outcome'):
            outcome_file = Path(root) / 'outcome.jsonl'; outcome_file.touch()
            environment.update(SCHEDULED_OUTCOME_FILE=str(outcome_file),
                               SCHEDULED_OUTCOME_HELPER=str(Path(__file__).with_name('gate_outcome.py')))
        # Repository-local transport (checkout's scoped extraheader/remote URL) is
        # retained; global/system config and local hooks cannot influence execution.
        yield environment


def validate(spec):
    for field in ('cwd', 'argv', 'env', 'timeout', 'deadline'):
        if field not in spec or spec[field] is None:
            raise ValueError('FAIL scheduled process specification: missing ' + field)
    if not isinstance(spec['cwd'], (str, Path)) or not Path(spec['cwd']).is_absolute() or not Path(spec['cwd']).is_dir():
        raise ValueError('FAIL scheduled process specification: invalid cwd')
    if not isinstance(spec['env'], dict) or not all(isinstance(k, str) and isinstance(v, str) for k, v in spec['env'].items()):
        raise ValueError('FAIL scheduled process specification: invalid env')
    if not isinstance(spec['timeout'], (int, float)) or not math.isfinite(spec['timeout']) or spec['timeout'] <= 0:
        raise ValueError('FAIL scheduled process specification: invalid timeout')
    if not isinstance(spec['deadline'], (int, float)) or not math.isfinite(spec['deadline']):
        raise ValueError('FAIL scheduled process specification: invalid deadline')
    if not isinstance(spec['argv'], (list, tuple)) or not spec['argv'] or not all(isinstance(x, str) for x in spec['argv']):
        raise ValueError('FAIL scheduled process specification: invalid argv')


def execute(spec):
    validate(spec)
    argv = list(spec['argv'])
    # Resolve relative executable/PATH entries against the declared cwd, never
    # against the controller's ambient working directory. Preserve symlink names.
    cwd = Path(spec['cwd'])
    search = os.pathsep.join(str(cwd / entry) if not Path(entry).is_absolute() else entry
                             for entry in spec['env'].get('PATH', os.defpath).split(os.pathsep))
    requested = argv[0]
    if os.path.dirname(requested) and not Path(requested).is_absolute():
        requested = str(cwd / requested)
    tool = shutil.which(requested, path=search)
    if tool is None:
        raise FileNotFoundError(argv[0])
    argv[0] = os.path.abspath(tool)
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
        cancellation = None
        try:
            outcome = supervise(argv, remaining, spec.get('grace', 10),
                                deadline=spec['deadline'], cwd=spec['cwd'], env=environment, stdin=source,
                                pass_fds=slot_descriptors(environment), umask=0o077,
                                stdout=out if stdout == subprocess.PIPE else stdout,
                                stderr=err if stderr == subprocess.PIPE else stderr)
        except Cancelled as error:
            cancellation = error
            outcome = 'cancelled'
        # Read the attempt's evidence before its private context is destroyed.
        # FAIL is final; an INCOMPLETE-only timed-out attempt remains retryable.
        attempt_rc = 0 if outcome in ('timeout', 'cancelled') else int(outcome.split()[1])
        gate_rc = None
        if spec.get('gate_outcome'):
            from gate_outcome import result as gate_result
            gate_rc = gate_result(environment['SCHEDULED_OUTCOME_FILE'], attempt_rc, environment.get('SELECTED_SHA'),
                                  failure_only=outcome in ('timeout', 'cancelled'))
        if cancellation is not None and gate_rc != 1:
            raise cancellation
        if outcome == 'timeout' and gate_rc != 1:
            raise subprocess.TimeoutExpired(spec['argv'], spec['timeout'])
        out.seek(0); err.seek(0)
        output = out.read() if stdout == subprocess.PIPE else None
        errors = err.read() if stderr == subprocess.PIPE else None
        if text:
            output = output.decode() if output is not None else None
            errors = errors.decode() if errors is not None else None
        rc = gate_rc if gate_rc is not None else attempt_rc
        result = subprocess.CompletedProcess(spec['argv'], rc, output, errors)
        if spec.get('check', False):
            result.check_returncode()
        return result


def run(argv, *, cwd=None, env=None, timeout=None, role='gate', **options):
    return execute(Invocation.capture(env, cwd, role).specification(argv, timeout=timeout, **options))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--cwd')
    parser.add_argument('--timeout', type=float)
    parser.add_argument('--deadline', type=float)
    parser.add_argument('--forward-deadline', action='store_true')
    parser.add_argument('--kill-after', type=float, default=10)
    parser.add_argument('--role', choices=('gate', 'publisher'), default='gate')
    parser.add_argument('--status')
    parser.add_argument('--input-file')
    parser.add_argument('--gate-outcome', action='store_true')
    parser.add_argument('argv', nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.argv[1:] if args.argv[:1] == ['--'] else args.argv
    deadline = args.deadline
    timeout = args.timeout
    if timeout is None and deadline is not None:
        timeout = deadline - time.monotonic()
        if timeout <= 0: raise ValueError('INCOMPLETE scheduled suite: deadline exhausted')
    if args.forward_deadline:
        if deadline is None:
            deadline = time.monotonic() + timeout
        command += ['--deadline', str(deadline)]
    try:
        result = run(command, cwd=args.cwd, env=dict(os.environ), role=args.role,
                     timeout=timeout, deadline=deadline, grace=args.kill_after, gate_outcome=args.gate_outcome,
                     input=Path(args.input_file).read_bytes() if args.input_file else None)
        outcome = 'exit ' + str(result.returncode)
    except subprocess.TimeoutExpired:
        outcome = 'timeout'
    if args.status:
        Path(args.status).write_text(outcome + '\n')
        return 0
    return 124 if outcome == 'timeout' else result.returncode



if __name__ == '__main__':
    try:
        sys.exit(main())
    except Cancelled as error:
        sys.exit(128 + error.sig)
    except (ValueError, OSError) as error:
        sys.exit(str(error))
