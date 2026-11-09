"""Scheduled invocation/process specification; lifecycle belongs to bounded_exec.

Step 1 centralizes boundaries and step 2 closes stdin. Context hardening and its
field-by-field controls remain the ordered step 3 obligation in scheduled/README.
"""
import argparse
from dataclasses import dataclass
import os
import math
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

sys.path.insert(0, str(Path(__file__).resolve().parent))
from bounded_exec import supervise, Cancelled


@dataclass(frozen=True)
class Invocation:
    cwd: Path
    environment: dict

    @classmethod
    def capture(cls, environment, cwd):
        return cls(Path(cwd).resolve(), dict(environment))

    def specification(self, argv, *, timeout=120, **streams):
        return dict(argv=list(argv), cwd=str(self.cwd), env=dict(self.environment),
                    timeout=timeout, **streams)


def validate(spec):
    for field in ('cwd', 'argv', 'env', 'timeout'):
        if field not in spec or spec[field] is None:
            raise ValueError('FAIL scheduled process specification: missing ' + field)
    if not Path(spec['cwd']).is_absolute() or not Path(spec['cwd']).is_dir():
        raise ValueError('FAIL scheduled process specification: invalid cwd')
    if not isinstance(spec['env'], dict) or not all(isinstance(k, str) and isinstance(v, str) for k, v in spec['env'].items()):
        raise ValueError('FAIL scheduled process specification: invalid env')
    if not isinstance(spec['timeout'], (int, float)) or not math.isfinite(spec['timeout']) or spec['timeout'] <= 0:
        raise ValueError('FAIL scheduled process specification: invalid timeout')
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
    with tempfile.TemporaryFile() as source, tempfile.TemporaryFile() as out, tempfile.TemporaryFile() as err:
        payload = spec.get('input')
        if payload is not None:
            source.write(payload.encode() if isinstance(payload, str) else payload)
            source.seek(0)
        capture = spec.get('capture_output', False)
        stdout = spec.get('stdout', subprocess.PIPE if capture else None)
        stderr = spec.get('stderr', subprocess.PIPE if capture else None)
        outcome = supervise(argv, spec['timeout'], spec.get('grace', 10),
                            cwd=spec['cwd'], env=spec['env'], stdin=source,
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


def run(argv, *, cwd, env, timeout=120, **options):
    return execute(Invocation.capture(env, cwd).specification(argv, timeout=timeout, **options))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--cwd', required=True)
    parser.add_argument('--timeout', type=float, required=True)
    parser.add_argument('--retry', type=float)
    parser.add_argument('--status')
    parser.add_argument('argv', nargs=argparse.REMAINDER)
    args = parser.parse_args()
    command = args.argv[1:] if args.argv[:1] == ['--'] else args.argv
    for budget in ([args.timeout, args.retry] if args.retry is not None else [args.timeout]):
        try:
            result = run(command, cwd=args.cwd, env=dict(os.environ), timeout=budget, capture_output=args.retry is not None)
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
