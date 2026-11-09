"""Independent assertions over the production reducer, transport and observer census."""
import itertools
import sys
import urllib.request
from pathlib import Path
import importlib.util


def load(path):
    spec = importlib.util.spec_from_file_location('planted_contract', path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def precedence(owner):
    reducer = owner.reduce_outcomes
    # Independent oracle: presence, never ordering or the production reducer.
    for size in range(1, 5):
        for values in itertools.product(('PASS', 'FAIL', 'INCOMPLETE', 'MISSING'), repeat=size):
            actual = reducer(values)
            expected = next(status for status in ('FAIL', 'INCOMPLETE', 'MISSING', 'PASS') if status in values)
            if actual != expected:
                raise ValueError('FAIL scheduled outcome precedence: FAIL masked')

    diagnostic = 'FAIL scheduled outcome precedence: FAIL masked'
    if owner.execution_status([]) != 'INCOMPLETE':
        raise ValueError(diagnostic)
    for missing, unavailable, nested in itertools.product((False, True), repeat=3):
        gates = [dict(status='PASS' if nested else 'FAIL',
                      controls=[dict(status='FAIL')] if nested else [])]
        if owner.execution_status(gates, missing=missing, unavailable=unavailable) != 'FAIL':
            raise ValueError(diagnostic)


def preserves_failure(status, boundary):
    if status != 'FAIL':
        raise ValueError('FAIL scheduled monotonicity: ' + boundary + ' masked FAIL')


def transport(handler):
    for target in ('https://api.github.com/next', 'https://artifact.example.invalid/result'):
        request = urllib.request.Request('https://api.github.com/start', headers={'Authorization': 'Bearer control-secret'})
        redirected = handler().redirect_request(request, None, 302, 'Found', {}, target)
        if any(key.lower() == 'authorization' for key in dict(redirected.header_items())):
            raise ValueError('FAIL scheduled transport: redirected credential')


def main(argv):
    try:
        if argv[0] == 'precedence':
            precedence(load(argv[1]))
        elif argv[0] == 'transport':
            transport(load(argv[1]).SafeRedirect)
        else:
            raise ValueError('invalid control')
    except ValueError as error:
        print(str(error), file=sys.stderr)
        return 1
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
