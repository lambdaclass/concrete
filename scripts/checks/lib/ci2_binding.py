#!/usr/bin/env python3
"""One adapter to ISS-0029's authoritative inventory parser, with no fallback."""
import importlib.util
import os
from pathlib import Path
import sys

class BindingError(ValueError):
    pass

def read_inventory(lib=None):
    path = (Path(lib) if lib is not None else Path(__file__).resolve().parent) / 'repro_manifest_compare.py'
    if not path.is_file():
        raise BindingError('INCOMPLETE dependency: ISS-0029 runner binding unavailable')
    spec=importlib.util.spec_from_file_location('iss0029_runner_inventory',path)
    module=importlib.util.module_from_spec(spec); spec.loader.exec_module(module)
    try:
        return module.read_inventory()
    except (SystemExit, OSError, ValueError) as error:
        raise BindingError(f'INCOMPLETE runner inventory: {error}') from error

def verify(repository, environment, name, runners=None):
    if environment != 'self-hosted':
        raise BindingError('forged runner: expected RUNNER_ENVIRONMENT=self-hosted')
    if name not in (read_inventory() if runners is None else runners).get(repository,set()):
        raise BindingError('forged runner: not inventoried concrete-ci-2')

if __name__ == '__main__':
    try:
        verify(os.environ.get('GITHUB_REPOSITORY'),os.environ.get('RUNNER_ENVIRONMENT'),os.environ.get('RUNNER_NAME'))
    except BindingError as error:
        sys.exit(str(error))
