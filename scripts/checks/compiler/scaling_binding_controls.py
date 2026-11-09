import sys
sys.path.insert(0, sys.argv[1])
from ci2_binding import verify, BindingError
runners = {'fixture/repo': {'eligible'}}
verify('fixture/repo', 'self-hosted', 'eligible', runners)
for repo, env, name, diagnostic in (
    ('fixture/repo', 'github-hosted', 'eligible', 'forged runner: expected RUNNER_ENVIRONMENT=self-hosted'),
    ('fixture/other', 'self-hosted', 'eligible', 'forged runner: not inventoried concrete-ci-2'),
    ('fixture/repo', 'self-hosted', 'light-pool', 'forged runner: not inventoried concrete-ci-2'),
    ('fixture/repo', 'self-hosted', 'unregistered', 'forged runner: not inventoried concrete-ci-2'),
):
    try:
        verify(repo, env, name, runners)
    except BindingError as error:
        assert str(error) == diagnostic, (str(error), diagnostic)
    else:
        raise AssertionError('substitute runner accepted')
print('ok scaling control ci2-provenance: eligible runners accepted and substitutes refused')