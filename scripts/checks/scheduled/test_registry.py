"""Generic registry schema and workflow completeness/drift controls."""
import copy
import unittest
import yaml
from protocol import _registry as registry

class Registry(unittest.TestCase):
    def setUp(self):
        self.data = registry.load()
        self.workflow = yaml.safe_load((registry.ROOT / '.github/workflows/lean_action_ci.yml').read_text())

    def rejected(self, diagnostic):
        with self.assertRaises(registry.GateManifestError) as caught:
            registry.check_workflow(self.data, workflow=self.workflow)
        self.assertEqual(str(caught.exception), diagnostic)

    def test_real_inventory(self):
        registry.check_workflow(self.data, workflow=self.workflow)
        # All registered rows, not a hardcoded campaign count.
        for row in self.data['gates']:
            for tier in row['tiers']:
                self.assertIn(row['id'], [r['id'] for r in registry.tier_rows(tier, self.data)])

    def test_removed_workflow_row(self):
        self.data['gates'] = [r for r in self.data['gates'] if r['id'] != 'backends']
        self.rejected('registry: unregistered workflow command: backends/Backend gate (LLVM and LCB, native)')

    def test_removed_root_row(self):
        self.data['gates'] = [r for r in self.data['gates'] if r['id'] != 'root-doc_links']
        self.rejected('registry: missing root gate row: doc_links')

    def test_duplicated_command_in_yaml(self):
        job = self.workflow['jobs']['backends']
        job['steps'].append(copy.deepcopy(job['steps'][-1]))
        self.rejected('registry: duplicate workflow invocation: backends')

    def test_literal_command_in_yaml(self):
        self.workflow['jobs']['backends']['steps'][-1]['run'] = 'bash scripts/checks/compiler/slice0_backends.sh'
        self.rejected('registry: workflow command drift: backends')

    def test_misrouted_tier(self):
        row = next(r for r in self.data['gates'] if r['id'] == 'backends')
        row['tiers'] = ['pr']
        self.rejected('registry: workflow tier mismatch: backends')

    def test_unknown_field(self):
        self.data['gates'][0]['invented'] = 1
        self.rejected('registry: unknown gate field: invented')

    def test_budget_drift(self):
        self.workflow['jobs']['backends']['timeout-minutes'] += 1
        self.rejected('registry: workflow budget drift: backends')

    def test_runner_drift(self):
        self.workflow['jobs']['backends']['runs-on'] = 'ubuntu-latest'
        self.rejected('registry: workflow runner class drift: backends')

    def test_no_gate_specific_tier_assumptions(self):
        row = copy.deepcopy(self.data['gates'][0])
        row.update(id='future-campaign', tiers=['daily','weekly'], runner_class='concrete-ci-2', workflow=None,
                   controls=[dict(id='refusal', command=['false'], exit_code=1, diagnostic='planted')])
        self.data['gates'].append(row)
        for tier in ('daily','weekly'):
            self.assertIn('future-campaign', [r['id'] for r in registry.scheduled_rows(tier, self.data)])

if __name__ == '__main__':
    unittest.main()
