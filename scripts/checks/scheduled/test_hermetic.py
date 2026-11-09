"""The environment control must detect changed outcomes, not merely two exits."""
import unittest
from hermetic_suite import compare, environments, PREFIXES


class Hermeticity(unittest.TestCase):
    def test_planted_result_drift(self):
        with self.assertRaisesRegex(ValueError, '^FAIL scheduled hermeticity: clean and hostile test results differ$'):
            compare([['planted', 'PASS']], [['planted', 'FAIL']])

    def test_foreign_namespace_inventory(self):
        clean, hostile = environments({'GITHUB_NEW_KEY': 'caller', 'PATH': '/runtime'})
        self.assertEqual(clean, {'PATH': '/runtime'})
        self.assertEqual(hostile['GITHUB_NEW_KEY'], 'foreign/GITHUB_NEW_KEY')
        for key in ('GITHUB_REPOSITORY', 'GITHUB_RUN_ID', 'GITHUB_RUN_ATTEMPT',
                    'SELECTED_SHA', 'SELECTED_TIER', 'SCHEDULED_ATTEMPT_ID',
                    'SCHEDULED_OUTCOME_FILE', 'RUNNER_NAME', 'RUNNER_ENVIRONMENT'):
            self.assertEqual(hostile[key], 'foreign/' + key)
        for prefix in PREFIXES:
            self.assertEqual(hostile[prefix + 'FOREIGN_CONTROL'], 'foreign/' + prefix + 'FOREIGN_CONTROL')
