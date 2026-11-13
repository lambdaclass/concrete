#!/usr/bin/env python3
"""Small production-census regression controls, independent of the historical snapshot."""
from pathlib import Path
import tempfile
from landing_census import census


def check_census(exporter):
    with tempfile.TemporaryDirectory(prefix='landing-controls-') as tmp:
        root = Path(tmp)
        (root/'ConcreteSpec').mkdir()
        (root/'ConcreteSpec.lean').write_text('import ConcreteSpec.ControlConsumer\n')
        (root/'ConcreteSpec/ControlConsumer.lean').write_text('import ConcreteSpec.Control\ntheorem branch_zero : ConcreteSpec.Control.branch 0 = 0 := by simp only [ConcreteSpec.Control.branch]\n')
        probe = root/'ConcreteSpec/Control.lean'
        probe.write_text('''namespace ConcreteSpec.Control
unsafe example : Nat := 3
private example : True := True.intro
noncomputable example : Nat := Classical.choice (show Nonempty Nat from ⟨0⟩)
def dep : Nat := 7
partial def loop (n : Nat) : Nat := if n == 0 then dep else loop (n-1)
def other : Nat := 8
theorem proofA : True := True.intro
theorem proofB : True := by trivial
def branch : Nat → Nat | 0 => 0 | n+1 => n
namespace Decoy
theorem proofA : True := True.intro
def other : Nat := 99
end Decoy
attribute [reducible] dep
/- Instance (law `does.not.exist`, mutant `also.missing`). -/
/-- Instance (law `proofA`, case `one`). -/
example : True := proofA
/-- Planted (law `proofA`, case `one`, mutant `other`). -/
example : other = 8 := rfl
def accepted (n : Nat) : Prop := n = dep
instance instAccepted (n : Nat) : Decidable (accepted n) :=
  inferInstanceAs (Decidable (n = dep))
example (n : Nat) : Bool := @decide (accepted n) (instAccepted n)
end ConcreteSpec.Control
''')
        before = probe.read_bytes()
        data = census(root, exporter,regenerate_modules=('ConcreteSpec.Control',))
        if probe.read_bytes()!=before or list(root.rglob('*.olean')):
            raise ValueError('census isolation control: caller sources/artifacts modified')
        metadata = [r for r in data if r['record']=='resolved-command']
        if not any(r['targets']==['ConcreteSpec.Control.dep'] for r in metadata):
            raise ValueError('reducible attribute target was not resolved by Lean')
        if not any(r['laws']==['ConcreteSpec.Control.proofA'] and r['mutants']==['ConcreteSpec.Control.other'] for r in metadata):
            raise ValueError('namespace-relative law/mutant names were not resolved by Lean')
        print('control PASS: reducible targets and namespace-relative annotations resolved by Lean')
        origins = {r['name']:r for r in data if r['record']=='origin'}
        decls = {r['name']:r for r in data if r['record']=='declaration'}
        if decls['ConcreteSpec.Control.dep']['shape']==decls['ConcreteSpec.Control.other']['shape']:
            raise ValueError('kernel fingerprint erased computational body changes')
        if decls['ConcreteSpec.Control.proofA']['shape']!=decls['ConcreteSpec.Control.proofB']['shape']:
            raise ValueError('kernel fingerprint did not preserve proof irrelevance')
        opaque = 'ConcreteSpec.Control.loop'
        helper = opaque+'._unsafe_rec'
        if origins[opaque]['line']!=6 or origins[opaque]['endLine']!=6:
            raise ValueError('partial definition origin lost its parsed command range')
        if any(origins[opaque][k]!=origins[helper][k] for k in ('file','line','endLine')):
            raise ValueError('partial definition and body helper have different command origins')
        if 'ConcreteSpec.Control.dep' not in decls[helper]['uses']:
            raise ValueError('partial body dependency absent from census')
        examples = {(r['file'],r['line']) for r in data if r['record']=='example'}
        required = {('ConcreteSpec/Control.lean',line) for line in (2,3,4)}
        if not required <= examples:
            raise ValueError('private/noncomputable/unsafe example control missing')
        if not any(r['record']=='origin' and r['file']=='ConcreteSpec/Control.lean' and r['line']==0 and r['name']=='ConcreteSpec.Control.branch.eq_1' for r in data):
            raise ValueError('lazy equation was not regenerated with its defining module')
        print('control PASS: reserved-name equation regeneration observed')
        print('control PASS: private/noncomputable/unsafe examples; opaque body origins; census isolation')
        instance_command = next(r for r in data if r['record']=='command' and r['example']
                                and 'instAccepted' in r['identifiers'])
        instance_example = next(r for r in data if r['record']=='example'
                                and r['line']==instance_command['line']
                                and r['file']==instance_command['file'])
        if 'ConcreteSpec.Control.instAccepted' not in instance_example['uses']:
            raise ValueError('example census replaced a kernel instance dependency with source policy names')
        print('control PASS: example instance constant preserved as an elaborated dependency')

        # Exercise graph traversal before elaboration, through normal source discovery.
        probe.write_text('import ConcreteSpec\n')
        try:
            census(root,exporter)
        except ValueError as exc:
            if not str(exc).startswith('import cycle:'):
                raise
        else:
            raise ValueError('import cycle control accepted')
        print('control PASS: discovered import cycle refused')
