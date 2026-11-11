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
end ConcreteSpec.Control
''')
        before = probe.read_bytes()
        data = census(root, exporter,regenerate_modules=('ConcreteSpec.Control',))
        if probe.read_bytes()!=before or list(root.rglob('*.olean')):
            raise ValueError('census isolation control: caller sources/artifacts modified')
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
