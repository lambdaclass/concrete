#!/usr/bin/env python3
"""Kernel-checked negative controls for each name-independence law, in private copies.

Every mutation adds an actual dependency on the owning diagnostics table to one dispatch.
It must build, then the corresponding unchanged theorem must fail in its proof (not its header).
No tracked source or build artifact is overwritten. The environment scan also plants both direct
and aliased semantic references, and checks Var's constructor rather than its printed spelling.
"""
from pathlib import Path
import os
import re
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parents[2]


def run(args, *, env=None):
    return subprocess.run(['bash', 'scripts/checks/lib/bounded.sh', '300', *args],
                          cwd=ROOT, env=env, text=True, capture_output=True)


def require(result, label):
    if result.returncode:
        raise SystemExit(f'{label}: failed\n{result.stdout}{result.stderr}')


def main():
    require(run(['lake', 'env', 'lean', '--run', 'scripts/proofs/local_identity.lean',
                 'ConcreteSpec']), 'semantic environment scan')
    path = run(['lake', 'env', 'printenv', 'LEAN_PATH'])
    require(path, 'Lean search path')
    boundary = (ROOT / 'ConcreteSpec/Diagnostics.lean').read_text()
    laws = (ROOT / 'ConcreteSpec/Proofs/LocalIdentity.lean').read_text()
    definitions = list(re.finditer(r'^def (\w+) .*? :=\n  (.*)$', boundary, re.M))
    statements = dict((m[1], m[0]) for m in re.finditer(
        r'^theorem (\w+_names)\b.*?(?=\n\n)', laws, re.M | re.S))
    required = {
        'read', 'statement', 'drop', 'sliceRun',
        'borrowRead', 'borrowWrite', 'borrowMove', 'borrowRestore', 'borrowRun',
        'borrowAssign', 'borrowScope', 'borrowExecute',
        'brandUse', 'brandFree', 'brandRelease', 'brandReset', 'brandGhost',
        'brandErased', 'brandCheck',
    }
    missing = required - {m[1] for m in definitions}
    if missing:
        raise SystemExit('missing operation laws/controls: ' + ', '.join(sorted(missing)))
    expected = {m[1] + '_names' for m in definitions}
    if expected != statements.keys():
        raise SystemExit('dispatches and law controls do not correspond exactly')
    # Concrete counterexamples prove the mutants invalidate the laws, not merely their old proofs.
    slot = '[(⟨0⟩, .val (.scalar (.bool true)))]'
    place = '[(⟨0⟩, { root := ⟨0⟩, mutable := true, owner := true, lock := .free })]'
    memory = '([(⟨0⟩, [7])], [])'
    state = '{ env := [(⟨1⟩, .heapV 0), (⟨0⟩, .allocV 0)], next := 1, gen := [0] }'
    alloc = '{ env := [(⟨0⟩, .allocV 0)], next := 1, gen := [0] }'
    witnesses = {
        'read': (slot, 'read [] STATE ⟨0⟩ false', None),
        'statement': ('[]', 'statement [] STATE ⟨0⟩ (.scalar .bool) (.atom (.bool true))', None),
        'drop': (slot, 'drop [] STATE ⟨0⟩ (.scalar .bool)', None),
        'sliceRun': ('[]', 'sliceRun STATE [] 1 [] (.int .u8 0)', None),
        'borrowRead': (place, 'borrowRead STATE ⟨0⟩ '+place, '.read rfl (by decide) .nil'),
        'borrowWrite': (place, 'borrowWrite STATE ⟨0⟩ '+place, '.write rfl rfl rfl .nil'),
        'borrowMove': (place, 'borrowMove STATE ⟨0⟩ []', '.move rfl rfl rfl .nil'),
        'borrowRestore': (place, 'borrowRestore STATE '+place, None),
        'borrowRun': (memory, 'borrowRun STATE ⟨0⟩ 2 []', None),
        'borrowAssign': (memory, 'borrowAssign STATE ⟨0⟩ 2 0 8', None),
        'borrowScope': (memory, 'borrowScope STATE ⟨0⟩ 3 false ⟨1⟩ 0 1 []', None),
        'borrowExecute': (memory, 'borrowExecute STATE 1 []', None),
        'brandUse': (state, 'brandUse STATE ⟨1⟩ (fun _ => false) 2 ⟨0⟩', None),
        'brandFree': (state, 'brandFree STATE ⟨1⟩ (fun _ => false) 2 ⟨0⟩', None),
        'brandRelease': (alloc, 'brandRelease STATE ⟨0⟩ (fun _ => false) 2', None),
        'brandReset': (alloc, 'brandReset STATE ⟨0⟩ (fun _ => false) 2 ⟨1⟩ 0', None),
        'brandGhost': ('Brand.State.init', 'brandGhost STATE (fun _ => false) 1 0 []', None),
        'brandErased': ('[]', 'brandErased STATE (fun _ => false) 1 0 []', None),
        'brandCheck': ('[]', 'brandCheck STATE [] [] []', '.nil'),
    }
    if witnesses.keys() != {m[1] for m in definitions}:
        raise SystemExit('each operation law needs a concrete mutant counterexample')
    with tempfile.TemporaryDirectory(prefix='local-identity-') as tmp:
        stage = Path(tmp)
        shutil.copytree(ROOT / ".lake/build/lib/lean/ConcreteSpec", stage / "ConcreteSpec")
        env = dict(os.environ, LEAN_PATH=tmp + ':' + path.stdout.strip())

        def compile_file(module, source):
            file = stage / (module.replace('.', '/') + '.lean')
            file.parent.mkdir(parents=True, exist_ok=True)
            file.write_text(source)
            return run(['lean', '-DwarningAsError=true', '-DautoImplicit=false',
                        '-R', tmp, '-o', str(file.with_suffix('.olean')), str(file)], env=env)

        # Compile each unchanged law alone first, ruling out a malformed control or missing import.
        header = laws[:laws.index('/-- Numeric identity')]
        numeric = laws[laws.index('/-- Numeric identity'):laws.index('/-- The first binding')]
        for match in definitions:
            name, body = match[1], match[2]
            theorem = statements[name + '_names']
            test = header + numeric + theorem + '\nend ConcreteSpec.LocalIdentity\n'
            require(compile_file('ConcreteSpec.Diagnostics', boundary), 'boundary baseline')
            require(compile_file('LawControl', test), name + ' positive control')
            # Both branches are well-typed and executable; only the table distinguishes them.
            # Prop-valued checks are refused; data results get an operation-specific stuck/empty.
            fallback = ('False' if ' : Prop :=' in match[0] else
                        '[]' if name == 'borrowRestore' else '.stuck' if name.startswith(('borrow', 'brand'))
                        else '.stuck .unbound')
            replacement = f'if s.names.isEmpty then {body} else {fallback}'
            mutant = boundary[:match.start(2)] + replacement + boundary[match.end(2):]
            require(compile_file('ConcreteSpec.Diagnostics', mutant), name + ' mutant builds')
            semantic, call, accepted = witnesses[name]
            left = call.replace('STATE', '⟨'+semantic+', []⟩')
            right = call.replace('STATE', '⟨'+semantic+', [(⟨0⟩, "renamed")]⟩')
            proof = ('intro h; have hok : '+left+' := '+accepted+'; exact Eq.mp h hok'
                     if accepted else 'intro h; cases h')
            witness = header + 'example : '+left+' ≠ '+right+' := by\n  '+proof+'\nend ConcreteSpec.LocalIdentity\n'
            require(compile_file('Counterexample', witness), name + ' mutant violates law')
            result = compile_file('LawControl', test)
            proof_line = test[:test.index(':= by', test.index('theorem '+name+'_names'))].count('\n') + 1
            errors = re.findall(r'LawControl.lean:(\d+):\d+: error:', result.stdout + result.stderr)
            if not result.returncode or not any(int(line) >= proof_line for line in errors):
                raise SystemExit(f'{name}: unchanged law did not reject the name-dependent dispatch\n'
                                 + result.stdout + result.stderr)
            print(f'control: {name}_names rejects diagnostics-dependent implementation')
        # A field carrying a spelling breaks numeric identity, even when all operations compile.
        mutant = header + '''structure NamedVar where
  id : Nat
  hint : String
  deriving DecidableEq
''' + numeric.replace('(x y : Var)', '(x y : NamedVar)').replace('congrArg Var.id', 'congrArg NamedVar.id') + '\nend ConcreteSpec.LocalIdentity\n'
        result = compile_file('IdentityControl', mutant)
        if not result.returncode or 'error:' not in result.stdout:
            raise SystemExit('numeric_identity accepted a name-carrying variable identity')
        print('control: numeric_identity rejects name-carrying identity')
        for fields in ['  id : Nat\n  hint : String', '  id : Int']:
            require(compile_file('ShapeControl', 'namespace ConcreteSpec\nstructure Var where\n' + fields + '\nend ConcreteSpec\n'), 'identity shape mutant builds')
            result = run(['lean', '--run', 'scripts/proofs/local_identity.lean', 'ShapeControl'], env=env)
            if not result.returncode or 'exactly one Nat field' not in result.stderr:
                raise SystemExit('constructor scan accepted broken Var shape\n' + result.stdout + result.stderr)
            print('control: constructor scan rejects ' + fields.replace('\n', ';'))
        # The first-binding law must reject searching the tail ahead of the head.
        start = laws.index('theorem first_binding')
        theorem = laws[start:laws.index('\n\n', start)]
        mutant = header + '''def lastLookup {α : Type} (l : Core.AList Var α) (x : Var) : Option α :=
  Core.AList.lookup l.reverse x
''' + theorem.replace('AList.lookup ((x, a) :: rest)', 'lastLookup ((x, a) :: rest)').replace('  rw [AList.lookup_cons, ite_eq_left rfl]', '  rfl') + '\nend ConcreteSpec.LocalIdentity\n'
        result = compile_file('FirstControl', mutant)
        if not result.returncode or 'error:' not in result.stdout:
            raise SystemExit('first_binding accepted tail-first lookup')
        print('control: first_binding rejects tail-first implementation')
        require(compile_file('ConcreteSpec.Diagnostics', boundary), 'restore staged boundary')
        for declaration in ['def leak : ConcreteSpec.LocalNames := []',
                            'abbrev Hidden := ConcreteSpec.LocalNames\ndef leak : Hidden := []']:
            require(compile_file('ConcreteSpec.Spec.NameLeak', 'import ConcreteSpec.Spec.Basics\n' + declaration), 'semantic leak builds')
            result = run(['lean', '--run', 'scripts/proofs/local_identity.lean',
                          'ConcreteSpec.Spec.NameLeak'], env=env)
            if not result.returncode or 'semantic diagnostics dependency:' not in result.stderr:
                raise SystemExit('semantic dependency scan accepted a planted name-table reference\n' + result.stdout + result.stderr)
            print('control: semantic scan rejects ' + ('aliased' if 'Hidden' in declaration else 'direct') + ' LocalNames dependency')


if __name__ == '__main__':
    main()
