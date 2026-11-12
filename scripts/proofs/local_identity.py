#!/usr/bin/env python3
"""Structural name-independence and numeric/lookup controls, in private copies.

The load-bearing environment scan rejects spelling tables in semantic interfaces.
Controls compile successfully before requiring the scan to accept declaration names
and reject local-spelling mixtures, including nested AST payloads.
"""
from pathlib import Path
import os
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
    laws = (ROOT / 'ConcreteSpec/Proofs/LocalIdentity.lean').read_text()
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

        header = laws[:laws.index('/-- Numeric identity')]
        numeric = laws[laws.index('/-- Numeric identity'):laws.index('/-- The first binding')]
        require(compile_file('NumericControl', header + numeric + '\nend ConcreteSpec.LocalIdentity\n'),
                'numeric identity baseline')
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
        require(compile_file('TableShapeControl', 'namespace ConcreteSpec\nstructure Var where\n  id : Nat\nabbrev LocalNames := List (Var × String)\nend ConcreteSpec\n'), 'table alias mutant builds')
        result = run(['lean', '--run', 'scripts/proofs/local_identity.lean', 'TableShapeControl'], env=env)
        if not result.returncode or 'nominal structure' not in result.stderr:
            raise SystemExit('nominal table scan accepted list alias')
        print('control: spelling table must be nominal')
        for declaration in [
            'structure Name where\n  text : String\n  hidden : Var',
            'abbrev Name := String',
        ]:
            require(compile_file('DeclarationNameControl',
                    'namespace ConcreteSpec\nstructure Var where\n  id : Nat\n'
                    'structure LocalNames where\n  entries : List (Var × String)\n' + declaration +
                    '\nend ConcreteSpec\n'), 'declaration name shape mutant builds')
            result = run(['lean', '--run', 'scripts/proofs/local_identity.lean',
                          'DeclarationNameControl'], env=env)
            if not result.returncode or not any(message in result.stderr for message in
                    ['missing nominal declaration Name constructor',
                     'declaration Name must have exactly one String field']):
                raise SystemExit('declaration name scan accepted broken nominal boundary\n' +
                                 result.stdout + result.stderr)
            print('control: declaration names must be nominal text only')
        require(compile_file('NumericDataControl',
                'namespace ConcreteSpec\nstructure Var where\n  id : Nat\n'
                'structure LocalNames where\n  entries : List (Var × String)\n'
                'structure Name where\n  text : String\n'
                'abbrev Numbers := List (Var × String)\nend ConcreteSpec\n'),
                'numeric data shape mutant builds')
        result = run(['lean', '--run', 'scripts/proofs/local_identity.lean',
                      'NumericDataControl'], env=env)
        if not result.returncode or 'Numbers must be exactly List Nat' not in result.stderr:
            raise SystemExit('numeric data scan accepted a spelling table alias')
        print('control: numeric data alias cannot hide a spelling table')
        accepted = {
            'numeric list data': 'def numbers (_ : ConcreteSpec.Var) (_ : ConcreteSpec.Numbers) : Bool := true',
            'variable and declaration name function': 'def field (_ : ConcreteSpec.Var) (_ : ConcreteSpec.Name) : Bool := true',
            'variable and declaration name judgment': 'inductive Field : ConcreteSpec.Var → ConcreteSpec.Name → Prop where\n  | entry : Field ⟨0⟩ ⟨"field"⟩',
            'AST field name and atoms': 'inductive FieldAST where\n  | entry : ConcreteSpec.Name → List ConcreteSpec.Atom → FieldAST',
            'declaration name Binder pattern': 'def pattern (pat : List (ConcreteSpec.Name × ConcreteSpec.Binder)) : Nat := pat.length',
        }
        for label, declaration in accepted.items():
            require(compile_file('ConcreteSpec.Spec.NameAccepted',
                    'import ConcreteSpec.Diagnostics.Types\nimport ConcreteSpec.Spec.Slice1Syntax\n' + declaration), label + ' builds')
            require(run(['lean', '--run', 'scripts/proofs/local_identity.lean',
                         'ConcreteSpec.Spec.NameAccepted'], env=env), label + ' accepted')
            print('control: semantic scan accepts ' + label)
        controls = {
            'character list spelling': 'def leak : List (ConcreteSpec.Var × List Char) := []',
            'scalar list spelling': 'def leak : List (ConcreteSpec.Var × List Nat) := []',
            'UTF-8 byte list spelling': 'def leak : List (ConcreteSpec.Var × List UInt8) := []',
            'Binder spelling structure': 'structure Hidden where\n  binder : ConcreteSpec.Binder\n  spelling : String\ndef leak (table : List Hidden) : Nat := table.length',
            'Binder spelling pair list': 'def leak (table : List (ConcreteSpec.Binder × String)) : Nat := table.length',
            'AST spelling payload': 'inductive Hidden where\n  | entry : ConcreteSpec.Var → String → Hidden\ndef leak (table : List Hidden) : Nat := table.length',

            'nested carrier fields': 'structure Key where\n  value : ConcreteSpec.Var\n  extra : Nat\nstructure Hidden where\n  key : Key\n  spelling : String',
            'unused split structure': 'structure Hidden where\n  key : ConcreteSpec.Var\n  spelling : String',
            'nested containers': 'def leak : List (List (ConcreteSpec.Var × String)) := []',
            'dependent-pair list': 'def leak (table : List (Sigma (fun _ : ConcreteSpec.Var => String))) : Nat := table.length',
            'dependent-ppair list': 'def leak (table : List (PSigma (fun _ : ConcreteSpec.Var => String))) : Nat := table.length',
            'membership parameter': 'def leak (table : ConcreteSpec.Var → String → Bool) : Bool := table ⟨0⟩ "x"',
            'membership field': 'structure Hidden where\n  member : ConcreteSpec.Var → String → Bool\ndef leak : Hidden := ⟨fun _ _ => true⟩',
            'reducible predicate': 'def Hidden (_ : ConcreteSpec.Var) (_ : String) : Prop := True',
            'empty indexed proposition': 'inductive Hidden : ConcreteSpec.Var → String → Prop',
            'dependent field alias': 'abbrev Text (_ : ConcreteSpec.Var) := String\ndef leak : List (Sigma Text) := []',
            'user matcher name': 'def match_1 (table : ConcreteSpec.Var → String → Bool) : Bool := table ⟨0⟩ "x"',
            'indexed proposition': 'inductive Hidden : ConcreteSpec.Var → String → Prop where\n  | entry : Hidden ⟨0⟩ "x"\ntheorem leak (h : Hidden ⟨0⟩ "x") : Hidden ⟨0⟩ "x" := h',
            'nominal': 'def leak : ConcreteSpec.LocalNames := ⟨[]⟩',
            'nominal alias': 'abbrev Hidden := ConcreteSpec.LocalNames\ndef leak : Hidden := ⟨[]⟩',
            'pair': 'def leak : ConcreteSpec.Var × String := (⟨0⟩, "x")',
            'plain list': 'def leak : List (ConcreteSpec.Var × String) := [(⟨0⟩, "x")]',
            'function': 'def leak : ConcreteSpec.Var → String := fun _ => "x"',
            'function alias': 'abbrev Hidden := ConcreteSpec.Var → String\ndef leak : Hidden := fun _ => "x"',
            'wrapped string': 'structure Text where\n  text : String\nstructure Hidden where\n  key : ConcreteSpec.Var\n  spelling : Text\ndef leak : Hidden := ⟨⟨0⟩, ⟨"x"⟩⟩',
            'function field': 'structure Hidden where\n  display : ConcreteSpec.Var → String\ndef leak : Hidden := ⟨fun _ => "x"⟩',
            'parameterized alias': 'abbrev Hidden (α : Type) := List (ConcreteSpec.Var × α)\ndef leak : Hidden String := []',
            'generic record': 'structure Hidden (κ α : Type) where\n  key : κ\n  spelling : α\ndef leak : Hidden ConcreteSpec.Var String := ⟨⟨0⟩, "x"⟩',
            'generic wrapper': 'structure Box (α : Type) where\n  value : α\nstructure Hidden where\n  key : ConcreteSpec.Var\n  spelling : Box String\ndef leak : Hidden := ⟨⟨0⟩, ⟨"x"⟩⟩',
            'tuple payload': 'def leak : ConcreteSpec.Var × String × Nat := (⟨0⟩, "x", 1)',
            'tuple key': 'def leak : (ConcreteSpec.Var × Nat) × String := ((⟨0⟩, 1), "x")',
            'curried function': 'def leak : ConcreteSpec.Var → Nat → String := fun _ _ => "x"',
            'pair alias': 'abbrev Hidden := List (ConcreteSpec.Var × String)\ndef leak : Hidden := []',
            'structure': 'structure Hidden where\n  key : ConcreteSpec.Var\n  spelling : String\ndef leak : Hidden := ⟨⟨0⟩, "x"⟩',
            'nested structure alias': 'structure Hidden where\n  entries : List (ConcreteSpec.Var × String)\nabbrev Alias := Hidden\ndef leak : Alias := ⟨[]⟩',
        }
        for label, declaration in controls.items():
            require(compile_file('ConcreteSpec.Spec.NameLeak',
                    'import ConcreteSpec.Diagnostics.Types\nimport ConcreteSpec.Spec.Slice1Syntax\n' + declaration), label + ' builds')
            result = run(['lean', '--run', 'scripts/proofs/local_identity.lean',
                          'ConcreteSpec.Spec.NameLeak'], env=env)
            if not result.returncode or 'semantic diagnostics dependency:' not in result.stderr:
                raise SystemExit(label + ': scan accepted spelling table\n' + result.stdout + result.stderr)
            print('control: semantic scan rejects ' + label)
        examples = (ROOT / 'ConcreteSpec/Spec/Examples.lean').read_text()
        for label, declaration in {
            'table': 'def unauthorized : ConcreteSpec.LocalNames := ⟨[]⟩',
            'arbitrary payload': 'def unauthorized : ConcreteSpec.Elaborated (ConcreteSpec.Var → String) := ⟨(fun _ => "x"), ⟨[]⟩⟩',
        }.items():
            require(compile_file('ConcreteSpec.Spec.Examples', examples + '\n' + declaration),
                    'Examples non-exempt ' + label + ' builds')
            result = run(['lean', '--run', 'scripts/proofs/local_identity.lean',
                          'ConcreteSpec.Spec.Examples'], env=env)
            if not result.returncode or 'semantic diagnostics dependency: unauthorized' not in result.stderr:
                raise SystemExit('Examples exemption accepted unauthorized ' + label)
            print('control: Examples exemption rejects ' + label)


if __name__ == '__main__':
    main()
