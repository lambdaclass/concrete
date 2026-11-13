#!/usr/bin/env python3
"""Structural name-independence and numeric/lookup controls, in private copies.

The load-bearing environment scan rejects spelling tables in semantic interfaces.
Semantic fixtures have distinct names and share one environment/closure traversal
with the main check. Shape mutants and the replacement Examples environment stay
separate because they deliberately redefine the same nominal declarations.
Set LOCAL_IDENTITY_PROFILE=1 to report scanner phase timings.
"""
from pathlib import Path
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time

ROOT = Path(__file__).resolve().parents[2]


def run(args, *, env=None):
    started = time.monotonic()
    result = subprocess.run(['bash', 'scripts/checks/lib/bounded.sh', '300', *args],
                            cwd=ROOT, env=env, text=True, capture_output=True)
    if os.environ.get('LOCAL_IDENTITY_PROFILE') == '1' and 'scripts/proofs/local_identity.lean' in args:
        print(f'scan: wall_s={time.monotonic() - started:.3f} ' + ' '.join(args),
              file=sys.stderr, flush=True)
        print(result.stderr, file=sys.stderr, end='', flush=True)
    return result


def require(result, label):
    if result.returncode:
        raise SystemExit(f'{label}: failed\n{result.stdout}{result.stderr}')


def reject(result, label, expected, *, lean=False):
    """Require the intended completed rejection, never an unrelated Lean failure."""
    output = result.stdout + result.stderr
    if lean:
        errors = re.findall(r'^.*?:\d+:\d+: error: (.*)', output, re.MULTILINE)
        valid = len(errors) == 1 and errors[0] == expected
    else:
        valid = expected in result.stderr and not re.search(r'\berror:', output)
    if result.returncode != 1 or not valid:
        raise SystemExit(f'{label}: expected rejection {expected!r}\n{output}')


def main():
    path = run(['lake', 'env', 'printenv', 'LEAN_PATH'])
    require(path, 'Lean search path')
    laws = (ROOT / 'ConcreteSpec/Proofs/LocalIdentity.lean').read_text()
    with tempfile.TemporaryDirectory(prefix='local-identity-') as tmp:
        stage = Path(tmp)
        shutil.copytree(ROOT / ".lake/build/lib/lean/ConcreteSpec", stage / "ConcreteSpec")
        # Lean resolves a package to its first search root as a whole. The batch
        # now imports ConcreteSpec itself, so stage its root artifacts too.
        for artifact in (ROOT / '.lake/build/lib/lean').glob('ConcreteSpec.*'):
            if artifact.is_file():
                shutil.copy2(artifact, stage / artifact.name)
        env = dict(os.environ, LEAN_PATH=tmp + ':' + path.stdout.strip())

        def compile_file(module, source):
            file = stage / (module.replace('.', '/') + '.lean')
            file.parent.mkdir(parents=True, exist_ok=True)
            file.write_text(source)
            return run(['lean', '-DwarningAsError=true', '-DautoImplicit=false',
                        '-R', tmp, '-o', str(file.with_suffix('.olean')), str(file)], env=env)

        scans = [('ConcreteSpec', 'semantic environment scan', None)]
        serial = 0

        def semantic_file(source, *, namespace=None):
            nonlocal serial
            serial += 1
            module = f'ConcreteSpec.Spec.IdentityControl{serial}'
            namespace = namespace or f'IdentityControl{serial}'
            # Imports must precede the namespace; declarations in independent
            # fixtures must never collide when imported into the batch environment.
            lines = source.splitlines(keepends=True)
            imports = ''.join(line for line in lines if line.startswith('import '))
            body = ''.join(line for line in lines if not line.startswith('import '))
            require(compile_file(module, imports + f'namespace {namespace}\n' + body +
                                 f'\nend {namespace}\n'), module + ' builds')
            return module, namespace

        def scan_later(module, namespace, label, expected=None):
            if expected:
                expected = expected.replace('dependency: leak', f'dependency: {namespace}.leak')
                expected = expected.replace('dependency: spellingLength',
                                            f'dependency: {namespace}.spellingLength')
            scans.append((module, label, expected))

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
        reject(result, 'numeric identity mutant',
               'Tactic `rfl` failed: The left-hand side', lean=True)
        if 'case mk.mk.mpr.refl' not in result.stdout or 'hint' not in result.stdout:
            raise SystemExit('numeric identity mutant did not expose unequal spelling fields')
        print('control: numeric_identity rejects name-carrying identity')
        for fields in ['  id : Nat\n  hint : String', '  id : Int']:
            require(compile_file('ShapeControl', 'namespace ConcreteSpec\nstructure Var where\n' + fields + '\nend ConcreteSpec\n'), 'identity shape mutant builds')
            result = run(['lean', '--run', 'scripts/proofs/local_identity.lean', 'ShapeControl'], env=env)
            reject(result, 'scanner mutant', 'exactly one Nat field')
            print('control: constructor scan rejects ' + fields.replace('\n', ';'))
        # The first-binding law must reject searching the tail ahead of the head.
        start = laws.index('theorem first_binding')
        theorem = laws[start:laws.index('\n\n', start)]
        require(compile_file('FirstBaseline', header + theorem + '\nend ConcreteSpec.LocalIdentity\n'),
                'first-binding original proof baseline')
        tail_lookup = '''def lastLookup {α : Type} (l : Core.AList Var α) (x : Var) : Option α :=
  Core.AList.lookup l.reverse x
'''
        require(compile_file('TailWitness', header + tail_lookup +
                'example : lastLookup [(⟨7⟩, 1), (⟨7⟩, 2)] ⟨7⟩ = some 2 := by decide\n' +
                'end ConcreteSpec.LocalIdentity\n'), 'tail-first shadowing witness')
        mutant = header + tail_lookup + theorem.replace(
            'AList.lookup ((x, a) :: rest)', 'lastLookup ((x, a) :: rest)') + '\nend ConcreteSpec.LocalIdentity\n'
        result = compile_file('FirstControl', mutant)
        reject(result, 'tail-first lookup mutant',
               'Tactic `rewrite` failed: Did not find an occurrence of the pattern', lean=True)
        print('control: first_binding rejects tail-first implementation')
        require(compile_file('TableShapeControl', 'namespace ConcreteSpec\nstructure Var where\n  id : Nat\nabbrev LocalNames := List (Var × String)\nend ConcreteSpec\n'), 'table alias mutant builds')
        result = run(['lean', '--run', 'scripts/proofs/local_identity.lean', 'TableShapeControl'], env=env)
        reject(result, 'scanner mutant', 'nominal structure')
        print('control: spelling table must be nominal')
        for declaration, expected in [
            ('structure Name where\n  text : String\n  hidden : Var',
             'declaration Name must have exactly one String field'),
            ('abbrev Name := String', 'missing nominal declaration Name constructor'),
        ]:
            require(compile_file('DeclarationNameControl',
                    'namespace ConcreteSpec\nstructure Var where\n  id : Nat\n'
                    'structure LocalNames where\n  entries : List (Var × String)\n' + declaration +
                    '\nend ConcreteSpec\n'), 'declaration name shape mutant builds')
            result = run(['lean', '--run', 'scripts/proofs/local_identity.lean',
                          'DeclarationNameControl'], env=env)
            reject(result, 'declaration name shape mutant', expected)
            print('control: declaration names must be nominal text only')
        require(compile_file('NumericDataControl',
                'namespace ConcreteSpec\nstructure Var where\n  id : Nat\n'
                'structure LocalNames where\n  entries : List (Var × String)\n'
                'structure Name where\n  text : String\n'
                'abbrev Numbers := List (Var × String)\nend ConcreteSpec\n'),
                'numeric data shape mutant builds')
        result = run(['lean', '--run', 'scripts/proofs/local_identity.lean',
                      'NumericDataControl'], env=env)
        reject(result, 'scanner mutant', 'Numbers must be exactly List Nat')
        print('control: numeric data alias cannot hide a spelling table')
        accepted = {
            'type constructor with declaration names': 'def field (F : Type → Type) (v : F ConcreteSpec.Var) (_name : ConcreteSpec.Name) : F ConcreteSpec.Var := v',
            'type class with numeric data': 'class Family where\n  Carrier : Type → Type\ndef field [f : Family] (v : f.Carrier ConcreteSpec.Var) (_numbers : ConcreteSpec.Numbers) : f.Carrier ConcreteSpec.Var := v',

            'numeric list data': 'def numbers (_ : ConcreteSpec.Var) (_ : ConcreteSpec.Numbers) : Bool := true',
            'variable and declaration name function': 'def field (_ : ConcreteSpec.Var) (_ : ConcreteSpec.Name) : Bool := true',
            'variable and declaration name judgment': 'inductive Field : ConcreteSpec.Var → ConcreteSpec.Name → Prop where\n  | entry : Field ⟨0⟩ ⟨"field"⟩',
            'AST field name and atoms': 'inductive FieldAST where\n  | entry : ConcreteSpec.Name → List ConcreteSpec.Atom → FieldAST',
            'declaration name Binder pattern': 'def pattern (pat : List (ConcreteSpec.Name × ConcreteSpec.Binder)) : Nat := pat.length',
        }
        for label, declaration in accepted.items():
            module, namespace = semantic_file(
                'import ConcreteSpec.Diagnostics.Types\nimport ConcreteSpec.Spec.Slice1Syntax\n' + declaration)
            scan_later(module, namespace, label)
        # Imported helpers must be inspected even when their module is outside Spec.
        helper_header = 'import ConcreteSpec.Diagnostics.Types\nimport ConcreteSpec.Spec.Slice1Syntax\n'
        require(compile_file('ExternalDiagnostics', helper_header +
                'namespace ConcreteSpec.Diagnostics\n'
                'def names : ConcreteSpec.LocalNames := ⟨[]⟩\n'
                'end ConcreteSpec.Diagnostics\n'), 'external diagnostics helper builds')
        for label, helper, semantic, refused in [
            ('external pairing signature',
             'def attach (v : ConcreteSpec.Var) : ConcreteSpec.Var × String := (v, "x")',
             'def spellingLength (v : ConcreteSpec.Var) : Nat := (attach v).2.length', True),
            ('external pairing body',
             'def hiddenLength (v : ConcreteSpec.Var) : Nat := ((v, "x") : ConcreteSpec.Var × String).2.length',
             'def spellingLength (v : ConcreteSpec.Var) : Nat := hiddenLength v', True),
            ('external legitimate helpers',
             'def numericHelper (v : ConcreteSpec.Var) : Nat := v.id + 0\n'
             'theorem numericHelper_eq (v : ConcreteSpec.Var) : numericHelper v = v.id := Nat.add_zero v.id',
             'def numeric (v : ConcreteSpec.Var) : Nat := numericHelper v\n'
             'theorem numeric_eq (v : ConcreteSpec.Var) : numeric v = v.id := numericHelper_eq v', False),
        ]:
            namespace = f'ExternalControl{serial + 1}'
            helper_module = f'ExternalHelpers{serial + 1}'
            imports = helper_header + ('import ExternalDiagnostics\n' if not refused else '')
            require(compile_file(helper_module, imports + f'namespace {namespace}\n' +
                                 helper + f'\nend {namespace}\n'), label + ' helper builds')
            module, namespace = semantic_file('import ' + helper_module + '\n' + semantic,
                                              namespace=namespace)
            scan_later(module, namespace, label,
                       'semantic diagnostics dependency: spellingLength' if refused else None)
        controls = {
            'lambda callback': 'def leak (vs : List ConcreteSpec.Var) : Nat := (vs.map (fun v => toString v.id)).length',
            'let callback': 'def leak (vs : List ConcreteSpec.Var) : Nat := let render := fun (v : ConcreteSpec.Var) => toString v.id; (vs.map render).length',
            'named local helper': 'def leak (vs : List ConcreteSpec.Var) : Nat := (vs.map render).length\nwhere render (v : ConcreteSpec.Var) : String := toString v.id',
            'Option.map callback': 'def leak (v : Option ConcreteSpec.Var) : Nat := (v.map (fun x => toString x.id)).getD "" |>.length',
            'Array.map callback': 'def leak (vs : Array ConcreteSpec.Var) : Nat := (vs.map (fun v => toString v.id)).size',
            'match arm callback': 'def leak (vs : Option (List ConcreteSpec.Var)) : Nat := match vs with | none => 0 | some xs => (xs.map (fun v => toString v.id)).length',
            'instance method callback': 'class Render (α : Type) where\n  render : α → String\ninstance : Render ConcreteSpec.Var where\n  render v := toString v.id\ndef leak (vs : List ConcreteSpec.Var) : Nat := (vs.map Render.render).length',
            'structure projection callback': 'structure Renderer where\n  render : ConcreteSpec.Var → String\ndef leak (vs : List ConcreteSpec.Var) : Nat := (vs.map ((⟨fun v => toString v.id⟩ : Renderer).render)).length',
            'type constructor parameter': 'def leak (F : Type → Type) (v : F ConcreteSpec.Var) (_spelling : String) : F ConcreteSpec.Var := v',
            'type class constructor parameter': 'class Family where\n  Carrier : Type → Type\ndef leak [f : Family] (v : f.Carrier ConcreteSpec.Var) (_spelling : String) : f.Carrier ConcreteSpec.Var := v',
            'universe polymorphic constructor': 'universe u v\ndef leak (F : Type u → Type v) (x : F (ULift.{u} ConcreteSpec.Var)) (_spelling : String) : F (ULift.{u} ConcreteSpec.Var) := x',
            'higher kinded constructor': 'def leak (F : (Type → Type) → Type) (x : F (fun _ => ConcreteSpec.Var)) (_spelling : String) : F (fun _ => ConcreteSpec.Var) := x',
            'type argument inside numeric index': 'inductive Hidden : ConcreteSpec.Numbers → Type where\n  | entry (xs) : Hidden xs\ndef leak (f : Type → ConcreteSpec.Numbers) (x : Hidden (f ConcreteSpec.Var)) (_spelling : String) : Hidden (f ConcreteSpec.Var) := x',
            'erased alias argument': 'abbrev Ignore (_ : Type) := Nat\ndef leak (_x : Ignore ConcreteSpec.Var) (_spelling : String) : Bool := true',

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
            module, namespace = semantic_file(
                'import ConcreteSpec.Diagnostics.Types\nimport ConcreteSpec.Spec.Slice1Syntax\n' + declaration)
            callback_controls = {
                'lambda callback', 'let callback', 'named local helper',
                'Option.map callback', 'Array.map callback', 'match arm callback',
                'instance method callback', 'structure projection callback',
            }
            scan_later(module, namespace, label, 'semantic diagnostics dependency: leak'
                       if label in callback_controls else 'semantic diagnostics dependency:')
        # An explicitly imported refused fixture remains part of the target's
        # protected roots, even when this module's own declaration is clean.
        refused_module, refused_namespace = module, namespace
        module, namespace = semantic_file('import ' + refused_module + '\n' +
            'def leak (v : ConcreteSpec.Var) : Nat := v.id')
        scan_later(module, namespace, 'import closure preserves refusal',
                   f'semantic diagnostics dependency: {refused_namespace}.leak')
        # A clean fixture checked AFTER refused fixtures remains clean in the
        # same imported environment. Its roots exclude unrelated fixture modules.
        module, namespace = semantic_file(helper_header +
            'def leak (v : ConcreteSpec.Var) : Nat := v.id')
        scan_later(module, namespace, 'independent outcome beside refused controls')
        result = run(['lean', '--run', 'scripts/proofs/local_identity.lean', '--batch',
                      *(module for module, _, _ in scans)], env=env)
        require(result, 'batched semantic controls')
        try:
            records = [json.loads(line) for line in result.stdout.splitlines()]
            if [record['module'] for record in records] != [item[0] for item in scans]:
                raise ValueError('missing, reordered or duplicate batch results')
            for record, (_, label, expected) in zip(records, scans):
                names = record['refusals']
                if not isinstance(names, list) or not all(isinstance(n, str) for n in names):
                    raise ValueError('invalid refusal list')
                output = '\n'.join('semantic diagnostics dependency: ' + n for n in names)
                if expected is None:
                    if names:
                        raise SystemExit(f'{label}: unexpected refusal\n{output}')
                elif expected not in output:
                    raise SystemExit(f'{label}: missing specific refusal {expected!r}\n{output}')
                print('control: semantic scan ' + ('accepts ' if expected is None else 'rejects ') + label)
        except (ValueError, KeyError, TypeError) as error:
            raise SystemExit(f'invalid semantic batch results: {error}\n{result.stdout}') from error
        examples = (ROOT / 'ConcreteSpec/Spec/Examples.lean').read_text()
        example_controls = {
            'table': ('unauthorizedTable',
                      'def unauthorizedTable : ConcreteSpec.LocalNames := ⟨[]⟩'),
            'arbitrary payload': ('unauthorizedPayload',
                'def unauthorizedPayload : ConcreteSpec.Elaborated (ConcreteSpec.Var → String) := ⟨(fun _ => "x"), ⟨[]⟩⟩'),
        }
        require(compile_file('ConcreteSpec.Spec.Examples', examples + '\n' +
                '\n'.join(declaration for _, declaration in example_controls.values())),
                'Examples non-exempt controls build')
        result = run(['lean', '--run', 'scripts/proofs/local_identity.lean',
                      'ConcreteSpec.Spec.Examples'], env=env)
        for label, (name, _) in example_controls.items():
            reject(result, 'Examples non-exempt ' + label,
                   'semantic diagnostics dependency: ' + name)
            print('control: Examples exemption rejects ' + label)



if __name__ == '__main__':
    main()
