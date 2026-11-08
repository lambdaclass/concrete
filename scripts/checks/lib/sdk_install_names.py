"""Read install names and re-export edges from the pinned SDK's TAPI YAML.

No host lookup and no inference from stub filenames. Unknown YAML tags, duplicate
keys, unsupported schemas and dangling re-exports are errors, not permissions.
"""
from pathlib import Path
import re
import yaml


class Loader(yaml.CSafeLoader):
    pass


def mapping(loader, node):
    result = {}
    for key, value in node.value:
        key = loader.construct_object(key)
        if not isinstance(key, str) or key in result:
            raise ValueError('duplicate or non-string key')
        result[key] = loader.construct_object(value)
    return result


class Stub(dict):
    def __init__(self, values, version):
        super().__init__(values)
        self.version = version


Loader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, mapping)
Loader.add_constructor('!tapi-tbd', lambda loader, node: Stub(mapping(loader, node), 4))
Loader.add_constructor('!tapi-tbd-v3', lambda loader, node: Stub(mapping(loader, node), 3))


def install_names(sdk, check_path=lambda path: None):
    names, edges = set(), set()
    top = {'tbd-version', 'targets', 'archs', 'platform', 'install-name',
           'current-version', 'compatibility-version', 'swift-abi-version',
           'swift-version', 'objc-constraint', 'flags', 'parent-umbrella',
           'exports', 'reexports', 'undefineds', 'reexported-libraries',
           'allowable-clients', 'uuids'}
    section = {'targets', 'archs', 'symbols', 'weak-symbols', 'weak-def-symbols',
               'weak-ref-symbols', 'thread-local-symbols', 'objc-classes',
               'objc-eh-types', 'objc-ivars', 're-exports', 'libraries',
               'allowable-clients', 'clients', 'umbrella'}

    def strings(value):
        if not isinstance(value, list) or not all(isinstance(x, str) and x for x in value):
            raise ValueError('expected nonempty strings in array')
        return value

    def name(value):
        if not isinstance(value, str) or not value.startswith(('/', '@rpath/')) or any(x in ('.', '..', '') for x in value.split('/')[1:]):
            raise ValueError('invalid install name')
        return value

    def applicable(fields, fallback=None):
        selectors = fields.get('targets', fields.get('archs', fallback))
        selectors = strings(selectors)
        if not selectors:
            raise ValueError('missing targets')
        if 'targets' in fields:
            if any(not re.fullmatch(r'[A-Za-z0-9_]+-[A-Za-z0-9_-]+', v) for v in selectors):
                raise ValueError('invalid target')
            return 'arm64-macos' in selectors
        return 'arm64' in selectors

    files = sorted(Path(sdk).rglob('*.tbd'))
    if not files:
        raise ValueError('SDK has no TAPI stubs')
    for path in files:
        check_path(str(path))
        try:
            documents = list(yaml.load_all(path.read_text(), Loader=Loader))
            if not documents:
                raise ValueError('empty stub')
            for doc in documents:
                if not isinstance(doc, Stub) or set(doc) - top:
                    raise ValueError('unsupported stub fields')
                if doc.version == 4:
                    if type(doc.get('tbd-version')) is not int or doc['tbd-version'] != 4 or 'targets' not in doc or 'archs' in doc or 'platform' in doc:
                        raise ValueError('invalid TAPI v4 header')
                elif 'tbd-version' in doc or 'targets' in doc or 'archs' not in doc:
                    raise ValueError('invalid TAPI v3 header')
                identity = name(doc['install-name'])
                active = applicable(doc)
                if 'targets' not in doc:
                    if doc.get('platform') not in ('macosx', 'ios', 'tvos', 'watchos', 'bridgeos', 'zippered'):
                        raise ValueError('invalid platform')
                    active = active and doc['platform'] in ('macosx', 'zippered')
                for field in ('swift-abi-version', 'swift-version'):
                    if field in doc and (type(doc[field]) is not int or doc[field] < 0):
                        raise ValueError('invalid Swift version')
                if 'flags' in doc:
                    if set(strings(doc['flags'])) - {'flat_namespace', 'not_app_extension_safe', 'installapi'}:
                        raise ValueError('unsupported flags')
                if 'objc-constraint' in doc and doc['objc-constraint'] not in ('none', 'retain_release', 'retain_release_for_simulator', 'retain_release_or_gc', 'gc'):
                    raise ValueError('invalid objc constraint')
                if 'uuids' in doc:
                    if any(not re.fullmatch(r'[A-Za-z0-9_]+: [A-Fa-f0-9-]{36}', v) for v in strings(doc['uuids'])):
                        raise ValueError('invalid UUID')
                for field in ('current-version', 'compatibility-version'):
                    if field in doc and not re.fullmatch(r'[0-9]+(?:\.[0-9]+){0,2}', str(doc[field])):
                        raise ValueError('invalid version')
                if active:
                    names.add(identity)
                if doc.version == 3 and 'parent-umbrella' in doc and not isinstance(doc['parent-umbrella'], str):
                    raise ValueError('invalid umbrella')
                for field in ('exports', 'reexports', 'undefineds', 'reexported-libraries', 'allowable-clients', 'parent-umbrella'):
                    if doc.version == 3 and field == 'parent-umbrella':
                        continue
                    groups = doc.get(field, [])
                    if not isinstance(groups, list):
                        raise ValueError('expected section array')
                    for group in groups:
                        permitted = {
                            'reexported-libraries': {'targets', 'libraries'},
                            'allowable-clients': {'targets', 'clients'},
                            'parent-umbrella': {'targets', 'umbrella'},
                        }.get(field, section - {'libraries', 'clients', 'umbrella'})
                        if not isinstance(group, dict) or set(group) - permitted:
                            raise ValueError('unsupported section fields')
                        selector = 'targets' if doc.version == 4 else 'archs'
                        if selector not in group or ('archs' if selector == 'targets' else 'targets') in group:
                            raise ValueError('missing or conflicting section targets')
                        group_active = applicable(group, doc.get('targets', doc.get('archs')))
                        for key, values in group.items():
                            if key == 'umbrella':
                                if not isinstance(values, str) or not values:
                                    raise ValueError('invalid umbrella')
                                continue
                            strings(values)
                            if key in ('re-exports', 'libraries'):
                                references = [name(v) for v in values]
                                if active and group_active:
                                    edges.update((identity, v) for v in references)
        except (ValueError, KeyError, TypeError, yaml.YAMLError) as error:
            raise ValueError(f'{path}: malformed TAPI stub: {error}') from error
    missing = {target for _, target in edges} - names
    if missing:
        raise ValueError('unresolved SDK re-exports: ' + ', '.join(sorted(missing)))
    return frozenset(names)


def require_install_name(path, names, declared):
    if path not in names:
        raise SystemExit('link purity: install name absent from SDK: ' + path)
    if path not in declared:
        raise SystemExit('link purity: install name not declared for mode: ' + path)
