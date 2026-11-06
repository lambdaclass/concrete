#!/usr/bin/env python3
"""Build the browsable owner view of the Lean specification (ConcreteSpec) as static HTML.

Usage: lean_site.py DECLS.jsonl OUT_DIR

DECLS.jsonl comes from `lake env lean scripts/spec/lean_site.lean`. Every page is generated from the
Lean sources and the elaborated environment: the module files themselves (doc comments render as
prose, everything else as code), and for each declaration its statement, what it uses and which
proofs use it. Nothing here is hand-written prose, so the view cannot drift from the sources. The
script fails if a generated link has no target or a specification module is missing.
"""
import bisect, html, json, os, re, sys, urllib.parse
from collections import defaultdict

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'proofs'))
import lean_lex  # noqa: E402  (the one Lean lexer, scripts/proofs/lean_lex.py)

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def esc(s):
    return html.escape(s, quote=True)


def inline(s):
    """Inline Markdown: `code` spans first, then **bold** only outside them."""
    out = []
    for k, part in enumerate(re.split(r'(`[^`]+`)', s)):
        if k % 2:
            out.append('<code>' + esc(part[1:-1]) + '</code>')
        else:
            out.append(re.sub(r'\*\*([^*]+)\*\*', r'<strong>\1</strong>', esc(part)))
    return ''.join(out)


FENCE = re.compile(r'(`{3,})')


def prose(text):
    """Minimal Markdown for doc comments: headings, paragraphs, bulleted and numbered lists, and
    fenced code (a fence closes only on a line of at least as many backticks and nothing else)."""
    out, para, lst, fence = [], [], [], None
    kind = None                                       # 'ul' or 'ol' for the open list
    def flush():
        nonlocal para, lst, kind
        if para:
            out.append('<p>' + inline(' '.join(para)) + '</p>')
            para = []
        if lst:
            out.append(f'<{kind}>' + ''.join('<li>' + inline(i) + '</li>' for i in lst) + f'</{kind}>')
            lst = []
    for line in text.split('\n'):
        st = line.strip()
        if fence is not None:
            n, body = fence
            if re.fullmatch('`{%d,}' % n, st):
                out.append('<pre class="src">' + esc('\n'.join(body)) + '</pre>')
                fence = None
            else:
                body.append(line)
            continue
        m = FENCE.match(st)
        if m:
            flush(); fence = (len(m.group(1)), [])
            continue
        m = re.match(r'(#+)\s+(.*)', st)
        bullet, number = re.match(r'[*-]\s+', st), re.match(r'\d+\.\s+', st)
        if m:
            flush(); n = min(len(m.group(1)) + 1, 6)
            out.append(f'<h{n}>{inline(m.group(2))}</h{n}>')
        elif bullet or number:
            k = 'ol' if number else 'ul'
            if para or (lst and kind != k):
                flush()
            kind = k
            lst.append(re.sub(r'^([*-]|\d+\.)\s+', '', st))
        elif not st:
            flush()
        elif lst and line.startswith('  '):
            lst[-1] += ' ' + st
        else:
            if lst:
                flush()
            para.append(st)
    if fence is not None:
        out.append('<pre class="src">' + esc('\n'.join(fence[1])) + '</pre>')
    flush()
    return '\n'.join(out)


def headings(text):
    """The Markdown headings of a doc, outside fenced code."""
    out, fence = [], None
    for line in text.split('\n'):
        st = line.strip()
        if fence is not None:
            if re.fullmatch('`{%d,}' % fence, st):
                fence = None
            continue
        m = FENCE.match(st)
        if m:
            fence = len(m.group(1)); continue
        m = re.match(r'#+\s+(.*)', st)
        if m:
            out.append(m.group(1))
    return out


CSS = """
:root{--bg:#fff;--fg:#1d1d1f;--mute:#666;--code:#f5f5f4;--line:#ddd;--link:#0b5cad}
@media (prefers-color-scheme:dark){:root{--bg:#161616;--fg:#e8e8e8;--mute:#9a9a9a;--code:#222;--line:#333;--link:#7fb4ff}}
body{background:var(--bg);color:var(--fg);font:16px/1.55 system-ui,sans-serif;margin:0}
main{max-width:980px;margin:0 auto;padding:16px}
a{color:var(--link)} nav{font-size:14px;margin-bottom:12px}
pre.src{background:var(--code);padding:10px;overflow-x:auto;border-radius:6px;font:13px/1.45 ui-monospace,monospace}
code{background:var(--code);padding:0 3px;border-radius:3px;font-size:90%}
.decl{border-left:3px solid var(--line);padding-left:10px;margin:4px 0 14px}
.meta{font-size:13px;color:var(--mute)} .meta a{margin-right:6px}
table{border-collapse:collapse} td,th{border-bottom:1px solid var(--line);padding:4px 8px;text-align:left;vertical-align:top}
.prose{max-width:760px}
"""


def page(title, body, depth=0):
    up = '../' * depth
    return (f'<!doctype html><html lang="en"><head><meta charset="utf-8">'
            f'<meta name="viewport" content="width=device-width,initial-scale=1">'
            f'<title>{esc(title)}</title><style>{CSS}</style></head><body><main>'
            f'<nav><a href="{up}index.html">Concrete specification</a> · '
            f'<a href="{up}theorems.html">Theorems</a></nav>{body}</main></body></html>\n')


_src_cache = {}


def source(mod):
    if mod not in _src_cache:
        _src_cache[mod] = open(os.path.join(ROOT, mod.replace('.', '/') + '.lean')).read().split('\n')
    return _src_cache[mod]


def written(d):
    """A declaration the source writes, as opposed to one Lean generates from it (recursors,
    `deriving` instances, equation lemmas, matchers): its name is the text at its selection range
    (or it is an explicit `instance`). Generated ones carry their parent's range. The selection may
    span lines (a quoted name «…» can hold a newline)."""
    lines = source(d['module'])
    if d['selEndLine'] == d['line']:
        t = lines[d['line'] - 1][d['selCol']:d['selEndCol']]
    else:
        t = '\n'.join([lines[d['line'] - 1][d['selCol']:]] + lines[d['line']:d['selEndLine'] - 1]
                      + [lines[d['selEndLine'] - 1][:d['selEndCol']]])
    t = t.replace('«', '').replace('»', '')
    last = d['last']
    return t == last or t.endswith('.' + last) or t == 'instance'


def module_order(env_mods):
    """The modules of the library: the root, the specification in `Spec.lean`'s import order, then
    every other module sorted. Every source file under `ConcreteSpec/` must be in the root's import
    closure, or the view would leave it out."""
    on_disk = set(modules_under('ConcreteSpec')) | {'ConcreteSpec'}
    missing = sorted(on_disk - set(env_mods))
    if missing:
        sys.exit(f'error: source modules outside the import closure of ConcreteSpec: {missing}')
    spec_root = open(os.path.join(ROOT, 'ConcreteSpec/Spec.lean')).read()
    first = ['ConcreteSpec', 'ConcreteSpec.Spec'] + re.findall(r'^import (ConcreteSpec\.Spec\.[\w.]+)', spec_root, re.M)
    first = [m for m in first if m in env_mods]
    return first + sorted(set(env_mods) - set(first))


def blocks(lines):
    """Split a Lean source into ('doc', text, line) and ('code', [(line, text)]) blocks. A doc
    block is a top-level `/--` or `/-!` comment (scripts/proofs/lean_lex.py finds the comments, so
    one inside another comment or in a string is not a doc block) that is the first text on its
    line; text after its closing `-/` on the same line stays code."""
    src = '\n'.join(lines)
    try:
        spans = lean_lex.comment_spans(src)
    except lean_lex.LexError as e:
        sys.exit(f'error: {e}')
    starts = [0]
    for line in lines[:-1]:
        starts.append(starts[-1] + len(line) + 1)
    line_of = lambda off: bisect.bisect_right(starts, off) - 1
    out, code, i = [], [], 0
    for s, e in spans:
        ls, le = line_of(s), line_of(e - 1)
        if not src.startswith(('/--', '/-!'), s) or src[starts[ls]:s].strip() or ls < i:
            continue
        code.extend((k + 1, lines[k]) for k in range(i, ls))
        if code:
            out.append(('code', code)); code = []
        out.append(('doc', src[s + 3:e - 2].strip(' '), ls + 1))
        col = e - starts[le]
        rest = lines[le][col:]
        if rest.strip():
            code.append((le + 1, ' ' * col + rest))
        i = le + 1
    code.extend((k + 1, lines[k]) for k in range(i, len(lines)))
    if code:
        out.append(('code', code))
    return out


def modules_under(rel):
    """Every module whose source is under ROOT/rel, at any depth, sorted."""
    out = []
    for d, _, files in os.walk(os.path.join(ROOT, rel)):
        for f in files:
            if f.endswith('.lean'):
                path = os.path.relpath(os.path.join(d, f), ROOT)[:-5]
                out.append(path.replace(os.sep, '.'))
    return sorted(out)


def main():
    decls_path, out_dir = sys.argv[1], sys.argv[2]
    decls, mdocs, env_mods = {}, defaultdict(list), []
    for line in open(decls_path):
        line = line.strip()
        if not line.startswith('{'):
            continue
        d = json.loads(line)
        if d['kind'] == 'moduleDoc':
            mdocs[d['module']].append(d)
        elif d['kind'] == 'module':
            env_mods.append(d['module'])
        elif d['kind'] != 'recursor' and written(d):
            decls[d['name']] = d
    mods = module_order(env_mods)
    spec_mods = [m for m in mods if m == 'ConcreteSpec.Spec' or m.startswith('ConcreteSpec.Spec.')]

    def href(name, from_depth=1):
        d = decls[name]
        return '../' * from_depth + 'm/' + d['module'] + '.html#' + urllib.parse.quote(name, safe='')

    used_by = defaultdict(set)
    for n, d in decls.items():
        for u in d['uses']:
            if u in decls:
                used_by[u].add(n)
    by_line = defaultdict(dict)
    for n, d in decls.items():
        by_line[d['module']].setdefault(d['line'], []).append(n)

    def ilean_refs(mod):
        """Where the module's source names a documented declaration, from the .ilean file Lean
        writes when it builds the module: line (1-based) → [(start, end, name)], columns in UTF-16
        code units as Lean records them. These are Lean's own name resolutions, so a local
        variable that shadows a declaration's name is never linked."""
        path = os.path.join(ROOT, '.lake/build/lib/lean', mod.replace('.', '/') + '.ilean')
        try:
            refs = json.load(open(path))['references']
        except (OSError, KeyError, ValueError) as e:
            sys.exit(f'error: no references for {mod} ({e}); build the library first')
        out = defaultdict(list)
        for key, r in refs.items():
            name = json.loads(key).get('c', {}).get('n')
            if name not in decls:
                continue
            for u in r.get('usages', []) + ([r['definition']] if r.get('definition') else []):
                if u[0] == u[2]:
                    out[u[0] + 1].append((u[1], u[3], name))
        return out

    def utf16_index(text, col):
        units = 0
        for i, ch in enumerate(text):
            if units >= col:
                return i
            units += 2 if ord(ch) > 0xFFFF else 1
        return len(text)

    def link_code(ln, text, refs):
        out, last = [], 0
        for a, b, name in sorted(refs.get(ln, [])):
            i, j = utf16_index(text, a), utf16_index(text, b)
            if i < last or j > len(text):
                continue
            out.append(esc(text[last:i])); out.append(f'<a href="{href(name)}">{esc(text[i:j])}</a>'); last = j
        out.append(esc(text[last:]))
        return ''.join(out)

    def short(u):
        return esc(decls[u]['last'] or decls[u]['display'])

    def meta(n):
        d = decls[n]
        uses = sorted(u for u in d['uses'] if u in decls)
        ub = sorted(used_by[n])
        prf = [u for u in ub if decls[u]['kind'] == 'theorem']
        oth = [u for u in ub if u not in prf]
        priv = 'private ' if d['private'] else ''
        parts = [f'<span>{priv}{esc(d["kind"])} <code>{esc(d["display"])}</code></span>']
        if uses:
            parts.append('uses: ' + ''.join(f'<a href="{href(u)}">{short(u)}</a>' for u in uses))
        if prf:
            parts.append('theorems using it: ' + ''.join(f'<a href="{href(u)}">{short(u)}</a>' for u in prf))
        if oth:
            parts.append('used by: ' + ''.join(f'<a href="{href(u)}">{short(u)}</a>' for u in oth))
        return '<div class="meta">' + '<br>'.join(parts) + '</div>'

    os.makedirs(os.path.join(out_dir, 'm'), exist_ok=True)
    counts = {}
    for mod in mods:
        path = os.path.join(ROOT, mod.replace('.', '/') + '.lean')
        src = source(mod)
        anchors = by_line.get(mod, {})
        refs = ilean_refs(mod)
        body = []
        nthm = ndef = 0
        nex = lean_lex.count_examples('\n'.join(src))
        for blk in blocks(src):
            if blk[0] == 'doc':
                body.append('<div class="prose">' + prose(blk[1]) + '</div>')
                continue
            code = blk[1]
            while code and not code[-1][1].strip():
                code.pop()
            while code and not code[0][1].strip():
                code.pop(0)
            if not code:
                continue
            body.append('<pre class="src">' + '\n'.join(link_code(ln, t, refs) for ln, t in code) + '</pre>')
            for ln, _ in code:
                for n in anchors.get(ln, []):
                    body.append(f'<div class="decl" id="{esc(n)}">' + meta(n) + '</div>')
        for n, d in decls.items():
            if d['module'] == mod:
                nthm += d['kind'] == 'theorem'; ndef += d['kind'] != 'theorem'
        counts[mod] = (ndef, nthm, nex)
        rel = os.path.relpath(path, ROOT)
        head = f'<h1>{esc(mod)}</h1><p class="meta">source: <code>{esc(rel)}</code></p>'
        open(os.path.join(out_dir, 'm', mod + '.html'), 'w').write(page(mod, head + '\n'.join(body), 1))

    # index: modules with their first module-doc heading
    def title(mod):
        for d in sorted(mdocs.get(mod, []), key=lambda d: d['line']):
            hs = headings(d['doc'])
            if hs:
                return hs[0]
        return ''
    rows = ''.join(f'<tr><td><a href="m/{esc(m)}.html">{esc(m)}</a></td><td>{inline(title(m))}</td>'
                   f'<td>{counts[m][0]}</td><td>{counts[m][1]}</td><td>{counts[m][2]}</td></tr>' for m in mods)
    top = ''
    for d in sorted(mdocs.get('ConcreteSpec.Spec', []), key=lambda d: d['line']):
        top += '<div class="prose">' + prose(d['doc']) + '</div>'
    idx = (top + '<h2>Modules</h2><table><tr><th>module</th><th>title</th><th>definitions</th>'
           '<th>theorems</th><th>examples</th></tr>' + rows + '</table>')
    open(os.path.join(out_dir, 'index.html'), 'w').write(page('Concrete specification', idx))
    # theorems page
    th = ['<h1>Theorems</h1>']
    for mod in mods:
        ts = sorted((d for d in decls.values() if d['module'] == mod and d['kind'] == 'theorem'),
                    key=lambda d: d['line'])
        if not ts:
            continue
        th.append(f'<h2><a href="m/{esc(mod)}.html">{esc(mod)}</a></h2>')
        for d in ts:
            doc = f'<div class="prose">{prose(d["doc"])}</div>' if d['doc'] else ''
            th.append(f'<div class="decl"><a href="{href(d["name"], 0)}"><code>{esc(d["display"])}</code></a>'
                      f'{doc}<pre class="src">{esc(d["type"])}</pre></div>')
    open(os.path.join(out_dir, 'theorems.html'), 'w').write(page('Theorems', '\n'.join(th)))
    # link check
    bad = []
    for root, _, files in os.walk(out_dir):
        for f in files:
            if not f.endswith('.html'):
                continue
            p = os.path.join(root, f); s = open(p).read()
            for tgt in re.findall(r'href="([^"]+)"', s):
                file, _, frag = tgt.partition('#')
                fp = os.path.normpath(os.path.join(root, file)) if file else p
                if not os.path.exists(fp):
                    bad.append((p, tgt)); continue
                if frag and f'id="{esc(urllib.parse.unquote(frag))}"' not in open(fp).read():
                    bad.append((p, tgt))
    if bad:
        sys.exit('error: broken links: ' + '; '.join(f'{a} -> {b}' for a, b in bad[:20]))
    print(f'spec site: {len(mods)} modules, {len(decls)} declarations, '
          f'{sum(c[2] for c in counts.values())} examples -> {out_dir}')


if __name__ == '__main__':
    main()
