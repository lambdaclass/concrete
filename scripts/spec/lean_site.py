#!/usr/bin/env python3
"""Build the browsable owner view of the Lean specification (ConcreteSpec) as static HTML.

Usage: lean_site.py DECLS.jsonl OUT_DIR

DECLS.jsonl comes from `lake env lean scripts/spec/lean_site.lean`. Every page is generated from the
Lean sources and the elaborated environment: the module files themselves (doc comments render as
prose, everything else as code), and for each declaration its statement, what it uses and which
proofs use it. Nothing here is hand-written prose, so the view cannot drift from the sources. The
script fails if a generated link has no target or a specification module is missing.
"""
import html, json, os, re, sys
from collections import defaultdict

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def esc(s):
    return html.escape(s, quote=True)


def inline(s):
    s = esc(s)
    s = re.sub(r'`([^`]+)`', r'<code>\1</code>', s)
    s = re.sub(r'\*\*([^*]+)\*\*', r'<strong>\1</strong>', s)
    return s


def prose(text):
    """Minimal Markdown for doc comments: headings, paragraphs, lists, fenced code."""
    out, para, lst, fence = [], [], [], None
    def flush():
        nonlocal para, lst
        if para:
            out.append('<p>' + inline(' '.join(para)) + '</p>')
            para = []
        if lst:
            out.append('<ul>' + ''.join('<li>' + inline(i) + '</li>' for i in lst) + '</ul>')
            lst = []
    for line in text.split('\n'):
        st = line.strip()
        if fence is not None:
            if st.startswith('```'):
                out.append('<pre class="src">' + esc('\n'.join(fence)) + '</pre>')
                fence = None
            else:
                fence.append(line)
            continue
        if st.startswith('```'):
            flush(); fence = []
            continue
        m = re.match(r'(#+)\s+(.*)', st)
        if m:
            flush(); n = min(len(m.group(1)) + 1, 6)
            out.append(f'<h{n}>{inline(m.group(2))}</h{n}>')
        elif re.match(r'[*-]\s+', st) or re.match(r'\d+\.\s+', st):
            if para:
                flush()
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
        out.append('<pre class="src">' + esc('\n'.join(fence)) + '</pre>')
    flush()
    return '\n'.join(out)


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
    (or it is an explicit `instance`). Generated ones carry their parent's range."""
    if d['selEndLine'] != d['line']:
        return False
    t = source(d['module'])[d['line'] - 1][d['selCol']:d['selEndCol']]
    last = d['name'].split('.')[-1]
    return t == last or t.endswith('.' + last) or t == 'instance'


def blocks(lines):
    """Split a Lean source into ('doc', text, line) and ('code', [(line, text)]) blocks. A doc
    block is a `/--` or `/-!` comment, with nested `/- -/` comments respected; text after its
    closing `-/` on the same line stays code."""
    out, code = [], []
    i = 0
    while i < len(lines):
        line = lines[i]; st = line.lstrip()
        if st.startswith('/--') or st.startswith('/-!'):
            if code:
                out.append(('code', code)); code = []
            col = len(line) - len(st) + 3
            depth, buf, j, k = 1, [], i, col
            while True:
                text = lines[j]
                while k < len(text):
                    if text.startswith('/-', k):
                        depth += 1; k += 2; continue
                    if text.startswith('-/', k):
                        depth -= 1
                        if depth == 0:
                            break
                        k += 2; continue
                    k += 1
                if depth == 0:
                    buf.append(text[(col if j == i else 0):k])
                    break
                buf.append(text[(col if j == i else 0):])
                j += 1; k = 0
                if j >= len(lines):
                    sys.exit(f'error: unterminated doc comment at line {i + 1}')
            out.append(('doc', '\n'.join(buf).strip(' '), i + 1))
            rest = lines[j][k + 2:]
            if rest.strip():
                code.append((j + 1, ' ' * (k + 2) + rest))
            i = j + 1
            continue
        code.append((i + 1, line))
        i += 1
    if code:
        out.append(('code', code))
    return out


def main():
    decls_path, out_dir = sys.argv[1], sys.argv[2]
    decls, mdocs = {}, defaultdict(list)
    for line in open(decls_path):
        line = line.strip()
        if not line.startswith('{'):
            continue
        d = json.loads(line)
        if d['kind'] == 'moduleDoc':
            mdocs[d['module']].append(d)
        elif d['kind'] not in ('constructor', 'recursor') and written(d):
            decls[d['name']] = d
    # module order: Spec.lean's imports, then the proofs
    spec_root = open(os.path.join(ROOT, 'ConcreteSpec/Spec.lean')).read()
    spec_mods = ['ConcreteSpec.Spec'] + re.findall(r'^import (ConcreteSpec\.Spec\.\w+)', spec_root, re.M)
    proof_mods = sorted({d['module'] for d in decls.values() if d['module'].startswith('ConcreteSpec.Proofs.')})
    on_disk = sorted('ConcreteSpec.Spec.' + f[:-5] for f in os.listdir(os.path.join(ROOT, 'ConcreteSpec/Spec')) if f.endswith('.lean'))
    missing = sorted(set(on_disk) - set(spec_mods))
    if missing:
        sys.exit(f'error: spec modules not imported by ConcreteSpec/Spec.lean: {missing}')
    mods = spec_mods + proof_mods

    def href(name, from_depth=1):
        d = decls[name]
        return '../' * from_depth + 'm/' + d['module'] + '.html#' + name

    used_by = defaultdict(set)
    for n, d in decls.items():
        for u in d['uses']:
            if u in decls:
                used_by[u].add(n)
    # unique short names, for linking identifiers inside code
    short = defaultdict(set)
    for n in decls:
        parts = n.split('.')
        for i in range(1, len(parts)):
            short['.'.join(parts[i:])].add(n)
    by_line = defaultdict(dict)
    for n, d in decls.items():
        by_line[d['module']].setdefault(d['line'], []).append(n)

    ident = re.compile(r"[A-Za-z_Ͱ-Ͽ][A-Za-z0-9_'!?Ͱ-Ͽ₀-₉.]*")
    def link_code(text, here):
        def rep(m):
            t = m.group(0)
            c = short.get(t)
            if c and len(c) == 1:
                (n,) = c
                return f'<a href="{href(n)}">{esc(t)}</a>'
            return esc(t)
        out, last = [], 0
        for m in ident.finditer(text):
            out.append(esc(text[last:m.start()])); out.append(rep(m)); last = m.end()
        out.append(esc(text[last:]))
        return ''.join(out)

    def meta(n):
        d = decls[n]
        uses = sorted(u for u in d['uses'] if u in decls)
        ub = sorted(used_by[n])
        prf = [u for u in ub if decls[u]['module'].startswith('ConcreteSpec.Proofs.')]
        oth = [u for u in ub if u not in prf]
        parts = [f'<span>{esc(d["kind"])} <code>{esc(n)}</code></span>']
        if uses:
            parts.append('uses: ' + ''.join(f'<a href="{href(u)}">{esc(u.split(".")[-1])}</a>' for u in uses))
        if prf:
            parts.append('proofs about it: ' + ''.join(f'<a href="{href(u)}">{esc(u.split(".")[-1])}</a>' for u in prf))
        if oth:
            parts.append('used by: ' + ''.join(f'<a href="{href(u)}">{esc(u.split(".")[-1])}</a>' for u in oth[:40]))
        return '<div class="meta">' + '<br>'.join(parts) + '</div>'

    os.makedirs(os.path.join(out_dir, 'm'), exist_ok=True)
    counts = {}
    for mod in mods:
        path = os.path.join(ROOT, mod.replace('.', '/') + '.lean')
        src = source(mod)
        anchors = by_line.get(mod, {})
        body = []
        nex = nthm = ndef = 0
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
            nex += sum(1 for _, t in code if t.lstrip().startswith('example'))
            body.append('<pre class="src">' + '\n'.join(link_code(t, mod) for _, t in code) + '</pre>')
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
            m = re.search(r'^#+\s+(.*)$', d['doc'], re.M)
            if m:
                return m.group(1)
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
    for mod in proof_mods + [m for m in spec_mods]:
        ts = sorted((d for d in decls.values() if d['module'] == mod and d['kind'] == 'theorem'
                     and d['doc']), key=lambda d: d['line'])
        if not ts:
            continue
        th.append(f'<h2><a href="m/{esc(mod)}.html">{esc(mod)}</a></h2>')
        for d in ts:
            th.append(f'<div class="decl"><a href="{href(d["name"], 0)}"><code>{esc(d["name"])}</code></a>'
                      f'<div class="prose">{prose(d["doc"])}</div><pre class="src">{esc(d["type"])}</pre></div>')
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
                if frag and f'id="{frag}"' not in open(fp).read():
                    bad.append((p, tgt))
    if bad:
        sys.exit('error: broken links: ' + '; '.join(f'{a} -> {b}' for a, b in bad[:20]))
    print(f'spec site: {len(mods)} modules, {len(decls)} declarations, '
          f'{sum(c[2] for c in counts.values())} examples -> {out_dir}')


if __name__ == '__main__':
    main()
