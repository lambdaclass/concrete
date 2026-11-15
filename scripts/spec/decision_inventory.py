#!/usr/bin/env python3
"""Check the inventory's entry containers independently of authority acceptance."""
from collections import Counter
import re
from markdown_it import MarkdownIt

ANCHOR = re.compile(r'<a\s+id="([^"\n]+)"\s*></a>')


def check_structure(text):
    """Every entry anchor starts its own item, including non-authority entries.

    Navigation anchors before headings and historical table rows are not entries.

    Do not require an approval title: open entries and recovery wrappers are inventory
    entries too. Authority eligibility is a separate, stricter contract in rules.py.
    """
    tokens = MarkdownIt('commonmark').enable('table').parse(text)
    expected = Counter(re.findall(r'<a\s+id="([^"\n]+)"[^>]*>', text))
    categorized = Counter()
    table_depth = 0
    opened = Counter()
    stack = []
    errors = []
    for index, token in enumerate(tokens):
        if token.type == 'table_open':
            table_depth += 1
        elif token.type == 'table_close':
            table_depth -= 1
        if table_depth:
            # Historical quotation anchors count only in parsed table cells.
            if token.type == 'inline':
                categorized.update(ANCHOR.findall(''.join(c.content for c in token.children or []
                    if c.type == 'html_inline')))
            continue
        if token.type == 'list_item_open':
            stack.append({'index': index, 'anchors': [], 'level': token.level})
        elif token.type == 'list_item_close':
            item = stack.pop()
            if len(item['anchors']) > 1:
                errors.append('multiple anchors in one item: ' + ', '.join(item['anchors']))
        elif token.type in ('fence', 'code_block') and not stack:
            errors.append('record fence outside a list item')
        elif token.type == 'inline':
            if not stack and re.search(r'^(?:Provenance:|Owner (?:reply|message)\b)', token.content, re.M):
                errors.append('record field outside a list item')
            anchors = ANCHOR.findall(''.join(child.content for child in token.children or []
                                            if child.type == 'html_inline'))
            # An anchor-only paragraph immediately before a heading is navigation.
            following = tokens[index + 2] if index + 2 < len(tokens) else None
            if (anchors and ANCHOR.fullmatch(token.content.strip()) and not stack
                    and following is not None and following.type == 'heading_open'):
                categorized.update(anchors)
                continue
            if anchors:
                if (not stack or index != stack[-1]['index'] + 2
                        or token.level != stack[-1]['level'] + 2
                        or not ANCHOR.match(token.content)):
                    errors.append('anchor does not open one list item: ' + ', '.join(anchors))
                else:
                    opened.update(anchors)
                    stack[-1]['anchors'].extend(anchors)
        elif token.type == 'html_block' and ANCHOR.search(token.content):
            # An anchor-only block before a heading labels a section.
            anchors = ANCHOR.findall(token.content)
            section = (ANCHOR.fullmatch(token.content.strip()) and index + 1 < len(tokens)
                       and tokens[index + 1].type == 'heading_open')
            if not section:
                errors.append('anchor outside an opening paragraph: ' + ', '.join(anchors))
            else:
                categorized.update(anchors)
    if opened + categorized != expected:
        errors.append('unaccounted explicit anchor: ' + ', '.join(sorted((expected - opened - categorized).elements())))
    if any(n != 1 for n in expected.values()):
        errors.append('duplicate explicit anchor')
    if opened + categorized != expected or any(n != 1 for n in expected.values()):
        errors.append('every explicit anchor must open exactly one unique item')
    return errors
