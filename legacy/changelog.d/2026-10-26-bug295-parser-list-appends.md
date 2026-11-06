### Long Lists Are Parsed in Linear Time (bug 295, 2026-10-26)

The parser built match arms, parameters, call arguments, fields, variants and its other lists by
appending each element to the list built so far, so a list of n elements took time n² to parse (a
`match` of 20000 arms: 2.1 s). Every such list is now collected in an array and converted once; a
20000-arm `match` parses in about 0.1 s and the output is unchanged
([bug 295](docs/bugs/295_parser_list_appends.md)).
