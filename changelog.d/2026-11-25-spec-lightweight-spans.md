Source spans carry a project-scoped file index and numeric byte coordinates.
The file table owns the source bytes and builds the coordinate index once.
The surface lexer and source locations share the CR, LF, and CRLF coordinate
rule, with kernel-checked agreement at decoded byte boundaries.
