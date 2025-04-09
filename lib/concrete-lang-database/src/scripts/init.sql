CREATE TABLE AliasItem (
    path TEXT NOT NULL,

    target TEXT NOT NULL,

    PRIMARY KEY (path)
) WITHOUT ROWID, STRICT;

CREATE TABLE ConstItem (
    path TEXT NOT NULL,

    type TEXT NOT NULL,
    value TEXT NOT NULL,

    PRIMARY KEY (path)
) WITHOUT ROWID, STRICT;

-- TODO: CREATE TABLE FuncItem ();
--   - Arguments table (named fields?)

-- TODO: CREATE TABLE EnumItem ();
--   - Variants table

-- TODO: CREATE TABLE StructItem ();
--   - Members table (named fields + unnamed fields table)

-- TODO: CREATE TABLE UnionItem ();
--   - Members table (named fields + unnamed fields table)
