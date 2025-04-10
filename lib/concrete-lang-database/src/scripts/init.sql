CREATE TABLE AliasItem (
    path TEXT NOT NULL,

    -- TODO: Visibility, docstring, generics...
    target TEXT NOT NULL,

    PRIMARY KEY (path)
) WITHOUT ROWID, STRICT;


CREATE TABLE ConstItem (
    path TEXT NOT NULL,

    -- TODO: Visibility, docstring...
    type  TEXT NOT NULL,
    value TEXT NOT NULL,

    PRIMARY KEY (path)
) WITHOUT ROWID, STRICT;


CREATE TABLE FuncItem (
    path TEXT NOT NULL,

    -- TODO: Visibility, docstring, generics...
    abi      TEXT,
    extern   INTEGER NOT NULL,
    ret_type TEXT NOT NULL,

    body BLOB,

    PRIMARY KEY (path)
) WITHOUT ROWID, STRICT;

CREATE TABLE FuncArg (
    path TEXT NOT NULL,

    name  TEXT NOT NULL,
    value TEXT NOT NULL,

    PRIMARY KEY (path, name)
) WITHOUT ROWID, STRICT;


CREATE TABLE EnumItem (
    path TEXT NOT NULL,

    -- TODO: Visibility, docstring, generics...

    PRIMARY KEY (path)
) WITHOUT ROWID, STRICT;

CREATE TABLE EnumVariant (
    path TEXT NOT NULL,

    -- Visibility?
    name TEXT NOT NULL,
    type INTEGER NOT NULL,

    PRIMARY KEY (path, name)
) WITHOUT ROWID, STRICT;


CREATE TABLE StructItem (
    path TEXT NOT NULL,

    -- TODO: Visibility, docstring, generics...
    type INTEGER NOT NULL,

    PRIMARY KEY (path)
) WITHOUT ROWID, STRICT;


-- TODO: CREATE TABLE UnionItem ();
--   - Members table (named fields + unnamed fields table)
CREATE TABLE UnionItem (
    -- TODO: Visibility, docstring, generics...
) WITHOUT ROWID, STRICT;


-- TODO: CREATE TABLE TraitItem ();


CREATE TABLE ItemFields (
    path TEXT NOT NULL,

    vis   INTEGER NOT NULL,
    name  TEXT NOT NULL,
    value TEXT NOT NULL,

    PRIMARY KEY (path, name)
) WITHOUT ROWID, STRICT;


-- CREATE VIEW Items ()
-- AS SELECT ...;
