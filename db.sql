-- Table containing all items.
CREATE TABLE Item (
    path STRING NOT NULL,
    name STRING NOT NULL,

    PRIMARY KEY (path, name)
);
-- CREATE INDEX ItemByName ON Item (name);

-- Alias items.
CREATE TABLE AliasItem (
    path STRING NOT NULL,
    name STRING NOT NULL,

    PRIMARY KEY (path, name),
    FOREIGN KEY (path, name) REFERENCES Item (path, name)
        ON UPDATE CASCADE
        ON DELETE CASCADE
);

-- Const items.
CREATE TABLE ConstItem (
    path STRING NOT NULL,
    name STRING NOT NULL,

    -- expr BLOB,

    PRIMARY KEY (path, name),
    FOREIGN KEY (path, name) REFERENCES Item (path, name)
        ON UPDATE CASCADE
        ON DELETE CASCADE
);

-- -- Function items.
-- CREATE TABLE FuncItem ();

-- -- Enum items.
-- CREATE TABLE EnumItem (
--     path STRING NOT NULL,
--     PRIMARY KEY (path),
--     FOREIGN KEY (path) REFERENCES Item (path)
-- ) WITHOUT ROWID;

-- CREATE TABLE EnumVariants (
--     path STRING NOT NULL,
--     name STRING NOT NULL,
--     PRIMARY KEY (path, name),
--     FOREIGN KEY (path) REFERENCES EnumItem (path)
-- ) WITHOUT ROWID;

--
-- Test code
--
INSERT INTO AliasItem (path, name)
VALUES ("my_crate::");

INSERT INTO AliasItem (path, name)
VALUES ("my_crate::");
