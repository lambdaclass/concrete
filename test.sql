PRAGMA foreign_keys = ON;

CREATE TABLE TableA (
    path STRING NOT NULL,
    PRIMARY KEY (path),
    FOREIGN KEY (path) REFERENCES TableB (path) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE TableB (
    path STRING NOT NULL,
    PRIMARY KEY (path),
    FOREIGN KEY (path) REFERENCES TableA (path) ON UPDATE CASCADE ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

BEGIN TRANSACTION;

INSERT INTO
    TableA (path)
VALUES
    ("item path");

INSERT INTO
    TableB (path)
VALUES
    ("item path");

COMMIT;

SELECT
    *
from
    TableA;

SELECT
    *
FROM
    TableB;

DELETE FROM TableA;

SELECT
    COUNT(*)
from
    TableA;

SELECT
    COUNT(*)
from
    TableB;
