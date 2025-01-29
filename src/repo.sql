BEGIN;

CREATE TABLE Modules (
    path TEXT NOT NULL,
    is_public BOOLEAN NOT NULL,
    PRIMARY KEY(path)
);

CREATE INDEX idx_module_path ON Modules(path);

CREATE TABLE Types (
    name TEXT NOT NULL,
    generics TEXT NULL, -- format: json array [of names]
    is_struct BOOLEAN NOT NULL DEFAULT FALSE,
    is_enum BOOLEAN NOT NULL DEFAULT FALSE,
    size INT,
    variants TEXT NULL, -- either struct fields or enum variants: json array of names (with generics?)
    path TEXT NOT NULL,
    span_from INT,
    span_to INT,
    PRIMARY KEY(name, path),
    FOREIGN KEY(path) REFERENCES Modules(path)
);

CREATE INDEX idx_types_name ON Types(name);
CREATE INDEX idx_types_path ON Types(path);

CREATE TABLE Functions (
    name TEXT NOT NULL,
    is_extern BOOLEAN NOT NULL,
    is_pub BOOLEAN NOT NULL,
    params TEXT NULL, -- json
    generics TEXT NULL, -- format: json array [of names]
    abi TEXT,
    body BLOB NULL,
    path TEXT NOT NULL,
    span_from INT,
    span_to INT,
    PRIMARY KEY(name, path),
    FOREIGN KEY(path) REFERENCES Modules(path)
);

CREATE INDEX idx_functions_name ON Functions(name);
CREATE INDEX idx_functions_path ON Functions(path);

CREATE TABLE TypeImpl (
    type_name TEXT NOT NULL,
    type_path TEXT NOT NULL,
    fn_name TEXT NOT NULL,
    fn_path TEXT NOT NULL,
    PRIMARY KEY(type_name, type_path, fn_name, fn_path),
    FOREIGN KEY(type_name, type_path) REFERENCES Types(name, path),
    FOREIGN KEY(fn_name, fn_path) REFERENCES Functions(name, path)
);

INSERT INTO Modules (path, is_public) VALUES ('std', true);

INSERT INTO Types (name, size, path) VALUES ('bool', 1, "std");
INSERT INTO Types (name, size, path) VALUES ('char', 32, "std");
INSERT INTO Types (name, size, path) VALUES ('u8', 8, "std");
INSERT INTO Types (name, size, path) VALUES ('u16', 16, "std");
INSERT INTO Types (name, size, path) VALUES ('u32', 32, "std");
INSERT INTO Types (name, size , path) VALUES ('u64', 64, "std");
INSERT INTO Types (name, size, path) VALUES ('u128', 128, "std");
INSERT INTO Types (name, size, path) VALUES ('i8', 8, "std");
INSERT INTO Types (name, size, path) VALUES ('i16', 16, "std");
INSERT INTO Types (name, size, path) VALUES ('i32', 32, "std");
INSERT INTO Types (name, size , path) VALUES ('i64', 64, "std");
INSERT INTO Types (name, size, path) VALUES ('i128', 128, "std");

INSERT INTO Types (name, size, path) VALUES ('f32', 32, "std");
INSERT INTO Types (name, size, path) VALUES ('f64', 64, "std");

COMMIT;
