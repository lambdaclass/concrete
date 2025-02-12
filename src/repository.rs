use std::{path::Path, sync::Arc, time};

use rusqlite::{backup, params, Connection, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone)]
pub struct Repo {
    db: Arc<Connection>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct DbType {
    pub name: String,
    pub generics: Vec<DbTypeName>,
    pub is_struct: bool,
    pub is_enum: bool,
    pub size: Option<u64>,
    pub variants: Vec<DbField>,
    pub path: String,
    pub span_from: Option<u64>,
    pub span_to: Option<u64>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct DbFunction {
    pub name: String,
    pub generics: Vec<DbTypeName>,
    pub is_extern: bool,
    pub is_pub: bool,
    pub params: Vec<String>,
    pub abi: Option<String>,
    pub body: Vec<u8>,
    pub path: String,
    pub span_from: Option<u64>,
    pub span_to: Option<u64>,
}

/// Used in variants/fields (json)
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct DbField {
    pub name: Option<String>,
    pub ty: DbTypeName,
}

/// Used in generics (json)
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct DbTypeName {
    pub name: String,
    pub path: String,
    pub generics: Vec<DbTypeName>,
}

/// Used a function parameter (json)
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct DbParam {
    pub name: String,
    pub ty: DbTypeName,
}

impl Repo {
    pub fn new() -> Result<Self> {
        let s = Self {
            db: Connection::open_in_memory()?.into(),
        };
        s.create_tables()?;
        Ok(s)
    }

    // Creates the db tables.
    fn create_tables(&self) -> Result<()> {
        self.db.execute_batch(include_str!("repo.sql"))?;
        Ok(())
    }

    pub fn save_to<P: AsRef<Path>>(&self, dst: P) -> Result<()> {
        let mut dst = Connection::open(dst)?;
        let backup = backup::Backup::new(&self.db, &mut dst)?;
        backup.run_to_completion(5, time::Duration::from_millis(250), None)
    }

    pub fn add_module(&self, path: &str, is_pub: bool) -> Result<()> {
        self.db.execute(
            r#"
        INSERT INTO Modules (path, is_public) VALUES (?1, ?2) "#,
            params![path, is_pub],
        )?;
        Ok(())
    }

    pub fn add_type(&self, ty: &DbType) -> Result<()> {
        let generics = serde_json::to_string(&ty.generics).expect("failed to deserialize");
        let variants = serde_json::to_string(&ty.variants).expect("failed to deserialize");

        self.db.execute(
            r#"
        INSERT INTO Types (
            name, generics, is_struct,
            is_enum, size, variants,
            path, span_from, span_to) VALUES
        (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)
        "#,
            params![
                ty.name,
                generics,
                ty.is_struct,
                ty.is_enum,
                ty.size,
                variants,
                ty.path,
                ty.span_from,
                ty.span_to
            ],
        )?;

        Ok(())
    }

    pub fn get_type(&self, name: &str, path: &str) -> Result<Option<DbType>> {
        let mut stmt = self.db.prepare_cached(
            r#"
        SELECT * FROM Types WHERE name = ?1 and path = ?2"#,
        )?;

        let mut rows = stmt.query(params![name, path])?;

        if let Some(row) = rows.next()? {
            let generics = serde_json::from_str(&row.get::<_, String>("generics")?)
                .expect("failed to deserialize");
            let variants = serde_json::from_str(&row.get::<_, String>("variants")?)
                .expect("failed to deserialize");

            Ok(Some(DbType {
                name: row.get("name")?,
                generics,
                is_struct: row.get("is_struct")?,
                is_enum: row.get("is_enum")?,
                size: row.get("size")?,
                variants,
                path: row.get("path")?,
                span_from: row.get("span_from")?,
                span_to: row.get("span_to")?,
            }))
        } else {
            Ok(None)
        }
    }

    pub fn add_function(&self, func: &DbFunction) -> Result<()> {
        let generics = serde_json::to_string(&func.generics).expect("failed to deserialize");
        let params = serde_json::to_string(&func.params).expect("failed to deserialize");

        self.db.execute(
            r#"
        INSERT INTO Types (
            name, is_extern, is_pub, params, generics,
            abi, body, path,
            span_from, span_to) VALUES
        (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10)
        "#,
            params![
                func.name,
                func.is_extern,
                func.is_pub,
                params,
                generics,
                func.abi,
                func.body,
                func.path,
                func.span_from,
                func.span_to
            ],
        )?;

        Ok(())
    }

    pub fn get_function(&self, name: &str, path: &str) -> Result<Option<DbFunction>> {
        let mut stmt = self.db.prepare_cached(
            r#"
        SELECT * FROM Functions WHERE name = ?1 and path = ?2"#,
        )?;

        let mut rows = stmt.query(params![name, path])?;

        if let Some(row) = rows.next()? {
            let generics = serde_json::from_str(&row.get::<_, String>("generics")?)
                .expect("failed to deserialize");
            let params = serde_json::from_str(&row.get::<_, String>("params")?)
                .expect("failed to deserialize");

            Ok(Some(DbFunction {
                name: row.get("name")?,
                generics,
                params,
                is_extern: row.get("is_extern")?,
                is_pub: row.get("is_pub")?,
                body: row.get("body")?,
                abi: row.get("abi")?,
                path: row.get("path")?,
                span_from: row.get("span_from")?,
                span_to: row.get("span_to")?,
            }))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::repository::{DbField, DbTypeName};

    use super::{DbType, Repo};

    #[test]
    fn create() {
        let repo = Repo::new().unwrap();

        repo.add_module("mymod", true).unwrap();

        let ty = DbType {
            name: "A".to_string(),
            path: "mymod".to_string(),
            is_struct: true,
            size: Some(64),
            variants: vec![DbField {
                name: Some("a".to_string()),
                ty: DbTypeName {
                    name: "i64".to_string(),
                    path: "std".to_string(),
                    generics: Vec::new(),
                },
            }],
            ..Default::default()
        };

        repo.add_type(&ty).unwrap();

        let ty_query = repo.get_type("A", "mymod").unwrap().unwrap();

        assert_eq!(ty, ty_query);
    }
}
