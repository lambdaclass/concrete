use std::sync::Arc;

use rusqlite::{params, types::FromSql, Connection, Result, Row};
use serde::Deserialize;

#[derive(Debug, Clone)]
pub struct Repo {
    db: Arc<Connection>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct DbType {
    pub name: String,
    /// JSON array of type names (maybe in the future with trait bounds?)
    pub generics: Option<Vec<String>>,
    pub is_struct: bool,
    pub is_enum: bool,
    pub size: Option<u64>,
    /// JSON array of type names, this can be fields for struct or variants for enum
    pub variants: Option<Vec<String>>,
    pub path: String,
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

    pub fn add_type(&self, ty: &DbType) -> Result<()> {
        let generics = ty
            .generics
            .as_deref()
            .map(serde_json::to_string)
            .transpose()
            .expect("failed to deserialize");
        let variants = ty
            .variants
            .as_deref()
            .map(serde_json::to_string)
            .transpose()
            .expect("failed to deserialize");

        self.db.execute(
            r#"
        INSERT INTO Types (name, generics, is_struct, is_enum, size, variants, path) VALUES
        (?1, ?2, ?3, ?4, ?5, ?6, ?7)
        "#,
            params![
                ty.name,
                generics,
                ty.is_struct,
                ty.is_enum,
                ty.size,
                variants,
                ty.path
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
            let generics: Option<String> = row.get("generics")?;
            let generics: Option<Vec<String>> = generics
                .map(|x| serde_json::from_str(&x))
                .transpose()
                .expect("failed to deserialize");
            let variants: Option<String> = row.get("variants")?;
            let variants: Option<Vec<String>> = variants
                .map(|x| serde_json::from_str(&x))
                .transpose()
                .expect("failed to deserialize");
            Ok(Some(DbType {
                name: row.get("name")?,
                generics,
                is_struct: row.get("is_struct")?,
                is_enum: row.get("is_enum")?,
                size: row.get("size")?,
                variants,
                path: row.get("path")?,
            }))
        } else {
            Ok(None)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{DbType, Repo};

    #[test]
    fn create() {
        let repo = Repo::new().unwrap();

        let ty = DbType {
            name: "A".to_string(),
            path: "mymod".to_string(),
            is_struct: true,
            variants: Some(vec!["i64".to_string(), "i32".to_string()]),
            ..Default::default()
        };

        repo.add_type(&ty).unwrap();

        let ty_query = repo.get_type("A", "mymod").unwrap().unwrap();

        assert_eq!(ty, ty_query);
    }
}
