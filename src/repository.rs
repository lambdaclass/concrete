use std::sync::Arc;

use rusqlite::{params, Connection, Result};

#[derive(Debug, Clone)]
pub struct Repo {
    db: Arc<Connection>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct DbType {
    pub name: String,
    pub generics: Option<String>,
    pub is_struct: bool,
    pub is_enum: bool,
    pub size: Option<u64>,
    pub variants: Option<String>,
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
        self.db.execute(
            r#"
        INSERT INTO Types (name, generics, is_struct, is_enum, size, variants, path) VALUES
        (?1, ?2, ?3, ?4, ?5, ?6, ?7)
        "#,
            params![
                ty.name,
                ty.generics,
                ty.is_struct,
                ty.is_enum,
                ty.size,
                ty.variants,
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
            Ok(Some(DbType {
                name: row.get("name")?,
                generics: row.get("generics")?,
                is_struct: row.get("is_struct")?,
                is_enum: row.get("is_enum")?,
                size: row.get("size")?,
                variants: row.get("variants")?,
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
            variants: Some("[i64, i32]".to_string()),
            ..Default::default()
        };

        repo.add_type(&ty).unwrap();

        let ty_query = repo.get_type("A", "mymod").unwrap().unwrap();

        assert_eq!(ty, ty_query);
    }
}
