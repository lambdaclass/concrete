use rusqlite::{Connection, Result};

#[derive(Debug)]
pub struct Repo {
    db: Connection,
}

impl Repo {
    pub fn new() -> Result<Self> {
        let mut s = Self {
            db: Connection::open_in_memory()?,
        };
        s.create_tables()?;
        Ok(s)
    }

    fn create_tables(&self) -> Result<()> {
        self.db.execute_batch(include_str!("repo.sql"))?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::Repo;

    #[test]
    fn create() {
        let _repo = Repo::new().unwrap();
    }
}
