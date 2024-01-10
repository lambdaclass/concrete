#[salsa::jar(db = Db)]
pub struct Jar(
    // TODO: Add stuff.
);

pub trait Db
where
    Self: salsa::DbWithJar<Jar> + salsa::DbWithJar<concrete_parser::db::Jar>,
{
}

impl<T> Db for T where T: ?Sized + salsa::DbWithJar<Jar> + salsa::DbWithJar<concrete_parser::db::Jar>
{}

#[derive(Default)]
#[salsa::db(Jar, concrete_parser::db::Jar)]
pub(crate) struct Database {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {}

impl salsa::ParallelDatabase for Database {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        salsa::Snapshot::new(Self {
            storage: self.storage.snapshot(),
        })
    }
}
