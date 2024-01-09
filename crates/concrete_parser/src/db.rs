#[salsa::jar(db = Db)]
pub struct Jar(crate::parse_ast, crate::ProgramSource);

pub trait Db
where
    Self: salsa::DbWithJar<Jar>,
{
}

impl<T> Db for T where T: ?Sized + salsa::DbWithJar<Jar> {}
