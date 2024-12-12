pub use salsa::Database as Db;

#[salsa::db]
#[derive(Default, Clone)]
pub struct DatabaseImpl {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for DatabaseImpl {
    fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
        let event = event();
        eprintln!("Event: {event:?}");
    }
}
