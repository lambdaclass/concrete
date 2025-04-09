use std::collections::{HashMap, hash_map::Entry};

pub struct LocalScope {
    /// Parent scope (either local or global).
    parent: (),

    items: HashMap<String, ()>,
}

impl LocalScope {
    pub fn insert(&mut self, name: impl Into<String>, value: ()) {
        match self.items.entry(name.into()) {
            Entry::Occupied(_) => panic!(),
            Entry::Vacant(entry) => {
                entry.insert(value);
            }
        }
    }

    pub fn remove(&mut self, name: &str) -> Option<()> {
        self.items.remove(name)
    }
}
