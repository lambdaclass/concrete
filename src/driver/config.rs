use std::{collections::HashMap, path::PathBuf};

use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Config {
    pub package: Package,
    pub profile: HashMap<String, Profile>,
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct Package {
    pub name: String,
    pub version: String,
    pub license: String,
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct Profile {
    pub release: bool,
    pub opt_level: u8,
    pub debug_info: bool,
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct Dependency {
    pub path: PathBuf,
    pub version: String,
}
