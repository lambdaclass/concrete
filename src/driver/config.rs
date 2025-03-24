use std::{collections::HashMap, path::PathBuf};

use serde::{Deserialize, Serialize};

/// A package config file. Namely Concrete.toml
#[derive(Debug, Serialize, Deserialize)]
pub struct Config {
    pub package: Package,
    pub profile: HashMap<String, Profile>,
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
}

/// Meta information about the package.
#[derive(Debug, Serialize, Deserialize, Default)]
pub struct Package {
    /// The name of the package.
    pub name: String,
    /// The SEMVER compatible version of the package.
    pub version: String,
    /// The SPDX license name.
    pub license: String,
}

/// Defines a compilation profile.
#[derive(Debug, Serialize, Deserialize, Default)]
pub struct Profile {
    /// Whether this profile is the --release profile.
    pub release: bool,
    /// The optimization level.
    pub opt_level: u8,
    /// Whether to enable debug info.
    pub debug_info: bool,
}

/// Defines a package dependency
#[derive(Debug, Serialize, Deserialize, Default)]
pub struct Dependency {
    /// The (relative or absolute) path to the dependency. (Currently only supports paths)
    pub path: PathBuf,
    /// The version of the dependency.
    pub version: String,
}
