#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum OptLevel {
    None,       // -O0
    Less,       // -O1
    Default,    // -O2
    Aggressive, // -O3
}

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub enum DebugInfo {
    None,
    Full,
}
