use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug, Default, Error, PartialEq)]
pub enum Error {
    #[default]
    #[error("unexpected token")]
    UnexpectedToken,

    #[error("byte literal 0x{0:02x} is out of range (0x00..=0x7F)")]
    ByteOutOfRange(u8),
    #[error("char literal 0x{0:02x} is out of range")]
    CharOutOfRange(u32),
    #[error("sequence '\\{0}' is not a valid escape code")]
    InvalidEscapeSequence(char),
}
