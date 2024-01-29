use std::collections::HashMap;

use crate::{BlockIndex, FnBody, LocalIndex};

pub struct FnBodyBuilder {
    pub body: FnBody,
    pub ret_local: Option<LocalIndex>,
    pub local_map: HashMap<String, LocalIndex>,
    pub current_block: BlockIndex,
}
