use std::{collections::HashMap, rc::Rc};

/// Keeps track of used scoreboard constants that are used in the datapack
#[derive(Debug, Default)]
pub(crate) struct ScoreboardConstants {
    constants: HashMap<i32, Rc<str>>,
}

impl ScoreboardConstants {
    pub fn name(value: i32) -> Rc<str> {
        format!("const_{}", value).into()
    }

    pub fn get_name(&mut self, value: i32) -> Rc<str> {
        self.constants
            .entry(value)
            .or_insert_with(|| ScoreboardConstants::name(value))
            .clone()
    }

    pub fn constants(&self) -> impl Iterator<Item = i32> + '_ {
        self.constants.keys().copied()
    }
}
