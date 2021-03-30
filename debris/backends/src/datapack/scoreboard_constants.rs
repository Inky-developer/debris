use std::{collections::HashMap, rc::Rc};

use crate::common::ScoreboardPlayer;

/// Keeps track of used scoreboard constants that are used in the datapack
#[derive(Debug)]
pub(crate) struct ScoreboardConstants {
    scoreboard: Rc<str>,
    constants: HashMap<i32, Rc<str>>,
}

impl ScoreboardConstants {
    pub fn new(scoreboard: Rc<str>) -> Self {
        ScoreboardConstants {
            scoreboard,
            constants: Default::default(),
        }
    }

    pub fn name(value: i32) -> Rc<str> {
        format!("const_{}", value).into()
    }

    pub fn get_name(&mut self, value: i32) -> ScoreboardPlayer {
        let player = self
            .constants
            .entry(value)
            .or_insert_with(|| ScoreboardConstants::name(value))
            .clone();
        ScoreboardPlayer {
            player,
            scoreboard: self.scoreboard.clone(),
        }
    }

    pub fn constants(&self) -> impl Iterator<Item = i32> + '_ {
        self.constants.keys().copied()
    }
}
