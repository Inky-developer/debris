use std::rc::Rc;

use debris_core::llir::utils::Scoreboard;
use rustc_hash::FxHashMap;

use crate::common::ScoreboardPlayer;

use super::scoreboard_context::ScoreboardContext;

/// Keeps track of used scoreboard constants that are used in the datapack
#[derive(Debug, Default)]
pub(crate) struct ScoreboardConstants {
    constants: FxHashMap<i32, Rc<str>>,
}

impl ScoreboardConstants {
    pub fn name(value: i32) -> Rc<str> {
        format!("const_{}", value).into()
    }

    pub fn get_name(&mut self, value: i32, ctx: &mut ScoreboardContext) -> ScoreboardPlayer {
        let player = self
            .constants
            .entry(value)
            .or_insert_with(|| ScoreboardConstants::name(value))
            .clone();
        ScoreboardPlayer {
            player,
            scoreboard: ctx.get_scoreboard(Scoreboard::Main),
        }
    }

    pub fn constants(&self) -> impl Iterator<Item = i32> + '_ {
        self.constants.keys().copied()
    }
}
