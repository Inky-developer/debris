use std::{collections::HashMap, rc::Rc};

use debris_core::llir::utils::{ItemId, Scoreboard};

/// Holds data about specific scoreboard contexts
#[derive(Debug)]
pub(super) struct ScoreboardContext {
    scoreboard_players: HashMap<ScoreboardPlayerId, Rc<str>>,
    pub scoreboards: HashMap<Scoreboard, Rc<str>>,
    scoreboard_prefix: Rc<str>,
}

impl ScoreboardContext {
    /// Creates a new scoreboard context with the default scoreboard name
    pub fn new(scoreboard_prefix: String) -> Self {
        ScoreboardContext {
            scoreboard_prefix: scoreboard_prefix.into(),
            scoreboard_players: Default::default(),
            scoreboards: Default::default(),
        }
    }

    /// Returns the name of this scoreboard
    ///
    /// Internally creates a scoreboard if it did not exist yet
    #[allow(clippy::map_entry)]
    pub fn get_scoreboard(&mut self, scoreboard: Scoreboard) -> Rc<str> {
        if !self.scoreboards.contains_key(&scoreboard) {
            self.scoreboards
                .insert(scoreboard, self.format_scoreboard(scoreboard));
        }
        self.scoreboards.get(&scoreboard).unwrap().clone()
    }

    /// Gets the scoreboard player that corresponds to this `ItemId`
    pub fn get_scoreboard_player(&mut self, item_id: ItemId) -> Rc<str> {
        let num_players = self.scoreboard_players.len() as u64;
        self.scoreboard_players
            .entry(item_id.into())
            .or_insert_with(|| Self::format_player(num_players))
            .clone()
    }

    // /// Makes a new scoreboard player and returns the name
    // pub fn get_temporary_player(&mut self) -> Rc<str> {
    //     let length = self.scoreboard_players.len() as u64;
    //     self.scoreboard_players
    //         .entry(ScoreboardPlayerId::Temporary(length))
    //         .or_insert_with(|| Self::format_player(length))
    //         .clone()
    // }

    fn format_player(id: u64) -> Rc<str> {
        format!("var_{}", id).into()
    }

    fn format_scoreboard(&self, scoreboard: Scoreboard) -> Rc<str> {
        match scoreboard {
            Scoreboard::Main => self.scoreboard_prefix.clone(),
            Scoreboard::Custom(id) => format!("{}.{}", self.scoreboard_prefix, id).into(),
            Scoreboard::Internal(_) => todo!(),
        }
    }
}

/// Used to differentiate between a generated id and a temporary id created by this backend
#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum ScoreboardPlayerId {
    Normal(ItemId),
    // Temporary(u64),
}

impl From<ItemId> for ScoreboardPlayerId {
    fn from(value: ItemId) -> Self {
        ScoreboardPlayerId::Normal(value)
    }
}