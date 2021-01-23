use std::{collections::HashMap, rc::Rc};

use debris_core::llir::utils::{ItemId, Scoreboard};

use crate::common::ScoreboardPlayer;

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
    /// Clippy reports a false positive here: https://github.com/rust-lang/rust-clippy/issues/4674
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
        let num_players = self.scoreboard_players.len();
        self.scoreboard_players
            .entry(item_id.into())
            .or_insert_with(|| Self::format_player(num_players))
            .clone()
    }

    /// Makes a new scoreboard player and returns it as a `ScoreboardPlayer`
    pub fn get_temporary_player(&mut self) -> ScoreboardPlayer {
        let length = self.scoreboard_players.len();
        let player = self
            .scoreboard_players
            .entry(ScoreboardPlayerId::Temporary(length))
            .or_insert_with(|| Self::format_player(length))
            .clone();
        let scoreboard = self.get_scoreboard(Scoreboard::Main);

        ScoreboardPlayer { player, scoreboard }
    }

    fn format_player(id: usize) -> Rc<str> {
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
    Temporary(usize),
}

impl From<ItemId> for ScoreboardPlayerId {
    fn from(value: ItemId) -> Self {
        ScoreboardPlayerId::Normal(value)
    }
}
