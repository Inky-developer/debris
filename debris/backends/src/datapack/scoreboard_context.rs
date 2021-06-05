use std::rc::Rc;

use debris_core::{
    llir::utils::{ItemId, Scoreboard},
    BuildMode,
};
use rustc_hash::FxHashMap;

use crate::common::ScoreboardPlayer;

/// Holds data about specific scoreboard contexts
#[derive(Debug)]
pub struct ScoreboardContext {
    scoreboard_players: FxHashMap<ScoreboardPlayerId, Rc<str>>,
    pub scoreboards: FxHashMap<Scoreboard, Rc<str>>,
    scoreboard_prefix: Rc<str>,
    player_fmt_string: &'static str,
}

impl ScoreboardContext {
    /// Creates a new scoreboard context with the default scoreboard name
    pub fn new(scoreboard_prefix: String, build_mode: BuildMode) -> Self {
        let player_fmt_string = match build_mode {
            BuildMode::Debug => "var_",
            BuildMode::Release => "#",
        };
        ScoreboardContext {
            scoreboard_prefix: scoreboard_prefix.into(),
            scoreboard_players: Default::default(),
            scoreboards: Default::default(),
            player_fmt_string,
        }
    }

    /// Returns the name of this scoreboard
    ///
    /// Internally creates a scoreboard if it did not exist yet
    /// Clippy reports a false positive here: <https://github.com/rust-lang/rust-clippy/issues/4674>
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
        let player_id = item_id.into();
        match self.scoreboard_players.get(&player_id) {
            Some(scoreboard_player) => Rc::clone(scoreboard_player),
            None => self.add_player(player_id),
        }
    }

    /// Makes a new scoreboard player and returns it as a `ScoreboardPlayer`
    pub fn get_temporary_player(&mut self) -> ScoreboardPlayer {
        let length = self.scoreboard_players.len();
        let player_id = ScoreboardPlayerId::Temporary(length);
        let player = match self.scoreboard_players.get(&player_id) {
            Some(scoreboard_player) => Rc::clone(scoreboard_player),
            None => self.add_player(player_id),
        };
        let scoreboard = self.get_scoreboard(Scoreboard::Main);

        ScoreboardPlayer { player, scoreboard }
    }

    fn add_player(&mut self, player_id: ScoreboardPlayerId) -> Rc<str> {
        let num_players = self.scoreboard_players.len();
        let string = self.format_player(num_players);
        self.scoreboard_players
            .insert(player_id, Rc::clone(&string));
        string
    }

    fn format_player(&self, id: usize) -> Rc<str> {
        format!("{}{}", self.player_fmt_string, id).into()
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
