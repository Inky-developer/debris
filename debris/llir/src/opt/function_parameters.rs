use rustc_hash::FxHashMap;

use crate::{block_id::BlockId, item_id::ItemId};

/// Declares how a variable is used by a function
#[derive(Debug, Clone, Copy)]
pub enum FunctionParameter {
    /// Variable is not used at all by this function
    None,
    /// Variable is read by this function
    Read,
    /// Variable is written to by this function
    Write,
    /// Variable is read from and written to by this function
    ReadWrite,
}

impl FunctionParameter {
    pub fn is_read(self) -> bool {
        matches!(self, Self::Read | Self::ReadWrite)
    }

    pub fn is_write(self) -> bool {
        matches!(self, Self::Write | Self::ReadWrite)
    }

    pub fn write(self) -> Self {
        match self {
            FunctionParameter::None | FunctionParameter::Write => FunctionParameter::Write,
            FunctionParameter::ReadWrite | FunctionParameter::Read => FunctionParameter::ReadWrite,
        }
    }

    pub fn read(self) -> Self {
        match self {
            FunctionParameter::None | FunctionParameter::Read => FunctionParameter::Read,
            FunctionParameter::ReadWrite | FunctionParameter::Write => FunctionParameter::ReadWrite,
        }
    }
}

impl Default for FunctionParameter {
    fn default() -> Self {
        FunctionParameter::None
    }
}

/// Stores how functions use variables as parameters.
#[derive(Debug, Default)]
pub struct FunctionParameters {
    parameters: FxHashMap<BlockId, FxHashMap<ItemId, FunctionParameter>>,
}

impl FunctionParameters {
    pub fn dependencies_for(
        &self,
        variable: ItemId,
    ) -> impl Iterator<Item = (BlockId, FunctionParameter)> + '_ {
        self.parameters
            .iter()
            .filter_map(move |(id, func)| func.get(&variable).map(|param| (*id, *param)))
    }

    pub fn is_dependency(&self, variable: ItemId) -> bool {
        self.dependencies_for(variable)
            .any(|(_, dep)| dep.is_read())
    }

    pub fn get(&mut self, function: BlockId, id: ItemId) -> FunctionParameter {
        *self
            .parameters
            .entry(function)
            .or_default()
            .entry(id)
            .or_default()
    }

    pub fn get_function_parameters(
        &self,
        function: BlockId,
    ) -> Option<&FxHashMap<ItemId, FunctionParameter>> {
        self.parameters.get(&function)
    }

    pub fn set_read(&mut self, function: BlockId, id: ItemId) {
        let param = self
            .parameters
            .entry(function)
            .or_default()
            .entry(id)
            .or_default();
        *param = param.read();
    }

    pub fn set_write(&mut self, function: BlockId, id: ItemId) {
        let param = self
            .parameters
            .entry(function)
            .or_default()
            .entry(id)
            .or_default();
        *param = param.write();
    }

    pub fn clear(&mut self) {
        for function in self.parameters.values_mut() {
            function.clear();
        }
    }
}
