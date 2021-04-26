use rustc_hash::FxHashMap;

use crate::llir::utils::{BlockId, ItemId};

/// Declares how a variable is used by a function
#[derive(Debug, Clone, Copy)]
pub enum FunctionParameter {
    /// Variable is not used at all by this function
    None,
    /// Variable is read by this function
    Read,
    /// Variable is written to by this function before it is read
    Write,
}

impl FunctionParameter {
    /// Returns `true` if the function_parameter is [`Read`].
    pub fn is_read(&self) -> bool {
        matches!(self, Self::Read)
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
            .filter_map(move |(id, funct)| match funct.get(&variable) {
                Some(param) => Some((*id, *param)),
                None => None,
            })
    }

    pub fn is_dependency(&self, variable: ItemId) -> bool {
        self.dependencies_for(variable)
            .any(|(_, dep)| dep.is_read())
    }

    // pub fn get(&self, function: &BlockId, id: &ItemId) -> FunctionParameter {
    //     self.parameters[function]
    //         .get(id)
    //         .copied()
    //         .unwrap_or_default()
    // }

    pub fn set_read(&mut self, function: BlockId, id: ItemId) {
        self.parameters
            .entry(function)
            .or_default()
            .insert(id, FunctionParameter::Read);
    }

    pub fn set_read_weak(&mut self, function: BlockId, id: ItemId) {
        match self
            .parameters
            .entry(function)
            .or_default()
            .entry(id)
            .or_default()
        {
            FunctionParameter::Write => {}
            FunctionParameter::Read => {}
            param @ FunctionParameter::None => *param = FunctionParameter::Read,
        };
    }

    pub fn set_write_weak(&mut self, function: BlockId, id: ItemId) {
        match self
            .parameters
            .entry(function)
            .or_default()
            .entry(id)
            .or_default()
        {
            FunctionParameter::Write => {}
            FunctionParameter::Read => {}
            param @ FunctionParameter::None => *param = FunctionParameter::Write,
        }
    }

    pub fn clear(&mut self) {
        for function in self.parameters.values_mut() {
            function.clear()
        }
    }
}
