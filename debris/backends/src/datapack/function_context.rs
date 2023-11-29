use std::rc::Rc;

use debris_llir::{block_id::BlockId, extern_item_path::ExternItemPath};
use indexmap::IndexMap;
use rustc_hash::FxHashMap;

use crate::{
    common::{FunctionIdent, MinecraftCommand},
    DatapackBackend,
};

#[derive(Debug, Default)]
struct BuildingFunction {
    commands: Vec<MinecraftCommand>,
    post_commands: Vec<MinecraftCommand>,
}

impl BuildingFunction {
    fn assemble(self) -> Vec<MinecraftCommand> {
        let mut result = self.commands;
        result.extend(self.post_commands);
        result
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Default)]
pub(super) struct FunctionId(usize);

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum FunctionLocation {
    Main { function_name: String },
    Custom { path: ExternItemPath },
}

impl FunctionLocation {
    fn as_function_ident(&self, namespace: Rc<str>) -> FunctionIdent {
        match self {
            FunctionLocation::Main { function_name } => FunctionIdent {
                is_collection: false,
                namespace,
                path: format!(
                    "{}{}",
                    DatapackBackend::FUNCTION_INTERNAL_PATH,
                    function_name
                ),
            },
            FunctionLocation::Custom { path } => FunctionIdent {
                is_collection: false,
                namespace,
                path: path.as_ref().to_string(),
            },
        }
    }
}

#[derive(Debug)]
pub(super) struct FunctionContext {
    functions: IndexMap<Rc<FunctionIdent>, Option<BuildingFunction>>,
    block_id_mapping: FxHashMap<BlockId, FunctionId>,
    current_function_id: usize,
    /// The debris namespace
    namespace: Rc<str>,
}

impl FunctionContext {
    pub fn new(main_namespace: Rc<str>) -> Self {
        FunctionContext {
            namespace: main_namespace,
            functions: Default::default(),
            block_id_mapping: Default::default(),
            current_function_id: Default::default(),
        }
    }

    pub fn into_functions(
        self,
    ) -> impl Iterator<Item = (Rc<FunctionIdent>, Vec<MinecraftCommand>)> {
        self.functions
            .into_iter()
            .filter_map(|(ident, opt)| opt.map(|function| (ident, function.assemble())))
    }

    pub fn reserve_at(&mut self, function_location: &FunctionLocation) -> FunctionId {
        let (idx, _) = self.functions.insert_full(
            function_location
                .as_function_ident(self.namespace.clone())
                .into(),
            Default::default(),
        );
        FunctionId(idx)
    }

    pub fn reserve(&mut self) -> FunctionId {
        let function_name = format!("block_{}", self.current_function_id);
        self.current_function_id += 1;
        self.reserve_at(&FunctionLocation::Main { function_name })
    }

    pub fn reserve_block(&mut self, block_id: BlockId) -> FunctionId {
        if let Some(function_id) = self.get_function_id(block_id) {
            return function_id;
        }

        let id = self.reserve();
        self.block_id_mapping.insert(block_id, id);
        id
    }

    pub fn insert(&mut self, id: FunctionId, commands: impl Iterator<Item = MinecraftCommand>) {
        match self.functions.get_index_mut(id.0).unwrap().1 {
            opt @ None => {
                *opt = Some(BuildingFunction {
                    commands: commands.collect(),
                    post_commands: Vec::new(),
                });
            }
            Some(function) => function.commands.extend(commands.into_iter()),
        };
    }

    pub fn append_to_fn(&mut self, id: FunctionId, command: MinecraftCommand) {
        match self.functions.get_index_mut(id.0).unwrap().1 {
            opt @ None => {
                *opt = Some(BuildingFunction {
                    commands: Vec::new(),
                    post_commands: vec![command],
                });
            }
            Some(func) => func.post_commands.push(command),
        }
    }

    pub fn get_function_id(&self, block_id: BlockId) -> Option<FunctionId> {
        self.block_id_mapping.get(&block_id).copied()
    }

    pub fn get_function_id_from_ident(&self, ident: &FunctionIdent) -> FunctionId {
        FunctionId(self.functions.get_full(ident).unwrap().0)
    }

    pub fn get_function_ident(&self, function_id: FunctionId) -> Rc<FunctionIdent> {
        self.functions.get_index(function_id.0).unwrap().0.clone()
    }
}
