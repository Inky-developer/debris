use std::rc::Rc;

use debris_llir::utils::BlockId;
use rustc_hash::FxHashMap;

use crate::common::{FunctionIdent, MinecraftCommand};

#[derive(Debug)]
pub(super) struct GeneratedFunction {
    pub identifier: Rc<FunctionIdent>,
    pub commands: Vec<MinecraftCommand>,
}

impl GeneratedFunction {
    pub fn get_filename(&self) -> String {
        format!("{}.mcfunction", self.identifier.path)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, Default)]
pub(super) struct FunctionId(usize);

impl FunctionId {
    pub fn increment(&mut self) {
        self.0 += 1;
    }
}

#[derive(Debug)]
pub(super) struct FunctionContext {
    /// The name of the current namespace
    function_namespace: Rc<str>,
    /// A bijective mapping from blocks to minecraft functions
    /// (Only one direction needed)
    user_id_map: FxHashMap<BlockId, FunctionId>,
    current_function_id: FunctionId,
    function_identifiers: FxHashMap<FunctionId, Rc<FunctionIdent>>,
    functions: FxHashMap<FunctionId, GeneratedFunction>,
}

impl FunctionContext {
    pub fn new(function_namespace: Rc<str>) -> Self {
        FunctionContext {
            function_namespace,
            current_function_id: Default::default(),
            function_identifiers: Default::default(),
            functions: Default::default(),
            user_id_map: Default::default(),
        }
    }

    /// Returns an iterator of all the generated functions
    pub fn functions(&self) -> impl Iterator<Item = &GeneratedFunction> {
        self.functions.values()
    }

    /// Register a function with this id.
    /// Returns the filename of the resulting function
    fn register(&mut self, id: FunctionId) {
        let function_name = format!("block_{}_", id.0);
        self.register_with_name(id, function_name);
    }

    pub fn register_with_name(&mut self, id: FunctionId, name: String) {
        let identifier = FunctionIdent {
            is_collection: false,
            namespace: self.function_namespace.clone(),
            path: name,
        };
        self.function_identifiers.insert(id, Rc::new(identifier));
    }

    // False positive due to borrow checker limitation
    #[allow(clippy::option_if_let_else)]
    pub fn register_function(&mut self, block: BlockId) -> FunctionId {
        if let Some(fn_id) = self.user_id_map.get(&block) {
            *fn_id
        } else {
            let function_id = self.next_function_id();
            self.user_id_map.insert(block, function_id);
            self.register(function_id);
            function_id
        }
    }

    /// Registers a custom function which was not specified by the llir
    ///
    /// Returns the id of this function
    pub fn register_custom_function(&mut self) -> FunctionId {
        let id = self.next_function_id();
        self.register(id);
        id
    }

    pub fn get_function_id(&self, block: BlockId) -> Option<FunctionId> {
        self.user_id_map.get(&block).copied()
    }

    // pub fn get_function_ident_with_id(&mut self, id: FunctionId) -> Option<Rc<FunctionIdent>> {
    //     self.get_function_ident(id)
    // }

    pub fn get_function_ident(&mut self, id: FunctionId) -> Option<Rc<FunctionIdent>> {
        self.function_identifiers.get(&id).cloned()
    }

    /// Inserts a function with a given id
    pub fn insert(&mut self, id: FunctionId, commands: Vec<MinecraftCommand>) {
        let identifier = self
            .function_identifiers
            .get(&id)
            .expect("Function must be registered first")
            .clone();
        self.functions.insert(
            id,
            GeneratedFunction {
                identifier,
                commands,
            },
        );
    }

    fn next_function_id(&mut self) -> FunctionId {
        let id = self.current_function_id;
        self.current_function_id.increment();
        id
    }
}
