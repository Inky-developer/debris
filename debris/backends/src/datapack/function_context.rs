use std::{collections::HashMap, ops::Add, rc::Rc};

use debris_core::mir::ContextId;

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

impl Add<usize> for FunctionId {
    type Output = Self;

    fn add(self, rhs: usize) -> Self {
        FunctionId(self.0 + rhs)
    }
}

#[derive(Debug)]
pub(super) struct FunctionContext {
    function_namespace: Rc<str>,
    user_id_map: HashMap<ContextId, FunctionId>,
    current_function_id: FunctionId,
    function_identifiers: HashMap<FunctionId, Rc<FunctionIdent>>,
    functions: Vec<GeneratedFunction>,
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

    /// Deletes this context and returns all generated functiosn
    pub fn functions(&self) -> &[GeneratedFunction] {
        self.functions.as_slice()
    }

    /// Register a function with this id.
    /// Returns the filename of the resulting function
    fn register(&mut self, id: FunctionId) {
        let function_name = format!("block_{}_", id.0);
        let identifier = FunctionIdent {
            is_collection: false,
            namespace: self.function_namespace.clone(),
            path: function_name,
        };
        self.function_identifiers.insert(id, Rc::new(identifier));
    }

    pub fn register_function(&mut self, id: ContextId) -> FunctionId {
        if let Some(fn_id) = self.user_id_map.get(&id) {
            *fn_id
        } else {
            let function_id = self.next_function_id();
            self.user_id_map.insert(id, function_id);
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

    pub fn get_function_with_id(&mut self, id: ContextId) -> Option<Rc<FunctionIdent>> {
        self.user_id_map
            .get(&id)
            .cloned()
            .and_then(|id| self.get_function(id))
    }

    pub fn get_function(&mut self, id: FunctionId) -> Option<Rc<FunctionIdent>> {
        self.function_identifiers.get(&id).cloned()
    }

    /// Inserts a function with a given id
    pub fn insert(&mut self, id: FunctionId, commands: Vec<MinecraftCommand>) {
        let identifier = self
            .function_identifiers
            .get(&id)
            .expect("Function must be registered first")
            .clone();
        self.functions.push(GeneratedFunction {
            commands,
            identifier,
        });
    }

    /// inserts a function with this name into the function root directory
    pub fn insert_with_name(&mut self, name: String, commands: Vec<MinecraftCommand>) {
        self.functions.push(GeneratedFunction {
            commands,
            identifier: Rc::new(FunctionIdent {
                is_collection: false,
                namespace: self.function_namespace.clone(),
                path: name,
            }),
        });
    }

    fn next_function_id(&mut self) -> FunctionId {
        let id = self.current_function_id;
        self.current_function_id = id + 1;
        id
    }
}
