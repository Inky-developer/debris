use rustc_hash::FxHashMap;

use super::{llir_nodes::Function, LLIRBuilder};
use crate::{
    error::Result,
    mir::{ContextId, MirContextMap, NamespaceArena},
};

/// The low-level intermediate representation struct
///
/// Contains all generated functions and a compilation configuration
#[derive(Debug)]
pub struct Llir {
    /// The entry point of the program
    pub main_function: Function,
    /// The functions which were created, excluding the main function
    pub functions: Vec<Function>,
}

impl Llir {
    /// Compiles the mir into a llir
    pub fn from_mir(contexts: &MirContextMap, namespaces: &mut NamespaceArena) -> Result<Llir> {
        let mut llir_helper = LlirFunctions::default();
        let main_context = contexts.get_main_context();

        LLIRBuilder::new(main_context, namespaces, contexts, &mut llir_helper).build()?;

        Ok(llir_helper.into())
    }
}

/// Contains the already generated llir functions and
/// in the future potentialle other details
#[derive(Debug, Default)]
pub(crate) struct LlirFunctions {
    pub functions: FxHashMap<ContextId, Function>,
    pub main_function: Option<ContextId>,
}

impl LlirFunctions {
    pub fn add(&mut self, function: Function) {
        // The last function must be the main function
        self.main_function = Some(function.id);

        let prev_function = self.functions.insert(function.id, function);
        assert!(
            prev_function.is_none(),
            "Duplicate function with id {:?}",
            prev_function.unwrap().id
        );
    }

    pub fn get_function(&mut self, id: &ContextId) -> &mut Function {
        self.functions.get_mut(id).expect("Could not find function")
    }
}

impl Into<Llir> for LlirFunctions {
    fn into(mut self) -> Llir {
        let main_function = self
            .functions
            .remove(&self.main_function.expect("No main function"))
            .unwrap();

        Llir {
            main_function,
            functions: self
                .functions
                .into_iter()
                .map(|(_id, function)| function)
                .collect(),
        }
    }
}
