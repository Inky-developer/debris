use super::{llir_nodes::Function, LLIRBuilder};
use crate::{error::Result, mir::MirContext, mir::NamespaceArena};

/// The low-level intermediate representation struct
///
/// Contains all generated functions and a compilation configuration
#[derive(Debug, Eq, PartialEq)]
pub struct Llir {
    /// The functions which were created, the main function at the last index
    pub functions: Vec<Function>,
}

impl Llir {
    /// Compiles the mir into a llir
    pub fn from_mir(contexts: &[MirContext], namespaces: &mut NamespaceArena) -> Result<Llir> {
        let mut llir_helper = LlirHelper::default();
        let main_context = &contexts[0];

        LLIRBuilder::new(main_context, namespaces, contexts, &mut llir_helper).build()?;

        Ok(llir_helper.into())
    }
}

#[derive(Default)]
pub(crate) struct LlirHelper {
    pub functions: Vec<Function>,
    current_function_id: usize,
}

impl LlirHelper {
    pub fn push(&mut self, function: Function) {
        self.functions.push(function)
    }

    pub fn next_id(&mut self) -> usize {
        let id = self.current_function_id;
        self.current_function_id += 1;
        id
    }
}

impl Into<Llir> for LlirHelper {
    fn into(self) -> Llir {
        Llir {
            functions: self.functions,
        }
    }
}
