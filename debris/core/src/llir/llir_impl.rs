use std::fmt;

use itertools::Itertools;
use rustc_hash::FxHashMap;

use crate::{error::Result, CompileContext, ObjectRef};

use super::{
    llir_nodes::{Call, Function, Node},
    opt::peephole_opt::PeepholeOptimizer,
    utils::BlockId,
    Runtime,
};
use crate::llir::opt::global_opt::GlobalOptimizer;
use crate::mir::Mir;

/// The low-level intermediate representation struct
///
/// Contains all generated functions and a compilation configuration
#[derive(Debug)]
pub struct Llir {
    /// The functions which were created, excluding the main function
    pub functions: FxHashMap<BlockId, Function>,
    /// The runtime, which stores resources
    pub runtime: Runtime,
}

impl Llir {
    /// Compiles the mir into a llir
    pub fn new(ctx: &CompileContext, _mir: &Mir) -> Result<Llir> {
        let runtime = Runtime::default();
        let optimizer = GlobalOptimizer::new(&ctx.config, &runtime, Default::default(), BlockId(0));
        let _result = optimizer.run();

        let mut peephole_optimizer = PeepholeOptimizer::default();
        peephole_optimizer.push(Node::Nop);
        println!("{:?}", peephole_optimizer.nodes());
        let _ = peephole_optimizer.take();
        // let mut llir_functions = LlirFunctions::default();
        // let main_context = contexts.get_main_context();
        //
        // let builder = LlirBuilder::new(main_context, namespaces, contexts, &mut llir_functions);
        // let main_function = builder.current_function;
        // builder.build()?;
        //
        // llir_functions.main_function = Some(main_function);
        // llir_functions.runtime.add_on_load(main_function);
        //
        // for ticking_context_id in &contexts.ticking_contexts {
        //     let context = contexts.get(*ticking_context_id);
        //     let builder = LlirBuilder::new(context, namespaces, contexts, &mut llir_functions);
        //     let block_id = builder.current_function;
        //     builder.build()?;
        //     llir_functions.runtime.schedule(block_id);
        // }
        //
        // let config = &main_context.compile_context.config;
        // Ok(llir_functions.into_llir(config))
        todo!()
    }

    pub fn get_function_calls(&self) -> FxHashMap<BlockId, usize> {
        let mut stats = FxHashMap::default();
        for function in self.functions.values() {
            for node in function.nodes() {
                node.iter(&mut |node| {
                    if let Node::Call(Call { id }) = node {
                        *stats.entry(*id).or_default() += 1;
                    }
                });
            }
        }

        for on_load_block in &self.runtime.load_blocks {
            *stats.entry(*on_load_block).or_default() += 1;
        }

        for on_tick_block in &self.runtime.scheduled_blocks {
            *stats.entry(*on_tick_block).or_default() += 1;
        }

        stats
    }
}

impl fmt::Display for Llir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let call_stats = self.get_function_calls();
        let fmt_function = &|func: &Function, f: &mut fmt::Formatter<'_>| {
            f.write_fmt(format_args!(
                "({} call(s)) - {}",
                call_stats.get(&func.id).unwrap_or(&0),
                func
            ))
        };

        for function in self.functions.values().sorted_by_key(|func| func.id) {
            fmt_function(function, f)?;
            f.write_str("\n")?;
        }

        Ok(())
    }
}

/// A function node as it is represented during the llir stage
#[derive(Debug)]
pub struct LlirFunction {
    pub returned_value: ObjectRef,
    pub nodes: PeepholeOptimizer,
}
