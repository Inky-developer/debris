use criterion::{criterion_group, criterion_main, Criterion};
// use debris_backends::{Backend, DatapackBackend};
use debris_common::{file_provider::FsFileProvider, Code, CompilationId, CompileContext, OptMode};
use debris_hir::{HirFile, ImportDependencies};
use debris_lang::CompileConfig;
use debris_llir::Llir;

fn run_code(code: Box<str>, opt_mode: OptMode) -> Llir {
    let mut config = CompileConfig::new(Box::new(FsFileProvider::new(".".into())));
    config.compile_context.config.opt_mode = opt_mode;
    let main_file = config.compile_context.add_input_file(Code {
        path: None,
        source: code,
    });

    let high_ir = config.compute_hir(main_file).unwrap();
    let medium_ir = config.compute_mir(&high_ir).unwrap();
    config
        .compute_llir(&medium_ir, debris_std::load_all)
        .unwrap()
}

const BENCHES: [(&str, &str); 3] = [
    ("basic", include_str!("benchmarks/basic.de")),
    ("long", include_str!("benchmarks/long.de")),
    ("math", include_str!("benchmarks/math.de")),
];

fn run_compiler_benchmarks(c: &mut Criterion) {
    for (name, bench) in BENCHES {
        for &opt_mode in &[OptMode::Debug, OptMode::Full] {
            c.bench_function(&format!("{}({:?})", name, opt_mode), |b| {
                b.iter(|| run_code(bench.into(), opt_mode));
            });
        }
    }
}

fn run_parser_benchmarks(c: &mut Criterion) {
    for (name, bench) in BENCHES {
        let mut compile_context = CompileContext::new(CompilationId(0));
        let id = compile_context.add_input_file(Code {
            path: None,
            source: bench.into(),
        });
        let code_ref = compile_context.input_files.get_code_ref(id);
        let mut dependency_list = ImportDependencies::default();
        c.bench_function(name, |b| {
            b.iter(|| HirFile::from_code(code_ref, &compile_context, &mut dependency_list));
        });
    }
}

criterion_group!(compiler_benchmarks, run_compiler_benchmarks);
criterion_group!(parser_benchmarks, run_parser_benchmarks);
criterion_main!(parser_benchmarks, compiler_benchmarks);
