use criterion::{criterion_group, criterion_main, Criterion};
// use debris_backends::{Backend, DatapackBackend};
use debris_common::{file_provider::FsFileProvider, Code, OptMode};
use debris_lang::CompileConfig;
use debris_llir::Llir;

pub fn run_code(code: Box<str>, opt_mode: OptMode) -> Llir {
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

pub fn run_benchmarks(c: &mut Criterion) {
    const BENCHES: [(&str, &str); 3] = [
        ("basic", include_str!("benchmarks/basic.de")),
        ("long", include_str!("benchmarks/long.de")),
        ("math", include_str!("benchmarks/math.de")),
    ];
    for (name, bench) in BENCHES {
        for &opt_mode in &[OptMode::Debug, OptMode::Full] {
            c.bench_function(&format!("{}({:?})", name, opt_mode), |b| {
                b.iter(|| run_code(bench.into(), opt_mode));
            });
        }
    }
}

criterion_group!(benches, run_benchmarks);
criterion_main!(benches);
