use criterion::{criterion_group, criterion_main, Criterion};
// use debris_backends::{Backend, DatapackBackend};
use debris_common::{Code, OptMode};
use debris_lang::CompileConfig;
use debris_llir::Llir;

pub fn run_code(code: String, opt_mode: OptMode) -> Llir {
    let mut config = CompileConfig::new(".".into());
    config.compile_context.config.opt_mode = opt_mode;
    let main_file = config.compile_context.add_input_file(Code {
        path: None,
        source: code,
    });

    let hir = config.compute_hir(main_file).unwrap();
    let mir = config.compute_mir(&hir).unwrap();
    config.compute_llir(&mir, debris_std::load_all).unwrap()
}

pub fn run_benchmarks(c: &mut Criterion) {
    const BENCHES: [(&str, &str); 3] = [
        ("basic", include_str!("benchmarks/basic.de")),
        ("long", include_str!("benchmarks/long.de")),
        ("math", include_str!("benchmarks/math.de")),
    ];
    for (name, bench) in BENCHES.iter() {
        for &opt_mode in &[OptMode::Debug, OptMode::Full] {
            c.bench_function(&format!("{}({:?})", name, opt_mode), |b| {
                b.iter(|| run_code(bench.to_string(), opt_mode))
            });
        }
    }
}

criterion_group!(benches, run_benchmarks);
criterion_main!(benches);
