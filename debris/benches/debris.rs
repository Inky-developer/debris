use criterion::{criterion_group, criterion_main, Criterion};
// use debris_backends::{Backend, DatapackBackend};
use debris_common::Code;
use debris_core::llir::Llir;
use debris_lang::{get_std_module, CompileConfig};

pub fn run_code(code: String) -> Llir {
    let mut config = CompileConfig::new(get_std_module().into(), ".".into());
    let main_file = config.compile_context.add_input_file(Code {
        path: None,
        source: code,
    });

    let hir = config.get_hir(main_file).unwrap();
    let mut mir = config.get_mir(&hir).unwrap();
    config.get_llir(&mir.contexts, &mut mir.namespaces).unwrap()
}

pub fn run_benchmarks(c: &mut Criterion) {
    const BENCHES: [(&str, &str); 3] = [
        ("basic", include_str!("benchmarks/basic.de")),
        ("long", include_str!("benchmarks/long.de")),
        ("math", include_str!("benchmarks/math.de")),
    ];
    for (name, bench) in BENCHES.iter() {
        c.bench_function(name, |b| b.iter(|| run_code(bench.to_string())));
    }
}

criterion_group!(benches, run_benchmarks);
criterion_main!(benches);
