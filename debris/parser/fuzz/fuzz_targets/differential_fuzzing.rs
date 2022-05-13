#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here
    if let Ok(s) = std::str::from_utf8(data) {
        let st = debris_parser::parser::parse(s);
        
        let mut ctx = debris_lang::common::CompileContext::new(debris_lang::common::CompilationId(0));
        let code_id = ctx.add_input_file(debris_lang::common::Code {
            source: s.into(),
            path: None,
        });
        let code_ref = ctx.input_files.get_code_ref(code_id);
        let hir_result = debris_hir::HirFile::from_code(code_ref, &ctx, &mut debris_hir::ImportDependencies::default());

        assert_eq!(st.errors.is_empty(), hir_result.is_ok());
    }
});
