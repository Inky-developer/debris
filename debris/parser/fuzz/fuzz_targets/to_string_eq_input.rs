#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here
    if let Ok(s) = std::str::from_utf8(data) {
        let st = debris_parser::parser::parse(s);
        assert_eq!(st.to_string(s).as_ref(), s);
    }
});
