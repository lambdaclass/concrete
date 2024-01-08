use std::env::var;

fn main() {
    let mlir_path = var("MLIR_SYS_170_PREFIX").expect("MLIR path should be set.");

    cc::Build::new()
        .cpp(true)
        .flag("-std=c++17")
        .flag_if_supported("-Wno-unused-parameter")
        .flag_if_supported("-Wno-comment")
        .include(&format!("{mlir_path}/include"))
        .file("src/wrappers.cpp")
        .compile("ffi");

    println!("cargo:rerun-if-changed=src/wrappers.cpp");
}
