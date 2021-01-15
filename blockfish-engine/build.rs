fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let out_dir = std::path::Path::new(&out_dir);

    let target_dir = out_dir.join("generated");
    std::fs::create_dir_all(&target_dir).unwrap();

    protoc_rust::Codegen::new()
        .out_dir(&target_dir)
        .include("..")
        .inputs(&["../blockfish.proto"])
        .customize(protoc_rust::Customize {
            gen_mod_rs: Some(true),
            ..protoc_rust::Customize::default()
        })
        .run()
        .expect("protoc failed");
}
