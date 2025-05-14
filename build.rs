use std::env;
use std::path::{Path, PathBuf};

fn main() {
    // `git submodule init update --init --recursive` is needed to
    // fetch the oniguruma submodule.
    // This is needed because the onigmo submodule is not
    // fetched by default when cloning the repository.
    if !Path::new("onigmo/onigmo.h").exists() {
        let status = std::process::Command::new("git")
            .args(&["submodule", "update", "--init", "--recursive"])
            .status()
            .expect("Failed to run git submodule update");

        if !status.success() {
            panic!("Failed to update git submodules");
        }
    }

    println!("cargo:rerun-if-changed=.gitmodules");

    // Tell cargo to tell rustc to link the system onigmo
    // static library.
    println!("cargo:rustc-link-lib=static=onigmo");

    // The bindgen::Builder is the main entry point
    // to bindgen, and lets you build up options for
    // the resulting bindings.
    let bindings = bindgen::Builder::default()
        // The input header we would like to generate
        // bindings for.
        .header("wrapper.h")
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");

    compile();
}

fn compile() {
    //bindgen_headers("oniguruma/src/oniguruma.h");

    let mut cc = cc::Build::new();
    let out_dir = PathBuf::from(env::var("OUT_DIR").expect("OUT_DIR"));
    let ref src = Path::new("onigmo");
    let config_h = out_dir.join("config.h");

    if !src.exists() {
        panic!(
            "Unable to find source files in {}. Is oniguruma submodule checked out?\n\
             Try git submodule init; git submodule update",
            src.display()
        );
    }

    let bits = env::var("CARGO_CFG_TARGET_POINTER_WIDTH").unwrap();

    let family = env::var("CARGO_CFG_TARGET_FAMILY");
    if let Ok("unix") = family.as_ref().map(String::as_str) {
        cc.define("HAVE_UNISTD_H", Some("1"));
        cc.define("HAVE_SYS_TYPES_H", Some("1"));
        cc.define("HAVE_SYS_TIME_H", Some("1"));
    }

    // Can't use size_of::<c_long>(), because it'd refer to build arch, not target arch.
    // so instead assume it's a non-exotic target (LP32/LP64).
    std::fs::write(
        config_h,
        format!(
            "
            #define HAVE_PROTOTYPES 1
            #define STDC_HEADERS 1
            #define HAVE_STRING_H 1
            #define HAVE_STDARG_H 1
            #define HAVE_STDLIB_H 1
            #define HAVE_LIMITS_H 1
            #define HAVE_INTTYPES_H 1
            #define SIZEOF_INT 4
            #define SIZEOF_SHORT 2
            #define SIZEOF_LONG {0}
            #define SIZEOF_VOIDP {0}
            #define SIZEOF_LONG_LONG 8
        ",
            if bits == "64" { "8" } else { "4" }
        ),
    )
    .expect("Can't write config.h to OUT_DIR");

    cc.include(out_dir); // Read config.h from there
    cc.include(src);
    cc.include(src.join("enc"));
    cc.include(src.join("enc/jis"));
    cc.include(src.join("enc/unicode"));

    let files = [
        "regexec.c",
        "regerror.c",
        "regparse.c",
        "regext.c",
        "regcomp.c",
        "reggnu.c",
        "regenc.c",
        "regsyntax.c",
        "regtrav.c",
        "regversion.c",
        "st.c",
        "enc/unicode.c",
        "enc/ascii.c",
        "enc/utf_8.c",
        "enc/utf_16be.c",
        "enc/utf_16le.c",
        "enc/utf_32be.c",
        "enc/utf_32le.c",
        "enc/euc_jp.c",
        "enc/shift_jis.c",
        "enc/iso_8859_1.c",
        "enc/iso_8859_2.c",
        "enc/iso_8859_3.c",
        "enc/iso_8859_4.c",
        "enc/iso_8859_5.c",
        "enc/iso_8859_6.c",
        "enc/iso_8859_7.c",
        "enc/iso_8859_8.c",
        "enc/iso_8859_9.c",
        "enc/iso_8859_10.c",
        "enc/iso_8859_11.c",
        "enc/iso_8859_13.c",
        "enc/iso_8859_14.c",
        "enc/iso_8859_15.c",
        "enc/iso_8859_16.c",
        "enc/euc_tw.c",
        "enc/euc_kr.c",
        "enc/big5.c",
        "enc/gb18030.c",
        "enc/koi8_r.c",
    ];
    for file in files.iter() {
        cc.file(src.join(file));
    }

    cc.warnings(false); // not actionable by the end user
    cc.compile("onigmo");
}
