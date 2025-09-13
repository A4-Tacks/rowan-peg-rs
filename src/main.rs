use std::{env::args, fs, io::{self, stdin}, process::exit};

use rowan_peg::quick_process;

fn main() {
    let options = getopts_macro::getopts_options! {
        -v, --version       "show version";
        -h, --help          "show help messages";
    };
    let matched = match options.parse(args().skip(1)) {
        Ok(matched) => matched,
        Err(e) => {
            eprintln!("{e}");
            exit(2)
        },
    };
    if matched.opt_present("help") {
        let proc = args().next().unwrap();
        let desc = "Convert ABNF like grammar to rust-peg declaration";
        let brief = format!("Usage: {proc} [Options] [FILE]\n{desc}");
        let usage = options.usage(&brief);
        print!("{usage}");
        return;
    }
    if matched.opt_present("version") {
        println!("{}", env!("CARGO_PKG_VERSION"));
        return;
    }

    if let Some(arg) = matched.free.get(1) {
        eprintln!("Extra input arg: {arg:?}");
        exit(2)
    }

    let input = match matched.free.first() {
        Some(path) => fs::read_to_string(path).unwrap(),
        None => io::read_to_string(stdin().lock()).unwrap(),
    };

    match quick_process(&input) {
        Ok(buf) => println!("{buf}"),
        Err(e) => {
            eprintln!("{e}");
            exit(1)
        },
    }
}
