use std::{env, fs, path::Path};

use proc_macro::*;
use proc_macro_tool::*;
use rowan_peg::quick_process;

/// Auto processing grammar to module
///
/// # Example
///
/// ```ignore
/// #[rowan_peg_macro::build(path = "grammar.abnf")]
/// mod grammar;
/// ```
#[proc_macro_attribute]
pub fn build(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut item = item.parse_iter();
    let vis = item.next_vis();
    let attrs = item.next_attributes();
    let Some(mod_kwd) = item.next() else { err!("unexpected end of input, expect `mod`") };
    if !mod_kwd.is_keyword("mod") { err!("unexpected token, expect `mod`", mod_kwd) }
    let Some(name) = item.next() else { err!("unexpected end of input, expect mod name") };
    let Some(name) = name.as_ident() else { err!("unexpected token, expect mod name", name) };
    let Some(group) = item.next() else { err!("unexpected end of input, expect `{}`") };
    let Some(group) = group.as_group() else { err!("unexpected token, expect `{}`", name) };
    if !group.stream().is_empty() { err!("unexpected non-empty group, expect `{}`", group) }
    if let Some(tt) = item.next() { err!("unexpected token, expect end of input", tt) }

    let mut attr = attr.parse_iter();
    let Some(kwd) = attr.next() else { err!("unexpected end of input, expect `path`") };
    if !kwd.is_keyword("path") { err!("unexpected token, expect `path`", kwd) }
    let Some(kwd) = attr.next() else { err!("unexpected end of input, expect `=`") };
    if !kwd.is_punch('=') { err!("unexpected token, expect `=`", kwd) }
    let Some(path) = attr.next() else { err!("unexpected end of input, expect string") };
    let Some(path) = path.as_literal() else { err!("unexpected token, expect string", path) };
    let path_span = path.span();
    let path = match litrs::StringLit::parse(path.to_string()) {
        Ok(path) => path.into_value(),
        Err(e) => err!(@("parse path error: {e}"), path),
    };
    let mut find_paths = vec![];
    find_paths.extend(Path::new(&path_span.file()).parent().map(|x| x.to_owned()));
    find_paths.extend(path_span.local_file().and_then(|mut p| {
        p.pop().then_some(p).filter(|p| !p.has_root())
    }));
    find_paths.extend(env::var("CARGO_MANIFEST_DIR")
        .map(|x| Path::new(&x).join("src")));
    let Some(grammar_path) = find_paths.iter().find_map(|p| {
        let path = p.join(&path);
        path.exists().then_some(path)
    }) else { err!("cannot find grammar path") };
    let grammar = match fs::read_to_string(&grammar_path) {
        Ok(s) => s,
        Err(e) => err!(@("read grammar error `{}`: {e}", grammar_path.display()), path_span),
    };

    quick_process(&grammar)
        .and_then(|src| src.parse::<TokenStream>().map_err(|e| e.to_string()))
        .map(|out| attrs.into_iter()
            .chain(vis)
            .chain([stream([mod_kwd, name.clone().into(), out.grouped_brace().into()])])
            .collect())
        .unwrap_or_else(|e| err!(@("{e}")))
}
