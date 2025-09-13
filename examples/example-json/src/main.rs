#[allow(dead_code)]
mod grammar;
use std::{collections::BTreeMap, env::args, fs, process::exit};

use grammar as ast;
use rowan::ast::AstNode;
use rowan_peg_utils::match_options;

#[derive(Debug)]
pub enum JsonValue {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<JsonValue>),
    Object(BTreeMap<String, JsonValue>),
}

fn main() {
    let input = fs::read_to_string(args().nth(1).unwrap()).unwrap();
    let state = &mut rowan_peg_utils::ParseState::default();
    if let Err(e) = ast::parser::json_text(&input, state) {
        eprintln!("parse {e}");
        exit(1)
    };
    let syntax_node = ast::SyntaxNode::new_root(state.finish());
    let json = ast::JsonText::cast(syntax_node).unwrap();
    dbg!(&json); // outputs too long
    let json_value = gen_json(json.value());
    println!("json value: {json_value:#?}");
}

fn gen_json(json: ast::Value) -> JsonValue {
    match_options! {match json {
        false_ as _ => JsonValue::Bool(false),
        null as _ => JsonValue::Null,
        true_ as _ => JsonValue::Bool(true),
        object => {
            JsonValue::Object(object.members()
                .map(|it| (parse_string(it.string()), gen_json(it.value())))
                .collect())
        },
        array => JsonValue::Array(array.values().map(gen_json).collect()),
        number => JsonValue::Number(number.syntax().text().to_string().parse().unwrap()),
        string => JsonValue::String(parse_string(string)),
        _ => unreachable!(),
    }}
}

fn parse_string(s: ast::String) -> String {
    #[allow(unused_variables)]
    s.chars().map(|it| match_options! {match it {
        unescaped => unescaped.text().chars().next().unwrap(),
        double_quote => '"',
        backslash => '\\',
        slash => '/',
        b_kw => '\x08',
        f_kw => '\x0c',
        n_kw => '\x0a',
        r_kw => '\x0d',
        t_kw => '\x09',
        hexdig4 => char::from_u32(u32::from_str_radix(hexdig4.text(), 16).unwrap()).unwrap(),
        _ => unreachable!(),
    }}).collect()
}
