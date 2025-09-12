mod grammar;
use std::{collections::BTreeMap, env::args, fs};

use grammar as ast;
use rowan::ast::AstNode;

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
    ast::parser::json_text(&input, state).unwrap();
    let syntax_node = ast::SyntaxNode::new_root(state.finish());
    let json = ast::JsonText::cast(syntax_node).unwrap();
    dbg!(&json); // outputs too long
    let json_value = gen_json(json.value());
    println!("json value: {json_value:#?}");
}

fn gen_json(json: ast::Value) -> JsonValue {
    json.false_().map(|_| JsonValue::Bool(false))
        .or_else(|| json.null().map(|_| JsonValue::Null))
        .or_else(|| json.true_().map(|_| JsonValue::Bool(true)))
        .or_else(|| json.object().map(|obj| JsonValue::Object(obj.members().map(|memb| (parse_string(memb.string()), gen_json(memb.value()))).collect())))
        .or_else(|| json.array().map(|arr| JsonValue::Array(arr.values().map(gen_json).collect())))
        .or_else(|| json.number().map(|n| JsonValue::Number(n.syntax().text().to_string().parse().unwrap())))
        .or_else(|| json.string().map(parse_string).map(JsonValue::String))
        .unwrap()
}

fn parse_string(s: ast::String) -> String {
    s.chars()
        .map(|ch| ch.unescaped()
            .map(|ch| ch.text().chars().next().unwrap())
            .or_else(|| ch.double_quote().map(|_| '"'))
            .or_else(|| ch.backslash().map(|_| '\\'))
            .or_else(|| ch.slash().map(|_| '/'))
            .or_else(|| ch.b_kw().map(|_| '\x08'))
            .or_else(|| ch.f_kw().map(|_| '\x0c'))
            .or_else(|| ch.n_kw().map(|_| '\x0a'))
            .or_else(|| ch.r_kw().map(|_| '\x0d'))
            .or_else(|| ch.t_kw().map(|_| '\x09'))
            .or_else(|| ch.u_kw().map(|_| {
                let n = u32::from_str_radix(ch.hexdig4().unwrap().text(), 16).unwrap();
                char::from_u32(n).unwrap()
            }))
            .unwrap())
        .collect()
}
