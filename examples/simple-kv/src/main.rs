use rowan::ast::AstNode;

#[allow(dead_code)]
#[rowan_peg_macro::build(path = "../grammar.abnf")]
mod grammar {}

fn main() {
    let s = "a = 2 b = text";
    dbg!(s);
    let state = &mut rowan_peg_utils::ParseState::default();
    grammar::parser::pair_list(s, state).unwrap();
    let node = grammar::SyntaxNode::new_root(state.finish());
    dbg!(&node);
    let pair_list = grammar::PairList::cast(node).unwrap();
    for pair in pair_list.pairs() {
        println!("{pair}");
        println!("  key: {}\n  val: {}", pair.key(), pair.val());
    }
}
