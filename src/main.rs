use std::{io::{self, stdin}, process::exit};

use rowan::ast::AstNode;
use rowan_peg::{DeclList, Processor, SyntaxNode};
use rowan_peg_utils::ParseState;

fn main() {
    let input = io::read_to_string(stdin().lock()).unwrap();
    let state = &mut ParseState::default();
    rowan_peg::decl_list(&input, state).unwrap();
    let syntax_node = SyntaxNode::new_root(state.finish());
    let decl_list = DeclList::cast(syntax_node).unwrap();
    let mut buf = String::new();
    let mut proc = Processor::from(&mut buf);
    match proc.start_process(&decl_list) {
        Ok(()) => {},
        Err(e) => {
            eprintln!("Process codegen error: {e:#?}");
            exit(1)
        },
    }
    println!("{buf}");
}
