use rowan::{ast::{support, AstChildren, AstNode}, Language};

macro_rules! classes {
    ($($pat:tt)*) => {
        |s, i| ::char_classes::FirstElem::first_elem(&s[i..])
            .filter(::char_classes::any!($($pat)*))
            .map_or(::peg::RuleResult::Failed, |ch| {
                ::peg::RuleResult::Matched(i+ch.len_utf8(), ())
            })
    };
}
macro_rules! decl_ast_node {
    ($node:ident, $kind:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $node(SyntaxNode);
        impl AstNode for $node {
            type Language = Lang;

            fn syntax(&self) -> &rowan::SyntaxNode<Self::Language> {
                &self.0
            }

            fn can_cast(kind: <Self::Language as Language>::Kind) -> bool {
                kind == SyntaxKind::$kind
            }

            fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self> {
                if Self::can_cast(node.kind()) {
                    Some(Self(node))
                } else {
                    None
                }
            }
        }
        impl core::fmt::Display for $node {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                core::fmt::Display::fmt(self.syntax(), f)
            }
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}
impl Language for Lang {
    type Kind = SyntaxKind;

    fn kind_to_raw(kind: Self::Kind) -> ::rowan::SyntaxKind {
        kind.into()
    }

    fn kind_from_raw(raw: ::rowan::SyntaxKind) -> Self::Kind {
        raw.into()
    }
}

pub type SyntaxNode = ::rowan::SyntaxNode<Lang>;
pub type SyntaxToken = ::rowan::SyntaxToken<Lang>;

pub use parser::*;
::peg::parser!(grammar parser<'b>(state: &'b ::rowan_peg_utils::ParseState<'input>) for str {
    use SyntaxKind::*;

    rule _back__(r: rule<()>) = g:({ state.guard_none() }) r() { g.accept_none() }
    rule _quiet__(r: rule<()>) = g:({state.quiet().guard_none()}) quiet!{r()} { g.accept_none() }
    rule _tok__(k: SyntaxKind, r: rule<()>) = (g:({state.quiet().guard_token(k)}) s:$(quiet!{r()}) { g.accept_token(s) })
    rule _node__(k: SyntaxKind, r: rule<()>) = (g:({state.guard(k)}) r() { g.accept() })

    rule comment() = ()()_back__(<()_tok__(COMMENT, <()()_back__(<()";" _back__(<()#{classes!(^"\n")}>)*>)>)>) / expected!("comment")
    rule whitespace() = ()()_back__(<()_tok__(WHITESPACE, <()()_back__(<()_back__(<()#{classes!(" \t\r\n")}>)*>)>)>)
    pub rule trivia() = _
    rule _() = _node__(TRIVIA, <()()_back__(<()whitespace() _back__(<()()_back__(<()comment() whitespace()>)>)*>)>)
    rule ident() = ()()_back__(<()_tok__(IDENT, <()()_back__(<()!_quiet__(<()#{classes!("0-9")}>) _back__(<()()_back__(<()#{classes!("0-9a-zA-Z_-")}>) / ()_back__(<()#{classes!(^"\u{00}-\u{a0}")}>)>)+>)>)>) / expected!("ident")
    rule number() = ()()_back__(<()_tok__(NUMBER, <()()_back__(<()_back__(<()#{classes!("0-9")}>)+>)>)>) / expected!("number")
    rule string() = ()()_back__(<()_tok__(STRING, <()()_back__(<()"\"" _back__(<()#{classes!(^"\"\r\n")}>)* "\"">)>)>) / expected!("string")
    rule matches() = ()()_back__(<()_tok__(MATCHES, <()()_back__(<()"<" _back__(<()#{classes!(^"\x3e\r\n")}>)* ">">)>)>) / expected!("matches")
    rule label() = _node__(LABEL, <()()_back__(<()ident()>) / ()_back__(<()string()>)>)
    rule repeat_rest() = _node__(REPEAT_REST, <()()_back__(<()_tok__(STAR, <()"*">) _back__(<()()_back__(<()number()>)>)?>)>)
    rule repeat() = _node__(REPEAT, <()()_back__(<()_tok__(PLUS, <()"+">)>) / ()_back__(<()number() _back__(<()()_back__(<()repeat_rest()>)>)?>) / ()_back__(<()repeat_rest()>)>)
    rule pat_expect() = _node__(PAT_EXPECT, <()()_back__(<()_tok__(AT, <()"@">) label()>)>)
    rule pat_atom() = _node__(PAT_ATOM, <()()_back__(<()ident() !_quiet__(<()()_back__(<()_() _tok__(EQ, <()"=">)>)>)>) / ()_back__(<()string()>) / ()_back__(<()matches()>) / ()_back__(<()_tok__(L_BRACK, <()"[">) _() pat_choice() _() _tok__(R_BRACK, <()"]">)>) / ()_back__(<()_tok__(L_PAREN, <()"(">) _() pat_choice() _() _tok__(R_PAREN, <()")">)>)>)
    rule pat_op() = _node__(PAT_OP, <()()_back__(<()_tok__(AMP, <()"&">) pat_atom()>) / ()_back__(<()_tok__(BANG, <()"!">) pat_atom()>) / ()_back__(<()_tok__(TILDE, <()"~">) pat_atom()>) / ()_back__(<()_tok__(DOLLAR, <()"$">) pat_atom()>) / ()_back__(<()repeat() _() pat_atom()>) / ()_back__(<()pat_atom()>)>)
    rule pat_list() = _node__(PAT_LIST, <()()_back__(<()pat_op() _back__(<()()_back__(<()_() pat_op()>)>)*>)>)
    rule pat_choice() = _node__(PAT_CHOICE, <()()_back__(<()pat_list() _back__(<()()_back__(<()_() _tok__(SLASH, <()"/">) _() pat_list()>)>)* _back__(<()()_back__(<()_() pat_expect()>)>)?>)>)
    rule named() = _node__(NAMED, <()()_back__(<()ident() _() _tok__(EQ, <()"=">) _()>)>)
    rule decl() = _node__(DECL, <()()_back__(<()named() pat_choice()>)>)
    rule export() = _node__(EXPORT, <()()_back__(<()_back__(<()()_back__(<()named()>)>)? ident()>)>)
    rule export_list() = _node__(EXPORT_LIST, <()()_back__(<()_tok__(EXPORTS_KW, <()"exports">) _() _tok__(L_BRACK, <()"[">) _() _back__(<()()_back__(<()export() _()>)>)* _tok__(R_BRACK, <()"]">)>)>)
    pub rule decl_list() = _node__(DECL_LIST, <()()_back__(<()_() _back__(<()()_back__(<()export_list() _()>)>)? _back__(<()()_back__(<()decl() _()>)>)+>)>)
});
#[repr(u16)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntaxKind {
    COMMENT = 0,
    WHITESPACE,
    TRIVIA,
    IDENT,
    NUMBER,
    STRING,
    MATCHES,
    LABEL,
    REPEAT_REST,
    STAR,
    REPEAT,
    PLUS,
    PAT_EXPECT,
    AT,
    PAT_ATOM,
    L_PAREN,
    R_PAREN,
    PAT_OP,
    AMP,
    BANG,
    TILDE,
    DOLLAR,
    PAT_LIST,
    PAT_CHOICE,
    SLASH,
    NAMED,
    EQ,
    DECL,
    EXPORT,
    EXPORT_LIST,
    EXPORTS_KW,
    L_BRACK,
    R_BRACK,
    DECL_LIST,
}
impl From<::rowan::SyntaxKind> for SyntaxKind { fn from(kind: ::rowan::SyntaxKind) -> Self { ::core::assert!(kind.0 <= Self::DECL_LIST as u16); unsafe { ::core::mem::transmute::<u16, SyntaxKind>(kind.0) } } }
impl From<SyntaxKind> for ::rowan::SyntaxKind { fn from(kind: SyntaxKind) -> Self { ::rowan::SyntaxKind(kind as u16) } }
decl_ast_node!(Trivia, TRIVIA);
impl Trivia {
}
decl_ast_node!(Label, LABEL);
impl Label {
    pub fn ident(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::IDENT)
    }
    pub fn string(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::STRING)
    }
}
decl_ast_node!(RepeatRest, REPEAT_REST);
impl RepeatRest {
    pub fn number(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::NUMBER)
    }
    pub fn star(&self) -> SyntaxToken {
        support::token(self.syntax(), SyntaxKind::STAR).unwrap()
    }
}
decl_ast_node!(Repeat, REPEAT);
impl Repeat {
    pub fn number(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::NUMBER)
    }
    pub fn plus(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::PLUS)
    }
    pub fn repeat_rest(&self) -> Option<RepeatRest> {
        support::child(self.syntax())
    }
}
decl_ast_node!(PatExpect, PAT_EXPECT);
impl PatExpect {
    pub fn at(&self) -> SyntaxToken {
        support::token(self.syntax(), SyntaxKind::AT).unwrap()
    }
    pub fn label(&self) -> Label {
        support::child(self.syntax()).unwrap()
    }
}
decl_ast_node!(PatAtom, PAT_ATOM);
impl PatAtom {
    pub fn ident(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::IDENT)
    }
    pub fn l_brack(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::L_BRACK)
    }
    pub fn l_paren(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::L_PAREN)
    }
    pub fn matches(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::MATCHES)
    }
    pub fn pat_choice(&self) -> Option<PatChoice> {
        support::child(self.syntax())
    }
    pub fn r_brack(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::R_BRACK)
    }
    pub fn r_paren(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::R_PAREN)
    }
    pub fn string(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::STRING)
    }
    pub fn trivia(&self) -> AstChildren<Trivia> {
        support::children(self.syntax())
    }
}
decl_ast_node!(PatOp, PAT_OP);
impl PatOp {
    pub fn amp(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::AMP)
    }
    pub fn bang(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::BANG)
    }
    pub fn dollar(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::DOLLAR)
    }
    pub fn pat_atom(&self) -> PatAtom {
        support::child(self.syntax()).unwrap()
    }
    pub fn repeat(&self) -> Option<Repeat> {
        support::child(self.syntax())
    }
    pub fn tilde(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SyntaxKind::TILDE)
    }
    pub fn trivia(&self) -> Option<Trivia> {
        support::child(self.syntax())
    }
}
decl_ast_node!(PatList, PAT_LIST);
impl PatList {
    pub fn pat_op(&self) -> AstChildren<PatOp> {
        support::children(self.syntax())
    }
    pub fn trivia(&self) -> AstChildren<Trivia> {
        support::children(self.syntax())
    }
}
decl_ast_node!(PatChoice, PAT_CHOICE);
impl PatChoice {
    pub fn pat_expect(&self) -> Option<PatExpect> {
        support::child(self.syntax())
    }
    pub fn pat_list(&self) -> AstChildren<PatList> {
        support::children(self.syntax())
    }
    pub fn trivia(&self) -> AstChildren<Trivia> {
        support::children(self.syntax())
    }
}
decl_ast_node!(Named, NAMED);
impl Named {
    pub fn eq(&self) -> SyntaxToken {
        support::token(self.syntax(), SyntaxKind::EQ).unwrap()
    }
    pub fn ident(&self) -> SyntaxToken {
        support::token(self.syntax(), SyntaxKind::IDENT).unwrap()
    }
    pub fn trivia(&self) -> AstChildren<Trivia> {
        support::children(self.syntax())
    }
}
decl_ast_node!(Decl, DECL);
impl Decl {
    pub fn named(&self) -> Named {
        support::child(self.syntax()).unwrap()
    }
    pub fn pat_choice(&self) -> PatChoice {
        support::child(self.syntax()).unwrap()
    }
}
decl_ast_node!(Export, EXPORT);
impl Export {
    pub fn ident(&self) -> SyntaxToken {
        support::token(self.syntax(), SyntaxKind::IDENT).unwrap()
    }
    pub fn named(&self) -> Option<Named> {
        support::child(self.syntax())
    }
}
decl_ast_node!(ExportList, EXPORT_LIST);
impl ExportList {
    pub fn export(&self) -> AstChildren<Export> {
        support::children(self.syntax())
    }
    pub fn exports_kw(&self) -> SyntaxToken {
        support::token(self.syntax(), SyntaxKind::EXPORTS_KW).unwrap()
    }
    pub fn l_brack(&self) -> SyntaxToken {
        support::token(self.syntax(), SyntaxKind::L_BRACK).unwrap()
    }
    pub fn r_brack(&self) -> SyntaxToken {
        support::token(self.syntax(), SyntaxKind::R_BRACK).unwrap()
    }
    pub fn trivia(&self) -> AstChildren<Trivia> {
        support::children(self.syntax())
    }
}
decl_ast_node!(DeclList, DECL_LIST);
impl DeclList {
    pub fn decl(&self) -> AstChildren<Decl> {
        support::children(self.syntax())
    }
    pub fn export_list(&self) -> Option<ExportList> {
        support::child(self.syntax())
    }
    pub fn trivia(&self) -> AstChildren<Trivia> {
        support::children(self.syntax())
    }
}

