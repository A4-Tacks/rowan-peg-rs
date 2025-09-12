use core::fmt;
use std::{collections::HashSet, fmt::Display, mem::{self, take}};
use linked_hash_map::LinkedHashMap as HashMap;

use rowan::{ast::{support, AstChildren, AstNode}, Language, NodeOrToken, SyntaxKind};
use rowan_peg_utils as rpu;
use to_true::{InTrue, ToTrue};
use unicode_ident::is_xid_continue;

use crate::utils::UsedBound;

mod utils;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}
impl Language for Lang {
    type Kind = Kind;

    fn kind_to_raw(kind: Self::Kind) -> SyntaxKind {
        kind.into()
    }

    fn kind_from_raw(raw: SyntaxKind) -> Self::Kind {
        raw.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;
pub type SyntaxToken = rowan::SyntaxToken<Lang>;

#[repr(u16)]
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    COMMENT = 0,
    WHITESPACE,
    TRIVIA,

    PLUS,
    STAR,
    AMP,
    BANG,
    TILDE,
    DOLLAR,
    EQ,
    AT,
    SLASH,
    L_BRACK,
    L_PAREN,
    R_BRACK,
    R_PAREN,
    IDENT,
    NUMBER,
    STRING,
    MATCHES,
    EXPORTS_KW,

    LABEL,
    REPEAT,
    REPEAT_REST,
    PAT_EXPECTED,
    PATATOM,
    PATOP,
    PATLIST,
    PATCHOICE,
    DECL,
    NAMED,
    EXPORT,
    EXPORT_LIST,
    DECL_LIST,
}

impl From<SyntaxKind> for Kind {
    fn from(value: SyntaxKind) -> Self {
        assert!(value.0 <= Self::DECL_LIST as u16);
        unsafe { mem::transmute::<u16, Kind>(value.0) }
    }
}
impl From<Kind> for SyntaxKind {
    fn from(value: Kind) -> Self {
        SyntaxKind(value as u16)
    }
}

peg::parser!(pub grammar parser<'b>(state: &'b rpu::ParseState<'input>) for str {
    use Kind::*;

    rule guard(k: Kind) -> rpu::RuleGuard<'b, 'input> = { state.guard(k) }
    rule guard_none() -> rpu::RuleGuard<'b, 'input> = { state.guard_none() }
    rule back(r: rule<()>) = g:guard_none() r() {g.accept_none();}
    rule opt(r: rule<()>) = back(<r()>)?
    rule quiet(r: rule<()>) = g:({state.quiet().guard_none()}) quiet!{r()} {g.accept_none();}
    rule tok(k: Kind, r: rule<()>) = (
        g:({state.quiet().guard_token(k)})
        s:$(quiet!{r()})
        {
            g.accept_token(s);
        })
    rule node(k: Kind, r: rule<()>) = (g:({state.guard(k)}) r() {g.accept();})

    rule comment() = tok(COMMENT, <";" [^'\n']*>) / expected!("comment")
    rule whitespace() = tok(WHITESPACE, <()[' '|'\t'|'\r'|'\n']*>)
    rule _() = node(TRIVIA, <whitespace() back(<comment() whitespace()>)*>)
    rule ident()
        = tok(IDENT, <
            !['0'..='9'] (
                ['0'..='9' | 'a'..='z' | 'A'..='Z' | '-' | '_']
                / [^'\x00'..='\u{a0}']
            )+
        >)
        / expected!("ident")
    rule number()
        = tok(NUMBER, <()['0'..='9']+>)
        / expected!("number")
    rule string()
        = tok(STRING, <"\"" s:$([^'"' | '\r' | '\n']*) "\"">)
        / expected!("string")
    rule matches()
        = tok(MATCHES, <"<" s:$([^'>' | '\r' | '\n']*) ">">) / expected!("string")
    rule label()
        = node(LABEL, <()ident()>)
        / node(LABEL, <()string()>)
    rule repeat_rest()
        = node(REPEAT_REST, <tok(STAR, <"*">) number()?>)
    rule repeat()
        = node(REPEAT, <()tok(PLUS, <"+">)>)
        / node(REPEAT, <()number() repeat_rest()?>)
        / node(REPEAT, <()repeat_rest()>)
    rule pat_expected()
        = node(PAT_EXPECTED, <tok(AT, <"@">) label()>)

    rule patatom()
        = node(PATATOM, <()ident() !quiet(<_ "=">)>)
        / node(PATATOM, <()string() >)
        / node(PATATOM, <()matches()>)
        / node(PATATOM, <()tok(L_BRACK, <"[">) _ patchoice() _ tok(R_BRACK, <"]">)>)
        / node(PATATOM, <()tok(L_PAREN, <"(">) _ patchoice() _ tok(R_PAREN, <")">)>)
    rule patop()
        = node(PATOP, <()tok(AMP,    <"&">) patatom()>)
        / node(PATOP, <()tok(BANG,   <"!">) patatom()>)
        / node(PATOP, <()tok(TILDE,  <"~">) patatom()>)
        / node(PATOP, <()tok(DOLLAR, <"$">) patatom()>)
        / node(PATOP, <()repeat() _         patatom()>)
        / node(PATOP, <()patatom()>)
    rule patlist()
        = node(PATLIST, <()patop() back(<_ patop()>)*>)
    rule patchoice()
        = node(PATCHOICE, <()patlist() back(<_ tok(SLASH, <"/">) _ patlist()>)* opt(<_ pat_expected()>)>)
    rule named()
        = node(NAMED, <ident() _ tok(EQ, <"=">) _>)
    rule decl()
        = node(DECL, <named() patchoice()>)
    rule export() = node(EXPORT, <named()? ident()>)
    rule export_list()
        = node(EXPORT_LIST, <
            tok(EXPORTS_KW, <"exports">)
            _ tok(L_BRACK, <"[">) _ back(<export() _>)* tok(R_BRACK, <"]">)
        >)
    pub
    rule decl_list()
        = node(DECL_LIST, <_ (export_list() _)? back(<decl() _>)+>)
});

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
                kind == Kind::$kind
            }

            fn cast(node: rowan::SyntaxNode<Self::Language>) -> Option<Self> {
                if Self::can_cast(node.kind()) {
                    Some(Self(node))
                } else {
                    None
                }
            }
        }
        impl Display for $node {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                Display::fmt(self.syntax(), f)
            }
        }
    };
}
decl_ast_node!(Trivia, TRIVIA);
decl_ast_node!(Label, LABEL);
decl_ast_node!(RepeatRest, REPEAT_REST);
decl_ast_node!(Repeat, REPEAT);
decl_ast_node!(PatExpected, PAT_EXPECTED);
decl_ast_node!(Patatom, PATATOM);
decl_ast_node!(Patop, PATOP);
decl_ast_node!(Patlist, PATLIST);
decl_ast_node!(Patchoice, PATCHOICE);
decl_ast_node!(Decl, DECL);
decl_ast_node!(Named, NAMED);
decl_ast_node!(Export, EXPORT);
decl_ast_node!(ExportList, EXPORT_LIST);
decl_ast_node!(DeclList, DECL_LIST);

impl Trivia {
    pub fn comments(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .filter(|tok| tok.kind() == Kind::COMMENT)
    }

    pub fn whitespaces(&self) -> impl Iterator<Item = SyntaxToken> {
        self.syntax()
            .children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .filter(|tok| tok.kind() == Kind::WHITESPACE)
    }
}
impl Label {
    pub fn ident(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::IDENT)
    }

    pub fn string(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::STRING)
    }
}
impl RepeatRest {
    pub fn number(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::NUMBER)
    }

    pub fn star(&self) -> SyntaxToken {
        support::token(self.syntax(), Kind::STAR).unwrap()
    }
}
impl Repeat {
    pub fn plus(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::PLUS)
    }

    pub fn number(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::NUMBER)
    }

    pub fn repeat_rest(&self) -> Option<RepeatRest> {
        support::child(self.syntax())
    }

    pub fn count_bounds(&self) -> (u32, Option<u32>) {
        if self.plus().is_some() {
            (1, None)
        } else if let Some(rest) = self.repeat_rest() {
            let lower_bound = self.number().as_ref().map_or(0, value::number);
            let upper_bound = rest.number().as_ref().map(value::number);
            (lower_bound, upper_bound)
        } else if let Some(number) = self.number() {
            let bound = value::number(&number);
            (bound, bound.into())
        } else {
            unreachable!()
        }
    }
}
impl PatExpected {
    pub fn label(&self) -> Label {
        support::child(self.syntax()).unwrap()
    }
}
impl Patatom {
    pub fn ident(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::IDENT)
    }

    pub fn string(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::STRING)
    }

    pub fn matches(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::MATCHES)
    }

    pub fn l_brack(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::L_BRACK)
    }

    pub fn l_paren(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::L_PAREN)
    }

    pub fn r_brack(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::R_BRACK)
    }

    pub fn r_paren(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::R_PAREN)
    }

    pub fn patchoice(&self) -> Option<Patchoice> {
        support::child(self.syntax())
    }
}
impl Patop {
    pub fn repeat(&self) -> Option<Repeat> {
        support::child(self.syntax())
    }

    pub fn patatom(&self) -> Patatom {
        support::child(self.syntax()).unwrap()
    }

    pub fn amp(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::AMP)
    }

    pub fn bang(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::BANG)
    }

    pub fn tilde(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::TILDE)
    }

    pub fn dollar(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), Kind::DOLLAR)
    }
}
impl Patlist {
    pub fn patops(&self) -> AstChildren<Patop> {
        support::children(self.syntax())
    }
}
impl Patchoice {
    pub fn patlists(&self) -> AstChildren<Patlist> {
        support::children(self.syntax())
    }

    pub fn pat_expected(&self) -> Option<PatExpected> {
        support::child(self.syntax())
    }
}
impl Named {
    pub fn ident(&self) -> SyntaxToken {
        support::token(self.syntax(), Kind::IDENT).unwrap()
    }
}
impl Export {
    pub fn named(&self) -> Option<Named> {
        support::child(self.syntax())
    }

    pub fn ident(&self) -> SyntaxToken {
        support::token(self.syntax(), Kind::IDENT).unwrap()
    }
}
impl ExportList {
    pub fn exports(&self) -> AstChildren<Export> {
        support::children(self.syntax())
    }
}
impl Decl {
    pub fn named(&self) -> Named {
        support::child(self.syntax()).unwrap()
    }

    pub fn patchoice(&self) -> Patchoice {
        support::child(self.syntax()).unwrap()
    }
}
impl DeclList {
    pub fn export_list(&self) -> Option<ExportList> {
        support::child(self.syntax())
    }

    pub fn decls(&self) -> AstChildren<Decl> {
        support::children(self.syntax())
    }
}

pub mod value {
    use crate::{Kind, Label, SyntaxToken};

    #[track_caller]
    pub fn string(s: &SyntaxToken) -> &str {
        debug_assert_eq!(s.kind(), Kind::STRING);
        let s = s.text();
        &s[1..s.len()-1]
    }

    #[track_caller]
    pub fn matches(s: &SyntaxToken) -> &str {
        debug_assert_eq!(s.kind(), Kind::MATCHES);
        let s = s.text();
        &s[1..s.len()-1]
    }

    #[track_caller]
    pub fn label(l: &Label) -> String {
        l.ident()
            .map(|ident| ident.text().to_owned())
            .unwrap_or_else(|| string(&l.string().unwrap()).to_owned())
    }

    pub fn number(s: &SyntaxToken) -> u32 {
        debug_assert_eq!(s.kind(), Kind::NUMBER);
        s.text().parse().unwrap()
    }
}

#[derive(Debug)]
pub enum Error {
    EmptyLiteral(SyntaxToken),
    UnknownLiteral(SyntaxToken),
    MatchesWithoutSlice(SyntaxToken),
    DisallowedSlice(SyntaxNode),
}

type Result<T, E = Error> = core::result::Result<T, E>;

enum Method {
    Optional,
    Strict,
    Many,
}

pub struct Processor<W: fmt::Write> {
    out: W,
    kind_names_map: HashMap<String, String>,
    slice: u32,
    is_token_decl: bool,
    exports: HashMap<String, String>,
    decl_name: String,
    refs_bound: HashMap<String, UsedBound>,
    is_tokens: HashSet<String>,
    methods: HashMap<String, Vec<(String, Method)>>,
}

impl<W: fmt::Write> From<W> for Processor<W> {
    fn from(out: W) -> Self {
        Self {
            out,
            kind_names_map: HashMap::new(),
            slice: 0,
            is_token_decl: false,
            exports: HashMap::new(),
            decl_name: String::new(),
            refs_bound: HashMap::new(),
            is_tokens: HashSet::new(),
            methods: HashMap::new(),
        }
    }
}

const PRE_DEFINE_ITEMS: &str = {
r#"use rowan::{ast::{support, AstChildren, AstNode}, Language};

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
"#};
const PRE_DEFINE_RULES: &str = {
r#"
    rule _back__(r: rule<()>) = g:({ state.guard_none() }) r() { g.accept_none() }
    rule _opt__(r: rule<()>) = _back__(<r()>)?
    rule _quiet__(r: rule<()>) = g:({state.quiet().guard_none()}) quiet!{r()} { g.accept_none() }
    rule _tok__(k: SyntaxKind, r: rule<()>) = (g:({state.quiet().guard_token(k)}) s:$(quiet!{r()}) { g.accept_token(s) })
    rule _node__(k: SyntaxKind, r: rule<()>) = (g:({state.guard(k)}) r() { g.accept() })
"#};

impl<W: fmt::Write> Processor<W> {
    fn regist_name(&mut self, name: &str) -> (String, String) {
        let name = utils::rule_name_of(name);
        let kind_name = self.kind_names_map.entry(name.to_owned())
            .or_insert_with(|| utils::kind_name_of(self.exports.get(&name).unwrap_or(&name)));
        (name, kind_name.clone())
    }

    fn regist_tok_name(&mut self, token: &SyntaxToken) -> Result<(String, String)> {
        let content = if token.kind() == Kind::STRING { value::string(token) } else { value::matches(token) };
        if content.is_empty() {
            return Err(Error::EmptyLiteral(token.clone()));
        }
        let (name, kind_name) = if let Some(name) = utils::punct_name_of(content) {
            (name.to_owned(), utils::kind_name_of(&name))
        } else if content.chars()
            .all(|ch| matches!(ch, '-' | '_') || is_xid_continue(ch))
        {
            let name = utils::rule_name_of(content)+"_kw";
            let kind_name = utils::kind_name_of(&name);
            (name, kind_name)
        } else {
            return Err(Error::UnknownLiteral(token.to_owned()));
        };

        self.is_tokens.insert(name.clone());
        self.kind_names_map.insert(name.clone(), kind_name.clone());

        Ok((name, kind_name))
    }

    fn add_bound(&mut self, name: impl Into<String>) {
        if !self.is_token_decl {
            let mut name = name.into();
            if let Some(renamed_name) = self.exports.get(&name) {
                name = renamed_name.to_owned();
            }
            *self.refs_bound.entry(name).or_default() += 1;
        }
    }

    fn dis_refs_bound<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let refs_bound = self.take_refs_bound();
        let result = f(self);
        self.refs_bound = refs_bound;
        result
    }

    #[must_use]
    fn take_refs_bound(&mut self) -> HashMap<String, UsedBound> {
        take(&mut self.refs_bound)
    }

    pub fn start_process(&mut self, decl_list: &DeclList) -> Result<()> {
        for export in decl_list.export_list().iter().flat_map(|list| list.exports()) {
            let name = export.ident();
            let new_name = export.named()
                .map_or(name.clone(), |it| it.ident());
            self.exports.insert(
                utils::rule_name_of(name.text()),
                utils::rule_name_of(new_name.text()),
            );
        }

        writeln!(self.out, "{PRE_DEFINE_ITEMS}").unwrap();
        writeln!(self.out, "pub use parser::*;").unwrap();
        writeln!(self.out, "::peg::parser!(grammar parser<'b>(state: \
            &'b ::rowan_peg_utils::ParseState<'input>) for str {{").unwrap();
        writeln!(self.out, "    use SyntaxKind::*;").unwrap();
        writeln!(self.out, "{PRE_DEFINE_RULES}").unwrap();
        for decl in decl_list.decls() {
            self.process_decl(decl)?;
        }
        writeln!(self.out, "}});").unwrap();
        writeln!(self.out, "#[repr(u16)]").unwrap();
        writeln!(self.out, "#[allow(non_camel_case_types)]").unwrap();
        writeln!(self.out, "#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]").unwrap();
        writeln!(self.out, "pub enum SyntaxKind {{").unwrap();
        let mut first = true;
        let mut last = None;
        for kind in self.kind_names_map.values() {
            first.to_false(|| {
                writeln!(self.out, "    {kind} = 0,").unwrap();
            }).unwrap_or_else(|| {
                writeln!(self.out, "    {kind},").unwrap();
            });
            last = kind.into();
        }
        writeln!(self.out, "}}").unwrap();
        writeln!(self.out, "impl From<::rowan::SyntaxKind> for SyntaxKind {{ \
            fn from(kind: ::rowan::SyntaxKind) -> Self {{ \
                ::core::assert!(kind.0 <= Self::{} as u16); \
                unsafe {{ ::core::mem::transmute::<u16, SyntaxKind>(kind.0) }} \
            }} \
        }}", last.unwrap()).unwrap();
        writeln!(self.out, "impl From<SyntaxKind> for ::rowan::SyntaxKind {{ \
            fn from(kind: SyntaxKind) -> Self {{ \
                ::rowan::SyntaxKind(kind as u16) \
            }} \
        }}").unwrap();
        for (rule_name, mut methods) in self.methods.drain() {
            if self.is_tokens.contains(&rule_name) { continue }
            let node_name = utils::node_name_of(&rule_name);
            let node_kind = utils::kind_name_of(&rule_name);
            methods.sort_by(|a, b| a.0.cmp(&b.0));

            writeln!(self.out, "decl_ast_node!({node_name}, {node_kind});").unwrap();
            writeln!(self.out, "impl {node_name} {{").unwrap();
            for (method_name, method) in methods {
                let is_token = self.is_tokens.contains(&method_name);
                let mut base_ty = if is_token {
                    "SyntaxToken".to_owned()
                } else {
                    utils::node_name_of(&method_name)
                };
                match method {
                    Method::Optional => base_ty = format!("Option<{base_ty}>"),
                    Method::Strict => (),
                    Method::Many => base_ty = format!("AstChildren<{base_ty}>"),
                }
                let body = if is_token {
                    let kind = utils::kind_name_of(&method_name);
                    match method {
                        Method::Optional => format!("support::token(self.syntax(), SyntaxKind::{kind})"),
                        Method::Strict => format!("support::token(self.syntax(), SyntaxKind::{kind}).unwrap()"),
                        Method::Many => continue,
                    }
                } else {
                    match method {
                        Method::Optional => "support::child(self.syntax())",
                        Method::Strict => "support::child(self.syntax()).unwrap()",
                        Method::Many => "support::children(self.syntax())",
                    }.into()
                };
                writeln!(self.out, "    pub fn {method_name}(&self) -> {base_ty} {{").unwrap();
                writeln!(self.out, "        {body}").unwrap();
                writeln!(self.out, "    }}").unwrap();
            }
            writeln!(self.out, "}}").unwrap();
        }
        Ok(())
    }

    fn decl_is_token(&self, decl: &Decl) -> bool {
        let Some(list) = utils::one_elem(decl.patchoice().patlists()) else { return false };
        let Some(op) = utils::one_elem(list.patops()) else { return false };
        if op.dollar().is_some() {
            return true;
        }
        op.patatom().syntax().text_range() != op.syntax().text_range()
            && op.patatom().string().is_some()
    }

    fn process_decl(&mut self, decl: Decl) -> Result<()> {
        let (name, kind_name) = self.regist_name(decl.named().ident().text());
        self.is_token_decl = self.decl_is_token(&decl);
        self.decl_name = name;
        let name = &self.decl_name;
        let mut vis = "";

        if self.is_token_decl {
            self.is_tokens.insert(name.clone());
        }
        if let Some(new_name) = self.exports.get(name) {
            if name == new_name {
                vis = "pub ";
            } else {
                writeln!(self.out, "    pub rule {new_name}() = {name}").unwrap();
            }
        }
        if self.is_token_decl {
            write!(self.out, "    {vis}rule {name}() = ()").unwrap();
        } else {
            write!(self.out, "    {vis}rule {name}() = _node__({kind_name}, <()").unwrap();
        }

        self.refs_bound.clear();
        self.process_pat_choice(decl.patchoice())?;

        if !self.is_token_decl {
            write!(self.out, ">)").unwrap();
        }
        writeln!(self.out).unwrap();
        let methods = self.refs_bound.iter().filter_map(|(name, bound)| {
            let ty = match bound {
                UsedBound(0, 0) => return None,
                UsedBound(0, 1) => Method::Optional,
                UsedBound(1, 1) => Method::Strict,
                _ => Method::Many,
            };
            Some((name.clone(), ty))
        }).collect();
        let name = self.exports.get(&self.decl_name).unwrap_or(&self.decl_name);
        self.methods.insert(name.clone(), methods);
        Ok(())
    }

    fn process_pat_choice(&mut self, patchoice: Patchoice) -> Result<()> {
        let mut first = true;
        let refs_bound = self.take_refs_bound();
        let mut prev_bound: Option<HashMap<String, UsedBound>> = None;
        for patlist in patchoice.patlists() {
            first.in_false(|| write!(self.out, " / ").unwrap());
            write!(self.out, "()_back__(<()").unwrap();
            self.process_pat_list(patlist)?;
            write!(self.out, ">)").unwrap();
            if let Some(prev_bound) = &mut prev_bound {
                self.merge_cover_to(prev_bound);
            } else {
                prev_bound = Some(self.take_refs_bound());
            }
        }
        assert_eq!(self.refs_bound.len(), 0);
        self.merge_add(refs_bound);
        self.merge_add(prev_bound.unwrap());
        if let Some(expected) = patchoice.pat_expected() {
            let name = value::label(&expected.label());
            write!(self.out, " / expected!({name:?})").unwrap();
        }
        Ok(())
    }

    fn merge_cover_to(&mut self, prev_bound: &mut HashMap<String, UsedBound>) {
        for key in self.refs_bound.keys() {
            if !prev_bound.contains_key(key) {
                prev_bound.insert(key.clone(), UsedBound::default());
            }
        }
        for (key, value) in &mut *prev_bound {
            let other = self.refs_bound.get(key).copied().unwrap_or_default();
            *value = value.cover(other);
        }
        self.refs_bound.clear();
    }

    fn merge_add(&mut self, refs_bound: HashMap<String, UsedBound>) {
        for (key, value) in refs_bound {
            *self.refs_bound.entry(key).or_default() += value;
        }
    }

    fn process_pat_list(&mut self, patlist: Patlist) -> Result<()> {
        let mut first = true;
        for patop in patlist.patops() {
            first.in_false(|| write!(self.out, " ").unwrap());
            self.process_patop(patop)?;
        }
        Ok(())
    }

    fn process_patop(&mut self, patop: Patop) -> Result<()> {
        let atom = patop.patatom();
        if patop.amp().is_some() {
            write!(self.out, "&_quiet__(<()").unwrap();
            self.dis_refs_bound(|this| this.process_patatom(atom))?;
            write!(self.out, ">)").unwrap();
        } else if patop.bang().is_some() {
            write!(self.out, "!_quiet__(<()").unwrap();
            self.dis_refs_bound(|this| this.process_patatom(atom))?;
            write!(self.out, ">)").unwrap();
        } else if patop.tilde().is_some() {
            write!(self.out, "quiet!{{").unwrap();
            self.dis_refs_bound(|this| this.process_patatom(atom))?;
            write!(self.out, "}}").unwrap();
        } else if patop.dollar().is_some() {
            if self.is_token_decl && self.slice == 0 {
                let name = &self.decl_name.clone();
                let (_, kind_name) = self.regist_name(name);
                write!(self.out, "_tok__({kind_name}, <()").unwrap();
                self.slice += 1;
                self.dis_refs_bound(|this| this.process_patatom(atom))?;
                self.slice -= 1;
                write!(self.out, ">)").unwrap();
            } else {
                return Err(Error::DisallowedSlice(patop.syntax().clone()));
            }
        } else if let Some(repeat) = patop.repeat() {
            let refs_bound = self.take_refs_bound();
            write!(self.out, "_back__(<()").unwrap();
            self.process_patatom(atom)?;
            let (lower_bound, upper_bound) = repeat.count_bounds();
            match (lower_bound, upper_bound) {
                (1, None) => write!(self.out, ">)+"),
                (0, None) => write!(self.out, ">)*"),
                (lower, None) => write!(self.out, ">)*<{lower},>"),
                (lower, Some(upper)) => write!(self.out, ">)*<{lower},{upper}>"),
            }.unwrap();
            let repeat_meta = UsedBound(
                lower_bound.try_into().unwrap(),
                upper_bound.unwrap_or(255).try_into().unwrap(),
            );
            self.refs_bound.iter_mut().for_each(|(_, bound)| *bound *= repeat_meta);
            self.merge_add(refs_bound);
        } else {
            self.process_patatom(atom)?;
        }
        Ok(())
    }

    fn process_patatom(&mut self, atom: Patatom) -> Result<()> {
        if atom.l_paren().is_some() {
            self.process_pat_choice(atom.patchoice().unwrap())?;
        } else if atom.l_brack().is_some() {
            let refs_bound = self.take_refs_bound();
            write!(self.out, "_opt__(<()").unwrap();
            self.process_pat_choice(atom.patchoice().unwrap())?;
            write!(self.out, ">)").unwrap();
            self.refs_bound.iter_mut().for_each(|(_, bound)| bound.0 = 0);
            self.merge_add(refs_bound);
        } else if let Some(ident) = atom.ident() {
            let name = utils::rule_name_of(ident.text());
            write!(self.out, "{}()", name).unwrap();
            self.add_bound(name);
        } else if let Some(string) = atom.string() {
            self.tok_or_in_slice(&string)?;
        } else if let Some(matches) = atom.matches()
            && value::matches(&matches).chars().count() == 1
        {
            // special unit string
            self.tok_or_in_slice(&matches)?;
        } else if let Some(matches) = atom.matches() {
            if self.slice == 0 {
                return Err(Error::MatchesWithoutSlice(matches));
            }
            let content = value::matches(&matches);
            if content.starts_with('^') {
                write!(self.out, "#{{classes!(^\"{}\")}}", &content[1..]).unwrap();
            } else {
                write!(self.out, "#{{classes!(\"{content}\")}}").unwrap();
            }
        } else {
            unreachable!()
        }
        Ok(())
    }

    fn tok_or_in_slice(&mut self, token: &SyntaxToken) -> Result<()> {
        let content = if token.kind() == Kind::STRING {
            value::string(token)
        } else {
            value::matches(token)
        };
        if self.slice == 0 {
            let (name, kind_name) = self.regist_tok_name(&token)?;
            self.add_bound(name);
            write!(self.out, "_tok__({kind_name}, <{content:?}>)").unwrap();
        } else {
            write!(self.out, "{content:?}").unwrap();
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use rowan::TextSize;

    use super::*;

    #[test]
    fn full_parser() {
        let s = r#"
;; use ABNF like grammar
;; char-val to case-sensitive
;; prose-val -> regexp
;; add peg lookaheads `!` `&`
;; add quiet `~`
;; add slice `$`
;; remove num-var
;;
;; vim:nowrap

comment     = ~<;[^\n]*(?:\n|$)> @comment
_           = ~<[ \t\r\n]*> [comment _]
ident       = ~<(?![0-9])(?:[0-9a-zA-Z\-_]|[^\x00-\xa0])+> @ident
number      = ~<[0-9]+> @number
string      = ~(<"> <[^\"\r\n]*> <">) @string
match       = ~("<" <[^\x3e\r\n]*> ">") @match
label       = ident / string
repeat      = "+"
            / "*" [number]
            / number ["*" [number]]
patatom     = ident !(_ "=")            ; a rule reference
            / string                    ; keyword
            / match                     ; regular expressions
            / "[" _ patchoice _ "]"     ; optional
            / "(" _ patchoice _ ")"     ; simple paren
            / "{" _ patchoice _ "}"     ; list group brace
patrepeat   = repeat _ patatom
            / patatom
patop       = "&" patrepeat ; positive lookahead
            / "!" patrepeat ; negative lookahead
            / "~" patrepeat ; quiet
            / "$" patrepeat ; slice
            / patrepeat
patlist     = patop *(_ patop)
patchoice   = patlist *(_ "/" _ patlist)
              *(_ "@" label); extra expected branch
decl        = ident _ "=" _ patchoice
decl-list   = +(_ decl) _
    "#;
        let mut state = rpu::ParseState::default();
        parser::decl_list(s, &state).unwrap();
        dbg!(&state);
        let node = SyntaxNode::new_root(state.finish());
        dbg!(&node);
        assert_eq!(TextSize::of(s), node.text_range().end());
        dbg!(&s.len());
        let decl_list = DeclList::cast(node).unwrap();
        println!("{decl_list}")
    }
}
