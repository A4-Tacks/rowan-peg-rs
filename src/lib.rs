use core::fmt;
use std::{fmt::Display, mem};
use linked_hash_map::LinkedHashMap as HashMap;

use rowan::{ast::{support, AstChildren, AstNode}, Language, NodeOrToken, SyntaxKind};
use rowan_peg_utils as utils;
use to_true::{InTrue, ToTrue};
use unicode_ident::is_xid_continue;

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

    LABEL,
    REPEAT,
    REPEAT_REST,
    PAT_EXPECTED,
    PATATOM,
    PATOP,
    PATLIST,
    PATCHOICE,
    DECL,
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

peg::parser!(pub grammar parser<'b>(state: &'b utils::ParseState<'input>) for str {
    use Kind::*;

    rule guard(k: Kind) -> utils::RuleGuard<'b, 'input> = { state.guard(k) }
    rule guard_none() -> utils::RuleGuard<'b, 'input> = { state.guard_none() }
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
    rule decl()
        = node(DECL, <()ident() _ tok(EQ, <"=">) _ patchoice()>)
    pub
    rule decl_list()
        = node(DECL_LIST, <()_ decl() back(<_ decl()>)* _>)
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
decl_ast_node!(Match, MATCHES);
decl_ast_node!(Label, LABEL);
decl_ast_node!(RepeatRest, REPEAT_REST);
decl_ast_node!(Repeat, REPEAT);
decl_ast_node!(PatExpected, PAT_EXPECTED);
decl_ast_node!(Patatom, PATATOM);
decl_ast_node!(Patop, PATOP);
decl_ast_node!(Patlist, PATLIST);
decl_ast_node!(Patchoice, PATCHOICE);
decl_ast_node!(Decl, DECL);
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
impl Decl {
    pub fn ident(&self) -> SyntaxToken {
        support::token(self.syntax(), Kind::IDENT).unwrap()
    }

    pub fn patchoice(&self) -> Patchoice {
        support::child(self.syntax()).unwrap()
    }
}
impl DeclList {
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

const RUST_KEYWORDS: &[&str] = {&[
    "Self",
    "abstract",
    "as",
    "become",
    "box",
    "break",
    "const",
    "continue",
    "crate",
    "do",
    "else",
    "enum",
    "extern",
    "false",
    "final",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "macro",
    "match",
    "mod",
    "move",
    "mut",
    "override",
    "priv",
    "pub",
    "ref",
    "return",
    "self",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "type",
    "typeof",
    "unsafe",
    "unsized",
    "use",
    "virtual",
    "where",
    "while",
    "yield",
    "async",
    "await",
    "dyn",
    "gen",
    "try",
]};
const PUNCT_NAMES: &[(&str, &str)] = {&[
    ("$", "dollar"),
    (";", "semicolon"),
    (",", "comma"),
    ("(", "l_paren"),
    (")", "r_paren"),
    ("{", "l_curly"),
    ("}", "r_curly"),
    ("[", "l_brack"),
    ("]", "r_brack"),
    ("<", "l_angle"),
    (">", "r_angle"),
    ("@", "at"),
    ("#", "pound"),
    ("~", "tilde"),
    ("?", "question"),
    ("&", "amp"),
    ("|", "pipe"),
    ("+", "plus"),
    ("*", "star"),
    ("/", "slash"),
    ("^", "caret"),
    ("%", "percent"),
    ("_", "underscore"),
    (".", "dot"),
    ("..", "dot2"),
    ("...", "dot3"),
    ("..=", "dot2eq"),
    (":", "colon"),
    ("::", "colon2"),
    ("=", "eq"),
    ("==", "eq2"),
    ("===", "eq3"),
    ("=>", "fat_arrow"),
    ("!", "bang"),
    ("!=", "neq"),
    ("!==", "neq2"),
    ("-", "minus"),
    ("->", "thin_arrow"),
    ("<=", "lteq"),
    (">=", "gteq"),
    ("+=", "pluseq"),
    ("-=", "minuseq"),
    ("|=", "pipeeq"),
    ("&=", "ampeq"),
    ("^=", "careteq"),
    ("/=", "slasheq"),
    ("*=", "stareq"),
    ("%=", "percenteq"),
    ("&&", "amp2"),
    ("||", "pipe2"),
    ("<<", "shl"),
    (">>", "shr"),
    ("<<=", "shleq"),
    (">>=", "shreq"),
]};

#[derive(Debug)]
pub enum Error {
    EmptyLiteral(SyntaxToken),
    UnknownPunct(SyntaxToken),
    MatchesWithoutSlice(SyntaxToken),
    DisallowedSlice(SyntaxNode),
}
type Result<T, E = Error> = core::result::Result<T, E>;

pub struct Processor<W: fmt::Write> {
    out: W,
    names_map: HashMap<String, String>,
    slice: u32,
}

impl<W: fmt::Write> From<W> for Processor<W> {
    fn from(out: W) -> Self {
        Self {
            out,
            names_map: HashMap::new(),
            slice: 0,
        }
    }
}

const PRE_DEFINE_ITEMS: &str = {
r#"macro_rules! classes {
    ($($pat:tt)*) => {
        |s, i| ::char_classes::FirstElem::first_elem(&s[i..])
            .filter(::char_classes::any!($($pat)*))
            .map_or(::peg::RuleResult::Failed, |ch| {
                ::peg::RuleResult::Matched(i+ch.len_utf8(), ())
            })
    };
}
"#};
const PRE_DEFINE_RULES: &str = {
r#"
    rule _back__(r: rule<()>) = g:({ state.guard_none() }) r() { g.accept_none() }
    rule _opt__(r: rule<()>) = _back__(<r()>)?
    rule _quiet__(r: rule<()>) = g:({state.quiet().guard_none()}) quiet!{r()} { g.accept_none() }
    rule _tok__(k: Kind, r: rule<()>) = (g:({state.quiet().guard_token(k)}) s:$(quiet!{r()}) { g.accept_token(s) })
    rule _node__(k: Kind, r: rule<()>) = (g:({state.guard(k)}) r() { g.accept() })
"#};

impl<W: fmt::Write> Processor<W> {
    fn regist_name(&mut self, name: &str) -> (String, String) {
        let name = if RUST_KEYWORDS.contains(&name) {
            format!("{name}_")
        } else {
            name.replace('-', "_")
        };
        let kind_name = self.names_map.entry(name.to_owned())
            .or_insert_with(|| name.to_ascii_uppercase());
        (name, kind_name.clone())
    }

    fn regist_tok_name(&mut self, token: &SyntaxToken) -> Result<(String, String)> {
        let content = if token.kind() == Kind::STRING { value::string(token) } else { value::matches(token) };
        if content.is_empty() {
            return Err(Error::EmptyLiteral(token.clone()));
        }
        let (name, kind_name) = if content.chars()
            .all(|ch| matches!(ch, '-' | '_') || is_xid_continue(ch))
        {
            let name = content.replace('-', "_")+"_kw";
            let kind_name = name.to_ascii_uppercase();
            (name, kind_name)
        } else if let Some(&(_, name)) = PUNCT_NAMES.iter()
            .find(|(punct, _)| content == *punct)
        {
            (name.to_owned(), name.to_ascii_uppercase())
        } else {
            return Err(Error::UnknownPunct(token.to_owned()));
        };

        self.names_map.insert(name.clone(), kind_name.clone());

        Ok((name, kind_name))
    }

    pub fn start_process(&mut self, decl_list: &DeclList) -> Result<()> {
        writeln!(self.out, "{PRE_DEFINE_ITEMS}").unwrap();
        writeln!(self.out, "pub use parser::*;").unwrap();
        writeln!(self.out, "::peg::parser!(grammar parser<'b>(state: \
            &'b ::rowan_peg_utils::ParseState<'input>) for str {{").unwrap();
        writeln!(self.out, "    use Kind::*;").unwrap();
        writeln!(self.out, "{PRE_DEFINE_RULES}").unwrap();
        for decl in decl_list.decls() {
            self.process_decl(decl)?;
        }
        writeln!(self.out, "}});").unwrap();
        writeln!(self.out, "#[repr(u16)]").unwrap();
        writeln!(self.out, "#[allow(non_camel_case_types)]").unwrap();
        writeln!(self.out, "#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]").unwrap();
        writeln!(self.out, "pub enum Kind {{").unwrap();
        let mut first = true;
        let mut last = None;
        for kind in self.names_map.values() {
            first.to_false(|| {
                writeln!(self.out, "    {kind} = 0,").unwrap();
            }).unwrap_or_else(|| {
                writeln!(self.out, "    {kind},").unwrap();
            });
            last = kind.into();
        }
        writeln!(self.out, "}}").unwrap();
        writeln!(self.out, "impl From<::rowan::SyntaxKind> for Kind {{ \
            fn from(kind: ::rowan::SyntaxKind) -> Self {{ \
                ::core::assert!(kind.0 <= Self::{} as u16); \
                unsafe {{ ::core::mem::transmute::<u16, Kind>(kind.0) }} \
            }} \
        }}", last.unwrap()).unwrap();
        writeln!(self.out, "impl From<Kind> for ::rowan::SyntaxKind {{ \
            fn from(kind: Kind) -> Self {{ \
                ::rowan::SyntaxKind(kind as u16) \
            }} \
        }}").unwrap();
        Ok(())
    }

    fn process_decl(&mut self, decl: Decl) -> Result<()> {
        let (name, kind_name) = self.regist_name(decl.ident().text());
        write!(self.out, "    rule {name}() = _node__({kind_name}, <()").unwrap();
        self.process_pat_choice(decl.patchoice())?;
        writeln!(self.out, ">)").unwrap();
        Ok(())
    }

    fn process_pat_choice(&mut self, patchoice: Patchoice) -> Result<()> {
        let mut first = true;
        for patlist in patchoice.patlists() {
            first.in_false(|| write!(self.out, " / ").unwrap());
            write!(self.out, "()_back__(<()").unwrap();
            self.process_pat_list(patlist)?;
            write!(self.out, ">)").unwrap();
        }
        if let Some(expected) = patchoice.pat_expected() {
            let name = value::label(&expected.label());
            write!(self.out, " / expected!({name:?})").unwrap();
        }
        Ok(())
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
            self.process_patatom(atom)?;
            write!(self.out, ">)").unwrap();
        } else if patop.bang().is_some() {
            write!(self.out, "!_quiet__(<()").unwrap();
            self.process_patatom(atom)?;
            write!(self.out, ">)").unwrap();
        } else if patop.tilde().is_some() {
            write!(self.out, "quiet!{{").unwrap();
            self.process_patatom(atom)?;
            write!(self.out, "}}").unwrap();
        } else if patop.dollar().is_some() {
            if let Some(decl) = patop.syntax().ancestors().skip(1).find_map(Decl::cast)
                && decl.patchoice().patlists().count() == 1
                && decl.patchoice().patlists().next().unwrap().syntax().text_range()
                    == patop.syntax().text_range()
            {
                let (_, kind_name) = self.regist_name(decl.ident().text());
                write!(self.out, "_tok__({kind_name}, <()").unwrap();
                self.slice += 1;
                self.process_patatom(atom)?;
                self.slice -= 1;
                write!(self.out, ">)").unwrap();
            } else {
                return Err(Error::DisallowedSlice(patop.syntax().clone()));
            }
        } else if let Some(repeat) = patop.repeat() {
            write!(self.out, "_back__(<()").unwrap();
            self.process_patatom(atom)?;
            if repeat.plus().is_some() {
                write!(self.out, ">)+").unwrap();
            } else if let Some(rest) = repeat.repeat_rest() {
                let lower_bound = repeat.number().as_ref().map_or(0, value::number);
                if let Some(upper_bound) = rest.number() {
                    write!(self.out, ">)*<{lower_bound},{upper_bound}>").unwrap();
                } else if repeat.number().is_some() {
                    write!(self.out, ">)*<{lower_bound},>").unwrap();
                } else {
                    write!(self.out, ">)*").unwrap();
                }
            } else if let Some(number) = repeat.number() {
                write!(self.out, ">)*<{number}>").unwrap();
            } else {
                unreachable!()
            }
        } else {
            self.process_patatom(atom)?;
        }
        Ok(())
    }

    fn process_patatom(&mut self, atom: Patatom) -> Result<()> {
        if atom.l_paren().is_some() {
            self.process_pat_choice(atom.patchoice().unwrap())?;
        } else if atom.l_brack().is_some() {
            write!(self.out, "_opt__(<()").unwrap();
            self.process_pat_choice(atom.patchoice().unwrap())?;
            write!(self.out, ">)").unwrap();
        } else if let Some(ident) = atom.ident() {
            write!(self.out, "{}()", ident.text().replace('-', "_")).unwrap();
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
            let (_, kind_name) = self.regist_tok_name(&token)?;
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
        let mut state = utils::ParseState::default();
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
