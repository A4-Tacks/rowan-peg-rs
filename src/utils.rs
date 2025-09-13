use std::ops;
use to_true::InTrue;

pub fn rule_name_of(name: &str) -> String {
    let name = name.to_ascii_lowercase();
    if RUST_KEYWORDS.contains(&&*name) {
        format!("{name}_")
    } else {
        name.replace('-', "_")
    }
}

pub fn kind_name_of(name: &str) -> String {
    name.trim_end_matches(['_', '-']).to_ascii_uppercase()
}

pub fn node_name_of(name: &str) -> String {
    to_upper_camel(name.trim_end_matches('_'), true)
}

pub fn punct_name_of(punct: &str) -> Option<String> {
    PUNCT_NAMES.iter()
        .find(|(it, _)| punct == *it)?
        .1.to_owned()
        .into()
}

pub fn to_upper_camel(s: &str, mut upper: bool) -> String {
    let mut buf = String::with_capacity(s.len() + s.len() / 4);
    let mut start = true;
    buf.extend(s.chars().filter_map(|ch| {
        let ch = if matches!(ch, '-' | '_') && !start {
            return upper.in_true(|| ch);
        } else if upper {
            upper = false;
            ch.to_ascii_uppercase()
        } else {
            ch.to_ascii_lowercase()
        };
        start = false;
        Some(ch)
    }));
    buf
}
#[test]
fn test_to_upper_camel() {
    assert_eq!("AbcDef", to_upper_camel("abc_def", true));
    assert_eq!("abcDef", to_upper_camel("abc_def", false));
    assert_eq!("_abcDef", to_upper_camel("_abc_def", false));
    assert_eq!("Abc_Def", to_upper_camel("abc__def", true));
}

pub fn one_elem<I: IntoIterator>(iter: I) -> Option<I::Item> {
    let mut iter = iter.into_iter();
    let first = iter.next()?;
    iter.next().is_none().then_some(first)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct UsedBound(pub u8, pub u8);

impl ops::MulAssign for UsedBound {
    fn mul_assign(&mut self, rhs: Self) {
        *self = *self * rhs;
    }
}

impl ops::AddAssign for UsedBound {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

impl ops::Add for UsedBound {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0.saturating_add(rhs.0), self.1.saturating_add(rhs.1))
    }
}

impl ops::Mul for UsedBound {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0.saturating_mul(rhs.0), self.1.saturating_mul(rhs.1))
    }
}

impl ops::AddAssign<u8> for UsedBound {
    fn add_assign(&mut self, rhs: u8) {
        *self = *self + rhs;
    }
}

impl ops::Add<u8> for UsedBound {
    type Output = Self;

    fn add(self, rhs: u8) -> Self::Output {
        Self(self.0.saturating_add(rhs), self.1.saturating_add(rhs))
    }
}

impl ops::MulAssign<u8> for UsedBound {
    fn mul_assign(&mut self, rhs: u8) {
        *self = *self * rhs;
    }
}

impl ops::Mul<u8> for UsedBound {
    type Output = Self;

    fn mul(self, rhs: u8) -> Self::Output {
        Self(self.0.saturating_mul(rhs), self.1.saturating_mul(rhs))
    }
}

impl UsedBound {
    pub fn cover(self, other: Self) -> Self {
        let min = self.0.min(other.0);
        let max = self.1.max(other.1);
        Self(min, max)
    }
}

const RUST_KEYWORDS: &[&str] = &[
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
    "rule",
];
const PUNCT_NAMES: &[(&str, &str)] = &[
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
    ("\\", "backslash"),
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
    ("'", "single_quote"),
    ("\"", "double_quote"),
];
