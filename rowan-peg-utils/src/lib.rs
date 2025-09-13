use std::{cell::{Cell, RefCell}, mem::{forget, take}};

use rowan::{GreenNode, GreenNodeBuilder, SyntaxKind};

#[derive(Debug)]
pub struct QuietGuard<'b, 'a>(&'b ParseState<'a>);

impl<'b, 'a> std::ops::Deref for QuietGuard<'b, 'a> {
    type Target = &'b ParseState<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'b, 'a> Drop for QuietGuard<'b, 'a> {
    fn drop(&mut self) {
        self.0.quiet.update(|n| n+1);
    }
}

#[derive(Debug)]
pub struct RuleGuard<'b, 'a> {
    state: &'b ParseState<'a>,
    len: usize,
    quiet: u32,
    kind: SyntaxKind,
}

impl<'b, 'a> RuleGuard<'b, 'a> {
    fn end_set(&self) {
        self.state.quiet.set(self.quiet);
    }

    pub fn accept_token(self, text: &'a str) {
        self.end_set();
        self.state.action(Action::Token(self.kind, text));
        forget(self);
    }

    pub fn accept_none(self) {
        self.end_set();
        forget(self);
    }

    pub fn accept(self) {
        self.end_set();
        self.state.action(Action::Finish);
        forget(self);
    }
}

impl<'b, 'a> Drop for RuleGuard<'b, 'a> {
    fn drop(&mut self) {
        let RuleGuard { state, len, quiet, kind: _ } = *self;
        state.actions.borrow_mut().truncate(len);
        state.quiet.set(quiet);
    }
}

#[derive(Debug, Clone)]
pub enum Action<'a> {
    Token(SyntaxKind, &'a str),
    Start(SyntaxKind),
    Finish,
}

#[derive(Debug, Default)]
pub struct ParseState<'a> {
    actions: RefCell<Vec<Action<'a>>>,
    quiet: Cell<u32>,
    builder: GreenNodeBuilder<'static>,
}

impl<'a> ParseState<'a> {
    pub fn clear(&mut self) {
        let mut actions = take(&mut self.actions);
        actions.get_mut().clear();
        *self = Self {
            actions,
            quiet: 0.into(),
            ..Default::default()
        }
    }

    pub fn action(&self, action: Action<'a>) {
        if self.quiet.get() == 0 {
            self.actions.borrow_mut().push(action);
        }
    }

    pub fn quiet(&self) -> QuietGuard<'_, 'a> {
        QuietGuard(self)
    }

    pub fn guard(&self, kind: impl Into<SyntaxKind>) -> RuleGuard<'_, 'a> {
        let kind = kind.into();
        let guard = self.guard_token(kind);
        self.action(Action::Start(kind));
        guard
    }

    pub fn guard_token(&self, kind: impl Into<SyntaxKind>) -> RuleGuard<'_, 'a> {
        let len = self.actions.borrow().len();
        RuleGuard { state: self, len, quiet: self.quiet.get(), kind: kind.into() }
    }

    pub fn guard_none(&self) -> RuleGuard<'_, 'a> {
        let kind = SyntaxKind(0);
        self.guard_token(kind)
    }

    pub fn finish(&mut self) -> GreenNode {
        for action in self.actions.get_mut().drain(..) {
            match action {
                Action::Token(kind, text) => {
                    if !text.is_empty() {
                        self.builder.token(kind, text)
                    }
                }
                Action::Start(kind) => self.builder.start_node(kind),
                Action::Finish => self.builder.finish_node(),
            }
        }
        take(&mut self.builder).finish()
    }
}

/// Expand into a let-chain matching option chain
///
/// # Roughly de-sugar
///
/// - `foo.bar.baz` -> `let Some(baz) = value.foo().bar().baz()`
/// - `foo.bar.baz as x` -> `let Some(x) = value.foo().bar().baz()`
/// - `foo.bar { a, b.c }` -> `let Some(bar) = value.foo().bar()
///   && let Some(a) = bar.a() && let Some(c) = bar.b().c()`
///
/// # Examples
///
/// ```ignore
/// match_options! {match atom {
///     l_paren as _ => {},
///     l_brack as _ => {
///         // ...
///     },
///     ident => ident.text(),
///     string => {},
///     matches if matches.text() == "<>" => {},
///     matches => {},
///     _ => unreachable!(),
/// }}
/// ```
#[macro_export]
macro_rules! match_options {
    (@arms($expr:tt)
        $($level1:ident).+ $(as $name1:tt)?
        $({$(
            $($level2:ident).+ $(as $name2:tt)?
            $({$(
                $($level3:ident).+ $(as $name3:tt)?
                $({$(
                    $($level4:ident).+ $(as $name4:tt)?
                ),+ $(,)?})?
            ),+ $(,)?})?
        ),+ $(,)?})?
        $(if $guard:expr)?
        => $code:expr
        $(, $($t:tt)*)?
    ) => {
        if let ::core::option::Option::Some(__level1) = $expr $(.$level1())+
            $($(
                && let ::core::option::Option::Some(__level2) = __level1 $(.$level2())+
                $($(
                    && let ::core::option::Option::Some(__level3) = __level2 $(.$level3())+
                    $($(
                        && let ::core::option::Option::Some($crate::match_options! { @last$(($name4))? $($level4)+ }) = __level3 $(.$level4())+
                    )+)?
                    && let $crate::match_options! { @last$(($name3))? $($level3)+ } = __level3
                )+)?
                && let $crate::match_options! { @last$(($name2))? $($level2)+ } = __level2
            )+)?
            && let $crate::match_options! { @last$(($name1))? $($level1)+ } = __level1
            $(&& $guard)?
        {
            $code
        } $(else {
            $crate::match_options! {
                @arms($expr) $($t)*
            }
        })?
    };
    (@last $i1:ident) => { $i1 };
    (@last $i1:ident $i2:ident) => { $i2 };
    (@last $i1:ident $i2:ident $i3:ident) => { $i3 };
    (@last $i1:ident $i2:ident $i3:ident $i4:ident) => { $i4 };
    (@last($renamed:pat) $($_:tt)*) => { $renamed };
    (@arms($expr:tt)) => {};
    (@arms($expr:tt) _ => $e:expr $(,)?) => {$e};

    (match $expr:tt {$($t:tt)+}) => {{
        #[allow(irrefutable_let_patterns)]
        {
            $crate::match_options! {
                @arms($expr) $($t)*
            }
        }
    }};

    // partial completion branch
    ($i:ident $expr:tt) => { $i $expr };
    (match $expr:tt {}) => { match $expr };
}

#[test]
#[allow(unused)]
fn feature() {
    struct Foo;
    struct Bar;
    struct Baz;
    impl Foo { fn bar(&self) -> Bar { Bar } }
    impl Bar { fn baz(&self) -> Option<Baz> { Some(Baz) } }
    impl Baz { fn foo(&self) -> Option<Foo> { Some(Foo) } }
    impl Baz { fn bar(&self) -> Option<Bar> { Some(Bar) } }
    match_options!{match Foo {
        bar.baz as m { foo as _, bar as _ } => {}
    }};
}
