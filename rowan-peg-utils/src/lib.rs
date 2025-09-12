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
