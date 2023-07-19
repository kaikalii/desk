use crate::lex::{Span, Token};

pub enum ParseErrorKind {}

pub struct ParseError<'i> {
    pub kind: ParseErrorKind,
    pub span: Span<'i>,
}

pub struct Ast<'i> {
    pub items: Vec<Item<'i>>,
    pub errors: Vec<ParseError<'i>>,
}

pub enum Item<'i> {
    Field(Field<'i>),
}

pub struct Field<'i> {
    pub name: &'i str,
    pub ty: Type<'i>,
}

pub enum Type<'i> {
    Named(&'i str),
    Slice(Box<Self>),
    Array(Box<Self>, usize),
}

pub enum Expr<'i> {
    Ident(&'i str),
    TypedLen(&'i str),
    RawLen(&'i str),
    End(&'i str),
    Number(f64),
    BinOp(BinOp, Box<Self>, Box<Self>),
    Paren(Box<Self>),
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinOp {
    pub fn token<'i>(&self) -> Token<'i> {
        match self {
            BinOp::Add => Token::Plus,
            BinOp::Sub => Token::Minus,
            BinOp::Mul => Token::Star,
            BinOp::Div => Token::Slash,
        }
    }
}
