// The MIT License (MIT)
//
// Copyright (c) 2015 mk2
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

//! Expr struct define module

use lex::token::Kind;

/// Expr is used for representing expression
#[derive(Debug, Clone)]
pub enum Expr {
    Desc(Box<Expr>),
    EmptyExpr,
    NilExpr,
    TrueExpr,
    FalseExpr,
    NumericExpr(String),
    StringExpr(String),
    BinOpExpr{ op: Kind, lhs: Box<Expr>, rhs: Box<Expr> },
    UnaOpExpr(Kind, Box<Expr>),
    TableConsExpr,
}

impl Expr {

    pub fn new() -> Expr {

        Expr::Desc(Box::new(Expr::EmptyExpr))
    }

    pub fn set(&mut self, expr: & Expr) {

        match *self {

            Expr::Desc(ref mut e) => {
                *e = Box::new(expr.clone());
            },

            Expr::UnaOpExpr(ref mut op, ref mut e) => {
                *e = Box::new(expr.clone());
            },

            Expr::BinOpExpr{ ref mut op, ref mut lhs, ref mut rhs} => {
                *rhs = Box::new(expr.clone());
            },

            _ => {},
        }
    }

    pub fn set_lhs(&mut self, expr: & Expr) {

        match *self {

            Expr::BinOpExpr{ ref mut op, ref mut lhs, ref mut rhs} => {
                *lhs = Box::new(expr.clone());
            },

            _ => {},
        }
    }

    pub fn set_rhs(&mut self, expr: & Expr) {

        match *self {

            Expr::BinOpExpr{ ref mut op, ref mut lhs, ref mut rhs} => {
                *rhs = Box::new(expr.clone());
            },

            _ => {},
        }
    }

}
