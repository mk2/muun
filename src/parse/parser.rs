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

//! Parser module

use std::rc::{Rc, Weak};
use std::cell::RefCell;

use lex::token::{Token, Kind};
use lex::token::UNDEFINED_TOKEN;
use lex::tokenizer::Tokenizer;

use super::ast::tree::Node;
use super::ast::expr::Expr;
use super::ast::stmt::Stmt;

/// Parser Result Error Enums
///

#[derive(Debug)]
pub enum ParseError<'a> {
    NotMatchedToken(&'a str),
    NotMatchedTokens(&'a str),
    NotFound(&'a str),
    NotFoundExpectedToken(&'a str),
    NotFoundExpectedTokens(&'a str),
    NotFoundStringLiteral(&'a str),
    NotFoundNumberLiteral(&'a str),
}

/// Parser State Enums
///

#[derive(Debug)]
pub enum ParserState<'a> {
    ParseNotStarted,
    ParseFinished,
    ParseErrorDetected(&'a str),
}

/// Parser Payload Enums
///

#[derive(Debug)]
pub enum ParsePayload<'a> {
    Nothing,
    WithMessage(&'a str),
    Expr(Expr),
    Kind(Kind),
}

/// Parser Result
///

type ParseResult<'a> = Result<ParsePayload<'a>, ParseError<'a>>;

/// Macros
///

#[macro_export]
macro_rules! check_parser {
    ($myself:expr) => {
        match $myself.state {
            ParserState::ParseErrorDetected(_) => {
                return;
            },
            _ => {}
        }
    }
}

macro_rules! try_parse {
    ($e:expr) => {
        {
            let parse_result = try!($e);
        }
    }
}

macro_rules! _assert_consume_lookahead {
    ($myself:expr,$p:path) => {
        if $myself.lookahead_token() {
            let is_match = match_token_kind!($myself.lookahead,$p);
            $myself.consume_lookahead();
            Ok(ParsePayload::Kind($myself.token.kind.clone()))
        } else {
            Err(ParseError::NotFoundExpectedToken("Not found"))
        }
    };
}

#[macro_export]
macro_rules! assert_consume_lookahead {
    ($myself:expr,$p:path) => {
        match _assert_consume_lookahead!($myself,$p) {
            ok @ Ok(_) => ok,
            err @ Err(_) => {
                return err;
            },
        }
    };
}

macro_rules! _assert_lookahead {
    ($myself:expr,$p:path) => {
        if $myself.lookahead_token() {
            let is_match = match_token_kind!($myself.lookahead,$p);
            if is_match {
                $myself.consume_lookahead();
                Ok(ParsePayload::Kind($myself.token.kind.clone()))
            } else {
                Err(ParseError::NotMatchedToken("Not matched tokens"))
            }
        } else {
            Err(ParseError::NotFoundExpectedToken("Not found"))
        }
    };
}

#[macro_export]
macro_rules! assert_lookahead {
    ($myself:expr,$p:path) => {
        match _assert_lookahead!($myself,$p) {
            ok @ Ok(_) =>  ok,
            err @ Err(_) => {
                return err;
            },
        }
    };
}

#[macro_export]
macro_rules! assert_all_lookaheads {
    ($myself:expr,$($p:path),+) => {
        $(
            match _assert_lookahead!($myself,$p) {
                err @ Err(_) => {
                    return err;
                },
                _ => {} // do nothing
            }
        )+
    }
}

#[macro_export]
macro_rules! assert_any_lookaheads {
    ($myself:expr,$($p:path),+) => {
        $(
            match _assert_lookahead!($myself,$p) {
                ok @ Ok(_) => {
                    return ok;
                },
                _ => {} // do nothing
            }
        )+
    }
}

#[macro_export]
macro_rules! partial_consume_match {
    ($myself:expr,{$($p:pat => $s:stmt),+; $fs:stmt}) => {
        match $myself.lookahead {
            $(
                $p => {
                    $myself.consume_lookahead();
                    $s;
                },
            )+
            _ => {
                println!("no matching");
                $fs;
            },
        }
    };
}

/// Parser struct
/// The struct contains:
///  - state : indicate parser state
///  - tokenizer
///  - token : consumed token
///  - lookahead : currently focused token
///  - expr : abstract syntax tree for storing temporary expressions
pub struct Parser<'a> {
    state     : ParserState<'a>,
    tokenizer : Tokenizer<'a>,
    token     : Token,
    lookahead : Token,
}

impl<'a> Parser<'a> {

    pub fn with_tokenizer(tokenizer: Tokenizer<'a>) -> Parser<'a> {

        Parser{
            state     : ParserState::ParseNotStarted,
            tokenizer : tokenizer,
            token     : UNDEFINED_TOKEN.clone(),
            lookahead : UNDEFINED_TOKEN.clone(),
        }
    }

    fn consume_lookahead(&mut self) {

        println!("the token: {:?} was consumed", self.lookahead);
        self.token = self.lookahead.clone();
        self.lookahead = UNDEFINED_TOKEN.clone();
    }

    fn lookahead_token(&mut self) -> bool {

        let mut found_lookahead = false;

        if match_token_kind!(self.lookahead, Kind::Undefined) {

            if let Some(result) = self.tokenizer.next() {

                println!("next token: {:?}", result);

                match result {

                    Ok(ref token) => {
                        self.lookahead = (*token).clone();
                        found_lookahead = true;
                    },

                    Err(err_str) => {
                        println!("Error: {}", err_str);
                    },
                }
            }

        } else {
            found_lookahead = true;
        }

        return found_lookahead;
    }

    fn parse_block(&mut self) -> ParseResult<'a> {

        while let Ok(_) = assert_consume_lookahead!(self, Kind::End) {
            println!("lookahead: {:?}", self.token);
        }

        Ok(ParsePayload::Nothing)
    }

    fn parse_stmt(&mut self) -> ParseResult<'a> {

        if self.lookahead_token() {
            partial_consume_match!(self, {
                Token{kind:Kind::Fun,value:_,place:_} => self.parse_fun_stmt();
                println!("no_match")
            })
        }

        Ok(ParsePayload::Nothing)
    }

    fn parse_fun_stmt(&mut self) -> ParseResult<'a> {

        self.parse_funname();
        self.parse_funbody();

        Ok(ParsePayload::Nothing)
    }

    fn parse_funname(&mut self) -> ParseResult<'a> {

        while self.lookahead_token() {

            partial_consume_match!(self, {
                Token{kind:Kind::Ident,value:Some(_),place:_} => println!("token: {:?}", self.token),
                Token{kind:Kind::Sdot,value:_,place:_} => {
                    println!("sdot");
                    continue;
                },
                Token{kind:Kind::Dcolon,value:_,place:_} => continue;
                break
            })
        }

        Ok(ParsePayload::Nothing)
    }

    fn parse_funbody(&mut self) -> ParseResult<'a> {

        assert_all_lookaheads!(self, Kind::Lpar, Kind::Rpar);
        try!(self.parse_block());
        assert_lookahead!(self, Kind::End);

        Ok(ParsePayload::Nothing)
    }

    fn parse_namelist(&mut self) -> ParseResult<'a> {

        Ok(ParsePayload::Nothing)
    }

    fn parse_chunk_stmt(&mut self) -> ParseResult<'a> {

        Ok(ParsePayload::Nothing)
    }

    fn parse_name(&mut self) -> ParseResult<'a> {

        assert_lookahead!(self, Kind::Ident);
        Ok(ParsePayload::Nothing)
    }

    /// Expr parse functions
    ///
    ///

    /// parse_expr is entry pointo to parse expression
    fn parse_expr(&mut self) -> ParseResult<'a> {

        let mut expr = Expr::new();

        try!(self.parse_sub_expr(&mut expr));

        Ok(ParsePayload::Expr(expr))
    }

    fn parse_sub_expr(&mut self, expr: &mut Expr) -> ParseResult<'a> {

        let mut sub_expr: Expr;

        let mut first_expr: Expr;

        if let Ok(ParsePayload::Kind(ref kind)) = self.parse_unaop_expr() {
            first_expr = Expr::UnaOpExpr(kind.clone(), Box::new(Expr::EmptyExpr));
            try!(self.parse_sub_expr(&mut first_expr));
            sub_expr = first_expr;
        } else {
            first_expr = match try!(self.parse_simple_expr()) {
                ParsePayload::Expr(expr) => expr,
                _ => Expr::EmptyExpr,
            };
            sub_expr = first_expr;
        }

        while let Ok(ParsePayload::Kind(ref kind)) = self.parse_binop_expr() {
            let mut binaop_expr = Expr::BinOpExpr{ op:kind.clone(), lhs:Box::new(sub_expr.clone()), rhs:Box::new(Expr::EmptyExpr) };
            try!(self.parse_sub_expr(&mut binaop_expr));
            sub_expr = binaop_expr;
        }

        expr.set(&sub_expr);
        Ok(ParsePayload::Nothing)
    }

    fn parse_primary_expr(&mut self) -> ParseResult<'a> {

        if let Ok(_) = assert_lookahead!(self, Kind::Lpar) {
            try!(self.parse_expr());
            assert_lookahead!(self, Kind::Rpar);
            Ok(ParsePayload::Nothing)
        } else {
            try!(self.parse_name());
            Ok(ParsePayload::Nothing)
        }
    }

    fn parse_suffixed_expr(&mut self) -> ParseResult<'a> {

        Ok(ParsePayload::Nothing)
    }

    fn parse_simple_expr(&mut self) -> ParseResult<'a> {

        partial_consume_match!(self, {

            Token{kind:Kind::Nlit,value:Some(_),place:_} => {
                if let Some(v) = self.token.value.as_ref() {
                    let expr = Expr::NumericExpr(v.clone());
                    return Ok(ParsePayload::Expr(expr));
                } else {
                    return Err(ParseError::NotFoundNumberLiteral(""));
                }
            },

            Token{kind:Kind::Slit,value:Some(_),place:_} => {
                if let Some(v) = self.token.value.as_ref() {
                    let expr = Expr::StringExpr(v.clone());
                    return Ok(ParsePayload::Expr(expr));
                } else {
                    return Err(ParseError::NotFoundStringLiteral(""));
                }
            },

            Token{kind:Kind::Nil,value:Some(_),place:_} => {
                let expr = Expr::NilExpr;
                return Ok(ParsePayload::Expr(expr));
            },

            Token{kind:Kind::True,value:Some(_),place:_} => {
                let expr = Expr::NilExpr;
                return Ok(ParsePayload::Expr(expr));
            };

            try!(self.parse_suffixed_expr())
        });

        Ok(ParsePayload::Nothing)
    }

    fn parse_unaop_expr(&mut self) -> ParseResult<'a> {

        assert_any_lookaheads!(self, Kind::Minus, Kind::Not, Kind::ArrLen);

        Err(ParseError::NotFoundExpectedTokens("unary operators"))
    }

    fn parse_binop_expr(&mut self) -> ParseResult<'a> {

        assert_any_lookaheads!(self,
            Kind::Plus, Kind::Minus, Kind::Multi, Kind::Divide, Kind::Remain, Kind::Pow, Kind::And, Kind::Or,
            Kind::NotEqual, Kind::GreaterEqual, Kind::Greater, Kind::LessEqual, Kind::Less);

        Err(ParseError::NotFoundExpectedTokens("binary operators"))
    }

}

#[cfg(test)]
mod tests {
    use super::Parser;
    use lex::tokenizer::Tokenizer;

    fn new_parser(s: &str) -> Parser {

        Parser::with_tokenizer(Tokenizer::from_str(s))
    }

    //#[test]
    fn test_fun_stmt() {

        let mut parser = new_parser("fun a.test() aaaa end");
        parser.parse_stmt();
    }

    #[test]
    fn test_fun_expr() {

        let mut parser = new_parser("true + `test`");
        let expr = parser.parse_expr();

        println!("expr: {:?}", expr);
    }
}
