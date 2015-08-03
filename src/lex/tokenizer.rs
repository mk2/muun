//! Tokenizer related module
//!
//!

use super::token::{Token, Kind, Place, LineNum, Pos, Length};
use std::str::Chars;

#[allow(dead_code)]
pub struct Tokenizer<'a> {
    pub lines : &'a str,
    next_char : Option<char>,
    chars     : Chars<'a>,
    linen     : LineNum,
    pos       : Pos,
    readchar  : char,
    readbuf   : String,
}

#[allow(dead_code)]
impl<'a> Tokenizer<'a> {

    pub fn from_str(raw_str: &str) -> Tokenizer {

        Tokenizer{
            lines     : raw_str,
            next_char : None,
            chars     : raw_str.chars().clone(),
            readchar  : ' ',
            readbuf   : String::new(),
            linen     : 0,
            pos       : 0,
        }
    }

    pub fn next_token(&mut self) -> Result<Token, String> {

        self.readbuf.clear();
        let mut res = Err("not found regular token".to_string());

        while let Some(c) = self.next_significant_char() {

            self.readbuf.push(c);

            match c {

                '(' | ')' | '{' | '}' | '[' | ',' | '/' | '%' | '^' | ';' | '\n' | '\r' | '#' => {
                    res = self.get_single_keyword_token();
                    break;
                },

                '.' | '-' | '+' | '*' | '=' | '<' | '>' | ']' | '!' | '?' => {
                    res = self.get_plural_keyword_token();
                    break;
                },

                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    res = self.get_number_literal();
                    break;
                },

                '"' | '`' => {
                    res = self.get_string_literal(c);
                    break;
                },

                _ => {
                    res = self.get_ident_token();
                    break;
                }
            }
        }

        res
    }

    fn next_significant_char(&mut self) -> Option<char> {

        let mut next_char = None;

        if let Some(c) = self.skip_whitespaces() {
                self.pos += 1;
                next_char = Some(c);
        }

        next_char
    }

    fn skip_whitespaces(&mut self) -> Option<char> {

        let mut last_char = None;

        while let Some(c) = self.next_char() {

            self.pos += 1;

            if c.is_whitespace() || c == '\t' {
                continue;
            } else {
                last_char = Some(c);
                break;
            }
        }

        last_char
    }

    fn next_char(&mut self) -> Option<char> {

        if let Some(c) = self.next_char {
            self.next_char = None;
            return Some(c);
        }

        self.chars.next()
    }

    fn get_ident_token(&mut self) -> Result<Token, String> {

        let lin = self.linen;
        let pos = self.pos;

        while let Some(c) = self.chars.next() {

            if !c.is_alphanumeric() {
                self.next_char = Some(c);
                break;
            }

            self.readbuf.push(c);
            continue;
        }

        let len = self.readbuf.len() as Length;

        let kind = match self.readbuf.as_ref() {

            "true" => Kind::True,

            "false" => Kind::False,

            "fun" => Kind::Fun,

            "if" => Kind::If,

            "else" => Kind::Else,

            "elseif" => Kind::ElseIf,

            "nil" => Kind::Nil,

            "do" => Kind::Do,

            "let" => Kind::Let,

            "in" => Kind::In,

            "repeat" => Kind::Repeat,

            "while" => Kind::While,

            "for" => Kind::For,

            "return" => Kind::Return,

            "break" => Kind::Break,

            "end" => Kind::End,

            "and" => Kind::And,

            "not" => Kind::Not,

            "or" => Kind::Or,

            _ => Kind::Ident,
        };

        let place = Place::with(lin, pos, len);

        if match_kind!(kind,Kind::Ident) {
            Ok(Token::with_kind_value(kind, place, self.readbuf.clone()))
        } else {
            Ok(Token::with_kind(kind, place))
        }
    }

    fn get_single_keyword_token(&mut self) -> Result<Token, String> {

        let lin = self.linen;
        let pos = self.pos;
        let len = 1;

        let kind = match self.readbuf.as_ref() {

            "(" => Kind::Lpar,
            ")" => Kind::Rpar,

            "{" => Kind::Lbrac,
            "}" => Kind::Rbrac,

            "[" => Kind::Lsqua,

            "," => Kind::Comma,

            "^" => Kind::Pow,
            "%" => Kind::Remain,
            "/" => Kind::Divide,

            "#" => Kind::ArrLen,

            "\n" => Kind::EOL,
            "\r" => Kind::EOL,
            ";" => Kind::EOL,

            _ => Kind::Unknown,
        };

        let place = Place{
            linen : lin,
            pos   : pos,
            len   : len,
        };

        if match_kind!(kind,Kind::EOL) {
            self.linen += 1;
        }

        if match_kind!(kind,Kind::Unknown) {
            Ok(Token::with_kind_value(kind, place, self.readbuf.clone()))
        } else {
            Ok(Token::with_kind(kind, place))
        }
    }

    fn get_plural_keyword_token(&mut self) -> Result<Token, String> {

        let lin = self.linen;
        let pos = self.pos;

        while let Some(c) = self.chars.next() {

            if c.is_alphanumeric() || c.is_whitespace() {
                self.next_char = Some(c);
                break;
            }

            self.readbuf.push(c);
            continue;
        }

        let len = self.readbuf.len() as Length;

        let kind = match self.readbuf.as_ref() {

            "+" => Kind::Plus,
            "-" => Kind::Minus,
            "<" => Kind::Less,
            "<=" => Kind::LessEqual,
            ">" => Kind::Greater,
            ">=" => Kind::GreaterEqual,
            "==" => Kind::Equal,
            "!=" => Kind::NotEqual,

            ":" => Kind::Dcolon,
            ":=" => Kind::Assign,
            "?:=" => Kind::ElvisAssign,

            "." => Kind::Sdot,
            ".." => Kind::Ddot,
            "..." => Kind::Tdot,

            "--" => Kind::LineComment,

            "--[[" => Kind::BlkCommentStart,
            "]]" => Kind::BlkCommentEnd,
            "--[[]]" => Kind::EmptyComment,

            "]" => Kind::Rsqua,

            _ => Kind::Unknown,
        };

        let place = Place{
            linen : lin,
            pos   : pos,
            len   : len,
        };

        if match_kind!(kind,Kind::Unknown) {
            Ok(Token::with_kind_value(kind, place, self.readbuf.clone()))
        } else {
            Ok(Token::with_kind(kind, place))
        }
    }

    fn get_number_literal(&mut self) -> Result<Token, String> {

        let lin = self.linen;
        let mut len = 1;

        while let Some(c) = self.chars.next() {

            match c {

                '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                    self.readbuf.push(c);
                    continue;
                }

                _ => {
                    self.next_char = Some(c);
                    break;
                }
            }
        }

        len = self.readbuf.len() as Length;

        let token = Token::with_kind_value(Kind::Nlit, Place{linen:lin,pos:self.pos,len:len}, self.readbuf.clone());

        Ok(token)
    }

    fn get_string_literal(&mut self, begin_char: char) -> Result<Token, String> {

        let lin = self.linen;
        let pos = self.pos;
        let mut res = Err("not valid string literal".to_string());

        self.readbuf.clear();

        while let Some(c) = self.chars.next() {

            match c {

                '"' if c == begin_char => {
                    let len = self.readbuf.len() as Length;
                    let token = Token::with_kind_value(Kind::Slit, Place::with(lin,pos,len), self.readbuf.clone());
                    res = Ok(token);
                    break;
                },

                '`' if c == begin_char => {
                    let len = self.readbuf.len() as Length;
                    let token = Token::with_kind_value(Kind::Slit, Place::with(lin,pos,len), self.readbuf.clone());
                    res = Ok(token);
                    break;
                },

                _ => {
                    self.readbuf.push(c);
                },
            }
        }

        res
    }
}

impl<'a> Iterator for Tokenizer<'a> {

    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Result<Token, String>> {

        let result = self.next_token();

        Some(result)
    }

}

#[cfg(test)]
mod tests {

    use super::Tokenizer;

    //#[test]
    fn new_basic_tokenizer() {

        let mut tokenizer = Tokenizer::from_str(" test 10 \t10000; --[[]] fun if for 1+2 [false,true] --[[ test ]] `test test`");

        while let Some(result) = tokenizer.next() {

            match result {
                Ok(token) => println!("token: {:?}", token),
                Err(_) => break,
            }
        }
    }

    //#[test]
    fn new_expr_tokenizer() {

        let mut tokenizer = Tokenizer::from_str("-10");

        while let Some(result) = tokenizer.next() {

            match result {
                Ok(token) => println!("token: {:?}", token),
                Err(_) => break,
            }
        }
    }
}
