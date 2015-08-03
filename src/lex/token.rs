
pub type LineNum = u16;
pub type Pos     = u32;
pub type Length  = u8;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Kind {

    /// Undefined token
    /// it is only used for undefined state.
    Undefined,

    /// ident token
    /// it is abstract token, can not used for concrete token
    Ident,

    /// Unknown token
    /// use for initialized token kind
    Unknown,

    /// EOL \n \r ;
    EOL,

    /// --
    LineComment,

    /// --[[]]
    EmptyComment,

    /// --[[
    BlkCommentStart,

    /// ]]
    BlkCommentEnd,

    /// Normal string literal
    /// "xxx"
    Slit,

    /// Quoted string literal
    /// `sample \a`
    QSlit,

    /// 1, ..., 99999
    /// 1.1, ...., 99999.9999...
    Nlit,

    /// (
    Lpar,
    /// )
    Rpar,

    /// {
    Lbrac,
    /// }
    Rbrac,

    /// [
    Lsqua,
    /// ]
    Rsqua,

    /// ;
    Scolon,
    /// :
    Dcolon,

    /// ,
    Comma,
    /// .
    Sdot,
    /// ..
    Ddot,
    /// ...
    Tdot,

    /// +
    Plus,
    /// -
    Minus,
    /// *
    Multi,
    /// /
    Divide,
    /// %
    Remain,
    /// ^
    Pow,
    /// #
    ArrLen,

    /// ==
    Equal,
    /// !=
    NotEqual,
    /// <=
    LessEqual,
    /// <
    Less,
    /// >=
    GreaterEqual,
    /// >
    Greater,

    /// :=
    Assign,

    /// ?:=
    ElvisAssign,

    /// and
    And,
    /// not
    Not,
    /// or
    Or,

    /// break
    Break,
    /// do
    Do,

    /// if
    If,
    /// else
    Else,
    /// elseif
    ElseIf,

    /// for
    For,
    /// end
    End,

    /// fun
    Fun,
    /// return
    Return,

    /// in
    In,
    /// let
    Let,

    /// true
    True,
    /// false
    False,

    /// nil
    Nil,

    /// repeat
    Repeat,
    /// then
    Then,
    /// until
    Until,
    /// while
    While,
}

#[derive(Debug, Clone)]
pub struct Place {
    pub linen : LineNum,
    pub pos   : Pos,
    pub len   : Length,
}

impl Place {

    pub fn with(linen: LineNum, pos: Pos, len: Length) -> Place {

        Place{
            linen : linen,
            pos   : pos,
            len   : len,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind  : Kind,
    pub value : Option<String>,
    pub place : Place,
}

pub static UNDEFINED_TOKEN: Token = Token{
    kind  : Kind::Undefined,
    value : None,
    place : Place{linen:0,pos:0,len:0},
};

impl Token {

    pub fn with_kind(kind: Kind, place: Place) -> Token {

        Token{
            kind  : kind,
            value : None,
            place : place,
        }
    }

    pub fn with_kind_value(kind: Kind, place: Place, value: String) -> Token {

        Token{
            kind  : kind,
            value : Some(value),
            place : place,
        }
    }
}

/// Matcher macros
///

#[macro_export]
macro_rules! match_kind {
    ($k:expr,$p:pat) => {
        match $k {
            $p => true,
            _ => false,
        }
    };
}

#[macro_export]
macro_rules! match_token_kind {
    ($t:expr,$p:path) => {
        match $t {
            Token{kind:$p,value:_,place:_} => true,
            _ => false,
        }
    };
}

