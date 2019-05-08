use std::fmt;

use symbol::pos::{Location, Spanned};

pub use crate::cst::{ArithOp, LogicOp};
use crate::lexer::error::PlexLexerError;

/// A delimiter token.
#[derive(Debug, Copy, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub enum Delimiter {
    LBrace,
    LBracket,
    LParen,
    RBrace,
    RBracket,
    RParen,
}

impl Delimiter {
    /// Whether the delimiter is a open delimiter.
    pub(crate) fn is_open(self) -> bool {
        use self::Delimiter::*;

        match self {
            LBrace | LBracket | LParen => true,
            _ => false,
        }
    }

    /// Whether the delimiter is a close delimiter.
    pub(crate) fn is_close(self) -> bool {
        !self.is_open()
    }

    /// Returns the opposed delimiter.
    pub(crate) fn opposed(self) -> Delimiter {
        use self::Delimiter::*;

        match self {
            RBrace => LBrace,
            RBracket => LBracket,
            RParen => LParen,
            LBrace => RBrace,
            LBracket => RBracket,
            LParen => RParen,
        }
    }

    /// Checks whether the delimiter can closes `open`.
    /// Panics if used with a non closing delimiter.
    pub(crate) fn closes(self, open: Delimiter) -> bool {
        assert!(self.is_close());

        self.opposed() == open
    }
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Delimiter::*;

        let s = match self {
            LBrace => "{",
            LBracket => "[",
            LParen => "(",
            RBrace => "}",
            RBracket => "]",
            RParen => ")",
        }
        .to_string();

        write!(f, "{}", s)
    }
}

pub(crate) const DEFAULT_CHAR: Token = Token::CharLiteral(' ');
pub(crate) const DEFAULT_NUM: Token = Token::IntLiteral(0);

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    // We use `String` here as we need ownership fo the input
    // to join grouped `DocComment` tokens into one token.
    DocComment(String),

    // Literals
    StringLiteral(String),
    CharLiteral(char),
    IntLiteral(i32),
    NatLiteral(u32),
    FloatLiteral(f32),
    SelfLit,

    Arith(ArithOp),

    Logic(LogicOp),

    // Types
    TypeVar(String),
    Data,
    Type,
    New,
    Struct,
    Impl,
    Forall,

    // Control-flow
    If,
    Unless,
    Else,
    While,
    Until,
    For,
    In,
    Loop,
    Break,
    Continue,
    Return,

    // Extras
    With,
    Match,
    Let,
    Mut,

    // Punctuation
    Colon,
    DoubleColon,
    SemiColon,
    Dot,
    Comma,
    Question,
    RArrow,
    FatRArrow,
    SqdRArrow,
    LArrow,
    Equals,
    Pipe,

    Delim(Delimiter),
}

pub type SpannedToken = Spanned<Token, Location>;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;

        let s = match self {
            Delim(delim) => delim.to_string(),
            Arith(op) => op.to_string(),
            Logic(op) => op.to_string(),
            CharLiteral(chl) => format!("{}", chl),
            IntLiteral(intl) => format!("{}", intl),
            NatLiteral(natl) => format!("{}", natl),
            FloatLiteral(fltl) => format!("{}", fltl),
            other => match other {
                Identifier(ident) => ident.as_str(),
                DocComment(doc) => doc.as_str(),

                StringLiteral(strl) => strl.as_str(),
                SelfLit => "SelfLit",

                Data => "data",
                Type => "type",
                New => "new",
                Struct => "struct",
                Impl => "impl",
                Forall => "forall",
                SelfTy => "Self",

                If => "if",
                Else => "else",
                Unless => "unless",
                While => "while",
                Until => "until",
                For => "for",
                In => "in",
                Loop => "loop",
                Break => "break",
                Continue => "continue",
                Return => "return",

                With => "with",
                Match => "match",
                Let => "let",
                Mut => "mut",

                Colon => ":",
                DoubleColon => "::",
                SemiColon => ";",
                Dot => ".",
                Comma => ",",
                Question => "?",
                RArrow => "->",
                FatRArrow => "=>",
                SqdRArrow => "~>",
                LArrow => "<-",
                Equals => "=",
                Pipe => "|",

                _ => unreachable!("shouldn't reach here."),
            }
            .to_string(),
        };

        write!(f, "\"{}\"", s)
    }
}

/// Internal token structure, bridges plex and the Inox Lexer.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum PlexToken {
    /// A valid lexical token.
    LexToken(Token),

    /// Start of an identifier.
    /// Used to signalize the main lexer that it should scan
    /// an identifier.
    ///
    /// This is needed as we doesn't have the XID_START and XID_CONTINUE
    /// tables in plex.
    IdentStart,

    /// A lexer error with a placeholder token.
    /// The placeholder token should be of a type that prevents the parser
    /// from breaking.
    Error(Token, PlexLexerError),

    // Junk.
    Comment,
    Whitespace,
    NewLine,
    EOF,
}

impl PlexToken {
    pub(crate) fn numerical_error(error: PlexLexerError) -> Self {
        PlexToken::Error(DEFAULT_NUM, error)
    }

    pub(crate) fn character_error(error: PlexLexerError) -> Self {
        PlexToken::Error(DEFAULT_CHAR, error)
    }
}
