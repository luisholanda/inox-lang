use core::num::IntErrorKind;

use crate::lexer::error::PlexLexerError;
use crate::lexer::token::{PlexToken, Token};

pub(crate) fn non_float_literal(text: &str, radix: u32) -> PlexToken {
    match i32::from_str_radix(text, radix) {
        Ok(int) => PlexToken::LexToken(Token::IntLiteral(int)),
        Err(err) => match err.kind() {
            IntErrorKind::Underflow =>
                PlexToken::numerical_error(PlexLexerError::IntegerLiteralUnderflow{value: text.to_owned()}),
            IntErrorKind::Overflow =>
                PlexToken::numerical_error(PlexLexerError::NaturalLiteralOverflow{value: text.to_owned()}),
            err => unreachable!("No empty string or non number literal should reach this function: {:#?}", err)
        }
    }
}

pub(crate) fn float_literal(text: &str) -> PlexToken {
    match text.parse() {
        Ok(num) => PlexToken::LexToken(Token::FloatLiteral(num)),
        Err(_) => PlexToken::numerical_error(PlexLexerError::InvalidFloatLiteral {
            value: text.to_owned(),
        }),
    }
}
