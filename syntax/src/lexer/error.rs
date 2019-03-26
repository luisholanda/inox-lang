use codespan_reporting::{Diagnostic, Label};
use failure::Fail;

use symbol::pos::{BytePos, HasSpan, Span, Location};

#[derive(Fail, Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    /// An unexpected character was found in the source input.
    #[fail(display = "An unexpected character {:?} was found.", found)]
    UnexpectedCharacter { start: Span<Location>, found: char },
    /// An empty character literal was found in the source input.
    #[fail(display = "Empty character literal.")]
    EmptyCharLiteral { start: Span<Location> },
    /// An unknown character literal was found in the source input.
    #[fail(display = "Unknown character literal {}.", found)]
    UnknownCharLiteral { start: Span<Location>, found: String },
    /// An unknown escaped character literal was found in the source input.
    #[fail(display = "An unknown escape code {} was found.", found)]
    UnknownEscapeCode { start: Span<Location>, found: String },
    /// An integer literal caused an underflow while parsing it.
    #[fail(display = "An integer literal {} was too small.", value)]
    IntegerLiteralUnderflow { start: Span<Location>, value: String },
    /// A natural literal caused an overflow while parsing it.
    #[fail(display = "A natural literal {} was too large.", value)]
    NaturalLiteralOverflow { start: Span<Location>, value: String },
    /// An invalid float-point literal was found in the source input.
    #[fail(display = "An invalid float literal {} was found.", value)]
    InvalidFloatLiteral { start: Span<Location>, value: String },
    /// The file ended too soon.
    #[fail(display = "Unexpected end of file.")]
    UnexpectedEOF { start: Span<Location> },
    /// An unmatched was found in the source input.
    #[fail(
        display = "Unexpected delimiter: expected '\\{}', found '\\{}'",
        expected, found
    )]
    UnmatchedDelimiter {
        start: Span<Location>,
        expected: String,
        found: String,
    },

    /// # Warnings
    #[fail(display = "Missing whitespace in documentation comment.")]
    MissingLeadingWhitespace { start: Span<Location> },
}

impl HasSpan for LexerError {
    fn span(&self) -> Span<BytePos> {
        use self::LexerError::*;

        match self {
            UnexpectedCharacter { start, .. } => start.span(),
            EmptyCharLiteral { start } => start.span(),
            UnknownCharLiteral { start, .. } => start.span(),
            UnknownEscapeCode { start, .. } => start.span(),
            IntegerLiteralUnderflow { start, .. } => start.span(),
            NaturalLiteralOverflow { start, .. } => start.span(),
            InvalidFloatLiteral { start, .. } => start.span(),
            UnexpectedEOF { start } => start.span(),
            UnmatchedDelimiter { start, .. } => start.span(),

            MissingLeadingWhitespace { start } => start.span(),
        }
    }
}

impl Into<Diagnostic> for LexerError {
    fn into(self) -> Diagnostic {
        use self::LexerError::*;

        let diag = match &self {
            UnexpectedCharacter { found, .. } => {
                Diagnostic::new_error(format!("unexpected character {}", found))
            }
            EmptyCharLiteral { .. } => Diagnostic::new_error("empty character literal".to_string()),
            UnknownCharLiteral { found, .. } => {
                Diagnostic::new_error(format!("unknown character literal: {}", found))
            }
            UnknownEscapeCode { found, .. } => {
                Diagnostic::new_error(format!("unknown escape code {}", found))
            }
            IntegerLiteralUnderflow { value, .. } => {
                Diagnostic::new_error(format!("integer literal underflow with value {}", value))
            }
            NaturalLiteralOverflow { value, .. } => {
                Diagnostic::new_error(format!("natural literal overflow with value {}", value))
            }
            InvalidFloatLiteral { value, .. } => {
                Diagnostic::new_error(format!("float literal {} is invalid", value))
            }
            UnexpectedEOF { .. } => Diagnostic::new_error("unexpected end of file"),
            UnmatchedDelimiter {
                expected, found, ..
            } => Diagnostic::new_error(format!(
                "unmatched delimiter: expected «{}», found: «{}»",
                expected, found
            )),

            MissingLeadingWhitespace { .. } => {
                Diagnostic::new_note("missing whitespace in start of documentation")
            }
        };

        diag.with_label(Label::new_primary(self.span()))
    }
}

#[derive(Fail, Debug, Clone, PartialEq, Eq)]
pub enum PlexLexerError {
    #[fail(display = "Empty character literal.")]
    EmptyCharLiteral,
    #[fail(display = "Unknown character literal {}.", found)]
    UnknownCharLiteral { found: String },
    #[fail(display = "An unknown escape code \\{} was found.", found)]
    UnknownEscapeCode { found: String },
    #[fail(display = "An integer literal {} was too small.", value)]
    IntegerLiteralUnderflow { value: String },
    #[fail(display = "A natural literal {} was too large", value)]
    NaturalLiteralOverflow { value: String },
    #[fail(display = "An invalid float literal {} was found.", value)]
    InvalidFloatLiteral { value: String },
}

impl PlexLexerError {
    pub(crate) fn into_lexer_error(self, start: Span<Location>) -> LexerError {
        use self::PlexLexerError::*;

        match self {
            EmptyCharLiteral => LexerError::EmptyCharLiteral { start },
            UnknownCharLiteral { found } => LexerError::UnknownCharLiteral { start, found },
            UnknownEscapeCode { found } => LexerError::UnknownEscapeCode { start, found },
            IntegerLiteralUnderflow { value } => LexerError::IntegerLiteralUnderflow { start, value },
            NaturalLiteralOverflow { value } => LexerError::NaturalLiteralOverflow { start, value },
            InvalidFloatLiteral { value } => LexerError::InvalidFloatLiteral { start, value },
        }
    }
}
