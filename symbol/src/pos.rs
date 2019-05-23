use std::fmt;
use std::ops::{Deref, DerefMut};

pub use codespan::{
    ByteIndex as BytePos, ByteOffset, ColumnIndex as Column, ColumnOffset, LineIndex as Line,
    LineOffset, Span,
};

/// A location in a source file.
#[derive(Debug, Copy, Clone, Default, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Location {
    pub line: Line,
    pub column: Column,
    pub absolute: BytePos,
}

impl Location {
    pub fn shift(&mut self, ch: char) {
        self.absolute += ByteOffset(1);

        if ch == '\n' {
            self.line += LineOffset(1);
            self.column = Column(1);
        } else if ch == '\r' {
            self.column = Column(1);
        } else {
            self.column += ColumnOffset(1);
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "({}, {})",
            self.line.number(),
            self.column.number()
        )
    }
}

impl Into<BytePos> for Location {
    fn into(self) -> BytePos {
        self.absolute
    }
}

#[derive(Debug, Copy, Clone, Default, Eq, PartialEq)]
pub struct Spanned<T, Pos: Copy> {
    pub span: Span<Pos>,
    pub value: T,
}

impl<T, Pos: Copy> Spanned<T, Pos> {
    pub fn map<U, F>(self, mut f: F) -> Spanned<U, Pos>
    where
        F: FnMut(T) -> U,
    {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }

    pub fn into_inner(self) -> T {
        self.value
    }
}

impl<T, Pos: Copy> AsRef<T> for Spanned<T, Pos> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T, Pos: Copy> Deref for Spanned<T, Pos> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T, Pos: Copy> DerefMut for Spanned<T, Pos> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T: fmt::Display, Pos: fmt::Display + Copy> fmt::Display for Spanned<T, Pos> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:10} ~ {:10} : {}", self.span.start(), self.span.end(), self.value)
    }
}

/// Construct a span from two positions.
pub fn span<Pos: Copy + Ord>(start: Pos, end: Pos) -> Span<Pos> {
    Span::new(start, end)
}

/// Construct a spanned value from the base value and it's position.
pub fn spanned<T, Pos: Copy + Ord>(span: Span<Pos>, value: T) -> Spanned<T, Pos> {
    Spanned { span, value }
}

// Construct a spanned value from the base value and it's position components.
pub fn spanned2<T, Pos: Copy + Ord>(start: Pos, end: Pos, value: T) -> Spanned<T, Pos> {
    Spanned {
        span: span(start, end),
        value,
    }
}

pub type Located<T> = Spanned<T, Location>;

pub trait HasSpan {
    fn span(&self) -> Span<BytePos>;
}

impl<T> HasSpan for Spanned<T, BytePos> {
    fn span(&self) -> Span<BytePos> {
        self.span
    }
}

impl<T> HasSpan for Spanned<T, Location> {
    fn span(&self) -> Span<BytePos> {
        let start = self.span.start().absolute;
        let end = self.span.end().absolute;

        span(start, end)
    }
}

impl HasSpan for Span<Location> {
    fn span(&self) -> Span<BytePos> {
        Span::new(self.start().absolute, self.end().absolute)
    }
}
