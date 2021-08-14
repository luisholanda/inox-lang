use std::collections::VecDeque;

use plex::lexer;
use unicode_xid::UnicodeXID;

use symbol::pos::{span, spanned, spanned2, Location, Span};

use crate::lexer::error::{LexerError, PlexLexerError};
use crate::lexer::escaped::unescape;
use crate::lexer::numeric::{float_literal, non_float_literal};
use crate::lexer::token::*;

fn scan_escape_code(text: &str) -> PlexToken {
    if let Some(ch) = unescape(text) {
        PlexToken::LexToken(Token::CharLiteral(ch))
    } else {
        let plex_err = PlexLexerError::UnknownEscapeCode {
            found: text.to_owned(),
        };

        PlexToken::character_error(plex_err)
    }
}

fn is_ident_start(ch: char) -> bool {
    UnicodeXID::is_xid_start(ch) || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    UnicodeXID::is_xid_continue(ch) || ch == '_' || ch == '\''
}

fn is_separator(ch: char) -> bool {
    " \t\r\n-<>|=?([{}])~,.:;+*/!".contains(ch)
}

/// Inox Lexer.
#[derive(Debug)]
pub struct Lexer<'inp> {
    /// Remaining input slice.
    remaining: &'inp str,
    /// Current location in `input`.
    current: Location,
    /// Stack of open delimiters.
    open_braces: Vec<Delimiter>,
    /// Errors that happen during lexing.
    pub errors: Vec<LexerError>,
    /// Warnings that happen during lexing.
    pub warnings: Vec<LexerError>,

    // Peek caching
    /// Next token returned in the source input.
    peek_tok: Option<PlexToken>,
    /// Source input text that generated the peeked token.
    peek_delta: Option<&'inp str>,
    /// Remaining of the source input after extracting the next token.
    peek_rem: Option<&'inp str>,

    /// Tokens on hold to be fetched by the Iterator.
    ///
    /// This is used to give us the possibility to emit multiple tokens
    /// at once, e.g. when trying to fix unmatched delimiters.
    token_stream: VecDeque<SpannedToken>,
}

impl<'inp> Lexer<'inp> {
    pub fn new(input: &'inp str) -> Self {
        Self {
            remaining: input,
            current: Default::default(),
            open_braces: vec![],
            errors: vec![],
            warnings: vec![],
            peek_tok: None,
            peek_rem: None,
            peek_delta: None,
            token_stream: VecDeque::new(),
        }
    }

    /// Shifts the current lexer position after fetching some
    /// token and returns the span of the input change.
    fn shift_location(&mut self, text: &'inp str) -> Span<Location> {
        let start = self.current;

        for ch in text.chars() {
            self.current.shift(ch);
        }

        span(start, self.current)
    }

    /// Look for the next token in the source input, updating the
    /// remaining input after getting the token.
    fn lookahead(&mut self) -> Option<(PlexToken, &'inp str)> {
        self.peek_token();

        self.peek_tok.take().and_then(|tok| match tok {
            PlexToken::EOF => None,
            tok => {
                self.remaining = self.peek_rem.take().unwrap();
                Some((tok, self.peek_delta.unwrap_or_default()))
            }
        })
    }

    /// Peek the next token in the source input.
    /// Doesn't update the remaining input.
    fn peek_token(&mut self) -> Option<&PlexToken> {
        if self.peek_tok.is_none() {
            match take_token(self.remaining) {
                Some((tok, new_rem)) => {
                    self.peek_tok = Some(tok.0);
                    self.peek_delta = Some(tok.1);
                    self.peek_rem = Some(new_rem);
                }
                None => {
                    self.peek_tok = Some(PlexToken::EOF);
                    self.peek_delta = None;
                    self.peek_rem = None;
                }
            }
        }

        self.peek_tok.as_ref()
    }

    /// Produces the next token in the source input.
    ///
    /// Takes cares of errors and generating the correct source span
    /// of the token.
    fn bump_token(&mut self) -> Option<SpannedToken> {
        while let Some((token, delta)) = self.lookahead() {
            let span = self.shift_location(delta);

            match token {
                PlexToken::EOF => {
                    self.receive_eof();
                    break;
                }
                PlexToken::Error(placeholder, plex_err) => {
                    let err = plex_err.into_lexer_error(span);
                    self.errors.push(err);

                    return Some(spanned(span, placeholder));
                }
                PlexToken::LexToken(token) => {
                    return Some(spanned(span, token));
                }
                PlexToken::IdentStart => {
                    let head = delta.chars().next().unwrap();
                    return Some(self.scan_identifier(head, span));
                }
                _ => continue,
            }
        }

        None
    }

    /// Consumes the next source input token without emitting it.
    fn ignore_token(&mut self) {
        if let Some((token, delta)) = self.lookahead() {
            let span = self.shift_location(delta);

            if let PlexToken::Error(_, plex_err) = token {
                let err = plex_err.into_lexer_error(span);
                self.errors.push(err);
            }
        }
    }

    /// Produces the next token to be emitted by the lexer.
    ///
    /// Takes care of updating the delimiters stack and aggregating
    /// documentation comments.
    fn produce_token(&mut self) -> Option<()> {
        let token = self.bump_token()?;

        let next = match token.as_ref() {
            Token::DocComment(_) => self.agg_doc_comments(token),
            Token::Delim(tok) if tok.is_open() => {
                self.open_braces.push(*tok);

                token
            }
            Token::Delim(delim) => {
                if !self.close_delimiter(*delim, token.span) {
                    return Some(());
                }
                token
            }
            _ => token,
        };

        self.token_stream.push_back(next);

        Some(())
    }

    /// Scans the source input for an identifier.
    fn scan_identifier(&mut self, head: char, start: Span<Location>) -> SpannedToken {
        let mut ident = String::new();

        if !is_ident_start(head) {
            self.errors
                .push(LexerError::UnexpectedCharacter { start, found: head });
        } else {
            ident.push(head);
        }

        let mut end = start.end();
        let mut offset = 0usize;

        for curr in self.remaining.chars() {
            if is_separator(curr) {
                break;
            }

            end.shift(curr);

            if is_ident_continue(curr) {
                ident.push(curr);
                offset += 1;
            } else {
                self.errors.push(LexerError::UnexpectedCharacter {
                    start: span(start.start(), end),
                    found: curr,
                })
            }
        }
        self.current = end;

        self.remaining = &self.remaining[offset..];

        let token = if ident.ends_with('\'') {
            // Remove apostrophe before sending token.
            ident.pop();
            Token::TypeVar(ident)
        } else {
            match ident.as_str() {
                "not" => Token::Logic(LogicOp::Not),
                "and" => Token::Logic(LogicOp::And),
                "or"  => Token::Logic(LogicOp::Or),

                "data"   => Token::Data,
                "type"   => Token::Type,
                "new"    => Token::New,
                "struct" => Token::Struct,
                "impl"   => Token::Impl,
                "forall" => Token::Forall,

                "if"     => Token::If,
                "else"   => Token::Else,
                "unless" => Token::Unless,
                "while"  => Token::While,
                "until"  => Token::Until,
                "for"    => Token::For,
                "in"     => Token::In,
                "loop"   => Token::Loop,
                "break"  => Token::Break,
                "continue" => Token::Continue,
                "return" => Token::Return,

                "with"  => Token::With,
                "match" => Token::Match,
                "let"   => Token::Let,
                "mut"   => Token::Mut,
                "self" => Token::SelfLit,
                "true" => Token::BoolLiteral(true),
                "false" => Token::BoolLiteral(false),
                _ => Token::Identifier(ident)
            }
        };

        spanned2(start.start(), end, token)
    }

    /// Aggregate documentation comments into one big token.
    ///
    /// This is used to facilitate future parsing of the documentation.
    fn agg_doc_comments(&mut self, first: SpannedToken) -> SpannedToken {
        let start = first.span.start();
        let mut end = first.span.end();

        let mut docs = match first.into_inner() {
            Token::DocComment(doc) => vec![doc],
            _ => panic!("agg_doc_comment called with non-documentation token"),
        };

        while let Some(tok) = self.peek_token() {
            match tok {
                PlexToken::LexToken(Token::DocComment(doc)) => {
                    let has_leading_space = doc.starts_with(' ');
                    docs.push(doc.to_owned());

                    let next = self.bump_token().unwrap();
                    end = next.span.end();

                   if !has_leading_space {
                        let warn = LexerError::MissingLeadingWhitespace { start: next.span };
                        self.warnings.push(warn)
                    }
                }
                PlexToken::Whitespace => self.ignore_token(),
                _ => break,
            }
        }

        let full_doc_token = Token::DocComment(docs.join(""));

        spanned2(start, end, full_doc_token)
    }

    fn receive_eof(&mut self) {
        if !self.open_braces.is_empty() {
            self.fix_delimiter_stack_eof();

            let start = self.current;
            self.current.shift(' ');
            self.errors.push(LexerError::UnexpectedEOF {
                start: span(start, self.current),
            })
        }
    }
}

// Delimiter stack fix.
impl<'inp> Lexer<'inp> {
    /// Verifies if the close delimiters matches the open delimiter
    /// on the top of the delimiters stack. Returns if the delimiter should be
    /// emitted.
    ///
    /// When the delimiters doesn't match and the close one matches one of the
    /// others in the stack, tries to fix the last by emitting the opposed delimiter
    /// of the part of the stack that didn't match. That is, when we have a stack like:
    ///
    /// ```text
    /// { { ( (
    /// ```
    ///
    /// And we receive a close delimiter `}`, we will emit two extra tokens,
    /// that will close the `( (` part of the stack, thus, having a clean state
    /// for the next tokens.
    fn close_delimiter(&mut self, delim: Delimiter, start: Span<Location>) -> bool {
        match self.open_braces.pop() {
            // We received a closing delimiter with an empty delimiter stack.
            // We don't want to emit the token, as this will certainly break
            // the parser.
            None => {
                let found = delim.to_string();

                self.errors.push(LexerError::UnexpectedCharacter {
                    start,
                    found: found.chars().next().unwrap(),
                });

                false
            }
            // We received a closing delimiter that closes the last delimiter.
            // Nothing to do, just emit the token.
            Some(open) if delim.closes(open) => true,
            // We received a closing delimiter that doesn't close the last delimiter.
            // Try to fix the delimiter stack.
            Some(open) => {
                // TODO(luisholanda): We should search the probably opening delimiter
                //  that match the closing one, as rustc does, instead of just telling
                //  what is expected.
                let expected = open.opposed().to_string();
                let found = delim.to_string();
                let error = LexerError::UnmatchedDelimiter {
                    start,
                    expected,
                    found,
                };
                self.errors.push(error);

                self.open_braces.push(open);

                self.try_fix_delimiter_stack(delim, start.start())
            }
        }
    }

    /// Tries to fix the delimiter stack given the closing delimiter `delim`.
    /// Returns if the closing delimiter should be emitted or not.
    ///
    /// The primary idea is that, when the closing delimiter matches some internal
    /// element of the stack, the user probably forgot/deleted internal delimiters
    /// of an expression. In this case, we emit extra tokens that will set the stack
    /// to a state that, at least, the parsing can continue without problems.
    ///
    /// When the closing delimiter doesn't match any internal element of the sack, we
    /// simply doesn't emit it, as this is probably an extra character that the user
    /// miss-typed or forgot to remove, and hope that further in the source input
    /// the stack will be fixed.
    fn try_fix_delimiter_stack(&mut self, delim: Delimiter, start: Location) -> bool {
        let delim_until_match = self
            .open_braces
            .iter()
            .rev()
            .take_while(|b| !delim.closes(**b))
            .count();

        if delim_until_match == self.open_braces.len() {
            // We didn't find a match in the stack, don't emit the delimiter.
            false
        } else {
            // We found a match in the stack, rollback the stack to before it,
            // emitting the correct delimiters.
            for _ in 0..delim_until_match {
                self.emit_opposed_delimiter(start);
            }

            true
        }
    }

    fn fix_delimiter_stack_eof(&mut self) {
        for _ in 0..self.open_braces.len() {
            self.emit_opposed_delimiter(self.current);
        }
    }

    fn emit_opposed_delimiter(&mut self, start: Location) {
        let open_token = self.open_braces.pop().unwrap();
        let close_token = Token::Delim(open_token.opposed());

        let mut end = start;
        end.shift(' ');
        let close_span = span(start, end);

        let span_token = spanned(close_span, close_token);

        self.token_stream.push_back(span_token);
    }
}

impl<'inp> Iterator for Lexer<'inp> {
    type Item = SpannedToken;

    fn next(&mut self) -> Option<Self::Item> {
        if self.token_stream.is_empty() {
            self.produce_token();
        }

        self.token_stream.pop_front()
    }
}

lexer! {
    fn take_token(text: 'inp) -> (PlexToken, &'inp str);

    r#"\r\n|\n"# => (PlexToken::NewLine, text),
    r#"[ \t]+"# => (PlexToken::Whitespace, text),

    // The second expression guarantees that multiple lines of
    // documentation aren't separated by Whitespace.
    //
    // The first one lexes a documentation in the last line of
    // the file, when the file doesn't end in a `\n`.
    // FIXME(luisholanda): This doesn't work in CR{LF} line endings.
    //  Probably will be easier to just convert them to LF before lexing,
    //  but this will require walking in the input twice.
    r#"///([^\n]+|[^\n]+\n)"# => {
        let trimmed = unsafe { text.get_unchecked(3..text.len()) };
        (PlexToken::LexToken(Token::DocComment(trimmed.to_owned())), text)
    },
    r#"//[^\n]+"# => (PlexToken::Comment, text),

    // The expression for floats is a little more complicated as we don't want to
    // allow float literals with unnecessary parts, like `00.01` (that must be written
    // as `0.01`) and `1.23E0` (that must be written as `1.23`).
    //
    // TODO(luisholanda): We need to relax the expression here and check latter if the
    //  user passed a case we want to avoid, emitting warnings to the user, instead of
    //  just failing. We can use the expression -?[0-9]+\.[0-9]([Ee]-?[0-9]+))?
    //
    // Using `?` the expression is: -?([1-9][0-9]*|0)\.[0-9]+((E|e)-?[1-9][0-9]*)?
    // Removing `?`: ((-([1-9][0-9]*))|(([1-9][0-9]*)|0))\.([0-9]+|[0-9]+((E|e)(-([1-9][0-9]*)|([1-9][0-9]*))))
    // Grouping redundancies, we get the bellow:
    r#"((-[1-9]|[1-9])[0-9]*|(-0|0))\.([0-9]+|[0-9]+([Ee]((-[1-9]|[1-9])[0-9]*)))"# => {
        (float_literal(text), text)
    },
    r#"(-0x|0x)[0-9A-F]+"# => (non_float_literal(&text[2..], 16), text),
    r#"((-[1-9]|[1-9])[0-9]*|0)"# => (non_float_literal(text, 10), text),
    r#""[^"]*""# => {
        let trimmed = unsafe { text.get_unchecked(1..text.len()-1) };
        let unescaped = unescape::unescape(trimmed).unwrap();

        (PlexToken::LexToken(Token::StringLiteral(unescaped)), text)
    },
    r#"'[^']*'"# => {
        let trimmed = unsafe { text.get_unchecked(1..text.len()-1) };

        if trimmed.is_empty() {
            (PlexToken::character_error(PlexLexerError::EmptyCharLiteral), text)
        } else if trimmed.starts_with('\\') {
            (scan_escape_code(trimmed), text)
        } else {
            match trimmed.parse() {
                Ok(ch) => (PlexToken::LexToken(Token::CharLiteral(ch)), text),
                Err(_) => {
                    let plex_err = PlexLexerError::UnknownCharLiteral{found: trimmed.to_owned()};
                    (PlexToken::character_error(plex_err), text)
                }
            }
        }
    },

    r#"\+"#   => (PlexToken::LexToken(Token::Arith(ArithOp::Add)), text),
    r#"-"#    => (PlexToken::LexToken(Token::Arith(ArithOp::Sub)), text),
    r#"\*\*"# => (PlexToken::LexToken(Token::Arith(ArithOp::Pow)), text),
    r#"\*"#   => (PlexToken::LexToken(Token::Arith(ArithOp::Mult)), text),
    r#"/"#    => (PlexToken::LexToken(Token::Arith(ArithOp::Div)), text),
    r#"%"#    => (PlexToken::LexToken(Token::Arith(ArithOp::Mod)), text),
    r#">>"#   => (PlexToken::LexToken(Token::Arith(ArithOp::RShift)), text),
    r#"<<"#   => (PlexToken::LexToken(Token::Arith(ArithOp::LShift)), text),

    r#"=="#  => (PlexToken::LexToken(Token::Logic(LogicOp::DoubleEquals)), text),
    r#"!="#  => (PlexToken::LexToken(Token::Logic(LogicOp::NotEquals)), text),
    r#"<="#  => (PlexToken::LexToken(Token::Logic(LogicOp::LessEquals)), text),
    r#"<"#   => (PlexToken::LexToken(Token::Logic(LogicOp::LessThan)), text),
    r#">="#  => (PlexToken::LexToken(Token::Logic(LogicOp::GreaterEquals)), text),
    r#">"#   => (PlexToken::LexToken(Token::Logic(LogicOp::GreaterThan)), text),

    r#"::"# => (PlexToken::LexToken(Token::DoubleColon), text),
    r#":"#  => (PlexToken::LexToken(Token::Colon), text),
    r#";"#  => (PlexToken::LexToken(Token::SemiColon), text),
    r#"\."# => (PlexToken::LexToken(Token::Dot), text),
    r#","#  => (PlexToken::LexToken(Token::Comma), text),
    r#"\?"# => (PlexToken::LexToken(Token::Question), text),
    r#"<-"# => (PlexToken::LexToken(Token::LArrow), text),
    r#"->"# => (PlexToken::LexToken(Token::RArrow), text),
    r#"=>"# => (PlexToken::LexToken(Token::FatRArrow), text),
    r#"\~>"# => (PlexToken::LexToken(Token::SqdRArrow), text),
    r#"="#  => (PlexToken::LexToken(Token::Equals), text),
    r#"\|"# => (PlexToken::LexToken(Token::Pipe), text),

    r#"\["# => (PlexToken::LexToken(Token::Delim(Delimiter::LBracket)), text),
    r#"{"#  => (PlexToken::LexToken(Token::Delim(Delimiter::LBrace)), text),
    r#"\("# => (PlexToken::LexToken(Token::Delim(Delimiter::LParen)), text),
    r#"\]"# => (PlexToken::LexToken(Token::Delim(Delimiter::RBracket)), text),
    r#"}"#  => (PlexToken::LexToken(Token::Delim(Delimiter::RBrace)), text),
    r#"\)"# => (PlexToken::LexToken(Token::Delim(Delimiter::RParen)), text),

    r#"."# => (PlexToken::IdentStart, text)
}

#[cfg(test)]
mod test {
    use codespan::{ByteIndex, RawIndex};

    use super::*;
    use symbol::pos::HasSpan;

    macro_rules! test {
        ($src:expr, $($span:expr => $token:expr,)*) => {{
            let lexed_tokens: Vec<_> = Lexer::new($src).map(|tok| (tok.span(), tok.value)).collect();

            let expected_tokens = vec![$({
                let start = ByteIndex($span.find("~").unwrap() as RawIndex);
                let end = ByteIndex($span.rfind("~").unwrap() as RawIndex + 1);

                (span(start, end), $token)
            }),*];

            assert_eq!(lexed_tokens, expected_tokens);
        }};
    }

    #[test]
    fn identifier() {
        test! {
            " hello_hahaha8ABC asdfg",
            " ~~~~~~~~~~~~~~~~      " => Token::Identifier("hello_hahaha8ABC".to_owned()),
            "                  ~~~~~" => Token::Identifier("asdfg".to_owned()),
        }
    }

    #[test]
    fn comment() {
        test! {
            "       // testing comments\n  ",
        }
    }

    #[test]
    fn documentation() {
        test! {
             "   /// Testing documentation comments\n  ",
             "   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  " => Token::DocComment(" Testing documentation comments\n".to_string()),
        }
    }

    #[test]
    fn group_documentation() {
        test! {
            "   /// Testing\n   /// documentation\n\n\n    /// comments\n ",
            "   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                     " => Token::DocComment(" Testing\n documentation\n".to_string()),
            "                                          ~~~~~~~~~~~~~  " => Token::DocComment(" comments\n".to_string()),
        }
    }

    #[test]
    fn char_literal() {
        test! {
            r#"'a' 'b' ' ' '\n' '\t' '\u1234' "#,
              "~~~                            " => Token::CharLiteral('a'),
              "    ~~~                        " => Token::CharLiteral('b'),
              "        ~~~                    " => Token::CharLiteral(' '),
              "            ~~~~               " => Token::CharLiteral('\n'),
              "                 ~~~~          " => Token::CharLiteral('\t'),
              "                      ~~~~~~~~ " => Token::CharLiteral('\u{1234}'),
        }
    }

    #[test]
    fn string_literal() {
        test! {
            r#" "testing literal strings: 'haha'" "#,
              " ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ " => Token::StringLiteral("testing literal strings: 'haha'".to_owned()),
        }
    }

    #[test]
    fn integer_literal() {
        test! {
            " 123456890 ",
            " ~~~~~~~~~ " => Token::IntLiteral(123456890),
        }
    }

    #[test]
    fn natural_literal() {
        test! {
            " 2456789013 ",
            " ~~~~~~~~~~ " => Token::NatLiteral(2456789013u32),
        }
    }

    #[test]
    fn hex_literal() {
        test! {
            " 0x123AFF ",
            " ~~~~~~~~ " => Token::IntLiteral(0x123AFF),
        }
    }

    #[test]
    fn float_literal() {
        test! {
            " 0.123e10 -0.123E10 0.123 -0.123",
            " ~~~~~~~~                       " => Token::FloatLiteral(0.123e10),
            "          ~~~~~~~~~             " => Token::FloatLiteral(-0.123e10),
            "                    ~~~~~       " => Token::FloatLiteral(0.123),
            "                          ~~~~~~" => Token::FloatLiteral(-0.123),
        }
    }

    #[test]
    fn operators() {
        test! {
            " + - * / ** << >> == != < > <= >= not and or",
            " ~                                          " => Token::Arith(ArithOp::Add),
            "   ~                                        " => Token::Arith(ArithOp::Sub),
            "     ~                                      " => Token::Arith(ArithOp::Mult),
            "       ~                                    " => Token::Arith(ArithOp::Div),
            "         ~~                                 " => Token::Arith(ArithOp::Pow),
            "            ~~                              " => Token::Arith(ArithOp::LShift),
            "               ~~                           " => Token::Arith(ArithOp::RShift),
            "                  ~~                        " => Token::Logic(LogicOp::DoubleEquals),
            "                     ~~                     " => Token::Logic(LogicOp::NotEquals),
            "                        ~                   " => Token::Logic(LogicOp::LessThan),
            "                          ~                 " => Token::Logic(LogicOp::GreaterThan),
            "                            ~~              " => Token::Logic(LogicOp::LessEquals),
            "                               ~~           " => Token::Logic(LogicOp::GreaterEquals),
            "                                  ~~~       " => Token::Logic(LogicOp::Not),
            "                                      ~~~   " => Token::Logic(LogicOp::And),
            "                                          ~~" => Token::Logic(LogicOp::Or),
        }
    }

    #[test]
    fn type_keywords() {
        test! {
            "self data type new struct impl forall",
            "~~~~                                 " => Token::SelfLit,
            "     ~~~~                            " => Token::Data,
            "          ~~~~                       " => Token::Type,
            "               ~~~                   " => Token::New,
            "                   ~~~~~~            " => Token::Struct,
            "                          ~~~~       " => Token::Impl,
            "                               ~~~~~~" => Token::Forall,
        }
    }

    #[test]
    fn flow_keywords() {
        test! {
            "if else unless while until for in loop break continue return",
            "~~                                                          " => Token::If,
            "   ~~~~                                                     " => Token::Else,
            "        ~~~~~~                                              " => Token::Unless,
            "               ~~~~~                                        " => Token::While,
            "                     ~~~~~                                  " => Token::Until,
            "                           ~~~                              " => Token::For,
            "                               ~~                           " => Token::In,
            "                                  ~~~~                      " => Token::Loop,
            "                                       ~~~~~                " => Token::Break,
            "                                             ~~~~~~~~       " => Token::Continue,
            "                                                      ~~~~~~" => Token::Return,
        }
    }

    #[test]
    fn punctuations() {
        test! {
            ":: : , . ? <- -> => ~> =",
            "~~                      " => Token::DoubleColon,
            "   ~                    " => Token::Colon,
            "     ~                  " => Token::Comma,
            "       ~                " => Token::Dot,
            "         ~              " => Token::Question,
            "           ~~           " => Token::LArrow,
            "              ~~        " => Token::RArrow,
            "                 ~~     " => Token::FatRArrow,
            "                    ~~  " => Token::SqdRArrow,
            "                       ~" => Token::Equals,
        }
    }

    #[test]
    fn delimiters() {
        test! {
            "{ [ ( ) ] }",
            "~          " => Token::Delim(Delimiter::LBrace),
            "  ~        " => Token::Delim(Delimiter::LBracket),
            "    ~      " => Token::Delim(Delimiter::LParen),
            "      ~    " => Token::Delim(Delimiter::RParen),
            "        ~  " => Token::Delim(Delimiter::RBracket),
            "          ~" => Token::Delim(Delimiter::RBrace),
        }
    }

    #[test]
    fn fixing_delimiters() {
        test! {
            "{ { ( ( [ }",
            "~          " => Token::Delim(Delimiter::LBrace),
            "  ~        " => Token::Delim(Delimiter::LBrace),
            "    ~      " => Token::Delim(Delimiter::LParen),
            "      ~    " => Token::Delim(Delimiter::LParen),
            "        ~  " => Token::Delim(Delimiter::LBracket),
            "          ~" => Token::Delim(Delimiter::RBracket),
            "          ~" => Token::Delim(Delimiter::RParen),
            "          ~" => Token::Delim(Delimiter::RParen),
            "          ~" => Token::Delim(Delimiter::RBrace),
        }

        test! {
            "{ } )",
            "~    " => Token::Delim(Delimiter::LBrace),
            "  ~  " => Token::Delim(Delimiter::RBrace),
        }
    }

    #[test]
    fn full_test() {
        test! {
            "let mut x = avalue.method();",
            "~~~                         " => Token::Let,
            "    ~~~                     " => Token::Mut,
            "        ~                   " => Token::Identifier("x".to_owned()),
            "          ~                 " => Token::Equals,
            "            ~~~~~~          " => Token::Identifier("avalue".to_owned()),
            "                  ~         " => Token::Dot,
            "                   ~~~~~~   " => Token::Identifier("method".to_owned()),
            "                         ~  " => Token::Delim(Delimiter::LParen),
            "                          ~ " => Token::Delim(Delimiter::RParen),
            "                           ~" => Token::SemiColon,
        }
    }
}
