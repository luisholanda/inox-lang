use std::fs::File;
use std::io::prelude::*;

use syntax::lexer::Lexer;
use syntax::parser::ParserSession;


fn main() -> std::io::Result<()> {
    let mut file = File::open("./example.in")?;
    let mut content = String::new();

    file.read_to_string(&mut content)?;

    let lexer = Lexer::new(&content);

    for tok in lexer {
        println!("{:^10} ~ {:^10}: {:?}", tok.span.start(), tok.span.end(), tok.as_ref());
    }

    let mut session = ParserSession::new();

    let (modu, errors, lexer_errors) = session.parse(&content);
    println!("{:#?}", modu);
    println!("{:#?}", errors);
    println!("{:#?}", lexer_errors);

    Ok(())
}
