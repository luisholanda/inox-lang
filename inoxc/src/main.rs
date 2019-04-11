use std::fs::File;
use std::io::prelude::*;

use bumpalo::Bump;

use syntax::parser::ParserSession;

fn main() -> std::io::Result<()> {
    let mut file = File::open("./example.in")?;
    let mut content = String::new();

    file.read_to_string(&mut content)?;

    let mut arena = Bump::new();
    let mut string_pool = Bump::new();

    let mut session = ParserSession::new(&mut arena, &mut string_pool);

    let (modu, errors, lexer_errors) = session.parse(&content);
    println!("{:#?}", modu);
    println!("{:#?}", errors);
    println!("{:#?}", lexer_errors);

    Ok(())
}
