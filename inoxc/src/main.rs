use codespan::CodeMap;

use frontend::ast::desugar::Desugar;
use frontend::codegen;
use frontend::tpck::*;
use symbol::SymGen;
use syntax::lexer::Lexer;
use syntax::parser::ParserSession;

fn main() -> std::io::Result<()> {
    let mut codemap = CodeMap::new();

    let file = codemap.add_filemap_from_disk("./example.in")?;

    let mut session = ParserSession::new();

    let (modu, errors, lexer_errors) = session.parse(file.src());

    if let Some(modu) = modu {
        let gen = SymGen::new();
        let des = Desugar::new(&gen);

        match des.desugar(&modu) {
            Ok(mut ast) => {
                println!("{:#?}", ast);
                let typ_errors = type_check(&mut ast);
                println!("{:#?}", typ_errors);
                println!("{}", codegen::emit(&ast));
            }
            Err(des_errors) => {
                println!("{:#?}", des_errors);
            }
        }

    } else {
        println!("{:#?}", lexer_errors);
        println!("{:#?}", errors);
    }

    Ok(())
}
