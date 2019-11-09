use crate::lexer;

#[derive(Debug, PartialEq)]
pub enum Ast {
    Data(u32),
}

pub fn parse(input: lexer::Lexer) -> Ast {
    for token in input {
        println!("{:?}", token);
    }

    Ast::Data(1)
}
