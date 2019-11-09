mod lexer;
mod parser;

pub fn parse_dice(input: &str) -> parser::Ast {
    parser::parse(
        lexer::Lexer::new(
            input
        )
    )
}
