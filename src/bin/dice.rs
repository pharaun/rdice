extern crate rdice;

use rdice::parser::parse;

fn main() {
    println!("{:?}", parse("d5"));
    println!("{:?}", parse("2d5"));
    println!("{:?}", parse("d5+2d6"));
}
