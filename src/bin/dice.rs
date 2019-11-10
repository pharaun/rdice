extern crate rdice;

use rdice::parser::parse;

fn main() {
    let dice = "10d10";
    let parse = parse(dice);

    println!("{:?}", parse);
}
