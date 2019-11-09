extern crate rdice;

fn main() {
    let dice = "10d10";
    let parse = rdice::parse_dice(dice);

    println!("{:?}", parse);
}
